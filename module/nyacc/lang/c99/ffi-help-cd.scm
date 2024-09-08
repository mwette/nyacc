;;; examples/nyacc/lang/c99/ffi-help-cd.scm

;; Copyright (C) 2016-2024 Matthew Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Notes:

;; This module converts C headers to scheme.  It includes conversion of
;; 1. typedefs, struct defs, union defs, enum defs, function decls, externs
;; 2. FH types are wrappers for an underlying C type system.  This may be
;;    a. scheme bytestructures
;;    b. cdata type (new module coming from this author)
;; 3. On top of the underlying types, FH types add
;;    a. source type name tracing (e.g., printer shows <foo_t* ...>)
;;    b. (pointer-to xxx) and (value-at xxx) converters
;; Functions calls include wrapper and unwrapping to the underlying libffi
;; support in Guile.

;; @table code
;; @item mtail->fh-wrapper
;; generates code to apply wrapper to objects returned from foreign call
;; @item mtail->fh-unwrapper
;; generated code to apply un-wrapper to arguments for foreign call
;; @end table

;; TODOs
;; 2) think about cnvt-fctn that generates C code
;; 3) add code for bytestructures' bounding-struct-descriptor
;; 4) cnvt-udecl needs complete rewrite using udecl->mdecl from c99/munge

;; Issue:
;; So issue is when 'typedef struct ref foo_t' has no 'struct def'
;; we never define a type.  Then later we may see 'typedef foo_t bar_t'
;; We are using define-ffi-type-alias but that then generates a reference
;; to an undefined type.  Maybe for the above we should have a void
;; pseudo-type with
;; name: void
;; (unwrap-void obj) => 'void
;; (wrap 'void) (make-xxx)
;; (pointer-to obj) => <void* obj>
;; (value-at void*-object) =. void

;; For enum typedefs we are not creating types but just using wrappers.

;;; Code:

(define-module (nyacc lang c99 ffi-help-cd)
  #:export (*ffi-help-version*
	    define-ffi-module
	    compile-ffi-file
	    load-include-file
	    ;;fh-cnvt-udecl fh-cnvt-cdecl fh-cnvt-cdecl->str fh-scm-str->scm-exp
	    ;;string-member-proc string-renamer
	    ;;
	    ;;C-decl->scm C-decl
	    ;;pkg-config-incs pkg-config-defs pkg-config-libs

	    ;;ffi-symmap  		; <= debugging

	    ;;C-fun-decl->scm		; deprecated
	    )
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 pretty-print)
  #:use-module (system base language)
  #:use-module (system foreign)
  #:use-module ((system base compile) #:select (compile-file))
  #:use-module ((srfi srfi-1)
                #:select (fold fold-right remove last append-reverse))
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-71)
  #:use-module (sxml fold)
  #:use-module (sxml match)
  #:use-module ((sxml xpath)
		#:renamer (lambda (s) (if (eq? s 'filter) 'sxml:filter s)))
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang c99 parser)
  #:use-module (nyacc lang c99 pprint)
  #:use-module (nyacc lang c99 munge)
  #:use-module (nyacc lang c99 cxeval)
  #:use-module (nyacc lang c99 util)
  #:use-module (nyacc version)
  #:use-module (nyacc lang sx-util)
  #:use-module ((nyacc lang util) #:select (cintstr->scm))
  #:use-module ((nyacc lex) #:select (cnumstr->scm))
  #:use-module ((nyacc util) #:select (ugly-print))
  #:re-export (*nyacc-version*)
  #:version (1 09 4))

;; maybe change to a record-type
(define *options* (make-parameter '()))
(define *debug-parse* (make-parameter #f)) ; parse debug mode
(define *show-incs* (make-parameter #f))   ; show include directories
(define *echo-decls* (make-parameter #f)) ; add echo-decls code for debugging

(define *prefix* (make-parameter "")) ; name prefix (e.g., prefix-syms)
(define *renamer* (make-parameter identity)) ; renamer from ffi-module

(define *udict* (make-parameter '()))	   ; udecl dict
(define *ddict* (make-parameter '()))	   ; cpp-def based dict
(define *defined* (make-parameter '()))    ; defined by define-fh-...
(define *wrapped* (make-parameter '()))    ; wrapped or defined
(define *ttag* (make-parameter ""))

(define *errmsgs* (make-parameter '()))	; list of warnings


;; === utilities

(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args)
  (force-output (current-error-port)))
(define (pperr tree)
  (pretty-print tree (current-error-port) #:per-line-prefix "  "))

(define (fherr fmt . args)
  (apply throw 'ffi-help-error fmt args))

(define (fherr/once fmt . args)
  (let ((errmsgs (*errmsgs*)))
    (cond
     ((member fmt errmsgs)
      (apply throw 'ffi-help-error #f '()))
     (else
      (*errmsgs* (cons fmt errmsgs))
      (apply throw 'ffi-help-error fmt args)))))

(define (sfstr fmt . args)
  (apply simple-format #f fmt args))

(define (sfsym fmt . args)
  (string->symbol (apply simple-format #f fmt args)))

(define (ppstr exp)
  (call-with-output-string
    (lambda (port)
      (pretty-print exp port #:per-line-prefix "  "))))

(define (make-arg-namer)
  (let ((ix 0))
    (lambda ()
      (let ((iv ix))
	(set! ix (1+ ix))
	(simple-format #f "arg~S" iv)))))

;; strings->symbol
(define (strings->symbol . string-list)
  (string->symbol (apply string-append string-list)))

(define (noblanks str)
  (string-map (lambda (c) (if (char=? #\space c) #\- c)) str))

;; "unsigned int" => unsigned-int
(define (cstrnam->symnam strname)
  (string->symbol (string-map (lambda (c) (if (char=? #\space c) #\- c))
                              strname)))

;; '(abc def) => "abc-def"
(define (m-path->name path)
  (string-join (map symbol->string path) "-"))

;; '(abc def) => "abc/def"
(define (m-path->f-path path)
  (string-join (map symbol->string path) "/"))

;; @deffn {Procedure} opts->attrs module-opts script-opts
;; The values in @var{script-opts} override @var{module-opts}.  That is,
;; if the value is a list then append, else replace.
;; @end deffn
(define (opts->attrs module-opts script-opts)
  ;; module-opts: inc-dirs pkg-config ...
  ;; script-opts: inc-dirs
  (fold-right
   (lambda (opt seed)
     (cond
      ((assq-ref seed (car opt)) =>
       (lambda (val)
	 (if (pair? val)
	     (acons (car opt) (append (cdr opt) val) seed)
	     (acons (car opt) val seed))))
      (else (cons opt seed))))
   (filter (lambda (pair) (symbol? (car pair))) module-opts)
   script-opts))

(define (opts->mopts opts) ;; module options to pass
  (filter (lambda (pair) (keyword? (car pair))) opts))

;; Run pkg-config
(define (pkg-config name . args)
  (if name
      (let* ((cmdstr (string-append "pkg-config" " "
				    (string-join args) " " name))
	     (port (open-input-pipe cmdstr))
	     (ostr (read-line port))
	     (status (close-pipe port))
	     (items (if (eof-object? ostr) '() (string-split ostr #\space))))
	(unless (zero? status) (fherr "failed: `~A'" cmdstr))
	items)
      '()))

;; use pkg-config to get a list of include dirs
;; (pkg-config-incs "cairo") => ("/opt/local/include/cairo" ...)
(define (pkg-config-incs name)
  (fold-right
   (lambda (s l)
     (cond
      ((< (string-length s) 3) l)
      ((string=? "-I" (substring/shared s 0 2))
       (cons (substring/shared s 2) l))
      (else l)))
   '()
   (pkg-config name "--cflags")))

(define (pkg-config-defs name)
  (fold-right
   (lambda (s l)
     (cond
      ((< (string-length s) 3) l)
      ((string=? "-D" (substring/shared s 0 2))
       (cons (substring/shared s 2) l))
      (else l)))
   '()
   (pkg-config name "--cflags")))

;; TODO:
;; 1) check if --libs provides -L that is not in ld.so dirs.
(define (pkg-config-libs name)
  (fold-right
   (lambda (s l)
     (cond
      ((< (string-length s) 3) l)
      ((string=? "-lm" s) l) ;; workaround for ubuntu libm issue
      ((string=? "-l" (substring/shared s 0 2))
       (cons (string-append "lib" (substring/shared s 2)) l))
      (else l)))
   '()
   (pkg-config name "--libs")))

(define (resolve-attr-val val)
  (let* ((val (if (procedure? val) (val) val)))
    (cond
     ((eq? #f val) '())
     ((list? val) val)
     ((string? val) (list val))
     (else (throw 'ffi-help-error "value does not resolve to list")))))

(define (cintstr->num str)
  (and=> (cintstr->scm str) string->number))

(define (sw/* name) (string-append name "*"))
(define (sw/& name) (string-append name "&"))
(define (sw/struct name) (string-append "struct-" name))
(define (sw/union name) (string-append "union-" name))
(define (sw/struct* name) (string-append "struct-" name "*"))
(define (sw/union* name) (string-append "union-" name "*"))

;; I was using (pointer . name) in *defined* but this has issues
;; because expand-typerefs does not recognize it.  Is a change needed?
(define (w/struct name) (cons 'struct name))
(define (w/union name) (cons 'union name))
(define (w/enum name) (cons 'enum name))
(define (w/* name) (cons 'pointer name))
(define (w/struct* name) (cons 'pointer (cons 'struct name)))
(define (w/union* name) (cons 'pointer (cons 'union name)))

(define (rename name)
  ((*renamer*) name))

(define (const-expr->number expr)
  (catch 'c99-error
    (lambda () (eval-c99-cx expr (*udict*) (*ddict*)))
    (lambda (key fmt . args)
      (apply throw 'ffi-help-error fmt args))))

(define (packed? aggr-attr)
  (and (and=> (assoc-ref aggr-attr 'attributes)
              (lambda (l) (string-contains (car l) "__packed__" 0)))
       #t))

;; @deffn {Procedure} cnvt-fields fields expand-tail
;; Convert field list for a struct or union bytestructure descriptor.
;; The @var{fields} is the tail of the form @code{(field-list field ...)}
;; The procedure @var{expand-tail} will expand the type in the field.
;; @end deffn
(define-public (cnvt-fields fields expand-tail)
  (define qq 'quasiquote)
  (define uq 'unquote)
  (map
   (lambda (dent)
     (let* ((name (car dent)) (udecl (cdr dent))
            (name (if (or (not name) (positive? (string-length name))) name #f))
	    (mdecl (udecl->mdecl (udecl-rem-type-qual udecl)))
	    (type (expand-tail (md-tail mdecl))))
       (if (and (pair? type) (eq? 'cbitfield (car type)))
           `(,qq (,(and=> name string->symbol) (,uq ,(cadr type)) ,(caddr type)))
           `(,qq (,(and=> name string->symbol) (,uq ,type))))))
   (clean-and-dictize-fields fields)))

;; Should be processed with canize-enum-def-list first
(define (enum-def-list->alist enum-def-list)
  (map (lambda (defn)
         (sx-match defn
           ((enum-defn (ident ,name) (fixed ,value))
            (list (string->symbol name) (string->number value)))))
       (sx-tail enum-def-list)))

(define def-defined
  '("void" "float" "double" "short" "short int" "signed short"
    "signed short int" "int" "signed" "signed int" "long" "long int"
    "signed long" "signed long int" "long long" "long long int"
    "signed long long" "signed long long int" "unsigned short int"
    "unsigned short" "unsigned int" "unsigned" "unsigned long int"
    "unsigned long" "unsigned long long int" "unsigned long long"
    "intptr_t" "uintptr_t" "size_t" "ssize_t" "ptrdiff_t" "int8_t"
    "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t" "int64_t"
    "uint64_t" "float _Complex" "double _Complex" "char" "signed char"
    "unsigned char" "wchar_t" "char16_t" "char32_t" "_Bool" "bool"))

(define ffi-long-long #f)               ; defined below from cpp-defs
(define ffi-unsigned-long-long #f)      ; defined below from cpp-defs

(define ffi-typemap
  ;; see system/foreign.scm
  `(("void" . ffi:void) ("float" . ffi:float) ("double" . ffi:double)
    ("complex float" . ffi:complex-float)
    ("complex double" . ffi:complex-double)
    ;;
    ("short" . ffi:short) ("short int" . ffi:short) ("signed short" . ffi:short)
    ("signed short int" . ffi:short) ("int" . ffi:int) ("signed" . ffi:int)
    ("signed int" . ffi:int) ("long" . ffi:long) ("long int" . ffi:long)
    ("signed long" . ffi:long) ("signed long int" . ffi:long)
    ("unsigned short int" . ffi:unsigned-short)
    ("unsigned short" . ffi:unsigned-short) ("unsigned int" . ffi:unsigned-int)
    ("unsigned" . ffi:unsigned-int) ("unsigned long int" . ffi:unsigned-long)
    ("unsigned long" . ffi:unsigned-long)
    ;;
    ("size_t" . ffi:size_t)
    ;;
    ("ssize_t" . ffi:ssize_t) ("ptrdiff_t" . ffi:ptrdiff_t)
    ("int8_t" . ffi:int8) ("uint8_t" . ffi:uint8)
    ("int16_t" . ffi:int16) ("uint16_t" . ffi:uint16)
    ("int32_t" . ffi:int32) ("uint32_t" . ffi:uint32)
    ("int64_t" . ffi:int64) ("uint64_t" . ffi:uint64)
    ;; hacks
    ("intptr_t" . ffi:intptr_t) ("uintptr_t" . ffi:uintptr_t)
    ("char" . ffi:int8) ("signed char" . ffi:int8) ("unsigned char" . ffi:uint8)
    ("wchar_t" . int) ("char16_t" . int16) ("char32_t" . int32)
    ("long long" . ,ffi-long-long) ("long long int" . ,ffi-long-long)
    ("signed long long" . ,ffi-long-long)
    ("signed long long int" . ,ffi-long-long)
    ("unsigned long long" . ,ffi-unsigned-long-long)
    ("unsigned long long int" . ,ffi-unsigned-long-long)
    ("_Bool" . ffi:int8) ("bool" . ffi:int8)))

(define ffi-defined (map car ffi-typemap))

(define ffi-symmap
  `((ffi:void . ,void) (ffi:float . ,float) (ffi:double . ,double)
    (ffi:short . ,short) (ffi:int . ,int) (ffi:long . ,long)
    (ffi:unsigned-short . ,unsigned-short) (ffi:unsigned-int . ,unsigned-int)
    (ffi:unsigned-long . ,unsigned-long) (ffi:size_t . ,size_t)
    (ffi:ssize_t . ,ssize_t) (ffi:ptrdiff_t . ,ptrdiff_t) (ffi:int8 . ,int8)
    (ffi:uint8 . ,uint8) (ffi:int16 . ,int16) (ffi:uint16 . ,uint16)
    (ffi:int32 . ,int32) (ffi:uint32 . ,uint32) (ffi:int64 . ,int64)
    (ffi:uint64 . ,uint64) ('* . *)))

;; assumes fields are unitized
;; this does not do the job right because needs to include double
;; if that is used
(define (bounding-mtail-for-union-fields fields)
  (let loop ((btail #f) (mxsz 0) (mxal 0) (flds fields))
    (if (null? flds) btail
	(let* ((udecl (car flds))
               (xdecl (expand-typerefs (car flds) (*udict*)))
               (mtail (cdr (udecl->mdecl xdecl))))
	  (call-with-values (lambda () (sizeof-mtail mtail (*udict*)))
	    (lambda (sz al)
	      (if (> sz mxsz)
		  (loop mtail sz (max al mxal) (cdr flds))
		  (loop btail mxsz (max al mxal) (cdr flds)))))))))

(define cfix-dict
  '(("signed char" . "char") ("signed short" . "short") ("short int" . "short")
    ("signed short int" . "short") ("unsigned short int" . "unsigned short")
    ("signed" . "int") ("signed int" . "int") ("unsigned" . "unsigned int")
    ("long int" . "long") ("signed long" . "long") ("signed long int" . "long")
    ("unsigned long int" . "unsigned long") ("long long int" . "long long")
    ("signed long long" . "long long") ("signed long long int" . "long long")
    ("unsigned long long int" . "unsigned long long")))

(define (cfix name)
  (or (assoc-ref cfix-dict name) name))


;; === cdata/ctype support =====================================================

(use-modules (system foreign arch-info))
;;(use-modules (system foreign cdata))

(define (mtail->ctype mtail)
  (let ((defined (*defined*)) (ttag (*ttag*)))
    (match mtail
      (`((pointer-to) (typename ,name))
       (let ((name (rename name)))
	 (if (member (w/* name) defined)
	     (strings->symbol name "*" ttag)
             `(cpointer ,(mtail->ctype (cdr mtail))))))
      (`((pointer-to) (void))
       `(cbase 'void*))
      (`((pointer-to) (fixed-type "char"))
       `(cpointer (cbase 'char)))
      (`((pointer-to) (fixed-type ,name))
       `(cpointer (cbase ',(cstrnam->symnam (cfix name)))))
      (`((pointer-to) (float-type ,name))
       `(cpointer (cbase  ',name)))
      (`((pointer-to) (function-returning (param-list . ,params)) . ,tail)
       (call-with-values
           (lambda () (let ((return (mdecl->udecl (cons "~ret" tail))))
                        (function*-wraps return params)))
         (lambda (wrapper unwrapper)
           `(cfunction ,wrapper ,unwrapper))))
      (`((pointer-to) (pointer-to) (function-returning . ,rest) . ,rest)
       `(cpointer (cbase 'void*)))
      (`((pointer-to) (struct-ref (ident ,name)))
       (if (member (w/struct name) defined)
           `(cpointer ,(strings->symbol "struct-" name ttag))
	   `(cpointer 'void)))
      (`((pointer-to) . ,rest)
       `(cpointer ,(mtail->ctype rest)))
      (`((array-of ,n) . ,rest)
       `(carray ,(mtail->ctype rest) ,(const-expr->number n)))
      (`((array-of) . ,rest)
       `(carray ,(mtail->ctype rest) 0))
      (`((bit-field ,size) . ,rest)
       `(cbitfield ,(mtail->ctype rest) ,(const-expr->number size)))
      (`((extern) . ,rest) (mtail->ctype rest))
      (__
       (sx-match (car mtail)
         ((typename ,name)
          (let ((name (rename name)))
	    (cond
             ((member name base-type-name-list)
              `(cbase ',(cstrnam->symnam name)))
             ((member name defined) (strings->symbol name ttag))
             (else (let* ((udecl `(udecl (decl-spec-list (type-spec . ,mtail))
                                         (init-declr (ident "_"))))
                          (xdecl (expand-typerefs udecl (*udict*) defined))
                          (mdecl (udecl->mdecl xdecl)))
                     (mtail->ctype (md-tail mdecl)))))))
         ((void) (error "mtail->ctype: arg not expected: void") #f)
         ((fixed-type ,name) `(cbase ',(cstrnam->symnam name)))
         ((float-type ,name) `(cbase ',(cstrnam->symnam name)))
         ((enum-def (@ . ,attr) (ident ,ident) ,rest)
          (mtail->ctype `((enum-def (@ . ,attr) ,rest))))
         ((enum-def (@ . ,attr) ,edl)
          (let ((def-list (canize-enum-def-list edl (*udict*) (*ddict*))))
            (if (packed? attr)
                `(cenum ',(enum-def-list->alist def-list) #t)
                `(cenum ',(enum-def-list->alist def-list)))))
         ((enum-ref ,name) (strings->symbol "enum-" name ttag))
         ((struct-def (@ . ,attr) (ident ,struct-name) ,field-list)
          (mtail->ctype `((struct-def (@ . ,attr) ,field-list))))
         ((struct-def (@ . ,attr) (field-list . ,fields))
          (let ((fields (cnvt-fields fields mtail->ctype)))
            (if (packed? attr)
                `(cstruct (list ,@fields) #t)
                `(cstruct (list ,@fields)))))
         ((struct-ref (ident ,struct-name))
          (string->symbol (string-append "struct-" struct-name ttag)))
         ((union-def (ident ,name) ,field-list)
          (mtail->ctype `((union-def ,field-list))))
         ((union-def (field-list . ,fields))
          `(cunion (list ,@(cnvt-fields fields mtail->ctype))))
         ((union-ref (ident ,name))
          (strings->symbol "union-" name ttag))
         (,otherwise (fherr "mtail->ctype missed:\n~A" (ppstr mtail))))))))
(export mtail->ctype)

;; === hookup ==================================================================

(define target 'ct)

(case target
  ((ct) (*ttag* ""))
  (else (error "bad target" target)))

(define Tmodules
  (case target
    ((ct) '((system foreign cdata)))
    (else (error "bad target" target))))

(define Tpointer
  (case target
    ((ct) 'cpointer)
    (else (error "bad target" target))))

(define mtail->ttype
  (case target
    ((ct) mtail->ctype)
    (else (error "bad target" target))))

;; === output ffi-module header ================================================

(define *mport* (make-parameter #t))	   ; output module port

(define (sfscm fmt . args)
  (apply simple-format (*mport*) fmt args))
(define* (ppscm tree #:key (per-line-prefix ""))
  (pretty-print tree (*mport*) #:per-line-prefix per-line-prefix))
(define* (upscm tree #:key (per-line-prefix ""))
  (ugly-print tree (*mport*) #:per-line-prefix per-line-prefix))
(define (c99scm tree)
  (pretty-print-c99 tree
		    (*mport*)
		    #:per-line-prefix ";; "))
(define (nlscm) (newline (*mport*)))

(define (ffimod-header path module-opts)
  (let* ((attrs (opts->attrs module-opts '()))
	 (pkg-config (assq-ref attrs 'pkg-config))
         (libs (resolve-attr-val (assq-ref attrs 'library)))
         (libs (if pkg-config (append (pkg-config-libs pkg-config) libs) libs))
 	 (libs (delete "libm" libs)))
    (sfscm ";; generated with `guild compile-ffi ~A.ffi'\n"
	   (m-path->f-path path))
    (nlscm)
    (sfscm "(define-module ~S\n" path)
    (for-each ;; ffi-modules
     (lambda (pair)
       (cond
	((eq? 'use-ffi-module (car pair))
	 (sfscm "  #:use-module ~S\n" (cdr pair)))))
     module-opts)
    (for-each ;; output pass-through options
     (lambda (pair) (sfscm "  ~S " (car pair)) (ppscm (cdr pair)))
     (opts->mopts module-opts))
    (sfscm "  #:use-module ((system foreign) #:prefix ffi:)\n")
    (sfscm "  #:use-module (system foreign-library))\n")
    (ppscm `(use-modules ,@Tmodules))
    (sfscm "\n")
    (ppscm
     `(define (foreign-pointer-search name)
        (let loop ((libs (list #f ,@libs)))
          (cond
           ((null? libs) (error "no library for ~s" name))
           ((false-if-exception (foreign-library-pointer (car libs) name)))
           (else (loop (cdr libs)))))))
    (sfscm "\n")
    (ppscm
     `(define (unwrap~enum arg)
        (or (assq-ref ,(strings->symbol (m-path->name path) "-symbol-tab") arg)
            arg)))
    (if (*echo-decls*) (sfscm "(define echo-decls #t)\n\n"))))


;; === ffi-helper code gen =====================================================

;; === FFI api support =========================================================

;; Dealing with functions requires for each parameter and the return:
;; 1. Convering C type decl's to associated Guile ffi type decl's.
;; 2. Associating an unwrapper to map (wrapped) scheme arguments to ffi args.
;; 3. Associating a wrapper to map returned ffi result to appropriate value.

(define* (mtail->ffi-decl mtail)

  (define (eval-dim dim) ;; may want to catch errors
    (eval-c99-cx dim (*udict*) (*ddict*)))

  (define (cnvt mtail)
    (match mtail
      (`((pointer-to) . ,rest) ''*)
      (`((fixed-type ,name))
       (or (assoc-ref ffi-typemap name)
	   (fherr/once "no FFI type for ~A" name)))
      (`((float-type ,name))
       (or (assoc-ref ffi-typemap name)
	   (fherr/once "no FFI type for ~S" name)))
      (`((typename ,name) . ,rest)
       (or (assoc-ref ffi-typemap name)
	   (fherr "no FFI type for ~S" name)))
      (`((void)) 'ffi:void)
      (`((enum-def . ,rest2) . ,rest1) 'ffi:int)
      (`((enum-ref . ,rest2) . ,rest1) 'ffi:int)

      (`((array-of ,dim) . ,rest)
       `(make-list ,(eval-dim dim) ,(cnvt rest)))
      (`((array-of) . ,rest)
       (cnvt `((array-of "0") . ,rest)))

      (`((struct-def (field-list . ,fields)))
       `(list ,@(map (lambda (fld)
                       (let* ((udict (dictize-comp-decl fld))
	                      (name (caar udict))
	                      (udecl (cdar udict))
	                      (udecl (udecl-rem-type-qual udecl))
	                      (mdecl (udecl->mdecl udecl)))
                         (cnvt (cdr mdecl))))
	             (clean-and-unitize-fields fields))))
      (`((struct-def (ident ,name) ,field-list))
       (cnvt `((struct-def ,field-list))))

      (`((union-def (field-list . ,fields)))
       (cnvt
        (bounding-mtail-for-union-fields
         (clean-and-unitize-fields fields))))
      (`((union-def (ident ,name) ,field-list))
       (cnvt `((union-def ,field-list))))

      (otherwise
       (fherr "mtail->ffi-decl missed:\n~A" (ppstr mtail)))))

  (match mtail
    (`((array-of ,dim) . ,rest) ''*)
    (__ (cnvt mtail))))

(define* (defined-type-unwrapper name mname)
  (let* ((udecl (expand-typerefs
                 `(udecl (decl-spec-list (type-spec (typename ,name)))
                         (init-declr (ident ,mname))) (*udict*) '()))
         (mdecl (udecl->mdecl udecl)))
    (match (md-tail mdecl)
      (`((pointer-to) . ,_0) `(unwrap-pointer ,mname ,(string->symbol name)))
      (`((fixed ,name)) `(unwrap-number ,mname))
      (`((float ,name)) `(unwrap-number ,mname))
      (`((enum-def . ,_)) `(unwrap~enum  ,mname))
      (__ #f))))

(define (unwrap-mdecl mdecl)
  (let ((wrapped (*wrapped*)) (defined (*defined*))
        (mname (string->symbol (md-label mdecl))) (mtail (md-tail mdecl)))
    (match (car mtail)
      (`(fixed-type ,name) `(unwrap-number ,mname))
      (`(float-type ,name) `(unwrap-number ,mname))
      (`(void) #f)
      (`(typename ,name)
       (cond
	((member name def-defined) `(unwrap-number ,name))
	((member name defined) (defined-type-unwrapper name mname))
	((member name wrapped) (list (strings->symbol "unwrap-" name) mname))
	(else #f)))
      (`(enum-def (ident ,name) ,_)
       (cond
	((member (w/enum name) wrapped)
         (list (strings->symbol "unwrap-enum-" name) mname))
	(else `(unwrap~enum ,mname))))
      (`(enum-def ,_) `(unwrap~enum ,mname))
      (`(enum-ref (ident ,name))
       (cond
	((member (w/enum name) wrapped)
         (list (strings->symbol "unwrap-enum-" name) mname))
	(else `(unwrap~enum ,mname))))
      (`(struct-ref (ident ,name)) `(fh-object-pointer mmname)) ;; FIXME
      (`(union-ref (ident ,name)) `(fh-object-pointer ,mname)) ;; FIXME
      (`(pointer-to) `(unwrap-pointer ,mname))
      (`(array-of ,size) `(unwrap-array ,mname))
      (`(array-of) `(unwrap-array ,mname))
      ;; not expected
      (`(struct-def . ,_) `(cdata&-ref ,mname))
      (`(union-def . ,_) `(cdata&-ref ,mname))
      (otherwise
       (fherr "unwrap-mdecl: missed:\n~A" (ppstr mtail))))))

(define (wrap-mdecl mdecl)
  (let ((wrapped (*wrapped*)) (defined (*defined*))
        (mname (string->symbol (md-label mdecl)))
        (mtail (md-tail mdecl)))
    (match mtail
      (`((fixed-type ,name)) #f)
      (`((float-type ,name)) #f)
      (`((void)) #f)
      (`((typename ,name))
       (cond
        ((member name def-defined) #f)
	((member name defined) `(make-cdata ,(string->symbol name) ,mname))
	((member name wrapped) (list (sfsym "wrap-~A" name) mname))
	(else #f)))
      (`((enum-def (ident ,name) ,rest))
       (and (member (w/enum name) wrapped)
            (list (sfscm "wrap-enum-~A" name) mname)))
      (`((enum-def ,_)) #f)
      (`((enum-ref (ident ,name)))
       (cond
        ((member (w/enum name) wrapped) (sfsym "wrap-enum-~A" name))
	(else 'wrap-enum)))
      (`((pointer-to) (typename ,tname))
       (cond
        ((member tname ffi-defined) #f)
	((member (w/* tname) defined)
         (let ((sname (sfsym "~A*" tname)))
           `(make-cdata ,sname ,mname)))
	((member (w/* tname) wrapped) (list (sfsym "wrap-~A*" tname) mname))
	(else #f)))
      (`((pointer-to) (struct-ref (ident ,aggr-name) . ,rest))
       (cond
        ((member (w/struct aggr-name) wrapped)
         (let ((sname (sfsym "struct-~A*" aggr-name)))
         `(make-cdata ,sname ,mname)))
	(else #f)))
      (`((pointer-to) (union-ref (ident ,aggr-name) . ,rest))
       (cond
	((member (w/union aggr-name) wrapped)
         (let ((sname (sfsym "union-~A*" aggr-name)))
         `(make-cdata ,sname ,mname)))
	(else #f)))
      (`((pointer-to) . ,otherwise) #f)
      (`((array-of) . ,rest)
       (wrap-mdecl (cons* (car mdecl) '(pointer-to) rest)))
      (otherwise
       (fherr "wrap-mdecl missed:\n~A" (ppstr mtail))))))

(define* (udecl->ffi-decl udecl #:optional (keepers ffi-defined))
  (mtail->ffi-decl
   (md-tail (udecl->mdecl
             (udecl-rem-type-qual
              (expand-typerefs udecl (*udict*) keepers))))))

(define* (unwrap-udecl udecl #:optional (keepers '()))
  (unwrap-mdecl
   (udecl->mdecl
    (udecl-rem-type-qual
     (expand-typerefs udecl (*udict*) keepers)))))

(define* (wrap-udecl udecl #:optional (keepers '()))
  (wrap-mdecl
   (udecl->mdecl
    (udecl-rem-type-qual
     (expand-typerefs udecl (*udict*) (*wrapped*))))))


;; === function types =========================================================

(define (setup-function return params)
  (define void-param '(param-decl (decl-spec-list (type-spec (void)))))
  (let* ((params (let loop ((rl '()) (pl params))
                   (cond
                    ((null? pl) (reverse rl))
                    ((equal? '(ellipsis) (car pl)) (reverse rl))
                    ((equal? void-param (car pl)) '())
                    (else (loop (cons (reify-udecl (car pl)) rl) (cdr pl))))))
         (names (map string->symbol
                     (map (lambda (u) (declr-name (sx-ref u 2))) params))))
    (values params names)))

(define (function-decls return params)
  (values (udecl->ffi-decl return) (map udecl->ffi-decl params)))

(define (function-execs return params)
  (values (wrap-udecl return (*wrapped*))
          (map (lambda (p) (unwrap-udecl p (*wrapped*))) params)))

(define (function-callbacks return params)
  (values (unwrap-udecl return (*wrapped*))
          (map (lambda (p) (wrap-udecl p (*wrapped*))) params)))

;; @deffn {Procedure} function*-wraps return params => values
;; Based on return udecl @var{return} and udecl params @var{params},
;; generate two procedures:
;; @enumerate
;; @item generate procedure from pointer
;; @item generate pointer from procedure
;; @end enumerate
;; @end deffn
(define (function*-wraps return params)
  (define varargs? (and (pair? params) (equal? (last params) '(ellipsis))))
  (let* ((params names (setup-function return params))
         (decl-ret decl-par (function-decls return params))
         (exec-ret exec-par (function-execs return params))
         (cbak-ret cbak-par (function-callbacks return params))
         (urap-par (fold-right (lambda (n u s) (if u (cons `(,n ,u) s) s))
                               '() names exec-par))
         (wrap-par (fold-right (lambda (n u s) (if u (cons `(,n ,u) s) s))
                               '() names cbak-par))
         (call `(~proc ,@names))
         (va-call `(apply ~proc ,names (map cdr ~rest))))
    (values
     ;; procedure->pointer
     `(lambda (~proc)
        (ffi:procedure->pointer
         ,decl-ret
         (lambda ,names
           (let ,wrap-par
             ,(if cbak-ret `((lambda (~ret) ,cbak-ret) ,call) call)))
         (list ,@decl-par)))
     (if varargs?
         ;; pointer->procedure (varargs)
         `(lambda (~fptr)
	    (lambda (,@names . ~rest)
	      (let ((~proc (ffi:pointer->procedure
                            ,decl-ret ~fptr
                            (cons* ,@decl-par (map car ~rest)))
	                   ,@urap-par))
		,(if exec-ret `((lambda (~ret) ,exec-ret) ,va-call) va-call))))
         ;; pointer->procedure
         `(lambda (~fptr)
	    (let ((~proc (ffi:pointer->procedure
                          ,decl-ret ~fptr (list ,@decl-par))))
	      (lambda ,names
	        (let ,urap-par
		  ,(if exec-ret `((lambda (~ret) ,exec-ret) ,call) call)))))))))


(define (cnvt-fctn name return params)
  ;; can't use function*-wraps just because of the delay :(
  (define varargs? (and (pair? params) (equal? (last params) '(ellipsis))))
  (let* ((params names (setup-function return params))
         (decl-ret decl-par (function-decls return params))
         (exec-ret exec-par (function-execs return params))
         (urap-par (fold-right (lambda (n u s) (if u (cons `(,n ,u) s) s))
                               '() names exec-par))
         (call `((force ~proc) ,@names))
	 (va-call `(apply ~proc ,@names (map cdr ~rest))))
    (ppscm
     `(define-public ,(string->symbol name)
        ,(if varargs?
             `(lambda (,@names . ~rest)
	        (let ((~proc (ffi:pointer->procedure
                              ,decl-ret (foreign-pointer-search ,name)
                              (cons* ,@decl-par (map car ~rest))))
	              ,@urap-par)
		  ,(if exec-ret `((lambda (~ret) ,exec-ret) ,va-call) va-call)))
	     `(let ((~proc
                     (delay (ffi:pointer->procedure
                             ,decl-ret (foreign-pointer-search ,name)
                             (list ,@decl-par)))))
                (lambda ,names
	          (let ,urap-par
                    ,(if exec-ret `((lambda (~ret) ,exec-ret) ,call) call)))))))))


;; === the main conversion driver ==============================================

;; This routine will munge the fields and then perform typeref expansion.
;; `defined' here means has -desc (what?)
(define (expand-field-list-typerefs field-list)
  (let ((udict (*udict*)) (defined (*defined*)))
    (cons 'field-list
	  (fold-right
	   (lambda (udecl seed)
	     (cons (expand-typerefs udecl udict defined) seed))
	   '() (clean-and-unitize-fields (sx-tail field-list))))))


;; @deffn {Procedure} cnvt-udecl udecl udict wrapped defined)
;; Given udecl produce a ffi-spec.
;; Return updated (string based) keep-list, which will be modified if the
;; declaration is a typedef.  The typelist is the set of keepers used for
;; @code{udecl->mdecl}.
;; Returns (values wrapped, defined), where defined is a fh-def for a type
;; and wrapped means there is are converters to/from fh to scheme.
;; @end deffn
(define (cnvt-udecl udecl udict wrapped defined)

  (define (specl-props specl)
    (let loop ((ss '()) (tq '()) (ts #f) (tl (sx-tail specl)))
      (if (null? tl) (values ss tq ts)
          (case (sx-tag (car tl))
            ((stor-spec)
             (loop (cons (sx-tag (sx-ref (car tl) 1)) ss) tq ts (cdr tl)))
            ((type-qual)
             (loop ss (cons (sx-tag (sx-ref (car tl) 1)) tq) ts (cdr tl)))
            ((type-spec)
             (loop ss tq (sx-ref (car tl) 1) (cdr tl)))))))

  (define ftn-declr?
    (let ((ff (node-closure
               (node-join (select-kids (node-typeof? 'ftn-declr))
                          (select-kids (node-typeof? 'ident))))))
      (lambda (declr) (pair? (ff (list '*TOP* declr))))))

  (define (bkref-extend! decl typename)
    (let ((aval (sx-attr-ref decl 'typedef)))
      (sx-attr-set! decl 'typedef
		    (if aval (string-append aval "," typename) typename))))
  (define (bkref-getall attr)
    (and=> (assq-ref attr 'typedef) (lambda (t) (string-split (car t) #\,))))

  ;; use fluids OR pass around
  (*wrapped* wrapped)
  (*defined* defined)

  (define-syntax-rule (deftype name type)
    `(define-public ,name (name-ctype ,type ',name)))

  (let* ((tag attr specl declr (split-udecl udecl))
         (sspec tqual tspec (specl-props specl)))

    (cond

     ((memq 'typedef sspec)
      (let* ((specl `(decl-spec-list (type-spec ,tspec)))
             (mdecl (udecl->mdecl (sx-list 'udecl #f specl declr)))
             (name (md-label mdecl))
             (type (strings->symbol name))
             (type* (strings->symbol name"*"))
             (mtail (md-tail mdecl)))

        (match mtail

          (`((array-of ,dim) . ,rest)
           (ppscm (deftype type (mtail->ttype mtail)))
           (ppscm (deftype type* `(cpointer ,type)))
           (values (cons name wrapped) (cons name defined)))

          (`((function-returning (param-list . ,params)) . ,rest)
           (let ((return (mdecl->udecl (cons "~ret" rest))))
             (call-with-values (lambda () (function*-wraps return params))
               (lambda (ptr->proc proc->ptr)
                 (ppscm (deftype type `(cfunction ,ptr->proc ,proc->ptr)))
                 (ppscm (deftype type* `(cpointer ,type))))))
           (values (cons* name (w/* name) wrapped)
                   (cons* name (w/* name) defined)))

          (`((pointer-to) (function-returning (param-list . ,params)) . ,rest)
           (let ((return (mdecl->udecl (cons "~ret" rest))))
             (call-with-values (lambda () (function*-wraps return params))
               (let ((*type (strings->symbol "*" name (*ttag*))))
                 (lambda (pr->pc pc->pr)
                   (ppscm `(define-public ,*type (cfunction ,pr->pc ,pc->pr)))
                   (ppscm (deftype type `(cpointer ,*type)))))))
           (values (cons name wrapped) (cons name defined)))

          (`((pointer-to) . ,rest)
           (ppscm (deftype type (mtail->ttype mtail)))
           (case (caar rest)
             ((fixed-type float-type) (values wrapped defined))
             (else (values (cons name wrapped) (cons name defined)))))

          (__
           (sx-match (car mtail)
             
             ((struct-def (@ . ,attr) (ident ,agname) ,field-list)
              (let ((aname (strings->symbol "struct-" agname))
                    (aname* (strings->symbol "struct-" agname "*")))
                (ppscm (deftype type (mtail->ttype mtail)))
                (ppscm (deftype type* `(cpointer ,type)))
                (ppscm (deftype aname type))
                (ppscm (deftype aname* type*)))
              (values (cons* name (w/* name) (w/struct agname)
                             (w/struct* agname) wrapped)
                      (cons* name (w/* name) (w/struct agname)
                             (w/struct* agname) defined)))

             ((struct-def ,field-list)
              (ppscm (deftype type (mtail->ttype mtail)))
              (ppscm (deftype type* `(cpointer ,type)))
              (values  (cons* name (w/* name) wrapped)
                       (cons* name (w/* name) defined)))

             ((union-def (ident ,agname) ,field-list)
              (let ((aname (strings->symbol "union-" agname))
                    (aname* (strings->symbol "union-" agname "*")))
                (ppscm (deftype type (mtail->ttype mtail)))
                (ppscm (deftype type* `(cpointer ,type)))
                (ppscm (deftype aname type))
                (ppscm (deftype aname* type*)))
              (values (cons* name (w/* name) (w/union agname)
                             (w/union* agname) wrapped)
                      (cons* name (w/* name) (w/union agname)
                             (w/union* agname) defined)))

             ((union-def ,field-list)
              (ppscm (deftype type (mtail->ttype mtail)))
              (ppscm (deftype type* `(cpointer ,type)))
              (values (cons* name (w/* name) wrapped)
                      (cons* name (w/* name) defined)))

             ((struct-ref (ident ,agname))
              (cond
               ((member (w/struct agname) defined) ;; defined previously
                (ppscm (deftype type (mtail->ttype mtail)))
	        (ppscm (deftype type* (sfsym "struct-~A*-type" agname))))
               ((udict-struct-ref udict agname) ;; defined later
                =>
                (lambda (decl)
                  (bkref-extend! decl name)
                  (ppscm `(define-public ,type 'void))
                  (ppscm (deftype type* `(cpointer (delay ,type))))))
               (else ;; not defined
                (ppscm `(define-public ,type 'void))
                (ppscm (deftype type* `(cpointer ,type)))))
              (values (cons* name (w/* name) wrapped)
                      (cons* name (w/* name) defined)))

             ((union-ref (ident ,agname))
              (cond
               ((member (w/union agname) defined) ;; defined previously
                (ppscm (deftype type (mtail->ttype mtail)))
	        (ppscm (deftype type* (sfsym "union-~A*-type" agname))))
               ((udict-union-ref udict agname) ;; defined later
                =>
                (lambda (decl)
                  (bkref-extend! decl name)
                  (ppscm `(define-public ,type 'void))
                  (ppscm `(define-public ,type* (cpointer (delay ,type))))))
               (else ;; not defined
                (ppscm `(define-public ,type 'void))
                (ppscm (deftype type* `(cpointer ,type)))))
              (values (cons* name (w/* name) wrapped)
                      (cons* name (w/* name) defined)))

             (((fixed-type float-type) ,basename)
              (ppscm (deftype type (mtail->ttype mtail)))
              (values wrapped defined))

             ((enum-def ,enum-def-list)
              (ppscm (deftype type (mtail->ttype mtail)))
              (ppscm `(define ,(sfsym "unwrap-~A" name)
                        (let ((vald (cenum-vald (ctype-info ,type))))
                          (lambda (arg) (or (assq-ref vald arg) arg)))))
              (ppscm `(define ,(sfsym "wrap-~A" name)
                        (let ((symd (cenum-symd (ctype-info ,type))))
                          (lambda (arg) (or (assq-ref symd arg) arg)))))
              (values (cons name wrapped) (cons name defined)))

             ((enum-def (ident ,enum-name) ,enum-def-list)
              (ppscm (deftype type (mtail->ttype mtail)))
              (ppscm `(define ,(sfsym "unwrap-~A" name)
                        (let ((vald (cenum-vald (ctype-info ,type))))
                          (lambda (arg) (or (assq-ref vald arg) arg)))))
              (ppscm `(define ,(sfsym "wrap-~A" name)
                        (let ((symd (cenum-symd (ctype-info ,type))))
                          (lambda (arg) (or (assq-ref symd arg) arg)))))
              (values (cons* name (w/enum enum-name) wrapped)
                      (cons* name (w/enum enum-name) defined)))

             ((enum-ref (ident ,enum-name))
              (ppscm `(define-public ,(sfsym "wrap-~A" name)
                        ,(sfstr "wrap-enum-~A" enum-name)))
              (ppscm `(define-public ,(sfsym "unwrap-~A" name)
                        ,(sfsym "unwrap-enum-~A" enum-name)))
              (values (cons (w/enum enum-name) wrapped) defined))

             ((void)
              (ppscm `(define-public ,type 'void))
              (ppscm (deftype type* `(cpointer ,type)))
              (values (cons* name (w/* name) wrapped)
                      (cons* name (w/* name) defined)))

             ((typename ,typename)
              (cond
	       ((member typename def-defined)
	        (values wrapped defined))
	       ((member typename defined)
                (let ((aka (string->symbol typename))
                      (atype (strings->symbol typename)))
	          (ppscm (deftype type atype))
	          (if (member (w/* typename) defined)
                      (let* ((name* (strings->symbol name "*"))
                             (aka* (strings->symbol typename "*"))
                             (atype* (strings->symbol typename "*" (*ttag*))))
	                (ppscm (deftype type* atype*))
	                (values (cons* name (w/* name) wrapped)
                                (cons* name (w/* name) defined)))
	              (values (cons name wrapped) (cons name defined)))))
	       (else
	        (let ((xdecl (expand-typerefs udecl (*udict*) defined)))
	          (cnvt-udecl xdecl udict wrapped defined)))))
             (,__
              (sferr "cnvt-udecl missed typedef:\n") (pperr mdecl)
              (values wrapped defined)))))))

     ((ftn-declr? declr)
      (let* ((specl `(decl-spec-list (type-spec ,tspec)))
             (mdecl (udecl->mdecl (sx-list 'udecl #f specl declr)))
             (name (md-label mdecl))
             (return (mdecl->udecl (cons "~ret" (cdr (md-tail mdecl)))))
             (params (cdadar (md-tail mdecl))))
        (cnvt-fctn name return params)
        (values wrapped defined)))

     ((sx-match tspec

        ((struct-def (@ . ,aggr-attr) (ident ,agname) ,field-list)
         (let* ((atype (strings->symbol "struct-" agname (*ttag*)))
                (atype* (strings->symbol "struct-" agname "*" (*ttag*)))
                (field-list (expand-field-list-typerefs field-list))
                (sflds (cnvt-fields (sx-tail field-list) mtail->ttype))
                (agdef (if (packed? aggr-attr)
                           `(cstruct (list ,@sflds) #t)
                           `(cstruct (list ,@sflds)))))
           (cond
            ((bkref-getall attr) =>
             (lambda (name-list)
               (ppscm (deftype atype agdef))
               (ppscm (deftype atype* `(cpointer ,atype)))
               (fold-values
	        (lambda (name wrapped defined)
                  (let ((type (strings->symbol name (*ttag*))))
	            (ppscm `(set! ,type ,atype)))
                  (values (cons name wrapped) (cons name defined)))
	        name-list
                (cons (w/struct agname) wrapped)
                (cons (w/struct agname) defined))))
	    ((not (member (w/struct agname) defined))
             (ppscm (deftype atype agdef))
             (ppscm (deftype atype* `(cpointer ,atype)))
	     (values (cons (w/struct agname) wrapped)
                     (cons (w/struct agname) defined)))
	    (else
	     (values wrapped defined)))))

        ((union-def (@ . ,aggr-attr) (ident ,agname) ,field-list)
         (cond
          ((bkref-getall attr) =>
           (lambda (name-list)
             (let* ((atype (strings->symbol "union-" agname (*ttag*)))
                    (atype* (strings->symbol "union-" agname "*" (*ttag*)))
                    (field-list (expand-field-list-typerefs field-list))
                    (sflds (cnvt-fields (sx-tail field-list) mtail->ttype))
                    (agdef `(cunion (list ,@sflds))))
               (ppscm (deftype atype agdef))
               (ppscm (deftype atype* `(cpointer ,atype)))
	       (fold-values
	        (lambda (name wrapped defined)
                  (let ((type (strings->symbol name (*ttag*)))
                        (syname (string->symbol name)))
	            (ppscm `(set! ,type ,atype)))
                  (values (cons name wrapped) (cons name defined)))
	        name-list
                (cons (w/union agname) wrapped)
                (cons (w/union agname) defined)))))
	  ((not (member (w/union agname) defined))
	   (values (cons (w/union agname) wrapped)
                   (cons (w/union agname) defined)))
	  (else
	   (values wrapped defined))))

        ((enum-def (ident ,enum-name) ,enum-def-list)
         (cond
	  ((member (w/enum enum-name) wrapped)
	   (values wrapped defined))
	  (else
	   (values (cons (w/enum enum-name) wrapped) defined))))

        ((enum-def ,enum-def-list)
	 (values wrapped defined))

        (,__ (values #f #f))) (lambda (a b) a) => values)

     ((memq 'extern sspec)
      (let* ((udecl (expand-typerefs udecl (*udict*) (*defined*)))
             (mdecl (udecl->mdecl udecl))
             (name (md-label mdecl))
             (mtail (cdr (md-tail mdecl))) ; remove (extern)
             (mtail* `((pointer-to) . ,mtail))
             (type* (mtail->ttype mtail*))
             (name* (strings->symbol name "*")))
        (ppscm `(define ,name* ,type*))
        (ppscm
         `(define-public ,(string->symbol name)
	    (let* ((obj
                    (delay
                      (make-cdata ,name* (foreign-pointer-search ,name)))))
	      (case-lambda
	        (() (cdata-ref (force obj) '*))
	        ((arg) (cdata-set! (force obj) '* arg)))))))
      (values wrapped defined))

     ((memq 'const sspec)
      (let ((udecl (expand-typerefs udecl (*udict*) (*defined*)))
            (mdecl (udecl->mdecl udecl)))
        (sferr "skipping const expression: can't do this yet"))
      (values wrapped defined))

     (else
      (sferr "cnvt-udecl: total miss\n") (pperr udecl)
      (values wrapped defined)))))

;; === enums and #defined => lookup

(define dev/null (%make-void-port "w"))

;; given keeper-def names and cpp defs (c-defs) expand the keeper
;; replacemnts down to constants (strings, integers, etc)
(define (gen-lookup-proc mod-def-names udict ddict ext-mods)

  (define typerefs (udict->typedef-names udict))

  (define (eval-code-string code)
    (let* ((tree (with-error-to-port dev/null
		   (lambda () (parse-c99x code typerefs))))
	   (value (and tree (eval-c99-cx tree udict ddict))))
      value))

  ;; @var{mod-dict} is list of CPP defs and enum key/val pairs. It is
  ;; possible for an enum symbol to be used as a macro function so we
  ;; need to first check for integer before trying expand-cpp-macro-ref.
  (sfscm "\n;; access to enum symbols and #define'd constants:\n")
  (let ((st-name (string->symbol (string-append (*prefix*) "-symbol-tab")))
	(sv-name (string->symbol (string-append (*prefix*) "-symbol-val")))
	(defs
	  (fold
	   (lambda (name seed)
	     (let* ((repl (expand-cpp-name name ddict '()))
		    (val (and (string? repl) (eval-code-string repl))))
	       (if val
		   (acons (string->symbol name) val seed)
		   seed)))
	   '() mod-def-names))
	(ext-ftns			; lookup in use-ffi-modules
	 (map
	  (lambda (mod)
	    (list (string->symbol
		   (string-append (m-path->name mod) "-symbol-val")) 'k))
	  ext-mods)))
    (ppscm `(define ,st-name '(,@defs)))
    (sfscm "\n")
    (ppscm `(define ,sv-name (lambda (k) (or (assq-ref ,st-name k) ,@ext-ftns))))
    (sfscm "(export ~A)\n" sv-name)
    ;;
    (nlscm)
    (ppscm
     `(define (unwrap~enum arg)
	(cond
	 ((number? arg) arg)
	 ((symbol? arg) (,sv-name arg))
	 ((cdata? arg) (cdata-ref arg))
	 (else (error "type mismatch")))))))


;; === Parsing the C header(s)

(define fh-cpp-defs
  (cond
   ((string-contains %host-type "darwin")
    (remove (lambda (s) (string-contains s "_ENVIRONMENT_MAC_OS_X_VERSION"))
	    (get-sys-cpp-defs)))
   (else (get-sys-cpp-defs))))

(define fh-cpp-dict
  (map (lambda (ent)
	 (let ((elts (string-split ent #\=)))
	   (cons (car elts) (if (null? (cdr elts)) "" (cadr elts)))))
       fh-cpp-defs))

(define fh-inc-dirs
  (append
   `(,(assq-ref %guile-build-info 'includedir) "/usr/include")
   (get-sys-inc-dirs)))

(define fh-inc-help c99-def-help)

(case (and=> (assoc-ref fh-cpp-dict "__LONG_LONG_WIDTH__") string->number)
  ((64)
   (set! ffi-long-long 'ffi:int64)
   (set! ffi-unsigned-long-long 'ffi:uint64))
  ((32)
   (set! ffi-long-long 'ffi:int32)
   (set! ffi-unsigned-long-long 'ffi:uint32))
  (else
   (sferr "ffi-help: warning: unknown ffi type: long-long")
   (set! ffi-long-long 'ffi:long)
   (set! ffi-unsigned-long-long 'ffi:unsigned-long)))

;; DEBUGGING
(set! fh-inc-dirs (cons "." fh-inc-dirs))

;; @deffn parse-code code [attrs]
;; Parse @var{code}, a Scheme string, using cpp-defs and inc-dirs from
;; @var{attrs}.
;; This procedure is used by @code{parse-includes}.
;; @end deffn
(define* (parse-code code #:optional (attrs '()))
  (let* ((cpp-defs (resolve-attr-val (assq-ref attrs 'cpp-defs)))
	 (inc-dirs (resolve-attr-val (assq-ref attrs 'inc-dirs)))
	 (inc-help (resolve-attr-val (assq-ref attrs 'inc-help)))
	 ;;
	 (pkg-config (assq-ref attrs 'pkg-config))
	 (cpp-defs (append (pkg-config-defs pkg-config) cpp-defs))
	 (inc-dirs (append (pkg-config-incs pkg-config) inc-dirs))
	 ;;
	 (cpp-defs (append cpp-defs fh-cpp-defs))
	 (inc-dirs (append inc-dirs fh-inc-dirs))
	 (inc-help (append inc-help fh-inc-help)))
    (or (with-input-from-string code
	  (lambda ()
	    (call-with-values
		(lambda ()
		  (parse-c99 #:cpp-defs cpp-defs
			     #:inc-dirs inc-dirs
			     #:inc-help inc-help
			     #:mode 'decl
			     #:xdef? #t	; expand CPP-defines (dev-1.02)
			     #:return-defs #t
			     #:show-incs (*show-incs*)
			     #:debug (*debug-parse*)))
	      (lambda (tree defs)
		(*ddict* defs) tree))))
	(fherr "parse failed"))))

;; @deffn parse-includes attrs
;; This routine generates a top-level source string-file with all the includes,
;; parses it, and then merges one level down of includes into the top level,
;; as if the bodies of the incudes had been combined into one file.
;; @end deffn
(define parse-includes
  (let* ((p (node-join
	     (select-kids (node-typeof? 'cpp-stmt))
	     (select-kids (node-typeof? 'include))
	     (select-kids (node-typeof? 'trans-unit))))
	 (merge-inc-bodies
	  (lambda (t) (cons 'trans-unit (apply append (map cdr (p t)))))))
    (lambda (attrs)
      (let* ((inc-files (resolve-attr-val (assq-ref attrs 'include)))
	     (prog (string-join
		    (map
		     (lambda (inc-file)
		       (string-append "#include \"" inc-file "\"\n"))
		     inc-files) "")))
	(and=> (parse-code prog attrs) merge-inc-bodies)))))

;; === main converter ==================

;; => (values wrapped defined)
(define* (process-decls decls udict
			#:optional (wrapped '()) (defined '())
			#:key (declf (lambda (k) #t)))
  (fold-values			  ; from (sxml fold)
   (lambda (name wrapped defined) ; name: "foo_t" or (enum . "foo")
     (catch 'ffi-help-error
       (lambda ()
	 (cond
	  ((and ;; Process the declaration if all conditions met:
	    (declf name)		  ; 1) user wants it
	    (not (member name defined))	  ; 2) not already defined
	    (not (if (pair? name)	  ; 3) not anonymous
		     (string=? "*anon*" (cdr name))
		     (string=? "" name))))
	   (let ((udecl (udict-ref udict name)))
	     (nlscm) (c99scm udecl)
	     (if (*echo-decls*)
		 (sfscm "(if echo-decls (display \"~A\\n\"))\n" name))
	     (cnvt-udecl udecl udict wrapped defined)))
	  (else (values wrapped defined))))
       ;; exception handler:
       (lambda (key fmt . args)
	 (if fmt (apply simple-format (current-error-port)
			(string-append "ffi-help: " fmt "\n") args))
	 (sfscm ";; ... failed.\n")
	 (values wrapped defined))))
   decls wrapped defined))

;; "abc,def" => '(abc def)
(define (comma-split str)
  (map string->symbol (string-split str #\,)))

;; process define-ffi-module expression
;; was intro-ffi
(define (expand-ffi-module-spec path module-options)
  (let* ((script-options (*options*))
	 (dbugl (or (and=> (assq-ref script-options 'debug) comma-split) '()))
	 (attrs (opts->attrs module-options script-options))
	 (incf (or (assq-ref attrs 'inc-filter) #f))
	 (declf (or (assq-ref attrs 'decl-filter) identity))
	 (renamer (or (assq-ref attrs 'renamer) identity))
	 ;;
	 (tree (begin
		 (if (memq 'parse dbugl) (*debug-parse* #t))
		 (and (assq-ref script-options 'show-incs) (*show-incs* #t))
		 (cond
		  ((assq-ref attrs 'include) (parse-includes attrs))
		  ((assq-ref attrs 'api-code) =>
		   (lambda (code) (parse-code code attrs)))
		  (else (fherr "expecing #:include or #:api-code")))))
	 (udict (c99-trans-unit->udict/deep tree))
	 (udecls (c99-trans-unit->udict tree #:inc-filter incf))
	 (ffi-decls (map car udecls))	; just the names, get decls from udict

	 ;; the list of typedefs we will generate (later):
	 (ffimod-defined #f)
	 ;; ext modules [from #:use-modules (ffi cairo)]
	 (ext-mods
	  (fold-right
	   (lambda (opt seed)
	     (if (eq? (car opt) 'use-ffi-module) (cons (cdr opt) seed) seed))
	   '() module-options))
	 (ext-defd
	  (fold
	   (lambda (upath seed)
	     (unless (resolve-module upath)
	       (fherr "module not defined: ~S" upath))
	     (let* ((modul (resolve-module upath #:ensure #f))
		    (pname (m-path->name upath))
		    (vname (string->symbol (string-append pname "-types")))
		    (var (module-ref modul vname)))
	       (append var seed)))
	   '() ext-mods)))

    ;; set globals
    (*prefix* (m-path->name path))
    (*udict* udict)
    (*ddict* (udict-enums->ddict udict (*ddict*)))
    (*renamer* renamer)
    (if (memq 'echo-decls dbugl) (*echo-decls* #t))

    ;; file and module header
    (ffimod-header path module-options)

    ;; Convert and output foreign declarations.
    (call-with-values
	(lambda ()
	  ;; We need to have externs in wrapped because function param
	  ;; type have wrapped types preserved (e.g., enums).
	  ;; swap of udecls with udict failed on glib g???
	  (process-decls ffi-decls udecls ;; udict <= swap failed 01 Dec 2018
			 ;; wrapped and defined:
			 ext-defd (append def-defined ext-defd)
			 ;; declaration filter
			 #:declf declf))
      (lambda (wrapped defined)
	;; Set ffimod-defined for including, but removed built-in types.
	(let* ((bity (car def-defined))	; first built-in type
	       (defd (let loop ((res '()) (defs defined))
		       (if (eq? (car defs) bity) res
			   (loop (cons (car defs) res) (cdr defs))))))
	  (set! ffimod-defined defd))))

    ;; output global constants (from enum and #define)
    (let* ((modd (c99-trans-unit->ddict tree #:inc-filter incf #:skip-fdefs #t))
	   (modd (udict-enums->ddict udecls modd))
	   (xtra (or (assq-ref module-options 'def-keepers) '()))
	   (mod-def-names (fold (lambda (p s) (cons (car p) s)) xtra modd)))
      (gen-lookup-proc mod-def-names udict (*ddict*) ext-mods))

    ;; output list of defined types
    (sfscm "\n(define ~A-types\n  '" (m-path->name path))
    (ugly-print ffimod-defined (*mport*) #:per-line-prefix "   " #:trim-ends #t)
    (sfscm ")\n(export ~A-types)\n" (m-path->name path))

    ;; Return the output port so the compiler can output remaining code.
    #t))


;; === test compiler ================

;; @deffn {Procedure} C-decl->scm code-string => sexp
;; Generate a symbolic expression that evals to a Guile procedure.
;; @example
;; (define fmod-exp (C-fun-decl->proc "double dmod(double, double);"))
;; (define fmod (eval fmod-exp (current-module)))
;; (fmod 2.3 0.5)
;; @end example
;; @end deffn
(define* (C-decl->scm code #:key expand)
  (let ((tree (with-input-from-string code
                (lambda () (parse-c99 #:cpp-defs (get-sys-cpp-defs)
                                      #:inc-dirs (get-sys-inc-dirs))))))
    (and tree
	 (let* ((udict (c99-trans-unit->udict tree))
		(udecl (cdr (last udict)))
		(udecl (if expand (expand-typerefs udecl udict) udecl))
		(str-decl (fh-cnvt-udecl udecl udict))
		(scm-decl (with-input-from-string str-decl read))
		(scm-value (sx-ref scm-decl 2)))
	   scm-value))))
(define C-fun-decl->scm C-decl->scm)

(define-syntax-rule (C-decl c-code-string)
  (eval (C-decl->scm c-code-string) (current-module)))

(define* (fh-cnvt-udecl udecl udict #:key (prefix "fh"))
  (parameterize ((*options* '()) (*wrapped* '()) (*defined* '())
		 (*renamer* identity) (*errmsgs* '()) (*prefix* prefix)
		 (*mport* (open-output-string)) (*udict* udict))
    (cnvt-udecl udecl udict '() '())
    (let ((res (get-output-string (*mport*))))
      (close (*mport*))
      res)))

;; convert string-body of Scheme code to a Scheme expression
;; @example
;; (fh-scm-str->scm-exp "(define a 1)") => '(begin (define a 1))
;; @end example
(define (fh-scm-str->scm-exp str)
  (call-with-input-string str
    (lambda (iport)
      (cons 'begin
	    (let loop ((exp (read iport)))
	      (if (eof-object? exp) '()
		  (cons exp (loop (read iport)))))))))

;; Convert declaration with @var{name} in string-body of C @var{code}
;; to string-body of Scheme code.
;; @example
;; (fh-cnvt-cdecl "sqrt" "double sqrt(double x);") =>
;;   "(define ~sqrt ...)\n (define (sqrt x) ...)"
;; @end example
(define* (fh-cnvt-cdecl->str name code #:key (prefix "fh"))
  (let* ((tree (with-input-from-string code parse-c99))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict name)))
    (fh-cnvt-udecl udecl udict)))

;; like above but then convert to Scheme expression
(define* (fh-cnvt-cdecl name code #:key (prefix "fh"))
  (and=> (fh-cnvt-cdecl->str name code #:prefix prefix) fh-scm-str->scm-exp))

;; === repl compiler ================

;; @deffn {Procedure} load-include-file filename [#pkg-config pkg]
;; This is the functionality that Ludo was asking for: to be at guile
;; prompt and be able to issue
;; @example
;; (use-modules (nyacc lang c99 ffi-help))
;; (load-include-file "cairo.h" #:pkg-config "cairo")
;; @end example
;; @end deffn
;; + Right now the only way would be to generate a file and eval it, because
;;   our code generates strings and not lists.
;; + and=> with-output-to-string  eval
;; + first need to cut up intro-ffi
;; options:
;;   api-code cpp-defs decl-filter
;;   inc-dirs inc-filter inc-help include
;;   library pkg-config renamer
(define* (load-include-file filename
			    #:key pkg-config)
  "- Procedure: load-include-file filename [#:pkg-config pkg]
     This is the functionality that Ludo was asking for: to be at guile
     prompt and be able to issue
          (use-modules (nyacc lang c99 ffi-help))
          (load-include-file \"cairo.h\" #:pkg-config \"cairo\")"
  (parameterize ((*options* '()) (*wrapped* '()) (*defined* '())
		 (*renamer* identity) (*errmsgs* '())
		 (*prefix* "") (*mport* #t) (*udict* '()))
    (let* ((attrs (acons 'include (list filename) '()))
	   (attrs (if pkg-config (acons 'pkg-config pkg-config attrs) attrs))
	   (tree (parse-includes attrs))
	   (udict (c99-trans-unit->udict/deep tree))
	   (udecls (c99-trans-unit->udict tree))
	   (decls (map car udecls))
	   (scmport (mkstemp! (string-copy ",_FH_XXXXXX")))
	   (scmfile (port-filename scmport))
	   (compile-file (@@ (system base compile) compile-file)))
      (*prefix* (symbol->string (gensym "fh-")))
      (*mport* scmport)
      (*udict* udict)
      (ppscm '(use-modules ((system foreign) #:prefix ffi:)))
      (ppscm '(use-modules (system foreign-library)))
      (ppscm `(use-modules ,@Tmodules))
      (ppscm
       `(define (foreign-pointer-search name)
          (let loop ((libs (list #f ,@(pkg-config-libs pkg-config))))
            (cond
             ((null? libs) (fherr "not found: ~s" name))
             ((false-if-exception (foreign-library-pointer (car libs) name)))
             (else (loop (cdr libs)))))))
      (process-decls decls udict '() def-defined)
      (close (*mport*))
      (simple-format #t "wrote ~S; compile and load: ...\n" scmfile)
      (load-compiled (compile-file scmfile #:opts '()))
      (if #f #f))))


;; === file compiler ================

;; Return #t when ffi-file has an mtime greater than that of scm-file
(define (more-recent? ffi-file scm-file)
  ;; copied from ice-9/boot-9.scm
  (let ((stat1 (stat ffi-file)) (stat2 (stat scm-file)))
    (or (> (stat:mtime stat1) (stat:mtime stat2))
	(and (= (stat:mtime stat1) (stat:mtime stat2))
	     (>= (stat:mtimensec stat1)
		 (stat:mtimensec stat2))))))

;; given modules spec, determine if any ffi-module dependencies are
;; outdated
(define (check-deps module-options)
  (let ((ext-modz ;; use filter?
	 (fold-right
	  (lambda (opt seed)
	    (if (eq? (car opt) 'use-ffi-module) (cons (cdr opt) seed) seed))
	  '() module-options))
	(ext-mods (map cdr (filter
			    (lambda (opt) (eq? (car opt) 'use-ffi-module))
			    module-options))))
    (for-each
     (lambda (fmod)
       (let* ((base (string-join (map symbol->string fmod) "/"))
	      (xffi (string-append base ".ffi"))
	      (xscm (string-append base ".scm")))
	 (when (not (access? xscm R_OK))
	   (fherr "compiled dependent ~S not found" fmod))
	 (when (more-recent? xffi xscm)
	   (fherr "dependent ~S needs recompile" xffi)
	   (sleep 2))))
     ext-mods)))

(define-syntax parse-module-options
  (lambda (x)
    (define (key->sym stx)
      (datum->syntax stx (keyword->symbol (syntax->datum stx))))
    (define (ffimod-option? key)
      (and (keyword? key)
	   (member key '(#:api-code
			 #:cpp-defs #:decl-filter #:inc-dirs #:inc-filter
			 #:inc-help #:include #:library #:pkg-config #:renamer
			 #:use-ffi-module #:def-keepers))))
    (define (module-option? key) (keyword? key))

    (syntax-case x ()
      ((_ key val option ...)
       (eq? (syntax->datum #'key) #:use-ffi-module)
       #`(cons
	  (cons (quote #,(key->sym #'key)) (quote val))
	  (parse-module-options option ...)))

      ((_ key val option ...)
       (ffimod-option? (syntax->datum #'key))
       #`(cons
	  (cons (quote #,(key->sym #'key)) val)
	  (parse-module-options option ...)))

      ((_ key val option ...)
       (module-option? (syntax->datum #'key))
       #`(cons
	  (cons key (quote val))
	  (parse-module-options option ...)))

      ((_ key val option ...)
       #'(syntax-error "compile-ffi: expecting keyword"))

      ((_) #''()))))

(define-syntax-rule (define-ffi-module path-list attr ...)
  (let ((module-options (parse-module-options attr ...)))
    (check-deps module-options)
    (expand-ffi-module-spec (quote path-list) module-options)))

(define (string-member-proc . args)
  (lambda (s) (member s args)))

;; to convert symbol-based #:renamer to string-based
(define (string-renamer proc)
  (lambda (s) (string->symbol (proc (symbol->string s)))))

(define call-with-output-file/atomic
  (@@ (system base compile) call-with-output-file/atomic))

;; @deffn {Procedure} compile-ffi-file file [options]
;; This procedure will
;; @end deffn
(define* (compile-ffi-file filename #:optional (options '()))
  (parameterize ((*options* options) (*wrapped* '()) (*defined* '())
		 (*renamer* identity) (*errmsgs* '()) (*prefix* "")
		 (*mport* #t) (*udict* '()) (*ddict* '()))
    (if (not (access? filename R_OK))
	(fherr "ERROR: not found: ~S" filename))
    (call-with-input-file filename
      (lambda (iport)
	(let ((output (or (assq-ref 'output options)
			  (string-append (dirname filename) "/"
					 (basename filename ".ffi") ".scm"))))
	  (call-with-output-file/atomic output
	    (lambda (oport)
	      (*mport* oport)
	      (let ((env (make-fresh-user-module)))
		(eval '(use-modules (nyacc lang c99 ffi-help-cd)) env)
		(let loop ((exp (read iport)))
		  (cond
		   ((eof-object? exp)
		    (display "\n;; --- last line ---\n" oport)
		    output)
		   ((and (pair? exp) (eqv? 'define-ffi-module (car exp)))
		    (eval exp env)
		    (loop (read iport)))
		   (else
		    (newline oport)
		    (pretty-print exp oport)
		    (loop (read iport)))))))))))))

;; --- last line ---
