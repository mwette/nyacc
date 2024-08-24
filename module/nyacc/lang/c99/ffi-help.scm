;;; examples/nyacc/lang/c99/ffi-help.scm

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

(define-module (nyacc lang c99 ffi-help)
  #:export (*ffi-help-version*
	    define-ffi-module
	    compile-ffi-file
	    load-include-file
	    fh-cnvt-udecl fh-cnvt-cdecl fh-cnvt-cdecl->str fh-scm-str->scm-exp
	    string-member-proc string-renamer
	    ;;
	    C-decl->scm C-decl fh-llibs
	    ;;pkg-config-incs pkg-config-defs pkg-config-libs

	    ;;ffi-symmap  		; <= debugging

	    C-fun-decl->scm		; deprecated
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
  #:use-module (srfi srfi-71) ;; was srfi-11
  #:use-module (srfi srfi-37)
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

;; DEBUGGING
(set! fh-inc-dirs (cons "." fh-inc-dirs))

;; maybe change to a record-type
(define *bs-version* (make-parameter 2)) ; scheme bytestructures version
(define *options* (make-parameter '()))
(define *debug-parse* (make-parameter #f)) ; parse debug mode
(define *show-incs* (make-parameter #f))   ; show include directories
(define *echo-decls* (make-parameter #f)) ; add echo-decls code for debugging

(define *prefix* (make-parameter "")) ; name prefix (e.g., prefix-syms)
(define *renamer* (make-parameter identity)) ; renamer from ffi-module

(define *mport* (make-parameter #t))	   ; output module port
(define *udict* (make-parameter '()))	   ; udecl dict
(define *ddict* (make-parameter '()))	   ; cpp-def based dict
(define *defined* (make-parameter '()))    ; defined by define-fh-...
(define *wrapped* (make-parameter '()))    ; wrapped or defined
(define *ttag* (make-parameter "-desc"))

(define *errmsgs* (make-parameter '()))	; list of warnings

(define dev/null
  (let ((port #f))
    (lambda ()
      (unless port (set! port (open-output-file "/dev/null")))
      port)))

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

;; === utilities

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

(define (link-libs)
  (string->symbol (string-append (*prefix*) "-llibs")))

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
  (and (assoc-ref aggr-attr 'attributes)
       (lambda (l) (string-contains (car l) "__packed__" 0))
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
       (if (and (pair? type) (eq? 'bit-field (car type)))
         `(,qq (,(and=> name string->symbol) (,uq ,(caddr type)) ,(cadr type)))
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


(define ffi-long-long #f)
(define ffi-unsigned-long-long #f)
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
    (ffi:uint64 . ,uint64) (ffi-void* . *)))

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


;; === bytestructure support ==================================================

(define bs-typemap
  '(("void" . void) ("float" . float) ("double" . double)
    ("short" . short) ("short int" . short) ("signed short" . short)
    ("signed short int" . short) ("int" . int) ("signed" . int)
    ("signed int" . int) ("long" . long) ("long int" . long)
    ("signed long" . long) ("signed long int" . long)
    ("long long" . long) ("long long int" . long-long)
    ("signed long long" . long-long) ("signed long long int" . long)
    ("unsigned short int" . unsigned-short)
    ("unsigned short" . unsigned-short) ("unsigned int" . unsigned-int)
    ("unsigned" . unsigned-int) ("unsigned long int" . unsigned-long)
    ("unsigned long" . unsigned-long)
    ("unsigned long long int" . unsigned-long-long)
    ("unsigned long long" . unsigned-long-long)
    ("intptr_t" . intptr_t) ("uintptr_t" . uintptr_t)
    ("size_t" . size_t) ("ssize_t" . ssize_t) ("ptrdiff_t" . ptrdiff_t)
    ("int8_t" . int8) ("uint8_t" . uint8) ("int16_t" . int16)
    ("uint16_t" . uint16) ("int32_t" . int32) ("uint32_t" . uint32)
    ("int64_t" . int64) ("uint64_t" . uint64)
    ("float _Complex" . complex64) ("double _Complex" . complex128)
    ;; hacks:
    ("char" . int8) ("signed char" . int8) ("unsigned char" . uint8)
    ("wchar_t" . int) ("char16_t" . int16) ("char32_t" . int32)
    ("_Bool" . int8) ("bool" . int8)))

(define-public (mtail->bs-desc mtail)
  (let ((defined (*defined*)) (ttag (*ttag*)))
    (match mtail
      ;; typename use renamers, ... ???
      (`((pointer-to) (typename ,name))
       (let ((name (rename name)))
	 (if (member (w/* name) defined)
	     (strings->symbol name "*" ttag)
             `(bs:pointer ,(mtail->bs-desc (cdr mtail))))))
      (`((pointer-to) (void))
       `(bs:pointer 'void))
      (`((pointer-to) (fixed-type "char"))
       `(bs:pointer int8))
      (`((pointer-to) (fixed-type ,fx-name))
       `(bs:pointer ,(assoc-ref bs-typemap fx-name)))
      (`((pointer-to) (float-type ,fx-name))
       `(bs:pointer ,(assoc-ref bs-typemap fx-name)))
      (`((pointer-to) (function-returning (param-list . ,params)) . ,tail)
       (call-with-values
           (lambda () (function*-wraps (mdecl->udecl (cons "_" tail)) params))
         (lambda (ptr->proc proc->ptr)
           `(bs:pointer (fh:function ,ptr->proc ,proc->ptr)))))
      (`((pointer-to) (pointer-to) (function-returning . ,rest) . ,rest)
       `(bs:pointer 'void)) ;; FIXME: I can probably do this now
      (`((pointer-to) (struct-ref (ident ,name)))
       (if (member (w/struct name) (*defined*))
	   `(bs:pointer ,(strings->symbol "struct-" name (*ttag*)))
	   `(bs:pointer 'void)))
      (`((pointer-to) (union-ref (ident ,name)))
       (if (member (w/union name) (*defined*))
	   `(bs:pointer ,(strings->symbol "union-" name (*ttag*)))
	   `(bs:pointer 'void)))
      (`((pointer-to) . ,rest)
       `(bs:pointer ,(mtail->bs-desc rest)))
      ;; In C99 array parameters are interpreted as pointers.
      (`((array-of ,n) (fixed-type ,name))
       (let ((ns (const-expr->number n)))
	 (cond
	  ((string=? name "char") `(bs:vector ,ns int8))
	  ((string=? name "unsigned char") `(bs:vector ,ns uint8))
	  (else `(bs:vector ,ns ,(mtail->bs-desc `((fixed-type ,name))))))))
      (`((array-of ,n) . ,rest)
       `(bs:vector ,(const-expr->number n) ,(mtail->bs-desc rest)))
      (`((array-of) . ,rest)
       `(bs:vector 0 ,(mtail->bs-desc rest)))
      (`((bit-field ,size) . ,rest)
       `(bit-field ,(const-expr->number size) ,(mtail->bs-desc rest)))
      (`((extern) . ,rest) (mtail->bs-desc rest))
      (__
       (sx-match (car mtail)
         ((typename ,name)
          (let ((name (rename name)))
	    (cond
             ((assoc-ref bs-typemap name))
             ((member name defined) (strings->symbol name ttag))
             (else (let* ((udecl `(udecl (decl-spec-list (type-spec . ,mtail))
                                         (init-declr (ident "_"))))
                          (xdecl (expand-typerefs udecl (*udict*) defined))
                          (mdecl (udecl->mdecl xdecl)))
                     (mtail->bs-desc (md-tail mdecl)))))))
         ((void) ''void)
         ((fixed-type "char") 'int)
         ((fixed-type "unsigned char") 'unsigned-int)
         ((fixed-type ,name) `(fhval-base-type ',(cstrnam->symnam (cfix name))))
         ((float-type ,name) `(fhval-base-type ',(cstrnam->symnam (cfix name))))
         ((enum-def (ident ,ident) ,rest) '(fhval-base-type 'int))
         ((enum-def ,elts) '(fhval-base-type 'int))
         ((enum-ref ,name) '(fhval-base-type 'int))
         ((struct-def (@ . ,attr) (ident ,struct-name) ,field-list)
          (mtail->bs-desc `((struct-def (@ . ,attr) ,field-list))))
         ((struct-def (@ . ,attr) (field-list . ,fields))
          (let ((fields (cnvt-fields fields mtail->bs-desc)))
            `(bs:struct ,(packed? attr) (list ,@fields))))
         ((struct-ref (ident ,struct-name))
          (string->symbol (string-append "struct-" struct-name ttag)))
         ((union-def (ident ,union-name) ,field-list)
          (mtail->bs-desc `((union-def ,field-list))))
         ((union-def (field-list . ,fields))
          (list 'bs:union `(list ,@(cnvt-fields fields mtail->bs-desc))))
         ((union-ref (ident ,union-name))
          (string->symbol (string-append "union-" union-name ttag)))
         (,otherwise
          (fherr "mtail->bs-desc missed:\n~A" (ppstr mtail))))))))


;; === hookup ==================================================================

(define target 'bs)

(define Tmodules
  (case target
    ((bs) '((bytestructures guile) (system ffi-help-rt)))
    (else (error "bad target" target))))

(define Tpointer
  (case target
    ((bs) 'bs:pointer)
    (else (error "bad target" target))))

(define mtail->target
  (case target
    ((bs) mtail->bs-desc)
    (else (error "bad target" target))))


;; === output scheme module header =============================================

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
           ((null? libs) (fherr "no library for ~s" name))
           ((false-if-exception (foreign-library-pointer (car libs) name)))
           (else (loop (cdr libs)))))))
    (sfscm "\n")
    (ppscm
     `(define (unwrap~enum arg)
        (or (assq-ref ,(strings->symbol (m-path->name path) "-symbol-tab") arg)
            arg)))
    (if (*echo-decls*) (sfscm "(define echo-decls #t)\n\n"))))


;; === fh type generation ======================================================

(define (fhscm-ref-deref typename)
  (let* ((typename (rename typename))
	 (type* (strings->symbol typename "*"))
	 (make* (strings->symbol "make-" typename "*"))
	 (type (strings->symbol typename))
	 (make (strings->symbol "make-" typename)))
    (ppscm `(fh-ref<=>deref! ,type* ,make* ,type ,make))))

(define (fhscm-gen-def kind name)
  (let* ((name (rename name))
	 (strname (if (string? name) name (symbol->string name)))
	 (symname (if (string? name) (string->symbol name) name))
         (desc (strings->symbol strname (*ttag*)))
	 (pred (strings->symbol strname "?"))
	 (make (strings->symbol "make-" strname)))
    (upscm (list kind symname desc pred make))
    (upscm `(export ,symname ,pred ,make))))

(define (fhscm-def-pointer name)
  (fhscm-gen-def 'define-fh-pointer-type name))

(define (fhscm-def-compound name)
  (fhscm-gen-def 'define-fh-compound-type name))

(define (fhscm-def-vector name)
  (fhscm-gen-def 'define-fh-vector-type name))

(define* (cnvt-param/fh-decl udecl #:optional namer)
  (mtail->target
   (md-tail (udecl->mdecl
             (udecl-rem-type-qual
              (expand-typerefs udecl (*udict*) ffi-defined))
             #:namer namer))))

;; fhscm-def-function* moved below


;; === ffi-helper code gen =====================================================

;; This routine will munge the fields and then perform typeref expansion.
;; `defined' here means has -desc (what?)
(define (expand-field-list-typerefs field-list)
  (let ((udict (*udict*)) (defined (*defined*)))
    (cons 'field-list
	  (fold-right
	   (lambda (udecl seed)
	     (cons (expand-typerefs udecl udict defined) seed))
	   '() (clean-and-unitize-fields (sx-tail field-list))))))

;; @deffn {Procedure} cnvt-aggr-def kind typename aggr-name field-list
;; Output an aggregate definition, where
;; @var{kind} is a string of @code{"struct"} or @code{"union"},
;; @var{typename} is a string for the typename, or @code{#f},
;; @var{aggr-name} is a string for the struct or union name, or @code{#f},
;; and @var{field-list} is the field-list from the C syntax tree.
;; @end deffn
(define (cnvt-aggr-def kind attr typename aggr-name field-list)
  (let* ((field-list (expand-field-list-typerefs field-list))
	 (sflds (cnvt-fields (sx-tail field-list) mtail->target))
	 (kind-s (symbol->string kind))
	 (aggrname (and aggr-name (string-append kind-s "-" aggr-name)))
	 (bs-kind (string->symbol (string-append "bs:" kind-s)))
	 (ty-desc (and typename (strings->symbol typename (*ttag*))))
	 (ty*-desc (and typename (strings->symbol typename "*" (*ttag*))))
	 (ag-desc (and aggrname (strings->symbol aggrname (*ttag*))))
	 (ag*-desc (and aggrname (strings->symbol aggrname "*" (*ttag*))))
	 (cattr (assoc-ref attr 'attributes)) ;; __attributes__
	 (bs-spec (if (packed? attr)
		      (list bs-kind #t `(list ,@sflds))
		      (list bs-kind `(list ,@sflds)))))
    (cond
     ((and typename aggr-name)
      (ppscm `(define-public ,ty-desc ,bs-spec))
      (fhscm-def-compound typename)
      (ppscm `(define-public ,ty*-desc (bs:pointer ,ty-desc)))
      (fhscm-def-pointer (sw/* typename))
      (fhscm-ref-deref typename)
      (ppscm `(define-public ,ag-desc ,ty-desc))
      (fhscm-def-compound aggrname)
      (ppscm `(define-public ,ag*-desc ,ty*-desc))
      (fhscm-def-pointer (sw/* aggrname))
      (fhscm-ref-deref aggrname))
     (typename
      (ppscm `(define-public ,ty-desc ,bs-spec))
      (fhscm-def-compound typename)
      (ppscm `(define-public ,ty*-desc (bs:pointer ,ty-desc)))
      (fhscm-def-pointer (sw/* typename))
      (fhscm-ref-deref typename))
     (aggr-name
      (ppscm `(define-public ,ag-desc ,bs-spec))
      (fhscm-def-compound aggrname)
      (ppscm `(define-public ,ag*-desc (bs:pointer ,ag-desc)))
      (fhscm-def-pointer (sw/* aggrname))
      (fhscm-ref-deref aggrname)))))

(define (cnvt-struct-def attr typename struct-name field-list)
  (cnvt-aggr-def 'struct attr typename struct-name field-list))

(define (cnvt-union-def attr typename union-name field-list)
  (cnvt-aggr-def 'union attr typename union-name field-list))


(define (fhscm-def-enum name name-val-list)
  (let ((nvl (sfsym "~A-enum-nvl" name))
        (vnl (sfsym "~A-enum-vnl" name)))
    (ppscm `(define ,nvl '(,@name-val-list)))
    (ppscm `(define ,vnl
              (map (lambda (pair) (cons (cdr pair) (car pair))) ,nvl)))
    (ppscm `(define-public (,(sfsym "unwrap-~A" name) n)
              (cond
               ((symbol? n)
                (or (assq-ref ,(sfsym "~A-enum-nvl" name) n)
                    (fherr "bad arg: ~A" n)))
               ((integer? n) n)
               (else (error "bad arg")))))
    (ppscm `(define-public (,(sfsym "wrap-~A" name) v)
              (assq-ref ,(sfsym "~A-enum-vnl" name) v)))))

(define (cnvt-enum-def typename enum-name enum-def-list)
  (let* ((udict (*udict*))
	 (ddict (*ddict*))
	 (name-val-l
	  (map
	   (lambda (def)
	     (let* ((n (sx-ref (sx-ref def 1) 1))
		    (x (sx-ref def 2))
		    (v (eval-c99-cx x udict ddict)))
	       (unless v
		 (throw 'ffi-help-error "unable to generate constant for ~S" n))
	       (cons (string->symbol n) v)))
	   (cdr (canize-enum-def-list enum-def-list udict ddict)))))
    (cond
     ((and typename enum-name)
      (fhscm-def-enum typename name-val-l)
      (sfscm "(define-public unwrap-enum-~A unwrap-~A)\n" enum-name typename)
      (sfscm "(define-public wrap-enum-~A wrap-~A)\n" enum-name typename))
     (typename
      (fhscm-def-enum typename name-val-l))
     (enum-name
      (fhscm-def-enum (string-append "enum-" enum-name) name-val-l)))))


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
      (`((pointer-to) . ,rest) 'ffi-void*)
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
    (`((array-of ,dim) . ,rest) 'ffi-void*)
    (__ (cnvt mtail))))

(define (maybe-function-pointer name)
  (let* ((udecl (expand-typerefs
                 `(udecl (decl-spec-list (type-spec (typename ,name)))
                         (init-declr (ident "_"))) (*udict*) '()))
         (mdecl (udecl->mdecl udecl)))
    (match (md-tail mdecl)
      (`((pointer-to) (function-returning ,params) . ,rest)
       `(lambda (arg)
          (let ((arg (if (procedure? arg)
                         (,(strings->symbol "make-" name) arg)
                         arg)))
            ((fht-unwrap ,(string->symbol name)) arg))))
      (__ `(fht-unwrap ,(string->symbol name))))))

(define (mtail->fh-unwrapper mtail)
  (let ((wrapped (*wrapped*)) (defined (*defined*)))
    (match (car mtail)
      (`(fixed-type ,name) 'unwrap~number)
      (`(float-type ,name) 'unwrap~number)
      (`(void) #f)
      (`(typename ,name)
       (cond
	((member name def-defined) 'unwrap~number)
	((member name defined)
         ;;`(fht-unwrap ,(string->symbol name))
         (maybe-function-pointer name))
	((member name wrapped) (strings->symbol "unwrap-" name))
	(else #f)))
      (`(enum-def (ident ,name) ,_)
       (cond
	((member (w/enum name) wrapped) (strings->symbol "unwrap-enum-" name))
	(else 'unwrap~enum)))
      (`(enum-def ,_) 'unwrap~enum)
      (`(enum-ref (ident ,name))
       (cond
	((member (w/enum name) wrapped) (strings->symbol "unwrap-enum-" name))
	(else 'unwrap~enum)))
      (`(struct-ref (ident ,name)) 'fh-object-pointer)
      (`(union-ref (ident ,name)) 'fh-object-pointer)
      (`(pointer-to) 'unwrap~pointer)
      (`(array-of ,size) 'unwrap~array)
      (`(array-of) 'unwrap~array)
      ;; not expected
      (`(struct-def . ,_) 'fh-object-pointer)
      (`(union-def . ,_) 'fh-object-pointer)
      (otherwise
       (fherr "mtail->fh-unwrapper: missed:\n~A" (ppstr mtail))))))

(define (mtail->fh-wrapper mtail)
  (let ((wrapped (*wrapped*)) (defined (*defined*)))
    (match mtail
      ;;(`((fixed-type ,name)) (strings->symbol "make-" (noblanks (cfix name))))
      ;;(`((float-type ,name)) (strings->symbol "make-" (noblanks (cfix name))))
      (`((fixed-type ,name)) #f)
      (`((float-type ,name)) #f)
      (`((void)) #f)
      (`((typename ,name))
       (cond
        ((member name def-defined) #f)
	((member name defined) (strings->symbol "make-" name))
	((member name wrapped) (strings->symbol "wrap-" name))
	(else #f)))
      (`((enum-def (ident ,name) ,rest))
       (and (member (w/enum name) wrapped) (strings->symbol "wrap-enum-" name)))
      (`((enum-def ,_)) #f)
      (`((enum-ref (ident ,name)))
       (cond
        ((member (w/enum name) wrapped) (strings->symbol "wrap-enum-" name))
	(else 'wrap-enum)))
      (`((pointer-to) (typename ,tname))
       (cond
        ((member tname ffi-defined) #f)
	((member (w/* tname) defined) (strings->symbol "make-" tname "*"))
	((member (w/* tname) wrapped) (strings->symbol "wrap-" tname "*"))
	(else #f)))
      (`((pointer-to) (struct-ref (ident ,aggr-name) . ,rest))
       (cond
        ((member (w/struct aggr-name) wrapped)
         (strings->symbol "make-" (sw/struct* aggr-name)))
	(else #f)))
      (`((pointer-to) (union-ref (ident ,aggr-name) . ,rest))
       (cond
	((member (w/union aggr-name) wrapped)
         (strings->symbol "make-" (sw/union* aggr-name)))
	(else #f)))
      (`((pointer-to) . ,otherwise) #f)
      (`((array-of) . ,rest) ;; cross fingers
       (mtail->fh-wrapper `((pointer-to) . ,rest)))
      (otherwise
       (fherr "mtail->fh-wrapper missed:\n~A" (ppstr mtail))))))

(define* (udecl->ffi-decl udecl #:optional (keepers ffi-defined))
  (mtail->ffi-decl
   (md-tail (udecl->mdecl
             (udecl-rem-type-qual
              (expand-typerefs udecl (*udict*) keepers))))))

(define* (udecl->fh-unwrapper udecl #:optional (keepers '()))
  (mtail->fh-unwrapper
   (md-tail (udecl->mdecl
             (udecl-rem-type-qual
              (expand-typerefs udecl (*udict*) keepers))))))

(define* (udecl->fh-wrapper udecl #:optional (keepers '()))
  (mtail->fh-wrapper
   (md-tail (udecl->mdecl
             (udecl-rem-type-qual
              (expand-typerefs udecl (*udict*) (*wrapped*)))))))


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
  (values (udecl->fh-wrapper return (*wrapped*))
          (map (lambda (p) (udecl->fh-unwrapper p (*wrapped*))) params)))

(define (function-callbacks return params)
  (values (udecl->fh-unwrapper return (*wrapped*))
          (map (lambda (p) (udecl->fh-wrapper p (*wrapped*))) params)))

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
         (urap-par (fold-right (lambda (n u s) (if u (cons `(,n (,u ,n)) s) s))
                               '() names exec-par))
         (wrap-par (fold-right (lambda (n u s) (if u (cons `(,n (,u ,n)) s) s))
                               '() names cbak-par))
         (call `(~proc ,@names))
         (va-call `(apply ~proc ,names (map cdr ~rest))))
    (values
     ;; procedure->pointer
     `(lambda (~proc)
        (ffi:procedure->pointer
         ,decl-ret
         (lambda ,names (let ,wrap-par ,(if cbak-ret (list cbak-ret call) call)))
         (list ,@decl-par)))
     (if varargs?
         ;; pointer->procedure (varargs)
         `(lambda (~fptr)
	    (lambda (,@names . ~rest)
	      (let ((~proc (ffi:pointer->procedure
                            ,decl-ret ~fptr
                            (append ,@decl-par (map car ~rest)))
	                   ,@urap-par))
		,(if exec-ret (list exec-ret va-call) va-call))))
         ;; pointer->procedure
         `(lambda (~fptr)
	    (let ((~proc (ffi:pointer->procedure
                          ,decl-ret ~fptr (list ,@decl-par))))
	      (lambda ,names
	        (let ,urap-par
		  ,(if exec-ret (list exec-ret call) call)))))))))


(define (cnvt-fctn name return params)
  ;; can't use function*-wraps just because of the delay :(
  (define varargs? (and (pair? params) (equal? (last params) '(ellipsis))))
  (let* ((params names (setup-function return params))
         (decl-ret decl-par (function-decls return params))
         (exec-ret exec-par (function-execs return params))
         (urap-par (fold-right (lambda (n u s) (if u (cons `(,n (,u ,n)) s) s))
                               '() names exec-par))
         (call `((force ~proc) ,@names))
	 (va-call `(apply (force ~proc) ,@names (map cdr ~rest))))
    (ppscm
     `(define-public ,(string->symbol name)
        ,(if varargs?
             `(lambda (,@names . ~rest)
	        (let ((~proc (ffi:pointer->procedure
                              ,decl-ret (foreign-pointer-search ,name)
                              (append ,@decl-par (map car ~rest))))
	              ,@urap-par)
		  ,(if exec-ret (list exec-ret va-call) va-call)))
	     `(let ((~proc
                     (delay (ffi:pointer->procedure
                             ,decl-ret (foreign-pointer-search ,name)
                             (list ,@decl-par)))))
                (lambda ,names
	          (let ,urap-par ,(if exec-ret (list exec-ret call) call)))))))))


;; === the main conversion driver ==============================================

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

  (let* ((tag attr specl declr (split-udecl udecl))
         (sspec tqual tspec (specl-props specl)))

    (cond

     ((memq 'typedef sspec)
      (let* ((specl `(decl-spec-list (type-spec ,tspec)))
             (mdecl (udecl->mdecl (sx-list 'udecl #f specl declr)))
             (label (md-label mdecl))
             (name (string->symbol label))
             (desc (strings->symbol label (*ttag*)))
             (pred (strings->symbol label "?"))
             (make (strings->symbol "make-" label))
             (name* (strings->symbol label "*"))
             (desc* (strings->symbol label "*" (*ttag*)))
             (pred* (strings->symbol label "?"))
             (make* (strings->symbol "make-" label "*"))
             (mtail (md-tail mdecl)))

        (match mtail

          (`((array-of ,dim) . ,rest)
           (ppscm `(define-public ,desc ,(mtail->target mtail)))
           (ppscm `(define-fh-vector-type ,name ,desc ,pred ,make))
           (ppscm `(export ,name ,pred ,make))
           (ppscm `(define-public ,desc* (bs:pointer ,desc)))
           (ppscm `(define-fh-pointer-type ,name* ,desc* ,pred* ,make*))
           (ppscm `(export ,name* ,pred* ,make*))
           (ppscm `(fh-ref<=>deref! ,name* ,make* ,name ,make))
           (values (cons label wrapped) (cons label defined)))

          (`((function-returning (param-list . ,params)) . ,rest)
           (let ((return (mdecl->udecl (cons "_" rest))))
             (call-with-values (lambda () (function*-wraps return params))
               (lambda (ptr->proc proc->ptr)
                 (ppscm `(define-public ,desc
                           (fh:function ,ptr->proc ,proc->ptr)))
                 (ppscm `(define-fh-function-type ,name ,desc ,pred ,make))
                 (ppscm `(define-public ,desc* (bs:pointer ,desc)))
                 (ppscm `(define-fh-pointer-type ,name* ,desc* ,pred* ,make*))
                 (ppscm `(export ,name* ,pred* ,make*))
                 ;; FIXME: fh-ref<=>deref! ???
                 )))
           (values (cons* label (w/* label) wrapped)
                   (cons* label (w/* label) defined)))

          (`((pointer-to) (function-returning (param-list . ,params)) . ,rest)
           (let ((return (mdecl->udecl (cons "_" rest))))
             (call-with-values (lambda () (function*-wraps return params))
               (let ((*desc (strings->symbol "*" label (*ttag*)))
                     (*name (strings->symbol "*" label))
                     (*pred (strings->symbol "*" label "?"))
                     (*make (strings->symbol "make-*" label)))
                 (lambda (ptr->proc proc->ptr)
                   (ppscm `(define-public ,*desc
                             (fh:function ,ptr->proc ,proc->ptr)))
                   (ppscm `(define-fh-function-type ,*name ,*desc ,*pred ,*make))
                   (ppscm `(define-public ,desc (bs:pointer ,*desc)))
                   (ppscm `(define-fh-pointer-type ,name ,desc ,pred ,make))
                   (ppscm `(export ,name ,pred ,make))
                   (ppscm `(fh-ref<=>deref! ,name ,make ,*name ,*make))
                   ))))
           (values (cons label wrapped) (cons label defined)))

          (`((pointer-to) . ,rest)
           (ppscm `(define-public ,desc ,(mtail->target mtail)))
           (ppscm `(define-fh-pointer-type ,name ,desc ,pred ,make))
           (ppscm `(export ,name ,pred ,make))
           ;; FIXME: fh-ref<=>deref! ???
           (case (caar rest)
             ((fixed-type float-type) (values wrapped defined)) ;; ???
             (else (values (cons label wrapped) (cons label defined)))))

          (__
           (sx-match (car mtail)
             ((struct-def (@ . ,attr) (ident ,aggr-name) ,field-list)
              ;;(ppscm `(define-public ,desc ,(mtail->target mtail)))
              ;;(ppscm `(define-public ,desc* (bs:pointer ,desc)))
              (cnvt-struct-def attr label aggr-name field-list)
              (values (cons* label (w/* label) (w/struct aggr-name)
                             (w/struct* aggr-name) wrapped)
                      (cons* label (w/* label) (w/struct aggr-name)
                             (w/struct* aggr-name) defined)))

             ((struct-def ,field-list)
              ;;(ppscm `(define-public ,desc ,(mtail->target mtail)))
              ;;(ppscm `(define-public ,desc* (bs:pointer ,desc)))
              (cnvt-struct-def attr label #f field-list)
              (values  (cons* label (w/* label) wrapped)
                       (cons* label (w/* label) defined)))

             ((union-def (ident ,aggr-name) ,field-list)
              ;;(ppscm `(define-public ,desc ,(mtail->target mtail)))
              ;;(ppscm `(define-public ,desc* (bs:pointer ,desc)))
              (cnvt-union-def attr label aggr-name field-list)
              (values (cons* label (w/* label) (w/union aggr-name)
                             (w/union* aggr-name) wrapped)
                      (cons* label (w/* label) (w/union aggr-name)
                             (w/union* aggr-name) defined)))

             ((union-def ,field-list)
              ;;(ppscm `(define-public ,desc ,(mtail->target mtail)))
              ;;(ppscm `(define-public ,desc* (bs:pointer ,desc)))
              (cnvt-union-def attr label #f field-list)
              (values (cons* label (w/* label) wrapped)
                      (cons* label (w/* label) defined)))

             ((struct-ref (ident ,aggr-name))
              (cond
               ((member (w/struct aggr-name) defined)
                ;; defined previously
                (ppscm `(define-public ,desc ,(mtail->target mtail)))
	        (ppscm `(define-public ,desc*
                          ,(sfsym "struct-~A*-desc" aggr-name)))
                (fhscm-def-compound label))
               ((udict-struct-ref udict aggr-name)
                => ;; defined later
                (lambda (decl)
                  (bkref-extend! decl label)
                  (ppscm `(define-public ,desc 'void))
                  (ppscm `(define-public ,desc* (bs:pointer (delay ,desc))))))
               (else ;; not defined
                (ppscm `(define-public ,desc 'void))
                (ppscm `(define-public ,desc* (bs:pointer ,desc)))))
              (fhscm-def-pointer (sw/* label))
              (values (cons* label (w/* label) wrapped)
                      (cons* label (w/* label) defined)))

             ((union-ref (ident ,aggr-name))
              (cond
               ((member (w/union aggr-name) defined)
                ;; defined previously
                (ppscm `(define-public ,desc ,(mtail->target mtail)))
	        (ppscm `(define-public ,desc*
                          ,(sfsym "union-~A*-desc" aggr-name)))
                (fhscm-def-compound label))
               ((udict-union-ref udict aggr-name)
                => ;; defined later
                (lambda (decl)
                  (bkref-extend! decl label)
                  (ppscm `(define-public ,desc 'void))
                  (ppscm `(define-public ,desc* (bs:pointer (delay ,desc))))))
               (else ;; not defined
                (ppscm `(define-public ,desc 'void))
                (ppscm `(define-public ,desc* (bs:pointer ,desc)))))
              (fhscm-def-pointer (sw/* label))
              (values (cons* label (w/* label) wrapped)
                      (cons* label (w/* label) defined)))

             (((fixed-type float-type) ,basename)
              (ppscm `(define-public ,desc ,(mtail->target mtail)))
              (values wrapped defined))

             ((enum-def ,enum-def-list)
              (ppscm `(define-public ,desc ,(mtail->target mtail)))
              (cnvt-enum-def name #f enum-def-list)
              (values (cons label wrapped) defined))

             ((enum-def (ident ,enum-name) ,enum-def-list)
              (ppscm `(define-public ,desc ,(mtail->target mtail)))
              (cnvt-enum-def name enum-name enum-def-list)
              (values (cons* label (w/enum enum-name) wrapped) defined))

             ((enum-ref (ident ,enum-name))
              (ppscm `(define-public ,(sfscm "wrap-~A" name)
                        ,(sfscm "wrap-enum-~A" enum-name)))
              (ppscm `(define-public ,(sfscm "unwrap-~A" name)
                        ,(sfscm "unwrap-enum-~A" enum-name)))
              (values (cons (w/enum enum-name) wrapped) defined))

             ((void)
              (ppscm `(define-public ,desc 'void))
              (ppscm `(define-public ,desc* (bs:pointer ,desc)))
              (fhscm-def-pointer (sw/* label))
              (values (cons* label (w/* label) wrapped)
                      (cons* label (w/* label) defined)))

             ((typename ,typename)
              (cond
	       ((member typename def-defined)
	        (values wrapped defined))
	       ((member typename defined)
                (let ((aka (string->symbol typename))
                      (adesc (strings->symbol typename (*ttag*))))
	          (ppscm `(define-public ,desc ,adesc))
                  (ppscm `(define-fh-type-alias ,name ,aka))
                  (ppscm `(export ,name))
	          (if (member (w/* typename) defined)
                      (let* ((name* (strings->symbol label "*"))
                             (aka* (strings->symbol typename "*"))
                             (adesc* (strings->symbol typename "*" (*ttag*)))
                             (amake (strings->symbol "make-" typename))
                             (amake* (strings->symbol "make-" typename "*")))
	                (ppscm `(define-public ,desc* ,adesc*))
                        (ppscm `(define-fh-type-alias ,name* ,aka*))
                        (ppscm `(fh-ref<=>deref! ,name* ,make* ,name ,make))
                        (ppscm `(export ,name*))
	                (values (cons* label (w/* label) wrapped)
                                (cons* label (w/* label) defined)))
	              (values (cons label wrapped) (cons label defined)))))
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
             (return (mdecl->udecl (cons "_" (cdr (md-tail mdecl)))))
             (params (cdadar (md-tail mdecl))))
        (cnvt-fctn name return params)
        (values wrapped defined)))

     ((sx-match tspec

        ((struct-def (@ . ,aggr-attr) (ident ,aggr-name) ,field-list)
         (cond
          ((bkref-getall attr) =>
           (lambda (name-list)
	     (cnvt-aggr-def 'struct aggr-attr #f aggr-name field-list)
             (fold-values
	      (lambda (name wrapped defined)
                (let ((adesc (strings->symbol "struct-" aggr-name (*ttag*)))
                      (desc (strings->symbol name (*ttag*))))
	          (ppscm `(set! ,desc ,adesc))
	          (fhscm-def-compound name)
	          (fhscm-ref-deref name))
                (values (cons name wrapped) (cons name defined)))
	      name-list
              (cons (w/struct aggr-name) wrapped)
              (cons (w/struct aggr-name) defined))))
	  ((not (member (w/struct aggr-name) defined))
	   (cnvt-aggr-def 'struct aggr-attr #f aggr-name field-list)
	   (values (cons (w/struct aggr-name) wrapped)
                   (cons (w/struct aggr-name) defined)))
	  (else
	   (values wrapped defined))))

        ((union-def (@ . ,aggr-attr) (ident ,aggr-name) ,field-list)
         (cond
          ((bkref-getall attr) =>
           (lambda (name-list)
	     (cnvt-aggr-def 'union aggr-attr #f aggr-name field-list)
	     (fold-values
	      (lambda (name wrapped defined)
                (let ((adesc (strings->symbol "union-" aggr-name (*ttag*)))
                      (desc (strings->symbol name (*ttag*)))
                      (pred (strings->symbol name "?"))
                      (make (strings->symbol "make-" name))
                      (syname (string->symbol name)))
	          (ppscm `(set! ,desc ,adesc))
	          (fhscm-def-compound name)
	          (fhscm-ref-deref name)
                  (ppscm `(define-fh-compound-type ,syname ,desc ,pred ,make))
                  (ppscm `(export ,syname ,pred ,make)))
                (values (cons name wrapped) (cons name defined)))
	      name-list
              (cons (w/union aggr-name) wrapped)
              (cons (w/union aggr-name) defined))))
	  ((not (member (w/union aggr-name) defined))
	   (cnvt-aggr-def 'union aggr-attr #f aggr-name field-list)
	   (values (cons (w/union aggr-name) wrapped)
                   (cons (w/union aggr-name) defined)))
	  (else
	   (values wrapped defined))))

        ((enum-def (ident ,enum-name) ,enum-def-list)
         (cond
	  ((member (w/enum enum-name) wrapped)
	   (values wrapped defined))
	  (else
	   (cnvt-enum-def #f enum-name enum-def-list)
	   (values (cons (w/enum enum-name) wrapped) defined))))

        ((enum-def ,enum-def-list)
	 (values wrapped defined))

        (,__ (values #f #f))) (lambda (a b) a) => values)

     ((memq 'extern sspec)
      (let* ((udecl (expand-typerefs udecl (*udict*) (*defined*)))
             (mdecl (udecl->mdecl udecl))
             (label (md-label mdecl))
             (name (string->symbol label))
             (mtail (cdr (md-tail mdecl))) ; remove (extern)
             (mtail* `((pointer-to) . ,mtail))
             (desc* (mtail->target mtail*))
             (name* (strings->symbol label "*"))
             (pred* (strings->symbol label "*?"))
             (make* (strings->symbol "make-" label "*")))
        (ppscm `(define-fh-pointer-type ,name* ,desc* ,pred* ,make*))
        (ppscm
         `(define-public ,name
	    (let* ((obj
                    (delay (value-at (,make* (foreign-pointer-search ,label))))))
	      (case-lambda
	        (() (fh-object-ref (force obj)))
	        ((arg) (fh-object-set! (force obj) arg)))))))
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

;; given keeper-def names and cpp defs (c-defs) expand the keeper
;; replacemnts down to constants (strings, integers, etc)
(define (gen-lookup-proc mod-def-names udict ddict ext-mods)

  (define typerefs (udict->typedef-names udict))

  (define (eval-code-string code)
    (let* ((tree (with-error-to-port (dev/null)
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
     `(define (unwrap~enum obj)
	(cond
	 ((number? obj) obj)
	 ((symbol? obj) (,sv-name obj))
	 ((fh-object? obj) (struct-ref obj 0)) ;; ???
	 (else (error "type mismatch")))))))


;; === Parsing the C header(s)

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

(define fh-llibs (delay '()))

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
		(eval '(use-modules (nyacc lang c99 ffi-help)) env)
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
