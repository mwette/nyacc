;;; examples/nyacc/lang/c99/ffi-help.scm
;;;
;;; Copyright (C) 2016-2017 Matthew R. Wette
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, see <http://www.gnu.org/licenses/>

;; WARNING: this is a prototype in development: anything goes right now

;; @table code
;; @item mspec->fh-wrapper
;; generates code to apply wrapper to objects returned from foreign call
;; @item mspec->fh-unwrapper
;; generated code to apply un-wrapper to arguments for foreign call
;; @end table

;; also have in (bytestructures guile ffi)
;; bytestructure->descriptor->ffi-descriptor
;; bs:pointer->proc

;; For enum typedefs we are not creating types but just using wrappers.

(define-module (nyacc lang c99 ffi-help)
  #:export (*ffi-help-version*
	    define-ffi-module
	    compile-ffi-file
	    intro-ffi
	    string-member-proc string-renamer
	    ;;pkg-config-incs pkg-config-defs pkg-config-libs
	    ;; debugging
	    ffi-symmap
	    )
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang c99 parser)
  #:use-module (nyacc lang c99 pprint)
  #:use-module (nyacc lang c99 munge)
  #:use-module (nyacc lang c99 util1)
  #:use-module ((nyacc lang util)
		#:select (cintstr->scm sx-ref sx-list sx-attr-ref sx-attr-set!))
  #:use-module ((nyacc lex) #:select (cnumstr->scm))
  #:use-module ((nyacc util) #:select (ugly-print))
  #:use-module (system foreign)
  #:use-module (sxml fold)
  #:use-module (sxml match)
  #:use-module ((sxml xpath)
		#:renamer (lambda (s) (if (eq? s 'filter) 'sxml:filter s)))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (system base pmatch)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:version (0 82 4)
  )

(use-modules (ice-9 pretty-print))

(define *ffi-help-version* "0.82.2")

(define fh-inc-dirs
  (append
   `(,(assq-ref %guile-build-info 'includedir)
     "/usr/include")
   (get-gcc-inc-dirs)))
(set! fh-inc-dirs (cons "." fh-inc-dirs)) ; DEBUGGING
(define fh-inc-help
  (cond
   ((string-contains %host-type "darwin")
    '(("__builtin"
       "__builtin_va_list=void*"
       "__attribute__(X)="
       "__inline=" "__inline__="
       "__asm(X)=" "__asm__(X)="
       "__has_include(X)=__has_include__(X)"
       "__extension__="
       "__signed=signed"
       )
      ;;("sys/cdefs.h" "__DARWIN_ALIAS(X)=")
      ))
   (else
    '(("__builtin"
       "__builtin_va_list=void*" "__attribute__(X)="
       "__inline=" "__inline__="
       "__asm(X)=" "__asm__(X)="
       "__has_include(X)=__has_include__(X)"
       "__extension__="
       )
      ))))
(define fh-cpp-defs
  (cond
   ((string-contains %host-type "darwin")
    '("__GNUC__=6")
    (remove (lambda (s)
	      (string-contains s "_ENVIRONMENT_MAC_OS_X_VERSION"))
	    (get-gcc-cpp-defs)))
   (else
    '())))

;; change to parameters
(define *options* (make-parameter '()))
(define *prefix* (make-parameter "."))	 ; prefix to files
(define *debug* (make-parameter #f))	 ; parse debug mode
(define *mport* (make-parameter #t))	 ; output module port
(define *udict* (make-parameter '()))	 ; udecl dict
(define *wrapped* (make-parameter '()))	 ; wrappers for foo_t and foo_t*
(define *defined* (make-parameter '()))	 ; has wrapper and is bytestructure?
(define *renamer* (make-parameter identity)) ; renamer from ffi-module
(define *errmsgs* (make-parameter '()))	     ; list of warnings
;; what about option to trace
(define *all-defs* (make-parameter '()))

(define *echo-decls* #f)		; add echo-decls code for debugging

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

(define (sfout fmt . args)
  (apply simple-format #t fmt args))
(define (ppout tree)
  (pretty-print tree #:per-line-prefix "    "))
(define (nlout) (newline))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
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

;; strings->symbol
(define (strings->symbol . string-list)
  (string->symbol (apply string-append string-list)))

;; '(abc def) => "abc-def"
(define (path->name path)
  (string-join (map symbol->string path) "-"))

;; '(abc def) => "abc/def"
(define (path->path path)
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
      (let* ((cmdstr (string-join (cons* "pkg-config" name args)))
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

(define (pkg-config-libs name)
  (fold-right
   (lambda (s l)
     (cond 
      ((< (string-length s) 3) l)
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
     (else (throw 'fh-error "value does not resolve to list")))))

(define (cintstr->num str)
  (and=> (cintstr->scm str) string->number))

(define (sw/* name) (string-append name "*"))
(define (sw/*-desc name) (string-append name "*-desc"))
(define (sw/& name) (string-append name "&"))

;; I was using (pointer . name) in *defined* but this has issues
;; because expand-typerefs does not recognize it.  Is a change needed?
(define (w/struct name) (cons 'struct name))
(define (w/union name) (cons 'union name))
(define (w/enum name) (cons 'enum name))
;; pointers needed?
(define (w/* name) (cons 'pointer name))
(define (w/struct* name) (cons 'pointer (cons 'struct name)))
(define (w/union* name) (cons 'pointer (cons 'union name)))

;; === output scheme module header 

(define (ffimod-header path module-opts)
  (let* ((attrs (opts->attrs module-opts '()))
	 (pkg-config (assq-ref attrs 'pkg-config))
	 (libraries (resolve-attr-val (assq-ref attrs 'library)))
	 (libraries (append
		     (if pkg-config (pkg-config-libs pkg-config) '())
		     libraries))
	 (library (car libraries))
	 (libraries (cdr libraries)))
    (sfscm ";; generated with `guild compile-ffi ~A.ffi'\n" (path->path path))
    (nlscm)
    (sfscm "(define-module ~S\n" path)
    (for-each
     (lambda (pair)
       (cond
	((eq? 'use-ffi-module (car pair))
	 (sfscm "  #:use-module ~S\n" (cdr pair)))))
     module-opts)
    ;;
    (for-each ;; output pass-through options
     (lambda (pair) (sfscm "  ~S " (car pair)) (ppscm (cdr pair)))
     (opts->mopts module-opts))
    ;;
    (sfscm "  #:use-module (system ffi-help-rt)\n")
    (sfscm "  #:use-module ((system foreign) #:prefix ffi:)\n")
    (sfscm "  #:use-module (bytestructures guile)\n")
    (sfscm "  )\n")
    ;;
    (for-each (lambda (l) (sfscm "(dynamic-link ~S)\n" l)) (reverse libraries))
    (sfscm "(define link-lib (dynamic-link ~S))\n" library)
    ;;(sfscm "(define (lib-func name) (dynamic-func name link-lib))\n")
    ;;
    ;;(sfscm "(define void intptr_t)\n")  ; bytestructures now has 'void
    (if *echo-decls* (sfscm "(define echo-decls #t)\n"))
    ))


;; === type conversion ==============

;; argument and return values will be
;; @item int types
;; @item double float
;; @item enum => int
;; @item function (pointer)
;; @item void
;; @item pointer
;; @item struct
;; @item union
;; strings dealt with by user

;; determine if type is an "alias", that is same
;; typedef int foo_t => int
;; but use (define foo_t (bs:pointer int))

(define bs-typemap
  '(("void" . 'void) ("float" . float) ("double" . double)
    ("short" . short) ("short int" . short) ("signed short" . short)
    ("signed short int" . short) ("int" . int) ("signed" . int)
    ("signed int" . int) ("long" . long) ("long int" . long)
    ("signed long" . long) ("signed long int" . long) ("long long" . long)
    ("long long int" . long) ("signed long long" . long)
    ("signed long long int" . long)
    ("unsigned short int" . unsigned-short) ("unsigned short" . unsigned-short)
    ("unsigned int" . unsigned-int) ("unsigned" . unsigned-int)
    ("unsigned long int" . unsigned-long) ("unsigned long" . unsigned-long)
    ("unsigned long long int" . unsigned-long)
    ("unsigned long long" . unsigned-long)
    ("intptr_t" . intptr_t) ("uintptr_t" . uintptr_t) ("size_t" . size_t)
    ("ssize_t" . ssize_t) ("ptrdiff_t" . ptrdiff_t)
    ("int8_t" . int8) ("uint8_t" . uint8) 
    ("int16_t" . int16) ("uint16_t" . uint16) 
    ("int32_t" . int32) ("uint32_t" . uint32)
    ("int64_t" . int64) ("uint64_t" . uint64)
    ("float _Complex" . complex64) ("double _Complex" . complex128)
    ;; hacks:
    ("char" . int8) ("signed char" . int8) ("unsigned char" . uint8)
    ("_Bool" . int8)
    ))

(define bs-defined (map car bs-typemap))

(define (const-expr->number expr)
  (let ((ns (or (string->number expr)
		(eval-cpp-expr (parse-cpp-expr expr) (*all-defs*)))))
    (unless ns (sferr "vector hell: ~S\n" expr))
    ns))

;; just the type, so parent has to build the name-value pairs for
;; struct members
(define (mtail->bs-desc mspec-tail)
  (let ((defined (*defined*))) ;; (udict (*udict*)))
    (pmatch mspec-tail
      ;; expand typeref, use renamer, ...? 
      (((typename ,name))
       (or (assoc-ref bs-typemap name)
	   (string->symbol (string-append name "-desc"))))

      (((void)) ''void)
      (((fixed-type "char")) 'int)
      (((fixed-type "unsigned char")) 'unsigned-int)
      (((fixed-type ,fx-name)) (assoc-ref bs-typemap fx-name))
      (((float-type ,fl-name)) (assoc-ref bs-typemap fl-name))
      (((enum-def (ident ,ident) ,rest)) 'int)
      (((enum-def ,rest)) 'int)

      (((struct-def (ident ,struct-name) ,field-list))
       (mtail->bs-desc `((struct-def ,field-list))))
      (((struct-def ,field-list))
       `(bs:struct (list ,@(cnvt-field-list field-list))))
      (((struct-ref (ident ,struct-name)))
       (string->symbol (string-append "struct-" struct-name "-desc")))
      
      (((union-def (ident ,union-name) ,field-list))
       (mtail->bs-desc `((union-def ,field-list))))
      (((union-def ,field-list))
       (list 'bs:union `(list ,@(cnvt-field-list field-list))))

      ;; POINTERS

      ;; typename use renamers, ... ???
      (((pointer-to) (typename ,name))
       (cond
	((assoc-ref name bs-typemap) =>
	 (lambda (n) `(bs:pointer ,n)))
	((member (w/* name) defined)
	 (strings->symbol name "*-desc"))
	((member name defined)
	 `(bs:pointer ,(strings->symbol name "-desc")))
	(else
	 (strings->symbol name "*-desc"))))

      (((pointer-to) (void))
       `(bs:pointer 'void))

      (((pointer-to) (fixed-type "char"))
       `(bs:pointer int8))
      (((pointer-to) (fixed-type ,fx-name))
       `(bs:pointer ,(assoc-ref bs-typemap fx-name)))
      (((pointer-to) (float-type ,fx-name))
       `(bs:pointer ,(assoc-ref bs-typemap fx-name)))

      ;; bs does not support function pointers
      (((function-returning . ,rest) . ,rest)
       `(bs:pointer 'void))
      (((pointer-to) (function-returning . ,rest) . ,rest)
       `(bs:pointer 'void))
      (((pointer-to) (pointer-to) (function-returning . ,rest) . ,rest)
       `(bs:pointer 'void))

      (((pointer-to) (struct-ref . ,rest))
       (let () ;; TODO: check for struct-def ???
	 `(bs:pointer 'void)))

      #;(((pointer-to) (struct-ref (ident ,name)))
      (let ()
      #f))

      ;; should use this more
      (((pointer-to) . ,rest)
       `(bs:pointer ,(mtail->bs-desc rest)))

      ;; In C99 array parameters are interpreted as pointers.
      (((array-of ,n) (fixed-type ,name))
       (let ((ns (const-expr->number n)))
	 (cond
	  ((string=? name "char") `(bs:vector ,ns int8))
	  ((string=? name "unsigned char") `(bs:vector ,ns uint8))
	  (else `(bs:vector ,ns ,(mtail->bs-desc `((fixed-type ,name))))))))
      (((array-of ,n) . ,rest)
       `(bs:vector ,(const-expr->number n) ,(mtail->bs-desc rest)))
      (((array-of) . ,rest)
       `(bs:pointer ,(mtail->bs-desc rest)))

      (((bit-field ,size) . ,rest)
       `(bit-field ,(const-expr->number size) ,(mtail->bs-desc rest)))

      (,otherwise
       (sferr "mtail->bs-desc missed mspec:\n")
       (pperr mspec-tail)
       ;;(error "quit") (quit)
       (fherr "mtail->bs-desc failed")
       )
      )))


;; --- output routines ---------------

(define (fhscm-export-def name)
  (let* ((st-name (if (string? name) name (symbol->string name)))
	 (sy-name (if (string? name) (string->symbol name) name))
	 (pred (string->symbol (string-append st-name "?")))
	 (make (string->symbol (string-append "make-" st-name))))
    ;;(sfscm "(export ~A ~A? make-~A)\n" name name name)
    (upscm `(export ,sy-name ,pred ,make))))

;;(define (fhscm-export-pdef name)
;;  (sfscm "(export ~A* ~A*? make-~A*)\n" name name name))

(define (fhscm-def-alias name orig)
  (let ((s-name (if (string? name) (string->symbol name) name))
	(s-orig (if (string? orig) (string->symbol orig) orig)))
    (ppscm `(define-public ,s-name ,s-orig))))

(define (fhscm-def-desc name desc)
  (let ((s-name (if (string? name) (string->symbol name) name))
	(s-desc (if (string? desc) (string->symbol desc) desc)))
    (ppscm `(define-public ,s-name ,s-desc))))

(define (fhscm-def-*desc name)
  (sfscm "(define-public ~A* (bs:pointer ~A-desc))\n" name name))

(define (fhscm-def-*desc/delay name)
  (sfscm "(define-public ~A* (bs:pointer (delay ~A-desc)))\n" name name))

(define (fhscm-def-compound name)
  (let* ((st-name (if (string? name) name (symbol->string name)))
	 (sy-name (if (string? name) (string->symbol name) name))
	 (desc (string->symbol (string-append st-name "-desc")))
	 (pred (string->symbol (string-append st-name "?")))
	 (make (string->symbol (string-append "make-" st-name))))
    (upscm `(define-fh-compound-type ,sy-name ,desc ,pred ,make))
    (fhscm-export-def name)))

(define (fhscm-def-pointer name)
  (let* ((st-name (if (string? name) name (symbol->string name)))
	 (sy-name (if (string? name) (string->symbol name) name))
	 (desc (string->symbol (string-append st-name "-desc")))
	 (pred (string->symbol (string-append st-name "?")))
	 (make (string->symbol (string-append "make-" st-name))))
    (upscm `(define-fh-pointer-type ,sy-name ,desc ,pred ,make))
    (fhscm-export-def name)))

(define (fhscm-def-pointer/delay name)
  (sfscm "(define-fh-pointer-type ~A* ~A*-desc\n" name)
  (sfscm "  ~A*? make-~A*)\n" name name)
  (fhscm-export-def name))

(define (fhscm-ref-deref typename)
  (let* ((type* (strings->symbol typename "*"))
	 (make* (strings->symbol "make-" typename "*"))
	 (type (strings->symbol typename))
	 (make (strings->symbol "make-" typename)))
    (ppscm `(ref<->deref! ,type* ,make* ,type ,make))))

(define (fhscm-def-function* name return params)
  (let* ((st-name (if (string? name) name (symbol->string name)))
	 (sy-name (if (string? name) (string->symbol name) name))
	 (wrap (string->symbol (string-append "fh-wrap-" st-name)))
	 (unwrap (string->symbol (string-append "unwrap-" st-name))))
    (sfscm "(define-public ~A-desc\n" name)
    (ppscm `(bs:pointer (delay (fh:function ,return (list ,@params))))
	   #:per-line-prefix "  ")
    (sfscm "  )\n")
    (ppscm `(define-fh-function*-type ,sy-name
	      ,(string->symbol (string-append name "-desc"))
	      ,(string->symbol (string-append name "?"))
	      ,(string->symbol (string-append "make-" name))
	      ))
    (fhscm-export-def name)
    ;;(sfscm "(export ~A ~A? make-~A)\n" name name name)
    ))
  
(define* (fhscm-def-fixed name)
  (sfscm "(define unwrap-~A unwrap~~fixed)\n" name)
  (sfscm "(define wrap-~A identity)\n" name))

(define* (fhscm-def-float name)
  (sfscm "(define unwrap-~A unwrap~~float)\n" name)
  (sfscm "(define wrap-~A identity)\n" name))

;; --- structs and unions

;; This routine will munge the fields and then perform typeref expansion.
;; `defined' here means has -desc (what?)
(define (expand-field-list-typerefs field-list)
  (let ((udict (*udict*)) (defined (*defined*)))
    (cons 'field-list
	  (fold-right
	   (lambda (pair seed)
	     (cons (expand-typerefs (cdr pair) udict defined) seed))
	   '() (fold-right unitize-comp-decl '() (cdr field-list))))))

;; field-list is (field-list . ,fields)
(define (cnvt-field-list field-list)

  (define (acons-defn name type seed)
    (cons (eval-string (simple-format #f "(quote `(~A ,~S))" name type)) seed))

  (define (acons-bfld name type seed)	; bit-field
    (let ((size (list-ref type 1)) (type (list-ref type 2)))
      (cons (eval-string
	     (simple-format #f "(quote `(~A ,~S ~A))" name type size)) seed)))

  ;;(sferr "\nfield-list:\n") (pperr field-list)
  (let* ((field-list (clean-field-list field-list)) ; remove lone comments
	 (uflds (fold-right unitize-comp-decl '() (cdr field-list))))
    ;;(sferr "field-list:\n") (pperr field-list)
    (let iter ((decls uflds))
      (if (null? decls) '()
	  (let* ((name (caar decls))
		 (udecl (cdar decls))
		 ;; fix the following, look at cleanup-udecl
		 (udecl (udecl-rem-type-qual udecl)) ;; remove "const" "extern"
		 (spec (udecl->mspec/comm udecl))
		 (tail (cddr spec))
		 (type (mtail->bs-desc tail)))
	    (cond
	     ((and (pair? type) (eq? 'bit-field (car type)))
	      (acons-bfld name type (iter (cdr decls)))) ; bit-field
	     (else
	      (acons-defn name type (iter (cdr decls))))))))))

;; @deffn {Procedure} cnvt-aggr-def aggr-t typename aggr-name field-list
;; Output an aggregate definition, where
;; @var{attr-t} is a string of @code{"struct"} or @code{"union"},
;; @var{typename} is a string for the typename, or @code{#f},
;; @var{aggr-name} is a string for the struct or union name, or @code{#f},
;; and @var{field-list} is the field-list from the C syntax tree.
;; @end deffn

(define (cnvt-aggr-def aggr-t typename aggr-name field-list)
  ;;(sferr "\nfield-list:\n") (pperr field-list)
  (let* ((field-list (expand-field-list-typerefs field-list))
	 (sflds (cnvt-field-list field-list))
	 (aggr-s (symbol->string aggr-t))
	 (aggrname (and aggr-name (string-append aggr-s "-" aggr-name)))
	 (bs-aggr-t (string->symbol (string-append "bs:" aggr-s)))
	 (ty-desc (and typename (strings->symbol typename "-desc")))
	 (ty*-desc (and typename (strings->symbol typename "*-desc")))
	 (ag-desc (and aggrname (strings->symbol aggrname "-desc")))
	 (ag*-desc (and aggrname (strings->symbol aggrname "*-desc"))))
    (cond
     ((and typename aggr-name)
      (sfscm ";; == ~A =>\n" typename)
      (ppscm `(define-public ,ty-desc ,(list bs-aggr-t `(list ,@sflds))))
      (fhscm-def-compound typename)
      (ppscm `(define-public ,ty*-desc (bs:pointer ,ty-desc)))
      (fhscm-def-pointer (sw/* typename))
      (fhscm-ref-deref typename)
      (sfscm ";; == ~A =>\n" aggrname)
      (ppscm `(define-public ,ag-desc ,ty-desc))
      (fhscm-def-compound aggrname)
      (ppscm `(define-public ,ag*-desc ,ty*-desc))
      (fhscm-def-pointer (sw/* aggrname))
      (fhscm-ref-deref aggrname))
     (typename
      (ppscm `(define-public ,ty-desc ,(list bs-aggr-t `(list ,@sflds))))
      (fhscm-def-compound typename)
      (ppscm `(define-public ,ty*-desc (bs:pointer ,ty-desc)))
      (fhscm-def-pointer (sw/* typename))
      (fhscm-ref-deref typename))
     (aggr-name
      (ppscm `(define-public ,ag-desc ,(list bs-aggr-t `(list ,@sflds))))
      (fhscm-def-compound aggrname)
      (ppscm `(define-public ,ag*-desc (bs:pointer ,ag-desc)))
      (fhscm-def-pointer (sw/* aggrname))
      (fhscm-ref-deref aggrname)))))

(define (cnvt-struct-def typename struct-name field-list)
  (cnvt-aggr-def 'struct typename struct-name field-list))

(define (cnvt-union-def typename union-name field-list)
  (cnvt-aggr-def 'union typename union-name field-list))

;; --- enums

(define (fhscm-def-enum name name-val-list)
  (sfscm "(define ~A-enum-nvl\n" name)
  (ppscm `(quote ,name-val-list) #:per-line-prefix "  ")
  (sfscm "  )\n")
  (sfscm "(define ~A-enum-vnl\n" name)
  (sfscm "  (map (lambda (pair) (cons (cdr pair) (car pair)))\n")
  (sfscm "       ~A-enum-nvl))\n" name)
  (sfscm "(define-public (unwrap-~A n)\n" name)
  (sfscm "  (cond\n")
  (sfscm "   ((symbol? n)\n")
  (sfscm "    (or (assq-ref ~A-enum-nvl n) (error \"bad arg\")))\n" name)
  (sfscm "   ((integer? n) n)\n")
  (sfscm "   (else (error \"bad arg\"))))\n")
  (sfscm "(define-public (wrap-~A v)\n" name)
  (sfscm "  (assq-ref ~A-enum-vnl v))\n" name)
  )

(define (cnvt-enum-def typename enum-name enum-def-list)
  (let* ((name-val-l
	  (map
	   (lambda (def)
	     (let ((n (sx-ref (sx-ref def 1) 1)) (x (sx-ref def 2)))
	       (cons (string->symbol n) (eval-cpp-expr x '()))))
	   (cdr (canize-enum-def-list enum-def-list)))))
    (cond
     ((and typename enum-name)
      (fhscm-def-enum typename name-val-l)
      (sfscm "(define-public unwrap-enum-~A unwrap-~A)\n" enum-name typename)
      (sfscm "(define-public wrap-enum-~A wrap-~A)\n" enum-name typename)
      )
     (typename
      (fhscm-def-enum typename name-val-l))
     (enum-name
      (fhscm-def-enum (string-append "enum-" enum-name) name-val-l)))))

;; === function declarations : signatures for pointer->procedure

(define ffi-typemap
  ;; see system/foreign.scm
  '(("void" . ffi:void) ("float" . ffi:float) ("double" . ffi:double)
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
    ("intptr_t" . ffi:long) ("uintptr_t" . ffi:usigned-long)
    ("char" . ffi:int8)
    ("signed char" . ffi:int8)
    ("unsigned char" . ffi:uint8)
    ("long long" . ffi:long)
    ("long long int" . ffi:long)
    ("signed long long" . ffi:long)
    ("signed long long int" . ffi:long)
    ("unsigned long long" . ffi:unsigned-long)
    ("unsigned long long int" . ffi:unsigned-long)
    ("_Bool" . ffi:int8)
    ))

(define ffi-defined (map car ffi-typemap))

(define ffi-symmap
  `((ffi:void . ,void) (ffi:float . ,float) (ffi:double . ,double)
    (ffi:short . ,short) (ffi:int . ,int) (ffi:long . ,long)
    (ffi:unsigned-short . ,unsigned-short) (ffi:unsigned-int . ,unsigned-int)
    (ffi:unsigned-long . ,unsigned-long) (ffi:size_t . ,size_t)
    (ffi:ssize_t . ,ssize_t) (ffi:ptrdiff_t . ,ptrdiff_t) (ffi:int8 . ,int8)
    (ffi:uint8 . ,uint8) (ffi:int16 . ,int16) (ffi:uint16 . ,uint16)
    (ffi:int32 . ,int32) (ffi:uint32 . ,uint32) (ffi:int64 . ,int64)
    (ffi:uint64 . ,uint64) (ffi-void* . *)
    ))

(define (mspec->ffi-sym mspec)
  ;;(sferr "mspec=~S\n" mspec)
  (if (and (pair? (cdr mspec)) (string? (cadr mspec))) (error "xxx"))
  (pmatch (cdr mspec)
    (((pointer-to) . ,rest) 'ffi-void*)
    (((array-of) . ,rest) 'ffi-void*)
    (((array-of ,size) . ,rest) 'ffi-void*)
    (((fixed-type ,name))
     (or (assoc-ref ffi-typemap name)
	 (fherr/once "no FFI fixed-type for ~A" name)))
    (((float-type ,name))
     (or (assoc-ref ffi-typemap name)
	 (fherr/once "no FFI float-type for ~S" name)))
    (((typename ,name) . ,rest)
     (or (assoc-ref ffi-typemap name)
	 (fherr "no FFI type for ~S" name)))
    (((void)) 'ffi:void)
    (((enum-def . ,rest2) . ,rest1) 'ffi:int)
    (((enum-ref . ,rest2) . ,rest1) 'ffi:int)

    (((struct-def (field-list . ,fields)))
     `(list ,@(map (lambda (fld)
		     (let* ((udict (unitize-comp-decl fld))
			    (name (caar udict))
			    (udecl (cdar udict))
			    (udecl (udecl-rem-type-qual udecl))
			    (mspec (udecl->mspec udecl)))
		       (mspec->ffi-sym mspec)))
		   fields)))
    (((struct-def (ident ,name) ,field-list))
     (mspec->ffi-sym (list (car mspec) `(struct-def ,field-list))))
    
    (((union-def (field-list . ,fields)))
     ;; TODO check libffi on how unions are passed and returned.
     ;; I assume here never passed as a floating point.
     (let iter ((type #f) (size 0) (flds fields))
       (if (null? flds)
	   (case type ((double) 'ffi:uint64) ((float) 'ffi:uint32) (else type))
	   (let* ((udict (unitize-comp-decl (car flds)))
		  (udecl (cdar udict))
		  (udecl (udecl-rem-type-qual udecl))
		  (mspec (udecl->mspec udecl))
		  (ftype (mspec->ffi-sym mspec))
		  (ftval (assq-ref ffi-symmap ftype))
		  (fsize (sizeof ftval)))
	     (if (> fsize size)
		 (iter ftype fsize (cdr flds))
		 (iter type size (cdr flds)))))))
    (((union-def (ident ,name) ,field-list))
     (mspec->ffi-sym (list (car mspec) `(union-def ,field-list))))
    
    (,otherwise
     (sferr "mspec->ffi-sym missed:\n") (pperr mspec) ;;(quit)
     (error "") (fherr "mspec->ffi-sym missed: ~S" mspec))))

;; Return a mspec for the return type.  The variable is called @code{NAME}.
(define (gen-decl-return udecl)
  (let* ((udecl1 (expand-typerefs udecl (*udict*) ffi-defined))
	 (udecl (udecl-rem-type-qual udecl1))
	 (mspec (udecl->mspec udecl1)))
    (mspec->ffi-sym mspec)))

(define (int->abs-ident ix)
  (simple-format #f "arg-~A" ix))

(define (gen-decl-params params)
  ;; Note that expand-typerefs will not eliminate enums or struct-refs :
  ;; mspec->ffi-sym needs to convert enum to int or void*
  (let iter ((ix 0) (params (fix-params params)))
    (cond
     ((null? params) '())
     ;;((equal? (car params) '(ellipsis)) (fherr/once "no varargs (yet)") '...)
     ((equal? (car params) '(ellipsis)) '())
     (else
      ;;(sferr "\nP: ~S\n" (car params))
      (let* ((udecl1 (expand-typerefs (car params) (*udict*) ffi-defined))
	     (udecl1 (udecl-rem-type-qual udecl1))
	     (mspec (udecl->mspec udecl1 #:abs-ident (int->abs-ident ix))))
	;;(sferr "  ~S\n" udecl1)
	(cons (mspec->ffi-sym mspec)
	      (iter (1+ ix) (cdr params))))))))

;; === function calls : unwrap args, call, wrap return

;; given mspec for an exec argument give the unwrapper
(define (mspec->fh-unwrapper mspec)
  ;;(sferr "mspec:\n") (pperr mspec)
  (let ((wrapped (*wrapped*)) (defined (*defined*)))
    ;; git_reference_foreach_name_cb not preserved
    (pmatch (cdr mspec)
      (((fixed-type ,name)) 'unwrap~fixed)
      (((float-type ,name)) 'unwrap~float)
      (((void)) #f)
      (((typename ,name))
       (cond ;; bit of a hack
	((member name '("float" "double")) 'unwrap~float)
	((member name '("float _Complex" "double _Complex")) 'unwrap~complex)
	((member name bs-defined) 'unwrap~fixed)
	((member name defined) `(fht-unwrap ,(string->symbol name)))
	((member name wrapped) (string->symbol (string-append "unwrap-" name)))
	(else #f)))
      
      (((enum-def (ident ,name) ,rest))
       (cond
	((member (w/enum name) wrapped)
	 (string->symbol (string-append "unwrap-enum-" name)))
	(else 'unwrap-enum)))
      (((enum-ref (ident ,name)))
       (cond
	((member (w/enum name) wrapped)
	 (string->symbol (string-append "unwrap-enum-" name)))
	(else 'unwrap-enum)))

      (((pointer-to) (typename ,typename))
       (cond
	;;((member typename ffi-defined) 'unwrap~pointer)
	((member (w/* typename) defined)
	 `(fht-unwrap ,(string->symbol (sw/* typename))))
	((member (w/* typename) wrapped)
	 (strings->symbol "unwrap-" typename "*"))
	((member typename defined)
	 `(fht-unwrap ,(string->symbol (sw/* typename))))
	((member (w/* typename) wrapped)
	 (strings->symbol "unwrap-" typename "*"))
	(else #f)))

      (((pointer-to) (struct-ref (ident ,struct-name) . ,rest))
       (cond
	((member (w/struct* struct-name) defined)
	 `(fht-unwrap ,(strings->symbol "struct-" struct-name "*")))
	((member (w/struct struct-name) defined)
	 `(fht-unwrap ,(strings->symbol "struct-" struct-name "*")))
	(else 'unwrap~pointer)))

      (((pointer-to) (function-returning (param-list . ,params)) . ,rest)
       (let* ((udecl (mspec->udecl (cons "~ret" rest)))
	      (udecl (expand-typerefs udecl (*udict*) ffi-defined))
	      (mspec (udecl->mspec udecl))
	      (decl-return (mspec->ffi-sym mspec))
	      (decl-params (gen-decl-params params)))
	 ;;(sferr "FIX RET => ~S\n" mspec)
	 (if (equal? (last decl-params) '...) (fherr/once "no varargs (yet)"))
	 `(make-fctn-param-unwrapper ,decl-return (list ,@decl-params))))
      
      (((pointer-to) . ,otherwise) 'unwrap~pointer)

      ;; TODO: int b[]
      ;; make ffi-help-rt unwrap bytevector  
      (((array-of ,size) . ,rest) 'unwrap~array)
      (((array-of) . ,rest) 'unwrap~array)

      (,otherwise
       (sferr "mspec->fh-unwrapper missed:\n") (pperr mspec) (quit)
       (fherr "mspec->fh-unwrapper missed: ~S" mspec)))))

(define (mspec->fh-wrapper mspec)
  (let ((wrapped (*wrapped*)) (defined (*defined*)))
    (pmatch (cdr mspec)
      (((fixed-type ,name)) (if (assoc-ref ffi-typemap name) #f
				(fherr "todo: ffi-wrap fixed")))
      (((float-type ,name)) (if (assoc-ref ffi-typemap name) #f
				(fherr "todo: ffi-wrap float")))
      (((void)) #f)
      (((typename ,name))
       (cond
	((member name bs-defined) #f)
	((member name defined) `(fht-wrap ,(string->symbol name)))
	((member name wrapped) (string->symbol (string-append "wrap-" name)))
	(else #f)))

      (((enum-def (ident ,name) ,rest))
       (cond
	((member (w/enum name) wrapped)
	 (string->symbol (string-append "wrap-enum-" name)))
	(else 'wrap-enum)))
      (((enum-ref (ident ,name)))
       (cond
	((member (w/enum name) wrapped)
	 (string->symbol (string-append "wrap-enum-" name)))
	(else 'wrap-enum)))

      (((pointer-to) (typename ,typename))
       (cond
	;;??((member typename ffi-defined) 'wrap~pointer)
	((member typename defined)
	 `(fht-wrap ,(strings->symbol typename "*")))
	((member typename wrapped)
	 (strings->symbol "wrap-" typename "*"))
	(else #f)))
      
      (((pointer-to) (struct-ref (ident ,struct-name) . ,rest))
       (cond
	((member (w/struct struct-name) wrapped)
	 `(fht-wrap ,(string->symbol (sw/* struct-name))))
	;;(else 'wrap~pointer)))
	(else #f)))

      ;;(((pointer-to) . ,otherwise) 'ffi:make-pointer)
      (((pointer-to) . ,otherwise) #f)

      (,otherwise (fherr "mspec->fh-wrapper missed: ~S" mspec)))))

;; given list of udecl params generate list of name-unwrap pairs
(define (gen-exec-params params)
  (fold-right
   (lambda (param-decl seed)
     (cond
      ((equal? param-decl '(ellipsis)) seed)
      (else
       ;; Changed to (*wrapped*) to include enum types.  If we need (*defined*)
       ;; then we will need to create enum types in cnvt-udecl typedefs.
       (let* ((param-decl (expand-typerefs param-decl (*udict*) (*wrapped*)))
	      (param-decl (udecl-rem-type-qual param-decl)) ;; ???
	      (mspec (udecl->mspec param-decl)))
	 (acons (car mspec) (mspec->fh-unwrapper mspec) seed)))))
   '() params))

;; given list of name-unwrap pairs generate function arg names
(define (gen-exec-arg-names params)
  (map (lambda (s) (string->symbol (car s))) params))

(define (gen-exec-unwrappers params)
  (fold-right
   (lambda (name-unwrap seed)
     (let ((name (car name-unwrap))
	   (unwrap (cdr name-unwrap)))
       (if unwrap
	   (cons `(,(string->symbol (string-append "~" name))
		   (,unwrap ,(string->symbol name)))
		 seed)
	   seed)))
   '()
   params))

;; This generates the list of arguments to the actual call.
(define (gen-exec-call-args params)
  (fold-right
   (lambda (name-unwrap seed)
     (let ((name (car name-unwrap))
	   (unwrap (cdr name-unwrap)))
       (cons (string->symbol (if unwrap (string-append "~" name) name)) seed)))
   '()
   params))

(define (gen-exec-return-wrapper udecl)
  ;;(let* ((udecl (expand-typerefs udecl (*udict*) (*defined*)))
  (let* ((udecl (expand-typerefs udecl (*udict*) (*wrapped*)))
	 (udecl (udecl-rem-type-qual udecl))
	 (mspec (udecl->mspec udecl)))
    (mspec->fh-wrapper mspec)))

(define (fix-params param-decls)

  (define (remove-void-param params)
    (if (and (pair? params) (null? (cdr params))
	     (equal? (car params)
		     '(param-decl (decl-spec-list (type-spec (void))))))
	'() params))
  
  (define (fix-param param-decl ix)
    ;; should this fix param names? -- above code should deal with it
    (sxml-match param-decl
      ((param-decl (decl-spec-list . ,specl))
       `(param-decl (decl-spec-list . ,specl)
		    (init-declr (ident ,(simple-format #f "arg-~A" ix)))))
      (,otherwise param-decl)))

  (let iter ((ix 0) (decls (remove-void-param param-decls)))
    (if (null? decls) '()
	(cons (fix-param (car decls) ix) (iter (1+ ix) (cdr decls))))))

;; @deffn {Procedure} cnvt-fctn name specl params
;; name is string
;; specl is decl-spec-list tree
;; params is list of param-decl trees (i.e., cdr of param-list tree)
;; @end deffn
(define (cnvt-fctn name rdecl params)
  ;;(when (string=? name "cairo_create") (pperr (*wrapped*)))
  (let* ((params (fix-params params))
	 (varargs? (equal? (last params) '(ellipsis)))
	 (decl-return (gen-decl-return rdecl))
	 (decl-params (gen-decl-params params))
	 (exec-return (gen-exec-return-wrapper rdecl))
	 (exec-params (gen-exec-params params))
	 (sname (string->symbol name))
	 (~name (string->symbol (string-append "~" name)))
	 ;;(call `(,~name ,@(gen-exec-call-args exec-params)))
	 (va-call `(apply ,~name ,@(gen-exec-call-args exec-params)
			  (map cdr ~rest)))
	 (call `((force ,~name) ,@(gen-exec-call-args exec-params)))
	 )
    (cond
     (varargs?
      (sfscm ";; to be used with fh-cast\n")
      (ppscm
     `(define (,sname ,@(gen-exec-arg-names exec-params) . ~rest)
	(let ((,~name (fh-link-proc
		       ,name ,decl-return
		       (append (list ,@decl-params) (map car ~rest))
		       link-lib))
		      ,@(gen-exec-unwrappers exec-params))
	  ,(if exec-return `(,exec-return va-call) va-call)))))
     (else
      (ppscm `(define ,~name
		(delay (fh-link-proc ,name ,decl-return
				     (list ,@decl-params) link-lib))))
      (ppscm
       `(define (,sname ,@(gen-exec-arg-names exec-params))
	  (let ,(gen-exec-unwrappers exec-params)
	    ,(if exec-return (list exec-return call) call))))))
      (sfscm "(export ~A)\n" name)))

;; === externs ========================

(define (cnvt-extern name ms-tail)
  (let* ((desc (mtail->bs-desc ms-tail))
	 (desc-name (string->symbol (string-append name "-desc")))
	 (code `(define ,(string->symbol name)
		  (let* ((addr #f)
			 (memoize-addr
			  (lambda ()
			    (unless addr
			      (set! addr
				    (make-bytestructure
				     (ffi:pointer->bytevector
				      (dynamic-pointer ,name (dynamic-link))
				      (ffi:sizeof '*)) 0
				      (bs:pointer ,desc)))))))
		    (case-lambda
		     (()
		      (memoize-addr)
		      (bytestructure-ref addr '*))
		     ;;((val) (bytestructure-set! addr '*))
		     )))))
    (ppscm code)
    (sfscm "(export ~A)\n" name)))


;; ------------------------------------

;; intended to provide decl's for pointer-to or vector-of args
(define (get-needed-defns params keep-list)
  (sferr "get-needed-defns [NOT DONE]\n") (pperr params)
  '())

;; extract (struct-def ...) from (udecl ...)
(define find-struct-def
  (let ((find-proc (sxpath '(// struct-def))))
    (lambda (udecl)
      (and=> (find-proc udecl) car))))

;; extract (union-def ...) from (udecl ...)
(define find-union-def
  (let ((find-proc (sxpath '(// union-def))))
    (lambda (udecl)
      (and=> (find-proc udecl) car))))


;; assume unit-declarator
;; TODO (ptr-declr (pointer (type-qual-list (type-qual "const"))))
;; todo:
;; 1) remove comments
;; 2) keep attributes! Used for forward decl's
(define (cleanup-udecl specl declr)
  (let* ((fctn? (pair? ((sxpath '(// ftn-declr)) declr)))
	 (specl (remove (lambda (node)
			  (or (equal? node '(stor-spec (auto)))
			      (equal? node '(stor-spec (register)))
			      (equal? node '(stor-spec (static)))
			      (and (pair? node)
				   (equal? (car node) 'type-qual))
			      ))
			specl))
	 (specl (if fctn?
		    (remove (lambda (node)
			      (equal? node '(stor-spec (extern)))) specl)
		    specl))
	 )
    (values specl declr)))

;; @deffn {Procecure} back-ref-extend! decl typename
;; @deffnx {Procecure} back-ref-getall decl typename
;; The first procecure adds a backward reference for a struct from typedef
;; forward reference.  The second procedure returns the list of references.
;; This is sort of a hack but don't want to carry a list of forward
;; references just yet.
;; @end deffn
(define (back-ref-extend! decl typename)
  (let ((aval (sx-attr-ref decl 'typedef)))
    (sx-attr-set! decl 'typedef
		  (if aval (string-append aval "," typename) typename))))
(define (back-ref-getall decl)
  (let ((aval (sx-attr-ref decl 'typedef)))
    (if aval (string-split aval #\,) '())))

;; @deffn {Procedure} cnvt-udecl udecl udict wrapped defined)
;; Given udecl produce a ffi-spec.
;; Return updated (string based) keep-list, which will be modified if the
;; declaration is a typedef.  The typelist is the set of keepers used for
;; @code{udecl->mspec}.
;; Returns values wrapped, defined.
;; @end deffn
;; NOT SURE WHAT defined MEANS NOW
;; was bytestructure in fh-type, but for
;; for any type we also declare a poitner type
(define (cnvt-udecl udecl udict wrapped defined)
  ;; This is a bit sloppy in that we have to know if the converters are
  ;; creating wrappers and/or (type) defines.
  
  (define (ptr-decl specl)
    `(udecl ,specl (init-declr (ptr-declr (pointer) (ident "_")))))
  (define (non-ptr-decl specl)
    `(udecl ,specl (init-declr (ident "_"))))
  
  ;; use fluids OR pass around
  (*wrapped* wrapped)
  (*defined* defined)

  (let*-values (((tag attr specl declr tail) (split-adecl udecl))
		((specl declr) (cleanup-udecl specl declr))
		)
    (sxml-match (sx-list tag #f specl declr)

      ;; typedef void **ptr_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef)) (type-spec (void)))
	(init-declr (ptr-declr (pointer (pointer)) (ident ,typename))))
       (sfscm "(define-public ~A-desc (bs:pointer (bs:pointer 'void)))\n"
	      typename)
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))
      
      ;; typedef void *ptr_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef)) (type-spec (void)))
	(init-declr (ptr-declr (pointer) (ident ,typename))))
       (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))
      
      ;; typedef void proxy_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (void)))
	(init-declr (ident ,typename)))
       (sfscm "(define-public ~A-desc 'void)\n" typename)
       (sfscm "(define-public ~A*-desc (bs:pointer ~A-desc))\n"
	      typename typename)
       (fhscm-def-pointer (sw/* typename))
       (values (cons* typename (w/* typename) wrapped)
	       (cons* typename (w/* typename) defined)))
      
      ;; typedef int foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (fixed-type ,name)))
	(init-declr (ident ,typename)))
       (sfscm "(define-public ~A-desc ~A)\n"
	      typename (assoc-ref bs-typemap name))
       (values wrapped defined))

      ;; typedef double foo_t;
      ;; If fh-object? then should be bytestructure.
      ;; Should wrap be to number or bytestructure?
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (float-type ,name)))
	(init-declr (ident ,typename)))
       (sfscm "(define-public ~A-desc ~A)\n"
	      typename (assoc-ref bs-typemap name))
       (values wrapped defined))

      ;; typedef foo_t *foo_ptr_t;
      ((udecl
	(decl-spec-list (stor-spec (typedef)) (type-spec (typename ,name)))
	(init-declr (ptr-declr (pointer) (ident ,typename))))
       (cond
	((member name defined)
	 (sfscm "(define-public ~A-desc (bs:pointer ~A-desc))\n" typename name)
	 (fhscm-def-pointer typename))
	(else
	 (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)
	 (fhscm-def-pointer typename)))
       (values (cons typename wrapped) (cons typename defined)))

      ;; typedef foo_t **foo_ptr_t;
      ((udecl
	(decl-spec-list (stor-spec (typedef)) (type-spec (typename ,name)))
	(init-declr (ptr-declr (pointer (pointer)) (ident ,typename))))
       (sfscm "(define-public ~A-desc (bs:pointer (bs:pointer ~A-desc)))\n"
	      typename name)
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))

      ;; typedef enum foo { ... } foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (enum-def (ident ,enum-name) ,enum-def-list . ,rest)))
	(init-declr (ident ,typename)))
       (cnvt-enum-def typename enum-name enum-def-list)
       (values (cons* typename (w/enum enum-name) wrapped) defined))

      ;; typedef enum { ... } foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (enum-def ,enum-def-list . ,rest)))
	(init-declr (ident ,typename)))
       (cnvt-enum-def typename #f enum-def-list)
       (values (cons typename wrapped) defined))

      ;; typedef enum foo foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (enum-ref (ident ,enum-name))))
	(init-declr (ident ,typename)))
       (sfscm "(define-public wrap-~A wrap-enum-~A)\n" typename enum-name)
       (sfscm "(define-public unwrap-~A unwrap-enum-~A)\n" typename enum-name)
       (values (cons typename wrapped) defined))
      
      ;; typedef struct foo { ... } foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-def (ident ,struct-name) ,field-list)))
	(init-declr (ident ,typename)))
       (cnvt-struct-def typename struct-name field-list)
       (values
	;; Hoping don't need to add (w/struct* struct-name)
	(cons* typename (w/* typename) (w/struct struct-name) wrapped)
	(cons* typename (w/* typename) (w/struct struct-name) defined)))

      ;; typedef struct { ... } foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-def ,field-list)))
	(init-declr (ident ,typename)))
       (cnvt-struct-def typename #f field-list)
       (values (cons* typename (w/* typename) wrapped)
	       (cons* typename (w/* typename) defined)))

      ;; typedef struct foo foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-ref (ident ,struct-name))))
	(init-declr (ident ,typename)))
       (cond
	;; This case represents three possible uses:
	((member struct-name defined)
	 ;; 1) struct defined previously
	 (sfscm "(define-public ~A-desc struct-~A-desc)\n" typename struct-name)
	 (sfscm "(define-public ~A*-desc struct-~A*-desc)\n"
		typename struct-name))
	((udict-struct-ref udict struct-name) =>
	 ;; 2) struct defined later
	 (lambda (struct-decl)
	   (back-ref-extend! struct-decl typename)
	   (sfscm "(define-public ~A-desc 'void)\n" typename)
	   (sfscm "(define-public ~A*-desc (bs:pointer (delay ~A-desc)))\n"
		typename typename)))
	(else
	 ;; 3) struct never defined; only used as pointer
	 (sfscm "(define-public ~A-desc 'void)\n" typename)
	 (sfscm "(define-public ~A*-desc (bs:pointer ~A-desc))\n"
		typename typename)))
       (fhscm-def-pointer (sw/* typename))
       (values (cons* typename (w/* typename) wrapped)
	       (cons* typename (w/* typename) defined)))

      ;; typedef struct foo *foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-ref (ident ,struct-name))))
	(init-declr (ptr-declr (pointer) (ident ,typename))))
       (cond
	;; This case represents three possible uses:
	((member struct-name defined)
	 ;; 1) struct defined previously
	 (sfscm "(define-public ~A-desc struct-~A*-desc)\n"
		typename struct-name))
	((udict-struct-ref udict struct-name) =>
	 ;; 2) struct defined later
	 (lambda (struct-decl)
	   (back-ref-extend! struct-decl (sw/& typename))
	   (sfscm "(define-public ~A&-desc 'void)\n" typename)
	   (sfscm "(define-public ~A-desc (bs:pointer (delay ~A&-desc)))\n"
		  typename typename)))
	(else
	 ;; 3) struct never defined; only used as pointer
	 (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)))
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))

      ;; typedef union foo { ... } foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (union-def (ident ,union-name) ,field-list)))
	(init-declr (ident ,typename)))
       (cnvt-union-def typename union-name field-list)
       (values
	(cons* typename (w/* typename) (w/union union-name) wrapped)
	(cons* typename (w/* typename) (w/union union-name) defined)))

      ;; typedef union { ... } foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (union-def ,field-list)))
	(init-declr (ident ,typename)))
       (cnvt-struct-def typename #f field-list)
       (values (cons* typename (w/* typename) wrapped)
	       (cons* typename (w/* typename) defined)))

      ;; typedef union foo foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (union-ref (ident ,union-name))))
	(init-declr (ident ,typename)))
       (cond
	;; This case represents three possible uses:
	((member union-name defined)
	 ;; 1) union defined previously
	 (sfscm "(define-public ~A-desc union-~A-desc)\n" typename union-name)
	 (sfscm "(define-public ~A*-desc union-~A*-desc)\n"
		typename union-name))
	((udict-union-ref udict union-name) =>
	 ;; 2) union defined later
	 (lambda (union-decl)
	   (back-ref-extend! union-decl typename)
	   (sfscm "(define-public ~A-desc 'void)\n" typename)
	   (sfscm "(define-public ~A*-desc (bs:pointer (delay ~A-desc)))\n"
		typename typename)))
	(else
	 ;; 3) union never defined; only used as pointer
	 (sfscm "(define-public ~A-desc 'void)\n" typename)
	 (sfscm "(define-public ~A*-desc (bs:pointer ~A-desc))\n"
		typename typename)))
       (fhscm-def-pointer (sw/* typename))
       (values (cons* typename (w/* typename) wrapped)
	       (cons* typename (w/* typename) defined)))

      ;; typedef union foo *foo_t;
      ;; TODO: check for forward reference
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (union-ref (ident ,union-name))))
	(init-declr (ptr-declr (pointer) (ident ,typename))))
       (cond
	;; This case represents three possible uses:
	((member union-name defined)
	 ;; 1) union defined previously
	 (sfscm "(define-public ~A-desc union-~A*-desc)\n"
		typename union-name))
	((udict-union-ref udict union-name) =>
	 ;; 2) union defined later
	 (lambda (union-decl)
	   (back-ref-extend! union-decl (sw/& typename))
	   (sfscm "(define-public ~A&-desc 'void)\n" typename)
	   (sfscm "(define-public ~A-desc (bs:pointer (delay ~A&-desc)))\n"
		typename typename)))
	(else
	 ;; 3) union never defined; only used as pointer
	 (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)))
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))

      ;; typedef int (*foo_t)(int x, ...);
      ;; extern int git_reference_foreach(git_repository *repo, 
      ;;     git_reference_foreach_cb callback, void *payload);
      ((udecl
	(decl-spec-list (stor-spec (typedef)) . ,rst)
	(init-declr
	 (ftn-declr
	  (scope (ptr-declr (pointer) (ident ,typename)))
	  (param-list . ,params))))
       ;;(if (string=? typename "git_reference_foreach") (pperr udecl))
       (let* ((ret-decl `(udecl (decl-spec-list . ,rst)
				(init-declr (ident "_"))))
	      (decl-return (gen-decl-return ret-decl))
	      (decl-params (gen-decl-params params)))
	 (fhscm-def-function* typename decl-return decl-params))
       (values (cons typename wrapped) (cons typename defined)))

      ;; typedef void* (*foo_t)(int x, ...);
      ((udecl
	(decl-spec-list (stor-spec (typedef)) . ,rst)
	(init-declr
	 (ptr-declr
	  (pointer)
	  (ftn-declr
	   (scope (ptr-declr (pointer) (ident ,typename)))
	   (param-list . ,params)))))
       (let* ((ret-decl `(udecl (decl-spec-list . ,rst)
				(init-declr (ptr-declr (pointer) (ident "_")))))
	      (decl-return (gen-decl-return ret-decl))
	      (decl-params (gen-decl-params params)))
	 (fhscm-def-function* typename decl-return decl-params))
       (values (cons typename wrapped) (cons typename defined)))

      ;; TODO: typedef void (foo_t)(int x)  [instead of *foo_t]
      ;; TODO: typedef void* (foo_t)(int x)

      ;; typedef foo_t bar_t
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (typename ,name)))
	(init-declr (ident ,typename)))
       (cond
	((member name bs-defined)
	 (values wrapped defined))
	((member name defined)
	 (sfscm "(define-public ~A-desc ~A-desc)\n" typename name)
	 (sfscm "(define-fh-type-alias ~A ~A)\n" typename name)
	 (sfscm "(export ~A)\n" typename)
	 (sfscm "(define-public ~A? ~A?)\n" typename name)
	 (sfscm "(define-public make-~A make-~A)\n" typename name)
	 (when (member (w/* name) defined)
	   (sfscm "(define-public ~A*-desc ~A*-desc)\n" typename name)
	   (sfscm "(define-fh-type-alias ~A* ~A*)\n" typename name)
	   (sfscm "(export ~A*)\n" typename)
	   (sfscm "(define-public ~A*? ~A*?)\n" typename name)
	   (sfscm "(define-public make-~A* make-~A*)\n" typename name))
	 (values (cons typename wrapped) (cons typename defined)))
	(else
	 (let ((xdecl (expand-typerefs udecl udict defined)))
	   (cnvt-udecl xdecl udict wrapped defined)))))

      ;; === structs and unions ==========

      ;; struct foo { ... }.
      ((udecl
	(decl-spec-list
	 (type-spec (struct-def (ident ,struct-name) ,field-list))))
       (cond
	((back-ref-getall udecl) =>
	 (lambda (name-list)
	   (cnvt-struct-def #f struct-name field-list)
	   (for-each
	    (lambda (name)
	      (sfscm "(set! ~A-desc struct-~A-desc)\n" name struct-name)
	      (fhscm-def-compound name))
	    name-list)
	   (values (cons (w/struct struct-name) wrapped)
		   (cons (w/struct struct-name) defined))))
	((not (member (w/struct struct-name) defined))
	 (cnvt-struct-def #f struct-name field-list)
	 ;; Hopting don't need w/struct*
	 (values (cons* (w/struct struct-name) wrapped)
		 (cons* (w/struct struct-name) defined)))
	(else
	 (values wrapped defined))))

      ;; struct { ... } ...
      ((udecl
	(decl-spec-list
	 (type-spec (struct-def ,field-list))))
       (sferr "bug in munge? unnamed struct-def\n")
       (pperr udecl)
       (values wrapped defined))

      ;; union foo { ... }.
      ((udecl
	(decl-spec-list
	 (type-spec (union-def (ident ,union-name) ,field-list))))
       (cond
	((back-ref-getall udecl) =>
	 (lambda (name-list)
	   (cnvt-union-def #f union-name field-list)
	   (for-each
	    (lambda (name)
	      (sfscm "(set! ~A-desc union-~A-desc)\n" name union-name)
	      (fhscm-def-compound name))
	    name-list)
	   (values (cons (w/union union-name) wrapped)
		   (cons (w/union union-name) defined))))
	((not (member (w/union union-name) defined))
	 (cnvt-union-def #f union-name field-list)
	 (values (cons (w/union union-name) wrapped)
		 (cons (w/union union-name) defined)))
	(else
	 (values wrapped defined))))

      ;; union { ... } ...
      ((udecl
	(decl-spec-list
	 (type-spec (union-def ,field-list))))
       (sferr "bug in munge? unnamed union-def\n")
       (pperr udecl)
       (values wrapped defined))

      ;; === enums =======================

      ;; enum foo { ... };
      ((udecl
	(decl-spec-list
	 (type-spec (enum-def (ident ,enum-name) ,enum-def-list . ,rest))))
       (cnvt-enum-def #f enum-name enum-def-list)
       ;; probably never use this as arg to function
       (values (cons (w/enum enum-name) wrapped) defined))
      
      ;; enum { ... };
      ((udecl
	(decl-spec-list
	 (type-spec (enum-def ,enum-def-list . ,rest))))
       ;; This is now filtered in the caller so the C-decl is not printed.
       (values wrapped defined))

      ;; === function declarations =======
      
      ;; function returning pointer value
      ((udecl ,specl
	      (init-declr
	       (ptr-declr
		(pointer . ,rest)
		(ftn-declr (ident ,name) (param-list . ,params)))))
       (cnvt-fctn name (ptr-decl specl) params)
       (values wrapped defined))

      ;; function returning non-pointer value
      ;; TODO: parse ident part and process separately
      ((udecl ,specl
	      (init-declr
	       (ftn-declr (ident ,name) (param-list . ,params))))
       (cnvt-fctn name (non-ptr-decl specl) params)
       (values wrapped defined))
      ((udecl ,specl
	      (init-declr
	       (ftn-declr (scope (ident ,name)) (param-list . ,params))))
       (cnvt-fctn name (non-ptr-decl specl) params)
       (values wrapped defined))
      ((udecl ,specl
	      (init-declr
	       (ftn-declr (scope (ptr-declr (pointer . ,rest)
					    (ident ,name)))
			  (param-list . ,params))))
       (cnvt-fctn name (ptr-decl specl) params)
       (values wrapped defined))

      ;; === external variables =========

      ;; pointer
      ((udecl (decl-spec-list (stor-spec (extern)) ,type-spec)
	      (init-declr (ptr-declr (pointer) (ident ,name))))
       (sfscm "(define ~A (dynamic-pointer ~S (dynamic-link)))\n" name name)
       (values wrapped defined))

      ;; non-pointer
      ((udecl (decl-spec-list (stor-spec (extern)) ,type-spec)
	      ,init-declr . ,rest)
       (let* ((udecl (expand-typerefs udecl udict (*defined*)))
	      (udecl (udecl-rem-type-qual udecl))
	      (mspec (udecl->mspec udecl)))
	 ;;(sferr "extern mspec:\n") (pperr mspec)
	 (cnvt-extern (car mspec) (cdr mspec))
	 (values wrapped defined)))

      ;; === special cases I need to fix =

      ;; from glib-2.0/gio.h
      ((udecl
	(decl-spec-list (stor-spec (typedef)) (type-spec (typename ,name)))
	(init-declr
	 (ptr-declr (pointer (pointer))
		    (scope (ftn-declr (scope (ptr-declr) (ident ,typename))
				      ,param-list)))))
       (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))
      ((udecl
	(decl-spec-list (stor-spec (typedef)) (type-spec (typename ,name)))
	(init-declr
	 (ptr-declr (pointer (pointer))
		    (ftn-declr (scope (ptr-declr (pointer) (ident ,typename)))
			       ,param-list))))
       (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))

      ;; from zzip/zzip.h
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (fixed-type ,typename))) ;; char
	(init-declr
	 (ptr-declr
	  (pointer (type-qual-list . ,rest))
	  (ident ,name))))
       (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))

      ;; from hdf5.h
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (fixed-type "unsigned char")))
	(init-declr
	 (array-of
	  (ident "hdset_reg_ref_t")
	  (add (sizeof-type
		(type-name
		 (decl-spec-list (type-spec (typename "haddr_t")))))
	       (p-expr (fixed "4"))))))
       (let* ((typename "haddr_t")
	      (size (+ (sizeof '*) 4))
	      )
	 (sfscm "(define-public ~A-desc (bs:vector ~A '*))\n" size typename)
	 ;;(sfscm "(define-fh-compound-type/p ~A ~A-desc)\n" typename typename)
	 (fhscm-def-compound typename)
	 (values (cons typename wrapped) (cons typename defined))))
      
      ;; === missed =====================

      (,otherwise
       (sferr "see below:\n")
       (pperr udecl)
       ;;(sferr "-\n")
       ;;(pperr `(udecl ,specl ,declr))
       (error "cnvt-udecl")
       (fherr "cnvt-udecl missed --^")
       (values wrapped defined)))))


;; === enums and #defined => lookup

;; given keeper-defs (k-defs) and cpp defs (c-defs) expand the keeper
;; replacemnts down to constants (strings, integers, etc)
(define (gen-lookup-proc prefix keep-defs cpp-defs ext-mods)
  ;; @var{keep-defs} is list of CPP defs and enum key/val pairs. It is
  ;; possible for an enum symbol to be used as a macro function so we
  ;; need to first check for integer before trying expand-cpp-macro-ref.
  (sfscm "\n;; access to enum symbols and #define'd constants:\n")
  (let ((name
	 (string->symbol (string-append prefix "symbol-val")))
	(defs
	  (fold
	   (lambda (def seed)
	     (let* ((name (car def)) (val (cdr def))
		    (repl (cond
			   ((pair? val) #f)
			   ((string->number (cdr def)) (cdr def))
			   (else (expand-cpp-macro-ref name cpp-defs)))))
	       (cond
		((not repl) seed)
		((not (string? repl)) (sferr "not string: ~S\n" repl))
		((zero? (string-length repl)) seed)
		((cintstr->num repl) =>
		 (lambda (val) (acons (string->symbol name) val seed)))
		((eqv? #\" (string-ref repl 0))
		 (acons (string->symbol name)
			(regexp-substitute/global ;; "abc" "def" => "abcdef"
			 #f "\"\\s*\""
			 (substring repl 1 (- (string-length repl) 1))
			 'pre 'post)
			seed))
		(else seed))))
	   '()
	   keep-defs))
	(ext-ftns			; lookup in use-ffi-modules
	 (map
	  (lambda (mod)
	    (list (string->symbol
		   (string-append (path->name mod) "-symbol-val")) 'k))
	  ext-mods)))
    (ppscm `(define ,name
	      (let ((sym-tab '(,@defs)))
		(lambda (k) (or (assq-ref sym-tab k) ,@ext-ftns)))))
    (sfscm "(export ~A)\n" name)
    ;;
    (nlscm)
    (ppscm
     `(define (unwrap-enum obj)
	(cond
	 ((number? obj) obj)
	 ((symbol? obj) (,name obj))
	 ((fh-object? obj) (struct-ref obj 0)) ;; ???
	 (else (error "type mismatch")))))
    ))


;; === Parsing the C header(s)

;; This routine generates a top-level source string-file with all the includes,
;; parses it, and then merges one level down of includes into the top level,
;; as if the bodies of the incudes had been combined into one file.
(define parse-includes
  (let* ((p (node-join
	     (select-kids (node-typeof? 'cpp-stmt))
	     (select-kids (node-typeof? 'include))
	     (select-kids (node-typeof? 'trans-unit))))
	 (merge-inc-bodies
	  (lambda (t) (cons 'trans-unit (apply append (map cdr (p t)))))))
    (lambda (attrs)
      (let* ((cpp-defs (resolve-attr-val (assq-ref attrs 'cpp-defs)))
	     (inc-dirs (resolve-attr-val (assq-ref attrs 'inc-dirs)))
	     (inc-help (resolve-attr-val (assq-ref attrs 'inc-help)))

	     (pkg-config (assq-ref attrs 'pkg-config))
	     (cpp-defs (append (pkg-config-defs pkg-config) cpp-defs))
	     (inc-dirs (append (pkg-config-incs pkg-config) inc-dirs))

	     (cpp-defs (append cpp-defs fh-cpp-defs))
	     (inc-dirs (append inc-dirs fh-inc-dirs))
	     (inc-help (append inc-help fh-inc-help))
	     
	     (inc-files (resolve-attr-val (assq-ref attrs 'include)))
	     (prog (string-join
		    (map
		     (lambda (inc-file)
		       (string-append "#include \"" inc-file "\"\n"))
		     inc-files) ""))

	     ;;(inc-dirs (cons "." inc-dirs)) ;; FOR DEBUGGING
	     )

	(or (with-input-from-string prog
	      (lambda ()
		(and=> 
		 (parse-c99 #:cpp-defs cpp-defs
			    #:inc-dirs inc-dirs
			    #:inc-help inc-help
			    #:mode 'decl
			    #:debug (*debug*))
		 merge-inc-bodies)))
	    (fherr "parse failed"))))))

;; === main converter ================

(define (derive-dirpath sfile mbase)
  (if (not sfile) "./"
      (let* ((sbase (string-drop-right sfile 4))
	     (sfxln (string-suffix-length sbase mbase))
	     (sblen (string-length sbase)))
	(if (not (= sfxln (string-length mbase))) ; need more robust
	    (error "filename-path inconsistent"))
	(substring sbase 0 (- sblen sfxln)))))

;; process define-ffi-module expression
(define (intro-ffi path module-options)
  (let* ((script-options (*options*))
	 ;;(mbase (string-append (string-join (map symbol->string path) "/")))
	 (mbase (path->path path))
	 (dirpath (derive-dirpath (assq-ref script-options 'file) mbase))
	 (mfile (string-append dirpath mbase ".scm"))
	 (mport (open-output-file mfile))
	 ;;
	 (attrs (opts->attrs module-options script-options))
	 (incf (or (assq-ref attrs 'inc-filter) #f))
	 (declf (or (assq-ref attrs 'decl-filter) identity))
	 (renamer (or (assq-ref attrs 'renamer) identity))
	 (prefix (string-append (path->name path) "-"))
	 ;;
	 (tree (parse-includes attrs))
	 (udecls (c99-trans-unit->udict tree #:inc-filter incf))
	 (udict (c99-trans-unit->udict/deep tree))
	 (ffi-decls (map car udecls))	; just the names, get decls from udict

	 ;; TODO: clean this up
	 ;;(enu-defs (udict-enums->ddict udict))
	 (enu-defs (udict-enums->ddict udecls))
	 (ffi-defs (c99-trans-unit->ddict tree enu-defs #:inc-filter incf))
	 (cpp-defs (c99-trans-unit->ddict tree #:inc-filter #t))
	 (all-defs (c99-trans-unit->ddict tree enu-defs
					  #:inc-filter #t #:skip-fdefs #t))

	 ;; the list of typedefs we will generate (later):
	 (ffimod-defined #f)

	 (saw-last #f)

	 (ext-mods			; ext modules (e.g., '(ffi cairo) ...)
	  (fold-right
	   (lambda (opt seed)
	     (if (eq? (car opt) 'use-ffi-module) (cons (cdr opt) seed) seed))
	   '() module-options))
	 (ext-defd			; list of exernal defined
	  (fold
	   (lambda (upath seed)
	     (unless (resolve-module upath)
	       (error "module not defined:" upath))
	     (let* ((modul (resolve-module upath))
		    (pname (path->name upath))
		    (vname (string->symbol (string-append pname "-types")))
		    (var (module-ref modul vname)))
	       (append var seed)))
	   '() ext-mods))
	 )

    ;; set globals
    (*udict* udict)
    (*mport* mport)
    (*all-defs* all-defs)
    ;; renamer?

    ;; file and module header
    (ffimod-header path module-options)

    ;; convert and output foreign declarations
    (call-with-values
	(lambda ()
	  (fold-values			  ; from (sxml fold)
	   (lambda (name wrapped defined) ; name: "foo_t" or (enum . "foo")
	     (catch 'ffi-help-error
	       (lambda ()
		 (cond
		  (saw-last
		   (c99scm (udict-ref udict name))
		   (values wrapped defined)) ; for debugging
		  
		  ((and ;; Process the declaration if all conditions met:
		    (declf name)		; 1) user wants it
		    (not (member name defined))	; 2) not already defined
		    (not (and (pair? name)	; 3) not anonymous
			      (string=? "*anon*" (cdr name)))))
		   ;;(sferr "~A\n" name)
		   (when (or
			  ;;(equal? name "gtk_im_context_simple_get_type")
			  #f)
		     ;;(pperr (udict-ref udict name))
		     (set! saw-last #t)
		     )
		   (let ((udecl (udict-ref udict name)))
		     (nlscm) (c99scm udecl)
		     (if *echo-decls*
			 (sfscm "(if echo-decls (display \"~A\\n\"))\n" name))
		     (cnvt-udecl (udict-ref udict name) udict wrapped defined))
		   )
		  
		  (else (values wrapped defined))))
	       (lambda (key fmt . args)
		 (if fmt
		     (apply simple-format (current-error-port)
			    (string-append "ffi-help: " fmt "\n") args))
		 (sfscm ";; ... failed.\n")
		 (values wrapped defined))))
	   ;; We need to have externs in wrapped because function param types
	   ;; have wrapped types preserved (e.g., enums).
	   ffi-decls ext-defd (append bs-defined ext-defd)))
      (lambda (wrapped defined)
	;; Set ffimod-defined for including, but removed built-in types.
	(let* ((bity (car bs-defined))	; first built-in type
	       (defd (let iter ((res '()) (defs defined))
		       (if (eq? (car defs) bity) res
			   (iter (cons (car defs) res) (cdr defs))))))
	  (set! ffimod-defined defd))))
    
    ;; output global constants (from enum and #define)
    (gen-lookup-proc prefix ffi-defs cpp-defs ext-mods)

    ;; output list of defined types
    (sfscm "\n(define ~A-types\n  '" (path->name path))
    (ugly-print ffimod-defined mport #:per-line-prefix "   " #:trim-ends #t)
    (sfscm ")\n(export ~A-types)\n" (path->name path))

    (sfscm "\n;; TODO: add renamer\n")

    ;; return port so compiler can output remaining code
    mport))

;; This macro converts #:key val to '(key val) for ffi-help options
;; and preserves other #:key-val pairs for passthrough to the module
;; Note that keywords are converted to symbols before they get here
;; NOT USED.
(define-syntax parse-ffimod-option
  (lambda (x)
    (define (sym->key stx)
      (datum->syntax stx (symbol->keyword (syntax->datum stx))))
    (syntax-case x (cpp-defs
		    decl-filter inc-dirs inc-filter inc-help include
		    library pkg-config renamer)
      ((_ cpp-defs proc) #'(cons 'cpp-defs proc))
      ((_ decl-filter proc) #'(cons 'decl-filter proc))
      ((_ inc-dirs proc) #'(cons 'inc-dirs proc))
      ((_ inc-filter proc) #'(cons 'inc-filter proc))
      ((_ inc-help expr) #'(cons 'inc-help expr))
      ((_ include expr) #'(cons 'include expr))
      ((_ library expr) #'(cons 'library expr)) ;; eval to list of libs
      ((_ pkg-config string) #'(cons 'pkg-config string))
      ((_ renamer proc) #'(cons'renamer (quote proc)))
      ;;((_ use-ffi-module path) #'(cons 'use-ffi-module (quote path)))
      ;; remaining options get passed to the module decl as-is:
      ;;((_ key val) #`(cons #,(sym->key #'key) (quote val)))
      ;;((_ key val) #`(cons #,(symbol->keyword #'key) (quote val)))
      )))

(define-syntax parse-module-options
  (lambda (x)
    (define (key->sym stx)
      (datum->syntax stx (keyword->symbol (syntax->datum stx))))
    (define (ffimod-option? key)
      (and (keyword? key)
	   (member key '(#:cpp-defs
			 #:decl-filter #:inc-dirs #:inc-filter #:inc-help
			 #:include #:library #:pkg-config #:renamer
			 #:use-ffi-module))))
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
  (intro-ffi (quote path-list) (parse-module-options attr ...)))


;; === file compiler ================

(use-modules (system base language))
(use-modules (ice-9 pretty-print))

(define (string-member-proc . args)
  (lambda (s) (member s args)))

;; to convert symbol-based #:renamer to string-based
(define (string-renamer proc)
  (lambda (s) (string->symbol (proc (symbol->string s)))))

(define scm-reader (language-reader (lookup-language 'scheme)))

;; @deffn {Procedure} compile-ffi-file file [options]
;; This procedure will 
;; @end deffn
(define* (OLD-compile-ffi-file file #:optional (options '()))
  (parameterize ((*options* (acons 'file file options))
		 (*prefix* ".")
		 (*mport* #t)
		 (*udict* '())
		 (*wrapped* '())
		 (*defined* '())
		 (*renamer* identity)
		 (*errmsgs* '()))
    ;;(sfout "+++ warning: the FFI helper is experimental\n")
    (sfout "ffi-help: WARNING: the FFI helper is experimental\n")
    ;; if not interactive ...
    (debug-disable 'backtrace)
    (call-with-input-file file
      (lambda (iport)
	(let iter ((oport #f))
	  ;; use scm-reader or read here?
	  (let ((exp (scm-reader iport (current-module))))
	    (cond
	     ((eof-object? exp)
	      (when oport
		(display "\n;; --- last line ---\n" oport)
		(sferr "wrote `~A'\n" (port-filename oport))
		(close-port oport)))
	     ((and (pair? exp) (eqv? 'define-ffi-module (car exp)))
	      (iter (eval exp (current-module))))
	     (else
	      (when oport
		(newline oport)
		(pretty-print exp oport)
		(iter oport))))))))))

(define* (compile-ffi-file file #:optional (options '()))
  (parameterize ((*options* (acons 'file file options))
		 (*prefix* ".")
		 (*mport* #t)
		 (*udict* '())
		 (*wrapped* '())
		 (*defined* '())
		 (*renamer* identity)
		 (*errmsgs* '()))
    ;;(sfout "+++ warning: the FFI helper is experimental\n")
    (sfout "ffi-help: WARNING: the FFI helper is experimental\n")
    ;; if not interactive ...
    (debug-disable 'backtrace)
    (call-with-input-file file
      (lambda (iport)
	(let iter ((oport #f) (exp (read iport)))
	  (cond
	   ((eof-object? exp)
	    (when oport
	      (display "\n;; --- last line ---\n" oport)
	      (sferr "wrote `~A'\n" (port-filename oport))
	      (close-port oport)))
	   ((and (pair? exp) (eqv? 'define-ffi-module (car exp)))
	    (iter (eval exp (current-module)) (read iport)))
	   (else
	    (when oport
	      (newline oport)
	      (pretty-print exp oport))
	    (iter oport (read iport)))))))))

;; --- last line ---
