;;; example/nyacc/lang/c99/ffi-help.scm
;;;
;;; Copyright (C) 2016-2017 Matthew R. Wette
;;;
;;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;;; or any later version published by the Free Software Foundation.  See
;;; the file COPYING included with the nyacc distribution.

;; TODO:
;; 1) copyright notice

;; WARNING: this is a prototype in development: anything goes right now

(add-to-load-path (string-append (getcwd) "/../../../../module"))

(define-module (ffi-help)
  #:export (*ffi-help-version*
	    define-ffi-module
	    compile-ffi-file
	    intro-ffi
	    unwrap-char*
	    string-member-proc string-renamer
	    pkg-config-incs
	    )
  #:use-module (nyacc lang c99 parser)
  #:use-module (nyacc lang c99 util1)
  #:use-module (nyacc lang c99 util2)
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang c99 pprint)
  #:use-module (nyacc lang util)
  #:use-module ((bytestructures guile) #:prefix bs:)
  #:use-module (system foreign)
  #:use-module (sxml fold)
  #:use-module (sxml match)
  #:use-module ((sxml xpath)
		 #:renamer (lambda (s) (if (eq? s 'filter) 'sxml:filter s)))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (system base pmatch)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  ;;#:re-export (make-record-type bs:bytestructure-bytevector)
  #:version (0 10 0)
  )

(use-modules (ice-9 pretty-print))

;; User is responsible for calling string->pointer and pointer->string.
;; 
;; By definition: wrap is c->scm; unwrap is scm->c
;; 

;; define-ffi-module options:
;; @table @code
;; @item #:decl-filter proc
;; @item #:inc-filter proc
;; @item #:include expr
;; expr is string or list or procecure that evaluates to string or list
;; @item #:library expr
;; expr is string or list or procecure that evaluates to string or list
;; @item #:pkg-config string
;; @item #:renamer proc
;; procdure
;; @end table

;; @table code
;; @item mspec->fh-wrapper
;; generates code to apply wrapper to objects returned from foreign call
;; @item mspec->fh-unwrapper
;; generated code to apply un-wrapper to arguments for foreign call
;; @end table

;; user interface
;; (define srf (cairo_svg_surface_create (string->pointer "abc.svg") 20.0 20.0))
;; (define cr (cairo_create srf))
;; (define mx (make-cairo-unit-matrix))
;; (cairo_get_font_matrix cr (pointer-to mx))
;; (define (unwrap-cairo_matrix_t mx)

;; also have in (bytestructures guile ffi)
;; bytestructure->descriptor->ffi-descriptor
;; bs:pointer->proc

;; what if foo(foo_t x[],
;; * user must make vector of foo_t
;; * ffi-module author should generate a make-foo_t-vector procedure

;; guardians for cairo_destroy cairo_surface_destroy 

;; assume that no raw aggregates get passed to c-functions for now

;; TODO
;; 02 if need foo_t pointer then I gen wrapper for foo_t* but add
;;    foo_t to *wrappers* so if I later run into need for foo_t may be prob
;; 03 allow user to specify #:renamer (lambda (n) "make_goo" => "make-goo")
;; DONE
;; 01 enum-wrap 0 => 'CAIRO_STATUS_SUCCESS
;;    enum-unwrap 'CAIRO_STATUS_SUCCESS => 0

(define *ffi-help-version* "0.02.0")

(define std-inc-dirs
  `(,(assq-ref %guile-build-info 'includedir)
    "/usr/include"
    ))
(define std-inc-help
  '(;;("__builtin" "__builtin_va_list=void*")
    ;;("sys/cdefs.h" "__DARWIN_ALIAS(X)=")
    ))

(define *debug* #f)
(define *port* #t)
(define *uddict* '())
(define *renamer* identity)

;; wrapped - keepers = pointer types [need the pointer types]
;; keepers - wrapped = builtin types
(define *keepers* '()) ;; list of strings of defined types + builtin 
;;(define *wrapped* '()) ;; list of strings of wrapped types (incl pointers)
;; I think we can just keep keepers and filter out builtin for wrappers

;; WORK TO GO
;; now hard part if we want to reference other ffi-modules for types
;; or other c-routines
;; say ffi-module foo defines foo_t
;; now in ffi-module bar we want to reference, but redefine, foo_t
;; (define-ffi-module (bar)
;;   #:use-ffi-module (foo)
;;   )
;; example:
;; (define-ffi-module (cairo cairo) ...)
;; (define-ffi-module (cairo cairo-svg) #:use-ffi-module (cairo cairo)

(define (sfscm fmt . args)
  (apply simple-format *port* fmt args))
(define* (ppscm tree #:key (per-line-prefix ""))
  (pretty-print tree *port* #:per-line-prefix per-line-prefix))
(define (c99scm tree)
  (pretty-print-c99 tree *port* #:per-line-prefix ";; "))
(define (nlscm) (newline *port*))

(define (sfout fmt . args)
  (apply simple-format #t fmt args))
(define (ppout tree)
  (pretty-print tree #:per-line-prefix "    "))
(define (nlout) (newline))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr tree)
  (pretty-print tree (current-error-port) #:per-line-prefix "    "))


(define (fherr fmt . args)
  (apply throw 'ffi-help-error fmt args))


;; === utilities

(define (opts->attrs opts)
  (filter (lambda (pair) (symbol? (car pair))) opts))

(define (opts->mopts opts) ;; module options to pass
  (filter (lambda (pair) (keyword? (car pair))) opts))

;; Run pkg-config
(define (pkg-config name . args)
  (let* ((port (open-input-pipe (string-join (cons* "pkg-config " name args))))
	 (ostr (read-line port))
	 (items (string-split ostr #\space)))
    (close-port port)
    items))

;; use pkg-config to get a list of include dirs
;; (pkg-config-incs "cairo") => ("/opt/local/include/cairo" ...)
(define (pkg-config-incs name)
  (fold-right
   (lambda (s l)
     (if (string=? "-I" (substring/shared s 0 2))
	 (cons (substring/shared s 2) l)
	 l))
   '()
   (pkg-config name "--cflags")))

(define (pkg-config-defs name)
  (fold-right
   (lambda (s l)
       (if (string=? "-D" (substring/shared s 0 2))
	   (cons (substring/shared s 2) l)
	   l))
     '()
     (pkg-config name "--cflags")))

(define (pkg-config-libs name)
    (fold-right
     (lambda (s l)
       (if (string=? "-l" (substring/shared s 0 2))
	   (cons (string-append "lib" (substring/shared s 2)) l)
	   l))
     '()
     (pkg-config name "--libs")))

(define (resolve-attr-val val)
  (let* ((val (if (procedure? val) (val) val)))
    (cond
     ((eq? #f val) '())
     ((list? val) val)
     ((string? val) (list val))
     (else (throw 'fh-error "value does not resolve to list")))))

;; === output scheme module header 

(define (ffimod-header path opts)
  (let* ((attrs (opts->attrs opts))
	 (pkg-config (assq-ref attrs 'pkg-config))
	 (libraries (resolve-attr-val (assq-ref attrs 'library)))
	 (libraries (append
		     (if pkg-config (pkg-config-libs pkg-config) '())
		     libraries))
	 (library (car libraries)))
    (sfscm ";; auto-generated by ffi-help.scm\n")
    (nlscm)
    (sfscm "(define-module ~S\n" path)
    (for-each ;; output pass-through options
     (lambda (pair) (sfscm "  ~S " (car pair)) (ppscm (cdr pair)))
     (opts->mopts opts))
    (sfscm "  #:use-module (ffi-help-rt)\n")
    (sfscm "  #:use-module ((system foreign) #:prefix ffi:)\n")
    (sfscm "  #:use-module ((bytestructures guile) #:prefix bs:)\n")
    ;;(sfscm "  #:use-module (bytestructures guile)\n")
    (sfscm "  #:use-module (srfi srfi-9)\n")
    (sfscm "  #:use-module (srfi srfi-9 gnu)\n")
    (sfscm "  )\n")
    (sfscm "(define bs:struct bs:bs:struct)\n")
    (sfscm "(define bs:union bs:bs:union)\n")
    (sfscm "(define bs:pointer bs:bs:pointer)\n")
    (sfscm "(define void*  (bs:pointer bs:int32))\n")
    (nlscm)
    (for-each
     (lambda (l) (sfscm "(dynamic-link ~S)\n" l))
     libraries)
    (sfscm "(define (lib-func name) (dynamic-func name ~A))\n" library)))


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

(define (unwrap-char* value)
  (if (string? value)
      (string->pointer value)
      value))

(define (mtype->bs mspec-tail)
  (pmatch mspec-tail
    (((fixed-type ,name)) (string-append "bs:" name))
    (((float-type ,name)) (string-append "bs:" name))
    (((void)) "bs:void")
    ;;
    (((pointer-to) (fixed-type ,name)) (string-append "bs:" name "*"))
    (((pointer-to) (float-type ,name)) (string-append "bs:" name "*"))
    (((pointer-to) (void)) "bs:void*")
    ;;
    (((typename ,name)) name)
    ;;(((pointer-to) (typename ,name)) (string-append name "*"))
    (((pointer-to) (typename ,name)) "bs:void*")
    ;;
    (,otherwise (error "1: missed" mspec-tail))))


;; --- structs and unions

(define (copy-of:mtype->bs mspec-tail)
  (pmatch mspec-tail
    (((fixed-type ,name)) (string-append "bs:" name))
    (((float-type ,name)) (string-append "bs:" name))
    (((void)) "bs:void")
    ;;
    (((pointer-to) (fixed-type ,name)) (string-append "bs:" name "*"))
    (((pointer-to) (float-type ,name)) (string-append "bs:" name "*"))
    (((pointer-to) (void)) "bs:void*")
    ;;
    (((typename ,name)) name)
    (((pointer-to) (typename ,name)) (string-append name "*"))
    ;;
    (,otherwise (error "2: missed" mspec-tail))))

(define (cnvt-field-list field-list)
  (define (acons-defn name type seed)
    (cons (eval-string (string-append "(quote `(" name " ," type "))")) seed))

  (let* ((fldl (clean-field-list field-list)) ; remove lone comments
	 (flds (cdr fldl))
	 (uflds (fold munge-comp-decl '() flds)) ; in reverse order
	 )
    (let iter ((sflds '()) (decls uflds))
      (if (null? decls) sflds
	  (let* ((name (caar decls))
		 (udecl (cdar decls))
		 (spec (udecl->mspec/comm udecl))
		 (type (mtype->bs (cddr spec))))
	    ;;(nlout) (ppout udecl) (ppout (cons name type))
	    (iter (acons-defn name type sflds) (cdr decls)))))))

;; aggr-t (tag) is 'struct or 'union
;; typename is string or #f
;; aggr-name is string or #f

;; not working with ....
;; union _cairo_path_data_t {
;;     struct {
;;         cairo_path_data_type_t type;
;;         int length;
;;     } header;
;;     struct {
;;         double x, y;
;;     } point;
;; };
;; should convert to
#;(define xxx
  (bs:union
   (list `(header ,(bs:struct (list `(type ,cairo_data_path_t???)
				   `(length ,bs:int))))
	 `(point ,(bs:struct (list `(x ,bs:double) `(y ,bs:double)))))))

(define (cnvt-aggr-def aggr-t typename aggr-name field-list)
  ;;(cnvt-field-list field-list)
  ;;(quit)
  (let* ((aggr-s (symbol->string aggr-t))
	 (bs-aggr-t (string->symbol (string-append "bs:" aggr-s)))
	 (fldl (clean-field-list field-list)) ; remove lone comments
	 (flds (cdr fldl))
	 (uflds (fold munge-comp-decl '() flds)) ; in reverse order
	 (sflds (cnvt-field-list field-list)))
    (cond
     ((and typename aggr-name)
      (sfscm "(define-fh-aggregate-type ~A\n" typename)
      (ppscm `(,bs-aggr-t (list ,@sflds)) #:per-line-prefix "  ")
      (sfscm "  )\n")
      (sfscm "(define ~A-~A ~A)\n" aggr-s aggr-name typename)
      (sfscm "(export ~A-~A)\n" aggr-s aggr-name)
      (sfscm "(define-fh-pointer-type ~A*)\n" typename)
      )
     (typename
      (ppscm `(define ,(string->symbol typename) (,bs-aggr-t (list ,@sflds))))
      ;;(sfscm "(define-std-pointer-type ~A*)\n" typename)
      (sfscm "(export ~A)\n" typename))
     (aggr-name
      (ppscm `(define ,(string->symbol (string-append aggr-s "-" aggr-name))
		(,bs-aggr-t (list ,@sflds))))
      ;;(sfscm "(define-std-pointer-type ~A-~A*)\n" aggr-s aggr-name)
      (sfscm "(export ~A-~A)\n" aggr-s aggr-name))
     (else
      ;; nothing to do?
      #f))))

(define (cnvt-struct-def typename struct-name field-list)
  (cnvt-aggr-def 'struct typename struct-name field-list))

(define (cnvt-union-def typename union-name field-list)
  (cnvt-aggr-def 'union typename union-name field-list))

;; --- enums

(define (cnvt-enum-def typename enum-name enum-def-list)
  (let* ((name-val-l (map
		      (lambda (def)
			(pmatch def
			  ((enum-defn (ident ,n) (p-expr (fixed ,v)))
			   (cons (string->symbol n) (string->number v)))
			  ((enum-defn (ident ,n) (neg (p-expr (fixed ,v))))
			   (cons (string->symbol n) (- (string->number v))))
			  (,otherwise (error "3: cnvt-enum-def coding" def))))
		      (cdr (canize-enum-def-list enum-def-list))))
	 (val-name-l (map (lambda (p) (cons (cdr p) (car p))) name-val-l))
	 (makeum (lambda (n)
		   (let ((w-name (string->symbol (string-append "wrap-" n)))
			 (u-name (string->symbol (string-append "unwrap-" n))))
		     (ppscm `(define ,w-name
			       (let ((vnl '(,@val-name-l)))
				 (lambda (code) (assq-ref vnl code)))))
		     (ppscm `(define ,u-name
			       (let ((nvl '(,@name-val-l)))
				 (lambda (name) (assq-ref nvl name)))))
		     ;; no export: internal to procedure wrappers
		     ;;(sfscm "(export ~A ~A)\n" w-name u-name))
		     ))))
    (sfscm "\n")
    (cond
     ((and typename enum-name)
      (sfscm ";; typedef enum ~A ~A;\n" enum-name typename))
     (typename (sfscm ";; typedef enum ~A;\n" typename))
     (enum-name (sfscm ";; enum ~A;\n" enum-name))
     (else (sfscm ";; anon enum\n")))
    (if typename (makeum typename))
    (if enum-name
	(if typename
	    (begin
	      (sfscm "(define wrap-enum-~A wrap-~A)\n" enum-name typename)
	      (sfscm "(define unwrap-enum-~A unwrap-~A)\n" enum-name typename))
	    (makeum (string-append "enum-" enum-name))))
    (unless (or #t typename enum-name) ;; anon enums in defines
      (for-each
       (lambda (pair) (sfscm "(define ~A ~A)\n" (car pair) (cdr pair)))
       name-val-l)
      (ppscm `(export ,@(map car name-val-l))))
    ))

;; --- pointers ???

;; === function declarations : signatures for pointer->procedure

(define ffi-typemap
  ;; see system/foreign.scm
  '(("void" . ffi:void)
    ("float" . ffi:float) ("double" . ffi:double)
    ("short" . ffi:short) ("short int" . ffi:short)
    ("unsigned short" . ffi:unsigned-short)
    ("unsigned short int" . ffi:unsigned-short)
    ("int" . ffi:int) ("unsigned" . ffi:unsigned-int)
    ("unsigned int" . ffi:unsigned-int) ("long" . ffi:long)
    ("long int" . ffi:long) ("unsigned long" . ffi:unsigned-long)
    ("unsigned long int" . ffi:unsigned-long) ("size_t" . ffi:size_t)
    ("ssize_t" . ffi:ssize_t) ("ptrdiff_t" . ffi:ptrdiff_t)
    ("int8_t" . int8) ("uint8_t" . ffi:uint8) 
    ("int16_t" . int16) ("uint16_t" . ffi:uint16) 
    ("int32_t" . int32) ("uint64_t" . ffi:uint64) 
    ))

(define ffi-keepers (map car ffi-typemap))

(define (mspec->ffi-sym mspec)
  (pmatch (cdr mspec)
    (((fixed-type ,name))
     (or (assoc-ref ffi-typemap name) (fherr "mspec->ffi-sym: ~A" name)))
    (((float-type ,name))
     (or (assoc-ref ffi-typemap name) (fherr "mspec->ffi-sym: ~A" name)))
    (((void)) 'ffi:void)
    (((pointer-to) . ,rest) ''*)
    (((enum-def . ,rest2) . ,rest1) 'ffi:int)
    (((typename ,name) . ,rest)
     (let* ((udecl `(decl (decl-spec-list (type-spec (typename ,name)))
			  (init-declr (ident "_"))))
	    (udecl (expand-typerefs udecl *uddict* #:keep ffi-keepers))
	    (mspec (udecl->mspec udecl)))
       (mspec->ffi-sym mspec)))
    (,otherwise (fherr "mspec->ffi-sym missed: ~S" mspec))))

;; Return a mspec for the return type.  The variable is called @code{NAME}.
(define (gen-decl-return udecl)
  (let* ((udecl1 (expand-typerefs udecl *uddict* #:keep ffi-keepers))
	 (mspec (udecl->mspec udecl1)))
    (mspec->ffi-sym mspec)))

(define (gen-decl-params params)
  ;; body
  (fold-right
   (lambda (param-decl seed)
     (cons (mspec->ffi-sym (udecl->mspec param-decl)) seed))
   '()
   params))


;; === function calls : unwrap args, call, wrap return

(define (mspec->fh-wrapper mspec)
  ;;(sfout "wrap this:\n") (ppout mspec)
  (pmatch (cdr mspec)
    (((fixed-type ,name)) (if (assoc-ref ffi-typemap name) #f
			      (fherr "todo: ffi-wrap fixed")))
    (((float-type ,name)) (if (assoc-ref ffi-typemap name) #f
			      (fherr "todo: ffi-wrap float")))
    (((void)) #f)
    (((enum-def . ,rest)) (string->symbol (string-append "wrap-" "xxx")))
    (((typename ,name)) (string->symbol (string-append "wrap-" name)))
    ;;
    (((pointer-to) (typename ,typename))
     (cond
      ((member typename ffi-keepers) #f)
      ((member typename *keepers*)
       (string->symbol (string-append "wrap-" typename "*")))
      (else #f)))
    ;;(((pointer-to) (struct ...
    ;;
    (,otherwise (fherr "mspec->ffi-wrapper missed: ~S" mspec))))

;; given mspec for an exec argument give the unwrapper
(define (mspec->fh-unwrapper mspec)
  (pmatch (cdr mspec)
    (((fixed-type ,name))
     (if (assoc-ref ffi-typemap name) #f (error "4: :( " name)))
    (((float-type ,name))
     (if (assoc-ref ffi-typemap name) #f (error "5: :( " name)))
    (((void)) #f)
    (((pointer-to) (typename ,typename))
     (cond
      ((member typename ffi-keepers) #f)
      ((member typename *keepers*)
       (string->symbol (string-append "unwrap-" typename "*")))
      (else #f)))
    ;;(((pointer-to) (struct-ref) ....)
    (((pointer-to) . ,rest) #f)
    (((typename ,name)) (string->symbol (string-append "unwrap-" name)))
    (,otherwise
     (fherr "mspec->fh-unwrapper missed: ~S" mspec))))

;; given list of udecl params generate list of name-unwrap pairs
(define (gen-exec-params params)
  (fold-right
   (lambda (param-decl seed)
     (let ((mspec (udecl->mspec param-decl)))
       (acons (car mspec) (mspec->fh-unwrapper mspec) seed)))
   '()
   params))

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
  (let* ((udecl (expand-typerefs udecl *uddict* #:keep *keepers*))
	 (mspec (udecl->mspec udecl)))
    (mspec->fh-wrapper mspec)))

;; @deffn {Procedure} cnvt-fctn name specl params
;; name is string
;; specl is decl-spec-list tree
;; params is list of param-decl trees (i.e., cdr of param-list tree)
;; @end deffn
(define (cnvt-fctn name rdecl params)
  (let* ((decl-return (gen-decl-return rdecl))
	 (decl-params (gen-decl-params params))
	 (exec-return (gen-exec-return-wrapper rdecl))
	 (exec-params (gen-exec-params params)))
    ;;(sfout "cnvt-fctn\n") (ppout params) (ppout decl-params) (ppout exec-params)
    (ppscm
     `(define ,(string->symbol name)
	(let ((f (ffi:pointer->procedure ,decl-return (lib-func ,name)
					 (list ,@decl-params))))
	  (lambda ,(gen-exec-arg-names exec-params)
	    (let ,(gen-exec-unwrappers exec-params)
	      ,(if exec-return
		   `(,exec-return (f ,@(gen-exec-call-args exec-params)))
		   `(f ,@(gen-exec-call-args exec-params))))))))
    (sfscm "(export ~A)\n" name)))

;; --- 

(define (fix-params param-decls)

  (define (remove-void-param params)
    (if (and (pair? params) (null? (cdr params))
	     (equal? (car params)
		     '(param-decl (decl-spec-list (type-spec (void))))))
	'() params))
  
  (define (fix-param param-decl ix)
    (sxml-match param-decl
      ((param-decl (decl-spec-list . ,specl))
       `(param-decl (decl-spec-list . ,specl)
		    (init-declr (ident ,(simple-format #f "arg-~A" ix)))))
      (,otherwise param-decl)))

  (let iter ((ix 0) (decls (remove-void-param param-decls)))
    (if (null? decls) '()
	(cons (fix-param (car decls) ix) (iter (1+ ix) (cdr decls))))))

;; intended to provide decl's for pointer-to or vector-of args
(define (get-needed-defns params keep-list)
  (sferr "get-needed-defns [NOT DONE]\n") (pperr params)
  '())

;; @deffn {Procedure} cnvt-udecl udecl udict keep-list
;; Given udecl produce a ffi-spec.
;; Return updated (string based) keep-list, which will be modified if the
;; declaration is a typedef.  The typelist is the set of keepers used for
;; @code{udecl->mspec}.
;; @end deffn
(define (cnvt-udecl udecl keep-list udict)
  (define (ptr-decl specl)
    `(udecl ,specl (init-declr (ptr-declr (pointer) (ident "_")))))
  (define (non-ptr-decl specl)
    `(udecl ,specl (init-declr (ident "_"))))

  (set! *keepers* keep-list)

  ;;(ppout udecl)
  (sxml-match udecl

    ;; typedef int foo_t;
    ((udecl
      (decl-spec-list
       (stor-spec (typedef))
       (type-spec (fixed-type ,name)))
      (init-declr (ident ,typename)))
     (let ()
       ;; don't use this
       ;;(sfscm "(define-std-type-wrapper ~A ~A)\n\n" typename name)
       (cons typename keep-list)))

    ;; typedef double foo_t;

    ;; typedef enum foo { ... } foo_t;
    ((udecl
      (decl-spec-list
       (stor-spec (typedef))
       (type-spec (enum-def (ident ,enum-name) ,enum-def-list . ,rest)))
      (init-declr (ident ,typename)))
     (cnvt-enum-def typename enum-name enum-def-list) 
     (cons typename keep-list))

    ;; typedef enum { ... } foo_t;
    ((udecl
      (decl-spec-list
       (stor-spec (typedef))
       (type-spec (enum-def ,enum-def-list . ,rest)))
      (init-declr (ident ,typename)))
     (cnvt-enum-def typename #f enum-def-list)
     (cons typename keep-list))

    ;; typedef struct foo { ... } foo_t;
    ((udecl
      (decl-spec-list
       (stor-spec (typedef))
       (type-spec (struct-def (ident ,struct-name) ,field-list)))
      (init-declr (ident ,typename)))
     (cnvt-struct-def typename struct-name field-list)
     (cons* typename (cons 'struct struct-name) keep-list))

    ;; typedef struct { ... } foo_t;
    ((udecl
      (decl-spec-list
       (stor-spec (typedef))
       (type-spec (struct-def ,field-list)))
      (init-declr (ident ,typename)))
     (cnvt-struct-def typename #f field-list)
     (cons* typename keep-list))

    ;; typedef struct foo foo_t;
    ((udecl
      (decl-spec-list
       (stor-spec (typedef))
       (type-spec (struct-ref (ident ,name))))
      (init-declr (ident ,typename)))
     (if (udict-struct-ref udict name) ;; struct is defined
	 (sfscm ";;(define ~A (delay struct-~A))\n" typename name)
	 (sfscm "(define-fh-pointer-type ~A*)\n" typename))
     (cons typename keep-list))

    ;; typedef union foo { ... } foo_t;
    ((udecl
      (decl-spec-list
       (stor-spec (typedef))
       (type-spec (union-def (ident ,union-name) ,field-list)))
      (init-declr (ident ,typename)))
     (cnvt-union-def typename union-name field-list)
     (cons* typename (cons 'union union-name) keep-list))

    ;; typedef union { ... } foo_t;
    ((udecl
      (decl-spec-list
       (stor-spec (typedef))
       (type-spec (union-def ,field-list)))
      (init-declr (ident ,typename)))
     (cnvt-struct-def typename #f field-list)
     (cons* typename keep-list))

    ;; typedef union foo foo_t;
    ((udecl
      (decl-spec-list
       (stor-spec (typedef))
       (type-spec (union-ref (ident ,name))))
      (init-declr (ident ,typename)))
     (if (udict-union-ref udict name) ;; union is defined
	 (sfscm ";;(define ~A (delay union-~A))\n" typename name)
	 (sfscm "(define-fh-pointer-type ~A*)\n" typename))
     (cons typename keep-list))

    ;; Need to capture declarators like
    ;;   extern struct { int x; } *foo;
    
    ;; struct foo { ... }.
    ((udecl
      (decl-spec-list
       (type-spec (struct-def (ident ,struct-name) ,field-list))))
     (if (assoc-ref `(struct . ,struct-name) keep-list)
	 keep-list
	 (begin
	   (cnvt-struct-def #f struct-name field-list)
	   (cons `(struct . ,struct-name) keep-list))))

    ;; struct { ... } ...
    ((udecl
      (decl-spec-list
       (type-spec (struct-def ,field-list))))
     (sferr "bug in util2? unnamed struct-def\n")
     (pperr udecl)
     keep-list)

    ;; union foo { ... }.
    ((udecl
      (decl-spec-list
       (type-spec (union-def (ident ,union-name) ,field-list))))
     (if (assoc-ref `(struct . ,union-name) keep-list)
	 keep-list
	 (begin
	   (cnvt-union-def #f union-name field-list)
	   (cons `(union . ,union-name) keep-list))))

    ;; union { ... } ...
    ((udecl
      (decl-spec-list
       (type-spec (union-def ,field-list))))
     (sferr "bug in util2? unnamed union-def\n")
     (pperr udecl)
     keep-list)


    ;; function typedef
    ((udecl
      (decl-spec-list (stor-spec (typedef)) . ,rst)
      (init-declr
       (ftn-declr (scope (ptr-declr (pointer) (ident ,typename)))
		  (param-list . ,params))))
     (let* ((ret-decl `(udecl (decl-spec-list . ,rst) (init-declr (ident "_"))))
	    (decl-return (gen-decl-return ret-decl))
	    (decl-params (gen-decl-params params)))
       (sfscm "(define (wrap-~A proc) ;; => pointer\n" typename)
       (ppscm
	`(ffi:procedure->pointer ,decl-return proc (list ,@decl-params))
	#:per-line-prefix " ")
       (sfscm " )\n")
       (sfscm "(export wrap-~A)\n" typename))
     (cons typename keep-list))
    
    ;; enum foo { ... };
    ((udecl
      (decl-spec-list
       (type-spec (enum-def (ident ,enum-name) ,enum-def-list . ,rest))))
     (cnvt-enum-def #f enum-name enum-def-list)
     ;; probably never use this as arg to function
     keep-list)
    
    ;; enum { ... };
    ((udecl
      (decl-spec-list
       (type-spec (enum-def ,enum-def-list . ,rest))))
     (cnvt-enum-def #f #f enum-def-list)
     keep-list)
    
    ;; function returning pointer value
    ((udecl ,specl
	    (init-declr
	     (ptr-declr
	      (pointer) (ftn-declr (ident ,name) (param-list . ,params)))))
     ;;(sfscm "\n;; ~A\n" name)
     (cnvt-fctn name (ptr-decl specl) (fix-params params))
     keep-list)

    ;; function returning non-pointer value
    ((udecl ,specl
	    (init-declr
	     (ftn-declr (ident ,name) (param-list . ,params))))
     (when #f ;; specifier and declarator on separate lines
       (c99scm specl)
       (sfscm "\n")
       (c99scm (caddr udecl))
       (sfscm "\n"))
     (cnvt-fctn name (non-ptr-decl specl) (fix-params params))
     keep-list)

    (,otherwise
     (sferr "cnvt-udecl missed:\n") (pperr udecl)
     (fherr "cnvt-udecl missed")
     keep-list)))

;; === enums and #defined => lookup

;; given keeper-defs (k-defs) and all defs (a-defs) expand the keeper
;; replacemnts down to constants (strings, integers, etc)
(define (gen-lookup-proc prefix k-defs a-defs)
  (sfscm "\n;; access to enum symbols and #define'd constants:\n")
  (let ((name (string->symbol (string-append prefix "lookup")))
	(defs (fold-right
	       (lambda (def seed)
		 (let* ((name (car def))
			(repl (if (pair? (cdr def)) ""
				  (expand-cpp-macro-ref name a-defs))))
		   (cond
		    ((zero? (string-length repl)) seed)
		    ((string->number repl) =>
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
	       k-defs)))
    (ppscm `(define ,name
	      (let ((symtab '(,@defs)))
		(lambda (k) (assq-ref symtab k)))))
    (sfscm "(export ~A)\n" name)))

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
    (lambda (attrs) ;;(cpp-defs inc-dirs inc-files helpers)
      ;;(ppout attrs)
      (let* ((pkg-config (assq-ref attrs 'pkg-config))
	     (cpp-defs (resolve-attr-val (assq-ref attrs 'cpp-defs)))
	     (inc-dirs (resolve-attr-val (assq-ref attrs 'inc-dirs)))
	     (inc-help (resolve-attr-val (assq-ref attrs 'inc-help)))
	     (inc-files (resolve-attr-val (assq-ref attrs 'include)))

	     (cpp-defs (append
			(if pkg-config (pkg-config-defs pkg-config) '())
			cpp-defs))
	     (inc-dirs (append
			(if pkg-config (pkg-config-incs pkg-config) '())
			inc-dirs))
	     (inc-help (append inc-help std-inc-help))
	     
	     (inc-dirs (append inc-dirs std-inc-dirs))
	     
	     (cpp-defs (append cpp-defs (gen-gcc-defs)))
	     (prog (string-join
		    (map
		     (lambda (inc-file)
		       (string-append "#include \"" inc-file "\"\n"))
		     inc-files) "")))
	(with-input-from-string prog
	  (lambda ()
	    (and=> 
	     (parse-c99 #:cpp-defs cpp-defs
			#:inc-dirs inc-dirs
			#:inc-help inc-help
			#:mode 'decl #:debug *debug*)
	     merge-inc-bodies)))))))

;; === main converter ================

;; process define-ffi-module expression
(define (intro-ffi path opts)
  (let* ((attrs (opts->attrs opts))
	 (dpath (string-join (map symbol->string path) "/"))
	 (dport (open-output-file (string-append dpath ".scm")))
	 (incf (or (assq-ref attrs 'inc-filter) #f))
	 (declf (or (assq-ref attrs 'decl-filter) identity))
	 (renamer (or (assq-ref attrs 'renamer) identity))
	 (prefix (string-append (symbol->string (last path)) "-"))
	 ;;
	 (tree (parse-includes (opts->attrs attrs)))
	 (udecls (c99-trans-unit->udict tree #:inc-filter incf))
	 (udict (c99-trans-unit->udict/deep tree))
	 ;;
	 (enu-defs (udict-enums->ddict udict))
	 (ffi-defs (c99-trans-unit->ddict tree enu-defs #:inc-filter incf))
	 (all-defs (c99-trans-unit->ddict tree enu-defs #:inc-filter #t))
	 )
    ;;(ppout incf) (quit)
    ;; set globals
    (set! *uddict* udict)
    (set! *port* dport)
    ;; renamer?

    ;; file and module header
    (ffimod-header path opts)
    
    ;; convert and output foreign declarations
    (fold
     (lambda (pair keep-list)
       (catch 'ffi-help-error
	 (lambda ()
	   (cond
	    ((declf (car pair))
	     (sferr "~S\n" (car pair))
	     (nlscm) (c99scm (cdr pair)) ;;  <= fix to turn xxx-def to xxx-ref
	     (cnvt-udecl (cdr pair) keep-list udict))
	    (else
	     keep-list)))
	 (lambda (key fmt . args)
	   (apply simple-format (current-error-port)
		  (string-append "ffi-help: " fmt "\n") args)
	   (sfscm ";; ... failed.\n")
	   keep-list)))
     fixed-width-int-names udecls)

    ;; output global constants (from enum and #define)
    ;;(sfscm "\n;; PLEASE un-comment gen-lookup-proc\n")
    (gen-lookup-proc prefix ffi-defs all-defs)

    ;; return port so compiler can output remaining code
    dport))

;; This macro converts #:key val to '(key val) for ffi-help options
;; and preserves other #:key-val pairs for passthrough to the module
(define-syntax fix-option
  (lambda (x)
    (define (sym->key stx)
      (datum->syntax stx (symbol->keyword (syntax->datum stx))))
    (syntax-case x (cpp-defs decl-filter inc-filter inc-help include library
			     pkg-config renamer)
      ((_ cpp-defs proc) #'(cons 'cpp-defs proc))
      ((_ decl-filter proc) #'(cons 'decl-filter proc))
      ((_ inc-filter proc) #'(cons 'inc-filter proc))
      ((_ inc-help expr) #'(cons 'inc-help expr))
      ((_ include expr) #'(cons 'include expr))
      ((_ library expr) #'(cons 'library expr)) ;; eval to list of libs
      ((_ pkg-config string) #'(cons 'pkg-config string))
      ((_ renamer proc) #'(cons 'renamer proc))
      ;; the rest gets passed to the module decl
      ((_ key arg) #`(cons #,(sym->key #'key) (quote arg)))
      ;; old module-like syntax
      ;;((_ inc-help (helper ...)) #'(cons 'inc-help (quote (helper ...))))
      ;;((_ include (string ...)) #'(list 'include string ...))
      ;;((_ library (string ...)) #'(list 'library string ...))
      )))

(define-syntax module-options
  (lambda (x)
    (define (key->sym stx)
      (datum->syntax x (keyword->symbol (syntax->datum stx))))

    (syntax-case x ()
      ((_ key val option ...)
       (keyword? (syntax->datum #'key))
       #`(cons
	  (fix-option #,(key->sym #'key) val)
	  (module-options option ...)))
      
      ;; ??? uncommenting generates syntax error but above fendor is passing
      ;;((_ key val option ...) (syntax-error "ffi: illegal keyword"))
      
      ((_) #''()))))

(define-syntax-rule (define-ffi-module path-list attr ...)
  (intro-ffi (quote path-list) (module-options attr ...)))


;; === file compiler ================

(use-modules (system base language))
(use-modules (ice-9 pretty-print))

(define (string-member-proc . args)
  (lambda (s) (member s args)))

;; to convert symbol-based #:renamer to string-based
(define (string-renamer proc)
  (lambda (s) (string->symbol (proc (symbol->string s)))))

(define scm-reader (language-reader (lookup-language 'scheme)))

(define (compile-ffi-file file)
  (sfout "make compile-ffi pass args to intro-ffi (e.g., path to gcc)\n")
  (call-with-input-file file
    (lambda (iport)
      (let iter ((oport #f))
	(let ((exp (scm-reader iport (current-module))))
	  ;;(display "exp:\n") (pretty-print exp)
	  (cond
	   ((eof-object? exp)
	    (when oport
	      (display "\n;; --- last line ---\n" oport)
	      (close oport)))
	   ((and (pair? exp) (eqv? 'define-ffi-module (car exp)))
	    (iter (eval exp (current-module))))
	   (else
	    (when oport
	      (newline oport)
	      (pretty-print exp oport)
	      (iter oport)))))))))

;; --- last line ---
