;;; example/nyacc/lang/c99/ffi-help.scm
;;;
;;; Copyright (C) 2016-2017 Matthew R. Wette
;;;
;;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;;; or any later version published by the Free Software Foundation.  See
;;; the file COPYING included with the nyacc distribution.

;; WARNING: this is a prototype in development: anything goes right now

;; User is responsible for calling string->pointer and pointer->string.
;;
;; By definition: wrap is c->scm; unwrap is scm->c

;; @table code
;; @item mspec->ffi-wrapper
;; generates code to apply wrapper to objects returned from foreign call
;; @item mspec->ffi-unwrapper
;; generated code to apply un-wrapper to arguments for foreign call
;; @end table

;; TODO
;; 02 if need foo_t pointer then I gen wrapper for foo_t* but add
;;    foo_t to *wrappers* so if I later run into need for foo_t may be prob
;; 03 allow user to specify #:renamer (lambda (n) "make_goo" => "make-goo")
;; DONE
;; 01 enum-wrap 0 => 'CAIRO_STATUS_SUCCESS
;;    enum-unwrap 'CAIRO_STATUS_SUCCESS => 0

(add-to-load-path (string-append (getcwd) "/../../../../module"))

(define-module (ffi-help)
  #:export-syntax (define-std-pointer-wrapper define-ffi-helper
		    debug-std-pointer-wrapper)
  #:export (*ffi-help-version*
	    compile-ffi-file
	    intro-ffi
	    unwrap-char*
	    bs-renamer ffi-renamer
	    pkg-config-incs
	    )
  #:use-module (nyacc lang c99 parser)
  #:use-module (nyacc lang c99 util1)
  #:use-module (nyacc lang c99 util2)
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang c99 pprint)
  #:use-module (system foreign)
  ;;#:use-module (bytestructures guile)
  #:use-module (ice-9 format)
  #:version (0 1 0))

#|
(define (ffi-renamer s)
  (let ((n (symbol->string s)))
    (if (string=? "ffi" (substring n 0 3)) s
	(string->symbol (string-append "ffi:" n)))))

(define (bs-renamer s)
  (let ((n (symbol->string s)))
    (if (string=? "bs:" (substring n 0 3)) s
	(string->symbol (string-append "bs:" n)))))
|#

(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 xparser))
(use-modules (nyacc lang c99 util1))
(use-modules (nyacc lang c99 util2))
(use-modules (nyacc lang c99 pprint))
(use-modules (nyacc lang util))
(use-modules (sxml fold))
(use-modules (sxml match))
(use-modules ((sxml xpath)
	      #:renamer (lambda (s) (if (eq? s 'filter) 'node-filter s))))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))
(use-modules (srfi srfi-37))
(use-modules (ice-9 regex))
;;(use-modules (ice-9 match))
(use-modules (system base pmatch))
(use-modules (ice-9 pretty-print))

(define *ffi-help-version* "0.01.0")

(define std-inc-dirs
  `("/usr/include"
    ;;,(assq-ref %guile-build-info 'includedir)
    ))

(define *port* #t)
(define *uddict* '())
(define *ffi-keepers*
  (append fixed-width-int-names
	  '("float" "double" "int" "unsigned int" "long" "unsigned long"
	    "size_t" "ssize_t" "ptrdiff_t" "void")))
(define *keepers* *ffi-keepers*)
(define *wrapped* '()) ;; list of strings, with appended "*" are wrapped

(define (sfout fmt . args)
  (apply simple-format #t fmt args))
(define (sfscm fmt . args)
  (apply simple-format *port* fmt args))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define* (ppscm tree #:key (per-line-prefix ""))
  (pretty-print tree *port* #:per-line-prefix per-line-prefix))
(define (ppout tree)
  (pretty-print tree #:per-line-prefix "    "))
(define (newln)
  (newline *port*))

(define (fherr fmt . args)
  (apply throw 'ffi-help-error fmt args))

(define (stx->str x)
  (symbol->string (syntax->datum x)))

(define (gen-id tmpl-id . args)
  (datum->syntax
   tmpl-id (string->symbol
	    (apply string-append
		   (map (lambda (x) (if (string? x) x (stx->str x)))
			args)))))

;;(define-syntax define-enum-wrapper
;;  (lambda (x)
;;    (

(define-syntax define-std-type-wrapper
  (lambda (x)
    #'(define (stx->str x) (symbol->string (syntax->datum x)))
    #'(define (gen-id tmpl-id . args)
      (datum->syntax
       tmpl-id (string->symbol
		(apply string-append
		       (map (lambda (x) (if (string? x) x (stx->str x)))
			    args)))))
    (syntax-case x ()
      ((_ name type)
       #`(begin
	  (define #'name #'type)
	  (define #,(gen-id #'name "wrap-" #'name) identity)
	  (define #,(gen-id #'name "unwrap-" #'name) identity)
	  (export #'name)
	  (export #,(gen-id #'name "wrap-" #'name))
	  (export #,(gen-id #'name "unwrap-" #'name)))))))

(define-syntax define-std-pointer-wrapper
  (lambda (x)
    (define (stx->str x) (symbol->string (syntax->datum x)))
    (define (gen-id tmpl-id . args)
      (datum->syntax
       tmpl-id (string->symbol
		(apply string-append
		       (map (lambda (x) (if (string? x) x (stx->str x)))
			    args)))))
    (syntax-case x ()
      ((_ name)
       (let ((pred (gen-id #'name #'name "?"))
	     (wrap (gen-id #'name "wrap-" #'name))
	     (unwr (gen-id #'name "unwrap-" #'name)))
	 #`(begin
	     (define-wrapped-pointer-type name #,pred #,wrap #,unwr
	       (lambda (v p)
		 ((@@ (ice-9 format) format) p
		  #,(string-append "<" (stx->str #'name) " ~x>")
		  (pointer-address (#,unwr v)))))
	     (export #,pred) (export #,wrap) (export #,unwr)))))))

(define-std-pointer-wrapper double)

(define (unwrap-char* value)
  (if (string? value)
      (string->pointer value)
      value))

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
    (lambda (cpp-defs inc-dirs inc-files)
      (let* ((all-defs (append cpp-defs (gen-gcc-defs)))
	     (prog (string-join
		    (map
		     (lambda (inc-file)
		       (string-append "#include \"" inc-file "\"\n"))
		     inc-files))))
	;;(sfout "prog:\n~A\n" prog)
	(with-input-from-string prog
	  (lambda ()
	    (and=> 
	     (parse-c99 #:cpp-defs all-defs
			#:inc-dirs inc-dirs
			#:mode 'decl #:debug #f)
	     merge-inc-bodies)))))))


(define (fold-enum-typenames dict seed)
  (fold
   (lambda (pair seed)
     (sxml-match (cdr pair)
       ((decl (decl-spec-list
	       (stor-spec (typedef))
	       (type-spec (enum-def . ,rest)))
	      (init-declr (ident ,name)))
	(cons name seed))
       (,otherwise
	seed)))
   '()
   dict))


;; --- type handling 

;; missing char short etc

(define (mtype->bs mspec-tail)
  (pmatch mspec-tail
    (((fixed-type ,name)) (string-append "bs:" name))
    (((float-type ,name)) (string-append "bs:" name))
    (((void)) "bs:void")
    (((pointer-to) (fixed-type ,name)) (string-append "bs:" name "*"))
    (((pointer-to) (float-type ,name)) (string-append "bs:" name "*"))
    (((pointer-to) (void)) "bs:void*")
    ;;
    (((typename ,name)) name)
    (((pointer-to) (typename ,name)) (string-append name "*"))
    ;;
    (,otherwise (error "1: missed" mspec-tail))))

(define (mtype->ffi mspec-tail)
  (pmatch mspec-tail
    (((fixed-type ,name)) (string-append "ffi:" name))
    (((float-type ,name)) (string-append "ffi:" name))
    (((void)) "ffi:void")
    (((pointer-to) (fixed-type ,name)) (string-append "ffi:" name "*"))
    (((pointer-to) (float-type ,name)) (string-append "ffi:" name "*"))
    (((pointer-to) (void)) "ffi:void*")
    ;;
    (((typename ,name)) name)
    (((pointer-to) (typename ,name)) (string-append name "*"))
    ;;
    (,otherwise (error "2: missed" mspec-tail))))

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

(define (mspec->ffi-sym mspec)
  (pmatch (cdr mspec)
    (((fixed-type ,name))
     (or (assoc-ref ffi-typemap name) (fherr "mspec->ffi-sym: ~A" name)))
    (((float-type ,name))
     (or (assoc-ref ffi-typemap name) (fherr "mspec->ffi-sym: ~A" name)))
    (((void)) 'ffi:void)
    (((pointer-to) . ,rest) ''*)
    (((enum-def . ,rest2) . ,rest1) "ffi:int")
    (((typename ,name) . ,rest)
     (let* ((udecl `(decl (decl-spec-list (type-spec (typename ,name)))
			  (init-declr (ident "_"))))
	    (udecl (expand-typerefs udecl *uddict* #:keep *ffi-keepers*))
	    (mspec (udecl->mspec udecl)))
       (mspec->ffi-sym mspec)))
    (,otherwise (fherr "mspec->ffi-sym missed: ~S" mspec))))

(define (mspec->ffi-wrapper mspec)
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
     (if (member typename *wrapped*)
	 (string->symbol (string-append "wrap-" typename "*"))
	 #f))
    (((pointer-to) . ,rest) 'identity)
    ;;
    (,otherwise (fherr "mspec->ffi-wrapper missed: ~S" mspec))))

(define (mspec->ffi-unwrapper mspec)
  ;;(sfout "cdr mspec = ~S\n" (cdr mspec))
  (pmatch (cdr mspec)
    (((fixed-type ,name))
     (if (assoc-ref ffi-typemap name) #f (error ":( " name)))
    (((float-type ,name))
     (if (assoc-ref ffi-typemap name) #f (error ":( " name)))
    (((pointer-to) (typename ,typename))
     (if #t ;;(member type *wrapped*)
	 (string->symbol (string-append "unwrap-" typename "*"))
	 #f))
    (((void)) #f)
    (((pointer-to) (typename ,typename))
     (if (member typename *wrapped*)
	 (string->symbol (string-append "unwrap-" typename "*"))
	 #f))
    (((pointer-to) . ,rest) 'identity)	; HACK
    (((typename ,name)) (string->symbol (string-append "unwrap-" name)))
    (,otherwise
     (fherr "mspec->ffi-unwrapper missed: ~S" mspec))))

;; --- structures 

(define (cnvt-field field)
  (let ((mspec (udecl->mspec field)))
    #f))

(define (acons-defn name type seed)
  (cons (eval-string (string-append "(quote `(" name " ," type "))")) seed))

;; cairo_matrix_t
(define (cnvt-struct-def typename struct-name field-list)
  (let* ((fldl (clean-field-list field-list)) ; remove lone comments
	 (flds (cdr fldl))
	 (uflds (fold munge-comp-decl '() flds)) ; reverse order
	 (sflds					 ; bs fields in order
	  (let iter ((sflds '()) (decls uflds))
	    (if (null? decls) sflds
		(let* ((name (caar decls))
		       (udecl (cdar decls))
		       (udecl (expand-typerefs udecl *uddict* #:keep *keepers*))
		       (spec (udecl->mspec/comm udecl))
		       (type (mtype->bs (cddr spec))))
		  (iter (acons-defn name type sflds) (cdr decls)))))))
    (sfscm "\n;; ~A\n" typename)
    (ppscm `(define ,(string->symbol typename) (bs:struct (list ,@sflds))))
    (sfscm "(export ~A)\n" typename)
    (newln)
    #t))

;; --- enums

;; MAY WANT TO define all enums!!
(define (cnvt-enum-def typename enum-name enum-def-list)
  (let* ((pname (if typename typename enum-name))
	 (sname (if typename enum-name #f))
	 (name-val-l (map
		      (lambda (def)
			(pmatch def
			  ((enum-defn (ident ,n) (p-expr (fixed ,v)))
			   (cons (string->symbol n) (string->number v)))
			  ((enum-defn (ident ,n) (neg (p-expr (fixed ,v))))
			   (cons (string->symbol n) (- (string->number v))))
			  (,otherwise (error "cnvt-enum-def coding" def))))
		      (cdr (canize-enum-def-list enum-def-list))))
	 (val-name-l (map (lambda (p) (cons (cdr p) (car p))) name-val-l))
	 (w-pname (string->symbol (string-append "wrap-" pname)))
	 (u-pname (string->symbol (string-append "unwrap-" pname))))
    (sfscm "\n")
    (cond
     (typename
      (if enum-name
	  (sfscm ";; typedef enum ~A ~A;\n" enum-name typename)
	  (sfscm ";; typedef enum ~A;\n" typename)))
     (enum-name
      (sfscm ";; enum ~A;\n" enum-name))
     (else
      (sfscm ";; anon enum;\n")))
    (cond
     ((or typename enum-name)
      (ppscm `(define ,w-pname
		(let ((vnl '(,@val-name-l)))
		  (lambda (code) (assq-ref vnl code)))))
      (ppscm `(define ,u-pname
		(let ((nvl '(,@name-val-l)))
		  (lambda (name) (assq-ref nvl name)))))
      (sfscm "(export ~A ~A)\n" w-pname u-pname)))
    (if #f ;; sname
	(sfscm "(define wrap-~A wrap-~A)\n(export ~A)\n" sname pname sname))
    ;; define all enums
    (for-each
     (lambda (pair)
       (sfscm "(define ~A ~A)\n" (car pair) (cdr pair)))
     name-val-l)
    (ppscm `(export ,@(map car name-val-l)))
    #f))

;; --- function

;; Return a mspec for the return type.  The variable is called @code{NAME}.
(define (gen-decl-return udecl)
  (let* ((udecl1 (expand-typerefs udecl *uddict* #:keep *ffi-keepers*))
	 (mspec (udecl->mspec udecl1)))
    (mspec->ffi-sym mspec)))

(define (gen-decl-params params)
  ;; body
  (fold-right
   (lambda (param-decl seed)
     (cons (mspec->ffi-sym (udecl->mspec param-decl)) seed))
   '()
   params))

;; given list of udecl params generate list of name-unwrap pairs
(define (gen-exec-params params)
  (fold-right
   (lambda (param-decl seed)
     (let ((mspec (udecl->mspec param-decl)))
       (acons (car mspec) (mspec->ffi-unwrapper mspec) seed)))
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

(define (gen-exec-call-args params)
  (fold-right
   (lambda (name-unwrap seed)
     (let ((name (car name-unwrap))
	   (unwrap (cdr name-unwrap)))
       (cons (string->symbol (if unwrap (string-append "~" name) name)) seed)))
   '()
   params))

(define (gen-exec-return-wrapper udecl)
  ;;(sfout "wrapped=~S\n" *wrapped*)
  (let* ((udecl (expand-typerefs udecl *uddict* #:keep *wrapped*))
	 (mspec (udecl->mspec udecl)))
    (mspec->ffi-wrapper mspec)))

;; @deffn {Procedure} make-fctn name specl params
;; name is string
;; specl is decl-spec-list tree
;; params is list of param-decl trees (i.e., cdr of param-list tree)
;; @end deffn
(define (make-fctn name rdecl params)
  (let* ((decl-return (gen-decl-return rdecl))
	 (decl-params (gen-decl-params params))
	 (wrap-return (gen-exec-return-wrapper rdecl))
	 (exec-params (gen-exec-params params)))
    ;;(sfout "make-fctn\n  ~S\n  ~S\n" params decl-params)
    (ppscm
     `(define ,(string->symbol name)
	(let ((f (ffi:pointer->procedure ,decl-return (lib-func ,name)
					 (list ,@decl-params))))
	  (lambda ,(gen-exec-arg-names exec-params)
	    (let ,(gen-exec-unwrappers exec-params)
	      ,(if wrap-return
		   `(,wrap-return (f ,@(gen-exec-call-args exec-params)))
		   `(f ,@(gen-exec-call-args exec-params))))))))
    (sfscm "(export ~A)\n" name)))

;; --- 

;; for eval (vs decl)
(define (xxx-param-arg-type typel)
  ;;(simple-format #t "do-param-arg-type ~S\n" typel)
  (pmatch typel
    (((fixed-type ,name)) name)
    (((float-type ,name)) name)
    (((typename ,name)) name)
    (((pointer-to) (fixed-type ,name)) (string-append name "*"))
    (((pointer-to) (float-type ,name)) (string-append name "*"))
    (((pointer-to) (typename ,name)) (string-append name "*"))
    (((pointer-to) (void)) "void*")
    (,otherwise (sferr "OTHERWISE=~S\n" typel))
    ))

(define (fix-param param-decl ix)
  (sxml-match param-decl
    ((param-decl (decl-spec-list . ,specl))
     `(param-decl (decl-spec-list . ,specl)
		  (init-declr (ident ,(simple-format #f "arg-~A" ix)))))
    (,otherwise param-decl)))

(define (fix-params param-decls)
  (define (remove-void-param params)
    (if (and (pair? params) (null? (cdr params))
	     (equal? (car params)
		     '(param-decl (decl-spec-list (type-spec (void))))))
	'() params))
  
  (let iter ((ix 0) (decls (remove-void-param param-decls)))
    (if (null? decls) '()
	(cons (fix-param (car decls) ix) (iter (1+ ix) (cdr decls))))))

;; Convert a udecl to a ffi-spec
;; Return updated (string based) type-list, which will be modified if the
;; declaration is a typedef.  The typelist is the set of keepers used for
;; @code{udecl->mspec}.
(define (udecl->ffi-decl udecl type-list)
  (define (ptr-decl specl)
    `(udecl ,specl (init-declr (ptr-declr (pointer) (ident "_")))))
  (define (non-ptr-decl specl)
    `(udecl ,specl (init-declr (ident "_"))))
  
  (set! *keepers* type-list)
  
  (sxml-match udecl

    ;; anonymous struct typedef: "typedef struct foo foo_t;" => foo_t*
    ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-ref (ident ,name))))
	(init-declr (ident ,typename)))
     (let ((p-typename (string-append typename "*")))
       (sfscm "\n")
       (pretty-print-c99 udecl *port* #:per-line-prefix ";; ")
       (sfscm "(define-std-pointer-wrapper ~A)\n" p-typename)
       (set! *wrapped* (cons typename *wrapped*))
       (cons typename type-list)))

    ;; named struct-def typedef
    ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-def (ident ,struct-name) ,field-list)))
	(init-declr (ident ,typename)))
     (let ((p-typename (string-append typename "*")))
       (cnvt-struct-def typename struct-name field-list)
       (sfscm "(define-std-pointer-wrapper ~A)\n" p-typename)
       (set! *wrapped* (cons p-typename *wrapped*))
       (cons typename type-list)))

    ;; ENUMs are special because the guts should have global visibility
    ;; enum-def typedef
    ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (enum-def (ident ,enum-name) ,enum-def-list . ,rest)))
	(init-declr (ident ,typename)))
     (cnvt-enum-def typename enum-name enum-def-list)
     (set! *wrapped* (cons typename *wrapped*))
     (cons typename type-list))
    ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (enum-def ,enum-def-list . ,rest)))
	(init-declr (ident ,typename)))
     (cnvt-enum-def typename #f enum-def-list)
     (set! *wrapped* (cons typename *wrapped*))
     (cons typename type-list))
    ((udecl
	(decl-spec-list
	 (type-spec (enum-def (ident ,enum-name) ,enum-def-list . ,rest))))
     (cnvt-enum-def #f enum-name enum-def-list)
     ;; probably never use this as arg to function
     ;;(set! *wrapped* (cons (cons 'enum enum-name) *wrapped*))
     type-list)
    ;; anonymous enum
    ((udecl
	(decl-spec-list
	 (type-spec (enum-def ,enum-def-list . ,rest))))
     (cnvt-enum-def #f #f enum-def-list)
     type-list)
       
    ;; fixed typedef 
    ((udecl
      (decl-spec-list
       (stor-spec (typedef))
       (type-spec (fixed-type ,name)))
      (init-declr (ident ,typename)))
     (let ()
       ;; don't use this
       ;;(sfscm "(define-std-type-wrapper ~A ~A)\n\n" typename name)
       (cons typename type-list)))

    ;; float typedef

    ;; function typedef
    ((udecl
      (decl-spec-list (stor-spec (typedef)) . ,rst)
      (init-declr
       (ftn-declr (scope (ptr-declr (pointer) (ident ,typename)))
		  (param-list . ,params))))
     (let* ((ret-decl `(udecl (decl-spec-list . ,rst) (init-declr (ident "_"))))
	    (decl-return (gen-decl-return ret-decl))
	    (decl-params (gen-decl-params params)))
       (sfscm "\n")
       (pretty-print-c99 udecl *port* #:per-line-prefix ";; ")
       (sfscm "(define (wrap-~A proc) ;; => pointer\n" typename)
       (ppscm
	`(ffi:procedure->pointer ,decl-return proc (list ,@decl-params))
	#:per-line-prefix " ")
       (sfscm " )\n")
       (sfscm "(export wrap-~A)\n" typename))
     (set! *wrapped* (cons typename *wrapped*))
     )
    
    ;; function returning pointer value
    ((udecl ,specl
	    (init-declr
	     (ptr-declr
	      (pointer) (ftn-declr (ident ,name) (param-list . ,params)))))
     ;;(sfscm "\n;; ~A\n" name)
     (sfscm "\n")
     (pretty-print-c99 udecl *port* #:per-line-prefix ";; ")
     (make-fctn name (ptr-decl specl) (fix-params params))
     type-list)

    ;; function returning non-pointer value
    ((udecl ,specl
	    (init-declr
	     (ftn-declr (ident ,name) (param-list . ,params))))
     (sfscm "\n")
     (pretty-print-c99 udecl *port* #:per-line-prefix ";; ")
     (when #f ;; specifier and declarator on separate lines
       (pretty-print-c99 specl *port* #:per-line-prefix ";; ")
       (sfscm "\n")
       (pretty-print-c99 (caddr udecl) *port* #:per-line-prefix ";; ")
       (sfscm "\n"))
     (make-fctn name (non-ptr-decl specl) (fix-params params))
     type-list)

    (,otherwise
     (ppout udecl)
     (fherr "udecl->ffi-decl missed")
     type-list)))

;; (sizeof '*) works

(use-modules (ice-9 popen))
(use-modules (ice-9 rdelim))

;; use pkg-config to get a list of include dirs
;; (pkg-config-incs "cairo") => ("/opt/local/include/cairo" ...)
(define (pkg-config-incs name)
  (let* ((port (open-input-pipe (string-append "pkg-config --cflags " name)))
	 (ostr (read-line port))
	 (incl (string-split ostr #\space))
	 )
    (close-port port)
    ;;(simple-format #t "~S\n" (map (lambda (s) (substring/shared s 2)) incl))
    (map (lambda (s) (substring/shared s 2)) incl)))

;; --- defs
(define rx1 (make-regexp "(.*[^ \t])[ \t]*/\\*.*\\*/ *$"))

(define (scrub-repl repl)
  (cond
   ((regexp-exec rx1 repl) =>
    (lambda (m) (match:substring m 1)))
   (else
    repl)))

(define (tree->defs tree)
  (fold
   (lambda (def dict)
     (sfout "def=~S\n" def)
     (sxml-match def
       ((define (name ,name) (repl ,repl))
	(acons name (scrub-repl repl) dict))
       (,otherwise dict)))
   '()
   ;;((sxpath '(cpp-stmt define (repl))) tree)))
   ((sxpath '(cpp-stmt define)) tree)))

(define trans-unit->defs
  (let ()
    (lambda (tree)
      '())))

;; sxml tree to xxx
;; (define (name "MAX") (args "X" "Y") (repl "stuff")) =>
;;     ("MAX" ("X" "Y") . "stuff")
(define (can-def-stmt defn)
  (let* ((name (car (assq-ref defn 'name)))
	 (args (assq-ref defn 'args))
	 (repl (car (assq-ref defn 'repl))))
    (cons name (if args (cons args repl) repl))))

;; tree => (("ABC" . "repl") ("MAX" ("X" "Y") . "(X)...") ...)
;; deep search
(define (trans-unit-defs/deep tree)
  (define (def? tree)
    (if (and (eq? 'cpp-stmt (sx-tag tree))
	     (eq? 'define (sx-tag (sx-ref tree 1))))
	(can-def-stmt (sx-ref tree 1))
	#f))
  (define (inc? tree)
    (if (and (eq? 'cpp-stmt (sx-tag tree))
	     (eq? 'include (sx-tag (sx-ref tree 1)))
	     (pair? (sx-ref (sx-ref tree 1) 2)))
	(sx-ref (sx-ref tree 1) 2)
	#f))
  (let iter ((defs '()) (elts (cdr tree)))
    (cond
     ((null? elts) defs)
     ((def? (car elts)) => (lambda (d) (iter (cons d defs) (cdr elts))))
     ((inc? (car elts)) => (lambda (t) (iter (iter defs (cdr t)) (cdr elts))))
     (else (iter defs (cdr elts))))))

;; just one level down

(define* (c99-trans-unit->ddict tree #:optional (seed '()) #:key inc-filter)
  (define (def? tree)
    (if (and (eq? 'cpp-stmt (sx-tag tree))
	     (eq? 'define (sx-tag (sx-ref tree 1))))
	(can-def-stmt (sx-ref tree 1))
	#f))
  (if (pair? tree)
      (fold-right
       (lambda (tree seed)
	 (cond
	  ((def? tree) =>
	   (lambda (def-stmt)
	     ;;(sfout "def-stmt=~S\n" def-stmt)
	     (cons def-stmt seed)))
	  ((inc-keeper? tree inc-filter) =>
	   (lambda (tree)
	     (c99-trans-unit->ddict tree seed #:inc-filter inc-filter)))
	  (else seed)))
       seed
       (cdr tree))
      seed))
  
(define next-down-plain-defs
  (let ((p (node-join
	    ;;(select-kids (node-typeof? 'cpp-stmt))
	    ;;(select-kids (node-typeof? 'include))
	    ;;(select-kids (node-typeof? 'trans-unit))
	    (select-kids (node-typeof? 'cpp-stmt))
	    (select-kids (node-typeof? 'define))
	    ;; could node filter on (select-kids *TEXT* xxx
	    (node-filter
	     (lambda (n)
	       (if (pair? ((select-kids (node-typeof? 'args)) n)) #f n))))))
    (lambda (tree)
      (map can-def-stmt (p tree)))))

;; given keeper-defs (k-defs) and all defs (a-defs) expand the keeper
;; replacemnts down to constants (strings, integers, etc)
(define (process-defs mod-name k-defs a-defs)
  (sfscm "\n;; access to #define constants:\n")
  (let ((name (string->symbol (string-append mod-name "-def-val")))
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
	      (let ((deftab '(,@defs)))
		(lambda (k) (assq-ref deftab k)))))
  (sfscm "(export ~A)\n" name)))

;; ---

(define (intro-ffi path . opts)
  ;; pkg-config --cflags <pkg>
  ;; pkg-config --libs <pkg>

  (define (opts->attrs opts)
    (let iter ((attrs '()) (opts opts))
      (if (null? opts) (reverse attrs)
	  (iter (acons (car opts) (cadr opts) attrs) (cddr opts)))))
    
  (define (get-tree attrs)
    (let iter ((defines '()) (inc-dirs std-inc-dirs) (inc-files '())
	       (attrs attrs))
      (cond
       ((null? attrs) (parse-includes (reverse defines)
				      (reverse inc-dirs)
				      (reverse inc-files)))
       ((eqv? #:pkg-config (caar attrs))
	(iter defines (append (pkg-config-incs (cdar attrs)) inc-dirs)
	      inc-files (cdr attrs)))
       ((eqv? #:include (caar attrs))
	(iter defines inc-dirs (cons (cdar attrs) inc-files) (cdr attrs)))
       ((eqv? #:define (caar attrs))
	(iter (cons (cdar attrs) defines) inc-dirs inc-files (cdr attrs)))
       (else
	;;(simple-format #t "skipping ~S\n" (caar attrs))
	(iter defines inc-dirs inc-files (cdr attrs))))))
    
  (let* ((attrs (opts->attrs opts))
	 (dpath (string-join (map symbol->string path) "/"))
	 (dport (open-output-file (string-append dpath ".scm")))
	 (sf (lambda (fmt . args) (apply simple-format dport fmt args)))
	 (tree (get-tree attrs))
	 (incf (or (assq-ref attrs #:inc-filter) #f))
	 (udecls (reverse (c99-trans-unit->udict tree #:inc-filter incf)))
	 (uddict (c99-trans-unit->udict/deep tree))
	 (prefix (or (assq-ref attrs #:prefix) (symbol->string (last path))))
	 ;;
	 (ffi-defs (c99-trans-unit->ddict tree #:inc-filter incf))
	 ;;(ffi-defs (next-down-plain-defs tree))
	 (all-defs (trans-unit-defs/deep tree))
	 )
    ;;(ppout incf)
    ;;(ppout ffi-defs)
    ;;(quit)
    
    (set! *uddict* uddict)
    (set! *port* dport) ;; HACK
    (sf ";;\n")
    (sf ";; auto-generated by ffi-help.scm\n")
    (sf ";;\n")
    (sf "\n")
    (sf "(define-module ~S\n" path)
    (sf "  #:use-module (ffi-help)\n")
    (sf "  #:use-module ((system foreign) #:prefix ffi:)\n")
    (sf "  #:use-module ((bytestructures guile) #:prefix bs:)\n")
    (sf "  )\n")
    (sf "(define bs:struct bs:bs:struct)\n")
    (sf "\n")
    (sf "(define lib-link (dynamic-link ~S))\n" (assq-ref attrs #:library))
    (sf "(define (lib-func name) (dynamic-func name lib-link))\n")
    (process-defs prefix ffi-defs all-defs)
    (fold

     (lambda (pair type-list)
       (catch 'ffi-help-error
	 (lambda ()
	   (cond
	    (#t
	     ;;(sfout "~S\n" (car pair))
	     (sfscm "\n;; ~S\n" (car pair))
	     (udecl->ffi-decl (cdr pair) type-list))

	    ((member (car pair) '(
				  #|
				  ;;"cairo_get_reference_count"
				  ;;"cairo_set_dash"
				  "cairo_bool_t"
				  "cairo_matrix_t"
				  "cairo_region_t"
				  "cairo_destroy_func_t"
				  "cairo_region_contains_point"
				  "cairo_set_user_data"
				  "cairo_status_t"
				  |#
				  "cairo_t"
				  "cairo_create"
				  "cairo_destroy"
				  "cairo_surface_t"
				  "cairo_surface_destroy"
				  "cairo_move_to" "cairo_line_to"
				  "cairo_stroke"
				  "cairo_svg_surface_create"
				  "cairo_operator_t"
				  ))
	     ;;(simple-format #t "\n~S =>\n" (car pair)) (ppout (cdr pair))
	     (udecl->ffi-decl (cdr pair) type-list))

	    (else
	     type-list)))
	 
	 (lambda (key fmt . args)
	   (apply simple-format (current-error-port)
		  (string-append "ffi-help: " fmt "\n") args)
	   (sfscm ";; ... failed.\n")
	   type-list)))
     
     fixed-width-int-names
     udecls)
    (sf "\n;; --- last line ---\n")
    (close dport)
    ))

(define-syntax-rule (define-ffi-helper path-list attr ...)
  (intro-ffi (quote path-list) attr ...))

;;(define (compile-ffi-file . args) (sfout "args=~S\n" args) )

;; --- last line ---
