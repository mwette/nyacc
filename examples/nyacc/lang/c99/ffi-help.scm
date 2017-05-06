;;
;;
;; User is responsible for calling string->pointer and pointer->string.

(add-to-load-path (string-append (getcwd) "/../../../../module"))

(define-module (ffi-help)
  #:export-syntax (define-std-pointer-wrapper define-ffi-helper)
  #:export (*ffi-help-version*
	    unwrap-char*
	    bs-renamer ffi-renamer
	    pkg-config-incs
	    )
  #:use-module (nyacc lang c99 parser)
  #:use-module (nyacc lang c99 util1)
  #:use-module (nyacc lang c99 util2)
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
(use-modules ((sxml xpath) #:select (sxpath)))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))
(use-modules (srfi srfi-37))
(use-modules (ice-9 regex))
(use-modules (ice-9 match))
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
(define (ppscm tree)
  (pretty-print tree *port*))
(define (ppout tree)
  (pretty-print tree #:per-line-prefix "    "))
(define (newln)
  (newline *port*))

(define-syntax define-std-type-wrapper
  (lambda (x)
    (define (stx->str x) (symbol->string (syntax->datum x)))
    (define (gen-id tmpl-id . args)
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
		 (format p #,(string-append "<" (stx->str #'name) " ~x>") v)))
	     (export #,pred) (export #,wrap) (export #,unwr)))))))

(define-std-pointer-wrapper double)

(define (unwrap-char* value)
  (if (string? value)
      (string->pointer value)
      value))

(define (parse-includes cpp-defs inc-dirs inc-files)
  ;;(simple-format #t "inc-dirs=~S\n" inc-dirs)
  (let* ((all-defs (append cpp-defs (gen-gcc-defs)))
	 (prog (string-join
		(map (lambda (inc-file)
		       (string-append "#include \"" inc-file "\"\n"))
		     inc-files)
		"\n"))
	 )
    (with-input-from-string prog
      (lambda () (parse-c99 #:cpp-defs all-defs #:inc-dirs inc-dirs
			    #:mode 'decl)))))

(define (parse-string str)
  (with-input-from-string str parse-c99))

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
  (match mspec-tail
    (`((fixed-type ,name)) (string-append "bs:" name))
    (`((float-type ,name)) (string-append "bs:" name))
    (`((void)) "bs:void")
    (`((pointer-to) (fixed-type ,name)) (string-append "bs:" name "*"))
    (`((pointer-to) (float-type ,name)) (string-append "bs:" name "*"))
    (`((pointer-to) (void)) "bs:void*")
    ;;
    (`((typename ,name)) name)
    (`((pointer-to) (typename ,name)) (string-append name "*"))
    ;;
    (otherwise (error "missed" mspec-tail))))

(define (mtype->ffi mspec-tail)
  (match mspec-tail
    (`((fixed-type ,name)) (string-append "ffi:" name))
    (`((float-type ,name)) (string-append "ffi:" name))
    (`((void)) "ffi:void")
    (`((pointer-to) (fixed-type ,name)) (string-append "ffi:" name "*"))
    (`((pointer-to) (float-type ,name)) (string-append "ffi:" name "*"))
    (`((pointer-to) (void)) "ffi:void*")
    ;;
    (`((typename ,name)) name)
    (`((pointer-to) (typename ,name)) (string-append name "*"))
    ;;
    (otherwise (error "missed" mspec-tail))))

(define ffi-typemap
  '(("void" . ffi:void)
    ("int8_t" . int8) ("uint8_t" . ffi:uint8) 
    ("int16_t" . int16) ("uint16_t" . ffi:uint16) 
    ("int32_t" . int32) ("uint64_t" . ffi:uint64) 
    ("double" . ffi:double) ("float" . ffi:float)
    ;;
    ("int" . ffi:int) ("unsigned" . ffi:unsigned-int)
    ("unsigned int" . ffi:unsigned-int)
    ("long" . ffi:long) ("long int" . ffi:long)
    ("unsigned long" . ffi:unsigned-long)
    ("unsigned long int" . ffi:unsigned-long)))

(define (mspec->ffi-sym mspec)
  (match (cdr mspec)
    (`((fixed-type ,name))
     (or (assoc-ref ffi-typemap name) (error ":( " name)))
    (`((float-type ,name))
     (or (assoc-ref ffi-typemap name) (error ":( " name)))
    (`((void)) 'ffi:void)
    ;;(`((pointer-to) ,type) 'ffi:pointer)
    (`((pointer-to) ,type) ''*)
    (otherwise (error "mspec->ffi-sym missed" mspec))))

(define (mspec->ffi-wrapper mspec)
  (match (cdr mspec)
    (`((fixed-type ,name))
     (if (assoc-ref ffi-typemap name) #f (error ":( " name)))
    (`((float-type ,name))
     (if (assoc-ref ffi-typemap name) #f (error ":( " name)))
    (`((void)) #f)
    (`((pointer-to) (typename ,typename))
     (if (member typename *wrapped*)
	 (string->symbol (string-append "wrap-" typename "*"))
	 #f))
    (`((pointer-to) . ,rest)		; HACK
     'identity)
    (otherwise (error "mspec->ffi-wrapper missed" mspec))))

(define (mspec->ffi-unwrapper mspec)
  ;;(sfout "cdr mspec = ~S\n" (cdr mspec))
  (match (cdr mspec)
    (`((fixed-type ,name))
     (if (assoc-ref ffi-typemap name) #f (error ":( " name)))
    (`((float-type ,name))
     (if (assoc-ref ffi-typemap name) #f (error ":( " name)))
    (`((void))
     #f)
    (`((pointer-to) (typename ,typename))
     (if #t ;;(member type *wrapped*)
	 (string->symbol (string-append "unwrap-" typename "*"))
	 #f))
    (`((pointer-to) . ,rest)		; HACK
     'identity)
    (otherwise (error "mspec->ffi-unwrapper missed" mspec))))

;; --- structures 

(define (cnvt-field field)
  (let ((mspec (udecl->mspec field)))
    #f))

(define (acons-defn name type seed)
  ;;(cons ``(,(string->symbol ,name) ,,(string->symbol type)) seed))
  ;;(cons (eval-string (string-append "`(" name " ," type ")")) seed))
  (cons (string-append "(" name " ," type ")") seed))

;; cairo_matrix_t
(define (cnvt-struct typename field-list struct-name)
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
		  (iter (acons-defn name type sflds) (cdr decls))))))
	 (xx (string-append "(quote `(define " typename " (bs:struct "
			    (string-join sflds " ") "))"))
	 (yy (simple-format #t "xx=[~S]\n" xx))
	 (spec
	  `(define ,(string->symbol typename) (bs:struct ,@sflds)))
	 ;;(eval-string
	 ;; (string-append "(quote (define " typename " (bs:struct `("
	;;		 (string-join sflds " ") "))))")))
	 )
    (ppscm spec)
    (sfscm "(export ~A)\n" typename)
    (newln)
    #t))

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
		   ((,unwrap ,(string->symbol name))))
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
  (match typel
    (`((fixed-type ,name)) name)
    (`((float-type ,name)) name)
    (`((typename ,name)) name)
    (`((pointer-to) (fixed-type ,name)) (string-append name "*"))
    (`((pointer-to) (float-type ,name)) (string-append name "*"))
    (`((pointer-to) (typename ,name)) (string-append name "*"))
    (`((pointer-to) (void)) "void*")
    (otherwise (sferr "OTHERWISE=~S\n" typel))
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

    ;; anonymous struct typedef
    ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-ref (ident ,name))))
	(init-declr (ident ,typename)))
     (let ((ptname (string-append typename "*")))
       (sfscm "\n(define-std-pointer-wrapper ~A)\n" ptname)
       (set! *wrapped* (cons typename *wrapped*))
       (cons typename type-list)))

    ;; populated struct typedef s/ name
    ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-def (ident ,name) ,field-list)))
	(init-declr (ident ,typename)))
     (let ()
       (cnvt-struct typename field-list name)
       ;;(sfscm "(define-std-pointer-wrapper ~A)\n" ptname)
       (cons typename type-list)))

    ;; typedef int
    ((udecl
      (decl-spec-list
       (stor-spec (typedef))
       (type-spec (fixed-type ,name)))
      (init-declr (ident ,typename)))
     (let ()
       ;; don't use this
       ;;(sfscm "(define-std-type-wrapper ~A ~A)\n\n" typename name)
       (cons typename type-list)))
    
    ;; function returning pointer value
    ((udecl ,specl
	    (init-declr
	     (ptr-declr
	      (pointer) (ftn-declr (ident ,name) (param-list . ,params)))))
     (sfscm "\n;; ~A\n" name)
     ;;(pretty-print-c99 udecl *port* #:per-line-prefix ";; ")
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
     (sfscm ";; MISSED IT\n")
     ;;(pretty-print udecl *port*)
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
       ((null? attrs) (parse-includes defines inc-dirs inc-files))
       ((eqv? #:pkg-config (caar attrs))
	(iter defines (append (pkg-config-incs (cdar attrs)) inc-dirs)
	      inc-files (cdr attrs)))
       ((eqv? #:include (caar attrs))
	(iter defines inc-dirs (cons (cdar attrs) inc-files) (cdr attrs)))
       ((eqv? #:define (caar attrs))
	(iter (cons (cdar attrs) defines) inc-dirs inc-files (cdr attrs)))
       (else
	(simple-format #t "skipping ~S\n" (car attrs))
	(iter defines inc-dirs inc-files (cdr attrs))))))
    
  (let* ((attrs (opts->attrs opts))
	 (dpath (string-join (map symbol->string path) "/"))
	 (dport (open-output-file (string-append dpath ".scm")))
	 (sf (lambda (fmt . args) (apply simple-format dport fmt args)))
	 (tree (get-tree attrs))
	 (filt (or (assq-ref attrs #:filter) identity))
	 (udecls (reverse (c99-trans-unit->udict tree #:filter filt)))
	 (uddict (c99-trans-unit->udict/deep tree))
	 )
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
    (fold
     (lambda (pair type-list)
       (cond
	(#f
	 (sfout "~S\n" (car pair))
	 (sfscm "\n;; ~S\n" (car pair))
	 (udecl->ffi-decl (cdr pair) type-list))

	((member (car pair) '(
			      ;;"cairo_get_reference_count"
			      ;;"cairo_set_user_data"
			      "cairo_matrix_t"
			      ;;"cairo_set_dash"
			      "cairo_bool_t"
			      "cairo_region_t"
			      "cairo_create"
			      "cairo_region_contains_point"
			      ))
	 ;;(simple-format #t "\n~S =>\n" (car pair)) (ppout (cdr pair))
	 (udecl->ffi-decl (cdr pair) type-list))

	(else
	 type-list)))
     fixed-width-int-names
     udecls)
    (sf "\n;; --- last line ---\n")
    (close dport)
    ))

(define-syntax-rule (define-ffi-helper path-list attr ...)
  (intro-ffi (quote path-list) attr ...))

;; --- last line ---
