#!/opt/local/bin/guile -e main -s
!#

(add-to-load-path (string-append (getcwd) "/../../../../examples/"))
(add-to-load-path (string-append (getcwd) "/../../../../module/"))

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
;;(use-modules (system base pmatch))

(define my-defs (gen-gcc-defs '()))
(define my-help '())
(define (my-xdef? name mode)
  (cond
   ((< (string-length name) 6) #f)
   ((string= name "CAIRO" 0 5) #t)
   ((string= name "cairo" 0 5) #t)
   (else #f)))
   
;; ===== XXXX ===========================

(define (sfout fmt . args)
  (apply simple-format #t fmt args))

(define (process-sx sx)
  
  (define (fD tree seed xtra) ;; => tree seed xtra
    (sxml-match tree
      (,otherwise
       (sfout "~s\n" (car tree))
       (values tree '() xtra))))

  (define (fU tree seed xtra kseed kxtra) ;; => seed xtra
    (values (cons (reverse kseed) seed) xtra))

  (define (fH tree seed xtra) ;; => seed xtra
    (values (cons tree seed) xtra))

  (let ()
    (foldts*-values fD fU fH sx '() '())))


(define rx1 (make-regexp "(.*[^ \t])[ \t]*/\\*.*\\*/ *$"))

(define (scrub-repl repl)
  (cond
   ((regexp-exec rx1 repl) =>
    (lambda (m) (match:substring m 1)))
   (else
    repl)))

;; @item get-udict file [incdirs]
(define (tree-defs tree)
  (reverse
   (fold
    (lambda (def dict)
      (sxml-match def
	((define (name ,name) (repl ,repl))
	 (acons name (scrub-repl repl) dict))
	(,otherwise dict)))
    '()
    ((sxpath '(cpp-stmt define)) tree))))

(define (file-decls tree . rest)
  (tree->udict tree))
  
(define (full-decls tree . rest)
  (tree->udict/deep tree))

(define (my-parse defs incs file)
  (with-input-from-file file
    (lambda () (parse-c99 #:cpp-defs (append my-defs defs) #:inc-dirs incs
                          #:inc-help my-help #:xdef? my-xdef?
			  #:mode 'decl #:debug #f))))

#;(define (parse defs incs)
  (parse-c99 #:cpp-defs defs #:inc-dirs incs
	     #:mode 'code #:inc-help helpers #:debug #f))
     

;; ===== main ===========================

(define (arg:inc-dir opt name arg incs defs files)
  (values (cons arg incs) defs files))

(define (arg:define opt name arg incs defs files)
  (values incs (cons arg defs) files))

(define (arg:free arg incs defs files)
  (values incs defs (cons arg files)))

(define (bad-arg opt name arg incs defs files)
  (simple-format #t "bad-arg: ~S\n" name)
  (quit))

(define opts
  (list
   (option '(#\I "inc-dir") #t #t arg:inc-dir)
   (option '(#\D "define") #t #t arg:define)))

(define (process-args args)
  (call-with-values
      (lambda () (args-fold args opts bad-arg arg:free '() '() '()))
    (lambda (incs defs files)
      (list (cons 'incs (reverse incs))
	    (cons 'defs (reverse defs))
	    (cons 'files (reverse files))))))

(define (main argl)
  (let* ((args (cdr (program-arguments)))
	 (argd (process-args args))
	 (file (car (assq-ref argd 'files)))
	 (defs (assq-ref argd 'defs))
	 (incs (assq-ref argd 'incs))
	 (tree (my-parse defs incs file))
	 ;;(file-decls (tree->udict tree))
	 ;;(decl-dict (tree->udict/deep tree))
	 )
    (display "hello, world!\n")
    #t))

;; === devel


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

(define keepers
  (append fixed-width-int-names
	  '("float" "double" "int" "unsigned int" "long" "unsigned long"
	    "size_t" "ssize_t" "ptrdiff_t" "void")))

;; @deffn ctypename->scm c-name => scm-name
;; Return the guile name (not symbol) for the associated C name.
;; @example
;; (ctypename->scm "unsigned int") => "unsigned-int"
;; @end example
;; Note: this might be changed to return symbol.
;; @end deffn
(define ctypename->scm
  (let ((map-list
	 (append '(("unsigned int" "unsigned-int")
		   ("unsigned long" "unsigned-long"))
		 (map (lambda (n)
			(cons n (substring n (- (string-length n) 2))))
		      fixed-width-int-names))))
    (lambda (name)
      (let iter ((ml map-list))
	(cond
	 ((null? ml) name)
	 ((string=? (caar ml) name) (cdar ml))
	 (else (iter (cdr ml))))))))

;; missing char short etc

(define (parse-string str)
  (with-input-from-string str parse-c99))

;; return symbol
(define type-name->ffi
  (let ((tm '(("void" . void)
	      ("int8_t" . int8) ("uint8_t" . uint8) 
	      ("int16_t" . int16) ("uint16_t" . uint16) 
	      ("int32_t" . int32) ("uint64_t" . uint64) 
	      ("double" . double) ("float" . float)
	      ;;
	      ("int" . int) ("unsigned" . unsigned-int)
	      ("unsigned int" . unsigned-int)
	      ("long" . long) ("long int" . long)
	      ("unsigned long" . unsigned-long)
	      ("unsigned long int" . unsigned-long)
	      )))
    (lambda (name)
      (let* ((ln (string-length name)))
	(cond
	 ((char=? #\* (string-ref name (1- ln))) ''*)
	 ((assoc-ref tm name))
	 (else (string->symbol (string-append "tbd:" name))))))))

(define (do-param-arg-type typel)
  ;;(sfout "dpat ~S\n" typel)
  (match typel
    (`((float-type ,name))
     name)
    ('((pointer-to) (fixed-type "char"))
     "char*")
    (`((pointer-to) (typename ,name))
     (string-append name "*"))
    (otherwise
     (sfout "OTHERWISE=~S\n" typel))
    ))

;; This will turn the C parameter list @code{(void)} into @code{()}.
(define (remove-void-param params)
  (cond
   ((null? params) params)
   ((and (null? (cdr params))
	 (equal? (car params)
		 '(param-decl (decl-spec-list (type-spec (void))))))
    '())
   (else params)))
	 
(define (do-params params)
  (fold
   (lambda (param-decl seed)
     (let* ((spec (udecl->mspec param-decl))
	    (name (car spec))
	    (tail (cdr spec)))
       (acons name (do-param-arg-type tail) seed)))
   '()
   (remove-void-param params)))

(define (f-arg-name name)
  (string->symbol (string-append "~" name)))

(define (prep-arg arg)
  (let* ((name (car arg)) (type (cdr arg)) (ln (string-length type)))
    (cond
     ((string=? type "char*")
      `(,(f-arg-name name) (string->pointer ,(string->symbol name))))
     ((char=? #\* (string-ref type (1- ln)))
      `(,(f-arg-name name)
	(,(string->symbol (string-append "unwrap-" type))
	 ,(string->symbol name))))
     (else
      `(,(f-arg-name name) ,(string->symbol name))))))

(define (wrap-result type val)
  (cond
   ((string=? type "void")
    val)
   (else
    `(,(string->symbol (string-append "wrap-" type)) ,val))))

(define (make-fctn name rtype args)
  `(define ,(string->symbol name)
     (let ((f (pointer->procedure
	       ,(type-name->ffi rtype) (lib-func ,name)
	       (list ,@(map (lambda (a) (type-name->ffi (cdr a))) args)))))
       (lambda ,(map (lambda (a) (string->symbol (car a))) args)
	 (let ,(map prep-arg args)
	   ,(wrap-result rtype `(f ,@(map f-arg-name (map car args)))))))))

(define (cnvt-field field)
  (let ((mspec (udecl->mspec field)))
    #f))

(define keepers '())

(define (cnvt-mtype mspec-tail)
  (match mspec-tail
    (`((float-type ,name))
     name)
    (`((typename ,name))
     name)
    (`((pointer-to) (typename ,name))
     (string-append name "*"))
    
    (`((pointer-to) (void)) "void*")
    (`((void)) "void")
    (otherwise
     (error "missed" mspec-tail))))

(define (acons-defn name type seed)
  (cons ``(,(string->symbol ,name) ,,(string->symbol type)) seed))

;; cairo_matrix_t
(define (cnvt-struct typename field-list struct-name)
  (let* ((fldl (clean-field-list field-list)) ; remove lone comments
	 (flds (cdr fldl))
	 (uflds (fold munge-comp-decl '() flds)) ; reverse order
	 (sflds					 ; bs fields in order
	  (let iter ((sflds '()) (decls uflds))
	    (if (null? decls) sflds
		(let* ((name (caar decls))
		       (spec (udecl->mspec/comm (cdar decls) keepers))
		       (type (cnvt-mtype (cddr spec))))
		  (iter (acons-defn name type sflds) (cdr decls))))))
	 (spec
	  `(define ,(string->symbol typename)
	     (bs:struct ,@sflds)))
	 )
    (pretty-print spec)
    (newline)
    #t))

(define (gen-pointer-return-type specl type-list)
  (let* ((ndecl `(udecl
		  ,specl
		  (init-declr (ptr-declr (pointer) (ident "NAME")))))
	 (mspec (udecl->mspec ndecl #:keep type-list)))
    (cnvt-mtype (cdr mspec))))
(define (gen-return-type specl type-list)
  (let* ((ndecl `(udecl ,specl (init-declr (ident "NAME"))))
	 (mspec (udecl->mspec ndecl #:keep type-list)))
    (cnvt-mtype (cdr mspec))))
	    
(define (udecl->ffi-decl udecl type-list)
  (set! keepers type-list)
  (sxml-match udecl
    ;; anonymous struct typedef
    ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-ref (ident ,name))))
	(init-declr (ident ,typename)))
     (let ((ptname (string-append typename "*")))
       (sfout "(define-std-pointer-wrapper ~A)\n" ptname)
       (sfout "(export ~A)\n\n" ptname)
       (cons typename type-list)))
    ;; populated struct typedef s/ name
    ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-def (ident ,name) ,field-list)))
	(init-declr (ident ,typename)))
     (let ()
       ;;(pretty-print field-list)
       (cnvt-struct typename field-list name)
       #t))
    
    ;; function
    ((udecl ,specl
	    (init-declr
	     (ptr-declr
	      (pointer) (ftn-declr (ident ,name) (param-list . ,params)))))
     (let* ((rtype (gen-pointer-return-type specl type-list))
	    (args (reverse (do-params params)))
	    (fctn (make-fctn name rtype args)))
       (pretty-print fctn)
       (sfout "(export ~A)\n\n" name)
       type-list))
    ((udecl ,specl
	    (init-declr
	     (ftn-declr (ident ,name) (param-list . ,params))))
     (let* ((rtype (gen-return-type specl type-list))
	    (args (reverse (do-params params)))
	    (fctn (make-fctn name rtype args)))
       (pretty-print fctn)
       (sfout "(export ~A)\n\n" name)
       type-list))
    
    (,otherwise
     (sfout "missed it\n")
     (pretty-print udecl)
     type-list)))

;; (eval-when ...)
;; %guile-build-info   (assq-ref %guile-build-info 'CFLAGS)
;; %host-type

;; typedef struct _cairo_region cairo_region_t;
;; cairo_public cairo_status_t
;; cairo_region_intersect (cairo_region_t *dst, const cairo_region_t *other);

;;(pretty-print keepers)
;; todo:
;; 1) in stripdown, add enum as keepers, but canonicalize as enum_typename_t
;;    or use poo to replace enum with int
;; 2) in stripdown, add struct pointer as keeper
;;(pretty-print keepers)
;; 3) need to detect pointer-only types (e.g.,
;;    typedef struct _cairo_region cairo_region_t;

;; keeping const
;; code to detect opaque

;; need to look for pointer-typename, and anon pointer-typename

;;(define-syntax-rule (foo a b) '(a ,b))
;;(pretty-print (foo x double))

(define (cairo-filter path)
  (string=? "cairo" (substring path 0 5)))

(define (inc-path->scm-path path)
  (let* ((dx (string-rindex path #\.)))
    (map string->symbol 
	 (string-split (substring path 0 dx) #\/))))

#;(let* ((file "/opt/local/include/cairo/cairo-svg.h")
       (path "/opt/local/lib/libcairo")
       (defs '())
       (incs '("/opt/local/include" "/opt/local/include/cairo" "/usr/include"))
       (tree (my-parse defs incs file))
       (file-decls (reverse (c99-trans-unit->udict tree #:filter cairo-filter)))
       (udecl-dict (c99-trans-unit->udict/deep tree)))

  (sfout ";; autogenerated\n")
  (sfout "(define-module ~S\n" (inc-path->scm-path "cairo/cairo.h"))
  (sfout "  #:use-module (ffi-help)\n")
  ;;(sfout " #:use-module (ice-9 format)\n")
  (sfout "  )\n\n")
  (sfout "(define lib-link (dynamic-link ~S))\n" path)
  (sfout "(define (lib-func name) (dynamic-func name lib-link))\n")
  (sfout "\n")

  (fold
   (lambda (pair type-list)
     (udecl->ffi-decl (cdr pair) type-list))
   fixed-width-int-names
   (filter
    (lambda (e)
      (member (car e)
	      '(
		;;"cairo_matrix_t" ;; type clash with bytestructures
		"cairo_surface_t" "cairo_surface_destroy"
		"cairo_svg_surface_create"
		"cairo_t" "cairo_create" "cairo_destroy"
		"cairo_move_to" "cairo_line_to" "cairo_stroke"
		)))
    file-decls))

  ;;(pretty-print (assoc-ref udecl-dict "cairo_move_to"))

  (sfout "\n;; --- last line ---\n")
)


(define (git-filter path)
  (string=? "git/" (substring path 0 4)))

;; pkg-config --cflags git2
;; pkg-config --libs git2

(let* ((file "/opt/local/include/git2.h")
       (defs (gen-gcc-defs))
       (incs '("/opt/local/include" "/opt/local/include/git2" "/usr/include"))
       (tree (my-parse defs incs file))
       (file-decls (reverse (c99-trans-unit->udict tree #:filter git-filter)))
       ;;(udecl-dict (c99-trans-unit->udict/deep tree))
       (udecl (assoc-ref file-decls "git_repository"))
       )
  (sfout "got it\n")
  (pretty-print udecl)
  )


  ;; --- last line ---

