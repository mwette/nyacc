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
(use-modules (ice-9 pretty-print))
(use-modules (system base pmatch))

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

(define (poo decl seed)
  (pretty-print decl)
  (sxml-match (cdr decl)
    ((decl (decl-spec-list . ,spec)
	   (init-declr (ftn-declr (ident ,name) (param-list . ,params))))
     #t)
    (,otherwise
     #f))
  #f)

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

;;(define (do-param-decl pdecl)

(define (do-fctn-udecl udecl)
  (let*-values
   (((ret-dsl)
     (values (sx-ref udecl 1)))
    ((name params)
     (pmatch (sx-ref udecl 2)
	     ((init-declr
	       (ftn-declr (ident ,name)
			  (param-list . ,params)))
	      (values name params))))
    )
   (simple-format #t "name=~S\n" name)
   (pretty-print
    (fold munge-param-decl '() params))
    ;;(declr->ident (caddar params)))
   ))

;; (eval-when ...)
;; %guile-build-info
;; %host-type

;; typedef struct _cairo_region cairo_region_t;
;; cairo_public cairo_status_t
;; cairo_region_intersect (cairo_region_t *dst, const cairo_region_t *other);

(let* ((file "/opt/local/include/cairo/cairo.h")
       (defs '())
       (incs '("/opt/local/include" "/opt/local/include/cairo" "/usr/include"))
       (tree (my-parse defs incs file))
       (file-decls (c99-trans-unit->udict tree))
       (udecl-dict (c99-trans-unit->udict/deep tree))
       ;;(keepers (fold-enum-typenames decl-dict '()))
       )

  ;;(pretty-print keepers)
  ;; todo:
  ;; 1) in stripdown, add enum as keepers, but canonicalize as enum_typename_t
  ;;    or use poo to replace enum with int
  ;; 2) in stripdown, add struct pointer as keeper
  ;;(pretty-print keepers)
  ;; 3) need to detect pointer-only types (e.g.,
  ;;    typedef struct _cairo_region cairo_region_t;

  ;; keep const?
  
  ;;#|
  (let* ((name "cairo_region_intersect")
	 (udecl (assoc-ref udecl-dict name))
	 ;;(decl (stripdown decl udecl-dict #:keep keepers))
	 ;;(decl (stripdown-2 decl udecl-dict))
	 (p1 '(param-decl
	       (decl-spec-list
		(type-qual "const")
		(type-spec (typename "cairo_region_t")))
	       (param-declr
		(ptr-declr (pointer) (ident "other")))))
	 )

    ;;(do-fctn-udecl udecl)
    (pretty-print p1)
    ;;(pretty-print (udecl->mspec p1))
    0)
  ;;|#

  ;; code to detect opaque

  #|
  (let* ((code "double x[10]; /* state vector */")
	 ;;(code "enum { ABC = 123 };")
	 ;;(code "typedef const char *string_t; extern string_t cmds[10];")
	 (code "typedef struct { int hi, *lo; } xyz_t; xyz_t f[3]; /* demo */")
	 (tree (parse-string code))
	 (udict (tree->udict tree))
	 (udecl (cdar udict))
	 (xdecl (expand-decl-typerefs udecl udict #:keep '()))
	 (mspec (udecl->mspec/comm udecl))
	 )
    (pretty-print tree)
    (pretty-print udecl)
    (pretty-print xdecl)
    (pretty-print-c99 xdecl)
    (pretty-print mspec)
    )
  |#

  #f)

;; --- last line ---

