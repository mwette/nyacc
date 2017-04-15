;;

(add-to-load-path (string-append (getcwd) "/../../../../module"))

(define-module (ffi-help)
  #:export-syntax (define-std-pointer-wrapper define-ffi-helper)
  #:export (*ffi-help-version*
	    char*? wrap-char* unwrap-char*
	    bs-renamer
	    pkg-config-incs
	    )
  #:use-module (nyacc lang c99 parser)
  #:use-module (nyacc lang c99 util1)
  #:use-module (nyacc lang c99 util2)
  #:use-module (system foreign)
  #:use-module ((bytestructures guile)
		#:renamer
		(lambda (s)
		  (let ((n (symbol->string s)))
		    (if (string=? "bs:" (substring n 0 3)) s
			(string->symbol (string-append "bs:" n)))))
		)
  #:use-module (ice-9 format)
  #:version (0 1 0))

(define *ffi-help-version* "0.01.0")

(define std-inc-dirs
  `("/usr/include"
    ;;,(assq-ref %guile-build-info 'includedir)
    ))

(define (sfout fmt . args)
  (apply simple-format #t fmt args))

(define *tree* #f)

(define (parse-includes cpp-defs inc-dirs inc-files)
  ;;(simple-format #t "inc-dirs=~S\n" inc-dirs)
  (let* ((all-defs (append cpp-defs (gen-gcc-defs)))
	 (prog (string-join
		(map (lambda (inc-file)
		       (string-append "#include \"" inc-file "\"\n"))
		     inc-files)
		"\n"))
	 (tree (with-input-from-string prog
		 (lambda () (parse-c99 #:cpp-defs all-defs #:inc-dirs inc-dirs
				       #:mode 'decl))))
	 )
    (set! *tree* tree)))

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
	(iter defines inc-dirs inc-files (cdr attrs)))))
    
  (let* ((attrs (opts->attrs opts))
	 (dpath (string-join (map symbol->string path) "/"))
	 (dport (open-output-file (string-append dpath ".scm")))
	 (sf (lambda (fmt . args) (apply simple-format dport fmt args)))
	 (tree (get-tree attrs))
	 ;;(file-decls (reverse (c99-trans-unit->udict tree #:filter cairo-filter)))
	 ;;(udecl-dict (c99-trans-unit->udict/deep tree)))
	 )

    (sf "(define-module (path-list)\n")
    (sf "  #:(use-module (ffi-help)\n")
    (sf "  #:(use-module (system foreign)\n")
    (sf "  #:(use-module ((bytestructures guile) #:renamer bs-renamer)\n")
    (sf "  )\n\n")
    (sf "(define lib-link (dynamic-link ~S))\n" (assq-ref attrs #:library))
    (sf "(define (lib-func name) (dynamic-func name lib-link))\n\n")

    
    (close dport)
    ))

(define-syntax-rule (define-ffi-helper path-list attr ...)
  (intro-ffi (quote path-list) attr ...))

;;(define-syntax define-ffi-helper
;;  (lambda (x)
;;    (letrec-syntax

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
       #`(define-wrapped-pointer-type name
	   #,(gen-id #'name #'name "?")
	   #,(gen-id #'name "wrap-" #'name)
	   #,(gen-id #'name "unwrap-" #'name)
	   (lambda (v p)
	     (format p #,(string-append "<" (stx->str #'name) " ~x>") v)))))))

;; @deffn {Variable} char*
;; The C string type.
;; @end deffn
(define-wrapped-pointer-type char*
  char*? wrap-char* unwrap-char*
  (lambda (v p)
    (format p "<char* ~a ~s>" v (pointer->string v))))

;; --- last line ---
