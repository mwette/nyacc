;;

(add-to-load-path (string-append (getcwd) "/../../../../module"))

(define-module (ffi-help)
  #:export-syntax (define-std-pointer-wrapper define-ffi-helper)
  #:export (*ffi-help-version*
	    char*? wrap-char* unwrap-char*
	    bs-renamer
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
    ,(assq-ref %guile-build-info 'includedir)
    ))

(define (sfout fmt . args)
  (apply simple-format #t fmt args))

(define *tree* #f)

(define (parse-includes cpp-defs inc-dirs inc-files)
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

(define (intro-ffi path . opts)
  ;; pkg-config --cflags <pkg>
  ;; pkg-config --libs <pkg>

  (define (opts->attrs opts)
    (let iter ((attrs '()) (opts opts))
      (if (null? opts) (reverse attrs)
	  (iter (acons (car opts) (cadr opts) attrs) (cddr opts)))))
    
  (let* ((attrs (opts->attrs opts))
	 (dpath (string-join (map symbol->string path) "/"))
	 (dport (open-output-file (string-append dpath ".scm")))
	 (sf (lambda (fmt . args) (apply simple-format dport fmt args)))
	 )
    (sf "(define-module (path-list)\n")
    (sf "  #:(use-module (ffi-help)\n")
    (sf "  #:(use-module (system foreign)\n")
    (sf "  #:(use-module ((bytestructures guile) #:renamer bs-renamer)\n")
    (sf "  )\n\n")
    (sf "(define lib-link (dynamic-link ~S))\n" (assq-ref attrs #:library))
    (sf "(define (lib-func name) (dynamic-func name lib-link))\n\n")

    ;; collect includes
    (let iter ((defs '()) (incs '()) (attrs attrs))
      (cond
       ((null? attrs) (parse-includes defs std-inc-dirs incs))
       ((eqv? #:include (caar attrs))
	(iter defs (cons (cdar attrs) incs) (cdr attrs)))
       ((eqv? #:define (caar attrs))
	(iter (cons (cdar attrs) defs) incs (cdr attrs)))
       (else (iter defs incs (cdr attrs)))))
    
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
