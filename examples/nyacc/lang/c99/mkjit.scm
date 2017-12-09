;; mkjit.scm

;; try to convert GNU lightning to Scheme

(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 xparser))
(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang c99 util1))
(use-modules (nyacc lang util))
(use-modules (nyacc lang sx-match))
(use-modules (nyacc util))
(use-modules (srfi srfi-1))
(use-modules (ice-9 pretty-print))
(use-modules ((sxml xpath) #:select (sxpath)))

(define (sferr fmt . args) (apply simple-format #t fmt args))
(define pperr pretty-print)


(define cpp-defs
  (append
   '("__GNUC__=6" "__signed=signed")
   (remove (lambda (s)
	     (string-contains s "_ENVIRONMENT_MAC_OS_X_VERSION"))
	   (get-gcc-cpp-defs))))

(define inc-dirs
  (append
   '("/opt/local/include" "/usr/include")
   (get-gcc-inc-dirs)))

(define inc-help
  (append
   '(("__builtin"
      "_sc=char" "_uc=unsigned\\ char" "_us=unsigned\\ short"
      "_ui=unsigned\\ int" "_sl=long" "_ul=unsigned\\ long"))
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
	)))
    (else
     '(("__builtin"
	"__builtin_va_list=void*" "__attribute__(X)="
	"__inline=" "__inline__="
	"__asm(X)=" "__asm__(X)="
	"__has_include(X)=__has_include__(X)"
	"__extension__="
	))))))

(define merge-inc-bodies
  (let* ((p (sxpath '(cpp-stmt include trans-unit))))
    (lambda (t) (cons 'trans-unit (apply append (map cdr (p t)))))))

(define (c99->scm expr)
  (sferr "c99->scm\n") (pperr expr)
  (sx-match expr
    ((fctn-call (ident ,name) (expr-list . ,argl))
     `(,name X)
     )
    (*
     ;;(sferr "missed ~S\n" expr)
     #f)))

    
(define (cnvt)
  (define (cnvt-fctn name args repl)
    (let* ((n (string->symbol name))
	   (a (map string->symbol args))
	   (r (catch 'c99-error
		(lambda () (parse-c99x repl))
		(lambda (tag . rest)
		  (sferr "missed ~S\n" repl))))
	   (x `(define (,n ,@a) #f))
	   )
      (pretty-print x)
      (if r
	  (c99->scm r)
	  (sferr "not parsing ~S\n" repl))
      #f))
  (define (cnvt-cpp-def stmt)
    (let ((name (car (assq-ref (sx-tail stmt 1) 'name)))
	  (args (assq-ref (sx-tail stmt 1) 'args))
	  (repl (car (assq-ref (sx-tail stmt 1) 'repl)))
	  )
      (if args
	  (cnvt-fctn name args repl))
      #f))
  (let* ((tree (with-input-from-string "#include <lightning.h>\n"
		 (lambda ()
		   (parse-c99 #:cpp-defs cpp-defs
			      #:inc-dirs inc-dirs
			      #:inc-help inc-help
			      #:mode 'decl))))
	 (tree (merge-inc-bodies tree))
	 (tree (merge-inc-bodies tree))
	 )
    (for-each
     (lambda (item)
       (case (car item)
	 ((comment)
	  #f)
	 ((decl)
	  #f)
	 ((cpp-stmt)
	  (case (sx-tag (sx-ref item 1))
	    ((define) (cnvt-cpp-def (sx-ref item 1)))
	    (else #f)))
	 (else
	  ;; jit_flush_code jit_fail 
	  ;;(pperr item)
	  #f)))
     (cdr tree))
    ))

(cnvt)

;; -- last line ---
