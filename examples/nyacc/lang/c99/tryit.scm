;; examples/nyacc/lang/c99/tryit.scm
;;(debug-set! stack 750)

(add-to-load-path (getcwd))

(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 cxeval))
(use-modules (nyacc lang c99 pprint))
(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang c99 cpp))
(use-modules (nyacc lang c99 util))
(use-modules (nyacc lang sx-util))
(use-modules (nyacc lang util))
(use-modules (nyacc lex))
(use-modules (nyacc util))
(use-modules (ice-9 pretty-print))

(define (sf fmt . args) (apply simple-format #t fmt args))
(define pp pretty-print)
(define ppin (lambda (sx) (pretty-print sx #:per-line-prefix "  ")))
(define pp99 (lambda (sx) (pretty-print-c99 sx #:per-line-prefix "  ")))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr sx) (pretty-print sx (current-error-port) #:per-line-prefix "  "))

(define cpp-defs (get-gcc-cpp-defs))
(define inc-dirs (get-gcc-inc-dirs))
(define inc-help c99-def-help)

(define mode 'code)
(define debug #f)
(define xdef? (lambda (name mode) (memq mode '(code decl))))

(define (parse-file file)
  (with-input-from-file file
    (lambda ()
      (parse-c99 #:cpp-defs cpp-defs 
		 #:inc-dirs inc-dirs
		 #:inc-help inc-help
		 #:mode mode #:debug debug
		 #:show-incs #t
		 #:xdef? xdef?))))

(define (parse-string str)
  (with-input-from-string str
    (lambda ()
      (parse-c99 #:cpp-defs cpp-defs
		 #:inc-dirs inc-dirs 
		 #:inc-help inc-help
		 #:show-incs #f
 		 #:mode mode #:debug debug
		 #:xdef? xdef?))))

(define (parse-string-list . str-l)
  (parse-string (apply string-append str-l)))

(use-modules (nyacc lang arch-info))
(use-modules (system foreign))
(use-modules (ice-9 match))

(define (fold p s l)
  (let loop ((s s) (l l))
    (if (null? l) s (loop (p (car l) s) (cdr l)))))

(define-syntax-rule (pass-if mesg expr)
  (begin
    (display mesg)
    (newline)
    (pp expr)))

(when #f
  (let* ((code "int foo = sizeof(int(*)());")
	 (tree (or (parse-string code) (error "parse failed")))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "foo")))
    (pp udecl)
    (sf "orig:  ~A\n" code)
    (sf "   =>") (pp99 udecl)))

(when #f
  (let* ((code (string-append
		"typedef int *bar_t;\n"
		;;"bar_t foo(bar_t (*)(bar_t));\n" ;; <= param-list broken
		"int foo(bar_t);\n"
		))
	 (tree (or (parse-string code) (error "parse failed")))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "foo"))
	 (xdecl (expand-typerefs udecl udict))
	 )
    (pp udecl)
    (pp xdecl)
    ;;(pp99 udecl)
    ;;(pp99 xdecl)
    #t))

(when #t
    (pass-if "evaluate sizeof(type)"
    (fold
     (lambda (case status)
       (let* ((code (string-append (car case) " int x = sizeof(foo_t);"))
	      (tree (parse-string code))
	      (udict (c99-trans-unit->udict tree))
	      (udecl (assoc-ref udict "x"))
	      (sotex (sx-ref* udecl 2 2 1))) ; (sizeof-type (type-name ...))
	 (call-with-values
	     (lambda () (eval-sizeof-type sotex udict))
	   (lambda (size align)
	     (and status (= size (cadr case)) (= align (cddr case)))))))
     #t
     '(("typedef int foo_t;" . (4 . 4))
       ("typedef long foo_t;" . (8 . 8))
       ("typedef int foo_t[5];" . (20 . 4))
       ("typedef struct { int x; } foo_t;" . (4 . 4))
       ("typedef struct { int x,y; } foo_t;" . (8 . 4))
       ("typedef struct { int x; double y; } foo_t;" . (16 . 8))
       ("typedef struct { double x; int y; } foo_t;" . (16 . 8))
       ("typedef struct { int x,y; double z; } foo_t;" . (16 . 8))
       ("typedef struct { int x; double z; void *p; } foo_t;" . (24 . 8))
       ("typedef union { double x; int y; } foo_t;" . (8 . 8))
       ("typedef union { double x; int y[3]; } foo_t;" . (16 . 8))
       ("typedef enum { FOO=1, BAR=2 } foo_t;" . (4 . 4))
       )))
)

;; --- last line ---
