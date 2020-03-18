;; examples/nyacc/lang/c99/tryit.scm

;; Copyright (C) 2020 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

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

(define *cpp-defs* (get-gcc-cpp-defs))
(define *inc-dirs* (get-gcc-inc-dirs))
(define *inc-help* c99-def-help)

(define *mode* 'code)
(define *debug* #f)
(define *xdef?* (lambda (name mode) (memq mode '(code decl))))

(define* (parse-file file #:key cpp-defs inc-dirs mode debug)
  (with-input-from-file file
    (lambda ()
      (parse-c99 #:cpp-defs (or cpp-defs *cpp-defs*)
		 #:inc-dirs (or inc-dirs *inc-dirs*)
		 #:inc-help *inc-help*
		 #:mode (or mode *mode*)
		 #:debug (or debug *debug*)
		 #:show-incs #f
		 #:xdef? *xdef?*))))

(define* (parse-string str #:key cpp-defs inc-dirs mode debug)
  (with-input-from-string str
    (lambda ()
      (parse-c99 #:cpp-defs (or cpp-defs *cpp-defs*)
		 #:inc-dirs (or inc-dirs *inc-dirs*)
		 #:inc-help *inc-help*
 		 #:mode (or mode *mode*)
		 #:debug (or debug *debug*)
		 #:show-incs #f
		 #:xdef? *xdef?*))))

(define (parse-string-list . str-l)
  (parse-string (apply string-append str-l)))

(use-modules (nyacc lang arch-info))
(use-modules ((system foreign) #:prefix ffi:))
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

(use-modules (sxml fold))

(define (remove-comments tree)
  (define (fD seed tree) '())
  (define (fU seed kseed tree)
    (sx-match tree
      ((comment ,text) seed)
      (,_ (if (pair? seed) (cons (reverse kseed) seed) (reverse kseed)))))
  (define (fH seed node) (cons node seed))
  (foldts fD fU fH '() tree))

;; def of form "ABC=123" ; rejects function types

(when #t
  (let* ((code (string-append
		"typedef struct { double d; char c; } foo_t;\n"
		"const int y[3] = { 1, 2, 3 };\n"
		"const int x = sizeof(y[0]);\n"
		;;"const foo_t *y;\n"
		;;"const int x = sizeof(*y);\n"
		;;"enum bar { A = 2, B, C = sizeof(foo_t), D };\n"
		))
	 (tree (or (parse-string code #:mode 'decl) (error "parse failed")))
	 ;;(tree (remove-comments tree))
	 (udict (c99-trans-unit->udict tree))
	 (ddict (split-cpp-defs (get-gcc-cpp-defs)))
	 (ddict '())
	 (udecl (assoc-ref udict "x"))
	 (szof (sx-ref* udecl 2 2 1))
	 )
    ;;(pp udecl)
    ;;(sf "expr:\n") (pp szof)
    ;;(sf "sizeof(*y) = ~S\n" (eval-c99-cx szof udict))
    (pp (eval-c99-cx szof udict))
    ;;(sf "~S\n" (eval-c99-cx `(ident "x") udict))
    #t))

;; --- last line ---
