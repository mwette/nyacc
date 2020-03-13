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

(define (ink-keeper? tree filter)
  (sx-match tree
    ((cpp-stmt (include (@ (path ,path)) ,spec ,tree))
     (and (if (procedure? filter) (filter spec path) filter) tree))
    ((cpp-stmt (include-next (@ (path ,path)) ,spec ,tree))
     (and (if (procedure? filter) (filter spec path) filter) tree))
    ((cpp-stmt . ,rest) #f)
    (,_ #f)))

(define* (x99-trans-unit->ddict tree
				#:optional (ddict '())
				#:key inc-filter skip-fdefs)
  (define (can-def-stmt tree)
    (sx-match tree
      ((cpp-stmt (define (name ,name) (repl ,repl)))
       (cons name repl))
      ((cpp-stmt (define (name ,name) (args . ,args) (repl ,repl)))
       (cons name (cons args repl)))
      (,_ #f)))

  (if (pair? tree)
      (fold
       (lambda (tree ddict)
	 (cond
	  ((can-def-stmt tree) => (lambda (def) (cons def ddict)))
	  ((ink-keeper? tree inc-filter) =>
	   (lambda (tree)
	     (x99-trans-unit->ddict tree ddict #:inc-filter inc-filter)))
	  (else
	   ddict)))
       ddict (sx-tail tree))
      ddict))

;; def of form "ABC=123" ; rejects function types

(when #t
  (let* ((code (string-append
		"#include <limits.h>\n"
		"#define ABC ULONG_MAX\n"
		"#define FOO(X) 1\n"
		"int x = 1;\n"
		))
	 (tree (or (parse-string code #:mode 'decl) (error "parse failed")))
	 (tree (remove-comments tree))
	 (udict (c99-trans-unit->udict tree))
	 (ddict (split-cpp-defs (get-gcc-cpp-defs)))
	 (ddict (x99-trans-unit->ddict tree ddict #:inc-filter #t))
	 )
    (newline)
    (sf "ABC=~S\n" (assoc-ref ddict "ABC"))
    ;;(sf "   =~S\n" (eval-c99-cx '(ident "ABC")))
    (sf "   =~S\n" (assoc-ref ddict "ULONG_MAX"))
    (sf "   =~S\n" (assoc-ref ddict "LONG_MAX"))
    (sf "   =~S\n" (assoc-ref ddict "__LONG_MAX__"))
    ;;(pp (get-gcc-cpp-defs))
    (sf "(expand-cpp-name \"ABC\") => ~S\n" (expand-cpp-name "ABC" ddict))
    (sf "(eval-c99-cx \"ABC\") => ~S\n" (eval-c99-cx '(ident "ABC") udict ddict))
    (newline)
    ;;(pp ddict)
    (sf "lesson: GOTTA ADD GCC DEFS to dicts\n")
    #t))

(when #f
  (let* ((tree '(a (@ (path "abc")) "d" (e "f")))
	 )
    (sx-match tree
      ((a (@ (path ,path)) ,text ,tree)
       (sf "got it path=~S text=~S\n" path text))
      (,_
       (sf "missed\n")))))

;; --- last line ---
