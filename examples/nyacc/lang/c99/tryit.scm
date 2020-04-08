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
(use-modules (nyacc lang c99 munge-base))
(use-modules (nyacc lang c99 cpp))
(use-modules (nyacc lang c99 util))
(use-modules (nyacc lang sx-util))
(use-modules (nyacc lang util))
(use-modules (nyacc lex))
(use-modules (nyacc util))
(use-modules (sxml fold))
(use-modules (ice-9 pretty-print))

(define (sf fmt . args) (apply simple-format #t fmt args))
(define pp pretty-print)
(define ppin (lambda (sx) (pretty-print sx #:per-line-prefix "  ")))
(define pp99 (lambda (sx) (pretty-print-c99 sx #:per-line-prefix "  ")))
(define cep current-error-port)
(define (sferr fmt . args) (apply simple-format (cep) fmt args))
(define (pperr sx) (pretty-print sx (cep) #:per-line-prefix "  "))
(define (ppe99 sx) (pretty-print-c99 sx (cep) #:per-line-prefix "  "))

(define *cpp-defs* (get-gcc-cpp-defs))
(define *inc-dirs* (get-gcc-inc-dirs))
(define *inc-help* c99-def-help)

(define *mode* (make-parameter 'code))
(define *debug* (make-parameter #f))
(define *xdef?* (lambda (name mode) (memq mode '(code decl))))

(define* (parse-file file #:key cpp-defs inc-dirs mode debug)
  (with-input-from-file file
    (lambda ()
      (parse-c99 #:cpp-defs (or cpp-defs *cpp-defs*)
		 #:inc-dirs (or inc-dirs *inc-dirs*)
		 #:inc-help *inc-help*
		 #:mode (or mode (*mode*))
		 #:debug (or debug (*debug*))
		 #:show-incs #f
		 #:xdef? *xdef?*))))

(define* (parse-string str
		       #:optional (tyns '())
		       #:key cpp-defs inc-dirs mode debug)
  (with-input-from-string str
    (lambda ()
      (parse-c99 tyns
		 #:cpp-defs (or cpp-defs *cpp-defs*)
		 #:inc-dirs (or inc-dirs *inc-dirs*)
		 #:inc-help *inc-help*
 		 #:mode (or mode (*mode*))
		 #:debug (or debug (*debug*))
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

(define (remove-comments tree)
  (define (fD seed tree) '())
  (define (fU seed kseed tree)
    (sx-match tree
      ((comment ,text) seed)
      (,_ (if (pair? seed) (cons (reverse kseed) seed) (reverse kseed)))))
  (define (fH seed node) (cons node seed))
  (foldts fD fU fH '() tree))

(define (fix-tree tree)
  (define (fD seed tree) '())
  (define (fU seed kseed tree)
    (case (car tree)
      ((include)
       (let ((t (reverse (cdr kseed))))
	 (if (pair? seed) (cons t seed) t)))
      ((comment) seed)
      (else
       (let ((t (reverse kseed)))
	 (if (pair? seed) (cons t seed) t)))))
  (define (fH seed node) (cons node seed))
  (foldts fD fU fH '() tree))

(when #t
  (let* ((code "int foo = sizeof(int(*)());")
	 (tree (or (parse-string code) (error "parse failed")))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "foo")))
    (pp udecl)
    (sf "orig:  ~A\n" code)
    (sf "   =>") (pp99 udecl)))

;; ffi-help patterns:
;; Figure out how to have ffi-help print message when new pattern shows up.
;;
;; typedef struct foo *bar_t;
;; struct foo; typedef struct foo *bar_t; stuct foo { int a; };
;; typedef struct foo bar_t; struct foo { int a; }; typedef bar_t *baz_t;
;; struct foo { int a; }; typedef struct foo bar_t;
;; struct foo { int a; }; typedef struct foo *bar_t;

;; struct foo; int baz(struct foo*); 

;; case 1
;; typedef struct foo *bar_t; struct foo { int a; }; =>
;; (define struct-foo-desc 'void)
;; (define bar_t (fh:pointer (delay struct-foo-desc)))
;; (define-ffi-pointer-type bar_t struct-foo-desc bar_t? make-bar_t)
;;
;; (set! struct-foo-desc (bs:struct (list `(a ,int))))
;; (define-fh-compound-type struct-foo struct-foo-desc
;;                          struct-foo? make-struct-foo)
;; (fh-ref-deref! bar_t* make-bar_t* struct-foo make-struct-foo)

;; --- last line ---
