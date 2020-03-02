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

(use-modules (bytestructures guile))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (rnrs bytevectors))

(define-record-type <vector&-metadata>
  (make-vector&-metadata element-descriptor)
  vector&-metadata?
  (element-descriptor vector&-metadata-element-descriptor))

(define (fh:vector& descriptor)
  (define element-size (bytestructure-descriptor-size descriptor))
  (define size 0)
  (define alignment (bytestructure-descriptor-alignment descriptor))
  (define (unwrapper syntax? bytevector offset index)
    (if syntax? (throw 'ffi-help-error "fh:vector& syntax not supported"))
    (values bytevector (+ offset (* index element-size)) descriptor))
  (define meta (make-vector&-metadata descriptor))
  (define (getter syntax? bytevector offset)
    (if syntax? (throw 'ffi-help-error "fh:vector& syntax not supported"))
    )
  (define (setter syntax? bytevector offset)
    (if syntax? (throw 'ffi-help-error "fh:vector& syntax not supported"))
    )
  (make-bytestructure-descriptor size alignment unwrapper #f #f meta))


(define bs1 #u16(1 2 3 4 5 6 7 8))

;; --- last line ---
