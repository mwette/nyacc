;; nyacc/lang/c99/c99-06.test		-*- scheme -*-
;;
;; Copyright (C) 2017 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

;; test C99 munge utilities

(add-to-load-path (string-append (getcwd) "/../../../../module/"))
(add-to-load-path (string-append (getcwd) "/../../../../test-suite/"))

(define-module (c99-06)
  #:use-module (nyacc lang c99 parser)
  #:use-module (nyacc lang c99 util1)
  #:use-module (nyacc lang c99 util2)
  #:use-module ((sxml xpath) #:select (sxpath))
  #:use-module (test-suite lib))

(define incs '("exam.d"))

(define (parse-string str)
  (with-input-from-string str
    (lambda ()
      (parse-c99 #:inc-dirs incs #:mode 'decl #:inc-help c99-std-help))))

(define (parse-file file)
  (with-input-from-file file
    (lambda ()
      (parse-c99 #:inc-dirs incs #:mode 'decl))))

;; parser test
(with-test-prefix "nyacc/c99-06, munging"

  ;; parse with include file
  (pass-if "expand-typerefs in function declarator"
    (let* ((code (string-append
		  "typedef int *foo_t;\n"
		  "typedef double hmm_t[3];\n"
		  "int bar(foo_t (*baz)(hmm_t y));\n"))
	   (tree (parse-string code))
	   (udict (c99-trans-unit->udict tree))
	   (decl (and=> ((sxpath '((decl 3))) tree) car))
	   (xdecl (expand-typerefs decl udict))
	   (xcode "int bar(int *(*baz)(double y[3]));\n") ;; what to expect
	   )
    (equal?
     xdecl
     '(decl (decl-spec-list (type-spec (fixed-type "int")))
	    (init-declr-list
	     (init-declr
	      (ftn-declr
	       (ident "bar")
	       (param-list
                (param-decl
		 (decl-spec-list (type-spec (fixed-type "int")))
		 (init-declr
		  (ptr-declr
		   (pointer)
		   (ftn-declr
		    (scope (ptr-declr (pointer) (ident "baz")))
		    (param-list
		     (param-decl
		      (decl-spec-list
		       (type-spec (float-type "double")))
		      (init-declr
		       (array-of
			(ident "y")
			(p-expr (fixed "3"))))))))))))))))))

  )

;; --- last line ---
