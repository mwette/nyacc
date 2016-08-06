;; Tmach.scm - demo C99: CPP and parser
;;
;; Copyright (C) 2015,2016 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

;;(use-modules (nyacc lang c99 mach))
(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 pprint))	; pretty-print-c99
(use-modules (nyacc lang c99 util1))	; remove-inc-trees
(use-modules (nyacc lang c99 util2))	; tree->udict
(use-modules (nyacc lang util))		; move-if-changed
;;(use-modules (nyacc lalr))
(use-modules (nyacc util))
;;(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

;; test def parser: show parse tree
(when #t
  (let* ((file "exam.d/ex01.c")
	 (defs '(("arch" . "x86_64"))) 
	 (incs  '("exam.d"))
	 (parse (lambda () (parse-c
			    #:cpp-defs defs #:inc-dirs incs
			    #:debug #f #:mode 'file #:xdef? #f)))
	 (sx (with-input-from-file file parse))
	 #|
	 (dd (tree->udict sx))
	 (dt (assoc-ref dd "foo_t"))
	 (d0 (assoc-ref dd "abc"))
         (d1 (stripdown d0 dd))
	 (u1 (udecl->mspec d1))
	 (fl (cadadr u1))
	 |#
         )
    #;(for-each
     (lambda (fld)
       (let ((mspec (udecl->mspec fld dd))
	     (mspec/c (udecl->mspec/comm fld dd)))
	 ;;(pretty-print mspec)
	 (pretty-print mspec/c)
	 ))
    (cdr fl))
    (pretty-print-c99 sx)
    ;;(pretty-print sx)
    #t))



;; --- last line ---
