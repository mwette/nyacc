;; Tmach.scm - test C99: CPP and parser
;;
;; Copyright (C) 2015,2016 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lang c99 mach))
(use-modules (nyacc lang c99 pprint))	; pretty-print-c99
(use-modules (nyacc lang c99 util1))	; remove-inc-trees
(use-modules (nyacc lang util))		; move-if-changed
(use-modules (nyacc lalr))
(use-modules (nyacc util))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

;; test def parser: show parse tree
(when #t
  (let* ((file "exam.d/ex10.c")
	 (defs '(("arch" . "x86_64"))) 
	 (incs  '("."))
	 (parse (lambda () (dev-parse-c
			     #:cpp-defs defs #:inc-dirs incs
			     #:debug #f #:mode 'file)))
	 (sx (with-input-from-file file parse))
	 ;;(sx (remove-inc-trees sx))
         )
    (pretty-print sx)
    #t))



;; --- last line ---
