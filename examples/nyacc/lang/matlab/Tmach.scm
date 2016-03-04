;; nyacc/lang/matlab/Tmach.scm - matlab
;;
;; Copyright (C) 2015,2016 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lang matlab mach))
(use-modules (nyacc lang util))
(use-modules (nyacc lalr))
(use-modules (nyacc util))
;;(use-modules (nyacc lex))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(when #t
  (gen-matlab-files)
  (system "touch parser.scm"))

(when #t
  (with-output-to-file "lang.txt"
    (lambda ()
      (pp-lalr-grammar matlab-spec)
      (pp-lalr-machine matlab-mach))))

(when #t
  (with-output-to-file "gram.y"
    (lambda () (lalr->bison matlab-spec))))

(when #t
  (let ((res (with-input-from-file "ex1.m"
	       (lambda () (dev-parse-ml #:debug #f)))))
    (pretty-print res)
    #t))

;; --- last line ---
