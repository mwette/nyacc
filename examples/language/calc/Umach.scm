;; Umach.scm - update calculator automata
;;
;; Copyright (C) 2018 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lang calc mach))
(use-modules (nyacc lalr))

(gen-calc-files)
(compile-file "parser.scm")

(with-output-to-file "full.txt"
  (lambda ()
    (pp-lalr-grammar full-spec)
    (pp-lalr-machine full-mach)))
(with-output-to-file "stmt.txt"
  (lambda ()
    (pp-lalr-grammar stmt-spec)
    (pp-lalr-machine stmt-mach)))

;; to see equivalent bison input file
;;(use-modules (nyacc export))
;;(with-output-to-file "calc.y"
;;  (lambda () (lalr->bison spec)))


;; --- last line ---
