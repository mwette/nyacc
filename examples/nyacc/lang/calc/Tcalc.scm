;; Tcalc.scm - calculator
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lang calc parser))
(use-modules (nyacc lalr))
(use-modules (nyacc parse))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

;; Generates a file with grammar and automaton, like bison's gram.output.
(with-output-to-file "lang.txt.new"
  (lambda ()
    (pp-lalr-grammar calc-spec)
    (pp-lalr-machine calc-mach)
    ))

#;(with-output-to-file "gram.y.new"
  (lambda () (lalr->bison calc-spec)))

;; Generate tables for a standalone parser.
;;(write-lalr-actions js-mach "actions.scm.new")
;;(write-lalr-tables js-mach "tables.scm.new")

(use-modules (nyacc lang calc compiler))

(define exp (with-input-from-string "a = 1 + 2 * 3\n"
	      (lambda () (calc-parse #:debug #t))))
(pretty-print exp)

;; --- last line
