;; Tlangm.scm - matlab
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(add-to-load-path (getcwd))
(add-to-load-path (string-append (getcwd) "/../../module"))

(use-modules (lang matlab pgen))
(use-modules (nyacc lalr))
(use-modules (nyacc lex))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(with-output-to-file "lang/matlab/lang.txt.new"
  (lambda ()
    (pp-lalr-grammar matlab-spec)
    (pp-lalr-machine matlab-mach)))
(with-output-to-file "lang/matlab/gram.y.new"
  (lambda () (lalr->bison matlab-spec)))
(write-lalr-tables matlab-mach "lang/matlab/tables.scm.new")

(define res (with-input-from-file "lang/matlab/ex1.m"
	      (lambda () (matlab-parser (gen-matlab-lexer) #:debug #f))))
(if res (pretty-print res))

;; --- last line
