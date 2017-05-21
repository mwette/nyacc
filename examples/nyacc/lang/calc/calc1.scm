;; Tcalc.scm - calculator
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(add-to-load-path "../../../../module")

(use-modules (nyacc lalr))
(use-modules (nyacc lex))
(use-modules (nyacc parse))

(define spec
  (lalr-spec
   (prec< (left "+" "-") (left "*" "/"))
   (start expr)
   (grammar
    (expr
     (expr "+" expr ($$ (+ $1 $3)))
     (expr "-" expr ($$ (- $1 $3)))
     (expr "*" expr ($$ (* $1 $3)))
     (expr "/" expr ($$ (/ $1 $3)))
     ($fixed ($$ (string->number $1)))
     ($float ($$ (string->number $1)))
     ("(" expr ")" ($$ $2))))))

(define mach (make-lalr-machine spec))
(define raw-parser (make-lalr-parser mach))
(define gen-lexer (make-lexer-generator (lalr-match-table mach)))

(define (calc1-eval str)
  (with-input-from-string str
    (lambda () (raw-parser (gen-lexer)))))

(define (calc1-demo string)
  (simple-format #t "~A => ~A\n" string (calc1-eval string)))

(calc1-demo "2 + 2")

#|
;; To debug, generate the .out file and use `calc1-eval' fromn below.
(with-output-to-file "calc1.out"
  (lambda ()
    (pp-lalr-grammar calc1-mach)
    (pp-lalr-machine calc1-mach)))
|#
#|
(define (calc1-eval str)
  (with-input-from-string str
    (lambda () (raw-parser (gen-lexer) #:debug #t))))
|#

#| ;; to generate actions and tables
(let ((mach (compact-machine (hashify-machine calc1-mach))))
  (write-lalr-actions mach "calc1-act.scm")
  (write-lalr-tables mach "calc1-tab.scm"))
|#

#| ;; to see equivalent bison input file
(use-modules (nyacc export))
(with-output-to-file "calc1.y"
  (lambda () (lalr->bison calc1-mach)))
|#

;; --- last line ---
