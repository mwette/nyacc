;; calc.scm - calculator
;;
;; Copyright (C) 2015,2017,2019 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

;;; Usage:

;; $ guile calc.scm
;; > 2 + 2
;; 4

;;; Code:

(use-modules (nyacc lalr))
(use-modules (nyacc lex))
(use-modules (nyacc parse))

(define (next) (newline) (display "> ") (force-output))

(define spec
  (lalr-spec
   (prec< (left "+" "-") (left "*" "/"))
   (start stmt-list)
   (grammar
    (stmt-list
     (stmt)
     (stmt-list "\n" stmt))
    (stmt
     (expr ($$ (display $1) (next))))
    (expr
     ($empty ($$ ""))
     (expr "+" expr ($$ (+ $1 $3)))
     (expr "-" expr ($$ (- $1 $3)))
     (expr "*" expr ($$ (* $1 $3)))
     (expr "/" expr ($$ (/ $1 $3)))
     ($fixed ($$ (string->number $1)))
     ($float ($$ (string->number $1)))
     ("(" expr ")" ($$ $2))))))

(define mach (make-lalr-machine spec))

(define mtab (lalr-match-table mach))

(define gen-lexer (make-lexer-generator mtab #:space-chars " \t"))
;; ^ Define space chars to not include "\n"

(define raw-parse (make-lalr-parser mach))

(define (parse) (raw-parse (gen-lexer)))

(next)
(parse)

;; --- last line ---
