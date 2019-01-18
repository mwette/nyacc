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

;; See the NYACC User's Manual for further information.

;;; Code:

(use-modules (nyacc lalr))
(use-modules (nyacc lex))
(use-modules (nyacc parse))

(define (next) (newline) (display "> ") (force-output))

(define spec
  (lalr-spec
   (prec< (left "+" "-") (left "*" "/"))
   (start prog)
   (grammar
    (prog
     (stmt-list))
    (stmt-list
     (stmt)
     (stmt-list "\n" stmt))
    (stmt
     ($empty ($$ (next)))
     (expr ($$ (display $1) (next)))
     (assn ($$ (module-define! (current-module) (car $1) (cdr $1))
	       (display (cdr $1)) (next))))
    (expr
     (expr "+" expr ($$ (+ $1 $3)))
     (expr "-" expr ($$ (- $1 $3)))
     (expr "*" expr ($$ (* $1 $3)))
     (expr "/" expr ($$ (/ $1 $3)))
     ($fixed ($$ (string->number $1)))
     ($float ($$ (string->number $1)))
     ($ident ($$ (module-ref (current-module) (string->symbol $1))))
     ("(" expr ")" ($$ $2)))
    (assn
     ($ident "=" expr ($$ (cons (string->symbol $1) $3)))))))


(define mach (make-lalr-machine spec))

(define mtab (lalr-match-table mach))

(define gen-lexer (make-lexer-generator mtab #:space-chars " \t"))
;; ^ Define space chars to not include "\n"

(define raw-parse (make-lalr-parser mach))

(define (parse) (raw-parse (gen-lexer) #:debug #f))

(next)
(parse)

;; --- last line ---
