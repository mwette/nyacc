;; calc2.scm - calculator
;;
;; Copyright (C) 2015,2017 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(add-to-load-path (string-append (getcwd) "/../../../../module"))

(use-modules (nyacc lalr))
(use-modules (nyacc lex))
(use-modules (nyacc parse))

(define (next) (newline) (display "> ") (force-output))

(define calc2-spec
  (lalr-spec
   (prec< (left "+" "-") (left "*" "/"))
   (start stmt-list)
   (grammar
    (stmt-list
     (stmt)
     (stmt-list stmt))
    (stmt
     (expr ($$ (display $1) (next)) "\n"))
    (expr
     ($empty)
     (expr "+" expr ($$ (+ $1 $3)))
     (expr "-" expr ($$ (- $1 $3)))
     (expr "*" expr ($$ (* $1 $3)))
     (expr "/" expr ($$ (/ $1 $3)))
     ($fixed ($$ (string->number $1)))
     ($float ($$ (string->number $1)))
     ("(" expr ")" ($$ $2))))))

(define calc2-mach (make-lalr-machine calc2-spec))
(define match-table (lalr-match-table calc2-mach))
(define parse (make-lalr-ia-parser calc2-mach))
(define gen-lexer (make-lexer-generator match-table #:space-chars " \t"))

(next)
(parse (gen-lexer))

;; --- last line ---
