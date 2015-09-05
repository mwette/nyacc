;; calc.scm - simple calculator
;;
;; Copyright (C) 2015 Matthew R. Wette
;;
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(add-to-load-path (string-append (getcwd) "/../../module"))

(use-modules (nyacc lalr))
(use-modules (nyacc lex))

(define calc-spec
  (lalr-spec
   (prec< (left "+" "-") (left "*" "/"))
   (start expr)
   (grammar
    (expr
     (expr "+" expr ($$ (+ $1 $3)))
     (expr "-" expr ($$ (- $1 $3)))
     (expr "*" expr ($$ (* $1 $3)))
     (expr "/" expr ($$ (/ $1 $3)))
     ('$fixed ($$ (string->number $1))))
    )))

(define calc-mach (make-lalr-machine calc-spec))
;;(define calc-mach (compact-machine calc-mach))
(define calc-mach (hashify-machine calc-mach))

;;(pp-lalr-grammar calc-spec)
;;(pp-lalr-machine calc-mach)

(define parse-expr
  (let ((gen-lexer (make-lexer-generator (assq-ref calc-mach 'mtab)))
	(calc-parser (make-lalr-parser calc-mach)))
    (lambda () (calc-parser (gen-lexer)))))

(define res (with-input-from-string "1 + 4 / 2 * 3 - 5" parse-expr))
(simple-format #t "~A\n" (if (= res 2) "PASS" "FAIL")) ;; expect: 2

;; --- last line
