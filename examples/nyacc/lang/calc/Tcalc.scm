;; Tcalc.scm - calculator
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lalr))
(use-modules (nyacc lex))
(use-modules (nyacc parse))

(define simple-spec
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

(define simple-mach (make-lalr-machine simple-spec))

(define match-table (assq-ref simple-mach 'mtab))

(define gen-lexer (make-lexer-generator match-table))

(define parse (make-lalr-parser simple-mach))

(define demo-string "2 + 2")

(simple-format #t "~A => ~A\n"
	       demo-string
	       (with-input-from-string demo-string
		 (lambda () (parse (gen-lexer)))))

;; --- last line ---
