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
     ("*" $error)
     ($fixed ($$ (string->number $1)))
     ($float ($$ (string->number $1)))
     ("(" expr ")" ($$ $2))))))

(define calc-mach (make-lalr-machine simple-spec))
(define gen-lexer (make-lexer-generator (assq-ref calc-mach 'mtab)))
(define parse (make-lalr-parser calc-mach))

(define demo-string "2 + 2")

(simple-format #t "~A => ~A\n"
	       demo-string
	       (with-input-from-string demo-string
		 (lambda () (parse (gen-lexer)))))

#|
(with-output-to-file "lang.txt"
  (lambda ()
    (pp-lalr-grammar simple-mach)
    (pp-lalr-machine simple-mach)))
(use-modules (nyacc export))
(with-output-to-file "gram.y"
  (lambda ()
    (lalr->bison simple-mach)))
|#

;; --- last line ---
