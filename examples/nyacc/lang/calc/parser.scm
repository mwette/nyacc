;;; nyacc/lang/calc/parser
;;;
;;; Copyright (C) 2015,2016 Matthew R. Wette
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by 
;;; the Free Software Foundation, either version 3 of the License, or 
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nyacc lang calc parser)
  #:export (calc-parse calc-spec calc-mach)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  )

(define calc-spec
  (lalr-spec
   (prec< (left "+" "-") (left "*" "/"))
   (start stmt-list-proxy)
   (grammar

    (stmt-list-proxy
     (stmt-list "\n" ($$ `(stmt-list ,@(reverse $1)))))

    (stmt-list
     (stmt ($$ (list $1)))
     (stmt-list ";" stmt ($$ (cons $3 $1))))

    (stmt
     (ident "=" expr ($$ `(assn-stmt ,$1 ,$3)))
     (expr ($$ `(expr-stmt ,$1)))
     ( ($$ '(empty-stmt))))

    (expr
     (expr "+" expr ($$ `(add ,$1 ,$3)))
     (expr "-" expr ($$ `(sub ,$1 ,$3)))
     (expr "*" expr ($$ `(mul ,$1 ,$3)))
     (expr "/" expr ($$ `(div ,$1 ,$3)))
     ($fixed ($$ `(fixed ,$1)))
     ($float ($$ `(float ,$1)))
     ("(" expr ")" ($$ $2)))

    (ident ($ident ($$ `(ident ,$1))))
    )))

(define calc-mach
  (compact-machine
   (hashify-machine
     (make-lalr-machine calc-spec))))

(define calc-parse
  (let ((gen-lexer (make-lexer-generator (assq-ref calc-mach 'mtab)
					 #:space-chars " \t"))
	(parser (make-lalr-ia-parser calc-mach)))
    (lambda* (#:key (debug #f)) (parser (gen-lexer) #:debug debug))))

;; --- last line ---
