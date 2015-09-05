;;; lang/c/cppgen.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
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

;; C preprocessor expression parser generator

(define-module (lang c cppgen)
  #:export (cpp-spec cpp-mach parse-cpp-expr eval-cpp-expr)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (lang util)
  #:use-module ((srfi srfi-43) #:select (vector-map))
  #:use-module (rnrs arithmetic bitwise)
  )

(define cpp-spec
  (lalr-spec
   (notice lang-crn-lic)
   (expect 0)
   (start conditional-expression)
   (grammar
    (conditional-expression
     (logical-or-expression)
     (logical-or-expression "?" logical-or-expression ":" conditional-expression
			    ($$ `(cond-expr ,$1 ,$3 ,$5))))
    (logical-or-expression
     (logical-and-expression)
     (logical-or-expression "||" logical-and-expression ($$ `(or ,$1 ,$3))))
    (logical-and-expression
     (bitwise-or-expression)
     (logical-and-expression "&&" bitwise-or-expression ($$ `(and ,$1 ,$3))))
    (bitwise-or-expression
     (bitwise-xor-expression)
     (bitwise-or-expression "|" bitwise-xor-expression ($$ `(bw-or ,$1 ,$3))))
    (bitwise-xor-expression
     (bitwise-and-expression)
     (bitwise-xor-expression "^" bitwise-and-expression ($$ `(bw-xor ,$1 ,$3))))
    (bitwise-and-expression
     (equality-expression)
     (bitwise-and-expression "&" equality-expression ($$ `(bw-and ,$1 ,$3))))
    (equality-expression
     (relational-expression)
     (equality-expression "==" relational-expression ($$ `(equal ,$1 ,$3)))
     (equality-expression "!=" relational-expression ($$ `(noteq ,$1 ,$3))))
    (relational-expression
     (shift-expression)
     (relational-expression "<" shift-expression ($$ `(lt ,$1 ,$3)))
     (relational-expression "<=" shift-expression ($$ `(le ,$1 ,$3)))
     (relational-expression ">" shift-expression ($$ `(gt ,$1 ,$3)))
     (relational-expression ">=" shift-expression ($$ `(ge ,$1 ,$3))))
    (shift-expression
     (additive-expression)
     (shift-expression "<<" additive-expression ($$ `(lshift ,$1 ,$3)))
     (shift-expression ">>" additive-expression ($$ `(rshift ,$1 ,$3))))
    (additive-expression
     (multiplicative-expression)
     (additive-expression "+" multiplicative-expression ($$ `(add ,$1 ,$3)))
     (additive-expression "-" multiplicative-expression ($$ `(sub ,$1 ,$3))))
    (multiplicative-expression
     (unary-expression)
     (multiplicative-expression "*" unary-expression ($$ `(mul ,$1 ,$3)))
     (multiplicative-expression "/" unary-expression ($$ `(div ,$1 ,$3)))
     (multiplicative-expression "%" unary-expression ($$ `(mod ,$1 ,$3))))
    (unary-expression
     (postfix-expression)
     ("-" unary-expression ($$ `(neg ,$2)))
     ("+" unary-expression ($$ `(pos ,$2)))
     ("!" unary-expression ($$ `(not ,$2)))
     ("~" unary-expression ($$ `(bw-not ,$2)))
     ("++" unary-expression ($$ `(pre-inc ,$2)))
     ("--" unary-expression ($$ `(pre-dec ,$2))))
    (postfix-expression
     (primary-expression)
     (postfix-expression "++" ($$ `(post-inc ,$1)))
     (postfix-expression "--" ($$ `(post-dec ,$1))))
    (primary-expression
     ('$ident ($$ `(ident ,$1)))
     ('$fixed ($$ `(fixed ,$1)))	; integer-constant
     ('$ch-lit ($$ `(char ,$1)))	; char-constant
     ("defined" "(" '$ident ")" ($$ `(defined ,$3)))
     ("(" expression-list ")" ($$ $2)))
    (expression-list
     (conditional-expression)
     (expression-list "," conditional-expression ($$ $3)))
    )))

;;; This is copied to cpp.scm
(define (eval-cpp-expr tree dict)
  (letrec
      ((tx (lambda (tr ix) (list-ref tr ix)))
       (tx1 (lambda (tr) (tx tr 1)))
       (ev (lambda (ex ix) (eval-expr (list-ref ex ix))))
       (ev1 (lambda (ex) (ev ex 1)))
       (ev2 (lambda (ex) (ev ex 2)))
       (ev3 (lambda (ex) (ev ex 3)))
       (parse-and-eval
	(lambda (str)
	  (if (not (string? str)) (throw 'error))
	  (let ((idtr (with-input-from-string str parse-cpp-expr)))
	    (eval-cpp-expr idtr dict))))
       (eval-expr
	(lambda (tree)
	  (case (car tree)
	    ((ident) (parse-and-eval (assoc-ref dict (tx1 tree))))
	    ((fixed) (string->number (tx1 tree)))
	    ((char) (char->integer (tx1 tree)))
	    ((defined) (if (assoc-ref dict (tx1 tree)) 1 0))
	    ;;
	    ((pre-inc post-inc) (1+ (ev1 tree)))
	    ((pre-dec post-dec) (1- (ev1 tree)))
	    ((pos) (ev1 tree))
	    ((neg) (- (ev1 tree)))
	    ((bw-not) (bitwise-not (ev1 tree)))
	    ((not) (if (zero? (ev1 tree)) 1 0))
	    ((mul) (* (ev1 tree) (ev2 tree)))
	    ((div) (/ (ev1 tree) (ev2 tree)))
	    ((mod) (modulo (ev1 tree) (ev2 tree)))
	    ((add) (+ (ev1 tree) (ev2 tree)))
	    ((sub) (- (ev1 tree) (ev2 tree)))
	    ((lshift) (bitwise-arithmetic-shift-left (ev1 tree) (ev2 tree)))
	    ((rshift) (bitwise-arithmetic-shift-right (ev1 tree) (ev2 tree)))
	    ((lt) (if (< (ev1 tree) (ev2 tree)) 1 0))
	    ((le) (if (<= (ev1 tree) (ev2 tree)) 1 0))
	    ((gt) (if (> (ev1 tree) (ev2 tree)) 1 0))
	    ((ge) (if (>= (ev1 tree) (ev2 tree)) 1 0))
	    ((equal) (if (= (ev1 tree) (ev2 tree)) 1 0))
	    ((noteq) (if (= (ev1 tree) (ev2 tree)) 0 1))
	    ((bw-or) (bitwise-ior (ev1 tree) (ev2 tree)))
	    ((bw-xor) (bitwise-xor (ev1 tree) (ev2 tree)))
	    ((bw-and) (bitwise-and (ev1 tree) (ev2 tree)))
	    ((or) (if (and (zero? (ev1 tree)) (zero? (ev2 tree))) 0 1))
	    ((and) (if (or (zero? (ev1 tree)) (zero? (ev2 tree))) 0 1))
	    ((cond-expr) (if (zero? (ev1 tree)) (ev3 tree) (ev2 tree)))
	    (else (error "incomplete implementation"))))))
    (catch 'error
	   (lambda () (eval-expr tree))
	   (lambda () #f))))

(define cpp-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine cpp-spec))))
(define mtab (assq-ref cpp-mach 'mtab))
(define raw-parser (make-lalr-parser cpp-mach))
(define gen-cpp-lexer (make-lexer-generator mtab))
(define (parse-cpp-expr) (raw-parser (gen-cpp-lexer)))

;; --- last line
