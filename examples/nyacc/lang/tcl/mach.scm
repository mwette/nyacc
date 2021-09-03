;;; lang/tcl/mach.scm
;;;
;;; Copyright (C) 2018,2021 Matthew R. Wette
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, see <http://www.gnu.org/licenses/>.

(define-module (nyacc lang tcl mach)
  #:export (tcl-expr-spec tcl-expr-mach gen-tcl-files)
  #:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc lang util)
  #:use-module (rnrs arithmetic bitwise))

(define tcl-expr-spec
  (lalr-spec
   (notice (string-append "Copyright (C) 2018 Matthew R. Wette"
			  license-lgpl3+))
   ;;(expect 0)
   (start conditional-expression)
   (grammar
    (conditional-expression
     (logical-or-expression)
     (logical-or-expression "?" logical-or-expression
			    ":" conditional-expression
			    ($$ `(cond-expr ,$1 ,$3 ,$5))))
    (logical-or-expression
     (logical-and-expression)
     (logical-or-expression "||" logical-and-expression ($$ `(or ,$1 ,$3))))
    (logical-and-expression
     (bitwise-or-expression)
     (logical-and-expression "&&" bitwise-or-expression ($$ `(and ,$1 ,$3))))
    (bitwise-or-expression
     (bitwise-xor-expression)
     (bitwise-or-expression "|" bitwise-xor-expression
			    ($$ `(bitwise-or ,$1 ,$3))))
    (bitwise-xor-expression
     (bitwise-and-expression)
     (bitwise-xor-expression "^" bitwise-and-expression
			     ($$ `(bitwise-xor ,$1 ,$3))))
    (bitwise-and-expression
     (equality-expression)
     (bitwise-and-expression "&" equality-expression
			     ($$ `(bitwise-and ,$1 ,$3))))
    (equality-expression
     (relational-expression)
     (equality-expression "==" relational-expression ($$ `(eq ,$1 ,$3)))
     (equality-expression "!=" relational-expression ($$ `(ne ,$1 ,$3))))
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
     ("~" unary-expression ($$ `(bitwise-not ,$2)))
     ("++" unary-expression ($$ `(pre-inc ,$2)))
     ("--" unary-expression ($$ `(pre-dec ,$2))))
    (postfix-expression
     (primary-expression)
     (postfix-expression "++" ($$ `(post-inc ,$1)))
     (postfix-expression "--" ($$ `(post-dec ,$1))))
    (primary-expression
     ($ident ($$ `(ident ,$1)))
     ($fixed ($$ `(fixed ,$1)))		; integer literal
     ($float ($$ `(float ,$1)))		; float literal
     ($chlit ($$ `(char ,$1)))		; char literal
     ;;($chlit/L ($$ `(char (@ (type "wchar_t")) ,$1)))
     ;;($chlit/u ($$ `(char (@ (type "char16_t")) ,$1)))
     ;;($chlit/U ($$ `(char (@ (type "char32_t")) ,$1)))
     ("(" expression-list ")" ($$ $2)))
    (expression-list
     (conditional-expression)
     (expression-list "," conditional-expression ($$ $3)))
    )))

(define tcl-expr-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine tcl-expr-spec))))

;;; =====================================

;; gen-tcl-files [dir] => #t
(define (gen-tcl-files . rest)
  (define (lang-dir path)
    (if (pair? rest) (string-append (car rest) "/" path) path))
  (define (xtra-dir path)
    (lang-dir (string-append "mach.d/" path)))

  (write-lalr-actions tcl-expr-mach (xtra-dir "expr-act.scm.new") 
   #:prefix "tcl-expr-")
  (write-lalr-tables tcl-expr-mach (xtra-dir "expr-tab.scm.new")
    #:prefix "tcl-expr-")
  (let ((a (move-if-changed (xtra-dir "expr-act.scm.new")
			    (xtra-dir "expr-act.scm")))
	(b (move-if-changed (xtra-dir "expr-tab.scm.new")
			    (xtra-dir "expr-tab.scm"))))
    (or a b)))

;; --- last line ---
