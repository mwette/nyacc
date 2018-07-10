;;; nyacc/lang/javascript/pprint.scm

;; Copyright (C) 2016-2018 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;; javascript pretty-printer

(define-module (nyacc lang javascript pprint)
  #:export (pretty-print-js)
  #:use-module ((srfi srfi-1) #:select (pair-for-each))
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
  )

(define op-prec
  '((delete void typeof pre-inc pre-dec pos neg not)
    (mul div mod)
    (add sub)
    (lshift rshift rrshift)
    (lt gt le ge instanceof in)
    (equal not-equal not-equal-eq)
    (bit-xor)
    (bit-or)
    (and)
    (or)
    ;; ...
    ))

(define op-assc
  '((left mul div mod add sub lshift rshift rrshift lt gt le ge)
    (right)
    (nonassoc)))

(define protect-lval? #f)
(define protect-rval? #f)
(let ((protect-expr? (make-protect-expr op-prec op-assc)))
  (set! protect-lval? (lambda (op lval) (protect-expr? 'left op lval)))
  (set! protect-rval? (lambda (op rval) (protect-expr? 'right op rval))))
	  
(define* (pretty-print-js tree #:key (indent-level 2))

  (define ppx
    (let* ((fmtr (make-pp-formatter))
	   (push-il (lambda () (fmtr 'push)))
	   (pop-il (lambda () (fmtr 'pop)))
	   (sf (lambda args (apply fmtr args)))
	   (sf-nl (lambda () (sf "\n")))
	   (ppx/p (lambda (tree) (sf "(") (ppx tree) (sf ")"))))

      (lambda (tree)
	(case (car tree)


	  ((NullLiteral) (sf "null"))
	  ((BooleanLiteral) (sf "~A" (cadr tree)))
	  ((NumericLiteral) (sf "~A" (cadr tree)))
	  ((StringLiteral) (sf "~S" (cadr tree)))


	  ;; Identifier
	  ((Identifier) (sf "~A" (cadr tree)))

	  ;; PrimaryExpression
	  ((PrimaryExpression) (ppx (cadr tree)))

	  ;; ArrayLiteral
	  ;; ElementList
	  ;; Elision
	  ;; ObjectLiteral
	  ;; PropertyNameAndValueList 

	  ;; aoo-ref
	  ((aoo-ref)
	   (ppx (sx-ref tree 1)) (sf "[") (ppx (sx-ref tree 2)) (sf "]"))

	  ;; obj-ref
	  ;; new

	  ;; CallExpression
	  ((CallExpression)
	   (ppx (sx-ref tree 1))
	   (ppx (sx-ref tree 2)))

	  ;; ArgumentList
	  ((ArgumentList)
	   (pair-for-each
	    (lambda (pair)
	      (ppx (car pair))
	      (if (pair? (cdr pair)) (sf ", ")))
	    (cdr tree)))

	  ;; post-inc
	  ;; post-dec

	  ;; delete
	  ;; void
	  ;; typeof
	  ;; pre-inc
	  ;; pre-dec
	  ;; pos
	  ;; neg
	  ;; ~
	  ;; not
	  ;; add sub mul div mod
	  ((add sub mul div mod)
	   (let ((op (sx-ref tree 0))
		 (lval (sx-ref tree 1))
		 (rval (sx-ref tree 2)))
	     (if (protect-lval? op lval)
		 (ppx/p lval)
		 (ppx lval))
	     (case (car tree)
	       ((add) (sf " + ")) ((sub) (sf " - "))
	       ((mul) (sf "*")) ((div) (sf "/")) ((mod) (sf "%")))
	     (if (protect-rval? op rval)
		 (ppx/p rval)
		 (ppx rval))
	     ))

	  ;; lshift
	  ;; rshift
	  ;; rrshift

	  ;; lt gt le ge eq neq
	  ((lt gt le ge eq neq)
	   (let ((op (sx-ref tree 0))
		 (lval (sx-ref tree 1))
		 (rval (sx-ref tree 2)))
	     (if (protect-lval? op lval)
		 (ppx/p lval)
		 (ppx lval))
	     (case op
	       ((lt) (sf " < ")) ((gt) (sf " <= "))
	       ((le) (sf " > ")) ((ge) (sf " >= "))
	       ((eq) (sf " == ")) ((neq) (sf " != ")))
	     (if (protect-rval? op rval)
		 (ppx/p rval)
		 (ppx rval))
	     ))

	  ;; instanceof
	  ;; in
	  ;; eq-eq
	  ;; neq-eq
	  ;; bit-and
	  ;; bit-xor
	  ;; bit-or
	  ;; and
	  ;; or
	  ;; ConditionalExpression

	  ;; AssignmentExpression
	  ((AssignmentExpression)
	   (ppx (sx-ref tree 1))
	   (sf " ~A " (cadr (sx-ref tree 2)))
	   (ppx (sx-ref tree 3)))

	  ;; assign mul-assign div-assign mod-assign add-assign sub-assign
	  ;; lshift-assign rshift-assign rrshift-assign and-assign
	  ;; xor-assign or-assign
	  ((assign) (sf " = "))
	  ((mul-assign) (sf " *= "))
	  ((div-assign) (sf " /= "))
	  ((mod-assign) (sf " %= "))
	  ((add-assign) (sf " += "))
	  ((sub-assign) (sf " -= "))
	  ((lshift-assign) (sf " <<= "))
	  ((rshift-assign) (sf " >>= "))
	  ((rrshift-assign) (sf " >>>= "))
	  ((and-assign) (sf " &= "))
	  ((xor-assign) (sf " ^= "))
	  ((or-assign) (sf " |= "))

	  ;; expr-list

	  ;; Block
	  ((Block)
	   (sf "{\n")
	   (push-il)
	   (ppx (sx-ref tree 1))
	   (pop-il)
	   (sf "}\n"))

	  ;; VariableStatement
	  ((VariableStatement)
	   (sf "var ")
	   (for-each (lambda (el) (ppx el)) (cdr tree))
	   (sf-nl))

	  ;; VariableDeclarationList
	  ;; VariableDeclaration
	  ((VariableDeclarationList)
	   (pair-for-each
	    (lambda (pair)
	      ;;(sf "decl=~S\n" (car pair))
	      (let* ((decl (car pair))
		     (id (cadr (sx-ref decl 1)))
		     (val (and (< 2 (length decl)) (sx-ref decl 2))))
		(sf "~A" id)
		(if val (ppx val))
		(if (pair? (cdr pair))
		    (sf ", ")
		    (sf ";"))
		))
	    (cdr tree)))

	  ;; Initializer
	  ((Initializer) (sf " = ") (ppx (cadr tree)))

	  ;; EmptyStatement
	  ((EmptyStatement) (sf ";"))

	  ;; ExpressionStatement
	  ((ExpressionStatement)
	   (ppx (sx-ref tree 1))
	   (sf ";\n"))
	  
	  ;; IfStatement
	  ((IfStatement)
	   (let ((ex (sx-ref tree 1))
		 (th (sx-ref tree 2))
		 (el (and (< 3 (length tree)) (sx-ref tree 3)))
		 )
	     ;;(simple-format #t "\nel=~S\n" el)
	     (sf "if (") (ppx ex) (sf ") {\n")
	     (push-il)
	     (ppx th)
	     (pop-il)
	     (if el
		 (if (eqv? 'IfStatement (car el))
		     (begin
		       (sf "} else ")
		       (ppx el))
		     (begin
		       (sf "} else {\n")
		       (push-il) (ppx el) (pop-il)
		       (sf "}\n")))
		 (sf "}\n"))))

	  ;; do
	  ;; while
	  ;; for

	  ;; for-in
	  ((for-in)
	   (let ((lhsx (sx-ref tree 1))
		 (expr (sx-ref tree 2))
		 (stmt (sx-ref tree 3)))
	     ;;(simple-format #t "\n(car stmt)=~S\n" (car stmt))
	     (sf "for (") (ppx lhsx) (sf " in ") (ppx expr)
	     (if (eqv? 'Block (car stmt))
		 (begin (sf ") ") (ppx stmt))
		 (begin (sf ")\n") (push-il) (ppx stmt) (pop-il)))))

	  ;; Expression
	  ;; ExprStmt
	  ;; ContinueStatement

	  ;; ReturnStatement
	  ((ReturnStatement)
	   (sf "return")
	   (when (< 1 (length tree)) (sf " ") (ppx (cadr tree)))
	   (sf ";\n"))

	  ;; WithStatement
	  ;; SwitchStatement
	  ;; CaseBlock
	  ;; CaseClauses
	  ;; CaseClause
	  ;; DefaultClause
	  ;; LabelledStatement
	  ;; ThrowStatement
	  ;; TryStatement
	  ;; Catch
	  ;; Finally

	  ;; FunctionDeclaration (see also fU)
	  ((FunctionDeclaration)
	   (let ((name (sx-ref tree 1))
		 (parl (sx-ref tree 2))
		 (body (sx-ref tree 3)))
	     (sf "function ~A(" (cadr name))
	     (ppx parl)
	     (sf ") {\n")
	     (push-il)
	     (ppx body)
	     (pop-il)
	     (sf "}\n")))

	  ;; FunctionExpression

	  ;; FormalParameterList
	  ((FormalParameterList)
	   (pair-for-each
	    (lambda (pair)
	      (sf "~A" (cadar pair))
	      (if (pair? (cdr pair)) (sf ", ")))
	    (cdr tree)))

	  ;; Program
	  ((Program)
	   (ppx (cadr tree)))		; should be start

	  ;; SourceElements
	  ((SourceElements) ;; with spaces around fctn-decl's
	   (pair-for-each
	    (lambda (pair)
	      (let ((selt (car pair)) (not-last (pair? (cdr pair))))
		(case (car selt)
		  ((EmptyStatement) #f)
		  ((FunctionDeclaration)
		   (ppx selt)
		   (if not-last (sf-nl)))
		  (else
		   (ppx selt)
		   (if (and not-last (eqv? 'FunctionDeclaration (caadr pair)))
		       (sf-nl))))))
	    (cdr tree)))

	  (else
	   (simple-format #t "\nnot handled: ~S\n" (car tree))
	   #f)))))

  (ppx tree))

;; --- last line ---
