;;; lang/ecmascript/pgen.scm
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

(define-module (lang javascript pgen)
  #:export (js-spec
	    js-mach
	    parse-js)
  #:use-module (lang util)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module ((srfi srfi-43) #:select (vector-map))
  )

;; NOTE
;; The 'NoIn' variants are needed to avoid confusing the in operator 
;; in a relational expression with the in operator in a for statement.


;; Not in the grammar yet: FunctionExpression
;; check on Elision

(define js-spec
  (lalr-spec
   (notice lang-crn)
   ;;(expect 1)
   (start Program)
   (grammar

    (Literal
     (NullLiteral ($$ `(NullLiteral)))
     (BooleanLiteral ($$ `(BooleanLiteral ,$1)))
     (NumericLiteral ($$ `(NumericLiteral ,$1)))
     (StringLiteral ($$ `(StringLiteral ,$1)))
     )

    (NullLiteral ("null"))
    (BooleanLiteral ("true") ("false"))
    (NumericLiteral ('$fx) ('$fl))
    (StringLiteral ('$string))
    ;;(DoubleStringCharacters ('$string))
    ;;(SingleStringCharacters ('$string))

    (Identifier ('$ident ($$ `(Identifier ,$1))))

    ;; A.3
    (PrimaryExpression
     ("this" ($$ `(PrimaryExpression ,$1)))
     (Identifier ($$ `(PrimaryExpression ,$1)))
     (Literal ($$ `(PrimaryExpression ,$1)))
     (ArrayLiteral ($$ `(PrimaryExpression ,$1)))
     (ObjectLiteral ($$ `(PrimaryExpression ,$1)))
     ("(" Expression ")" ($$ `(PrimaryExpression ,$2)))
     )

    (ArrayLiteral
     ("[" Elision "]" ($$ `(ArrayLiteral)))
     ("[" "]" ($$ `(ArrayLiteral)))
     ("[" ElementList "," Elision "]" ($$ `(ArrayLiteral ,$2)))
     ("[" ElementList "," "]" ($$ `(ArrayLiteral ,$2)))
     )

    (ElementList
     (Elision AssignmentExpression ($$ (make-tl 'ElementList $2)))
     (AssignmentExpression ($$ (make-tl 'ElementList $1)))
     (ElementList "," Elision AssignmentExpression ($$ (tl-append $1 $4)))
     (ElementList "," AssignmentExpression ($$ (tl-append $1 $3)))
     )

    (Elision
     ("," ($$ '(Elision)))
     (Elision ",")
     )

    (ObjectLiteral
     ("{" "}" ($$ `(ObjectLiteral)))
     ("{" PropertyNameAndValueList "}" ($$ `(ObjectLiteral ,(tl->list $2))))
     )

    (PropertyNameAndValueList
     (PropertyName ":" AssignmentExpression
		   ($$ (make-tl `PropertyNameAndValueList $1 $3)))
     (PropertyNameAndValueList "," PropertyName ":" AssignmentExpression
		   ($$ (tl->append $1 $3 $5)))
     )

    (PropertyName
     (Identifier)
     (StringLiteral)
     (NumericLiteral)
     )

    (MemberExpression
     (PrimaryExpression)
     ;;(FunctionExpression)
     (MemberExpression "[" Expression "]" ($$ `(array-ref ,$3 ,$1)))
     (MemberExpression "." Identifier ($$ `(elt-ref ,$3 ,$1)))
     ("new" MemberExpression Arguments ($$ `(new ,$2 ,$3)))
     )

    (NewExpression
     (MemberExpression)
     ("new" NewExpression ($$ `(new ,$2)))
     )

    (CallExpression
     (MemberExpression Arguments ($$ `(CallExpression ,$1 ,$2)))
     (CallExpression Arguments ($$ `(CallExpression ,$1 ,$2)))
     (CallExpression "[" Expression "]" ($$ `(array-ref ,$3 ,$1))) ; ??
     (CallExpression "." Identifier ($$ `(elt-ref ,$3 ,$1))) ; ??
     )

    (Arguments
     ("(" ")" ($$ '(Arguments)))
     ("(" ArgumentList ")" ($$ $2))
     )

    (ArgumentList
     (AssignmentExpression ($$ (make-tl 'ArgumentList $1)))
     (ArgumentList "," AssignmentExpression ($$ (tl-append $1 $3)))
     )

    (LeftHandSideExpression
     (NewExpression)
     (CallExpression)
     )

    (PostfixExpression
     (LeftHandSideExpression)
     (LeftHandSideExpression ($$ (NLT)) "++" ($$ `(post-inc $1)))
     (LeftHandSideExpression ($$ (NLT)) "--" ($$ `(post-dec $1)))
     )

    (UnaryExpression
     (PostfixExpression)
     ("delete" UnaryExpression ($$ `(delete ,$2)))
     ("void" UnaryExpression ($$ `(void ,$2)))
     ("typeof" UnaryExpression ($$ `(typeof ,$2)))
     ("++" UnaryExpression ($$ `(pre-inc ,$2)))
     ("--" UnaryExpression ($$ `(pre-inc ,$2)))
     ("+" UnaryExpression ($$ `(pos ,$2)))
     ("-" UnaryExpression ($$ `(neg ,$2)))
     ("~" UnaryExpression ($$ `(??? ,$2)))
     ("!" UnaryExpression ($$ `(not ,$2)))
     )

    (MultiplicativeExpression
     (UnaryExpression)
     (MultiplicativeExpression "*" UnaryExpression
			       ($$ `(mul ,$1 ,$3)))
     (MultiplicativeExpression "/" UnaryExpression
			       ($$ `(div ,$1 ,$3)))
     (MultiplicativeExpression "%" UnaryExpression
			       ($$ `(mod ,$1 ,$3)))
     )

    (AdditiveExpression
     (MultiplicativeExpression)
     (AdditiveExpression "+" MultiplicativeExpression
			 ($$ `(add ,$1 ,$3)))
     (AdditiveExpression "-" MultiplicativeExpression
			 ($$ `(sub ,$1 ,$3)))
     )

    (ShiftExpression
     (AdditiveExpression)
     (ShiftExpression "<<" AdditiveExpression
		      ($$ `(lshift ,$1 ,$3)))
     (ShiftExpression ">>" AdditiveExpression
		      ($$ `(rshift ,$1 ,$3)))
     (ShiftExpression ">>>" AdditiveExpression
		      ($$ `(rrshift ,$1 ,$3)))
     )

    (RelationalExpression
     (ShiftExpression)
     (RelationalExpression "<" ShiftExpression
			   ($$ `(lt ,$1 ,$3)))
     (RelationalExpression ">" ShiftExpression
			   ($$ `(gt ,$1 ,$3)))
     (RelationalExpression "<=" ShiftExpression
			   ($$ `(le ,$1 ,$3)))
     (RelationalExpression ">=" ShiftExpression
			   ($$ `(ge ,$1 ,$3)))
     (RelationalExpression "instanceof" ShiftExpression
			   ($$ `(instanceof ,$1 ,$3)))
     (RelationalExpression "in" ShiftExpression
			   ($$ `(in ,$1 ,$3)))
     )

    (RelationalExpressionNoIn
     (ShiftExpression)
     (RelationalExpressionNoIn "<" ShiftExpression
			       ($$ `(lt ,$1 ,$3)))
     (RelationalExpressionNoIn ">" ShiftExpression
			       ($$ `(gt ,$1 ,$3)))
     (RelationalExpressionNoIn "<=" ShiftExpression
			       ($$ `(le ,$1 ,$3)))
     (RelationalExpressionNoIn ">=" ShiftExpression
			       ($$ `(ge ,$1 ,$3)))
     (RelationalExpressionNoIn "instanceof" ShiftExpression
			       ($$ `(instanceof ,$1 ,$3)))
     )

    (EqualityExpression
     (RelationalExpression)
     (EqualityExpression "==" RelationalExpression
			 ($$ `(equal ,$1 ,$3)))
     (EqualityExpression "!=" RelationalExpression
			 ($$ `(not-equal ,$1 ,$3)))
     (EqualityExpression "===" RelationalExpression
			 ($$ `(equal-eq ,$1 ,$3)))
     (EqualityExpression "!==" RelationalExpression
			 ($$ `(not-equal-eq ,$1 ,$3)))
     )

    (EqualityExpressionNoIn
     (RelationalExpressionNoIn)
     (EqualityExpressionNoIn "==" RelationalExpressionNoIn
			 ($$ `(equal ,$1 ,$3)))
     (EqualityExpressionNoIn "!=" RelationalExpressionNoIn
			 ($$ `(not-equal ,$1 ,$3)))
     (EqualityExpressionNoIn "===" RelationalExpressionNoIn
			 ($$ `(equal-eq ,$1 ,$3)))
     (EqualityExpressionNoIn "!==" RelationalExpressionNoIn
			 ($$ `(not-equal-eq ,$1 ,$3)))
     )

    (BitwiseANDExpression
     (EqualityExpression)
     (BitwiseANDExpression "&" EqualityExpression
			   ($$ `(bit-and ,$1 ,$3)))
     )

    (BitwiseANDExpressionNoIn
     (EqualityExpressionNoIn)
     (BitwiseANDExpressionNoIn "&" EqualityExpressionNoIn
			   ($$ `(bit-and ,$1 ,$3)))
     )

    (BitwiseXORExpression
     (BitwiseANDExpression)
     (BitwiseXORExpression "^" BitwiseANDExpression
			   ($$ `(bit-xor ,$1 ,$3)))
     )

    (BitwiseXORExpressionNoIn
     (BitwiseANDExpressionNoIn)
     (BitwiseXORExpressionNoIn "^" BitwiseANDExpressionNoIn
			       ($$ `(bit-xor ,$1 ,$3)))
     )

    (BitwiseORExpression
     (BitwiseXORExpression)
     (BitwiseORExpression "|" BitwiseXORExpression
			  ($$ `(bit-or ,$1 ,$3)))
     )

    (BitwiseORExpressionNoIn
     (BitwiseXORExpressionNoIn)
     (BitwiseORExpressionNoIn "|" BitwiseXORExpressionNoIn
			      ($$ `(bit-or ,$1 ,$3)))
     )

    (LogicalANDExpression
     (BitwiseORExpression)
     (LogicalANDExpression "&&" BitwiseORExpression
			   ($$ `(and ,$1 ,$3)))
     )

    (LogicalANDExpressionNoIn
     (BitwiseORExpressionNoIn)
     (LogicalANDExpressionNoIn "&&" BitwiseORExpressionNoIn
			       ($$ `(and ,$1 ,$3)))
     )

    (LogicalORExpression
     (LogicalANDExpression)
     (LogicalORExpression "||" LogicalANDExpression
			  ($$ `(or ,$1 ,$3)))
     )

    (LogicalORExpressionNoIn
     (LogicalANDExpressionNoIn)
     (LogicalORExpressionNoIn "||" LogicalANDExpressionNoIn
			      ($$ `(or ,$1 ,$3)))
     )

    (ConditionalExpression
     (LogicalORExpression)
     (LogicalORExpression "?" AssignmentExpression ":" AssignmentExpression
			  ($$ `(ConditionalExpressoin ,$1 ,$3 ,$5)))
     )
    
    (ConditionalExpressionNoIn
     (LogicalORExpressionNoIn)
     (LogicalORExpressionNoIn "?" AssignmentExpressionNoIn
			      ":" AssignmentExpressionNoIn
			      ($$ `(ConditionalExpressoin ,$1 ,$3 ,$5)))
     )

    (AssignmentExpression
     (ConditionalExpression)
     (LeftHandSideExpression AssignmentOperator AssignmentExpression
			     ($$ `(AssignmentExpression ,$1 ,$2 ,$3)))
     )

    (AssignmentExpressionNoIn
     (ConditionalExpressionNoIn)
     (LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn
			     ($$ `(AssignmentExpression ,$1 ,$2 ,$3)))
     )

    (AssignmentOperator
     ;; todo
     ("=" ($$ '(assign)))
     ("*=" ($$ '(mul-assign))) ("/=" ($$ '(div-assign)))
     ("%=" ($$ '(mod-assign)))
     ("+=" ($$ '(add-assign))) ("-=" ($$ '(sub-assign)))
     ("<<=" ($$ '(lshift-assign))) (">>=" ($$ '(rshift-assign)))
     (">>>=" ($$ '(rrshift-assign)))
     ("&=" ($$ '(and-assign))) ("^=" ($$ '(xor-assign)))
     ("|=" ($$ '(or-assign))))

    (Expression
     (AssignmentExpression)
     (Expression
      "," AssignmentExpression
      ($$ (if (and (pair? (car $1)) (eqv? 'expr-list (caar $1)))
	      (tl-append $1 $3)
	      (make-tl 'expr-list $1 $3))))
     )

    (ExpressionNoIn
     (AssignmentExpressionNoIn)
     (ExpressionNoIn
      "," AssignmentExpressionNoIn
      ($$ (if (and (pair? (car $1)) (eqv? 'expr-list (caar $1)))
	      (tl-append $1 $3)
	      (make-tl 'expr-list $1 $3))))
     )

    ;; A.4
    (Statement
     (Block)
     (VariableStatement)
     (EmptyStatement)
     (ExpressionStatement)
     (IfStatement)
     (IterationStatement)
     (ContinueStatement)
     (BreakStatement)
     (ReturnStatement)
     (WithStatement)
     (LabelledStatement)
     (SwitchStatement)
     (ThrowStatement)
     (TryStatement)
     )

    (Block
     ("{" StatementList "}")
     ("{" "}")
     )

    (StatementList
     (Statement ($$ (make-tl 'StatementList $1)))
     (StatementList Statement ($$ (tl-append $1 $2)))
     )

    (VariableStatement
     ("var" VariableDeclarationList ";"
      ($$ `(VariableStatement ,(tl->list $2))))
     )

    (VariableDeclarationList
     (VariableDeclaration ($$ (make-tl 'VariableDeclarationList $1)))
     (VariableDeclarationList "," VariableDeclaration ($$ (tl-append $1 $3)))
     )

    (VariableDeclarationListNoIn
     (VariableDeclarationNoIn ($$ (make-tl 'VariableDeclarationList $1)))
     (VariableDeclarationListNoIn "," VariableDeclarationNoIn
				  ($$ (tl-append $1 $3)))
     )

    (VariableDeclaration
     (Identifier Initializer ($$ `(VariableDeclaration ,$1 ,$2)))
     (Identifier ($$ `(VariableDeclaration ,$1)))
     )

    (VariableDeclarationNoIn
     (Identifier InitializerNoIn ($$ `(VariableDeclaration ,$1 ,$2)))
     (Identifier ($$ `(VariableDeclaration ,$1)))
     )

    (Initializer
     ("=" AssignmentExpression ($$ `(Initializer ,$2)))
     )

    (InitializerNoIn
     ("=" AssignmentExpressionNoIn ($$ `(Initializer ,$2)))
     )

    (EmptyStatement
     (";" ($$ '(EmptyStatement)))
     )

    (ExpressionStatement
     ;; spec says: Reject if lookahead "{" "," "function".
     ;; => prune FunctionExpression, ObjectLiteral
     (($with Expression
	     ($prune FunctionExpression)
	     ($prune ObjectLiteral))
      ";"))

    (IfStatement
     ("if" "(" Expression ")" Statement "else" Statement
      ($$ `(IfStatement ,$3 ,$5 ,$7))
      )
     ("if" "(" Expression ")" Statement
      ($$ `(IfStatement ,$3 ,$5))
      )
     )

    (IterationStatement
     ("do" Statement "while" "(" Expression ")" ";"
      ($$ `(do ,$2 ,$5))
      )
     ("while" "(" Expression ")" Statement
      ($$ `(while ,$3 ,$5))
      )
     ("for" "(" OptExprStmtNoIn OptExprStmt OptExprClose Statement
      ;;($$ `(for ,$3 ,$
      )
     ("for" "(" "var" VariableDeclarationListNoIn ";"
      OptExprStmt OptExprClose Statement
      )
     ("for" "(" LeftHandSideExpression "in" Expression ")" Statement)
     ("for" "(" "var" VariableDeclarationNoIn "in" Expression ")" Statement)
     )
    (OptExprStmtNoIn
     (":" ($$ `(Expression)))
     (ExpressionNoIn ";")
     )
    (OptExprStmt
     (";" ($$ '(ExprStmt)))
     (Expression ";")
     )
    (OptExprClose
     (";" ($$ '(Expression)))
     (Expression ")")
     )

    (ContinueStatement
     ("continue" ($$ (NLT)) Identifier ";"
      ($$ `(ContinueStatement ,$3)))
     ("continue" ";" ($$ '(ContinueStatement)))
     )

    (BreakStatement
     ("break" ($$ (NLT)) Identifier ";"
      ($$ `(BreakStatement ,$3)))
     ("break" ";" ($$ '(ContinueStatement)))
     )

    (ReturnStatement
     ("return" ($$ (NLT)) Expression ";"
      ($$ `(ReturnStatement ,$3)))
     ("return" ";" ($$ '(ReturnStatement)))
     )

    (WithStatement
     ("with" "(" Expression ")" Statement
      ($$ `(WithStatement ,$3 ,$5))))

    (SwitchStatement
     ("switch" "(" Expression ")" CaseBlock
      ($$ `(SwitchStatement ,$3 ,$5))))

    (CaseBlock
     ("{" CaseClauses "}" ($$ `(CaseBlock ,$2)))
     ("{" "}" ($$ '(CaseBlock)))
     ("{" CaseClauses DefaultClause CaseClauses "}"
      ($$ `(CaseBlock ,(tl->list $2) ,$3 ,(tl->list $4))))
     ("{" CaseClauses DefaultClause "}"
      ($$ `(CaseBlock ,(tl->list $2) ,$3)))
     ("{" DefaultClause CaseClauses "}"
      ($$ `(CaseBlock ,$2 ,(tl->list $3))))
     ("{" DefaultClause "}"
      ($$ `(CaseBlock ,$2)))
     )

    (CaseClauses
     (CaseClause ($$ (make-tl 'CaseClauses $1)))
     (CaseClauses CaseClause ($$ (tl-append $1 $2)))
     )

    (CaseClause
     ("case" Expression ":" StatementList
      ($$ `(CaseClause ,$2 ,$4)))
     ("case" Expression ":"
      ($$ `(CaseClause ,$2)))
     )

    (DefaultClause
      ("default" ":" StatementList
       ($$ `(DefaultClause ,(tl->list $2))))
      ("default" ":"
       ($$ `(DefaultClause)))
      )

    (LabelledStatement
     (Identifier ":" Statement
		 ($$ `(LabelledStatement ,$1 ,$3)))
     )

    (ThrowStatement
     ("throw" ($$ (NLT)) Expression ";"
      ($$ `(ThrowStatement ,$3)))
     )

    (TryStatement
     ("try" Block Catch
      ($$ `(TryStatement ,$2 ,$3)))
     ("try" Block Finally
      ($$ `(TryStatement ,$2 ,$3)))
     ("try" Block Catch Finally
      ($$ `(TryStatement ,$2 ,$3 ,$4)))
     )

    (Catch
     ("catch" "(" Identifier ")" Block
      ($$ `(Catch ,3 ,$5)))
     )

    (Finally
     ("finally" Block
      ($$ `(Finally ,2)))
     )

    ;; A.5
    (FunctionDeclaration
     ("function" Identifier "(" FormalParameterList ")" "{" FunctionBody "}"
      ($$ `(FunctionDeclaration ,$2 ,(tl->list $4) ,$7)))
     ("function" Identifier "(" ")" "{" FunctionBody "}"
      ($$ `(FunctionDeclaration ,$2 ,$6)))
     )

    (FunctionExpression
     ("function" Identifier "(" FormalParameterList ")" "{" FunctionBody "}"
      ($$ `(FunctionExpression ,$2 ,(tl->list $4) ,$7)))
     ("function" "(" FormalParameterList ")" "{" FunctionBody "}"
      ($$ `(FunctionExpression ,(tl->list $4) ,$6)))
     ("function" Identifier "(" ")" "{" FunctionBody "}"
      ($$ `(FunctionExpression ,$2 ,$6)))
     ("function" "(" ")" "{" FunctionBody "}"
      ($$ `(FunctionExpression ,$5)))
     )

    (FormalParameterList
     (Identifier ($$ (make-tl 'FormalParameterList $1)))
     (FormalParameterList "," Identifier ($$ (tl-append $1 $3)))
     )

    (FunctionBody
     (SourceElements)
     )

    (Program
     (SourceElements ($$ (tl->list $1)))
     )

    (SourceElements
     (SourceElement ($$ (make-tl 'SourceElements $1)))
     (SourceElements SourceElement ($$ (tl-append $1 $2)))
     )

    (SourceElement
     (Statement)
     (FunctionDeclaration)
     )
    
    )))

(define js-mach
  (identity ;;hashify-machine
   (make-lalr-machine js-spec)))

#|

(define len-v (assq-ref js-mach 'len-v))
(define pat-v (assq-ref js-mach 'pat-v))
(define rto-v (assq-ref js-mach 'rto-v))
(define mtab (assq-ref js-mach 'mtab))
(define sya-v (vector-map
	       (lambda (ix nrg guts) (wrap-action nrg guts))
	       (assq-ref js-mach 'nrg-v) (assq-ref js-mach 'act-v)))
(define act-v (vector-map (lambda (ix f) (eval f (current-module))) sya-v))

(include "pbody.scm")
|#

#|
    (InputElementDiv
     (WhiteSpace) (LineTerminator) (Comment) (Token) (DivPunctuator))
    (InputElementRegExp
     (WhiteSpace) (LineTerminator) (Comment) (Token) (RegularExpressionLiteral))
    (WhiteSpace ("\t") ("\vt") ("\ff") (" ") ("&nbsp;") ("&usp;"))
    (LineTerminator ("\n") ("\r") (LS) (PS))
    (MultiLineComment ('multiline-comment))
|#
;;; --- last line    
