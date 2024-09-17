;; ./mach.d/ia-js-act.scm

;; Copyright 2015-2018 Matthew R. Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING.LESSER included with the this distribution.

(define ia-js-act-v
  (vector
   ;; $start => ProgramElement
   (lambda ($1 . $rest) $1)
   ;; Literal => NullLiteral
   (lambda ($1 . $rest) $1)
   ;; Literal => BooleanLiteral
   (lambda ($1 . $rest) $1)
   ;; Literal => NumericLiteral
   (lambda ($1 . $rest) $1)
   ;; Literal => StringLiteral
   (lambda ($1 . $rest) $1)
   ;; NullLiteral => "null"
   (lambda ($1 . $rest) '(NullLiteral))
   ;; BooleanLiteral => "true"
   (lambda ($1 . $rest) `(BooleanLiteral ,$1))
   ;; BooleanLiteral => "false"
   (lambda ($1 . $rest) `(BooleanLiteral ,$1))
   ;; NumericLiteral => '$fixed
   (lambda ($1 . $rest) `(NumericLiteral ,$1))
   ;; NumericLiteral => '$float
   (lambda ($1 . $rest) `(NumericLiteral ,$1))
   ;; StringLiteral => '$string
   (lambda ($1 . $rest) `(StringLiteral ,$1))
   ;; Identifier => '$ident
   (lambda ($1 . $rest) `(Identifier ,$1))
   ;; PrimaryExpression => "this"
   (lambda ($1 . $rest) `(PrimaryExpression (this)))
   ;; PrimaryExpression => Identifier
   (lambda ($1 . $rest) `(PrimaryExpression ,$1))
   ;; PrimaryExpression => Literal
   (lambda ($1 . $rest) `(PrimaryExpression ,$1))
   ;; PrimaryExpression => ArrayLiteral
   (lambda ($1 . $rest) `(PrimaryExpression ,$1))
   ;; PrimaryExpression => ObjectLiteral
   (lambda ($1 . $rest) `(PrimaryExpression ,$1))
   ;; PrimaryExpression => "(" Expression ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; ArrayLiteral => "[" Elision "]"
   (lambda ($3 $2 $1 . $rest)
     `(ArrayLiteral (Elision ,(number->string $2))))
   ;; ArrayLiteral => "[" "]"
   (lambda ($2 $1 . $rest) `(ArrayLiteral))
   ;; ArrayLiteral => "[" ElementList "]"
   (lambda ($3 $2 $1 . $rest)
     `(ArrayLiteral ,(tl->list $2)))
   ;; ArrayLiteral => "[" ElementList "," Elision "]"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(ArrayLiteral (Elision ,(number->string $2))))
   ;; ArrayLiteral => "[" ElementList "," "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ArrayLiteral ,(tl->list $2)))
   ;; ElementList => Elision AssignmentExpression
   (lambda ($2 $1 . $rest)
     (make-tl
       'ElementList
       `(Elision ,(number->string $2))))
   ;; ElementList => AssignmentExpression
   (lambda ($1 . $rest) (make-tl 'ElementList $1))
   ;; ElementList => ElementList "," Elision AssignmentExpression
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 `(Elision ,(number->string $3)) $4))
   ;; ElementList => ElementList "," AssignmentExpression
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; Elision => ","
   (lambda ($1 . $rest) 1)
   ;; Elision => Elision ","
   (lambda ($2 $1 . $rest) (#{1+}# $1))
   ;; ObjectLiteral => "{" "}"
   (lambda ($2 $1 . $rest) `(ObjectLiteral))
   ;; ObjectLiteral => "{" PropertyNameAndValueList "}"
   (lambda ($3 $2 $1 . $rest)
     `(ObjectLiteral ,(tl->list $2)))
   ;; ObjectLiteral => "{" PropertyNameAndValueList "," "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ObjectLiteral ,(tl->list $2)))
   ;; PropertyNameAndValueList => PropertyName ":" AssignmentExpression
   (lambda ($3 $2 $1 . $rest)
     (make-tl
       `PropertyNameAndValueList
       `(PropertyNameAndValue ,$1 ,$3)))
   ;; PropertyNameAndValueList => PropertyNameAndValueList "," PropertyName...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(PropertyNameAndValue ,$3 ,$5)))
   ;; PropertyName => Identifier
   (lambda ($1 . $rest) $1)
   ;; PropertyName => StringLiteral
   (lambda ($1 . $rest) $1)
   ;; PropertyName => NumericLiteral
   (lambda ($1 . $rest) $1)
   ;; MemberExpression => PrimaryExpression
   (lambda ($1 . $rest) $1)
   ;; MemberExpression => FunctionExpression
   (lambda ($1 . $rest) $1)
   ;; MemberExpression => MemberExpression "[" Expression "]"
   (lambda ($4 $3 $2 $1 . $rest) `(ooa-ref ,$1 ,$3))
   ;; MemberExpression => MemberExpression "." Identifier
   (lambda ($3 $2 $1 . $rest) `(obj-ref ,$1 ,$3))
   ;; MemberExpression => "new" MemberExpression Arguments
   (lambda ($3 $2 $1 . $rest) `(new ,$2 ,$3))
   ;; NewExpression => MemberExpression
   (lambda ($1 . $rest) $1)
   ;; NewExpression => "new" NewExpression
   (lambda ($2 $1 . $rest) `(new ,$2))
   ;; CallExpression => MemberExpression Arguments
   (lambda ($2 $1 . $rest)
     `(CallExpression ,$1 ,$2))
   ;; CallExpression => CallExpression Arguments
   (lambda ($2 $1 . $rest)
     `(CallExpression ,$1 ,$2))
   ;; CallExpression => CallExpression "[" Expression "]"
   (lambda ($4 $3 $2 $1 . $rest) `(ooa-ref ,$1 ,$3))
   ;; CallExpression => CallExpression "." Identifier
   (lambda ($3 $2 $1 . $rest) `(obj-ref ,$1 ,$3))
   ;; Arguments => "(" ")"
   (lambda ($2 $1 . $rest) '(ArgumentList))
   ;; Arguments => "(" ArgumentList ")"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; ArgumentList => AssignmentExpression
   (lambda ($1 . $rest) (make-tl 'ArgumentList $1))
   ;; ArgumentList => ArgumentList "," AssignmentExpression
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; LeftHandSideExpression => NewExpression
   (lambda ($1 . $rest) $1)
   ;; LeftHandSideExpression => CallExpression
   (lambda ($1 . $rest) $1)
   ;; PostfixExpression => LeftHandSideExpression
   (lambda ($1 . $rest) $1)
   ;; PostfixExpression => LeftHandSideExpression $P1 "++"
   (lambda ($3 $2 $1 . $rest) `(post-inc ,$1))
   ;; PostfixExpression => LeftHandSideExpression $P2 "--"
   (lambda ($3 $2 $1 . $rest) `(post-dec ,$1))
   ;; $P1 => 
   (lambda ($1 . $rest) (NSI))
   ;; $P2 => 
   (lambda ($1 . $rest) (NSI))
   ;; UnaryExpression => PostfixExpression
   (lambda ($1 . $rest) $1)
   ;; UnaryExpression => "delete" UnaryExpression
   (lambda ($2 $1 . $rest) `(delete ,$2))
   ;; UnaryExpression => "void" UnaryExpression
   (lambda ($2 $1 . $rest) `(void ,$2))
   ;; UnaryExpression => "typeof" UnaryExpression
   (lambda ($2 $1 . $rest) `(typeof ,$2))
   ;; UnaryExpression => "++" UnaryExpression
   (lambda ($2 $1 . $rest) `(pre-inc ,$2))
   ;; UnaryExpression => "--" UnaryExpression
   (lambda ($2 $1 . $rest) `(pre-dec ,$2))
   ;; UnaryExpression => "+" UnaryExpression
   (lambda ($2 $1 . $rest) `(pos ,$2))
   ;; UnaryExpression => "-" UnaryExpression
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; UnaryExpression => "~" UnaryExpression
   (lambda ($2 $1 . $rest) `(bitwise-not?? ,$2))
   ;; UnaryExpression => "!" UnaryExpression
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; MultiplicativeExpression => UnaryExpression
   (lambda ($1 . $rest) $1)
   ;; MultiplicativeExpression => MultiplicativeExpression "*" UnaryExpression
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; MultiplicativeExpression => MultiplicativeExpression "/" UnaryExpression
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; MultiplicativeExpression => MultiplicativeExpression "%" UnaryExpression
   (lambda ($3 $2 $1 . $rest) `(mod ,$1 ,$3))
   ;; AdditiveExpression => MultiplicativeExpression
   (lambda ($1 . $rest) $1)
   ;; AdditiveExpression => AdditiveExpression "+" MultiplicativeExpression
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; AdditiveExpression => AdditiveExpression "-" MultiplicativeExpression
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; ShiftExpression => AdditiveExpression
   (lambda ($1 . $rest) $1)
   ;; ShiftExpression => ShiftExpression "<<" AdditiveExpression
   (lambda ($3 $2 $1 . $rest) `(lshift ,$1 ,$3))
   ;; ShiftExpression => ShiftExpression ">>" AdditiveExpression
   (lambda ($3 $2 $1 . $rest) `(rshift ,$1 ,$3))
   ;; ShiftExpression => ShiftExpression ">>>" AdditiveExpression
   (lambda ($3 $2 $1 . $rest) `(rrshift ,$1 ,$3))
   ;; RelationalExpression => ShiftExpression
   (lambda ($1 . $rest) $1)
   ;; RelationalExpression => RelationalExpression "<" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; RelationalExpression => RelationalExpression ">" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; RelationalExpression => RelationalExpression "<=" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; RelationalExpression => RelationalExpression ">=" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; RelationalExpression => RelationalExpression "instanceof" ShiftExpres...
   (lambda ($3 $2 $1 . $rest) `(instanceof ,$1 ,$3))
   ;; RelationalExpression => RelationalExpression "in" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(in ,$1 ,$3))
   ;; RelationalExpressionNoIn => ShiftExpression
   (lambda ($1 . $rest) $1)
   ;; RelationalExpressionNoIn => RelationalExpressionNoIn "<" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; RelationalExpressionNoIn => RelationalExpressionNoIn ">" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; RelationalExpressionNoIn => RelationalExpressionNoIn "<=" ShiftExpres...
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; RelationalExpressionNoIn => RelationalExpressionNoIn ">=" ShiftExpres...
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; RelationalExpressionNoIn => RelationalExpressionNoIn "instanceof" Shi...
   (lambda ($3 $2 $1 . $rest) `(instanceof ,$1 ,$3))
   ;; EqualityExpression => RelationalExpression
   (lambda ($1 . $rest) $1)
   ;; EqualityExpression => EqualityExpression "==" RelationalExpression
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; EqualityExpression => EqualityExpression "!=" RelationalExpression
   (lambda ($3 $2 $1 . $rest) `(neq ,$1 ,$3))
   ;; EqualityExpression => EqualityExpression "===" RelationalExpression
   (lambda ($3 $2 $1 . $rest) `(eq-eq ,$1 ,$3))
   ;; EqualityExpression => EqualityExpression "!==" RelationalExpression
   (lambda ($3 $2 $1 . $rest) `(neq-eq ,$1 ,$3))
   ;; EqualityExpressionNoIn => RelationalExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; EqualityExpressionNoIn => EqualityExpressionNoIn "==" RelationalExpre...
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; EqualityExpressionNoIn => EqualityExpressionNoIn "!=" RelationalExpre...
   (lambda ($3 $2 $1 . $rest) `(neq ,$1 ,$3))
   ;; EqualityExpressionNoIn => EqualityExpressionNoIn "===" RelationalExpr...
   (lambda ($3 $2 $1 . $rest) `(eq-eq ,$1 ,$3))
   ;; EqualityExpressionNoIn => EqualityExpressionNoIn "!==" RelationalExpr...
   (lambda ($3 $2 $1 . $rest) `(neq-eq ,$1 ,$3))
   ;; BitwiseANDExpression => EqualityExpression
   (lambda ($1 . $rest) $1)
   ;; BitwiseANDExpression => BitwiseANDExpression "&" EqualityExpression
   (lambda ($3 $2 $1 . $rest) `(bit-and ,$1 ,$3))
   ;; BitwiseANDExpressionNoIn => EqualityExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; BitwiseANDExpressionNoIn => BitwiseANDExpressionNoIn "&" EqualityExpr...
   (lambda ($3 $2 $1 . $rest) `(bit-and ,$1 ,$3))
   ;; BitwiseXORExpression => BitwiseANDExpression
   (lambda ($1 . $rest) $1)
   ;; BitwiseXORExpression => BitwiseXORExpression "^" BitwiseANDExpression
   (lambda ($3 $2 $1 . $rest) `(bit-xor ,$1 ,$3))
   ;; BitwiseXORExpressionNoIn => BitwiseANDExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; BitwiseXORExpressionNoIn => BitwiseXORExpressionNoIn "^" BitwiseANDEx...
   (lambda ($3 $2 $1 . $rest) `(bit-xor ,$1 ,$3))
   ;; BitwiseORExpression => BitwiseXORExpression
   (lambda ($1 . $rest) $1)
   ;; BitwiseORExpression => BitwiseORExpression "|" BitwiseXORExpression
   (lambda ($3 $2 $1 . $rest) `(bit-or ,$1 ,$3))
   ;; BitwiseORExpressionNoIn => BitwiseXORExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; BitwiseORExpressionNoIn => BitwiseORExpressionNoIn "|" BitwiseXORExpr...
   (lambda ($3 $2 $1 . $rest) `(bit-or ,$1 ,$3))
   ;; LogicalANDExpression => BitwiseORExpression
   (lambda ($1 . $rest) $1)
   ;; LogicalANDExpression => LogicalANDExpression "&&" BitwiseORExpression
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; LogicalANDExpressionNoIn => BitwiseORExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; LogicalANDExpressionNoIn => LogicalANDExpressionNoIn "&&" BitwiseOREx...
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; LogicalORExpression => LogicalANDExpression
   (lambda ($1 . $rest) $1)
   ;; LogicalORExpression => LogicalORExpression "||" LogicalANDExpression
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; LogicalORExpressionNoIn => LogicalANDExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; LogicalORExpressionNoIn => LogicalORExpressionNoIn "||" LogicalANDExp...
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; ConditionalExpression => LogicalORExpression
   (lambda ($1 . $rest) $1)
   ;; ConditionalExpression => LogicalORExpression "?" AssignmentExpression...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(ConditionalExpression ,$1 ,$3 ,$5))
   ;; ConditionalExpressionNoIn => LogicalORExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; ConditionalExpressionNoIn => LogicalORExpressionNoIn "?" AssignmentEx...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(ConditionalExpression ,$1 ,$3 ,$5))
   ;; AssignmentExpression => ConditionalExpression
   (lambda ($1 . $rest) $1)
   ;; AssignmentExpression => LeftHandSideExpression AssignmentOperator Ass...
   (lambda ($3 $2 $1 . $rest)
     `(AssignmentExpression ,$1 ,$2 ,$3))
   ;; AssignmentExpressionNoIn => ConditionalExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; AssignmentExpressionNoIn => LeftHandSideExpression AssignmentOperator...
   (lambda ($3 $2 $1 . $rest)
     `(AssignmentExpression ,$1 ,$2 ,$3))
   ;; AssignmentOperator => "="
   (lambda ($1 . $rest) `(assign ,$1))
   ;; AssignmentOperator => "*="
   (lambda ($1 . $rest) `(mul-assign ,$1))
   ;; AssignmentOperator => "/="
   (lambda ($1 . $rest) `(div-assign ,$1))
   ;; AssignmentOperator => "%="
   (lambda ($1 . $rest) `(mod-assign ,$1))
   ;; AssignmentOperator => "+="
   (lambda ($1 . $rest) `(add-assign ,$1))
   ;; AssignmentOperator => "-="
   (lambda ($1 . $rest) `(sub-assign ,$1))
   ;; AssignmentOperator => "<<="
   (lambda ($1 . $rest) `(lshift-assign ,$1))
   ;; AssignmentOperator => ">>="
   (lambda ($1 . $rest) `(rshift-assign ,$1))
   ;; AssignmentOperator => ">>>="
   (lambda ($1 . $rest) `(rrshift-assign ,$1))
   ;; AssignmentOperator => "&="
   (lambda ($1 . $rest) `(and-assign ,$1))
   ;; AssignmentOperator => "^="
   (lambda ($1 . $rest) `(xor-assign ,$1))
   ;; AssignmentOperator => "|="
   (lambda ($1 . $rest) `(or-assign ,$1))
   ;; Expression => AssignmentExpression
   (lambda ($1 . $rest) $1)
   ;; Expression => Expression "," AssignmentExpression
   (lambda ($3 $2 $1 . $rest)
     (if (and (pair? (car $1))
              (eqv? 'expr-list (caar $1)))
       (tl-append $1 $3)
       (make-tl 'expr-list $1 $3)))
   ;; ExpressionNoIn => AssignmentExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; ExpressionNoIn => ExpressionNoIn "," AssignmentExpressionNoIn
   (lambda ($3 $2 $1 . $rest)
     (if (and (pair? (car $1))
              (eqv? 'expr-list (caar $1)))
       (tl-append $1 $3)
       (make-tl 'expr-list $1 $3)))
   ;; Statement => Block
   (lambda ($1 . $rest) $1)
   ;; Statement => VariableStatement
   (lambda ($1 . $rest) $1)
   ;; Statement => EmptyStatement
   (lambda ($1 . $rest) $1)
   ;; Statement => ExpressionStatement
   (lambda ($1 . $rest) $1)
   ;; Statement => IfStatement
   (lambda ($1 . $rest) $1)
   ;; Statement => IterationStatement
   (lambda ($1 . $rest) $1)
   ;; Statement => ContinueStatement
   (lambda ($1 . $rest) $1)
   ;; Statement => BreakStatement
   (lambda ($1 . $rest) $1)
   ;; Statement => ReturnStatement
   (lambda ($1 . $rest) $1)
   ;; Statement => WithStatement
   (lambda ($1 . $rest) $1)
   ;; Statement => LabelledStatement
   (lambda ($1 . $rest) $1)
   ;; Statement => SwitchStatement
   (lambda ($1 . $rest) $1)
   ;; Statement => ThrowStatement
   (lambda ($1 . $rest) $1)
   ;; Statement => TryStatement
   (lambda ($1 . $rest) $1)
   ;; Block => "{" LetStatementList StatementList "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(Block unquote
             (append
               (sx-tail (tl->list $2))
               (sx-tail (tl->list $3)))))
   ;; Block => "{" StatementList "}"
   (lambda ($3 $2 $1 . $rest)
     `(Block unquote (sx-tail (tl->list $2))))
   ;; Block => "{" "}"
   (lambda ($2 $1 . $rest) '(Block))
   ;; LetStatementList => LetStatement
   (lambda ($1 . $rest)
     (make-tl 'LetStatementList $1))
   ;; LetStatementList => LetStatementList LetStatement
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; LetStatement => "let" DeclarationList ";"
   (lambda ($3 $2 $1 . $rest)
     `(LetStatement ,(tl->list $2)))
   ;; StatementList => Statement
   (lambda ($1 . $rest) (make-tl 'StatementList $1))
   ;; StatementList => StatementList Statement
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; VariableStatement => "var" DeclarationList ";"
   (lambda ($3 $2 $1 . $rest)
     `(VariableStatement ,(tl->list $2)))
   ;; DeclarationList => VariableDeclaration
   (lambda ($1 . $rest)
     (make-tl 'DeclarationList $1))
   ;; DeclarationList => DeclarationList "," VariableDeclaration
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; DeclarationListNoIn => VariableDeclarationNoIn
   (lambda ($1 . $rest)
     (make-tl 'DeclarationList $1))
   ;; DeclarationListNoIn => DeclarationListNoIn "," VariableDeclarationNoIn
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; VariableDeclaration => Identifier Initializer
   (lambda ($2 $1 . $rest)
     `(VariableDeclaration ,$1 ,$2))
   ;; VariableDeclaration => Identifier
   (lambda ($1 . $rest) `(VariableDeclaration ,$1))
   ;; VariableDeclarationNoIn => Identifier InitializerNoIn
   (lambda ($2 $1 . $rest)
     `(VariableDeclaration ,$1 ,$2))
   ;; VariableDeclarationNoIn => Identifier
   (lambda ($1 . $rest) `(VariableDeclaration ,$1))
   ;; Initializer => "=" AssignmentExpression
   (lambda ($2 $1 . $rest) `(Initializer ,$2))
   ;; InitializerNoIn => "=" AssignmentExpressionNoIn
   (lambda ($2 $1 . $rest) `(Initializer ,$2))
   ;; EmptyStatement => ";"
   (lambda ($1 . $rest) '(EmptyStatement))
   ;; ExpressionStatement => Expression ";"
   (lambda ($2 $1 . $rest)
     `(ExpressionStatement ,$1))
   ;; IfStatement => "if" "(" Expression ")" Statement "else" Statement
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(IfStatement ,$3 ,$5 ,$7))
   ;; IfStatement => "if" "(" Expression ")" Statement
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(IfStatement ,$3 ,$5))
   ;; IterationStatement => "do" Statement "while" "(" Expression ")" ";"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(do ,$2 ,$5))
   ;; IterationStatement => "while" "(" Expression ")" Statement
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(while ,$3 ,$5))
   ;; IterationStatement => "for" "(" OptExprStmtNoIn OptExprStmt OptExprCl...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(for $3 $4 $5 $6))
   ;; IterationStatement => "for" "(" "var" DeclarationListNoIn ";" OptExpr...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for $4 $6 $7 $8))
   ;; IterationStatement => "for" "(" LeftHandSideExpression "in" Expressio...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for-in $3 $5 $7))
   ;; IterationStatement => "for" "(" "var" VariableDeclarationNoIn "in" Ex...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for-in $4 $6 $8))
   ;; OptExprStmtNoIn => ":"
   (lambda ($1 . $rest) '(NoExpression))
   ;; OptExprStmtNoIn => ExpressionNoIn ";"
   (lambda ($2 $1 . $rest) $1)
   ;; OptExprStmt => ";"
   (lambda ($1 . $rest) '(NoExpression))
   ;; OptExprStmt => Expression ";"
   (lambda ($2 $1 . $rest) $1)
   ;; OptExprClose => ";"
   (lambda ($1 . $rest) '(NoExpression))
   ;; OptExprClose => Expression ")"
   (lambda ($2 $1 . $rest) $1)
   ;; ContinueStatement => "continue" $P3 Identifier ";"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ContinueStatement ,$3))
   ;; ContinueStatement => "continue" ";"
   (lambda ($2 $1 . $rest) '(ContinueStatement))
   ;; $P3 => 
   (lambda ($1 . $rest) (NSI))
   ;; BreakStatement => "break" $P4 Identifier ";"
   (lambda ($4 $3 $2 $1 . $rest)
     `(BreakStatement ,$3))
   ;; BreakStatement => "break" ";"
   (lambda ($2 $1 . $rest) '(BreakStatement))
   ;; $P4 => 
   (lambda ($1 . $rest) (NSI))
   ;; ReturnStatement => "return" $P5 Expression ";"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ReturnStatement ,$3))
   ;; ReturnStatement => "return" ";"
   (lambda ($2 $1 . $rest) '(ReturnStatement))
   ;; $P5 => 
   (lambda ($1 . $rest) (NSI))
   ;; WithStatement => "with" "(" Expression ")" Statement
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(WithStatement ,$3 ,$5))
   ;; SwitchStatement => "switch" "(" Expression ")" $P6 CaseBlock
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(SwitchStatement ,$3 ,$6))
   ;; $P6 => 
   (lambda ($4 $3 $2 $1 . $rest) (NSI))
   ;; CaseBlock => "{" CaseBlockTail
   (lambda ($2 $1 . $rest) $2)
   ;; CaseBlock => "{" seq-of-semis CaseBlockTail
   (lambda ($3 $2 $1 . $rest) $3)
   ;; seq-of-semis => ";"
   (lambda ($1 . $rest) $1)
   ;; seq-of-semis => seq-of-semis ";"
   (lambda ($2 $1 . $rest) $1)
   ;; CaseBlockTail => "}"
   (lambda ($1 . $rest) '(CaseBlock))
   ;; CaseBlockTail => CaseClauses "}"
   (lambda ($2 $1 . $rest)
     `(CaseBlock ,(tl->list $1)))
   ;; CaseBlockTail => CaseClauses DefaultClause "}"
   (lambda ($3 $2 $1 . $rest)
     `(CaseBlock ,(tl->list $1) ,$2))
   ;; CaseBlockTail => CaseClauses DefaultClause CaseClauses "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(CaseBlock ,(tl->list $1) ,$2 ,(tl->list $3)))
   ;; CaseBlockTail => DefaultClause CaseClauses "}"
   (lambda ($3 $2 $1 . $rest)
     `(CaseBlock ,$1 ,(tl->list $2)))
   ;; CaseBlockTail => DefaultClause "}"
   (lambda ($2 $1 . $rest) `(CaseBlock ,$1))
   ;; CaseClauses => CaseClause
   (lambda ($1 . $rest) (make-tl 'CaseClauses $1))
   ;; CaseClauses => CaseClauses CaseClause
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; CaseClause => "case" Expression ":" StatementList
   (lambda ($4 $3 $2 $1 . $rest)
     `(CaseClause ,$2 ,(tl->list $4)))
   ;; CaseClause => "case" Expression ":"
   (lambda ($3 $2 $1 . $rest) `(CaseClause ,$2))
   ;; DefaultClause => "default" ":" StatementList
   (lambda ($3 $2 $1 . $rest)
     `(DefaultClause ,(tl->list $3)))
   ;; DefaultClause => "default" ":"
   (lambda ($2 $1 . $rest) `(DefaultClause))
   ;; LabelledStatement => Identifier ":" Statement
   (lambda ($3 $2 $1 . $rest)
     `(LabelledStatement ,$1 ,$3))
   ;; ThrowStatement => "throw" $P7 Expression ";"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ThrowStatement ,$3))
   ;; $P7 => 
   (lambda ($1 . $rest) (NSI))
   ;; TryStatement => "try" Block Catch
   (lambda ($3 $2 $1 . $rest)
     `(TryStatement ,$2 ,$3))
   ;; TryStatement => "try" Block Finally
   (lambda ($3 $2 $1 . $rest)
     `(TryStatement ,$2 ,$3))
   ;; TryStatement => "try" Block Catch Finally
   (lambda ($4 $3 $2 $1 . $rest)
     `(TryStatement ,$2 ,$3 ,$4))
   ;; Catch => "catch" "(" Identifier ")" Block
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(Catch ,$3 ,$5))
   ;; Finally => "finally" Block
   (lambda ($2 $1 . $rest) `(Finally ,$2))
   ;; FunctionDeclaration => "function" Identifier "(" FormalParameterList ...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(FunctionDeclaration ,$2 ,(tl->list $4) ,$7))
   ;; FunctionDeclaration => "function" Identifier "(" ")" "{" FunctionBody...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(FunctionDeclaration
        ,$2
        (FormalParameterList)
        ,$6))
   ;; FunctionExpression => "function" Identifier "(" FormalParameterList "...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(FunctionExpression ,$2 ,(tl->list $4) ,$7))
   ;; FunctionExpression => "function" "(" FormalParameterList ")" "{" Func...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(FunctionExpression ,(tl->list $3) ,$6))
   ;; FunctionExpression => "function" Identifier "(" ")" "{" FunctionBody "}"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(FunctionExpression
        ,$2
        (FormalParameterList)
        ,$6))
   ;; FunctionExpression => "function" "(" ")" "{" FunctionBody "}"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(FunctionExpression (FormalParameterList) ,$5))
   ;; FormalParameterList => Identifier
   (lambda ($1 . $rest)
     (make-tl 'FormalParameterList $1))
   ;; FormalParameterList => FormalParameterList "," Identifier
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; FunctionBody => FunctionElements
   (lambda ($1 . $rest) (tl->list $1))
   ;; FunctionElements => FunctionElement
   (lambda ($1 . $rest)
     (make-tl 'FunctionElements $1))
   ;; FunctionElements => FunctionElements FunctionElement
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; FunctionElement => Statement
   (lambda ($1 . $rest) $1)
   ;; FunctionElement => FunctionDeclaration
   (lambda ($1 . $rest) $1)
   ;; Program => ProgramElements
   (lambda ($1 . $rest) `(Program ,(tl->list $1)))
   ;; ProgramElements => ProgramElement
   (lambda ($1 . $rest)
     (make-tl 'ProgramElements $1))
   ;; ProgramElements => ProgramElements ProgramElement
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; ProgramElement => Statement
   (lambda ($1 . $rest) $1)
   ;; ProgramElement => FunctionDeclaration
   (lambda ($1 . $rest) $1)
   ))

;;; end tables
