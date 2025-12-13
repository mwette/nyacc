;; js-act.scm

;; Copyright (C) 2015-2018 Matthew Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING included with the this distribution.

(define js-act-v
  (vector
   ;; 0. $start => Program
   (lambda ($1 . $rest) $1)
   ;; 1. Literal => NullLiteral
   (lambda ($1 . $rest) $1)
   ;; 2. Literal => BooleanLiteral
   (lambda ($1 . $rest) $1)
   ;; 3. Literal => NumericLiteral
   (lambda ($1 . $rest) $1)
   ;; 4. Literal => StringLiteral
   (lambda ($1 . $rest) $1)
   ;; 5. NullLiteral => "null"
   (lambda ($1 . $rest) '(NullLiteral))
   ;; 6. BooleanLiteral => "true"
   (lambda ($1 . $rest) `(BooleanLiteral ,$1))
   ;; 7. BooleanLiteral => "false"
   (lambda ($1 . $rest) `(BooleanLiteral ,$1))
   ;; 8. NumericLiteral => '$fixed
   (lambda ($1 . $rest) `(NumericLiteral ,$1))
   ;; 9. NumericLiteral => '$float
   (lambda ($1 . $rest) `(NumericLiteral ,$1))
   ;; 10. StringLiteral => '$string
   (lambda ($1 . $rest) `(StringLiteral ,$1))
   ;; 11. term => ";"
   (lambda ($1 . $rest) $1)
   ;; 12. term => "\n"
   (lambda ($1 . $rest) $1)
   ;; 13. Identifier => '$ident
   (lambda ($1 . $rest) `(Identifier ,$1))
   ;; 14. PrimaryExpression => "this"
   (lambda ($1 . $rest) `(PrimaryExpression (this)))
   ;; 15. PrimaryExpression => Identifier
   (lambda ($1 . $rest) `(PrimaryExpression ,$1))
   ;; 16. PrimaryExpression => Literal
   (lambda ($1 . $rest) `(PrimaryExpression ,$1))
   ;; 17. PrimaryExpression => ArrayLiteral
   (lambda ($1 . $rest) `(PrimaryExpression ,$1))
   ;; 18. PrimaryExpression => ObjectLiteral
   (lambda ($1 . $rest) `(PrimaryExpression ,$1))
   ;; 19. PrimaryExpression => "(" Expression ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 20. ArrayLiteral => "[" Elision "]"
   (lambda ($3 $2 $1 . $rest)
     `(ArrayLiteral (Elision ,(number->string $2))))
   ;; 21. ArrayLiteral => "[" "]"
   (lambda ($2 $1 . $rest) `(ArrayLiteral))
   ;; 22. ArrayLiteral => "[" ElementList "]"
   (lambda ($3 $2 $1 . $rest)
     `(ArrayLiteral ,(tl->list $2)))
   ;; 23. ArrayLiteral => "[" ElementList "," Elision "]"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(ArrayLiteral (Elision ,(number->string $2))))
   ;; 24. ArrayLiteral => "[" ElementList "," "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ArrayLiteral ,(tl->list $2)))
   ;; 25. ElementList => Elision AssignmentExpression
   (lambda ($2 $1 . $rest)
     (make-tl
       'ElementList
       `(Elision ,(number->string $2))))
   ;; 26. ElementList => AssignmentExpression
   (lambda ($1 . $rest) (make-tl 'ElementList $1))
   ;; 27. ElementList => ElementList "," Elision AssignmentExpression
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 `(Elision ,(number->string $3)) $4))
   ;; 28. ElementList => ElementList "," AssignmentExpression
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 29. Elision => ","
   (lambda ($1 . $rest) 1)
   ;; 30. Elision => Elision ","
   (lambda ($2 $1 . $rest) (#{1+}# $1))
   ;; 31. ObjectLiteral => "{" "}"
   (lambda ($2 $1 . $rest) `(ObjectLiteral))
   ;; 32. ObjectLiteral => "{" PropertyNameAndValueList "}"
   (lambda ($3 $2 $1 . $rest)
     `(ObjectLiteral ,(tl->list $2)))
   ;; 33. ObjectLiteral => "{" PropertyNameAndValueList "," "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ObjectLiteral ,(tl->list $2)))
   ;; 34. PropertyNameAndValueList => PropertyName ":" AssignmentExpression
   (lambda ($3 $2 $1 . $rest)
     (make-tl
       `PropertyNameAndValueList
       `(PropertyNameAndValue ,$1 ,$3)))
   ;; 35. PropertyNameAndValueList => PropertyNameAndValueList "," PropertyName...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(PropertyNameAndValue ,$3 ,$5)))
   ;; 36. PropertyName => Identifier
   (lambda ($1 . $rest) $1)
   ;; 37. PropertyName => StringLiteral
   (lambda ($1 . $rest) $1)
   ;; 38. PropertyName => NumericLiteral
   (lambda ($1 . $rest) $1)
   ;; 39. MemberExpression => PrimaryExpression
   (lambda ($1 . $rest) $1)
   ;; 40. MemberExpression => FunctionExpression
   (lambda ($1 . $rest) $1)
   ;; 41. MemberExpression => MemberExpression "[" Expression "]"
   (lambda ($4 $3 $2 $1 . $rest) `(ooa-ref ,$1 ,$3))
   ;; 42. MemberExpression => MemberExpression "." Identifier
   (lambda ($3 $2 $1 . $rest) `(obj-ref ,$1 ,$3))
   ;; 43. MemberExpression => "new" MemberExpression Arguments
   (lambda ($3 $2 $1 . $rest) `(new ,$2 ,$3))
   ;; 44. NewExpression => MemberExpression
   (lambda ($1 . $rest) $1)
   ;; 45. NewExpression => "new" NewExpression
   (lambda ($2 $1 . $rest) `(new ,$2))
   ;; 46. CallExpression => MemberExpression Arguments
   (lambda ($2 $1 . $rest)
     `(CallExpression ,$1 ,$2))
   ;; 47. CallExpression => CallExpression Arguments
   (lambda ($2 $1 . $rest)
     `(CallExpression ,$1 ,$2))
   ;; 48. CallExpression => CallExpression "[" Expression "]"
   (lambda ($4 $3 $2 $1 . $rest) `(ooa-ref ,$1 ,$3))
   ;; 49. CallExpression => CallExpression "." Identifier
   (lambda ($3 $2 $1 . $rest) `(obj-ref ,$1 ,$3))
   ;; 50. Arguments => "(" ")"
   (lambda ($2 $1 . $rest) '(ArgumentList))
   ;; 51. Arguments => "(" ArgumentList ")"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; 52. ArgumentList => AssignmentExpression
   (lambda ($1 . $rest) (make-tl 'ArgumentList $1))
   ;; 53. ArgumentList => ArgumentList "," AssignmentExpression
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 54. LeftHandSideExpression => NewExpression
   (lambda ($1 . $rest) $1)
   ;; 55. LeftHandSideExpression => CallExpression
   (lambda ($1 . $rest) $1)
   ;; 56. PostfixExpression => LeftHandSideExpression
   (lambda ($1 . $rest) $1)
   ;; 57. PostfixExpression => LeftHandSideExpression "++"
   (lambda ($2 $1 . $rest) `(post-inc ,$1))
   ;; 58. PostfixExpression => LeftHandSideExpression "--"
   (lambda ($2 $1 . $rest) `(post-dec ,$1))
   ;; 59. UnaryExpression => PostfixExpression
   (lambda ($1 . $rest) $1)
   ;; 60. UnaryExpression => "delete" UnaryExpression
   (lambda ($2 $1 . $rest) `(delete ,$2))
   ;; 61. UnaryExpression => "void" UnaryExpression
   (lambda ($2 $1 . $rest) `(void ,$2))
   ;; 62. UnaryExpression => "typeof" UnaryExpression
   (lambda ($2 $1 . $rest) `(typeof ,$2))
   ;; 63. UnaryExpression => "++" UnaryExpression
   (lambda ($2 $1 . $rest) `(pre-inc ,$2))
   ;; 64. UnaryExpression => "--" UnaryExpression
   (lambda ($2 $1 . $rest) `(pre-dec ,$2))
   ;; 65. UnaryExpression => "+" UnaryExpression
   (lambda ($2 $1 . $rest) `(pos ,$2))
   ;; 66. UnaryExpression => "-" UnaryExpression
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; 67. UnaryExpression => "~" UnaryExpression
   (lambda ($2 $1 . $rest) `(bitwise-not?? ,$2))
   ;; 68. UnaryExpression => "!" UnaryExpression
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; 69. MultiplicativeExpression => UnaryExpression
   (lambda ($1 . $rest) $1)
   ;; 70. MultiplicativeExpression => MultiplicativeExpression "*" UnaryExpression
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 71. MultiplicativeExpression => MultiplicativeExpression "/" UnaryExpression
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 72. MultiplicativeExpression => MultiplicativeExpression "%" UnaryExpression
   (lambda ($3 $2 $1 . $rest) `(mod ,$1 ,$3))
   ;; 73. AdditiveExpression => MultiplicativeExpression
   (lambda ($1 . $rest) $1)
   ;; 74. AdditiveExpression => AdditiveExpression "+" MultiplicativeExpression
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; 75. AdditiveExpression => AdditiveExpression "-" MultiplicativeExpression
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; 76. ShiftExpression => AdditiveExpression
   (lambda ($1 . $rest) $1)
   ;; 77. ShiftExpression => ShiftExpression "<<" AdditiveExpression
   (lambda ($3 $2 $1 . $rest) `(lshift ,$1 ,$3))
   ;; 78. ShiftExpression => ShiftExpression ">>" AdditiveExpression
   (lambda ($3 $2 $1 . $rest) `(rshift ,$1 ,$3))
   ;; 79. ShiftExpression => ShiftExpression ">>>" AdditiveExpression
   (lambda ($3 $2 $1 . $rest) `(rrshift ,$1 ,$3))
   ;; 80. RelationalExpression => ShiftExpression
   (lambda ($1 . $rest) $1)
   ;; 81. RelationalExpression => RelationalExpression "<" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; 82. RelationalExpression => RelationalExpression ">" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; 83. RelationalExpression => RelationalExpression "<=" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; 84. RelationalExpression => RelationalExpression ">=" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; 85. RelationalExpression => RelationalExpression "instanceof" ShiftExpres...
   (lambda ($3 $2 $1 . $rest) `(instanceof ,$1 ,$3))
   ;; 86. RelationalExpression => RelationalExpression "in" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(in ,$1 ,$3))
   ;; 87. RelationalExpressionNoIn => ShiftExpression
   (lambda ($1 . $rest) $1)
   ;; 88. RelationalExpressionNoIn => RelationalExpressionNoIn "<" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; 89. RelationalExpressionNoIn => RelationalExpressionNoIn ">" ShiftExpression
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; 90. RelationalExpressionNoIn => RelationalExpressionNoIn "<=" ShiftExpres...
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; 91. RelationalExpressionNoIn => RelationalExpressionNoIn ">=" ShiftExpres...
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; 92. RelationalExpressionNoIn => RelationalExpressionNoIn "instanceof" Shi...
   (lambda ($3 $2 $1 . $rest) `(instanceof ,$1 ,$3))
   ;; 93. EqualityExpression => RelationalExpression
   (lambda ($1 . $rest) $1)
   ;; 94. EqualityExpression => EqualityExpression "==" RelationalExpression
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; 95. EqualityExpression => EqualityExpression "!=" RelationalExpression
   (lambda ($3 $2 $1 . $rest) `(neq ,$1 ,$3))
   ;; 96. EqualityExpression => EqualityExpression "===" RelationalExpression
   (lambda ($3 $2 $1 . $rest) `(eq-eq ,$1 ,$3))
   ;; 97. EqualityExpression => EqualityExpression "!==" RelationalExpression
   (lambda ($3 $2 $1 . $rest) `(neq-eq ,$1 ,$3))
   ;; 98. EqualityExpressionNoIn => RelationalExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; 99. EqualityExpressionNoIn => EqualityExpressionNoIn "==" RelationalExpre...
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; 100. EqualityExpressionNoIn => EqualityExpressionNoIn "!=" RelationalExpre...
   (lambda ($3 $2 $1 . $rest) `(neq ,$1 ,$3))
   ;; 101. EqualityExpressionNoIn => EqualityExpressionNoIn "===" RelationalExpr...
   (lambda ($3 $2 $1 . $rest) `(eq-eq ,$1 ,$3))
   ;; 102. EqualityExpressionNoIn => EqualityExpressionNoIn "!==" RelationalExpr...
   (lambda ($3 $2 $1 . $rest) `(neq-eq ,$1 ,$3))
   ;; 103. BitwiseANDExpression => EqualityExpression
   (lambda ($1 . $rest) $1)
   ;; 104. BitwiseANDExpression => BitwiseANDExpression "&" EqualityExpression
   (lambda ($3 $2 $1 . $rest) `(bit-and ,$1 ,$3))
   ;; 105. BitwiseANDExpressionNoIn => EqualityExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; 106. BitwiseANDExpressionNoIn => BitwiseANDExpressionNoIn "&" EqualityExpr...
   (lambda ($3 $2 $1 . $rest) `(bit-and ,$1 ,$3))
   ;; 107. BitwiseXORExpression => BitwiseANDExpression
   (lambda ($1 . $rest) $1)
   ;; 108. BitwiseXORExpression => BitwiseXORExpression "^" BitwiseANDExpression
   (lambda ($3 $2 $1 . $rest) `(bit-xor ,$1 ,$3))
   ;; 109. BitwiseXORExpressionNoIn => BitwiseANDExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; 110. BitwiseXORExpressionNoIn => BitwiseXORExpressionNoIn "^" BitwiseANDEx...
   (lambda ($3 $2 $1 . $rest) `(bit-xor ,$1 ,$3))
   ;; 111. BitwiseORExpression => BitwiseXORExpression
   (lambda ($1 . $rest) $1)
   ;; 112. BitwiseORExpression => BitwiseORExpression "|" BitwiseXORExpression
   (lambda ($3 $2 $1 . $rest) `(bit-or ,$1 ,$3))
   ;; 113. BitwiseORExpressionNoIn => BitwiseXORExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; 114. BitwiseORExpressionNoIn => BitwiseORExpressionNoIn "|" BitwiseXORExpr...
   (lambda ($3 $2 $1 . $rest) `(bit-or ,$1 ,$3))
   ;; 115. LogicalANDExpression => BitwiseORExpression
   (lambda ($1 . $rest) $1)
   ;; 116. LogicalANDExpression => LogicalANDExpression "&&" BitwiseORExpression
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; 117. LogicalANDExpressionNoIn => BitwiseORExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; 118. LogicalANDExpressionNoIn => LogicalANDExpressionNoIn "&&" BitwiseOREx...
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; 119. LogicalORExpression => LogicalANDExpression
   (lambda ($1 . $rest) $1)
   ;; 120. LogicalORExpression => LogicalORExpression "||" LogicalANDExpression
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; 121. LogicalORExpressionNoIn => LogicalANDExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; 122. LogicalORExpressionNoIn => LogicalORExpressionNoIn "||" LogicalANDExp...
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; 123. ConditionalExpression => LogicalORExpression
   (lambda ($1 . $rest) $1)
   ;; 124. ConditionalExpression => LogicalORExpression "?" AssignmentExpression...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(ConditionalExpression ,$1 ,$3 ,$5))
   ;; 125. ConditionalExpressionNoIn => LogicalORExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; 126. ConditionalExpressionNoIn => LogicalORExpressionNoIn "?" AssignmentEx...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(ConditionalExpression ,$1 ,$3 ,$5))
   ;; 127. AssignmentExpression => ConditionalExpression
   (lambda ($1 . $rest) $1)
   ;; 128. AssignmentExpression => LeftHandSideExpression AssignmentOperator Ass...
   (lambda ($3 $2 $1 . $rest)
     `(AssignmentExpression ,$1 ,$2 ,$3))
   ;; 129. AssignmentExpressionNoIn => ConditionalExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; 130. AssignmentExpressionNoIn => LeftHandSideExpression AssignmentOperator...
   (lambda ($3 $2 $1 . $rest)
     `(AssignmentExpression ,$1 ,$2 ,$3))
   ;; 131. AssignmentOperator => "="
   (lambda ($1 . $rest) `(assign ,$1))
   ;; 132. AssignmentOperator => "*="
   (lambda ($1 . $rest) `(mul-assign ,$1))
   ;; 133. AssignmentOperator => "/="
   (lambda ($1 . $rest) `(div-assign ,$1))
   ;; 134. AssignmentOperator => "%="
   (lambda ($1 . $rest) `(mod-assign ,$1))
   ;; 135. AssignmentOperator => "+="
   (lambda ($1 . $rest) `(add-assign ,$1))
   ;; 136. AssignmentOperator => "-="
   (lambda ($1 . $rest) `(sub-assign ,$1))
   ;; 137. AssignmentOperator => "<<="
   (lambda ($1 . $rest) `(lshift-assign ,$1))
   ;; 138. AssignmentOperator => ">>="
   (lambda ($1 . $rest) `(rshift-assign ,$1))
   ;; 139. AssignmentOperator => ">>>="
   (lambda ($1 . $rest) `(rrshift-assign ,$1))
   ;; 140. AssignmentOperator => "&="
   (lambda ($1 . $rest) `(and-assign ,$1))
   ;; 141. AssignmentOperator => "^="
   (lambda ($1 . $rest) `(xor-assign ,$1))
   ;; 142. AssignmentOperator => "|="
   (lambda ($1 . $rest) `(or-assign ,$1))
   ;; 143. Expression => AssignmentExpression
   (lambda ($1 . $rest) $1)
   ;; 144. Expression => Expression "," AssignmentExpression
   (lambda ($3 $2 $1 . $rest)
     (if (and (pair? (car $1))
              (eqv? 'expr-list (caar $1)))
       (tl-append $1 $3)
       (make-tl 'expr-list $1 $3)))
   ;; 145. ExpressionNoIn => AssignmentExpressionNoIn
   (lambda ($1 . $rest) $1)
   ;; 146. ExpressionNoIn => ExpressionNoIn "," AssignmentExpressionNoIn
   (lambda ($3 $2 $1 . $rest)
     (if (and (pair? (car $1))
              (eqv? 'expr-list (caar $1)))
       (tl-append $1 $3)
       (make-tl 'expr-list $1 $3)))
   ;; 147. Statement => Block
   (lambda ($1 . $rest) $1)
   ;; 148. Statement => VariableStatement
   (lambda ($1 . $rest) $1)
   ;; 149. Statement => EmptyStatement
   (lambda ($1 . $rest) $1)
   ;; 150. Statement => ExpressionStatement
   (lambda ($1 . $rest) $1)
   ;; 151. Statement => IfStatement
   (lambda ($1 . $rest) $1)
   ;; 152. Statement => IterationStatement
   (lambda ($1 . $rest) $1)
   ;; 153. Statement => ContinueStatement
   (lambda ($1 . $rest) $1)
   ;; 154. Statement => BreakStatement
   (lambda ($1 . $rest) $1)
   ;; 155. Statement => ReturnStatement
   (lambda ($1 . $rest) $1)
   ;; 156. Statement => WithStatement
   (lambda ($1 . $rest) $1)
   ;; 157. Statement => LabelledStatement
   (lambda ($1 . $rest) $1)
   ;; 158. Statement => SwitchStatement
   (lambda ($1 . $rest) $1)
   ;; 159. Statement => ThrowStatement
   (lambda ($1 . $rest) $1)
   ;; 160. Statement => TryStatement
   (lambda ($1 . $rest) $1)
   ;; 161. Block => "{" LetStatementList StatementList "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(Block unquote
             (append
               (sx-tail (tl->list $2))
               (sx-tail (tl->list $3)))))
   ;; 162. Block => "{" StatementList "}"
   (lambda ($3 $2 $1 . $rest)
     `(Block unquote (sx-tail (tl->list $2))))
   ;; 163. Block => "{" "}"
   (lambda ($2 $1 . $rest) '(Block))
   ;; 164. LetStatementList => LetStatement
   (lambda ($1 . $rest)
     (make-tl 'LetStatementList $1))
   ;; 165. LetStatementList => LetStatementList LetStatement
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 166. LetStatement => "let" DeclarationList term
   (lambda ($3 $2 $1 . $rest)
     `(LetStatement ,(tl->list $2)))
   ;; 167. StatementList => Statement
   (lambda ($1 . $rest) (make-tl 'StatementList $1))
   ;; 168. StatementList => StatementList Statement
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 169. VariableStatement => "var" DeclarationList term
   (lambda ($3 $2 $1 . $rest)
     `(VariableStatement ,(tl->list $2)))
   ;; 170. DeclarationList => VariableDeclaration
   (lambda ($1 . $rest)
     (make-tl 'DeclarationList $1))
   ;; 171. DeclarationList => DeclarationList "," VariableDeclaration
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 172. DeclarationListNoIn => VariableDeclarationNoIn
   (lambda ($1 . $rest)
     (make-tl 'DeclarationList $1))
   ;; 173. DeclarationListNoIn => DeclarationListNoIn "," VariableDeclarationNoIn
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 174. VariableDeclaration => Identifier Initializer
   (lambda ($2 $1 . $rest)
     `(VariableDeclaration ,$1 ,$2))
   ;; 175. VariableDeclaration => Identifier
   (lambda ($1 . $rest) `(VariableDeclaration ,$1))
   ;; 176. VariableDeclarationNoIn => Identifier InitializerNoIn
   (lambda ($2 $1 . $rest)
     `(VariableDeclaration ,$1 ,$2))
   ;; 177. VariableDeclarationNoIn => Identifier
   (lambda ($1 . $rest) `(VariableDeclaration ,$1))
   ;; 178. Initializer => "=" AssignmentExpression
   (lambda ($2 $1 . $rest) `(Initializer ,$2))
   ;; 179. InitializerNoIn => "=" AssignmentExpressionNoIn
   (lambda ($2 $1 . $rest) `(Initializer ,$2))
   ;; 180. EmptyStatement => term
   (lambda ($1 . $rest) '(EmptyStatement))
   ;; 181. ExpressionStatement => Expression term
   (lambda ($2 $1 . $rest)
     `(ExpressionStatement ,$1))
   ;; 182. IfStatement => "if" "(" Expression ")" Statement "else" Statement
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(IfStatement ,$3 ,$5 ,$7))
   ;; 183. IfStatement => "if" "(" Expression ")" Statement
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(IfStatement ,$3 ,$5))
   ;; 184. IterationStatement => "do" Statement "while" "(" Expression ")" term
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(do ,$2 ,$5))
   ;; 185. IterationStatement => "while" "(" Expression ")" Statement
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(while ,$3 ,$5))
   ;; 186. IterationStatement => "for" "(" OptExprStmtNoIn OptExprStmt OptExprCl...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(for $3 $4 $5 $6))
   ;; 187. IterationStatement => "for" "(" "var" DeclarationListNoIn ";" OptExpr...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for $4 $6 $7 $8))
   ;; 188. IterationStatement => "for" "(" LeftHandSideExpression "in" Expressio...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for-in $3 $5 $7))
   ;; 189. IterationStatement => "for" "(" "var" VariableDeclarationNoIn "in" Ex...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for-in $4 $6 $8))
   ;; 190. OptExprStmtNoIn => ":"
   (lambda ($1 . $rest) '(NoExpression))
   ;; 191. OptExprStmtNoIn => ExpressionNoIn ";"
   (lambda ($2 $1 . $rest) $1)
   ;; 192. OptExprStmt => ";"
   (lambda ($1 . $rest) '(NoExpression))
   ;; 193. OptExprStmt => Expression ";"
   (lambda ($2 $1 . $rest) $1)
   ;; 194. OptExprClose => ";"
   (lambda ($1 . $rest) '(NoExpression))
   ;; 195. OptExprClose => Expression ")"
   (lambda ($2 $1 . $rest) $1)
   ;; 196. ContinueStatement => "continue" Identifier term
   (lambda ($3 $2 $1 . $rest)
     `(ContinueStatement ,$2))
   ;; 197. ContinueStatement => "continue" term
   (lambda ($2 $1 . $rest) '(ContinueStatement))
   ;; 198. BreakStatement => "break" Identifier term
   (lambda ($3 $2 $1 . $rest) `(BreakStatement ,$2))
   ;; 199. BreakStatement => "break" term
   (lambda ($2 $1 . $rest) '(BreakStatement))
   ;; 200. ReturnStatement => "return" Expression term
   (lambda ($3 $2 $1 . $rest)
     `(ReturnStatement ,$2))
   ;; 201. ReturnStatement => "return" term
   (lambda ($2 $1 . $rest) '(ReturnStatement))
   ;; 202. WithStatement => "with" "(" Expression ")" Statement
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(WithStatement ,$3 ,$5))
   ;; 203. SwitchStatement => "switch" "(" Expression ")" CaseBlock
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(SwitchStatement ,$3 ,$5))
   ;; 204. CaseBlock => "{" CaseBlockTail
   (lambda ($2 $1 . $rest) $2)
   ;; 205. CaseBlock => "{" seq-of-semis CaseBlockTail
   (lambda ($3 $2 $1 . $rest) $3)
   ;; 206. seq-of-semis => ";"
   (lambda ($1 . $rest) $1)
   ;; 207. seq-of-semis => seq-of-semis ";"
   (lambda ($2 $1 . $rest) $1)
   ;; 208. CaseBlockTail => "}"
   (lambda ($1 . $rest) '(CaseBlock))
   ;; 209. CaseBlockTail => CaseClauses "}"
   (lambda ($2 $1 . $rest)
     `(CaseBlock ,(tl->list $1)))
   ;; 210. CaseBlockTail => CaseClauses DefaultClause "}"
   (lambda ($3 $2 $1 . $rest)
     `(CaseBlock ,(tl->list $1) ,$2))
   ;; 211. CaseBlockTail => CaseClauses DefaultClause CaseClauses "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(CaseBlock ,(tl->list $1) ,$2 ,(tl->list $3)))
   ;; 212. CaseBlockTail => DefaultClause CaseClauses "}"
   (lambda ($3 $2 $1 . $rest)
     `(CaseBlock ,$1 ,(tl->list $2)))
   ;; 213. CaseBlockTail => DefaultClause "}"
   (lambda ($2 $1 . $rest) `(CaseBlock ,$1))
   ;; 214. CaseClauses => CaseClause
   (lambda ($1 . $rest) (make-tl 'CaseClauses $1))
   ;; 215. CaseClauses => CaseClauses CaseClause
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 216. CaseClause => "case" Expression ":" StatementList
   (lambda ($4 $3 $2 $1 . $rest)
     `(CaseClause ,$2 ,(tl->list $4)))
   ;; 217. CaseClause => "case" Expression ":"
   (lambda ($3 $2 $1 . $rest) `(CaseClause ,$2))
   ;; 218. DefaultClause => "default" ":" StatementList
   (lambda ($3 $2 $1 . $rest)
     `(DefaultClause ,(tl->list $3)))
   ;; 219. DefaultClause => "default" ":"
   (lambda ($2 $1 . $rest) `(DefaultClause))
   ;; 220. LabelledStatement => Identifier ":" Statement
   (lambda ($3 $2 $1 . $rest)
     `(LabelledStatement ,$1 ,$3))
   ;; 221. ThrowStatement => "throw" Expression term
   (lambda ($3 $2 $1 . $rest) `(ThrowStatement ,$2))
   ;; 222. TryStatement => "try" Block Catch
   (lambda ($3 $2 $1 . $rest)
     `(TryStatement ,$2 ,$3))
   ;; 223. TryStatement => "try" Block Finally
   (lambda ($3 $2 $1 . $rest)
     `(TryStatement ,$2 ,$3))
   ;; 224. TryStatement => "try" Block Catch Finally
   (lambda ($4 $3 $2 $1 . $rest)
     `(TryStatement ,$2 ,$3 ,$4))
   ;; 225. Catch => "catch" "(" Identifier ")" Block
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(Catch ,$3 ,$5))
   ;; 226. Finally => "finally" Block
   (lambda ($2 $1 . $rest) `(Finally ,$2))
   ;; 227. FunctionDeclaration => "function" Identifier "(" FormalParameterList ...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(FunctionDeclaration ,$2 ,(tl->list $4) ,$7))
   ;; 228. FunctionDeclaration => "function" Identifier "(" ")" "{" FunctionBody...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(FunctionDeclaration
        ,$2
        (FormalParameterList)
        ,$6))
   ;; 229. FunctionExpression => "function" Identifier "(" FormalParameterList "...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(FunctionExpression ,$2 ,(tl->list $4) ,$7))
   ;; 230. FunctionExpression => "function" "(" FormalParameterList ")" "{" Func...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(FunctionExpression ,(tl->list $3) ,$6))
   ;; 231. FunctionExpression => "function" Identifier "(" ")" "{" FunctionBody "}"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(FunctionExpression
        ,$2
        (FormalParameterList)
        ,$6))
   ;; 232. FunctionExpression => "function" "(" ")" "{" FunctionBody "}"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(FunctionExpression (FormalParameterList) ,$5))
   ;; 233. FormalParameterList => Identifier
   (lambda ($1 . $rest)
     (make-tl 'FormalParameterList $1))
   ;; 234. FormalParameterList => FormalParameterList "," Identifier
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 235. FunctionBody => FunctionElements
   (lambda ($1 . $rest) (tl->list $1))
   ;; 236. FunctionElements => FunctionElement
   (lambda ($1 . $rest)
     (make-tl 'FunctionElements $1))
   ;; 237. FunctionElements => FunctionElements FunctionElement
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 238. FunctionElement => Statement
   (lambda ($1 . $rest) $1)
   ;; 239. FunctionElement => FunctionDeclaration
   (lambda ($1 . $rest) $1)
   ;; 240. Program => ProgramElements
   (lambda ($1 . $rest) `(Program ,(tl->list $1)))
   ;; 241. ProgramElements => ProgramElement
   (lambda ($1 . $rest)
     (make-tl 'ProgramElements $1))
   ;; 242. ProgramElements => ProgramElements ProgramElement
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 243. ProgramElement => Statement
   (lambda ($1 . $rest) $1)
   ;; 244. ProgramElement => FunctionDeclaration
   (lambda ($1 . $rest) $1)
   ))

;; --- last line ---
