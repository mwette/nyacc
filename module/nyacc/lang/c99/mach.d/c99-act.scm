;; c99-act.scm

;; Copyright (C) 2015-2025 Matthew Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING included with the this distribution.

(define c99-act-v
  (vector
   ;; 0. $start => translation-unit
   (lambda ($1 . $rest) $1)
   ;; 1. primary-expression => identifier
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; 2. primary-expression => constant
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; 3. primary-expression => string-literal
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; 4. primary-expression => "(" expression ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 5. primary-expression => "(" "{" $P1 block-item-list $P2 "}" ")"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(stmt-expr (@ (extension "GNUC")) ,(tl->list $4)))
   ;; 6. $P1 => 
   (lambda ($2 $1 . $rest) (cpi-push))
   ;; 7. $P2 => 
   (lambda ($4 $3 $2 $1 . $rest) (cpi-pop))
   ;; 8. postfix-expression => primary-expression
   (lambda ($1 . $rest) $1)
   ;; 9. postfix-expression => postfix-expression "[" expression "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(array-ref ,$3 ,$1))
   ;; 10. postfix-expression => postfix-expression "(" argument-expression-list...
   (lambda ($4 $3 $2 $1 . $rest)
     `(fctn-call ,$1 ,(tl->list $3)))
   ;; 11. postfix-expression => postfix-expression "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(fctn-call ,$1 (expr-list)))
   ;; 12. postfix-expression => postfix-expression "." identifier
   (lambda ($3 $2 $1 . $rest) `(d-sel ,$3 ,$1))
   ;; 13. postfix-expression => postfix-expression "->" identifier
   (lambda ($3 $2 $1 . $rest) `(i-sel ,$3 ,$1))
   ;; 14. postfix-expression => postfix-expression "++"
   (lambda ($2 $1 . $rest) `(post-inc ,$1))
   ;; 15. postfix-expression => postfix-expression "--"
   (lambda ($2 $1 . $rest) `(post-dec ,$1))
   ;; 16. postfix-expression => "(" type-name ")" "{" literal-list "}"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(comp-lit ,$2 ,$5))
   ;; 17. postfix-expression => "(" attribute-specifier type-name ")" "{" liter...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(comp-lit (@ ,(attrl->attrs $2) ,$3 ,$6)))
   ;; 18. literal-list => initializer-list
   (lambda ($1 . $rest) $1)
   ;; 19. literal-list => initializer-list ","
   (lambda ($2 $1 . $rest) $1)
   ;; 20. argument-expression-list => assignment-expression
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; 21. argument-expression-list => argument-expression-list "," assignment-e...
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 22. argument-expression-list => arg-expr-hack
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; 23. argument-expression-list => argument-expression-list "," arg-expr-hack
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 24. arg-expr-hack => declaration-specifiers abstract-declarator
   (lambda ($2 $1 . $rest) `(param-decl ,$1 ,$2))
   ;; 25. arg-expr-hack => declaration-specifiers
   (lambda ($1 . $rest) `(param-decl ,$1))
   ;; 26. unary-expression => postfix-expression
   (lambda ($1 . $rest) $1)
   ;; 27. unary-expression => "++" unary-expression
   (lambda ($2 $1 . $rest) `(pre-inc ,$2))
   ;; 28. unary-expression => "--" unary-expression
   (lambda ($2 $1 . $rest) `(pre-dec ,$2))
   ;; 29. unary-expression => unary-operator cast-expression
   (lambda ($2 $1 . $rest) (list $1 $2))
   ;; 30. unary-expression => "sizeof" unary-expression
   (lambda ($2 $1 . $rest) `(sizeof-expr ,$2))
   ;; 31. unary-expression => "sizeof" "(" type-name ")"
   (lambda ($4 $3 $2 $1 . $rest) `(sizeof-type ,$3))
   ;; 32. unary-expression => "_Alignof" "(" type-name ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(alignof-type ,$3))
   ;; 33. unary-expression => "__builtin_offsetof" "(" type-name "," constant-e...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(offsetof-type ,$3 ,$5))
   ;; 34. unary-operator => "&"
   (lambda ($1 . $rest) 'ref-to)
   ;; 35. unary-operator => "*"
   (lambda ($1 . $rest) 'de-ref)
   ;; 36. unary-operator => "+"
   (lambda ($1 . $rest) 'pos)
   ;; 37. unary-operator => "-"
   (lambda ($1 . $rest) 'neg)
   ;; 38. unary-operator => "~"
   (lambda ($1 . $rest) 'bitwise-not)
   ;; 39. unary-operator => "!"
   (lambda ($1 . $rest) 'not)
   ;; 40. cast-expression => unary-expression
   (lambda ($1 . $rest) $1)
   ;; 41. cast-expression => "(" type-name ")" cast-expression
   (lambda ($4 $3 $2 $1 . $rest) `(cast ,$2 ,$4))
   ;; 42. multiplicative-expression => cast-expression
   (lambda ($1 . $rest) $1)
   ;; 43. multiplicative-expression => multiplicative-expression "*" cast-expre...
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 44. multiplicative-expression => multiplicative-expression "/" cast-expre...
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 45. multiplicative-expression => multiplicative-expression "%" cast-expre...
   (lambda ($3 $2 $1 . $rest) `(mod ,$1 ,$3))
   ;; 46. additive-expression => multiplicative-expression
   (lambda ($1 . $rest) $1)
   ;; 47. additive-expression => additive-expression "+" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; 48. additive-expression => additive-expression "-" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; 49. shift-expression => additive-expression
   (lambda ($1 . $rest) $1)
   ;; 50. shift-expression => shift-expression "<<" additive-expression
   (lambda ($3 $2 $1 . $rest) `(lshift ,$1 ,$3))
   ;; 51. shift-expression => shift-expression ">>" additive-expression
   (lambda ($3 $2 $1 . $rest) `(rshift ,$1 ,$3))
   ;; 52. relational-expression => shift-expression
   (lambda ($1 . $rest) $1)
   ;; 53. relational-expression => relational-expression "<" shift-expression
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; 54. relational-expression => relational-expression ">" shift-expression
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; 55. relational-expression => relational-expression "<=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; 56. relational-expression => relational-expression ">=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; 57. equality-expression => relational-expression
   (lambda ($1 . $rest) $1)
   ;; 58. equality-expression => equality-expression "==" relational-expression
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; 59. equality-expression => equality-expression "!=" relational-expression
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; 60. bitwise-and-expression => equality-expression
   (lambda ($1 . $rest) $1)
   ;; 61. bitwise-and-expression => bitwise-and-expression "&" equality-expression
   (lambda ($3 $2 $1 . $rest)
     `(bitwise-and ,$1 ,$3))
   ;; 62. bitwise-xor-expression => bitwise-and-expression
   (lambda ($1 . $rest) $1)
   ;; 63. bitwise-xor-expression => bitwise-xor-expression "^" bitwise-and-expr...
   (lambda ($3 $2 $1 . $rest)
     `(bitwise-xor ,$1 ,$3))
   ;; 64. bitwise-or-expression => bitwise-xor-expression
   (lambda ($1 . $rest) $1)
   ;; 65. bitwise-or-expression => bitwise-or-expression "|" bitwise-xor-expres...
   (lambda ($3 $2 $1 . $rest) `(bitwise-or ,$1 ,$3))
   ;; 66. logical-and-expression => bitwise-or-expression
   (lambda ($1 . $rest) $1)
   ;; 67. logical-and-expression => logical-and-expression "&&" bitwise-or-expr...
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; 68. logical-or-expression => logical-and-expression
   (lambda ($1 . $rest) $1)
   ;; 69. logical-or-expression => logical-or-expression "||" logical-and-expre...
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; 70. conditional-expression => logical-or-expression
   (lambda ($1 . $rest) $1)
   ;; 71. conditional-expression => logical-or-expression "?" expression ":" co...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(cond-expr ,$1 ,$3 ,$5))
   ;; 72. assignment-expression => conditional-expression
   (lambda ($1 . $rest) $1)
   ;; 73. assignment-expression => unary-expression assignment-operator assignm...
   (lambda ($3 $2 $1 . $rest)
     `(assn-expr ,$1 (op ,$2) ,$3))
   ;; 74. assignment-operator => "="
   (lambda ($1 . $rest) $1)
   ;; 75. assignment-operator => "+="
   (lambda ($1 . $rest) $1)
   ;; 76. assignment-operator => "-="
   (lambda ($1 . $rest) $1)
   ;; 77. assignment-operator => "*="
   (lambda ($1 . $rest) $1)
   ;; 78. assignment-operator => "/="
   (lambda ($1 . $rest) $1)
   ;; 79. assignment-operator => "%="
   (lambda ($1 . $rest) $1)
   ;; 80. assignment-operator => "<<="
   (lambda ($1 . $rest) $1)
   ;; 81. assignment-operator => ">>="
   (lambda ($1 . $rest) $1)
   ;; 82. assignment-operator => "&="
   (lambda ($1 . $rest) $1)
   ;; 83. assignment-operator => "^="
   (lambda ($1 . $rest) $1)
   ;; 84. assignment-operator => "|="
   (lambda ($1 . $rest) $1)
   ;; 85. expression => assignment-expression
   (lambda ($1 . $rest) $1)
   ;; 86. expression => expression "," assignment-expression
   (lambda ($3 $2 $1 . $rest)
     (if (eqv? 'comma-expr (sx-tag $1))
       (append $1 (list $3))
       `(comma-expr ,$1 ,$3)))
   ;; 87. constant-expression => conditional-expression
   (lambda ($1 . $rest) $1)
   ;; 88. declaration => declaration-no-comment ";"
   (lambda ($2 $1 . $rest) $1)
   ;; 89. declaration => declaration-no-comment ";" code-comment
   (lambda ($3 $2 $1 . $rest) (sx-attr-add $1 $3))
   ;; 90. declaration-no-comment => declaration-specifiers init-declarator-list
   (lambda ($2 $1 . $rest)
     (save-typenames `(decl ,$1 ,$2)))
   ;; 91. declaration-no-comment => declaration-specifiers
   (lambda ($1 . $rest) `(decl ,$1))
   ;; 92. declaration-specifiers => declaration-specifiers-1
   (lambda ($1 . $rest)
     (process-specs (tl->list $1)))
   ;; 93. declaration-specifiers-1 => storage-class-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; 94. declaration-specifiers-1 => storage-class-specifier declaration-speci...
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; 95. declaration-specifiers-1 => type-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; 96. declaration-specifiers-1 => type-specifier declaration-specifiers-1
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; 97. declaration-specifiers-1 => type-qualifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; 98. declaration-specifiers-1 => type-qualifier declaration-specifiers-1
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; 99. declaration-specifiers-1 => function-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; 100. declaration-specifiers-1 => function-specifier declaration-specifiers-1
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; 101. declaration-specifiers-1 => attribute-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; 102. declaration-specifiers-1 => attribute-specifier declaration-specifiers-1
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; 103. storage-class-specifier => "auto"
   (lambda ($1 . $rest) '(stor-spec (auto)))
   ;; 104. storage-class-specifier => "extern"
   (lambda ($1 . $rest) '(stor-spec (extern)))
   ;; 105. storage-class-specifier => "register"
   (lambda ($1 . $rest) '(stor-spec (register)))
   ;; 106. storage-class-specifier => "static"
   (lambda ($1 . $rest) '(stor-spec (static)))
   ;; 107. storage-class-specifier => "typedef"
   (lambda ($1 . $rest) '(stor-spec (typedef)))
   ;; 108. type-specifier => "void"
   (lambda ($1 . $rest) '(type-spec (void)))
   ;; 109. type-specifier => fixed-type-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; 110. type-specifier => float-type-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; 111. type-specifier => fixpt-type-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; 112. type-specifier => "_Sat" fixpt-type-specifier
   (lambda ($2 $1 . $rest)
     `(type-spec ,(string-append "_Sat " $2)))
   ;; 113. type-specifier => "_Bool"
   (lambda ($1 . $rest)
     '(type-spec (fixed-type "_Bool")))
   ;; 114. type-specifier => complex-type-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; 115. type-specifier => struct-or-union-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; 116. type-specifier => enum-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; 117. type-specifier => typedef-name
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; 118. type-specifier => "typeof" "(" unary-expression ")"
   (lambda ($4 $3 $2 $1 . $rest) `(typeof-expr ,$3))
   ;; 119. type-specifier => "typeof" "(" type-name ")"
   (lambda ($4 $3 $2 $1 . $rest) `(typeof-type ,$3))
   ;; 120. type-specifier => "__typeof" "(" unary-expression ")"
   (lambda ($4 $3 $2 $1 . $rest) `(typeof-expr ,$3))
   ;; 121. type-specifier => "__typeof" "(" type-name ")"
   (lambda ($4 $3 $2 $1 . $rest) `(typeof-type ,$3))
   ;; 122. type-specifier => "__typeof__" "(" unary-expression ")"
   (lambda ($4 $3 $2 $1 . $rest) `(typeof-expr ,$3))
   ;; 123. type-specifier => "__typeof__" "(" type-name ")"
   (lambda ($4 $3 $2 $1 . $rest) `(typeof-type ,$3))
   ;; 124. fixed-type-specifier => "short"
   (lambda ($1 . $rest) '(fixed-type "short"))
   ;; 125. fixed-type-specifier => "short" "int"
   (lambda ($2 $1 . $rest) '(fixed-type "short"))
   ;; 126. fixed-type-specifier => "signed" "short"
   (lambda ($2 $1 . $rest) '(fixed-type "short"))
   ;; 127. fixed-type-specifier => "signed" "short" "int"
   (lambda ($3 $2 $1 . $rest) '(fixed-type "short"))
   ;; 128. fixed-type-specifier => "int"
   (lambda ($1 . $rest) '(fixed-type "int"))
   ;; 129. fixed-type-specifier => "signed"
   (lambda ($1 . $rest) '(fixed-type "int"))
   ;; 130. fixed-type-specifier => "signed" "int"
   (lambda ($2 $1 . $rest) '(fixed-type "int"))
   ;; 131. fixed-type-specifier => "long"
   (lambda ($1 . $rest) '(fixed-type "long"))
   ;; 132. fixed-type-specifier => "long" "int"
   (lambda ($2 $1 . $rest) '(fixed-type "long"))
   ;; 133. fixed-type-specifier => "signed" "long"
   (lambda ($2 $1 . $rest) '(fixed-type "long"))
   ;; 134. fixed-type-specifier => "signed" "long" "int"
   (lambda ($3 $2 $1 . $rest) '(fixed-type "long"))
   ;; 135. fixed-type-specifier => "long" "long"
   (lambda ($2 $1 . $rest)
     '(fixed-type "long long"))
   ;; 136. fixed-type-specifier => "long" "long" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "long long"))
   ;; 137. fixed-type-specifier => "signed" "long" "long"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "long long"))
   ;; 138. fixed-type-specifier => "signed" "long" "long" "int"
   (lambda ($4 $3 $2 $1 . $rest)
     '(fixed-type "long long"))
   ;; 139. fixed-type-specifier => "unsigned" "short"
   (lambda ($2 $1 . $rest)
     '(fixed-type "unsigned short"))
   ;; 140. fixed-type-specifier => "unsigned" "short" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "unsigned short"))
   ;; 141. fixed-type-specifier => "unsigned"
   (lambda ($1 . $rest) '(fixed-type "unsigned"))
   ;; 142. fixed-type-specifier => "unsigned" "int"
   (lambda ($2 $1 . $rest) '(fixed-type "unsigned"))
   ;; 143. fixed-type-specifier => "unsigned" "long"
   (lambda ($2 $1 . $rest)
     '(fixed-type "unsigned long"))
   ;; 144. fixed-type-specifier => "unsigned" "long" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "unsigned long"))
   ;; 145. fixed-type-specifier => "unsigned" "long" "long"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "unsigned long long"))
   ;; 146. fixed-type-specifier => "unsigned" "long" "long" "int"
   (lambda ($4 $3 $2 $1 . $rest)
     '(fixed-type "unsigned long long"))
   ;; 147. fixed-type-specifier => "char"
   (lambda ($1 . $rest) '(fixed-type "char"))
   ;; 148. fixed-type-specifier => "signed" "char"
   (lambda ($2 $1 . $rest)
     '(fixed-type "signed char"))
   ;; 149. fixed-type-specifier => "unsigned" "char"
   (lambda ($2 $1 . $rest)
     '(fixed-type "unsigned char"))
   ;; 150. fixed-type-specifier => "__int128"
   (lambda ($1 . $rest) '(fixed-type "__int128"))
   ;; 151. fixed-type-specifier => "unsigned" "__int128"
   (lambda ($2 $1 . $rest)
     '(fixed-type "unsigned __int128"))
   ;; 152. fixed-type-specifier => "short" "signed"
   (lambda ($2 $1 . $rest) '(fixed-type "short"))
   ;; 153. fixed-type-specifier => "short" "signed" "int"
   (lambda ($3 $2 $1 . $rest) '(fixed-type "short"))
   ;; 154. fixed-type-specifier => "long" "signed"
   (lambda ($2 $1 . $rest) '(fixed-type "long"))
   ;; 155. fixed-type-specifier => "long" "signed" "int"
   (lambda ($3 $2 $1 . $rest) '(fixed-type "long"))
   ;; 156. fixed-type-specifier => "long" "long" "signed"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "long long"))
   ;; 157. fixed-type-specifier => "long" "long" "signed" "int"
   (lambda ($4 $3 $2 $1 . $rest)
     '(fixed-type "long long"))
   ;; 158. fixed-type-specifier => "short" "unsigned"
   (lambda ($2 $1 . $rest)
     '(fixed-type "unsigned short"))
   ;; 159. fixed-type-specifier => "short" "unsigned" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "unsigned short"))
   ;; 160. fixed-type-specifier => "long" "unsigned" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "unsigned long"))
   ;; 161. fixed-type-specifier => "long" "long" "unsigned" "int"
   (lambda ($4 $3 $2 $1 . $rest)
     '(fixed-type "unsigned long long"))
   ;; 162. float-type-specifier => "float"
   (lambda ($1 . $rest) '(float-type "float"))
   ;; 163. float-type-specifier => "double"
   (lambda ($1 . $rest) '(float-type "double"))
   ;; 164. float-type-specifier => "long" "double"
   (lambda ($2 $1 . $rest)
     '(float-type "long double"))
   ;; 165. float-type-specifier => "_Float16"
   (lambda ($1 . $rest) '(float-type "_Float16"))
   ;; 166. float-type-specifier => "_Float128"
   (lambda ($1 . $rest) '(float-type "_Float128"))
   ;; 167. float-type-specifier => "_float16"
   (lambda ($1 . $rest) '(float-type "_Float16"))
   ;; 168. float-type-specifier => "_float128"
   (lambda ($1 . $rest) '(float-type "_Float128"))
   ;; 169. complex-type-specifier => "float" "_Complex"
   (lambda ($2 $1 . $rest)
     '(complex-type "float _Complex"))
   ;; 170. complex-type-specifier => "double" "_Complex"
   (lambda ($2 $1 . $rest)
     '(complex-type "double _Complex"))
   ;; 171. complex-type-specifier => "long" "double" "_Complex"
   (lambda ($3 $2 $1 . $rest)
     '(complex-type "long double _Complex"))
   ;; 172. fixpt-type-specifier => "short" "_Fract"
   (lambda ($2 $1 . $rest)
     '(fixpt-type "short _Fract"))
   ;; 173. fixpt-type-specifier => "signed" "short" "_Fract"
   (lambda ($3 $2 $1 . $rest)
     '(fixpt-type "short _Fract"))
   ;; 174. fixpt-type-specifier => "_Fract"
   (lambda ($1 . $rest) '(fixpt-type "_Fract"))
   ;; 175. fixpt-type-specifier => "signed" "_Fract"
   (lambda ($2 $1 . $rest) '(fixpt-type "_Fract"))
   ;; 176. fixpt-type-specifier => "long" "_Fract"
   (lambda ($2 $1 . $rest)
     '(fixpt-type "long _Fract"))
   ;; 177. fixpt-type-specifier => "signed" "long" "_Fract"
   (lambda ($3 $2 $1 . $rest)
     '(fixpt-type "long _Fract"))
   ;; 178. fixpt-type-specifier => "unsigned" "short" "_Fract"
   (lambda ($3 $2 $1 . $rest)
     '(fixpt-type "unsigned short _Fract"))
   ;; 179. fixpt-type-specifier => "unsigned" "_Fract"
   (lambda ($2 $1 . $rest)
     '(fixpt-type "unsigned _Fract"))
   ;; 180. fixpt-type-specifier => "unsigned" "long" "_Fract"
   (lambda ($3 $2 $1 . $rest)
     '(fixpt-type "unsigned long _Fract"))
   ;; 181. fixpt-type-specifier => "short" "_Accum"
   (lambda ($2 $1 . $rest)
     '(fixpt-type "short _Accum"))
   ;; 182. fixpt-type-specifier => "signed" "short" "_Accum"
   (lambda ($3 $2 $1 . $rest)
     '(fixpt-type "short _Accum"))
   ;; 183. fixpt-type-specifier => "_Accum"
   (lambda ($1 . $rest) '(fixpt-type "_Accum"))
   ;; 184. fixpt-type-specifier => "signed" "_Accum"
   (lambda ($2 $1 . $rest) '(fixpt-type "_Accum"))
   ;; 185. fixpt-type-specifier => "long" "_Accum"
   (lambda ($2 $1 . $rest)
     '(fixpt-type "long _Accum"))
   ;; 186. fixpt-type-specifier => "signed" "long" "_Accum"
   (lambda ($3 $2 $1 . $rest)
     '(fixpt-type "long _Accum"))
   ;; 187. fixpt-type-specifier => "unsigned" "short" "_Accum"
   (lambda ($3 $2 $1 . $rest)
     '(fixpt-type "unsigned short _Accum"))
   ;; 188. fixpt-type-specifier => "unsigned" "_Accum"
   (lambda ($2 $1 . $rest)
     '(fixpt-type "unsigned _Accum"))
   ;; 189. fixpt-type-specifier => "unsigned" "long" "_Accum"
   (lambda ($3 $2 $1 . $rest)
     '(fixpt-type "unsigned long _Accum"))
   ;; 190. struct-or-union-specifier => "struct" opt-attr-specs ident-like "{" s...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (sx-list 'struct-def $2 $3 $5))
   ;; 191. struct-or-union-specifier => "struct" opt-attr-specs "{" struct-decla...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (sx-list 'struct-def $2 $4))
   ;; 192. struct-or-union-specifier => "struct" opt-attr-specs ident-like
   (lambda ($3 $2 $1 . $rest)
     (sx-list 'struct-ref $2 $3))
   ;; 193. struct-or-union-specifier => "union" opt-attr-specs ident-like "{" st...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (sx-list 'union-def $2 $3 $5))
   ;; 194. struct-or-union-specifier => "union" opt-attr-specs "{" struct-declar...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (sx-list 'union-def $2 $4))
   ;; 195. struct-or-union-specifier => "union" opt-attr-specs ident-like
   (lambda ($3 $2 $1 . $rest)
     (sx-list 'union-ref $2 $3))
   ;; 196. ident-like => identifier
   (lambda ($1 . $rest) $1)
   ;; 197. ident-like => typedef-name
   (lambda ($1 . $rest) `(ident ,(sx-ref $1 1)))
   ;; 198. opt-attr-specs => 
   (lambda $rest (list))
   ;; 199. opt-attr-specs => attribute-specifiers
   (lambda ($1 . $rest) `(@ ,(attrl->attrs $1)))
   ;; 200. struct-declaration-list => struct-declaration-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 201. struct-declaration-list => 
   (lambda $rest
     '(field-list
        (comp-decl
          (decl-spec-list (type-spec (fixed-type "char")))
          (comp-declr-list
            (comp-declr (ary-declr (ident "*anon*")))))))
   ;; 202. struct-declaration-list-1 => struct-declaration
   (lambda ($1 . $rest) (make-tl 'field-list $1))
   ;; 203. struct-declaration-list-1 => struct-declaration-list-1 struct-declara...
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 204. struct-declaration-list-1 => lone-comment
   (lambda ($1 . $rest) (make-tl 'field-list $1))
   ;; 205. struct-declaration-list-1 => struct-declaration-list-1 lone-comment
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 206. struct-declaration-list-1 => ";"
   (lambda ($1 . $rest) (make-tl 'field-list))
   ;; 207. struct-declaration-list-1 => struct-declaration-list-1 ";"
   (lambda ($2 $1 . $rest) $1)
   ;; 208. struct-declaration => struct-declaration-no-comment ";"
   (lambda ($2 $1 . $rest) $1)
   ;; 209. struct-declaration => struct-declaration-no-comment ";" code-comment
   (lambda ($3 $2 $1 . $rest) (sx-attr-add $1 $3))
   ;; 210. struct-declaration-no-comment => specifier-qualifier-list struct-decl...
   (lambda ($2 $1 . $rest)
     `(comp-decl ,$1 ,(tl->list $2)))
   ;; 211. struct-declaration-no-comment => specifier-qualifier-list
   (lambda ($1 . $rest) `(comp-decl ,$1))
   ;; 212. specifier-qualifier-list => specifier-qualifier-list-1
   (lambda ($1 . $rest)
     (process-specs (tl->list $1)))
   ;; 213. specifier-qualifier-list-1 => type-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; 214. specifier-qualifier-list-1 => type-specifier specifier-qualifier-list-1
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; 215. specifier-qualifier-list-1 => type-qualifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; 216. specifier-qualifier-list-1 => type-qualifier specifier-qualifier-list-1
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; 217. specifier-qualifier-list-1 => attribute-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; 218. specifier-qualifier-list-1 => attribute-specifier specifier-qualifier...
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; 219. specifier-qualifier-list/no-attr => specifier-qualifier-list/no-attr-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 220. specifier-qualifier-list/no-attr-1 => type-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; 221. specifier-qualifier-list/no-attr-1 => type-specifier specifier-qualif...
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; 222. specifier-qualifier-list/no-attr-1 => type-qualifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; 223. specifier-qualifier-list/no-attr-1 => type-qualifier specifier-qualif...
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; 224. struct-declarator-list => struct-declarator
   (lambda ($1 . $rest)
     (make-tl 'comp-declr-list $1))
   ;; 225. struct-declarator-list => struct-declarator-list "," struct-declarator
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 226. struct-declarator-list => struct-declarator-list "," attribute-specif...
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 $3 $4))
   ;; 227. struct-declarator => struct-declarator-1
   (lambda ($1 . $rest) (process-declr $1))
   ;; 228. struct-declarator-1 => declarator
   (lambda ($1 . $rest) `(comp-declr ,$1))
   ;; 229. struct-declarator-1 => declarator attribute-specifiers
   (lambda ($2 $1 . $rest) `(comp-declr ,$1 ,$2))
   ;; 230. struct-declarator-1 => declarator ":" constant-expression
   (lambda ($3 $2 $1 . $rest)
     `(comp-declr (bit-field ,$1 ,$3)))
   ;; 231. struct-declarator-1 => ":" constant-expression
   (lambda ($2 $1 . $rest)
     `(comp-declr (bit-field ,$2)))
   ;; 232. enum-specifier => "enum" ident-like "{" enumerator-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(enum-def ,$2 ,(tl->list $4)))
   ;; 233. enum-specifier => "enum" ident-like "{" enumerator-list "," "}"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(enum-def ,$2 ,(tl->list $4)))
   ;; 234. enum-specifier => "enum" "{" enumerator-list "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(enum-def ,(tl->list $3)))
   ;; 235. enum-specifier => "enum" "{" enumerator-list "," "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(enum-def ,(tl->list $3)))
   ;; 236. enum-specifier => "enum" ident-like
   (lambda ($2 $1 . $rest) `(enum-ref ,$2))
   ;; 237. enumerator-list => enumerator
   (lambda ($1 . $rest) (make-tl 'enum-def-list $1))
   ;; 238. enumerator-list => enumerator-list "," enumerator
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 239. enumerator => identifier
   (lambda ($1 . $rest) `(enum-defn ,$1))
   ;; 240. enumerator => identifier attribute-specifiers
   (lambda ($2 $1 . $rest) `(enum-defn ,$1 ,$2))
   ;; 241. enumerator => identifier "=" constant-expression
   (lambda ($3 $2 $1 . $rest) `(enum-defn ,$1 ,$3))
   ;; 242. enumerator => identifier attribute-specifiers "=" constant-expression
   (lambda ($4 $3 $2 $1 . $rest)
     `(enum-defn ,$1 ,$2 ,$4))
   ;; 243. type-qualifier => "const"
   (lambda ($1 . $rest) `(type-qual (const)))
   ;; 244. type-qualifier => "volatile"
   (lambda ($1 . $rest) `(type-qual (volatile)))
   ;; 245. type-qualifier => "restrict"
   (lambda ($1 . $rest) `(type-qual (restrict)))
   ;; 246. type-qualifier => "_Atomic"
   (lambda ($1 . $rest) `(type-qual (atomic)))
   ;; 247. function-specifier => "inline"
   (lambda ($1 . $rest) `(fctn-spec ,$1))
   ;; 248. function-specifier => "_Noreturn"
   (lambda ($1 . $rest) `(fctn-spec ,$1))
   ;; 249. attribute-specifiers => attribute-specifier
   (lambda ($1 . $rest) $1)
   ;; 250. attribute-specifiers => attribute-specifiers attribute-specifier
   (lambda ($2 $1 . $rest) (append $1 (cdr $2)))
   ;; 251. attribute-specifier => "__attribute__" "(" "(" attribute-list ")" ")"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) $4)
   ;; 252. attribute-specifier => attr-name
   (lambda ($1 . $rest)
     `(attribute-list (attribute ,$1)))
   ;; 253. attr-name => "__packed__"
   (lambda ($1 . $rest) '(ident "__packed__"))
   ;; 254. attr-name => "__aligned__"
   (lambda ($1 . $rest) '(ident "__aligned__"))
   ;; 255. attr-name => "__alignof__"
   (lambda ($1 . $rest) '(ident "__alignof__"))
   ;; 256. attribute-list => attribute-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 257. attribute-list-1 => attribute
   (lambda ($1 . $rest)
     (make-tl 'attribute-list $1))
   ;; 258. attribute-list-1 => attribute-list-1 "," attribute
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 259. attribute-list-1 => attribute-list-1 ","
   (lambda ($2 $1 . $rest) $1)
   ;; 260. attribute => attr-word
   (lambda ($1 . $rest) `(attribute ,$1))
   ;; 261. attribute => attr-word "(" attr-expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(attribute ,$1 ,$3))
   ;; 262. attribute => "const"
   (lambda ($1 . $rest)
     `(attribute (ident "const")))
   ;; 263. attr-word => attr-name
   (lambda ($1 . $rest) $1)
   ;; 264. attr-word => identifier
   (lambda ($1 . $rest) $1)
   ;; 265. attr-expr-list => attr-expr-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 266. attr-expr-list-1 => attribute-expr
   (lambda ($1 . $rest)
     (make-tl 'attr-expr-list $1))
   ;; 267. attr-expr-list-1 => attr-expr-list-1 "," attribute-expr
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 268. attribute-expr => type-name
   (lambda ($1 . $rest) $1)
   ;; 269. attribute-expr => string-literal
   (lambda ($1 . $rest) $1)
   ;; 270. attribute-expr => attr-word "(" attr-expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(attribute ,$1 ,$3))
   ;; 271. attribute-expr => attr-additive-expr
   (lambda ($1 . $rest) $1)
   ;; 272. attr-additive-expr => attr-multiplicative-expr
   (lambda ($1 . $rest) $1)
   ;; 273. attr-additive-expr => attr-additive-expr "+" attr-multiplicative-expr
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; 274. attr-additive-expr => attr-additive-expr "-" attr-multiplicative-expr
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; 275. attr-multiplicative-expr => attr-unary-expr
   (lambda ($1 . $rest) $1)
   ;; 276. attr-multiplicative-expr => attr-multiplicative-expr "*" attr-unary-expr
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 277. attr-multiplicative-expr => attr-multiplicative-expr "/" attr-unary-expr
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 278. attr-multiplicative-expr => attr-multiplicative-expr "%" attr-unary-expr
   (lambda ($3 $2 $1 . $rest) `(mod ,$1 ,$3))
   ;; 279. attr-unary-expr => attr-primary-expr
   (lambda ($1 . $rest) $1)
   ;; 280. attr-unary-expr => "sizeof" "(" type-name ")"
   (lambda ($4 $3 $2 $1 . $rest) `(sizeof-type ,$3))
   ;; 281. attr-unary-expr => "_Alignof" "(" type-name ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(alignof-type ,$3))
   ;; 282. attr-primary-expr => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; 283. attr-primary-expr => identifier
   (lambda ($1 . $rest) $1)
   ;; 284. init-declarator-list => init-declarator-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 285. init-declarator-list-1 => init-declarator
   (lambda ($1 . $rest)
     (make-tl 'init-declr-list $1))
   ;; 286. init-declarator-list-1 => init-declarator-list-1 "," init-declarator
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 287. init-declarator-list-1 => init-declarator-list-1 "," attribute-specif...
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 $3 $4))
   ;; 288. init-declarator => init-declarator-1
   (lambda ($1 . $rest) (process-declr $1))
   ;; 289. init-declarator-1 => declarator
   (lambda ($1 . $rest) `(init-declr ,$1))
   ;; 290. init-declarator-1 => declarator "=" initializer
   (lambda ($3 $2 $1 . $rest) `(init-declr ,$1 ,$3))
   ;; 291. init-declarator-1 => declarator asm-expression
   (lambda ($2 $1 . $rest) `(init-declr ,$1 ,$2))
   ;; 292. init-declarator-1 => declarator asm-expression "=" initializer
   (lambda ($4 $3 $2 $1 . $rest)
     `(init-declr ,$1 ,$2 ,$4))
   ;; 293. init-declarator-1 => declarator attribute-specifiers
   (lambda ($2 $1 . $rest) `(init-declr ,$1 ,$2))
   ;; 294. init-declarator-1 => declarator attribute-specifiers "=" initializer
   (lambda ($4 $3 $2 $1 . $rest)
     `(init-declr ,$1 ,$2 ,$4))
   ;; 295. init-declarator-1 => declarator asm-expression attribute-specifiers
   (lambda ($3 $2 $1 . $rest)
     `(init-declr ,$1 ,$2 ,$3))
   ;; 296. declarator => pointer direct-declarator
   (lambda ($2 $1 . $rest) `(ptr-declr ,$1 ,$2))
   ;; 297. declarator => direct-declarator
   (lambda ($1 . $rest) $1)
   ;; 298. pointer => "*" type-qualifier-list pointer
   (lambda ($3 $2 $1 . $rest) `(pointer ,$2 ,$3))
   ;; 299. pointer => "*" type-qualifier-list
   (lambda ($2 $1 . $rest) `(pointer ,$2))
   ;; 300. pointer => "*" attribute-specifiers pointer
   (lambda ($3 $2 $1 . $rest) `(pointer ,$3))
   ;; 301. pointer => "*" attribute-specifiers
   (lambda ($2 $1 . $rest) '(pointer))
   ;; 302. pointer => "*" pointer
   (lambda ($2 $1 . $rest) `(pointer ,$2))
   ;; 303. pointer => "*"
   (lambda ($1 . $rest) '(pointer))
   ;; 304. direct-declarator => identifier
   (lambda ($1 . $rest) $1)
   ;; 305. direct-declarator => "(" declarator ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 306. direct-declarator => "(" attribute-specifier declarator ")"
   (lambda ($4 $3 $2 $1 . $rest) $3)
   ;; 307. direct-declarator => direct-declarator "[" type-qualifier-list assign...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(ary-declr ,$1 ,$3 ,$4))
   ;; 308. direct-declarator => direct-declarator "[" type-qualifier-list "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ary-declr ,$1 ,$3))
   ;; 309. direct-declarator => direct-declarator "[" assignment-expression "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ary-declr ,$1 ,$3))
   ;; 310. direct-declarator => direct-declarator "[" "]"
   (lambda ($3 $2 $1 . $rest) `(ary-declr ,$1))
   ;; 311. direct-declarator => direct-declarator "[" "static" type-qualifier-li...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(ary-declr (@ (storage "static")) ,$1 ,$4 ,$5))
   ;; 312. direct-declarator => direct-declarator "[" "static" assignment-expres...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(ary-declr (@ (storage "static")) ,$1 ,$4))
   ;; 313. direct-declarator => direct-declarator "[" type-qualifier-list "stati...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(ary-declr (@ (storage "static")) ,$1 ,$3 ,$5))
   ;; 314. direct-declarator => direct-declarator "[" type-qualifier-list "*" "]"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(ary-declr ,$1 ,$3 (var-len)))
   ;; 315. direct-declarator => direct-declarator "[" "*" "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ary-declr ,$1 (var-len)))
   ;; 316. direct-declarator => direct-declarator "(" parameter-type-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ftn-declr ,$1 ,$3))
   ;; 317. direct-declarator => direct-declarator "(" identifier-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ftn-declr ,$1 ,$3))
   ;; 318. direct-declarator => direct-declarator "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(ftn-declr ,$1 (param-list)))
   ;; 319. type-qualifier-list => type-qualifier-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 320. type-qualifier-list-1 => type-qualifier
   (lambda ($1 . $rest)
     (make-tl 'type-qual-list $1))
   ;; 321. type-qualifier-list-1 => type-qualifier-list-1 type-qualifier
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 322. parameter-type-list => parameter-list
   (lambda ($1 . $rest) (tl->list $1))
   ;; 323. parameter-type-list => parameter-list "," "..."
   (lambda ($3 $2 $1 . $rest)
     (tl->list (tl-append $1 '(ellipsis))))
   ;; 324. parameter-list => parameter-declaration
   (lambda ($1 . $rest) (make-tl 'param-list $1))
   ;; 325. parameter-list => parameter-list "," parameter-declaration
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 326. parameter-declaration => declaration-specifiers declarator
   (lambda ($2 $1 . $rest)
     `(param-decl ,$1 (param-declr ,$2)))
   ;; 327. parameter-declaration => declaration-specifiers abstract-declarator
   (lambda ($2 $1 . $rest)
     `(param-decl ,$1 (param-declr ,$2)))
   ;; 328. parameter-declaration => declaration-specifiers
   (lambda ($1 . $rest) `(param-decl ,$1))
   ;; 329. parameter-declaration => declaration-specifiers declarator attribute-...
   (lambda ($3 $2 $1 . $rest)
     `(param-decl ,$1 (param-declr ,$2)))
   ;; 330. identifier-list => identifier-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 331. identifier-list-1 => identifier
   (lambda ($1 . $rest) (make-tl 'ident-list $1))
   ;; 332. identifier-list-1 => identifier-list-1 "," identifier
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 333. type-name => specifier-qualifier-list/no-attr abstract-declarator
   (lambda ($2 $1 . $rest) `(type-name ,$1 ,$2))
   ;; 334. type-name => specifier-qualifier-list/no-attr
   (lambda ($1 . $rest) `(type-name ,$1))
   ;; 335. abstract-declarator => pointer direct-abstract-declarator
   (lambda ($2 $1 . $rest) `(ptr-declr ,$1 ,$2))
   ;; 336. abstract-declarator => pointer
   (lambda ($1 . $rest) `(abs-ptr-declr ,$1))
   ;; 337. abstract-declarator => direct-abstract-declarator
   (lambda ($1 . $rest) $1)
   ;; 338. direct-abstract-declarator => "(" abstract-declarator ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 339. direct-abstract-declarator => direct-abstract-declarator "(" paramete...
   (lambda ($4 $3 $2 $1 . $rest)
     `(ftn-declr ,$1 ,$3))
   ;; 340. direct-abstract-declarator => direct-abstract-declarator "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(ftn-declr ,$1 (param-list)))
   ;; 341. direct-abstract-declarator => direct-abstract-declarator "[" type-qua...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(ary-declr ,$1 ,$3 ,$4))
   ;; 342. direct-abstract-declarator => direct-abstract-declarator "[" type-qua...
   (lambda ($4 $3 $2 $1 . $rest)
     `(ary-declr ,$1 ,$3))
   ;; 343. direct-abstract-declarator => direct-abstract-declarator "[" assignme...
   (lambda ($4 $3 $2 $1 . $rest)
     `(ary-declr ,$1 ,$3))
   ;; 344. direct-abstract-declarator => direct-abstract-declarator "[" "]"
   (lambda ($3 $2 $1 . $rest) `(ary-declr ,$1))
   ;; 345. direct-abstract-declarator => direct-abstract-declarator "[" "static"...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(ary-declr
        ,$1
        ,(tl->list (tl-insert $4 '(stor-spec (static))))
        ,$5))
   ;; 346. direct-abstract-declarator => direct-abstract-declarator "[" "static"...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(ary-declr
        ,$1
        ,(tl->list (tl-insert $4 '(stor-spec (static))))))
   ;; 347. direct-abstract-declarator => direct-abstract-declarator "[" type-qua...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(ary-declr
        ,$1
        ,(tl->list (tl-insert $3 '(stor-spec (static))))
        ,$5))
   ;; 348. direct-abstract-declarator => direct-abstract-declarator "[" "*" "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(star-ary-declr ,$1))
   ;; 349. direct-abstract-declarator => "(" parameter-type-list ")"
   (lambda ($3 $2 $1 . $rest) `(abs-ftn-declr ,$2))
   ;; 350. direct-abstract-declarator => "(" ")"
   (lambda ($2 $1 . $rest)
     '(abs-ftn-declr (param-list)))
   ;; 351. direct-abstract-declarator => "[" type-qualifier-list assignment-expr...
   (lambda ($4 $3 $2 $1 . $rest)
     `(abs-ary-declr ,$2 ,$3))
   ;; 352. direct-abstract-declarator => "[" type-qualifier-list "]"
   (lambda ($3 $2 $1 . $rest) `(abs-ary-declr ,$2))
   ;; 353. direct-abstract-declarator => "[" assignment-expression "]"
   (lambda ($3 $2 $1 . $rest) `(abs-ary-declr ,$2))
   ;; 354. direct-abstract-declarator => "[" "]"
   (lambda ($2 $1 . $rest) `(abs-ary-declr))
   ;; 355. direct-abstract-declarator => "[" "static" type-qualifier-list assign...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(abs-ary-declr
        ,(tl->list (tl-insert $3 '(stor-spec (static))))
        ,$4))
   ;; 356. direct-abstract-declarator => "[" "static" type-qualifier-list "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(abs-ary-declr
        ,(tl->list (tl-insert $3 '(stor-spec (static))))))
   ;; 357. direct-abstract-declarator => "[" type-qualifier-list "static" assign...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(abs-ary-declr
        ,(tl->list (tl-insert $2 '(stor-spec (static))))
        ,$4))
   ;; 358. direct-abstract-declarator => "[" "*" "]"
   (lambda ($3 $2 $1 . $rest) '(abs-star-ary-declr))
   ;; 359. typedef-name => 'typename
   (lambda ($1 . $rest) `(typename ,$1))
   ;; 360. initializer => assignment-expression
   (lambda ($1 . $rest) `(initzer ,$1))
   ;; 361. initializer => "{" initializer-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(initzer ,(tl->list $2)))
   ;; 362. initializer => "{" initializer-list "," "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(initzer ,(tl->list $2)))
   ;; 363. initializer-list => designation initializer
   (lambda ($2 $1 . $rest)
     (make-tl 'initzer-list $1 $2))
   ;; 364. initializer-list => initializer
   (lambda ($1 . $rest) (make-tl 'initzer-list $1))
   ;; 365. initializer-list => initializer-list "," designation initializer
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 $3 $4))
   ;; 366. initializer-list => initializer-list "," initializer
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 367. designation => designator-list "="
   (lambda ($2 $1 . $rest) `(desig ,$1))
   ;; 368. designator-list => designator
   (lambda ($1 . $rest) (make-tl 'desgr-list $1))
   ;; 369. designator-list => designator-list designator
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 370. designator => "[" constant-expression "]"
   (lambda ($3 $2 $1 . $rest) `(array-dsgr ,$2))
   ;; 371. designator => "." identifier
   (lambda ($2 $1 . $rest) `(sel-dsgr ,$2))
   ;; 372. statement => labeled-statement
   (lambda ($1 . $rest) $1)
   ;; 373. statement => compound-statement
   (lambda ($1 . $rest) $1)
   ;; 374. statement => expression-statement
   (lambda ($1 . $rest) $1)
   ;; 375. statement => selection-statement
   (lambda ($1 . $rest) $1)
   ;; 376. statement => iteration-statement
   (lambda ($1 . $rest) $1)
   ;; 377. statement => jump-statement
   (lambda ($1 . $rest) $1)
   ;; 378. statement => asm-statement
   (lambda ($1 . $rest) $1)
   ;; 379. statement => pragma
   (lambda ($1 . $rest) $1)
   ;; 380. statement => cpp-statement
   (lambda ($1 . $rest) $1)
   ;; 381. labeled-statement => identifier ":" statement
   (lambda ($3 $2 $1 . $rest)
     `(labeled-stmt ,$1 ,$3))
   ;; 382. labeled-statement => identifier ":" attribute-specifier statement
   (lambda ($4 $3 $2 $1 . $rest)
     `(labeled-stmt ,$1 ,$4))
   ;; 383. labeled-statement => "case" constant-expression ":" statement
   (lambda ($4 $3 $2 $1 . $rest) `(case ,$2 ,$4))
   ;; 384. labeled-statement => "default" ":" statement
   (lambda ($3 $2 $1 . $rest) `(default ,$3))
   ;; 385. compound-statement => "{" $P3 block-item-list $P4 "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(compd-stmt ,(tl->list $3)))
   ;; 386. compound-statement => "{" "}"
   (lambda ($2 $1 . $rest)
     `(compd-stmt (block-item-list)))
   ;; 387. $P3 => 
   (lambda ($1 . $rest) (cpi-push))
   ;; 388. $P4 => 
   (lambda ($3 $2 $1 . $rest) (cpi-pop))
   ;; 389. block-item-list => block-item
   (lambda ($1 . $rest)
     (make-tl 'block-item-list $1))
   ;; 390. block-item-list => block-item-list block-item
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 391. block-item => declaration
   (lambda ($1 . $rest) $1)
   ;; 392. block-item => statement
   (lambda ($1 . $rest) $1)
   ;; 393. expression-statement => expression ";"
   (lambda ($2 $1 . $rest) `(expr-stmt ,$1))
   ;; 394. expression-statement => ";"
   (lambda ($1 . $rest) '(expr-stmt))
   ;; 395. selection-statement => "if" "(" expression ")" statement
   (lambda ($5 $4 $3 $2 $1 . $rest) `(if ,$3 ,$5))
   ;; 396. selection-statement => "if" "(" expression ")" statement "else" state...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$3 ,$5 ,$7))
   ;; 397. selection-statement => "switch" "(" expression ")" statement
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(switch ,$3 ,$5))
   ;; 398. iteration-statement => "while" "(" expression ")" statement
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(while ,$3 ,$5))
   ;; 399. iteration-statement => "do" statement "while" "(" expression ")" ";"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(do-while ,$2 ,$5))
   ;; 400. iteration-statement => "for" "(" initial-clause opt-expression ";" op...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for ,$3 ,$4 ,$6 ,$8))
   ;; 401. initial-clause => expression ";"
   (lambda ($2 $1 . $rest) $1)
   ;; 402. initial-clause => ";"
   (lambda ($1 . $rest) '(expr))
   ;; 403. initial-clause => declaration
   (lambda ($1 . $rest) $1)
   ;; 404. opt-expression => 
   (lambda $rest '(expr))
   ;; 405. opt-expression => expression
   (lambda ($1 . $rest) $1)
   ;; 406. jump-statement => "goto" identifier ";"
   (lambda ($3 $2 $1 . $rest) `(goto ,$2))
   ;; 407. jump-statement => "continue" ";"
   (lambda ($2 $1 . $rest) '(continue))
   ;; 408. jump-statement => "break" ";"
   (lambda ($2 $1 . $rest) '(break))
   ;; 409. jump-statement => "return" expression ";"
   (lambda ($3 $2 $1 . $rest) `(return ,$2))
   ;; 410. jump-statement => "return" ";"
   (lambda ($2 $1 . $rest) `(return (expr)))
   ;; 411. asm-statement => asm-expression ";"
   (lambda ($2 $1 . $rest) `(expr-stmt ,$1))
   ;; 412. asm-expression => "__asm__" opt-asm-qualifiers "(" string-literal ")"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(asm-expr (@ (extension "GNUC") ,@$2) ,$4))
   ;; 413. asm-expression => "__asm__" opt-asm-qualifiers "(" string-literal asm...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(asm-expr (@ (extension "GNUC") ,@$2) ,$4 ,$5))
   ;; 414. asm-expression => "__asm__" opt-asm-qualifiers "(" string-literal asm...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(asm-expr
        (@ (extension "GNUC") ,@$2)
        ,$4
        ,$5
        ,$6))
   ;; 415. asm-expression => "__asm__" opt-asm-qualifiers "(" string-literal asm...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(asm-expr
        (@ (extension "GNUC") ,@$2)
        ,$4
        ,$5
        ,$6
        ,$7))
   ;; 416. asm-expression => "__asm__" opt-asm-qualifiers "(" string-literal asm...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(asm-expr
        (@ (extension "GNUC") ,@$2)
        ,$4
        (asm-outputs)
        ,$6
        ,$7
        ,$8))
   ;; 417. opt-asm-qualifiers => 
   (lambda $rest (list))
   ;; 418. opt-asm-qualifiers => "__volatile__"
   (lambda ($1 . $rest) (list '(volatile "true")))
   ;; 419. opt-asm-qualifiers => "__goto__"
   (lambda ($1 . $rest) (list '(goto "true")))
   ;; 420. opt-asm-qualifiers => "volatile"
   (lambda ($1 . $rest) (list '(volatile "true")))
   ;; 421. opt-asm-qualifiers => "goto"
   (lambda ($1 . $rest) (list '(goto "true")))
   ;; 422. asm-outputs => asm-outputs-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 423. asm-outputs-1 => ":"
   (lambda ($1 . $rest) (make-tl 'asm-outputs))
   ;; 424. asm-outputs-1 => ":" asm-output
   (lambda ($2 $1 . $rest)
     (make-tl 'asm-outputs $2))
   ;; 425. asm-outputs-1 => asm-outputs-1 "," asm-output
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 426. asm-output => string-literal "(" identifier ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(asm-operand ,$1 ,$3))
   ;; 427. asm-output => "[" identifier "]" string-literal "(" identifier ")"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(asm-operand ,$2 ,$4 ,$6))
   ;; 428. asm-inputs => asm-inputs-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 429. asm-inputs-1 => ":"
   (lambda ($1 . $rest) (make-tl 'asm-inputs))
   ;; 430. asm-inputs-1 => ":" asm-input
   (lambda ($2 $1 . $rest) (make-tl 'asm-inputs $2))
   ;; 431. asm-inputs-1 => asm-inputs-1 "," asm-input
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 432. asm-input => string-literal "(" expression ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(asm-operand ,$1 ,$3))
   ;; 433. asm-input => "[" identifier "]" string-literal "(" expression ")"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(asm-operand ,$2 ,$4 ,$6))
   ;; 434. asm-clobbers => asm-clobbers-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 435. asm-clobbers-1 => ":"
   (lambda ($1 . $rest) (make-tl 'asm-clobbers))
   ;; 436. asm-clobbers-1 => ":" string-literal
   (lambda ($2 $1 . $rest)
     (make-tl 'asm-clobbers $2))
   ;; 437. asm-clobbers-1 => asm-clobbers-1 "," string-literal
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 438. asm-gotos => asm-gotos-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 439. asm-gotos-1 => ":"
   (lambda ($1 . $rest) (make-tl 'asm-gotos))
   ;; 440. asm-gotos-1 => ":" identifier
   (lambda ($2 $1 . $rest) (make-tl 'asm-gotos $2))
   ;; 441. asm-gotos-1 => asm-gotos-1 "," identifier
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 442. translation-unit => external-declaration-list
   (lambda ($1 . $rest) (tl->list $1))
   ;; 443. external-declaration-list => 
   (lambda $rest (make-tl 'trans-unit))
   ;; 444. external-declaration-list => external-declaration-list external-decla...
   (lambda ($2 $1 . $rest)
     (if (eqv? (sx-tag $2) 'extern-block)
       (tl-extend $1 (sx-tail $2 1))
       (tl-append $1 $2)))
   ;; 445. external-declaration => function-definition
   (lambda ($1 . $rest) $1)
   ;; 446. external-declaration => declaration
   (lambda ($1 . $rest) $1)
   ;; 447. external-declaration => lone-comment
   (lambda ($1 . $rest) $1)
   ;; 448. external-declaration => cpp-statement
   (lambda ($1 . $rest) $1)
   ;; 449. external-declaration => pragma
   (lambda ($1 . $rest) $1)
   ;; 450. external-declaration => "extern" '$string "{" $P5 external-declaratio...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(extern-block
        (extern-begin ,$2)
        ,@(sx-tail (tl->list $5) 1)
        (extern-end)))
   ;; 451. external-declaration => ";"
   (lambda ($1 . $rest)
     `(decl (@ (extension "GNUC"))))
   ;; 452. $P5 => 
   (lambda ($3 $2 $1 . $rest) (cpi-dec-blev!))
   ;; 453. $P6 => 
   (lambda ($5 $4 $3 $2 $1 . $rest) (cpi-inc-blev!))
   ;; 454. function-definition => declaration-specifiers declarator compound-sta...
   (lambda ($3 $2 $1 . $rest)
     `(fctn-defn ,$1 ,$2 ,$3))
   ;; 455. identifier => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; 456. constant => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; 457. constant => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; 458. constant => '$chlit
   (lambda ($1 . $rest) `(char ,$1))
   ;; 459. constant => '$chlit/L
   (lambda ($1 . $rest)
     `(char (@ (type "wchar_t")) ,$1))
   ;; 460. constant => '$chlit/u
   (lambda ($1 . $rest)
     `(char (@ (type "char16_t")) ,$1))
   ;; 461. constant => '$chlit/U
   (lambda ($1 . $rest)
     `(char (@ (type "char32_t")) ,$1))
   ;; 462. string-literal => string-literal-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 463. string-literal-1 => '$string
   (lambda ($1 . $rest) (make-tl 'string $1))
   ;; 464. string-literal-1 => string-literal-1 '$string
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 465. code-comment => '$code-comm
   (lambda ($1 . $rest) `(comment ,$1))
   ;; 466. lone-comment => '$lone-comm
   (lambda ($1 . $rest) `(comment ,$1))
   ;; 467. cpp-statement => 'cpp-stmt
   (lambda ($1 . $rest) `(cpp-stmt ,$1))
   ;; 468. pragma => '$pragma
   (lambda ($1 . $rest) `(pragma ,$1))
   ))

;; --- last line ---
