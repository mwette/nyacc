;; tsh-file-act.scm

;; Copyright (C) 2021-2023 Matthew R. Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING included with the this distribution.

(define tsh-file-act-v
  (vector
   ;; 0. $start => top
   (lambda ($1 . $rest) $1)
   ;; 1. top => script
   (lambda ($1 . $rest) $1)
   ;; 2. script => script-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 3. script-1 => item
   (lambda ($1 . $rest) (make-tl 'script $1))
   ;; 4. script-1 => script-1 item
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 5. item => topl-decl term
   (lambda ($2 $1 . $rest) $1)
   ;; 6. item => stmt term
   (lambda ($2 $1 . $rest) $1)
   ;; 7. topl-decl => "source" string
   (lambda ($2 $1 . $rest) `(source ,$2))
   ;; 8. topl-decl => "use" path
   (lambda ($2 $1 . $rest) `(use ,@(cdr $2)))
   ;; 9. stmt => decl-stmt
   (lambda ($1 . $rest) $1)
   ;; 10. stmt => exec-stmt
   (lambda ($1 . $rest) $1)
   ;; 11. stmt => fill-stmt
   (lambda ($1 . $rest) $1)
   ;; 12. proc-stmt-list => fill-stmt-list/term decl-stmt-list/term exec-stmt-list
   (lambda ($3 $2 $1 . $rest) `(stmt-list ,@(cdr $1) ,@(cdr $2) ,@(cdr $3)))
   ;; 13. proc-stmt-list => decl-stmt-list/term exec-stmt-list
   (lambda ($2 $1 . $rest) `(stmt-list ,@(cdr $1) ,@(cdr $2)))
   ;; 14. proc-stmt-list => fill-stmt-list/term exec-stmt-list
   (lambda ($2 $1 . $rest) `(stmt-list ,@(cdr $1) ,@(cdr $2)))
   ;; 15. proc-stmt-list => exec-stmt-list
   (lambda ($1 . $rest) `(stmt-list ,@(cdr $1)))
   ;; 16. proc-stmt-list => 
   (lambda $rest `(stmt-list (empty-stmt)))
   ;; 17. block-stmt-list => fill-stmt-list/term exec-stmt-list
   (lambda ($2 $1 . $rest) `(stmt-list ,@(cdr $1) ,@(cdr $2)))
   ;; 18. block-stmt-list => exec-stmt-list
   (lambda ($1 . $rest) `(stmt-list ,@(cdr $1)))
   ;; 19. fill-stmt => 
   (lambda $rest `(empty-stmt))
   ;; 20. fill-stmt => '$lone-comm
   (lambda ($1 . $rest) `(comment ,$1))
   ;; 21. fill-stmt-list/term => fill-stmt-list/term-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 22. fill-stmt-list/term-1 => fill-stmt term
   (lambda ($2 $1 . $rest) (make-tl `stmt-list $1))
   ;; 23. fill-stmt-list/term-1 => fill-stmt-list/term-1 lone-comm term
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; 24. fill-stmt-list/term-1 => fill-stmt-list/term-1 term
   (lambda ($2 $1 . $rest) $1)
   ;; 25. decl-stmt-list/term => decl-stmt-list/term-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 26. decl-stmt-list/term-1 => decl-stmt term
   (lambda ($2 $1 . $rest) (make-tl 'stmt-list $1))
   ;; 27. decl-stmt-list/term-1 => decl-stmt-list/term-1 decl-stmt term
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; 28. decl-stmt-list/term-1 => decl-stmt-list/term-1 lone-comm term
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; 29. decl-stmt-list/term-1 => decl-stmt-list/term-1 term
   (lambda ($2 $1 . $rest) $1)
   ;; 30. exec-stmt-list => exec-stmt-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 31. exec-stmt-list-1 => exec-stmt
   (lambda ($1 . $rest) (make-tl 'stmt-list $1))
   ;; 32. exec-stmt-list-1 => exec-stmt-list-1 term exec-stmt
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 33. exec-stmt-list-1 => exec-stmt-list-1 term lone-comm
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 34. exec-stmt-list-1 => exec-stmt-list-1 term
   (lambda ($2 $1 . $rest) $1)
   ;; 35. decl-stmt => "proc" ident "(" arg-list ")" "{" proc-stmt-list "}"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest) `(proc ,$2 ,$4 ,$7))
   ;; 36. decl-stmt => "global" name-seq
   (lambda ($2 $1 . $rest) `(global ,@(cdr $2)))
   ;; 37. decl-stmt => "nonlocal" name-seq
   (lambda ($2 $1 . $rest) `(nonlocal ,@(cdr $2)))
   ;; 38. decl-stmt => "local" name-seq
   (lambda ($2 $1 . $rest) `(local ,@(cdr $2)))
   ;; 39. arg-list => 
   (lambda $rest (make-tl 'arg-list))
   ;; 40. arg-list => arg-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 41. arg-list-1 => ident
   (lambda ($1 . $rest) (make-tl 'arg-list $1))
   ;; 42. arg-list-1 => arg-list-1 "," ident
   (lambda ($3 $2 $1 . $rest) (tl-append $1 `(arg ,$3)))
   ;; 43. name-seq => name-seq-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 44. name-seq-1 => '$ident
   (lambda ($1 . $rest) (make-tl 'name-seq $1))
   ;; 45. name-seq-1 => name-seq-1 '$ident
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 46. exec-stmt => "set" ident unit-expr
   (lambda ($3 $2 $1 . $rest) `(set ,$2 ,$3))
   ;; 47. exec-stmt => "set" '$deref/ix "(" expr-list ")" unit-expr
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(set-indexed
       (ident ,$2)
       ,(if (eq? 'expr (sx-tag $4)) `(expr-list ,$4) $4)
       ,$6))
   ;; 48. exec-stmt => ident expr-seq
   (lambda ($2 $1 . $rest) `(call ,$1 ,@(cdr $2)))
   ;; 49. exec-stmt => "lambda" "(" arg-list ")" "{" proc-stmt-list "}"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) `(lambda ,$3 ,$6))
   ;; 50. exec-stmt => "(" expr-list ")"
   (lambda ($3 $2 $1 . $rest) `(last ,$2))
   ;; 51. exec-stmt => if-stmt
   (lambda ($1 . $rest) $1)
   ;; 52. exec-stmt => "switch" unit-expr "{" case-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(switch ,$2 ,@(cdr $4)))
   ;; 53. exec-stmt => "while" unit-expr "{" block-stmt-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(while ,$2 ,$4))
   ;; 54. exec-stmt => "for" "{" block-stmt-list "}" "{" unit-expr "}" "{" bloc...
   (lambda ($13 $12 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for ,$3 ,$6 ,$9 ,$12))
   ;; 55. exec-stmt => "format" expr-seq
   (lambda ($2 $1 . $rest) `(format unquote (cdr $2)))
   ;; 56. exec-stmt => "return"
   (lambda ($1 . $rest) `(return))
   ;; 57. exec-stmt => "return" unit-expr
   (lambda ($2 $1 . $rest) `(return ,$2))
   ;; 58. exec-stmt => "incr" ident
   (lambda ($2 $1 . $rest) `(incr ,$2))
   ;; 59. exec-stmt => "incr" ident unit-expr
   (lambda ($3 $2 $1 . $rest) `(incr ,$2 ,$3))
   ;; 60. if-stmt => "if" unit-expr "{" block-stmt-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4))
   ;; 61. if-stmt => "if" unit-expr "{" block-stmt-list "}" "else" "{" block-st...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4 (else ,$8)))
   ;; 62. if-stmt => "if" unit-expr "{" block-stmt-list "}" elseif-list
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4 ,@(sx-tail $6)))
   ;; 63. if-stmt => "if" unit-expr "{" block-stmt-list "}" elseif-list "else" ...
   (lambda ($10 $9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,@(sx-tail $6) (else ,$9)))
   ;; 64. elseif-list => elseif-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 65. elseif-list-1 => "elseif" unit-expr "{" block-stmt-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (make-tl 'elseif-list `(elseif ,$2 ,$4)))
   ;; 66. elseif-list-1 => elseif-list-1 "elseif" unit-expr "{" block-stmt-list...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (tl-append $1 'elseif-list `(elseif ,$2 ,$4)))
   ;; 67. case-list => case-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 68. case-list => case-list-1 default-case-expr
   (lambda ($2 $1 . $rest) (append (tl->list $1) (list $2)))
   ;; 69. case-list-1 => case-expr
   (lambda ($1 . $rest) (make-tl 'case-list $1))
   ;; 70. case-list-1 => term
   (lambda ($1 . $rest) (make-tl 'case-list))
   ;; 71. case-list-1 => case-list-1 case-expr
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 72. case-list-1 => case-list-1 term
   (lambda ($2 $1 . $rest) $1)
   ;; 73. case-expr => unit-expr unit-expr
   (lambda ($2 $1 . $rest) `(case ,$1 ,$2))
   ;; 74. case-expr => unit-expr "{" block-stmt-list "}"
   (lambda ($4 $3 $2 $1 . $rest) `(case ,$1 ,$3))
   ;; 75. default-case-expr => "default" unit-expr
   (lambda ($2 $1 . $rest) `(case (default) ,$2))
   ;; 76. unit-expr => primary-expression
   (lambda ($1 . $rest) `(expr ,$1))
   ;; 77. expression => logical-or-expression
   (lambda ($1 . $rest) $1)
   ;; 78. logical-or-expression => logical-and-expression
   (lambda ($1 . $rest) $1)
   ;; 79. logical-or-expression => logical-or-expression "||" logical-and-expre...
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; 80. logical-and-expression => bitwise-or-expression
   (lambda ($1 . $rest) $1)
   ;; 81. logical-and-expression => logical-and-expression "&&" bitwise-or-expr...
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; 82. bitwise-or-expression => bitwise-xor-expression
   (lambda ($1 . $rest) $1)
   ;; 83. bitwise-or-expression => bitwise-or-expression "|" bitwise-xor-expres...
   (lambda ($3 $2 $1 . $rest) `(bitwise-or ,$1 ,$3))
   ;; 84. bitwise-xor-expression => bitwise-and-expression
   (lambda ($1 . $rest) $1)
   ;; 85. bitwise-xor-expression => bitwise-xor-expression "^" bitwise-and-expr...
   (lambda ($3 $2 $1 . $rest) `(bitwise-xor ,$1 ,$3))
   ;; 86. bitwise-and-expression => equality-expression
   (lambda ($1 . $rest) $1)
   ;; 87. bitwise-and-expression => bitwise-and-expression "&" equality-expression
   (lambda ($3 $2 $1 . $rest) `(bitwise-and ,$1 ,$3))
   ;; 88. equality-expression => relational-expression
   (lambda ($1 . $rest) $1)
   ;; 89. equality-expression => equality-expression "==" relational-expression
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; 90. equality-expression => equality-expression "!=" relational-expression
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; 91. relational-expression => shift-expression
   (lambda ($1 . $rest) $1)
   ;; 92. relational-expression => relational-expression "<" shift-expression
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; 93. relational-expression => relational-expression "<=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; 94. relational-expression => relational-expression ">" shift-expression
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; 95. relational-expression => relational-expression ">=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; 96. shift-expression => additive-expression
   (lambda ($1 . $rest) $1)
   ;; 97. shift-expression => shift-expression "<<" additive-expression
   (lambda ($3 $2 $1 . $rest) `(lshift ,$1 ,$3))
   ;; 98. shift-expression => shift-expression ">>" additive-expression
   (lambda ($3 $2 $1 . $rest) `(rshift ,$1 ,$3))
   ;; 99. additive-expression => multiplicative-expression
   (lambda ($1 . $rest) $1)
   ;; 100. additive-expression => additive-expression "+" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; 101. additive-expression => additive-expression "-" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; 102. multiplicative-expression => unary-expression
   (lambda ($1 . $rest) $1)
   ;; 103. multiplicative-expression => multiplicative-expression "*" unary-expr...
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 104. multiplicative-expression => multiplicative-expression "/" unary-expr...
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 105. multiplicative-expression => multiplicative-expression "%" unary-expr...
   (lambda ($3 $2 $1 . $rest) `(mod ,$1 ,$3))
   ;; 106. unary-expression => primary-expression
   (lambda ($1 . $rest) $1)
   ;; 107. unary-expression => "-" unary-expression
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; 108. unary-expression => "+" unary-expression
   (lambda ($2 $1 . $rest) `(pos ,$2))
   ;; 109. unary-expression => "!" unary-expression
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; 110. unary-expression => "~" unary-expression
   (lambda ($2 $1 . $rest) `(bitwise-not ,$2))
   ;; 111. primary-expression => '$deref
   (lambda ($1 . $rest) `(deref ,$1))
   ;; 112. primary-expression => '$deref/ix "(" expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest) `(deref-indexed ,$1 ,$3))
   ;; 113. primary-expression => fixed
   (lambda ($1 . $rest) $1)
   ;; 114. primary-expression => float
   (lambda ($1 . $rest) $1)
   ;; 115. primary-expression => string
   (lambda ($1 . $rest) $1)
   ;; 116. primary-expression => symbol
   (lambda ($1 . $rest) $1)
   ;; 117. primary-expression => keychar
   (lambda ($1 . $rest) $1)
   ;; 118. primary-expression => keyword
   (lambda ($1 . $rest) $1)
   ;; 119. primary-expression => "(" expr-list ")"
   (lambda ($3 $2 $1 . $rest) `(last ,$2))
   ;; 120. primary-expression => "[" exec-stmt "]"
   (lambda ($3 $2 $1 . $rest) `(eval ,$2))
   ;; 121. expr-list => expr-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 122. expr-list => expr-list-1 ","
   (lambda ($2 $1 . $rest) (tl->list $1))
   ;; 123. expr-list-1 => expression
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; 124. expr-list-1 => expr-list-1 "," expression
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 125. expr-seq => expr-seq-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 126. expr-seq-1 => 
   (lambda $rest (make-tl 'seq-list))
   ;; 127. expr-seq-1 => expr-seq-1 primary-expression
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 128. path => path-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 129. path-1 => '$ident
   (lambda ($1 . $rest) (make-tl 'path $1))
   ;; 130. path-1 => '$string
   (lambda ($1 . $rest) (make-tl 'path $1))
   ;; 131. path-1 => path-1 'no-ws "::" 'no-ws '$ident
   (lambda ($5 $4 $3 $2 $1 . $rest) (tl-append $1 $5))
   ;; 132. path-1 => path-1 'no-ws "::" 'no-ws '$string
   (lambda ($5 $4 $3 $2 $1 . $rest) (tl-append $1 $5))
   ;; 133. ident => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; 134. fixed => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; 135. float => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; 136. string => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ;; 137. symbol => ident
   (lambda ($1 . $rest) $1)
   ;; 138. keychar => '$keychar
   (lambda ($1 . $rest) `(keychar ,$1))
   ;; 139. keyword => '$keyword
   (lambda ($1 . $rest) `(keyword ,$1))
   ;; 140. lone-comm => '$lone-comm
   (lambda ($1 . $rest) `(comment ,$1))
   ;; 141. term => ";"
   (lambda ($1 . $rest) $1)
   ;; 142. term => "\n"
   (lambda ($1 . $rest) $1)
   ))

;; --- last line ---
