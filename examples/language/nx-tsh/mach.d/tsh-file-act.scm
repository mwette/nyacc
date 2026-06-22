;; tsh-file-act.scm

;; Copyright (C) 2021-2023 Matthew R. Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file LICENSE included with the this distribution.

(define tsh-file-act-v
  (vector
   ;; 0. $start => top
   (lambda ($1 . $rest) $1)
   ;; 1. top => script
   (lambda ($1 . $rest) $1)
   ;; 2. script => script-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 3. script-1 => script-stmt
   (lambda ($1 . $rest) (make-tl 'script $1))
   ;; 4. script-1 => script-1 script-stmt
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 5. script-stmt => user-stmt
   (lambda ($1 . $rest) $1)
   ;; 6. script-stmt => lone-comm
   (lambda ($1 . $rest) $1)
   ;; 7. user-stmt => "source" string
   (lambda ($2 $1 . $rest) `(source ,$2))
   ;; 8. user-stmt => "use" path
   (lambda ($2 $1 . $rest) `(use ,@(cdr $2)))
   ;; 9. user-stmt => stmt term
   (lambda ($2 $1 . $rest) $1)
   ;; 10. stmt => decl-stmt
   (lambda ($1 . $rest) $1)
   ;; 11. stmt => exec-stmt
   (lambda ($1 . $rest) $1)
   ;; 12. stmt => 
   (lambda $rest `(empty-stmt))
   ;; 13. stmt-list => stmt-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 14. stmt-list-1 => stmt
   (lambda ($1 . $rest) (make-tl 'stmt-list $1))
   ;; 15. stmt-list-1 => stmt-list-1 term stmt
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 16. decl-stmt => "proc" ident "(" arg-list ")" "{" stmt-list "}"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest) `(proc ,$2 ,$4 ,$7))
   ;; 17. decl-stmt => "global" name-seq
   (lambda ($2 $1 . $rest) `(global ,@(cdr $2)))
   ;; 18. decl-stmt => "nonlocal" name-seq
   (lambda ($2 $1 . $rest) `(nonlocal ,@(cdr $2)))
   ;; 19. decl-stmt => "local" name-seq
   (lambda ($2 $1 . $rest) `(local ,@(cdr $2)))
   ;; 20. arg-list => 
   (lambda $rest (make-tl 'arg-list))
   ;; 21. arg-list => arg-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 22. arg-list-1 => ident
   (lambda ($1 . $rest) (make-tl 'arg-list $1))
   ;; 23. arg-list-1 => arg-list-1 "," ident
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 24. name-seq => name-seq-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 25. name-seq-1 => '$ident
   (lambda ($1 . $rest) (make-tl 'name-seq $1))
   ;; 26. name-seq-1 => name-seq-1 '$ident
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 27. exec-stmt => "set" ident unit-expr
   (lambda ($3 $2 $1 . $rest) `(set ,$2 ,$3))
   ;; 28. exec-stmt => "set" '$deref/ix "(" expr-list ")" unit-expr
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(set-indexed
       (ident ,$2)
       ,(if (eq? 'expr (sx-tag $4)) `(expr-list ,$4) $4)
       ,$6))
   ;; 29. exec-stmt => ident expr-seq
   (lambda ($2 $1 . $rest) `(call ,$1 ,@(cdr $2)))
   ;; 30. exec-stmt => "lambda" "(" arg-list ")" "{" stmt-list "}"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) `(lambda ,$3 ,$6))
   ;; 31. exec-stmt => "(" expr-list ")"
   (lambda ($3 $2 $1 . $rest) `(last ,$2))
   ;; 32. exec-stmt => if-stmt
   (lambda ($1 . $rest) $1)
   ;; 33. exec-stmt => "switch" unit-expr "{" case-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(switch ,$2 ,@(cdr $4)))
   ;; 34. exec-stmt => "while" unit-expr "{" stmt-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(while ,$2 ,$4))
   ;; 35. exec-stmt => "for" "{" stmt-list "}" "{" unit-expr "}" "{" stmt-list ...
   (lambda ($13 $12 $11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for ,$3 ,$6 ,$9 ,$12))
   ;; 36. exec-stmt => "format" expr-seq
   (lambda ($2 $1 . $rest) `(format unquote (cdr $2)))
   ;; 37. exec-stmt => "return"
   (lambda ($1 . $rest) `(return))
   ;; 38. exec-stmt => "return" unit-expr
   (lambda ($2 $1 . $rest) `(return ,$2))
   ;; 39. exec-stmt => "incr" ident
   (lambda ($2 $1 . $rest) `(incr ,$2))
   ;; 40. exec-stmt => "incr" ident unit-expr
   (lambda ($3 $2 $1 . $rest) `(incr ,$2 ,$3))
   ;; 41. if-stmt => "if" unit-expr "{" stmt-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4))
   ;; 42. if-stmt => "if" unit-expr "{" stmt-list "}" "else" "{" stmt-list "}"
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4 (else ,$8)))
   ;; 43. if-stmt => "if" unit-expr "{" stmt-list "}" elseif-list
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4 ,@(sx-tail $6)))
   ;; 44. if-stmt => "if" unit-expr "{" stmt-list "}" elseif-list "else" "{" st...
   (lambda ($10 $9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,@(sx-tail $6) (else ,$9)))
   ;; 45. elseif-list => elseif-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 46. elseif-list-1 => "elseif" unit-expr "{" stmt-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (make-tl 'elseif-list `(elseif ,$2 ,$4)))
   ;; 47. elseif-list-1 => elseif-list-1 "elseif" unit-expr "{" stmt-list "}"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (tl-append $1 'elseif-list `(elseif ,$2 ,$4)))
   ;; 48. case-list => case-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 49. case-list => case-list-1 default-case-expr
   (lambda ($2 $1 . $rest) (append (tl->list $1) (list $2)))
   ;; 50. case-list-1 => case-expr
   (lambda ($1 . $rest) (make-tl 'case-list $1))
   ;; 51. case-list-1 => term
   (lambda ($1 . $rest) (make-tl 'case-list))
   ;; 52. case-list-1 => case-list-1 case-expr
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 53. case-list-1 => case-list-1 term
   (lambda ($2 $1 . $rest) $1)
   ;; 54. case-expr => unit-expr unit-expr
   (lambda ($2 $1 . $rest) `(case ,$1 ,$2))
   ;; 55. case-expr => unit-expr "{" stmt-list "}"
   (lambda ($4 $3 $2 $1 . $rest) `(case ,$1 ,$3))
   ;; 56. default-case-expr => "default" unit-expr
   (lambda ($2 $1 . $rest) `(case (default) ,$2))
   ;; 57. unit-expr => primary-expression
   (lambda ($1 . $rest) `(expr ,$1))
   ;; 58. expression => logical-or-expression
   (lambda ($1 . $rest) $1)
   ;; 59. logical-or-expression => logical-and-expression
   (lambda ($1 . $rest) $1)
   ;; 60. logical-or-expression => logical-or-expression "||" logical-and-expre...
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; 61. logical-and-expression => bitwise-or-expression
   (lambda ($1 . $rest) $1)
   ;; 62. logical-and-expression => logical-and-expression "&&" bitwise-or-expr...
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; 63. bitwise-or-expression => bitwise-xor-expression
   (lambda ($1 . $rest) $1)
   ;; 64. bitwise-or-expression => bitwise-or-expression "|" bitwise-xor-expres...
   (lambda ($3 $2 $1 . $rest) `(bitwise-or ,$1 ,$3))
   ;; 65. bitwise-xor-expression => bitwise-and-expression
   (lambda ($1 . $rest) $1)
   ;; 66. bitwise-xor-expression => bitwise-xor-expression "^" bitwise-and-expr...
   (lambda ($3 $2 $1 . $rest) `(bitwise-xor ,$1 ,$3))
   ;; 67. bitwise-and-expression => equality-expression
   (lambda ($1 . $rest) $1)
   ;; 68. bitwise-and-expression => bitwise-and-expression "&" equality-expression
   (lambda ($3 $2 $1 . $rest) `(bitwise-and ,$1 ,$3))
   ;; 69. equality-expression => relational-expression
   (lambda ($1 . $rest) $1)
   ;; 70. equality-expression => equality-expression "==" relational-expression
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; 71. equality-expression => equality-expression "!=" relational-expression
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; 72. relational-expression => shift-expression
   (lambda ($1 . $rest) $1)
   ;; 73. relational-expression => relational-expression "<" shift-expression
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; 74. relational-expression => relational-expression "<=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; 75. relational-expression => relational-expression ">" shift-expression
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; 76. relational-expression => relational-expression ">=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; 77. shift-expression => additive-expression
   (lambda ($1 . $rest) $1)
   ;; 78. shift-expression => shift-expression "<<" additive-expression
   (lambda ($3 $2 $1 . $rest) `(lshift ,$1 ,$3))
   ;; 79. shift-expression => shift-expression ">>" additive-expression
   (lambda ($3 $2 $1 . $rest) `(rshift ,$1 ,$3))
   ;; 80. additive-expression => multiplicative-expression
   (lambda ($1 . $rest) $1)
   ;; 81. additive-expression => additive-expression "+" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; 82. additive-expression => additive-expression "-" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; 83. multiplicative-expression => unary-expression
   (lambda ($1 . $rest) $1)
   ;; 84. multiplicative-expression => multiplicative-expression "*" unary-expr...
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 85. multiplicative-expression => multiplicative-expression "/" unary-expr...
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 86. multiplicative-expression => multiplicative-expression "%" unary-expr...
   (lambda ($3 $2 $1 . $rest) `(mod ,$1 ,$3))
   ;; 87. unary-expression => primary-expression
   (lambda ($1 . $rest) $1)
   ;; 88. unary-expression => "-" unary-expression
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; 89. unary-expression => "+" unary-expression
   (lambda ($2 $1 . $rest) `(pos ,$2))
   ;; 90. unary-expression => "!" unary-expression
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; 91. unary-expression => "~" unary-expression
   (lambda ($2 $1 . $rest) `(bitwise-not ,$2))
   ;; 92. primary-expression => "$" 'no-ws ident
   (lambda ($3 $2 $1 . $rest) `(deref ,(sx-ref $3 1)))
   ;; 93. primary-expression => "$" 'no-ws ident 'no-ws "(" expr-list ")"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(deref-indexed ,(sx-ref $3 1) ,$6))
   ;; 94. primary-expression => "$" 'no-ws "(" unit-expr ")" 'no-ws "(" expr-li...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(deref-indexed-expr ,$4 ,$8))
   ;; 95. primary-expression => fixed
   (lambda ($1 . $rest) $1)
   ;; 96. primary-expression => float
   (lambda ($1 . $rest) $1)
   ;; 97. primary-expression => string
   (lambda ($1 . $rest) $1)
   ;; 98. primary-expression => symbol
   (lambda ($1 . $rest) $1)
   ;; 99. primary-expression => keychar
   (lambda ($1 . $rest) $1)
   ;; 100. primary-expression => keyword
   (lambda ($1 . $rest) $1)
   ;; 101. primary-expression => "(" expr-list ")"
   (lambda ($3 $2 $1 . $rest) `(last ,$2))
   ;; 102. primary-expression => "[" exec-stmt "]"
   (lambda ($3 $2 $1 . $rest) `(eval ,$2))
   ;; 103. expr-list => expr-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 104. expr-list => expr-list-1 ","
   (lambda ($2 $1 . $rest) (tl->list $1))
   ;; 105. expr-list-1 => expression
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; 106. expr-list-1 => expr-list-1 "," expression
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 107. expr-seq => expr-seq-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 108. expr-seq-1 => 
   (lambda $rest (make-tl 'seq-list))
   ;; 109. expr-seq-1 => expr-seq-1 primary-expression
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 110. path => path-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 111. path-1 => '$ident
   (lambda ($1 . $rest) (make-tl 'path $1))
   ;; 112. path-1 => '$string
   (lambda ($1 . $rest) (make-tl 'path $1))
   ;; 113. path-1 => path-1 'no-ws "::" 'no-ws '$ident
   (lambda ($5 $4 $3 $2 $1 . $rest) (tl-append $1 $5))
   ;; 114. path-1 => path-1 'no-ws "::" 'no-ws '$string
   (lambda ($5 $4 $3 $2 $1 . $rest) (tl-append $1 $5))
   ;; 115. ident => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; 116. fixed => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; 117. float => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; 118. string => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ;; 119. symbol => ident
   (lambda ($1 . $rest) $1)
   ;; 120. keychar => '$keychar
   (lambda ($1 . $rest) `(keychar ,$1))
   ;; 121. keyword => '$keyword
   (lambda ($1 . $rest) `(keyword ,$1))
   ;; 122. lone-comm => '$lone-comm
   (lambda ($1 . $rest) `(comment ,$1))
   ;; 123. term => ";"
   (lambda ($1 . $rest) $1)
   ;; 124. term => "\n"
   (lambda ($1 . $rest) $1)
   ))

;; --- last line ---
