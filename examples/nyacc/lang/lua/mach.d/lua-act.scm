;; lua-act.scm

(define lua-act-v
  (vector
   ;; 0. $start => block
   (lambda ($1 . $rest) $1)
   ;; 1. block => stmt-list-1
   (lambda ($1 . $rest)
     `(block ,@(cdr (tl->list $1))))
   ;; 2. block => stmt-list-1 finish
   (lambda ($2 $1 . $rest)
     `(block ,@(cdr (tl->list $1)) ,$2))
   ;; 3. stmt-list-1 => stmt opt-semi
   (lambda ($2 $1 . $rest) (make-tl 'stmt-list $1))
   ;; 4. stmt-list-1 => stmt-list-1 stmt opt-semi
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; 5. finish => finish-1 opt-semi
   (lambda ($2 $1 . $rest) $1)
   ;; 6. finish-1 => "return"
   (lambda ($1 . $rest) `(return))
   ;; 7. finish-1 => "return" exprs
   (lambda ($2 $1 . $rest) `(return ,$2))
   ;; 8. finish-1 => "break"
   (lambda ($1 . $rest) `(break))
   ;; 9. finish-1 => "break" name
   (lambda ($2 $1 . $rest) `(break ,$1))
   ;; 10. stmt => var-list "=" exprs
   (lambda ($3 $2 $1 . $rest) `(assn ,$1 ,$3))
   ;; 11. stmt => call
   (lambda ($1 . $rest) $1)
   ;; 12. stmt => "do" block "end"
   (lambda ($3 $2 $1 . $rest) `(do ,$2))
   ;; 13. stmt => "while" expr "do" block "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(while ,$2 ,$4))
   ;; 14. stmt => "repeat" block "until" expr
   (lambda ($4 $3 $2 $1 . $rest)
     `(repeat-until ,$4 ,$2))
   ;; 15. stmt => "if" expr "then" block "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4))
   ;; 16. stmt => "if" expr "then" block "else" block "end"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,$6))
   ;; 17. stmt => "if" expr "then" block elseif-list "end"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,$5))
   ;; 18. stmt => "if" expr "then" block elseif-list "else" block "end"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,$5 ,$7))
   ;; 19. stmt => "for" name "=" expr "," expr "do" block "end"
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for ,$2 (range ,$4 ,$6) ,$8))
   ;; 20. stmt => "for" name "=" expr "," expr "," expr "do" block "end"
   (lambda ($11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for ,$2 (range ,$4 ,$6 ,$8) ,$10))
   ;; 21. stmt => "for" name "," name "in" expr "do" block "end"
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for-in ,$ ,$))
   ;; 22. stmt => "function" func-name "(" ")" block "end"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(function ,$2 (params) ,$5))
   ;; 23. stmt => "function" func-name "(" params ")" block "end"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(function ,$2 ,$3 ,$4))
   ;; 24. stmt => "local" name-list
   (lambda ($2 $1 . $rest) `(local ,$2))
   ;; 25. stmt => "local" name-list "=" exprs
   (lambda ($4 $3 $2 $1 . $rest) `(local ,$2 ,$4))
   ;; 26. elseif-list => elseif-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 27. elseif-list-1 => elseif
   (lambda ($1 . $rest) (make-tl 'elseif-list $1))
   ;; 28. elseif-list-1 => elseif-list-1 elseif
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 29. elseif => "elseif" expr "then" block
   (lambda ($4 $3 $2 $1 . $rest) `(elseif ,$2 ,$4))
   ;; 30. name-list => name
   (lambda ($1 . $rest) $1)
   ;; 31. name-list => name-list "," name
   (lambda ($3 $2 $1 . $rest) $1)
   ;; 32. func-name => name
   (lambda ($1 . $rest) $1)
   ;; 33. func-name => name ":" key
   (lambda ($3 $2 $1 . $rest) $1)
   ;; 34. func-name => name keys
   (lambda ($2 $1 . $rest) $1)
   ;; 35. func-name => name keys ":" key
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; 36. keys => "." key
   (lambda ($2 $1 . $rest) $1)
   ;; 37. keys => keys "." key
   (lambda ($3 $2 $1 . $rest) $1)
   ;; 38. var-list => exprs
   (lambda ($1 . $rest) $1)
   ;; 39. params => "..."
   (lambda ($1 . $rest) `(params (ellipsis)))
   ;; 40. params => name-list
   (lambda ($1 . $rest) `(params ,@(cdr $1)))
   ;; 41. params => name-list "..."
   (lambda ($2 $1 . $rest)
     `(params ,@(cdr $1) (ellipsis)))
   ;; 42. exprs => expr
   (lambda ($1 . $rest) $1)
   ;; 43. exprs => exprs "," expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; 44. expr => or-expr
   (lambda ($1 . $rest) $1)
   ;; 45. or-expr => and-expr
   (lambda ($1 . $rest) $1)
   ;; 46. or-expr => or-expr "or" and-expr
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; 47. and-expr => equ-expr
   (lambda ($1 . $rest) $1)
   ;; 48. and-expr => and-expr "and" equ-expr
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; 49. equ-expr => rel-expr
   (lambda ($1 . $rest) $1)
   ;; 50. equ-expr => equ-expr "==" rel-expr
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; 51. equ-expr => equ-expr "~=" rel-expr
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; 52. rel-expr => strcat-expr
   (lambda ($1 . $rest) $1)
   ;; 53. rel-expr => rel-expr "<" strcat-expr
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; 54. rel-expr => rel-expr "<=" strcat-expr
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; 55. rel-expr => rel-expr ">" strcat-expr
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; 56. rel-expr => rel-expr ">=" strcat-expr
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; 57. strcat-expr => add-expr
   (lambda ($1 . $rest) $1)
   ;; 58. strcat-expr => strcat-expr ".." add-expr
   (lambda ($3 $2 $1 . $rest) `(strcat ,$1 ,$3))
   ;; 59. add-expr => mul-expr
   (lambda ($1 . $rest) $1)
   ;; 60. add-expr => add-expr "+" mul-expr
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; 61. add-expr => add-expr "-" mul-expr
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; 62. mul-expr => unary-expr
   (lambda ($1 . $rest) $1)
   ;; 63. mul-expr => mul-expr "*" unary-expr
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 64. mul-expr => mul-expr "/" unary-expr
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 65. mul-expr => mul-expr "%" unary-expr
   (lambda ($3 $2 $1 . $rest) `(mod ,$1 ,$3))
   ;; 66. unary-expr => exp-expr
   (lambda ($1 . $rest) $1)
   ;; 67. unary-expr => "-" unary-expr
   (lambda ($2 $1 . $rest) `(neg ,$1))
   ;; 68. unary-expr => "#" unary-expr
   (lambda ($2 $1 . $rest) `(len ,$1))
   ;; 69. unary-expr => "not" unary-expr
   (lambda ($2 $1 . $rest) `(not ,$1))
   ;; 70. exp-expr => prim-expr
   (lambda ($1 . $rest) $1)
   ;; 71. exp-expr => exp-expr "^" prim-expr
   (lambda ($3 $2 $1 . $rest) `(exp ,$1 ,$3))
   ;; 72. prim-expr => "nil"
   (lambda ($1 . $rest) '(nil))
   ;; 73. prim-expr => literal
   (lambda ($1 . $rest) $1)
   ;; 74. prim-expr => table-cons
   (lambda ($1 . $rest) $1)
   ;; 75. prim-expr => "function" "(" ")" block "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(fctn-expr (params) ,$4))
   ;; 76. prim-expr => "function" "(" params ")" block "end"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-expr ,$3 ,$5))
   ;; 77. prim-expr => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 78. primary => prim-expr
   (lambda ($1 . $rest) $1)
   ;; 79. var => name
   (lambda ($1 . $rest) $1)
   ;; 80. var => primary index
   (lambda ($2 $1 . $rest) `(indexed ,$1 ,$2))
   ;; 81. var => var index
   (lambda ($2 $1 . $rest) `(indexed ,$1 ,$2))
   ;; 82. var => call index
   (lambda ($2 $1 . $rest) `(call ,$1))
   ;; 83. index => "[" expr "]"
   (lambda ($3 $2 $1 . $rest) `(expr-index ,$2))
   ;; 84. index => "." key
   (lambda ($2 $1 . $rest) `(key-index ,$2))
   ;; 85. call => primary ":" key args
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; 86. call => primary args
   (lambda ($2 $1 . $rest) $1)
   ;; 87. call => var ":" key args
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; 88. call => var args
   (lambda ($2 $1 . $rest) $1)
   ;; 89. call => call ":" key args
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; 90. call => call args
   (lambda ($2 $1 . $rest) $1)
   ;; 91. args => "(" exprs ")"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; 92. args => "(" ")"
   (lambda ($2 $1 . $rest) $1)
   ;; 93. args => table-cons
   (lambda ($1 . $rest) $1)
   ;; 94. args => literal
   (lambda ($1 . $rest) $1)
   ;; 95. table-cons => "{" fields "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; 96. table-cons => "{" "}"
   (lambda ($2 $1 . $rest) $1)
   ;; 97. fields => expr-fields ";" mapping-fields
   (lambda ($3 $2 $1 . $rest) $1)
   ;; 98. fields => expr-fields ";"
   (lambda ($2 $1 . $rest) $1)
   ;; 99. fields => expr-fields
   (lambda ($1 . $rest) $1)
   ;; 100. fields => mapping-fields ";" expr-fields
   (lambda ($3 $2 $1 . $rest) $1)
   ;; 101. fields => mapping-fields ";"
   (lambda ($2 $1 . $rest) $1)
   ;; 102. fields => mapping-fields
   (lambda ($1 . $rest) $1)
   ;; 103. fields => ";" expr-fields
   (lambda ($2 $1 . $rest) $1)
   ;; 104. fields => ";" mapping-fields
   (lambda ($2 $1 . $rest) $1)
   ;; 105. expr-fields => exprs ","
   (lambda ($2 $1 . $rest) $1)
   ;; 106. expr-fields => exprs
   (lambda ($1 . $rest) $1)
   ;; 107. mapping-fields => mapping-field
   (lambda ($1 . $rest) $1)
   ;; 108. mapping-fields => mapping-fields "," mapping-field
   (lambda ($3 $2 $1 . $rest) $1)
   ;; 109. mapping-fields => mapping-fields ","
   (lambda ($2 $1 . $rest) $1)
   ;; 110. mapping-field => "[" expr "]" "=" expr
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; 111. mapping-field => key "=" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; 112. literal => number
   (lambda ($1 . $rest) $1)
   ;; 113. literal => string
   (lambda ($1 . $rest) $1)
   ;; 114. key => name
   (lambda ($1 . $rest) $1)
   ;; 115. key => string
   (lambda ($1 . $rest) $1)
   ;; 116. key => number
   (lambda ($1 . $rest) $1)
   ;; 117. opt-semi => 
   (lambda $rest (list))
   ;; 118. opt-semi => ";"
   (lambda ($1 . $rest) $1)
   ;; 119. name => '$ident
   (lambda ($1 . $rest) `(name ,$1))
   ;; 120. number => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; 121. number => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; 122. string => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ))

;; --- last line ---
