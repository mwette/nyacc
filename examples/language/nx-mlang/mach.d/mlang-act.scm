;; mlang-act.scm

;; Copyright 2015-2025 Matthew Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING included with the this distribution.

(define mlang-act-v
  (vector
   ;; 0. $start => translation-unit
   (lambda ($1 . $rest) $1)
   ;; 1. translation-unit => triv-stmt-list nontrivial-statement mlang-item-list
   (lambda ($3 $2 $1 . $rest)
     `(script-file ,@(sx-tail $1) ,$2 ,@(sx-tail $3)))
   ;; 2. translation-unit => nontrivial-statement mlang-item-list
   (lambda ($2 $1 . $rest)
     `(script-file ,$1 ,@(sx-tail $2)))
   ;; 3. translation-unit => triv-stmt-list function-defn mlang-item-list
   (lambda ($3 $2 $1 . $rest)
     `(function-file ,@(sx-tail $1) ,$2 ,@(sx-tail $3)))
   ;; 4. translation-unit => function-defn mlang-item-list
   (lambda ($2 $1 . $rest)
     `(function-file ,$1 ,@(sx-tail $2)))
   ;; 5. translation-unit => triv-stmt-list class-defn mlang-item-list
   (lambda ($3 $2 $1 . $rest)
     `(classdef-file ,@(sx-tail $1) ,$2 ,@(sx-tail $3)))
   ;; 6. translation-unit => class-defn mlang-item-list
   (lambda ($2 $1 . $rest)
     `(classdef-file ,$1 ,@(sx-tail $2)))
   ;; 7. mlang-item-list => mlang-item-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 8. mlang-item-list-1 => 
   (lambda $rest (make-tl 'mitem-list))
   ;; 9. mlang-item-list-1 => mlang-item-list-1 mlang-item
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 10. mlang-item => function-defn
   (lambda ($1 . $rest) $1)
   ;; 11. mlang-item => statement
   (lambda ($1 . $rest) $1)
   ;; 12. class-defn => "classdef" "(" attr-list ")" ident "<" supers term clas...
   (lambda ($10 $9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(class-defn ,$5 ,$7 ,$3 ,@(cdr $9)))
   ;; 13. class-defn => "classdef" "(" attr-list ")" ident term class-parts "end"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(class-defn ,$5 ,$3 ,@(cdr $7)))
   ;; 14. class-defn => "classdef" ident "<" supers term class-parts "end"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(class-defn ,$2 ,$4 ,@(cdr $6)))
   ;; 15. class-defn => "classdef" ident term class-parts "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(class-defn ,$2 ,@(cdr $4)))
   ;; 16. supers => supers-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 17. supers-1 => ident
   (lambda ($1 . $rest) (make-tl 'supers $1))
   ;; 18. supers-1 => supers-1 "&" ident
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 19. class-parts => class-parts-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 20. class-parts-1 => 
   (lambda $rest (make-tl 'seq))
   ;; 21. class-parts-1 => class-parts-1 "properties" "(" attr-list ")" prop-li...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(properties ,$4 ,@(cdr $6))))
   ;; 22. class-parts-1 => class-parts-1 "properties" prop-list "end"
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 `(properties ,@(cdr $3))))
   ;; 23. class-parts-1 => class-parts-1 "methods" "(" attr-list ")" method-lis...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(methods ,$4 ,@(cdr $6))))
   ;; 24. class-parts-1 => class-parts-1 "methods" method-list "end"
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 `(methods ,@(cdr $3))))
   ;; 25. attr-list => attr-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 26. attr-list-1 => attr
   (lambda ($1 . $rest) (make-tl 'attr-list $1))
   ;; 27. attr-list-1 => attr-list-1 "," attr
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 28. attr => ident
   (lambda ($1 . $rest) `(attr ,$1))
   ;; 29. attr => ident "=" expr
   (lambda ($3 $2 $1 . $rest) `(attr ,$1 ,$3))
   ;; 30. prop-list => prop-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 31. prop-list-1 => 
   (lambda $rest (make-tl 'properties))
   ;; 32. prop-list-1 => prop-list-1 prop
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 33. prop => ident term
   (lambda ($2 $1 . $rest) `(property ,$1))
   ;; 34. method-list => method-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 35. method-list-1 => method-item
   (lambda ($1 . $rest) (make-tl 'methods $1))
   ;; 36. method-list-1 => method-list-1 method-item
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 37. method-item => function-defn
   (lambda ($1 . $rest) $1)
   ;; 38. method-item => ident
   (lambda ($1 . $rest) $1)
   ;; 39. method-item => function-sig
   (lambda ($1 . $rest) `(function-sig ,@(cdr $1)))
   ;; 40. function-defn => function-decl non-comment-statement stmt-list the-end
   (lambda ($4 $3 $2 $1 . $rest)
     `(fctn-defn
        ,$1
        ,(if $2 `(stmt-list ,$2 ,@(cdr $3)) $3)))
   ;; 41. function-defn => function-decl non-comment-statement the-end
   (lambda ($3 $2 $1 . $rest)
     `(fctn-defn
        ,$1
        ,(if $2 `(stmt-list ,$2) '(stmt-list))))
   ;; 42. function-defn => function-decl the-end
   (lambda ($2 $1 . $rest)
     `(fctn-defn ,$1 (stmt-list)))
   ;; 43. the-end => "end" term
   (lambda ($2 $1 . $rest) $1)
   ;; 44. function-decl => "function" function-sig term lone-comment-list
   (lambda ($4 $3 $2 $1 . $rest)
     (append $2 (list $4)))
   ;; 45. function-decl => "function" function-sig term
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 46. function-sig => "[" ident-list "]" "=" ident "(" ident-list ")"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$5 ,$7 ,$2))
   ;; 47. function-sig => "[" ident-list "]" "=" ident "(" ")"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$5 (ident-list) ,$2))
   ;; 48. function-sig => ident "=" ident "(" ident-list ")"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$3 ,$5 (ident-list ,$1)))
   ;; 49. function-sig => ident "=" ident "(" ")"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$3 (ident-list) (ident-list ,$1)))
   ;; 50. function-sig => ident "(" ident-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(fctn-decl ,$1 ,$3 (ident-list)))
   ;; 51. function-sig => ident "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(fctn-decl ,$1 (ident-list) (ident-list)))
   ;; 52. ident-list => ident-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 53. ident-list-1 => ident
   (lambda ($1 . $rest) (make-tl 'ident-list $1))
   ;; 54. ident-list-1 => ident-list-1 "," ident
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 55. stmt-list => stmt-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 56. stmt-list-1 => statement
   (lambda ($1 . $rest)
     (if $1
       (make-tl 'stmt-list $1)
       (make-tl 'stmt-list)))
   ;; 57. stmt-list-1 => stmt-list-1 statement
   (lambda ($2 $1 . $rest)
     (if $2 (tl-append $1 $2) $1))
   ;; 58. triv-stmt-list => triv-stmt-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 59. triv-stmt-list-1 => trivial-statement
   (lambda ($1 . $rest)
     (make-tl 'triv-stmt-list $1))
   ;; 60. triv-stmt-list-1 => triv-stmt-list-1 trivial-statement
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 61. statement => trivial-statement
   (lambda ($1 . $rest) $1)
   ;; 62. statement => nontrivial-statement
   (lambda ($1 . $rest) $1)
   ;; 63. non-comment-statement => term
   (lambda ($1 . $rest) '(empty-stmt))
   ;; 64. non-comment-statement => nontrivial-statement
   (lambda ($1 . $rest) $1)
   ;; 65. trivial-statement => lone-comment "\n"
   (lambda ($2 $1 . $rest) $1)
   ;; 66. trivial-statement => term
   (lambda ($1 . $rest) '(empty-stmt))
   ;; 67. nontrivial-statement => nontrivial-statement-1 term
   (lambda ($2 $1 . $rest) $1)
   ;; 68. nontrivial-statement-1 => expr
   (lambda ($1 . $rest) `(expr-stmt ,$1))
   ;; 69. nontrivial-statement-1 => expr "=" expr
   (lambda ($3 $2 $1 . $rest) `(assn ,$1 ,$3))
   ;; 70. nontrivial-statement-1 => "for" ident "=" expr term stmt-list "end"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for ,$2 ,$4 ,$6))
   ;; 71. nontrivial-statement-1 => "while" expr term stmt-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(while ,$2 ,$4))
   ;; 72. nontrivial-statement-1 => "if" expr term stmt-list elseif-list "else"...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,@(cdr $5) (else ,$8)))
   ;; 73. nontrivial-statement-1 => "if" expr term stmt-list elseif-list "end"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,@(cdr $5)))
   ;; 74. nontrivial-statement-1 => "if" expr term stmt-list "else" term stmt-l...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 (else ,$7)))
   ;; 75. nontrivial-statement-1 => "if" expr term stmt-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4))
   ;; 76. nontrivial-statement-1 => "switch" expr term case-list "otherwise" te...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(switch ,$2 ,@(cdr $4) (otherwise ,$7)))
   ;; 77. nontrivial-statement-1 => "switch" expr term case-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(switch ,$2 ,@(cdr $4)))
   ;; 78. nontrivial-statement-1 => "return"
   (lambda ($1 . $rest) '(return))
   ;; 79. nontrivial-statement-1 => command arg-list
   (lambda ($2 $1 . $rest) (append $1 (cdr $2)))
   ;; 80. nontrivial-statement-1 => command "(" arg-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     (append $1 (cdr $3)))
   ;; 81. command => command-name
   (lambda ($1 . $rest) `(command ,$1))
   ;; 82. command-name => "clc"
   (lambda ($1 . $rest) $1)
   ;; 83. command-name => "doc"
   (lambda ($1 . $rest) $1)
   ;; 84. command-name => "drawnow"
   (lambda ($1 . $rest) $1)
   ;; 85. command-name => "format"
   (lambda ($1 . $rest) $1)
   ;; 86. command-name => "global"
   (lambda ($1 . $rest) $1)
   ;; 87. command-name => "grid"
   (lambda ($1 . $rest) $1)
   ;; 88. command-name => "help"
   (lambda ($1 . $rest) $1)
   ;; 89. command-name => "hold"
   (lambda ($1 . $rest) $1)
   ;; 90. command-name => "load"
   (lambda ($1 . $rest) $1)
   ;; 91. command-name => "pause"
   (lambda ($1 . $rest) $1)
   ;; 92. command-name => "rotate3d"
   (lambda ($1 . $rest) $1)
   ;; 93. command-name => "save"
   (lambda ($1 . $rest) $1)
   ;; 94. command-name => "uiimport"
   (lambda ($1 . $rest) $1)
   ;; 95. command-name => "ver"
   (lambda ($1 . $rest) $1)
   ;; 96. arg-list => arg-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 97. arg-list-1 => ident
   (lambda ($1 . $rest)
     (make-tl 'arg-list (cons 'arg (cdr $1))))
   ;; 98. arg-list-1 => arg-list-1 ident
   (lambda ($2 $1 . $rest)
     (tl-append $1 (cons 'arg $2)))
   ;; 99. elseif-list => elseif-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 100. elseif-list-1 => "elseif" expr term stmt-list
   (lambda ($4 $3 $2 $1 . $rest)
     (make-tl 'elseif-list `(elseif ,$2 ,$4)))
   ;; 101. elseif-list-1 => elseif-list-1 "elseif" expr term stmt-list
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(elseif ,$3 ,$5)))
   ;; 102. case-list => 
   (lambda $rest (make-tl 'case-list))
   ;; 103. case-list => case-list "case" case-expr term stmt-list
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(case ,$3 ,$5)))
   ;; 104. case-expr => fixed
   (lambda ($1 . $rest) $1)
   ;; 105. case-expr => string
   (lambda ($1 . $rest) $1)
   ;; 106. case-expr => "{" fixed-list "}"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; 107. case-expr => "{" string-list "}"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; 108. fixed-list => fixed
   (lambda ($1 . $rest) (make-tl 'fixed-list $1))
   ;; 109. fixed-list => fixed-list fixed
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 110. string-list => string
   (lambda ($1 . $rest) (make-tl 'string-list $1))
   ;; 111. string-list => string-list string
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 112. expr-list => expr-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 113. expr-list-1 => expr
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; 114. expr-list-1 => expr-list-1 "," expr
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 115. expr => or-expr
   (lambda ($1 . $rest) $1)
   ;; 116. expr => ":"
   (lambda ($1 . $rest) `(colon-expr))
   ;; 117. expr => or-expr ":" or-expr
   (lambda ($3 $2 $1 . $rest) `(colon-expr ,$1 ,$3))
   ;; 118. expr => or-expr ":" or-expr ":" or-expr
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 ,$5))
   ;; 119. expr => or-expr ":" "end"
   (lambda ($3 $2 $1 . $rest)
     `(colon-expr ,$1 (end)))
   ;; 120. expr => or-expr ":" or-expr ":" "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 (end)))
   ;; 121. expr-nosp => or-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 122. expr-nosp => ":"
   (lambda ($1 . $rest) `(colon-expr))
   ;; 123. expr-nosp => or-expr-nosp ":" or-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(colon-expr ,$1 ,$3))
   ;; 124. expr-nosp => or-expr-nosp ":" or-expr-nosp ":" or-expr-nosp
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 ,$5))
   ;; 125. expr-nosp => or-expr-nosp ":" "end"
   (lambda ($3 $2 $1 . $rest)
     `(colon-expr ,$1 (end)))
   ;; 126. expr-nosp => or-expr-nosp ":" or-expr-nosp ":" "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 (end)))
   ;; 127. or-expr => and-expr
   (lambda ($1 . $rest) $1)
   ;; 128. or-expr => or-expr "|" and-expr
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; 129. or-expr => or-expr "||" and-expr
   (lambda ($3 $2 $1 . $rest) `(ss-or ,$1 ,$3))
   ;; 130. or-expr-nosp => and-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 131. or-expr-nosp => or-expr-nosp "|" and-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; 132. or-expr-nosp => or-expr-nosp "||" and-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ss-or ,$1 ,$3))
   ;; 133. and-expr => equality-expr
   (lambda ($1 . $rest) $1)
   ;; 134. and-expr => and-expr "&" equality-expr
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; 135. and-expr => and-expr "&&" equality-expr
   (lambda ($3 $2 $1 . $rest) `(ss-and ,$1 ,$3))
   ;; 136. and-expr-nosp => equality-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 137. and-expr-nosp => and-expr-nosp "&" equality-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; 138. and-expr-nosp => and-expr-nosp "&&" equality-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ss-and ,$1 ,$3))
   ;; 139. equality-expr => rel-expr
   (lambda ($1 . $rest) $1)
   ;; 140. equality-expr => equality-expr "==" rel-expr
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; 141. equality-expr => equality-expr "~=" rel-expr
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; 142. equality-expr-nosp => rel-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 143. equality-expr-nosp => equality-expr-nosp "==" rel-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; 144. equality-expr-nosp => equality-expr-nosp "~=" rel-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; 145. rel-expr => add-expr
   (lambda ($1 . $rest) $1)
   ;; 146. rel-expr => rel-expr "<" add-expr
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; 147. rel-expr => rel-expr ">" add-expr
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; 148. rel-expr => rel-expr "<=" add-expr
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; 149. rel-expr => rel-expr ">=" add-expr
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; 150. rel-expr-nosp => add-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 151. rel-expr-nosp => rel-expr-nosp "<" add-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; 152. rel-expr-nosp => rel-expr-nosp ">" add-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; 153. rel-expr-nosp => rel-expr-nosp "<=" add-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; 154. rel-expr-nosp => rel-expr-nosp ">=" add-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; 155. add-expr => mul-expr
   (lambda ($1 . $rest) $1)
   ;; 156. add-expr => add-expr "+" mul-expr
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; 157. add-expr => add-expr "-" mul-expr
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; 158. add-expr => add-expr ".+" mul-expr
   (lambda ($3 $2 $1 . $rest) `(dot-add ,$1 ,$3))
   ;; 159. add-expr => add-expr ".-" mul-expr
   (lambda ($3 $2 $1 . $rest) `(dot-sub ,$1 ,$3))
   ;; 160. add-expr-nosp => mul-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 161. add-expr-nosp => add-expr-nosp "+" mul-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; 162. add-expr-nosp => add-expr-nosp "-" mul-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; 163. add-expr-nosp => add-expr-nosp ".+" mul-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-add ,$1 ,$3))
   ;; 164. add-expr-nosp => add-expr-nosp ".-" mul-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-sub ,$1 ,$3))
   ;; 165. mul-expr => unary-expr
   (lambda ($1 . $rest) $1)
   ;; 166. mul-expr => mul-expr "*" unary-expr
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 167. mul-expr => mul-expr "/" unary-expr
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 168. mul-expr => mul-expr "\\" unary-expr
   (lambda ($3 $2 $1 . $rest) `(ldiv ,$1 ,$3))
   ;; 169. mul-expr => mul-expr "^" unary-expr
   (lambda ($3 $2 $1 . $rest) `(pow ,$1 ,$3))
   ;; 170. mul-expr => mul-expr ".*" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-mul ,$1 ,$3))
   ;; 171. mul-expr => mul-expr "./" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-div ,$1 ,$3))
   ;; 172. mul-expr => mul-expr ".\\" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-ldiv ,$1 ,$3))
   ;; 173. mul-expr => mul-expr ".^" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-pow ,$1 ,$3))
   ;; 174. mul-expr-nosp => unary-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 175. mul-expr-nosp => mul-expr-nosp "*" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 176. mul-expr-nosp => mul-expr-nosp "/" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 177. mul-expr-nosp => mul-expr-nosp "\\" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ldiv ,$1 ,$3))
   ;; 178. mul-expr-nosp => mul-expr-nosp "^" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(pow ,$1 ,$3))
   ;; 179. mul-expr-nosp => mul-expr-nosp ".*" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-mul ,$1 ,$3))
   ;; 180. mul-expr-nosp => mul-expr-nosp "./" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-div ,$1 ,$3))
   ;; 181. mul-expr-nosp => mul-expr-nosp ".\\" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-ldiv ,$1 ,$3))
   ;; 182. mul-expr-nosp => mul-expr-nosp ".^" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-pow ,$1 ,$3))
   ;; 183. unary-expr => postfix-expr
   (lambda ($1 . $rest) $1)
   ;; 184. unary-expr => "-" postfix-expr
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; 185. unary-expr => "+" postfix-expr
   (lambda ($2 $1 . $rest) `(pos $2))
   ;; 186. unary-expr => "~" postfix-expr
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; 187. unary-expr => "@" postfix-expr
   (lambda ($2 $1 . $rest) `(handle ,$2))
   ;; 188. unary-expr-nosp => postfix-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 189. unary-expr-nosp => "-" postfix-expr-nosp
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; 190. unary-expr-nosp => "+" postfix-expr-nosp
   (lambda ($2 $1 . $rest) $2)
   ;; 191. unary-expr-nosp => "~" postfix-expr-nosp
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; 192. postfix-expr => primary-expr
   (lambda ($1 . $rest) $1)
   ;; 193. postfix-expr => postfix-expr "'"
   (lambda ($2 $1 . $rest) `(transpose ,$1))
   ;; 194. postfix-expr => postfix-expr ".'"
   (lambda ($2 $1 . $rest) `(conj-transpose ,$1))
   ;; 195. postfix-expr => postfix-expr "(" expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(aref-or-call ,$1 ,$3))
   ;; 196. postfix-expr => postfix-expr "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(aref-or-call ,$1 (expr-list)))
   ;; 197. postfix-expr => postfix-expr "{" expr-list "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(cell-ref ,$1 ,$3))
   ;; 198. postfix-expr => postfix-expr "." ident
   (lambda ($3 $2 $1 . $rest) `(sel ,$3 ,$1))
   ;; 199. postfix-expr => postfix-expr ".(" ident ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(obj-prop ,$3 ,$1))
   ;; 200. postfix-expr-nosp => primary-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 201. postfix-expr-nosp => postfix-expr-nosp "'"
   (lambda ($2 $1 . $rest) `(transpose ,$1))
   ;; 202. postfix-expr-nosp => postfix-expr-nosp ".'"
   (lambda ($2 $1 . $rest) `(conj-transpose ,$1))
   ;; 203. postfix-expr-nosp => postfix-expr-nosp "(" expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(aref-or-call ,$1 ,$3))
   ;; 204. postfix-expr-nosp => postfix-expr-nosp "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(aref-or-call ,$1 (expr-list)))
   ;; 205. postfix-expr-nosp => postfix-expr-nosp "{" expr-list "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(cell-ref ,$1 ,$3))
   ;; 206. postfix-expr-nosp => postfix-expr-nosp "." ident
   (lambda ($3 $2 $1 . $rest) `(sel ,$3 ,$1))
   ;; 207. postfix-expr-nosp => postfix-expr-nosp ".(" ident ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(obj-prop ,$3 ,$1))
   ;; 208. primary-expr => ident
   (lambda ($1 . $rest) $1)
   ;; 209. primary-expr => number
   (lambda ($1 . $rest) $1)
   ;; 210. primary-expr => string
   (lambda ($1 . $rest) $1)
   ;; 211. primary-expr => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 212. primary-expr => "[" "]"
   (lambda ($2 $1 . $rest) '(matrix))
   ;; 213. primary-expr => "[" matrix-row-list "]"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 214. primary-expr => "{" "}"
   (lambda ($2 $1 . $rest) '(cell-array))
   ;; 215. primary-expr => "{" matrix-row-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(cell-array unquote (cdr $2)))
   ;; 216. primary-expr-nosp => ident
   (lambda ($1 . $rest) $1)
   ;; 217. primary-expr-nosp => number
   (lambda ($1 . $rest) $1)
   ;; 218. primary-expr-nosp => string
   (lambda ($1 . $rest) $1)
   ;; 219. primary-expr-nosp => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) `(wrap ,$2))
   ;; 220. primary-expr-nosp => "[" "]"
   (lambda ($2 $1 . $rest) '(matrix))
   ;; 221. primary-expr-nosp => "[" matrix-row-list "]"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 222. primary-expr-nosp => "{" "}"
   (lambda ($2 $1 . $rest) '(cell-array))
   ;; 223. primary-expr-nosp => "{" matrix-row-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(cell-array unquote (cdr $2)))
   ;; 224. matrix-row-list => matrix-row-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 225. matrix-row-list-1 => matrix-row
   (lambda ($1 . $rest) (make-tl 'matrix $1))
   ;; 226. matrix-row-list-1 => matrix-row-list-1 row-term matrix-row
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 227. row-term => ";"
   (lambda ($1 . $rest) $1)
   ;; 228. row-term => "\n"
   (lambda ($1 . $rest) $1)
   ;; 229. matrix-row => matrix-row-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 230. matrix-row-1 => expr-nosp
   (lambda ($1 . $rest) (make-tl 'row $1))
   ;; 231. matrix-row-1 => matrix-row-1 "," expr-nosp
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 232. matrix-row-1 => matrix-row-1 'sp expr-nosp
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 233. term-list => term
   (lambda ($1 . $rest) $1)
   ;; 234. term-list => term-list term
   (lambda ($2 $1 . $rest) $1)
   ;; 235. term => ";" '$code-comm "\n"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; 236. term => ";" "\n"
   (lambda ($2 $1 . $rest) $1)
   ;; 237. term => ";"
   (lambda ($1 . $rest) $1)
   ;; 238. term => ","
   (lambda ($1 . $rest) $1)
   ;; 239. term => "\n"
   (lambda ($1 . $rest) $1)
   ;; 240. lone-comment-list => lone-comment-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 241. lone-comment-list-1 => lone-comment "\n"
   (lambda ($2 $1 . $rest) (make-tl 'comm-list $1))
   ;; 242. lone-comment-list-1 => lone-comment-list-1 lone-comment "\n"
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; 243. ident => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; 244. fixed => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; 245. float => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; 246. number => fixed
   (lambda ($1 . $rest) $1)
   ;; 247. number => float
   (lambda ($1 . $rest) $1)
   ;; 248. string => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ;; 249. lone-comment => '$lone-comm
   (lambda ($1 . $rest) `(comm ,$1))
   ))

;; --- last line ---
