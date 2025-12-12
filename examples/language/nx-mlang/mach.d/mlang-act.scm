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
   ;; 55. q-ident-list => q-ident-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 56. q-ident-list-1 => q-ident
   (lambda ($1 . $rest) (make-tl 'ident-list $1))
   ;; 57. q-ident-list-1 => q-ident-list-1 "," q-ident
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 58. q-ident => q-ident-1
   (lambda ($1 . $rest) `(qident ,@(reverse $1)))
   ;; 59. q-ident-1 => '$ident
   (lambda ($1 . $rest) (list $1))
   ;; 60. q-ident-1 => q-ident-1 "." '$ident
   (lambda ($3 $2 $1 . $rest) (cons $3 $1))
   ;; 61. stmt-list => stmt-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 62. stmt-list-1 => statement
   (lambda ($1 . $rest)
     (if $1
       (make-tl 'stmt-list $1)
       (make-tl 'stmt-list)))
   ;; 63. stmt-list-1 => stmt-list-1 statement
   (lambda ($2 $1 . $rest)
     (if $2 (tl-append $1 $2) $1))
   ;; 64. triv-stmt-list => triv-stmt-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 65. triv-stmt-list-1 => trivial-statement
   (lambda ($1 . $rest)
     (make-tl 'triv-stmt-list $1))
   ;; 66. triv-stmt-list-1 => triv-stmt-list-1 trivial-statement
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 67. statement => trivial-statement
   (lambda ($1 . $rest) $1)
   ;; 68. statement => nontrivial-statement
   (lambda ($1 . $rest) $1)
   ;; 69. non-comment-statement => term
   (lambda ($1 . $rest) '(empty-stmt))
   ;; 70. non-comment-statement => nontrivial-statement
   (lambda ($1 . $rest) $1)
   ;; 71. trivial-statement => lone-comment "\n"
   (lambda ($2 $1 . $rest) $1)
   ;; 72. trivial-statement => term
   (lambda ($1 . $rest) '(empty-stmt))
   ;; 73. nontrivial-statement => nontrivial-statement-1 term
   (lambda ($2 $1 . $rest) $1)
   ;; 74. nontrivial-statement-1 => expr
   (lambda ($1 . $rest) `(expr-stmt ,$1))
   ;; 75. nontrivial-statement-1 => expr "=" expr
   (lambda ($3 $2 $1 . $rest) `(assn ,$1 ,$3))
   ;; 76. nontrivial-statement-1 => "for" ident "=" expr term stmt-list "end"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for ,$2 ,$4 ,$6))
   ;; 77. nontrivial-statement-1 => "while" expr term stmt-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(while ,$2 ,$4))
   ;; 78. nontrivial-statement-1 => "if" expr term stmt-list elseif-list "else"...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,@(cdr $5) (else ,$8)))
   ;; 79. nontrivial-statement-1 => "if" expr term stmt-list elseif-list "end"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,@(cdr $5)))
   ;; 80. nontrivial-statement-1 => "if" expr term stmt-list "else" term stmt-l...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 (else ,$7)))
   ;; 81. nontrivial-statement-1 => "if" expr term stmt-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4))
   ;; 82. nontrivial-statement-1 => "switch" expr term case-list "otherwise" te...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(switch ,$2 ,@(cdr $4) (otherwise ,$7)))
   ;; 83. nontrivial-statement-1 => "switch" expr term case-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(switch ,$2 ,@(cdr $4)))
   ;; 84. nontrivial-statement-1 => "return"
   (lambda ($1 . $rest) '(return))
   ;; 85. nontrivial-statement-1 => command arg-list
   (lambda ($2 $1 . $rest) (append $1 (cdr $2)))
   ;; 86. nontrivial-statement-1 => command "(" arg-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     (append $1 (cdr $3)))
   ;; 87. command => command-name
   (lambda ($1 . $rest) `(command ,$1))
   ;; 88. command-name => "clc"
   (lambda ($1 . $rest) $1)
   ;; 89. command-name => "clear"
   (lambda ($1 . $rest) $1)
   ;; 90. command-name => "doc"
   (lambda ($1 . $rest) $1)
   ;; 91. command-name => "drawnow"
   (lambda ($1 . $rest) $1)
   ;; 92. command-name => "format"
   (lambda ($1 . $rest) $1)
   ;; 93. command-name => "global"
   (lambda ($1 . $rest) $1)
   ;; 94. command-name => "grid"
   (lambda ($1 . $rest) $1)
   ;; 95. command-name => "help"
   (lambda ($1 . $rest) $1)
   ;; 96. command-name => "hold"
   (lambda ($1 . $rest) $1)
   ;; 97. command-name => "load"
   (lambda ($1 . $rest) $1)
   ;; 98. command-name => "pause"
   (lambda ($1 . $rest) $1)
   ;; 99. command-name => "rotate3d"
   (lambda ($1 . $rest) $1)
   ;; 100. command-name => "save"
   (lambda ($1 . $rest) $1)
   ;; 101. command-name => "uiimport"
   (lambda ($1 . $rest) $1)
   ;; 102. command-name => "ver"
   (lambda ($1 . $rest) $1)
   ;; 103. arg-list => arg-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 104. arg-list-1 => ident
   (lambda ($1 . $rest)
     (make-tl 'arg-list (cons 'arg (cdr $1))))
   ;; 105. arg-list-1 => arg-list-1 ident
   (lambda ($2 $1 . $rest)
     (tl-append $1 (cons 'arg $2)))
   ;; 106. elseif-list => elseif-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 107. elseif-list-1 => "elseif" expr term stmt-list
   (lambda ($4 $3 $2 $1 . $rest)
     (make-tl 'elseif-list `(elseif ,$2 ,$4)))
   ;; 108. elseif-list-1 => elseif-list-1 "elseif" expr term stmt-list
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(elseif ,$3 ,$5)))
   ;; 109. case-list => 
   (lambda $rest (make-tl 'case-list))
   ;; 110. case-list => case-list "case" case-expr term stmt-list
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(case ,$3 ,$5)))
   ;; 111. case-expr => fixed
   (lambda ($1 . $rest) $1)
   ;; 112. case-expr => string
   (lambda ($1 . $rest) $1)
   ;; 113. case-expr => "{" fixed-list "}"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; 114. case-expr => "{" string-list "}"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; 115. fixed-list => fixed
   (lambda ($1 . $rest) (make-tl 'fixed-list $1))
   ;; 116. fixed-list => fixed-list fixed
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 117. string-list => string
   (lambda ($1 . $rest) (make-tl 'string-list $1))
   ;; 118. string-list => string-list string
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 119. expr-list => expr-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 120. expr-list-1 => expr
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; 121. expr-list-1 => expr-list-1 "," expr
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 122. expr => or-expr
   (lambda ($1 . $rest) $1)
   ;; 123. expr => ":"
   (lambda ($1 . $rest) `(colon-expr))
   ;; 124. expr => or-expr ":" or-expr
   (lambda ($3 $2 $1 . $rest) `(colon-expr ,$1 ,$3))
   ;; 125. expr => or-expr ":" or-expr ":" or-expr
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 ,$5))
   ;; 126. expr => or-expr ":" "end"
   (lambda ($3 $2 $1 . $rest)
     `(colon-expr ,$1 (end)))
   ;; 127. expr => or-expr ":" or-expr ":" "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 (end)))
   ;; 128. expr-nosp => or-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 129. expr-nosp => ":"
   (lambda ($1 . $rest) `(colon-expr))
   ;; 130. expr-nosp => or-expr-nosp ":" or-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(colon-expr ,$1 ,$3))
   ;; 131. expr-nosp => or-expr-nosp ":" or-expr-nosp ":" or-expr-nosp
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 ,$5))
   ;; 132. expr-nosp => or-expr-nosp ":" "end"
   (lambda ($3 $2 $1 . $rest)
     `(colon-expr ,$1 (end)))
   ;; 133. expr-nosp => or-expr-nosp ":" or-expr-nosp ":" "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 (end)))
   ;; 134. or-expr => and-expr
   (lambda ($1 . $rest) $1)
   ;; 135. or-expr => or-expr "|" and-expr
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; 136. or-expr => or-expr "||" and-expr
   (lambda ($3 $2 $1 . $rest) `(ss-or ,$1 ,$3))
   ;; 137. or-expr-nosp => and-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 138. or-expr-nosp => or-expr-nosp "|" and-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; 139. or-expr-nosp => or-expr-nosp "||" and-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ss-or ,$1 ,$3))
   ;; 140. and-expr => equality-expr
   (lambda ($1 . $rest) $1)
   ;; 141. and-expr => and-expr "&" equality-expr
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; 142. and-expr => and-expr "&&" equality-expr
   (lambda ($3 $2 $1 . $rest) `(ss-and ,$1 ,$3))
   ;; 143. and-expr-nosp => equality-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 144. and-expr-nosp => and-expr-nosp "&" equality-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; 145. and-expr-nosp => and-expr-nosp "&&" equality-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ss-and ,$1 ,$3))
   ;; 146. equality-expr => rel-expr
   (lambda ($1 . $rest) $1)
   ;; 147. equality-expr => equality-expr "==" rel-expr
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; 148. equality-expr => equality-expr "~=" rel-expr
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; 149. equality-expr-nosp => rel-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 150. equality-expr-nosp => equality-expr-nosp "==" rel-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; 151. equality-expr-nosp => equality-expr-nosp "~=" rel-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; 152. rel-expr => add-expr
   (lambda ($1 . $rest) $1)
   ;; 153. rel-expr => rel-expr "<" add-expr
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; 154. rel-expr => rel-expr ">" add-expr
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; 155. rel-expr => rel-expr "<=" add-expr
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; 156. rel-expr => rel-expr ">=" add-expr
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; 157. rel-expr-nosp => add-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 158. rel-expr-nosp => rel-expr-nosp "<" add-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; 159. rel-expr-nosp => rel-expr-nosp ">" add-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; 160. rel-expr-nosp => rel-expr-nosp "<=" add-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; 161. rel-expr-nosp => rel-expr-nosp ">=" add-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; 162. add-expr => mul-expr
   (lambda ($1 . $rest) $1)
   ;; 163. add-expr => add-expr "+" mul-expr
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; 164. add-expr => add-expr "-" mul-expr
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; 165. add-expr => add-expr ".+" mul-expr
   (lambda ($3 $2 $1 . $rest) `(dot-add ,$1 ,$3))
   ;; 166. add-expr => add-expr ".-" mul-expr
   (lambda ($3 $2 $1 . $rest) `(dot-sub ,$1 ,$3))
   ;; 167. add-expr-nosp => mul-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 168. add-expr-nosp => add-expr-nosp "+" mul-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; 169. add-expr-nosp => add-expr-nosp "-" mul-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; 170. add-expr-nosp => add-expr-nosp ".+" mul-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-add ,$1 ,$3))
   ;; 171. add-expr-nosp => add-expr-nosp ".-" mul-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-sub ,$1 ,$3))
   ;; 172. mul-expr => handle-expr
   (lambda ($1 . $rest) $1)
   ;; 173. mul-expr => mul-expr "*" handle-expr
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 174. mul-expr => mul-expr "/" handle-expr
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 175. mul-expr => mul-expr "\\" handle-expr
   (lambda ($3 $2 $1 . $rest) `(ldiv ,$1 ,$3))
   ;; 176. mul-expr => mul-expr "^" handle-expr
   (lambda ($3 $2 $1 . $rest) `(pow ,$1 ,$3))
   ;; 177. mul-expr => mul-expr ".*" handle-expr
   (lambda ($3 $2 $1 . $rest) `(dot-mul ,$1 ,$3))
   ;; 178. mul-expr => mul-expr "./" handle-expr
   (lambda ($3 $2 $1 . $rest) `(dot-div ,$1 ,$3))
   ;; 179. mul-expr => mul-expr ".\\" handle-expr
   (lambda ($3 $2 $1 . $rest) `(dot-ldiv ,$1 ,$3))
   ;; 180. mul-expr => mul-expr ".^" handle-expr
   (lambda ($3 $2 $1 . $rest) `(dot-pow ,$1 ,$3))
   ;; 181. mul-expr-nosp => handle-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 182. mul-expr-nosp => mul-expr-nosp "*" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 183. mul-expr-nosp => mul-expr-nosp "/" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 184. mul-expr-nosp => mul-expr-nosp "\\" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ldiv ,$1 ,$3))
   ;; 185. mul-expr-nosp => mul-expr-nosp "^" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(pow ,$1 ,$3))
   ;; 186. mul-expr-nosp => mul-expr-nosp ".*" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-mul ,$1 ,$3))
   ;; 187. mul-expr-nosp => mul-expr-nosp "./" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-div ,$1 ,$3))
   ;; 188. mul-expr-nosp => mul-expr-nosp ".\\" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-ldiv ,$1 ,$3))
   ;; 189. mul-expr-nosp => mul-expr-nosp ".^" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-pow ,$1 ,$3))
   ;; 190. handle-expr => unary-expr
   (lambda ($1 . $rest) $1)
   ;; 191. handle-expr => "@" q-ident
   (lambda ($2 $1 . $rest) `(handle ,$2))
   ;; 192. handle-expr => "@" "(" q-ident-list ")" ident "(" q-ident-list ")"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(handle ,$5 ,$7 ,$3))
   ;; 193. handle-expr-nosp => unary-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 194. handle-expr-nosp => "@" q-ident
   (lambda ($2 $1 . $rest) `(handle ,$2))
   ;; 195. handle-expr-nosp => "@" "(" q-ident-list ")" ident "(" q-ident-list ")"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(handle ,$5 ,$7 ,$3))
   ;; 196. unary-expr => postfix-expr
   (lambda ($1 . $rest) $1)
   ;; 197. unary-expr => "-" postfix-expr
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; 198. unary-expr => "+" postfix-expr
   (lambda ($2 $1 . $rest) `(pos $2))
   ;; 199. unary-expr => "~" postfix-expr
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; 200. unary-expr => "~"
   (lambda ($1 . $rest) '(ignore))
   ;; 201. unary-expr-nosp => postfix-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 202. unary-expr-nosp => "-" postfix-expr-nosp
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; 203. unary-expr-nosp => "+" postfix-expr-nosp
   (lambda ($2 $1 . $rest) $2)
   ;; 204. unary-expr-nosp => "~" postfix-expr-nosp
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; 205. unary-expr-nosp => "~"
   (lambda ($1 . $rest) '(ignore))
   ;; 206. postfix-expr => primary-expr
   (lambda ($1 . $rest) $1)
   ;; 207. postfix-expr => postfix-expr "'"
   (lambda ($2 $1 . $rest) `(transpose ,$1))
   ;; 208. postfix-expr => postfix-expr ".'"
   (lambda ($2 $1 . $rest) `(conj-transpose ,$1))
   ;; 209. postfix-expr => postfix-expr "(" expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(aref-or-call ,$1 ,$3))
   ;; 210. postfix-expr => postfix-expr "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(aref-or-call ,$1 (expr-list)))
   ;; 211. postfix-expr => postfix-expr "{" expr-list "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(cell-ref ,$1 ,$3))
   ;; 212. postfix-expr => postfix-expr "." ident
   (lambda ($3 $2 $1 . $rest) `(sel ,$3 ,$1))
   ;; 213. postfix-expr => postfix-expr ".(" ident ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(obj-prop ,$3 ,$1))
   ;; 214. postfix-expr-nosp => primary-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 215. postfix-expr-nosp => postfix-expr-nosp "'"
   (lambda ($2 $1 . $rest) `(transpose ,$1))
   ;; 216. postfix-expr-nosp => postfix-expr-nosp ".'"
   (lambda ($2 $1 . $rest) `(conj-transpose ,$1))
   ;; 217. postfix-expr-nosp => postfix-expr-nosp "(" expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(aref-or-call ,$1 ,$3))
   ;; 218. postfix-expr-nosp => postfix-expr-nosp "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(aref-or-call ,$1 (expr-list)))
   ;; 219. postfix-expr-nosp => postfix-expr-nosp "{" expr-list "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(cell-ref ,$1 ,$3))
   ;; 220. postfix-expr-nosp => postfix-expr-nosp "." ident
   (lambda ($3 $2 $1 . $rest) `(sel ,$3 ,$1))
   ;; 221. postfix-expr-nosp => postfix-expr-nosp ".(" ident ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(obj-prop ,$3 ,$1))
   ;; 222. primary-expr => ident
   (lambda ($1 . $rest) $1)
   ;; 223. primary-expr => number
   (lambda ($1 . $rest) $1)
   ;; 224. primary-expr => string
   (lambda ($1 . $rest) $1)
   ;; 225. primary-expr => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 226. primary-expr => "[" "]"
   (lambda ($2 $1 . $rest) '(matrix))
   ;; 227. primary-expr => "[" matrix-row-list "]"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 228. primary-expr => "{" "}"
   (lambda ($2 $1 . $rest) '(cell-array))
   ;; 229. primary-expr => "{" matrix-row-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(cell-array unquote (cdr $2)))
   ;; 230. primary-expr-nosp => ident
   (lambda ($1 . $rest) $1)
   ;; 231. primary-expr-nosp => number
   (lambda ($1 . $rest) $1)
   ;; 232. primary-expr-nosp => string
   (lambda ($1 . $rest) $1)
   ;; 233. primary-expr-nosp => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) `(wrap ,$2))
   ;; 234. primary-expr-nosp => "[" "]"
   (lambda ($2 $1 . $rest) '(matrix))
   ;; 235. primary-expr-nosp => "[" matrix-row-list "]"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 236. primary-expr-nosp => "{" "}"
   (lambda ($2 $1 . $rest) '(cell-array))
   ;; 237. primary-expr-nosp => "{" matrix-row-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(cell-array unquote (cdr $2)))
   ;; 238. matrix-row-list => matrix-row-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 239. matrix-row-list-1 => matrix-row
   (lambda ($1 . $rest) (make-tl 'matrix $1))
   ;; 240. matrix-row-list-1 => matrix-row-list-1 row-term matrix-row
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 241. row-term => ";"
   (lambda ($1 . $rest) $1)
   ;; 242. row-term => "\n"
   (lambda ($1 . $rest) $1)
   ;; 243. matrix-row => matrix-row-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 244. matrix-row-1 => expr-nosp
   (lambda ($1 . $rest) (make-tl 'row $1))
   ;; 245. matrix-row-1 => matrix-row-1 "," expr-nosp
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 246. matrix-row-1 => matrix-row-1 'sp expr-nosp
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 247. term-list => term
   (lambda ($1 . $rest) $1)
   ;; 248. term-list => term-list term
   (lambda ($2 $1 . $rest) $1)
   ;; 249. term => ";" '$code-comm "\n"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; 250. term => ";" "\n"
   (lambda ($2 $1 . $rest) $1)
   ;; 251. term => ";"
   (lambda ($1 . $rest) $1)
   ;; 252. term => ","
   (lambda ($1 . $rest) $1)
   ;; 253. term => "\n"
   (lambda ($1 . $rest) $1)
   ;; 254. lone-comment-list => lone-comment-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 255. lone-comment-list-1 => lone-comment "\n"
   (lambda ($2 $1 . $rest) (make-tl 'comm-list $1))
   ;; 256. lone-comment-list-1 => lone-comment-list-1 lone-comment "\n"
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; 257. ident => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; 258. fixed => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; 259. float => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; 260. number => fixed
   (lambda ($1 . $rest) $1)
   ;; 261. number => float
   (lambda ($1 . $rest) $1)
   ;; 262. string => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ;; 263. lone-comment => '$lone-comm
   (lambda ($1 . $rest) `(comm ,$1))
   ))

;; --- last line ---
