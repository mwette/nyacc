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
   (lambda ($2 $1 . $rest) `(script-file ,$1 ,@(sx-tail $2)))
   ;; 3. translation-unit => triv-stmt-list function-defn mlang-item-list
   (lambda ($3 $2 $1 . $rest)
     `(function-file ,@(sx-tail $1) ,$2 ,@(sx-tail $3)))
   ;; 4. translation-unit => function-defn mlang-item-list
   (lambda ($2 $1 . $rest) `(function-file ,$1 ,@(sx-tail $2)))
   ;; 5. translation-unit => triv-stmt-list class-defn mlang-item-list
   (lambda ($3 $2 $1 . $rest)
     `(classdef-file ,@(sx-tail $1) ,$2 ,@(sx-tail $3)))
   ;; 6. translation-unit => class-defn mlang-item-list
   (lambda ($2 $1 . $rest) `(classdef-file ,$1 ,@(sx-tail $2)))
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
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) `(class-defn ,$2 ,$4 ,@(cdr $6)))
   ;; 15. class-defn => "classdef" ident term class-parts "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(class-defn ,$2 ,@(cdr $4)))
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
   (lambda ($4 $3 $2 $1 . $rest) (tl-append $1 `(properties ,@(cdr $3))))
   ;; 23. class-parts-1 => class-parts-1 "methods" "(" attr-list ")" method-lis...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(methods ,$4 ,@(cdr $6))))
   ;; 24. class-parts-1 => class-parts-1 "methods" method-list "end"
   (lambda ($4 $3 $2 $1 . $rest) (tl-append $1 `(methods ,@(cdr $3))))
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
     `(fctn-defn ,$1 ,(if $2 `(stmt-list ,$2 ,@(cdr $3)) $3)))
   ;; 41. function-defn => function-decl non-comment-statement the-end
   (lambda ($3 $2 $1 . $rest)
     `(fctn-defn ,$1 ,(if $2 `(stmt-list ,$2) '(stmt-list))))
   ;; 42. function-defn => function-decl the-end
   (lambda ($2 $1 . $rest) `(fctn-defn ,$1 (stmt-list)))
   ;; 43. the-end => "end" term
   (lambda ($2 $1 . $rest) $1)
   ;; 44. function-decl => "function" function-sig term lone-comment-list
   (lambda ($4 $3 $2 $1 . $rest) (append $2 (list $4)))
   ;; 45. function-decl => "function" function-sig term
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 46. function-sig => "[" ident-list-out "]" "=" ident "(" ident-list-in ")"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest) `(fctn-decl ,$5 ,$7 ,$2))
   ;; 47. function-sig => "[" ident-list-out "]" "=" ident "(" ")"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) `(fctn-decl ,$5 (ident-list) ,$2))
   ;; 48. function-sig => ident-out "=" ident "(" ident-list-in ")"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$3 ,$5 (ident-list ,$1)))
   ;; 49. function-sig => ident-out "=" ident "(" ")"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$3 (ident-list) (ident-list ,$1)))
   ;; 50. function-sig => ident "(" ident-list-in ")"
   (lambda ($4 $3 $2 $1 . $rest) `(fctn-decl ,$1 ,$3 (ident-list)))
   ;; 51. function-sig => ident "(" ")"
   (lambda ($3 $2 $1 . $rest) `(fctn-decl ,$1 (ident-list) (ident-list)))
   ;; 52. ident-out => ident
   (lambda ($1 . $rest) $1)
   ;; 53. ident-out => "varargout"
   (lambda ($1 . $rest) '(varargout))
   ;; 54. ident-list-in => ident-list-in-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 55. ident-list-in => "varargin"
   (lambda ($1 . $rest) (list 'ident-list '(varargin)))
   ;; 56. ident-list-in => ident-list-in-1 "varargin"
   (lambda ($2 $1 . $rest) (tl->list (tl-append $1 '(varargin))))
   ;; 57. ident-list-in-1 => ident
   (lambda ($1 . $rest) (make-tl 'ident-list $1))
   ;; 58. ident-list-in-1 => ident-list-in-1 "," ident
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 59. ident-list-out => ident-list-out-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 60. ident-list-out => ident-list-out-1 "varargout"
   (lambda ($2 $1 . $rest) (tl->list (tl-append $1 '(varargout))))
   ;; 61. ident-list-out-1 => ident
   (lambda ($1 . $rest) (make-tl 'ident-list $1))
   ;; 62. ident-list-out-1 => ident-list-out-1 "," ident
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 63. q-ident-list => q-ident-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 64. q-ident-list-1 => q-ident
   (lambda ($1 . $rest) (make-tl 'ident-list $1))
   ;; 65. q-ident-list-1 => q-ident-list-1 "," q-ident
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 66. q-ident => q-ident-1
   (lambda ($1 . $rest) `(qident ,@(reverse $1)))
   ;; 67. q-ident-1 => '$ident
   (lambda ($1 . $rest) (list $1))
   ;; 68. q-ident-1 => q-ident-1 "." '$ident
   (lambda ($3 $2 $1 . $rest) (cons $3 $1))
   ;; 69. stmt-list => stmt-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 70. stmt-list-1 => statement
   (lambda ($1 . $rest)
     (if $1 (make-tl 'stmt-list $1) (make-tl 'stmt-list)))
   ;; 71. stmt-list-1 => stmt-list-1 statement
   (lambda ($2 $1 . $rest) (if $2 (tl-append $1 $2) $1))
   ;; 72. triv-stmt-list => triv-stmt-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 73. triv-stmt-list-1 => trivial-statement
   (lambda ($1 . $rest) (make-tl 'triv-stmt-list $1))
   ;; 74. triv-stmt-list-1 => triv-stmt-list-1 trivial-statement
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 75. statement => trivial-statement
   (lambda ($1 . $rest) $1)
   ;; 76. statement => nontrivial-statement
   (lambda ($1 . $rest) $1)
   ;; 77. non-comment-statement => term
   (lambda ($1 . $rest) '(empty-stmt))
   ;; 78. non-comment-statement => nontrivial-statement
   (lambda ($1 . $rest) $1)
   ;; 79. trivial-statement => lone-comment "\n"
   (lambda ($2 $1 . $rest) $1)
   ;; 80. trivial-statement => term
   (lambda ($1 . $rest) '(empty-stmt))
   ;; 81. nontrivial-statement => nontrivial-statement-1 term
   (lambda ($2 $1 . $rest) $1)
   ;; 82. nontrivial-statement-1 => expr
   (lambda ($1 . $rest) `(expr-stmt ,$1))
   ;; 83. nontrivial-statement-1 => expr "=" expr
   (lambda ($3 $2 $1 . $rest) `(assn ,$1 ,$3))
   ;; 84. nontrivial-statement-1 => "for" ident "=" expr term stmt-list "end"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) `(for ,$2 ,$4 ,$6))
   ;; 85. nontrivial-statement-1 => "while" expr term stmt-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(while ,$2 ,$4))
   ;; 86. nontrivial-statement-1 => "if" expr term stmt-list elseif-list "else"...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,@(cdr $5) (else ,$8)))
   ;; 87. nontrivial-statement-1 => "if" expr term stmt-list elseif-list "end"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4 ,@(cdr $5)))
   ;; 88. nontrivial-statement-1 => "if" expr term stmt-list "else" term stmt-l...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4 (else ,$7)))
   ;; 89. nontrivial-statement-1 => "if" expr term stmt-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4))
   ;; 90. nontrivial-statement-1 => "switch" expr term case-list "otherwise" te...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(switch ,$2 ,@(cdr $4) (otherwise ,$7)))
   ;; 91. nontrivial-statement-1 => "switch" expr term case-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(switch ,$2 ,@(cdr $4)))
   ;; 92. nontrivial-statement-1 => "return"
   (lambda ($1 . $rest) '(return))
   ;; 93. nontrivial-statement-1 => 'cmd-line
   (lambda ($1 . $rest) `(cmd-line ,@$1))
   ;; 94. nontrivial-statement-1 => 'cmd-call "(" expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest) `(cmd-call ,$1 ,@(sx-tail $3)))
   ;; 95. elseif-list => elseif-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 96. elseif-list-1 => "elseif" expr term stmt-list
   (lambda ($4 $3 $2 $1 . $rest) (make-tl 'elseif-list `(elseif ,$2 ,$4)))
   ;; 97. elseif-list-1 => elseif-list-1 "elseif" expr term stmt-list
   (lambda ($5 $4 $3 $2 $1 . $rest) (tl-append $1 `(elseif ,$3 ,$5)))
   ;; 98. case-list => case-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 99. case-list-1 => 
   (lambda $rest (make-tl 'case-list))
   ;; 100. case-list-1 => case-list-1 "case" case-expr term stmt-list
   (lambda ($5 $4 $3 $2 $1 . $rest) (tl-append $1 `(case ,$3 ,$5)))
   ;; 101. case-expr => fixed
   (lambda ($1 . $rest) $1)
   ;; 102. case-expr => string
   (lambda ($1 . $rest) $1)
   ;; 103. case-expr => q-ident
   (lambda ($1 . $rest) $1)
   ;; 104. case-expr => "{" fixed-list "}"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; 105. case-expr => "{" string-list "}"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; 106. fixed-list => fixed
   (lambda ($1 . $rest) (make-tl 'fixed-list $1))
   ;; 107. fixed-list => fixed-list fixed
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 108. string-list => string
   (lambda ($1 . $rest) (make-tl 'string-list $1))
   ;; 109. string-list => string-list string
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 110. expr-list => expr-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 111. expr-list-1 => expr
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; 112. expr-list-1 => "end"
   (lambda ($1 . $rest) (make-tl 'expr-list '(end)))
   ;; 113. expr-list-1 => expr-list-1 "," expr
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 114. expr-list-1 => expr-list-1 "," "end"
   (lambda ($3 $2 $1 . $rest) (tl-append $1 '(end)))
   ;; 115. expr => or-expr
   (lambda ($1 . $rest) $1)
   ;; 116. expr => ":"
   (lambda ($1 . $rest) `(colon-expr))
   ;; 117. expr => or-expr ":" or-expr
   (lambda ($3 $2 $1 . $rest) `(colon-expr ,$1 ,$3))
   ;; 118. expr => or-expr ":" or-expr ":" or-expr
   (lambda ($5 $4 $3 $2 $1 . $rest) `(colon-expr ,$1 ,$3 ,$5))
   ;; 119. expr => or-expr ":" "end"
   (lambda ($3 $2 $1 . $rest) `(colon-expr ,$1 (end)))
   ;; 120. expr => or-expr ":" or-expr ":" "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(colon-expr ,$1 ,$3 (end)))
   ;; 121. expr-nosp => or-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 122. expr-nosp => ":"
   (lambda ($1 . $rest) `(colon-expr))
   ;; 123. expr-nosp => or-expr-nosp ":" or-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(colon-expr ,$1 ,$3))
   ;; 124. expr-nosp => or-expr-nosp ":" or-expr-nosp ":" or-expr-nosp
   (lambda ($5 $4 $3 $2 $1 . $rest) `(colon-expr ,$1 ,$3 ,$5))
   ;; 125. expr-nosp => or-expr-nosp ":" "end"
   (lambda ($3 $2 $1 . $rest) `(colon-expr ,$1 (end)))
   ;; 126. expr-nosp => or-expr-nosp ":" or-expr-nosp ":" "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(colon-expr ,$1 ,$3 (end)))
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
   ;; 165. mul-expr => handle-expr
   (lambda ($1 . $rest) $1)
   ;; 166. mul-expr => mul-expr "*" handle-expr
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 167. mul-expr => mul-expr "/" handle-expr
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 168. mul-expr => mul-expr "\\" handle-expr
   (lambda ($3 $2 $1 . $rest) `(ldiv ,$1 ,$3))
   ;; 169. mul-expr => mul-expr "^" handle-expr
   (lambda ($3 $2 $1 . $rest) `(pow ,$1 ,$3))
   ;; 170. mul-expr => mul-expr ".*" handle-expr
   (lambda ($3 $2 $1 . $rest) `(dot-mul ,$1 ,$3))
   ;; 171. mul-expr => mul-expr "./" handle-expr
   (lambda ($3 $2 $1 . $rest) `(dot-div ,$1 ,$3))
   ;; 172. mul-expr => mul-expr ".\\" handle-expr
   (lambda ($3 $2 $1 . $rest) `(dot-ldiv ,$1 ,$3))
   ;; 173. mul-expr => mul-expr ".^" handle-expr
   (lambda ($3 $2 $1 . $rest) `(dot-pow ,$1 ,$3))
   ;; 174. mul-expr-nosp => handle-expr-nosp
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
   ;; 183. handle-expr => unary-expr
   (lambda ($1 . $rest) $1)
   ;; 184. handle-expr => "@" q-ident
   (lambda ($2 $1 . $rest) `(handle ,$2))
   ;; 185. handle-expr => "@" "(" q-ident-list ")" ident "(" q-ident-list ")"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest) `(handle ,$5 ,$7 ,$3))
   ;; 186. handle-expr-nosp => unary-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 187. handle-expr-nosp => "@" q-ident
   (lambda ($2 $1 . $rest) `(handle ,$2))
   ;; 188. handle-expr-nosp => "@" "(" q-ident-list ")" ident "(" q-ident-list ")"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest) `(handle ,$5 ,$7 ,$3))
   ;; 189. unary-expr => postfix-expr
   (lambda ($1 . $rest) $1)
   ;; 190. unary-expr => "-" postfix-expr
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; 191. unary-expr => "+" postfix-expr
   (lambda ($2 $1 . $rest) `(pos $2))
   ;; 192. unary-expr => "~" postfix-expr
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; 193. unary-expr => "~"
   (lambda ($1 . $rest) '(ignore))
   ;; 194. unary-expr-nosp => postfix-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 195. unary-expr-nosp => "-" postfix-expr-nosp
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; 196. unary-expr-nosp => "+" postfix-expr-nosp
   (lambda ($2 $1 . $rest) $2)
   ;; 197. unary-expr-nosp => "~" postfix-expr-nosp
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; 198. unary-expr-nosp => "~"
   (lambda ($1 . $rest) '(ignore))
   ;; 199. postfix-expr => primary-expr
   (lambda ($1 . $rest) $1)
   ;; 200. postfix-expr => postfix-expr "'"
   (lambda ($2 $1 . $rest) `(transpose ,$1))
   ;; 201. postfix-expr => postfix-expr ".'"
   (lambda ($2 $1 . $rest) `(conj-transpose ,$1))
   ;; 202. postfix-expr => postfix-expr "(" expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest) `(aref-or-call ,$1 ,$3))
   ;; 203. postfix-expr => postfix-expr "(" ")"
   (lambda ($3 $2 $1 . $rest) `(aref-or-call ,$1 (expr-list)))
   ;; 204. postfix-expr => postfix-expr "{" expr-list "}"
   (lambda ($4 $3 $2 $1 . $rest) `(cell-ref ,$1 ,$3))
   ;; 205. postfix-expr => postfix-expr "." ident
   (lambda ($3 $2 $1 . $rest) `(sel ,$3 ,$1))
   ;; 206. postfix-expr => postfix-expr ".(" ident ")"
   (lambda ($4 $3 $2 $1 . $rest) `(obj-prop ,$3 ,$1))
   ;; 207. postfix-expr-nosp => primary-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; 208. postfix-expr-nosp => postfix-expr-nosp "'"
   (lambda ($2 $1 . $rest) `(transpose ,$1))
   ;; 209. postfix-expr-nosp => postfix-expr-nosp ".'"
   (lambda ($2 $1 . $rest) `(conj-transpose ,$1))
   ;; 210. postfix-expr-nosp => postfix-expr-nosp "(" expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest) `(aref-or-call ,$1 ,$3))
   ;; 211. postfix-expr-nosp => postfix-expr-nosp "(" ")"
   (lambda ($3 $2 $1 . $rest) `(aref-or-call ,$1 (expr-list)))
   ;; 212. postfix-expr-nosp => postfix-expr-nosp "{" expr-list "}"
   (lambda ($4 $3 $2 $1 . $rest) `(cell-ref ,$1 ,$3))
   ;; 213. postfix-expr-nosp => postfix-expr-nosp "." ident
   (lambda ($3 $2 $1 . $rest) `(sel ,$3 ,$1))
   ;; 214. postfix-expr-nosp => postfix-expr-nosp ".(" ident ")"
   (lambda ($4 $3 $2 $1 . $rest) `(obj-prop ,$3 ,$1))
   ;; 215. primary-expr => ident
   (lambda ($1 . $rest) $1)
   ;; 216. primary-expr => "nvarargin"
   (lambda ($1 . $rest) '(ident "nvarargin"))
   ;; 217. primary-expr => "nvarargout"
   (lambda ($1 . $rest) '(ident "nvarargout"))
   ;; 218. primary-expr => number
   (lambda ($1 . $rest) $1)
   ;; 219. primary-expr => string
   (lambda ($1 . $rest) $1)
   ;; 220. primary-expr => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 221. primary-expr => "[" "]"
   (lambda ($2 $1 . $rest) '(matrix))
   ;; 222. primary-expr => "[" matrix-row-list "]"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 223. primary-expr => "{" "}"
   (lambda ($2 $1 . $rest) '(cell-array))
   ;; 224. primary-expr => "{" matrix-row-list "}"
   (lambda ($3 $2 $1 . $rest) `(cell-array unquote (cdr $2)))
   ;; 225. primary-expr-nosp => ident
   (lambda ($1 . $rest) $1)
   ;; 226. primary-expr-nosp => number
   (lambda ($1 . $rest) $1)
   ;; 227. primary-expr-nosp => string
   (lambda ($1 . $rest) $1)
   ;; 228. primary-expr-nosp => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) `(wrap ,$2))
   ;; 229. primary-expr-nosp => "[" "]"
   (lambda ($2 $1 . $rest) '(matrix))
   ;; 230. primary-expr-nosp => "[" matrix-row-list "]"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 231. primary-expr-nosp => "{" "}"
   (lambda ($2 $1 . $rest) '(cell-array))
   ;; 232. primary-expr-nosp => "{" matrix-row-list "}"
   (lambda ($3 $2 $1 . $rest) `(cell-array unquote (cdr $2)))
   ;; 233. matrix-row-list => matrix-row-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 234. matrix-row-list-1 => matrix-row
   (lambda ($1 . $rest) (make-tl 'matrix $1))
   ;; 235. matrix-row-list-1 => matrix-row-list-1 row-term matrix-row
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 236. row-term => ";"
   (lambda ($1 . $rest) $1)
   ;; 237. row-term => "\n"
   (lambda ($1 . $rest) $1)
   ;; 238. matrix-row => matrix-row-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 239. matrix-row-1 => expr-nosp
   (lambda ($1 . $rest) (make-tl 'row $1))
   ;; 240. matrix-row-1 => matrix-row-1 "," expr-nosp
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 241. matrix-row-1 => matrix-row-1 'sp expr-nosp
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; 242. term-list => term
   (lambda ($1 . $rest) $1)
   ;; 243. term-list => term-list term
   (lambda ($2 $1 . $rest) $1)
   ;; 244. term => ";" '$code-comm "\n"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; 245. term => ";" "\n"
   (lambda ($2 $1 . $rest) $1)
   ;; 246. term => ";"
   (lambda ($1 . $rest) $1)
   ;; 247. term => ","
   (lambda ($1 . $rest) $1)
   ;; 248. term => "\n"
   (lambda ($1 . $rest) $1)
   ;; 249. lone-comment-list => lone-comment-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; 250. lone-comment-list-1 => lone-comment "\n"
   (lambda ($2 $1 . $rest) (make-tl 'comm-list $1))
   ;; 251. lone-comment-list-1 => lone-comment-list-1 lone-comment "\n"
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; 252. ident => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; 253. fixed => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; 254. float => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; 255. number => fixed
   (lambda ($1 . $rest) $1)
   ;; 256. number => float
   (lambda ($1 . $rest) $1)
   ;; 257. string => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ;; 258. lone-comment => '$lone-comm
   (lambda ($1 . $rest) `(comm ,$1))
   ))

;; --- last line ---
