;; cpp-act.scm

;; Copyright (C) 2016,2017 Matthew R. Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING included with the this distribution.

(define cpp-act-v
  (vector
   ;; 0. $start => conditional-expression
   (lambda ($1 . $rest) $1)
   ;; 1. conditional-expression => logical-or-expression
   (lambda ($1 . $rest) $1)
   ;; 2. conditional-expression => logical-or-expression "?" logical-or-expres...
   (lambda ($5 $4 $3 $2 $1 . $rest) `(cond-expr ,$1 ,$3 ,$5))
   ;; 3. logical-or-expression => logical-and-expression
   (lambda ($1 . $rest) $1)
   ;; 4. logical-or-expression => logical-or-expression "||" logical-and-expre...
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; 5. logical-and-expression => bitwise-or-expression
   (lambda ($1 . $rest) $1)
   ;; 6. logical-and-expression => logical-and-expression "&&" bitwise-or-expr...
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; 7. bitwise-or-expression => bitwise-xor-expression
   (lambda ($1 . $rest) $1)
   ;; 8. bitwise-or-expression => bitwise-or-expression "|" bitwise-xor-expres...
   (lambda ($3 $2 $1 . $rest) `(bitwise-or ,$1 ,$3))
   ;; 9. bitwise-xor-expression => bitwise-and-expression
   (lambda ($1 . $rest) $1)
   ;; 10. bitwise-xor-expression => bitwise-xor-expression "^" bitwise-and-expr...
   (lambda ($3 $2 $1 . $rest) `(bitwise-xor ,$1 ,$3))
   ;; 11. bitwise-and-expression => equality-expression
   (lambda ($1 . $rest) $1)
   ;; 12. bitwise-and-expression => bitwise-and-expression "&" equality-expression
   (lambda ($3 $2 $1 . $rest) `(bitwise-and ,$1 ,$3))
   ;; 13. equality-expression => relational-expression
   (lambda ($1 . $rest) $1)
   ;; 14. equality-expression => equality-expression "==" relational-expression
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; 15. equality-expression => equality-expression "!=" relational-expression
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; 16. relational-expression => shift-expression
   (lambda ($1 . $rest) $1)
   ;; 17. relational-expression => relational-expression "<" shift-expression
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; 18. relational-expression => relational-expression "<=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; 19. relational-expression => relational-expression ">" shift-expression
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; 20. relational-expression => relational-expression ">=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; 21. shift-expression => additive-expression
   (lambda ($1 . $rest) $1)
   ;; 22. shift-expression => shift-expression "<<" additive-expression
   (lambda ($3 $2 $1 . $rest) `(lshift ,$1 ,$3))
   ;; 23. shift-expression => shift-expression ">>" additive-expression
   (lambda ($3 $2 $1 . $rest) `(rshift ,$1 ,$3))
   ;; 24. additive-expression => multiplicative-expression
   (lambda ($1 . $rest) $1)
   ;; 25. additive-expression => additive-expression "+" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; 26. additive-expression => additive-expression "-" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; 27. multiplicative-expression => unary-expression
   (lambda ($1 . $rest) $1)
   ;; 28. multiplicative-expression => multiplicative-expression "*" unary-expr...
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 29. multiplicative-expression => multiplicative-expression "/" unary-expr...
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 30. multiplicative-expression => multiplicative-expression "%" unary-expr...
   (lambda ($3 $2 $1 . $rest) `(mod ,$1 ,$3))
   ;; 31. unary-expression => postfix-expression
   (lambda ($1 . $rest) $1)
   ;; 32. unary-expression => "-" unary-expression
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; 33. unary-expression => "+" unary-expression
   (lambda ($2 $1 . $rest) `(pos ,$2))
   ;; 34. unary-expression => "!" unary-expression
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; 35. unary-expression => "~" unary-expression
   (lambda ($2 $1 . $rest) `(bitwise-not ,$2))
   ;; 36. unary-expression => "++" unary-expression
   (lambda ($2 $1 . $rest) `(pre-inc ,$2))
   ;; 37. unary-expression => "--" unary-expression
   (lambda ($2 $1 . $rest) `(pre-dec ,$2))
   ;; 38. postfix-expression => primary-expression
   (lambda ($1 . $rest) $1)
   ;; 39. postfix-expression => postfix-expression "++"
   (lambda ($2 $1 . $rest) `(post-inc ,$1))
   ;; 40. postfix-expression => postfix-expression "--"
   (lambda ($2 $1 . $rest) `(post-dec ,$1))
   ;; 41. primary-expression => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; 42. primary-expression => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; 43. primary-expression => '$chlit
   (lambda ($1 . $rest) `(char ,$1))
   ;; 44. primary-expression => '$chlit/L
   (lambda ($1 . $rest) `(char (@ (type "wchar_t")) ,$1))
   ;; 45. primary-expression => '$chlit/u
   (lambda ($1 . $rest) `(char (@ (type "char16_t")) ,$1))
   ;; 46. primary-expression => '$chlit/U
   (lambda ($1 . $rest) `(char (@ (type "char32_t")) ,$1))
   ;; 47. primary-expression => "defined" "(" '$ident ")"
   (lambda ($4 $3 $2 $1 . $rest) `(defined ,$3))
   ;; 48. primary-expression => "defined" '$ident
   (lambda ($2 $1 . $rest) `(defined ,$2))
   ;; 49. primary-expression => "__has_include__" "(" '$string ")"
   (lambda ($4 $3 $2 $1 . $rest) `(has-include ,$3))
   ;; 50. primary-expression => "__has_include_next__" "(" '$string ")"
   (lambda ($4 $3 $2 $1 . $rest) `(has-include-next ,$3))
   ;; 51. primary-expression => "(" expression-list ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 52. expression-list => conditional-expression
   (lambda ($1 . $rest) $1)
   ;; 53. expression-list => expression-list "," conditional-expression
   (lambda ($3 $2 $1 . $rest) $3)
   ))

;; --- last line ---
