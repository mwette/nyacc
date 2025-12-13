;; c99cx-act.scm

;; Copyright (C) 2025 Matthew Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING included with the this distribution.

(define c99cx-act-v
  (vector
   ;; 0. $start => constant-expression
   (lambda ($1 . $rest) $1)
   ;; 1. primary-expression => identifier
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; 2. primary-expression => constant
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; 3. primary-expression => string-literal
   (lambda ($1 . $rest) `(p-expr ,(tl->list $1)))
   ;; 4. primary-expression => "(" constant-expression ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 5. postfix-expression => primary-expression
   (lambda ($1 . $rest) $1)
   ;; 6. postfix-expression => postfix-expression "[" constant-expression "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(array-ref ,$3 ,$1))
   ;; 7. postfix-expression => postfix-expression "." identifier
   (lambda ($3 $2 $1 . $rest) `(d-sel ,$3 ,$1))
   ;; 8. postfix-expression => postfix-expression "->" identifier
   (lambda ($3 $2 $1 . $rest) `(i-sel ,$3 ,$1))
   ;; 9. postfix-expression => postfix-expression "++"
   (lambda ($2 $1 . $rest) `(post-inc ,$1))
   ;; 10. postfix-expression => postfix-expression "--"
   (lambda ($2 $1 . $rest) `(post-dec ,$1))
   ;; 11. unary-expression => postfix-expression
   (lambda ($1 . $rest) $1)
   ;; 12. unary-expression => "++" unary-expression
   (lambda ($2 $1 . $rest) `(pre-inc ,$2))
   ;; 13. unary-expression => "--" unary-expression
   (lambda ($2 $1 . $rest) `(pre-dec ,$2))
   ;; 14. unary-expression => unary-operator cast-expression
   (lambda ($2 $1 . $rest) (list $1 $2))
   ;; 15. unary-expression => "sizeof" unary-expression
   (lambda ($2 $1 . $rest) `(sizeof-expr ,$2))
   ;; 16. unary-operator => "&"
   (lambda ($1 . $rest) 'ref-to)
   ;; 17. unary-operator => "*"
   (lambda ($1 . $rest) 'de-ref)
   ;; 18. unary-operator => "+"
   (lambda ($1 . $rest) 'pos)
   ;; 19. unary-operator => "-"
   (lambda ($1 . $rest) 'neg)
   ;; 20. unary-operator => "~"
   (lambda ($1 . $rest) 'bitwise-not)
   ;; 21. unary-operator => "!"
   (lambda ($1 . $rest) 'not)
   ;; 22. cast-expression => unary-expression
   (lambda ($1 . $rest) $1)
   ;; 23. multiplicative-expression => cast-expression
   (lambda ($1 . $rest) $1)
   ;; 24. multiplicative-expression => multiplicative-expression "*" cast-expre...
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 25. multiplicative-expression => multiplicative-expression "/" cast-expre...
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 26. multiplicative-expression => multiplicative-expression "%" cast-expre...
   (lambda ($3 $2 $1 . $rest) `(mod ,$1 ,$3))
   ;; 27. additive-expression => multiplicative-expression
   (lambda ($1 . $rest) $1)
   ;; 28. additive-expression => additive-expression "+" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; 29. additive-expression => additive-expression "-" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; 30. shift-expression => additive-expression
   (lambda ($1 . $rest) $1)
   ;; 31. shift-expression => shift-expression "<<" additive-expression
   (lambda ($3 $2 $1 . $rest) `(lshift ,$1 ,$3))
   ;; 32. shift-expression => shift-expression ">>" additive-expression
   (lambda ($3 $2 $1 . $rest) `(rshift ,$1 ,$3))
   ;; 33. relational-expression => shift-expression
   (lambda ($1 . $rest) $1)
   ;; 34. relational-expression => relational-expression "<" shift-expression
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; 35. relational-expression => relational-expression ">" shift-expression
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; 36. relational-expression => relational-expression "<=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; 37. relational-expression => relational-expression ">=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; 38. equality-expression => relational-expression
   (lambda ($1 . $rest) $1)
   ;; 39. equality-expression => equality-expression "==" relational-expression
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; 40. equality-expression => equality-expression "!=" relational-expression
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; 41. bitwise-and-expression => equality-expression
   (lambda ($1 . $rest) $1)
   ;; 42. bitwise-and-expression => bitwise-and-expression "&" equality-expression
   (lambda ($3 $2 $1 . $rest)
     `(bitwise-and ,$1 ,$3))
   ;; 43. bitwise-xor-expression => bitwise-and-expression
   (lambda ($1 . $rest) $1)
   ;; 44. bitwise-xor-expression => bitwise-xor-expression "^" bitwise-and-expr...
   (lambda ($3 $2 $1 . $rest)
     `(bitwise-xor ,$1 ,$3))
   ;; 45. bitwise-or-expression => bitwise-xor-expression
   (lambda ($1 . $rest) $1)
   ;; 46. bitwise-or-expression => bitwise-or-expression "|" bitwise-xor-expres...
   (lambda ($3 $2 $1 . $rest) `(bitwise-or ,$1 ,$3))
   ;; 47. logical-and-expression => bitwise-or-expression
   (lambda ($1 . $rest) $1)
   ;; 48. logical-and-expression => logical-and-expression "&&" bitwise-or-expr...
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; 49. logical-or-expression => logical-and-expression
   (lambda ($1 . $rest) $1)
   ;; 50. logical-or-expression => logical-or-expression "||" logical-and-expre...
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; 51. conditional-expression => logical-or-expression
   (lambda ($1 . $rest) $1)
   ;; 52. conditional-expression => logical-or-expression "?" constant-expressi...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(cond-expr ,$1 ,$3 ,$5))
   ;; 53. constant-expression => conditional-expression
   (lambda ($1 . $rest) $1)
   ;; 54. identifier => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; 55. constant => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; 56. constant => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; 57. constant => '$chlit
   (lambda ($1 . $rest) `(char ,$1))
   ;; 58. constant => '$chlit/L
   (lambda ($1 . $rest)
     `(char (@ (type "wchar_t")) ,$1))
   ;; 59. constant => '$chlit/u
   (lambda ($1 . $rest)
     `(char (@ (type "char16_t")) ,$1))
   ;; 60. constant => '$chlit/U
   (lambda ($1 . $rest)
     `(char (@ (type "char32_t")) ,$1))
   ;; 61. string-literal => '$string
   (lambda ($1 . $rest) (make-tl 'string $1))
   ;; 62. string-literal => string-literal '$string
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ))

;; --- last line ---
