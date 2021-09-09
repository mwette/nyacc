;; tsh-act.scm

;; Copyright (C) 2021 Matthew R. Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING included with the this distribution.

(define tsh-act-v
  (vector
   ;; $start => top
   (lambda ($1 . $rest) $1)
   ;; top => item-list
   (lambda ($1 . $rest) $1)
   ;; item-list => item-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; item-list-1 => item term
   (lambda ($2 $1 . $rest) (make-tl 'item-list $1))
   ;; item-list-1 => item-list-1 item term
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; item => "proc" ident "{" arg-list "}" "{" stmt-list "}"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; item => stmt
   (lambda ($1 . $rest) $1)
   ;; arg-list => arg-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; arg-list-1 => 
   (lambda $rest (make-tl 'arg-list))
   ;; arg-list-1 => arg-list-1 ident
   (lambda ($2 $1 . $rest)
     (tl-append $1 `(arg ,$2)))
   ;; arg-list-1 => arg-list-1 "{" ident expr "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(arg ,$3 ,$4)))
   ;; stmt-list => stmt-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; stmt-list-1 => stmt
   (lambda ($1 . $rest) (make-tl 'stmt-list $1))
   ;; stmt-list-1 => stmt-list-1 term stmt
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; stmt => 
   (lambda $rest `(empty))
   ;; stmt => '$lone-comm
   (lambda ($1 . $rest) `(comment ,$1))
   ;; stmt => "{" stmt-list "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; stmt => "set" ident expr
   (lambda ($3 $2 $1 . $rest) `(set ,$2 ,$3))
   ;; stmt => "set" ident/ix expression-list expr
   (lambda ($4 $3 $2 $1 . $rest)
     `(set/ix ,$2 ,$3 ,$4))
   ;; stmt => if-stmt
   (lambda ($1 . $rest) $1)
   ;; if-stmt => "if" expr "{" stmt-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; expr => primary-expression
   (lambda ($1 . $rest) `(expr ,$1))
   ;; expression => logical-or-expression
   (lambda ($1 . $rest) $1)
   ;; logical-or-expression => logical-and-expression
   (lambda ($1 . $rest) $1)
   ;; logical-or-expression => logical-or-expression "||" logical-and-expre...
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; logical-and-expression => bitwise-or-expression
   (lambda ($1 . $rest) $1)
   ;; logical-and-expression => logical-and-expression "&&" bitwise-or-expr...
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; bitwise-or-expression => bitwise-xor-expression
   (lambda ($1 . $rest) $1)
   ;; bitwise-or-expression => bitwise-or-expression "|" bitwise-xor-expres...
   (lambda ($3 $2 $1 . $rest) `(bitwise-or ,$1 ,$3))
   ;; bitwise-xor-expression => bitwise-and-expression
   (lambda ($1 . $rest) $1)
   ;; bitwise-xor-expression => bitwise-xor-expression "^" bitwise-and-expr...
   (lambda ($3 $2 $1 . $rest)
     `(bitwise-xor ,$1 ,$3))
   ;; bitwise-and-expression => equality-expression
   (lambda ($1 . $rest) $1)
   ;; bitwise-and-expression => bitwise-and-expression "&" equality-expression
   (lambda ($3 $2 $1 . $rest)
     `(bitwise-and ,$1 ,$3))
   ;; equality-expression => relational-expression
   (lambda ($1 . $rest) $1)
   ;; equality-expression => equality-expression "==" relational-expression
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; equality-expression => equality-expression "!=" relational-expression
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; relational-expression => shift-expression
   (lambda ($1 . $rest) $1)
   ;; relational-expression => relational-expression "<" shift-expression
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; relational-expression => relational-expression "<=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; relational-expression => relational-expression ">" shift-expression
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; relational-expression => relational-expression ">=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; shift-expression => additive-expression
   (lambda ($1 . $rest) $1)
   ;; shift-expression => shift-expression "<<" additive-expression
   (lambda ($3 $2 $1 . $rest) `(lshift ,$1 ,$3))
   ;; shift-expression => shift-expression ">>" additive-expression
   (lambda ($3 $2 $1 . $rest) `(rshift ,$1 ,$3))
   ;; additive-expression => multiplicative-expression
   (lambda ($1 . $rest) $1)
   ;; additive-expression => additive-expression "+" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; additive-expression => additive-expression "-" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; multiplicative-expression => unary-expression
   (lambda ($1 . $rest) $1)
   ;; multiplicative-expression => multiplicative-expression "*" unary-expr...
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; multiplicative-expression => multiplicative-expression "/" unary-expr...
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; multiplicative-expression => multiplicative-expression "%" unary-expr...
   (lambda ($3 $2 $1 . $rest) `(mod ,$1 ,$3))
   ;; unary-expression => postfix-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => "-" unary-expression
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; unary-expression => "+" unary-expression
   (lambda ($2 $1 . $rest) `(pos ,$2))
   ;; unary-expression => "!" unary-expression
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; unary-expression => "~" unary-expression
   (lambda ($2 $1 . $rest) `(bitwise-not ,$2))
   ;; unary-expression => "++" unary-expression
   (lambda ($2 $1 . $rest) `(pre-inc ,$2))
   ;; unary-expression => "--" unary-expression
   (lambda ($2 $1 . $rest) `(pre-dec ,$2))
   ;; postfix-expression => primary-expression
   (lambda ($1 . $rest) $1)
   ;; postfix-expression => postfix-expression "++"
   (lambda ($2 $1 . $rest) `(post-inc ,$1))
   ;; postfix-expression => postfix-expression "--"
   (lambda ($2 $1 . $rest) `(post-dec ,$1))
   ;; primary-expression => "$" '$ident
   (lambda ($2 $1 . $rest) `(de-ref ,$2))
   ;; primary-expression => "$" '$ident/ix "(" expression-list ")"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(de-ref ,$2))
   ;; primary-expression => fixed
   (lambda ($1 . $rest) $1)
   ;; primary-expression => float
   (lambda ($1 . $rest) $1)
   ;; primary-expression => string
   (lambda ($1 . $rest) $1)
   ;; primary-expression => symbol
   (lambda ($1 . $rest) $1)
   ;; primary-expression => "(" expression-list ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; primary-expression => "[" ident expr-seq "]"
   (lambda ($4 $3 $2 $1 . $rest) `(call $2 $3))
   ;; expression-list => expression expression-list-tail
   (lambda ($2 $1 . $rest)
     (tl->list (tl-insert $2 $1)))
   ;; expression-list => expression
   (lambda ($1 . $rest) $1)
   ;; expression-list-tail => "," expression
   (lambda ($2 $1 . $rest) (make-tl 'expr-list $2))
   ;; expression-list-tail => expression-list-tail "," expression
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; expr-seq => expr-seq-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; expr-seq-1 => 
   (lambda $rest (make-tl 'seq-list))
   ;; expr-seq-1 => expr-seq-1 primary-expression
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; ident => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; ident/ix => '$ident/ix
   (lambda ($1 . $rest) `(ident/ix ,$1))
   ;; fixed => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; float => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; string => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ;; symbol => '$symbol
   (lambda ($1 . $rest) `(symbol ,$1))
   ;; term => ";"
   (lambda ($1 . $rest) $1)
   ;; term => "\n"
   (lambda ($1 . $rest) $1)
   ))

;;; end tables
