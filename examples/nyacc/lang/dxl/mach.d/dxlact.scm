;; mach.d/dxlact.scm

;; Copyright (C) 2015 Matthew R. Wette -- all rights reserved

(define act-v
  (vector
   ;; $start => program-proxy
   (lambda ($1 . $rest) $1)
   ;; program-proxy => program
   (lambda ($1 . $rest) (tl->list $1))
   ;; program => program-element
   (lambda ($1 . $rest)
     (if $1 (make-tl 'program $1) (make-tl 'program)))
   ;; program => program ";" program-element
   (lambda ($3 $2 $1 . $rest)
     (if $3 (tl-append $1 $3) $1))
   ;; program-element => 
   (lambda $rest #f)
   ;; program-element => pragma
   (lambda ($1 . $rest) $1)
   ;; program-element => statement
   (lambda ($1 . $rest) $1)
   ;; program-element => function-definition
   (lambda ($1 . $rest) $1)
   ;; pragma => "pragma" identifier const-expr-list
   (lambda ($3 $2 $1 . $rest)
     `(pragma ,$2 ,(tl->list $3)))
   ;; const-expr-list => 
   (lambda $rest (make-tl 'const-expr-list))
   ;; const-expr-list => const-expr-list "," constant
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; function-definition => type-name identifier "(" param-list ")" "{" st...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-defn ,$1 ,$2 ,(tl->list $4) ,(tl->list $7)))
   ;; function-definition => type-name identifier "(" ")" "{" statement-lis...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-defn ,$1 ,$2 (param-list) ,(tl->list $6)))
   ;; param-list => type-name identifier
   (lambda ($2 $1 . $rest)
     (make-tl 'param-list `(param ,$1 ,$2)))
   ;; param-list => type-name "&" identifier
   (lambda ($3 $2 $1 . $rest)
     (make-tl 'param-list `(ref-param ,$1 ,$2)))
   ;; param-list => param-list "," type-name identifier
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 `(param ,$3 ,$4)))
   ;; param-list => param-list "," type-name "&" identifier
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(ref-param ,$3 ,$4)))
   ;; param-list => param-list "," identifier
   (lambda ($3 $2 $1 . $rest)
     (tl-append $1 `(param ,$3)))
   ;; param-list => param-list "," "&" identifier
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 `(ref-param ,$3 ,$4)))
   ;; statement => "if" "(" conditional-expression ")" statement
   (lambda ($5 $4 $3 $2 $1 . $rest) `(if ,$3 ,$5))
   ;; statement => "if" "(" conditional-expression ")" statement "else" sta...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$3 ,$5 ,$7))
   ;; statement => "if" expression "then" statement
   (lambda ($4 $3 $2 $1 . $rest) `(if ,$2 ,$4))
   ;; statement => "if" expression "then" statement "else" statement
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) `(if))
   ;; statement => "for" identifier "in" expression "do" statement
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(for-in ,$2 ,$4 ,$6))
   ;; statement => "for" identifier "in" expression "->" expression "do" st...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for-all-olinks ,$2 ,$4 ,$6 ,$8))
   ;; statement => "for" identifier "in" expression "<-" expression "do" st...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for-all-ilinks ,$2 ,$4 ,$6 ,$8))
   ;; statement => "for" for-expressions statement
   (lambda ($3 $2 $1 . $rest)
     `(for ,(car $2) ,(cadr $2) ,(caddr $2) ,$3))
   ;; statement => "while" "(" expression ")" statement
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(while ,$3 ,$5))
   ;; statement => "{" "}"
   (lambda ($2 $1 . $rest) '(comp-stmt))
   ;; statement => "{" statement-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(comp-stmt ,(tl->list $2)))
   ;; statement => type-name variable-definition-list
   (lambda ($2 $1 . $rest)
     `(vble-defn ,$1 ,(tl->list $2)))
   ;; statement => expression
   (lambda ($1 . $rest) `(expr-stmt ,$1))
   ;; statement => "return" expression
   (lambda ($2 $1 . $rest) `(return ,$2))
   ;; statement => "return"
   (lambda ($1 . $rest) `(return))
   ;; type-name => type-name-1
   (lambda ($1 . $rest) `(type-name ,$1))
   ;; type-name-1 => "bool"
   (lambda ($1 . $rest) $1)
   ;; type-name-1 => "char"
   (lambda ($1 . $rest) $1)
   ;; type-name-1 => "int"
   (lambda ($1 . $rest) $1)
   ;; type-name-1 => "real"
   (lambda ($1 . $rest) $1)
   ;; type-name-1 => "void"
   (lambda ($1 . $rest) $1)
   ;; type-name-1 => "string"
   (lambda ($1 . $rest) $1)
   ;; type-name-1 => 'built-in-type
   (lambda ($1 . $rest) $1)
   ;; variable-definition-list => variable-definition-list-item
   (lambda ($1 . $rest)
     (make-tl 'vble-defn-list $1))
   ;; variable-definition-list => variable-definition-list "," variable-def...
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; variable-definition-list-item => identifier
   (lambda ($1 . $rest) `(vble-list-item ,$1))
   ;; variable-definition-list-item => identifier "=" conditional-expression
   (lambda ($3 $2 $1 . $rest)
     `(vble-list-item ,$1 (initzer ,$3)))
   ;; for-expressions => "(" opt-expr-w-term opt-expr-w-term expression ")"
   (lambda ($5 $4 $3 $2 $1 . $rest) (list $2 $3 $4))
   ;; for-expressions => "(" opt-expr-w-term opt-expr-w-term ")"
   (lambda ($4 $3 $2 $1 . $rest)
     (list $2 $3 '(expr)))
   ;; opt-expr-w-term => expression ";"
   (lambda ($2 $1 . $rest) $1)
   ;; opt-expr-w-term => ";"
   (lambda ($1 . $rest) '(expr))
   ;; statement-list => statement
   (lambda ($1 . $rest) (make-tl 'stmt-list $1))
   ;; statement-list => statement-list ";" statement
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; expression => comma-expression
   (lambda ($1 . $rest) $1)
   ;; comma-expression => assignment-expression
   (lambda ($1 . $rest) $1)
   ;; comma-expression => comma-expression "," assignment-expression
   (lambda ($3 $2 $1 . $rest) `(comma-expr ,$1 ,$3))
   ;; assignment-expression => conditional-expression
   (lambda ($1 . $rest) $1)
   ;; assignment-expression => postfix-expression assignment-op assignment-...
   (lambda ($3 $2 $1 . $rest)
     `(assn-expr ,$1 (op ,$2) ,$3))
   ;; assignment-op => "="
   (lambda ($1 . $rest) $1)
   ;; assignment-op => "+="
   (lambda ($1 . $rest) $1)
   ;; assignment-op => "-="
   (lambda ($1 . $rest) $1)
   ;; assignment-op => "*="
   (lambda ($1 . $rest) $1)
   ;; assignment-op => "/="
   (lambda ($1 . $rest) $1)
   ;; assignment-op => "%="
   (lambda ($1 . $rest) $1)
   ;; assignment-op => "<<="
   (lambda ($1 . $rest) $1)
   ;; assignment-op => ">>="
   (lambda ($1 . $rest) $1)
   ;; assignment-op => "&="
   (lambda ($1 . $rest) $1)
   ;; assignment-op => "^="
   (lambda ($1 . $rest) $1)
   ;; assignment-op => "|="
   (lambda ($1 . $rest) $1)
   ;; conditional-expression => logical-or-expression
   (lambda ($1 . $rest) $1)
   ;; conditional-expression => logical-or-expression "?" expression ":" co...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(cond-expr ,$1 ,$3 ,$5))
   ;; logical-or-expression => logical-and-expression
   (lambda ($1 . $rest) $1)
   ;; logical-or-expression => logical-or-expression "||" logical-and-expre...
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; logical-or-expression => logical-or-expression "or" logical-and-expre...
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; logical-and-expression => bitwise-or-expression
   (lambda ($1 . $rest) $1)
   ;; logical-and-expression => logical-and-expression "&&" bitwise-or-expr...
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; logical-and-expression => logical-and-expression "and" bitwise-or-exp...
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; bitwise-or-expression => bitwise-xor-expression
   (lambda ($1 . $rest) $1)
   ;; bitwise-or-expression => bitwise-or-expression "|" bitwise-xor-expres...
   (lambda ($3 $2 $1 . $rest) `(bit-or ,$1 ,$3))
   ;; bitwise-xor-expression => bitwise-and-expression
   (lambda ($1 . $rest) $1)
   ;; bitwise-xor-expression => bitwise-xor-expression "^" bitwise-and-expr...
   (lambda ($3 $2 $1 . $rest) `(bit-xor ,$1 ,$3))
   ;; bitwise-and-expression => equality-expression
   (lambda ($1 . $rest) $1)
   ;; bitwise-and-expression => bitwise-and-expression "&" equality-expression
   (lambda ($3 $2 $1 . $rest) `(bit-and ,$1 ,$3))
   ;; equality-expression => relational-expression
   (lambda ($1 . $rest) $1)
   ;; equality-expression => equality-expression "==" relational-expression
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; equality-expression => equality-expression "!=" relational-expression
   (lambda ($3 $2 $1 . $rest) `(neq ,$1 ,$3))
   ;; relational-expression => shift-expression
   (lambda ($1 . $rest) $1)
   ;; relational-expression => relational-expression "<" shift-expression
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; relational-expression => relational-expression "<=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; relational-expression => relational-expression ">" shift-expression
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; relational-expression => relational-expression ">=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; shift-expression => additive-expression
   (lambda ($1 . $rest) $1)
   ;; shift-expression => shift-expression shift-op additive-expression
   (lambda ($3 $2 $1 . $rest) (list $2 $1 $3))
   ;; shift-op => "<<"
   (lambda ($1 . $rest) 'lshift)
   ;; shift-op => ">>"
   (lambda ($1 . $rest) 'rshift)
   ;; additive-expression => multiplicative-expression
   (lambda ($1 . $rest) $1)
   ;; additive-expression => additive-expression add-op multiplicative-expr...
   (lambda ($3 $2 $1 . $rest) (list $2 $1 $3))
   ;; add-op => "+"
   (lambda ($1 . $rest) 'add)
   ;; add-op => "-"
   (lambda ($1 . $rest) 'sub)
   ;; multiplicative-expression => cast-expression
   (lambda ($1 . $rest) $1)
   ;; multiplicative-expression => multiplicative-expression mult-op cast-e...
   (lambda ($3 $2 $1 . $rest) (list $2 $1 $3))
   ;; mult-op => "*"
   (lambda ($1 . $rest) 'mul)
   ;; mult-op => "/"
   (lambda ($1 . $rest) 'div)
   ;; mult-op => "%"
   (lambda ($1 . $rest) 'mod)
   ;; cast-expression => unary-expression
   (lambda ($1 . $rest) $1)
   ;; cast-expression => "(" type-name cast-expression ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(cast-expr ,$2 ,$3))
   ;; unary-expression => concatenation
   (lambda ($1 . $rest) $1)
   ;; unary-expression => "sizeof" unary-expression
   (lambda ($2 $1 . $rest) `(sizeof ,$2))
   ;; unary-expression => "-" unary-expression
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; unary-expression => "+" unary-expression
   (lambda ($2 $1 . $rest) `(pos ,$2))
   ;; unary-expression => "!" unary-expression
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; unary-expression => "~" unary-expression
   (lambda ($2 $1 . $rest) `(what ,$2))
   ;; unary-expression => "&" cast-expression
   (lambda ($2 $1 . $rest) `(ref-to ,$2))
   ;; unary-expression => "++" unary-expression
   (lambda ($2 $1 . $rest) `(pre-inc ,$2))
   ;; unary-expression => "--" unary-expression
   (lambda ($2 $1 . $rest) `(pre-dec ,$2))
   ;; concatenation => postfix-expression
   (lambda ($1 . $rest) $1)
   ;; concatenation => postfix-expression concatenation
   (lambda ($2 $1 . $rest) `(concat ,$1 ,$2))
   ;; postfix-expression => primary-expression
   (lambda ($1 . $rest) $1)
   ;; postfix-expression => postfix-expression "." '$string
   (lambda ($3 $2 $1 . $rest)
     `(obj-sel ,$1 (string ,$3)))
   ;; postfix-expression => postfix-expression "." identifier
   (lambda ($3 $2 $1 . $rest) `(obj-sel ,$1 ,$3))
   ;; postfix-expression => postfix-expression "[" conditional-expression "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(array-ref ,$1 ,$3))
   ;; postfix-expression => postfix-expression "++"
   (lambda ($2 $1 . $rest) `(post-inc ,$1))
   ;; postfix-expression => postfix-expression "--"
   (lambda ($2 $1 . $rest) `(post-dec ,$1))
   ;; postfix-expression => postfix-expression ":" primary-expression
   (lambda ($3 $2 $1 . $rest) `(range ,$1 ,$3))
   ;; postfix-expression => postfix-expression ":" primary-expression "by" ...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(range ,$1 ,$3 ,$5))
   ;; primary-expression => identifier
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; primary-expression => constant
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; primary-expression => "(" expression ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; primary-expression => "(" "current" type-name ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(current (@ (type ,(sx-ref $3 1)))))
   ;; primary-expression => "current"
   (lambda ($1 . $rest) '(current))
   ;; identifier => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; constant => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; constant => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; constant => '$chlit
   (lambda ($1 . $rest) `(char ,$1))
   ;; constant => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ))

;;; end tables
