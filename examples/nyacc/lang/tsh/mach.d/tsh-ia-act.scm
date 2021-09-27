;; tsh-ia-act.scm

;; Copyright (C) 2021 Matthew R. Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING included with the this distribution.

(define tsh-ia-act-v
  (vector
   ;; $start => item
   (lambda ($1 . $rest) $1)
   ;; top => script
   (lambda ($1 . $rest) $1)
   ;; script => script-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; script-1 => item
   (lambda ($1 . $rest) (make-tl 'script $1))
   ;; script-1 => script-1 item
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; item => topl-decl term
   (lambda ($2 $1 . $rest) $1)
   ;; item => stmt term
   (lambda ($2 $1 . $rest) $1)
   ;; topl-decl => "source" string
   (lambda ($2 $1 . $rest) `(source ,$2))
   ;; topl-decl => "proc" ident "{" arg-list "}" "{" stmt "}"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(proc ,$2 ,$4 ,$7))
   ;; arg-list => arg-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; arg-list-1 => 
   (lambda $rest (make-tl 'arg-list))
   ;; arg-list-1 => arg-list-1 ident
   (lambda ($2 $1 . $rest)
     (tl-append $1 `(arg ,$2)))
   ;; arg-list-1 => arg-list-1 "{" ident unit-expr "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(opt-arg ,$3 ,$4)))
   ;; arg-list-1 => arg-list-1 "args"
   (lambda ($2 $1 . $rest)
     (tl-append $1 `(rest-arg (ident "args"))))
   ;; stmt-list => stmt-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; stmt-list-1 => stmt
   (lambda ($1 . $rest) (make-tl 'stmt-list $1))
   ;; stmt-list-1 => stmt term stmt-list-1
   (lambda ($3 $2 $1 . $rest) (tl-insert $3 $1))
   ;; stmt => decl-stmt
   (lambda ($1 . $rest) $1)
   ;; stmt => exec-stmt
   (lambda ($1 . $rest) $1)
   ;; stmt => 
   (lambda $rest `(empty-stmt))
   ;; stmt => '$lone-comm
   (lambda ($1 . $rest) `(comment ,$1))
   ;; decl-stmt => "set" ident unit-expr
   (lambda ($3 $2 $1 . $rest) `(set ,$2 ,$3))
   ;; decl-stmt => "set" ident/ix expr-list unit-expr
   (lambda ($4 $3 $2 $1 . $rest)
     `(set-ix ,$2 ,$3 ,$4))
   ;; exec-stmt => ident expr-seq
   (lambda ($2 $1 . $rest) `(call ,$1 ,@(cdr $2)))
   ;; exec-stmt => "{" stmt-list "}"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; exec-stmt => if-stmt
   (lambda ($1 . $rest) $1)
   ;; exec-stmt => "switch" unit-expr "{" case-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(switch ,$2 ,@(cdr $4)))
   ;; exec-stmt => "while" unit-expr "{" stmt-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(while ,$2 ,$4))
   ;; exec-stmt => "format" expr-seq
   (lambda ($2 $1 . $rest)
     `(format unquote (cdr $2)))
   ;; exec-stmt => "return"
   (lambda ($1 . $rest) `(return))
   ;; exec-stmt => "return" unit-expr
   (lambda ($2 $1 . $rest) `(return ,$2))
   ;; exec-stmt => "incr" ident
   (lambda ($2 $1 . $rest) `(incr TODO))
   ;; exec-stmt => "incr" ident unit-expr
   (lambda ($3 $2 $1 . $rest) `(incr TODO))
   ;; exec-stmt => "incr" ident/ix expr-list
   (lambda ($3 $2 $1 . $rest) `(incr TODO))
   ;; exec-stmt => "incr" ident/ix expr-list unit-expr
   (lambda ($4 $3 $2 $1 . $rest) `(incr TODO))
   ;; if-stmt => "if" unit-expr "{" stmt-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4))
   ;; if-stmt => "if" unit-expr "{" stmt-list "}" "else" "{" stmt-list "}"
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 (else ,$8)))
   ;; if-stmt => "if" unit-expr "{" stmt-list "}" elseif-list
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,@(sx-tail $6)))
   ;; if-stmt => "if" unit-expr "{" stmt-list "}" elseif-list "else" "{" st...
   (lambda ($10 $9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,@(sx-tail $6) (else ,$9)))
   ;; elseif-list => elseif-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; elseif-list-1 => "elseif" unit-expr "{" stmt-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (make-tl 'elseif-list `(elseif ,$2 ,$4)))
   ;; elseif-list-1 => elseif-list-1 "elseif" unit-expr "{" stmt-list "}"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (tl-append $1 'elseif-list `(elseif ,$2 ,$4)))
   ;; case-list => case-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; case-list => case-list-1 default-case-expr
   (lambda ($2 $1 . $rest)
     (append (tl->list $1) (list $2)))
   ;; case-list-1 => case-expr
   (lambda ($1 . $rest) (make-tl 'case-list $1))
   ;; case-list-1 => term
   (lambda ($1 . $rest) (make-tl 'case-list))
   ;; case-list-1 => case-list-1 case-expr
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; case-list-1 => case-list-1 term
   (lambda ($2 $1 . $rest) $1)
   ;; case-expr => unit-expr unit-expr
   (lambda ($2 $1 . $rest) `(case ,$1 ,$2))
   ;; case-expr => unit-expr "{" stmt-list "}"
   (lambda ($4 $3 $2 $1 . $rest) `(case ,$1 ,$3))
   ;; default-case-expr => "default" unit-expr
   (lambda ($2 $1 . $rest) `(case (default) ,$2))
   ;; unit-expr => primary-expression
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
   (lambda ($2 $1 . $rest) `(deref ,$2))
   ;; primary-expression => "$" '$ident/ix "(" expr-list ")"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(deref-indexed ,$2 ,$4))
   ;; primary-expression => fixed
   (lambda ($1 . $rest) $1)
   ;; primary-expression => float
   (lambda ($1 . $rest) $1)
   ;; primary-expression => string
   (lambda ($1 . $rest) $1)
   ;; primary-expression => symbol
   (lambda ($1 . $rest) $1)
   ;; primary-expression => "(" expr-list ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; primary-expression => "[" exec-stmt "]"
   (lambda ($3 $2 $1 . $rest) `(eval ,$2))
   ;; expr-list => expression expr-list-tail
   (lambda ($2 $1 . $rest)
     (tl->list (tl-insert $2 $1)))
   ;; expr-list => expression
   (lambda ($1 . $rest) $1)
   ;; expr-list-tail => "," expression
   (lambda ($2 $1 . $rest) (make-tl 'expr-list $2))
   ;; expr-list-tail => expr-list-tail "," expression
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
   ;; symbol => ident
   (lambda ($1 . $rest) $1)
   ;; term => ";"
   (lambda ($1 . $rest) $1)
   ;; term => "\n"
   (lambda ($1 . $rest) $1)
   ))

;;; end tables
