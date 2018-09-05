;; mach.d/oct-act.scm

;; Copyright 2015-2018 Matthew R. Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING.LESSER included with the this distribution.

(define oct-act-v
  (vector
   ;; $start => mfile
   (lambda ($1 . $rest) $1)
   ;; mfile => script-file
   (lambda ($1 . $rest)
     (tl->list (add-file-attr $1)))
   ;; mfile => function-file
   (lambda ($1 . $rest)
     (tl->list (add-file-attr $1)))
   ;; script-file => lone-comment-list non-comment-statement
   (lambda ($2 $1 . $rest)
     (make-tl 'script-file (tl->list $1)))
   ;; script-file => non-comment-statement
   (lambda ($1 . $rest)
     (if $1
       (make-tl 'script-file $1)
       (make-tl 'script-file)))
   ;; script-file => script-file statement
   (lambda ($2 $1 . $rest)
     (if $2 (tl-append $1 $2) $1))
   ;; function-file => function-defn
   (lambda ($1 . $rest) (make-tl 'function-file $1))
   ;; function-file => function-file function-defn
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; function-defn => function-decl non-comment-statement stmt-list opt-end
   (lambda ($4 $3 $2 $1 . $rest)
     `(fctn-defn
        ,$1
        ,(tl->list (if $2 (tl-insert $3 $2) $3))))
   ;; function-defn => function-decl non-comment-statement opt-end
   (lambda ($3 $2 $1 . $rest)
     `(fctn-defn
        ,$1
        ,(if $2 `(stmt-list ,$2) '(stmt-list))))
   ;; function-defn => function-decl opt-end
   (lambda ($2 $1 . $rest)
     `(fctn-defn ,$1 (stmt-list)))
   ;; opt-end => 
   (lambda $rest (list))
   ;; opt-end => "end" term-list
   (lambda ($2 $1 . $rest) $1)
   ;; function-decl => function-decl-line lone-comment-list
   (lambda ($2 $1 . $rest)
     (append $1 (list (tl->list $2))))
   ;; function-decl => function-decl-line
   (lambda ($1 . $rest) $1)
   ;; function-decl-line => "function" "[" ident-list "]" "=" ident "(" ide...
   (lambda ($10 $9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$6 ,(tl->list $8) ,(tl->list $3)))
   ;; function-decl-line => "function" "[" ident-list "]" "=" ident "(" ")"...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$6 (ident-list) ,(tl->list $3)))
   ;; function-decl-line => "function" ident "=" ident "(" ident-list ")" term
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$4 ,(tl->list $6) (ident-list ,$2)))
   ;; function-decl-line => "function" ident "=" ident "(" ")" term
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$4 (ident-list) (ident-list ,$2)))
   ;; function-decl-line => "function" ident "(" ident-list ")" term
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$2 ,(tl->list $4) (ident-list)))
   ;; function-decl-line => "function" ident "(" ")" term
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$2 (ident-list) (ident-list)))
   ;; ident-list => ident
   (lambda ($1 . $rest) (make-tl 'ident-list $1))
   ;; ident-list => ident-list "," ident
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; stmt-list => statement
   (lambda ($1 . $rest)
     (if $1
       (make-tl 'stmt-list $1)
       (make-tl 'stmt-list)))
   ;; stmt-list => stmt-list statement
   (lambda ($2 $1 . $rest)
     (if $2 (tl-append $1 $2) $1))
   ;; statement => lone-comment
   (lambda ($1 . $rest) $1)
   ;; statement => non-comment-statement
   (lambda ($1 . $rest) $1)
   ;; non-comment-statement => non-comment-statement-1 term
   (lambda ($2 $1 . $rest)
     (sx-attr-add $1 'term $2))
   ;; non-comment-statement-1 => 
   (lambda $rest '(empty-stmt))
   ;; non-comment-statement-1 => expr
   (lambda ($1 . $rest) `(expr-stmt ,$1))
   ;; non-comment-statement-1 => expr "=" expr
   (lambda ($3 $2 $1 . $rest) `(assn ,$1 ,$3))
   ;; non-comment-statement-1 => "for" ident "=" expr term stmt-list "end"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for ,$2 ,$4 ,(tl->list $6)))
   ;; non-comment-statement-1 => "while" expr term stmt-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(while ,$2 ,(tl->list $4)))
   ;; non-comment-statement-1 => "if" expr term stmt-list elseif-list "else...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2
        ,(tl->list $4)
        ,@(cdr (tl->list $5))
        (else ,(tl->list $7))))
   ;; non-comment-statement-1 => "if" expr term stmt-list elseif-list "end"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,(tl->list $4) ,@(cdr (tl->list $5))))
   ;; non-comment-statement-1 => "if" expr term stmt-list "else" stmt-list ...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,(tl->list $4) (else ,(tl->list $6))))
   ;; non-comment-statement-1 => "if" expr term stmt-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,(tl->list $4)))
   ;; non-comment-statement-1 => "switch" expr term case-list "otherwise" t...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(switch
        ,$2
        ,@(cdr (tl->list $4))
        (otherwise ,(tl->list $7))))
   ;; non-comment-statement-1 => "switch" expr term case-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(switch ,$2 ,@(cdr (tl->list $4))))
   ;; non-comment-statement-1 => "return"
   (lambda ($1 . $rest) '(return))
   ;; non-comment-statement-1 => command arg-list
   (lambda ($2 $1 . $rest)
     `(command ,$1 ,(tl->list $2)))
   ;; command => "global"
   (lambda ($1 . $rest) '(ident "global"))
   ;; command => "clear"
   (lambda ($1 . $rest) '(ident "clear"))
   ;; arg-list => ident
   (lambda ($1 . $rest)
     (make-tl 'arg-list (cons 'arg (cdr $1))))
   ;; arg-list => arg-list ident
   (lambda ($2 $1 . $rest)
     (tl-append $1 (cons 'arg $2)))
   ;; elseif-list => "elseif" expr term stmt-list
   (lambda ($4 $3 $2 $1 . $rest)
     (make-tl
       'elseif-list
       `(elseif ,$2 ,(tl->list $4))))
   ;; elseif-list => elseif-list "elseif" expr term stmt-list
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(elseif ,$3 ,(tl->list $5))))
   ;; case-list => 
   (lambda $rest (make-tl 'case-list))
   ;; case-list => case-list "case" case-expr term stmt-list
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(case ,$3 ,(tl->list $5))))
   ;; case-expr => fixed
   (lambda ($1 . $rest) $1)
   ;; case-expr => string
   (lambda ($1 . $rest) $1)
   ;; case-expr => "{" fixed-list "}"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; case-expr => "{" string-list "}"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; fixed-list => fixed
   (lambda ($1 . $rest) (make-tl 'fixed-list $1))
   ;; fixed-list => fixed-list fixed
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; string-list => string
   (lambda ($1 . $rest) (make-tl 'string-list $1))
   ;; string-list => string-list string
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; expr-list => expr
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; expr-list => expr-list "," expr
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; expr => or-expr
   (lambda ($1 . $rest) $1)
   ;; expr => or-expr ":" or-expr
   (lambda ($3 $2 $1 . $rest) `(colon-expr ,$1 ,$3))
   ;; expr => or-expr ":" or-expr ":" or-expr
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 ,$5))
   ;; expr => or-expr ":" "end"
   (lambda ($3 $2 $1 . $rest)
     `(colon-expr ,$1 (end)))
   ;; expr => or-expr ":" or-expr ":" "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 (end)))
   ;; or-expr => and-expr
   (lambda ($1 . $rest) $1)
   ;; or-expr => or-expr "|" and-expr
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; and-expr => equality-expr
   (lambda ($1 . $rest) $1)
   ;; and-expr => and-expr "&" equality-expr
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; equality-expr => rel-expr
   (lambda ($1 . $rest) $1)
   ;; equality-expr => equality-expr "==" rel-expr
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; equality-expr => equality-expr "~=" rel-expr
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; rel-expr => add-expr
   (lambda ($1 . $rest) $1)
   ;; rel-expr => rel-expr "<" add-expr
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; rel-expr => rel-expr ">" add-expr
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; rel-expr => rel-expr "<=" add-expr
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; rel-expr => rel-expr ">=" add-expr
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; add-expr => mul-expr
   (lambda ($1 . $rest) $1)
   ;; add-expr => add-expr "+" mul-expr
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; add-expr => add-expr "-" mul-expr
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; add-expr => add-expr ".+" mul-expr
   (lambda ($3 $2 $1 . $rest) `(dot-add ,$1 ,$3))
   ;; add-expr => add-expr ".-" mul-expr
   (lambda ($3 $2 $1 . $rest) `(dot-sub ,$1 ,$3))
   ;; mul-expr => unary-expr
   (lambda ($1 . $rest) $1)
   ;; mul-expr => mul-expr "*" unary-expr
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; mul-expr => mul-expr "/" unary-expr
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; mul-expr => mul-expr "\\" unary-expr
   (lambda ($3 $2 $1 . $rest) `(ldiv ,$1 ,$3))
   ;; mul-expr => mul-expr "^" unary-expr
   (lambda ($3 $2 $1 . $rest) `(pow ,$1 ,$3))
   ;; mul-expr => mul-expr ".*" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-mul ,$1 ,$3))
   ;; mul-expr => mul-expr "./" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-div ,$1 ,$3))
   ;; mul-expr => mul-expr ".\\" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-ldiv ,$1 ,$3))
   ;; mul-expr => mul-expr ".^" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-pow ,$1 ,$3))
   ;; unary-expr => postfix-expr
   (lambda ($1 . $rest) $1)
   ;; unary-expr => "-" postfix-expr
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; unary-expr => "+" postfix-expr
   (lambda ($2 $1 . $rest) $2)
   ;; unary-expr => "~" postfix-expr
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; postfix-expr => primary-expr
   (lambda ($1 . $rest) $1)
   ;; postfix-expr => postfix-expr "'"
   (lambda ($2 $1 . $rest) `(transpose ,$1))
   ;; postfix-expr => postfix-expr ".'"
   (lambda ($2 $1 . $rest) `(conj-transpose ,$1))
   ;; postfix-expr => postfix-expr "(" expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(aref-or-call ,$1 ,(tl->list $3)))
   ;; postfix-expr => postfix-expr "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(aref-or-call ,$1 (expr-list)))
   ;; postfix-expr => postfix-expr "." ident
   (lambda ($3 $2 $1 . $rest) `(sel ,$3 ,$1))
   ;; primary-expr => ident
   (lambda ($1 . $rest) $1)
   ;; primary-expr => number
   (lambda ($1 . $rest) $1)
   ;; primary-expr => string
   (lambda ($1 . $rest) $1)
   ;; primary-expr => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; primary-expr => "[" "]"
   (lambda ($2 $1 . $rest) '(matrix))
   ;; primary-expr => "[" matrix-row-list "]"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; primary-expr => "{" "}"
   (lambda ($2 $1 . $rest) '(cell-array))
   ;; primary-expr => "{" matrix-row-list "}"
   (lambda ($3 $2 $1 . $rest)
     (cons 'cell-array (cdr (tl->list $2))))
   ;; matrix-row-list => matrix-row
   (lambda ($1 . $rest)
     (make-tl 'matrix (tl->list $1)))
   ;; matrix-row-list => matrix-row-list row-term matrix-row
   (lambda ($3 $2 $1 . $rest)
     (tl-append $1 (tl->list $3)))
   ;; row-term => ";"
   (lambda ($1 . $rest) $1)
   ;; row-term => nl
   (lambda ($1 . $rest) $1)
   ;; matrix-row => expr
   (lambda ($1 . $rest) (make-tl 'row $1))
   ;; matrix-row => matrix-row "," expr
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; term-list => term
   (lambda ($1 . $rest) $1)
   ;; term-list => term-list term
   (lambda ($2 $1 . $rest) $1)
   ;; term => nl
   (lambda ($1 . $rest) $1)
   ;; term => ";"
   (lambda ($1 . $rest) $1)
   ;; term => ","
   (lambda ($1 . $rest) $1)
   ;; lone-comment-list => lone-comment nl
   (lambda ($2 $1 . $rest) (make-tl 'comm-list $1))
   ;; lone-comment-list => lone-comment-list lone-comment nl
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; ident => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; fixed => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; float => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; number => fixed
   (lambda ($1 . $rest) $1)
   ;; number => float
   (lambda ($1 . $rest) $1)
   ;; string => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ;; lone-comment => '$lone-comm
   (lambda ($1 . $rest) `(comm ,$1))
   ;; nl => "\n"
   (lambda ($1 . $rest) $1)
   ))

;;; end tables
