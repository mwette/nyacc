;; lang/matlab/actions.scm

;; Copyright (C) 2015 Matthew R. Wette
;; 
;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;; or any later version published by the Free Software Foundation.  See the
;; file COPYING included with the this distribution.

(define act-v
  (vector
   ;; $start => mfile
   (lambda ($1 . $rest) $1)
   ;; mfile => statement-list
   (lambda ($1 . $rest)
     `(script-file ,(tl->list $1)))
   ;; mfile => function-file
   (lambda ($1 . $rest) (tl->list $1))
   ;; function-file => function-defn
   (lambda ($1 . $rest) (make-tl 'function-file $1))
   ;; function-file => function-file function-defn
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; function-defn => function-decl non-comment-statement statement-list o...
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
   ;; nl-list => #\newline
   (lambda ($1 . $rest) $1)
   ;; nl-list => nl-list #\newline
   (lambda ($2 $1 . $rest) $1)
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
   ;; statement-list => statement
   (lambda ($1 . $rest)
     (if $1
       (make-tl 'stmt-list $1)
       (make-tl 'stmt-list)))
   ;; statement-list => statement-list statement
   (lambda ($2 $1 . $rest)
     (if $2 (tl-append $1 $2) $1))
   ;; statement => lone-comment
   (lambda ($1 . $rest) $1)
   ;; statement => non-comment-statement
   (lambda ($1 . $rest) $1)
   ;; non-comment-statement => term
   (lambda ($1 . $rest) #f)
   ;; non-comment-statement => ident "(" expr-list ")" term
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(call ,$1 ,(tl->list $3)))
   ;; non-comment-statement => lval-expr "=" expr term
   (lambda ($4 $3 $2 $1 . $rest) `(assn ,$1 ,$3))
   ;; non-comment-statement => "[" lval-expr-list "]" "=" ident "(" ")" term
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(multi-assign ,(tl->list $2) ,$5 (expr-list)))
   ;; non-comment-statement => "[" lval-expr-list "]" "=" ident "(" expr-li...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(multi-assign ,(tl->list $2) ,$5 ,(tl->list $7)))
   ;; non-comment-statement => "for" ident "=" expr term statement-list "en...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for ,$2 ,$4 ,(tl->list $6)))
   ;; non-comment-statement => "while" expr term statement-list "end" term
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(while ,$2 ,(tl->list $4)))
   ;; non-comment-statement => "if" expr term statement-list elseif-list "e...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2
        ,(tl->list $4)
        ,@(cdr (tl->list $5))
        (else ,(tl->list $7))))
   ;; non-comment-statement => "if" expr term statement-list "else" stateme...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,(tl->list $4) (else ,(tl->list $6))))
   ;; non-comment-statement => "if" expr term statement-list "end" term
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,(tl->list $4)))
   ;; non-comment-statement => "switch" expr term case-list "otherwise" ter...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(switch
        ,$2
        ,@(tl->list $4)
        (otherwise ,(tl->list $7))))
   ;; non-comment-statement => "switch" expr term case-list "end" term
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(switch ,$2 ,@(tl->list $4)))
   ;; non-comment-statement => "return" term
   (lambda ($2 $1 . $rest) '(return))
   ;; non-comment-statement => command ident-nc-list term
   (lambda ($3 $2 $1 . $rest)
     `(command ,$1 ,(tl->list $2)))
   ;; lval-expr-list => lval-expr
   (lambda ($1 . $rest)
     (make-tl 'lval-expr-list $1))
   ;; lval-expr-list => lval-expr-list "," lval-expr
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; lval-expr => ident
   (lambda ($1 . $rest) $1)
   ;; lval-expr => ident "(" expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(array-ref ,$1 ,(tl->list $3)))
   ;; command => "global"
   (lambda ($1 . $rest) '(ident "global"))
   ;; command => "clear"
   (lambda ($1 . $rest) '(ident "clear"))
   ;; ident-nc-list => ident
   (lambda ($1 . $rest) (make-tl 'ident-list $1))
   ;; ident-nc-list => ident-nc-list ident
   (lambda ($2 $1 . $rest) (tl-append $1 $3))
   ;; elseif-list => "elseif" expr term statement-list
   (lambda ($4 $3 $2 $1 . $rest)
     (make-tl
       'elseif-list
       `(elseif ,$2 ,(tl->list $4))))
   ;; elseif-list => elseif-list "elseif" expr term statement-list
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(elseif ,$3 ,(tl->list $5))))
   ;; case-list => 
   (lambda $rest (make-tl))
   ;; case-list => case-list "case" expr term statement-list
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl->append $1 `(case ,$3 ,(tl->list $5))))
   ;; expr-list => expr
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; expr-list => ":"
   (lambda ($1 . $rest)
     (make-tl 'expr-list '(colon-expr)))
   ;; expr-list => expr-list "," expr
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; expr-list => expr-list "," ":"
   (lambda ($3 $2 $1 . $rest)
     (tl-append $1 '(colon-expr)))
   ;; expr => or-expr
   (lambda ($1 . $rest) $1)
   ;; expr => expr ":" or-expr
   (lambda ($3 $2 $1 . $rest) `(colon-expr ,$1 ,$3))
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
   (lambda ($3 $2 $1 . $rest) `(equal ,$1 ,$3))
   ;; equality-expr => equality-expr "~=" rel-expr
   (lambda ($3 $2 $1 . $rest) `(noteq ,$1 ,$3))
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
   ;; mul-expr => unary-expr
   (lambda ($1 . $rest) $1)
   ;; mul-expr => mul-expr "*" unary-expr
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; mul-expr => mul-expr "/" unary-expr
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; mul-expr => mul-expr "\" unary-expr
   (lambda ($3 $2 $1 . $rest) `(ldiv ,$1 ,$3))
   ;; mul-expr => mul-expr "^" unary-expr
   (lambda ($3 $2 $1 . $rest) `(pow ,$1 ,$3))
   ;; mul-expr => mul-expr ".*" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-mul ,$1 ,$3))
   ;; mul-expr => mul-expr "./" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-div ,$1 ,$3))
   ;; mul-expr => mul-expr ".\" unary-expr
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
   ;; postfix-expr => ident "(" expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(aref-or-call ,$1 ,(tl->list $3)))
   ;; postfix-expr => postfix-expr "'"
   (lambda ($2 $1 . $rest) `(xpose ,$1))
   ;; postfix-expr => postfix-expr ".'"
   (lambda ($2 $1 . $rest) `(conj-xpose ,$1))
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
   ;; matrix-row-list => matrix-row
   (lambda ($1 . $rest)
     (make-tl 'matrix (tl->list $1)))
   ;; matrix-row-list => matrix-row-list row-term matrix-row
   (lambda ($3 $2 $1 . $rest)
     (tl-append $1 (tl->list $3)))
   ;; row-term => ";"
   (lambda ($1 . $rest) $1)
   ;; row-term => #\newline
   (lambda ($1 . $rest) $1)
   ;; matrix-row => expr
   (lambda ($1 . $rest) (make-tl 'row $1))
   ;; matrix-row => matrix-row "," expr
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; term-list => term
   (lambda ($1 . $rest) $1)
   ;; term-list => term-list term
   (lambda ($2 $1 . $rest) $1)
   ;; lone-comment-list => lone-comment #\newline
   (lambda ($2 $1 . $rest)
     (make-tl 'comment-list $1))
   ;; lone-comment-list => lone-comment-list lone-comment #\newline
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; term => #\newline
   (lambda ($1 . $rest) $1)
   ;; term => ";"
   (lambda ($1 . $rest) $1)
   ;; term => ","
   (lambda ($1 . $rest) $1)
   ;; ident => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; number => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; number => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; string => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ;; lone-comment => '$lone-comm
   (lambda ($1 . $rest) `(comm ,$1))
   ))

;;; end tables
