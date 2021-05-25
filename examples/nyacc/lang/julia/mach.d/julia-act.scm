;; julia-act.scm

;; Copyright 2018,2020,2021 Matthew R. Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING included with the this distribution.

(define julia-act-v
  (vector
   ;; $start => translation-unit
   (lambda ($1 . $rest) $1)
   ;; translation-unit => ident "=" expr term
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; translation-unit => function-defn
   (lambda ($1 . $rest) $1)
   ;; function-defn => "function" ident "(" arg-list ")" term stmt-list ter...
   (lambda ($10 $9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(function-defn ,$2 ,$4 ,$7))
   ;; function-defn => "function" ident "(" arg-list ")" "=" expr term
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(function-expr ,$1 ,$4 $7))
   ;; arg-list => arg-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; arg-list-1 => ident
   (lambda ($1 . $rest) (make-tl 'arg-list $1))
   ;; arg-list-1 => arg-list-1 "," ident
   (lambda ($3 $2 $1 . $rest) (tl->append $1 $3))
   ;; tuple-expr => "(" ")"
   (lambda ($2 $1 . $rest) '(tuple-expr))
   ;; tuple-expr => "(" expr-list-1 ")"
   (lambda ($3 $2 $1 . $rest)
     `(tuple-expr ,@(cdr (tl->list $2))))
   ;; expr-list => expr-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; expr-list-1 => expr
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; expr-list-1 => expr-list-1 "," opt-nl expr
   (lambda ($4 $3 $2 $1 . $rest) (tl-append $1 $2))
   ;; stmt-list => stmt-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; stmt-list-1 => stmt-expr
   (lambda ($1 . $rest) $1)
   ;; stmt-list-1 => stmt-list-1 stmt-expr
   (lambda ($2 $1 . $rest) $1)
   ;; stmt-expr => if-expr term
   (lambda ($2 $1 . $rest) $1)
   ;; stmt-expr => return-expr term
   (lambda ($2 $1 . $rest) $1)
   ;; stmt-expr => assn-expr term
   (lambda ($2 $1 . $rest) $1)
   ;; return-expr => "return" expr
   (lambda ($2 $1 . $rest) $1)
   ;; if-expr => "if" expr term stmt-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; if-expr => "if" expr term stmt-list "else" stmt-list "end"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; if-expr => "if" expr term stmt-list elseif-list "end"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; if-expr => "if" expr term stmt-list elseif-list "else" stmt-list "end"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; elseif-list => elseif-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; elseif-list-1 => "elseif" expr term stmt-list
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; elseif-list-1 => elseif-list-1 "elseif" expr term stmt-list
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; expr => assn-expr
   (lambda ($1 . $rest) $1)
   ;; assn-expr => colon-expr
   (lambda ($1 . $rest) $1)
   ;; assn-expr => arg-list "=" colon-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; colon-expr => or-expr ":" or-expr
   (lambda ($3 $2 $1 . $rest) `(colon-expr ,$1 ,$3))
   ;; colon-expr => or-expr ":" or-expr ":" or-expr
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 ,$5))
   ;; colon-expr => or-expr ":" "end"
   (lambda ($3 $2 $1 . $rest)
     `(colon-expr ,$1 (end)))
   ;; colon-expr => or-expr ":" or-expr ":" "end"
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
   ;; mul-expr => mul-expr "//" unary-expr
   (lambda ($3 $2 $1 . $rest) `(idiv ,$1 ,$3))
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
   ;; primary-expr => compound-expr
   (lambda ($1 . $rest) $1)
   ;; compound-expr => "begin" stmt-list "end"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; compound-expr => "(" stmt-list ")"
   (lambda ($3 $2 $1 . $rest) $1)
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
   ;; term => ";"
   (lambda ($1 . $rest) $1)
   ;; term => "\n"
   (lambda ($1 . $rest) $1)
   ;; opt-nl => 
   (lambda $rest (list))
   ;; opt-nl => nl
   (lambda ($1 . $rest) $1)
   ;; nl => "\n"
   (lambda ($1 . $rest) $1)
   ;; ident => '$ident
   (lambda ($1 . $rest) $1)
   ;; number => integer
   (lambda ($1 . $rest) $1)
   ;; number => float
   (lambda ($1 . $rest) $1)
   ;; integer => '$fixed
   (lambda ($1 . $rest) $1)
   ;; float => '$float
   (lambda ($1 . $rest) $1)
   ;; string => '$string
   (lambda ($1 . $rest) $1)
   ))

;;; end tables
