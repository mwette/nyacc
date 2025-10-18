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
   ;; translation-unit => function-defn
   (lambda ($1 . $rest) $1)
   ;; translation-unit => stmt-expr
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
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; lval-list => expr-list
   (lambda ($1 . $rest) $1)
   ;; expr-list => expr-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; expr-list-1 => expr-no-assn
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; expr-list-1 => expr-list-1 "," expr-no-assn
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
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
   ;; stmt-expr => lval-list "=" expr-no-assn
   (lambda ($3 $2 $1 . $rest) $1)
   ;; stmt-expr => assn-expr
   (lambda ($1 . $rest) $1)
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
   ;; expr-no-assn => keyval-expr
   (lambda ($1 . $rest) $1)
   ;; assn-expr => keyval-expr
   (lambda ($1 . $rest) $1)
   ;; assn-expr => keyval-expr "=" assn-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; keyval-expr => cond-expr
   (lambda ($1 . $rest) $1)
   ;; cond-expr => logor-expr
   (lambda ($1 . $rest) $1)
   ;; logor-expr => logand-expr
   (lambda ($1 . $rest) $1)
   ;; logor-expr => logand-expr "||" logor-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; logand-expr => rel-expr
   (lambda ($1 . $rest) $1)
   ;; logand-expr => rel-expr "&&" logand-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; rel-expr => ??1-expr
   (lambda ($1 . $rest) $1)
   ;; rel-expr => rel-expr ">" rel-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; ??1-expr => pipe-expr
   (lambda ($1 . $rest) $1)
   ;; pipe-expr => colon-expr
   (lambda ($1 . $rest) $1)
   ;; colon-expr => add-expr
   (lambda ($1 . $rest) $1)
   ;; add-expr => mul-expr
   (lambda ($1 . $rest) $1)
   ;; add-expr => add-expr "+" mul-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; mul-expr => frac-expr
   (lambda ($1 . $rest) $1)
   ;; mul-expr => mul-expr "*" frac-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; frac-expr => shift-expr
   (lambda ($1 . $rest) $1)
   ;; shift-expr => unary-expr
   (lambda ($1 . $rest) $1)
   ;; unary-expr => exp-expr
   (lambda ($1 . $rest) $1)
   ;; exp-expr => ttag-expr
   (lambda ($1 . $rest) $1)
   ;; ttag-expr => sel-expr
   (lambda ($1 . $rest) $1)
   ;; sel-expr => primary-expr
   (lambda ($1 . $rest) $1)
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
   ;; primary-expr => compound-expr
   (lambda ($1 . $rest) $1)
   ;; compound-expr => "begin" stmt-list "end"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; term => ";"
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
