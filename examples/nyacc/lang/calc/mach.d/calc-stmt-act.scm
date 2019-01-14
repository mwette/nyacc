;; mach.d/calc-stmt-act.scm

(define calc-stmt-act-v
  (vector
   ;; $start => stmt
   (lambda ($1 . $rest) $1)
   ;; prog => stmt-list
   (lambda ($1 . $rest) (tl->list $1))
   ;; stmt-list => stmt
   (lambda ($1 . $rest) (make-tl 'stmt-list $1))
   ;; stmt-list => stmt-list stmt
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; stmt => "\n"
   (lambda ($1 . $rest) `(empty-stmt))
   ;; stmt => expr "\n"
   (lambda ($2 $1 . $rest) `(expr-stmt ,$1))
   ;; stmt => assn "\n"
   (lambda ($2 $1 . $rest) `(assn-stmt ,$1))
   ;; expr => expr "+" expr
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; expr => expr "-" expr
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; expr => expr "*" expr
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; expr => expr "/" expr
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; expr => '$fixed
   (lambda ($1 . $rest) `(num ,$1))
   ;; expr => '$float
   (lambda ($1 . $rest) `(num ,$1))
   ;; expr => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; expr => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; assn => '$ident "=" expr
   (lambda ($3 $2 $1 . $rest)
     `(assn (ident ,$1) ,$3))
   ))

;;; end tables
