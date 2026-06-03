;; calc-stmt-act.scm

(define calc-stmt-act-v
  (vector
   ;; 0. $start => stmt
   (lambda ($1 . $rest) $1)
   ;; 1. prog => stmt-list
   (lambda ($1 . $rest) (tl->list $1))
   ;; 2. stmt-list => stmt
   (lambda ($1 . $rest) (make-tl 'stmt-list $1))
   ;; 3. stmt-list => stmt-list stmt
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; 4. stmt => "\n"
   (lambda ($1 . $rest) `(empty-stmt))
   ;; 5. stmt => expr "\n"
   (lambda ($2 $1 . $rest) `(expr-stmt ,$1))
   ;; 6. stmt => assn "\n"
   (lambda ($2 $1 . $rest) `(assn-stmt ,$1))
   ;; 7. expr => expr "+" expr
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; 8. expr => expr "-" expr
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; 9. expr => expr "*" expr
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; 10. expr => expr "/" expr
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; 11. expr => '$fixed
   (lambda ($1 . $rest) `(num ,$1))
   ;; 12. expr => '$float
   (lambda ($1 . $rest) `(num ,$1))
   ;; 13. expr => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; 14. expr => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; 15. assn => '$ident "=" expr
   (lambda ($3 $2 $1 . $rest) `(assn (ident ,$1) ,$3))
   ))

;; --- last line ---
