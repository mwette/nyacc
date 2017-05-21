;; calc1-act.scm

(define act-v
  (vector
   ;; $start => expr
   (lambda ($1 . $rest) $1)
   ;; expr => expr "+" expr
   (lambda ($3 $2 $1 . $rest) (+ $1 $3))
   ;; expr => expr "-" expr
   (lambda ($3 $2 $1 . $rest) (- $1 $3))
   ;; expr => expr "*" expr
   (lambda ($3 $2 $1 . $rest) (* $1 $3))
   ;; expr => expr "/" expr
   (lambda ($3 $2 $1 . $rest) (/ $1 $3))
   ;; expr => "*" '$error
   (lambda ($2 $1 . $rest)
     (display "syntax error\n"))
   ;; expr => '$fixed
   (lambda ($1 . $rest) (string->number $1))
   ;; expr => '$float
   (lambda ($1 . $rest) (string->number $1))
   ;; expr => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ))

;;; end tables
