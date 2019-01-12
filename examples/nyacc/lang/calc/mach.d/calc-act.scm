;; mach.d/calc-act.scm

(define calc-act-v
  (vector
   ;; $start => prog
   (lambda ($1 . $rest) $1)
   ;; prog => stmt-list
   (lambda ($1 . $rest) (tl->list $1))
   ;; stmt-list => stmt
   (lambda ($1 . $rest) (make-tl 'stmt-list $1))
   ;; stmt-list => stmt-list "\n" stmt
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; stmt => expr
   (lambda ($1 . $rest) $1)
   ;; expr => 
   (lambda $rest "")
   ;; expr => expr "+" expr
   (lambda ($3 $2 $1 . $rest) (+ $1 $3))
   ;; expr => expr "-" expr
   (lambda ($3 $2 $1 . $rest) (- $1 $3))
   ;; expr => expr "*" expr
   (lambda ($3 $2 $1 . $rest) (* $1 $3))
   ;; expr => expr "/" expr
   (lambda ($3 $2 $1 . $rest) (/ $1 $3))
   ;; expr => '$fixed
   (lambda ($1 . $rest) (string->number $1))
   ;; expr => '$float
   (lambda ($1 . $rest) (string->number $1))
   ;; expr => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ))

;;; end tables
