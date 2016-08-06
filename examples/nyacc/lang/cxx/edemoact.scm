;; edemoact.scm

(define act-v
  (vector
   ;; $start => stmts
   (lambda ($1 . $rest) $1)
   ;; stmts => 
   (lambda $rest (list))
   ;; stmts => stmts ";"
   (lambda ($2 $1 . $rest) $1)
   ;; stmts => stmts "1" ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; stmts => stmts '$error ";"
   (lambda ($3 $2 $1 . $rest)
     (display "error: bad statement\n"))
   ))

;;; end tables
