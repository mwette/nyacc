;;

(define-record-type <fh-bkend>
  (make-fh-bkend foo bar)
  (foo bkend-foo)
  (bar bkend-foo))

(define guile-bkend
  (make-fh-bkend
    (lambda (be x y)
      (+ x y))
    (lambda (be a b)
      (* a b))
    ))
