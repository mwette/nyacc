
(define (symlist->phlkup symlist)
  (define (nextn n) (+ n (/ (if (odd? n) (1+ n) n) 2)))
  (let loop ((kl '()) (n (length symlist)) (mx -1) (mn #xffffffff) (sl symlist))
    (if (pair? sl)
        (let ((hv (hash (car sl) n)))
          (if (memq hv kl)
              (loop '() (nextn n) -1 #x7fffffff symlist)
              (loop (cons hv kl) n (max mx hv) (min mn hv) (cdr sl))))
        (let* ((sz (- mx mn -1))
               (hv (make-vector sz #f)))
          (for-each (lambda (k s) (vector-set! hv (- k mn) s)) kl symlist)
          (list sz mn)
          ))))


;; generate (alist) of count symbols with min max symbol length
;; ascii, a-z 0-9 _
(define symchars "abcdefghijklmnopqrstuvwxyz_0123456789")
(define nsymchar (string-length symchars))
(define (randsym mxln mnln)
  (let loop ((chl '()) (ln (+ mnln (random (- mxln mnln -1)))))
    (cond
     ((zero? ln) (string->symbol (reverse-list->string chl)))
     ((null? chl) (loop (cons (string-ref symchars (random (- nsymchar 10))) chl) (1- ln)))
     (else (loop (cons (string-ref symchars (random nsymchar)) chl) (1- ln))))))
    
(define (gen-syms count mxln mnln)
  (let loop ((res '()) (cnt count))
    (if (zero? cnt) res (loop (cons (randsym mxln mnln) res) (1- cnt)))))

;; for 6 8 4 get up to 143 w/o much


