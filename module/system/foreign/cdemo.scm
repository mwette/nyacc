(use-modules (ice-9 pretty-print))
(define (pp exp) (pretty-print exp (current-output-port)))
(define (sf fmt . args) (apply simple-format #t fmt args))

(use-modules (system foreign arch-info))
(use-modules (system foreign cdata))

;;(sferr "arch=~s\n" (*arch*))
;;(sferr "sizeof void* = ~s\n" (sizeof-basetype "void*"))
;;(sferr "(cbase 'double) => \n") (pperr (cbase 'double))

(define (bounding-ffi t1)
  (let* ((cs (ctype-info t1))
        )
    (let loop ((ffis '()) (size 0) (flds (cstruct-fields cs)))
      (if (null? flds) (reverse ffis)
          (begin
            (sf "\noffs=~s\n" (cfield-offset (car flds)))
            (pp (car flds))
            (cond
             ((eq? 'bitfield (ctype-kind (cfield-type (car flds))))
              (sf "bf type=~s shift=~s width=~s\n"
                  (cbitfield-mtype (ctype-info (cfield-type (car flds))))
                  (cbitfield-shift (ctype-info (cfield-type (car flds))))
                  (cbitfield-width (ctype-info (cfield-type (car flds)))))
              (loop ffis (cfield-offset (car flds)) (cdr flds)))
             (else
              (loop ffis (cfield-offset (car flds)) (cdr flds)))))))))

(when #t
  (let ((t1 (cstruct
             (list `(x double)
                   `(l char)
                   `(m short)
                   `(n int 4)
                   `(o short 5)
                   `(p char 6)
                   `(q int)
                   ))))
    (pp t1)
    (pretty-print-ctype t1)
    (bounding-ffi t1)
    ))



;; --- last line ---
