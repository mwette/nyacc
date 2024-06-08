(use-modules (ice-9 pretty-print))
(define (pperr exp) (pretty-print exp (current-error-port)))
(define (sferr fmt . args) (apply simple-format #t fmt args))

(use-modules (nyacc lang arch-info))
(use-modules (nyacc lang cdata))

;;(sferr "arch=~s\n" (*arch*))
;;(sferr "sizeof void* = ~s\n" (sizeof-basetype "void*"))
;;(sferr "(cbase 'double) => \n") (pperr (cbase 'double))

(when #f
  (let ((t1 (cstruct
             (list `(x ,(cbase 'double))
                   `(l ,(cbase 'char) 3)
                   `(m ,(cbase 'short) 5)
                   `(#f (cbase 'short) 0)
                   `(n ,(cbase 'int) 4)
                   `(p ,(cbase 'int))
                   ))))
    (pperr t1)))


;; Round number of bits to next alignment size.
(define (roundup-bits a s)
  (* a (quotient (+ s (1- a)) a)))

;; Given bitfield width and alignment, update running struct size, a rational
(define (incr-bit-size w a s)
  (let* ((a (* 8 a)) (s (* 8 s)) (ru (roundup-bits a s)))
    (/ (cond ((zero? w) ru) ((> (+ s w) ru) (+ w ru)) (else (+ w s))) 8)))

;; Given bitfield width and alignment, compute byte offset for this field.
(define (bfld-offset w a s)
  (let* ((a (* 8 a)) (s (* 8 s)) (u (roundup-bits a s)))
    (/ (cond ((> (+ s w) u) u) (else (- u a))) 8)))


(define (show-ibs bs a rs)
  (sferr "(incr-bit-size ~s ~s ~s) => ~s\n" bs a rs (incr-bit-size bs a rs)))

(define (sao t) (case t ((u8 s8) 1) ((u16 s16) 2) ((u32 s32) 4) ((u64 s64) 8)))

(define flds '((a u16 7) (b u8 3) (c s8 2)))
(set! flds '((a u16 7) (b u8 3) (c s8 2)))

;; ci == containing integer
(define (process flds)
  (let loop ((cs 0) (ca 0) (fl flds))   ; current size, align; fields
    (when (pair? fl)
      (let* ((f (car fl))
             (fn (list-ref f 0))        ; name
             (ft (list-ref f 1))        ; type
             (bw (list-ref f 2))        ; width
             (fs (sao ft))              ; size
             (fa (sao ft))              ; alignment
             (sx (cbase-signed? ft))    ; sign-extend?
             (ns (incr-bit-size bw fa cs)) ; next size
             (so (bfld-offset bw fa cs))
             (bo (- (* 8 ns) (* 8 so) bw)) ; bit offset in ci
             )
        ;; (if (xor fn (zero? bw)) error) OR (and fn (zero? bw)) => error
        (show-ibs bw fa cs)
        (sferr "  ns*8=~s  so*8=~s bw=~s\n" (* 8 ns) (* 8 so) bw)
        (sferr "  ci=~s  bo=~s  bw=~s  sx=~s\n" so bo bw sx)
        (loop ns (max fa ca) (cdr fl))))))
