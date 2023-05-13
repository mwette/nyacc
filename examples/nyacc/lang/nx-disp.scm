;;; nyacc/lang/nx-disp.scm - display routines

;; Copyright (C) 2019 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Notes:

;; format syntax: %[flags][width][.precision][length]type
;; flags: #\- #\+ #\space #\0 #\#
;; width: min number of chars to output
;; precision: max number of chars to output
;; length modifier: size of arg; not needed for scheme
;; type: #\d #\x #\f #\e -- add #\o: just the (display obj)

;; Architecture:
;; 1) parse format string to list of alternating
;;    *) start-end index for string
;;    *) specifiers redord

#;(define-record-type <format-spec>
  (make-format-spec flags width prec type)
  format-spec?
  (flags format-spec-flags)
  (width format-spec-width)
  (prec format-spec-prec)
  (type format-spec-type))


;; flt->fstr:
;;   flags list of chars
;;   type #\e #\f
;;   NEEDS WORK: getting negative val-wid's

;; (flt->fstr val type flags width prec) ->  "  1.234"
;; (flt->estr val type flags width prec) ->  "  1.234e0"
;; (int->str val type flags width prec) ->  "  1.234e0"
;; (int->str 23 '(#\+) 4 0 #\d) => " +23"

;;; Code:

(define-module (nyacc lang nx-disp)
  #:export (mat-disp vec-disp parse-fmt parse-fmt-str fmt->str)
  )

(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(use-modules (ice-9 pretty-print))
(define (pperr x) (pretty-print x (current-error-port) #:per-line-prefix "  "))

;; For a value tell me how many digits it will have, or equiv pow-10 exponent.
;; 35 => 2
;; 3.459393e32 => 32

;; int->str:
;;   when (memq #\- flags) left align
;;   flags list of flags: #\0 #\- #\+
;;   width - min wid or 0 for any
;;   type - #\d #\x

;; (parse-fmt ch port) => (type width prec flags) | #\%
;; maybe change to (type width prec . flags)
(define* (parse-fmt ch #:optional (port (current-input-port)))
  (define (rd-ch) (read-char port))
  (define ch0 (char->integer #\0))
  (define (ch-add ch val) (+ (- (char->integer ch) ch0) (* 10 val)))
  
  (let loop ((fl '()) (wd #f) (pc #f) (ty #f) (st 0) (ch ch))
    ;; flags width precision type state char
    (case st
      ((0) ;; looking for %
       (case ch
         ((#\%) (loop fl wd pc ty 1 (rd-ch)))
         (else #f)))
      ((1) ;; read flags
       (case ch
         ((#\- #\+ #\0 #\space) (loop (cons ch fl) wd pc ty st (rd-ch)))
         (else (loop fl wd pc ty 2 ch))))
      ((2) ;; read width
       (cond
        ((char-numeric? ch) (loop fl (ch-add ch 0) pc ty 21 (rd-ch)))
        ((char=? #\. ch) (loop fl wd pc ty 3 (rd-ch)))
        (else (loop fl wd pc ty 4 ch))))
      ((21)
       (cond
        ((char-numeric? ch) (loop fl (ch-add ch wd) pc ty st (rd-ch)))
        ((char=? #\. ch) (loop fl wd pc ty 3 (rd-ch)))
        (else (loop fl wd pc ty 4 ch))))
      ((3) ;; read precision
       (cond
        ((char-numeric? ch) (loop fl wd (ch-add ch 0) ty 31 (rd-ch)))
        (else (loop fl wd pc ty 4 ch))))
      ((31)
       (cond
        ((char-numeric? ch) (loop fl wd (ch-add ch pc) ty st (rd-ch)))
        (else (loop fl wd pc ty 4 ch))))
      ((4) ;; read type
       (case ch
         ((#\%) #\%)
         ((#\d #\i #\u) (loop fl wd pc #\d 5 ch))
         ((#\x #\X) (loop fl wd pc #\x 5 ch))
         ((#\e #\E #\g #\G) (loop fl wd pc #\e 5 ch))
         ((#\f #\F) (loop fl wd pc #\f 5 ch))
         ;;((#\s)
         ;;((#\o)
         ))
      ((5) (list ty wd pc fl)))))

;; exp for val = [1.0,10.0)x10^exp

(define (exp-of-10 val)
  (if (eqv? val 0.0) 0
      (inexact->exact (floor (log10 (abs val))))))

(define (exp-of-16 val)
  (define (log16 x) (/ (log x) (log 16)))
  (if (eqv? val 0.0) 0
      (inexact->exact (floor (log16 (abs val))))))

(define (exp-of-8 val)
  (define (log8 x) (/ (log x) (log 8)))
  (if (eqv? val 0.0) 0
      (inexact->exact (floor (log8 (abs val))))))

(define (exp-of-base val base)
  (case base
    ((10 #\d) (exp-of-10 val))
    ((16 #\x) (exp-of-16 val))
    ((8 #\o) (exp-of-8 val))))

;; (int->numch 3) => #\3
(define int->numch
  (let ((cv (vector #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                    #\A #\B #\C #\D #\E #\F)))
    (lambda (iv) (vector-ref cv iv))))

(define digit->char
  (let ((lc #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))
        (uc #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F)))
    (lambda* (v #:optional upper)
      (vector-ref (if upper uc lc) v))))

;; (int->str 23 #\d '(#\+) 4 0) => " +23"
(define (int->str val type width prec flags)
  (let* ((base (case type ((#\d) 10) ((#\x) 16) ((#\o) 8)
                     (else (error "bad type"))))
         (left (memq #\- flags))
         (sign (cond ((negative? val) #\-)
                     ((memq #\+ flags) #\+)
                     (else #f)))
         (val (abs val))
         (exb (exp-of-base (* 1.0 val) base))
         (valw (1+ (if sign (1+ exb) exb)))
         (wid (cond ((not width) valw)
                    ((zero? width) valw)
                    ((> valw width) valw)
                    (else width)))
         (npad (let ((n (if (> valw wid) 0 (- wid valw)))) (if left (- n) n)))
         (pad (cond ((zero? npad) #f) (left #\space)
                    ((memq #\0 flags) #\0) (else #\space)))
         (sign-first (and (positive? npad) (char=? pad #\0)))
         (upper (char-upper-case? type)))
    (with-output-to-string
      (lambda ()
        (if (and sign sign-first) (write-char sign))
        (let loop ((n npad)) (when (> n 0) (write-char pad) (loop (1- n))))
        (if (and sign (not sign-first)) (write-char sign))
        (let loop ((v val) (m (expt base exb)))
          (when (positive? v)
            (write-char (digit->char (quotient v m) type))
            (loop (remainder v m) (quotient m base))))
        (let loop ((n npad))
          (when (< n 0) (write-char pad) (loop (1+ n))))))))

(define-public (test-int->str)
  (define (doit fmt val)
    (sferr "~S ~S => ~S\n" fmt val (apply int->str val (parse-fmt-str fmt))))
  (doit "%d" 1)
  (doit "%-04x" 1)
  )

;; (split-float 2.3492e-23) => (2.3492 . -23)
(define (split-float val)
  (if (eqv? val 0.0)
      (cons 0.0 0)
      (let ((sign (if (negative? val) -1.0 +1.0))
            (val (abs val))
            (v_expt (exp-of-10 val)))
        (cons (* sign val (expt 10 (- v_expt))) v_expt))))
(export split-float)

(define vch
  (let ((dv #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
    (lambda (v) (vector-ref dv (inexact->exact (floor v))))))

(define (vnx v) (* 10 (- v (inexact->exact (floor v)))))
  
;; (flt->fstr 3.456 %f '() 8 4) => "  3.4560"
;; maybe change to . flags)
(define (flt->fstr val type width prec flags)
  (let* ((base 10)
         (left (memq #\- flags))
         (sign (cond ((negative? val) #\-) ((memq #\+ flags) #\+) (else #f)))
         (val (abs val))
         (v:me (split-float val)) (val-m (car v:me)) (val-e (cdr v:me))
         (valw (+ (if sign 1 0)
                  (if (positive? prec) (+ 1 prec) 0)
                  (if (positive? val-e) val-e 0)))
         (prec (if prec prec (max (- width valw) 0)))
         (wid (cond ((zero? width) valw) ((> valw width) valw) (else width)))
         (npad (let ((n (if (> valw width) 0 (- wid valw)))) (if left (- n) n)))
         (pad (cond ((zero? npad) #f) (left #\space)
                    ((memq #\0 flags) #\0) (else #\space)))
         (sign-first (and (positive? npad) (char=? pad #\0)))
         (upper (char-upper-case? type))
         )
    (with-output-to-string
      (lambda ()
        (if (and sign sign-first) (write-char sign))
        (let loop ((n npad)) (when (> n 0) (write-char pad) (loop (1- n))))
        (if (and sign (not sign-first)) (write-char sign))
        (if (negative? val-e)
            (write-char #\0)
            (let loop ((v val-m))
              (when (> v 1.0)
                (write-char (digit->char (remainder v base) type))
                )))
        (let loop ((n npad)) (when (< n 0) (write-char pad) (loop (1+ n))))))
    ;;(if (negative? wid valw) (make-string wid #\#))
    #|
    (let loop ((chl chl) (nl wid) (v val-m) (e (1+ val-e)))
      (cond
       ((negative? e) (loop (cons #\0 chl) (1- nl) v (1+ e)))
       ((= 1 e) (loop (cons* #\. (vch v) chl) (- nl 2) (vnx v) (1- e)))
       ((positive? e) (loop (cons (vch v) chl) (1- nl) (vnx v) (1- e)))
       ((positive? nl) (loop (cons (vch v) chl) (1- nl) (vnx v) e))
       (else (reverse-list->string chl))))
    |#
    ))

;; (flt->estr -12.34e-22 '() 12 5) => "-1.23400e-21"
(define (flt->estr val type width prec flags)
  (let* ((base 10)
         (left (memq #\- flags))
         (pad (if (memq #\0 flags) #\0 #\space))
         (val-pair (split-float val))
         (val-m (car val-pair))
         (val-e (cdr val-pair))
         (e-str (int->str val-e #\d '(#\+) 0 0))
         (m-wid (- width 1 (string-length e-str)))
         (m-str (flt->fstr val-m #\f flags m-wid prec)))
    ;;(sferr "val-m=~S\n" val-m)
    ;;(sferr "val-e=~S\n" val-e)
    ;;(sferr "e-str=~S\n" e-str)
    (string-append m-str "e" e-str)))

(export int->str)
(export flt->fstr)
(export flt->estr)

(define (apply-fmt fmt val)
  ;; fmt = (ty fl wd pc)
  (case (car fmt) ;; ty
    ((#\d) (unless (integer? val) (error "yuck")) (apply int->str val fmt))
    ((#\x) (unless (integer? val) (error "yuck")) (apply int->str val fmt))
    ((#\e) (unless (inexact? val) (error "yuck")) (apply flt->estr val fmt))
    ((#\f) (unless (inexact? val) (error "yuck")) (apply flt->fstr val fmt))
    ;;((#\g) (unless (inexact? val) (error "yuck")) (apply flt->gstr val fmt))
    ))

;; === sprintf 

(define rls reverse-list->string)

(define (nx-format fmt . args)
  (with-input-from-string fmt
    (lambda ()
      (let loop ((stl '()) (chl '()) (ch (read-char)) (args args))
        ;;(sferr "ch=~S\n" ch)
        (cond
         ((eof-object? ch)
          (apply string-append (reverse (cons (rls chl) stl))))
         ((char=? ch #\%)
          (let ((fmt (parse-fmt ch)))
            (cond
             ((char? fmt) (loop stl (cons #\% chl) (read-char) args))
             ((null? args) (error "not enough args"))
             (else (loop (cons* (apply-fmt fmt (car args)) (rls chl) stl)
                         '() (read-char) (cdr args))))))
         (else
          (loop stl (cons ch chl) (read-char) args)))))))

;; === matrices and vectors ========

;; (parse-fmt-str spec-str) => (flags width prec type) | #\%
;; spec-str "%-12.5e" ...
(define (parse-fmt-str str)
  (let ((port (open-input-string str)))
    (parse-fmt (read-char port) port)))
(export parse-fmt-str)

(define (flt->str val . fmt)
  (case (car fmt)
    ((#\i) (apply int->str val fmt))
    ((#\f) (apply flt->fstr val fmt))
    ((#\e) (apply flt->estr val fmt))))
  
(define (mat-disp/strict array port format)
  (let* ((type (array-type array))
         (dims (array-dimensions array))
         (dimz (map (lambda (i) (min i 8)) dims))
         (spec (parse-fmt-str format)))
    (do ((i 0 (1+ i))) ((= i (list-ref dimz 0)))
      (do ((j 0 (1+ j))) ((= j (list-ref dimz 1)))
        (display " " port)
        (display (apply flt->str (array-ref array i j) spec) port))
      (newline port))))

(define* (mat-disp array #:optional (port #t) #:key (format "%12.5e"))
  (cond
   ((eq? #f port)
    (let ((strp (open-output-string)))
      (mat-disp/strict array strp format)
      (get-output-string strp)))
   ((eq? #t port)
    (mat-disp/strict array (current-output-port) format))
   (else
    (mat-disp/strict array port format))))

(define (vec-disp/strict array port format)
  (let* ((type (array-type array))
         (dims (array-dimensions array))
         (dimz (map (lambda (i) (min i 8)) dims))
         (spec (parse-fmt-str format)))
    (do ((i 0 (1+ i))) ((= i (list-ref dimz 0)))
      (display " " port)
      (display (apply flt->str (array-ref array i) spec) port)
      (newline port))))

(define* (vec-disp array #:optional (port #t) #:key (format "%12.5e"))
  (cond
   ((eq? #f port)
    (let ((strp (open-output-string)))
      (vec-disp/strict array strp format)
      (get-output-string strp)))
   ((eq? #t port)
    (vec-disp/strict array (current-output-port) format))
   (else
    (vec-disp/strict array port format))))

;; === deprecated


(define (disp/fmt val type flags width prec)
  (case type
    ((#\e) (flt->estr val type flags width prec))
    ((#\f) (flt->fstr val type flags width prec))
    ((#\d) (int->str val type flags width prec))
    ;;((#\s) 
    (else (error "fmat"))))
  
(define (numstr->string val)
  (if (string? val) val (number->string val)))
(define (escape ch)
  (case ch
    ((#\\) #\\)
    ((#\n) #\newline)
    (else ch)))

;; --- last line ---
