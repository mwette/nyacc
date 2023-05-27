;;; nyacc/lang/nx-format.scm - display routines

;; Copyright (C) 2019,2023 Matthew Wette
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

;; format syntax: %[flags][width][.precision]conv
;; flags: #\- #\+ #\space #\0 #\#
;; width: min number of chars to output
;; precision: max number of chars to output
;; conv: #\d #\x #\f #\e -- add #\o: just the (display obj)

;; Architecture:
;; 1) parse format string to list of alternating
;;    *) string
;;    *) val-formatter

;;; Code:

(define-module (nyacc lang nx-format)
  #:export (nx-format
            nx-format1 nx-formatp
            parse-format-string apply-fmt
            mat-disp vec-disp))

(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(use-modules (ice-9 pretty-print))
(define (pperr x) (pretty-print x (current-error-port) #:per-line-prefix "  "))

(define rls reverse-list->string)

;;.@deffn {Scheme} parse-fmt ch port => (conv width prec . flags) | #\% | #f
;; @example
;;   conv : conversion specifier (e.g., #\d, #\x)
;;   width: minimum width
;;   prec : precision - ignored for ints
;;   flags: #\0 #\- #\+
;; @end example
;; @end deffn
(define (parse-fmt ch port)
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
       (cond
        ((eof-object? ch) #f)
        ;; (#\%)
        ((memq ch '(#\- #\+ #\0 #\space))
         (loop (cons ch fl) wd pc ty st (rd-ch)))
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
      ((4) ;; read conversion char
       (case ch
         ((#\%) #\%)
         ((#\d #\D #\i #\I #\u #\U) (cons* #\d wd pc fl))
         ((#\e #\E #\g #\G #\f #\F #\s #\x #\X) (cons* ch wd pc fl))
         ((#\o)  (cons* ch wd pc fl))
         (else #f))))))

;; @deffn {Scheme} parse-format-string fmt
;; This will parse a printf-like format string into a list of alternating
;; strings and formatters where a formatter is a list of
;; conv, width, prec and flags.
;; @end deffn
(define (parse-format-string fmt) ;; => list of string n format lists)
  (define (update res chl) (if (pair? chl) (cons (rls chl) res) res))
  
  (call-with-input-string fmt
    (lambda (port)
      (let loop ((res '()) (chl '()) (ch (read-char port)))
        (cond
         ((eof-object? ch) (reverse (update res chl)))
         ((char=? #\% ch)
          (let ((f (parse-fmt ch port)))
            (cond
             ((and (char? f) (char=? f #\%))
              (loop res (cons ch chl) (read-char port)))
             ((list? f)
              (loop (cons f (update res chl)) '() (read-char port)))
             (else #f))))
         (else (loop res (cons ch chl) (read-char port))))))))


;; exp for val = [1.0,10.0)x10^exp

(define (exp-of-10 val)
  (if (eqv? val 0.0) 0 (inexact->exact (floor (log10 (abs val))))))

(define (exp-of-16 val)
  (define (log16 x) (/ (log x) (log 16)))
  (if (eqv? val 0.0) 0 (inexact->exact (floor (log16 (abs val))))))

(define (exp-of-8 val)
  (define (log8 x) (/ (log x) (log 8)))
  (if (eqv? val 0.0) 0 (inexact->exact (floor (log8 (abs val))))))

(define (exp-of-base val base)
  (case base
    ((10 #\d) (exp-of-10 val))
    ((16 #\x) (exp-of-16 val))
    ((8 #\o) (exp-of-8 val))))

(define digit->char
  (let ((lc #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))
        (uc #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F)))
    (lambda* (v #:optional upper)
      (vector-ref (if upper uc lc) v))))

;; (int->str 23 #\d 4 #f #\+) => " +23"
(define (int->str val conv width prec . flags)
  (let* ((base (case conv ((#\d #\D) 10) ((#\x #\X) 16) ((#\o) 8)
                     (else (error "bad conversion char"))))
         (left (memq #\- flags))
         (sign (cond ((negative? val) #\-)
                     ((memq #\+ flags) #\+)
                     (else #f)))
         (aval (abs val))
         (exb (exp-of-base (* 1.0 aval) base))
         (raw (1+ (if sign (1+ exb) exb)))
         (wid (cond ((not width) raw)
                    ((zero? width) raw)
                    ((> raw width) raw)
                    (else width)))
         (npad (let ((n (if (> raw wid) 0 (- wid raw)))) (if left (- n) n)))
         (pad (cond ((zero? npad) #f) (left #\space)
                    ((memq #\0 flags) #\0) (else #\space)))
         (sign-first (and (positive? npad) (char=? pad #\0)))
         (upper? (char-upper-case? conv)))
    (with-output-to-string
      (lambda ()
        (if (and sign sign-first) (write-char sign))
        (let loop ((n npad)) (when (> n 0) (write-char pad) (loop (1- n))))
        (if (and sign (not sign-first)) (write-char sign))
        (let loop ((v aval) (m (expt base exb)))
          (when (<= 1 m)
            (write-char (digit->char (quotient v m) upper?))
            (loop (remainder v m) (/ m base))))
        (let loop ((n npad))
          (when (< n 0) (write-char pad) (loop (1+ n))))))))

;; (flt->fstr 3.456 %f '() 8 4) => "  3.4560"
;; maybe change to . flags)
(define (flt->fstr val conv width prec . flags)
  (let* ((base 10) (defprec 6)
         (left (memq #\- flags))
         (sign (cond ((negative? val) #\-) ((memq #\+ flags) #\+) (else #f)))
         (aval (abs val))
         (dsh (1+ (max 0 (exp-of-10 aval)))) ; decimal shift, digits left of .
         (raw (+ (if sign 1 0)               ; width w/o constraint
                 (cond ((not prec) (1+ defprec))
                       ((positive? prec) (1+ prec))
                       (else 0))
                 (if (positive? dsh) dsh 1)))
         (prec (cond (prec prec) (width (max (- width raw))) (else defprec)))
         (wid (cond ((not width) raw) ((zero? width) raw)
                    ((> raw width) raw) (else width)))
         (npad (let ((n (if (> raw wid) 0 (- wid raw)))) (if left (- n) n)))
         (pad (cond ((zero? npad) #f) (left #\space)
                    ((memq #\0 flags) #\0) (else #\space)))
         (sign-first (and (positive? npad) (char=? pad #\0)))
         (ival (inexact->exact (floor aval)))
         (fval (inexact->exact (floor (* (- aval ival) (expt 10 prec))))))
    (with-output-to-string
      (lambda ()
        (if (and sign sign-first) (write-char sign))
        (let loop ((n npad)) (when (> n 0) (write-char pad) (loop (1- n))))
        (if (and sign (not sign-first)) (write-char sign))
        (let loop ((v ival) (m (expt base (1- dsh))) (c dsh))
          (cond
           ((positive? c)
            (write-char (digit->char (quotient v m)))
            (loop (remainder v m) (/ m base) (1- c)))
           ((zero? c)
            (write-char #\.)
            (loop fval (expt base (1- prec)) (1- c)))
           ((positive? (+ prec 1 c))
            (write-char (digit->char (quotient v m)))
            (loop (remainder v m) (max 1 (/ m base)) (1- c)))))
        (let loop ((n npad)) (when (< n 0) (write-char pad) (loop (1+ n))))))))

(define (flt->estr val conv width prec . flags)
  (call-with-values
      (lambda () ;; 2.34e-23 => 2.34 -23
        (if (eqv? val 0.0)
            (cons 0.0 0)
            (let ((sign (if (negative? val) -1.0 +1.0))
                  (val (abs val))
                  (v_expt (exp-of-10 val)))
              (values (* sign val (expt 10 (- v_expt))) v_expt))))
    (lambda (m-val e-val)
      (string-append
       (apply flt->fstr m-val conv (and width (- width 4)) prec flags)
       (if (char-upper-case? conv) "E" "e")
       (int->str e-val #\d 3 #f #\0 #\+)))))

(define (apply-fmt port fmt val)
  (let ((str (case (car fmt) ;; ty
               ((#\d #\D #\x #\X) (apply int->str val fmt))
               ((#\f #\F) (apply flt->fstr val fmt))
               ((#\e #\E) (apply flt->estr val fmt))
               ((#\g #\G) (if (or (> (abs val) 1.0e3) (< (abs val) 1.0e-3))
                              (apply flt->estr val fmt)
                              (apply flt->fstr val fmt)))
               ((#\s #\S) val)
               ((#\o) (simple-format #f "~S" val)))))
    (display str port)
    (string-length str)))

(define (nx-formatp port fmt . vals)
  (let* ((fmts (if (string? fmt) (parse-format-string fmt) fmt)))
    (let loop ((n 0) (fmts fmts) (vals vals))
      (cond
       ((null? fmts) (if (pair? vals) (error "too many vals")) n)
       ((string? (car fmts))
        (display (car fmts) port)
        (loop (+ n (string-length (car fmts))) (cdr fmts) vals))
       ((null? vals) (error "too many fmts"))
       (else
        (let ((n (+ n (apply-fmt port (car fmts) (car vals)))))
          (loop n  (cdr fmts) (cdr vals))))))))

(define (nx-format fmt . vals)
  (call-with-output-string
    (lambda (port) (apply nx-formatp port fmt vals))))

(define (nx-format1 fmt . vals)
  (apply nx-formatp (current-output-port) fmt vals))
         
;; === matrices and vectors ========

(define (mat-disp/strict array port format)
  (let* ((conv (array-type array))
         (dims (array-dimensions array))
         (dimz (map (lambda (i) (min i 8)) dims))
         (spec (parse-conv-str format)))
    (do ((i 0 (1+ i))) ((= i (list-ref dimz 0)))
      (do ((j 0 (1+ j))) ((= j (list-ref dimz 1)))
        (display " " port)
        (apply-fmt port spec (array-ref array i j)))
      (newline port))))

(define* (mat-disp array #:optional (port #t) (format "%12.5e"))
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
  (let* ((conv (array-type array))
         (dims (array-dimensions array))
         (dimz (map (lambda (i) (min i 8)) dims))
         (spec (parse-conv-str format)))
    (do ((i 0 (1+ i))) ((= i (list-ref dimz 0)))
      (display " " port)
      (apply-fmt port spec (array-ref array i))
      (newline port))))

(define* (vec-disp array #:optional (port #t) (format "%12.5e"))
  (cond
   ((eq? #f port)
    (let ((strp (open-output-string)))
      (vec-disp/strict array strp format)
      (get-output-string strp)))
   ((eq? #t port)
    (vec-disp/strict array (current-output-port) format))
   (else
    (vec-disp/strict array port format))))


;; === testing

(define (parse-conv-str str) ;; test routine
  (let ((port (open-input-string str))) (parse-fmt (read-char port) port)))

(define (test-istr)
  (define (doit fmt val)
    (sferr "~S ~S => ~S\n" fmt val (apply int->str val (parse-conv-str fmt))))
  (doit "%d" 1)
  (doit "%-04x" 1)
  )

(define (test-fstr)
  (define (doit fmt val)
    (sferr "\t~S\t~S\t=> ~S\n"
           fmt val (apply flt->fstr val (parse-conv-str fmt))))
  (doit "%f" 123.45)
  (doit "%7.1f" 123.45) 
  (doit "%-9.6f" 12.3456789)
  (doit "%-9.6f" -12.3456789)
  (doit "%3.1f" 1.23) 
  (doit "%3.1f" 0.123) 
  (doit "%3.1f" 0.0123) 
  (doit "%.1f" 1.23) 
  (doit "%.1f" 0.123) 
  (doit "%.1f" 0.0123) 
  )

(export parse-conv-str)
(export test-istr test-fstr)
(export int->str flt->fstr flt->estr)


;; === deprecated

(define (disp/fmt val conv flags width prec)
  (case conv
    ((#\e) (flt->estr val conv flags width prec))
    ((#\f) (flt->fstr val conv flags width prec))
    ((#\d) (int->str val conv flags width prec))
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
