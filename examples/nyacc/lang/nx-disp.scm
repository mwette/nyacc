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
;; type: #\d #\x #\f #\e

;; flt->str/f:
;;   flags list of chars
;;   type #\e #\f
;;   NEEDS WORK: getting negative val-wid's

;;; Code:

(define-module (nyacc lang nx-disp)
  #:export (mat-disp vec-disp parse-fmt parse-fmt-str fmt->str)
  )

(define (sf fmt . args) (apply simple-format #t fmt args))

;; For a value tell me how many digits it will have, or equiv pow-10 exponent.
;; 35 => 2
;; 3.459393e32 => 32

;; int->str:
;;   when (memq #\- flags) left align
;;   flags list of flags: #\0 #\- #\+
;;   width - min wid or 0 for any
;;   type - #\d #\x

;; exp for val = [1.0,10.0)x10^exp
(define (exp-of-10 val)
  (if (eqv? val 0.0) 0
      (inexact->exact (floor (log10 (abs val))))))

(define (exp-of-16 val)
  (define (log16 x) (/ (log x) (log 16)))
  (if (eqv? val 0.0) 0
      (inexact->exact (floor (log16 (abs val))))))

(define (exp-of-base val base)
  (case base
    ((10 #\d) (exp-of-10 val))
    ((16 #\x) (exp-of-16 val))))

;; (int->numch 3) => #\3
(define int->numch
  (let ((cv (vector #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
		    #\A #\B #\C #\D #\E #\F)))
    (lambda (iv) (vector-ref cv iv))))

;; (int->str 23 '(#\+) 4 0 #\d) => " +23"
(define (int->str val type flags width prec)
  ;;(sf "val  =~S\n" val)
  ;;(sf "type =~S\n" type)
  ;;(sf "flags=~S\n" flags)
  ;;(sf "width=~S\n" width)
  ;;(sf "prec =~S\n" prec)
  (let* ((base (case type ((#\d) 10) ((#\x) 16) (else (error "bad type"))))
	 (left (memq #\- flags))
	 (pad (if (memq #\0 flags) #\0 #\space))
	 (sign (cond ((negative? val) #\-) ((memq #\+ flags) #\+) (else #f)))
	 (val (abs val))
	 (val-wid (+ (exp-of-base (* 1.0 val) base) 1 (if sign 1 0)))
	 (wid (if (zero? width) val-wid width))
	 (npad (- wid val-wid))
	 (str (make-string wid (if (negative? npad) #\# pad))))
    ;;(sf "base=~S\n" base)
    ;;(sf "exp-of-base=~S\n" (exp-of-base (* 1.0 val) base))
    ;;(sf "val-wid=~S\n" val-wid)
    ;;(sf "wid=~S\n" wid)
    ;;(sf "npad=~S\n" npad)
    (if(negative? npad)
       (make-string wid #\#)
       ;; This is ugly, need rework.
       (let ((str (make-string wid pad))
	     (ix (- wid 1 (if (memq #\- flags) npad 0))))
	 (let loop ((x ix) (v val))
	   (cond
	    ((zero? v)
	     (if (= ix x) (begin (string-set! str x #\0) (set! x (1- x))))
	     (if sign (string-set! str x sign))
	     str)
	    (else
	     (string-set! str x (int->numch (remainder v base)))
	     (loop (1- x) (quotient v base)))))))))
(export int->str)

;; frexp(double x, int *exp) => double ; result always in [0.5,1.0)

;;(define (frexp val)
;;  (

;; (split-float 2.3492e-23) => (2.3492 -23)
(define (split-float val)
  (if (eqv? val 0.0)
      (cons 0.0 0)
      (let ((sign (if (negative? val) -1.0 +1.0))
	    (val (abs val))
	    (v_expt (exp-of-10 val)))
	(cons (* sign val (expt 10 (- v_expt))) v_expt))))
(export split-float)

;; (vch 3.235) => #\3
(define (vch v) (int->numch (inexact->exact (floor v))))

;; (vnx 3.235) => 2.35
(define (vnx v) (* 10 (- v (inexact->exact (floor v)))))

;; (flt->str/f 3.456 '() 8 4) => "  3.4560"
(define (flt->str/f val type flags width prec)
  (let* ((base 10)
	 (left (memq #\- flags))
	 (pad (if (memq #\0 flags) #\0 #\space))
	 (sign (cond ((negative? val) #\-) ((memq #\+ flags) #\+) (else #f)))
	 (val (abs val))
	 (v-p (split-float val)) (val-m (car v-p)) (val-e (cdr v-p))
	 (val-wid (min 6 (+ (- (abs val-e) 2) (if sign 1 0))))
	 (wid (if (zero? width) val-wid width))
	 (wid (if sign (1- wid) wid))
	 (chl (if sign (list sign) '()))
	 (wid (if (negative? val-e) (- wid 2) wid))
	 (chl (if (negative? val-e) (cons* #\. #\0 chl) chl)))
    ;;(if (negative? wid val-wid) (make-string wid #\#))
    (let loop ((chl chl) (nl wid) (v val-m) (e (1+ val-e)))
      (cond
       ((negative? e) (loop (cons #\0 chl) (1- nl) v (1+ e)))
       ((= 1 e) (loop (cons* #\. (vch v) chl) (- nl 2) (vnx v) (1- e)))
       ((positive? e) (loop (cons (vch v) chl) (1- nl) (vnx v) (1- e)))
       ((positive? nl) (loop (cons (vch v) chl) (1- nl) (vnx v) e))
       (else (reverse-list->string chl))))))

;; (flt->str/e -12.34e-22 '() 12 5) => "-1.23400e-21"
(define (flt->str/e val type flags width prec)
  (let* ((base 10)
	 (left (memq #\- flags))
	 (pad (if (memq #\0 flags) #\0 #\space))
	 (val-pair (split-float val))
	 (val-m (car val-pair))
	 (val-e (cdr val-pair))
	 (e-str (int->str val-e #\d '(#\+) 0 0))
	 (m-wid (- width 1 (string-length e-str)))
	 (m-str (flt->str/f val-m #\f flags m-wid prec)))
    ;;(sf "val-m=~S\n" val-m)
    ;;(sf "val-e=~S\n" val-e)
    ;;(sf "e-str=~S\n" e-str)
    (string-append m-str "e" e-str)))

(define (flt->str val type flags width prec)
  (case type
    ((#\e) (flt->str/e val type flags width prec))
    ((#\f) (flt->str/f val type flags width prec))
    (else (error "fmat"))))
(export flt->str)

(define (disp/fmt val type flags width prec)
  (case type
    ((#\e) (flt->str/e val type flags width prec))
    ((#\f) (flt->str/f val type flags width prec))
    ((#\d) (int->str val type flags width prec))
    ;;((#\s) 
    (else (error "fmat"))))
  
;; (parse-fmt ch port) => (type flags width prec) | #\%
(define* (parse-fmt ch #:optional (port (current-input-port)))
  (define (rd-ch) (read-char port))
  (define C0 (char->integer #\0))
  (define (ch-add ch val) (+ (- (char->integer ch) C0) (* 10 val)))
  ;; flags width precision type state char
  (let loop ((fl '()) (wd 0) (pc 0) (ty #f) (st 0) (ch ch))
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
	((char-numeric? ch) (loop fl (ch-add ch wd) pc ty st (rd-ch)))
	((char=? #\. ch) (loop fl wd pc ty 3 (rd-ch)))
	(else (loop fl wd pc ty 4 ch))))
      ((3) ;; read precision
       (cond
	((char-numeric? ch) (loop fl wd (ch-add ch pc) ty st (rd-ch)))
	(else (loop fl wd pc ty 4 ch))))
      ((4) ;; read type
       (case ch
	 ((#\%) #\%)
	 ((#\d #\i #\u) (loop fl wd pc #\d 5 ch))
	 ((#\x #\X) (loop fl wd pc #\x 5 ch))
	 ((#\e #\E #\g #\G) (loop fl wd pc #\e 5 ch))
	 ((#\f #\F) (list fl wd pc #\f 5 ch))))
      ((5) (list ty fl wd pc)))))

;; (parse-fmt-str spec-str) => (flags width prec type) | #\%
;; spec-str "%-12.5e" ...
(define (parse-fmt-str str)
  (let ((port (open-input-string str)))
    (parse-fmt (read-char port) port)))
  
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

;; --- last line ---
