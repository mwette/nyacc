;;; nyacc/lang/javascript/xlib-01.scm

;; Copyright (C) 2017-2018 Matthew R. Wette
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; xlib-01.scm: operators
;; included into jslib.scm

(define (js:neg val)
  (- (if (string? val) (string->number val) val)))
  
(define (js:pos val)
  (if (string? val) (string->number val) val))
  
(define (js:+ lt rt)
  (let ((lt (if (pair? lt) (js-ooa-get lt) lt))
	(rt (if (pair? rt) (js-ooa-get rt) rt)))
    (cond
     ((and (number? lt) (number? rt)) (+ lt rt))
     ((string? lt)
      (cond ((string? rt) (string-append lt rt))
	    ((number? rt) (string-append lt (g-str rt)))
	    (else (string-append lt (g-str rt)))))
     ((string? rt)
      (cond ((number? lt) (string-append (g-str lt) rt))
	    (else (string-append (g-str lt) rt))))
     (else
      (string-append (g-str lt) (g-str rt))))))
(define (js:- lt rt)
  ;; should convert to numbers if string
  (let ((lt (if (pair? lt) (js-ooa-get lt) lt))
	(rt (if (pair? rt) (js-ooa-get rt) rt)))
    (- lt rt)))
(define (js:* lt rt)
  (let ((lt (if (pair? lt) (js-ooa-get lt) lt))
	(rt (if (pair? rt) (js-ooa-get rt) rt)))
    (* lt rt)))
(define js:/ /)
(define js:% modulo)
(export js:+ js:- js:* js:/ js:%)

(define (js:lshift lt rt)
  (ash lt rt))
(define (js:rshift lt rt)
  (ash lt (- rt)))
(define (js:rrshift lt rt)		; FIX
  (ash lt (- rt)))
(define (js:bit-and lt rt)		; FIX
  (logand lt rt))
(define (js:bit-or lt rt)		; FIX
  (logior lt rt))
(define (js:bit-xor lt rt)		; FIX
  (logxor lt rt))
(define (js:or lt rt)			; FIX
  (or lt rt))
(define (js:and lt rt)			; FIX
  (and	 lt rt))
(define (js:eq lt rt)			; FIX
  (equal? lt rt))
(define (js:neq lt rt)			; FIX
  (not (js:eq lt rt)))
(export js:lshift js:rshift js:rrshift js:and js:or)

;; (and-assign . js:and) (xor-assign . js:xor) (or-assign . js:or)

(define (js:lt lt rt)
  (< lt rt))
(define (js:gt lt rt)
  (> lt rt))
(define (js:le lt rt)
  (<= lt rt))
(define (js:ge lt rt)
  (>= lt rt))
(export js:lt js:gt js:le js:ge)

(define (js:== lt rt)
  (cond
   ((eqv? 'null lt) (eqv? js:undefined rt) #t)
   ((eqv? 'null rt) (eqv? js:undefined lt) #t)
   (else (equal? lt rt))))
(define js:=== eqv?)
(export js:== js:===)

(define (js:_++ ooa)
  (js-ooa-put ooa (js:+ 1 (js-ooa-get ooa)))
  (js-ooa-get ooa))

;; --- last line ---
