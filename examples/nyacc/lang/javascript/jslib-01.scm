;;; nyacc/lang/javascript/jslib-01.scm
;;;
;;; Copyright (C) 2017 Matthew R. Wette
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; jslib-01.scm: operators
;; included into jslib.scm

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

(define (js:== lt rt)
  (cond
   ((eqv? 'null lt) (eqv? js:undefined rt) #t)
   ((eqv? 'null rt) (eqv? js:undefined lt) #t)
   (else (equal? lt rt))))
(define js:=== eqv?)
(export js:== js:===)

(define (js:_++ ooa)
  (js-ooa-set ooa (js:+ 1 (js-ooa-ref ooa)))
  (js-ooa-ref ooa))

;; --- last line ---
