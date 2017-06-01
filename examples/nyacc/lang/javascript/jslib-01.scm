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

(define js:+
  (lambda (a b)
    (cond
     ((and (number? a) (number? b)) (+ a b))
     ((string? a)
      (cond ((string? b) (string-append a b))
	    ((number? b) (string-append a (g-str b)))
	    (else (string-append a (g-str b)))))
     ((string? b)
      (cond ((number? a) (string-append (g-str a) b))
	    (else (string-append (g-str a) b))))
     (else
      (string-append (g-str a) (g-str b))))))
(define js:- -)				; hack, convert to numbers
(define js:* *)
(define js:/ /)
(define js:% modulo)
(export js:+ js:- js:* js:/ js:%)

(define js:== equal?)
(define js:=== eqv?)
(export js:== js:===)

;; --- last line ---
