;;; nyacc/lang/javascript/jslib.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
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

;; needs: null, undefined, undeclared?,

(define-module (nyacc lang javascript jslib)
  #:export (JSdict)
  )

(define JSdict
  `((@l . 0) (@P . ())
    ;;("Object" . (@@ (jslib) Object))
    ;;("Math" . (@@ (jslib) Math))
    ;;("Number" . (@@ (jslib) Number))
    ;;("JS:+" . (@@ (jslib) JS:+))
    ))

(define undefined (if #f #f))

;; @item lkup obj name
;; Find property in object, or prototype, or ???
(define (lkup obj key)
  (if (string? key) (lkup obj (string->symbol key))
      (cond
       ((hashq-ref obj key))
       (else #f))))

;; @item make-args args gsym
;; This should generate a arguments object for every function.
;; The input is a list of argument names bound to the array '@args w/ gsym.
(define (make-args args gsym)
  #f)

(define Object (make-hash-table 31))
(hashq-set! Object 'constructor
	    (case-lambda
	     (() (make-hash-table 31))
	     ((value) (make-hash-table 31))))
(hashq-set! Object 'prototype Object)
;; hasOwnProperty (lambda () ...)
;; isPrototypeOf (lambda () ...)
;; propertyIsEnumerable (lambda () ...)
;; toLocaleString (lambda () ...)
;; toString (lambda () ...)
;; valueOf (lambda () ...)

(define Math (make-hash-table 31))
(hashq-set! Math 'sqrt (lambda (n) (sqrt n)))

(define Number (make-hash-table 31))
(hash-set! Number 'MAX_VALUE 9999)
(hash-set! Number 'MIN_VALUE  -9999)
(hash-set! Number 'NaN (nan))
(hash-set! Number 'toString (lambda (n) (number->string n)))

(define JS:+ (lambda (a b)
	      (cond
	       ((and (string? a) (string? b)) (string-append a b))
	       ((and (number? a) (number? b)) (+ a b))
	       (else 'undefined))))

;; --- last line ---
