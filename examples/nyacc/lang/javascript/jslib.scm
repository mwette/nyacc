;;; nyacc/lang/javascript/jslib.scm
;;;
;;; Copyright (C) 2015,2017 Matthew R. Wette
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
  #:export (JSdict
	    js:undefined)
  )

(define JSdict
  `((@l . 0) ;; (@P . '())
    ;;("Object" . (@@ (jslib) Object))
    ;;("Math" . (@@ (jslib) Math))
    ;;("Number" . (@@ (jslib) Number))
    ;;("js:+" . (@@ (jslib) js:+))
    ))

;; null is 'js:null

(define js:undefined (if #f #f))

;; like python str(obj)
(define (g-str obj)
  (call-with-output-string
   (lambda (port)
     (display obj port))))

;; === Objects and Arrays ============

;; @subheading Objects and Arrays
;; ooa     : object or array
;; ooa-elt : (cons <ooa> <expr>)
;; (define (js-ooa-ref ooa-elt) =>
;;     (if (number? expr) (vector-ref js-ooa elt) (assq-ref ooa elt)

;; @deffn {Procedure} js-make-object @dots{} => js-obj
;; Make an object given name, value, name, value, ...
;; @end deffn
(define (js-make-object . rest)
  (let ((obj (make-hash-table 31)))
    (let iter ((pairs rest))
      (when (pair? pairs)
	(hash-set! obj (car pairs) (cadr pairs))
	(iter (cddr pairs))))
    obj))
(export js-make-object)
(define mkobj js-make-object)

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

;; @item lkup obj name
;; Find property in object, or prototype, or ???
(define (lkup obj key)
  (if (string? key) (lkup obj (string->symbol key))
      (cond
       ((hashq-ref obj key))
       (else #f))))

;; ==============================

(define Math (make-hash-table 31))
(hashq-set! Math 'sqrt (lambda (n) (sqrt n)))

(define Number (make-hash-table 31))
(hash-set! Number 'MAX_VALUE 9999)
(hash-set! Number 'MIN_VALUE  -9999)
(hash-set! Number 'NaN (nan))
(hash-set! Number 'toString (lambda (n) (number->string n)))


(include-from-path "nyacc/lang/javascript/jslib-01.scm")

;; --- last line ---
