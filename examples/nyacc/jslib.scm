;; lang/javascript/jslib.scm
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;; or any later version published by the Free Software Foundation.  See
;; the file COPYING included with the this distribution.

;; needs: null, undefined, undeclared?,

(define-module (jslib)
  #:export (JSdict)
  )

(define JSdict
  `((@l . 0) (@P . ())
    ;;("Object" . (@@ (jslib) Object))
    ;;("Math" . (@@ (jslib) Math))
    ;;("Number" . (@@ (jslib) Number))
    ;;("JS+" . (@@ (jslib) JS+))
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

(define JS+ (lambda (a b)
	      (cond
	       ((and (string? a) (string? b)) (string-append a b))
	       ((and (number? a) (number? b)) (+ a b))
	       (else 'undefined))))

;; --- last line ---
