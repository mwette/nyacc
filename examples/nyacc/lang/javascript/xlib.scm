;;; nyacc/lang/javascript/xlib.scm - extension library

;; Copyright (C) 2015,2017-2018 Matthew R. Wette
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

;;; needs: null, undefined, undeclared?,

(define-module (nyacc lang javascript xlib)
  #:export (JSdict
	    js:undefined
	    js:null
	    )
  )

(define (obj-toString obj)
  "[object Object]")


(define (mkref name) `(@@ (nyacc lang javascript jslib) ,name))

(define JSdict
  `(
    (js:+ ,(mkref 'js:+))
    (js-ooa-get ,(mkref 'js-ooa-get))
    (js-ooa-put ,(mkref 'js-ooa-put))
    ))

;; this should throw an Error object
(define (js-error text)
  (throw 'js-error text))
(export js-error)

(define js:undefined (if #f #f))
(define js:null '())

;; like python str(obj)
(define (g-str obj)
  (call-with-output-string
   (lambda (port)
     (display obj port))))

;; === Objects and Arrays ============

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

;; @subheading Objects and Arrays
;; ooa     : object or array
;; ooa-elt : (cons <ooa> <expr>)
;; (define (js-ooa-get ooa-elt) =>
;;     (if (number? expr) (vector-ref js-ooa elt) (assq-ref ooa elt)
;; (define (js-ooa-put ooa-elt val) =>
;;     (if (number? expr) (vector-ref js-ooa elt) (assq-ref ooa elt)

;; default atttributes
;; [[Value]]
;; [[Get]]
;; [[Set]]
;; [[Writable]]
;; [[Enumerable]]
;; [[Configurable]]
;; Internal Properties:
;; [[Prototype]]
;; [[Class]]
;; [[Extensible]]

;; @subsubheading References
;; References (to properties of objects or elements of arrays) are implemented
;; as cons cells where car is the object expr and cdr is the name

;; but what about _++ and ++_

;; @deffn {Procedure} js-make-object @dots{} => js-obj
;; Make an object given name, value, name, value, ...
;; @example
;; (define console (js-make-object ("abc" . 123) ("log" . (lambda* ...))))
;; @end example
;; @end deffn
(define-public (js-make-object . rest)
  (let ((obj (make-hash-table)))
    (let loop ((pairs rest))
      (when (pair? pairs)
	(hashq-set! obj (car pairs) (cadr pairs))
	(loop (cddr pairs))))
    obj))
(define mkobj js-make-object)

;; new 180821
(define-public (js:ooa-ref ooa arg)
  (cond
   ((vector? ooa) (vector-ref ooa arg))
   ((hash-table? ooa) (hashq-ref ooa arg))
   (else (error "expecing object or array:" ooa))))

(define (js-make-array . rest)
  (apply vector rest))
(export js-make-array)
(define mkary js-make-array)

(define (js-resolve exp)
  (cond
   ((not (pair? exp)) exp)
   ((vector? (car exp)) (vector-ref (car exp) (js-resolve (cdr exp))))
   ((hash-table? (car exp)) (hashq-ref (car exp) (js-resolve (cdr exp))))
   (else exp)))

(define (js-ooa-get ooa-elt)
  (cond
   ((not (pair? ooa-elt)) (js-error "js-ooa-get"))
   ((hash-table? (car ooa-elt)) (hashq-ref (car ooa-elt) (cdr ooa-elt)))
   ((vector? (car ooa-elt)) (vector-ref (car ooa-elt) (cdr ooa-elt)))
   (else (js-error "js-ooa-get"))))
(export js-ooa-get)

(define (js-ooa-put ooa-elt val)
  (cond
   ((not (pair? ooa-elt)) (js-error "js-ooa-put 1"))
   ((hash-table? (car ooa-elt)) (hashq-set! (car ooa-elt) (cdr ooa-elt) val))
   ((vector? (car ooa-elt)) (vector-set! (car ooa-elt) (cdr ooa-elt) val))
   (else (js-error "js-ooa-put 2"))))
(export js-ooa-put)

;; @item lkup obj name
;; Find property in object, or prototype, or ???
(define (lkup obj key)
  (if (string? key) (lkup obj (string->symbol key))
      (cond
       ((hashq-ref obj key))
       (else #f))))

;; ==============================

(define-public Math (make-hash-table 31))
(hashq-set! Math 'sqrt (lambda (n) (sqrt n)))

(define Number (make-hash-table 31))
(hash-set! Number 'MAX_VALUE 9999)
(hash-set! Number 'MIN_VALUE  -9999)
(hash-set! Number 'NaN (nan))
(hash-set! Number 'toString (lambda (n) (number->string n)))

;; for me

(include-from-path "nyacc/lang/javascript/xlib-01.scm")

(define-public (js_lookup name)
  (let ((var (module-variable (current-module) (string->symbol name))))
    (if var (variable-ref var) (throw 'js-error "not defined"))))

(define-public (js_format fmt . args)
  (apply simple-format #t fmt args))

;; --- last line ---
