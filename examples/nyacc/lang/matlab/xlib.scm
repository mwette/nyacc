;; nyacc/lang/matlab/xlib.scm - extension library

;; Copyright (C) 2018 Matthew R. Wette
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

;;; Description:

;; Note: range is type

;;; Code:

(define-module (nyacc lang matlab xlib)
  #:export (xdict)
  #:use-module (srfi srfi-9)
  )
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))

(define-record-type ml-range
  (make-ml-range lb inc ub)
  ml-range?
  (lb ml-range-lb)
  (inc ml-range-inc)
  (ub ml-range-ub))
;; above generates syntax; we need procedures
(define-public (make-ml:range lb inc ub) (make-ml-range lb inc ub))
(define-public (ml:range? obj) (ml-range? obj))
(define-public (ml:range-lb rng) (ml-range-lb rng))
(define-public (ml:range-inc rng) (ml-range-inc rng))
(define-public (ml:range-inc rng) (ml-range-ub rng))


(define-public (ml:or a b) (if (and (zero? a) (zero? b)) 0 1))
(define-public (ml:and a b) (if (or (zero? a) (zero? b)) 0 1))
(define-public (ml:eq a b) (if (equal? a b) 1 0))
(define-public (ml:ne a b) (- 1 (ml:eq a b)))
(define-public (ml:lt a b) (if (< a b) 1 0))
(define-public (ml:gt a b) (if (> a b) 1 0))
(define-public (ml:le a b) (if (<= a b) 1 0))
(define-public (ml:ge a b) (if (>= a b) 1 0))
(define-public (ml:+ a b) (+ a b))
(define-public (ml:- a b) (- a b))
(define-public (ml:* a b) (* a b))
(define-public (ml:/ a b) (/ a b))

(define-public (ml:vector-ref vec arg)
  ;; arg can be a positive integer, a range, or an array
  ;;(sferr "arg=~S\n" arg)
  (cond
   ((integer? arg) (vector-ref vec (1- arg)))
   (else (error "matlab: expecing vector arg of integer, range or array"))))

(define-public (ml:array-ref vec . args)
  ;; args can be positive integer, a range, or an array
  (let ((arg (car args))
	)
    (cond
     ((integer? arg) (array-ref vec (1- arg)))
     (else (error "matlab: expecting array args of integer, range or array")))))

(define-public (ml:aref-or-call proc-or-array . args)
  ;;(sferr "proc-or-array=~S  args=~S\n" proc-or-array args)
  (cond
   ((procedure? proc-or-array)
    (apply proc-or-array args))
   ((vector? proc-or-array)
    (unless (= 1 (length args))
      (error "matlab: vector ref requires 1 int arg"))
    (ml:vector-ref proc-or-array (car args)))
   ((array? proc-or-array)
    (apply ml:array-ref proc-or-array args))
   (else
    (error "expecting function or array"))))

(define-public (ml:assn-elt arry expl value)
  #f)
      
;; @deffn {Procedure} ml:make-struct [args]
;; Generate a struct.  Currently no args are processed.
;; The hash size is 31.
;; @end deffn
(define-public (ml:make-struct . args)
  (make-hash-table 31))

;; @deffn {Procedure} ml:struct-set! expr name
;; Get @code{expr.name}.  @var{name} is assumed to be a symbol.
;; @end deffn
(define-public (ml:struct-ref expr name)
  (unless (hash-table? expr) (error "expecting hash table"))
  (hashq-ref expr name))

;; @deffn {Procedure} ml:struct-set! expr name value
;; Set @code{expr.name = value}.  The struct member @var{name}
;; does not need to exist.  @var{name} is assumed to be a
;; symbol.
;; @end deffn
(define-public (ml:struct-set! expr name value)
  (unless (hash-table? expr) (error "expecting hash table"))
  (hashq-set! expr name value)
  (if #f #f))


(define xdict
 `(
   ("struct" . (@ (nyacc lang matlab xlib) ml:make-struct))
    ))

;; --- last line ---
