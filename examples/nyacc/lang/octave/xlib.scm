;; nyacc/lang/octave/xlib.scm - extension library for the m-language

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

(define-module (nyacc lang octave xlib)
  #:export (xdict)
  #:use-module (srfi srfi-9)
  )
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))

(define* (xassert cnd #:optional msg)
  (unless cnd (error (or msg "assertion failed"))))

(define-record-type ml-range
  (make-ml-range start delta end)
  ml-range?
  (start ml-range-start)
  (delta ml-range-delta)
  (end ml-range-end))
;; above generates syntax; we need procedures
(define-public (make-oct:range start delta end) (make-ml-range start delta end))
(define-public (oct:range? obj) (ml-range? obj))
(define-public (oct:range-start rng) (ml-range-start rng))
(define-public (oct:range-delta rng) (ml-range-delta rng))
(define-public (oct:range-end rng) (ml-range-end rng))

;; @deffn {Procedure} oct:range-next rng index
;; Given an index in a range, generate the next index, or @code{#f} if
;; there is no next index.
;; @end deffn
(define-public (oct:range-next rng index)
  (xassert (oct:range? rng))
  (let ((nx (+ index (oct:range-delta rng))))
    (if (positive? (oct:range-delta rng))
	(if (> nx (oct:range-end rng)) #f nx)
	(if (< nx (oct:range-end rng)) #f nx))))

;; for 1-d array do the same
(define-public (oct:array-next ary index)
  (xassert (array? ary))
  (let* ((ub (cadr (car (array-shape ary))))
	 (nx (1+ index)))
    (if (> nx ub) #f nx)))

(define-public (oct:iter-first obj)
  (cond
   ((oct:range? obj) (oct:range-start obj))
   ((array? obj)
    (xassert (= 1 (array-rank obj)) "expecting array to be 1-d")
    (array-ref obj (caar (array-shape obj))))
   (else (xassert #f "expecting range or array"))))

(define-public (oct:iter-next obj index)
  (cond
   ((oct:range? obj) (oct:range-next obj index))
   ((array? obj) (oct:array-next obj index))
   (else (xassert #f "expecting range or array"))))

(define-public (oct:or a b) (if (and (zero? a) (zero? b)) 0 1))
(define-public (oct:and a b) (if (or (zero? a) (zero? b)) 0 1))
(define-public (oct:eq a b) (if (equal? a b) 1 0))
(define-public (oct:ne a b) (- 1 (oct:eq a b)))
(define-public (oct:lt a b) (if (< a b) 1 0))
(define-public (oct:gt a b) (if (> a b) 1 0))
(define-public (oct:le a b) (if (<= a b) 1 0))
(define-public (oct:ge a b) (if (>= a b) 1 0))
(define-public (oct:+ a b) (+ a b))
(define-public (oct:- a b) (- a b))
(define-public (oct:* a b) (* a b))
(define-public (oct:/ a b) (/ a b))

(define-public (oct:vector-ref vec arg)
  ;; arg can be a positive integer, a range, or an array
  ;;(sferr "arg=~S\n" arg)
  (cond
   ((integer? arg) (vector-ref vec (1- arg)))
   (else (error "octave: expecing vector arg of integer, range or array"))))

;; ===

;; Note:
;; We use row-major order, whereas MATLAB used column major order,
;; or at least it originally did.  Not sure since super matrices.
;;

;; matrix: f64-typed array
;; cell array: untyped array
;; struct: hashv table

(define-public (oct:array-ref vec . args)
  ;; args can be positive integer, a range, or an array
  (let ((arg (car args))
	)
    (cond
     ((integer? arg) (array-ref vec (1- arg)))
     (else (error "octave: expecting array args of integer, range or array")))))

(define-public (oct:array-set-row! ary ix row)
  (let loop ((jx 0) (elts row))
    (unless (null? elts)
      (array-set! ary (car elts) ix jx)
      (loop (1+ jx) (cdr elts)))))

(define-public (oct:aref-or-call proc-or-array . args)
  (sferr "proc-or-array=~S  args=~S\n" proc-or-array args)
  (cond
   ((procedure? proc-or-array)
    (apply proc-or-array args))
   ((vector? proc-or-array)
    (unless (= 1 (length args))
      (error "octave: vector ref requires 1 int arg"))
    (oct:vector-ref proc-or-array (car args)))
   ((array? proc-or-array)
    (apply oct:array-ref proc-or-array args))
   (else
    (error "expecting function or array"))))

(define-public (oct:assn-elt arry expl value)
  #f)
      
;; @deffn {Procedure} oct:make-struct [args]
;; Generate a struct.  Currently no args are processed.
;; The hash size is 31.
;; @end deffn
(define-public (oct:make-struct . args)
  (make-hash-table 31))

;; @deffn {Procedure} oct:struct-set! expr name
;; Get @code{expr.name}.  @var{name} is assumed to be a symbol.
;; @end deffn
(define-public (oct:struct-ref expr name)
  (unless (hash-table? expr) (error "expecting hash table"))
  (hashq-ref expr name))

;; @deffn {Procedure} oct:struct-set! expr name value
;; Set @code{expr.name = value}.  The struct member @var{name}
;; does not need to exist.  @var{name} is assumed to be a
;; symbol.
;; @end deffn
(define-public (oct:struct-set! expr name value)
  (unless (hash-table? expr) (error "expecting hash table"))
  (hashq-set! expr name value)
  (if #f #f))


;; ===

(define xdict
 `(
   ("struct" . (@ (nyacc lang octave xlib) oct:make-struct))
    ))

;; --- last line ---
