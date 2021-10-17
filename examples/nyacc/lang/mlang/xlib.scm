;; nyacc/lang/mlang/xlib.scm - extension library for the m-language

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

(define-module (nyacc lang mlang xlib)
  #:export (xdict)
  #:use-module (srfi srfi-9)
  #:use-module (nyacc lang mlang parser)
  #:use-module (nyacc lang mlang compile-tree-il)
  #:use-module (system base compile)
  )
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(use-modules (ice-9 pretty-print))
(define (pperr exp)
  (pretty-print exp (current-error-port)))

(define undefined (if #f #f))

(define* (xassert cnd #:optional msg)
  (unless cnd (error (or msg "assertion failed"))))

(define-record-type ml-range
  (make-ml-range start delta end)
  ml-range?
  (start ml-range-start)
  (delta ml-range-delta)
  (end ml-range-end))
;; above generates syntax; we need procedures
(define-public (make-mlang:range start delta end)
  (make-ml-range start delta end))
(define-public (ml:range? obj) (ml-range? obj))
(define-public (ml:range-start rng) (ml-range-start rng))
(define-public (ml:range-delta rng) (ml-range-delta rng))
(define-public (ml:range-end rng) (ml-range-end rng))

;; @deffn {Procedure} ml:range-next rng index
;; Given an index in a range, generate the next index, or @code{#f} if
;; there is no next index.
;; @end deffn
(define-public (ml:range-next rng index)
  (xassert (ml:range? rng))
  (let ((nx (+ index (ml:range-delta rng))))
    (if (positive? (ml:range-delta rng))
	(if (> nx (ml:range-end rng)) #f nx)
	(if (< nx (ml:range-end rng)) #f nx))))

;; for 1-d array do the same
(define-public (ml:array-next ary index)
  (xassert (array? ary))
  (let* ((ub (cadr (car (array-shape ary))))
	 (nx (1+ index)))
    (if (> nx ub) #f nx)))

(define-public (ml:iter-first obj)
  (cond
   ((ml:range? obj) (ml:range-start obj))
   ((array? obj)
    (xassert (= 1 (array-rank obj)) "expecting array to be 1-d")
    (array-ref obj (caar (array-shape obj))))
   (else (xassert #f "expecting range or array"))))

(define-public (ml:iter-next obj index)
  (cond
   ((ml:range? obj) (ml:range-next obj index))
   ((array? obj) (ml:array-next obj index))
   (else (xassert #f "expecting range or array"))))

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
   (else (error "mlang: expecing vector arg of integer, range or array"))))

;; ===

;; inargs and outargs are initialized to 
(define-public (ml:narg . args)
  (let loop ((args args))
    (if (null? args) 0
	(if (eq? (car args) undefined)
	    (loop (cdr args))
	    (1+ (loop (cdr args)))))))

;; Note:
;; We use row-major order, whereas MATLAB used column major order,
;; or at least it originally did.  Not sure since super matrices.
;;

;; matrix: f64-typed array
;; cell array: untyped array
;; struct: hashv table

(define-public (ml:array-ref vec . args)
  ;; args can be positive integer, a range, or an array
  (let ((arg (car args))
	)
    (cond
     ((integer? arg) (array-ref vec (1- arg)))
     (else (error "mlang: expecting array args of integer, range or array")))))

(define-public (ml:array-set-row! ary ix row)
  (let loop ((jx 0) (elts row))
    (unless (null? elts)
      (array-set! ary (car elts) ix jx)
      (loop (1+ jx) (cdr elts)))))

(define-public (ml:aref-or-call proc-or-array . args)
  (cond
   ((procedure? proc-or-array)
    (apply proc-or-array args))
   ((vector? proc-or-array)
    (unless (= 1 (length args))
      (error "mlang: vector ref requires 1 int arg"))
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

;; ===

(define-public (ml:command name . args)
  (cond
   ((string=? name "load")
    (let* ((env (current-module))
	   (file (car args))
	   )
      #f))
   (else
    (throw 'mlang-error "unknown command: ~S" name)))
  (if #f #f))

(define-public (ml:source file)
  (show-mlang-sxml #t)
  (show-mlang-xtil #t)
  (let* ((env (current-module))
	 (tree (and (string? file) (access? file R_OK)
		    (call-with-input-file file
		      (lambda (port) (read-mlang-file port env)))))
	 (itil (compile-tree-il tree env '()))
	 (base (basename file ".m"))
	 (var (module-variable env (string->symbol base)))
	 )
    (compile itil #:from 'tree-il #:to 'value #:env env)
    ;;(sferr "env=~S\n" env)
    ;;(sferr "var=~S\n" var)
    ;;(pperr var)
    ;;(quit)
    (if #f #f)))

;; ===

(define xdict
 `(("struct" . (@ (nyacc lang mlang xlib) ml:make-struct))
   ("source" . (@ (nyacc lang mlang xlib) ml:source))))

;; --- last line ---
