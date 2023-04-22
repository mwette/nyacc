;;; nyacc/lang/tsh/xlib.scm

;; Copyright (C) 2018,2021 Matthew R. Wette
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

;;; Notes:

;; 1) need (tsh:array-ref ref expr-list)

;;; Code:

(define-module (nyacc lang tsh xlib)
  #:export (xdict xlib-ref tsh:source tsh:puts)
  #:use-module (nyacc lang tsh parser)
  #:use-module (nyacc lang tsh compile-tree-il)
  #:use-module (nyacc lang nx-util)
  #:use-module (system base compile)
  ;;#:use-module (rnrs arithmetic bitwise)
  )
(use-modules (ice-9 pretty-print))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr exp) (pretty-print exp (current-error-port)))

(define (xlib-ref name) `(@@ (nyacc lang tsh xlib) ,name))

(define tsh:+ +)
(define tsh:- -)
(define tsh:* *)
(define tsh:% modulo)
(define (tsh:/ lt rt)
  (if (and (exact? lt) (exact? rt)) (quotient lt rt) (/ lt rt)))

(define (fix-pred/1 proc) (lambda (arg) (if (proc arg) 1 0)))
(define (fix-pred/2 proc) (lambda (lt rt) (if (proc lt rt) 1 0)))
(define (fix-pred/* proc) (lambda args (if (apply proc args) 1 0)))

(define tsh:eq (fix-pred/2 equal?))
(define tsh:ne (lambda (a b) (if (equal? a b) 0 1)))
(define tsh:gt (fix-pred/2 >))
(define tsh:lt (fix-pred/2 <))
(define tsh:le (fix-pred/2 <=))
(define tsh:ge (fix-pred/2 >=))

;; @deffn {tsh} source file
;; where @var{file} is a string
;; @end deffn
;;(define-public (tsh:source file env)
(define* (tsh:source file #:optional (env (current-module)))
  (let* ((sx (call-with-input-file file
	       (lambda (port) (read-tsh-file port env))))
	 (tx (call-with-values
		 (lambda () (compile-tree-il sx env '()))
	       (lambda (itil env cenv) itil))))
    (when #f
      (sferr "tsh:source, sx:\n")
      (pperr sx)
      (sferr "  src-prop:\n")
      (pperr (add-src-prop-attr sx)))
    (compile tx #:from 'tree-il #:to 'value #:env env)
    (if #f #f)))

;; puts object
;; puts <port> object
;; puts -nonewline object
;; puts -nonewline <port> object
(define-public tsh:puts
  (case-lambda
   ((val) (display val) (newline))
   ((arg0 val)
    (if (and (keyword? arg0) (equal? arg0 #:no_newline))
	(display val)
	(display val arg0)))		; broken need arg0 => port
   ((nnl chid val)
    (unless (and (keyword? nnl) (equal? nnl #:nonewline))
      (throw 'tsh-error "puts: bad arg"))
    "(not implemented)"
    )))

;; f64 unit-expr or maybe  use the array i/f
(define (list->typed-vec type elts)
    (list->typed-array type '(0) elts))

(define-public tsh:array-ref array-ref)
(define-public (tsh:fvec . args) (list->typed-vec 'f64 args))
(define-public (tsh:ivec . args) (list->typed-vec 's32 args))
(define-public (tsh:avec . args) (list->typed-vec #t args))
(define-public tsh:vlen array-length)

(define-public tsh:vtype
  (lambda (ary)
    (case (array-type ary)
      ((f64) 'flt)
      ((s32) 'int)
      ((#t) 'any))))

;; === xdict

(define xdict
  `(
    ("puts" . ,(xlib-ref 'tsh:puts))
    ("fvec" . ,(xlib-ref 'tsh:fvec))
    ("ivec" . ,(xlib-ref 'tsh:ivec))
    ("avec" . ,(xlib-ref 'tsh:avec))
    ;; 
    ("vlen" . ,(xlib-ref 'tsh:vlen))
    ("vtype" . ,(xlib-ref 'tsh:vtype))
    ))

;; --- last line ---
