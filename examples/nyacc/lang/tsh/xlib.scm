;;; nyacc/lang/tsh/xlib.scm

;; Copyright (C) 2018,2021,2023 Matthew Wette
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
  #:use-module (nyacc lang nx-lib)
  #:use-module (nyacc lang nx-format)
  #:use-module ((srfi srfi-1) #:select (split-at last))
  #:use-module (srfi srfi-111)          ; boxes
  #:use-module (system base compile)
  #:use-module (ice-9 hash-table))

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
  (case-lambda ;; maybe match for keywords
   ((val) (display val) (newline))
   ((port val) (display val port) (newline))
   ))

(define-public tsh:format nx-format)

(define-public tsh:last last)

;; f64 unit-expr or maybe  use the array i/f
(define (list->typed-vec type elts)
    (list->typed-array type '(0) elts))

(define-public (tsh:fvec . args) (list->typed-vec 'f64 args))
(define-public (tsh:ivec . args) (list->typed-vec 's32 args))
(define-public (tsh:avec . args) (list->typed-vec #t args))
(define-public tsh:vlen array-length)

(define-public (tsh:indexed-ref obj indx)
  (if (null? indx) obj
      (cond
       ((hash-table? obj)
        (unless (symbol? (car indx)) (nx-error "expecting symbol"))
        (let ((val (hashq-ref obj (car indx))))
          (unless val (nx-error "field does not exist: ~S" (car indx)))
          (tsh:indexed-ref val (cdr indx))))
       ((array? obj)
        (let ((rk (array-rank obj))
              (nx (length indx)))
          (unless (<= rk nx) (nx-error "no shared-arrays (yet)"))
          (call-with-values
              (lambda () (split-at indx rk))
            (lambda (indx rest)
              (tsh:indexed-ref (apply array-ref obj indx) rest)))))
       (else (nx-error "indexed-ref on non-array, non-struct")))))
  
(define-public (tsh:indexed-set! obj indx val)
  ;; complicated : from end get symbol or longest string of ints
  (let loop ((l1 '()) (l2 '()) (l3 indx))
    (cond
     ((null? l3)
      (let ((obj (tsh:indexed-ref obj (reverse l1))))
        (cond
         ((array? obj) (apply array-set! obj val (reverse l2)))
         ((hash-table? obj) (hashq-set! obj (car l2) val))
         (else (nx-error "indexed-set! on non-array, non-struct")))))
     ((integer? (car l3))
      (loop l1 (cons (car l3) l2) (cdr l3)))
     ((symbol? (car l3))
      (loop (cons (car l3) (append l2 l1)) '() (cdr l3)))
     (else
      (nx-error "expecting symbol or integer index, got: ~S" (car l3))))))
  
(define-public tsh:vtype
  (lambda (ary)
    (case (array-type ary)
      ((f64) 'flt)
      ((s32) 'int)
      ((#t) 'any))))

(define-public (tsh:struct . args)
  (throw 'nx-error "foo bar" '())
  (let loop ((sal '()) (rgl args))
    (cond
     ((null? rgl) (alist->hashq-table sal))
     ((null? (cdr rgl)) (nx-error "expecting even number"))
     ((symbol? (car rgl)) (loop (acons (car rgl) (cadr rgl) sal) (cddr rgl)))
     (else (error "expecting symbol")))))

(define-public tsh:isstruct (nx-C-predicate hash-table?))

(define-public tsh:box box)
(define-public tsh:unbox unbox)
(define-public tsh:isbox (nx-C-predicate box?))
(define-public tsh:setbox set-box!)

;; ====

(define (tsh:show_sxml)
  (set! (@@ (nyacc lang tsh compile-tree-il) show-sxml) #t))
(define (tsh:hide_sxml)
  (set! (@@ (nyacc lang tsh compile-tree-il) show-sxml) #f))
(define (tsh:show_xtil)
  (set! (@@ (nyacc lang tsh compile-tree-il) show-xtil) #t))
(define (tsh:hide_xtil)
  (set! (@@ (nyacc lang tsh compile-tree-il) show-xtil) #f))
    
;; === xdict

(define xdict
  `(
    ("puts" . ,(xlib-ref 'tsh:puts))
    ("format" . ,(xlib-ref 'tsh:format))
    ;;
    ("avec" . ,(xlib-ref 'tsh:avec))
    ("fvec" . ,(xlib-ref 'tsh:fvec))
    ("ivec" . ,(xlib-ref 'tsh:ivec))
    ("vlen" . ,(xlib-ref 'tsh:vlen))
    ("vtype" . ,(xlib-ref 'tsh:vtype))
    ;;
    ("struct" . ,(xlib-ref 'tsh:struct))
    ("isstruct" . ,(xlib-ref 'tsh:isstruct))
    ("box" . ,(xlib-ref 'tsh:box))
    ("unbox" . ,(xlib-ref 'tsh:unbox))
    ("isbox" . ,(xlib-ref 'tsh:isbox))
    ("setbox" . ,(xlib-ref 'tsh:setbox))
    ;; 
    ("show_sxml" . ,(xlib-ref 'tsh:show_sxml))
    ("hide_sxml" . ,(xlib-ref 'tsh:hide_sxml))
    ("show_xtil" . ,(xlib-ref 'tsh:show_xtil))
    ("hide_xtil" . ,(xlib-ref 'tsh:hide_xtil))
    ))

;; --- last line ---
