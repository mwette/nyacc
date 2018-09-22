;;; nyacc/lang/tcl/xlib.scm

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

;;; Notes:

;; 1) expr needs exp (i.e., ** as in 2**4 => 16)

;;; Code:

(define-module (nyacc lang tcl xlib)
  #:export (xdict xlib-ref tcl-eval)
  #:use-module (rnrs arithmetic bitwise)
  )
(use-modules (ice-9 pretty-print))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr exp) (pretty-print exp (current-error-port)))

;; Evaluate expression (a string)
(define* (tcl-eval expr #:optional (env (current-module)))
  #f
  ;;(eval-string expr #:lang 'nx-tcl)
  )

(define (xlib-ref name) `(@@ (nyacc lang tcl xlib) ,name))

;; expr evaluator

(use-modules (nyacc lalr))
(use-modules (nyacc lex))
(use-modules (nyacc parse))

(include-from-path "nyacc/lang/tcl/mach.d/expr-act.scm")
(include-from-path "nyacc/lang/tcl/mach.d/expr-tab.scm")

(define expr-lexr ((make-lexer-generator tcl-expr-mtab)))
  
(define raw-parser
  (make-lalr-parser
   (list (cons 'act-v tcl-expr-act-v) (cons 'len-v tcl-expr-len-v)
	 (cons 'pat-v tcl-expr-pat-v) (cons 'rto-v tcl-expr-rto-v)
	 (cons 'mtab tcl-expr-mtab))))

(define (parse-expr-string str)
  (with-input-from-string str
    (lambda ()
      (catch 'nyacc-error
	(lambda ()
	  (raw-parser expr-lexr #:debug #f))
	(lambda (key fmt . args)
	  (apply simple-format (current-error-port) fmt args))
	#f))))

(define sx-ref list-ref)

(define (eval-expr tree)
  (letrec
      ((tx (lambda (tr ix) (sx-ref tr ix)))
       (tx1 (lambda (tr) (sx-ref tr 1)))
       (ev (lambda (ex ix) (eval-expr (sx-ref ex ix))))
       (ev1 (lambda (ex) (ev ex 1)))	; eval expr in arg 1
       (ev2 (lambda (ex) (ev ex 2)))	; eval expr in arg 2
       (ev3 (lambda (ex) (ev ex 3)))	; eval expr in arg 3
       (eval-expr
	(lambda (tree)
	  (case (car tree)
	    ((fixed) (string->number (cnumstr->scm (tx1 tree))))
	    ((float) (string->number (cnumstr->scm (tx1 tree))))
	    ((string) (sx-ref tree 1))
	    ((ident) (sx-ref tree 1))
	    ((pre-inc post-inc) (1+ (ev1 tree)))
	    ((pre-dec post-dec) (1- (ev1 tree)))
	    ((pos) (ev1 tree))
	    ((neg) (- (ev1 tree)))
	    ((not) (if (zero? (ev1 tree)) 1 0))
	    ((mul) (* (ev1 tree) (ev2 tree)))
	    ((div) (/ (ev1 tree) (ev2 tree)))
	    ((mod) (modulo (ev1 tree) (ev2 tree)))
	    ((add) (+ (ev1 tree) (ev2 tree)))
	    ((sub) (- (ev1 tree) (ev2 tree)))
	    ((lshift) (bitwise-arithmetic-shift-left (ev1 tree) (ev2 tree)))
	    ((rshift) (bitwise-arithmetic-shift-right (ev1 tree) (ev2 tree)))
	    ((lt) (if (< (ev1 tree) (ev2 tree)) 1 0))
	    ((le) (if (<= (ev1 tree) (ev2 tree)) 1 0))
	    ((gt) (if (> (ev1 tree) (ev2 tree)) 1 0))
	    ((ge) (if (>= (ev1 tree) (ev2 tree)) 1 0))
	    ((eq) (if (= (ev1 tree) (ev2 tree)) 1 0))
	    ((ne) (if (= (ev1 tree) (ev2 tree)) 0 1))
	    ((bitwise-not) (lognot (ev1 tree)))
	    ((bitwise-or) (logior (ev1 tree) (ev2 tree)))
	    ((bitwise-xor) (logxor (ev1 tree) (ev2 tree)))
	    ((bitwise-and) (logand (ev1 tree) (ev2 tree)))
	    ((or) (if (and (zero? (ev1 tree)) (zero? (ev2 tree))) 0 1))
	    ((and) (if (or (zero? (ev1 tree)) (zero? (ev2 tree))) 0 1))
	    ((cond-expr) (if (zero? (ev1 tree)) (ev3 tree) (ev2 tree)))
	    (else (error "incomplete expr implementation" tree))))))
    (eval-expr tree)))

(define-public (tcl:word . args)
  (apply string-append (map tcl:any->str args)))

;; @deffn {Procedure} tcl:expr frags
;; @var{frags} is a list of string fragments.  We join, parse and execute.
;; @end deffn
(define-public (tcl:expr . frags)
  (let* ((strs (map tcl:any->str frags))
	 (xarg (apply string-append strs))
	 (tree (parse-expr-string xarg))
	 (xval (eval-expr tree)))
    xval))


;; @deffn {Procedure} tcl:list arg ...
;; This creates a tcl list.
;; @end deffn
(define-public (tcl:list . args)
  args)

;; === (associative) arrays 

;; arrays are what set abc(foo) mean
;; they are apparently ordered

;; @deffn {Procedure} tcl:make-array name
;; Make an array.  In Tcl this actually takes an argument and would add
;; to the current scope.  To do that this would need to look like
;; @example
;; (tcl:make-array dict name)
;; @end example
;; @end deffn
(define-public (tcl:make-array)
  (make-hash-table))

;; @deffn {Procedure} tcl:array-get name index
;; Get value from the array.  What if not there?
;; The argument @var{index} will be converted to a symbol.@*
;; Note: What if it's an integer (e.g., @code{1}, or then @code{"1"}).
;; @end deffn
(define-public (tcl:array-get name index)
  (let ((key (if (string? index) (string->symbol index) index)))
    (hashq-ref name key)))

;; @deffn {Procedure} tcl:array-set name index value
;; @end deffn
(define-public (tcl:array-set1 name index value)
  (let ((key (if (string? index) (string->symbol index) index)))
    (hashq-set! name key value)))

;;(define-public (ztcl:array-set env name value)
;;  (let ((key (if (string? index) (string->symbol index) index)))
;;    (hashq-set! name key value)))

;; ===================================

(define (tcl:list->string tcl-list)
  (map (lambda (elt)
	 (let ((str (tcl:any->str elt)))
	   (if (string-any #\space str) (string-append "{" str "}") str)))
       tcl-list))

;; @deffn {Procedure} tcl:any->str [value] [index]
;; Convert value to string.
;; @end deffn
(define-public tcl:any->str
  (case-lambda
    ((val)
     (cond
      ((string? val) val)
      ((number? val) (number->string val))
      ((list? val) (tcl:list->string val))
      ;;((vector? val) ...
      (else (simple-format #f "~A" val))))
     ((val index)
      (error "indexed deref not implemented")
      )))

(define-public tcl:puts
  (case-lambda
   ((val) (display (tcl:any->str val)) (newline))
   ((arg0 val)
    (if (string=? arg0 "-nonewline")
	(display val)
	(display val arg0)))		; broken need arg0 => port
   ((nnl chid val)
    (unless (string=? nnl "-nonewline") (throw 'tcl-error "puts: bad arg"))
    "(not implemented)"
    )))


;; === xdict

(define xdict
  `(
    ("puts" . ,(xlib-ref 'tcl:puts))
     ))

;; --- last line ---
