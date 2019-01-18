;;; nyacc/lang/calc/compiler

;; Copyright (C) 2015,2018,2019 Matthew R. Wette
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

;; This module provides two methods to compile `calc'S.  The first compiles
;; to Tree-IL, and then uses compile to get to CPS.  The second compiles to
;; CPS directly.  You can pick which implementation is used in the file
;; spec.scm which lives in the language/ directory.

;;; Code:

(define-module (nyacc lang calc compiler)
  #:export (compile-tree-il compile-cps))

(use-modules (sxml match))

(define show-code? #t)			; to show intermediate code in output

(use-modules (ice-9 pretty-print))
(define (pp exp) (pretty-print exp #:per-line-prefix "  "))
(define (sf fmt . args) (apply simple-format #t fmt args))


;; === Compile `calc' to Tree-IL ==============================================
 
(use-modules (language tree-il))
(use-modules (system base compile))
(use-modules (sxml fold))

(define (mkseq expr-list)
  (let loop ((xl expr-list))
    (if (null? xl) '((void))
	(cons* 'seq (car expr-list) (loop (cdr expr-list))))))

(define (show/til expr)
  `(seq (call (toplevel display) ,expr) (seq (call (toplevel newline)) (void))))

(define (fup-til tree)
  (sxml-match tree
    ((*TOP* ,tree) tree)
    ((assn-stmt ,assn) assn)
    ((expr-stmt ,expr) expr)
    ((empty-stmt) '(void))
    ((add ,lt ,rt) `(primcall + ,lt ,rt))
    ((sub ,lt ,rt) `(primcall - ,lt ,rt))
    ((mul ,lt ,rt) `(primcall * ,lt ,rt))
    ((div ,lt ,rt) `(primcall / ,lt ,rt))
    ((num ,nv) `(const ,(string->number nv)))
    ((ident ,name) `(toplevel ,(string->symbol name)))
    ((assn ,toplevel ,expr)
    `(seq (define ,(cadr toplevel) ,expr) ,toplevel))
    (,otherwise (error "missed" tree))))

(define (compile-tree-il exp env opts)
  (when show-code? (sf "SXML:\n") (pp exp))
  (let* ((xtil (foldt fup-til identity `(*TOP* ,exp))) ; external Tree-IL
	 (itil (parse-tree-il xtil)))		       ; internal Tree-IL
    (when show-code? (sf "Tree-IL:\n") (pp xtil))
    (values itil env env)))


;; === Compile `calc' to CPS. ==================================================

;; To try this fix the #:compiler key in ../../language/calc/spec.scm

;; unfinished work: use of variables doesn't yet work; 1 + 2 does

(use-modules (language cps))
(use-modules (language cps intmap))
(use-modules (language cps with-cps))
(use-modules (language cps utils))	;  counters
(use-modules (ice-9 match))

;; Thanks to Mark Weaver for helping on some of the concepts here.

;; The parameters @code{label-counter} and @code{var-counter} are defined
;; in @code{(cps utils)}; @code{compile-cps} uses also @code{scope-counter}.
;; Within the form @code{with-cps} procedures invoked by $ must return
;; @code{(values cps val)}.

;; sytab (subst in tree-il/compile-cps) is a map of (unique) symbol names
;; to cps variable index.  In tree-il the entries are (sym ix boxed?).

;; @deffn {Procedure} cnvt-arg cps exp kc => cps term
;; Convert an expression in the source language to an index.
;; As a side effect add the evaluation to the soup. (???)
;; The continuation @var{kc} here is a procedure @code{(kc cps vx)} that
;; takes the cps and associated variable index for the argument.
;; The coding style here is copied from tree-il/compile-cps.scm.
;; @end deffn
(define (cnvt-arg cps exp kc)
  (with-cps cps
    (letv arg)
    (let$ body (kc arg))
    (letk karg ($kargs ('arg) (arg) ,body))
    ($ (cnvt exp karg))))

;; @deffn {Procedure} cnvt-argl cps expl kc => cps term.
;; Convert a list of expressions in the source language to a list of (integer)
;; names.  As a side effect, generate a string of continuations in the cps to
;; evaluate the arguments and assign to the names.  The argument @var{kc} is a
;; continuation procedure of the form @code{(kc cps expl}} where @code{expl}
;; is the converted expression list.
;; @end deffn
(define (cnvt-argl cps expl kc)
  (match expl
    (() (kc cps '()))
    ((exp . expl)
     (cnvt-arg cps exp
       (lambda (cps name)
	 (cnvt-argl cps expl
	   (lambda (cps namel)
	     (kc cps (cons name namel)))))))))

;; @deffn {Procedure} mkrecv cps kx
;; @end deffn
(define (mkrecv cps kx)
  (with-cps cps
    (letv res)
    (letk ky ($kreceive (list res) 'rest kx))
    ky))

(define (mktop sym)
  (assq-ref '((add . +) (sub . -) (mul . *) (div . /)) sym))

;; @deffn {Procedure} cnvt cps exp kx
;; @var{kx} is the cps-index of the continuation for the expression.
;; @end deffn
(define (cnvt cps exp kx) ;; => (values cps term)
  (match exp
    (`(num ,value)
     (let ((numval (string->number value)))
       ;; Constants are legal expressions in $continue term.
       (values cps (build-term ($continue kx #f ($const numval))))))
    (`(ident ,name)
     (cnvt cps `(sym ,(string->symbol name)) kx))
    (`(sym ,sym)
     (with-cps cps
       ;; Here we create an identifier for symbol and #t, and then use
       ;; 'resolve to generate a boxed object for the top-level identifier.
       ($ (with-cps-constants ((sym sym) (t #t))
	      (build-term ($continue kx #f ($primcall 'resolve (sym t))))))))
    (`(unbox ,box)			; where box is a top-level var
     (with-cps cps
       (letv bx)			; ident returns var object
       (letk kc ($kargs ('bx) (bx) ($continue kx #f ($primcall 'box-ref (bx)))))
       ($ (cnvt box kc))))		; cnvt id to box and continue
    (((or 'add 'sub 'mul 'div) lt rt)
     ;; This is basically from tree-il/compile-cps.scm.
     ;; The $call must continue to a $ktail or $kreceive.
     (cnvt-argl cps (list `(unbox (sym ,(mktop (car exp)))) lt rt)
       (match-lambda*
	 ((cps (proc . args))
	  (call-with-values
	      (lambda () ;; If not a $ktail, then wrap in a $kreceive.
		(match (intmap-ref cps kx)
		  (($ $ktail) (values cps kx))
		  (else (mkrecv cps kx))))
	    (lambda (cps kx)
	      (with-cps cps
		(letv res)
		(build-term ($continue kx #f ($call proc args))))))))))
    (`(assn-stmt (assn (ident ,name) ,expr))
     (newline)
     (sf "name:\n") (pp name)
     (sf "expr:\n") (pp expr)
     (sleep 1)
     (newline)
     (error "not done yet"))
    (`(expr-stmt ,expr)
     (cnvt cps expr kx))
    (_ (error "missed" exp))))

(define (calc->cps exp) ;; => cps
  (parameterize ((label-counter 0) (var-counter 0))
    (with-cps empty-intmap
      (letv init)			; variable for closure (???)
      (letk kinit ,#f)			; reserve ix 0 for program start
      (letk ktail ($ktail))		; like halt in Kennedy paper ?
      (let$ body (cnvt exp ktail))	; term for @var{exp}
      (letk kbody ($kargs () () ,body))	; it's corresponding continuation
      (letk kclause			; thunk clause for program
	    ($kclause ('() '() #f '() #f) kbody #f))
      ($ ((lambda (cps)
	    (let ((init (build-cont ($kfun #f '() init ktail kclause))))
	      ;; Set the initial thunk-continuation at index 0 and return
	      ;; a persistent cps.  (See wingolog blog post on intmaps.)
	      (persistent-intmap (intmap-replace! cps kinit init)))))))))

(define (showit cps)
  (for-each
   (lambda (x) (sf "  ~S\n" x))
   (intmap-fold-right acons cps '())))

(define (compile-cps exp env opts)
  (when show-code? (sf "SXML:\n") (pp exp))
  (let* ((code (calc->cps exp)))
    (when show-code? (sf "CPS:\n") (showit code))
    (values code env env)))

;; Local Variables:
;; eval: (put 'cnvt-arg 'scheme-indent-function 2)
;; eval: (put 'cnvt-argl 'scheme-indent-function 2)
;; End:

;; --- last line ---
