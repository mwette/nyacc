;;; nyacc/lang/calc/compiler

;; Copyright (C) 2015,2018 Matthew R. Wette
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

;; This module provides two methods to compile to CPS.  The first compiles
;; to Tree-IL, and then uses compile to get to CPS.  The second compiles to
;; CPS directly.

(define-module (nyacc lang calc compiler)
  #:export (compile-cps))

(define show-code? #t)			; to show intermediate code in output

(use-modules (ice-9 pretty-print))
(define pp pretty-print)
(define (sf fmt . args) (apply simple-format #t fmt args))

(define-syntax-rule (if-true form ...) (begin form ...))
(define-syntax-rule (if-false form ...) (begin))


(if-false
 ;; === using tree-il =========================================================
 
 (use-modules (sxml match))
 (use-modules (sxml fold))
 (use-modules (language tree-il))
 (use-modules (system base compile))


 (define (mkseq expr-list)
   (let iter ((xl expr-list))
     (if (null? xl) '((void))
	 (cons* 'seq (car expr-list) (iter (cdr expr-list))))))

 (define (show/til expr)
   `(seq (call (toplevel display) ,expr) (seq (call (toplevel newline)) (void))))

 (define (fup-til tree)
   ;;(simple-format #t "~S\n" tree)
   (sxml-match tree
     ((fixed ,fx) `(const ,(string->number fx)))
     ((float ,fl) `(const ,(string->number fl)))
     ((ident ,id) `(toplevel ,(string->symbol id)))
     ((add ,lt ,rt) `(call (toplevel +) ,lt ,rt))
     ((sub ,lt ,rt) `(call (toplevel -) ,lt ,rt))
     ((mul ,lt ,rt) `(call (toplevel *) ,lt ,rt))
     ((div ,lt ,rt) `(call (toplevel /) ,lt ,rt))
     ((assn-stmt ,lhs ,rhs) (mkseq `((define ,lhs ,rhs) ,(show/til lhs))))
     ((expr-stmt ,expr) (show/til expr))
     ((empty-stmt) '(void))
     ((program . ,stmt-list) (mkseq stmt-list))
     (,otherwise tree)))

 (define (compile-cps exp env opts)
   (if show-code? (pp exp))
   (let* ((tree (foldt fup-til identity exp))
	  (til (parse-tree-il tree))
	  (cps (compile til #:from 'tree-il #:to 'cps)))
     (if show-code? (pp tree))
     (values cps env env)))

 )


(if-true
 ;; === using cps =============================================================

 (use-modules (language cps))
 (use-modules (language cps intmap))
 (use-modules (language cps with-cps))
 (use-modules (language cps utils))	;  counters
 (use-modules (ice-9 match))

 ;; dcl tracks decls and scope
 ;; stk is the continuation stack
 ;; cps is the CPS intmap

 ;; (program (expr-stmt (add (fixed "2") (fixed "2"))))

 (define (show/cps exp kx)
   ;; (display exp) (newline)
   #f)

 (use-modules (sxml match))
 
 (define (x-compile-cps exp env opts)

   (define (fD-cps tree stk cps)
     (sxml-match tree
       ((fixed ,val) (values '() stk (build-exp ($const val))))
       (,otherwise (values tree stk cps))))

   (define (fU-cps tree stk cps)
     ;;(simple-format #t "~S\n" tree)
     (sxml-match tree
       ((fixed ,fx) `(const ,(string->number fx)))
       ((float ,fl) `(const ,(string->number fl)))
       ((ident ,id) `(toplevel ,(string->symbol id)))
       ((add ,lt ,rt) #f)
       ((sub ,lt ,rt) `(call (toplevel -) ,lt ,rt))
       ((mul ,lt ,rt) `(call (toplevel *) ,lt ,rt))
       ((div ,lt ,rt) `(call (toplevel /) ,lt ,rt))
       ;;((assn-stmt ,lhs ,rhs) (mkseq `((define ,lhs ,rhs) ,(show lhs))))
       ;;((expr-stmt ,expr) (show expr))
       ((empty-stmt) '(void))
       ((program . ,stmt-list)
	#f)
       (,otherwise tree)))

   (define (fH-cps leaf stk cps)
     (match leaf
       ('add '())
       (#t cps)
       )
     (values stk cps))


   (let* (;;(code (foldts*-values fD-cps fU-cps fH-cps exp '() empty-intmap))
	  (code empty-intmap)
	  )
     (values code env env)))

 (use-modules (nyacc lang calc parser))

 (define (showit cps)
   (for-each
    (lambda (x) (sf "~S\n" x))
    (intmap-fold-right acons cps '())))

 ;; Thanks to Mark Weaver for help on this stuff.

 ;; The parameters label-counter and var-counter are needed by cps, I guess.
 ;; compile-cps uses also scope-counter
 ;; with-cps the procedure invoked by $ must (values cps val)

 ;; sytab (subst in tree-il/compile-cps) is a map of (unique) symbol names
 ;; to cps variable index.  In tree-il the entries are (sym ix boxed?).

 ;; Convert an expression in the source language to an index
 ;; As a side effect add the evaluation to the soup xxx
 ;; The continuation @var{kc} here is a procedure @code{(kc cps vx)} that
 ;; takes the cps and associated variable index for the argument.
 ;; (code format swipped from tree-il/compile-cps.scm)
 (define (cnvt-arg cps exp kc)
   (with-cps cps
     (letv arg)
     (let$ body (kc arg))
     (letk karg ($kargs ('arg) (arg) ,body))
     ($ (cnvt exp karg))))

 ;; Convert a list of expressions in the source language to a list of
 ;; (integer) names.  As a side effect, generate a string of continuations
 ;; in the cps to evaluate the arguments and assign to the names.  The
 ;; continuation @var{kc} is a procedure @code{(kc cps expl}} that takes
 ;; ...
 ;; (code format swipped from tree-il/compile-cps.scm)
 (define (cnvt-argl cps expl kc)
   (match expl
     (() (kc cps '()))
     ((exp . expl)
      (cnvt-arg cps exp
	(lambda (cps name)
	  (cnvt-argl cps expl
	    (lambda (cps namel)
	      (kc cps (cons name namel)))))))))

 (define (mktop sym) (assq-ref '((add . +) (sub . -) (mul . *) (div . /)) sym))

 (define (mkrecv cps kx)
   (with-cps cps
     (letv res)
     (letk ky ($kreceive (list res) 'rest kx))
     ky))

 ;; @var{kx} is the cps-index of the continuation for the expression.
 (define (cnvt cps exp kx) ;; => cps term
   ;;(sf "cnvt: ") (pp exp)
   (match exp
     (`(program . ,stmts)
      ;; For now we only eval the first expression in a line and don't display.
      (cnvt cps (car stmts) kx))
     (`(expr-stmt ,expr)
      (case (car expr)
	((fixed float)
	 (with-cps cps
	   (letv val)
	   (letk kwrap ($kargs ('val) (val) ($continue kx #f ($values (val)))))
	   ($ (cnvt expr kwrap))))
	(else
	 (cnvt cps expr kx))))
     (`(fixed ,value)
      (let ((numval (string->number value)))
	;; Constants are legal expressions in $continue term.
	(values cps (build-term ($continue kx #f ($const numval))))))
     (`(ident ,name)
      (with-cps cps
	;; Here we create an identifier for name and #t, and then use
	;; 'resolve to generate a boxed object for the top-level identifier.
	($ (with-cps-constants ((name name) (t #t))
	     (build-term ($continue kx #f ($primcall 'resolve (name t))))))))
     (`(unbox ,box)			; where box is a top-level var
      (with-cps cps
	(letv bx)			; ident returns var object
	(letk kc ($kargs ('bx) (bx) ($continue kx #f ($primcall 'box-ref (bx)))))
	($ (cnvt box kc))))		; cnvt id to box and continue
     (((or 'add 'sub 'mul 'div) lt rt)
      ;; This is basically ripped-off from tree-il/compile-cps.scm.
      ;; The $call must continue to a $ktail or $kreceive.
      (cnvt-argl cps (list `(unbox (ident ,(mktop (car exp)))) lt rt)
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
     
     (`(xadd ,lval ,rval)
      ;; illustrate use of primcall 
      (with-cps cps
	(letv lv rv)
	(let$ opb ($continue kx #f ($primcall 'add lv rv)))
	(letk opk ($kargs ('lv 'rv) (lv rv) ,opb))
	(values cps #f)
	))
     (else (error "missed" exp))))

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


 (define (compile-cps exp env opts)
   (if show-code? (pp exp))
   (let* ((code (calc->cps exp)))
     (if show-code? (showit code))
     (values code env env)))

 )

;; Local Variables:
;; eval: (put 'cnvt-arg 'scheme-indent-function 2)
;; eval: (put 'cnvt-argl 'scheme-indent-function 2)
;; End:

;; --- last line ---
