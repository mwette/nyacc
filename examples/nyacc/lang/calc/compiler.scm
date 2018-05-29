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

(define-module (nyacc lang calc compiler)
  #:export (compile-tree-il
	    compile-cps
	    ))

(use-modules (ice-9 pretty-print))
(define pp pretty-print)
(define (sf fmt . args) (apply simple-format #t fmt args))


;; === using tree-il ===========================================================

(use-modules (sxml match))
(use-modules (sxml fold))
(use-modules (language tree-il))
(use-modules (ice-9 pretty-print))
(use-modules (system base compile))


(define (mkseq expr-list)
  (let iter ((xl expr-list))
    (if (null? xl) '((void))
	(cons* 'seq (car expr-list) (iter (cdr expr-list))))))

(define (show expr)
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
    ((assn-stmt ,lhs ,rhs) (mkseq `((define ,lhs ,rhs) ,(show lhs))))
    ((expr-stmt ,expr) (show expr))
    ((empty-stmt) '(void))
    ((program . ,stmt-list) (mkseq stmt-list))
    (,otherwise tree)))


(define (compile-tree-il exp env opts)
  (let* ((tree (foldt fup-til identity exp))
 	 (code (parse-tree-il tree))
	 (cps (compile code #:from 'tree-il #:to 'cps))
	 )
    ;;(pp tree)
    ;;(showit cps)
    (values code env env)))


;; === using cps ===============================================================

(use-modules (language cps))
(use-modules (language cps intmap))
(use-modules (language cps with-cps))
(use-modules (language cps utils))	;  counters
(use-modules (ice-9 match))

;; dcl tracks decls and scope
;; stk is the continuation stack
;; cps is the CPS intmap

;; (program (expr-stmt (add (fixed "2") (fixed "2"))))

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
      ((add ,lt ,rt)
       #f)
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


  (let* (;;(cps empty-intmap)
	 ;;(code (foldts*-values fD-cps fU-cps fH-cps exp '() cps))
	 (code (call-with-values doit
		 (lambda (cps val) (showit cps) val)))
	 )
    (values code env env)))

(use-modules (nyacc lang calc parser))

(define (showit cps)
  (for-each
   (lambda (x) (sf "~S\n" x))
   (intmap-fold-right acons cps '())))

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

;; @var{k} is the cps-index of the continuation for the expression.
(define (cnvt cps exp kx) ;; => cps term
  ;;(sf "cnvt: ") (pp exp)
  (match exp
    (`(program . ,stmts) (cnvt cps (car stmts) kx)) ;; ONLY eval the 1st one
    (`(expr-stmt ,expr) (cnvt cps expr kx))
    (`(fixed ,value)
     (let ((numval (string->number value)))
       (values cps (build-term ($continue kx #f ($const numval))))))
    (`(ident ,name)
     (with-cps cps
       ($ (with-cps-constants ((name name) (t #t))
	      (build-term ($continue kx #f ($primcall 'resolve (name t))))))))
    (`(add ,lt ,rt)
     (cnvt cps lt
	   (lambda (cps exp)
	     (cnvt 
    (`(x-add ,lt ,rt)
     (cnvt-argl cps (list '(ident +) lt rt)
       (match-lambda*
	 ((cps (proc . args))
	  (with-cps cps
	    (build-term ($continue kx #f ($call proc args))))))))
     
    (else (error "missed" exp))))

(define (calc->cps exp) ;; => cps cont
  (parameterize ((label-counter 0) (var-counter 0))
    (with-cps empty-intmap
      (letv init rval)			; rval: return value
      (letk kinit ,#f)			; program start: always zero
      (letk ktail ($ktail))		; like halt in Kennedy paper ?
      (letk kwrap			; wrapper for returned value
	    ($kargs ('rval) (rval) ($continue ktail #f ($values (rval)))))
      (let$ body (cnvt exp kwrap))	; term for tree
      (letk kbody ($kargs () () ,body))	; cont for tree
      (letk kclause			; thunk for program
	    ($kclause ('() '() #f '() #f) kbody #f))
      ($ ((lambda (cps)
	    (let ((init (build-cont ($kfun #f '() init ktail kclause))))
	      (with-cps (persistent-intmap (intmap-replace! cps kinit init))
		kinit))))))))

(define (gen-cps exp)
  (let ((tree (foldt fup-til identity exp)))
    (compile (parse-tree-il tree) #:from 'tree-il #:to 'cps)))

(define (compile-cps exp env opts)
  (pp exp)
  (let* ((code (call-with-values
		   (lambda () (calc->cps exp))
		 (lambda (cps val) (showit cps) cps))))
    ;;(showit (gen-cps exp))
    (values code env env)))


;; This is a near-minimal cps program.  It returns the constant 99.
(define (doit)
  (parameterize ((label-counter 0) (var-counter 0))
    (with-cps empty-intmap
      (letv init)
      (letk kinit ,#f)
      (letk ktail ($ktail))
      (let$ body (with-cps-constants ((val 99))
      		     (build-term ($continue ktail #f ($values (val))))))
      (letk kbody ($kargs () () ,body))
      (letk kclause ($kclause ('() '() #f '() #f) kbody #f))
      ($ ((lambda (cps)
	    (let ((init (build-cont ($kfun #f '() init ktail kclause))))
	      (with-cps (persistent-intmap (intmap-replace! cps kinit init))
		kinit))))))))

;;(showit (exec-cps-exp '($const 1)))
;;(define (gen-bcode cps) (compile cps #:from 'cps #:to 'bytecode))
;;(gen-bcode (exec-cps-exp '($const 1)))
;;(compile (doit) #:from 'cps #:to 'bytecode)
;;(showit (doit))
;;(define cal-tree (with-input-from-string "2 + 2\n" calc-parse)) (pp cal-tree)

;; Local Variables:
;; eval: (put 'cnvt-arg 'scheme-indent-function 2)
;; eval: (put 'cnvt-argl 'scheme-indent-function 2)
;; End:


;; --- last line ---
