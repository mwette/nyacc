;;; nyacc/lang/tsh/compile-tree-il.scm - compile tclish sxml to tree-il

;; Copyright (C) 2021 Matthew R. Wette
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Notes:

;; 1) Derived from tcl/compile-tree-il.scm.

;;; Todo:

;; 1) clean up fD handling of set

;;; Code:

(define-module (nyacc lang tsh compile-tree-il)
  #:export (compile-tree-il show-tsh-sxml show-tsh-xtil)
  #:use-module (nyacc lang tsh xlib)
  #:use-module (nyacc lang nx-util)
  #:use-module (nyacc lang sx-util)
  #:use-module ((sxml fold) #:select (foldts*-values))
  #:use-module ((srfi srfi-1) #:select (fold fold-right append-reverse))
  #:use-module (language tree-il)
  #:use-module (ice-9 match)
  ;;#:use-module (system base compile)
  )
(use-modules (ice-9 pretty-print))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr tree)
  (pretty-print tree (current-error-port) #:per-line-prefix "  "))

(define xlib-mod '(nyacc lang tsh xlib))
(define xlib-module (resolve-module xlib-mod))
(define (xlib-ref name) `(@@ (nyacc lang tsh xlib) ,name))

;; scope must be manipulated at execution time
;; the @code{proc} command should push-scope
(define push-scope nx-push-scope)
(define pop-scope nx-pop-scope)
(define top-level? nx-top-level?)
(define add-toplevel nx-add-toplevel)
(define add-lexical nx-add-lexical)
(define add-lexicals nx-add-lexicals)
(define add-variable nx-add-variable)
(define (lookup name dict)
  (or (nx-lookup name dict)
      (nx-lookup-in-env name xlib-module)))

(define make-opcall (opcall-generator xlib-mod))

;; @deffn {Procedure} make-arity args
;; This procedure generates a tree-il arity part of a lambda-case.
;; @end deffn
(define (X-make-arity args)
  (let loop ((req '()) (opt '()) (rest #f) (inits '()) (gsyms '()) (args args))
    (if (null? args)
	(list (reverse req) (reverse opt) rest #f
	      (reverse inits) (reverse gsyms))
	(let* ((arg (car args))
	       (lref (cadr arg)) (var (cadr lref)) (sym (caddr lref)))
	  (case (car arg)
	    ((arg)
	     (loop (cons var req) opt rest inits (cons sym gsyms) (cdr args)))
	    ((opt-arg)
	     (loop req (cons var opt) rest (cons (caddr arg) inits)
		   (cons sym gsyms) (cdr args)))
	    ((rest)
	     (loop req opt var inits (cons sym gsyms) (cdr args)))
	    (else (error "coding error")))))))

(define (opcall-node op seed kseed kdict)
  (values (cons (rev/repl 'call (xlib-ref op) kseed) seed) kdict))

;; for lt + rt, etc
(define (op-call op kseed)
  (rev/repl 'call (xlib-ref op) kseed))
(define (op-call/prim op kseed)
  (rev/repl 'prim-call op kseed))

(define (make-function name arity body)
  (let* ((meta '((language . nx-tsh)))
	 (meta (if name (cons `(name . ,name) meta) meta)))
    `(lambda ,meta (lambda-case (,arity ,body)))))

;; @deffn {Procedure} sxml->xtil exp env opts
;; Compile SXML tree to external Tree-IL representation.
;; @end deffn
(define-public (sxml->xtil exp env opts)

  (define (fD tree seed dict) ;; => tree seed dict
    (when #f
      (sferr "fD: ~S\n" tree)
      )
    (sx-match tree

      ((string ,sval)
       (values '() `(const ,sval) dict))

      ((float ,sval)
       (values '() `(const ,(string->number sval)) dict))

      ((fixed ,sval)
       (values '() `(const ,(string->number sval)) dict))

      #;((symbol ,sval)
       (values '() `(const ,(string->symbol sval)) dict))
      ((ident ,sval)
      (values '() `(const ,(string->symbol sval)) dict))

      ((eval . ,stmts)
       (values tree '() (add-lexical "return" (push-scope dict))))
      
      ((deref ,name)
       (let ((ref (lookup name dict)))
	 (unless ref (throw 'tcl-error "undefined variable: ~A" name))
	 (values '() ref dict)))

      ((deref-indexed ,name ,expr-list)
       (let ((ref (lookup name dict)))
	 (unless ref (throw 'tsh-error "undefined variable: ~A" name))
	 (values '() `(call ,(xlib-ref 'tsh:array-ref) ,ref ,expr-list) dict)))

      ((switch . ,stmts)
       (values tree '() (add-lexicals "swx~val" "break" (push-scope dict))))
      
      ((proc (ident ,name) (arg-list . ,args) ,body)
       ;; replace each name with (lexical name gsym)
       (let* ((dict (add-variable name dict))
	      (nref (lookup name dict))
	      (dict (push-scope dict))
	      ;; clean this up
	      (dict (fold (lambda (a d) (add-lexical (cadadr a) d)) dict args))
	      (args (fold-right ;; replace arg-name with lexical-ref
		     (lambda (a l)
		       (cons (cons* (car a) (lookup (cadadr a) dict) (cddr a))
			     l)) '() args))
	      (dict (add-lexical "return" dict))
	      (dict (acons '@F name dict)))
	 (values `(proc ,nref (arg-list . ,args) ,body) '() dict)))

      ((incr (ident ,var) ,val)
       (values `(incr ,var ,val) '() dict))
      ((incr (ident ,var))
       (values `(incr ,var (const 1)) '() dict))
      
      ((set (ident ,name) ,value)
       ;; todo-1: clean this up  NOT RIGHT, frame?
       (let* ((nref (lookup name dict))
	      (dict (if nref dict (or (nx-ensure-in-frame name dict)
				      (add-variable name dict))))
	      (nref (or nref (lookup name dict))))
	 (values `(set ,nref ,value) '() dict)))
      ;;((set otherwise could be ugly

      ((set-indexed (ident ,name) ,index ,value)
       (let* (;;(dict (add-variable name dict))
	      (nref (lookup name dict)))
	 (values `(set-ix ,nref ,index ,value) '() dict)))

      ((call (ident ,name) . ,args)
       (let ((ref (lookup name dict)))
	 (unless ref (throw 'tsh-error "not defined"))
	 (values `(call ,ref . ,args) '() dict)))

      ((script . ,stmts)
       (values tree '() (add-lexical "return" (push-scope dict))))
      
      ;; don't process resolved references
      ((@@ ,module ,symbol)
       (values '() tree dict))

      ((empty-stmt)
       (values '() '(void) dict))

      (,_
       ;;(sferr "fD: default\n") (pperr tree) (sferr "\n")
       (values tree '() dict))))

  (define (fU tree seed dict kseed kdict) ;; => seed dict
    (when #f
      ;;(sferr "fU: ~S\n" (if (pair? tree) (car tree) tree))
      ;;(sferr "    kseed=~S\n    seed=~S\n" kseed seed)
      (sferr "fU: ~S, tree, kseed, seed\n" (if (pair? tree) (car tree) tree))
      (pperr tree) (pperr kseed) (pperr seed)
      (sferr "\n")
      ;;(pperr tree)
      )
    ;; This routine rolls up processes leaves into the current branch.
    ;; We have to be careful about returning kdict vs dict.
    ;; Approach: always return kdict or (pop-scope kdict)
    (if
     (null? tree) (if (null? kseed)
		      (values seed kdict) 
		      (values (cons kseed seed) kdict))
     
     (case (car tree)

       ;; before leaving add a call to make sure all toplevels are defined
       ((*TOP*)
	(let ((tail (rtail kseed)))
	  (cond
	   ((null? tail) (values '(void) kdict)) ; just comments
	   (else (values (car kseed) kdict)))))

       ((script)
	(values (cons (block (rtail kseed)) seed) kdict))

       ((stmt-list)
	(values (cons (block (rtail kseed)) seed) kdict))

       ((comment)
	(values seed kdict))

       ((proc)
	(sferr "proc (reverse kseed):\n") (pperr (reverse kseed))
	(let* ((tail (rtail kseed))
	       (name-ref (list-ref tail 0))
	       (argl (list-ref tail 1))
	       (body (block (list-tail tail 2)))
	       (ptag (lookup "return" kdict))
	       (arity (make-arity argl))
	       ;; add lexicals : CLEAN THIS UP -- used in nx-octave also
	       (lvars (let loop ((ldict kdict))
			(if (eq? '@F (caar ldict)) '()
			    (cons (cdar ldict) (loop (cdr ldict))))))
	       (body (let loop ((nl '()) (ll '()) (vl '()) (vs lvars))
		       (if (null? vs)
			   `(let ,nl ,ll ,vl ,body)
			   (loop
			    (cons (list-ref (car vs) 1) nl)
			    (cons (list-ref (car vs) 2) ll)
			    (cons '(void) vl)
			    (cdr vs)))))
	       ;;
	       (body (with-escape/arg ptag body))
	       (fctn (make-function (cadr name-ref) arity body))
	       (stmt (if (eq? 'toplevel (car name-ref))
			 `(define ,(cadr name-ref) ,fctn)
			 `(set! ,name-ref ,fctn))) ;; never used methinks
	       )
	  ;;(sferr "proc ~S:\n" name-ref) (pperr tail) (pperr fctn)
	  (values (cons stmt seed) (pop-scope kdict))))

       ((return)
	(let ((ret `(abort ,(lookup "return" kdict)
			   (,(if (> (length kseed) 1) (car kseed) '(void)))
			   (const ()))))
	  (values (cons ret seed) kdict)))

       ((X-arg-list)
	(sferr "arg-list:\n") (pperr (reverse kseed)) (quit)
	)

       ;; conditional: elseif and else are translated by the default case
       ((if)
	(let* ((tail (rtail kseed))
	       (cond-expr `(primcall not (primcall zero? ,(list-ref tail 0))))
	       (then-expr (list-ref tail 1))
	       (rest-part (list-tail tail 2))
	       (rest-expr
		(let loop ((rest-part rest-part))
		  (match rest-part
		    ('() '(void))
		    (`((else ,body)) (block body))
		    (`((elseif ,cond-part ,body-part) . ,rest)
		     `(if (primcall not (primcall zero? ,cond-part))
			  ,body-part
			  ,(loop (cdr rest-part)))))))
	       (stmt `(if ,cond-expr ,then-expr ,rest-expr)))
	  (values (cons stmt seed) kdict)))
       ((elseif else)
	(values (cons (reverse kseed) seed) kdict))

       ((switch)
	(let ((val (lookup "swx~val" kdict)))
	  (values
	   (cons
	    (identity ;; pk
	     (if (eq? (caar kseed) 'default)
		 (make-switch val (cdr kseed) (car kseed))
		 (make-switch val kseed '(void)))
	     )
	    seed)
	   (pop-scope kdict))))
       
       ((case)
	(values
	 (if (and (pair? seed) (eq? (caar seed) 'default))
	     (cons* (car seed) (reverse kseed) (cdr seed)) ; default in front
	     (cons (reverse kseed) seed))
	 kdict))
	
       ;; for allows continue and break
       ((for)
	(sferr "todo: for\n")
	(values (cons '(void) seed) kdict))

       ((while)
	(let* ((cond-expr `(primcall not (primcall zero? ,(list-ref kseed 1))))
	       (body-part (list-ref kseed 0))
	       (while-exp (make-while cond-expr body-part kdict)))
	  (values (cons while-exp seed) kdict)))

       ((set)
	(values
	 (let* ((value (car kseed))
		(nref (cadr kseed))
		(toplev? (eq? (car nref) 'toplevel)))
	   (cons (if toplev?
		     `(define ,(cadr nref) ,value)
		     `(set! ,nref ,value))
		 seed))
	 kdict))

       ((set-indexed)
	;; This only works if the variable appeared as string constant in fD.?
	(let* ((value (car kseed))
	       (indx (cadr kseed))
	       (nref (caddr kseed))
	       (toplev? (eq? (car nref) 'toplevel)))
	  (values
	   (cons
	    `(seq
	      ,(if toplev?
		   (make-defonce (cadr nref) `(call ,(xlib-ref 'tsh:make-array)))
		   `(set! ,nref (if (call (toplevel hash-table?) ,nref) ,nref
				    `(call ,(xlib-ref 'tsh:make-array)))))
	      (call ,(xlib-ref 'tsh:array-set1) ,nref ,indx ,value))
	    seed) kdict)))

       #;((body)
	(values (cons (block (rtail kseed)) seed) kdict))
       
       ((call)
	(values (cons `(call . ,(rtail kseed)) seed) kdict))

       ((eval)
	(let ((body (with-escape/arg (lookup "return" kdict) (car kseed))))
 	  (values (cons body seed) (pop-scope kdict))))

       ((void)
	(values (cons '(void) seed) kdict))

       ;; others to add: incr foreach while continue break
       ((incr)
	(let* ((tail (rtail kseed))
	       (name (car tail))
	       (expr (cadr tail))
	       (vref (lookup (car tail) kdict))
	       (stmt `(set! ,vref (primcall + ,vref ,expr))))
	  (values (cons stmt seed) kdict)))

       ((source)
	(let ((stmt `(call (@@ (nyacc lang tsh xlib) tsh:source)
			   ,(car kseed)
			   ;;(call (toplevel current-module))
			   )))
	  (values (cons stmt seed) kdict)))

       ((format)
	(let* ((tail (rtail kseed))
	       (stmt `(call (@@ (nyacc lang nx-lib) sprintf) . ,tail)))
	  (values (cons stmt seed) kdict)))

       ((expr)
	;;(sferr "expr:~S\n" kseed)
	(values (cons (car kseed) seed) kdict))

       ;; pos neg ~ not
       ((pos) (opcall-node 'tsh:pos seed kseed kdict))
       ((neg) (opcall-node 'tsh:neg seed kseed kdict))
       ((lognot) (opcall-node 'tsh:lognot seed kseed kdict))
       ((not) (opcall-node 'tsh:not seed kseed kdict))

       ;; mul div mod add sub
       ((mul) (opcall-node 'tsh:* seed kseed kdict))
       ((div) (opcall-node 'tsh:/ seed kseed kdict))
       ((mod) (opcall-node 'tsh:% seed kseed kdict))
       ((add) (opcall-node 'tsh:+ seed kseed kdict))
       ((sub) (opcall-node 'tsh:- seed kseed kdict))
       
       ;; lshift rshift rrshift
       ((lshift) (opcall-node 'tsh:lshift seed kseed kdict))
       ((rshift) (opcall-node 'tsh:rshift seed kseed kdict))

       ;; lt gt le ge
       ((eq) (values (cons (op-call 'tsh:eq kseed) seed) kdict))
       ((ne) (values (cons (op-call 'tsh:ne kseed) seed) kdict))
       ((lt) (values (cons (op-call 'tsh:lt kseed) seed) kdict))
       ((gt) (values (cons (op-call 'tsh:gt kseed) seed) kdict))
       ((le) (values (cons (op-call 'tsh:le kseed) seed) kdict))
       ((ge) (values (cons (op-call 'tsh:ge kseed) seed) kdict))

       (else
	(unless (member (car tree)
			'(@@ toplevel lexical abort arg-list arg rest-arg))
	  (sferr "MISSED: ~S\n" (car tree)))
	(cond
	 ((null? seed) (values (reverse kseed) kdict))
	 (else (values (cons (reverse kseed) seed) kdict)))))))

  (define (fH leaf seed dict)
    (values (cons leaf seed) dict))

  (catch 'tsh-error
    (lambda () (foldts*-values fD fU fH `(*TOP* ,exp) '() env))
    (lambda (key fmt . args)
      (apply simple-format (current-error-port)
	     (string-append "*** tsh: " fmt "\n") args)
      (values '(void) env))))

(define show-sxml #t)
(define (show-tsh-sxml v) (set! show-sxml v))
(define show-xtil #t)
(define (show-tsh-xtil v) (set! show-xtil v))

(define (compile-tree-il exp env opts)
  (when show-sxml (sferr "sxml:\n") (pperr exp) (unless exp (quit)))
  ;; Need to make an interp.  All TCLish commands execute in an interp
  ;; so need (interp-lookup at turntime)
  (let ((cenv (if (module? env) (cons* `(@top . #t) `(@M . ,env) xdict) env)))
    (if exp 
	(call-with-values
	    (lambda ()
	      (sxml->xtil exp cenv opts)
	      ;;(values #f cenv)
	      )
	  (lambda (exp cenv)
	    (when show-xtil (sferr "tree-il:\n") (pperr exp))
	    (values (parse-tree-il exp) env cenv)
	    ;;(values (parse-tree-il '(const "[hello]")) env cenv)
     	    )
	  )
	(values (parse-tree-il '(void)) env cenv))))

;; --- last line ---
