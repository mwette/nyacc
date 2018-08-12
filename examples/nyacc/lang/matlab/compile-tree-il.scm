;;; compile matlab sxml to tree-il

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
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;; limitations:
;; 1) variables cannot be introduced by lhs expression:
;;    i.e., a = 1 is OK, but a(1) = 1 is not

(define-module (nyacc lang matlab compile-tree-il)
  #:export (compile-tree-il)
  #:use-module (nyacc lang matlab xlib)
  #:use-module (nyacc lang nx-util)
  #:use-module (nyacc lang sx-util)
  ;;#:use-module (nyacc lang util)
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

(define xlib-mod '(nyacc lang matlab xlib))
(define xlib-module (resolve-module xlib-mod))
(define (xlib-ref name) `(@@ (nyacc lang matlab xlib) ,name))
		       
(define push-scope nx-push-scope)
(define pop-scope nx-pop-scope)
(define top-level? nx-top-level?)
(define add-toplevel nx-add-toplevel)
(define add-lexical nx-add-lexical)
(define add-lexicals nx-add-lexicals)
(define add-symbol nx-add-symbol)
(define (lookup name dict)
  (or (nx-lookup name dict)
      (nx-lookup-in-env name xlib-module)))

;; @deffn {Procedure} ensure-toplevel name
;; Generate a TIL expression that will ensure the toplevel name is defined.
;; If a define needs to be issues the value is @code{(void)}.
;; @end deffn
(define (make-toplevel-defcheck name)
  (let ((var (genxsym "var")) (sym (string->symbol name)))
    `(let (var) (,var)
	  ((call (toplevel module-local-variable)
		 (call (toplevel current-module))
		 (const ,sym)))
	  (if (lexical var ,var) (void) (define ,sym (void))))))

;; Add toplevel def's from dict before evaluating expression.  This puts
;; @var{expr} at the end of a chain of @code{seq}'s that execution
;; conditional defines to a void.  See @code{make-toplevel-defcheck}.
(define (add-topdefs dict expr)
  (let iter ((refs dict))
    (cond
     ((null? refs) expr)
     ((string? (caar refs))
      `(seq ,(make-toplevel-defcheck (caar refs))
	    ,(iter (cdr refs))))
     (else (iter (cdr refs))))))

(define make-opcall (opcall-generator xlib-mod))

;; @deffn {Procedure} display-result? tree
;; Predicate that looks at @code{term} attribute to determine if user wants
;; the result of this statement displayed.  In Guile, this is implemented as
;; a return value for the translated statement.
;; @end deffn
(define (display-result? tree)
  (and=> (sx-attr-ref tree 'term)
	 (lambda (t) (not (string=? t ";")))))

;; @deffn {Procedure} xlang-sxml->xtil exp env opts
;; Compile extension SXML tree to external Tree-IL representation.
;; This one is public because it's needed for debugging the compiler.
;; @end deffn
(define (xlang-sxml->xtil exp env opts)

  (define (fD tree seed dict) ;; => tree seed dict
    (sx-match tree

      ((ident ,name)
       (cond
	((lookup name dict) => (lambda (r) (values '() r dict)))
	(else (let ((dict (add-symbol name dict)))
		(values '() (lookup name dict) dict)))))

      ((fixed ,sval)
       (values '() `(const ,(string->number sval)) dict))

      ((float ,sval)
       (values '() `(const ,(string->number sval)) dict))

      ;; (assn (ident ,name) ,rhs))=> (var-assn (ident ,name) ,rhs)
      ;; (assn (aref-or-call ,ident ,expl)) => (elt-assn ,ident ,expl ,rhs)
      ;; (assn (sel ,ident ,expr) ,rhs) => (mem-assn ,ident ,expr ,rhs)
      ;; (assn . ,other) => syntax error
      ((assn (ident ,name) ,rhs)
       (values `(var-assn (ident ,name) ,rhs) '() dict))
      ((assn (aref-or-call ,ident ,expl) ,rhs)
       (values `(elt-assn ,ident ,expl ,rhs) '() dict))
      ((assn (sel ,ident ,expr) ,rhs)
       (values `(mem-assn ,ident ,expr ,rhs) '() dict))
      ((assn . ,other)
       (throw 'nyacc-error "syntax error"))

      ((fctn-defn (fctn-decl (ident ,name)
			     (ident-list . ,inargs)
			     (ident-list . ,outargs))
		  ,_)
       ;; note: In the following (1) placement of "return" and (2) use of
       ;; fold (vs fold-right) is critical for fctn-defn handling in fU.
       (let* ((dict (push-scope (add-symbol name dict)))
	      (dict (fold (lambda (sx dt) (add-lexical (sx-ref sx 1) dt))
			  dict inargs))
	      (dict (fold (lambda (sx dt) (add-lexical (sx-ref sx 1) dt))
			  dict outargs))
	      (dict (add-lexical "return" dict)))
	 (values tree '() dict)))
       
      (else
       (values tree '() dict))))

  (define (fU tree seed dict kseed kdict) ;; => seed dict
    (when #f
      (sferr "fU: ~S\n" (if (pair? tree) (car tree) tree))
      (sferr "    kseed=~S\n    seed=~S\n" kseed seed)
      ;;(pperr tree)
      )
    ;; This routine rolls up processes leaves into the current branch.
    ;; We have to be careful about returning kdict vs dict.
    ;; Approach: always return kdict or (pop-scope kdict)
    (if
     (null? tree) (values (cons kseed seed) kdict)
     
     (case (car tree)

       ;; before leaving add a call to make sure all toplevels are defined
       ((*TOP*)
	(values (add-topdefs kdict (car kseed)) kdict))

       ((function-file)
	(let* ((tail (rtail kseed))
	       (body (fold-right
		      (lambda (stmt body) (if body `(seq ,stmt ,body) stmt))
		      #f tail)))
	  (values (cons body seed) kdict)))

       ;; For functions, need to check kdict for lexicals and add them.
       ((fctn-defn)
	(let* ((tail (rtail kseed))
	       (decl (list-ref tail 0))
	       (name (cadr (list-ref decl 1)))
	       (iargs (cdr (list-ref decl 2))) ; in reverse order
	       (oargs (cdr (list-ref decl 3))) ; in reverse order
	       (lvars (let iter ((ldict kdict))
			(if (string=? "return" (caar ldict)) oargs
			    (cons (cdar ldict) (iter (cdr ldict))))))
	       ;; Ensure that last call is a return.
	       (body (list-ref tail 1))
	       ;; Set up the return prompt expr
	       (ptag (lookup "return" kdict))
	       (body (with-escape ptag body))
	       ;; The tail expression is return value(s).
	       (rval (case (length oargs)
		       ((0) '(void))
		       ((1) (car oargs))
		       (else `(primcall values ,@oargs))))
	       (body `(seq ,body ,rval))
	       ;; Now wrap in local `let'
	       (body (let iter ((nl '()) (ll '()) (vl '()) (vs lvars))
		       (if (null? vs)
			   `(let ,nl ,ll ,vl ,body)
			   (iter
			    (cons (list-ref (car vs) 1) nl)
			    (cons (list-ref (car vs) 2) ll)
			    (cons '(void) vl)
			    (cdr vs)))))
	       ;;
	       (fctn `(set! (toplevel ,name)
			(lambda ((name . ,name))
			  (lambda-case ((() ,(map cadr iargs) #f #f
					 ,(map (lambda (v) '(void)) iargs)
					 ,(map caddr iargs)) ,body))))))
	  (values (cons fctn seed) (pop-scope kdict))))

       ;; fctn-decl: handled by fctn-defn case

       ((stmt-list)
	(let* ((tail (rtail kseed))
	       (body (fold-right
		      (lambda (stmt body) (if body `(seq ,stmt ,body) stmt))
		      #f tail)))
	  (values (cons body seed) kdict)))

       ;; Statements
       ((empty-stmt)
	(values seed kdict))

       ((expr-stmt)
	(values (cons (car kseed) seed) kdict))

       ;; Assignment needs to deal with all left hand expressions
       ;; 1) (ident ,name)
       ;; 2) (aref-or-call ,expr-list)
       ;; 3) (sel (ident ,name) ,expr)
       ;; ...
       ;; 1) set!
       ;; 2) vector-set!
       ;; 3) array-set!
       ;; 4) hash-set!
       ((var-assn)
	(let* ((tail (rtail kseed))
	       (lhs (car tail))
	       (rhs (cadr tail))
	       (stmt `(set! ,lhs ,rhs))
	       (disp (display-result? tree)))
	  (values (cons (if disp `(seq ,stmt ,lhs) stmt) seed) kdict)))

       ((multi-assn)
	;; This executes a call within a call-with-values where the values
	;; handler does a sequence of set! for lhs args.
	;;   [a, b] = f(1, 2, 3);
	;; =>
	;;   (call-with-values
	;;       (lambda () (f 1 2 3))
	;;     (lambda ($arg0 $arg1 . $rest)
	;;       (set! a $arg0)
	;;       (set! b $arg1))
	;; TODO: currently use set! but need to expand
	(let* ((body (car kseed))
	       (lvals (cdadr kseed))
	       (avars (let iter ((lvs lvals) (ix 0))
			(if (null? lvs) '()
			    (let* ((n (string-append "$arg" (number->string ix)))
				   (s (string->symbol n)) (g (genxsym n))
				   (x `(set! ,(car lvs) (lexical ,s ,g))))
			      (cons (list s g x) (iter (cdr lvs) (1+ ix)))))))
	       (rest (genxsym "$rest"))
	       (body `(primcall
		       call-with-values ,(make-thunk body)
		       (lambda ()
			 (lambda-case ((,(map car avars) #f $rest #f
					() (,@(map cadr avars) ,rest))
				       ,(vblock (map caddr avars))))))))
	  (values (cons body seed) kdict)))
       
       ((for) ;; TODO
	(values (cons '(void) kseed) kdict))
       
       ((while) ;; TODO
	(values (cons '(void) kseed) kdict))
       
       ((if) ;; TODO
	(values (cons '(void) kseed) kdict))
       
       ((switch) ;; TODO
	(values (cons '(void) kseed) kdict))

       ((case-list) ;; TODO
	(values (cons '(void) kseed) kdict))
       ((case) ;; TODO
	(values (cons '(void) kseed) kdict))

       ((return)
	(values
	 (cons `(abort ,(lookup "return" kdict) () (const ())) seed)
	 kdict))

       ((command) ;; TODO
	(values (cons '(void) kseed) kdict))

       ((expr-list)
	(values (cons (reverse kseed) seed) kdict))

       ((colon-expr) ;; TODO
	(values (cons '(void) kseed) kdict))

       ((or) (make-opcall 'ml:or seed kseed kdict))
       ((and) (make-opcall 'ml:and seed kseed kdict))
       ((eq) (make-opcall 'ml:eq seed kseed kdict))
       ((ne) (make-opcall 'ml:ne seed kseed kdict))
       ((lt) (make-opcall 'ml:lt seed kseed kdict))
       ((gt) (make-opcall 'ml:gt seed kseed kdict))
       ((le) (make-opcall 'ml:le seed kseed kdict))
       ((ge) (make-opcall 'ml:ge seed kseed kdict))
       
       ((add) (make-opcall 'ml:+ seed kseed kdict))
       ((sub) (make-opcall 'ml:- seed kseed kdict))
       ((dot-add) (make-opcall 'ml:.+ seed kseed kdict))
       ((dot-sub) (make-opcall 'ml:.- seed kseed kdict))
       ((mul) (make-opcall 'ml:* seed kseed kdict))
       ((div) (make-opcall 'ml:/ seed kseed kdict))
       ((ldiv) (make-opcall 'ml:\ seed kseed kdict))
       ((pow) (make-opcall 'ml:^ seed kseed kdict))
       ((dot-mul) (make-opcall 'ml:.* seed kseed kdict))
       ((dot-div) (make-opcall 'ml:./ seed kseed kdict))
       ((dot-pow) (make-opcall 'ml:.^ seed kseed kdict))
       
       ((neg) (make-opcall 'ml:neg seed kseed kdict))
       ((pos) (make-opcall 'ml:pos seed kseed kdict))
       ((not) (make-opcall 'ml:not seed kseed kdict))
       
       ((transpose) (make-opcall 'ml:xpose seed kseed kdict))
       ((conj-transpose) (make-opcall 'ml:cj-xpose seed kseed kdict))

       ;; aref-or-call
       ((aref-or-call)
	(let ((proc-or-array (cadr kseed)) (args (cdar kseed)))
	  (values
	   (cons `(call ,(xlib-ref 'ml:aref-or-call) ,proc-or-array ,@args) seed)
	   kdict)))

       ;; sel

       ;; row
       ;; matrix
       ;; cell-array

       ;; comm-list

       ;; ident, fixed, float, string, comm

       ((@) (values seed kdict))

       (else
	(cond
	 ((null? seed) (values (reverse kseed) kdict))
	 (else (values (cons (reverse kseed) seed) kdict)))))))

  (define (fH leaf seed dict)
    (values (cons leaf seed) dict))

  (foldts*-values fD fU fH `(*TOP* ,exp) '() env)
  )

(define (compile-tree-il exp env opts)
  ;;(sferr "sxml:\n") (pperr exp)
  (let ((cenv (if (module? env) (acons '@top #t (acons '@M env xdict)) env)))
    (if exp 
	(call-with-values
	    (lambda () (xlang-sxml->xtil exp cenv opts))
	  (lambda (exp cenv)
	    ;;(sferr "tree-il:\n") (pperr exp)
	    (values (parse-tree-il exp) env cenv)
	    ;;(values (parse-tree-il '(const "[compile-tree-il skip]")) env cenv)
     	    ))
	(values (parse-tree-il '(void)) env cenv))))

;; --- last line ---
