;;; nyacc/lang/octave/compile-tree-il.scm compile octave sxml to tree-il

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

;;; Notes:

;; limitations:
;; 1) variables cannot be introduced by lhs expression:
;;    i.e., a = 1 is OK, but a(1) = 1 is not

;;; Code:

(define-module (nyacc lang octave compile-tree-il)
  #:export (compile-tree-il show-octave-sxml show-octave-xtil)
  #:use-module (nyacc lang octave xlib)
  #:use-module (nyacc lang nx-util)
  #:use-module (nyacc lang sx-util)
  ;;#:use-module (nyacc lang util)
  #:use-module ((sxml fold) #:select (foldts*-values))
  #:use-module ((srfi srfi-1) #:select (fold fold-right append-reverse last))
  #:use-module (language tree-il)
  #:use-module (ice-9 match)
  ;;#:use-module (system base compile)
  )
(use-modules (ice-9 pretty-print))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr tree)
  (pretty-print tree (current-error-port) #:per-line-prefix "  "))

(define xlib-mod '(nyacc lang octave xlib))
(define xlib-module (resolve-module xlib-mod))
(define (xlib-ref name) `(@@ (nyacc lang octave xlib) ,name))
		       
(define push-scope nx-push-scope)
;;(define pop-scope nx-pop-scope)
(define top-level? nx-top-level?)
(define add-toplevel nx-add-toplevel)
(define add-lexical nx-add-lexical)
(define add-lexicals nx-add-lexicals)
;;(define add-symbol nx-add-symbol)
(define (lookup name dict)
  (or (nx-lookup name dict)
      (nx-lookup-in-env name xlib-module)))

;; This will push undeclared lexicals up one level.  Needs cleanup?
(define (pop-scope dict)
  (let ((pdict (nx-pop-scope dict)))
    (let loop ((prev #f) (next dict))
      ;;(sferr "pop next=~S\n" next)
      (cond
       ((eq? '@L (caar next)) (cond (prev (set-cdr! next pdict) dict)
				    (else pdict)))
       ((eq? '@P (caar next)) (cdar next))
       (else (loop (car next) (cdr next)))))))

;; @deffn {Procedure} function-scope? dict
;; Looks up the dict levels to see if there exists a @code{'@F} tag,
;; which denotes that context is in a function.
;; @end deffn
(define (function-scope? dict)
  (let loop ((dict dict))
    (cond
     ((nx-top-level? dict) #f)
     ((assq '@F dict) #t)
     (else (loop (nx-pop-scope dict))))))

;; In octave, variables are not declared so this will add to either the
;; containting function or toplevel.
(define (add-symbol name dict)
  (if (function-scope? dict)
      (nx-add-lexical name dict)
      (nx-add-toplevel name dict)))
		   
;; @deffn {Procedure} make-def-ifndef name
;; Generate a TIL expression that will ensure the toplevel name is defined.
;; If a define needs to be issues the value is @code{(void)}.  Generates
;; @example
;; (if (defined? 'a) undefined (define a undefined))
;; @end example
;; @noindent
;; where @code{undefined} is like @code{(if #f #f)}.
;; @end deffn
(define (make-def-ifndef symbol)
  `(if (call (toplevel module-local-variable)
	     (call (toplevel current-module))
	     (const ,symbol))
       (void) (define ,symbol (void))))
;; another option: like `(define-once foo (if #f #f))'
(define (x-make-def-indef symbol)
  `(define ,symbol
     (if (call (toplevel module-local-variable)
	       (call (toplevel current-module))
	       (const ,symbol))
	 (toplevel ,symbol)
	 (void))))

;; Add toplevel def's from dict before evaluating expression.  This puts
;; @var{expr} at the end of a chain of @code{seq}'s that execution
;; conditional defines to a void.  See @code{make-toplevel-defcheck}.
(define (add-topdefs dict expr)
  (let loop ((refs dict))
    (cond
     ((null? refs) expr)
     ((string? (caar refs)) ;; add define if not in toplevel
      (let* ((env0 (lookup '@M dict))
	     (name (caar refs))
	     (ref (nx-lookup-in-env name env0)))
	(if ref
	    (loop (cdr refs))
	    `(seq (define ,(string->symbol name) (void)) ,(loop (cdr refs))))))
     (#f ;;(string? (caar refs)) ;; check at runtime
      `(seq ,(make-def-ifndef (string->symbol (caar refs)))
	    ,(loop (cdr refs))))
     ((eq? '@top (caar refs)) expr)
     (else (loop (cdr refs))))))

(define make-opcall (opcall-generator xlib-mod))

;; @deffn {Procedure} display-result? tree
;; Predicate that looks at @code{term} attribute to determine if user wants
;; the result of this statement displayed.  In Guile, this is implemented as
;; a return value for the translated statement.
;; @end deffn
(define (display-result? tree)
  (and=> (sx-attr-ref tree 'term)
	 (lambda (t) (not (string=? t ";")))))

;; lvar: loop variable -- e.g., (lexical i i-1234)
;; should be
;; (make-for lvar init next body dict)
(define-public (make-for lvar expr body dict)
  (let* ((rsym (genxsym "%range")) (rval `(lexical %range ,rsym))
	 (frst `(call ,(xlib-ref 'ml:iter-first) ,rval))
	 (next `(call ,(xlib-ref 'ml:iter-next) ,rval ,lvar))
	 (ilsym (genxsym "iloop"))
	 
	 (olsym (genxsym "oloop"))
	 (bsym (lookup-gensym "break" dict))
	 (csym (lookup-gensym "continue" dict))
	 (inext `(call (lexical iloop ,ilsym) ,next))
	 (ifrst `(call (lexical iloop ,ilsym) ,frst))
	 (ocall `(call (lexical oloop ,olsym)))

	 (iloop `(lambda ((name . iloop))
		   (lambda-case (((,(cadr lvar)) #f #f #f () (,(caddr lvar)))
				 (if ,lvar (seq ,body ,inext) (void))))))
	 
  	 (ohdlr `(lambda () (lambda-case (((k) #f #f #f () (,(genxsym "k")))
					  ,inext))))
	 (oloop (make-thunk `(prompt #t (lexical continue ,csym) ,ifrst ,ohdlr)
			    #:name 'oloop))
 	 (hdlr `(lambda () (lambda-case (((k) #f #f #f () (,(genxsym "k")))
					 (void))))))
    ;; NOTE: the range could also go into the letrec
    `(let (%range break continue) (,rsym ,bsym ,csym)
	  (,expr
	   (primcall make-prompt-tag (const break))
	   (primcall make-prompt-tag (const continue)))
	  (letrec (iloop oloop) (,ilsym ,olsym) (,iloop ,oloop)
		  (prompt #t (lexical break ,bsym) ,ocall ,hdlr)))))
  
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

      ((string ,sval)
       (values '() `(const ,sval) dict))

      ((sel (ident ,name) ,expr)
       (values `(sel ,name ,expr) '() dict))

      ((switch ,expr . ,rest)
       ;; Convert
       ;;  (switch expr (case a stmtL) (case b stmtL) ... (otherwise stmtL))
       ;; to
       ;;  (xswitch expr (xif expr stmtL (xif expr stmtL ...  stmtL))
       (values
	`(xswitch ,expr
		  ,(let loop ((tail rest))
		     (cond
		      ((null? tail) '(empty-stmt))
		      ((eq? 'otherwise (sx-tag (car tail)))
		       (sx-ref (car tail) 1))
		      ((eq? 'case (sx-tag (car tail)))
		       `(xif (eq (ident "swx-val") ,(sx-ref (car tail) 1))
			     ,(sx-ref (car tail) 2) ,(loop (cdr tail))))
		      (else (error "unsupported case-expr")))))
	'()
	(acons '@L "switch"
	       (add-lexicals "swx-val" "break" (push-scope dict)))))

      ((if ,expr ,stmts . ,rest)
       ;; Convert
       ;;  (if expr stmt (elseif expr stmt) ... (else stmt))
       ;; to
       ;;  (xif expr stmt (xif expr stmt ...  stmt))
       (values
	`(xif ,expr ,stmts
	      ,(let loop ((tail rest))
		 (cond
		  ((null? tail) '(empty-stmt))
		  ((eq? 'else (sx-tag (car tail))) (sx-ref (car tail) 1))
		  ((eq? 'elseif (sx-tag (car tail)))
		   `(xif ,(sx-ref (car tail) 1) ; cond
			 ,(sx-ref (car tail) 2) ; then
			 ,(loop (cdr tail))))   ; else
		  (else (error "oops")))))
	'() dict))

      ((while . ,rest)
       (values tree '()
	       (acons '@L "while"
		      (add-lexicals "break" "continue" (push-scope dict)))))

      ((for . ,rest)
       (values tree '()
	       (acons '@L "for"
		      (add-lexicals "break" "continue" (push-scope dict)))))

      ;; (assn (ident ,name) ,rhs))=> (var-assn (ident ,name) ,rhs)
      ;; (assn (aref-or-call ,aexp ,expl)) => (elt-assn ,aexp ,expl ,rhs)
      ;; (assn (sel ,ident ,expr) ,rhs) => (mem-assn ,ident ,expr ,rhs)
      ;; (assn . ,other) => syntax error
      ((assn (@ . ,attr) (ident ,name) ,rhsx)	; assign variable
       (values `(var-assn (@ . ,attr) (ident ,name) ,rhsx) '() dict))
      ((assn (@ . ,attr) (aref-or-call ,aexp ,expl) ,rhsx) ; assign element
       (values `(elt-assn (@ . ,attr) ,aexp ,expl ,rhsx) '() dict))
      ((assn (@ . ,attr) (sel (ident ,name) ,expr) ,rhsx) ; assign member
       (values `(mem-assn (@ . ,attr) ,expr ,name ,rhsx) '() dict))
      ((assn . ,other)
       (throw 'nyacc-error "syntax error"))

      ((multi-assn (@ . ,attr) (lval-list . ,elts) ,rhsx)
       (let* ((lval-expl
	       (let loop ((elts elts) (ix 0))
		 (if (null? elts) '()
		     (let* ((n (string-append "arg" (number->string ix)))
			    (s (string->symbol n)) (g (genxsym n))
			    (rv `(lexical ,s ,g)))
		       (cons
			(sx-match (car elts)
			  ((ident ,name)
			   `(var-assn (ident ,name) ,rv))
			  ((aref-or-call ,aexp ,expl)
			   `(elt-assn ,aexp ,expl ,rv))
			  ((sel (ident ,name) ,expr)
			   `(mem-assn ,expr ,name ,rv))
			  (else (throw 'nyacc-error "bad lhs syntax")))
			(loop (cdr elts) (1+ ix))))))))
	 (values `(multi-assn (@ . ,attr) (lval-list . ,lval-expl) ,rhsx)
		 '() dict)))

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
	      (dict (add-lexical "return" dict))
	      (dict (acons '@F name dict)))
	 (values tree '() dict)))

      ;;((comm . ,_) (values '() '() dict))
       
      (else
       (values tree '() dict))))

  (define (fU tree seed dict kseed kdict) ;; => seed dict
    ;; This routine rolls up processes leaves into the current branch.
    ;; We have to be careful about returning kdict vs dict.
    ;; Approach: always return kdict or (pop-scope kdict)
    (when #f
      (sferr "fU: ~S\n" (if (pair? tree) (car tree) tree))
      (sferr "    kseed=~S\n    seed=~S\n" kseed seed)
      ;;(pperr tree)
      )
    (if
     (null? tree) (if (null? kseed)
		      (values seed kdict)		; fD said ignore
		      (values (cons kseed seed) kdict)) ; fD replacement

     (case (car tree)

       ;; before leaving add a call to make sure all toplevels are defined
       ((*TOP*)
	(values (add-topdefs kdict (car kseed)) kdict))

       ((comm) (values seed kdict))

       ((script function-file)
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
	       (lvars (let loop ((ldict kdict))
			;;(if (string=? "return" (caar ldict)) oargs
			(if (eq? '@F (caar ldict)) oargs
			    (cons (cdar ldict) (loop (cdr ldict))))))
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
	       (body (let loop ((nl '()) (ll '()) (vl '()) (vs lvars))
		       (if (null? vs)
			   `(let ,nl ,ll ,vl ,body)
			   (loop
			    (cons (list-ref (car vs) 1) nl)
			    (cons (list-ref (car vs) 2) ll)
			    (cons '(void) vl)
			    (cdr vs)))))
	       ;;
	       (fctn `(set! (toplevel ,name)
			    (lambda ((name . ,name) (language . nx-octave))
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
	(values (cons '(void) seed) kdict))

       ((expr-stmt)
	(values (cons (car kseed) seed) kdict))

       ;; Assignment needs to deal with all left hand expressions.
       ((var-assn) ;; variable assignment
	(let* ((tail (rtail kseed))
	       (lhs (car tail))
	       (rhs (cadr tail))
	       (stmt `(set! ,lhs ,rhs))
	       (disp (display-result? tree)))
	  ;;(sferr "var-assn:\n") (pperr kseed) ;;(pperr lhs) (pperr rhs)
	  (values (cons (if disp `(seq ,stmt ,lhs) stmt) seed) kdict)))

       ((elt-assn) ;; element assignment
	(let* ((tail (rtail kseed))
	       (aexp (list-ref tail 0))
	       (expl `(primcall list ,@(cdr (list-ref tail 1))))
	       (rhsx (list-ref tail 2))
	       (stmt `(call ,(xlib-ref 'ml:elt-assn) ,aexp ,expl ,rhsx)))
	  (values (cons stmt seed) kdict)))

       ((mem-assn) ;; member assignment
	(let* ((tail (rtail kseed))
	       (expr (list-ref tail 0))
	       (name `(const ,(string->symbol (list-ref tail 1))))
	       (rhsx (list-ref tail 2))
	       (stmt `(call ,(xlib-ref 'ml:struct-set!) ,expr ,name ,rhsx)))
	  (values (cons stmt seed) kdict)))

       ((multi-assn)
	;; This executes a call within a call-with-values where the values
	;; handler does a sequence of set! for lhs args.
	;;   [a, b(1), c.n] = f(1, 2, 3);
	;; =>
	;;   (call-with-values
	;;       (lambda () (f 1 2 3))
	;;     (lambda ($arg0 $arg1 $arg2 . $rest)
	;;       (set! a $arg0)
	;;       (ml:elt-assn b 1 $arg1)
	;;       (ml:mem-assn c "n" $arg2)))
	(let* ((body (car kseed))
	       (lhsxs (cdadr kseed))	; expr's generated by fD above
	       (avars (map last lhsxs))	; rhs of var-assn, -elt or -mem
	       (rest (genxsym "$rest"))
	       (disp (display-result? tree))
	       (blok (vblock lhsxs))
	       (blok (if (display-result? tree)
			 `(seq ,blok (primcall values ,@avars))
			 blok))
	       (body `(primcall
		       call-with-values ,(make-thunk body)
		       (lambda ()
			 (lambda-case ((,(map cadr avars) #f $rest #f
					() (,@(map caddr avars) ,rest))
				       ,blok))))))
	  (values (cons body seed) kdict)))

       ;; looping
       ;; 1) octave does have break statement, and continue I think
       ;; 2) for needs index and should call ml:iter-first ml:iter-next
       ;; 3) BUG top-levels can be introduced here, but we pop scope
       ;;    so these need to be moved to function or global scope
       
       ;; ("for" ident "=" expr term stmt-list "end"
       ((for) ;; TODO
	(let* ((tail (rtail kseed))
	       (lvar (list-ref tail 0))	; lvar
	       (expr (list-ref tail 1))	; expr
	       (body (list-ref tail 2))	; stmt-list
	       (stmt (make-for lvar expr body kdict))
	       )
	  (values (cons stmt kseed) (pop-scope kdict))))
       
       ((while)
	(let* ((tail (rtail kseed))
	       (expr `(if (primcall zero? ,(car tail)) (const #f) (const #t)))
	       (body (cdr tail)))
	  (values (cons (make-while expr body kdict) kseed) (pop-scope kdict))))

       ;; @code{if} converted to @code{xif} in fD
       ((xif)
	(let* ((tail (rtail kseed))
	       (cond1 `(if (primcall zero? ,(car tail)) (const #f) (const #t)))
	       (then1 (cadr tail))
	       (else1 (caddr tail)))
	  (values (cons `(if ,cond1 ,then1 ,else1) seed) kdict)))
       
       ;; converted in @code{fD} from switch, case-list, case, otherwise
       ((xswitch)
	(let* ((body (car kseed))
	       (expr (cadr kseed))
	       (swxv (lookup "swx-val" kdict))
	       (swxg (caddr swxv)))
	  (values
	   (cons `(let (swx-val) (,swxg) (,expr) ,body) kseed)
	   (pop-scope kdict))))

       ((return)
	(values
	 (cons `(abort ,(lookup "return" kdict) () (const ())) seed)
	 kdict))

       ((command) ;; TODO
	(values (cons '(void) kseed) kdict))

       ((expr-list)
	(values (cons (reverse kseed) seed) kdict))

       ((colon-expr fixed-colon-expr)
	(let* ((tail (rtail kseed))
	       (lb (list-ref tail 0))
	       (inc (if (= 2 (length tail)) '(const 1) (list-ref tail 1)))
	       (ub (list-ref tail (if (= 2 (length tail)) 1 2))))
	  (values (cons `(call ,(xlib-ref 'make-ml:range) ,lb ,inc ,ub) seed)
		  kdict)))

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
	   (cons `(call ,(xlib-ref 'ml:aref-or-call) ,proc-or-array ,@args)
		 seed)
	   kdict)))

       ((sel)
	(values
	 (cons `(call ,(xlib-ref 'ml:struct-ref) ,(car kseed)
		      (const ,(string->symbol (cadr kseed))))
	       seed)
	 kdict))

       ;; @section Matrix Constructs
       ;; Static semantics will extract the following:
       ;; @itemize
       ;; @item 1-D matrices (aka vectors) with only scalar integers:
       ;; These can include @code{+-*} expressions.  Used for indices.
       ;; @item 2-D matrices with only scalar floats:
       ;; These can include @code{+-*/} expressions. More efficient than ...
       ;; @item other matrices:
       ;; If a matrix expression includes, say, a variable reference, then the
       ;; dimension of that variable can only be determined at run-time.
       ;; @end itemize

       ;; row
       ((row)
	(values (cons (reverse kseed) seed) kdict))
       
       ;; matrix TODO
       ((matrix)
	(values (cons `(const 1001) seed) kdict))

       ((float-matrix)
	;; In a `let', create an array and then set a row at a time.
	;; In the following M=nrow-1, N=ncol-1.
	;; (let (($aval (make-array 'f64 nrow ncol))
	;;   (ml:array-set-row! 0 (list a00 a01 ... a0N))
	;;   ...
	;;   (ml:array-set-row! M (list aM0 aM1 ... aMN))
	;;   $aval)
	 (let* ((tail (rtail kseed))
		(row1 (car tail))
		(ncol (length (sx-tail row1)))
		(asym (genxsym "$aval"))
		(aval `(lexical $aval ,asym))
		(nrow (length tail))
		(makea `(call (toplevel make-typed-array) (const f64)
			      (const 0.0) (const ,nrow) (const ,ncol)))
		(body (let loop ((ix 0) (rows tail))
			(if (null? rows) aval
			    `(seq (call ,(xlib-ref 'ml:array-set-row!)
					,aval (const ,ix)
					(primcall list . ,(cdar rows)))
				  ,(loop (1+ ix) (cdr rows))))))
		(expr `(let ($aval) (,asym) (,makea) ,body)))
	   (values (cons expr seed) kdict)))
	 
       ((fixed-vector)
	(values (cons `(primcall vector . ,(rtail kseed)) seed) kdict))

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

(define show-sxml #t)
(define (show-octave-sxml v) (set! show-sxml v))
(define show-xtil #f)
(define (show-octave-xtil v) (set! show-xtil v))

(define (compile-tree-il exp env opts)
  (when show-sxml (sferr "sxml:\n") (pperr exp))
  (let ((cenv (if (module? env) (acons '@top #t (acons '@M env xdict)) env)))
    (if exp 
	(call-with-values
	    (lambda () (xlang-sxml->xtil exp cenv opts))
	  (lambda (exp cenv)
	    (when show-xtil (sferr "tree-il:\n") (pperr exp))
	    (values (parse-tree-il exp) env cenv)
	    ;;(values (parse-tree-il '(const "[hello]")) env cenv)
     	    ))
	(values (parse-tree-il '(void)) env cenv))))

;; --- last line ---
