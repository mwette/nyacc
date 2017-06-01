;;; compile javascript sxml from parser to tree-il
;;;
;;; Copyright (C) 2015,2017 Matthew R. Wette
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by 
;;; the Free Software Foundation, either version 3 of the License, or 
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; NOTE: in guile22 apply => call

(define-module (nyacc lang javascript compile-tree-il)
  #:export (compile-tree-il js-sxml->tree-il-ext)
  #:use-module (nyacc lang javascript jslib)
  #:use-module ((sxml match) #:select (sxml-match))
  #:use-module ((sxml fold) #:select (foldts*-values))
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (language tree-il)
  )

(define-syntax-rule (if-guile-20 then else)
  (if (string=? "2.0" (effective-version)) then else))

;; guile 2.0 or 2.2
(define il-call (if-guile-20 'apply 'call))
(define (make-call proc . args) (cons* il-call proc args))
(define (make-pcall name . args)
  (if-guile-20 (cons* 'apply `(primitive ,name) args)
	       (cons* 'primcall name args)))

(define (rtail kseed)
  (cdr (reverse kseed)))

(define (binop-call op kseed)
  (rev/repl il-call `(@@ ,jslib-mod ,op) kseed))

(use-modules (ice-9 pretty-print))

(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr tree)
  (pretty-print tree (current-error-port) #:per-line-prefix "  "))

(define jslib-mod '(nyacc lang javascript jslib))

(define (x-assn rhs op lhs junk)
  (case (car op)
    ((assign) `(set! ,lhs ,rhs))
    ((add-assign) `(set! ,lhs ,(make-call `(@@ ,jslib-mod JS+) lhs rhs)))
    (else
     (sferr "\nUNKNOWN OP: ~S\n\n" op)
     '(unknown))))

;; @heading variable scope
;; Variables in the compiler are kept in a scope-stack with the highest
;; level being the current module.  Why do I convert to xxx?

;; We catch FunctionDecl and VariableDecl's on the way down and generate new
;; lexical scope and variable declartions let forms or function xxx

;; function declarations are always just a list of args;
;; @example
;;   function foo(x, y) { return x + y; }
;; =>
;;   (define foo (lambda @args (+ 
;; @end example
;; we just use rest arg and then express each
;; var reference as (list-ref @args index)
;; Another option is to use case-lambda ...

;; @subheading non-tail return
;; need to use prompts here, I think ... Hey just use let/ec ?
;; @example
;; (let/ec return ((var1 val1) (var2 val2)) ... (return x) ...)
;; @end example

;; SourceElements occurs in a Program (top-level) or as Function Body
;; We translate Program to begin
;; We translate FunctionBody to let

;; the dictionary will maintain entries with
;; '(lexical var JS~123)
;; variable references are of the forms
;; @table @code
;; @item (toplevel name)
;; top level env
;; @item (@ mod name)
;; exported module refernce
;; @item (@@ mod name)
;; unexported
;; @item (lexical name gensym)
;; lexical scoped variable
;; @end table

;; push/pop scope level
(define (push-scope dict)
  (list (cons '@l (1+ (assq-ref dict '@l))) (cons '@P dict)))
(define (pop-scope dict)
  (or (assq-ref dict '@P) (error "coding error: too many pops")))
(define (top-scope? dict)
  (eqv? 0 (assoc-ref dict '@l)))

(define (add-lexical name dict)
  (acons name `(lexical ,(string->symbol name) ,(gensym "JS~")) dict))

;; Add toplevel to dict.
(define (add-toplevel name dict)
  (acons name `(toplevel ,(string->symbol name)) dict))

;; Add lexcial or toplevel based on level.
(define (add-symboldef name dict)
  ;;(sferr "add-symboldef for ~S at @l=~S\n" name (assq-ref dict '@l))
  (if (positive? (assq-ref dict '@l))
      (add-lexical name dict)
      (add-toplevel name dict)))

(define (lookup name dict)
  ;;(when (string=? name "foo") (sferr "lookup ~S\n" name) (pperr dict))
  (cond
   ((not dict) #f)
   ((null? dict) #f)
   ((assoc-ref dict name))		; => value
   ((assoc-ref dict '@P) =>		; parent level
    (lambda (dict) (lookup name dict)))
   (else
    (let* ((env (assoc-ref dict '@M))	; host module, aka top level
	   (sym (string->symbol name))
	   (var (module-variable env sym)))
      (if (not var) #f
	  `(@@ ,(module-name env) ,sym))))))

;; ice-9 r5rs

;; body needs a line to build "var arguments" from Array(@args)
;; Right now args is the gensym of the rest argument named @code{@@args}.
(define (make-function name args body)
  (if (not args) (error "no args"))
  (let ((fname (case (car name) ((@ @@) (caddr name)) (else (cadr name))))
	(tagsym (gensym "JS~"))	(valsym (gensym "JS~")))
    ;;(sferr "make-function ~S ~S ~S\n" fname name args)
    `(define ,fname
       (lambda ((name . ,fname))
	 (lambda-case ((() #f @args #f () (,args))
		       (prompt
			(const return)	; tag
			,body		; body
			(lambda-case	; handler
			 (((tag val) #f #f #f () (,tagsym ,valsym))
			  (lexical val ,valsym))))))))
    ))

;; @deffn {Procedure} make-let bindings exprs
;; Generates a Tree-IL let form from arguments, where @var{bindings} looks like
;; @example
;; (((lexical v JS~5897) #<unspecified>)
;;  ((lexical w JS~5898) (const 3)))
;; @end example
;; @noindent
;; and @var{exprs} is a list of expressions, to something like
;; @example
;; (let (v w) (JS~5897 JS~5898) (#<unspecified> (const 3)) . exprs)
;; @end example
;; @end deffn
(define (make-let bindings exprs)
  (let iter ((names '()) (gsyms '()) (vals '()) (binds bindings))
    (if (null? binds)
	`(let ,(reverse names) ,(reverse gsyms) ,(reverse vals) (begin ,@exprs))
	(iter (cons (car (cdaar binds)) names)
	      (cons (cadr (cdaar binds)) gsyms)
	      (cons (cadar binds) vals)
	      (cdr binds)))))

;; reverse list but replace new head with @code{head}
;; @example
;; (rev/repl 'a '(4 3 2 1)) => '(a 2 3 4)
;; @end example
(define rev/repl
  (case-lambda
   ((arg0 revl)
    (let iter ((res '()) (inp revl))
      (if (null? (cdr inp)) (cons arg0 res)
	  (iter (cons (car inp) res) (cdr inp)))))
   ((arg0 arg1 revl)
    (let iter ((res '()) (inp revl))
      (if (null? (cdr inp)) (cons* arg0 arg1 res)
	  (iter (cons (car inp) res) (cdr inp)))))
   ))
	 
;; @deffn {Procedure} js-xml->tree-il-ext exp env opts
;; Compile javascript SXML tree to external tree-il representation.
;; This one is public because it's needed for debugging the compiler.
;; @end deffn
(define (js-sxml->tree-il-ext exp env opts)

  (define cep (current-error-port))
  
  ;; In the case where we pick off ``low hanging fruit'' we need to coordinate
  ;; the actions of the up and down handlers.   The down handler will provide
  ;; a kid-seed in order and generate a null list.  The up handler, upon seeing
  ;; a null list, will just incorporate the kids w/o the normal reverse.

  ;; @deffn {Procedure} fold-in-blocks src-elts-tail => src-elts-tail
  ;; Look through source elements.  Change every var xxx to a
  ;; @example
  ;; (@dots{} (VariableStatement (VariableDeclrationList ...)) @dots{})
  ;; @end example
  ;; @noindent
  ;; (@dots{} (VariableDeclarationList ...) (Block @dots{}))
  ;; @example
  ;; @dots{} @{ var a = 1; @dots{} @}
  ;; @end example
  ;; @noindent
  ;; We assume no elements of @code{SourceElements} is text.
  ;; @end deffn
  (define (fold-in-blocks src-elts-tail)
    (let iter ((src  src-elts-tail))
      (if (null? src) '()
	  (let ((elt (car src)) (rest (cdr src)))
	    ;;(simple-format #t "=> ~S\n" (car elt))
	    (if (eq? (car elt) 'VariableStatement)
		;;(list elt (cons 'Block (iter rest))) ;; .. (Vs ) (Block )
		(list (cons* 'Block elt (iter rest))) ;; .. (Block (Vs ))
		(cons elt (iter rest)))))))
		     
  (define (fD tree seed dict) ;; => tree seed dict
    ;; This handles branches as we go down the tree.  We do two things here:
    ;; @enumerate
    ;; @item Pick off low hanging fruit: items we can completely convert
    ;; @item trap places where variables are declared and maybe bump scope
    ;; Add symbols to the dictionary, keeping track of lexical scope.
    ;; @end enumerate
    ;; declarations: we need to trap ident references and replace them
    
    ;;(sferr "fD: tree=~S ...\n" (car tree))
    ;;(display "fD\n" cep) (pperr tree)
    (sxml-match tree

      ((Identifier ,name)
       ;;(sferr "fD: ret null\n")
       (let ((ref (lookup name dict)))
	 (if (not ref) (error "lookup 2 failed"))
	 (values '() ref dict)))
      
      ((PrimaryExpression (this))
       (error "not implemented: PrimaryExpression (this)"))
	      
      ((PrimaryExpression (Identifier ,name))
       ;;(when (string=? name "foo") (sferr "======\n"))
       (let ((ident (lookup name dict)))
	 (if (not ident) (error "JS: identifier not found:" name))
	 (values '() ident dict)))

      ((PrimaryExpression (NullLiteral ,null))
       (values '() '(const js:null) dict))

      ((BooleanLiteral ,true-or-false)
       (values '() `(const ,(char=? (string-ref true-or-false 0) #\t)) dict))

      ((PrimaryExpression (NumericLiteral ,val))
       (values '() `(const ,(string->number val)) dict))

      ((PrimaryExpression (StringLiteral ,str))
       (values '() `(const ,str) dict))

      ((PropertyNameAndValue (Identifier ,name) ,expr)
       (values `(PropertyNameAndValue (PropertyName ,name) ,expr) '() dict))

      ((obj-ref ,expr (Identifier ,name))
       (values `(aoo-ref ,expr (PropertyName ,name)) '() dict))

      ;; if toplevel we generate (toplevel "name")
      ;; if lexical we generate (lexical "name" "gensym")
      ((VariableDeclaration (Identifier ,name) . ,rest)
       ;;(sferr "fU: VD\n") (pperr dict)
       (let* ((dict1 (add-symboldef name dict))
	      (tree1 (lookup name dict1)))
	 (if (not tree1) (error "lookup failed"))
	 (values `(VariableDeclaration ,tree1 . ,rest) '() dict1)))

      ((FunctionDeclaration (Identifier ,name) ,rest ...)
       ;;(sferr "push ~S\n" name)
       (values tree '() (push-scope (add-symboldef name dict))))
      
      ((FormalParameterList ,idlist ...)
       ;; For all functions we just use rest arg and then express each
       ;; var reference as (list-ref @args index)
       ;; Another option is to use case-lambda ...
       (let* ((args (add-lexical "@args" dict))
	      (gsym (list-ref (car args) 3)) ; need gensym ref
	      (dikt (fold
		     (lambda (name indx seed)
		       (acons name (make-call `(toplevel list-ref)
					      `(lexical @args ,gsym)
					      `(const ,indx))
			      seed))
		     args
		     (map cadr idlist)
		     (let iter ((r '()) (n (length idlist))) ;; n-1 ... 0
		       (if (zero? n) r (iter (cons (1- n) r) (1- n))))
		     ))
	      )
	 (values tree '() dikt)))
      
      ((SourceElements . ,elts)
       ;; make to VDL always followed by a Block to end of
       (let* (
	      ;;(lvl (assq-ref dict '@l))
	      ;;(selt (if (zero? lvl) tree
		;;	(cons 'SourceElements (fold-in-blocks elts))))
	      (selt (if (top-scope? dict) tree
			(cons 'SourceElements (fold-in-blocks elts))))
	      )
	 ;;(sferr "lvl=~S\n" lvl)
	 (unless (or #t (top-scope? dict))
	   (sferr "was:\n") (pperr tree)
	   (sferr " is:\n") (pperr selt))
	 (values selt '() dict)))

      (,otherwise
       ;;(sferr "fD: otherwise\n") (pperr tree)
       (values tree '() dict))
      ))

  (define (fU tree seed dict kseed kdict) ;; => seed dict
    ;;(sferr "fU: kseed=~S\n    seed=~S\n" kseed seed) (pperr tree)
    ;; This routine rolls up processes leaves into the current branch.
    ;; We have to be careful about returning kdict vs dict.
    ;; Approach: always return kdict or (pop-scope kdict)
    (if
     (null? tree) (values (cons kseed seed) dict)
     (case (car tree)
       ((*TOP*)
	(values kseed kdict))

       ;; Identifier => fU

       ;; PrimaryExpression (w/ ArrayLiteral or ObjectLiteral only)
       ((PrimaryExpression)
	(values (cons (car kseed) seed) kdict))
      
       ;; ArrayLiteral
       ;; mkary is just primitive vector
       ((ArrayLiteral)
	(let ((exp (apply make-call `(@@ ,jslib-mod mkary) (car kseed))))
	  (values (cons exp seed) kdict)))
       
       ;; ElementList
       ((ElementList)
	(values (cons (rtail kseed) seed) kdict))

       ;; Elision: e.g., (Elision "3")
       ;; Convert to js:undefined: a bit of a hack for now, but wtf.
       ((Elision)
	(let* ((len (string->number (car kseed)))
	       (avals (make-list len '(void))))
	  (values (append avals seed) kdict)))

       ;; ObjectLiteral
       ((ObjectLiteral)
	(values (cons (car kseed) seed) kdict))
       
       ;; PropertyNameAndValueList
       ((PropertyNameAndValueList)
	(values
	 (cons `(apply (@@ ,jslib-mod mkobj) ,@(rtail kseed)) seed)
	 kdict))

       ;; PropertyNameAndValue
       ((PropertyNameAndValue)
	;;(values (cons* (car kseed) `(const ,(cadr kseed)) seed) kdict))
	(values (cons* (car kseed) (cadr kseed) seed) kdict))

       ;; PropertyName
       ((PropertyName)
	(values (cons `(const ,(car kseed)) seed) kdict))

       ;; aoo-ref (array-or-object ref), a cons cell: (dict name)
       ;; => (cons <expr> <name>)
       ((aoo-ref)
	(values (cons (make-pcall 'cons (car kseed) (cadr kseed)) seed) kdict))

       ;; obj-ref (converted to aoo-ref in fD)

       ;; new

       ;; CallExpression
       ((CallExpression)
	(values (cons (rev/repl il-call kseed) seed) kdict))

       ;; ArgumentList
       ((ArgumentList) ;; append-reverse-car ??? 
	(values (append (rtail kseed) seed) kdict))

       ;; delete
       ;; void
       ;; typeof
       ;; pre-inc
       ;; pre-dec
       ;; pos
       ;; neg
       ;; ~
       ;; not

       ;; mul div mod add sub
       ((mul) (values (cons (binop-call 'js:* kseed) seed) kdict))
       ((div) (values (cons (binop-call 'js:/ kseed) seed) kdict))
       ((mod) (values (cons (binop-call 'js:% kseed) seed) kdict))
       ((add) (values (cons (binop-call 'js:+ kseed) seed) kdict))
       ((sub) (values (cons (binop-call 'js:- kseed) seed) kdict))
       
       ;; lshift
       ;; rshift
       ;; rrshift
       ;; lt
       ;; gt
       ;; le
       ;; ge
       ;; instanceof
       ;; in
       ;; eq
       ;; neq
       ;; eq-eq
       ;; neq-eq
       ;; bit-and
       ;; bit-xor
       ;; bit-or
       ;; and
       ;; or
       ;; ConditionalExpression

       ;; AssignmentExpression
       ((AssignmentExpression)
	(values (cons (apply x-assn kseed) seed) kdict))

       ;; assign
       ;; mul-assign
       ;; div-assign
       ;; mod-assign
       ;; add-assign
       ;; sub-assign
       ;; lshift-assign
       ;; rshift-assign
       ;; rrshift-assign
       ;; and-assign
       ;; xor-assign
       ;; or-assign
       ;; expr-list

       ;; Block
       ((Block)
	;;(sferr "Bl tree 1st:\n") (pperr (cadr tree))
	;;(sferr "Bl kids:\n") (pperr (cadr (reverse kseed)))
	(let* ((tail (rtail kseed))
	       (exp1 (if (pair? tail) (car tail) #f))
	       (blck (if (and exp1 (eqv? 'bindings (car exp1)))
			 (make-let (cdar tail) (cdr tail))
			 (cons 'begin tail)))
	       )
	  ;;(pperr blck)
	  (values (cons blck seed) kdict)))

       ;; VariableStatement
       ((VariableStatement)
	(values (cons (car kseed) seed) kdict))

       ;; VariableDeclarationList
       ((VariableDeclarationList)
	(let* ((top (= 0 (assq-ref dict '@l))) ; at top ?
	       (top (top-scope? dict))
	       (tag (if top 'begin 'bindings)) ; begin or bindings for let
	       (tail (rtail kseed)))
	  ;;(sferr "VDL:\n") (pperr expr)
	  ;; kdict here because that brings in new xxx
	  (values (cons (cons tag tail) seed) kdict)))
       
       ;; VariableDeclaration
       ((VariableDeclaration)
	;;(sferr "VD: seed=~S kseed=~S\n\n" #f kseed)
	(let* ((top (= 0 (assq-ref dict '@l))) ; at top ?
	       (w/i (= 3 (length kseed))) ; w/ initializer
	       (elt0 (list-ref kseed 0))
	       (elt1 (list-ref kseed 1)))
	  (values
	   (cons
	    (if top
		(if w/i ;; toplevel defines
		    `(define ,(cadr elt1) ,elt0)
		    `(define ,(cadr elt0) (void)))
		(if w/i ;; bindings for let
		    (list elt1 elt0)
		    (list elt0 '(void))))
	    seed)
	   kdict)))
       
       ;; Initializer
       ((Initializer)		       ; just grab the single argument
	(values (cons (car kseed) seed) kdict))

       ;; EmptyStatement
       ((EmptyStatement)		; ignore
	(values seed dict))

       ;; ExpressionStatement
       ((ExpressionStatement)	       ; just grab the single argument
	(values (cons (car kseed) seed) kdict))

       ;; IfStatement
       ;; do
       ;; while
       ;; for
       ;; for-in
       ;; Expression
       ;; ExprStmt
       ;; ContinueStatement

       ;; ReturnStatement
       ((ReturnStatement) ;; will need a prompt for return, until optimized?
	;;(sferr "fU: kseed=~S\n    seed=~S\n" kseed seed) (pperr tree)
	(values (cons `(abort (const return) ,(rtail kseed) (const '()))
		      seed) kdict))

       ;; WithStatement
       ;; SwitchStatement
       ;; CaseBlock
       ;; CaseClauses
       ;; CaseClause
       ;; DefaultClause
       ;; LabelledStatement
       ;; ThrowStatement
       ;; TryStatement
       ;; Catch
       ;; Finally

       ;; FunctionDeclaration (see also fU)
       ;; This is going to be a bit complicated.
       ;; We will need to pop scope (in dict) until we find the parent.
       ;; And in the FunctionBody, appearing as SourceElements, we will
       ;; need to track statements into 'begin expressions somehow ...
       ((FunctionDeclaration)
	;;(sferr "fUP.FDecl kdict=\n") (pperr kdict)
	;;(sferr "fUP.FDecl  dict=\n") (pperr dict)
	(let* ((name (cadr kseed))
	       (args (list-ref (lookup "@args" kdict) 2))
	       (body `(begin ,@(car kseed)))
	       (fctn (make-function name args body))
	       )
	  (values (cons fctn seed) (pop-scope kdict))))

       ;; FunctionExpression

       ;; FormalParameterList
       ((FormalParameterList) ;; all in @code{@@args}.
	(values seed kdict))

       ;; Program
       ((Program)
	(values (cons 'begin (car kseed)) kdict))
       
       ;; SourceElements
       ((SourceElements)
	;; return kdict here because we may need to peel off decls'
	(values (cons (rtail kseed) seed) kdict))

       (else
	;;(sferr "fU: kseed=~S  [else]\n    seed=~S\n" kseed seed) (pperr tree)
	(cond
	 ((null? seed) (values (reverse kseed) kdict))
	 ;;((null? kseed) (values (cons (car tree) seed) dict)) ;; ???
	 (else (values (cons (reverse kseed) seed) kdict)))))))

  (define (fH leaf seed dict)
    (values (cons leaf seed) dict))

  ;; We generate a dictionary with the env (module?) available at the top.
  (let ((dict (acons `@M env JSdict))
	(sexp `(*TOP* ,exp)))
    (foldts*-values fD fU fH sexp '() dict)))

;; @deffn {Procedure} compile-tree-il exp env opts => 
(define (compile-tree-il exp env opts)
  (sferr "exp:\n") (pperr exp)
  (let* ((xrep (js-sxml->tree-il-ext exp env opts)))
    (sferr "tree-il:\n") (pperr xrep)
    (values (parse-tree-il '(const "stub")) env env)
    (values (parse-tree-il xrep) env env)
    ))

;; --- last line ---
