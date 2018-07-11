;;; compile javascript sxml from parser to tree-il

;; Copyright (C) 2015-2018 Matthew R. Wette
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

;; My goal in this development was to get experience with comping SXML trees
;; to tree-il: putting together patterns and utility procedures for converting
;; common structures (e.g, return <expr>, break).  It might be fun to also try
;; converting to CPS. (But need to read more on this.) -- Matt

;; TODO:
;; @itemize
;; @item convert to guile-2.2 (atomic-box, apply->call, begin->seq)
;; @item Implement @code{this}.
;; @item Imeplement @code{for}.
;; @item Imeplement @code{for-in}.
;; @item Update to es5.
;; @item Implement objects and prototypes correctly.
;; @item Implement unary and binary operators (in jslib-01.scm) correctly.
;; @end itemize
;;
;; NOTES
;; @itemize
;; @item JS functions will need to be re-implemented as objects, with the
;;       `[[Call]]' property used to make calls.  
;; @end itemize

(define-module (nyacc lang javascript compile-tree-il)
  #:export (compile-tree-il js-sxml->tree-il-ext)
  #:use-module (nyacc lang javascript jslib)
  #:use-module (nyacc lang sx-util)
  #:use-module ((sxml fold) #:select (foldts*-values))
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (language tree-il)
  #:use-module (system base pmatch)
  )
(use-modules (ice-9 pretty-print))

;; === portability ===================
;; no longer supports guile 2.0: assuming 2.2

;; list of expressions to (seq ... (last))
(define (block expr-or-expr-list)
  (cond
   ((pair? expr-or-expr-list)
    (let iter ((xl expr-or-expr-list))
      (if (null? (cdr xl)) (car xl)
	  (cons* 'seq (car xl) (iter (cdr xl))))))
   ((null? expr-or-expr-list)
    `(void))
   (else
    expr-or-expr-list)))

(define (jsym) (gensym "JS~"))

;; === debugging =====================

(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr tree)
  (pretty-print tree (current-error-port) #:per-line-prefix "  "))

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

;; === symbol table ================

(define jslib-mod '(nyacc lang javascript jslib))
(define (jslib-ref name) `(@@ (nyacc lang javascript jslib) ,name))

;; may need to push-level (blocks) and push-scope (functions)

;; push/pop scope level
(define (push-scope dict)
  (list (cons '@P dict)))
(define (pop-scope dict)
  (or (assq-ref dict '@P) (error "coding error: too many pops")))
(define (top-level? dict)
  (assoc-ref dict '@top))

;; Add toplevel to dict, return dict
(define (add-toplevel name dict)
  (acons name `(toplevel ,(string->symbol name)) dict))

;; Add lexical to dict, return dict
(define (add-lexical name dict)
  (acons name `(lexical ,(string->symbol name) ,(jsym)) dict))

;; (add-lexicals name1 name2 ... dict) 
(define (add-lexicals . args)
  (let iter ((args args))
    (if (null? (cddr args)) (add-lexical (car args) (cadr args))
	(add-lexical (car args) (iter (cdr args))))))

;; Add lexcial or toplevel based on level.
(define (add-symboldef name dict)
  (if (top-level? dict)
      (add-toplevel name dict)
      (add-lexical name dict)))

;; add label for continue break.  The value will be a pair
;; with car the continue ref and cdr the break ref
(define (add-label name dict)
  (acons name (cons #f #f) dict))

(define (c-name->guile name)
  ;; | scm_  | _ - | ! _x | _to_ -> | _less < | _gr > | _leq <= | _geq >= |
  (string-map (lambda (ch) (if (char=? ch #\_) #\- ch)) name))

(define (find-in-env name env)
  (let ((sym (string->symbol name)))
    (if (module-variable env sym)
	`(@@ ,(module-name env) ,sym)
	#f)))

(define (lookup name dict)
  ;;(when (string=? name "foo") (sferr "lookup ~S\n" name) (pperr dict))
  (cond
   ((not dict) #f)
   ((null? dict) #f)
   ((assoc-ref dict name))		; => value
   ((assoc-ref dict '@P) =>		; parent level
    (lambda (dict) (lookup name dict)))
   ((find-in-env name (assoc-ref dict '@M)))
   ((find-in-env (c-name->guile name) (assoc-ref dict '@M)))
   (else #f)))

;; === using prompts ====================

(define (add-exit name dict)
  (acons name (make-prompt-tag "JS~") dict))

;; @deffn {Procedure} find-exit-tag name dict => prompt-tag
;; used along with @code{with-exit} (see below)
;; (find-exit dict) => JS~1234
;; (find-exit dict #:label "oloop") => JS~1234
;; @end deffn
;; if label then lookup label and get exit from child
(define* (find-exit-tag name dict #:key label)
  (if label
      (let iter ((cdict dict) (pdict (assoc-ref dict '@P)))
	(if (not pdict) #f
	    (if (and (assoc-ref pdict label)
		     (assoc-ref "~exit" cdict))
		(assoc-ref name cdict)
		(iter pdict (assoc-ref pdict '@P)))))
      (let* ((sym (lookup name dict)))
	(if (not sym) (error "JS: exit not found for " name))
	sym)))
    
;; @deffn {Procedure} with-exit tag body
;; use for return and break where break is passed '(void)
;; tag is from (make-prompt-tag)
;; @end deffn
(define (with-exit-arg tag body)
  (let ((arg-sym (jsym)))
    `(prompt
      (const ,tag)
      ,body
      (lambda-case (((cont arg) #f #f #f () (,(jsym) ,arg-sym))
		    (lexical arg ,arg-sym))))))

;; now handler has one arg: called-by-continue?
;; tagvar is (
(define (with-exit-handler tag body handler arg)
  `(prompt
    (const ,tag)
    ,body
    (lambda-case (((k ,(cadr arg)) #f #f #f () (,(jsym) ,(caddr arg)))
		  ,handler))))

;; === codegen procedures =============

(define (rtail kseed)
  (cdr (reverse kseed)))

;; (and a b c) => (if a (if b (if c #t #f) #f) #f)
(define (make-and . args)
  (let iter ((args args))
    (if (null? args) '(const #t)
	`(if ,(car args) ,(iter (cdr args)) (const #f)))))

;; (or a b c) => (if a #t (if b #t (if c #t #f)))
(define (make-or . args)
  (let iter ((args args))
    (if (null? args) '(const #f)
	`(if ,(car args) (const #t) ,(iter (cdr args))))))

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
	`(let ,(reverse names) ,(reverse gsyms) ,(reverse vals) ,(block exprs))
	(iter (cons (car (cdaar binds)) names)
	      (cons (cadr (cdaar binds)) gsyms)
	      (cons (cadar binds) vals)
	      (cdr binds)))))

;; @deffn {Procedure} make-thunk expr => `(lambda ...)
;; Generate a thunk.
;; @end deffn
(define* (make-thunk expr #:key name)
  `(lambda ,(if name '((name)) '()) (lambda-case ((() #f #f #f () ()) ,expr))))

;; body needs a line to build "var arguments" from Array(@args)
;; Right now args is the gensym of the rest argument named @code{@@args}.
(define* (make-function this args body #:key name)
  (if (not args) (error "no args"))
  `(lambda ,(if name `((name ,name)) '())
     ;; If I add this as first argument, then calls to guile procedures
     ;; don't work.  Either dynamic binding to this or check if js call
     ;; (e.g., add meta of (js . #t)
     ;; -
     ;;(lambda-case (((this) #f @args #f () (,this ,args)) ,body))))
     (lambda-case ((() #f @args #f () (,args)) ,body))))

;; used in fU for switch cases
;; (let ((key (if key #t (equal? case-key val))))
;;   (if key case-stmts)
;;   next)
;; or chained we get
;; (let ((key (if key #t (equal? case-key val))))
;;   (if key case-stmts)
;;   (let ((key (if key #t (equal? case-key val))))
;;     (if key case-stmts)
;;     next))
(define (make-case val sym psym kseed next)
  `(let (~key) (,sym)
	((if (lexical ~key ,psym) (const #t)
	    (prim-call equal? ,val ,(cadr kseed))))
	(seq (if (lexical ~key ,sym) ,(car kseed) (void))
	       ,next)))
	 
;; @deffn {Procedure} resolve-ref ref => exp
;; WARNING: I think this is more subtle than I am making it.@*
;; Resolve a possible reference (lval) to an expression (rval).
;; Right now this will convert an object-or-array ref to its value
;; via @code{js-ooa-get}.  Otherwise just return the value.
;; @end deffn
(define (resolve-ref ref)
  (let ((tag (car ref)))
    (if (or (vector? tag) (hash-table? tag))
	`(call ,(jslib-ref 'js-ooa-get) ,ref)
	ref)))

;; @deffn {Procedure} op-on-ref ref op ord => `(let ...)
;; This routine generates code for @code{ref++}, etc where @var{ref} is
;; a @code{toplevel}, @code{lexical} or @emph{ooa-ref} (object or array
;; reference).  The argument @var{op} is @code{'js:+} or @code{'js:-} and
;; @var{ord} is @code{'pre} or @code{'post}.
;; @end deffn
(define (op-on-ref ref op ord)
  (let* ((sym (jsym))
	 (val (case (car ref)
		((toplevel) ref)
		((lexical) ref)
		(else `(call ,(jslib-ref 'js-ooa-get) ,ref))))
	 (loc `(lexical ~ref ,sym))
	 (sum `(call ,(jslib-ref op) (const 1) ,loc))
	 (set (case (car ref)
		((toplevel lexical) `(set! ,ref ,sum))
		(else `(call ,(jslib-ref 'js-ooa-put) ,ref ,sum))))
	 (rval (case ord ((pre) val) ((post) loc))))
    `(let (~ref) (,sym) (,val) (seq ,set (seq ,rval (void))))))

;; for lt + rt, etc
(define (op-call op kseed)
  (rev/repl 'call (jslib-ref op) kseed))
(define (op-call/prim op kseed)
  (rev/repl 'prim-call op kseed))

;; deffn {Procedure} op-assn kseed => `(set! lhs rhs)
;; op-assn: for lhs += rhs etc
;; end deffn
(define op-assn
  (let ((opmap
	 '((mul-assign . js:*) (div-assign . js:/) (mod-assign . js:%)
	   (add-assign . js:+) (sub-assign . js:-) (lshift-assign . js:lshift)
	   (rshift-assign . js:rshift) (rrshift-assign . js:rrshift)
	   (and-assign . js:and) (xor-assign . js:xor) (or-assign . js:or)
	   (assign . #f))))
    (lambda (kseed)
      (let ((lhs (caddr kseed))
	    (op (assq-ref opmap (caadr kseed)))
	    (rhs (car kseed)))
	(if op
	    `(set! ,lhs (call (@@ ,jslib-mod ,op) lhs rhs))
	    `(set! ,lhs ,rhs))))))

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

;; ====================================
	 
;; @deffn {Procedure} js-sxml->tree-il/ext exp env opts
;; Compile javascript SXML tree to external tree-il representation.
;; This one is public because it's needed for debugging the compiler.
;; @end deffn
(define (js-sxml->tree-il/ext exp env opts)

  ;; In the case where we pick off ``low hanging fruit'' we need to coordinate
  ;; the actions of the up and down handlers.   The down handler will provide
  ;; a kid-seed in order and generate a null list.  The up handler, upon seeing
  ;; a null list, will just incorporate the kids w/o the normal reverse.

  ;; @deffn {Procedure} remove-empties src-elts-tail => src-elts-tail
  ;; @end deffn
  (define (remove-empties src-elts-tail)
    (let iter ((src src-elts-tail))
      (if (null? src) '()
	  (let ((elt (car src)) (rest (cdr src)))
	    (if (eq? (car elt) 'EmptyStatement)
		(iter rest)
		(cons elt (iter rest)))))))

  ;; @deffn {Procedure} labelable-stmt? stmt => #f|stmt
  ;; This predicate determines if the statement can have a preceeding label.
  ;; @end deffn
  (define (labelable-stmt? stmt)
    (memq (car stmt) '(do while for for-in BreakStatement LabelledStatement)))
  
  ;; @deffn {Procedure} cleanup-labels src-elts-tail => src-elts-tail
  ;; Assumes all top-level EmptyStatements have been removed.
  ;; This reduces @code{LabelledStatement}s to the form
  ;; @example
  ;; @dots{} (LabelledStatement id iter-stmt) @dots{}
  ;; @dots{} (LabelledStatement id (LabelledStatement id iter-stmt)) @dots{}
  ;; @end example
  ;; @noindent
  ;; where @code{iter-stmt} is @code{do}, @code{while}, @code{for} or
  ;; @code{switch}, or removes them if not preceeding iteration statement.
  ;; @end deffn
  (define (cleanup-labels src-elts-tail)
    (let iter ((src src-elts-tail))
      (if (null? src) '()
	  (if (eq? (caar src) 'LabelledStatement)
	      (call-with-values
		  (lambda ()
		    (let* ((elt (car src)) (rest (cdr src))
			   (id (cadr elt)) (stmt (caddr elt)))
		      (if (eqv? 'EmptyStatement (car stmt))
			  (if (and (pair? rest) (labelable-stmt? (car rest)))
			      (values id (car rest) (cdr rest))
			      (values id stmt rest))
			  (if (labelable-stmt? stmt)
			      (values id stmt rest)
			      (values id '(EmptyStatement) (cons stmt rest))))))
		(lambda (id stmt rest)
		  (if (eqv? 'EmptyStatement (car stmt))
		      (begin
			(simple-format (current-error-port)
				       "removing misplaced label: ~A\n"
				       (cadr id))
			(iter rest))
		      (cons `(LabelledStatement ,id ,stmt) (iter rest)))))
	      (cons (car src) (iter (cdr src)))))))

  
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
	    (if (eq? (car elt) 'VariableStatement)
		(list (cons* 'Block (cadr elt) (iter rest)))
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
    (sx-match tree

      ((Identifier ,name)
       ;;(sferr "fD: ret null\n")
       (let ((ref (lookup name dict)))
	 (if (not ref) (error "lookup 2 failed"))
	 (values '() ref dict)))
      
      ((PrimaryExpression (this))
       (error "not implemented: PrimaryExpression (this)"))
	      
      ((PrimaryExpression (Identifier ,name))
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
       (values `(ooa-ref ,expr (PropertyName ,name)) '() dict))

      ((Block . ,elts) ;; see comments on SourceElements below
       (let* ((elts (remove-empties elts))
	      (elts (cleanup-labels elts))
	      (elts (fold-in-blocks elts)))
	 (values tree '() dict)))
      
      ((StatementList . ,stmts)
       (let* ((stmts (remove-empties stmts))
	      (stmts (fold-in-blocks stmts)))
	 (values tree '() dict)))
      
      ((VariableDeclaration (Identifier ,name) . ,rest)
       (let* ((dict1 (add-symboldef name dict))
	      (tree1 (lookup name dict1)))
	 (if (not tree1) (error "lookup failed"))
	 (values `(VariableDeclaration ,tree1 . ,rest) '() dict1)))

      ((do . ,rest)
       (values tree '() (add-exit "~exit" (push-scope dict))))

      ((while . ,rest)
       (values tree '() (add-exit "~exit" (push-scope dict))))

      ((for . ,rest)
       (values tree '() (add-exit "~exit" (push-scope dict))))

      ((for-in . ,rest)
       (values tree '() (add-exit "~exit" (push-scope dict))))

      ((SwitchStatement . ,rest)
       (values tree '() (add-lexical "~val"
				     (add-exit "~exit" (push-scope dict)))))

      ((LabelledStatement (Identifier ,name) ,stmt)
       (values tree '() (add-label name dict)))

      ((TryStatement . ,expr)
       (values tree '() (add-exit "~catch" (push-scope dict))))
       
      ((Catch (Identifier ,name) ,block)
       (values tree '() (add-lexical name dict)))
       
      ((FunctionDeclaration (Identifier ,name) . ,rest)
       (values tree '()
	       (add-exit "~return"
			 (add-lexical "this"
				      (push-scope (add-symboldef name dict))))))
      
      ((FunctionExpression (Identifier ,name) . ,rest)
       (values tree '()
	       (add-exit "~return"
			 (add-lexical "this"
				      (add-lexical name (push-scope dict))))))
      
      ((FunctionExpression . ,rest)
       (values tree '()
	       (add-exit "~return"
			 (add-lexical "this"
			    (add-symboldef "*anon*" (push-scope dict))))))
      
      ((FormalParameterList . ,idlist)
       ;; For all functions we just use rest arg and then express each
       ;; var reference as (list-ref @args index)
       ;; Another option is to use case-lambda ...
       (let* ((args (add-lexical "@args" dict))
	      (gsym (list-ref (car args) 3)) ; need gensym ref
	      (dikt (fold
		     (lambda (name indx seed)
		       (acons name `(call (toplevel list-ref)
					  (lexical @args ,gsym)
					  (const ,indx))
			      seed))
		     args
		     (map cadr idlist)
		     (let iter ((r '()) (n (length idlist))) ;; n-1 ... 0
		       (if (zero? n) r (iter (cons (1- n) r) (1- n))))
		     ))
	      )
	 (values tree '() dikt)))
      
      ((SourceElements . ,elts) ;; a list of statements and fctn-decls
       ;; Fix up list of source elements.
       ;; 1) Remove EmptyStatements.
       ;; 2) If LabelledStatement has EmptyStatement, merge with following
       ;;    do, while, for or switch.  Otherwise remove.
       ;; 3) Make to VDL always followed by a Block to end of SourceElements.
       (let* ((elts (remove-empties elts))
	      (elts (cleanup-labels elts))
	      (elts (if (top-level? dict) elts (fold-in-blocks elts))))
	 (values (cons 'SourceElements elts) '() dict)))

      (else
       ;;(sferr "fD: otherwise\n") (pperr tree)
       (values tree '() dict))
      ))

  (define (opcall-node op seed kseed kdict)
    (values (cons (rev/repl 'call (jslib-ref op) kseed) seed) kdict))
  
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

       ;; Identifier: handled in fD above

       ;; PrimaryExpression (w/ ArrayLiteral or ObjectLiteral only)
       ((PrimaryExpression)
	(values (cons (car kseed) seed) kdict))
      
       ;; ArrayLiteral
       ;; mkary is just primitive vector
       ((ArrayLiteral)
	(let ((exp `(call (@@ ,jslib-mod mkary) (car kseed))))
	  (values (cons exp seed) kdict)))
       
       ;; ElementList
       ((ElementList)
	(values (cons (rtail kseed) seed) kdict))

       ;; Elision: convert to list of js:undefined
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
	 (cons `(call (@@ ,jslib-mod mkobj) ,@(rtail kseed)) seed)
	 kdict))

       ;; PropertyNameAndValue
       ((PropertyNameAndValue)
	(values (cons* (car kseed) (cadr kseed) seed) kdict))

       ;; PropertyName
       ((PropertyName)
	(values (cons `(const ,(car kseed)) seed) kdict))

       ;; ooa-ref (object-or-array ref), a cons cell: (dict name)
       ;; obj-ref: converted to ooa-ref in fD
       ;; => (cons <expr> <name>)
       ;; a bit ugly now ???
       ((ooa-ref)
	(values
	 (cons `(prim-call cons
			   (call ,(jslib-ref 'js-resolve) ,(cadr kseed))
			   ,(car kseed))
	       seed) kdict))

       ;; new: for now just call object
       ((new) (values (cons (car kseed) seed) kdict))
       
       ;; CallExpression ;; this should probably insert a (de-ref fct)(args)
       ;; Should defined be boxed?
       ((CallExpression) ;; need to deal with "this"
	;;(pperr (cons* 'apply (cadr kseed) (car kseed)))
	(values (cons (cons* 'call (cadr kseed) (car kseed)) seed) kdict))

       ;; ArgumentList
       ((ArgumentList) ;; append-reverse-car ??? 
	(values (cons (rtail kseed) seed) kdict))

       ;; post-inc
       ((post-inc)
	(values (cons (op-on-ref (car kseed) 'js:+ 'post) seed) kdict))
	
       ;; post-dec
       ((post-dec)
	(values (cons (op-on-ref (car kseed) 'js:- 'post) seed) kdict))

       ;; delete
       ;; void
       ;; typeof

       ;; pre-inc
       ((pre-inc)
	(values (cons (op-on-ref (car kseed) 'js:+ 'pre) seed) kdict))

       ;; pre-dec
       ((pre-dec)
	(values (cons (op-on-ref (car kseed) 'js:- 'pre) seed) kdict))

       ;; pos neg ~ not
       ((pos) (opcall-node 'js:pos seed kseed kdict))
       ((neg) (opcall-node 'js:neg seed kseed kdict))
       ((lognot) (opcall-node 'js:lognot seed kseed kdict))
       ((not) (opcall-node 'js:not seed kseed kdict))

       ;; mul div mod add sub
       ((mul) (opcall-node 'js:* seed kseed kdict))
       ((div) (opcall-node 'js:/ seed kseed kdict))
       ((mod) (opcall-node 'js:% seed kseed kdict))
       ((add) (opcall-node 'js:+ seed kseed kdict))
       ((sub) (opcall-node 'js:- seed kseed kdict))
       
       ;; lshift rshift rrshift
       ((lshift) (opcall-node 'js:lshift seed kseed kdict))
       ((rshift) (opcall-node 'js:rshift seed kseed kdict))
       ((rrshift) (opcall-node 'js:rrshift seed kseed kdict))

       ;; lt gt le ge
       ((lt) (values (cons (op-call 'js:lt kseed) seed) kdict))
       ((gt) (values (cons (op-call 'js:gt kseed) seed) kdict))
       ((le) (values (cons (op-call 'js:le kseed) seed) kdict))
       ((ge) (values (cons (op-call 'js:ge kseed) seed) kdict))
       
       ;; instanceof
       ;; in
       
       ;; eq neq eq-eq neq-eq
       ((eq) (values (cons (op-call 'js:eq kseed) seed) kdict))
       ((neq) (values (cons (op-call 'js:neq kseed) seed) kdict))
       ((eq-eq) (values (cons (op-call 'js:neq-eq kseed) seed) kdict))
       ((neq-eq) (values (cons (op-call 'js:neq-eq kseed) seed) kdict))

       ;; bit-and bit-xor bit-or
       ((bit-and) (values (cons (op-call 'js:bit-and kseed) seed) kdict))
       ((bit-xor) (values (cons (op-call 'js:bit-xor kseed) seed) kdict))
       ((bit-or) (values (cons (op-call 'js:bit-or kseed) seed) kdict))

       ;; and or
       ((and) (values (cons (op-call 'js:and kseed) seed) kdict))
       ((or) (values (cons (op-call 'js:or kseed) seed) kdict))

       ;; ConditionalExpression => (if expr a b)
       ((ConditionalExpression)
	(values
	 (cons `(if ,(caddr kseed) ,(cadr kseed) ,(car kseed)) seed) kdict))

       ;; AssignmentExpression
       ;; assign mul-assign div-assign od-assign add-assign sub-assign
       ;; lshift-assign rshift-assign rrshift-assign and-assign
       ;; xor-assign or-assign
       ((AssignmentExpression)
	(values (cons (op-assn kseed) seed) kdict))

       ;; expr-list

       ;; Block
       ((Block)
	(let* ((tail (rtail kseed))
	       (exp1 (if (pair? tail) (car tail) #f))
	       (blck (if (and exp1 (eqv? 'bindings (car exp1)))
			 (make-let (cdar tail) (cdr tail))
			 (block tail))))
	  (values (cons blck seed) kdict)))

       ((StatementList)
	(let* ((tail (rtail kseed))
	       (exp1 (if (pair? tail) (car tail) #f))
	       (blck (if (and exp1 (eqv? 'bindings (car exp1)))
			 (make-let (cdar tail) (cdr tail))
			 (block tail))))
	  (values (cons blck seed) kdict)))

       ;; VariableStatement
       ((VariableStatement)
	(values (cons (car kseed) seed) kdict))

       ;; VariableDeclarationList
       ((VariableDeclarationList)
	(let* ((top (top-level? dict))
	       (tag (if top 'begin 'bindings)) ; begin or bindings for let
	       (tail (rtail kseed)))
	  (values (cons (cons tag tail) seed) kdict)))
       
       ;; VariableDeclaration
       ((VariableDeclaration)
	(let* ((top (top-level? dict))
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
       ((IfStatement)
	(values (cons (if (= 3 (length kseed))
			  `(if ,(cadr kseed) ,(car kseed) (void))
			  `(if ,(caddr kseed) ,(cadr kseed) ,(car kseed)))
		      seed) kdict))

       ;; ======================================================================
       ;; @subheading Iteration with @code{do}, @code{while} and @code{for}
       ;;
       ;; @item During fD we push scope w/ ~exit, the abort tag.
       ;; @item During fU we use that tag to abort for continue and break
       ;; @item for "switch" we map the continue handler to (abort #t) with
       ;;       the parent tag
       ;;
       ;; pattern for continue and break is
       ;; @example
       ;; (letrec
       ;;     ((iloop (lambda ()
       ;; 	      (stmt)
       ;; 	      (if (kond) (iloop))))
       ;;      (oloop (lambda ()
       ;; 	      (prompt
       ;; 	       (lambda () (iloop) #f)
       ;; 	       (lambda (k cont?) (if cont? (kond) #f))))))
       ;;   (oloop))
       ;; @example
       ;; @noindent
       ;; where continue=(abort #t) and break=(abort #f)

       
       ;; do: "do" stmt "while" expr ;
       ((do)
	(let* ((kond (car kseed)) (stmt (cadr kseed))
	       (xtag (find-exit-tag "~exit" kdict))
	       (isym (jsym)) (iloop-ref `(lexical ~iloop ,isym))
	       (osym (jsym)) (oloop-ref `(lexical ~oloop ,osym))
	       (csym (jsym)) (cont?-ref `(lexical ~cont? ,csym))
	       (ibody `(seq ,stmt (if ,kond (call ,iloop-ref) (void))))
	       (hdlr `(if ,cont?-ref ,kond (const #f)))
	       (obody `(if ,(with-exit-handler
			     xtag `(seq (call ,iloop-ref) (const #f))
			     hdlr cont?-ref)
			   (call ,oloop-ref)
			   (void)))
	       (body `(letrec (~iloop ~oloop) (,isym ,osym)
			      (,(make-thunk ibody) ,(make-thunk obody))
			      (call ,oloop-ref))))
	  (values (cons body seed) (pop-scope kdict))))

       ((while)
	(let* ((kond (car kseed)) (stmt (cadr kseed))
	       (xtag (find-exit-tag "~exit" kdict))
	       (isym (jsym)) (iloop-ref `(lexical ~iloop ,isym))
	       (osym (jsym)) (oloop-ref `(lexical ~oloop ,osym))
	       (csym (jsym)) (cont?-ref `(lexical ~cont? ,csym))
	       (ibody `(if ,kond
			   (seq ,stmt (seq (call ,iloop-ref) (void)))
			   (void)))
	       (hdlr cont?-ref)
	       (obody `(if ,(with-exit-handler
			     xtag
			     `(seq (call ,iloop-ref) (seq (const #f) (void)))
			     hdlr cont?-ref)
			   (call ,oloop-ref) (void)))
	       (body `(letrec (~iloop ~oloop) (,isym ,osym)
			      (,(make-thunk ibody) ,(make-thunk obody))
			      (call ,oloop-ref))))
	  (values (cons body seed) (pop-scope kdict))))

       ;; for    : pop-scope needed
       ;; for-in : pop-scope needed

       ;; NoExpression (used by for and for-in)
       ((NoExpression)
	(values (cons '(void) seed) kdict))
       
       ;; ContinueStatement: abort w/ zero args
       ((ContinueStatement)
	(values
	 (cons
	  (if (> (length kseed) 1)
	      (error "unsupported JS: continue <label>")
	      `(abort (const ,(find-exit-tag "~exit" kdict))
		      ((const #t)) (const '())))
	  seed) kdict))

       ;; BreakStatement: abort w/ zero args
       ((BreakStatement)
	(values
	 (cons
	  (if (> (length kseed) 1)
	      (error "unsupported JS: break <label>")
	      `(abort (const ,(find-exit-tag "~exit" kdict)) ((const #f))
		      (const '())))
	  seed) kdict))

       ;; ReturnStatement: abort w/ one arg
       ((ReturnStatement)
	(values
	 (cons `(abort (const ,(find-exit-tag "~return" kdict))
		       (,(if (> (length kseed) 1)
			     (car kseed)	       ; argument
			     (lookup "this" kdict)))   ; default
		       (const '()))
	       seed) kdict))

       ;; WithStatement

       ;; ======================================================================
       ;; @subheading Switch Statement
       ;; The pattern for SwitchStatment is as follows:
       ;; given
       ;; @example
       ;;   switch (v) {
       ;;   case A: A-stmts ... ; 
       ;;   case B: B-stmts ... ; 
       ;;   case C: C-stmts ... ;
       ;;   default: D-stmts ... ;
       ;;   case E: E-stmts ... ; 
       ;;   case F: F-stmts ... ; 
       ;;   case G: G-stmts ... ; 
       ;; @end example
       ;; @noindent
       ;; generate
       ;; @example
       ;;  (let ((key #f))
       ;;    (let ((key (if key #t (equal? val A))))
       ;;       (if key A-stmts)
       ;;       (let ((key (if key #t (equal? val B))))
       ;;          (if key B-stmts)
       ;;          (let ((key (if key #t (equal? val C))))
       ;;             (if key C-stmts))))
       ;;  (let ((key #f))
       ;;    (let ((key (if key #t (equal? val E))))
       ;;       (if key E-stmts)
       ;;       (let ((key (if key #t (equal? val F))))
       ;;          (if key F-stmts)
       ;;          (let ((key (if key #t (equal? val G))))
       ;;             (if key G-stmts))))
       ;;  D-stmts
       ;; @end example
       
       ;; SwitchStatement: pop-scope needed, also continue => break
       ;; CaseBlock CaseClauses CaseClause DefaultClause
       ((SwitchStatement)
	(let* ((v-ref (lookup "~val" kdict))	  ; val ref
	       (v-val (resolve-ref (cadr kseed))) ; expr as l-val
	       (body (pmatch (car kseed)
		       (((let . ,A-clz) (seq . ,def) (let . ,B-clz))
			(block (cons* `(let . ,A-clz) `(let . ,B-clz) def)))
		       (((let . ,A-clz) (seq . ,def))
			(block (cons `(let . ,A-clz) def)))
		       (((seq . ,def) (let . ,B-clz))
			(block (cons `(let . ,B-clz) def)))
		       (((let . ,A-clz)) `(let . ,A-clz))))
	       (csym (jsym)) (cont?-ref `(lexical ~cont? ,csym))
	       (px (lookup "~exit" dict)) ; parent exit
	       (hdlr `(if ,cont?-ref
			  (abort (const ,px) ((const #t)) (const '())) (void)))
	       (body `(let (,(cadr v-ref)) (,(caddr v-ref)) (,v-val) ,body))
	       (body (with-exit-handler (find-exit-tag "~exit" kdict) body
					hdlr cont?-ref))
	       )
	  (values (cons body seed) (pop-scope kdict))))

       ((CaseBlock)
	(values (cons (rtail kseed) seed) kdict))
       
       ((CaseClauses)
	(values
	 (cons 
	  (let ((val (lookup "~val" kdict)))
	    (let iter ((next '(void)) (sym (jsym)) (ks kseed))
	      (if (eq? (car ks) 'CaseClauses)
		  `(let (~key) (,sym) ((const #f)) ,next)
		  (let ((psym (jsym)))
		    (iter (make-case val sym psym (car ks) next)
			  psym (cdr ks))))))
	  seed)
	 kdict))
       
       ((CaseClause)
	(values (cons kseed seed) kdict))

       ((DefaultClause)
	(values
	 (if (eqv? 1 (length kseed)) seed (cons (car kseed) seed))
	 kdict))
       
       ;; LabelledStatement
       ((LabelledStatement)
	(values (cons (car kseed) seed) kdict))

       ;; ======================================================================
       ;; @subheading Exceptions
       ;; @example
       ;; try { stmts } catch (var) { stmts }
       ;; try { stmts } finally { stmts }
       ;; try { stmts } catch (var) { stmts } finally { stmts }
       ;; @end example
       ;; @example
       ;; (begin (prompt (const ~catch) try-stmts
       ;;           (lambda-case (((k var) #f #f #f (,k-sym ,var-sym))
       ;;                         catch-stmts)))
       ;;        finally-stmts)
       ;; @end example
       ;; @no indent
       ;; so the catch needs to return the lambda-case I think.
       
       ;; ThrowStatement throw expression
       ((ThrowStatement)
	(values (cons `(abort (const ,(find-exit-tag "~catch" kdict))
			      (,(car kseed)) (const '())) seed) kdict))
       
       ;; TryStatement Catch Finally
       ((TryStatement)
	(let* ((rseed (rtail kseed))
	       (ctag (find-exit-tag "~catch" kdict))
	       (dummy `(lexical ~arg ,(jsym)))
	       (body (pmatch rseed
		       ((,try-stmts (lambda-case . ,rest))
			`(prompt (const ,ctag) ,try-stmts ,(cadr rseed)))
		       ((,try-stmts ,finally)
			`(seq
			   ,(with-exit-handler ctag try-stmts '(void) dummy)
			   ,finally))
		       (,otherwise
			`(seq
			   (prompt (const ,ctag) ,(car rseed) ,(cadr rseed))
			   ,(caddr rseed))))))
	  (values (cons body seed) (pop-scope kdict))))

       ((Catch)
	(let* ((var-n (cadr (cadr tree))) ; name from tree
	       (var-r (caddr (lookup var-n kdict)))
	       (body `(lambda-case (((k ,(string->symbol var-n))
				     #f #f #f () (,(jsym) ,var-r))
				    ,(car kseed)))))
	  (values (cons body seed) kdict)))

       ((Finally)
	(values (cons (car kseed) seed) kdict))

       ;; ======================================================================

       ;; FunctionDeclaration (see also fU)
       ((FunctionDeclaration)
	(let* ((il-name (cadr kseed))
	       (name (case (car il-name)
		       ((@ @@) (caddr il-name)) (else (cadr il-name))))
	       (this (caddr (lookup "this" kdict)))
	       (args (list-ref (lookup "@args" kdict) 2))
	       (ptag (lookup "~return" kdict))
	       (body (with-exit-arg ptag (block (car kseed))))
	       (fctn `(define ,name
			,(make-function this args body #:name name))))
	  (values (cons fctn seed) (pop-scope kdict))))

       ;; FunctionExpression
       ((FunctionExpression)
	(let* ((this (lookup "this" kdict))
	       (args (list-ref (lookup "@args" kdict) 2))
	       (ptag (lookup "~return" kdict))
	       (body (with-exit-arg ptag (block (car kseed))))
	       (fctn (make-function this args body)))
	  (values (cons fctn seed) (pop-scope kdict))))

       ;; FormalParameterList
       ((FormalParameterList) ;; all in @code{@@args}.
	(values seed kdict))

       ;; Program
       ((Program)
	(values (block (car kseed)) kdict))
       
       ;; SourceElements
       ((SourceElements)
	(values (cons (rtail kseed) seed) kdict))

       (else
	(cond
	 ((null? seed) (values (reverse kseed) kdict))
	 (else (values (cons (reverse kseed) seed) kdict)))))))

  (define (fH leaf seed dict)
    (values (cons leaf seed) dict))

  ;; We generate a dictionary with the env (module?) available at the top.
  (let ((dict (acons '@top #t (acons '@M env JSdict)))
	(sexp `(*TOP* ,exp)))
    (call-with-values
	(lambda () (foldts*-values fD fU fH sexp '() dict))
      (lambda (seed dict) seed))))


;; @deffn {Procedure} compile-tree-il exp env opts => 
(define (compile-tree-il exp env opts)
  ;;(sferr "sxml:\n") (pperr exp)
  (if exp
      (let* ((xrep (js-sxml->tree-il/ext exp env opts)))
	;;(sferr "tree-il:\n") (pperr xrep)
	;;(values (parse-tree-il '(const "[skip compile & execute]")) env env)
	(values (parse-tree-il xrep) env env)
	)
      (values (parse-tree-il '(const "javascript parse error")) env env)))

;; --- last line ---
