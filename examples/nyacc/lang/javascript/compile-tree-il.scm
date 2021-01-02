;; compile javascript sxml from parser to tree-il

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

;;; Description:

;; My goal in this development was to get experience with comping SXML trees
;; to tree-il: putting together patterns and utility procedures for converting
;; common structures (e.g, return <expr>, break).  It might be fun to also try
;; converting to CPS. (But need to read more on this.) -- Matt

;; Building the parser and compiler can be a little tricky.  It has to work
;; interactively, for loaded files, and for maybe other stuff.
;; The reader (or parser) needs to be able to parse one top-level form and
;; return.  Note that usually parsers are designed to read files, so they
;; look for EOF to stop parsing.  In interactive mode, we want to stop
;; when a top-level form ends with "\n".

;; You should look at the source for (ice-9 boot-9) to see how load works,
;; and at the source for (system base compile) to see how compilation gets
;; done.  I'm still absorbing those and don't know how much insight I can
;; provide. -- Matt - 15 Jul 2018

;;; Notes:

;; @itemize
;; @item JS functions will need to be re-implemented as objects, with the
;;       `[[Call]]' property used to make calls.
;; @item don't support the arguments property in functions
;; @item could maybe handle this using
;;  d.f1 = function... => d.f1 = (let ((this d)) f1)
;;  but then d1.f = d0.f does not work
;; @item (void) == (const *unspecified*) i think
;; @item need atomic-box?
;; @end itemize

;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/\
;;   Details_of_the_Object_Model
;; If Manager: Employee
;; function Manager() {
;;   Employee.call(this);
;;   this.reports = [];
;; }
;; Manager.prototype = Object.create(Employee.prototype);
;; Manager.prototype.constructor = Manager;

;; TODO:
;; @itemize
;; @item Imeplement @code{with}.
;; @item Imeplement @code{for}.
;; @item Imeplement @code{for-in}.
;; @item Update to es5.
;; @item Implement objects and prototypes correctly.
;; @item Implement unary and binary operators (in xlib-01.scm) correctly.
;; @end itemize
;;
;; DONE:
;; @itemize
;; @item add let only allow var at top-level and function start scope
;; @end itemize

;;; Code:

(define-module (nyacc lang javascript compile-tree-il)
  #:export (compile-tree-il show-javascript-sxml show-javascript-xtil)
  #:use-module (nyacc lang javascript xlib)
  #:use-module (nyacc lang sx-util)
  #:use-module (nyacc lang nx-util)
  #:use-module ((sxml fold) #:select (foldts*-values))
  #:use-module ((srfi srfi-1) #:select (fold append-reverse))
  #:use-module (language tree-il)
  #:use-module (ice-9 match)
  )
(use-modules (ice-9 pretty-print))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr tree)
  (pretty-print tree (current-error-port) #:per-line-prefix "  "))

;; === portability ===================
;; no longer supports guile 2.0: assuming 2.2

(define (jsym) (gensym "JS~"))

;; =======================


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
;; CHANGE THIS.  Use optional arguments for everything

;; @subheading non-tail return
;; need to use prompts here, I think ... Hey just use let/ec ?
;; @example
;; (let/ec return ((var1 val1) (var2 val2)) ... (return x) ...)
;; @end example

;; ProgramElements occurs in a Program (top-level).
;; We translate Program to seq (aka begin).
;; FunctionElements occurs in a Function.
;; We translate FunctionBody to let.

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

(define xlib-mod '(nyacc lang javascript xlib))
(define (xlib-ref name) `(@@ (nyacc lang javascript xlib) ,name))

(define undefined '(void))
(define null (xlib-ref 'js:null))

;; may need to push-level (blocks) and push-scope (functions)

;; add label for continue break.  The value will be a pair
;; with car the continue ref and cdr the break ref
(define (add-label name dict)
  (acons name (cons #f #f) dict))

;;(define (c-name->guile name)
;;  ;; | scm_  | _ - | ! _x | _to_ -> | _less < | _gr > | _leq <= | _geq >= |
;;  (string-map (lambda (ch) (if (char=? ch #\_) #\- ch)) name))

(define xlib-module (resolve-module '(nyacc lang javascript xlib)))
		       
#;(define (x-lookup name dict)
  (cond
   ((not dict) #f)
   ((null? dict) #f)
   ((assoc-ref dict name))		; => value
   ((assoc-ref dict '@P) =>		; parent level
    (lambda (dict) (lookup name dict)))
   ((find-in-env name (assoc-ref dict '@M)))
   ((find-in-env (c-name->guile name) (assoc-ref dict '@M)))
   ((find-in-env name xlib-module))
   (else #f)))

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


;; === codegen procedures =============

;; @deffn {Procedure} make-let bindings exprs
;; Generates a Tree-IL let form from arguments, where @var{bindings} looks like
;; @noindent
;; and @var{exprs} is a list of expressions, to something like
;; @example
;; (let (v w) (JS~5897 JS~5898) (#<unspecified> (const 3)) . exprs)
;; @end example
;; @end deffn
(define (make-let bindings exprs)
  (let loop ((names '()) (gsyms '()) (vals '()) (bindings bindings))
    (if (null? bindings)
	`(let ,(reverse names) ,(reverse gsyms) ,(reverse vals) ,(block exprs))
	(loop (cons (list-ref (car bindings) 1) names)
	      (cons (list-ref (car bindings) 2) gsyms)
	      (cons (list-ref (car bindings) 3) vals)
	      (cdr bindings)))))

;; @deffn {Procedure} wrap-bindings body => body
;; @example
;; (seq (bindings ...) ...) => (let ... (seq ...))
;; expr => expr
;; @end example
;; @noindent where bindings may look like
;; @example
;; (bindings (bind v v~5897 (void)) (bind w w~5898 (const 3)))
;; @end example
;; @noindent
;; Oh, I think this needs to use @code{letrec*}.
;; @end deffn
(define (wrap-bindings body)
  ;;(sferr "wrap:\n  body\n") (pperr body)
  (let loop1 ((bindings '()) (rest body))
    (match rest
      (`(seq (bindings . ,bnds) ,rst)
       (loop1 (fold cons bindings bnds) rst))
      (_
       ;;(sferr "  bindings, rest:\n") (pperr bindings) (pperr rest)
       (if (null? bindings)
	   body
	   (let loop2 ((names '()) (gsyms '()) (inits '()) (binds bindings))
	     (if (null? binds)
		 `(letrec* ,names ,gsyms ,inits ,rest)
		 (loop2 (cons (list-ref (car binds) 1) names)
			(cons (list-ref (car binds) 2) gsyms)
			(cons (list-ref (car binds) 3) inits)
			(cdr binds)))))))))

;; @deffn {Procedure} make-function name this args body
;; Pass arguments as optional and add @code{this} keyword argument.@*
;; Note: Change this @code{make-arity} (see tcl/c-t-il.scm).
;; @end deffn.
(define (make-function name this args body)
  (unless args (error "no args"))
  (let* ((meta '((language . nx-javascript)))
	 (meta (if name (cons `(name . ,name) meta) meta)))
    `(lambda ,meta
       (lambda-case ((()			; req
		      ,(map car args)	; opt
		      #f			; rest
		      (#f (#:this this ,this)) ; kw
		      (,@(map (lambda (v) undefined) args) ,undefined) ; inits
		      (,@(map cadr args) ,this)) ; syms
		     ,body)))))

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

;; NOT USED -- NOT NEEDED
;; @deffn {Procedure} resolve-ref ref => exp
;; WARNING: I think this is more subtle than I am making it.@*
;; Resolve a possible reference (lval) to an expression (rval).
;; Right now this will convert an object-or-array ref to its value
;; via @code{js-ooa-get}.  Otherwise just return the value.
;; @end deffn
(define (resolve-ref ref)
  (let ((tag (car ref)))
    (if (or (vector? tag) (hash-table? tag))
	`(call ,(xlib-ref 'js-ooa-get) ,ref)
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
		(else `(call ,(xlib-ref 'js-ooa-get) ,ref))))
	 (loc `(lexical ~ref ,sym))
	 (sum `(call ,(xlib-ref op) (const 1) ,loc))
	 (set (case (car ref)
		((toplevel lexical) `(set! ,ref ,sum))
		(else `(call ,(xlib-ref 'js-ooa-put) ,ref ,sum))))
	 (rval (case ord ((pre) val) ((post) loc))))
    `(let (~ref) (,sym) (,val) (seq ,set (seq ,rval (void))))))

;; for lt + rt, etc
(define (op-call op kseed)
  (rev/repl 'call (xlib-ref op) kseed))
(define (op-call/prim op kseed)
  (rev/repl 'prim-call op kseed))

;; @deffn {Procedure} op-assn kseed => `(set! lhs rhs)
;; op-assn: for lhs += rhs etc
;; @end deffn
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
	    `(set! ,lhs (call (@@ ,xlib-mod ,op) lhs rhs))
	    `(set! ,lhs ,rhs))))))

(define (opcall-node op seed kseed kdict)
  (values (cons (rev/repl 'call (xlib-ref op) kseed) seed) kdict))
(define make-opcall xlib-mod)
  
;; ====================================
	 
;; @deffn {Procedure} xlang-sxml->xtil exp env opts
;; Compile extension SXML tree to external Tree-IL representation.
;; This one is public because it's needed for debugging the compiler.
;; @end deffn
(define-public (xlang-sxml->xtil exp env opts)

  ;; In the case where we pick off ``low hanging fruit'' we need to coordinate
  ;; the actions of the up and down handlers.   The down handler will provide
  ;; a kid-seed in order and generate a null list.  The up handler, upon seeing
  ;; a null list, will just incorporate the kids w/o the normal reverse.

  ;; @deffn {Procedure} remove-empties elements => elements
  ;; @end deffn
  (define (remove-empties elements)
    (let loop ((elts elements))
      (if (null? elts) '()
	  (let ((elt (car elts)) (rest (cdr elts)))
	    (if (eq? (car elt) 'EmptyStatement)
		(loop rest)
		(cons elt (loop rest)))))))

  ;; @deffn {Procedure} labelable-stmt? stmt => #f|stmt
  ;; This predicate determines if the statement can have a preceeding label.
  ;; @end deffn
  (define (labelable-stmt? stmt)
    (memq (car stmt) '(do while for for-in BreakStatement LabelledStatement)))
  
  ;; @deffn {Procedure} cleanup-labels elements => elements
  ;; The argument @var{elements} is the tail of @code{ProgramElements}
  ;; or @code{FunctionElements}.  This procedure assumes all top-level
  ;; @code{EmptyStatements} have been removed.  This procedure reduces
  ;; @code{LabelledStatement}s to the form
  ;; @example
  ;; @dots{} (LabelledStatement id iter-stmt) @dots{}
  ;; @dots{} (LabelledStatement id (LabelledStatement id iter-stmt)) @dots{}
  ;; @end example
  ;; @noindent
  ;; where @code{iter-stmt} is @code{do}, @code{while}, @code{for} or
  ;; @code{switch}, or removes them if not preceeding iteration statement.
  ;; @end deffn
  (define (cleanup-labels src-elts-tail)
    (let loop ((src src-elts-tail))
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
			      (values id '(EmptyStatement)
				      (cons stmt rest))))))
		(lambda (id stmt rest)
		  (if (eqv? 'EmptyStatement (car stmt))
		      (begin
			(simple-format (current-error-port)
				       "removing misplaced label: ~A\n"
				       (cadr id))
			(loop rest))
		      (cons `(LabelledStatement ,id ,stmt) (loop rest)))))
	      (cons (car src) (loop (cdr src)))))))

  ;; @deffn {Procedure} hoist-decls elements => elements
  ;; Move all variable declarations to front and replace old ones with
  ;; assignment.
  ;; @end deffn
  (define hoist-stmts '(VariableStatement LetStatement))
  (define (split-dstmt dstmt hoisted rest) ;; => hoisted rest
    ;; move Let/VariableStatement to hoisted, moving initializers
    ;; to decls as assignment statements
    (let loop ((hdecls '()) (rest rest) (decls (cdadr dstmt)))
      (if (null? decls)
	  (values (cons `(,(car dstmt) (DeclarationList ,hdecls)) hoisted)
		  rest)
	  (let* ((decl (car decls))
		 (init (if (= 3 (length decl)) (caddr decl) #f))
		 (decl (if init (list (car decl) (cadr decl)) decl)))
	    (loop (cons decl hdecls)
		  (if init
		      (cons `(ExpressionStatement
			      (AssignmentExpression
			       (PrimaryExpression ,(cadr decl))
			       (assign "=") ,(cadr init)))
			    rest)
		      rest)
		  (cdr decls))))))
  (define (hoist-decls elements)
    (let ((tail (let loop ((elts elements)) ; tail after Let/VarStatements
		  (cond
		   ((null? elts) elts)
		   ((not (memq (caar elts) hoist-stmts)) elts)
		   (loop (cdr elts))))))
      (let loop ((hoisted '()) (rest '()) (elts tail))
	(cond
	 ((null? elts)
	  (if (null? hoisted)
	      elements			   ; nothing hoisted
	      (let loop2 ((elts elements)) ; rebuild statement list
		(if (eq? elts tail) (append-reverse hoisted (reverse rest))
		    (cons (car elts) (loop2 (cdr elts)))))))
	 ((memq (caar elts) hoist-stmts)
	  (call-with-values		; decl=>hoisted init=>rest
	      (lambda () (split-dstmt (car elts) hoisted rest))
	    (lambda (hoisted rest)
	      (loop hoisted rest (cdr elts)))))
	 (else
	  (loop hoisted (cons (car elts) rest) (cdr elts)))))))
	  
  
  ;; @deffn {Procedure} fold-in-blocks elts-tail => elts-tail
  ;; NOTE: THIS IS NOT CORRECT.  JS DOES NOT SCOPE BLOCKS.@*
  ;; Look through source elements.  Change every var xxx to a
  ;; @example
  ;; (@dots{} (VariableStatement (DeclarationList ...)) @dots{})
  ;; @end example
  ;; @noindent
  ;; (@dots{} (DeclarationList ...) (Block @dots{}))
  ;; @example
  ;; @dots{} @{ var a = 1; @dots{} @}
  ;; @end example
  ;; @noindent
  ;; We assume no elements of @code{FunctionElements} is text.
  ;; @end deffn
  (define (x-fold-in-blocks src-elts-tail)
    (let loop ((src  src-elts-tail))
      (if (null? src) '()
	  (let ((elt (car src)) (rest (cdr src)))
	    (if (eq? (car elt) 'VariableStatement)
		(list (cons* 'Block (cadr elt) (loop rest)))
		(cons elt (loop rest)))))))

  ;; @deffn {Procedure} check-scoping elements
  ;; Check blocks to make sure @code{var} is only used in blocks at entry
  ;; to functions.  Otherwise issue an error message.
  ;; @end deffn
  (define (check-scoping elements)
    elements)
		     
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
       (let ((ref (lookup name dict)))
	 (cond
	  (ref
	   (values '() ref dict))
	  (else
	   ;; maybe not defined, assume toplevel
	   (sferr "javascript: maybe undefined: ~A\n" name)
	   (values '() `(toplevel ,(string->symbol name)) dict)))))

      ((PrimaryExpression (this))
       (let ((ref (lookup "this" dict)))
	 (if (not ref) (error "javascript: not found: \"this\""))
	 (values '() ref dict)))
      
      ((PrimaryExpression (NullLiteral ,null))
       (values '() '(const js:null) dict))

      ((BooleanLiteral ,true-or-false)
       (values '() `(const ,(char=? (string-ref true-or-false 0) #\t)) dict))

      ((PrimaryExpression (NumericLiteral ,val))
       (values '() `(const ,(string->number val)) dict))

      ((PrimaryExpression (StringLiteral ,str))
       (values '() `(const ,str) dict))

      ((CallExpression
	(obj-ref ,expr (PrimaryExpression (Identifier ,name))) ,arg-list)
       (sferr "1\n")
       (values `(obj-CallExpression ,expr ,name ,arg-list) '() dict))
      ((CallExpression (obj-ref ,expr (Identifier ,name)) ,arg-list)
       (sferr "2\n")
       (values `(obj-CallExpression ,expr ,name ,arg-list) '() dict))
      ((CallExpression . ,rest)
       (values tree '() dict))

      ((PropertyNameAndValue (Identifier ,name) ,expr)
       (values `(PropertyNameAndValue (PropertyName ,name) ,expr) '() dict))

      ((obj-ref ,expr (Identifier ,name))
       (values `(ooa-ref ,expr ,name) '() dict))

      ((Block . ,elts) ;; see comments on FunctionElements below
       (let* ((elts (remove-empties elts))
	      (elts (cleanup-labels elts)))
	 (values tree '() dict)))

      ;; Convert AssignmentExpression to:
      ;; 1) var-AssignmentExpression => ... (set! ...)
      ;; 2) obj-AssignmentExpression => ... (hash-set! ...)
      ;; 3) ooa-AssignmentExpression => ... (vector-set! ...)|(hash-set! ...)
      ;; TODO: check LeftHandSideExpression
      ((AssignmentExpression
	(@ . ,attr) (PrimaryExpression (Identifier ,name)) ,assn ,rhs)
       (values `(var-AssignmentExpression . ,(cdr tree)) '() dict))
      ((AssignmentExpression
	(@ . ,attr) (obj-ref ,expr (Identifier ,name)) ,assn ,rhs)
       (values
	`(obj-AssignmentExpression (@ . ,attr) ,expr ,name ,assn ,rhs)
	'() dict))
      ((AssignmentExpression (@ . ,attr) (ooa-ref ,expr ,expr) ,rhs)
       (values
	`(ooa-AssignmentExpression (@ . ,attr) ,expr ,expr ,rhs)
	'() dict))
      
      ((StatementList . ,stmts)
       (let* ((stmts (remove-empties stmts))
	      (stmts (check-scoping stmts)))
	 (values tree '() dict)))
      
      ((VariableDeclaration (Identifier ,name) . ,rest)
       (let* ((dict1 (add-symbol name dict))
	      (tree1 (lookup name dict1)))
	 (if (not tree1) (error "javascript coding error"))
	 (values `(VariableDeclaration ,tree1 . ,rest) '() dict1)))

      ((do . ,rest)
       (values tree '() (add-lexicals "break" "continue" (push-scope dict))))

      ((while . ,rest)
       (values tree '() (add-lexicals "break" "continue" (push-scope dict))))

      ((for . ,rest)
       (values tree '() (add-lexicals "break" "continue" (push-scope dict))))

      ((for-in . ,rest)
       (values tree '() (add-lexicals "break" "continue" (push-scope dict))))

      ((SwitchStatement . ,rest)
       (values tree '() (add-lexicals "swx~val" "break" (push-scope dict))))

      ((LabelledStatement (Identifier ,name) ,stmt)
       (values tree '() (add-label name dict)))

      ((TryStatement . ,expr)
       (values tree '() (add-lexical "catch" (push-scope dict))))
      
      ((Catch (Identifier ,name) ,block)
       (values tree '() (add-lexical name dict)))
      
      ((FunctionDeclaration (Identifier ,name) . ,rest)
       (values
	tree '()
	(add-lexicals "this" "return" (push-scope (add-symbol name dict)))))
      
      ((FunctionExpression (Identifier ,name) . ,rest)
       (values tree '() (add-lexicals "this" "return" name (push-scope dict))))

      ((FunctionExpression . ,rest)
       (values tree '()
	       (add-lexicals "this" "return"
			     (push-scope (add-symbol "*anon*" dict)))))
      
      ((FormalParameterList . ,idlist)
       (values
	tree '()
	(acons 'arguments-used? #f
	       (add-lexical "arguments"
			    (fold add-lexical dict (map cadr idlist))))))
      
      ((FunctionElements . ,elts)
       ;; Fix up list of function elements:
       ;; 1) Remove EmptyStatements.
       ;; 2) If LabelledStatement has EmptyStatement, merge with following
       ;;    do, while, for or switch.  Otherwise remove.
       ;; 3) Make to DeclList always followed by a Block to end of Elements.???
       (let* ((elts (remove-empties elts))
	      (elts (hoist-decls elts))
	      (elts (cleanup-labels elts)))
	 (values `(FunctionElements . ,elts) '() dict)))

      ((ProgramElements . ,elts) ;; a list of top-level statements
       (values tree '() dict))

      (,_
       ;;(sferr "fD: otherwise\n") (pperr tree)
       (values tree '() dict))
      ))

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
       
       ((*TOP*)
	(values (car kseed) kdict))

       ;; Identifier: handled in fD above

       ;; PrimaryExpression (w/ ArrayLiteral or ObjectLiteral only)
       ((PrimaryExpression)
	(values (cons (car kseed) seed) kdict))

       ;; ArrayLiteral
       ;; mkary is just primitive vector
       ((ArrayLiteral)
	(let ((exp `(call (@@ ,xlib-mod mkary) (car kseed))))
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
	(let* ((tail (rtail kseed)))
	  (values
	   (cons
	    (if (null? tail) `(call (toplevel make-hash-table)) (car tail))
	    seed)
	   kdict)))
       
       ;; PropertyNameAndValueList
       ((PropertyNameAndValueList)
	(sferr "prop list\n") (pperr (reverse kseed))
	(values
	 (cons `(call (@@ ,xlib-mod mkobj) ,@(rtail kseed)) seed)
	 kdict))

       ;; PropertyNameAndValue
       ((PropertyNameAndValue)
	(values (cons* (car kseed) (cadr kseed) seed) kdict))

       ;; PropertyName
       ((PropertyName)
	(values (cons `(const ,(string->symbol (car kseed))) seed) kdict))

       ;; ooa-ref (object-or-array ref), a cons cell: (dict name)
       ;; obj-ref: converted to ooa-ref in fD
       ;; => (cons <expr> <name>)
       ;; a bit ugly now ???
       ((ooa-ref)
	(sferr "ooaref:\n") (pperr (rtail kseed))
	(let* ((expr `(primcall cons
				 (call ,(xlib-ref 'js-resolve) ,(cadr kseed))
				 ,(car kseed)))
	       (tail (rtail kseed))
	       (ooa (list-ref tail 0))
	       (arg `(const ,(string->symbol (list-ref tail 1))))
	       (expr `(call ,(xlib-ref 'js:ooa-ref) ,ooa ,arg))
	       )
	  (pperr expr)
	  (values (cons expr seed) kdict)))

       ;; new
       ((new)
	(let* ((tail (rtail kseed))
	       (expr `(call ,(car tail) ,@(cadr tail) (const #:this)
			    (call (toplevel make-hash-table)))))
	  (values (cons expr seed) kdict)))
       
        ;; obj-CallExpression obj name args : add args #:this obj
       ((obj-CallExpression)
	(let* ((tail (rtail kseed))
	       (obj (list-ref tail 0))
	       (mem `(const ,(string->symbol (list-ref tail 1))))
	       (args (list-ref tail 2))
	       (args (append args (list `(const #:this) obj)))
	       (expr `(call (call ,(xlib-ref 'js:ooa-ref) ,obj ,mem) . ,args)))
	  ;;(sferr "obj-C\n") (pperr tail)
	  (values (cons expr seed) kdict)))
       
       ;; CallExpression
       ((CallExpression)
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
       ;; Note that assignment needs to return the value always
       ((var-AssignmentExpression)
	(let* ((tail (rtail kseed))
	       )
	  ;;(sferr "obj-Ass\n") (pperr (reverse kseed)) ;;(pperr tail)
	  (values (cons (op-assn kseed) seed) kdict)))

       ((obj-AssignmentExpression)
	(let* ((tail (rtail kseed)))
	  (values (cons '(void) seed) dict)))
       
       ;; expr-list

       ;; Block : has same elements as StatementList
       ;; except decl's will be let (by static semantics)
       ((Block)
	(values (cons (wrap-bindings (block (rtail kseed))) seed) kdict))

       ((StatementList)
	(values (cons (block (rtail kseed)) seed) kdict))

       ;; LetStatement
       ((LetStatement)
	(values (cons (car kseed) seed) kdict))

       ;; VariableStatement
       ((VariableStatement)
	(values (cons (car kseed) seed) kdict))

       ;; DeclarationList
       ((DeclarationList)
	(values
	 (acons (if (top-level? dict) 'begin 'bindings) (rtail kseed) seed)
	 kdict))
       
       ;; VariableDeclaration
       ((VariableDeclaration)
	(let* ((tail (rtail kseed))
	       (name (cadar tail))
	       (gsym (if (eq? 'lexical (caar tail)) (caddar tail) #f))
	       (init (if (null? (cdr tail)) '(void) (cadr tail))))
	  ;;(sferr "vdef: name=~S init=~S gsym=~S\n" name init gsym)
	  (values
	   (cons
	    (if (top-level? dict)
		`(define ,name ,init)
		`(bind ,name ,gsym ,init))
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

       ;; =====================================================================
       ;; @subheading Iteration with @code{do}, @code{while} and @code{for}
       ;;
       ;; @item During fD we push scope w/ ~exit, the abort tag.
       ;; @item During fU we use that tag to abort for continue and break
       ;; @item for "switch" we map the continue handler to (abort #t) with
       ;;       the parent tag

       ;; do: "do" stmt "while" expr ;
       ((do)
	(let* ((expr (car kseed)) (body (cadr kseed)))
	  (values (cons (make-do-while expr body kdict) seed)
		  (pop-scope kdict))))

       ;; while: while expr stmt
       ((while)
	(let* ((expr (cadr kseed)) (body (car kseed)))
	  (values (cons (make-while expr body kdict) seed)
		  (pop-scope kdict))))

       ;; for    : pop-scope needed
       ;; for-in : pop-scope needed

       ;; NoExpression (used by for and for-in)
       ((NoExpression)
	(values (cons '(void) seed) kdict))
       
       ;; ContinueStatement: abort w/ zero args
       ((ContinueStatement)
	(if (> (length kseed) 1)
	    (throw 'js-error "unsupported: break <label>"))
	(values
	 (cons `(abort ,(lookup "continue" kdict) () (const ())) seed)
	 kdict))

       ;; BreakStatement: abort w/ zero args
       ((BreakStatement)
	(if (> (length kseed) 1)
	    (throw 'js-error "unsupported: break <label>"))
	(values
	 (cons `(abort ,(lookup "break" kdict) () (const ())) seed)
	 kdict))

       ;; ReturnStatement: abort w/ one arg
       ((ReturnStatement)
	(values
	 (cons `(abort ,(lookup "return" kdict)
		       (,(if (> (length kseed) 1) (car kseed) undefined))
		       (const ()))
	       seed)
	 kdict))

       ;; WithStatement

       ;; ====================================================================
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
       
       ;; SwitchStatement: pop-scope needed
       ;; CaseBlock CaseClauses CaseClause DefaultClause
       ((SwitchStatement)
	(let* ((expr (cadr kseed))
	       (body (match (car kseed)
		       (`((let . ,A-clz) (seq . ,def) (let . ,B-clz))
			(block (cons* `(let . ,A-clz) `(let . ,B-clz) def)))
		       (`((let . ,A-clz) (seq . ,def))
			(block (cons `(let . ,A-clz) def)))
		       (`((seq . ,def) (let . ,B-clz))
			(block (cons `(let . ,B-clz) def)))
		       (`((let . ,A-clz)) `(let . ,A-clz))))
	       (vsym (lookup-gensym "swx~val" kdict))
	       (body `(let (swx~val) (,vsym) (,expr) ,body))
	       (body (with-escape (lookup "break" kdict) body)))
	  (values (cons body seed) (pop-scope kdict))))

       ((CaseBlock)
	(values (cons (rtail kseed) seed) kdict))
       
       ((CaseClauses)
	(values
	 (cons 
	  (let ((val (lookup "swx~val" kdict)))
	    (let loop ((next '(void)) (sym (jsym)) (ks kseed))
	      (if (eq? (car ks) 'CaseClauses)
		  `(let (~key) (,sym) ((const #f)) ,next)
		  (let ((psym (jsym)))
		    (loop (make-case val sym psym (car ks) next)
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

       ;; ====================================================================
       ;; @subheading Exceptions
       ;; @example
       ;; try { stmt ... } catch (var) { stmt ... }
       ;; try { stmt ... } finally { stmt ... }
       ;; try { stmt ... } catch (var) { stmt ... } finally { stmt ... }
       ;; @end example
       ;; @noindent
       ;; gets translated to 
       ;; @example
       ;; (let ((catch (make-prompt-handler 'catch)))
       ;;   (call-with-prompt catch
       ;;      (lambda () stmt ... (abort-to-prompt catch arg) ...)
       ;;      (lambda (k arg) stmt ...)))
       ;;   stmt ...) ;; finally ...
       ;; @end example
       ;; @no indent
       ;; If no catch is specified then, a throw skips to the finally.
       
       ;; ThrowStatement throw expression
       ((ThrowStatement)
	(values (cons `(abort ,(lookup "catch" kdict)
			      (,(car kseed)) (const '())) seed) kdict))
       
       ;; TryStatement, Catch, Finally
       ((TryStatement)
	(let* ((rseed (rtail kseed))
	       (try-stmts (car rseed))
	       (ctag (lookup-gensym "catch" kdict))
	       (catch (match (cdr rseed)
			((`(catch ,hdlr) . rest) hdlr)
			(otherwise (make-handler '() '(void)))))
	       (finally (match (cdr rseed)
			  (`((catch ,hdlr) (finally . ,stmts)) stmts)
			  (`((finally . ,stmts)) stmts)
			  (otherwise '(void))))
	       (body
		`(let (catch) (,ctag)
		      ((primcall make-prompt-tag (const catch)))
		      (seq
		       (prompt #t (lexical catch ,ctag) ,try-stmts ,catch)
		       ,finally))))
	  (values (cons body seed) (pop-scope kdict))))
       
       ((Catch)
	(let* ((arg-name (cadr (cadr tree)))	 ; arg name as string
	       (a-sym (string->symbol arg-name)) ; as symbol
	       (a-gsym (lookup-gensym arg-name kdict)) ; its gensym
	       (jcatch `(lambda ()
			  (lambda-case (((k ,a-sym) #f #f #f ()
					 (,(jsym) ,a-gsym)) ,(car kseed))))))
	  (values (acons 'catch jcatch seed) kdict)))

       ((Finally)
	(values (acons 'finally (car kseed) seed) kdict))

       ;; ====================================================================

       ;; FunctionDeclaration (see also fD above)
       ;; If the body starts with (bindings ...) then make a let.
       ((FunctionDeclaration)
	;; TODO: check for arguments-used?
	(let* ((name (let ((n (caddr kseed)))
		       (if (memq (car n) '(@ @@)) (caddr n) (cadr n))))
	       (this (caddr (lookup "this" kdict)))
	       (args (map cdr (cdadr kseed))) ;; '((x JS-123) (y JS-124))
	       (ptag (lookup "return" kdict))
	       (body (wrap-bindings (car kseed)))
	       (body (with-escape/arg ptag (block body)))
	       (fctn (make-function name this args body)))
	  (if (assq-ref kdict 'arguments-used?)
	      (throw 'javascript-error
		     "function `arguments' not supported yet"))
	  (values (cons `(define ,name ,fctn) seed) (pop-scope kdict))))

       ;; FunctionExpression
       ;; If the function has a name then wrap in a let so it can recurse.
       ((FunctionExpression)
	(let* ((lref (if (pair? (caddr kseed)) (caddr kseed) #f))
	       (name (if lref (cadr lref) #f))
	       (gsym (if lref (caddr lref) #f))
	       (this (caddr (lookup "this" kdict)))
	       (args (map cdr (cdadr kseed))) ;; '((x JS-123) (y JS-124))
	       (ptag (lookup "return" kdict))
 	       (body (wrap-bindings (car kseed)))
	       (body (with-escape/arg ptag (block body)))
	       (fctn (make-function name this args body))
	       (fctn (if lref `(let (,name) (,gsym) (,fctn) ,lref))))
	  (if (assq-ref kdict 'arguments-used?)
	      (throw 'javascript-error
		     "function `arguments' not supported yet"))
	  (values (cons fctn seed) (pop-scope kdict))))

       ;; FormalParameterList
       ((FormalParameterList)
	(values (cons (reverse kseed) seed) kdict))

       ;; FunctionElements
       ((FunctionElements)
	(values (cons (block (rtail kseed)) seed) kdict))

       ;; Program
       ((Program)
	(values (cons (car kseed) seed) kdict))
       
       ;; ProgramElements
       ((ProgramElements)
	(values (cons (block (rtail kseed)) seed) kdict))

       (else
	(cond
	 ((null? seed) (values (reverse kseed) kdict))
	 (else (values (cons (reverse kseed) seed) kdict)))))))

  (define (fH leaf seed dict)
    (values (cons leaf seed) dict))

  ;; We generate a dictionary with the env (module?) available at the top.
  (foldts*-values fD fU fH `(*TOP* ,exp) '() env))


(define show-sxml #f)
(define (show-javascript-sxml v) (set! show-sxml v))
(define show-xtil #f)
(define (show-javascript-xtil v) (set! show-xtil v))

(define (compile-tree-il exp env opts)
  (when show-sxml (sferr "sxml:\n") (pperr exp))
  (let ((cenv (if (module? env) (acons '@top #t (acons '@M env JSdict)) env)))
    (if exp 
	(call-with-values
	    (lambda () (xlang-sxml->xtil exp cenv opts))
	  (lambda (exp cenv)
	    (when show-xtil (sferr "tree-il:\n") (pperr exp))
	    (values (parse-tree-il exp) env cenv)
	    ;;(values (parse-tree-il '(const "[hello]")) env cenv)
     	    ))
	(values (parse-tree-il '(void)) env cenv))))

#|
(use-modules (nyacc lang javascript ia-parser))
(define-public (test-1)
  (let* ((prog "function f(x) { var a,b=1; a = 1; var c,d=4; return a+d; }")
	 (tree (with-input-from-string prog parse-js-elt)))
    (compile-tree-il tree (current-module) '())))
|#
;; --- last line ---
