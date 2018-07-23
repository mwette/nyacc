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

;; TODO:
;; @itemize
;; @item Imeplement @code{with}.
;; @item Imeplement @code{for}.
;; @item Imeplement @code{for-in}.
;; @item Update to es5.
;; @item Implement objects and prototypes correctly.
;; @item Implement unary and binary operators (in jslib-01.scm) correctly.
;; @end itemize
;;
;; DONE:
;; @itemize
;; @item add let only allow var at top-level and function start scope
;; @end itemize
;;
;; NOTES
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

(define-module (nyacc lang javascript compile-tree-il)
  #:export (compile-tree-il js-sxml->tree-il-ext)
  #:use-module (nyacc lang javascript jslib)
  #:use-module (nyacc lang sx-util)
  #:use-module ((sxml fold) #:select (foldts*-values))
  #:use-module ((srfi srfi-1) #:select (fold append-reverse))
  #:use-module (language tree-il)
  #:use-module (ice-9 match)
  )
(use-modules (ice-9 pretty-print))
(define pp pretty-print)

;; === portability ===================
;; no longer supports guile 2.0: assuming 2.2

;; @deffn {Procedure} block expr-or-expr-list => expr | (seq ex1 (seq ... exN))
;; Return an expression or build a seq-train returning last expression.
;; @end deffn
(define (block expr-or-expr-list)
  (if (pair? (car expr-or-expr-list))
      ;; expr list
      (let iter ((xl expr-or-expr-list))
	(if (null? (cdr xl)) (car xl)
	    `(seq ,(car xl) ,(iter (cdr xl)))))
      expr-or-expr-list))

;; @deffn {Procedure} vblock expr-list => (seq ex1 (seq ... (void)))
;; Return an expression or build a seq-train returning undefined.
;; @end deffn
(define (vblock expr-list)
  (let iter ((expl expr-list))
    (if (null? expl) '(void)
	(acons 'seq (car expl) (iter (cdr expl))))))

(define (jsym) (gensym "JS~"))
(define (genjsym name) (gensym (string-append name "-")))

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

(define jslib-mod '(nyacc lang javascript jslib))
(define (jslib-ref name) `(@@ (nyacc lang javascript jslib) ,name))

;;(define undefined (jslib-ref 'js:undefined))
(define undefined '(void))
(define null (jslib-ref 'js:null))

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
  (acons name `(lexical ,(string->symbol name) ,(genjsym name)) dict))

;; (add-lexicals name1 name2 ... dict) 
(define (add-lexicals . args)
  (let iter ((args args))
    (if (null? (cddr args)) (add-lexical (car args) (cadr args))
	(add-lexical (car args) (iter (cdr args))))))

;; Add lexical or toplevel based on level.
(define (add-symbol name dict)
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

(define jslib-module (resolve-module '(nyacc lang javascript jslib)))
		       
(define (lookup name dict)
  (cond
   ((not dict) #f)
   ((null? dict) #f)
   ((assoc-ref dict name))		; => value
   ((assoc-ref dict '@P) =>		; parent level
    (lambda (dict) (lookup name dict)))
   ((find-in-env name (assoc-ref dict '@M)))
   ((find-in-env (c-name->guile name) (assoc-ref dict '@M)))
   ((find-in-env name jslib-module))
   (else #f)))

;; @deffn {Procedure} lkup-gensym name dict [label] => gensym
;; lookup up nearest parent lexical and return gensym
;; (lkup-gensym "foo" dict) => JS~1234
;; (lkup-gensym "foo" dict #:label "oloop") => JS~432
;; @end deffn
(define* (lkup-gensym name dict #:key label)
  (if label
      (let iter ((cdict dict) (pdict (assoc-ref dict '@P)))
	(if (not pdict) #f
	    (if (and (assoc-ref pdict label)
		     (assoc-ref "~exit" cdict))
		(assoc-ref name cdict)
		(iter pdict (assoc-ref pdict '@P)))))
      (let* ((sym (lookup name dict)))
	(if (not sym) (error "javascript: not found:" name))
	(caddr sym))))

;; === using prompts ====================

;; @deffn {Procedure} make-handler args body
;; Generate an escape @code{lambda} for a prompt.  The continuation arg
;; is not used.  @var{args} is a list of lexical references and @var{body}
;; is an expression that may reference the args.
;; @end deffn
(define (make-handler args body)
  (call-with-values
      (lambda ()
	(let iter ((names '()) (gsyms '()) (args args))
	  (if (null? args)
	      (values (reverse names) (reverse gsyms))
	      (iter (cons (cadar args) names)
		    (cons (caddar args) gsyms)
		    (cdr args)))))
    (lambda (names gsyms)
      `(lambda ()
	 (lambda-case ((,(cons 'k names) #f #f #f () ,(cons (genjsym "k") gsyms))
		       ,body))))))
	 
;; @deffn {Procedure} with-escape tag-ref body
;; @deffx {Procedure} with-escape/arg tag-ref body
;; use for return and break where break is passed '(void)
;; tag-ref is of the form (lexical name gensym)
;; @end deffn
(define (with-escape/handler tag-ref body hdlr)
  (let ((tag-name (cadr tag-ref))
	(tag-gsym (caddr tag-ref)))
    `(let (,tag-name) (,tag-gsym) ((primcall make-prompt-tag (const ,tag-name)))
	  (prompt #t ,tag-ref ,body ,hdlr))))
  
(define (with-escape/arg tag-ref body)
  (let ((arg-gsym (genjsym "arg")))
    (with-escape/handler
     tag-ref body
     `(lambda ()
	(lambda-case (((k arg) #f #f #f () (,(genjsym "k") ,arg-gsym))
		      (lexical arg ,arg-gsym)))))))

(define (with-escape tag-ref body)
  (let ((arg-gsym (genjsym "arg")))
    (with-escape/handler
     tag-ref body
     `(lambda ()
	(lambda-case (((k) #f #f #f () (,(genjsym "k")))
		      (void)))))))

(define (xx-with-escape/arg tag-ref body)
  (let ((tag-name (cadr tag-ref))
	(tag-gsym (caddr tag-ref))
	(arg-gsym (genjsym "arg")))
    `(let (,tag-name) (,tag-gsym) ((primcall make-prompt-tag (const ,tag-name)))
	  (prompt #t ,tag-ref
		  ,body
		  (lambda ()
		    (lambda-case
		     (((k arg) #f #f #f () (,(genjsym "k") ,arg-gsym))
		      (lexical arg ,arg-gsym))))))))
(define (xx-with-escape tag-ref body)
  (let ((tag-name (cadr tag-ref))
	(tag-gsym (caddr tag-ref)))
    `(let (,tag-name) (,tag-gsym) ((primcall make-prompt-tag (const ,tag-name)))
	  (prompt #t ,tag-ref
		  ,body
		  (lambda ()
		    (lambda-case
		     (((k) #f #f #f () (,(genjsym "k")))
		      (void))))))))

;; === codegen procedures =============

(define (rtail kseed)
  (cdr (reverse kseed)))

(define (singleton? expr)
  (and (pair? expr) (null? (cdr expr))))

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
;; @noindent
;; and @var{exprs} is a list of expressions, to something like
;; @example
;; (let (v w) (JS~5897 JS~5898) (#<unspecified> (const 3)) . exprs)
;; @end example
;; @end deffn
(define (make-let bindings exprs)
  (let iter ((names '()) (gsyms '()) (vals '()) (bindings bindings))
    (if (null? bindings)
	`(let ,(reverse names) ,(reverse gsyms) ,(reverse vals) ,(block exprs))
	(iter (cons (list-ref (car bindings) 1) names)
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
;; (bindings (bind v v~5897 (void))(bind w w~5898 (const 3)))
;; @end example
;; @noindent
;; Oh, I think this needs to use @code{letrec*}.
(define (wrap-bindings body)
  ;;(sferr "wrap:\n  body\n") (pperr body)
  (let iter1 ((bindings '()) (rest body))
    (match rest
      (`(seq (bindings . ,bnds) ,rst)
       (iter1 (fold cons bindings bnds) rst))
      (_
       ;;(sferr "  bindings, rest:\n") (pperr bindings) (pperr rest)
       (if (null? bindings)
	   body
	   (let iter2 ((names '()) (gsyms '()) (inits '()) (binds bindings))
	     (if (null? binds)
		 `(letrec* ,names ,gsyms ,inits ,rest)
		 (iter2 (cons (list-ref (car binds) 1) names)
			(cons (list-ref (car binds) 2) gsyms)
			(cons (list-ref (car binds) 3) inits)
			(cdr binds)))))))))

;; @deffn {Procedure} make-thunk expr => `(lambda ...)
;; Generate a thunk.
;; @end deffn
(define* (make-thunk expr #:key name)
  `(lambda ,(if name '((namhe)) '()) (lambda-case ((() #f #f #f () ()) ,expr))))

;; @deffn {Procedure} make-do-while expr body bsym csym
;; @deffnx {Procedure} make-while expr body bsym csym
;; This generates code for the following source:
;; @example
;; "do" body "where" expr
;; "while" body "do" expr
;; @end example
;; @noindent
;; where @arg{expr} is the condtional expression, @arg{body} is the body,
;; @arg{bsym} is the gensym for @code{break}, @arg{csym} is the gensym for
;; @code{continue}.  The code generated is based on the following pattern:
;; @example
;; (let ((break! (make-prompt-tag 'break))
;;       (continue! (make-prompt-tag 'continue)))
;;    (letrec ((iloop (lambda () (body) (if (expr) (iloop))))
;;             (oloop
;;              (lambda ()
;;               (call-with-prompt continue!
;;                  thunk
;;                  (lambda (k) (if (expr) (oloop)))))))
;;      (call-with-prompt break!
;;        oloop
;;        (lambda (k) (if #f #f))))))
;; @end example
;; @noindent
;; where @code{break!} and @code{continue!} are lexicals generated for
;; the code and @code{thunk} is @*
;; @code{(lambda () (iloop))} for do-while and @*
;; @code{(lambda () (if (expr) (iloop)))} for while-do.
(define (make-loop expr body dict ilsym tbody)
  (let* ((olsym (genjsym "oloop"))
	 (bsym (lkup-gensym "break" dict))
	 (csym (lkup-gensym "continue" dict))
	 (icall `(call (lexical iloop ,ilsym)))
	 (ocall `(call (lexical oloop ,olsym)))
	 (iloop (make-thunk `(seq ,body (if ,expr ,icall (void)))))
  	 (ohdlr `(lambda ()
		   (lambda-case (((k) #f #f #f () (,(genjsym "k")))
				 (if ,expr ,ocall (void))))))
	 (oloop (make-thunk `(prompt #t (lexical continue ,csym) ,tbody ,ohdlr)))
 	 (hdlr `(lambda ()
		  (lambda-case (((k) #f #f #f () (,(genjsym "k"))) (void))))))
    `(let (break continue) (,bsym ,csym)
	  ((primcall make-prompt-tag (const break))
	   (primcall make-prompt-tag (const continue)))
	  (letrec (iloop oloop) (,ilsym ,olsym) (,iloop ,oloop)
		  (prompt #t (lexical break ,bsym) ,ocall ,hdlr)))))

(define (make-do-while expr body dict)
  (let ((ilsym (genjsym "iloop")))
    (make-loop expr body dict ilsym `(call (lexical iloop ,ilsym)))))

(define (make-while expr body dict)
  (let ((ilsym (genjsym "iloop")))
    (make-loop expr body dict ilsym
		    `(if ,expr (call (lexical iloop ,ilsym)) (void)))))

;; Pass arguments as optional and add @code{this} keyword argument.
(define (make-function name this args body)
  (if (not args) (error "no args"))
  `(lambda ,(if name `((name . ,name)) '())
     (lambda-case ((()			; req
		    ,(map car args)	; opt
		    #f			; rest
		    (#f (#:this this ,this)) ; kw
		    (,@(map (lambda (v) undefined) args) ,undefined) ; inits
		    (,@(map cadr args) ,this)) ; syms
		   ,body))))

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

  ;; @deffn {Procedure} remove-empties elements => elements
  ;; @end deffn
  (define (remove-empties elements)
    (let iter ((elts elements))
      (if (null? elts) '()
	  (let ((elt (car elts)) (rest (cdr elts)))
	    (if (eq? (car elt) 'EmptyStatement)
		(iter rest)
		(cons elt (iter rest)))))))

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

  ;; @deffn {Procedure} hoist-decls elements => elements
  ;; Move all variable declarations to front and replace old ones with
  ;; assignment.
  ;; @end deffn
  (define hoist-stmts '(VariableStatement LetStatement))
  (define (split-dstmt dstmt hoisted rest) ;; => hoisted rest
    ;; move Let/VariableStatement to hoisted, moving initializers
    ;; to decls as assignment statements
    (let iter ((hdecls '()) (rest rest) (decls (cdadr dstmt)))
      (if (null? decls)
	  (values (cons `(,(car dstmt) (DeclarationList ,hdecls)) hoisted) rest)
	  (let* ((decl (car decls))
		 (init (if (= 3 (length decl)) (caddr decl) #f))
		 (decl (if init (list (car decl) (cadr decl)) decl)))
	    (iter (cons decl hdecls)
		  (if init
		      (cons `(ExpressionStatement
			      (AssignmentExpression
			       (PrimaryExpression ,(cadr decl))
			       (assign "=") ,(cadr init)))
			    rest)
		      rest)
		  (cdr decls))))))
  (define (hoist-decls elements)
    (let ((tail (let iter ((elts elements)) ; tail after Let/VarStatements
		  (cond
		   ((null? elts) elts)
		   ((not (memq (caar elts) hoist-stmts)) elts)
		   (iter (cdr elts))))))
      (let iter ((hoisted '()) (rest '()) (elts tail))
	(cond
	 ((null? elts)
	  (if (null? hoisted)
	      elements			   ; nothing hoisted
	      (let iter2 ((elts elements)) ; rebuild statement list
		(if (eq? elts tail) (append-reverse hoisted (reverse rest))
		    (cons (car elts) (iter2 (cdr elts)))))))
	 ((memq (caar elts) hoist-stmts)
	  (call-with-values		; decl=>hoisted init=>rest
	      (lambda () (split-dstmt (car elts) hoisted rest))
	    (lambda (hoisted rest)
	      (iter hoisted rest (cdr elts)))))
	 (else
	  (iter hoisted (cons (car elts) rest) (cdr elts)))))))
	  
  
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
    (let iter ((src  src-elts-tail))
      (if (null? src) '()
	  (let ((elt (car src)) (rest (cdr src)))
	    (if (eq? (car elt) 'VariableStatement)
		(list (cons* 'Block (cadr elt) (iter rest)))
		(cons elt (iter rest)))))))

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

      ((PropertyNameAndValue (Identifier ,name) ,expr)
       (values `(PropertyNameAndValue (PropertyName ,name) ,expr) '() dict))

      ((obj-ref ,expr (Identifier ,name))
       (values `(ooa-ref ,expr (PropertyName ,name)) '() dict))

      ((Block . ,elts) ;; see comments on FunctionElements below
       (let* ((elts (remove-empties elts))
	      (elts (cleanup-labels elts)))
	 (values tree '() dict)))
      
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
       (values tree '() (fold add-lexical dict (map cadr idlist))))
      
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

      (else
       ;;(sferr "fD: otherwise\n") (pperr tree)
       (values tree '() dict))
      ))

  (define (opcall-node op seed kseed kdict)
    (values (cons (rev/repl 'call (jslib-ref op) kseed) seed) kdict))
  
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
     (null? tree) (values (cons kseed seed) dict)
     
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

       ;; ======================================================================
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
	(if (> (length kseed) 1) (throw 'js-error "unsupported: break <label>"))
	(values
	 (cons `(abort ,(lookup "continue" kdict) () (const ())) seed)
	 kdict))

       ;; BreakStatement: abort w/ zero args
       ((BreakStatement)
	(if (> (length kseed) 1) (throw 'js-error "unsupported: break <label>"))
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
	       (vsym (lkup-gensym "swx~val" kdict))
	       (body `(let (swx~val) (,vsym) (,expr) ,body))
	       (body (with-escape (lookup "break" kdict) body)))
	  (values (cons body seed) (pop-scope kdict))))

       ((CaseBlock)
	(values (cons (rtail kseed) seed) kdict))
       
       ((CaseClauses)
	(values
	 (cons 
	  (let ((val (lookup "swx~val" kdict)))
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
	       (ctag (lkup-gensym "catch" kdict))
	       (catch (match (cdr rseed)
			((`(catch ,hdlr) . rest) hdlr)
			(otherwise (make-handler '() '(void)))))
	       (finally (match (cdr rseed)
			  (`((catch ,hdlr) (finally . ,stmts)) stmts)
			  (`((finally . ,stmts)) stmts)
			  (otherwise '(void))))
	       (body
		`(let (catch) (,ctag) ((primcall make-prompt-tag (const catch)))
		      (seq
		       (prompt #t (lexical catch ,ctag) ,try-stmts ,catch)
		       ,finally))))
	  (values (cons body seed) (pop-scope kdict))))
       
       ((Catch)
	(let* ((arg-name (cadr (cadr tree)))	 ; arg name as string
	       (a-sym (string->symbol arg-name)) ; as symbol
	       (a-gsym (lkup-gensym arg-name kdict)) ; its gensym
	       (catch `(lambda ()
			 (lambda-case (((k ,a-sym) #f #f #f () (,(jsym) ,a-gsym))
				       ,(car kseed))))))
	  (values (acons 'catch catch seed) kdict)))

       ((Finally)
	(values (acons 'finally (car kseed) seed) kdict))

       ;; ======================================================================

       ;; FunctionDeclaration (see also fD above)
       ;; If the body starts with (bindings ...) then make a let.
       ((FunctionDeclaration)
	(let* ((name (let ((n (caddr kseed)))
		       (if (memq (car n) '(@ @@)) (caddr n) (cadr n))))
	       (this (caddr (lookup "this" kdict)))
	       (args (map cdr (cdadr kseed))) ;; '((x JS-123) (y JS-124))
	       (ptag (lookup "return" kdict))
	       (body (wrap-bindings (car kseed)))
	       (body (with-escape/arg ptag (block body)))
	       (fctn (make-function name this args body)))
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
	  (values (cons fctn seed) (pop-scope kdict))))

       ;; FormalParameterList
       ((FormalParameterList)
	(values (cons (reverse kseed) seed) kdict))

       ;; FunctionElements
       ((FunctionElements)
	(values (cons (block (rtail kseed)) seed) kdict))

       ;; Program
       ((Program)
	(values (car kseed) kdict))
       
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
  #;(let ((dict (acons '@top #t (acons '@M env JSdict)))
	(sexp `(*TOP* ,exp)))
    (call-with-values
	(lambda () (foldts*-values fD fU fH sexp '() dict))
      (lambda (seed dict) seed)))
  (foldts*-values fD fU fH `(*TOP* ,exp) '() env)
  )


(use-modules (language tree-il compile-cps))

;; @deffn {Procedure} compile-tree-il exp env opts => exp env cenv
;; On input @var{exp} is the SXML from our reader, @var{env} is ``an
;; environment'', and @var{opts} is a keyword list of options.  The procedure
;; return three values: the compiled expressin, the corresponding environment
;; for the target for the compiled language, and a continuation environment
;; for the next javascript tree.
;; @end deffn
(define (compile-tree-il exp env opts)
  ;;(sferr "env=module? ~S\n" (module? env))
  ;;(sferr "\nenv=~S\n" env)
  ;;(sferr "sxml:\n") (pp exp)
  (let ((cenv (if (module? env) (acons '@top #t (acons '@M env JSdict)) env)))
    (sferr "env=~S\n" env)
    (sferr "cenv=~S\n" cenv)
    (if exp 
	(call-with-values
	    (lambda () (js-sxml->tree-il/ext exp cenv opts))
	  (lambda (exp cenv)
	    ;;(sferr "tree-il:\n") (pperr exp)
	    (values (parse-tree-il exp) env cenv)
	    ;;(values (parse-tree-il '(const "[compile-tree-il skip]")) env cenv)
     	    ))
	(values (parse-tree-il '(void)) env cenv))))

#|
(use-modules (nyacc lang javascript iaparser))
(define-public (test-1)
  (let* ((prog "function f(x) { var a,b=1; a = 1; var c,d=4; return a+d; }")
	 (tree (with-input-from-string prog parse-js-elt)))
    (compile-tree-il tree (current-module) '())))
|#
;; --- last line ---
