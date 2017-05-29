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

;; guile 2.0 or 2.2
(define il-call 'apply)
(define (make-call proc . args) `(apply ,proc ,@args))
;;(define il-call 'call)
;;(define (make-call proc . args) `(call ,proc ,@args))


(use-modules (ice-9 pretty-print))

(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))

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

(define (add-lexical name dict)
  (acons name `(lexical ,(string->symbol name) ,(gensym "JS~")) dict))

;; Add toplevel to dict.
(define (add-toplevel name dict)
  (acons name `(toplevel ,(string->symbol name)) dict))

;; Add lexcial or toplevel based on level.
(define (add-symboldef name dict)
  ;;(sferr "add-symboldef at @l=~S\n" (assq-ref dict '@l))
  (if (positive? (assq-ref dict '@l))
      (add-lexical name dict)
      (add-toplevel name dict)))

(define (lookup name dict)
  ;;(sferr "lookup ~S ~S\n" name dict)
  (cond
   ((not dict) #f)
   ((null? dict) #f)
   ((assoc-ref dict name))		; => value
   ((assoc-ref dict '@M) =>		; only at top level
    (lambda (env)
      (let* ((sym (string->symbol name))
	     (var (module-variable env sym)))
	(if (not var) (error "not found:" sym))
	;;`(toplevel ,sym))))
	`(@@ ,(module-name env) ,sym))))
   (else (lookup name (assoc-ref dict '@P)))))

;; ice-9 r5rs

;; body needs a line to build "var arguments" from Array(@args)
;; Right now args is the gensym of the rest argument named @code{@@args}.
(define (make-function name args body)
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
			  (lexical val ,valsym))))))))))

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

  (define (fD tree seed dict) ;; => tree seed dict
    ;; This handles branches as we go down the tree.  We do two things here:
    ;; @enumerate
    ;; @item Pick off low hanging fruit: items we can completely convert
    ;; @item trap places where variables are declared and maybe bump scope
    ;; Add symbols to the dictionary, keeping track of lexical scope.
    ;; @end enumerate
    
    ;;(sferr "fD: tree=~S ...\n" (car tree))
    ;;(display "fD\n" cep) (pretty-print tree cep)
    (sxml-match tree
      ((NullLiteral)
       (values '() 'JS:null dict))
      
      ((Identifier ,name)
       ;;(sferr "fD: ret null\n")
       (let ((ref (lookup name dict)))
	 (if (not ref) (error "lookup 2 failed"))
	 (values '() ref dict)))
      
      ((PrimaryExpression (this))
       (error "not implemented: PrimaryExpression (this)"))
	      
      ((PrimaryExpression (Identifier ,name))
       ;;(sferr "id dict\n") (pretty-print dict (current-error-port))
       (let ((ident (lookup name dict)))
	 (if (not ident) (error "JS: identifier not found:" name))
	 (values '() ident dict)))

      ((PrimaryExpression (NullLiteral ,null))
       (values '() '(const null) dict))

      ((BooleanLiteral ,true-or-false)
       (values '() `(const ,(char=? (string-ref true-or-false 0) #\t)) dict))

      ((PrimaryExpression (NumericLiteral ,val))
       (values '() `(const ,(string->number val)) dict))

      ((PrimaryExpression (StringLiteral ,str))
       (values '() `(const ,str) dict))

      ((obj-ref ,object ,ident)
       ;; Convert the tree: obj.ref ==> obj["ref"]
       (values
	`(ary-ref ,object (PrimaryExpression (StringLiteral ,(cadr ident))))
	'() dict))

      ;; declarations: we need to trap ident references and replace them

      ;; if toplevel we generate (toplevel "name")
      ;; if lexical we generate (lexical "name" "gensym")
      ((VariableDeclaration (Identifier ,name) . ,rest)
       (let* ((dict1 (add-symboldef name dict))
	      (tree1 (lookup name dict1)))
	 (if (not tree1) (error "lookup failed"))
	 (values `(VariableDeclaration ,tree1 . ,rest) '() dict1)))

      ((FunctionDeclaration (Identifier ,name) ,rest ...)
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
      
      ((SourceElements ,elts ...) ;; like 'begin so no new scope
       (values tree '() dict))

      (,otherwise
       ;;(display "fD otherwise\n" cep) (pretty-print tree cep)
       ;;(sferr "fD: otherwise\n")
       ;;(pretty-print tree (current-error-port))
       (values tree '() dict))
      ))

  (define (fU tree seed dict kseed kdict) ;; => seed dict
    ;;(sferr "fU: kseed=~S\n    seed=~S\n" kseed seed)
    ;;(pretty-print tree cep)
    ;; This routine rolls up processes leaves into the current branch.
    (if
     (null? tree) (values (cons kseed seed) dict)
     (case (car tree)

       ((*TOP*)
	;;(sferr "TOP: kseed=~S seed=~S\n" kseed seed)
	;;(pretty-print tree (current-error-port))
	;;(values (car kseed) dict))
	(values kseed dict))

       ;; PrimaryExpression (w/ ArrayLiteral or ObjectLiteral only)
       ((PrimaryExpression)
	(values (cons (car kseed) seed) dict))
      
       ;; ArrayLiteral
       ;; ElementList
       ;; Elision
       ;; ObjectLiteral
       ;; PropertyNameAndValueList 

       ;; ary-ref
       ((ary-ref)
	(values (cons (make-call `(@@ ,jslib-mod lkup) (cadr kseed) (car kseed))
		      seed) dict))

       ;; obj-ref
       ((obj-ref) ;; ???
	(values (cons (make-call `(@@ ,jslib-mod lkup) (cadr kseed) (car kseed))
		      seed) dict))

       ;; new

       ;; CallExpression
       ((CallExpression)
	(values (cons (rev/repl il-call kseed) seed) dict))

       ;;fU: ArgumentList
       ((ArgumentList) ;; append-reverse-car ??? 
	(values (append (cdr (reverse kseed)) seed) dict))

       ;; delete
       ;; void
       ;; typeof
       ;; pre-inc
       ;; pre-dec
       ;; pos
       ;; neg
       ;; ~
       ;; not
       ;; mul
       ;; div
       ;; mod
       
       ;; add
       ((add)
	;;(sferr "fU: kseed=~S\n    seed=~S\n" kseed seed)
	;;(pretty-print tree cep)
	;;(values (cons `(@@ ,jslib-mod JS:+) seed) dict))
	(values (cons (rev/repl il-call
				`(@@ ,jslib-mod JS:+)
				kseed) seed) dict))

       ;; sub
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
       ;;fU: bit-and
       ;;fU: bit-xor
       ;;fU: bit-or
       ;;fU: and
       ;;fU: or
       ;;fU: ConditionalExpression

       ;;fU: AssignmentExpression
       ((AssignmentExpression)
	(values (cons (apply x-assn kseed) seed) dict))

       ;;fU: assign
       ;;fU: mul-assign
       ;;fU: div-assign
       ;;fU: mod-assign
       ;;fU: add-assign
       ;;fU: sub-assign
       ;;fU: lshift-assign
       ;;fU: rshift-assign
       ;;fU: rrshift-assign
       ;;fU: and-assign
       ;;fU: xor-assign
       ;;fU: or-assign
       ;;fU: expr-list
       ;;fU: Block
       ;;fU: StatementList

       ;;fU: VariableStatement
       ((VariableStatement)
	;;(sferr "fU: kseed=~S\n    seed=~S\n" kseed seed)
	;;(pretty-print tree cep)
	(values (cons (car kseed) seed) kdict))

       ;;fU: VariableDeclarationList
       ((VariableDeclarationList)
	(let ((expr (rev/repl 'begin kseed)))
	  ;;(simple-format #t "VDL: ~S\n" expr)
	  (values (cons expr seed) kdict)))
       
       ;;fU: VariableDeclaration
       ((VariableDeclaration)
	;; NEEDS TO BE let OR (define (toplevel NAME) VALUE)
	;;(sferr "  VarDecl: seed=~S kseed=~S\n\n" seed kseed)
	(let ((expr (if (= 3 (length kseed))
			`(define ,(cadr (list-ref kseed 1)) ,(list-ref kseed 0))
			`(define ,(cadr (list-ref kseed 0)) (void)))))
	  ;;(simple-format #t "VD: ~S\n" kseed)
	  (values (cons expr seed) kdict)))
       
       ;;fU: Initializer
       ((Initializer)		       ; just grab the single argument
	(values (cons (car kseed) seed) dict))

       ;; EmptyStatement
       ((EmptyStatement)		; ignore
	(values seed dict))

       ;; ExpressionStatement
       ((ExpressionStatement)	       ; just grab the single argument
	(values (cons (car kseed) seed) dict))

       ;;fU: IfStatement
       ;;fU: do
       ;;fU: while
       ;;fU: for
       ;;fU: Expression
       ;;fU: ExprStmt
       ;; ContinueStatement

       ;;fU: ReturnStatement
       ((ReturnStatement) ;; will need a prompt for return, until optimized?
	;;(sferr "fU: kseed=~S\n    seed=~S\n" kseed seed)
	;;(pretty-print tree cep)
	(values (cons `(abort (const return)
			      ,(cdr (reverse kseed))
			      (const ()))
		      seed) dict))

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

       ;; FunctionDeclaration
       ((FunctionDeclaration)
	;;(sferr "fU: kseed=~S\n    seed=~S\n" kseed seed)
	;;(pretty-print tree cep)
	(values
	 (let ((name (cadr kseed))
	       (args (list-ref (lookup "@args" kdict) 2))
	       (body (car kseed)))
	   (if (not args) (error "args lookup error"))
	   (cons (make-function name args body) seed))
	 kdict))

       ;; FunctionExpression

       ;; FormalParameterList
       ((FormalParameterList)
	;; We build the function with the rest argument @code{@@args}.
	(values seed kdict))

       ;; Program
       ((Program)
	(values (car kseed) dict))
       
       ;; SourceElements
       ((SourceElements)
	(values (cons (rev/repl 'begin kseed) seed) dict))

       (else
	;;(sferr "fU: kseed=~S  [else]\n    seed=~S\n" kseed seed)
	;;(pretty-print tree cep)
	(cond
	 ((null? seed) (values (reverse kseed) dict))
	 ;;((null? kseed) (values (cons (car tree) seed) dict)) ;; ???
	 (else (values (cons (reverse kseed) seed) dict)))))))

  (define (fH leaf seed dict)
    (values (cons leaf seed) dict))

  ;; We generate a dictionary with the env (module?) available at the top.
  (let ((dict (acons `@M env JSdict))
	(sexp `(*TOP* ,exp)))
    (foldts*-values fD fU fH sexp '() dict)))

;; @deffn {Procedure} compile-tree-il exp env opts => 
(define (compile-tree-il exp env opts)
  (when #f
    (display "exp:\n" (current-error-port))
    (pretty-print exp (current-error-port)))
  (let* ((xrep (js-sxml->tree-il-ext exp env opts))
	 (code (parse-tree-il '(const 1)))
	 (code (parse-tree-il xrep))
	 )
    ;;(pretty-print xrep (current-error-port))
    (values code env env)))

;; --- last line ---
