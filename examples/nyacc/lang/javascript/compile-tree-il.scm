;;; javascript specification for Guile
;;;
;;; Copyright (C) 2015 Matthew R. Wette
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

(define-module (nyacc lang javascript compile-tree-il)
  #:export (compile-tree-il js-sxml->tree-il-ext)
  #:use-module (nyacc lang javascript jslib)
  ;;#:use-module (system base language)
  ;;#:use-module (nyacc lang javascript separser)
  #:use-module ((sxml match) #:select (sxml-match))
  #:use-module ((sxml fold) #:select (foldts*-values))
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (language tree-il)
  )

(use-modules (ice-9 pretty-print))

(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))

(define jslib-mod '(nyacc lang javascript jslib))

(define (x-assn rhs op lhs junk)
  (case (car op)
    ((assign) `(set! ,lhs ,rhs))
    ((add-assign) `(set! ,lhs (apply (@@ ,jslib-mod JS+) ,lhs ,rhs)))
    (else
     (sferr "\nUNKNOWN OP: ~S\n\n" op)
     '(unknown))))

;; @heading variable scope
;; Variables in the compiler are kept in a scope-stack with the highest
;; level being the current module.  Why do I convert to xxx?

(define (add-lexical name dict)
  (acons name `(lexical ,(string->symbol name) ,(gensym "JS~")) dict))

;; Add toplevel to dict.
(define (add-toplevel name dict)
  (acons name `(toplevel ,(string->symbol name)) dict)
  #;(let ((mod (let iter ((dict dict))
	       (or (assq-ref dict '@M) (iter (assq-ref dict '@P))))))
          (acons name `(@ ,mod ,(string->symbol name)) dict)))

;; Add lexcial or toplevel based on level.
(define (add-reference name dict)
  (if (> (assq-ref dict '@l) 1)
      (add-lexical name dict)
      (add-toplevel name dict)))
;; push/pop scope level
(define (push-level dict)
  (list (cons '@l (1+ (assq-ref dict '@l))) (cons '@P dict)))
(define (pop-level dict)
  (or (assq-ref dict '@P) (error "coding error: too many pops")))

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
	;;`(@ ,env ,sym)))) ;; how to turn env into module ref
	`(toplevel ,sym))))
   (else (lookup name (assoc-ref dict '@P)))))

;; ice-9 r5rs

;; body needs a line to build "var arguments" from Array(@args)
;; Right now args is the gensym of the rest argument named @code{@@args}.
(define (make-function name args body)
  (let ((tagsym (gensym "JS~")) (valsym (gensym "JS~")))
    `(define ,(cadr name)
       (lambda ((name . ,(cadr name)))
	 (lambda-case ((() #f @args #f () (,args))
		       (prompt
			(const return)	; tag
			,body		; body
			(lambda-case	; handler
			 (((tag val) #f #f #f () (,tagsym ,valsym))
			  (lexical val ,valsym))))))))))

;; @deffn {Procedure} js-xml->tree-il-ext exp env opts
;; Compile javascript SXML tree to external tree-il representation.
;; This one is public because it's needed for debugging the compiler.
;; @end deffn
;;(define (js-sxml->tree-il-ext exp env opts)

  (define cep (current-error-port))
  
  ;; In the case where we pick off ``low hanging fruit'' we need to coordinate
  ;; the actions of the up and down handlers.   The down handler will provide
  ;; a kid-seed in order and generate a null list.  The up handler, upon seeing
  ;; a null list, will just incorporate the kids w/o the normal reverse.

  (define (fD tree seed dict) ;; => tree seed dict
    ;; This handles branches as we go down the tree.  We do two things here:
    ;; @enumerate
    ;; @item Pick off low hanging fruit: items we can completely convert
    ;; @item Add symbols to the dictionary, keeping track of lexical scope.
    ;; @end enumerate
    
    ;;(sferr "fD: tree=~S ...\n" (car tree))
    ;;(display "fD\n" cep) (pretty-print tree cep)
    (sxml-match tree
      ((NullLiteral)
       (values '() `JS-null dict))
      
      ((Identifier ,name)
       ;;(sferr "fD: ret null\n")
       (values '() (lookup name dict) dict))
      
      ((PrimaryExpression (Identifier ,name))
       ;;(sferr "fD: ret null\n")
       (let ((ident (lookup name dict)))
	 (if (not ident) (error "JS: identifier not found:" name)) 
	 (values '() ident dict)))

      ((PrimaryExpression (StringLiteral ,str))
       ;;(sferr "fD: ret null\n")
       (values '() `(const ,str) dict))

      ((PrimaryExpression (NumericLiteral ,val))
       ;;(sferr "fD: ret null\n")
       (values '() `(const ,(string->number val)) dict))

      ((obj-ref ,object ,ident)
       ;; Convert the tree: obj.ref ==> obj["ref"]
       (values
	`(ary-ref ,object (PrimaryExpression (StringLiteral ,(cadr ident))))
	'() dict))

      ((VariableDeclaration (Identifier ,name) ,rest ...)
       (values tree '() (add-reference name dict)))

      ((FunctionDeclaration (Identifier ,name) ,rest ...)
       (values tree '() (push-level (add-reference name dict))))
      
      ((FormalParameterList ,idlist ...)
       ;; For all functions we just use rest arg and then express each
       ;; var reference as (list-ref @args index)
       ;; Another option is to use case-lambda ...
       (let* ((args (add-lexical "@args" dict))
	      (gsym (list-ref (car args) 3)) ; need gensym ref
	      (dikt (fold
		     (lambda (name indx seed)
		       (acons name `(apply (toplevel list-ref)
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
      
      ((SourceElements ,elts ...)
       (values tree '() (push-level dict)))

      (,otherwise
       ;;(display "fD otherwise\n" cep) (pretty-print tree cep)
       ;;(sferr "fD: otherwise\n")
       ;;(pretty-print tree (current-error-port))
       (values tree '() dict))
      ))

  (define (fU tree seed dict kseed kdict) ;; => seed dict
    (sferr "fU: kseed=~S\n    seed=~S\n" kseed seed)
    (pretty-print tree cep)
    ;; This routine rolls up processes leaves into the current branch.
    (if
     (null? tree) (values (cons kseed seed) dict)
     (case (car tree)

       ((*TOP*)
	;;(sferr "TOP: kseed=~S seed=~S\n" kseed seed)
	;;(pretty-print tree (current-error-port))
	(values (car kseed) dict))
       
       ((CallExpression)
	(values (cons `(apply ,@(reverse kseed)) seed) dict))

       ((ArgumentList)
	(values (append kseed seed) dict))

       ((ary-ref)
	(values (cons `(apply (@@ ,jslib-mod lkup) ,(cadr kseed)
			      ,(car kseed)) seed) dict))

       ((obj-ref) ;; ???
	(values (cons `(apply (@@ ,jslib-mod lkup) ,(cadr kseed)
			      ,(car kseed)) seed) dict))

       ((add)
	(values (cons `(@@ ,jslib-mod JS:+) seed) dict))
       
       ((AssignmentExpression)
	(values (cons (apply x-assn kseed) seed) dict))

       ((FormalParameterList)
	;; We build the function with the rest argument @code{@@args}.
	(values seed kdict))

       ((VariableStatement)
	(values (append (car kseed) seed) kdict))

       ((VariableDeclarationList)
	;;(values (append (cdr (reverse kseed)) seed) kdict))
	(values (cons `(begin ,(cdr (reverse kseed))) seed) kdict))

       ((VariableDeclaration)
	;;(sferr "  VarDecl: seed=~S kseed=~S\n\n" seed kseed)
	(values
	 (cons
	  (if (= 3 (length kseed))
	      `(define ,(cadr (list-ref kseed 1)) ,(list-ref kseed 0))
	      `(define ,(cadr (list-ref kseed 0)) ,(if #f #f)))
	  seed)
	 kdict))

       ((Initializer)
	(values (cons (car kseed) seed) dict))

       ((ExpressionStatement)
	(values (cons (car kseed) seed) dict))

       ((EmptyStatement)
	(values seed dict))

       ((ReturnStatement) ;; will need a prompt for return, until optimized?
	(values (cons `(abort (const return) (,kseed) (const ())) seed)
		dict))

       ((FunctionDeclaration)
	(values
	 (let ((name (cadr kseed))
	       (args (list-ref (lookup "@args" kdict) 2))
	       (body (car kseed)))
	   (cons (make-function name args body) seed))
	 kdict))

       ((SourceElements)
	(values (cons `(begin ,@(reverse kseed)) seed) dict))

       ((Program)
	(values (car kseed) dict))
       
       (else
	(cond
	 ((null? seed) (values (reverse kseed) dict))
	 ;;((null? kseed) (values (cons (car tree) seed) dict)) ;; ???
	 (else (values (cons (reverse kseed) seed) dict)))))))

  (define (fH leaf seed dict)
    (values (cons leaf seed) dict))

(define (js-sxml->tree-il-ext exp env opts)
  ;; We generate a dictionary with the env (module?) available at the top.
  (let ((dict (acons `@M env JSdict))
	(sexp `(*TOP* ,exp)))
    (foldts*-values fD fU fH sexp '() dict)))

(define (compile-tree-il exp env opts)
  ;;(sferr "exp=~S\n" exp)
  ;;(display "exp:\n" (current-error-port))
  ;;(pretty-print exp (current-error-port))
  (let* ((xrep (js-sxml->tree-il-ext exp env opts))
	 (code (parse-tree-il xrep)))
    (values code env env)))

;; --- last line ---
