;; jsdev.scm -- javascript dev
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;; or any later version published by the Free Software Foundation.  See the
;; file COPYING included with the this distribution.

(add-to-load-path (getcwd))
(add-to-load-path (string-append (getcwd) "/../../module"))

(use-modules (lang javascript parser))
(use-modules (sxml match))
(use-modules (sxml fold))
(use-modules ((srfi srfi-1) #:select (fold)))
(use-modules (ice-9 pretty-print))

(use-modules (jslib))

(define (fmtout fmt . args) (apply simple-format #t fmt args))
(define db #f)

(define (x-assn lhs op rhs)
  (case op
    ((assign) `(set! ,lhs ,rhs))
    ((add-assign) `(set! ,lhs (apply (@@ (jslib) JS+) ,lhs ,rhs)))
    (else
     (fmtout "x-assn unhandled: ~S\n" op)
     '(unknown))))

(define (lookup dict name)
  (cond
   ((not dict) #f)
   ((null? dict) #f)
   ((assoc-ref dict name))
   (else (lookup (assoc-ref dict '@P) name))))

(define (add-lexical name dict)
  (acons name `(lexical ,(string->symbol name) ,(gensym "JS~")) dict))
(define (add-toplevel name dict)
  (acons name `(toplevel ,(string->symbol name)) dict))
(define (add-reference name dict)
  (if (> (assq-ref dict '@l) 1)
      (add-lexical name dict)
      (add-toplevel name dict)))
(define (push-level dict)
  (list (cons '@l (1+ (assq-ref dict '@l))) (cons '@P dict)))
(define (pop-level dict)
  (assq-ref dict '@P))

;; body needs a line to build "var arguments" from Array(@args)
(define (make-function name args body)
  (let ((tagsym (gensym "JS~")) (valsym (gensym "JS~")))
    `(define ,(cadr name)
       (lambda ((name . ,(cadr name)))
	 (lambda-case ((() #f @args #f () ,(cddadr args))
		       (prompt
			(const return) ,body
			(lambda-case
			 (((tag val) #f #f #f () (,tagsym ,valsym))
			  (lexical val ,valsym))))))))))

;; In the case where we pick off ``low hanging fruit'' we need to coordinate
;; the actions of the up and down handlers.   The down handler will provide
;; a kid-seed in order and generate a null list.  The up handler, upon seeing
;; a null list, will just incorporate the kids w/o the normal reverse.

(define (fD node seed dict) ;; => node seed dict
  ;; This handles branches as we go down the tree.  We do two things here:
  ;; @enumerate
  ;; @item Pick off low hanging fruit: items we can quickly convert in entirety.
  ;; @item Add symbols to the dictionary.  This keeps track of lexical scope.
  ;; @end enumerate
  (if db (fmtout "D node =~S\n  seed =~S\n  dict =~S\n" node seed dict))
  (sxml-match node
    ((NullLiteral)
     (values '() `JS-null dict))
	     
    ((Identifier ,name)
     (values '() (lookup dict name) dict))
    
    ((PrimaryExpression (Identifier ,name))
     (values '() (lookup dict name) dict))

    ((PrimaryExpression (StringLiteral ,str))
     (values '() `(const ,str) dict))

    ((PrimaryExpression (NumericLiteral ,val))
     (values '() `(const ,(string->number val)) dict))

    ((obj-ref ,object ,ident)
     ;; Convert the tree: obj.ref ==> obj["ref"]
     (values
      `(ary-ref ,object (PrimaryExpression (StringLiteral ,(cadr ident))))
      '() dict))

    ((VariableDeclaration (Identifier ,name) ,rest ...)
     (values node '() (add-reference name dict)))

    ((FunctionDeclaration (Identifier ,name) ,rest ...)
     (values node '() (push-level (add-reference name dict))))
    
    ((FormalParameterList ,idlist ...)
     (let* ((args (add-lexical "@args" dict))
	    (dikt (fold
		   (lambda (name seed)
		     (acons name
			    ;;`(apply (@@ (jslib) lkup) ,args (const ,name))
			    `(lkup @args ,name)
			    seed))
		   args
		   (map cadr idlist)))
	    )
       (values node '() dikt)))
    
    ((SourceElements ,elts ...)
     (values node '() (push-level dict)))

    (,otherwise
     (values node '() dict))
    ))

(define (fU node seed dict kseed kdict) ;; => seed dict
  ;; This routine rolls up processes leaves into the current branch.
  (if db (fmtout "U node =~S\n  seed =~S\n  kseed=~S\n  dict =~S\n  kdict=~S\n"
		 node seed kseed dict kdict))
  (if
   (null? node) (values (cons kseed seed) dict)
   (case (car node)
     
     ((CallExpression)
      (values (cons `(apply ,(cadr kseed) ,(car kseed)) seed) dict))

     ((ArgumentList)
      (values (append kseed seed) dict))

     ((ary-ref)
      (values (cons `(apply (@@ (jslib) lkup) ,(cadr kseed) ,(car kseed)) seed)
	      dict))

     ((obj-ref) ;; ???
      (values (cons `(apply (@@ (jslib) lkup) ,(cadr kseed) ,(car kseed)) seed)
	      dict))
     
     ((AssignmentExpression)
      (values (cons (apply x-assn (reverse kseed)) seed) dict))

     ((VariableStatement VariableDeclarationList)
      (values (append (reverse kseed) seed) kdict))

     ((VariableDeclaration)
      (values
       (cons
	(if (= 2 (length kseed))
	    `(define ,(cadr (list-ref kseed 1)) ,(list-ref kseed 0))
	    `(define ,(cadr (list-ref kseed 0)) (@@ (jslib) undefined)))
	seed)
       kdict))
     
     ((Initializer)
      (values (cons (car kseed) seed) dict))

     ((EmptyStatement)
      (values seed dict))

     ((ReturnStatement) ;; will need a prompt for return, until optimized
      (values (cons `(abort (const return) (,(car kseed)) (const ())) seed)
	      dict))

     ((FunctionDeclaration)
      (values
       (let ((name (caddr kseed)) (args (cadr kseed)) (body (car kseed)))
	 (cons (make-function name args body) seed))
       dict))

     ((FormalParameterList)
      (values
       (cons `(params ,(lookup kdict "@args") ,@(reverse kseed)) seed)
       dict))

     ((SourceElements)
      (values (cons `(begin ,@(reverse kseed)) seed) dict))

     ((Program)
      (values (car kseed) dict))
     
     (else
      ;;(fmtout "  ^=== no handler\n")
      (cond
       ((null? seed) (values (reverse kseed) dict))
       ((null? kseed) (values (cons (car node) seed) dict)) ;; ???
       (else (values (cons (reverse kseed) seed) dict)))))))

(define (fH atom seed dict)
  (if db (fmtout "H atom =~S\n  seed =~S\n  dict =~S\n" atom seed dict))
  (if (string? atom) (values (cons atom seed) dict)
      (case atom
	((add) (values (cons* '(@@ (jslib) JS+) 'apply seed) dict))
	(else (values seed dict)))))

(define (doit tree seed dict)
  (foldts*-values fD fU fH tree seed dict))

;; ===================================

(define res (with-input-from-file "lang/javascript/ex1.js" parse-js))
(define rez1
  '(Program
    (SourceElements
     (PrimaryExpression (NumericLiteral "26.01")))))
(define rez2
  '(Program
    (SourceElements
     (CallExpression
      (obj-ref
       (PrimaryExpression (Identifier "Math"))
       (Identifier "sqrt"))
      (ArgumentList
       (PrimaryExpression (NumericLiteral "26.01")))))))
(define rez3
  '(Program
    (SourceElements
     (FunctionDeclaration
      (Identifier "foo")
      (FormalParameterList
       (Identifier "a")
       (Identifier "b")
       )
      (SourceElements
       (EmptyStatement)
       (ReturnStatement
	(PrimaryExpression (NumericLiteral "1"))))))))

(set! db #f)
(define x0 res)

(system "cat lang/javascript/ex1.js")
(fmtout "==(parser)==> \n")
(pretty-print x0)
#|
(fmtout "==(foldts*-values)==> \n")
(define x1 (doit x0 '() JSdict))
(pretty-print x1)
(use-modules (language tree-il))
(define x2 (parse-tree-il x1))
(fmtout "==(compile)==> \n")
(define x3 (compile x2 #:from 'tree-il #:env (current-module)))
(simple-format #t "~S\n" x3)
|#

;; --- last line ---
