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
(define (push-level dict)
  (list (cons '@l (1+ (assq-ref dict '@l))) (cons '@P dict)))
(define (pop-level dict)
  (assq-ref dict '@P))

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
     (values
      node '()
      (if (> 1 (lookup dict '@l))
	  ;;(acons name `(lexical ,(string->symbol name) ,(gensym "JS~")) dict)
	  ;;(acons name `(toplevel ,(string->symbol name)) dict))))
	  (add-lexical name dict)
	  (add-toplevel name dict))))

    ((FunctionDeclaration (Identifier ,name) ,rest ...)
     (values
      node '()
      (let ((lev (lookup dict '@l)))
	(list
	 (cons '@l (1+ lev))
	 (cons '@P
	       (if (> lev 1)
		   (acons name `(lexical ,(string->symbol name) ,(gensym "JS~"))
			  dict)
		   (acons name `(toplevel ,(string->symbol name))
			  dict)))))))
    
    ((FunctionParameterList ,idlist ...)
     (values
      node '()
      (append
       (map
	(lambda (name) name `(lexical ,(string->symbol name) ,(gensym "JS~")))
	(map cdr idlist))
       dict)))

    ((SourceElements ,elts ...)
     (values
      node '()
      (list (cons '@l (1+ (lookup dict '@l))) ; push level
	    (cons '@P dict))))

    (,otherwise
     (values node '() dict))
    ))

(define (fU node seed dict kseed kdict) ;; => seed dict
  ;; This routine rolls up processes leaves into the current branch.
  (if db (fmtout "U node =~S\n  seed =~S\n  dict =~S\n  kseed=~S\n  kdict=~S\n"
		 node seed dict kseed kdict))
  (if
   (null? node) (values (cons kseed seed) dict)
   (case (car node)
     #;((Identifier)
      (values (cons kseed seed) dict))

     #;((PrimaryExpression)
      (values (cons kseed seed) dict))

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
      (values (cons `(return ,kseed) seed) dict))

     ((FunctionDeclaration)
      (values
       (let (;;(name (caddr kseed))
	     ;;(args (cadr kseed))
	     ;;(body (cadr kseed))
	     )
	 ;;(fmtout "name=~S\nargs=~S\nbody=~S\n" name args body)
	 (cons `(lambda () ,kseed) seed))
       dict))

     ((FunctionParameterList)
      (values (reverse kseed) dict))

     ((SourceElements)
      (values `(begin ,@(reverse kseed)) dict))
     
     (else
      ;;(fmtout "  ^=== no handler\n")
      (cond
       ((null? seed) (values (reverse kseed) dict))
       ((null? kseed) (values (cons (car node) seed) dict)) ;; ???
       (else (values (cons (reverse kseed) seed) dict))
       )))))


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
  '(SourceElements
    (PrimaryExpression (NumericLiteral "26.01"))))
(define rez2
  '(SourceElements
    (CallExpression
     (obj-ref
      (PrimaryExpression (Identifier "Math"))
      (Identifier "sqrt"))
     (ArgumentList
      (PrimaryExpression (NumericLiteral "26.01"))))))
(define rez3
  '(SourceElements
    (FunctionDeclaration
     (Identifier "foo")
     (FormalParameterList
      (Identifier "a")
      (Identifier "b"))
     (SourceElements
      (EmptyStatement)
      (ReturnStatement
       (PrimaryExpression (NumericLiteral "1")))))))
(set! db #t)

(system "cat lang/javascript/ex1.js")
(fmtout "==(parser)==> \n")
(define x0 rez3)
(pretty-print x0)
(fmtout "==(foldts*-values)==> \n")
(define x1 (doit x0 '() JSdict))
(pretty-print x1)
#|
(use-modules (language tree-il))
(define x2 (parse-tree-il x1))
(fmtout "==(compile)==> \n")
(define x3 (compile x2 #:from 'tree-il #:env (current-module)))
(simple-format #t "~S\n" x3)
|#

;; --- last line ---
