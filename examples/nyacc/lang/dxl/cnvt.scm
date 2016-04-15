;;; nyacc/lang/dxl/cnvt.scm -- convert dxl to javascript
;;;
;;; Copyright (C) 2015,2016 Matthew R. Wette
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

;;(add-to-load-path (string-append (getcwd) "/../../../nyacc.git/module"))
;;(add-to-load-path (string-append (getcwd) "/../../../nyacc.git/examples"))
(add-to-load-path (string-append (getcwd) "/../.."))

(use-modules (nyacc lang util))
(use-modules (nyacc lang dxl mach))
(use-modules (nyacc lang dxl mach))
(use-modules (nyacc lang javascript pprint))
(use-modules (sxml match))
(use-modules (sxml fold))
(use-modules ((srfi srfi-1) #:select (fold append-reverse)))
(use-modules (ice-9 pretty-print))

(define (fmtout fmt . args) (apply simple-format #t fmt args))
(define db #f)

;; @deffn elifify tree => tree
;; Convert else (if to else-if.  See lang/c99/util1.scm.
(define (elifify tree)
  (define (fU tree)
    (simple-format #t "\n") (pretty-print tree)
    (sxml-match tree
      ((if ,x1 ,tp (if ,x2 ,t2 ,f2))
       `(if ,x1 ,tp (else-if ,x2 ,t2) ,f2))
      (,otherwise
       tree)))
  (foldt fU identity tree))

(define (lookup dict name)
  (cond ((not dict) #f)
	((null? dict) #f)
	((assoc-ref dict name))
	(else (lookup (assoc-ref dict '@P) name))))
(define (push-level dict)
  (list (cons '@P dict)))
(define (pop-level dict)
  (assq-ref dict '@P))

(define (fD tree seed dict) ;; => tree seed dict
  ;; This handles branches as we go down the tree.  We do two things here:
  ;; @enumerate
  ;; @item Pick off low hanging fruit: items we can quickly convert in entirety.
  ;; @item Add symbols to the dictionary.  This keeps track of lexical scope.
  ;; @end enumerate
  (if db (fmtout "D tree =~S\n  seed =~S\n  dict =~S\n" tree seed dict))
  (sxml-match tree
    ((pragma ,item ...)
     (values '() `pragma dict))

    ((vble-defn ,type ,vble-list)
     ;; Remove the type specifier.
     (values (cons (car tree) (cddr tree)) '() dict))
	     
    ((fctn-defn ,type . ,rest)
     (values (cons 'fctn-defn rest) '() (push-level dict)))

    ((param (type-name ,type) (ident ,ident))
     (values '() `(Identifier ,ident) dict))

    ((ref-param (type-name ,type) (ident ,ident)) ; can't handle this yes
     (values `(ref-param (ident ,ident)) '() dict))

    ((concat ,expr ,concat)
     (let* ((id (and (eqv? 'ident (caadr expr)) (cadadr expr)))
	    (ty (and id (assoc-ref dict id))))
       (if (eqv? ty 'vble)
	   (values tree '() dict)	; handled below
	   (values `(fctn-call ,expr (expr-list ,concat)) '() dict))))

    ((current (@ (type ,name)))		; cast
     (values '() `(CallExpression
		   (PrimaryExpression
		    (Identifier ,(string-append "RM.getCurrent" name)))
		   (Arguments)) dict))

    ((string ,val)
     (values '() `(StringLiteral ,val) dict))

    (,otherwise
     (values tree '() dict))
    ))

(define (fU tree seed dict kseed kdict) ;; => seed dict
  ;; This routine rolls up processes leaves into the current branch.
  (if db (fmtout "U tree =~S\n  seed =~S\n  kseed=~S\n  dict =~S\n  kdict=~S\n"
		 tree seed kseed dict kdict))
  (if
   (null? tree) (values (cons kseed seed) dict)
   (case (car tree)

     ((program)
      (values `(Program ,(reverse kseed) . ,seed) dict))

     ;; declarations 
     ((vble-defn vble-defn-list)
      ;; We need to push dict entries for defined variables up.
      (values (cons (reverse kseed) seed) kdict))
      
     ((vble-list-item)
      ;;(fmtout "VBLE: ~S\n" (cadar kseed))
      (values
       (cons (reverse kseed) seed)
       (acons (cadadr tree) 'vble kdict)))
     
     ;; statements
     #;((stmt-list)
      (values (cons `(Block ,(reverse kseed)) seed) dict))
     
     ((for-all-olinks)
      (let* ((iden (list-ref kseed 3))	; identifier
	     (objx (list-ref kseed 2))	; object expression
	     (lnkx (list-ref kseed 1))	; link expression
	     (stmt (list-ref kseed 0))	; statement
	     (call `(CallExpression
		     (PrimaryExpression (Identifier "obj_links_to"))
		     (Arguments
		      (ArgumentList ,objx ,lnkx)))))
	(values (cons `(for-in ,iden ,call ,stmt) seed) dict)))

     ;; expressions
     ((fctn-call)
      (let* ((id (cadr kseed))
	     (xl (cdar kseed))
	     (jsid `(PrimaryExpression ,id))
	     (jsxl `(Arguments (ArgumentList ,@xl))))
	(values (cons `(CallExpression ,jsid ,jsxl) seed) dict)))

     ((concat)
      ;; If we get here concat is a concat-op (on strings).
      (values (cons (cons 'add (cdr (reverse kseed))) seed) dict))

     (else
      ;;(fmtout "  ^=== no handler\n")
      (cond
       ((null? seed) (values (reverse kseed) dict))
       ;;((null? kseed) (values (cons (car tree) seed) dict)) ;; ???
       (else (values (cons (reverse kseed) seed) dict)))))))

(define (fH atom seed dict) ;; => seed dict
  (if db (fmtout "H atom =~S\n  seed =~S\n  dict =~S\n" atom seed dict))
  (if (string? atom) (values (cons atom seed) dict)
      (case atom
	((program) (values (cons 'SourceElements seed) dict))
	((vble-defn) (values (cons 'VariableStatement seed) dict))
	((vble-defn-list) (values (cons 'VariableDeclarationList seed) dict))
	((vble-list-item) (values (cons 'VariableDeclaration seed) dict))
	((initzer) (values (cons 'Initializer seed) dict))
	((fctn-defn) (values (cons 'FunctionDeclaration seed) dict))
	((param-list) (values (cons 'FormalParameterList seed) dict))
	((stmt-list) (values (cons 'StatementList seed) dict))
	((comp-stmt) (values (cons 'Block seed) dict))
	((expr-stmt) (values (cons 'ExpressionStatement seed) dict))
	((if) (values (cons 'IfStatement seed) dict))
	;;((param) (values seed dict))
	((return) (values (cons 'ReturnStatement seed) dict))
	((fctn-call) (values (cons 'CallExpression seed) dict))
	((assn-expr) (values (cons 'AssignmentExpression seed) dict))
	((obj-sel) (values (cons 'ary-ref seed) dict))
	((p-expr) (values (cons 'PrimaryExpression seed) dict))
	((ident) (values (cons 'Identifier seed) dict))
	((fixed) (values (cons 'NumericLiteral seed) dict))
	(else (values (cons atom seed) dict)))))

(define (doit tree seed dict)
  (foldts*-values fD fU fH tree seed dict))

;; ===================================

(define dxl-dict
  '(("regexp" . fctn)
    ))

(define infile "getmodxml.dxl")
;;(define infile "exam.d/ex.dxl")
(set! db #f)

(define x0
  (let* ((sx (with-input-from-file infile
	      (lambda () (dev-parse-dxl #:debug #f))))
	 (sx (elifify sx)))
    sx))

;;(system (string-append "cat " infile))
(fmtout "==(parser)==> \n")
(pretty-print x0)
(when #f
  (let ((x1 (doit x0 '() dxl-dict)))
    (fmtout "==(cnvter)==> \n")
    (pretty-print x1)
    (fmtout "==(pprint)==> \n")
    (pretty-print-js x1)))

;; --- last line ---
