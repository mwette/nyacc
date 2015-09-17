;; jsdev.scm -- javascript dev

(add-to-load-path (getcwd))
(add-to-load-path (string-append (getcwd) "/../../module"))
(add-to-load-path (string-append (getcwd) "/../../../nyacc.dev"))

(use-modules (lang javascript parser))

(use-modules ((sxml xpath) #:select (sxpath)
              #:renamer (lambda (s) (if eq? s 'filter) 'xp-filter s)))
(use-modules (sxml match))
(use-modules (sxml fold))
(use-modules (srfi srfi-1))
(use-modules (ice-9 pretty-print))

(define (fmtout fmt . args) (apply simple-format #t fmt args))

(define res (with-input-from-file "lang/javascript/ex1.js" parse-js))
;;(pretty-print res)

;; document.print("hello\n")
;; (hashq-set! htab 'print (lambda () ...))

(define (x-assn lhs op rhs)
  (case op
    ((add-assign)
     `(set! ,lhs (apply (toplevel +) ,lhs ,rhs)))
    (else
     '(unknown))))

(define (x-defs dict)
  ;;(fmtout "DEFS: dict=~S\n" dict)
  (let iter ((res '()) (defs dict))
    (cond
     ((null? defs) (reverse res))
     ((symbol? (caar defs)) (iter res (cdr defs)))
     ((pair? (cdar defs))
      (iter (cons `(define ,(cdar defs) 99) res) (cdr defs)))
     (else
      (iter (cons `(define ,(cdar defs)) res) (cdr defs))))))

(define (lookup dict name)
  (cond
   ((not dict) #f)
   ((null? dict) #f)
   ((assoc-ref dict name) =>
    (lambda (val) (cons (if (pair? val) (car val) val) (assoc-ref dict '@l))))
   (else (lookup (assq-ref dict '@P) name))))

(define (fd1 node seed dict) ;; => node seed dict
  #;(fmtout "D node =~S\n  seed =~S\n  dict =~S\n" node seed dict)
  (sxml-match node
    ((SourceElements ,elts ...)
     (values
      node '()
      (list (cons '@l (1+ (assoc-ref dict '@l))) ; push level
	    (cons '@P dict))))

    ((VariableDeclaration (Identifier ,name) ,rest ...)
     (values
      node '()
      (if (= 1 (assoc-ref dict '@l))
	  (acons name `(toplevel ,(string->symbol name)) dict)
	  (acons name `(lexical ,(string->symbol name) ,(gensym "JS~")) dict))))

    ((PrimaryExpression (Identifier ,name))
     (values '(PrimaryExpression) (assoc-ref dict name) dict))
    ((PrimaryExpression (NumericLiteral ,val))
     (values '(PrimaryExpression) `(const ,(string->number val)) dict))

    ((Identifier ,name)
     (values '(Identifier) (assoc-ref dict name) dict))
    
    (,otherwise
     (values node '() dict))
    ))

(define (fu1 node seed dict kseed kdict) ;; => seed dict
  (fmtout "U node =~S\n  seed =~S\n  dict =~S\n  kseed=~S\n  kdict=~S\n"
	  node seed dict kseed kdict)
  (if
   (null? node) (values seed dict)
   (case (car node)
     ((SourceElements)
      (values `(begin ,@(x-defs kdict) ,@(reverse kseed)) dict))

     ((VariableDeclaration)
      (values
       seed
       kdict))

     ((Initializer)
      (values (cons (car kseed) seed) dict))

     ((EmptyStatement) ;;(fmtout "  EMPTY\n")
      (values seed dict))

     ((AssignmentExpression)
      (values (cons (apply x-assn (reverse kseed)) seed) dict))

     ((PrimaryExpression)
      (values (cons kseed seed) dict))

     ((Identifier)
      (values (cons kseed seed) dict))

     ;; (else (values seed dict))
     (else
      (cond
       ((null? seed) (values (reverse kseed) dict))
       ((null? kseed) (values (cons (car node) seed) dict))
       (else (values (cons (reverse kseed) seed) dict))
       )))))

(define (fh1 atom seed dict)
  #;(fmtout "H atom =~S\n  seed =~S\n  dict =~S\n" atom seed dict)
  (if (string? atom) (values (cons atom seed) dict)
      (case atom
	(else (values seed dict)))))

(define (doit tree seed dict)
  (let ((fd fd1) (fu fu1) (fh fh1))
    (foldts*-values fd fu fh tree seed dict)))

(define rez
  '(SourceElements
    (EmptyStatement)
    (VariableDeclaration
     (Identifier "x")
     (Initializer
      (PrimaryExpression (NumericLiteral "5"))))
    (AssignmentExpression
     (PrimaryExpression (Identifier "x"))
     (add-assign)
     (PrimaryExpression (NumericLiteral "10")))
    ))

(define (init-dict) (list (cons '@l 0) (cons '@P '())))

(define x0 rez)
(pretty-print x0)

(fmtout "===> \n")
(define x1 (doit rez '() (init-dict)))
(pretty-print x1)

;;(use-modules (language tree-il))
;;(define x2 (parse-tree-il x1))


;; --- last line ---
