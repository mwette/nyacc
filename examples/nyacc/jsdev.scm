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

(define (x-assn lhs op rhs)
  (case op
    ((add-assign)
     `(set! ,lhs (+ ,lhs ,rhs)))
    (else
     '(unknown))))

(define (x-defs dict)
  ;;(fmtout "DEFS: dict=~S\n" dict)
  (let iter ((res '()) (defs dict))
    (cond
     ((null? defs) (reverse res))
     ((symbol? (caar defs)) (iter res (cdr defs)))
     ((pair? (cdar defs))
      (iter (cons `(define ,(cadar defs) ,(cddar defs)) res) (cdr defs)))
     (else
      (iter (cons `(define ,(cdar defs)) res) (cdr defs))))))

(define (lookup dict name)
  (cond
   ((not dict) #f)
   ((null? dict) #f)
   ((assoc-ref dict name) => (lambda (val) (if (pair? val) (car val) val)))
   (else (lookup (assq-ref dict '@P) name))))

(define (fd1 node seed dict) ;; => node seed dict
  #;(fmtout "D node =~S\n  seed =~S\n  dict =~S\n" node seed dict)
  (sxml-match node
    ((SourceElements ,elts ...)
     (values
      node '()
      (list (cons '@l (1+ (assq-ref dict '@l)))
	    (cons '@P dict))))

    ((VariableDeclaration (Identifier ,name) ,rest ...)
     (let ((symb (gensym "JS~")))
       (values node '() (acons name symb dict))))

    ((PrimaryExpression (Identifier ,name))
     ;; Convert here.
     (let ((gsym (lookup dict name)))
       (values '(PrimaryExpression) `(lexical ,name ,gsym) dict)))
    ((PrimaryExpression (NumericLiteral ,val))
     (values '(PrimaryExpression) `(const ,(string->number val)) dict))

    ((Identifier ,name)
     (let ((gsym (lookup dict name)))
       (values '(Identifier) `((lexical ,name ,gsym)) dict)))

    (,otherwise
     (values node '() dict))
    ))

(define (fu1 node seed dict kseed kdict) ;; => seed dict
  #;(fmtout "U node =~S\n  seed =~S\n  dict =~S\n  kseed=~S\n  kdict=~S\n"
  node seed dict kseed kdict)
  (if
   (null? node) (values seed dict)
   (case (car node)
     ((SourceElements) ;;(fmtout "  SOURCE\n")
      (values `(begin ,@(x-defs kdict) ,@(reverse kseed)) dict))

     ((VariableDeclaration)
      (values
       seed
       (if (eqv? 2 (length kseed))
	   (let* ((ini (car kseed)) (ref (cadr kseed))
		  (name (cadr ref)) (gsym (caddr ref)))
	     (acons name (cons gsym ini) dict))
	   dict)))

     ((Initializer)
      (values (cons (car kseed) seed) dict))

     ((EmptyStatement) ;;(fmtout "  EMPTY\n")
      (values seed dict))

     ((AssignmentExpression)
      (values (cons (apply x-assn (reverse kseed)) seed) dict))

     ((PrimaryExpression)
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
(let ((seed '()) (dict (list (cons '@l 1) (cons '@P '()))))
  (pretty-print rez)
  (when #t
    (fmtout "===> \n")
    (pretty-print (doit rez seed dict))
    )
  (newline))
	      
;; --- last line ---
