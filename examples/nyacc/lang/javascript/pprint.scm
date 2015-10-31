;; nyacc/lang/javascript/pp.scm
;;

(define-module (nyacc lang javascript pprint)
  #:export (pretty-print-js)
  #:use-module ((srfi srfi-1) #:select (pair-for-each))
  )

(define (pretty-print-js tree)

  (letrec
      ((col 0)
       (lev 0)
       (bl "                        ")
       (is (lambda (il) (substring bl 0 (* 2 il)))) ; indent string
       (cs (lambda (il) (substring bl 0 (+ 4 (* 2 il))))) ; cont str
       
       (sf
	(lambda (fmt . args)
	  (apply simple-format #t fmt args)))

       (ppx
	(lambda (il tree)
	  ;;(sf "car tree=~S\n" (car tree))
	  (case (car tree)

	    ((Program)
	     (ppx il (cadr tree)))	; should be start

	    ((SourceElements)
	     (for-each
	      (lambda (el)
		(unless (eqv? 'EmptyStatement (car el))
		  (sf "~A" (is il))
		  (ppx il el)
		  (sf "\n")))
	      (cdr tree)))

	    ((FunctionDeclaration)
	     (let ((name (list-ref tree 1))
		   (parl (list-ref tree 2))
		   (body (list-ref tree 3)))
	       (sf "function ~A(" (cadr name))
	       (ppx il parl)
	       (sf ") {\n")
	       (ppx (1+ il) body)
	       (sf "}")
	       ))

	    ((FormalParameterList)
	     (pair-for-each
	      (lambda (pair)
		(sf "~A" (cadar pair))
		(if (pair? (cdr pair)) (sf ", ")))
	      (cdr tree)))

	    ((ExpressionStatement)
	     (ppx il (list-ref tree 1))
	     (sf ";"))
	     
	    ((ReturnStatement)
	     (sf "return")
	     (when (< 1 (length tree)) (sf " ") (ppx il (cadr tree)))
	     (sf ";"))

	    ((VariableStatement)
	     (sf "var ")
	     (for-each (lambda (el) (ppx il el)) (cdr tree)))

	    ((VariableDeclarationList)
	     (pair-for-each
	      (lambda (pair)
		;;(sf "decl=~S\n" (car pair))
		(let* ((decl (car pair))
		       (id (cadr (list-ref decl 1)))
		       (val (and (< 2 (length decl)) (list-ref decl 2))))
		  (sf "~A" id)
		  (if val (ppx il val))
		  (if (pair? (cdr pair))
		      (sf ", ")
		      (sf ";"))
		  ))
	      (cdr tree)))

	    ((Initializer) (sf " = ") (ppx il (cadr tree)))

	    ((AssignmentExpression)
	     (ppx il (list-ref tree 1))
	     (ppx il (list-ref tree 2))
	     (ppx il (list-ref tree 3)))

	    ((assign) (sf " = "))
	    ((mul-assign) (sf " *= "))
	    ((div-assign) (sf " /= "))
	    ((mod-assign) (sf " %= "))
	    ((add-assign) (sf " += "))
	    ((sub-assign) (sf " -= "))
	    ((lshift-assign) (sf " <<= "))
	    ((rshift-assign) (sf " >>= "))
	    ((rrshift-assign) (sf " >>>= "))
	    ((and-assign) (sf " &= "))
	    ((xor-assign) (sf " ^= "))
	    ((or-assign) (sf " |= "))

	    ((CallExpression)
	     (ppx il (list-ref tree 1))
	     (ppx il (list-ref tree 2)))

	    ((Arguments)
	     (sf "(")
	     (if (< 1 (length tree)) (ppx il (list-ref tree 1)))
	     (sf ")"))

	    ((ArgumentList)
	     (pair-for-each
	      (lambda (pair)
		(ppx il (car pair))
		(if (pair? (cdr pair)) (sf ", ")))
	      (cdr tree)))
	    
	    ((add)
	     ;; NEED TO FIX So that "scope" added for printing parens
	     (sf "(")
	     (ppx il (list-ref tree 1))
	     (sf " + ")
	     (ppx il (list-ref tree 2))
	     (sf ")"))

	    ((PrimaryExpression) (ppx il (cadr tree)))

	    ((NumericLiteral) (sf "~A" (cadr tree)))
	    ((Identifier) (sf "~A" (cadr tree)))

	    (else
	     #f))))
       )
    
    (ppx 0 tree)))

;; --- last line ---
