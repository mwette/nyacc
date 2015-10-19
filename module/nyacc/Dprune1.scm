;; ,prune.scm

(use-modules ((srfi srfi-1) #:select (lset-adjoin)))
(define (fmtout fmt . args) (apply simple-format #t fmt args))

(define non-terms
  '(ExpressionList ObjectLiteral AssignmentExpression Expression $P1
		   StatementList Statement))

(define terminals '("=" $ident "}" "{" $end))

(define lhs-v
  #($start				; 0
    Statement				; 1
    Statement				; 2
    $P1					; 3
    StatementList			; 4
    StatementList			; 5
    Expression				; 6
    Expression				; 7
    AssignmentExpression		; 8
    ObjectLiteral			; 9
    ExpressionList			; 10
    ExpressionList))			; 11

(define rhs-v
  #(#(Statement)			; 0
    #(#\{ StatementList #\})		; 1
    #($P1)				; 2
    #(Expression)			; 3
    #(Statement)			; 4
    #(StatementList Statement)		; 5
    #(AssignmentExpression)		; 6
    #(ObjectLiteral)			; 7
    #($ident #\= Expression)		; 8
    #(#\{ ExpressionList #\})		; 9
    #(Expression)			; 10
    #(ExpressionList Expression)))	; 11

(define prune-al
  '(($P1 . (ObjectLiteral))))

(define (terminal? symb) (member symb terminals))

(define (prule-range lhs)
  (let* ((n (vector-length lhs-v))
	 (match? (lambda (ix symb) (eqv? (vector-ref lhs-v ix) symb))))
    (cond
     ((terminal? lhs) '())
     ((eq? lhs '$epsilon) '())
     (else
      (let iter-st ((st 0))
	;; Iterate to find the start index.
	(if (= st n) '()		; not found
	    (if (match? st lhs)
		;; Start found, now iteratate to find end index.
		(let iter-nd ((nd st))
		  (if (= nd n) (cons st nd)
		      (if (not (match? nd lhs)) (cons st nd)
			  (iter-nd (1+ nd)))))
		(iter-st (1+ st)))))))))
(define (range-next rng)
  (if (null? rng) '()
      (let ((nxt (cons (1+ (car rng)) (cdr rng))))
	(if (= (car nxt) (cdr nxt)) '() nxt))))
(define (range-last? rng)
  (and (pair? rng) (= (1+ (car rng)) (cdr rng))))
(define (range-done? rng)
  (null? rng))

(define (rhs-first-symb gx)
  (let ((rhs (vector-ref rhs-v gx)))
    (if (zero? (vector-length rhs))
	'$epsilon
	(vector-ref rhs 0))))

;; gxl - grammar index list

;; This should return a list of prule incides.
(define (non-kernels symbol)
  (let iter ((gxl '())
	     (prune-l '())
	     (rng (prule-range symbol)))
    (fmtout "gxl=~S prune-l=~S\n" gxl prune-l)
    (if (range-done? rng) gxl
	(let* ((gx (car rng))
	       (rhs1 (rhs-first-symb gx))
	       (png (assq-ref prune-al rhs1))
	       (prune1 (if png (append png prune-l) prune-l)))
	  (fmtout "  rhs1=~S png=~S\n" rhs1 png)
	  (if (memq rhs1 prune-l)
	      (iter gxl prune1 (range-next rng))
	      (iter (iter (lset-adjoin eqv? gxl gx) prune1 (prule-range rhs1))
		    prune1 (range-next rng)))))))

(let* ((rng0 (prule-range 'Statement))
       (rng1 (range-next rng0))
       (rng2 (range-next rng1))
       (rng3 (range-next rng2)))
  ;;(fmtout "~S\n" rng0) (fmtout "~S\n" rng1) (fmtout "~S\n" rng3)
  ;;(fmtout "~S\n" (range-first rng0))
  (fmtout "=> ~S\n" (non-kernels 'Statement))
  )
   

