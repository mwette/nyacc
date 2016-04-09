;;

;; (bison-
;;   (grammar (rules ...) (terminals ...) (nonterminals ...))
;;   (automaton (state ...)

;;                 (rule (@ (usefulness "useful") (number "37"))
;;                        (lhs "non_comment_statement")
;;                        (rhs (symbol "command")
;;                             (symbol "ident_nc_list")
;;                             (symbol "term")))
;;                  (rule (@ (usefulness "useful") (number "38"))
;;                        (lhs "lval_expr_list")
;;                        (rhs (symbol "lval_expr")))

(use-modules (sxml simple))
(use-modules (sxml match))
(use-modules (sxml fold))
(use-modules (sxml xpath))
(use-modules (ice-9 pretty-print))
(use-modules ((srfi srfi-1) #:select (fold)))
(use-modules ((srfi srfi-43) #:select (vector-for-each vector-map)))
(use-modules (nyacc export))

(include-from-path "nyacc/lang/matlab/bis.spec")

;;
;; generate vector that maps bison rule # to nyacc rule #
;; currently returns only an alist
(define (chewon-grammar tree lhs-v rhs-v terms) ;; bison-rule => nyacc-rule map

  ;; match rule index, if no match return @code{-1}
  ;; could be improved by starting with last rule number and wrapping
  (define (match-rule lhs rhs)
    (let iter ((ix 0))
      (if (eqv? ix (vector-length lhs-v)) -1
	  (if (and (equal? lhs (elt->bison (vector-ref lhs-v ix) terms))
		   (equal? rhs (vector->list
				(vector-map
				 (lambda (ix val) (elt->bison val terms))
				 (vector-ref rhs-v ix)))))
	      ix
	      (iter (1+ ix))))))

  ;; this is a fold
  (define (rule->index-al tree seed)
    (sxml-match tree
      ;; Skip first bison rule: always $accept.
      ((rule (@ (number "0")) (lhs "$accept") . ,rest)
       (acons 0 0 seed))
      ;; This matches all others.
      ((rule (@ (number ,n)) (lhs ,lhs) (rhs (symbol ,rhs) ...))
       (acons (string->number n) (match-rule lhs rhs) seed))
      (,otherwise seed)))

  (fold rule->index-al '() ((sxpath '(// rule)) tree)))

(define (chewon-automaton tree gx-al bs->ns)

  (define (do-state state)
    ;;(simple-format #t "~S\n" (cadr state))
    (let* ((b-items ((sxpath '(// item)) state))
	   (n-items (fold
		     (lambda (tree seed)
		       (sxml-match tree
			 ((item (@ (rule-number "0") (point "2"))) seed)
			 ((item (@ (rule-number ,rn) (point ,pt)) . ,rest)
			  (acons (assq-ref gx-al (string->number rn))
				 (string->number pt) seed))
			 (,otherwise (error "broken item")))) '() b-items))
	   (b-trans ((sxpath '(// transition)) state))
	   (n-trans (map
		     (lambda (tree)
		       (sxml-match tree
			 ((transition (@ (symbol ,symb) (state ,dest)))
			  (cons* (bs->ns symb) 'shift (string->number dest)))
			 (,otherwise (error "broken tran")))) b-trans))
	   (b-redxs ((sxpath '(// reduction)) state))
	   (n-redxs (map
		     (lambda (tree)
		       (sxml-match tree
			 ((reduction (@ (symbol ,symb) (rule "accept")))
			  (cons* (bs->ns symb) 'accept 0))
			 ((reduction (@ (symbol ,symb) (rule ,rule)))
			  (cons* (bs->ns symb) 'reduce
				 (assq-ref gx-al (string->number rule))))
			 (,otherwise (error "broken redx" tree)))) b-redxs))
	   )
      (list
       (cons 'kis (reverse n-items))
       (cons 'pat (append n-trans n-redxs)))))

  (define st-numb
    (let ((xsnum (sxpath '(@ number *text*))))
      (lambda (state)
	(string->number (car (xsnum state))))))

  (let ((xsf (sxpath '(state itemset (@ (rule-number "0") (point "2"))))))
    (fold
     (lambda (state seed)
       (if (pair? (xsf state))
	   (acons 0 '() seed)		; extra state
	   (acons (st-numb state) (do-state state) seed)))
     '() (cdr tree))))

(define (atomize terminal)		; from lalr.scm
  (if (string? terminal)
      (string->symbol (string-append "$:" terminal))
      terminal))

(define (make-bison->nyacc-symbol-mapper terminals non-terms)
  (let ((bs->ns-al
	 (map (lambda (symb) (cons (elt->bison symb terminals) symb))
	      (append (map atomize terminals) non-terms))))
    (lambda (name) (assoc-ref bs->ns-al name))))

;; add %define lr.default-reduction accepting
(let* ((bsym->nsym (make-bison->nyacc-symbol-mapper terminals non-terms))
       (s0 (call-with-input-file "bis.xml"
             (lambda (p) (xml->sxml p #:trim-whitespace? #t))))
       (sG ((sxpath '(// grammar)) s0))
       (sG (if (pair? sG) (car sG) sG))
       (sA ((sxpath '(// automaton)) s0))
       (sA (if (pair? sA) (car sA) sA))
       (pG (chewon-grammar sG lhs-v rhs-v terminals))
       (pA (chewon-automaton sA pG bsym->nsym))
       (ns (1+ (caar pA)))
       (pat-v (make-vector ns '()))
       (kis-v (make-vector ns '()))
       )
  (for-each
   (lambda (state)
     (let ((sx (car state))
	   (pat (assq-ref (cdr state) 'pat))
	   (kis (assq-ref (cdr state) 'kis)))
       (vector-set! pat-v sx pat)
       (vector-set! kis-v sx kis)))
   pA)
  (pretty-print pA)
  ;;(simple-format #t "nstates=~A\n" ns)
  ;;(with-output-to-file "bis.sxml" (lambda () (pretty-print s0)))
  ;;(pretty-print pat-v)
  ;;(pretty-print kis-v)
  ;;(pretty-print (chewon-grammar sG lhs-v rhs-v terms))
  #t)

#|
bison:
(rules
 (rule (@ (usefulness "useful") (number "0"))
  (lhs "$accept")
  (rhs (symbol "mfile") (symbol "$end")))
 (rule (@ (usefulness "useful") (number "1"))
  (lhs "mfile")
  (rhs (symbol "function_defn")))
 (rule (@ (usefulness "useful") (number "2"))
  (lhs "function_defn")
  (rhs (symbol "function_decl") (symbol "opt_end")))
|#
#|
nyacc:
start
non-terms
terminals
lhs-v gx => LHS list of symbols
rhs-v gx => RHS list of symbols
rto-v gx => rule lengths
pat-v sx => (($ident . (shift . 3)) ('$:foo . (reduce . 6)) ...
kis-v sx => list of kernel items
kit-v sx => lookahead tokens per item
|#
#|
(state (@ (number "0"))
       (itemset
         (item (@ (rule-number "0") (point "0")))
         (item (@ (rule-number "1") (point "0")))
         (item (@ (rule-number "2") (point "0")))
         (item (@ (rule-number "5") (point "0")))
         (item (@ (rule-number "6") (point "0"))))
       (actions
         (transitions
           (transition
             (@ (type "shift")
                (symbol "FUNCTION")
                (state "1")))
           (transition
             (@ (type "goto") (symbol "mfile") (state "2")))
|#
 
