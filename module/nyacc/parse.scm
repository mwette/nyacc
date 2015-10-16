;;; nyacc/parse.scm
;;;
;;; Copyright (C) 2014, 2015 Matthew R. Wette
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;; make parser that provide list of la-toks to lexer:
;; e.g., if comment not in latok, just throw away

(define-module (nyacc parse)
  #:export (make-lalr-parser
	    wrap-action
	    make-lalr-x-parser
	    make-lalr-y-parser
	    )
  #:use-module (nyacc util)
  #:use-module ((srfi srfi-43) #:select (vector-map vector-for-each))
  )

;; @item (machine-hashed? mach) => #t|#f
;; Indicate if the machine has been hashed.
(define (machine-hashed? mach)
  (number? (caar (vector-ref (assq-ref mach 'pat-v) 0))))

;; @item make-arg-list N => '($N $Nm1 $Nm2 ... $1 . $rest)
;; This is a helper for @code{mkact}.
(define (make-arg-list n)
  (let ((mkarg
	 (lambda (i) (string->symbol (string-append "$" (number->string i))))))
    (let iter ((r '(. $rest)) (i 1))
      (if (> i n) r (iter (cons (mkarg i) r) (1+ i))))))

;; @item wrap-action (n . guts) => `(lambda ($n ... $2 $1 . $rest) ,@guts)
;; Wrap user-specified action (body, as list) of n arguments in a lambda.
;; The rationale for the arglist format is that we can @code{apply} this
;; lambda to the the semantic stack.
(define (wrap-action actn)
  (cons* 'lambda (make-arg-list (car actn)) (cdr actn)))

;; @item make-lalr-parser mach => parser
;; This generates a procedure that takes one argument, a lexical analyzer:
;; @example
;; (parser lexical-analyzer [#:debug #t])
;; @end example
;; and is used as
;; @example
;; (define xyz-parse (make-lalr-parser xyz-mach))
;; (with-input-from-file "sourcefile.xyz" (lambda () (xyz-parse (gen-lexer))))
;; @end example
;; The generated parser is reentrant.
(define* (make-lalr-parser mach)
  (let* ((len-v (assq-ref mach 'len-v))
	 (rto-v (assq-ref mach 'rto-v))	; reduce to
	 (pat-v (assq-ref mach 'pat-v))
	 (actn-v (assq-ref mach 'act-v)) ; unknown action vector
	 (xact-v (if (procedure? (vector-ref actn-v 0)) actn-v
		     (vector-map
		      ;; Turn symbolic action into executable procedures:
		      (lambda (ix f) (eval f (current-module)))
		      (vector-map
		       (lambda (ix actn) (wrap-action actn))
		       actn-v))))
	 ;;
	 (dmsg (lambda (s t a) (fmtout "state ~S, token ~S\t=> ~S\n" s t a)))
	 (hashed (number? (caar (vector-ref pat-v 0)))) ; been hashified?
	 ;;(def (assq-ref (assq-ref mach 'mtab) '$default))
	 (def (if hashed -1 '$default))
	 ;; predicate to test for shift action:
	 (shift? (if hashed
		     (lambda (a) (positive? a))
		     (lambda (a) (eq? 'shift (car a)))))
	 ;; On shift, transition to this state:
	 (shift-to (if hashed (lambda (x) x) (lambda (x) (cdr x))))
	 ;; predicate to test for reduce action:
	 (reduce? (if hashed
		      (lambda (a) (negative? a))
		      (lambda (a) (eq? 'reduce (car a)))))
	 ;; On reduce, reduce this production-rule:
	 ;;(reduce-pr (if hashed (lambda (a) (abs a)) (lambda (a) (cdr a))))
	 (reduce-pr (if hashed abs cdr))
	 ;; If no action found in transition list, then this:
	 (parse-error (if hashed #f (cons 'error 0)))
	 ;; predicate to test for error
	 (error? (if hashed
		     (lambda (a) (eq? #f a))
		     (lambda (a) (eq? 'error (car a)))))
	 )
    (lambda* (lexr #:key debug)
      (let iter ((state (list 0))	; state stack
		 (stack (list '$@))	; sval stack
		 (nval #f)		; prev reduce to non-term val
		 (lval (lexr)))		; lexical value (from lex'er)
	(let* ((tval (car (if nval nval lval))) ; token (syntax value)
	       (sval (cdr (if nval nval lval))) ; semantic value
	       (stxl (vector-ref pat-v (car state))) ; state transition list
	       (stx (or (assq-ref stxl tval) ; trans action (e.g. shift 32)
			(assq-ref stxl def)  ; default action
			parse-error)))
	  (if debug (dmsg (car state) (if nval tval sval) stx))
	  (cond
	   ((error? stx)
	    ;; Ugly to have to check this first every time, but
	    ;; @code{positive?} and @code{negative?} fail otherwise.
	    (let ((fn (or (port-filename (current-input-port)) "(unknown)"))
		  (ln (1+ (port-line (current-input-port)))))
	      (fmterr "~A:~A: parse failed at state ~A, on input ~S\n"
		      fn ln (car state) sval))
	    #f)
	   ((shift? stx)
	    ;; We could check here to determine if next transition only has a
	    ;; default reduction and, if so, go ahead and process the reduction
	    ;; without reading another input token.  Needed for interactive.
	    (iter (cons (shift-to stx) state) (cons sval stack)
		  #f (if nval lval (lexr))))
	   ((reduce? stx)
	    (let* ((gx (reduce-pr stx)) (gl (vector-ref len-v gx))
		   ($$ (apply (vector-ref xact-v gx) stack)))
	      (iter (list-tail state gl) 
		    (list-tail stack gl)
		    (cons (vector-ref rto-v gx) $$)
		    lval)))
	   (else ;; accept
	    (car stack))))))))

(define* (make-lalr-x-parser mach)
  (let* ((len-v (assq-ref mach 'len-v))
	 (rto-v (assq-ref mach 'rto-v))	; reduce to
	 (pat-v (assq-ref mach 'pat-v))
	 (actn-v (assq-ref mach 'act-v)) ; unknown action vector
	 (xact-v (if (procedure? (vector-ref actn-v 0)) actn-v
		     (vector-map
		      ;; Turn symbolic action into executable procedures:
		      (lambda (ix f) (eval f (current-module)))
		      (vector-map
		       (lambda (ix actn) (wrap-action actn))
		       actn-v))))
	 ;;
	 (dmsg (lambda (s t a) (fmtout "state ~S, token ~S\t=> ~S\n" s t a)))
	 (hashed (number? (caar (vector-ref pat-v 0)))) ; been hashified?
	 ;;(def (assq-ref (assq-ref mach 'mtab) '$default))
	 (def (if hashed -1 '$default))
	 ;; predicate to test for shift action:
	 (shift? (if hashed
		     (lambda (a) (positive? a))
		     (lambda (a) (eq? 'shift (car a)))))
	 ;; On shift, transition to this state:
	 (shift-to (if hashed (lambda (x) x) (lambda (x) (cdr x))))
	 ;; predicate to test for reduce action:
	 (reduce? (if hashed
		      (lambda (a) (negative? a))
		      (lambda (a) (eq? 'reduce (car a)))))
	 ;; On reduce, reduce this production-rule:
	 ;;(reduce-pr (if hashed (lambda (a) (abs a)) (lambda (a) (cdr a))))
	 (reduce-pr (if hashed abs cdr))
	 ;; If no action found in transition list, then this:
	 (parse-error (if hashed #f (cons 'error 0)))
	 ;; predicate to test for error
	 (error? (if hashed
		     (lambda (a) (eq? #f a))
		     (lambda (a) (eq? 'error (car a)))))
	 )
    (lambda* (lexr #:key debug)
      (let iter ((state (list 0))	; state stack
		 (stack (list '$@))	; sval stack
		 (nval #f)		; prev reduce to non-term val
		 (lval #f))		; lexical value (from lex'er)
	(let ((stxl (vector-ref pat-v (car state))))
	  (if (eqv? def (caar stxl))
	      (let* ((stx (cdar stxl))
		     (gx (reduce-pr stx))
		     (gl (vector-ref len-v gx))
		     ($$ (apply (vector-ref xact-v gx) stack)))
		(simple-format #t "auto reduce to ~S\n" (list-ref state gl))
		(iter (list-tail state gl) (list-tail stack gl)
		      (cons (vector-ref rto-v gx) $$) lval))
	      (let* ((laval (or nval (or lval (lexr))))
		     (tval (car laval)) (sval (cdr laval))
		     (stx (or (assq-ref stxl tval)
			      (assq-ref stxl def)
			      parse-error)))
		(if debug (dmsg (car state) (if nval tval sval) stx))
		(cond
		 ((error? stx)
		  (let ((fn (or (port-filename (current-input-port)) "(???)"))
			(ln (1+ (port-line (current-input-port)))))
		    (fmterr "~A:~A: parse failed at state ~A, on input ~S\n"
			    fn ln (car state) sval))
		  #f)
		 ((shift? stx)
		  (iter (cons (shift-to stx) state) (cons sval stack) #f
			;;(if nval lval laval)
			lval
			))
		 ((reduce? stx)
		  (let* ((gx (reduce-pr stx)) (gl (vector-ref len-v gx))
			 ($$ (apply (vector-ref xact-v gx) stack)))
		    (iter (list-tail state gl) 
			  (list-tail stack gl)
			  (cons (vector-ref rto-v gx) $$)
			  ;;(if nval lval laval)
			  lval
			  )))
		 (else ;; accept
		  (car stack))))))))))
  
#;(define* (make-lalr-y-parser mach)
  (let* ((len-v (assq-ref mach 'len-v))
	 (rto-v (assq-ref mach 'rto-v))	; reduce to
	 (pat-v (assq-ref mach 'pat-v))
	 (actn-v (assq-ref mach 'act-v)) ; unknown action vector
	 (xact-v (if (procedure? (vector-ref actn-v 0)) actn-v
		     (vector-map
		      ;; Turn symbolic action into executable procedures:
		      (lambda (ix f) (eval f (current-module)))
		      (vector-map
		       (lambda (ix actn) (wrap-action actn))
		       actn-v))))
	 ;;
	 (dmsg (lambda (s t a) (fmtout "state ~S, token ~S\t=> ~S\n" s t a)))
	 (hashed (number? (caar (vector-ref pat-v 0)))) ; been hashified?
	 ;;(def (assq-ref (assq-ref mach 'mtab) '$default))
	 (def (if hashed -1 '$default))
	 ;; predicate to test for shift action:
	 (shift? (if hashed
		     (lambda (a) (positive? a))
		     (lambda (a) (eq? 'shift (car a)))))
	 ;; On shift, transition to this state:
	 (shift-to (if hashed (lambda (x) x) (lambda (x) (cdr x))))
	 ;; predicate to test for reduce action:
	 (reduce? (if hashed
		      (lambda (a) (negative? a))
		      (lambda (a) (eq? 'reduce (car a)))))
	 ;; On reduce, reduce this production-rule:
	 ;;(reduce-pr (if hashed (lambda (a) (abs a)) (lambda (a) (cdr a))))
	 (reduce-pr (if hashed abs cdr))
	 ;; If no action found in transition list, then this:
	 (parse-error (if hashed #f (cons 'error 0)))
	 ;; predicate to test for error
	 (error? (if hashed
		     (lambda (a) (eq? #f a))
		     (lambda (a) (eq? 'error (car a)))))
	 )
    
    (lambda* (lexr #:key debug)
      ;; state: state stack
      ;; stack: sem-val stack
      ;; nval: previously reduced rule, or #f
      ;; lval: lexical pair (from lex'er) or #f
      
      (define (process state stack nval lval stx)
	(let ((xval (or nval lval))
	      (tval (car lval))
	      (sval (cdr lval)))
	  (cond
	   ((error? stx)
	    (let ((fn (or (port-filename (current-input-port))
			  "(unknown)"))
		  (ln (1+ (port-line (current-input-port)))))
	      (fmterr "~A:~A: parse failed at state ~A, on input ~S\n"
		      fn ln (car state) sval))
	    #f)
	   ((shift? stx)
	    (iter (cons (shift-to stx) state) (cons sval stack) #f lval))
	   ((reduce? stx)
	    (let* ((gx (reduce-pr stx)) (gl (vector-ref len-v gx))
		   ($$ (apply (vector-ref xact-v gx) stack)))
	      (iter (list-tail state gl) 
		    (list-tail stack gl)
		    (cons (vector-ref rto-v gx) $$)
		    lval)))
	   (else ;; accept
	    (car stack)))))

      (define (iter state stack nval lval)
	(let ((stxl (vector-ref pat-v (car state))))
	  (cond
	   ((eqv? def (caar stxl))
	    ;; default reduction; take it
	    (let* ((stx (cdar stxl))
		   (gx (reduce-pr stx))
		   (gl (vector-ref len-v gx))
		   ($$ (apply (vector-ref xact-v gx) stack)))
	      (simple-format #t "auto reduce to ~S\n" (list-ref state gl))
	      (iter (list-tail state gl) (list-tail stack gl)
		    (cons (vector-ref rto-v gx) $$) lval)))
	   (nval
	    (let* ((tval (car nval))
		   (sval (cdr nval))
		   (stx (or (assq-ref stxl tval)
			    (assq-ref stxl def)
			    parse-error)))
	    ;; act on non-terminal
	    (process state stack sval stx)))
	   (else
	    ;; act on terminal
	    (let* ((lval (or lval (lexr)))
		   (tval (car lval))
		   (sval (cdr lval))
		   (stx (or (assq-ref stxl tval)
			    (assq-ref stxl def)
			    parse-error)))
	      (if debug (dmsg (car state) (if nval tval sval) stx))
	      (process state stack sval stx))))))

      (iter (list 0) (list '$@) #f #f))))

;; @end itemize
;;; --- last line ---
