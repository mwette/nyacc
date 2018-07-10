;;; nyacc/parse.scm
;;;
;;; Copyright (C) 2014-2018 Matthew R. Wette
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
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, see <http://www.gnu.org/licenses/>

;; procedures to generate parsers, given a lexical analyzer
;; one for files; one for interactive use: newline is possible end of input

(define-module (nyacc parse)
  #:export (make-lalr-parser
	    make-lalr-ia-parser
	    make-lalr-ia-parser/num)
  #:use-module (nyacc util))
(cond-expand
  (mes)
  (guile-2
   (use-modules (srfi srfi-43)))
  (guile
   (use-modules (ice-9 optargs))
   (use-modules (nyacc compat18)))
  (else))

;; @item (machine-hashed? mach) => #t|#f
;; Indicate if the machine has been hashed.
(define (machine-hashed? mach)
  (number? (caar (vector-ref (assq-ref mach 'pat-v) 0))))

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
(define (dmsg s t a) (fmterr "state ~S, token ~S\t=> ~S\n" s t a))
(define (make-xct av)
  (if (procedure? (vector-ref av 0))
      av
      (vector-map (lambda (ix f) (eval f (current-module)))
		  (vector-map (lambda (ix actn) (wrap-action actn)) av))))

(define* (make-lalr-parser mach)
  (let* ((len-v (assq-ref mach 'len-v))	 ; production RHS length
	 (rto-v (assq-ref mach 'rto-v))	 ; reduce to
	 (pat-v (assq-ref mach 'pat-v))	 ; parse action (shift, reduce) table
	 (actn-v (assq-ref mach 'act-v)) ; symbolic actions
	 (mtab (assq-ref mach 'mtab))
	 (xact-v (if (procedure? (vector-ref actn-v 0)) actn-v
		     (vector-map
		      ;; Turn symbolic action into executable procedures:
		      (lambda (ix f) (eval f (current-module)))
		      (vector-map
		       (lambda (ix actn) (wrap-action actn))
		       actn-v))))
	 ;;
	 (hashed (number? (caar (vector-ref pat-v 0)))) ; been hashified?
	 ;;(def (assq-ref mtab '$default))
	 (def (if hashed -1 '$default))
	 (end (assq-ref mtab '$end))
	 (err (assq-ref mtab '$error))
	 (comm (list (assq-ref mtab '$lone-comm) (assq-ref mtab '$code-comm)))
	 ;; predicate to test for shift action:
	 (shift? (if hashed
		     (lambda (a) (positive? a))
		     (lambda (a) (eq? 'shift (car a)))))
	 ;; On shift, transition to this state:
	 (shift-to (if hashed (lambda (x) x) (lambda (x) (cdr x))))
	 ;; Predicate to test for reduce action:
	 (reduce? (if hashed
		      (lambda (a) (negative? a))
		      (lambda (a) (eq? 'reduce (car a)))))
	 ;; On reduce, reduce this production-rule:
	 (reduce-pr (if hashed abs cdr))
	 ;; If error, make the right packet.
	 (other (if hashed 0 '(other . 0)))
	 )

    (lambda* (lexr #:key debug)
      (let iter ((state (list 0))	; state stack
		 (stack (list '$@))	; sval stack
		 (nval #f)		; prev reduce to non-term val
		 (lval (lexr)))		; lexical value (from lex'er)

	(let* ((tval (car (if nval nval lval))) ; token (syntax value)
	       (sval (cdr (if nval nval lval))) ; semantic value
	       (stxl (vector-ref pat-v (car state))) ; state transition xtra
	       (oact #f) ;; if not shift/reduce, then accept, error or skip
	       (stx (cond ;; state transition
		     ((assq-ref stxl tval)) ; shift/reduce in table
		     ((memq tval comm) (set! oact 'skip) other)
		     ((assq-ref stxl err)) ; error recovery
		     ((assq-ref stxl def))  ; default action
		     (else (set! oact 'error) other))))

	  (if debug (dmsg (car state) (if nval tval sval) stx))
	  (cond
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
	   (else ;; other action: skip, error, or accept
	    (case oact
	      ((skip) (iter state stack nval (lexr)))
	      ((error) (throw 'nyacc-error
			      "parse failed at state ~A, on input ~S"
			      (car state) sval))
	      (else ;; accept
	       (car stack))))))))))


(define (parse-error state laval)
  (let ((fn (or (port-filename (current-input-port)) "(unknown)"))
	(ln (1+ (port-line (current-input-port)))))
    (fmterr "~A:~A: parse failed at state ~A, on input ~S\n"
	    fn ln (car state) (cdr laval)))
  #f)

;; @deffn {Procedure} make-lalr-ia-parser mach
;; Make an interactive parser.   This will automatically process default
;; redunctions if that is the only choice, and does not wait for '$end to
;; return.  This needs algorithm verification.  Makes some assumptions that
;; need to be verified. @*
;; Assume a parser is built to accept a list of expressions.  We are done when
;; the state stack is at zero and the lookahead is a newline.@*
;; Currently hardcoded to look for newline or EOF as end of input.
;; @end deffn
(define* (make-lalr-ia-parser/sym mach)
  (let* ((len-v (assq-ref mach 'len-v))
	 (rto-v (assq-ref mach 'rto-v))
	 (pat-v (assq-ref mach 'pat-v))
	 (xct-v (make-xct (assq-ref mach 'act-v)))
	 (mtab (assq-ref mach 'mtab))
	 (end (assq-ref mtab '$end)))
    (lambda* (lexr #:key debug)
      (let iter ((state (list 0))	; state stack
		 (stack (list '$@))	; sval stack
		 (nval #f)		; prev reduce to non-term val
		 (lval #f))		; lexical value (from lex'er)
	(let ((stxl (vector-ref pat-v (car state))))
	  (cond
	   ((eqv? '$default (caar stxl))
	    (let* ((stx (cdar stxl))
		   (gx (cdr stx))
		   (gl (vector-ref len-v gx))
		   ($$ (apply (vector-ref xct-v gx) stack)))
	      (iter (list-tail state gl) (list-tail stack gl)
		    (cons (vector-ref rto-v gx) $$) lval)))
	   ((and (null? (cdr state)) lval (or (eof-object? (cdr lval))
					      (string=? "\n" (cdr lval))))
	    (cdr nval))
	   (else
	    (let* ((laval (or nval (or lval (lexr))))
		   (tval (car laval)) (sval (cdr laval))
		   (stx (or (assq-ref stxl tval)
			    (assq-ref stxl '$default)
			    (cons 'error 0))))
	      (cond
	       ((eq? 'error (car stx))	; error
		(let ((fn (or (port-filename (current-input-port)) "(unknown)"))
		      (ln (1+ (port-line (current-input-port)))))
		  (fmterr "~A:~A: parse failed at state ~A, on input ~S\n"
			  fn ln (car state) sval))
		#f)
	       ((eq? 'shift (car stx))
		(iter (cons (cdr stx) state) (cons sval stack)
		      #f (if nval lval #f)))
	       ((eq? 'reduce (car stx))
		(let* ((gx (cdr stx)) (gl (vector-ref len-v gx))
		       ($$ (apply (vector-ref xct-v gx) stack)))
		  (iter (list-tail state gl)
			(list-tail stack gl)
			(cons (vector-ref rto-v gx) $$)
			(if nval lval laval)
			)))
	       (else ;; accept
		(car stack)))))))))))

(define (dmsg/n s t a)
  (cond
   ((positive? a) (fmterr "state ~S, token ~S\t=> shift ~S\n" s t a))
   ((negative? a) (fmterr "state ~S, token ~S\t=> reduce ~S\n" s t (- a)))
   ((zero? a) (fmterr "state ~S, token ~S\t=> accept\n" s t))
   (else (error "coding error in (nyacc parse)"))))

(define* (make-lalr-ia-parser/num mach)
  (let* ((len-v (assq-ref mach 'len-v))
	 (rto-v (assq-ref mach 'rto-v))
	 (pat-v (assq-ref mach 'pat-v))
	 (xct-v (make-xct (assq-ref mach 'act-v))))
    (lambda* (lexr #:key debug)
      (let iter ((state (list 0))	; state stack
		 (stack (list '$@))	; sval stack
		 (nval #f)		; prev reduce to non-term val
		 (lval #f))		; lexical value (from lex'er)
	(if (not (or nval lval))
	    (iter state stack nval (lexr)) ; get token
	    (let* ((laval (or nval lval))
		   (tval (car laval)) (sval (cdr laval))
		   (stxl (vector-ref pat-v (car state)))	  
		   (stx (or (assq-ref stxl tval) (assq-ref stxl -1) #f)))
	      (if debug (dmsg/n (car state) (if nval tval sval) stx))
	      (cond
	       ((eq? #f stx)		; error
		(parse-error state laval))
	       ((and lval		; "\n" = $end
		     (not (eq? (car lval) -2))
		     (string=? (cdr lval) "\n")
		     (assq-ref stxl -2))
		(iter state stack nval (cons -2 "\n")))
	       ((negative? stx)		; reduce
		(let* ((gx (abs stx))
		       (gl (vector-ref len-v gx))
		       ($$ (apply (vector-ref xct-v gx) stack)))
		  (iter (list-tail state gl) 
			(list-tail stack gl)
			(cons (vector-ref rto-v gx) $$)
			lval)))
	       ((positive? stx)		; shift
		(iter (cons stx state) (cons sval stack) #f (if nval lval #f)))
	       (else			; accept
		(car stack)))))))))
	       

(define* (make-lalr-ia-parser mach)
  (let* ((len-v (assq-ref mach 'len-v))
	 (rto-v (assq-ref mach 'rto-v))	; reduce to
	 (pat-v (assq-ref mach 'pat-v))
	 (actn-v (assq-ref mach 'act-v)) ; unknown action vector
	 (mtab (assq-ref mach 'mtab))
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
	 (end (assq-ref mtab '$end))
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
	  (cond
	   ((eqv? def (caar stxl))
	    (let* ((stx (cdar stxl))
		   (gx (reduce-pr stx))
		   (gl (vector-ref len-v gx))
		   ($$ (apply (vector-ref xact-v gx) stack)))
              (if debug (fmterr "state ~S, default => reduce ~S, goto ~S\n"
                                (car state) gx (list-ref state gl)))
	      (iter (list-tail state gl) (list-tail stack gl)
		    (cons (vector-ref rto-v gx) $$) lval)))
	   ((eqv? end (caar stxl))	; only '$end remains, return for i/a
            (if debug (fmterr "in state ~S, looking at '$end => accept\n"
			      (car state)))
	    (if (reduce? (cdar stxl))
		;; Assuming this is the final reduction ...
		(apply (vector-ref xact-v (reduce-pr (cdar stxl))) stack)
		;; Or already done ...
		(car stack)))
	   (else
	    (let* ((laval (or nval (or lval (lexr))))
		   (tval (car laval)) (sval (cdr laval))
		   (stx (or (assq-ref stxl tval)
			    (assq-ref stxl def)
			    parse-error)))
	      ;;(if debug (fmterr "  lval=~S  laval=~S\n" lval laval))
	      (if debug (dmsg (car state) (if nval tval sval) stx))
	      (cond
	       ((error? stx)
		(let ((fn (or (port-filename (current-input-port)) "(unknown)"))
		      (ln (1+ (port-line (current-input-port)))))
		  (fmterr "~A:~A: parse failed at state ~A, on input ~S\n"
			  fn ln (car state) sval))
		#f)
	       ((shift? stx)
		(iter (cons (shift-to stx) state) (cons sval stack)
		      #f (if nval lval #f)))
	       ((reduce? stx)
		(let* ((gx (reduce-pr stx)) (gl (vector-ref len-v gx))
		       ($$ (apply (vector-ref xact-v gx) stack)))
		  (iter (list-tail state gl) 
			(list-tail stack gl)
			(cons (vector-ref rto-v gx) $$)
			(if nval lval laval)
			)))
	       (else ;; accept
		(car stack)))))))))))

(define* (NEW-make-lalr-ia-parser mach)
  (if (number? (caar (vector-ref (assq-ref mach 'pat-v) 0)))
      (make-lalr-ia-parser/num mach)	; hashed
      (make-lalr-ia-parser/sym mach)))	; not hashed

;; @end itemize
;;; --- last line ---
