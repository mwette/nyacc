;;; lang/ecmascript/pbody.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
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

;;; This is the javascript runtime.

(define (es-Boolean? v) (or (eq? #t v) (eq? #f v)))
(define (es-Null? v) (eq? 'Null v))

(define *insert-semicolon* (make-fluid))
  
(define (NLT)
  (fluid-set! *insert-semicolon* #f))

(define read-js-ident
  ;; This is incomplete.
  (let* ((if (char-set-union char-set:letter (string->char-set "_$")))
	 (ir (char-set-union if char-set:digit (string->char-set ".+-"))))
    (make-ident-reader if ir)))

(define (read-js-string delim)
  (if (not (or (eq? delim #\') (eq? delim #\"))) #f
      (let iter ((cl '()) (ch (read-char)))
	(cond ((eq? ch #\\)
	       (let ((c1 (read-char)))
		 (if (eq? c1 #\newline)
		     (iter cl (read-char))
		     (iter (cons* c1 cl) (read-char)))))
	      ((eq? ch delim) (list->string (reverse cl)))
	      (else (iter (cons ch cl) (read-char)))))))

(define gen-js-lexer
  (let* ((match-table mtab)
	 (space-cs (string->char-set " \t\r"))
	 ;;
	 (strtab (filter-mt string? match-table)) ; strings in grammar
	 (kwstab (filter-mt like-c-ident? strtab)) ; keyword strings =>
	 (keytab (map-mt string->symbol kwstab)) ; keywords in grammar
	 (chrseq (remove-mt like-c-ident? strtab))  ; character sequences
	 (symtab (filter-mt symbol? match-table)) ; symbols in grammar
	 (chrtab (filter-mt char? match-table))	; characters in grammar
	 ;;
	 (read-chseq (make-chseq-reader chrseq))
	 (semicolon (assoc-ref chrseq ";"))
	 (assc-$ (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair))))
	 )
    (lambda ()
      (let ((bol #t))
	(lambda ()
	  (let iter ((ch (read-char)))
	    (cond
	     ((eof-object? ch) (assc-$ (cons '$end ch)))
	     ((char-set-contains? space-cs ch) (iter (read-char)))
	     ((eqv? ch #\newline)
	      (if (fluid-ref *insert-semicolon*)
		  (cons semicolon ";")
		  (iter (read-char))))
	     ((and (eqv? ch #\newline) (set! bol #t) #f))
	     ((read-js-string ch) assc-$)
	     ((read-c-comm ch) (iter (read-char)))
	     ((read-js-ident ch) =>
	      (lambda (s)
		(or (and=> (assq-ref keytab (string->symbol s))
			   (lambda (tval) (cons tval s)))
		    (assc-$ (cons '$ident s)))))
	     ((read-c-num ch) => assc-$)
	     ((read-chseq ch) => identity)
	     ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
	     (else (cons ch (string ch)))))))))) ; should be error
 
(define parser
  (let* ((dmsg (lambda (s t a)
		 (simple-format #t "state ~S, token ~S\t=> ~S\n" s t a)))
	 ;; predicate to test for shift action:
	 (shift? (lambda (a) (positive? a)))
	 (shift-to (lambda (x) x))
	 (reduce? (lambda (a) (negative? a)))
	 (reduce-pr abs)
	 (error? (lambda (a) (eq? #f a)))
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
			(assq-ref stxl -1)  ; default action
			#f)))
	  (when debug (dmsg (car state) (if nval tval sval) stx))
	  (cond
	   ((error? stx)
	    ;; Ugly to have to check this first every time, but
	    ;; @code{positive?} and @code{negative?} fail otherwise.
	    (let ((fn (or (port-filename (current-input-port)) "(unknown)"))
		  (ln (1+ (port-line (current-input-port)))))
	      (fmterr "~A: ~A: parse failed at state ~A, on input ~S\n"
		      fn ln (car state) sval))
	    #f)
	   ((shift? stx)
	    (iter (cons (shift-to stx) state) (cons sval stack)
		  #f (if nval lval (lexr))))
	   ((reduce? stx)
	    (let* ((gx (reduce-pr stx)) (gl (vector-ref len-v gx))
		   ($$ (apply (vector-ref act-v gx) stack)))
	      (iter (list-tail state gl) 
		    (list-tail stack gl)
		    (cons (vector-ref rto-v gx) $$)
		    lval)))
	   (else ;; accept
	    (car stack))))))))

(define (parse-js)
  (with-fluid* *insert-semicolon* #t
	       (lambda () (parser (gen-js-lexer) #:debug #f))))

;;; --- last line
