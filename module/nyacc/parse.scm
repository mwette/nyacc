;;; nyacc/parse.scm

;; Copyright (C) 2014-2021 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Description:

;; procedures to generate parsers, given a lexical analyzer
;; one for files; one for interactive use: newline is possible end of input

;;; Code:

(define-module (nyacc parse)
  #:export (make-lalr-parser
	    make-lalr-parser/sym
	    make-lalr-parser/num))

(define $default 1)			; sync w/ lalr.scm
(define $error 2)			; sync w/ lalr.scm

(use-modules (ice-9 pretty-print))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))

(define (vector-map proc vec)		; see (srfi srfi-43)
  (let* ((ln (vector-length vec)) (res (make-vector ln)))
    (let loop ((ix 0))
      (unless (= ix ln)
	(vector-set! res ix (proc ix (vector-ref vec ix)))
	(loop (1+ ix))))
    res))

(define (wrap-action actn)		; see util.scm
  (define (mkarg i) (string->symbol (string-append "$" (number->string i))))
  (define (make-arg-list n) (let loop ((r '(. $rest)) (i 1))
			      (if (> i n) r (loop (cons (mkarg i) r) (1+ i)))))
  (cons* 'lambda (make-arg-list (car actn)) (cdr actn)))

(define (make-xct av env)
  (if (procedure? (vector-ref av 0))
      av
      (vector-map (lambda (ix f)
		    (eval f (or env (current-module))))
		  (vector-map (lambda (ix actn) (wrap-action actn)) av))))

(define (dmsg/n s t a ntab)
  (let ((t (or (assq-ref ntab t) t)))
    (cond
     ((not a) (sferr "state ~S, token ~S\t=> parse error\n" s t))
     ((positive? a) (sferr "state ~S, token ~S  => shift, goto ~S\n" s t a))
     ((negative? a) (sferr "state ~S, token ~S  => reduce ~S\n" s t (- a)))
     ((zero? a) (sferr "state ~S, token ~S  => accept\n" s t))
     (else (error "coding error in (nyacc parse)")))))

(define (dmsg/s s t a)
  (case (car a)
    ((error) (sferr "state ~S, token ~S  => parse error\n" s t))
    ((shift) (sferr "state ~S, token ~S  => shift, goto ~S\n" s t (cdr a)))
    ((reduce) (sferr "state ~S, token ~S  => reduce ~S\n" s t (cdr a)))
    ((accept) (sferr "state ~S, token ~S  => accept\n" s t))
    (else (error "coding error in (nyacc parse)"))))

(define (parse-error state laval)
  (let* ((sp (source-properties laval))
         (fn (or (assq-ref sp 'filename)
                 (port-filename (current-input-port))
                 "(unknown)"))
	 (ln (1+ (or (assq-ref sp 'line)
                     (port-line (current-input-port))))))
    (throw 'nyacc-error
	   "~A:~A: parse failed at state ~A, on input ~S"
	   fn ln (car state) (cdr laval))))

(define* (make-lalr-parser/sym mach #:key (skip-if-unexp '()) interactive env)
  (let* ((len-v (assq-ref mach 'len-v))
	 (rto-v (assq-ref mach 'rto-v))
	 (pat-v (assq-ref mach 'pat-v))
	 (xct-v (make-xct (assq-ref mach 'act-v) env))
	 (start (assq-ref (assq-ref mach 'mtab) '$start)))
    (lambda* (lexr #:key debug)
      (let loop ((state (list 0))	; state stack
		 (stack (list '$@))	; semantic value stack
		 (nval #f)		; non-terminal from prev reduction
		 (lval #f))		; lexical value (from lex'er)
	(cond
	 ((not (or nval lval))
	  (if (eqv? '$default (caar (vector-ref pat-v (car state))))
	      (loop state stack (cons-source stack '$default #f) lval)
	      (loop state stack nval (lexr)))) ; reload
	 ((and (zero? (car state))
               interactive nval 
	       (eqv? (car nval) start)) ; done
	  (cdr nval))
	 (else
	  (let* ((laval (or nval lval))
		 (tval (car laval))
		 (sval (cdr laval))
		 (stxl (vector-ref pat-v (car state)))
		 (stx (or (assq-ref stxl tval)
			  (and (not (memq tval skip-if-unexp))
			       (assq-ref stxl '$default))
			  (cons 'error #f))))
	    (if debug (dmsg/s (car state) (if nval tval sval) stx))
	    (cond
	     ((eq? 'error (car stx))	; error ???
	      (if (memq tval skip-if-unexp)
		  (loop state stack #f #f)
		  (parse-error state laval)))
	     ((eq? 'reduce (car stx))	; reduce
	      (let* ((gx (cdr stx))
		     (gl (vector-ref len-v gx))
		     ($$ (apply (vector-ref xct-v gx) stack))
		     (pobj (if (zero? gl) laval (list-tail stack (1- gl))))
		     (pval (source-properties pobj))
		     (tval (cons-source pobj (vector-ref rto-v gx) $$)))
		(if (supports-source-properties? $$)
		    (set-source-properties! $$ pval))
		(loop (list-tail state gl) (list-tail stack gl) tval lval)))
	     ((eq? 'shift (car stx))	; shift
	      (loop (cons (cdr stx) state) (cons-source laval sval stack)
		    #f (if nval lval #f)))
	     (else			; accept
	      (car stack))))))))))

;; source property strategy:
;; lexemes (i.e., token-value pairs) have source properties
;; as the values are shifted onto @var{stack} the source properties
;; are attached to the cons cells connecting the elements.
;; when a reduction occurs, the srcprop for the left-most reduced
;; value is attached to the return so
;;    $n $n-1 ... $1 $0 $-1 $-2 ...
;; red
;;    $$ | $0 $-1 $-2 ...
;; (src-prop $1) applied to $$

(define* (make-lalr-parser/num mach #:key (skip-if-unexp '()) interactive env)
  (let* ((len-v (assq-ref mach 'len-v))
	 (rto-v (assq-ref mach 'rto-v))
	 (pat-v (assq-ref mach 'pat-v))
	 (xct-v (make-xct (assq-ref mach 'act-v) env))
	 (ntab (assq-ref mach 'ntab))
	 (start (assq-ref (assq-ref mach 'mtab) '$start)))
    (lambda* (lexr #:key debug)
      (let loop ((state (list 0))	; state stack
		 (stack (list '$@))	; semantic value stack
		 (nval #f)		; non-terminal from prev reduction
		 (lval #f))		; lexical value (from lex'r)
	(cond
	 ((not (or nval lval))
	  (if (eqv? $default (caar (vector-ref pat-v (car state))))
	      (loop state stack (cons-source stack $default #f) lval)
	      (loop state stack nval (lexr)))) ; reload
	 ((and (zero? (car state))
               interactive nval
	       (eqv? (car nval) start)) ; done
	  (cdr nval))
	 (else
	  (let* ((laval (or nval lval))
		 (tval (car laval))
		 (sval (cdr laval))
		 (stxl (vector-ref pat-v (car state)))
		 (stx (or (assq-ref stxl tval)
			  (and (not (memq tval skip-if-unexp))
			       (assq-ref stxl $default))
			  #f)))		; error
	    (if debug (dmsg/n (car state) (if nval tval sval) stx ntab))
	    (cond
	     ((eq? #f stx)		; error
	      (if (memq tval skip-if-unexp)
		  (loop state stack #f #f)
		  (parse-error state laval)))
	     ((negative? stx)		; reduce
	      (let* ((gx (abs stx))
		     (gl (vector-ref len-v gx))
		     ($$ (apply (vector-ref xct-v gx) stack))
		     (pobj (if (zero? gl) laval (list-tail stack (1- gl))))
		     (pval (source-properties pobj))
		     (tval (cons-source pobj (vector-ref rto-v gx) $$)))
		(if (supports-source-properties? $$)
		    (set-source-properties! $$ pval))
		(loop (list-tail state gl) (list-tail stack gl) tval lval)))
	     ((positive? stx)		; shift
	      (loop (cons stx state) (cons-source laval sval stack) 
		    #f (if nval lval #f)))
	     (else			; accept
	      (car stack))))))))))

;; @deffn {Procedure} make-lalr-parser mach [options] => parser
;; Generate a procedure for parsing a language, where @var{mach} is
;; a machine generated by @code{make-lalr-machine}.
;; This generates a procedure that takes one argument, a lexical analyzer:
;; @example
;; (parser lexical-analyzer #:debug #t)
;; @end example
;; @noindent
;; and is used as
;; @example
;; (define xyz-parse (make-lalr-parser xyz-mach))
;; (with-input-from-file "sourcefile.xyz"
;;   (lambda () (xyz-parse (gen-lexer))))
;; @end example
;; @noindent
;; The generated parser is reentrant.  Options are:
;; @table @code
;; @item #:skip-if-unexp 
;; This is a list of tokens to skip if not expected.  It is used
;; to allow comments to be skipped.  The default is @code{'()}.
;; @item #:interactive
;; If @code{#t}, this tells the parser that this is being called
;; interactively, so that the token @code{$end} is not expected.
;; The default value is @code{#f}.
;; @item #:env
;; Use the passed environment to the parser actions provided in the
;; specification.  This can be a module (e.g., @code{(current-module)}
;; or an environment (e.g., @code{(interaction-environment)}).   The
;; default is to use @code{(current-module)}; that is, the module in which
;; this procedure is being executed.
;; @end table
;; @noindent
;; @end deffn
(define* (make-lalr-parser mach
			   #:key
			   (skip-if-unexp '())
			   interactive
			   env)
  "- Procedure: make-lalr-parser mach [options] => parser
     Generate a procedure for parsing a language, where MACH is a
     machine generated by 'make-lalr-machine'.  This generates a
     procedure that takes one argument, a lexical analyzer:
          (parser lexical-analyzer #:debug #t)
     and is used as
          (define xyz-parse (make-lalr-parser xyz-mach))
          (with-input-from-file \"sourcefile.xyz\"
            (lambda () (xyz-parse (gen-lexer))))
     The generated parser is reentrant.  Options are:
     '#:skip-if-unexp'
          This is a list of tokens to skip if not expected.  It is used
          to allow comments to be skipped.  The default is ''()'.
     '#:interactive'
          If '#t', this tells the parser that this is being called
          interactively, so that the token '$end' is not expected.  The
          default value is '#f'.
     '#:env'
          Use the passed environment to the parser actions provided in
          the specification.  This can be a module (e.g.,
          '(current-module)' or an environment (e.g.,
          '(interaction-environment)').  The default is to use
          '(current-module)'; that is, the module in which this
          procedure is being executed."

  ;; BUG: this should fail if rrconf exists.

  (let* ((mtab (assq-ref mach 'mtab))
	 (siu (map (lambda (n) (assoc-ref mtab n)) skip-if-unexp)))
    (if (number? (caar (vector-ref (assq-ref mach 'pat-v) 0)))
	;; hashed:
	(make-lalr-parser/num mach
			      #:skip-if-unexp siu
			      #:interactive interactive
			      #:env env)
	;; not hashed:
	(make-lalr-parser/sym mach
			      #:skip-if-unexp siu
			      #:interactive interactive
			      #:env env))))

;;; --- last line ---
