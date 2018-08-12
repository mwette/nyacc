;;; nyacc/lang/nx-util.scm - utilities for Guile extension languages

;; Copyright (C) 2018 Matthew R. Wette
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

(define-module (nyacc lang nx-util)
  #:export (genxsym
	    nx-push-scope nx-pop-scope nx-top-level?
	    nx-add-toplevel nx-add-lexical nx-add-lexicals nx-add-symbol
	    nx-lookup-in-env nx-lookup
	    rtail singleton?
	    make-and make-or make-thunk
	    with-escape/handler with-escape/arg with-escape/expr with-escape
	    rev/repl
	    opcall-generator
	    block vblock
	    )
  )

(define (genxsym name) (gensym (string-append name "-")))

;; push/pop scope level
(define (nx-push-scope dict)
  (list (cons '@P dict)))
(define (nx-pop-scope dict)
  (or (assq-ref dict '@P) (error "coding error: too many pops")))
(define (nx-top-level? dict)
  (assoc-ref dict '@top))

;; Add toplevel to dict, return dict
(define (nx-add-toplevel name dict)
  (acons name `(toplevel ,(string->symbol name)) dict))

;; Add lexical to dict, return dict
(define (nx-add-lexical name dict)
  (acons name `(lexical ,(string->symbol name) ,(genxsym name)) dict))

;; (add-lexicals name1 name2 ... dict) 
(define (nx-add-lexicals . args)
  (let iter ((args args))
    (if (null? (cddr args)) (nx-add-lexical (car args) (cadr args))
	(nx-add-lexical (car args) (iter (cdr args))))))

;; Add lexical or toplevel based on level.
(define (nx-add-symbol name dict)
  (if (nx-top-level? dict)
      (nx-add-toplevel name dict)
      (nx-add-lexical name dict)))

(define (nx-lookup-in-env name env)
  (let ((sym (string->symbol name)))
    (if (module-variable env sym)
	`(@@ ,(module-name env) ,sym)
	#f)))

;; @deffn {Procedure} x_y->x-y a_string => a-string
;; Convert a C-like name to a Scheme-like name.
;; @end deffn
(define (x_y->x-y name)
  (string-map (lambda (ch) (if (char=? ch #\_) #\- ch)) name))

(define (nx-lookup name dict)
    (cond
   ((not dict) #f)
   ((null? dict) #f)
   ((assoc-ref dict name))		; => value
   ((assoc-ref dict '@P) =>		; parent level
    (lambda (dict) (nx-lookup name dict)))
   ((nx-lookup-in-env name (assoc-ref dict '@M)))
   ((nx-lookup-in-env (x_y->x-y name) (assoc-ref dict '@M)))
   (else #f)))

(define (rtail kseed)
  (cdr (reverse kseed)))

(define (singleton? expr)
  (and (pair? expr) (null? (cdr expr))))

;; (and a b c) => (if a (if b (if c #t #f) #f) #f)
(define (make-and . args)
  (let iter ((args args))
    (if (null? args) '(const #t)
	`(if ,(car args) ,(iter (cdr args)) (const #f)))))

;; (or a b c) => (if a #t (if b #t (if c #t #f)))
(define (make-or . args)
  (let iter ((args args))
    (if (null? args) '(const #f)
	`(if ,(car args) (const #t) ,(iter (cdr args))))))

;; reverse list but replace new head with @code{head}
;; @example
;; (rev/repl 'a '(4 3 2 1)) => '(a 2 3 4)
;; @end example
(define rev/repl
  (case-lambda
   ((arg0 revl)
    (let iter ((res '()) (inp revl))
      (if (null? (cdr inp)) (cons arg0 res)
          (iter (cons (car inp) res) (cdr inp)))))
   ((arg0 arg1 revl)
    (let iter ((res '()) (inp revl))
      (if (null? (cdr inp)) (cons* arg0 arg1 res)
          (iter (cons (car inp) res) (cdr inp)))))
   ))

(define (opcall-generator xlib)
  (define (xlib-ref name) `(@@ ,xlib ,name))
  (lambda (op seed kseed kdict)
    (values (cons (rev/repl 'call (xlib-ref op) kseed) seed) kdict)))

;; @deffn {Procedure} make-thunk expr => `(lambda ...)
;; Generate a thunk.
;; @end deffn
(define* (make-thunk expr #:key name)
  `(lambda ,(if name `((name . ,name)) '())
     (lambda-case ((() #f #f #f () ()) ,expr))))

;; === Using Prompts 

;; @deffn {Procedure} make-handler args body
;; Generate an escape @code{lambda} for a prompt.  The continuation arg
;; is not used.  @var{args} is a list of lexical references and @var{body}
;; is an expression that may reference the args.
;; @end deffn
(define (make-handler args body)
  (call-with-values
      (lambda ()
	(let iter ((names '()) (gsyms '()) (args args))
	  (if (null? args)
	      (values (reverse names) (reverse gsyms))
	      (iter (cons (cadar args) names)
		    (cons (caddar args) gsyms)
		    (cdr args)))))
    (lambda (names gsyms)
      `(lambda ()
	 (lambda-case ((,(cons 'k names) #f #f #f () ,(cons (genxsym "k") gsyms))
		       ,body))))))
	 
;; @deffn {Procedure} with-escape tag-ref body
;; @deffx {Procedure} with-escape/arg tag-ref body
;; @deffx {Procedure} with-escape/expr tag-ref body
;; use for return and break where break is passed '(void)
;; tag-ref is of the form (lexical name gensym)
;; @var{expr} is a fixed expression 
;; @end deffn
(define (with-escape/handler tag-ref body hdlr)
  (let ((tag-name (cadr tag-ref))
	(tag-gsym (caddr tag-ref)))
    `(let (,tag-name) (,tag-gsym) ((primcall make-prompt-tag (const ,tag-name)))
	  (prompt #t ,tag-ref ,body ,hdlr))))
  
(define (with-escape/arg tag-ref body)
  (let ((arg-gsym (genxsym "arg")))
    (with-escape/handler
     tag-ref body
     `(lambda ()
	(lambda-case (((k arg) #f #f #f () (,(genxsym "k") ,arg-gsym))
		      (lexical arg ,arg-gsym)))))))

(define (with-escape/expr tag-ref body expr)
  (with-escape/handler
   tag-ref body
   `(lambda () (lambda-case (((k) #f #f #f () (,(genxsym "k"))) ,expr)))))

(define (with-escape tag-ref body)
  (with-escape/expr tag-ref body '(void)))

;; @deffn {Procedure} block expr-or-expr-list => expr | (seq ex1 (seq ... exN))
;; Return an expression or build a seq-train returning last expression.
;; @end deffn
(define (block expr-or-expr-list)
  (if (pair? (car expr-or-expr-list))
      ;; expr list
      (let iter ((xl expr-or-expr-list))
	(if (null? (cdr xl)) (car xl)
	    `(seq ,(car xl) ,(iter (cdr xl)))))
      expr-or-expr-list))

;; @deffn {Procedure} vblock expr-list => (seq ex1 (seq ... (void)))
;; Return an expression or build a seq-train returning undefined.
;; @end deffn
(define (vblock expr-list)
  (let iter ((xl expr-list))
    (if (null? xl) '(void)
	`(seq ,(car xl) ,(iter (cdr xl))))))

;; --- last line ---
