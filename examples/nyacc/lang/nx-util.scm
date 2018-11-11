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

;;; Notes:

;; 1) should make a (make-return expr)

;;; Code:

(define-module (nyacc lang nx-util)
  #:export (genxsym
	    nx-undefined-xtil
	    nx-push-scope nx-pop-scope nx-top-level?
	    nx-add-toplevel nx-add-lexical nx-add-lexicals nx-add-symbol
	    nx-lookup-in-env nx-lookup
	    rtail singleton?
	    make-and make-or make-thunk make-defonce
	    with-escape/handler with-escape/arg with-escape/expr with-escape
	    rev/repl
	    make-handler
	    opcall-generator
	    block vblock
	    make-loop make-do-while make-while lookup-gensym
	    )
  )

(define (genxsym name)
  (gensym (string-append (if (string? name) name (symbol->string name)) "-")))

(define nx-undefined-xtil `(const ,(if #f #f)))

;; @deffn {Procedure} nx-push-scope dict
;; Push scope level of dict, returning new dict.
;; @end deffn
(define (nx-push-scope dict)
  "- Procedure: nx-push-scope dict
     Push scope level of dict, returning new dict."
  (list (cons '@P dict)))

;; @deffn {Procedure} nx-push-scope dict
;; Pop scope level of dictionary @var{dict}, returning dictionary
;; for popped scope.
;; @end deffn
(define (nx-pop-scope dict)
  "- Procedure: nx-push-scope dict
     Pop scope level of dictionary DICT, returning dictionary for popped
     scope."
  (or (assq-ref dict '@P) (error "coding error: too many pops")))

;; @deffn {Procedure} nx-top-level? dict
;; This is a predicate to indicate if @var{dict}'s scope top-level.
;; for popped scope.
;; @end deffn
(define (nx-top-level? dict)
  "- Procedure: nx-top-level? dict
     This is a predicate to indicate if DICT's scope top-level.  for
     popped scope."
  (assoc-ref dict '@top))

;; @deffn {Procedure} nx-add-toplevel name dict
;; Given a string @var{name} and dictionary @var{dict} return a new
;; dictionary with a top-level reference for name added.  This can be
;; retrieved with @code{nx-lookup name dict} where @code{dict} is the
;; return value.
;; @example
;; (let ((dict (nx-add-toplevel "foo" dict)))
;;    (nx-lookup "foo" dict)) => (toplevel foo)
;; @end example
;; @end deffn
(define (nx-add-toplevel name dict)
  "- Procedure: nx-add-toplevel name dict
     Given a string NAME and dictionary DICT return a new dictionary
     with a top-level reference for name added.  This can be retrieved
     with 'nx-lookup name dict' where 'dict' is the return value.
          (let ((dict (nx-add-toplevel \"foo\" dict)))
             (nx-lookup \"foo\" dict)) => (toplevel foo)"
  (acons name `(toplevel ,(string->symbol name)) dict))

;; @deffn {Procedure} nx-add-lexical name dict
;; Given a string @var{name} and dictionary @var{dict} return a new
;; dictionary with a lexical reference added.  The reference can be
;; retrieved with @code{nx-lookup name dict} where @code{dict} is the
;; return value.
;; @example
;; (let ((dict (nx-add-lexical "foo" dict)))
;;    (nx-lookup "foo" dict)) => (lexical foo foo-123)
;; @end example
;; @end deffn
(define (nx-add-lexical name dict)
  "- Procedure: nx-add-lexical name dict
     Given a string NAME and dictionary DICT return a new dictionary
     with a lexical reference added.  The reference can be retrieved
     with 'nx-lookup name dict' where 'dict' is the return value.
          (let ((dict (nx-add-lexical \"foo\" dict)))
             (nx-lookup \"foo\" dict)) => (lexical foo foo-123)"
  (acons name `(lexical ,(string->symbol name) ,(genxsym name)) dict))

;; @deffn {Procedure} nx-add-lexicals name1 ... nameN dict
;; A fold-right with @code{nx-add-lexical}, equivalent to
;; @example
;; (fold-right nx-add-lexical dict (name1 ... nameN))
;; @end example
;; @end deffn
(define (nx-add-lexicals . args)
  "- Procedure: nx-add-lexicals name1 ... nameN dict
     A fold-right with 'nx-add-lexical', equivalent to
          (fold-right nx-add-lexical dict (name1 ... nameN))"
  (let iter ((args args))
    (if (null? (cddr args)) (nx-add-lexical (car args) (cadr args))
	(nx-add-lexical (car args) (iter (cdr args))))))

;; Add lexical or toplevel based on level.
(define (nx-add-symbol name dict)
  (if (nx-top-level? dict)
      (nx-add-toplevel name dict)
      (nx-add-lexical name dict)))

(define (nx-lookup-in-env name env)
  (let ((sym (if (string? name) (string->symbol name) name)))
    (if (and env (module-variable env sym))
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

;; @deffn {Procedure} make-thunk expr [#:name name] [#:lang lang]
;; Generate a thunk @code{`(lambda ...)}.
;; @end deffn
(define* (make-thunk expr #:key name lang)
  (let* ((meta '())
	 (meta (if lang (cons `(language . ,lang) meta) meta))
	 (meta (if name (cons `(name . ,name) meta) meta)))
    `(lambda ,meta (lambda-case ((() #f #f #f () ()) ,expr)))))

;; @deffn {Procedure} make-defonce name value
;; Generate a TIL expression that will ensure the toplevel name is defined.
;; If a define needs to be issues the value is @code{(void)}.  Generates
;; @example
;; (if (defined? 'a) undefined (define a undefined))
;; @end example
;; @noindent
;; where @code{undefined} is like @code{(if #f #f)}.
;; @end deffn
(define (make-defonce symbol value)
  `(define ,symbol
	 (if (call (toplevel module-variable)
		   (call (toplevel current-module))
		   (const ,symbol))
	     (toplevel ,symbol)
	     ,value)))

;; === Using Prompts 

;; @deffn {Procedure} make-handler args body
;; Generate an escape @code{lambda} for a prompt.  The continuation arg
;; is not used.  @var{args} is a list of lexical references and @var{body}
;; is an expression that may reference the args.
;; @end deffn
(define (make-handler args body)
  "- Procedure: make-handler args body
     Generate an escape 'lambda' for a prompt.  The continuation arg is
     not used.  ARGS is a list of lexical references and BODY is an
     expression that may reference the args."
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
;; @deffnx {Procedure} with-escape/arg tag-ref body
;; @deffnx {Procedure} with-escape/expr tag-ref body
;; This is used to generate return and break where break is passed '(void).
;; @var{tag-ref} is of the form @code{(lexical name gensym)} and
;; @var{expr} is an expression.
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
  "- Procedure: vblock expr-list => (seq ex1 (seq ... (void)))
     Return an expression or build a seq-train returning undefined."
  (let iter ((xl expr-list))
    (if (null? xl) '(void)
	`(seq ,(car xl) ,(iter (cdr xl))))))

;; @deffn {Procedure} lookup-gensym name dict [label] => gensym
;; lookup up nearest parent lexical and return gensym
;; (lookup-gensym "foo" dict) => JS~1234
;; (lookup-gensym "foo" dict #:label "oloop") => JS~432
;; @end deffn
(define* (lookup-gensym name dict #:key label)
  "- Procedure: lookup-gensym name dict [label] => gensym
     lookup up nearest parent lexical and return gensym (lookup-gensym
     \"foo\" dict) => JS~1234 (lookup-gensym \"foo\" dict #:label \"oloop\")
     => JS~432"
  (if label
      (let iter ((cdict dict) (pdict (assoc-ref dict '@P)))
	(if (not pdict) #f
	    (if (and (assoc-ref pdict label)
		     (assoc-ref "~exit" cdict))
		(assoc-ref name cdict)
		(iter pdict (assoc-ref pdict '@P)))))
      (let* ((sym (nx-lookup name dict)))
	(if (not sym) (error "javascript: not found:" name))
	(caddr sym))))

;; @deffn {Procedure} make-loop expr body dict ilsym tbody
;; This is a helper procedure for building loops like the following:
;; @example
;; "do" body "where" expr
;; "while" body "do" expr
;; "for" i "in" range "do" body
;; @end example
;; @noindent
;; The argument @var{expr} is the conditional, @var{body} is the code to
;; execute, which may contain @code{abort-to-prompt} given by @code{break}
;; or @code{continue}.
;; The code generated is based on the following pattern:
;; @example
;; (let ((break! (make-prompt-tag 'break))
;;       (continue! (make-prompt-tag 'continue)))
;;    (letrec ((iloop (lambda () (body) (if (expr) (iloop))))
;;             (oloop
;;              (lambda ()
;;               (call-with-prompt continue!
;;                  thunk
;;                  (lambda (k) (if (expr) (oloop)))))))
;;      (call-with-prompt break!
;;        oloop
;;        (lambda (k) (if #f #f))))))
;; @end example
;; @noindent
;; where @code{break!} and @code{continue!} are lexicals generated for
;; the code and @code{thunk} is @*
;; @code{(lambda () (iloop))} for do-while and @*
;; @code{(lambda () (if (expr) (iloop)))} for while-do.
;; @end deffn
;; TODO #:key (break "break") (continue "continue")
(define* (make-loop expr body dict ilsym tbody)
  "- Procedure: make-loop expr body dict ilsym tbody
     This is a helper procedure for building loops like the following:
          \"do\" body \"where\" expr
          \"while\" body \"do\" expr
          \"for\" i \"in\" range \"do\" body
     The argument EXPR is the conditional, BODY is the code to execute,
     which may contain 'abort-to-prompt' given by 'break' or 'continue'.
     The code generated is based on the following pattern:
          (let ((break! (make-prompt-tag 'break))
                (continue! (make-prompt-tag 'continue)))
             (letrec ((iloop (lambda () (body) (if (expr) (iloop))))
                      (oloop
                       (lambda ()
                        (call-with-prompt continue!
                           thunk
                           (lambda (k) (if (expr) (oloop)))))))
               (call-with-prompt break!
                 oloop
                 (lambda (k) (if #f #f))))))
     where 'break!' and 'continue!' are lexicals generated for the code
     and 'thunk' is
     '(lambda () (iloop))' for do-while and
     '(lambda () (if (expr) (iloop)))' for while-do."
  (let* ((olsym (genxsym "oloop"))
	 (bsym (lookup-gensym "break" dict))
	 (csym (lookup-gensym "continue" dict))
	 (icall `(call (lexical iloop ,ilsym)))
	 (ocall `(call (lexical oloop ,olsym)))
	 (iloop (make-thunk `(seq ,body (if ,expr ,icall (void))) #:name 'iloop))
  	 (ohdlr `(lambda ()
		   (lambda-case (((k) #f #f #f () (,(genxsym "k")))
				 (if ,expr ,ocall (void))))))
	 (oloop (make-thunk `(prompt #t (lexical continue ,csym) ,tbody ,ohdlr)
			    #:name 'oloop))
 	 (hdlr `(lambda ()
		  (lambda-case (((k) #f #f #f () (,(genxsym "k"))) (void))))))
    `(let (break continue) (,bsym ,csym)
	  ((primcall make-prompt-tag (const break))
	   (primcall make-prompt-tag (const continue)))
	  (letrec (iloop oloop) (,ilsym ,olsym) (,iloop ,oloop)
		  (prompt #t (lexical break ,bsym) ,ocall ,hdlr)))))

;; @deffn {Procedure} make-do-while expr body dict
;; This generates code for do-while loops where @var{expr} is the condtional
;; expression, @var{body} is the body, @var{dict} is the scope dictionary
;; which must contain the labels for @code{break} and @code{continue}.
;; @end deffn
(define (make-do-while expr body dict)
  "- Procedure: make-do-while expr body dict
     This generates code for do-while loops where EXPR is the condtional
     expression, BODY is the body, DICT is the scope dictionary which
     must contain the labels for 'break' and 'continue'."
  (let ((ilsym (genxsym "iloop")))
    (make-loop expr body dict ilsym `(call (lexical iloop ,ilsym)))))

;; @deffn {Procedure} make-while expr body dict
;; This generates code for the following source:
;; where @var{expr} is the condtional expression, @var{body} is the body,
;; and is the scope dictionary which must contain the labels for
;; @code{break} and @code{continue}.
;; @end deffn
(define (make-while expr body dict)
  "- Procedure: make-while expr body dict
     This generates code for the following source: where EXPR is the
     condtional expression, BODY is the body, and is the scope
     dictionary which must contain the labels for 'break' and
     'continue'."
  (let ((ilsym (genxsym "iloop")))
    (make-loop expr body dict ilsym
		    `(if ,expr (call (lexical iloop ,ilsym)) (void)))))

;; --- last line ---
