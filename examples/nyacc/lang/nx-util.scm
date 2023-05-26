;;; nyacc/lang/nx-util.scm - utilities for Guile extension languages

;; Copyright (C) 2018,2021,2023 Matthew Wette
;;
;; This library is free software; you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License
;; for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Notes:

;; 1) should make a (make-return expr)
;; 2) The dict is an alist of symbols with scope levels.  Within each scope
;;    The key @code{@P} points to the parent scope.   If a scope has a key
;;    @code{F} then it is a frame????
;;    Some scope levels can be frames.  If a variable is defined it should
;;    be placed in the current frame.  
;;    @code{@P} points to the parent scope
;;    @code{@F} denotes a frame ????
;;    If code to define a variable it should be put in the current frame.
;; 3) routines that return dict's should return '() if not found
;;    e.g. nx-parent dict => '()
;; 4) stanard prompt tags: return break (exit ?)


;;    entries are of the form
;;      ("foo" . '(lexical foo foo-123))
;;    or
;;      ("foo" . '(toplevel foo foo-123))
;;            
;;                   ((b . a~1) (@F . "foo") (a . b~1) (@P . ^))
;;  ((c . c~1) (@P . ^))
;;
;;  used like this 
;;    [local] -> @F -> [return] -> @P -> [global]
;;
;;  If a scope includes a @F symbol then it is a frame.
;;  Sometimes new symbols must be inserted into a "frame" a scope associated
;;  with a procedure, for example.   The symbols after '@F are the calling
;;  arguments or special arguments, so xxx
;; 
;;; Code:

(define-module (nyacc lang nx-util)
  #:export (genxsym
	    nx-undefined-xtil
	    nx-push-scope nx-pop-scope nx-top-level?
	    nx-add-lexical nx-add-framelevel nx-add-toplevel
	    nx-add-lexicals nx-add-variable

            nx-lookup
	    nx-lookup-in-frame nx-lookup-in-scope
	    nx-lookup-in-env
	    nx-ensure-variable nx-ensure-variable/scope
            nx-lookup-gensym
	    
	    rtail singleton?
	    rev/repl
	    
	    with-escape/handler with-escape/arg with-escape/expr with-escape
	    make-handler

	    make-+SP
	    ;;opcall-generator

	    wrap-locals
	    block vblock
	    make-arity
	    make-and make-or make-thunk make-defonce
	    make-switch make-loop make-do-while make-while make-for
	    ;; deprecated
	    nx-add-symbol)
  #:use-module ((srfi srfi-1) #:select (fold append-reverse)))

(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(use-modules (ice-9 pretty-print))
(define (pperr exp) (pretty-print exp #:per-line-prefix "  "))

(define (genxsym name)
  (gensym (string-append (if (string? name) name (symbol->string name)) "-")))

;; @deffn {Procedure} x_y->x-y a_string => a-string
;; Convert a C-like name to a Scheme-like name.
;; @end deffn
(define (x_y->x-y name)
  (string-map (lambda (ch) (if (char=? ch #\_) #\- ch)) name))

;; @deffn {XTIL} nx-undefined-xtil
;; as to @code{nx-undefined} in @path{nx-lib}
;; @end deffn
(define nx-undefined-xtil `(const ,(if #f #f)))

;; @deffn {Procedure} nx-push-scope dict
;; Push scope level of dict, returning new dict.
;; @end deffn
(define (nx-push-scope dict)
  (list (cons '@P dict)))

;; @deffn {Procedure} nx-pop-scope dict
;; Pop scope level of dictionary @var{dict}, returning dictionary
;; for popped scope.
;; @end deffn
(define (nx-pop-scope dict)
  (or (assq-ref dict '@P) (error "coding error: too many pops")))

;; @deffn {Procedure} nx-top-level? dict
;; This is a predicate to indicate if @var{dict}'s scope top-level.
;; for popped scope.
;; @end deffn
(define (nx-top-level? dict)
  (let loop ((dict dict))
    (cond
     ((assoc-ref dict '@top) #t)
     ((assoc-ref dict '@F) #f)
     (else (loop (assoc-ref dict '@P))))))

;; to add to scope (@P) or frame (@F) etc
(define (nx-add-taglevel entry dict tag)
  (if (assq-ref dict tag)
      (cons entry dict)
      (let loop ((@P (assq '@P dict)))
	(and @P (let ((d (cdr @P)))
		  (cond
		   ((not @P) #f)
		   ((assq-ref d tag) (set-cdr! @P (cons entry d)) dict)
		   (else (loop (assq '@P d)))))))))

;; @deffn {Procedure} nx-add-toplevel name dict
;; @deffnx {Procedure} nx-add-framelevel name dict
;; Given a string @var{name} and dictionary @var{dict} return a new
;; dictionary with a top-level reference for name added.  This can be
;; retrieved with @code{nx-lookup name dict} where @code{dict} is the
;; return value.
;; @example
;; (let ((dict (nx-add-toplevel "foo" dict)))
;;    (nx-lookup "foo" dict)) => (toplevel foo)
;; @end example
;; @end deffn
(define (str-and-sym str-or-sym)
  (values
   (if (string? str-or-sym) str-or-sym (symbol->string str-or-sym))
   (if (symbol? str-or-sym) str-or-sym (string->symbol str-or-sym))))

(define (nx-add-toplevel name dict)
  (call-with-values (lambda () (str-and-sym name))
    (lambda (str sym)
      (nx-add-taglevel (cons str `(toplevel ,sym)) dict '@top))))

(define (nx-add-framelevel name dict)
  (call-with-values (lambda () (str-and-sym name))
    (lambda (str sym)
      (nx-add-taglevel (cons str `(lexical ,sym ,(genxsym str))) dict '@F))))

(define (nx-insert-scopelevel dict names)
  ;; why not add-tag-level?
  (define (finish head tail)
    (append-reverse
     (fold
      (lambda (name head)
        (let ((ref (nx-lookup name tail)))
          (unless ref (sferr "+++ warning: ~S not defined\n" name))
          (acons name (if ref ref `(toplevel ,(string->symbol name))) head)))
      head names)
     tail))
  (let loop ((head '()) (tail dict))
    (cond
     ;;((null? tail) #f)                    ; should never happen
     ((eq? (caar tail) '@P) (finish head tail))
     ((eq? (caar tail) '@top) (finish head tail))
     (else (loop (cons (car tail) head) (cdr tail))))))
(export nx-insert-scopelevel)
      
;; @deffn {Procedure} nx-lexical-symbol? name dict
;; This is a predicate to indicate if @var{name} is a lexical symbol.
;; @end deffn
(define (nx-lexical-symbol? name dict)
  (let ((ref (nx-lookup name dict)))
    (and ref (eq? 'lexical (car ref)))))

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
  (acons name `(lexical ,(string->symbol name) ,(genxsym name)) dict))

;; @deffn {Procedure} nx-add-lexicals name1 ... nameN dict
;; A fold-right with @code{nx-add-lexical}, equivalent to
;; @example
;; (fold-right nx-add-lexical dict (name1 ... nameN))
;; @end example
;; @end deffn
(define (nx-add-lexicals . args)
  (let iter ((args args))
    (if (null? (cddr args)) (nx-add-lexical (car args) (cadr args))
        (nx-add-lexical (car args) (iter (cdr args))))))

;; @deffn {Procedure} nx-add-variable name dict
;; Add lexical or toplevel based on level.  This will
;; call @code{nx-add-toplevel} if in top-level frame, else
;; @code{nx-add-lexical}.
;; @end deffn
(define (nx-add-variable name dict)
  (if (nx-top-level? dict)
      (nx-add-toplevel name dict)
      (nx-add-lexical name dict)))
(define nx-add-symbol nx-add-variable)

;; @deffn nx-lookup name dict
;; NEED TO DO THIS
;; for nonlocals either push between
;; @end deffn
(define (nx-lookup name dict)
  (cond
   ((not dict) #f)
   ((null? dict) #f)
   ((assoc-ref dict name))
   ((assoc-ref dict '@P) => (lambda (dict) (nx-lookup name dict)))
   ((nx-lookup-in-env name (assoc-ref dict '@M)))
   ((nx-lookup-in-env (x_y->x-y name) (assoc-ref dict '@M)))
   (else #f)))

;; @deffn nx-lookup-in-frame name dict
;; @xdeffn nx-lookup-in-scope name dict
;; @end deffn
(define (nx-lookup-in-frame name dict)
  (let loop ((dict dict))
    (cond
     ((null? dict) #f)
     ((eq? '@F (caar dict)) #f)
     ((equal? name (caar dict)) (cdar dict))
     (else (loop (cdr dict))))))

(define (nx-lookup-in-scope name dict)
  (assoc-ref dict name))

;; @deffn {Procedure} nx-lookup-in-env name env
;; @end deffn
(define (nx-lookup-in-env name env)
  (let ((sym (if (string? name) (string->symbol name) name)))
    (if (and env (module-variable env sym))
	`(@@ ,(module-name env) ,sym)
	#f)))

;; @deffn nx-ensure-variable name dict => dict
;; @xdeffn nx-ensure-variable/scope name dict => dict
;; Ensure deffn is in frame, starting from current scope dict,
;; or at toplevel if no frames are defined.
;; A modified dict may be returned, or a modified parent.
;; The second form checks only the local scope.
;; @end deffn
(define (nx-ensure-variable name dict)
  (if (nx-lookup name dict)
      dict
      (or (nx-add-framelevel name dict)
	  (nx-add-toplevel name dict))))

(define (nx-ensure-variable/scope name dict)
  (if (lookup-in-scope name dict)
      dict
      (or (nx-add-framelevel name dict)
	  (nx-add-toplevel name dict))))

;; @deffn {Procedure} nx-lookup-gensym name dict [label] => gensym
;; Lookup up nearest parent lexical and return associated gensym.
;; @lisp
;; (nx-lookup-gensym "foo" dict) => JS~1234
;; (nx-lookup-gensym "foo" dict #:label "oloop") => JS~432
;; @end lisp
;; @end deffn
(define* (nx-lookup-gensym name dict #:key label)
  (if label
      (let iter ((cdict dict) (pdict (assoc-ref dict '@P)))
	(if (not pdict) #f
	    (if (and (assoc-ref pdict label)
		     (assoc-ref "~exit" cdict))
		(assoc-ref name cdict)
		(iter pdict (assoc-ref pdict '@P)))))
      (let* ((sym (nx-lookup name dict)))
	(if (not sym) (error "nx-util: not found:" name))
	(caddr sym))))

;; @deffn {Procedure} rtail kseed
;; This is used often in the up-phase of converting sxml trees to
;; Tree-IL trees.
;; @end deffn
(define (rtail kseed)
  (cdr (reverse kseed)))

(define (singleton? expr)
  (and (pair? expr) (null? (cdr expr))))

;; @deffn {Procedure} make-and . args
;; (and a b c) => (if a (if b (if c #t #f) #f) #f)
;; @end deffn
(define (make-and . args)
  (let iter ((args args))
    (if (null? args) '(const #t)
        `(if ,(car args) ,(iter (cdr args)) (const #f)))))

;; @deffn {Procedure} make-or . args
;; (or a b c) => (if a #t (if b #t (if c #t #f)))
;; @end deffn
(define (make-or . args)
  (let iter ((args args))
    (if (null? args) '(const #f)
        `(if ,(car args) (const #t) ,(iter (cdr args))))))

;; @deffn {Procedure} rev/repl arg0 list
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
;; @example
;;   NEED EXAMPLE
;; @end example
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


;; @deffn {Procedure} make-arity arg-list
;; This procedure generates a tree-il arity part of a lambda-case.
;; @list
;;  (arg-list (arg (lexical a a-1)) (opt-arg (lexical b b-1) (const 1))
;;      (key-arg (lexical c c-1) (const 1)))
;;  (req opt rest kw inits gensyms)
;; @end lisp
;; @end deffn
(define (make-arity arg-list)
  (let loop ((req '())
	     (opt '())
	     (rest #f)
	     (kw '())
	     (inits '())
	     (gsyms '())
	     (args (cdr arg-list)))
    (if (null? args)
	(list (reverse req) (reverse opt) rest
	      (if (null? kw) #f (reverse kw))
	      (reverse inits) (reverse gsyms))
	(let* ((rg (car args)) (lx (cadr rg)) (var (cadr lx)) (sym (caddr lx)))
	  (case (car rg)
	    ((arg)
	     (loop (cons var req) opt rest kw inits
		   (cons sym gsyms) (cdr args)))
	    ((opt-arg)
	     (loop req (cons var opt) rest kw (cons (caddr rg) inits)
		   (cons sym gsyms) (cdr args)))
	    ((key-arg) 
	     (loop req opt rest (cons var kw) (cons (caddr rg) inits)
		   (cons sym gsyms) (cdr args)))
	    ((rest-arg)
	     (loop req opt var kw inits (cons sym gsyms) (cdr args)))
	    (else (error "coding error")))))))


;; Terms:
;; expr-or-expr-list: ((...) ...) or (tag ...)

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

;; @deffn {Procecure} make-switch swx-var kseed default
;; options: mem (membership), equ (equality)
;; where @var{swx-var} is the switch case variable used to bind the
;; switch expression.  @var{kseed} is the reverse list from the translator
;; where the end looks like @code{exp} @code{'switch}.
;; @exmaple
;; ((default) (case const-or-list seq) ... expr 'switch)
;; @end example
;; @example
;; switch expr case ...
;; case: expr-or-expr-list stmt-list
;; @end example
;; where cases are in reverse order and may end in a tag
;; @example
;; var-name: lexical for defined expr-arg to switch
;; cases : list of (tag [til-const | til-list] seq)
;; @end example
;; Note: As switch cases are handled in the translater one could work
;; to keep the default case at the head of the list.
;; @end deffn
(define* (make-switch var kseed def #:key (mem 'member) (equ 'equal?))
  ;; sc : switch case : (case ,key ,val)
  (let loop ((nxt-case def) (nxt-seed (car kseed)) (ks kseed))
    (if (symbol? (car ks)) ;; == 'switch
	`(let ,(list (cadr var)) ,(list (caddr var)) ,(list nxt-seed) ,nxt-case)
	(let* ((key (cadr nxt-seed)) (seq (caddr nxt-seed))
	       (op (if (eq? 'const (car key)) equ mem)))
	  (loop `(if (call (primitive ,op) ,var ,key) ,seq ,nxt-case)
		(car ks) (cdr ks))))))

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
  (let* ((olsym (genxsym "oloop"))
         (bsym (nx-lookup-gensym "break" dict))
         (csym (nx-lookup-gensym "continue" dict))
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
  (let ((ilsym (genxsym "iloop")))
    (make-loop expr body dict ilsym `(call (lexical iloop ,ilsym)))))

;; @deffn {Procedure} make-while test body dict
;; This generates code for the following source:
;; where @var{expr} is the condtional expression, @var{body} is the body,
;; and is the scope dictionary which must contain the labels for
;; @code{break} and @code{continue}.
;; @end deffn
(define (make-while test body dict)
  (let ((ilsym (genxsym "iloop")))
    (make-loop test body dict ilsym
                    `(if ,test (call (lexical iloop ,ilsym)) (void)))))

;; @deffn {Procedure} make-for init test next body dict
;; This generates code for the following source:
;; where @var{expr} is the condtional expression, @var{body} is the body,
;; and is the scope dictionary which must contain the labels for
;; @code{break} and @code{continue}.
;; @end deffn
(define (make-for init test next body dict)
  (let ((ilsym (genxsym "iloop")) (body `(seq ,body ,next)))
    `(seq ,init
          ,(make-loop test body dict ilsym
                      `(if ,test (call (lexical iloop ,ilsym)) (void))))))

;; -- to be organized into above

;; @deffn make-+SP tree
;; @end deffn
(define (make-+SP tree)
  (lambda (obj)
    (set-source-properties! obj (source-properties tree))
    obj))

;; @deffn wrap-locals body kdict
;; Given @var{body}, a tree-il, executing in lexical env given by dict
;; @var{kdict} generate a tree-il lex form containing the locals.
;; @*
;; Problem here is that how do we omit non-locals?
;; @end deffn
(define (wrap-locals body kdict)
  (let loop ((nl '()) (ll '()) (vl '())
             (vs (let loop ((kd kdict))
		   (if (eq? '@F (caar kd)) '()
                       (case (cadar kd)
                         ((nonlocal toplevel) (loop (cdr kd)))
                         (else (cons (cdar kd) (loop (cdr kd)))))))))
    (if (null? vs)
        `(let ,nl ,ll ,vl ,body)
	(loop (cons (list-ref (car vs) 1) nl)
	      (cons (list-ref (car vs) 2) ll)
	      (cons '(void) vl)
	      (cdr vs)))))

;; -- to be deprecated

;; deprecate
(define (opcall-generator xlib)
  (define (xlib-ref name) `(@@ ,xlib ,name))
  (lambda (op seed kseed kdict)
    (values (cons (rev/repl 'call (xlib-ref op) kseed) seed) kdict)))

;; --- last line ---
