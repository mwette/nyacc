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
;;    @code{@F} denotes frame, where we need to make room for locals
;;    If code to define a variable it should be put in the current frame.
;; 3) routines that return dict's should return '() if not found
;;    e.g. nx-parent dict => '()
;; 4) stanard prompt tags: return break (exit ?)


;; Entries in the symbol table are of the form
;;   (name . var)
;; where var is '(lexical sym sym-tag) or '(toplevel sym).
;; For example.
;;   ("foo" . '(lexical foo foo-123))
;; or
;;   ("foo" . '(toplevel foo))
;;
;; The symbol table has the form
;;                     (("c" . v3) ("b" . v2) ("a" . v1) (@P . '()))
;;   (("b" . v4) (@P . ^))
;; which we will write in the eqivalent form
;;   (("b" . v4) (@P ("c" . v3) ("b" . v2) ("a" . v1)))
;; and where @P is a key for the parent scope.
;;
;; Reserved tags are @P (parent scope), @M (module), @env and @top.
;; See behavior of nx-lookup: it may be not great.
;; It is conventional to use @code{@F} for a frame.
;;
;; When we lookup a variable using assoc it will not penetrate the current
;; scope.  Thus, in the above example, a lookup for "b" will return v4; a
;; lookup for "b" in the parent scope will return v2.  Ordinarily, when we
;; lookup variables we will search parent scopes until we find something.
;; If we want to stop at some scope, say the top scope in a function
;; definition, we will add a tag (e.g., @F) for the scope.  In this case
;; the dict may look like the following:
;;   (("b" . v4) (@F . "foo") @P ("c" . v3) ("b" . v2) ("a" . v1) @P)
;; So when you start compiling a function definition, you'd probly push
;; scope, and then add a tag (e.g., @F) to mark that scope level.  Then,
;; maybe several scope levels down, you can lookup using the following:
;;   (lookup-tagged "foo" dict '@F)
;;
;; If you run across an identifier that should be in the current fuction
;; you would call, say, @code{(ensure-variable/tagged name dict '@F)}
;;   ensure-variable dict name
;;   ensure-variable/scope dict name
;;   ensure-variable/tagged dict tag tag
;;   ensure-variable/global dict name

;; FIXME:
;; 1. Currently assumes dict has @top tag at top.
;; 2. Include modules

;;  used like this
;;    [local] -> (@F) -> [return] -> (@P -> [global])
;;
;;  TODO: discuss @M for modules
;;    ((@env . #<directory (guile-user) 7fdad962cc80>)
;;     ("puts" @@ (nyacc lang tsh xlib) tsh:puts)
;;     ("format" @@ (nyacc lang tsh xlib) tsh:format)
;;     ("avec" @@ (nyacc lang tsh xlib) tsh:avec)
;;     ("fvec" @@ (nyacc lang tsh xlib) tsh:fvec)
;;     ("ivec" @@ (nyacc lang tsh xlib) tsh:ivec)
;;     ("vlen" @@ (nyacc lang tsh xlib) tsh:vlen)
;;     (@top . #t)
;;; Code:

(define-module (nyacc lang nx-util)
  #:export (genxsym
            nx-unspecified-xtil

            nx-top-level?
            nx-push-scope nx-pop-scope nx-add-tag
            nx-lookup-env nx-env-lookup

            nx-add-toplevel
            nx-add-taglevel nx-add-taglevel*
            nx-add-at-scope nx-add-at-scope*
            nx-add-variable
            
            ;;nx-add-lexical
            ;;nx-add-lexicals

            nx-lookup/global
            nx-lookup/tagged
            nx-lookup/scope
            nx-lookup

            nx-ensure/global
            nx-ensure/tagged
            nx-ensure/scope
            nx-ensure

            nx-insert-nonlocals         ; ???
            nx-lexical-ref

            ;; decprecate:
            nx-lookup-in-frame
            nx-add-framelevel ;; use nx-add-taglevel
            ;; ^ deprecate?

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
            make-function
            make-switch make-loop make-do-while make-while make-for
            ;; deprecated
            nx-add-symbol)
  #:use-module ((srfi srfi-1) #:select (fold append-reverse)))

(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(use-modules (ice-9 pretty-print))
(define (pperr exp) (pretty-print exp #:per-line-prefix "  "))


;; ============================================================================
;; general utilities

;; @deffn {Procedure} genxsym str-or-sym => gensym
;; Given a string or symbol, generate a unque symbol of the form
;; @code{str-ddd} where @code{str} is the argument and @code{ddd}
;; is a sequence of digits.
;; @end deffn
(define (genxsym name)
  "- Procedure: genxsym str-or-sym => gensym
     Given a string or symbol, generate a unque symbol of the form
     ‘str-ddd’ where ‘str’ is the argument and ‘ddd’ is a sequence of
     digits."
  (gensym (string-append (if (string? name) name (symbol->string name)) "-")))

;; @deffn {XTIL} nx-undefined-xtil
;; as to @code{nx-undefined} in @path{nx-lib}
;; @end deffn
(define nx-unspecified-xtil `(const ,(if #f #f)))

;; @deffn {Procedure} x_y->x-y a_string => a-string
;; Convert a C-like name to a Scheme-like name.
;; @end deffn
(define (x_y->x-y name)
  "- Procedure: x_y->x-y a_string => a-string
     Convert a C-like name to a Scheme-like name."
  (string-map (lambda (ch) (if (char=? ch #\_) #\- ch)) name))

;; @deffn {Procedure} x-y->x_y a-string => a_string
;; Convert a Scheme-like name to a C-like name.
;; @end deffn
(define (x-y->x_y name)
  "- Procedure: x-y->x_y a-string => a_string
     Convert a Scheme-like name to a C-like name."
  (string-map (lambda (ch) (if (char=? ch #\-) #\_ ch)) name))

;; @deffn {Procedure} str-and-sym str-or-sym
;; Given a string or symbol argument, return the string and symbol
;; as values.
;; @end deffn
(define (str-and-sym str-or-sym)
  "- Procedure: str-and-sym str-or-sym
     Given a string or symbol argument, return the string and symbol as
     values."
  (values
   (if (string? str-or-sym) str-or-sym (symbol->string str-or-sym))
   (if (symbol? str-or-sym) str-or-sym (string->symbol str-or-sym))))

;; @deffn {Procedure} nx-env-lookup env name
;; Lookup (string) @var{name} in @var{env}.  The argument
;; @var{env} can be a Guile module or an alist.  If not found,
;; (or @code{env} is @code{#f}, say) return @code{#f}.
;; @end deffn
(define (nx-env-lookup env name)
  (cond
   ((module? env)
    (let ((mname (module-name env)) (sname (string->symbol name)))
      (and (module-variable env sname) `(@@ ,mname ,sname))))
   (else #f)))

;; @deffn {Procedure} nx-lookup-env dict
;; Find the env in the symtol table. [TODO: move this]
;; @end deffn
(define (nx-lookup-env dict)
  (cond
   ((assoc-ref dict '@P) => nx-lookup-env)
   ((assoc-ref dict '@env))
   (else #f)))

;; ============================================================================
;; symbol table - this has been a struggle

;; @deffn {Procedure} nx-top-level? dict
;; This is a predicate to indicate if @var{dict}'s scope is at the top level.
;; @end deffn
(define (nx-top-level? dict)
  (and (not (assq-ref dict '@P)) #t))

;; @deffn {Procedure} nx-push-scope dict
;; Push scope level of dict, returning new dict.
;; @end deffn
(define (nx-push-scope dict)
  "- Procedure: nx-push-scope dict
     Push scope level of dict, returning new dict."
  (list (cons '@P dict)))

;; @deffn {Procedure} nx-pop-scope dict
;; Pop scope level of dictionary @var{dict}, returning dictionary
;; for popped scope.
;; @end deffn
(define (nx-pop-scope dict)
  "- Procedure: nx-pop-scope dict
     Pop scope level of dictionary DICT, returning dictionary for popped
     scope."
  (or (assq-ref dict '@P) (error "coding error: too many pops")))

;; @deffn {Procedure} nx-add-tag dict tag [name]
;; Add the given tag in the current scope.   This allows the current
;; scope level to be referenced perhaps several scope levels down.  It
;; is commonly used to add function level variables.
;; @end deffn
(define* (nx-add-tag dict tag #:optional name)
  (acons tag name dict))


;; @deffn {Procedure} nx-add-toplevel dict name
;; Add a toplevel lexical to the current scope.  The dictionary @var{dict} may
;; be mutated.  Returns the mutates dictionary or a new dictionary.
;; @end example
;; @end deffn
(define (nx-add-toplevel dict name)
  (nx-add-taglevel dict '@top name))

;; generate a pair, one of
;;   ("name" . (toplevel name))
;;   ("name" . (lexical name name-123))
;; depending on if toplevel or not
(define (mk-entry name dict)
  (let ((symrep (if (string? name) (string->symbol name) name))
        (strrep (if (symbol? name) (symbol->string name) name)))
    (cons strrep (if (nx-top-level? dict)
                     `(toplevel ,symrep)
                     `(lexical ,symrep ,(gensym strrep))))))

;; @deffn {Procedure} nx-add-taglevel dict tag name
;; Add @var{name} a symbol or string (maybe narrow later?).
;; Example: Given @code{dict}
;; @example
;;  (("a" ..) ("b" ..) (@F . "") ("c" ..) @P ...)
;; @end example
;; the expression 
;; @example
;; (nx-add-taglevel dict '@F "d")
;; @end example
;; generates the a-list
;; @example
;;  (("a" ..) ("b" ..) ("d" ..) (@F . "") ("c" ..) @P)
;; @end example
;; @end deffn
(define (nx-add-taglevel dict tag name)
  (if (eq? tag (caar dict))
      (cons (mk-entry name dict) dict)
      (let loop ((fst (car dict)) (rst (cdr dict)))
        (cond
         ((null? rst) #f)
         ((eq? tag (caar rst))
          (set-cdr! fst (cons (mk-entry name rst) rst))
          dict)
         ((eq? '@P (caar rst)) (loop (car rst) (cdar rst)))
         (else (loop (car rst) (cdr rst)))))))

;; @deffn {Procedure} nx-add-taglevel* dict tag name ...
;; Add multiple names at taglevel.  See @code{nx-add-taglevel}.
;; @end deffn
(define (nx-add-taglevel* dict tag . names)
  (let loop ((dt dict))
    (cond
     ((null? dt) #f)
     ((eq? tag (caar dt))
      (set-cdr! dt (fold (lambda (n d) (cons (mk-entry n dt) d))
                         (cdr dt) names))
      dict)
     ((eq? '@P (caar dt)) (loop (cdar dict)))
     (else (loop (cdr dt))))))

(define (nx-add-at-scope dict name)
  (cons (mk-entry name dict) dict))

(define (nx-add-at-scope* dict . names)
  (let loop ((dt dict) (nl names))
    (if (pair? nl) (loop (cons (mk-entry (car nl) dict) dt) (cdr nl)) dt)))

;; @deffn {Procedure} nx-add-variable dict name
;; Add a variable (aka symbol) to the current scope.  It may be lexical or
;; toplevel. The dictionary @var{dict} is not mutated.   A new dictionary is
;; returned.
;; @end deffn
(define (nx-add-variable dict name)
  (if (nx-top-level? dict)
      (nx-add-toplevel dict name)
      (nx-add-lexical dict name)))



;; @deffn {Procedure} nx-lexical-symbol? dict name
;; This is a predicate to indicate if @var{name} is a lexical symbol.
;; @end deffn
(define (nx-lexical-symbol? dict name)
  (let ((ref (nx-lookup dict name)))
    (and ref (eq? 'lexical (car ref)))))

;; @deffn {Procedure} nx-add-lexical dict name
;; Given a string @var{name} and dictionary @var{dict} return a new
;; dictionary with a lexical reference added.  The reference can be
;; retrieved with @code{nx-lookup dict name} where @code{dict} is the
;; return value.
;; @example
;; (let ((dict (nx-add-lexical dict "foo")))
;;    (nx-lookup dict "foo")) => (lexical foo foo-123)
;; @end example
;; @end deffn
(define (nx-add-lexical dict name)
  (acons name `(lexical ,(string->symbol name) ,(genxsym name)) dict))


;; @deffn {Procedure} nx-add-lexicals dict name ...
;; Evaluate @code{nx-add-lexical} for each @var{NAME}
;; @example
;; (nx-add-lexical dict '("foo" "bar" "baz"))
;; @end example
;; @end deffn
(define (nx-add-lexicals dict . names)
  (for-each (lambda (name) (nx-add-lexical dict name)) names))


;; @deffn nx-lookup dict name
;; needs documentation @*
;; for nonlocals either push between
;; @end deffn
(define (nx-lookup dict name)
  (cond
   ((not dict) #f)
   ((assoc-ref dict name))
   ((assoc-ref dict '@P) => (lambda (dict) (nx-lookup dict name)))
   ((nx-env-lookup (nx-lookup-env dict) name))
   ((nx-lookup/modules dict name))
   (else #f)))

;; @deffn nx-lookup/tagged name dict tag
;; Lookup @var{name} in the scope with @var{tag}
;; @end deffn
(define (nx-lookup/tagged dict tag name)
  (let loop ((dict dict))
    (cond
     ((null? dict) #f)
     ((equal? name (caar dict))
      (let ((ref (cdar dict)))
        (and (eq? 'lexical (car ref)) ref)))
     ((eq? '@P (caar dict))
      (loop (cdar dict)))
     ((eq? tag (caar dict))
      (assoc-ref (cdr dict) name))
     (else (loop (cdr dict))))))

;; @deffn nx-lookup/scope dict name
;; Lookup @var{name} in the current @var{dict} scope.
;; @end deffn
(define (nx-lookup/scope dict name)
  (assoc-ref dict name))

;; @deffn {Procedure} nx-lookup/modules dict name
;; Lookup in the dictionary modules.
;; @end deffn
(define (nx-lookup/modules dict name)
  (cond
   ((assoc-ref dict '@P) => (lambda (dict) (nx-lookup/modules dict name)))
   ((assoc '@M dict) => (lambda (mods) (or (nx-env-lookup (car mods) name)
                                           (nx-lookup/modules (cdr mods) name))))
   (else #f)))

;; @deffn nx-ensure dict name => dict
;; @xdeffn nx-ensure/scope dict name => dict
;; @xdeffn nx-ensure/tagged dict name tag => dict
;; @xdeffn nx-ensure/global dict name => dict
;; Ensure @var{name} is defined in the table, scope, (tagged) frame,
;; or global.  If not existing, add to local scope.
;; A mutated dict may be returned in the @code{/} forms.
;; @end deffn
(define (nx-ensure dict name)
  (if (nx-lookup dict name)
      dict
      (if (nx-top-level? dict)
          (nx-add-toplevel dict name)
          (nx-add-lexical dict name))))

(define (nx-ensure/scope dict name)
  (if (nx-lookup/scope dict name)
      dict
      (if (nx-top-level? dict)
          (nx-add-toplevel dict name)
          (nx-add-lexical dict name))))

(define (nx-ensure/tagged dict tag name)
  (if (nx-lookup/tagged dict tag name)
      dict
      (or (nx-add-taglevel dict tag name)
          (nx-add-toplevel dict name))))

(define (nx-ensure/global dict name)
  (if (nx-lookup-in-frame dict name)
      dict
      (or (nx-add-framelevel dict name)
          (nx-add-toplevel dict name))))

(define (nx-insert-nonlocals dict names)
  (define (finish head tail)
    (let ((entries
           (map (lambda (name)
                  (let* ((ref (nx-lookup dict name))
                         (val (if ref ref `(toplevel ,(string->symbol name)))))
                    (unless ref (sferr "warning: ~S not defined\n" name))
                    (cons name val)))
                names)))
      (let loop ((head head) (tail (append-reverse entries tail)))
        (cond
         ((null? head) tail)
         ((eq? '@P (car head)) (loop (cdr head) (list (cons '@P tail))))
         (else (loop (cdr head) (cons (car head) tail)))))))
  (let loop ((head '()) (tail dict))
    (cond
     ((eq? (caar tail) '@F) (finish (cons (car tail) head) (cdr tail)))
     ((eq? (caar tail) '@P) (loop (cons (caar tail) head) (cdar tail)))
     ((eq? (caar tail) '@top) (finish head tail))
     (else (loop (cons (car tail) head) (cdr tail))))))

;; @deffn {Procedure} nx-lexical-ref dict name [label] => gensym
;; Lookup up nearest parent lexical and return associated gensym.
;; @lisp
;; (nx-lookup-gensym dict "foo") => JS~1234
;; (nx-lookup-gensym dict "foo" #:label "oloop") => JS~432
;; @end lisp
;; @end deffn
(define* (nx-lexical-ref dict name #:key label)
  (if label
      (let iter ((cdict dict) (pdict (assoc-ref dict '@P)))
        (if (not pdict) #f
            (if (and (assoc-ref pdict label)
                     (assoc-ref "~exit" cdict))
                (assoc-ref name cdict)
                (iter pdict (assoc-ref pdict '@P)))))
      (let* ((sym (nx-lookup dict name)))
        (if (not sym) (error "nx-util: not found:" name))
        (caddr sym))))

;; to be deprecated .....

;; @deffn {Procedure} nx-add-frametag dict [name]
;; Push frame tag on the stack.
;; @end deffn
(define* (nx-add-frametag dict #:optional name)
  (acons '@F name dict))

;; @deffn nx-lookup-in-frame dict name
;; @xdeffn nx-lookup-in-scope dict name
;; in frame, lookup to end of frame scope, so
;; for dict = (... (@F . "def") ("xx" toplevel xx) (@P . (more)))
;; (lookup-in-frame "xx" dict) => (toplevel "xx")
;; @end deffn
(define (nx-lookup-in-frame dict name)
  (let loop ((dict dict))
    (cond
     ((null? dict) #f)
     ((equal? name (caar dict))
      (let ((ref (cdar dict)))
        (and (eq? 'lexical (car ref)) ref)))
     ((eq? '@P (caar dict))
      (loop (cdar dict)))
     ((eq? '@F (caar dict))
      (assoc-ref (cdr dict) name))
     (else (loop (cdr dict))))))

;; ^ ... to be deprecated


;; ============================================================================

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
;; @var{body} is an expression.
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

;; @deffn {Scheme} make-function name lang arity body
;; return a function in itil
;; @end deffn
(define (make-function name lang arity body)
  (let* ((meta '())
         (meta (if lang (cons `(language . ,lang) meta) meta))
         (meta (if name (cons `(name . ,name) meta) meta)))
    `(lambda ,meta (lambda-case (,arity ,body)))))

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
         (bsym (nx-lexical-ref "break" dict))
         (csym (nx-lexical-ref "continue" dict))
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
(define nx-add-symbol nx-add-variable)

(define (nx-add-framelevel dict name)
  (call-with-values (lambda () (str-and-sym name))
    (lambda (str sym)
      (nx-add-taglevel (cons str `(lexical ,sym ,(genxsym str))) dict '@F))))

(define (opcall-generator xlib)
  (define (xlib-ref name) `(@@ ,xlib ,name))
  (lambda (op seed kseed kdict)
    (values (cons (rev/repl 'call (xlib-ref op) kseed) seed) kdict)))

;; --- last line ---
