;;; nyacc/lang/c99/munge.scm - util's for processing output of the C99 parser

;; Copyright (C) 2015-2018,2020 Matthew R. Wette
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Notes:

;; 1) mdecl == munged (unwrapped) declaration
;;    udecl : unitzied decl (not declr-list and one init-declr, etc
;;    utent : (name . udecl) where name is string or pair (struct . "foo")
;; 2) Usual sequence is: expand-typerefs, stripdown-udecl, udecl->mdecl.
;; 3) unitize-decl is shallow.  It does not dive into structs and unitize.
;; 4) expand-typerefs is shallow?  It does not dive into structs and unitize.
;; 5) dictize replaces old unitize, w/ unitize making more sense now
;;    a) unitize: (int (x y)) -> ((int x) (int y))
;;    b) dictize: (int (x y)) -> (("x" . (int x)) ("y" . (int y)))

;; Todo:
;; 2) I want a way to keep named enums in expand-typerefs.
;;    Currently, they are expanded to int.
;; 5) In expand-typerefs if need to expand `foo_t *x' then change to
;;    a) if struct use `struct foo *x;'
;;    b) if fixed/float use `int *x;' etc
;;    c> if function use `void *x;'
;; 6) Check use of comments as attributes.
;; 7) maybe move to munge-base: typedef-decl? udict->typedef-names

;;; Code:

(define-module (nyacc lang c99 munge)
  #:export (c99-trans-unit->udict
	    c99-trans-unit->udict/deep
	    udict-ref udict-struct-ref udict-union-ref udict-enum-ref

	    ;; generating def's dict
	    c99-trans-unit->ddict udict-enums->ddict udict-add-enums

	    ;; munging
	    stripdown-udecl
	    udecl-rem-type-qual specl-rem-type-qual
	    udecl->mdecl/comm mdecl->udecl

	    unwrap-decl
	    canize-enum-def-list
	    fixed-width-int-names

	    typedef-decl?
	    
	    unitize-decl unitize-comp-decl unitize-param-decl
	    dictize-decl dictize-comp-decl dictize-param-decl
	    decl-id
	    iter-declrs
	    split-decl

	    udict->typedef-names
	    inc-keeper?

	    ;; debugging
	    stripdown-1
	    tdef-splice-specl
	    tdef-splice-declr)
  #:re-export (expand-typerefs
	       reify-declr reify-decl
	       udecl->mdecl split-udecl
	       declr-ident declr-name
	       clean-field-list clean-fields)
  #:use-module (nyacc lang c99 cxeval)
  #:use-module (nyacc lang c99 munge-base)
  #:use-module (nyacc lang c99 pprint)
  #:use-module (nyacc lang c99 util)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
  #:use-module ((sxml fold) #:select (foldts foldts*))
  #:use-module (sxml match)
  #:use-module (srfi srfi-11)		; let-values
  #:use-module (srfi srfi-2)
  #:use-module ((srfi srfi-1) #:select (fold fold-right remove))
  #:use-module (system base pmatch)
  ;; debugging:
  #:use-module (system vm trace)
  #:use-module (ice-9 pretty-print))
;; undocumented Guile builtins: or-map

(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))
(define OA object-address)

(define tmap-fmt
  '(("char" "%hhd")
    ("unsigned char" "%hhu")
    ("short int" "%hd")
    ("unsigned short int" "%hu")
    ("int" "%d")
    ("unsigned int" "%u")
    ("long int" "%ld")
    ("unsigned long int" "%lu")
    ("long long int" "%lld")
    ("unsigned long long int" "%llu")))

;; @deffn {Procedure} attr-append . attr-lists
;; This is hack for now: it is @code{append}. It needs to be set up to combine
;; @code{attributes} and @code{comment}.
;; @end deffn
;; TODO FIXME
(define attr-append append)

;; Use the term @dfn{udecl}, or unit-declaration, for a declaration which has
;; only one decl-item.  That is where,
;; @example
;; @end example
;; (decl (decl-spec-list ...) (init-declr-list (init-declr ...) ...))
;; @noindent
;; has been replaced by
;; (decl (decl-spec-list ...) (init-declr ...))
;; ...
;; @example
;; @end example

;; mdecl is
;; ("foo" (pointer-to) (ary-declr 3) (fixed-type "unsigned int"))
;; which can be converted to
;; ("(*foo) (ary-declr 3) (fixed-type "unsigned int"))
;; which can be converted to
;; (("((*foo)[0])" (fixed-type "unsigned int"))
;;  ("((*foo)[1])" (fixed-type "unsigned int"))
;;  ("((*foo)[2])" (fixed-type "unsigned int"))

;; may need to replace (typename "int32_t") with (fixed-type "int32_t")

;; @deffn {Procedure} inc-keeper? tree inc-filter => #f| tree
;; This is a helper.  @var{inc-filter} is @code{#t}, @code{#f} or a
;; precicate procedure, which given the name of the file, determines if
;; it should be processed.
;; @end deffn
(define (inc-keeper? tree filter)
  (sx-match tree
    ((cpp-stmt (include (@ (path ,path)) ,spec ,tree))
     (and (if (procedure? filter) (filter spec path) filter) tree))
    ((cpp-stmt (include-next (@ (path ,path)) ,spec ,tree))
     (and (if (procedure? filter) (filter spec path) filter) tree))
    ((cpp-stmt . ,rest) #f)
    (,_ #f)))

;; @deffn {Procedure} iter-declrs tag specl declrs seed
;; @deffnx {Procedure} iter-declrs-right tag specl declrs tail seed
;; This is a support procedure for the munge routines.  If no decl'rs
;; then @var{declrs} should be #f.
;; Since @code{trans-unit->udict} is fold-right, this must be fold-right.
;; @end deffn
(define (iter-declrs tag attr specl declrs seed)
  (if (pair? declrs)
      (fold-right
       (lambda (declr seed)
	 (acons (declr-name declr) (sx-list tag attr specl declr) seed))
       seed declrs)
      (acons "" (sx-list tag attr specl) seed)))

;; @deffn {Procedure} split-decl decl => values
;; This routine splits a declaration (or comp-decl or param-decl) into
;; its constituent parts.  Attributes are currently not passed.
;; Get @code{(values tag spec-l declrs)}.  If the declrator is
;; already unitized you get that, else the list (w/o tag) of declarators.
;; @example
;;   (split-decl
;;      '(decl (decl-spec-list (typedef) (fixed-type "int"))
;;             (init-declr-list (declr (ident "a")) (declr (ident "b")))
;;             (comment " good stuff ")))
;; =>
;;   (values decl
;;           #f
;;           (decl-spec-list (typedef) (fixed-type "int")
;;           ((declr (ident "a")) (declr (ident "b")))
;;           ((comment " good stuff ")))
;; @end example
;; @end deffn
(define (split-decl decl)
  (let* ((tag (sx-tag decl))
	 (attr (sx-attr decl))
	 (spec-l (sx-ref decl 1))	  ; (decl-spec-list ...)
	 (dclr-l (sx-ref decl 2))	  ; (init-declr-list (...))
	 (declrs (and=> dclr-l sx-tail))) ; ((...))
    (values tag attr spec-l declrs)))

;; @deffn {Procedure} dictize-comp-decl decl [seed] [#:namer namer]
;; This will turn
;; @example
;; (comp-decl (decl-spec-list (type-spec "int"))
;;            (comp-decl-list
;;             (comp-declr (ident "a")) (comp-declr (ident "b"))))
;; @end example
;; @noindent
;; into
;; @example
;; ("a" . (comp-udecl (decl-spec-list ...) (comp-declr (ident "a"))))
;; ("b" . (comp-udecl (decl-spec-list ...) (comp-declr (ident "b"))))
;; @end example
;; @noindent
;; This is coded to be used with fold to be consistent with other unitize
;; functions @code{struct} and @code{union} field lists.  The result needs
;; to be reversed.
;; @end deffn
(define* (dictize-comp-decl decl #:optional (seed '()) #:key (namer def-namer))
  (cond
   ((not (pair? decl))
    (throw 'nyacc-error "dictize-decl: bad arg: ~S" decl))
   ((eqv? (sx-tag decl) 'comp-udecl)
    (acons (udecl-id decl) decl seed))
   ((eqv? (sx-tag (sx-ref decl 2)) 'comp-declr-list)
    (let-values (((tag attr spec-l declrs) (split-decl decl)))
      (iter-declrs 'comp-udecl attr spec-l declrs seed)))
   (else
    (acons (namer) decl seed))))

(define* (unitize-comp-decl decl #:optional (seed '()) #:key (namer def-namer))
  (fold-right
   (lambda (ud-entry seed)
     (if (pair? (car ud-entry)) seed (cons (cdr ud-entry) seed)))
   seed (dictize-comp-decl decl #:namer namer)))

;; @deffn {Procedure} dictize-param-decl param-decl [seed] [#:expand-enums #f]
;; This will turn
;; @example
;; (param-decl (decl-spec-list (type-spec "int"))
;;             (param-declr (ident "a")))
;; @end example
;; @noindent
;; into
;; @example
;; ("a" . (param-decl (decl-spec-list ...) (param-declr (ident "a"))))
;; @end example
;; @noindent
;; This is coded to be used with fold-right in order to preserve order
;; in @code{struct} and @code{union} field lists.
;; @*
;; TODO: What about abstract declarators?  Should use "*anon*".
;; @end deffn
(define* (dictize-param-decl decl #:optional (seed '()))
  (if (not (eqv? 'param-decl (car decl))) seed
      (let* ((tag (sx-ref decl 0))
	     (attr (sx-attr decl))
	     (spec (sx-ref decl 1))	; (type-spec ...)
	     (declr (sx-ref decl 2))	; (param-declr ...)
	     (ident (declr-ident declr))
	     (name (cadr ident)))
	(acons name decl seed))))

(define* (unitize-param-decl decl #:optional (seed '()))
  (fold-right
   (lambda (ud-entry seed)
     (if (pair? (car ud-entry)) seed (cons (cdr ud-entry) seed)))
   seed (dictize-param-decl decl)))

;; @deffn {Procedure} dictize-decl decl [seed] [#:expand-enums #f] => seed
;; This is a fold iterator intended to be used by @code{c99-trans-unit->udict}.
;; It converts the multiple @code{init-declr} items in an @code{init-declr-list}
;; of a @code{decl} into an a-list of multiple pairs of name and @code{udecl}
;; trees with a single @code{init-declr} and no @code{init-declr-list}.
;; That is, a @code{decl} of the form
;; @example
;; (decl (decl-spec-list ...)
;;       (init-declr-list (init-declr (... "a")) (init-declr (... "b")) ...))
;; @end example
;; @noindent
;; is munged into list with elements
;; @example
;; ("a" . (udecl (decl-spec-list ...) (init-declr (... "a"))))
;; ("b" . (udecl (decl-spec-list ...) (init-declr (... "b"))))
;; @end example
;; Here we generate a dictionary of all declared items in a file:
;; @example
;; (let* ((sx0 (with-input-from-file src-file parse-c))
;;	  (sx1 (merge-inc-trees! sx0))
;;	  (name-dict (fold unitize-decl-1 '() (cdr sx1))))
;; @end example
;; Enum, struct and union def's have keys @code{(enum . "name")},
;; @code{(struct . "name")} and @code{(union . "name)}, respectively.
;; See @code{udict-struct-ref}, @code{udict-union-ref}, @code{udict-enum-ref}
;; and @code{udict-ref}.  This procecure is robust to already munged decls.
;; To capture enum values as globals use @code{->ddict}.
;; @*
;; Notes: Now saving attributes at top level.  Adding attributes for
;; struct and union (e.g., @code{__packed__}).  The latter is needed
;; because they appear in files under @file{/usr/include}.
;; @end deffn
(define* (dictize-decl decl #:optional (seed '()))
  
  (define* (make-udecl type-tag attr guts #:optional typename)
    (if (and attr (pair? attr))
	`(udecl (decl-spec-list (type-spec ,(cons* type-tag `(@ ,@attr) guts))))
	`(udecl (decl-spec-list (type-spec ,(cons type-tag guts))))))

  (define (update name value tag attr specl declrs seed)
    (iter-declrs tag attr specl declrs (acons name value seed)))

  (cond
   ((not (pair? decl))
    (throw 'nyacc-error "dictize-decl: bad arg: ~S" decl))
   ((eqv? (sx-tag decl) 'udecl)
    (acons (udecl-id decl) decl seed))

   ;; todo: think more about attributes
   ;; * specl attributes are merged into structs and unions right now.
   ;;   any others?
   ((and (eqv? (sx-tag decl) 'decl) (pair? (sx-tail decl)))
    (let*-values (((tag decl-attr specl declrs) (split-decl decl))
		  ((tag) (values 'udecl)))
      ;; TODO: for typedefs add attr (typedef "name") to associated udecls
      (sx-match specl

	;; struct typedefs 
	((decl-spec-list
	  (@ . ,specl-attr)
	  (stor-spec (typedef))
	  (type-spec (struct-def (@ . ,attr) (ident ,name) . ,rest2) . ,rest1))
	 (update `(struct . ,name)
		 (make-udecl 'struct-def (attr-append attr specl-attr)
			     `((ident ,name) . ,rest2))
		 tag '() specl declrs seed))
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (struct-def . ,rest2) . ,rest1))
	 (iter-declrs tag decl-attr specl declrs seed))
	
	;; union typedefs 
	((decl-spec-list
	  (@ . ,specl-attr)
	  (stor-spec (typedef))
	  (type-spec (union-def (@ . ,attr) (ident ,name) . ,rest2) . ,rest1))
	 (update `(union . ,name)
		 (make-udecl 'union-def (attr-append attr specl-attr)
			     `((ident ,name) . ,rest2))
		 tag decl-attr specl declrs seed))
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (union-def . ,rest2) . ,rest1))
	 (iter-declrs tag decl-attr specl declrs seed))

	;; enum typedefs  -- todo: handle attributes
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (enum-def (ident ,name) . ,rest2) . ,rest1))
	 (update `(enum . ,name)
		 (make-udecl 'enum-def #f `((ident ,name) . ,rest2))
		 tag decl-attr specl declrs seed))
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (enum-def . ,rest2) . ,rest1))
	 (iter-declrs tag #f specl declrs 
		      (acons `(enum . "*anon*") (make-udecl 'enum-def #f rest2)
			     seed))
	 (update `(enum . "*anon*") (make-udecl 'enum-def #f rest2)
		 tag decl-attr specl declrs seed))
	
	;; structs
	((decl-spec-list
	  (@ . ,specl-attr)
	  (type-spec (struct-def (@ . ,attr) (ident ,name) . ,rest2) . ,rest1))
	 (update `(struct . ,name)
		 (make-udecl 'struct-def (attr-append attr specl-attr)
			     `((ident ,name) . ,rest2))
		 tag decl-attr specl declrs seed))
	((decl-spec-list
	  (type-spec (struct-def . ,rest2) . ,rest1))
	 (iter-declrs tag decl-attr specl declrs seed))

	;; unions
	((decl-spec-list
	  (type-spec (union-def (@ . ,aattr) (ident ,name) . ,rest2) . ,rest1))
	 (update `(union . ,name)
		 (make-udecl 'union-def aattr `((ident ,name) . ,rest2))
		 tag decl-attr specl declrs seed))
	((decl-spec-list
	  (type-spec (union-def . ,rest2) . ,rest1))
	 (iter-declrs tag decl-attr specl declrs seed))

	;; enums
	((decl-spec-list
	  (type-spec (enum-def (ident ,name) . ,rest2) . ,rest1))
	 (update `(enum . ,name)
		 (make-udecl 'enum-def #f `((ident ,name) . ,rest2))
		 tag decl-attr specl declrs seed))
	((decl-spec-list
	  (type-spec (enum-def . ,rest2) . ,rest1))
	 (update `(enum . "*anon*") (make-udecl 'enum-def #f rest2)
		 tag decl-attr specl declrs seed))

	(,_ (iter-declrs tag decl-attr specl declrs seed)))))
   
   ((eqv? (sx-tag decl) 'comp-udecl) (acons (udecl-id decl) decl seed))
   ((eqv? (sx-tag decl) 'comp-decl) (dictize-comp-decl decl seed))
   ((eqv? (sx-tag decl) 'param-decl) (dictize-param-decl decl seed))
   (else seed)))

(define* (unitize-decl decl #:optional (seed '()))
  (fold-right
   (lambda (ud-entry seed)
     ;;(sferr "ue: ~S\n" (car ud-entry)) (pperr (cdr ud-entry))
     ;;(pperr ud-entry)
     (if (pair? (car ud-entry)) seed (cons (cdr ud-entry) seed)))
   seed (dictize-decl decl)))

;; @deffn {Procedure} c99-trans-unit->udict tree [seed] [#:inc-filter f]
;; @deffnx {Procedure} c99-trans-unit->udict/deep tree [seed]
;; Convert a C parse tree into a assoc-list of global names and definitions.
;; This will unwrap @code{init-declr-list} into list of decls w/
;; @code{init-declr}.
;; The declarations come reversed from order in file!
;; @example
;; BUG: need to add struct and union defn's: struct foo { int x; };
;; how to deal with this
;; lookup '(struct . "foo"), "struct foo", ???
;; wanted "struct" -> dict but that is not great
;; solution: unitize-decl => '(struct . "foo") then filter to generate
;; ("struct" ("foo" . decl) ("bar" . decl) ...)
;; ("union" ("bar" . decl) ("bar" . decl) ...)
;; ("enum" ("" . decl) ("foo" . decl) ("bar" . decl) ...)
;; @end example
;; So globals could be in udict, udefs or anon-enum.
;; @example
;; What about anonymous enums?  And enums in general?
;; Anonmous enum should be expaneded into 
;; @end example
;; @noindent
;; Notes:
;; @itemize
;; @item
;; If @var{tree} is not a pair then @var{seed} -- or @code{'()} -- is returned.
;; The inc-filter @var{f} is either @code{#t}, @code{#f} or predicate procedure
;; of one argument, the include path, to indicate whether it should be included
;; in the dictionary.
;; @item
;; If this routine is called multiple times on the same tree the u-decl's will
;; not be @code{eq?} since the top-level lists are generated on the fly.
;; (See @code{unitize-decl}.)
;; @end itemize
;; @end deffn
(define* (c99-trans-unit->udict tree #:optional (seed '()) #:key inc-filter)
  (if (pair? tree)
      (fold-right
       (lambda (tree seed)
	 (cond
	  ((and (eqv? (sx-tag tree) 'decl) (pair? (sx-tail tree)))
	   (dictize-decl tree seed))
	  ((inc-keeper? tree inc-filter) =>
	   (lambda (inc-tree)
	     (c99-trans-unit->udict inc-tree seed #:inc-filter inc-filter)))
	  (else seed)))
       seed
       (cdr tree))
      seed))

(define (c99-trans-unit->udict/deep tree)
  (c99-trans-unit->udict tree #:inc-filter #t))

;; @deffn {Procedure} udecl-id udecl => string
;; generate the name 
;; @end deffn
(define (udecl-id udecl)
  ;; must be udecl w/ name
  (declr-name (sx-ref udecl 2)))

;; like member but returns first non-declr of type in dict
(define (non-declr type udict)
  (let loop ((dict udict))
    (cond ((null? dict) '())
	  ((and (pair? (caar dict)) (eqv? type (caaar dict))) dict)
	  (else (loop (cdr dict))))))

(define (enum-decl-val enum-udecl name)
  ;; (decl (decl-spec-list (type-sped (enum-def (enum-def-list ...) => "123"
  (enum-ref (cadadr (cadadr enum-udecl)) name))

;; @deffn {Procedure} udict-ref name
;; @deffnx {Procedure} udict-struct-ref name
;; @deffnx {Procedure} udict-union-ref name
;; @deffnx {Procedure} udict-enum-ref name
;; @deffnx {Procedure} udict-enum-val name
;; Look up refernce in a u-dict.  If the reference is found return the
;; u-decl.  In the case of @code{udict-enum-val} the string value is returned.
;; @end deffn
(define (udict-ref udict name)
  (or (assoc-ref udict name)
      (let loop ((dict (non-declr 'enum udict)))
	(cond
	 ((null? dict) #f)
	 ((enum-decl-val (cdar dict) name) =>
	  (lambda (val) (gen-enum-udecl name val)))
	 (else (loop (non-declr 'enum (cdr dict))))))))
(define (udict-struct-ref udict name)
  (assoc-ref udict `(struct . ,name)))
(define (udict-union-ref udict name)
  (assoc-ref udict `(union . ,name)))
(define* (udict-enum-ref udict name)
  (assoc-ref udict `(enum . ,name)))
(define* (udict-enum-val udict name)
  (let loop ((dict (non-declr 'enum udict)))
    (cond ((null? dict) #f)
	  ((enum-decl-val (cdar dict) name))
	  (else (loop (non-declr 'enum (cdr dict)))))))

;; @deffn {Variable} fixed-width-int-names
;; This is a list of standard integer names (e.g., @code{"uint8_t"}).
;; @end deffn
(define fixed-width-int-names
  '("int8_t" "uint8_t" "int16_t" "uint16_t"
    "int32_t" "uint32_t" "int64_t" "uint64_t"))

;; === enums and defines ===============

;; @deffn {Procedure} typedef-decl? decl)
;; @end deffn
(define (typedef-decl? decl)
  (eq? 'typedef (and=> (sx-ref* decl 1 1 1) sx-tag)))

(define* (udict->typedef-names udict #:optional (names '()))
  (fold (lambda (pair names)
	  (let ((name (car pair)) (decl (cdr pair)))
	    (if (typedef-decl? decl) (cons name names) names)))
	names udict))

;; === enums and defines ===============

;; @deffn {Procedure} c99-trans-unit->ddict tree [seed] [#:key ...]
;; Extract the #defines from a tree as
;; @*
;; Example:
;; @example
;;  (define (name "ABC") (repl "repl"))
;;  (define (name "MAX") (args "X" "Y") (repl "(X)..."))
;; =>
;;  (("ABC" . "repl") ("MAX" ("X" "Y") . "(X)...") ...)
;; @end example
;; @noindent
;; The entries appear in reverse order wrt how in file.
;; @*
;; New option: #:skip-fdefs to skip function defs
;; @end deffn
(define* (c99-trans-unit->ddict tree
				#:optional (ddict '())
				#:key inc-filter skip-fdefs)
  (define (can-def-stmt tree)
    (sx-match tree
      ((cpp-stmt (define (name ,name) (repl ,repl)))
       (cons name repl))
      ((cpp-stmt (define (name ,name) (args . ,args) (repl ,repl)))
       (if skip-fdefs #f (cons name (cons args repl))))
      (,_ #f)))

  (if (pair? tree)
      (fold
       (lambda (tree ddict)
	 (cond
	  ((can-def-stmt tree) => (lambda (def) (cons def ddict)))
	  ((inc-keeper? tree inc-filter) =>
	   (lambda (tree)
	     (c99-trans-unit->ddict tree ddict
				    #:inc-filter inc-filter
				    #:skip-fdefs skip-fdefs)))
	  (else ddict)))
       ddict (sx-tail tree))
      ddict))

;; @deffn {Procedure} udict-enums->ddict udict [ddict] => defs
;; Given a udict this generates a list that looke like the internal
;; CPP define structure.  That is,
;; @example
;; (enum-def-list (enum-def (ident "ABC")) ...)
;; @end example
;; @noindent
;; to
;; @example
;; (("ABC" . "0") ...)
;; @end example
;; @end deffn
(define* (udict-enums->ddict udict #:optional (ddict '()))
  (define (gen-nvl edef-list ddict)
    (let ((def-list (and=> (canize-enum-def-list edef-list udict ddict) cdr)))
      (fold (lambda (edef dd) (acons (sx-ref* edef 1 1) (sx-ref* edef 2 1) dd))
	    ddict def-list)))
  (fold
   (lambda (pair ddict)
     (if (or (pair? (car pair)) (positive? (string-length (car pair))))
	 (let* ((specs (sx-ref (cdr pair) 1))
		(tspec (sx-ref (sx-find 'type-spec specs) 1)))
	   (sx-match tspec
	     ((enum-def (ident ,name) (enum-def-list . ,defs))
	      (gen-nvl (sx-ref tspec 2) ddict))
	     ((enum-def (enum-def-list . ,defs))
	      (gen-nvl (sx-ref tspec 1) ddict))
	     (,_
	      ddict)))
	 ddict))
   ddict udict))

;; add enum symbols to dict as @code{(fixed "1")}
(define* (udict-add-enums udict #:optional (ddict '()))
  (define (gen-nvl edef-list udict ddict)
    (let ((def-list (and=> (canize-enum-def-list edef-list udict ddict) cdr)))
      (fold
       (lambda (edef dd)
	 (let ((name (sx-ref* edef 1 1)))
	   (if (member name dd) dd (acons name (sx-ref* edef 2) dd))))
       udict def-list)))
  (reverse
   (fold
    (lambda (pair udict)
      (if (or (pair? (car pair)) (positive? (string-length (car pair))))
	  (let* ((specs (sx-ref (cdr pair) 1))
		 (tspec (sx-ref (sx-find 'type-spec specs) 1)))
	    (sx-match tspec
	      ((enum-def (ident ,name) (enum-def-list . ,defs))
	       (gen-nvl (sx-ref tspec 2) udict ddict))
	      ((enum-def (enum-def-list . ,defs))
	       (gen-nvl (sx-ref tspec 1) udict ddict))
	      (,_
	       udict)))
	  udict))
    udict udict)))


;; === enum handling ===================

;; @deffn {Procedure} canize-enum-def-list enum-def-list [udict [ddict]] \
;;      => enum-def-list
;; Fill in constants for all entries of an enum list.
;; Expects @code{(enum-def-list (...))} (i.e., not the tail).
;; All enum-defs will have the form like @code{(fixed "1")}.
;; This will perform the transformation
;; @example
;; (enum-def-list (enum-def (ident "FOO") ...))
;; => 
;; (enum-def-list (enum-def (ident "FOO") (fixed "0") ...))
;; @end example
;; @noindent
;; @end deffn
(define* (canize-enum-def-list enum-def-list
			       #:optional (udict '()) (ddict '())
			       #:key fail-proc)
  (define (fail fmt . args)
    (and fail-proc (apply fail-proc fmt args) #f))

  (define (enum-cons attr id val enums)
    (cons (sx-list 'enum-defn attr id `(fixed ,(number->string val))) enums))

  (define (idcons ident ival ddict)
    (acons (sx-ref ident 1) (number->string ival) ddict))

  (let loop ((rez '()) (nxt 0) (ddict ddict) (edl (sx-tail enum-def-list 1)))
    (cond
     ((null? edl)
      (sx-cons* (sx-tag enum-def-list) (sx-attr enum-def-list) (reverse rez)))
     (else
      (sx-match (car edl)
	((enum-defn (@ . ,attr) ,ident)
	 (loop (enum-cons attr ident nxt rez) (1+ nxt)
	       (idcons ident nxt ddict) (cdr edl)))
	((enum-defn (@ . ,attr) ,ident (attribute-list . ,attrs))
	 (loop (enum-cons attr ident nxt rez) (1+ nxt)
	       (idcons ident nxt ddict) (cdr edl)))
	((enum-defn (@ . ,attr) ,ident ,expr)
	 (let ((ival (or (eval-c99-cx expr udict ddict)
			 (fail "munge: can't expand ~S" (sx-ref ident 1))
			 nxt)))
	   (loop (enum-cons attr ident ival rez) (1+ (max nxt ival))
		 (idcons ident ival ddict) (cdr edl))))
	((enum-defn (@ . ,attr) ,ident (attribute-list . ,attrs) ,expr)
	 (let ((ival (or (eval-c99-cx expr udict ddict)
			 (fail "munge: can't expand ~S" (sx-ref ident 1))
			 nxt)))
	   (loop (enum-cons attr ident ival rez) (1+ (max nxt ival))
		 (idcons ident ival ddict) (cdr edl)))))))))

;; @deffn {Procecure} enum-ref enum-def-list name => string
;; Gets value of enum where @var{enum-def-list} looks like
;; @example
;; (enum-def-list (enum-defn (ident "ABC") (p-expr (fixed "123")) ...))
;; @end example
;; so that
;; @example
;; (enum-def-list edl "ABC") => "123"
;; @end example
(define (enum-ref enum-def-list name)
  (let loop ((el (cdr (canize-enum-def-list enum-def-list))))
    (cond
     ((null? el) #f)
     ((not (eqv? 'enum-defn (sx-tag (car el)))) (loop (cdr el)))
     ((string=? name (sx-ref* (car el) 1 1)) (sx-ref* (car el) 1 2 1))
     (else (loop (cdr el))))))

;; @deffn {Procedure} gen-enum-udecl nstr vstr => (udecl ...)
;; @example
;; (gen-enum-udecl "ABC" "123")
;; =>
;; (udecl (decl-spec-list
;;         (type-spec
;;          (enum-def
;;           (enum-def-list
;;            (enum-defn (ident "ABC") (p-expr (fixed "123")))))))))
;; @end example
;; @end deffn
(define (gen-enum-udecl nstr vstr)
  `(udecl (decl-spec-list
	   (type-spec
	    (enum-def
             (enum-def-list
	      (enum-defn (ident ,nstr) (p-expr (fixed ,vstr)))))))))

;; === stripdown =======================

;; Remove remove @emph{stor-spec} elements and attributes from a u-decl.
;; what all?
;; * attributes (incl comments)
;; * type-specifiers: register, auto
;; * declrs: pointer-to type => pointer-to (void)
;; (stor-spec "auto") => empty

;; needs to work as stream processor, like sxpath, or foldts
;; (strip-attr)
;; (stor-spec ,name) w/ name in ("auto" "extern" "register" "static" "typedef")
;; (type-spec ,type) (void) (fixed-type ,name) (float-type ,name) aggr array

(define (stripdown-specl specl)
  (cons (sx-tag specl)
	(fold-right
	 (lambda (form seed)
	   (case (sx-tag form)
	     ((type-spec) (cons form seed))
	     ((stor-spec)
	      (if (eq? 'typedef (caadr form)) (cons form seed) seed))
	     (else seed)))
	 '() (sx-tail specl))))

(define (stripdown-declr declr)
  (define (fD seed tree)
    (sx-match tree
      ((pointer ,_1 ,_2) (values '() `(pointer ,_2)))
      ((pointer (type-qual-list . ,_)) (values '() '(pointer)))
      (,_ (values '() tree))))

  (define (fU seed kseed tree)
    (cond
     ((null? seed) (reverse kseed))
     ((eqv? (sx-tag tree) 'stor-spec) seed)
     ((eqv? (sx-tag tree) 'type-qual) seed)
     (else (cons (reverse kseed) seed))))
  
  (define (fH seed tree)
    (cons tree seed))
  
  (foldts* fD fU fH '() declr))

;; @deffn {Procedure} stripdown-udecl udecl => udecl
;; This routine removes forms from a declaration that are presumably not
;; required for FFI generation.
;; See also @code{cleanup-udecl} in @file{ffi-help.scm}.
;; @example
;; =>
;; @end example
;; @noindent
;; @end deffn
(define (stripdown-udecl udecl)
  (call-with-values
      (lambda () (split-udecl udecl))
    (lambda (tag attr specl declr)
      (sx-list tag attr (stripdown-specl specl) (stripdown-declr declr)))))

;; remove type qualifiers: "const" "volatile" and "restrict"
(define (specl-tail-rem-type-qual specl-tail)
  (remove (lambda (elt) (eq? 'type-qual (car elt))) specl-tail))
(define (specl-rem-type-qual specl)
  (if (not (eq? (sx-tag specl) 'decl-spec-list))
      (throw 'nyacc-error "expecting specl"))
  (sx-cons* (sx-tag specl) (sx-attr specl)
	    (specl-tail-rem-type-qual (sx-tail specl 1))))
(define (udecl-rem-type-qual udecl)
  (let ((tag (sx-tag udecl))
	(attr (sx-attr udecl))
	(specl (sx-ref udecl 1))
	(tail (sx-tail udecl 2)))
    (sx-cons* tag attr (specl-rem-type-qual specl) tail)))

;; @deffn {Procedure} strip-decl-spec-tail dsl-tail [#:keep-const? #f]
;; Remove cruft from declaration-specifiers (tail). ??
;; @end deffn
(define* (strip-decl-spec-tail dsl-tail #:key keep-const?)
  (let loop ((dsl1 '()) (const-seen? #f) (tail dsl-tail))
    (if (null? tail)
	(reverse (if (and const-seen? keep-const?)
		     (cons '(type-qual (const)) dsl1)
		     dsl1))
	(case (caar tail)
	  ((type-qual)
	   (if (equal? (cadar tail) '(const))
	       (loop dsl1 #t (cdr tail))
	       (loop dsl1 const-seen? (cdr tail))))
	  ((stor-spec)
	   (loop dsl1 const-seen? (cdr tail)))
	  (else
	   (loop (cons (car tail) dsl1) const-seen? (cdr tail)))))))

;; === munged specification ============

(define* (udecl->mdecl/comm decl #:key (def-comm ""))
  (let* ((comm (or (and=> (assq 'comment (sx-attr decl)) cadr) def-comm))
	 (spec (udecl->mdecl decl)))
    (cons* (car spec) comm (cdr spec))))

;; @deffn {Procedure} mdecl->udecl mdecl xxx
;; needed for xxx
;; @end deffn
(define* (mdecl->udecl mdecl)

  (define (make-udecl types declr)
    ;; TODO: w/ attr needed?
    `(udecl (decl-spec-list (type-spec ,types)) (init-declr ,declr)))

  (define (doit declr mdecl-tail)
    (pmatch mdecl-tail
      (((fixed-type ,name)) (make-udecl (car mdecl-tail) declr))
      (((float-type ,name)) (make-udecl (car mdecl-tail) declr))
      (((typename ,name)) (make-udecl (car mdecl-tail) declr))
      (((void)) (make-udecl (car mdecl-tail) declr))
      
      (((pointer-to) . ,rest)
       (doit `(ptr-declr (pointer) ,declr) rest))
      (((array-of ,size) . ,rest)
       (doit `(ary-declr ,declr ,size) rest))

      (((struct-ref (ident ,name))) (make-udecl (car mdecl-tail) declr))
      (((union-ref (ident ,name))) (make-udecl (car mdecl-tail) declr))
     (((struct-def (ident ,name))) (make-udecl (car mdecl-tail) declr))
      (((union-def (ident ,name))) (make-udecl (car mdecl-tail) declr))

      (,_
       (sferr "munge/mdecl->udecl missed:\n")
       (pperr mdecl-tail)
       (throw 'nyacc-error "munge/mdecl->udecl failed"))))

  (let ((name (car mdecl))
	(rest (cdr mdecl)))
    (doit `(ident ,name) rest)))


;; --- last line ---
