;;; nyacc/lang/c99/util2.scm - C processing code
;;; call this munge.scm ?
;;; 
;;; Copyright (C) 2015-2017 Matthew R. Wette
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

;; utilities for processing output trees

;; KEEPING STRUCTS ENUMS etc
;; if have typename and want to keep it, then change
;;   (typename "foo_t")
;; to
;;   (typename (@ (base "struct")) "foo_t")

;; ALSO
;;  (make-proxy comp-decl) => udecl
;;  (revert-proxy decl) => comp-decl

;; NOTE
;;  stripdown no longer deals with typeref expansion
;; use
;;  expand-typerefs, then stripdown, then udecl->mspec
;;
;; NOTE
;;  munge-decl is shallow: it only works on init-decl-list.
;;  You may need to rerun if ...

(define-module (nyacc lang c99 util2)
  #:export (c99-trans-unit->udict
	    c99-trans-unit->udict/deep
	    udict-ref udict-struct-ref udict-union-ref udict-enum-ref

	    c99-trans-unit->ddict udict-enums->ddict

	    expand-typerefs
	    stripdown
	    udecl->mspec udecl->mspec/comm

	    unwrap-decl
	    canize-enum-def-list
	    fixed-width-int-names

	    munge-decl munge-comp-decl munge-param-decl
	    declr-ident declr-id decl-id
	    split-decl iter-declrs

	    clean-field-list
	    inc-keeper?

	    ;; deprecated
	    tree->udict tree->udict/deep
	    declr->ident
	    match-decl match-comp-decl match-param-decl
	    expand-decl-typerefs
	    fix-fields
	    ;; debuggins
	    stripdown-1 stripdown-2
	    )
  #:use-module (nyacc lang c99 pprint)
  #:use-module (nyacc lang util)
  #:use-module ((sxml fold) #:select (foldts foldts*))
  #:use-module (sxml match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11) ; let-values
  ;;#:use-module (system base pmatch)
  #:use-module (nyacc lang c99 pprint)
  #:use-module (ice-9 pretty-print)
  #:use-module (system base pmatch)
  )

(define (sf fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pp exp) (pretty-print exp (current-error-port)))

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

;; mspec is
;; ("foo" (pointer-to) (array-of 3) (fixed-type "unsigned int"))
;; which can be converted to
;; ("(*foo) (array-of 3) (fixed-type "unsigned int"))
;; which can be converted to
;; (("((*foo)[0])" (fixed-type "unsigned int"))
;;  ("((*foo)[1])" (fixed-type "unsigned int"))
;;  ("((*foo)[2])" (fixed-type "unsigned int"))

;; may need to replace (typename "int32_t") with (fixed-type "int32_t")

;; @deffn {Procedure} ink-keeper? tree inc-filter => #f| tree
;; This is a helper.  @var{inc-filter} is @code{#t}, @code{#f} or a
;; precicate procedure, which given the name of the file, determines if
;; it should be processed.
;; @end deffn
(define (inc-keeper? tree filter)
  (if (and (eqv? (sx-tag tree) 'cpp-stmt)
	   (eqv? (sx-tag (sx-ref tree 1)) 'include)
	   (pair? (sx-ref (sx-ref tree 1) 2))
	   (if (procedure? filter)
	       (filter (let ((arg (sx-ref (sx-ref tree 1) 1)))
			 (substring arg 1 (1- (string-length arg)))))
	       filter))
      (sx-ref (sx-ref tree 1) 2)
      #f))

;; @deffn {Procedure} c99-trans-unit->udict tree [seed] [#:inc-filter f]
;; @deffnx {Procedure} c99-trans-unit->udict/deep tree [seed]
;; Convert a C parse tree into a assoc-list of global names and definitions.
;; This will unwrap @code{init-declr-list} into list of decls w/
;; @code{init-declr}.
;; @example
;; BUG: need to add struct and union defn's: struct foo { int x; };
;; how to deal with this
;; lookup '(struct . "foo"), "struct foo", ???
;; wanted "struct" -> dict but that is not great
;; solution: munge-decl => '(struct . "foo") then filter to generate
;; ("struct" ("foo" . decl) ("bar" . decl) ...)
;; ("union" ("bar" . decl) ("bar" . decl) ...)
;; ("enum" ("" . decl) ("foo" . decl) ("bar" . decl) ...)
;; @end example
;; So globals could be in udict, udefs or anon-enum.
;; @example
;; What about anonymous enums?  And enums in general?
;; Anonmous enum should be expaneded into 
;; @end example
;; @end deffn
;; @noindent
;; If @var{tree} is not a pair then @var{seed} -- or @code{'()} -- is returned.
;; The inc-filter @var{f} is either @code{#t}, @code{#f} or predicate procedure
;; of one argument, the include path, to indicate whether it should be included
;; in the dictionary.
(define* (c99-trans-unit->udict tree #:optional (seed '()) #:key inc-filter)
  #;(define (inc-keeper? tree) ;; pull in this (cpp-stmt (include ...)) ?
    (if (and (eqv? (sx-tag tree) 'cpp-stmt)
	     (eqv? (sx-tag (sx-ref tree 1)) 'include))
	(if (procedure? filter)
	    (filter (let ((arg (sx-ref (sx-ref tree 1) 1)))
		      (substring arg 1 (1- (string-length arg)))))
	    filter)
	#f))
  (if (pair? tree)
      (fold-right
       (lambda (tree seed)
	 (cond
	  ((eqv? (sx-tag tree) 'decl)
	   (munge-decl tree seed))
	  #;((inc-keeper? tree inc-filter)
	   (c99-trans-unit->udict (sx-ref (sx-ref tree 1) 2) seed
				  #:inc-filter inc-filter))
	  ((inc-keeper? tree inc-filter) =>
	   (lambda (tree)
	     (c99-trans-unit->udict tree seed #:inc-filter inc-filter)))
	  (else seed)))
       seed
       (cdr tree))
      seed))
(define (c99-trans-unit->udict/deep tree)
  (c99-trans-unit->udict tree #:inc-filter #t))

;; @deffn {Procedure} split-decl decl => values
;; This routine splits a declaration (or comp-decl or param-decl) into
;; its constituent parts.  Attributes are currently not passed.
;; Get @code{(values tag spec-l declrs tail)}.  If the declrator is
;; already unitized you get that, else the list (w/o tag) of declarators.
;; @example
;;   (split-decl
;;      '(decl (decl-spec-list (typedef) (fixed-type "int")
;;             (init-declr-list (declr (ident "a")) (declr (ident "b")))))
;; =>
;;   (values decl #f (type-spec-list (typedef) (fixed-type "int")
;;           ((declr (ident "a")) (declr (ident "b"))) ())
;; @end example
;; @end deffn
(define (split-decl decl)
  (let* ((tag (sx-tag decl))
	 (attr (sx-attr decl))
	 (spec-l (sx-ref decl 1))	; (decl-spec-list ...)
	 (dclr-l (sx-ref decl 2))	; (init-declr-list ...)
	 (declrs (if (and dclr-l (not (eq? (sx-tag dclr-l) 'comment)))
		     (cdr dclr-l) #f))
	 (tail (sx-tail decl (if declrs 3 2))))
    (values tag attr spec-l declrs tail)))

;; @deffn {Procedure} iter-declrs tag specl declrs tail seed
;; This is a support procedure for the munge routines.  If no decl'rs
;; then @var{declrs} should be #f.
;; @end deffn
(define (iter-declrs tag attr specl declrs tail seed)
  (cond
   ((not declrs) seed)
   (attr
    (let iter ((declrs declrs))
      (if (null? declrs) seed
	  (acons (declr-id (car declrs))
		 (cons* tag attr specl (car declrs) tail)
		 (iter (cdr declrs))))))
   (else
    (let iter ((declrs declrs))
      (if (null? declrs) seed
	  (acons (declr-id (car declrs))
		 (cons* tag specl (car declrs) tail)
		 (iter (cdr declrs))))))))

;; @deffn {Procedure} munge-decl decl [seed] [#:expand-enums #f] => seed
;; This is a fold iterator to used by @code{c99-trans-unit->udict}.  It
;; converts the multiple @code{init-declr} items in an @code{init-declr-list}
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
;; The @code{/deep} version will plunge into cpp-includes.
;; Here we generate a dictionary of all declared items in a file:
;; @example
;; (let* ((sx0 (with-input-from-file src-file parse-c))
;;	  (sx1 (merge-inc-trees! sx0))
;;	  (name-dict (fold munge-decl-1 '() (cdr sx1))))
;; @end example
;; Enum, struct and union def's have keys @code{(enum . "name")},
;; @code{(struct . "name")} and @code{(union . "name)}, respectively.
;; See @code{udict-struct-ref}, @code{udict-union-ref}, @code{udict-enum-ref}
;; and @code{udict-ref}.  This procecure is robust to already munged decls.
;; To capture enum values as globals use @code{->ddict}.
;; @*
;; Notes: not saving attributes.
;; @end deffn

(define* (munge-decl decl #:optional (seed '()) #:key (expand-enums #f))
  
  (define (make-udecl type guts)
    `(udecl (decl-spec-list (type-spec (,type . ,guts)))))

  ;;(simple-format #t "munge-decl ~S\n" decl)
  (cond
   ((not (pair? decl))
    (error "bad arg"))
   ((eqv? (sx-tag decl) 'udecl)
    (acons (udecl-id decl) decl seed))
   
   ((eqv? (sx-tag decl) 'decl)
    (let*-values (((tag attr spec-l declrs tail) (split-decl decl))
		  ((tag) (values 'udecl)))
      (sxml-match spec-l
	;; Caution: We are not parsing or saving all spec-items here.
	((decl-spec-list
	  (type-spec (struct-def (ident ,name) . ,rest2) . ,rest1))
	 (iter-declrs tag #f spec-l declrs tail
		      (acons `(struct . ,name)
			     (make-udecl 'struct-def `((ident ,name) . ,rest2))
			     seed)))
	((decl-spec-list
	  (type-spec (struct-def . ,rest2) . ,rest1))
	 (iter-declrs tag #f spec-l declrs tail seed))
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (struct-def (ident ,name) . ,rest2) . ,rest1))
	 (iter-declrs tag #f spec-l declrs tail
		      (acons `(struct . ,name)
			     (make-udecl 'struct-def `((ident ,name) . ,rest2))
			     seed)))
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (struct-def . ,rest2) . ,rest1))
	 (iter-declrs tag #f spec-l declrs tail seed))

	;; unions
	((decl-spec-list
	  (type-spec (union-def (ident ,name) . ,rest2) . ,rest1))
	 (iter-declrs tag #f spec-l declrs tail
		      (acons `(union . ,name)
			     (make-udecl 'union-def `((ident ,name) . ,rest2))
			     seed)))
	((decl-spec-list
	  (type-spec (union-def . ,rest2) . ,rest1))
	 (iter-declrs tag #f spec-l declrs tail seed))
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (union-def (ident ,name) . ,rest2) . ,rest1))
	 (iter-declrs tag #f spec-l declrs tail
		      (acons `(union . ,name)
			     (make-udecl 'union-def `((ident ,name) . ,rest2))
			     seed)))
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (union-def . ,rest2) . ,rest1))
	 (iter-declrs tag #f spec-l declrs tail seed))

	;; enums
	((decl-spec-list
	  (type-spec (enum-def (ident ,name) . ,rest2) . ,rest1))
	 (iter-declrs tag #f spec-l declrs tail
		      (acons `(enum . ,name)
			     (make-udecl 'enum-def `((ident ,name) . ,rest2))
			     seed)))
	((decl-spec-list
	  (type-spec (enum-def . ,rest2) . ,rest1))
	 (iter-declrs tag #f spec-l declrs tail
		      (acons `(enum . "*anon*") (make-udecl 'enum-def rest2)
			     seed)))
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (enum-def (ident ,name) . ,rest2) . ,rest1))
	 (iter-declrs tag #f spec-l declrs tail
		      (acons `(enum . ,name)
			     (make-udecl 'enum-def `((ident ,name) . ,rest2))
			     seed)))
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (enum-def . ,rest2) . ,rest1))
	 (iter-declrs tag #f spec-l declrs tail
		      (acons `(enum . "*anon*") (make-udecl 'enum-def rest2)
			     seed)))

	(,otherwise
	 (iter-declrs tag #f spec-l declrs tail seed)))))
   
   ((eqv? (sx-tag decl) 'comp-udecl)
    (acons (udecl-id decl) decl seed))
   ((eqv? (sx-tag decl) 'comp-decl)
    (munge-comp-decl decl seed #:expand-enums expand-enums))
   ((eqv? (sx-tag decl) 'param-decl)
    (munge-param-decl decl seed #:expand-enums expand-enums))
   (else seed)))

;; @deffn {Procedure} munge-comp-decl decl [seed] [#:expand-enums #f]
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
;; This is coded to be used with fold-right in order to preserve order
;; in @code{struct} and @code{union} field lists.
;; @end deffn
(define* (munge-comp-decl decl #:optional (seed '()) #:key (expand-enums #f))
  (cond
   ;;((not (pair? decl)) (error "bad arg"))
   ;;((not (eqv? (sx-tag decl) 'comp-decl)) (cons decl seed))
   ((not (eqv? (sx-tag (sx-ref decl 2)) 'comp-declr-list))
    (acons (udecl-id decl) decl seed))
   (else
    (let-values (((tag attr spec-l declrs tail) (split-decl decl)))
      (iter-declrs 'comp-udecl attr spec-l declrs tail seed)))))
(define* (OLD-munge-comp-decl decl seed #:key (expand-enums #f))
  (cond
   ;;((not (pair? decl)) (error "bad arg"))
   ;;((not (eqv? (sx-tag decl) 'comp-decl)) (cons decl seed))
   ((not (eqv? (sx-tag (sx-ref decl 2)) 'comp-declr-list))
    (acons (declr-id (sx-ref decl 2)) decl seed))
   (else
    (let* ((tag (sx-ref decl 0))
	   (attr (sx-attr decl))
	   (spec (sx-ref decl 1))	; (type-spec ...)
	   (cd-l (sx-ref decl 2))	; (comp-declr-list ...)
	   (tail (sx-tail decl 3)))	; opt comment, different here
      (fold-right
       (lambda (declr seed)
	 (let ((name (declr-id declr)))
	   (if attr
	       (acons name (cons* 'comp-udecl attr spec declr tail) seed)
	       (acons name (cons* 'comp-udecl spec declr tail) seed))))
       seed (cdr cd-l))))))

;; @deffn {Procedure} munge-param-decl param-decl [seed] [#:expand-enums #f]
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
(define* (munge-param-decl decl #:optional (seed '()) #:key (expand-enums #f))
  (if (not (eqv? 'param-decl (car decl))) seed
      (let* ((tag (sx-ref decl 0))
	     (attr (sx-attr decl))
	     (spec (sx-ref decl 1))	; (type-spec ...)
	     (declr (sx-ref decl 2))	; (param-declr ...)
	     (ident (declr-ident declr))
	     (name (cadr ident)))
	(acons name decl seed))))
	
;; @deffn {Procedure} declr-ident declr => (ident "name")
;; Given a declarator, aka @code{init-declr}, return the identifier.
;; This is used by @code{trans-unit->udict}.
;; @end deffn
(define (declr-ident declr)
  (sxml-match declr
    ((ident ,name) declr)
    ((init-declr ,declr . ,rest) (declr-ident declr))
    ((comp-declr ,declr) (declr-ident declr))
    ((param-declr ,declr) (declr-ident declr))
    ((array-of ,dir-declr ,array-spec) (declr-ident dir-declr))
    ((array-of ,dir-declr) (declr-ident dir-declr))
    ((ptr-declr ,pointer ,dir-declr) (declr-ident dir-declr))
    ((ftn-declr ,dir-declr ,rest ...) (declr-ident dir-declr))
    ((scope ,declr) (declr-ident declr))
    (,otherwise (throw 'util-error "c99/util2: unknown declarator: " declr))))
(define declr->ident declr-ident)

;; @deffn {Procedure} declr-id decl => "name"
;; This extracts the name from the return value of @code{declr-ident}.
;; @end deffn
(define (declr-id declr)
  (and=> (declr-ident declr) cadr))

;; @deffn {Procedure} udecl-id udecl => string
;; generate the name 
;; @end deffn
(define (udecl-id udecl)
  ;; must be udecl w/ name
  (declr-id (sx-ref udecl 2)))

;; like member but returns first non-declr of type in dict
(define (non-declr type udict)
  (let iter ((dict udict))
    (cond ((null? dict) '())
	  ((and (pair? (caar dict)) (eqv? type (caaar dict))) dict)
	  (else (iter (cdr dict))))))

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
      (let iter ((dict (non-declr 'enum udict)))
	(cond
	 ((null? dict) #f)
	 ((enum-decl-val (cdar dict) name) =>
	  (lambda (val) (gen-enum-udecl name val)))
	 (else (iter (non-declr 'enum (cdr dict))))))))
(define (udict-struct-ref udict name)
  (assoc-ref udict `(struct . ,name)))
(define (udict-union-ref udict name)
  (assoc-ref udict `(union . ,name)))
(define* (udict-enum-ref udict name)
  (assoc-ref udict `(enum . ,name)))
(define* (udict-enum-val udict name)
  (let iter ((dict (non-declr 'enum udict)))
    (cond ((null? dict) #f)
	  ((enum-decl-val (cdar dict) name))
	  (else (iter (non-declr 'enum (cdr dict)))))))

;; @deffn {Variable} fixed-width-int-names
;; This is a list of standard integer names (e.g., @code{"uint8_t"}).
;; @end deffn
(define fixed-width-int-names
  '("int8_t" "uint8_t" "int16_t" "uint16_t"
    "int32_t" "uint32_t" "int64_t" "uint64_t"))

;; @deffn {Procedure} typedef-decl? decl)
;; @end deffn
(define (typedef-decl? decl)
  (sxml-match decl
    ((decl (decl-spec-list (stor-spec (typedef)) . ,r1) . ,r2) #t)
    (,otherwise #f)))

;; @deffn {Procedure} splice-declarators orig-declr tdef-declr => 
;; Splice the original declarator into the typedef declarator.
;; This is a helper for @code{expand-*-typename-ref} procecures.
;; @end deffn
(define (splice-declarators orig-declr tdef-declr)
  
  (define (fD seed tree)		; => (values seed tree)
    (sxml-match tree
      ((param-list . ,rest) (values tree '())) ; don't process
      ((ident ,name) (values (reverse (cadr orig-declr)) '())) ; replace
      (,otherwise (values '() tree))))

  (define (fU seed kseed tree)
    (let ((ktree (case (car kseed)
		   ((param-list ident) kseed)
		   (else (reverse kseed)))))
      (if (null? seed) ktree (cons ktree seed))))

  (define (fH seed tree)
    (cons tree seed))

  ;; This cons transfers the tag from orig-declr to the result.
  (cons
   (car orig-declr)			; init-declr or comp-declr
   (cdr (foldts* fD fU fH '() tdef-declr)))) ; always init-declr


;; @deffn {Procedure} repl-typespec decl-spec-list replacement
;; This is a helper for expand-typerefs
;; @end deffn
(define (repl-typespec decl-spec-list replacement)
  (fold-right
   (lambda (item seed)
     (cond ((symbol? item) (cons item seed))
	   ((eqv? 'type-spec (car item))
	    (if (pair? (car replacement))
		(append replacement seed)
		(cons replacement seed)))
	   (else (cons item seed))))
   '() decl-spec-list))

;; @deffn {Procedure} expand-typerefs udecl udecl-dict [#:keep '()]
;; Given a declaration or component-declaration, return a udecl with all
;; typenames (not in @var{keep}), structs and unions, expanded, and
;; enums turned into int.
;; @example
;; typedef const int  (*foo_t)(int a, double b);
;; extern    foo_t    fctns[2];
;; =>
;; extern const int  (*fctns[2])(int a, double b);
;; @end example
;; @end deffn
;; idea: if we have a pointer to an undefined type, then use void*
;; BUG HERE: if we run into a struct then the struct members have not
;; been munged into udecls.  The behavior is actually NOT DEFINED.
(define* (expand-typerefs udecl udecl-dict #:key (keep '()))
  ;; ??? add (init-declr-list) OR having predicate (has-init-declr? decl)
  (let* ((tag (sx-tag udecl))		; decl or comp-decl
	 (attr (sx-attr udecl))		; (@ ...)
	 (specl (sx-ref udecl 1))	; decl-spec-list
	 (declr (or (sx-find 'init-declr udecl) ; declarator
		    (sx-find 'comp-declr udecl)
		    (sx-find 'param-declr udecl)))
	 (tail (if declr (sx-tail udecl 3) (sx-tail udecl 2))) ; opt comment
	 (tspec (cadr (sx-find 'type-spec specl))))
    (case (car tspec)
      ((typename)
       (cond
	((member (cadr tspec) keep) udecl)
	(else ;; splice in the typedef
	 (let* ((name (sx-ref tspec 1))
		(decl (or (assoc-ref udecl-dict name) ; decl for typename
			  (throw 'c99-error "util2 decl error")))
		(tdef-specl (sx-ref decl 1)) ; decl-spec-list for typename
		(tdef-declr (sx-ref decl 2)) ; init-declr for typename
		;; splice the typedef specifiers into target:
		(fixd-specl (repl-typespec specl (sx-tail tdef-specl 2)))
		(fixd-declr (splice-declarators declr tdef-declr))
		(fixed-udecl (cons* tag fixd-specl fixd-declr tail)))
	   (expand-typerefs fixed-udecl udecl-dict #:keep keep)))))

      ((struct-ref union-ref)
       ;; Still more to think about here.
       ;; If the ref has an associated def, then replace with that.
       (let* ((ident (cadr tspec))
	      (name (cadr ident))
	      (def (case (car tspec)
		     ((struct-ref) (udict-struct-ref udecl-dict name))
		     ((union-ref) (udict-union-ref udecl-dict name)))))
	 (if def
	     (expand-typerefs `(udecl ,(cadr def) ,declr . ,tail)
			      udecl-dict #:keep keep)
	     udecl)))

      ((struct-def union-def)
       (let* ((ident (sx-find 'ident tspec))
	      (field-list (sx-find 'field-list tspec))
	      (orig-flds (cdr field-list))
	      (unit-flds (map cdr (fold-right munge-comp-decl '() orig-flds)))
	      (fixd-flds (map
			  (lambda (fld)
			    (expand-typerefs fld udecl-dict #:keep keep))
			  unit-flds))
	      (fixd-tspec
	       (if (and ident #t)
		   `(type-spec (struct-def ,ident (field-list ,@fixd-flds)))
		   `(type-spec (struct-def (field-list ,@fixd-flds)))))
	      (fixd-specl (repl-typespec specl fixd-tspec)))
	 (if declr (cons* tag fixd-specl declr tail)
	     (cons* tag fixd-specl tail))))
      
      ((OLD-enum-def)
       ;;(pretty-print udecl)
       ;; enums should expand to int unless keeper
       (let* ((ident (sx-find 'ident tspec))
	      (enum-def-list (sx-find 'enum-def-list tspec))
	      (fixd-def-list (canize-enum-def-list enum-def-list))
	      (fixd-tspec
	       (if (and ident #t)
		   `(type-spec (enum-def ,ident ,fixd-def-list))
		   `(type-spec (fixed-type "int"))))
	      (fixd-specl (repl-typespec specl fixd-tspec))
	      (fixed-decl (cons* tag fixd-specl declr tail)))
	 fixed-decl))

      ((OLD-enum-ref)
       (let* ((ident (cadr tspec))
	      (name (cadr ident))
	      (def (udict-enum-ref udecl-dict name)))
	 (if def
	     (expand-typerefs `(udecl ,(cadr def) ,declr . ,tail)
			      udecl-dict #:keep keep)
	     udecl)))

      ((enum-def)
       (let* ((ident (sx-find 'ident tspec)))
	 (if (and ident (member `(enum . ,(cadr ident)) keep))
	     udecl
	     (cons* tag
		    '(decl-spec-list (type-spec (fixed-type "int")))
		    declr tail))))

      ((enum-ref)
       (let* ((ident (cadr tspec))
	      (name (cadr ident)))
	 (if (member `(enum . ,name) keep)
	     udecl
	     (cons* tag
		    '(decl-spec-list (type-spec (fixed-type "int")))
		    declr tail))))

      (else
       udecl))))
(define expand-decl-typerefs expand-typerefs) ;; deprecated

;; === enums and defines 

;; @deffn {Procedure} c99-trans-unit->ddict tree [seed] [#:inc-filter proc]
;; Extract the #defines from a tree as
;; @example
;; tree => (("ABC" . "repl") ("MAX" ("X" "Y") . "(X)...") ...)
;; @end example
;; @end deffn
(define* (c99-trans-unit->ddict tree #:optional (seed '()) #:key inc-filter)
  (define (def? tree)
    (if (and (eq? 'cpp-stmt (sx-tag tree))
	     (eq? 'define (sx-tag (sx-ref tree 1))))
	(can-def-stmt (sx-ref tree 1))
	#f))
  (define (can-def-stmt defn)
    (let* ((name (car (assq-ref defn 'name)))
	   (args (assq-ref defn 'args))
	   (repl (car (assq-ref defn 'repl))))
      (cons name (if args (cons args repl) repl))))
  (if (pair? tree)
      (fold-right
       (lambda (tree seed)
	 (cond
	  ((def? tree) =>
	   (lambda (def-stmt)
	     (cons def-stmt seed)))
	  ((inc-keeper? tree inc-filter) =>
	   (lambda (tree)
	     (c99-trans-unit->ddict tree seed #:inc-filter inc-filter)))
	  (else seed)))
       seed
       (cdr tree))
      seed))

;; @deffn {Procedure} udict-enums->ddict [seed] => defs
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
(define* (udict-enums->ddict udict #:optional (seed '()))
  (define (gen-nvl enum-def-list)
    (map
     (lambda (def)
       (pmatch def
	 ((enum-defn (ident ,n) (p-expr (fixed ,v)))
	  (cons n v))
	 ((enum-defn (ident ,n) (neg (p-expr (fixed ,v))))
	  (cons n (string-append "-" v)))
	 (,otherwise (error "gen-name-val-l" def))))
     (cdr (canize-enum-def-list enum-def-list))))
  (append
   seed
   (fold-right
    (lambda (pair seed)
      (if (and (pair? (car pair)) (eq? 'enum (caar pair)))
	  (let* ((specl (caddr pair)) (tspec (car (assq-ref specl 'type-spec))))
	    (if (eq? 'enum-def (car tspec))
		(append (gen-nvl (assq 'enum-def-list (cdr tspec))) seed)
		seed))
	  seed))
    '()
    udict)))
 

;; === enum handling ...
  
;; @deffn {Procedure} canize-enum-def-list
;; Fill in constants for all entries of an enum list.
;; Expects @code{(enum-def-list (...))} ???
;; @end deffn
(define (canize-enum-def-list enum-def-list)
  (define (get-used edl)
    (fold
     (lambda (def seed)
       (sxml-match def
	 ((enum-defn (ident ,name) (p-expr ,num) . ,rest)
	  (cons (string->number (cadr num)) seed))
	 ((enum-defn (ident ,name) (neg (p-expr ,num)) . ,rest)
	  (cons (- (string->number (cadr num))) seed))
	 (,otherwise seed)))
     '() edl))
  (let ((used (get-used (cdr enum-def-list))))
    (let iter ((rez '()) (ix 0) (edl (cdr enum-def-list)))
      (cond
       ((null? edl) (cons (car enum-def-list) (reverse rez)))
       ((sxml-match (car edl)
	  ((enum-defn (ident ,name) (p-expr ,num) . ,rest) #t)
	  ((enum-defn (ident ,name) (neg (p-expr ,num)) . ,rest) #t)
	  (,otherwise #f))
	(iter (cons (car edl) rez) ix (cdr edl)))
       (else
	(let* ((ix (let iter ((ix ix)) (if (memq ix used) (iter (1+ ix)) ix)))
	       (is (number->string ix)))
	  (iter (cons (append (car edl) `((p-expr (fixed ,is)))) rez)
		(1+ ix) (cdr edl))))))))

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
  (let iter ((el (cdr (canize-enum-def-list enum-def-list))))
    ;;(simple-format #t "~S\n" el)
    (cond
     ((null? el) #f)
     ((not (eqv? 'enum-defn (caar el))) (iter (cdr el)))
     ((string=? name (cadr (cadar el))) (cadadr (caddar el)))
     (else (iter (cdr el))))))

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

;; === enum handling ...

;;@deffn {Procedure} stripdown-1 udecl decl-dict [options]=> decl
;; This is deprecated.
;; 1) remove stor-spec
;; 2) expand typenames
;; @example
;; typedef int *x_t;
;; x_t a[10];
;; (spec (typename x_t)) (init-declr (array-of 10 (ident a)))
;; (spec (typedef) (fixed-type "int")) (init-declr (pointer) (ident "x_t"))
;; =>
;; (udecl (decl-spec-list (type-spec ...) ... (type-qual "const"))
;;        (init-declr (ptr-declr (pointer ...)
;; @end example
;; @end deffn
(define* (stripdown-1 udecl decl-dict #:key (keep '()))

  ;;(define strip-list '(stor-spec type-qual comment))
  (define strip-list '(stor-spec type-qual))

  (define (fsD seed tree)
    '())

  (define (fsU seed kseed tree)
    (cond
     ((eqv? (sx-tag tree) 'stor-spec) seed)
     ((eqv? (sx-tag tree) 'type-qual) seed)
     ((null? seed) (reverse kseed))
     (else (cons (reverse kseed) seed))))
	
  (define (fsH seed tree)
    (cons tree seed))

  (let* ((xdecl (expand-decl-typerefs udecl decl-dict #:keep keep))
	 (tag (sx-tag xdecl))
	 (attr (sx-attr xdecl))
	 (specl (sx-ref xdecl 1))
	 (declr (sx-ref xdecl 2))
	 (specl1 (foldts fsD fsU fsH '() specl)))
    (list tag specl1 declr)))

(define* (stripdown-declr declr #:key const-ptr)
  (define (fD seed tree) '())

  (define (fU seed kseed tree)
    (cond
     ((null? seed) (reverse kseed))
     ((eqv? (sx-tag tree) 'stor-spec) seed)
     ((eqv? (sx-tag tree) 'type-qual)
      (if (and const-ptr (string=? (sx-ref tree 1) "const"))
	  (cons (reverse kseed) seed)
	  seed))
     (else (cons (reverse kseed) seed))))
	
  (define (fH seed tree)
    (cons tree seed))
   
  (foldts fD fU fH '() declr))

;; @deffn {Procedure} stripdown udecl => udecl
;; Remove remove @emph{stor-spec} elements from a u-decl.
;; @example
;; =>
;; @end example
;; @end deffn
(define* (stripdown udecl #:key keep-const-ptr)
  (let* (;;(speclt (sx-tail udecl))	; decl-spec-list tail
	 (xdecl udecl)
	 (tag (sx-tag xdecl))
	 (attr (sx-attr xdecl))
	 (specl (sx-ref xdecl 1))
	 (declr (sx-ref xdecl 2))
	 (s-declr (stripdown-declr declr))
	 (is-ptr? (declr-is-ptr? declr))
	 ;;
	 (s-tag (sx-tag specl))
	 (s-attr (sx-attr specl))
	 (s-tail (strip-decl-spec-tail
		  (sx-tail specl)
		  #:keep-const? (and keep-const-ptr is-ptr?)))
	 (specl (sx-cons* s-tag s-attr s-tail)))
    ;;(pretty-print declr)
    ;;(pretty-print s-declr)
    (sx-list tag attr specl s-declr)))

(define (declr-is-ptr? declr)
  (and
   (pair? (cdr declr))
   (eqv? 'ptr-declr (caadr declr))))
    
(define* (strip-decl-spec-tail dsl-tail #:key keep-const?)
  ;;(simple-format #t "spec=tail: ~S\n" dsl-tail)
  (let iter ((dsl1 '()) (const-seen? #f) (tail dsl-tail))
    (if (null? tail)
	(reverse (if (and const-seen? keep-const?)
		     (cons '(type-qual "const") dsl1)
		     dsl1))
	(case (caar tail)
	  ((type-qual)
	   (if (string=? (cadar tail) "const")
	       (iter dsl1 #t (cdr tail))
	       (iter dsl1 const-seen? (cdr tail))))
	  ((stor-spec)
	   (iter dsl1 const-seen? (cdr tail)))
	  (else
	   (iter (cons (car tail) dsl1) const-seen? (cdr tail)))))))

;; @deffn {Procedure} udecl->mspec udecl
;; @deffnx {Procedure} udecl->mspec/comm udecl [#:def-comm ""]
;; Turn a stripped-down unit-declaration into an m-spec.  The second version
;; include a comment. This assumes decls have been run through
;; @code{stripdown}.
;; @example
;; (decl (decl-spec-list (type-spec "double"))
;;       (init-declr-list (
;;       (comment "state vector")
;; =>
;; ("x" "state vector" (array-of 10) (float "double")
;; @end example
;; @end deffn
(define (udecl->mspec decl)

  (define (cnvt-array-size size-spec)
    (with-output-to-string (lambda () (pretty-print-c99 size-spec))))

  (define (unwrap-specl specl)
    (and=> (assq-ref (sx-tail specl) 'type-spec) car))
   ;;?   (sxml-match tspec
   ;;?    ((xxx-struct-def (field-list . ,rest))
    
  (define* (unwrap-declr declr #:key (const #f))
    ;;(simple-format #t "#:const=~S (car declr)=~S\n" const (car declr))
    (sxml-match declr
      ((ident ,name)
       (list name))
      ((init-declr ,item)
       (unwrap-declr item #:const const))
      ((ptr-declr (pointer . ,r) ,dir-declr)
       (if const
	   (cons* '(const) '(pointer-to) (unwrap-declr dir-declr))
	   (cons '(pointer-to) (unwrap-declr dir-declr))))
      ((array-of ,dir-declr ,size)
       (cons `(array-of ,(cnvt-array-size size)) (unwrap-declr dir-declr)))
      ((ftn-declr ,dir-declr ,params)
       (cons '(function-returning) (unwrap-declr dir-declr)))
      ((scope ,expr)
       (unwrap-declr expr))
      ((comp-declr ,item) (unwrap-declr item))
      ((param-declr ,item) (unwrap-declr item))
      (,otherwise
       (simple-format (current-error-port) "OTHERWISE:\n")
       (pretty-print otherwise (current-error-port))
       (error "c99/util2: udecl->mspec failed")
       #f)))

  (define (find-type-spec decl-spec-list)
    (let iter ((tsl (cdr decl-spec-list)))
      (if (eqv? 'type-spec (caar tsl)) (car tsl)
	  (iter (cdr tsl))))) 
  
  (let* (;;(decl-dict (if (pair? rest) (car rest) '()))
	 (specl (sx-ref decl 1))
	 (tspec (cadr specl))		; type-spec
	 (const (and=> (sx-ref specl 2)	; const pointer ???
		       (lambda (sx) (equal? (sx-ref sx 1) "const"))))
 	 (declr (or (sx-ref decl 2) ;; param-decl -- e.g., f(void)
		    '(ident "@arg")))
	 (comm (sx-ref decl 3))
	 (m-specl (unwrap-specl specl))
	 (m-declr (unwrap-declr declr #:const const))
	 (m-decl (reverse (cons m-specl m-declr))))
    m-decl))

(define* (udecl->mspec/comm decl #:key (def-comm ""))
  (let* ((comm (or (and=> (sx-ref decl 3) cadr) def-comm))
	 (spec (udecl->mspec decl)))
    (cons* (car spec) comm (cdr spec))))

;; @deffn {Procedure} clean-field-list field-list => flds
;; Process the tagged field-list element of a struct and remove lone comments.
;; If a field following a lone comment has no code-comment, the lone comment
;; will be used.  For example,
;; @example
;;   /* foo */
;;   int x;
;; @end example
;; @noindent
;; will be treated as if it was denereed
;; @example
;;   int x; /* foo */
;; @end example
;; @noindent
;; @end deffn
(define (clean-field-list fld-list)
  (let iter ((rz '()) (cl '()) (fl (cdr fld-list)))
    (if (null? fl)
	(cons 'field-list (reverse rz)) ;; => fold-right ?
	(pmatch (car fl)
	  ((comment ...)
	   (iter rz (cons (cadar fl) cl) (cdr fl)))
	  ((,tag ,specl (comment ,comm)) ; anonymous
	   (iter rz '() (cdr fl)))
	  ((,tag ,specl)		; anonymous
	   (iter rz '() (cdr fl)))
	  ((,tag ,specl ,declr (comment ,comm))
	   (iter (cons (car fl) rz) '() (cdr fl)))
	  ((,tag ,specl ,declr)
	   (let ((cs `(comment ,(apply string-append (reverse cl)))))
	     (if (null? cl) (iter (cons (car fl) rz) '() (cdr fl))
		 (iter (cons (append (car fl) (list cs)) rz) '() (cdr fl)))))
	  (else
	   (error "util2: clean-field-list" (car fl)))))))

;; @deffn {Procedure} fix-fields flds => flds
;; This is just @code{(cdr (clean-field-list `(field-list . ,flds)))}
;; @end deffn
(define (fix-fields flds)
  (cdr (clean-field-list `(field-list . ,flds))))


;; === deprecated ====================

(define tree->udict c99-trans-unit->udict)
(define tree->udict/deep c99-trans-unit->udict/deep)
(define unwrap-decl munge-decl)
(define match-comp-decl munge-comp-decl)
(define match-param-decl munge-param-decl)

;; --- last line ---
