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
;;  unitize-decl is shallow: it only works on init-decl-list.
;;  You may need to rerun if ...

;; mspec == munged (unwrapped) declaration

(define-module (nyacc lang c99 util2)
  #:export (c99-trans-unit->udict
	    c99-trans-unit->udict/deep
	    udict-ref udict-struct-ref udict-union-ref udict-enum-ref

	    ;; generating def's dict
	    c99-trans-unit->ddict udict-enums->ddict

	    ;; munging
	    expand-typerefs
	    stripdown
	    udecl->mspec udecl->mspec/comm
	    remove-type-qual rem-specl-type-qual

	    unwrap-decl
	    canize-enum-def-list
	    fixed-width-int-names

	    ;; should be
	    ;; unitize-decl unitize-comp-decl unitize-param-decl
	    unitize-decl unitize-comp-decl unitize-param-decl
	    declr-ident declr-id decl-id
	    split-decl iter-declrs
	    split-adecl

	    clean-field-list
	    inc-keeper?

	    ;; deprecated
	    tree->udict tree->udict/deep
	    declr->ident
	    match-decl match-comp-decl match-param-decl
	    expand-decl-typerefs
	    fix-fields
	    ;; debugging
	    stripdown-1
	    tdef-splice-specl
	    tdef-splice-declr
	    )
  #:use-module (nyacc lang c99 pprint)
  #:use-module (nyacc lang util)
  #:use-module ((sxml fold) #:select (foldts foldts*))
  #:use-module (sxml match)
  #:use-module (srfi srfi-11)		; let-values
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-1)
  #:use-module (system base pmatch)
  ;; debugging:
  #:use-module (system vm trace)
  #:use-module (ice-9 pretty-print)
  )
;; undocumented Guile builtins: or-map

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
;; @code{init-declr}.  The declarations come reversed from order in file!
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
;; @end deffn
;; @noindent
;; If @var{tree} is not a pair then @var{seed} -- or @code{'()} -- is returned.
;; The inc-filter @var{f} is either @code{#t}, @code{#f} or predicate procedure
;; of one argument, the include path, to indicate whether it should be included
;; in the dictionary.
(define* (c99-trans-unit->udict tree #:optional (seed '()) #:key inc-filter)
  (if (pair? tree)
      ;;(fold-right			; I think this should be fold(-left)
      (fold
       (lambda (tree seed)
	 (cond
	  ((eqv? (sx-tag tree) 'decl)
	   (unitize-decl tree seed))
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
;; REPLACE WITH split-adecl
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

;; @deffn {Procedure} unitize-decl decl [seed] [#:expand-enums #f] => seed
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
;;	  (name-dict (fold unitize-decl-1 '() (cdr sx1))))
;; @end example
;; Enum, struct and union def's have keys @code{(enum . "name")},
;; @code{(struct . "name")} and @code{(union . "name)}, respectively.
;; See @code{udict-struct-ref}, @code{udict-union-ref}, @code{udict-enum-ref}
;; and @code{udict-ref}.  This procecure is robust to already munged decls.
;; To capture enum values as globals use @code{->ddict}.
;; @*
;; Notes: not saving attributes.
;; @end deffn

(define* (unitize-decl decl #:optional (seed '()) #:key (expand-enums #f))
  
  (define (make-udecl type guts)
    `(udecl (decl-spec-list (type-spec (,type . ,guts)))))

  ;;(simple-format #t "unitize-decl ~S\n" decl)
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
    (unitize-comp-decl decl seed #:expand-enums expand-enums))
   ((eqv? (sx-tag decl) 'param-decl)
    (unitize-param-decl decl seed #:expand-enums expand-enums))
   (else seed)))

;; @deffn {Procedure} unitize-comp-decl decl [seed] [#:expand-enums #f]
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
(define* (unitize-comp-decl decl #:optional (seed '()) #:key (expand-enums #f))
  (cond
   ((not (pair? decl))
    (error "bad arg"))
   ((eqv? (sx-tag decl) 'comp-udecl)
    (acons (udecl-id decl) decl seed))
   ((eqv? (sx-tag (sx-ref decl 2)) 'comp-declr-list)
    (let-values (((tag attr spec-l declrs tail) (split-decl decl)))
      (iter-declrs 'comp-udecl attr spec-l declrs tail seed)))
   (else
    seed)))

;; @deffn {Procedure} unitize-param-decl param-decl [seed] [#:expand-enums #f]
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
(define* (unitize-param-decl decl #:optional (seed '()) #:key (expand-enums #f))
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

;; @deffn {Procedure} repl-typespec decl-spec-list repl-type-spec
;; In the decl-spec-list replace the type-specifier.
;; @end deffn
(define (repl-typespec decl-spec-list replacement)
  (fold-right
   (lambda (item seed)
     (cond ((symbol? item) (cons item seed))
	   ((eq? 'type-spec (car item)) (cons replacement seed))
	   (else (cons item seed))))
   '() decl-spec-list))

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



;; @deffn {Procedure} split-adecl decl => values
;; This routine splits a declaration (decl, udecl, comp-decl or param-decl)
;; into its constituent parts.  Attributes are currently not passed.
;; Get @code{(values tag spec-l declrs tail)}.  If the declrator is
;; already unitized you get that, else the list (w/o tag) of declarators.
;; @example
;;   (split-udecl
;;      '(udecl (decl-spec-list (typedef) (fixed-type "int"))
;;              (declr (ident "a"))
;; =>
;;   (values decl
;;           #f
;;           (decl-spec-list (typedef) (fixed-type "int")
;;           (declr (ident "a")))
;; @end example
;; @end deffn
(define (split-adecl adecl)
  (let* ((tag (sx-tag adecl))
	 (attr (sx-attr adecl))
	 (specl (sx-ref adecl 1))	; (decl-spec-list ...)
	 (declr (sx-ref adecl 2))	; *-declr|*-declr-list|comment|#f
	 (declr (if (and declr (not (eq? (sx-tag declr) 'comment))) declr #f))
	 (tail (sx-tail adecl (if declr 3 2)))) ; '()|((comment ""))
    (values tag attr specl declr tail)))

;; @deffn {Procedure} declr-list? declr
;; Determine if declr it is a list or not.
;; Often declr can be xxxx-declr-list or xxxx-declr.
;; @end deffn
(define (declr-list? declr)
  (member (sx-tag declr) '(init-declr-list comp-declr-list)))

;; === typedef expansion ===============

;; allows only one storage specifier besides typedef
;; call this (injest-in-specl orig-specl repl-specl)
(define (tdef-splice-specl orig-specl repl-specl)
  (let iter ((specl '()) (repll '()) (origl (cdr orig-specl)))
    (cond
     ((pair? repll)
      (cond
       ((equal? (car repll) '(stor-spec (typedef)))
	(iter specl (cdr repll) origl))
       ((equal? (car repll) '(stor-spec (const)))
	(iter (cons (car repll) specl) (cdr repll) origl))
       ((member (car repll) specl)	; don't duplicate other stor-spec's
	(iter specl (cdr repll) origl))
       (else 
	(iter (cons (car repll) specl) (cdr repll) origl))))
     ((pair? origl)
      (cond
       ((pmatch (car origl) ((type-spec (typename ,name)) #t) (,othersize #f))
	(iter specl (cdr repl-specl) (cdr origl))) ; now insert replacement
       ((equal? (car origl) '(stor-spec (const)))
	(iter (cons (car repl) specl) repll (cdr origl)))
       ((member (car origl) specl) ; don't duplicate "auto", "extern"
	(iter specl repll (cdr origl)))
       (else
	(iter (cons (car origl) specl) repll (cdr origl)))))
     (else
      (cons 'decl-spec-list (reverse specl))))))

(define (tdef-splice-declr orig-declr tdef-declr)
  (define (probe-declr declr)
    (sxml-match declr
      ((ident ,name)
       (sx-ref orig-declr 1))
      ((init-declr ,declr . ,rest)
       `(init-declr ,(probe-declr declr) . ,rest))
      ((comp-declr ,declr)
       `(comp-declr ,(probe-declr declr)))
      ((param-declr ,declr)
       `(param-declr ,(probe-declr declr)))
      ((array-of ,dir-declr ,array-spec)
       `(array-of ,(probe-declr dir-declr) ,array-spec))
      ((array-of ,dir-declr)
       `(array-of ,(probe-declr dir-declr)))
      ((ptr-declr ,pointer ,dir-declr)
       `(ptr-declr ,pointer ,(probe-declr dir-declr)))
      ((ftn-declr ,dir-declr . ,rest)
       `(ftn-declr ,(probe-declr dir-declr) . ,rest))
      ((scope ,declr)
       `(scope ,(probe-declr declr)))
      (,otherwise (throw 'util-error "c99/util2: unknown declarator: " declr))))
  (probe-declr tdef-declr))

;; iterate tdef-splice-declr over a declr-init-list (or equiv)
(define (tdef-splice-declr-list orig-declr-list tdef-declr)
  (sx-cons*
   (sx-tag orig-declr-list) (sx-attr orig-declr-list)
   (fold-right
    (lambda (declr seed)
      (cons (tdef-splice-declr declr tdef-declr) seed))
    '() (sx-tail orig-declr-list 1))))

;; @deffn {Procedure} compound-key type-spec-tag name
;; type-spec-tag is struct/union-ref/def
;; @example
;; (compound-key 'struct-ref) => struct
;; (compount-key 'union-ref "_foo") => (union . "_foo")
;; @end example
;; @end deffn
(define* (compound-key type-spec-tag #:optional name)
  (let ((key (case type-spec-tag
	       ((struct-ref struct-def) 'struct)
	       ((union-ref union-def) 'union))))
    (if name (cons key name) key)))

;; Replace the type-spec in @var{decl-spec-list} with @var{type-spec}.
(define (replace-type-spec decl-spec-list type-spec)
  (sx-cons*
   (sx-tag decl-spec-list) (sx-attr decl-spec-list)
   (map
    (lambda (elt) (if (eq? (sx-tag elt) 'type-spec) type-spec elt))
    (sx-tail decl-spec-list 1))))

;; declr can be xxxx-declr-list or xxxx-declr
;; This needs to be able to accept @code{#f} @var{declr}.
(define (expand-specl-typerefs specl declr udict keep)

  (define (re-expand specl declr) ;; applied after typename
    (expand-specl-typerefs specl declr udict keep))

  (define (expand-fld decl) ;; applied to fields in struct|union
    (let*-values (((tag attr specl declr tail) (split-adecl decl)))
      (expand-specl-typerefs specl declr udict keep)))

  (define (splice-typename specl declr name udict)
    (let* ((decl (assoc-ref udict name)) ; decl for typename
	   (tdef-specl (sx-ref decl 1))	 ; pec's for typename
	   (tdef-declr (sx-ref decl 2))) ; declr for typename
      (values ;; fixdd-specl fixed-declr
       (tdef-splice-specl specl tdef-specl)
       (cond
	((not declr) declr)
	((declr-list? declr) (tdef-splice-declr-list declr tdef-declr))
	(else (tdef-splice-declr declr tdef-declr))))))
  
  (let* ((tspec (and=> (sx-find 'type-spec specl) cadr))
	 (class (sx-tag tspec))		; e.g., typename, fixed-type
	 (name (sx-ref tspec 1)))	; e.g., "foo_t"
    (case class
      ((typename)
       (cond
	((member name keep) 
	 (values specl declr)) ;; keeper; don't expand
	(else
	 (call-with-values
	     (lambda () (splice-typename specl declr name udict))
	   (lambda (specl declr) (re-expand specl declr))))))
      ((struct-def union-def)
       (let* ((tag (sx-tag tspec))
	      (attr (sx-attr tspec))
	      (fld1 (sx-ref tspec 1))
	      (ident (if (eq? 'ident (sx-tag fld1)) fld1 #f))
	      (field-list (if ident (sx-ref tspec 2) fld1))
	      (orig-flds (sx-tail field-list 1))
	      (fixd-flds (map expand-fld orig-flds))
	      (fixd-field-list `(field-list ,@fixd-flds))
	      (fixd-tspec (if ident
			      (sx-cons* tag attr ident fixd-field-list)
			      (sx-cons* tag attr ident fixd-field-list)))
	      (fixd-specl (replace-type-spec specl fixd-tspec)))
	 (values fixd-specl declr)))
      ((struct-ref union-ref) ;; compound reference; replace w/ def
       (let* ((cmpd-name (and=> (sx-find 'ident tspec)
				(lambda (id) (sx-ref id 1))))
	      (cmpd-key (compound-key class cmpd-name))
	      (cmpd-decl (and cmpd-key (assoc-ref udict cmpd-key))))
	 (values
	  (if (and cmpd-key (member cmpd-key keep))
	      specl
	      (replace-type-spec specl
				 (sx-find 'type-spec (sx-ref cmpd-decl 1))))
	  declr)))
      ((enum-ref enum-def)
       ;; If not keeper, then replace enum with int.
       (let* ((type (sx-ref (sx-find 'type-spec specl) 1))
	      (name (and=> (sx-find 'ident type) cadr)))
	 (cond
	  ((and name (member `(enum . ,name) keep))
	   (values specl declr))
	  (else
	   (values
	    (repl-typespec specl `(type-spec (fixed-type "int")))
	    declr)))))
      (else (values specl declr)))))

;; @deffn {Procedure} expand-typerefs adecl udict [keep]
;; Given a declaration or component-declaration, return a udecl with all
;; typenames (not in the list @var{keep}), structs and unions, expanded,
;; and enums turned into int.
;; @example
;; typedef const int  (*foo_t)(int a, double b);
;; extern    foo_t    fctns[2];
;; =>
;; extern const int  (*fctns[2])(int a, double b);
;; @end example
;; @noindent
;; Note: @var{keep} was formally keyword argument. 
;; @end deffn
;; idea: if we have a pointer to an undefined type, then use void*
;; @*BUG HERE? if we run into a struct then the struct members have not
;; been munged into udecls.  The behavior is actually NOT DEFINED.
(define* (expand-typerefs adecl udict #:optional (keep '()))

  (define (fix-param-list param-list)
    (sx-cons*
     (sx-tag param-list) (sx-attr param-list)
     (fold-right
      (lambda (param seed)
	(cons
	 (case (sx-tag param)
	   ((param-decl)
	    (let* ((declr1 (sx-ref param 2))
		   (xparam (expand-typerefs param udict keep))
		   (declr2 (sx-ref xparam 2)) 
		   (xdeclr (fix-declr declr2))
		   (xtag (sx-tag xparam))
		   (xattr (sx-attr xparam))
		   (xspecl (sx-ref xparam 1)))
	      (cond
	       ((and (eq? xparam param) (eq? xdeclr declr1)) param)
	       (xdeclr (sx-cons* xtag xattr xspecl xdeclr (sx-tail xparam 3)))
	       (else (sx-cons* xtag xattr xspecl (sx-tail xparam 2))))))
	   ((ellipsis) param))
	 seed))
      '() (sx-tail param-list 1))))
  
  (define (fix-declr declr)
    ;;(sferr "tag=~S\n" (sx-tag declr))
    (sxml-match declr
      (#f declr)
      ((ident ,name) declr)
      
      ((init-declr ,declr1 . ,rest)
       (let ((xdeclr (fix-declr declr1)))
	 (if (eq? xdeclr declr1) declr `(init-declr ,xdeclr . ,rest))))
      ((comp-declr ,declr1)
       (let ((xdeclr (fix-declr declr1)))
	 (if (eq? xdeclr declr1) declr `(comp-declr ,xdeclr))))
      ((param-declr ,declr1)
       (let ((xdeclr (fix-declr declr1)))
	 (if (eq? xdeclr declr1) declr `(param-declr ,xdeclr))))
      ((array-of ,declr1 ,array-spec)
       (let ((xdeclr (fix-declr declr1)))
	 (if (eq? xdeclr declr1) declr `(array-of ,xdeclr ,array-spec))))
      ((array-of ,dir-declr)
       (let ((xdeclr (fix-declr dir-declr)))
	 (if (eq? xdeclr dir-declr) declr `(array-of ,xdeclr))))
      ((ptr-declr ,pointer ,dir-declr)
       (let ((xdeclr (fix-declr dir-declr)))
	 (if (eq? xdeclr dir-declr) declr `(ptr-declr ,pointer ,xdeclr))))

      ((scope ,declr1)
       (let ((xdeclr (fix-declr declr1)))
	 (if (eq? xdeclr declr1) declr `(scope ,xdeclr))))

      ;; abstract declarator and direct abstract declarator
      ((abs-declr ,pointer ,dir-abs-declr)
       (let ((xdeclr (fix-declr dir-abs-declr)))
	 (if (eq? xdeclr dir-abs-declr) declr `(abs-declr ,pointer ,xdeclr))))
      ((abs-declr (pointer))
       declr)
      ((abs-declr (pointer ,pointer-val))
       declr)
      ((abs-declr ,dir-abs-declr)
       (let ((xdeclr (fix-declr dir-abs-declr)))
	 (if (eq? xdeclr dir-abs-declr) declr `(abs-declr ,xdeclr))))

      ;; declr-scope
      ;; declr-array dir-abs-declr
      ;; declr-array dir-abs-declr assn-expr
      ;; declr-array dir-abs-declr type-qual-list
      ;; declr-array dir-abs-declr type-qual-list assn-expr
      ((declr-scope ,abs-declr)		; ( abs-declr )
       (let ((xdeclr (fix-declr abs-declr)))
	 (if (eq? xdeclr abs-declr) declr `(declr-scope ,xdeclr))))
      ((declr-array ,dir-abs-declr)	; []
       (let ((xdeclr (fix-declr dir-abs-declr)))
	 (if (eq? xdeclr dir-abs-declr) declr `(declr-array ,xdeclr))))
      ((declr-array ,dir-abs-declr (type-qual-list . ,type-quals))
       (let ((xdeclr (fix-declr dir-abs-declr)))
	 (if (eq? xdeclr dir-abs-declr) declr
	     `(declr-array ,xdeclr (type-qual-list . ,type-quals)))))
      ((declr-array ,dir-abs-declr ,assn-expr)
       (let ((xdeclr (fix-declr dir-abs-declr)))
	 (if (eq? xdeclr dir-abs-declr) declr
	     `(declr-array ,xdeclr ,assn-expr))))
      ((declr-array ,dir-abs-declr ,type-qual-list ,assn-expr)
       (let ((xdeclr (fix-declr dir-abs-declr)))
	 (if (eq? xdeclr dir-abs-declr) declr
	     `(declr-array ,xdeclr ,type-qual-list ,assn-expr))))

      ((ftn-declr ,dir-declr ,param-list)
       (let ((xdeclr (fix-declr dir-declr))
	     (xparam-list (fix-param-list param-list)))
	 (if (and (eq? xdeclr dir-declr) (eq? xparam-list param-list)) declr
	     `(ftn-declr ,xdeclr ,xparam-list))))
      ((abs-ftn-declr ,dir-declr ,param-list)
       (let ((xdeclr (fix-declr dir-declr))
	     (xparam-list (if (eq? 'param-list (sx-tag param-list))
			      (fix-param-list param-list)
			      param-list)))
	 (if (and (eq? xdeclr dir-declr) (eq? xparam-list param-list)) declr
	     `(abs-ftn-declr ,xdeclr ,xparam-list))))
      ((anon-ftn-declr ,param-list)
       (let ((xparam-list (fix-param-list param-list)))
	 (if (eq? xparam-list param-list) declr
	     `(anon-ftn-declr ,xparam-list))))
      ((init-declr-list . ,declrs)
       (let ((xdeclrs (map fix-declr declrs)))
	 (if (fold (lambda (l r seed) (and (eq? l r) seed)) #t xdeclrs declrs)
	     declr
	     `(init-declr-list . ,xdeclrs))))
      ((comp-declr-list . ,declrs)
       (let ((xdeclrs (map fix-declr declrs)))
	 (if (fold (lambda (l r seed) (and (eq? l r) seed)) #t xdeclrs declrs)
	     declr
	     `(comp-declr-list . ,xdeclrs))))
      (,otherwise (throw 'util-error "c99/util2: unknown declarator: " declr))))
    
  (let*-values (((tag attr orig-specl orig-declr tail)
		 (split-adecl adecl))
		((repl-specl repl-declr)
		 (expand-specl-typerefs orig-specl orig-declr udict keep))
		)
    (let ((repl-declr (fix-declr repl-declr)))
      (if (and (eq? orig-specl repl-specl)
	       (eq? orig-declr repl-declr))
	  adecl ;; <= unchanged; return original
	  (if repl-declr
	      (sx-cons* tag attr repl-specl repl-declr tail)
	      (sx-cons* tag attr repl-specl tail))))))

;; === enums and defines ===============

;; @deffn {Procedure} c99-trans-unit->ddict tree [seed] [#:inc-filter proc]
;; Extract the #defines from a tree as
;; @example
;; tree => (("ABC" . "repl") ("MAX" ("X" "Y") . "(X)...") ...)
;; @end example
;; @noindent
;; The entries appear in reverse order wrt how in file.
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
      (fold
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
 

;; === enum handling ===================
  
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

;; === stripdown =======================

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

;; remove type qualifiers: "const" "volatile" and "restrict"
(define (rem-specl-type-qual specl-tail)
  (remove (lambda (elt) (eq? 'type-qual (car elt))) specl-tail))
(define* (remove-type-qual udecl)
  (let ((tag (sx-tag udecl))
	(attr (sx-attr udecl))
	(specl (sx-ref udecl 1))
	(rest (sx-tail udecl 2)))
    (sx-cons* tag attr
	      (cons 'decl-spec-list (rem-specl-type-qual (sx-tail specl 1)))
	      rest)))
      
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


;; === munged specification ============

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

  (define (unwrap-pointer pointer)  ;; =>list IGNORES TYPE QUALIFIERS
    ;;(sferr "unwrap-pointer ~S\n" pointer)
    (sxml-match pointer
      ((pointer (type-qual-list ,type-qual ...) ,pointer)
       (cons '(pointer-to) (unwrap-pointer pointer)))
      ((pointer ,pointer) (cons '(pointer-to) (unwrap-pointer pointer)))
      ((pointer (type-qual-list ,type-qual ...)) '((pointer-to)))
      ((pointer) '((pointer-to)))
      (,otherwise
       (sferr "unwrap-pointer failed on:\n") (pperr pointer)
       (error "unwrap-pointer"))
      ))

  (define (make-abs-dummy) ;; for abstract declarator make a dummy
    (symbol->string (gensym "@")))
  (define (make-abs-dummy-tail)
    (list (make-abs-dummy)))
    
  (define* (unwrap-declr declr #:key (const #f))
    ;;(sferr "unwrap-declr:\n") (pperr declr #:per-line-prefix "  ")
    (sxml-match declr
      ((ident ,name)
       (list name))
      ((init-declr ,item)
       (unwrap-declr item #:const const))

      ((array-of ,dir-declr ,size)
       (cons `(array-of ,(cnvt-array-size size)) (unwrap-declr dir-declr)))
      ((array-of ,dir-declr)
       (cons `(array-of) (unwrap-declr dir-declr)))

      ((ftn-declr ,dir-declr ,param-list)
       (cons `(function-returning ,param-list) (unwrap-declr dir-declr)))
      ((abs-ftn-declr ,dir-abs-declr)
       (cons `(function-returning) (unwrap-declr dir-abs-declr)))
      ((abs-ftn-declr ,dir-abs-declr ,param-list)
       (cons `(function-returning ,param-list) (unwrap-declr dir-abs-declr)))
      ;;((anon-ftn-declr ,param-list) ???

      ((scope ,expr) (unwrap-declr expr))

      ((ptr-declr ,pointer ,dir-declr)
       (let ((res (append (unwrap-pointer pointer) (unwrap-declr dir-declr))))
	 (if const (cons '(const) res) res)))

      ;; abstract declarator and direct abstract declarator
      ((abs-declr ,pointer ,dir-abs-declr)
       (append (unwrap-pointer pointer) (unwrap-declr dir-abs-declr)))
      ((abs-declr (pointer))
       (append (unwrap-pointer (sx-ref declr 1)) (make-abs-dummy-tail)))
      ((abs-declr (pointer ,pointer-val))
       (cons* '(pointer-to) (make-abs-dummy-tail)))
      ((abs-declr ,dir-abs-declr)
       (unwrap-declr dir-abs-declr))

      ;; declr-scope
      ;; declr-array dir-abs-declr
      ;; declr-array dir-abs-declr assn-expr
      ;; declr-array dir-abs-declr type-qual-list
      ;; declr-array dir-abs-declr type-qual-list assn-expr
      ((declr-scope ,abs-declr)		; ( abs-declr )
       (unwrap-declr abs-declr))
      ((declr-array ,dir-abs-declr)	; []
       (cons '(array-of "") (make-abs-dummy-tail))) ;; ???
      ((declr-array ,dir-abs-declr (type-qual-list . ,type-quals))
       ;; TODO: deal with "const" type-qualifier
       (cons '(array-of "") (make-abs-dummy-tail)))
      ((declr-array ,dir-abs-declr ,assn-expr)
       (cons `(array-of ,assn-expr) (unwrap-declr dir-abs-declr)))
      ((declr-array ,dir-abs-declr ,type-qual-list ,assn-expr)
       ;; TODO: deal with "const" type-qualifier
       (cons `(array-of ,assn-expr) (unwrap-declr dir-abs-declr)))

      ((comp-declr ,item) (unwrap-declr item))
      ((param-declr ,item) (unwrap-declr item))

      (,otherwise
       (sferr "unwrap-declr missed:\n")
       (pperr otherwise)
       (error "c99/util2: udecl->mspec failed")
       #f)))

  (let* (;;(decl-dict (if (pair? rest) (car rest) '()))
	 (specl (sx-ref decl 1))	; decl-spec-list
	 (tspec (cadr specl))		; type-spec
	 (const (and=> (sx-ref specl 2)	; const pointer ???
		       (lambda (sx) (equal? (sx-ref sx 1) "const"))))
 	 (declr (or (sx-ref decl 2)	; param-decl -- e.g., f(int)
		    `(ident ,(make-abs-dummy))))
	 (comm (sx-ref decl 3))		; comment
	 (m-specl (unwrap-specl specl))
	 (m-declr (unwrap-declr declr #:const const))
	 (m-decl (reverse (cons m-specl m-declr))))
    ;;(sferr "decl:\n") (pperr decl)
    ;;(sferr "declr:\n") (pperr declr)
    ;;(sferr "r-mspec: ~S\n" (cons m-specl m-declr))
    m-decl))

(define* (udecl->mspec/comm decl #:key (def-comm ""))
  (let* ((comm (or (and=> (sx-ref decl 3) cadr) def-comm))
	 (spec (udecl->mspec decl)))
    (cons* (car spec) comm (cdr spec))))

;; === deprecated ====================

(define tree->udict c99-trans-unit->udict)
(define tree->udict/deep c99-trans-unit->udict/deep)
(define unwrap-decl unitize-decl)
(define match-comp-decl unitize-comp-decl)
(define match-param-decl unitize-param-decl)
(define expand-decl-typerefs expand-typerefs)
(define declr->ident declr-ident)
(define (fix-fields flds) (cdr (clean-field-list `(field-list . ,flds))))

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

  (let* ((xdecl (expand-decl-typerefs udecl decl-dict keep))
	 (tag (sx-tag xdecl))
	 (attr (sx-attr xdecl))
	 (specl (sx-ref xdecl 1))
	 (declr (sx-ref xdecl 2))
	 (specl1 (foldts fsD fsU fsH '() specl)))
    (list tag specl1 declr)))


;; --- last line ---
