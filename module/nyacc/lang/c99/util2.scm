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

;; \input texinfo.tex
;; @settitle NYACC C99 Munge Module
;;
;; @headings off
;; @everyfooting @| @thispage @|
;;
;; @node Top
;; @top NYACC C99 Munge Module
;; @format
;; Matt Wette
;; March 11, 2017
;; @end format
;;
;; @heading Introduction
;;
;; The sxml parse tree can be used to provide autocoding via the
;; @code{(nyacc lang c99 munge)} module. For example, start with the
;; following C code
;; @example
;; typedef const char *string_t;
;; extern string_t cmds[10];
;; @end example
;; The nyacc output (call it @code{the-tree}) for this will be
;; @noindent
;; (trans-unit
;;   (decl (decl-spec-list
;;           (stor-spec (typedef))
;;           (type-qual "const")
;;           (type-spec (fixed-type "char")))
;;         (init-declr-list
;;           (init-declr
;;             (ptr-declr (pointer) (ident "string_t")))))
;;   (decl (decl-spec-list
;;           (stor-spec (extern))
;;           (type-spec (typename "string_t")))
;;         (init-declr-list
;;           (init-declr
;;            (array-of (ident "cmds") (p-expr (fixed "10")))))))
;; @end example
;; If we feed @code{the-tree} into @code{tree->udict} and use @code{assoc-ref}
;; to lookup @code{"cmds"} we get
;; @noindent
;; (udecl (decl-spec-list
;;          (stor-spec (extern))
;;          (type-spec (typename "string_t")))
;;        (init-declr
;;          (array-of (ident "cmds") (p-expr (fixed "10")))))
;; @end example
;; Now take this and feed into @code{expand-decl-typerefs} to get
;; @example
;; (udecl (decl-spec-list
;;          (stor-spec (extern))
;;          (type-qual "const")
;;          (type-spec (fixed-type "char")))
;;        (init-declr
;;          (ptr-declr
;;            (pointer)
;;            (array-of (ident "cmds") (p-expr (fixed "10"))))))
;; @end example
;; @noindent
;; which, when fed through the C99 pretty-printer, generates
;; @example
;; extern const char *cmds[10];
;; @end example
;; @noindent
;; Since the NYACC C99 parser captures some comments, these can be preserved
;; in the above procedure.

;; KEEPING STRUCTS ENUMS etc
;; if have typename and want to keep it, then change
;;   (typename "foo_t")
;; to
;;   (typename (@ (base "struct")) "foo_t")

;; ALSO
;;  (make-proxy comp-udecl) => udecl
;;  (revert-proxy udecl) => comp-udecl

(define-module (nyacc lang c99 util2)
  #:export (c99-trans-unit->udecl
	    c99-trans-unit->udecl/full

	    stripdown stripdown-2
	    udecl->mspec udecl->mspec/comm

	    unwrap-decl
	    canize-enum-def-list
	    fixed-width-int-names

	    munge-decl match-comp-decl match-param-decl
	    declr->ident
	    expand-decl-typerefs

	    ;; deprecated
	    tree->udict tree->udict/deep
	    fix-fields
	    )
  #:use-module (nyacc lang c99 pprint)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module ((sxml fold) #:select (foldts foldts*))
  #:use-module (sxml match)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang c99 pprint)
  )

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

;; @deffn {Procedure} declr->ident declr => (ident "name")
;; Given a declarator, aka @code{init-declr}, return the identifier.
;; This is used by @code{tree->udict}.  See also: declr->id-name in body.scm.
;; @end deffn
(define (declr->ident declr)
  (sxml-match declr
    ((init-declr ,declr . ,rest) (declr->ident declr))
    ((comp-declr ,declr) (declr->ident declr))
    ((param-declr ,declr) (declr->ident declr))
    ((ident ,name) declr)
    ((array-of ,dir-declr ,array-spec) (declr->ident dir-declr))
    ((array-of ,dir-declr) (declr->ident dir-declr))
    ((ptr-declr ,pointer ,dir-declr) (declr->ident dir-declr))
    ((ftn-declr ,dir-declr ,rest ...) (declr->ident dir-declr))
    ((scope ,declr) (declr->ident declr))
    (,otherwise (throw 'util-error "c99/util2: unknown declarator: " declr))))

;; @deffn {Procedure} c99-trans-unit->udict tree [seed] => udict
;; @deffnx {Procedure} c99-trans-unit->udict/deep tree [seed] => udict
;; Turn a C parse tree into a assoc-list of names and definitions.
;; This will unwrap @code{init-declr-list} into list of decls w/
;; @code{init-declr}.
;; BUG: need to add struct and union defn's: struct foo { int x; };
;; how to deal with this
;; lookup '(struct . "foo"), "struct foo", ???
;; wanted "struct" -> dict but that is not great
;; solution: munge-decl => '(struct . "foo") then filter to generate
;; ("struct" ("foo" . decl) ..)
;; ("union" ("bar" . decl) ..)
;; @end deffn
;; @noindent
;; If @var{tree} is not a pair then @var{seed} -- or @code{'()} -- is returned.
(define* (c99-trans-unit->udict tree #:optional (seed '()))
  (if (pair? tree)
      (fold munge-decl seed (cdr tree))
      seed))
(define* (c99-trans-unit->udict/deep tree #:optional (seed '()))
  (if (pair? tree)
      (fold
       (lambda (tree seed)
	 (let ((tag (sx-tag tree)))
	   (cond
	    ((eqv? tag 'decl) (munge-decl tree seed))
	    ((and (eqv? tag 'cpp-stmt)
		  (eqv? (sx-tag (sx-ref tree 1)) 'cpp-include))
	     (tree->udict/deep (sx-ref tree 2) seed))
	    (else seed))))
       seed
       (cdr tree))
      seed))
(define tree->udict c99-trans-unit->udict)
(define tree->udict/deep c99-trans-unit->udict/deep)
       

;; @deffn {Procedure} munge-decl decl seed [#:expand-enums #f] => seed
;; This is a fold iterator to used by @code{tree->udict}.  It converts the
;; multiple @code{init-declr} items in an @code{init-declr-list} of a
;; @code{decl} into an a-list of multiple pairs of name and @code{udecl}
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
;;	  (name-dict (fold match-decl-1 '() (cdr sx1))))
;; @end example
;; TODO: add enums because they are global!!, but this should be user opt
;; @example
;; enum { ABC = 123 }; => ???
;; @end example
;; Unexpanded, unnamed enums have keys @code{"enum"}.
;; Enum, struct and union def's have keys @code{(enum . "name")},
;; @code{(struct . "name")} and @code{(union . "name)}, respectively.
;; @end deffn
(define* (munge-decl decl seed #:key (expand-enums #f))
  (if (not (and (pair? decl) (eqv? 'decl (sx-tag decl)))) seed
      (let* ((tag (sx-tag decl)) (tag 'udecl)
	     (attr (sx-attr decl))
	     (spec (sx-ref decl 1))	; (decl-spec-list ...)
	     (tbd (sx-ref decl 2)))	; (init-declr-list ...) OR ...
	(cond
	 ((or (not tbd) (eqv? 'comment (sx-tag tbd)))
	  ;; no init-declr-list => struct or union def or param-decl enum
	  ;;(display "spec:\n") (pretty-print decl)
	  (sxml-match spec
	    ((decl-spec-list
	      (type-spec
	       (struct-def (ident ,name) . ,rest2) . ,rest1))
	     (acons `(struct . ,name) decl seed))
	    ((decl-spec-list
	      (type-spec
	       (union-def (ident ,name) . ,rest2) . ,rest1))
	     (acons `(union . ,name) decl seed))
	    ((decl-spec-list
	      (type-spec
	       (enum-def
		(enum-def-list
		 (enum-defn
		  (ident "ABC")
		  (p-expr (fixed "123")))))))
	     ;; TODO
	     seed)
	    (,otherwise
	     ;; e.g., enum { VAL = 1 };
	     ;;(simple-format #t "+++ otherwise: ~S\n" tbd) (pretty-print decl)
	     seed)))
	 (else ;; decl with init-declr-list
	  (let* ((id-l tbd) (tail (sx-tail decl 3)))
	    (let iter ((res seed) (idl (cdr id-l)))
	      (if (null? idl) res
		  (let* ((declr (sx-ref (car idl) 1))
			 (ident (declr->ident declr))
			 (name (cadr ident)))
		    (iter
		     (acons name
			    (if attr
				(cons* tag attr spec (car idl) tail)
				(cons* tag spec (car idl) tail))
			    res)
		     (cdr idl)))))))))))

;;.@deffn gen-enum-udecl nstr vstr => (udecl ...)
;; @example
;; (gen-enum-udecl "ABC" "123")
;; =>
;; (udecl (decl-spec-list
;;         (type-spec
;;          (enum-def
;;           (enum-def-list ;; remove?
;;            (enum-defn (ident "ABC") (p-expr (fixed "123")))))))))
;; @end example
;; @end deffn
(define (gen-enum-udecl nstr vstr)
  `(udecl (decl-spec-list
	   (type-spec
	    (enum-def
             (enum-def-list
	      (enum-defn (ident ,nstr) (p-expr (fixed ,vstr)))))))))

;; @deffn {Procedure} match-comp-decl decl seed
;; This will turn
;; @example
;; (comp-decl (decl-spec-list (type-spec "int"))
;;            (comp-decl-list
;;             (comp-declr (ident "a")) (comp-declr (ident "b"))))
;; @end example
;; @noindent
;; into
;; @example
;; ("a" . (comp-decl (decl-spec-list ...) (comp-declr (ident "a"))))
;; ("b" . (comp-decl (decl-spec-list ...) (comp-declr (ident "b"))))
;; @end example
;; @noindent
;; This is coded to be used with fold-right in order to preserve order
;; in @code{struct} and @code{union} field lists.
;; @end deffn
(define (match-comp-decl decl seed)
  (if (not (eqv? 'comp-decl (car decl))) seed
      (let* ((tag (sx-ref decl 0))
	     (attr (sx-attr decl))
	     (spec (sx-ref decl 1))	; (type-spec ...)
	     (id-l (sx-ref decl 2))	; (init-declr-list ...)
	     (tail (sx-tail decl 3)))	; opt comment, different here
	;;(simple-format #t "1: ~S\n" id-l)
	(let iter ((res seed) (idl (cdr id-l)))
	  (if (null? idl) res
	      (let* ((declr (sx-ref (car idl) 1))
		     (ident (declr->ident declr))
		     (name (cadr ident)))
		;;(pretty-print `(comp-decl ,spec ,(car idl) . ,tail))
		(acons name
		       (if attr
			   (cons* tag attr spec (car idl) tail)
			   (cons* tag spec (car idl) tail))
		       (iter res (cdr idl)))))))))

;; @deffn {Procedure} match-param-decl param-decl seed
;; This will turn
;; @example
;; (param-decl (decl-spec-list (type-spec "int"))
;;             (param-declr (ident "a")))
;; @end example
;; @noindent
;; into
;; @example
;; ("a" . (comp-decl (decl-spec-list ...) (comp-declr (ident "a"))))
;; @end example
;; @noindent
;; This is coded to be used with fold-right in order to preserve order
;; in @code{struct} and @code{union} field lists.
;; @end deffn
(define (match-param-decl decl seed)
  (if (not (eqv? 'param-decl (car decl))) seed
      (let* ((tag (sx-ref decl 0))
	     (attr (sx-attr decl))
	     (spec (sx-ref decl 1))	; (type-spec ...)
	     (declr (sx-ref decl 2))	; (param-declr ...)
	     (ident (declr->ident declr))
	     (name (cadr ident)))
	(acons name decl seed))))
	
;; @deffn {Procedure} find-special udecl-alist seed => ..
;; NOT DONE
;; @example
;; '((struct . ("foo" ...) ...)
;;   (union . ("bar" ...) ...)
;;   (enum . ("bar" ...) ...)
;;   seed)
;; @end example
;; @end deffn
(define (find-special udecl-alist seed)
  (let iter ((struct '()) (union '()) (enum '()) (udal udecl-alist))
    (if (null? udal) (cons* (cons 'struct struct)
			  (cons 'union union)
			  (cons 'enum enum)
			  seed)
	'())))

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
;; This is a helper for expand-decl-typerefs
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

;; @deffn {Procedure} expand-decl-typerefs udecl udecl-dict [#:keep '()] => udecl
;; Given a declaration or component-declaration, expand all typename,
;; struct, union and enum refs.
;; @example
;; typedef const int  (*foo_t)(int a, double b);
;; extern    foo_t    fctns[2];
;; =>
;; extern const int  (*fctns[2])(int a, double b);
;; @end example
;; @noindent
;; Cool. Eh? (but is it done?)
;; @end deffn
(define* (expand-decl-typerefs udecl udecl-dict #:key (keep '()))
  (display "FIXME: some decls have no init-declr-list\n")
  ;; between adding (init-declr-list) to those or having predicate
  ;; (has-init-declr? decl)
  (let* ((tag (sx-tag udecl))		; decl or comp-decl
	 (attr (sx-attr udecl))		; (@ ...)
	 (specl (sx-ref udecl 1))	; decl-spec-list
	 (declr (or (sx-find 'init-declr udecl)
		    (sx-find 'comp-declr udecl)))
	 (tail (if declr (sx-tail udecl 3) (sx-tail udecl 2))) ; opt comment
	 (tspec (cadr (sx-find 'type-spec specl))))
    ;;(simple-format #t "=D> ~S\n" decl-spec-list)
    ;;(simple-format #t "init-declr: ~S\n" init-declr)
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
	   (expand-decl-typerefs fixed-udecl udecl-dict #:keep keep)))))

      ((struct-ref union-ref)
       (simple-format (current-error-port)
		      "+++ c99/util2: struct/union-ref: more to do?\n")
       ;;(simple-format #t "\nstruct-ref:\n") (pretty-print udecl)
       udecl)

      ((struct-def union-def)
       (let* ((ident (sx-find 'ident tspec))
	      (field-list (sx-find 'field-list tspec))
	      (orig-flds (cdr field-list))
	      (unit-flds (map cdr (fold-right match-comp-decl '() orig-flds)))
	      (fixd-flds (map
			  (lambda (fld)
			    (expand-decl-typerefs fld udecl-dict #:keep keep))
			  unit-flds))
	      (fixd-tspec
	       (if #f ;;ident
		   `(type-spec (struct-def ,ident (field-list ,@fixd-flds)))
		   `(type-spec (struct-def (field-list ,@fixd-flds)))))
	      (fixd-specl (repl-typespec specl fixd-tspec)))
	 (if declr (cons* tag fixd-specl declr tail)
	     (cons* tag fixd-specl tail))))
      
      ((enum-def)
       (let* ((enum-def-list (sx-find 'enum-def-list tspec))
	      (fixd-def-list (canize-enum-def-list enum-def-list))
	      (fixd-tspec `(type-spec (enum-def ,fixd-def-list)))
	      (fixd-specl (repl-typespec specl fixd-tspec))
	      (fixed-decl (cons* tag fixd-specl declr tail))) ;; !!!
	 fixed-decl))

      ((enum-ref)
       (simple-format (current-error-port) "chack: enum-ref NOT DONE\n")
       udecl)

      (else
       udecl))))
  
;; @deffn {Procedure} canize-enum-def-list
;; Fill in constants for all entries of an enum list.
;; @end deffn
(define (canize-enum-def-list enum-def-list)
  (define (get-used edl)
    (let iter ((uzd '()) (edl edl))
	 (cond
	  ((null? edl) uzd)
	  ((assq-ref (cdar edl) 'p-expr) =>
	   (lambda (x)
	     (iter (cons (string->number (cadar x)) uzd) (cdr edl))))
	  (else
	   (iter uzd (cdr edl))))))
  (let ((used (get-used (cdr enum-def-list))))
    (let iter ((rez '()) (ix 0) (edl (cdr enum-def-list)))
      (cond
       ((null? edl) (cons (car enum-def-list) (reverse rez)))
       ((assq-ref (cdar edl) 'p-expr)
	(iter (cons (car edl) rez) ix (cdr edl)))
       (else
	(let* ((ix1 (let iter ((ix (1+ ix)))
		      (if (memq ix used) (iter (1+ ix)) ix)))
	       (is1 (number->string ix1)))
	  (iter (cons (append (car edl) `((p-expr (fixed ,is1)))) rez)
		ix1 (cdr edl))))))))

;; @deffn {Procedure} stripdown udecl decl-dict => decl
;; 1) remove stor-spec
;; 2) expand typenames
;; @example
;; typedef int *x_t;
;; x_t a[10];
;; (spec (typename x_t)) (init-declr (array-of 10 (ident a)))
;; (spec (typedef) (fixed-type "int")) (init-declr (pointer) (ident "x_t"))
;; =>
;; [TO BE DOCUMENTED]
;; @end example
;; @end deffn
(define* (stripdown udecl decl-dict #:key (keep '()))

  ;;(define strip-list '(stor-spec type-qual comment))
  (define strip-list '(stor-spec type-qual))

  (define (fsD seed tree)
    '())

  (define (fsU seed kseed tree)
    (if (memq (car tree) strip-list)
	seed
	(if (null? seed)
	    (reverse kseed)
	    (cons (reverse kseed) seed))))
	
  (define (fsH seed tree)
    (cons tree seed))

  (let* ((xdecl (expand-decl-typerefs udecl decl-dict #:keep keep))
	 (tag (sx-tag xdecl))
	 (attr (sx-attr xdecl))
	 (specl (sx-ref xdecl 1))
	 (declr (sx-ref xdecl 2))
	 (specl1 (foldts fsD fsU fsH '() specl)))
    (list tag specl1 declr)))

;; @deffn {Procecure} stripdown-2
;; apparently for ffi but don't see difference
;; @end deffn
(define* (stripdown-2 udecl decl-dict #:key (keep '()))

  ;;(define strip-list '(stor-spec type-qual comment))
  (define strip-list '(stor-spec type-qual))

  (define (fsD seed tree)
    '())

  (define (fsU seed kseed tree)
    (if (memq (car tree) strip-list)
	seed
	(if (null? seed)
	    (reverse kseed)
	    (cons (reverse kseed) seed))))
	
  (define (fsH seed tree)
    (cons tree seed))

  (let* ((speclt (sx-tail udecl 1)))	; decl-spec-list tail
    ;; don't expand typedefs, structure specs etc,
    (cond
     ((and (eqv? 'stor-spec (caar speclt))
	   (eqv? 'typedef (cadar speclt)))
      udecl)
     ;; lone struct ref
     (else
      (let* ((xdecl (expand-decl-typerefs udecl decl-dict #:keep keep))
	     (tag (sx-tag xdecl))
	     (attr (sx-attr xdecl))
	     (specl (sx-ref xdecl 1))
	     (declr (sx-ref xdecl 2))
	     (specl1 (foldts fsD fsU fsH '() specl)))
	(list tag specl1 declr))))
      ))


;; @deffn {Procedure} udecl->mspec sudecl
;; @deffnx {Procedure} udecl->mspec/comm decl [dict] [#:def-comm ""]
;; Turn a stripped-down unit-declaration into an m-spec.
;; This assumes decls have been run through @code{stripdown}.
;; @example
;; (decl (decl-spec-list (type-spec "double"))
;;       (init-declr-list (
;;       (comment "state vector")
;; =>
;; ("x" "state vector" (array-of 10) (float "double")
;; @end example
;; @end deffn
(define (udecl->mspec decl . rest)

  (define (cnvt-array-size size-spec)
    (with-output-to-string (lambda () (pretty-print-c99 size-spec))))

  (define (unwrap-specl specl)
    (let ((tspec (cadadr specl)))
      ;;(simple-format #t "tspec:\n") (pretty-print tspec)
      (sxml-match tspec
	((xxx-struct-def (field-list . ,rest))
	 `(struct-def ,@rest))
	(,otherwise
	 tspec))))
    
  (define (unwrap-declr declr)
    (sxml-match declr
      ((ident ,name)
       (list name))
      ((init-declr ,item)
       (unwrap-declr item))
      ((comp-declr ,item)
       (unwrap-declr item))
      ((ptr-declr (pointer . ,r) ,dir-declr)
       (cons '(pointer-to) (unwrap-declr dir-declr)))
      ((array-of ,dir-declr ,size)
       (cons `(array-of ,(cnvt-array-size size)) (unwrap-declr dir-declr)))
      ((ftn-declr ,dir-declr ,params)
       (cons '(function-returning) (unwrap-declr dir-declr)))
      ((scope ,expr)
       (unwrap-declr expr))
      (,otherwise
       (simple-format #t "unwrap-declr: OTHERWISE\n") (pretty-print otherwise)
       ;; failed got: (array-of (ident "foo")) FROM const char foo[];
       #f)))

  (define (find-type-spec decl-spec-list)
    (let iter ((tsl (cdr decl-spec-list)))
      (if (eqv? 'type-spec (caar tsl)) (car tsl)
	  (iter (cdr tsl))))) 
  
  (let* ((decl-dict (if (pair? rest) (car rest) '()))
	 (specl (sx-ref decl 1))
 	 (declr (sx-ref decl 2))
	 (comm (sx-ref decl 3))
	 (m-specl (unwrap-specl specl))
	 (m-declr (unwrap-declr declr))
	 (m-decl (reverse (cons m-specl m-declr))))
    m-decl))

(define* (udecl->mspec/comm decl #:optional (dict '()) #:key (def-comm ""))
  (let* ((comm (or (and=> (sx-ref decl 3) cadr) def-comm))
	 (spec (udecl->mspec decl dict)))
    (cons* (car spec) comm (cdr spec))))

;; @deffn {Procedure} clean-field-list flds => flds
;; This will take a list of fields from a struct and remove lone comments.
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
    ;;(pretty-print fl)
    (cond
     ((null? fl) `(field-list ,(reverse rz)))
     ((eqv? 'comment (caar fl))
      (iter rz (cons (cadar fl) cl) (cdr fl)))
     ((eqv? 'comp-decl (caar fl))
      (if (eq? 4 (length (car fl)))
	  (iter (cons (car fl) rz) '() (cdr fl))	 ; has comment
	  (let* ((cs (apply string-append (reverse cl))) ; add comment
		 (fd (append (car fl) (list (list 'comment cs)))))
	    (iter (cons fd rz) '() (cdr fl)))))
     (else
      (error "bad field")))))

(define (fix-fields flds)
  (cdr (clean-field-list `(field-list ,flds))))

;; @bye


;; ===== not used? ====================

;;.@deffn {Procedure} unwrap-decl decl seed => seed
;; This is a fold to break up multiple declarators.
;; @example
;; (decl (decl-spec-list ...) (init-declr-list (init-declr ...) ...))
;; =>
;; ((decl (decl-spec-list ...) (init-declr ...))
;;  (decl (decl-spec-list ...) (init-declr ...))
;;  ...)
;; @end example
;; @end deffn
(define (unwrap-decl decl seed)
  (cond
   ((not (eqv? 'decl (car decl))) seed)
   ((< (length decl) 3) seed)		; this should catch struct-ref etc.
   (else
    (let* ((tag (sx-ref decl 0))
	   (attr (sx-attr decl))
	   (spec (sx-ref decl 1))	; (decl-spec-list ...)
	   (id-l (sx-ref decl 2))	; (init-declr-list ...)
	   (tail (sx-tail decl 3)))	; comment
      (let iter ((res seed) (idl (cdr id-l)))
	(if (null? idl) res
	    (let* ((declr (sx-ref (car idl) 1))
		   (ident (declr->ident declr))
		   (name (cadr ident)))
	      (iter (cons (if attr
			      (cons* tag attr spec (car idl) tail)
			      (cons* tag spec (car idl) tail))
			  res)
		    (cdr idl)))))))))


;; --- last line ---
