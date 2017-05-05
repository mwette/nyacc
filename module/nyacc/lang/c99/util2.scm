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
;;  (make-proxy comp-udecl) => udecl
;;  (revert-proxy udecl) => comp-udecl

;; NOTE
;;  stripdown no longer deals with typeref expansion
;; use
;;  expand-typerefs, then stripdown, then udecl->mspec

(define-module (nyacc lang c99 util2)
  #:export (c99-trans-unit->udict
	    c99-trans-unit->udict/deep

	    expand-typerefs
	    stripdown
	    udecl->mspec udecl->mspec/comm

	    unwrap-decl
	    canize-enum-def-list
	    fixed-width-int-names

	    munge-decl munge-comp-decl munge-param-decl
	    declr->ident

	    clean-field-list

	    ;; deprecated
	    tree->udict tree->udict/deep
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
  ;;#:use-module (system base pmatch)
  #:use-module (nyacc lang c99 pprint)
  #:use-module (ice-9 pretty-print)
  )

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

;; @deffn {Procedure} declr->ident declr => (ident "name")
;; Given a declarator, aka @code{init-declr}, return the identifier.
;; This is used by @code{trans-unit->udict}.
;; See also: declr->id-name in body.scm.
;; @end deffn
(define (declr->ident declr)
  (sxml-match declr
    ((ident ,name) declr)
    ((init-declr ,declr . ,rest) (declr->ident declr))
    ((comp-declr ,declr) (declr->ident declr))
    ((param-declr ,declr) (declr->ident declr))
    ((array-of ,dir-declr ,array-spec) (declr->ident dir-declr))
    ((array-of ,dir-declr) (declr->ident dir-declr))
    ((ptr-declr ,pointer ,dir-declr) (declr->ident dir-declr))
    ((ftn-declr ,dir-declr ,rest ...) (declr->ident dir-declr))
    ((scope ,declr) (declr->ident declr))
    (,otherwise (throw 'util-error "c99/util2: unknown declarator: " declr))))

;; @deffn {Procedure} c99-trans-unit->udict tree [seed] [#:filter f] => udict
;; @deffnx {Procedure} c99-trans-unit->udict/deep tree [seed]=> udict
;; Turn a C parse tree into a assoc-list of global names and definitions.
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
;; The filter @var{f} is either @code{#t}, @code{#f} or predicate procedure
;; of one argument, the include path, to indicate whether it should be included
;; in the dictionary.
(define* (c99-trans-unit->udict tree #:optional (seed '()) #:key filter)
  (define (inc-keeper? tree)
    (if (and (eqv? (sx-tag tree) 'cpp-stmt)
	     (eqv? (sx-tag (sx-ref tree 1)) 'include))
	(if (procedure? filter)
	    (filter (let ((arg (sx-ref (sx-ref tree 1) 1)))
		      (substring arg 1 (1- (string-length arg)))))
	    filter)
	#f))
  (if (pair? tree)
      (fold
       (lambda (tree seed)
	 (cond
	  ((eqv? (sx-tag tree) 'decl)
	   (munge-decl tree seed))
	  ((inc-keeper? tree)
	   (c99-trans-unit->udict (sx-ref tree 1) seed #:filter filter))
	  (else seed)))
       seed
       (cdr tree))
      seed))
(define (c99-trans-unit->udict/deep tree)
  (c99-trans-unit->udict tree #:filter #t))
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
;; See @code{udict-struct-ref}, @code{udict-union-ref}, @code{udict-enum-ref}
;; and @code{udict-ref}.
;; @end deffn

;; TODO:
;; enum tag => "enum" (tag . udecl)
;; enum { NAME } => "enum" ("" . udecl) ???? WHAT TO DO ????
;; need (ud-lookup name) (ud-lookup-struct name) (ud-lookup-union name)
;; USING ud-udict-struct-ref 
(define* (munge-decl decl seed #:key (expand-enums #f))

  (define (iter-declrs init-declr-l tail seed)
    (if (not init-declr-l) seed
	(let iter ((seed seed) (idl (cdr init-declr-l)))
	  (if (null? idl) seed
	      (let* ((tag 'udecl) (attr (sx-attr decl))
		     (specl (sx-ref decl 1)) (declr (sx-ref (car idl) 1))
		     (ident (declr->ident declr)) (name (cadr ident)))
		(iter
		 (acons name (sx-cons* tag attr specl (car idl) tail) seed)
		 (cdr idl)))))))

  ;;(simple-format #t "munge-decl ~S\n" decl)
  (cond
   ((not (pair? decl)) seed)
   ((eqv? (sx-tag decl) 'decl)
    (let* ((tag (sx-tag decl)) (tag 'udecl)
	   (attr (sx-attr decl))
	   (spec (sx-ref decl 1))	; (decl-spec-list ...)
	   (sx2 (sx-ref decl 2)) ; (init-declr-list ...) OR ...
	   (init-d-l (if (and sx2 (eqv? (sx-tag sx2) 'init-declr-list)) sx2 #f))
	   (tail (sx-tail decl (if init-d-l 3 2))))
      (sxml-match spec
	((decl-spec-list
	  (type-spec (struct-def (ident ,name) . ,rest2) . ,rest1))
	 (acons `(struct . ,name) decl (iter-declrs init-d-l tail seed)))
	((decl-spec-list
	  (type-spec (struct-def . ,rest2) . ,rest1))
	 (acons `(struct . "*anon*") decl (iter-declrs init-d-l tail seed)))
	((decl-spec-list
	  (type-spec (union-def (ident ,name) . ,rest2) . ,rest1))
	 (acons `(union . ,name) decl (iter-declrs init-d-l tail seed)))
	((decl-spec-list
	  (type-spec (union-def . ,rest2) . ,rest1))
	 (acons `(union . "*anon*") decl (iter-declrs init-d-l tail seed)))
	((decl-spec-list
	  (type-spec (enum-def (ident ,name) . ,rest2) . ,rest1))
	 (acons `(enum . ,name) decl (iter-declrs init-d-l tail seed)))
	((decl-spec-list
	  (type-spec (enum-def . ,rest2) . ,rest1))
	 (acons `(enum . "*anon*") decl (iter-declrs init-d-l tail seed)))
	(,otherwise
	 (iter-declrs init-d-l tail seed)))))
   ((eqv? (sx-tag decl) 'comp-decl)
    (munge-comp-decl decl seed #:expand-enums expand-enums))
   ((eqv? (sx-tag decl) 'param-decl)
    (munge-param-decl decl seed #:expand-enums expand-enums))
   (else seed)))

;; @deffn {Procedure} munge-comp-decl decl seed [#:expand-enums #f]
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
(define* (munge-comp-decl decl seed #:key (expand-enums #f))
  (if (not (eqv? 'comp-decl (car decl))) seed
      (let* ((tag (sx-ref decl 0))
	     (attr (sx-attr decl))
	     (spec (sx-ref decl 1))	; (type-spec ...)
	     (id-l (sx-ref decl 2))	; (init-declr-list ...)
	     (tail (sx-tail decl 3)))	; opt comment, different here
	(let iter ((res seed) (idl (cdr id-l)))
	  (if (null? idl) res
	      (let* ((declr (sx-ref (car idl) 1))
		     (ident (declr->ident declr))
		     (name (cadr ident)))
		(acons name
		       (if attr
			   (cons* tag attr spec (car idl) tail)
			   (cons* tag spec (car idl) tail))
		       (iter res (cdr idl)))))))))
(define match-comp-decl munge-comp-decl)

;; @deffn {Procedure} match-param-decl param-decl seed [#:expand-enums #f]
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
(define* (munge-param-decl decl seed #:key (expand-enums #f))
  (if (not (eqv? 'param-decl (car decl))) seed
      (let* ((tag (sx-ref decl 0))
	     (attr (sx-attr decl))
	     (spec (sx-ref decl 1))	; (type-spec ...)
	     (declr (sx-ref decl 2))	; (param-declr ...)
	     (ident (declr->ident declr))
	     (name (cadr ident)))
	(acons name decl seed))))
(define match-param-decl munge-param-decl)
	
;; like member but returns first non-declr of type in dict
(define (non-declr type udict)
  (let iter ((dict udict))
    (cond
     ((null? dict) #f)
     ((and (pair? (car dict)) (eqv? type (caar dict))) dict)
     (else (iter (cdr dict))))))
     
;; @deffn {Procedure} udict-ref name
;; @deffnx {Procedure} udict-struct-ref name
;; @deffnx {Procedure} udict-union-ref name
;; @deffnx {Procedure} udict-enum-ref name
;; @end deffn
(define (udict-ref udict name)
  (or (assoc-ref udict name)
      (let iter ((dict (non-declr 'enum udict)))
	(cond
	 ((not dict) #f)
	 ((assoc-ref (cdar dict) name))
	 (else (iter (non-declr 'enum dict)))))))
(define (udict-struct-ref udict name)
  #f)
(define (udict-union-ref udict name)
  #f)
(define (udict-enum-ref udict name)
  #f)

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

;; @deffn {Procedure} expand-typerefs udecl udecl-dict [#:keep '()]
;; Given a declaration or component-declaration, return a udecl with all
;; typenames (not in @var{keep}), struct, union and enum refs expanded.
;; (but enums to int?)
;; @example
;; typedef const int  (*foo_t)(int a, double b);
;; extern    foo_t    fctns[2];
;; =>
;; extern const int  (*fctns[2])(int a, double b);
;; @end example
;; @end deffn
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
	   (expand-decl-typerefs fixed-udecl udecl-dict #:keep keep)))))

      ((struct-ref union-ref)
       (simple-format (current-error-port)
		      "+++ c99/util2: struct/union-ref: more to do?\n")
       (simple-format #t "\nstruct-ref:\n") (pretty-print udecl)
       (let* ((is-struct (eqv? 'struct-ref (car tspec)))
	      (ident (cadr tspec))
	      (name (cadr ident))
	      (ref (if is-struct
		       (udict-struct-ref udecl-dict name)
		       (udict-union-ref udecl-dict name)))
	      )
	 #f)
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
(define expand-decl-typerefs expand-typerefs)

;; === enum handling ...
  
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

;; @deffn {Procedure} stripdown udecl decl-dict [options]=> decl
;; 1) remove stor-spec
;; @example
;; =>
;; @end example
;; @end deffn
(define* (stripdown udecl decl-dict #:key const-ptr)
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
		  (sx-tail specl) #:keep-const? (and #:const-ptr is-ptr?)))
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
       (simple-format #t "unwrap-declr: OTHERWISE\n") (pretty-print otherwise)
       ;; failed got: (array-of (ident "foo")) FROM const char foo[];
       (error "udecl->mspec failed")
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
    ;;(pretty-print fl)
    (cond
     ((null? fl) (cons 'field-list (reverse rz)))
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
  (cdr (clean-field-list `(field-list . ,flds))))

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
