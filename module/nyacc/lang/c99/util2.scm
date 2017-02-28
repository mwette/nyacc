;;; nyacc/lang/c99/util2.scm - C processing code
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

;; The idea is to convert declarations into something like
;; @example
;; const char *args[21];  /* command arguments */
;; @end example
;; @noindent
;; into
;; @example
;; ("args" (comment " command arguments ")
;;  (array-of 21) (pointer-to) (fixed "char"))
;; @end example
;; @noindent
;; or without the comment.  It is a question whether we need the fixed part.
;; In addition, we want to reduce to a set of canonical types.  So something
;; like @code{foo_t} should be expanded.

;; KEEPING STRUCTS ENUMS etc
;; if have typename and want to keep it, then change
;;   (typename "foo_t")
;; to
;;   (typename (@ (base "struct")) "foo_t")

;; ALSO
;;  (make-proxy comp-udecl) => udecl
;;  (revert-proxy udecl) => comp-udecl

(define-module (nyacc lang c99 util2)
  #:export (tree->udict
	    stripdown stripdown-2
	    udecl->mspec
	    udecl->mspec/comm

	    unwrap-decl
	    canize-enum-def-list
	    fix-fields
	    fixed-width-int-names

	    match-decl match-comp-decl match-param-decl
	    declr->ident
	    expand-decl-typerefs
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


;; @deffn declr->ident declr => (ident "name")
;; just match the declarator
;; (init-declr <declr> [<initzer>])
;; See also: declr->id-name in body.scm.
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

;; @deffn unwrap-decl decl seed => seed
;; This is a fold to break up multiple declarators.
;; @example
;; (decl (decl-spec-list ...) (init-declr-list (init-declr ...) ...))
;; =>
;; ((decl (decl-spec-list ...) (init-declr ...))
;;  (decl (decl-spec-list ...) (init-declr ...))
;;  ...)
;; @end example
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


;; @deffn tree->udict tree => udict
;; Turn a C parse tree into a assoc-list of names and definitions.
;; This will unwrap @code{init-declr-list} into list of decls w/
;; @code{init-declr}.
;; BUG: need to add struct and union defn's: struct foo { int x; };
;; how to deal with this
;; lookup '(struct . "foo"), "struct foo", ???
;; wanted "struct" -> dict but that is not great
;; solution: match-decl => '(struct . "foo") then filter to generate
;; ("struct" ("foo" . decl) ..)
;; ("union" ("bar" . decl) ..)
(define (tree->udict tree)
  (if (pair? tree)
      (fold match-decl '() (cdr tree))
      '()))

;; @deffn match-decl decl seed
;; This procedure is intended to be used with @code{fold}.  It breaks up
;; up the init-declr-list into individual init-declr items and associates
;; with the identifier being declared.  So this is a fold iterator to
;; provide a dictionary of declared names.
;; @example
;; (decl (decl-spec-list ...) (init-declr-list (init-declr ...) ...))
;; @end example
;; @noindent
;; has been replaced by
;; @example
;; (decl (decl-spec-list ...) (init-declr ...))
;; (decl (decl-spec-list ...) ...)
;; @end example
;; Here we generate a dictionary of all declared items:
;; @example
;; (let* ((sx0 (with-input-from-file src-file parse-c))
;;	  (sx1 (merge-inc-trees! sx0))
;;	  (name-dict (fold match-decl-1 '() (cdr sx1))))
;; @end example
;; TODO: add enums because they are global!!
;; turn enum { ABC = 123 }; into '(ABC . (enum .. "ABC" "123" .. )
(define (match-decl decl seed)
  (let* ((tag (sx-ref decl 0)) (attr (sx-attr decl)))
    (case tag
      ((decl)
       (let* ((spec (sx-ref decl 1))	; (decl-spec-list ...)
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
		      (cdr idl))))))))))
      (else seed))))

;; @deffn match-comp-decl decl seed
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

;; @deffn match-param-decl param-decl seed
;; This will turn
;; @example
;; (param-decl (decl-spec-list (type-spec "int")) (param-declr (ident "a")))
;; @end example
;; @noindent
;; into
;; @example
;; ("a" . (comp-decl (decl-spec-list ...) (comp-declr (ident "a"))))
;; @end example
;; @noindent
;; This is coded to be used with fold-right in order to preserve order
;; in @code{struct} and @code{union} field lists.
(define (match-param-decl decl seed)
  (if (not (eqv? 'param-decl (car decl))) seed
      (let* ((tag (sx-ref decl 0))
	     (attr (sx-attr decl))
	     (spec (sx-ref decl 1))	; (type-spec ...)
	     (declr (sx-ref decl 2))	; (param-declr ...)
	     (ident (declr->ident declr))
	     (name (cadr ident)))
	(acons name decl seed))))
	
;; @deffn find-special udecl-alist seed => ..
;; NOT DONE
;; @example
;; '((struct . ("foo" ...) ...)
;;   (union . ("bar" ...) ...)
;;   (enum . ("bar" ...) ...)
;;   seed)
;; @end example
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

(define fixed-width-int-names
  '("int8_t" "uint8_t" "int16_t" "uint16_t"
    "int32_t" "uint32_t" "int64_t" "uint64_t"))

;; @deffn typedef-decl? decl)
(define (typedef-decl? decl)
  (sxml-match decl
    ((decl (decl-spec-list (stor-spec (typedef)) . ,r1) . ,r2) #t)
    (,otherwise #f)))

;; @deffn splice-declarators orig-declr tdef-declr => 
;; Splice the original declarator into the typedef declarator.
;; This is a helper for @code{expand-*-typename-ref} procecures.
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


;; @deffn repl-typespec decl-spec-list replacement
;; This is a helper for expand-decl-typerefs
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

;; @deffn expand-decl-typerefs udecl udecl-dict => udecl
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
(define* (expand-decl-typerefs udecl udecl-dict #:key (keep '()))
  ;;(display "FIXME: some decls have no init-declr-list\n")
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

      (else udecl))))
  
;; @deffn canize-enum-def-list
;; Fill in constants for all entries of an enum list.
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

;; @deffn stripdown udecl decl-dict => decl
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

;; This one experimental for guile ffi.
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


;; @deffn udecl->mspec sudecl
;; Turn a stripped-down unit-declaration into an m-spec.
;; This assumes decls have been run through @code{stripdown}.
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

;; @deffn udecl->mspec/comm decl [dict] [#:def-comm ""]
;; Convert declaration tree to an mspec
;; @example
;; (decl ... (comment "state vector")
;; =>
;; ("x" "state vector" (array-of 10) (float "double")
;; @end example
(define* (udecl->mspec/comm decl #:optional (dict '()) #:key (def-comm ""))
  (let* ((comm (or (and=> (sx-ref decl 3) cadr) def-comm))
	 (spec (udecl->mspec decl dict)))
    (cons* (car spec) comm (cdr spec))))

;; @deffn fix-fields flds => flds
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
(define (fix-fields flds)
  (let iter ((rz '()) (cl '()) (fl flds))
    ;;(pretty-print fl)
    (cond
     ((null? fl) (reverse rz))
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

;; --- last line ---
