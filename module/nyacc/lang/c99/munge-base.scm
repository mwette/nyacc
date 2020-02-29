;;; nyacc/lang/c99/munge-base.scm -

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

;; Todo:
;; 1) mdecl == munged (unwrapped) declaration
;; 2) I want a way to keep named enums in expand-typerefs.
;;    Currently, they are expanded to int.
;; 3) Usual sequence is: expand-typerefs, stripdown-udecl, udecl->mdecl.
;; 4) Unitize-decl is shallow.  It does not dive into structs and unitize.
;; 5) In expand-typerefs if need to expand `foo_t *x' then change to
;;    a) if struct use `struct foo *x;'
;;    b) if fixed/float use `int *x;' etc
;;    c> if function use `void *x;'
;; 6) Check use of comments as attributes.

;;; Code:

(define-module (nyacc lang c99 munge-base)
  #:export (expand-typerefs
	    udecl->mdecl
	    reify-declr reify-decl
	    split-udecl
	    clean-field-list clean-fields)
  #:use-module (nyacc lang sx-util)
  #:use-module (srfi srfi-11)		; let-values
  #:use-module (srfi srfi-1)
  #:use-module (system base pmatch))

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

;; @deffn {Procedure} declr-list? declr
;; Determine if declr it is a list or not.
;; Often declr can be xxxx-declr-list or xxxx-declr.
;; @end deffn
(define (declr-list? declr)
  (and (pair? declr)
       (member (sx-tag declr) '(init-declr-list comp-declr-list))))

;; @deffn {Procedure} clean-field-list field-list => field-list
;; @deffnx {Procedure} clean-fields fields => fields
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
(define (clean-fields fields)
  (define (str-app-rev strl) (apply string-append (reverse strl)))
  (let loop ((rz '()) (cl '()) (fl fields))
    (if (null? fl)
	(reverse rz)
	(sx-match (car fl)
	  ((comment ,text)
	   (loop rz (cons text cl) (cdr fl)))
	  (((comp-udecl comp-decl) (@ . ,attr) . ,rest)
	   (let* ((comm (assq-ref attr 'comment))
		  (decl (car fl))
		  (decl
		   (cond
		    (comm decl)
		    ((null? cl) decl)
		    (else (sx-attr-add decl 'comment (str-app-rev cl))))))
	     (loop (cons decl rz) '() (cdr fl))))
	  (,_ (throw 'nyacc-error "clean-field-list: ~S" (car fl)))))))

(define (clean-field-list field-list)
  (cons (car field-list) (clean-fields (cdr field-list))))

;; @deffn {Procedure} pointer-declr? declr
;; This predictate indicates if @var{declr} is a pointer.
;; Does not handle @code{(*ftn)()}.
;; The argument can also be @code{init-declr-list} or @code{comp-declr-list}
;; in which case all elements need to be pointers.
;; @end deffn
(define (pointer-declr? declr)
  (and
   declr
   (sx-match declr
     ((init-declr ,declr) (pointer-declr? declr))
     ((comp-declr ,declr) (pointer-declr? declr))
     ((param-declr ,declr) (pointer-declr? declr))
     ;;
     ((ptr-declr ,pointer ,dir-declr) #t)
     ((ary-declr . ,rest) #t)
     ((ftn-declr . ,rest) #t)
     ((abs-declr (pointer . ,r1) . ,r2) #t)
     ;;
     ((init-declr-list . ,declrs)
      (fold (lambda (dcl seed) (and (pointer-declr? dcl) seed)) #t declrs))
     ((comp-declr-list . ,declrs)
      (fold (lambda (dcl seed) (and (pointer-declr? dcl) seed)) #t declrs))
     ;;
     (,_ #f))))

;; @deffn {Procedure} pointer-pass-declr? declr => #t|#f
;; This predicate determines if the declarator is implemented as a pointer.
;; That is, it is an explicit pointer, an array (ERROR), or a function.
;; @end deffn
(define (pointer-pass-declr? declr)
  (and
   declr
   (sx-match declr
     ((init-declr ,declr) (pointer-declr? declr))
     ((comp-declr ,declr) (pointer-declr? declr))
     ((param-declr ,declr) (pointer-declr? declr))
     ;;
     ((ptr-declr ,pointer ,dir-declr) #t)
     ((ary-declr . ,rest) #t)
     ((ftn-declr . ,rest) #t)
     ((abs-declr (pointer . ,r1) . ,r2) #t)
     ;;
     ((init-declr-list . ,declrs)
      (fold (lambda (dcl seed) (and (pointer-declr? dcl) seed)) #t declrs))
     ((comp-declr-list . ,declrs)
      (fold (lambda (dcl seed) (and (pointer-declr? dcl) seed)) #t declrs))
     ;;
     (,_ #f))))

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

;; @deffn {Procedure} split-udecl decl => tag attr decl declr
;; This routine splits a unitized declaration into its constituent parts.
;; Get @code{(values tag attr spec-l declrs tail)}.
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
(define (split-udecl udecl)
  (let ((tag (sx-tag udecl))
	(attr (sx-attr udecl))
	(specl (sx-ref udecl 1))	; (decl-spec-list ...)
	(declr (sx-ref udecl 2)))	; (declr ...)|#f
    (values tag attr specl declr)))

;; === typedef expansion ===============

;; allows only one storage specifier besides typedef
;; call this (injest-in-specl orig-specl repl-specl)
(define (tdef-splice-specl orig-specl repl-specl)
  (let loop ((specl '()) (repll '()) (origl (cdr orig-specl)))
    (cond
     ((pair? repll)
      (cond
       ((equal? (car repll) '(stor-spec (typedef)))
	(loop specl (cdr repll) origl))
       ((equal? (car repll) '(stor-spec (const)))
	(loop (cons (car repll) specl) (cdr repll) origl))
       ((member (car repll) specl)	; don't duplicate other stor-spec's
	(loop specl (cdr repll) origl))
       (else 
	(loop (cons (car repll) specl) (cdr repll) origl))))
     ((pair? origl)
      (cond
       ((pmatch (car origl) ((type-spec (typename ,name)) #t) (,othersize #f))
	(loop specl (cdr repl-specl) (cdr origl))) ; now insert replacement
       ((equal? (car origl) '(stor-spec (const)))
	(loop (cons (car repll) specl) repll (cdr origl)))
       ((member (car origl) specl) ; don't duplicate "auto", "extern"
	(loop specl repll (cdr origl)))
       (else
	(loop (cons (car origl) specl) repll (cdr origl)))))
     (else
      (cons 'decl-spec-list (reverse specl))))))

;; @deffn {Procedure} tdef-splice-declr orig-declr tdef-declr
;; @example
;; typedef int *foo_t;
;; foo_t bla[3];
;; @end example
;; @noindent
;; maps
;; @example
;; bla[3] => *(bla[3])
;; @end example
;; @end deffn
(define (tdef-splice-declr orig-declr tdef-declr)

  (define (probe-declr declr)
    (sx-match declr
      ((ident ,name)
       (sx-ref orig-declr 1))		; returns #f if abstract
      ((init-declr ,dcl)
       `(init-declr ,(probe-declr dcl)))
      ((comp-declr ,dcl)
       `(comp-declr ,(probe-declr dcl)))
      ((param-declr ,dcl)
       (let ((dcl (probe-declr dcl)))
	 (if dcl `(param-declr ,dcl) `(param-declr))))
      ((ptr-declr ,ptr ,dcl)
       (let ((dcl (probe-declr dcl)))
	 (if dcl
	     (sx-match dcl		; chaining of pointers
	       ((ptr-declr ,ptr1 ,dcl1) `(ptr-declr (pointer ,ptr1) ,dcl1))
	       ((abs-ptr-declr ,ptr1) `(abs-ptr-declr (pointer ,ptr1)))
	       (,_ `(ptr-declr ,ptr ,dcl)))
	     `(abs-ptr-declr ,ptr))))
      ((ary-declr ,dcl . ,rest)
       (let ((dcl (probe-declr dcl)))
	 (if dcl `(ary-declr ,dcl . ,rest) `(abs-ary-decl . ,rest))))
      ((ftn-declr ,dcl . ,rest)
       (let ((dcl (probe-declr dcl)))
	 (if dcl `(ftn-declr ,dcl . ,rest) `(abs-ftn-declr . ,rest))))
      ((scope ,dcl)
       `(scope ,(probe-declr dcl)))
      (,_ (throw 'c99-error "c99/munge: unknown declarator: ~S" declr))))

  (probe-declr (cons (sx-tag orig-declr) (sx-tail tdef-declr))))

;; @deffn {Procedure} tdef-splice-declr-list orig-declr-list tdef-declr
;; iterate tdef-splice-declr over a declr-init-list (or equiv)
;; @end deffn
(define (tdef-splice-declr-list orig-declr-list tdef-declr)
  (sx-cons*
   (sx-tag orig-declr-list)
   (sx-attr orig-declr-list)
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
   (sx-tag decl-spec-list)
   (sx-attr decl-spec-list)
   (map
    (lambda (elt) (if (eq? (sx-tag elt) 'type-spec) type-spec elt))
    (sx-tail decl-spec-list 1))))

;; declr can be xxxx-declr-list or xxxx-declr
;; This needs to be able to accept @code{#f} @var{declr}. <= done, methinks

(define (expand-specl-typerefs specl declr udict keep)

  ;; In the process of expanding typerefs it is crutial that routines which
  ;; expand parts return the original if no change made.  That is, if there
  ;; are no changes then @code{(eq? (expand-typerefs expr) expr)} is true.
  ;; If not, then infinite loop will result.

  (define (re-expand specl declr) ;; applied after typename
    (expand-specl-typerefs specl declr udict keep))

  (define (splice-typename specl declr name udict)
    (let* ((decl (or (assoc-ref udict name) ; decl for typename
		     (throw 'c99-error "typedef not found for: ~S" name)))
	   (tdef-specl (sx-ref decl 1))	 ; specs for typename
	   (tdef-declr (sx-ref decl 2))) ; declr for typename
      (values
       (tdef-splice-specl specl tdef-specl)
       (cond
	((declr-list? declr) (tdef-splice-declr-list declr tdef-declr))
	(else (tdef-splice-declr declr tdef-declr))))))

  (let* ((tspec (and=> (sx-find 'type-spec specl) cadr))
	 (class (sx-tag tspec))		; e.g., typename, fixed-type
	 (name (sx-ref tspec 1)))	; e.g., "foo_t"
    (case class
      ((typename)
       (cond
	((member name keep)		; keeper; don't expand
	 (values specl declr))
	((and #t ;; (pointer-declr? declr)
	      (member (cons 'pointer name) keep))
	 (values specl declr))
	(#f ;;(pointer-declr? name)		; replace with void*
	 (let ((specl (replace-type-spec specl '(type-spec (void)))))
	   (call-with-values
	       (lambda () (splice-typename specl declr name udict))
	     (lambda (specl declr) (re-expand specl declr)))))
	(else				; expand
	 (call-with-values
	     (lambda () (splice-typename specl declr name udict))
	   (lambda (specl declr)
	     (re-expand specl declr))))))
      ((struct-def union-def)
       (let* ((tag (sx-tag tspec))
	      (attr (sx-attr tspec))
	      (fld1 (sx-ref tspec 1))
	      (ident (if (eq? 'ident (sx-tag fld1)) fld1 #f))
	      (field-list (if ident (sx-ref tspec 2) fld1))
	      (field-list (clean-field-list field-list)) ; why remove comments?
	      (orig-flds (sx-tail field-list 1))
	      (fixd-flds (map
			  (lambda (fld) (expand-typerefs fld udict keep))
			  orig-flds))
	      (fixd-field-list `(field-list ,@fixd-flds))
	      (fixd-struct (if ident
			       (sx-list tag attr ident fixd-field-list)
			       (sx-list tag attr fixd-field-list)))
	      (fixd-tspec `(type-spec ,fixd-struct))
	      (fixd-specl (replace-type-spec specl fixd-tspec)))
	 (values fixd-specl declr)))
      ((struct-ref union-ref) ;; compound reference; replace unless pointer
       (let* ((c-name (and=> (sx-find 'ident tspec)
			     (lambda (id) (sx-ref id 1))))
	      (c-key (compound-key class c-name)) ;; e.g., (struct . "foo")
	      (c-decl (and c-key (assoc-ref udict c-key)))
	      (t-spec (and c-decl (sx-find 'type-spec (sx-ref c-decl 1)))))
	 (if (or (and c-key
		      (or (member c-key keep)
			  (member (cons 'pointer c-key) keep)))
		 (not c-decl)
		 (pointer-declr? declr))
	     (values specl declr)
	     (let ((r-specl (replace-type-spec specl t-spec)))
	       (re-expand r-specl declr)))))
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
;; Note: @var{keep} was formally keyword argument.@*
;; Note: works with @code{(struct . "foo")}@*
;; Note: works with @code{(pointer . "foo_t")}
;; @end deffn
;; idea: if we have a pointer to an undefined type, then use void*
;; @*BUG HERE? if we run into a struct then the struct members have not
;; been munged into udecls.  The behavior is actually NOT DEFINED.
;; @end deffn
(define* (expand-typerefs adecl udict #:optional (keep '()))

  ;; In the process of expanding typerefs it is crutial that routines which
  ;; expand parts return the original if no change made.  That is, if there
  ;; are no changes then @code{(eq? (expand-typerefs expr) expr)} is true.
  ;; If not, then infinite loop will result.

  (define (fix-param-list param-list)
    (let* ((tail (sx-tail param-list 1))
	   (xtail
	    (let loop ((xparams '()) (chg? #f) (params tail))
	      (if (null? params)
		  (if chg? (reverse xparams) tail)
		  (case (sx-tag (car params))
		    ((param-decl)
		     (let* ((param (car params))
			    (xparam (expand-typerefs param udict keep)))
		       (if (eq? param xparam)
			   (loop (cons param xparams) chg? (cdr params))
			   (loop (cons xparam xparams) #t (cdr params)))))
		    ((ellipsis)
		     (loop (cons (car params) xparams) chg? (cdr params))))))))
      (if (eq? xtail tail)
	  param-list
	  (sx-cons* (sx-tag param-list) (sx-attr param-list) xtail))))

  ;; This will check for function declrs and fix parameters.
  (define (fix-declr declr)
    (and
     declr
     (sx-match declr
       ((ident ,name) declr)
       ((bit-field . ,rest) declr)
       
       ((init-declr ,declr1 . ,rest)
	(let ((xdeclr (fix-declr declr1)))
	  (if (eq? xdeclr declr1) declr `(init-declr ,xdeclr . ,rest))))
       ((comp-declr ,declr1)
	(let ((xdeclr (fix-declr declr1)))
	  (if (eq? xdeclr declr1) declr `(comp-declr ,xdeclr))))
       ((param-declr ,declr1)
	(let ((xdeclr (fix-declr declr1)))
	  (if (eq? xdeclr declr1) declr `(param-declr ,xdeclr))))
       ((param-declr)
	declr)
       ((ary-declr ,declr1 ,array-spec)
	(let ((xdeclr (fix-declr declr1)))
	  (if (eq? xdeclr declr1) declr `(ary-declr ,xdeclr ,array-spec))))
       ((ary-declr ,dir-declr)
	(let ((xdeclr (fix-declr dir-declr)))
	  (if (eq? xdeclr dir-declr) declr `(ary-declr ,xdeclr))))
       ((ptr-declr ,pointer ,dir-declr)
	(let ((xdeclr (fix-declr dir-declr)))
	  (if (eq? xdeclr dir-declr) declr `(ptr-declr ,pointer ,xdeclr))))
       #;((abs-ptr-declr ,pointer ,abs-declr)
	(let ((xdeclr (fix-declr abs-declr)))
	  (if (eq? xdeclr abs-declr) declr `(abs-ptr-declr ,pointer ,xdeclr))))
       ((abs-ptr-declr ,pointer)
	declr)

       ((scope ,declr1)
	(let ((xdeclr (fix-declr declr1)))
	  (if (eq? xdeclr declr1) declr `(scope ,xdeclr))))
       ((abs-scope ,declr1)
	(let ((xdeclr (fix-declr declr1)))
	  (if (eq? xdeclr declr1) declr `(abs-scope ,xdeclr))))

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
       
       ;; declr-anon-array type-qual-list assn-expr
       ((declr-anon-array ,type-qual-list ,assn-expr) declr)
       ;; declr-anon-array type-qual-list 
       ((declr-anon-array (type-qual-list ,type-qual-tail)) declr)
       ;; ???? declr-anon-array assn-expr
       ((declr-anon-array ,assn-expr) declr)
       ;; declr-anon-array
       ((declr-anon-array) declr)
       ;; declr-star dir-abs-decl
       ((declr-star ,dir-abs-declr)
	(let ((xdeclr (fix-declr dir-abs-declr)))
	  (if (eq? xdeclr dir-abs-declr) declr
	      `(declr-star ,xdeclr))))
       ;; declr-star
       ((declr-star) declr)

       ;; ftn-declr
       ((ftn-declr ,dir-declr ,param-list)
	(let ((xdeclr (fix-declr dir-declr))
	      (xparam-list (fix-param-list param-list)))
	  (if (and (eq? xdeclr dir-declr) (eq? xparam-list param-list)) declr
	      `(ftn-declr ,xdeclr ,xparam-list))))
       
       ((abs-ftn-declr ,abs-declr ,param-list)
	(let ((xdeclr (fix-declr abs-declr))
	      (xparam-list (fix-param-list param-list)))
	  (if (and (eq? xdeclr abs-declr) (eq? xparam-list param-list)) declr
	      `(abs-ftn-declr ,xdeclr ,xparam-list))))
       ((anon-ftn-declr ,param-list)
	(let ((xparam-list (fix-param-list param-list)))
	  (if (eq? xparam-list param-list) declr
	      `(anon-ftn-declr ,xparam-list))))

       ;; declr-lists too
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
       
       (,_ (throw 'c99-error "c99/munge: unknown declarator: " declr)))))

  (let*-values (((tag attr orig-specl orig-declr)
		 (split-udecl adecl))
		((repl-specl repl-declr)
		 (expand-specl-typerefs orig-specl orig-declr udict keep)))
    (when #f
      (sferr "orig-specl, orig-declr\n")
      (pperr orig-specl) (pperr orig-declr)
      (sferr "\n")
      (sferr "repl-specl, repl-declr\n")
      (pperr repl-specl) (pperr repl-declr)
      (sferr "\n"))
    (let ((repl-declr (fix-declr repl-declr)))
      (if (and (eq? orig-specl repl-specl)
	       (eq? orig-declr repl-declr))
	  adecl ;; <= unchanged; return original
	  (sx-list tag attr repl-specl repl-declr)))))


;; === reify abstract declaration =====

(define (def-namer) "_")

;; @deffn {Procedure} reify-declr declr [#:namer proc]
;; This procedure turns tails of abstract declarations into init-declr's.
;; This is useful for sending through @code{udecl->mdecl} for the purpose
;; of processing with munge tools.
;; @end deffn
(define* (reify-declr declr #:optional (namer def-namer))

  (define (probe-declr declr)
    (sx-match declr
      ((ident ,name) declr)
      ((init-declr ,dcl) `(init-declr ,(probe-declr dcl)))
      ((comp-declr ,dcl) `(comp-declr ,(probe-declr dcl)))
      ((param-declr ,dcl) `(param-declr ,(probe-declr dcl)))
      ((param-declr) `(param-declr (ident ,(namer))))
      ((ptr-declr ,ptr ,dcl) `(ptr-declr ,ptr ,(probe-declr dcl)))
      ((ary-declr ,dcl . ,rest) `(ary-declr ,(probe-declr dcl) . ,rest))
      ((ftn-declr ,dcl ,pl) `(ftn-declr ,(probe-declr dcl) ,pl))
      ((scope ,dcl) `(scope ,(probe-declr dcl)))
      ((abs-ptr-declr ,ptr) `(ptr-declr ,ptr (ident ,(namer))))
      ((abs-ary-declr . ,rest) `(ary-declr (ident ,(namer)) . ,rest))
      ((abs-ftn-declr ,pl) `(ftn-declr (ident ,(namer)) ,pl))
      ;; Don't dive into function param lists
      #;((ftn-declr ,dcl ,pl)
       (let ((param-list
	      (sx-list (sx-tag pl) (sx-attr pl)
		       (map (lambda (d) (reify-decl d namer)) (sx-tail pl)))))
	 `(ftn-declr ,(probe-declr dcl) ,param-list)))
      #;((abs-ftn-declr ,pl)
       (let ((param-list
	      (sx-list (sx-tag pl) (sx-attr pl)
		       (map (lambda (d) (reify-decl d namer)) (sx-tail pl)))))
	 `(ftn-declr (ident ,(namer)) ,param-list)))
      (,_ (throw 'c99-error "c99/munge: unknown declarator: ~S" declr))))

  ;;(if declr (probe-declr declr) `(init-declr ,(namer))))
  (probe-declr declr))

(define* (reify-decl udecl #:optional (namer def-namer))
  (call-with-values
      (lambda () (split-udecl udecl))
    (lambda (tag attr specl declr)
      (let ((declr
	     (sx-match declr
	       ((init-declr-list . ,declrs)
		(sx-list
		 (sx-tag declr) (sx-attr declr)
		 (map (lambda (d) (reify-declr d namer)) (sx-tail declr))))
	       (,_ (reify-declr declr namer)))))
      (sx-list tag attr specl declr)))))

	   
;; === munged specification ============

;; @deffn {Procedure} udecl->mdecl udecl [#:namer def-namer]
;; @deffnx {Procedure} udecl->mdecl/comm udecl [#:def-comm ""]
;; Turn a stripped-down unit-declaration into an m-spec.  The second version
;; includes the comment. This assumes decls have been run through
;; @code{stripdown}.
;; @example
;; (decl (decl-spec-list (type-spec "double"))
;;       (init-declr-list (...))
;;       (comment "state vector"))
;; =>
;; ("x" "state vector" (array-of 10) (float "double")
;; @end example
;; @noindent
;; The optional keyword argument @var{namer} is a procdedure returning a string
;; to add for abstract declarators.  If an identifier is not provided, a
;; random identifier starting with @code{@} will be provided.
;; @end deffn
(define* (udecl->mdecl decl #:key (namer def-namer))

  (define (unwrap-pointer pointer tail)
    (sx-match pointer
      ((pointer (type-qual-list . ,type-qual) ,pointer)
       (unwrap-pointer pointer (cons '(pointer-to)  tail)))
      ((pointer (type-qual-list . ,type-qual)) (cons '(pointer-to) tail))
      ((pointer ,pointer) (unwrap-pointer pointer (cons '(pointer-to) tail)))
      ((pointer) (cons '(pointer-to) tail))
      (,_ (sferr "unwrap-pointer failed on:\n") (pperr pointer)
	  (throw 'nyacc-error "unwrap-pointer"))))

  (define* (unwrap-declr declr #:optional (tail '()))
    ;;(sferr "unwrap-declr  ~S  ~S\n" declr tail)
    (sx-match declr
      ((init-declr ,item) (unwrap-declr item tail))
      ((comp-declr ,item) (unwrap-declr item tail))
      ((param-declr ,item) (unwrap-declr item tail))
      ((param-declr) (cons (namer) tail))
      ((ident ,name) (cons name tail))
      ((ptr-declr ,ptr ,dcl)
       (unwrap-declr dcl (unwrap-pointer ptr tail)))
      ((abs-ptr-declr ,ptr) (cons* (namer) (unwrap-pointer ptr tail)))
      ((ary-declr ,dcl (type-qual . ,rest) . ,rest)
       (unwrap-declr `(ary-declr ,dcl . ,rest) tail))
      ((ary-declr ,dcl ,size) (unwrap-declr dcl (cons `(array-of ,size) tail)))
      ((ary-declr ,dcl) (unwrap-declr dcl (cons `(array-of) tail)))
      ((abs-ary-declr ,size) (cons* (namer) `(array-of ,size) tail))
      ((abs-ary-declr) (cons* (namer) `(array-of) tail))
      ((ftn-declr ,dcl ,param-list)
       (unwrap-declr dcl (cons `(function-returning ,param-list) tail)))
      ((abs-ftn-declr ,param-list)
       (cons* (namer) `(function-returning ,param-list) tail))
      ((scope ,expr) (unwrap-declr expr tail))
      ((bit-field (ident ,name) ,size) (cons* name `(bit-field ,size) tail))
      (,_
       (sferr "munge/unwrap-declr missed:\n")
       (pperr declr)
       (throw 'nyacc-error "c99/munge: udecl->mdecl failed")
       #f)))

  (let-values (((tag attr specl declr) (split-udecl decl)))
    (let* ((declr (or declr `(ident ,(namer))))
	   (stor-spec (and=> (sx-find 'stor-spec specl)
			     (lambda (sx) (sx-ref sx 1))))
	   (type-spec (and=> (sx-find 'type-spec specl) sx-tail))
	   (m-declr (unwrap-declr declr type-spec))
	   (m-declr (if (and (equal? stor-spec '(extern))
			     (not (equal? 'function-returning (caadr m-declr))))
	   (cons* (car m-declr) '(extern) (cdr m-declr)) m-declr)))
      ;;(pperr decl)
      ;;(sferr "m-declr:\n") (pperr m-declr)
      ;;(sferr "type-spec:\n") (pperr type-spec)      
      m-declr)))

;; --- last line ---
