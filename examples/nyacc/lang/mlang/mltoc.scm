;;; nyacc/lang/mlang/mltoc.scm

;; Copyright (C) 2018,2020 Matthew R. Wette
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

;;; Code:

(define-module (nyacc lang mlang mltoc)
  #:export (mlang-to-c99))

(use-modules (nyacc lang mlang parser))
(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang sx-util))

(use-modules (nyacc lang nx-util))
(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 pprint))

(use-modules ((srfi srfi-1) #:select (fold last)))
(use-modules (srfi srfi-9))		; define-record-type
(use-modules (srfi srfi-11))
(use-modules (sxml match))
(use-modules (sxml fold))		; fold-values
(use-modules (ice-9 regex))
(use-modules (ice-9 match))

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

;; === type system ====================

;;(define-mstruct sym (fields name uval type))
(define-record-type nxsym
  (make-nxsym name gsym)
  nxsym?
  (name nxsym-name)
  (gsym nxsym-gsym)
  (type nxsym-type set-nxsym-type!)	; (cdr mdecl)
  ;;(immutable? nxsym-immutable? set-nxsym-immutable!)
  )
;; is 

;; types:
;; scalar fixed
;; scalar float
;; struct
;; vector fixed
;; vector float
;; matrix float

;; typedef struct mldvec { uint16_t nelt; double *elts; } mldvec_t;
;; typedef struct mldmat { uint16_t nelt; double *elts; } mldmat_t;

;; structs - must check that user does not mutate input structs, unless
;; indicated by a pragma
;; but structs will always be passed by reference
;; but mutating scalars is OK.

(define (set-type! name type dict)
  (let* ((hit1 (lookup name dict))
	 (xsym (if (pair? hit1) (lookup (cadr hit1) dict) hit1)))
    (set-nxsym-type! xsym type)
    dict))
(define (get-type name dict)
  (let* ((hit1 (lookup name dict))
	 (xsym (if (pair? hit1) (lookup (cadr hit1) dict) hit1)))
    (nxsym-type xsym)))

(define push-scope nx-push-scope)
(define pop-scope nx-pop-scope)
(define lookup nx-lookup)
(define top-level? nx-top-level?)

;; The symbol table is set up like:
;; ("name" . (lexical "name-123")
;; ("name-123" . <nxsym>)

(define (add-nxsym tag name dict)
  (let* ((gsym (symbol->string (genxsym name)))
	 (xsym (make-nxsym name gsym)))
    (acons name (list tag gsym) (acons gsym xsym dict))))
(define (add-toplevel name dict)
  (add-nxsym 'global name dict))
(define (add-lexical name dict)
    (add-nxsym 'lexical name dict))
(define (add-lexicals . args)
  (let loop ((args args))
    (if (null? (cdr args)) (car args)
	(add-lexical (car args) (loop (cdr args))))))
(define (add-symbol name dict)
  (if (top-level? dict) (add-symbol name dict) (add-lexical name dict)))
(define (add-reference name dict)
  (add-nxsym 'reference name dict))
(define (add-return name dict)
  (let* ((gsym (symbol->string (genxsym name)))
	 (xsym (make-nxsym name gsym)))
    (acons name (list 'lexical gsym)
	   (acons "return" (list 'lexical gsym)
		  (acons gsym xsym dict)))))

(define (lookup-name ref dict)
  ;; The `(if (string? ...' ugliness is to deal with the sitution where
  ;; "return" returns something like `(lexical "c")' (???)
  ;;(if (string? ref) (sferr "lookup ~S => ~S\n" ref (lookup ref dict)))
  (nxsym-name (lookup (sx-ref (if (string? ref) (lookup ref dict) ref) 1) dict)))

;; ====================================

(define *udict* '())

;; determine if a comment is C code, if so, return it
(define (comm-c-code comm)
  (let ((cstr (let loop ((s comm))
		(if (char-whitespace? (string-ref s 0))
		    (loop (substring s 1))
		    s))))
    (cond
     ((string-prefix? "/*" cstr) cstr)
     ((string-prefix? "//" cstr) cstr)
     (else #f))))


;;(define my-help
;;  '(("__builtin" "mldvec_t" "mlivec_t" "mldmat_t")))

(define (parse-c99-file file)
  (with-input-from-file file
    (lambda ()
      (parse-c99 #:inc-dirs '(".") #:mode 'code))))

(define (parse-c99-string code)
  (with-input-from-string code
    (lambda () (parse-c99 #:inc-dirs '(".") #:mode 'code))))

(define (c99-code->udict code)
  (let* ((tree (or (parse-c99-string code)
		   (throw 'mltoc-error "error")))
	 (udict (c99-trans-unit->udict tree))
	)
    udict))

;; ====================================

;; if return type passed as pointer => ("x" (de-ref (float-type "double")))

;; This should update dict to
;; 1) add "return" as void
;; 2) update inputs as lexical
;; 3) update outputs as lexical or reference
;; match mlang function sig to c function sig, 
(define* (match-sigs inargs outargs udecl dict #:optional udict)
  (let ((dict (add-lexical "return" dict)))
    (set-type! "return" '((void)) dict)	; default return type
    (sx-match udecl
      ((xudecl (decl-spec-list (type-spec (void)))
	       (init-declr (ftn-declr (ident ,name) (param-list . ,params))))
       #f)
      ((xudecl (decl-spec-list (stor-spec (static)) (type-spec (void)))
	       (init-declr (ftn-declr (ident ,name) (param-list . ,params))))
       #f)
      ((udecl ,decl-spec-list
	      (init-declr (ftn-declr (ident ,name) (param-list . ,params))))
       (let* ((r-udecl `(udecl ,decl-spec-list (init-declr (ident "@"))))
	      (r-mdecl (udecl->mdecl r-udecl))
	      (cargs (map (lambda (param-decl)
			    (let* ((udecl (unitize-param-decl param-decl))
				   (mdecl (udecl->mdecl (cdar udecl))))
			      mdecl)) params)))
	 (let loop ((dict dict) (il0 inargs) (ol0 outargs) (cl cargs))
	   (cond
	    ((pair? il0)
	     (if (string=? (sx-ref (car il0) 1) (caar cl))
		 (let* ((name (caar cl))
			(dict (add-lexical name dict))
			(type (cdar cl)))
		   (set-type! name type dict)
		   (loop dict (cdr il0) ol0 (cdr cl)))
		 (begin
		   (sferr "file: line: param mismatch ~S VS ~S"
		       (caar il0) (caar cl))
		   (loop dict (cdr il0) ol0 (cdr cl)))))
	    ((null? ol0)
	     dict)
	    ((pair? cl)
	     (if (string=? (sx-ref (car ol0) 1) (caar cl))
		 (let* ((name (caar cl))
			(dict (add-lexical name dict))
			(type (cdar cl)))
		   (set-type! name type dict)
		   (loop dict il0 (cdr ol0) (cdr cl)))
		 (let* ((name (sx-ref (car ol0) 1))
			(dict (add-return name dict)) ; update "return"
			(type (cdr r-mdecl)))
		   (set-type! name type dict)
		   (loop dict il0 ol0 (cdr cl)))))
	    ((pair? ol0)
	     (let* ((name (sx-ref (car ol0) 1))
		    (dict (add-return name dict)) ; update "return"
		    (type (cdr r-mdecl)))
	       (set-type! name type dict)
	       (loop dict il0 (cdr ol0) cl)))
	    (else
	     (sferr "file: line: param mismatch [~S] = f(~S)\n" inargs outargs)
	     dict)))))
      (,_ (error "yucky")))))

(define (maybe-add-symbol name dict)
  (if (lookup name dict) dict (add-symbol name dict)))

(define (make-opcall op seed kseed kdict)
  (values seed kdict))

;; such a hack: like sx-attr-ref but provides the tail
;; (zx-attr-ref/x '(foo (@ (t (foo) (bar) (baz)))) 't) => ((foo) (bar) (baz))
(define (zx-attr-ref/x sx key)
  (assq-ref (cond ((null? sx) sx) ((pair? (car sx)) sx)
		  ((eqv? '@ (car sx)) (car sx)) ((sx-attr sx)) (else '())) key))

;; (ident (@ (gsym "foo-123")) "foo") => (fixed-type "double"), a mdecl
(define (tree-type tree dict)
  (cond
   ((zx-attr-ref/x tree 'type))
   ((sx-attr-ref tree 'gsym) =>
    (lambda (gsym) (and=> (assq-ref dict gsym) nxsym-type)))
   (else
    (sferr "oops:\n") (pperr tree) (error "oops"))))

(define (resolve-binop lt rt)
  (if (equal? lt rt) lt
      (match lt
	(`((float-type "double"))
	 (match rt
	   (`((float-type "double"))
	    rt))))))

;; Note: We are abusing the use of attributes (@) by not liminting to
;; a string in use of the "type"

(define (make-opcall1 kseed seed kdict)
  (let* ((form (reverse kseed)) (op (sx-tag form))
	 (lx (sx-ref form 1)) (lt (tree-type lx kdict))
	 (rx (sx-ref form 2)) (rt (and rx (tree-type rx kdict)))
	 (zt (if rt (resolve-binop lt rt) lt)))
    ;;(sferr "make-opcall1\n") (pperr (list op `(@ (type . ,zt)) lx rx))
    (values (cons (list op `(@ (type . ,zt)) lx rx) seed) kdict)))

(define rx-dcl1 (make-regexp "^>\\s*([^\\s]+)\\s+([^; ]+)"))

(define* (mk-param-decl sym #:key rval?)
  (let ((name (nxsym-name sym))
	(type (nxsym-type sym)))
    (cond
     ((string=? type "double")
      `(param-decl
	(decl-spec-list (type-spec (float-type "double")))
	,(if rval?
	     `(param-declr (ident ,name))
	     `(param-declr (ptr-declr (pointer) (ident ,name))))))
     (else
      (sferr "n=~S t=~S\n" name type)
      (error"9: missed")))))

(define (check-decls dict)
  (let loop ((dl '()) (dict dict))
    (cond
     ((eq? (caar dict) '@P) dl)
     #;((eq? (cadar dict) 'lexical)
      (sferr "LEXICAL\n")
      (loop dl (cdr dict)))
     (else
      (sferr "... ~S\n" (car dict))
      (loop dl (cdr dict))))))

;; ====================================

(define (ml->c99 exp opts)

  (define (rem-empties stmts)
    (filter (lambda (item) (not (eq? 'empty-stmt (sx-tag item)))) stmts))
  
  (define (fD tree seed dict) ;; => tree seed dict
    (sx-match tree

      ((ident ,name)
       (let ((id-ref (lookup name dict)))
	 (cond
	  (id-ref
	   (let ((tag (sx-tag id-ref)) (gsym (sx-ref id-ref 1)))
	     (case tag
	       ((reference)
		(values '() `(de-ref (ident (@ (gsym ,gsym)) ,name)) dict))
	       (else
		(values '() `(ident (@ (gsym ,gsym)) ,name) dict)))))
	  ((member name '("nargsin" "nargsout"))
	   (let* ((dict (maybe-add-symbol name dict))
		  (nref (lookup name dict))
		  (gsym (sx-ref nref 1)))
	     (values '() `(ident (@ (gsym ,gsym)) ,name) dict)))
	  (else
	   (sferr "variable used before defined: ~S\n" name)
	   (values '() `(ident (@ (gsym "UNDEFINED")) ,name) dict)))))

      ((fixed ,sval)
       (values '() `(const ,(string->number sval)) dict))

      ((float ,sval)
       (values '() `(const ,(string->number sval)) dict))

      ((string ,sval)
       (values '() `(const ,sval) dict))

      ((sel (ident ,name) ,expr)
       (values `(sel ,name ,expr) '() dict))

      ((switch ,expr . ,rest)
       ;; Convert
       ;;  (switch expr (case a stmtL) (case b stmtL) ... (otherwise stmtL))
       ;; to
       ;;  (xswitch expr (xif expr stmtL (xif expr stmtL ...  stmtL))
       (values
	`(xswitch ,expr
		  ,(let loop ((tail rest))
		     (cond
		      ((null? tail) '(empty-stmt))
		      ((eq? 'otherwise (sx-tag (car tail)))
		       (sx-ref (car tail) 1))
		      ((eq? 'case (sx-tag (car tail)))
		       `(xif (eq (ident "swx-val") ,(sx-ref (car tail) 1))
			     ,(sx-ref (car tail) 2) ,(loop (cdr tail))))
		      (else (error "unsupported case-expr")))))
	'()
	(acons '@L "switch" (add-lexicals "swx-val" (push-scope dict)))))

      ((if ,expr ,stmts . ,rest)
       ;; Convert
       ;;  (if expr stmt (elseif expr stmt) ... (else stmt))
       ;; to
       ;;  (xif expr stmt (xif expr stmt ...  stmt))
       (values
	`(xif ,expr ,stmts
	      ,(let loop ((tail rest))
		 (cond
		  ((null? tail) '(empty-stmt))
		  ((eq? 'else (sx-tag (car tail))) (sx-ref (car tail) 1))
		  ((eq? 'elseif (sx-tag (car tail)))
		   `(xif ,(sx-ref (car tail) 1) ; cond
			 ,(sx-ref (car tail) 2) ; then
			 ,(loop (cdr tail))))   ; else
		  (else (error "oops")))))
	'() dict))

      ((while . ,rest)
       (values tree '() dict))

      ((for (ident ,name) . ,rest)
       ;;(sferr "for:\n") (pperr tree)
       (let* ((ref (lookup name dict))
	      (dict (if (and ref (eq? 'lexical (car ref))) dict
			(add-symbol name dict)))
	      (dict (push-scope dict))
	      (dict (add-lexicals "break" "continue" dict)))
	 (values tree '() dict)))
      ((for . ,rest) (throw 'nyacc-error "syntax error: for"))

      ;; check lhs for input arg
      ((assn (@ . ,attr) (ident ,name) ,rhsx)	; assign variable
       (sferr "assn, rhsx=~S\n" rhsx)
       ;; if rhsx == (aref-or-call (ident ,n) (expr-list (fixed-colon-expr)))
       ;; then array copy
       (values `(var-assn (ident ,name) ,rhsx) '() (maybe-add-symbol name dict)))
      ((assn (@ . ,attr) (aref-or-call ,aexp ,expl) ,rhsx) ; assign element
       (values `(elt-assn ,aexp ,expl ,rhsx) '() dict))
      ((assn (@ . ,attr) (sel (ident ,name) ,expr) ,rhsx) ; assign member
       (values `(mem-assn (@ . ,attr) ,expr ,name ,rhsx) '() dict))
      ((assn . ,other) (throw 'nyacc-error "syntax error: assn"))

      ;; This is like
      ;; @example
      ;;   [x, y] = f(a)
      ;; @end example
      ;; @example
      ;; (call-with-values
      ;;   (lambda () (f a))
      ;;  (lambda (arg0 arg1 . $rest) (set! x arg0) (set! y arg1)))
      ;; @end example
      ((multi-assn (@ . ,attr) (lval-list . ,elts) ,rhsx)
       (let loop ((lvxl '()) (dict dict) (elts elts) (ix 0))
	 (if (null? elts)
	     (values
	      `(multi-assn (@ . ,attr) (lval-list . ,(reverse lvxl)) ,rhsx)
	      '() dict)
	     (let* ((n (string-append "arg" (number->string ix)))
		    (s (string->symbol n)) (g (genxsym n))
		    (rv `(lexical ,s ,g)))
n	       (sx-match (car elts)
		 ((ident ,name)
		  (loop (cons `(var-assn (ident ,name) ,rv) lvxl)
			(maybe-add-symbol name dict) (cdr elts) (1+ ix)))
		 ((aref-or-call ,ax ,xl)
		  (loop (cons `(elt-assn ,ax ,xl ,rv) lvxl)
			dict (cdr elts) (1+ ix)))
		 ((sel (ident ,name) ,expr)
		  (loop (cons `(mem-assn ,expr ,name ,rv) lvxl)
			dict (cdr elts) (1+ ix)))
		 (,_ (throw 'nyacc-error "bad lhs syntax")))))))
      ((multi-assn . ,rest) (throw 'nyacc-error "syntax error: multi-assn"))

      ((stmt-list . ,stmts)
       (values `(stmt-list . ,(rem-empties stmts)) '() dict))

      ((fctn-defn
	(fctn-decl (ident ,name) (ident-list . ,inargs) (ident-list . ,outargs)
		   . ,rest)
	,stmt-list)
       ;;(sferr "name=~S, udict:\n" name) (pperr *udict*) (quit)
       (let* ((dict (if (top-level? dict) (add-symbol name dict) dict))
	      (dict (push-scope dict))
	      (udecl (udict-ref *udict* name))
	      ;; "return" is inserted by match-sigs
	      (dict (match-sigs inargs outargs udecl dict))
	      (dict (acons '@F name dict))
	      ;; ensure last statement is a return
	      #;(tree (if (eq? 'return (sx-tag (last stmt-list))) tree
			`(fctn-defn ,(sx-ref tree 1)
				    ,(append stmt-list '((return))))))
	      )
	 (values tree '() dict)))
      ((fctn-defn . ,rest) (throw 'nyacc-error "syntax error: function def"))

      ((command (ident ,cname) . ,args)
       (unless (string=? cname "global") (error "bad command: ~S" cname))
       (values
	'() '()
	(fold (lambda (arg dict) (add-toplevel (sx-ref arg 1) dict))
	      dict args)))

      ((function-file . ,tail)
       ;; Here we add provide ability for forward refs to all functions.
       ;; Need to insert static int foo( ... );
       (values tree '()
	       (fold
		(lambda (tree dict)
		  (sx-match tree
		    ((fctn-defn (fctn-decl (ident ,name) . ,_1) . ,_2)
		     (if(top-level? dict) (push-scope (add-toplevel name dict))
			(set-type! name "static" (add-lexical name dict))))
		    (,_ dict)))
		dict tail)))

      ((comm ,comment)
       (cond
	((comm-c-code comment) =>
	 (lambda (code)
	   (let* ((c-tree (or (parse-c99-string code)
			      (throw 'mltoc-error "could not parse")))
		  ;; remove comment just used to signal mltoc
		  (c-tree `(trans-unit . ,(sx-tail c-tree 2)))
		  (udict (c99-trans-unit->udict c-tree)))
	     ;;(sferr "c-api:\n") (pperr c-tree)
	     (set! *udict* udict)
	     (values '() c-tree dict))))
	(else (values tree '() dict))))
       
      (,_
       (values tree '() dict))))

  (define (fU tree seed dict kseed kdict) ;; => seed dict
    ;; This routine rolls up processes leaves into the current branch.
    ;; We have to be careful about returning kdict vs dict.
    ;; Approach: always return kdict or (pop-scope kdict)
    (when #f
      (sferr "fU: ~S\n" (if (pair? tree) (car tree) tree))
      ;;(sferr "    kseed=~S\n    seed=~S\n" kseed seed)
      ;;(pperr tree)
      )
    ;; (case ((pair? tree) all stuff) (pair? kseed) ... (else 
    (if
     (null? tree) (if (null? kseed)
		      (values seed kdict)		; fD said ignore
		      (values (cons kseed seed) kdict)) ; fD replacement

     (case (car tree)

       ;; before leaving add a call to make sure all toplevels are defined
       ((*TOP*)
	(let ((tail (rtail kseed)))
	  (cond
	   ((null? tail) (values '(void) kdict)) ; just comments
	   (else (values (car kseed) kdict)))))

       ((comm)
	(values (cons `(comment ,(car kseed)) seed) kdict))

       ((script)
	(values seed kdict))

       ((function-file)
	;;(sferr "function-file:\n") (pperr (reverse kseed))
	(values (cons `(trans-unit . ,(rtail kseed)) seed) kdict))

       ((c99-trans-unit)
	(values (append (rtail kseed) seed) kdict))
	 
       ;; For functions, need to check kdict for lexicals and add them.
       ((fctn-defn)
	(let* ((tail (rtail kseed)) (defn (car tail))
	       (name (sx-ref* defn 1 1))
	       (udecl (udict-ref *udict* name))
	       (dcl-slist (sx-ref udecl 1))
	       (ftn-declr (sx-ref* udecl 2 1))
	       (comms (reverse (sx-tail (sx-ref defn 4))))
	       ;; return
	       (void-ret (equal? (get-type "return" kdict) '((void))))
	       (ret-name (lookup-name "return" kdict))
	       (ret-decl (unless void-ret
			   (mdecl->udecl
			    (cons ret-name (get-type ret-name kdict)))))
	       ;; clean up statement block:
	       (compd-stmt (cadr tail))
	       (items (sx-tail (sx-ref compd-stmt 1)))
	       ;; TODO: add decls for non-arg lexicals
	       (items (if void-ret items (cons ret-decl items)))
	       (items (if void-ret items
			  (if (eq? 'return (sx-tag (last items))) items
			      (append items `((return (ident ,ret-name)))))))
	       (compd-stmt `(compd-stmt (block-item-list . ,items)))
	       ;;
	       (fctn `(fctn-defn ,dcl-slist ,ftn-declr ,compd-stmt)))
	  ;;(sferr "ret-decl: ~S\n" ret-decl)
	  ;; The `append' puts function decl' comments in front of function.
	  (values (cons fctn (append comms seed)) (pop-scope kdict))))

       ;; fctn-decl: handled by fctn-defn case

       ((stmt-list)
	;;(sferr "stmt-list:\n") (pperr (reverse kseed)) ;; (quit)
	(values
	 (cons `(compd-stmt (block-item-list . ,(rtail kseed))) seed) kdict))

       ;; Statements
       ((empty-stmt)
	(values seed kdict))

       ((expr-stmt)
	(values (cons (car kseed) seed) kdict))

       ;; Assignment needs to deal with all left hand expressions.
       ((var-assn) ;; variable assignment
	(sferr "var-assn (+quit):\n") (pperr (reverse kseed)) (quit)
	(let* ((tail (rtail kseed))
	       (lhs (car tail)) (lt (tree-type lhs kdict))
	       (rhs (cadr tail)) (rt (tree-type rhs kdict))
	       )
	  (sferr "  lt=~S\n" lt) (sferr "  rt=~S\n" rt)
	  (cond
	   ((not lt) (set-type! (sx-ref lhs 1) rt kdict))
	   ((equal? lt rt))
	   (else (sferr "in var-assn, types don't match: ~S VS ~S\n" lt rt)
		 (quit)))
	  (values
	   (cons
	    `(expr-stmt (assn-expr (p-expr ,lhs) (eq "=") (p-expr ,rhs)))
	    seed) kdict)))

       ((elt-assn) ;; element assignment
	(sferr "not implemented: ~S, so I quit\n" (car tree)) (quit)
	(values seed kdict))

       ((mem-assn) ;; member assignment
	(sferr "not implemented: ~S, so I quit\n" (car tree)) (quit)
	(values seed kdict))

       ((multi-assn)
	(sferr "not implemented: ~S, so I quit\n" (car tree)) (quit)
	(values seed kdict))

       ;; looping
       ;; 1) octave does have break statement, and continue I think
       ;; 2) for needs index and should call ml:iter-first ml:iter-next
       ;; 3) BUG top-levels can be introduced here, but we pop scope
       ;;    so these need to be moved to function or global scope
       ;; 4) for-loops do not restrict scope of the iteration var
       
       ;; ("for" ident "=" expr term stmt-list "end"
       ((for) ;; TODO
	(sferr "not implemented: ~S, so I quit\n" (car tree)) (quit)
	(values seed (pop-scope kdict)))
       
       ((while)
	(sferr "not implemented: ~S, so I quit\n" (car tree)) (quit)
	(values seed kdict))

       ;; @code{if} converted to @code{xif} in fD
       ((xif)
	(sferr "not implemented: ~S, so I quit\n" (car tree)) (quit)
	(values seed kdict))
       
       ;; converted in @code{fD} from switch, case-list, case, otherwise
       ((xswitch)
	(sferr "not implemented: ~S, so I quit\n" (car tree)) (quit)
	(values seed kdict))

       ((return)
	(let* ((rty (get-type "return" kdict))
	       (ret (if (equal? rty '((void)))
			'(return)
			`(return (ident ,(lookup-name "return" kdict))))))
	  (sferr "ret=~S\n" ret)
	  (values (cons ret seed) kdict)))

       ((command) ;; TODO
	(sferr "not implemented: ~S, so I quit\n" (car tree)) (quit)
	(values seed kdict))

       ((expr-list)
	(values (cons kseed seed) kdict))

       ((colon-expr fixed-colon-expr)
	(sferr "not implemented: ~S, so I quit\n" (car tree)) (quit)
	(values seed kdict))

       ((and or) (make-opcall1 kseed seed kdict))
       ((eq ne lt gt le ge) (make-opcall1 kseed seed kdict))

       ((add sub mul div) (make-opcall1 kseed seed kdict))
       ((dot-add) (make-opcall 'ml:.+ seed kseed kdict))
       ((dot-sub) (make-opcall 'ml:.- seed kseed kdict))
       ((ldiv) (make-opcall 'ml:\ seed kseed kdict))
       ((pow) (make-opcall 'ml:^ seed kseed kdict))
       ((dot-mul) (make-opcall 'ml:.* seed kseed kdict))
       ((dot-div) (make-opcall 'ml:./ seed kseed kdict))
       ((dot-pow) (make-opcall 'ml:.^ seed kseed kdict))
       
       ((neg) (make-opcall 'ml:neg seed kseed kdict))
       ((pos) (make-opcall 'ml:pos seed kseed kdict))
       ((not) (make-opcall 'ml:not seed kseed kdict))
       
       ((transpose) (make-opcall 'ml:xpose seed kseed kdict))
       ((conj-transpose) (make-opcall 'ml:cj-xpose seed kseed kdict))

       ;; aref-or-call
       ((aref-or-call)
	(values seed kdict))

       ((sel)
	(values seed kdict))

       ;; @section Matrix Constructs
       ;; Static semantics will extract the following:
       ;; @itemize
       ;; @item 1-D matrices (aka vectors) with only scalar integers:
       ;; These can include @code{+-*} expressions.  Used for indices.
       ;; @item 2-D matrices with only scalar floats:
       ;; These can include @code{+-*/} expressions. More efficient than ...
       ;; @item other matrices:
       ;; If a matrix expression includes, say, a variable reference, then the
       ;; dimension of that variable can only be determined at run-time.
       ;; @end itemize

       ;; row
       ((row)
	(values seed kdict))
       
       ;; matrix TODO
       ((matrix)
	(values seed kdict))

       ((float-matrix)
	(values seed kdict))
       
       ((fixed-vector)
	(values seed kdict))

       ;; cell-array

       ;; ident, fixed, float, string, comm
       #;((X-ident)
	(let* ((form (reverse kseed))
	       (attr (sx-attr form))
	       (name (sx-ref form 1))
	       (xsym #f)
	       )
	  `(ident (@ (type ,type) . ,attr) ,name)))

       ;;((@) (values seed kdict))

       (else
	(cond
	 ((null? seed) (sferr "NULL\n") (values (reverse kseed) kdict))
	 (else (values (cons (reverse kseed) seed) kdict)))))))

  (define (fH leaf seed dict)
    (values (if (null? leaf) seed (cons leaf seed)) dict))

  (foldts*-values fD fU fH `(*TOP* ,exp) '() (acons '@top #t '())))

(define (mlang-to-c99 srcfile opts)
  (let* ((base (basename srcfile))
	 (dstfile (string-append (basename srcfile ".m") "_m.c"))
	 (tree (call-with-input-file srcfile
		 (lambda (iport) (read-mlang-file iport '())))))
    (pperr tree)
    (unless tree (error "compile failed"))
    (let ((ct (ml->c99 tree opts)))
      (call-with-output-file dstfile
	(lambda (oport)
	  (pretty-print-c99 ct oport)
	  (simple-format #t "wrote ~S\n" dstfile))))
    0))

;; --- last line ---
