;;; nyacc/lang/c99/c99eval.scm - evaluate constant expressions

;; Copyright (C) 2018-2020 Matthew R. Wette
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

;;; Code:

(define-module (nyacc lang c99 cxeval)
  #:export (parse-c99-cx eval-c99-cx eval-sizeof-type)
  #:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc util)
  #:use-module ((nyacc lang util) #:select (make-tl tl-append tl->list))
  #:use-module (nyacc lang sx-util)
  #:use-module (nyacc lang arch-info)
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang c99 munge-base)
  #:use-module (rnrs arithmetic bitwise)
  ;;#:use-module ((srfi srfi-43) #:select (vector-map vector-for-each))
  #:use-module (system foreign)
  #:use-module (ice-9 match))

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

(define ffi-type-map
  `(("void" . ,void) ("float" . ,float) ("double" . ,double) ("short" . ,short)
    ("short int" . ,short) ("signed short" . ,short)
    ("signed short int" . ,short) ("int" . ,int) ("signed" . ,int)
    ("signed int" . ,int) ("long" . ,long) ("long int" . ,long)
    ("signed long" . ,long) ("signed long int" . ,long)
    ("unsigned short int" . ,unsigned-short)
    ("unsigned short" . ,unsigned-short)
    ("unsigned int" . ,unsigned-int) ("unsigned" . ,unsigned-int)
    ("unsigned long int" . ,unsigned-long) ("unsigned long" . ,unsigned-long)
    ("char" . ,int8) ("signed char" . ,int8) ("unsigned char" . ,uint8)
    ("wchar_t" . ,int) ("char16_t" . ,int16) ("char32_t" . ,int32)
    ("long long" . ,long) ("long long int" . ,long)
    ("signed long long" . ,long) ("signed long long int" . ,long)
    ("unsigned long long" . ,unsigned-long)
    ("unsigned long long int" . ,unsigned-long) ("_Bool" . ,int8)
    ("size_t" . ,size_t)))

(define (sizeof-type name)
  (or (and=> (assoc-ref ffi-type-map name) sizeof)
      (throw 'nyacc-error "bad type")))

;; (string "abc" "dev")
(define (sizeof-string-const value)
  #f)

(include-from-path "nyacc/lang/c99/mach.d/c99cx-act.scm")
(include-from-path "nyacc/lang/c99/mach.d/c99cx-tab.scm")

(define c99cx-raw-parser
  (make-lalr-parser
   (acons 'act-v c99cx-act-v c99cx-tables)))

(define gen-c99cx-lexer
  (let* ((reader (make-comm-reader '(("/*" . "*/"))))
	 (comm-skipper (lambda (ch) (reader ch #f))))
    (make-lexer-generator c99cx-mtab
			  #:comm-skipper comm-skipper
			  #:chlit-reader read-c-chlit
			  #:num-reader read-c-num)))

(define (parse-c99cx text)
  (with-throw-handler
      'nyacc-error
    (lambda ()
      (with-input-from-string text
	(lambda () (c99cx-raw-parser (gen-c99cx-lexer)))))
    (lambda (key fmt . args)
      (apply throw 'cpp-error fmt args))))

(define (expand-typename typename udict)
  (let* ((decl `(udecl (decl-spec-list
			(type-spec (typename ,typename)))
		       (declr (ident "_"))))
	 (xdecl (expand-typerefs decl udict))
	 (xname (and xdecl (sx-ref* xdecl 1 1 1 1))))
    xname))

(define* (sizeof-specdec specl declr #:optional (udict '()) (ddict '()))

  ;; (decl attr specl (declr-list declr1 declr2 ...))
  ;; => ((decl attr specl declr1) (decl attr specl declr2) ...)
  (define (splitup-decl decl)
    (let ((tag (sx-tag decl)) (attr (sx-attr decl))
	  (specl (sx-ref decl 1)) (dclrl (sx-ref decl 2)))
      (map (lambda (declr) (sx-list tag attr specl declr)) (sx-tail dclrl))))

  ;; Update running struct size (rs) given new item s and a.
  (define (incr-size s a rs)
    (+ s (* a (quotient (+ rs (1- a)) a))))

  ;; Update running union size (rs) given new item s and a.
  (define (maxi-size s a rs)
    (max s rs))

  (define (exec/decl decl size base-align update)
    (let ((mtail (sx-tail (sx-find 'type-spec (sx-ref decl 1))))
	  (declrs (sx-tail (sx-ref decl 2))))
      (let loop ((size size) (base-align base-align) (declrs declrs))
	(if (null? declrs)
	    (values size base-align)
	    (call-with-values
		(lambda ()
		  (sizeof-mtail
		   ;;(cdr (udecl->mdecl `(decl ,specl ,(car declrs))))))
		   (cdr (m-unwrap-declr (car declrs) mtail))))
	      (lambda (elt-size elt-align)
		(loop (update elt-size elt-align size)
		      (max elt-align base-align) (cdr declrs))))))))

  (define (incr/decl decl size base-align)
    (exec/decl decl size base-align incr-size))
  
  (define (maxi/decl decl size base-align)
    (exec/decl decl size base-align maxi-size))

  (define (sizeof-mtail mtail)
    (match mtail
      (`((pointer-to) . ,rest)
       (values (sizeof-basetype '*) (alignof-basetype '*)))
      (`((fixed-type ,name))
       (values (sizeof-basetype name) (alignof-basetype name)))
      (`((float-type ,name))
       (values (sizeof-basetype name) (alignof-basetype name)))
      (`((array-of ,size) . ,rest)
       (let ((mult (eval-c99-cx size)))
	 (call-with-values
	     (lambda () (sizeof-mtail rest))
	   (lambda (size align)
	     (values (* mult size) align)))))

      (`((struct-def (field-list . ,fields)))
       (let loop ((size 0) (align 0) (flds fields))
	 (if (null? flds)
	     (values (incr-size 0 align size) align)
	     (call-with-values
		 (lambda () (incr/decl (car flds) size align))
	       (lambda (size align)
		 (loop size align (cdr flds)))))))

      (`((union-def (field-list . ,fields)))
       (let loop ((size 0) (align 0) (flds fields))
	 (if (null? flds)
	     (values (incr-size 0 align size) align)
	     (call-with-values
		 (lambda () (maxi/decl (car flds) size align))
	       (lambda (size align)
		 (loop size align (cdr flds)))))))

      (_ (sferr "c99/eval-sizeof-type: missed\n") (pperr mtail)
	 (quit)
	 (throw 'nyacc-error "coding error"))))

  (let* ((udecl `(udecl ,specl ,declr))
	 (xdecl (expand-typerefs udecl udict ddict))
	 (mdecl (udecl->mdecl xdecl)))
    (sizeof-mtail (cdr mdecl))))

;; @deffn {Procedure} eval-sizeof-type tree [udict ddict]
;; => (values sizeof-val align-of)
;; @end deffn
(define* (eval-sizeof-type tree #:optional (udict '()) (ddict '()))
  (let* ((type-name (sx-ref tree 1))
	 (specl (sx-ref type-name 1))
	 (declr (or (sx-ref type-name 2) '(param-declr))))
    (sizeof-specdec specl declr udict ddict)))

;; need to map expression to type, then sizeof type
;; (sizeof unary-expr)
;;    (primary-expression			; S 6.5.1
;;     (identifier ($$ `(p-expr ,$1)))
;;     (constant ($$ `(p-expr ,$1)))
;;     (string-literal ($$ `(p-expr ,(tl->list $1))))
;;     ("(" expression ")" ($$ $2))
;;     ("(" "{" block-item-list "}" ")"
;;      ($$ `(stmt-expr (@ (extension "GNUC")) ,$3)))
;;     )
(define (eval-sizeof-expr tree udict)
  (let* ((expr (sx-ref tree 1)))
    (sx-match expr
      ((p-expr (string . ,strl))
       (let loop ((l 0) (sl strl))
	 (if (pair? sl) (loop (+ l (string-length (car sl))) (cdr sl)) l)))
      (,_
       (throw 'c99-error "failed to expand sizeof expr ~S" expr)))))

(define (eval-ident name udict ddict)
  (and=>
   (assoc-ref ddict name)
   (lambda (hit)
     ;; This should actually go through the cpp-expander first methinks.
     (and (string? hit)
	  (let ((expr (parse-cpp-expr hit)))
	    (eval-c99-cx expr udict ddict))))))

;; @deffn {Procedure} eval-c99-cx tree [udict [ddict]] [#:fail-proc fail-proc]
;; Evaluate the constant expression or return #f
;; If @code{fail-proc} is provided it is called with the tree that could not
;; be parsed.
;; @end deffn
(define* (eval-c99-cx tree #:optional udict ddict #:key fail-proc)
  (define (fail) #f)
  (letrec
      ((ev (lambda (ex ix) (eval-expr (sx-ref ex ix))))
       (ev1 (lambda (ex) (ev ex 1)))	; eval expr in arg 1
       (ev2 (lambda (ex) (ev ex 2)))	; eval expr in arg 2
       (ev3 (lambda (ex) (ev ex 3)))	; eval expr in arg 3
       (uop (lambda (op ex) (and op ex (op ex))))
       (bop (lambda (op lt rt) (and op lt rt (op lt rt))))
       (eval-expr
	(lambda (tree)
	  (case (car tree)
	    ((fixed) (string->number (cnumstr->scm (sx-ref tree 1))))
	    ((float) (string->number (cnumstr->scm (sx-ref tree 1))))
	    ((char) (char->integer (string-ref (sx-ref tree 1) 0)))
	    ((string) (string-join (sx-tail tree 1) ""))
	    ((pre-inc post-inc) (uop 1+ (ev1 tree)))
	    ((pre-dec post-dec) (uop 1- (ev1 tree)))
	    ((pos) (and tree (ev1 tree)))
	    ((neg) (uop - (ev1 tree)))
	    ((not) (and tree (if (equal? 0 (ev1 tree)) 1 0)))
	    ((mul) (bop * (ev1 tree) (ev2 tree)))
	    ((div) (bop / (ev1 tree) (ev2 tree)))
	    ((mod) (bop modulo (ev1 tree) (ev2 tree)))
	    ((add) (bop + (ev1 tree) (ev2 tree)))
	    ((sub) (bop - (ev1 tree) (ev2 tree)))
	    ((lshift) (bop bitwise-arithmetic-shift-left (ev1 tree) (ev2 tree)))
	    ((rshift) (bop bitwise-arithmetic-shift-right (ev1 tree) (ev2 tree)))
	    ((lt) (if (bop < (ev1 tree) (ev2 tree)) 1 0))
	    ((le) (if (bop <= (ev1 tree) (ev2 tree)) 1 0))
	    ((gt) (if (bop > (ev1 tree) (ev2 tree)) 1 0))
	    ((ge) (if (bop >= (ev1 tree) (ev2 tree)) 1 0))
	    ((eq) (if (bop = (ev1 tree) (ev2 tree)) 1 0))
	    ((ne) (if (bop = (ev1 tree) (ev2 tree)) 0 1))
	    ((bitwise-not) (uop lognot (ev1 tree)))
	    ((bitwise-or) (bop logior (ev1 tree) (ev2 tree)))
	    ((bitwise-xor) (bop logxor (ev1 tree) (ev2 tree)))
	    ((bitwise-and) (bop logand (ev1 tree) (ev2 tree)))
	    ;;
	    ((or)
	     (let ((e1 (ev1 tree)) (e2 (ev2 tree)))
	       (if (and e1 e2) (if (and (zero? e1) (zero? e2)) 0 1) #f)))
	    ((and)
	     (let ((e1 (ev1 tree)) (e2 (ev2 tree)))
	       (if (and e1 e2) (if (or (zero? e1) (zero? e2)) 0 1) #f)))
	    ((cond-expr)
	     (let ((e1 (ev1 tree)) (e2 (ev2 tree)) (e3 (ev3 tree)))
	       (if (and e1 e2 e3) (if (zero? e1) e3 e2) #f)))
	    ;;
	    ((sizeof-type)
	     (catch 'c99-error
	       (lambda () (eval-sizeof-type tree udict))
	       (lambda (key fmt . args)
		 (cond
		  (fail-proc (fail-proc tree fmt args))
		  (else 
		   (sferr "eval-c99-cx: ") (apply sferr fmt args)
		   (newline (current-error-port)) #f)))))
	    ((sizeof-expr)
	     (catch 'c99-error
	       (lambda () (eval-sizeof-expr tree udict))
	       (lambda (key fmt . args)
		 (cond
		  (fail-proc (fail-proc tree fmt args))
		  (else 
		   (sferr "eval-c99-cx: ") (apply sferr fmt args)
		   (newline (current-error-port)) #f)))))
	    ((ident) (eval-ident (sx-ref tree 1) udict ddict))
	    ((p-expr) (ev1 tree))
	    ((cast) (ev2 tree))
	    ((fctn-call) #f)		; assume not constant
	    ;;
	    ;; TODO 
	    ((comp-lit) (fail))		; return a bytearray
	    ((comma-expr) (fail))
	    ((i-sel) (fail))
	    ((d-sel) (fail))
	    ((array-ref) (fail))
	    ;; 
	    (else (fail))))))
    (eval-expr tree)))
 
;; --- last line ---
