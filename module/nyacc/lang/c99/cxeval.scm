;;; nyacc/lang/c99/c99eval.scm - evaluate constant expressions
;;;
;;; Copyright (C) 2018 Matthew R. Wette
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, see <http://www.gnu.org/licenses/>.

(define-module (nyacc lang c99 cxeval)
  #:export (parse-c99-cx
	    eval-c99-cx
	    )
  #:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc util)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-match)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module ((srfi srfi-43) #:select (vector-map vector-for-each))
  #:use-module (system foreign)
  ;;#:use-module (system base pmatch)
  )
(use-modules (ice-9 pretty-print))
(define pp pretty-print)

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
    ("unsigned long long int" . ,unsigned-long) ("_Bool" . ,int8)))

(define (sizeof-type name)
  (or (and=> (assoc-ref ffi-type-map name) sizeof)
      (throw 'nyacc-error "bad type")))

(define (sizeof-string-const name)
  #f)

(include-from-path "nyacc/lang/c99/mach.d/cxtab.scm")
(include-from-path "nyacc/lang/c99/mach.d/cxact.scm")

(define c99cx-raw-parser
  (make-lalr-parser
   `((len-v . ,c99cx-len-v) (pat-v . ,c99cx-pat-v)
     (rto-v . ,c99cx-rto-v) (mtab . ,c99cx-mtab)
     (act-v . ,c99cx-act-v))))

(define gen-cx-lexer
  (let* ((reader (make-comm-reader '(("/*" . "*/"))))
	 (comm-skipper (lambda (ch) (reader ch #f))))
    (make-lexer-generator c99cx-mtab
			  #:comm-skipper comm-skipper
			  #:chlit-reader read-c-chlit
			  #:num-reader read-c-num)))

(define (parse-c99-cx text)
  (with-throw-handler
   'nyacc-error
   (lambda ()
     (with-input-from-string text
       (lambda () (c99cx-raw-parser (gen-cx-lexer)))))
   (lambda (key fmt . args)
     (apply throw 'cpp-error fmt args))))

;; (sizeof type-name)
;; (type-name specificer-qualifier-list abstract-declarator)
;; (decl-spec-list 
;; (abs-decl
(define (eval-sizeof-type tree udict)
  (let* ((type-name (sx-ref tree 1))
	 (spec-list (sx-ref type-name 1))
	 (type-spec (assq 'type-spec (sx-tail spec-list 1)))
	 )
    (pp type-spec)
    (sx-match (sx-ref type-spec 1)
      ((fixed-type ,name)
       (let* ((ffi-type (assoc-ref ffi-type-map name)))
	 (sizeof ffi-type)))
      ((float-type ,name)
       (let* ((ffi-type (assoc-ref ffi-type-map name)))
	 (sizeof ffi-type)))
      (* (pp type-spec))
      )
  #t))

;; (sizeof unary-expr)
;;    (primary-expression			; S 6.5.1
;;     (identifier ($$ `(p-expr ,$1)))
;;     (constant ($$ `(p-expr ,$1)))
;;     (string-literal ($$ `(p-expr ,(tl->list $1))))
;;     ("(" expression ")" ($$ $2))
;;     ("(" "{" block-item-list "}" ")"
;;      ($$ `(stmt-expr (@ (extension "GNUC")) ,$3)))
;;     )
;;
(define (eval-sizeof-expr tree udict)
  (let* ((expr (sx-ref tree 1))
	 )
    (pp expr)
    (sx-match expr
      ((p-expr (string ,str))
       (string-length str))
      (* #f))))

(define (eval-ident tree udict)
;;   (let ((xxx)
;;  (cond
;;    ((assoc-ref dict (sx-ref tree 1)) => string->number) 0))
  #f)

(define* (eval-c99-cx tree #:optional (udict '()))
  (letrec
      ((tx (lambda (tr ix) (sx-ref tr ix)))
       (tx1 (lambda (tr) (sx-ref tr 1)))
       (ev (lambda (ex ix) (eval-expr (sx-ref ex ix))))
       (ev1 (lambda (ex) (ev ex 1)))	; eval expr in arg 1
       (ev2 (lambda (ex) (ev ex 2)))	; eval expr in arg 2
       (ev3 (lambda (ex) (ev ex 3)))	; eval expr in arg 3
       (eval-expr
	(lambda (tree)
	  (case (car tree)
	    ((fixed) (string->number (cnumstr->scm (tx1 tree))))
	    ((char) (char->integer (string-ref (tx1 tree) 0)))
	    ((pre-inc post-inc) (1+ (ev1 tree)))
	    ((pre-dec post-dec) (1- (ev1 tree)))
	    ((pos) (ev1 tree))
	    ((neg) (- (ev1 tree)))
	    ((not) (if (zero? (ev1 tree)) 1 0))
	    ((mul) (* (ev1 tree) (ev2 tree)))
	    ((div) (/ (ev1 tree) (ev2 tree)))
	    ((mod) (modulo (ev1 tree) (ev2 tree)))
	    ((add) (+ (ev1 tree) (ev2 tree)))
	    ((sub) (- (ev1 tree) (ev2 tree)))
	    ((lshift) (bitwise-arithmetic-shift-left (ev1 tree) (ev2 tree)))
	    ((rshift) (bitwise-arithmetic-shift-right (ev1 tree) (ev2 tree)))
	    ((lt) (if (< (ev1 tree) (ev2 tree)) 1 0))
	    ((le) (if (<= (ev1 tree) (ev2 tree)) 1 0))
	    ((gt) (if (> (ev1 tree) (ev2 tree)) 1 0))
	    ((ge) (if (>= (ev1 tree) (ev2 tree)) 1 0))
	    ((equal) (if (= (ev1 tree) (ev2 tree)) 1 0))
	    ((noteq) (if (= (ev1 tree) (ev2 tree)) 0 1))
	    ((bitwise-not) (lognot (ev1 tree)))
	    ((bitwise-or) (logior (ev1 tree) (ev2 tree)))
	    ((bitwise-xor) (logxor (ev1 tree) (ev2 tree)))
	    ((bitwise-and) (logand (ev1 tree) (ev2 tree)))
	    ((or) (if (and (zero? (ev1 tree)) (zero? (ev2 tree))) 0 1))
	    ((and) (if (or (zero? (ev1 tree)) (zero? (ev2 tree))) 0 1))
	    ((cond-expr) (if (zero? (ev1 tree)) (ev3 tree) (ev2 tree)))

	    ((sizeof-type) (eval-sizeof-type tree udict))
	    ((sizeof-expr) (eval-sizeof-expr tree udict))
	    ((ident) (eval-ident-value tree udict))
	    
	    ((p-expr) (ev1 tree))
	    ((cast) (ev2 tree))
	    (else (error "incomplete eval-c99-cx implementation"))))))
    (eval-expr tree)))

;; --- last line ---
