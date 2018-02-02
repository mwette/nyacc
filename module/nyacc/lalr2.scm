(add-to-load-path "/Users/mwette/repo/sv/nyacc-master/module")
;;; system/base/lalrz.scm
;;;
;;; Copyright (C) 2014-2018 Matthew R. Wette
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
;;; along with this library; if not, see <http://www.gnu.org/licenses/>

;; I need to find way to preserve srconf, rrconf after hashify.
;; compact needs to deal with it ...

;; todo add token expression to compensate for use of symbol constants
;; that is, 'expr => (token expr) and expr

;; random name : lalr2 might be interpreted as LALR(2)
;;(define-module (system base lalrz)
(define-module (nyacc lalr2)
  #:export (*bison-version*
            bison-parser
	    )
;;  #:use-module ((srfi srfi-1) #:select (fold fold-right remove lset-union
;;					     lset-intersection lset-difference))
  #:use-module ((srfi srfi-9) #:select (define-record-type))
;;  #:use-module ((srfi srfi-43) #:select (vector-map vector-for-each vector-any))
  )
(use-modules (ice-9 pretty-print))

(define *bison-version* "0.01.0")

(define (fmtstr fmt . args)
  (apply simple-format #f fmt args))
(define (fmterr fmt . args)
  (apply simple-format #t fmt args))
(define (fmtout fmt . args)
  (apply simple-format #t fmt args))
(define fmt simple-format)

;;(define *tokens* (make-fluid '())) ;; wonder if we can hack tokens

;; --- import from lalr & lalr.upstream --------

(define-record-type lexical-token
  (make-lexical-token category source value)
  lexical-token?
  (category lexical-token-category)
  (source   lexical-token-source)
  (value    lexical-token-value))
(export make-lexical-token lexical-token?
	lexical-token-category lexical-token-source
	lexical-token-value)

(define-record-type source-location
  (make-source-location input line column offset length)
  source-location?
  (input   source-location-input)
  (line    source-location-line)
  (column  source-location-column)
  (offset  source-location-offset)
  (length  source-location-length))
(export make-source-location source-location?
	source-location-input source-location-line
	source-location-column source-location-offset
	source-location-length)

(define (source-location->source-properties loc)
  `((filename . ,(source-location-input loc))
    (line . ,(source-location-line loc))
    (column . ,(source-location-column loc))))
(export source-location->source-properties)

;; --------------------------------------

;; @deffn reserved? grammar-symbol
;; Determine whether the syntax argument is a reserved symbol, that is.
;; So instead of writing @code{'$fixed} for syntax one can write
;; @code{$fixed}.  We may want to change this to
;; @example
;; (reserved-terminal? grammar-symbol)
;; (reserved-non-term? grammar-symbol)
;; @end example
(define (reserved? grammar-symbol)
  ;; If the first character `$' then it's reserved.
  (eqv? #\$ (string-ref (symbol->string (syntax->datum grammar-symbol)) 0)))
  
;; @deffn {Syntax} lalr2-spec grammar => spec
;; This routine reads a grammar in a scheme-like syntax and returns an a-list.
;; @end deffn
(define-syntax parse-rhs
  (lambda (x)
    (syntax-case x (quote $$ $prec $empty)
      ((_ ($$ <guts> ...) <e2> ...)
       #'(cons '(action #f <guts> ...) (parse-rhs <e2> ...)))
      ;; other internal $-syntax
      ((_ ($prec <tok>) <e2> ...)
       #'(cons (cons 'prec (tokenize <tok>)) (parse-rhs <e2> ...)))
      ((_ $empty <e2> ...)	; TODO: propagate to processor
       #'(cons '(empty) (parse-rhs <e2> ...)))
      ;; terminals and non-terminals
      ((_ (quote <e1>) <e2> ...)
       #'(cons '(terminal . <e1>) (parse-rhs <e2> ...)))
      ((_ (<f> ...) <e2> ...)
       #'(cons (<f> ...) (parse-rhs <e2> ...)))
      ((_ <e1> <e2> ...)
       (identifier? (syntax <e1>)) ; fender to trap non-terminals
       (if (reserved? (syntax <e1>))
	   #'(cons '(terminal . <e1>) (parse-rhs <e2> ...))
	   #'(cons '(non-terminal . <e1>) (parse-rhs <e2> ...))))
      ((_ <e1> <e2> ...)
       #'(cons '(terminal . <e1>) (parse-rhs <e2> ...)))
      ((_) #'(list)))))

(define-syntax parse-rhs-list
  (syntax-rules ()
    ((_ (<ex> ...) <rhs> ...)
     (cons (parse-rhs <ex> ...) (parse-rhs-list <rhs> ...)))
    ((_) '())))


(define-syntax parse-grammar
  (syntax-rules ()
    ((_ (<lhs> <rhs> ...) <prod> ...)
     (cons (cons '<lhs> (parse-rhs-list <rhs> ...))
	   (parse-grammar <prod> ...)))
    ((_) '())))

(define-syntax tokenize
  (lambda (x)
    (syntax-case x ()
      ((_ <tk>) (identifier? (syntax <tk>)) #'(quote <tk>))
      ((_ <tk>) #'<tk>))))

(define-syntax tokenize-list
  (syntax-rules ()
    ((_ <tk1> <tk2> ...)
     (cons (tokenize <tk1>) (tokenize-list <tk2> ...)))
    ((_) '())))

(define-syntax parse-precedence
  (syntax-rules (left right nonassoc)
    ((_ (left <tk> ...) <ex> ...)
     (cons (cons 'left (tokenize-list <tk> ...))
	   (parse-precedence <ex> ...)))
    ((_ (right <tk> ...) <ex> ...)
     (cons (cons 'right (tokenize-list <tk> ...))
	   (parse-precedence <ex> ...)))
    ((_ (nonassoc <tk> ...) <ex> ...)
     (cons (cons 'nonassoc (tokenize-list <tk> ...))
	   (parse-precedence <ex> ...)))
    ((_ <tk> <ex> ...)
     (cons (list 'undecl (tokenize <tk>))
	   (parse-precedence <ex> ...)))
    ((_) '())))

(define-syntax bison-spec-1
  (syntax-rules (start expect notice prec< prec> grammar)
    ((_ (start <symb>) <e> ...)
     (cons (cons 'start '<symb>) (lalr-spec-1 <e> ...)))
    ((_ (expect <n>) <e> ...)
     (cons (cons 'expect <n>) (lalr-spec-1 <e> ...)))
    ((_ (notice <str>) <e> ...)
     (cons (cons 'notice <str>) (lalr-spec-1 <e> ...)))
    ((_ (prec< <ex> ...) <e> ...)
     (cons (cons 'precedence (parse-precedence <ex> ...))
	   (lalr-spec-1 <e> ...)))
    ((_ (prec> <ex> ...) <e> ...)
     (cons (cons 'precedence (reverse (parse-precedence <ex> ...)))
	   (lalr-spec-1 <e> ...)))
    ((_ (grammar <prod> ...) <e> ...)
     (cons (cons 'grammar (parse-grammar <prod> ...))
	   (lalr-spec-1 <e> ...))) 
    ((_) '())))

(define-syntax bison-parser
  (syntax-rules ()
    ((_ <expr> ...)
     '(identity (bison-spec-1 <expr> ...)))))

;; @deffn atomize terminal => object
;; Generate an atomic object for a terminal.   Expected terminals are strings,
;; characters and symbols.  This will convert the strings @code{s} to symbols
;; of the form @code{'$:s}.
(define (atomize terminal)
  (if (string? terminal)
      (string->symbol (string-append "$:" terminal))
      terminal))

(define (process-spec tree)

  ;; Make a new symbol. This is a helper for proxies and mid-rule-actions.
  ;; The counter here is the only @code{set!} in @code{process-spec}.
  ;; Otherwise, I believe @code{process-spec} is referentially transparent.
  (define maksy
    (let ((cntr 1))
      (lambda ()
	(let ((c cntr))
	  (set! cntr (1+ cntr))
	  (string->symbol (string-append "$P" (number->string c)))))))

  ;; Canonicalize precedence and associativity. Precedence will appear
  ;; as sets of equivalent items in increasing order of precedence
  ;; (e.g., @code{((+ -) (* /)}).  The input tree has nodes that look like
  ;; @example
  ;; '(precedence (left "+" "-") (left "*" "/"))
  ;; '(precedence ('then "else")
  ;; @end example
  ;; @noindent
  ;; =>
  ;; @example
  ;; (prec ((+ -) (* /)) ((then) (else)))
  ;; @end example
  (define (prec-n-assc tree)
    ;; prec-l; lt-assc-l rt-assc-l non-assc-l pspec
    (let iter ((pll '()) (pl '()) (la '()) (ra '()) (na '())
	       (spec '()) (tree tree))
      (cond
       ((pair? spec)
	;; item ~ ('left "+" "-") => a ~ 'left, tl ~ (#\+ #\-)
	(let* ((item (car spec)) (as (car item)) (tl (map atomize (cdr item))))
	  (case as
	    ((left)
	     (iter pll (cons tl pl) (append tl la) ra na (cdr spec) tree))
	    ((right)
	     (iter pll (cons tl pl) la (append tl ra) na (cdr spec) tree))
	    ((nonassoc)
	     (iter pll (cons tl pl) la ra (append tl na) (cdr spec) tree))
	    ((undecl)
	     (iter pll (cons tl pl) la ra na (cdr spec) tree)))))
       ((pair? pl)
	(iter (cons (reverse pl) pll) '() la ra na spec tree))
       ((pair? tree)
	(iter pll pl la ra na
	      (if (eqv? 'precedence (caar tree)) (cdar tree) '()) (cdr tree)))
       (else
	(list
	 `(prec . ,(reverse pll))
	 `(assc (left ,@la) (right ,@ra) (nonassoc ,@na)))))))

  (let* ((gram (assq-ref tree 'grammar))
	 (start-symbol (and=> (assq-ref tree 'start) atomize))
	 (start-rule (lambda () (list start-symbol)))
	 (add-el (lambda (e l) (if (member e l) l (cons e l))))
	 (pna (prec-n-assc tree)))
    ;; We sweep through the grammar to generate a canonical specification.
    ;; Note: the local rhs is used to hold RHS terms, but a
    ;; value of @code{'()} is used to signal "add rule", and a value of
    ;; @code{#f} is used to signal ``done, proceed to next rule.''
    ;; We use @code{tail} below to go through all remaining rules so that any
    ;; like LHS get absorbed before proceeding: This keeps LHS in sequence.
    ;; Note: code-comm and lone-comm are added to terminals so that they end
    ;; up in the match-table.  The parser will skip these if the automoton has
    ;; no associated transitions for these.  This allows users to parse for
    ;; comments in some rules but skip the rest.
    (let iter ((tl '($error $end))	; set of terminals
	       (head gram)   ; head of unprocessed productions
	       (rhs-l '())   ; list of RHSs being processed
	       (rhs #f))     ; RHS being processed
      (cond
       ((pair? rhs)
	(case (caar rhs)
	  ((terminal)
	   (iter (add-el (cdar rhs) tl) head rhs-l (cdr rhs)))
	  (else
	   (iter tl head rhs-l (cdr rhs)))))
       ((pair? rhs-l)
	(iter tl head (cdr rhs-l) (car rhs-l)))
       ((pair? head)
	(iter tl (cdr head) (car head) rhs-l))
       (else
	(simple-format #t "need to process terminals\n")
	(pretty-print pna)
	tl)))
    ))

;; @end itemize
;;; --- last line ---
