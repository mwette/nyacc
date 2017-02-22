;;; lang/c/cpp.scm
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

;; C preprocessor.  This is not complete.

(define-module (nyacc lang c99 cpp)
  #:export (parse-cpp-stmt
	    cpp-line->stmt
	    parse-cpp-expr
	    eval-cpp-expr
	    cpp-expand-text
	    expand-cpp-macro-ref
	    )
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc lang util)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (ice-9 match)
  )

(define c99-std-defs
  '("__DATE__" "__FILE__" "__LINE__" "__STDC__" "__STDC_HOSTED__"
    "__STDC_VERSION__" "__TIME__"))

(define (c99-std-def? str)
  (let iter ((defs c99-std-defs))
    (cond
     ((null? defs) #f)
     ((string=? (car defs) str) #t)
     (else (iter (cdr defs))))))

(define (c99-std-val str)
  (cond
   ((string=? str "__DATE__") "M01 01 2001")
   ((string=? str "__FILE__") "(unknown)")
   ((string=? str "__LINE__") 0)
   ((string=? str "__STDC__") 1)
   ((string=? str "__STDC_HOSTED__") 0)
   ((string=? "__STDC_VERSION__") 201701)
   ((string=? "__TIME__") "00:00:00")
   (else #f)))

;; @deffn read-ellipsis ch
;; read ellipsis
(define (read-ellipsis ch)
  (cond
   ((eof-object? ch) #f)
   ((char=? ch #\.) (read-char) (read-char) "...") ; assumes correct syntax
   (else #f)))

;; @deffn cpp-define => (define (name "ADD") (args "X" "Y") (repl "X+Y"))
;; output is like
;; @code{(name "ABC") (repl "123")} or
;; @code{(name "ABC") (args "X" "Y") (repl "X+Y")}
(define (cpp-define)

  (define (p-args la) ;; parse args
    (if (eq? la #\()
	(let iter ((args '()) (la (skip-il-ws (read-char))))
	  (cond
	   ((eq? la #\)) (reverse args))
	   ((read-c-ident la) =>
	    (lambda (arg) (iter (cons arg args) (skip-il-ws (read-char)))))
	   ((read-ellipsis la) =>
	    (lambda (arg) (iter (cons arg args) (skip-il-ws (read-char)))))
	   ((eq? la #\,) (iter args (skip-il-ws (read-char))))))
	(begin (if (char? la) (unread-char la)) #f)))

  (define (p-rest la) ;; parse rest
    (cond ((eof-object? la) "")
	  (else (unread-char la) (drain-input (current-input-port)))))

  (let* ((name (read-c-ident (skip-il-ws (read-char))))
	 (args (or (p-args (read-char)) '()))
	 (repl (p-rest (skip-il-ws (read-char)))))
    (if (pair? args)
	`(define (name ,name) (args . ,args) (repl ,repl))
	`(define (name ,name) (repl ,repl)))))
	

;; @deffn cpp-include
;; Parse CPP include statement.
(define (cpp-include)
  (let* ((beg-ch (skip-il-ws (read-char)))
	 (end-ch (if (eq? beg-ch #\<) #\> #\"))
	 (path (let iter ((cl (list beg-ch)) (ch (read-char)))
		 (if (eq? ch end-ch) (list->string (reverse (cons ch cl)))
		     (iter (cons ch cl) (read-char))))))
    `(include ,path)))

;; @deffn cpp-line->stmt line defs => (stmt-type text)
;; Parse a line from a CPP statement and return a parse tree.
;; @example
;; (parse-cpp-stmt "define X 123") => (define "X" "123")
;; (parse-cpp-stmt "if defined(A) && defined(B) && defined(C)"
;; => (if "defined(A) && defined(B) && defined(C)")
;; @end example
;; To evaluate the @code{if} statements use @code{parse-cpp-expr} and
;; @code{eval-cpp-expr}.
(define (cpp-line->stmt line)
  (define (rd-ident) (read-c-ident (skip-il-ws (read-char))))
  (define (rd-num) (and=> (read-c-num (skip-il-ws (read-char))) cdr))
  (define (rd-rest) (let ((ch (skip-il-ws (read-char))))
		      (if (not (eof-object? ch)) (unread-char ch))
		      (drain-input (current-input-port))))
  (with-input-from-string line
    (lambda ()
      (let ((cmd (string->symbol (read-c-ident (skip-il-ws (read-char))))))
	(case cmd
	  ((include) (cpp-include))
	  ((define) (cpp-define))
	  ((undef) `(undef ,(rd-ident)))
	  ((ifdef)
 	   `(if ,(string-append "defined(" (rd-ident) ")" (rd-rest))))
	  ((ifndef)
	   `(if ,(string-append "!defined(" (rd-ident) ")" (rd-rest))))
	  ((if elif else endif line error pragma) (list cmd (rd-rest)))
	  (else '(unknown "")))))))

(include-from-path "nyacc/lang/c99/mach.d/cpptab.scm")
(include-from-path "nyacc/lang/c99/mach.d/cppact.scm")

(define raw-parser
  (make-lalr-parser
   (list (cons 'len-v len-v) (cons 'pat-v pat-v) (cons 'rto-v rto-v)
	 (cons 'mtab mtab) (cons 'act-v act-v))))

(define (cpp-err fmt . args)
  (apply throw 'cpp-error fmt args))

;;.@deffn skip-il-ws ch
;; Skip in-line whitespace
(define skip-il-ws
  (let ((il-ws (list->char-set '(#\space #\tab))))
    (lambda (ch)
      (cond
       ((eof-object? ch) ch)
       ((char-set-contains? il-ws ch) (skip-il-ws (read-char)))
       (else ch)))))

;; Since we want to be able to get CPP statements with comment in tact
;; (e.g., for passing to @code{pretty-print-c99}) we need to remove
;; comments when parsing CPP expressions.  We convert a comm-reader
;; into a comm-skipper here.  And from that generate a lexer generator.
(define cpp-comm-skipper
  (let ((reader (make-comm-reader '(("/*" . "*/")))))
    (lambda (ch)
      (reader ch #f))))

;; generate a lexical analyzer per string
(define gen-cpp-lexer
  (make-lexer-generator mtab #:comm-skipper cpp-comm-skipper))

;; @deffn parse-cpp-expr text => tree
;; Given a string returns a cpp parse tree.  This is called by
;; @code{eval-cpp-expr}.  The text will have had all CPP defined symbols
;; expanded already so no identifiers should appear in the text.
;; A @code{cpp-error} will be thrown if a parse error occurs.
(define (parse-cpp-expr text)
  (with-throw-handler
   'nyacc-error
   (lambda ()
     (with-input-from-string text
       (lambda () (raw-parser (gen-cpp-lexer)))))
   (lambda (key fmt . args)
     (apply throw 'cpp-error fmt args))))

;; @deffn eval-cpp-expr tree dict => datum
;; Evaluate a tree produced from @code{parse-cpp-expr}.
;; The tree passed to this routine is 
(define (eval-cpp-expr tree dict)
  (letrec
      ((tx (lambda (tr ix) (list-ref tr ix)))
       (tx1 (lambda (tr) (tx tr 1)))
       (ev (lambda (ex ix) (eval-expr (list-ref ex ix))))
       (ev1 (lambda (ex) (ev ex 1)))	; eval expr in arg 1
       (ev2 (lambda (ex) (ev ex 2)))	; eval expr in arg 2
       (ev3 (lambda (ex) (ev ex 3)))	; eval expr in arg 3
       (eval-expr
	(lambda (tree)
	  (case (car tree)
	    ((fixed) (string->number (cnumstr->scm (tx1 tree))))
	    ((char) (char->integer (tx1 tree)))
	    ((defined) (if (assoc-ref dict (tx1 tree)) 1 0))
	    ((pre-inc post-inc) (1+ (ev1 tree)))
	    ((pre-dec post-dec) (1- (ev1 tree)))
	    ((pos) (ev1 tree))
	    ((neg) (- (ev1 tree)))
	    ((bw-not) (bitwise-not (ev1 tree)))
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
	    ((bw-or) (bitwise-ior (ev1 tree) (ev2 tree)))
	    ((bw-xor) (bitwise-xor (ev1 tree) (ev2 tree)))
	    ((bw-and) (bitwise-and (ev1 tree) (ev2 tree)))
	    ((or) (if (and (zero? (ev1 tree)) (zero? (ev2 tree))) 0 1))
	    ((and) (if (or (zero? (ev1 tree)) (zero? (ev2 tree))) 0 1))
	    ((cond-expr) (if (zero? (ev1 tree)) (ev3 tree) (ev2 tree)))
	    ((ident) (cpp-err "undefined identifier: ~S" (cadr tree)))
	    (else (error "incomplete implementation"))))))
    (eval-expr tree)))

;; Note: scan-cpp-input scans replacement text.  When identifiers are found
;; they are tested for expansion as follows:
;; @enumerate
;; @item If already expanded, then ignore.
;; @item If followed by @code{(}, then use @code{collect-args} to get the
;; arguments and ...
;; @item Otherwise insert the replacement text and continue scanning (at
;; first character of new replacement text.
;; @end enumerate

;; @deffn rtokl->string tokl => string
;; Convert reverse token-list to string.
(define (rtokl->string tokl)

  ;; Turn reverse chl into a string and insert it into the string list stl.
  (define (add-chl chl stl)
    (if (null? chl) stl (cons (list->string chl) stl)))

  ;; Works like this: Scan through the list of tokens (key-val pairs or lone
  ;; characters).  Lone characters are collected in a list (@code{chl}); pairs
  ;; are converted into strings and combined with list of characters into a
  ;; list of strings.  When done the list of strings is combined to one string.
  (let iter ((stl '()) (chl '()) (nxt #f) (tkl tokl))
    (cond
     (nxt (iter (cons nxt (add-chl chl stl)) '() #f tkl))
     ((null? tkl) (apply string-append (add-chl chl stl)))
     ((char? (car tkl)) (iter stl (cons (car tkl) chl) nxt (cdr tkl)))
     (else
      (match tkl
	((('arg . arg) 'dhash (key . val) . rest)
	 (iter stl chl nxt
	       (acons key (string-append val arg) (list-tail tkl 3))))

	(((key . val) 'dhash ('arg . arg) . rest)
	 (iter stl chl nxt
	       (acons 'arg (string-append arg val) (list-tail tkl 3))))

	((('arg . arg) 'hash . rest)
	 (iter stl chl (string-append "\"" arg "\"") (list-tail tkl 2)))

	((('comm . val) . rest)
	 (iter stl chl (string-append "/*" val " */") (cdr tkl)))

	((('ident . rval) ('ident . lval) . rest)
	 (iter stl chl (string-append " " rval) (cdr tkl)))

	(((key . val) . rest)
	 (iter stl chl val rest))

	(('space . rest)
	 (iter stl (cons #\space chl) nxt rest))

	(otherwise
	 (error "no match" tkl)))))))

;; @deffn scan-cpp-input argd used dict end-tok => string
;; Process replacement text from the input port and generate a (reversed)
;; token-list.  If end-tok, stop at, and push back, @code{,} or @code{)}.
;; If end-tok is @code{,} then read until @code{,} or @code{(}.
;; The argument @var{argd} is a dictionary (argument name, argument
;; value) pairs which will be expanded as needed.  This routine is called
;; by collect-args, expand-cpp-repl and cpp-expand-text.
(define (scan-cpp-input argd dict used end-tok)
  ;; Works like this: scan for tokens (comments, parens, strings, char's, etc).
  ;; Tokens are collected in a (reverse ordered) list (tkl) and merged together
  ;; to a string on return using @code{rtokl->string}.

  ;; We just scanned "defined", now need to scan the arg to inhibit expansion.
  ;; For example, we have scanned "defined"; we now scan "(FOO)" or "FOO", and
  ;; return "defined(FOO)" or "defined FOO".
  (define (scan-defined-arg)
    (let* ((ch (skip-il-ws (read-char))) (no-ec (not (char=? ch #\())))
      (let iter ((chl (list ch)) (ch (read-char)))
	(cond
	 ((eof-object? ch)
	  (if no-ec
	      (string-append "defined " (list->string (reverse chl)))
	      (cpp-err "illegal argument to `defined'")))
	 ((char-set-contains? c:ir ch)
	  (iter (cons ch chl) (read-char)))
	 (no-ec
	  (unread-char ch)
	  (string-append "defined " (list->string (reverse chl))))
	 ((char=? #\) (skip-il-ws ch))
	  (string-append "defined" (list->string (reverse (cons #\) chl)))))
	 (else
	  (cpp-err "illegal argument to  `defined'"))))))

  ;; token list is list of
  ;; 1) characters as char
  ;; 2) identifiers as string
  ;; 3) strings as '(string . <string>)
  ;; 4) 'hash 'dhash
  (let iter ((tkl '())		; token list of 
	     (lvl 0)		; level
	     (ch (read-char)))	; next character
    (cond
     ;; have item to add, but first add in char's
     ;;(nxt (iter (cons nxt (add-chl chl tkl)) '() #f lvl ch))
     ;; If end of string or see end-ch at level 0, then return.
     ((eof-object? ch) (rtokl->string tkl))
     
     ((and (eqv? end-tok ch) (zero? lvl))
      (unread-char ch) (rtokl->string tkl))
     ((and end-tok (char=? #\) ch) (zero? lvl))
      (unread-char ch) (rtokl->string tkl))
     
     ((read-c-comm ch #f) =>
      (lambda (cp) (iter (acons `comm (cdr cp) tkl) lvl (read-char))))
     
     ((char-set-contains? c:ws ch)
      (if (and (pair? tkl) (char? (car tkl)))
	  (iter (cons 'space tkl) lvl (read-char))
	  (iter tkl lvl (read-char))))
     
     ((char=? #\( ch) (iter (cons ch tkl) (1+ lvl) (read-char)))
     ((char=? #\) ch) (iter (cons ch tkl) (1- lvl) (read-char)))
     ((char=? #\# ch)
      (let ((ch (read-char)))
	(if (eqv? ch #\#)
	    (iter (cons 'dhash tkl) lvl (read-char))
	    (iter (cons 'hash tkl) lvl ch))))
     ((read-c-string ch) =>
      (lambda (st) (iter (acons 'string (cdr st) tkl) lvl (read-char))))
     ((read-c-ident ch) =>
      (lambda (iden)
	(if (equal? iden "defined")
	    ;; "defined" is a special case
	    (let ((arg (scan-defined-arg)))
	      (iter (acons 'defined arg tkl) lvl (read-char)))
	    ;; otherwise ...
	    (let* ((aval (assoc-ref argd iden))  ; lookup argument
		   (rval (assoc-ref dict iden))) ; lookup macro def
	      (cond
	       ((member iden used)	; name used
		(iter (cons iden tkl) lvl (read-char)))
	       (aval			; arg ref
		(iter (acons 'arg aval tkl) lvl (read-char)))
	       ((string? rval)		; cpp repl
		(iter (acons 'string rval tkl) lvl (read-char)))
	       ((pair? rval)		; cpp macro
		(let* ((argl (car rval)) (text (cdr rval))
		       (argd (collect-args argl argd dict used))
		       (newl (expand-cpp-repl text argd dict (cons iden used))))
		  (iter (acons 'string newl tkl) lvl (read-char))))
	       (else			; normal identifier
		(iter (acons 'ident iden tkl) lvl (read-char))))))))
     (else
      (iter (cons ch tkl) lvl (read-char))))))

;; @deffn collect-args argl argd dict used => argd
;; to be documented
;; I think argd is a passthrough for scan-cpp-input
;; argl: list of formal arguments in #define
;; argd: used? (maybe just a pass-through for scan-cpp-input
;; dict: dict of macro defs
;; used: list of already expanded macros
;; TODO clean this up
;; should be looking at #\( and eat up to matching #\)
(define (collect-args argl argd dict used)
  (let iter ((argl argl) (argv '()) (ch (skip-il-ws (read-char))))
    ;; ch should always be #\(, #\, or #\)
    (cond
     ((eqv? ch #\)) (reverse argv))
     ((null? argl) (cpp-err "arg count"))
     ((and (null? (cdr argl)) (string=? (car argl) "..."))
      (let ((val (scan-cpp-input argd dict used #\))))
	(iter (cdr argl) (acons "__VA_ARGS__" val argv) (read-char))))
     ((or (eqv? ch #\() (eqv? ch #\,))
      (let ((val (scan-cpp-input argd dict used #\,)))
	(iter (cdr argl) (acons (car argl) val argv) (read-char))))
     (else (error "coding error, ch=" ch)))))

;; @deffn expand-cpp-repl
;; to be documented
(define (expand-cpp-repl repl argd dict used)
  (with-input-from-string repl
    (lambda () (scan-cpp-input argd dict used #f))))

;; @deffn cpp-expand-text text dict => string
(define (cpp-expand-text text dict)
  (with-input-from-string text
    (lambda () (scan-cpp-input '() dict '() #f))))

;; @deffn expand-cpp-macro-ref ident dict => repl|#f
;; Given an identifier seen in C99 input, this checks for associated
;; definition in @var{dict} (generated from CPP defines).  If found,
;; the expansion is returned as a string.  If @var{ident} refers
;; to a macro with arguments, then the arguments will be read from the
;; current input.  The format of the @code{dict} entries are
;; @example
;; ("ABC" . "123")
;; ("MAX" ("X" "Y") . "((X)>(Y)?(X):(Y))")
;; @end example
(define (expand-cpp-macro-ref ident dict . rest)
  (let ((used (if (pair? rest) (car rest) '()))
	(rval (assoc-ref dict ident)))
    (cond
     ((not rval) #f)
     ((member ident used) ident)
     ((string? rval)
      (let ((expd (expand-cpp-repl rval '() dict (cons ident used))))
	expd))
     ((pair? rval)
      (let* ((argl (car rval)) (repl (cdr rval))
	     (argd (collect-args argl '() dict '()))
	     (expd (expand-cpp-repl repl argd dict (cons ident used))))
	expd))
     ((c99-std-val ident))
     (else #f))))

;;; --- last line ---
