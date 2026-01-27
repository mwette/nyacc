;;; lang/c/cpp.scm - C preprocessor

;; Copyright (C) 2015-2026 Matthew Wette
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

;; There are two parts to the C preprocessor.  First, is evaluating #-lines.
;; The second is expanding macros in the C program (as well as in #-lines).
;; The following #-statments are handled by line->stmt:
;; * include, include_next
;; * define, undef
;; * ifdef, ifndef
;; * if, elif, else, endif
;; * line, error, warning, pragma
;; The parser calls `expand-cpp-macro-ref' on all identifiers when parsing
;; C code.

;; ref: https://gcc.gnu.org/onlinedocs/gcc-3.0.1/cpp_3.html

;;; Code:

(define-module (nyacc lang c99 cpp)
  #:export (cpp-line->stmt
	    expand-cpp-macro-ref
	    eval-cpp-cond-text          ; #if etc
	    find-incl-in-dirl           ; #include
            tokenize-cpp-string         ; #define
	    ;; not used directly
            macro-expand-text
	    parse-cpp-expr
	    eval-cpp-expr)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (append-reverse))
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc lang sx-util)
  #:use-module ((nyacc lang util) #:select (report-error)))

(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(use-modules (ice-9 pretty-print))
(define* (pperr exp #:optional (plp "  "))
  (pretty-print exp (current-error-port) #:per-line-prefix plp))

(define rls reverse-list->string)

(define c99-std-defs
  '("__DATE__" "__FILE__" "__LINE__" "__TIME__"
    "__STDC__" "__STDC_HOSTED__" "__STDC_VERSION__"))

(define (c99-std-def? str)
  (let loop ((defs c99-std-defs))
    (cond
     ((null? defs) #f)
     ((string=? (car defs) str) #t)
     (else (loop (cdr defs))))))

(define (c99-std-val str sl)
  (cond
   ((string=? str "__DATE__") '(($string . "M01 01 2001")))
   ((string=? str "__FILE__")
    `(($string . ,(or (assq-ref sl 'filename) "(unknown)"))))
   ((string=? str "__LINE__")
    `(($string . ,(number->string (or (assq-ref sl 'line) 0)))))
   ((string=? str "__TIME__") '(($strings . "00:00:00")))
   ((string=? str "__STDC__") '(($strings . "1")))
   ((string=? str "__STDC_HOSTED__") '(($strings . "0")))
   ((string=? str "__STDC_VERSION__") '(($strings . "199901L")))
   (else #f)))

(define inline-whitespace (list->char-set '(#\space #\tab)))

;; Skip in-line whitespace
(define (skip-il-ws ch)
  (cond
   ((eof-object? ch) ch)
   ((char-set-contains? inline-whitespace ch) (skip-il-ws (read-char)))
   (else ch)))

;; This reads the rest of the input, with ch and returns a string;
;; Replaces get-string-all from (ice-9 textual-ports).
(define (read-rest ch)
  (reverse-list->string
   (let loop ((res '())(ch ch))
     (if (eof-object? ch) res
         (loop (cons ch res) (read-char))))))

;; Not sure about this. We want to turn a list of tokens into a string
;; with proper escapes.
(define (esc-c-str str)
  (list->string
   (string-fold-right
    (lambda (ch chl)
      (case ch
	((#\\ #\") (cons* #\\ ch chl))
	(else (cons ch chl))))
    '() str)))

(define (unesc-c-str str)
  (list->string
   (string-fold-right
    (lambda (ch chl)
      (if (char=? ch #\\) chl (cons ch chl)))
    '() str)))

(define ident-like? (make-ident-like-p read-c-ident))

(define (read-ellipsis ch)
  (cond
   ((eof-object? ch) #f)
   ((char=? ch #\.) (read-char) (read-char) "...") ; assumes correct syntax
   (else #f)))

;; @deffn {Procedure} find-incl-in-dirl file dirl [next] => path
;; Find path to include file expression, (i.e., @code{<foo.h>} or
;; @code{"foo.h"}.  If @code{"foo.h"} form look in current directory first.
;; If @var{next} (default false) is true then remove current directory from
;; search path.
;; @*Refs:
;; @itemize
;; @item https://gcc.gnu.org/onlinedocs/cpp/Search-Path.html
;; @item https://gcc.gnu.org/onlinedocs/cpp/Wrapper-Headers.html
;; @end itemize
;; @end deffn
(define* (find-incl-in-dirl file dirl #:optional (next #f))
  (let* ((cid (and=> (port-filename (current-input-port)) dirname))
	 (file-type (string-ref file 0)) ;; #\< or #\"
	 (file-name (substring file 1 (1- (string-length file))))
	 (dirl (if (and cid (char=? #\" file-type)) (cons cid dirl) dirl)))
    (if (char=? #\/ (string-ref file-name 0))
        file-name
        (let loop ((dirl dirl))
          (if (null? dirl) #f
	      (if (and next (string=? (car dirl) cid))
	          (loop (cdr dirl))
	          (let ((p (string-append (car dirl) "/" file-name)))
		    (if (access? p R_OK) p (loop (cdr dirl))))))))))

;; @deffn {Procedure} cpp-include
;; Parse CPP include statement.
;; @end deffn
(define (cpp-include)
  (define (read-to-end cl ch end-ch)
    (if (eq? ch end-ch)  (reverse-list->string (cons ch cl))
	(read-to-end (cons ch cl) (read-char) end-ch)))
  (let ((ch (skip-il-ws (read-char))))
    (cond
     ((char=? ch #\<) (read-to-end (list #\<) (read-char) #\>))
     ((char=? ch #\") (read-to-end (list #\") (read-char) #\"))
     ((read-c-ident ch))
     (else (throw 'cpp-error "bad include")))))

;; @deffn {Procedure} cpp-define
;; Reads CPP define from current input and generates a t
;; expression.
;; @example
;;   (define (name "ABC") (repl "123"))
;; OR
;;   (define (name "ABC") (args "X" "Y") (repl "X+Y"))
;; @example
;; @end deffn
(define (cpp-define)

  (define (p-args la) ;; parse args
    (if (eq? la #\()
	(let loop ((args '()) (la (skip-il-ws (read-char))))
	  (cond
	   ((eq? la #\)) (reverse args))
           ((read-c-comm la) (loop args (skip-il-ws (read-char))))
	   ((read-c-ident la) =>
	    (lambda (arg) (loop (cons arg args) (skip-il-ws (read-char)))))
	   ((read-ellipsis la) =>
	    (lambda (arg) (loop (cons arg args) (skip-il-ws (read-char)))))
	   ((eq? la #\,) (loop args (skip-il-ws (read-char))))))
	(begin (if (char? la) (unread-char la)) #f)))

  (define (p-rest la)
    (read-rest la))

  (let* ((name (let loop ((ch (skip-il-ws (read-char))))
		 (cond
		  ((eof-object? ch) (throw 'cpp-error "bad #define"))
		  ((read-c-ident ch))
		  ((cpp-comm-skipper ch) (loop (skip-il-ws (read-char))))
		  (else (throw 'cpp-error "bad #define")))))
	 (args (p-args (read-char)))
	 (repl (p-rest (skip-il-ws (read-char)))))
    (if args
	`(define (name ,name) (args . ,args) (repl ,repl))
	`(define (name ,name) (repl ,repl)))))

;; @deffn {Procedure} cpp-line->stmt line defs => (stmt-type text)
;; Parse a line from a CPP statement and return a parse tree.
;; @example
;; (parse-cpp-stmt "define X 123") => (define "X" "123")
;; (parse-cpp-stmt "if defined(A) && defined(B) && defined(C)"
;; => (if "defined(A) && defined(B) && defined(C)")
;; @end example
;; To evaluate the @code{if} statements use @code{parse-cpp-expr} and
;; @code{eval-cpp-expr}.
;; @end deffn
(define (cpp-line->stmt line)
  (define (rd-id) (read-c-ident (skip-il-ws (read-char))))
  (define (rd-num) (and=> (read-c-num (skip-il-ws (read-char))) cdr))
  (define (rd-rest) (read-rest (skip-il-ws (read-char))))
  (with-input-from-string line
    (lambda ()
      (let ((ch (skip-il-ws (read-char))))
	(cond
	 ((read-c-ident ch) =>
	  (lambda (cmdstr)
	    (case (string->symbol cmdstr)
	      ((include) `(include ,(cpp-include)))
	      ((include_next) `(include-next ,(cpp-include)))
	      ((define) (cpp-define))
	      ((undef) `(undef ,(rd-id)))
	      ((ifdef) `(if ,(string-append "defined(" (rd-id) ")" (rd-rest))))
	      ((ifndef) `(if ,(string-append "!defined(" (rd-id) ")" (rd-rest))))
	      ((if elif else endif line error warning pragma)
	       (list (string->symbol cmdstr) (rd-rest)))
	      (else
	       (list 'warning (simple-format #f "unknown CPP: ~S" line))))))
	 ((read-c-num ch) => (lambda (num) `(line ,num ,(rd-rest))))
	 (else '(null)))))))


(include-from-path "nyacc/lang/c99/mach.d/cpp-tab.scm")
(include-from-path "nyacc/lang/c99/mach.d/cpp-act.scm")

(define cpp-raw-parser
  (make-lalr-parser (acons 'act-v cpp-act-v cpp-tables)))

(define (cpp-err fmt . args)
  (apply throw 'cpp-error fmt args))

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
  (make-lexer-generator cpp-mtab
			#:comm-skipper cpp-comm-skipper
			#:chlit-reader read-c-chlit
			#:num-reader read-c-num))

;; @deffn {Procedure} parse-cpp-expr text => tree
;; Given a string returns a cpp parse tree.  This is called by
;; @code{eval-cpp-expr}.  The text will have had all CPP defined symbols
;; expanded already so no identifiers should appear in the text.
;; A @code{cpp-error} will be thrown if a parse error occurs.
;; @end deffn
(define (parse-cpp-expr text)
  (with-throw-handler
      'nyacc-error
    (lambda ()
      (with-input-from-string text
        (lambda () (cpp-raw-parser (gen-cpp-lexer)))))
    (lambda (key fmt . args)
      (apply throw 'cpp-error fmt args))))

;; @deffn {Procedure} eval-cpp-expr tree [options] => datum
;; Evaluate a tree produced from @code{parse-cpp-expr}.
;; Options include optional dictionary for defines and values
;; and @code{#:inc-dirs} for @code{has_include} etc
;; @end deffn
(define* (eval-cpp-expr tree #:optional (defs '()) #:key (inc-dirs '()))
  (letrec
      ((tx (lambda (tr ix) (sx-ref tr ix)))
       (tx1 (lambda (tr) (sx-ref tr 1)))
       (ev (lambda (ex ix) (eval-expr (sx-ref ex ix))))
       (ev1 (lambda (ex) (ev ex 1)))	; eval expr in arg 1
       (ev2 (lambda (ex) (ev ex 2)))	; eval expr in arg 2b
       (ev3 (lambda (ex) (ev ex 3)))	; eval expr in arg 3
       (eval-expr
	(lambda (tree)
	  (case (car tree)
	    ((fixed) (string->number (cnumstr->scm (tx1 tree))))
	    ((char) (char->integer (string-ref (tx1 tree) 0)))
	    ((defined) (if (assoc-ref defs (tx1 tree)) 1 0))
	    ((has-include)
	     (if (find-incl-in-dirl (unesc-c-str (tx1 tree)) inc-dirs #f) 1 0))
	    ((has-include-next)
	     (if (find-incl-in-dirl (unesc-c-str (tx1 tree)) inc-dirs #t) 1 0))
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
	    ((eq) (if (= (ev1 tree) (ev2 tree)) 1 0))
	    ((ne) (if (= (ev1 tree) (ev2 tree)) 0 1))
	    ((bitwise-not) (lognot (ev1 tree)))
	    ((bitwise-or) (logior (ev1 tree) (ev2 tree)))
	    ((bitwise-xor) (logxor (ev1 tree) (ev2 tree)))
	    ((bitwise-and) (logand (ev1 tree) (ev2 tree)))
	    ((or) (if (and (zero? (ev1 tree)) (zero? (ev2 tree))) 0 1))
	    ((and) (if (or (zero? (ev1 tree)) (zero? (ev2 tree))) 0 1))
	    ((cond-expr) (if (zero? (ev1 tree)) (ev3 tree) (ev2 tree)))
	    ;; If ident is not defined it should be zero:
	    ((ident) (if (assoc-ref defs (tx1 tree))
                         (throw 'cpp-error "ident not expanded ???")
                         0))
	    ((p-expr) (ev1 tree))
	    ((cast) (ev2 tree))
	    (else (throw 'cpp-error "eval-cpp-expr: incomplete impl"))))))
    (eval-expr tree)))


;; === C preprocessor macro expansion =========================================
;; Macro expansion is hairy, with lots of corner cases.  

(define (mknox tokl)
  (map (lambda (tok)
         (if (eq? (car tok) '$ident) `($idnox . ,(cdr tok)) tok))
       tokl))

;; @deffn {Procedure} macro-expand tokl defs [used [seed [keep-comm]]]
;; Process the token list @var{tokl}, using alist of macro definitions
;; @var{defs}.  The list of strings @var{used} defined macros already used.
;; Return the result as a list of reversed tokens, prepened to @var{seed}
;; (default @code{()}.  The unused boolean option @var{keep-comm} is intended
;; to be option to keep comments from macros, but is not yet implemented.
;; Return the processed token sequence in @emph{reverse order}.
;; @end deffn
(define* (cpp-expand tokl defs #:optional (used '()) (seed '()) keep-comm)
  (let loop ((osq seed) (used used) (isq tokl))
    (match isq
      ('() osq)
      (`(($ident . ,ident) . ,rest)
       (cond
        ((member ident used)
         (loop (cons `($idnox . ,ident) osq) used (cdr isq)))
        ((lookup-def defs ident) =>
         (lambda (rhs)
           (cond
            ((null? rhs)
             (loop osq used rest))
            ((string? (caar rhs))
             (call-with-values
                 (lambda ()
                   (if (pair? rest)
                       (collect-args (car rhs) rest)
                       (values (tokenize-args (car rhs)) '())))
               (lambda (argd rest)
                 (if argd
                     (let* ((tkl (cpp-subst (cdr rhs) argd defs used))
                            (uzed (cons ident used))
                            (csq (cpp-expand tkl defs uzed '() keep-comm))
                            (rest (append-reverse csq rest)))
                       (loop osq used rest))
                     (loop (cons (car isq) osq) used (cdr isq))))))
            (else
             (let* ((tkl (cpp-subst rhs '() defs used))
                    (uzed (cons ident used))
                    (csq (cpp-expand tkl defs uzed '() keep-comm))
                    (rest (append-reverse csq rest)))
               (loop osq used rest))))))
        (else (loop (cons (car isq) osq) used (cdr isq)))))
      (_ (loop (cons (car isq) osq) used (cdr isq))))))

;; @deffn {Procedure} cpp-subst argd repl defs used => tokl
;; Given an alist of function argument names and values @var{argd}, and
;; tokenized macro replacement text @var{repl}, perform the preexpansion.
;; For @code{#} or @code{##} forms, the stringification or pasting is
;; performed.  Formal parameters in the replacement are replaced with the
;; expanded token sequence from @var{argd}, and identifiers are marked for
;; no further expansion.
;; @end defun
(define (cpp-subst tokl argd defs used)
  (let loop ((osq '()) (isq tokl))
    (match isq
      ('() (reverse osq))
      (`(($hash . ,_1) ($ident . ,ident) . ,_2)
       (let ((arg (assoc-ref argd ident)))
         (unless arg (throw 'cpp-error "not found: ~s" ident))
         (loop (acons '$string (tokl->string arg) osq) (cddr isq))))

      (`(($dhash . ,_1) (#\space . ,_2) . ,rest)
       (loop osq (cons (car isq) (cddr isq))))

      (`(($dhash . ,_1) ($ident . ,name) . ,rest)
       (let* ((isp (eq? (caar osq) #\space))
              (rpl (or (assoc-ref argd name) (list (cadr isq))))
              (txt (string-append (if isp (cdadr osq) (cdar osq)) (cdar rpl)))
              (tkl (tokenize-cpp-string txt))
              (osq1 (if isp (cddr osq) (cdr osq)))
              (osq (append-reverse (cdr rpl) (append-reverse tkl osq1))))
         (loop osq rest)))

      (`(($dhash . ,_1) ,rs . ,rest)
       (let* ((isp (eq? (caar osq) #\space))
              (txt (string-append (if isp (cdadr osq) (cdar osq)) (cdr rs)))
              (tkl (tokenize-cpp-string txt))
              (osq (append tkl (if isp (cddr osq) (cdr osq)))))
         (loop osq rest)))

      (`(($ident . ,ident) . ,rest)
       (loop
        (cond
         ((member ident used) (cons `($idnox . ,ident) osq))
         ((assoc-ref argd ident) =>
          (lambda (rpl) (append (mknox (cpp-expand rpl defs used)) osq)))
         (else (cons (car isq) osq)))
        rest))

      (`(($hash . ,_1) . ,_2) (throw 'cpp-error "bad #"))
      (_ (loop (cons (car isq) osq) (cdr isq))))))


;;.@deffn {Procedure} collect-args argl tokl
;; Given the list of argument strings @var{argl}, search the token list
;; @var{tokl} for macro function arguments and return two values: an alist of
;; name-value pairs (in reverse order of @var{argl}) and a list of remaining,
;; unprocessed, tokens.  If the first non-space token is not @code{(} return
;; @code{#f}.  See also @code{tokenize-args}.
;;.@end deffn
(define (collect-args argl tokl) ;; => argd | #f tail
  (define (finish atkl)
    (reverse (if (and (pair? atkl) (eqv? (caar atkl) #\space)) (cdr atkl) atkl)))

  ;; #\(=%28  #\)=%29  #\,=%2c
  (cond
   ((null? tokl) (values #f tokl))
   ((eq? '%28 (caar tokl))
    ;; ad: arg-dict av: tokens, lv: paren-level, al: argl
    (let loop ((argd '()) (tk (car tokl)) (tkl (cdr tokl)) (argl argl))
      (case (car tk)
        ((#f) (throw 'cpp-error "end of input collecting args"))
        ((%28 %2c)                     ; start of arg (#\( #\,)
         (let* ((anam (and (pair? argl) (car argl)))
                (mark (if (equal? anam "...") '%29 '%2c))
                (anam (if (equal? anam "...") "__VA_ARGS__" anam)))
           ;;(sferr "subloop: tk=~s tkl=~s\n" tk tkl)
           (let lp ((atkl '()) (lv 0) (tkl tkl)) ; collect-to-mark
             (when (null? tkl) (throw 'cpp-error "incomplete arg list"))
             (let* ((tk (car tkl)) (k (car tk)))
               ;;(sferr "  lp: k=~s mark=~s\n" k mark)
               (cond
                ((and (null? atkl) (eq? k #\space))
                 (lp atkl lv (cdr tkl)))
                ((eq? '%28 k)
                 (lp (cons tk atkl) (1+ lv) (cdr tkl)))
                ((positive? lv)
                 (lp (cons tk atkl) (if (eq? k '%29) (1- lv) lv) (cdr tkl)))
                ((or (eq? k mark) (and mark (eq? k '%29)))
                 (loop (acons anam (finish atkl) argd)
                       (car tkl) (cdr tkl) (cdr argl)))
                (else (lp (cons tk atkl) lv (cdr tkl))))))))
        ((%29)                         ; end of args (#\))
         (cond
          ((null? argl) (values argd tkl))
          ((equal? argl '("...")) (values (acons "__VA_ARGS__" '() argd) tkl))
          (else (throw 'cpp-error "function macro arg count mismatch"))))
        (else
         (throw 'cpp-error "collect-args coding error")))))
   ((and (eq? #\space (caar tokl)) (pair? (cdr tokl)) (eq? '%28 (caadr tokl)))
    (collect-args argl (cdr tokl)))
   (else (values #f tokl))))


;; === tokenize & reverse ============

(define cpp-chseq-tab (remove-mt ident-like? (filter-mt string? cpp-mtab)))

(define read-chseq
  (make-chseq-reader (chseq->canon-map cpp-chseq-tab)))

(define (read-cpp-token)
  (let loop ((ch (read-char)) (ws #f))
    (cond
     ((eof-object? ch) (if ws (cons #\space " ") (cons '$end "#<eof>")))
     ((char-set-contains? c:ws ch) (loop (read-char) #t))
     ((eq? ch #\newline) (loop (read-char) #t))
     ((read-c-comm ch #f) (loop (read-char) #t))
     (ws (unread-char ch) (cons #\space " "))
     ((read-c-mclit ch) => identity)
     ((read-c-ident ch) => (lambda (name) (cons '$ident name)))
     ((read-c-num ch) => identity)
     ((read-c-string ch) => identity)
     ((char=? ch #\#)
      (let ((ch (read-char)))
        (cond ((eqv? ch #\#) `($dhash . "##"))
              (else (unread-char ch) `($hash . "#")))))
     ((read-chseq ch) => identity)
     ((eqv? ch #\\)
      (let ((ch (read-char)))
	(cond
         ((eqv? #\newline ch) (loop (read-char) ws)) ; extend
	 (else (unread-char ch) (cons #\\ "\\")))))  ; error
     (else (cons ch (string ch))))))

;; mark must be one of #\, #\) #f
;; Does not eat the mark.  If #\, or #\) remove trailing space

;;.@deffn {Procedure} tokenize-to-mark mark
;; Generate a token list from characters read from the current input port,
;; up to condition indicated by @var{mark}.  If the mark is @code{#f},
;; read to end of input; if the mark is @code{#\)}-token, read balanced
;; right paren;if the mark is @code{#\,}-token, read to the balanced
;; comma-token or right paren. In the above balanced means additional
;; @code{#\(}-token must first be matched with a @code{#\)}-token.
;; @*An identifier appearing after the text @code{defined} will be marked
;; for no-expansion.  A string or @code{<>} based include path appearing
;; after @code{__has_include} will be converted to a string or text, and
;; not be expanded (e.g., if @code{stdio} is defined @code{<stdio.h>} will
;; not be expanded.
;; @*The @code{#\)} is of the form @code{'%29} (hex symbol);
;; @code{#\)} and @code{#\)} tokens are of the same form.
;; 
;;.@end deffn
(define (tokenize-to-mark mark)
  ;; assert (memq mark '(#f %29 %2c))

  (define (ctx id cx)
    (if (string=? id "defined") #\D cx))
  
  (define (finish tkl)
    (reverse tkl))
  ;;(reverse (if (and mark (pair? tkl) (eq? #\space (caar tkl))) (cdr tkl) tkl)))

  ;; cx(context, see ctx above):, #\D=def #\I=inc
  (let loop ((tkl '()) (cx #\nul) (lv 0) (tk (read-cpp-token)))
    ;;(sferr "tk=~s\n" tk)
    (let ((k (car tk)) (v (cdr tk)))
      (cond
       ((and mark (null? tkl) (eq? #\space k)) (loop tkl cx lv (read-cpp-token)))
       ((eq? k '$ident)
        (loop (cons (if (eq? cx #\D) `($idnox . ,(cdr tk)) tk) tkl)
              (ctx v #\nul) lv (read-cpp-token)))
       ((eq? k '%28) (loop (cons tk tkl) cx (1+ lv) (read-cpp-token)))
       ((eq? k '$end) (if mark (throw 'cpp-error "yuck") (finish tkl)))
       ((positive? lv)
        (loop (cons tk tkl) cx (if (eq? '%29 k) (1- lv) lv) (read-cpp-token)))
       ((or (eq? k mark) (and mark (eq? k '%29)))
        (unread-char (case k ((%29) #\)) ((%2c) #\,))) (finish tkl))
       (else (loop (cons tk tkl) cx lv (read-cpp-token)))))))

(define (tokenize-cpp-string str)
  (with-input-from-string str
    (lambda () (tokenize-to-mark #f))))
  

;;.@deffn {Procedure} tokenize-args argl
;; Given the list of argument strings @var{argl}, search the current input
;; character stream for macro function arguments and return an alist of
;; name-value pairs (in reverse order of @var{argl}).  If the first non-space
;; token is not @code{(} return @code{#f}.  See also @code{collect-args}.
;;.@end deffn
(define (tokenize-args argl) ;; => argd | #f
  (and
   (let loop1 ((sp #f) (ch (read-char)))
     (cond
      ((eof-object? ch) (if sp (unread-char #\space)) #f)
      ((char-set-contains? c:ws ch) (loop1 #t (read-char)))
      ((not (char=? #\( ch))
       (if sp (unread-char #\space)) (unread-char ch) #f)))
   ;; found #\(
   (let loop2 ((argd '()) (ch #\() (argl argl))
     (cond
      ((eof-object? ch)
       (throw 'cpp-error "end of input collecting args"))
      ((memq ch '(#\( #\,)) ; start of arg
       (let* ((anam (and (pair? argl) (car argl)))
              (tl (tokenize-to-mark (if (equal? anam "...") '%29 '%2c)))
              (atkl (if (and (pair? tl) (eq? #\space (caar tl))) (cdr tl) tl)))
         (loop2
	  (cond
           ((and (char=? #\( ch) (null? argl) (null? atkl)) argd)
           ((null? argl) (throw 'cpp-error "too many values"))
           ((string=? anam "...") (acons "__VA_ARGS__" atkl argd))
           (else (acons anam atkl argd)))
          (read-char) (if (pair? argl) (cdr argl) argl))))
      ((char=? ch #\))                  ; end of args
       (cond
        ((null? argl) argd)
        ((equal? argl '("...")) (acons "__VA_ARGS__" '() argd))
        (else (throw 'cpp-error "function macro arg count mismatch"))))
      (else
       (throw 'cpp-error "cpp.scm(tokenize-args): coding error"))))))

;; gobject does shit like GMAC('hello world')
(define (collect-one-arg mark)
  (let loop ((lv 0) (chl '()) (ch (read-char)))
    (cond
     ((eof-object? ch) (error "crap"))
     ((and mark (null? chl) (eq? #\space ch)) (loop lv chl (read-char)))
     ((char=? ch #\() (loop (1+ lv) (cons ch chl) (read-char)))
     ((positive? lv)
      (loop (if (char=? ch #\)) (1- lv) lv) (cons ch chl) (read-char)))
     ((or (char=? ch mark) (and mark (char=? ch #\))))
      (unread-char ch) (if (pair? chl) (cons '$arg (rls chl)) #f))
     (else (loop lv (cons ch chl) (read-char))))))

(define (kludge-args argl) 
  (and
   (let loop1 ((sp #f) (ch (read-char)))
     (cond
      ((eof-object? ch) (if sp (unread-char #\space)) #f)
      ((char-set-contains? c:ws ch) (loop1 #t (read-char)))
      ((not (char=? #\( ch))
       (if sp (unread-char #\space)) (unread-char ch) #f)))
   ;; found #\(
   (let loop2 ((argd '()) (ch #\() (argl argl))
     (cond
      ((eof-object? ch)
       (throw 'cpp-error "end of input collecting args"))
      ((memq ch '(#\( #\,)) ; start of arg
       (let* ((anam (and (pair? argl) (car argl)))
              (atk (collect-one-arg (if (equal? anam "...") #\) #\,)))
              (atkl (if atk (list atk) '())))
         (loop2
	  (cond
           ((and (char=? #\( ch) (null? argl) (null? atkl)) argd)
           ((null? argl) (throw 'cpp-error "too many values"))
           ((string=? anam "...") (acons "__VA_ARGS__" atkl argd))
           (else (acons anam atkl argd)))
          (read-char) (if (pair? argl) (cdr argl) argl))))
      ((char=? ch #\))                  ; end of args
       (cond
        ((null? argl) argd)
        ((equal? argl '("...")) (acons "__VA_ARGS__" '() argd))
        (else (throw 'cpp-error "function macro arg count mismatch"))))
      (else
       (throw 'cpp-error "cpp.scm(kludge-args): coding error"))))))


;;.@deffn {Procedure} rtokl->string reverse-token-list => string
;; Convert reverse token-list to string.
;;.@end deffn
(define (rtokl->string tokl)
  (let loop ((stl '()) (tkl tokl))
    (cond
     ((null? tkl) (string-join stl ""))
     ((memq (caar tkl) '($string $text))
      (loop (cons (string-append "\"" (cdar tkl) "\"") stl) (cdr tkl)))
     ((char? (car tkl)) (error "fixme"))
     (else (loop (cons (cdar tkl) stl) (cdr tkl))))))

;;.@deffn {Procedure} tokl->string reverse-token-list => string
;; Convert token-list to string (using @code{rtokl->string}).
;;.@end deffn
(define (tokl->string tokl)
  (rtokl->string (reverse tokl)))

;; The question is whether to tokenize all cpp-defs
;; or keep the strings and convert to tokens on the fly.
(define-public (lookup-def defs ident)
  (let* ((def (assoc ident defs)) (ref (and def (cdr def))))
    (when #f ;; (string=? ident "__USE_DYNAMIC_STACK_SIZE")
      (sferr "ident=~s\n" ident)
      (sferr "    =>~s\n" def)
      (quit))
    (cond
     ((not ref))                        ; catch ("FOO" . #f)
     ((null? ref))
     ((string? ref) (set-cdr! def (tokenize-cpp-string ref)))
     ((string? (cdr ref)) (set-cdr! ref (tokenize-cpp-string (cdr ref)))))
    (and ref (cdr def))))
;; vs
;;(define-public (lookup-def defs ident)
;;  (assoc-ref defs ident))  


;; === exports =======================

;; @deffn {Procedure} macro-expand-text text defs => text
;; Like @code{macro-expand} but processes text entirely and generates
;; text result.
;;.@end deffn
(define (macro-expand-text text defs)
  (with-input-from-string text
    (lambda ()
      (let* ((tokl (tokenize-to-mark #f))
             (rtkl (cpp-expand tokl defs))
             (repl (rtokl->string rtkl)))
        repl))))

;; This of bit of a kludge to deal with _Pragma in odd places.  The parser
;; can't reject the full deal so we parse here and return as ($pragma . str)
;; which the parser can ignore.  (Should the make-lalr-parser have a hook
;; for language pramgas?
(define (finish-pragma)
  (define (sk-ws ch)
    (if (char-whitespace? ch) (sk-ws (read-char)) ch))
  (unless (char=? #\( (sk-ws (read-char))) (throw 'c99-error "_Pragma: 1"))
  (let ((str (read-c-string (sk-ws (read-char)))))
    (unless str (throw 'c99-error "_Pragma: 2"))
    (unless (char=? #\) (sk-ws (read-char))) (throw 'c99-error "_Pragma: 3"))
    (cons '$pragma (cdr str))))
(export finish-pragma)


;; @deffn {Procedure} expand-cpp-macro-ref ident defs sp => repl | #f
;; Given an identifier seen in the current input, this checks for associated
;; definition in @var{defs} (generated from CPP defines).  If found as simple
;; macro, the expansion is returned as a string.  If @var{ident} refers
;; to a macro with arguments, then the arguments will be read from the
;; current input.  The format of the @code{defs} entries are
;; @example
;; ("ABC" . "123")
;; ("MAX" ("X" "Y") . "((X)>(Y)?(X):(Y))")
;; @end example
;; @* TODO: think about replacing rhs w/ tokenized form
;; @noindent
;; Note that this routine will look in the current-input so if you want to
;; expand text, you must use (with-input-from-string "" ...)
;; The argument @var{sp} is source properties (aka location info).
;; @end deffn
(define* (expand-cpp-macro-ref ident defs #:optional (sp '()))

  (define (cleanup seq)
    (let loop ((out '()) (in seq))
      (cond
       ((null? in) out)
       ((eq? #\space (caar in)) (loop out (cdr in)))
       ((eq? '$idnox (caar in)) (loop (acons '$ident (cdar in) out) (cdr in)))
       (else (loop (cons (car in) out) (cdr in))))))

  (identity ;; false-if-exception
   (cond
    ((lookup-def defs ident)
     (cleanup (cpp-expand `(($ident . ,ident)) defs '() '())))
    ((c99-std-val ident sp)
     => (lambda (s) (list (cons '$string s))))
    (else #f))))


;; @deffn {Procedure} eval-cpp-cond-text text [defs] => 0 | 1
;; Evaluate CPP condition expression (text).
;; Undefined identifiers are replaced with @code{0}.
;; @end deffn
(define* (eval-cpp-cond-text text #:optional (defs '()) #:key (inc-dirs '()))
  (with-throw-handler
      'cpp-error
    (lambda ()
      (let* ((repl (macro-expand-text text defs))
	     (exp (parse-cpp-expr repl)))
        (eval-cpp-expr exp defs #:inc-dirs inc-dirs)))
    (lambda (key fmt . args)
      (report-error fmt args)
      (throw 'c99-error "CPP error"))))


;; =============================================================================
;; deprecated code

;; @deffn {Procedure} expand-cpp-name name defs [sp]=> repl | #f
;; Calls @code{expand-cpp-macro-ref} with null input string (w/o further
;; input).  If @var{name} is has a function definition @code{#f} is returned.
;; @var{sp} is optional source properties
;; @end deffn
(define (expand-cpp-name name defs sp)
  (with-input-from-string ""
    (lambda () (expand-cpp-macro-ref name defs))))

;; @deffn {Procedure} skip-cpp-macro-ref name defs
;; Like @code{expand-cpp-macro-ref} but skip over the reference.
;; @* may not catch strings w/ non-matching parens
;; @end deffn
(define (skip-cpp-macro-ref name defs)
  "- Procedure: skip-cpp-macro-ref name defs
     Like ‘expand-cpp-macro-ref’ but skip over the reference.
     may not catch strings w/ non-matching parens"
  (let ((rval (lookup-def defs name)))
    (and
     (pair? rval)
     (let loop ((ch (read-char)))
       (cond ((eof-object? ch) (throw 'c99-error "eof when expecting `('"))
	     ((char-whitespace? ch) (loop (read-char)))
	     ((char=? ch #\() #t)
	     (else (unread-char ch) #f)))
     (let loop ((lv 0) (ch (read-char)))
       (cond ((eof-object? ch) (throw 'c99-error "expecting `)'"))
	     ((char=? #\( ch) (loop (1+ lv) (read-char)))
	     ((char=? #\) ch) (if (zero? lv) #f (loop (1- lv) (read-char))))
	     (else (loop lv (read-char))))))))


;; --- last line ---
