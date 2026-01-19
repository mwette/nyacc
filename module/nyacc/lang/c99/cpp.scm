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

;; C preprocessor macro expansion and condition text parse-and-eval
;; ref: https://gcc.gnu.org/onlinedocs/gcc-3.0.1/cpp_3.html

;;; Code:

(define-module (nyacc lang c99 cpp)
  #:export (
	    cpp-line->stmt
	    find-incl-in-dirl
	    eval-cpp-cond-text
	    expand-cpp-macro-ref
	    skip-cpp-macro-ref
	    ;;
	    expand-cpp-name
	    parse-cpp-expr
	    eval-cpp-expr
            scan-arg-literal
	    )
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
(define* (pperr exp #:optional (plp ""))
  (pretty-print exp (current-error-port) #:per-line-prefix plp))

(define rls reverse-list->string)

(define mkid (let ((id 0)) (lambda () (set! id (1+ id)) id)))
(define DBG #t)

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
   ((string=? str "__DATE__") "M01 01 2001")
   ((string=? str "__FILE__") (or (assq-ref sl 'filename) "(unknown)"))
   ((string=? str "__LINE__") (number->string (or (assq-ref sl 'line) 0)))
   ((string=? str "__TIME__") "00:00:00")
   ((string=? str "__STDC__") "1")
   ((string=? str "__STDC_HOSTED__") "0")
   ((string=? str "__STDC_VERSION__") "199901L")
   (else #f)))

(define inline-whitespace (list->char-set '(#\space #\tab)))

;;.@deffn {Procedure} skip-il-ws ch
;; Skip in-line whitespace
;; @end deffn
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

;; @deffn {Procedure} read-ellipsis ch
;; read ellipsis
;; @end deffn
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
;; Reads CPP define from current input and generates a cooresponding sxml
;; expression.
;; @example
;;   (define (name "ABC") (repl (11 . "123"))
;; OR
;;   (define (name "ABC") (args "X" "Y")
;;           (repl ($ident "X") (22 . "+") ($ident . "Y")))
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
    (tokenize-string-to-mark (read-rest la) #f))

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
  (define (rd-ident) (read-c-ident (skip-il-ws (read-char))))
  (define (rd-num) (and=> (read-c-num (skip-il-ws (read-char))) cdr))
  (define (rd-rest) (read-rest (skip-il-ws (read-char))))
  (with-input-from-string line
    (lambda ()
      (let ((ch (skip-il-ws (read-char))))
	(cond
	 ((read-c-ident ch) =>
	  (lambda (cmds)
	    (let ((cmd (string->symbol cmds)))
	      (case cmd
		((include) `(include ,(cpp-include)))
		((include_next) `(include-next ,(cpp-include)))
		((define) (cpp-define))
		((undef) `(undef ,(rd-ident)))
		((ifdef)
		 `(if ,(string-append "defined(" (rd-ident) ")" (rd-rest))))
		((ifndef)
		 `(if ,(string-append "!defined(" (rd-ident) ")" (rd-rest))))
		((if elif else endif line error warning pragma)
		 (list cmd (rd-rest)))
		(else
		 (list 'warning (simple-format #f "unknown CPP: ~S" line)))))))
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
(define* (eval-cpp-expr tree #:optional (dict '()) #:key (inc-dirs '()))
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
	    ((defined) (if (assoc-ref dict (tx1 tree)) 1 0))
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
	    ;; in CPP if ident is not defined it should be zero
	    ((ident) (or (and=> (assoc-ref dict (tx1 tree)) string->number) 0))
	    ((p-expr) (ev1 tree))
	    ((cast) (ev2 tree))
	    (else (error "nyacc eval-cpp-expr: incomplete implementation"))))))
    (eval-expr tree)))


;; === C preprocessor macro expansion =========================================
;; Macro expansion is hairy, with lots of corner cases.  


(define (expand-plain-macro ident repl defs used seed)
  (let* (;; need to call cpp-subst here?
         (rtkl (cpp-expand repl defs (cons ident used) seed)))
    (maybe-expand-again rtkl defs used)))

(define (expand-fnctn-macro ident argl repl defs used seed)
  (sferr "expand-fnctn-macro: last-def:\n  ~s\n" (car defs))
  (and=> (tokenize-args argl)
    (lambda (argd)
      (sferr "  argd: ~s\n" argd)
      (let* ((pxtl (cpp-subst repl argd defs used))
             ;;(x (begin (sferr "argd=~s pxtl=~s\n" argd pxtl) (quit)))
             (rtkl (cpp-expand pxtl defs (cons ident used) seed)))
        (maybe-expand-again rtkl defs used)))))

(define (maybe-expand-again rtkl defs used)
  "expand again if new ident introduces a new function macro call"
  (match rtkl
    ((#\space ('$ident . ident) . rest)
     ;;(maybe-expand-again (cdr rtkl) defs used rest))
     (maybe-expand-again (cdr rtkl) defs used))
    ((('$ident . ident) . rest)
     (cond
      ((assoc-ref defs ident) =>
       (lambda (rhs)
         (if (string? rhs)
             rtkl
             (expand-fnctn-macro ident (car rhs) (cdr rhs) defs used rest))))
      (else rtkl)))
    (_ rtkl)))

;;.@deffn {Procedure} macro-expand tokl defs [used seed keep-comm] => rtkl| #f
;; Process the token list @var{tokl}, using alist of macro definitions
;; @var{defs}.  The list of strings @var{used} defined macros already used.
;; Return the result as a list of reversed tokens, prepened to @var{seed}
;; (default @code{()}.  The unused boolean option @var{keep-comm} is intended
;; to be option to keep comments from macros.
;;.@end deffn


;;.@deffn {Procedure} pre-expand argd repl defs used
;; Given alist of function argument names and values @var{argd}, and tokenized
;; macro replacement text, perform the preexpansion.  For arguments that are
;; subject to @code{#} or @code{##} perform the associated operation
;; (stringificaton or paste).  Otherwise, expand the argument value and mark
;; expanded identifiers (i.e., replace key @code{$ident} with @code{$idnox}).
;;.@end defun
;; TODO: retokenize after ## application (from start-or-space to end-or-space)
;; e.g. hex numbers 0 ## xf is not an identifier

;; currently outputs rtkl
(define (cpp-subst inseq argd defs used)
  (let loop ((osq '()) (uzd used) (isq inseq))
    ;;(sferr "cpp-subst loop:\n  isq: ~s\n  osq: ~s\n" isq osq)
    (match isq
     ('() (reverse osq))
     (`(($hash . ,_1) ($ident . ,name) . ,_2)
      (let ((arg (assoc-ref argd name)))
        (unless arg (throw 'cpp-error "not found: ~s" name))
        (loop (acons '$string (tokl->string arg) osq) uzd (cddr isq))))

     ;; ALL the ## cases HERE =>
     (`(($dhash . ,_1) ($ident . ,name) . ,rest)
      (let* ((arg (assoc-ref argd name))
             (xrest (append (if arg arg (list (cadr isq))) rest))
             (txt (string-append (cdar osq) (cdar xrest)))
             (tok (with-input-from-string txt read-cpp-token)))
        (loop (cons tok (cdr osq)) uzd (cdr xrest))))
     
     (`(($dhash . ,_1) ,rs . ,resti)
      (let* ((txt (string-append (cdar osq) (cdr rs)))
             (tok (with-input-from-string txt read-cpp-token)))
        (loop (cons tok (cdr osq)) uzd resti)))
     
     (`(($ident . ,name) . ,rest)
      (let* ((arg (assoc-ref argd name))
             (osq (if arg
                      (cpp-expand arg defs (cons name used) osq)
                      (cons (car isq) osq))))
        (loop osq uzd rest)))
     ;; error cases
     (`(($hash . ,_1) . ,_2) (throw 'cpp-error "bad #"))
     (__ (loop (cons (car isq) osq) uzd (cdr isq))))))

;; @deff cpp-expand tokl defs [used [seed [keep-comm]]] => tokl
(define* (cpp-expand tokl defs #:optional (used '()) (seed '()) keep-comm)
  (define DBG #t)
  (when DBG (sferr "cpp-expand:\n"))
  (let loop ((osq seed) (uzd used) (isq tokl))
    (when DBG (sferr "  loop:\n    isq: ~s\n    osq:  ~s\n" isq osq))
    (match isq
      ('() (reverse osq))
      (`(($ident . ,ident) . ,rest)
       (when DBG (sferr "    ident=~s\n" ident))
       (cond
        ((member ident uzd)
         (loop (cons (car isq) osq) uzd (cdr isq)))
        ((assoc-ref defs ident) =>
         (lambda (rhs)
           (when DBG (sferr "    rhs: ~s\n" rhs))
           (cond
            ((null? rhs) (loop osq uzd rest))
            ((pair? (car rhs))
             ;; plain macro call
             (when DBG (sferr "    plain: used:~s\n" uzd))
             (let* ((uzd (cons ident uzd))
                    (tkl (cpp-subst (cdr rhs) '() defs uzd))
                    (osq (cpp-expand tkl defs uzd osq keep-comm)))
               (loop osq uzd rest)))
            ((collect-args (car rhs) rest) (lambda (a b) a) =>
             (lambda (argd rest)
               (when DBG (sferr "    fnctn:\n"))
               (if argd
                   ;; fnctn call macro
                   (let* ((uzd (cons ident uzd))
                          (tkl (cpp-subst isq argd defs uzd))
                          (osq (cpp-expand tkl defs uzd osq keep-comm)))
                     (loop osq uzd rest))
                   ;; no macro call
                   (loop (cons (car isq) osq) uzd (cdr isq))))))))
        (else (loop (cons (car isq) osq) uzd (cdr isq)))))
      (__ (loop (cons (car isq) osq) uzd (cdr isq))))))
             

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

  (match tokl
    (`((#\( . ,_) . ,rest)
     ;; ad: arg-dict av: tokens, lv: paren-level, al: argl
     (let loop ((argd '()) (tk (car tokl)) (tkl rest) (argl argl))
       (match (car tk)
         (#f (throw 'cpp-error "end of input collecting args"))
         ((or #\( #\,)                  ; start of arg
          (let* ((anam (and (pair? argl) (car argl)))
                 (mark (if (equal? anam "...") #\) #\,))
                 (anam (if (equal? anam "...") "__VA_ARGS__" anam)))
            (let lp ((atkl '()) (lv 0) (tkl tkl)) ; collect-to-mark
              (let* ((tk (and (pair? tkl) (car tkl))) (k (car tk)))
                (cond
                 ((null? tkl)
                  (throw 'cpp-error "yuck"))
                 ((and (null? atkl) (eq? k #\space))
                  (lp atkl lv (cdr tkl)))
                 ((eq? #\( k)
                  (lp (cons tk atkl) (1+ lv) (cdr tkl)))
                 ((positive? lv)
                  (lp (cons tk atkl) (if (eq? k #\)) (1- lv) lv) (cdr tkl)))
                 ((or (eq? mark k) (and mark (eq? #\) k)))
                  (loop (acons anam (finish atkl) argd)
                        (car tkl) (cdr tkl) (cdr argl)))
                 (else (lp (cons tk atkl) lv (cdr tkl))))))))
         (#\)                           ; end of args
          (cond
           ((null? argl) (values argd tkl))
           ((equal? argl '("...")) (values (acons "__VA_ARGS__" '() argd) tkl))
           (else (throw 'cpp-error "function macro arg count mismatch"))))
         (__
          (throw 'cpp-error "collect-args coding error")))))
    (`((#\space . ,_) (#\( . ,_) . ,rest) (collect-args argl (cdr tokl)))
    (_ (values #f tokl))))


;; === tokenize & reverse ============

(include-from-path "nyacc/lang/c99/mach.d/c99-tab.scm")

;; extended char sequence tab
(define chseqtab (remove-mt ident-like? (filter-mt string? c99-mtab)))

;; see expand-cpp-macro-ref/reverse-and-cleanup
(define xsymtab
  (cons* (cons #\( (assoc-ref "(" chseqtab))
         (cons #\, (assoc-ref "," chseqtab))
         (cons #\) (assoc-ref ")" chseqtab))
         (cons #\< (assoc-ref "<" chseqtab))
         ;;(cons '$ident '$ident)
	 (filter-mt symbol? c99-mtab)))

(define read-chseq (make-chseq-reader chseqtab))

(define (read-cpp-token)
  (let loop ((ch (read-char)) (ws #f))
    (cond
     ((eof-object? ch) '($end . "#<eof>"))
     ((char-set-contains? c:ws ch) (loop (read-char) #t))
     ;;((begin (sferr "2: ~s\n" ch) #f))
     ((eq? ch #\newline) (loop (read-char) #t))
     ((read-c-comm ch #f) (loop (read-char) #t))
     (ws (unread-char ch) (cons #\space " "))
     ((read-c-chlit ch) => identity)
     ((read-c-ident ch) => (lambda (name) (cons '$ident name)))
     ((read-c-num ch) => identity)
     ((read-c-string ch) => identity)
     ((char=? ch #\() (cons #\( "("))
     ((char=? ch #\,) (cons #\, ","))
     ((char=? ch #\)) (cons #\) ")"))
     ((char=? ch #\<) (cons #\< "<"))   ; as in <foo.h>
     ((char=? ch #\#)
      (let ((ch (read-char)))
        (cond ((eqv? ch #\#) `($dhash . "#"))
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
;; read to end of input; if the mark is @code{#\)}, read balanced right paren;
;; if the mark is @code{#\,}, read to the balanced comma or right paren.
;; In the above balanced means additional @code{#\(} must first be matched
;; with @code{#\)}.
;; @*An identifier appearing after the text @code{defined} will be marked
;; for no-expansion.  A string or @code{<>} based include path appearing
;; after @code{__has_include__} will be converted to a string or text, and
;; not be expanded (e.g., if @code{stdio} is defined @code{<stdio.h>} will
;; not be expanded.
;;.@end deffn

(define (tokenize-to-mark mark)

  (define (ctx id cx)
    (cond
     ((member id '("defined")) #\D)
     ((member id '("__has_include__" "__has_include_next__")) #\I)
     (else cx)))
  
  (define (finish tkl)
    (reverse (if (and (pair? tkl) (eqv? #\space (caar tkl))) (cdr tkl) tkl)))

  ;; cx(context, see ctx above):, #\D=def #\I=inc
  (let loop ((tkl '()) (cx #\nul) (lv 0) (tk (read-cpp-token)))
    (let ((k (car tk)) (v (cdr tk)))
      ;;(sferr "loop: k=~s v=~s cx=~s\n" k v cx)
      (cond
       ((and (null? tkl) (eq? #\space k)) (loop tkl cx lv (read-cpp-token)))
       ((eq? k '$ident)
        (loop (cons (if (eq? cx #\D) `($idnox . ,(cdr tk)) tk) tkl)
              (ctx v #\nul) lv (read-cpp-token)))
       ((and (char=? #\I cx) (eq? #\< k))
        (let lp ((chl (list k)) (ch (read-char)))
          (if (char=? #\> ch)
              (loop (acons '$text (rls (cons ch chl)) tkl)
                    #\nul lv (read-cpp-token))
              (lp (cons ch chl) (read-char)))))
       ((eq? k #\() (loop (cons tk tkl) cx (1+ lv) (read-cpp-token)))
       ((eq? k '$end) (if mark (throw 'cpp-error "yuck") (finish tkl)))
       ((positive? lv)
        (loop (cons tk tkl) cx (if (eq? #\ k) (1- lv) lv) (read-cpp-token)))
       ((or (eqv? mark k) (and mark (eq? #\) k))) (unread-char k) (finish tkl))
       (else (loop (cons tk tkl) cx lv (read-cpp-token)))))))
  
(define (tokenize-string-to-mark str mark)
  (with-input-from-string str (lambda () (tokenize-to-mark mark))))


;;.@deffn {Procedure} tokenize-args argl
;; Given the list of argument strings @var{argl}, search the current input
;; character stream for macro function arguments and return an alist of
;; name-value pairs (in reverse order of @var{argl}).  If the first non-space
;; token is not @code{(} return @code{#f}.  See also @code{collect-args}.
;;.@end deffn
(define (tokenize-args argl) ;; => argd | #f
  (sferr "argl: ~s\n" argl)
  (and
   (let loop1 ((sp #f) (ch (read-char)))
     ;;(sferr "  ch: ~s\n" ch)
     (cond
      ((eof-object? ch) (if sp (unread-char #\space)) #f)
      ((char-set-contains? c:ws ch) (loop1 #t (read-char)))
      ((not (char=? #\( ch)) (if sp (unread-char #\space)) #f)))
   ;; found #\(
   (let loop2 ((argd '()) (ch #\() (argl argl))
     ;;(sferr "loop2: ch=~s argl=~s argd=~s\n" ch argl argd)
     (cond
      ((eof-object? ch)
       (throw 'cpp-error "end of input collecting args"))
      ((memq ch '(#\( #\,)) ; start of arg
       (let* ((anam (and (pair? argl) (car argl)))
              (atkl (tokenize-to-mark (if (equal? anam "...") #\) #\,))))
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


;;.@deffn {Procedure} rtokl->string reverse-token-list => string
;; Convert reverse token-list to string.
;;.@end deffn
(define (rtokl->string tokl)
  (let loop ((stl '()) (tkl tokl))
    (cond
     ((null? tkl) (string-join stl ""))
     ((eq? '$string (caar tkl))
      (loop (cons (string-append "\"" (cdar tkl) "\"") stl) (cdr tkl)))
     ;;((eq? #\( (car tkl)) (loop (cons "(" stl) (cdr tkl)))
     ;;((eq? #\) (car tkl)) (loop (cons ")" stl) (cdr tkl)))
     ((char? (car tkl)) (error "fixme"))
     (else (loop (cons (cdar tkl) stl) (cdr tkl))))))

(define (OLD_rtokl->string tokl)

  ;; Turn reverse chl into a string and insert it into the string list stl.
  (define (add-chl chl stl)
    (if (null? chl) stl (cons (list->string chl) stl)))

  ;; Works like this: Scan through the list of tokens (key-val pairs or
  ;; lone characters).  Lone characters are collected in a list (@code{chl});
  ;; pairs are converted into strings and combined with list of characters
  ;; into a list of strings.  When done the list of strings is combined to
  ;; one string.  (The token 'argval is expansion of argument.)
  ;;(sferr "R->S\n")
  (let loop ((stl '())		   ; list of strings to reverse-append
	     (chl '())		   ; char list
	     (nxt #f)		   ; next string to add after chl
	     (tkl tokl))	   ; input token list
    ;;(sferr " tkl=~s chl=~s stl=~s\n" tkl chl stl)
    (cond
     (nxt
      (loop (cons nxt (add-chl chl stl)) '() #f tkl))
     ((null? tkl)
      (apply string-append (add-chl chl stl)))
     ((char? (car tkl))
      (loop stl (cons (car tkl) chl) nxt (cdr tkl)))
     (else
      (match tkl
	(`(($ident . ,ident) . ,rest)
	 (loop stl chl ident rest))
	(`(($idnox . ,ident) . ,rest)
	 (loop stl chl ident rest))
	#;(`(($cppop . ,ident) . ,rest)
	(loop stl chl ident rest))
	(`(($string . ,val) . ,rest)
	 (loop stl (cons #\" chl) (esc-c-str val) (cons #\" rest)))
	(`(($text . ,val) . ,rest)
	 (loop stl chl val rest))
	(`(#\space #\space . ,rest)
	 (loop stl chl nxt rest))
	(`(#\space . ,rest)
	 (loop stl (cons #\space chl) nxt rest))
	(`(($comm . ,val) . ,rest)
	 (loop stl chl (string-append "/*" val "*/ ") rest))
        (`($hash . ,rest)
         (loop stl (cons #\# chl) nxt rest))
        (`($dhash . ,rest)
         (loop stl (cons* #\# #\# chl) nxt rest))
	(`(,asis . ,rest)
	 (loop stl chl asis rest))
	(otherwise
	 (error "nyacc cpp rtokl->string, no match" tkl)))))))

;;.@deffn {Procedure} tokl->string reverse-token-list => string
;; Convert token-list to string (using @code{rtokl->string}).
;;.@end deffn
(define (tokl->string tokl)
  (rtokl->string (reverse tokl)))

;; === convert to tokens and process

"
cpp-expand tokl defs used => tokl used
  calls
  1.  maybe-expand-ref
  2.  collect-args

expand-macro-ref ident tokl defs used => head tail used
  calls
  1.  collect-args
  2.  tokenize/memoize REPL  via (lkup-repl defs ident) -> tokl
  3.  macro-expand

expand-cpp-macro-ref ident defs sp
  calls
  1. tokenize-args
  2. macro-expand
  3. tokl->string
"

;;.@deffn {Procedure} macro-expand-text text defs
;; Like @code{macro-expand} but processes text entirely and generates
;; text result.  See @code{macro-expand}.
;;.@end deffn
(define (macro-expand-text text defs)
  (let* ((tokl (tokenize-string-to-mark text #f))
         (zz (begin (sferr "defs: ~s\n" defs) (sferr "tokl: ~s\n" tokl)))
         (rtokl (pk (cpp-expand tokl defs)))
         (repl (rtokl->string rtokl)))
    (pperr repl)
    repl))

;; === exports =======================

;; This of bit of a kludge to deal with _Pragma in odd places.  The parser
;; can't reject the full deal so we parse here and return as ($pragma . str)
;; which the parser can ignore.  (Should the make-lalr-parser have a hoook
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
(display "cpp.scm: restore false-if-exception\n")
(define* (expand-cpp-macro-ref ident defs #:optional (sp '()))

  (define (noreverse-and-cleanup seq)
    (sferr "seq: ~s\n" seq)
    ;; preserve '$ident
    (let loop ((out '()) (in seq))
      (cond
       ((null? in) (pk (reverse out)))
       ((eq? #\space (caar in)) (loop out (cdr in)))
       ((eq? '$ident (caar in)) (loop (cons (car in) in) (cdr in)))
       ((or (char? (caar in)) (symbol? (caar in)))
        (set-car! (car in) (assq-ref xsymtab (caar in)))
        (loop (cons (car in) out) (cdr in)))
       (else (loop (cons (car in) out) (cdr in))))))

  ;;(sferr "assoc-ref defs ~s => ~s\n" ident (assoc-ref defs ident))
  
  (identity ;;false-if-exception
   (cond
    ((assoc-ref defs ident) =>
     (lambda (rhs)
       (cond
        ((null? rhs)
         '())
        ((string? (car rhs))
         (noreverse-and-cleanup
          (expand-plain-macro ident rhs defs '() '())))
        (else
         (noreverse-and-cleanup
          (expand-fnctn-macro ident (car rhs) (cdr rhs) defs '() '()))))))
    ((c99-std-val ident sp) => (lambda (s) (list (cons '$string s))))
    (else #f))))

;; @deffn {Procedure} skip-cpp-macro-ref name defs
;; Like @code{expand-cpp-macro-ref} but skip over the reference.
;; @* may not catch strings w/ non-matching parens
;; @end deffn
(define (skip-cpp-macro-ref name defs)
  "- Procedure: skip-cpp-macro-ref name defs
     Like ‘expand-cpp-macro-ref’ but skip over the reference.
     may not catch strings w/ non-matching parens"
  (let ((rval (assoc-ref defs name)))
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

;; @deffn {Procedure} expand-cpp-name name defs [sp]=> repl | #f
;; Calls @code{expand-cpp-macro-ref} with null input string (w/o further
;; input).  If @var{name} is has a function definition @code{#f} is returned.
;; @var{sp} is optional source properties
;; @end deffn
(define (expand-cpp-name name defs sp)
  "- Procedure: expand-cpp-name name defs [sp]=> repl | #f
     Calls ‘expand-cpp-macro-ref’ with null input string (w/o further
     input).  If NAME is has a function definition ‘#f’ is returned.  SP
     is optional source properties"
  (with-input-from-string ""
    (lambda () (expand-cpp-macro-ref name defs))))


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

;; --- last line ---
