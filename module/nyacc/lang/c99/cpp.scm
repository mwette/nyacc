;;; lang/c/cpp.scm - C preprocessor

;; Copyright (C) 2015-2024 Matthew Wette
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
  #:use-module (srfi srfi-71)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc lang sx-util)
  #:use-module ((nyacc lang util) #:select (report-error))
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (ice-9 match))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(use-modules (ice-9 pretty-print))
(define* (pperr exp #:optional (plp ""))
  (pretty-print exp (current-error-port) #:per-line-prefix plp))
(define mkid (let ((id 0)) (lambda () (set! id (1+ id)) id)))

(define DBG #t)

(define c99-std-defs
  '("__DATE__" "__FILE__" "__LINE__" "__STDC__" "__STDC_HOSTED__"
    "__STDC_VERSION__" "__TIME__"))

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
   ((string=? str "__STDC__") "1")
   ((string=? str "__STDC_HOSTED__") "0")
   ((string=? str "__STDC_VERSION__") "201701")
   ((string=? str "__TIME__") "00:00:00")
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
(export ident-like?)
(display "cpp: ident-like? does not work how I assumed\n")

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
    (let loop ((dirl dirl))
      (if (null? dirl) #f
	  (if (and next (string=? (car dirl) cid))
	      (loop (cdr dirl))
	      (let ((p (string-append (car dirl) "/" file-name)))
		(if (access? p R_OK) p (loop (cdr dirl)))))))))

;; @deffn {Procedure} cpp-define
;; Reads CPP define from current input and generates a cooresponding sxml
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

  (define (p-rest la) (read-rest la))

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


;; @deffn {Procedure} cpp-include
;; Parse CPP include statement.
(define (cpp-include)
  (define (loop cl ch end-ch)
    (if (eq? ch end-ch)  (reverse-list->string (cons ch cl))
	(loop (cons ch cl) (read-char) end-ch)))
  (let ((ch (skip-il-ws (read-char))))
    (cond
     ((char=? ch #\<) (loop (list #\<) (read-char) #\>))
     ((char=? ch #\") (loop (list #\") (read-char) #\"))
     ((read-c-ident ch))
     (else (throw 'cpp-error "bad include")))))

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

(define (X-update-next-used uzd nuzd used)
  ;; For scanning multiple macros in a scan, update union of used macros from
  ;; all occurances, where @{used} is entering set, @{uzd} is from last macro
  ;; expanded, and @var{nuzd} is union of new macros expanded to this point.
  ;; Note: both uzd and nuzd have used as a tail.
  (let loop ((nuzd nuzd) (uzd uzd))
    (cond
     ((eq? uzd used) nuzd)
     ((member (car uzd) nuzd) (loop nuzd (cdr uzd)))
     (else (loop (cons (car uzd) nuzd) (cdr uzd))))))

(define* (X-scan+expand-to-mark defs used end-tok #:optional (keep-comm #t))
  ;; Scan and expand to mark (i.e., @code{#\)}, @code{#\,}, @code{#f}), where
  ;; @table @code
  ;; @item #\)
  ;; end of top-level paren's
  ;; @item #\,
  ;; next top-level comma, or end of top-level paren's
  ;; @item #f
  ;; end of input (i.e., string)
  ;; Works like this: Tokenize input (comments, parens, strings, char's, etc).
  ;; Collect tokens in a (reverse ordered) list (tkl).  When complete merged
  ;; tokens for from a result string.
  ;; During this process, we keep track of expanded identifiers and rerun if
  ;; something got expanded.
  ;; @* Also, keep track of @code{##} and spaces so that we can parse with
  ;; interspersed comments (e.g., ABC/*foo*/(123,456): it happens).
  
  (define id (mkid))
  
  (define (uu uzd nuzd) (X-update-next-used uzd nuzd used))
  
  (define (trim-spaces tkl)
    (if (and (pair? tkl) (eqv? '$space (car tkl)))
	(trim-spaces (cdr tkl))
	tkl))

  (define (maybe-finish tkl nuzd)
    (let* ((tkl (if end-tok (trim-spaces tkl) tkl))
	   (repl (rtokl->string tkl)))
      (when DBG
        (sferr "    ~s: scan-x.maybe-finish: ~a repl=~s nuzd=~s\n"
               id (if (eq? nuzd used) "DONE" "REDO") repl nuzd))
      (if (eq? nuzd used)
          (values repl used)
          (with-input-from-string repl
            (lambda () (scan+expand-to-mark defs nuzd #f keep-comm))))))

  (if DBG (sferr "\n~s: scan+x end-tok=~s used=~s\n" id end-tok used))
  (let loop ((tkl '())                      ; token list
             (nuzd used)                    ; next used
	     (lv 0)                         ; paren level
	     (ch (skip-il-ws (read-char)))) ; next char
    ;;(sferr "  ~s: scan+x.loop: nuzd=~s used=~s\n" id nuzd used)
    (cond
     ((eof-object? ch) (maybe-finish tkl nuzd))
     ((and (eqv? end-tok ch) (zero? lv))
      (unread-char ch) (maybe-finish tkl nuzd))
     ((and end-tok (char=? #\) ch) (zero? lv))
      (unread-char ch) (maybe-finish tkl nuzd))
     ((char-set-contains? c:ws ch)	; whitespace
      (loop (cons '$space tkl) nuzd lv (skip-il-ws (read-char))))
     ((read-c-comm ch #f) =>		; comment
      (lambda (comm)
	;; Normally comments in CPP def's are replaced by a space.  We allow
	;; comments to get passed through, hoping this does not break code.
	(if keep-comm
	    (loop (acons '$comm (cdr comm) tkl) nuzd lv (skip-il-ws (read-char)))
	    (loop (cons '$space tkl) nuzd lv (skip-il-ws (read-char))))))
     ((read-c-ident ch) =>
      (lambda (iden)
        (sferr "    ~s: scan-x: got iden ~s\n" id iden)
	(cond
	 ((string=? iden "defined")
	  (loop (acons '$text (string-append iden (scan-defined-arg)) tkl)
                nuzd lv (read-char)))
	 ((member iden '("__has_include__" "__has_include_next__"))
	  (cond
	   ((scan-arg-literal) =>
	    (lambda (arg)
              (let ((tkl (acons '$text (string-append iden arg) tkl)))
	        (loop tkl nuzd lv (read-char)))))
	   (else
	    (loop (acons '$ident iden tkl) nuzd lv (read-char)))))
	 (else
          ;;(sferr "  ~s: about to macro-expand ~s w/ used=~s\n" id iden used)
 	  (let* ((rval uzd (expand-macro-ref iden defs '() used))
                 (tkl (if rval (cons rval tkl) (acons '$ident iden tkl))))
            ;;(sferr "  ~s: returns w/ uzd=~s used=~s\n" id uzd used)
            (loop tkl (uu uzd nuzd) lv (read-char)))))))
     ((read-c-string ch) =>
      (lambda (pair) (loop (cons pair tkl) nuzd lv (read-char))))
     ((char=? #\( ch) (loop (cons ch tkl) nuzd (1+ lv) (read-char)))
     ((char=? #\) ch) (loop (cons ch tkl) nuzd (1- lv) (read-char)))
     (else
      (loop (cons ch tkl) nuzd lv (read-char))))))

(define* (macro-expand tokl defs used #:optional keep-comm) ;; => (tokl used)
  (values `(macro-expand ,tokl) used))

#;(define* (macro-expand tokl defs used #:optional (keep-comm #t))
  
  (define id (mkid))
  
  (define (uu uzd nuzd) (update-next-used uzd nuzd used))
  
  (define (trim-spaces tkl)
    (if (and (pair? tkl) (eqv? '$space (car tkl)))
	(trim-spaces (cdr tkl))
	tkl))

  (define (maybe-finish tkl nuzd)
    (let* (;;(tkl (if end-tok (trim-spaces tkl) tkl))
	   ;;(repl (rtokl->string tkl))
           )
      (if (eq? nuzd used)
          (values repl used)
          (with-input-from-string repl
            (lambda () (scan+expand-to-mark defs nuzd #f keep-comm))))))

  (if DBG (sferr "\n~s: scan+x end-tok=~s used=~s\n" id end-tok used))
  (let loop ((ntkl '())                 ; next token list
             (nuzd used)                ; next used
	     (lv 0)                     ; paren level
	     (ptkl tokl))               ; prev token list
    (cond
     ((eof-object? ch) (maybe-finish tkl nuzd))
     ((and (eqv? end-tok ch) (zero? lv))
      (unread-char ch) (maybe-finish tkl nuzd))
     ((and end-tok (char=? #\) ch) (zero? lv))
      (unread-char ch) (maybe-finish tkl nuzd))
     ((char-set-contains? c:ws ch)	; whitespace
      (loop (cons '$space tkl) nuzd lv (skip-il-ws (read-char))))
     ((read-c-comm ch #f) =>		; comment
      (lambda (comm)
	;; Normally comments in CPP def's are replaced by a space.  We allow
	;; comments to get passed through, hoping this does not break code.
	(if keep-comm
	    (loop (acons '$comm (cdr comm) tkl) nuzd lv (skip-il-ws (read-char)))
	    (loop (cons '$space tkl) nuzd lv (skip-il-ws (read-char))))))
     ((read-c-ident ch) =>
      (lambda (iden)
        (sferr "    ~s: scan-x: got iden ~s\n" id iden)
	(cond
	 ((string=? iden "defined")
	  (loop (acons '$text (string-append iden (scan-defined-arg)) tkl)
                nuzd lv (read-char)))
	 ((member iden '("__has_include__" "__has_include_next__"))
	  (cond
	   ((scan-arg-literal) =>
	    (lambda (arg)
              (let ((tkl (acons '$text (string-append iden arg) tkl)))
	        (loop tkl nuzd lv (read-char)))))
	   (else
	    (loop (acons '$ident iden tkl) nuzd lv (read-char)))))
	 (else
          ;;(sferr "  ~s: about to macro-expand ~s w/ used=~s\n" id iden used)
 	  (let* ((rval uzd (expand-macro-ref iden defs '() used))
                 (tkl (if rval (cons rval tkl) (acons '$ident iden tkl))))
            ;;(sferr "  ~s: returns w/ uzd=~s used=~s\n" id uzd used)
            (loop tkl (uu uzd nuzd) lv (read-char)))))))
     ((read-c-string ch) =>
      (lambda (pair) (loop (cons pair tkl) nuzd lv (read-char))))
     ((char=? #\( ch) (loop (cons ch tkl) nuzd (1+ lv) (read-char)))
     ((char=? #\) ch) (loop (cons ch tkl) nuzd (1- lv) (read-char)))
     (else
      (loop (cons ch tkl) nuzd lv (read-char))))))

(define* (scan+expand-to-mark defs used mark #:optional (keep-comm #t))
  (macro-expand (tokenize-to-mark mark) defs used keep-comm))

;; @deffn {Procedure} cpp-expand-text text defs [used] => string used
;; Expand the string @var{text} using the provided CPP @var{defs} a-list.
;; Identifiers in the list of strings @var{used} will not be expanded.
;; @end deffn
(define* (cpp-expand-text text defs #:optional (used '()))
  ;;(define id (mkid))
  ;;(sferr "~s: cpp-expand-text text=~s used=~s\n" id text used)
  (with-input-from-string text
    (lambda () (scan+expand-to-mark defs used #f))))

;; @deffn {Procedure} collect-args argl defs used => (argd used)
;; Collect arguments to a function macro which appears in C code.  Return an
;; alist of arg-name, arg-tokl elements.  If not looking at @code{(} return
;; @code{#f}, else scan and eat up to closing @code{)}.  If multiple
;; whitespace characters are skipped at the front then only one @code{#\space}
;; is re-inserted.
;; @end deffn
(display "cpp.scm: fix collect-args: does not need to return used\n")
(define (collect-args argl defs used)
  (define id (mkid))
  (if DBG (sferr "~s: collect-args argl=~s used=~s\n" id argl used))
  (and
   (let loop1 ((sp #f) (ch (read-char)))
     (cond
      ((eof-object? ch) (if sp (unread-char #\space)) #f)
      ((char-set-contains? c:ws ch) (loop1 #t (read-char)))
      ((not (char=? #\( ch)) (if sp (unread-char #\space)) #f)
      (else (unread-char ch))))
   ;; found #\(
   (let loop2 ((argl argl) (argv '()) (ch (read-char)))
     (cond
      ((eof-object? ch)
       (throw 'cpp-error "EOF reading args"))
      ((char=? ch #\))
       (reverse (if (and (pair? argl) (string=? (car argl) "..."))
		    (acons "__VA_ARGS__" "" argv)
                    argv)))
      ((and (pair? argl) (string=? (car argl) "..."))
       (let* ((argx (scan+expand-to-mark defs used #\))))
	 (loop2 '() (acons "__VA_ARGS__" argx argv) (read-char))))
      ((or (char=? ch #\() (char=? ch #\,))
       (let ((repl (scan+expand-to-mark defs used #\))))
	 (cond
	  ((pair? argl)
	   (loop2 (cdr argl) (acons (car argl) (cons '$text repl) argv)
                  (read-char)))
	  ((string=? repl "") ;; ???  -- I think foo()
	   (loop2 argl argv (read-char)))
	  (else (throw 'cpp-error "function macro arg mismatch")))))
      (else
       (throw 'cpp-error "cpp.scm: collect-args coding error"))))))

;; @deffn {Procedure} px-cpp-ftn argd repl used => tokl
;; Pre-expand CPP function where @var{argd} is an a-list of arg name,
;; non-expanded token list pairs, and repl is the tokenized RHS.
;; @end deffn
(define (px-cpp-ftn argd repl used)

  ;; Turn reverse chl into a string and insert it into the token stream.
  (define (ins-chl chl stl)
    (if (null? chl) stl (cons (reverse-list->string chl) stl)))

  (define (rem-space chl)
    (let loop ((chl chl))
      (cond
       ((null? chl) chl)
       ((char-set-contains? c:ws (car chl)) (loop (cdr chl)))
       (else chl))))

  (define (mk-string str) (string-append "\"" (esc-c-str str) "\""))

  (define (loop stl chl nxt ch)
    ;; stl: str list; chl: char list; nxt: next str; ch: next char
    (cond
     (nxt (loop (cons nxt (ins-chl chl stl)) '() #f ch))
     ((eof-object? ch)
      (apply string-append (reverse (ins-chl chl stl))))
     ((char-set-contains? c:ws ch)
      (loop stl (cons #\space chl) nxt (skip-il-ws (read-char))))
     ((read-c-comm ch #f) (loop stl (cons #\space chl) nxt (read-char)))
     ((read-c-string ch) =>
      (lambda (st) (loop stl chl (mk-string (cdr st)) (read-char))))
     ((char=? #\( ch) (loop stl (cons ch chl) nxt (read-char)))
     ((char=? #\) ch) (loop stl (cons ch chl) nxt (read-char)))
     ((read-c-ident ch) =>		; replace if aval
      (lambda (iden)
	(loop stl chl (or (assoc-ref argd iden) iden) (read-char))))
     ((char=? #\# ch)
      (let ((ch (read-char)))
	(if (eqv? ch #\#)
	    (loop stl (rem-space chl) nxt (skip-il-ws (read-char)))
	    (let* ((aref (read-c-ident (skip-il-ws ch)))
		   (aval (assoc-ref argd aref)))
	      (if (not aref) (cpp-err "expecting arg-ref"))
	      (if (not aval) (cpp-err "expecting arg-val"))
	      (loop stl chl (mk-string aval) (read-char))))))
     (else (loop stl (cons ch chl) nxt (read-char)))))

  ;;(sferr "px-cpp-ftn\n")
  (with-input-from-string repl
    (lambda () (loop '() '() #f (read-char)))))

(define (pre-expand argd repl defs used)
  (define (maybe-x-id rg) ;; maybe expand ident
    (match rg (('$ident . id) (or (assoc-ref argd id) rg)) (__ rg)))
  (let loop ((tokl repl))
    (match tokl
      ('() tokl)
      ((lt '$dhash rt . rest)
       (let ((lx (maybe-x-id lt)) (rx (maybe-x-id rt)))
         (cons* lx rx (loop rest))))
      (('$hash rg . rest)
       (cons* `($string . ,(tokl->string (list (maybe-x-id rg)))) (loop rest)))
      ((`($ident . ,id) . rest)
       (cond
        ((assoc-ref argd id) =>
         (lambda (argval)
           (call-with-values (lambda () (macro-expand argval defs used))
             (lambda (tokl used) (append tokl (loop rest))))))
        (else (cons (car tokl) (loop (cdr tokl))))))
      (else (cons (car tokl) (loop (cdr tokl)))))))

;; @deffn {Procedure} expand-macro-ref ident defs sl used => repl used
;; This is the workhorse for expand-cpp-macro-ref.
;; @end deffn
(define (expand-macro-ref ident defs sl used)
  (let ((rval (assoc-ref defs ident)))
    (cond
     ((member ident used) (values #f used))
     ((string? rval) ;; simple macro
      ;; THIS IS ALL MESSED UP
      (let* ((used (cons ident used))
             (repl used (cpp-expand-text rval defs used)))
	(if #f ;; (ident-like? repl)
            (call-with-values
                (lambda () (expand-macro-ref repl defs sl used))
              (lambda (rpl used) (values (or rpl repl) used)))
	    (values repl used))))
     ((pair? rval)   ;; function macro
      ;;(sferr "  ~s: ~s is function macro\n" id ident)
      ;; GNU CPP manual: "A function-like macro is only expanded if its name
      ;; appears with a pair of parentheses after it.  If you just write the
      ;; name, it is left alone."
      ;;(sferr "  ~s: pre collect used=~s\n" id used)
      (call-with-values
          ;;(lambda () (collect-args (car rval) defs (cons ident used)))
          (lambda () (collect-args (car rval) defs ident))
        (lambda (argd used-no-no)
          ;;(sferr "  ~s: post collect used=~s\n" id used)
          ;;(sferr "  ~s: collected argd: ~s\n" id argd)
          (if argd
	      (let* ((used (cons ident used))
                     (prep (px-cpp-ftn argd (cdr rval)))
		     (repl used (cpp-expand-text prep defs used)))
		(if #f ;; (ident-like? repl)
                    (call-with-values
                        (lambda () (expand-macro-ref repl defs sl used))
                      (lambda (rpl used) (values (or rpl repl) used)))
		    (values repl used)))
              (values #f used)))))
     ((c99-std-val ident sl) => (lambda (repl) (values repl used)))
     ;;((string=? ident "_Pragma") (finish-pragma))
     ;;^ does not work here: move here when cpp is token-based
     (else (values #f used)))))


;; === tokenize & reverse ============

;; We just scanned "defined", now need to scan the arg to inhibit expansion.
;; For example, we have scanned "defined"; we now scan "(FOO)" or "FOO", and
;; return "defined(FOO)" or "defined FOO".
(define (scan-defined-arg)
  (let* ((ch (skip-il-ws (read-char))) (no-ec (not (char=? ch #\())))
    (let loop ((chl (list ch)) (ch (skip-il-ws (read-char))))
      (cond
       ((eof-object? ch)
	(if no-ec
	    (list->string (cons #\space (reverse chl)))
	    (cpp-err "illegal argument to `defined'")))
       ((char-set-contains? c:ir ch)
	(loop (cons ch chl) (read-char)))
       (no-ec
	(unread-char ch)
	(list->string (cons #\space (reverse chl))))
       ((char=? #\) (skip-il-ws ch))
	(reverse-list->string (cons #\) chl)))
       (else
	(cpp-err "illegal argument to  `defined'"))))))

;; must be (\s*<xxx>\s*) OR (\s*"xxx"\s*) => ("<xxx>") OR ("\"xxx\"")
(define (scan-arg-literal)
  (let ((ch (read-char)))
    ;; if exit, then did not defined __has_include(X)=__has_include__(X)
    (if (or (eof-object? ch) (not (char=? #\( ch)))
	(throw 'cpp-error "expecting `('")))
  (let loop ((chl '()) (ch (skip-il-ws (read-char))))
    (cond
     ((eof-object? ch) (cpp-err "illegal argument"))
     ((char=? #\) ch)
      (let loop2 ((res '()) (chl chl))
	(cond
	 ((null? chl)
	  (string-append "(\"" (esc-c-str (list->string res)) "\")"))
	 ((and (null? res) (char-whitespace? (car chl))) (loop2 res (cdr chl)))
	 (else (loop2 (cons (car chl) res) (cdr chl))))))
     (else (loop (cons ch chl) (read-char))))))

(define* (tokenize-to-mark mark #:optional trim-trailing-space)
  ;; mark must be one of #\, #\) #f
  ;; Does not eat the mark.
  (define (finish tkl)
    (reverse
     (if (and trim-trailing-space (pair? tkl) (eqv? '$space (car tkl)))
         (cdr tkl)
         tkl)))

  (let loop ((tkl '()) (lv 0) (ch (skip-il-ws (read-char))))
    (cond
     ((eof-object? ch) (finish tkl))
     ((and (eqv? mark ch) (zero? lv))
      (unread-char ch) (finish tkl))
     ((and mark (char=? #\) ch) (zero? lv)) ;; mark is #\, see #\)
      (unread-char ch) (finish tkl))
     ((char-set-contains? c:ws ch)	; whitespace
      (loop (cons '$space tkl) lv (skip-il-ws (read-char))))
     ((read-c-comm ch #f)		; comment
      (loop (cond ((null? tkl) tkl)
                  ((eq? #\space (car tkl)) tkl)
                  (else (cons '$space tkl)))
            lv (skip-il-ws (read-char))))
     ((read-c-ident ch) =>
      (lambda (iden)
	(cond
	 ((string=? iden "defined")
	  (loop (acons '$text (string-append iden (scan-defined-arg)) tkl)
                lv (read-char)))
	 ((member iden '("__has_include__" "__has_include_next__"))
	  (cond
	   ((scan-arg-literal) =>
	    (lambda (arg) (let ((tkn (string-append iden arg)))
	                    (loop (acons '$text tkn tkl) lv (read-char)))))
	   (else
	    (loop (acons '$ident iden tkl) lv (read-char)))))
	 (else
          (loop (acons '$ident iden tkl) lv (read-char))))))
     ((read-c-string ch) =>
      (lambda (pair) (loop (cons pair tkl) lv (read-char))))
     ((char=? #\( ch) (loop (cons ch tkl) (1+ lv) (read-char)))
     ((char=? #\) ch) (loop (cons ch tkl) (1- lv) (read-char)))
     (else (loop (cons ch tkl) lv (read-char))))))

(define (tokenize-args argl) ;; => args | #f
  (and
   (let loop1 ((sp #f) (ch (read-char)))
     (cond
      ((eof-object? ch) (if sp (unread-char #\space)) #f)
      ((char-set-contains? c:ws ch) (loop1 #t (read-char)))
      ((not (char=? #\( ch)) (if sp (unread-char #\space)) #f)))
   ;; found #\(
   (let loop2 ((argv '()) (ch #\() (argl argl))
     (cond
      ((eof-object? ch)
       (throw 'cpp-error "EOF reading args"))
      ((memq ch '(#\( #\,)) ; start of arg
       (let* ((anam (and (pair? argl) (car argl)))
              (atkl (tokenize-to-mark (if (equal? anam "...") #\) #\,) #t)))
         (loop2
	  (cond
           ((and (char=? #\( ch) (null? argl) (null? atkl)) argv)
           ((null? argl) (throw 'cpp-error "too many values"))
           ((string=? anam "...") (acons "__VA_ARGS__" atkl argv))
           (else (acons anam atkl argv)))
          (read-char) (if (pair? argl) (cdr argl) argl))))
      ((char=? ch #\))                  ; end of args
       (if (pair? argl)
           (throw 'cpp-error "function macro arg count mismatch")
           (reverse argv)))
      (else
       (throw 'cpp-error "cpp.scm(tokenize-args): coding error"))))))


;;.@deffn {Procedure} rtokl->string reverse-token-list => string
;; Convert reverse token-list to string.
;; @end deffn
(define (rtokl->string tokl)

  ;; Turn reverse chl into a string and insert it into the string list stl.
  (define (add-chl chl stl)
    (if (null? chl) stl (cons (list->string chl) stl)))

  ;; Works like this: Scan through the list of tokens (key-val pairs or
  ;; lone characters).  Lone characters are collected in a list (@code{chl});
  ;; pairs are converted into strings and combined with list of characters
  ;; into a list of strings.  When done the list of strings is combined to
  ;; one string.  (The token 'argval is expansion of argument.)
  (let loop ((stl '())		   ; list of strings to reverse-append
	     (chl '())		   ; char list
	     (nxt #f)		   ; next string to add after chl
	     (tkl tokl))	   ; input token list
    (cond
     (nxt
      (loop (cons nxt (add-chl chl stl)) '() #f tkl))
     ((null? tkl)
      (apply string-append (add-chl chl stl)))
     ((char? (car tkl))
      (loop stl (cons (car tkl) chl) nxt (cdr tkl)))
     (else
      (match tkl
	(`(($ident . ,rval) $dhash ($ident . ,lval) . ,rest)
	 (loop stl chl nxt
	       (acons '$ident (string-append lval rval) (list-tail tkl 3))))
	(`(($ident . ,arg) $hash . ,rest)
	 (loop stl chl (string-append "\"" arg "\"") (list-tail tkl 2)))
	(`(($ident . ,iden) ($ident . ,lval) . ,rest)
	 (loop stl chl iden rest))
	(`(($ident . ,iden) . ,rest)
	 (loop stl chl iden rest))
	(`(($string . ,val) . ,rest)
	 (loop stl (cons #\" chl) (esc-c-str val) (cons #\" rest)))
	(`(($text . ,val) . ,rest)
	 (loop stl chl val rest))
	(`($space $space . ,rest)
	 (loop stl chl nxt rest))
	(`($space . ,rest)
	 (loop stl (cons #\space chl) nxt rest))
	(`(($comm . ,val) . ,rest)
	 ;; replace comment with extra trailing space
	 (loop stl chl (string-append "/*" val "*/ ") rest))
	(`(,asis . ,rest)
	 (loop stl chl asis rest))
	(otherwise
	 (error "nyacc cpp rtokl->string, no match" tkl)))))))

(define (tokl->string tokl)
  (rtokl->string (reverse tokl)))

(export
 tokenize-to-mark
 tokenize-args
 tokl->string)
       
;; === convert to tokens and process

(define (maybe-expand-macro ident defs sloc used) ;; => tokl used
  (let ((rval (assoc-ref defs ident)))
    (cond
     ;;((member ident used) #f)
     ((string? rval) ;; simple macro
      (call-with-values
          (lambda () (macro-expand `(($ident . ,ident)) defs used))
        (lambda (repl used)
          (and repl (tokl->string repl)))))
     ((pair? rval)
      (let ((argd (tokenize-args (cdr rval))))
        (and argd
             (error "@@@")
             )
        (call-with-values
          (lambda () (collect-args (car rval) defs ident))
        (lambda (argd used-no-no)
          (if argd
	      (let* ((used (cons ident used))
                     (prep (px-cpp-ftn argd (cdr rval)))
		     (repl used (cpp-expand-text prep defs used)))
		(if #f ;; (ident-like? repl)
                    (call-with-values
                        (lambda ()
                          (expand-macro-ref repl defs sloc used))
                      (lambda (rpl used) (values (or rpl repl) used)))
		     (values repl used)))
              (values #f used))))))
     ((c99-std-val ident sloc) => (lambda (repl) (values repl used)))
     ;;((string=? ident "_Pragma") (finish-pragma))
     ;;^ does not work here: move here when cpp is token-based
     (else (values #f used)))))

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

;; @deffn {Procedure} eval-cpp-cond-text text [defs] => string
;; Evaluate CPP condition expression (text).
;; Undefined identifiers are replaced with @code{0}.
;; @end deffn
(define* (eval-cpp-cond-text text #:optional (defs '()) #:key (inc-dirs '()))
  (with-throw-handler
      'cpp-error
    (lambda ()
      (let* ((rhs used (cpp-expand-text text defs))
	     (exp (parse-cpp-expr rhs)))
        (eval-cpp-expr exp defs #:inc-dirs inc-dirs)))
    (lambda (key fmt . args)
      (report-error fmt args)
      (throw 'c99-error "CPP error"))))
;; ^ TODO: move below expand-cpp-name


;; @deffn {Procedure} expand-cpp-macro-ref ident defs sl => repl|#f
;; Given an identifier seen in the current input, this checks for associated
;; definition in @var{defs} (generated from CPP defines).  If found as simple
;; macro, the expansion is returned as a string.  If @var{ident} refers
;; to a macro with arguments, then the arguments will be read from the
;; current input.  The format of the @code{defs} entries are
;; @example
;; ("ABC" . "123")
;; ("MAX" ("X" "Y") . "((X)>(Y)?(X):(Y))")
;; @end example
;; @noindent
;; Note that this routine will look in the current-input so if you want to
;; expand text, you must use (with-input-from-string "" ...)
;; The argument @var{sl} is source location info.
;; @end deffn

(display "cpp.scm: fix expand-cpp-macro-ref\n")
(define (expand-cpp-macro-ref ident defs sl)
  (expand-macro-ref ident defs sl '()))
(define (xexpand-cpp-macro-ref ident defs sl)
  (false-if-exception
   (call-with-values
       (lambda () (expand-macro-ref ident defs sl '()))
     (lambda (repl used)
       (sferr "expand-cpp-macro-ref ~s => ~s\n" ident repl)
       repl))))

;; may not catch strings w/ non-matching parens
(define (skip-cpp-macro-ref name defs)
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

;; @deffn {Procedure} expand-cpp-name name defs => repl|#f
;; Calls @code{expand-cpp-macro-ref} with null input string (w/o further
;; input).  If @var{name} is has a function definition @code{#f} is returned.
;; @end deffn
(define (expand-cpp-name name defs sl)
  (with-input-from-string ""
    (lambda () (expand-cpp-macro-ref name defs sl))))

;;; --- last line ---
