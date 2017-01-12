;;; nyacc/lang/c99/cppbody.scm
;;;
;;; Copyright (C) 2016-2017 Matthew R. Wette
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

(define (cpp-err fmt . args)
  (apply throw 'cpp-error fmt args))

;;.@deffn skip-ws ch
(define (skip-ws ch)
  (if (eof-object? ch) ch
      (if (char-set-contains? c:ws ch)
	  (skip-ws (read-char))
	  ch)))

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
	    ((fixed) (string->number (tx1 tree)))
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

;; @deffn scan-cpp-input argd used dict for-argl => string
;; Process replacement text from the input port and generate a (reversed)
;; token-list.  If for-argl, stop at, and push back, @code{,} or @code{)}.
;; This is called by: collect-args, expand-cpp-repl, cpp-expand-text
(define (scan-cpp-input argd dict used for-argl)
  (let ((result (x-scan-cpp-input argd dict used for-argl)))
    (simple-format #t "scan=> ~S\n" result)
    result))
(define (x-scan-cpp-input argd dict used for-argl)
  ;; Works like this: scan tokens (comments, parens, strings, char's, etc).
  ;; Tokens (i.e., strings) are collected in a (reverse ordered) list (stl)
  ;; and merged together on return.  Lone characters are collected in the
  ;; list @code{chl}.  Once a token border is seen the character list is
  ;; converted to a string and added to the string list first, followed by
  ;; the new token.

  ;; Turn reverse chl into a string and insert it into the string list stl.
  (define (add-chl chl stl)
    (if (null? chl) stl (cons (list->string (reverse chl)) stl)))

  ;; used when we see `#foo'; converts foo to "foo"
  (define (stringify str)
    (string-append "\"" str "\""))

  (define conjoin string-append)

  ;; We just scanned "defined", now need to scan the arg to inhibit expansion.
  ;; For example, we have scanned "defined"; we now scan "(FOO)" or "FOO", and
  ;; return "defined(FOO)".  We use ec (end-char) as state indicator: nul at
  ;; start, #\) on seeing #\( or #\space if other.
  (define (scan-defined)
    (let* ((ch (skip-ws (read-char))) (ec (if (char=? ch #\() #\) #\space)))
      (let iter ((chl '(#\()) (ec ec) (ch ch))
	(cond
	 ((and (eof-object? ch) (char=? #\space ec))
	  (string-append "defined" (list->string (reverse (cons #\) chl)))))
	 ((eof-object? ch) (cpp-err "illegal argument to `defined'"))
	 ((and (char=? ch #\)) (char=? ec #\)))
	  (string-append "defined" (list->string (reverse (cons ch chl)))))
	 ((char-set-contains? c:ir ch)
	  (iter (cons ch chl) ec (read-char)))
	 (else (cpp-err "illegal identifier"))))))

  (let iter ((stl '())		; string list (i.e., tokens)
	     (chl '())		; char-list (current list of input chars)
	     (nxt #f)		; next string 
	     (lvl 0)		; level
	     (ch (read-char)))	; next character
    (simple-format #t "  iter stl=~S chl=~S nxt=~S lvl=~S ch=~S\n"
		   stl chl nxt lvl ch)
    (cond
     ;; have item to add, but first add in char's
     (nxt (iter (cons nxt (add-chl chl stl)) '() #f lvl ch))
     ;; If end of string or see end-ch at level 0, then return.
     ((eof-object? ch)  ;; CHECK (ab++)
      (apply string-append (reverse (add-chl chl stl))))
     ((and for-argl (memq ch '(#\) #\,)) (zero? lvl))
      (unread-char ch) (apply string-append (reverse (add-chl chl stl))))
     ((read-c-comm ch #f) =>
      (lambda (cp) (iter stl chl (string-append "/*" (cdr cp) "*/")
			 lvl (read-char))))
     ;; not sure about this:
     ((char-set-contains? c:ws ch) (iter stl chl nxt lvl (read-char)))
     ((char=? #\( ch) (iter stl (cons ch chl) nxt (1+ lvl) (read-char)))
     ((char=? #\) ch) (iter stl (cons ch chl) nxt (1- lvl) (read-char)))
     ((char=? #\# ch)
      (let ((ch (read-char)))
	(if (eqv? ch #\#)
	    (iter stl chl "##" lvl (read-char))
	    (iter stl chl "#" lvl ch))))
     ((read-c-string ch) =>
      (lambda (st) (iter stl chl st lvl (read-char))))
     ((read-c-ident ch) =>
      (lambda (iden)
	(simple-format #t "  read-c-ident => ~S\n" iden)
	(if (equal? iden "defined")
	    ;; "defined" is a special case
	    (iter stl chl (scan-defined) lvl (read-char))
	    ;; otherwise ...
	    (let* ((aval (assoc-ref argd iden))  ; lookup argument
		   (rval (assoc-ref dict iden))) ; lookup macro def
	      (simple-format #t "    aval=~S rval=~S\n" aval rval)
	      (cond
	       ((and (pair? stl) (string=? "#" (car stl)))
		;;(simple-format #t "TEST iden=~S aval=~S\n" iden aval)
		(iter (cdr stl) chl (stringify aval) lvl (read-char)))
	       ((and (pair? stl) (string=? "##" (car stl)))
		(simple-format #t "TEST iden=~S aval=~S\n" iden aval)
		(iter (cddr stl) chl (conjoin (cadr stl) aval) lvl (read-char)))
	       ((member iden used)	; name used
		(iter stl chl iden lvl (read-char)))
	       (aval			; arg ref
		(iter stl chl aval lvl (read-char)))
	       ((string? rval)		; cpp repl
		(iter stl chl rval lvl (read-char)))
	       ((pair? rval)		; cpp macro
		(let* ((argl (car rval)) (text (cdr rval))
		       (argv (collect-args argd dict used))
		       (argd (map cons argl argv))
		       (newl (expand-cpp-repl text argd dict (cons iden used))))
		  (iter stl chl newl lvl (read-char))))
	       (else			; normal identifier
		(simple-format #t "normal id stl=~S\n" stl)
		(iter stl chl iden lvl (read-char))))))))
     (else
      (iter stl (cons ch chl) #f lvl (read-char))))))

;; @deffn collect-args argd dict used
;; to be documented
(define (collect-args argd dict used)
  (if (not (eqv? (skip-ws (read-char)) #\()) (cpp-err "CPP expecting `('"))
  (let iter ((argl (list (scan-cpp-input argd dict used #t))))
    (let ((ch (read-char)))
      (if (eqv? ch #\)) (reverse argl)
	  (iter (cons (scan-cpp-input argd dict used #t) argl))))))

;; @deffn expand-cpp-repl
;; to be documented
(define (expand-cpp-repl repl argd dict used)
  (with-input-from-string repl
    (lambda () (scan-cpp-input argd dict used #f))))

;; @deffn cpp-expand-text text dict => string
(define (cpp-expand-text text dict)
  (with-input-from-string text
    (lambda () (scan-cpp-input '() dict '() #f))))

;; @deffn expand-cpp-mref ident dict => repl|#f
;; Given an identifier seen in C99 input, this checks for associated
;; definition in @var{dict} (generated from CPP defines).  If found,
;; the expansion is returned as a string.  If @var{ident} refers
;; to a macro with arguments, then the arguments will be read from the
;; current input.
(define (expand-cpp-mref ident dict . rest)
  (let ((used (if (pair? rest) (car rest) '()))
	(rval (assoc-ref dict ident)))
    (cond
     ((not rval) #f)
     ((member ident used) ident)
     ((string? rval)
      (let ((expd (expand-cpp-repl rval '() dict (cons ident used))))
	expd))
     ((pair? rval)
      (let* ((args (car rval)) (repl (cdr rval))
	     (argv (collect-args '() dict '()))
	     (argd (map cons args argv))
	     (xx (simple-format #t "args=~S argv=~S argd=~S repl=~S\n"
				args argv argd repl))
	     (expd (expand-cpp-repl repl argd dict (cons ident used))))
	expd)))))

;;; --- last line ---
