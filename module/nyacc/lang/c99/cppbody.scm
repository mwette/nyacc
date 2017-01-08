;;; nyacc/lang/c99/cppbody.scm
;;;
;;; Copyright (C) 2016 Matthew R. Wette
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

(define gen-cpp-lexer (make-lexer-generator mtab))

;; @deffn parse-cpp-expr text => tree
;; Given a string returns a cpp parse tree.  This is called by
;; @code{parse-cpp-stmt} and @code{eval-cpp-expr}.  The latter because the
;; parsed expression may include terms which are cpp-defined
;; and should be evaluated lazy mode.
(define (parse-cpp-expr text)
  (with-input-from-string text
    (lambda () (raw-parser (gen-cpp-lexer)))))

;; @deffn eval-cpp-expr tree dict => datum
;; Evaluate a tree produced from
;; This should be updated to use @code{expand-cpp-def}.  See below.
(use-modules (ice-9 pretty-print))
(define (eval-cpp-expr tree dict)
  ;;(display "eval-cpp-expr:\n") (pretty-print tree)
  (letrec
      ((tx (lambda (tr ix) (list-ref tr ix)))
       (tx1 (lambda (tr) (tx tr 1)))
       (ev (lambda (ex ix) (eval-expr (list-ref ex ix))))
       (ev1 (lambda (ex) (ev ex 1)))
       (ev2 (lambda (ex) (ev ex 2)))
       (ev3 (lambda (ex) (ev ex 3)))
       #;(parse-and-eval
	(lambda (str)
	  (if (not (string? str)) (throw 'parse-error "cpp-eval"))
	  (let ((idtr (parse-cpp-expr str)))
	    (eval-cpp-expr idtr dict))))
       (eval-expr
	(lambda (tree)
	  (case (car tree)
	    ;;((ident) (parse-and-eval (assoc-ref dict (tx1 tree))))
	    ((fixed) (string->number (tx1 tree)))
	    ((char) (char->integer (tx1 tree)))
	    ((defined) (if (assoc-ref dict (tx1 tree)) 1 0))
	    ;;
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
	    (else (error "incomplete implementation"))))))
    (catch 'parse-error
	   (lambda () (eval-expr tree))
	   (lambda () #f))))


;; @deffn scan-cpp-input argd used dict for-argl => string
;; Process the replacement text and generate a (reversed) token-list.
;; If for-argl, stop at, and push back, @code{,} or @code{)}.
(define (scan-cpp-input argd dict used for-argl)
  ;; Works like this: scan tokens (comments, parens, strings, char's, etc).
  ;; Tokens (i.e., strings) are collected in a (reverse ordered) list (stl)
  ;; and merged together on return.  Lone characters are collected in the
  ;; list @code{chl}.  Once a non-char token is found the character list is
  ;; converted to a string and added to the string list first, followed by
  ;; the new token.

  ;; Turn reverse chl into a string and insert it into the string list stl.
  (define (add-chl chl stl)
    (if (null? chl) stl (cons (list->string (reverse chl)) stl)))

  ;; We just scanned "defined", not need to scan the arg to inhibit expansion.
  ;; E.g., scanned "defined", now scan "(FOO)", and return "defined(FOO)".
  (define (scan-defined)
    (let iter ((chl '()) (ch (read-char)))
      (cond ((eof-object? ch) (throw 'parse-error "bad CPP defined"))
	    ((char=? #\) ch)
	     (string-append "defined" (list->string (reverse (cons ch chl)))))
	    (else (iter (cons ch chl) (read-char))))))
  
  ;; 
  (let iter ((stl '())		; string list (i.e., tokens)
	     (chl '())		; char-list (current list of input chars)
	     (nxt #f)		; next string 
	     (lvl 0)		; level
	     (ch (read-char)))	; next character
    ;;(simple-format #t "iter stl=~S chl=~S nxt=~S ch=~S\n" stl chl nxt ch)
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
     ((char=? #\( ch) (iter stl (cons ch chl) nxt (1+ lvl) (read-char)))
     ((char=? #\) ch) (iter stl (cons ch chl) nxt (1- lvl) (read-char)))
     ((char=? #\# ch)
      (let ((ch (read-char)))
	(if (eqv? ch #\#)
	    (iter (cons "##" stl) chl #f lvl (read-char))
	    (iter (cons "#" stl) chl #f lvl ch))))
     ((read-c-string ch) =>
      (lambda (st) (iter stl chl st lvl (read-char))))
     ((read-c-ident ch) =>
      (lambda (iden)
	;;(simple-format #t "    iden=~S\n" iden)
	(if (equal? iden "defined")
	    ;; "defined" is a special case
	    (iter stl chl (scan-defined) lvl (read-char))
	    ;; otherwise ...
	    (let* ((aval (assoc-ref argd iden))  ; lookup argument
		   (rval (assoc-ref dict iden))) ; lookup macro def
	      (cond
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
		(iter stl chl iden lvl (read-char))))))))
     (else
      (iter stl (cons ch chl) #f lvl (read-char))))))
  
(define (collect-args argd dict used)
  ;;(simple-format #t "collect-args\n")
  (if (not (eqv? (skip-ws (read-char)) #\())
      (let ((fn (or (port-filename (current-input-port)) "(unknown)"))
	    (ln (1+ (port-line (current-input-port)))))
	(throw 'parse-error "~A:~A: CPP expecting `('" fn ln)))
  (let iter ((argl (list (scan-cpp-input argd dict used #t))))
    ;;(simple-format #t "args: ~S\n" argl)
    (let ((ch (read-char)))
      (if (eqv? ch #\)) (reverse argl)
	  (iter (cons (scan-cpp-input argd dict used #t) argl))))))
    
(define (expand-cpp-repl repl argd dict used)
  ;;(simple-format #t "expand-cpp-repl repl=~S argd=~S\n" repl argd)
  (with-input-from-string repl
    (lambda () (scan-cpp-input argd dict used #f))))

;; @deffn cpp-expand-text text dict => string
(define (cpp-expand-text text dict)
  ;;(simple-format #t "cpp-expand-text: ~S\n" text)
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
	;;(simple-format #t "expand ~S => ~S\n" ident expd)
	expd))
     ((pair? rval)
      (let* ((args (car rval)) (repl (cdr rval))
	     (argv (collect-args '() dict '()))
	     (argd (map cons args argv))
	     (expd (expand-cpp-repl repl argd dict (cons ident used))))
	;;(simple-format #t "expand ~S => ~S\n" ident expd)
	expd)))))

;;; --- last line ---
