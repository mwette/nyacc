;;; lang/lua/parser.scm

;; Copyright (C) 2021 Matthew R. Wette
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

;;; Description:

;; This is a Lua parser for Guile.

(define-module (nyacc lang lua parser)
  #:export (parse-lua read-lua-stmt read-lua-file)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang sx-util)
  #:use-module (nyacc lang util))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(use-modules (ice-9 pretty-print))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

;; === body ==========================

;; (define (lua-Boolean? v) (or (eq? #t v) (eq? #f v)))
;; (define (lua-Null? v) (eq? 'Null v))

;; This is the state to determine if #\newline gets converted to semicolon.
;; The ECMA-262 specification (3rd ed., Sec 7.9.1) says that conversion is done
;; if the parser would otherwise generate an error on the following token.  For
;; now, there is handshaking between the lexer and parser to state when
;; conversion is not allowed (e.g., between "return" and expression) and the
;; lexer does the conversion.  The real solution is to modify the parser to
;; check on error and see if last token was newline.  Right now user needs to
;; be careful about linebreaks in his/her code.

(define *insert-semi* (make-fluid))

(define (NSI) ;; no semicolon insertion
  (fluid-set! *insert-semi* #f))

(define read-lua-ident
  (let* ((idf (char-set-union char-set:letter (string->char-set "_$")))
	 (idr (char-set-union idf char-set:digit)))
    (make-ident-reader idf idr)))

;; @deffn {Procecure} read-lua-string ch => ($string "abc") | #f
(define-public (read-lua-string delim)
  (if (not (or (eq? delim #\') (eq? delim #\"))) #f
      (let loop ((cl '()) (ch (read-char)))
	(cond ((eq? ch #\\)
	       (let ((c1 (read-char)))
		 (loop
		  (case c1 		; see sec 7.8.4 in es5.1 spec
		    ((#\newline) (throw 'lua-err "newline in string literal"))
		    ((#\' #\" #\\) (cons c1 cl))
		    ((#\b) (cons #\backspace cl))
		    ((#\f) (cons #\page cl))
		    ((#\n) (cons #\newline cl))
		    ((#\r) (cons #\return cl))
		    ((#\t) (cons #\tab cl))
		    ((#\v) (cons #\vtab cl))
		    (else (cons c1 cl)))
		  (read-char))))
	      ((eq? ch delim) (cons '$string (list->string (reverse cl))))
	      (else (loop (cons ch cl) (read-char)))))))

;; Add #! ... !# to comment format so that we can use shebang scripts.
(define read-lua-comm
  (make-comm-reader '(("//" . "\n") ("#!" . "!#")) #:eat-newline #f))

;; maybe turn double newline into ';'
(define-public (make-lua-lexer-generator match-table)
  (let* ((space-cs (string->char-set " \t\r"))
	 ;;
	 (strtab (filter-mt string? match-table))  ; strings in gram
	 (kwstab (filter-mt like-c-ident? strtab)) ; keywd strings =>
	 (keytab (map-mt string->symbol kwstab))   ; keywds in gram
	 (chrseq (remove-mt like-c-ident? strtab)) ; character seq's
	 (symtab (filter-mt symbol? match-table))  ; sym's in gram
	 (chrtab (filter-mt char? match-table))	   ; char's in gram
	 ;;
	 (read-chseq (make-chseq-reader chrseq))
	 (semicolon (assoc-ref chrseq ";"))
	 (assc-$ (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair)))))
    (lambda ()
      (let ((bol #t))
	(lambda ()
          (define (process lexeme) (fluid-set! *insert-semi* #t) lexeme)
          (process
	   (let loop ((ch (read-char)))
	     (cond
	      ((eof-object? ch) (assc-$ (cons '$end ch)))
	      ((char-set-contains? space-cs ch) (loop (read-char)))
	      ((eqv? ch #\newline)
	       (set! bol #t)
	       (if (fluid-ref *insert-semi*)
		   (cons semicolon "\n")
		   (loop (read-char))))
	      ((read-lua-comm ch bol) (loop (read-char)))
	      ((and (set! bol #f) #f))
	      ((read-lua-string ch) => assc-$)
	      ((read-lua-ident ch) =>
	       (lambda (s)
		 (or (and=> (assq-ref keytab (string->symbol s))
			    (lambda (tval) (cons tval s)))
		     (assc-$ (cons '$ident s)))))
	      ((read-c-num ch) => assc-$)
	      ((read-chseq ch) => identity)
	      ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
	      (else (cons ch (string ch))))))))))) ; should be error


;; (InputElementDiv
;;  (WhiteSpace) (LineTerminator) (Comment) (Token) (DivPunctuator))
;; (InputElementRegExp
;;  (WhiteSpace) (LineTerminator) (Comment) (Token) (RegularExpressionLiteral)
;;  (WhiteSpace ("\t") ("\vt") ("\ff") (" ") ("&nbsp;") ("&usp;"))
;; (LineTerminator ("\n") ("\r") (LS) (PS))
;; (MultiLineComment ('multiline-comment))

;; === file parser ===================

(include-from-path "nyacc/lang/lua/mach.d/lua-tab.scm")
(include-from-path "nyacc/lang/lua/mach.d/lua-act.scm")

(define gen-lua-lexer (make-lua-lexer-generator (assq-ref lua-mach 'mtab)))

(define raw-parser
  (make-lalr-parser (acons 'act-v lua-act-v lua-tables)))

;; @deffn {Procedure} parse-lua [#:debug bool] 
;; to be documented
;; @end deffn
(define* (parse-lua #:key debug)
  (catch 'nyacc-error
    (lambda ()
     (with-fluid* *insert-semi* #t
       (lambda () (raw-parser (gen-lua-lexer) #:debug #f))))
    (lambda (key fmt . rest)
      (apply simple-format (current-error-port) fmt rest)
      #f)))

;; @deffn {Procedure} read-lua-file port env => sxml
;; Read file unit from port and return SXML AST.
;; @end deffn
(define (read-lua-file port env)
  (if (eof-object? (peek-char port))
      (read-char port)
      (with-input-from-port port parse-js)))

;; === interactive parser =============

;; If a syntax error is detected by the reader then we usually want to flush
;; input until an end of statement is seen.  And return #f
(define flush-input-after-error
  (let ((read-string (make-string-reader #\")))
    (lambda (port)
      (let loop ((ch (read-char port)))
	(cond
	 ((eqv? ch #\;) #f)
	 ((read-lua-string ch) (loop (read-char port)))
	 ((read-c-comm ch #t) #f)
	 (else (loop (read-char port))))))))

(include-from-path "nyacc/lang/lua/mach.d/luaia-tab.scm")
(include-from-path "nyacc/lang/lua/mach.d/luaia-act.scm")

(define gen-luaia-lexer (make-lua-lexer-generator luaia-mtab))

(define raw-ia-parser
  (make-lalr-parser
   (acons 'act-v luaia-act-v luaia-tables)
   #:interactive #t))

;; @deffn {Procedure} parse-lua-stmt
;; Parse a program statement from interactive user.
;; @end deffn
(define (parse-lua-stmt)
  (catch 'nyacc-error
    (lambda ()
      (with-fluid* *insert-semi* #t
	(lambda () (raw-ia-parser (gen-luaia-lexer) #:debug #f))))
    (lambda (key fmt . rest)
      (apply simple-format (current-error-port) fmt rest)
      #f)))
(export parse-lua-stmt)

(define (read-lua-stmt port env)
  (if (eof-object? (peek-char port))
      (read-char port)
      (let ((elt (with-input-from-port port parse-lua-stmt)))
	(cond
	 ((equal? elt '(EmptyStatement)) #f)
	 (elt)
	 (else (flush-input-after-error port) #f)))))

;; --- last line ---
