;;; lang/javascript/body.scm

;; Copyright (C) 2015,2017 Matthew R. Wette
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

;;; This is the javascript runtime.

(define (js-Boolean? v) (or (eq? #t v) (eq? #f v)))
(define (js-Null? v) (eq? 'Null v))

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

(define read-js-ident
  (let* ((idf (char-set-union char-set:letter (string->char-set "_$")))
	 (idr (char-set-union idf char-set:digit)))
    (make-ident-reader idf idr)))

;; @deffn {Procecure} read-js-string ch => ($string "abc") | #f
(define-public (read-js-string delim)
  (if (not (or (eq? delim #\') (eq? delim #\"))) #f
      (let iter ((cl '()) (ch (read-char)))
	(cond ((eq? ch #\\)
	       (let ((c1 (read-char)))
		 (iter
		  (case c1 		; see sec 7.8.4 in es5.1 spec
		    ((#\newline) (throw 'js-err "newline in string literal"))
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
	      (else (iter (cons ch cl) (read-char)))))))

;; Add #! ... !# to comment format so that we can use shebang scripts.
(define read-js-comm
  (make-comm-reader '(("/*" . "*/") ("//" . "\n")
		      ("#!" . "!#") ("#lang" . "\n"))))

;; maybe turn double newline into ';'
(define-public (make-js-lexer-generator match-table)
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
	   (let iter ((ch (read-char)))
	     (cond
	      ((eof-object? ch) (assc-$ (cons '$end ch)))
	      ((char-set-contains? space-cs ch) (iter (read-char)))
	      ((eqv? ch #\newline)
	       (set! bol #t)
	       (if (fluid-ref *insert-semi*)
		   (cons semicolon "\n")
		   (iter (read-char))))
	      ((read-js-comm ch bol) (iter (read-char)))
	      ((and (set! bol #f) #f))
	      ((read-js-string ch) => assc-$)
	      ((read-js-ident ch) =>
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

;;; --- last line ---
