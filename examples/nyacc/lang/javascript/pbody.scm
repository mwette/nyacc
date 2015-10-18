;;; lang/ecmascript/pbody.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
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

;;; This is the javascript runtime.

(define (es-Boolean? v) (or (eq? #t v) (eq? #f v)))
(define (es-Null? v) (eq? 'Null v))

(define *insert-semi* (make-fluid))
  
(define (NSI) ;; no semicolon insertion
  (fluid-set! *insert-semi* #f))

(define read-js-ident
  (let* ((idf (char-set-union char-set:letter (string->char-set "_$")))
	 (idr (char-set-union idf char-set:digit)))
    (make-ident-reader idf idr)))

(define (read-js-string delim)
  (if (not (or (eq? delim #\') (eq? delim #\"))) #f
      (let iter ((cl '()) (ch (read-char)))
	(cond ((eq? ch #\\)
	       (let ((c1 (read-char)))
		 (if (eq? c1 #\newline)
		     (iter cl (read-char))
		     (iter (cons* c1 cl) (read-char)))))
	      ((eq? ch delim) (list->string (reverse cl)))
	      (else (iter (cons ch cl) (read-char)))))))

(define gen-js-lexer
  (let* ((match-table mtab)
	 (space-cs (string->char-set " \t\r"))
	 ;;
	 (strtab (filter-mt string? match-table)) ; strings in grammar
	 (kwstab (filter-mt like-c-ident? strtab)) ; keyword strings =>
	 (keytab (map-mt string->symbol kwstab)) ; keywords in grammar
	 (chrseq (remove-mt like-c-ident? strtab))  ; character sequences
	 (symtab (filter-mt symbol? match-table)) ; symbols in grammar
	 (chrtab (filter-mt char? match-table))	; characters in grammar
	 ;;
	 (read-chseq (make-chseq-reader chrseq))
	 (semicolon (assoc-ref chrseq ";"))
	 (assc-$ (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair))))
	 )
    (lambda ()
      (let ((bol #t))
	(lambda ()
          (define (cont rez) (fluid-set! *insert-semi* #t) rez)
          (cont
	  (let iter ((ch (read-char)))
	    (cond
	     ((eof-object? ch) (assc-$ (cons '$end ch)))
	     ((char-set-contains? space-cs ch) (iter (read-char)))
	     ((eqv? ch #\newline)
	      (if (fluid-ref *insert-semi*)
		  (cons semicolon ";")
		  (iter (read-char))))
	     ((and (eqv? ch #\newline) (set! bol #t) #f))
	     ((read-js-string ch) assc-$)
	     ((read-c-comm ch) (iter (read-char)))
	     ((read-js-ident ch) =>
	      (lambda (s)
		(or (and=> (assq-ref keytab (string->symbol s))
			   (lambda (tval) (cons tval s)))
		    (assc-$ (cons '$ident s)))))
	     ((read-c-num ch) => assc-$)
	     ((read-chseq ch) => identity)
	     ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
	     (else (cons ch (string ch))))))))))) ; should be error

#|
    (InputElementDiv
     (WhiteSpace) (LineTerminator) (Comment) (Token) (DivPunctuator))
    (InputElementRegExp
     (WhiteSpace) (LineTerminator) (Comment) (Token) (RegularExpressionLiteral))
    (WhiteSpace ("\t") ("\vt") ("\ff") (" ") ("&nbsp;") ("&usp;"))
    (LineTerminator ("\n") ("\r") (LS) (PS))
    (MultiLineComment ('multiline-comment))
|#

;;; --- last line ---
