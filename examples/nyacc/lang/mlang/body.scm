;;; lang/mlang/body.scm - common lexical generator code for mlang

;; Copyright (C) 2015,2018 Matthew R. Wette
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Code:

;; @deffn add-file-attr tl => tl
;; Given a tagged-list this routine adds an attribute @code{(file basename)}
;; which is the basename (with @code{.m} removed) of the current input.
;; This is used for the top-level node of the mlang parse tree to indicate
;; from which file the script or function file originated.  For example,
;; @example
;; (function-file (@ (file "myftn")) (fctn-defn (ident "myftn") ...
;; @end example
(define (add-file-attr tl)
  (let ((fn (port-filename (current-input-port))))
    (if fn (tl+attr tl 'file (basename fn ".m")) tl)))

;;; === lexical analyzer

;; @deffn {Procedure} mlang-read-string ch
;; Read string and return @code{($string . "string")}.  If @var{ch} is
;; not @code{"} or @code{'} the return value is @code{#f}.
;; @end deffn
(define (mlang-read-string ch)
  (case ch
    ((#\")
     (read-c-string ch))
    ((#\')
     (let loop ((cl '()) (ch (read-char)))
       (cond ((eq? ch #\\)
	      (let ((c1 (read-char)))
		(if (eq? c1 #\newline)
		    (loop cl (read-char))
		    (loop (cons c1 cl) (read-char)))))
	     ((eq? ch #\')
	      (let ((nextch (read-char)))
		(if (eq? #\' nextch)
		    (loop (cons ch cl) (read-char))
		    (begin
		      (unread-char nextch)
		      (cons '$string (list->string (reverse cl)))))))
	     (else (loop (cons ch cl) (read-char))))))
    (else #f)))


(define mlang-read-comm
  (make-comm-reader '(("%" . "\n") ("#" . "\n") ("#{" . "#}") ("%{" . "%}")
		      ("#!" . "!#"))))

;; elipsis reader "..." whitespace "\n"
(define (elipsis? ch)
  (if (eqv? ch #\.)
      (let ((c1 (read-char)))
	(if (eqv? c1 #\.)
	    (let ((c2 (read-char)))
	      (if (eqv? c2 #\.)
		  (cons (string->symbol "...") "...")
		  (begin (unread-char c2) (unread-char c1) #f)))
	    (begin (unread-char c1) #f)))
      #f))

(define (skip-to-next-line)
  (let loop ((ch (read-char)))
    (cond
     ((eof-object? ch) ch)
     ((eqv? ch #\newline) (read-char))
     (else (loop (read-char))))))

;; @deffn {Procedure} make-mlang-lexer-generator match-table
;; This function, given the @var{match-table} from a lalr-generated
;; machine, generates a procedure that returns lexical analyzers for
;; use in Octave parsers.  See @code{parse-oct}.
;; @end deffn
(define-public (make-mlang-lexer-generator match-table)
  ;; There is some trickery here to assure that if the last line
  ;; ends w/o newline then one gets inserted.
  (let* ((read-string mlang-read-string)
	 (read-comm mlang-read-comm)
	 (read-ident read-c$-ident)
	 (space-cs (string->char-set " \t\r\f"))
	 ;;
	 (strtab (filter-mt string? match-table)) ; strings in grammar
	 (kwstab (filter-mt like-c$-ident? strtab)) ; keyword strings =>
	 (keytab (map-mt string->symbol kwstab)) ; keywords in grammar
	 (chrseq (remove-mt like-c-ident? strtab)) ; character sequences
	 (symtab (filter-mt symbol? match-table)) ; symbols in grammar
	 (chrtab (filter-mt char? match-table))	; characters in grammar
	 (read-chseq (make-chseq-reader chrseq))
	 (newline-val (assoc-ref chrseq "\n"))
	 (assc-$ (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair)))))
    (if (not newline-val) (error "mlang setup error"))
    (lambda ()
      (let ((qms #f) (bol #t))		; qms: quote means string
	(lambda ()
	  (let loop ((ch (read-char)))
	    (cond
	     ((eof-object? ch)
	      (if bol (assc-$ (cons '$end ch)) (loop #\newline)))
 	     ((elipsis? ch) (loop (skip-to-next-line)))
	     ((eqv? ch #\newline) (set! bol #t) (cons newline-val "\n"))
	     ((char-set-contains? space-cs ch) (set! qms #t) (loop (read-char)))
	     ((read-comm ch bol) => (lambda (p) (set! bol #f) (assc-$ p)))
	     (bol (set! bol #f) (loop ch))
	     ((read-ident ch) =>
	      (lambda (s) ;; s is a string
		(set! qms #f)
		(or (and=> (assq-ref keytab (string->symbol s))
			   (lambda (tval) (cons tval s)))
		    (assc-$ (cons '$ident s)))))
	     ((read-c-num ch) => (lambda (p) (set! qms #f) (assc-$ p)))
	     ((char=? ch #\") (assc-$ (read-string ch)))
	     ((char=? ch #\') (if qms (assc-$ (read-string ch)) (read-chseq ch)))
	     ((read-chseq ch) =>
	      (lambda (p) (set! qms (and (memq ch '(#\= #\, #\()) #t)) p))
	     ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
	     (else (cons ch (string ch))))))))))

;;; --- last line ---
