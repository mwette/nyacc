;;; nyacc/lang/mlang/julia.scm - parsing 

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

;;; Code:

(define-module (nyacc lang julia parser)
  #:export (parse-jl)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
  #:use-module (sxml fold)
  #:use-module ((srfi srfi-1) #:select (fold)))

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

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


(define julia-read-comm
  (make-comm-reader '(("#" . "\n"))))

;; @deffn {Procedure} make-julia-lexer-generator match-table
;; This function, given the @var{match-table} from a lalr-generated
;; machine, generates a procedure that returns lexical analyzers for
;; use in Octave parsers. 
;; @end deffn
(define-public (make-julia-lexer-generator match-table)
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

;; === file parser 

(include-from-path "nyacc/lang/julia/mach.d/julia-tab.scm")
(include-from-path "nyacc/lang/mlang/mach.d/julia-act.scm")

(define gen-julia-lexer (make-julia-lexer-generator julia-mtab))

;; Parse given a token generator.
(define raw-parser
  (make-lalr-parser (acons 'act-v julia-act-v julia-tables)))

(define* (parse-julia #:key debug)
  (catch
   'nyacc-error
   (lambda ()
     (apply-mlang-statics
      (raw-parser (gen-julia-lexer) #:debug debug)))
   (lambda (key fmt . args)
     (apply simple-format (current-error-port) fmt args)
     (newline (current-error-port))
     #f)))

(define (read-julia-file port env)
  (with-input-from-port port
    (lambda ()
      (if (eof-object? (peek-char port))
	  (read-char port)
	  (parse-julia #:debug #f)))))

;; --- last line ---
