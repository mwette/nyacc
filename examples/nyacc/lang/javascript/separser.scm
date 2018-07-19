;;; nyacc/lang/javascript/separser.scm

;; Copyright (C) 2015-2018 Matthew R. Wette
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

;; JavaScript SourceElement parser - for interactive use

(define-module (nyacc lang javascript separser)
  #:export (parse-js-selt js-reader)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util))

(include-from-path "nyacc/lang/javascript/mach.d/setab.scm")
(include-from-path "nyacc/lang/javascript/body.scm")
(include-from-path "nyacc/lang/javascript/mach.d/seact.scm")

;; Parse given a token generator.  Uses fluid @code{*info*}.
(define raw-parser
  (make-lalr-ia-parser/num		; TEMPORARY 
   (list
    (cons 'len-v len-v)
    (cons 'pat-v pat-v)
    (cons 'rto-v rto-v)
    (cons 'mtab mtab)
    (cons 'act-v act-v))))

;; @deffn {Procedure} parse-js-selt [#:debug bool] 
;; Parse a source element.  We need to wrap as follows to use w/ compiler:
;; @example
;; `(Program (SourceElements ,(parse-js-selt)))
;; @end example
;; @end deffn
(define* (parse-js-selt)
   (catch
   'nyacc-error
   (lambda ()
     (with-fluid*
	 *insert-semi* #t
	 (lambda () (raw-parser (gen-js-lexer) #:debug #f))))
   (lambda (key fmt . rest)
     (apply simple-format (current-error-port) fmt rest)
     #f)))

;; If a syntax error is detected by the reader then we usually want to flush
;; input until an end of statement is seen.  And return #f
(define flush-input-after-error
  (let ((read-string (make-string-reader #\")))
    (lambda (port)
      (let iter ((ch (read-char port)))
	(cond
	 ((eqv? ch #\;) #f)
	 ((read-js-string ch) (iter (read-char port)))
	 ((read-c-comm ch #t) #f)
	 (else (iter (read-char port))))))))

;; This is used for language support in guile REPL.  See ``Compiling to the
;; Virtual Machine'' in the Guile Reference Manual.  What is not documented
;; is that the reader should return the eof-object to stop reading from the
;; port.
(define (js-reader port env)
  (if (eof-object? (peek-char port))
      (read-char port)
      (let ((selt (with-input-from-port port parse-js-selt)))
	(cond
	 ((equal? selt '(EmptyStatement)) #f)
	 (selt `(Program (SourceElements ,selt)))
	 (else (flush-input-after-error port) #f)))))


(define new-parse-js-selt
  (let ((raw-parser
	 (make-lalr-ia-parser/num
	  (list (cons 'len-v len-v) (cons 'pat-v pat-v) (cons 'rto-v rto-v)
		(cons 'mtab mtab) (cons 'act-v act-v)))))
    (lambda (new-parse-js-selt lexer)
      (catch 'nyacc-error
	(lambda ()
	  (with-fluid*
	      *insert-semi* #t
	    (lambda () (raw-parser lexer #:debug #f))))
	(lambda (key fmt . rest)
	  (apply simple-format (current-error-port) fmt rest)
	  #f)))))

(define js-user-reader
  (let ((lexer (gen-js-lexer)))
    (lambda (port env)
      (cond
       ((eof-object? (peek-char port))
	(error "separser: need to regen lexer") ;; (set! lexer (gen-js-lexer))
	(read-char port))
       (else
	(let ((selt (with-input-from-port port
		      (lambda () (new-parse-js-selt lexer)))))
	  (cond
	   ((equal? selt '(EmptyStatement)) #f)
	   (selt `(Program (SourceElements ,selt)))
	   (else (flush-input-after-error port) #f))))))))

;; --- last line ---
