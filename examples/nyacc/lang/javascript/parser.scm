;;; lang/javascript/parser.scm

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
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Description:

;; This is a JavaScript parser for Guile.

(define-module (nyacc lang javascript parser)
  #:export (parse-js js-stmt-reader js-file-reader)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang sx-util)
  #:use-module (nyacc lang util))

(include-from-path "nyacc/lang/javascript/body.scm")

;; === file parser ===================

(include-from-path "nyacc/lang/javascript/mach.d/js-tab.scm")
(include-from-path "nyacc/lang/javascript/mach.d/js-act.scm")

(define gen-js-lexer (make-js-lexer-generator js-mtab))

(define raw-parser
  (make-lalr-parser (acons 'act-v js-act-v js-tables)))

;; @deffn {Procedure} parse-js-file [#:debug bool] 
;; to be documented
;; @end deffn
(define* (parse-js #:key debug)
  (catch 'nyacc-error
    (lambda ()
     (with-fluid* *insert-semi* #t
       (lambda () (raw-parser (gen-js-lexer) #:debug #f))))
    (lambda (key fmt . rest)
      (apply simple-format (current-error-port) fmt rest)
      #f)))

;; @deffn {Procedure} js-file-reader port env => sxml
;; Read file unit from port and return SXML AST.
;; @end deffn
(define (js-file-reader port env)
  (if (eof-object? (peek-char port))
      (read-char port)
      (with-input-from-port port parse-js)))

;; === interactive parser =============

(include-from-path "nyacc/lang/javascript/mach.d/ia-js-tab.scm")
(include-from-path "nyacc/lang/javascript/mach.d/ia-js-act.scm")

(define gen-ia-js-lexer (make-js-lexer-generator ia-js-mtab))

(define raw-ia-parser
  (make-lalr-parser
   (acons 'act-v ia-js-act-v ia-js-tables)
   #:interactive #t))

;; @deffn {Procedure} parse-js-stmt
;; Parse a program statement from interactive user.
;; @end deffn
(define (parse-js-stmt)
  (catch 'nyacc-error
    (lambda ()
      (with-fluid* *insert-semi* #t
	(lambda () (raw-ia-parser (gen-ia-js-lexer) #:debug #f))))
    (lambda (key fmt . rest)
      (apply simple-format (current-error-port) fmt rest)
      #f)))

;; If a syntax error is detected by the reader then we usually want to flush
;; input until an end of statement is seen.  And return #f
(define flush-input-after-error
  (let ((read-string (make-string-reader #\")))
    (lambda (port)
      (let loop ((ch (read-char port)))
	(cond
	 ((eqv? ch #\;) #f)
	 ((read-js-string ch) (loop (read-char port)))
	 ((read-c-comm ch #t) #f)
	 (else (loop (read-char port))))))))

(define (js-stmt-reader port env)
  (if (eof-object? (peek-char port))
      (read-char port)
      (let ((elt (with-input-from-port port parse-js-stmt)))
	(cond
	 ((equal? elt '(EmptyStatement)) #f)
	 (elt)
	 (else (flush-input-after-error port) #f)))))

;; --- last line ---
