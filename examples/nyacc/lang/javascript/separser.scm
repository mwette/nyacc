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
(define* (parse-js-selt #:key debug)
   (catch
   'nyacc-error
   (lambda ()
     (with-fluid*
	 *insert-semi* #t
	 (lambda () (raw-parser (gen-js-lexer) #:debug debug))))
   (lambda (key fmt . rest)
     (apply simple-format (current-error-port) fmt rest)
     #f)))

;; This is used for language support in guile REPL.  See Compiling to the
;; Virtual Machine in the Guile Reference Manual.
(define (js-reader port env)
  (let ((iport (current-input-port)))
    (dynamic-wind
	(lambda () (set-current-input-port port))
	(lambda () (and=> (parse-js-selt #:debug #f)
			  (lambda (s) `(Program (SourceElements ,s)))))
	(lambda () (set-current-input-port iport)))))

;; --- last line ---
