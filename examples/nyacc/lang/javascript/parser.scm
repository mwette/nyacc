;;; lang/javascript/parser.scm
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

;; JavaScript parser

(define-module (nyacc lang javascript parser)
  #:export (parse-js)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util)
  )

;;(include "tables.scm")
(include-from-path "nyacc/lang/javascript/tables.scm")
;;(include "pbody.scm")
(include-from-path "nyacc/lang/javascript/pbody.scm")
;;(include "actions.scm")
(include-from-path "nyacc/lang/javascript/actions.scm")

;; Parse given a token generator.  Uses fluid @code{*info*}.
(define raw-parser
  (make-lalr-parser 
   (list
    (cons 'len-v len-v)
    (cons 'pat-v pat-v)
    (cons 'rto-v rto-v)
    (cons 'mtab mtab)
    (cons 'act-v act-v))))

;; @item parse-js [#:debug bool] 
;; to be documented
(define* (parse-js #:key debug)
  (catch
   'parse-error
   (lambda ()
     (with-fluid*
	 *insert-semi* #t
	 (lambda () (raw-parser (gen-js-lexer) #:debug #f))))
   (lambda (key fmt . rest)
     (apply simple-format (current-error-port) fmt rest)
     #f)))

;; --- last line ---
