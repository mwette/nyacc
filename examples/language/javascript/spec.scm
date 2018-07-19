;;; NYACC javascript specification for Guile

;; Copyright (C) 2015,2017-2018 Matthew R. Wette
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

(define-module (language javascript spec)
  #:export (javascript)
  #:use-module (nyacc lang javascript separser)
  #:use-module (nyacc lang javascript compile-tree-il)
  #:use-module (nyacc lang javascript jslib)
  #:use-module (nyacc lang javascript pprint)
  #:use-module (system base language))

;; so probably the reader should have 

(define-language javascript
  #:title	"javascript"
  #:reader	js-reader
  ;;#:reader	(lambda (p e) (if (eq? e (interaction-enviornment))
  ;;				  (js-user-reader p e)
  ;;				  (js-file-reader p e)))
  #:compilers   `((tree-il . ,compile-tree-il))
  #:evaluator	(lambda (exp mod) (primitive-eval exp))
  #:printer	pretty-print-js
  ;;#:joiner	(lambda (exps env) (cons 'SourceStatements (map cddr exps)))
  #:make-default-environment
		(lambda ()
		  ;; ripoff from language/scheme/spec.scm
		  (let ((env (make-fresh-user-module)))
		    (module-define! env 'current-reader (make-fluid))
		    env))
  )

;; --- last line ---
