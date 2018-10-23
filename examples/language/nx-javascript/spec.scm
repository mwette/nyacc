;; language/nx-javascript/spec.scm - NYACC extension for JavaScript

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

;;; Code:

(define-module (language nx-javascript spec)
  #:export (nx-javascript)
  #:use-module (nyacc lang javascript parser)
  #:use-module (nyacc lang javascript compile-tree-il)
  #:use-module (nyacc lang javascript pprint)
  #:use-module (system base language))

(define-language nx-javascript
  #:title	"nx-javascript"
  #:reader	(lambda (p e)
		  (cond
		   ((and (file-port? p)
			 (string? (port-filename p))
			 (not (string-prefix? "/dev/" (port-filename p))))
		    (read-js-file p e))
		   (else (read-js-stmt p e))))
  #:compilers   `((tree-il . ,compile-tree-il))
  #:evaluator	(lambda (exp mod) (primitive-eval exp))
  #:printer	pretty-print-js
  #:make-default-environment
		(lambda ()
		  ;; ripoff from language/scheme/spec.scm
		  (let ((env (make-fresh-user-module)))
		    (module-define! env 'current-reader (make-fluid))
		    env)))

;; --- last line ---
