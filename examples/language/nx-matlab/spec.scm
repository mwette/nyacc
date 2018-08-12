;; Nyacc eXtension for matlab

;; Copyright (C) 2018 Matthew R. Wette
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

(define-module (language nx-matlab spec)
  #:export (nx-matlab)
  #:use-module (nyacc lang matlab parser)
  #:use-module (nyacc lang matlab compile-tree-il)
  #:use-module (nyacc lang matlab pprint)
  #:use-module (system base language))

(define-language nx-matlab
  #:title	"nx-matlab"
  #:reader	(lambda (p e) (if (eq? e (interaction-environment))
				  (ml-stmt-reader p e)
				  (ml-file-reader p e)))
  #:compilers   `((tree-il . ,compile-tree-il))
  #:evaluator	(lambda (exp mod) (primitive-eval exp))
  #:printer	pretty-print-ml
  #:make-default-environment
		(lambda ()
		  (let ((env (make-fresh-user-module)))
		    (module-define! env 'current-reader (make-fluid))
		    env))
  )

;; --- last line ---
