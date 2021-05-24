;; language/nx-mlang/spec.scm - NYACC extension for Octave

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

(define-module (language nx-mlang spec)
  #:export (nx-mlang)
  #:use-module (nyacc lang mlang parser)
  #:use-module (nyacc lang mlang compile-tree-il)
  #:use-module (nyacc lang mlang pprint)
  #:use-module (system base language))

(define-language nx-mlang
  #:title	"nx-mlang"
  #:reader	(lambda (p e)
		  (cond
		   ((and (file-port? p)
			 (string? (port-filename p))
			 (not (string-prefix? "/dev/" (port-filename p))))
		    (read-mlang-file p e))
		   (else (read-mlang-stmt p e))))
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
