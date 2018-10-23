;;; language/nx-tcl/spec.scm - NYACC extension for Tcl

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

(define-module (language nx-tcl spec)
  #:export (nx-tcl)
  #:use-module (nyacc lang tcl parser)
  #:use-module (nyacc lang tcl compile-tree-il)
  #:use-module (system base language))

(define-language nx-tcl
  #:title	"nx-tcl"
  #:reader	(lambda (p e)
		  (cond
		   ((and (file-port? p)
			 (string? (port-filename p))
			 (not (string-prefix? "/dev/" (port-filename p))))
		    (read-tcl-file p e))
		   (else (read-tcl-stmt p e))))
  #:compilers   `((tree-il . ,compile-tree-il))
  #:evaluator	(lambda (exp mod) (primitive-eval exp))
  #:printer	write
  #:make-default-environment
		(lambda ()
		  (let ((env (make-fresh-user-module)))
		    (module-define! env 'current-reader (make-fluid))
		    env))
  )

;; --- last line ---
