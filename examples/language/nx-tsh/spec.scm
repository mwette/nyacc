;;; language/nx-tsh/spec.scm - NYACC extension for Tclish

;; Copyright (C) 2021,2023 Matthew Wette
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

(define-module (language nx-tsh spec)
  #:export (nx-tsh)
  #:use-module (nyacc lang tsh parser)
  #:use-module (nyacc lang tsh compile-tree-il)
  #:use-module (system base language)
  #:use-module (ice-9 exceptions))

(define-language nx-tsh
  #:title	"nx-tsh"
  #:reader	(lambda (p e)
		  (if (and (file-port? p)
			   (string? (port-filename p))
			   (not (string-prefix? "/dev/" (port-filename p))))
		      (read-tsh-file p e)
		      (read-tsh-stmt p e)))
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
