;;; language/spec.scm - NYACC's calculator demo

;; Copyright (C) 2015,2018,2019 Matthew R. Wette
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

(define-module (language calc spec)
  #:export (calc)
  #:use-module (system base language)
  #:use-module (nyacc lang calc parser)
  #:use-module (nyacc lang calc compiler))

(define-language calc
  #:title	"calc"
  #:reader	read-calc
  #:compilers   `((tree-il . ,compile-tree-il))
  ;;#:compilers   `((cps . ,compile-cps))
  #:printer	write)

;; --- last line ---
