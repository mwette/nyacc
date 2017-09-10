;;; NYACC's javascript specification for Guile
;;;
;;; Copyright (C) 2015,2017 Matthew R. Wette
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

(define-module (language javascript spec)
  #:export (javascript)
  #:use-module (nyacc lang javascript separser)
  #:use-module (nyacc lang javascript compile-tree-il)
  #:use-module (nyacc lang javascript jslib)
  #:use-module (nyacc lang javascript pprint)
  #:use-module (system base language))

(define-language javascript
  #:title	"javascript"
  #:reader	js-reader
  #:compilers   `((tree-il . ,compile-tree-il))
  #:evaluator	(lambda (exp mod) (primitive-eval exp))
  #:printer	pretty-print-js)

;; --- last line ---
