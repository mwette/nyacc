;;; nyacc/lang/matlab/util.scm - matlab processing code
;;; 
;;; Copyright (C) 2016 Matthew R. Wette
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

;; utilities for processing output trees

(define-module (nyacc lang matlab util)
  #:export (
	    apply-ml-sem ;; apply static semantics
	    )
  #:use-module (nyacc lang matlab pprint)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)		; let*-values
  #:use-module ((sxml fold) #:select (foldts foldts*))
  #:use-module (sxml match)
  #:use-module (nyacc lang util)
  )

;; need to remove aref-or-call
;; only way is to know if ident is variable or function
;; local variables
;; @item
;; global variables
;; @item
;; function arguments
;; @item
;; look for str2func
;; @item
;; look in dict for function
;; @item
;; if function argument is function then use will always be w/ ftn ref (@@).
(define (apply-ml-sem tree . rest)
  (let* ((ml-dict (if (pair? rest) (car rest) '()))
	 )
    tree))

;; --- last line ---
