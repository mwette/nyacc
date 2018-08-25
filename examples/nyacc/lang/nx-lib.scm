;; nyacc/lang/nx-util.scm - run-time library

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
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Description:

;; This module provide run-time procecures for the NYACC extension (nx)
;; languages.  The intent is to provide a consistent data model between
;; nx languages so that they can inter-operate.

;;; Notes:

;; For OO languages we should use single-inheritance with interfaces.
;; An object is a hash table with entries for data and either
;;   A) a single class (type) entry, or
;;   B) one class (type) entry and N interface entries
;; This module will need to provide run-time type determination.  Well,
;; We need a procedure (obj-call obj name args)
;; 

;;; Todos:

;;  1) add traits (aka interfaces)

;;; Code:

(define-module (nyacc lang nx-lib)
  #:export (nx-get-method)
  )

;; @deffn {Procedure} nx-get-method obj name
;; find a 
;; @end deffn 
(define (nx-get-method obj name)
  #f)

(define fooo 1)

;; --- last line ---
