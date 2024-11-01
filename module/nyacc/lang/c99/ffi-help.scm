;;; examples/nyacc/lang/c99/ffi-help.scm

;; Copyright (C) 2016-2024 Matthew Wette
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

;;; Code:

(define-module (nyacc lang c99 ffi-help)
  #:use-module (nyacc lang c99 ffi-help-cd)
  #:re-export (*ffi-help-version*
               define-ffi-module
               compile-ffi-file
               load-include-file
               ccode->sexp
               udecl->sexp))

;; --- last line ---
