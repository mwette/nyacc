;;; test-suite/test-suite/lib.scm
;;;
;;; Copyright (C) 2017 Matthew R. Wette
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, see <http://www.gnu.org/licenses/>

#!
(cond-expand
  (guile-2
   (load-from-path "test-suite/2.0/lib.scm")
   1)
  (guile
   (load-from-path "test-suite/1.8/lib.scm")
   1)
  (else #t))
!#
(cond-expand
  (guile-2
   (include-from-path "test-suite/2.0/lib.scm"))
  (guile
   (use-modules (nyacc compat18))
   (include-from-path "test-suite/1.8/lib.scm"))
  (else #t))
  
;; --- last line ---
