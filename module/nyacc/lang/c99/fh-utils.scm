;;; nyacc/lang/c99/fh-utils.scm

;; Copyright (C) 2016-2025 Matthew Wette
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

(define-module (nyacc lang c99 fh-utils)
  #:export (make-fh-backend
            fhbe-name fhbe-header fhbe-trailer
            fhbe-base fhbe-array fhbe-pointer
            fhbe-struct fhbe-bitfield fhbe-union
            fhbe-function fhbe-enum
            fhbe-typedef fhbe-makeobj
            *errmsgs*
            fherr fherr/once)
  #:use-module (srfi srfi-9))

(define *errmsgs* (make-parameter '())) ; list of warnings

(define-record-type <fh-backend>
  (make-fh-backend name header trailer
                   mt-base mt-array mt-pointer mt-struct
                   mt-bitfield mt-union mt-function mt-enum
                   make-type-defn make-object)
  fhbe-impl?
  (name fhbe-name)
  (header fhbe-header)
  (trailer fhbe-trailer)
  (mt-base fhbe-base)
  (mt-array fhbe-array)
  (mt-pointer fhbe-pointer)
  (mt-struct fhbe-struct)
  (mt-bitfield fhbe-bitfield)
  (mt-union fhbe-union)
  (mt-function fhbe-function)
  (mt-enum fhbe-enum)
  (make-type-defn fhbe-typedef)
  (make-object fhbe-makeobj))

(define (fherr fmt . args)
  (apply throw 'ffi-help-error fmt args))

(define (fherr/once fmt . args)
  (let ((errmsgs (*errmsgs*)))
    (cond
     ((member fmt errmsgs)
      (apply throw 'ffi-help-error #f '()))
     (else
      (*errmsgs* (cons fmt errmsgs))
      (apply throw 'ffi-help-error fmt args)))))

;; --- last line ---
