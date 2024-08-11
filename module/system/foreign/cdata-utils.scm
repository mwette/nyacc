;;; system/foreign/cdata-utils.scm -

;; Copyright (C) 2024 Matthew Wette
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

(define-module (system foreign cdata-utils)
  #:export (marshall-ctype-X)

  #:use-module (system foreign arch-info)
  #:use-module (system foreign cdata))

;; @deffn {Procedure} marshall-type ct-decl src dst
;; to be documented
;; @end deffn
(define* (marshall-ctype ct-decl src dst
                         #:optional
                         (port (current-output-port))
                         #:key
                         (per-line-prefix ""))
  (define (sf fmt . args)
    (apply simple-format port (string-append per-line-prefix fmt) args))
  (define (copy n dix six)
    (let loop ((ix 0))
      (when (< ix n)
        (sf "dst[~a] = src[~a];\n" (+ dix ix) (+ six ix))
        (loop (1+ ix)))))
  (define (swap n dix six)
    (let loop ((ix 0))
      (when (< ix n)
        (sf "dst[~a] = src[~a];\n" (+ dix (- n ix 1)) (+ six ix))
        (loop (1+ ix)))))
  (define (roundup sz al)
    (* al (quotient (+ sz (1- al)) al)))

  (define (cnvt dct dix sct six)
    (unless (eq? (ctype-kind dct) (ctype-kind sct))
      (error "marsall failure"))
    (let ((dinf (ctype-info dct)) (sinf (ctype-info sct)))
      (case (ctype-kind dct)
        ((base)
         (unless (eq? (mtype-signed? dinf) (mtype-signed? sinf))
           (error "marshall: missed signedness"))
         (let ((dsz (sizeof-mtype dinf)) (ssz (sizeof-mtype dinf)))
           (if (equal? (mtype-endianness dinf) (mtype-endianness sinf))
               (copy (min dsz ssz) dix six)
               (swap (min dsz ssz) dix six))))
        ((struct)
         (for-each
          (lambda (dfld sfld)
            (if (eq? 'bitfield (ctype-kind (cfield-type sfld)))
                (error "marslall: work to go: bitfields")
                (cnvt (cfield-type dfld) (+ dix (cfield-offset dfld))
                      (cfield-type sfld) (+ six (cfield-offset sfld)))))
          (cstruct-fields dinf) (cstruct-fields sinf)))
        ((union)
         (error "marshall: can't do unions"))
        ((pointer)
         (error "marshall: can't do unions"))
        ((array)
         (let* ((dat (carray-type dinf)) (sat (carray-type sinf))
                (dsz (roundup (ctype-size dat) (ctype-align dat)))
                (ssz (roundup (ctype-size sat) (ctype-align sat)))
                (nel (carray-length dinf)))
           (let loop ((ix 0))
             (when (< ix nel)
               (cnvt dat (+ dix (* ix dsz)) sat (+ six (* ix ssz)))
               (loop (1+ ix))))))
        (else (error "marshall: needs work" (ctype-kind dct))))))
  (let ((dct (eval `(with-arch ,dst ,ct-decl) (current-module)))
        (sct (eval `(with-arch ,src ,ct-decl) (current-module))))
    (cnvt dct 0 sct 0)))
(define marshall-ctype-X marshall-ctype)

;; --- last line ---
