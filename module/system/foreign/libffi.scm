;;; system/foreign/libffi.scm -

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


;; needed hooks?
;;   (make-cif ret args) -> cif
;;   (make-cif-var ret fixed-args var-args) -> cif
;;   (pointer->procedure/cif pointer cif) -> procedure
;; my procs:
;;   (ctype->ffi_type ctype) -> ffi_type
;;   (map ctype->ffi_type ctypes)
;;   (make-cif/ctype ctype-ret ctype-args) -> cif
;;   (pointer->procedure/cif xxx

(use-modules (system foreign))

(define ffi_type*
  (cpointer (delay (cstruct `((size size_t)
                              (alignment unsigned-short)
                              (type unsigned-short)
                              (elements ,(cpointer ffi_type*)))))))
(define ffi_type
  (cpointer-type (ctype-info ffi_type*)))
(define ffi_type**
  (cpointer ffi_type*))

(define ffi_cif
  (cstruct
   `((abi int) ;; ffi_abi
     (nargs unsigned)
     (arg_types ,ffi_type**)
     (rtype ,ffi_type*)
     (bytes unsigned)
     (flags unsigned))))

(define libffi (load-foreign-library "libffi"))

(define (get-ft-prim mtype ptype)
  (make-cdata ffi_type* #:from-pointer (foreign-library-pointer libffi ptype)))
(export get-ft-prim)

(define mtype->ffi-prim
  (let ((ftmap `((u8 . ,(get-ft-prim 'uint8_t "ffi_type_uint8"))
                 (s8 . ,(get-ft-prim 'int8_t "ffi_type_sint8"))
                 (u16le . ,(get-ft-prim 'uint16_t "ffi_type_uint16"))
                 (s16le . ,(get-ft-prim 'sint16_t "ffi_type_sint16"))
                 (u32le . ,(get-ft-prim 'uint32_t "ffi_type_uint32"))
                 (s32le . ,(get-ft-prim 'sint32_t "ffi_type_sint32"))
                 (u64le . ,(get-ft-prim 'uint64_t "ffi_type_uint64"))
                 (s64le . ,(get-ft-prim 'sint64_t "ffi_type_sint64"))
                 (f32le . ,(get-ft-prim 'float "ffi_type_float"))
                 (f64le . ,(get-ft-prim 'double "ffi_type_double"))
                 )))
    (lambda (mtype) (assq-ref ftmap mtype))))

(define ffi-type
  (let ((ftmap '((void . 0) (int . 1)
                 (f32le . 2) (f64le . 3) (f128le . 4)
                 (u8 . 5) (s8 . 6) (u16le . 7) (s16le . 8)
                 (u32le . 9) (s32le . 10) (u64le . 11) (s64le . 12)
                 (struct . 13) (pointer . 14) (complex . 15))))
    (lambda (mtype) (assq-ref ftmap mtype))))
(export ffi-type)

;; @deffn {Procedure} ctype->ffi_type ctype => cdata  (OR bytevector)
;; @*TODO: handle bitfields
;; We should pack in a single typevector
;; @end deffn
(define (ctype->ffi_type ctype)
  (assert-ctype 'ctype->ffi_type ctype)
  (define (cnvt type)
    (let ((info (ctype-info type))
          (ffit (make-cdata ffi_type)))
      (case (ctype-kind type)
        ((base)
         (cdata-set! ffit (ffi-type (ctype-info type)) 'type))
        ((pointer)
         (cdata-set! ffit (ffi-type 'pointer) 'type))
        ((struct)
         (let* ((flds (cstruct-fields info))
                (nfld (length flds)) ; does not work for bitfields
                (fld-pt (carray ffi_type* nfld))
                (fld-pd (make-cdata fld-pt)))
           (cdata-set! ffit (ffi-type 'struct) 'type)
           (cdata-set! ffit (cdata-cast fld-pd ffi_type**) 'elements)
           (for-each
            (lambda (ix fld)
              (let ((ftype (cfield-type fld)))
                (if (eq? 'bitfield (ctype-kind ftype))
                    (error "uh uh ahhh: bitfield"))
                (cdata-set! fld-pd (cnvt (cfield-type fld)) ix)))
            (iota nfld) flds)))
        ((union array) ; just fill
         (let* ((align (ctype-align type))
                (asize (/ (ctype-size type) align))
                (fld-pt (carray ffi_type* asize))
                (fld-pd (make-cdata fld-pt))
                (etype (mtype->ffi-prim (case align
                                          ((1) 'u8) ((2) 'u16le) ((3) 'u32le)
                                          ((4) 'u32le) ((8) 'u64le)))))
           (cdata-set! ffit (ffi-type 'struct) 'type)
           (for-each (lambda (ix) (cdata-set! fld-pd etype ix)) (iota asize))))
        (else (error "ctype->ffi_type: missed something")))
      (cdata& ffit)))
  (case (ctype-kind ctype)
    ((array) (let ((ffit (make-cdata ffi_type)))
               (cdata-set! ffit (ffi-type 'pointer) 'type)))
    (else (cnvt ctype))))

(export ffi_cif ctype->ffi_type)
(export ffi_type ffi_type* ffi_type** mtype->ffi-prim)

;; call cif

;; --- last line ---
