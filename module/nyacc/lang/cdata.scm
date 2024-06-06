;;; lang/cdata.scm -

;; Copyright (C) 2023-2024 Matthew Wette
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

;;; Notes:

;;; Code:

(define-module (nyacc lang cdata)
  #:export (make-ctype
            cbase
            )
  #:use-module (rnrs bytevectors)
  #:use-module ((srfi srfi-1) #:select (fold-right))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (nyacc lang arch-info))

;; if selector then aggregate else base
;; if base then meta is basetype

;; cdata bv ix ct

;;   (cdata-ref obj . tags) => <ref> == <cdata bv ix ct>
;;   (cdata-val obj) => <val> | #f
;;   (cdata-set! <ref> <val>)
;;   (cdata-detag bv ix ct tag) => (values bv bv ct)

;; (make-Foo* val) => <cdata bv=0xdead ix=0 ct=0xbeef tn="Foo*">

(define-record-type <ctype>
  (%make-ctype size almt class info ptype)
  ctype?
  (size ctype-size)                ; size in bytes
  (almt ctype-alignment)           ; alignment in bytes
  (class ctype-class)              ; type class (base aggr array bits)
  (info ctype-info)                ; class-specific info
  (ptype ctype-ptr-type set-ctype-ptr-type!)) ; pointer-to unless pointer

(define-record-type <cdata>
  (%make-cdata bv ix ct tn)
  cdata?
  (bv cdata-bv)                         ; bvec
  (ix cdata-ix)                         ; index
  (ct cdata-ct)                         ; type
  (tn cdata-tn))                        ; optional (interned) type name

(define-record-type <cbitfield>
  (%make-cbitfield offset width)
  cbitfield?
  (offset cbitfield-offset)
  (width cbitfield-width))

(define-record-type <cfield>
  (%make-cfield offset type)
  cfield?
  (offset cfield-offset)
  (type cfield-type))

(define-record-type <cstruct>
  (%make-cstruct fields)
  cstruct?
  (fields cstruct-fields))              ; alist name => field

(define-record-type <cunion>
  (%make-cunion fields)
  cunion?
  (fields cunion-fields))               ; alist name => type

(define-record-type <carray>
  (%make-carray length type)
  carray?
  (length carray-length)
  (type carray-type))

(define-record-type <cpointer>
  (%make-cpointer type)
  cpointer?
  (type cpointer-type))

(define make-ctype %make-ctype)

(define* (make-cdata bv ix ct #:optional (tn ""))
  (%make-cdata bv ix ct tn))

(define be (endianness big))
(define le (endianness little))

(define (cdata-detag bv ix ct tag)
  (let ((ti (ctype-info ct)))
    (case (ctype-class ct)
      ((struct)
       (let ((fld (assq-ref (cstruct-fields ti) tag)))
         (values bv (+ ix (cfield-offset fld)) (cfield-type fld))))
      ((union)
       (let ((ftype (assq-ref (cunion-fields ti) tag)))
         (values bv ix ftype)))
      ((array)
       (unless (integer? tag) (error "bad array ref"))
       (let ((type (carray-type ti)))
         (values bv (+ ix (* tag (ctype-size type))) type)))
      ((bitfield)
       (error "tbd")))))

(define (cdata-ref data . tags)
  (let ((bv (cdata-bv data)))
    (let loop ((ix (cdata-ix data)) (ct (cdata-ct data)) (tags tags))
      (if (null? tags)
          (make-cdata bv ix ct)
          (call-with-values (lambda () (cdata-detag bv ix ct (car tags)))
            (lambda (bv ix ct) (loop ix ct (cdr tags))))))))

(define (cdata-val data)
  (let ((bv (cdata-bv data))
        (ix (cdata-ix data))
        (ct (cdata-ct data)))
    (case (ctype-class ct)
      ((base)
       (case (ctype-info ct)
         ((u8) (bytevector-u8-ref bv ix))
         ((s8) (bytevector-s8-ref bv ix))
         ((u16le) (bytevector-u16-ref bv ix le))
         ((s16le) (bytevector-s16-ref bv ix le))
         ((u32le) (bytevector-u32-ref bv ix le))
         ((s32le) (bytevector-s32-ref bv ix le))
         ((u64le) (bytevector-u64-ref bv ix le))
         ((s64le) (bytevector-s64-ref bv ix le))
         ((f32le) (bytevector-ieee-single-ref bv ix le))
         ((f64le) (bytevector-ieee-double-ref bv ix le))
         ((u16be) (bytevector-u16-ref bv ix be))
         ((s16be) (bytevector-s16-ref bv ix be))
         ((u32be) (bytevector-u32-ref bv ix be))
         ((s32be) (bytevector-s32-ref bv ix be))
         ((u64be) (bytevector-u64-ref bv ix be))
         ((s64be) (bytevector-s64-ref bv ix be))
         ((f32be) (bytevector-ieee-single-ref bv ix be))
         ((f64be) (bytevector-ieee-double-ref bv ix be))))
      ((struct) #f)
      ((union) #f)
      ((bitfield) #f)
      ((array) #f)
      (else
       (error "bad stuff")))))

(define (ctype-equal? a b)
  #f)


(define* (make-cstruct flds #:key packed?)
  (let loop ((cfl '()) (sfl flds))
    (if (null? sfl) (reverse cfl)
        (let ()
          (loop (cons (car sfl) cfl) (cdr sfl))))))


(define (make-cbase name ptr-name)
  (let* ((name (if (string? name) (strname->symname name) name))
         (size (sizeof-basetype name))
         (align (alignof-basetype name)))
    (%make-ctype size align 'base name ptr-name)))

;; @deffn {Procedure} make-cbase-map arch
;; where @var{arch} is string or @code{<arch>}
;; @end deffn
(define (make-cbase-map arch) ;; => ((double . (cbase "double")) ...)
  (with-arch arch
    (let* ((void (%make-ctype 0 0 'base 'void #f))
           (psize (sizeof-basetype "void*"))
           (palgn (alignof-basetype "void*")))
      (fold-right
       (lambda (cname seed)
         (let* ((name (strname->symname cname))
                (ptr-name (strname->symname (string-append cname "*")))
                (type (make-cbase name ptr-name))
                (ptr-type (make-ctype (%make-cpointer type)))
           (cons* (cons name type) (cons ptr-name ptr-type) seed)))
       (list (cons 'void void) (cons 'void* (%make-cpointer void)))
       base-type-name-list))))
(export make-cbase-map)

(define (cbase symname)
  (let ((arch (*arch*)))
    (unless (arch-ctype-map arch)
      (set-arch-ctype-map! arch (make-cbase-map arch)))
    (assq-ref (arch-ctype-map arch) symname)))

(define* (cstruct fields #:key packed?)
  (let loop ((cfl '()) (size 0) (align 0) (sfl flds))
    (if (null? sfl) (reverse cfl)
        (let* ((name (caar sfl))
               (type (caar sfl))
               (tsize (ctype-size type))
               (talign (ctype-align type))
               (foo 0)
               )
          (loop (cons (car sfl) cfl) (cdr sfl))))))

#|
|#
;; --- last line ---
