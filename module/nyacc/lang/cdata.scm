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
            cbase cstruct
            ctype? ctype-size ctype-align ctype-class ctype-info
            )
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold-right))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (rnrs bytevectors)
  #:use-module (nyacc lang arch-info))

(use-modules (ice-9 pretty-print))
(define (pperr exp) (pretty-print exp (current-error-port)))
(define (sferr fmt . args) (apply simple-format #t fmt args))

;; if selector then aggregate else base
;; if base then meta is basetype

;; cdata bv ix ct

;;   (cdata-ref obj . tags) => <ref> == <cdata bv ix ct>
;;   (cdata-val obj) => <val> | #f
;;   (cdata-set! <ref> <val>)
;;   (cdata-detag bv ix ct tag) => (values bv bv ct)

;; (make-Foo* val) => <cdata bv=0xdead ix=0 ct=0xbeef tn="Foo*">

;; @deftp {Record} <ctype> size align class info [ptype]
;; maybe @var{ptype} should only be for @code{cbase} class types
;; @end deftp
(define-record-type <ctype>
  (%make-ctype size align class info ptype)
  ctype?
  (size ctype-size)                ; size in bytes
  (align ctype-align)              ; alignment in bytes
  (class ctype-class)              ; type class (base aggr array bits)
  (info ctype-info)                ; class-specific info
  (ptype ctype-ptr-type))          ; name of pointer-to

;; @deftp {Record} <cdata> bv ix ct [tn]
;; @end deftp
(define-record-type <cdata>
  (%make-cdata bv ix ct tn)
  cdata?
  (bv cdata-bv)                         ; bvec
  (ix cdata-ix)                         ; index
  (ct cdata-ct)                         ; type
  (tn cdata-tn))                        ; optional (interned) type name

;; @deftp {Record} <cbitfield> type mask sext shift
;; @end deftp
(define-record-type <cbitfield>
  (%make-cbitfield type offset width sext)
  cbitfield?
  (type cbitfield-type)
  (offset cbitfield-offset)             ; offset
  (width cbitfield-width)               ; width
  (sext cbitfield-sext))                ; sign-extend?

;; @deftp {Record} <cfield> type offset
;; @end deftp
(define-record-type <cfield>
  (%make-cfield name type offset)
  cfield?
  (name cfield-name)
  (type cfield-type)
  (offset cfield-offset))

;; @deftp {Record} <cstruct> fields
;; This contains a list of fields with name, type and offset (in the struct)
;; as well as a dictinoary to map name (including names in anonymous structs
;; and unions) to fields.  Note that for anonymous structs and unions, ...
;; @end deftp
(define-record-type <cstruct>
  (%make-cstruct fields)
  cstruct?
  (fields cstruct-fields)              ; list of fields
  (dict cstruct-dict))                 ; reified dict => (type . offset)

;; @deftp {Record} <cunion> fields
;; @end deftp
(define-record-type <cunion>
  (%make-cunion fields)
  cunion?
  (fields cunion-fields))               ; alist name => type

;; @deftp {Record} <carray> type length
;; @end deftp
(define-record-type <carray>
  (%make-carray type length)
  carray?
  (type carray-type)
  (length carray-length))

;; @deftp {Record} <cpointer> type
;; NOT USED (YET)
;; @end deftp
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

(define (cbase-signed? base-info)
  (and
   (member base-info '(s8 s16 s32 s64 s16 s32le s64le s16be s32be s64be))
   #t))
(export cbase-signed?)

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

(define (cpointer type)
  (%make-ctype (sizeof-basetype 'void*) (alignof-basetype 'void*)
               'pointer type #f))

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
                (ptr-type (cpointer type)))
           (cons* (cons name type) (cons ptr-name ptr-type) seed)))
       (list (cons 'void void) (cons 'void* (%make-cpointer void)))
       base-type-name-list))))
(export make-cbase-map)

(define (cbase symname)
  (let ((arch (*arch*)))
    (unless (arch-ctype-map arch)
      (set-arch-ctype-map! arch (make-cbase-map arch)))
    (assq-ref (arch-ctype-map arch) symname)))


#|
(define (bfud exp mtail size align) ; bit-field update
  (call-with-values (lambda () (sizeof-mtail mtail udict))
    (lambda (elt-sz elt-al)
      (let* ((bits (eval-c99-cx exp udict))
             (size (incr-bit-size bits elt-al size)))
        (values size (max elt-al align))))))
|#

;; special case: to be confirmed
(define (cbitfield type width)
  (cons type width))

;; Given field size and alignment, update the running struct size.
(define (incr-size fs fa ss)
  (+ fs (* fa (quotient (+ ss (1- fa)) fa))))

;; Round number of bits to next alignment size.
(define (roundup-bits a s)
  (* a (quotient (+ s (1- a)) a)))

;; Given bitfield width and alignment, update running struct size, a rational
(define (incr-bit-size w a s)
  (let* ((a (* 8 a)) (s (* 8 s)) (ru (roundup-bits a s)))
    (/ (cond ((zero? w) ru) ((> (+ s w) ru) (+ w ru)) (else (+ w s))) 8)))

;; Given bitfield width and alignment, compute byte offset for this field.
(define (bf-offset w a s)
  (let* ((a (* 8 a)) (s (* 8 s)) (u (roundup-bits a s)))
    (/ (cond ((> (+ s w) u) u) (else (- u a))) 8)))

(define (maxi-size fs fa ss)
  (max fs ss))


;; cases
;; bitfield
;; 1) non-bitfield, no name => transferred and reified
;; 2) non-bitfield, w/ name => transferred
;; 3) bitfield, w/ name, positive size => transferred
;; 4) bitfield, no name, zero size => round-up, not transferred
;; 5) bitfield, no name, positive size => padding, not transferred
;; cases 4&5 can be combined easily, I think
(define* (cstruct fields #:key packed?)
  (let loop ((cfl '()) (ral '()) (ssz 0) (sal 0) (sfl fields))
    ;; cfl: c field list; ral: reified a-list; ssz: struct size;
    ;; sal:struct alignment; sfl: scheme fields
    (if (null? sfl) (reverse cfl)
        (match (car sfl)
          ((name type)
           (let* ((fsz (ctype-size type))
                  (fal (ctype-align type))
                  (ssz (incr-bit-size 0 fal ssz)))
             (if name
                 (let ((cf (%make-cfield name type ssz)))
                   (loop (cons cf cfl) (acons name cf ral)
                         (incr-size fsz fal ssz) (max fal sal) (cdr sfl)))
                 (let ()
                   (sferr "skipping anonymous struct/union case")
                   (loop cfl ral ssz sal (cdr sfl))))))
          ((name type width)            ; bitfield
           ;; assume type is ctype, width is non-neg integer
           (let* ((fsz (ctype-size type))
                  (fal (ctype-align type))
                  (sx? (cbase-signed? (ctype-info type)))
                  (ssz (incr-bit-size width fal ssz))
                  (cio (bf-offset width fal ssz))     ; ci offset in struct
                  (bfo (- (* 8 ssz) width (* 8 cio))) ; bitfield offset in ci
                  )
             (if name
                 (let* ((bf (%make-cbitfield type bfo width sx?))
                        (ty (%make-ctype fsz fal 'bitfield bf #f))
                        (cf (%make-cfield name ty cio)))
                   (loop (cons cf cfl) (acons name cf ral)
                         ssz (max fal sal) (cdr sfl)))
                 (let ()
                   (sferr "skipping padding bitfield")
                   (loop cfl ral ssz sal (cdr sfl))))))
          (otherwize
           (error "yuck"))))))

#|
|#

;; logtest logbit? bit-extract
;; --- last line ---
