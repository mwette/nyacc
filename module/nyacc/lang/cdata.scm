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
  #:use-module ((srfi srfi-1) #:select (fold))
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
  (%make-ctype size align class info)
  ctype?
  (size ctype-size)                ; size in bytes
  (align ctype-align)              ; alignment in bytes
  (class ctype-class)              ; type class (base aggr array bits)
  (info ctype-info))                ; class-specific info

;; @deftp {Record} <cdata> bv ix ct [tn]
;; @end deftp
(define-record-type <cdata>
  (%make-cdata bv ix ct tn)
  cdata?
  (bv cdata-bv)                         ; bvec
  (ix cdata-ix)                         ; index
  (ct cdata-ct)                         ; type
  (tn cdata-tn))                        ; optional (interned) type name

;; @deftp {Record} <cbitfield> type shift width sext
;; @end deftp
(define-record-type <cbitfield>
  (%make-cbitfield type shift width sext)
  cbitfield?
  (type cbitfield-type)
  (shift cbitfield-shift)
  (width cbitfield-width)
  (sext cbitfield-sext))

;; @deftp {Record} <cfield> name type offset
;; @end deftp
(define-record-type <cfield>
  (%make-cfield name type offset)
  cfield?
  (name cfield-name)
  (type cfield-type)
  (offset cfield-offset))

#|
;; @deftp {Record} <cstruct> fields
;; This contains a list of fields with name, type and offset (in the struct)
;; as well as a dictinoary to map name (including names in anonymous structs
;; and unions) to fields.  Note that for anonymous structs and unions, ...
;; @end deftp
(define-record-type <cstruct>
  (%make-cstruct fields)
  cstruct?
  (fields cstruct-fields)            ; list of fields
  (dict cstruct-dict))               ; reified dict => (type . offset)

;; @deftp {Record} <cunion> fields
;; @end deftp
(define-record-type <cunion>
  (%make-cunion fields)
  cunion?
  (fields cunion-fields)             ; alist name => type
  (dict cunion-dict))                ; reified dict => (type . offset)
|#

;; @deftp {Record} <caggate> fields dict
;; aggregate: struct or union
;; @end deftp
(define-record-type <caggate>
  (%make-caggate fields dict)
  caggate?
  (fields caggate-fields)             ; alist name => type
  (dict caggate-dict))                ; reified dict => (type . offset)

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

(set-record-type-printer! <ctype>
  (lambda (type port)
    (let ((sz (ctype-size type)) (al (ctype-align type))
          (cl (ctype-class type)) (nf (ctype-info type)))
      (if (eq? 'base cl)
          (simple-format port "#<ctype ~a>" nf)
          (simple-format port "#<ctype ~a>" cl)))))

(define make-ctype %make-ctype)

(define* (make-cdata bv ix ct #:optional (tn ""))
  (%make-cdata bv ix ct tn))

(define be (endianness big))
(define le (endianness little))

(define (cdata-detag bv ix ct tag)
  (let ((ti (ctype-info ct)))
    (case (ctype-class ct)
      ((struct union)
       (let ((fld (assq-ref (caggate-dict ti) tag)))
         (values bv (+ ix (cfield-offset fld)) (cfield-type fld))))
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


(define (make-cbase name)
  (let* ((mtype (mtypeof-basetype name))
         (size (sizeof-basetype name))
         (align (alignof-basetype name)))
    (%make-ctype size align 'base mtype)))

(define (cpointer type)
  (%make-ctype (sizeof-basetype 'void*) (alignof-basetype 'void*) 'pointer type))

;; @deffn {Procedure} make-cbase-map arch
;; where @var{arch} is string or @code{<arch>}
;; @end deffn
(define (make-cbase-map arch) ;; => ((double . (cbase "double")) ...)
  (with-arch arch
    (map (lambda (name) (cons name (make-cbase name)))
         base-type-symbol-list)))
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



(define (add-fields fields offset dict)
  (define (cfield/moved-offset field extra)
    (%make-cfield (cfield-name field) (cfield-type field)
                  (+ extra (cfield-offset field))))
  (fold (lambda (field seed)
          (acons (cfield-name field) (cfield/moved-offset field offset) seed))
        dict fields))

;; @deffn {Procedure} cstruct fields [packed] => ctype
;; fields is a list with entries @code{(name type)}
;; @end deffn
(define* (cstruct fields #:optional packed?)
  ;; cases
  ;; bitfield
  ;; 1) non-bitfield, no name => transferred and reified
  ;; 2) non-bitfield, w/ name => transferred
  ;; 3) bitfield, w/ name, positive size => transferred
  ;; 4) bitfield, no name, zero size => round-up, not transferred
  ;; 5) bitfield, no name, positive size => padding, not transferred
  ;; cases 4&5 can be combined easily, I think
  (let loop ((cfl '()) (ral '()) (ssz 0) (sal 0) (sfl fields))
    ;; cfl: c field list; ral: reified a-list; ssz: struct size;
    ;; sal:struct alignment; sfl: scheme fields
    (if (null? sfl)
        (%make-ctype ssz sal 'struct (%make-caggate (reverse cfl) (reverse ral)))
        (match (car sfl)
          ((name type)                  ; non-bitfield
           (let* ((fsz (ctype-size type))
                  (fal (ctype-align type))
                  (ssz (quotient (+ (* 8 ssz) 7) 8))
                  (ssz (incr-bit-size 0 fal ssz))
                  (cf (%make-cfield name type ssz)))
             ;;(assert (or name (memq (ctype-class type) '(struct union))))
             (if name
                 (loop (cons cf cfl) (acons name cf ral)
                       (incr-size fsz fal ssz) (max fal sal) (cdr sfl))
                 (loop (cons cf cfl)
                       (add-fields (caggate-fields (ctype-info type)) ssz ral)
                       (incr-size fsz fal ssz) (max fal sal) (cdr sfl)))))
          ((name type width)            ; bitfield
           (let* ((fsz (ctype-size type))
                  (fal (ctype-align type))
                  (sx? (cbase-signed? (ctype-info type)))
                  (ssz (incr-bit-size width fal ssz))
                  (cio (bf-offset width fal ssz)) ; ci offset in struct
                  (bfo (- (* 8 ssz) width (* 8 cio)))) ; bf offset in ci
             (if name
                 (let* ((bf (%make-cbitfield type bfo width sx?))
                        (ty (%make-ctype fsz fal 'bitfield bf))
                        (cf (%make-cfield name ty cio)))
                   (loop (cons cf cfl) (acons name cf ral)
                         ssz (max fal sal) (cdr sfl)))
                 (loop cfl ral ssz sal (cdr sfl)))))
          (otherwize
           (error "yuck"))))))

(define* (pretty-print-caggate caggate #:optional port)
  (let* ((port (or port (current-output-port)))
         (fields (caggate-fields caggate))
         (format (lambda (fmt . args) (apply simple-format port fmt args))))
    (format "cstruct/union:\n")
    (for-each
     (lambda (field)
       (format "  ~s ~s\n" (cfield-name field) (cfield-type field)))
     (caggate-fields caggate))))
(export pretty-print-caggate)


;; --- ffi support -------------------------------------------------------------

(use-modules ((system foreign) #:prefix ffi:))

(define (mtype->ffi-type mtype)
  (or
   (assq-ref
    `((s8 . ,ffi:int8) (s16 . ,ffi:int16) (s32 . ,ffi:int32) (s64 . ,ffi:int64)
      (i128 . #f) (u8 . ,ffi:uint8) (u16 . ,ffi:uint16) (u32 . ,ffi:uint32)
      (u64 . ,ffi:uint64) (u128 . #f) (f16 . #f) (f32 . ,ffi:float)
      (f64 . ,ffi:double) (f128 . #f) (s8le . ,ffi:int8) (s16le . ,ffi:int16)
      (s32le . ,ffi:int32) (s64le . ,ffi:int64) (i128le . #f) (u8le . ,ffi:uint8)
      (u16le . ,ffi:uint16) (u32le . ,ffi:uint32) (u64le . ,ffi:uint64)
      (u128le . #f) (f16le . #f) (f32le . ,ffi:float) (f64le . ,ffi:double)
      (f128le . #f) (s8be . ,ffi:int8) (s16be . ,ffi:int16) (s32be . ,ffi:int32)
      (s64be . ,ffi:int64) (i128be . #f) (u8be . ,ffi:uint8)
      (u16be . ,ffi:uint16) (u32be . ,ffi:uint32) (u64be . ,ffi:uint64)
      (u128be . #f) (f16be . #f) (f32be . ,ffi:float) (f64be . ,ffi:double)
      (f128be . #f))
    mtype)
   (error "mtype->ffi-type: bad mtype")))
(define (mtype->ffi-type-name mtype)
  (or
   (assq-ref
    `((s8 . ffi:int8) (s16 . ffi:int16) (s32 . ffi:int32) (s64 . ffi:int64)
      (i128 . #f) (u8 . ffi:uint8) (u16 . ffi:uint16) (u32 . ffi:uint32)
      (u64 . ffi:uint64) (u128 . #f) (f16 . #f) (f32 . ffi:float)
      (f64 . ffi:double) (f128 . #f) (s8le . ffi:int8) (s16le . ffi:int16)
      (s32le . ffi:int32) (s64le . ffi:int64) (i128le . #f) (u8le . ffi:uint8)
      (u16le . ffi:uint16) (u32le . ffi:uint32) (u64le . ffi:uint64)
      (u128le . #f) (f16le . #f) (f32le . ffi:float) (f64le . ffi:double)
      (f128le . #f) (s8be . ffi:int8) (s16be . ffi:int16) (s32be . ffi:int32)
      (s64be . ffi:int64) (i128be . #f) (u8be . ffi:uint8)
      (u16be . ffi:uint16) (u32be . ffi:uint32) (u64be . ffi:uint64)
      (u128be . #f) (f16be . #f) (f32be . ffi:float) (f64be . ffi:double)
      (f128be . #f))
    mtype)
   (error "mtype->ffi-type: bad mtype")))
(export mtype->ffi-type-name)

(define (cstruct->ffi-struct struct)
  ;; making this ok for bitfields will be a little involved
  (map
   (lambda (field)
     (let* ((name (cfield-name field))
            (type (cfield-type field))
            (class (ctype-class type))
            (info (ctype-info type)))
       (case (ctype-class type)
         ((base) (mtype->ffi-type info))
         ((struct) (cstruct->ffi-struct info))
         ((union) (cunion->ffi-struct info))
         (else (error "not yet supported")))))
   (caggate-fields (ctype-info struct))))

(define (cunion->ffi-struct union)
  (error "cunion not yet supported"))

(export mtype->ffi-type cstruct->ffi-struct cunion->ffi-struct)

;; --- c99 support -------------------------------------------------------------

(use-modules (nyacc lang c99 pprint))

(define (mtype->c-type mtype)
  (or
   (assq-ref
    `((s8 . "int8_t") (s16 . "int16_t") (s32 . "int32_t") (s64 . "int64_t")
      (i128 . "int128_t") (u8 . "uint8_t") (u16 . "uint16_t") (u32 . "uint32_t")
      (u64 . "uint64_t") (u128 . "uint128_t") (f16 . "float16") (f32 . "float")
      (f64 . "double") (f128 . "long double") (s8le . "int8_t")
      (s16le . "int16_t") (s32le . "int32_t") (s64le . "int64_t")
      (i128le . "int128_t") (u8le . "uint8_t") (u16le . "uint16_t")
      (u32le . "uint32_t") (u64le . "uint64_t") (u128le . "uint128_t")
      (f16le . "float16") (f32le . "float") (f64le . "double")
      (f128le . "long double") (s8be . "int8_t") (s16be . "int16_t")
      (s32be . "int32_t") (s64be . "int64_t") (i128be . #f) (u8be . "uint8_t")
      (u16be . "uint16_t") (u32be . "uint32_t") (u64be . "uint64_t")
      (u128be . "int128_t") (f16be . "float16") (f32be . "float")
      (f64be . "double") (f128be . "long double"))
    mtype)
   (error "mtype->c-type: bad mtype")))

(define (cstruct->c-struct struct)
  `(struct-def
    (field-list
     ,@(map
        (lambda (field)
          (let* ((name (cfield-name field))
                 (type (cfield-type field))
                 (class (ctype-class type))
                 (info (ctype-info type)))
            (case (ctype-class type)
              ((base)
               (if name
                   `(comp-decl
                     (decl-spec-list
                      (type-spec (typename ,(mtype->c-type info))))
                     (comp-declr-list
                      (comp-declr (ident ,(symbol->string name)))))
                   `(comp-decl
                     (decl-spec-list
                      (type-spec (typename ,(mtype->c-type info)))))))
              ((struct) #f)
              ((union) #f)
              (else (error "not yet supported")))))
        (caggate-fields (ctype-info struct))))))

(export mtype->c-type cstruct->c-struct)

;; --- last line ---
