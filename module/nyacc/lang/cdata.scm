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
  #:export (cbase
            cstruct cunion cpointer
            cdata-val cdata-ref cdata-set!
            make-ctype ctype? ctype-size ctype-align ctype-class ctype-info
            make-cdata cdata? cdata-bv cdata-ix cdata-ct cdata-tn
            ;; debug
            cstruct-fields cstruct-dict
            cunion-fields cunion-dict
            cfield-name cfield-type cfield-offset
            cbitfield-mtype cbitfield-shift cbitfield-width cbitfield-sext?
            )
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (rnrs bytevectors)
  #:use-module (nyacc lang arch-info))

(use-modules (ice-9 pretty-print))
(define (pperr exp) (pretty-print exp (current-error-port)))
(define (sferr fmt . args) (apply simple-format #t fmt args))

;; => arch-info
(define (mtype-bv-ref mtype bv ix)
  (case mtype
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

;; => arch-info
(define (mtype-bv-set! mtype bv ix value)
  (case mtype
    ((u8) (bytevector-u8-set! bv ix value))
    ((s8) (bytevector-s8-set! bv ix value))
    ((u16le) (bytevector-u16-set! bv ix value le))
    ((s16le) (bytevector-s16-set! bv ix value le))
    ((u32le) (bytevector-u32-set! bv ix value le))
    ((s32le) (bytevector-s32-set! bv ix value le))
    ((u64le) (bytevector-u64-set! bv ix value le))
    ((s64le) (bytevector-s64-set! bv ix value le))
    ((f32le) (bytevector-ieee-single-set! bv ix value le))
    ((f64le) (bytevector-ieee-double-set! bv ix value le))
    ((u16be) (bytevector-u16-set! bv ix value be))
    ((s16be) (bytevector-s16-set! bv ix value be))
    ((u32be) (bytevector-u32-set! bv ix value be))
    ((s32be) (bytevector-s32-set! bv ix value be))
    ((u64be) (bytevector-u64-set! bv ix value be))
    ((s64be) (bytevector-s64-set! bv ix value be))
    ((f32be) (bytevector-ieee-single-set! bv ix value be))
    ((f64be) (bytevector-ieee-double-set! bv ix value be))))

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

;; @deftp {Record} <cbitfield> type shift width sext?
;; @end deftp
(define-record-type <cbitfield>
  (%make-cbitfield mtype shift width sext?)
  cbitfield?
  (mtype cbitfield-mtype)
  (shift cbitfield-shift)
  (width cbitfield-width)
  (sext? cbitfield-sext?))

;; @deftp {Record} <cfield> name type offset
;; @end deftp
(define-record-type <cfield>
  (%make-cfield name type offset)
  cfield?
  (name cfield-name)
  (type cfield-type)
  (offset cfield-offset))

;; @deftp {Record} <cstruct> fields dict
;; struct
;; @end deftp
(define-record-type <cstruct>
  (%make-cstruct fields dict)
  cstruct?
  (fields cstruct-fields)             ; list of fields (change to vector)
  (dict cstruct-dict))                ; dict: name => field

;; @deftp {Record} <cunion> fields dict
;; union
;; @end deftp
(define-record-type <cunion>
  (%make-cunion fields dict)
  cunion?
  (fields cunion-fields)           ; list of fields (change to vector)
  (dict cunion-dict))              ; dict: name => field

;; @deftp {Record} <carray> type length
;; XXX
;; @end deftp
(define-record-type <carray>
  (%make-carray type length)
  carray?
  (type carray-type)
  (length carray-length))

;; @deftp {Record} <cpointer> type
;; Once we get to this level, we shouldn't need @code{arch} anymore
;; so we need to log the pointer type
;; @end deftp
(define-record-type <cpointer>
  (%make-cpointer type mtype)
  cpointer?
  (type cpointer-type)
  (mtype cpointer-mtype))

;; @deftp {Record} <cfunction> return-type param-types
;; @end deftp

(set-record-type-printer! <ctype>
  (lambda (type port)
    (let ((cl (ctype-class type)) (nf (ctype-info type))
          (addr (ffi:pointer-address (ffi:scm->pointer type))))
      (format port "#<ctype ~s 0x~x>" (if (eq? 'base cl) nf cl) addr))))

(define make-ctype %make-ctype)

;; @deftp {Record} <cdata> bv ix ct [tn]
;; @end deftp
(define-record-type <cdata>
  (%make-cdata bv ix ct tn)
  cdata?
  (bv cdata-bv)                         ; bvec
  (ix cdata-ix)                         ; index
  (ct cdata-ct)                         ; type
  (tn cdata-tn))                        ; optional (interned) type name

(set-record-type-printer! <cdata>
  (lambda (data port)
    (let* ((bv (cdata-bv data))
           (ix (cdata-ix data))
           (type (cdata-ct data))
           (tn (cdata-tn data))
           (cl (ctype-class type))
           (nf (ctype-info type))
           (addr (ffi:pointer-address (ffi:bytevector->pointer bv)))
           )
      (format port "#<cdata 0x~x" addr)
      (format port " ~a>" cl))))

(define be (endianness big))
(define le (endianness little))

;; move to arch-info?
(define (mtype-signed? mtype)
  (and (member mtype '(s8 s16 s32 s64 s16le s32le s64le s16be s32be s64be)) #t))
(export mtype-signed?)


(define (ctype-detag ix ct tag)
  (let ((ti (ctype-info ct)))
    (case (ctype-class ct)
      ((struct)
       (let ((fld (assq-ref (cstruct-dict ti) tag)))
         (values (+ ix (cfield-offset fld)) (cfield-type fld))))
      ((union)
       (let ((fld (assq-ref (cunion-dict ti) tag)))
         (values (+ ix (cfield-offset fld)) (cfield-type fld))))
      ((array)
       (unless (integer? tag) (error "bad array ref"))
       (let ((type (carray-type ti)))
         (values (+ ix (* tag (ctype-size type))) type)))
      ((bitfield)
       (error "tbd")))))

(define (follow-tags ix ct tags)
  (let loop ((ix ix) (ct ct) (tags tags))
    (if (null? tags)
        (values ix ct)
        (call-with-values (lambda () (ctype-detag ix ct (car tags)))
          (lambda (ix ct) (loop ix ct (cdr tags)))))))

(define (cdata-ref data . tags)
  (if (null? tags)
      data
      (call-with-values
          (lambda () (follow-tags (cdata-ix data) (cdata-ct data) tags))
        (lambda (ix ct)
          (%make-cdata (cdata-bv data) ix ct #f)))))

(define (sign-bitfield v w)
  (let ((smask (expt 2 (1- w))))
    (if (logand v smask) (- (1+ (logand v (1- smask)))) v)))

(define (cdata-val data . tags)
  (call-with-values
      (lambda () (follow-tags (cdata-ix data) (cdata-ct data) tags))
    (lambda (ix ct)
      (let ((bv (cdata-bv data)))
        (case (ctype-class ct)
          ((base)
           (mtype-bv-ref (ctype-info ct) bv ix))
          ((pointer)
           (ffi:make-pointer
            (mtype-bv-ref (cpointer-mtype (ctype-info ct)) bv ix)))
          ((bitfield)
           (let* ((bi (ctype-info ct))
                  (mt (cbitfield-mtype bi)) (sh (cbitfield-shift bi))
                  (wd (cbitfield-width bi)) (sx (cbitfield-sext? bi))
                  (sm (expt 2 (1- wd))) ; sign-bit mask
                  (v (bit-extract (mtype-bv-ref mt bv ix) sh (+ sh wd))))
             ;;(sferr "bv wd=~s sm=~s v=~s\n" wd sm v)
             ;;(format (current-error-port) "  v ~s\n" v)
             ;;(format (current-error-port) "  v ~b\n" v)
             ;;(sferr "  logbit=~s ~s\n" (1- wd) (logbit? (1- wd) v))
             (if (and sx (logbit? (1- wd) v)) (- (logand v (1- sm)) sm) v)))
          ((struct) #f)
          ((union) #f)
          ((array) #f)
          (else (error "bad stuff")))))))

(define (cdata-set! data value)
  (let ((bv (cdata-bv data)) (ix (cdata-ix data)) (ct (cdata-ct data)))
    (case (ctype-class ct)
      ((base) (mtype-bv-set! (ctype-info ct) bv ix value))
      ((pointer) (mtype-bv-set! (cpointer-mtype (ctype-info ct)) bv ix value))
      ((bitfield) #f)
      ((struct) #f)
      ((union) #f)
      ((array) #f)
      (else
       (error "bad stuff")))))

(define (ctype-equal? a b)
  #f)


(define* (make-cdata type #:optional value #:key name)
  (let ((data (%make-cdata (make-bytevector (ctype-size type)) 0 type name)))
    (if value (cdata-set! data value))
    data))

(define (cpointer type)
  (%make-ctype (sizeof-basetype 'void*) (alignof-basetype 'void*)
               'pointer (%make-cpointer type (mtypeof-basetype 'void*))))

;; This is more than a bent pipe.  It allocates storage.
(define (pointer-to data)
  (let* ((bv (cdata-bv data)) (ix (cdata-ix data)) (ct (cdata-ct data))
         (pa (+ (ffi:pointer-address (ffi:bytevector->pointer bv)) ix))
         )
    (make-cdata (cpointer ct) pa)))

(export pointer-to)

;; @deffn {Procedure} make-cbase-map arch
;; where @var{arch} is string or @code{<arch>}
;; @end deffn
(define (make-cbase-map arch) ;; => ((double . (cbase "double")) ...)
  (define (make-cbase name)
    (let* ((mtype (mtypeof-basetype name))
           (size (sizeof-basetype name))
           (align (alignof-basetype name)))
      (%make-ctype size align 'base mtype)))
  (with-arch arch
    (map (lambda (name) (cons name (make-cbase name)))
         base-type-symbol-list)))
(export make-cbase-map)

(define (cbase symname)
  (let ((arch (*arch*)))
    (unless (arch-ctype-map arch)
      (set-arch-ctype-map! arch (make-cbase-map arch)))
    (assq-ref (arch-ctype-map arch) symname)))

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
;; fields is a list with entries @code{(name type)} where @code{type} is
;; a @code{<ctype>} object or a symbol for a base type.
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
    ;; cfl: C field list; ral: reified a-list; ssz: struct size;
    ;; sal:struct alignment; sfl: scheme fields
    (if (null? sfl)
        ;; TODO: change field-list to vector
        (%make-ctype (incr-bit-size 0 sal ssz) sal 'struct
                     (%make-cstruct (reverse cfl) (reverse ral)))
        (match (car sfl)
          ((name type)                  ; non-bitfield
           (let* ((type (cond ((symbol? type) (cbase type)) (else type)))
                  (fsz (ctype-size type))
                  (fal (ctype-align type))
                  (ssz (quotient (+ (* 8 ssz) 7) 8))
                  (ssz (incr-bit-size 0 fal ssz))
                  (cf (%make-cfield name type ssz)))
             ;;(assert (or name (memq (ctype-class type) '(struct union))))
             (if name
                 (loop (cons cf cfl) (acons name cf ral)
                       (incr-size fsz fal ssz) (max fal sal) (cdr sfl))
                 (loop (cons cf cfl)
                       (add-fields (cstruct-fields (ctype-info type)) ssz ral)
                       (incr-size fsz fal ssz) (max fal sal) (cdr sfl)))))
          ((name type width)            ; bitfield
           (unless (eq? (ctype-class type) 'base) (error "bad type"))
           (let* ((type (cond ((symbol? type) (cbase type)) (else type)))
                  (fsz (ctype-size type))
                  (fal (ctype-align type))
                  (mty (ctype-info type))
                  (sx? (mtype-signed? mty))
                  ;; order is critical here:
                  (cio (bf-offset width fal ssz)) ; ci struct offset
                  (ssz (incr-bit-size width fal ssz))  ; moved
                  (bfo (- (* 8 ssz) width (* 8 cio)))) ; offset wrt ci
             (if name
                 (let* ((bf (%make-cbitfield mty bfo width sx?))
                        (ty (%make-ctype fsz fal 'bitfield bf))
                        (cf (%make-cfield name ty cio)))
                   (loop (cons cf cfl) (acons name cf ral)
                         ssz (max fal sal) (cdr sfl)))
                 (loop cfl ral ssz sal (cdr sfl)))))
          (otherwize
           (sferr "cstruct bad form: ~s" (car sfl))
           (error "yuck"))))))



(define* (pretty-print-ctype ctype #:optional (port (current-output-port)))
  (define (fmt . args) (apply simple-format port fmt args))
  (case (ctype-class ctype)
    ((struct)
     (let* ((struct (ctype-info ctype))
            (fields (cstruct-fields struct)))
       (fmt "cstruct/union:\n")
       (for-each
        (lambda (field)
          (fmt "  ~s ~s\n" (cfield-name field) (cfield-type field)))
        fields)))
    (else
     #f)))
(export pretty-print-ctype)


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
   (cstruct-fields (ctype-info struct))))

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
        (cstruct-fields (ctype-info struct))))))

(export mtype->c-type cstruct->c-struct)

;; --- last line ---
