;;; system/foreign/cdata.scm -

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

;; new notation:

;; reference set select
;; (cdata-ref data tag ...) -> scheme-value | <cdata>
;; (cdata-set! data val tag ...)
;; (cdata-sel data tag ...) -> <cdata>
;; (ctype-sel type ix tags) -> ix, <ctype>
;; (ctype-equal? a b)
;; (make-cdata ct)
;; (make-cdata ct val)
;; (make-cdata bv ix ct)
;; (make-cdata bv ix ct val)

;; use existing bytevector
;; (%make-cdata bv ix ct tn)

;; (cbase symb) and cstruct cunion carray cpointer cfunction
;; (list->vector (map (lambda (ix) (cdata-ref data ix)) (iota 10)))

;; ffi:
;; (cdata-cast cdata type) -> cdata
;; (cdata-arg type value)
;; (cpointer->procedure ret-arg args [va-args])

;;; Code:

(define-module (system foreign cdata)
  #:export (cbase
            cstruct cunion cpointer carray
            ctype? ctype-size ctype-align ctype-kind ctype-info ctype-equal?
            make-cdata cdata? cdata-bv cdata-ix cdata-ct cdata-tn
            cdata-ref cdata-set! cdata-sel ctype-sel cdata& cdata* cdata-cast
            ctype->ffi
            ;;
            mtype-bv-ref mtype-bv-set!
            ;; debug
            cstruct-fields cstruct-dict
            cunion-fields cunion-dict
            cfield-name cfield-type cfield-offset
            cbitfield-mtype cbitfield-shift cbitfield-width cbitfield-sext?
            cpointer-type cpointer-mtype
            carray-type carray-length
            cfunction-ret-type cfunction-arg-types
            pretty-print-ctype)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module ((srfi srfi-1) #:select (fold xcons))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system foreign arch-info))
(export %cpointer-type)

(use-modules (ice-9 pretty-print))
(define (pperr exp) (pretty-print exp (current-error-port)))
(define (sferr fmt . args) (apply simple-format #t fmt args))
(define (ppct ct) (pretty-print-ctype ct (current-error-port)))

;; @deftp {Record} <ctype> size align kind info [ptype]
;; maybe @var{ptype} should only be for @code{cbase} kind types
;; @end deftp
(define-record-type <ctype>
  (%make-ctype size align kind info)
  ctype?
  (size ctype-size)                ; size in bytes
  (align ctype-align)              ; alignment in bytes
  (kind ctype-kind)                ; type kind (base aggr array bits)
  (info ctype-info))               ; kind-specific info

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

;; @deftp {Record} <cenum> mtype symd vald
;; enum
;; @end deftp
(define-record-type <cenum>
  (%make-cenum mtype symd vald)
  cenum?
  (mtype cenum-mtype)                   ; underlying basic C type
  (symd cenum-sym-dict)                 ; name -> value
  (vald cenum-val-dict))                ; value-> name

;; @deftp {Record} <cpointer> type mtype
;; Once we get to this level, we shouldn't need @code{arch} anymore
;; so we need to log the pointer type
;; @end deftp
(define-record-type <cpointer>
  (%make-cpointer type mtype)
  cpointer?
  (type %cpointer-type)
  (mtype cpointer-mtype))

(define (cpointer-type info)
  (let ((type (%cpointer-type info)))
    (if (promise? type) (force type) type)))

;; @deftp {Record} <cfunction> unwrapper wrapper
;; The @var{unwrapper} argument is a procedure to convert from pointer to
;; lambda.  The @var{wrapper} takes lambda and generates a pointer.
;; @end deftp
(define-record-type <cfunction>
  (%make-cfunction unwrapper wrapper)
  cfunction?
  (unwrapper cfunction-unwrapper)
  (wrapper cfunction-wrapper))

(set-record-type-printer! <ctype>
  (lambda (type port)
    (let ((cl (ctype-kind type)) (nf (ctype-info type))
          (addr (pointer-address (scm->pointer type))))
      (format port "#<ctype ~s 0x~x>" (if (eq? 'base cl) nf cl) addr))))

(define make-ctype %make-ctype)

;; @deftp {Record} <cdata> bv ix ct [tn]
;; Record to hold C data.  Underneath it's a bytevector, index and type.
;; There is an optional type-name symbol that can be used to indicate
;; a source-language type (e.g., struct-Foo).
;; @end deftp
(define-record-type <cdata>
  (%make-cdata bv ix ct tn)
  cdata?
  (bv cdata-bv)                         ; bvec
  (ix cdata-ix)                         ; index
  (ct cdata-ct)                         ; type
  (tn cdata-tn))                        ; optional (interned) type name
(export %make-cdata) ;; needed?

(set-record-type-printer! <cdata>
  (lambda (data port)
    (let* ((bv (cdata-bv data))
           (ix (cdata-ix data))
           (type (cdata-ct data))
           (cl (ctype-kind type))
           (bv-addr (pointer-address (bytevector->pointer bv))))
      (format port "#<cdata 0x~x" (+ bv-addr ix))
      (format port " ~a>" cl))))


(define-inlinable (assert-ctype p v)
  (unless (ctype? v)
    (error (simple-format #f "~a: expected <ctype>, got ~s" p v))))

(define-inlinable (assert-cdata p v)
  (unless (cdata? v)
    (error (simple-format #f "~a: expected <cdata>, got ~s" p v))))

(define-syntax be (identifier-syntax (endianness big)))
(define-syntax le (identifier-syntax (endianness little)))

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



;; @deffn {Procedure} ctype-detag ix ct tag
;; Follows @var{tag}.  For structs and unions, the tag is a symbolic
;; field name.  For arrays and pointers, the tag is a non-negative integer.
;; An integer tag applied to the pointer increments the pointer by the
;; associated number of elements referenced.
;; @end deffn
(define (ctype-detag ix ct tag)
  ;;(assert-type 'ctype-detag ct)
  (let ((ti (ctype-info ct)))
    (case (ctype-kind ct)
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
      ((pointer)
       (unless (integer? tag) (error "bad array ref"))
       (let ((type (cpointer-type ti)))
         (values (+ ix (* tag (ctype-size type))) ct)))
      (else (error "bad tag" tag)))))

(define (ctype-sel type ix tags)
  (assert-ctype 'ctype-sel type)
  (let loop ((ix ix) (ct type) (tags tags))
    (if (null? tags)
        (values ix ct)
        (call-with-values (lambda () (ctype-detag ix ct (car tags)))
          (lambda (ix ct) (loop ix ct (cdr tags)))))))


;; @deffn {Procedure} ctype-equal? a b
;; This predicate assesses equality of it's arguments.
;; Two types are considered equal if they have the same size,
;; alignment, kind, and eqivalent kind-specific properties.
;; For base types, the symbolic mtype must be equal; this includes
;; size, integer versus float, and signed versus unsigned.
;; For struct and union kinds, the names and types of all fields
;; must be equal.
;; @*TODO: algorithm to prevent infinite search for recursive structs
;; @end deffn
(define (ctype-equal? a b)
  "- Procedure: ctype-equal? a b
     This predicate assesses equality of it’s arguments.  Two types are
     considered equal if they have the same size, alignment, kind, and
     eqivalent kind-specific properties.  For base types, the symbolic
     mtype must be equal; this includes size, integer versus float, and
     signed versus unsigned.  For struct and union kinds, the names and
     types of all fields must be equal."
  (letrec*
      ((fields-equal?
        (lambda (fl gl)
          (fold (lambda (a b seed)
                  (and seed
                       (eq? (cfield-name a) (cfield-name b))
                       (eqv? (cfield-offset a) (cfield-offset b))
                       (ctype-equal? (cfield-type a) (cfield-type b))))
                #t fl gl)))
       (cinfo-equal?
        (lambda (kind a b)
          (case kind
            ((base) (eq? a b))
            ((struct) (fields-equal? (cstruct-fields a) (cstruct-fields b)))
            ((union) (fields-equal? (cunion-fields a) (cunion-fields b)))
            ((pointer)
             (let* ((at (%cpointer-type a))
                    (bt (%cpointer-type b)))
               (cond
                ((and (promise? at) (promise? bt)) (eq? (force at) (force bt)))
                ((promise? at) (eq? (force at) bt))
                ((promise? bt) (eq? at (force bt)))
                ((eq? at bt)))))
                (else #f)))))
    (cond
     ((or (not (ctype? a)) (not (ctype? b))) #f)
     ((not (eq? (ctype-kind a) (ctype-kind b))) #f)
     ((not (eqv? (ctype-size a) (ctype-size b))) #f)
     ((not (eqv? (ctype-align a) (ctype-align b))) #f)
     (else (cinfo-equal? (ctype-kind a) (ctype-info a) (ctype-info b))))))

;; @deffn {Procedure} cpointer type
;; Generate a C pointer type to @var{type}. To reference or de-reference
;; cdata object see @code{cdata&} and @code{cdata*}.  @var{type} can be
;; the symbol @code{void} or a symbolic name used as argument to @code{cbase}.
;; @*note: Should we allow @ver{type} to be a promise?
;; @example
;; (define foo_t (cbase 'void))
;; (cpointer (delay foo_t))
;; @end example
;; @end deffn
(define (cpointer type)
  (let ((type (cond
               ((ctype? type) type)
               ((eq? 'void type) type)
               ((symbol? type) (cbase type))
               ((promise? type) type)
               (else (error "cpointer: bad arg" type)))))
    (%make-ctype (sizeof-basetype 'void*) (alignof-basetype 'void*)
                 'pointer (%make-cpointer type (mtypeof-basetype 'void*)))))

;;.@deffn {Procedure} make-cbase-map arch
;; where @var{arch} is string or @code{<arch>}
;;.@end deffn
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

(define cbase-symbols
  '(s8 u8 s16 s32 s64 i128 u16 u32 u64 u128 f16 f32 f64 f128
    s16le s32le s64le i128le u16le u32le u64le u128le
    f16le f32le f64le f128le s16be s32be s64be i128be
    u16be u32be u64be u128be f16be f32be f64be f128be))

(define (cbase name)
  (let ((arch (*arch*))
        (name (if (symbol? name) name
                  (if (string? name) (strname->symname name)
                      (error "cbase: bad arg")))))
    (unless (arch-cbase-map arch)
      (set-arch-cbase-map! arch (make-cbase-map arch)))
    (or (assq-ref (arch-cbase-map arch) name) ; make it idempotent
        (and (memq name cbase-symbols) name)
        (error "not found"))))
(display "cdata: check use of arch-cbase-map in here\n")

;; Update running struct size given field size and alignment.
(define (incr-size fs fa ss)
  (+ fs (* fa (quotient (+ ss (1- fa)) fa))))

;; Update running union size given field size and alignment.
(define (maxi-size fs fa ss)
  (max fs ss))

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
  "- Procedure: cstruct fields [packed] => ctype
     fields is a list with entries ‘(name type)’ where ‘type’ is a
     ‘<ctype>’ object or a symbol for a base type."
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
        (%make-ctype (incr-bit-size 0 sal ssz) sal 'struct
                     (%make-cstruct (reverse cfl) (reverse ral)))
        (match (car sfl)
          ((name type)                  ; non-bitfield
           (let* ((type (cond ((symbol? type) (cbase type))
                              ((ctype? type) type)
                              ((promise? type) (force type))
                              (else (error "cstruct: bad type" type))))
                  (fsz (ctype-size type))
                  (fal (ctype-align type))
                  (ssz (quotient (+ (* 8 ssz) 7) 8))
                  (ssz (incr-bit-size 0 fal ssz))
                  (cf (%make-cfield name type ssz)))
             (loop (cons cf cfl)
                   (cond
                    (name (acons name cf ral))
                    ((eq? 'struct (ctype-kind type))
                     (add-fields (cstruct-fields (ctype-info type)) ssz ral))
                    ((eq? 'union (ctype-kind type))
                     (add-fields (cunion-fields (ctype-info type)) ssz ral))
                    (else (error "bad field")))
                   (incr-size fsz fal ssz) (max fal sal) (cdr sfl))))

          ((name type width)            ; bitfield
           (let* ((type (cond ((symbol? type) (cbase type))
                              ((ctype? type) type)
                              (else (error "bad type:" type))))
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
           (sferr "cstruct: bad form: ~s" (car sfl))
           (error "yuck"))))))


;; @deffn {Procedure} cunion fields
;; fields is a list with entries @code{(name type)} where @code{type} is
;; a @code{<ctype>} object or a symbol for a base type.
;; @end deffn
(define (cunion fields)
  "- Procedure: cunion fields
     fields is a list with entries ‘(name type)’ where ‘type’ is a
     ‘<ctype>’ object or a symbol for a base type."
  (let loop ((cfl '()) (ral '()) (ssz 0) (sal 0) (sfl fields))
    (if (null? sfl)
        (%make-ctype (incr-size 0 sal ssz) sal 'union
                     (%make-cunion (reverse cfl) (reverse ral)))
        (let* ((name (caar sfl))
               (type (cadar sfl))
               (type (cond ((symbol? type)
                            (cbase type))
                           ((ctype? type) type)
                           ((promise? type) (force type))
                           (else (error "cunion: bad type"))))
               (fsz (ctype-size type))
               (fal (ctype-align type))
               (ssz (maxi-size 0 fal ssz))
               (cf (%make-cfield name type 0)))
          (loop (cons cf cfl)
                (cond
                 (name (acons name cf ral))
                 ((eq? 'struct (ctype-kind type))
                  (add-fields (cstruct-fields (ctype-info type)) ssz ral))
                 ((eq? 'union (ctype-kind type))
                  (add-fields (cunion-fields (ctype-info type)) ssz ral))
                 (else (error "bad field")))
                (maxi-size fsz fal ssz) (max fal sal) (cdr sfl))))))


;; @deffn {Procedure} carray type n
;; Create an array of @var{type} with @var{length}.
;; If @var{length} is zero, the array length is unbounded (so be careful).
;; @end deffn
(define (carray type n)
  "- Procedure: carray type n
     Create an array of TYPE with LENGTH.  If LENGTH is zero, the array
     length is unbounded (so be careful)."
  (%make-ctype (* n (ctype-size type)) (ctype-align type)
               'array (%make-carray type n)))


;; @deffn {Procedure} cenum enum-list [#:short=#f]
;; @var{enum-list} is a list of name or name-value pairs
;; @example
;; (cenum '((a 1) b (c 4))
;; @end example
;; If @var{short} is @code{#t} the size wil be smallest that can hold it.
;; @end deffn
(define* (cenum enum-list #:key short)
  "- Procedure: cenum enum-list
     ENUM-LIST is a list of name or name-value pairs
          (cenum '((a 1) b (c 4))"
  (define (short-mtype mn mx)
    (if (< 0 mn)
        (cond
         ((and (<= -128 mn) (< mx 128)) 's8)
         ((and (<= -32768 mn) (< mx 32768)) (mtypeof-basetype 'short))
         ((and (<= -2147483648 mn) (< mx 2147483648)) (mtypeof-basetype 'int))
         (else (mtypeof-basetype 'long)))
        (cond
         ((< mx 256) 'u8)
         ((< mx 32768) (mtypeof-basetype 'unsigned-short))
         ((< mx 2147483648) (mtypeof-basetype 'unsigned-int))
         (else (mtypeof-basetype 'unsigned-long)))))
  (let loop ((nvl '()) (nxt 0) (enl enum-list))
    (if (null? enl)
        (let* ((mx (car nvl)) (nvl (reverse nvl))
               (mn (car nvl)) (vnl (map xcons nvl))
               (mtype (if short (short-mtype mn mx) (mtypeof-basetype 'int))))
          (%make-ctype (sizeof-mtype mtype) (alignof-mtype mtype)
                       'enum (%make-cenum mtype nvl vnl)))
        (match (car enl)
          (`(,n ,v) (loop (acons n v nvl) (1+ v) (cdr enl)))
          ((? symbol? n) (loop (acons n nxt nvl) (1+ nxt) (cdr enl)))
          (((? symbol? n)) (loop (acons n nxt nvl) (1+ nxt) (cdr enl)))
          (_ (error "cenum: bad enum def'n"))))))

;; @deffn {Procedure} cfunction ret-type arg-types [#:variadic? VA]
;; n can be zero in which case ...@*
;; names ???
;; @end deffn
(define* (cfunction ret-type arg-types #:key variadic? arg-names)
  (let ((type (cbase 'void*)))
    (%make-ctype (ctype-size type) (ctype-align (type)) 'function
                 (%make-cfunction ret-type arg-types arg-names variadic?))))

;; @deffn {Procedure} make-cdata type [value] [options]
;; @table @code
;; @item #:name
;; name to attach to data object (typically a typename)
;; @item #:from-pointer
;; generate from address of data
;; @end table
;; @end deffn
(define* (make-cdata type #:optional value #:key name from-pointer)
  (assert-ctype 'make-cdata type)
  (let* ((size (ctype-size type))
         (bv (if from-pointer
                 (pointer->bytevector from-pointer size)
                 (make-bytevector size)))
         (data (%make-cdata bv 0 type name)))
    (if value (cdata-set! data value))
    data))

;; @deffn {Procedure} cdata-sel data tag ... => cdata
;; Return a new @code{cdata} object representing the associated selection.
;; For example,
;; @example
;; dat1 -> <cdata 0x12345678 struct>
;; (cdata-ref dat1 'a 'b 'c) -> <cdata 0x12345700> f64le>
;; @end example
;; @end deffn
(define (cdata-sel data . tags)
  "- Procedure: cdata-sel data tag ...
     Return a new ‘cdata’ object representing the associated selection.
     For example,
          dat1 -> <cdata 0x12345678 struct>
          (cdata-ref dat1 'a 'b 'c) -> <cdata 0x12345700> f64le>"
  (assert-cdata 'cdata-sel data)
  (if (null? tags)
      data
      (call-with-values
          (lambda () (ctype-sel (cdata-ct data) (cdata-ix data) tags))
        (lambda (ix ct)
          (%make-cdata (cdata-bv data) ix ct #f)))))

;; @deffn {Procedure} cdata-ref data [tag ...]
;; Return the Scheme (scalar) slot value for selected @var{tag ...} with
;; respect to the cdata object @var{data}.
;; @example
;; (cdata-ref my-struct-value 'a 'b 'c))
;; @end example
;; This procedure works for cdata kinds @emph{base}, @emph{pointer} and (in
;; the future) @emph{function}.  Attempting to obtain values for C-type kinds
;; @emph{struct}, @emph{union}, @emph{array} will result in @code{#f}.
;; If, in those cases, you would like a cdata then use this:
;; @example
;; (or (cdata-ref data tag ...) (cdata-sel data tag ...))
;; @end example
;; (Or should we just make this the default behavior?)
;; @end deffn
(define (cdata-ref data . tags)
  "- Procedure: cdata-ref data [tag ...]
     Return the Scheme (scalar) slot value for selected TAG ... with
     respect to the cdata object DATA.  This works for cdata kinds
     _base_, _pointer_ and (in the future) _function_.  Attempting to
     obtain values for C-type kinds _struct_, _union_, or _array_ will
     result in ‘#f’.
          (cdata-ref my-struct-value 'a 'b 'c))"
  (assert-cdata 'cdata-ref data)
  (let ((bv (cdata-bv data)))
    (call-with-values
        (lambda () (ctype-sel (cdata-ct data) (cdata-ix data) tags))
      (lambda (ix ct)
        (case (ctype-kind ct)
          ((base)
           (mtype-bv-ref (ctype-info ct) bv ix))
          ((pointer)
           (make-pointer
            (mtype-bv-ref (cpointer-mtype (ctype-info ct)) bv ix)))
          ((bitfield)
           (let* ((bi (ctype-info ct)) (mt (cbitfield-mtype bi))
                  (sh (cbitfield-shift bi)) (wd (cbitfield-width bi))
                  (sx (cbitfield-sext? bi)) (sm (expt 2 (1- wd)))
                  (v (bit-extract (mtype-bv-ref mt bv ix) sh (+ sh wd))))
             (if (and sx (logbit? (1- wd) v)) (- (logand v (1- sm)) sm) v)))
          ((enum) @@@)
          ((function) @@@)
          ((array) #f)
          ((struct) #f)
          ((union) #f)
          (else (error "bad stuff")))))))

;; @deffn {Procedure} cdata-set! data value [tag ...]
;; Set slot for selcted @var{tag ...} with respect to cdata @var{data} to
;; @var{value}.  Example:
;; @example
;; (cdata-set! my-struct-data 42 'a 'b 'c))
;; @end example
;; If @var{value} is a @code{<cdata>} object copy that, if types match.
;; @end deffn
(define (cdata-set! data value . tags)
  "- Procedure: cdata-set! data value [tag ...]
     Set slot for selcted TAG ... with respect to cdata DATA to VALUE.
     Example:
          (cdata-set! my-struct-data 42 'a 'b 'c))"
  (assert-cdata 'cdata-set! data)
  (let ((bv (cdata-bv data)) (ix (cdata-ix data)) (ct (cdata-ct data)))
    (call-with-values
        (lambda () (ctype-sel (cdata-ct data) (cdata-ix data) tags))
      (lambda (ix ct)
        (if (cdata? value)
            (let ((sz (ctype-size ct)))
              (unless (ctype-equal? (cdata-ct value) ct)
                (error "cdata-set!: bad arg"))
              (bytevector-copy! (cdata-bv value) (cdata-ix value) bv ix sz))
            (case (ctype-kind ct)
              ((base)
               (mtype-bv-set! (ctype-info ct) bv ix value))
              ((pointer)
               (mtype-bv-set! (cpointer-mtype (ctype-info ct)) bv ix value))
              ((bitfield)
               (let* ((bi (ctype-info ct)) (mt (cbitfield-mtype bi))
                      (sh (cbitfield-shift bi)) (wd (cbitfield-width bi))
                      (sx (cbitfield-sext? bi)) (am (1- (expt 2 wd)))
                      (dmi (lognot (ash am sh))) (mv (mtype-bv-ref mt bv ix))
                      (mx (bit-extract mv 0 (1- (* 8 (ctype-size ct))))))
                 (mtype-bv-set! mt bv ix (logior (logand mx dmi)
                                                 (ash value sh)))))
              ((struct) #f)
              ((union) #f)
              ((array) #f)
              (else
               (error "cdata-set!: bad arg 2" value))))))))

;; @deffn {Procedure} cdata& data => cdata
;; Generate a reference (i.e., cpointer) to the contents in the underlying
;; bytevector.
;; @end deffn
(define (cdata& data)
  (assert-cdata 'cdata& data)
  (let* ((bv (cdata-bv data)) (ix (cdata-ix data)) (ct (cdata-ct data))
         (pa (+ (pointer-address (bytevector->pointer bv)) ix)))
    (make-cdata (cpointer ct) pa)))

;; @deffn {Procedure} cdata* data => cdata
;; De-reference a pointer.  Returns a @emph{cdata} object representing the
;; contents at the address in the underlying bytevector.
;; @end deffn
(define (cdata* data)
  "- Procedure: cdata* data
     De-reference a pointer.  Returns a _cdata_ object representing the
     contents at the address in the underlying bytevector."
  (assert-cdata 'cdata* data)
  (unless (and (cdata? data) (eq? 'pointer (ctype-kind (cdata-ct data))))
    (error "cdata*: bad arg"))
  (let* ((cptr (ctype-info (cdata-ct data)))
         (type (cpointer-type cptr))
         (mtype (cpointer-mtype cptr))
         (addr (mtype-bv-ref mtype (cdata-bv data) (cdata-ix data)))
         (bv (pointer->bytevector (make-pointer addr) (ctype-size type))))
    ;; TODO: check for (cdata-tn data)
    (%make-cdata bv 0 type #f)))


;; need to be able to cast array to pointer
(define* (cdata-cast data type #:key skip-check)
  ;;(assert-cdata 'cdata-cast data)
  ;;(assert-ctype 'cdata-cast type)
  (define (type-miss)
    (error "cdata-cast: type-mismatch:" (list (cdata-ct data) type)))
  (define (type-check ft tt)
    (unless (or skip-check (ctype-equal? ft tt)) (type-miss)))
  (let ((bv (cdata-bv data))
        (ix (cdata-ix data))
        (ct (cdata-ct data))
        (tokind (ctype-kind type)))
    (case (ctype-kind ct)
      ((base)
       (case tokind
         ((base)
          #f)
         (else (type-miss))))
      ((array)
       (case tokind
         ((pointer)
          (let* ((array (ctype-info ct))
                 (atype (carray-type array))
                 (ptype (cpointer-type (ctype-info type)))
                 )
            ;;(sferr "array of ~s\n" atype)
            ;;(sferr "point to ~s\n" ptype)
            ;;(quit)
            (type-check (carray-type (ctype-info ct))
                        (cpointer-type (ctype-info type)))
            (%make-cdata bv ix type #f)))
         (else (type-miss))))
      (else (type-miss)))))

;; @deffn {Procedure} pretty-print-ctype type [port]
;; Converts type to a literal tree and uses Guile's pretty-print function
;; to display it.  The default port is the current output port.
;; @end deffn
(define* (pretty-print-ctype type #:optional (port (current-output-port)))
  "- Procedure: pretty-print-ctype type [port]
     Converts type to a literal tree and uses Guile’s pretty-print
     function to display it.  The default port is the current output
     port."
  (assert-ctype 'pretty-print-ctype type)
  (define (cnvt type)
    (let ((info (ctype-info type)))
      (case (ctype-kind type)
        ((base)
         info)
        ((struct)
         `(cstruct
           ,(map
             (lambda (fld)
               (if (eq? 'bitfield (ctype-kind (cfield-type fld)))
                   (list (cfield-name fld)
                         (cbitfield-mtype (ctype-info (cfield-type fld)))
                         (cbitfield-width (ctype-info (cfield-type fld)))
                         #:offset (cfield-offset fld))
                   (list (cfield-name fld) (cnvt (cfield-type fld))
                         #:offset (cfield-offset fld))))
             (cstruct-fields info))))
        ((union)
         `(cunion
           ,(map
             (lambda (fld) (list (cfield-name fld) (cnvt (cfield-type fld))))
             (cunion-fields info))))
        ((pointer)
         (if (promise? (%cpointer-type info))
             `(cpointer (delay ...))
             `(cpointer ,(cnvt (cpointer-type info)))))
        ((array)
         `(carray ,(cnvt (carray-type info)) (carray-length info)))
        (else (error "pretty-print-ctype: needs work" (ctype-kind type))))))
  (pretty-print (cnvt type) port))


;; --- guile ffi api support ---------------------------------------------------

(define (mtype->ffi mtype)
  (or
   (assq-ref
    `((s8 . ,int8)  (u8 . ,uint8)
      ;;(s16 . ,int16) (s32 . ,int32) (s64 . ,int64)
      ;;(u16 . ,uint16) (u32 . ,uint32) (u64 . ,uint64)
      ;;(f32 . ,float) (f64 . ,double)
      ;;(s128 . #f) (u128 . #f) (f16 . #f) (f128 . #f)
      (s16le . ,int16) (s32le . ,int32) (s64le . ,int64)
      (u16le . ,uint16) (u32le . ,uint32) (u64le . ,uint64)
      (f32le . ,float) (f64le . ,double)
      (s16be . ,int16) (s32be . ,int32) (s64be . ,int64)
      (u16be . ,uint16) (u32be . ,uint32) (u64be . ,uint64)
      (f32be . ,float) (f64be . ,double)
      (u128le . #f) (f16le . #f) (f128le . #f) (s128be . #f)
      (i128le . #f) (u128be . #f) (f16be . #f) (f128be . #f))
    mtype)
   (error "mtype->ffi: bad mtype")))

(define (ctype->ffi type)
  (assert-ctype 'ctype->ffi type)
  (let ((info (ctype-info type)))
    (case (ctype-kind type)
      ((base) (mtype->ffi info))
      ((struct union)
       (make-list (/ (ctype-size type) (ctype-align type))
                  (case (ctype-align type)
                    ((1) int8) ((2) int16) ((4) int32) ((8) int64))))
      ((pointer array) '*)
      ((function) (error "ctype->ffi: functions are work to go"))
      (else (error "ctype->ffi: unsupported:" (ctype-kind type))))))


(use-modules (system foreign-library))

(define (foreign-library-pointer/search libs name)
  (let loop ((libs libs))
    (cond
     ((null? libs) (error "not found"))
     ((false-if-exception (foreign-library-pointer (car libs) name)))
     (else (loop (cdr libs))))))

;; --- c99 support -------------------------------------------------------------

(define (mtype->c-name mtype)
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
   (error "mtype->c-name: bad mtype")))
(export mtype->c-name)

;; --- last line ---
