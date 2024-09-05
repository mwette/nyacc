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

;; (cbase sym-name) -> <ctype>
;; (cstruct ((name type) ...) [#t])-> <ctype>
;; (cunion ((name type) ...)) -> <ctype>
;; (cpointer type) -> <ctype>
;; (carray type sz) -> <ctype>
;; (cenum ((name . val) ...)) -> <ctype>
;; (cfunction proc->ptr ptr->proc) -> <ctype>

;; (make-cdata ct [val [name]]) -> <cdata>
;; (cdata-ref data tag ...) -> number | pointer | <cdata>
;; (cdata-set! data val tag ...) -> undefined
;; (cdata* data) -> <cdata>
;; (cdata& data) -> <cdata>
;; (cdata&-ref data) -> pointer
;; (cdata*-ref data) -> number | procedure | ???

;; lesser used:
;; (cdata-sel data tag ...) -> <cdata>
;; (ctype-equal? a b)
;; (ctype-kind type) -> in '(base pointer struct union array function)
;; (cdata-kind data) -> in '(base pointer struct union array function)
;; (ctype-sel type ix tag ...) -> ix ct
;; (ctype-sel* type ix tag ...) -> ((ix . ct) (ix . ct) ...)
;; (cdata-cast cdata type) -> <cdata>
;; (cdata-arg ffi-type guile-value) -> (tpye . value)

;; thinking about this this:
;; (ctype-sel type ix tags) -> (values ix ct)
;; (make-cdata ct)
;; (make-cdata bv ix ct)
;; (make-cdata bv ix ct val)
;; (make-cdata ct #:from-pointer ptr)
;; (make-cdata* ct #:from-pointer pointer #:offset 0)

;; use existing bytevector
;; (%make-cdata bv ix ct tn)

;; (cbase symb) and cstruct cunion carray cpointer cfunction
;; (list->vector (map (lambda (ix) (cdata-ref data ix)) (iota 10)))

;; ffi:
;; (cpointer->procedure ret-arg args [va-args])

;;; Code:

(define-module (system foreign cdata)
  #:export (NULL
            cbase cstruct cunion cpointer carray cenum cfunction
            make-cdata cdata-ref cdata-set! cdata&-ref cdata-cast
            pretty-print-ctype
            
            ctype? ctype-size ctype-align ctype-kind ctype-info ctype-equal?
            cdata? cdata-bv cdata-ix cdata-ct cdata-tn
            
            cdata-kind cdata& cdata* cdata-sel
            ctype-sel ctype-sel*
            ;;ctype->ffi
            ;;
            NULL
            unwrap-number unwrap-pointer unwrap-array
            ;;
            mtype-bv-ref mtype-bv-set!
            ;; debug
            cstruct-fields cstruct-dict
            cunion-fields cunion-dict
            cfield-name cfield-type cfield-offset
            cbitfield-mtype cbitfield-shift cbitfield-width cbitfield-signed?
            cpointer-type cpointer-mtype
            carray-type carray-length
            cenum-vald cenum-symd
            cfunction-proc->ptr cfunction-ptr->proc
            )
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

;; @deftp {Record} <cbitfield> type shift width signed?
;; signed? is true if type is signed, means we need to sign-extend
;; @end deftp
(define-record-type <cbitfield>
  (%make-cbitfield mtype shift width signed?)
  cbitfield?
  (mtype cbitfield-mtype)
  (shift cbitfield-shift)
  (width cbitfield-width)
  (signed? cbitfield-signed?))

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
;; @table @var
;; @item mtype
;; machine type to store
;; @item symd
;; value to symbol dict (or call it nambynum?)
;; @item vald
;; symbol to value (or call it numbynam?)
;; @end deftp
(define-record-type <cenum>
  (%make-cenum mtype symd vald)
  cenum?
  (mtype cenum-mtype)                   ; underlying basic C type
  (symd cenum-symd)                     ; value -> name
  (vald cenum-vald))                    ; name -> value

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

;; @deftp {Record} <cfunction> proc->ptr ptr->proc variadic?
;; The argument @var{proc->ptr} is a procedure converts a Guile procedure
;; to a Guile pointer (typically using Guile's @code{procedure->pointer}).
;; The argument @var{ptr->proc} is a procedure to convert from pointer to
;; procedure (typically calling Guile's @code{pointer->procedure}).
;; @end deftp
(define-record-type <cfunction>
  (%make-cfunction proc->ptr ptr->proc variadic?)
  cfunction?
  (proc->ptr cfunction-proc->ptr)
  (ptr->proc cfunction-ptr->proc)
  (variadic? cfunction-variadic?))

(set-record-type-printer! <ctype>
  (lambda (type port)
    (let ((kd (ctype-kind type)) (nf (ctype-info type))
          (ad (pointer-address (scm->pointer type))))
      (format port "#<ctype ~s 0x~x>" (if (eq? 'base kd) nf kd) ad))))

(define make-ctype %make-ctype)

;; @deftp {Record} <cbase-info> arch ctyped
;; This record keeps, for each arch, map of symbolic type names to
;; @code{<ctype>}.
;; @end deftp
(define-record-type <cbase-info>
  (%make-cbase-info arch ctyped)
  cbase-info?
  (arch cbase-info-arch)
  (ctyped cbase-info-ctyped))

;; map of arch (from @code{(*arch*)}) -> cbase-info
(define *cbase-map* (make-parameter '()))

(display "cdata: need global address offset for cross-targets\n")
;;(define *cdata-adm* (make-parameter '()))
;; native => identity
;; or base-address
;; (with-address-offset #x10000 ....

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
           (name (cdata-tn data))
           (kind (ctype-kind type))
           (bv-addr (pointer-address (bytevector->pointer bv))))
      (format port "#<cdata")
      (if name (format port " ~a>" name) (format port " ~a>" kind))
      (format port " 0x~x>" (+ bv-addr ix)))))

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

;; @deffn {Procedure} cbase name
;; @end deffn
(define (cbase name)
  (let* ((arch (*arch*))
         (name (cond ((symbol? name) name)
                     ((string? name) (strname->symname name))
                     (else (error "cbase: bad arg"))))
         (cmap (or (assoc-ref (*cbase-map*) arch)
                   (let ((cmap (make-cbase-map arch)))
                     (*cbase-map* (acons arch cmap (*cbase-map*)))
                     cmap))))
    (or (assq-ref cmap name)
        (and (memq name cbase-symbols) name)
        (error "cbase: not found: " name))))

;; @deffn {Procedure} cpointer type
;; Generate a C pointer type to @var{type}. To reference or de-reference
;; cdata object see @code{cdata&} and @code{cdata*}.  @var{type} can be
;; the symbol @code{void} or a symbolic name used as argument to @code{cbase}.
;; @*note: Should we allow @ver{type} to be a promise?
;; @example
;; (define foo_t (cbase 'int))
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

;; Update running struct size given field size and alignment.
(define (incr-size fs fa ss)
  (+ fs (* fa (quotient (+ ss (1- fa)) fa))))

;; Update running union size given field size and alignment.
(define (maxi-size fs fa ss)
  (max fs ss))

;; Round number of bits to next alignment size.z
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
  (assert-ctype 'carray type)
  (%make-ctype (* n (ctype-size type)) (ctype-align type)
               'array (%make-carray type n)))


;; @deffn {Procedure} cenum enum-list [packed]
;; @var{enum-list} is a list of name or name-value pairs
;; @example
;; (cenum '((a 1) b (c 4))
;; @end example
;; If @var{packed} is @code{#t} the size wil be smallest that can hold it.
;; @end deffn
(define* (cenum enum-list #:optional basetype)
  "- Procedure: cenum enum-list [basetype]
     ENUM-LIST is a list of name or name-value pairs
          (cenum '((a 1) b (c 4))
     If SHORT is ‘#t’ the size wil be smallest that can hold it."
  (define (short-mtype mn mx)
    (if (< 0 mn)
        (cond
         ((and (<= -128 mn) (< mx 128)) 's8)
         ((and (<= -32768 mn) (< mx 32768)) (mtypeof-basetype 'int16_t))
         ((and (<= -2147483648 mn) (< mx 2147483648))
          (mtypeof-basetype 'int32_t))
         (else (mtypeof-basetype 'int)))
        (cond
         ((< mx 256) 'u8)
         ((< mx 32768) (mtypeof-basetype 'uint16_t))
         ((< mx 2147483648) (mtypeof-basetype 'uint32_t))
         (else (mtypeof-basetype 'int)))))
  (let loop ((nvl '()) (nxt 0) (enl enum-list))
    (if (null? enl)
        (let* ((mx (cdar nvl)) (nvl (reverse nvl)) (mn (cdar nvl))
               (vnl (map (lambda (p) (cons (cdr p) (car p))) nvl))
               (mtype (if short (short-mtype mn mx) (mtypeof-basetype 'int))))
          (%make-ctype (sizeof-mtype mtype) (alignof-mtype mtype)
                       'enum (%make-cenum mtype vnl nvl)))
        (match (car enl)
          (`(,n ,v) (loop (acons n v nvl) (1+ v) (cdr enl)))
          ((? symbol? n) (loop (acons n nxt nvl) (1+ nxt) (cdr enl)))
          (((? symbol? n)) (loop (acons n nxt nvl) (1+ nxt) (cdr enl)))
          (_ (error "cenum: bad enum def'n"))))))

;; @deffn {Procedure} cfunction proc->ptr ptr->proc [variadic?]
;; Generate a C function pointer type.  You must pass the @var{wrapper}
;; and @var{unwrapper} procedures that convert a pointer to a procedure,
;; and procedure to pointer, respectively.  The optional argument
;; @var{#:variadic}, if @code{#t},  indicates the function uses variadic
;; arguments.  For this case, (to be documented).
;; @end deffn
(define* (cfunction proc->ptr ptr->proc #:optional variadic?)
  (let ((type (cbase 'intptr_t)))
    (%make-ctype (ctype-size type) (ctype-align type) 'function
                 (%make-cfunction proc->ptr ptr->proc variadic?))))

;; @deffn {Procedure} ctype-detag type ix tag
;; Follows @var{tag}.  For structs and unions, the tag is a symbolic
;; field name.  For arrays and pointers, the tag is a non-negative integer.
;; An integer tag applied to the pointer increments the pointer by the
;; associated number of elements referenced.
;; @end deffn
;; MAYBE JUST MAYBE make this return a list of (ix ct) (ix ct) ...
;; 
(define (ctype-detag ct ix tag)
  (assert-ctype 'ctype-detag ct) ;; not needed assuming stable mod
  (let ((ti (ctype-info ct)))
    (case (ctype-kind ct)
      ((struct)
       (let ((fld (assq-ref (cstruct-dict ti) tag)))
         (unless fld (error "ctype: bad field " tag))
         (values (+ ix (cfield-offset fld)) (cfield-type fld))))
      ((union)
       (let ((fld (assq-ref (cunion-dict ti) tag)))
         (unless fld (error "ctype: bad field " tag))
         (values (+ ix (cfield-offset fld)) (cfield-type fld))))
      ((array)
       (unless (integer? tag) (error "bad array ref"))
       (let ((type (carray-type ti)))
         (values (+ ix (* tag (ctype-size type))) type)))
      ((pointer)
       (error "ctype-detag: don't call on me for a pointer dereference"))
      (else (error "bad tag" tag)))))

;; @deffn {Procedure} ctype-sel type ix tags => ix ct
;; @deffnx {Procedure} ctype-sel* type ix tags => ((ix . ct) (ix . ct) ...)
;; offset from zero
;; @end deffn
(define (ctype-sel type ix tags)
  (assert-ctype 'ctype-sel type)
  (let loop ((ct type) (ix ix) (tags tags))
    (cond
     ((null? tags)
      (values ix ct))
     ((eq? 'pointer (ctype-kind ct))
      (error "ctype-sel: pointer dereference; try ctype-sel*"))
     (else
      (call-with-values (lambda () (ctype-detag ct ix (car tags)))
        (lambda (ix ct) (loop ct ix (cdr tags))))))))

(define (ctype-sel* type ix tags)
  (assert-ctype 'ctype-sel type)
  (let loop ((res '()) (ct type) (ix 0) (tags tags))
    (sferr "tags=~s  res=~s\n  ct=~s  ix=~s\n" tags res ct ix)
    (cond
     ((null? tags)
      (reverse (cons (cons ix ct) res)))
     ((eq? 'pointer (ctype-kind ct))
      (let* ((info (ctype-info ct))
             (type (cpointer-type info)))
        (cond
         ((eq? '* (car tags))
          (loop (cons (cons ix ct) res) type 0 (cdr tags)))
         ((integer? (car tags))
          (let ((ix (+ ix (* (car tags) (ctype-size type)))))
            (loop (cons (cons ix ct) res) type 0 (cdr tags))))
         (else
          (error "bad tag for pointer")))))
     (else
      (call-with-values (lambda () (ctype-detag ct ix (car tags)))
        (lambda (ix ct) (loop res ct ix (cdr tags))))))))


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

;; @deffn {Procedure} make-cdata type [value [name]]
;; Generate a @emph{cdata} object of type @var{type} with optional
;; @var{value} and @var{name}.  To specify name but no value use
;; something like
;; @example
;; (make-cdata mytype #f "foo")
;; @end example
;; @end deffn
(define* (make-cdata type #:optional value name)
  (assert-ctype 'make-cdata type)
  (case (ctype-kind type)
    ((array)
     (let* ((ca (ctype-info type)) (ln (carray-length ca)))
       (cond
        ((zero? ln)
         (unless (integer? value) (error "make-cdata: zero sized array type"))
         (let* ((et (carray-type ca)) (sz (ctype-size et))
                (bv (make-bytevector (* ln sz))))
           (%make-cdata bv 0 (carray et ln) name)))
        (else
         (when value (error "can't initialize arrays yet"))
         (%make-cdata (make-bytevector (ctype-size type)) 0 type name)))))
    (else
     (let* ((size (ctype-size type))
            (bvec (make-bytevector size))
            (data (%make-cdata bvec 0 type name)))
       (if value (cdata-set! data value))
       data))))

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
  (if (null? tags) data
      (let loop ((bv (cdata-bv data)) (ix (cdata-ix data)) (ct (cdata-ct data))
                 (tags tags))
        (cond
         ((null? tags) (%make-cdata bv ix ct #f))
         ((eq? 'pointer (ctype-kind ct))
          (let* ((cptr (ctype-info ct))
                 (addr (mtype-bv-ref (cpointer-mtype cptr) bv ix))
                 (elty (cpointer-type cptr))
                 (elsz (ctype-size elty)))
            (cond
             ((eq? '* (car tags))
              (let ((eptr (make-pointer addr)))
                (loop (pointer->bytevector eptr elsz) 0 elty (cdr tags))))
             ((integer? (car tags))
              (let ((eptr (make-pointer (+ addr (* elsz (car tags)) elsz))))
                (loop (pointer->bytevector eptr elsz) 0 elty (cdr tags))))
             (else
              (error "cdata-sel: bad tag for pointer:" (car tags))))))
         (else
          (call-with-values (lambda () (ctype-detag ct ix (car tags)))
            (lambda (ix ct) (loop bv ix ct (cdr tags)))))))))

;; @deffn {Procedure} cdata-ref data [tag ...]
;; Return the Scheme (scalar) slot value for selected @var{tag ...} with
;; respect to the cdata object @var{data}.
;; @example
;; (cdata-ref my-struct-value 'a 'b 'c))
;; @end example
;; This procedure returns XXX for cdata kinds @emph{base}, @emph{pointer} and (in
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
  (let* ((data (apply cdata-sel data tags))
         (bv (cdata-bv data))
         (ix (cdata-ix data))
         (ct (cdata-ct data)))
    (case (ctype-kind ct)
      ((base)
       (mtype-bv-ref (ctype-info ct) bv ix))
      ((pointer)
       (make-pointer
        (mtype-bv-ref (cpointer-mtype (ctype-info ct)) bv ix)))
      ((bitfield)
       (let* ((bi (ctype-info ct)) (mt (cbitfield-mtype bi))
              (sh (cbitfield-shift bi)) (wd (cbitfield-width bi))
              (sx (cbitfield-signed? bi)) (sm (expt 2 (1- wd)))
              (v (bit-extract (mtype-bv-ref mt bv ix) sh (+ sh wd))))
         (if (and sx (logbit? (1- wd) v)) (- (logand v (1- sm)) sm) v)))
      ((enum)
       (let* ((info (ctype-info ct))
              (mtype (cenum-mtype info))
              (vnl (cenum-symd info)))
         (assq-ref vnl (mtype-bv-ref mtype bv ix))))
      ((array struct union) (make-cdata bv ix ct))
      ((function) #f)
      (else (error "bad stuff")))))

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
               (let ((addr (cond
                            ((pointer? value) (pointer-address value))
                            ((integer? value) value)
                            (else (error "cdata-set!: bad value" value)))))
                 (mtype-bv-set! (cpointer-mtype (ctype-info ct)) bv ix addr)))
              ((bitfield)
               (let* ((bi (ctype-info ct)) (mt (cbitfield-mtype bi))
                      (sh (cbitfield-shift bi)) (wd (cbitfield-width bi))
                      (sx (cbitfield-signed? bi)) (am (1- (expt 2 wd)))
                      (dmi (lognot (ash am sh))) (mv (mtype-bv-ref mt bv ix))
                      (mx (bit-extract mv 0 (1- (* 8 (ctype-size ct))))))
                 (mtype-bv-set! mt bv ix (logior (logand mx dmi)
                                                 (ash value sh)))))
              ((enum)
               (let* ((info (ctype-info ct)) (mtype (cenum-mtype info)))
                 (cond
                  ((integer? value)
                   (mtype-bv-set! mtype bv ix value))
                  ((symbol? value)
                   (mtype-bv-set! mtype bv ix
                                  (assq-ref (cenum-vald info) value)))
                  (else
                   (error "cdata-set! bad value arg: ~s" value)))))
              ((function) (error "cdata-set!: can't set! function value"))
              ((struct) (error "cdata-set!: can't set! struct value"))
              ((union) (error "cdata-set!: can't set! union value"))
              ((array) (error "cdata-set!: can't set! array value"))
              (else (error "cdata-set!: bad arg 2" value))))))))

;; @deffn {Procedure} make-cdata/* type pointer [name]
;; Make a cdata object from a pointer.   That is, instead of creating a
;; bytevector to hold the data use the memory at the pointer using
;; @code{pointer->bytevector}.
;; @* Maybe cdata-cast can do this?
;; @end deffn
(define* (make-cdata/* type pointer #:optional name)
   (assert-ctype 'make-cdata/ type)
   (let* ((size (ctype-size type))
          (bvec (pointer->bytevector pointer size))
          (data (%make-cdata bvec 0 type name)))
     data))

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
         (kind (ctype-kind type))
         (bvec (cdata-bv data))
         (indx (cdata-ix data))
         ;;
         (addr (mtype-bv-ref mtype (cdata-bv data) (cdata-ix data)))
         (pntr (make-pointer addr)))
    (case kind
      ((function) #f)
      (else
       (%make-cdata
        (pointer->bytevector pntr (ctype-size type))
        0 type #f)))))
;; TODO: check for (cdata-tn data)

;; @deffn {Procedure} cdata-kind data
;; Return the kind of @var{data}: pointer, base, struct, ...
;; @end deffn
(define (cdata-kind data)
  (assert-cdata 'cdata-kind data)
  (ctype-kind (cdata-ct data)))

;; @deffn {Procedure} cdata&-ref data
;; Does not work work (yet) for offset addresses.
;; @end deffn
(define (cdata&-ref data)
  (assert-cdata 'cdata&-ref data)
  (call-with-values (lambda () (cdata-sel data))
    (lambda (bv ix ct)
      (if (zero? ix)
          (bytevector->pointer bv)
          (make-pointer (+ (pointer-address (bytevector->pointer bv))
                           (* (sizeof '*) ix)))))))

;; @deffn {Procedure} cdata-cast data type [do-check] => <cdata>
;; need to be able to cast array to pointer
;; @* maybe call this ccast ?
;; @example
;; (cdata-cast val Target*) (ccast Target* val)
;; @end example
;; @end deffn
(define* (cdata-cast data type #:key do-check)
  (assert-cdata 'cdata-cast data)
  (assert-ctype 'cdata-cast type)
  (define (type-miss)
    (error "cdata-cast: incompatible type:" (list (cdata-ct data) type)))
  (define (type-check ft tt)
    (when (and do-check (ctype-equal? ft tt)) (type-miss)))
  (error "cdata-cast not done")
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
      ((pointer)
       (make-cdata ct bv ix))
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

;; --- not sure about this ===--------------------------------------------------

(define NULL %null-pointer)

(define (unwrap-number arg)
  (cond ((number? arg) arg)
        ((cdata? arg) (cdata-ref arg))
        (else (error "unwrap-number: bad arg: ~s" arg))))

(define* (unwrap-pointer arg #:optional hint)
  (cond ((pointer? arg) arg)
        ((string? arg) (string->pointer arg))
        ((cdata? arg) (cdata-ref arg))
        ((and (procedure? arg) (ctype? hint))
         (let* ((info (ctype-info hint))
                (func (case (ctype-kind hint)
                        ((function) info)
                        ((pointer) (ctype-info (cpointer-type info)))
                        (else (error "not ok")))))
           ((cfunction-proc->ptr func) arg)))
        (else (error "unwrap-pointer: bad arg: ~s" arg))))

(define (unwrap-array arg)
  (unless
    (cdata? arg)
    (error "unwrap-array: bad arg: " arg))
  (case (cdata-kind arg)
    ((pointer) (cdata-ref arg))
    ((array) (cdata&-ref arg))
    (else (error "unwrap-array: bad arg: " arg))))


;; --- guile ffi api support ---------------------------------------------------
;; maybe this should all be pulled out

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
