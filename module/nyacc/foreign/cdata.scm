;;; nyacc/foreign/cdata.scm -

;; Copyright (C) 2023-2025 Matthew Wette
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

;; basic usage
;; (cbase sym-name) -> <ctype>
;; (cstruct ((name type) ...) [#t])-> <ctype>
;; (cunion ((name type) ...)) -> <ctype>
;; (cpointer type) -> <ctype>
;; (carray type sz) -> <ctype>
;; (cenum ((name . val) ...)) -> <ctype>
;; (cfunction proc->ptr ptr->proc) -> <ctype>
;; (make-cdata ct [val]) -> <cdata>
;; (cdata-ref data tag ...) -> value | <cdata>
;; (cdata-set! data val tag ...)
;; (cdata& data) -> <cdata>

;; going further
;; (cdata-sel data tag ...) -> <cdata>
;; (cdata* data) -> <cdata>
;; (cdata*-sel data tag ...) -> <cdata>
;; (cdata*-ref data tag ...) -> value | <cdata>
;; (cdata&-sel data) -> <cdata>
;; (cdata&-ref data) -> <pointer>
;; (Xcdata-ref bv ix ct) -> value
;; (Xcdata-set! bv ix ct value)

;; working with types
;; (name-ctype name type)
;; (ctype-equal? a b)
;; (ctype-sel type ix tag ...) -> ((ix . ct) (ix . ct) ...)
;; (make-cdata-getter sel [offset]) => (proc data) -> value
;; (make-cdata-setter sel [offset]) => (proc data value) -> undefined

;; working with C function calls
;; (ccast type data) -> <cdata>
;; (unwrap-pointer data) -> <pointer>
;; (unwrap-number data) -> number
;; (unwrap-array data) -> <pointer>
;; (cdata-arg ffi-type guile-value) -> (ffi-desc . value)
;; (ctype->ffi type) => ffi-type (e.g., ffi:int)

;; misc utilities
;; (ctype-kind type) -> symbol
;; (cdata-kind data) -> symbol


;; thinking about this this:
;; (make-cdata ct)
;; (make-cdata bv ix ct)
;; (make-cdata bv ix ct val)
;; (make-cdata ct #:from-pointer ptr)
;; (make-cdata* ct #:from-pointer pointer #:offset 0)

;; redef cKIND? procedures to accept types? (cpointer? type) => pointer

;; (cbase symb) and cstruct cunion carray cpointer cfunction
;; (list->vector (map (lambda (ix) (cdata-ref data ix)) (iota 10)))

;; ffi:
;; (cpointer->procedure ret-arg args [va-args])

;;(define *cdata-adm* (make-parameter '()))
;; native => identity
;; or base-address
;; (with-address-offset #x10000 ....

;;; Code:

(define-module (nyacc foreign cdata)
  #:export (cbase
            cstruct cunion cpointer carray cenum cfunction
            make-cdata cdata-ref cdata-set! cdata&-ref ccast
            pretty-print-ctype
            
            ctype-size ctype-align ctype-kind ctype-info ctype-name
            ctype? ctype-equal?
            cdata? cdata-bv cdata-ix cdata-ct
            cdata-copy name-ctype

            Xcdata-ref Xcdata-set!
            
            cdata-kind cdata& cdata* cdata-sel 
            cdata*-sel cdata*-ref cdata&-sel cdata&-ref
            ctype-sel make-cdata-getter make-cdata-setter
            ctype->ffi
            ;;
            NULL NULL?
            cdata-arg->number cdata-arg->pointer
            unwrap-number unwrap-pointer unwrap-array
            ;;
            ;; debug
            cstruct-fields cstruct-select
            cunion-fields cunion-select
            cfield-name cfield-type cfield-offset
            cbitfield-mtype cbitfield-shift cbitfield-width cbitfield-signed?
            cpointer-type cpointer-mtype
            carray-type carray-length
            cenum-symf cenum-numf cenum-syml
            cfunction-proc->ptr cfunction-ptr->proc)
  #:re-export (mtype-bv-ref mtype-bv-set!)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module ((srfi srfi-1) #:select (fold xcons))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign)
                #:select (%null-pointer
                          make-pointer pointer? pointer-address
                          pointer->bytevector bytevector->pointer
                          scm->pointer string->pointer float double
                          int8 uint8 int16 uint16 int32 uint32 int64 uint64))
  #:use-module (nyacc foreign arch-info))

(use-modules (ice-9 pretty-print))
(define (pperr exp) (pretty-print exp (current-error-port)))
(define (sferr fmt . args) (apply simple-format #t fmt args))
(define (ppct ct) (pretty-print-ctype ct (current-error-port)))

;; @deftp {Record} <ctype> size align kind info name
;; @table @var
;; @item size
;; size in bytes
;; @item align
;; alignment in bytes
;; @item kind
;; A symbol, one of @code{base}, @code{pointer}, @code{struct}, @code{union},
;; @code{array}, @code{enum} or @code{function}
;; @item info
;; provides kind-specific data.  For @code{base} this is the symbolic
;; machine type (mtype).  For others, there are specific records,
;; described below.
;; @item name
;; can be @code{#f} or a symbolic name for the type, used when printing
;; the type.
;; @end table
;; @end deftp
(define-record-type <ctype>
  (%make-ctype size align kind info name)
  ctype?
  (size ctype-size)                ; size in bytes
  (align ctype-align)              ; alignment in bytes
  (kind ctype-kind)                ; type kind (base aggr array bits)
  (info ctype-info)                ; kind-specific info
  (name ctype-name))               ; name or #f

;; @deftp {Record} <cbitfield> mtype shift width signed?
;; Bitfields are not direct type kinds, but part of fields within
;; a struct.
;; @table var
;; @item type
;; the declared type of the bitfield
;; @item shift
;; @item signed?
;; is true if type is signed, means we need to sign-extend
;; @end table
;; @end deftp
(define-record-type <cbitfield>
  (%make-cbitfield mtype shift width signed?)
  cbitfield?
  (mtype cbitfield-mtype)
  (shift cbitfield-shift)
  (width cbitfield-width)
  (signed? cbitfield-signed?))

;; @deftp {Record} <cfield> name type offset
;; @table @var
;; @item name
;; @item type
;; @item offset
;; @end table
;; @end deftp
(define-record-type <cfield>
  (%make-cfield name type offset)
  cfield?
  (name cfield-name)
  (type cfield-type)
  (offset cfield-offset))

;; @deftp {Record} <cstruct> fields select
;; @table @var
;; @item fields
;; @item select
;; @end table
;; @end deftp
(define-record-type <cstruct>
  (%make-cstruct fields select)
  cstruct?
  (fields cstruct-fields)          ; list of fields
  (select cstruct-select))         ; proc: symbol => field

;; @deftp {Record} <cunion> fields select
;; @table @var
;; @item fields
;; @item select
;; @end table
;; @end deftp
(define-record-type <cunion>
  (%make-cunion fields select)
  cunion?
  (fields cunion-fields)           ; list of fields
  (select cunion-select))          ; proc: symbol => field

;; @deftp {Record} <carray> type length
;; XXX
;; @table @var
;; @item type
;; @end table
;; @end deftp
(define-record-type <carray>
  (%make-carray type length)
  carray?
  (type carray-type)
  (length carray-length))

;; @deftp {Record} <cenum> mtype symf numf syml
;; @table @var
;; @item mtype
;; machine type to store
;; @item symf
;; value to symbol proc
;; @item numf
;; symbol to value proc
;; @item syml
;; list of symbols
;; @end table
;; @end deftp
(define-record-type <cenum>
  (%make-cenum mtype symf numf syml)
  cenum?
  (mtype cenum-mtype)                   ; underlying basic C type
  (symf cenum-symf)                     ; proc: number -> symbol
  (numf cenum-numf)                     ; proc: symbol -> number
  (syml cenum-syml))                    ; list of symbols

;; @deftp {Record} <cpointer> type mtype
;; Once we get to this level, we shouldn't need @code{arch} anymore
;; so we need to log the pointer type
;; @table @var
;; @item type
;; @end table
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
;; This type represents a C function in memory.  The data value will be
;; a proxy: a pointer to the initial location in memory.
;; @table @var
;; @item type
;; @end table
;; The argument @var{proc->ptr} is a procedure converts a Guile procedure
;; to a Guile pointer (typically using Guile's @code{procedure->pointer}).
;; The argument @var{ptr->proc} is a procedure to convert from pointer to
;; procedure (typically calling Guile's @code{pointer->procedure}).
;; @* psize is proxy size (usually void* mtype).  Think of the proxy as a
;; trampoline for the function.  The pointer points to proxy (trampoline)
;; @* This may need another field to enable correct @code{ctype-equal?}.
;; @end deftp
(define-record-type <cfunction>
  (%make-cfunction proc->ptr ptr->proc variadic? ptr-mtype)
  cfunction?
  (proc->ptr cfunction-proc->ptr)
  (ptr->proc cfunction-ptr->proc)
  (variadic? cfunction-variadic?)
  (ptr-mtype cfunction-ptr-mtype))

(set-record-type-printer! <ctype>
  (lambda (type port)
    (let ((kd (ctype-kind type))
          (nf (ctype-info type))
          (ad (pointer-address (scm->pointer type)))
          (nm (ctype-name type)))
      (format port "#<ctype ~s" (if (eq? 'base kd) nf kd))
      (if nm (format port " ~s" nm))
      (format port " 0x~x>" ad))))

(define make-ctype %make-ctype)

;; map of arch (from @code{(*arch*)}) -> cbase
(define *cbase-map* (make-parameter '()))

;; @deftp {Record} <cdata> bv ix ct pl
;; Record to hold C data.  Underneath it's a bytevector, index and type.
;; @end deftp
(define-record-type <cdata>
  (%make-cdata bv ix ct pl)
  cdata?
  (bv cdata-bv)                         ; bvec
  (ix cdata-ix)                         ; index
  (ct cdata-ct)                         ; type
  (pl cdata-pl))                        ; pointers

(set-record-type-printer! <cdata>
  (lambda (data port)
    (let* ((bv (cdata-bv data))
           (ix (cdata-ix data))
           (type (cdata-ct data))
           (kind (ctype-kind type))
           (name (ctype-name type))
           (bv-addr (pointer-address (bytevector->pointer bv))))
      (format port "#<cdata")
      (cond
       (name (format port " ~a" name))
       ((eq? 'base kind) (format port " ~a" (ctype-info type)))
       (else (format port " ~a" kind)))
      (format port " 0x~x>" (+ bv-addr ix)))))

(define-inlinable (assert-ctype p v)
  (unless (ctype? v)
    (error (simple-format #f "~a: expected <ctype>, got ~s" p v))))

(define-inlinable (assert-cdata p v)
  (unless (cdata? v)
    (error (simple-format #f "~a: expected <cdata>, got ~s" p v))))

;;.@deffn {Procedure} make-cbase-map arch => hashq-table
;; Create a hashq table for @var{arch} mapping base C types to cdata types.
;;.@end deffn
(define (make-cbase-map arch)
  (define (make-cbase name)
    (let* ((mtype (mtypeof-basetype name))
           (size (sizeof-basetype name))
           (align (alignof-basetype name)))
      (%make-ctype size align 'base mtype #f)))
  (with-arch arch
    (alist->hashq-table
     (map (lambda (name) (cons name (make-cbase name)))
          base-type-symbol-list))))

(define cbase-symbols
  '(s8 u8 s16 s32 s64 i128 u16 u32 u64 u128 f16 f32 f64 f128
       s16le s32le s64le i128le u16le u32le u64le u128le
       f16le f32le f64le f128le s16be s32be s64be i128be
       u16be u32be u64be u128be f16be f32be f64be f128be))

;; @deffn {Procedure} cbase name => <ctype>
;; Given symbolic @var{name} generate a base ctype.   The name can
;; be something like @code{unsigned-int}, @code{double}, or can be a
;; @emph{cdata} machine type like @code{u64le}.  For example,
;; @example
;; (define double-type (cbase 'double))
;; @end example
;; @end deffn
(define (cbase name)
  "- Procedure: cbase name => <ctype>
     Given symbolic NAME generate a base ctype.  The name can be
     something like ‘unsigned-int’, ‘double’, or can be a _cdata_
     machine type like ‘u64le’.  For example,
          (define double-type (cbase 'double))"
  (let* ((arch (*arch*))
         (name (cond ((symbol? name) name)
                     ((string? name) (strname->symname name))
                     (else (error "cbase: bad arg:" name))))
         (cmap (or (assoc-ref (*cbase-map*) arch)
                   (let ((cmap (make-cbase-map arch)))
                     (*cbase-map* (acons arch cmap (*cbase-map*)))
                     cmap))))
    (or (hashq-ref cmap name)
        (and (memq name cbase-symbols) name)
        (error "cbase: not found:" name))))

;; @deffn {Procedure} cpointer type => <ctype>
;; Generate a C pointer type for @var{type}. To reference or de-reference
;; cdata object see @code{cdata&} and @code{cdata*}.  @var{type} can be
;; the symbol @code{void} or a symbolic name used as argument to @code{cbase}.
;; @*note: Should we allow @var{type} to be a promise?
;; @example
;; (define foo_t (cbase 'int))
;; (cpointer (delay foo_t))
;; @end example
;; @end deffn
(define (cpointer type)
  "- Procedure: cpointer type => <ctype>
     Generate a C pointer type for TYPE.  To reference or de-reference
     cdata object see ‘cdata&’ and ‘cdata*’.  TYPE can be the symbol
     ‘void’ or a symbolic name used as argument to ‘cbase’.
     note: Should we allow TYPE to be a promise?
          (define foo_t (cbase 'int))
          (cpointer (delay foo_t))"
  (let ((type (cond
               ((ctype? type) type)
               ((eq? 'void type) type)
               ((symbol? type) (cbase type))
               ((promise? type) type)
               (else (error "cpointer: bad arg:" type)))))
    (%make-ctype (sizeof-basetype 'void*) (alignof-basetype 'void*)
                 'pointer (%make-cpointer type (mtypeof-basetype 'void*)) #f)))

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

;; convert alist to perfect hash lookup : work in progress
(define (alist->phlkup alist)
  (define (nextn n) (+ n (/ (if (odd? n) (1+ n) n) 2)))
  (let loop ((kl '()) (n (length alist)) (mx -1) (mn #xffffffff) (al alist))
    (if (pair? al)
        (let ((hv (hash (caar al) n)))
          (if (memq hv kl)
              (loop '() (nextn n) -1 #xffffffff alist)
              (loop (cons hv kl) n (max mx hv) (min mn hv) (cdr al))))
        (let* ((sz (- mx mn -1))
               (hv (make-vector sz #f)))
          (for-each (lambda (k kv) (vector-set! hv (- k mn) (cdr kv))) kl alist)
          ;; to finish need lambda:
          (sferr "ral size =~s ph len=~s min=~s\n" (length alist) sz mn)
          (lambda (sym) (vector-ref hv (- (hash sym n) mn)))))))

;; @deffn {Procedure} cstruct fields [packed] => <ctype>
;; Construct a struct ctype with given @var{fields}.  If @var{packed},
;; @code{#f} by default, is @code{#t}, create a packed structure.
;; @var{fields} is a list with entries of the form @code{(name type)} or
;; @code{(name type lenth)} where @code{name} is a symbol or @code{#f}
;; (for anonymous structs and unions), @code{type} is a @code{<ctype>}
;; object or a symbol for a base type and @code{length} is the length
;; of the associated bitfield.
;; @end deffn
(define* (cstruct fields #:optional packed?)
  "- Procedure: cstruct fields [packed] => ctype
     Construct a struct ctype with given FIELDS.  If PACKED, ‘#f’ by
     default, is ‘#t’, create a packed structure.  FIELDS is a list with
     entries of the form ‘(name type)’ or ‘(name type lenth)’ where
     ‘name’ is a symbol or ‘#f’ (for anonymous structs and unions),
     ‘type’ is a ‘<ctype>’ object or a symbol for a base type and
     ‘length’ is the length of the associated bitfield."
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
        (let* ((ral (reverse ral))
               (lkup (case-lambda ((sym) (assq-ref ral sym))
                                  (() (map car ral)))))
          (%make-ctype (incr-bit-size 0 sal ssz) sal 'struct
                       (%make-cstruct (reverse cfl) lkup) #f))
        (match (car sfl)
          ((name type)                  ; non-bitfield
           (let* ((type (cond ((symbol? type) (cbase type))
                              ((ctype? type) type)
                              (else (error "cstruct: bad type" type))))
                  (fsz (ctype-size type))
                  (fal (if packed? 1 (ctype-align type)))
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
                  (fal (if packed? 1 (ctype-align type)))
                  (mty (ctype-info type))
                  (sx? (mtype-signed? mty))
                  ;; order is critical here:
                  (cio (bf-offset width fal ssz)) ; ci struct offset
                  (ssz (incr-bit-size width fal ssz))  ; moved
                  (bfo (- (* 8 ssz) width (* 8 cio)))) ; offset wrt ci
             (if name
                 (let* ((bf (%make-cbitfield mty bfo width sx?))
                        (ty (%make-ctype fsz fal 'bitfield bf #f))
                        (cf (%make-cfield name ty cio)))
                   (loop (cons cf cfl) (acons name cf ral)
                         ssz (max fal sal) (cdr sfl)))
                 (loop cfl ral ssz sal (cdr sfl)))))
          (otherwize
           (sferr "cstruct: bad form: ~s" (car sfl))
           (error "yuck"))))))


;; @deffn {Procedure} cunion fields => <ctype>
;; Construct a union ctype with given @var{fields}.
;; See @emph{cstruct} for a description of the @var{fields} argument.
;; @end deffn
(define (cunion fields)
  "- Procedure: cunion fields
     Construct a union ctype with given FIELDS.  See _cstruct_ for a
     description of the FIELDS argument."
  (let loop ((cfl '()) (ral '()) (ssz 0) (sal 0) (sfl fields))
    (if (null? sfl)
        (let* ((ral (reverse ral)) (lkup (lambda (sym) (assq-ref ral sym))))
          (%make-ctype (incr-size 0 sal ssz) sal 'union
                       (%make-cunion (reverse cfl) lkup) #f))
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


;; @deffn {Procedure} carray type n => <ctype>
;; Create an array of @var{type} with @var{length}.
;; If @var{length} is zero, the array length is unbounded: it's length
;; can be specified as argument to @code{make-cdata}.
;; @end deffn
(define (carray type n)
  "- Procedure: carray type n
     Create an array of TYPE with LENGTH.  If LENGTH is zero, the array
     length is unbounded: it's length can be specified as argument to
     ‘make-cdata’."
  (assert-ctype 'carray type)
  (%make-ctype (* n (ctype-size type)) (ctype-align type)
               'array (%make-carray type n) #f))


;; @deffn {Procedure} cenum enum-list [packed] => <ctype>
;; @var{enum-list} is a list of name or name-value pairs
;; @example
;; (cenum '((a 1) b (c 4))
;; @end example
;; If @var{packed} is @code{#t} the size will be smallest that can hold it,
;; as if defined in C with @code{__attribute__((packed))}.
;; @end deffn
(define* (cenum enum-list #:optional packed)
  "- Procedure: cenum enum-list [packed] => <ctype>
     ENUM-LIST is a list of name or name-value pairs
          (cenum '((a 1) b (c 4))
     If PACKED is ‘#t’ the size will be smallest that can hold it, as if
     defined in C with ‘__attribute__((packed))’."
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
  (let loop ((snl '()) (nxt 0) (enl enum-list))
    (if (null? enl)
        (let* ((mx (cdar snl)) (snl (reverse snl)) (mn (cdar snl))
               (nsl (map (lambda (p) (cons (cdr p) (car p))) snl))
               (mtype (if packed (short-mtype mn mx) (mtypeof-basetype 'int)))
               (symf (lambda (num) (assq-ref nsl num)))
               (numf (lambda (sym) (assq-ref snl sym))))
          (%make-ctype (sizeof-mtype mtype) (alignof-mtype mtype)
                       'enum (%make-cenum mtype symf numf (map car snl)) #f))
        (match (car enl)
          (`(,n ,v) (loop (acons n v snl) (1+ v) (cdr enl)))
          ((? symbol? n) (loop (acons n nxt snl) (1+ nxt) (cdr enl)))
          (((? symbol? n)) (loop (acons n nxt snl) (1+ nxt) (cdr enl)))
          (_ (error "cenum: bad enum def'n"))))))

;; @deffn {Procedure} cfunction proc->ptr ptr->proc [variadic?] => <ctype>
;; Generate a C function type to be used with @code{cpointer}.  You must
;; pass the @var{wrapper} and @var{unwrapper} procedures that convert a
;; procedure to a pointer, and pointer to procedure, respectively.  The
;; optional argument @var{#:variadic}, if @code{#t},  indicates the function
;; uses variadic arguments.  For this case (I need to add documention).
;; Here is an example:
;; @example
;; (define (f-proc->ptr proc)
;;   (ffi:procedure->pointer ffi:void proc (list)))
;; (define (f-ptr->proc fptr)
;;   (ffi:pointer->procedure ffi:void fptr (list)))
;; (define ftype (cpointer (cfunction f-proc->ptr f-ptr->proc)))
;; @end example
;; @end deffn
(define* (cfunction proc->ptr ptr->proc #:optional variadic?)
  "- Procedure: cfunction proc->ptr ptr->proc [variadic?] => <ctype>
     Generate a C function type to be used with ‘cpointer’.  You must
     pass the WRAPPER and UNWRAPPER procedures that convert a procedure
     to a pointer, and pointer to procedure, respectively.  The optional
     argument #:VARIADIC, if ‘#t’, indicates the function uses variadic
     arguments.  For this case (I need to add documention).  Here is an
     example:
          (define (f-proc->ptr proc)
            (ffi:procedure->pointer ffi:void proc (list)))
          (define (f-ptr->proc fptr)
            (ffi:pointer->procedure ffi:void fptr (list)))
          (define ftype (cpointer (cfunction f-proc->ptr f-ptr->proc)))"
  (let ((type (cbase 'void*)) (mtype (mtypeof-basetype 'void*)))
    (%make-ctype (ctype-size type) (ctype-align type) 'function
                 (%make-cfunction proc->ptr ptr->proc variadic? mtype) #f)))

;; @deffn {Procedure} ctype-detag type ix tag => type ix
;; Follows @var{tag}.  For structs and unions, the tag is a symbolic
;; field name.  For arrays and pointers, the tag is a non-negative integer.
;; An integer tag applied to the pointer increments the pointer by the
;; associated number of elements referenced.
;; @end deffn
;; MAYBE JUST MAYBE make this return a list of (ix ct) (ix ct) ...
;; 
(define (ctype-detag ct ix tag)
  (assert-ctype 'ctype-detag ct) ;; not needed assuming stable mod
  (unless (integer? ix) (error "ctype-detag: expecting integer, got" ix))
  (let ((ti (ctype-info ct)))
    (case (ctype-kind ct)
      ((struct)
       (let ((fld ((cstruct-select ti) tag)))
         (unless fld (error "ctype: bad field " tag))
         (values (cfield-type fld) (+ ix (cfield-offset fld)))))
      ((union)
       (let ((fld ((cunion-select ti) tag)))
         (unless fld (error "ctype: bad field " tag))
         (values (cfield-type fld) (+ ix (cfield-offset fld)))))
      ((array)
       (unless (integer? tag) (error "bad array ref"))
       (let ((type (carray-type ti)))
         (values type (+ ix (* tag (ctype-size type))))))
      ((pointer)
       (error "ctype-detag: don't call on me for a pointer dereference"))
      (else (error "bad tag:" tag)))))

;; @deffn {Procedure} ctype-sel type ix [tag ...] => ((ix . ct) (ix . ct) ...)
;; This generate a list of (offset, type) pairs for a type.  The result is
;; used to create getters and setter for foreign machine architectures.
;; See @emph{make-cdata-getter} and @emph{make-cdata-setter}.
;; @end deffn
(define (ctype-sel type ix . tags)
  "- Procedure: ctype-sel type ix [tag ...] => ((ix . ct) (ix . ct) ...)
     This generate a list of (offset, type) pairs for a type.  The
     result is used to create getters and setter for foreign machine
     architectures.  See _make-cdata-getter_ and _make-cdata-setter_."
  (assert-ctype 'ctype-sel type)
  (let loop ((res '()) (ct type) (ix 0) (tags tags))
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
         (else (error "ctype-sel: bad tag for pointer")))))
     (else
      (call-with-values (lambda () (ctype-detag ct ix (car tags)))
        (lambda (ct ix)
          (loop res ct ix (cdr tags))))))))


;; @deffn {Procedure} ctype-equal? a b => #t|#f
;; This predicate assesses equality of it's arguments.
;; Two types are considered equal if they have the same size,
;; alignment, kind, and eqivalent kind-specific properties.
;; For base types, the symbolic mtype must be equal; this includes
;; size, integer versus float, and signed versus unsigned.
;; For struct and union kinds, the names and types of all fields
;; must be equal.
;; @* WARNING: Currently equality of pointers uses @code{eq?} (with
;; forced promises).  Fixme is to create algorithm to chase down pointers
;; without infinite search.
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
                ((eq? at bt))
                (else #f))))
            ((array)
             (and (eqv? (carray-length a) (carray-length b))
                  (ctype-equal? (carray-type a) (carray-type b))))
            ((enum)
             (and (eq? (cenum-mtype a) (cenum-mtype b))
                  (equal? (cenum-syml a) (cenum-syml b))))
            ((function)
             (and
              (eq? (cfunction-variadic? a) (cfunction-variadic? b))
              (eq? (cfunction-ptr-mtype a) (cfunction-ptr-mtype b))
              (eq? (cfunction-proc->ptr a) (cfunction-proc->ptr b))
              ;; HARD TO DO
              #t))
            (else #f)))))
    (cond
     ((or (not (ctype? a)) (not (ctype? b))) #f)
     ((not (eq? (ctype-kind a) (ctype-kind b))) #f)
     ((not (eqv? (ctype-size a) (ctype-size b))) #f)
     ((not (eqv? (ctype-align a) (ctype-align b))) #f)
     (else (cinfo-equal? (ctype-kind a) (ctype-info a) (ctype-info b))))))

;; @deffn {Procedure} name-ctype name type -> <ctype>
;; Create a new named version of the type.  The name is useful when the type
;; is printed.  This procedure does not mutate: a new type object is created.
;; If a specific type is used by multiple names the names can share
;; the underlying type guts.  The following generates two named types.
;; @example
;; (define raw (cstruct '((a 'int) (b 'double))))
;; (define foo_t (name-ctype 'foo_t raw))
;; (define struct-foo (name-ctype 'struct-foo raw))
;; @end example
;; These types are equal:
;; @example
;; (ctype-equal? foo_t struct-foo) => #t
;; @end example
;; @end deffn
(define (name-ctype name type)
  "- Procedure: name-ctype name type -> <ctype>
     Create a new named version of the type.  The name is useful when
     the type is printed.  This procedure does not mutate: a new type
     object is created.  If a specific type is used by multiple names
     the names can share the underlying type guts.  The following
     generates two named types.
          (define raw (cstruct '((a 'int) (b 'double))))
          (define foo_t (name-ctype 'foo_t raw))
          (define struct-foo (name-ctype 'struct-foo raw))
     These types are equal:
          (ctype-equal? foo_t struct-foo) => #t"
  (%make-ctype (ctype-size type) (ctype-align type)
               (ctype-kind type) (ctype-info type)
               name))


;; @deffn {Procedure} make-cdata type [value]
;; Generate a @emph{cdata} object of type @var{type} with optional @var{value}.
;; If @var{value} is not provided, the object is zeroed.
;; As a special case, a positive integer arg to a zero-sized array type will
;; allocate storage for that many items, associating it with an array type of
;; that size.
;; @end deffn
(define* (make-cdata type #:optional value)
  "- Procedure: make-cdata type [value]
     Generate a _cdata_ object of type TYPE with optional VALUE.  If
     VALUE is not provided, the object is zeroed.  As a special case, a
     positive integer arg to a zero-sized array type will allocate
     storage for that many items, associating it with an array type of
     that size."
  (define (make-data type value)
    (let* ((data (%make-cdata (make-bytevector (ctype-size type) 0) 0 type #f)))
      (if value (cdata-set! data value))
      data))
  (assert-ctype 'make-cdata type)
  (case (ctype-kind type)
    ((array)
     (let* ((ca (ctype-info type)) (ln (carray-length ca)))
       (cond
        ((zero? ln)
         (unless (and (integer? value) (positive? value))
           (error "make-cdata: zero sized array type"))
         (let* ((et (carray-type ca)) (sz (ctype-size et))
                (bv (make-bytevector (* value sz) 0)))
           (%make-cdata bv 0 (carray et value) #f)))
        (else
         (make-data type value)))))
    (else (make-data type value))))

;; @deffn {Procedure} cdata-sel data tag ... => cdata
;; Return a new @code{cdata} object representing the associated selection.
;; Note this is different from @code{cdata-ref}: it always returns a cdata
;; object.  For example,
;; @example
;; > (define t1 (cstruct '((a int) (b double))))
;; > (define d1 (make-cdata t1))
;; > (cdata-set! d1 42 'a)
;; > (cdata-sel d1 'a)
;; $1 = #<cdata s32le 0x77bbf8e52260>
;; > (cdata-ref $1)
;; $2 = 42
;; @end example
;; @end deffn
(define (cdata-sel data . tags)
  "- Procedure: cdata-sel data tag ... => cdata
     Return a new ‘cdata’ object representing the associated selection.
     Note this is different from ‘cdata-ref’: it always returns a cdata
     object.  For example,
          > (define t1 (cstruct '((a int) (b double))))
          > (define d1 (make-cdata t1))
          > (cdata-set! d1 42 'a)
          > (cdata-sel d1 'a)
          $1 = #<cdata s32le 0x77bbf8e52260>
          > (cdata-ref $1)
          $2 = 42"
  (assert-cdata 'cdata-sel data)
  (if (null? tags) data
      (let loop ((bv (cdata-bv data)) (ix (cdata-ix data)) (ct (cdata-ct data))
                 (tags tags))
        (cond
         ((null? tags)
          (%make-cdata bv ix ct #f))
         ((eq? 'pointer (ctype-kind ct))
          (if (eq? 'void (cpointer-type (ctype-info ct)))
              (error "cdata-sel: attempt to deference void*"))
          (let* ((tag (car tags))
                 (cptr (ctype-info ct))
                 (elty (cpointer-type cptr))
                 (elsz (ctype-size elty))
                 (addr (let ((addr (mtype-bv-ref (cpointer-mtype cptr) bv ix)))
                         (cond ((eq? '* tag) addr)
                               ((integer? tag) (+ addr (* elsz tag)))
                               (else (error "cdata-sel: bad tag:" tag)))))
                 (eptr (make-pointer addr)))
            (cond
             ((zero? addr)
              (error "cdata-sel: attempt to dereference null pointer"))
             ((eq? 'function (ctype-kind elty))
              (loop bv ix elty (cdr tags)))
             (else
              (loop (pointer->bytevector eptr elsz) 0 elty (cdr tags))))))
         (else
          (call-with-values (lambda () (ctype-detag ct ix (car tags)))
            (lambda (ct ix) (loop bv ix ct (cdr tags)))))))))

;; @deffn {Procedure} Xcdata-ref bv ix ct -> value
;; Reference a deconstructed cdata object. See @emph{cdata-ref}.
;; @end deffn
(define (Xcdata-ref bv ix ct)
  "- Procedure: Xcdata-ref bv ix ct -> value
     Reference a deconstructed cdata object.  See _cdata-ref_."
  (let* ((ti (ctype-info ct)))
    (case (ctype-kind ct)
      ((base)
       (mtype-bv-ref ti bv ix))
      ((pointer)
       (make-pointer (mtype-bv-ref (cpointer-mtype ti) bv ix)))
      ((bitfield)
       (let* ((mt (cbitfield-mtype ti)) (sh (cbitfield-shift ti))
              (wd (cbitfield-width ti)) (sx (cbitfield-signed? ti))
              (sm (expt 2 (1- wd)))
              (v (bit-extract (mtype-bv-ref mt bv ix) sh (+ sh wd))))
         (if (and sx (logbit? (1- wd) v)) (- (logand v (1- sm)) sm) v)))
      ((enum)
       (let* ((info (ctype-info ct))
              (mtype (cenum-mtype info))
              (symf (cenum-symf info)))
         (symf (mtype-bv-ref mtype bv ix))))
      ((array struct union) (%make-cdata bv ix ct #f))
      ((function)
       (let* ((ti (ctype-info ct))
              (mtype (cfunction-ptr-mtype ti))
              (addr (mtype-bv-ref mtype bv ix))
              (ptr->proc (cfunction-ptr->proc ti)))
         (if (zero? addr) (error "cdata-ref: null function address"))
         (ptr->proc (make-pointer addr))))
      (else (error "cdata-ref: giving up")))))

;; @deffn {Procedure} Xcdata-set! bv ix ct value
;; Set the value of a deconstructed cdata object, where @var{bv}, @var{ix}
;; and @var{ct} are extracted from a cdata objerct. See @emph{cdata-set!}.
;; @end deffn
(define (Xcdata-set! bv ix ct value)
  "- Procedure: Xcdata-set! bv ix ct value
     Set the value of a deconstructed cdata object, where BV, IX and CT
     are extracted from a cdata objerct.  See _cdata-set!_."
  (let* ()
    (if (cdata? value)
        (let ((sz (ctype-size ct)))
          (unless (ctype-equal? (cdata-ct value) ct)
            (error "cdata-set!: bad arg:" value))
          (bytevector-copy! (cdata-bv value) (cdata-ix value) bv ix sz))
        (case (ctype-kind ct)
          ((base)
           (mtype-bv-set! (ctype-info ct) bv ix value))
          ((pointer)
           (let* ((pi (ctype-info ct))
                  (pt (cpointer-type pi))
                  (mtype (cpointer-mtype pi)))
             (cond
              ((pointer? value)
               (mtype-bv-set! mtype bv ix (pointer-address value)))
              ((integer? value)
               (mtype-bv-set! mtype bv ix value))
              ((string? value)
               (mtype-bv-set! mtype bv ix
                              (pointer-address (string->pointer value))))
              ((procedure? value)
               (unless (eq? (ctype-kind pt) 'function)
                 (error "cdata: expecting pointer to function, got" pt))
               (mtype-bv-set! mtype bv ix
                              (pointer-address
                               ((cfunction-proc->ptr (ctype-info pt)) value))))
              (else (error "cdata-set!: bad arg:" value)))))
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
               (mtype-bv-set! mtype bv ix ((cenum-numf info) value)))
              (else
               (error "cdata-set! bad value arg: ~s" value)))))
          ((array)
           (let* ((ti (ctype-info ct))
                  (at (carray-type ti))
                  (al (carray-length ti)))
             (if (and (array? value)
                      (eqv? 1 (array-rank value))
                      (eq? 'base (ctype-kind at))
                      (= (array-length value) al))
                 (let* ((mt (ctype-info at)) (sz (sizeof-mtype mt)))
                   (do ((i 0 (1+ i)) (ix ix (+ ix sz)))
                       ((>= i al) (if #f #f))
                     (mtype-bv-set! mt bv ix (array-ref value i))))
                 (error "cdata-set!: can't set! this array value"))))
          ((function) (error "cdata-set!: can't set! function value"))
          ((struct) (error "cdata-set!: can't set! struct value"))
          ((union) (error "cdata-set!: can't set! union value"))
          (else (error "cdata-set!: bad arg 2" value))))))

;; @deffn {Procedure} cdata-ref data [tag ...] => value
;; Return the Scheme (scalar) slot value for selected @var{tag ...} with
;; respect to the cdata object @var{data}.
;; @example
;; (cdata-ref my-struct-value 'a 'b 'c))
;; @end example
;; This procedure returns Guile values for cdata kinds @emph{base},
;; @emph{pointer} and @emph{procedure}.   For other cases, a @emph{cdata}
;; object is returned.  If you always want a cdata object, use @code{cdata-sel}.
;; @end deffn
(define (cdata-ref data . tags)
  "- Procedure: cdata-ref data [tag ...] => value
     Return the Scheme (scalar) slot value for selected TAG ... with
     respect to the cdata object DATA.
          (cdata-ref my-struct-value 'a 'b 'c))
     This procedure returns Guile values for cdata kinds _base_,
     _pointer_ and _procedure_.  For other cases, a _cdata_ object is
     returned.  If you always want a cdata object, use ‘cdata-sel’."
  (assert-cdata 'cdata-ref data)
  (let ((data (apply cdata-sel data tags)))
    (Xcdata-ref (cdata-bv data) (cdata-ix data) (cdata-ct data))))

;; @deffn {Procedure} cdata-set! data value [tag ...]
;; Set slot for selcted @var{tag ...} with respect to cdata @var{data} to
;; @var{value}.  Example:
;; @example
;; (cdata-set! my-struct-data 42 'a 'b 'c))
;; @end example
;; If @var{value} is a @code{<cdata>} object copy that, if types match.
;; @*The @var{value} argument can be a procedure when the associated ctype
;; is a pointer to function.
;; value.
;; @end deffn
(define (cdata-set! data value . tags)
  "- Procedure: cdata-set! data value [tag ...]
     Set slot for selcted TAG ... with respect to cdata DATA to VALUE.
     Example:
          (cdata-set! my-struct-data 42 'a 'b 'c))"
  (assert-cdata 'cdata-set! data)
  (let ((data (apply cdata-sel data tags)))
    (case (cdata-kind data)
      ((struct union)
       (unless (pair? value) (error "cdata-set!: bad arg:" value))
       (for-each
        (lambda (field) (cdata-set! data (cadr field) (car field)))
        value))
      (else
       (Xcdata-set! (cdata-bv data) (cdata-ix data) (cdata-ct data) value)))))

;; @deffn {Procedure} make-cdata/* type pointer
;; Make a cdata object from a pointer.   That is, instead of creating a
;; bytevector to hold the data use the memory at the pointer using
;; @code{pointer->bytevector}.
;; @* Maybe ccast can do this?
;; @end deffn
(define (make-cdata/* type pointer)
  (assert-ctype 'make-cdata/* type)
  (let* ((size (ctype-size type))
         (bvec (pointer->bytevector pointer size))
         (data (%make-cdata bvec 0 type #f)))
    data))

;; @deffn {Procedure} cdata-copy src) => <cdata>
;; Copy a data object (which might be a reference from another data object).
;; @end deffn
(define (cdata-copy data)
  "- Procedure: cdata-copy src) => <cdata>
     Copy a data object (which might be a reference from another data
     object)."
  (assert-cdata 'cdata-copy data)
  (let* ((bv (cdata-bv data))
         (ix (cdata-ix data))
         (ct (cdata-ct data))
         (sz (ctype-size ct))
         (bvdst (make-bytevector sz)))
    (bytevector-copy! bv ix bvdst 0 (ctype-size ct))
    (%make-cdata bvdst 0 ct #f)))
    
;; @deffn {Procedure} cdata& data => cdata
;; Generate a reference (i.e., cpointer) to the contents in the underlying
;; bytevector.
;; @end deffn
(define (cdata& data)
  "- Procedure: cdata& data => cdata
     Generate a reference (i.e., cpointer) to the contents in the
     underlying bytevector."
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
       (%make-cdata (pointer->bytevector pntr (ctype-size type)) 0 type #f)))))

;; @deffn {Procedure} cdata-kind data
;; Return the kind of @var{data}: pointer, base, struct, ...
;; @end deffn
(define (cdata-kind data)
  "- Procedure: cdata-kind data
     Return the kind of DATA: pointer, base, struct, ..."
  (assert-cdata 'cdata-kind data)
  (ctype-kind (cdata-ct data)))

;; @deffn {Procedure} cdata*-sel data [tag ...] => value
;; Shortcut for @code{(cdata-sel (cdata* data) tag ...)}
;; @end deffn
(define (cdata*-sel data . tags)
  "- Procedure: cdata*-sel data [tag ...] => value
     Shortcut for ‘(cdata-sel (cdata* data) tag ...)’"
  (apply cdata-sel data '* tags))

;; @deffn {Procedure} cdata*-ref data [tag ...] => value
;; Shortcut for @code{(cdata-ref (cdata* data) tag ...)}
;; @end deffn
(define (cdata*-ref data . tags)
  "- Procedure: cdata*-ref data [tag ...] => value
     Shortcut for ‘(cdata-ref (cdata* data) tag ...)’"
  (apply cdata-ref data '* tags))

;; @deffn {Procedure} cdata&-sel data [tag ...] => value
;; Provide a pointer ctype for the address of the selected value.
;; @end deffn
(define (cdata&-sel data . tags)
  "- Procedure: cdata&-sel data [tag ...] => value
     Provide a pointer ctype for the address of the selected value."
  (assert-cdata 'cdata&-sel data)
  (let* ((data (apply cdata-sel data tags))
         (bptr (bytevector->pointer (cdata-bv data)))
         (addr (+ (pointer-address bptr) (cdata-ix data)))
         (type (cpointer (cdata-ct data))))
    (make-cdata type addr)))

;; @deffn {Procedure} cdata&-ref data [tag ...] => value
;; Provide a (Guile) pointer to the selected value.
;; @end deffn
(define (cdata&-ref data . tags)
  "- Procedure: cdata&-ref data [tag ...] => value
     Provide a (Guile) pointer to the selected value."
  (assert-cdata 'cdata&-ref data)
  (let* ((data (apply cdata-sel data tags))
         (bptr (bytevector->pointer (cdata-bv data)))
         (addr (+ (pointer-address bptr) (cdata-ix data))))
    (make-pointer addr)))

;; @deffn {Procedure} ccast type data [do-check] => <cdata>
;; need to be able to cast array to pointer
;; @example
;; (ccast Target* val)
;; @end example
;; @end deffn
(define* (ccast type data #:key do-check)
  "- Procedure: ccast type data [do-check] => <cdata>
     need to be able to cast array to pointer
          (ccast Target* val)"
  (assert-ctype 'cdata-cast type)
  (assert-cdata 'cdata-cast data)
  (define (type-miss)
    (error "ccast: incompatible type:" (list (cdata-ct data) type)))
  (define (type-check ft tt)
    (when (and do-check (ctype-equal? ft tt)) (type-miss)))
  (let ((bv (cdata-bv data)) (ix (cdata-ix data)) (ct (cdata-ct data)))
    (case (ctype-kind ct)
      ((base) (make-cdata type (cdata-ref data)))
      ((pointer) (%make-cdata bv ix type #f))
      ((array)
       (case (ctype-kind type)
         ((pointer)
          (let* ((array (ctype-info ct))
                 (atype (carray-type array))
                 (ptype (cpointer-type (ctype-info type))))
            (type-check (carray-type (ctype-info ct))
                        (cpointer-type (ctype-info type)))
            (%make-cdata bv ix type #f)))
         (else (type-miss))))
      (else (type-miss)))))

;;.@deffn {Procedure} Xloop sel offset data tags
;; Defined internally for @code{make-cdata-getter} and @code{make-cdata-setter}.
;; @var{offset} is a base address offset for pointer dereferencing. 
;;.@end deffn 
(define (Xloop sel offset data tags) ;; => bv ix ct
  (let loop ((bv (cdata-bv data)) (ix (cdata-ix data)) (ct (cdata-ct data))
             (head (car sel)) (tail (cdr sel)))
    (cond
     ((null? tail)
      (let lp ((ix (+ ix (car head))) (ct (cdr head)) (tags tags))
        (if (null? tags) (values bv ix ct)
            (call-with-values (lambda () (ctype-detag ct ix (car tags)))
              (lambda (ct ix) (lp ix ct (cdr tags)))))))
     ((eq? 'pointer (ctype-kind (cdr head)))
      (let* ((px (car head)) (pt (cdr head))
             (dty (cpointer-type (ctype-info pt)))
             (mtype (cpointer-mtype (ctype-info pt)))
             (addr (mtype-bv-ref mtype bv (+ ix px)))
             (dptr (make-pointer (+ addr offset)))
             (bvec (pointer->bytevector dptr (ctype-size dty))))
        (loop bvec 0 dty (car tail) (cdr tail))))
     (else
      (error "cdata-getter/setter: expecting pointer, bad tag?")))))

;; @deffn {Procedure} make-cdata-getter sel [offset] => lambda
;; Genererate a procedure that given a cdata object will fetch the value
;; at indicated by the @var{sel}, generated by @code{ctype-sel}.
;; The procedure takes one argument: @code{(proc data [tag ...])}.
;; Pointer dereference tags (@code{'*'}) are not allowed.
;; The optional @var{offset} argument (default 0), is used for cross
;; target use: it is the offset of the address in the host context.
;; @end deffn 
(define* (make-cdata-getter sel #:optional (offset 0))
  "- Procedure: make-cdata-getter sel [offset] => lambda
     Genererate a procedure that given a cdata object will fetch the
     value at indicated by the SEL, generated by ‘ctype-sel’.  The
     procedure takes one argument: ‘(proc data [tag ...])’.  Pointer
     dereference tags (‘'*'’) are not allowed.  The optional OFFSET
     argument (default 0), is used for cross target use: it is the
     offset of the address in the host context."
  (unless (and (pair? sel) (pair? (cdr sel)))
    (error "make-cdata-getter: bad SEL arg"))
  (unless (integer? offset)
    (error "make-cdata-setter: bad OFFSET arg"))
  (lambda (data . tags)
    (unless (cdata? data) (error "cdata-getter: bad DATA arg"))
    (call-with-values (lambda () (Xloop sel offset data tags)) Xcdata-ref)))

;; @deffn {Procedure} make-cdata-setter sel [offset] => lambda
;; Genererate a procedure that given a cdata object will set the value
;; at the offset given the selector, generated by @code{ctype-sel}.
;; The procedure takes two arguments: @code{(proc data value [tag ...])}.
;; Pointer dereference tags (@code{'*'}) are not allowed.
;; The optional @var{offset} argument (default 0), is used for cross
;; target use: it is the offset of the address in the host context.
;; @end deffn 
(define* (make-cdata-setter sel #:optional (offset 0))
  "- Procedure: make-cdata-setter sel [offset] => lambda
     Genererate a procedure that given a cdata object will set the value
     at the offset given the selector, generated by ‘ctype-sel’.  The
     procedure takes two arguments: ‘(proc data value [tag ...])’.
     Pointer dereference tags (‘'*'’) are not allowed.  The optional
     OFFSET argument (default 0), is used for cross target use: it is
     the offset of the address in the host context."
  (unless (and (pair? sel) (pair? (cdr sel)))
    (error "make-cdata-setter: bad SEL arg"))
  (unless (integer? offset)
    (error "make-cdata-setter: bad OFFSET arg"))
  (lambda (data value . tags)
    (unless (cdata? data) (error "cdata-getter: bad DATA arg"))
    (call-with-values (lambda () (Xloop sel offset data tags))
      (lambda (bv ix ct) (Xcdata-set! bv ix ct value)))))

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
         (cond
          ((promise? (%cpointer-type info)) `(cpointer (delay ...)))
          ((eq? 'void (cpointer-type info)) `(cpointer 'void))
          ((ctype-name (%cpointer-type info)) => (lambda (n) `(cpointer ,n)))
          (else `(cpointer ,(cnvt (cpointer-type info))))))
        ((array)
         `(carray ,(cnvt (carray-type info)) ,(carray-length info)))
        ((enum)
         (cond
          ((ctype-name type) => identity)
          (else `(enum ...))))
        ((function)
         (cond
          ((ctype-name type) => identity)
          (else `(function ...))))
        (else (error "pretty-print-ctype: needs work:" (ctype-kind type))))))
  (pretty-print (cnvt type) port))


;; --- not sure about this -----------------------------------------------------

(define (mtype->ffi mtype)
  (or
   (assq-ref
    `((s8 . ,int8)  (u8 . ,uint8)
      (s16le . ,int16) (s32le . ,int32) (s64le . ,int64)
      (u16le . ,uint16) (u32le . ,uint32) (u64le . ,uint64)
      (f32le . ,float) (f64le . ,double)
      (s16be . ,int16) (s32be . ,int32) (s64be . ,int64)
      (u16be . ,uint16) (u32be . ,uint32) (u64be . ,uint64)
      (f32be . ,float) (f64be . ,double)
      (u128le . #f) (f16le . #f) (f128le . #f) (s128be . #f)
      (i128le . #f) (u128be . #f) (f16be . #f) (f128be . #f)
      ;;(s16 . ,int16) (s32 . ,int32) (s64 . ,int64)
      ;;(u16 . ,uint16) (u32 . ,uint32) (u64 . ,uint64)
      ;;(f32 . ,float) (f64 . ,double)
      ;;(s128 . #f) (u128 . #f) (f16 . #f) (f128 . #f)
      )
    mtype)
   (error "mtype->ffi: bad mtype")))

;; @deffn {Procedure} ctype->ffi type
;; Generate a argument spec for Guile's ffi interface.
;; Example:
;; @example
;; (ctype->ffi (cpointer (cbase int))) => '*
;; @end example
;; @end deffn
(define (ctype->ffi type)
  "- Procedure: ctype->ffi type
     Generate a argument spec for Guile's ffi interface.  Example:
          (ctype->ffi (cpointer (cbase int))) => '*"
  (assert-ctype 'ctype->ffi type)
  (let ((info (ctype-info type)))
    (case (ctype-kind type)
      ((base) (mtype->ffi info))
      ((struct union)
       (make-list (/ (ctype-size type) (ctype-align type))
                  (case (ctype-align type)
                    ((1) int8) ((2) int16) ((4) int32) ((8) int64))))
      ((pointer array function) '*)
      (else (error "ctype->ffi: unsupported:" (ctype-kind type))))))

;; @deffn {Procedure} cdata-arg->number arg
;; Convert an argument to numeric form for a ffi procedure call.
;; This will reference a cdata object or pass a number through.
;; @end deffn
(define (cdata-arg->number arg)
  "- Procedure: arg->number arg
     Convert an argument to numeric form for a ffi procedure call.  This
     will reference a cdata object or pass a number through."
  (cond ((number? arg) arg)
        ((cdata? arg) (cdata-ref arg))
        (else (error "cdata-arg->number: bad arg:" arg))))
(define unwrap-number cdata-arg->number)

;; @deffn {Procedure} cdata-arg->pointer arg
;; Convert an argument to a Guile pointer for a ffi procedure call.
;; This will reference a cdata object or pass a number through.
;; If the argument is a function, it will attempt to convert that
;; to a pointer via @code{procedure->pointer} if given the function
;; pointer type @var{hint}.
;; @end deffn
(define* (cdata-arg->pointer arg #:optional hint)
  "- Procedure: arg->pointer arg
     Convert an argument to a Guile pointer for a ffi procedure call.
     This will reference a cdata object or pass a number through.  If
     the argument is a function, it will attempt to convert that to a
     pointer via ‘procedure->pointer’ if given the function pointer type
     HINT."
  (cond ((pointer? arg) arg)
        ((string? arg) (string->pointer arg))
        ((equal? 0 arg) %null-pointer)
        ((cdata? arg)
         (case (cdata-kind arg)
           ((pointer) (cdata-ref arg))
           ((array struct union) (cdata&-ref arg))
           (else (error "cdata-arg->pointer; bad arg:" arg))))
        ((and (procedure? arg) (ctype? hint))
         (let* ((info (ctype-info hint))
                (func (case (ctype-kind hint)
                        ((function) info)
                        ((pointer) (ctype-info (cpointer-type info)))
                        (else (error "not ok")))))
           ((cfunction-proc->ptr func) arg)))
        (else (error "cdata-arg->pointer: bad arg:" arg))))
(define unwrap-pointer cdata-arg->pointer)
(define unwrap-array cdata-arg->pointer)

(define NULL %null-pointer)

;; @deffn {Procedure} NULL? arg
;; Check if argument is null Guile pointer, or a cdata form of the same.
;; @end deffn
(define (NULL? arg)
  "- Procedure: NULL? arg
     Check if argument is null Guile pointer, or a cdata form of the
     same."
  (equal? (if (cdata? arg) (cdata-ref arg) arg) %null-pointer))


;; --- deprecated --------------------------------------------------------------

;; --- c99 support -------------------------------------------------------------

#|
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
|#

;; --- last line ---
