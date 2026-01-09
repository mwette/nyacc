;;; nyacc/foreign/arch-info.scm - map c types to machine arch' types

;; Copyright (C) 2020-2025 Matthew Wette
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

;; The architectures names should really use targets but I think.
;; Currently, this is sync'd with <march>-*-linux.
;; For example, chars on linux are signed but on some other OSs they
;; are unsigned.  I may make these like `x86_64-linux` and update the
;; arch selection routines

;; 1) We have defined arch's i686 ppc riscv32 riscv64 sparc x86_64
;; 2) Can we remove non-le/be mtypes?
;; 3) The complex type c32le means each part (real, imag) are each 32 bits.


;;; Code:

(define-module (nyacc foreign arch-info)
  #:export (lookup-arch
            *arch* with-arch native-arch *arch-map*
            arch-cbase-map set-arch-cbase-map!
            arch-name arch-endianness
            mtype-size mtype-alignment mtype-endianness
            sizeof-basetype alignof-basetype
            mtypeof-basetype sizeof-mtype alignof-mtype
            base-type-name-list base-type-symbol-list
            c-strname->symname c-symname->strname
            mtype-bv-ref mtype-bv-set!
            bv-c32-ref bv-c32-set! bv-c64-ref bv-c64-set!
            bv-s128-ref bv-s128-set! bv-u128-ref bv-u128-set!
            bv-f96-ref bv-f96-set! bv-c96-ref bv-c96-set!
            mtype-signed? mtype-noendian)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors))

(use-modules (ice-9 pretty-print))
(define (pperr exp) (pretty-print exp (current-error-port)))
(define (sferr fmt . args) (apply simple-format #t fmt args))

(define (c-strname->symname strname)
  (string->symbol (string-map (lambda (c) (if (char=? #\space c) #\- c))
                              strname)))

(define (c-symname->strname symname)
  (string-map (lambda (c) (if (char=? #\space c) #\- c))
              (symbol->string symname)))

(define-public symname->strname c-symname->strname)
(define-public strname->symname c-strname->symname)

;; @defty {Record} <arch-info>
;; @table @code
;; @item name
;; a symbolic name (e.g., @code{'aarch64'})
;; @item regbits
;; usually 64 or 32
;; @item endianness
;; either 'little or 'big.
;; @item mtype-map
;; an alist mapping symbolic C types (e.g. unsigned-int) to symbolic
;; machine type (e.g., i32le)
;; @item align-map
;; an alist mapping machine type (e.g., i32le) to alignment (e.g., 4)
;; @end table
;; @end defty
(define-record-type <arch-info>
  (%make-arch-info name regbits endianness mtype-map align-map)
  arch-info?
  (name arch-name)                      ; e.g., "x86_64"
  (regbits arch-regbits)                ; register size in bits
  (endianness arch-endianness)          ; 'little or 'big
  (mtype-map arch-mtype-map)            ; c-ish name => f32l3, u8, ...
  (align-map arch-align-map))           ; f32, u8 => alignment
(export <arch-info>)

(define (make-arch-info name regbits endianness mtype-map align-map)
  (unless (eq? 'void* (caar mtype-map)) (error "expecting void*"))
  (%make-arch-info name regbits endianness mtype-map align-map))

(define sizeof-mtype-map
  '((s8 . 1) (u8 . 1)
    (s16 . 2) (s32 . 4) (s64 . 8) (s128 . 16)
    (u16 . 2) (u32 . 4) (u64 . 8) (u128 . 16)
    (f16 . 2) (f32 . 4) (f64 . 8) (f128 . 16)
    (s16le . 2) (s32le . 4) (s64le . 8) (s128le . 16)
    (u16le . 2) (u32le . 4) (u64le . 8) (u128le . 16)
    (f16le . 2) (f32le . 4) (f64le . 8) (f128le . 16)
    (s16be . 2) (s32be . 4) (s64be . 8) (s128be . 16)
    (u16be . 2) (u32be . 4) (u64be . 8) (u128be . 16)
    (f16be . 2) (f32be . 4) (f64be . 8) (f128be . 16)
    (c32le . 8) (c64le . 16) (c32be . 8) (c64be . 16)))

(define endianness-mtype-map
  (let ((le 'little) (be 'big))
    `((s8 . any) (u8 . any)
      (s16le . ,le) (s32le . ,le) (s64le . ,le) (s128le . .le)
      (u16le . ,le) (u32le . ,le) (u64le . ,le) (u128le . ,le)
      (f16le . ,le) (f32le . ,le) (f64le . ,le) (f128le . ,le)
      (s16be . ,be) (s32be . ,be) (s64be . ,be) (s128be . ,be)
      (u16be . ,be) (u32be . ,be) (u64be . ,be) (u128be . ,be)
      (f16be . ,be) (f32be . ,be) (f64be . ,be) (f128be . ,be)
      (c32le . ,le) (c64le . ,le) (c32be . ,be) (c64be . ,be))))

(define alignof-mtype-map/natural
  (map (lambda (p)
         (case (car p)
           ((c32le c32be) (cons (car p) 4))
           ((c64le c64be) (cons (car p) 8))
           (else p)))
       sizeof-mtype-map))

(define base-type-name-list
  '("void*"
    "char" "signed char" "unsigned char"
    "short" "unsigned short"
    "int" "unsigned"
    "long" "unsigned long"
    "long long" "unsigned long long"
    "float" "double"
    "int8_t" "uint8_t" "int16_t" "uint16_t"
    "int32_t" "uint32_t" "int64_t" "uint64_t"
    "size_t" "ssize_t" "ptrdiff_t"
    "intptr_t" "uintptr_t"
    "_Bool" "bool"
    "wchar_t" "char16_t" "char32_t"
    "long double" "_Float16" "_Float128"
    "float _Complex" "double _Complex" "long double _Complex"
    "__int128" "unsigned __int128"
    ;; sugar:
    "unsigned int"))

(define base-type-symbol-list
  (map strname->symname base-type-name-list))

(define *arch-map* (make-parameter '()))

(define (add-to-arch-map name arch)
  (*arch-map* (acons name arch (*arch-map*))))

(define (lookup-arch name)
  (assoc-ref (*arch-map*) name))

;; @deffn {Parameter} *arch*
;; Parameter set to global architecture record; initialized to native arch.
;; Currently supported machine architectures are @emph{avr}, @emph{i686},
;; @emph{powerpc32}, @emph{powerpc64}, @emph{riscv32}, @emph{riscv64},
;; @emph{sparc32}, @emph{sparc64} and @emph{x86_64}.
;; @end deffn
(define *arch* (make-parameter #f))

;; @deffn {Syntax} with-arch arch body ...
;; Evaluate @var{body} with arch set to @var{arch}, which can be an
;; @code{<arch>} object or a string (e.g., @code{"x86_64"}
;; @end deffn
(define-syntax-rule (with-arch arch body ...)
  (parameterize
      ((*arch* (let ((march (if (arch-info? arch) arch (lookup-arch arch))))
                 (unless march (error "with-arch: no such arch: " arch))
                 march)))
    body ...))

;; @deffn {Procedure} typeof-basetype base-type-name => 'f64
;; Return the machine base type for the c base type
;; @end deffn
(define (mtypeof-basetype base-type-name)
  (let ((name (cond ((symbol? base-type-name) base-type-name)
                    ((string? base-type-name) (strname->symname base-type-name))
                    (else (error "mtype-of-basetype: bad argument")))))
    (assoc-ref (arch-mtype-map (*arch*)) name)))

(define (mtype-size mtype)
  (assq-ref sizeof-mtype-map mtype))

(define (mtype-alignment mtype)
  (assq-ref (arch-align-map (*arch*)) mtype))

(define (mtype-endianness mtype)
  (assq-ref endianness-mtype-map mtype))

(define sizeof-mtype mtype-size)

(define alignof-mtype mtype-alignment)

(define-syntax be (identifier-syntax (endianness big)))
(define-syntax le (identifier-syntax (endianness little)))

;; @deffn {Procedure} sizeof-basetype name
;; @var{name} can be string (e.g., @code{"short"}) or
;; symbol (e.g., @code{short-int}).
;; Return the size in bytes of the basetype @var{type}, a string, based on
;; the global parameter @var{*arch*}.
;; @example
;; (sizeof-basetype "unsigned") => 4
;; @end example
;; @end deffn
(define (sizeof-basetype name)
  "- Procedure: sizeof-basetype name
     NAME can be string (e.g., ‘\"short\"’) or symbol (e.g., ‘short-int’).
     Return the size in bytes of the basetype TYPE, a string, based on
     the global parameter *ARCH*.
          (sizeof-basetype \"unsigned\") => 4"
  (let ((name (if (string? name) (c-strname->symname name) name))
        (arch (*arch*)))
    (and=> (assq-ref (arch-mtype-map arch) name)
           sizeof-mtype)))

;; @deffn {Procedure} alignof-basetype name
;; @var{name} can be string (e.g., @code{"short"}) or
;; symbol (e.g., @code{short-int}).
;; Return the alignment of the basetype @var{type-name} based on
;; the global parameter @var{*arch*}.
;; @end deffn
(define (alignof-basetype name)
  "- Procedure: alignof-basetype name
     NAME can be string (e.g., ‘\"short\"’) or symbol (e.g., ‘short-int’).
     Return the alignment of the basetype TYPE-NAME based on the global
     parameter *ARCH*."
  (let ((name (if (string? name) (strname->symname name) name))
        (arch (*arch*)))
    (and=> (assq-ref (arch-mtype-map arch) name)
           (lambda (type) (assq-ref (arch-align-map arch) type)))))

;; move to arch-info
(define (mtype-signed? mtype)
  (and (member mtype '(s8 s16 s32 s64 s16le s32le s64le s16be s32be s64be)) #t))

(define (bv-c32-ref bv ix en)
  (make-rectangular (bytevector-ieee-single-ref bv ix en)
                    (bytevector-ieee-single-ref bv (+ ix 4) en)))

(define (bv-c32-set! bv ix value en)
  (bytevector-ieee-single-set! bv ix (real-part value) en)
  (bytevector-ieee-single-set! bv (+ ix 4) (imag-part value) en))

(define (bv-c64-ref bv ix en)
  (make-rectangular (bytevector-ieee-double-ref bv ix en)
                    (bytevector-ieee-double-ref bv (+ ix 8) en)))

(define (bv-c64-set! bv ix value en)
  (bytevector-ieee-double-set! bv ix (real-part value) en)
  (bytevector-ieee-double-set! bv (+ ix 8) (imag-part value) en))

(define (bv-s128-ref bv ix en)
  (cond
   ((eq? en le)
    (+ (ash (bytevector-s64-ref bv (+ ix 8) le) 64)
       (bytevector-u64-ref bv ix le)))
   ((eq? en be)
    (+ (ash (bytevector-s64-ref bv ix be) 64)
       (bytevector-u64-ref bv (+ ix 8) be)))
   (else (error "bv-s128-ref: bad endianness"))))

(define (bv-s128-set! bv ix value en)
  (let ((hi (ash value -64)) (lo (logand value #xFFFFFFFFFFFFFFFF)))
    (cond
     ((eq? en le)
      (bytevector-s64-set! bv ix lo le)
      (bytevector-s64-set! bv (+ ix 2) hi le))
     ((eq? en be)
      (bytevector-s64-set! bv (+ ix 2) lo be)
      (bytevector-s64-set! bv ix hi be))
     (else (error "bv-s128-set!: bad endianness")))))

(define (bv-u128-ref bv ix en)
  (cond
   ((eq? en le)
    (+ (ash (bytevector-u64-ref bv (+ ix 8) le) 64)
       (bytevector-u64-ref bv ix le)))
   ((eq? en be)
    (+ (ash (bytevector-u64-ref bv ix be) 64)
       (bytevector-u64-ref bv (+ ix 8) be)))
   (else (error "bv-u128-ref: bad endianness"))))

(define (bv-u128-set! bv ix value en)
  (let ((hi (ash value -64)) (lo (logand value #xFFFFFFFFFFFFFFFF)))
    (cond
     ((eq? en le)
      (bytevector-u64-set! bv ix lo le)
      (bytevector-u64-set! bv (+ ix 2) hi le))
     ((eq? en be)
      (bytevector-u64-set! bv (+ ix 2) lo be)
      (bytevector-u64-set! bv ix hi be))
     (else (error "bv-u128-set!: bad endianness")))))

;; test
(define-public (bv-s32-ref bv ix en)
  (cond
   ((eq? en le)
    (+ (ash (bytevector-s16-ref bv (+ ix 2) le) 16)
       (bytevector-u16-ref bv ix le)))
   ((eq? en be)
    (+ (ash (bytevector-s16-ref bv ix be) 16)
       (bytevector-u16-ref bv (+ ix 2) be)))
   (else (error "bv-s16-ref: bad endianness"))))

(define-public (bv-s32-set! bv ix value en)
  (let ((hi (ash value -16)) (lo (logand value #xFFFF)))
    (cond
     ((eq? en le)
      (bytevector-s16-set! bv ix lo le)
      (bytevector-s16-set! bv (+ ix 2) hi le))
     ((eq? en be)
      (bytevector-s16-set! bv (+ ix 2) lo be)
      (bytevector-s16-set! bv ix hi be))
     (else (error "bv-s32-set!: bad endianness")))))

(define-public (bv-f96-ref bv ix en)
  (error "not implemented"))

(define-public (bv-f96-set! bv ix en)
  (error "not implemented"))

(define-public (bv-c96-ref bv ix en)
  (error "not implemented"))

(define-public (bv-c96-set! bv ix en)
  (error "not implemented"))

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
    ;;
    ((u16be) (bytevector-u16-ref bv ix be))
    ((s16be) (bytevector-s16-ref bv ix be))
    ((u32be) (bytevector-u32-ref bv ix be))
    ((s32be) (bytevector-s32-ref bv ix be))
    ((u64be) (bytevector-u64-ref bv ix be))
    ((s64be) (bytevector-s64-ref bv ix be))
    ((f32be) (bytevector-ieee-single-ref bv ix be))
    ((f64be) (bytevector-ieee-double-ref bv ix be))
    ;;
    ((s128le) (bv-s128-ref bv ix le))
    ((u128le) (bv-u128-ref bv ix le))
    ((s128be) (bv-s128-ref bv ix be))
    ((u128be) (bv-u128-ref bv ix be))
    ((c32le) (bv-c32-ref bv ix le))
    ((c64le) (bv-c64-ref bv ix le))
    ((c32be) (bv-c32-ref bv ix be))
    ((c64be) (bv-c64-ref bv ix be))
    ;;
    ((f96be) (bv-f96-ref bv ix be))
    ((c96be) (bv-c96-ref bv ix be))
    ((f96le) (bv-f96-ref bv ix le))
    ((c96le) (bv-c96-ref bv ix le))
    ;;
    (else (error "mtype-bv-ref: bad type " mtype))))

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
    ;;
    ((u16be) (bytevector-u16-set! bv ix value be))
    ((s16be) (bytevector-s16-set! bv ix value be))
    ((u32be) (bytevector-u32-set! bv ix value be))
    ((s32be) (bytevector-s32-set! bv ix value be))
    ((u64be) (bytevector-u64-set! bv ix value be))
    ((s64be) (bytevector-s64-set! bv ix value be))
    ((f32be) (bytevector-ieee-single-set! bv ix value be))
    ((f64be) (bytevector-ieee-double-set! bv ix value be))
    ;;
    ((s128le) (bv-s128-set! bv ix value le))
    ((u128le) (bv-u128-set! bv ix value le))
    ((s128be) (bv-s128-set! bv ix value be))
    ((u128be) (bv-u128-set! bv ix value be))
    ;;
    ((c32le) (bv-c32-set! bv ix value le))
    ((c64le) (bv-c64-set! bv ix value le))
    ((c32be) (bv-c32-set! bv ix value be))
    ((c64be) (bv-c64-set! bv ix value be))
    ;;
    ((f96le) (bv-f96-ref bv ix le))
    ((c96le) (bv-c96-ref bv ix le))
    ((f96be) (bv-f96-ref bv ix be))
    ((c96be) (bv-c96-ref bv ix be))
    ;;
    (else (error "mtype-bv-set!: bad type " mtype))))


;; === maps ====================================================================

;; ARM 64bit (little-endian)
(define mtype-map/aarch64
 '((void* . u64le)
   (char . u8) (signed-char . s8) (unsigned-char . u8)
   (short . s16le) (unsigned-short . u16le)
   (int . s32le) (unsigned . u32le)
   (long . s64le) (unsigned-long . u64le)
   (long-long . s64le) (unsigned-long-long . u64le)
   (float . f32le) (double . f64le)
   (int8_t . s8le) (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le)
   (int32_t . s32le) (uint32_t . u32le) (int64_t . s64le) (uint64_t . u64le)
   (size_t . u64le) (ssize_t . s64le)
   (ptrdiff_t . s64le) (intptr_t . s64le) (uintptr_t . u64le)
   (_Bool . u8) (bool . u8)
   (wchar_t . u32le) (char16_t . u16le) (char32_t . u32le)
   (long-double . f128le) (_Float16 . f16le) (_Float128 . f128le)
   (float-_Complex . c64le) (double-_Complex . c128le)
   (long-double-_Complex . c256le)
   (__int128 . s128le) (unsigned-__int128 . u128le)
   (unsigned-int . u32le)))

(define align-map/aarch64
  '((u8 . 1) (u8 . 1) (s16le . 2) (u16le . 2)
    (s32le . 4) (u32le . 4) (s64le . 8) (u64le . 8)
    (f32le . 4) (f64le . 8) (f128le . 16) (f16le . 2) (f128le . 16)
    (c32le . 4) (c64le . 8) (c128le . 16)
    (s128le . 16) (u128le . 16)))

(define arch/aarch64
  (make-arch-info 'aarch64 64 'little mtype-map/aarch64 align-map/aarch64))

(add-to-arch-map "aarch64" arch/aarch64)


(define mtype-map/armv8l
 '((void* . u32le)
   (char . u8) (signed-char . s8) (unsigned-char . u8)
   (short . s16le) (unsigned-short . u16le)
   (int . s32le) (unsigned . u32le)
   (long . s32le) (unsigned-long . u32le)
   (long-long . s64le) (unsigned-long-long . u64le)
   (float . f32le) (double . f64le)
   (int8_t . s8) (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le)
   (int32_t . s32le) (uint32_t . u32le) (int64_t . s64le) (uint64_t . u64le)
   (size_t . u32le) (ssize_t . s32le) (ptrdiff_t . s32le)
   (intptr_t . s32le) (uintptr_t . u32le)
   (_Bool . u8) (bool . u8)
   (wchar_t . u32le) (char16_t . u16le) (char32_t . u32le)
   (long-double . f64le) (_Float16 . #f) (_Float128 . #f)
   (float-_Complex . c32le) (double-_Complex . c64le)
   (long-double-_Complex . c64le)
   (__int128 . #f) (unsigned-__int128 . #f)
   (unsigned-int . u32le)))

(define align-map/armv8l
  '((s8 . 1) (u8 . 1) (s16le . 2) (u16le . 2) (s32le . 4) (u32le . 4)
    (s64le . 8) (u64le . 8) (f32le . 4) (f64le . 8) (c32le . 4) (c64le . 8)))

(define arch/armv8l
  (make-arch-info 'armv8l 32 'little mtype-map/armv8l align-map/armv8l))

(add-to-arch-map "armv8l" arch/armv8l)


(define mtype-map/avr
  '((void* . u16le)
    (char . s8) (signed-char . s8) (unsigned-char . u8)
    (short . s16le) (unsigned-short . u16le)
    (int . s16le) (unsigned . u16le)
    (long . s32le) (unsigned-long . u32le)
    (long-long . s64le) (unsigned-long-long . u64le)
    (float . f32le) (double . f32le)
    (int8_t . s8) (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le)
    (int32_t . s32le) (uint32_t . iu32le) (int64_t . s64le) (uint64_t . u64le)
    (size_t . u16le) (ssize_t . #f) (ptrdiff_t . s16le)
    (intptr_t . s16le) (uintptr_t . u16le)
    (_Bool . u8) (bool . u8)
    (wchar_t . #f) (char16_t . #f) (char32_t . #f)
    (long-double . f32le) (_Float16 . f16le) (_Float128 . f64le)
    (float-_Complex . c32le) (double-_Complex . c32le)
    (long-double-_Complex . #f) ;; need to check
    (__int128 . s128le) (unsigned-__int128 . u128le)
    (unsigned-int . u16le)))

(define alignof-mtype-map/avr
  (map (lambda (pair) (cons (car pair) 1)) sizeof-mtype-map))

(define arch/avr
  (make-arch-info
   'avr 8 'little mtype-map/avr alignof-mtype-map/avr))

(add-to-arch-map "avr" arch/avr)

;; hppa
(define mtype-map/hppa
 '((void* . u32be)
   (char . s8) (signed-char . s8) (unsigned-char . u8)
   (short . s16be) (unsigned-short . u16be)
   (int . s32be) (unsigned . u32be)
   (long . s32be) (unsigned-long . u32be)
   (long-long . s64be) (unsigned-long-long . u64be)
   (float . f32be) (double . f64be)
   (int8_t . s8) (uint8_t . u8) (int16_t . s16be) (uint16_t . u16be)
   (int32_t . s32be) (uint32_t . u32be) (int64_t . s64be) (uint64_t . u64be)
   (size_t . u32be) (ssize_t . s32be)
   (ptrdiff_t . s32be) (intptr_t . s32be) (uintptr_t . u32be)
   (_Bool . u8) (bool . u8)
   (wchar_t . s32be) (char16_t . u16be) (char32_t . u32be)
   (long-double . f64be) (_Float16 . #f) (_Float128 . #f)
   (float-_Complex . c32be) (double-_Complex . c64be)
   (long-double-_Complex . c64be)
   (__int128 . #f) (unsigned-__int128 . #f)
   (unsigned-int . u32be)))

(define align-map/hppa
 '((s8 . 1) (u8 . 1) (s16be . 2) (u16be . 2)
   (s32be . 4) (u32be . 4) (s64be . 8) (u64be . 8)
   (f32be . 4) (f64be . 8) (c32be . 4) (c64be . 8)))

(define arch/hppa
  (make-arch-info
   'hppa 64 'big mtype-map/hppa align-map/hppa))

(add-to-arch-map "hppa" arch/hppa)


;; i686
(define mtype-map/i686
 '((void* . u32le)
   (char . s8) (signed-char . s8) (unsigned-char . u8)
   (short . s16le) (unsigned-short . u16le)
   (int . s32le) (unsigned . u32le)
   (long . s32le) (unsigned-long . u32le)
   (long-long . s64le) (unsigned-long-long . u64le)
   (float . f32le) (double . f64le)
   (int8_t . s8) (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le)
   (int32_t . s32le) (uint32_t . u32le) (int64_t . s64le) (uint64_t . u64le)
   (size_t . u32le) (ssize_t . s32le) (ptrdiff_t . s32le)
   (intptr_t . s32le) (uintptr_t . u32le)
   (_Bool . u8) (bool . u8)
   (wchar_t . s32le) (char16_t . u16le) (char32_t . u32le)
   (long-double . f96le) (_Float16 . f16le) (_Float128 . f128le)
   (float-_Complex . c32le) (double-_Complex . c64le)
   (long-double-_Complex . c96le)
   (__int128 . #f) (unsigned-__int128 . #f)
   (unsigned-int . u32le)))

(define align-map/i686
 '((s8 . 1) (u8 . 1) (s16le . 2) (u16le . 2)
   (s32le . 4) (u32le . 4) (s64le . 8) (u64le . 8)
   (f32le . 4) (f64le . 8)
   (f96le . 4) (f16le . 2) (f128le . 16)
   (c32le . 4) (c64le . 8) (c96le . 4)))

(define arch/i686
  (make-arch-info
   'i686 32 'little mtype-map/i686 align-map/i686))

(add-to-arch-map "i686" arch/i686)


;; LoongArch 64bit (little-endian)
(define mtype-map/loongarch64
 '((void* . u64le)
   (char . s8) (signed-char . s8) (unsigned-char . u8)
   (short . s16le) (unsigned-short . u16le)
   (int . s32le) (unsigned . u32le)
   (long . s64le) (unsigned-long . u64le)
   (long-long . s64le) (unsigned-long-long . u64le)
   (float . f32le) (double . f64le)
   (int8_t . s8) (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le)
   (int32_t . s32le) (uint32_t . u32le) (int64_t . s64le) (uint64_t . u64le)
   (size_t . u64le) (ssize_t . s64le)
   (ptrdiff_t . s64le) (intptr_t . s64le) (uintptr_t . u64le)
   (_Bool . u8) (bool . u8)
   (wchar_t . s32le) (char16_t . u16le) (char32_t . u32le)
   (long-double . f128le) (_Float16 . #f) (_Float128 . f128le)
   (float-_Complex . c32le) (double-_Complex . c64le)
   (long-double-_Complex . c128le)
   (__int128 . s128le) (unsigned-__int128 . u128le)
   (unsigned-int . u32le)))

(define align-map/loongarch64
 '((s8 . 1) (u8 . 1) (s16le . 2) (u16le . 2)
   (s32le . 4) (u32le . 4) (s64le . 8) (u64le . 8)
   (f32le . 4) (f64le . 8) (f128le . 16)
   (c32le . 4) (c64le . 8) (c128le . 16)
   (s128le . 16) (u128le . 16)))

(define arch/loongarch64
  (make-arch-info
   'loongarch64 64 'little mtype-map/loongarch64 align-map/loongarch64))

(add-to-arch-map "loongarch64" arch/loongarch64)


;; m68k
(define mtype-map/m68k
 '((void* . u32be)
   (char . s8) (signed-char . s8) (unsigned-char . u8)
   (short . s16be) (unsigned-short . u16be)
   (int . s32be) (unsigned . u32be)
   (long . s32be) (unsigned-long . u32be)
   (long-long . s64be) (unsigned-long-long . u64be)
   (float . f32be) (double . f64be)
   (int8_t . s8) (uint8_t . u8) (int16_t . s16be) (uint16_t . u16be)
   (int32_t . s32be) (uint32_t . u32be) (int64_t . s64be) (uint64_t . u64be)
   (size_t . u32be) (ssize_t . s32be) (ptrdiff_t . s32be)
   (intptr_t . s32be) (uintptr_t . u32be)
   (_Bool . u8) (bool . u8)
   (wchar_t . s32be) (char16_t . u16be) (char32_t . u32be)
   (long-double . f96be) (_Float16 . #f) (_Float128 . #f)
   (float-_Complex . c32be) (double-_Complex . c64be)
   (long-double-_Complex . c96be)
   (__int128 . #f) (unsigned-__int128 . #f)
   (unsigned-int . u32be)))

(define align-map/m68k
 '((s8 . 1) (u8 . 1) (s16be . 2) (u16be . 2)
   (s32be . 2) (u32be . 2) (s64be . 2) (u64be . 2)
   (f32be . 2) (f64be . 2)))

(define arch/m68k
  (make-arch-info
   'm68k 32 'big mtype-map/m68k align-map/m68k))

(add-to-arch-map "m68k" arch/m68k)


;; mips
(define mtype-map/mips
 '((void* . u32le)
   (char . s8) (signed-char . s8) (unsigned-char . u8)
   (short . s16le) (unsigned-short . u16le)
   (int . s32le) (unsigned . u32le)
   (long . s32le) (unsigned-long . u32le)
   (long-long . s64le) (unsigned-long-long . u64le)
   (float . f32le) (double . f64le)
   (int8_t . s8) (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le)
   (int32_t . s32le) (uint32_t . u32le) (int64_t . s64le) (uint64_t . u64le)
   (size_t . u32le) (ssize_t . s32le)
   (ptrdiff_t . s32le) (intptr_t . s32le) (uintptr_t . u32le)
   (_Bool . u8) (bool . u8)
   (wchar_t . s32le) (char16_t . u16le) (char32_t . u32le)
   (long-double . f64le) (_Float16 . #f) (_Float128 . #f)
   (float-_Complex . c32le) (double-_Complex . c64le)
   (long-double-_Complex . c64le)
   (__int128 . #f) (unsigned-__int128 . #f)
   (unsigned-int . u32le)))

(define align-map/mips
 '((s8 . 1) (u8 . 1) (s16le . 2) (u16le . 2)
   (s32le . 4) (u32le . 4) (s64le . 8) (u64le . 8) (f32le . 4)
   (f64le . 8) (f64le . 8) (c32le . 4) (c64le . 8) (c128le . 8)))

(define arch/mips
  (make-arch-info
   'mips 32 'little mtype-map/mips align-map/mips))

(add-to-arch-map "mips" arch/mips)


;; 64 bit powerpc, aka ppc64 (big endian)
(define mtype-map/powerpc64
 '((void* . u64be)
   (char . u8) (signed-char . s8) (unsigned-char . u8)
   (short . s16be) (unsigned-short . u16be)
   (int . s32be) (unsigned . u32be)
   (long . s64be) (unsigned-long . u64be)
   (long-long . s64be) (unsigned-long-long . u64be)
   (float . f32be) (double . f64be)
   (int8_t . s8) (uint8_t . u8) (int16_t . s16be) (uint16_t . u16be)
   (int32_t . s32be) (uint32_t . u32be) (int64_t . s64be) (uint64_t . u64be)
   (size_t . u64be) (ssize_t . s64be)
   (ptrdiff_t . s64be) (intptr_t . s64be) (uintptr_t . u64be)
   (_Bool . u8) (bool . u8)
   (wchar_t . s32be) (char16_t . u16be) (char32_t . u32be)
   (long-double . f128be) (_Float16 . #f) (_Float128 . #f)
   (float-_Complex . c32be) (double-_Complex . c64be)
   (long-double-_Complex . c128be)
   (__int128 . s128be) (unsigned-__int128 . u128be)
   (unsigned-int . u32be)))

(define align-map/powerpc64
 '((s8 . 1) (u8 . 1) (s16be . 2) (u16be . 2)
   (s32be . 4) (u32be . 4) (s64be . 8) (u64be . 8)
   (f32be . 4) (f64be . 8) (f128be . 16)
   (c32be . 4) (c64be . 8) (c128be . 16)
   (s128be . 16) (u128be . 16)))

(define arch/powerpc64
  (make-arch-info
   'powerpc64 64 'big mtype-map/powerpc64 align-map/powerpc64))

(add-to-arch-map "powerpc64" arch/powerpc64)


;; 64 bit powerpc64le, aka ppc64le (little endian)
(define mtype-map/powerpc64le
 '((void* . u64le)
   (char . u8) (signed-char . s8) (unsigned-char . u8)
   (short . s16le) (unsigned-short . u16le)
   (int . s32le) (unsigned . u32le)
   (long . s64le) (unsigned-long . u64le)
   (long-long . s64le) (unsigned-long-long . u64le)
   (float . f32le) (double . f64le)
   (int8_t . s8) (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le)
   (int32_t . s32le) (uint32_t . u32le) (int64_t . s64le) (uint64_t . u64le)
   (size_t . u64le) (ssize_t . s64le)
   (ptrdiff_t . s64le) (intptr_t . s64le) (uintptr_t . u64le)
   (_Bool . u8) (bool . u8)
   (wchar_t . s32le) (char16_t . u16le) (char32_t . u32le)
   (long-double . f128le) (_Float16 . #f) (_Float128 . f128le)
   (float-_Complex . c32le) (double-_Complex . c64le)
   (long-double-_Complex . c128le)
   (__int128 . s128le) (unsigned-__int128 . u128le)
   (unsigned-int . u32le)))

(define align-map/powerpc64le
 '((s8 . 1) (u8 . 1) (s16le . 2) (u16le . 2)
   (s32le . 4) (u32le . 4) (s64le . 8) (u64le . 8)
   (f32le . 4) (f64le . 8)
   (f128le . 16)
   (c32le . 4) (c64le . 8) (c128le . 16)
   (s128le . 16) (u128le . 16)))

(define arch/powerpc64le
  (make-arch-info
   'powerpc64le 64 'little mtype-map/powerpc64le align-map/powerpc64le))

(add-to-arch-map "powerpc64le" arch/powerpc64le)


#|
;; riscv 32 bit (little endian)
;; riscv-gcc -march=rv32gc -dM -E - </dev/null
(define mtype-map/riscv32 mtype-map/normal64be)

(define arch/riscv32
  (make-arch-info
   'riscv32 32 'little mtype-map/riscv32 alignof-mtype-map/natural))

(add-to-arch-map "riscv32" arch/riscv32)
|#

;; RISC-V 64bit (little-endian)
(define mtype-map/riscv64
 '((void* . u64le)
   (char . u8) (signed-char . s8) (unsigned-char . u8)
   (short . s16le) (unsigned-short . u16le)
   (int . s32le) (unsigned . u32le)
   (long . s64le) (unsigned-long . u64le)
   (long-long . s64le) (unsigned-long-long . u64le)
   (float . f32le) (double . f64le)
   (int8_t . s8le) (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le)
   (int32_t . s32le) (uint32_t . u32le) (int64_t . s64le) (uint64_t . u64le)
   (size_t . u64le) (ssize_t . s64le) (ptrdiff_t . s64le)
   (intptr_t . s64le) (uintptr_t . u64le)
   (_Bool . u8) (bool . u8)
   (wchar_t . u32le) (char16_t . u16le) (char32_t . u32le)
   (long-double . f128le) (_Float16 . f16le) (_Float128 . f128le)
   (float-_Complex . c32le) (double-_Complex . c64le)
   (long-double-_Complex . c128le)
   (__int128 . s128le) (unsigned-__int128 . u128le)
   (unsigned-int . u32le)))

(define arch/riscv64
  (make-arch-info
   'riscv64 64 'little mtype-map/riscv64 alignof-mtype-map/natural))

(add-to-arch-map "riscv64" arch/riscv64)


;; s390x 64 bit (big endian)
(define mtype-map/s390x
 '((void* . u64be)
   (char . u8) (signed-char . s8) (unsigned-char . u8)
   (short . s16be) (unsigned-short . u16be)
   (int . s32be) (unsigned . u32be)
   (long . s64be) (unsigned-long . u64be)
   (long-long . s64be) (unsigned-long-long . u64be)
   (float . f32be) (double . f64be)
   (int8_t . s8) (uint8_t . u8) (int16_t . s16be) (uint16_t . u16be)
   (int32_t . s32be) (uint32_t . u32be) (int64_t . s64be) (uint64_t . u64be)
   (size_t . u64be) (ssize_t . s64be) (ptrdiff_t . s64be)
   (intptr_t . s64be) (uintptr_t . u64be)
   (_Bool . u8) (bool . u8)
   (wchar_t . s32be) (char16_t . u16be) (char32_t . u32be)
   (long-double . f128be) (_Float16 . #f) (_Float128 . f128be)
   (float-_Complex . c32be) (double-_Complex . c64be)
   (long-double-_Complex . c128be)
   (__int128 . s128be) (unsigned-__int128 . u128be)
   (unsigned-int . u32be)))

(define align-map/s390x
 '((s8 . 1) (u8 . 1) (s16be . 2) (u16be . 2)
   (s32be . 4) (u32be . 4) (s64be . 8) (u64be . 8)
   (f32be . 4) (f64be . 8) (f128be . 8)
   (c32be . 4) (c64be . 8) (c128be . 8)
   (s128be . 8) (u128be . 8)))

(define arch/s390x
  (make-arch-info
   's390x 64 'big mtype-map/s390x align-map/s390x))

(add-to-arch-map "s390x" arch/s390x)


;; sparc 32 bit (big endian)
(define mtype-map/sparc32
  '((void* . u32be)
    (char . s8) (signed-char . s8) (unsigned-char . u8)
    (short . s16be) (unsigned-short . u16be)
    (int . s32be) (unsigned . u32be)
    (long . s32be) (unsigned-long . u32be)
    (long-long . s64be) (unsigned-long-long . u64be)
    (float . f32be) (double . f64be)
    (int8_t . s8) (uint8_t . u8) (int16_t . s16be) (uint16_t . u16be)
    (int32_t . s32be) (uint32_t . u32be) (int64_t . s64be) (uint64_t . u64be)
    (size_t . u32be) (ssize_t . s32be) (ptrdiff_t . s32be)
    (intptr_t . s32be) (uintptr_t . u32be)
    (_Bool . u8) (bool . u8)
    (wchar_t . u32be) (char16_t . u16be) (char32_t . u32be)
    (long-double . f128be) (_Float16 . f16be) (_Float128 . f128be)
    (float-_Complex . c32be) (double-_Complex . c64be)
    (long-double-_Complex . #f)
    (__int128 . s128be) (unsigned-__int128 . u128be)
    (unsigned-int . u32be)))

(define arch/sparc32
  (make-arch-info
   'sparc32 32 'big mtype-map/sparc32 alignof-mtype-map/natural))

(add-to-arch-map "sparc32" arch/sparc32)


;; sparc 64 bit (big endian)
(define mtype-map/sparc64
 '((void* . u64be)
   (char . s8) (signed-char . s8) (unsigned-char . u8)
   (short . s16be) (unsigned-short . u16be)
   (int . s32be) (unsigned . u32be)
   (long . s64be) (unsigned-long . u64be)
   (long-long . s64be) (unsigned-long-long . u64be)
   (float . f32be) (double . f64be)
   (int8_t . s8) (uint8_t . u8) (int16_t . s16be) (uint16_t . u16be)
   (int32_t . s32be) (uint32_t . u32be) (int64_t . s64be) (uint64_t . u64be)
   (size_t . u64be) (ssize_t . s64be) (ptrdiff_t . s64be)
   (intptr_t . s64be) (uintptr_t . u64be)
   (_Bool . u8) (bool . u8)
   (wchar_t . s32be) (char16_t . u16be) (char32_t . u32be)
   (long-double . f128be) (_Float16 . #f) (_Float128 . f128be)
   (float-_Complex . c32be) (double-_Complex . c64be)
   (long-double-_Complex . c128be)
   (__int128 . s128be) (unsigned-__int128 . u128be)
   (unsigned-int . u32be)))

(define align-map/sparc64
 '((s8 . 1) (u8 . 1) (s16be . 2) (u16be . 2)
   (s32be . 4) (u32be . 4) (s64be . 8) (u64be . 8)
   (f32be . 4) (f64be . 8) (f128be . 16)
   (c32be . 4) (c64be . 8) (c128be . 16)
   (s128be . 16) (u128be . 16)))

(define arch/sparc64
  (make-arch-info
   'sparc64 64 'big mtype-map/sparc64 align-map/sparc64))

(add-to-arch-map "sparc64" arch/sparc64)


;; intel amd x86_64
(define mtype-map/x86_64
 '((void* . u64le)
   (char . s8) (signed-char . s8) (unsigned-char . u8)
   (short . s16le) (unsigned-short . u16le)
   (int . s32le) (unsigned . u32le)
   (long . s64le) (unsigned-long . u64le)
   (long-long . s64le) (unsigned-long-long . u64le)
   (float . f32le) (double . f64le)
   (int8_t . s8) (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le)
   (int32_t . s32le) (uint32_t . u32le) (int64_t . s64le) (uint64_t . u64le)
   (size_t . u64le) (ssize_t . s64le) (ptrdiff_t . s64le)
   (intptr_t . s64le) (uintptr_t . u64le)
   (_Bool . u8) (bool . u8)
   (wchar_t . s32le) (char16_t . u16le) (char32_t . u32le)
   (long-double . f128le) (_Float16 . f16le) (_Float128 . f128le)
   (float-_Complex . c32le) (double-_Complex . c64le)
   (long-double-_Complex . c128le)
   (__int128 . s128le) (unsigned-__int128 . u128le)
   (unsigned-int . u32le)))

(define align-map/x86_64
 '((s8 . 1) (u8 . 1) (s16le . 2) (u16le . 2)
   (s32le . 4) (u32le . 4) (s64le . 8) (u64le . 8)
   (f32le . 4) (f64le . 8) (f128le . 16) (f16le . 2) (f128le . 16)
   (c32le . 4) (c64le . 8) (c128le . 16)
   (s128le . 16) (u128le . 16)))

(define arch/x86_64
  (make-arch-info 'x86_64 64 'little mtype-map/x86_64 align-map/x86_64))

(add-to-arch-map "x86_64" arch/x86_64)

;; aliases
(add-to-arch-map "arm" arch/armv8l)
(add-to-arch-map "armhf" arch/armv8l)
(add-to-arch-map "i386" arch/i686)
(add-to-arch-map "mipsel" arch/mips)
(add-to-arch-map "parisc64" arch/hppa)
(add-to-arch-map "powerpc64be" arch/powerpc64)
(add-to-arch-map "ppc64" arch/powerpc64)
(add-to-arch-map "ppc64le" arch/powerpc64le)


;; invert to get in above order
(*arch-map* (reverse (*arch-map*)))

;; native
(eval-when (expand load eval)
  (define host-arch-name
    (and=> (string-split %host-type #\-) car)))

(define native-arch
  (assoc-ref (*arch-map*) host-arch-name))

(add-to-arch-map "native" native-arch)

;; add symbolic names
(*arch-map*
 (let loop ((archl (*arch-map*)))
   (if (null? archl) (*arch-map*)
       (acons (string->symbol (caar archl)) (cdar archl) (loop (cdr archl))))))

(*arch* native-arch)


;; === needed?

(define mtype-noendian-map
  '((s8 . s8) (u8 . u8)
    (s16le . s16) (u16le . u16) (s32le . s32) (u32le . u32)
    (s64le . s64) (u64le . u64) (f32le . f32) (f64le . f64)
    (s16be . s16) (u16be . u16) (s32be . s32) (u32be . u32)
    (s64be . s64) (u64be . u64) (f32be . f32) (f64be . f64)
    (c32le . c32) (u64le . c64) (c32be . c32) (u64be . c64)))

;; @deffn {Procedure} mtype-noendian mtype => type symbol
;; Strip the endianness part of an arch-info mtype symbol.
;; For example,
;; @example
;; (mtype-noendian 'u64le) => u64
;; @end example
;; @end deffn
(define (mtype-noendian mtype)
  (assq-ref mtype-noendian-map mtype))

;; --- last line ---
;; compare arch to base
(use-modules ((srfi srfi-1)
              #:select (lset-difference fold)))
(define bkeys
  '(void*
    char signed-char unsigned-char short unsigned-short int unsigned long
    unsigned-long long-long unsigned-long-long float double int8_t uint8_t
    int16_t uint16_t int32_t uint32_t int64_t uint64_t size_t ssize_t
    ptrdiff_t intptr_t uintptr_t _Bool bool wchar_t char16_t char32_t
    long-double _Float16 _Float128 float-_Complex double-_Complex
    long-double-_Complex __int128 unsigned-__int128 unsigned-int))

(define-public (check-arch arch-name)
  (sferr "check-arch: ~s\n" arch-name)
  (let* ((arch (assq-ref (*arch-map*) arch-name))
         (atm (arch-mtype-map arch)) (akeys (map car atm))
         (a-b-keys (lset-difference equal? akeys bkeys))
         (b-a-keys (lset-difference equal? bkeys akeys))
         )
    (when (pair? a-b-keys) (sferr "a-b-keys: ~s\n" a-b-keys))
    (when (pair? b-a-keys) (sferr "b-a-keys: ~s\n" b-a-keys))
    (unless (assq-ref atm 'unsigned-int) (sferr "missing unsigned-int\n"))
    ))

