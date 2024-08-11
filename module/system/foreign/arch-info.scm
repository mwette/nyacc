;;; system/foreign/arch-info.scm - sizeof and alignof

;; Copyright (C) 2020-2024 Matthew Wette
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

;; have: avr i686 ppc riscv32 riscv64 x86_64
;; Can we remove non-le/be mtypes?

;;; Code:

(define-module (system foreign arch-info)
  #:export (lookup-arch
            *arch* with-arch native-arch
            arch-cbase-map set-arch-cbase-map!
            mtype-size mtype-alignment mtype-endianness
            sizeof-basetype alignof-basetype
            mtypeof-basetype sizeof-mtype alignof-mtype
            base-type-name-list base-type-symbol-list
            c-strname->symname c-symname->strname
            mtype-signed?
            ;;mtype-bv-ref mtype-bv-set! <= requires (rnrs bytevectors)
            )
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

;;(display "arch reified types should not be called ctypes\n")
;; maybe mtype for machine type

(define-record-type <arch-info>
  (make-arch-info name endianness mtype-map align-map cbase-map)
  arch-info?
  (name arch-name)                      ; e.g., "x86_64"
  (endianness arch-endianness)          ; 'little or 'big
  (mtype-map arch-mtype-map)            ; c-ish name => f32l3, u8, ...
  (align-map arch-align-map)            ; f32, u8 => alignment
  (cbase-map arch-cbase-map set-arch-cbase-map!))

(define sizeof-mtype-map
  '((s8 . 1) (u8 . 1)
    (s16 . 2) (s32 . 4) (s64 . 8) (i128 . 16)
    (u16 . 2) (u32 . 4) (u64 . 8) (u128 . 16)
    (f16 . 2) (f32 . 4) (f64 . 8) (f128 . 16)
    (s16le . 2) (s32le . 4) (s64le . 8) (i128le . 16)
    (u16le . 2) (u32le . 4) (u64le . 8) (u128le . 16)
    (f16le . 2) (f32le . 4) (f64le . 8) (f128le . 16)
    (s16be . 2) (s32be . 4) (s64be . 8) (i128be . 16)
    (u16be . 2) (u32be . 4) (u64be . 8) (u128be . 16)
    (f16be . 2) (f32be . 4) (f64be . 8) (f128be . 16)))

(define endianness-mtype-map
  (let ((le 'little) (be 'big))
    `((s8 . any) (u8 . any)
      (s16le . ,le) (s32le . ,le) (s64le . ,le) (i128le . .le)
      (u16le . ,le) (u32le . ,le) (u64le . ,le) (u128le . ,le)
      (f16le . ,le) (f32le . ,le) (f64le . ,le) (f128le . ,le)
      (s16be . ,be) (s32be . ,be) (s64be . ,be) (i128be . ,be)
      (u16be . ,be) (u32be . ,be) (u64be . ,be) (u128be . ,be)
      (f16be . ,be) (f32be . ,be) (f64be . ,be) (f128be . ,be))))

(define alignof-mtype-map/natural sizeof-mtype-map)

(define base-type-name-list
  '("void*"
    "char" "short" "int" "long" "float" "double" "unsigned short" "unsigned"
    "unsigned long" "size_t" "ssize_t" "ptrdiff_t" "int8_t" "uint8_t"
    "int16_t" "uint16_t" "int32_t" "uint32_t" "int64_t" "uint64_t"
    "signed char" "unsigned char" "short int" "signed short" "signed short int"
    "signed" "signed int" "long int" "signed long" "signed long int"
    "unsigned short int" "unsigned int" "unsigned long int" "_Bool" "bool"
    "intptr_t" "uintptr_t" "wchar_t" "char16_t" "char32_t" "long double"
    "long long" "long long int" "signed long long" "signed long long int"
    "unsigned long long" "unsigned long long int"))

(define base-type-symbol-list
  '(void*
    char short int long float double unsigned-short unsigned unsigned-long
    size_t ssize_t ptrdiff_t int8_t uint8_t int16_t uint16_t int32_t
    uint32_t int64_t uint64_t signed-char unsigned-char short-int
    signed-short signed-short-int signed signed-int long-int signed-long
    signed-long-int unsigned-short-int unsigned-int unsigned-long-int
    _Bool bool intptr_t uintptr_t wchar_t char16_t char32_t long-double
    long-long long-long-int signed-long-long signed-long-long-int
    unsigned-long-long unsigned-long-long-int))

(define *arch-map* (make-parameter '()))

(define (add-to-arch-map name arch)
  (let ((symname (if (symbol? name) name (string->symbol name)))
        (strname (if (string? name) name (symbol->string name))))
    (*arch-map* (cons* (cons symname arch) (cons strname arch) (*arch-map*)))))

(define (lookup-arch name)
  (assoc-ref (*arch-map*) name))

;; @deffn {Parameter} *arch*
;; parameter set to global architecture record
;; NEW => if #f then native
;; @end deffn
(define *arch* (make-parameter #f))

;; @deffn {Syntax} with-arch arch body ...
;; @end deffn
(define-syntax-rule (with-arch arch body ...)
  (parameterize
      ((*arch* (let ((march (if (arch-info? arch) arch (lookup-arch arch))))
                 (unless march (error "with-arch: no such arch"))
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



;; @deffn {Procedure} sizeof-basetype name
;; @var{name} can be string (e.g., @code{"short int"}) or
;; symbol (e.g., @code{short-int}).
;; Return the size in bytes of the basetype @var{type}, a string, based on
;; the global parameter @var{*arch*}.
;; @example
;; (sizeof-basetype "unsigned int") => 4
;; @end example
;; @end deffn
(define (sizeof-basetype name)
  (let ((name (if (string? name) (c-strname->symname name) name))
        (arch (*arch*)))
    (and=> (assq-ref (arch-mtype-map arch) name)
           sizeof-mtype)))

;; @deffn {Procedure} alignof-basetype name
;; @var{name} can be string (e.g., @code{"short int"}) or
;; symbol (e.g., @code{short-int}).
;; Return the alignment of the basetype @var{type-name} based on
;; the global parameter @var{*arch*}.
;; @end deffn
(define (alignof-basetype name)
  (let ((name (if (string? name) (strname->symname name) name))
        (arch (*arch*)))
    (and=> (assq-ref (arch-mtype-map arch) name)
           (lambda (type) (assq-ref (arch-align-map arch) type)))))

;; move to arch-info
(define (mtype-signed? mtype)
  (and (member mtype '(s8 s16 s32 s64 s16le s32le s64le s16be s32be s64be)) #t))


;; === maps ====================================================================

(define mtype-map/avr
  '((void* . u16le)
    (char . s8) (short . s16le) (int . s16le) (long . s32le)
    (float . f32le) (double . f32le)
    (unsigned-short . u16le) (unsigned . u16le) (unsigned-long . u32le)
    ;;
    (size_t . u16le) (ssize_t . #f) (ptrdiff_t . s16le)
    (int8_t . s8) (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le)
    (int32_t . s32le) (uint32_t . iu32le)
    (int64_t . s64le) ("uint64_t" . u64le)
    ;;
    (signed-char . s8) (unsigned-char . u8)
    (short-int . s16le) (signed-short . s16le) (signed-short-int . s16le)
    (signed . s16le) (signed-int . s16le)
    (long-int . s32le) (signed-long . s32le) (signed-long-int . s32le)
    (unsigned-short-int . u8) (unsigned-int . u16le)
    (unsigned-long-int . u32le)
    ;;
    (_Bool . s8) (bool . s8)
    (intptr_t . s16le) (uintptr_t . u16le)
    (wchar_t . #f) (char16_t . #f) (char32_t . #f)
    ;;
    (long-double . f32le)
    (long-long . s64le) (long-long-int . s64le) (signed-long-long . s64le)
    (signed-long-long-int . s64le) (unsigned-long-long . u64le)
    (unsigned-long-long-int . u64le)))

(define alignof-mtype-map/avr
  (map (lambda (pair) (cons (car pair) 1)) sizeof-mtype-map))

(define arch/avr
  (make-arch-info 'avr 'little mtype-map/avr alignof-mtype-map/avr #f))

(add-to-arch-map "avr" arch/avr)


(define mtype-map/i686
  '((void* . u32le)
    (char . s8) (short . s16le) (int . s32le) (long . s32le)
    (float . f32le) (double . f64le)
    (unsigned-short . u16le) (unsigned . u32le) (unsigned-long . u32le)
    ;;
    (size_t . s32le) (ssize_t . s32le) (ptrdiff_t . s32le)
    (int8_t . s8) (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le)
    (int32_t . s32le) (uint32_t . u32le) (int64_t . s64le)
    (uint64_t . u64le)
    ;;
    (signed-char . s8) (unsigned-char . u8)
    (short-int . s16le) (signed-short . s16le) (signed-short-int . s16le)
    (signed . 32le) (signed-int . s32le)
    (long-int . s32le) (signed-long . s32le) (signed-long-int . s32le)
    (unsigned-short-int . u16le) (unsigned-int . u32le)
    (unsigned-long-int . u32le)
    ;;
    (_Bool . s8) (bool . s8)
    (intptr_t . s32le) (uintptr_t . u32le)
    (wchar_t . u32le) (char16_t . u16le) (char32_t . u32le)
    ;;
    (long-double . f128)
    (long-long . s64le) (long-long-int . s64le) (signed-long-long . s64le)
    (signed-long-long-int . s64le) (unsigned-long-long . u64le)
    (unsigned-long-long-int . u64le)))

(define arch/i686
  (make-arch-info 'i686 'little mtype-map/i686 alignof-mtype-map/natural #f))

(add-to-arch-map "i686" arch/i686)


;; 32bit powerpc aka ppc, big endian
(define mtype-map/powerpc
  '((void* . u32be)
    (char . s8) (short . s16be) (int . s32be) (long . s64be)
    (float . f32be) (double . f64be)
    (unsigned-short . u16be) (unsigned . u32be) (unsigned-long . u64be)
    ;;
    (size_t . s64be) (ssize_t . s64be) (ptrdiff_t . s64be) (int8_t . s8)
    (uint8_t . u8) (int16_t . s16be) (uint16_t . u16be) (int32_t . s32be)
    (uint32_t . u32be) (int64_t . s64le) (uint64_t . u64le)
    ;;
    (signed-char . s8) (unsigned-char . u8) (short-int . s16be)
    (signed-short . s16be) (signed-short-int . s16be) (signed . s32be)
    (signed-int . s32be) (long-int . s64be) (signed-long . s64be)
    (signed-long-int . s64le) (unsigned-short-int . u16be)
    (unsigned-int . u32be) (unsigned-long-int . u64be)
    ;;
    (_Bool . s8) (bool . s8)
    (intptr_t . s32be) (uintptr_t . u32be)
    (wchar_t . u32be) (char16_t . u16be) (char32_t . u32be)
    ;;
    (long-double . f128be)
    (long-long . s64be) (long-long-int . s64be) (signed-long-long . s64be)
    (signed-long-long-int . s64be) (unsigned-long-long . u64be)
    (unsigned-long-long-int . u64be)))

(define arch/powerpc
  (make-arch-info 'powerpc 'big mtype-map/powerpc alignof-mtype-map/natural #f))

(add-to-arch-map "powerpc" arch/powerpc)


;; riscv 32 bit (little endian)
;; riscv-gcc -march=rv32gc -dM -E - </dev/null
(define mtype-map/riscv32
  '((void* . u32le)
    (char . s8) (short . s16le) (int . s32le) (long . s32le)
    (float . f32le) (double . f64le)
    (unsigned-short . u16le) (unsigned . u32le) (unsigned-long . u32le)
    ;;
    (size_t . u32le) (ssize_t . s32le) (ptrdiff_t . s32le) (int8_t . s8)
    (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le) (int32_t . s32le)
    (uint32_t . u32le) (int64_t . s64le) (uint64_t . u64le)
    ;;
    (signed-char . s8) (unsigned-char . u8)
    (short-int . s16le) (signed-short . s16le) (signed-short-int . s16le)
    (signed . s32le) (signed-int . s32le) (long-int . s32le)
    (signed-long . s32le) (signed-long-int . s32le)
    (unsigned-short-int . s16le) (unsigned-int . u32le)
    (unsigned-long-int . u32le)
    ;;
    (_Bool . s8) (bool . s8) (intptr_t . s32le) (uintptr_t . u32le)
    (wchar_t . u32le) (char16_t . u16le) (char32_t . u32le)
    ;;
    (long-double . f128le)
    (long-long . s64le) (long-long-int . s64le) (signed-long-long . s64le)
    (signed-long-long-int . s64le) (unsigned-long-long . u64le)
    (unsigned-long-long-int . u64le)))

(define arch/riscv32
  (make-arch-info 'riscv32 'little mtype-map/riscv32 alignof-mtype-map/natural
                  #f))

(add-to-arch-map "riscv32" arch/riscv32)


;; RISC-V 64bit (little-endian)
;; riscv-gcc -march=rv64g -mabi=lp64d -dM -E - </dev/null
(define mtype-map/riscv64
  '((void* . u64le)
    (char . s8) (short . s16le) (int . s32le) (long . s64le)
    (float . f32le) (double . f64le) (unsigned-short . u16le)
    (unsigned . u32le) (unsigned-long . u64le)
    ;;
    (size_t . u64le) (ssize_t . s64le) (ptrdiff_t . s64le) (int8_t . s8)
    (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le) (int32_t . s32le)
    (uint32_t . u32le) (int64_t . s64le) (uint64_t . u64le)
    ;;
    (signed-char . s8) (unsigned-char . u8)
    (short-int . s16le) (signed-short . s16le) (signed-short-int . s16le)
    (signed . s32le) (signed-int . s32le) (long-int . s64le)
    (signed-long . s64le) (signed-long-int . s64le)
    (unsigned-short-int . u16le) (unsigned-int . u32le)
    (unsigned-long-int . u64le)
    ;;
    (_Bool . s8) (bool . s8)
    (intptr_t . s64le) (uintptr_t . u64le)
    (wchar_t . u32le) (char16_t . u16le) (char32_t . u32le)
    ;;
    (long-double . f128le)
    (long-long . s64le) (long-long-int . s64le) (signed-long-long . s64le)
    (signed-long-long-int . s64le) (unsigned-long-long . u64le)
    (unsigned-long-long-int . u64le)))

(define arch/riscv64
  (make-arch-info "riscv64" 'little mtype-map/riscv64 alignof-mtype-map/natural
                  #f))

(add-to-arch-map "riscv64" arch/riscv64)


;; sparc 32 bit (big endian)
(define mtype-map/sparc
  '((void* . u32be)
    (char . s8) (short . s16be) (int . s32be) (long . s32be)
    (float . f32be) (double . f64be)
    (unsigned-short . u16be) (unsigned . u32be) (unsigned-long . u32be)
    ;;
    (size_t . u32be) (ssize_t . s32be) (ptrdiff_t . s32be) (int8_t . s8)
    (uint8_t . u8) (int16_t . s16be) (uint16_t . u16be) (int32_t . s32be)
    (uint32_t . u32be) (int64_t . s64be) (uint64_t . u64be)
    ;;
    (signed-char . s8) (unsigned-char . u8)
    (short-int . s16be) (signed-short . s16be) (signed-short-int . s16be)
    (signed . s32be) (signed-int . s32be) (long-int . s32be)
    (signed-long . s32be) (signed-long-int . s32be)
    (unsigned-short-int . s16be) (unsigned-int . u32be)
    (unsigned-long-int . u32be)
    ;;
    (_Bool . s8) (bool . s8) (intptr_t . s32be) (uintptr_t . u32be)
    (wchar_t . u32be) (char16_t . u16be) (char32_t . u32be)
    ;;
    (long-double . f128be)
    (long-long . s64be) (long-long-int . s64be) (signed-long-long . s64be)
    (signed-long-long-int . s64be) (unsigned-long-long . u64be)
    (unsigned-long-long-int . u64be)))

(define arch/sparc
  (make-arch-info 'sparc 'big mtype-map/sparc alignof-mtype-map/natural #f))

(add-to-arch-map "sparc" arch/sparc)


(define mtype-map/x86_64
  '((void* . u64le)
    (char . s8) (short . s16le) (int . s32le) (long . s64le)
    (float . f32le) (double . f64le)
    (unsigned-short . u16le) (unsigned . u32le) (unsigned-long . u64le)
    ;;
    (size_t . u64le) (ssize_t . u64le) (ptrdiff_t . s64le)
    (int8_t . s8) (uint8_t . u8) (int16_t . s16le) (uint16_t . u16le)
    (int32_t . s32le) (uint32_t . u32le) (int64_t . s64le)
    (uint64_t . u64le)
    ;;
    (signed-char . s8) (unsigned-char . u8)
    (short-int . s16le) (signed-short . s16le) (signed-short-int . s16le)
    (signed . s32le) (signed-int . s32le)
    (long-int . s64le) (signed-long . s64le) (signed-long-int . s32le)
    (unsigned-short-int . u8) (unsigned-int . u32le)
    (unsigned-long-int . u32le)
    ;;
    (_Bool . u8) (bool . s8) (intptr_t . s64le) (uintptr_t . u64le)
    (wchar_t . u32le) (char16_t . u16le) (char32_t . u32le)
    ;;
    (long-double . f128)
    (long-long . s64le) (long-long-int . s64le) (signed-long-long . s64le)
    (signed-long-long-int . s64le) (unsigned-long-long . u64le)
    (unsigned-long-long-int . u64le)))

(define arch/x86_64
  (make-arch-info "x86_64" 'little mtype-map/x86_64 sizeof-mtype-map #f))

(add-to-arch-map "x86_64" arch/x86_64)


;; === native =================================================================

(define host-arch-name
  (eval-when (expand eval compile)
    (and=> (string-split %host-type #\-) car)))

#|
(define mtype-noendian-map
  '((s8 . s8) (u8 . u8)
    (s16le . s16) (u16le . u16) (s32le . s32) (u32le . u32)
    (s64le . s64) (u64le . u64) (f32le . f32) (f64le . f64)
    (s16be . s16) (u16be . u16) (s32be . s32) (u32be . u32)
    (s64be . s64) (u64be . u64) (f32be . f32) (f64be . f64)))

(define (make-native-arch arch)
  (let ((mtype-map (map (lambda (pair)
                          (cons (car pair) (assq-ref mxmap (cdr pair))))
                        (arch-mtype-map arch))))
    (make-arch-info x y z)))))
|#

(define native-arch
  (assoc-ref (*arch-map*) host-arch-name))

(*arch* native-arch)

;; --- last line ---
