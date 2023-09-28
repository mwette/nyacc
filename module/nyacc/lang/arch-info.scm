;;; nyacc/lang/arch-info.scm - sizeof and alignof

;; Copyright (C) 2020-2023 Matthew Wette
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

;; ppc alpha mips ia64 i386 x86_64
;; avr i386 ppc x86_64

;; todo: bv-help: bv-u32le-ref bv-u32be-ref bv-32-ref
;;; Code:

(define-module (nyacc lang arch-info)
  #:export (lookup-arch
            sizeof-basetype alignof-basetype
            *arch* with-arch native-arch

            sizeof-mtype alignof-mtype
            mtypeof-basetype            ; machine type
            )
  #:use-module (srfi srfi-9))

;;(display "arch reified types should not be called ctypes\n")
;; maybe mtype for machine type

(define-record-type <arch-info>
  (make-arch-info name endianness mtype-map align-map)
  arch-info?
  (name arch-name)                      ; e.g., "x86_64"
  (endianness arch-endianness)          ; 'little or 'big
  (mtype-map arch-mtype-map)            ; nyacc name => f32, u8, etc
  (align-map arch-align-map))           ; f32, u8 => integer

(define sizeof-map
  '((i8 . 1) (i16 . 2) (i32 . 4) (i64 . 8) (i128 . 16)
    (u8 . 1) (u16 . 2) (u32 . 4) (u64 . 8) (u128 . 16)
    (f16 . 2) (f32 . 4) (f64 . 8) (f128 . 16)
    (i16le. 2) (i32le . 4) (i64le . 8) (i128le . 16)
    (u16le . 2) (u32le . 4) (u64le . 8) (u128le . 16)
    (f16le . 2) (f32le . 4) (f64le . 8) (f128le . 16)
    (i16be . 2) (i32be . 4) (i64be . 8) (i128be . 16)
    (u16be . 2) (u32be . 4) (u64be . 8) (u128be . 16)
    (f16be . 2) (f32be . 4) (f64be . 8) (f128be . 16)))

(define (sizeof-mtype mtype)
  (assq-ref sizeof-map mtype))

(define mtype-map/avr
  '((* . u16le)
    ("char" . i8) ("short" . i16le) ("int" . i16le) ("long" . i32le)
    ("float" . f32le) ("double" . f32le)
    ("unsigned short" . u16le) ("unsigned" . u16le) ("unsigned long" . u32le)
    ;;
    ("size_t" . u16le) ("ssize_t" . #f) ("ptrdiff_t" . i16le)
    ("int8_t" . i8) ("uint8_t" . u8) ("int16_t" . i16le) ("uint16_t" . u16le)
    ("int32_t" . i32le) ("uint32_t" . iu32le)
    ("int64_t" . i64le) ("uint64_t" . u64le)
    ;;
    ("signed char" . i8) ("unsigned char" . u8)
    ("short int" . i16le) ("signed short" . i16le) ("signed short int" . i16le)
    ("signed" . i16le) ("signed int" . i16le)
    ("long int" . i32le) ("signed long" . i32le) ("signed long int" . i32le)
    ("unsigned short int" . u8) ("unsigned int" . u16le)
    ("unsigned long int" . u32le)
    ;;
    ("_Bool" . u8)
    ("intptr_t" . i16le) ("uintptr_t" . u16le)
    ("wchar_t" . #f) ("char16_t" . #f) ("char32_t" . #f)
    ;;
    ("long double" . f32le)
    ("long long" . i64le) ("long long int" . i64le) ("signed long long" . i64le)
    ("signed long long int" . i64le) ("unsigned long long" . u64le)
    ("unsigned long long int" . u64le)))

(define alignof-map/avr
  (map (lambda (pair) (cons (car pair) 1)) sizeof-map))

(define arch/avr
  (make-arch-info "avr" 'little mtype-map/avr alignof-map/avr))


(define mtype-map/i686
  '((* . u32le)
    ("char" . i8) ("short" . i16le) ("int" . i32le) ("long" . i32le)
    ("float" . f32le) ("double" . f64le)
    ("unsigned short" . u16le) ("unsigned" . u32le) ("unsigned long" . u32le)
    ;;
    ("size_t" . i32le) ("ssize_t" . i32le) ("ptrdiff_t" . i32le)
    ("int8_t" . i8) ("uint8_t" . u8) ("int16_t" . i16le) ("uint16_t" . u16le)
    ("int32_t" . i32le) ("uint32_t" . u32le) ("int64_t" . i64le) ("uint64_t" . u64le)
    ;;
    ("signed char" . i8) ("unsigned char" . u8)
    ("short int" . i16le) ("signed short" . i16le) ("signed short int" . i16le)
    ("signed" . 32le) ("signed int" . i32le)
    ("long int" . i32le) ("signed long" . i32le) ("signed long int" . i32le)
    ("unsigned short int" . u16le) ("unsigned int" . u32le)
    ("unsigned long int" . u32le)
    ;;
    ("_Bool" . u8)
    ("intptr_t" . i32le) ("uintptr_t" . u32le)
    ("wchar_t" . u32le) ("char16_t" . u16le) ("char32_t" . u32le)
    ;;
    ("long double" . f128)
    ("long long" . i64le) ("long long int" . i64le) ("signed long long" . i64le)
    ("signed long long int" . i64le) ("unsigned long long" . u64le)
    ("unsigned long long int" . u64le)))

(define arch/i686
  (make-arch-info "i686" 'little mtype-map/i686 sizeof-map))


;; 32bit powerpc aka ppc, big endian
(define mtype-map/powerpc
  '((* . u32be)
    ("char" . i8) ("short" . i16be) ("int" . i32be) ("long" . i64be)
    ("float" . f32be) ("double" . f64be)
    ("unsigned short" . u16be) ("unsigned" . u32be) ("unsigned long" . u64be)
    ;;
    ("size_t" . i64be) ("ssize_t" . i64be) ("ptrdiff_t" . i64be) ("int8_t" . i8)
    ("uint8_t" . u8) ("int16_t" . i16be) ("uint16_t" . u16be) ("int32_t" . i32be)
    ("uint32_t" . u32be) ("int64_t" . i64le) ("uint64_t" . u64le)
    ;;
    ("signed char" . i8) ("unsigned char" . u8) ("short int" . i16be)
    ("signed short" . i16be) ("signed short int" . i16be) ("signed" . i32be)
    ("signed int" . i32be) ("long int" . i64be) ("signed long" . i64be)
    ("signed long int" . i64le) ("unsigned short int" . u16be)
    ("unsigned int" . u32be) ("unsigned long int" . u64be)
    ;;
    ("_Bool" . u8)
    ("intptr_t" . i32be) ("uintptr_t" . u32be)
    ("wchar_t" . u32be) ("char16_t" . u16be) ("char32_t" . u32be)
    ;;
    ("long double" . f128be)
    ("long long" . i64be) ("long long int" . i64be) ("signed long long" . i64be)
    ("signed long long int" . i64be) ("unsigned long long" . u64be)
    ("unsigned long long int" . u64be)))

(define arch/powerpc
  (make-arch-info "powerpc" 'big mtype-map/powerpc sizeof-map))


#|
;; riscv 32 bit little endian
(define sizeof-map/riscv32
  '((* . 4)
    ("char" . i8) ("short" . i16le) ("int" . i32le) ("long" . i32le)
    ("float" . f32le) ("double" . f64le)
    ("unsigned short" . u16le) ("unsigned" . u32le) ("unsigned long" . u32le)
    ;;
    ("size_t" . 8) ("ssize_t" . 8) ("ptrdiff_t" . 8)
    ("int8_t" . 1) ("uint8_t" . 1) ("int16_t" . 2) ("uint16_t" . 2)
    ("int32_t" . 4) ("uint32_t" . 4) ("int64_t" . 8) ("uint64_t" . 8)
    ;;
    ("signed char" . 1) ("unsigned char" . 1)
    ("short int" . 2) ("signed short" . 2) ("signed short int" . 2)
    ("signed" . 4) ("signed int" . 4)
    ("long int" . 8) ("signed long" . 8) ("signed long int" . 8)
    ("unsigned short int" . 2) ("unsigned int" . 4) ("unsigned long int" . 8)
    ;;
    ("_Bool" . 1)
    ("intptr_t" . 4) ("uintptr_t" . 4)
    ("wchar_t" . 4) ("char16_t" . 2) ("char32_t" . 4)
    ;;
    ("long double" . 16)
    ("long long" . 8) ("long long int" . 8) ("signed long long" . 8)
    ("signed long long int" . 8) ("unsigned long long" . 8)
    ("unsigned long long int" . 8)))

(define alignof-map/riscv sizeof-map/riscv)
|#

(define mtype-map/x86_64
  '((* . u64le)
    ("char" . i8) ("short" . i16le) ("int" . i32le) ("long" . i64le)
    ("float" . f32le) ("double" . f64le)
    ("unsigned short" . u16le) ("unsigned" . u32le) ("unsigned long" . u64le)
    ;;
    ("size_t" . u64le) ("ssize_t" . u64le) ("ptrdiff_t" . i64le)
    ("int8_t" . i8) ("uint8_t" . u8) ("int16_t" . i16le) ("uint16_t" . u16le)
    ("int32_t" . i32le) ("uint32_t" . u32le) ("int64_t" . i64le) ("uint64_t" . u64le)
    ;;
    ("signed char" . i8) ("unsigned char" . u8)
    ("short int" . i16le) ("signed short" . i16le) ("signed short int" . i16le)
    ("signed" . i32le) ("signed int" . i32le)
    ("long int" . i64le) ("signed long" . i64le) ("signed long int" . i32le)
    ("unsigned short int" . u8) ("unsigned int" . u32le)
    ("unsigned long int" . u32le)
    ;;
    ("_Bool" . u8)
    ("intptr_t" . i64le) ("uintptr_t" . u64le)
    ("wchar_t" . u32le) ("char16_t" . u16le) ("char32_t" . u32le)
    ;;
    ("long double" . f128)
    ("long long" . i64le) ("long long int" . i64le) ("signed long long" . i64le)
    ("signed long long int" . i64le) ("unsigned long long" . u64le)
    ("unsigned long long int" . u64le)))

(define arch/x86_64
  (make-arch-info "x86_64" 'little mtype-map/x86_64 sizeof-map))


(define arch-map
  `(("avr" . ,arch/avr)
    ("i686" . ,arch/i686)
    ("powerpc" . ,arch/powerpc)
    ("x86_64" . ,arch/x86_64)))

(define (lookup-arch name)
  (assoc-ref arch-map name))

(define host-arch-name
  (eval-when (expand eval compile)
    (and=> (string-split %host-type #\-) car)))

(define native-arch (lookup-arch host-arch-name))

;; @deffn {Parameter} *arch*
;; parameter set to global architecture record
;; NEW => if #f then native
;; @end deffn
;;(define *arch* (make-parameter native-arch))
(define *arch* (make-parameter #f))

(define-syntax-rule (with-arch arch body ...)
  (parameterize ((*arch* (if (string? arch) (lookup-arch arch) arch)))
    body ...))


;; @deffn {Procedure} typeof-basetype base-type-name => 'f64
;; @end deffn
(define (mtypeof-basetype base-type-name)
  (assoc-ref (arch-mtype-map (*arch*)) base-type-name))

(define (alignof-mtype mtype)
  (assq-ref (arch-align-map (*arch*)) mtype))

;; @deffn {Procedure} sizeof-basetype type
;; Return the size in bytes of the basetype @var{type}, a string, based on
;; the global parameter @var{*arch*}.
;; @example
;; (sizeof-basetype "unsigned int") => 4
;; @end example
;; @end deffn
(define (sizeof-basetype base-type-name)
  (let ((arch (*arch*)))
    (and=> (assoc-ref (arch-mtype-map arch) base-type-name)
           (lambda (type) (assq-ref sizeof-map type)))))

;; @deffn {Procedure} alignof-basetype type
;; Return the alignment of the basetype @var{type}, a string, based on
;; the global parameter @var{*arch*}.
;; @end deffn
(define (alignof-basetype base-type-name)
  (let ((arch (*arch*)))
    (and=> (assoc-ref (arch-mtype-map arch) base-type-name)
           (lambda (type) (assq-ref (arch-align-map arch) type)))))

;; defs __SIZEOF_ + + __
;; FLOAT80 INT POINTER LONG LONG_DOUBLE SIZE_T WINT_T PTRDIFF_T FLOAT FLOAT128
;; SHORT INT128 WCHAR_T DOUBLE LONG_LONG

;; --- last line ---
