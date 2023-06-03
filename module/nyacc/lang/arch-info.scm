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
            *arch* with-arch)
  #:use-module (srfi srfi-9))

(define-record-type <arch-info>
  (make-arch-info name endianness ctype-map align-map)
  arch-info?
  (name arch-name)                      ; e.g., "x86_64"
  (endianness arch-endianness)          ; 'little or 'big
  (ctype-map arch-ctype-map)            ; nyacc name => f32, u8, etc
  (align-map arch-align-map))           ; f32, u8 => integer

(define sizeof-map
  '((i8 . 1) (i16 . 2) (i32 . 4) (i64 . 8) (i128 . 16)
    (u8 . 1) (u16 . 2) (u32 . 4) (u64 . 8) (u128 . 16)
    (f16 . 2) (f32 . 4) (f64 . 8) (f128 . 16)))


(define sizeof-map/avr
  '((* . 2)
    ("char" . 1) ("short" . 2) ("int" . 2) ("long" . 4)
    ("float" . 4) ("double" . 4)
    ("unsigned short" . 2) ("unsigned" . 2) ("unsigned long" . 4)
    ;;
    ("size_t" . 2) ("ssize_t" . #f) ("ptrdiff_t" . 2)
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
    ("intptr_t" . 2) ("uintptr_t" . 2)
    ("wchar_t" . #f) ("char16_t" . #f) ("char32_t" . #f)
    ;;
    ("long double" . 4)
    ("long long" . 8) ("long long int" . 8) ("signed long long" . 8)
    ("signed long long int" . 8) ("unsigned long long" . 8)
    ("unsigned long long int" . 8)))

(define alignof-map/avr
  (map (lambda (pair) (cons (car pair) 1)) sizeof-map/avr))

(define sizeof-map/i686
  '((* . 4)
    ("char" . 1) ("short" . 2) ("int" . 4) ("long" . 4)
    ("float" . 4) ("double" . 8)
    ("unsigned short" . 2) ("unsigned" . 4) ("unsigned long" . 4)
    ;;
    ("size_t" . 4) ("ssize_t" . 4) ("ptrdiff_t" . 4)
    ("int8_t" . 1) ("uint8_t" . 1) ("int16_t" . 2) ("uint16_t" . 2) 
    ("int32_t" . 4) ("uint32_t" . 4) ("int64_t" . 8) ("uint64_t" . 8)
    ;;
    ("signed char" . 1) ("unsigned char" . 1)
    ("short int" . 2) ("signed short" . 2) ("signed short int" . 2)
    ("signed" . 4) ("signed int" . 4)
    ("long int" . 4) ("signed long" . 4) ("signed long int" . 4)
    ("unsigned short int" . 2) ("unsigned int" . 4) ("unsigned long int" . 4)
    ;;
    ("_Bool" . 1)
    ("intptr_t" . 4) ("uintptr_t" . 4)
    ("wchar_t" . 4) ("char16_t" . 2) ("char32_t" . 4)
    ;;
    ("long double" . 16)
    ("long long" . 8) ("long long int" . 8) ("signed long long" . 8)
    ("signed long long int" . 8) ("unsigned long long" . 8)
    ("unsigned long long int" . 8)))

(define alignof-map/i686 sizeof-map/i686)

;; 32bit powerpc aka ppc, big endian
(define sizeof-map/powerpc
  '((* . 4)
    ("char" . 1) ("short" . 2) ("int" . 4) ("long" . 8)
    ("float" . 4) ("double" . 8)
    ("unsigned short" . 2) ("unsigned" . 4) ("unsigned long" . 8)
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

(define alignof-map/powerpc sizeof-map/powerpc)

;; riscv 32 bit little endian
(define sizeof-map/riscv
  '((* . 4)
    ("char" . 1) ("short" . 2) ("int" . 4) ("long" . 8)
    ("float" . 4) ("double" . 8)
    ("unsigned short" . 2) ("unsigned" . 4) ("unsigned long" . 8)
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

(define ctype-map/x86_64
  '((* . u64)
    ("char" . i8) ("short" . i16) ("int" . i32) ("long" . i64)
    ("float" . f32) ("double" . f64)
    ("unsigned short" . u16) ("unsigned" . u32) ("unsigned long" . u64)
    ;;
    ("size_t" . u64) ("ssize_t" . u64) ("ptrdiff_t" . i64)
    ("int8_t" . i8) ("uint8_t" . u8) ("int16_t" . i16) ("uint16_t" . u16) 
    ("int32_t" . i32) ("uint32_t" . u32) ("int64_t" . i64) ("uint64_t" . u64)
    ;;
    ("signed char" . i8) ("unsigned char" . u8)
    ("short int" . i16) ("signed short" . i16) ("signed short int" . i16)
    ("signed" . i32) ("signed int" . i32)
    ("long int" . i64) ("signed long" . i64) ("signed long int" . i32)
    ("unsigned short int" . u8) ("unsigned int" . u32)
    ("unsigned long int" . u32)
    ;;
    ("_Bool" . u8)
    ("intptr_t" . i64) ("uintptr_t" . u64)
    ("wchar_t" . u32) ("char16_t" . u16) ("char32_t" . u32)
    ;;
    ("long double" . f128)
    ("long long" . i64) ("long long int" . i64) ("signed long long" . i64)
    ("signed long long int" . i64) ("unsigned long long" . u64)
    ("unsigned long long int" . u64)))

(define arch/x86_64
  (make-arch-info "x86_64" 'little ctype-map/x86_64 sizeof-map))

(define arch-map
  `(
    ("x86_64" . ,arch/x86_64)))

(define (lookup-arch name)
  (assoc-ref arch-map name))

(define host-arch
  (eval-when (expand eval compile)
    (and=> (string-split %host-type #\-) car)))

;; @deffn {Parameter} *arch*
;; parameter set to global architecture record
;; @end deffn
(define *arch* (make-parameter (lookup-arch host-arch)))

(define-syntax-rule (with-arch arch body ...)
  (parameterize ((*arch* (if (string? arch) (lookup-arch arch) arch)))
    body ...))


;; @deffn {Procedure} sizeof-basetype type
;; Return the size in bytes of the basetype @var{type}, a string, based on
;; the global parameter @var{*arch*}.
;; @example
;; (sizeof-basetype "unsigned int") => 4
;; @end example
;; @end deffn
(define (sizeof-basetype base-type-name)
  (let ((arch (*arch*)))
    (and=> (assoc-ref (arch-ctype-map arch) base-type-name)
           (lambda (type) (assq-ref sizeof-map type)))))

;; @deffn {Procedure} alignof-basetype type
;; Return the alignment of the basetype @var{type}, a string, based on
;; the global parameter @var{*arch*}.
;; @end deffn
(define (alignof-basetype base-type-name)
  (let ((arch (*arch*)))
    (and=> (assoc-ref (arch-ctype-map arch) base-type-name)
           (lambda (type) (assq-ref (arch-align-map arch) type)))))

;; defs __SIZEOF_ + + __
;; FLOAT80 INT POINTER LONG LONG_DOUBLE SIZE_T WINT_T PTRDIFF_T FLOAT FLOAT128
;; SHORT INT128 WCHAR_T DOUBLE LONG_LONG

;; --- last line ---
