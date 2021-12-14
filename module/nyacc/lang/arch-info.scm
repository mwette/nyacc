;;; nyacc/lang/arch-info.scm - sizeof and alignof

;; Copyright (C) 2020-2021 Matthew R. Wette
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

;;; Code:

(define-module (nyacc lang arch-info)
  #:export (lookup-arch
	    arch-info-host
	    sizeof-basetype alignof-basetype
	    sizeof-map/native
	    alignof-map/native
	    *arch*
	    with-arch)
  #:use-module (srfi srfi-9))

(define-record-type <arch-info>
  (make-arch-info endianness sizeof-dict alignof-dict)
  arch-info?
  (endianness arch-endianness)
  (sizeof-dict arch-sizeof-dict)
  (alignof-dict arch-alignof-dict))

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

(define sizeof-map/i386
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

(define alignof-map/i386 sizeof-map/i386)

;; PPC  GUESS
(define sizeof-map/ppc
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

(define alignof-map/ppc sizeof-map/ppc)

;; risc-v - GUESS
(define sizeof-map/rv32
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

(define alignof-map/rv32 sizeof-map/rv32)

(define sizeof-map/x86_64
  '((* . 8)
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
    ("intptr_t" . 8) ("uintptr_t" . 8)
    ("wchar_t" . 4) ("char16_t" . 2) ("char32_t" . 4)
    ;;
    ("long double" . 16)
    ("long long" . 8) ("long long int" . 8) ("signed long long" . 8)
    ("signed long long int" . 8) ("unsigned long long" . 8)
    ("unsigned long long int" . 8)))

(define alignof-map/x86_64 sizeof-map/x86_64)

(use-modules (system foreign))

(cond-expand
 (guile-2.2)
 (guile-2
  (define intptr_t long)
  (define uintptr_t unsigned-long)
  ))

(define sizeof-map/native-builtin
  `((* . ,(sizeof '*))
    ("char" . 1) ("short" . ,(sizeof short)) ("int" . ,(sizeof int))
    ("long" . ,(sizeof long)) ("float" . ,(sizeof float))
    ("double" . ,(sizeof double)) ("unsigned short" . ,(sizeof unsigned-short))
    ("unsigned" . ,(sizeof unsigned-int))
    ("unsigned long" . ,(sizeof unsigned-long))
    ;;
    ("size_t" . ,(sizeof size_t)) ("ssize_t" . ,(sizeof ssize_t))
    ("ptrdiff_t" . ,(sizeof ptrdiff_t))
    ("int8_t" . 1) ("uint8_t" . 1) ("int16_t" . 2) ("uint16_t" . 2) 
    ("int32_t" . 4) ("uint32_t" . 4) ("int64_t" . 8) ("uint64_t" . 8)
    ;;
    ("signed char" . 1) ("unsigned char" . 1)
    ("short int" . ,(sizeof short)) ("signed short" . ,(sizeof short))
    ("signed short int" . ,(sizeof short)) ("signed" . ,(sizeof int))
    ("signed int" . ,(sizeof int)) ("long int" . ,(sizeof long))
    ("signed long" . ,(sizeof long)) ("signed long int" . ,(sizeof long))
    ("unsigned short int" . ,(sizeof short)) ("unsigned int" . ,(sizeof int))
    ("unsigned long int" . ,(sizeof long))
    ;;
    ("_Bool" . 1)
    ("intptr_t" . ,(sizeof intptr_t)) ("uintptr_t" . ,(sizeof uintptr_t))
    ("wchar_t" . ,(sizeof int)) ("char16_t" . 2) ("char32_t" . 4)
    ;;
    ("long double" . 16)
    ("long long" . 8) ("long long int" . 8) ("signed long long" . 8)
    ("signed long long int" . 8) ("unsigned long long" . 8)
    ("unsigned long long int" . 8)))

(define alignof-map/native-builtin
  `((* . ,(alignof '*))
    ("char" . 1) ("short" . ,(alignof short)) ("int" . ,(alignof int))
    ("long" . ,(alignof long)) ("float" . ,(alignof float))
    ("double" . ,(alignof double)) ("unsigned short" . ,(alignof unsigned-short))
    ("unsigned" . ,(alignof unsigned-int))
    ("unsigned long" . ,(alignof unsigned-long))
    ;;
    ("size_t" . ,(alignof size_t)) ("ssize_t" . ,(alignof ssize_t))
    ("ptrdiff_t" . ,(alignof ptrdiff_t))
    ("int8_t" . 1) ("uint8_t" . 1) ("int16_t" . 2) ("uint16_t" . 2) 
    ("int32_t" . 4) ("uint32_t" . 4) ("int64_t" . 8) ("uint64_t" . 8)
    ;;
    ("signed char" . 1) ("unsigned char" . 1)
    ("short int" . ,(alignof short)) ("signed short" . ,(alignof short))
    ("signed short int" . ,(alignof short)) ("signed" . ,(alignof int))
    ("signed int" . ,(alignof int)) ("long int" . ,(alignof long))
    ("signed long" . ,(alignof long)) ("signed long int" . ,(alignof long))
    ("unsigned short int" . ,(alignof short)) ("unsigned int" . ,(alignof int))
    ("unsigned long int" . ,(alignof long))
    ;;
    ("_Bool" . 1)
    ("intptr_t" . ,(alignof intptr_t)) ("uintptr_t" . ,(alignof uintptr_t))
    ("wchar_t" . ,(alignof int)) ("char16_t" . 2) ("char32_t" . 4)
    ;;
    ("long double" . 16)
    ("long long" . 8) ("long long int" . 8) ("signed long long" . 8)
    ("signed long long int" . 8) ("unsigned long long" . 8)
    ("unsigned long long int" . 8)))

(define sizeof-map/native sizeof-map/native-builtin)
(define alignof-map/native alignof-map/native-builtin)

(define arch-sizeof-map
  `(("native" . ,sizeof-map/native)
    ("avr" . ,sizeof-map/avr)
    ("i386" . ,sizeof-map/i386)
    ("ppc" . ,sizeof-map/ppc)
    ("rv32" . ,sizeof-map/rv32)
    ("x86_64" . ,sizeof-map/x86_64)))

(define arch-alignof-map
  `(("native" . ,alignof-map/native)
    ("avr" . ,alignof-map/avr)
    ("i386" . ,alignof-map/i386)
    ("ppc" . ,alignof-map/ppc)
    ("rv32" . ,alignof-map/rv32)
    ("x86_64" . ,alignof-map/x86_64)))

(define (lookup-arch name)
  (let ((sizeof-dict (assoc-ref arch-sizeof-map name))
	(alignof-dict (assoc-ref arch-alignof-map name)))
    (and sizeof-dict alignof-dict (cons sizeof-dict alignof-dict))))


(define arch-info-host
  (eval-when (expand eval compile)
    (and=> (string-split %host-type #\-) car)))

(define sizeof-map/native-arch
  (assoc-ref arch-sizeof-map arch-info-host))
	 
(define alignof-map/native-arch
  (assoc-ref arch-alignof-map arch-info-host))

(define *arch* (make-parameter (cons sizeof-map/native alignof-map/native)))

(define-syntax-rule (with-arch arch body ...)
  (parameterize ((*arch* (if (string? arch) (lookup-arch arch) arch)))
    body ...))

;; @deffn {Procedure} sizeof-basetype type
;; Return the size in bytes of the basetype @var{type}, a string, based on
;; @var{*arch*}, a global parameter, which must be a pair.   If the car
;; is a pair, it must be an a-list mapping string to integer for
;; the size.  If the car is a procedure, the procedure must take @var{type}
;; and return an integer.
;; @end deffn
(define (sizeof-basetype type)
  (let ((dict-or-proc (car (*arch*))))
    (cond
     ((pair? dict-or-proc) (assoc-ref dict-or-proc type))
     ((procedure? dict-or-proc) (dict-or-proc type))
     (else (throw 'nyacc-error "bad arch")))))

;; @deffn {Procedure} alignof-basetype type
;; Return the alignment of the basetype @var{type}, a string, based on
;; @var{*arch*}, a global parameter, whcih must be a pair.  If the cdr
;; is a pair, it must be an a-list mapping strint to integer for
;; the size.  If the car is a procedure, the procedure must take @var{type}
;; and return an integer.
;; @end deffn
(define (alignof-basetype type)
  (let ((dict-or-proc (cdr (*arch*))))
    (cond
     ((pair? dict-or-proc) (assoc-ref dict-or-proc type))
     ((procedure? dict-or-proc) (dict-or-proc type))
     (else (throw 'nyacc-error "bad arch")))))

;; defs __SIZEOF_ + + __
;; FLOAT80 INT POINTER LONG LONG_DOUBLE SIZE_T WINT_T PTRDIFF_T FLOAT FLOAT128
;; SHORT INT128 WCHAR_T DOUBLE LONG_LONG

;; --- last line ---
