;;; nyacc/lang/arch-info.scm

(define-module (arch-info)
  #:export (%host-arch sizeof-basetype alignof-basetype
		       sizeof-map/native 
		       )
  #:use-module (system foreign))

(use-modules (ice-9 pretty-print))
(define pp pretty-print)
(define (sf fmt . args) (apply simple-format #t fmt args))

(define sizeof-map/x86_64
  `((* . 8)
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

(define sizeof-map/avr
  `(('* . 2)
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

(define arch-sizeof-map
  `(("x86_64" . ,sizeof-map/x86_64)
    ("avr" . ,sizeof-map/avr)))

(define arch-alignof-map
  `(("x86_64" . ,alignof-map/x86_64)
    ("avr" . ,alignof-map/avr)))

(define %host-arch
  (eval-when (expand eval compile)
    (and=> (string-split %host-type #\-) car)))

(define sizeof-map/native
  (eval-when (expand eval compile)
    (assoc-ref arch-sizeof-map %host-arch)))
	 
(define alignof-map/native
  (eval-when (expand eval compile)
    (assoc-ref arch-alignof-map (and=> (string-split %host-type #\-) car))))
	 
(define* (sizeof-basetype type #:optional arch)
  (let ((sizeof-map (if arch
			(or (assoc-ref arch-sizeof-map arch)
			    (error "unknown architecture: " arch))
			sizeof-map/native)))
    (assoc-ref sizeof-map type)))

(define* (alignof-basetype type #:optional arch)
  (let ((alignof-map (if arch
			(or (assoc-ref arch-alignof-map arch)
			    (error "unknown architecture: " arch))
			alignof-map/native)))
    (assoc-ref alignof-map type)))

;; defs __SIZEOF_ + + __
;; FLOAT80 INT POINTER LONG LONG_DOUBLE SIZE_T WINT_T PTRDIFF_T FLOAT FLOAT128
;; SHORT INT128 WCHAR_T DOUBLE LONG_LONG

;; --- last line ---
