;;; bytestructures.scm

;; Copyright (C) 2025-2026 Matthew Wette
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Notes:
;; To convert a struct
;;   (use-modules (nyacc ffi-bkend bytestructures))
;;   (ccode->bytestructures-sexp "typedef struct { int x; int y; } foo_t;")
;; => 
;;   (begin
;;     (define foo_t (bs:struct (list `(x ,int) `(y ,int))))
;;     (define foo_t* (bs:pointer foo_t))
;;     (export foo_t foo_t*))

;;; Code:

(define-module (nyacc ffi-bkend bytestructures)
  #:export (backend ccode->bytestructures-sexp)
  #:use-module (ice-9 match)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructures guile)
  #:use-module (nyacc lang c99 ffi-help))

(use-modules (ice-9 pretty-print))
(define (pp exp) (pretty-print exp #:per-line-prefix "  "))
(define (sf fmt . args) (apply simple-format #t fmt args))

(define (header)
  `(begin
     (use-modules (bytestructures guile))
     (define (arg->number arg)
       (cond ((number? arg) arg)
             ((bytestructure? arg) (bytestructure-ref arg))
             (else (error "bytestructure-arg->number: bad arg:" arg))))
     (define* (arg->pointer arg #:optional hint)
       (cond ((ffi:pointer? arg) arg)
             ((string? arg) (ffi:string->pointer arg))
             ((equal? 0 arg) ffi:%null-pointer)
             ((bytestructure? arg)
              (let* ((desc (bytestructure-descriptor arg))
                     (meta (bytestructure-descriptor-metadata desc)))
                (cond
                 ((pointer-metadata? meta)
                  (ffi:make-pointer (bytestructure-ref arg)))
                 ((or (vector-metadata? meta)
                      (struct-metadata? meta)
                      (union-metadata? meta))
                  (call-with-values
                      (lambda () (bytestructure-unwrap arg))
                    (lambda (bv ix ds)
                      (ffi:make-pointer (+ (ffi:bytevector->pointer bv) ix)))))
                 (else (error "bytestrucgture-arg->pointer; bad arg:" arg)))))
             (else (error "bytestructure-arg->pointer: bad arg:" arg))))
     (define (extern-ref obj) (bytestructure-ref obj '*))
     (define (extern-set! obj val) (bytestructure-set! obj '* val))))

(define (trailer defs)
  (let ((sym->val (or (assq-ref defs 'sym->val) '(const #f))))
    `(define (unwrap-enum arg)
       (cond
        ((number? arg) arg)
        ((symbol? arg) (,sym->val arg))
        ((bytestructure? arg) (bytestructure-ref arg))
        (else (error "type mismatch"))))))

(define (no-base name)
  (fherr/once "no backend type for ~a" name))

(define (base name)
  (case name
    ((void) ''void)
    ((char) 'int8)
    ((signed-char) 'int8)
    ((unsigned-char) 'uint8)
    ((short) 'short)
    ((unsigned-short) 'short)
    ((int) 'int)
    ((unsigned) 'unsigned-int)
    ((long) 'long)
    ((unsigned-long) 'unsigned-long)
    ((long-long) 'long-long)
    ((unsigned-long-long) 'unsigned-long-long)
    ((float) 'float)
    ((double) 'double)
    ((int8_t) 'int8)
    ((uint8_t) 'uint8)
    ((int16_t) 'int16)
    ((uint16_t) 'uint16)
    ((int32_t) 'int32)
    ((uint32_t) 'uint32)
    ((int64_t) 'int64)
    ((uint64_t) 'uint64)
    ((size_t) 'size_t)
    ((ssize_t) 'ssize_t)
    ((ptrdiff_t) 'ptrdiff_t)
    ((intptr_t) 'intptr_t)
    ((uintptr_t) 'uintptr_t)
    ((_Bool bool) 'int8)
    ((wchar_t) 'uint32)
    ((char16_t) 'uint16)
    ((char32_t) 'uint32)
    ((long-double) (no-base name))
    ((_Float16) (no-base name))
    ((_Float128) (no-base name))
    ((float-_Complex) 'complex64)
    ((double-_Complex) 'complex128)
    ((long-double-_Complex) (no-base name))
    ((__int128) (no-base name))
    ((unsigned-__int128) (no-base name))
    (else (no-base name))))

(define (array type dim)
  `(bs:vector ,dim ,type))

(define* (struct fields #:optional packed)
  (let ((flds (map (match-lambda
                     (`(,qq (,nm (,uq (bitfld ,ty ,sz))))
                      `(,qq (,nm (,uq ,ty) ,sz)))
                     (`(,qq (,nm (,uq ,ty)))
                      `(,qq (,nm (,uq ,ty)))))
              fields)))
    `(bs:struct (list ,@flds))))

(define (bitfield type size)
  `(bitfld ,type ,size))

(define* (union fields #:optional packed)
  `(bs:union (list ,@fields)))

(define backend
  (make-fh-backend
   'bytestructures
   header
   trailer
   base
   array
   (lambda (type)                       ; pointer
     `(bs:pointer ,type)) 
   struct
   bitfield
   union
   (lambda (pr->pc pc->pr)              ; function
     ''void)
   (lambda* (alist #:optional packed)   ; enum
     (if packed
         (fherr "ffi/bytestructures: WARNING: packed enums not supported"))
     'int)
   (lambda (name type)                  ; deftype
     `(define ,name ,type))
   (lambda* (type #:optional value)
     (if value `(bytestructure ,type ,value) `(bytestructure ,type)))))

;; @deffn {Procedure} ccode->bytestructures-sexp code [attrs] => sexp
;; Convert @var{code}, a string of C code, to a s-expression of
;; @emph{bstructs} code, for use in Guile.  For example,
;; @example
;; (use-modules (nyacc ffi-bkend bytestructures))
;; (ccode->bytestructures-sexp "typedef struct @{ int x; int y; @} foo_t;")
;; => 
;; (begin
;;   (define foo_t (bs:struct (list `(x ,int) `(y ,int))))
;;   (define foo_t* (bs:pointer foo_t))
;;   (export foo_t foo_t*))
;; @end example
;; See nyacc documentation for @code{ccode->sexp} to obtain information
;; on the @var{attrs} argument.
;; @end deffn
(define* (ccode->bytestructures-sexp ccode #:optional (attrs '()))
  "- Procedure: ccode->bytestructures-sexp code [attrs] => sexp
     Convert CODE, a string of C code, to a s-expression of _bstructs_
     code, for use in Guile.  For example,
          (use-modules (nyacc ffi-bkend bytestructures))
          (ccode->bytestructures-sexp \"typedef struct { int x; int y; } foo_t;\")
          =>
          (begin
            (define foo_t (bs:struct (list `(x ,int) `(y ,int))))
            (define foo_t* (bs:pointer foo_t))
            (export foo_t foo_t*))
     See nyacc documentation for ‘ccode->sexp’ to obtain information on
     the ATTRS argument."
  (parameterize ((*fh-backend* backend))
    (ccode->sexp ccode attrs)))

;; --- last line ---
