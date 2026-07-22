;;; bstructs.scm -- bstructs backend for the ffi helper

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

;; Users need to understand that bstructs is it's own language on top
;; of scheme macros.  Inside define-bstruct one can only reference
;; bstruct keywords and type symbols, nothing else.  Creating a direct
;; converter is going to be tricky.  This implementation instead
;; creates cdata types (i.e., ctypes) and then feeds them to a
;; ctype->bstruct converter.  The be-routines procude ctype directly
;; (instead of sexp's) and the deftype method converts the whole
;; thing to a bstruct.

;; To convert a struct
;;   (use-modules (nyacc ffi-bkend bstructs))
;;   (ccode->bstructs-sexp "typedef struct { int x; int y;} foo_t;")
;; => 
;;   (begin
;;     (define-bstruct struct-foo (struct (x int) (y int)))
;;     (define-bstruct struct-foo* (* struct-foo))
;;     (export struct-foo struct-foo*))

;;; Code:

(define-module (nyacc ffi-bkend bstructs)
  #:export (backend ccode->bstructs-sexp)
  #:use-module (bstructs)
  #:use-module (ice-9 match)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (nyacc lang c99 ffi-help))

(use-modules (ice-9 pretty-print))
(define (pp exp) (pretty-print exp #:per-line-prefix "  "))
(define (sf fmt . args) (apply simple-format #t fmt args))

;; Instead of wrestling with bstructs language (which does not
;; compose w/ scheme easily), we process type declarations with
;; cdata and then convert at the end.

(use-modules ((nyacc foreign arch-info)))
(use-modules ((nyacc foreign cdata)))
(define %cpointer-type (@@ (nyacc foreign cdata) %cpointer-type))
(define *mod* (make-parameter #f))

(define (base name)
  (case name
    ((void) 'void)
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
    ((long-double) #f)
    ((_Float16) #f)
    ((_Float128) #f)
    ((float-_Complex) 'complex64)
    ((double-_Complex) 'complex128)
    ((long-double-_Complex) #f)
    ((__int128) #f)
    ((unsigned-__int128) #f)
    (else #f)))

(define qq 'quasiquote)
(define uq 'unquote)


(define (header)
  (*mod* (make-fresh-user-module))
  (let ((mod (*mod*)))
    (eval '(use-modules (nyacc foreign cdata)) mod)
    (for-each
     (lambda (name)
       ;;(eval `(define ,name (name-ctype ',name (cbase ',name))) mod))
       (eval `(define ,name (name-ctype ',(base name) (cbase ',name))) mod))
     (cdr base-type-symbol-list))
    (eval '(define void (name-ctype 'void (cbase 'void))) mod)
    (eval '(define void* (name-ctype 'void* (cpointer 'void))) mod))
  `(begin
     (use-modules (bstructs))
     (define (obj-type obj)
       ((@@ (bstructs) bstruct-descriptor-name) (struct-vtable obj)))
     (define-syntax-rule (arg->number arg)
       (cond ((number? arg) arg)
             ;;((bstruct? arg) (bstruct-ref (obj-type arg) arg)) nope
             (else (error "ffi-bkend/bstruct: arg->number: bad arg:" arg))))
     (define-syntax arg->pointer
       (syntax-rules ()
         ((_ arg)
          (cond ((ffi:pointer? arg) arg)
                ((string? arg) (ffi:string->pointer arg))
                ((equal? 0 arg) ffi:%null-pointer)
                (else arg)))
         ((_ arg hint) (arg->pointer arg))))
     (define-syntax-rule (extern-ref obj)
       (bstruct-ref (obj-type obj) obj '*))
     (define-syntax-rule (extern-set! obj val)
       (bstruct-set! (obj-type obj) obj '* val))))

(define (trailer defs)
  (*mod* #f)
  (let ((sym->val (or (assq-ref defs 'sym->val) '(const #f))))
    `(define (unwrap-enum arg)
       (cond
        ((number? arg) arg)
        ((symbol? arg) (,sym->val arg))
        ;;((bstruct? arg) (bstruct-ref arg)) nope
        (else (error "ffi-bkend/bstruct: type mismatch"))))))

(define (ctype->bstruct ctype)
  (define (ifor gap)
    (case gap
      ((1) 'int8)
      ((2) 'int16)
      ((4) 'int32)
      ((8) 'int64)))

  (define (cnvt-aggr type flds)
    (define mkpad
      (let ((pc 0))
        (lambda ()
          (set! pc (1+ pc))
          (string->symbol (simple-format #f "_~a" pc)))))

    (let loop ((bsl '()) (po 0) (ps 0) (bits '()) (bu #f) (bs 0) (cdl flds))
      ;; po: prev offset; ps: prev size; bits used: if bitmask
      ;; np: next pad, bs: bitfield start
      (if (pair? cdl)
          (let* ((fld (car cdl))
                 (name (cfield-name fld))
                 (type (cfield-type fld))
                 (typename (ctype-name type))
                 (offs (cfield-offset fld))
                 (size (ctype-size type))
                 (kind (ctype-kind type))
                 (info (ctype-info type)))
            (cond
             ((eq? 'bitfield kind)
              (let* ((shift (cbitfield-shift info))
                     (width (cbitfield-width info))
                     (mtype (cbitfield-mtype info))
                     (bs (if bu bs shift))
                     (sign (if (mtype-signed? mtype) 's 'u))
                     (bits (cons (list name width sign) bits)))
                (loop bsl offs size bits (+ shift width) bs (cdr cdl))))
             (bu
              (let* ((gap (- (* 8 (- offs po)) bu))
                     (bits (if (zero? gap) bits (cons (list '_ gap 's) bits)))
                     (bsl (cons `(,(mkpad) (bits ,@(reverse bits))) bsl)))
                (loop bsl offs size '() #f bs cdl)))
             (else
              (loop (cons `(,name ,(or typename (cnvt type))) bsl)
                    offs size '() #f bs (cdr cdl)))))
          (let ((gap (- (ctype-size type) (+ po ps))))
            (if (not (zero? gap))
                (reverse (cons `(_ ,(ifor gap)) bsl))
                (reverse bsl))))))

  (define (cnvt type)
    (cond
     ((symbol? type) type)
     ((ctype-name type) => identity)
     (else
      (let ((info (ctype-info type)))
        (case (ctype-kind type)
          ((base) (if (eq? info 'void) 'void (error "oops")))
          ((struct) `(struct ,@(cnvt-aggr type (cstruct-fields info))))
          ((union) `(union ,@(cnvt-aggr type (cunion-fields info))))
          ((pointer)
           (let* ((ptype (%cpointer-type info)) (pname (ctype-name ptype)))
             (cond
              ((promise? ptype) `(* void))
              (pname `(* ,pname))
              (else `(* ,(cnvt ptype))))))
          ((array) `(vector ,(carray-length info) ,(cnvt (carray-type info))))
          ((enum) (base 'int))
          ((function) (base 'void))
          (else (error "ctype->bstruct: needs work:" (ctype-kind type))))))))
  
    (cnvt ctype))
(export ctype->bstruct)

(define (as-ctype type)
  (cond
   ((ctype? type) type)
   ((symbol? type) (module-ref (*mod*) type))
   ;;((and (pair? type) (eq? 'delay (car type)))
   (else
    (sf "type: ~s\n" type)
    (error "coding error"))))

(define (deftype name type)
  (let ((rtype (if (ctype? type) type (module-ref (*mod*) type)))
        (cm #f))
    (module-define! (*mod*) name (name-ctype name rtype))
    (dynamic-wind
      (lambda () (set! cm (set-current-module (*mod*))))
      (lambda () `(define-bstruct ,name ,(ctype->bstruct rtype)))
      (lambda () (set-current-module cm)))))

(define (makeobj typename . args)
  ;;`(bstruct-alloc ,typename ,@args))
  `(identity ,@args))


(define (fix-flds fields)
  (map (lambda (f) (match f
                     (`(,uq (,n (cbitfield ,t ,s))) `(,n ,(as-ctype t) ,s))
                     (`(,qq (,n (,uq ,t))) `(,n ,(as-ctype t)))))
       fields))

(define backend
  (make-fh-backend
   'bstructs
   header
   trailer
   (lambda (name)                       ; base
     ;;(cbase name))
     name)
   (lambda (type dim)                   ; array
     (carray (as-ctype type) dim))
   (lambda (type)                       ; pointer
     (cond
      ((and (pair? type) (eq? 'delay (car type)))
       (cpointer (cbase 'void)))
      ((symbol? type) (cpointer (module-ref (*mod*) type)))
      (else (error "be-pointer failed"))))
   (lambda* (flds #:optional packed)    ; struct
     (cstruct (fix-flds flds) packed))
   (lambda (type size)                  ; bitfield
     `(cbitfield ,(as-ctype type) ,size))
   (lambda (flds)                       ; union
     (cunion (fix-flds flds)))
   (lambda (pr->pc pc->pr)              ; function
     (cfunction pr->pc pc->pr))
   (lambda* (alist #:optional packed)   ; enum
     (let ((etype (cenum alist packed)))
       (if packed 
           (case (cenum-mtype (ctype-info etype))
             ((s8) (cbase 'int8_t)) ((u8) (cbase 'uint8_t))
             ((s16) (cbase 'int16_t)) ((u16) (cbase 'uint16_t))
             ((s32) (cbase 'int32_t)) ((u32) (cbase 'uint32_t))
             ((s64) (cbase 'int64_t)) ((u64) (cbase 'uint64_t)))
           (cbase 'int))))
   deftype
   makeobj))


;; @deffn {Procedure} ccode->bstructs-sexp code [attrs] => sexp
;; Convert @var{ccode}, a string of C code, to a s-expression of
;; @emph{bstructs} code, for use in Guile.  For example,
;; @example
;; (use-modules (nyacc ffi-bkend bstructs))
;; (ccode->bstructs-sexp "typedef struct @{ int x; int y; @} foo_t;")
;; =>
;; (begin
;;   (define-bstruct struct-foo (struct (x int) (y int)))
;;   (define-bstruct struct-foo* (* struct-foo))
;;   (export struct-foo struct-foo*))
;; @end example
;; See nyacc documentation for @code{ccode->sexp} to obtain information
;; on the @var{attrs} argument.
;; @end deffn
(define* (ccode->bstructs-sexp ccode #:optional (attrs '()))
  "- Procedure: ccode->bstructs-sexp code [attrs] => sexp
     Convert CCODE, a string of C code, to a s-expression of _bstructs_
     code, for use in Guile.  For example,
          (use-modules (nyacc ffi-bkend bstructs))
          (ccode->bstructs-sexp \"typedef struct { int x; int y; } foo_t;\")
          =>
          (begin
            (define-bstruct struct-foo (struct (x int) (y int)))
            (define-bstruct struct-foo* (* struct-foo))
            (export struct-foo struct-foo*))
     See nyacc documentation for ‘ccode->sexp’ to obtain information on
     the ATTRS argument."
  (parameterize ((*fh-backend* backend)
                 (*mod* (make-fresh-user-module)))
    (let ((mod (*mod*)))
      (eval '(use-modules (nyacc foreign cdata)) mod)
      (for-each
       (lambda (name)
         ;;(eval `(define ,name (name-ctype ',name (cbase ',name))) mod))
         (eval `(define ,name (name-ctype ',(base name) (cbase ',name))) mod))
       (cdr base-type-symbol-list))
      (eval '(define void (name-ctype 'void (cbase 'void))) mod)
      (eval '(define void* (name-ctype 'void* (cpointer 'void))) mod))
    (ccode->sexp ccode attrs)))

;; --- last line ---
