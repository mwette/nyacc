;;; system

;; Copyright (C) 2016-2019,2022-2024 Matthew Wette
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

;;; Code:

(define-module (system ffi-help-rt)
  #:export (*ffi-help-version*
            make-fht fh-type? fherr
            pointer-to value-at fh-cast fh-varg

            fh-object? fh-object-ref fh-object-set!
            fh-object-sel fh-object-val fh-object-type
            fh-wrap fh-unwrap fht-wrap fht-unwrap 

            make-fhval fhval? fhval-ref fhval-set! fhval* fhval&
            fhval-sel fhval-addr fhval-base-type fhval-pointer-type

            NULL !0 ffi-void*

            unwrap~number unwrap~pointer unwrap~array unwrap~function*

            ;; called from output of the ffi-compiler
            define-fh-pointer-type
            define-fh-type-alias
            define-fh-compound-type
            define-fh-vector-type
            define-fh-function-type
            fh-ref<=>deref!
            make-symtab-function

            ;; commonly used libc functions
            fopen fclose)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (srfi srfi-9)
  #:version (2 00 4))

(define *ffi-help-version* "2.00.4")

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

(define (fherr fmt . args)
  (throw 'ffi-help-error (apply simple-format #f fmt args)))


;; --- bytestructure support --------------------------------------------------

(use-modules (bytestructures guile))
(use-modules (bytestructures guile ffi))

;; some adopted from https://github.com/TaylanUB covered by GPL3+ and
;; Copyright (C) 2015 Taylan Ulrich BayirliKammer <taylanbayirli@gmail.com>

(define bytevector-address-ref
  (case (ffi:sizeof '*)
    ((1) bytevector-u8-ref)
    ((2) bytevector-u16-native-ref)
    ((4) bytevector-u32-native-ref)
    ((8) bytevector-u64-native-ref)))

(define bytevector-address-set!
  (case (ffi:sizeof '*)
    ((1) bytevector-u8-set!)
    ((2) bytevector-u16-native-set!)
    ((4) bytevector-u32-native-set!)
    ((8) bytevector-u64-native-set!)))

(define-record-type <function-metadata>
  (make-function-metadata proc->ptr ptr->proc)
  function-metadata?
  (proc->ptr function-metadata-proc->ptr)
  (ptr->proc function-metadata-ptr->proc))
(export function-metadata?
        function-metadata-proc->ptr
        function-metadata-ptr->proc)

;; @deffn {Procedure} fh:function proc->ptr ptr->proc
;; This is a pseudo-object.  It needs to have size for bogus allocation.
;; @end deffn
(define (fh:function proc->ptr ptr->proc)
  (define size (ffi:sizeof '*))
  (define alignment size)
  (define (getter syntax? bytevector offset)
    (when syntax? (throw 'ffi-help-error "fh:function* has no macros"))
    (fherr "fh:function ref outsize of fhval-ref, I think"))
  (define (setter syntax? bytevector offset)
    (when syntax? (throw 'ffi-help-error "fh:function* has no macros"))
    (fherr "fh:function set! outsize of fhval-set!, I think"))
  (define meta (make-function-metadata proc->ptr ptr->proc))
  (make-bytestructure-descriptor size alignment #f getter setter meta))
(export fh:function)


;; ---- hookups ----------------------------------------------------------------

;; make-fhval               : make underlying type (bytestructure)
;; fhval?                   : an underlying type (bytestructure?)
;; fhval-ref                : ref
;; fhval-set!               : set!
;; fhval*                   : dereference a pointer
;; fhval&                   : pointer to value
;; fhval-sel                : like ref, but don't convert
;; fhval-addr               : provide underlying address
;; fhval-base-type          : generate C base type for a fhval
;; fhval-pointer-type       : generate pointer type from type

;; @deffn {Procedure} make-fhval desc [arg] [#:name name])
;; FIXME
;; @end deffn
(define make-fhval
  (case-lambda 
    ((desc arg)
     (let ((meta (bytestructure-descriptor-metadata desc)))
       (cond
        ((bytestructure? arg) arg)
        ((ffi:pointer? arg) (bytestructure desc (ffi:pointer-address arg)))
        ((vector-metadata? meta)
         (let* ((ln (vector-metadata-length meta)))
           (cond
            ((and (zero? ln) (integer? arg) (positive? arg))
             (let* ((eltt (vector-metadata-element-descriptor meta))
                    (eltd (bs:vector arg eltt))
                    (elts (bytestructure-descriptor-size eltt))
                    (bvec (make-bytevector (* elts arg))))
               (make-bytestructure bvec 0 eltd)))
            (arg (fherr "make-fhval: can't do vector args")))))
        ((procedure? arg)
         (let* ((proc->ptr
                 (cond
                  ((function-metadata? meta)
                   (function-metadata-proc->ptr meta))
                  ((and (pointer-metadata? meta)
                        (function-metadata?
                         (bytestructure-descriptor-metadata
                          (pointer-metadata-content-descriptor meta))))
                   (function-metadata-proc->ptr
                    (bytestructure-descriptor-metadata
                     (pointer-metadata-content-descriptor meta))))
                  (else (fherr "make-fhval: bad arg"))))
                (ptr (proc->ptr arg))
                (bvec
                 (cond
                  ((function-metadata? meta)
                   (ffi:pointer->bytevector ptr (ffi:sizeof '*)))
                  ((pointer-metadata? meta)
                   (let ((bv (make-bytevector (ffi:sizeof '*))))
                     (bytevector-address-set! bv 0 (ffi:pointer-address ptr))
                     bv)))))
           (make-bytestructure bvec 0 desc)))
        (else (bytestructure desc arg)))))
    ((desc) (bytestructure desc))))

(define (fhval? val)
  (bytestructure? val))

;; @deffn {Syntax} fhval-ref obj tag ...
;; Get equivalent Guile object, if applicable, @code{#f} otherwise.
;; For pointers, return the Guile pointer not the address.
;; @end deffn
(define-syntax-rule (fhval-ref val tag ...)
  (call-with-values
      (lambda () (bytestructure-unwrap val tag ...))
    (lambda (bvec oset desc)
      (let ((getter (bytestructure-descriptor-getter desc)))
        (cond
         ((not getter) (make-bytestructure bvec oset desc))
         ((pointer-metadata? (bytestructure-descriptor-metadata desc))
          (ffi:make-pointer (getter #f bvec oset)))
         (else (getter #f bvec oset)))))))

;; @deffn {Syntax} fhval-set! val arg tag ...
;; Set the object value from a Scheme object.
;; If you are using @code{'*} you probably don't intend to:
;; look at @code{value-at}.  
;; @end deffn
(define-syntax-rule (fhval-set! val tag ... arg)
  ;; prob use fhval-sel 
  (let ((rval val)
        (rarg (cond ((ffi:pointer? arg) (ffi:pointer-address arg)) (else arg))))
    (bytestructure-set! rval tag ... rarg)
    #;(call-with-values
        (lambda () (bytestructure-unwrap val tag ...))
      (lambda (bvec oset desc)
        (let ((setter (bytestructure-descriptor-setter desc)))
          (setter #f bvec oset rarg))))))

;; @deffn {Procedure} fhval* obj
;; dereference a pointer
;; @end deffn
(define (fhval* val)
  (fhval-sel val '*))

(define (NEWfhval* val)
  (let* ((desc (bytestructure-descriptor val))
         (meta (bytestructure-descriptor-metadata desc)))
    (cond
     ((function-metadata? meta)
      (let* ((bv (bytestructure-bytevector val))
             (bv* (ffi:bytevector->pointer bv)))
        ((function-metadata-ptr->proc meta) bv*)))
     (else
      (fhval-sel val)))))

;; @deffn {Procedure} fhval& obj
;; generate pointer to value
;; @end deffn
(define (fhval& val)
  (let* ((desc (bytestructure-descriptor val))
         (meta (bytestructure-descriptor-metadata desc))
         (desc* (bs:pointer desc))
         (bvec (bytestructure-bytevector val))
         (bptr (ffi:bytevector->pointer bvec))
         (bv* (make-bytevector (ffi:sizeof '*))))
    (bytevector-address-set! bv* 0 (ffi:pointer-address bptr))
    (make-bytestructure bv* 0 desc*)))

;; @deffn {Syntax} fhval-sel val tag ...)
;; Select the underlying value at the end of the @var{tag ...} selector.
;; @end deffn
(define-syntax-rule (fhval-sel val tag ...)
  (call-with-values
      (lambda () (bytestructure-unwrap val tag ...))
    (lambda (bvec oset desc)
      (make-bytestructure bvec oset desc))))

;; @deffn {Procedure} fhval-addr val)
;; Return the underlying numeric address of the data.
;; @end deffn
(define (fhval-addr val)
  (call-with-values
      (lambda () (bytestructure-unwrap val))
    (lambda (bv offs desc)
      (+ (ffi:pointer-address (ffi:bytevector->pointer bv)) offs))))


(define bs-base-type-map
  `((void* . *) (char . ,int8) (unsigned-char . ,uint8) (int8_t . ,int8)
    (uint8_t . ,uint8) (int16_t . ,int16) (uint16_t . ,uint16)
    (int32_t . ,int32) (uint32_t . ,uint32) (int64_t . ,int64)
    (uint64_t . ,uint64) (float . ,float32) (double . ,float64)
    (short . ,short) (unsigned-short . ,unsigned-short) (int . ,int)
    (unsigned-int . ,unsigned-int) (long . ,long)
    (unsigned-long . ,unsigned-long) (long-long . ,long-long)
    (unsigned-long-long . ,unsigned-long-long) (intptr_t . ,intptr_t)
    (uintptr_t . ,uintptr_t) (size_t . ,size_t) (ssize_t . ,ssize_t)
    (ptrdiff_t . ,ptrdiff_t) (_Bool . ,int8) (bool . ,int8)
    (signed-char . ,int8) (long-double . #f) (wchar_t . ,int32)
    (char16_t . ,int16) (char32_t . ,int32)))

(define base-type-alias-map
  '((signed-short . short) (short-int . short) (signed-short-int . short)
    (unsigned-short-int . unsigned-short) (signed . int) (signed-int . int)
    (unsigned . unsigned-int) (long-int . long) (signed-long . long)
    (signed-long-int . long) (unsigned-long-int . unsigned-long)
    (long-long-int . long-long) (signed-long-long . long-long)
    (signed-long-long-int . long-long)
    (unsigned-long-long-int . unsigned-long-long)))

(define-syntax-rule (fhval-base-type ctype)
  (or (assq-ref bs-base-type-map ctype)
      (assq-ref bs-base-type-map (assq-ref base-type-alias-map ctype))))

(define-syntax-rule (fhval-pointer-type desc)
  (bs:pointer desc))


;; ----------------------------------------------------------------------------

;; The FFI helper uses a base type based on Guile structs and vtables.
;; The base vtable uses these (lambda (obj) ...) fields:
;; 0 unwrap     : convert helper-type object to ffi argument
;; 1 wrap       : convert ffi object to helper-type object
;; 2 pointer-to : (pointer-to <foo_t-obj>) => <foo_t*-obj>
;; 3 value-at   : (value-at <foo_t*-obj>) => <foo_t-obj>
;; The C-based (child) types will add a slot for the object value.
(define ffi-helper-type
  (make-vtable
   (string-append standard-vtable-fields "pwpwpwpw")
   (lambda (v p) (display "#<ffi-helper-type>" p))))

;; @deffn {Procedure} fh-type? type
;; This predicate tests for FH types.
;; @end deffn
(define (fh-type? type)
  (and (struct-vtable? type)
       (eq? (struct-vtable type) ffi-helper-type)))

;; @deffn {Procedure} fh-object? obj
;; This predicate tests for FH objects, i.e., FFI defined types.
;; @example
;; (define-fh-pointer-type foo_t* foo_t*-desc)
;; (define val (make-foo_t*))
;; (fh-object? val) => #t
;; @end example
;; @end deffn
(define (fh-object? obj)
  (and (struct? obj)
       (fh-type? (struct-vtable obj))))

;; @deffn {Procedure} fh-object-val obj
;; Return the object value slot for the FH object.
;; @deffn
(define (fh-object-val obj)
  (unless (fh-object? obj) (fherr "fh-object-val: bad arg"))
  (struct-ref obj 0))

;; @deffn {Procedure} fh-object-ref obj arg ...
;; This returns a Guile object if appropriate, otherwise the underlying
;; type-system value.  Not great, so maybe cdata approach will be better.
;; @end deffn
(define-syntax-rule (fh-object-ref obj tag ...)
  (let ((robj obj))
    (cond
     ((fh-object? robj) (fhval-ref (struct-ref robj 0) tag ...))
     ((fhval? robj) (fhval-ref robj tag ...))
     (else (fherr "fh-object-ref: bad obj arg")))))

;; @deffn {Procedure} fh-object-set! obj arg ...
;; I'm sad that I did it this way.  Oh well.
;; @end deffn
(define-syntax-rule (fh-object-set! obj tag ... val)
  (let ((robj obj)
        (rval (cond ((fh-object? val) (fh-object-ref val)) (else val))))
    (cond
     ((fhval? robj) (fhval-set! robj tag ... rval))
     ((fh-object? robj) (fhval-set! (struct-ref robj 0) tag ... rval))
     (else (fherr "fh-object-set!: bad obj arg")))))

(define-syntax-rule (fh-object-sel obj tag ...)
  (let ((robj obj))
    (cond
     ((fh-object? robj) (fhval-sel (struct-ref robj 0) tag ...))
     ((fhval? robj) (fhval-sel robj tag ...))
     (else (fherr "fh-object-sel: bad argument")))))

(define unwrap-ix 0)
(define wrap-ix 1)
(define pointer-to-ix 2)
(define value-at-ix 3)

;; fh-type accessors
(define (fht-unwrap type)
  (struct-ref type (+ vtable-offset-user unwrap-ix)))
(define (fht-wrap type)
  (struct-ref type (+ vtable-offset-user wrap-ix)))
(define (fht-pointer-to type)
  (struct-ref type (+ vtable-offset-user pointer-to-ix)))
(define (fht-value-at type)
  (struct-ref type (+ vtable-offset-user value-at-ix)))
(define (fht-printer type)
  (struct-ref type vtable-index-printer))
(export fht-pointer-to)

;; execute the type method on the object
(define (fh-unwrap type obj)
  ((fht-unwrap type) obj))
(define (fh-wrap type val)
  ((fht-wrap type) val))

;; @deffn {Syntax} make-fht name unwrap wrap pointer-to value-at printer
;; We call make-struct here but we are actually making a vtable
;; We should check with struct-vtable?
;; name as symbol
(define* (make-fht name unwrap wrap pointer-to value-at printer)
  (let* ((ty (make-struct/no-tail
              ffi-helper-type
              (make-struct-layout "pw") ;; 1 slot for value
              printer
              (or unwrap (lambda (obj) (fherr "no unwrapper")))
              (or wrap (lambda (obj) (fherr "no wrapper")))
              (or pointer-to (lambda (obj) (fhval& (struct-ref obj 0))))
              (or value-at (lambda (obj) (fhval* (struct-ref obj 0))))))
         (vt (struct-vtable ty)))
    (set-struct-vtable-name! vt name)
    ty))

;; @deffn {Syntax} fh-ref<=>deref! p-type p-make type make
;; This procedure will ``connect'' the two types so that the procedures
;; @code{pointer-to} and @code{value-at} work.
;; @end deffn
(define (fh-ref<=>deref! p-type p-make type make)
  (if p-make
      (struct-set! type (+ vtable-offset-user 2) ; pointer-to
                   (lambda (obj) (p-make (fhval& (fh-object-val obj))))))
  (if make
      (struct-set! p-type (+ vtable-offset-user 3) ; value-at
                   (lambda (obj) (make (fhval* (fh-object-val obj)))))))

;; Right now this returns a ffi pointer.
;; TODO: add field option so we can do (pointer-to xstr 'vec) ??
(define (pointer-to obj)
  (unless (fh-object? obj) (fherr "pointer-to: not an object: ~s" obj))
  ((fht-pointer-to (struct-vtable obj)) obj))

(define (value-at obj)
  (unless (fh-object? obj) (fherr "value-at: not an object: ~s" obj))
  ((fht-value-at (struct-vtable obj)) obj))

(define NULL ffi:%null-pointer)
(define (!0 v) (not (zero? v)))
;;(define FALSE 0)
;;(define TRUE 1)


;; === unwrappers

;; unwrap~number
;; unwrap~pointer
;; unwrap~array
;; unwrap~compound
;; unwrap~function

;; wrap/unwrap enums !!!

(define (unwrap~number arg)
  (cond
   ((number? arg) arg)
   ((fh-object? arg) (fh-object-ref arg))
   ((fhval? arg) (fhval-ref arg))
   (else (fherr "unwrap~~number: bad arg: ~s" arg))))

(define (unwrap~pointer arg)
  (cond
   ((ffi:pointer? arg) arg)
   ((string? arg) (ffi:string->pointer arg))
   ((fh-object? arg) (fh-object-ref arg))
   ((fhval? arg) (fhval-ref arg))
   (else (fherr "unwrap~~pointer: bad arg: ~s" arg))))

(define (unwrap~array arg)
  (cond
   ((array? arg) (ffi:scm->pointer arg))
   ((fh-object? arg) (fh-object-ref (pointer-to arg)))
   ((fhval? arg) (fhval-ref (fhval& arg)))
   (else (fherr "unwrap~~array: bad arg: ~s" arg))))

(define (unwrap~function* obj)
  (cond
   (else (fherr "unwrap~~function*: bad arg: ~s" obj))))


;; === objects ============

(eval-when (expand load eval)
  (define (gen-id tid . args)
    (define (arg->str arg)
      (cond
       ((string? arg) arg)
       ((symbol? arg) (symbol->string arg))
       (else (symbol->string (syntax->datum arg)))))
    (let ((strid (apply string-append (map arg->str args))))
      (datum->syntax tid (string->symbol strid)))))

;; @deffn {Procedure} fh-object-type obj
;; return the object type
;; @end deffn
(define (fh-object-type obj)
  (unless (fh-object? obj) (fherr "fh-object-type: expecting fh-object arg"))
  (struct-vtable obj))

(define (make-printer type)
  (lambda (obj port)
    (unless (fh-object? obj) (fherr "fh printer: expecting ~s, got ~s" type obj))
    (display "#<" port)
    (display type port)
    (display " 0x" port)
    (display (number->string (fhval-addr (struct-ref obj 0)) 16) port)
    (display ">" port)))
(export make-printer)

;; show what it points to
(define (make-pointer-printer type)
  (lambda (obj port)
    (unless (fh-object? obj) (fherr "fh printer: expecting ~s, got ~s" type obj))
    (display "#<" port)
    (display type port)
    (display " 0x" port)
    (display (number->string
              (ffi:pointer-address (fhval-ref (struct-ref obj 0))) 16) port)
    (display ">" port)))

;;;
;; make-<type> arg:
;; 1) what if (fh-object? arg) a: identity, b: new from val, c: #f
;; 2) fhval 

;; @deffn {Syntax} define-fh-pointer-type name desc type? make
;; @example
;; (define foo_t*-desc (bs:pointer foo_t-desc))
;; (define-fh-pointer-type foo_t*
;; @end example
;; The second form is based on already defined @code{bs:pointer} descriptor.
;; @end deffn
(define-syntax-rule (define-fh-pointer-type type desc type? make)
  (begin
    (define type
      (make-fht (quote type)
                (lambda (obj) (unwrap~pointer obj))
                (lambda (val) (make val))
                #f #f
                (make-pointer-printer (quote type))))
    (define (type? obj)
      (and (fh-object? obj) (eq? (struct-vtable obj) type)))
    (define make
      (case-lambda
        ((arg)
         (cond ;; need to boil down to the raw pointer
          ((fh-object? arg)
           (make-struct/no-tail type (make-fhval desc (fh-object-ref arg))))
          ((fhval? arg)
           (make-struct/no-tail type (make-fhval desc (fhval-ref arg))))
          (else
           (make-struct/no-tail type (make-fhval desc arg)))))
        (() (make-struct/no-tail type (make-fhval desc ffi:%null-pointer)))))))

;; @deffn {Syntax} define-fh-compound-type type desc type? make
;; Generates an FH aggregate type based on the underlying type.
;; @end deffn
(define-syntax-rule (define-fh-compound-type type desc type? make)
  (begin
    (define type
      (make-fht (quote type)
                (lambda (obj) (fhval-ref (fh-object-val obj)))
                (lambda (val) (make val))
                #f #f
                (make-printer (quote type))))
    (define (type? obj)
      (and (fh-object? obj) (eq? (struct-vtable obj) type)))
    (define make
      (case-lambda
        ((arg)
         (cond
          ((fh-object? arg) (make-struct/no-tail type (fh-object-val arg)))
          ((fhval? arg) (make-struct/no-tail type arg))
          (else (make-struct/no-tail type (make-fhval desc arg)))))
        (() (make-struct/no-tail type (make-fhval desc)))))))

;; @deffn {Syntax} define-fh-vector-type type desc type? make
;; The @var{desc} argument is NOT for the element type.   The integer
;; number of elements is determine when a @code{make-type} is used???
;; @end deffn
(define-syntax-rule (define-fh-vector-type type desc type? make)
  (begin
    (define type
      (make-fht (quote type)
                (lambda (obj) (fhval-ref (fh-object-val obj)))
                (lambda (arg) (make arg))
                #f #f
                (make-printer (quote type))))
    (define (type? obj)
      (and (fh-object? obj) (eq? (struct-vtable obj) type)))
    (define make
      (case-lambda
        ((arg)
         (cond
          ((fh-object? arg) (make-struct/no-tail type (fh-object-val arg)))
          ((fhval? arg) (make-struct/no-tail type arg))
          (else (make-struct/no-tail type (make-fhval desc arg)))))
        (() (make-struct/no-tail type (make-fhval desc)))))))

;; @deffn {Syntax} define-fh-type-alias alias type
;; set up type alias.  Caller needs to match type? and make.
;; This is one of the places we use generated id's.
;; The following are generated: @emph{alias} @code{make-}@emph{alias}
;; @emph{alias}@code{?}.  This macro is not hygenic.
;; @end deffn
(define-syntax define-fh-type-alias
  (lambda (x)
    (syntax-case x ()
      ((_ alias type)
       (let ((desc (gen-id x #'alias "-desc"))
             (pred (gen-id x #'alias "?"))
             (make (gen-id x "make-" #'alias)))
         #`(begin
             (define alias
               (make-fht (quote alias)
                         (fht-unwrap type)
                         (lambda (arg) (#,make arg))
                         #f #f
                         (make-printer (quote alias))))
             (define (#,pred obj)
               (and (fh-object? obj) (eq? (struct-vtable obj) alias)))
             (define #,make
               (case-lambda
                 ((arg)
                  (cond
                   ((fh-object? arg)
                    (make-struct/no-tail alias (fh-object-ref arg)))
                   ((fhval? arg) (make-struct/no-tail alias arg))
                   (else (make-struct/no-tail alias (make-fhval #,desc arg)))))
                 (() (make-struct/no-tail alias (make-fhval #,desc)))))
             (export #,pred #,make)))))))

;; @deffn {Syntax} define-fh-function-type type desc pred make
;; machine code for a function, but we only keep one word
;; bytevector->pointer will give us the address
;; @end deffn
(define-syntax-rule (define-fh-function-type type desc pred make)
  (begin
    (define type
      (make-fht (quote type)
                (lambda (obj) (error "not expected"))
                (lambda (arg) (make-struct/no-tail type (make-fhval desc arg)))
                #f #f
                (make-printer (quote type))))
    (define (type? obj)
      (and (fh-object? obj) (eq? (struct-vtable obj) type)))
    (define (make arg) (make-struct/no-tail type (make-fhval desc arg)))))

;; @deffn {Syntax} define-fh-base-type type
;; This generates a local-only type but exports the predicate and generator.
;; So,  @emph{type}@code{?} @code{make-}@emph{type} and
;; and  @emph{type}@code{*?} @code{make-}@emph{type}@code{*} are exported.
;; @end deffn
(define-syntax define-base-type
  (lambda (x)
    (syntax-case x ()
      ((_ type)
       (with-syntax ((~type (gen-id #'type "~" #'type)) ; hidden type
                     (desc (gen-id #'type #'type "-desc"))
                     (type? (gen-id #'type #'type "?"))
                     (make (gen-id #'type "make-" #'type))
                     (type* (gen-id #'type #'type "*"))
                     (desc* (gen-id #'type #'type "*-desc"))
                     (type*? (gen-id #'type #'type "*?"))
                     (make* (gen-id #'type "make-" #'type "*")))
         #'(begin
             (define desc (fhval-base-type (quote type)))
             (define ~type
               (make-fht (quote type)
                         (lambda (obj) (fhval-ref (fh-object-val obj)))
                         (lambda (arg) (make arg))
                         (lambda (obj) (make* (fhval& (fh-object-val obj))))
                         #f
                         (make-printer (quote type))))
             (define (type? obj)
               (and (fh-object? obj) (eq? (struct-vtable obj) ~type)))
             (define-public make
               (case-lambda
                 ((arg)
                  (cond
                   ((fh-object? arg)
                    (make-struct/no-tail ~type (fh-object-ref arg)))
                   ((fhval? arg) (make-struct/no-tail ~type arg))
                   (else (make-struct/no-tail ~type (make-fhval desc arg)))))
                 (() (make-struct/no-tail ~type (make-fhval desc)))))
             (define desc* (fhval-pointer-type desc))
             (define type*
               (make-fht (quote type*)
                         (lambda (obj) (unwrap~pointer obj))
                         (lambda (val) (make* val))
                         #f
                         (lambda (obj) (make (fhval* (fh-object-val obj))))
                         (make-pointer-printer (quote type*))))
             (define (type*? obj)
               (and (fh-object? obj) (eq? (struct-vtable obj) type*)))
             (define make*
               (case-lambda
                 ((arg)
                  (cond
                   ((fh-object? arg)
                    (make-struct/no-tail type* (fh-object-ref arg)))
                   ((fhval? arg) (make-struct/no-tail type* arg))
                   (else (make-struct/no-tail type* (make-fhval desc* arg)))))
                 (() (make* ffi:%null-pointer))))
             (export desc type? make desc* type*? make*)))))))

(define-base-type short) (define-base-type unsigned-short)
(define-base-type int) (define-base-type unsigned-int)
(define-base-type long) (define-base-type unsigned-long)
(define-base-type long-long) (define-base-type unsigned-long-long)
(define-base-type float) (define-base-type double)

(define-base-type int8_t) (define-base-type uint8_t)
(define-base-type int16_t) (define-base-type uint16_t)
(define-base-type int32_t) (define-base-type uint32_t)
(define-base-type int64_t) (define-base-type uint64_t)

(define-base-type intptr_t) (define-base-type uintptr_t)
(define-base-type size_t) (define-base-type ssize_t)
(define-base-type ptrdiff_t)
(define-base-type char) (define-base-type unsigned-char)
(define-base-type _Bool) (define-base-type bool)

(define-fh-pointer-type void* (fhval-pointer-type 'void) void*? make-void*)

(let ((was-make-char* make-char*))
  (set! make-char* 
        (case-lambda
          ((arg)
           (was-make-char* (if (string? arg) (ffi:string->pointer arg) arg)))
          (() (was-make-char*)))))

(define char**-desc (fhval-pointer-type char*-desc))
(define-fh-pointer-type char** char**-desc char**? make-char**)
(fh-ref<=>deref! char** make-char** char* make-char*)
(export char**? make-char**)


;; @deffn {Syntax} fh-cast type value
;; Cast to new type.
;; Example: Given @code{bar} of type @code{Bar*}:
;; @example
;; (fh-cast Foo* bar) => <Foo* 0xabcd1234>
;; @end example
;; @end deffn
;; C allows cast of base types and pointer types
;; look at define-fh-pointer-type make
(define-syntax fh-cast
  (lambda (x)
    "- Syntax: fh-cast type value
     Cast to new type.  Always a pointer, unless I missed something.
     Example: Given ‘bar’ of type ‘Bar*’:
          (fh-cast Foo* bar) => <Foo* 0xabcd1234>"
    (syntax-case x ()
      ((_ type expr)
       #`(#,(gen-id x "make-" #'type) (fh-object-ref expr))))))
(export fh-cast)

;; @deffn {Procedure} fh-varg type value
;; Generate variadic argument for variadic procedure.
;; @example
;; (fh-cast foo_desc_t* 321)
;; (use-modules ((system foreign) #:prefix 'ffi:))
;; (fh-varg ffi:short 321)
;; We might have a procedure that wants be passed as a pointer but
;; @end deffn
;; use cases
;; @itemize
;; @item
;; @example
;; (lambda (x y) #f) => (procedure->pointer void (list '* '*))
;; @end example
;; @end itemize
;; can we now do a vector->pointer
(define (fh-varg type expr)
  (cons type expr))


;; --- random stuff --------------------

(define ffi-void* '*)

(define-public (char*->string obj)
  (ffi:pointer->string (fh-object-ref obj)))

;; @deffn {Procedure} make-argv str-list => char**
;; For C functions that take an argument of the form @code{const char *names[]},
;; this routine will convert a scheme list of strings into an appropriate
;; low-level @code{char**} type which can be passed via @code{unwrap~pointer}.
;; @end deffn
(define-public (make-argv str-list)
  "- Procedure: make-argv str-list => char**
     For C functions that take an argument of the form ‘const char
     *names[]’, this routine will convert a scheme list of strings into
     an appropriate low-level ‘char**’ type which can be passed via
     ‘unwrap~pointer’."
  (let* ((n (length str-list))
         (ptrsz (ffi:sizeof '*))
         (addresses (map (compose ffi:pointer-address ffi:string->pointer)
                         str-list))
         (bv (make-bytevector (* n ptrsz)))
         (bv-set! (case ptrsz
                    ((4) bytevector-u32-native-set!)
                    ((8) bytevector-u64-native-set!))))
    (for-each (lambda (address index) (bv-set! bv (* ptrsz index) address))
              addresses (iota n))
    (make-char** (ffi:bytevector->pointer bv))))

;; @deffn {Procedure} make-symtab-function symbol-value-table prefix
;; generate a symbol table function
;; @example
;; (define-public BUS (make-symtab-function ffi-dbus-symbol-tab))
;; @end example
;; Then use in code as this:
;; @example
;; (define bus (DBUS 'SERVICE_BUS))
;; @end example
;; @noindent
;; which is equivalent to
;; @example
;; (define bus (ffi-dbus-symbol-val 'DBUS_SERVICE_BUS)
;; @end example
;; @end deffn
(define (make-symtab-function symbol-value-table prefix)
  (let* ((cnvt (lambda (pair seed)
                 (let* ((k (car pair)) (v (cdr pair))
                        (n (symbol->string k))
                        (l (string-length prefix)))
                   (if (string-prefix? prefix n)
                       (acons (string->symbol (substring n l)) v seed)
                       seed))))
         (symtab (let loop ((o '()) (i symbol-value-table))
                   (if (null? i) o (loop (cnvt (car i) o) (cdr i))))))
    (lambda (key) (assq-ref symtab key))))


;; === common c functions called

;; @deffn {Procedure} fopen filename mode
;; Call the C fucntion fopen and return a scheme @code{<pointer>} type.
;; @end deffn
(define fopen
  (let ((~fopen (ffi:pointer->procedure
                 '* (dynamic-func "fopen" (dynamic-link)) (list '* '*))))
    (lambda (filename mode)
      (~fopen (ffi:string->pointer filename) (ffi:string->pointer mode)))))

;; @deffn {Procedure} fopen file
;; Call the C fucntion fclose on @var<file>, a @code{<pointer>} type generated
;; by @code{fopen}.
;; @end deffn
(define fclose
  (let ((~fclose (ffi:pointer->procedure
                 ffi:int (dynamic-func "fclose" (dynamic-link)) (list '*))))
    (lambda (file)
      (~fclose file))))


;; --- last line ---
