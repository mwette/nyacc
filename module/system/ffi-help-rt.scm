;;; system/ffi-help-rt.scm - NYACC's FFI help runtime

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

;; desc => vtype or vt

;;; Code:

(define-module (system ffi-help-rt)
  #:export (*ffi-help-version*

            ;; user level routines
            fh-type? fh-object? fherr
            pointer-to value-at fh-cast fh-varg

            fh-object-ref fh-object-set! fh-object-sel

            fhval-ref fhval-set! fhval-sel
            fhval* fhval&

            NULL !0

            ;; maybe used outside of modules?
            ;;bs-addr
            ffi-void*
            make-fht

            unwrap~number unwrap~pointer unwrap~array unwrap~function*

            ;; called from output of the ffi-compiler
            define-fh-pointer-type
            define-fh-type-alias
            define-fh-compound-type
            define-fh-vector-type
            define-fh-function*-type
            fh-ref<=>deref!
            make-symtab-function

            ;;fh-find-symbol-addr
            fht-wrap fht-unwrap fh-wrap fh-unwrap

            ;;unwrap~fixed unwrap~float
            ;; fh-link-proc fh-link-extern

            ;; commonly used libc functions
            fopen fclose

            ;; deprecated ???
            fh-link-bstr ;; => fh-link-extern
            ref<->deref!
            make-fctn-param-unwrapper
            )
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (srfi srfi-9)
  #:version (1 09 4))

(define *ffi-help-version* "1.09.4")

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

(define (fherr fmt . args)
  (throw 'ffi-help-error (apply simple-format #f fmt args)))

(eval-when (expand load eval)
  (define (gen-id tmpl-id . args)
    (define (stx->str stx)
      (symbol->string (syntax->datum stx)))
    (datum->syntax
     tmpl-id
     (string->symbol
      (apply
       string-append
       (map (lambda (ss) (if (string? ss) ss (stx->str ss))) args))))))

;; --- bytestructure support --------------------------------------------------

(use-modules (bytestructures guile))
(use-modules (bytestructures guile ffi))

;; some adopted from https://github.com/TaylanUB covered by GPL3+ and
;; Copyright (C) 2015 Taylan Ulrich BayirliKammer <taylanbayirli@gmail.com>

(define make-pointer-metadata
  (@@ (bytestructures guile pointer) make-pointer-metadata))

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

;; @deffn {Procedure} fh:pointer descriptor
;; Define a descriptor for a pointer to descriptor (type).
;; @enumerate
;; @item if '* generate bytevector copy of object
;; @item if integer, assume it's a vector
;; @item if symbol generate new bytevector from pointer value
;; @end enumerate
;; @end deffn
(define (fh:pointer %descriptor)
  (define (pointer-ref bytevector offset content-size)
    (let ((address (bytevector-address-ref bytevector offset)))
      (if (zero? address)
          (fherr "fh:pointer: attempt to dereference null-pointer")
          (ffi:pointer->bytevector (ffi:make-pointer address) content-size))))
  (define (pointer-idx-ref bytevector offset index content-size)
    (let* ((base-address (bytevector-address-ref bytevector offset))
           (address (+ base-address (* index content-size))))
      (if (zero? base-address)
          (fherr "fh:pointer: attempt to dereference null-pointer")
          (ffi:pointer->bytevector (ffi:make-pointer address) content-size))))
  (define (pointer-set! bytevector offset value)
    (cond
     ((exact-integer? value)
      (bytevector-address-set! bytevector offset value))
     ((ffi:pointer? value)
      (bytevector-address-set! bytevector offset (ffi:pointer-address value)))
     ((string? value)
      (bytevector-address-set! bytevector offset
                               (ffi:pointer-address
                                (ffi:string->pointer value))))
     ((bytevector? value)
      (bytevector-address-set! bytevector offset
                               (ffi:pointer-address
                                (ffi:bytevector->pointer value))))
     ((bytestructure? value)
      (bytevector-address-set! bytevector offset
                               (bytestructure-ref value)))
     ((fh-object? value)
      (pointer-set! bytevector offset (fh-object-ref value)))))
  (define (get-descriptor)
    (if (promise? %descriptor)
        (force %descriptor)
        %descriptor))
  (define size (ffi:sizeof '*))
  (define alignment size)
  (define (unwrapper syntax? bytevector offset index)
    (define (syntax-list id . elements)
      (datum->syntax id (map syntax->datum elements)))
    (let ((descriptor (get-descriptor)))
      (when (eq? 'void descriptor)
        (fherr "fh:pointer: attempt to follow void pointer"))
      (let* ((size (bytestructure-descriptor-size descriptor))
             (index-datum (if syntax? (syntax->datum index) index)))
        (cond
         ((eq? '* index-datum)
          (if syntax?
              (values #`(bytevector-copy
                         (pointer-ref #,bytevector #,offset #,size))
                      0 descriptor)
              (values (bytevector-copy
                       (pointer-ref bytevector offset size))
                      0 descriptor)))
         ((integer? index-datum)
          (if syntax?
              (values #`(pointer-idx-ref #,bytevector #,offset ,index #,size)
                      0 descriptor)
              (values (pointer-idx-ref bytevector offset index-datum size)
                      0 descriptor)))
         (else
          (if syntax?
              (let ((bytevector* #`(pointer-ref #,bytevector #,offset #,size)))
                (bytestructure-unwrap/syntax
                 bytevector* 0 descriptor (syntax-list index index)))
              (let ((bytevector* (pointer-ref bytevector offset size)))
                (bytestructure-unwrap*
                 bytevector* 0 descriptor index))))))))
  (define (getter syntax? bytevector offset)
    (if syntax?
        #`(ffi:make-pointer (bytevector-address-ref #,bytevector #,offset))
        (ffi:make-pointer (bytevector-address-ref bytevector offset))))
  (define (setter syntax? bytevector offset value)
    (if syntax?
        #`(pointer-set! #,bytevector #,offset #,value)
        (pointer-set! bytevector offset value)))
  (define meta (make-pointer-metadata %descriptor))
  (make-bytestructure-descriptor size alignment unwrapper getter setter meta))
(export fh:pointer)

(define-record-type <function*-metadata>
  (make-function*-metadata wrapper unwrapper)
  function*-metadata?
  (wrapper function*-metadata-wrapper)
  (unwrapper function*-metadata-unwrapper))
(export function*-metadata?
        function*-metadata-wrapper
        function*-metadata-unwrapper)

(define (fh:function* wrapper unwrapper) ;; TBD documentation?
  (define size (ffi:sizeof '*))
  (define alignment size)
  (define (getter syntax? bytevector offset)
    (when syntax? (throw 'ffi-help-error "fh:function* has no macros"))
    (wrapper (ffi:make-pointer (bytevector-address-ref bytevector offset))))
  (define (setter syntax? bytevector offset value)
    (when syntax? (throw 'ffi-help-error "fh:function* has no macros"))
    (unwrapper value))
  (define meta (make-function*-metadata wrapper unwrapper))
  (make-bytestructure-descriptor size alignment #f getter setter meta))
(export fh:function*)


;; ---- hookups ----------------------------------------------------------------


(define-syntax-rule (fhval? val)
  (bytestructure? val))

;; @deffn {Syntax} fhval-ref obj arg ...
;; Get equivalent Guile object, if applicable, @code{#f} otherwise.
;; @end deffn
(define-syntax-rule (fhval-ref val arg ...)
  (bytestructure-ref val arg ...))

;; @deffn {Syntax} fhval-set! obj arg ...
;; Set the object value from a Scheme object.
;; If you are using @code{'*} you probably don't intend to:
;; look at @code{value-at}.
;; @end deffn
(define-syntax-rule (fhval-set! val arg ...)
  (bytestructure-set! val arg ...))

;; @deffn {Syntax} fhval-sel obj arg ...)
;; xxx
;; @end deffn
(define-syntax-rule (fhval-sel val arg ...)
  (call-with-values
      (lambda () (bytestructure-unwrap val arg ...))
    (lambda (bvec oset desc)
      (make-bytestructure bvec oset desc))))

;; @deffn {Syntax} fhval-addr val)
;; return the underlying address of the data
;; @end deffn
(define-syntax-rule (fhval-addr val)
  (call-with-values
      (lambda () (bytestructure-unwrap val))
    (lambda (bv offs desc)
      (+ (ffi:pointer-address (ffi:bytevector->pointer bv)) offs))))

;; @deffn {Syntax} fhval-pointer val
;; like fhval-addr but convert to a pointer
;; @end deffn
(define-syntax-rule (fhval-pointer val)
  (ffi:make-pointer (fhval-addr obj)))

;; @deffn {Syntax} fhval* obj
;; dereference a pointer
;; @end deffn
(define-syntax-rule (fhval* val)
  (fhval-sel val '*))

;; @deffn {Syntax} fhval& obj
;; dereference a pointer
;; @end deffn
(define-syntax-rule (fhval& obj)
  (error "fhval& not implemented"))

;; @deffn {Procedure} make-fhval desc [arg] [#:name name])
;; @end deffn
(define-syntax make-fhval
  (syntax-rules ()
    ((_ desc arg)
     (cond
      ((bytestructure? arg) arg)
      ((ffi:pointer? arg) (bytestructure desc (ffi:pointer-address arg)))
      (else (bytestructure desc arg))))
    ((_ desc) (bytestructure desc))))


;; (define (make-base-fhval name)

(define-syntax make-maker
  (syntax-rules ()
    ((_ desc make-base-obj)
     (define-public make-base-obj
       (let ()
         (case-lambda
           ((arg) (bytestructure desc arg))
           (() (bytestructure desc))))))))

(make-maker short make-short) (make-maker unsigned-short make-unsigned-short)
(make-maker int make-int) (make-maker unsigned-int make-unsigned-int)
(make-maker long make-long) (make-maker unsigned-long make-unsigned-long)
(make-maker intptr_t make-intptr_t) (make-maker uintptr_t make-uintptr_t)
(make-maker size_t make-size_t) (make-maker ssize_t make-ssize_t)
(make-maker ptrdiff_t make-ptrdiff_t)
(make-maker float make-float) (make-maker double make-double)
(make-maker int8 make-int8) (make-maker uint8 make-uint8)
(make-maker int16 make-int16) (make-maker uint16 make-uint16)
(make-maker int32 make-int32) (make-maker uint32 make-uint32)
(make-maker int64 make-int64) (make-maker uint64 make-uint64)


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

;; @deffn {Procedure} fh-object? obj
;; This predicate tests for FH objects, i.e., FFI defined types.
;; @example
;; (define-fh-pointer-type foo_t* foo_t*-desc)
;; (define val (make-foo_t*))
;; (fh-object? val) => #t
;; @end example
;; @end deffn
(define (fh-object? obj)
  (and
   (struct? obj)
   (struct-vtable? obj)
   (eq? (struct-vtable obj) ffi-helper-type)))

;; @deffn {Procedure} fh-object-val obj
;; Return the object value slot for the FH object.
;; @deffn
(define (fh-object-val obj)
  (unless (fh-object? obj) (fherr "fh-object-val: got ~s" obj))
  (struct-ref obj 0))

(define-syntax-rule (fh-object-ref obj arg ...)
  (begin
    (unless (fh-object? obj) (fherr "fh-object-ref: got ~s" obj))
    (fhval-ref (struct-ref obj 0) arg ...)))

(define-syntax-rule (fh-object-set! obj val arg ...)
  (begin
    (unless (fh-object? obj) (fherr "fh-object-set!: got ~s" obj))
    (fhval-set! (struct-ref obj 0) val arg ...)))

;; @deffn {Procedure} fh-type? type
;; This predicate tests for FH types.
;; @end deffn
(define (fh-type? type)
  (and (struct? type)
       (struct-vtable? type)
       (eq? (struct-vtable type) ffi-helper-type)))

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

;; execute the type method on the object
(define (fh-unwrap type obj)
  ((fht-unwrap type) obj))
(define (fh-wrap type val)
  ((fht-wrap type) val))

;; @deffn {Syntax} make-fh-type name unwrap wrap pointer-to value-at printer
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
              (or pointer-to (lambda (obj) (fhval& (fh-object-val obj))))
              (or value-at (lambda (obj) (fhval* (fh-object-val obj))))))
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
                   (lambda (obj) (fhval& (fh-object-val obj)))))
  (if make
      (struct-set! p-type (+ vtable-offset-user 3) ; value-at
                   (lambda (obj) (fhval* (fh-object-val obj))))))

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

;; === objects ============

;; @deffn {Procedure} fh-object-type obj
;; return the object type
;; @end deffn
(define (fh-object-type obj)
  (or (fh-object? obj) (fherr "fh-object-type: expecting fh-object arg"))
  (struct-vtable obj))

(define (make-printer type)
  (lambda (obj port)
    (display "#<" port)
    (display type port)
    (display " 0x" port)
    (display (number->string (fhval-addr (fh-object-val obj)) 16) port)
    (display ">" port)))

;; show what it points to
(define (make-pointer-printer type)
  (lambda (obj port)
    (display "#<" port)
    (display type port)
    (display " 0x" port)
    (display (number->string (fhval-addr (fh-object-val obj)) 16) port)
    (display ">" port)))

;; @deffn {Syntax} define-fh-type-alias alias type
;; set up type alias.  Caller needs to match type? and make.
;; @end deffn
(define-syntax define-fh-type-alias
  (lambda (x)
    (syntax-case x ()
      ((_ alias type)
       (with-syntax ((desc (gen-id x #'alias "-desc"))
                     (pred (gen-id x #'alias "?"))
                     (make (gen-id x "make-" #'alias)))
         #`(begin
             (define alias
               (make-fht (quote alias)
                         (fht-unwrap type)
                         make
                         #f #f
                         (make-printer (quote alias))))
             (define pred (and (fh-object? obj) (eq? (struct-vtable obj) alias)))
             (define make
               (case-lambda
                 ((arg) (make-struct/no-tail alias (make-fhval desc arg)))
                 (() (make-struct/no-tail alias (make-fhval desc)))))
             (export pred make)))))))

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
                unwrap~pointer
                (lambda (val) (make val))
                #f #f
                (make-pointer-printer (quote type))))
    (define (type? obj)
      (and (fh-object? obj) (eq? (struct-vtable obj) type)))
    (define make
      (case-lambda
        ((val)
         (cond
          ((number? val)
           (make-struct/no-tail type (make-fhval desc val)))
          ((ffi:pointer? val)
           (make-struct/no-tail type (make-fhval desc val)))
          (else (make-struct/no-tail type val))))
        (() (make 0))))))

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
        ((arg) (make-struct/no-tail type (make-fhval desc arg)))
        (() (make-struct/no-tail type (make-fhval desc)))))))

(define-syntax-rule (define-fh-vector-type type elt-desc type? make)
  (begin
    (define type
      (make-fht (quote type)
                (lambda (obj) (fhval-ref (fh-object-val obj)))
                (lambda (obj) (make obj))
                #f #f
                (make-printer (quote type))))
    (define (type? obj)
      (and (fh-object? obj) (eq? (struct-vtable obj) type)))
    (define make
      (case-lambda
        ((arg) (make-struct/no-tail type (make-fhval desc arg)))
        (() (make-struct/no-tail type (make-fhval desc)))))))

;; @deffn {Syntax} define-fh-function*-type type desc type? make
;; document this
;; @end deffn
(define-syntax define-fh-function*-type
  (syntax-rules ()
    ((_ type desc type? make)
     (begin
       (define type
         (make-fht (quote type)
                (lambda (obj) (fhval-ref (fh-object-val obj)))
                (lambda (obj) (make obj))
                #f #f
                (make-printer (quote type))))
       (define (type? obj)
         (and (fh-object? obj) (eq? (struct-vtable obj) type)))
       (define make
         (case-lambda
           ((arg) (make-struct/no-tail type (make-fhval desc arg)))
           (() (make-struct/no-tail type (make-fhval desc)))))))))

;; @deffn {Syntax} fh-cast type value
;; Cast to new type.  Always a pointer, unless I missed something.
;; Example: Given @code{bar} of type @code{Bar*}:
;; @example
;; (fh-cast Foo* bar) => <Foo* 0xabcd1234>
;; @end example
;; @end deffn
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

;; --- unwrap / wrap procedures

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
   (else (fherr "unwrap~number: bad arg: ~s" arg))))

(define (unwrap~pointer arg)
  (cond
   ((ffi:pointer? arg) arg)
   ((string? arg) (ffi:string->pointer arg))
   ((fh-object? arg) (fh-object-ref arg))
   ((fhval? arg) (fhval-ref arg))
   (else (fherr "unwrap~pointer: bad arg: ~s" arg))))

(define (unwrap~array arg)
  (cond
   ((array? arg) (ffi:scm->pointer arg))
   ((fh-object? arg) (fh-object-ref (pointer-to arg)))
   ((fhval? arg) (fhval-ref (fhval& arg)))
   (else (fherr "unwrap~array: bad arg: ~s" arg))))

(define (unwrap~function* obj)
  (cond
   (else (fherr "unwrap~function*: bad arg: ~s" obj))))


;; --- types ---------------------------

(define char*-desc (bs:pointer 'void))
(define char**-desc (bs:pointer char*-desc))
(define void-desc 'void)
(define void*-desc (bs:pointer 'void))
(define void**-desc (bs:pointer (bs:pointer 'void)))

;; All other FFI types are variables which as bound to constant expressions.
;; Here we bind '* to a variable to avoid special cases in the code generator.

(define ffi-void* '*)

(define char*
  (make-fht 'char*
            (lambda (obj) (ffi:pointer->string (fhval-ref (fh-object-val obj))))
            (lambda (val) (make-char* val))
            #f #f
            (make-printer 'char*)))
(define make-char*
  (case-lambda
    ((arg)
     (cond
      ((string? arg) (make-fhval char*-desc (ffi:string->pointer arg)))
      ((ffi:pointer? arg) (make-fhval char*-desc arg))
      (else (fherr "make-char*: bad arg: ~s" arg))))
    (() (make-fhval char*-desc))))
(define char*? (lambda (obj) (eq? (struct-vtable obj) char*)))
(export char*? make-char*)

(define-fh-pointer-type char** char**-desc char**? make-char**)
(export char**? make-char**)

(fh-ref<=>deref! char** make-char** char* make-char*)

(define-public (char*->string obj)
  (ffi:pointer->string (fh-object-ref obj)))

(define fh-void
  (make-fht 'void
            (lambda (obj) void-desc)
            (lambda (val) (fherr "fh-void: attempt to make void"))
            #f #f
            (lambda (obj port) (display "#<fh-void>" port))))
(define fh-void?
  (lambda (obj) (and (struct? obj) (eq? (struct-vtable obj) fh-void))))
(define make-fh-void
  (case-lambda
    (() (make-struct/no-tail fh-void 'void))
    ((arg) (make-struct/no-tail fh-void arg))))
;;(export fh-void? make-fh-void)

(define void*
  (make-fht 'void*
            unwrap~pointer
            (case-lambda
              ((val)
               (cond
                ((string? val)
                 (make-struct/no-tail
                  void* (bytestructure
                         void*-desc (ffi:pointer-address
                                     (ffi:string->pointer val)))))
                ((bytestructure? val)
                 (make-struct/no-tail void* val))
                (else
                 (make-struct/no-tail void* (bytestructure void*-desc val)))))
              (() (make-struct/no-tail void* (bytestructure void*-desc))))
            #f #f
            (lambda (obj port)
              (display "#<void* 0x" port)
              (display (number->string (struct-ref obj 0) 16) port)
              (display ">" port))))
(define make-void* (fht-wrap void*))
(define void*?
  (lambda (obj) (and (struct? obj) (eq? (struct-vtable obj) void*))))
(fh-ref<=>deref! void* make-void* fh-void make-fh-void)
(export void*? make-void*)

(define void**
  (make-fht 'void**
            unwrap~pointer
            (case-lambda
              ((val)
               (make-struct/no-tail void** (bytestructure void**-desc val)))
              (()
               (make-struct/no-tail void** (bytestructure void**-desc))))
            #f #f
            (lambda (obj port)
              (display "#<void** 0x" port)
              (display (number->string (struct-ref obj 0) 16) port)
              (display ">" port))))
(define make-void** (fht-wrap void**))
(define void**?
  (lambda (obj) (and (struct? obj) (eq? (struct-vtable obj) void**))))
(fh-ref<=>deref! void** make-void** void* make-void*)
(export void**? make-void**)


(define-syntax define-base-pointer-type
  (lambda (x)
    (syntax-case x ()
      ((_ desc)
       (with-syntax ((desc* (gen-id #'desc #'desc "*-desc"))
                     (type* (gen-id #'desc #'desc "*"))
                     (type*? (gen-id #'desc #'desc "*?"))
                     (make* (gen-id #'desc "make-" #'desc "*")))
         #'(begin
             (define desc* (bs:pointer desc))
             (define-fh-pointer-type type* desc* type*? make*)
             (export type* desc* type*? make-type*)))))))

(define-base-pointer-type short) (define-base-pointer-type unsigned-short)
(define-base-pointer-type int) (define-base-pointer-type unsigned-int)
(define-base-pointer-type long) (define-base-pointer-type unsigned-long)
(define-base-pointer-type float) (define-base-pointer-type double)

;; --- other items --------------------

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


(define (fh-find-symbol-addr name dl-lib-list)
  (let loop ((dll (cons (dynamic-link) dl-lib-list)))
    (cond
     ((null? dll) (throw 'ffi-help-error "function not found"))
     ((catch #t
        (lambda () (dynamic-func name (car dll)))
        (lambda args #f)))
     (else (loop (cdr dll))))))

;; @deffn {Procedure} fh-link-proc return name args dy-lib-list
;; Generate Guile procedure from C library.
;; @end deffn
(define* (fh-link-proc return name args dl-lib-list)
  ;; Given a list of links (output of @code{(dynamic-link @it{library})}
  ;; try to get the dynamic-func for the provided function.  Usually
  ;; the first dynamic link is @code{(dynamic-link)} and that should work.
  ;; But on some systems we need to find the actual library :(, apparently.
  (let ((dfunc (fh-find-symbol-addr name dl-lib-list)))
    (and dfunc (ffi:pointer->procedure return dfunc args))))

;; @deffn {Procedure} fh-link-extern name desc db-lib-list => bs
;; Generate a bytestructure from the bytes in the library at the var addr.
;; @end deffn
(define* (fh-link-extern name desc dl-lib-list)
  (let* ((addr (fh-find-symbol-addr name dl-lib-list))
         (size (bytestructure-descriptor-size desc)))
    (make-bytestructure (ffi:pointer->bytevector addr size) 0 desc)))


#|
;; @deffn {Procedure} make-argv str-list => bv
;; For C functions that take an argument of the form @code{const char *names[]},
;; this routine will convert a scheme list of strings into an appropriate
;; bytevector which can be passed via @code{unwrap~pointer}.
;; @end deffn
(define-public (make-argv str-list)
  "- Procedure: make-argv str-list => bv
     For C functions that take an argument of the form 'const char
     *names[]', this routine will convert a scheme list of strings into
     an appropriate bytevector which can be passed via 'unwrap~pointer'."
  (let* ((n (length string-list))
         (addresses (map (compose pointer-address
                                  string->pointer)
                         string-list))
             (bv (make-bytevector (* n (sizeof '*))))
             (bv-set! (case (sizeof '*)
                            ((4) bytevector-u32-native-set!)
                            ((8) bytevector-u64-native-set!))))
    (for-each (lambda (address index)
                (bv-set! bv (* (sizeof '*) index) address))
              addresses (iota n))
    bv))
|#

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
