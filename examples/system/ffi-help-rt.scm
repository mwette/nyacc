;;; example/system/ffi-help-rt.scm - NYACC's FFI help runtime
;;;
;;; Copyright (C) 2016-2017 Matthew R. Wette
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, see <http://www.gnu.org/licenses/>

;; runtime for generated ffi-compiled dot-ffi files

(define-module (system ffi-help-rt)
  #:export (*ffi-help-version*
	    fh-type?
	    fh-object? fh-object-val
	    fh-object-ref fh-object-set!
	    pointer-to value-at

	    fh:function fh:cast fh-cast bs-addr
	    define-fh-pointer-type
	    define-fh-type-alias
	    define-fh-compound-type
	    define-fh-function*-type
	    ref<->deref!

	    fht-wrap fht-unwrap fh-wrap fh-unwrap
	    unwrap~fixed unwrap~float
	    unwrap~pointer unwrap~array
	    make-fctn-param-unwrapper
	    fh-link-proc fh-link-bstr ffi-void*)
  #:use-module (bytestructures guile)
  #:use-module (bytestructures guile ffi)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (srfi srfi-9))

(define *ffi-help-version* "0.83.2")

(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))

;; The FFI helper uses a base type based on Guile structs and vtables.
;; The base vtable uses these (lambda (obj) ...) fields:
;; 0 unwrap	: convert helper-type object to ffi argument
;; 1 wrap	: convert ffi object to helper-type object
;; 2 pointer-to	: (pointer-to <foo_t-obj>) => <foo_t*-obj>
;; 3 value-at	: (value-at <foo_t*-obj>) => <foo_t-obj>
;; The C-based (child) types will add a slot for the object value.
(define ffi-helper-type
  (make-vtable
   (string-append standard-vtable-fields "prprpwpw")
   (lambda (v p)
     (display "#<ffi-helper-type>" p))))
(define unwrap-ix 0)
(define wrap-ix 1)
(define pointer-to-ix 2)
(define value-at-ix 3)

;; @deffn {Syntax} make-fh-type name unwrap pointer-to value-at printer
;; We call make-struct here but we are actually making a vtable
;; We should check with struct-vtable?
;; name as symbol
(define* (make-fht name unwrap wrap pointer-to value-at printer)
  ;;(simple-format #t "make-fht: ~S\n" name)
  (let* ((ty (make-struct/no-tail
	      ffi-helper-type
	      (make-struct-layout "pw") ;; 1 slot for value
	      printer
	      (or unwrap (lambda (obj) (error "no unwrapper")))
	      (or wrap (lambda (obj) (error "no wrapper")))
	      ;;(or pointer-to (lambda (obj) (error "no pointer-to")))
	      (or pointer-to (lambda (obj) (ffi:bytevector->pointer
					    (bytestructure-bytevector
					     (fh-object-val obj)))))
	      ;;(or value-at (lambda (obj) (error "no value-at")))
	      (or value-at (lambda (obj) (bytestructure-ref
					  (fh-object-val obj) '*)))
	      ))
	 (vt (struct-vtable ty)))
    (set-struct-vtable-name! vt name)
    ty))

;; @deffn {Procedure} fh-type? type
;; This predicate tests for FH types.
;; @end deffn
(define (fh-type? type)
  (and (struct? type)
       (struct-vtable? type)
       (eq? (struct-vtable type) ffi-helper-type)))

;; return methods from the type
;; do not export, but check
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
  ((fht-unwrap type)) obj)
(define (fh-wrap type val)
  ((fht-wrap type)) val)

;; Right now this returns a ffi pointer.
;; TODO: add field option so we can do (pointer-to xstr 'vec)
(define (pointer-to obj)
  (cond
   ((fh-object? obj)
    ((fht-pointer-to (struct-vtable obj)) obj))
   ((bytestructure? obj)
    (ffi:bytevector->pointer (bytestructure-bytevector obj)))
   ((bytevector? obj)
    (ffi:bytevector->pointer obj))
   (else
    (error "expecting something I can point to"))))

(define (value-at obj)
  (cond
   ((fh-object? obj)
    ((fht-value-at (struct-vtable obj)) obj))
   ((bytestructure? obj)
    (bytestructure-ref obj '*))
   (else
    (throw 'ffi-help-error "expecting something I can dereference"))))

;; === objects ============

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
   (fh-type? (struct-vtable obj))))

;; @deffn {Procedure} fh-object-type obj
;; return the object type
;; @end deffn
(define (fh-object-type obj)
  (or (fh-object? obj) (error "expecting ffi-help type"))
  (struct-vtable obj))

;; @deffn {Procedure} fh-object-val obj
;; Return the bytestructure object associate with the FH object.
;; If @var{object} is a bytestructure, return that.
;; @deffn
(define (fh-object-val obj)
  (cond
   ((fh-object? obj) (struct-ref obj 0))
   ((bytestructure? obj) obj)
   (else (error "expecting ffi-help object or bytestructure"))))

(define-syntax-rule (fh-object-ref obj arg ...)
  (bytestructure-ref (fh-object-val obj) arg ...))

(define-syntax-rule (fh-object-set! obj arg ...)
  (bytestructure-set! (fh-object-val obj) arg ...))

(eval-when (expand load eval)
  (define (gen-id tmpl-id . args)
    (define (stx->str stx)
      (symbol->string (syntax->datum stx)))
    (datum->syntax
     tmpl-id
     (string->symbol
      (apply string-append
             (map (lambda (ss) (if (string? ss) ss (stx->str ss))) args))))))


;; --- typedefs

;; @deffn {Procedure} bs-addr bst
;; Return the raw, numerical address of the bytestruture bytevector data.
;; @end deffn
(define (bs-addr bst)
  (ffi:pointer-address
   (ffi:bytevector->pointer
    (bytestructure-bytevector bst))))

;; type printer for bytestructures-based types
(define (make-bs-printer type)
  (lambda (obj port)
    (display "#<" port)
    (display type port)
    (when #f
      (display " bs-desc:0x" port)
      (display (number->string (ffi:scm->pointer (struct-ref obj 0)) 16) port))
    (when #t
      (display " 0x" port)
      (display (number->string (bs-addr (struct-ref obj 0)) 16) port))
    (display ">" port)))

(define (make-bs*-printer type)
  (lambda (obj port)
    (display "#<" port)
    (display type port)
    (display " 0x" port)
    (display (number->string (bytestructure-ref (struct-ref obj 0)) 16) port)
    (display ">" port)))

(define (make-printer type)
  (lambda (obj port)
    (display "#<" port)
    (display type port)
    (display " 0x" port)
    (display (number->string
	      (ffi:pointer-address (ffi:scm->pointer (struct-ref obj 0)))
	      16) port)
    (display ">" port)))

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
		(lambda (val)
		  (make (bytestructure desc (ffi:pointer-address val))))
		#f #f
		(make-bs*-printer (quote type))))
    (define (type? obj)
      (and (fh-object? obj) (eq? (struct-vtable obj) type)))
    (define make
      (case-lambda
	((val)
	 (cond
	  ((bytestructure? val)
	   (make-struct/no-tail type val))
	  ((bytevector? val)
	   (make-struct/no-tail type (bytestructure desc val)))
	  ((number? val)
	   (make-struct/no-tail type (bytestructure desc val)))
	  ((ffi:pointer? val)
	   (make-struct/no-tail type (bytestructure desc
						    (ffi:pointer-address val))))
	  (else (make-struct/no-tail type val))))
	(() (make 0))))))

;; @deffn {Syntax} ref<->deref! p-type p-make type make
;; This procedure will ``connect'' the two types so that the procedures
;; @code{pointer-to} and @code{value-at} work.
;; @end deffn
(define (ref<->deref! p-type p-make type make)
  (struct-set! type (+ vtable-offset-user 2) ; pointer-to
	       (lambda (obj) (p-make (bs-addr (fh-object-val obj)))))
  (struct-set! p-type (+ vtable-offset-user 3) ; value-at
	       (lambda (obj) (make (fh-object-ref obj '*)))))

;; @deffn {Syntax} define-fh-type-alias alias type
;; set up type alias.  Caller needs to match type? and make
;; @end deffn
(define-syntax-rule (define-fh-type-alias alias type)
  (define alias
    (make-fht (quote alias)
	      (fht-wrap type)
	      (fht-unwrap type)
	      (fht-pointer-to type)
	      (fht-value-at type)
	      (fht-printer type))))

;; @deffn {Syntax} define-fh-compound-type type desc type? make
;; Generates an FY aggregate type based on a bytestructure descriptor.
;; @end deffn
(define-syntax-rule (define-fh-compound-type type desc type? make)
  (begin
    (define type
      (make-fht (quote type)
		(lambda (obj)
		  (bytestructure-bytevector (struct-ref obj 0)))
		(lambda (val)
		  (make-struct/no-tail type (bytestructure desc val)))
		#f #f
		(make-bs-printer (quote type))))
    (define (type? obj)
      (and (fh-object? obj) (eq? (struct-vtable obj) type)))
    (define make
      (case-lambda
	((arg) (if (bytestructure? arg)
		   (make-struct/no-tail type arg)
		   (make-struct/no-tail type (bytestructure desc arg))))
	(args (make-struct/no-tail type (apply bytestructure desc args)))))))

;; == extension to bytestructures ==============================================

;; @deffn {Procedure} fh:function return-desc param-desc-list
;; @deffnx {Syntax} define-fh-function*-type name desc type? make
;; Generate a descriptor for a function pseudo-type, and then the associated
;; function pointer type. 
;; @example
;; (define foo_t*-desc (bs:pointer (delay ffi:double (list ffi:double))))
;; (define-fh-function-type foo_t* foo_t*-desc foo_t? make-foo_t)
;; @end example
;; @end deffn
(define-record-type <function-metadata>
  (make-function-metadata return-descriptor param-descriptor-list attributes)
  function-metadata?
  (return-descriptor function-metadata-return-descriptor)
  (param-descriptor-list function-metadata-param-descriptor-list)
  (attributes function-metadata-attributes))

(define (pointer->procedure/varargs return-ffi pointer param-ffi-list)
  (define (arg->ffi arg)
    (cond
     ((bytestructure? arg)
      (bytestructure-descriptor->ffi-descriptor
       (bytestructure-descriptor arg)))
     ((and (pair? arg) (bytestructure-descriptor? (car arg)))
      (bytestructure-descriptor->ffi-descriptor (car arg)))
     ((pair? arg) (car arg))
     (else (error "can't interpret argument"))))
  (define (arg->val arg)
    (cond
     ((bytestructure? arg) (bytestructure-ref arg))
     ((and (pair? arg) (bytestructure? (cdr arg)))
      (bytestructure-ref (cdr arg)))
     ((pair? arg) (cdr arg))
     (else arg)))
  (define (arg-list->ffi-list param-list arg-list)
    (let iter ((param-l param-list) (argl arg-list))
      (cond
       ((pair? param-l) (cons (car param-l) (iter (cdr param-l) (cdr argl))))
       ((pair? argl) (cons (arg->ffi (car argl)) (iter param-l (cdr argl))))
       (else '()))))
  (lambda args
    (let ((ffi-l (arg-list->ffi-list param-ffi-list args))
	  (arg-l (map arg->val args)))
      (sferr "return=~S  params=~S\n" return-ffi ffi-l)
      (apply (ffi:pointer->procedure return-ffi pointer ffi-l) arg-l))))

;; right now the code generator only uses ffi types
(define (old-fh:function %return-desc %param-desc-list)
  #;(define (get-return-ffi syntax?)
  (if syntax?
  #`(bytestructure-descriptor->ffi-descriptor %return-desc)
  (bytestructure-descriptor->ffi-descriptor %return-desc)))
  #;(define (get-param-ffi-list syntax?)
  (let iter ((params %param-desc-list))
  (cond
  ((null? params)
  '())
  ((pair? (car params))		; (list name desc)
  (cons (cadar params) (iter (cdr params))))
  ((eq? '... (car params))		; '...
  '())
  ((bytestructure-descriptor? (car params)) ; desc
  (cons (bytestructure-descriptor->ffi-descriptor (car params))
  (iter (cdr params))))
  (else (error "bad parameter")))))
  (define (get-return-ffi syntax?)
    (if syntax?
	#`%return-desc
	%return-desc))
  (define (get-param-ffi-list syntax?)
    (let iter ((params %param-desc-list))
      (cond
       ((null? params) '())
       ((pair? (car params)) (cons (cadar params) (iter (cdr params))))
       ((eq? '... (car params)) '())
       (else (cons (car params) (iter (cdr params)))))))
  (define size (ffi:sizeof '*))
  (define alignment size)
  (define attributes
    (let iter ((param-l %param-desc-list))
      (cond ((null? param-l) '())
	    ((eq? '... (car param-l)) '(varargs))
	    (else (iter (cdr param-l))))))
  (define (getter syntax? bytevector offset) ; assumes zero offset!
    (if (memq 'varargs attributes)
	(if syntax?
	    #`(pointer->procedure/varargs
	       (get-return-ffi #f)
	       (ffi:bytevector->pointer bytevector)
	       (get-param-ffi-list #f))
	    (pointer->procedure/varargs
	     (get-return-ffi #f)
	     (ffi:bytevector->pointer bytevector)
	     (get-param-ffi-list #f)))
	(if syntax?
	    #`(ffi:pointer->procedure
	       #,(get-return-ffi #t)
	       (ffi:bytevector->pointer #,bytevector)
	       #,(get-param-ffi-list #t))
	    (ffi:pointer->procedure
	     (get-return-ffi #f)
	     (ffi:bytevector->pointer bytevector)
	     (get-param-ffi-list #f)))))
  (define meta
    (make-function-metadata %return-desc %param-desc-list attributes))
  (make-bytestructure-descriptor size alignment #f getter #f meta))

;; ==============

;; @deffn {Procedure} fh:function return-desc param-desc-list
;; @deffnx {Syntax} define-fh-function*-type name desc type? make
;; Generate a descriptor for a function pseudo-type, and then the associated
;; function pointer type. 
;; @example
;; (define foo_t*-desc (bs:pointer (delay (fh:function double (list double)))))
;; (define-fh-function-type foo_t* foo_t*-desc foo_t? make-foo_t)
;; @end example
;; @end deffn
(define (fh:function %return-desc %param-desc-list)
  (define (get-return-ffi syntax?)
    (if syntax?
	#`%return-desc
	%return-desc))
  (define (get-param-ffi-list syntax?)
    (let iter ((params %param-desc-list))
      (cond
       ((null? params) '())
       ((pair? (car params)) (cons (cadar params) (iter (cdr params))))
       ((eq? '... (car params)) '())
       (else (cons (car params) (iter (cdr params)))))))
  (define size (ffi:sizeof '*))
  (define alignment size)
  (define attributes
    (let iter ((param-l %param-desc-list))
      (cond ((null? param-l) '())
	    ((eq? '... (car param-l)) '(varargs))
	    (else (iter (cdr param-l))))))
  (define (getter syntax? bytevector offset) ; assumes zero offset!
    (if (memq 'varargs attributes)
	(if syntax?
	    #`(pointer->procedure/varargs
	       (get-return-ffi #f)
	       (ffi:bytevector->pointer bytevector)
	       (get-param-ffi-list #f))
	    (pointer->procedure/varargs
	     (get-return-ffi #f)
	     (ffi:bytevector->pointer bytevector)
	     (get-param-ffi-list #f)))
	(if syntax?
	    #`(ffi:pointer->procedure
	       #,(get-return-ffi #t)
	       (ffi:bytevector->pointer #,bytevector)
	       #,(get-param-ffi-list #t))
	    (ffi:pointer->procedure
	     (get-return-ffi #f)
	     (ffi:bytevector->pointer bytevector)
	     (get-param-ffi-list #f)))))
  (define meta
    (make-function-metadata %return-desc %param-desc-list attributes))
  (make-bytestructure-descriptor size alignment #f getter #f meta))

;; =============================================================================

(define (bs-desc->ffi-desc bs-desc)
  (cond
   ((bytestructure-descriptor? bs-desc)
    (bytestructure-descriptor->ffi-descriptor bs-desc))
   ((eq? bs-desc 'void) ffi:void)
   (else (error "missed type"))))

;; given a fs:function return pair: (return-type . arg-list)
(define (fs-function*-signature desc)
  (let* ((meta (bytestructure-descriptor-metadata desc))
	 (desc (pointer-metadata-content-descriptor meta))
	 (desc (if (promise? desc) (force desc) desc))
	 (meta (bytestructure-descriptor-metadata desc))
	 (bs-rt (function-metadata-return-descriptor meta))
	 (ffi-rt (bs-desc->ffi-desc bs-rt))
	 (bs-al (function-metadata-param-descriptor-list meta))
	 (ffi-bs-al (map bs-desc->ffi-desc bs-al)))
    (cons ffi-rt ffi-bs-al)))

;; @deffn {Syntax} define-fh-function*-type type desc type? make
;; document this
;; @end deffn
(define-syntax define-fh-function*-type
  (syntax-rules ()
    ((_ type desc type? make)
     (begin
       (define type
	 (make-fht
	  (quote type)
	  ;; unwrap:
	  (lambda (obj)
	    (cond
	     ((procedure? obj)		; a lambda
	      (let* ((sig (fs-function*-signature desc)))
		(ffi:procedure->pointer (car sig) obj (cdr sig))))
	     #;((and (pair? obj)
	     (or (fh-type? (car obj))
	     (bytestructure-descriptor? (car obj))))
	     #f)
	     (else (unwrap~pointer obj))))
	  ;; wrap:
	  (lambda (val) (make (bytestructure desc (ffi:pointer-address val))))
	  ;; pointer-to:
	  #f
	  ;; value-at:
	  (lambda (obj)	(fh-object-ref obj '*))
	  (make-bs*-printer (quote type))))
       (define (type? obj)
	 (and (fh-object? obj) (eq? (struct-vtable obj) type)))
       (define make
	 (case-lambda
	   ((val)
	    (cond
	     ((number? val) (bytestructure desc val))
	     ((ffi:pointer? val) (bytestructure desc (ffi:pointer-address val)))
	     ((procedure? val) ;; special case, proceadure not pointer
	      (let* ((sig (fs-function*-signature desc)))
		(bytestructure
		 desc
		 (ffi:pointer-address
		  (ffi:procedure->pointer (car sig) val (cdr sig))))))
	     (else (error "bad argument type"))))
	   (() (make-struct/no-tail type (bytestructure desc)))))
       (export type type? make)))))

;; @deffn {Procedure} fh:cast type value
;; @deffnx {Procedure} fh-cast type value
;; @example
;; (fh-cast foo_desc_t* 321)
;; (use-modules ((system foreign) #:prefix 'ffi:))
;; (fh-cast ffi:short 321)
;; We might have a procedure that wants be passed as a pointer but
;; @end deffn
;; use cases
;; @itemize
;; @item
;; @example
;; (lambda (x y) #f) => (procedure->pointer void (list '* '*))
;; @end example
;; @end itemize
(define (fh:cast type expr)
  (let* ((r-type			; resolved type
	  (cond
	   ((bytestructure-descriptor? type)
	    (bytestructure-descriptor->ffi-descriptor type))
	   (else type)))
	 (r-expr			; resolved value
	  (cond
	   ((and (equal? r-type ffi-void*) (string? expr))
	    (ffi:string->pointer expr))
	   (else expr))))
    (cons r-type r-expr)))
(define fh-cast fh:cast)

;; --- unwrap / wrap procedures

;; now support for the base types
(define (unwrap~fixed obj)
  (cond
   ((number? obj) obj)
   ((bytestructure? obj) (bytestructure-ref obj))
   ((fh-object? obj) (struct-ref obj 0))
   (else (error "type mismatch"))))

(define unwrap~float unwrap~fixed)

;; unwrap-enum has to be inside module

;; FFI wants to see a ffi:pointer type
(define (unwrap~pointer obj)
  (cond
   ((ffi:pointer? obj) obj)
   ((string? obj) (ffi:string->pointer obj))
   ((bytestructure? obj) (ffi:make-pointer (bytestructure-ref obj)))
   ((fh-object? obj) (unwrap~pointer (struct-ref obj 0)))
   ;; TODO: work out casting pointer types
   ;;((and (pair? obj)
   ;;(or (fh-type? (car obj)) (bytestructure-descriptor? (car obj))))
   (else (error "expecting pointer type"))))

;; @deffn {Procedure} make-fctn-param-unwrapper ret-t args-t => lambda
;; This procedure will convert an argument, 
;; @end deffn
(define (make-fctn-param-unwrapper ret-t args-t)
  (lambda (obj)
    (cond
     ((ffi:pointer? obj) obj)
     ((procedure? obj) (ffi:procedure->pointer ret-t obj args-t))
     (else (error "expecting pointer or procedure")))))

;; --- types ---------------------------

;; All other FFI types are variables which as bound to constant expressions.
;; Here we bind '* to a variable to avoid special cases in the code generator.
(define ffi-void* '*)

(define char*-desc (bs:pointer 'void))
(define-fh-pointer-type char* char*-desc char*? make-char*)
(define char**-desc (bs:pointer char*-desc))
(define-fh-pointer-type char** char**-desc char**? make-char**)
(ref<->deref! char** make-char** char* make-char*)
(define (char*->string obj)
  (ffi:pointer->string (ffi:make-pointer (fh-object-ref obj))))
(export make-char* char*->string)

(define-syntax make-maker
  (syntax-rules ()
    ((_ desc make-bs-obj)
     (define-public make-bs-obj
       (case-lambda
	 ((arg) (bytestructure desc arg))
	 (() (bytestructure desc)))))))

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

(define (find-addr name dl-lib-list)
  (let iter ((dll (cons (dynamic-link) dl-lib-list)))
    (cond
     ((null? dll) (throw 'ffi-help-error "function not found"))
     ((catch #t
	(lambda () (dynamic-func name (car dll)))
	(lambda args #f)))
     (else (iter (cdr dll))))))

;; @deffn {Procedure} fh-link-proc return name args dy-lib-list
;; Generate Guile procedure from C library. 
;; @end deffn
(define* (fh-link-proc return name args dl-lib-list)
  ;; Given a list of links (output of @code{(dynamic-link @it{library})}
  ;; try to get the dynamic-func for the provided function.  Usually
  ;; the first dynamic link is @code{(dynamic-link)} and that should work.
  ;; But on some systems we need to find the actual library :(, apparently.
  (let ((dfunc (find-addr name dl-lib-list)))
    (and dfunc (ffi:pointer->procedure return dfunc args))))

;; @deffn {Procedure} fh-link-bstr name desc db-lib-list => bytestructure
;; Generate a bytestructure from the bytes in the library at the var addr.
;; @end deffn
(define* (fh-link-bstr name desc dl-lib-list)
  (let* ((addr (find-addr name dl-lib-list))
	 (size (bytestructure-descriptor-size desc)))
    (make-bytestructure (ffi:pointer->bytevector addr size) 0 desc)))

;; --- last line ---
