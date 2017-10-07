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
  #:export (fh-type?
	    fh-object?
	    fh-object-val
	    pointer-to value-at
	    fh-cast
	    ffi-void*

	    define-fh-pointer-type
	    define-fh-compound-type
	    define-fh-function*-type
	    ref<->deref!
	    
	    fh-link-proc
	    fht-wrap fht-unwrap
	    unwrap~fixed unwrap~float
	    unwrap~pointer unwrap~array
	    make-fctn-param-unwrapper

	    fh:function fh:cast
	    fh-bsref fh-bsset!
	    bs-addr
	    )
  #:use-module (bytestructures guile)
  #:use-module (bytestructures guile ffi)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (srfi srfi-9)
  #:version (0 82 2)
  )

;; The FFI helper uses a base type based on Guile structs and vtables.
;; The base vtable uses these (lambda (obj) ...) fields:
;; 0 unwrap	: convert helper-type object to ffi argument
;; 1 unwrap	: convert helper-type object to ffi argument
;; 2 pointer-to	: (pointer-to <foo_t-obj>) => <foo_t*-obj>
;; 3 value-at	: (value-at <foo_t*-obj>) => <foo_t-obj>
;; The C-based types will add a slot for the object value.
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
	      (or pointer-to (lambda (obj) (error "no pointer-to")))
	      (or value-at (lambda (obj) (error "no value-at")))
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
(define (fht-unwrap obj)
  (struct-ref obj (+ vtable-offset-user unwrap-ix)))
(define (fht-wrap obj)
  (struct-ref obj (+ vtable-offset-user wrap-ix)))
(define (fht-pointer-to obj)
  (struct-ref obj (+ vtable-offset-user pointer-to-ix)))
(define (fht-value-at obj)
  (struct-ref obj (+ vtable-offset-user value-at-ix)))

;; execute the type method on the object
(define (fh-unwrap type obj)
  ((fht-unwrap type)) obj)
(define (fh-wrap type val)
  ((fht-wrap type)) val)

;; Right now this returns a ffi pointer.
(define (pointer-to obj)
  (or (fh-object? obj) (error "expecting ffi-help object"))
  ((fht-pointer-to (struct-vtable obj)) obj))

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
;; Return the value associate with the object.  This is most often
;; (I think) a bytestructure.
;; @deffn
(define (fh-object-val obj)
  (or (fh-object? obj) (error "expecting ffi-help object"))
  (struct-ref obj 0))

(define-syntax-rule (fh-object-ref obj arg ...)
  (bytestructure-ref (fh-object-val obj) arg ...))

(define-syntax-rule (fh-object-set! obj arg ...)
  (bytestructure-set! (fh-object-val obj) arg ...))

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

;; @deffn {Syntax} define-fh-pointer-type name
;; @example
;; (define foo_t*-desc (bs:pointer foo_t-desc))
;; (define-fh-pointer-type foo_t*
;; @end example
;; The second form is based on already defined @code{bs:pointer} descriptor.
;; @end deffn
(define-syntax define-fh-pointer-type
  (syntax-rules ()
    ((_ type desc type? make)
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
       (define (make val)
	 (cond
	  ((bytestructure? val)
	   (make-struct/no-tail type val))
	  ((bytevector? val)
	   (make-struct/no-tail type (bytestructure desc val)))
	  ((number? val)
	   (make-struct/no-tail type (bytestructure desc val)))
	  (else (make-struct/no-tail type val))))
       (export type type? make)))))

;; @deffn {Procedure} ref<->deref! p-type p-make type make
;; This procedure will ``connect'' the two types so that the procedures
;; @code{pointer-to} and @code{value-at} work.
;; @end deffn
(define-syntax ref<->deref!
  (syntax-rules ()
    ((_ p-type p-make type make)
     (begin
       (struct-set!		; pointer-to
	type (+ vtable-offset-user 2)
	(lambda (obj)
	  (p-make (bs-addr (fh-object-val obj)))))
       (struct-set!		; value-at
	p-type (+ vtable-offset-user 3)
	(lambda (obj)
	  (make (bytestructure-ref (fh-object-val obj) '* obj))))))))

;; @deffn {Syntax} define-fh-compound-type type desc type? make
;; The first form generates an FY aggregate type based on a bytestructure
;; descriptor.  The second and third forms will build, in addition,
;; pointer-to type and pointer-to-pointer-to type.
;; @end deffn
(define-syntax define-fh-compound-type
  (syntax-rules ()
    ((_ type desc type? make)
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
	  ((arg)
	   (if (bytestructure? arg)
	       (make-struct/no-tail type arg)
	       (make-struct/no-tail type (bytestructure desc arg))))
	  (args
	   (make-struct/no-tail type (apply bytestructure desc args)))))
       (export type type? make)))))

(define-syntax define-fh-function*-type
  (syntax-rules ()
    ((_ type desc type? make)
     (begin
       (define type
	 (make-fht (quote type)
		   unwrap~pointer
		   (lambda (val)
		     (make (bytestructure desc (ffi:pointer-address val))))
		   #f
		   (lambda (obj) (fh-object-ref obj '*))
		   (make-bs*-printer (quote type))))
       (define (type? obj)
	 (and (fh-object? obj) (eq? (struct-vtable obj) type)))
       (define make
	 (case-lambda
	  ((val)
	   (cond
	    ((number? val)
	     (bytestructure desc val))
	    ((ffi:pointer? val)
	     (bytestructure desc (ffi:pointer-address val)))
	    ((procedure? val) ;; special case
	     (bytestructure
	      desc
	      (ffi:pointer-address
	       (ffi:procedure->pointer
		(function-metadata-return-descriptor desc)
		val
		(function-metadata-param-descriptor-list desc)))))
	    (else (error "bad argument type"))))
	  (()
	   (make-struct/no-tail type (bytestructure desc)))))
       (export type type? make)))))

;; @deffn {Procedure} fh:cast type value
;; @deffnx {Procedure} fh-cast type value
;; @example
;; (fh-cast foo_desc_t* 321)
;; (use-modules ((system foreign) #:prefix 'ffi:))
;; (fh-cast ffi:short 321)
;; @end deffn 
(define (fh:cast type value)
  (cons type value))
(define (fh-cast type expr)
  (cons type expr))

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

;; --- types 

;; All other FFI types are variables which as bound to constant expressions.
;; Here we bind '* to a variable to avoid special cases in the code generator.
(define ffi-void* '*)

(define double*-desc (bs:pointer double))
(define-fh-pointer-type double* double*-desc double*? make-double*)

(define int*-desc (bs:pointer int))
(define-fh-pointer-type fh-int* int*-desc int*? make-int*)

(define unsigned-int*-desc (bs:pointer unsigned-int))
(define-fh-pointer-type fh-unsigned-int* unsigned-int*-desc
  unsigned-int*? make-unsigned-int*)


;; --- other items

;; @deffn {Procedure} fh-link-proc name return args [library]
;; Generate Guile procedure from C library.  The argument @var{library}
;; results from @code{(dymamic-link "lib")}.  If @var{library} is not
;; provided @code{(dymamic-link)} is used.
;; @end deffn
(define* (fh-link-proc name return args #:optional library)
  (ffi:pointer->procedure
   return (dynamic-func name (or library (dynamic-link))) args))


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
     ((pair? arg)
      (car arg))
     (else
      (error "can't interpret argument"))))
  (define (arg->val arg)
    (cond
     ((bytestructure? arg)
      (bytestructure-ref arg))
     ((and (pair? arg) (bytestructure? (cdr arg)))
      (bytestructure-ref (cdr arg)))
     ((pair? arg)
      (cdr arg))
     (else
      arg)))
  (lambda args
    (let ((ffi-l (let iter ((param-l param-ffi-list) (argl args))
		   (cond
		    ((pair? param-l)
		     (cons (car param-l) (iter (cdr param-l) (cdr argl))))
		    ((pair? argl)
		     (cons (arg->ffi (car argl)) (iter param-l (cdr argl))))
		    (else '()))))
	  (arg-l (map arg->val args))
	  )
      (apply (ffi:pointer->procedure return-ffi pointer ffi-l) arg-l))))

;; right now the code generator only uses ffi types
(define (fh:function %return-desc %param-desc-list)
  #;(define (get-return-ffi syntax?)
    (if syntax?
	#`(bytestructure-descriptor->ffi-descriptor %return-desc)
	(bytestructure-descriptor->ffi-descriptor %return-desc)))
  (define (get-return-ffi syntax?)
    (if syntax?
	#`%return-desc
	%return-desc))
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

;; --- last line ---
