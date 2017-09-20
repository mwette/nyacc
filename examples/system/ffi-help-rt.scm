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
	    pointer-to points-to
	    fh-cast
	    void*
	    
	    define-fh-pointer-type
	    define-fh-compound-type define-fh-compound-type/p
	    define-fh-fixed define-fh-fixed/p
	    define-fh-float define-fh-float/p
	    define-fh-enum
	    define-fh-function define-fh-function/p
	    unwrap~fixed unwrap~float
	    unwrap~pointer unwrap~array
	    make-ftn-arg-unwrapper
	    fh-link-proc
	    ref<->deref!

	    fh-bsref fh-bsset!
	    
	    ;; debugging
	    fht-unwrap fht-pointer-to fht-points-to
	    bs-data-address make-bs-printer
	    ;; deprecated
	    )
  #:use-module (bytestructures guile)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign) #:prefix ffi:)
  #:version (0 82 2)
  )

;; All other FFI types are variables which as bound to constant expressions.
;; Here we bind '* to a variable to avoid special cases in the code generator.
(define void* '*)

;; The FFI helper uses a base type based on Guile structs and vtables.
;; The base vtable uses these (lambda (obj) ...) fields:
;; 0 unwrap	: convert helper-type object to ffi argument
;; 1 pointer-to	: (pointer-to <foo_t-obj>) => <foo_t*-obj>
;; 2 points-to	: (points-to <foo_t*-obj>) => <foo_t-obj>
;; The C-based types will add a slot for the object value.
(define ffi-helper-type
  (make-vtable
   (string-append standard-vtable-fields "prpwpw")
   (lambda (v p)
     (display "#<ffi-helper-type>" p))))

;; @deffn {Procedure} fh-type? type
;; This predicate tests for FH types.
;; @end deffn
(define (fh-type? type)
  (and (struct? type)
       (struct-vtable? type)
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
  (and
   (struct? obj)
   (fh-type? (struct-vtable obj))))

;; @deffn {Procedure} fh-object-val obj
;; Return the value associate with the object.  This is most often
;; (I think) a bytestructure.
;; @deffn
(define (fh-object-val obj)
  (struct-ref obj 0))

(define (fht-unwrap obj)
  (struct-ref obj (+ vtable-offset-user 0)))
(define (fht-pointer-to obj)
  (struct-ref obj (+ vtable-offset-user 1)))
(define (fht-points-to obj)
  (struct-ref obj (+ vtable-offset-user 2)))

;; @deffn {Syntax} make-fh-type name unwrap pointer-to value-at printer
;; We call make-struct here but we are actually making a vtable
;; We should check with struct-vtable?
;; name as symbol
(define* (make-fht name unwrap pointer-to value-at printer)
  ;;(simple-format #t "make-fht: ~S\n" name)
  (let* ((ty (make-struct/no-tail ffi-helper-type
				  (make-struct-layout "pw") ;; 1 slot for value
				  printer unwrap pointer-to value-at))
	 (vt (struct-vtable ty)))
    (set-struct-vtable-name! vt name)
    ty))

(eval-when (expand load eval)
  (define (gen-id tmpl-id . args)
    (define (stx->str stx)
      (symbol->string (syntax->datum stx)))
    (datum->syntax
     tmpl-id
     (string->symbol
      (apply string-append
	     (map (lambda (ss) (if (string? ss) ss (stx->str ss))) args))))))

;; @deffn {Procedure} bs-data-address bs
;; Return the raw, numerical address of the bytestruture bytevector data.
;; @end deffn
(define (bs-data-address bs)
  (ffi:pointer-address
   (ffi:bytevector->pointer
    (bytestructure-bytevector bs))))

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
      (display (number->string (bs-data-address (struct-ref obj 0)) 16) port))
    (display ">" port)))

(define (make-bs*-printer type)
  (lambda (obj port)
    (display "#<" port)
    (display type port)
    (display " 0x" port)
    (display (number->string (bytestructure-ref (struct-ref obj 0)) 16) port)
    (display ">" port)))

;; @deffn {Syntax} define-fh-pointer-type name
;; @deffnx {Syntax} define-fh-pointer-type name desc
;; @example
;; (define foo_t*-desc (bs:pointer foo_t-desc))
;; (define-fh-pointer-type foo_t*
;; @end example
;; The second form is based on already defined @code{bs:pointer} descriptor.
;; @end deffn
(define-syntax define-fh-pointer-type
  (lambda (x)
    (syntax-case x ()
      ((_ type desc)			; based on bytestructure
       (with-syntax ((make (gen-id #'type "make-" #'type))
		     (type? (gen-id #'type #'type "?"))
		     (wrap (gen-id #'type "wrap-" #'type))
		     (unwrap (gen-id #'type "unwrap-" #'type)))
	 #'(begin
	     (define (make val)
	       (cond
		((bytestructure? val)
		 (make-struct/no-tail type val))
		((bytevector? val)
		 (make-struct/no-tail type (bytestructure desc val)))
		((number? val)
		 (make-struct/no-tail type (bytestructure desc val)))
		(else (make-struct/no-tail type val))))
	     (define (type? obj)
	       (and (fh-object? obj) (eq? (struct-vtable obj) type)))
	     (define (unwrap obj)
	       (unwrap~pointer obj))
	     (define type
	       (make-fht (quote type) unwrap #f #f
			 (make-bs*-printer (quote type))))
	     (define (wrap val) ;; pointer returned from code
	       (make (bytestructure desc (ffi:pointer-address val))))
	     (export make type? unwrap type wrap)
	     )))
	     
      ((_ type)		      ; based on guile pointer wrapper
       #'(define-fh-pointer-type type (bs:pointer void)))
      )))

;; @deffn {Syntax} define-fh-compound-type name desc
;; @deffnx {Syntax} define-fh-compound-type/p name desc
;; @deffnx {Syntax} define-fh-compound-type/pp name desc
;; The first form generates an FY aggregate type based on a bytestructure
;; descriptor.  The second and third forms will build, in addition,
;; pointer-to type and pointer-to-pointer-to type.
;; @end deffn
(define-syntax define-fh-compound-type
  (lambda (x)
    (syntax-case x ()
      ((_ type desc)
       (with-syntax ((unwrap (gen-id #'type "unwrap-" #'type))
		     (type? (gen-id #'type #'type "?"))
		     (make (gen-id #'type "make-" #'type))
		     (wrap (gen-id #'type "wrap-" #'type)))
	 #'(begin
	     (define (unwrap obj)
	       (bytestructure-bytevector (struct-ref obj 0)))
	     (define type
	       (make-fht (quote type) unwrap #f #f
			 (make-bs-printer (quote type))))
	     (define (type? obj)
	       (and (fh-object? obj) (eq? (struct-vtable obj) type)))
	     #;(define (make . args)
	       (make-struct/no-tail type (apply bytestructure desc args)))
	     (define make
	       (case-lambda
		((arg)
		 (if (bytestructure? arg)
		     (make-struct/no-tail type arg)
		     (make-struct/no-tail type (bytestructure desc arg))))
		(args
		 (make-struct/no-tail type (apply bytestructure desc args)))))
	     (define (wrap raw)	; raw is bytevector
	       (make-struct/no-tail type (bytestructure desc raw)))
	     (export type type? make wrap unwrap)
	     ))))))

;; @deffn {Procedure} ref<->deref! p-type type
;; This procedure will ``connect'' the two types so that the procedures
;; @code{pointer-to} and @code{value-at} work.
;; @end deffn
(define-syntax ref<->deref!
  (lambda (x)
    (syntax-case x ()
      ((_ p-type type)
       (with-syntax ((p-make (gen-id #'type "make-" #'type "*"))
		     (p-desc (gen-id #'type  #'type "*-desc"))
		     (make (gen-id #'type "make-" #'type)))
	 #'(begin
	     (struct-set!		; pointer-to
	      type (+ vtable-offset-user 1)
	      (lambda (obj)
		(p-make (bs-data-address (struct-ref obj 0)))))
	     (struct-set!		; value-at FIX LATER
	      type (+ vtable-offset-user 2)
	      (lambda (obj) ;; CHECK THIS
		(make (bytestructure-ref p-desc '* obj))))))))))

(define-syntax define-fh-compound-type/p
  (lambda (x)
    (syntax-case x ()
      ((_ type desc)
       (with-syntax ((p-type (gen-id #'type #'type "*"))
		     (p-desc (gen-id #'type #'type "*-desc"))
		     (p-make (gen-id #'type "make-" #'type "*"))
		     (make (gen-id #'type "make-" #'type)))
	 #'(begin
	     (define-fh-compound-type type desc)
	     (define p-desc (bs:pointer desc))
	     (export p-desc)
	     (define-fh-pointer-type p-type p-desc)
	     (ref<->deref! p-type type)
	     (export type desc p-type p-desc)))))))

;; @deffn {Syntax} define-fh-fixed name desc
;; @deffn {Syntax} define-fh-fixed/p name desc
;; Define wrappers for a fixed type, and optionally for its pointer type.
;; (Is descriptor needed?)
;; @end deffn
(define-syntax define-fh-fixed
  (lambda (x)
    (syntax-case x ()
      ((_ type desc)
       (with-syntax ((unwrap (gen-id #'type "unwrap-" #'type))
		     (wrap (gen-id #'type "wrap-" #'type)))
	 #'(begin
	     (define unwrap unwrap~fixed)
	     (define wrap identity)
	     (export wrap unwrap)))))))
(define-syntax define-fh-fixed/p
  (lambda (x)
    (syntax-case x ()
      ((_ type desc)
       (with-syntax ((unwrap* (gen-id #'type "unwrap-" #'type "*"))
		     (wrap* (gen-id #'type "wrap-" #'type "*"))
		     (desc* (gen-id #'type "*-desc")))
	 #'(begin
	     (define-fh-fixed type desc)
	     (define desc* (bs:pointer desc))
	     (define unwrap* unwrap~pointer)
	     (define wrap* identity)
	     (export desc* wrap* unwrap*)))))))

;; @deffn {Syntax} define-fh-float name desc
;; @deffn {Syntax} define-fh-float/p name desc
;; Define wrappers for a fixed type, and optionally for its pointer type.
;; (Is descriptor needed?)
;; @end deffn
(define-syntax define-fh-float
  (lambda (x)
    (syntax-case x ()
      ((_ type desc)
       (with-syntax ((unwrap (gen-id #'type "unwrap-" #'type))
		     (wrap (gen-id #'type "wrap-" #'type)))
	 #'(begin
	     (define unwrap unwrap~float)
	     (define wrap identity)
	     (export wrap unwrap)))))))
(define-syntax define-fh-float/p
  (lambda (x)
    (syntax-case x ()
      ((_ type desc)
       (with-syntax ((unwrap* (gen-id #'type "unwrap-" #'type "*"))
		     (wrap* (gen-id #'type "wrap-" #'type "*"))
		     (desc* (gen-id #'type "*-desc")))
	 #'(begin
	     (define-fh-float type desc)
	     (define desc* (bs:pointer desc))
	     (define unwrap* unwrap~pointer)
	     (define wrap* identity)
	     (export desc* wrap* unwrap*)))))))



;; @deffn {Syntax} make-fh-enum
;; This makes enum wrapper unwrapper, and descriptor (for int).
;; @end deffn
(define-syntax define-fh-enum
  (lambda (x)
    (syntax-case x ()
      ((_ type nv-map)			; based on bytestructure
       (with-syntax ((desc (gen-id #'type #'type "-desc"))
		     (unwrap (gen-id #'type "unwrap-" #'type))
		     (wrap (gen-id #'type "wrap-" #'type))
		     (unwrap* (gen-id #'type "unwrap-" #'type "*")))
         #'(begin
	     (define desc int)
	     (define wrap
	       (let ((vnl (map (lambda (pair) (cons (cdr pair) (car pair)))
			       nv-map)))
		 (lambda (code) (assq-ref vnl code))))
	     (define unwrap
	       (let ((nvl nv-map))
		 (lambda (name) (assq-ref nvl name))))
	     (define (unwrap* obj) ;; ugh
	       (error "pointer to enum type not done"))
	     (export desc wrap unwrap unwrap*)))))))

;; @deffn {Syntax} define-fh-function name return-type arg-types
;; @deffnx {Syntax} define-fh-function/p name return-type arg-types
;; Define wrapper and unwrapper for the function.  With @code{/p} form
;; also define pointer descriptor and wrap/unwrap alias.
;; @end deffn
(define-syntax define-fh-function
  (lambda (x)
    (syntax-case x ()
      ((_ name return-t args-t)
       (with-syntax ((wrap (gen-id #'name "wrap-" #'name))
		     (unwrap (gen-id #'name "unwrap-" #'name)))
	 #'(define-fh-function name return-t args-t wrap unwrap)))
      ((_ name return-t args-t wrap unwrap)
       (with-syntax ((desc (gen-id #'name #'name "-desc")))
	 #'(begin
	     (define desc (bs:pointer void))
	     (define (wrap proc)
	       (ffi:pointer->procedure return-t proc args-t))
	     (define (unwrap ptr)
	       (ffi:procedure->pointer return-t ptr args-t))
	     (export desc unwrap wrap))))
      )))

;; @deffn {Procecure} fh-case type expr
;; For use on varargs calls, cast to a ffi type.
;; @example
;; (use-modules ((system foreign) #:prefix 'ffi:))
;; (fh-cast ffi:short 321)
;; @end example
;; @end deffn
(define (fh-cast type expr)
  (cons type expr))

(define-syntax define-fh-function/p
  (lambda (x)
    (syntax-case x ()
      ((_ name return-t args-t)
       (with-syntax ((wrap (gen-id #'name "wrap-" #'name))
		     (unwrap (gen-id #'name "unwrap-" #'name))
		     (desc* (gen-id #'name #'name "*-desc"))
		     (wrap* (gen-id #'name "wrap-" #'name "*"))
		     (unwrap* (gen-id #'name "unwrap-" #'name "*")))
	 #'(begin
	     (define-fh-function name return-t args-t wrap unwrap)
	     (define desc* (bs:pointer intptr_t))
	     (define wrap* wrap)
	     (define unwrap* unwrap)
	     (export desc* wrap* unwrap*)))))))

;; right now this returns a ffi pointer
;; it should probably be a bs:pointer
(define (pointer-to obj)
  ((fht-pointer-to (struct-vtable obj)) obj))

;; @deffn {Procedure} make-ftn-arg-unwrapper ret-t args-t => lambda
;; This procedure will convert an argument, 
;; @end deffn
(define (make-ftn-arg-unwrapper ret-t args-t)
  (lambda (obj)
    (cond
     ((ffi:pointer? obj) obj)
     ((procedure? obj) (ffi:procedure->pointer ret-t obj args-t))
     (else (error "expecting pointer or procedure")))))

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

;; @deffn {Procedure} fh-link-proc name return args [library]
;; Generate Guile procedure from C library.  The argument @var{library}
;; results from @code{(dymamic-link "lib")}.  If @var{library} is not
;; provided @code{(dymamic-link)} is used.
;; @end deffn
(define* (fh-link-proc name return args #:optional library)
  (ffi:pointer->procedure
   return (dynamic-func name (or library (dynamic-link))) args))


;; @deffn {Syntax} define-fh-base-type
;; Define wrappers for base types like @code{double}, @code{int}, etc.
;; @end deffn
(define-syntax-rule (define-fh-base-type type ...)
  (define-fh-compound-type/p type ...))

(define double-desc double)
(define-fh-base-type fh-double double-desc)
(define int-desc int)
(define-fh-base-type fh-int int-desc)
(define unsigned-int-desc unsigned-int)
(define-fh-base-type fh-unsigned-int unsigned-int-desc)

(define-syntax-rule (fh-bsref obj arg ...)
  (bytestructure-ref (fh-object-val obj) arg ...))

(define-syntax-rule (fh-bsset! obj arg ...)
  (bytestructure-set! (fh-object-val obj) arg ...))

;; === deprecated ===================

;; @deffn {Syntax} define-fh-base-type
;; supports pointer also
;; @end deffn
(define-syntax define-fh-base-type-xxx
  (lambda (x)
    (syntax-case x ()
      ((_ type ffi-type)
       (with-syntax ((make (gen-id #'type "make-" #'type))
		     (type? (gen-id #'type #'type "?"))
		     (wrap (gen-id #'type "wrap-" #'type))
		     (unwrap (gen-id #'type "unwrap-" #'type))
		     (p-type (gen-id #'type #'type "*"))
		     )
	 #'(begin
	     (define make
	       (case-lamba
		((val) (make-struct/no-tail type val))
		(() (make-struct/no-tail type (default-val ffi-type)))))
	     (define type
	       (make-fht (quote type) unwrap
			 (lambda (obj) 0) ;; pointer-to
			 #f (make-bs-printer (quote type))))
	     ))))))

(define (default-val ffi-type)
  #f)

;; --- last line ---
