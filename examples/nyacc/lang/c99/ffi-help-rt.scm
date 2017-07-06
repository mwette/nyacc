;;; example/nyacc/lang/c99/ffi-help-rt.scm
;;;
;;; Copyright (C) 2016-2017 Matthew R. Wette
;;;
;;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;;; or any later version published by the Free Software Foundation.  See
;;; the file COPYING included with the nyacc distribution.

;; runtime for generated ffi-compiled dot-ffi files

(define-module (ffi-help-rt)
  #:export (define-fh-aggregate-type
	    define-fh-pointer-type
	    pointer-to
	    unwrap-fixed
	    unwrap-float
	    unwrap-enum
	    unwrap-pointer
	    wrap-void*
	    )
  #:use-module ((system foreign) #:prefix 'ffi:)
  #:version (0 10 0)
  )

;; ffi-helper base type (aka class) with fields
;; 0 wrap
;; 1 unwrap
;; 2 pointer-to : (pointer-to foo_t-obj) => address-of-obj
;; 3 points-to : (points-to address-of-obj) => foo_t-obj
;;   dereference : (dereference address-of-obj) => foo_t-obj
(define fh-base-type
  (make-vtable
   (string-append standard-vtable-fields "prprpwpw")
   (lambda (v p)
     (display "#<fh-base-type>" p))))

;; @deffn {Procedure} fh-type? type
;; This predicate tests for FH types.
;; @end deffn
(define (fh-type? type)
  (and (struct? type)
       (struct-vtable? type)
       (eq? (struct-vtable type) fh-base-type)))

;; @deffn {Procedure} fh-object? obj
;; This predicate tests for FH objects.
;; @end deffn
(define (fh-object? obj)
  (and
   (struct? obj)
   (fh-type? (struct-vtable obj))))

(define (fht-wrap obj)
  (struct-ref obj (+ vtable-offset-user 0)))
(define (fht-unwrap obj)
  (struct-ref obj (+ vtable-offset-user 1)))
(define (fht-pointer-to obj)
  (struct-ref obj (+ vtable-offset-user 2)))
(define (fht-points-to obj)
  (struct-ref obj (+ vtable-offset-user 3)))

;; We call make-struct here but we are actually making a vtable
;; We should check with struct-vtable?
;; name as symbol
(define* (make-fht name wrap unwrap pointer-to points-to printer)
  (let* ((ty (make-struct/no-tail ffi-helper-type
				  (make-struct-layout "pw") ;; 1 slot for value
				  printer wrap unwrap pointer-to points-to))
	 (vt (struct-vtable ty)))
    (set-struct-vtable-name! vt name)
    ty))

;; type printer for bytestructures-based types
(define (make-bs-printer type)
  (lambda (obj port)
    (display "#<" port)
    (display type port)
    (when #f
      (display " bs-desc:0x" port)
      (display (number->string
		(scm->pointer
		 (struct-ref obj 0))
		16) port))
    (when #t
      (display " 0x" port)
      (display (number->string
		(ffi:pointer-address
		 (ffi:scm->pointer
		  (bytestructure-bytevector
		   (struct-ref obj 0))))
		16) port))
    (display ">" port)))

;; @deffn {Syntax} define-fh-aggregate-type name desc
;; @deffnx {Syntax} define-fh-aggregate-type/p name desc
;; @deffnx {Syntax} define-fh-aggregate-type/pp name desc
;; The first form generates an FY aggregate type based on a bytestructure
;; descriptor.  The second and third forms will build, in addition,
;; pointer-to type and pointer-to-pointer-to type.
;; @end deffn
(define-syntax define-fh-aggregate-type
  (lambda (x)
    (define (stx->str stx)
      (symbol->string (syntax->datum stx)))
    (define (gen-id tmpl-id . args)
      (datum->syntax
       tmpl-id
       (string->symbol
	(apply string-append
	       (map (lambda (ss) (if (string? ss) ss (stx->str ss))) args)))))
    (syntax-case x ()
      ((_ type desc)
       (with-syntax
	   ((type? (gen-id x #'type "?"))
	    (make (gen-id x "make-" #'type))
	    (wrap (gen-id x "wrap-" #'type))
	    (unwrap (gen-id x "unwrap-" #'type)))
	 #`(begin
	     (define (make . args)
	       (make-struct/no-tail type (apply bs:bytestructure desc args)))
	     (define (wrap raw)		; raw is bytevector
	       (make raw))
	     (define (unwrap obj)
	       (bytestructure-bytevector (struct-ref obj 0)))
	     (define type
	       (make-fht (quote #'type) wrap unwrap #f #f
			 (make-bs-printer #'type)))
	     (define (type? obj)
	       (and (fh-object? obj)
		    (eq? (struct-vtable obj) type)))
	     (export make wrap unwrap type type?)))))))

;; @deffn {Procedure} ref<->deref! p-type type
;; This procedure will ``connect'' the two types so that the procedures
;; @code{pointer-to} and @code{points-to} work.
;; @end deffn
(define (ref<->deref! p-type type)
  (or (fh-type? p-type) (error "not a base type:" p-type))
  (or (fh-type? type) (error "not a base type:" type))
  (struct-set! type 2 p-type)
  (struct-set! p-type 3 type))

(define-syntax define-fh-pointer-type
  (lambda (x)
    (define (stx->str stx)
      (symbol->string (syntax->datum stx)))
    (define (gen-id tmpl-id . args)
      (datum->syntax
       tmpl-id
       (string->symbol
	(apply string-append
	       (map (lambda (ss) (if (string? ss) ss (stx->str ss))) args)))))
    (syntax-case x ()
      ((_ type desc)			; based on bytestructure
       (with-syntax
	   ((pred (gen-id x #'type "?"))
	    (wrap (gen-id x "wrap-" #'type))
	    (unwr (gen-id x "unwrap-" #'type))
	    )
         #`(begin
             (define-wrapped-pointer-type type pred wrap unwr
               (lambda (v p)
		 (display "#<" p)
		 (display (symbol->string (quote type)) p)
		 (display " 0x" p)
		 (display (number->string (ffi:pointer-address (unwr v)) 16) p)
		 (display ">" p)))
             (export type pred wrap unwr))))
      ((_ type)			      ; based on guile pointer wrapper
       (with-syntax
	   ((pred (gen-id x #'type "?"))
	    (wrap (gen-id x "wrap-" #'type))
	    (unwr (gen-id x "unwrap-" #'type))
	    )
         #`(begin
             (define-wrapped-pointer-type type pred wrap unwr
               (lambda (v p)
		 (display "#<" p)
		 (display (symbol->string (quote type)) p)
		 (display " 0x" p)
		 (display (number->string (ffi:pointer-address (unwr v)) 16) p)
		 (display ">" p)))
             (export type pred wrap unwr))))
      )))

(define-syntax define-fh-enum-type
  (lambda (x)
    (define (stx->str stx)
      (symbol->string (syntax->datum stx)))
    (define (gen-id tmpl-id . args)
      (datum->syntax
       tmpl-id
       (string->symbol
	(apply string-append
	       (map (lambda (ss) (if (string? ss) ss (stx->str ss))) args)))))
    (syntax-case x ()
      ((_ type nv-map)			; based on bytestructure
       (with-syntax
	   ((pred (gen-id x #'type "?"))
	    (wrap (gen-id x "wrap-" #'type))
	    (unwr (gen-id x "unwrap-" #'type))
	    )
         #`(begin
	     (
	     (define wrap
	       (let ((vnl (map (lambda (pair) (cons (cdr pair) (car pair)))
			       nv-map)))
		 (lambda (code) (assq-ref vnl code))))
	     (define unwrap
	       (let ((nvl nv-map))
		 (lambda (name) (assq-ref nvl name))))
	     
	     
(define-syntax old-define-fh-aggregate-type
  (lambda (x)

    (define (stx->str stx)
      (symbol->string (syntax->datum stx)))

    (define (gen-id tmpl-id . args)
      (datum->syntax
       tmpl-id (string->symbol
		(apply string-append
		       (map (lambda (x) (if (string? x) x (stx->str x)))
			    args)))))

    (syntax-case x ()
      ((_ type aggr-def)
       (with-syntax
	   ((type-desc (gen-id x #'type "-desc"))
	    (type? (gen-id x #'type "?"))
	    (make-type (gen-id x "make-" #'type))
	    (wrap (gen-id x "wrap-" #'type "*"))
	    )
	 
	 #`(begin
	     (define type-desc addr-def)
	     ;; 0: desc; 1: pointer-to
	     ;; display the address used for C code
	     (define type
	       (make-vtable
		"pwpr"
		(lambda (obj port)
		  (display "#<" port)
		  (display (symbol->string (quote type)) port)
		  (write-char #\space port)
		  (when #f
		    (display "bs-desc:0x" port)
		    (display (number->string
			      (scm->pointer (struct-ref obj 0))
			     16) port)
		    (write-char #\space port))
		  (display "0x" port)
		  (display (number->string
			    (ffi:pointer-address
			     (ffi:scm->pointer
			      (bs:bytestructure-bytevector (struct-ref obj 0))))
			    16) port)
		  (display ">" port))))

	     (define (type? obj)
	       (and (struct? obj) (eq? (struct-vtable obj) type)))
	     (define make-type
	       (let ((desc type-desc)
		     (obj->pointer (lambda (obj)
				     (wrap
				      (scm->pointer
				       (bs:bytestructure-bytevector
					(struct-ref obj 0))))))
		     )
		 (lambda args
		   (make-struct/no-tail type
					(apply bs:bytestructure desc args)
					obj->pointer))))

	     (export type type-desc type? make-type)))))))

;; right now this returns a ffi pointer
;; it should probably be a bs:pointer
(define (pointer-to obj)
  ((fht-pointer-to obj) obj))


;; now support for the base types

(define (unwrap-fixed obj)
  (cond
   ((number? obj) obj)
   ((bytestructure? obj) (bytestructure-ref obj))
   ((fh-object? obj) (struct-ref obj 0))
   (else (error "type mismatch"))))

(define unwrap-float unwrap-fixed)

(define (unwrap-enum value)
  (cond
   ((number? obj) obj)
   ((symbol? obj) (local-lookup obj))
   ((fh-object? obj) (struct-ref obj 0)) ;; ???
   (else (error "type mismatch"))))

(define (unwrap-pointer obj)
  (cond
   ((pointer? obj) (ffi:pointer-address obj))
   ((bytestructure? obj) (bytestructure-ref obj))
   ((fh-object? obj) (unwrap-pointer (struct-ref obj 0)))
   (else (error "expecting pointer type"))))

(define (wrap-void* raw)
  (ffi:make-pointer raw))

;; --- last line ---
;; (pointer->bytevector ptr len) => bytevector
;; (bytevector->pointer bv) => pointer

;; another way to handle this is to define the types
;; (define foo_t-desc (bs:struct ...))
;; (define-type foo_t foo_t-desc)
;; (define foo_t-*desc (bs:pointer foo_t-desc))
;; (define-type foo_t* foo_t-*desc)
;; (ref<->deref foo_t* foo_t)

;; typedef struct { ... } foo_t;
;; (define foo_t-desc (bs:struct ...))
;; (define-aggregate-type foo_t foo_t-desc)
;;   (define foo_t (make-struct ...))
;;   (define foo_t*-desc (bs:pointer foo_t-desc))
;; (define obj (make-foo_t #(...)))
;; (pointer-to obj) => p-obj
;; (unwrap-foo_t*
;;   (dereference-pointer
;;     (bytevector->pointer
;;        (bytestructure-bytevector p-obj))))

