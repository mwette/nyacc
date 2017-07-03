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
	    )
  #:use-module ((system foreign) #:prefix 'ffi:)
  #:version (0 10 0)
  )

;; ffi-helper base type (aka class) with fields
;; * name
;; * wrap
;; * unwrap
;; * pointer-to : (pointer-to foo_t-obj) => address-of-obj
;; * points-to : (dereference address-of-obj) => foo_t-obj
(define ffi-helper-type
  (make-vtable
   (string-append standard-vtable-fields "pwpwpwpw")
   (lambda (v p)
     (display "#<ffi-helper-type>" p))))

(define (fht-name obj)
  (struct-ref obj (+ vtable-offset-user 0)))
(define (fht-wrap obj)
  (struct-ref obj (+ vtable-offset-user 1)))
(define (fht-unwrap obj)
  (struct-ref obj (+ vtable-offset-user 2)))
(define (fht-pointer-to obj)
  (struct-ref obj (+ vtable-offset-user 3)))
(define (fht-reference-to

;; We call make-struct here but we are actually making a vtable
;; We should check with struct-vtable?
(define* (make-fht name wrap unwrap #:optional pointer-to)
  (let (
  ;;(make-struct/no-tail
  )


(define-syntax define-fh-pointer-type
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
      ((_ type)
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
             (export type pred wrap unwr)))))))

;; @deffn {Syntax} define-fh-aggregate-type name desc
;; generates a struct type
;; @example
;; (define-fh-aggregate-type foo_t (bs:struct `((x ,int) (y ,int))))
;; =>
;; (define foo_t <vtable>) ;; with slots for value and pointer-to
;; (define foo_t-desc (bs:struct  `((x ,int) (y ,int))))
;; (define make-foo_t) => obj
;; (define foo_t? obj) == (eq? foo-desc (bytevector-descriptor ... obj))
;; @end example
;; @noindent
;; @example
;; make-name => <obj>
;; name? <obj> 
;; name-bvec <obj> => bytevector
;; name-desc <obj> => descriptor
;; @end example
;; @end deffn
(define-syntax define-fh-aggregate-type
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
  ((struct-ref obj 1) obj))

;; --- last line ---
