;; dbus00.scm -- common items

(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (bytestructures guile))

(use-modules (ffi glib))
(use-modules (ffi gobject))
(use-modules (ffi gio))

(define (sf fmt . args) (apply simple-format #t fmt args))
(use-modules (ice-9 pretty-print))
(define pp pretty-print)

(define FALSE 0)

(define (got-error? error)
  (not (zero? (bytestructure-ref (fh-object-val error)))))

(define (gv-null? error)
  (zero? (bytestructure-ref (fh-object-val error))))

(define (g-error-message error)
  (let* ((eval (fh-object-ref error '* 'message))
	 (pval (ffi:make-pointer eval))
	 (sval (ffi:pointer->string pval)))
    sval))

(define glib-guardian (make-guardian))

(define gv-string-singleton-type	; gen. variant type "(s)"
  (let* ((code "s")
	 (cptr (ffi:string->pointer code)) ; GVariantType* for "s"
	 (cadr (ffi:pointer-address cptr))
	 (cvec (bytestructure (bs:vector 1 (bs:pointer int8)) (vector cadr)))
	 (cptr (ffi:make-pointer (bs-addr cvec)))
	 (gvar (g_variant_type_new_tuple cptr 1)))
    (glib-guardian code)		; guard "s" from collection
    gvar))

;; y uint8
;; b bool
;; n int16
;; q uint16
;; i int32
;; u uint32
;; x int64
;; t uint64
;; d double
;; s utf-8 string
;; o Dbus object path
;; g Dbus sig string
;; a array
;; ( struct beg
;; ) struct end
;; v variant

;; --- last line ---
