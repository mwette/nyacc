;; exam.d/cairo02.scm

(use-modules (cairo cairo))		; auto-generated from cairo.h etc
(use-modules (system ffi-help-rt))	; pointer-to
(use-modules (system foreign))		; string->pointer, pointer<->scm

(define srf (cairo_svg_surface_create (string->pointer "cairo02.svg")
				      200.0 200.0))
(define cr (cairo_create srf))

;; typedef struct _cairo_user_data_key {
;;   int unused;
;; } cairo_user_data_key_t;
;;
;; typedef void (*cairo_destroy_func_t)(void *data);
;;
;; cairo_status_t cairo_set_user_data(cairo_t *cr, const cairo_user_data_key_t
;;      *key, void *user_data, cairo_destroy_func_t destroy);

(define k1 (make-cairo_user_data_key_t)) ; make a key
(define v1 '((abc . 123) (def . 456)))	 ; make some data
(define (d1 data)			 ; callback
  (simple-format #t "d1 called with ~S\n" (pointer->scm data)))
	 
(cairo_set_user_data cr (pointer-to k1) (scm->pointer v1) d1)

(cairo_destroy cr)
(cairo_surface_destroy srf)

;; --- last line ---
