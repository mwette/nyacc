;; cairo-02.scm - try key

;; Copyright (C) 2017 Matthew R. Wette

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (ffi cairo))		; auto-generated from cairo.h etc
(use-modules (system ffi-help-rt))	; pointer-to
(use-modules (system foreign))		; pointer<->scm

(define srf (cairo_image_surface_create 'CAIRO_FORMAT_ARGB32 200 200))
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

(cairo_surface_write_to_png srf "cairo-02.png")
(cairo_destroy cr)
(cairo_surface_destroy srf)

;; --- last line ---
