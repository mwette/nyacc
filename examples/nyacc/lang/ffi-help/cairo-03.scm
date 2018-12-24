;; cairo-03.scm -- cairo matrix

;; Copyright (C) 2017 Matthew R. Wette
;;
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (ffi cairo))		; auto-generated from cairo.h etc
(use-modules (system ffi-help-rt))	; pointer-to

(define srf (cairo_image_surface_create 'CAIRO_FORMAT_ARGB32 120 120))
(define cr (cairo_create srf))

(define mx (make-cairo_matrix_t))
(cairo_matrix_init (pointer-to mx) 100 0 0 100 10 10)
(cairo_set_matrix cr (pointer-to mx))
(cairo_set_line_width cr 0.02)

(cairo_move_to cr 0.0 0.0)
(cairo_line_to cr 1.0 1.0)
(cairo_stroke cr)

(cairo_surface_write_to_png srf "cairo-03.png")
(cairo_destroy cr)
(cairo_surface_destroy srf)

;; --- last line ---
