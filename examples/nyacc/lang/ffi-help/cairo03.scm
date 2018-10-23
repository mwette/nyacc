;; cairo03.scm -- cairo matrix

;; Copyright (C) 2017 Matthew R. Wette
;;
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (ffi cairo))		; auto-generated from cairo.h etc
(use-modules (system ffi-help-rt))	; pointer-to

(define srf (cairo_image_surface_create 'CAIRO_FORMAT_ARGB32 256 256))
(define cr (cairo_create srf))

(define mx (make-cairo_matrix_t))
(cairo_matrix_init (pointer-to mx) 100 0 0 100 10 10)
(cairo_move_to cr 0 0)
(cairo_line_to cr 5 5)

(cairo_surface_write_to_png srf "cairo03.png")
(cairo_destroy cr)
(cairo_surface_destroy srf)

;; --- last line ---
