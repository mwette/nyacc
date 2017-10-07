;; exam.d/cairo03.scm

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (ffi cairo))		; auto-generated from cairo.h etc
(use-modules (system ffi-help-rt))	; pointer-to
(use-modules (system foreign))		; string->pointer, pointer<->scm

(define srf (cairo_image_surface_create 'CAIRO_FORMAT_ARGB32 200 200))
(define cr (cairo_create srf))

(define mx (make-cairo_matrix_t))
(simple-format #t "mx: ~S\n" mx)
(simple-format #t "&mx: ~S\n" (pointer-to mx))

(cairo_surface_write_to_png srf "cairo02.png")
(cairo_destroy cr)
(cairo_surface_destroy srf)

;; --- last line ---
