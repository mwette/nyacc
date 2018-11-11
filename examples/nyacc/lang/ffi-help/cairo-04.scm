;; cairo-04.scm - text demo from cairographics.org:
;;   https://www.cairographics.org/samples/text_align_center/

;; Copyright (C) 2017 Matthew R. Wette
;;
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (ffi cairo))
(use-modules (system ffi-help-rt))

(define srf (cairo_image_surface_create 'CAIRO_FORMAT_ARGB32 256 256))
(define cr (cairo_create srf))

(define extents (make-cairo_text_extents_t))

(define text "cairo")

(cairo_select_font_face cr "Sans"
			'CAIRO_FONT_SLANT_NORMAL
			'CAIRO_FONT_WEIGHT_NORMAL)

(cairo_set_font_size cr 52.0)
(cairo_text_extents cr text (pointer-to extents))
(define x (- 128.0 (+ (/ (fh-object-ref extents 'width) 2.0)
		      (fh-object-ref extents 'x_bearing))))
(define y (- 128.0 (+ (/ (fh-object-ref extents 'height) 2.0)
		      (fh-object-ref extents 'y_bearing))))

(cairo_move_to cr x y)
(cairo_show_text cr text)

;; draw helping lines 
(cairo_set_source_rgba cr 1 0.2 0.2 0.6)
(cairo_set_line_width cr 6.0)
(cairo_arc cr  x  y  10.0 0 (* 2 M_PI))
(cairo_fill cr)
(cairo_move_to cr 128.0 0)
(cairo_rel_line_to cr 0 256)
(cairo_move_to cr 0 128.0)
(cairo_rel_line_to cr 256 0)
(cairo_stroke cr)

(cairo_surface_write_to_png srf "cairo-04.png")
(cairo_destroy cr)
(cairo_surface_destroy srf)

;; --- last line ---
