;; exam.d/cairo03.scm

(use-modules (cairo cairo))
(use-modules (ffi-help-rt))
(use-modules (system foreign)) ;; string->pointer

#|
(define srf (cairo_svg_surface_create (string->pointer "abc.svg") 200.0 200.0))
(define cr (cairo_create srf))
|#

(define mx (make-cairo_matrix_t))
(simple-format #t "mx: ~S\n" mx)
(simple-format #t "&mx: ~S\n" (pointer-to mx))

#|
(cairo_move_to cr 10.0 10.0)
(cairo_line_to cr 190.0 10.0)
(cairo_line_to cr 190.0 190.0)
(cairo_line_to cr 10.0 190.0)
(cairo_line_to cr 10.0 10.0)
(cairo_stroke cr)

(cairo_destroy cr)
(cairo_surface_destroy srf)
|#
;; --- last line ---
