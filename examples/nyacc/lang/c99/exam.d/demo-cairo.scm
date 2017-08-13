;;; demo-cairo.scm

;; Copyright (C) 2017 Matthew R. Wette

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (cairo))
(use-modules (systme ffi-help-rt))
(use-modules (system foreign)) ;; string->pointer

;;(define mx (make-cairo_matrix_t))
;;(simple-format #t "mx: ~S\n" mx)
;;(simple-format #t "&mx: ~S\n" (pointer-to mx))

(define srf (cairo_svg_surface_create (string->pointer "abc.svg") 200.0 200.0))

(define cr (cairo_create srf))

(cairo_move_to cr 10.0 10.0)
(cairo_line_to cr 190.0 10.0)
(cairo_line_to cr 190.0 190.0)
(cairo_line_to cr 10.0 190.0)
(cairo_line_to cr 10.0 10.0)
(cairo_stroke cr)

;; need to add guardians?
(cairo_destroy cr)
(cairo_surface_destroy srf)

;;; --- last line ---
