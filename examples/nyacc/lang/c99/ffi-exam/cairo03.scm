;; exam.d/cairo03.scm

(use-modules (cairo cairo))
(use-modules (system ffi-help-rt))	; pointer-to
(use-modules (system foreign))		; string->pointer

(define mx (make-cairo_matrix_t))
(simple-format #t "mx: ~S\n" mx)
(simple-format #t "&mx: ~S\n" (pointer-to mx))

;; --- last line ---
