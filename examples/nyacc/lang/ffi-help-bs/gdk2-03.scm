;; gdk2-03.scm

;; https://www.manpagez.com/html/gdk2/gdk2-2.24.28/

;; Copyright (C) 2022,2024 Matthew Wette

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (ffi-bs gdk2))
(use-modules (ffi-bs cairo))
(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))
(define GDK gdk-symval)

(define (NULL? ptr) (equal? ptr ffi:%null-pointer))

;; Initialize.
(gdk_init NULL NULL)

(define win
  (let* ((attr (make-GdkWindowAttr)))
    (fh-object-set! attr 'event_mask (GDK 'GDK_EXPOSE))
    (fh-object-set! attr 'width 400)
    (fh-object-set! attr 'height 300)
    (fh-object-set! attr 'wclass (GDK 'GDK_INPUT_OUTPUT))
    (fh-object-set! attr 'window_type (GDK 'GDK_WINDOW_TOPLEVEL))
    (gdk_window_new NULL (pointer-to attr) 0)))

(define cr (gdk_cairo_create win))
;;(define srf (gdk_window_create_similar_surface win))
;;(define cr (cairo_create srf))

(define (paint-1 cro)
  (let* ((text "Hi")
         (extents (make-cairo_text_extents_t)))
    (cairo_select_font_face
     cro "Sans" 'CAIRO_FONT_SLANT_NORMAL 'CAIRO_FONT_WEIGHT_NORMAL)
    (cairo_set_font_size cro 52.0)
    (cairo_text_extents cro text (pointer-to extents))
    (let ((x (- 250.0 (+ (/ (fh-object-ref extents 'width) 2.0)
                         (fh-object-ref extents 'x_bearing))))
          (y (- 250.0 (+ (/ (fh-object-ref extents 'height) 2.0)
                         (fh-object-ref extents 'y_bearing)))))
      (cairo_move_to cro x y)
      (cairo_show_text cro text)
      (cairo_set_source_rgba cro 1 0.2 0.2 0.6)
      (cairo_set_line_width cro 6.0)
      (cairo_arc cro  x  y  10.0 0 (* 2 M_PI)))
    (cairo_fill cro)
    (cairo_move_to cro 128.0 0)
    (cairo_rel_line_to cro 0 256)
    (cairo_move_to cro 0 128.0)
    (cairo_rel_line_to cro 256 0)
    (cairo_stroke cro)
    cro)
  (cairo_move_to cro 10 10)
  (cairo_rel_line_to cro 80 80)
  (cairo_stroke cro))

(define (paint-it cr)
  (cairo_set_source_rgb cr 0.5 0.5 0.5)
  (cairo_move_to cr 10 10)
  (cairo_rel_line_to cr 80 80)
  (cairo_stroke cr)
  (cairo_paint cr)
  )

(gdk_window_show win)
;;(gdk_keyboard_ungrab)

(let loop ((n 0) (evt (gdk_event_get)))
  (when (< n 100)

    (unless (NULL? (fh-object-ref evt))
      (let* ((type (wrap-GdkEventType (fh-object-ref evt '* 'type))))
        (simple-format #t "type=~S\n" type)
        (case type
          ((GDK_MAP) (paint-it cr)))
        (gdk_event_free evt)))
    
    (usleep 250000)
    (loop (1+ n) (gdk_event_get))))

;; --- last line ---

