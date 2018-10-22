;; pangocairo demo - this works
;; https://developer.gnome.org/pango/stable/pango-Cairo-Rendering.html

(use-modules (ffi glib))
(use-modules (ffi gobject))
(use-modules (ffi cairo))
;;(use-modules (ffi pango))
(use-modules (ffi pangocairo))
(use-modules (system ffi-help-rt))

(define PANGO_SCALE (ffi-pangocairo-symbol-val 'PANGO_SCALE))

(define RADIUS 150)
(define N_WORDS 10)
(define FONT "Sans Bold 27")

(define (draw-text cr)
  (define layout #f)
  (define desc #f)
  
  (cairo_translate cr RADIUS RADIUS)

  (set! layout (pango_cairo_create_layout cr))

  (pango_layout_set_text layout "Text" -1)
  (set! desc (pango_font_description_from_string FONT))
  (pango_layout_set_font_description layout desc)
  (pango_font_description_free desc)

  (do ((i 0 (1+ i))) ((= i N_WORDS))
    (let ((width (make-int))
	  (height (make-int))
	  (angle (/ (* 360.0 i) N_WORDS))
	  (red 0.0))
      (cairo_save cr)
      (set! red (/ (1+ (cos (/ (* (- angle 60.0) G_PI) 180.0))) 2.0))
      (cairo_set_source_rgb cr red 0.0 (- 1.0 red))

      (cairo_rotate cr (/ (* angle G_PI) 180.0))

      (pango_cairo_update_layout cr layout)

      (pango_layout_get_size layout (pointer-to width) (pointer-to height))
      (cairo_move_to cr (- (/ (fh-object-ref width) PANGO_SCALE 2.0)) (- RADIUS))
      (pango_cairo_show_layout cr layout)
      (cairo_restore cr)))

  (g_object_unref layout))

(define (main argv)
  (define filename #f)
  (define surface #f)
  (define cr #f)
  (define status #f)
  
  (unless (= 2 (length argv))
    (display "usage: cairopango FILENAME\n")
    (quit))
  
  (set! filename (list-ref argv 1))

  (set! surface	(cairo_image_surface_create 'CAIRO_FORMAT_ARGB32
					    (* 2 RADIUS) (* 2 RADIUS)))
  (set! cr (cairo_create surface))

  (cairo_set_source_rgb cr 1.0 1.0 1.0)
  (cairo_paint cr)
  (draw-text cr)
  (cairo_destroy cr)

  (set! status (cairo_surface_write_to_png surface filename))
  (cairo_surface_destroy surface)

  (unless (eq? status 'CAIRO_STATUS_SUCCESS)
    (simple-format #t "Could not save png to ~S.\n" filename)))

(let ((args (program-arguments)))
  (main args))

;; --- last line ---
