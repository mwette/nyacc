;; gtkgl-01.scm

;; Copyright (C) 2018 Matthew R. Wette

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (ffi gtk2))
(use-modules (ffi gtkglext1))
(use-modules (ffi glugl))
(use-modules (system ffi-help-rt))
(use-modules (bytestructures guile))

(define TRUE 1)
(define FALSE 0)

;; GL window
;;(define glwin (gdk_gl_window_new win 0))
;;(simple-format #t "glwin=~S\n" win)

(define make-dvec4
  (let ((dvec4-desc (bs:vector 4 double)))
    (lambda (a b c d)
      (bytestructure dvec4-desc (vector a b c d)))))

(define realize
  (let ((light-diffuse (make-dvec4 1.0 0.0 0.0 1.0))
	(light-position (make-dvec4 1.0 1.0 1.0 0.0)))
    (lambda(widget data)
      (let ((glcontext (gtk_widget_get_gl_context widget))
	    (gldrawable (gtk_widget_get_gl_drawable widget))
	    (qobj #f))
	(cond
	 ((zero? (gdk_gl_drawable_gl_begin gldrawable glcontext)))
	 (else
	  (set! qobj (gluNewQuaric))
	  (gluQuadricDrawStyle qobj 'GLU_FILL)
	  (glNewList 1 'GL_COMPILE)
	  (gluSphere qobj 1.0 20 20)
	  (glEndList)

	  (glLightfv 'GL_LIGHT0 'GL_DIFFUSE (pointer-to light-diffuse))
	  (glLightfv 'GL_LIGHT0 'GL_POSITION (pointer-to light-position))
	  (glEnable 'GL_LIGHTING)
	  (glEnable 'GL_LIGHT0)
	  (glEnable 'GL_DEPTH_TEST)

	  (glClearColor 1.0 1.0 1.0 1.0)
	  (glClearDepth 1.0)

	  (glViewport 0 0
		      (fh-object-ref widget 'allocation 'width)
		      (fh-object-ref widget 'allocation 'width))

	  (glMatrixMode 'GL_PROJECTION)
	  (glLoadIdentity)
	  (gluLookAt 0.0 0.0 3.0 0.0 0.0 0.0 0.0 1.0 0.0)
	  (glTranslatef 0.0 0.0 -3.0)

	  (gdk_gl_drawable_gl_end gldrawable)))))))

(define (configure-event widget event data)
  (let ((glcontext (gtk_widget_get_gl_context widget))
	(gldrawable (gtk_widget_get_gl_drawable widget)))
    (cond
     ((not (zero? (gtk_gl_drawable_gl_begin gldrawable glcontext))) FALSE)
     (else
      (glViewport 0 0 
		  (fh-object-ref widget 'allocation 'width)
		  (fh-object-ref widget 'allocation 'width))
      (gdk_gl_drawable_gl_end gldrawable)
      TRUE))))

(define expose-event
  (let* ((m '(GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	 (n (apply logior (map glugl-symval m))))
    (lambda (widget event data)
      (let ((glcontext (gtk_widget_get_gl_context widget))
	    (gldrawable (gtk_widget_get_gl_drawable widget)))
	(cond
	 ((not (zero? (gtk_gl_drawable_gl_begin gldrawable glcontext))) FALSE)
	 (else
	  (glClear n)
	  (glCallList 1)
	  (if (not (zero? (gdk_gl_drawable_is_double_buffered gldrawable)))
	      (gdk_gl_drawable_swap_buffers gldrawable)
	      (glFlush))

	  (gdk_gl_drawable_gl_end gldrawable)
	  TRUE))))))
      
;; Initialize.
(gtk_init NULL NULL)
(gtk_gl_init NULL NULL)

;; double-buffered visual
(define glconfig
  (let* ((m '(GDK_GL_MODE_RGB GDK_GL_MODE_DEPTH GDK_GL_MODE_DOUBLE))
	 (n (apply logior (map gtkgl-symval m))))
    (gdk_gl_config_new_by_mode n)))
(simple-format #t "glconfig=~S\n" glconfig)

(define window (gtk_window_new 'GTK_WINDOW_TOPLEVEL))
(gtk_window_set_title window "simple")

(gtk_container_set_reallocate_redraws window TRUE)

(g_signal_connect window "delete-event" ~gtk_main_quit NULL)
(g_signal_connect window "destroy" ~gtk_main_quit NULL)

(define vbox (gtk_vbox_new FALSE 0))
(gtk_container_add window vbox)
(gtk_widget_show vbox)

(define drawing-area (gtk_drawing_area_new))
(gtk_widget_set_size_request drawing-area 200 200)

(gtk_widget_set_gl_capability drawing-area glconfig NULL TRUE 'GDK_GL_RGBA_TYPE)

(g_signal_connect_after drawing-area "realize" realize NULL)
(g_signal_connect drawing-area "configure_event" configure-event NULL)
(g_signal_connect drawing-area "expose_event" expose-event NULL)

(gtk_box_pack_start vbox drawing-area TRUE TRUE 0)

(gtk_widget_show drawing-area)

(define button (gtk_button_new_with_label "Quit"))

(g_signal_connect button "clicked" ~gtk_main_quit NULL)

(gtk_box_pack_start vbox button FALSE FALSE 0)

(gtk_widget_show button)

(gtk_widget_show window)

(gtk_main)

;; --- last line ---

