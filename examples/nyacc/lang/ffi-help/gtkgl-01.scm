;; gtkgl-01.scm - works!
;;   http://www.ccp4.ac.uk/dist/checkout/gtkglext-1.2.0/examples/simple.c

;; Copyright (C) 2018 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

(use-modules (ffi gobject))
(use-modules (ffi gtk2))
(use-modules (ffi gtkglext1))
(use-modules (ffi glugl))
(use-modules (system ffi-help-rt))
(use-modules (bytestructures guile))

(define (sf fmt . args) (apply simple-format #t fmt args))

(define TRUE 1)
(define FALSE 0)

(display "Hey, why not add a user-defined unwrapper for GLenum.\n")

(define make-dvec4
  (let ((dvec4-desc (bs:vector 4 double)))
    (lambda (a b c d)
      (bytestructure dvec4-desc (vector a b c d)))))

(define realize
  (let ((light-diffuse (make-dvec4 1.0 0.0 0.0 1.0))
	(light-position (make-dvec4 1.0 1.0 1.0 0.0)))
    (make-GtkCallback
     (lambda(widget data)
       (let* ((widget (make-GtkWidget* widget))
	      (glcontext (gtk_widget_get_gl_context widget))
	      (gldrawable (gtk_widget_get_gl_drawable widget))
	      (qobj #f))
	 (unless (zero? (gdk_gl_drawable_gl_begin gldrawable glcontext))
	   (set! qobj (gluNewQuadric))
	   (gluQuadricDrawStyle qobj (glugl-symval 'GLU_FILL))
	   (glNewList 1 (glugl-symval 'GL_COMPILE))
	   (gluSphere qobj 1.0 20 20)
	   (glEndList)

	   (glLightfv (glugl-symval 'GL_LIGHT0) (glugl-symval 'GL_DIFFUSE)
		      (pointer-to light-diffuse))
	   (glLightfv (glugl-symval 'GL_LIGHT0) (glugl-symval 'GL_POSITION)
		      (pointer-to light-position))
	   (glEnable (glugl-symval 'GL_LIGHTING))
	   (glEnable (glugl-symval 'GL_LIGHT0))
	   (glEnable (glugl-symval 'GL_DEPTH_TEST))

	   (glClearColor 1.0 1.0 1.0 1.0)
	   (glClearDepth 1.0)

	   (glViewport 0 0
		       (fh-object-ref widget 'allocation 'width)
		       (fh-object-ref widget 'allocation 'height))

	   (glMatrixMode (glugl-symval 'GL_PROJECTION))
	   (glLoadIdentity)
	   (gluPerspective 40.0 1.0 1.0 10.0)

	   (glMatrixMode (glugl-symval 'GL_MODELVIEW))
	   (glLoadIdentity)
	   (gluLookAt 0.0 0.0 3.0
		      0.0 0.0 0.0
		      0.0 1.0 0.0)
	   (glTranslatef 0.0 0.0 -3.0)

	   (gdk_gl_drawable_gl_end gldrawable)))))))

(define configure-event
  (make-GtkEventCallback
   (lambda (widget event data)
     (let ((widget (make-GtkWidget* widget))
	   (glcontext (gtk_widget_get_gl_context widget))
	   (gldrawable (gtk_widget_get_gl_drawable widget)))
       (cond
	((zero? (gdk_gl_drawable_gl_begin gldrawable glcontext))
	 FALSE)
	(else
	 (glViewport 0 0 
		     (fh-object-ref widget 'allocation 'width)
		     (fh-object-ref widget 'allocation 'height))
	 (gdk_gl_drawable_gl_end gldrawable)
	 TRUE))))))

(define expose-event
  (let* ((m '(GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	 (n (apply logior (map glugl-symval m))))
    (make-GtkEventCallback
     (lambda (widget event data)
       (let ((glcontext (gtk_widget_get_gl_context widget))
	     (gldrawable (gtk_widget_get_gl_drawable widget)))
	 (cond
	  ((zero? (gdk_gl_drawable_gl_begin gldrawable glcontext))
	   FALSE)
	  (else
	   (glClear n)
	   (glCallList 1)
	   (if (!0 (gdk_gl_drawable_is_double_buffered gldrawable))
	       (gdk_gl_drawable_swap_buffers gldrawable)
	       (glFlush))
	   (gdk_gl_drawable_gl_end gldrawable)
	   TRUE)))))))

(define (print-gl-config-attrib glconfig attrib is_boolean)
  (let ((attrib_str (symbol->string attrib))
	(attr (gtkgl-symval attrib))
	(value (make-int)))
    (sf "~A = " attrib_str)
    (if (!0 (gdk_gl_config_get_attrib glconfig attr (pointer-to value)))
	(if (!0 is_boolean)
	    (sf "~A\n" (if (eqv? (fh-object-ref value) TRUE) "TRUE" "FALSE"))
	    (sf "~A\n" (fh-object-ref value)))
	(sf "*** cannot get attributes\n"))))

(define (examine_gl_config_attrib glconfig)
  (define (tf x) (if (zero? x) "FALSE" "TRUE"))
  (sf "\nOpenGL visual configurations :\n\n")
  (sf "gdk_gl_config_is_rgba (glconfig) = ~A\n"
      (tf (gdk_gl_config_is_rgba glconfig)))
  (sf "gdk_gl_config_is_double_buffered (glconfig) = ~A\n"
      (tf (gdk_gl_config_is_double_buffered glconfig)))
  (sf "gdk_gl_config_is_stereo (glconfig) = ~A\n"
      (tf (gdk_gl_config_is_stereo glconfig)))
  (sf "gdk_gl_config_has_alpha (glconfig) = ~A\n"
      (tf (gdk_gl_config_has_alpha glconfig)))
  (sf "gdk_gl_config_has_depth_buffer (glconfig) = ~A\n"
      (tf (gdk_gl_config_has_depth_buffer glconfig)))
  (sf "gdk_gl_config_has_stencil_buffer (glconfig) = ~A\n"
      (tf (gdk_gl_config_has_stencil_buffer glconfig)))
  (sf "gdk_gl_config_has_accum_buffer (glconfig) = ~A\n"
      (tf (gdk_gl_config_has_accum_buffer glconfig)))
  (sf "\n")
  (print-gl-config-attrib glconfig 'GDK_GL_USE_GL 1)
  (print-gl-config-attrib glconfig 'GDK_GL_BUFFER_SIZE 0)
  (print-gl-config-attrib glconfig 'GDK_GL_LEVEL 0)
  (print-gl-config-attrib glconfig 'GDK_GL_RGBA 1)
  (print-gl-config-attrib glconfig 'GDK_GL_DOUBLEBUFFER 1)
  (print-gl-config-attrib glconfig 'GDK_GL_STEREO 1)
  (print-gl-config-attrib glconfig 'GDK_GL_AUX_BUFFERS 0)
  (print-gl-config-attrib glconfig 'GDK_GL_RED_SIZE 0)
  (print-gl-config-attrib glconfig 'GDK_GL_GREEN_SIZE 0)
  (print-gl-config-attrib glconfig 'GDK_GL_BLUE_SIZE 0)
  (print-gl-config-attrib glconfig 'GDK_GL_ALPHA_SIZE 0)
  (print-gl-config-attrib glconfig 'GDK_GL_DEPTH_SIZE 0)
  (print-gl-config-attrib glconfig 'GDK_GL_STENCIL_SIZE 0)
  (print-gl-config-attrib glconfig 'GDK_GL_ACCUM_RED_SIZE 0)
  (print-gl-config-attrib glconfig 'GDK_GL_ACCUM_GREEN_SIZE 0)
  (print-gl-config-attrib glconfig 'GDK_GL_ACCUM_BLUE_SIZE 0)
  (print-gl-config-attrib glconfig 'GDK_GL_ACCUM_ALPHA_SIZE 0)
  )
  
;; Initialize.
(define argc (make-int 0))
(gtk_init (pointer-to argc) NULL)
(gtk_gl_init (pointer-to argc) NULL)

(let ((major (make-int)) (minor (make-int)))
  (gdk_gl_query_version (pointer-to major) (pointer-to minor))
  (sf "\nOpenGL extension version - ~A.~A\n"
      (fh-object-ref major) (fh-object-ref minor)))

;; double-buffered visual
(define glconfig
  (let* ((m '(GDK_GL_MODE_RGB GDK_GL_MODE_DEPTH GDK_GL_MODE_DOUBLE))
	 (n (apply logior (map gtkgl-symval m))))
    (gdk_gl_config_new_by_mode n)))

(examine_gl_config_attrib glconfig)

(define window (gtk_window_new 'GTK_WINDOW_TOPLEVEL))
(gtk_window_set_title window "simple")

(gtk_container_set_reallocate_redraws window TRUE)

(g_signal_connect window "delete_event" ~gtk_main_quit NULL)

(define vbox (gtk_vbox_new FALSE 0))
(gtk_container_add window vbox)
(gtk_widget_show vbox)

(define drawing-area (gtk_drawing_area_new))
(gtk_widget_set_size_request drawing-area 200 200)

(gtk_widget_set_gl_capability drawing-area glconfig NULL TRUE
			      (gtkgl-symval 'GDK_GL_RGBA_TYPE))

(g_signal_connect_after drawing-area "realize" (fh-cast GCallback realize) NULL)
(g_signal_connect drawing-area
		  "configure_event" (fh-cast GCallback configure-event) NULL)
(g_signal_connect drawing-area
		  "expose_event" (fh-cast GCallback expose-event) NULL)

(gtk_box_pack_start vbox drawing-area TRUE TRUE 0)

(gtk_widget_show drawing-area)

(define button (gtk_button_new_with_label "Quit"))

(g_signal_connect button "clicked" ~gtk_main_quit NULL)

(gtk_box_pack_start vbox button FALSE FALSE 0)

(gtk_widget_show button)

(gtk_widget_show window)

(gtk_main)

;; --- last line ---

