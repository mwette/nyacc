#!/opt/local/bin/guile
!#

;; https://developer.gnome.org/gtk-tutorial/stable/c39.html#SEC-HELLOWORLD

(use-modules (ffi glib))
(use-modules (ffi gobject))
(use-modules (ffi gtk2+))
(use-modules (bytestructures guile))
(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))

(define NULL ffi:%null-pointer)

(define (hello widget data)
  ;;(g_print "Hello World!\n")
  (display "Hello World!\n")
  )

(define (delete-event widget event data)
  (display "delete event occurred\n")
  1)

(define (destroy widget data)
  (gtk_main_quit))

(define (main)
  (define window #f)
  (define button #f)
  (define argc (bytestructure int 0))

  (gtk_init (pointer-to argc) NULL)

  (set! window (gtk_window_new 'GTK_WINDOW_TOPLEVEL))
  (g_signal_connect window "delete-event" delete-event NULL)
  (g_signal_connect window "destroy" destroy NULL)
  (gtk_container_set_border_width window 10)

  (set! button (gtk_button_new_with_label "Hello World"))
  (g_signal_connect button "clicked" hello NULL)
  (g_signal_connect_swapped button "clicked" gtk_widget_destroy window)
  (gtk_container_add window button)

  (gtk_widget_show button)
  (gtk_widget_show window)

  (gtk_main))

(main)
;; --- last line ---
