;; gtk2-01.scm
;;   https://developer.gnome.org/gtk-tutorial/stable/c39.html#SEC-HELLOWORLD

;; Copyright (C) 2018 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or modify it under
;; the terms of the GNU Lesser General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option) any
;; later version.
;;
;; This library is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
;; details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

(use-modules (system ffi-help-rt))
(use-modules (bytestructures guile))

(use-modules (ffi glib))		; ffi:31 scm:31889
(use-modules (ffi gobject))		; ffi:26 scm:12044
(use-modules (ffi gtk2))		; ffi:26 scm:92964

;; This will generate a FFI code wrapper around the lambda.  Then below
;; we use (fh-cast GCallback hello) to match the argument signature.
(define hello
  (make-GtkCallback
   (lambda (widget data)
     (display "Hello world!\n"))))

(define (delete-event widget event data)
  (display "delete event occurred\n")
  1)

(define (main)
  (define window #f)
  (define button #f)
  (define argc (bytestructure int 0))

  (gtk_init (pointer-to argc) NULL)

  (set! window (gtk_window_new 'GTK_WINDOW_TOPLEVEL))
  (g_signal_connect window "delete-event" delete-event NULL)
  (g_signal_connect window "destroy" ~gtk_main_quit NULL)
  (gtk_container_set_border_width window 10)

  (set! button (gtk_button_new_with_label "Hello World"))
  (g_signal_connect button "clicked" (fh-cast GCallback hello) NULL)
  (g_signal_connect_swapped button "clicked" ~gtk_widget_destroy window)
  (gtk_container_add window button)

  (gtk_widget_show button)
  (gtk_widget_show window)

  (gtk_main))

(main)

;; --- last line ---
