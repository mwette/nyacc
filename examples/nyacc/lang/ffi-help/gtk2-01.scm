;; gtk2-01.scm
;;   https://developer.gnome.org/gtk-tutorial/stable/c39.html#SEC-HELLOWORLD

;; Copyright (C) 2018,2024 Matthew Wette
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

(use-modules (nyacc foreign cdata))
(use-modules (ffi glib))
(use-modules (ffi gobject))
(use-modules (ffi gtk2))

(define delete-event
  (make-cdata
   GtkEventCallback
   (lambda (widget event data)
     (display "delete event occurred\n")
     (gtk_main_quit)
     1)))

(define hello
  (make-cdata
   GtkCallback
   (lambda (widget data)
     (display "Hello, World!\n"))))

(define (main)
  (define window #f)
  (define button #f)
  (define argc (make-cdata (cbase 'int) 0))

  (gtk_init (cdata& argc) NULL)

  (set! window (gtk_window_new 'GTK_WINDOW_TOPLEVEL))
  (g_signal_connect window "delete-event" delete-event NULL)
  (gtk_container_set_border_width window 10)

  (set! button (gtk_button_new_with_label "Hello World"))
  (g_signal_connect button "clicked" (ccast GCallback hello) NULL)
  (gtk_container_add window button)

  (gtk_widget_show button)
  (gtk_widget_show window)

  (gtk_main))

(main)

;; --- last line ---
