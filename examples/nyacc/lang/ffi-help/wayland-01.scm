;; waylang-01.scm
;;   https://wayland-book.com/wayland-display/creation.html

;; Copyright (C) 2022 Matthew R. Wette
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

(define (sf fmt . args) (apply simple-format #t fmt args))

(use-modules (system ffi-help-rt))
(use-modules (bytestructures guile))

(use-modules (ffi wayland-client))
;;(use-modules (ffi wayland-server))

(define socket
  (let ((dir (getenv "XDG_RUNTIME_DIR"))
	(dpy (getenv "WAYLAND_DISPLAY")))
    (and dir dpy (string-append dir "/" dpy))))

(define (main)
  (let* ((display (wl_display_connect NULL))
	 )
    (sf "display = ~S\n" display)
    (sf "socket = ~S\n" socket)


    (wl_display_disconnect display)))

(main)

;; --- last line ---
