;;; examples/nyacc/lang/ffi-help/dbus-02.scm - mainloop example

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

;;; Description:

;; you can do
;;   $ guile dbus-02.scm
;; or
;;   $ guile -l dbus-02.scm
;; to send more

;;; Code:

(add-to-load-path (getcwd))

(use-modules (system dbus))
(use-modules (ffi dbus))
(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))

(define (sf fmt . args) (apply simple-format #t fmt args))

(use-modules (ice-9 pretty-print))
(define pp pretty-print)

(define (send-msg conn msg)
  (let ((pending (make-DBusPendingCall*)))
    (if (eqv? FALSE (dbus_connection_send_with_reply
		     conn msg (pointer-to pending) -1))
	(error "*** send_with_reply FAILED\n"))
    (dbus_message_unref msg)
    pending))

(define (send-sig conn sig)
  (let ((serial (make-uint32)))
    (if (eqv? FALSE (dbus_connection_send
		     conn sig (pointer-to serial)))
	(error "*** send FAILED\n"))
    (dbus_message_unref sig)
    serial))

(define (there-yet? pending)
  (eqv? TRUE (dbus_pending_call_get_completed pending)))

(define (handle-it pending)
  (let ((msg (dbus_pending_call_steal_reply pending))
	(msg-iter (make-DBusMessageIter)))
    (if (zero? (fh-object-ref msg)) (error "*** reply message NULL\n"))
    (dbus_pending_call_unref pending)
    (dbus_message_iter_init msg (pointer-to msg-iter))
    (sf "result:\n")
    (pretty-print (read-dbus-val (pointer-to msg-iter)) #:per-line-prefix "  ")
    (dbus_message_unref msg)))

(define (block-and-handle-it pending)
  (dbus_pending_call_block pending)
  (handle-it pending))

;; ==========================================================================
;; d-feet is GUI to check dictionary
;; https://pythonhosted.org/txdbus/dbus_overview.html
;; http://git.0pointer.net/rtkit.git/tree/README

(define msg02/ses			; works
  (dbus_message_new_method_call
   "org.freedesktop.DBus"		; bus name
   "/org/freedesktop/DBus"		; object path
   "org.freedesktop.DBus.Debug.Stats"	; interface name
   "GetStats"))				; method

(define msg03/all			; works
  (dbus_message_new_method_call
   "org.freedesktop.DBus"		; bus name
   "/org/freedesktop/DBus"		; object path
   "org.freedesktop.DBus"		; interface name
   "GetId"))				; method

(define conn (spawn-dbus-mainloop 'session))

(define pending (send-msg conn msg02/ses))

(let loop ((got-it? (there-yet? pending)))
  (sf "there-yet? => ~S\n" got-it?)
  (cond (got-it? (handle-it pending))
	(else (sleep 1) (loop (there-yet? pending)))))

;; --- last line ---
