;; dbus-05.scm - example for mainloop

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

(add-to-load-path (getcwd))

(use-modules (dbus00))
(use-modules (dbusML))
(use-modules (ffi dbus))
(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (ice-9 pretty-print))

(define (sf fmt . args) (apply simple-format #t fmt args))
(define pp pretty-print)

(define (send-msg conn msg)
  (let ((pending (make-DBusPendingCall*)))
    (if (eqv? FALSE (dbus_connection_send_with_reply
		     conn msg (pointer-to pending) -1))
	(error "*** send_with_reply FAILED\n"))
    (dbus_message_unref msg)
    pending))

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

(define conn (spawn-dbus-mainloop 'session))
;;(define conn (spawn-dbus-mainloop 'system))

(define msg01 (dbus_message_new_method_call
	     "org.freedesktop.DBus.Peer" ; bus name
	     "/org/freedesktop/DBus/any" ; object path
	     "org.freedesktop.DBus.Peer" ; interface name
	     "Ping"))			 ; method

(define msg04 (dbus_message_new_method_call
	      "org.freedesktop.DBus"		; bus name
	      "/org/freedesktop/DBus"		; object path
	      "org.freedesktop.DBus.Debug.Stats"	; interface name
	      "GetStats"))			; method

(define msg05 (dbus_message_new_method_call
	     "org.freedesktop.DBus"		; bus name
	     "/org/freedesktop/DBus"		; object path
	     "org.freedesktop.DBus"		; interface name
	     "GetId"))				; method

(define pending (send-msg conn msg04))

(let iter ((got-it? (there-yet? pending)))
  (sf "there-yet? => ~S\n" got-it?)
  (cond
   (got-it? (handle-it pending))
   (else
    (sleep 1)
    (iter (there-yet? pending)))))

;; --- last line ---
