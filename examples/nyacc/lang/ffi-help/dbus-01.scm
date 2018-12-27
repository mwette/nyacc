;; dbus-01.scm - dbus
;; see http://www.matthew.ath.cx/misc/dbus

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

(use-modules (system dbus))
(use-modules (ffi dbus))
(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (bytestructures guile))

(define (sf fmt . args) (apply simple-format #t fmt args))
(use-modules (ice-9 pretty-print))
(define pp pretty-print)

(define (check-error error)
  (let ((err (dbus-error error)))
    (if err (sf "~A\n" err))))
  
;; ====================================

(define error (make-DBusError))
(dbus_error_init (pointer-to error))

(define conn (dbus_bus_get 'DBUS_BUS_SESSION (pointer-to error)))
(check-error error)
(sf "conn: ~S = ~S\n" conn (ffi:pointer->string (dbus_bus_get_unique_name conn)))

(define msg (dbus_message_new_method_call
	     "org.freedesktop.DBus"		; bus name (was NULL)
	     "/org/freedesktop/DBus"		; object path
	     "org.freedesktop.DBus.Debug.Stats"	; interface name
	     "GetStats"))			; method

(define pending (make-DBusPendingCall*))
(or (dbus_connection_send_with_reply conn msg (pointer-to pending) -1)
    (error "*** send_with_reply FAILED\n"))
(if (zero? (fh-object-ref pending)) (display "*** pending NULL\n"))

(dbus_connection_flush conn)
(dbus_message_unref msg)
(dbus_pending_call_block pending)

(set! msg (dbus_pending_call_steal_reply pending))
(if (zero? (fh-object-ref msg)) (error "*** reply message NULL\n"))
(sf "msg reply: ~S, serial: ~S, type: ~A\n" msg (dbus_message_get_serial msg)
    (let ((msg-type (dbus_message_get_type msg)))
      (cond
       ((eq? (DBUS 'MESSAGE_TYPE_INVALID) msg-type) "invalid")
       ((eq? (DBUS 'MESSAGE_TYPE_METHOD_CALL) msg-type) "method call")
       ((eq? (DBUS 'MESSAGE_TYPE_METHOD_RETURN) msg-type) "method return")
       ((eq? (DBUS 'MESSAGE_TYPE_ERROR) msg-type) "error")
       ((eq? (DBUS 'MESSAGE_TYPE_SIGNAL) msg-type) "signal"))))

(define &iter (make-DBusMessageIter&))
(dbus_pending_call_unref pending)

(sf "iter_init => ~S\n" (dbus_message_iter_init msg &iter))
(sf "result:\n")
(pp (read-dbus-val &iter) #:per-line-prefix "  ")

(dbus_message_unref msg)

;; --- last line ---
