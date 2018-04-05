;; dbus05.scm - dbus04 applied to example

(load "dbus04.scm")

(define msg (dbus_message_new_method_call
	     "org.freedesktop.DBus"		; bus name (was NULL)
	     "/org/freedesktop/DBus"		; object path
	     "org.freedesktop.DBus.Debug.Stats"	; interface name
	     "GetStats"))			; method

(define pending (make-DBusPendingCall*))

(or (dbus_connection_send_with_reply conn msg (pointer-to pending) -1)
    (error "*** send_with_reply FAILED\n"))
(if (zero? (fh-object-ref pending)) (display "*** pending NULL\n"))

(dbus_message_unref msg)
(dbus_pending_call_block pending)

(set! msg (dbus_pending_call_steal_reply pending))
(if (zero? (fh-object-ref msg)) (error "*** reply message NULL\n"))
(sf "msg from reply:~S, serial:~S\n" msg (dbus_message_get_serial msg))

(define msg-iter (make-DBusMessageIter))
(dbus_pending_call_unref pending)

(sf "iter_init => ~S\n" (dbus_message_iter_init msg (pointer-to msg-iter)))
(sf "result:\n")
(pretty-print (read-dbus-val (pointer-to msg-iter)) #:per-line-prefix "  ")

(dbus_message_unref msg)
;;(dbus_connection_close conn)

;; --- last line ---
