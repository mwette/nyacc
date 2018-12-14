;; nyacc/lang/ffi-help/dbus-04.scm - simple square

;; Copyright (C) 2018 Matthew R. Wette

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

;;; Notes:

;; In two separate terminals execute:
;; ---------------------------------
;; $ guile dbus-04.scm worker
;; ---------------------------------
;; $ guile dbus-04.scm manager


;;; Code:

(add-to-load-path (getcwd))

(use-modules (dbus00))
(use-modules (ffi dbus))
(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))

(define (sf fmt . args) (apply simple-format #t fmt args))

(define iface "local.Neighbor")
(define iface-pat "interface='local.Neighbor'")

(define (send-ping conn role)
  (let* ((&role (pointer-to (make-char* role)))
	 (sig (dbus_message_new_signal "/" iface "Ping"))
	 (&iter (make-DBusMessageIter&))
	 (serial (make-uint32)))
    (sf "\nsending ping with ~S\n" role)
    (dbus_message_iter_init_append sig &iter)
    (dbus_message_iter_append_basic &iter (DBUS 'TYPE_STRING) &role)
    (dbus_connection_send conn sig (pointer-to serial))
    serial))
    
(define (send-pong conn rem role)
  (let ((&role (pointer-to (make-char* role)))
	(loc (make-char* (dbus_bus_get_unique_name conn)))
	(rpl (dbus_message_new_method_call rem "/" iface "Pong"))
	(&iter (make-DBusMessageIter&))
	(serial (make-uint32)))
    (sf "\nsending pong to ~S with ~S\n" rem role)
    (dbus_message_iter_init_append rpl &iter)
    (dbus_message_iter_append_basic &iter (DBUS 'TYPE_STRING) &role)
    (dbus_connection_send conn rpl (pointer-to serial))
    serial))

(define (rply-pong conn msg)
  (let ((rpl (dbus_message_new_method_return msg))
	(&iter (make-DBusMessageIter&))
	(bval (make-uint32 (dbus-symval 'TRUE)))
	(serial (make-uint32)))
    (dbus_message_iter_init msg &iter)
    (sf "\nreply OK to pong from ~S, a ~S\n"
	(dbus-message-get-sender msg)
	(car (get-dbus-message-args msg)))
    (dbus_message_iter_init_append rpl &iter)
    (dbus_message_iter_append_basic &iter (DBUS 'TYPE_BOOLEAN) (pointer-to bval))
    (dbus_connection_send conn rpl (pointer-to serial))
    serial))

(define conn #f)

(define (p2s p)
  (if (zero? (ffi:pointer-address p)) "" (ffi:pointer->string p)))

(define (show msg)
  (sf "\n  got message ~S\n" msg)
  (sf "    type   = ~S\n" (dbus-message-type (dbus_message_get_type msg)))
  (sf "    path   = ~S\n" (p2s (dbus_message_get_path msg)))
  (sf "    iface  = ~S\n" (p2s (dbus_message_get_interface msg)))
  (sf "    member = ~S\n" (p2s (dbus_message_get_member msg))))

;; worker listens for ping, sends pong
(define (run-peer role)
  (let ((error (make-DBusError)))
    (set! conn (dbus_bus_get 'DBUS_BUS_SESSION NULL))
    (sf "bus=~S\n" (dbus-bus-get-unique-name conn))
    (dbus_bus_add_match conn iface-pat NULL)
    (send-ping conn role)
    (let loop ()
      (dbus_connection_read_write conn 0)
      (let ((msg (dbus_connection_pop_message conn)))
	(unless (zero? (fh-object-ref msg))
	  (show msg)
	  (cond

	   ((= (dbus_message_get_type msg) (DBUS 'MESSAGE_TYPE_ERROR))
	    (dbus_set_error_from_message (pointer-to error) msg)
	    (sf "error: ~S\n"
		(ffi:pointer->string
		 (ffi:make-pointer
		  (fh-object-ref error 'message))))
	    #f)

	   ((and (!0 (dbus_message_has_member msg "Ping"))
		 (not (string=? (dbus-bus-get-unique-name conn)
				(dbus-message-get-sender msg))))
	    (sf "\nping from ~S, a ~S\n"
		(dbus-message-get-sender msg)
		(car (get-dbus-message-args msg)))
	    (send-pong conn (dbus-message-get-sender msg) role))

	   ((!0 (dbus_message_has_member msg "Pong"))
	    (rply-pong conn msg))

	   (else #t))
	  (dbus_message_unref msg))
	(sleep 1)
	(loop)))
    #t))

(let ((args (cdr (program-arguments))))
  (if (null? args)
      (run-peer "worker")
      (run-peer (car args))))

;; --- last line ---
