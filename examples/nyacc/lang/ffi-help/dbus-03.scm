;; nyacc/lang/ffi-help/dbus-03.scm - peer-to-peer over the session bus

;; Copyright (C) 2018,2020 Matthew R. Wette

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

;;; Notes:

;; In two separate terminals execute:
;; ---------------------------------
;; $ guile dbus-03.scm worker
;; ---------------------------------
;; $ guile dbus-03.scm monitor


;;; Code:

(add-to-load-path (getcwd))

(use-modules (dbus00))
(use-modules (ffi dbus))
(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))

(define (sf fmt . args) (apply simple-format #t fmt args))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))

(define iface "local.Neighbor")
(define iface-pat "interface='local.Neighbor'")

(define workers '())
(define monitors '())

(define (add-role role name)
  (cond
   ((string=? role "worker")
    (unless (member name workers)
      (set! workers (cons name workers))))
   ((string=? role "monitor")
    (unless (member name monitors)
      (set! monitors (cons name monitors))))
   (else (sf "*** unknown role: ~A\n" role))))

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

;;;

(define (send-update conn data)
  #f)

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
    (sf "\nbus=~S\n" (dbus-bus-get-unique-name conn))
    (dbus_bus_add_match conn iface-pat NULL)
    (send-ping conn role)
    (let loop ((clk 0))
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
	    (let ((r-name (dbus-message-get-sender msg))
		  (r-role (list-ref (get-dbus-message-args msg) 0)))
	      (sf "\nping from ~S, a ~S\n" r-name r-role)
	      (add-role r-role r-name)
	      (send-pong conn r-name role)))

	   ((!0 (dbus_message_has_member msg "Pong"))
	    (let ((r-name (dbus-message-get-sender msg))
		  (l-name (dbus-bus-get-unique-name conn))
		  (r-role (list-ref (get-dbus-message-args msg) 0)))
	      (unless (string=? r-name l-name)
		(add-role r-role r-name)
		(rply-pong conn msg))))

	   (else #t))
	  (sf "\n")
	  (sf "  workers : ~S\n" workers)
	  (sf "  monitors: ~S\n" monitors)
	  (dbus_message_unref msg))
	(sleep 1)
	(loop (1+ clk))))
    #t))

(let ((args (cdr (program-arguments))))
  (when (null? args)
    (sferr "usage: guile dbus-03.scm (worker|monitor)\n")
    (error "quitting"))
  (run-peer (car args)))

;; --- last line ---
