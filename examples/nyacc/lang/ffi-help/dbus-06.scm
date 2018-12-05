;; dbus06.scm - server / client
;;   DTD: https://dbus.freedesktop.org/doc/busconfig.dtd
;; https://dbus.freedesktop.org/doc/api/html/group__DBus.html

;; https://stackoverflow.com/questions/37099254/simple-client-server-using-dbus

;; iter_init_append, iter_append_basic, ...

(add-to-load-path (getcwd))

(use-modules (dbus00))
(use-modules (dbusML))
(use-modules (ffi dbus))
(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (sxml simple))
(use-modules (ice-9 pretty-print))

(define (sf fmt . args) (apply simple-format #t fmt args))
(define pp pretty-print)

(define p2s ffi:pointer->string)
(define s2p ffi:string->pointer)

;; -- reuseable

(define (send-msg conn msg)
  (let ((pending (make-DBusPendingCall*)))
    (if (zero? (dbus_connection_send_with_reply
		conn msg (pointer-to pending) -1))
	(error "*** send FAILED\n"))
    (dbus_message_unref msg)
    pending))

(define (send-sig conn sig)
  (let ((serial (make-uint32)))
    (if (eqv? FALSE (dbus_connection_send
		     conn sig (pointer-to serial)))
	(error "*** send FAILED\n"))
    (dbus_message_unref sig)
    serial))

;; -------------------

(define bus-name "local.mwette")
;;(define obj-path "/local/mwette/demo1")
(define obj-path "/")
(define if-name "local.mwette.Basic")

(define mwette-demo1-sxml
  `(*TOP*
    (node
     (@ (name "/"))
     (interface
      (@ (name ,if-name))
      (signal (@ (name "Ping")) (arg (@ (name "time") (type "i")))))
     )))

(define mwette-demo1-xml
  (call-with-output-string
    (lambda (port) (sxml->xml mwette-demo1-sxml port))))

(define (reply-to-intro conn msg)
  (sf "calling (reply-to-intro ~S ~S) ...\n" conn msg)
  (let* ((rpl (dbus_message_new_method_return msg))
	 (iter (make-DBusMessageIter))
	 (&iter (pointer-to iter))
	 (serial (make-int32))
	 (&str (make-dbus-string mwette-demo1-xml)))
    (dbus_message_iter_init_append rpl &iter)
    (dbus_message_iter_append_basic &iter (DBUS 'TYPE_STRING) &str)
    (dbus_connection_send conn rpl (pointer-to serial))
    serial))

;; ====================

(define conn #f)

(define (show msg)
  (sf "\ngot message ~S\n" msg)
  (sf "  path   = ~S\n" (p2s (dbus_message_get_path msg)))
  (sf "  type   = ~S\n" (dbus-message-type (dbus_message_get_type msg)))
  (sf "  iface  = ~S\n" (p2s (dbus_message_get_interface msg)))
  (sf "  member = ~S\n" (p2s (dbus_message_get_member msg))))

(define (run-server)
  (newline)
  (set! conn (dbus_bus_get 'DBUS_BUS_SESSION NULL))
  ;;(sf "conn=~S\n" conn)
  (let* ((flags (apply logior
		       (map dbus-symval
			    '(
			      ;; DBUS_NAME_FLAG_ALLOW_REPLACEMENT
			      ;; DBUS_NAME_FLAG_DO_NOT_QUEUE
			      ))))
	 )
    (let* ((rpl (dbus_bus_request_name conn bus-name flags NULL)))
      (sf "request ~S => ~S\n" bus-name (dbus-request-name-reply rpl))
      (case (dbus-request-name-reply rpl)
	((PRIMARY_OWNER) #t)
	((IN_QUEUE) (error "IN_QUEUE"))
	((EXISTS) (error "EXISTS"))
	((ALREADY_OWNER) #t)))
    (let* ((error (make-DBusError)) (&error (pointer-to error)))
      (dbus_error_init &error)
      (dbus_bus_add_match conn
			  "type='signal',interface='local.mwette.Basic'"
			  &error)
      (unless (zero? (dbus_error_is_set &error))
	(sf "error\n")))
    (let loop ()
      (dbus_connection_read_write conn 0)
      (let ((msg (dbus_connection_pop_message conn)))
	(when (!0 (fh-object-ref msg))
	  (show msg)
	  (cond
	   ((!0 (dbus_message_has_interface
		 msg "org.freedesktop.DBus.Introspectable"))
	    (reply-to-intro conn msg))
	   )
	  (dbus_message_unref msg))
	(sleep 1)
	(loop)))
    0))

(define (run-client)
  (set! conn (dbus_bus_get 'DBUS_BUS_SESSION NULL))
  (sf "\nconn: ~S\n" (ffi:pointer->string (dbus_bus_get_unique_name conn)))
  (let* ((sig01 (dbus_message_new_signal obj-path if-name "Ping"))
	 (iter (make-DBusMessageIter))
	 (&iter (pointer-to iter))
	 (tval (make-int32 99)))

    (dbus_message_iter_init_append sig01 &iter)
    (dbus_message_iter_append_basic &iter (DBUS 'TYPE_INT32) (pointer-to tval))

    (let loop ()
      (dbus_connection_read_write conn 0)
      (let ((msg (dbus_connection_pop_message conn)))
	(unless (zero? (fh-object-ref msg))
	  (show msg)
	  (cond
	   ((string=? (p2s (dbus_message_get_member msg)) "NameAcquired")
	    (let ((serial (make-uint32)) (sig sig01))
	      (sf "sending ...\n")
	      (if (zero? (dbus_connection_send conn sig (pointer-to serial)))
		  (error "*** send FAILED\n"))
	      )))
	  (dbus_message_unref msg))
	(sleep 1)
	(loop)))
    0))

  
;; -------------------

(use-modules (srfi srfi-37))

(define options
  (list
   (option '(#\S "server") #f #f
	   (lambda (opt name arg seed) (acons 'server #t seed)))
   (option '(#\C "client") #f #f
	   (lambda (opt name arg seed) (acons 'client #t seed)))
   ))

(define (parse-args args)
  (args-fold args options
	     (lambda (opt name arg seed) (error "bad option arg"))
	     (lambda (arg seed) (error "bad file arg" arg))
	     '()))

(define (main . args)
  (let* ((opts (parse-args args))
	 (server? (assq-ref opts 'server)))
    (cond
     ((assq-ref opts 'server) (run-server))
     ((assq-ref opts 'client) (run-client))
     (else (error "expecting server or client")))))

(apply main (cdr (program-arguments)))

;; --- last line ---
