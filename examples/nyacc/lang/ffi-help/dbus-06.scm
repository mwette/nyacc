;; dbus06.scm - server
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

(define dbus-message-type
  (if (and
       (= 0 (dbus-symval 'DBUS_MESSAGE_TYPE_INVALID))
       (= 1 (dbus-symval 'DBUS_MESSAGE_TYPE_METHOD_CALL))
       (= 2 (dbus-symval 'DBUS_MESSAGE_TYPE_METHOD_RETURN))
       (= 3 (dbus-symval 'DBUS_MESSAGE_TYPE_ERROR))
       (= 4 (dbus-symval 'DBUS_MESSAGE_TYPE_SIGNAL)))
      (lambda (ival)
	(case ival
	  ((0) 'INVALID)
	  ((1) 'METHOD_CALL)
	  ((2) 'METHOD_RETURN)
	  ((3) 'ERROR)
	  ((4) 'SIGNAL)
	  (else #f)))
      (lambda (ival) ival)))

(define dbus-request-name-reply
  (if (and
       (= 1 (dbus-symval 'DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER))
       (= 2 (dbus-symval 'DBUS_REQUEST_NAME_REPLY_IN_QUEUE))
       (= 3 (dbus-symval 'DBUS_REQUEST_NAME_REPLY_EXISTS))
       (= 4 (dbus-symval 'DBUS_REQUEST_NAME_REPLY_ALREADY_OWNER)))
      (lambda (ival)
	(case ival
	  ((1) 'PRIMARY_OWNER)
	  ((2) 'IN_QUEUE)
	  ((3) 'REPLY_EXISTS)
	  ((4) 'ALREADY_OWNER)
	  (else #f)))
      (lambda (ival) ival)))

(use-modules ((ice-9 iconv) #:select (string->bytevector)))
(use-modules (rnrs bytevectors))

;; returns a marshalled pointer -- DONT USE
(define (make-dbus-string str)
  (let* ((sbv (string->bytevector str "utf-8"))
	 (len (bytevector-length sbv))
	 (dbus-bv (make-bytevector (+ 5 len))))
    (bytevector-u32-native-set! dbus-bv 0 len)
    (bytevector-copy! sbv 0 dbus-bv 4 len)
    (bytevector-u8-set! dbus-bv (+ 4 len) 0) ; trailing \nul
    (sf "bv=~S\n" dbus-bv)
    (ffi:bytevector->pointer dbus-bv)))

;; -------------------

(define mwette-demo1-sxml
  '(*TOP*
    #;(*PI* !DOCTYPE
	  "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN"
	  "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd")
    (node
     (@ (name "/local/mwette/demo1"))
     (interface
      (@ (name "local.mwette.Heartbeat"))
      (signal (@ (name "ping")) (arg (@ (name "time") (type "i")))))
     #|
     <node name="/com/example/sample_object0">
     <interface name="com.example.SampleInterface0">
     <method name="Frobate">
     <arg name="foo" type="i" direction="in"/>
     <arg name="bar" type="s" direction="out"/>
     <arg name="baz" type="a{us}" direction="out"/>
     <annotation name="org.freedesktop.DBus.Deprecated" value="true"/>
     </method>
     <method name="Bazify">
     <arg name="bar" type="(iiu)" direction="in"/>
     <arg name="bar" type="v" direction="out"/>
     </method>
     <method name="Mogrify">
     <arg name="bar" type="(iiav)" direction="in"/>
     </method>
     <signal name="Changed">
     <arg name="new_value" type="b"/>
     </signal>
     <property name="Bar" type="y" access="readwrite"/>
     </interface>
     <node name="child_of_sample_object"/>
     <node name="another_child_of_sample_object"/>
     |#
     )))


(define mwette-demo1-xml
  (call-with-output-string
    (lambda (port) (sxml->xml mwette-demo1-sxml port))))

(define printf
  (ffi:pointer->procedure
   ffi:int (dynamic-func "printf" (dynamic-link)) (list '*)))

(define (reply-to-intro conn msg)
  (sf "calling (reply-to-intro ~S ~S) ...\n" conn msg)
  (let* ((rpl (dbus_message_new_method_return msg))
	 (iter (make-DBusMessageIter))
	 (&iter (pointer-to iter))
	 (serial (make-int32))
	 (str mwette-demo1-xml)
	 (strp (ffi:string->pointer str))
	 (strpv (ffi:pointer-address strp))
	 (sizeof* (ffi:sizeof '*))
	 (strpbv (let ((bv (make-bytevector sizeof*)))
		   (case sizeof*
		     ((8) (bytevector-u64-native-set! bv 0 strpv))
		     ((4) (bytevector-u32-native-set! bv 0 strpv))
		     (else (error "bad pointer size")))
		   bv))
	 (str* (ffi:bytevector->pointer strpbv))
	 )
    ;;(sf "  xml: ~S\n" mwette-demo1-xml)
    ;;(sf " ~S\n" (unwrap~pointer (s2p mwette-demo1-xml)))
    ;;(sf " &ter=~S\n" &iter)
    ;;(printf (s2p mwette-demo1-xml)) (printf (s2p "\n"))
    ;;(pp mwette-demo-sxml)
    (dbus_message_iter_init_append rpl &iter)
    (dbus_message_iter_append_basic &iter (DBUS 'TYPE_STRING) str*)
    (dbus_connection_send conn rpl (pointer-to serial))
    serial))

(define conn #f)

(define bus-name "local.mwette")
(define obj-path "/local/mwette/demo1")
(define if-name "local.mwette.Heartbeat")

(define (handle-it pending)
  (let ((msg (dbus_pending_call_steal_reply pending))
	(msg-iter (make-DBusMessageIter)))
    (if (zero? (fh-object-ref msg)) (error "*** reply message NULL\n"))
    (dbus_pending_call_unref pending)
    (dbus_message_iter_init msg (pointer-to msg-iter))
    (sf "result:\n")
    (pretty-print (read-dbus-val (pointer-to msg-iter)) #:per-line-prefix "  ")
    (dbus_message_unref msg)))

(define (run-server)
  ;;(set! conn (spawn-dbus-mainloop 'session))
  (set! conn (dbus_bus_get 'DBUS_BUS_SESSION NULL))
  (sf "conn=~S\n" conn)
  (let* ((flags (apply logior
		       (map dbus-symval
			    '(
			      ;; DBUS_NAME_FLAG_ALLOW_REPLACEMENT
			      ;; DBUS_NAME_FLAG_DO_NOT_QUEUE
			      ))))
	 )
    (let* ((rpl (dbus_bus_request_name conn bus-name flags NULL)))
      (sf "req name => ~S ~S\n" rpl (dbus-request-name-reply rpl))
      (case (dbus-request-name-reply rpl)
	((PRIMARY_OWNER) #t)
	((IN_QUEUE) (error "IN_QUEUE"))
	((EXISTS) (error "EXISTS"))
	((ALREADY_OWNER) #t)))
    ;; org.freedesktop.DBus.Peer : Ping(), GetMachineId()
    ;; org.freedesktop.DBus.Introspectable : Introspect()
    ;; org.freedesktop.DBus.Properties
    ;; org.freedesktop.DBus.ObjectManager
    ;;(dbus_bus_add_match conn ""
    (let loop ()
      (dbus_connection_read_write conn 0)
      (let ((msg (dbus_connection_pop_message conn)))
	(cond
	 ((zero? (fh-object-ref msg)) #f)
	 (else
	  (sf "\ngot message ~S\n" msg)
	  (sf "  path=~S\n" (p2s (dbus_message_get_path msg)))
	  (sf "  type=~S\n" (dbus-message-type (dbus_message_get_type msg)))
	  (sf "  i/f =~S\n" (p2s (dbus_message_get_interface msg)))
	  (sf "  mem =~S\n" (p2s (dbus_message_get_member msg)))
	  ;;(sf "  ~S\n" (p2s (dbus_message_get_signature msg)))
	  (if (!0 (dbus_message_has_interface
		   msg "org.freedesktop.DBus.Introspectable"))
	      (reply-to-intro conn msg))
	  (dbus_message_unref msg)))
	(sleep 1)
	(loop)))
    ))

(define (run-client)
  (set! conn (spawn-dbus-mainloop 'session))
  (let ((sig01 (dbus_message_new_signal bus-name obj-path if-name))
	)
    (send-sig conn sig01)
    #f))

  
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

;;(apply main (cdr (program-arguments)))

;; --- last line ---
