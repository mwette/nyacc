;; dbus1.scm

;; https://askubuntu.com/questions/276392/d-bus-where-to-get-started
;; d-feet command on ubuntu
(define (sf fmt . args) (apply simple-format #t fmt args))

(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (bytestructures guile))

(use-modules (ffi glib))
(use-modules (ffi gobject))
(use-modules (ffi gio))

(define FALSE 0)			; should this go somewhere?

(define (g-error-message error)
  (let* ((eval (fh-object-ref error '* 'message))
	 (pval (ffi:make-pointer eval))
	 (sval (ffi:pointer->string pval)))
    sval))

(define res #f)

(define loop (g_main_loop_new NULL FALSE))

(g_type_init)

(define error (make-GError*))

(define (got-error? error)
  (not (zero? (bytestructure-ref (fh-object-val error)))))

(define conn
  (g_bus_get_sync 'G_BUS_TYPE_SESSION NULL (pointer-to error)))

(define glib-guardian (make-guardian))

(define gv-string-singleton-type	; verified to work
  (let* ((code "s")
	 (cptr (ffi:string->pointer code)) ; GVariantType* for string
	 (cadr (ffi:pointer-address cptr))
	 (cvec (bytestructure (bs:vector 1 (bs:pointer int8)) (vector cadr)))
	 (cptr (ffi:make-pointer (bs-addr cvec)))
	 (gvar (g_variant_type_new_tuple cptr 1)))
    (glib-guardian code)		; guard "s" from collection
    gvar))

;; verify gv-string-singleton-type => "(s)"
;;(sf "~S\n" (ffi:pointer->string
;;	    (g_variant_type_dup_string gv-string-singleton-type)))
;;(quit)

(define (check-rez rez)			; rez: GVariant*
  (let* ((type (ffi:pointer->string (g_variant_get_type_string rez)))
	 (elt0 (g_variant_get_child_value rez 0))
	 (strp (g_variant_get_string elt0 NULL))
	 (strv (ffi:pointer->string strp))
	 )
    (sf "rez: ~S\n" rez)
    (sf "  type: ~S\n" type)
    (sf "  elt0: ~S\n" elt0)
    (sf "  strv: ~S\n" strv)
    ))

(define callback
  (make-GAsyncReadyCallback
   (lambda (~src ~res user_data)
     (let* ((src (make-GObject* ~src))
	    (res (make-GAsyncResult* ~res))
	    (err (make-GError*))
	    (rez (g_dbus_connection_call_finish conn res (pointer-to err)))
	    )
       (sf "src: ~S\n" src)		; GObject*
       (sf "res: ~S\n" res)		; GAsyncResult*
       (sf "err: ~S\n" err)		; GError*
       (if (got-error? err)
	   (sf "~A\n" (g-error-message err))
	   (check-rez rez))
       (g_main_loop_quit loop)
       (if #f #f)))))

(define cancellable (g_cancellable_new))

(set! res (g_dbus_connection_call
	   conn				; connection
	   "com.dell.RecoveryMedia"	; bus name (was NULL)
	   "/RecoveryMedia"		; object path
	   "org.freedesktop.DBus.Introspectable" ; interface name
	   "Introspect"			; method
	   NULL				; parameters
	   gv-string-singleton-type	; GVariantType* (should be "(s)")
	   'G_DBUS_CALL_FLAGS_NONE	; GDBusCallFlags
	   6000				; timeout_msec
	   cancellable			; GCancellable*
	   callback			; GAsyncReadyCallback
	   NULL				; user_data
	   ))

(g_main_loop_run loop)

;; --- last line ---
