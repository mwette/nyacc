;; dbus01.scm

(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (bytestructures guile))

(use-modules (ffi glib))
(use-modules (ffi gobject))
(use-modules (ffi gio))

(define (sf fmt . args) (apply simple-format #t fmt args))

(define FALSE 0)

(define (got-error? error)
  (not (zero? (bytestructure-ref (fh-object-val error)))))

(define (g-error-message error)
  (let* ((eval (fh-object-ref error '* 'message))
	 (pval (ffi:make-pointer eval))
	 (sval (ffi:pointer->string pval)))
    sval))

(define glib-guardian (make-guardian))

(define gv-string-singleton-type	; gen. variant type "(s)"
  (let* ((code "s")
	 (cptr (ffi:string->pointer code)) ; GVariantType* for "s"
	 (cadr (ffi:pointer-address cptr))
	 (cvec (bytestructure (bs:vector 1 (bs:pointer int8)) (vector cadr)))
	 (cptr (ffi:make-pointer (bs-addr cvec)))
	 (gvar (g_variant_type_new_tuple cptr 1)))
    (glib-guardian code)		; guard "s" from collection
    gvar))

;; === main ============================

(define loop (g_main_loop_new NULL FALSE))

(define error (make-GError*))

(define conn (g_bus_get_sync 'G_BUS_TYPE_SESSION NULL (pointer-to error)))
(sf "conn=~S\n" conn)

(define (check-rez rez)			; rez: GVariant*
  (let* ((type (ffi:pointer->string (g_variant_get_type_string rez)))
	 (elt0 (g_variant_get_child_value rez 0))
	 (strp (g_variant_get_string elt0 NULL))
	 (strv (ffi:pointer->string strp)))
    ;; needs work
    (glib-guardian elt0)
    (display strv)))

(define callback
  (make-GAsyncReadyCallback
   (lambda (~src ~res user_data)
     (let* ((src (make-GObject* ~src))
	    (res (make-GAsyncResult* ~res))
	    (err (make-GError*))
	    (rez (g_dbus_connection_call_finish conn res (pointer-to err)))
	    )
       (if (got-error? err)
	   (sf "~A\n" (g-error-message err))
	   (check-rez rez))
       (g_main_loop_quit loop)
       (if #f #f)))))

(g_dbus_connection_call
 conn					; connection
 "org.freedesktop.DBus"			; bus name (was NULL)
 "/"					; object path
 "org.freedesktop.DBus.Introspectable"	; interface name
 "Introspect"				; method
 NULL					; parameters
 gv-string-singleton-type		; GVariantType*
 'G_DBUS_CALL_FLAGS_NONE		; GDBusCallFlags
 1000					; timeout_msec
 NULL					; GCancellable*
 callback				; GAsyncReadyCallback
 NULL					; user_data
 )

(g_main_loop_run loop)

;; --- last line ---
