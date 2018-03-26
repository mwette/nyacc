;; dbus02.scm

(include-from-path "nyacc/lang/c99/ffi-exam/dbus00.scm")


;; === main ============================

(use-modules (sxml simple))
(use-modules (sxml xpath))

(define loop (g_main_loop_new NULL FALSE))

(define error (make-GError*))

(define conn (g_bus_get_sync 'G_BUS_TYPE_SESSION NULL (pointer-to error)))

(define (check-rez rez)			; rez: GVariant*
  (let* ((type (ffi:pointer->string (g_variant_get_type_string rez)))
	 (elt0 (g_variant_get_child_value rez 0))
	 (strp (g_variant_get_string elt0 NULL))
	 (strv (ffi:pointer->string strp))
	 (sx0 (xml->sxml strv #:trim-whitespace? #t))
	 (sxi ((sxpath '(// interface)) sx0)) ; interfaces
	 )
    ;; needs work
    (glib-guardian elt0)
    ;;(display strv)
    (pp sxi)
    ))

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
