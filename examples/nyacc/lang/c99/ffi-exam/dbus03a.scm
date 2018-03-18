;; dbus03a.scm - dbus stats, cleaned up

(include-from-path "nyacc/lang/c99/ffi-exam/dbus00.scm")

;; === main ============================

;; @deffn {Procedure} for-each-variant proc coll
;; iterate over the variants with @var{proc} using @code{(proc elt)}.
;; @end deffn
(define (for-each-variant proc coll)
  (let ((gviter (g_variant_iter_new coll)))
    (let iter ()
      (let ((gv (g_variant_iter_next_value gviter)))
	(unless (gv-null? gv)
	  ;; we should pass a scheme value maybe
	  (proc gv)
	  (iter))))))

(define (for-each-gv-dict-entry proc coll)
  (for-each-variant
   (lambda (elt)
     ;; should get {sv}
     (let* ((~key (g_variant_get_child_value elt 0))
	    (~val (g_variant_get_child_value elt 1))
	    (key (ffi:pointer->string (g_variant_get_string ~key NULL)))
	    (val ~val))
       (proc key val)))
   coll))

(use-modules (sxml simple))
(use-modules (sxml xpath))

(define loop (g_main_loop_new NULL FALSE))

(define error (make-GError*))

(define conn (g_bus_get_sync 'G_BUS_TYPE_SESSION NULL (pointer-to error)))

(define return-type (g_variant_type_new "(a{sv})"))

(define (check-rez rez)			; rez: GVariant*
  (let* ((type (ffi:pointer->string (g_variant_get_type_string rez)))
	 (elt0 (g_variant_get_child_value rez 0)))
    (for-each-gv-dict-entry
     (lambda (key val)
       (let* ((vv (g_variant_get_variant val))
	      (vt (g_variant_get_type_string vv))
	      (val (g_variant_get_uint32 vv)))
       (sf "~A: ~S\n" key val)))
     elt0)))

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
 "/org/freedesktop/DBus"		; object path
 "org.freedesktop.DBus.Debug.Stats"	; interface name
 "GetStats"				; method
 NULL					; parameters
 return-type				; GVariantType*
 'G_DBUS_CALL_FLAGS_NONE		; GDBusCallFlags
 1000					; timeout_msec
 NULL					; GCancellable*
 callback				; GAsyncReadyCallback
 NULL					; user_data
 )

(g_variant_type_free return-type)

(g_main_loop_run loop)

;; --- last line ---
