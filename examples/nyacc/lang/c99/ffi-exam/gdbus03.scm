;; gdbus03.scm - dbus stats w/ type printing

(include-from-path "nyacc/lang/c99/ffi-exam/gdbus00.scm")

;; (sv) => GVariant*
(define (make-gv-type spec)
  #f)

;; "s" => GVariantType*
(define (make-gv-base-type str)
  (let* ((code (string-copy str))
	 (ptr (ffi:string->pointer code))
	 (addr (ffi:pointer-address ptr)))
    (glib-guardian code)
    (make-GVariantType* addr)))

(define gv-uint8-type (make-gv-base-type "y"))
(define gv-bool-type (make-gv-base-type "b"))
(define gv-int16-type (make-gv-base-type "n"))
(define gv-uint16-type (make-gv-base-type "q"))
(define gv-int32-type (make-gv-base-type "i"))
(define gv-uint32-type (make-gv-base-type "u"))
(define gv-int64-type (make-gv-base-type "x"))
(define gv-uint64-type (make-gv-base-type "t"))
(define gv-double-type (make-gv-base-type "d"))
(define gv-string-type (make-gv-base-type "s"))

(define (nonzero? v) (not (zero? v)))

;; simple variant -> scm
(define (simple-gv->scm variant)
  (cond
   ;;((nonzero? (g_variant_type_is_of_type variant gv-uint8-type)) #f)
   ((nonzero? (g_variant_is_of_type variant gv-bool-type))
    (not (zero? (g_variant_get_boolean variant))))
   ((nonzero? (g_variant_is_of_type variant gv-int16-type))
    (g_variant_get_int16 variant))
   ((nonzero? (g_variant_is_of_type variant gv-uint16-type))
    (g_variant_get_uint16 variant))
   ((nonzero? (g_variant_is_of_type variant gv-int32-type))
    (g_variant_get_int32 variant))
   ((nonzero? (g_variant_is_of_type variant gv-uint32-type))
    (g_variant_get_uint32 variant))
   ((nonzero? (g_variant_is_of_type variant gv-int64-type))
    (g_variant_get_int64 variant))
   ((nonzero? (g_variant_is_of_type variant gv-uint64-type))
    (g_variant_get_uint64 variant))
   ((nonzero? (g_variant_is_of_type variant gv-double-type))
    (g_variant_get_double variant))
   (else
    (error "not handled"))))

;; GVariant* => scm
(define (gv->scm variant)
  (let* ((type (g_variant_get_type_string variant))
	 (type (ffi:pointer->string type))
	 )
    (cond
     ((< 1 (string-length type))
      ;; compound type
      #f)
     ((member (string-ref type 0) '(#\v)) 
      (gv->scm (g_variant_get_variant variant)))
     (else
      (simple-gv->scm variant)))))

;; (array-of dict


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
	    (val ~val)
	    )
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
	 (elt0 (g_variant_get_child_value rez 0))
	 )
    ;; needs work
    (glib-guardian elt0)
    (for-each-gv-dict-entry
     (lambda (key val)
       (let* ((vv (g_variant_get_variant val))
	      (vt (g_variant_get_type_string vv))
	      (v (g_variant_get_uint32 vv))
	      )
	 ;;(sf "~S:\n" key)
	 ;;(sf " ~S: ~S\n" (ffi:pointer->string vt) v)
	 (sf "~A: ~S\n" key (gv->scm vv))
	 ))
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
