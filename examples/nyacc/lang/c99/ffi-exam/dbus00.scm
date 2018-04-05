;;

(define-module (dbus00)
  #:export (check-error get-bval read-dbus-val)
  #:use-module (system ffi-help-rt)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructures guile)
  #:use-module (ffi dbus)
  )

(define (check-error error)
  (or (zero? (dbus_error_is_set (pointer-to error)))
      (sf "~A\n" (ffi:pointer->string
		  (ffi:make-pointer (fh-object-ref error 'message))))))
  
(define (get-bval &iter key)
  (let* ((bval (make-DBusBasicValue)))
    (dbus_message_iter_get_basic &iter (pointer-to bval))
    (fh-object-ref bval key)))

(define (read-dbus-val &iter)
  ;; 0   0 : invalid; y 121 : byte; b  98 : boolean; n 110 : int16;
  ;; q 113 : uint16; i 105 : int32; u 117 : uint32; x 120 : int64
  ;; t 116 : uint64; d 100 : double; s 115 : string; o 111 : object path
  ;; g 103 : signature; h 104 : unix fd; a  97 : array; v 118 : variant
  ;; r 114 : struct; e 101 : dict entry
  (case (dbus_message_iter_get_arg_type &iter)
    ((0) (if #f #f)) ;; 0 - invalid
    ((121) (get-bval &iter 'byt))		    ; y - byte
    ((98) (not (zero? (get-bval &iter 'bool_val)))) ; b - boolean
    ((110) (get-bval &iter 'i16))		    ; n - int16
    ((113) (get-bval &iter 'u16))		    ; q - uint16
    ((105) (get-bval &iter 'i32))		    ; i - int32
    ((117) (get-bval &iter 'u32))		    ; u - uint32
    ((120) (get-bval &iter 'i64))		    ; x - int64
    ((116) (get-bval &iter 'u32))		    ; t - uint64
    ((100) (get-bval &iter 'dbl))		    ; d - double
    ((115 111 103)				    ; s, o, g 
     (ffi:pointer->string (ffi:make-pointer (get-bval &iter 'str))))
    ((104) (get-bval &iter 'fd))	; h - unix fd
    ((97)				; a - array
     (let* ((sub-iter (make-DBusMessageIter))
	    (&sub-iter (pointer-to sub-iter)))
       (dbus_message_iter_recurse &iter &sub-iter)
       (let loop ()
	 (cons (read-dbus-val &sub-iter)
	       (if (zero? (dbus_message_iter_next &sub-iter)) '() (loop))))))
    ((118)				; v - variant (boxed value)
     (let* ((sub-iter (make-DBusMessageIter))
	    (&sub-iter (pointer-to sub-iter)))
       (dbus_message_iter_recurse &iter &sub-iter)
       (read-dbus-val &sub-iter)))
    ((114) (error "not defined: r"))	; r - struct
    ((101)				; e - dict entry
     (let* ((sub-iter (make-DBusMessageIter))
	    (&sub-iter (pointer-to sub-iter)))
       (dbus_message_iter_recurse &iter &sub-iter)
       (cons
	(read-dbus-val &sub-iter)
	(begin
	  (dbus_message_iter_next &sub-iter)
	  (read-dbus-val &sub-iter)))))
    (else
     (error "not defined"))))

;; --- last line ---
