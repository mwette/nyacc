;; dbus03.scm - dbus
;; see http://www.matthew.ath.cx/misc/dbus

;; Copyright (C) 2018 Matthew R. Wette
;;
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

(use-modules (ice-9 pretty-print))
(define (sf fmt . args) (apply simple-format #t fmt args))

(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (bytestructures guile))

(use-modules (ffi dbus))

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
    ((115) (ffi:pointer->string (ffi:make-pointer (get-bval &iter 'str)))) ; s
    ((111) (error "not defined: o"))	; o - object path
    ((103) (error "not defined: g"))    ; g - signature
    ((104) (error "not defined: h"))	; h - unix fd
    ((97) ; a - array
     (let* ((sub-iter (make-DBusMessageIter))
	    (&sub-iter (pointer-to sub-iter)))
       (dbus_message_iter_recurse &iter &sub-iter)
       (let loop ()
	 (cons (read-dbus-val &sub-iter)
	       (if (zero? (dbus_message_iter_next &sub-iter)) '()
		   (loop))))))
    ((118) ; v - variant (boxed value)
     (let* ((sub-iter (make-DBusMessageIter))
	    (&sub-iter (pointer-to sub-iter)))
       (dbus_message_iter_recurse &iter &sub-iter)
       (read-dbus-val &sub-iter)))
    ((114) (error "not defined: r"))	; r - struct
    ((101) ;; e - dict entry
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

;; ====================================

(define error (make-DBusError))
(dbus_error_init (pointer-to error))

(define conn (dbus_bus_get 'DBUS_BUS_SESSION (pointer-to error)))
(check-error error)
(sf "conn: ~S = ~S\n" conn (ffi:pointer->string (dbus_bus_get_unique_name conn)))

(define msg (dbus_message_new_method_call
	     "org.freedesktop.DBus"		; bus name (was NULL)
	     "/org/freedesktop/DBus"		; object path
	     "org.freedesktop.DBus.Debug.Stats"	; interface name
	     "GetStats"))			; method

(define pending (make-DBusPendingCall*))
(or (dbus_connection_send_with_reply conn msg (pointer-to pending) -1)
    (error "*** send_with_reply FAILED\n"))
(if (zero? (fh-object-ref pending)) (display "*** pending NULL\n"))

(dbus_connection_flush conn)
(dbus_message_unref msg)
(dbus_pending_call_block pending)

(set! msg (dbus_pending_call_steal_reply pending))
(if (zero? (fh-object-ref msg)) (error "*** reply message NULL\n"))
(sf "msg from reply:~S, serial:~S, type:~A\n" msg (dbus_message_get_serial msg)
    (let ((msg-type (dbus_message_get_type msg)))
      (cond
       ((eq? (DBUS 'MESSAGE_TYPE_INVALID) msg-type) "invalid")
       ((eq? (DBUS 'MESSAGE_TYPE_METHOD_CALL) msg-type) "method call")
       ((eq? (DBUS 'MESSAGE_TYPE_METHOD_RETURN) msg-type) "method return")
       ((eq? (DBUS 'MESSAGE_TYPE_ERROR) msg-type) "error")
       ((eq? (DBUS 'MESSAGE_TYPE_SIGNAL) msg-type) "signal"))))

(define msg-iter (make-DBusMessageIter))
(dbus_pending_call_unref pending)

(sf "iter_init => ~S\n" (dbus_message_iter_init msg (pointer-to msg-iter)))
(sf "result:\n")
(pretty-print (read-dbus-val (pointer-to msg-iter)) #:per-line-prefix "  ")

(dbus_message_unref msg)
;;(dbus_connection_close conn)

;; --- last line ---
