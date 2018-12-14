;; dbus00.scm 

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

(define-module (dbus00)
  #:export (check-error
	    get-bval
	    read-dbus-val
	    get-dbus-message-args
	    nonzero?
	    TRUE FALSE
	    ;;
	    dbus-version
	    dbus-bus-get-unique-name
	    dbus-message-get-sender
	    make-DBusMessageIter&
	    ;;
	    dbus-message-type
	    dbus-request-name-reply
	    make-dbus-string
	    make-dbus-pointer
	    )
  #:use-module (system ffi-help-rt)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructures guile)
  #:use-module (ffi dbus)
  )
(use-modules ((ice-9 iconv) #:select (string->bytevector)))
(use-modules (rnrs bytevectors))

(define TRUE 1)
(define FALSE 0)
(define (nonzero? val) (not (zero? val)))

(define (check-error error)
  (or (zero? (dbus_error_is_set (pointer-to error)))
      (simple-format
       #t "~A\n" (ffi:pointer->string
		  (ffi:make-pointer (fh-object-ref error 'message))))))
  
(define (get-bval &iter key)
  (let* ((bval (make-DBusBasicValue)))
    (dbus_message_iter_get_basic &iter (pointer-to bval))
    (fh-object-ref bval key)))

(define (read-dbus-val &iter)
  (case (dbus_message_iter_get_arg_type &iter)
    ;;((0) (if #f #f))				    ; 0 - invalid
    ((0) '())					    ; 0 - invalid
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

;; Given a message (or message) reply return the list of args.
(define (get-dbus-message-args msg)
  (let* ((iter (make-DBusMessageIter))
	 (&iter (pointer-to iter)))
    (dbus_message_iter_init msg &iter)
    (let loop ((arg (read-dbus-val &iter)))
      (cond ((null? arg) '())
	    (else (dbus_message_iter_next &iter)
		  (cons arg (loop (read-dbus-val &iter))))))))

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

(define (dbus-version)
  (let ((maj (make-int)) (min (make-int)) (mic (make-int)))
    (dbus_get_version (pointer-to maj) (pointer-to min) (pointer-to mic))
    (simple-format #f "~A.~A.~A"
		   (fh-object-ref maj)
		   (fh-object-ref min)
		   (fh-object-ref mic))))

(define (dbus-bus-get-unique-name conn)
  (ffi:pointer->string (dbus_bus_get_unique_name conn)))

(define (dbus-message-get-sender conn)
  (ffi:pointer->string (dbus_message_get_sender conn)))

(define (make-DBusMessageIter&)
  (pointer-to (make-DBusMessageIter)))

;; When marshalling and de-marshalling strings requires pointer.
;;   char *str_send = "hello";
;;   char *str_recv;
;; dbus_message_iter_append_basic(&iter, DBUS_TYPE_STRING, &str_send);
;; dbus_message_iter_get_basic(&iter, DBUS_TYPE_STRING, &str_recv);

;; generate a pointer to pointer to string
;;   char *str = "hello"; => &str

;; use make-char* pointer-to char*->string
#|
(define (make-dbus-string str)
  (let* ((ptr-size (ffi:sizeof '*))
	 (addr (ffi:pointer-address (ffi:string->pointer str)))
	 (bv (make-bytevector ptr-size)))
    (case ptr-size
      ((8) (bytevector-u64-native-set! bv 0 addr))
      ((4) (bytevector-u32-native-set! bv 0 addr))
      (else (error "bad pointer size")))
    (ffi:bytevector->pointer bv)))

(define (make-dbus-pointer)
  (let* ((ptr-size (ffi:sizeof '*))
	 (bv (make-bytevector ptr-size)))
    (ffi:bytevector->pointer bv)))
|#

;; --- last line ---
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
