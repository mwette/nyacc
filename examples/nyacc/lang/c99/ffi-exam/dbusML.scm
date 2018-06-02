;; dbusML.scm - dbus in another thread : sync calls
;; https://lists.freedesktop.org/archives/dbus/2007-October/008859.html
;; https://stackoverflow.com/questions/9378593/dbuswatch-and-dbustimeout-examples

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

(define-module (dbusML)
  #:export (spawn-dbus-mainloop)
  )
  
(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (bytestructures guile))

(use-modules (ice-9 threads))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-43))

(use-modules (ffi epoll))
(use-modules (ffi dbus))
(use-modules (dbus00))

(use-modules (ice-9 pretty-print))
(use-modules (ice-9 format))
(define (sf fmt . args) (apply simple-format #t fmt args))
(define (ff fmt . args) (apply format #t fmt args))

;; ====================================
;; use case 01: want to queue in any thread, poll for ready

;; moved to dbus00
;;(define TRUE 1)
;;(define FALSE 0)
;;(define (nonzero? val) (not (zero? val)))

;; notes
;; 1) One can call epoll_ctl while epoll_wait is blocked in another thread.
;;    ref: epoll_wait man page
;; 2) It is safe to use scm->pointer and pointer->scm as long as the pointed
;;    object was allocated with scm_gc_malloc().  (Ref: Ludo answer to Q on list)

;; convert mask of DBUS_WATCH to enum EPOLL_EVENTS
(define (dbus-watch-flags->epoll-events dbus-flags)
  (fold
   (lambda (dbus-flag epoll-types)
     (if (not (zero? (logand dbus-flag dbus-flags)))
	 (case dbus-flag
	   ((1) (logior epoll-types 1))  ; readable
	   ((2) (logior epoll-types 4))  ; writable
	   ((4) (logior epoll-types 8))	 ; error
	   ((8) (logior epoll-types 16)) ; hangup
	   (else (error "unhandled case")))
	 epoll-types))
   0 '(1 2 4 8)))

(define (epoll-events->dbus-watch-flags epoll-events)
  (fold
   (lambda (epoll-event dbus-flags)
     (if (not (zero? (logand epoll-event epoll-events)))
	 (case epoll-event
	   ((1) (logior dbus-flags 1))  ; readable
	   ((4) (logior dbus-flags 2))  ; writable
	   ((8) (logior dbus-flags 4))  ; error
	   ((16) (logior dbus-flags 8)) ; hangup
	   (else (error "unhandled case")))
	 dbus-flags))
   0 '(1 4 8 16)))

(define (scm->addr scm)
  (ffi:pointer-address (ffi:scm->pointer scm)))
(define (addr->scm addr)
  (ffi:pointer->scm (ffi:make-pointer addr)))

(define-record-type dbus-data
  (make-dbus-data fd ev wv)
  dbus-data?
  (fd dbus-data-fd)			; epoll fd, if in epoll set
  (ev dbus-data-ev)			; epoll event
  (wv dbus-data-wv set-dbus-data-wv!))	; watch vector

(define *dbus-maxw* 3)
(define *dbus-fd-dict* (make-hash-table 31))

;; if not found add a blank entry
(define (dbus-lookup-fd fd)
  (or (hashv-ref *dbus-fd-dict* fd)
      (let* ((event (make-struct-epoll_event))
	     (ddent (make-dbus-data fd event (make-vector *dbus-maxw* #f))))
	(hashv-set! *dbus-fd-dict* fd ddent)
	(fh-object-set! event 'data 'ptr (scm->addr ddent))
	;;(ff "new ddent@ 0x~x   w/ event ~s\n" (scm->addr ddent) event)
	ddent)))

(define (find-wv-slot wv)
  (let iter ((i 0) (n (vector-length wv)))
    (cond
     ((= i n) -1)
     ((vector-ref wv i) (iter (1+ i) n))
     (else i))))

(define (dbus-data-watched? ddent)
  (= 0 (find-wv-slot (dbus-data-wv ddent))))

(define (dbus-data-watchless? ddent)
  (not (dbus-data-watched? ddent)))

(define (dbus-fd-watchless? fd)
  (not (and=> (hashv-ref *dbus-fd-dict* fd) dbus-data-watched?)))

(define (dbus-data-free ddent)
  (if #f #f))

;; no need implement this with use of the get_dispatch_status/dispatch loop
(define (dispatch-status connection ~status data)
  ;;(display "dispatch-status called\n")
  (if #f #f))

;; @deffn {Procedure} add-watch watch data
;; @end deffn
(define (add-watch ~watch data)
  (let* ((watch (make-DBusWatch* ~watch))
	 (muxfd (ffi:pointer-address data))
	 (addfd (dbus_watch_get_unix_fd watch))
	 (flags (dbus_watch_get_flags watch))
	 (ddent (dbus-lookup-fd muxfd))
	 (event (dbus-data-ev ddent)))
    ;;(ff "\nadd-watch  ~s: addfd=~s flags=0x~x...\n" watch addfd flags)

    (dbus_watch_set_data watch (ffi:scm->pointer ddent) dbus-data-free)
    
    ;; Set up the indended set of epoll events.
    (if (nonzero? (dbus_watch_get_enabled watch))
	(fh-object-set! event 'events
			(logior (fh-object-ref event 'events)
				(dbus-watch-flags->epoll-events flags))))

    ;; If this is the use of this fd, then initialize the ev and add to epoll.
    (if (dbus-data-watchless? ddent)
	(epoll_ctl muxfd (EPOLL '_CTL_ADD) addfd (pointer-to event)))
    
    ;; Set watches based on flags.
    (let* ((wv (dbus-data-wv ddent)) (wx (find-wv-slot wv)))
      (if (negative? wx) (error "max exceeded")
	  (vector-set! wv wx watch)))
    
    TRUE))

;; @deffn {Procedure} remove-watch watch data
;; @end deffn
(define (remove-watch ~watch data)
  (let* ((watch (make-DBusWatch* ~watch))
	 (muxfd (ffi:pointer-address data))
	 (delfd (dbus_watch_get_unix_fd watch))
	 ;;(ddent (dbus_watch_get_data watch))
	 (ddent (hashv *dbus-fd-dict* delfd))
	 (event (dbus-data-ev ddent))
	 (events (fh-object-ref event 'events))
	 )
    (when #t
      (sf "\nrem-watch  ~S  ~S\n" watch data)
      )
    ;; remove watch from ddent and fix events mask.
    ;; if no watches left then remove fd from epoll
    ;;(if (no more watches on this fd)
    ;;    (epoll_ctl muxfd (EPOLL '_CTL_DEL) delfd (ffi:scm->pointer ddent)))
    (if #f #f)))

;; @deffn {Procedure} watch-toggled watch data
;; @end deffn
(define (watch-toggled ~watch data)
  (let* ((watch (make-DBusWatch* ~watch))
	 (muxfd (ffi:pointer-address data))
	 (flags (dbus_watch_get_flags watch))
	 )
    (when #t
      (sf "watch-tog  ~S  ~S ...\n" watch data)
      (sf "  enabled: ~S\n" (dbus_watch_get_enabled watch)))
    (if #f #f)))


(use-modules (sched))

(define *dbus-sched* (make-scheduler))

(define (dbus-timeout-handler sch timeout)
  (sf "\ntimeout handler called\n")
  (dbus_timeout_handle timeout))

;; timeout is DBusTimeout
(define (add-timeout ~timeout data)
  (let* ((tod (gettimeofday))
	 (timeout (make-DBusWatch* ~timeout))
	 (interval (dbus_timeout_get_interval timeout))
	 (exp (t+us tod interval)))
    ;;(ff "add-tmout: ~s expire at ~s\n" timeout exp)
    (schedule-event *dbus-sched* exp dbus-timeout-handler timeout)
    (write #\x (ffi:pointer->scm data))	; wake up mainloop
    TRUE))

(define (remove-timeout ~timeout data)
  (let* ((timeout (make-DBusWatch* ~timeout)))
    ;;(ff "rem-tmout: ~s cancel at ~s\n" timeout (gettimeofday))
    (cancel-events/data *dbus-sched* timeout)
    (if #f #f)))

(define (timeout-toggled timeout data)
  (let ()
    (display "tmout-tog called (to be completed!)\n")
    (if #f #f)))

;; This sets up capability to make runtime-sized vectors and use
;; @code{pointer-to} cast for function args.  See @code{epoll_wait} below.
(define-fh-unsized-vector-type struct-epoll_event-vec struct-epoll_event-desc
  struct-epoll_event-vec? make-struct-epoll_event-vec)
(fh-ref<->deref! struct-epoll_event* make-struct-epoll_event*
		 struct-epoll_event-vec #f)

(define (filter-func c m data)
  (when #f
    (sf "filter-func called ...\n  iface : ~S\n  member: ~S\n  path  : ~S\n"
	(ffi:pointer->string (dbus_message_get_interface m))
	(ffi:pointer->string (dbus_message_get_member m))
	(ffi:pointer->string (dbus_message_get_path m))))
  (DBUS 'HANDLER_RESULT_NOT_YET_HANDLED)
  ;;(DBUS 'HANDLER_RESULT_HANDLED)
  )

(define (set-nonblocking! port)
  (fcntl port F_SETFL (logior O_NONBLOCK (fcntl port F_GETFL))))

(define (my-main-loop connection)
  (define (dispatch-em)
    (while (eq? 'DBUS_DISPATCH_DATA_REMAINS
		(dbus_connection_get_dispatch_status connection))
      (dbus_connection_dispatch connection)))
  
  (define (handle-watch watch flags)
    (let ((flags (logand flags (dbus_watch_get_flags watch))))
      ;; This loop-sleeps while out of memory.
      (while (equal? FALSE (dbus_watch_handle watch flags))
	(sleep 1))))
  
  (let* ((muxfd (epoll_create 1))
	 (muxpt (ffi:make-pointer muxfd))
	 (wpipe (pipe)) (wiport (car wpipe)) (woport (cdr wpipe))
	 (~woport (ffi:scm->pointer woport))
	 (max-events 2)
	 (eventv (make-struct-epoll_event-vec max-events))
	 (eventp (pointer-to eventv)))

    ;; Set up wakeup, initiated from handlers.
    (setvbuf woport  'none)
    (set-nonblocking! woport)
    (set-nonblocking! wiport) ;; needed?
    (let ((event (make-struct-epoll_event)))
      (fh-object-set! event 'events (EPOLL 'IN))
      (epoll_ctl muxfd (EPOLL '_CTL_ADD) (port->fdes wiport) (pointer-to event)))
    ;; Set up DBus MT locks, and mainloop hook functions.
    (dbus_threads_init_default)
    (dbus_connection_set_dispatch_status_function
     connection dispatch-status muxpt dbus-data-free)
    (dbus_connection_set_watch_functions
     connection add-watch remove-watch watch-toggled muxpt dbus-data-free)
    (dbus_connection_set_timeout_functions
     connection add-timeout remove-timeout timeout-toggled ~woport NULL)
    ;; These are in dbus list archive 008859.html.
    (dbus_connection_add_filter connection filter-func NULL NULL)
    ;;(dbus_bus_add_match connection "type='signal'" NULL)
    (dbus_bus_add_match connection "type='method_call'" NULL)
    (let loop ()
      (let iter ((i 0) (n (epoll_wait muxfd eventp max-events -1)))
	;; timeouts use (pair (gettimeofday)) => (sec . usec)
	(let iter ((tod (gettimeofday))
		   (nxt (earliest-event-time *dbus-sched*)))
	  (when (t< tod nxt)
	    (exec-schedule *dbus-sched* tod)
	    (iter (gettimeofday) (earliest-event-time *dbus-sched*))))
	;; events: wake-up or watches
	(unless (= i n)
	  ;;(sf "\n  i=~S/~S ...\n" i n)
	  (let* ((event (fh-object-ref eventv i))
		 (events (bytestructure-ref event 'events))
		 (data-ptr (bytestructure-ref event 'data 'ptr)))
	    (cond
	     ((zero? data-ptr)
	      (read-char wiport))
	     (else
	      (let* ((flags (epoll-events->dbus-watch-flags events))
		     (ddent (addr->scm data-ptr)))
		;;(ff "flags=0x~x\n" flags)
		(vector-for-each
		 (lambda (ix watch)
		   (when watch
		     (handle-watch watch flags)
		     (dbus_connection_ref connection)
		     (dispatch-em)
		     (dbus_connection_unref connection)))
		 (dbus-data-wv ddent))))))
	  (iter (1+ i) n)))
      (loop))
    ;;
    (close-fdes muxfd)))

;; === main ==========================

;;(define error (make-DBusError))
;;(dbus_error_init (pointer-to error))
;;(define conn (dbus_bus_get 'DBUS_BUS_SESSION (pointer-to error)))
;;(check-error error)
;;(sf "conn=~S\n" (ffi:pointer->string (dbus_bus_get_unique_name conn)))
;;(my-main-loop conn) ;; later in a dedicated thread

(define (spawn-dbus-mainloop service)
  (let* ((bus-id
	  (case service
	    ((session) 'DBUS_BUS_SESSION)
	    ((system) 'DBUS_BUS_SYSTEM)
	    (else (error "bad bus id"))))
	 (error
	  (let ((error (make-DBusError)))
	    (dbus_error_init (pointer-to error))
	    error))
	 (conn
	  (let ((conn (dbus_bus_get bus-id (pointer-to error))))
	    (check-error error)
	    conn)))
    (call-with-new-thread (lambda () (my-main-loop conn)))
    (sleep 1)
    conn))

;; --- last line ---
