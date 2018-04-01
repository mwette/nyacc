;; dbus04.scm - dbus in another thread : sync calls
;; https://lists.freedesktop.org/archives/dbus/2007-October/008859.html
;; https://stackoverflow.com/questions/9378593/dbuswatch-and-dbustimeout-examples

(use-modules (ice-9 pretty-print))
(define (sf fmt . args) (apply simple-format #t fmt args))

(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (bytestructures guile))

(use-modules (ice-9 threads))
(use-modules (srfi srfi-1))

(use-modules (ffi epoll))
(use-modules (ffi dbus))
(use-modules (dbus00))

;; ====================================
;; use case 01: want to queue in any thread, poll for ready

(define TRUE 1)
(define FALSE 0)
(define (nonzero? val) (not (zero? val)))

(define (dbus-free data) (if #f #f))

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

;;(define dbus-guard (make-guardian))
(define *dbus-guard-list* '())
(define (dbus-guard item)
  (set! *dbus-guard-list* (cons item *dbus-guard-list*)))
(define (dbus-release item)
  (if (not (member item *dbus-guard-list*)) (error "dbus/epoll code bug"))
  (set! *dbus-guard-list* (delete item dbus-guard-list)))

(define (dispatch-status connection ~status data)
  (display "dispatch-status called\n")
  (let ((status (wrap-DBusDispatchStatus ~status))
	)
    (if (eq? status 'DBUS_DISPATCH_DATA_REMAINS)
	#f)
    #f))

(define (add-watch ~watch data)
  ;; design is broken because one fd may be associated with multiple watches
  (let* ((watch (make-DBusWatch* ~watch))
	 (muxfd (ffi:pointer-address data))
	 (addfd (dbus_watch_get_fd watch))
	 (flags (dbus_watch_get_flags watch))
	 (events (if (nonzero? (dbus_watch_get_enabled watch))
		     (dbus-watch-flags->epoll-events flags)
		     0))
	 (ev (make-struct-epoll_event)))
    (when #t
      (sf "\nadd-watch  ~S  ~S  ...\n" watch data)
      (sf "  addfd: ~S\n" addfd)
      (sf "  enabled: ~S\n" (dbus_watch_get_enabled watch))
      (sf "  flags ~S -> events: ~S\n" flags events))
    
    (dbus-guard-event ev)
    (fh-object-set! ev 'events events)
    (fh-object-set! ev 'data 'ptr (fh-object-ref watch))
    (if (nonzero? events)		; <== NEEDED because broken design
	(epoll_ctl muxfd (EPOLL '_CTL_ADD) addfd (pointer-to ev)))
    ;;
    (dbus_watch_set_data watch (pointer-to ev) dbus-free)
    TRUE))

(define (remove-watch ~watch data)
  (let* ((watch (make-DBusWatch* ~watch))
	 (muxfd (ffi:pointer-address data))
	 (delfd (dbus_watch_get_fd watch))
	 (ev (ffi:pointer->scm (dbus_watch_get_data watch)))
	 )
    (when #t
      (sf "\nrem-watch  ~S  ~S\n" watch data)
      )
    ;;(epoll_ctl muxfd (EPOLL '_CTL_DEL) delfd (pointer-to ev))
    (dbus-release-event ev)
    (if #f #f)))

(define (watch-toggled ~watch data)
  (let* ((watch (make-DBusWatch* ~watch))
	 (muxfd (ffi:pointer-address data))
	 (flags (dbus_watch_get_flags watch))
	 (events (if (nonzero? (dbus_watch_get_enabled watch))
		     (dbus-watch-flags->epoll-events flags)
		     0))
	 (ev (ffi:pointer->scm (dbus_watch_get_data watch))))
    (when #t
      (sf "watch-tog  ~S  ~S ...\n" watch data)
      (sf "  enabled: ~S\n" (dbus_watch_get_enabled watch)))
    (fh-object-set! ev 'events events)
    ;; Do I need the following to just change the events?
    ;;(epoll_ctl muxfd (EPOLL '_CTL_MOD) addfd (pointer-to ev))
    (if #f #f)))

;; timeout is DBusTimeout
(define add-timeout
  (lambda (timeout data)
    (display "add-tmout called\n")
    TRUE))

(define remove-timeout
  (lambda (timeout data)
    (display "rem-tmout called\n")
    (if #f #f)))

(define timeout-toggled
  (lambda (timeout data)
    (display "tmout-tog called\n")
    (if #f #f)))

;; This sets up capability to make runtime-sized vectors and use
;; @code{pointer-to} cast for function args.  See @code{epoll_wait} below.
(define-fh-unsized-vector-type struct-epoll_event-vec struct-epoll_event-desc
  struct-epoll_event-vec? make-struct-epoll_event-vec)
(fh-ref<->deref! struct-epoll_event* make-struct-epoll_event*
		 struct-epoll_event-vec #f)

(define (filter-func c m data)
  (sf "filter-func called ...\n  iface: ~S\n member: ~S\n   path: ~S\n"
      (ffi:pointer->string (dbus_message_get_interface m))
      (ffi:pointer->string (dbus_message_get_member m))
      (ffi:pointer->string (dbus_message_get_path m)))
  (DBUS 'HANDLER_RESULT_HANDLED))

(define (my-main-loop connection)
  (let* ((muxfd (epoll_create 1))
	 (data (ffi:make-pointer muxfd))
	 (max-events 10)
	 (eventv (make-struct-epoll_event-vec max-events)))
    ;; Set up MT locks, and mainloop hook functions.
    (dbus_threads_init_default)
    (dbus_connection_set_dispatch_status_function
     connection dispatch-status data dbus-free)
    (dbus_connection_set_watch_functions
     connection add-watch remove-watch watch-toggled data dbus-free)
    (dbus_connection_set_timeout_functions
     connection add-timeout remove-timeout timeout-toggled data dbus-free)
    ;; These are in dbus list archive 008859.html.
    (dbus_connection_add_filter connection filter-func NULL NULL)
    (dbus_bus_add_match connection "type='signal'" NULL)
    (dbus_bus_add_match connection "type='method_call'" NULL)
    ;;
    (let loop ()
      (let ((n (epoll_wait muxfd (pointer-to eventv) max-events -1)))
	(sf "\nloop n=~S\n" n)
	(let iter ((i 0))
	  (unless (>= i n)
	    (let* ((event (fh-object-ref eventv i))
		   (events (bytestructure-ref event 'events))
		   (flags (epoll-events->dbus-watch-flags events))
		   (data-ptr (bytestructure-ref event 'data 'ptr))
		   (watch (make-DBusWatch* data-ptr))
		   )
	      (sf " i=~S ...\n" i)
	      (sf "  epoll-events=~S => dbus-watch-flags=~S\n" events flags)
	      (sf "  watch=~S\n" watch)
	      (sf "  watch fd=~S\n" (dbus_watch_get_fd watch))
	      (while (equal? FALSE (dbus_watch_handle watch flags)) (sleep 1))
	      (dbus_connection_ref connection)
	      (while (eq? 'DBUS_DISPATCH_DATA_REMAINS
			  (dbus_connection_get_dispatch_status connection))
		(sf "  dispatch ...\n")
		(dbus_connection_dispatch connection))
	      (dbus_connection_unref connection)
	      (sleep 1)
	      #t)
	    (iter (1+ i)))))
      (loop))
    ;;
    (close-fdes muxfd)))

;; === main ==========================

(define error (make-DBusError))
(dbus_error_init (pointer-to error))

(define conn (dbus_bus_get 'DBUS_BUS_SESSION (pointer-to error)))
(check-error error)
(sf "conn=~S\n" (ffi:pointer->string (dbus_bus_get_unique_name conn)))

(my-main-loop conn) ;; later in a dedicated thread
(sleep 3)

;; --- last line ---
