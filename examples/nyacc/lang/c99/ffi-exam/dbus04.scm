;; dbus04.scm - dbus in another thread : sync calls
;; https://lists.freedesktop.org/archives/dbus/2007-October/008859.html
;; https://stackoverflow.com/questions/9378593/dbuswatch-and-dbustimeout-examples

(use-modules (ice-9 pretty-print))
(use-modules (ice-9 format))
(define (sf fmt . args) (apply simple-format #t fmt args))
(define (ff fmt . args) (apply format #t fmt args))

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

(define d-a (@@ (bytestructures guile struct) debug-alignment))

;; ====================================
;; use case 01: want to queue in any thread, poll for ready

(define TRUE 1)
(define FALSE 0)
(define (nonzero? val) (not (zero? val)))

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

(define-record-type dbus-data
  (make-dbus-data fd ev wv)
  dbus-data?
  (fd dbus-data-fd)			; epoll fd, if in epoll set
  (ev dbus-data-ev)			; epoll event
  (wv dbus-data-wv set-dbus-data-wv!)	; watch vector
  )

(define dbus-data-maxw 3)

(define (scm->addr scm)
  (ffi:pointer-address (ffi:scm->pointer scm)))
(define (addr->scm addr)
  (ffi:pointer->scm (ffi:make-pointer addr)))

(define *dbus-fd-dict* (make-hash-table 31))

;; (define (dbus-lookup-fd goes here 
;; if not found add a blank entry
(define (dbus-lookup-fd fd)
  (or (hashv-ref *dbus-fd-dict* fd)
      (let* ((event (make-struct-epoll_event))
	     (ddent (make-dbus-data fd event (make-vector dbus-data-maxw #f))))
	(hashv-set! *dbus-fd-dict* fd ddent)
	(fh-object-set! event 'data 'ptr (scm->addr ddent))
	(ff "new ddent@ 0x~x   w/ event ~s\n" (scm->addr ddent) event)
	ddent)))

(define (find-wv-slot wv)
  (let iter ((i 0))
    (cond
     ((= i dbus-data-maxw) -1)
     ((vector-ref wv i) (iter (1+ i)))
     (else i))))

(define (dbus-data-watched? ddent)
  (= 0 (find-wv-slot (dbus-data-wv ddent))))

(define (dbus-data-watchless? ddent)
  (not (dbus-data-watched? ddent)))
       
(define (dbus-fd-watchless? fd)
  (not (and=> (hashv-ref *dbus-fd-dict* fd) dbus-data-watched?)))
  
(define (dbus-data-free ddent)
  (if #f #f))

(define (dispatch-status connection ~status data)
  (display "dispatch-status called\n")
  (let ((status (wrap-DBusDispatchStatus ~status))
	)
    (if (eq? status 'DBUS_DISPATCH_DATA_REMAINS)
	#f)
    #f))

;; (define (add-watch ...) goes here
(define (add-watch ~watch data)
  (let* ((watch (make-DBusWatch* ~watch))
	 (muxfd (ffi:pointer-address data))
	 (addfd (dbus_watch_get_unix_fd watch))
	 (flags (dbus_watch_get_flags watch))
	 (ddent (dbus-lookup-fd muxfd))
	 (event (dbus-data-ev ddent))	 )
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
      
    (sf "    ~S" (bytestructure-bytevector (fh-object-val event))) ;; add \n !
    ;; Set watches based on flags.
    (let* ((wv (dbus-data-wv ddent)) (wx (find-wv-slot wv)))
      (sf " @ wx=~S\n" wx)
      (if (negative? wx) (error "max exceeded")
	  (vector-set! wv wx watch)))
    
    TRUE))

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

(define (watch-toggled ~watch data)
  (let* ((watch (make-DBusWatch* ~watch))
	 (muxfd (ffi:pointer-address data))
	 (flags (dbus_watch_get_flags watch))
	 )
    (when #t
      (sf "watch-tog  ~S  ~S ...\n" watch data)
      (sf "  enabled: ~S\n" (dbus_watch_get_enabled watch)))
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
	 (max-events 2)
	 (eventv (make-struct-epoll_event-vec max-events))
	 (eventp (pointer-to eventv)))
    ;; Set up MT locks, and mainloop hook functions.
    (dbus_threads_init_default)
    (dbus_connection_set_dispatch_status_function
     connection dispatch-status data dbus-data-free)
    (dbus_connection_set_watch_functions
     connection add-watch remove-watch watch-toggled data dbus-data-free)
    (dbus_connection_set_timeout_functions
     connection add-timeout remove-timeout timeout-toggled data dbus-data-free)
    ;; These are in dbus list archive 008859.html.
    (dbus_connection_add_filter connection filter-func NULL NULL)
    (dbus_bus_add_match connection "type='signal'" NULL)
    (dbus_bus_add_match connection "type='method_call'" NULL)
    ;;
    (let loop ()
      (display "waiting ...\n")
      (let iter ((i 0) (n (epoll_wait muxfd eventp max-events -1)))
	(unless (= i n)
	  (sf "\n  i=~S ...\n" i)
	  (let* ((event (fh-object-ref eventv i))
		 (events (bytestructure-ref event 'events))
		 (data-ptr (bytestructure-ref event 'data 'ptr))
		 (ddent (addr->scm data-ptr))
		 (flags (epoll-events->dbus-watch-flags events)))
	    ;;(ff "a  event = ~s\n" (bytestructure-bytevector event))
	    (ff "  events=0x~x => flags=0x~x\n" events flags)
	    ;;(sf "  ddent=~S\n" ddent)
	    ;;(sf "  watches=~S\n" watches)
	    (vector-for-each
	     (lambda (ix watch)
	       (when (and watch
			  (nonzero? (logand flags (dbus_watch_get_flags watch))))
		 (while (equal? FALSE (dbus_watch_handle watch flags)) (sleep 1))
		 (dbus_connection_ref connection)
		 (while (eq? 'DBUS_DISPATCH_DATA_REMAINS
			     (dbus_connection_get_dispatch_status connection))
		   (sf "    dispatch ...\n")
		   (dbus_connection_dispatch connection))
		 (dbus_connection_unref connection)))
	     (dbus-data-wv ddent)))
	  (iter (1+ i) n)))
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
