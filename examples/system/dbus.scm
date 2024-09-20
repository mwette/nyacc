;;; system/dbus.scm - dbus module, on top of (ffi dbus)

;; Copyright (C) 2018,2024 Matthew Wette
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

;;; Notes:

;; This is a work in progress for Guile dbus based on libdbus.

;; See:
;;   https://lists.freedesktop.org/archives/dbus/2007-October/008859.html
;;   https://stackoverflow.com/questions/9378593/
;;                                     dbuswatch-and-dbustimeout-examples

;; Marshalling and de-marshalling strings requires pointer.
;;   char *str_recv, *str_send = "hello";
;;   dbus_message_iter_get_basic(&iter, DBUS_TYPE_STRING, &str_recv);
;;   dbus_message_iter_append_basic(&iter, DBUS_TYPE_STRING, &str_send);

;; Variables names starting with `&' represent pointers to allocated
;; memory as in
;;   char *str = "hello";
;;   foo(&str);

;;; Code:

(define-module (system dbus)
  #:export (spawn-dbus-mainloop

            TRUE FALSE

            dbus-version
            dbus-bus-get-unique-name
            dbus-message-get-sender
            dbus-error

            ;; utils:
            read-dbus-val
            get-dbus-message-args
            nonzero?
            TRUE FALSE
            ;;
            ;;
            make-DBusMessageIter&
            dbus-message-type
            dbus-request-name-reply
            make-dbus-string
            make-dbus-pointer
            )
  #:use-module (ffi epoll)
  #:use-module (ffi dbus)
  #:use-module (nyacc foreign cdata)
  #:use-module (system foreign)

  #:use-module (ice-9 threads)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-43))

(use-modules (ice-9 format))
(define (ff fmt . args) (apply format #t fmt args))
(define (sf fmt . args) (apply simple-format #t fmt args))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))

(define *trace* (make-parameter #f))

(define (ensure-pointer maybe-ptr)
  (if (pointer? maybe-ptr) maybe-ptr (make-pointer maybe-ptr)))

(define (ensure-address maybe-ptr)
  (if (pointer? maybe-ptr) (pointer-address maybe-ptr) maybe-ptr))

(define (!0 arg) (not (eqv? 0 arg)))

;; ====================================

(define TRUE 1)
(define FALSE 0)

(define int* (cpointer (cbase 'int)))

(define (dbus-version)
  (let ((maj (make-cdata int*)) (min (make-cdata int*)) (mic (make-cdata int*)))
    (dbus_get_version (cdata& maj) (cdata& min) (cdata& mic))
    (simple-format #f "~A.~A.~A"
                   (cdata-ref maj)
                   (cdata-ref min)
                   (cdata-ref mic))))

(define (dbus-bus-get-unique-name conn)
  (if (*trace*) (sferr "get-unique-name ~s\n" conn))
  (pointer->string (dbus_bus_get_unique_name conn)))

(define (dbus-message-get-sender conn)
  (if (*trace*) (sferr "get-sender ~s\n" conn))
  (pointer->string (dbus_message_get_sender conn)))

;; @deffn {Procedure} dbus-error error => #f|string
;; If @var{error} (a @code{DBusError} value) represents an error return
;; the error string.  Otherwise return @code{#f}.
;; @end deffn
(define (dbus-error error)
  (and (!0 (dbus_error_is_set (cdata& error)))
       (pointer->string (ensure-pointer (cdata-ref error 'message)))))

(define (get-bval &iter key)
  (let* ((bval (make-cdata DBusBasicValue)))
    (dbus_message_iter_get_basic &iter (cdata& bval))
    (cdata-ref bval key)))

(define (read-dbus-val &iter)
  (if (*trace*) (sferr "read-bus-val ~s\n" &iter))
  (case (dbus_message_iter_get_arg_type &iter)
    ;;((0) (if #f #f))                              ; 0 - invalid
    ((0) '())                                       ; 0 - invalid
    ((121) (get-bval &iter 'byt))                   ; y - byte
    ((98) (not (zero? (get-bval &iter 'bool_val)))) ; b - boolean
    ((110) (get-bval &iter 'i16))                   ; n - int16
    ((113) (get-bval &iter 'u16))                   ; q - uint16
    ((105) (get-bval &iter 'i32))                   ; i - int32
    ((117) (get-bval &iter 'u32))                   ; u - uint32
    ((120) (get-bval &iter 'i64))                   ; x - int64
    ((116) (get-bval &iter 'u32))                   ; t - uint64
    ((100) (get-bval &iter 'dbl))                   ; d - double
    ((115 111 103)                                  ; s, o, g
     (pointer->string (ensure-pointer (get-bval &iter 'str))))
    ((104) (get-bval &iter 'fd))        ; h - unix fd
    ((97)                               ; a - array
     (let* ((sub-iter (make-cdata DBusMessageIter))
            (&sub-iter (cdata& sub-iter)))
       (dbus_message_iter_recurse &iter &sub-iter)
       (let loop ()
         (cons (read-dbus-val &sub-iter)
               (if (zero? (dbus_message_iter_next &sub-iter)) '() (loop))))))
    ((118)                              ; v - variant (boxed value)
     (let* ((sub-iter (make-cdata DBusMessageIter))
            (&sub-iter (cdata& sub-iter)))
       (dbus_message_iter_recurse &iter &sub-iter)
       (read-dbus-val &sub-iter)))
    ((114) (error "not defined: r"))    ; r - struct
    ((101)                              ; e - dict entry
     (let* ((sub-iter (make-cdata DBusMessageIter))
            (&sub-iter (cdata& sub-iter)))
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
  (if (*trace*) (sferr "get-dbus-message-args ~s\n" msg))
  (let* ((iter (make-cdata DBusMessageIter))
         (&iter (cdata& iter)))
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
        ;;(sferr "dbus-message-type ~s\n" ival)
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
        ;;(sferr "dbus-req-name-reply ~s\n" ival)
        (case ival
          ((1) 'PRIMARY_OWNER)
          ((2) 'IN_QUEUE)
          ((3) 'REPLY_EXISTS)
          ((4) 'ALREADY_OWNER)
          (else #f)))
      (lambda (ival) ival)))

(define (make-DBusMessageIter&) (cdata& (make-cdata DBusMessageIter)))

;; === scheduler ======================

;; time is pair: (sec . usec)
(define-record-type bus-event
  (make-bus-event next prev time proc data)
  bus-evt?
  (next bus-ev-next set-bus-ev-next!)
  (prev bus-ev-prev set-bus-ev-prev!)
  (time bus-ev-time set-bus-ev-time!)
  (proc bus-ev-proc set-bus-ev-proc!)
  (data bus-ev-data set-bus-ev-data!))

(set-record-type-printer! bus-event
  (lambda (evt port)
    (let ((evt-addr (object-address evt))
          (nxt-addr (object-address (bus-ev-next evt)))
          (prv-addr (object-address (bus-ev-prev evt))))
      (display "#<bus-event" port)
      (format port " 0x~x" evt-addr)
      (format port " @~s"(bus-ev-time evt))
      (display ">" port))))

(define-record-type bus-sched
  (make-sched todo free lock)
  sched?
  (todo sched-todo set-sched-todo!)     ; pending events
  (free sched-free set-sched-free!)     ; free list
  (lock sched-lock))                    ; lock

(define *fence* '(1999999999 . 999999))

;; @deffn {Procedure} make-scheduler [size]
;; Create a scheduler.  The optional size is the initial number of events
;; on the free list.  Events are reused to avoid heavy GC action.  Not sure
;; that is needed.  Scheduling events and execution are protected by a
;; mutex.
;; @end deffn
(define* (make-scheduler #:optional (size 5))
  (if (negative? size) (error "size too small"))
  (let* ((todo (make-bus-event #f #f *fence* #f #f))
         (free (make-bus-event #f #f #f #f size))
         (lock (make-mutex)))
    (let iter ((prev free) (n (1- size)))
      (set-bus-ev-next! prev (make-bus-event #f prev #f #f n))
      (if (zero? n) prev
          (iter (bus-ev-next prev) (1- n))))
    (make-sched todo free lock)))

(define (t> a b)
  (cond
   ((> (car a) (car b)) #t)
   ((< (car a) (car b)) #f)
   ((> (cdr a) (cdr b)) #t)
   (else #f)))

(define (t< a b) (t> b a))

;; @deffn {Procedure} schedule-event sched time proc data
;; Schedule an event.  Manipulation of the schedule is protected by
;; lock and unlock.  The argument @var{time} is a pair of seconds
;; and micro-seconds as returned by @code{gettimeofday}.
;; @end deffn
(define (schedule-event sch time proc data)
  (if (*trace*) (sferr "sched-event \n" ))
  (and=> (sched-lock sch) lock-mutex)
  (let ((ev (sched-free sch))
        (todo (sched-todo sch)))
    (if (not ev) (error "free list exhausted"))
    (set-sched-free! sch (bus-ev-next ev))
    (set-bus-ev-time! ev time)
    (set-bus-ev-proc! ev proc)
    (set-bus-ev-data! ev data)
    (let iter ((prev #f) (next (sched-todo sch)))
      (cond
       ((t> time (bus-ev-time next))
        (iter next (bus-ev-next next)))
       (else
        (set-bus-ev-next! ev next)
        (set-bus-ev-prev! ev prev)
        (if (not prev) (set-sched-todo! sch ev)))))
    (and=> (sched-lock sch) unlock-mutex)
    ev))

;; @deffn {Procedure} cancel-events/data sch data
;; Cancel all events with handle @var{data}.
;; @end deffn
(define (cancel-events/data sch data)
  (if (*trace*) (sferr "cancel-event \n" ))
  (and=> (sched-lock sch) lock-mutex)
  (let iter ((evt (sched-todo sch)))
    (when (and (bus-ev-next evt) (equal? data (bus-ev-data evt)))
      (let ((next (bus-ev-next evt))
            (prev (bus-ev-prev evt)))
        ;; remove the event from the todo list
        (if next (set-bus-ev-prev! next prev))
        (if prev (set-bus-ev-next! prev next)
            (set-sched-todo! sch next))
        ;; add to free list and clean up
        (set-bus-ev-next! evt (sched-free sch))
        (set-sched-free! sch evt)
        (set-bus-ev-time! evt #f)
        (set-bus-ev-prev! evt #f)
        (set-bus-ev-proc! evt #f)
        (set-bus-ev-data! evt #f)
        (iter next))))
  (and=> (sched-lock sch) unlock-mutex)
  (if #f #f))

;; @deffn {Procedure} earliest-event-time sched => time
;; return the earliest event time
;; @end deffn
(define (earliest-event-time sch)
  (and=> (sched-lock sch) lock-mutex)
  (let ((time (bus-ev-time (sched-todo sch))))
    (and=> (sched-lock sch) unlock-mutex)
    time))

;; @deffn {Procedure} exec-schedule sched time
;; Execute events scheduled up to and including @var{time}.
;; The schedule is unlocked during each event execution so that
;; new events can be scheduled.
;; @end deffn
(define (exec-schedule sch time)
  (if (*trace*) (sferr "exec schedule\n"))
  (let ((lock (sched-lock sch)))
    (and=> lock lock-mutex)
    (let loop ((evt (sched-todo sch)))
      (when (t> time (bus-ev-time evt))
        (let ((next (bus-ev-next evt)))
          ;; remove the event from the todo list
          (set-sched-todo! sch next)
          ;; execute the event
          (when (bus-ev-proc evt)
            (and=> lock unlock-mutex)
            ((bus-ev-proc evt) sch (bus-ev-data evt))
            (and=> lock lock-mutex))
          ;; update the todo and free list, cleaning the event just executed
          (set-bus-ev-prev! (sched-free sch) evt)
          (set-bus-ev-time! evt #f)
          (set-bus-ev-next! evt (sched-free sch))
          (set-bus-ev-prev! evt #f)
          (set-bus-ev-proc! evt #f)
          (set-bus-ev-data! evt #f)
          (set-sched-free! sch evt)
          (loop next))))
    (and=> lock unlock-mutex))
  (if #f #f))

(define (t+us time us)
  (let* ((us (+ (cdr time) us))
         (t (cons (car time) us)))
    (if (> us 999999)
        (cons (1+ (car t)) (- (cdr t) 1000000))
        t)))


(define (sch-print sch)
  (sf "  todo:\n")
  (let iter ((evt (sched-todo sch)))
    (when evt
      (sf "  ~S ~S ~S\n" (bus-ev-time evt) (bus-ev-proc evt) (bus-ev-data evt))
      (iter (bus-ev-next evt))))
  (sf "  free:\n")
  (let iter ((evt (sched-free sch)))
    (when evt
      (sf "  ~S ~S ~S\n" (bus-ev-time evt) (bus-ev-proc evt) (bus-ev-data evt))
      (iter (bus-ev-next evt)))))

(define-public (test-sched)
  (define (hello sch data)
    (sf "hello ~S\n" data))

  (let* ((t0 (gettimeofday))
         (t1 (t+us t0 1000))
         (t2 (t+us t0 10000))
         (t3 (t+us t0 100000))
         (sch1 (make-scheduler)))
    (sf "t0=~S\n" t0)
    (sf "t0=~S\n" t1)
    (sf "t0=~S\n" t2)
    (sf "t0=~S\n" t3)
    ;;(sch-print sch1)
    (sf "schedule event\n")
    (schedule-event sch1 t2 hello "world")
    (sch-print sch1)
    (sf "exec t1\n")
    (exec-schedule sch1 t1)
    (sch-print sch1)
    (sf "exec t3\n")
    (exec-schedule sch1 t3)
    (sch-print sch1)))


;; === DBus mainloop ==================

;; notes
;; 1) One can call epoll_ctl while epoll_wait is blocked in another thread.
;;    ref: epoll_wait man page
;; 2) It is safe to use scm->pointer and pointer->scm as long as the pointed
;;    object was allocated with scm_gc_malloc().  (Ludo answer to Q on list)

;; Convert mask of DBUS_WATCH to enum EPOLL_EVENTS.
(define (dbus-watch-flags->epoll-events dbus-flags)
  (if (*trace*) (sferr "flags->events\n"))
  (fold
   (lambda (dbus-flag epoll-types)
     (if (not (zero? (logand dbus-flag dbus-flags)))
         (case dbus-flag
           ((1) (logior epoll-types 1))  ; readable
           ((2) (logior epoll-types 4))  ; writable
           ((4) (logior epoll-types 8))  ; error
           ((8) (logior epoll-types 16)) ; hangup
           (else (error "unhandled case")))
         epoll-types))
   0 '(1 2 4 8)))

(define (epoll-events->dbus-watch-flags epoll-events)
  (if (*trace*) (sferr "events->flags\n"))
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

#|
(define (scm->addr scm)
  (pointer-address (scm->pointer scm)))
(define (addr->scm addr)
(pointer->scm (make-pointer addr)))
|#

;; This is the user-data associated with a watch, i.e., an FD to be monitored.
;; Guile needs something equivalent to epoll-OR-kevent.
(define-record-type dbus-data
  (make-dbus-data fd ev wv)
  dbus-data?
  (fd dbus-data-fd)                     ; epoll fd, if in epoll set
  (ev dbus-data-ev)                     ; epoll event or kevent ???
  (wv dbus-data-wv set-dbus-data-wv!))  ; watch vector

(define *dbus-maxw* 3)
(define *dbus-fd-dict* (make-hash-table 31))

;; Lookup the fd in the dbus-data dictionary.  If not found add a blank entry.
(define (dbus-lookup-fd fd)
  (if (*trace*) (sferr "dbus-lookkup-fd ~s\n" fd))
  (or (hashv-ref *dbus-fd-dict* fd)
      (let* ((event (make-cdata struct-epoll_event))
             (ddent (make-dbus-data fd event
                                    (make-vector *dbus-maxw* #f))))
        (hashv-set! *dbus-fd-dict* fd ddent)
        (cdata-set! event (scm->pointer ddent) 'data 'ptr)
        ddent)))

;; Find an available slot in the watch-vector.
(define (find-wv-slot wv)
  (if (*trace*) (sferr "find slot ~s\n" wv))
  (let iter ((i 0) (n (vector-length wv)))
    (cond
     ((= i n) -1)
     ((vector-ref wv i) (iter (1+ i) n))
     (else i))))

;; Is the fd in the dbus-data object being watched?
;; This implmenetation is a kludge.
(define (dbus-data-watched? ddent)
  (if (*trace*) (sferr "dbus-data-watched? \n"))
  (= 0 (find-wv-slot (dbus-data-wv ddent))))

(define (dbus-data-watchless? ddent)
  (if (*trace*) (sferr "dbus-data-watchless? \n"))
  (not (dbus-data-watched? ddent)))

(define (dbus-fd-watchless? fd)
  (if (*trace*) (sferr "dbus-fd-watchless? \n"))
  (not (and=> (hashv-ref *dbus-fd-dict* fd) dbus-data-watched?)))

;; De-allocate the dbus data entry.
(define (dbus-data-free ddent)
  (if (*trace*) (sferr "dbus-data-free\n"))
  (if #f #f))

;; no need implement this with use of the get_dispatch_status/dispatch loop
(define (dispatch-status connection ~status data)
  (if (*trace*) (sferr "dbus-dispatch-status \n"))
  (if #f #f))

;; @deffn {Procedure} add-watch watch data
;; @end deffn
(define (add-watch watch data)
  (if (*trace*) (sferr "add-watch ~s\n" watch))
  (let* ((muxfd (pointer-address data))
         (addfd (dbus_watch_get_unix_fd watch))
         (flags (dbus_watch_get_flags watch))
         (ddent (dbus-lookup-fd muxfd))
         (event (dbus-data-ev ddent)))

    (dbus_watch_set_data watch (scm->pointer ddent) dbus-data-free)

    ;; Set up the indended set of epoll events.
    ;;(sferr "dbus watch enabled => ~s\n" (dbus_watch_get_enabled watch))
    (if (!0 (dbus_watch_get_enabled watch))
        (cdata-set! event
                    (logior (cdata-ref event 'events)
                            (dbus-watch-flags->epoll-events flags))
                     'events))

    ;; If this is the use of this fd, then initialize the ev and add to epoll.
    ;;(sferr "dbus watchless => ~s\n" (dbus-data-watchless? ddent))
    (if (dbus-data-watchless? ddent)
        (epoll_ctl muxfd (EPOLL '_CTL_ADD) addfd (cdata& event)))

    ;; Set watches based on flags.
    (let* ((wv (dbus-data-wv ddent)) (wx (find-wv-slot wv)))
      (if (negative? wx) (error "max exceeded")
          (vector-set! wv wx watch)))

    TRUE))

;; @deffn {Procedure} remove-watch watch data
;; @end deffn
(define (remove-watch watch data)
  (if (*trace*) (sferr "remove-watch\n"))
  (let* ((muxfd (pointer-address data))
         (delfd (dbus_watch_get_unix_fd watch))
         ;;(ddent (dbus_watch_get_data watch))
         (ddent (hashv *dbus-fd-dict* delfd))
         (event (dbus-data-ev ddent))
         (events (cdata-ref event 'events))
         )
    ;; remove watch from ddent and fix events mask.
    ;; if no watches left then remove fd from epoll
    ;;(if (no more watches on this fd)
    ;;    (epoll_ctl muxfd (EPOLL '_CTL_DEL) delfd (scm->pointer ddent)))
    (if #f #f)))

;; @deffn {Procedure} watch-toggled watch data
;; @end deffn
(define (watch-toggled watch data)
  (if (*trace*) (sferr "watch-toggled\n"))
  (let* ((muxfd (pointer-address data))
         (flags (dbus_watch_get_flags watch))
         )
    (when #f
      (sf "watch-tog  ~S  ~S ...\n" watch data)
      (sf "  enabled: ~S\n" (dbus_watch_get_enabled watch)))
    (if #f #f)))

(export add-watch remove-watch watch-toggled dbus-data-free)

(define *dbus-sched* (make-scheduler))

(define (dbus-timeout-handler sch timeout)
  (if (*trace*) (sf "\ntimeout handler called\n"))
  (dbus_timeout_handle timeout))

;; timeout is DBusTimeout
(define (add-timeout timeout data)
  (if (*trace*) (sferr "add-timeout ~s\n" timeout))
  (let* ((tod (gettimeofday))
         (interval (dbus_timeout_get_interval timeout))
         (exp (t+us tod interval)))
    (schedule-event *dbus-sched* exp dbus-timeout-handler timeout)
    (write #\x (pointer->scm data)) ; wake up mainloop
    TRUE))

(define (remove-timeout timeout data)
  (if (*trace*) (sferr "rem-timeout ~s\n" timeout))
  (cancel-events/data *dbus-sched* timeout)
  (if #f #f))

(define (timeout-toggled timeout data)
  (if (*trace*) (sferr "tog-timeout ~s\n" timeout))
  (let ()
    (display "tmout-tog called (to be completed!)\n")
    (if #f #f)))

;; This sets up capability to make runtime-sized vectors and use
;; @code{cdata&} cast for function args.  See @code{epoll_wait} below.
(define-public struct-epoll_event-vec
  (name-ctype 'struct-epoll_event-vec (carray struct-epoll_event 0)))

(define (filter-func c m data)
  (if (*trace*) (sferr "filter-func\n"))
  (when #f
    (sf "filter-func called ...\n  iface : ~S\n  member: ~S\n  path  : ~S\n"
        (pointer->string (dbus_message_get_interface m))
        (pointer->string (dbus_message_get_member m))
        (pointer->string (dbus_message_get_path m))))
  (DBUS 'HANDLER_RESULT_NOT_YET_HANDLED)
  ;;(DBUS 'HANDLER_RESULT_HANDLED)
  )

(define (set-nonblocking! port)
  (fcntl port F_SETFL (logior O_NONBLOCK (fcntl port F_GETFL))))

(define* (my-main-loop connection #:key (max-events 5))
  (define (dispatch-em)
    (while (eq? 'DBUS_DISPATCH_DATA_REMAINS
                (dbus_connection_get_dispatch_status connection))
      (dbus_connection_dispatch connection)))

  (define (handle-watch watch flags)
    (if (*trace*) (sferr "hw: flags=~s\n" flags))
    (let ((flags (logand flags (dbus_watch_get_flags watch))))
      ;; This loop-sleeps while out of memory.
      (while (equal? FALSE (dbus_watch_handle watch flags))
        (sf "SLEEP 1\n")
        (sleep 1))))

  (let* ((muxfd (epoll_create 1))
         (muxpt (make-pointer muxfd))
         (wpipe (pipe)) (wiport (car wpipe)) (woport (cdr wpipe))
         (woport-ptr (scm->pointer woport))
         (eventv (make-cdata struct-epoll_event-vec max-events))
         (eventp (cdata& eventv)))

    ;; Set up wakeup, initiated from handlers.
    (setvbuf woport 'none)
    (set-nonblocking! woport)
    (set-nonblocking! wiport) ;; needed?
    (let ((event (make-cdata struct-epoll_event)))
      (cdata-set! event (EPOLL 'IN) 'events)
      (epoll_ctl muxfd (EPOLL '_CTL_ADD) (port->fdes wiport) (cdata& event)))

    ;; Set up DBus MT locks, and mainloop hook functions.
    (dbus_threads_init_default)
    (dbus_connection_set_dispatch_status_function
     connection dispatch-status muxpt dbus-data-free)
    (dbus_connection_set_watch_functions
     connection add-watch remove-watch watch-toggled muxpt dbus-data-free)
    (dbus_connection_set_timeout_functions
     connection add-timeout remove-timeout timeout-toggled woport-ptr NULL)

    ;; These are in dbus list archive 008859.html.
    (dbus_connection_add_filter connection filter-func NULL NULL)
    ;;(dbus_bus_add_match connection "type='signal'" NULL)
    (dbus_bus_add_match connection "type='method_call'" NULL)

    (let loop ()
      (let iter ((i 0) (n (epoll_wait muxfd eventp max-events -1)))

        ;; timeouts use (pair (gettimeofday)) => (sec . usec)
        ;; FIXME FIXME FIXME two iters
        (let iter ((tod (gettimeofday))
                    (nxt (earliest-event-time *dbus-sched*)))
          (when (t< tod nxt)
            (usleep 50000)
            (exec-schedule *dbus-sched* tod)
            (iter (gettimeofday) (earliest-event-time *dbus-sched*))))

        ;; events: wake-up or watches
        (unless (= i n)
          (let* ((event (cdata-ref eventv i))
                 (events (cdata-ref event 'events))
                 (data-ptr (cdata-ref event 'data 'ptr)))
            (if (*trace*) (sferr "ddataptr=~s\n" data-ptr))
            (cond
             ((NULL? data-ptr)
              (read-char wiport))
             (else
              ;;(sferr "   m=~s\n" data-ptr)
              ;;(sferr "   n=~s\n" (pointer->scm data-ptr))
              (let* ((flags (epoll-events->dbus-watch-flags events))
                     (ddent (pointer->scm data-ptr)))
                (if (*trace*) (sferr " 1 flags=~s\n" flags))
                ;;(sferr "   vec-for-each ...\n")
                (vector-for-each
                 (lambda (ix watch)
                   (if (*trace*) (sferr "     ix=~s watch=~s\n" ix watch))
                   (when watch
                     (handle-watch watch flags)
                     (dbus_connection_ref connection)
                     (dispatch-em)
                     (dbus_connection_unref connection)))
                 (dbus-data-wv ddent))))))
          (iter (1+ i) n)))
      (usleep 100)
      (loop))
    (close-fdes muxfd)))

(define (spawn-dbus-mainloop service)
  (let* ((bus-id
          (case service
            ((session) 'DBUS_BUS_SESSION)
            ((system) 'DBUS_BUS_SYSTEM)
            (else (error "bad bus id"))))
         (error
          (let ((error (make-cdata DBusError)))
            (dbus_error_init (cdata& error))
            error))
         (conn
          (let ((conn (dbus_bus_get bus-id (cdata& error))))
            (dbus-error error)
            conn)))
    (call-with-new-thread (lambda () (my-main-loop conn)))
    (sleep 1)
    conn))

;; --- last line ---
