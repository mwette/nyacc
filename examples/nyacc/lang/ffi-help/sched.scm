;; sched.scm - scheduler for dbus

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

(define-module (sched)
  #:export (make-scheduler
	    schedule-event
	    cancel-events/data
	    earliest-event-time
	    exec-schedule
	    t+us t< t>
	    )
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  )

(use-modules (ice-9 pretty-print))
(define pp pretty-print)
(define (sf fmt . args) (apply simple-format #t fmt args))
(use-modules (ice-9 format))
(define (ff fmt . args) (apply format #t fmt args))

(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu))

;; time is pair: (sec . usec)
(define-record-type sch-event
  (make-sch-event next prev time proc data)
  sched-evt?
  (next sch-ev-next set-sch-ev-next!)
  (prev sch-ev-prev set-sch-ev-prev!)
  (time sch-ev-time set-sch-ev-time!)
  (proc sch-ev-proc set-sch-ev-proc!)
  (data sch-ev-data set-sch-ev-data!)
  )

(set-record-type-printer! sch-event
  (lambda (evt port)
    (let ((evt-addr (object-address evt))
	  (nxt-addr (object-address (sch-ev-next evt)))
	  (prv-addr (object-address (sch-ev-prev evt))))
      (display "#<sch-event" port)
      (format port " 0x~x" evt-addr)
      (format port " @~s"(sch-ev-time evt))
      (display ">" port))))
			  
;; the schedule datatype
(define-record-type sch-sched
  (make-sched todo free lock)
  sched?
  (todo sched-todo set-sched-todo!)	; pending events
  (free sched-free set-sched-free!)	; free list
  (lock sched-lock)			; lock
  )

(define *sch-fence* '(1999999999 . 999999))

;; @deffn {Procedure} make-scheduler [size]
;; Create a scheduler.  The optional size is the initial number of events
;; on the free list.  Events are reused to avoid heavy GC action.  Not sure
;; that is needed.  Scheduling events and execution are protected by a
;; mutex.
;; @end deffn
(define* (make-scheduler #:optional (size 5))
  (if (negative? size) (error "size too small"))
  (let* ((todo (make-sch-event #f #f *sch-fence* #f #f))
	 (free (make-sch-event #f #f #f #f size))
	 (lock (make-mutex)))
    (let iter ((prev free) (n (1- size)))
      (set-sch-ev-next! prev (make-sch-event #f prev #f #f n))
      (if (zero? n) prev
	  (iter (sch-ev-next prev) (1- n))))
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
  (and=> (sched-lock sch) lock-mutex)
  (let ((ev (sched-free sch))
	(todo (sched-todo sch)))
    (if (not ev) (error "free list exhausted"))
    (set-sched-free! sch (sch-ev-next ev))
    (set-sch-ev-time! ev time)
    (set-sch-ev-proc! ev proc)
    (set-sch-ev-data! ev data)
    (let iter ((prev #f) (next (sched-todo sch)))
      (cond
       ((t> time (sch-ev-time next))
	(iter next (sch-ev-next next)))
       (else
	(set-sch-ev-next! ev next)
	(set-sch-ev-prev! ev prev)
	(if (not prev) (set-sched-todo! sch ev)))))
    (and=> (sched-lock sch) unlock-mutex)
    ev))

;; @deffn {Procedure} cancel-events/data sch data
;; Cancel all events with handle @var{data}.
;; @end deffn
(define (cancel-events/data sch data)
  (and=> (sched-lock sch) lock-mutex)
  (let iter ((evt (sched-todo sch)))
    (when (and (sch-ev-next evt) (equal? data (sch-ev-data evt)))
      (let ((next (sch-ev-next evt))
	    (prev (sch-ev-prev evt)))
	;; remove the event from the todo list
	(if next (set-sch-ev-prev! next prev))
	(if prev (set-sch-ev-next! prev next)
	    (set-sched-todo! sch next))
	;; add to free list and clean up
	(set-sch-ev-next! evt (sched-free sch))
	(set-sched-free! sch evt)
	(set-sch-ev-time! evt #f)
	(set-sch-ev-prev! evt #f)
	(set-sch-ev-proc! evt #f)
	(set-sch-ev-data! evt #f)
	(iter next))))
  (and=> (sched-lock sch) unlock-mutex)
  (if #f #f))

;; @deffn {Procedure} earliest-event-time sched => time
;; return the earliest event time
;; @end deffn
(define (earliest-event-time sch)
  (and=> (sched-lock sch) lock-mutex)
  (let ((time (sch-ev-time (sched-todo sch))))
    (and=> (sched-lock sch) unlock-mutex)
    time))

;; @deffn {Procedure} exec-schedule sched time
;; Execute events scheduled up to and including @var{time}.
;; The schedule is unlocked during each event execution so that
;; new events can be scheduled.
;; @end deffn
(define (exec-schedule sch time)
  (let ((lock (sched-lock sch)))
    (and=> lock lock-mutex)
    (let iter ((evt (sched-todo sch)))
      (when (t> time (sch-ev-time evt))
	(let ((next (sch-ev-next evt)))
	  ;; remove the event from the todo list
	  (set-sched-todo! sch next)
	  ;; execute the event
	  (when (sch-ev-proc evt)
	    (and=> lock unlock-mutex)
	    ((sch-ev-proc evt) sch (sch-ev-data evt))
	    (and=> lock lock-mutex))
	  ;; update the todo and free list, cleaning the event just executed
	  (set-sch-ev-prev! (sched-free sch) evt)
	  (set-sch-ev-time! evt #f)
	  (set-sch-ev-next! evt (sched-free sch))
	  (set-sch-ev-prev! evt #f)
	  (set-sch-ev-proc! evt #f)
	  (set-sch-ev-data! evt #f)
	  (set-sched-free! sch evt)
	  (iter next))))
    (and=> lock unlock-mutex))
  (if #f #f))

(define (sch-print sch)
  (sf "  todo:\n")
  (let iter ((evt (sched-todo sch)))
    (when evt
      (sf "  ~S ~S ~S\n" (sch-ev-time evt) (sch-ev-proc evt) (sch-ev-data evt))
      (iter (sch-ev-next evt))))
  (sf "  free:\n")
  (let iter ((evt (sched-free sch)))
    (when evt
      (sf "  ~S ~S ~S\n" (sch-ev-time evt) (sch-ev-proc evt) (sch-ev-data evt))
      (iter (sch-ev-next evt))))
  )

(define (t+us time us)
  (let* ((us (+ (cdr time) us))
	 (t (cons (car time) us)))
    (if (> us 999999)
	(cons (1+ (car t)) (- (cdr t) 1000000))
	t)))

#|
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
  (sch-print sch1)
  )
|#


;; --- last line ---
