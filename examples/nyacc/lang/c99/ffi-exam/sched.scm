;; scheduler

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
	  (prv-addr (object-address (sch-ev-prev evt)))
	  )
      (display "#<sch-event" port)
      (format port " 0x~x" evt-addr)
      (format port " @~s"(sch-ev-time evt))
      (display ">" port))))
			  

;; schedule
(define-record-type sched
  (make-sched todo free)
  sched?
  (todo sched-todo set-sched-todo!)	; pending events
  (free sched-free set-sched-free!)	; free list
  )

(define *sch-fence* '(1999999999 . 999999))

(define* (make-scheduler #:optional (size 5))
  (if (negative? size) (error "size too small"))
  (let* ((todo (make-sch-event #f #f *sch-fence* #f #f))
	 (free (make-sch-event #f #f #f #f size)))
    (let iter ((prev free) (n (1- size)))
      (set-sch-ev-next! prev (make-sch-event #f prev #f #f n))
      (if (zero? n) prev
	  (iter (sch-ev-next prev) (1- n))))
    (make-sched todo free)))

(define (t> a b)
  (cond
   ((> (car a) (car b)) #t)
   ((< (car a) (car b)) #f)
   ((> (cdr a) (cdr b)) #t)
   (else #f)))

;; proc can be #f
(define (sch-sched-event sch time proc data)
  (let ((ev (sched-free sch))
	(todo (sched-todo sch))
	)
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
    ev))

;; sch-exec run itmes up to and including time
(define (sch-exec sch time)
  (let iter ((evt (sched-todo sch)))
    (when (t> time (sch-ev-time evt))
      (let ((next (sch-ev-next evt)))
	(set-sched-todo! sch next) ; remove from todo list
	;;(sf "exec ~S\n" evt)		  ; execute
	(if (sch-ev-proc evt) ((sch-ev-proc evt) sch (sch-ev-data evt)))
	(set-sch-ev-prev! (sched-free sch) evt)
	;;(set-sch-ev-time! evt #f)
	(set-sch-ev-next! evt (sched-free sch))
	(set-sch-ev-prev! evt #f)
	(set-sch-ev-proc! evt #f)
	(set-sch-ev-data! evt #f)
	(set-sched-free! sch evt)
	(iter next))))
  (if #f #f))
(define (sch-exec/lock sch time) #f)

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

(define sch1 (make-scheduler))

(define (hello sch data)
  (sf "hello ~S\n" data))

(define (t+us time us)
  (let* ((us (+ (cdr time) us))
	 (t (cons (car time) us)))
    (if (> us 999999)
	(cons (1+ (car t)) (- (cdr t) 1000000))
	t)))

(let* ((t0 (gettimeofday))
       (t1 (t+us t0 1000))
       (t2 (t+us t0 10000))
       (t3 (t+us t0 100000)))
  (sf "t0=~S\n" t0)
  (sf "t0=~S\n" t1)
  (sf "t0=~S\n" t2)
  (sf "t0=~S\n" t3)
  ;;(sch-print sch1)
  (sf "schedule event\n")
  (sch-sched-event sch1 t2 hello "world")
  (sch-print sch1)
  (sf "exec t1\n")
  (sch-exec sch1 t1)
  (sch-print sch1)
  (sf "exec t3\n")
  (sch-exec sch1 t3)
  (sch-print sch1)
  )


;; --- last line ---
