;; gdk2-ex02.scm

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

(use-modules (ffi gdk2))
;;(use-modules ((system foreign) #:prefix ffi:))
;;(use-modules (bytestructures guile))
(use-modules (system foreign cdata))

(define (sf fmt . args) (apply simple-format #t fmt args))

(gdk_init NULL NULL)

(define win
  (let ((attr (make-GdkWindowAttr)))
    (cdata-set! attr (apply logior (map gdk-symval '(GDK_EXPOSE))) 'event_mask)
    (cdata-set! attr 400 'width)
    (cdata-set! attr 300 'height)
    (cdata-set! attr (gdk-symval 'GDK_INPUT_OUTPUT) 'wclass)
    (cdata-set! attr (gdk-symval 'GDK_WINDOW_TOPLEVEL) 'window_type)
    (gdk_window_new NULL (cdata& attr) 0)))

(gdk_window_show win)

(define (gdk-event-type evt)
  (if (zero? (cdata-ref evt)) 'GDK_NOTHING
      (case (cdata-ref evt '* 'type)
        ((0) 'GDK_DELETE)
        ((1) 'GDK_DESTROY)
        ((2) 'GDK_EXPOSE)
        ((3) 'GDK_MOTION_NOTIFY)
        ((4) 'GDK_BUTTON_PRESS)
        ((5) 'GDK_2BUTTON_PRESS)
        ((6) 'GDK_3BUTTON_PRESS)
        ((7) 'GDK_BUTTON_RELEASE)
        ((8) 'GDK_KEY_PRESS)
        ((9) 'GDK_KEY_RELEASE)
        ((10) 'GDK_ENTER_NOTIFY)
        ((11) 'GDK_LEAVE_NOTIFY)
        ((12) 'GDK_FOCUS_CHANGE)
        ((13) 'GDK_CONFIGURE)
        ((14) 'GDK_MAP)
        ((15) 'GDK_UNMAP)
        ((16) 'GDK_PROPERTY_NOTIFY)
        ((17) 'GDK_SELECTION_CLEAR)
        ((18) 'GDK_SELECTION_REQUEST)
        ((19) 'GDK_SELECTION_NOTIFY)
        ((20) 'GDK_PROXIMITY_IN)
        ((21) 'GDK_PROXIMITY_OUT)
        ((22) 'GDK_DRAG_ENTER)
        ((23) 'GDK_DRAG_LEAVE)
        ((24) 'GDK_DRAG_MOTION)
        ((25) 'GDK_DRAG_STATUS)
        ((26) 'GDK_DROP_START)
        ((27) 'GDK_DROP_FINISHED)
        ((28) 'GDK_CLIENT_EVENT)
        ((29) 'GDK_VISIBILITY_NOTIFY)
        ((30) 'GDK_NO_EXPOSE)
        ((31) 'GDK_SCROLL)
        ((32) 'GDK_WINDOW_STATE)
        ((33) 'GDK_SETTING)
        ((34) 'GDK_OWNER_CHANGE)
        ((35) 'GDK_GRAB_BROKEN)
        ((36) 'GDK_DAMAGE)
        (else
         (sf "unknown event: ~S\n" (cdata-ref evt '* 'type))
         'UNKNOWN))))

(define (check1)
  (let* ((evt (make-GdkEventAny))
         (p (pointer-to evt)))
    (let loop ((n 36))
      (unless (negative? n)
        (cdata-set! evt n 'type)
        (unless (= n (gdk-symval (gdk-event-type p) ))
          (simple-formt (current-error-port) "mismatch ~S\n" n)
          (loop (1- n)))))))

;; Any Expose NoExpose Visibility Motion Button Scroll Key Focus Crossing
;; Configure Property Selection OwnerChange Promimity Client DND WindowState
;; Setting GrabBroken 
(define (fork-event evt)
  (if (zero? (cdata-ref evt)) #f
      (case (cdata-ref evt '* 'type)
        ;;((0) (make-GdkEventDelete* (cdata-ref evt)))
        ;;((1) (make-GdkEventDestroy* (cdata-ref evt)))
        ((2) (cdata&-ref evt '* 'expose)))
        ((3) (cdata&-ref evt '* 'motion)))
        ((4 5 6 7) (cdata&-ref evt '* 'button))
        ((8 9) (cdata&-ref evt '* 'key))
        ((10 11) (cdata&-ref evt '* 'motion))
        ((12) (cdata&-ref evt '* 'focus_change))
        ((13) (cdata&-ref evt '* 'configure))
        #|
        ((14) (make-GdkEventMap* (cdata-ref evt)))
        ((15) (make-GdkEventUnmap* (cdata-ref evt)))
        ((16) (make-GdkEventProperty* (cdata-ref evt)))
        ;((17 18 19) (make-GdkEventSelectionClear* (cdata-ref evt)))
        ((20 21) (make-GdkEventProximity* (cdata-ref evt)))
        ((22 23 24 25 26 27) (make-GdkEventDND* (cdata-ref evt)))
        ((28) (make-GdkEventClient* (cdata-ref evt)))
        ((29) (make-GdkEventVisibility* (cdata-ref evt)))
        ((30) (make-GdkEventNoExpose* (cdata-ref evt)))
        ((31) (make-GdkEventScroll* (cdata-ref evt)))
        ((32) (make-GdkEventWindowState* (cdata-ref evt)))
        ((33) (make-GdkEventSetting* (cdata-ref evt)))
        ((34) (make-GdkEventOwnerChange* (cdata-ref evt)))
        ((35) (make-GdkEventGrabBroken* (cdata-ref evt)))
        ;;((36) (make-GdkEventDamage* (cdata-ref evt)))
        |#
        (else
         (sf "missed it\n")
         evt))))

(let loop ((n 0) (evt (gdk_event_get)))
  (when (< n 10)
    (simple-format #t "evt ~S is ~S\n" evt (gdk-event-type evt))
    (sleep 1)
    (loop (1+ n) (gdk_event_get))))

;; --- last line ---

