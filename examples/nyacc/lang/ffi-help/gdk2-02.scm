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
(use-modules (nyacc foreign cdata))

(define (sf fmt . args) (apply simple-format #t fmt args))

(gdk_init NULL NULL)

(define win
  (let ((attr (make-cdata GdkWindowAttr)))
    (cdata-set! attr (apply logior (map gdk-symval '(GDK_EXPOSE))) 'event_mask)
    (cdata-set! attr 400 'width)
    (cdata-set! attr 300 'height)
    (cdata-set! attr (gdk-symval 'GDK_INPUT_OUTPUT) 'wclass)
    (cdata-set! attr (gdk-symval 'GDK_WINDOW_TOPLEVEL) 'window_type)
    (gdk_window_new NULL (cdata& attr) 0)))

(gdk_window_show win)

(define (check1)
  (let* ((evt (make-cdata GdkEventAny))
         (p (cdata& evt)))
    (let loop ((n 36))
      (unless (negative? n)
        (cdata-set! evt n 'type)
        (unless (= n (gdk-symval (cdata-ref evt '* 'type)))
          (simple-format (current-error-port) "mismatch ~S\n" n)
          (loop (1- n)))))))

;; Any Expose NoExpose Visibility Motion Button Scroll Key Focus Crossing
;; Configure Property Selection OwnerChange Promimity Client DND WindowState
;; Setting GrabBroken 
(define (fork-event evt)
  (define (select key) (cdata&-ref evt '* key))
  (and
   (equal? NULL (cdata-ref evt))
   (case (cdata-ref evt '* 'type)
     ((GTK_DELETE) (cdata&-ref evt '* 'delete?))
     ((GDK_DESTORY) (select 'destory))
     ((GDK_EXPOSE) (cdata&-ref evt '* 'expose))
     ((GDK_MOTION_NOTIFY) (cdata&-ref evt '* 'motion))
     ((GDK_BUTTON_PRESS GDK_BUTTON_RELEASE) (select 'button))
     ((GDK_BUTTON_PRESS2 GDK_BUTTON3_PRESS) (select 'button))
     ((GDK_KEY_PRESS GDK_KEY_RELEASE) (select 'key))
     ((GDK_ENTER_NOTIFY GDK_LEAVE_NOTIFY) (select 'motion))
     ((GDK_FOCUS_CHANGE) (select 'focus_change))
     ((GDK_CONFIGURE) (select 'configure))
     ((GDK_MAP) (select 'unknown))
     ((GDK_UNMAP) (select 'unknown))
     ((GDK_PROPERTY_NOTIFY) (select 'unknown))
     ((GDK_SELECTION_CLEAR GDK_SELECTION_REQUEST GDK_SELECTION_NOTIFY)
      (select 'unknown))
     ((GDK_PROXIMITY_IN GDK_PROXIMITY_OUT) (select 'unknown))
     ((GDK_DRAG_ENTER GDK_DRAG_LEAVE GDK_DRAG_MOTION GDK_DRAG_STATUS
                      GDK_DROP_START GDK_DROP_STOP GDK_DROP_FINISHED)
      (select 'unknown))
     ((GDK_CLIENT_EVENT) (select 'client))
     ((GDK_VISIBILITY_NOTIFY) (select 'unknown))
     ((GDK_NO_EXPOSE) select 'no_expose)
     ((GDK_SCROLL) (select 'scroll))
     ((GDK_WINDOW_STATE) (select 'unknown))
     ((GDK_SETTING) (select 'unknown))
     ((GDK_OWNER_CHANGE) (select 'unknown))
     ((GDK_GRAB_BROKEN) (select 'unknown))
     ((GDK_DAMAGE) (select 'unknown))
     (else (sf "missed it\n") evt))))

(let loop ((n 0) (evt (gdk_event_get)))
  (when (< n 10)
    (if (equal? NULL (cdata-ref evt))
        (sf "evt ~s\n" evt)
        (sf "evt ~s, ~s\n" evt (cdata-ref evt '* 'type)))
    (sleep 1)
    (loop (1+ n) (gdk_event_get))))

;;(define evt (gdk_event_get))

;; --- last line ---

