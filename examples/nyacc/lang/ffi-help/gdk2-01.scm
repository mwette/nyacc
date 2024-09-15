;; gdk-ex1.scm

;; Copyright (C) 2018,2024 Matthew Wette

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (ffi gdk2))
(use-modules (nyacc foreign cdata))

;; Initialize.
(gdk_init NULL NULL)

#;(define win
  (let ((attr (make-cdata GdkWindowAttr)))
    (cdata-set! attr (apply logior (map gdk-symval '(GDK_EXPOSE))) 'event_mask)
    (cdata-set! attr 400 'width)
    (cdata-set! attr 300 'height)
    (cdata-set! attr (gdk-symval 'GDK_INPUT_OUTPUT) 'wclass)
    (cdata-set! attr (gdk-symval 'GDK_WINDOW_TOPLEVEL) 'window_type)
    (gdk_window_new NULL (cdata& attr) 0)))
(define win
  (let ((attr (make-cdata GdkWindowAttr)))
    (cdata-set! attr
                (apply logior (map gdk-symval '(GDK_EXPOSE)))
                'event_mask)
    (cdata-set! attr 400 'width)
    (cdata-set! attr 300 'height)
    (cdata-set! attr 'GDK_INPUT_OUTPUT 'wclass)
    (cdata-set! attr 'GDK_WINDOW_TOPLEVEL 'window_type)
    (gdk_window_new NULL (cdata& attr) 0)))
(simple-format #t "win=~S\n" win)

(gdk_window_show win)

#;(let loop ((n 0) (evt (gdk_event_get)))
  (when (< n 10)
    (simple-format #t "evt = ~S\n" evt)
    (simple-format #t "    =>~S\n" (cdata-ref evt '* 'type))
    (sleep 1)
    (loop (1+ n) (gdk_event_get))))

;; --- last line ---

