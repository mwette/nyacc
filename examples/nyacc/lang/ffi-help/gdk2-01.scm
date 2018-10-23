;; gdk-ex1.scm

;; Copyright (C) 2018 Matthew R. Wette

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (ffi gdk2))
(use-modules (system ffi-help-rt))

;; Initialize.
(gdk_init NULL NULL)

(define win
  (let ((attr (make-GdkWindowAttr)))
    (fh-object-set! attr 'event_mask
		    (apply logior (map gdk-symval '(GDK_EXPOSE))))
    (fh-object-set! attr 'width 400)
    (fh-object-set! attr 'height 300)
    (fh-object-set! attr 'wclass (gdk-symval 'GDK_INPUT_OUTPUT))
    (fh-object-set! attr 'window_type (gdk-symval 'GDK_WINDOW_TOPLEVEL))
    (gdk_window_new NULL (pointer-to attr) 0)))
(simple-format #t "win=~S\n" win)

(gdk_window_show win)

(let loop ((n 0) (evt (gdk_event_get)))
  (when (< n 10)
    (simple-format #t "evt=~S\n" evt)
    (sleep 1)
    (loop (1+ n) (gdk_event_get))))

;; --- last line ---

