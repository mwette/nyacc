;; gdk-ex1.scm

(use-modules (ffi gdk2))
(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (bytestructures guile))

(define (sf fmt . args) (apply simple-format #t fmt args))
(define NULL ffi:%null-pointer)

(define attr (make-GdkWindowAttr))

;; must config event-mask, width, height, wclass, window_type
(fh-object-set! attr 'event_mask (apply logior (map gdk-symval '(GDK_EXPOSE))))
(fh-object-set! attr 'width 400)
(fh-object-set! attr 'height 300)
(fh-object-set! attr 'wclass (gdk-symval 'GDK_INPUT_OUTPUT))
(fh-object-set! attr 'window_type (gdk-symval 'GDK_WINDOW_TOPLEVEL))

;;(sf "~S\n" attr)

;; I think I need to initialize something.
;;(define win (gdk_window_new NULL (pointer-to attr) 0))
#|
|#

;; --- last line ---

