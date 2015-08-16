;;; examples/nyacc/lang/t-util.scm
;;; 
;;; Copyright (C) 2015 Matthew R. Wette
;;; 
;;; Copying and distribution of this file, with or without modification,
;;; are permitted in any medium without royalty provided the copyright
;;; notice and this notice are preserved.  This file is offered as-is,
;;; without any warranty.

;; Test the runtime parsing utilities.
;; examples/nyacc$ guile lang/t-util.scm

(add-to-load-path (getcwd))

(use-modules (lang util))

(let* ((tl0 (make-tl 'abc 1))
       (tl1 (tl-append tl0 2))
       (tl2 (tl-insert tl1 'a))
       (tl3 (tl+attr tl2 'x "true"))
       (tl4 (tl-append tl3 20 25))
       (tl5 (tl-insert tl4 'z))
       (tlx tl5))
  ;; expect (abc (@ (x "true")) z a 1 2 20)
  (simple-format #t "~S\n" (tl->list tlx))
  )

(let* ((tl (make-tl 'abc 1)))
  (set! tl (tl-append tl 2))
  (set! tl (tl-insert tl 'a))
  (set! tl (tl+attr tl 'x "true"))
  (set! tl (tl-append tl 20))
  (set! tl (tl+attr tl 'y "true"))
  (set! tl (tl-append tl 30))
  (set! tl (tl+attr tl 'z "true"))
  (set! tl (tl-append tl 40))
  (set! tl (tl-insert tl 'z))
  (simple-format #t "~S\n" (tl->list tl))
  )

;;; --- last line
