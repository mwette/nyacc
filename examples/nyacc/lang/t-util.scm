;;; t-util.scm - parsing utilities
(add-to-load-path (getcwd))

(use-modules (lang util))

(let* ((tl0 (make-tl 'abc 1))
       (tl1 (tl-append tl0 2))
       (tl2 (tl-insert tl1 'a))
       (tl3 (tl+attr tl2 'x "true"))
       (tl4 (tl-append tl3 20))
       (tl5 (tl-insert tl4 'z))
       (tlx tl5))
  ;; expect (abc (@ (x "true")) z a 1 2 20)
  (simple-format #t "~S\n" (tl->list tlx))
  )
;;; --- last line
