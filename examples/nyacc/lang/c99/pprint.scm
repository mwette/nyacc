;; pprint.scm - hammer on pretty-print-c99
;;
;; Copyright (C) 2015,2016 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lang c99 parser))	; pretty-print-c99
(use-modules (nyacc lang c99 pprint))	; pretty-print-c99
(use-modules (nyacc lang c99 util1))	; remove-inc-trees
(use-modules (nyacc lang util))		; move-if-changed
;;(use-modules (nyacc util))
(use-modules (ice-9 pretty-print))

(define (my-parser file)
  (with-input-from-file file
    (lambda ()
      (parse-c99 #:inc-dirs '("exam.d")))))

;; test pp
(when #t
  (let* ((sx (my-parser "exam.d/ex3.c"))
	 (sx (remove-inc-trees sx))
	 #;(sx (elifify sx))
	 )
    (pretty-print-c99 sx)
    #t))

;; --- last line ---
