;; Tc99expr.scm - clang expression parser
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

;;(use-modules (nyacc lang c99 pgen))
(use-modules (nyacc lang c99 pprint))
;;(use-modules (nyacc lalr))
;;(use-modules (nyacc util))
(use-modules (nyacc export))
(use-modules (nyacc lang c99 xparser))
(use-modules (ice-9 pretty-print))
(use-modules (srfi srfi-43))


(let* ((st0 "(int)(((foo_t*)0)->x)")
       (st0 "(int)(((((foo_t*)0)->x)->y)->z)")
       (st0 "(int*)(&(((foo_t*)0)->x.y.z))")
       (st0 "(a.b)[ix]")
       (st0 "(foo_t)(top/bot)%rest")
       (sx0 (parse-cx st0 #:tyns '("foo_t")))
       (st1 (with-output-to-string (lambda () (pretty-print-c99 sx0))))
       (sx1 (parse-cx st1 #:tyns '("foo_t")))
       )
  (simple-format #t "~S => \n" st0)
  (pretty-print sx0)
  (simple-format #t "=> ~S =>\n" st1)
  (pretty-print sx1)
  #f)

;; --- last line ---
