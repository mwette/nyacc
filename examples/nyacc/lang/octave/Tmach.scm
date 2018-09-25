;; nyacc/lang/octave/Tmach.scm - octave (aka matlab)
;;
;; Copyright (C) 2015-2018 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lang octave mach))
(use-modules (nyacc lang octave parser))
(use-modules (nyacc lang util))
(use-modules (nyacc lalr))
(use-modules (nyacc util))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(when #f ;; using the "development" parser
  (let* ((sx0 (with-input-from-file "exam.d/ex03a.m"
		(lambda () (dev-parse-oct #:debug #f)))))
    (pretty-print sx0)))

(when #t
  (with-input-from-file "dbl"
    (lambda () (pretty-print (parse-oct #:debug #f)))))

;; Experimental octave->c converter.
#|
(use-modules (nyacc lang octave util))
(when #f
  (let* ((file "exam.d/ex03b.m")
	 (sx0 (with-input-from-file file dev-parse-oct)))
    (pretty-print sx0)
    (let ((sx1 (and sx0 (declify-ffile sx0))))
      (pretty-print sx1)
      #t)))
|#


;; --- last line ---
