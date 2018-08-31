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
		(lambda () (dev-parse-ml #:debug #f)))))
    (pretty-print sx0)))

;; To generate the tables which will be included in the "fast" parser.
(when #t
  (gen-octave-files)
  (compile-file "parser.scm")
  )

;; Geneates a text file showing the grammar and state machine.
(when #t
  (with-output-to-file ",file.txt"
    (lambda ()
      (pp-lalr-notice octave-spec)
      (pp-lalr-grammar octave-spec)
      (pp-lalr-machine octave-mach)))
  (with-output-to-file ",stmt.txt"
    (lambda ()
      (pp-lalr-notice octave-ia-spec)
      (pp-lalr-grammar octave-ia-spec)
      (pp-lalr-machine octave-ia-mach))))

(when #f ;; reg parser from parser.scm
  (let ((sx0 (with-input-from-file "exam.d/ex03b.m"
	       (lambda () (parse-ml #:debug #f)))))
    (pretty-print sx0)))

;; Experimental octave->c converter.
#|
(use-modules (nyacc lang octave util))
(when #f
  (let* ((file "exam.d/ex03b.m")
	 (sx0 (with-input-from-file file dev-parse-ml)))
    (pretty-print sx0)
    (let ((sx1 (and sx0 (declify-ffile sx0))))
      (pretty-print sx1)
      #t)))
|#


;; --- last line ---
