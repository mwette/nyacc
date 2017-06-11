;; nyacc/lang/matlab/Tmach.scm - matlab
;;
;; Copyright (C) 2015-2017 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(add-to-load-path (string-append (getcwd) "/../../../../module"))
(add-to-load-path (string-append (getcwd) "/../../../../examples"))

(use-modules (nyacc lang matlab mach))
(use-modules (nyacc lang matlab parser))
(use-modules (nyacc lang matlab util))
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
  (gen-matlab-files)
  (system "touch parser.scm"))

(when #f ;; reg parser from parser.scm
  (let ((sx0 (with-input-from-file "exam.d/ex03b.m"
	       (lambda () (parse-ml #:debug #f)))))
    (pretty-print sx0)))

;; Experimental matlab->c converter.
(when #t
  (let* ((file ",ex1.m")
	 (sx0 (with-input-from-file file dev-parse-ml)))
    (pretty-print sx0)
    (let ((sx1 (and sx0 (declify-ffile sx0))))
      (pretty-print sx1)
      #t)))

;; Geneates a text file showing the grammar and state machine.
(when #f
  (with-output-to-file "lang.txt"
    (lambda ()
      (pp-lalr-notice matlab-spec)
      (pp-lalr-grammar matlab-spec)
      (pp-lalr-machine matlab-mach)
      (system "zip lang lang.txt"))))

;; --- last line ---
