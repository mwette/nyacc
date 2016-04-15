;; nyacc/lang/matlab/Tmach.scm - matlab
;;
;; Copyright (C) 2015,2016 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

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
(when #t ;; processing
  (let* ((a-file "exam.d/ex03a.m")
	 (b-file "exam.d/ex03b.m")
	 (sx0a (with-input-from-file a-file dev-parse-ml))
	 (sx1a (and sx0a (declify-script sx0a)))
	 ;;(sx0b (with-input-from-file b-file dev-parse-ml))
	 ;;(sx1b (and sx0b (declify-ffile sx0b)))
	 )
    ;;(pretty-print sx0a)
    ;;(simple-format #t "==>\n")
    (pretty-print sx1a)
    #t))

;; Geneates a text file showing the grammar and state machine.
(when #t
  (with-output-to-file "lang.txt"
    (lambda ()
      (pp-lalr-notice matlab-spec)
      (pp-lalr-grammar matlab-spec)
      (pp-lalr-machine matlab-mach)
      (system "zip lang.txt"))))

;; The following illustrates the generation of bison input file:
(when #f
  (with-output-to-file "gram.y" (lambda () (lalr->bison matlab-spec))))

;; --- last line ---
