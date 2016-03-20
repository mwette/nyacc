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

(when #t
  (gen-matlab-files)
  (system "touch parser.scm"))

(when #t
  (with-output-to-file "lang.txt"
    (lambda ()
      (pp-lalr-notice matlab-spec)
      (pp-lalr-grammar matlab-spec)
      (pp-lalr-machine matlab-mach)
      ;;(system "zip lang.txt")
      )))

(when #f
  (with-output-to-file "gram.y"
    (lambda () (lalr->bison matlab-spec))))

(when #t ;; dev parser from mach.scm
  (let* ((sx0 (with-input-from-file "exam.d/ex03b.m"
		(lambda () (dev-parse-ml #:debug #f))))
	 (sx1 (typeify-tree sx0))
	 )
    ;;(pretty-print sx0)
    ;;(simple-format # "==>\n")
    (pretty-print sx1)
    #t))

(when #f ;; reg parser from parser.scm
  (let ((sx0 (with-input-from-file "exam.d/ex03.m"
	       (lambda () (parse-ml #:debug #f))))
	)
    (pretty-print sx0)
    #t))

;; --- last line ---
