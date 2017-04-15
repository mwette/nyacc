;; Tmach.scm - test C99: CPP and parser
;;
;; Copyright (C) 2015,2016 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(add-to-load-path (string-append (getcwd) "/../../../../module/"))

(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 mach))
(use-modules (nyacc lang c99 pprint))	; pretty-print-c99
(use-modules (nyacc lang c99 util1))	; remove-inc-trees
(use-modules (nyacc lang util))		; move-if-changed
(use-modules (nyacc lalr))
(use-modules (nyacc util))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

;; test def parser: show parse tree
(when #f
  (let* ((file "exam.d/,ex.c")
	 (defs '(("arch" . "x86_64"))) 
	 (incs  '("."))
	 (parse (lambda () (dev-parse-c
			     #:cpp-defs defs #:inc-dirs incs
			     #:debug #f #:mode 'file)))
	 (sx (with-input-from-file file parse))
	 ;;(sx (remove-inc-trees sx))
         )
    (pretty-print sx)
    #t))


(when #f
  (gen-c99-files "../../../../module/nyacc/lang/c99")
  (system "touch ../../../../module/nyacc/lang/c99/parser.scm")
  (gen-c99x-files "../../../../module/nyacc/lang/c99")
  (system "touch ../../../../module/nyacc/lang/c99/xparser.scm")
  )

;; test pp
(when #f
  (use-modules (nyacc lang c99 parser))
  (let* ((sx0 (with-input-from-file "exam.d/ex06.c"
		(lambda () (parse-c99 #:inc-dirs '("exam.d")))))
	 (sx1 (and sx0 (remove-inc-trees sx0)))
	 #;(sx2 (elifify sx1)))
    (pretty-print sx1)
    (simple-format #t "===>\n")
    (pretty-print-c99 sx1)
    #t))

;; grammar and automaton in zipped text file 
(when #t
  (with-output-to-file "lang.txt"
    (lambda ()
      (pp-lalr-notice c99-spec)
      (pp-lalr-grammar c99-spec)
      (pp-lalr-machine c99-mach)
      #;(system "zip lang.txt"))))

;; equivalent bison input file
(when #f
  (with-output-to-file "gram.y"
    (lambda () (lalr->bison c99-spec))))

;; --- last line ---
