;; Tmach.scm - test C99 automaton and dev parser
;;
;; Copyright (C) 2015,2016 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lang c99 mach))
(use-modules (nyacc lang c99 pprint))	; pretty-print-c99
(use-modules (nyacc lang c99 util1))	; remove-inc-trees
(use-modules (nyacc lang util))		; move-if-changed
(use-modules (nyacc lalr))
(use-modules (nyacc util))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(when #f
  (with-output-to-file "lang.txt"
    (lambda ()
      (pp-lalr-notice c99-spec)
      (pp-lalr-grammar c99-spec)
      (pp-lalr-machine c99-mach))))

;; test parser
(when #f
  (let* ((defs '(("arch" . "x86_64"))) 
	 (incs  '("."))
	 (sx (with-input-from-file "ex1.c"
	       (lambda ()
	 	 (dev-parse-c #:cpp-defs defs #:inc-dirs incs #:debug #f))))
	 (sx (remove-inc-trees sx))
         )
    (pretty-print sx)
    (simple-format #t "===>")
    (pretty-print-c99 sx)
    #t))

(when #f
  (with-output-to-file "gram.y.new"
    (lambda () (lalr->bison c99-spec)))
  (move-if-changed "gram.y.new" "gram.y"))

(when #f
  (gen-c99-files "../../../../module/nyacc/lang/c99")
  (system "touch ../../../../module/nyacc/lang/c99/parser.scm"))

;; test pp
(when #t
  (let* ((sx (with-input-from-file "ex1.c" dev-parse-c99))
	 (sx (remove-inc-trees sx))
	 (sx (elifify sx)))
    (pretty-print sx)
    (simple-format #t "===>\n")
    (pretty-print-c99 sx)
    #t))

;; test pp
(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 util2))
(use-modules (sxml match))
(when #f
  (let* ((sx (with-input-from-file "ex1.c" parse-c99))
	 (sx (merge-inc-trees! sx))
	 (udict (and sx (tree->udict sx)))
	 (xx (assoc-ref udict "f1"))
	 (xx (stripdown xx udict))
	 (xx (udecl->mspec xx))
	 )
    (pretty-print xx)
    #t))

;; --- last line ---
