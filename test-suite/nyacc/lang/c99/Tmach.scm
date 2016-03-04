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
(use-modules (nyacc lang util))		; move-if-changed
(use-modules (nyacc lalr))
(use-modules (nyacc util))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(define defs '(("arch" . "x86_64")))
(define incs '("."))

(when #t
  (gen-c99-files "../../../../module/nyacc/lang/c99")
  (system "touch ../../../../module/nyacc/lang/c99/parser.scm"))

;; test parser
(when #t
  (let ((sx (with-input-from-file "ex1.c"
	      (lambda ()
		(dev-parse-c #:cpp-defs defs #:inc-dirs incs #:debug #f)))))
    (pretty-print sx)
    (simple-format #t "===>")
    (pretty-print-c99 sx)
    #t))

#;(define (module-path filename)
  (string-append "../../../../module/nyacc/lang/c99/" filename))

(when #t
  (with-output-to-file "lang.txt.new"
    (lambda ()
      (let* ((notice (assq-ref (assq-ref c99-spec 'attr) 'notice))
	     (lines (if notice (string-split notice #\newline) '())))
	(for-each (lambda (l) (simple-format #t "  ~A\n" l)) lines)
	(newline))
      (pp-lalr-grammar c99-spec)
      (pp-lalr-machine c99-mach)))
  (move-if-changed "lang.txt.new" "lang.txt"))

(when #t
  (with-output-to-file "gram.y.new"
    (lambda () (lalr->bison c99-spec)))
  (move-if-changed "gram.y.new" "gram.y"))

;; --- last line ---
