;; nyacc/lang/Tcpp.scm
;;
;; Copyright (C) 2015,2016 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lang c99 cppmach))
(use-modules (nyacc lang c99 cpp))
(use-modules (nyacc lalr))
(use-modules (nyacc export))
(use-modules (nyacc lang util))		; move-if-changed
(use-modules (ice-9 pretty-print))

(when #t
  (gen-cpp-files "../../../../module/nyacc/lang/c99")
  (system "touch ../../../../module/nyacc/lang/c99/cpp.scm"))

(when #f
  (with-output-to-file "cpplang.txt.new"
    (lambda ()
      (let* ((notice (assq-ref (assq-ref cpp-spec 'attr) 'notice))
	     (lines (if notice (string-split notice #\newline) '())))
	(for-each (lambda (l) (simple-format #t "  ~A\n" l)) lines)
	(newline))
      (pp-lalr-grammar cpp-spec)
      (pp-lalr-machine cpp-mach)))
  (move-if-changed "cpplang.txt.new" "cpplang.txt")
  #t)

(when #f
  (let ((dt '(("A" . "1") ("B" . "2")))
	(sx (with-input-from-string
		"defined(A) && defined(B) && !defined(C)"
	      parse-cpp-expr)))
    (pretty-print sx)
    (simple-format #t "=> ~S\n" (eval-cpp-expr sx dt))
    #f))


;; --- last line ---
