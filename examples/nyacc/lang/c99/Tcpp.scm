;; nyacc/lang/Tcpp.scm
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

;;(add-to-load-path (string-append (getcwd) "/../../../"))
;;(add-to-load-path (string-append (getcwd) "/../../../../module"))

(use-modules (nyacc lang c99 cppgen))
(use-modules (nyacc lalr))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(define (module-path filename)
  (string-append "../../../../module/nyacc/lang/c99/" filename))

(begin
  (write-lalr-actions cpp-mach "cppact.scm.new")
  (write-lalr-tables cpp-mach "cpptab.scm.new")
  (move-if-changed "cppact.scm.new" (module-path "cppact.scm"))
  (move-if-changed "cpptab.scm.new" (module-path "cpptab.scm"))
  )

(let ((dt '(("A" . "1") ("B" . "2")))
      (sx (with-input-from-string
	      "defined(A) && defined(B) && !defined(C)"
	    parse-cpp-expr)))
  (pretty-print sx)
  (simple-format #t "=> ~S\n" (eval-cpp-expr sx dt))
  #f)


;; --- last line ---
