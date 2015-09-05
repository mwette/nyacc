
(add-to-load-path (string-append (getcwd) "/../../"))
(add-to-load-path (string-append (getcwd) "/../../../../module"))

(use-modules (lang c cppgen))
(use-modules (nyacc lalr))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))
(write-lalr-tables cpp-mach "cpptab.scm.new")
(write-lalr-actions cpp-mach "cppact.scm.new")

(let ((dt '(("A" . "1") ("B" . "2")))
      (sx (with-input-from-string
	      "defined(A) && defined(B) && !defined(C)"
	    parse-cpp-expr)))
  (pretty-print sx)
  (simple-format #t "=> ~S\n" (eval-cpp-expr sx dt))
  #f)


;; --- last line
