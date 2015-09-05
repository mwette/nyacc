;; lalrT.scm - modelica dev
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(add-to-load-path (getcwd))
(add-to-load-path (string-append (getcwd) "/../../module"))

(use-modules (lang modelica pgen))
(use-modules (nyacc lalr))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(with-output-to-file "lang/modelica/gram.y.new"
  (lambda () (lalr->bison modelica-spec)))
(with-output-to-file "lang/modelica/lang.txt.new"
  (lambda ()
    (pp-lalr-grammar modelica-spec)
    (pp-lalr-machine modelica-mach)))
(write-lalr-tables modelica-mach "lang/modelica/tables.scm.new")
(write-lalr-actions modelica-mach "lang/modelica/actions.scm.new")
		
(define res
  (with-input-from-file "lang/modelica/ex1.mo"
    (lambda ()
      (modelica-parser (gen-mod-lexer) #:debug #f))))
(pretty-print res)

;; --- last line
