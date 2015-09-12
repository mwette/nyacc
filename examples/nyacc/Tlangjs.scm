;; Tlangjs.scm - javascript
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(add-to-load-path (getcwd))
(add-to-load-path (string-append (getcwd) "/../../module"))

(use-modules (lang javascript pgen))
(use-modules (nyacc lalr))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(with-output-to-file "lang/javascript/gram.y.new"
  (lambda () (lalr->bison js-spec)))
(with-output-to-file "lang/javascript/lang.txt.new"
  (lambda () (pp-lalr-grammar js-spec)
             (pp-lalr-machine js-mach)))
(write-lalr-actions js-mach "lang/javascript/actions.scm.new")
(write-lalr-tables js-mach "lang/javascript/tables.scm.new")

(define res (with-input-from-file "lang/javascript/ex1.js" parse-js))
(pretty-print res)

;; --- last line
