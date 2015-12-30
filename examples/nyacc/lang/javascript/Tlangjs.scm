;; Tlangjs.scm - javascript
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(add-to-load-path (string-append (getcwd) "/../../.."))
(add-to-load-path (string-append (getcwd) "/../../../../module"))

(use-modules (nyacc lang javascript pgen))
(use-modules (nyacc lalr))
(use-modules (nyacc parse))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(with-output-to-file "gram.y.new"
  (lambda () (lalr->bison js-spec)))
(with-output-to-file "lang.txt.new"
  (lambda ()
    (let* ((notice (assq-ref (assq-ref js-spec 'attr) 'notice))
           (lines (if notice (string-split notice #\newline) '())))
      (for-each (lambda (l) (simple-format #t "  ~A\n" l)) lines)
      (newline))
    (pp-lalr-grammar js-spec)
    (pp-lalr-machine js-mach)))
(write-lalr-actions js-mach "actions.scm.new")
(write-lalr-tables js-mach "tables.scm.new")

(define res (with-input-from-file "ex1.js" dev-parse-js))
(pretty-print res)

(use-modules (nyacc lang javascript pprint))
(pretty-print-js res)

;; --- last line ---
