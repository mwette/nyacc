;; Tmach.scm - javascript
;;
;; Copyright (C) 2015,2016 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lang javascript mach))
(use-modules (nyacc lang javascript pprint))
(use-modules (nyacc lalr))
(use-modules (nyacc parse))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(when #t
  (gen-js-files)
  (system "touch parser.scm"))

(when #t
  (gen-se-files)
  (system "touch separser.scm"))

(when #t
  (with-output-to-file "lang.txt"
    (lambda ()
      (pp-lalr-notice js-spec)
      (pp-lalr-grammar js-spec)
      (pp-lalr-machine js-mach))))

(when #t
  (with-output-to-file "gram.y.new"
    (lambda () (lalr->bison js-spec))))

(when #t
  (let ((res (with-input-from-file "ex1.js" dev-parse-js)))
    (pretty-print res)
    (pretty-print-js res)))

;; --- last line ---
