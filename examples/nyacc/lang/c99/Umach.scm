;; Umach.scm - update C99 machines and parser (after editing mach.scm)
;;
;; Copyright (C) 2015,2016,2018 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(define mod-dir "../../../../module/nyacc/lang/c99")

(use-modules (nyacc lang c99 mach))
(use-modules (nyacc lang c99 cppmach))
(use-modules (nyacc lang c99 cxmach))
(use-modules (nyacc lalr))

(when #t
  (if (gen-cpp-files mod-dir)
      (compile-file (string-append mod-dir "/cpp.scm")))
  (if (gen-c99-files mod-dir)
      (compile-file (string-append mod-dir "/parser.scm"))))

(when #t
  (with-output-to-file ",file.txt"
    (lambda ()
      (pp-lalr-notice c99-spec)
      (pp-lalr-grammar c99-spec)
      (pp-lalr-machine c99-mach)))
  (with-output-to-file ",expr.txt"
    (lambda ()
      (pp-lalr-notice c99x-spec)
      (pp-lalr-grammar c99x-spec)
      (pp-lalr-machine c99x-mach)))
  #t)

(when #f
  (with-output-to-file ",cexp.txt"
    (lambda ()
      (pp-lalr-notice c99cx-spec)
      (pp-lalr-grammar c99cx-spec)
      (pp-lalr-machine c99cx-mach)))
  #t)

;; --- last line ---
