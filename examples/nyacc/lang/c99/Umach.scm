;; Umach.scm - demo C99: CPP and parser
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
(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 cxmach))
(use-modules (nyacc lang c99 cxeval))
(use-modules (nyacc lang c99 pprint))	; pretty-print-c99
(use-modules (nyacc lang c99 util1))	; remove-inc-trees
(use-modules (nyacc lang c99 munge))	; tree->udict
(use-modules (nyacc lang util))		; move-if-changed
(use-modules (nyacc lalr))
(use-modules (nyacc util))
;;(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

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

(when #f
  (pretty-print
   (with-input-from-string
       "int x;\nint inc(int x) { return x + 1; }\n"
     parse-c99)))

;; --- last line ---
