;;; Umach.scm - modelica dev

;; Copyright (C) 2015,2017 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

;;; Usage:

;; If you edit mach.scm, you'll want to have the tables in mach.d/
;; regenerated, and then the parser recompiled.  To do this use:

;; $ guile Umach.scm

;;; Code:

(use-modules (nyacc lang modelica mach))
(use-modules (nyacc lalr))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(gen-modelica-files)
(compile-file "parser.scm")

(with-output-to-file ",file.txt"
  (lambda ()
    (pp-lalr-notice modelica-spec)
    (pp-lalr-grammar modelica-spec)
    (pp-lalr-machine modelica-mach)))

;; --- last line ---
