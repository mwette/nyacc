;; Tselt.scm - generate javascript SourceElement parser
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lang javascript pgen))
(use-modules (nyacc lalr))

(define js-elt-spec
  (let* ((spec js-spec)
	 (rhs-v (assq-ref spec 'rhs-v))
	 (rhs-0 (vector-ref rhs-v 0)))
    (vector-set! rhs-0 0 'SourceElement)
    spec))
	  
(define js-elt-mach
  ;;(hashify-machine
  (identity
   (compact-machine
    (make-lalr-machine js-elt-spec))))

(with-output-to-file "elang.txt.new"
  (lambda () (pp-lalr-grammar js-elt-spec)
             (pp-lalr-machine js-elt-mach)))
(write-lalr-actions js-elt-mach "eactions.scm.new")
(write-lalr-tables js-elt-mach "etables.scm.new")

;; --- last line ---
