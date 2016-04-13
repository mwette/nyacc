;; 
;; 

(use-modules (nyacc lang c99 mach))
(use-modules (nyacc util))
(use-modules (nyacc lalr))
(use-modules (nyacc export))
(use-modules (nyacc bison))

;;(with-output-to-file "gram.y" (lambda () (lalr->bison c99-spec)))
;;(simple-format #t "~S\n" (assq-ref c99-spec 'assc))
#;(define c99-mach/b
  (hashify-machine
   (compact-machine
    (make-lalr-machine/bison c99-spec))))
(write-lalr-actions c99-mach/b "c99bact.scm")
(write-lalr-tables c99-mach/b "c99btab.scm")
;; len-v
;; rto-v
;;(ugly-print (assq-ref c99-mach/b 'mtab))
