;; t-c99.scm
;;

(use-modules (test-suite lib))

(with-test-prefix "c99"

  (pass-if "c99-t0" (lambda () #t))

  )

#;(pass-if "c99-t1"
  (lambda ()
    (let ((sx (with-input-from-string "int x;"))
	  )
      (equal? sx
	      '(trans-unit
		(decl (decl-spec-list (type-spec (fixed-type "int")))
		      (init-declr-list (init-declr (ident "x")))))))))

;; --- last line ---
