


(define-module (fheck)
  #:export (check_dvec)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (system ffi-help-rt))

(define fheck-so (dynamic-link "fheck"))

(define check_dvec
  (let ((~check_dvec
	 (ffi:pointer->procedure
	  ffi:void
	  (dynamic-func "fh_check_dvec" fheck-so)
	  (list '* ffi:int))))
    (lambda (p n)
      (let ((~p (unwrap~pointer p))
	    (~n (unwrap~fixed n)))
	(~check_dvec p n)))))

;; --- last line ---

