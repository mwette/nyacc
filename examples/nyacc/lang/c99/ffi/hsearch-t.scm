;; hsearch-t.scm - hsearch test

(define-module (ffi hsearch-t)
  #:use-module (ffi hsearch-s)
  #:use-module (test-suite lib)
  #:use-module ((system foreign) #:prefix ffi:)
  )

(define (str->int8* str)
  (ffi:pointer-address (ffi:string->pointer str)))

(pass-if "ffi/hsearch/01"
  (let* ((fd (hcreate 31))
	 (kv (make-ENTRY `((key ,(str->int8* "abc")) (data 0))))
	 )
    #t))


;; --- last line ---

