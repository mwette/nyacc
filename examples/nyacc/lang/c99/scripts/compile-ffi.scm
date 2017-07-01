;;; scripts/compile-ffi.scm --- command-line Guile Scheme ffi compiler

;; Copyright (C) 2017 Matthew R. Wette
;;
;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;; or any later version published by the Free Software Foundation.  See
;; the file COPYING included with the nyacc distribution.

(define-module (scripts compile-ffi)
  #:use-module (ffi-help)
  #:use-module (system base language)
  #:use-module (srfi srfi-37)
  )

(define %options
  ;; specification of command-line options
  (list (option '(#\h "help") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'help? #t result)))
	(option '("version") #f #f
		(lambda (opt name arg result)
		  (show-version)
		  (exit 0)))
	(option '(#\t "to") #t #f
		(lambda (opt name arg result)
		  (alist-cons 'to (string->symbol arg) result)))
	))

;; from scripts/compile.scm
(define (parse-args args)
  "Parse argument list @var{args} and return an alist with all the relevant
options."       
  (args-fold args %options
             (lambda (opt name arg result)
               (format (current-error-port) "~A: unrecognized option" name)
               (exit 1))
             (lambda (file result)
               (let ((input-files (assoc-ref result 'input-files)))
                 (alist-cons 'input-files (cons file input-files)
                             result)))
                      
             ;; default option values
             '((input-files) 
               (load-path)
               (warnings unsupported-warning))))

(define *fh-version* "0.1.1")
(define (show-version)
  (format #t "compile-ffi ~A~%" *fh-version*))

(define (compile-ffi . args)
  (use-modules (ffi-help))		; needed here!
  (if (null? args) (error "expecting argument"))
  (let* ((file (car args)))
     (compile-ffi-file file)))

(define main compile-ffi)

;; --- last line ---
