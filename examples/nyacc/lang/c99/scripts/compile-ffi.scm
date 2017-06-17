;;; scripts/compile-ffi.scm --- command-line Guile Scheme ffi compiler

;; Copyright (C) 2017 Matthew R. Wette
;;
;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;; or any later version published by the Free Software Foundation.  See
;; the file COPYING included with the nyacc distribution.

(add-to-load-path (string-append (getcwd) "../../../../module/"))

(define-module (scripts compile-ffi)
  #:use-module (system base language)
  #:use-module (ffi-help)
  )

(define (compile-ffi . args)
  (use-modules (ffi-help))		; needed here!
  (if (null? args) (error "expecting argument"))
  (let* ((file (car args))
         )
     (compile-ffi-file file)
     ))

(define main compile-ffi)

;;(compile-ffi-file "cairo/cairo-svg.ffi")
;;(eval '(define-ffi-module (cairo cairo)) (current-module))
;;(compile-ffi-file "zzz.ffi")

;; --- last line ---
