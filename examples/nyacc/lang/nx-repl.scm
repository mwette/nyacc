#!/bin/bash
# -*- scheme -*-

exec guile $0
!#
;;; nyacc/lang/nx-repl.scm - multi-language REPL

;; Copyright (C) 2023 Matthew Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Code:

;; (define-module ...)

(use-modules (ice-9 history))
(use-modules (ice-9 readline))
(use-modules ((system repl repl) #:select (start-repl)))
(use-modules ((system repl common) #:select (make-repl repl-options)))

(activate-readline)

(define prompting-meta-read (@@ (system repl repl) prompting-meta-read))
(define run-repl* (@@ (system repl repl) run-repl*))

(define (start-nx-repl* lang debug prompting-meta-read)
  ;; ,language at the REPL will update the current-language.  Make
  ;; sure that it does so in a new dynamic scope.
  (parameterize ((current-language lang))
    (let ((repl (make-repl lang debug)))
      ;; NEW:
      (let ((spec (assq 'on-error (repl-options repl))))
        (set-car! (cdr spec)
                  #;(lambda (k . args)
                    (format #t "error: ~S\n" args)
                    (k (if #f #f)))
                  (lambda (k a1 fmt args a4)
                    (apply format #t fmt args)
                    (newline)
                    (if #f #f))
                  ))
      ;;
      (run-repl* repl prompting-meta-read))))

(define* (start-nx-repl #:optional (language (current-language)) #:key debug)
  (start-nx-repl* language debug prompting-meta-read))

(define call-with-sigint
  (if (not (provided? 'posix))
      (lambda (thunk) (thunk))
      (lambda (thunk)
        (let ((handler #f))
          (dynamic-wind
            (lambda ()
              (set! handler
                    (sigaction SIGINT
                      (lambda (sig)
                        (scm-error 'signal #f "User interrupt" '()
                                   (list sig))))))
            thunk
            (lambda ()
              (if handler
                  ;; restore Scheme handler, SIG_IGN or SIG_DFL.
                  (sigaction SIGINT (car handler) (cdr handler))
                  ;; restore original C handler.
                  (sigaction SIGINT #f))))))))

(define (install-locale)
  (and (defined? 'setlocale)
       (catch 'system-error
         (lambda ()
           (setlocale LC_ALL ""))
         (lambda (key subr fmt args errno)
           (format (current-error-port)
                   "warning: failed to install locale: ~a~%"
                   (strerror (car errno)))))))

(define* (nx-repl
          #:key
          (use '(((ice-9 session)) ((ice-9 regex)) ((ice-9 threads))))
          (language (current-language))
          )
  (let ((guile-user-module (resolve-module '(guile-user))))
    (set-current-module guile-user-module)
    (process-use-modules use)

    (call-with-sigint
     (lambda ()
       (install-locale)
       (let ((status (start-nx-repl language)))
         (run-hook exit-hook)
         status)))))

(nx-repl)

;; Local Variables:
;; eval: (put 'repl-option-set! 'scheme-indent-function 2)
;; End:

;; --- last line ---
