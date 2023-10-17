#!/bin/bash
# -*- scheme -*-

exec guile $0
!#

;; replace this:

;; with ...y


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

(define* (start-nx-repl #:optional (lang (current-language)) #:key debug)
  (start-nx-repl* lang debug prompting-meta-read))

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

(define (nx-repl)
  (let ((guile-user-module (resolve-module '(guile-user))))
    (set-current-module guile-user-module)
    ;;(process-use-modules '((ice-9 session) (ice-9 regex) (ice-9 threads)))

    (call-with-sigint
     (lambda ()
       (and (defined? 'setlocale)
            (catch 'system-error
              (lambda ()
                (setlocale LC_ALL ""))
              (lambda (key subr fmt args errno)
                (format (current-error-port)
                        "warning: failed to install locale: ~a~%"
                        (strerror (car errno))))))

       (let ((status (start-nx-repl (current-language))))
         (run-hook exit-hook)
         status)))))

(nx-repl)

;; Local Variables:
;; eval: (put 'repl-option-set! 'scheme-indent-function 2)
;; End:

;; --- last line ---
