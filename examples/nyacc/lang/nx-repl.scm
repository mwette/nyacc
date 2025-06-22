#!/bin/bash
# -*- scheme -*-

exec guile $0
!#
;;; nyacc/lang/nx-repl.scm - multi-language REPL

;; Copyright (C) 2023-2024 Matthew Wette
;; Copyright (C) 2001, 2009-2014 Free Software Foundation, Inc.
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

;; The bulk of this code is currently from Guile, (system repl *).
;; This is all pulled together to understand and modify for
;; embedded application's that don't use Scheme.

;;; Code:

;; (define-module ...)

;; Guile (ice-9 history) is a bust because variables are $n
;; + use (set-hook! before-print-hook

(use-modules (ice-9 history))
(use-modules (ice-9 readline))
(use-modules (ice-9 control))           ; %
(use-modules (system base language))
(use-modules (system repl common))
(use-modules ((system repl repl) #:select (start-repl)))
(use-modules ((system repl common)
              #:select (make-repl repl-options repl-option-set!)))

(activate-readline)

;; define-record in system/base/syntax.scm
(define-record <debug> frames index error-message)

(define (error-string stack key args)
  (call-with-output-string
   (lambda (port)
     (let ((frame (and (< 0 (vector-length stack)) (vector-ref stack 0))))
       (print-exception port frame key args)))))
                  
(define (stack->vector stack)
  (let* ((len (stack-length stack))
         (v (make-vector len)))
    (if (positive? len)
        (let lp ((i 0) (frame (stack-ref stack 0)))
          (if (< i len)
              (begin
                (vector-set! v i frame)
                (lp (1+ i) (frame-previous frame))))))
    v))

(define (narrow-stack->vector stack . args)
  (let ((narrowed (apply make-stack (stack-ref stack 0) args)))
    (if narrowed
        (stack->vector narrowed)
        #()))) ; ? Can be the case for a tail-call to `throw' tho

(define* (call-with-error-handling thunk #:key
                                   (on-error 'debug) (post-error 'catch)
                                   (pass-keys '(quit)) (trap-handler 'debug)
                                   (report-keys '(stack-overflow out-of-memory)))
  (let ((in (current-input-port))
        (out (current-output-port))
        (err (current-error-port)))
    (define (with-saved-ports thunk)
      (with-input-from-port in
        (lambda ()
          (with-output-to-port out
            (lambda ()
              (with-error-to-port err
                thunk))))))

    (define (debug-trap-handler frame trap-idx trap-name)
      (let* ((tag (and (pair? (fluid-ref %stacks))
                       (cdr (fluid-ref %stacks))))
             (stack (narrow-stack->vector
                     (make-stack frame)
                     ;; Take the stack from the given frame, cutting 0
                     ;; frames.
                     0
                     ;; Narrow the end of the stack to the most recent
                     ;; start-stack.
                     tag
                     ;; And one more frame, because %start-stack
                     ;; invoking the start-stack thunk has its own frame
                     ;; too.
                     0 (and tag 1)))
             (error-msg (if trap-idx
                            (format #f "Trap ~d: ~a" trap-idx trap-name)
                            trap-name))
             (debug (make-debug stack 0 error-msg)))
        (with-saved-ports
         (lambda ()
           (if trap-idx
               (begin
                 (format #t "~a~%" error-msg)
                 (format #t "Entering a new prompt.  ")
                 (format #t "Type `,bt' for a backtrace or `,q' to continue.\n")))
           ((@ (system repl repl) start-repl) #:debug debug)))))

    (define (null-trap-handler frame trap-idx trap-name)
      #t)

    (define le-trap-handler
      (case trap-handler
        ((debug) debug-trap-handler)
        ((pass) null-trap-handler)
        ((disabled) #f)
        (else (error "Unknown trap-handler strategy" trap-handler))))

    (define (report-error key args)
      (with-saved-ports
       (lambda ()
         (run-hook before-error-hook)
         (print-exception err #f key args)
         (run-hook after-error-hook)
         (force-output err))))

    (catch #t
      (lambda () 
        (with-default-trap-handler le-trap-handler
          (lambda () (%start-stack #t thunk))))

      (case post-error
        ((report)
         (lambda (key . args)
           (if (memq key pass-keys)
               (apply throw key args)
               (begin
                 (report-error key args)
                 (if #f #f)))))
        ((catch)
         (lambda (key . args)
           (when (memq key pass-keys)
             (apply throw key args))
           (when (memq key report-keys)
             (report-error key args))
           (if #f #f)))
        (else
         (if (procedure? post-error)
             (lambda (k . args)
               (apply (if (memq k pass-keys) throw post-error) k args))
             (error "Unknown post-error strategy" post-error))))

      (case on-error
        ((debug)
         (lambda (key . args)
           (if (not (memq key pass-keys))
               (let* ((tag (and (pair? (fluid-ref %stacks))
                                (cdr (fluid-ref %stacks))))
                      (stack (narrow-stack->vector
                              (make-stack #t)
                              ;; Cut three frames from the top of the stack:
                              ;; make-stack, this one, and the throw handler.
                              3
                              ;; Narrow the end of the stack to the most recent
                              ;; start-stack.
                              tag
                              ;; And one more frame, because %start-stack invoking
                              ;; the start-stack thunk has its own frame too.
                              0 (and tag 1)))
                      (error-msg (error-string stack key args))
                      (debug (make-debug stack 0 error-msg)))
                 (with-saved-ports
                  (lambda ()
                    (format #t "~a~%" error-msg)
                    (format #t "Entering a new prompt.  ")
                    (format #t "Type `,bt' for a backtrace or `,q' to continue.\n")
                    ((@ (system repl repl) start-repl) #:debug debug)))))))
        ((report)
         (lambda (key . args)
           (unless (memq key pass-keys)
             (report-error key args))
           (if #f #f)))
        ((backtrace)
         (lambda (key . args)
           (if (not (memq key pass-keys))
               (let* ((tag (and (pair? (fluid-ref %stacks))
                                (cdr (fluid-ref %stacks))))
                      (frames (narrow-stack->vector
                               (make-stack #t)
                               ;; Narrow as above, for the debugging case.
                               3 tag 0 (and tag 1))))
                 (with-saved-ports (lambda () (print-frames frames)))
                 (report-error key args)
                 (if #f #f)))))
        ((pass)
         (lambda (key . args)
           ;; fall through to rethrow
           #t))
        (else
         (if (procedure? on-error)
             (lambda (k . args)
               (apply (if (memq k pass-keys) throw on-error) k args))
             (error "Unknown on-error strategy" on-error)))))))

(define-syntax-rule (with-error-handling form)
  (call-with-error-handling (lambda () form)))

#|
(define (flush-leading-whitespace)
  (let ((ch (peek-char)))
    (cond ((eof-object? ch) ch)
          ((char-whitespace? ch) (read-char) (flush-leading-whitespace))
          (else ch))))

(define (meta-reader lang env)
  (lambda* (#:optional (port (current-input-port)))
    (with-input-from-port port
      (lambda ()
        (let ((ch (flush-leading-whitespace)))
          (cond ((eof-object? ch)
                 (read-char))  ; consume the EOF and return it
                ((eqv? ch #\,)
                 (read-char)
                 meta-command-token)
                ((read-comment lang port ch)
                 *unspecified*)
(else ((language-reader lang) port env))))))))
        
(define (prompting-meta-read repl)
  (catch #t
    (lambda ()
      (repl-reader (lambda () (repl-prompt repl))
                   (meta-reader (repl-language repl) (current-module))))
    (lambda (key . args)
      (case key
        ((quit)
         (apply throw key args))
        (else
         (format (current-output-port) "While reading expression:\n")
         (print-exception (current-output-port) #f key args)
         (flush-all-input)
         *unspecified*)))))
|#

(define (flush-to-newline) 
  (if (char-ready?)
      (let ((ch (peek-char)))
        (if (and (not (eof-object? ch)) (char-whitespace? ch))
            (begin
              (read-char)
              (if (not (char=? ch #\newline))
                  (flush-to-newline)))))))

(define (with-stack-and-prompt thunk)
  (call-with-prompt (default-prompt-tag)
    (lambda () (start-stack #t (thunk)))
    (lambda (k proc)
      (with-stack-and-prompt (lambda () (proc k))))))
  
(define (run-repl* repl #;prompting-meta-read)
  (% (with-fluids ((*repl-stack*
                    (cons repl (or (fluid-ref *repl-stack*) '()))))
       (let prompt-loop ()
         ;;(let ((exp (prompting-meta-read repl)))
         (let ((exp (repl-reader "$ ")))
           (cond
            ((eqv? exp *unspecified*))  ; read error or comment, pass
            #|
            ((eq? exp meta-command-token)
             (catch #t
               (lambda ()
                 (meta-command repl))
               (lambda (k . args)
                 (if (eq? k 'quit)
                     (abort args)
                     (begin
                       (format #t "While executing meta-command:~%")
                       (print-exception (current-output-port) #f k args))))))
            |#
            ((eof-object? exp)
             (newline)
             (abort '()))
            (else
             ;; since the input port is line-buffered, consume up to the
             ;; newline
             (flush-to-newline)
             (call-with-error-handling
              (lambda ()
                (catch 'quit
                  (lambda ()
                    (call-with-values
                        (lambda ()
                          (% (let ((thunk
                                    (abort-on-error "compiling expression"
                                      (repl-prepare-eval-thunk
                                       repl
                                       (abort-on-error "parsing expression"
                                         (repl-parse repl exp))))))
                               (run-hook before-eval-hook exp)
                               (call-with-error-handling
                                (lambda ()
                                  (with-stack-and-prompt thunk))
                                #:on-error (repl-option-ref repl 'on-error)))
                             (lambda (k) (values))))
                      (lambda l
                        (for-each (lambda (v)
                                    (repl-print repl v))
                                  l))))
                  (lambda (k . args)
                    (abort args))))
              #:on-error (repl-option-ref repl 'on-error)
              #:trap-handler 'disabled)))
           (flush-to-newline) ;; consume trailing whitespace
           (prompt-loop))))
     (lambda (k status)
       status)))

(define (start-nx-repl* lang debug #;prompting-meta-read)
  ;; ,language at the REPL will update the current-language.  Make
  ;; sure that it does so in a new dynamic scope.
  (parameterize ((current-language lang))
    (let ((repl (make-repl lang debug)))
      (repl-option-set! repl 'prompt
        (const (format #f "~A> " (language-name (repl-language repl)))))
      ;; 'print (lambda (repl val) ...)
      (repl-option-set! repl 'value-history #f)
      ;; see ice-9/history.scm:
      ;;(add-hook! before-print-hook save-value)
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
      (run-repl* repl #;prompting-meta-read))))

(define* (start-nx-repl #:optional (language (current-language)) #:key debug)
  (start-nx-repl* language debug #;prompting-meta-read))

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
