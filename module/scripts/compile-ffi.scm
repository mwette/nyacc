;;; scripts/compile-ffi.scm --- NYACC's command-line FFI compiler

;; Copyright (C) 2017-2019 Matthew R. Wette
;;
;; This program is free software; you can redistribute it and/or
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Author: Matt Wette <mwette@alumni.caltech.edu>

;;; Commentary:

;; Usage: compile-ffi [ARGS]
;; Type `compile-ffi --help' for help.

;;; Code:

(define-module (scripts compile-ffi)
  #:use-module (nyacc lang c99 ffi-help)
  #:use-module (system base language)
  #:use-module ((system base compile) #:select (compile-file))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:version (0 99 2))

(define *ffi-help-version* "0.99.2")

(define %summary
  "Compile a ffi-file (C interface spec) to Scheme (or maybe .go).")

(define (sfmt fmt . args)
  (apply simple-format (current-error-port) fmt args)
  (force-output (current-error-port)))

(define (fail fmt . args)
  (simple-format  (current-error-port) "compile-ffi: error: ")
  (apply simple-format (current-error-port) fmt args)
  (newline (current-error-port))
  (exit 1))

(define (warn fmt . args)
  (simple-format  (current-error-port) "compile-ffi: warning: ")
  (apply simple-format (current-error-port) fmt args)
  (newline (current-error-port)))

(define (note fmt . args)
  (simple-format  (current-error-port) "compile-ffi: notice: ")
  (apply simple-format (current-error-port) fmt args)
  (newline (current-error-port)))

(define (show-version)
  (simple-format #t "compile-ffi ~A\n" *ffi-help-version*))

(define (acons/seed key val seed)
  (acons key (cons val (or (assq-ref seed key) '())) seed))

(define (show-usage)
  (simple-format #t "Usage: compile [OPTION] FILE
Generate a Guile Scheme file from the source FFI file FILE.

  -h, --help           print this help message

  -L  --load-path=DIR  add DIR to the front of the module load path
  -I  --inc-dir=DIR    add DIR to list of dir's to search for C headers
  -o, --output=OFILE   write output to OFILE
  -d, --debug=x,y      set debug flags: echo-decl, parse
  -X, --no-exec        don't generate .go file(s)
  -R, --no-recurse     don't do recursive compile on dep's

Report bugs to https://savannah.nongnu.org/projects/nyacc.\n"))
  
(define options
  ;; specification of command-line options
  ;; (option (char str) req-arg? opt-arg? proc)
  (list
   (option '(#\h "help") #f #f
	   (lambda (opt name arg opts files)
	     (values (acons 'help #t opts) files)))
   (option '("version") #f #f
	   (lambda (opt name arg opts files)
	     (show-version) (exit 0)))
   (option '(#\d "debug") #t #f
	   (lambda (opt name arg opts files)
	     (values (acons 'debug arg opts) files)))
   (option '(#\o "output") #t #f
	   (lambda (opt name arg opts files)
	     (if (assoc-ref opts 'output-file)
		 (fail "`-o' option cannot be specified more than once"))
	     (values (acons 'output arg opts) files)))
   (option '(#\L "load-path") #f #f
	   (lambda (opt name arg opts files)
	     (values (acons/seed 'load-path arg opts) files)))
   (option '(#\I "inc-dir") #t #f
	   (lambda (opt name arg opts files)
	     (values (acons/seed 'inc-dirs arg opts) files)))
   (option '(#\R "dont-recurse") #f #f
	   (lambda (opt name arg opts files)
	     (values (acons 'no-recurse #t opts) files)))
   (option '(#\X "no-exec") #f #f
	   (lambda (opt name arg opts files)
	     (values (acons 'no-exec #t opts) files)))
   (option '("any-suffix") #f #f
	   (lambda (opt name arg opts files)
	     (values (acons 'any-sufffix #t opts) files)))
   ))

;; from scripts/compile.scm
(define (parse-args args)
  (args-fold args
	     options
             (lambda (opt name arg files opts)
               (fail "unrecognized option: ~S" name)
               (exit 1))
             (lambda (file opts files)
	       (or (assq-ref opts 'any-suffix)
		   (string-suffix? ".ffi" file)
		   (fail "expecting .ffi suffix"))
	       (values opts (cons file files)))
	     '() '()))

;; --- new code to check dependencies -------------------------------------------

;; may not work yet due to 
;; ... https://debbugs.gnu.org/cgi/bugreport.cgi?bug=15602


(define (sf fmt . args) (apply simple-format #t fmt args))

(define (more-recent? ffi-file scm-file)
  ;; copied from ice-9/boot-9.scm
  (let ((stat1 (stat ffi-file)) (stat2 (stat scm-file)))
    (or (> (stat:mtime stat1) (stat:mtime stat2))
	(and (= (stat:mtime stat1) (stat:mtime stat2))
	     (>= (stat:mtimensec stat1)
		 (stat:mtimensec stat2))))))

(define (find-in-path file)
  (let loop ((pathl %load-path))
    (if (null? pathl) #f
	(let ((path (string-append (car pathl) "/" file)))
	  (if (access? path R_OK) path
	      (loop (cdr pathl)))))))

;; Given module spec and list of dep's, return list of out-of-date dep's.
;; This routine assumes the scm file is in the same dir as the ffi file.
(define (check-ffi-deps ffimod uses)
  (fold-right
   (lambda (fmod seed)
     (let* ((base (string-join (map symbol->string fmod) "/"))
	    (xffi (find-in-path (string-append base ".ffi")))
	    (xscm (find-in-path (string-append base ".scm"))))
       (unless xffi (fail "compiled dependent ~S not found" fmod))
       (cond
	((not xscm) (cons xffi seed))
	((more-recent? xffi xscm) (cons xffi seed))
	(else seed))))
   '() uses))

(define-syntax find-ffi-uses
  (lambda (x)
    (syntax-case x ()
      ((_ key val rest ...)
       (eq? (syntax->datum #'key) #:use-ffi-module)
       #'(cons (quote val) (find-ffi-uses rest ...)))
      ((_ attr rest ...) #'(find-ffi-uses rest ...))
      ((_) #''()))))

(define-syntax-rule (define-ffi-module path-list attr ...)
  (check-ffi-deps (quote path-list) (find-ffi-uses attr ...)))
(export define-ffi-module)

(define (updated-ffi-deps file options)
  (unless (access? file R_OK)
    (fail "not found: ~S" file))
  (call-with-input-file file
    (lambda (iport)
      (let ((env (make-fresh-user-module)))
	(eval '(use-modules (scripts compile-ffi)) env)
	(let loop ((exp (read iport)))
	  (cond
	   ((eof-object? exp) (fail "no `define-ffi-module' in ~S" file))
	   ((and (pair? exp) (eqv? 'define-ffi-module (car exp))) (eval exp env))
	   (else (loop (read iport)))))))))

(define (ensure-ffi-deps file options)
  (for-each
   (lambda (dep-file)
     (cond
      ((assq-ref options 'no-recurse)
       (warn "please recompile `~A'" dep-file))
      (else
       (compile-ffi dep-file (acons 'no-exec #f options)))))
   (updated-ffi-deps file options)))

;; -----------------------------------------------------------------------------

(define (cleanup path)
  (basename path))

(define* (compile-ffi ffi-file options #:key module)
  (let* ((base (string-drop-right ffi-file 4))
	 (scm-file (string-append base ".scm")))
    (ensure-ffi-deps ffi-file options)
    (catch 'ffi-help-error
      (lambda ()
	(sfmt "compiling `~A' ...\n" (cleanup ffi-file))
	(compile-ffi-file ffi-file options)
	(sfmt "... wrote `~A'\n" (cleanup scm-file)))
      (lambda (key fmt . args)
	(apply fail fmt args)
	(exit 1)))
    (unless (assq-ref options 'no-exec)
      (sfmt "compiling `~A' ...\n" (cleanup scm-file))
       (let ((go-file (compile-file scm-file
				   #:from 'scheme #:to 'bytecode
				   #:opts '())))
	(load-compiled go-file)
	(sfmt "... wrote `~A'\n" (cleanup go-file)))
      (sleep 1))))

(define (main . args)
  (call-with-values
      (lambda () (parse-args args))
    (lambda (opts files)
      (when (or (assq-ref options 'help) (null? files)) (show-usage) (exit 0))
      (for-each (lambda (file) (compile-ffi file opts)) (reverse files))))
  (exit 0))

;;; Todo:

;; 1) remove output file on error? => generate default output file here

;; --- last line ---
