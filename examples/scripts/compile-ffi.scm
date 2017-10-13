;;; scripts/compile-ffi.scm --- NYACC's command-line FFI compiler

;; Copyright (C) 2017 Matthew R. Wette
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by 
;; the Free Software Foundation, either version 3 of the License, or 
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Author: Matt Wette <mwette@alumni.caltech.edu>

;;; Commentary:

;; Usage: compile-ffi [ARGS]
;; Type `compile-ffi --help' for help.

;;; Code:

(define-module (scripts compile-ffi)
  #:use-module (nyacc lang c99 ffi-help)
  #:use-module (system base language)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  )

(define %summary
  "Compile a ffi-file (C interface spec) to Scheme (or maybe .go).")

(define (fail fmt . args)
  (apply simple-format (current-error-port)
	 (string-append "error: " fmt "\n")
	 args)
  (exit 1))

(define (acons/seed key val seed)
  (acons key (cons val (or (assq-ref seed key) '())) seed))

(define options
  ;; specification of command-line options
  (list (option '(#\h "help") #f #f
		(lambda (opt name arg seed) (acons 'help #t seed)))
	(option '("version") #f #f
		(lambda (opt name arg seed) (show-version) (exit 0)))
	
	(option '(#\o "output") #t #f
		(lambda (opt name arg seed)
		  (if (assoc-ref seed 'output-file)
		      (fail "`-o' option cannot be specified more than once"))
		  (acons 'output arg seed)))
	
	(option '(#\L "load-path") #f #f
		(lambda (opt name arg seed)
		  (acons/seed 'load-path arg seed)))
	(option '(#\I "inc-dir") #t #f
		(lambda (opt name arg seed)
		  (acons/seed 'inc-dirs arg seed)))

	(option '(#\X "make-go") #f #f
		(lambda (opt name arg seed) (acons 'make-go #t seed)))
	))

;; from scripts/compile.scm
(define (parse-args args)
  (args-fold args options
             (lambda (opt name arg seed)
               (fail "unrecognized option: ~S" name)
               (exit 1))
             (lambda (file seed)
	       (if (assq-ref 'file seed)
		   (fail "only one input file can be specified"))
	       (or (assq-ref seed 'any-suffix)
		   (string-suffix? ".ffi" file)
		   (fail "expecting .ffi suffix"))
	       (acons 'file file seed))
	     '()))

(define *fh-version* "0.1.1")

(define (show-version)
  (simple-format #t "compile-ffi ~A\n" *fh-version*))

(define (compile-ffi . args)
  (use-modules (nyacc lang c99 ffi-help)) ; needed here, but why
  (let* ((options (parse-args args))
	 (file (assoc-ref options 'file))
	 (base (string-drop-right file 4))
	 (dot-scm (string-append base ".scm")))
    (if (or (assq-ref options 'help) (not file))
        (begin
          (simple-format #t "Usage: compile [OPTION] FILE...
Compile each Guile source file FILE into a Guile object.

  -h, --help           print this help message

  -L  --load-path=DIR  add DIR to the front of the module load path
  -I  --inc-dir=DIR    add DIR to list of dir's to search for C headers
  -o, --output=OFILE   write output to OFILE

Report bugs to https://savannah.nongnu.org/projects/nyacc.\n")
          (exit 0)))

    (catch 'ffi-help-error
      (lambda ()
	(compile-ffi-file file options))
      (lambda (key fmt . args)
	(apply simple-format (current-error-port)
	       (string-append "*** compile-ffi: " fmt "\n") args)
	(exit 1)))

    (if (assq-ref options 'make-go)
	(compile dot-scm))
    
    (exit 0)))

(define main compile-ffi)

;;; Todo:

;; 1) remove output file on error? => generate default output file here
;; 2) add arg to override default file suffix (i.e., `.ffi'): 'any-suffix

;; --- last line ---
