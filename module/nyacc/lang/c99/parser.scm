;;; nyacc/lang/c99/parser.scm
;;;
;;; Copyright (C) 2015-2017 Matthew R. Wette
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by 
;;; the Free Software Foundation, either version 3 of the License, or 
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; C parser

(define-module (nyacc lang c99 parser)
  #:export (parse-c99
	    def-xdef? c99-std-dict
	    gen-c-lexer
	    gen-gcc-defs
	    )
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang c99 cpp)
  )

(include-from-path "nyacc/lang/c99/mach.d/c99tab.scm")
(include-from-path "nyacc/lang/c99/body.scm")
(include-from-path "nyacc/lang/c99/mach.d/c99act.scm")

;; Parse given a token generator.  Uses fluid @code{*info*}.
;; A little ugly wrt re-throw but
(define raw-parser
  (let ((c99-parser (make-lalr-parser
		     (list (cons 'len-v len-v) (cons 'pat-v pat-v)
			   (cons 'rto-v rto-v) (cons 'mtab mtab)
			   (cons 'act-v act-v)))))
    (lambda* (lexer #:key (debug #f))
      (with-throw-handler
       'nyacc-error
       (lambda () (c99-parser lexer #:debug debug))
       (lambda (key fmt . args) (apply throw 'c99-error fmt args))))))

;; This is used to parse included files at top level.
(define (run-parse)
  (let ((info (fluid-ref *info*)))
    (raw-parser (gen-c-lexer) #:debug (cpi-debug info))))

;; @deffn parse-c99 [#:cpp-defs def-a-list] [#:inc-dirs dir-list] \
;;               [#:mode ('code|'file)] [#:debug bool]
;; This needs to be explained in some detail.
;; tdd = typedef dict: (("<time>" time_t) ... ("<unistd.h>" ...))
;; Default mode is @code{'code}.
;; @example
;; (with-input-from-file "abc.c"
;;   (parse-c #:cpp-defs '(("ABC" . "123"))
;;            #:inc-dirs (append '("." "./incs" "/usr/include") c99-std-dict)
;;            #:td-dict '(("myinc.h" "foo_t" "bar_t"))
;;            #:mode 'file))
;; @end example
(define* (parse-c99 #:key
		    (cpp-defs '())	; CPP defines
		    (inc-dirs '())	; include dirs
		    (td-dict '())	; typedef dictionary
		    (mode 'code)	; mode: 'file or 'code
		    (xdef? #f)		; pred to determine expand
		    (debug #f))		; debug
  (catch
   'c99-error
   (lambda ()
     (let ((info (make-cpi debug cpp-defs (cons "." inc-dirs) td-dict)))
       (with-fluid*
	   *info* info
	   (lambda ()
	     (raw-parser (gen-c-lexer #:mode mode #:xdef? xdef?)
			 #:debug debug)))))
   (lambda (key fmt . rest)
     (report-error fmt rest)
     #f)))

(define parse-c parse-c99)

(use-modules (ice-9 rdelim))
(use-modules (ice-9 popen))
(use-modules (ice-9 regex))

;; @deffn gen-gcc-defs args  => '(("ABC" . "123") ...)
;; Generate a list of default defines produced by gcc.
(define gen-gcc-defs
  ;; @code{"gcc -dM -E"} will generate lines like @code{"#define ABC 123"}.
  ;; We generate and return a list like @code{'(("ABC" . "123") ...)}.
  (let ((rx (make-regexp "#define\\s+(\\S+)\\s+(.*)")))
    (lambda (args)
      (map
       (lambda (l)
	 (let ((m (regexp-exec rx l)))
	   (cons (match:substring m 1) (match:substring m 2))))
       (let ((ip (open-input-pipe "gcc -dM -E - </dev/null")))
	 (let iter ((lines '()) (line (read-line ip 'trim)))
	   (if (eof-object? line) lines
	       (iter (cons line lines) (read-line ip 'trim)))))))))

;; --- last line ---
