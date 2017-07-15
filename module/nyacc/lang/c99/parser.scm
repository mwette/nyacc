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
  #:export (parse-c99)
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
(define c99-raw-parser
  (let ((parser (make-lalr-parser
		     (list (cons 'len-v c99-len-v) (cons 'pat-v c99-pat-v)
			   (cons 'rto-v c99-rto-v) (cons 'mtab c99-mtab)
			   (cons 'act-v c99-act-v)))))
    (lambda* (lexer #:key (debug #f))
      (catch
       'nyacc-error
       (lambda () (parser lexer #:debug debug))
       (lambda (key fmt . args)
	 (report-error fmt args)
	 ;;(pop-input)			; not sure this is correct
	 (throw 'c99-error "C99 parse error"))))))

;; This is used to parse included files at top level.
(define (run-parse)
  (let ((info (fluid-ref *info*)))
    (c99-raw-parser (gen-c-lexer #:mode 'decl) #:debug (cpi-debug info))))

;; @deffn {Procedure} parse-c99 [#:cpp-defs def-a-list] [#:inc-dirs dir-list] \
;;               [#:mode ('code|'file|'decl)] [#:debug bool]
;; This needs to be explained in some detail.
;; tdd = typedef dict: (("<time>" time_t) ... ("<unistd.h>" ...))
;; Default mode is @code{'code}.
;; @example
;; (with-input-from-file "abc.c"
;;   (parse-c #:cpp-defs '("ABC=123"))
;;            #:inc-dirs '(("." "./incs" "/usr/include"))
;;            #:inc-help (append '("myinc.h" "foo_t" "bar_t") c99-std-help)
;;            #:mode 'file))
;; @end example
;; Note: for @code{file} mode user still needs to make sure CPP conditional
;; expressions can be fully evaluated, which may mean adding compiler generated
;; defines (e.g., using @code{gen-cpp-defs}).
;; @end deffn
(use-modules (ice-9 pretty-print))
(define* (parse-c99 #:key
		    (cpp-defs '())	; CPP defines
		    (inc-dirs '())	; include dirs
		    (inc-help '())	; include helpers
		    (mode 'code)	; mode: 'file, 'code or 'decl
=		    (xdef? #f)		; pred to determine expand
		    (debug #f))		; debug
  (let ((info (make-cpi debug cpp-defs (cons "." inc-dirs) inc-help)))
    (if (and (pair? cpp-defs) (pair? (car cpp-defs)))
	;; old: ("ABC" . "123"), new: "ABC=123"
	(error "usage deprecated: use #:cpp-defs '(\"ABC=123\")"))
    (with-fluids ((*info* info)
		  (*input-stack* '()))
      (catch 'c99-error
	(lambda () (c99-raw-parser
		    (gen-c-lexer #:mode mode #:xdef? xdef?)
		    #:debug debug))
	(lambda (key fmt . args)
	  (report-error fmt args)
	  (when #f
	    (simple-format (current-error-port)
			   "input-stack: ~S\ncpi-defs:\n"
			   (fluid-ref *input-stack*))
	    (pretty-print (cpi-defs info) (current-error-port)))
	  #f)))))

;; --- last line ---
