;;; nyacc/lang/c99/xparser.scm - copied from parser.scm
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

(define-module (nyacc lang c99 xparser)
  #:export (parse-c99x)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang c99 cpp)
  #:use-module ((srfi srfi-9) #:select (define-record-type)))

(include-from-path "nyacc/lang/c99/mach.d/c99xtab.scm")
(define c99-mtab c99x-mtab)
(include-from-path "nyacc/lang/c99/body.scm")
(include-from-path "nyacc/lang/c99/mach.d/c99xact.scm")

;; Parse given a token generator.  Uses fluid @code{*info*}.
(define c99x-raw-parser
  (let ((parser (make-lalr-parser 
		 (list (cons 'len-v c99x-len-v) (cons 'pat-v c99x-pat-v)
		       (cons 'rto-v c99x-rto-v) (cons 'mtab c99x-mtab)
		       (cons 'act-v c99x-act-v)))))
    (lambda* (lexer #:key (debug #f))
      (catch
       'nyacc-error
       (lambda () (parser lexer #:debug debug))
       (lambda (key fmt . args)
	 (report-error fmt args)
	 (pop-input)			; not sure this is right
	 (throw 'c99-error "C99 parse error"))))))

(define (run-parse)
  (let ((info (fluid-ref *info*)))
    (c99x-raw-parser (gen-c-lexer #:mode 'decl) #:debug (cpi-debug info))))

;; @deffn {Procedure} parse-c99x [#:cpp-defs defs] [#:debug bool] [tyns]
;; This needs to be explained in some detail.
;; [tyns '("foo_t")]
;; @end deffn
(define* (parse-c99x expr-string
		     #:optional
		     (tyns '())	; defined typenames
		     #:key
		     (cpp-defs '())	; CPP defines
		     (inc-help '())	; include helper
		     (xdef? #f)		; pred to determine expand
		     (debug #f))	; debug?
  (let ((info (make-cpi debug cpp-defs '(".") inc-help)))
    (set-cpi-ptl! info (cons tyns (cpi-ptl info)))
    (with-fluids ((*info* info)
		  (*input-stack* '()))
      (with-input-from-string expr-string
	(lambda ()
	  (catch 'c99-error
	    (lambda () (c99x-raw-parser
			(gen-c-lexer #:mode 'code #:xdef? xdef?)
			#:debug debug))
	    (lambda (key fmt . rest)
	      (report-error fmt rest)
	      #f)))))))

;; --- last line ---
