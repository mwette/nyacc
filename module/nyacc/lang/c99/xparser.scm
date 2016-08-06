;;; nyacc/lang/c99/xparser.scm - copied from parser.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
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
  #:export (parse-cx parse-c99x)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang c99 cpp)
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:use-module ((sxml xpath) #:select (sxpath))
  )

(include-from-path "nyacc/lang/c99/mach.d/c99xtab.scm")
(include-from-path "nyacc/lang/c99/body.scm")
(include-from-path "nyacc/lang/c99/mach.d/c99xact.scm")

;; Parse given a token generator.  Uses fluid @code{*info*}.
(define raw-parser
  (make-lalr-parser 
   (list
    (cons 'len-v len-v)
    (cons 'pat-v pat-v)
    (cons 'rto-v rto-v)
    (cons 'mtab mtab)
    (cons 'act-v act-v))))

(define (run-parse)
  (let ((info (fluid-ref *info*)))
    (raw-parser (gen-c-lexer) #:debug (cpi-debug info))))

;; @item parse-cx [#:cpp-defs def-a-list] [#:debug bool]
;; This needs to be explained in some detail.
;; [#:tyns '("foo_t")]
(define* (parse-c99x xstr
		   #:key (cpp-defs '()) (tn-dict '()) (debug #f) (tyns '()))
  (catch
   'parse-error
   (lambda ()
     (let ((info (make-cpi debug cpp-defs '(".") tn-dict)))
       (set-cpi-ptl! info (cons tyns (cpi-ptl info)))
       (with-fluid*
	   *info* info
	   (lambda ()
	     (with-input-from-string xstr
	       (lambda ()
		   (raw-parser (gen-c-lexer #:mode 'code) #:debug debug)))))))
   (lambda (key fmt . rest)
     (apply simple-format (current-error-port) (string-append fmt "\n") rest)
     #f)))

(define parse-cx parse-c99x)

;; --- last line ---
