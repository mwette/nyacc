;;; nyacc/lang/dxl/parser.scm
;;;
;;; Copyright (C) 2015,2016 Matthew R. Wette
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

;; dxl parser

(define-module (nyacc lang dxl parser)
  #:export (parse-dxl)
  #:use-module (nyacc lex)
  #:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc util)
  #:use-module (nyacc lang util)
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  ;;#:use-module ((sxml xpath) #:select (sxpath))
  )

;; utility routines
;; match table
;; lexical analyzer
;; actions

(include-from-path "nyacc/lang/dxl/mach.d/dxltab.scm")
(include-from-path "nyacc/lang/dxl/body.scm")
(include-from-path "nyacc/lang/dxl/mach.d/dxlact.scm")

(define *info* (make-fluid))

;; Parse given a token generator.  Uses fluid @code{*info*}.
(define raw-parser
  (make-lalr-parser 
   (list
    (cons 'len-v len-v)
    (cons 'pat-v pat-v)
    (cons 'rto-v rto-v)
    (cons 'mtab mtab)
    (cons 'act-v act-v))))

;; @item parse-dxl [#:debug bool])
(define* (parse-dxl #:key debug)
  (catch
   'parse-error
   (lambda ()
     (let ((info '())) ;;(make-dxi)))
       (with-fluid*
	   *info* info
	   (lambda ()
	     (raw-parser (gen-dxl-lexer) #:debug debug)))))
   (lambda (key fmt . rest)
     (apply simple-format (current-error-port) fmt rest)
     #f)))

;; --- last line
