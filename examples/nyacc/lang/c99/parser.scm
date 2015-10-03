;;; lang/c/parser.scm
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

(define-module (nyacc lang c99 parser)
  #:export (parse-c)
  #:use-module (nyacc lex)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang c99 cpp)
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:use-module ((sxml xpath) #:select (sxpath))
  )

;; utility routines
;; match table
;; lexical analyzer
;; actions

(include "tables.scm")
(include "pbody.scm")
(include "actions.scm")

;; Parse given a token generator.  Uses fluid @code{*info*}.
(define raw-parser
  (make-lalr-parser 
   (list
    (cons 'len-v len-v)
    (cons 'pat-v pat-v)
    (cons 'rto-v rto-v)
    (cons 'mtab mtab)
    (cons 'act-v act-v))))

(define (run-parse) (raw-parser (gen-c-lexer)))

;; @item parse-c [#:cpp-defs def-a-list] [#:inc-dirs dir-list] [#:debug bool] \
;;               [#:mode ('code|'file)]
;; This needs to be explained in some detail.
(define* (parse-c #:key (cpp-defs '()) (inc-dirs '()) (mode 'file) debug)
  (catch
   'parse-error
   (lambda ()
     (let ((info (make-cpi cpp-defs (cons "." inc-dirs))))
       (with-fluid*
	   *info* info
	   (lambda ()
	     (raw-parser (gen-c-lexer #:mode mode) #:debug debug)))))
   (lambda (key fmt . rest)
     (apply simple-format (current-error-port) fmt rest)
     #f)))

;; --- last line
