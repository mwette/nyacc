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

(define-module (lang c parser)
  #:export (parse-c)
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:use-module ((sxml fold) #:select (foldts*-values foldts))
  #:use-module ((sxml xpath) #:select (sxpath))
  #:use-module (nyacc lex)
  #:use-module (nyacc lalr)
  #:use-module (lang util)
  #:use-module (lang c cpp)
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
(define* (parse-c #:key (cpp-defs '()) (inc-dirs '()) (mode 'file) debug)
  (catch
   'parse-failed
   (lambda ()
     (let ((info (make-cpi cpp-defs (cons "." inc-dirs))))
       (with-fluid*
	   *info* info
	   (lambda ()
	     (raw-parser (gen-c-lexer #:mode mode) #:debug debug)))))
   (lambda ()
     (fmterr "parse failed")
     #f)))

;; --- last line
