;;; nyacc/lang/matlab/parser.scm
;;;
;;; Copyright (C) 2016 Matthew R. Wette
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

(define-module (nyacc lang matlab parser)
  #:export (parse-ml)
  #:use-module (nyacc lex)
  #:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util))

(include-from-path "nyacc/lang/matlab/mach.d/mltab.scm")
(include-from-path "nyacc/lang/matlab/body.scm")
(include-from-path "nyacc/lang/matlab/mach.d/mlact.scm")

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
(define* (parse-ml #:key debug)
  (catch
   'parse-error
   (lambda ()
     (raw-parser (gen-matlab-lexer) #:debug debug))
   (lambda (key fmt . rest)
     (apply simple-format (current-error-port) fmt rest)
     #f)))

;; --- last line ---
