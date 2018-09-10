;;; lang/modelica/parser.scm - NYACC Modelica parser

;; Copyright (C) 2018 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or
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

;;; Description:

;; This is a Modelica parser for Guile.

;;; Code:

(define-module (nyacc lang modelica parser)
  #:export (parse-modelica read-mo-file)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang sx-util)
  #:use-module (nyacc lang util))

(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(use-modules (ice-9 pretty-print))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

(include-from-path "nyacc/lang/modelica/body.scm")

;; === file parser ===================

(include-from-path "nyacc/lang/modelica/mach.d/mo-tab.scm")
(include-from-path "nyacc/lang/modelica/mach.d/mo-act.scm")

;; does not support Q-ident (single quoted identifier)
(define gen-mo-lexer
  (make-lexer-generator mo-mtab #:comm-skipper read-c-comm))

(define raw-parser
  (make-lalr-parser (acons 'act-v mo-act-v mo-tables)))

;; @deffn {Procedure} parse-modelica [#:debug #f] 
;; to be documented
;; @end deffn
(define* (parse-modelica #:key debug)
  (catch 'nyacc-error
    (lambda ()
      (raw-parser (gen-mo-lexer) #:debug #f))
    (lambda (key fmt . rest)
      (apply simple-format (current-error-port) fmt rest)
      #f)))

;; @deffn {Procedure} read-mo-file port env => sxml
;; Read file unit from port and return SXML AST.
;; @end deffn
(define (read-mo-file port env)
  (if (eof-object? (peek-char port))
      (read-char port)
      (with-input-from-port port parse-modelica)))

;; --- last line ---



