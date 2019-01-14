;;; nyacc/lang/calc/parser

;; Copyright (C) 2015-2019 Matthew R. Wette
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

(define-module (nyacc lang calc parser)
  #:export (parse-calc read-calc)	; parse full, read stmt
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util))

;;; Full parser

;; Include the reduction actions that get called when a production
;; rule is reduced.  This is a separate file to it could be changed
;; by hand and place in lexical context with routines called.
(include-from-path "nyacc/lang/calc/mach.d/calc-full-act.scm")

;; Include the automaton tables.  These are used by the parser defined
;; in nyacc/parser.scm.
(include-from-path "nyacc/lang/calc/mach.d/calc-full-tab.scm")

;; Generate a lexer.  Look in nyacc/lex.scm to see how this is formulated.
;; The object calc-mtab is defined in mach.d/calc-tab.scm.
(define gen-full-lexer
  (make-lexer-generator calc-full-mtab #:space-chars " \t"))

;; The raw parser is a procecure that parses (current-input-port)
;; given a lexical analyzer procedure.  See parse-calc below.
(define raw-full-parser
  (make-lalr-parser (acons 'act-v calc-full-act-v calc-full-tables)))

;; This is nominal procedure called by the user.  If called with
;; @code{#:debug #t} a trace of parser shifts and reductions will
;; be echoed to (current-error-port).
(define* (parse-calc #:key debug)
  (catch 'nyacc-error
    (lambda () (raw-full-parser (gen-full-lexer) #:debug debug))
    (lambda (key fmt . args)
      (apply simple-format (current-error-port) fmt args))))

;;; Stmt parser

(include-from-path "nyacc/lang/calc/mach.d/calc-stmt-act.scm")
(include-from-path "nyacc/lang/calc/mach.d/calc-stmt-tab.scm")

(define gen-stmt-lexer
  (make-lexer-generator calc-stmt-mtab #:space-chars " \t"))

;; This is interactive so that input does not have to end in eof-object.
(define raw-stmt-parser
  (make-lalr-parser (acons 'act-v calc-stmt-act-v calc-stmt-tables)
		    #:interactive #t))

(define (parse-stmt)
  (catch 'nyacc-error
    (lambda () (raw-stmt-parser (gen-stmt-lexer) #:debug #t))
    (lambda (key fmt . args)
      (apply simple-format (current-error-port) fmt args)
      (newline (current-error-port))
      #f)))

;; This is defined for use by Guile's extension language facility.
;; See ../../../language/spec.scm and compiler.scm.
(define (read-calc port env)
  (define (flush-input port)
    (let loop ((ch (read-char port)))
      (if (char=? #\newline) #f (loop (read-char port)))))

  (if (eof-object? (peek-char port))
      (read-char port)
      (let ((elt (with-input-from-port port parse-stmt)))
	(or elt (flush-input port)))))

;; --- last line ---
