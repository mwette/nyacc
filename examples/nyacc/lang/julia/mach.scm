;;; nyacc/lang/julia/mach.scm - grammar file for Julia

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

;;; Notes:

;; Julia has no documented syntax.  We will need to scrounge through 
;; the manual, example code and the sources to extract something.

;; Everything is an expression.

;;; Code:

(define-module (nyacc lang julia mach)
  #:export (julia-spec
	    )
  #:use-module (nyacc lang util)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (ice-9 pretty-print)
  )

(define julia-spec
  (lalr-spec
   (notice (string-append "Copyright 2018 Matthew R. Wette" license-lgpl3+))
   (start top)
   (grammar

    (top
     (ident "=" expr term)
     ;;("struct" ident "{" type-list "}" "<:" what)
     (return-stmt)
     )

    (function-defn
     ("function" ident "(" arg-list ")" break
      )
     )

    (return-stmt
     ("return" expr))

    (compound-expr
     ("begin" expr-list "end")
     ("(" expr-list ")")
     )

    (if-expr
     ("if" expr break expr-list break "end")
     ("if" expr break expr-list break "else" expr-list break "end")
     ("if" expr break expr-list break elseif-list break "end")
     ("if" expr break expr-list break elseif-list "else" expr-list break "end"))
    (elseif-list-1
     ("elseif" expr break expr-list)
     (elseif-list-1 break "elseif" expr break expr-list))

    (expr-list (expr-list-1))
    (expr-list-1
     (expr)
     (expr-list-1 break expr)
     (expr-list-1 ";" expr))

    (break ("\n"))
    
    )))

;; === parsers ==========================

;;(define julia-mach
;;  (hashify-machine
;;   (compact-machine
;;    (make-lalr-machine julia-spec))))

;; --- last line ---
