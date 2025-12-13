;;; nyacc/lang/julia/mach.scm - grammar file for Julia

;; Copyright (C) 2018,2020,2021,2025 Matthew Wette
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

;; see https://github.com/julia-vscode/CSTParser.jl

;; Julia has no documented syntax.  We will need to scrounge through 
;; the manual, example code and the sources to extract something.
;; However, the parser is written in "flisp" ~= Scheme

;; Everything is an expression, but we use "statement" as a convenience
;; to distinguish context: expressions may be part of a body where they
;; are terminated by \n or ;

;; lexer special:
;; 1) 2im => imaginary
;; 2) """ .... """ string literal
;; 3) v"0.2" version number literal

;; grammar todos:
;; 1) operators see sec 14.6
;; 2) tuples (1, 2)
;; 3) keywords: false true missing
;; 4) multiple return values: works like python : use tuples
;; 5) named tuples: (a = 1, b = 2)

;;; Code:

(define-module (nyacc lang julia mach)
  #:export (julia-spec julia-mach gen-julia-files)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (ice-9 pretty-print)
  )

(define julia-spec
  (lalr-spec
   (notice (string-append "Copyright 2018,2020,2021 Matthew R. Wette"
			  license-lgpl3+))
   ;;(expect 1)
   (start translation-unit)
   (grammar

    (translation-unit
     ;;("struct" ident "{" type-list "}" "<:" what)
     (function-defn)
     (stmt-expr)
     )

    (function-defn
     ("function" ident "(" arg-list ")" term stmt-list term "end" term
      ($$ `(function-defn ,$2 ,$4 ,$7)))
     ("function" ident "(" arg-list ")" "=" expr term
      ($$ `(function-expr ,$1 ,$4 $7)))
     )

    (arg-list
     (arg-list-1 ($$ (tl->list $1))))
    (arg-list-1
     (ident ($$ (make-tl 'arg-list $1)))
     (arg-list-1 "," ident ($$ (tl-append $1 $3))))

    (lval-list (expr-list))

    #|
    (tuple-expr
     ("(" ")" ($$ '(tuple-expr)))
     ("(" tuple-guts ")" ($$ (tl->list $2))))
     ("(" tuple-guts "," ")" ($$ (tl->list $2))))
    (tuple-guts
     (expr ($$ (make-tl 'tuple-expr $1)))
     (tuple-guts "," expr  ($$ (tl-append $1 $3))))
    |#
    
    (expr-list
     (expr-list-1 ($$ (tl->list $1))))
    (expr-list-1
     (expr-no-assn ($$ (make-tl 'expr-list $1)))
     (expr-list-1 "," expr-no-assn ($$ (tl-append $1 $3))))
    
    (stmt-list
     (stmt-list-1 ($$ (tl->list $1))))
    (stmt-list-1
     (stmt-expr)
     (stmt-list-1 stmt-expr))

    (stmt-expr
     (if-expr term)
     (return-expr term)
     (lval-list "=" expr-no-assn)
     (assn-expr)
     )

    (return-expr
     ("return" expr)
     )

    ;; 18 46
    (if-expr
     ("if" expr term stmt-list "end")
     ("if" expr term stmt-list "else" stmt-list "end")
     ("if" expr term stmt-list elseif-list "end")
     ("if" expr term stmt-list elseif-list "else" stmt-list "end")
     )
    (elseif-list
     (elseif-list-1 ($$ (tl->list $1))))
    (elseif-list-1
     ("elseif" expr term stmt-list)
     (elseif-list-1 "elseif" expr term stmt-list))

    (expr (assn-expr))
    (expr-no-assn (keyval-expr))

    (assn-expr
     (keyval-expr)
     (keyval-expr "=" assn-expr)
     #|
     (keyval-expr "+=" assn-expr)
     (keyval-expr "-=" assn-expr)
     (keyval-expr "*=" assn-expr)
     (keyval-expr "/=" assn-expr)
     (keyval-expr "//=" assn-expr)
     (keyval-expr "\\=" assn-expr)
     (keyval-expr "^=" assn-expr)
     (keyval-expr "%=" assn-expr)
     (keyval-expr "|=" assn-expr)
     (keyval-expr "&=" assn-expr)
     (keyval-expr "<<=" assn-expr)
     (keyval-expr ">>=" assn-expr)
     (keyval-expr ">>>=" assn-expr)
     |#
     )

    (keyval-expr
     (cond-expr)
     ;;(cond-expr "=>" keyval-expr)
     )

    (cond-expr
     (logor-expr)
     ;;(cond-expr "?" logor-expr ":" logor-expr)
     )

    (logor-expr
     (logand-expr)
     (logand-expr "||" logor-expr)
     )

    (logand-expr
     (rel-expr)
     (rel-expr "&&" logand-expr)
     )

    (rel-expr
     (??1-expr)
     (rel-expr ">" rel-expr)
     #|
     (rel-expr "<" rel-expr)
     (rel-expr ">=" rel-expr)
     (rel-expr "<=" rel-expr)
     (rel-expr "==" rel-expr)
     (rel-expr "===" rel-expr)
     (rel-expr "!=" rel-expr)
     (rel-expr "!==" rel-expr)
     (rel-expr "<:" rel-expr)
     |#
     )

    (??1-expr
     (pipe-expr)
     ;;(pipe-expr "<|" ??1-expr)
     )

    (pipe-expr
     (colon-expr)
     ;;(pipe-expr "|>" colon-expr)
     )

    (colon-expr
     (add-expr)
     #|
     (add-expr ":" add-expr)
     (add-expr ":" add-expr ":" add-expr)
     (add-expr ".." add-expr)
     |#
     )

    (add-expr
     (mul-expr)
     (add-expr "+" mul-expr)
     ;;(add-expr "-" mul-expr)
     ;;(add-expr "|" mul-expr)
     )

    (mul-expr
     (frac-expr)
     (mul-expr "*" frac-expr)
     #|
     (mul-expr "/" frac-expr)
     (mul-expr "%" frac-expr)
     (mul-expr "&" frac-expr)
     (mul-expr "\\" frac-expr)
     |#
     )

    (frac-expr
     (shift-expr)
     ;;(frac-expr "//" shift-expr)
     )

    (shift-expr
     (unary-expr)
     #|
     (shift-expr "<<" unary-expr)
     (shift-expr ">>" unary-expr)
     (shift-expr ">>>" unary-expr)
     |#
     )

    (unary-expr
     (exp-expr)
     #|
     ("+" unary-expr)
     ("-" unary-expr)
     ("!" unary-expr)
     ("~" unary-expr)
     ("<:" unary-expr)
     (">:" unary-expr)
     |#
     )

    (exp-expr
     (ttag-expr)
     ;;(ttag-expr "^" exp-expr)
     )

    (ttag-expr  ;; type-tag
     (sel-expr)
     ;;(ttag-expr "::" sel-expr ($$ (type-tag ,$1 ,$3)))
     )

    (sel-expr
     (primary-expr)
     ;;(sel-expr "." primary-expr)
     )
    
    (primary-expr
     (ident)
     (number)
     (string)
     ("(" expr ")" ($$ $2))
     ("[" "]" ($$ '(matrix)))
     ;;("[" matrix-row-list "]" ($$ (tl->list $2)))
     (compound-expr)
     )

    (compound-expr
     ("begin" stmt-list "end")
     ;;("(" stmt-list ")")
     )

    #|
    ;; hcat in julia
    (matrix-row-list
     (matrix-row ($$ (make-tl 'matrix (tl->list $1))))
     (matrix-row-list row-term matrix-row ($$ (tl-append $1 (tl->list $3)))))
    (row-term (";") ("\n"))

    ;; vcat in julia
    (matrix-row
     (expr ($$ (make-tl 'row $1)))
     (matrix-row "," expr ($$ (tl-append $1 $3))))
    |#

    (term (";"))
    ;;(term (";") ("\n"))

    (ident ($ident))
    (number (integer) (float))
    (integer ($fixed))
    (float ($float))
    (string ($string))
    
    )))


;; === parsers ==========================

(define julia-mach (hashify-machine (make-lalr-machine julia-spec)))
#;(define julia-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine julia-spec))
   #:keep 0 #:keepers '($code-comm $lone-comm sp "\n")))

#|
;; Build an automaton for expressions to be used as Guile language.
;; Guile wants to see one statement at a time, so replace 'prog' start
;; with 'stmt' start.
(define juliaia-spec
  (restart-spec julia-spec 'stmt))

;; For purpose of demo, do not hashify the interactive one.
;; But the machine must have compacted tables!
(define stmt-mach
  (compact-machine
    (make-lalr-machine stmt-spec)))
|#

;; Procedure to generate actions and tables.
(define (gen-julia-files)
  (write-lalr-actions 
   julia-mach "mach.d/julia-act.scm" #:prefix "julia-")
  (write-lalr-tables 
   julia-mach "mach.d/julia-tab.scm" #:prefix "julia-")
  )

;; --- last line ---
