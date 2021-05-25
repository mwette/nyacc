;;; nyacc/lang/julia/mach.scm - grammar file for Julia

;; Copyright (C) 2018,2020,2021 Matthew R. Wette
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
   (start translation-unit)
   (grammar

    (translation-unit
     (ident "=" expr term)
     ;;("struct" ident "{" type-list "}" "<:" what)
     (function-defn)
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
     (arg-list-1 "," ident ($$ (tl->append $1 $3))))

    (tuple-expr
     ("(" ")" ($$ '(tuple-expr)))
     ("(" expr-list-1 ")" ($$ `(tuple-expr ,@(cdr (tl->list $2))))))
    (expr-list
     (expr-list-1 ($$ (tl->list $1))))
    (expr-list-1
     (expr ($$ (make-tl 'expr-list $1)))
     (expr-list-1 "," opt-nl expr ($$ (tl-append $1 $2))))
    
    (stmt-list
     (stmt-list-1 ($$ (tl->list $1))))
    (stmt-list-1
     (stmt-expr)
     (stmt-list-1 stmt-expr))

    (stmt-expr
     (if-expr term)
     (return-expr term)
     (assn-expr term)
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

    #;(function-decl
    ("function" ident "(" arg-list ")" term
    )
    )

    (expr
     (assn-expr)
     )

    (assn-expr
     (colon-expr)
     (arg-list "=" colon-expr)
     )

    (colon-expr ;; ???
     (or-expr ":" or-expr ($$ `(colon-expr ,$1 ,$3)))
     (or-expr ":" or-expr ":" or-expr ($$ `(colon-expr ,$1 ,$3 ,$5)))
     (or-expr ":" "end" ($$ `(colon-expr ,$1 (end))))
     (or-expr ":" or-expr ":" "end" ($$ `(colon-expr ,$1 ,$3 (end)))))

    (or-expr
     (and-expr)
     (or-expr "|" and-expr ($$ `(or ,$1 ,$3))))

    (and-expr
     (equality-expr)
     (and-expr "&" equality-expr ($$ `(and ,$1 ,$3))))

    (equality-expr
     (rel-expr)
     (equality-expr "==" rel-expr ($$ `(eq ,$1 ,$3)))
     (equality-expr "~=" rel-expr ($$ `(ne ,$1 ,$3))))

    (rel-expr
     (add-expr)
     (rel-expr "<" add-expr ($$ `(lt ,$1 ,$3)))
     (rel-expr ">" add-expr ($$ `(gt ,$1 ,$3)))
     (rel-expr "<=" add-expr ($$ `(le ,$1 ,$3)))
     (rel-expr ">=" add-expr ($$ `(ge ,$1 ,$3))))

    (add-expr
     (mul-expr)
     (add-expr "+" mul-expr ($$ `(add ,$1 ,$3)))
     (add-expr "-" mul-expr ($$ `(sub ,$1 ,$3)))
     (add-expr ".+" mul-expr ($$ `(dot-add ,$1 ,$3)))
     (add-expr ".-" mul-expr ($$ `(dot-sub ,$1 ,$3))))

    (mul-expr
     (unary-expr)
     (mul-expr "*" unary-expr ($$ `(mul ,$1 ,$3)))
     (mul-expr "/" unary-expr ($$ `(div ,$1 ,$3)))
     (mul-expr "\\" unary-expr ($$ `(ldiv ,$1 ,$3)))
     (mul-expr "^" unary-expr ($$ `(pow ,$1 ,$3)))
     (mul-expr ".*" unary-expr ($$ `(dot-mul ,$1 ,$3)))
     (mul-expr "./" unary-expr ($$ `(dot-div ,$1 ,$3)))
     (mul-expr ".\\" unary-expr ($$ `(dot-ldiv ,$1 ,$3)))
     (mul-expr ".^" unary-expr ($$ `(dot-pow ,$1 ,$3)))
     (mul-expr "//" unary-expr ($$ `(idiv ,$1 ,$3)))
     )

    (unary-expr
     (postfix-expr)
     ("-" postfix-expr ($$ `(neg ,$2)))
     ("+" postfix-expr ($$ $2))
     ("~" postfix-expr ($$ `(not ,$2))))

    (postfix-expr
     (primary-expr)
     (postfix-expr "'" ($$ `(transpose ,$1)))
     (postfix-expr ".'" ($$ `(conj-transpose ,$1)))
     (postfix-expr "(" expr-list ")" ($$ `(aref-or-call ,$1 ,(tl->list $3))))
     (postfix-expr "(" ")" ($$ `(aref-or-call ,$1 (expr-list))))
     (postfix-expr "." ident ($$ `(sel ,$3 ,$1))))
    
    (primary-expr
     (ident)
     (number)
     (string)
     ("(" expr ")" ($$ $2))
     ("[" "]" ($$ '(matrix)))
     ("[" matrix-row-list "]" ($$ (tl->list $2)))
     ("{" "}" ($$ '(cell-array)))
     ("{" matrix-row-list "}" ($$ (cons 'cell-array (cdr (tl->list $2)))))
     (compound-expr)
     )

    (compound-expr
     ("begin" stmt-list "end")
     ("(" stmt-list ")")
     )

    ;; hcat in julia
    (matrix-row-list
     (matrix-row ($$ (make-tl 'matrix (tl->list $1))))
     (matrix-row-list row-term matrix-row ($$ (tl-append $1 (tl->list $3)))))
    (row-term (";") (nl))

    ;; vcat in julia
    (matrix-row
     (expr ($$ (make-tl 'row $1)))
     (matrix-row "," expr ($$ (tl-append $1 $3))))

    (term (";") ("\n"))
    (opt-nl ($empty) (nl))
    (nl ("\n"))

    (ident ($ident))
    (number (integer) (float))
    (integer ($fixed))
    (float ($float))
    (string ($string))
    
    )))


;; === parsers ==========================

(define julia-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine julia-spec))))

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
