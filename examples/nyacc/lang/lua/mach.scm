;; nyacc/lang/lua/mach.scm

;; Copyright (C) 2017,2018 Matthew R. Wette
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>

(define-module (nyacc lang lua mach)
  #:export (lua-spec
	    )
  #:use-module (nyacc lang util)
  #:use-module (nyacc lalr)
  )

(define lua-spec
  (lalr-spec
   (start block)
   (grammar

    (block ("{" stmt opt-semi "}" opt-finish))

    (opt-semi ($empty) (";"))

    (opt-finish ("finish" opt-semi))

    (stmt
     (var-list "=" exprs)
     (call)
     ("do" block "end")
     ("while" expr "do" block "end")
     ("repeat" block "until" expr)
     ("if" expr "then" block "end")
     ("if" expr "then" block "else" block "end")
     ("if" expr "then" block "elseif" expr "then" block "end")
     ("if" expr "then" block "elseif" expr "then" block "else" block "end")
     ("for" name "=" expr "," expr "do" block "end")
     ("for" name "=" expr "," expr "," expr "do" block "end")
     ("for" name "," name "in" expr "do" block "end")
     ("function" func-name "(" ")" block "end")
     ("function" func-name "(" params ")" block "end")
     ("local" name-list) 
     ("local" name-list "=" exprs) 
     )
    (name-list
     (name)
     (name-list "," name)
     )

    (finish
     ("return")
     ("return" exprs)
     ("break")
     ("break" name)
     )

    (func-name
     (name)
     (name ":" key)
     (name key-list)
     (name key-list ":" key)
     )
    (key (name))

    (var-list
     (var)
     (var-list "," var)
     )

    (params
     ("...")
     (name-list)
     (name-list "...")
     )

    (exprs
     (expr)
     (expr "," expr)
     )

    (expr
     (primary)
     (var)
     (call)
     (expr binop expr)
     (unop expr)
     )

    (primary
     ("nil")
     (number)
     (literal)
     ("%" name)
     (table-cons)
     ("function" "(" ")" block "end")
     ("function" "(" params ")" block "end")
     ("(" expr ")")
     )

    (var
     (name)
     (primary index)
     (var index)
     (call index)
     )

    (index
     ("[" expr "]")
     ("." key)
     )

    (call
     (primary ":" key args)
     (primary args)
     (var ":" key args)
     (var args)
     (call ":" key args)
     (call args)
     )

    (args
     ("(" exprs ")")
     ("(" ")")
     (table-cons)
     (literal)
     )

    (table-cons
     ("{" fields "}")
     ("{" "}")
     )

    (fields
     (expr-fields ";" mapping-fields)
     (expr-fields ";")
     (expr-fields)
     (mapping-fields ";" expr-fields)
     (mapping-fields ";")
     (mapping-fields)
     (";" expr-fields)
     (";" mapping-fields)
     )

    (expr-fields
     (exprs ",")
     (exprs)
     )

    (mapping-fields
     (mapping-field)
     (mapping-fields "," mapping-field)
     (mapping-fields ",")
     )

    (mapping-field
     ("[" expr "]" "=" expr)
     (key "=" expr)
     )

    (binop
     ("+") ("-") ("*") ("/")
     ("^") ("..")
     ("and") ("or")
     ("<") ("<=") (">") (">=") ("==") ("~=")
     )

    (unop
     ("-") ("not")
     )
    
    (key-hack
     (name) ("and") ("break") ("do") ("end") ("else") ("elseif") ("for")
     ("function") ("global") ("if") ("in") ("local") ("nil") ("not") ("or")
     ("return") ("repeat") ("then") ("until") ("while")
     )
      
    )))

;; --- last line ---
