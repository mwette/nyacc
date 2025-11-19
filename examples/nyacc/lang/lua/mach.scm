;; nyacc/lang/lua/mach.scm

;; Copyright (C) 2017,2018 Matthew Wette
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
  #:export (lua-spec lua-mach
	    gen-lua-files)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lalr)
  )

(define lua-spec
  (lalr-spec
   (start block)
   (grammar

    (block ("{" stmt opt-semi "}" opt-finish))

    (opt-semi ($empty) (";"))

    (opt-finish (finish opt-semi))

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
     (name keys)
     (name keys ":" key)
     )

    (keys
     ("." key)
     (keys "." key)
     )

    (var-list
     (exprs)
     )

    (params
     ("...")
     (name-list)
     (name-list "...")
     )

    (exprs
     (expr)
     (exprs "," expr)
     )

    (expr
     (or-expr)
     )
    (or-expr
     (and-expr)
     (or-expr "or" and-expr))
    (and-expr
     (equ-expr)
     (and-expr "and" equ-expr))
    (equ-expr
     (rel-expr)
     (equ-expr "==" rel-expr)
     (equ-expr "~=" rel-expr))
    (rel-expr
     (add-expr)
     (rel-expr "<" add-expr)
     (rel-expr "<=" add-expr)
     (rel-expr ">" add-expr)
     (rel-expr ">=" add-expr))
    (add-expr
     (mul-expr)
     (mul-expr "+" add-expr)
     (mul-expr "-" add-expr))
    (mul-expr
     (una-expr)
     (mul-expr "*" una-expr)
     (mul-expr "/" una-expr))
    (una-expr
     (prim-expr)
     ("+" una-expr)
     ("-" una-expr))
    (prim-expr
     ("nil")
     (literal)
     ("%" name)
     (table-cons)
     ("function" "(" ")" block "end")
     ("function" "(" params ")" block "end")
     ("(" expr ")"))

    (primary (prim-expr))
    
    ;; todo: .. <= string-cat
     
    #|
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
     (string)
     ("%" name)
     (table-cons)
     ("function" "(" ")" block "end")
     ("function" "(" params ")" block "end")
     ("(" expr ")")
     )

    (binop
     ("+") ;; ("-") ("*") ("/")
     ("^") ("..")
     ("and") ("or")
     ("<") ("<=") (">") (">=") ("==") ("~=")
     )

    (unop
     ("-") ("not")
     )
    |#

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
     ;;(exprs ",")
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

    (literal
     (number)
     (string))

    (name ($ident))
    (number ($fixed) ($float))
    (string ($string))
    
    (key (name))
    #;(key
     (name) ("and") ("break") ("do") ("end") ("else") ("elseif") ("for")
     ("function") ("global") ("if") ("in") ("local") ("nil") ("not") ("or")
     ("return") ("repeat") ("then") ("until") ("while")
     )
      
    )))

;; === parsers ==========================

(define lua-mach
  (hashify-machine
   (compact-machine
    (make-lalr-machine lua-spec))))

(define luaia-spec (restart-spec lua-spec 'stmt))

(define luaia-mach
  (hashify-machine
   (compact-machine
    (make-lalr-machine luaia-spec))))

;; === automaton file generators =========

(define* (gen-lua-files #:optional (path "."))
  (define (mdir file) (mach-dir path file))
  (write-lalr-actions lua-mach (mdir "lua-act.scm.new") #:prefix "lua-")
  (write-lalr-tables lua-mach (mdir "lua-tab.scm.new") #:prefix "lua-")
  (let ((a (move-if-changed (mdir "lua-act.scm.new") (mdir "lua-act.scm")))
        (b (move-if-changed (mdir "lua-tab.scm.new") (mdir "lua-tab.scm"))))
    (or a b)))

;; --- last line ---
