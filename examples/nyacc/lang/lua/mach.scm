;; nyacc/lang/lua/mach.scm

;; Copyright (C) 2017,2018,2025 Matthew Wette
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

;; currenty seven SR conflicts :(
(define lua-spec
  (lalr-spec
   (start block)
   (grammar

    (block
     (stmt-list-1 ($$ `(block ,@(cdr (tl->list $1)))))
     (stmt-list-1 finish ($$ `(block ,@(cdr (tl->list $1)) ,$2))))
    (stmt-list-1
     (stmt opt-semi ($$ (make-tl 'stmt-list $1)))
     (stmt-list-1 stmt opt-semi ($$ (tl-append $1 $2))))

    (finish
     (finish-1 opt-semi ($$ $1)))
    (finish-1
     ("return" ($$ `(return)))
     ("return" exprs ($$ `(return ,$2)))
     ("break" ($$ `(break)))
     ("break" name ($$ `(break ,$1))))

    (stmt
     (var-list "=" exprs ($$ `(assn ,$1 ,$3)))
     (call)
     ("do" block "end" ($$ `(do ,$2)))
     ("while" expr "do" block "end" ($$ `(while ,$2 ,$4)))
     ("repeat" block "until" expr ($$ `(repeat-until ,$4 ,$2)))
     ("if" expr "then" block "end" ($$ `(if ,$2 ,$4)))
     ("if" expr "then" block "else" block "end" ($$ `(if ,$2 ,$4 ,$6)))
     ("if" expr "then" block elseif-list "end" ($$ `(if ,$2 ,$4 ,$5)))
     ("if" expr "then" block elseif-list "else" block "end"
      ($$ `(if ,$2 ,$4 ,$5 ,$7)))
     ("for" name "=" expr "," expr "do" block "end"
      ($$ `(for ,$2 (range ,$4 ,$6) ,$8)))
     ("for" name "=" expr "," expr "," expr "do" block "end"
      ($$ `(for ,$2 (range ,$4 ,$6 ,$8) ,$10)))
     ("for" name "," name "in" expr "do" block "end"
      ($$ `(for-in ,$ ,$)))
     ("function" func-name "(" ")" block "end"
      ($$ `(function ,$2 (params) ,$5)))
     ("function" func-name "(" params ")" block "end"
      ($$ `(function ,$2 ,$3 ,$4)))
     ("local" name-list ($$ `(local ,$2))) 
     ("local" name-list "=" exprs ($$ `(local ,$2 ,$4))))

    (elseif-list
     (elseif-list-1 ($$ (tl->list $1))))
    (elseif-list-1
     (elseif ($$ (make-tl 'elseif-list $1)))
     (elseif-list-1 elseif ($$ (tl-append $1 $2))))
    (elseif
     ("elseif" expr "then" block ($$ `(elseif ,$2 ,$4))))

    (name-list
     (name)
     (name-list "," name))

    (func-name
     (name)
     (name ":" key)
     (name keys)
     (name keys ":" key))

    (keys
     ("." key)
     (keys "." key))

    (var-list
     (exprs))

    (params
     ("..." ($$ `(params (ellipsis))))
     (name-list ($$ `(params ,@(cdr $1))))
     (name-list "..." ($$ `(params ,@(cdr $1) (ellipsis)))))

    (exprs
     (expr)
     (exprs "," expr))

    (expr
     (or-expr))
    (or-expr
     (and-expr)
     (or-expr "or" and-expr ($$ `(or ,$1 ,$3))))
    (and-expr
     (equ-expr)
     (and-expr "and" equ-expr ($$ `(and ,$1 ,$3))))
    (equ-expr
     (rel-expr)
     (equ-expr "==" rel-expr ($$ `(eq ,$1 ,$3)))
     (equ-expr "~=" rel-expr ($$ `(ne ,$1 ,$3))))
    (rel-expr
     (strcat-expr)
     (rel-expr "<" strcat-expr ($$ `(lt ,$1 ,$3)))
     (rel-expr "<=" strcat-expr ($$ `(le ,$1 ,$3)))
     (rel-expr ">" strcat-expr ($$ `(gt ,$1 ,$3)))
     (rel-expr ">=" strcat-expr ($$ `(ge ,$1 ,$3))))
    (strcat-expr
     (add-expr)
     (strcat-expr ".." add-expr ($$ `(strcat ,$1 ,$3))))
    (add-expr
     (mul-expr)
     (add-expr "+" mul-expr ($$ `(add ,$1 ,$3)))
     (add-expr "-" mul-expr ($$ `(sub ,$1 ,$3))))
    (mul-expr
     (unary-expr)
     (mul-expr "*" unary-expr ($$ `(mul ,$1 ,$3)))
     (mul-expr "/" unary-expr ($$ `(div ,$1 ,$3)))
     (mul-expr "%" unary-expr ($$ `(mod ,$1 ,$3))))
    (unary-expr
     (exp-expr)
     ;;("+" unary-expr)                    ; not in lua
     ("-" unary-expr ($$ `(neg ,$1)))
     ("#" unary-expr ($$ `(len ,$1)))      ; length
     ("not" unary-expr ($$ `(not ,$1))))
    (exp-expr
     (prim-expr)
     (exp-expr "^" prim-expr ($$ `(exp ,$1 ,$3))))
    (prim-expr
     ("nil" ($$ '(nil)))
     (literal)
     (table-cons)
     ("function" "(" ")" block "end" ($$ `(fctn-expr (params) ,$4)))
     ("function" "(" params ")" block "end" ($$ `(fctn-expr ,$3 ,$5)))
     ("(" expr ")" ($$ $2)))

    (primary (prim-expr))
    
    (var
     (name)
     (primary index ($$ `(indexed ,$1 ,$2))) ;; ???
     (var index ($$ `(indexed ,$1 ,$2))) ;; ???
     (call index ($$ `(call ,$1))))
    
    (index
     ("[" expr "]" ($$ `(expr-index ,$2)))
     ("." key ($$ `(key-index ,$2))))

    (call
     (primary ":" key args)
     (primary args)
     (var ":" key args)
     (var args)
     (call ":" key args)
     (call args))

    (args
     ("(" exprs ")")
     ("(" ")")
     (table-cons)
     (literal)
     )

    (table-cons
     ("{" fields "}")
     ("{" "}"))

    (fields
     (expr-fields ";" mapping-fields)
     (expr-fields ";")
     (expr-fields)
     (mapping-fields ";" expr-fields)
     (mapping-fields ";")
     (mapping-fields)
     (";" expr-fields)
     (";" mapping-fields))

    (expr-fields
     (exprs ",")
     (exprs))

    (mapping-fields
     (mapping-field)
     (mapping-fields "," mapping-field)
     (mapping-fields ","))

    (mapping-field
     ("[" expr "]" "=" expr)
     (key "=" expr))

    (literal ;; => 3 SR conflicts
     (number)
     (string))

    (key
     (name)
     (string)
     (number))
    
    (opt-semi ($empty) (";"))

    (name ($ident ($$ `(name ,$1))))
    (number ($fixed ($$ `(fixed ,$1)))
            ($float ($$ `(float ,$1))))
    (string ($string ($$ `(string ,$1)))))))

;; === parsers ==========================

(define lua-mach
  (hashify-machine
   (compact-machine
    (make-lalr-machine lua-spec))))

#|
(define luaia-spec (restart-spec lua-spec 'stmt))

(define luaia-mach
  (hashify-machine
   (compact-machine
    (make-lalr-machine luaia-spec))))
|#

;; === automaton file generators =========

(define* (gen-lua-files #:optional (path "."))
  (define (mdir file) (mach-dir path file))
  (write-lalr-actions lua-mach (mdir "lua-act.scm.new") #:prefix "lua-")
  (write-lalr-tables lua-mach (mdir "lua-tab.scm.new") #:prefix "lua-")
  (let ((a (move-if-changed (mdir "lua-act.scm.new") (mdir "lua-act.scm")))
        (b (move-if-changed (mdir "lua-tab.scm.new") (mdir "lua-tab.scm"))))
    (or a b)))

;; --- last line ---
