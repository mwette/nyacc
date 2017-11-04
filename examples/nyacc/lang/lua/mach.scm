

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

    )

    
