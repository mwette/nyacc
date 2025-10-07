;; lua-act.scm

(define lua-act-v
  (vector
   ;; $start => block
   (lambda ($1 . $rest) $1)
   ;; block => "{" stmt opt-semi "}" opt-finish
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; opt-semi => 
   (lambda $rest (list))
   ;; opt-semi => ";"
   (lambda ($1 . $rest) $1)
   ;; opt-finish => finish opt-semi
   (lambda ($2 $1 . $rest) $1)
   ;; stmt => var-list "=" exprs
   (lambda ($3 $2 $1 . $rest) $1)
   ;; stmt => call
   (lambda ($1 . $rest) $1)
   ;; stmt => "do" block "end"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; stmt => "while" expr "do" block "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; stmt => "repeat" block "until" expr
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; stmt => "if" expr "then" block "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; stmt => "if" expr "then" block "else" block "end"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; stmt => "if" expr "then" block "elseif" expr "then" block "end"
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; stmt => "if" expr "then" block "elseif" expr "then" block "else" bloc...
   (lambda ($11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     $1)
   ;; stmt => "for" name "=" expr "," expr "do" block "end"
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; stmt => "for" name "=" expr "," expr "," expr "do" block "end"
   (lambda ($11 $10 $9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     $1)
   ;; stmt => "for" name "," name "in" expr "do" block "end"
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; stmt => "function" func-name "(" ")" block "end"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; stmt => "function" func-name "(" params ")" block "end"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; stmt => "local" name-list
   (lambda ($2 $1 . $rest) $1)
   ;; stmt => "local" name-list "=" exprs
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; name-list => name
   (lambda ($1 . $rest) $1)
   ;; name-list => name-list "," name
   (lambda ($3 $2 $1 . $rest) $1)
   ;; finish => "return"
   (lambda ($1 . $rest) $1)
   ;; finish => "return" exprs
   (lambda ($2 $1 . $rest) $1)
   ;; finish => "break"
   (lambda ($1 . $rest) $1)
   ;; finish => "break" name
   (lambda ($2 $1 . $rest) $1)
   ;; func-name => name
   (lambda ($1 . $rest) $1)
   ;; func-name => name ":" key
   (lambda ($3 $2 $1 . $rest) $1)
   ;; func-name => name keys
   (lambda ($2 $1 . $rest) $1)
   ;; func-name => name keys ":" key
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; keys => "." key
   (lambda ($2 $1 . $rest) $1)
   ;; keys => keys "." key
   (lambda ($3 $2 $1 . $rest) $1)
   ;; var-list => exprs
   (lambda ($1 . $rest) $1)
   ;; params => "..."
   (lambda ($1 . $rest) $1)
   ;; params => name-list
   (lambda ($1 . $rest) $1)
   ;; params => name-list "..."
   (lambda ($2 $1 . $rest) $1)
   ;; exprs => expr
   (lambda ($1 . $rest) $1)
   ;; exprs => exprs "," expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; expr => or-expr
   (lambda ($1 . $rest) $1)
   ;; or-expr => and-expr
   (lambda ($1 . $rest) $1)
   ;; or-expr => or-expr "or" and-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; and-expr => equ-expr
   (lambda ($1 . $rest) $1)
   ;; and-expr => and-expr "and" equ-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; equ-expr => rel-expr
   (lambda ($1 . $rest) $1)
   ;; equ-expr => equ-expr "==" rel-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; equ-expr => equ-expr "~=" rel-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; rel-expr => add-expr
   (lambda ($1 . $rest) $1)
   ;; rel-expr => rel-expr "<" add-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; rel-expr => rel-expr "<=" add-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; rel-expr => rel-expr ">" add-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; rel-expr => rel-expr ">=" add-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; add-expr => mul-expr
   (lambda ($1 . $rest) $1)
   ;; add-expr => mul-expr "+" add-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; add-expr => mul-expr "-" add-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; mul-expr => una-expr
   (lambda ($1 . $rest) $1)
   ;; mul-expr => mul-expr "*" una-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; mul-expr => mul-expr "/" una-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; una-expr => prim-expr
   (lambda ($1 . $rest) $1)
   ;; una-expr => "+" una-expr
   (lambda ($2 $1 . $rest) $1)
   ;; una-expr => "-" una-expr
   (lambda ($2 $1 . $rest) $1)
   ;; prim-expr => "nil"
   (lambda ($1 . $rest) $1)
   ;; prim-expr => literal
   (lambda ($1 . $rest) $1)
   ;; prim-expr => "%" name
   (lambda ($2 $1 . $rest) $1)
   ;; prim-expr => table-cons
   (lambda ($1 . $rest) $1)
   ;; prim-expr => "function" "(" ")" block "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; prim-expr => "function" "(" params ")" block "end"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; prim-expr => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; primary => prim-expr
   (lambda ($1 . $rest) $1)
   ;; var => name
   (lambda ($1 . $rest) $1)
   ;; var => primary index
   (lambda ($2 $1 . $rest) $1)
   ;; var => var index
   (lambda ($2 $1 . $rest) $1)
   ;; var => call index
   (lambda ($2 $1 . $rest) $1)
   ;; index => "[" expr "]"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; index => "." key
   (lambda ($2 $1 . $rest) $1)
   ;; call => primary ":" key args
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; call => primary args
   (lambda ($2 $1 . $rest) $1)
   ;; call => var ":" key args
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; call => var args
   (lambda ($2 $1 . $rest) $1)
   ;; call => call ":" key args
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; call => call args
   (lambda ($2 $1 . $rest) $1)
   ;; args => "(" exprs ")"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; args => "(" ")"
   (lambda ($2 $1 . $rest) $1)
   ;; args => table-cons
   (lambda ($1 . $rest) $1)
   ;; args => literal
   (lambda ($1 . $rest) $1)
   ;; table-cons => "{" fields "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; table-cons => "{" "}"
   (lambda ($2 $1 . $rest) $1)
   ;; fields => expr-fields ";" mapping-fields
   (lambda ($3 $2 $1 . $rest) $1)
   ;; fields => expr-fields ";"
   (lambda ($2 $1 . $rest) $1)
   ;; fields => expr-fields
   (lambda ($1 . $rest) $1)
   ;; fields => mapping-fields ";" expr-fields
   (lambda ($3 $2 $1 . $rest) $1)
   ;; fields => mapping-fields ";"
   (lambda ($2 $1 . $rest) $1)
   ;; fields => mapping-fields
   (lambda ($1 . $rest) $1)
   ;; fields => ";" expr-fields
   (lambda ($2 $1 . $rest) $1)
   ;; fields => ";" mapping-fields
   (lambda ($2 $1 . $rest) $1)
   ;; expr-fields => exprs
   (lambda ($1 . $rest) $1)
   ;; mapping-fields => mapping-field
   (lambda ($1 . $rest) $1)
   ;; mapping-fields => mapping-fields "," mapping-field
   (lambda ($3 $2 $1 . $rest) $1)
   ;; mapping-fields => mapping-fields ","
   (lambda ($2 $1 . $rest) $1)
   ;; mapping-field => "[" expr "]" "=" expr
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; mapping-field => key "=" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; literal => number
   (lambda ($1 . $rest) $1)
   ;; literal => string
   (lambda ($1 . $rest) $1)
   ;; name => '$ident
   (lambda ($1 . $rest) $1)
   ;; number => '$fixed
   (lambda ($1 . $rest) $1)
   ;; number => '$float
   (lambda ($1 . $rest) $1)
   ;; string => '$string
   (lambda ($1 . $rest) $1)
   ;; key => name
   (lambda ($1 . $rest) $1)
   ))

;;; end tables
