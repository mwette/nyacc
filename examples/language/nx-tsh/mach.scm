;;; lang/tsh/mach.scm - code and expr grammars

;; Copyright (C) 2021-2023 Matthew Wette
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

;; 1) set-indexed must be predefined identifier
;;    set d [make-array (10, 10)]  # 10 x 10 array
;;    set d [make-vector f64 (10, 10)]  # 10 x 10 matrix

;; 2) symbol is just a lone identifier, not single-quoted (e.g, 'foo')

;; 3) need special consts: void = (), true, false,
;;     $true = 1, $false = 0, () = (void) = void

;; false can be 0 or false (the symbol)

;;; Code:

(define-module (nyacc lang tsh mach)
  #:export (gen-tsh-files
	    tsh-file-spec tsh-file-mach 
	    tsh-stmt-spec tsh-stmt-mach)
  #:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc lang util)
  #:use-module (rnrs arithmetic bitwise))

;; need #<unspecified>

(define tsh-file-spec
  (lalr-spec
   (notice (string-append "Copyright (C) 2021-2023 Matthew R. Wette"
			  license-lgpl3+))
   ;;(expect 0)

   (reserve '$code-comm)
   (start top)
   (grammar

    (top
     (script))

    (script (script-1 ($$ (tl->list $1))))
    (script-1
     (item ($$ (make-tl 'script $1)))
     (script-1 item ($$ (tl-append $1 $2))))

    ;; item: top-level declaration or decl-stmt or exec-stmt
    (item
     (topl-decl term)
     (stmt term))

    (topl-decl
     ("source" string ($$ `(source ,$2)))
     ("use" path ($$ `(use ,@(cdr $2)))))

    (stmt
     (decl-stmt)
     (exec-stmt)
     (fill-stmt))

    ;; stmt list uses term as statement separators
    #|
    (stmt-list
     (stmt-list-1 ($$ (tl->list $1))))
    (stmt-list-1
     (stmt ($$ (make-tl 'stmt-list $1)))
     (stmt-list-1 term stmt ($$ (tl-append $1 $3))))
    |#

    (proc-stmt-list
     (fill-stmt-list/term decl-stmt-list/term exec-stmt-list
                          ($$ `(stmt-list ,@(cdr $1) ,@(cdr $2) ,@(cdr $3))))
     (decl-stmt-list/term exec-stmt-list
                          ($$ `(stmt-list ,@(cdr $1) ,@(cdr $2))))
     (fill-stmt-list/term exec-stmt-list
                          ($$ `(stmt-list ,@(cdr $1) ,@(cdr $2))))
     (exec-stmt-list ($$ `(stmt-list ,@(cdr $1))))
     ($empty ($$ `(stmt-list (empty-stmt)))))

    (block-stmt-list
     (fill-stmt-list/term exec-stmt-list ($$ `(stmt-list ,@(cdr $1) ,@(cdr $2))))
     (exec-stmt-list ($$ `(stmt-list ,@(cdr $1)))))
    
    (fill-stmt
     ($empty ($$ `(empty-stmt)))
     ($lone-comm ($$ `(comment ,$1))))
     
    (fill-stmt-list/term
     (fill-stmt-list/term-1 ($$ (tl->list $1))))
    (fill-stmt-list/term-1
     (fill-stmt term ($$ (make-tl `stmt-list $1)))
     (fill-stmt-list/term-1 lone-comm term ($$ (tl-append $1 $2)))
     (fill-stmt-list/term-1 term))
     
    (decl-stmt-list/term
     (decl-stmt-list/term-1 ($$ (tl->list $1))))
    (decl-stmt-list/term-1
     (decl-stmt term ($$ (make-tl 'stmt-list $1)))
     (decl-stmt-list/term-1 decl-stmt term ($$ (tl-append $1 $2)))
     (decl-stmt-list/term-1 lone-comm term ($$ (tl-append $1 $2)))
     (decl-stmt-list/term-1 term))
     
    (exec-stmt-list
     (exec-stmt-list-1 ($$ (tl->list $1))))
    (exec-stmt-list-1
     (exec-stmt ($$ (make-tl 'stmt-list $1)))
     (exec-stmt-list-1 term exec-stmt ($$ (tl-append $1 $3)))
     (exec-stmt-list-1 term lone-comm ($$ (tl-append $1 $3)))
     (exec-stmt-list-1 term))
     
    (decl-stmt
     ("proc" ident "{" arg-list "}" "{" proc-stmt-list "}"
      ($$ `(proc ,$2 ,$4 ,$7)))
     #;("proc" ident symbol "{" proc-stmt-list "}"
      ($$ `(proc ,$2 (arg-list (arg ,$3)) ,$5)))
     ("global" name-seq ($$ `(global ,@(cdr $2))))
     ("nonlocal" name-seq ($$ `(nonlocal ,@(cdr $2))))
     ("local" name-seq ($$ `(local ,@(cdr $2)))))

    (arg-list
     (arg-list-1 ($$ (tl->list $1))))
    (arg-list-1
     ($empty ($$ (make-tl 'arg-list)))
     (arg-list-1 ident ($$ (tl-append $1 `(arg ,$2))))
     (arg-list-1 "{" ident unit-expr "}" ($$ (tl-append $1 `(opt-arg ,$3 ,$4))))
     (arg-list-1 "args" ($$ (tl-append $1 `(rest-arg (ident "args"))))))

    (name-seq ;; "foo" "bar" ...
     (name-seq-1 ($$ (tl->list $1))))
    (name-seq-1
     ($ident ($$ (make-tl 'name-seq $1)))
     (name-seq-1 $ident ($$ (tl-append $1 $2))))

    (exec-stmt
     ;;("upvar" ident-list ($$ `(nonlocal ,@(cdr $2))))
     ("set" ident unit-expr ($$ `(set ,$2 ,$3)))
     ("set" $deref/ix "(" expr-list ")" unit-expr
      ($$ `(set-indexed (deref (ident ,$2)) ,$4 ,$6)))
     (ident expr-seq ($$ `(call ,$1 ,@(cdr $2))))
     ("lambda" "{" arg-list "}" "{" proc-stmt-list "}" ($$ `(lambda ,$3 ,$6)))
     ("(" expr-list ")" ($$ $2))
     ;;("{" stmt-list "}" ($$ $2))
     (if-stmt)
     ("switch" unit-expr "{" case-list "}" ($$ `(switch ,$2 ,@(cdr $4))))
     ("while" unit-expr "{" block-stmt-list "}" ($$ `(while ,$2 ,$4)))
     ("for" "{" block-stmt-list "}" "{" unit-expr "}" "{" block-stmt-list "}" 
      "{" block-stmt-list "}" ($$ `(for ,$3 ,$6 ,$9 ,$12)))
     ;;("lambda "{" arg-list "}" "{" stmt-list "}" )
     ("format" expr-seq ($$ `(format . ,(cdr $2))))
     ("return" ($$ `(return)))
     ("return" unit-expr ($$ `(return ,$2)))
     ("incr" ident ($$ `(incr ,$2)))
     ("incr" ident unit-expr ($$ `(incr ,$2 ,$3)))
     ;;("incr" ident 'no-ws "(" expr-list ")" ($$ `(incr/ix ,$2 ,$5)))
     #;("incr" ident 'no-ws "(" expr-list ")" unit-expr
     ($$ `(incr/ix ,$2 ,$5 ,$7)))
     )

    (if-stmt
     ("if" unit-expr "{" block-stmt-list "}"
      ($$ `(if ,$2 ,$4)))
     ("if" unit-expr "{" block-stmt-list "}" "else" "{" block-stmt-list "}"
      ($$ `(if ,$2 ,$4 (else ,$8))))
     ("if" unit-expr "{" block-stmt-list "}" elseif-list
      ($$ `(if ,$2 ,$4 ,@(sx-tail $6))))
     ("if" unit-expr "{" block-stmt-list "}" elseif-list "else"
      "{" block-stmt-list "}" ($$ `(if ,$2 ,$4 ,@(sx-tail $6) (else ,$9)))))
    (elseif-list
     (elseif-list-1 ($$ (tl->list $1))))
    (elseif-list-1
     ("elseif" unit-expr "{" block-stmt-list "}" 
      ($$ (make-tl 'elseif-list `(elseif ,$2 ,$4))))
     (elseif-list-1
      "elseif" unit-expr "{" block-stmt-list "}"
      ($$ (tl-append $1 'elseif-list `(elseif ,$2 ,$4)))))

    (case-list
     (case-list-1 ($$ (tl->list $1)))
     (case-list-1 default-case-expr ($$ (append (tl->list $1) (list $2)))))
    (case-list-1
     (case-expr ($$ (make-tl 'case-list $1)))
     (term ($$ (make-tl 'case-list)))
     (case-list-1 case-expr ($$ (tl-append $1 $2)))
     (case-list-1 term ($$ $1)))
    (case-expr
     (unit-expr unit-expr ($$ `(case ,$1 ,$2)))
     (unit-expr "{" block-stmt-list "}" ($$ `(case ,$1 ,$3))))
    (default-case-expr
     ("default" unit-expr ($$ `(case (default) ,$2))))
    
    (unit-expr
     (primary-expression ($$ `(expr ,$1))))

    (expression
     (logical-or-expression))
    
    (logical-or-expression
     (logical-and-expression)
     (logical-or-expression "||" logical-and-expression ($$ `(or ,$1 ,$3))))
    (logical-and-expression
     (bitwise-or-expression)
     (logical-and-expression "&&" bitwise-or-expression ($$ `(and ,$1 ,$3))))
    (bitwise-or-expression
     (bitwise-xor-expression)
     (bitwise-or-expression "|" bitwise-xor-expression
			    ($$ `(bitwise-or ,$1 ,$3))))
    (bitwise-xor-expression
     (bitwise-and-expression)
     (bitwise-xor-expression "^" bitwise-and-expression
			     ($$ `(bitwise-xor ,$1 ,$3))))
    (bitwise-and-expression
     (equality-expression)
     (bitwise-and-expression "&" equality-expression
			     ($$ `(bitwise-and ,$1 ,$3))))
    (equality-expression
     (relational-expression)
     (equality-expression "==" relational-expression ($$ `(eq ,$1 ,$3)))
     (equality-expression "!=" relational-expression ($$ `(ne ,$1 ,$3))))
    (relational-expression
     (shift-expression)
     (relational-expression "<" shift-expression ($$ `(lt ,$1 ,$3)))
     (relational-expression "<=" shift-expression ($$ `(le ,$1 ,$3)))
     (relational-expression ">" shift-expression ($$ `(gt ,$1 ,$3)))
     (relational-expression ">=" shift-expression ($$ `(ge ,$1 ,$3))))
    (shift-expression
     (additive-expression)
     (shift-expression "<<" additive-expression ($$ `(lshift ,$1 ,$3)))
     (shift-expression ">>" additive-expression ($$ `(rshift ,$1 ,$3))))
    (additive-expression
     (multiplicative-expression)
     (additive-expression "+" multiplicative-expression ($$ `(add ,$1 ,$3)))
     (additive-expression "-" multiplicative-expression ($$ `(sub ,$1 ,$3))))
    (multiplicative-expression
     (unary-expression)
     (multiplicative-expression "*" unary-expression ($$ `(mul ,$1 ,$3)))
     (multiplicative-expression "/" unary-expression ($$ `(div ,$1 ,$3)))
     (multiplicative-expression "%" unary-expression ($$ `(mod ,$1 ,$3))))
    (unary-expression
     (primary-expression)
     ("-" unary-expression ($$ `(neg ,$2)))
     ("+" unary-expression ($$ `(pos ,$2)))
     ("!" unary-expression ($$ `(not ,$2)))
     ("~" unary-expression ($$ `(bitwise-not ,$2))))
    (primary-expression
     #|
     ("$" 'no-ws $ident ($$ `(deref ,$3)))
     ("$" 'no-ws $ident 'no-ws "(" expr-list ")" ($$ `(deref-indexed ,$3 ,$6)))
     |#
     ($deref ($$ `(deref ,$1)))
     ($deref/ix "(" expr-list ")" ($$ `(deref-indexed ,$1 ,$3)))
     (fixed)
     (float)
     (string)
     (symbol)
     (keychar)
     (keyword)
     ;;($chlit ($$ `(char ,$1)))
     ("(" expr-list ")" ($$ `(last ,$2)))
     ("[" exec-stmt "]" ($$ `(eval ,$2)))
     )

    (expr-list
     (expr-list-1 ($$ (tl->list $1))))
    (expr-list-1
     (expression ($$ (make-tl 'expr-list $1)))
     (expr-list-1 "," expression ($$ (tl-append $1 $3))))

    (expr-seq
     (expr-seq-1 ($$ (tl->list $1))))
    (expr-seq-1
     ($empty ($$ (make-tl 'seq-list)))
     (expr-seq-1 primary-expression ($$ (tl-append $1 $2))))

    (path
     (path-1 ($$ (tl->list $1))))
    (path-1
     ($ident ($$ (make-tl 'path $1)))
     ($string ($$ (make-tl 'path $1)))
     (path-1 'no-ws "::" 'no-ws $ident ($$ (tl-append $1 $5)))
     (path-1 'no-ws "::" 'no-ws $string ($$ (tl-append $1 $5))))

    (ident ($ident ($$ `(ident ,$1))))
    (fixed ($fixed ($$ `(fixed ,$1))))
    (float ($float ($$ `(float ,$1))))
    (string ($string ($$ `(string ,$1))))
    (symbol (ident))
    (keychar ($keychar ($$ `(keychar ,$1))))
    (keyword ($keyword ($$ `(keyword ,$1))))
    (lone-comm ($lone-comm ($$ `(comment ,$1))))
    ;;(code-comm ($code-comm ($$ `(comment ,$1))))
    (term (";") ("\n"))
    )))

(define tsh-file-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine tsh-file-spec))
   #:keep 0 #:keepers '($code-comm $lone-comm "\n" 'no-ws)))

(define tsh-stmt-spec
  (restart-spec tsh-file-spec 'item))

(define tsh-stmt-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine tsh-stmt-spec))
   #:keep 0 #:keepers '(no-ws)))

;;; =====================================

;; @defun {Scheme} gen-tsh-code-files [dir] => #t
;; Update or generate the files @quot{tsh-act.scm} and @quot{tsha-tab.scm}.
;; If there are no changes to existing files, no update occurs.
;; @end deffn
(define (gen-tsh-files . rest)
  (define (lang-dir path)
    (if (pair? rest) (string-append (car rest) "/" path) path))
  (define (xtra-dir path)
    (lang-dir (string-append "mach.d/" path)))
  (write-lalr-actions
   tsh-file-mach (xtra-dir "tsh-file-act.scm.new") #:prefix "tsh-file-")
  (write-lalr-tables
   tsh-file-mach (xtra-dir "tsh-file-tab.scm.new") #:prefix "tsh-file-")
  (write-lalr-actions
   tsh-stmt-mach (xtra-dir "tsh-stmt-act.scm.new") #:prefix "tsh-stmt-")
  (write-lalr-tables
   tsh-stmt-mach (xtra-dir "tsh-stmt-tab.scm.new") #:prefix "tsh-stmt-")
  (let ((a (move-if-changed (xtra-dir "tsh-file-act.scm.new")
			    (xtra-dir "tsh-file-act.scm")))
	(b (move-if-changed (xtra-dir "tsh-file-tab.scm.new")
			    (xtra-dir "tsh-file-tab.scm")))
	(c (move-if-changed (xtra-dir "tsh-stmt-act.scm.new")
			    (xtra-dir "tsh-stmt-act.scm")))
	(d (move-if-changed (xtra-dir "tsh-stmt-tab.scm.new")
			    (xtra-dir "tsh-stmt-tab.scm"))))
    (or a b c d)))

;; --- last line ---
