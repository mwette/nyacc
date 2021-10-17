;; lang/mlang/mach.scm

;; Copyright (C) 2015-2018 Matthew R. Wette
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

;;; Description:

;; mlang parser
;; 1) does NOT support non-comma rows [ 1 2 ] => syntax error

;; TODO:
;; 1) function handles: {foo = @bar;} where bar is a function
;; 2) anonymous functions: {foo = @(arg1,arg2) arg1 + arg2;}
;; 3) structs
;; 4) cell arrays (hoping not needed)

;;; Code:

(define-module (nyacc lang mlang mach)
  #:export (mlang-spec
	    mlang-mach
	    mlang-ia-spec
	    mlang-ia-mach
	    dev-parse-ml
	    gen-mlang-files)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (ice-9 pretty-print)
  )

(define mlang-spec
  (lalr-spec
   (notice (string-append "Copyright 2015-2018 Matthew R. Wette"
			  license-lgpl3+))
   (start translation-unit)
   (grammar
    
    (translation-unit
     (triv-stmt-list nontrivial-statement mlang-item-list
		     ($$ `(script ,@(sx-tail $1) ,$2 ,@(sx-tail $3))))
     (triv-stmt-list function-defn mlang-item-list
		     ($$ `(function-file ,@(sx-tail $1) ,$2 ,@(sx-tail $3))))
     (nontrivial-statement mlang-item-list
			    ($$ `(script ,$1 ,@(sx-tail $2))))
     (function-defn mlang-item-list
			    ($$ `(function-file ,$1 ,@(sx-tail $2)))))

    (mlang-item-list
     (mlang-item-list-1 ($$ (tl->list $1))))
    (mlang-item-list-1
     ($empty ($$ (make-tl 'mitem-list)))
     (mlang-item-list-1 mlang-item ($$ (tl-append $1 $2))))
    
    (mlang-item
     (function-defn)
     (statement))

    (function-defn
     (function-decl non-comment-statement stmt-list the-end
      ($$ `(fctn-defn ,$1 ,(tl->list (if $2 (tl-insert $3 $2) $3)))))
     (function-decl non-comment-statement the-end
      ($$ `(fctn-defn ,$1 ,(if $2 `(stmt-list ,$2) '(stmt-list)))))
     (function-decl the-end
      ($$ `(fctn-defn ,$1 (stmt-list)))))
    
    (the-end ("end" term)) 

    (function-decl
     (function-decl-line lone-comment-list ($$ (append $1 (list $2))))
     (function-decl-line ($$ $1)))

    (function-decl-line
     ;; fctn-decl name input-args output-args
     ("function" "[" ident-list "]" "=" ident "(" ident-list ")" term
      ($$ `(fctn-decl ,$6 ,(tl->list $8) ,(tl->list $3))))
     ("function" "[" ident-list "]" "=" ident "(" ")" term
      ($$ `(fctn-decl ,$6 (ident-list) ,(tl->list $3))))
     ("function" ident "=" ident "(" ident-list ")" term
      ($$ `(fctn-decl ,$4 ,(tl->list $6) (ident-list ,$2))))
     ("function" ident "=" ident "(" ")" term
      ($$ `(fctn-decl ,$4 (ident-list) (ident-list ,$2))))
     ("function" ident "(" ident-list ")" term
      ($$ `(fctn-decl ,$2 ,(tl->list $4) (ident-list))))
     ("function" ident "(" ")" term
      ($$ `(fctn-decl ,$2 (ident-list) (ident-list))))
      )

    (ident-list
     (ident ($$ (make-tl 'ident-list $1)))
     (ident-list "," ident ($$ (tl-append $1 $3))))

    (stmt-list
     (statement ($$ (if $1 (make-tl 'stmt-list $1) (make-tl 'stmt-list))))
     (stmt-list statement ($$ (if $2 (tl-append $1 $2) $1))))

    (triv-stmt-list
     (triv-stmt-list-1 ($$ (tl->list $1))))
    (triv-stmt-list-1
     (trivial-statement ($$ (make-tl 'triv-stmt-list $1)))
     (triv-stmt-list-1 trivial-statement ($$ (tl-append $1 $2))))

    (statement
     (trivial-statement)
     (nontrivial-statement))

    (non-comment-statement
     (term ($$ '(empty-stmt)))
     (nontrivial-statement))

    (trivial-statement
     (lone-comment "\n")
     (term ($$ '(empty-stmt))))

    (nontrivial-statement
     (nontrivial-statement-1 term ($$ (sx-attr-add $1 'term $2))))
    (nontrivial-statement-1
     (expr ($$ `(expr-stmt ,$1)))
     (expr "=" expr ($$ `(assn ,$1 ,$3)))
     ("for" ident "=" expr term stmt-list "end"
      ($$ `(for ,$2 ,$4 ,(tl->list $6))))
     ("while" expr term stmt-list "end"
      ($$ `(while ,$2 ,(tl->list $4))))
     ("if" expr term stmt-list elseif-list "else" stmt-list "end"
      ($$ `(if ,$2 ,(tl->list $4)
	       ,@(cdr (tl->list $5))
	       (else ,(tl->list $7)))))
     ("if" expr term stmt-list elseif-list "end"
      ($$ `(if ,$2 ,(tl->list $4) ,@(cdr (tl->list $5)))))
     ("if" expr term stmt-list "else" stmt-list "end"
      ($$ `(if ,$2 ,(tl->list $4) (else ,(tl->list $6)))))
     ("if" expr term stmt-list "end"
      ($$ `(if ,$2 ,(tl->list $4))))
     ("switch" expr term case-list "otherwise" term stmt-list "end"
      ($$ `(switch ,$2 ,@(cdr (tl->list $4)) (otherwise ,(tl->list $7)))))
     ("switch" expr term case-list "end"
      ($$ `(switch ,$2 ,@(cdr (tl->list $4)))))
     ("return"
      ($$ '(return)))
     (command arg-list ($$ `(command ,$1 ,@(cdr (tl->list $2))))))

    (command
     ("clear" ($$ '(command "clear")))
     ("global" ($$ '(command "global")))
     ("load" ($$ '(command "load"))))

    ;; Only ident list type commands are allowed
    (arg-list
     (ident ($$ (make-tl 'arg-list (cons 'arg (cdr $1)))))
     (arg-list ident ($$ (tl-append $1 (cons 'arg $2)))))

    (elseif-list
     ("elseif" expr term stmt-list
      ($$ (make-tl 'elseif-list `(elseif ,$2 ,(tl->list $4)))))
     (elseif-list "elseif" expr term stmt-list
		   ($$ (tl-append $1 `(elseif ,$3 ,(tl->list $5))))))

    ;; The switch case for this mlang only allows case-expr of form
    ;; @code{fixed}, @code{string}, @code{fixed-list} or @code{string-list}.
    (case-list
     ($empty ($$ (make-tl 'case-list)))
     (case-list "case" case-expr term stmt-list
		($$ (tl-append $1 `(case ,$3 ,(tl->list $5))))))
    (case-expr
     (fixed) (string)
     ("{" fixed-list "}" ($$ (tl->list $2)))
     ("{" string-list "}" ($$ (tl->list $2))))
    (fixed-list
     (fixed ($$ (make-tl 'fixed-list $1)))
     (fixed-list fixed ($$ (tl-append $1 $2))))
    (string-list
     (string ($$ (make-tl 'string-list $1)))
     (string-list string ($$ (tl-append $1 $2))))
     
    (expr-list
     (expr ($$ (make-tl 'expr-list $1)))
     (expr-list "," expr ($$ (tl-append $1 $3))))

    (expr
     (or-expr)
     (":" ($$ `(colon-expr)))
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
     (mul-expr ".^" unary-expr ($$ `(dot-pow ,$1 ,$3))))

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
     ("{" matrix-row-list "}" ($$ (cons 'cell-array (cdr (tl->list $2))))))

    (matrix-row-list
     (matrix-row ($$ (make-tl 'matrix (tl->list $1))))
     (matrix-row-list row-term matrix-row ($$ (tl-append $1 (tl->list $3)))))
    (row-term (";") (nl))

    (matrix-row
     (expr ($$ (make-tl 'row $1)))
     (matrix-row "," expr ($$ (tl-append $1 $3))))

    (term-list (term) (term-list term))

    (term (nl) (";") (","))

    (lone-comment-list
     (lone-comment-list-1 ($$ (tl->list $1))))
    (lone-comment-list-1
     (lone-comment nl ($$ (make-tl 'comm-list $1)))
     (lone-comment-list-1 lone-comment nl ($$ (tl-append $1 $2))))

    (ident ($ident ($$ `(ident ,$1))))
    (fixed ($fixed ($$ `(fixed ,$1))))
    (float ($float ($$ `(float ,$1))))
    (number (fixed) (float))
    (string ($string ($$ `(string ,$1))))
    (lone-comment ($lone-comm ($$ `(comm ,$1))))
    ;;(code-comment ($code-comm ($$ `(comm ,$1))))
    (nl ("\n"))
    )))

;; === parsers ==========================

(define mlang-mach
  (hashify-machine
   (compact-machine
    (make-lalr-machine mlang-spec))))

(define mlang-ia-spec (restart-spec mlang-spec 'mlang-item))

(define mlang-ia-mach
  (let* ((mach (make-lalr-machine mlang-ia-spec))
	 (mach (compact-machine mach #:keep 0 #:keepers '()))
	 (mach (hashify-machine mach))
	 )
    mach))

;; === automaton file generators =========

(define (gen-mlang-files . rest)
  (define (lang-dir path)
    (if (pair? rest) (string-append (car rest) "/" path) path))
  (define (xtra-dir path)
    (lang-dir (string-append "mach.d/" path)))

  (write-lalr-actions mlang-mach (xtra-dir "mlang-act.scm.new")
		      #:prefix "mlang-")
  (write-lalr-tables mlang-mach (xtra-dir "mlang-tab.scm.new")
		     #:prefix "mlang-")
  (write-lalr-actions mlang-ia-mach (xtra-dir "mlangia-act.scm.new")
		      #:prefix "mlangia-")
  (write-lalr-tables mlang-ia-mach (xtra-dir "mlangia-tab.scm.new")
		     #:prefix "mlangia-")
  (let ((a (move-if-changed (xtra-dir "mlang-act.scm.new")
			    (xtra-dir "mlang-act.scm")))
	(b (move-if-changed (xtra-dir "mlang-tab.scm.new")
			    (xtra-dir "mlang-tab.scm")))
	(c (move-if-changed (xtra-dir "mlangia-act.scm.new")
			    (xtra-dir "mlangia-act.scm")))
	(d (move-if-changed (xtra-dir "mlangia-tab.scm.new")
			    (xtra-dir "mlangia-tab.scm"))))
    (or a b c d)))

;; --- last line ---
