;; lang/mlang/mach.scm

;; Copyright (C) 2015-2018,2025 Matthew Wette
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

;; This should be close to complete, but more will need to be added as
;; I could not find any complete syntax description.

;;; Code:

(define-module (language nx-mlang mach)
  #:export (mlang-spec
            mlang-mach
            mlang-ia-spec
            mlang-ia-mach
            gen-mlang-files)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lang util))

(define mlang-spec
  (lalr-spec
   (notice (string-append "Copyright 2015-2025 Matthew Wette"
                          license-lgpl3+))
   (expect 1)                           ; 'sp after expr
   (start translation-unit)
   (grammar
    
    (translation-unit
     #;(triv-stmt-list nontrivial-statement mlang-item-list
                     ($$ `(script ,@(sx-tail $1) ,$2 ,@(sx-tail $3))))
     (nontrivial-statement mlang-item-list
                           ($$ `(script ,$1 ,@(sx-tail $2))))
     (triv-stmt-list function-defn mlang-item-list
                     ($$ `(function-file ,@(sx-tail $1) ,$2 ,@(sx-tail $3))))
     (triv-stmt-list class-defn mlang-item-list
                     ($$ `(classdef-file ,@(sx-tail $1) ,$2 ,@(sx-tail $3))))
     (function-defn mlang-item-list
                    ($$ `(function-file ,$1 ,@(sx-tail $2))))
     (class-defn mlang-item-list
                 ($$ `(classdef-file ,$1 ,@(sx-tail $2))))
     )
 
    (mlang-item-list
     (mlang-item-list-1 ($$ (tl->list $1))))
    (mlang-item-list-1
     ($empty ($$ (make-tl 'mitem-list)))
     (mlang-item-list-1 mlang-item ($$ (tl-append $1 $2))))
    
    (mlang-item
     (function-defn)
     (statement))

    (class-defn
     ("classdef" "(" attr-list ")" ident "<" supers term class-parts "end"
      ($$ `(class-defn ,$5 ,$7 ,$3 ,@(cdr $9))))
     ("classdef" "(" attr-list ")" ident term class-parts "end"
      ($$ `(class-defn ,$5 ,$3 ,@(cdr $7))))
     ("classdef" ident "<" supers term class-parts "end"
      ($$ `(class-defn ,$2 ,$4 ,@(cdr $6))))
     ("classdef" ident term class-parts "end"
      ($$ `(class-defn ,$2 ,@(cdr $4)))))

    (supers
     (supers-1 ($$ (tl->list $1))))
    (supers-1
     (ident ($$ (make-tl 'supers $1)))
     (supers-1 "&" ident ($$ (tl-append $1 $3))))

    (class-parts
     (class-parts-1 ($$ (tl->list $1))))
    (class-parts-1
     ($empty ($$ (make-tl 'seq)))
     (class-parts-1 "properties" "(" attr-list ")" prop-list "end"
                 ($$ (tl-append $1 `(properties ,$4 ,@(cdr $6)))))
     (class-parts-1 "properties" prop-list "end"
                 ($$ (tl-append $1 `(properties ,@(cdr $3)))))
     (class-parts-1 "methods" "(" attr-list ")" method-list "end"
                 ($$ (tl-append $1 `(methods ,$4 ,@(cdr $6)))))
     (class-parts-1 "methods" method-list "end"
                 ($$ (tl-append $1 `(methods ,@(cdr $3)))))
     #|
     (class-parts-1 "events" "(" attr-list ")" evnt-list "end"
                 ($$ (tl-append $1 `(events ,$4 ,$6))))
     (class-parts-1 "events" evnt-list "end"
                 ($$ (tl-append $1 `(events ,$4))))
     (class-parts-1 "enumeration" "(" attr-list ")" enum-list "end"
                 ($$ (tl-append $1 `(enumeration ,$4 ,$6))))
     (class-parts-1 "enumeration" enum-list "end"
                 ($$ (tl-append $1 `(enumeration ,$6))))
     |#
     )
     
    (attr-list
     (attr-list-1 ($$ (tl->list $1))))
    (attr-list-1
     (attr ($$ (make-tl 'attr-list $1)))
     (attr-list-1 "," attr ($$ (tl-append $1 $3))))
    (attr
     (ident ($$ `(attr ,$1)))
     (ident "=" expr ($$ `(attr ,$1 ,$3))))

    (prop-list
     (prop-list-1 ($$ (tl->list $1))))
    (prop-list-1
     ($empty ($$ (make-tl 'properties)))
     (prop-list-1 prop ($$ (tl-append $1 $2))))
    (prop
     (ident term ($$ `(property ,$1)))
     )

    (method-list
     (method-list-1 ($$ (tl->list $1))))
    (method-list-1
     (method-item ($$ (make-tl 'methods $1)))
     (method-list-1 method-item ($$ (tl-append $1 $2))))
    (method-item
     (function-defn)
     (ident)
     (function-sig ($$ `(function-sig ,@(cdr $1)))))

    (function-defn
     (function-decl non-comment-statement stmt-list the-end
      ($$ `(fctn-defn ,$1 ,(if $2 `(stmt-list ,$2 ,@(cdr $3)) $3))))
     (function-decl non-comment-statement the-end
      ($$ `(fctn-defn ,$1 ,(if $2 `(stmt-list ,$2) '(stmt-list)))))
     (function-decl the-end
      ($$ `(fctn-defn ,$1 (stmt-list)))))
    
    (the-end ("end" term)) 

    (function-decl
     ("function" function-sig term lone-comment-list ($$ (append $2 (list $4))))
     ("function" function-sig term ($$ $2)))

    (function-sig
     ;; fctn-decl name input-args output-args
     ("[" ident-list "]" "=" ident "(" ident-list ")"
      ($$ `(fctn-decl ,$5 ,$7 ,$2)))
     ("[" ident-list "]" "=" ident "(" ")"
      ($$ `(fctn-decl ,$5 (ident-list) ,$2)))
     (ident "=" ident "(" ident-list ")"
      ($$ `(fctn-decl ,$3 ,$5 (ident-list ,$1))))
     (ident "=" ident "(" ")"
      ($$ `(fctn-decl ,$3 (ident-list) (ident-list ,$1))))
     (ident "(" ident-list ")"
      ($$ `(fctn-decl ,$1 ,$3 (ident-list))))
     (ident "(" ")"
      ($$ `(fctn-decl ,$1 (ident-list) (ident-list)))))

    (ident-list
     (ident-list-1 ($$ (tl->list $1))))
    (ident-list-1
     (ident ($$ (make-tl 'ident-list $1)))
     (ident-list-1 "," ident ($$ (tl-append $1 $3))))

    (stmt-list
     (stmt-list-1 ($$ (tl->list $1))))
    (stmt-list-1
     (statement ($$ (if $1 (make-tl 'stmt-list $1) (make-tl 'stmt-list))))
     (stmt-list-1 statement ($$ (if $2 (tl-append $1 $2) $1))))

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
      ($$ `(for ,$2 ,$4 ,$6)))
     ("while" expr term stmt-list "end"
      ($$ `(while ,$2 ,$4)))
     ("if" expr term stmt-list elseif-list "else" term stmt-list "end"
      ($$ `(if ,$2 ,$4 ,@(cdr $5) (else ,$8))))
     ("if" expr term stmt-list elseif-list "end"
      ($$ `(if ,$2 ,$4 ,@(cdr $5))))
     ("if" expr term stmt-list "else" term stmt-list "end"
      ($$ `(if ,$2 ,$4 (else ,$7))))
     ("if" expr term stmt-list "end"
      ($$ `(if ,$2 ,$4)))
     ("switch" expr term case-list "otherwise" term stmt-list "end"
      ($$ `(switch ,$2 ,@(cdr $4) (otherwise ,$7))))
     ("switch" expr term case-list "end"
      ($$ `(switch ,$2 ,@(cdr $4))))
     ("return"
      ($$ '(return)))
     (command arg-list ($$ (append $1 (cdr $2))))
     (command "(" arg-list ")" ($$ (append $1 (cdr $3)))))

    (command
     (command-name ($$ `(command ,$1))))
    (command-name
     ("clc") ("doc") ("drawnow") ("format") ("global") ("grid")
     ("help") ("hold") ("load") ("pause") ("rotate3d") ("save")
     ("uiimport") ("ver"))

    ;; Only ident list type commands are allowed
    (arg-list
     (arg-list-1 ($$ (tl->list $1))))
    (arg-list-1
     (ident ($$ (make-tl 'arg-list (cons 'arg (cdr $1)))))
     (arg-list-1 ident ($$ (tl-append $1 (cons 'arg $2)))))

    (elseif-list
     (elseif-list-1 ($$ (tl->list $1))))
    (elseif-list-1
     ("elseif" expr term stmt-list
      ($$ (make-tl 'elseif-list `(elseif ,$2 ,$4))))
     (elseif-list-1 "elseif" expr term stmt-list
                    ($$ (tl-append $1 `(elseif ,$3 ,$5)))))

    ;; The switch case for this mlang only allows case-expr of form
    ;; @code{fixed}, @code{string}, @code{fixed-list} or @code{string-list}.
    (case-list
     ($empty ($$ (make-tl 'case-list)))
     (case-list "case" case-expr term stmt-list
                ($$ (tl-append $1 `(case ,$3 ,$5)))))
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
     (expr-list-1 ($$ (tl->list $1))))
    (expr-list-1
     (expr ($$ (make-tl 'expr-list $1)))
     (expr-list-1 "," expr ($$ (tl-append $1 $3))))

    ;; We need separate expr and expr w/o space to avoid picking up
    ;; machine that accepts all partial expressions followed by 'sp.

    (expr
     (or-expr)
     (":" ($$ `(colon-expr)))
     (or-expr ":" or-expr ($$ `(colon-expr ,$1 ,$3)))
     (or-expr ":" or-expr ":" or-expr ($$ `(colon-expr ,$1 ,$3 ,$5)))
     (or-expr ":" "end" ($$ `(colon-expr ,$1 (end))))
     (or-expr ":" or-expr ":" "end" ($$ `(colon-expr ,$1 ,$3 (end)))))

    (expr-nosp
     (or-expr-nosp)
     (":" ($$ `(colon-expr)))
     (or-expr-nosp ":" or-expr-nosp ($$ `(colon-expr ,$1 ,$3)))
     (or-expr-nosp ":" or-expr-nosp ":" or-expr-nosp
                   ($$ `(colon-expr ,$1 ,$3 ,$5)))
     (or-expr-nosp ":" "end" ($$ `(colon-expr ,$1 (end))))
     (or-expr-nosp ":" or-expr-nosp ":" "end"
                   ($$ `(colon-expr ,$1 ,$3 (end)))))

    (or-expr
     (and-expr)
     (or-expr "|" and-expr ($$ `(or ,$1 ,$3)))
     (or-expr "||" and-expr ($$ `(ss-or ,$1 ,$3))))

    (or-expr-nosp
     (and-expr-nosp)
     (or-expr-nosp "|" and-expr-nosp ($$ `(or ,$1 ,$3)))
     (or-expr-nosp "||" and-expr-nosp ($$ `(ss-or ,$1 ,$3))))

    (and-expr
     (equality-expr)
     (and-expr "&" equality-expr ($$ `(and ,$1 ,$3)))
     (and-expr "&&" equality-expr ($$ `(ss-and ,$1 ,$3))))

    (and-expr-nosp
     (equality-expr-nosp)
     (and-expr-nosp "&" equality-expr-nosp ($$ `(and ,$1 ,$3)))
     (and-expr-nosp "&&" equality-expr-nosp ($$ `(ss-and ,$1 ,$3))))

    (equality-expr
     (rel-expr)
     (equality-expr "==" rel-expr ($$ `(eq ,$1 ,$3)))
     (equality-expr "~=" rel-expr ($$ `(ne ,$1 ,$3))))

    (equality-expr-nosp
     (rel-expr-nosp)
     (equality-expr-nosp "==" rel-expr-nosp ($$ `(eq ,$1 ,$3)))
     (equality-expr-nosp "~=" rel-expr-nosp ($$ `(ne ,$1 ,$3))))

    (rel-expr
     (add-expr)
     (rel-expr "<" add-expr ($$ `(lt ,$1 ,$3)))
     (rel-expr ">" add-expr ($$ `(gt ,$1 ,$3)))
     (rel-expr "<=" add-expr ($$ `(le ,$1 ,$3)))
     (rel-expr ">=" add-expr ($$ `(ge ,$1 ,$3))))

    (rel-expr-nosp
     (add-expr-nosp)
     (rel-expr-nosp "<" add-expr-nosp ($$ `(lt ,$1 ,$3)))
     (rel-expr-nosp ">" add-expr-nosp ($$ `(gt ,$1 ,$3)))
     (rel-expr-nosp "<=" add-expr-nosp ($$ `(le ,$1 ,$3)))
     (rel-expr-nosp ">=" add-expr-nosp ($$ `(ge ,$1 ,$3))))

    (add-expr
     (mul-expr)
     (add-expr "+" mul-expr ($$ `(add ,$1 ,$3)))
     (add-expr "-" mul-expr ($$ `(sub ,$1 ,$3)))
     (add-expr ".+" mul-expr ($$ `(dot-add ,$1 ,$3)))
     (add-expr ".-" mul-expr ($$ `(dot-sub ,$1 ,$3))))

    (add-expr-nosp
     (mul-expr-nosp)
     (add-expr-nosp "+" mul-expr-nosp ($$ `(add ,$1 ,$3)))
     (add-expr-nosp "-" mul-expr-nosp ($$ `(sub ,$1 ,$3)))
     (add-expr-nosp ".+" mul-expr-nosp ($$ `(dot-add ,$1 ,$3)))
     (add-expr-nosp ".-" mul-expr-nosp ($$ `(dot-sub ,$1 ,$3))))

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

    (mul-expr-nosp
     (unary-expr-nosp)
     (mul-expr-nosp "*" unary-expr-nosp ($$ `(mul ,$1 ,$3)))
     (mul-expr-nosp "/" unary-expr-nosp ($$ `(div ,$1 ,$3)))
     (mul-expr-nosp "\\" unary-expr-nosp ($$ `(ldiv ,$1 ,$3)))
     (mul-expr-nosp "^" unary-expr-nosp ($$ `(pow ,$1 ,$3)))
     (mul-expr-nosp ".*" unary-expr-nosp ($$ `(dot-mul ,$1 ,$3)))
     (mul-expr-nosp "./" unary-expr-nosp ($$ `(dot-div ,$1 ,$3)))
     (mul-expr-nosp ".\\" unary-expr-nosp ($$ `(dot-ldiv ,$1 ,$3)))
     (mul-expr-nosp ".^" unary-expr-nosp ($$ `(dot-pow ,$1 ,$3))))

    (unary-expr
     (postfix-expr)
     ("-" postfix-expr ($$ `(neg ,$2)))
     ("+" postfix-expr ($$ `(pos $2)))
     ("~" postfix-expr ($$ `(not ,$2)))
     ("@" postfix-expr ($$ `(handle ,$2))))
    
    (unary-expr-nosp
     (postfix-expr-nosp)
     ("-" postfix-expr-nosp ($$ `(neg ,$2)))
     ("+" postfix-expr-nosp ($$ $2))
     ("~" postfix-expr-nosp ($$ `(not ,$2))))

    (postfix-expr
     (primary-expr)
     (postfix-expr "'" ($$ `(transpose ,$1)))
     (postfix-expr ".'" ($$ `(conj-transpose ,$1)))
     (postfix-expr "(" expr-list ")" ($$ `(aref-or-call ,$1 ,$3)))
     (postfix-expr "(" ")" ($$ `(aref-or-call ,$1 (expr-list))))
     (postfix-expr "{" expr-list "}" ($$ `(cell-ref ,$1 ,$3)))
     (postfix-expr "." ident ($$ `(sel ,$3 ,$1))))
    
    (postfix-expr-nosp
     (primary-expr-nosp)
     (postfix-expr-nosp "'" ($$ `(transpose ,$1)))
     (postfix-expr-nosp ".'" ($$ `(conj-transpose ,$1)))
     (postfix-expr-nosp "(" expr-list ")" ($$ `(aref-or-call ,$1 ,$3)))
     (postfix-expr-nosp "(" ")" ($$ `(aref-or-call ,$1 (expr-list))))
     (postfix-expr-nosp "{" expr-list "}" ($$ `(cell-ref ,$1 ,$3)))
     (postfix-expr-nosp "." ident ($$ `(sel ,$3 ,$1))))

    (primary-expr
     (ident)
     (number)
     (string)
     ("(" expr ")" ($$ $2))
     ("[" "]" ($$ '(matrix)))
     ("[" matrix-row-list "]" ($$ $2))
     ("{" "}" ($$ '(cell-array)))
     ("{" matrix-row-list "}" ($$ `(cell-array . ,(cdr $2)))))

    (primary-expr-nosp
     (ident)
     (number)
     (string)
     ("(" expr ")" ($$ `(wrap ,$2)))
     ("[" "]" ($$ '(matrix)))
     ("[" matrix-row-list "]" ($$ $2))
     ("{" "}" ($$ '(cell-array)))
     ("{" matrix-row-list "}" ($$ `(cell-array . ,(cdr $2)))))

    (matrix-row-list
     (matrix-row-list-1 ($$ (tl->list $1))))
    (matrix-row-list-1
     (matrix-row ($$ (make-tl 'matrix $1)))
     (matrix-row-list-1 row-term matrix-row ($$ (tl-append $1 $3))))
    (row-term (";") ("\n"))

    (matrix-row
     (matrix-row-1 ($$ (tl->list $1))))
    (matrix-row-1
     (expr-nosp ($$ (make-tl 'row $1)))
     (matrix-row-1 "," expr-nosp ($$ (tl-append $1 $3)))
     (matrix-row-1 'sp expr-nosp ($$ (tl-append $1 $3))))

    (term-list (term) (term-list term))

    (term
     (";" $code-comm "\n")
     (";" "\n")
     (";") (",") ("\n"))

    (lone-comment-list
     (lone-comment-list-1 ($$ (tl->list $1))))
    (lone-comment-list-1
     (lone-comment "\n" ($$ (make-tl 'comm-list $1)))
     (lone-comment-list-1 lone-comment "\n" ($$ (tl-append $1 $2))))

    (ident ($ident ($$ `(ident ,$1))))
    (fixed ($fixed ($$ `(fixed ,$1))))
    (float ($float ($$ `(float ,$1))))
    (number (fixed) (float))
    (string ($string ($$ `(string ,$1))))
    (lone-comment ($lone-comm ($$ `(comm ,$1)))))))

;; === parsers ==========================

(define mlang-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine mlang-spec))
   #:keep 0 #:keepers '($code-comm $lone-comm sp "\n")))

(define mlang-ia-spec (restart-spec mlang-spec 'mlang-item))

(define mlang-ia-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine mlang-ia-spec))
   ;;#:keep 0 #:keepers '($code-comm $lone-comm sp)))
   #:keep 0 #:keepers '(sp)))

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
