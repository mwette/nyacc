;;; lang/matlab/mach.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; matlab parser
;; 1) does NOT parse lone expressions of the form [ 1, 2] => syntax error
;; 2) does NOT support non-comma rows [ 1 2 ] => syntax error

(define-module (nyacc lang matlab mach)
  #:export (matlab-spec
	    matlab-mach
	    dev-parse-ml
	    gen-matlab-lexer
	    gen-matlab-files)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (ice-9 pretty-print)
  )

;; TODO:
;; 1) function handles: {foo = @bar;} where bar is a function
;; 2) anonymous functions: {foo = @(arg1,arg2) arg1 + arg2;}
;; 3) structs
;; 4) cell arrays (hoping not needed)

(define matlab-spec
  (lalr-spec
   (notice lang-crn-lic)
   (start mfile)
   (grammar
    
    (mfile
     (script-file ($$ (tl->list (add-file-attr $1))))
     (function-file ($$ (tl->list (add-file-attr $1))))
     )

    (script-file
     (lone-comment-list
      non-comment-statement
      ($$ (make-tl 'script-file (tl->list $1))))
     (non-comment-statement
      ($$ (if $1 (make-tl 'script-file $1) (make-tl 'script-file))))
     (script-file statement ($$ (if $2 (tl-append $1 $2) $1))))

    (function-file
     (function-defn ($$ (make-tl 'function-file $1)))
     (function-file function-defn ($$ (tl-append $1 $2)))
     )

    (function-defn
     (function-decl non-comment-statement statement-list opt-end
      ($$ `(fctn-defn ,$1 ,(tl->list (if $2 (tl-insert $3 $2) $3)))))
     (function-decl non-comment-statement opt-end
      ($$ `(fctn-defn ,$1 ,(if $2 `(stmt-list ,$2) '(stmt-list)))))
     (function-decl opt-end
      ($$ `(fctn-defn ,$1 (stmt-list))))
     )
    (opt-end () ("end" term-list))

    (function-decl
     (function-decl-line lone-comment-list
			 ($$ (append $1 (list (tl->list $2)))))
     (function-decl-line))
    (nl-list (#\newline) (nl-list #\newline))

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

    ;;(opt-ident-list () (ident-list))
    (ident-list
     (ident ($$ (make-tl 'ident-list $1)))
     (ident-list "," ident ($$ (tl-append $1 $3))))

    (statement-list
     (statement ($$ (if $1 (make-tl 'stmt-list $1) (make-tl 'stmt-list))))
     (statement-list statement ($$ (if $2 (tl-append $1 $2) $1))))

    (statement
     (lone-comment)
     (non-comment-statement))
    (non-comment-statement
     (term ($$ #f))
     (lval-expr "(" expr-list ")" term ($$ `(call ,$1 ,(tl->list $3))))
     (lval-expr "=" expr term ($$ `(assn ,$1 ,$3)))
     ("[" lval-expr-list "]" "=" ident "(" ")" term
      ($$ `(multi-assign ,(tl->list $2) ,$5 (expr-list))))
     ("[" lval-expr-list "]" "=" ident "(" expr-list ")" term
      ($$ `(multi-assign ,(tl->list $2) ,$5 ,(tl->list $7))))
     ("for" ident "=" expr term statement-list "end" term
      ($$ `(for ,$2 ,$4 ,(tl->list $6))))
     ("while" expr term statement-list "end" term
      ($$ `(while ,$2 ,(tl->list $4))))
     ("if" expr term statement-list elseif-list "else" statement-list "end" term
      ($$ `(if ,$2 ,(tl->list $4) ,@(cdr (tl->list $5)) (else ,(tl->list $7)))))
     ("if" expr term statement-list "else" statement-list "end" term
      ($$ `(if ,$2 ,(tl->list $4) (else ,(tl->list $6)))))
     ("if" expr term statement-list "end" term
      ($$ `(if ,$2 ,(tl->list $4))))
     ("switch" expr term case-list "otherwise" term statement-list "end" term
      ($$ `(switch ,$2 ,@(tl->list $4) (otherwise ,(tl->list $7)))))
     ("switch" expr term case-list "end" term
      ($$ `(switch ,$2 ,@(tl->list $4))))
     ("return" term
      ($$ '(return)))
     (command arg-list term ($$ `(command ,$1 ,(tl->list $2))))
     )

    (lval-expr-list
     (lval-expr ($$ (make-tl 'lval-expr-list $1)))
     (lval-expr-list "," lval-expr ($$ (tl-append $1 $3)))
     )

    (command
     ("global" ($$ '(ident "global")))
     ("clear" ($$ '(ident "clear")))
     )
    ;; Only ident list type commands are allowed
    (arg-list
     (ident ($$ (make-tl 'arg-list (cons 'arg (cdr $1)))))
     (arg-list ident ($$ (tl-append $1 (cons 'arg $2)))))

    (elseif-list
     ("elseif" expr term statement-list
      ($$ (make-tl 'elseif-list `(elseif ,$2 ,(tl->list $4)))))
     (elseif-list "elseif" expr term statement-list
		   ($$ (tl-append $1 `(elseif ,$3 ,(tl->list $5)))))
     )
    
    (case-list
     ($empty ($$ (make-tl 'case-list)))
     (case-list "case" expr term statement-list
		($$ (tl-append $1 `(case ,$3 ,(tl->list $5)))))
     )

    ;; Lone colon-expr's can only exist in expr-list for array ref.
    (expr-list
     (expr ($$ (make-tl 'expr-list $1)))
     (":" ($$ (make-tl 'expr-list '(colon-expr))))
     (expr-list "," expr ($$ (tl-append $1 $3)))
     (expr-list "," ":" ($$ (tl-append $1 '(colon-expr))))
     )

    (expr
     (or-expr)
     (expr ":" or-expr ($$ `(colon-expr ,$1 ,$3)))
     )

    (or-expr
     (and-expr)
     (or-expr "|" and-expr ($$ `(or ,$1 ,$3)))
     )

    (and-expr
     (equality-expr)
     (and-expr "&" equality-expr ($$ `(and ,$1 ,$3)))
     )

    (equality-expr
     (rel-expr)
     (equality-expr "==" rel-expr ($$ `(equal ,$1 ,$3)))
     (equality-expr "~=" rel-expr ($$ `(noteq ,$1 ,$3)))
     )

    (rel-expr
     (add-expr)
     (rel-expr "<" add-expr ($$ `(lt ,$1 ,$3)))
     (rel-expr ">" add-expr ($$ `(gt ,$1 ,$3)))
     (rel-expr "<=" add-expr ($$ `(le ,$1 ,$3)))
     (rel-expr ">=" add-expr ($$ `(ge ,$1 ,$3)))
     )

    (add-expr
     (mul-expr)
     (add-expr "+" mul-expr ($$ `(add ,$1 ,$3)))
     (add-expr "-" mul-expr ($$ `(sub ,$1 ,$3)))
     )

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
     )

    (unary-expr
     (postfix-expr)
     ("-" postfix-expr ($$ `(neg ,$2)))
     ("+" postfix-expr ($$ $2))
     ("~" postfix-expr ($$ `(not ,$2)))
     )

    (postfix-expr
     (lval-expr)
     (primary-expr)
     (postfix-expr "'" ($$ `(xpose ,$1)))
     (postfix-expr ".'" ($$ `(conj-xpose ,$1)))
     )

    (lval-expr
     (ident)
     (lval-expr "(" expr-list ")" ($$ `(aref-or-call ,$1 ,(tl->list $3))))
     (lval-expr "." ident ($$ `(sel ,$3 ,$1)))
     )
    
    (primary-expr
     (number)
     (string)
     ("(" expr ")" ($$ $2))
     ("[" "]" ($$ '(matrix)))
     ("[" matrix-row-list "]" ($$ (tl->list $2)))
     )

    (matrix-row-list
     (matrix-row ($$ (make-tl 'matrix (tl->list $1))))
     (matrix-row-list row-term matrix-row ($$ (tl-append $1 (tl->list $3))))
     )
    (row-term (";") (#\newline))

    (matrix-row
     (expr ($$ (make-tl 'row $1)))
     (matrix-row "," expr ($$ (tl-append $1 $3)))
     )

    (term-list (term) (term-list term))

    (lone-comment-list
     (lone-comment #\newline ($$ (make-tl 'comm-list $1)))
     (lone-comment-list lone-comment #\newline ($$ (tl-append $1 $2))))

    (term (#\newline) (";") (","))
    (ident ($ident ($$ `(ident ,$1))))
    (number ($fixed ($$ `(fixed ,$1))) ($float ($$ `(float ,$1))))
    (string ($string ($$ `(string ,$1))))
    (lone-comment ($lone-comm ($$ `(comm ,$1))))
    ;;(code-comment ($code-comm ($$ `(comm ,$1))))
    )))

;; === parser ==========================

(define matlab-mach
  (identity ;;(hashify-machine
   (identity ;;(compact-machine
    (make-lalr-machine matlab-spec))))

(define mtab (assq-ref matlab-mach 'mtab))
(include-from-path "nyacc/lang/matlab/body.scm")

(define raw-parser (make-lalr-parser matlab-mach))

(define* (dev-parse-ml #:key debug)
  (catch
   'parse-error
   (lambda ()
     (raw-parser (gen-matlab-lexer) #:debug debug))
   (lambda (key fmt . rest)
     (apply simple-format (current-error-port) (string-append fmt "\n") rest)
     #f)))

;; === automaton file generator =========

(define (gen-matlab-files . rest)
  (define (lang-dir path)
    (if (pair? rest) (string-append (car rest) "/" path) path))
  (define (xtra-dir path)
    (lang-dir (string-append "mach.d/" path)))

  (write-lalr-actions matlab-mach (xtra-dir "mlact.scm.new"))
  (write-lalr-tables matlab-mach (xtra-dir "mltab.scm.new"))
  (let ((a (move-if-changed (xtra-dir "mlact.scm.new")
			    (xtra-dir "mlact.scm")))
	(b (move-if-changed (xtra-dir "mltab.scm.new")
			    (xtra-dir "mltab.scm"))))
    (when (or a b) 
      (system (string-append "touch " (lang-dir "parser.scm"))))))


;;; --- last line ---
