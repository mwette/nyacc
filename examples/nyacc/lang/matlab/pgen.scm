;;; lang/matlab/pgen.scm
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

(define-module (lang matlab pgen)
  #:export (matlab-spec
	    matlab-mach
	    gen-matlab-lexer
	    matlab-parser
	    parse-m)
  #:use-module (lang util)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (ice-9 pretty-print)
  )

(define matlab-spec
  (lalr-spec
   (notice lang-crn)
   (start mfile)
   (grammar
    
    (mfile
     (statement-list ($$ `(script-file ,(tl->list $1))))
     (function-file ($$ (tl->list $1)))
     )

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
     (ident "(" expr-list ")" term ($$ `(call ,$1 ,(tl->list $3))))
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
     (command ident-nc-list term ($$ `(command ,$1 ,(tl->list $2))))
     )

    (lval-expr-list
     (lval-expr ($$ (make-tl 'lval-expr-list $1)))
     (lval-expr-list "," lval-expr ($$ (tl-append $1 $3)))
     )

    (lval-expr
     (ident)
     (ident "(" expr-list ")" ($$ `(array-ref ,$1 ,(tl->list $3)))))

    (command
     ("global" ($$ '(ident "global")))
     ("clear" ($$ '(ident "clear")))
     )
    
    (ident-nc-list
     (ident ($$ (make-tl 'ident-list $1)))
     (ident-nc-list ident ($$ (tl-append $1 $3))))

    (elseif-list
     ("elseif" expr term statement-list
      ($$ (make-tl 'elseif-list `(elseif ,$2 ,(tl->list $4)))))
     (elseif-list "elseif" expr term statement-list
		   ($$ (tl-append $1 `(elseif ,$3 ,(tl->list $5)))))
     )
    
    (case-list
     ( ($$ (make-tl)))
     (case-list "case" expr term statement-list
		($$ (tl->append $1 `(case ,$3 ,(tl->list $5)))))
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
     (primary-expr)
     (ident "(" expr-list ")" ($$ `(aref-or-call ,$1 ,(tl->list $3))))
     (postfix-expr "'" ($$ `(xpose ,$1)))
     (postfix-expr ".'" ($$ `(conj-xpose ,$1)))
     ;;(postfix-expr "." ident ($$ `(sel ,$3 ,$1))) ;; struct
     )

    (primary-expr
     (ident)
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
     (lone-comment #\newline ($$ (make-tl 'comment-list $1)))
     (lone-comment-list lone-comment #\newline ($$ (tl-append $1 $2))))

    (term (#\newline) (";") (","))
    (ident ('$ident ($$ `(ident ,$1))))
    (number ('$fx ($$ `(fixed ,$1))) ('$fl ($$ `(float ,$1))))
    (string ('$string ($$ `(string ,$1))))
    (lone-comment ('$lone-comm ($$ `(comm ,$1))))
    ;;(code-comment ('$code-comm ($$ `(comm ,$1))))
    )))

;;; === lexical analyzer

(define (matlab-read-string ch)
  (if (not (eq? ch #\')) #f
      (let iter ((cl '()) (ch (read-char)))
	(cond ((eq? ch #\\)
	       (let ((c1 (read-char)))
		 (if (eq? c1 #\newline)
		     (iter cl (read-char))
		     (iter (cons c1 cl) (read-char)))))
	      ((eq? ch #\')
	       (let ((nextch (read-char)))
		 (if (eq? #\' nextch)
		     (iter (cons ch cl) (read-char))
		     (begin
		       (unread-char nextch)
		       (cons '$string (list->string (reverse cl)))))))
	      (else (iter (cons ch cl) (read-char)))))))

(define matlab-read-comm (make-comm-reader '(("%" . "\n"))))

(define (make-matlab-lexer-generator match-table)
  
  ;; elipsis reader "..." whitespace "\n"
  (define (elipsis? ch)
    (if (eqv? ch #\.)
	(let ((c1 (read-char)))
	  (if (eqv? c1 #\.)
	      (let ((c2 (read-char)))
		(if (eqv? c2 #\.)
		    (cons (string->symbol "...") "...")
		    (begin (unread-char c2) (unread-char c1) #f)))
	      (begin (unread-char c1) #f)))
	#f))

  (define (skip-to-next-line)
    (let iter ((ch (read-char)))
      (cond
       ((eof-object? ch) ch)
       ((eqv? ch #\newline) (read-char))
       (else (iter (read-char))))))
  
  (let* ((read-string matlab-read-string)
	 (space-cs (string->char-set " \t\r"))
	 ;;
	 (strtab (filter-mt string? match-table)) ; strings in grammar
	 (kwstab (filter-mt like-c-ident? strtab))  ; keyword strings =>
	 (keytab (map-mt string->symbol kwstab))  ; keywords in grammar
	 (chrseq (remove-mt like-c-ident? strtab))  ; character sequences
	 (symtab (filter-mt symbol? match-table)) ; symbols in grammar
	 (chrtab (filter-mt char? match-table))	  ; characters in grammar
	 ;;
	 (read-chseq (make-chseq-reader chrseq))
	 (assc-$ (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair))))
	 )
    (lambda ()
      (let ((qms #f) (bol #t))		; qms: quote means space
	;; C pgen handles bol in a cleaner way IMO
	(lambda ()
	  (let iter ((ch (read-char)))
	    (cond
	     ((eof-object? ch) (assc-$ (cons '$end ch)))
 	     ((elipsis? ch) (iter (skip-to-next-line)))
	     ((char-set-contains? space-cs ch) (iter (read-char)))
	     ((and (eqv? ch #\newline) (set! bol #t) #f))
	     ((matlab-read-comm ch) => ;; (lambda () (iter (read-char)))) OR
	      (lambda (p)
		(let ((was-bol bol))
		  (set! bol #f)
		  (cons (if was-bol
			    (assq-ref symtab '$lone-comm)
			    (assq-ref symtab '$code-comm))
			(cdr p)))))
	     ((read-c-ident ch) =>
	      (lambda (s)
		(set! qms #f)
		(or (and=> (assq-ref keytab (string->symbol s))
			   (lambda (tval) (cons tval s)))
		    (assc-$ (cons '$ident s)))))
	     ((read-c-num ch) => (lambda (p) (set! qms #f) (assc-$ p)))
	     ((eqv? ch #\') (if qms (assc-$ (read-string ch)) (read-chseq ch)))
	     ((read-chseq ch) =>
	      (lambda (p)
		(cond ((memq ch '(#\= #\, #\()) (set! qms #t))
		      (else (set! qms #f)))
		p))
	     ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
	     (else (cons ch (string ch)))))))))) ; else should be error


;;; === now build the parser

(define matlab-mach
  (hashify-machine
   ;;(identity
   ;;(compact-machine
   (identity
    (make-lalr-machine matlab-spec))))

(define gen-matlab-lexer
  (make-matlab-lexer-generator (lalr-match-table matlab-mach)))

(define matlab-parser (make-lalr-parser matlab-mach))

(define (parse-m) (matlab-parser (gen-matlab-lexer)))

;;; --- last line
