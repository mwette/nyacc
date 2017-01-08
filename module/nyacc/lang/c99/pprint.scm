;;; nyacc/lang/c99/pprint.scm
;;;
;;; Copyright (C) 2015,2016 Matthew R. Wette
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

(define-module (nyacc lang c99 pprint)
  #:export (pretty-print-c99)
  #:use-module ((srfi srfi-1) #:select (pair-for-each))
  #:use-module (nyacc lang util)
  #:use-module (sxml match)
  #:use-module (ice-9 pretty-print)
  )

(define op-sym
  (let ((ot '(("=" . eq) ("+=" . pl-eq) ("-=" . mi-eq) ("*=" . ti-eq)
	      ("/=" . di-eq) ("%=" . mo-eq) ("<<=" . ls-eq) (">>=" . rs-eq)
	      ("&=" . ba-eq) ("^=" . bx-eq) ("|=" bo-eq))))
    (lambda (name)
      (assoc-ref ot name))))

(define op-prec
  ;; in order of decreasing precedence
  '((p-expr ident fixed float string)
    (comp-lit post-inc post-dec i-sel d-sel fctn-call array-ref)
    (de-ref ref-to neg pos not bitwise-not sizeof pre-inc pre-dec)
    (cast)
    (mul div mod)
    (add sub)
    (lshift rshift)
    (lt gt le ge)
    (eq ne)
    (bitwise-and)
    (bitwise-xor)
    (bitwise-or)
    (and)
    (or)
    (cond-expr)
    (assn-expr)
    (comma)))

(define op-assc
  '((left array-ref d-sel i-sel post-inc post-dec comp-lit mul div mod add sub
	  lshift rshift lt gt le ge bitwise-and bitwise-xor bitwise-or and or)
    (right pre-inc pre-dec sizeof bitwise-not not pos neg ref-to de-ref cast
	   cond assn-expr)
    (nonassoc)))

(define protect-expr? (make-protect-expr op-prec op-assc))

;; @deffn pretty-print-c99 tree [#:indent-level 2]
;; Convert and print a C99 sxml tree to the current output port.
;; The optional keyword argument @code{#:indent-level} provides the
;; indent level, with default of 2.
(define* (pretty-print-c99 tree #:key (indent-level 2) (ugly #f))

  ;;(define fmtr (make-pp-formatter))
  (define fmtr (if ugly (make-pp-formatter/ugly) (make-pp-formatter)))
  (define (push-il)(fmtr 'push))
  (define (pop-il) (fmtr 'pop))

  (define (sf . args) (apply fmtr args))

  (define (cpp-ppx tree)
    (fmtr 'nlin)
    (sxml-match tree
      ((define (name ,name) (args . ,args) (repl ,repl))
       (sf "#define ~A(" name)
       (pair-for-each
	(lambda (pair) (sf "~A" (car pair)) (if (pair? (cdr pair)) (sf ",")))
	args)
       (sf ") ~A\n" repl))
      ((define (name ,name) (repl ,repl))
       (sf "#define ~A ~A\n" name repl))
      ((if ,text) (sf "#if ~A\n" text))
      ((elif ,text) (sf "#elif ~A\n" text))
      ((else ,text) (sf "#else ~A\n" text))
      ((else) (sf "#else\n"))
      ((endif ,text) (sf "#endif ~A\n" text))
      ((endif) (sf "#endif\n"))
      ((include . ,rest) (sf "#include ~A\n" (sx-ref tree 1)))
      ((error ,text) (sf "#error ~A\n" text))
      ((pragma ,text) (sf "#pragma ~A\n" text))
      (,otherwise
       (simple-format #t "\n*** pprint/cpp-ppx: NO MATCH: ~S\n" tree))
      )
    (fmtr 'nlin))

  (define (unary/l op rep rval)
    (sf rep)
    (if (protect-expr? 'rt op rval)
	(ppx/p rval)
	(ppx rval)))
  
  (define (unary/r op rep lval)
    (if (protect-expr? 'lt op lval)
	(ppx/p lval)
	(ppx lval))
    (sf rep))
  
  (define (binary op rep lval rval)
    (if (protect-expr? 'lt op lval)
	(ppx/p lval)
	(ppx lval))
    (sf rep)
    (if (protect-expr? 'rt op rval)
	(ppx/p rval)
	(ppx rval)))

  (define (comp declr initr)
    (let ((iexpr (and initr (sx-ref initr 1))))
      (ppx declr)
      (when initr
	(sf " = ")
	(case (sx-tag iexpr)
	  ((initzer-list)
	   (sf "{")
	   (for-each
	    (lambda (expr) (ppx (sx-ref expr 1)) (sf ", "))
	    (sx-tail iexpr 1))
	   (sf "}"))
	  (else
	   (ppx iexpr))))))

  (define (struct-union-def struct-or-union name fields)
    (if name
	(sf "~A ~A {\n" struct-or-union name)
	(sf "~A {\n" struct-or-union))
    (push-il)
    (for-each ppx fields)
    (pop-il)
    (sf "}"))

  (define (ppx/p tree) (sf "(") (ppx tree) (sf ")"))

  ;; TODO: comp-lit
  (define (ppx-1 tree)
    (sxml-match tree

      ((p-expr ,expr) (ppx expr))
      ((ident ,name) (sf "~A" name))
      ((char ,value) (sf "'~A'" (sx-ref tree 1)))
      ((fixed ,value) (sf "~A" value))
      ((float ,value) (sf "~A" value))

      ((string . ,value-l)
       (pair-for-each
	(lambda (pair)
	  (sf "~S" (car pair))
	  (if (pair? (cdr pair)) (sf " ")))
	value-l))

      ((comment ,text) (sf "/*~A*/\n" text))

      ((scope ,expr) (sf "(") (ppx expr) (sf ")"))
      
      ((array-ref ,dim ,expr)
       (ppx expr) (sf "[") (ppx dim) (sf "]"))

      ((d-sel ,id ,ex) (binary 'd-del "." ex id))
      ((i-sel ,id ,ex) (binary 'i-del "->" ex id))

      ((pre-inc ,expr) (unary/l 'pre-inc "++" expr))
      ((pre-dec ,expr) (unary/l 'pre-dec "--" expr))
      ((ref-to ,expr) (unary/l 'ref-to "&" expr))
      ((de-ref ,expr) (unary/l 'de-ref "*" expr))
      ((pos ,expr) (unary/l 'pos "+" expr))
      ((neg ,expr) (unary/l 'neg "-" expr))
      ((bitwise-not ,expr) (unary/l 'bitwise-not "~" expr))
      ((not ,expr) (unary/l 'not "!" expr))
      ((sizeof-expr ,expr) (sf "sizeof(") (ppx expr) (sf ")"))
      ((sizeof-type ,type) (sf "sizeof(") (ppx type) (sf ")"))

      ((cast ,tn ,ex)
       (sf "(") (ppx tn) (sf ")")
       (if (protect-expr? 'rt 'cast ex)
	   (ppx/p ex)
	   (ppx ex)))

      ((add ,lval ,rval) (binary 'add " + " lval rval))
      ((sub ,lval ,rval) (binary 'sub " - " lval rval))
      ((mul ,lval ,rval) (binary 'mul "*" lval rval))
      ((div ,lval ,rval) (binary 'div "/" lval rval))
      ((mod ,lval ,rval) (binary 'mod "%" lval rval))

      ((lshift ,lval ,rval) (binary 'lshift "<<" lval rval))
      ((rshift ,lval ,rval) (binary 'lshift "<<" lval rval))

      ((lt ,lval ,rval) (binary 'lt " < " lval rval))
      ((gt ,lval ,rval) (binary 'gt " > " lval rval))

      ((le ,lval ,rval) (binary 'le " <= " lval rval))
      ((ge ,lval ,rval) (binary 'ge " >= " lval rval))
      ((eq ,lval ,rval) (binary 'eq " == " lval rval))
      ((ne ,lval ,rval) (binary 'ne " != " lval rval))
      
      ((bitwise-and ,lval ,rval) (binary 'bitwise-and " & " lval rval))
      ((bitwise-or ,lval ,rval) (binary 'bitwise-and " | " lval rval))
      ((bitwise-xor ,lval ,rval) (binary 'bitwise-xor " ^ " lval rval))

      ((and ,lval ,rval) (binary 'and " && " lval rval))
      ((or ,lval ,rval) (binary 'and " || " lval rval))

      ;; CHECK THIS
      ((cond-expr ,cond ,tval ,fval)
       (ppx cond) (sf "? ") (ppx tval) (sf ": ") (ppx fval))

      ((post-inc ,expr) (unary/r 'post-inc "++" expr))
      ((post-dec ,expr) (unary/r 'post-dec "--" expr))

      ;; TODO: check protection 
      ((fctn-call ,expr ,arg-list)
       (if (protect-expr? 'rt 'fctn-call expr)
	   (ppx/p expr)
	   (ppx expr))
       (sf "(")
       (ppx arg-list)
       (sf ")"))

      ((expr-list . ,expr-l)
       (pair-for-each
	(lambda (pair)
	  (ppx (car pair))
	  (if (pair? (cdr pair)) (sf ", ")))
	expr-l))
      
      ((assn-expr ,lval ,op ,rval)
       (binary (car op) (simple-format #f " ~A " (cadr op)) lval rval))

      ;; TODO: check protection
      ((comma-expr . ,expr-list)
       (pair-for-each
	(lambda (pair)
	  (cond
	   ((pair? (cdr pair))
	    (if (protect-expr? 'rt 'comma-expr (car pair))
		(ppx/p (car pair))
		(ppx (car pair)))
	    (sf ", "))
	   (else (ppx (car pair)))))
	expr-list))

      ;; #|
      ;; gotta break up ppx because sxml-match seems to eat stack space:
      ;; everthing together results in SIGABRT from vm_error_stack_overflow()
      (,otherwise
       (ppx-2 tree))))
  
  (define (ppx-2 tree)
    
    (sxml-match tree
      ;; sxml-match continues here to avoid stack overflow
      ;; |#
      
      ((decl ,decl-spec-list)
       (ppx decl-spec-list) (sf ";\n"))
      ((decl ,decl-spec-list ,init-declr-list)
       (ppx decl-spec-list) (ppx init-declr-list) (sf ";\n"))
      ((decl ,decl-spec-list ,init-declr-list ,comment)
       (ppx decl-spec-list) (ppx init-declr-list) (sf "; ") (ppx comment))
      ((decl-no-newline ,decl-spec-list ,init-declr-list) ; for (int i = 0;
       (ppx decl-spec-list) (ppx init-declr-list) (sf ";"))

      ((comp-decl ,spec-qual-list ,declr-list)
       (ppx spec-qual-list) (ppx declr-list) (sf ";\n"))
      ((comp-decl ,spec-qual-list ,declr-list ,comment)
       (ppx spec-qual-list) (ppx declr-list) (sf "; ") (ppx comment))

      ((decl-spec-list . ,dsl)
       (let iter ((dsl dsl))
	 (when (pair? dsl)
	   (case (sx-tag (car dsl))
	     ((stor-spec) (sf "~A " (car (sx-ref (car dsl) 1))))
	     ((type-qual) (sf "~A " (sx-ref (car dsl) 1)))
	     ((type-spec) (ppx (car dsl)))
	     (else (sf "[?:~S] " (car dsl))))
	   ;;(if (pair? (cdr dsl)) (sf " "))
	   (iter (cdr dsl)))))

      ((init-declr-list . ,rest)
       (pair-for-each
	(lambda (pair)
	  (sf " ")
	  (ppx (car pair))
	  (if (pair? (cdr pair)) (sf ",")))
	rest))
      ((comp-declr-list . ,rest)
       (pair-for-each
	(lambda (pair)
	  (sf " ")
	  (ppx (car pair))
	  (if (pair? (cdr pair)) (sf ",")))
	rest))

      ((init-declr ,declr ,initr) (comp declr initr))
      ((init-declr ,declr) (comp declr #f))
      ((comp-declr ,declr) (comp declr #f))
      ((param-declr ,declr) (comp declr #f))

      ((type-spec ,arg)
       (case (sx-tag arg)
	 ((fixed-type) (sf "~A" (sx-ref arg 1)))
	 ((float-type) (sf "~A" (sx-ref arg 1)))
	 ((struct-ref) (ppx arg))
	 ((struct-def) (ppx arg))
	 ((union-ref) (ppx arg))
	 ((union-def) (ppx arg))
	 ((enum-def) (ppx arg))
	 ((typename) (sf "~A" (sx-ref arg 1)))
	 ((void) (sf "void"))
	 (else (error "missing " arg))))

      ((struct-ref (ident ,name)) (sf "struct ~A" name))
      ((union-ref (ident ,name)) (sf "union ~A" name))
      
      ((struct-def (ident ,name) (field-list . ,fields))
       (struct-union-def 'struct name fields))
      ((struct-def (field-list . ,fields))
       (struct-union-def 'struct #f fields))
      ((union-def (ident ,name) (field-list . ,fields))
       (struct-union-def 'union name fields))
      ((union-def (field-list . ,fields))
       (struct-union-def 'union #f fields))

      ((enum-def (ident ,name) (enum-def-list . ,edl))
       (sf "enum ~A " name) (ppx `(enum-def-list . ,edl)))

      ((enum-def (enum-def-list . ,edl))
       (sf "enum ") (ppx `(enum-def-list . ,edl)))

      ((enum-def-list . ,defns)
       (sf "{\n") (push-il)
       (for-each ppx defns)
       (pop-il) (sf "}"))

      ((enum-defn (ident ,name) (p-expr (fixed ,value)))
       (sf "~A = ~A,\n" name value))
      ((enum-defn (ident ,name))
       (sf "~A,\n" name))

      ((fctn-spec "inline")
       (sf "inline "))

      ((ptr-declr ,ptr ,dir-declr)
       (ppx ptr) (ppx dir-declr))

      ((pointer) (sf "*"))
      ((pointer ,one) (sf "*") (ppx one))
      ((pointer ,one ,two) (sf "*") (ppx one) (ppx two))

      ((array-of ,dir-declr ,arg)
       (ppx dir-declr) (sf "[") (ppx arg) (sf "]"))
      ((array-of ,dir-declr)
       (ppx dir-declr) (sf "[]"))
      ;; MORE TO GO
      
      ((ftn-declr ,dir-declr ,param-list)
       (ppx dir-declr) (sf "(") (ppx param-list) (sf ")"))

      ((type-name ,spec-qual-list ,abs-declr)
       (ppx spec-qual-list) (ppx abs-declr))
      ((type-name ,decl-spec-list)
       (ppx decl-spec-list))

      ((abs-declr ,pointer ,dir-abs-declr) (ppx pointer) (ppx dir-abs-declr))
      ((abs-declr ,one-of-above) (ppx one-of-above))

      ((compd-stmt (block-item-list . ,items))
       (sf "{\n") (push-il) (for-each ppx items) (pop-il) (sf "}\n"))
      ((compd-stmt-no-newline (block-item-list . ,items))
       (sf "{\n") (push-il) (for-each ppx items) (pop-il) (sf "} "))
      
      ;; #|
      ;; gotta break up ppx because sxml-match seems to eat stack space:
      ;; everthing together results in SIGABRT from vm_error_stack_overflow()
      (,otherwise
       (ppx-3 tree))))
  
  (define (ppx-3 tree)

    (sxml-match tree
      ;; sxml-match continues here to avoid stack overflow
      ;; |#
      
      ;; expression-statement
      ((expr-stmt ,expr) (ppx expr) (sf ";\n"))
      ((expr-stmt ,expr ,comm) (ppx expr) (sf "; ") (ppx comm))
      
      ((expr) (sf ""))		; for lone expr-stmt and return-stmt

      ;; selection-statement
      ((if . ,rest)
       (let ((cond-part (sx-ref tree 1))
	     (then-part (sx-ref tree 2)))
	 (sf "if (") (ppx cond-part) (sf ") ")
	 (ppx then-part)
	 (let iter ((else-l (sx-tail tree 3)))
	   (cond
	    ((null? else-l) #t)
	    ((eqv? 'else-if (caar else-l))
	     (sf "else if (") (ppx (sx-ref (car else-l) 1)) (sf ") ")
	     (ppx (sx-ref (car else-l) 2))
	     (iter (cdr else-l)))
	    (else
	     (sf "else ")
	     (ppx (car else-l)))))))

      ((switch ,expr (compd-stmt (block-item-list . ,items)))
       (sf "switch (") (ppx expr) (sf ") {\n")
       (for-each
	(lambda (item)
	  (unless (memq (car item) '(case default)) (push-il))
	  (ppx item)
	  (unless (memq (car item) '(case default)) (pop-il)))
	items)
       (sf "}\n"))

      ;; labeled-statement
      ((case ,expr ,stmt)
       (sf "case ") (ppx expr) (sf ":\n")
       (push-il) (ppx stmt) (pop-il))

      ((default ,stmt)
       (sf "default:\n")
       (push-il) (ppx stmt) (pop-il))

      ;; CHECK THIS
      ((while ,expr ,stmt)
       (sf "while (") (ppx expr) (sf ") ") (ppx stmt)
       )

      ;; This does not meet the convention of "} while" on same line. 
      ((do-while ,stmt ,expr)
       (sf "do ")
       (if (eqv? 'compd-stmt (sx-tag stmt)) 
	   (ppx `(compd-stmt-no-newline ,(sx-ref stmt 1)))
	   (ppx stmt))
       (sf "while (") (ppx expr) (sf ");\n"))
      
      ;; for
      ((for (decl . ,rest) ,test ,iter ,stmt)
       (sf "for (") (ppx `(decl-no-newline . ,rest))
       (sf " ") (ppx test) (sf "; ") (ppx iter) (sf ") ")
       (ppx stmt))

      ((for (decl . ,rest) ,expr2 ,expr3 ,stmt)
       (sf "for (")
       (ppx `(decl . ,rest)) (sf " ") (ppx expr2) (sf "; ") (ppx expr3)
       (sf ") ") (ppx stmt))
      ((for ,expr1 ,expr2 ,expr3 ,stmt)
       (sf "for (")
       (ppx expr1) (sf "; ") (ppx expr2) (sf "; ") (ppx expr3)
       (sf ") ") (ppx stmt))

      ;; jump-statement
      ((goto ,where)
       (pop-il)			; unindent
       (sf "goto ~A;" (sx-ref where 1))
       ;; comment?
       (sf "\n")
       (push-il))			; re-indent

      ((continue) (sf "continue;\n"))
      ((break) (sf "break;\n"))
      ((return ,expr) (sf "return ") (ppx expr) (sf ";\n"))
      ((return) (sf "return;\n"))

      ((trans-unit . ,items)
       (pair-for-each
	(lambda (pair)
	  (let ((this (car pair))
		(next (and (pair? (cdr pair)) (cadr pair))))
	    (ppx this)
	    (cond ;; add blank line if next is different or fctn defn
	     ((not next))
	     ((not (eqv? (sx-tag this) (sx-tag next))) (sf "\n"))
	     ((eqv? (sx-tag next) 'fctn-defn) (sf "\n")))))
	items))

      ((fctn-defn . ,rest) ;; but not yet (knr-fctn-defn)
       (let* ((decl-spec-list (sx-ref tree 1))
	      (declr (sx-ref tree 2))
	      (compd-stmt (sx-ref tree 3)))
	 (ppx decl-spec-list)
	 (sf " ")
	 (ppx declr)
	 (sf " ")
	 (ppx compd-stmt)))

      ((ptr-declr . ,rest)
       (ppx (sx-ref tree 1)) (ppx (sx-ref tree 2)))
      
      ((ftn-declr . ,rest)
       (ppx (sx-ref tree 1))	; direct-declarator
       (sf "(") (ppx (sx-ref tree 2)) (sf ")"))

      ((param-list . ,params)
       (pair-for-each
	(lambda (pair) (ppx (car pair)) (if (pair? (cdr pair)) (sf ", ")))
	params))

      ((ellipsis)	;; should work
       (sf "..."))

      ((param-decl ,decl-spec-list ,param-declr)
       (ppx decl-spec-list) (sf " ") (ppx param-declr))
      ((param-decl ,decl-spec-list)
       (ppx decl-spec-list))
      
      ((cpp-stmt . ,rest)
       (cpp-ppx (sx-ref tree 1)))

      ((extern-begin ,lang) (sf "extern \"~A\" {\n" lang))
      ((extern-end) (sf "}\n"))

      (,otherwise
       (simple-format #t "\n*** pprint/ppx: NO MATCH: ~S\n" (car tree)))
      ))

  (define ppx ppx-1)

  (if (not (pair? tree)) (error "expecing sxml tree"))
  (ppx tree)
  (if ugly (newline)))

;; --- last line ---
