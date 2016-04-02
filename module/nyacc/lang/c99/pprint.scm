;;; nyacc/lang/c99/pprint.scm
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

(define* (pretty-print-c99 tree #:key (indent-level 2))

  (define fmtr (make-pp-formatter))

  (define sf (lambda args (apply fmtr args)))
  
  (define cpp-ppx
    (lambda (tree)
      (sxml-match tree
	((define . ,rest)
	 (sf "#define ~A" (sx-ref (sx-ref tree 1) 1))
	 (and=> (assq-ref tree 'args)
		(lambda (args)
		  (sf "(")
		  (pair-for-each
		   (lambda (pair)
		     (sf "~A" (car pair))
		     (if (pair? (cdr pair)) (sf ",")))
		   args)
		  (sf ")")))
	 (sf " ~A\n" (sx-ref (assq 'repl (cdr tree)) 1)))
	;; Output #ifdef and #ifndef when possible.
	((if (defined ,text)) (guard (string? text))
	 (sf "#ifdef ~A\n" text))
	((if (not (defined ,text))) (guard (string? text))
	 (sf "#ifndef ~A\n" text))
	;; #if with expression:
	;;((if . ,rest) (sf "#if ") (cpp-ppx (sx-ref tree 1)) (sf "\n"))
	;; now with text
	((if ,text) (sf "#if ~S\n" text))
	;; easy statements
	((elif . ,rest) (sf "#elif ") (cpp-ppx (sx-ref tree 1)) (sf "\n"))
	((else . ,rest) (sf "#else\n"))
	((endif . ,rest) (sf "#endif\n"))
	((include . ,rest) (sf "#include ~A\n" (sx-ref tree 1)))
	((error . ,rest) (sf "#error ~A\n" (sx-ref tree 1)))

	;; expressions ..
	((defined . ,rest)
	 (sf "defined(~A)" (sx-ref tree 1)))
	(,otherwise
	 (simple-format (current-error-port) "NO MATCH: ~S\n" tree))
	)))

  (define (unary/l op rep rval)
    (sf rep)
    (if (protect-expr? 'rt op rval)
	(ppx/p rval)
	(ppx rval)))
  
  (define (unary/r op rep lval)
    (sf rep)
    (if (protect-expr? 'lt op lval)
	(ppx/p lval)
	(ppx lval)))
  
  (define (binary op rep lval rval)
    (if (protect-expr? 'lt op lval)
	(ppx/p lval)
	(ppx lval))
    (sf rep)
    (if (protect-expr? 'rt op rval)
	(ppx/p rval)
	(ppx rval)))
	   
  (define ppx/p (lambda (tree) (sf "(") (ppx tree) (sf ")")))
  
  (define ppx
    (let* ((x-fmtr (make-pp-formatter))
	   (push-il (lambda () (fmtr 'push)))
	   (pop-il (lambda () (fmtr 'pop)))
	   (x-sf (lambda args (apply fmtr args)))
	   (sf-nl (lambda () (sf "\n")))
	   ;;(ppx/p (lambda (tree) (sf "(") (ppx tree) (sf ")")))
	   )

      (lambda (tree)
	(sxml-match tree

	  ((p-expr ,expr) (ppx expr))
	  ((ident ,name) (sf "~A" name))
	  ((char ,value) (sf "'~A'" (sx-ref tree 1)))
	  ((fixed ,value) (sf "~A" value))
	  ((float ,value) (sf "~A" value))
	  ((string ,value) (sf "~S" value))

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

	  ((lt ,lval ,rval) (binary 'lt " < " lval rval))
	  ((gt ,lval ,rval) (binary 'gt " > " lval rval))

	  ((le ,lval ,rval) (binary 'le " <= " lval rval))
	  ((ge ,lval ,rval) (binary 'ge " >= " lval rval))
	  ((eq ,lval ,rval) (binary 'eq " == " lval rval))
	  ((neq ,lval ,rval) (binary 'neq " != " lval rval))
	  
	  ((post-inc ,expr) (unary/r 'post-inc "++" expr))
	  ((post-dec ,expr) (unary/r 'post-dec "--" expr))
	  
	  ((assn-expr ,lval ,op ,rval)
	   (binary (car op) (simple-format #f " ~A " (cadr op)) lval rval))

	  ((decl . ,rest)
	   (let ((specs (sx-ref tree 1))
		 (initl (assq 'init-declr-list (cdr tree)))
		 (comm (assq 'comment (cdr tree))))
	     (ppx specs)
	     (if initl (ppx initl))
	     (sf "; ")
	     (if comm (ppx comm) (sf "\n"))))
	  ;; hack for: for (int i = 0; i < 10; i++) { ... }
	  ((decl-no-newline . ,rest)
	   (let ((specs (sx-ref tree 1))
		 (initl (assq 'init-declr-list (cdr tree)))
		 (comm (assq 'comment (cdr tree))))
	     (ppx specs)
	     (if initl (ppx initl))
	     (sf "; ")))

	  ;; TODO: udpate
	  ((decl-spec-list . ,rest)
	   (let iter ((dsl (sx-tail tree 1)))
	     (when (pair? dsl)
	       (case (sx-tag (car dsl))
		 ((stor-spec) (sf "~A" (car (sx-ref (car dsl) 1))))
		 ((type-qual) (sf "qual=~A" (sx-ref (car dsl) 1)))
		 ((type-spec) (ppx (car dsl)))
		 (else
		  (sf "[?:~S] " (car dsl))))
	       (if (pair? (cdr dsl)) (sf " "))
	       (iter (cdr dsl)))))

	  ;; TODO: udpate
	  ((init-declr-list . ,rest)
	   (pair-for-each
	    (lambda (pair)
	      (sf " ")
	      (ppx (car pair))
	      (if (pair? (cdr pair)) (sf ",")))
	    (sx-tail tree 1)))
	  ((comp-declr-list . ,rest)
	   (pair-for-each
	    (lambda (pair)
	      (sf " ")
	      (ppx (car pair))
	      (if (pair? (cdr pair)) (sf ",")))
	    (sx-tail tree 1)))

	  ;; TODO: udpate
	  ((init-declr . ,rest)
	   (let* ((declr (sx-ref tree 1))
		  (initr (sx-ref tree 2))
		  (iexpr (and initr (sx-ref initr 1))))
	     (ppx declr)
	     (when initr
	       (sf " = ")
	       (case (sx-tag iexpr)
		 ((initzer-list)
		  (sf "{")
		  (sf "initzer-list")
		  (sf " }"))
		 (else
		  (ppx iexpr))))))
	  ;; TODO: udpate
	  ((comp-declr . ,rest)
	   (let* ((declr (sx-ref tree 1))
		  (initr (sx-ref tree 2))
		  (iexpr (and initr (sx-ref initr 1))))
	     (ppx declr)
	     (when initr
	       (sf " = ")
	       (case (sx-tag iexpr)
		 ((initzer-list)
		  (sf "{")
		  (sf "initzer-list")
		  (sf " }"))
		 (else
		  (ppx iexpr))))))
	  ;; TODO: udpate
	  ((param-declr . ,rest)
	   (let* ((declr (sx-ref tree 1))
		  (initr (sx-ref tree 2))
		  (iexpr (and initr (sx-ref initr 1))))
	     (ppx declr)
	     (when initr
	       (sf " = ")
	       (case (sx-tag iexpr)
		 ((initzer-list)
		  (sf "{")
		  (sf "initzer-list")
		  (sf " }"))
		 (else
		  (ppx iexpr))))))

	  ;; TODO: udpate
	  ((type-spec . ,rest)
	   (let ((arg (sx-ref tree 1))) ;; did I mess this up?
	     (case (sx-tag arg)
	       ((fixed-type) (sf "~A" (sx-ref arg 1)))
	       ((float-type) (sf "~A" (sx-ref arg 1)))
	       ((struct-ref) (ppx arg))
	       ((struct-def) (ppx arg))
	       ((union-ref) (ppx arg))
	       ((union-def) (ppx arg))
	       ((enum-def) (ppx arg))
	       ((typename) (sf "~A" (sx-ref arg 1)))
	       (else (error "missing " arg)))))

	  ;; TODO: udpate
	  ((struct-ref . ,rest) (sf "struct ~A" (sx-ref (sx-ref tree 1) 1)))
	  ;; TODO: udpate
	  ((union-ref . ,rest) (sf "union ~A" (sx-ref (sx-ref tree 1) 1)))
	  
	  ;; TODO: udpate
	  ((struct-def . ,rest)
	   (let ((name (assq-ref tree 'ident))
		 (flds (assq-ref tree 'field-list)))
	     (if name
		 (sf "struct ~S {\n" name)
		 (sf "struct {\n"))
	     (push-il)
	     (pair-for-each
	      (lambda (pair)
		(case (caar pair)
		  ((comment) (ppx (car pair)))
		  ((comp-decl) (ppx (car pair)))
		  (else (error "pprint: fixup struct-def"))))
	      flds)
	     (pop-il)
	     (sf "}")))
	  ((union-def . ,rest)
	   (let ((name (assq-ref tree 'ident))
		 (flds (assq-ref tree 'field-list)))
	     (if name
		 (sf "struct ~S {\n" name)
		 (sf "struct {\n"))
	     (push-il)
	     (pair-for-each
	      (lambda (pair)
		(case (caar pair)
		  ((comment) (ppx (car pair)))
		  ((comp-decl) (ppx (car pair)))
		  (else (error "pprint: fixup struct-def"))))
	      flds)
	     (pop-il)
	     (sf "}")))

	  ;; TODO: udpate
	  ((comp-decl . ,rest)
	   (let ((specs (sx-ref tree 1))
		 (initl (assq 'comp-declr-list (cdr tree)))
		 (comm (assq 'comment (cdr tree))))
	     (ppx specs)
	     (if initl (ppx initl))
	     (sf "; ")
	     (if comm (ppx comm) (sf "\n"))))

	  ((enum-def (ident ,name) (enum-def-list . ,edl))
	   (sf "enum ~A " name) (ppx `(enum-def-list . ,edl)))

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

	  ((pointer (decl-spec-list . ,items))
	   (sf "*") (ppx `(decl-spec-list . ,items)))
	  ((pointer)
	   (sf "*"))
	  ((pointer (decl-spec-list . ,items) (pointer . ,rest))
	   (sf "*") (ppx `(decl-spec-list . ,items)) (ppx `(pointer . ,rest)))
	  ((pointer (pointer . ,rest))
	   (sf "*") (ppx `(pointer . ,rest)))

	  ((array-of ,dir-declr ,arg)
	   (ppx dir-declr) (sf "[") (ppx arg) (sf "]"))
	  ((array-of ,dir-declr)
	   (ppx dir-declr) (sf "[]"))
	  ;; MORE TO GO
	    
	  ((ftn-declr ,dir-declr ,param-type-l)
	   (ppx dir-declr) (sf "(") (ppx param-type-l) (sf ")"))
	  ((ftn-declr ,dir-declr)
	   (ppx dir-declr) (sf "()"))

	  ;; TODO: udpate
	  ((type-name . ,rest)
	   (let ((spec (sx-ref tree 1))
		 (abdr (and (<  2 (length tree)) (sx-ref tree 2))))
	     (if (not (eqv? (sx-tag spec) 'decl-spec-list))
		 (error "assuming decl-spec-list"))
	     (ppx spec)
	     (if abdr (ppx abdr))))

	  ;; TODO: udpate
	  ;; abs-declr
	  ((abs-declr . ,rest)
	   (let iter ((decls (sx-tail tree 1)))
	     (when (pair? decls)
	       (case (sx-tag (car decls))
		 ((pointer)
		  (sf "*"))
		 (else
		  (error "need to finish abs-declr")))
	       (iter (cdr decls)))))


	  ;; TODO: udpate
	  ((compd-stmt . ,rest)
	   (sf "{\n")
	   (push-il)
	   (for-each ppx (sx-tail (sx-ref tree 1) 1))
	   (pop-il)
	   (sf "}\n"))
	  ((compd-stmt-no-newline . ,rest)
	   (sf "{\n")
	   (push-il)
	   (for-each ppx (sx-tail (sx-ref tree 1) 1))
	   (pop-il)
	   (sf "} "))

	  ;; expression-statement
	  ((expr-stmt . ,rest)
	   (ppx (sx-ref tree 1))
	   (sf "; ")
	   ;; comment ?
	   (sf "\n"))
	  
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

	  ;; This does not meet the convention of "} while" on same line. 
	  ((do-while ,stmt ,expr)
	   (sf "do ")
	   (if (eqv? 'compd-stmt (sx-tag stmt)) 
	       (ppx (cons 'compd-stmt-no-newline (cdr stmt)))
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
	      (ppx (car pair))
	      ;; Generatwe a blank line if followed by a fctn-defn.
	      (if (and (pair? (cdr pair))
		       (eqv? (sx-tag (cadr pair)) 'fctn-defn))
		  (sf "\n")))
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

	  ((param-list . ,rest)
	   (pair-for-each
	    (lambda (pair)
	      (ppx (car pair))
	      (if (pair? (cdr pair)) (sf ", ")))
	    (sx-tail tree 1)))

	  ((param-decl . ,rest)
	   (let ((specs (sx-ref tree 1))
		 (declr (assq 'param-declr (cdr tree))))
	     (ppx specs)
	     (sf " ")
	     (ppx declr)))
	  
	  ((cpp-stmt . ,rest)
	   (cpp-ppx (sx-ref tree 1)))

	  ((extern-C-begin) (sf "extern \"C\" {\n"))
	  ((extern-C-end) (sf "}\n"))

	  (,otherwise
	   (simple-format #t "\n*** NOT HANDLED: ~S\n" (car tree)))
	  ))))
  
  (ppx tree))

;; --- last line ---
