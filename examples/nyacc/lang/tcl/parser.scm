;;; nyacc/lang/tcl/parser.scm - parse tcl code

;; Copyright (C) 2018,2020 Matthew R. Wette
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

;; TODO: be more agressive to parse 123 as fixed and 567.0 as float.
;; We can convert to string later.

;; args string => (arg-list (arg "abc") (opt-arg "def" "123") (rest "args"))
;; @table asis
;; @item @code{arg} @emph{var}
;; @item @code{opt-arg} @emph{var} @emph{val}
;; @item @code{rest "args"}
;; @end table

;;; Code:

(define-module (nyacc lang tcl parser)
  #:export (read-command
	    read-tcl-stmt
	    read-tcl-file
	    ;; debugging:
	    split-body
	    cnvt-tree
	    cnvt-args
	    splice-xtail
	    tclme
	    )
  #:use-module ((nyacc lex) #:select (read-basic-num cnumstr->scm c:ir))
  #:use-module (nyacc lang sx-util)
  #:use-module (sxml match)
  #:use-module ((srfi srfi-1) #:select (fold-right))
  #:use-module (ice-9 match)
  )

(use-modules (ice-9 pretty-print))
(define pp pretty-print)
(define (sf fmt . args) (apply simple-format #t fmt args))
(define (zf fmt . args) #f)
(define db zf)

(define (echo obj)
  (sf " ~S\n" obj)
  obj)

(define char-hex?
  (let ((cs (string->char-set "0123456789abcdefABCDEF")))
    (lambda (ch)
      (char-set-contains? cs ch))))

(define char-oct? 
  (let ((cs (string->char-set "01234567")))
    (lambda (ch)
      (char-set-contains? cs ch))))

(define (rls chl) (reverse-list->string chl))

;; bug? No char for eof.  Maybe use nul for that???
(define cs:ws (string->char-set " \t"))

(define cs:nl (string->char-set "\n"))
(define cs:nl+ws (string->char-set "\n" cs:ws))

;; command terminator
(define cs:ct (string->char-set "];\n"))
(define cs:ct+ws (string->char-set "];\n" cs:ws))

;; right square
(define cs:rs (string->char-set "]"))
(define cs:rs+ws (string->char-set "]" cs:ws))

;; left paren
(define cs:lparen (string->char-set "("))
;; right paren
(define cs:rparen (string->char-set ")"))

;; left curly brace
(define cs:lcurly (string->char-set "{"))
;; right curly brace
(define cs:rcurly (string->char-set "}"))
;; double quote
(define cs:dquote (string->char-set "\""))

;; variable terminator
;;(define cs:vt (string->char-set "()+-*/%&|!^<>?" cs:ct+ws))
(define cs:vt (string->char-set "()+-*/%&|!^<>?\"" cs:ct+ws))

(define (report-error fmt . args)
  (let* ((port (current-input-port))
	 (fn (or (port-filename port) "(unknown)"))
	 (ln (1+ (port-line port))))
    (apply simple-format (current-error-port) fmt args))
  (throw 'tcl-error))

(define noop-command '(command (string "NOOP")))

(define (read-command port)
  ;; This is a bit of a hack job.
  (letrec
      ((error
	(lambda (fmt . args)
	  (with-input-from-port port
	    (lambda () (apply report-error port fmt args)))))
       
       (foldin
	(lambda (word word-list)
	  (cons (if (and (pair? (cdr word)) (null? (cddr word)))
		    (cadr word)
		    word)
		word-list)))

       (skip-ws
	(lambda (port)
	  (let loop ((ch (peek-char port)))
	    (cond
	     ((eof-object? ch) ch)
	     ((char-set-contains? cs:ws ch)
	      (read-char port)
	      (loop (peek-char port)))
	     (else ch)))))

       (read-cmmd
	(lambda (end-cs)
	  (db "C: read-cmmd end-cs=~S\n" end-cs)
	  (let loop ((wordl '()) (ch (skip-ws port)))
	    (db "C: wordl=~S ch=~S\n" wordl ch)
	    (cond
	     ((eof-object? ch)
	      (db "C: done\n")
	      (if (pair? wordl)
		  (cons 'command (reverse wordl))
		  ch))

	     ((char=? ch #\#)
	      (read-char port)
	      `(comment
		,(reverse-list->string
		  (let lp ((chl '()) (ch (read-char port)))
		    (cond
		     ((eof-object? ch) chl)
		     ((char=? ch #\newline) (unread-char ch port) chl)
		     (else (lp (cons ch chl) (read-char port))))))))

	     ((char-set-contains? end-cs ch)
	      (db "C: done\n")
	      (if (pair? wordl)
		  `(command ,@(reverse wordl))
		  noop-command))
	     
	     ((char-set-contains? cs:ws ch)
	      (read-char port)
	      (loop wordl (peek-char port)))

	     ((char=? #\" ch)
	      (read-char port)
	      (db "C: \" .. reading word\n")
	      (let ((word (read-word cs:dquote)))
		(db "C: \" .. word=~S lach=~S\n" word (peek-char port))
		(read-char port) ;; "
		(db "C: \" .. word=~S lach=~S\n" word (peek-char port))
		(loop (foldin word wordl) (peek-char port))))
	     
	     ((char=? #\{ ch)
	      (read-char port) ;; {
	      (loop (cons (read-brace) wordl) (peek-char port)))

	     ;;((eq? end-cs cs:nl)
	     ((eq? end-cs cs:ct)
	      ;;(let ((word (read-word cs:nl+ws)))
	      (let ((word (read-word cs:ct+ws)))
		(loop (foldin word wordl) (peek-char port))))
	     
	     ((eq? end-cs cs:rs)
	      (let ((word (read-word cs:rs+ws)))
		(loop (foldin word wordl) (peek-char port))))

	     (else (error "coding error"))))))

       (read-word
	(lambda (end-cs)
	  (let loop ((fragl '()) (frag (read-frag end-cs)))
	    (db "W:   fragl=~S frag=~S\n" fragl frag)
	    (cond
	     ((eq? frag end-cs)
	      (db "W:   done\n")
	      (cons 'word (reverse fragl)))
	     (else
	      (loop (cons frag fragl) (read-frag end-cs)))))))

       (finish
	(lambda (tag chl ch)
	  (db "F:     finish ~S ~S ~S\n" tag chl ch)
	  (unless (eof-object? ch) (unread-char ch port))
	  ;;(case tag ((string) (rls chl)) (else (list tag (rls chl))))))
	  (list tag (rls chl))))

       (swallow
	(lambda (val)
	  (db "swallow ")
	  (read-char port) val))

       (read-brace
	(lambda ()
	  (let loop ((chl '()) (bl 1) (ch (read-char port)))
	    (db "B:       ch=~S bl=~S chl=~S\n" ch bl chl)
	    (cond
	     ((eof-object? ch) (error "no end brace"))
	     ((char=? ch #\})
	      (if (= 1 bl)
		  (list 'string (rls chl))
		  (loop (cons ch chl) (1- bl) (read-char port))))
	     ((char=? ch #\{)
	      (loop (cons ch chl) (1+ bl) (read-char port)))
	     (else
	      (loop (cons ch chl) bl (read-char port)))))))
       
       (read-index
	(lambda ()
	  ;;(if (not (char=? #\( (read-char port))) (error "coding error"))
	  (if (char=? (peek-char port) #\() (read-char port))
	  (let ((word (read-word cs:rparen)))
	    ;;(sf "index word=~S\n" word)
	    (if (not (char=? #\) (read-char port))) (error "coding error"))
	    `(index . ,(foldin word '())))))
       
       (read-escape
	;; a b f n r t v ; xhh ...
	(lambda ()
	  (let* ((ch (read-char port)))
	    (case ch
	      ((#\a) #\alarm) ((#\b) #\backspace)
	      ;;((#\f) #\formfeed)
	      ((#\n) #\newline) ((#\r) #\return) ((#\t) #\tab)
	      (else ch)))))

       (read-vref
	(lambda ()
	  (let* ((frag (read-frag cs:vt))
		 (frag (sx-ref frag 1)) ; extract string value
		 (ch1 (peek-char port))
		 (indx (cond ((eof-object? ch1) #f)
			     ((char=? ch1 #\() #t)
			     (else #f))))
	    (db "$frag=~S ch1=~S\n" frag ch1)
	    (if indx
		      `(deref-indexed ,frag ,(cadr (read-index)))
		      `(deref ,frag)))))

       (init-blev			; initial brace level
	(lambda (end-cs) (if (eq? end-cs cs:rcurly) 1 0)))
       
       (read-frag
	(lambda (end-cs)
	  (db "\n== read frag ~S\n" end-cs)
	  (let loop ((tag #f)		     ; tag: string etc
		     (chl '())		     ; list of chars read
		     (bl (init-blev end-cs)) ; brace level
		     (ch (read-char port))) ; next char
	    (db "F:     tag=~S ch=~S chl=~S bl=~S\n" tag ch chl bl)
	    ;;(db "F:     tag=~S ch=~S chl=~S bl=~S end-cs=~S\n"
		;;tag ch chl bl end-cs)
	    (cond
	     ((eof-object? ch)
	      (if (positive? bl) (error "missing end-brace"))
	      (if (pair? chl) (finish tag chl ch) end-cs))
	     ((and (zero? bl) (char-set-contains? end-cs ch))
	      (db "F:     done\n")
	      (if tag
		  (finish tag chl ch)
		  (begin (unread-char ch port) end-cs)))
	     ((and (char=? ch #\\) (zero? bl))
	      (let ((ch (read-escape)))
		(loop (or tag 'string) (cons ch chl) bl (read-char port))))
	     ((char=? ch #\$)
 	      (if tag (finish tag chl ch) (read-vref)))
	     ((char=? ch #\{)
	      (read-brace))
	     ((char=? ch #\()
	      (if tag (finish tag chl ch) (read-index)))
	     ((char=? ch #\[)
	      (if tag (finish tag chl ch))
	      (swallow (read-cmmd cs:rs)))
	     ((char=? ch #\")
	      ;;(swallow (read-frag cs:dquote))
	      (loop 'string chl bl (read-char port))
	      )
	     (else
	      (loop (or tag 'string) (cons ch chl) bl (read-char port)))))))
       )

    ;; wrap w/ catch
    ;;(read-cmmd cs:nl)
    (let ((cmd (read-cmmd cs:ct))
	  (nxt (peek-char port)))
      (cond
       ((eof-object? nxt))
       ((char-set-contains? cs:ct nxt) (read-char port)))
      cmd)
    ;; or (swallow (read-cmmd cs:ct))
    ))

;; @deffn {Procedure} split-body body
;; For the string @var{body} which is known to be interpreted as a sequence
;; of commands, split the string into a sequence of commands inside a
;; @code{(body ...)} element.
;; @end deffn
(define (split-body body)
  (cons
   'body
   (call-with-input-string body
     (lambda (port)
       (let loop ((cmd (read-command port)))
	 (cond
	  ((eof-object? cmd) '())
	  ((null? (cdr cmd)) (loop (read-command port)))
	  ((eq? cmd noop-command) (loop (read-command port)))
	  (else (cons cmd (loop (read-command port))))))))))

(define (fix-expr-string xstr)
  (let* ((cmmd (call-with-input-string (string-append "expr " xstr)
		(lambda (port) (read-command port))))
	 (tail (sx-tail cmmd 2))
	 (tail (splice-xtail tail)))
    `(expr . ,tail)))

;; convert all words in an expr command to a single list of frags
(define (splice-xtail tail)
  (let* ((blank `(string " "))
	 (terms (fold-right
		 (lambda (word terms)
		   (if (eq? 'word (sx-tag word))
		       (append (list blank) (cdr word) terms)
		       (cons* blank word terms)))
		 '() tail))
	 (toks (fold-right
		(lambda (term toks)
		  (if (string? term)
		      (cons term toks)
		      (if (eq? 'command (sx-tag term))
			  (cons term toks)
			  (cons term toks))))
		'() (cdr terms))))
    ;;(sf "sxt ~S => ~S\n" tail toks)
    toks))

;; @deffn {Procedure} cnvt-args astr
;; Given the argument string @var{astr} for a procedure definition,
;; convert to an sxml tree (@code{arg-list}) with child elements with
;; tags @code{arg} (required), @code{opt-arg} (optional with default),
;; or @code{rest}.
;; @end deffn
(define (cnvt-args astr)
  (with-input-from-string astr
    (lambda ()
      (define (unread-chl-1 chl)
	(cond
	 ((null? chl))
	 ((null? (cdr chl)))
	 ((eof-object? (car chl)))
	 (else (unread-char (car chl)) (unread-chl-1 (cdr chl)))))
      (define (skip-ws ch)
	(if (char-set-contains? cs:ws ch) (skip-ws (read-char)) ch))
      (define (read-non-ws ch)
	(let loop ((chl '()) (ch ch))
	  (cond
	   ((eof-object? ch) (rls chl))
	   ((char-set-contains? cs:ws ch) (rls chl))
	   (else (loop (cons ch chl) (read-char))))))
      (define (read-pair ch)
	(if (not (char=? ch #\{)) #f
	    (let loop ((arg #f) (chl '()) (ch (read-char)))
	      (cond
	       ((eof-object? ch) (report-error "missing right brace"))
	       ((char=? ch #\}) `(opt-arg ,arg (string ,(rls chl))))
	       ((char-set-contains? cs:ws ch)
		(if arg
		    (if (pair? chl)
			(loop arg (cons ch chl) (read-char))
			(loop arg chl (read-char)))
		    (loop (rls chl) '() (read-char))))
	       (else (loop arg (cons ch chl) (read-char)))))))
      (define (read-args-kw ch)
	(let loop ((kw '(#\a #\r #\g #\s)) (chl '()) (ch ch))
	  (cond
	   ((null? kw) #t)
	   ((eof-object? ch) (unread-chl-1 chl) #f)
	   ((char=? ch (car kw))
	    (loop (cdr kw) (cons (car kw) chl) (read-char)))
	   (else (unread-chl-1 (cons ch chl)) #f))))
      (cons
       'arg-list
       (let loop ((ch (read-char)))
	 (cond
	  ((eof-object? ch) '())
	  ((char-set-contains? cs:ws ch) (loop (read-char)))
	  ((char=? ch #\{) (cons (read-pair ch) (loop (read-char))))
	  ((read-args-kw ch) (cons '(rest "args") (loop (read-char))))
	  (else (let ((argval (read-non-ws ch)))
		  (cons `(arg ,argval) (loop (read-char)))))))))))

;; @deffn {Procedure} num-string cstr => #f|(number ,value)
;; Given a string return a numeric sxml element like @code{(number "123")}
;; @code{#f} if not a number.  Leading sign is accepted.
;; @end deffn
(define (num-string cstr)
  (and=>
   (with-input-from-string cstr
     (lambda ()
       (let ((val (read-basic-num (read-char) #:signed #t)))
	 (and (eof-object? (read-char)) val))))
   (lambda (pair) `(number ,(cdr pair)))))

;; @deffn {Procedure} vec-string str
;; Given a string return an array of numeric elements like
;; @code{(float-vec "123.0" "456.0")} or @code{#f} if not.
;; @end deffn
;; needs regex impl
(define (vec-string cstr)
  (fold-right
   (lambda (str seed) (and seed (cons (num-string str) seed)))
   '()
   (string-split cstr #\space)))

;; ((string "elseif") cond-part body-part . rest)
;; ((string "else") body-part)
(define (cnvt-cond-tail ctail)
  (db "ctail=~S\n" ctail)
  (match ctail
    ('() '())
    (`((string "else") (string ,body-part))
     `((else ,(split-body body-part))))
    (`((string "elseif") (string ,cnd) (string "then")
       (string ,body-part) . ,rest)
     (cons
      `(elseif ,(fix-expr-string cnd) ,(split-body body-part))
      (cnvt-cond-tail (list-tail ctail 3))))
    (`((string "elseif") (string ,cnd) (string ,body-part) . ,rest)
     (cons
      `(elseif ,(fix-expr-string cnd) ,(split-body body-part))
      (cnvt-cond-tail (list-tail ctail 3))))
    (_
     ;;(sf "ctail: @ ~S\n" (port-line (current-input-port)))
     ;;(pp ctail) (quit)
     (throw 'tcl-error "syntax error"))))
    

;; This is an alist of commands which are translated into pre-compiled
;; constructs.  For example, while, for, proc, if.
;; The lambdas in  here should NEVER emit a command element, otherwise
;; infinite loop will happen
(define tcl-res-cmds
  `(("NOOP"
     . ,(lambda (tree)
	  `(void)))
    ("array"
     . ,(lambda (tree)
	  (let ((cmmd (sx-ref* tree 2 1))
		(name (sx-ref* tree 3 1))
		(rest (sx-tail tree 4)))
	    (if (string=? cmmd "set")
		`(array-set ,name ,@rest)
		`(array ,name ,@rest)))))
    ("break"
     . ,(lambda (tree) `(break)))
    ("continue"
     . ,(lambda (tree) `(continue)))
    ("expr"
     . ,(lambda (tree) `(expr . ,(splice-xtail (sx-tail tree 2)))))
    ("format"
     . ,(lambda (tree) `(format . ,(sx-tail tree 2))))
    ("if"
     . ,(lambda (tree)
	  ;;(sf "tree:\n") (pp tree) ;;(quit)
	  ;;(sf "============\n")
	  ;;(pp
	  (sxml-match tree
	    ((command (string "if") (string ,cnd) (string "then")
		      (string ,bdy) . ,rest)
	     ;;(sf "GOT IT\n") (quit)
	     `(if ,(fix-expr-string cnd) ,(split-body bdy)
		  . ,(cnvt-cond-tail rest)))
	    ((command (string "if") (string ,cnd) (string ,bdy) . ,rest)
	     `(if ,(fix-expr-string cnd) ,(split-body bdy)
		  . ,(cnvt-cond-tail rest)))
	    (,_ (report-error "usage: if cond then else")))
	  ;;) (quit)
	  ))
    ("incr"
     . ,(lambda (tree) `(incr ,@(sx-tail tree 2))))
    ("proc"
     ;; This assumes default arguments are strings constants.
     . ,(lambda (tree)
	  (sxml-match tree
	    ((command (string "proc") (string ,name) (string ,args)
		      (string ,body))
	     `(proc ,name ,(cnvt-args args) ,(split-body body)))
	    (,_ (report-error "usage: proc name args body")))))
    ("return"
     . ,(lambda (tree)
	  `(return ,(sx-ref tree 2))))
    ("set"
     . ,(lambda (tree)
	  (sxml-match tree
	    ((command (string "set") (string ,name) . ,rest)
	     (if (pair? rest)
		 `(set . ,(sx-tail tree 2))
		 `(deref ,name)))
	    ((command (string "set") (word (string ,name) (index ,indx)) . ,rest)
	     (if (pair? rest)
		 `(set-indexed (string ,name) (word ,indx) . ,rest)
		 `(deref-indexed ,name (word ,indx))))
	    ;;(,_ (report-error "can't handle this yet")))))
	    (,_ `(set . ,(sx-tail tree 2))))))
    ("while"
     . ,(lambda (tree)
	  (sxml-match tree
	    ((command (string "while") ,cond (string ,body))
	     `(while (expr . ,(splice-xtail (list cond)))
		,(split-body body)))
	    (,_ (report-error "usage: while cond body")))))
    ;;("array" . #f)
    ;;("list" . #f)
    ;; === string-> Scheme for calling Scheme functions
    ;;("number" . #f)
    ;;("integer" . #f)
    ))

;; @deffn {Procedure} nxify-command form => form
;; Convert commands like @code{while} or @code{proc} to
;; the associated nx-tcl form.
;; @example
;; (nxify-command '(command "if" "1" "set x 3")) =>
;;   (if (expr "1") (body "set x 3")))
;; @end example
;; If no match if found, returns @code{#f}.
;; @noindent
;; Further processing should be executed on the body.
;; @end deffn
(define (nxify-command tree)
  (let ((repl (and=> (assoc-ref tcl-res-cmds (sx-ref* tree 1 1))
		     (lambda (proc) (proc tree)))))
    (if (and (pair? repl) (eq? 'command (car repl))) (error "coding error"))
    repl))
(export nxify-command)

 ;; convert special commands and words into nx-tcl syntax
;; @example
;; (cnvt-tcl '(command (string "expr") ...)) => (expr ...)
;; @end example
(define (cnvt-tree tree)
  ;;(sf "tree=~S\n" tree)
  (letrec
      ((cnvt-elt
	(lambda (tree)
	  (if (string? tree) tree
	      (sxml-match tree
		((command . ,rest)
		 (or (and=> (nxify-command tree) cnvt-tree)
		     (let* ((tail0 (sx-tail tree))
			    (tail1 (cnvt-tail tail0)))
		       (if (eq? tail1 tail0) tree `(command . ,tail1)))))
		((string ,val)
		 (or (num-string val) tree))
		((word (string ,val))
		 (cnvt-tree (sx-ref tree 1)))
		((word . ,rest)
		 (let* ((tail0 (sx-tail tree))
			(tail1 (cnvt-tail tail0)))
		   (if (eq? tail1 tail0) tree `(word . ,tail1))))
		(,_
		 (let* ((tag (sx-tag tree))
			(tail0 (sx-tail tree))
			(tail1 (cnvt-tail tail0)))
		   ;;(sf "cnvt _ tag=~S\n" tag)
		   ;;(sf "     tail0=~S tail1=~S\n" tail0 tail1)
		   (if (eq? tail1 tail0) tree (cons tag tail1))))))))
       (cnvt-tail
	(lambda (tail)
	  (if (null? tail) tail
	      (let* ((head0 (car tail)) (head1 (cnvt-elt head0))
		     (tail0 (cdr tail)) (tail1 (cnvt-tail tail0)))
		;;(sf "cnvt-tail ~S ~S\n" head0 tail0)
		;;(sf "cnvt-tail ~S ~S\n" head1 tail1)
		(if (eq? head1 head0)
		    (if (eq? tail1 tail0)
			tail
			(cons head0 tail1))
		    (if (eq? tail1 tail0)
			(cons head1 tail0)
			(cons head1 tail1))))))))
    (cnvt-elt tree)))

;; @deffn {Procedure} read-tcl-stmt port env
;; Guile extension language routine to read a single statement.
;; @end deffn
(define (read-tcl-stmt port env)
  "- Procedure: read-tcl-stmt port env
     Guile extension language routine to read a single statement."
  (let* ((cmmd0 (read-command port))
	 (cmmd1 (cnvt-tree cmmd0)))
    ;;(sf "s:cmd1:\n ") (pp cmmd0)
    ;;(sf "s:cmd2:\n ") (pp cmmd1)
    cmmd1))

;; @deffn {Procedure} read-tcl-file port env
;; Read a Tcl file.  Return a SXML tree.
;; @end deffn
(define (read-tcl-file port env)
  "- Procedure: read-tcl-file port env
     Read a Tcl file.  Return a SXML tree."
  (if (eof-object? (peek-char port))
      (read-char port)
      (cons
       'unit
       (let loop ((cmd (read-command port)))
	 (if (eof-object? cmd) '()
	     (let ((cmd1 cmd) (cmd2 (cnvt-tree cmd)))
	       ;;(sf "cmd1:\n") (pp cmd1)
	       ;;(sf "cmd2::n") (pp cmd2)
	       (cons (cnvt-tree cmd) (loop (read-command port))))))))
  )


;; --- last line ---
