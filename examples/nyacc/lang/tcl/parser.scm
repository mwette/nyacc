;; parser.scm

;; Copyright (C) 2018 Matthew R. Wette
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

(define-module (nyacc lang tcl parser)
  #:export (read-command
	    read-tcl-stmt
	    read-tcl-body
	    split-body tclme
	    cnvt-tcl-tree
	    cnvt-args
	    )
  )

(define (sf fmt . args) (apply simple-format #t fmt args))
(define (db fmt . args) #f)

(define (xunread-char char port)
  (sf "unread-char ~S\n" char)
  (unread-char char port))
(define (xread-char port)
  (let ((ch (read-char port)))
    (sf "read-char ~S\n" ch)
    ch))

(define (rls chl) (reverse-list->string chl))

;; bug? No char for eof.  Maybe use nul for that???
(define cs:ws (string->char-set " \t"))

(define cs:nl (string->char-set "\n"))
(define cs:nl+ws (string->char-set "\n" cs:ws))

;; command terminator
(define cs:ct (string->char-set ";\n"))
(define cs:ct+ws (string->char-set ";\n" cs:ws))

(define cs:rs (string->char-set "]"))	; right square
(define cs:rs+ws (string->char-set "]" cs:ws))

(define cs:rc (string->char-set ")"))	; right curve

(define cs:lcurly (string->char-set "{"))
(define cs:rcurly (string->char-set "}"))
(define cs:dquote (string->char-set "\""))

(define cs:lix (string->char-set "(" cs:rs+ws))
(define cs:rix (string->char-set ")"))

(define (read-command port)
  (letrec
      ((report-error
	(lambda (fmt . args)
	  (let ((fn (or (port-filename port) "(unknown)"))
		(ln (1+ (port-line port))))
	    (apply simple-format (current-error-port) fmt args))
	  (throw 'tcl-error)))

       (swallow
	(lambda (val)
	  (db "swallow ")
	  (xread-char port) val))

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
	      (xread-char port)
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
	      (if (pair? wordl) (cons 'command (reverse wordl)) ch))

	     ((char-set-contains? end-cs ch)
	      (db "C: done\n")
	      (cons 'command (reverse wordl)))
	     
	     ((char-set-contains? cs:ws ch)
	      (xread-char port)
	      (loop wordl (peek-char port)))

	     ((char=? #\" ch)
	      (xread-char port)
	      (let ((word (read-word cs:dquote)))
		(db "C: \" .. word=~S lach=~S\n" word (peek-char port))
		(xread-char port) ;; "
		(db "C: \" .. word=~S lach=~S\n" word (peek-char port))
		(loop (foldin word wordl) (peek-char port))))
	     
	     ((char=? #\{ ch)
	      (xread-char port) ;; {
	      (loop (foldin (read-brace) wordl) (peek-char port)))

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
	  (unless (eof-object? ch) (xunread-char ch port))
	  (case tag ((string) (rls chl)) (else (list tag (rls chl))))))

       (read-brace
	(lambda ()
	  (let loop ((chl '()) (bl 1) (ch (xread-char port)))
	    (sf "B:       ch=~S bl=~S chl=~S\n" ch bl chl)
	    (cond
	     ((eof-object? ch) (error "no end brace"))
	     ((char=? ch #\})
	      (if (= 1 bl)
		  (list 'string (rls chl))
		  (loop (cons ch chl) (1- bl) (xread-char port))))
	     ((char=? ch #\{)
	      (loop (cons ch chl) (1+ bl) (xread-char port)))
	     (else
	      (loop (cons ch chl) bl (xread-char port)))))))
       
       (read-frag
	(lambda (end-cs)
	  (sf "\n")
	  (let loop ((tag #f)
		     (chl '())
		     (bl (if (eq? end-cs cs:rcurly) 1 0))
		     (ch (xread-char port)))
	    (sf "F:     tag=~S ch=~S chl=~S bl=~S end-cs=~S\n"
		tag ch chl bl end-cs)
	    (cond
	     ((eof-object? ch)
	      (if (positive? bl) (error "missing end-brace"))
	      (if (pair? chl) (finish tag chl ch) end-cs))
	     ((and (zero? bl) (char-set-contains? end-cs ch))
	      (db "F:     done\n")
	      (if tag (finish tag chl ch)
		  (begin (xunread-char ch port) end-cs)))
	     ((and (char=? ch #\\) (zero? bl)) ;; a b f n r t v ; xhh ...
	      (let* ((c2 (xread-char port))
		     (c1 (case c2
			   ((#\a) #\alarm) ((#\b) #\backspace);((#\f) #\formfeed)
			   ((#\n) #\newline) ((#\r) #\return) ((#\t) #\tab)
			   (else c2))))

		(loop tag (cons c2 chl) bl (xread-char port))))
	     ((char=? ch #\$)
	      (cond
	       (tag (finish tag chl ch))
	       (else
		(let* ((frag (read-frag cs:lix))
		       (ch1 (peek-char port))
		       (indx (cond ((eof-object? ch1) #f)
				   ((char=? ch1 #\() #t)
				   (else #f)))
		       )
		  (if indx
		      `(deref ,frag ,(read-index))
		      `(deref ,frag))))))
	     ((char=? ch #\{) (read-brace))
	     ((char=? ch #\[) (swallow (read-cmmd cs:rs)))
	     ((char=? ch #\") (swallow (read-frag cs:dquote)))
	     ((not tag) (loop 'string (cons ch chl) bl (xread-char port)))
	     (else (loop tag (cons ch chl) bl (xread-char port)))))))

       (read-index
	(lambda ()
	  (if (not (char=? #\( (xread-char port))) (error "coding errro"))
	  (let ((word (read-word cs:rc)))
	    (if (not (char=? #\) (xread-char port))) (error "coding errro"))
	    word)))

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

;; For strings which are known to be interpreted as bodies,
;; split them into a sequence of commands.
(define (split-body body)
  (cons
   'body
   (call-with-input-string body
     (lambda (port)
       (let loop ((cmd (read-command port)))
	 (cond
	  ((eof-object? cmd) '())
	  ((null? (cdr cmd)) (loop (read-command port)))
	  (else (cons cmd (loop (read-command port))))))))))

(use-modules ((sxml fold) #:select (foldt)))
(use-modules (nyacc lang sx-util))

;; args string => (arg-list (arg "abc") (arg "abc" "123") ((args)))
(define (cnvt-args astr)
  (with-input-from-string astr
    (lambda ()
      (define (unread-chl chl)
	(cond
	 ((null? chl))
	 ((eof-object? (car chl)))
	 (else (unread-char (car chl)) (unread-chl (cdr chl)))))
      (define (skip-ws ch)
	(if (char-set-contains? cs:ws ch) (skip-ws (read-char)) ch))
      (define (read-non-ws ch)
	(let loop ((chl '()) (ch ch))
	  (cond
	   ((eof-object? ch) (rls chl))
	   ((char-set-contains? cs:ws ch) (rls chl))
	   (else (loop (cons ch chl) (read-char))))))
      (define (read-args-kw)
	(let loop ((kw '(#\a #\r #\g #\s)) (chl '()) (ch (read-char)))
	  (sf "rak chl=~S ch=~S\n" chl ch)
	  (cond
	   ((null? (car kw)) #t)
	   ((eof-object? ch) (unread-chl (cons ch chl)) #f)
	   ((char=? (car kw) ch) (loop (cdr kw) (cons (car kw) chl) (read-char)))
	   (else (unread-chl (cons ch chl)) #f))))
      (define (read-pair)
	(read-char)
	(let loop ((arg #f) (chl '()) (ch (read-char)))
	  (cond
	   ((eof-object? ch) (unread-char ch) #f)
	   ((char=? ch #\}) `(opt-arg ,arg ,(rls chl)))
	   ((char-set-contains? cs:ws ch)
	    (if arg
		(if (pair? chl)
		    (loop arg (cons ch chl) (read-char))
		    (loop arg chl (read-char)))
		(loop (rls chl) '() (read-char))))
	   (else (loop arg (cons ch chl) (read-char))))))
      (cons
       'arg-list
       (let loop ((ch (peek-char)))
	 (sf "\npeek ~S\n" ch)
	 (cond
	  ((eof-object? ch) '())
	  ((and (sf "1\n") #f))
	  ((char-set-contains? cs:ws ch)
	   (loop (read-char)))
	  ((and (sf "2\n") #f))
	  ((char=? ch #\{)
	   (cons (read-pair) (loop (read-char))))
	  ((and (sf "3\n") #f))
	  ((read-args-kw)
	   (cons '(args) (loop (read-char))))
	  ((and (sf "4\n") #f))
	  (else
	   (let ((argval (read-non-ws (read-char))))
	     (cons `(arg ,argval) (loop (read-char)))))))))))
    
;; should be converted to a chl (see nyacc lexer)
(define reserved-proc-names
  '("expr" "if" "proc" "set" "while"))

;; proc if while expr
(define (cnvt-tcl-tree tree)
  (case (sx-tag tree)
    ((command)
     (let ((cmd (sx-ref tree 1)))
       (cond
	((string=? "proc") ;; proc name args body
	 `(proc (sx-ref tree 1) xxx))
	(else
	 tree))))
    ((word)
     tree)
    (else
     tree)))

(define (read-tcl-stmt port env) (read-command port))

(use-modules (ice-9 pretty-print))
(define pp pretty-print)

(define (try str)
  (let ((res (call-with-input-string str read-command)))
    (sf "\n~S =>\n" str)
    (pp res)))

(define (tclme)
  (for-each
   try
   (list
    ;;"abc def ghi"
    ;;"abc \"def ghi\""
    ;;"abc [def ghi] "
    ;;"abc [def ghi] lkj"
    ;;"abc [def $ghi] lkj"
    ;;"abc def$ghi"
    ;;"abc def$ghi$jkl"
    ;;"abc [def ${ghi}] lkj"
    ;;"abc [def ${ghi jkl}] mno"
    "abc {a {b 1} c}"
    ;; later
    ;;"abc ${def}(ghi)"
    )))

#|
(define (tclme)
  (pp (split-body "
abc def ghi
abc \"def ghi\"
abc [def ghi] 
abc [def ghi] lkj
abc [def $ghi] lkj
abc def$ghi
abc def$ghi$jkl
abc [def ${ghi}] lkj
abc [def ${ghi jkl}] mno
")))

(define (read-tcl-body port)
  `(body
    (let loop ((cmd (read-command port)))
      (if (eof-object? cmd) '()
	  (cons cmd (loop (read-command port)))))))


;;|#

;; --- last line ---
