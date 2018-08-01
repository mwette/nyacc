;; set up the reader

(define (sf fmt . args) (apply simple-format #t fmt args))

(define (read-command port)

  (define cs:ws (string->char-set " \t"))

  (define cs:nl (string->char-set "\n"))
  (define cs:nl+ws (string->char-set "\n" cs:ws))

  (define cs:rs (string->char-set "]"))
  (define cs:rs+ws (string->char-set "]" cs:ws))

  (define cs:lcurly (string->char-set "{"))
  (define cs:rcurly (string->char-set "}"))
  (define cs:dquote (string->char-set "\""))

  (define (rls chl) (reverse-list->string chl))

  (letrec
      ((swallow
	(lambda (val) (read-char port) val))

       (foldin
	(lambda (word word-list)
	  (cons (if (null? (cddr word)) (cadr word) word) word-list)))
	  ;;(cons word word-list)))

       (next-non-ws
	(lambda (port)
	  (let ((ch (read-char port)))
	    (if (eof-object? ch) ch
		(let iter ((ch ch))
		  (if (not (char-set-contains? cs:ws ch)) ch
		      (iter (read-char port))))))))

       (read-cmmd
	(lambda (end-cs)
	  (sf "C: read-cmmd\n")
	  (let iter ((wordl '()) (ch (next-non-ws port)))
	    (sf "C: wordl=~S ch=~S\n" wordl ch)
	    (cond
	     ((eof-object? ch)
	      (sf "C: done\n")
	      (cons 'command (reverse wordl)))
	     ((char-set-contains? cs:ws ch)
	      (iter wordl (read-char port)))

	     ((char-set-contains? end-cs ch)
	      (sf "C: done\n")
	      (cons 'command (reverse wordl)))
	     
	     ((char=? #\" ch)
	      (let ((word (read-word cs:dquote)))
		(read-char port)
		(iter (foldin word wordl) (peek-char port))))
	     ((char=? #\{ ch)
	      (let ((word (read-word cs:rcurly)))
		(read-char port)
		(iter (foldin word wordl) (peek-char port))))

	     ((eq? end-cs cs:nl)
	      (unread-char ch port)
	      (let ((word (read-word cs:nl+ws)))
		(iter (foldin word wordl) (peek-char port))))
	     ((eq? end-cs cs:rs)
	      (unread-char ch port)
	      (let ((word (read-word cs:rs+ws)))
		(iter (foldin word wordl) (peek-char port))))

	     (else (error "coding error"))))))

       (read-word
	(lambda (end-cs)
	  (let iter ((fragl '()) (frag (read-frag end-cs)))
	    (sf "W:   fragl=~S frag=~S\n" fragl frag)
	    (cond
	     ((eq? frag end-cs)
	      (sf "W:   done\n")
	      (cons 'word (reverse fragl)))
	     (else
	      (iter (cons frag fragl) (read-frag end-cs)))))))

       (finish
	(lambda (tag chl ch)
	  (unless (eof-object? ch) (unread-char ch port))
	  (case tag ((string) (rls chl)) (else (list tag (rls chl))))))
	  
       (read-frag
	(lambda (end-cs)
	  (let iter ((tag #f) (chl '()) (ch (read-char port)))
	    ;;(sf "F:     ch=~S chl=~S\n" ch chl)
	    (cond
	     ((eof-object? ch)
	      (cond
	       ((pair? chl) (finish tag chl ch))
	       (else end-cs)))
	     ((char=? ch #\$)
	      (if tag
		  (finish tag chl ch)
		  (iter 'deref chl (read-char port))))
	     ((char-set-contains? end-cs ch)
	      ;;(sf "F:     done\n")
	      (cond
	       ((pair? chl) (finish tag chl ch))
	       (else end-cs)))
	     ((char=? ch #\\)
	      (let* ((c2 (read-char port))
		     (c1 (case c2
			   ((#\t) #\tab)
			   (else c2))))
		(iter tag (cons c2 chl) (read-char port))))
	     ((char=? ch #\{) (swallow `(brace ,(read-frag cs:rcurly))))
	     ((char=? ch #\") (swallow (read-frag cs:dquote)))
	     ((char=? ch #\$) `(deref ,(read-frag end-cs)))
	     ((char=? ch #\[) (read-cmmd cs:rs))
	     ((null? chl) (iter 'string (cons ch chl) (read-char port)))
	     (else (iter tag (cons ch chl) (read-char port)))))))
       )

    ;; wrap w/ catch
    (read-cmmd cs:nl)))


(define (read-stmt port env) (read-command port))
  
(use-modules (ice-9 pretty-print))
(define pp pretty-print)

(define (try str)
  (let ((res (call-with-input-string str read-command)))
    (sf "\n~S =>\n" str)
    (pp res)))

(for-each
 try
 (list
  ;;"abc def ghi"
  ;;"abc \"def ghi\""
  "abc [def ghi] lkj"
  ;;"abc [def $ghi] lkj"
  ;;"abc def$ghi"
  ;;"abc def$ghi$jkl"
  ;;"abc [def ${ghi}] lkj"
  ))
