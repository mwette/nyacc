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
  #:export (read-command read-tcl-stmt read-tcl-body split-body)
  )

(define (sf fmt . args) (apply simple-format #t fmt args))
(define (db fmt . args) #f)

(define (xunread-char char port)
  (db "unread-char ~S\n" char)
  (unread-char char port))
(define (xread-char port)
  (let ((ch (read-char port)))
    (db "read-char ~S\n" ch)
    ch))

(define (read-command port)

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

  (define (rls chl) (reverse-list->string chl))

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
	  (cons (if (null? (cddr word)) (cadr word) word) word-list)))
	  ;;(cons word word-list)))

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
	      (let ((word (read-word cs:rcurly)))
		(xread-char port) ;; }
		(loop (foldin word wordl) (peek-char port))))

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

       (read-frag
	(lambda (end-cs)
	  (let loop ((tag #f) (chl '()) (ch (xread-char port)))
	    (db "F:     tag=~S ch=~S chl=~S end-cs=~S\n" tag ch chl end-cs)
	    (cond
	     ((eof-object? ch)
	      (if (pair? chl) (finish tag chl ch) end-cs))
	     ((char-set-contains? end-cs ch)
	      (db "F:     done\n")
	      (if tag (finish tag chl ch)
		  (begin (xunread-char ch port) end-cs)))
	     ((char=? ch #\\) ;; a b f n r t v ; xhh uhhhh Uhhhhhhhh
	      (let* ((c2 (xread-char port))
		     (c1 (case c2
			   ((#\a) #\alarm) ((#\b) #\backspace);((#\f) #\formfeed)
			   ((#\n) #\newline) ((#\r) #\return) ((#\t) #\tab)
			   (else c2))))

		(loop tag (cons c2 chl) (xread-char port))))
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
	     ;;((char=? ch #\{) (swallow `(string ,(read-frag cs:rcurly))))
	     ((char=? ch #\{) (swallow (read-frag cs:rcurly)))
	     ((char=? ch #\[) (swallow (read-cmmd cs:rs)))
	     ((char=? ch #\") (swallow (read-frag cs:dquote)))
	     ((not tag) (loop 'string (cons ch chl) (xread-char port)))
	     (else (loop tag (cons ch chl) (xread-char port)))))))

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
      cmd)))

;; For strings which are known to be interpreted as bodies,
;; split them into a sequence of commands.
(define (split-body body)
  `(body
    ,@(call-with-input-string body
	(lambda (port)
	  (let loop ((cmd (read-command port)))
	    (if (eof-object? cmd) '()
		(cons cmd (loop (read-command port)))))))))

(define (read-tcl-body port)
  `(body
    (let loop ((cmd (read-command port)))
      (if (eof-object? cmd) '()
	  (cons cmd (loop (read-command port)))))))

(use-modules ((sxml fold) #:select (foldt)))
(use-modules (nyacc lang sx-util))

#;(define (cnvt-tcl-body body)
  (define (fU tree)
    (sx-match tree
      ((command ,cmd . ,args)
       )
      (else
       tree)))

  (define (fH tree)
    tree)

  (cadr (foldt fU fH `(*TOP* ,tree))))

(define (read-tcl-stmt port env) (read-command port))

(use-modules (ice-9 pretty-print))
(define pp pretty-print)

(define (doit)
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

#|
(define (try str)
  (let ((res (call-with-input-string str read-command)))
    (sf "\n~S =>\n" str)
    (pp res)))

(for-each
 try
 (list
  "abc def ghi"
  "abc \"def ghi\""
  "abc [def ghi] "
  "abc [def ghi] lkj"
  "abc [def $ghi] lkj"
  "abc def$ghi"
  "abc def$ghi$jkl"
  "abc [def ${ghi}] lkj"
  "abc [def ${ghi jkl}] mno"

  ;; later
  "abc ${def}(ghi)"
  ))
;;|#

;; --- last line ---
