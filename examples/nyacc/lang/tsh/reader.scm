;; @deffn {Procedure} read-basic-num ch [#:signed #t] => #f|pair
;; Reader for numbers in most C-like languages.  The @var{pair} returned
;; is one of @code{($fixed . "1")} or @code{($float . "1.0")}.
;; The @code{fixed} form include decimal, hexidecimal (starting with "0x"),
;; and binary (starting with "0b") integers.  The @code{float} form is
;; floating point.  This will not generate exceptions.  Normally this
;; reads unsigned numbers; with the @code{#:signed #t} option, leading
;; @code{-} is allowed.
;; @end deffn
(define* (read-basic-num ch #:key signed)
  (define (unread-chl chl)
    (let lp ((l chl)) (unless (null? l) (unread-char (car l)) (lp (cdr l)))))
  (let loop ((chl '()) (ty #f) (st 10) (ch ch))
    ;;(sf "lp: ch=~S st=~S ty=~S chl=~S\n" ch st ty chl)
    (case st
      ((10) ;; entry
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char=? #\0 ch) (loop (cons ch chl) '$fixed 11 (read-char))) 
	((char-numeric? ch) (loop chl '$fixed 20 ch))
	((char=? #\. ch) (loop (cons ch chl) ty 12 (read-char)))
	((and signed (char=? #\- ch)) (loop (cons ch chl) ty 13 (read-char)))
	(else #f)))
      ((11) ;; got 0/fixed, allow x, b
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char=? ch #\.) (loop (cons ch chl) '$float 30 (read-char)))
	((memq ch '(#\X #\x)) (loop (cons ch chl) ty 21 (read-char)))
	((memq ch '(#\B #\b)) (loop (cons ch chl) ty 22 (read-char)))
	((char-oct? ch) (loop (cons ch chl) ty 23 (read-char)))
	(else (loop chl ty 70 ch))))
      ((12) ;; got `.'
       (cond
	((eof-object? ch) #f)
	((char-numeric? ch) (loop (cons ch chl) '$float 30 (read-char)))
	(else (unread-char ch) #f)))
      ((13) ;; got '-'
       (cond
	((eof-object? ch) (unread-char #\-) #f)
	((char=? #\0 ch) (loop (cons ch chl) '$fixed 11 (read-char)))
	((char-numeric? ch) (loop chl '$fixed 20 ch))
	((char=? #\. ch) (loop (cons ch chl) ty 12 (read-char)))
	(else (loop (cons ch chl) ty 73 ch))))
      ((20) ;; parse decimal integer part
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-numeric? ch) (loop (cons ch chl) ty 20 (read-char)))
	((char=? ch #\.) (loop (cons #\. chl) '$float 30 (read-char)))
	((memq ch '(#\E #\e)) (loop (cons ch chl) '$float 40 (read-char)))
	(else (loop chl '$fixed 70 ch))))
      ((21) ;; parse fixed hex
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-hex? ch) (loop (cons ch chl) ty 21 (read-char)))
	(else (loop chl ty 70 ch))))
      ((22) ;; parse fixed octal	      
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-oct? ch) (loop (cons ch chl) ty 22 (read-char)))
	(else (loop chl ty 70 ch))))
      ((23) ;; parse fixed binary 
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((memq ch '(#\0 #\1)) (loop (cons ch chl) ty 11 (read-char)))
	(else (loop chl ty 70 ch))))
      ((30) ;; parse float fractional part
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-numeric? ch) (loop (cons ch chl) ty 30 (read-char)))
	((memq ch '(#\E #\e)) (loop (cons ch chl) ty 40 (read-char)))
	(else (loop chl ty 70 ch))))
      ((40) ;; have float e|p, look for sign
       (cond
	((eof-object? ch) (loop chl ty 73 ch))
	((memq ch '(#\+ #\-)) (loop (cons ch chl) ty 50 (read-char)))
	(else (loop chl ty 50 ch))))
      ((50) ;; parse rest of exponent
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-numeric? ch) (loop (cons ch chl) ty 50 (read-char)))
	(else (loop chl ty 70 ch))))
      ((70)
       (cond
	((eof-object? ch) (cons ty (rls chl)))
	;;((memq ch '(f i u)) (loop
	(else (loop chl ty 71 ch))))
      ((71) (unread-char ch) (cons ty (rls chl)))
      ((72) (cons ty (rls chl)))
      ((73) (unread-chl chl) #f)
      ((74) ;; rustoleum
       (cond
	((eof-object? ch) (loop chl ty 73 ch))
	((char-numeric? ch) (loop (cons ch chl) ty 75 (read-char)))
	;;(else (
	))
      ((75) ;; rustoleum
      (else (error "coding error")))))
