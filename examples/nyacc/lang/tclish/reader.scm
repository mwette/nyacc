;; (tclish reader)

;; proc foo {{x 1} {y 2}} {
;; }

;; syntax cond {else}
;;  {{cond {(expr) body ...} {(expr) body ...} ... {else 
;; 

(use-modules (nyacc lex))

(use-modules (ice-9 pretty-print))
(define pp pretty-print)
(define (sf fmt . args) (apply simple-format #t fmt args))

;; core types: c8 u32 i32 f64 f64x3 f64x6 symbol
;; ref types: vector string dict(symbols only)
;; symbols stored w/ (997) hash

(define mt
  '(("\n" . nl)
    ("set" . set)
    ("(" . lparen)
    ("(" . rparen)
    ;;($float . float)
    ;;($fixed . float)
    ))

(define (make-ident-keyword-reader ident-reader match-table)
  (let ((ident-like? (make-ident-like-p ident-reader)))
    (let loop ((kt '()) (mt match-table))
      (if (null? mt)
	  (lambda (ch)
	    (and=> (ident-reader ch)
		   (lambda (s) (cons (or (assoc-ref kt s) '$ident) s))))
	  (loop (if (ident-like? (caar mt)) (cons (car mt) mt) mt) (cdr mt))))))
	 
(define (make-lexer-generator match-table)
  (let* ((space-cs (string->char-set " \t\r\f"))
	 (strtab (filter-mt string? match-table)) ; strings in grammar
	 (kwstab (filter-mt like-c$-ident? strtab)) ; keyword strings =>
	 (keytab (map-mt string->symbol kwstab)) ; keywords in grammar
	 (chrseq (remove-mt like-c-ident? strtab)) ; character sequences
	 (symtab (filter-mt symbol? match-table)) ; symbols in grammar
	 (chrtab (filter-mt char? match-table))	; characters in grammar
	 (read-chseq (make-chseq-reader chrseq))
	 (nl-val (assoc-ref chrseq "\n"))
	 (id-or-kw? (make-ident-keyword-reader read-c-ident match-table))
	 (assc-$ (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair))))
	 )
    (lambda ()
      (let ((mode 'code)
	    (plev 0)			; paren level
	    (bol #t)			; begin-of-line?
	    )
	(lambda ()
	  (let loop ((ch (read-char)))
	    (case mode
	      ((code)
	       (cond
		((eof-object? ch) (cons '$end ch))
		((eqv? ch #\newline) (set! bol #t) (cons nl-val "\n"))
		;;((read-c-num ch) => (lambda (p) (assc-$ p)))
		((read-c-num ch))
		((char-set-contains? space-cs ch) (loop (read-char)))
		((char=? #\newline ch) (cons nl-val "\n"))
		((char=? #\( ch) (set! plev (1+ plev)) (cons 'lparen "("))
		((char=? #\) ch) (set! plev (1- plev)) (cons 'rparen ")"))
		((id-or-kw? ch))
		((read-chseq ch))
		(else (cons ch (string ch))))))))))))


#|
(define (tsh-parse-code tok state stack)
  #f)

(define (tsh-parse-expr tok state)
  #f)

(define (tsh-read state)
  (let loop ((ch (read-char)))
    (cond
     ((eof-object? ch) (tsh-parse (cons $end ch)))
     ((char=? #\newline ch) #f)
     ((char=? #\( ch) (tsh-read-expr state))
     ((read-c-ident ch) => (lambda (tk) (tsh-parse tk state)))
     (else (error "xxx")))))
|#

(let* ((file "demo01.tsh")
      (make-lexer (make-lexer-generator mt))
      (lxr (make-lexer))
      )
  (with-input-from-file "demo01.tsh"
    (lambda ()
      (let loop ((tok (lxr)))
	(cond
	 ((and (pair? tok) (eq? '$end (car tok))))
	 (else
	  (pp tok)
	  (loop (lxr))))))
      ))

;; --- last line ---
