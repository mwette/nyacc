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

(include-from-path "nyacc/lang/tclish/mach.d/tsh-tab.scm")

;; core types: i8 u8 i32 u32 i64 u64 f64 f64x3 f64x6 symbol
;; ref types: vector string dict(symbols only)
;; symbols stored w/ (997) hash

;; symbol: 'abc'
;; ident : abc
;; expr-list '(' abc , def ')'

;; variable is foo or foo(x,y,z)
(define read-tsh-id-kw
  (let ((ikr (make-ident-keyword-reader read-c-ident tsh-mtab)))
    (lambda (ch)
      (let ((p (ikr ch)))
	(and p (let ((ch (read-char)))
		 (cond
		  ((eof-object? ch) p)
		  ((char=? ch #\() (unread-char ch) `(ident/ix . (cdr p)))
		  (else (unread-char ch) p))))))))

(define make-tsh-lexer
  (let* ((space-cs (string->char-set " \t\r\f"))
	 (chrseq (remove-mt like-c-ident? tsh-mtab))
	 (read-chseq (make-chseq-reader chrseq))
	 (read-comm (make-comm-reader '(("#" . "\n")) #:eat-newline #f))
	 (nl-val (assoc-ref chrseq "\n"))
	 (lparen (assoc-ref chrseq "("))
	 (rparen (assoc-ref chrseq "("))
	 (assc-$ (lambda (pair)
		   (cons (assq-ref symtab (car pair)) (cdr pair))))
	 )
    (lambda ()
      (let ((plev 0) (bol #t))
	(lambda ()
	  (let loop ((ch (read-char)))
	    (cond
	     ((eof-object? ch) (cons '$end ch))
	     ((eqv? ch #\newline) (set! bol #t) (cons nl-val "\n"))
	     ((char-set-contains? space-cs ch) (loop (read-char)))
	     ((read-comm ch bol))
	     ((read-c-num ch) => (lambda (p) (assc-$ p)))
	     ((read-tsh-id-kw ch))
	     ((char=? #\( ch) (set! plev (1+ plev)) (cons lparen "("))
	     ((char=? #\) ch) (set! plev (1- plev)) (cons rparen ")"))
	     ((read-chseq ch))
	     (else (cons ch (string ch))))))))))


(let* ((file "demo01.tsh")
       ;;(make-lexer (make-lexer-generator mt))
      (lxr (make-tsh-lexer))
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
