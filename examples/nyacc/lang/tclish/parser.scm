;; (tclish reader)

;; proc foo {{x 1} {y 2}} {
;; }

;; syntax cond {else}
;;  {{cond {(expr) body ...} {(expr) body ...} ... {else 
;; 

(use-modules (nyacc lex))
(use-modules (nyacc lalr))
(use-modules (nyacc parse))
(use-modules (nyacc lang sx-util))
(use-modules (nyacc lang util))

(use-modules (ice-9 pretty-print))
(define pp pretty-print)
(define (sf fmt . args) (apply simple-format #t fmt args))

(include-from-path "nyacc/lang/tclish/mach.d/tsh-tab.scm")

;; core types: i8 u8 i32 u32 i64 u64 f64 f64x3 f64x6 symbol
;; ref types: vector string dict(symbols only)
;; symbols stored w/ (997) hash
;; expr-list: ( ... )
;; expr-arry: @( ... )

;; symbol: 'abc'
;; ident : abc
;; expr-list '(' abc , def ')'


;; variable is foo or foo(x,y,z)
(define read-tsh-id-kw
  (let ((id-kw-rdr (make-ident-keyword-reader read-c-ident tsh-mtab))
	(ident/ix (assq-ref tsh-mtab '$ident/ix)))
    (lambda (ch)
      (let ((pair (id-kw-rdr ch)))
	(and
	 pair
	 (let ((ch (read-char)))
	   (cond
	    ((eof-object? ch) pair)
	    ((char=? ch #\() (unread-char ch) (cons ident/ix (cdr pair)))
	    (else (unread-char ch) pair))))))))

(define make-tsh-lexer
  (let* ((space-cs (string->char-set " \t\r\f"))
	 (strtab (filter-mt string? tsh-mtab))
	 (chrseq (remove-mt like-c-ident? strtab))
	 (read-chseq (make-chseq-reader chrseq))
	 ;;(x (begin (pp chrseq) (quit)))
	 (symtab (filter-mt symbol? tsh-mtab))
	 ;;
	 (rd-str (make-string-reader #\" #:token (assq-ref tsh-mtab '$string)))
	 (rd-sym (make-string-reader #\' #:token (assq-ref tsh-mtab '$symbol)))
	 (read-comm (make-comm-reader '(("#" . "\n")) #:eat-newline #f))
	 (nl-val (assoc-ref chrseq "\n"))
	 (lparen (assoc-ref chrseq "("))
	 (rparen (assoc-ref chrseq ")"))
	 (assc-$ (lambda (p) (cons (assq-ref symtab (car p)) (cdr p)))))
    (lambda ()
      (let ((plev 0) (bol #t))
	(lambda ()
	  (
	   identity
	   ;;pk
	   (let loop ((ch (read-char)))
	     (cond
	      ((eof-object? ch) (assc-$ (cons '$end ch)))
	      ((eqv? ch #\newline) (set! bol #t) (cons nl-val "\n"))
	      ((char-set-contains? space-cs ch) (loop (read-char)))
	      ((read-comm ch bol) => assc-$)
	      ((read-c-num ch) => (lambda (p) #;(pp p) (assc-$ p)))
	      ((read-tsh-id-kw ch))
	      ((char=? #\( ch) (set! plev (1+ plev)) (cons lparen "("))
	      ((char=? #\) ch) (set! plev (1- plev)) (cons rparen ")"))
	      ((rd-str ch))
	      ((rd-sym ch))
	      ((read-chseq ch))
	      (else (cons ch (string ch)))))))))))


(include-from-path "nyacc/lang/tclish/mach.d/tsh-act.scm")

(define raw-parser
  (make-lalr-parser (acons 'act-v tsh-act-v tsh-tables)
		    ;; #:skip-if-unexp '($lone-comm $code-comm "\n")
		    ))

(define* (parse-tsh #:key debug)
  (catch 'nyacc-error
    (lambda () (raw-parser (make-tsh-lexer) #:debug debug))
    (lambda (key fmt . args)
      (apply simple-format (current-error-port) fmt args)
      (newline (current-error-port))
      #f)))
  
(define raw-ia-parser
  (make-lalr-parser
   (acons 'act-v tsh-act-v tsh-tables)
   #:skip-if-unexp '($lone-comm $code-comm "\n")
   #:interactive #t))

(define (parse-tsh-stmt lexer)
  (catch 'nyacc-error
    (lambda () (raw-ia-parser lexer #:debug #f))
    (lambda (key fmt . args)
      (apply simple-format (current-error-port) fmt args)
      #f)))

(define (show-toks lxr)
  (let loop ((tok (lxr)))
    (cond
     ((and (pair? tok) (eq? '$end (car tok))))
     (else
      (pp tok)
      (loop (lxr))))))

(with-input-from-file "demo01.tsh"
  (lambda () (pp (parse-tsh #:debug #f))))

;; --- last line ---
