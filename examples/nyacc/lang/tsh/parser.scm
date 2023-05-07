;; nyacc/lang/tsh/parser.scm

;; Copyright (C) 2021 Matthew R. Wette
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

;; proc foo {{x 1} {y 2}} {
;; }

;; syntax cond {else}
;;  {{cond {(expr) body ...} {(expr) body ...} ... {else 
;;

(define-module (nyacc lang tsh parser)
  #:export (parse-tsh
	    read-tsh-stmt
	    read-tsh-file
	    )
  #:use-module (nyacc lex)
  #:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang sx-util)
  #:use-module (nyacc lang util))

(use-modules (ice-9 pretty-print))
(define pp pretty-print)
(define (sf fmt . args) (apply simple-format #t fmt args))

(define rls reverse-list->string)

;; core types: i8 u8 i32 u32 i64 u64 f64 f64x3 f64x6 symbol
;; ref types: vector string dict(symbols only)
;; symbols stored w/ (997) hash
;; expr-list: ( ... )
;; expr-arry: @( ... )

;; symbol: abc                ($ident )
;; keysym: -abc               ($ident/key ) ???
;; indexed-symbol: abc(...)   ($ident/ix ...)

(define cs:keyword (char-set-adjoin char-set:letter+digit #\_))

(define (read-key ch)
  (and
   (char=? ch #\-)
   (let ((ch (read-char)))
     (if (char=? ch #\-)
         (let loop ((chl '()) (ch (read-char)))
           (cond
            ((eof-object? ch) `($keyword . ,(rls chl)))
            ((char-set-contains? cs:keyword ch)
             (loop (cons ch chl) (read-char)))
            (else (unread-char ch) `($keyword . ,(rls chl)))))
         (let ((ch (read-char)))
           `($keychar . ,(string ch)))))))

#|
(define (read-path)
  (define (return path chl)
    (reverse (if (pair? chl) (cons (rls chl) path) path)))
  (let loop ((path '()) (chl '()) (st 0) (ch (read-char)))
    (case st
      ((0)
       (cond
        ((memq ch '(#\space #\tab #\return)) (loop path chl st (read-char)))
        (else (loop path chl 1 ch))))
      ((1)
       (cond
        ((char=? #\newline ch) (unread-char ch) (return path chl))
        ((char=? #\return ch) (return path chl))
        ((char=? #\: ch) (loop path chl 2 (read-char)))
        (else (loop path (cons ch chl) 1 (read-char)))))
      ((2)
       (cond
        ((char=? #\newline ch) (unread-char ch) (return path chl))
        ((char=? #\return ch) (return path chl))
        ((char=? #\: ch) (loop (cons (rls chl) path) '() 1 (read-char)))
        (else (loop path (cons ch chl) 1 ch)))))))
|#

(define (read-$-form ch)
  (and
   (char=? ch #\$)
   (let loop ((chl '()) (st 0) (ch (read-char)))
     (case st
       ((0)
        (cond
         ((char-set-contains? c:if ch) (loop (cons ch chl) 1 (read-char)))
         (else (error "bad ident")))) ;; FIXME
       ((1)
        (cond
         ((eof-object? ch) (cons '$deref (rls chl)))
         ((char-set-contains? c:ir ch) (loop (cons ch chl) st (read-char)))
         ((char=? ch #\() (unread-char ch) (cons '$deref/ix (rls chl)))
         (else (unread-char ch) (cons '$deref (rls chl)))))))))

(define (make-tsh-lexer-generator match-table)
  (let* ((tsh-mtab match-table)
	 (space-cs (string->char-set " \t\r\f"))
	 (strtab (filter-mt string? tsh-mtab))
	 (chrseq (remove-mt like-c-ident? strtab))
	 (read-chseq (make-chseq-reader chrseq))
	 (symtab (filter-mt symbol? tsh-mtab))
	 ;;
	 (rd-str (make-string-reader #\" (assq-ref tsh-mtab '$string)))
 	 (rd-sym (make-string-reader #\' (assq-ref tsh-mtab '$symbol)))
	 (read-comm (make-comm-reader '(("#" . "\n")) #:eat-newline #f))
	 (read-tsh-symbol (make-ident-keyword-reader read-c-ident match-table))
         (nl-val (assoc-ref chrseq "\n"))
	 (lparen (assoc-ref chrseq "("))
	 (rparen (assoc-ref chrseq ")"))
	 (lbrack (assoc-ref chrseq "["))
	 (rbrack (assoc-ref chrseq "]"))
         (use (assoc-ref match-table "use"))
 	 (assc-$ (lambda (p) (cons (assq-ref symtab (car p)) (cdr p)))))
    (define (add-src-prop pair)
      (let* ((port (current-input-port))
	     (srcp `((filename . ,(or (port-filename port) "user prompt"))
		     (line . ,(port-line port)) (column . 0))))
	(set-source-properties! pair srcp)
	pair))
    (lambda ()
      (let ((plev 0) (blev 0) (nws #f) (bol #t))
        ;; (-level, [-level, no-ws seen, begin-of-line
        ;; first token should be OK w/ ws
	(lambda ()
	  (add-src-prop
	   (let loop ((ch (read-char)))
	     (cond
	      ((eof-object? ch) (assc-$ (cons '$end ch)))
	      ((eqv? ch #\newline) (set! bol #t) (cons nl-val "\n"))
	      ((char-set-contains? space-cs ch)
               (set! nws #f) (loop (read-char)))
              #;((and bol (not wss) (char=? ch #\%)) ;; %if 0 ... %endif
              (preprocessor-insn))
              (nws (unread-char ch) (set! nws #f) (assc-$ `(no-ws . "")))
              ((begin (set! nws #t) #f))
	      ((read-comm ch bol) => assc-$)
              ((read-$-form ch) => assc-$)
              ((and (or (zero? plev) (> blev plev)) (read-key ch)) => assc-$)
	      ((read-c-num ch) => (lambda (p) (assc-$ p)))
	      ((read-tsh-symbol ch))
	      ((char=? #\( ch) (set! plev (1+ plev)) (cons lparen "("))
	      ((char=? #\) ch) (set! plev (1- plev)) (cons rparen ")"))
	      ((char=? #\( ch) (set! blev (1+ blev)) (cons lbrack "["))
	      ((char=? #\) ch) (set! blev (1- blev)) (cons rbrack "]"))
	      ((rd-str ch))
	      ((rd-sym ch))
	      ((read-chseq ch))
	      (else (cons ch (string ch)))))))))))

(include-from-path "nyacc/lang/tsh/mach.d/tsh-tab.scm")
(include-from-path "nyacc/lang/tsh/mach.d/tsh-act.scm")

(define raw-parser
  (make-lalr-parser (acons 'act-v tsh-act-v tsh-tables)
		    #:skip-if-unexp '($lone-comm $code-comm "\n" no-ws)))

(define parse-tsh
  (let ((make-tsh-lexer (make-tsh-lexer-generator tsh-mtab)))
    (lambda* (#:key debug)
      (catch 'nyacc-error
	(lambda () (raw-parser (make-tsh-lexer) #:debug debug))
	(lambda (key fmt . args)
	  (apply simple-format (current-error-port) fmt args)
	  (newline (current-error-port))
	  #f)))))

;; @deffn {Procedure} read-tsh-file port env
;; Read a TCLish file.  Return a SXML tree;
;; @end deffn
(define* (read-tsh-file port env #:key debug)
  (let ((prev (current-input-port)))
    (dynamic-wind
      (lambda () (set-current-input-port port))
      (lambda () (parse-tsh #:debug debug))
      (lambda () (set-current-input-port prev)))))
  
(include-from-path "nyacc/lang/tsh/mach.d/tsh-ia-tab.scm")
(include-from-path "nyacc/lang/tsh/mach.d/tsh-ia-act.scm")

(define raw-ia-parser
  (make-lalr-parser (acons 'act-v tsh-ia-act-v tsh-ia-tables)
                    #:skip-if-unexp '(no-ws) #:interactive #t))

;; @deffn {Procedure} read-tsh-stmt port env
;; Read a TCLish item.  Return a SXML tree;
;; @end deffn
(define read-tsh-stmt
  (let* ((make-tsh-lexer (make-tsh-lexer-generator tsh-ia-mtab))
	 (lexer (make-tsh-lexer)))
    (lambda (port env)
      (let ((prev (current-input-port)))
	(dynamic-wind
	  (lambda () (set-current-input-port port))
	  (lambda ()
	    (catch 'nyacc-error
	      (lambda () (raw-ia-parser lexer #:debug #f))
	      (lambda (key fmt . args)
		(apply simple-format (current-error-port) fmt args)
		(newline (current-error-port))
		#f)))
	  (lambda () (set-current-input-port prev)))))))

;; --- last line ---
