;;; nyacc/lang/mlang/parser.scm - parsing 

;; Copyright (C) 2016,2018 Matthew R. Wette
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

;;; Code:

(define-module (nyacc lang mlang parser)
  #:export (parse-mlang read-mlang-stmt read-mlang-file)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
  #:use-module (sxml fold)
  #:use-module ((srfi srfi-1) #:select (fold)))

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

;; === body

;; @deffn add-file-attr tl => tl
;; Given a tagged-list this routine adds an attribute @code{(file basename)}
;; which is the basename (with @code{.m} removed) of the current input.
;; This is used for the top-level node of the mlang parse tree to indicate
;; from which file the script or function file originated.  For example,
;; @example
;; (function-file (@ (file "myftn")) (fctn-defn (ident "myftn") ...
;; @end example
(define (add-file-attr tl)
  (let ((fn (port-filename (current-input-port))))
    (if fn (tl+attr tl 'file (basename fn ".m")) tl)))

;;; === lexical analyzer

(define *src-file* (make-parameter #f))
(define *src-line* (make-parameter #f))

;; @deffn {Procedure} mlang-read-string ch
;; Read string and return @code{($string . "string")}.  If @var{ch} is
;; not @code{"} or @code{'} the return value is @code{#f}.
;; @end deffn
(define (mlang-read-string ch)
  (case ch
    ((#\")
     (read-c-string ch))
    ((#\')
     (let loop ((cl '()) (ch (read-char)))
       (cond ((eq? ch #\\)
	      (let ((c1 (read-char)))
		(if (eq? c1 #\newline)
		    (loop cl (read-char))
		    (loop (cons c1 cl) (read-char)))))
	     ((eq? ch #\')
	      (let ((nextch (read-char)))
		(if (eq? #\' nextch)
		    (loop (cons ch cl) (read-char))
		    (begin
		      (unread-char nextch)
		      (cons '$string (list->string (reverse cl)))))))
	     (else (loop (cons ch cl) (read-char))))))
    (else #f)))


(define mlang-read-comm
  (make-comm-reader '(("%" . "\n") ("#" . "\n") ("#{" . "#}") ("%{" . "%}")
		      ("#!" . "!#"))))

;; elipsis reader "..." whitespace "\n"
(define (elipsis? ch)
  (if (eqv? ch #\.)
      (let ((c1 (read-char)))
	(if (eqv? c1 #\.)
	    (let ((c2 (read-char)))
	      (if (eqv? c2 #\.)
		  (cons (string->symbol "...") "...")
		  (begin (unread-char c2) (unread-char c1) #f)))
	    (begin (unread-char c1) #f)))
      #f))

(define (skip-to-next-line)
  (let loop ((ch (read-char)))
    (cond
     ((eof-object? ch) ch)
     ((eqv? ch #\newline) (read-char))
     (else (loop (read-char))))))

;; @deffn {Procedure} make-mlang-lexer-generator match-table
;; This function, given the @var{match-table} from a lalr-generated
;; machine, generates a procedure that returns lexical analyzers for
;; use in Octave parsers. 
;; @end deffn
(define-public (make-mlang-lexer-generator match-table)
  ;; There is some trickery here to assure that if the last line
  ;; ends w/o newline then one gets inserted.
  (let* ((read-string mlang-read-string)
	 (read-comm mlang-read-comm)
	 (read-ident read-c$-ident)
	 (space-cs (string->char-set " \t\r\f"))
	 ;;
	 (strtab (filter-mt string? match-table)) ; strings in grammar
	 (kwstab (filter-mt like-c$-ident? strtab)) ; keyword strings =>
	 (keytab (map-mt string->symbol kwstab)) ; keywords in grammar
	 (chrseq (remove-mt like-c-ident? strtab)) ; character sequences
	 (symtab (filter-mt symbol? match-table)) ; symbols in grammar
	 (chrtab (filter-mt char? match-table))	; characters in grammar
	 (read-chseq (make-chseq-reader chrseq))
	 (newline-val (assoc-ref chrseq "\n"))
	 (assc-$ (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair)))))
    (if (not newline-val) (error "mlang setup error"))
    (lambda ()
      (let ((qms #f) (bol #t) (line 0))	; qms: quote means string
	(define (loop ch)
	  (cond
	   ((eof-object? ch)
	    (if bol (assc-$ (cons '$end ch)) (loop #\newline)))
 	   ((elipsis? ch) (loop (skip-to-next-line)))
	   ((eqv? ch #\newline) (set! bol #t) (cons newline-val "\n"))
	   ((char-set-contains? space-cs ch) (set! qms #t) (loop (read-char)))
	   ((read-comm ch bol) => (lambda (p) (set! bol #f) (assc-$ p)))
	   (bol (set! bol #f) (set! line (1+ line)) (loop ch))
	   ((read-ident ch) =>
	    (lambda (s) ;; s is a string
	      (set! qms #f)
	      (or (and=> (assq-ref keytab (string->symbol s))
			 (lambda (tval) (cons tval s)))
		  (assc-$ (cons '$ident s)))))
	   ((read-c-num ch) => (lambda (p) (set! qms #f) (assc-$ p)))
	   ((char=? ch #\") (assc-$ (read-string ch)))
	   ((char=? ch #\') (if qms (assc-$ (read-string ch)) (read-chseq ch)))
	   ((read-chseq ch) =>
	    (lambda (p) (set! qms (and (memq ch '(#\= #\, #\()) #t)) p))
	   ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
	   (else (cons ch (string ch)))))
	(if #t 			; read properties
	    (lambda ()
	      (let* ((lxm (loop (read-char)))
		     (port (current-input-port))
		     (file (port-filename port))
		     (props `((filename ,file) (line ,line) (column ,0))))
		(set-source-properties! lxm props)
		lxm))
	    (lambda () (loop (read-char))))))))

;; === static semantics

;; 1) assn: "[ ... ] = expr" => multi-assign
;; 2) matrix: "[ int, int, int ]" => ivec
;; 3) matrix: "[ ... (fixed) ... ]" => error
;; .) colon-expr => fixed-colon-expr

(define (fixed-colon-expr? expr)
  (sx-match expr
    ((colon-expr) #t)
    ((colon-expr (fixed ,s) (fixed ,e)) #t)
    ((colon-expr (fixed ,s) (fixed ,i) (fixed ,e)) #t)
    (,_ #f)))

(define (fixed-expr? expr)
  (define (fixed-primary-expr? expr)
    (sx-match expr
      ((fixed ,val) #t)
      ((add ,lt ,rt) (and (fixed-expr? lt) (fixed-expr? rt)))
      ((sub ,lt ,rt) (and (fixed-expr? lt) (fixed-expr? rt)))
      ((mul ,lt ,rt) (and (fixed-expr? lt) (fixed-expr? rt)))
      (,_ #f)))
  (or (fixed-primary-expr? expr)
      (eq? 'fixed-colon-expr (sx-tag expr))))

(define (float-expr? expr)
  (define (float-primary-expr? expr)
    (sx-match expr
      ((float ,val) #t)
      ((add ,lt ,rt) (and (float-expr? lt) (float-expr? rt)))
      ((sub ,lt ,rt) (and (float-expr? lt) (float-expr? rt)))
      ((mul ,lt ,rt) (and (float-expr? lt) (float-expr? rt)))
      ((div ,lt ,rt) (and (float-expr? lt) (float-expr? rt)))
      (,_ #f)))
  (float-primary-expr? expr))

(define (fixed-vec? row)
  (fold (lambda (elt fx) (and fx (fixed-expr? elt))) #t (sx-tail row)))

(define (float-vec? row)
  (fold (lambda (elt fx) (and fx (float-expr? elt))) #t (sx-tail row)))

(define (float-mat? mat)
  (fold (lambda (row fx) (and fx (float-vec? row))) #t
	(map sx-tail (sx-tail mat))))

(define (check-matrix mat)
  (let* ((rows (sx-tail mat))
	 (nrow (length rows))
	 (row1 (if (positive? nrow) (car rows) #f)))
    (cond
     ((zero? nrow) mat)
     ((and (= 1 nrow) (fixed-vec? row1))
      (cons-source row1 'fixed-vector (cdr row1)))
     ((float-mat? mat)
      (cons-source mat 'float-matrix (cdr mat)))
     (else mat))))

;; @deffn {Procedure} apply-mlang-statics tree => tree
;; Apply static semantics for Octave.  Currently, this includes
;; @itemize
;; @item Change @code{assn} with matrix expression on LHS to a
;; multiple value assignment (@code{multi-assn}).
;; @end itemize
;; @end deffn
(define (apply-mlang-statics tree)

  (define (fU tree)
    (sx-match tree
      ((assn (@ . ,attr) (matrix (row . ,elts)) ,rhs)
       (cons-source tree 'multi-assn `((@ . ,attr) (lval-list . ,elts) ,rhs)))
      ((colon-expr . ,rest)
       (if (fixed-colon-expr? tree)
	   (cons-source tree 'fixed-colon-expr (cdr tree))
	   tree))
      ((matrix . ,rest)
       (check-matrix tree))
      (,_ tree)))
  
  (define (fH tree) tree)
  
  (cadr (foldt fU fH `(*TOP* ,tree))))

;; === file parser 

(include-from-path "nyacc/lang/mlang/mach.d/mlang-tab.scm")
(include-from-path "nyacc/lang/mlang/mach.d/mlang-act.scm")

(define gen-mlang-lexer (make-mlang-lexer-generator mlang-mtab))

;; Parse given a token generator.
(define raw-parser
  (make-lalr-parser
   (acons 'act-v mlang-act-v mlang-tables)))

(define* (parse-mlang #:key debug)
  (catch
   'nyacc-error
   (lambda ()
     (apply-mlang-statics
      (raw-parser (gen-mlang-lexer) #:debug debug)))
   (lambda (key fmt . args)
     (apply simple-format (current-error-port) fmt args)
     (newline (current-error-port))
     #f)))

(define (read-mlang-file port env)
  ;;(sferr "<parse file>\n")
  (with-input-from-port port
    (lambda ()
      (if (eof-object? (peek-char port))
	  (read-char port)
	  (parse-mlang #:debug #f)))))

;; === interactive parser

(include-from-path "nyacc/lang/mlang/mach.d/mlangia-tab.scm")
(include-from-path "nyacc/lang/mlang/mach.d/mlangia-act.scm")

(define raw-ia-parser
  (make-lalr-parser
   (acons 'act-v mlangia-act-v mlangia-tables)
   #:interactive #t))

(define (parse-mlang-stmt lexer)
  (catch 'nyacc-error
    (lambda ()
      (apply-mlang-statics (raw-ia-parser lexer #:debug #f)))
    (lambda (key fmt . args)
      (apply simple-format (current-error-port) fmt args)
      (newline (current-error-port))
      #f)))

(define gen-mlang-ia-lexer (make-mlang-lexer-generator mlangia-mtab))

(define read-mlang-stmt
  (let ((lexer (gen-mlang-ia-lexer)))
    (lambda (port env)
      ;;(sferr "<parse stmt>\n")
      (cond
       ((eof-object? (peek-char port))
	(read-char port))
       (else
	(let* ((stmt (with-input-from-port port
		      (lambda () (parse-mlang-stmt lexer))))
	       (stmt (apply-mlang-statics stmt)))
	  ;;(sferr "stmt=~S\n" stmt)
	  (cond
	   ((equal? stmt '(empty-stmt)) #f)
	   (stmt)
	   ;;(else (flush-input-after-error port) #f)
	   )))))))

;; --- last line ---
