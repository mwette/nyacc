;;; language/nx-mlang/parser.scm - parsing 

;; Copyright (C) 2016,2018,2025 Matthew Wette
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

(define-module (language nx-mlang parser)
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

(define space-cs (string->char-set " \t"))

(define (flush-ws)
  (let loop ((ch (read-char)))
    (cond
     ((eof-object? ch))
     ((char-set-contains? space-cs ch) (loop (read-char)))
     (else (unread-char ch)))))

;; @deffn {Procedure} make-mlang-lexer-generator match-table
;; This function, given the @var{match-table} from a lalr-generated
;; machine, generates a procedure that returns lexical analyzers for
;; use in Octave parsers. 
;; @end deffn
(define* (make-mlang-lexer-generator match-table #:key interactive)
  ;; There is some trickery here to assure that if the last line
  ;; ends w/o newline then one gets inserted.
  (let* ((read-string mlang-read-string)
         (read-comm mlang-read-comm)
         (skip-comm (if interactive read-comm (const #f)))
         (read-ident read-c$-ident)
         ;;
         (strtab (filter-mt string? match-table)) ; strings in grammar
         (kwstab (filter-mt like-c$-ident? strtab)) ; keyword strings =>
         (keytab (map-mt string->symbol kwstab)) ; keywords in grammar
         (chrseq (remove-mt like-c-ident? strtab)) ; character sequences
         (symtab (filter-mt symbol? match-table)) ; symbols in grammar
         (chrtab (filter-mt char? match-table)) ; characters in grammar
         (read-chseq (make-chseq-reader chrseq))
         (nl-val (assoc-ref match-table "\n"))
         (sp-val (assoc-ref match-table 'sp))
         (assc-$
          (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair)))))
    (if (not nl-val) (error "mlang setup error"))
    (lambda ()
      (let ((qms #f) (bol #t)) ; qms: quote means string
        (define (loop ch)
          (cond
           ((eof-object? ch)
            (if bol (assc-$ (cons '$end ch)) (loop #\newline)))
           ((elipsis? ch) (loop (skip-to-next-line)))
           ((eqv? ch #\newline) (set! bol #t) (cons nl-val "\n"))
           ((char-set-contains? space-cs ch)
            (set! qms #t) (flush-ws) (cons sp-val " "))
           ((skip-comm ch) (loop (read-char))) ; must be before read-comm
           ((read-comm ch bol) => (lambda (p) (set! bol #f) (assc-$ p)))
           (bol (set! bol #f) (loop ch))
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
            ;;(lambda (p) (set! qms (and (memq ch '(#\= #\, #\()) #t)) p))
            (lambda (p) (set! qms (not (memq ch '(#\] #\} #\))))) p))
           ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
           (else (cons ch (string ch)))))
            (lambda ()
              (let* ((lxm (loop (read-char)))
                     (port (current-input-port))
                     (file (port-filename port))
                     (line (1+ (port-line port)))
                     (props `((filename . ,file) (line . ,line) (column . 0))))
                (set-source-properties! lxm props)
                lxm))))))


;; === static semantics

;; This won't be able to resolve all aref-or-call if classes are used.

;; 1) assn: "[ ... ] = expr" => assn-many
;; 2) matrix: "[ int, int, int ]" => ivec
;; 3) matrix: "[ ... (fixed) ... ]" => error
;; 4) colon-expr => fixed-colon-expr
;; 5) aref-or-call => array-ref | call

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
;; multiple value assignment (@code{assn-many}).
;; @end itemize
;; TODO: aref-or-call:
;; @end deffn
(define (apply-old-mlang-statics tree)

  (define (fU tree)
    (sx-match tree
      ((assn (@ . ,attr) (matrix (row . ,elts)) ,rhs)
       (cons-source tree 'assn-many `((@ . ,attr) (lval-list . ,elts) ,rhs)))
      ((colon-expr . ,rest)
       (if (fixed-colon-expr? tree)
           (cons-source tree 'fixed-colon-expr (cdr tree))
           tree))
      ((matrix . ,rest)
       (check-matrix tree))
      (,_ tree)))
  
  (define (fH tree) tree)
  
  (cadr (foldt fU fH `(*TOP* ,tree))))

(define (lval-root lval)
  (sx-match lval
    ((ident ,name) lval)
    ((aref-or-call ,par ,exl) (lval-root par))
    ((cell-ref ,par ,exl) (lval-root par))
    ((sel ,kid ,par) (lval-root par))
    (,otherwise #f)))

(define (lval-name lval)
  (and=> (lval-root lval) cadr))

(define (fix-lval lval)
  (sx-match lval
    ((aref-or-call ,par ,exl) `(array-ref ,par ,exl))
    (,__ lval)))

;; assn matrix rhs => assn-many lval-list rhs
;;(define apply-mlang-statics identity)
(define (apply-mlang-statics tree)
  ;; (aref-or-call (handle ...) ...) is call
  ;; gbl : global variables (e.g., from global)
  ;; lcl : local variables (e.g., function ins or outs)
  (define (fD tree seed gbl lcl)        ; => tree seed gbl lcl
    ;;(sferr "fD:\n") (pperr tree) ;;(pperr seed)
    (sx-match tree
      ((assn (@ . ,attr) (matrix (row . ,elts)) ,rhs)
       (values (sx-list/src tree 'assn-many attr `(lval-list . ,elts) rhs)
               '() gbl lcl))
      ((assn (@ . ,attr) (ident ,name) ,rhs)
       (cond
        ((member name lcl) (values tree '() gbl lcl))
        ((member name gbl) (values tree '() gbl lcl))
        (else (values tree '() gbl (cons name lcl)))))
      ((fctn-defn (fctn-decl ,name ,iargs ,oargs . ,_) ,stmts)
       (values tree '() gbl (map cadr (append (cdr iargs) (cdr iargs)))))
      ((command "global" . ,args)
       (values tree '() (fold cadr gbl args) lcl))
      (,__ (values tree '() gbl lcl))))

  (define (fU tree seed gbl lcl kseed kgbl klcl) ; => seed gbl lcl
    (sx-match tree
      (() (values (cons (reverse kseed) seed) gbl lcl))
      ((aref-or-call (@ . ,attr) (ident ,name) . ,rest)
       (let ((op (if (or (member name lcl) (member name gbl))
                     'array-ref 'call))
             (tail (cdr (reverse kseed))))
         (values (cons (cons-source tree op tail) seed) gbl lcl)))
      ((aref-or-call (@ . ,attr) (handle (ident ,name)) . ,rest)
       (let ((tail (cdr (reverse kseed))))
         (values (cons (cons-source tree 'call tail) seed) gbl lcl)))
      ((colon-expr . ,rest)
       (let ((form (reverse kseed)))
         (values (if (fixed-colon-expr? tree)
                     (cons-source tree 'fixed-colon-expr (cdr form))
                     form)) gbl lcl))
      ((matrix . ,rest)
       (values (cons (check-matrix (reverse kseed)) seed) gbl lcl))
      ((fctn-defn (fctn-decl ,name ,iargs ,oargs . ,_) ,stmts)
       (values (cons (reverse kseed) seed) gbl '()))
      (,__
       (values (cons (reverse kseed) seed) gbl lcl))))
  
  (define (fH leaf seed gbl lcl)
    (values (cons leaf seed) gbl lcl))
  
  (cadar (foldts*-values fD fU fH `(*TOP* ,tree) '() '() '())))


;; === file parser 

(include-from-path "language/nx-mlang/mach.d/mlang-tab.scm")
(include-from-path "language/nx-mlang/mach.d/mlang-act.scm")

(define gen-mlang-lexer (make-mlang-lexer-generator mlang-mtab))

;; Parse given a token generator.
(define raw-parser
  (make-lalr-parser
   (acons 'act-v mlang-act-v mlang-tables)
   #:skip-if-unexp '($code-comm $lone-comm sp "\n")))

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
          (let ((sx (parse-mlang #:debug #f))
                (fn (port-filename port)))
            (if (and sx fn)
                (cons* (car sx) `(@ (filename ,fn)) (cdr sx))
                sx))))))

;; === interactive parser

(include-from-path "language/nx-mlang/mach.d/mlangia-tab.scm")
(include-from-path "language/nx-mlang/mach.d/mlangia-act.scm")

(define raw-ia-parser
  (make-lalr-parser
   (acons 'act-v mlangia-act-v mlangia-tables)
   #:skip-if-unexp '($code-comm $lone-comm sp)
   #:interactive #t))

(define (parse-mlang-stmt lexer)
  (catch 'nyacc-error
    (lambda ()
      (apply-mlang-statics (raw-ia-parser lexer #:debug #f)))
    (lambda (key fmt . args)
      (apply simple-format (current-error-port) fmt args)
      (newline (current-error-port))
      #f)))

(define gen-mlang-ia-lexer
  (make-mlang-lexer-generator mlangia-mtab #:interactive #t))

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
