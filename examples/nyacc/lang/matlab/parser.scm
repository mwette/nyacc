;; nyacc/lang/matlab/parser.scm

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

(define-module (nyacc lang matlab parser)
  #:export (parse-ml ml-stmt-reader ml-file-reader)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util))

(include-from-path "nyacc/lang/matlab/body.scm")

;; === static semantics

;; 1) assn: "[ ... ] = expr" => multi-assign
;; 2) matrix: "[ int, int, int ]" => ivec
;; 3) matrix: "[ ... (fixed) ... ]" => error
;; .) colon-expr => fixed-colon-expr

(use-modules (nyacc lang sx-util))
(use-modules (sxml fold))
(use-modules ((srfi srfi-1) #:select (fold)))
(use-modules (ice-9 pretty-print))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr exp) (pretty-print exp (current-error-port)))

(define (fixed-colon-expr? expr)
  (sx-match expr
    ((colon-expr (fixed ,s) (fixed ,e)) #t)
    ((colon-expr (fixed ,s) (fixed ,i) (fixed ,e)) #t)
    (else #f)))

(define (fixed-expr? expr)
  (define (fixed-primary-expr? expr)
    (sx-match expr
      ((fixed ,val) #t)
      ((add ,lt ,rt) (and (fixed-expr? lt) (fixed-expr? rt)))
      ((sub ,lt ,rt) (and (fixed-expr? lt) (fixed-expr? rt)))
      ((mul ,lt ,rt) (and (fixed-expr? lt) (fixed-expr? rt)))
      (else #f)))
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
      (else #f)))
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
     ((and (= 1 nrow) (fixed-vec? row1)) `(fixed-vector . ,(cdr row1)))
     ((float-mat? mat) `(float-matrix . ,(cdr mat)))
     (else mat))))
	  
;; @deffn {Procedure} update-matlab-tree tree => tree
;; change find multiple value assignment and change to @code{multi-assn}.
;; @end deffn
(define (update-matlab-tree tree)

  (define (fU tree)
    (sx-match tree
      ((assn (matrix (row . ,elts)) ,rhs)
       `(multi-assn (lval-list . ,elts) ,rhs))
      ((colon-expr . ,rest)
       (if (fixed-colon-expr? tree) `(fixed-colon-expr ,(cdr tree)) tree))
      ((matrix . ,rest)
       (check-matrix tree))
      (else tree)))
  
  (define (fH tree)
    tree)
  
  (cadr (foldt fU fH `(*TOP* ,tree))))

;; === file parser 

(include-from-path "nyacc/lang/matlab/mach.d/mltab.scm")
(include-from-path "nyacc/lang/matlab/mach.d/mlact.scm")

(define gen-matlab-lexer (make-matlab-lexer-generator ml-mtab))

;; Parse given a token generator.
(define raw-parser
  (make-lalr-parser 
   (list
    (cons 'len-v ml-len-v)
    (cons 'pat-v ml-pat-v)
    (cons 'rto-v ml-rto-v)
    (cons 'mtab ml-mtab)
    (cons 'act-v ml-act-v))))

(define* (parse-ml #:key debug)
  (catch
   'parse-error
   (lambda ()
     (raw-parser (gen-matlab-lexer) #:debug debug))
   (lambda (key fmt . args)
     (apply simple-format (current-error-port) fmt args)
     #f)))

(define (ml-file-reader port env)
  (with-input-from-port port
    (lambda ()
      (display "parse file\n")
      (if (eof-object? (peek-char port))
	  (read-char port)
	  (update-matlab-tree
	   (parse-ml #:debug #f))))))

;; === interactive parser

(include-from-path "nyacc/lang/matlab/mach.d/ia-mltab.scm")
(include-from-path "nyacc/lang/matlab/mach.d/ia-mlact.scm")

(define gen-ia-matlab-lexer (make-matlab-lexer-generator ia-ml-mtab))

(define raw-ia-parser
  (make-lalr-parser
   (list (cons 'len-v ia-ml-len-v) (cons 'pat-v ia-ml-pat-v)
	 (cons 'rto-v ia-ml-rto-v) (cons 'mtab ia-ml-mtab)
	 (cons 'act-v ia-ml-act-v))
   #:interactive #t))

(define (parse-ml-stmt lexer)
  (catch 'nyacc-error
    (lambda ()
      (raw-ia-parser lexer #:debug #f))
    (lambda (key fmt . args)
      (apply simple-format (current-error-port) fmt args)
      #f)))

(define ml-stmt-reader
  (let ((lexer (gen-ia-matlab-lexer)))
    (lambda (port env)
      (cond
       ((eof-object? (peek-char port))
	(read-char port))
       (else
	(let* ((stmt (with-input-from-port port
		      (lambda () (parse-ml-stmt lexer))))
	       (stmt (update-matlab-tree stmt)))
	  (cond
	   ((equal? stmt '(empty-stmt)) #f)
	   (stmt)
	   ;;(else (flush-input-after-error port) #f)
	   )))))))


;; ... (assn (
;;

;; --- last line ---
