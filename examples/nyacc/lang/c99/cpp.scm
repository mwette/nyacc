;;; lang/c/cpp.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by 
;;; the Free Software Foundation, either version 3 of the License, or 
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; C preprocessor.  This is not complete.

(define-module (nyacc lang c99 cpp)
  #:export (parse-cpp-line
	    eval-cpp-expr
	    ;;
	    n-parse-cpp-expr
	    )
  ;;#:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc lang util)
  #:use-module (rnrs arithmetic bitwise)
  )

#|
  #define  #undef  #include  #if  #ifdef  #ifndef  #else  #endif  #elif
  #line  defined  #-operator  ##-operator  #pragma  #error

strategy:
  don't expand macro calls -- treat like function calls, but provide dict
todo:
  pragma
  #-op ##-op
  provide dict of #defines
  provide util to expand defines
|#

;;.@item skip-ws ch
;; Helper for 
(define (skip-ws ch)
  (if (eof-object? ch) ch
      (if (char-set-contains? c:ws ch)
	  (skip-ws (read-char))
	  ch)))
;; @item cpp-define => #f|???
(define (cpp-define)
  ;; The (broken) parse architecture is "unread la argument if no match"
  (letrec
      ((p-cppd ;; parse all
	(lambda ()
	  (let* ((iden (read-c-ident (skip-ws (read-char))))
		 ;;(args (or (p-args (skip-ws (read-char))) '()))
		 ;; "define ABC(ARG)" not the same as "define ABC (ARG)"
		 (args (or (p-args (read-char)) '()))
		 (amap (lambda (as) (map (lambda (a) (list 'arg a)) as)))
		 (rest (or (p-rest (skip-ws (read-char))) " ")))
	    (if (pair? args)
		`(define (name ,iden) ,(cons 'args (amap args)) (repl ,rest))
		`(define (name ,iden) (repl ,rest))))))
       (p-args ;; parse args
	(lambda (la) ;; unread la if no match :(
	  (if (eq? la #\()
	      (let iter ((args '()) (la (skip-ws (read-char))))
		(cond
		 ((eq? la #\)) (reverse args))
		 ((read-c-ident la) =>
		  (lambda (arg) (iter (cons arg args) (skip-ws (read-char)))))
		 ((eq? la #\,)
		  (iter args (skip-ws (read-char))))))
	      (begin (if (char? la) (unread-char la)) #f)))) ;; CLEANUP
       (p-rest ;; parse rest
	(lambda (la)
	  (cond ((char? la) (unread-char la) (drain-input (current-input-port)))
		(else #f)))))
    (p-cppd)))

;; @item cpp-include
;; Parse CPP include statement.
(define (cpp-include)
  (let* ((beg-ch (skip-ws (read-char)))
	 (end-ch (if (eq? beg-ch #\<) #\> #\"))
	 (path (let iter ((cl (list beg-ch)) (ch (read-char)))
		 (if (eq? ch end-ch) (list->string (reverse (cons ch cl)))
		     (iter (cons ch cl) (read-char))))))
    `(include ,path)))

;; @item parse-cpp-line line => tree
;; Parse a line from a CPP statement and return a parse tree.
;; @example
;; (parse-cpp-line "define X 123") => (define "X" "123")
;; @end example
(define (parse-cpp-line line)
  (with-input-from-string line
    (lambda ()
      (let ((cmd (string->symbol (read-c-ident (skip-ws (read-char)))))
	    (rd-ident (lambda () (read-c-ident (skip-ws (read-char)))))
	    (rd-num (lambda () (read-c-num (skip-ws (read-char)))))
	    (rd-str (lambda () (read-c-string (skip-ws (read-char))))))
	 (case cmd
	   ((include) (cpp-include))
	   ((ifdef) `(if (defined ,(rd-ident))))
	   ((ifndef) `(if (not (defined ,(rd-ident)))))
	   ((define) (cpp-define))
	   ((if elif) (list cmd (parse-cpp-expr)))
	   ((else endif) (list cmd))
	   ((undef) `(undef ,(rd-ident)))
	   ((line) `(line ,(rd-num)))
	   ((error) `(error ,(rd-str)))
	   ;;((pragma) (cpp-define)) ; ???
	   (else '()))))))
    
;;(include "cpptab.scm")
(include-from-path "nyacc/lang/c99/cpptab.scm")
;;(include "cppact.scm")
(include-from-path "nyacc/lang/c99/cppact.scm")
(define raw-parser
  (make-lalr-parser
   (list
    (cons 'len-v len-v)
    (cons 'pat-v pat-v)
    (cons 'rto-v rto-v)
    (cons 'mtab mtab)
    (cons 'act-v act-v))))
(define gen-cpp-lexer (make-lexer-generator mtab))

;; @item parse-cpp-expr => 
;; A thunk that reads from default input and returns a parse tree.
(define (parse-cpp-expr) (raw-parser (gen-cpp-lexer)))

;; @item eval-cpp-expr tree dict => value
;; Evaluate a CPP expression tree returned from @code{parse-cpp-expr}.
(define (eval-cpp-expr tree dict)
  (letrec
      ((tx (lambda (tr ix) (list-ref tr ix)))
       (tx1 (lambda (tr) (tx tr 1)))
       (ev (lambda (ex ix) (eval-expr (list-ref ex ix))))
       (ev1 (lambda (ex) (ev ex 1)))
       (ev2 (lambda (ex) (ev ex 2)))
       (ev3 (lambda (ex) (ev ex 3)))
       (parse-and-eval
	(lambda (str)
	  (if (not (string? str)) (throw 'error))
	  (let ((idtr (with-input-from-string str parse-cpp-expr)))
	    (eval-cpp-expr idtr dict))))
       (eval-expr
	(lambda (tree)
	  (case (car tree)
	    ((ident) (parse-and-eval (assoc-ref dict (tx1 tree))))
	    ((fixed) (string->number (tx1 tree)))
	    ((char) (char->integer (tx1 tree)))
	    ((defined) (if (assoc-ref dict (tx1 tree)) 1 0))
	    ;;
	    ((pre-inc post-inc) (1+ (ev1 tree)))
	    ((pre-dec post-dec) (1- (ev1 tree)))
	    ((pos) (ev1 tree))
	    ((neg) (- (ev1 tree)))
	    ((bw-not) (bitwise-not (ev1 tree)))
	    ((not) (if (zero? (ev1 tree)) 1 0))
	    ((mul) (* (ev1 tree) (ev2 tree)))
	    ((div) (/ (ev1 tree) (ev2 tree)))
	    ((mod) (modulo (ev1 tree) (ev2 tree)))
	    ((add) (+ (ev1 tree) (ev2 tree)))
	    ((sub) (- (ev1 tree) (ev2 tree)))
	    ((lshift) (bitwise-arithmetic-shift-left (ev1 tree) (ev2 tree)))
	    ((rshift) (bitwise-arithmetic-shift-right (ev1 tree) (ev2 tree)))
	    ((lt) (if (< (ev1 tree) (ev2 tree)) 1 0))
	    ((le) (if (<= (ev1 tree) (ev2 tree)) 1 0))
	    ((gt) (if (> (ev1 tree) (ev2 tree)) 1 0))
	    ((ge) (if (>= (ev1 tree) (ev2 tree)) 1 0))
	    ((equal) (if (= (ev1 tree) (ev2 tree)) 1 0))
	    ((noteq) (if (= (ev1 tree) (ev2 tree)) 0 1))
	    ((bw-or) (bitwise-ior (ev1 tree) (ev2 tree)))
	    ((bw-xor) (bitwise-xor (ev1 tree) (ev2 tree)))
	    ((bw-and) (bitwise-and (ev1 tree) (ev2 tree)))
	    ((or) (if (and (zero? (ev1 tree)) (zero? (ev2 tree))) 0 1))
	    ((and) (if (or (zero? (ev1 tree)) (zero? (ev2 tree))) 0 1))
	    ((cond-expr) (if (zero? (ev1 tree)) (ev3 tree) (ev2 tree)))
	    (else (error "incomplete implementation"))))))
    (catch 'error
	   (lambda () (eval-expr tree))
	   (lambda () #f))))

;; mode: scheme ***
;; --- last line ---
