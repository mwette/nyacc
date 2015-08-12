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

;; C preprocessor (an admitted hack, OK?)

(define-module (lang c cpp)
  #:export (parse-cpp-line eval-cpp-expr)
  #:use-module (nyacc lex)
  #:use-module (lang util)
  ;;#:use-module ((srfi srfi-1) #:select (last remove))
  ;;#:use-module ((srfi srfi-9) #:select (define-record-type))
  ;;#:use-module ((sxml fold) #:select (foldts*-values foldts))
  ;;#:use-module ((sxml xpath) #:select (sxpath))
  ;;#:use-module (ice-9 pretty-print)
  )

(define (fmterr fmt . args)
  (apply simple-format (current-error-port) fmt args))

#|
  #define  #undef  #include  #if  #ifdef  #ifndef  #else  #endif  #elif
  #line  defined  # <operator>  ## <operator>  #pragma  #error

  need mini-cpp and full-cpp
  mini-cpp:
  don't expand macro calls -- treat like function calls, but provide dict
  if ifdef args not defined hmmm -- for now make sure they are
     ... need script to get #defines from cc
  provide dict of #defines
|#

(define (skip-ws ch)
  (if (eof-object? ch) ch
      (if (char-set-contains? c:ws ch)
	  (skip-ws (read-char))
	  ch)))

;; grammar:
;; expr   => expr == equal | equal
;; equal  => equal || term | term
;; term   => term && factor | factor
;; factor => defined '(' ident ')' | const
;; TO
;; expr => equal expr'
;; expr' => == equal | $eps
;; equal => term equal'
;; equal' => || term | $eps
;; term => factor term'
;; term' => && factor | $eps
;; factor => defined | const
(define (parse-cpp-expr)
  ;; defined(X) && define(Y) ...
  (letrec
      ((p-expr
	(lambda (la)
	  (let* ((equal (p-equal la)) (la1 (read-char))
		 (expr1 (if (eof-object? la1) #f (p-expr1 la1))))
	    ;;(simple-format #t "p-expr la=~S\n" la)
	    (if expr1 (cons equal expr1) equal))))
       (p-expr1
	(lambda (la)
	  #f))
       (p-equal
	(lambda (la)
	  (let* ((term (p-term la)) (la1 (if term (read-char) la))
		 (equal1 (if (eof-object? la1) #f (p-equal1 la1))))
	    ;;(simple-format #t "p-equal la=~S\n" la)
	    (if equal1 (cons term equal1) term))))
       (p-equal1
	(lambda (la)
	  #f))
       (p-term
	(lambda (la)
	  (let* ((factor (p-factor la)) (la1 (if factor (read-char) la))
		 (term1 (if (eof-object? la1) #f (p-term1 la1))))
	    ;;(simple-format #t "p-term la=~S factor=~S\n" la factor)
	    (if term1 (cons factor term1) factor))))
       (p-term1
	(lambda (la)
	  #f))
       (p-factor
	(lambda (la)
	  (let ((la1 (skip-ws la)))
	    (cond
	     ((p-cnst la1))
	     ((p-defd la1))
	     ((read-c-ident la1) => (lambda (id) `(ident ,id)))
	     (else #f)))))
       (p-defd		    ; "defined(IDENT)" => '(defined_p "IDENT")
 	(let ((rd-defd (make-chseq-reader '(("defined" . defined_p)))))
	  (lambda (la)
	    (and
	     (rd-defd la)
	     (eq? (read-char) #\()
	     (let ((ident (read-c-ident (read-char))))
	       (and (eq? (read-char) #\))
		    `(defined_p ,ident)))))))
       (p-cnst
	(lambda (la)
	  (let ((num (read-c-num la)))
	    (if num `(num ,(cdr (read-c-num la))) #f))))
       )
    (p-expr (read-char))))

;; @item eval-cpp-expr tree dict => value
;; evaluate a CPP expression
(define (eval-cpp-expr tree dict)
  (let* ()
    (case (car tree)
      ((num)
       (string->number (cadr tree)))
      ((ident) ;; ref
       (let* ((repl (assoc-ref dict (cadr tree))) ; replacement
	      (tree (and repl (with-input-from-string repl parse-cpp-expr)))
	      (value (and tree (eval-cpp-expr tree dict))))
	 ;; returns value of #f if not ident not defined
	 value))
      ((not)
       (let ((arg (eval-cpp-expr (cadr tree) dict)))
	 (if (eq? arg #f) #f
	     (if (zero? arg) 1 0))))
      ((defined_p)
       (let ((ident (string->symbol (cadr tree))))
	 (if (assq-ref dict ident) 1 0)))
      (else #f))))

;; @item cpp-define => #f|???
(define (cpp-define)
  ;; The (broken) parse architecture is "unread la argument if no match"
  (letrec
      ((p-cppd ;; parse all
	(lambda ()
	  (let* ((iden (read-c-ident (skip-ws (read-char))))
		 (args (or (p-args (skip-ws (read-char))) '()))
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

(define (cpp-include)
  (let* ((beg-ch (skip-ws (read-char)))
	 (end-ch (if (eq? beg-ch #\<) #\> #\"))
	 (path (let iter ((cl (list beg-ch)) (ch (read-char)))
		 (if (eq? ch end-ch) (list->string (reverse (cons ch cl)))
		     (iter (cons ch cl) (read-char))))))
    `(include ,path)))

(define (parse-cpp-line line)
  (with-input-from-string line
    (lambda ()
      (let ((cmd (string->symbol (read-c-ident (skip-ws (read-char)))))
	    (rd-ident (lambda () (read-c-ident (skip-ws (read-char))))))
	(cons
	 'cpp-stmt
	 (case cmd
	   ((include) (cpp-include))
	   ((ifdef) `(if (defined_p ,(rd-ident))))
	   ((ifndef) `(if (not (defined_p ,(rd-ident)))))
	   ((define) (cpp-define))
	   ((if elseif) (list cmd (parse-cpp-expr)))
	   ((else endif) (list cmd))
	   (else '())))))))
    
;; mode: scheme ***
;; --- last line
