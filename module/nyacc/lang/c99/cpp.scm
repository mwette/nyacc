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
  #:export (parse-cpp-stmt
	    parse-cpp-expr
	    eval-cpp-expr
	    expand-cpp-def
	    )
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
  ;; The (weak?) parse architecture is "unread la argument if no match"
  (letrec
      ((p-cppd ;; parse all
	(lambda ()
	  (let* ((iden (read-c-ident (skip-ws (read-char))))
		 ;;(args (or (p-args (skip-ws (read-char))) '()))
		 ;; "define ABC(ARG)" not the same as "define ABC (ARG)"
		 (args (or (p-args (read-char)) '()))
		 (rest (or (p-rest (skip-ws (read-char))) " ")))
	    (if (pair? args)
		`(define (name ,iden) ,(cons 'args args) (repl ,rest))
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

;; @item parse-cpp-stmt line => tree
;; Parse a line from a CPP statement and return a parse tree.
;; @example
;; (parse-cpp-stmt "define X 123") => (define "X" "123")
;; (parse-cpp-stmt "if defined(A) && defined(B) && defined(C)"
;; => (if (and (and (defined "A") (defined "B"))
;;             (not (defined "C")))
;; @end example
(define (parse-cpp-stmt line)
  (with-input-from-string line
    (lambda ()
      (let ((cmd (string->symbol (read-c-ident (skip-ws (read-char)))))
	    (rd-ident
	     #;(lambda () (or (and=> (read-c-ident (skip-ws (read-char))) cdr)
	     (throw 'parse-error)))
	     (lambda () (read-c-ident (skip-ws (read-char))))
	     )
	    (rd-num
	     (lambda () (or (and=> (read-c-num (skip-ws (read-char))) cdr)
			    (throw 'parse-error))))
	    )
	 (case cmd
	   ((include) (cpp-include))
	   ((ifdef) `(if (defined ,(rd-ident))))
	   ((ifndef) `(if (not (defined ,(rd-ident)))))
	   ((define) (cpp-define))
	   ((if elif) (list cmd (parse-cpp-expr)))
	   ((else endif) (list cmd))
	   ((undef) `(undef ,(rd-ident)))
	   ((line) `(line ,(rd-num)))
	   ((error) (read-char) `(error ,(drain-input (current-input-port))))
	   ;;((pragma) (cpp-define)) ; ???
	   (else '()))))))
    
(include-from-path "nyacc/lang/c99/mach.d/cpptab.scm")
(include-from-path "nyacc/lang/c99/mach.d/cppact.scm")

(define raw-parser
  (make-lalr-parser
   (list (cons 'len-v len-v) (cons 'pat-v pat-v) (cons 'rto-v rto-v)
	 (cons 'mtab mtab) (cons 'act-v act-v))))

;; cppbody provides: gen-cpp-lexer parse-cpp-expr eval-cpp-expr
(include-from-path "nyacc/lang/c99/cppbody.scm")
 
;; mode: scheme ***
;; --- last line ---
