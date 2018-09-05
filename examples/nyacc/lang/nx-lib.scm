;; nyacc/lang/nx-util.scm - run-time library

;; Copyright (C) 2018 Matthew R. Wette
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Description:

;; This module provide run-time procecures for the NYACC extension (nx)
;; languages.  The intent is to provide a consistent data model between
;; nx languages so that they can inter-operate.

;;; Notes:

;; For OO languages we should use single-inheritance with interfaces.
;; An object is a hash table with entries for data and either
;;   A) a single class (type) entry, or
;;   B) one class (type) entry and N interface entries
;; This module will need to provide run-time type determination.  Well,
;; We need a procedure (obj-call obj name args)
;;
;; Idea: for each object add a lang-specific field to decorate
;; (hashq-ref* obj 'lang 'nx-javascript) => ...

;; @subheading Object Architecture
;; The principles are
;; @itemize
;; @item campatability among many languages important
;; @item strict language adherence is not priority
;; @item base Scheme compatiblity is priority
;; @item speed is not high priority
;; @end itemize

;;; Todos:

;;  1) add traits (aka interfaces)

;;; Code:

(define-module (nyacc lang nx-lib)
  #:export (nx-get-method
	    install-inline-language-evaluator
	    uninstall-inline-language-evaluator
	    read-inline-code
	    )
  )
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(use-modules (ice-9 pretty-print))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

;; @deffn {Procedure} nx-get-method obj name
;; find a 
;; @end deffn 
(define (nx-get-method obj name)
  #f)

(define fooo 1)

(use-modules (system base language))
(use-modules (system base compile))
(use-modules (language tree-il))

;; #<nx-octave: a = [1, 2]; >#
;; this needs to return a scheme expression
;; so probably use reader to convert tree-il
;; then convert to scheme
(define (read-inline-code reader-char port)
  (let* ((str-port (open-output-string))
	 (name (let loop ((chl '()) (ch (read-char port)))
		 (cond
		  ((eof-object? ch) ch)
		  ((char=? ch #\:) (reverse-list->string chl))
		  (else (loop (cons ch chl) (read-char port))))))
	 (code (let loop ((ch (read-char port)))
		 (cond
		  ((eof-object? ch) (error "oops"))
		  ((char=? ch #\>)
		   (let ((ch1 (read-char port)))
		     (cond
		      ((eof-object? ch) (error "oops"))
		      ((char=? ch1 #\#) (get-output-string str-port))
		      (else (display ch str-port) (loop ch1)))))
		  (else
		   (display ch str-port)
		   (loop (read-char port))))))
	 ;;
	 (lang (lookup-language (string->symbol name)))
	 (lread (and lang (language-reader lang)))
	 (lcomp (and lang (assq-ref (language-compilers lang) 'tree-il)))
	 ;;
	 (sxml (and lread
		    (call-with-input-string code
		      (lambda (port) (lread port (current-module))))))
	 (itil (and lcomp
		    (call-with-values
			(lambda () (lcomp sxml (current-module) '()))
		      (lambda (exp env cenv) exp))))
	 (xtil (unparse-tree-il itil))
	 (scm (decompile itil))
	 )
    (unless lang (error "no such language:" name))
    #;(call-with-input-string code
      (let loop ((block '(begin)) (sxml (lread port (current-module))))
	(if (eof-object? sxml)
	    (reverse block)
	    (loop (cons sxml block)
		  (lread port (current-module))))))
    ;;(when sxml (sferr "<sxml:\n") (pperr sxml))
    ;;(when xtil (sferr "<xtil:\n") (pperr xtil))
    ;;(when scm (sferr "<scm:\n") (pperr scm))
    ;;code
    scm))

(define (install-inline-language-evaluator)
  (read-hash-extend #\< read-inline-code))

(define (uninstall-inline-language-evaluator)
  (read-hash-extend #\< #f))

;; --- last line ---
