;;; nyacc/lang/tcl/pprint.scm

;; Copyright (C) 2020 Matthew R. Wette
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

(define-module (nyacc lang tcl pprint)
  #:export (pretty-print-tcl)
  #:use-module ((srfi srfi-1) #:select (pair-for-each fold-right))
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
  #:use-module (ice-9 pretty-print)
  )

(define* (pretty-print-tcl tree #:key (indent-level 2))

  (define fmtr (make-pp-formatter))
  (define (push-il) (fmtr 'push))
  (define (pop-il) (fmtr 'pop))
  
  (define sf (lambda args (apply fmtr args)))
  
  (define (ppx tree)
    (sx-match tree

      ((string ,val)
       (sf "~S" val))
      
      ((number ,rep)
       (sf "~A" rep))
      
      ((comment ,text)
       (sf "# ~A" text))

      ((deref ,arg)			; check string protect
       (sf "$~A" arg))

      ((word . ,forms)
       (sf "[todo word]"))

      ((void))

      ((expr . ,forms)
       (sf "expr ")
       (pair-for-each
	(lambda (pair)
	  (ppx (car pair))
	  (if (pair? (cdr pair)) (sf " ")))
	forms))

      ((set ,var ,val)
       (sf "set ") (ppx var) (sf " ") (ppx val))

      ((if ,cndx ,body . ,rest)
       (sf "if ") (ppx cndx) (sf " ") (ppx body)
       (for-each
	(lambda (form)
	  (case (sx-tag form)
	    ((else)
	     (sf " else ") (ppx (sx-ref form 1)))
	    ((elseif)
	     (sf " elseif ") (ppx (sx-ref form 1)) (sf " ")
	     (ppx (sx-ref form 2)))
	    (else (error "yuck"))))
	rest))

      ((arg-list . ,args)
       (pair-for-each
	(lambda (pair)
	  (sf "~A" (cadar pair))
	  (if (pair? (cdr pair)) (sf " ")))
	args))

      ((return) (sf "return"))
      ((return ,arg) (sf "return ") (ppx arg))

      ((proc ,name ,args ,body)
       (sf "proc ~A {" name) (ppx args) (sf "} ") (ppx body))

      ((body . ,forms)
       (push-il) (sf "{\n")
       (pair-for-each
	(lambda (pair) (ppx (car pair)) (if (pair? (cdr pair)) (sf "\n")))
	forms)
       (pop-il) (sf "\n}"))
      
      ((unit . ,forms)
       (for-each (lambda (form) (ppx form) (sf "\n")) forms))

      (,_ (simple-format #t "\n*** NOT HANDLED: ~S\n" (car tree)))))

  (ppx tree))

;; --- last line ---
