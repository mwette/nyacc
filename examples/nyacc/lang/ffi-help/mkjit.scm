;; mkjit.scm

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

;; convert GNU lightning macros to Scheme functions
;; see output in exmamples/ffi/lightning.ffi

(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang c99 util))
(use-modules (nyacc lang util))
(use-modules (nyacc lang sx-util))
(use-modules (nyacc lex))
(use-modules (nyacc util))
(use-modules (ice-9 pretty-print))
(use-modules ((sxml xpath) #:select (sxpath)))

(define (sferr fmt . args) (apply simple-format #t fmt args))
(define (pperr exp) (pretty-print exp #:per-line-prefix ""))

(define cpp-defs
  (append
   '("__restrict=restrict")
   (get-gcc-cpp-defs)))

(define inc-dirs
  (append
   '("/usr/include")
   (get-gcc-inc-dirs)))

(define inc-help
  (cond
   ((string-contains %host-type "darwin")
    '(("__builtin" "__builtin_va_list=void*"
       "__attribute__(X)="
       "__inline=" "__inline__="
       "__asm(X)=" "__asm__(X)="
       "__has_include(X)=__has_include__(X)"
       "__extension__=" "__signed=signed")))
   (else
    '(("__builtin"
       "__builtin_va_list=void*" "__attribute__(X)="
       "__inline=" "__inline__="
       "__asm(X)=" "__asm__(X)="
       "__has_include(X)=__has_include__(X)"
       "__extension__=")))))

;; for defining macros.  Let's see how far we can go here ...
(define (c99->scm expr)
  (sx-match expr
    ((fctn-call (p-expr (ident ,name)) (expr-list . ,args))
     (cons (string->symbol name) (map c99->scm args)))
    ((p-expr (ident ,name)) (string->symbol name))
    ((p-expr (fixed ,value)) (string->number (cnumstr->scm value)))
    ((and ,l ,r) `(and ,(c99->scm l) ,(c99->scm r)))
    ((le ,l ,r) `(<= ,(c99->scm l) ,(c99->scm r)))
    ((ge ,l ,r) `(>= ,(c99->scm l) ,(c99->scm r)))
    ((bitwise-and ,l ,r) `(logand ,(c99->scm l) ,(c99->scm r)))
    (,_ (sferr "can't handle this:\n") (pperr expr))))

(define (cnvt)
  (define (cnvt-cpp-ftn-def defn)
    (let* ((name (car defn)) (argl (cadr defn)) (repl (cddr defn))
	   (expr (parse-c99x repl))
	   (name (string->symbol name)) (argl (map string->symbol argl)))
      ;;(sferr "~S:\n" name)  (pperr expr)
      (sferr "\n")
      (pperr `(define (,name ,@argl) ,(c99->scm expr)))))
  (define (inc-filter file-spec path-spec)
    (string-contains path-spec "lightning"))
  (let* ((tree (with-input-from-string "#include <lightning.h>\n"
		 (lambda ()
		   (parse-c99 #:cpp-defs cpp-defs
			      #:inc-dirs inc-dirs
			      #:inc-help inc-help
			      #:mode 'decl))))
	 (ddict (c99-trans-unit->ddict tree #:inc-filter inc-filter)))
    (for-each
     (lambda (defn)
       (cond
	((string=? (car defn) "offsetof") #f)
	((pair? (cdr defn)) (cnvt-cpp-ftn-def defn))))
     ddict)))

(cnvt)

;; -- last line ---
