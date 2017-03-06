;;; nyacc/lang/calc/compiler
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

(define-module (nyacc lang calc compiler)
  #:export (compile-tree-il)
  #:use-module (sxml match)
  #:use-module (sxml fold)
  #:use-module (language tree-il))

(define (fup tree)
  (sxml-match tree
    ((fixed ,fx) `(const ,(string->number fx)))
    ((float ,fl) `(const ,(string->number fl)))
    ((ident ,id) `(toplevel ,(string->symbol id)))
    ((add ,lt ,rt) `(apply (toplevel +) ,lt ,rt))
    ((sub ,lt ,rt) `(apply (toplevel -) ,lt ,rt))
    ((mul ,lt ,rt) `(apply (toplevel *) ,lt ,rt))
    ((div ,lt ,rt) `(apply (toplevel /) ,lt ,rt))
    ((assn-stmt (toplevel ,lhs) ,rhs) `(define ,lhs ,rhs))
    ((empty-stmt) '(begin))
    ((stmt-list ,items ...) `(begin ,items ...))
    (,otherwise tree)))

(define (copmile-tree-il exp env opts)
  (let* ((tree (foldt fup identity exp))
	 (code (parse-tree-il tree)))
    (values code env env)))

;; --- last line ---
