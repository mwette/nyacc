;;; nyacc/lang/calc/compiler

;; Copyright (C) 2015,2018 Matthew R. Wette
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

(define-module (nyacc lang calc compiler)
  #:export (compile-tree-il)
  #:use-module (sxml match)
  #:use-module (sxml fold)
  #:use-module (language tree-il))
(use-modules (ice-9 pretty-print))

(define (show val)
  `(begin (apply (toplevel display) ,val) (apply (toplevel newline))))

(define (fup tree)
  (sxml-match tree
    ((fixed ,fx) `(const ,(string->number fx)))
    ((float ,fl) `(const ,(string->number fl)))
    ((ident ,id) `(toplevel ,(string->symbol id)))
    ((add ,lt ,rt) `(apply (toplevel +) ,lt ,rt))
    ((sub ,lt ,rt) `(apply (toplevel -) ,lt ,rt))
    ((mul ,lt ,rt) `(apply (toplevel *) ,lt ,rt))
    ((div ,lt ,rt) `(apply (toplevel /) ,lt ,rt))
    ((assn-stmt ,lhs ,rhs) `(begin (define ,lhs ,rhs) ,(show lhs)))
    ((expr-stmt ,expr) (show expr))
    ((empty-stmt) '(begin))
    ((program . ,stmt-list) `(begin . ,stmt-list))
    (,otherwise tree)))

(define (compile-tree-il exp env opts)
  (let* ((tree (foldt fup identity exp))
 	 (code (parse-tree-il tree)))
      (values code env env)))

;; --- last line ---
