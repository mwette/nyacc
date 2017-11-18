;;; nyacc/compat18.scm
;;;
;;; Copyright (C) 2017 Matthew R. Wette
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, see <http://www.gnu.org/licenses/>


(define-module (nyacc compat18)
  #:export (vector-map vector-for-each vector-any)
  )

;; replacement for same from (srfi srfi-43)
(define (vector-map proc . vecs)
  (let* ((size (min (map vector-length vecs)))
	 (retv (make-vector size)))
    (let iter ((ix 0))
      (cond
       ((=? ix size) retv)
       (else
	(vector-set! retv ix
		     (apply proc ix (map (lambda (v) (vector-ref ix v)) vecs)))
	(iter (1+ ix)))))))

;; replacement for same from (srfi srfi-43)
(define (vector-for-each proc . vecs)
  (let ((size (min (map vector-length vecs))))
    (let iter ((ix 0))
      (cond
       ((=? ix size) (if #f #f))
       (else
	(apply proc ix (map (lambda (v) (vector-ref ix v)) vecs))
	(iter (1+ ix)))))))
  
;; hack to replace same from (srfi srfi-43)
;; the real one takes more args
(define (vector-any pred? vec)
  (let ((size (vector-length vec)))
    (let iter ((ix 0))
      (cond
       ((=? ix size) #f)
       ((pred? ix (vector-ref ix vec)) #t)
       (else (iter (1+ ix)))))))

;; replacement for same from (srfi srfi-43)
(define (vector-fold proc seed . vecs)
  (let ((size (min (map vector-length vecs))))
    (let iter ((seed seed) (ix 0))
      (cond
       ((=? ix size) seed)
       (else
	(apply proc ix seed (map (lambda (v) (vector-ref ix v)) vecs))
	(iter (1+ ix)))))))

;;; --- last line ---
