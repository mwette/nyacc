;;; nyacc/util.scm
;;;
;;; Copyright (C) 2014-2016 Matthew R. Wette
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
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (nyacc util)
  #:export (
	    fmtstr fmtout fmterr fmt
	    wrap-action
	    obj->str
	    fixed-point prune-assoc
	    map-attr->vector
	    x-flip x-comb
	    write-vec
	    ugly-print
	    tzort
	    )
  #:use-module ((srfi srfi-43) #:select (vector-fold))
  )

(define (fmtstr fmt . args)
  (apply simple-format #f fmt args))
(define (fmtout fmt . args)
  (apply simple-format (current-output-port) fmt args))
(define (fmterr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define fmt simple-format)

;; @item make-arg-list N => '($N $Nm1 $Nm2 ... $1 . $rest)
;; This is a helper for @code{mkact}.
(define (make-arg-list n)
  (let ((mkarg
	 (lambda (i) (string->symbol (string-append "$" (number->string i))))))
    (let iter ((r '(. $rest)) (i 1))
      (if (> i n) r (iter (cons (mkarg i) r) (1+ i))))))

;; @item wrap-action (n . guts) => `(lambda ($n ... $2 $1 . $rest) ,@guts)
;; Wrap user-specified action (body, as list) of n arguments in a lambda.
;; The rationale for the arglist format is that we can @code{apply} this
;; lambda to the the semantic stack.
(define (wrap-action actn)
  (cons* 'lambda (make-arg-list (car actn)) (cdr actn)))

;; @deffn obj->str object => string
;; Convert terminal (symbol, string, character) to string.
;; This is like @code{write} but will prefix symbols with @code{'}.
(define (obj->str obj)
  (cond ((string? obj) (simple-format #f "~S" obj))
	((symbol? obj) (string-append "'" (symbol->string obj)))
	((char? obj) (simple-format #f "~S" obj))))

;; @deffn prune-assoc al
;; Prune obsolete entries from an a-list.  This is order n^2.
(define (prune-assoc al)
  (let iter ((al1 '()) (al0 al))
    (if (null? al0) al1
	(iter (if (assoc (caar al0) al1) al1 (cons (car al0) al1)) (cdr al0)))))

;; @deffn fixed-point proc seed
;; .item fixed-point-by-elt proc seed
;; @example
;; proc: element list -> list
;; @end example
;; proc will take an element and insert updates at the front of list
;; and return the list
;; seed is a list
;; fixed-point processes a list
;; The procedure @code{proc} takes as arguments an element from the list
;; and the entire list.   Updates should be cons'd onto the front of the
;; list.
;; It works by setting prev to the empty list and next, curr and item to
;; the seed.  The item reference is propagated through the current list
;; until it reaches prev.  The calls to proc will update @code{next}.
;; @example
;; next-> +---+
;;        |   |
;; curr-> +---+
;;        |   |
;; item-> |   |
;;        |   |
;; prev-> +---+
;;        |   |
;;        +---+
;; @end example
(define (fixed-point proc seed)
  ;; (let ((seed (if (null? seed) (fixed-point proc (proc seed '())))))
  (let iter ((prev '()) (item seed) (curr seed) (next seed))
    (cond
     ((not (eqv? item prev))
      (iter prev (cdr item) curr (proc (car item) next)))
     ((not (eqv? next curr))
      (iter curr next next next))
     (else
      curr))))

;; @deffn vector-fixed-point proc vec => vec
;; (proc vec) => chg (boolean)
;; Not used yet (in step3).
(define (vector-fixed-point proc vec)
  (let iter ((chg #t))
    (if chg (proc vec) vec)))

;; @deffn map-attr->vector list-of-alists key => vector
;; map list of attribute lists to vector of attr
;; @example
;; (map-attr->vector '(((a . 1) ...) ((a . 2) ...) ...) => #(1 2 ...)
;; @end example
(define (map-attr->vector al-l key)
  (list->vector (map (lambda (al) (assq-ref al key)) al-l)))

;; @deffn flip al => a-list
;; change (a 1 2 3) to ((1 . a) (2 . a) (3 . a))
(define (x-flip al)
  (let iter ((result '()) (tail (cdr al)))
    (if (null? tail) result
	(iter (acons (car tail) (car al) result) (cdr tail)))))

;; @deffn x-comb (a1 a2 a3) (b1 b2 b3) => (a1 b1) (a1 b2) ...
;; The implementation needs work.
(define (x-comb a b)
  (let iter ((res '()) (al a) (bl b))
    (cond
     ((null? al) res)
     ((pair? bl) (iter (acons (car al) (car bl) res) al (cdr bl)))
     ((pair? al) (iter res (cdr al) b)))))

(define (write-vec port vec)
  (let* ((nv (vector-length vec)))
    (fmt port "  #(")
    (let iter ((col 4) (ix 0))
      (if (eq? ix nv) #f
	  (let* ((item (vector-ref vec ix))
		 (stng (fmt #f "~S " item))
		 (leng (string-length stng)))
	    (cond
	     ((> (+ col leng) 78)
	      (fmt port "\n    ~A" stng)
	      (iter (+ 4 leng) (1+ ix)))
	     (else
	      (fmt port "~A" stng)
	      (iter (+ col leng) (1+ ix)))))))
    (fmt port ")")))


;; @deffn ugly-print sexp [#:indent 4] [#:extent 78] [#:port port]
;; This will print in compact form which shows no structure.
(define* (ugly-print sexp #:optional port #:key (indent 4) (extent 78))

  (define (obj->str obj)
    (simple-format #f "~S" obj))

  ;; @deffn make-strout indent extent port
  ;; This will generate a procedure of signature @code{(proc col str)} which
  ;; takes a column and string, prints the string and returns updated column.
  (define (make-strout ind ext port)
    (let ((leader (make-string ind #\space)))
      (lambda (col str)
	(let* ((len (string-length str)))
	  (cond
	   ((> (+ col len) ext)
	    (newline port)
	    (display leader port)
	    (unless (string-every #\space str) (display str port))
	    (+ ind len))
	   (else
	    (display str port)
	    (+ col len)))))))

  (letrec ((out-p (or port (current-output-port)))
	   (leader (make-string 2 #\space))
	   (strout (make-strout indent extent out-p))

	   (iter1
	    (lambda (col sx)
	      (cond
	       ((pair? sx) (strout (iter2 (strout col "(") sx) ")"))
	       ((vector? sx)
		(strout
		 (vector-fold
		  (lambda (ix col elt)
		    (iter1 (if (zero? ix) col (strout col " ")) elt))
		  (strout col "#(") sx) ")"))
	       (else (strout col (obj->str sx))))))
	   
	   (iter2
	    (lambda (col sx)
	      (cond
	       ((pair? sx)
		(if (null? (cdr sx))
		    (iter2 (iter1 col (car sx)) (cdr sx))
		    (iter2 (strout (iter1 col (car sx)) " ") (cdr sx))))
	       ((null? sx) col)
	       (else (strout (strout col ". ") (obj->str sx))))))
	   )
    ;;(simple-format out-p leader)
    (iter1 (if (pair? sexp) (strout indent "'") indent) sexp)
    ;;(iter1 indent sexp)
    ;;(newline out-p)
    ))

;; stuff

;; @deffn depth-first-search graph => (values ht gv tv xl)
;; The argument @var{gfraph} is a list of verticies and adjacency nodes:
;; @example
;; graph => ((1 2 3 4) (2 6 7) ...)
;; @end example
;; @noindent
;; @table @var
;; @item ht
;; hash of vertex to index
;; @item gv
;; vector of index to vertex
;; @item tv
;; vector of (d . f)
;; @end table
;; ref: Algorithms, p 478
(define (depth-first-search graph)
  (let* ((n (length graph))
	   (ht (make-hash-table n))	; vertex -> index
	   (gv (make-vector n))		; index -> vertex
	   (tv (make-vector n #f))	; index -> times
	   (pv (make-vector n #f))	; index -> predecessor :unused
	   (xl '()))
    (letrec
	((next-t (let ((t 0)) (lambda () (set! t (+ 1 t)) t)))
	 (visit (lambda (k)
		  (vector-set! tv k (cons (next-t) #f))
		  (let iter ((l (cdr (vector-ref gv k))))
		    (if (not (null? l))
			(let ((ix (hashq-ref ht (car l))))
			  (unless (vector-ref tv ix)
				  (fmtout "set-pv! ~a ~a" ix k)
				  (vector-set! pv ix k)
				  (visit ix))
			  (iter (cdr l)))))
		  (set! xl (cons k xl))
		  (set-cdr! (vector-ref tv k) (next-t))
		  ))
	   )
      ;; Set up hash of vertex to index.
      (do ((i 0 (+ i 1)) (l graph (cdr l))) ((= i n))
	(vector-set! gv i (car l)) ; (vector-ref gv i) = (list-ref graph i)
	(hashq-set! ht (caar l) i)) ; (hash-ref ht (list-ref graph i)) = i
      ;; Run through vertices.
      (do ((i 0 (+ 1 i))) ((= i n))
	(unless (vector-ref tv i) (visit i)))
      (values ht gv tv xl))))

;; @deffn tzort dag
;; Given DAG return order of nodes.  The DAG is provided as list of:
;; (<node> <priors>)
;; ref: D.E.Knuth - The Art of C.P., Vol I, Sec 2.2.3
(define (tzort dag)
  (let* ((n (length dag))
	 (ht (make-hash-table n))	; node -> ix
	 (nv (make-vector n #f))	; ix -> (node . adj-list)
	 (cv (make-vector n 0))		; ix -> count
	 (incr (lambda (ix) (vector-set! cv ix (+ (vector-ref cv ix) 1))))
	 (decr (lambda (ix) (vector-set! cv ix (- (vector-ref cv ix) 1)))))
    ;; Set up ht and nv.
    (do ((i 0 (+ i 1)) (l dag (cdr l))) ((= n i))
      (vector-set! nv i (car l))
      (hashq-set! ht (caar l) i))
    ;; set up cv
    (do ((i 0 (+ i 1))) ((= n i))
      (for-each (lambda (n) (incr (hashq-ref ht n)))
		(cdr (vector-ref nv i))))
    ;; Iterate through nodes until cv all zero.
    (let iter1 ((ol '()) (uh '())	; ordered list, unordered head 
		(ut (let r ((l '()) (x 0)) ; unordered tail
		      (if (= x n) l (r (cons x l) (+ x 1))))))
      (cond
       ((null? ut)
	(if (null? uh) 
	    (reverse (map (lambda (e) (car (vector-ref nv e))) ol))
	    (iter1 ol '() uh)))
       (else
	(let* ((ix (car ut)))
	  (if (zero? (vector-ref cv ix))
	      (iter1
	       (let iter2 ((l (cdr (vector-ref nv ix))))
		     (if (null? l) (cons ix ol)
			 (begin
			   (decr (hashq-ref ht (car l)))
			   (iter2 (cdr l)))))
	       uh
	       (cdr ut))
	      (iter1 ol (cons ix uh) (cdr ut)))))))))

;;; --- last line ---
