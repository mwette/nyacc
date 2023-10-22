;;; ffi/cdata.scm -

;; Copyright (C) 2023 Matthew Wette
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

;;; Notes:

;;; Code:

(define-module (nyacc lang c99 ffi-cdata)
  #:export (make-ctype)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  )

;; if selector then aggregate else base
;; if base then meta is basetype

;; change selector to operator: but not good as does not operate on index
;;   '* : deref
;;   '& : pointer-to
;;   'label : xxx

;; 'array => 1
;; 'struct => 'a
;; 'union => 'a
;; 'pointer => '*

;; cdata-cref => (ct bv ix)
;; cdata-ref => scm-val   | #f if not simple
;; cdata-set! cdat scm-val . sels
;; cdata-set! cdat cdat . sels

;; macro
;;   (define-foo
;;     (with-type int32_t
;;       (define-set! (bv ix
;; =>
;;   (define (cdata-ref cd) ... ((vector-ref refs (ct-id ct)) bv ix)

;; (define-base-type '64
;; archos riscv-linux

;; removed from ctype
;;(getter ctype-getter)              ; (bv ix) => scm-val
;;(setter ctype-setter)              ; (bv ix scm-val) => nil
;;(selector ctype-selector)          ; (ct ix (sel . rest)) => (ct ix rest)

(define-record-type <ctype>
  (%make-ctype name size almt getter setter selector meta)
  ctype?
  (name ctype-name)                  ; symbolic type name
  (size ctype-size)
  (almt ctype-alignment)
  (type-id ctype-id)            ; internal rep: f64le, ..., struct, union,
  ;; add (arch ???)
  (meta ctype-meta))

(define-record-type <cdata>
  (make-cdata ct bv ix)
  cdata?
  (ct cdata-ct)
  (bv cdata-bv)
  (ix cdata-ix))

(define native-getters
  (vector
   #f ;; void type
   (lambda (ct bv ix) #f)
   ))
(define native-setters
  (vector
   #f ;; void type
   (lambda (ct bv ix vl) #f)
   ))
(define native-selectors
  (vector
   #f ;; void type
   #f
   ))

;; (pointer-to x 'a 'b ')

(define double
  (let ((str "double") (sym (string->symbol "double")))
    (let ((mtype (mtypeof-basetype str))
      (%make-ctype sym
                   mtype
                   (sizeof-mtype mtype)
                   (alignof-mtype mtype)
                   1
                   #f
                   'f64)))) ;; vs 'f64le 'f64be

(define (ctype-sel ct ix se)
  (let* ((tid (ct-type-id ct))
         (selectors (hmmm))
         (selector (vector-ref selectors tid)))
    (selector ix se)))

(define (ctype-sel* ct ix sl)
  (if (null? sl)
      (values ct ix)
      (call-with-values
          (lambda () (ctype-sel ct ix (car sl)))
        (lambda (ct ix) (ctype-sel* ct ix (cdr sl))))))

(define (cdata-cref ct bv ix sl)
  (call-with-values
      (lambda () (ctype-sel ct ix sl))
    (lambda (ct ix)
      (make-cdata ct bv ix))))

(define (cdata-ref ct bv ix sl)
  (call-with-values
      (lambda () (ctype-sel ct ix sl))
    (lambda (ct ix)
      (make-cdata ct bv ix))))

(define-syntax-rule (make-basetype name)
  (define (string->symbol name)
    (make-ctype (string->symbol name) (ctypeof-basetype name)
                (sizeof-basetype name) (alignof-basetype name))))
(export make-basetype)
;; f64

(make-basetype "double")

(ct:struct `((x ,double) (y ,f64)))


(define-record-type <struct-meta>
  (make-struct-meta packed? fields)
  struct-meta?
  (packed? struct-meta-packed?)
  (fields struct-meta-fields))

(define-record-type <field>
  (%make-field name type offset bitlength bitoffset)
  field?
  (name field-name)
  (type field-name)
  (offset field-offset)
  (bitlength field-bitlength)
  (bitoffset field-bitoffset))

(define* (make-field name type offset #:optional bitlength bitoffset)
  (%make-field name type offset bitlength bitoffset))

;; Update struct running-size (rs) given new item size (s) and align't (a).
(define (incr-size s a rs)
  (+ s (* a (quotient (+ rs (1- a)) a))))

;; Update running union size (rs) given new item s and a.
(define (maxi-size s a rs)
  (max s rs))

(define ct:struct
  (case-lambda
   ((fields) (ct:struct #f fields))
   ((pack fields)
    ;; size: running size; dflds : declared fields
    (let loop ((iflds '()) (size 0) (alnmt 0) (dflds fields))
      (cond
       ((null? dflds)
        (something))
       (else
        (let ((name (car flds))
              (type (cadr flds))
              (bits (and=> (pair? (cddr flds)) caddr))
              (csize (ctype-size type))
              (calnmt (ctype-alignment type)))
          (let* ((oset (incr-size 0 calnmt size))
                 (size (incr-size size calnmt csize)))
            (loop (cons (make-field name type oset) cflds)
                  (max alnmt calnmt) (cdr dflds)))))
       ))
    )))


(define (ct-sel ct ix fsel . rest) ; (ct ix (sel . rest)) => (ct ix rest)
  #f)

(define (test-01)
  (define ct1 (make-ctype "foo_t" (bs:struct '((x double) (y double)))))
  #f)

;; --- last line ---
