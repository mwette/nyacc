;; simple-match

;; Copyright (C) 2026 Matthew Wette
;; SPDX-License-Identifier: LGPL-3.0-or-later

;; v260201b

;; a possible speed-up where you see (car ,exp) or (cdr ,exp), e.g.:
;;   (smatch-pat (cdr ,exp) ,(cdr pat) ,kt ,kf)))
;; =>
;;   ,(let (($w (gensym)))
;;      `(let ((,$w (cdr ,exp)))
;;         (smatch-pat ,$w ,(cdr pat) ,kt ,kf)))

;; handles quote, quasiquote, unquote, _ and ? forms.

(define-module (smatch)
  #:export (smatch-lambda smatch smatch-1 smatch-pat smatch-qqpat))

(define-macro (smatch-lambda . cases)
  (let (($x (gensym)))
    `(lambda (,$x) (smatch-1 ,$x ,cases))))

(define-macro (smatch exp . cases)
  (let (($x (gensym)))
    `((lambda (,$x) (smatch-1 ,$x ,cases)) ,exp)))

;; does not verify that each case is a pair
(define-macro (smatch-1 exp cases)
  (if (pair? cases)
      (let (($kf (gensym)))
        `(let ((,$kf (lambda () (smatch-1 ,exp ,(cdr cases)))))
           (smatch-pat ,exp ,(caar cases) (begin #f ,@(cdar cases)) (,$kf))))
      `(error "no match")))

(define-macro (smatch-pat exp pat kt kf)
  (if (null? pat)
      kt
      (if (pair? pat)
          (if (eq? (car pat) 'quote)
              `(if (equal? ,exp ,pat)
                   ,kt
                   ,kf)
              (if (eq? (car pat) 'quasiquote)
                  `(smatch-qqpat ,exp ,(cadr pat) ,kt ,kf)
                  (if (eq? (car pat) '?)
                      `(if (,(cadr pat) ,exp)
                           ,kt
                           ,kf)
                      `(if (pair? ,exp)
                           (smatch-pat (car ,exp) ,(car pat)
                                       (smatch-pat (cdr ,exp) ,(cdr pat) ,kt ,kf)
                                       ,kf)
                           ,kf))))
          (if (symbol? pat)
              (if (eq? pat '_)
                  kt
                  `((lambda (,pat) ,kt) ,exp))
              `(if (equal? ,exp ,pat)
                   ,kt
                   ,kf)))))

(define-macro (smatch-qqpat exp pat kt kf)
  (if (pair? pat)
      (if (eq? (car pat) 'unquote)
          `(smatch-pat ,exp ,(cadr pat) ,kt ,kf)
          `(if (pair? ,exp)
               (smatch-qqpat (car ,exp) ,(car pat)
                             (smatch-qqpat (cdr ,exp) ,(cdr pat) ,kt ,kf)
                             ,kf)
               ,kf))
      `(if (equal? ,exp (quote ,pat))
           ,kt
           ,kf)))
      
;; test

(define (test)
  (define (t1 exp)
    (smatch exp
      ('() 1)
      (`(($hash . ,_1) ($ident . ,ident) . ,_2) ident)
      (`(($dhash . ,_1) (#\space . ,_2) . ,rest) 3)
      (`(($dhash . ,_1) ($ident . ,name) . ,rest) name)
      (`(($dhash . ,_1) ,rs . ,rest) rs)
      (`(($ident . ,ident) . ,rest) ident)
      (`(($hash . ,_1) . ,_2) 7)
      (_ 8)))

  (define (t2 exp)
    (smatch exp
      ('() 1)
      (`(($ident . ,ident) . ,rest) 2)
      (_ 3)))

  (define (t3 exp)
    (smatch exp
      ((? (lambda (x) (integer? x))) 1)
      (_ 2)))

  (and
   (equal? (t1 '()) 1)
   (equal? (t1 '(($hash . "foo") ($ident . "bar"))) "bar")
   (equal? (t1 '(($hash . "foo") ($ident . "bar") 3 4)) "bar")
   (equal? (t1 '(($dhash . "##") (#\space . " "))) 3)
   (equal? (t1 '(($dhash . "##") (#\space . " ") 5 6)) 3)
   (equal? (t1 '(($dhash . "##") ($ident . "bar") 7 8)) "bar")
   (equal? (t1 '(($dhash . "##") ($fixed . "123") 9 10)) '($fixed . "123"))
   (equal? (t1 '(($ident . "bar") 11 12)) "bar")
   (equal? (t1 '(($hash . "#") 13 14)) 7)
   (equal? (t1 '((fixed . "1") ($ident . "zip"))) 8)
   (equal? (t2 '()) 1)
   (equal? (t2 '(($ident . "foo") 1 2)) 2)
   (equal? (t2 '((fixed . "1") ($ident . "zip"))) 3)
   (equal? (t3 1) 1)
   (equal? (t3 'a) 2)
   #t))

;;(display (test)) (newline)

;;; --- last line ---
