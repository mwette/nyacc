;;; nyacc/lang/sx-match.scm
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

;; sx-match: somewhat like sxml-match but hoping to be more usable and more
;; efficient for nyacc.  Note that sxml-match used in c99/pprint has to be
;; broken up in order to not overflow the stack during compilation.

;; patterns:
;;   (foo (@ . ,<name>) (bar ,abc) ...)
;;   (foo (bar ,abc) ...)
;;   (* ...)			any sexp
;;   *				any node (i.e., sexp or text)
;; or use `*any*'
;; need don't care: (foo a b c . ?) (foo a b ? c)
;; if expect text, must use , or ?
;; ideas:
;;   instead of (* ...) use (*any* ...)
;;   use (*text* "text") to match text or (*text* ,text)

;; @deffn {Syntax} sx-match exp (pat body ...) ...
;; This syntax will attempt to match @var{expr} against the patterns.
;; At runtime, when @var{pat} is matched against @var{exp}, then @var{body ...}
;; will be evaluated.
;; @end deffn
(define-syntax-rule (sx-match e c ...)
  (let ((v e)) (sx-match-1 v c ...)))

;; kt kf are continuation syntax expresions
;; [ht][vp] = [head,tail][value,pattern]
(define-syntax sxm-tail
  (syntax-rules (unquote ?)
    ((_ v ? kt kf) kt)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v ,var kt kf) (let ((var v)) kt))
    ((_ v (hp . tp) kt kf)
     (if (pair? v)
	 (let ((hv (car v)) (tv (cdr v)))
	   (sxm-tail hv hp (sxm-tail tv tp kt kf) kf))
	 kf))
    ;;((_ v p kt kf) kt))) ;; must be text, and always ? or , for text
    ))

;; nv = node value; np = node pattern
(define-syntax sxm-attr
  (syntax-rules (@)
    ((_ nv (@ . ,var)) #t)
    ((_ nv np) #f)
    ((_ nv) #f)))
	
;; sx-match-tag tag-val tag-pat
(define-syntax sxm-tag
  (syntax-rules ()
    ((_ tv (t0 t1 ...) kt kf)
     (if (memq? tv '(t0 t1 ...)) kt kf))
    ((_ tv t0 kt kf)
     (if (eqv? tv 't0) kt kf))))

(define-syntax sx-match-1
  (syntax-rules (@ * unquote)
    ((_ v) (if #f #f))
    ((_ v ((tag) ex ...) c1 ...)
     (let ((kf (lambda () (sx-match-1 v c1 ...))))
       (sxm-tag (car v) tag (begin ex ...) (kf))))
    ((_ v ((tag (@ . ,pl)) ex ...) c1 ...)
     (let ((kf (lambda () (sx-match-1 v c1 ...))))
       (sxm-tag (car v) tag (let ((pl (cdadr v))) ex ...) (kf))))
    ;; capture attributes
    ((_ v ((tag (@ . ,pl) . nl) ex ...) c1 ...)
     (let ((kf (lambda () (sx-match-1 v c1 ...))) (pl (cdadr v)))
       (sxm-tag (car v) tag
		(sxm-tail (cddr v) nl (let ((pl (cdadr v))) ex ...) kf)
		(kf))))
    ;; ignore attributes; ND0 may be an attr node. If so, ignore it.
    ((_ v ((tag nd0 . nl) ex ...) c1 ...)
     (let ((kf (lambda () (sx-match-1 v c1 ...))))
       (sxm-tag (car v) tag
		     (if (sxm-attr (cadr v) nd0)
			 (sxm-tail (cddr v) nl (begin ex ...) kf)
			 (sxm-tail (cdr v) (nd0 . nl) (begin ex ...) kf))
		     (kf))))
    ;; else part, as sexp or node
    ((_ v ((* ...) ex ...)) (begin ex ...))
    ((_ v (* ex ...)) (begin ex ...))
    ))

;; --- last line ---
