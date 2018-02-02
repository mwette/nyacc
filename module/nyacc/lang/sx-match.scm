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
;; This uses only syntax-rules; sxml uses syntax-case.

(define-module (nyacc lang sx-match)
  #:export (sx-match sx-haz-attr?))

;; sx-haz-attr? val
(define (sx-haz-attr? sx)
  (and (pair? (cdr sx)) (pair? (cadr sx)) (eqv? '@ (caadr sx)) #t))
	
;; Given that a tag must be ... we define the syntax of SXML as follows:
;; SXML is a text format for XML using S-expressions, sexp's whose first
;; element is a symbol for a legal XML tag.  The syntax is:
;;   sexp: (tag node ...) | (tag (@ sexp ...) node ...)
;;   node: sexp | *text*
;; OR
;;   sexp: (tag attl tail) | (tag tail)
;;   attl: (@ (k "v") ...)
;;   tail: (node ...)
;;   node: sexp | *text*
;; where
;;   tag is a Scheme symbol for a legal XML tag name
;;   attl is an attribute list, a list whose first element is '@
;;   tail is a list of node
;;   node is sexp or a text string.

;; patterns:
;; attribute list only specified by (@ . ,<name>) where <name> is a var name
;; Specify attribute list if you want to capture the list of attributes with
;; a binding.  Otherwise leave it out.  The only way to test for no attributes
;; is to capture the a-list and test with @code{pair?}
;; 
;;   (foo (@ . ,<name>) (bar ,abc) ...)
;;   (foo (bar ,abc) ...)
;;   (foo ... , *)
;;   (* ...)			any sexp
;;   *				any node (i.e., sexp or text)
;; or use `*any*'
;; need don't care: (foo a b c . ?) (foo a b ? c)
;; if expect text, must use , or ?
;; ideas:
;;   instead of (* ...) use (*any* ...)
;;   use (*text* "text") to match text or (*text* ,text)
;; kt kf are continuation syntax expresions

;; @deffn {Syntax} sx-match exp (pat body ...) ...
;; This syntax will attempt to match @var{expr} against the patterns.
;; At runtime, when @var{pat} is matched against @var{exp}, then @var{body ...}
;; will be evaluated.
;; @end deffn
(define-syntax sx-match
  (syntax-rules ()
    ((_ e c ...)
     (let ((v e)) (sx-match-1 v c ...)))))

(define-syntax sx-match-1
  (syntax-rules ()
    ((_ v (pat exp ...) c1 ...)
     (let ((kf (lambda () (sx-match-1 v c1 ...))))
       (sxm-sexp v pat (begin exp ...) (kf))))
    ((_ v) (error "sx-match: nothing matches"))))

;; sxm-sexp val pat kt kf
;; match sexp
(define-syntax sxm-sexp
  (syntax-rules (@ * unquote)
    ;; capture attributes
    ((_ v (tag (@ . (unquote al)) . nl) kt kf)
     (sxm-tag (car v) tag
	      (if (sx-haz-attr? v)
		  (let ((al (cdadr v))) (sxm-tail (cddr v) nl kt kf))
		  (let ((al '())) (sxm-tail (cdr v) nl kt kf)))
	      kf))
    ;; ignore attributes; (cadr v) may be an attr node. If so, ignore it.
    ((_ v (tag . nl) kt kf)
     (sxm-tag (car v) tag
	      (if (sx-haz-attr? v)
		  (sxm-tail (cddr v) nl kt kf)
		  (sxm-tail (cdr v) nl kt kf))
	      kf))
    ;; accept anything
    ((_ v * kt kf) kt)))
 
;; sxm-tag val pat kt kf
;; match tag
(define-syntax sxm-tag
  (syntax-rules ()
    ((_ tv (t0 t1 ...) kt kf)
     (if (memq? tv '(t0 t1 ...)) kt kf))
    ((_ tv t0 kt kf)
     (if (eqv? tv 't0) kt kf))))

;; sxm-tail val pat kt kf
;; match tail of sexp = list of nodes
(define-syntax sxm-tail
  (syntax-rules (unquote *)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v * kt kf) kt)
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (hp . tp) kt kf)
     (if (pair? v)
	 (let ((hv (car v)) (tv (cdr v)))
	   (sxm-node hv hp (sxm-tail tv tp kt kf) kf))
	 kf))
    ((_ v p kt kf) kf)))

;; [ht][vp] = [head,tail][value,pattern]
;; Can this be set up to match a string constant?
(define-syntax sxm-node
  (syntax-rules (unquote *)
    ((_ v * kt kf) kt)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (hp . tp) kt kf)
     (if (pair? v) (sxm-sexp v (hp . tp) kt kf) kf))
    ((_ v s) (if (string? v) kt kf))))

;; --- last line ---
