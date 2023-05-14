;;; nyacc/lang/@x@/xlib.scm

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

(define-module (nyacc lang @x@ xlib)
  #:export (xdict xlib-ref)
  #:use-module (nyacc lang nx-lib)
  #:use-module (ice-9 hash-table))

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr exp) (pretty-print exp (current-error-port)))

(define (xlib-ref name) `(@@ (nyacc lang @x@ xlib) ,name))

(define @x@:+ +)



(define (@x@:show_sxml)
  (set! (@@ (nyacc lang @x@ compile-tree-il) show-sxml) #t))
(define (tsh:hide_sxml)
  (set! (@@ (nyacc lang @x@ compile-tree-il) show-sxml) #f))
(define (tsh:show_xtil)
  (set! (@@ (nyacc lang @x@ compile-tree-il) show-xtil) #t))
(define (tsh:hide_xtil)
  (set! (@@ (nyacc lang @x@ compile-tree-il) show-xtil) #f))
    
;; === xdict =====================

(define xdict
  `(
    ;;("format" . ,(xlib-ref '@x@:format))
    ;; 
    ("show_sxml" . ,(xlib-ref 'tsh:show_sxml))
    ("hide_sxml" . ,(xlib-ref 'tsh:hide_sxml))
    ("show_xtil" . ,(xlib-ref 'tsh:show_xtil))
    ("hide_xtil" . ,(xlib-ref 'tsh:hide_xtil))
    ))

;; --- last line ---
