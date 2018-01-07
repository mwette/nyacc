;;; lang/javascript/lgen.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
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

(define-module (nyacc lang javascript lgen)
  #:export ()
  #:use-module (nyacc lex)
  )

;; lexical items
;; -------------
;; NaN, Infinity
;; :: RegExp grmmaras 
;; ::: NumbericString grammar
;; any unicode 
;; unicode not interpreted within comments
;; white space: tab verttab formfeed space &npsp; (\u00A0) Zs chars
;; line terminators: \u000A \u000D (\r) \u2028 \u2029
;; comments: /* .. */ , // .. \n (but return-newline)
;; future reserved words: "double" ...
;; divpunct: / /=
;; num: e or E for exponent
;; hex integer 0x
;;
;; escapes in strings: ' " \ b f n r t v
;; regexp /rebody/reflags; reflags = empty | reflags IdentifierPart
;; read on automatic semicolon insertion:
;; 
;; lexical state may be: nl is error, nl is semicolon, ...
;;   nl-seen == nl since last token => parser state ?
;;
;; built in types: Undefined Null Boolean Number String
;; otherwise: Object

;; build up the ident-reader
;; identifiers _ and $ can be first char, rest are unicode letter: LuLlLtLmLoNl
;; Lu = uppercase letter
;; Ll = lowercase letter
;; Lt = titlecase letter
;; Lm = modifier letter
;; Lo = other letter
;; Nl = letter number

;;; --- last line ---
