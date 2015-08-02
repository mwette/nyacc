;;; lang/javascript/lgen.scm
;;; lexical

(define-module (lang javascript lgen)
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

;;; --- last line

