;; nyacc/lang/java/parser.scm

;; Copyright (C) 2020 Matthew R. Wette
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


(define-module (nyacc lang java parser)
  #:export (parse-java parse-java-file)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:version (0 1 0)
  )

(use-modules (nyacc lang sx-util))
(use-modules (nyacc lang util))
(include-from-path "nyacc/lang/java/mach.d/java-act.scm")
(include-from-path "nyacc/lang/java/mach.d/java-tab.scm")

(define java-raw-parser
  (make-lalr-parser
   (acons 'act-v java-act-v java-tables)
   #:skip-if-unexp '($lone-comm $code-comm)))

(define read-comm read-c-comm)

(define gen-java-lexer
  (make-lexer-generator java-mtab #:comm-reader read-comm))

(define* (parser-java #:key debug)
  (catch 'java-error
    (lambda ()
      (catch 'nyacc-error
	(lambda () (java-raw-parser (gen-java-lexer) #:debug debug))
	(lambda (key fmt . args)
	  (apply throw 'java-error fmt args))))
    (lambda (key fmt . args)
      (report-error fmt args)
      #f)))

(define* (parse-java-file filename #:key debug)
  (with-input-from-file filename
    (lambda () (parse-java #:debug debug))))

;; --- last line ---
