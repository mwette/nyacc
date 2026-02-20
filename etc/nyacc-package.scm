;; nyacc/guix.scm

;; Copyright (C) 2026 Matthew Wette
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>

(define-module (nyacc-package)
  #:use-module (guix)
  #:use-module (guix packages)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile))


(define nyacc-sha256-base32-map
  '(("9.99.9" . "marker - do not delete")
    ("3.02.0" . "0lkjc52x7q04ris0vndn32cylfnzfrqqj2m3s8m0kbbj5rl2ggyh")
    ("3.00.0" . "066wg0hvr7272bdxbn8b2ahn4yqrphfir8f9w9qd222s4ipd0iqa")
    ("0.00.0" . "marker - do not delete")))

(define nyacc-base
  (package
   (name "nyacc-base")
   (version "3.00.0")
   (source 
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/mwette/nyacc")
           (commit (string-append "V" version))))
     (file-name (string-append "nyacc-" version))
     (sha256 (base32 (assoc-ref nyacc-sha256-base32-map version)))))
   (build-system gnu-build-system)
   (native-inputs (list guile-3.0))
   (home-page "https://github.com/mwette/nyacc")
   (synopsis "parser generator, c parser, ffi-helper for guile")
   (license lgpl3+)
   (description
    "NYACC, for Not Yet Another Compiler Compiler, is set of guile modules for
generating parsers and lexical analyzers.  It also provides sample parsers
and pretty-printers using SXML trees as an intermediate representation.

It provides a decent C parser and a `FFI Helper' tool to help create
Guile Scheme bindings for C-based libraries.

It provides (partially implemented) compilers based on above mentioned
parsers to allow execution with Guile as extension languages.")))

(define-public nyacc
  (package
   (inherit nyacc-base)
   (name "nyacc")
   (version "3.02.0")
   (source 
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/mwette/nyacc")
           (commit (string-append "V" version))))
     (file-name (string-append "nyacc-" version))
     (sha256 (base32 (assoc-ref nyacc-sha256-base32-map version)))))))


;; -- development versions

(define nyacc-dev-sha256-base32-map
  '(("9.99.99" . "marker - do not delete")
    ("3.03.03" . "0h9m3mszc2wrhvxacm9wphci5ip2g8cw3k6k677lx4mfrfys49xk")
    ("0.00.00" . "marker - do not delete")))

(define-public nyacc-dev
  (package
   (inherit nyacc-base)
   (name "nyacc-dev")
   (version "3.03.03")
   (source 
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/mwette/nyacc")
           (commit (string-append "dev-" version))))
     (file-name (string-append "nyacc-dev-" version))
     (sha256 (base32 (assoc-ref nyacc-dev-sha256-base32-map version)))))))


;; -- local package 

(define vcs-file?
  (or (git-predicate (dirname (current-source-directory))) (const #t)))

;not in a Git checkout
(define-public nyacc-local
  (package
   (inherit nyacc-base)
   (name "nyacc")
   (version "local")
   (source
    (local-file ".." "nyacc-local" #:recursive? #t #:select? vcs-file?))))

nyacc-local

;; --- last line ---
