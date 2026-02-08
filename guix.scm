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

(use-modules (guix)
             (guix packages)
             (guix git)
             (guix git-download)
             (guix build-system gnu)
             (guix licenses)
             (gnu packages bash)
             (gnu packages autotools)
             (gnu packages guile))
 
(package
 (name "nyacc-dev")
 (version "3.03")
 (source (git-checkout (url (dirname (current-filename)))))
 (build-system gnu-build-system)
 (native-inputs (list guile-3.0))
 (home-page "https://github.com/mwette/nyacc")
 (synopsis "parser generator, c parser, ffi-helper for guile")
 (license (list lgpl3+))
 (description "tbd"))

;; --- last line ---
