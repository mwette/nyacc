;; clang01.scm  - NOT WORKING
;;   http://bastian.rieck.ru/blog/posts/2015/baby_steps_libclang_ast/

;; Copyright (C) 2018 Matthew R. Wette
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

(use-modules (ffi clang))

(define file "clang-01a.cc")
(define astf "clang-01a.ast")

(with-output-to-file file
  (lambda ()
    (display "int foo(int bar) { return 0; }\n")
    ))
(system (string-append "/usr/lib/llvm-6.0/bin/clang++ -emit-ast " file))

(let* ((index (clang_createIndex 0 1))
       (tunit (clang_createTranslationUnit index astf))
       (tcurs (clang_getTranslationUnitCursor tunit))
       )
  (clang_disposeTranslationUnit tunit)
  (clang_disposeIndex index)
  #f)

(system (string-append "rm -f " file " " astf))

;; --- last line ---
