;; libgit2-demo1.scm

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

(define repo-path "/home/mwette/repo/github.com/pycparser")

(use-modules (ffi libgit2))
(use-modules (system ffi-help-rt))

;; Must initialize, then shutdown after use.
(git_libgit2_init)

(define repo (make-git_repository* 0))

(let ((res (git_repository_open (pointer-to repo) repo-path)))
  (unless (zero? res) (error "repo_open failed")))

(let ((res (git_repository_is_empty repo)))
  (if (zero? res)
      (display "repo is not empty.\n")
      (display "repo is empty.\n")))

(git_libgit2_shutdown)

;; --- last line ---
