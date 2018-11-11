;; libgit2-demo2.scm - not working - libgit2 seems incomplete

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
(use-modules ((system foreign) #:prefix ffi:))

(define (sf fmt . args) (apply simple-format #t fmt args))

(git_libgit2_init)

(define repo (make-git_repository*))

(let ((res (git_repository_open (pointer-to repo) repo-path)))
  (unless (zero? res) (error "repo_open failed")))

(if (zero? (git_repository_is_empty repo))
    (display "The repo is not empty.\n")
    (display "The repo is empty.\n"))

(define (demo1)
  (define (name-cb name payld)
    (sf "~S\n" (ffi:pointer->string name))
    1) ;; terminate on non-zero
  (define (ref-cb ref data)
    ;;(sf "ref-cb called w/ ~S\n" ref)
    (let* ((ref (make-git_reference* ref))
	   (name (make-char*))
	   (res (git_branch_name (pointer-to name) ref)))
      (sf "~S\n" (char*->string name))
      1))
  (sf "repo=~S\n" repo)
  (git_reference_foreach_name repo name-cb repo)
  (git_reference_foreach repo ref-cb NULL)
  )

(define (demo2)
  (define walker (make-git_revwalk*))
  (define oid (make-git_oid))
  (define commit (make-git_commit*))

  ;;(git_commit_id 
  ;;(git_revwalk_next (pointer-to oid) walker)
  ;;(git_commit_lookup (pointer-to commit) repo oid)
  ;; ...
  ;;(git_commit_free commit)
  ;;(git_commit_lookup (pointer-to commit) repo 
  )

;;(demo1)

(git_libgit2_shutdown)

;; --- last line ---
