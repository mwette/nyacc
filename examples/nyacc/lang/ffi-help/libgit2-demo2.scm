;; libgit2-demo2.scm - not working - libgit2 seems incomplete

(define repo-path "/Users/mwette/repo/github.com/pycparser")

(use-modules (ffi libgit2))
(use-modules (system ffi-help-rt))
(use-modules ((system foreign) #:prefix ffi:))

(git_libgit2_init)

(define repo (make-git_repository*))

(let ((rez (git_repository_open (pointer-to repo) repo-path)))
  (unless (zero? rez) (error "repo_open failed")))

(if (zero? (git_repository_is_empty repo))
    (display "The repo is not empty.\n")
    (display "The repo is empty.\n"))

(define (demo1)
  (define (show-ref ref data)
    (display "show-ref called\n")
    (let ((name (make-char*)))
      (git_branch_name (pointer-to name) ref)
      ;;(simple-format #t "~S\n" (char*->string name))) ;; give null pointer
      0))
  ;;(define show-ref-cb (make-git_reference_foreach_cb* show-ref))
  ;; The current arg handling for git_reference_foreach is not right.
  (git_reference_foreach repo show-ref repo)
  )

(define (demo2)
  (define walker (make-git_revwalk*))
  (define oid (make-git_oid))
  (define commit (make-git_commit*))

  (git_revwalk_next (pointer-to oid) walker)
  (git_commit_lookup (pointer-to commit) repo oid)
  ;; ...
  (git_commit_free commit)
  )

;;(demo1)

(git_libgit2_shutdown)

;; --- last line ---
