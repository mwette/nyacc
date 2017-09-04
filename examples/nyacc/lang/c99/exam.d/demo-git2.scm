;; demo-git2.scm

(define repo-path "/Users/mwette/repo/github.com/pycparser")

(use-modules (system foreign))
(use-modules (system ffi-help-rt))
(use-modules (bytestructures guile))
(use-modules (libgit2))

(define repo (make-git_repository* 0))

(define rez #f)

(set! rez (git_repository_open (pointer-to repo) (string->pointer repo-path)))
(if (not (zero? rez)) (error "repo_open failed"))

(set! rez (git_repository_is_empty repo))
(if (zero? rez)
    (display "repo is not empty.\n")
    (display "repo is empty.\n"))





