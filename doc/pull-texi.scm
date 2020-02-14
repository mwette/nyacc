#!/usr/bin/env guile -s 
!#
;; pull-texi.scm
;;
;; Extract embedded texinfo from .scm files.
;;
;; Copyright (C) 2015 Matthew R. Wette
;;
;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;; or any later version published by the Free Software Foundation.  See the
;; file COPYING included with this distribution.

(use-modules (ice-9 rdelim))

(let* ((files (cdr (program-arguments))))
  (for-each
   (lambda (f)
     (simple-format
      #t "\n~A\n"
      (string-join
       (with-input-from-file f
	 (lambda ()
	   (let iter ((res '()) (pn #f) (line (read-line)))
	     (if (string? line)
		 (if pn
		     (if (and (> (string-length line) 2)
			      (string=? ";; " (substring line 0 3)))
			 (iter (cons (substring line 3) res) #t (read-line))
			 (iter res #f (read-line)))
		     (if (and (> (string-length line) 3)
			      (string=? ";; @" (substring line 0 4)))
			 (iter (cons (substring line 3) res) #t (read-line))
			 (iter res #f (read-line))))
		 (reverse res)))))
       "\n")))
   files))

;;; --- last line
