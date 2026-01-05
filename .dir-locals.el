;; Per-directory local variables for GNU Emacs 23 and later.

((nil             . ((fill-column . 78)
                     (tab-width   .  8)))
 (scheme-mode
  . ((indent-tabs-mode . nil)
     ;;(eval . (add-to-list 'write-file-functions 'delete-trailing-whitespace))
     ;;(eval . (add-to-list 'write-file-functions
;;			  (lambda () (untabify 0 (buffer-end)))))
     (eval . (put 'sx-match            'scheme-indent-function 1))
     (eval . (put 'sx-match-tail       'scheme-indent-function 1))
     (eval . (put 'with-arch           'scheme-indent-function 1))
     (eval . (put 'match               'scheme-indent-function 1))
     (eval . (put 'and=>               'scheme-indent-function 1))
     (eval . (put 'pass-if             'scheme-indent-function 1))))
 (emacs-lisp-mode . ((indent-tabs-mode . nil)))
 (texinfo-mode    . ((indent-tabs-mode . nil)
                     (fill-column . 78))))

;; --- last line ---
