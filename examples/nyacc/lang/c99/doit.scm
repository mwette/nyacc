;; 
;; 

(use-modules (sxml match))

(let ((s0 '(ident "fooey"))
      (s1 '(ident (@ (line "123")) "fooey")))
 (sxml-match s1
  ((ident ,name)
   (display name) (newline))
  ))
