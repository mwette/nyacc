;; edemotab.scm

(define len-v
  #(1 0 2 3 3))

(define pat-v
  #(((stmts shift . 1) ($end reduce . 1) ($error reduce . 1) ($:1 reduce . 1
    ) (#{$:;}# reduce . 1)) (($end accept . 0) ($error shift . 2) ($:1 shift 
    . 3) (#{$:;}# shift . 4)) ((#{$:;}# shift . 5)) ((#{$:;}# shift . 6)) ((
    $end reduce . 2) ($error reduce . 2) ($:1 reduce . 2) (#{$:;}# reduce . 2)
    ) (($end reduce . 4) ($error reduce . 4) ($:1 reduce . 4) (#{$:;}# reduce 
    . 4)) (($end reduce . 3) ($error reduce . 3) ($:1 reduce . 3) (#{$:;}# 
    reduce . 3))))

(define rto-v
  #($start stmts stmts stmts stmts))

(define mtab
  '(("1" . $:1) (";" . #{$:;}#) ($code-comm . $code-comm) ($lone-comm . 
    $lone-comm) ($error . $error) ($end . $end)))

;;; end tables
