;; calc-user-tab.scm

(define calc-user-mtab
  '(($start . stmt) ("=" . $:=) (")" . #{$:\x29;}#) ("(" . #{$:\x28;}#) 
    ($ident . $ident) ($float . $float) ($fixed . $fixed) ("/" . $:/) ("*" . 
    $:*) ("-" . $:-) ("+" . $:+) ("\n" . #{$:\xa;}#) ($error . $error) 
    ($end . $end)))

(define calc-user-ntab
  '((assn . assn) (expr . expr) (stmt . stmt) (stmt-list . stmt-list) (prog . 
    prog)))

(define calc-user-len-v
  #(1 1 1 2 1 2 2 3 3 3 3 1 1 1 3 3))

(define calc-user-rto-v
  #($start prog stmt-list stmt-list stmt stmt stmt expr expr expr expr expr 
    expr expr expr assn))

(define calc-user-pat-v
  #(((#{$:\x28;}# shift . 1) ($ident shift . 2) ($float shift . 3) ($fixed 
    shift . 4) (assn shift . 5) (expr shift . 6) (#{$:\xa;}# shift . 7) 
    (stmt shift . 8)) ((#{$:\x28;}# shift . 1) ($ident shift . 16) ($float 
    shift . 3) ($fixed shift . 4) (expr shift . 17)) (($:= shift . 15) 
    ($default reduce . 13)) (($default reduce . 12)) (($default reduce . 11)) 
    ((#{$:\xa;}# shift . 14)) ((#{$:\xa;}# shift . 9) ($:+ shift . 10) 
    ($:- shift . 11) ($:* shift . 12) ($:/ shift . 13)) (($default reduce . 4)
    ) (($end accept . 0)) (($default reduce . 5)) ((#{$:\x28;}# shift . 1) 
    ($ident shift . 16) ($float shift . 3) ($fixed shift . 4) (expr shift . 23
    )) ((#{$:\x28;}# shift . 1) ($ident shift . 16) ($float shift . 3) 
    ($fixed shift . 4) (expr shift . 22)) ((#{$:\x28;}# shift . 1) ($ident 
    shift . 16) ($float shift . 3) ($fixed shift . 4) (expr shift . 21)) 
    ((#{$:\x28;}# shift . 1) ($ident shift . 16) ($float shift . 3) ($fixed 
    shift . 4) (expr shift . 20)) (($default reduce . 6)) ((#{$:\x28;}# shift 
    . 1) ($ident shift . 16) ($float shift . 3) ($fixed shift . 4) (expr shift
    . 19)) (($default reduce . 13)) ((#{$:\x29;}# shift . 18) ($:+ shift . 10)
    ($:- shift . 11) ($:* shift . 12) ($:/ shift . 13)) (($default reduce . 14
    )) (($:+ shift . 10) ($:- shift . 11) ($:* shift . 12) ($:/ shift . 13) 
    ($default reduce . 15)) (($default reduce . 10)) (($default reduce . 9)) 
    (($:* shift . 12) ($:/ shift . 13) ($default reduce . 8)) (($:* shift . 12
    ) ($:/ shift . 13) ($default reduce . 7))))

(define calc-user-tables
  (list
   (cons 'mtab calc-user-mtab)
   (cons 'ntab calc-user-ntab)
   (cons 'len-v calc-user-len-v)
   (cons 'rto-v calc-user-rto-v)
   (cons 'pat-v calc-user-pat-v)
   ))

;;; end tables
