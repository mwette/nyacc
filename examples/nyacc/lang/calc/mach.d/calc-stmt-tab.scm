;; mach.d/calc-stmt-tab.scm

(define calc-stmt-mtab
  '(($start . stmt) ("=" . $:=) (")" . #{$:\x29;}#) ("(" . #{$:\x28;}#) 
    ($ident . $ident) ($float . $float) ($fixed . $fixed) ("/" . $:/) ("*" . 
    $:*) ("-" . $:-) ("+" . $:+) ("\n" . #{$:\xa;}#) ($error . $error) 
    ($end . $end)))

(define calc-stmt-ntab
  '((assn . assn) (expr . expr) (stmt . stmt) (stmt-list . stmt-list) (prog . 
    prog)))

(define calc-stmt-len-v
  #(1 1 1 2 1 2 2 3 3 3 3 1 1 1 3 3))

(define calc-stmt-rto-v
  #($start prog stmt-list stmt-list stmt stmt stmt expr expr expr expr expr 
    expr expr expr assn))

(define calc-stmt-pat-v
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

(define calc-stmt-tables
  (list
   (cons 'mtab calc-stmt-mtab)
   (cons 'ntab calc-stmt-ntab)
   (cons 'len-v calc-stmt-len-v)
   (cons 'rto-v calc-stmt-rto-v)
   (cons 'pat-v calc-stmt-pat-v)
   ))

;;; end tables
