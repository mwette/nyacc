;; exprtab.scm

;; Copyright (C) 2015 Matthew R. Wette
;; 
;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;; or any later version published by the Free Software Foundation.  See the
;; file COPYING included with the this distribution.

(define len-v
  #(1 1 1 1 1 3 1 4 4 3 3 3 2 2 6 7 1 3 1 3 1 2 2 2 2 4 1 1 1 1 1 1 1 4 1 3 
    3 3 1 3 3 1 3 3 1 3 3 3 3 1 3 3 1 3 1 3 1 3 1 3 1 3 1 5 1 3 1 1 1 1 1 1 1 
    1 1 1 1 1 3 1 5 3 0 1 2 1 2 1 2 1 2 1 3 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 
    2 3 1 1 2 1 2 2 3 2 3 3 4 3 2 2 1 3 2 4 3 1 2 2 1 1 2 1 2 2 3 5 4 2 5 4 2 
    1 1 2 2 4 2 1 2 1 1 3 1 3 2 5 6 4 5 2 1 3 1 3 1 1 1 1 2 1 1 3 5 4 4 3 6 6 
    5 4 4 4 3 2 1 3 2 1 2 1 3 1 3 2 2 1 1 3 2 1 1 2 1 3 5 4 4 3 6 5 6 4 3 3 2 
    5 4 5 4 3 4 3 3 2 1 1 3 4 2 1 4 3 2 1 2 3 2 1 1 1 1 1 1 1 3 4 3 4 2 1 2 1 
    1 2 1 5 7 5 5 7 8 7 7 6 2 1 1 3 2 2 3 2 1 2 1 1 1 1 5 4 3 1 2 0 1 1 1 1 1 
    1 1 2 1 1 1))

(define pat-v
  #(((cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 36) (expression shift . 37)) (($:? reduce . 
    34) ($:, reduce . 34) ($:|| reduce . 34) ($:&& reduce . 34) ($:| reduce . 
    34) ($:^ reduce . 34) ($:& reduce . 34) ($:!= reduce . 34) ($:== reduce . 
    34) ($:< reduce . 34) ($:> reduce . 34) ($:<= reduce . 34) ($:>= reduce . 
    34) ($:>> reduce . 34) ($:<< reduce . 34) ($:+ reduce . 34) ($:- reduce . 
    34) ($:% reduce . 34) ($:/ reduce . 34) ($:* reduce . 34) (#{$:\x29;}# 
    reduce . 34) (#{$:\x5d;}# reduce . 34) ($:: reduce . 34) (#{$:\x7d;}# 
    reduce . 34) ($end reduce . 34) (#{$:;}# reduce . 34)) (($:* shift . 119) 
    ($:/ shift . 120) ($:% shift . 121) ($:? reduce . 38) ($:- reduce . 38) (
    $:+ reduce . 38) ($:<< reduce . 38) ($:>> reduce . 38) ($:>= reduce . 38) 
    ($:<= reduce . 38) ($:> reduce . 38) ($:< reduce . 38) ($:== reduce . 38) 
    ($:!= reduce . 38) ($:& reduce . 38) ($:^ reduce . 38) ($:| reduce . 38) (
    $:&& reduce . 38) ($:|| reduce . 38) ($:, reduce . 38) (#{$:\x29;}# reduce
    . 38) (#{$:\x5d;}# reduce . 38) ($:: reduce . 38) (#{$:\x7d;}# reduce . 
    38) ($end reduce . 38) (#{$:;}# reduce . 38)) (($:+ shift . 117) ($:- 
    shift . 118) ($:? reduce . 41) ($:, reduce . 41) ($:|| reduce . 41) ($:&& 
    reduce . 41) ($:| reduce . 41) ($:^ reduce . 41) ($:& reduce . 41) ($:!= 
    reduce . 41) ($:== reduce . 41) ($:< reduce . 41) ($:> reduce . 41) ($:<= 
    reduce . 41) ($:>= reduce . 41) ($:>> reduce . 41) ($:<< reduce . 41) (
    #{$:\x29;}# reduce . 41) (#{$:\x5d;}# reduce . 41) ($:: reduce . 41) (
    #{$:\x7d;}# reduce . 41) ($end reduce . 41) (#{$:;}# reduce . 41)) (($:<< 
    shift . 115) ($:>> shift . 116) ($:? reduce . 44) ($:>= reduce . 44) ($:<=
    reduce . 44) ($:> reduce . 44) ($:< reduce . 44) ($:== reduce . 44) ($:!=
    reduce . 44) ($:& reduce . 44) ($:^ reduce . 44) ($:| reduce . 44) ($:&& 
    reduce . 44) ($:|| reduce . 44) ($:, reduce . 44) (#{$:\x29;}# reduce . 44
    ) (#{$:\x5d;}# reduce . 44) ($:: reduce . 44) (#{$:\x7d;}# reduce . 44) (
    $end reduce . 44) (#{$:;}# reduce . 44)) (($:< shift . 111) ($:> shift . 
    112) ($:<= shift . 113) ($:>= shift . 114) ($:? reduce . 49) ($:, reduce 
    . 49) ($:|| reduce . 49) ($:&& reduce . 49) ($:| reduce . 49) ($:^ reduce 
    . 49) ($:& reduce . 49) ($:!= reduce . 49) ($:== reduce . 49) (#{$:\x29;}#
    reduce . 49) (#{$:\x5d;}# reduce . 49) ($:: reduce . 49) (#{$:\x7d;}# 
    reduce . 49) ($end reduce . 49) (#{$:;}# reduce . 49)) (($:== shift . 109)
    ($:!= shift . 110) ($:? reduce . 52) ($:& reduce . 52) ($:^ reduce . 52) 
    ($:| reduce . 52) ($:&& reduce . 52) ($:|| reduce . 52) ($:, reduce . 52) 
    (#{$:\x29;}# reduce . 52) (#{$:\x5d;}# reduce . 52) ($:: reduce . 52) (
    #{$:\x7d;}# reduce . 52) ($end reduce . 52) (#{$:;}# reduce . 52)) (($:& 
    shift . 108) ($:? reduce . 54) ($:, reduce . 54) ($:|| reduce . 54) ($:&& 
    reduce . 54) ($:| reduce . 54) ($:^ reduce . 54) (#{$:\x29;}# reduce . 54)
    (#{$:\x5d;}# reduce . 54) ($:: reduce . 54) (#{$:\x7d;}# reduce . 54) (
    $end reduce . 54) (#{$:;}# reduce . 54)) (($:= reduce . 296) ($:+= reduce 
    . 296) ($:-= reduce . 296) ($:*= reduce . 296) ($:/= reduce . 296) ($:%= 
    reduce . 296) ($:<<= reduce . 296) ($:>>= reduce . 296) ($:&= reduce . 296
    ) ($:^= reduce . 296) ($:|= reduce . 296) ($:-- reduce . 296) ($:++ reduce
    . 296) ($:-> reduce . 296) ($:. reduce . 296) (#{$:\x28;}# reduce . 296) 
    (#{$:\x5b;}# reduce . 296) ($string reduce . 296) ($:? reduce . 296) ($:* 
    reduce . 296) ($:/ reduce . 296) ($:% reduce . 296) ($:- reduce . 296) (
    $:+ reduce . 296) ($:<< reduce . 296) ($:>> reduce . 296) ($:>= reduce . 
    296) ($:<= reduce . 296) ($:> reduce . 296) ($:< reduce . 296) ($:== 
    reduce . 296) ($:!= reduce . 296) ($:& reduce . 296) ($:^ reduce . 296) (
    $:| reduce . 296) ($:&& reduce . 296) ($:|| reduce . 296) ($:, reduce . 
    296) (#{$:\x29;}# reduce . 296) (#{$:\x5d;}# reduce . 296) ($:: reduce . 
    296) (#{$:\x7d;}# reduce . 296) ($end reduce . 296) (#{$:;}# reduce . 296)
    ) (($:= reduce . 295) ($:+= reduce . 295) ($:-= reduce . 295) ($:*= reduce
    . 295) ($:/= reduce . 295) ($:%= reduce . 295) ($:<<= reduce . 295) (
    $:>>= reduce . 295) ($:&= reduce . 295) ($:^= reduce . 295) ($:|= reduce 
    . 295) ($:-- reduce . 295) ($:++ reduce . 295) ($:-> reduce . 295) ($:. 
    reduce . 295) (#{$:\x28;}# reduce . 295) (#{$:\x5b;}# reduce . 295) ($:? 
    reduce . 295) ($:* reduce . 295) ($:/ reduce . 295) ($:% reduce . 295) (
    $:- reduce . 295) ($:+ reduce . 295) ($:<< reduce . 295) ($:>> reduce . 
    295) ($:>= reduce . 295) ($:<= reduce . 295) ($:> reduce . 295) ($:< 
    reduce . 295) ($:== reduce . 295) ($:!= reduce . 295) ($:& reduce . 295) (
    $:^ reduce . 295) ($:| reduce . 295) ($:&& reduce . 295) ($:|| reduce . 
    295) ($:, reduce . 295) (#{$:\x29;}# reduce . 295) (#{$:\x5d;}# reduce . 
    295) ($:: reduce . 295) (#{$:\x7d;}# reduce . 295) ($end reduce . 295) (
    #{$:;}# reduce . 295)) (($:= reduce . 294) ($:+= reduce . 294) ($:-= 
    reduce . 294) ($:*= reduce . 294) ($:/= reduce . 294) ($:%= reduce . 294) 
    ($:<<= reduce . 294) ($:>>= reduce . 294) ($:&= reduce . 294) ($:^= reduce
    . 294) ($:|= reduce . 294) ($:-- reduce . 294) ($:++ reduce . 294) ($:-> 
    reduce . 294) ($:. reduce . 294) (#{$:\x28;}# reduce . 294) (#{$:\x5b;}# 
    reduce . 294) ($:? reduce . 294) ($:* reduce . 294) ($:/ reduce . 294) (
    $:% reduce . 294) ($:- reduce . 294) ($:+ reduce . 294) ($:<< reduce . 294
    ) ($:>> reduce . 294) ($:>= reduce . 294) ($:<= reduce . 294) ($:> reduce 
    . 294) ($:< reduce . 294) ($:== reduce . 294) ($:!= reduce . 294) ($:& 
    reduce . 294) ($:^ reduce . 294) ($:| reduce . 294) ($:&& reduce . 294) (
    $:|| reduce . 294) ($:, reduce . 294) (#{$:\x29;}# reduce . 294) (
    #{$:\x5d;}# reduce . 294) ($:: reduce . 294) (#{$:\x7d;}# reduce . 294) (
    $end reduce . 294) (#{$:;}# reduce . 294)) (($:= reduce . 293) ($:+= 
    reduce . 293) ($:-= reduce . 293) ($:*= reduce . 293) ($:/= reduce . 293) 
    ($:%= reduce . 293) ($:<<= reduce . 293) ($:>>= reduce . 293) ($:&= reduce
    . 293) ($:^= reduce . 293) ($:|= reduce . 293) ($:-- reduce . 293) ($:++ 
    reduce . 293) ($:-> reduce . 293) ($:. reduce . 293) (#{$:\x28;}# reduce 
    . 293) (#{$:\x5b;}# reduce . 293) ($:? reduce . 293) ($:* reduce . 293) (
    $:/ reduce . 293) ($:% reduce . 293) ($:- reduce . 293) ($:+ reduce . 293)
    ($:<< reduce . 293) ($:>> reduce . 293) ($:>= reduce . 293) ($:<= reduce 
    . 293) ($:> reduce . 293) ($:< reduce . 293) ($:== reduce . 293) ($:!= 
    reduce . 293) ($:& reduce . 293) ($:^ reduce . 293) ($:| reduce . 293) (
    $:&& reduce . 293) ($:|| reduce . 293) ($:, reduce . 293) (#{$:\x29;}# 
    reduce . 293) (#{$:\x5d;}# reduce . 293) ($:: reduce . 293) (#{$:\x7d;}# 
    reduce . 293) ($end reduce . 293) (#{$:;}# reduce . 293)) (($:= reduce . 
    292) ($:+= reduce . 292) ($:-= reduce . 292) ($:*= reduce . 292) ($:/= 
    reduce . 292) ($:%= reduce . 292) ($:<<= reduce . 292) ($:>>= reduce . 292
    ) ($:&= reduce . 292) ($:^= reduce . 292) ($:|= reduce . 292) ($:-- reduce
    . 292) ($:++ reduce . 292) ($:-> reduce . 292) ($:. reduce . 292) (
    #{$:\x28;}# reduce . 292) (#{$:\x5b;}# reduce . 292) ($:? reduce . 292) (
    $:* reduce . 292) ($:/ reduce . 292) ($:% reduce . 292) ($:- reduce . 292)
    ($:+ reduce . 292) ($:<< reduce . 292) ($:>> reduce . 292) ($:>= reduce 
    . 292) ($:<= reduce . 292) ($:> reduce . 292) ($:< reduce . 292) ($:== 
    reduce . 292) ($:!= reduce . 292) ($:& reduce . 292) ($:^ reduce . 292) (
    $:| reduce . 292) ($:&& reduce . 292) ($:|| reduce . 292) ($:, reduce . 
    292) (#{$:\x29;}# reduce . 292) (#{$:\x5d;}# reduce . 292) ($:: reduce . 
    292) (#{$:\x7b;}# reduce . 292) (#{$:\x7d;}# reduce . 292) (#{$:;}# reduce
    . 292) ($end reduce . 292) ($:inline reduce . 292) ($:auto reduce . 292) 
    ($:extern reduce . 292) ($:register reduce . 292) ($:static reduce . 292) 
    ($:typedef reduce . 292) ($:const reduce . 292) ($:volatile reduce . 292) 
    ($:restrict reduce . 292) ($:void reduce . 292) ($:_Bool reduce . 292) (
    typename reduce . 292) ($:enum reduce . 292) ($:struct reduce . 292) (
    $:union reduce . 292) ($:_Complex reduce . 292) ($:float reduce . 292) (
    $:double reduce . 292) ($:long reduce . 292) ($:short reduce . 292) (
    $:signed reduce . 292) ($:int reduce . 292) ($:unsigned reduce . 292) (
    $:char reduce . 292) (cpp-ident reduce . 292) ($ident reduce . 292)) (($:=
    reduce . 291) ($:+= reduce . 291) ($:-= reduce . 291) ($:*= reduce . 291)
    ($:/= reduce . 291) ($:%= reduce . 291) ($:<<= reduce . 291) ($:>>= 
    reduce . 291) ($:&= reduce . 291) ($:^= reduce . 291) ($:|= reduce . 291) 
    ($:-- reduce . 291) ($:++ reduce . 291) ($:-> reduce . 291) ($:. reduce . 
    291) (#{$:\x28;}# reduce . 291) (#{$:\x5b;}# reduce . 291) ($:? reduce . 
    291) ($:* reduce . 291) ($:/ reduce . 291) ($:% reduce . 291) ($:- reduce 
    . 291) ($:+ reduce . 291) ($:<< reduce . 291) ($:>> reduce . 291) ($:>= 
    reduce . 291) ($:<= reduce . 291) ($:> reduce . 291) ($:< reduce . 291) (
    $:== reduce . 291) ($:!= reduce . 291) ($:& reduce . 291) ($:^ reduce . 
    291) ($:| reduce . 291) ($:&& reduce . 291) ($:|| reduce . 291) ($:, 
    reduce . 291) (#{$:\x29;}# reduce . 291) (#{$:\x5d;}# reduce . 291) ($:: 
    reduce . 291) (#{$:\x7b;}# reduce . 291) (#{$:\x7d;}# reduce . 291) (
    #{$:;}# reduce . 291) ($end reduce . 291) ($:inline reduce . 291) ($:auto 
    reduce . 291) ($:extern reduce . 291) ($:register reduce . 291) ($:static 
    reduce . 291) ($:typedef reduce . 291) ($:const reduce . 291) ($:volatile 
    reduce . 291) ($:restrict reduce . 291) ($:void reduce . 291) ($:_Bool 
    reduce . 291) (typename reduce . 291) ($:enum reduce . 291) ($:struct 
    reduce . 291) ($:union reduce . 291) ($:_Complex reduce . 291) ($:float 
    reduce . 291) ($:double reduce . 291) ($:long reduce . 291) ($:short 
    reduce . 291) ($:signed reduce . 291) ($:int reduce . 291) ($:unsigned 
    reduce . 291) ($:char reduce . 291) (cpp-ident reduce . 291) ($ident 
    reduce . 291)) (($:^ shift . 107) ($:? reduce . 56) ($:| reduce . 56) (
    $:&& reduce . 56) ($:|| reduce . 56) ($:, reduce . 56) (#{$:\x29;}# reduce
    . 56) (#{$:\x5d;}# reduce . 56) ($:: reduce . 56) (#{$:\x7d;}# reduce . 
    56) ($end reduce . 56) (#{$:;}# reduce . 56)) (($:| shift . 106) ($:? 
    reduce . 58) ($:, reduce . 58) ($:|| reduce . 58) ($:&& reduce . 58) (
    #{$:\x29;}# reduce . 58) (#{$:\x5d;}# reduce . 58) ($:: reduce . 58) (
    #{$:\x7d;}# reduce . 58) ($end reduce . 58) (#{$:;}# reduce . 58)) ((
    $string shift . 105) ($:|= reduce . 4) ($:^= reduce . 4) ($:&= reduce . 4)
    ($:>>= reduce . 4) ($:<<= reduce . 4) ($:%= reduce . 4) ($:/= reduce . 4)
    ($:*= reduce . 4) ($:-= reduce . 4) ($:+= reduce . 4) ($:= reduce . 4) (
    #{$:\x5b;}# reduce . 4) (#{$:\x28;}# reduce . 4) ($:. reduce . 4) ($:-> 
    reduce . 4) ($:++ reduce . 4) ($:-- reduce . 4) ($:? reduce . 4) ($:, 
    reduce . 4) ($:|| reduce . 4) ($:&& reduce . 4) ($:| reduce . 4) ($:^ 
    reduce . 4) ($:& reduce . 4) ($:!= reduce . 4) ($:== reduce . 4) ($:< 
    reduce . 4) ($:> reduce . 4) ($:<= reduce . 4) ($:>= reduce . 4) ($:>> 
    reduce . 4) ($:<< reduce . 4) ($:+ reduce . 4) ($:- reduce . 4) ($:% 
    reduce . 4) ($:/ reduce . 4) ($:* reduce . 4) (#{$:\x29;}# reduce . 4) (
    #{$:\x5d;}# reduce . 4) ($:: reduce . 4) (#{$:\x7d;}# reduce . 4) ($end 
    reduce . 4) (#{$:;}# reduce . 4)) (($:|= reduce . 3) ($:^= reduce . 3) (
    $:&= reduce . 3) ($:>>= reduce . 3) ($:<<= reduce . 3) ($:%= reduce . 3) (
    $:/= reduce . 3) ($:*= reduce . 3) ($:-= reduce . 3) ($:+= reduce . 3) (
    $:= reduce . 3) (#{$:\x5b;}# reduce . 3) (#{$:\x28;}# reduce . 3) ($:. 
    reduce . 3) ($:-> reduce . 3) ($:++ reduce . 3) ($:-- reduce . 3) ($:? 
    reduce . 3) ($:, reduce . 3) ($:|| reduce . 3) ($:&& reduce . 3) ($:| 
    reduce . 3) ($:^ reduce . 3) ($:& reduce . 3) ($:!= reduce . 3) ($:== 
    reduce . 3) ($:< reduce . 3) ($:> reduce . 3) ($:<= reduce . 3) ($:>= 
    reduce . 3) ($:>> reduce . 3) ($:<< reduce . 3) ($:+ reduce . 3) ($:- 
    reduce . 3) ($:% reduce . 3) ($:/ reduce . 3) ($:* reduce . 3) (
    #{$:\x29;}# reduce . 3) (#{$:\x5d;}# reduce . 3) ($:: reduce . 3) (
    #{$:\x7d;}# reduce . 3) ($end reduce . 3) (#{$:;}# reduce . 3)) (($:|= 
    reduce . 2) ($:^= reduce . 2) ($:&= reduce . 2) ($:>>= reduce . 2) ($:<<= 
    reduce . 2) ($:%= reduce . 2) ($:/= reduce . 2) ($:*= reduce . 2) ($:-= 
    reduce . 2) ($:+= reduce . 2) ($:= reduce . 2) (#{$:\x5b;}# reduce . 2) (
    #{$:\x28;}# reduce . 2) ($:. reduce . 2) ($:-> reduce . 2) ($:++ reduce . 
    2) ($:-- reduce . 2) ($:? reduce . 2) ($:, reduce . 2) ($:|| reduce . 2) (
    $:&& reduce . 2) ($:| reduce . 2) ($:^ reduce . 2) ($:& reduce . 2) ($:!= 
    reduce . 2) ($:== reduce . 2) ($:< reduce . 2) ($:> reduce . 2) ($:<= 
    reduce . 2) ($:>= reduce . 2) ($:>> reduce . 2) ($:<< reduce . 2) ($:+ 
    reduce . 2) ($:- reduce . 2) ($:% reduce . 2) ($:/ reduce . 2) ($:* reduce
    . 2) (#{$:\x29;}# reduce . 2) (#{$:\x5d;}# reduce . 2) ($:: reduce . 2) (
    #{$:\x7d;}# reduce . 2) ($end reduce . 2) (#{$:;}# reduce . 2)) (($:&& 
    shift . 104) ($:? reduce . 60) ($:|| reduce . 60) ($:, reduce . 60) (
    #{$:\x29;}# reduce . 60) (#{$:\x5d;}# reduce . 60) ($:: reduce . 60) (
    #{$:\x7d;}# reduce . 60) ($end reduce . 60) (#{$:;}# reduce . 60)) ((
    cpp-ident reduce . 31) ($ident reduce . 31) ($chlit reduce . 31) ($float 
    reduce . 31) ($fixed reduce . 31) ($string reduce . 31) ($:! reduce . 31) 
    ($:~ reduce . 31) ($:- reduce . 31) ($:+ reduce . 31) ($:* reduce . 31) (
    $:& reduce . 31) ($:sizeof reduce . 31) ($:-- reduce . 31) ($:++ reduce . 
    31) (#{$:\x28;}# reduce . 31)) ((cpp-ident reduce . 30) ($ident reduce . 
    30) ($chlit reduce . 30) ($float reduce . 30) ($fixed reduce . 30) (
    $string reduce . 30) ($:! reduce . 30) ($:~ reduce . 30) ($:- reduce . 30)
    ($:+ reduce . 30) ($:* reduce . 30) ($:& reduce . 30) ($:sizeof reduce . 
    30) ($:-- reduce . 30) ($:++ reduce . 30) (#{$:\x28;}# reduce . 30)) ((
    cpp-ident reduce . 29) ($ident reduce . 29) ($chlit reduce . 29) ($float 
    reduce . 29) ($fixed reduce . 29) ($string reduce . 29) ($:! reduce . 29) 
    ($:~ reduce . 29) ($:- reduce . 29) ($:+ reduce . 29) ($:* reduce . 29) (
    $:& reduce . 29) ($:sizeof reduce . 29) ($:-- reduce . 29) ($:++ reduce . 
    29) (#{$:\x28;}# reduce . 29)) ((cpp-ident reduce . 28) ($ident reduce . 
    28) ($chlit reduce . 28) ($float reduce . 28) ($fixed reduce . 28) (
    $string reduce . 28) ($:! reduce . 28) ($:~ reduce . 28) ($:- reduce . 28)
    ($:+ reduce . 28) ($:* reduce . 28) ($:& reduce . 28) ($:sizeof reduce . 
    28) ($:-- reduce . 28) ($:++ reduce . 28) (#{$:\x28;}# reduce . 28)) ((
    cpp-ident reduce . 27) ($ident reduce . 27) ($chlit reduce . 27) ($float 
    reduce . 27) ($fixed reduce . 27) ($string reduce . 27) ($:! reduce . 27) 
    ($:~ reduce . 27) ($:- reduce . 27) ($:+ reduce . 27) ($:* reduce . 27) (
    $:& reduce . 27) ($:sizeof reduce . 27) ($:-- reduce . 27) ($:++ reduce . 
    27) (#{$:\x28;}# reduce . 27)) ((cpp-ident reduce . 26) ($ident reduce . 
    26) ($chlit reduce . 26) ($float reduce . 26) ($fixed reduce . 26) (
    $string reduce . 26) ($:! reduce . 26) ($:~ reduce . 26) ($:- reduce . 26)
    ($:+ reduce . 26) ($:* reduce . 26) ($:& reduce . 26) ($:sizeof reduce . 
    26) ($:-- reduce . 26) ($:++ reduce . 26) (#{$:\x28;}# reduce . 26)) ((
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 36) (expression shift . 66) (typename shift 
    . 67) ($:enum shift . 68) ($:union shift . 69) ($:struct shift . 70) (
    $:_Complex shift . 71) ($:double shift . 72) ($:float shift . 73) ($:char 
    shift . 74) ($:unsigned shift . 75) ($:long shift . 76) ($:int shift . 77)
    ($:signed shift . 78) ($:short shift . 79) ($:inline shift . 80) (
    $:restrict shift . 81) ($:volatile shift . 82) ($:const shift . 83) (
    typedef-name shift . 84) (enum-specifier shift . 85) (
    struct-or-union-specifier shift . 86) (complex-type-specifier shift . 87) 
    ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) ($:typedef shift . 92
    ) ($:static shift . 93) ($:register shift . 94) ($:extern shift . 95) (
    $:auto shift . 96) (function-specifier shift . 97) (type-qualifier shift 
    . 98) (type-specifier shift . 99) (storage-class-specifier shift . 100) (
    declaration-specifiers shift . 101) (specifier-qualifier-list shift . 102)
    (type-name shift . 103)) (($:= reduce . 6) ($:+= reduce . 6) ($:-= reduce
    . 6) ($:*= reduce . 6) ($:/= reduce . 6) ($:%= reduce . 6) ($:<<= reduce 
    . 6) ($:>>= reduce . 6) ($:&= reduce . 6) ($:^= reduce . 6) ($:|= reduce 
    . 6) ($:-- reduce . 6) ($:++ reduce . 6) ($:-> reduce . 6) ($:. reduce . 6
    ) (#{$:\x28;}# reduce . 6) (#{$:\x5b;}# reduce . 6) ($:? reduce . 6) ($:* 
    reduce . 6) ($:/ reduce . 6) ($:% reduce . 6) ($:- reduce . 6) ($:+ reduce
    . 6) ($:<< reduce . 6) ($:>> reduce . 6) ($:>= reduce . 6) ($:<= reduce 
    . 6) ($:> reduce . 6) ($:< reduce . 6) ($:== reduce . 6) ($:!= reduce . 6)
    ($:& reduce . 6) ($:^ reduce . 6) ($:| reduce . 6) ($:&& reduce . 6) (
    $:|| reduce . 6) ($:, reduce . 6) (#{$:\x29;}# reduce . 6) (#{$:\x5d;}# 
    reduce . 6) ($:: reduce . 6) (#{$:\x7d;}# reduce . 6) ($end reduce . 6) (
    #{$:;}# reduce . 6)) (($:? shift . 64) ($:|| shift . 65) ($:, reduce . 62)
    (#{$:\x29;}# reduce . 62) (#{$:\x5d;}# reduce . 62) ($:: reduce . 62) (
    #{$:\x7d;}# reduce . 62) ($end reduce . 62) (#{$:;}# reduce . 62)) ((
    $string shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 
    11) (cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16)
    (constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25)
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 62) (#{$:\x28;}# shift . 63)) (($string 
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift 
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25) 
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (
    cast-expression shift . 61)) (($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift 
    . 23) ($:* shift . 24) ($:& shift . 25) (#{$:\x28;}# shift . 57) (
    primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator shift
    . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift . 33)
    (unary-expression shift . 59)) (($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift 
    . 23) ($:* shift . 24) ($:& shift . 25) (#{$:\x28;}# shift . 57) (
    primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator shift
    . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift . 33)
    (unary-expression shift . 58)) ((#{$:\x5b;}# shift . 51) (#{$:\x28;}# 
    shift . 52) ($:. shift . 53) ($:-> shift . 54) ($:++ shift . 55) ($:-- 
    shift . 56) ($:|= reduce . 20) ($:^= reduce . 20) ($:&= reduce . 20) (
    $:>>= reduce . 20) ($:<<= reduce . 20) ($:%= reduce . 20) ($:/= reduce . 
    20) ($:*= reduce . 20) ($:-= reduce . 20) ($:+= reduce . 20) ($:= reduce 
    . 20) ($:? reduce . 20) ($:, reduce . 20) ($:|| reduce . 20) ($:&& reduce 
    . 20) ($:| reduce . 20) ($:^ reduce . 20) ($:& reduce . 20) ($:!= reduce 
    . 20) ($:== reduce . 20) ($:< reduce . 20) ($:> reduce . 20) ($:<= reduce 
    . 20) ($:>= reduce . 20) ($:>> reduce . 20) ($:<< reduce . 20) ($:+ reduce
    . 20) ($:- reduce . 20) ($:% reduce . 20) ($:/ reduce . 20) ($:* reduce 
    . 20) (#{$:\x29;}# reduce . 20) (#{$:\x5d;}# reduce . 20) ($:: reduce . 20
    ) (#{$:\x7d;}# reduce . 20) ($end reduce . 20) (#{$:;}# reduce . 20)) ((
    $:|= shift . 39) ($:^= shift . 40) ($:&= shift . 41) ($:>>= shift . 42) (
    $:<<= shift . 43) ($:%= shift . 44) ($:/= shift . 45) ($:*= shift . 46) (
    $:-= shift . 47) ($:+= shift . 48) ($:= shift . 49) (assignment-operator 
    shift . 50) ($:? reduce . 32) ($:* reduce . 32) ($:/ reduce . 32) ($:% 
    reduce . 32) ($:- reduce . 32) ($:+ reduce . 32) ($:<< reduce . 32) ($:>> 
    reduce . 32) ($:>= reduce . 32) ($:<= reduce . 32) ($:> reduce . 32) ($:< 
    reduce . 32) ($:== reduce . 32) ($:!= reduce . 32) ($:& reduce . 32) ($:^ 
    reduce . 32) ($:| reduce . 32) ($:&& reduce . 32) ($:|| reduce . 32) ($:, 
    reduce . 32) (#{$:\x29;}# reduce . 32) (#{$:\x5d;}# reduce . 32) ($:: 
    reduce . 32) (#{$:\x7d;}# reduce . 32) ($end reduce . 32)) (($:, reduce . 
    64) (#{$:\x29;}# reduce . 64) (#{$:\x5d;}# reduce . 64) ($:: reduce . 64) 
    (#{$:\x7d;}# reduce . 64) ($end reduce . 64)) (($:, reduce . 77) (
    #{$:\x29;}# reduce . 77) (#{$:\x5d;}# reduce . 77) ($:: reduce . 77) ($end
    reduce . 77)) (($:, shift . 38) ($end accept . 0)) ((cast-expression 
    shift . 1) (multiplicative-expression shift . 2) (additive-expression 
    shift . 3) (shift-expression shift . 4) (relational-expression shift . 5) 
    (equality-expression shift . 6) (bitwise-and-expression shift . 7) (
    $string shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 
    11) (cpp-ident shift . 12) ($ident shift . 13) (bitwise-xor-expression 
    shift . 14) (bitwise-or-expression shift . 15) (string-literal shift . 16)
    (constant shift . 17) (identifier shift . 18) (logical-and-expression 
    shift . 19) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift 
    . 23) ($:* shift . 24) ($:& shift . 25) (#{$:\x28;}# shift . 26) (
    primary-expression shift . 27) (logical-or-expression shift . 28) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (unary-expression shift . 34) 
    (conditional-expression shift . 35) (assignment-expression shift . 187)) (
    (cpp-ident reduce . 76) ($ident reduce . 76) ($chlit reduce . 76) ($float 
    reduce . 76) ($fixed reduce . 76) ($string reduce . 76) (#{$:\x28;}# 
    reduce . 76) ($:! reduce . 76) ($:~ reduce . 76) ($:- reduce . 76) ($:+ 
    reduce . 76) ($:* reduce . 76) ($:& reduce . 76) ($:sizeof reduce . 76) (
    $:-- reduce . 76) ($:++ reduce . 76)) ((cpp-ident reduce . 75) ($ident 
    reduce . 75) ($chlit reduce . 75) ($float reduce . 75) ($fixed reduce . 75
    ) ($string reduce . 75) (#{$:\x28;}# reduce . 75) ($:! reduce . 75) ($:~ 
    reduce . 75) ($:- reduce . 75) ($:+ reduce . 75) ($:* reduce . 75) ($:& 
    reduce . 75) ($:sizeof reduce . 75) ($:-- reduce . 75) ($:++ reduce . 75))
    ((cpp-ident reduce . 74) ($ident reduce . 74) ($chlit reduce . 74) (
    $float reduce . 74) ($fixed reduce . 74) ($string reduce . 74) (
    #{$:\x28;}# reduce . 74) ($:! reduce . 74) ($:~ reduce . 74) ($:- reduce 
    . 74) ($:+ reduce . 74) ($:* reduce . 74) ($:& reduce . 74) ($:sizeof 
    reduce . 74) ($:-- reduce . 74) ($:++ reduce . 74)) ((cpp-ident reduce . 
    73) ($ident reduce . 73) ($chlit reduce . 73) ($float reduce . 73) ($fixed
    reduce . 73) ($string reduce . 73) (#{$:\x28;}# reduce . 73) ($:! reduce 
    . 73) ($:~ reduce . 73) ($:- reduce . 73) ($:+ reduce . 73) ($:* reduce . 
    73) ($:& reduce . 73) ($:sizeof reduce . 73) ($:-- reduce . 73) ($:++ 
    reduce . 73)) ((cpp-ident reduce . 72) ($ident reduce . 72) ($chlit reduce
    . 72) ($float reduce . 72) ($fixed reduce . 72) ($string reduce . 72) (
    #{$:\x28;}# reduce . 72) ($:! reduce . 72) ($:~ reduce . 72) ($:- reduce 
    . 72) ($:+ reduce . 72) ($:* reduce . 72) ($:& reduce . 72) ($:sizeof 
    reduce . 72) ($:-- reduce . 72) ($:++ reduce . 72)) ((cpp-ident reduce . 
    71) ($ident reduce . 71) ($chlit reduce . 71) ($float reduce . 71) ($fixed
    reduce . 71) ($string reduce . 71) (#{$:\x28;}# reduce . 71) ($:! reduce 
    . 71) ($:~ reduce . 71) ($:- reduce . 71) ($:+ reduce . 71) ($:* reduce . 
    71) ($:& reduce . 71) ($:sizeof reduce . 71) ($:-- reduce . 71) ($:++ 
    reduce . 71)) ((cpp-ident reduce . 70) ($ident reduce . 70) ($chlit reduce
    . 70) ($float reduce . 70) ($fixed reduce . 70) ($string reduce . 70) (
    #{$:\x28;}# reduce . 70) ($:! reduce . 70) ($:~ reduce . 70) ($:- reduce 
    . 70) ($:+ reduce . 70) ($:* reduce . 70) ($:& reduce . 70) ($:sizeof 
    reduce . 70) ($:-- reduce . 70) ($:++ reduce . 70)) ((cpp-ident reduce . 
    69) ($ident reduce . 69) ($chlit reduce . 69) ($float reduce . 69) ($fixed
    reduce . 69) ($string reduce . 69) (#{$:\x28;}# reduce . 69) ($:! reduce 
    . 69) ($:~ reduce . 69) ($:- reduce . 69) ($:+ reduce . 69) ($:* reduce . 
    69) ($:& reduce . 69) ($:sizeof reduce . 69) ($:-- reduce . 69) ($:++ 
    reduce . 69)) ((cpp-ident reduce . 68) ($ident reduce . 68) ($chlit reduce
    . 68) ($float reduce . 68) ($fixed reduce . 68) ($string reduce . 68) (
    #{$:\x28;}# reduce . 68) ($:! reduce . 68) ($:~ reduce . 68) ($:- reduce 
    . 68) ($:+ reduce . 68) ($:* reduce . 68) ($:& reduce . 68) ($:sizeof 
    reduce . 68) ($:-- reduce . 68) ($:++ reduce . 68)) ((cpp-ident reduce . 
    67) ($ident reduce . 67) ($chlit reduce . 67) ($float reduce . 67) ($fixed
    reduce . 67) ($string reduce . 67) (#{$:\x28;}# reduce . 67) ($:! reduce 
    . 67) ($:~ reduce . 67) ($:- reduce . 67) ($:+ reduce . 67) ($:* reduce . 
    67) ($:& reduce . 67) ($:sizeof reduce . 67) ($:-- reduce . 67) ($:++ 
    reduce . 67)) ((cpp-ident reduce . 66) ($ident reduce . 66) ($chlit reduce
    . 66) ($float reduce . 66) ($fixed reduce . 66) ($string reduce . 66) (
    #{$:\x28;}# reduce . 66) ($:! reduce . 66) ($:~ reduce . 66) ($:- reduce 
    . 66) ($:+ reduce . 66) ($:* reduce . 66) ($:& reduce . 66) ($:sizeof 
    reduce . 66) ($:-- reduce . 66) ($:++ reduce . 66)) ((cast-expression 
    shift . 1) (multiplicative-expression shift . 2) (additive-expression 
    shift . 3) (shift-expression shift . 4) (relational-expression shift . 5) 
    (equality-expression shift . 6) (bitwise-and-expression shift . 7) (
    $string shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 
    11) (cpp-ident shift . 12) ($ident shift . 13) (bitwise-xor-expression 
    shift . 14) (bitwise-or-expression shift . 15) (string-literal shift . 16)
    (constant shift . 17) (identifier shift . 18) (logical-and-expression 
    shift . 19) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift 
    . 23) ($:* shift . 24) ($:& shift . 25) (#{$:\x28;}# shift . 26) (
    primary-expression shift . 27) (logical-or-expression shift . 28) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (unary-expression shift . 34) 
    (conditional-expression shift . 35) (assignment-expression shift . 186)) (
    (cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 36) (expression shift . 185)) ((#{$:\x29;}# 
    shift . 181) (cast-expression shift . 1) (multiplicative-expression shift 
    . 2) (additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (typename shift . 67) (unary-expression shift . 34) (
    conditional-expression shift . 35) (typedef-name shift . 182) (
    assignment-expression shift . 183) (argument-expression-list shift . 184))
    ((cpp-ident shift . 12) ($ident shift . 13) (identifier shift . 180)) ((
    cpp-ident shift . 12) ($ident shift . 13) (identifier shift . 179)) (($:= 
    reduce . 12) ($:+= reduce . 12) ($:-= reduce . 12) ($:*= reduce . 12) (
    $:/= reduce . 12) ($:%= reduce . 12) ($:<<= reduce . 12) ($:>>= reduce . 
    12) ($:&= reduce . 12) ($:^= reduce . 12) ($:|= reduce . 12) ($:-- reduce 
    . 12) ($:++ reduce . 12) ($:-> reduce . 12) ($:. reduce . 12) (#{$:\x28;}#
    reduce . 12) (#{$:\x5b;}# reduce . 12) ($:? reduce . 12) ($:* reduce . 12
    ) ($:/ reduce . 12) ($:% reduce . 12) ($:- reduce . 12) ($:+ reduce . 12) 
    ($:<< reduce . 12) ($:>> reduce . 12) ($:>= reduce . 12) ($:<= reduce . 12
    ) ($:> reduce . 12) ($:< reduce . 12) ($:== reduce . 12) ($:!= reduce . 12
    ) ($:& reduce . 12) ($:^ reduce . 12) ($:| reduce . 12) ($:&& reduce . 12)
    ($:|| reduce . 12) ($:, reduce . 12) (#{$:\x29;}# reduce . 12) (
    #{$:\x5d;}# reduce . 12) ($:: reduce . 12) (#{$:\x7d;}# reduce . 12) ($end
    reduce . 12) (#{$:;}# reduce . 12)) (($:= reduce . 13) ($:+= reduce . 13)
    ($:-= reduce . 13) ($:*= reduce . 13) ($:/= reduce . 13) ($:%= reduce . 
    13) ($:<<= reduce . 13) ($:>>= reduce . 13) ($:&= reduce . 13) ($:^= 
    reduce . 13) ($:|= reduce . 13) ($:-- reduce . 13) ($:++ reduce . 13) (
    $:-> reduce . 13) ($:. reduce . 13) (#{$:\x28;}# reduce . 13) (#{$:\x5b;}#
    reduce . 13) ($:? reduce . 13) ($:* reduce . 13) ($:/ reduce . 13) ($:% 
    reduce . 13) ($:- reduce . 13) ($:+ reduce . 13) ($:<< reduce . 13) ($:>> 
    reduce . 13) ($:>= reduce . 13) ($:<= reduce . 13) ($:> reduce . 13) ($:< 
    reduce . 13) ($:== reduce . 13) ($:!= reduce . 13) ($:& reduce . 13) ($:^ 
    reduce . 13) ($:| reduce . 13) ($:&& reduce . 13) ($:|| reduce . 13) ($:, 
    reduce . 13) (#{$:\x29;}# reduce . 13) (#{$:\x5d;}# reduce . 13) ($:: 
    reduce . 13) (#{$:\x7d;}# reduce . 13) ($end reduce . 13) (#{$:;}# reduce 
    . 13)) ((typename shift . 67) ($:enum shift . 68) ($:union shift . 69) (
    $:struct shift . 70) ($:_Complex shift . 71) ($:double shift . 72) (
    $:float shift . 73) ($:char shift . 74) ($:unsigned shift . 75) ($:long 
    shift . 76) ($:int shift . 77) ($:signed shift . 78) ($:short shift . 79) 
    ($:inline shift . 80) ($:restrict shift . 81) ($:volatile shift . 82) (
    $:const shift . 83) (typedef-name shift . 84) (enum-specifier shift . 85) 
    (struct-or-union-specifier shift . 86) (complex-type-specifier shift . 87)
    ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) ($:typedef shift . 92
    ) ($:static shift . 93) ($:register shift . 94) ($:extern shift . 95) (
    $:auto shift . 96) (function-specifier shift . 97) (type-qualifier shift 
    . 98) (type-specifier shift . 99) (storage-class-specifier shift . 100) (
    declaration-specifiers shift . 101) (specifier-qualifier-list shift . 102)
    (type-name shift . 178) (cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) ($string
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (bitwise-xor-expression shift . 
    14) (bitwise-or-expression shift . 15) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) (logical-and-expression shift
    . 19) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23)
    ($:* shift . 24) ($:& shift . 25) (#{$:\x28;}# shift . 26) (
    primary-expression shift . 27) (logical-or-expression shift . 28) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (unary-expression shift . 34) 
    (conditional-expression shift . 35) (assignment-expression shift . 36) (
    expression shift . 66)) (($:|= reduce . 21) ($:^= reduce . 21) ($:&= 
    reduce . 21) ($:>>= reduce . 21) ($:<<= reduce . 21) ($:%= reduce . 21) (
    $:/= reduce . 21) ($:*= reduce . 21) ($:-= reduce . 21) ($:+= reduce . 21)
    ($:= reduce . 21) ($:? reduce . 21) ($:, reduce . 21) ($:|| reduce . 21) 
    ($:&& reduce . 21) ($:| reduce . 21) ($:^ reduce . 21) ($:& reduce . 21) (
    $:!= reduce . 21) ($:== reduce . 21) ($:< reduce . 21) ($:> reduce . 21) (
    $:<= reduce . 21) ($:>= reduce . 21) ($:>> reduce . 21) ($:<< reduce . 21)
    ($:+ reduce . 21) ($:- reduce . 21) ($:% reduce . 21) ($:/ reduce . 21) (
    $:* reduce . 21) (#{$:\x29;}# reduce . 21) (#{$:\x5d;}# reduce . 21) ($:: 
    reduce . 21) (#{$:\x7d;}# reduce . 21) ($end reduce . 21) (#{$:;}# reduce 
    . 21)) (($:|= reduce . 22) ($:^= reduce . 22) ($:&= reduce . 22) ($:>>= 
    reduce . 22) ($:<<= reduce . 22) ($:%= reduce . 22) ($:/= reduce . 22) (
    $:*= reduce . 22) ($:-= reduce . 22) ($:+= reduce . 22) ($:= reduce . 22) 
    ($:? reduce . 22) ($:, reduce . 22) ($:|| reduce . 22) ($:&& reduce . 22) 
    ($:| reduce . 22) ($:^ reduce . 22) ($:& reduce . 22) ($:!= reduce . 22) (
    $:== reduce . 22) ($:< reduce . 22) ($:> reduce . 22) ($:<= reduce . 22) (
    $:>= reduce . 22) ($:>> reduce . 22) ($:<< reduce . 22) ($:+ reduce . 22) 
    ($:- reduce . 22) ($:% reduce . 22) ($:/ reduce . 22) ($:* reduce . 22) (
    #{$:\x29;}# reduce . 22) (#{$:\x5d;}# reduce . 22) ($:: reduce . 22) (
    #{$:\x7d;}# reduce . 22) ($end reduce . 22) (#{$:;}# reduce . 22)) (($:* 
    reduce . 32) ($:/ reduce . 32) ($:% reduce . 32) ($:- reduce . 32) ($:+ 
    reduce . 32) ($:<< reduce . 32) ($:>> reduce . 32) ($:>= reduce . 32) (
    $:<= reduce . 32) ($:> reduce . 32) ($:< reduce . 32) ($:== reduce . 32) (
    $:!= reduce . 32) ($:& reduce . 32) ($:^ reduce . 32) ($:| reduce . 32) (
    $:&& reduce . 32) ($:? reduce . 32) ($:|| reduce . 32) (#{$:\x5d;}# reduce
    . 32) ($:|= reduce . 32) ($:^= reduce . 32) ($:&= reduce . 32) ($:>>= 
    reduce . 32) ($:<<= reduce . 32) ($:%= reduce . 32) ($:/= reduce . 32) (
    $:*= reduce . 32) ($:-= reduce . 32) ($:+= reduce . 32) ($:= reduce . 32) 
    ($:, reduce . 32) (#{$:\x29;}# reduce . 32) ($:: reduce . 32) (#{$:\x7d;}#
    reduce . 32) ($end reduce . 32) (#{$:;}# reduce . 32)) (($:|= reduce . 23
    ) ($:^= reduce . 23) ($:&= reduce . 23) ($:>>= reduce . 23) ($:<<= reduce 
    . 23) ($:%= reduce . 23) ($:/= reduce . 23) ($:*= reduce . 23) ($:-= 
    reduce . 23) ($:+= reduce . 23) ($:= reduce . 23) ($:? reduce . 23) ($:, 
    reduce . 23) ($:|| reduce . 23) ($:&& reduce . 23) ($:| reduce . 23) ($:^ 
    reduce . 23) ($:& reduce . 23) ($:!= reduce . 23) ($:== reduce . 23) ($:< 
    reduce . 23) ($:> reduce . 23) ($:<= reduce . 23) ($:>= reduce . 23) ($:>>
    reduce . 23) ($:<< reduce . 23) ($:+ reduce . 23) ($:- reduce . 23) ($:% 
    reduce . 23) ($:/ reduce . 23) ($:* reduce . 23) (#{$:\x29;}# reduce . 23)
    (#{$:\x5d;}# reduce . 23) ($:: reduce . 23) (#{$:\x7d;}# reduce . 23) (
    $end reduce . 23) (#{$:;}# reduce . 23)) (($:|= reduce . 24) ($:^= reduce 
    . 24) ($:&= reduce . 24) ($:>>= reduce . 24) ($:<<= reduce . 24) ($:%= 
    reduce . 24) ($:/= reduce . 24) ($:*= reduce . 24) ($:-= reduce . 24) (
    $:+= reduce . 24) ($:= reduce . 24) ($:? reduce . 24) ($:, reduce . 24) (
    $:|| reduce . 24) ($:&& reduce . 24) ($:| reduce . 24) ($:^ reduce . 24) (
    $:& reduce . 24) ($:!= reduce . 24) ($:== reduce . 24) ($:< reduce . 24) (
    $:> reduce . 24) ($:<= reduce . 24) ($:>= reduce . 24) ($:>> reduce . 24) 
    ($:<< reduce . 24) ($:+ reduce . 24) ($:- reduce . 24) ($:% reduce . 24) (
    $:/ reduce . 24) ($:* reduce . 24) (#{$:\x29;}# reduce . 24) (#{$:\x5d;}# 
    reduce . 24) ($:: reduce . 24) (#{$:\x7d;}# reduce . 24) ($end reduce . 24
    ) (#{$:;}# reduce . 24)) ((typename shift . 67) ($:enum shift . 68) (
    $:union shift . 69) ($:struct shift . 70) ($:_Complex shift . 71) (
    $:double shift . 72) ($:float shift . 73) ($:char shift . 74) ($:unsigned 
    shift . 75) ($:long shift . 76) ($:int shift . 77) ($:signed shift . 78) (
    $:short shift . 79) ($:inline shift . 80) ($:restrict shift . 81) (
    $:volatile shift . 82) ($:const shift . 83) (typedef-name shift . 84) (
    enum-specifier shift . 85) (struct-or-union-specifier shift . 86) (
    complex-type-specifier shift . 87) ($:_Bool shift . 88) (
    float-type-specifier shift . 89) (fixed-type-specifier shift . 90) ($:void
    shift . 91) ($:typedef shift . 92) ($:static shift . 93) ($:register 
    shift . 94) ($:extern shift . 95) ($:auto shift . 96) (function-specifier 
    shift . 97) (type-qualifier shift . 98) (type-specifier shift . 99) (
    storage-class-specifier shift . 100) (declaration-specifiers shift . 101) 
    (specifier-qualifier-list shift . 102) (type-name shift . 177) (
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 36) (expression shift . 66)) ((
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 36) (expression shift . 176)) (($string 
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift 
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25) 
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) (bitwise-xor-expression shift . 14) (
    bitwise-or-expression shift . 15) (logical-and-expression shift . 175)) ((
    #{$:\x29;}# shift . 174) ($:, shift . 38)) ((#{$:\x29;}# reduce . 230) (
    $:inline reduce . 230) ($:auto reduce . 230) ($:extern reduce . 230) (
    $:register reduce . 230) ($:static reduce . 230) ($:typedef reduce . 230) 
    ($:const reduce . 230) ($:volatile reduce . 230) ($:restrict reduce . 230)
    ($:void reduce . 230) ($:_Bool reduce . 230) (typename reduce . 230) (
    $:enum reduce . 230) ($:struct reduce . 230) ($:union reduce . 230) (
    $:_Complex reduce . 230) ($:float reduce . 230) ($:double reduce . 230) (
    $:long reduce . 230) ($:short reduce . 230) ($:signed reduce . 230) ($:int
    reduce . 230) ($:unsigned reduce . 230) ($:char reduce . 230) ($:* reduce
    . 230) (#{$:\x5b;}# reduce . 230) (#{$:\x28;}# reduce . 230) ($:, reduce 
    . 230) (cpp-ident reduce . 230) ($ident reduce . 230) ($:: reduce . 230)) 
    ((#{$:\x7b;}# shift . 172) (cpp-ident shift . 12) ($ident shift . 13) (
    identifier shift . 173)) ((#{$:\x7b;}# shift . 170) (cpp-ident shift . 12)
    ($ident shift . 13) (identifier shift . 171)) ((#{$:\x7b;}# shift . 168) 
    (cpp-ident shift . 12) ($ident shift . 13) (identifier shift . 169)) ((
    #{$:\x29;}# reduce . 137) ($:inline reduce . 137) ($:auto reduce . 137) (
    $:extern reduce . 137) ($:register reduce . 137) ($:static reduce . 137) (
    $:typedef reduce . 137) ($:const reduce . 137) ($:volatile reduce . 137) (
    $:restrict reduce . 137) ($:void reduce . 137) ($:_Bool reduce . 137) (
    typename reduce . 137) ($:enum reduce . 137) ($:struct reduce . 137) (
    $:union reduce . 137) ($:_Complex reduce . 137) ($:float reduce . 137) (
    $:double reduce . 137) ($:long reduce . 137) ($:short reduce . 137) (
    $:signed reduce . 137) ($:int reduce . 137) ($:unsigned reduce . 137) (
    $:char reduce . 137) ($:* reduce . 137) (#{$:\x5b;}# reduce . 137) (
    #{$:\x28;}# reduce . 137) ($:, reduce . 137) (cpp-ident reduce . 137) (
    $ident reduce . 137) ($:: reduce . 137)) (($:_Complex shift . 167) (
    #{$:\x29;}# reduce . 135) ($:inline reduce . 135) ($:auto reduce . 135) (
    $:extern reduce . 135) ($:register reduce . 135) ($:static reduce . 135) (
    $:typedef reduce . 135) ($:const reduce . 135) ($:volatile reduce . 135) (
    $:restrict reduce . 135) ($:void reduce . 135) ($:_Bool reduce . 135) (
    typename reduce . 135) ($:enum reduce . 135) ($:struct reduce . 135) (
    $:union reduce . 135) ($:float reduce . 135) ($:double reduce . 135) (
    $:long reduce . 135) ($:short reduce . 135) ($:signed reduce . 135) ($:int
    reduce . 135) ($:unsigned reduce . 135) ($:char reduce . 135) ($:* reduce
    . 135) (#{$:\x5b;}# reduce . 135) (#{$:\x28;}# reduce . 135) ($:, reduce 
    . 135) (cpp-ident reduce . 135) ($ident reduce . 135) ($:: reduce . 135)) 
    (($:_Complex shift . 166) (#{$:\x29;}# reduce . 134) ($:inline reduce . 
    134) ($:auto reduce . 134) ($:extern reduce . 134) ($:register reduce . 
    134) ($:static reduce . 134) ($:typedef reduce . 134) ($:const reduce . 
    134) ($:volatile reduce . 134) ($:restrict reduce . 134) ($:void reduce . 
    134) ($:_Bool reduce . 134) (typename reduce . 134) ($:enum reduce . 134) 
    ($:struct reduce . 134) ($:union reduce . 134) ($:float reduce . 134) (
    $:double reduce . 134) ($:long reduce . 134) ($:short reduce . 134) (
    $:signed reduce . 134) ($:int reduce . 134) ($:unsigned reduce . 134) (
    $:char reduce . 134) ($:* reduce . 134) (#{$:\x5b;}# reduce . 134) (
    #{$:\x28;}# reduce . 134) ($:, reduce . 134) (cpp-ident reduce . 134) (
    $ident reduce . 134) ($:: reduce . 134)) ((#{$:\x29;}# reduce . 131) (
    $:inline reduce . 131) ($:auto reduce . 131) ($:extern reduce . 131) (
    $:register reduce . 131) ($:static reduce . 131) ($:typedef reduce . 131) 
    ($:const reduce . 131) ($:volatile reduce . 131) ($:restrict reduce . 131)
    ($:void reduce . 131) ($:_Bool reduce . 131) (typename reduce . 131) (
    $:enum reduce . 131) ($:struct reduce . 131) ($:union reduce . 131) (
    $:_Complex reduce . 131) ($:float reduce . 131) ($:double reduce . 131) (
    $:long reduce . 131) ($:short reduce . 131) ($:signed reduce . 131) ($:int
    reduce . 131) ($:unsigned reduce . 131) ($:char reduce . 131) ($:* reduce
    . 131) (#{$:\x5b;}# reduce . 131) (#{$:\x28;}# reduce . 131) ($:, reduce 
    . 131) (cpp-ident reduce . 131) ($ident reduce . 131) ($:: reduce . 131)) 
    (($:short shift . 162) ($:int shift . 163) ($:long shift . 164) ($:char 
    shift . 165) (#{$:\x29;}# reduce . 126) ($:inline reduce . 126) ($:auto 
    reduce . 126) ($:extern reduce . 126) ($:register reduce . 126) ($:static 
    reduce . 126) ($:typedef reduce . 126) ($:const reduce . 126) ($:volatile 
    reduce . 126) ($:restrict reduce . 126) ($:void reduce . 126) ($:_Bool 
    reduce . 126) (typename reduce . 126) ($:enum reduce . 126) ($:struct 
    reduce . 126) ($:union reduce . 126) ($:_Complex reduce . 126) ($:float 
    reduce . 126) ($:double reduce . 126) ($:signed reduce . 126) ($:unsigned 
    reduce . 126) ($:* reduce . 126) (#{$:\x5b;}# reduce . 126) (#{$:\x28;}# 
    reduce . 126) ($:, reduce . 126) (cpp-ident reduce . 126) ($ident reduce 
    . 126) ($:: reduce . 126)) (($:int shift . 159) ($:long shift . 160) (
    $:double shift . 161) (#{$:\x29;}# reduce . 115) ($:inline reduce . 115) (
    $:auto reduce . 115) ($:extern reduce . 115) ($:register reduce . 115) (
    $:static reduce . 115) ($:typedef reduce . 115) ($:const reduce . 115) (
    $:volatile reduce . 115) ($:restrict reduce . 115) ($:void reduce . 115) (
    $:_Bool reduce . 115) (typename reduce . 115) ($:enum reduce . 115) (
    $:struct reduce . 115) ($:union reduce . 115) ($:_Complex reduce . 115) (
    $:float reduce . 115) ($:short reduce . 115) ($:signed reduce . 115) (
    $:unsigned reduce . 115) ($:char reduce . 115) ($:* reduce . 115) (
    #{$:\x5b;}# reduce . 115) (#{$:\x28;}# reduce . 115) ($:, reduce . 115) (
    cpp-ident reduce . 115) ($ident reduce . 115) ($:: reduce . 115)) ((
    #{$:\x29;}# reduce . 112) ($:inline reduce . 112) ($:auto reduce . 112) (
    $:extern reduce . 112) ($:register reduce . 112) ($:static reduce . 112) (
    $:typedef reduce . 112) ($:const reduce . 112) ($:volatile reduce . 112) (
    $:restrict reduce . 112) ($:void reduce . 112) ($:_Bool reduce . 112) (
    typename reduce . 112) ($:enum reduce . 112) ($:struct reduce . 112) (
    $:union reduce . 112) ($:_Complex reduce . 112) ($:float reduce . 112) (
    $:double reduce . 112) ($:long reduce . 112) ($:short reduce . 112) (
    $:signed reduce . 112) ($:int reduce . 112) ($:unsigned reduce . 112) (
    $:char reduce . 112) ($:* reduce . 112) (#{$:\x5b;}# reduce . 112) (
    #{$:\x28;}# reduce . 112) ($:, reduce . 112) (cpp-ident reduce . 112) (
    $ident reduce . 112) ($:: reduce . 112)) (($:short shift . 155) ($:int 
    shift . 156) ($:long shift . 157) ($:char shift . 158) (#{$:\x29;}# reduce
    . 113) ($:inline reduce . 113) ($:auto reduce . 113) ($:extern reduce . 
    113) ($:register reduce . 113) ($:static reduce . 113) ($:typedef reduce 
    . 113) ($:const reduce . 113) ($:volatile reduce . 113) ($:restrict reduce
    . 113) ($:void reduce . 113) ($:_Bool reduce . 113) (typename reduce . 
    113) ($:enum reduce . 113) ($:struct reduce . 113) ($:union reduce . 113) 
    ($:_Complex reduce . 113) ($:float reduce . 113) ($:double reduce . 113) (
    $:signed reduce . 113) ($:unsigned reduce . 113) ($:* reduce . 113) (
    #{$:\x5b;}# reduce . 113) (#{$:\x28;}# reduce . 113) ($:, reduce . 113) (
    cpp-ident reduce . 113) ($ident reduce . 113) ($:: reduce . 113)) (($:int 
    shift . 154) (#{$:\x29;}# reduce . 108) ($:inline reduce . 108) ($:auto 
    reduce . 108) ($:extern reduce . 108) ($:register reduce . 108) ($:static 
    reduce . 108) ($:typedef reduce . 108) ($:const reduce . 108) ($:volatile 
    reduce . 108) ($:restrict reduce . 108) ($:void reduce . 108) ($:_Bool 
    reduce . 108) (typename reduce . 108) ($:enum reduce . 108) ($:struct 
    reduce . 108) ($:union reduce . 108) ($:_Complex reduce . 108) ($:float 
    reduce . 108) ($:double reduce . 108) ($:long reduce . 108) ($:short 
    reduce . 108) ($:signed reduce . 108) ($:unsigned reduce . 108) ($:char 
    reduce . 108) ($:* reduce . 108) (#{$:\x5b;}# reduce . 108) (#{$:\x28;}# 
    reduce . 108) ($:, reduce . 108) (cpp-ident reduce . 108) ($ident reduce 
    . 108) ($:: reduce . 108)) (($:typedef reduce . 173) ($:static reduce . 
    173) ($:register reduce . 173) ($:extern reduce . 173) ($:auto reduce . 
    173) ($:char reduce . 173) ($:unsigned reduce . 173) ($:int reduce . 173) 
    ($:signed reduce . 173) ($:short reduce . 173) ($:long reduce . 173) (
    $:double reduce . 173) ($:float reduce . 173) ($:_Complex reduce . 173) (
    $:union reduce . 173) ($:struct reduce . 173) ($:enum reduce . 173) (
    typename reduce . 173) ($:_Bool reduce . 173) ($:void reduce . 173) (
    $:restrict reduce . 173) ($:volatile reduce . 173) ($:const reduce . 173) 
    ($:inline reduce . 173) (#{$:\x29;}# reduce . 173) ($ident reduce . 173) (
    cpp-ident reduce . 173) (#{$:\x28;}# reduce . 173) (#{$:\x5b;}# reduce . 
    173) ($:* reduce . 173) ($:, reduce . 173)) ((#{$:\x28;}# reduce . 172) (
    #{$:\x5b;}# reduce . 172) ($:* reduce . 172) ($:char reduce . 172) (
    $:unsigned reduce . 172) ($:int reduce . 172) ($:signed reduce . 172) (
    $:short reduce . 172) ($:long reduce . 172) ($:double reduce . 172) (
    $:float reduce . 172) ($:_Complex reduce . 172) ($:union reduce . 172) (
    $:struct reduce . 172) ($:enum reduce . 172) (typename reduce . 172) (
    $:_Bool reduce . 172) ($:void reduce . 172) ($:restrict reduce . 172) (
    $:volatile reduce . 172) ($:const reduce . 172) ($:typedef reduce . 172) (
    $:static reduce . 172) ($:register reduce . 172) ($:extern reduce . 172) (
    $:auto reduce . 172) ($:inline reduce . 172) (#{$:\x29;}# reduce . 172) (
    #{$:\x5d;}# reduce . 172) ($:++ reduce . 172) ($:-- reduce . 172) (
    $:sizeof reduce . 172) ($:& reduce . 172) ($:+ reduce . 172) ($:- reduce 
    . 172) ($:~ reduce . 172) ($:! reduce . 172) ($string reduce . 172) (
    $fixed reduce . 172) ($float reduce . 172) ($chlit reduce . 172) ($ident 
    reduce . 172) (cpp-ident reduce . 172) ($:, reduce . 172) ($:: reduce . 
    172)) ((#{$:\x28;}# reduce . 171) (#{$:\x5b;}# reduce . 171) ($:* reduce 
    . 171) ($:char reduce . 171) ($:unsigned reduce . 171) ($:int reduce . 171
    ) ($:signed reduce . 171) ($:short reduce . 171) ($:long reduce . 171) (
    $:double reduce . 171) ($:float reduce . 171) ($:_Complex reduce . 171) (
    $:union reduce . 171) ($:struct reduce . 171) ($:enum reduce . 171) (
    typename reduce . 171) ($:_Bool reduce . 171) ($:void reduce . 171) (
    $:restrict reduce . 171) ($:volatile reduce . 171) ($:const reduce . 171) 
    ($:typedef reduce . 171) ($:static reduce . 171) ($:register reduce . 171)
    ($:extern reduce . 171) ($:auto reduce . 171) ($:inline reduce . 171) (
    #{$:\x29;}# reduce . 171) (#{$:\x5d;}# reduce . 171) ($:++ reduce . 171) (
    $:-- reduce . 171) ($:sizeof reduce . 171) ($:& reduce . 171) ($:+ reduce 
    . 171) ($:- reduce . 171) ($:~ reduce . 171) ($:! reduce . 171) ($string 
    reduce . 171) ($fixed reduce . 171) ($float reduce . 171) ($chlit reduce 
    . 171) ($ident reduce . 171) (cpp-ident reduce . 171) ($:, reduce . 171) (
    $:: reduce . 171)) ((#{$:\x28;}# reduce . 170) (#{$:\x5b;}# reduce . 170) 
    ($:* reduce . 170) ($:char reduce . 170) ($:unsigned reduce . 170) ($:int 
    reduce . 170) ($:signed reduce . 170) ($:short reduce . 170) ($:long 
    reduce . 170) ($:double reduce . 170) ($:float reduce . 170) ($:_Complex 
    reduce . 170) ($:union reduce . 170) ($:struct reduce . 170) ($:enum 
    reduce . 170) (typename reduce . 170) ($:_Bool reduce . 170) ($:void 
    reduce . 170) ($:restrict reduce . 170) ($:volatile reduce . 170) ($:const
    reduce . 170) ($:typedef reduce . 170) ($:static reduce . 170) (
    $:register reduce . 170) ($:extern reduce . 170) ($:auto reduce . 170) (
    $:inline reduce . 170) (#{$:\x29;}# reduce . 170) (#{$:\x5d;}# reduce . 
    170) ($:++ reduce . 170) ($:-- reduce . 170) ($:sizeof reduce . 170) ($:& 
    reduce . 170) ($:+ reduce . 170) ($:- reduce . 170) ($:~ reduce . 170) (
    $:! reduce . 170) ($string reduce . 170) ($fixed reduce . 170) ($float 
    reduce . 170) ($chlit reduce . 170) ($ident reduce . 170) (cpp-ident 
    reduce . 170) ($:, reduce . 170) ($:: reduce . 170)) ((#{$:\x28;}# reduce 
    . 107) (#{$:\x5b;}# reduce . 107) ($:* reduce . 107) ($:char reduce . 107)
    ($:unsigned reduce . 107) ($:int reduce . 107) ($:signed reduce . 107) (
    $:short reduce . 107) ($:long reduce . 107) ($:double reduce . 107) (
    $:float reduce . 107) ($:_Complex reduce . 107) ($:union reduce . 107) (
    $:struct reduce . 107) ($:enum reduce . 107) (typename reduce . 107) (
    $:_Bool reduce . 107) ($:void reduce . 107) ($:restrict reduce . 107) (
    $:volatile reduce . 107) ($:const reduce . 107) ($:typedef reduce . 107) (
    $:static reduce . 107) ($:register reduce . 107) ($:extern reduce . 107) (
    $:auto reduce . 107) ($:inline reduce . 107) (#{$:\x29;}# reduce . 107) (
    $ident reduce . 107) (cpp-ident reduce . 107) ($:, reduce . 107) ($:: 
    reduce . 107)) ((#{$:\x28;}# reduce . 106) (#{$:\x5b;}# reduce . 106) ($:*
    reduce . 106) ($:char reduce . 106) ($:unsigned reduce . 106) ($:int 
    reduce . 106) ($:signed reduce . 106) ($:short reduce . 106) ($:long 
    reduce . 106) ($:double reduce . 106) ($:float reduce . 106) ($:_Complex 
    reduce . 106) ($:union reduce . 106) ($:struct reduce . 106) ($:enum 
    reduce . 106) (typename reduce . 106) ($:_Bool reduce . 106) ($:void 
    reduce . 106) ($:restrict reduce . 106) ($:volatile reduce . 106) ($:const
    reduce . 106) ($:typedef reduce . 106) ($:static reduce . 106) (
    $:register reduce . 106) ($:extern reduce . 106) ($:auto reduce . 106) (
    $:inline reduce . 106) (#{$:\x29;}# reduce . 106) ($ident reduce . 106) (
    cpp-ident reduce . 106) ($:, reduce . 106) ($:: reduce . 106)) ((
    #{$:\x28;}# reduce . 105) (#{$:\x5b;}# reduce . 105) ($:* reduce . 105) (
    $:char reduce . 105) ($:unsigned reduce . 105) ($:int reduce . 105) (
    $:signed reduce . 105) ($:short reduce . 105) ($:long reduce . 105) (
    $:double reduce . 105) ($:float reduce . 105) ($:_Complex reduce . 105) (
    $:union reduce . 105) ($:struct reduce . 105) ($:enum reduce . 105) (
    typename reduce . 105) ($:_Bool reduce . 105) ($:void reduce . 105) (
    $:restrict reduce . 105) ($:volatile reduce . 105) ($:const reduce . 105) 
    ($:typedef reduce . 105) ($:static reduce . 105) ($:register reduce . 105)
    ($:extern reduce . 105) ($:auto reduce . 105) ($:inline reduce . 105) (
    #{$:\x29;}# reduce . 105) ($ident reduce . 105) (cpp-ident reduce . 105) (
    $:, reduce . 105) ($:: reduce . 105)) ((#{$:\x28;}# reduce . 104) (
    #{$:\x5b;}# reduce . 104) ($:* reduce . 104) ($:char reduce . 104) (
    $:unsigned reduce . 104) ($:int reduce . 104) ($:signed reduce . 104) (
    $:short reduce . 104) ($:long reduce . 104) ($:double reduce . 104) (
    $:float reduce . 104) ($:_Complex reduce . 104) ($:union reduce . 104) (
    $:struct reduce . 104) ($:enum reduce . 104) (typename reduce . 104) (
    $:_Bool reduce . 104) ($:void reduce . 104) ($:restrict reduce . 104) (
    $:volatile reduce . 104) ($:const reduce . 104) ($:typedef reduce . 104) (
    $:static reduce . 104) ($:register reduce . 104) ($:extern reduce . 104) (
    $:auto reduce . 104) ($:inline reduce . 104) (#{$:\x29;}# reduce . 104) (
    $ident reduce . 104) (cpp-ident reduce . 104) ($:, reduce . 104) ($:: 
    reduce . 104)) ((#{$:\x28;}# reduce . 103) (#{$:\x5b;}# reduce . 103) ($:*
    reduce . 103) ($:char reduce . 103) ($:unsigned reduce . 103) ($:int 
    reduce . 103) ($:signed reduce . 103) ($:short reduce . 103) ($:long 
    reduce . 103) ($:double reduce . 103) ($:float reduce . 103) ($:_Complex 
    reduce . 103) ($:union reduce . 103) ($:struct reduce . 103) ($:enum 
    reduce . 103) (typename reduce . 103) ($:_Bool reduce . 103) ($:void 
    reduce . 103) ($:restrict reduce . 103) ($:volatile reduce . 103) ($:const
    reduce . 103) ($:typedef reduce . 103) ($:static reduce . 103) (
    $:register reduce . 103) ($:extern reduce . 103) ($:auto reduce . 103) (
    $:inline reduce . 103) (#{$:\x29;}# reduce . 103) ($ident reduce . 103) (
    cpp-ident reduce . 103) ($:, reduce . 103) ($:: reduce . 103)) ((
    #{$:\x28;}# reduce . 102) (#{$:\x5b;}# reduce . 102) ($:* reduce . 102) (
    $:char reduce . 102) ($:unsigned reduce . 102) ($:int reduce . 102) (
    $:signed reduce . 102) ($:short reduce . 102) ($:long reduce . 102) (
    $:double reduce . 102) ($:float reduce . 102) ($:_Complex reduce . 102) (
    $:union reduce . 102) ($:struct reduce . 102) ($:enum reduce . 102) (
    typename reduce . 102) ($:_Bool reduce . 102) ($:void reduce . 102) (
    $:restrict reduce . 102) ($:volatile reduce . 102) ($:const reduce . 102) 
    ($:typedef reduce . 102) ($:static reduce . 102) ($:register reduce . 102)
    ($:extern reduce . 102) ($:auto reduce . 102) ($:inline reduce . 102) (
    #{$:\x29;}# reduce . 102) ($ident reduce . 102) (cpp-ident reduce . 102) (
    $:, reduce . 102) ($:: reduce . 102)) ((#{$:\x28;}# reduce . 101) (
    #{$:\x5b;}# reduce . 101) ($:* reduce . 101) ($:char reduce . 101) (
    $:unsigned reduce . 101) ($:int reduce . 101) ($:signed reduce . 101) (
    $:short reduce . 101) ($:long reduce . 101) ($:double reduce . 101) (
    $:float reduce . 101) ($:_Complex reduce . 101) ($:union reduce . 101) (
    $:struct reduce . 101) ($:enum reduce . 101) (typename reduce . 101) (
    $:_Bool reduce . 101) ($:void reduce . 101) ($:restrict reduce . 101) (
    $:volatile reduce . 101) ($:const reduce . 101) ($:typedef reduce . 101) (
    $:static reduce . 101) ($:register reduce . 101) ($:extern reduce . 101) (
    $:auto reduce . 101) ($:inline reduce . 101) (#{$:\x29;}# reduce . 101) (
    $ident reduce . 101) (cpp-ident reduce . 101) ($:, reduce . 101) ($:: 
    reduce . 101)) ((#{$:\x28;}# reduce . 100) (#{$:\x5b;}# reduce . 100) ($:*
    reduce . 100) ($:char reduce . 100) ($:unsigned reduce . 100) ($:int 
    reduce . 100) ($:signed reduce . 100) ($:short reduce . 100) ($:long 
    reduce . 100) ($:double reduce . 100) ($:float reduce . 100) ($:_Complex 
    reduce . 100) ($:union reduce . 100) ($:struct reduce . 100) ($:enum 
    reduce . 100) (typename reduce . 100) ($:_Bool reduce . 100) ($:void 
    reduce . 100) ($:restrict reduce . 100) ($:volatile reduce . 100) ($:const
    reduce . 100) ($:typedef reduce . 100) ($:static reduce . 100) (
    $:register reduce . 100) ($:extern reduce . 100) ($:auto reduce . 100) (
    $:inline reduce . 100) (#{$:\x29;}# reduce . 100) ($ident reduce . 100) (
    cpp-ident reduce . 100) ($:, reduce . 100) ($:: reduce . 100)) (($:typedef
    reduce . 99) ($:static reduce . 99) ($:register reduce . 99) ($:extern 
    reduce . 99) ($:auto reduce . 99) ($:char reduce . 99) ($:unsigned reduce 
    . 99) ($:int reduce . 99) ($:signed reduce . 99) ($:short reduce . 99) (
    $:long reduce . 99) ($:double reduce . 99) ($:float reduce . 99) (
    $:_Complex reduce . 99) ($:union reduce . 99) ($:struct reduce . 99) (
    $:enum reduce . 99) (typename reduce . 99) ($:_Bool reduce . 99) ($:void 
    reduce . 99) ($:restrict reduce . 99) ($:volatile reduce . 99) ($:const 
    reduce . 99) ($:inline reduce . 99) (#{$:\x29;}# reduce . 99) ($ident 
    reduce . 99) (cpp-ident reduce . 99) (#{$:\x28;}# reduce . 99) (
    #{$:\x5b;}# reduce . 99) ($:* reduce . 99) ($:, reduce . 99)) (($:typedef 
    reduce . 98) ($:static reduce . 98) ($:register reduce . 98) ($:extern 
    reduce . 98) ($:auto reduce . 98) ($:char reduce . 98) ($:unsigned reduce 
    . 98) ($:int reduce . 98) ($:signed reduce . 98) ($:short reduce . 98) (
    $:long reduce . 98) ($:double reduce . 98) ($:float reduce . 98) (
    $:_Complex reduce . 98) ($:union reduce . 98) ($:struct reduce . 98) (
    $:enum reduce . 98) (typename reduce . 98) ($:_Bool reduce . 98) ($:void 
    reduce . 98) ($:restrict reduce . 98) ($:volatile reduce . 98) ($:const 
    reduce . 98) ($:inline reduce . 98) (#{$:\x29;}# reduce . 98) ($ident 
    reduce . 98) (cpp-ident reduce . 98) (#{$:\x28;}# reduce . 98) (
    #{$:\x5b;}# reduce . 98) ($:* reduce . 98) ($:, reduce . 98)) (($:typedef 
    reduce . 97) ($:static reduce . 97) ($:register reduce . 97) ($:extern 
    reduce . 97) ($:auto reduce . 97) ($:char reduce . 97) ($:unsigned reduce 
    . 97) ($:int reduce . 97) ($:signed reduce . 97) ($:short reduce . 97) (
    $:long reduce . 97) ($:double reduce . 97) ($:float reduce . 97) (
    $:_Complex reduce . 97) ($:union reduce . 97) ($:struct reduce . 97) (
    $:enum reduce . 97) (typename reduce . 97) ($:_Bool reduce . 97) ($:void 
    reduce . 97) ($:restrict reduce . 97) ($:volatile reduce . 97) ($:const 
    reduce . 97) ($:inline reduce . 97) (#{$:\x29;}# reduce . 97) ($ident 
    reduce . 97) (cpp-ident reduce . 97) (#{$:\x28;}# reduce . 97) (
    #{$:\x5b;}# reduce . 97) ($:* reduce . 97) ($:, reduce . 97)) (($:typedef 
    reduce . 96) ($:static reduce . 96) ($:register reduce . 96) ($:extern 
    reduce . 96) ($:auto reduce . 96) ($:char reduce . 96) ($:unsigned reduce 
    . 96) ($:int reduce . 96) ($:signed reduce . 96) ($:short reduce . 96) (
    $:long reduce . 96) ($:double reduce . 96) ($:float reduce . 96) (
    $:_Complex reduce . 96) ($:union reduce . 96) ($:struct reduce . 96) (
    $:enum reduce . 96) (typename reduce . 96) ($:_Bool reduce . 96) ($:void 
    reduce . 96) ($:restrict reduce . 96) ($:volatile reduce . 96) ($:const 
    reduce . 96) ($:inline reduce . 96) (#{$:\x29;}# reduce . 96) ($ident 
    reduce . 96) (cpp-ident reduce . 96) (#{$:\x28;}# reduce . 96) (
    #{$:\x5b;}# reduce . 96) ($:* reduce . 96) ($:, reduce . 96)) (($:typedef 
    reduce . 95) ($:static reduce . 95) ($:register reduce . 95) ($:extern 
    reduce . 95) ($:auto reduce . 95) ($:char reduce . 95) ($:unsigned reduce 
    . 95) ($:int reduce . 95) ($:signed reduce . 95) ($:short reduce . 95) (
    $:long reduce . 95) ($:double reduce . 95) ($:float reduce . 95) (
    $:_Complex reduce . 95) ($:union reduce . 95) ($:struct reduce . 95) (
    $:enum reduce . 95) (typename reduce . 95) ($:_Bool reduce . 95) ($:void 
    reduce . 95) ($:restrict reduce . 95) ($:volatile reduce . 95) ($:const 
    reduce . 95) ($:inline reduce . 95) (#{$:\x29;}# reduce . 95) ($ident 
    reduce . 95) (cpp-ident reduce . 95) (#{$:\x28;}# reduce . 95) (
    #{$:\x5b;}# reduce . 95) ($:* reduce . 95) ($:, reduce . 95)) ((typename 
    shift . 67) ($:enum shift . 68) ($:union shift . 69) ($:struct shift . 70)
    ($:_Complex shift . 71) ($:double shift . 72) ($:float shift . 73) (
    $:char shift . 74) ($:unsigned shift . 75) ($:long shift . 76) ($:int 
    shift . 77) ($:signed shift . 78) ($:short shift . 79) ($:inline shift . 
    80) ($:restrict shift . 81) ($:volatile shift . 82) ($:const shift . 83) (
    typedef-name shift . 84) (enum-specifier shift . 85) (
    struct-or-union-specifier shift . 86) (complex-type-specifier shift . 87) 
    ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) ($:typedef shift . 92
    ) ($:static shift . 93) ($:register shift . 94) ($:extern shift . 95) (
    $:auto shift . 96) (function-specifier shift . 97) (type-qualifier shift 
    . 146) (type-specifier shift . 147) (storage-class-specifier shift . 100) 
    (declaration-specifiers shift . 153) (#{$:\x29;}# reduce . 89) ($:, reduce
    . 89) ($:* reduce . 89) (#{$:\x5b;}# reduce . 89) (#{$:\x28;}# reduce . 
    89) (cpp-ident reduce . 89) ($ident reduce . 89)) (($:inline shift . 80) (
    $:typedef shift . 92) ($:static shift . 93) ($:register shift . 94) (
    $:extern shift . 95) ($:auto shift . 96) (function-specifier shift . 97) (
    storage-class-specifier shift . 100) (declaration-specifiers shift . 151) 
    (typename shift . 67) ($:enum shift . 68) ($:union shift . 69) ($:struct 
    shift . 70) ($:_Complex shift . 71) ($:double shift . 72) ($:float shift 
    . 73) ($:char shift . 74) ($:unsigned shift . 75) ($:long shift . 76) (
    $:int shift . 77) ($:signed shift . 78) ($:short shift . 79) ($:restrict 
    shift . 81) ($:volatile shift . 82) ($:const shift . 83) (typedef-name 
    shift . 84) (enum-specifier shift . 85) (struct-or-union-specifier shift 
    . 86) (complex-type-specifier shift . 87) ($:_Bool shift . 88) (
    float-type-specifier shift . 89) (fixed-type-specifier shift . 90) ($:void
    shift . 91) (type-qualifier shift . 98) (type-specifier shift . 99) (
    specifier-qualifier-list shift . 152) (#{$:\x29;}# reduce . 87) ($:* 
    reduce . 155) (#{$:\x5b;}# reduce . 155) (#{$:\x28;}# reduce . 155)) ((
    $:inline shift . 80) ($:typedef shift . 92) ($:static shift . 93) (
    $:register shift . 94) ($:extern shift . 95) ($:auto shift . 96) (
    function-specifier shift . 97) (storage-class-specifier shift . 100) (
    declaration-specifiers shift . 149) (typename shift . 67) ($:enum shift . 
    68) ($:union shift . 69) ($:struct shift . 70) ($:_Complex shift . 71) (
    $:double shift . 72) ($:float shift . 73) ($:char shift . 74) ($:unsigned 
    shift . 75) ($:long shift . 76) ($:int shift . 77) ($:signed shift . 78) (
    $:short shift . 79) ($:restrict shift . 81) ($:volatile shift . 82) (
    $:const shift . 83) (typedef-name shift . 84) (enum-specifier shift . 85) 
    (struct-or-union-specifier shift . 86) (complex-type-specifier shift . 87)
    ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) (type-qualifier shift
    . 98) (type-specifier shift . 99) (specifier-qualifier-list shift . 150) 
    (#{$:\x29;}# reduce . 85) ($:* reduce . 153) (#{$:\x5b;}# reduce . 153) (
    #{$:\x28;}# reduce . 153)) ((typename shift . 67) ($:enum shift . 68) (
    $:union shift . 69) ($:struct shift . 70) ($:_Complex shift . 71) (
    $:double shift . 72) ($:float shift . 73) ($:char shift . 74) ($:unsigned 
    shift . 75) ($:long shift . 76) ($:int shift . 77) ($:signed shift . 78) (
    $:short shift . 79) ($:inline shift . 80) ($:restrict shift . 81) (
    $:volatile shift . 82) ($:const shift . 83) (typedef-name shift . 84) (
    enum-specifier shift . 85) (struct-or-union-specifier shift . 86) (
    complex-type-specifier shift . 87) ($:_Bool shift . 88) (
    float-type-specifier shift . 89) (fixed-type-specifier shift . 90) ($:void
    shift . 91) ($:typedef shift . 92) ($:static shift . 93) ($:register 
    shift . 94) ($:extern shift . 95) ($:auto shift . 96) (function-specifier 
    shift . 97) (type-qualifier shift . 146) (type-specifier shift . 147) (
    storage-class-specifier shift . 100) (declaration-specifiers shift . 148) 
    (#{$:\x29;}# reduce . 83) ($:, reduce . 83) ($:* reduce . 83) (#{$:\x5b;}#
    reduce . 83) (#{$:\x28;}# reduce . 83) (cpp-ident reduce . 83) ($ident 
    reduce . 83)) ((#{$:\x29;}# reduce . 205)) ((#{$:\x5b;}# shift . 140) (
    #{$:\x28;}# shift . 141) ($:* shift . 142) (direct-abstract-declarator 
    shift . 143) (pointer shift . 144) (abstract-declarator shift . 145)) ((
    #{$:\x29;}# shift . 139)) (($string shift . 8) ($chlit shift . 9) ($float 
    shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident shift . 13)
    (string-literal shift . 16) (constant shift . 17) (identifier shift . 18)
    ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* 
    shift . 24) ($:& shift . 25) (primary-expression shift . 27) ($:sizeof 
    shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ shift . 32
    ) (postfix-expression shift . 33) (#{$:\x28;}# shift . 26) (
    unary-expression shift . 60) (cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) (
    bitwise-xor-expression shift . 14) (bitwise-or-expression shift . 138)) ((
    $:= reduce . 297) ($:+= reduce . 297) ($:-= reduce . 297) ($:*= reduce . 
    297) ($:/= reduce . 297) ($:%= reduce . 297) ($:<<= reduce . 297) ($:>>= 
    reduce . 297) ($:&= reduce . 297) ($:^= reduce . 297) ($:|= reduce . 297) 
    ($:-- reduce . 297) ($:++ reduce . 297) ($:-> reduce . 297) ($:. reduce . 
    297) (#{$:\x28;}# reduce . 297) (#{$:\x5b;}# reduce . 297) ($string reduce
    . 297) ($:? reduce . 297) ($:* reduce . 297) ($:/ reduce . 297) ($:% 
    reduce . 297) ($:- reduce . 297) ($:+ reduce . 297) ($:<< reduce . 297) (
    $:>> reduce . 297) ($:>= reduce . 297) ($:<= reduce . 297) ($:> reduce . 
    297) ($:< reduce . 297) ($:== reduce . 297) ($:!= reduce . 297) ($:& 
    reduce . 297) ($:^ reduce . 297) ($:| reduce . 297) ($:&& reduce . 297) (
    $:|| reduce . 297) ($:, reduce . 297) (#{$:\x29;}# reduce . 297) (
    #{$:\x5d;}# reduce . 297) ($:: reduce . 297) (#{$:\x7d;}# reduce . 297) (
    $end reduce . 297) (#{$:;}# reduce . 297)) (($string shift . 8) ($chlit 
    shift . 9) ($float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) 
    ($ident shift . 13) (string-literal shift . 16) (constant shift . 17) (
    identifier shift . 18) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) 
    ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25) (primary-expression 
    shift . 27) ($:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift 
    . 31) ($:++ shift . 32) (postfix-expression shift . 33) (#{$:\x28;}# shift
    . 26) (unary-expression shift . 60) (cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) (
    bitwise-xor-expression shift . 137)) (($string shift . 8) ($chlit shift . 
    9) ($float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift 
    . 23) ($:* shift . 24) ($:& shift . 25) (primary-expression shift . 27) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (#{$:\x28;}# shift . 26) (
    unary-expression shift . 60) (cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 136)) ((
    $string shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 
    11) (cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16)
    (constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25)
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 135)) ((
    $string shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 
    11) (cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16)
    (constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25)
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 134)) (($string shift . 8) ($chlit shift . 9
    ) ($float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift 
    . 23) ($:* shift . 24) ($:& shift . 25) (primary-expression shift . 27) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (#{$:\x28;}# shift . 26) (
    unary-expression shift . 60) (cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 133)) (($string
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift 
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25) 
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 132)) (($string 
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift 
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25) 
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 131)) (($string 
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift 
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25) 
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 130)) (($string 
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift 
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25) 
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 129)) (($string 
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift 
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25) 
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 128)) (($string shift . 8) ($chlit shift . 9) 
    ($float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift 
    . 23) ($:* shift . 24) ($:& shift . 25) (primary-expression shift . 27) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (#{$:\x28;}# shift . 26) (
    unary-expression shift . 60) (cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 127)) ((
    $string shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 
    11) (cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16)
    (constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25)
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (
    cast-expression shift . 1) (multiplicative-expression shift . 126)) ((
    $string shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 
    11) (cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16)
    (constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25)
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (
    cast-expression shift . 1) (multiplicative-expression shift . 125)) ((
    $string shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 
    11) (cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16)
    (constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25)
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (
    cast-expression shift . 124)) (($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift 
    . 23) ($:* shift . 24) ($:& shift . 25) (primary-expression shift . 27) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (#{$:\x28;}# shift . 26) (
    unary-expression shift . 60) (cast-expression shift . 123)) (($string 
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift 
    . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25) 
    (primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (
    cast-expression shift . 122)) (($:? reduce . 37) ($:, reduce . 37) ($:|| 
    reduce . 37) ($:&& reduce . 37) ($:| reduce . 37) ($:^ reduce . 37) ($:& 
    reduce . 37) ($:!= reduce . 37) ($:== reduce . 37) ($:< reduce . 37) ($:> 
    reduce . 37) ($:<= reduce . 37) ($:>= reduce . 37) ($:>> reduce . 37) (
    $:<< reduce . 37) ($:+ reduce . 37) ($:- reduce . 37) ($:% reduce . 37) (
    $:/ reduce . 37) ($:* reduce . 37) (#{$:\x29;}# reduce . 37) (#{$:\x5d;}# 
    reduce . 37) ($:: reduce . 37) (#{$:\x7d;}# reduce . 37) ($end reduce . 37
    ) (#{$:;}# reduce . 37)) (($:? reduce . 36) ($:, reduce . 36) ($:|| reduce
    . 36) ($:&& reduce . 36) ($:| reduce . 36) ($:^ reduce . 36) ($:& reduce 
    . 36) ($:!= reduce . 36) ($:== reduce . 36) ($:< reduce . 36) ($:> reduce 
    . 36) ($:<= reduce . 36) ($:>= reduce . 36) ($:>> reduce . 36) ($:<< 
    reduce . 36) ($:+ reduce . 36) ($:- reduce . 36) ($:% reduce . 36) ($:/ 
    reduce . 36) ($:* reduce . 36) (#{$:\x29;}# reduce . 36) (#{$:\x5d;}# 
    reduce . 36) ($:: reduce . 36) (#{$:\x7d;}# reduce . 36) ($end reduce . 36
    ) (#{$:;}# reduce . 36)) (($:? reduce . 35) ($:, reduce . 35) ($:|| reduce
    . 35) ($:&& reduce . 35) ($:| reduce . 35) ($:^ reduce . 35) ($:& reduce 
    . 35) ($:!= reduce . 35) ($:== reduce . 35) ($:< reduce . 35) ($:> reduce 
    . 35) ($:<= reduce . 35) ($:>= reduce . 35) ($:>> reduce . 35) ($:<< 
    reduce . 35) ($:+ reduce . 35) ($:- reduce . 35) ($:% reduce . 35) ($:/ 
    reduce . 35) ($:* reduce . 35) (#{$:\x29;}# reduce . 35) (#{$:\x5d;}# 
    reduce . 35) ($:: reduce . 35) (#{$:\x7d;}# reduce . 35) ($end reduce . 35
    ) (#{$:;}# reduce . 35)) (($:* shift . 119) ($:/ shift . 120) ($:% shift 
    . 121) ($:? reduce . 40) ($:- reduce . 40) ($:+ reduce . 40) ($:<< reduce 
    . 40) ($:>> reduce . 40) ($:>= reduce . 40) ($:<= reduce . 40) ($:> reduce
    . 40) ($:< reduce . 40) ($:== reduce . 40) ($:!= reduce . 40) ($:& reduce
    . 40) ($:^ reduce . 40) ($:| reduce . 40) ($:&& reduce . 40) ($:|| reduce
    . 40) ($:, reduce . 40) (#{$:\x29;}# reduce . 40) (#{$:\x5d;}# reduce . 
    40) ($:: reduce . 40) (#{$:\x7d;}# reduce . 40) ($end reduce . 40) (
    #{$:;}# reduce . 40)) (($:* shift . 119) ($:/ shift . 120) ($:% shift . 
    121) ($:? reduce . 39) ($:- reduce . 39) ($:+ reduce . 39) ($:<< reduce . 
    39) ($:>> reduce . 39) ($:>= reduce . 39) ($:<= reduce . 39) ($:> reduce 
    . 39) ($:< reduce . 39) ($:== reduce . 39) ($:!= reduce . 39) ($:& reduce 
    . 39) ($:^ reduce . 39) ($:| reduce . 39) ($:&& reduce . 39) ($:|| reduce 
    . 39) ($:, reduce . 39) (#{$:\x29;}# reduce . 39) (#{$:\x5d;}# reduce . 39
    ) ($:: reduce . 39) (#{$:\x7d;}# reduce . 39) ($end reduce . 39) (#{$:;}# 
    reduce . 39)) (($:+ shift . 117) ($:- shift . 118) ($:? reduce . 43) ($:, 
    reduce . 43) ($:|| reduce . 43) ($:&& reduce . 43) ($:| reduce . 43) ($:^ 
    reduce . 43) ($:& reduce . 43) ($:!= reduce . 43) ($:== reduce . 43) ($:< 
    reduce . 43) ($:> reduce . 43) ($:<= reduce . 43) ($:>= reduce . 43) ($:>>
    reduce . 43) ($:<< reduce . 43) (#{$:\x29;}# reduce . 43) (#{$:\x5d;}# 
    reduce . 43) ($:: reduce . 43) (#{$:\x7d;}# reduce . 43) ($end reduce . 43
    ) (#{$:;}# reduce . 43)) (($:+ shift . 117) ($:- shift . 118) ($:? reduce 
    . 42) ($:, reduce . 42) ($:|| reduce . 42) ($:&& reduce . 42) ($:| reduce 
    . 42) ($:^ reduce . 42) ($:& reduce . 42) ($:!= reduce . 42) ($:== reduce 
    . 42) ($:< reduce . 42) ($:> reduce . 42) ($:<= reduce . 42) ($:>= reduce 
    . 42) ($:>> reduce . 42) ($:<< reduce . 42) (#{$:\x29;}# reduce . 42) (
    #{$:\x5d;}# reduce . 42) ($:: reduce . 42) (#{$:\x7d;}# reduce . 42) ($end
    reduce . 42) (#{$:;}# reduce . 42)) (($:<< shift . 115) ($:>> shift . 116
    ) ($:? reduce . 48) ($:>= reduce . 48) ($:<= reduce . 48) ($:> reduce . 48
    ) ($:< reduce . 48) ($:== reduce . 48) ($:!= reduce . 48) ($:& reduce . 48
    ) ($:^ reduce . 48) ($:| reduce . 48) ($:&& reduce . 48) ($:|| reduce . 48
    ) ($:, reduce . 48) (#{$:\x29;}# reduce . 48) (#{$:\x5d;}# reduce . 48) (
    $:: reduce . 48) (#{$:\x7d;}# reduce . 48) ($end reduce . 48) (#{$:;}# 
    reduce . 48)) (($:<< shift . 115) ($:>> shift . 116) ($:? reduce . 47) (
    $:>= reduce . 47) ($:<= reduce . 47) ($:> reduce . 47) ($:< reduce . 47) (
    $:== reduce . 47) ($:!= reduce . 47) ($:& reduce . 47) ($:^ reduce . 47) (
    $:| reduce . 47) ($:&& reduce . 47) ($:|| reduce . 47) ($:, reduce . 47) (
    #{$:\x29;}# reduce . 47) (#{$:\x5d;}# reduce . 47) ($:: reduce . 47) (
    #{$:\x7d;}# reduce . 47) ($end reduce . 47) (#{$:;}# reduce . 47)) (($:<< 
    shift . 115) ($:>> shift . 116) ($:? reduce . 46) ($:>= reduce . 46) ($:<=
    reduce . 46) ($:> reduce . 46) ($:< reduce . 46) ($:== reduce . 46) ($:!=
    reduce . 46) ($:& reduce . 46) ($:^ reduce . 46) ($:| reduce . 46) ($:&& 
    reduce . 46) ($:|| reduce . 46) ($:, reduce . 46) (#{$:\x29;}# reduce . 46
    ) (#{$:\x5d;}# reduce . 46) ($:: reduce . 46) (#{$:\x7d;}# reduce . 46) (
    $end reduce . 46) (#{$:;}# reduce . 46)) (($:<< shift . 115) ($:>> shift 
    . 116) ($:? reduce . 45) ($:>= reduce . 45) ($:<= reduce . 45) ($:> reduce
    . 45) ($:< reduce . 45) ($:== reduce . 45) ($:!= reduce . 45) ($:& reduce
    . 45) ($:^ reduce . 45) ($:| reduce . 45) ($:&& reduce . 45) ($:|| reduce
    . 45) ($:, reduce . 45) (#{$:\x29;}# reduce . 45) (#{$:\x5d;}# reduce . 
    45) ($:: reduce . 45) (#{$:\x7d;}# reduce . 45) ($end reduce . 45) (
    #{$:;}# reduce . 45)) (($:< shift . 111) ($:> shift . 112) ($:<= shift . 
    113) ($:>= shift . 114) ($:? reduce . 51) ($:, reduce . 51) ($:|| reduce 
    . 51) ($:&& reduce . 51) ($:| reduce . 51) ($:^ reduce . 51) ($:& reduce 
    . 51) ($:!= reduce . 51) ($:== reduce . 51) (#{$:\x29;}# reduce . 51) (
    #{$:\x5d;}# reduce . 51) ($:: reduce . 51) (#{$:\x7d;}# reduce . 51) ($end
    reduce . 51) (#{$:;}# reduce . 51)) (($:< shift . 111) ($:> shift . 112) 
    ($:<= shift . 113) ($:>= shift . 114) ($:? reduce . 50) ($:, reduce . 50) 
    ($:|| reduce . 50) ($:&& reduce . 50) ($:| reduce . 50) ($:^ reduce . 50) 
    ($:& reduce . 50) ($:!= reduce . 50) ($:== reduce . 50) (#{$:\x29;}# 
    reduce . 50) (#{$:\x5d;}# reduce . 50) ($:: reduce . 50) (#{$:\x7d;}# 
    reduce . 50) ($end reduce . 50) (#{$:;}# reduce . 50)) (($:== shift . 109)
    ($:!= shift . 110) ($:? reduce . 53) ($:& reduce . 53) ($:^ reduce . 53) 
    ($:| reduce . 53) ($:&& reduce . 53) ($:|| reduce . 53) ($:, reduce . 53) 
    (#{$:\x29;}# reduce . 53) (#{$:\x5d;}# reduce . 53) ($:: reduce . 53) (
    #{$:\x7d;}# reduce . 53) ($end reduce . 53) (#{$:;}# reduce . 53)) (($:& 
    shift . 108) ($:? reduce . 55) ($:, reduce . 55) ($:|| reduce . 55) ($:&& 
    reduce . 55) ($:| reduce . 55) ($:^ reduce . 55) (#{$:\x29;}# reduce . 55)
    (#{$:\x5d;}# reduce . 55) ($:: reduce . 55) (#{$:\x7d;}# reduce . 55) (
    $end reduce . 55) (#{$:;}# reduce . 55)) (($:^ shift . 107) ($:? reduce . 
    57) ($:| reduce . 57) ($:&& reduce . 57) ($:|| reduce . 57) ($:, reduce . 
    57) (#{$:\x29;}# reduce . 57) (#{$:\x5d;}# reduce . 57) ($:: reduce . 57) 
    (#{$:\x7d;}# reduce . 57) ($end reduce . 57) (#{$:;}# reduce . 57)) (($:| 
    shift . 106) ($:? reduce . 59) ($:, reduce . 59) ($:|| reduce . 59) ($:&& 
    reduce . 59) (#{$:\x29;}# reduce . 59) (#{$:\x5d;}# reduce . 59) ($:: 
    reduce . 59) (#{$:\x7d;}# reduce . 59) ($end reduce . 59) (#{$:;}# reduce 
    . 59)) ((#{$:\x7b;}# shift . 233) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift 
    . 23) ($:* shift . 24) ($:& shift . 25) (primary-expression shift . 27) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (#{$:\x28;}# shift . 26) (
    unary-expression shift . 60) (cast-expression shift . 234)) ((
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:& shift . 25) (
    #{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 228) (#{$:\x5d;}# shift . 229) ($:static 
    shift . 230) ($:restrict shift . 81) ($:volatile shift . 82) ($:const 
    shift . 83) (type-qualifier shift . 219) (type-qualifier-list shift . 231)
    ($:* shift . 232)) ((#{$:\x5b;}# shift . 140) (#{$:\x28;}# shift . 141) (
    $:* shift . 142) (direct-abstract-declarator shift . 143) (pointer shift 
    . 144) (abstract-declarator shift . 222) (typename shift . 67) ($:enum 
    shift . 68) ($:union shift . 69) ($:struct shift . 70) ($:_Complex shift 
    . 71) ($:double shift . 72) ($:float shift . 73) ($:char shift . 74) (
    $:unsigned shift . 75) ($:long shift . 76) ($:int shift . 77) ($:signed 
    shift . 78) ($:short shift . 79) ($:inline shift . 80) ($:restrict shift 
    . 81) ($:volatile shift . 82) ($:const shift . 83) (typedef-name shift . 
    84) (enum-specifier shift . 85) (struct-or-union-specifier shift . 86) (
    complex-type-specifier shift . 87) ($:_Bool shift . 88) (
    float-type-specifier shift . 89) (fixed-type-specifier shift . 90) ($:void
    shift . 91) ($:typedef shift . 92) ($:static shift . 93) ($:register 
    shift . 94) ($:extern shift . 95) ($:auto shift . 96) (function-specifier 
    shift . 97) (type-qualifier shift . 146) (type-specifier shift . 147) (
    storage-class-specifier shift . 100) (declaration-specifiers shift . 223) 
    (parameter-declaration shift . 224) (parameter-list shift . 225) (
    parameter-type-list shift . 226) (#{$:\x29;}# shift . 227)) (($:restrict 
    shift . 81) ($:volatile shift . 82) ($:const shift . 83) (type-qualifier 
    shift . 219) (type-qualifier-list shift . 220) ($:* shift . 142) (pointer 
    shift . 221) (#{$:\x5b;}# reduce . 190) (#{$:\x28;}# reduce . 190) (
    #{$:\x29;}# reduce . 190) (cpp-ident reduce . 190) ($ident reduce . 190) (
    $:, reduce . 190)) ((#{$:\x5b;}# shift . 217) (#{$:\x28;}# shift . 218) (
    #{$:\x29;}# reduce . 208) ($:, reduce . 208)) ((#{$:\x5b;}# shift . 140) (
    #{$:\x28;}# shift . 141) (direct-abstract-declarator shift . 216) (
    #{$:\x29;}# reduce . 206)) ((#{$:\x29;}# reduce . 204)) ((typename shift 
    . 67) ($:enum shift . 68) ($:union shift . 69) ($:struct shift . 70) (
    $:_Complex shift . 71) ($:double shift . 72) ($:float shift . 73) ($:char 
    shift . 74) ($:unsigned shift . 75) ($:long shift . 76) ($:int shift . 77)
    ($:signed shift . 78) ($:short shift . 79) ($:inline shift . 80) (
    $:restrict shift . 81) ($:volatile shift . 82) ($:const shift . 83) (
    typedef-name shift . 84) (enum-specifier shift . 85) (
    struct-or-union-specifier shift . 86) (complex-type-specifier shift . 87) 
    ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) ($:typedef shift . 92
    ) ($:static shift . 93) ($:register shift . 94) ($:extern shift . 95) (
    $:auto shift . 96) (function-specifier shift . 97) (type-qualifier shift 
    . 146) (type-specifier shift . 147) (storage-class-specifier shift . 100) 
    (declaration-specifiers shift . 151) ($:, reduce . 87) (#{$:\x29;}# reduce
    . 87) ($:* reduce . 87) (#{$:\x5b;}# reduce . 87) (#{$:\x28;}# reduce . 
    87) (cpp-ident reduce . 87) ($ident reduce . 87)) ((typename shift . 67) (
    $:enum shift . 68) ($:union shift . 69) ($:struct shift . 70) ($:_Complex 
    shift . 71) ($:double shift . 72) ($:float shift . 73) ($:char shift . 74)
    ($:unsigned shift . 75) ($:long shift . 76) ($:int shift . 77) ($:signed 
    shift . 78) ($:short shift . 79) ($:inline shift . 80) ($:restrict shift 
    . 81) ($:volatile shift . 82) ($:const shift . 83) (typedef-name shift . 
    84) (enum-specifier shift . 85) (struct-or-union-specifier shift . 86) (
    complex-type-specifier shift . 87) ($:_Bool shift . 88) (
    float-type-specifier shift . 89) (fixed-type-specifier shift . 90) ($:void
    shift . 91) ($:typedef shift . 92) ($:static shift . 93) ($:register 
    shift . 94) ($:extern shift . 95) ($:auto shift . 96) (function-specifier 
    shift . 97) (type-qualifier shift . 146) (type-specifier shift . 147) (
    storage-class-specifier shift . 100) (declaration-specifiers shift . 149) 
    ($:, reduce . 85) (#{$:\x29;}# reduce . 85) ($:* reduce . 85) (#{$:\x5b;}#
    reduce . 85) (#{$:\x28;}# reduce . 85) (cpp-ident reduce . 85) ($ident 
    reduce . 85)) ((#{$:\x29;}# reduce . 84) ($:, reduce . 84) ($:* reduce . 
    84) (#{$:\x5b;}# reduce . 84) (#{$:\x28;}# reduce . 84) (cpp-ident reduce 
    . 84) ($ident reduce . 84)) ((#{$:\x29;}# reduce . 86) ($:, reduce . 86) (
    $:* reduce . 86) (#{$:\x5b;}# reduce . 86) (#{$:\x28;}# reduce . 86) (
    cpp-ident reduce . 86) ($ident reduce . 86)) (($:* reduce . 152) (
    #{$:\x5b;}# reduce . 152) (#{$:\x28;}# reduce . 152) (cpp-ident reduce . 
    152) ($ident reduce . 152) ($:: reduce . 152)) ((#{$:\x29;}# reduce . 88) 
    ($:, reduce . 88) ($:* reduce . 88) (#{$:\x5b;}# reduce . 88) (#{$:\x28;}#
    reduce . 88) (cpp-ident reduce . 88) ($ident reduce . 88)) (($:* reduce 
    . 154) (#{$:\x5b;}# reduce . 154) (#{$:\x28;}# reduce . 154) (cpp-ident 
    reduce . 154) ($ident reduce . 154) ($:: reduce . 154)) ((#{$:\x29;}# 
    reduce . 90) ($:, reduce . 90) ($:* reduce . 90) (#{$:\x5b;}# reduce . 90)
    (#{$:\x28;}# reduce . 90) (cpp-ident reduce . 90) ($ident reduce . 90)) (
    (#{$:\x29;}# reduce . 109) ($:inline reduce . 109) ($:auto reduce . 109) (
    $:extern reduce . 109) ($:register reduce . 109) ($:static reduce . 109) (
    $:typedef reduce . 109) ($:const reduce . 109) ($:volatile reduce . 109) (
    $:restrict reduce . 109) ($:void reduce . 109) ($:_Bool reduce . 109) (
    typename reduce . 109) ($:enum reduce . 109) ($:struct reduce . 109) (
    $:union reduce . 109) ($:_Complex reduce . 109) ($:float reduce . 109) (
    $:double reduce . 109) ($:long reduce . 109) ($:short reduce . 109) (
    $:signed reduce . 109) ($:int reduce . 109) ($:unsigned reduce . 109) (
    $:char reduce . 109) ($:* reduce . 109) (#{$:\x5b;}# reduce . 109) (
    #{$:\x28;}# reduce . 109) ($:, reduce . 109) (cpp-ident reduce . 109) (
    $ident reduce . 109) ($:: reduce . 109)) (($:int shift . 215) (#{$:\x29;}#
    reduce . 110) ($:inline reduce . 110) ($:auto reduce . 110) ($:extern 
    reduce . 110) ($:register reduce . 110) ($:static reduce . 110) ($:typedef
    reduce . 110) ($:const reduce . 110) ($:volatile reduce . 110) (
    $:restrict reduce . 110) ($:void reduce . 110) ($:_Bool reduce . 110) (
    typename reduce . 110) ($:enum reduce . 110) ($:struct reduce . 110) (
    $:union reduce . 110) ($:_Complex reduce . 110) ($:float reduce . 110) (
    $:double reduce . 110) ($:long reduce . 110) ($:short reduce . 110) (
    $:signed reduce . 110) ($:unsigned reduce . 110) ($:char reduce . 110) (
    $:* reduce . 110) (#{$:\x5b;}# reduce . 110) (#{$:\x28;}# reduce . 110) (
    $:, reduce . 110) (cpp-ident reduce . 110) ($ident reduce . 110) ($:: 
    reduce . 110)) ((#{$:\x29;}# reduce . 114) ($:inline reduce . 114) ($:auto
    reduce . 114) ($:extern reduce . 114) ($:register reduce . 114) ($:static
    reduce . 114) ($:typedef reduce . 114) ($:const reduce . 114) ($:volatile
    reduce . 114) ($:restrict reduce . 114) ($:void reduce . 114) ($:_Bool 
    reduce . 114) (typename reduce . 114) ($:enum reduce . 114) ($:struct 
    reduce . 114) ($:union reduce . 114) ($:_Complex reduce . 114) ($:float 
    reduce . 114) ($:double reduce . 114) ($:long reduce . 114) ($:short 
    reduce . 114) ($:signed reduce . 114) ($:int reduce . 114) ($:unsigned 
    reduce . 114) ($:char reduce . 114) ($:* reduce . 114) (#{$:\x5b;}# reduce
    . 114) (#{$:\x28;}# reduce . 114) ($:, reduce . 114) (cpp-ident reduce . 
    114) ($ident reduce . 114) ($:: reduce . 114)) (($:long shift . 213) (
    $:int shift . 214) (#{$:\x29;}# reduce . 117) ($:inline reduce . 117) (
    $:auto reduce . 117) ($:extern reduce . 117) ($:register reduce . 117) (
    $:static reduce . 117) ($:typedef reduce . 117) ($:const reduce . 117) (
    $:volatile reduce . 117) ($:restrict reduce . 117) ($:void reduce . 117) (
    $:_Bool reduce . 117) (typename reduce . 117) ($:enum reduce . 117) (
    $:struct reduce . 117) ($:union reduce . 117) ($:_Complex reduce . 117) (
    $:float reduce . 117) ($:double reduce . 117) ($:short reduce . 117) (
    $:signed reduce . 117) ($:unsigned reduce . 117) ($:char reduce . 117) (
    $:* reduce . 117) (#{$:\x5b;}# reduce . 117) (#{$:\x28;}# reduce . 117) (
    $:, reduce . 117) (cpp-ident reduce . 117) ($ident reduce . 117) ($:: 
    reduce . 117)) ((#{$:\x29;}# reduce . 132) ($:inline reduce . 132) ($:auto
    reduce . 132) ($:extern reduce . 132) ($:register reduce . 132) ($:static
    reduce . 132) ($:typedef reduce . 132) ($:const reduce . 132) ($:volatile
    reduce . 132) ($:restrict reduce . 132) ($:void reduce . 132) ($:_Bool 
    reduce . 132) (typename reduce . 132) ($:enum reduce . 132) ($:struct 
    reduce . 132) ($:union reduce . 132) ($:_Complex reduce . 132) ($:float 
    reduce . 132) ($:double reduce . 132) ($:long reduce . 132) ($:short 
    reduce . 132) ($:signed reduce . 132) ($:int reduce . 132) ($:unsigned 
    reduce . 132) ($:char reduce . 132) ($:* reduce . 132) (#{$:\x5b;}# reduce
    . 132) (#{$:\x28;}# reduce . 132) ($:, reduce . 132) (cpp-ident reduce . 
    132) ($ident reduce . 132) ($:: reduce . 132)) ((#{$:\x29;}# reduce . 116)
    ($:inline reduce . 116) ($:auto reduce . 116) ($:extern reduce . 116) (
    $:register reduce . 116) ($:static reduce . 116) ($:typedef reduce . 116) 
    ($:const reduce . 116) ($:volatile reduce . 116) ($:restrict reduce . 116)
    ($:void reduce . 116) ($:_Bool reduce . 116) (typename reduce . 116) (
    $:enum reduce . 116) ($:struct reduce . 116) ($:union reduce . 116) (
    $:_Complex reduce . 116) ($:float reduce . 116) ($:double reduce . 116) (
    $:long reduce . 116) ($:short reduce . 116) ($:signed reduce . 116) ($:int
    reduce . 116) ($:unsigned reduce . 116) ($:char reduce . 116) ($:* reduce
    . 116) (#{$:\x5b;}# reduce . 116) (#{$:\x28;}# reduce . 116) ($:, reduce 
    . 116) (cpp-ident reduce . 116) ($ident reduce . 116) ($:: reduce . 116)) 
    (($:int shift . 212) (#{$:\x29;}# reduce . 119) ($:inline reduce . 119) (
    $:auto reduce . 119) ($:extern reduce . 119) ($:register reduce . 119) (
    $:static reduce . 119) ($:typedef reduce . 119) ($:const reduce . 119) (
    $:volatile reduce . 119) ($:restrict reduce . 119) ($:void reduce . 119) (
    $:_Bool reduce . 119) (typename reduce . 119) ($:enum reduce . 119) (
    $:struct reduce . 119) ($:union reduce . 119) ($:_Complex reduce . 119) (
    $:float reduce . 119) ($:double reduce . 119) ($:long reduce . 119) (
    $:short reduce . 119) ($:signed reduce . 119) ($:unsigned reduce . 119) (
    $:char reduce . 119) ($:* reduce . 119) (#{$:\x5b;}# reduce . 119) (
    #{$:\x28;}# reduce . 119) ($:, reduce . 119) (cpp-ident reduce . 119) (
    $ident reduce . 119) ($:: reduce . 119)) (($:_Complex shift . 211) (
    #{$:\x29;}# reduce . 136) ($:inline reduce . 136) ($:auto reduce . 136) (
    $:extern reduce . 136) ($:register reduce . 136) ($:static reduce . 136) (
    $:typedef reduce . 136) ($:const reduce . 136) ($:volatile reduce . 136) (
    $:restrict reduce . 136) ($:void reduce . 136) ($:_Bool reduce . 136) (
    typename reduce . 136) ($:enum reduce . 136) ($:struct reduce . 136) (
    $:union reduce . 136) ($:float reduce . 136) ($:double reduce . 136) (
    $:long reduce . 136) ($:short reduce . 136) ($:signed reduce . 136) ($:int
    reduce . 136) ($:unsigned reduce . 136) ($:char reduce . 136) ($:* reduce
    . 136) (#{$:\x5b;}# reduce . 136) (#{$:\x28;}# reduce . 136) ($:, reduce 
    . 136) (cpp-ident reduce . 136) ($ident reduce . 136) ($:: reduce . 136)) 
    (($:int shift . 210) (#{$:\x29;}# reduce . 124) ($:inline reduce . 124) (
    $:auto reduce . 124) ($:extern reduce . 124) ($:register reduce . 124) (
    $:static reduce . 124) ($:typedef reduce . 124) ($:const reduce . 124) (
    $:volatile reduce . 124) ($:restrict reduce . 124) ($:void reduce . 124) (
    $:_Bool reduce . 124) (typename reduce . 124) ($:enum reduce . 124) (
    $:struct reduce . 124) ($:union reduce . 124) ($:_Complex reduce . 124) (
    $:float reduce . 124) ($:double reduce . 124) ($:long reduce . 124) (
    $:short reduce . 124) ($:signed reduce . 124) ($:unsigned reduce . 124) (
    $:char reduce . 124) ($:* reduce . 124) (#{$:\x5b;}# reduce . 124) (
    #{$:\x28;}# reduce . 124) ($:, reduce . 124) (cpp-ident reduce . 124) (
    $ident reduce . 124) ($:: reduce . 124)) ((#{$:\x29;}# reduce . 125) (
    $:inline reduce . 125) ($:auto reduce . 125) ($:extern reduce . 125) (
    $:register reduce . 125) ($:static reduce . 125) ($:typedef reduce . 125) 
    ($:const reduce . 125) ($:volatile reduce . 125) ($:restrict reduce . 125)
    ($:void reduce . 125) ($:_Bool reduce . 125) (typename reduce . 125) (
    $:enum reduce . 125) ($:struct reduce . 125) ($:union reduce . 125) (
    $:_Complex reduce . 125) ($:float reduce . 125) ($:double reduce . 125) (
    $:long reduce . 125) ($:short reduce . 125) ($:signed reduce . 125) ($:int
    reduce . 125) ($:unsigned reduce . 125) ($:char reduce . 125) ($:* reduce
    . 125) (#{$:\x5b;}# reduce . 125) (#{$:\x28;}# reduce . 125) ($:, reduce 
    . 125) (cpp-ident reduce . 125) ($ident reduce . 125) ($:: reduce . 125)) 
    (($:long shift . 208) ($:int shift . 209) (#{$:\x29;}# reduce . 128) (
    $:inline reduce . 128) ($:auto reduce . 128) ($:extern reduce . 128) (
    $:register reduce . 128) ($:static reduce . 128) ($:typedef reduce . 128) 
    ($:const reduce . 128) ($:volatile reduce . 128) ($:restrict reduce . 128)
    ($:void reduce . 128) ($:_Bool reduce . 128) (typename reduce . 128) (
    $:enum reduce . 128) ($:struct reduce . 128) ($:union reduce . 128) (
    $:_Complex reduce . 128) ($:float reduce . 128) ($:double reduce . 128) (
    $:short reduce . 128) ($:signed reduce . 128) ($:unsigned reduce . 128) (
    $:char reduce . 128) ($:* reduce . 128) (#{$:\x5b;}# reduce . 128) (
    #{$:\x28;}# reduce . 128) ($:, reduce . 128) (cpp-ident reduce . 128) (
    $ident reduce . 128) ($:: reduce . 128)) ((#{$:\x29;}# reduce . 133) (
    $:inline reduce . 133) ($:auto reduce . 133) ($:extern reduce . 133) (
    $:register reduce . 133) ($:static reduce . 133) ($:typedef reduce . 133) 
    ($:const reduce . 133) ($:volatile reduce . 133) ($:restrict reduce . 133)
    ($:void reduce . 133) ($:_Bool reduce . 133) (typename reduce . 133) (
    $:enum reduce . 133) ($:struct reduce . 133) ($:union reduce . 133) (
    $:_Complex reduce . 133) ($:float reduce . 133) ($:double reduce . 133) (
    $:long reduce . 133) ($:short reduce . 133) ($:signed reduce . 133) ($:int
    reduce . 133) ($:unsigned reduce . 133) ($:char reduce . 133) ($:* reduce
    . 133) (#{$:\x5b;}# reduce . 133) (#{$:\x28;}# reduce . 133) ($:, reduce 
    . 133) (cpp-ident reduce . 133) ($ident reduce . 133) ($:: reduce . 133)) 
    ((#{$:\x29;}# reduce . 138) ($:inline reduce . 138) ($:auto reduce . 138) 
    ($:extern reduce . 138) ($:register reduce . 138) ($:static reduce . 138) 
    ($:typedef reduce . 138) ($:const reduce . 138) ($:volatile reduce . 138) 
    ($:restrict reduce . 138) ($:void reduce . 138) ($:_Bool reduce . 138) (
    typename reduce . 138) ($:enum reduce . 138) ($:struct reduce . 138) (
    $:union reduce . 138) ($:_Complex reduce . 138) ($:float reduce . 138) (
    $:double reduce . 138) ($:long reduce . 138) ($:short reduce . 138) (
    $:signed reduce . 138) ($:int reduce . 138) ($:unsigned reduce . 138) (
    $:char reduce . 138) ($:* reduce . 138) (#{$:\x5b;}# reduce . 138) (
    #{$:\x28;}# reduce . 138) ($:, reduce . 138) (cpp-ident reduce . 138) (
    $ident reduce . 138) ($:: reduce . 138)) ((#{$:\x29;}# reduce . 139) (
    $:inline reduce . 139) ($:auto reduce . 139) ($:extern reduce . 139) (
    $:register reduce . 139) ($:static reduce . 139) ($:typedef reduce . 139) 
    ($:const reduce . 139) ($:volatile reduce . 139) ($:restrict reduce . 139)
    ($:void reduce . 139) ($:_Bool reduce . 139) (typename reduce . 139) (
    $:enum reduce . 139) ($:struct reduce . 139) ($:union reduce . 139) (
    $:_Complex reduce . 139) ($:float reduce . 139) ($:double reduce . 139) (
    $:long reduce . 139) ($:short reduce . 139) ($:signed reduce . 139) ($:int
    reduce . 139) ($:unsigned reduce . 139) ($:char reduce . 139) ($:* reduce
    . 139) (#{$:\x5b;}# reduce . 139) (#{$:\x28;}# reduce . 139) ($:, reduce 
    . 139) (cpp-ident reduce . 139) ($ident reduce . 139) ($:: reduce . 139)) 
    ((typename shift . 67) ($:enum shift . 68) ($:union shift . 69) ($:struct 
    shift . 70) ($:_Complex shift . 71) ($:double shift . 72) ($:float shift 
    . 73) ($:char shift . 74) ($:unsigned shift . 75) ($:long shift . 76) (
    $:int shift . 77) ($:signed shift . 78) ($:short shift . 79) ($:restrict 
    shift . 81) ($:volatile shift . 82) ($:const shift . 83) (typedef-name 
    shift . 84) (enum-specifier shift . 85) (struct-or-union-specifier shift 
    . 86) (complex-type-specifier shift . 87) ($:_Bool shift . 88) (
    float-type-specifier shift . 89) (fixed-type-specifier shift . 90) ($:void
    shift . 91) (type-qualifier shift . 199) (type-specifier shift . 200) (
    $lone-comm shift . 201) (specifier-qualifier-list shift . 202) (
    lone-comment shift . 203) (struct-declaration shift . 204) (
    struct-declaration-list shift . 207)) ((#{$:\x7b;}# shift . 206) (
    #{$:\x29;}# reduce . 143) ($:inline reduce . 143) ($:auto reduce . 143) (
    $:extern reduce . 143) ($:register reduce . 143) ($:static reduce . 143) (
    $:typedef reduce . 143) ($:const reduce . 143) ($:volatile reduce . 143) (
    $:restrict reduce . 143) ($:void reduce . 143) ($:_Bool reduce . 143) (
    typename reduce . 143) ($:enum reduce . 143) ($:struct reduce . 143) (
    $:union reduce . 143) ($:_Complex reduce . 143) ($:float reduce . 143) (
    $:double reduce . 143) ($:long reduce . 143) ($:short reduce . 143) (
    $:signed reduce . 143) ($:int reduce . 143) ($:unsigned reduce . 143) (
    $:char reduce . 143) ($:* reduce . 143) (#{$:\x5b;}# reduce . 143) (
    #{$:\x28;}# reduce . 143) ($:, reduce . 143) (cpp-ident reduce . 143) (
    $ident reduce . 143) ($:: reduce . 143)) ((typename shift . 67) ($:enum 
    shift . 68) ($:union shift . 69) ($:struct shift . 70) ($:_Complex shift 
    . 71) ($:double shift . 72) ($:float shift . 73) ($:char shift . 74) (
    $:unsigned shift . 75) ($:long shift . 76) ($:int shift . 77) ($:signed 
    shift . 78) ($:short shift . 79) ($:restrict shift . 81) ($:volatile shift
    . 82) ($:const shift . 83) (typedef-name shift . 84) (enum-specifier 
    shift . 85) (struct-or-union-specifier shift . 86) (complex-type-specifier
    shift . 87) ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) (type-qualifier shift
    . 199) (type-specifier shift . 200) ($lone-comm shift . 201) (
    specifier-qualifier-list shift . 202) (lone-comment shift . 203) (
    struct-declaration shift . 204) (struct-declaration-list shift . 205)) ((
    #{$:\x7b;}# shift . 198) (#{$:\x29;}# reduce . 146) ($:inline reduce . 146
    ) ($:auto reduce . 146) ($:extern reduce . 146) ($:register reduce . 146) 
    ($:static reduce . 146) ($:typedef reduce . 146) ($:const reduce . 146) (
    $:volatile reduce . 146) ($:restrict reduce . 146) ($:void reduce . 146) (
    $:_Bool reduce . 146) (typename reduce . 146) ($:enum reduce . 146) (
    $:struct reduce . 146) ($:union reduce . 146) ($:_Complex reduce . 146) (
    $:float reduce . 146) ($:double reduce . 146) ($:long reduce . 146) (
    $:short reduce . 146) ($:signed reduce . 146) ($:int reduce . 146) (
    $:unsigned reduce . 146) ($:char reduce . 146) ($:* reduce . 146) (
    #{$:\x5b;}# reduce . 146) (#{$:\x28;}# reduce . 146) ($:, reduce . 146) (
    cpp-ident reduce . 146) ($ident reduce . 146) ($:: reduce . 146)) ((
    cpp-ident shift . 12) ($ident shift . 13) (identifier shift . 195) (
    enumerator shift . 196) (enumerator-list shift . 197)) ((#{$:\x7b;}# shift
    . 194) (#{$:\x29;}# reduce . 165) ($:inline reduce . 165) ($:auto reduce 
    . 165) ($:extern reduce . 165) ($:register reduce . 165) ($:static reduce 
    . 165) ($:typedef reduce . 165) ($:const reduce . 165) ($:volatile reduce 
    . 165) ($:restrict reduce . 165) ($:void reduce . 165) ($:_Bool reduce . 
    165) (typename reduce . 165) ($:enum reduce . 165) ($:struct reduce . 165)
    ($:union reduce . 165) ($:_Complex reduce . 165) ($:float reduce . 165) (
    $:double reduce . 165) ($:long reduce . 165) ($:short reduce . 165) (
    $:signed reduce . 165) ($:int reduce . 165) ($:unsigned reduce . 165) (
    $:char reduce . 165) ($:* reduce . 165) (#{$:\x5b;}# reduce . 165) (
    #{$:\x28;}# reduce . 165) ($:, reduce . 165) (cpp-ident reduce . 165) (
    $ident reduce . 165) ($:: reduce . 165)) (($:|= reduce . 5) ($:^= reduce 
    . 5) ($:&= reduce . 5) ($:>>= reduce . 5) ($:<<= reduce . 5) ($:%= reduce 
    . 5) ($:/= reduce . 5) ($:*= reduce . 5) ($:-= reduce . 5) ($:+= reduce . 
    5) ($:= reduce . 5) (#{$:\x5b;}# reduce . 5) (#{$:\x28;}# reduce . 5) ($:.
    reduce . 5) ($:-> reduce . 5) ($:++ reduce . 5) ($:-- reduce . 5) ($:? 
    reduce . 5) ($:, reduce . 5) ($:|| reduce . 5) ($:&& reduce . 5) ($:| 
    reduce . 5) ($:^ reduce . 5) ($:& reduce . 5) ($:!= reduce . 5) ($:== 
    reduce . 5) ($:< reduce . 5) ($:> reduce . 5) ($:<= reduce . 5) ($:>= 
    reduce . 5) ($:>> reduce . 5) ($:<< reduce . 5) ($:+ reduce . 5) ($:- 
    reduce . 5) ($:% reduce . 5) ($:/ reduce . 5) ($:* reduce . 5) (
    #{$:\x29;}# reduce . 5) (#{$:\x5d;}# reduce . 5) ($:: reduce . 5) (
    #{$:\x7d;}# reduce . 5) ($end reduce . 5) (#{$:;}# reduce . 5)) (($:&& 
    shift . 104) ($:? reduce . 61) ($:|| reduce . 61) ($:, reduce . 61) (
    #{$:\x29;}# reduce . 61) (#{$:\x5d;}# reduce . 61) ($:: reduce . 61) (
    #{$:\x7d;}# reduce . 61) ($end reduce . 61) (#{$:;}# reduce . 61)) (($:: 
    shift . 193) ($:, shift . 38)) ((#{$:\x29;}# shift . 192)) ((#{$:\x29;}# 
    shift . 191)) (($:= reduce . 11) ($:+= reduce . 11) ($:-= reduce . 11) (
    $:*= reduce . 11) ($:/= reduce . 11) ($:%= reduce . 11) ($:<<= reduce . 11
    ) ($:>>= reduce . 11) ($:&= reduce . 11) ($:^= reduce . 11) ($:|= reduce 
    . 11) ($:-- reduce . 11) ($:++ reduce . 11) ($:-> reduce . 11) ($:. reduce
    . 11) (#{$:\x28;}# reduce . 11) (#{$:\x5b;}# reduce . 11) ($:? reduce . 
    11) ($:* reduce . 11) ($:/ reduce . 11) ($:% reduce . 11) ($:- reduce . 11
    ) ($:+ reduce . 11) ($:<< reduce . 11) ($:>> reduce . 11) ($:>= reduce . 
    11) ($:<= reduce . 11) ($:> reduce . 11) ($:< reduce . 11) ($:== reduce . 
    11) ($:!= reduce . 11) ($:& reduce . 11) ($:^ reduce . 11) ($:| reduce . 
    11) ($:&& reduce . 11) ($:|| reduce . 11) ($:, reduce . 11) (#{$:\x29;}# 
    reduce . 11) (#{$:\x5d;}# reduce . 11) ($:: reduce . 11) (#{$:\x7d;}# 
    reduce . 11) ($end reduce . 11) (#{$:;}# reduce . 11)) (($:= reduce . 10) 
    ($:+= reduce . 10) ($:-= reduce . 10) ($:*= reduce . 10) ($:/= reduce . 10
    ) ($:%= reduce . 10) ($:<<= reduce . 10) ($:>>= reduce . 10) ($:&= reduce 
    . 10) ($:^= reduce . 10) ($:|= reduce . 10) ($:-- reduce . 10) ($:++ 
    reduce . 10) ($:-> reduce . 10) ($:. reduce . 10) (#{$:\x28;}# reduce . 10
    ) (#{$:\x5b;}# reduce . 10) ($:? reduce . 10) ($:* reduce . 10) ($:/ 
    reduce . 10) ($:% reduce . 10) ($:- reduce . 10) ($:+ reduce . 10) ($:<< 
    reduce . 10) ($:>> reduce . 10) ($:>= reduce . 10) ($:<= reduce . 10) ($:>
    reduce . 10) ($:< reduce . 10) ($:== reduce . 10) ($:!= reduce . 10) ($:&
    reduce . 10) ($:^ reduce . 10) ($:| reduce . 10) ($:&& reduce . 10) ($:||
    reduce . 10) ($:, reduce . 10) (#{$:\x29;}# reduce . 10) (#{$:\x5d;}# 
    reduce . 10) ($:: reduce . 10) (#{$:\x7d;}# reduce . 10) ($end reduce . 10
    ) (#{$:;}# reduce . 10)) (($:= reduce . 9) ($:+= reduce . 9) ($:-= reduce 
    . 9) ($:*= reduce . 9) ($:/= reduce . 9) ($:%= reduce . 9) ($:<<= reduce 
    . 9) ($:>>= reduce . 9) ($:&= reduce . 9) ($:^= reduce . 9) ($:|= reduce 
    . 9) ($:-- reduce . 9) ($:++ reduce . 9) ($:-> reduce . 9) ($:. reduce . 9
    ) (#{$:\x28;}# reduce . 9) (#{$:\x5b;}# reduce . 9) ($:? reduce . 9) ($:* 
    reduce . 9) ($:/ reduce . 9) ($:% reduce . 9) ($:- reduce . 9) ($:+ reduce
    . 9) ($:<< reduce . 9) ($:>> reduce . 9) ($:>= reduce . 9) ($:<= reduce 
    . 9) ($:> reduce . 9) ($:< reduce . 9) ($:== reduce . 9) ($:!= reduce . 9)
    ($:& reduce . 9) ($:^ reduce . 9) ($:| reduce . 9) ($:&& reduce . 9) (
    $:|| reduce . 9) ($:, reduce . 9) (#{$:\x29;}# reduce . 9) (#{$:\x5d;}# 
    reduce . 9) ($:: reduce . 9) (#{$:\x7d;}# reduce . 9) ($end reduce . 9) (
    #{$:;}# reduce . 9)) ((#{$:\x29;}# reduce . 18) ($:, reduce . 18)) ((
    #{$:\x29;}# reduce . 16) ($:, reduce . 16)) ((#{$:\x29;}# shift . 189) (
    $:, shift . 190)) ((#{$:\x5d;}# shift . 188) ($:, shift . 38)) (($:, 
    reduce . 65) (#{$:\x29;}# reduce . 65) (#{$:\x5d;}# reduce . 65) ($:: 
    reduce . 65) (#{$:\x7d;}# reduce . 65) ($end reduce . 65)) (($:, reduce . 
    78) ($end reduce . 78) (#{$:\x29;}# reduce . 78) ($:: reduce . 78) (
    #{$:\x5d;}# reduce . 78)) (($:= reduce . 7) ($:+= reduce . 7) ($:-= reduce
    . 7) ($:*= reduce . 7) ($:/= reduce . 7) ($:%= reduce . 7) ($:<<= reduce 
    . 7) ($:>>= reduce . 7) ($:&= reduce . 7) ($:^= reduce . 7) ($:|= reduce 
    . 7) ($:-- reduce . 7) ($:++ reduce . 7) ($:-> reduce . 7) ($:. reduce . 7
    ) (#{$:\x28;}# reduce . 7) (#{$:\x5b;}# reduce . 7) ($:? reduce . 7) ($:* 
    reduce . 7) ($:/ reduce . 7) ($:% reduce . 7) ($:- reduce . 7) ($:+ reduce
    . 7) ($:<< reduce . 7) ($:>> reduce . 7) ($:>= reduce . 7) ($:<= reduce 
    . 7) ($:> reduce . 7) ($:< reduce . 7) ($:== reduce . 7) ($:!= reduce . 7)
    ($:& reduce . 7) ($:^ reduce . 7) ($:| reduce . 7) ($:&& reduce . 7) (
    $:|| reduce . 7) ($:, reduce . 7) (#{$:\x29;}# reduce . 7) (#{$:\x5d;}# 
    reduce . 7) ($:: reduce . 7) (#{$:\x7d;}# reduce . 7) ($end reduce . 7) (
    #{$:;}# reduce . 7)) (($:= reduce . 8) ($:+= reduce . 8) ($:-= reduce . 8)
    ($:*= reduce . 8) ($:/= reduce . 8) ($:%= reduce . 8) ($:<<= reduce . 8) 
    ($:>>= reduce . 8) ($:&= reduce . 8) ($:^= reduce . 8) ($:|= reduce . 8) (
    $:-- reduce . 8) ($:++ reduce . 8) ($:-> reduce . 8) ($:. reduce . 8) (
    #{$:\x28;}# reduce . 8) (#{$:\x5b;}# reduce . 8) ($:? reduce . 8) ($:* 
    reduce . 8) ($:/ reduce . 8) ($:% reduce . 8) ($:- reduce . 8) ($:+ reduce
    . 8) ($:<< reduce . 8) ($:>> reduce . 8) ($:>= reduce . 8) ($:<= reduce 
    . 8) ($:> reduce . 8) ($:< reduce . 8) ($:== reduce . 8) ($:!= reduce . 8)
    ($:& reduce . 8) ($:^ reduce . 8) ($:| reduce . 8) ($:&& reduce . 8) (
    $:|| reduce . 8) ($:, reduce . 8) (#{$:\x29;}# reduce . 8) (#{$:\x5d;}# 
    reduce . 8) ($:: reduce . 8) (#{$:\x7d;}# reduce . 8) ($end reduce . 8) (
    #{$:;}# reduce . 8)) ((typename shift . 67) (typedef-name shift . 287) (
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 288)) ((#{$:\x7b;}# shift . 233)) ((
    #{$:\x7b;}# shift . 233) ($:|= reduce . 25) ($:^= reduce . 25) ($:&= 
    reduce . 25) ($:>>= reduce . 25) ($:<<= reduce . 25) ($:%= reduce . 25) (
    $:/= reduce . 25) ($:*= reduce . 25) ($:-= reduce . 25) ($:+= reduce . 25)
    ($:= reduce . 25) ($:? reduce . 25) ($:, reduce . 25) ($:|| reduce . 25) 
    ($:&& reduce . 25) ($:| reduce . 25) ($:^ reduce . 25) ($:& reduce . 25) (
    $:!= reduce . 25) ($:== reduce . 25) ($:< reduce . 25) ($:> reduce . 25) (
    $:<= reduce . 25) ($:>= reduce . 25) ($:>> reduce . 25) ($:<< reduce . 25)
    ($:+ reduce . 25) ($:- reduce . 25) ($:% reduce . 25) ($:/ reduce . 25) (
    $:* reduce . 25) (#{$:\x29;}# reduce . 25) (#{$:\x5d;}# reduce . 25) ($:: 
    reduce . 25) (#{$:\x7d;}# reduce . 25) ($end reduce . 25) (#{$:;}# reduce 
    . 25)) (($string shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed
    shift . 11) (cpp-ident shift . 12) ($ident shift . 13) (string-literal 
    shift . 16) (constant shift . 17) (identifier shift . 18) ($:! shift . 20)
    ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& 
    shift . 25) (primary-expression shift . 27) ($:sizeof shift . 29) (
    unary-operator shift . 30) ($:-- shift . 31) ($:++ shift . 32) (
    postfix-expression shift . 33) (#{$:\x28;}# shift . 26) (unary-expression 
    shift . 60) (cast-expression shift . 1) (multiplicative-expression shift 
    . 2) (additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) (bitwise-xor-expression shift . 14) (
    bitwise-or-expression shift . 15) (logical-and-expression shift . 19) (
    logical-or-expression shift . 28) (conditional-expression shift . 286)) ((
    cpp-ident shift . 12) ($ident shift . 13) (identifier shift . 195) (
    enumerator shift . 196) (enumerator-list shift . 285)) (($:= shift . 284) 
    (#{$:\x7d;}# reduce . 168) ($:, reduce . 168)) ((#{$:\x7d;}# reduce . 166)
    ($:, reduce . 166)) ((#{$:\x7d;}# shift . 282) ($:, shift . 283)) ((
    typename shift . 67) ($:enum shift . 68) ($:union shift . 69) ($:struct 
    shift . 70) ($:_Complex shift . 71) ($:double shift . 72) ($:float shift 
    . 73) ($:char shift . 74) ($:unsigned shift . 75) ($:long shift . 76) (
    $:int shift . 77) ($:signed shift . 78) ($:short shift . 79) ($:restrict 
    shift . 81) ($:volatile shift . 82) ($:const shift . 83) (typedef-name 
    shift . 84) (enum-specifier shift . 85) (struct-or-union-specifier shift 
    . 86) (complex-type-specifier shift . 87) ($:_Bool shift . 88) (
    float-type-specifier shift . 89) (fixed-type-specifier shift . 90) ($:void
    shift . 91) (type-qualifier shift . 199) (type-specifier shift . 200) (
    $lone-comm shift . 201) (specifier-qualifier-list shift . 202) (
    lone-comment shift . 203) (struct-declaration shift . 204) (
    struct-declaration-list shift . 281)) ((typename shift . 67) ($:enum shift
    . 68) ($:union shift . 69) ($:struct shift . 70) ($:_Complex shift . 71) 
    ($:double shift . 72) ($:float shift . 73) ($:char shift . 74) ($:unsigned
    shift . 75) ($:long shift . 76) ($:int shift . 77) ($:signed shift . 78) 
    ($:short shift . 79) ($:restrict shift . 81) ($:volatile shift . 82) (
    $:const shift . 83) (typedef-name shift . 84) (enum-specifier shift . 85) 
    (struct-or-union-specifier shift . 86) (complex-type-specifier shift . 87)
    ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) (type-qualifier shift
    . 199) (type-specifier shift . 200) (specifier-qualifier-list shift . 152
    ) ($:* reduce . 155) (cpp-ident reduce . 155) ($ident reduce . 155) (
    #{$:\x28;}# reduce . 155) ($:: reduce . 155)) ((typename shift . 67) (
    $:enum shift . 68) ($:union shift . 69) ($:struct shift . 70) ($:_Complex 
    shift . 71) ($:double shift . 72) ($:float shift . 73) ($:char shift . 74)
    ($:unsigned shift . 75) ($:long shift . 76) ($:int shift . 77) ($:signed 
    shift . 78) ($:short shift . 79) ($:restrict shift . 81) ($:volatile shift
    . 82) ($:const shift . 83) (typedef-name shift . 84) (enum-specifier 
    shift . 85) (struct-or-union-specifier shift . 86) (complex-type-specifier
    shift . 87) ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) (type-qualifier shift
    . 199) (type-specifier shift . 200) (specifier-qualifier-list shift . 150
    ) ($:* reduce . 153) (cpp-ident reduce . 153) ($ident reduce . 153) (
    #{$:\x28;}# reduce . 153) ($:: reduce . 153)) ((#{$:\x7d;}# reduce . 299) 
    ($:const reduce . 299) ($:volatile reduce . 299) ($:restrict reduce . 299)
    ($:void reduce . 299) ($:_Bool reduce . 299) (typename reduce . 299) (
    $:enum reduce . 299) ($:struct reduce . 299) ($:union reduce . 299) (
    $:_Complex reduce . 299) ($:float reduce . 299) ($:double reduce . 299) (
    $:long reduce . 299) ($:short reduce . 299) ($:signed reduce . 299) ($:int
    reduce . 299) ($:unsigned reduce . 299) ($:char reduce . 299) ($lone-comm
    reduce . 299)) ((cpp-ident shift . 12) ($ident shift . 13) ($:* shift . 
    142) (#{$:\x28;}# shift . 275) (identifier shift . 253) (direct-declarator
    shift . 254) (pointer shift . 276) ($:: shift . 277) (declarator shift . 
    278) (struct-declarator shift . 279) (struct-declarator-list shift . 280))
    ((#{$:\x7d;}# reduce . 148) ($lone-comm reduce . 148) ($:char reduce . 
    148) ($:unsigned reduce . 148) ($:int reduce . 148) ($:signed reduce . 148
    ) ($:short reduce . 148) ($:long reduce . 148) ($:double reduce . 148) (
    $:float reduce . 148) ($:_Complex reduce . 148) ($:union reduce . 148) (
    $:struct reduce . 148) ($:enum reduce . 148) (typename reduce . 148) (
    $:_Bool reduce . 148) ($:void reduce . 148) ($:restrict reduce . 148) (
    $:volatile reduce . 148) ($:const reduce . 148)) ((#{$:\x7d;}# reduce . 
    147) ($lone-comm reduce . 147) ($:char reduce . 147) ($:unsigned reduce . 
    147) ($:int reduce . 147) ($:signed reduce . 147) ($:short reduce . 147) (
    $:long reduce . 147) ($:double reduce . 147) ($:float reduce . 147) (
    $:_Complex reduce . 147) ($:union reduce . 147) ($:struct reduce . 147) (
    $:enum reduce . 147) (typename reduce . 147) ($:_Bool reduce . 147) (
    $:void reduce . 147) ($:restrict reduce . 147) ($:volatile reduce . 147) (
    $:const reduce . 147)) ((#{$:\x7d;}# shift . 274) (typename shift . 67) (
    $:enum shift . 68) ($:union shift . 69) ($:struct shift . 70) ($:_Complex 
    shift . 71) ($:double shift . 72) ($:float shift . 73) ($:char shift . 74)
    ($:unsigned shift . 75) ($:long shift . 76) ($:int shift . 77) ($:signed 
    shift . 78) ($:short shift . 79) ($:restrict shift . 81) ($:volatile shift
    . 82) ($:const shift . 83) (typedef-name shift . 84) (enum-specifier 
    shift . 85) (struct-or-union-specifier shift . 86) (complex-type-specifier
    shift . 87) ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) (type-qualifier shift
    . 199) (type-specifier shift . 200) (specifier-qualifier-list shift . 202
    ) (struct-declaration shift . 271) ($lone-comm shift . 201) (lone-comment 
    shift . 272)) ((typename shift . 67) ($:enum shift . 68) ($:union shift . 
    69) ($:struct shift . 70) ($:_Complex shift . 71) ($:double shift . 72) (
    $:float shift . 73) ($:char shift . 74) ($:unsigned shift . 75) ($:long 
    shift . 76) ($:int shift . 77) ($:signed shift . 78) ($:short shift . 79) 
    ($:restrict shift . 81) ($:volatile shift . 82) ($:const shift . 83) (
    typedef-name shift . 84) (enum-specifier shift . 85) (
    struct-or-union-specifier shift . 86) (complex-type-specifier shift . 87) 
    ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) (type-qualifier shift
    . 199) (type-specifier shift . 200) ($lone-comm shift . 201) (
    specifier-qualifier-list shift . 202) (lone-comment shift . 203) (
    struct-declaration shift . 204) (struct-declaration-list shift . 273)) ((
    #{$:\x7d;}# shift . 270) (typename shift . 67) ($:enum shift . 68) (
    $:union shift . 69) ($:struct shift . 70) ($:_Complex shift . 71) (
    $:double shift . 72) ($:float shift . 73) ($:char shift . 74) ($:unsigned 
    shift . 75) ($:long shift . 76) ($:int shift . 77) ($:signed shift . 78) (
    $:short shift . 79) ($:restrict shift . 81) ($:volatile shift . 82) (
    $:const shift . 83) (typedef-name shift . 84) (enum-specifier shift . 85) 
    (struct-or-union-specifier shift . 86) (complex-type-specifier shift . 87)
    ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) (type-qualifier shift
    . 199) (type-specifier shift . 200) (specifier-qualifier-list shift . 202
    ) (struct-declaration shift . 271) ($lone-comm shift . 201) (lone-comment 
    shift . 272)) (($:int shift . 269) (#{$:\x29;}# reduce . 130) ($:inline 
    reduce . 130) ($:auto reduce . 130) ($:extern reduce . 130) ($:register 
    reduce . 130) ($:static reduce . 130) ($:typedef reduce . 130) ($:const 
    reduce . 130) ($:volatile reduce . 130) ($:restrict reduce . 130) ($:void 
    reduce . 130) ($:_Bool reduce . 130) (typename reduce . 130) ($:enum 
    reduce . 130) ($:struct reduce . 130) ($:union reduce . 130) ($:_Complex 
    reduce . 130) ($:float reduce . 130) ($:double reduce . 130) ($:long 
    reduce . 130) ($:short reduce . 130) ($:signed reduce . 130) ($:unsigned 
    reduce . 130) ($:char reduce . 130) ($:* reduce . 130) (#{$:\x5b;}# reduce
    . 130) (#{$:\x28;}# reduce . 130) ($:, reduce . 130) (cpp-ident reduce . 
    130) ($ident reduce . 130) ($:: reduce . 130)) ((#{$:\x29;}# reduce . 127)
    ($:inline reduce . 127) ($:auto reduce . 127) ($:extern reduce . 127) (
    $:register reduce . 127) ($:static reduce . 127) ($:typedef reduce . 127) 
    ($:const reduce . 127) ($:volatile reduce . 127) ($:restrict reduce . 127)
    ($:void reduce . 127) ($:_Bool reduce . 127) (typename reduce . 127) (
    $:enum reduce . 127) ($:struct reduce . 127) ($:union reduce . 127) (
    $:_Complex reduce . 127) ($:float reduce . 127) ($:double reduce . 127) (
    $:long reduce . 127) ($:short reduce . 127) ($:signed reduce . 127) ($:int
    reduce . 127) ($:unsigned reduce . 127) ($:char reduce . 127) ($:* reduce
    . 127) (#{$:\x5b;}# reduce . 127) (#{$:\x28;}# reduce . 127) ($:, reduce 
    . 127) (cpp-ident reduce . 127) ($ident reduce . 127) ($:: reduce . 127)) 
    ((#{$:\x29;}# reduce . 123) ($:inline reduce . 123) ($:auto reduce . 123) 
    ($:extern reduce . 123) ($:register reduce . 123) ($:static reduce . 123) 
    ($:typedef reduce . 123) ($:const reduce . 123) ($:volatile reduce . 123) 
    ($:restrict reduce . 123) ($:void reduce . 123) ($:_Bool reduce . 123) (
    typename reduce . 123) ($:enum reduce . 123) ($:struct reduce . 123) (
    $:union reduce . 123) ($:_Complex reduce . 123) ($:float reduce . 123) (
    $:double reduce . 123) ($:long reduce . 123) ($:short reduce . 123) (
    $:signed reduce . 123) ($:int reduce . 123) ($:unsigned reduce . 123) (
    $:char reduce . 123) ($:* reduce . 123) (#{$:\x5b;}# reduce . 123) (
    #{$:\x28;}# reduce . 123) ($:, reduce . 123) (cpp-ident reduce . 123) (
    $ident reduce . 123) ($:: reduce . 123)) ((#{$:\x29;}# reduce . 140) (
    $:inline reduce . 140) ($:auto reduce . 140) ($:extern reduce . 140) (
    $:register reduce . 140) ($:static reduce . 140) ($:typedef reduce . 140) 
    ($:const reduce . 140) ($:volatile reduce . 140) ($:restrict reduce . 140)
    ($:void reduce . 140) ($:_Bool reduce . 140) (typename reduce . 140) (
    $:enum reduce . 140) ($:struct reduce . 140) ($:union reduce . 140) (
    $:_Complex reduce . 140) ($:float reduce . 140) ($:double reduce . 140) (
    $:long reduce . 140) ($:short reduce . 140) ($:signed reduce . 140) ($:int
    reduce . 140) ($:unsigned reduce . 140) ($:char reduce . 140) ($:* reduce
    . 140) (#{$:\x5b;}# reduce . 140) (#{$:\x28;}# reduce . 140) ($:, reduce 
    . 140) (cpp-ident reduce . 140) ($ident reduce . 140) ($:: reduce . 140)) 
    ((#{$:\x29;}# reduce . 120) ($:inline reduce . 120) ($:auto reduce . 120) 
    ($:extern reduce . 120) ($:register reduce . 120) ($:static reduce . 120) 
    ($:typedef reduce . 120) ($:const reduce . 120) ($:volatile reduce . 120) 
    ($:restrict reduce . 120) ($:void reduce . 120) ($:_Bool reduce . 120) (
    typename reduce . 120) ($:enum reduce . 120) ($:struct reduce . 120) (
    $:union reduce . 120) ($:_Complex reduce . 120) ($:float reduce . 120) (
    $:double reduce . 120) ($:long reduce . 120) ($:short reduce . 120) (
    $:signed reduce . 120) ($:int reduce . 120) ($:unsigned reduce . 120) (
    $:char reduce . 120) ($:* reduce . 120) (#{$:\x5b;}# reduce . 120) (
    #{$:\x28;}# reduce . 120) ($:, reduce . 120) (cpp-ident reduce . 120) (
    $ident reduce . 120) ($:: reduce . 120)) (($:int shift . 268) (#{$:\x29;}#
    reduce . 121) ($:inline reduce . 121) ($:auto reduce . 121) ($:extern 
    reduce . 121) ($:register reduce . 121) ($:static reduce . 121) ($:typedef
    reduce . 121) ($:const reduce . 121) ($:volatile reduce . 121) (
    $:restrict reduce . 121) ($:void reduce . 121) ($:_Bool reduce . 121) (
    typename reduce . 121) ($:enum reduce . 121) ($:struct reduce . 121) (
    $:union reduce . 121) ($:_Complex reduce . 121) ($:float reduce . 121) (
    $:double reduce . 121) ($:long reduce . 121) ($:short reduce . 121) (
    $:signed reduce . 121) ($:unsigned reduce . 121) ($:char reduce . 121) (
    $:* reduce . 121) (#{$:\x5b;}# reduce . 121) (#{$:\x28;}# reduce . 121) (
    $:, reduce . 121) (cpp-ident reduce . 121) ($ident reduce . 121) ($:: 
    reduce . 121)) ((#{$:\x29;}# reduce . 118) ($:inline reduce . 118) ($:auto
    reduce . 118) ($:extern reduce . 118) ($:register reduce . 118) ($:static
    reduce . 118) ($:typedef reduce . 118) ($:const reduce . 118) ($:volatile
    reduce . 118) ($:restrict reduce . 118) ($:void reduce . 118) ($:_Bool 
    reduce . 118) (typename reduce . 118) ($:enum reduce . 118) ($:struct 
    reduce . 118) ($:union reduce . 118) ($:_Complex reduce . 118) ($:float 
    reduce . 118) ($:double reduce . 118) ($:long reduce . 118) ($:short 
    reduce . 118) ($:signed reduce . 118) ($:int reduce . 118) ($:unsigned 
    reduce . 118) ($:char reduce . 118) ($:* reduce . 118) (#{$:\x5b;}# reduce
    . 118) (#{$:\x28;}# reduce . 118) ($:, reduce . 118) (cpp-ident reduce . 
    118) ($ident reduce . 118) ($:: reduce . 118)) ((#{$:\x29;}# reduce . 111)
    ($:inline reduce . 111) ($:auto reduce . 111) ($:extern reduce . 111) (
    $:register reduce . 111) ($:static reduce . 111) ($:typedef reduce . 111) 
    ($:const reduce . 111) ($:volatile reduce . 111) ($:restrict reduce . 111)
    ($:void reduce . 111) ($:_Bool reduce . 111) (typename reduce . 111) (
    $:enum reduce . 111) ($:struct reduce . 111) ($:union reduce . 111) (
    $:_Complex reduce . 111) ($:float reduce . 111) ($:double reduce . 111) (
    $:long reduce . 111) ($:short reduce . 111) ($:signed reduce . 111) ($:int
    reduce . 111) ($:unsigned reduce . 111) ($:char reduce . 111) ($:* reduce
    . 111) (#{$:\x5b;}# reduce . 111) (#{$:\x28;}# reduce . 111) ($:, reduce 
    . 111) (cpp-ident reduce . 111) ($ident reduce . 111) ($:: reduce . 111)) 
    ((#{$:\x5b;}# shift . 217) (#{$:\x28;}# shift . 218) (#{$:\x29;}# reduce 
    . 207) ($:, reduce . 207)) (($:static shift . 263) (#{$:\x5d;}# shift . 
    264) (cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 265) ($:& shift
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 266) ($:restrict shift . 81) ($:volatile 
    shift . 82) ($:const shift . 83) (type-qualifier shift . 219) (
    type-qualifier-list shift . 267)) ((#{$:\x29;}# shift . 261) (typename 
    shift . 67) ($:enum shift . 68) ($:union shift . 69) ($:struct shift . 70)
    ($:_Complex shift . 71) ($:double shift . 72) ($:float shift . 73) (
    $:char shift . 74) ($:unsigned shift . 75) ($:long shift . 76) ($:int 
    shift . 77) ($:signed shift . 78) ($:short shift . 79) ($:inline shift . 
    80) ($:restrict shift . 81) ($:volatile shift . 82) ($:const shift . 83) (
    typedef-name shift . 84) (enum-specifier shift . 85) (
    struct-or-union-specifier shift . 86) (complex-type-specifier shift . 87) 
    ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) ($:typedef shift . 92
    ) ($:static shift . 93) ($:register shift . 94) ($:extern shift . 95) (
    $:auto shift . 96) (function-specifier shift . 97) (type-qualifier shift 
    . 146) (type-specifier shift . 147) (storage-class-specifier shift . 100) 
    (declaration-specifiers shift . 223) (parameter-declaration shift . 224) (
    parameter-list shift . 225) (parameter-type-list shift . 262)) (($:static 
    reduce . 193) ($:restrict reduce . 193) ($:volatile reduce . 193) ($:const
    reduce . 193) (#{$:\x5d;}# reduce . 193) (cpp-ident reduce . 193) ($ident
    reduce . 193) ($chlit reduce . 193) ($float reduce . 193) ($fixed reduce 
    . 193) ($string reduce . 193) (#{$:\x28;}# reduce . 193) ($:! reduce . 193
    ) ($:~ reduce . 193) ($:- reduce . 193) ($:+ reduce . 193) ($:* reduce . 
    193) ($:& reduce . 193) ($:sizeof reduce . 193) ($:-- reduce . 193) ($:++ 
    reduce . 193) (#{$:\x5b;}# reduce . 193) (#{$:\x29;}# reduce . 193) ($:, 
    reduce . 193)) (($:* shift . 142) (pointer shift . 260) ($:restrict shift 
    . 81) ($:volatile shift . 82) ($:const shift . 83) (type-qualifier shift 
    . 246) (#{$:\x5b;}# reduce . 189) (#{$:\x28;}# reduce . 189) (#{$:\x29;}# 
    reduce . 189) (cpp-ident reduce . 189) ($ident reduce . 189) ($:, reduce 
    . 189)) ((#{$:\x5b;}# reduce . 192) (#{$:\x28;}# reduce . 192) (
    #{$:\x29;}# reduce . 192) (cpp-ident reduce . 192) ($ident reduce . 192) (
    $:, reduce . 192)) ((#{$:\x29;}# shift . 259)) ((cpp-ident shift . 12) (
    $ident shift . 13) (identifier shift . 253) (direct-declarator shift . 254
    ) (declarator shift . 255) (#{$:\x5b;}# shift . 140) (#{$:\x28;}# shift . 
    256) ($:* shift . 142) (direct-abstract-declarator shift . 143) (pointer 
    shift . 257) (abstract-declarator shift . 258) (#{$:\x29;}# reduce . 201) 
    ($:, reduce . 201)) (($:, reduce . 197) (#{$:\x29;}# reduce . 197)) (($:, 
    shift . 252) (#{$:\x29;}# reduce . 195)) ((#{$:\x29;}# shift . 251)) ((
    #{$:\x28;}# reduce . 229) (#{$:\x5b;}# reduce . 229) (#{$:\x29;}# reduce 
    . 229) ($:, reduce . 229)) ((#{$:\x5d;}# shift . 250)) ((#{$:\x28;}# 
    reduce . 220) (#{$:\x5b;}# reduce . 220) (#{$:\x29;}# reduce . 220) ($:, 
    reduce . 220)) (($:restrict shift . 81) ($:volatile shift . 82) ($:const 
    shift . 83) (type-qualifier shift . 219) (type-qualifier-list shift . 249)
    ) (($:static shift . 245) ($:restrict shift . 81) ($:volatile shift . 82) 
    ($:const shift . 83) (type-qualifier shift . 246) (#{$:\x5d;}# shift . 247
    ) (cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 248)) ((#{$:\x5d;}# shift . 244) (cpp-ident 
    reduce . 27) ($ident reduce . 27) ($chlit reduce . 27) ($float reduce . 27
    ) ($fixed reduce . 27) ($string reduce . 27) ($:! reduce . 27) ($:~ reduce
    . 27) ($:- reduce . 27) ($:+ reduce . 27) ($:* reduce . 27) ($:& reduce 
    . 27) ($:sizeof reduce . 27) ($:-- reduce . 27) ($:++ reduce . 27) (
    #{$:\x28;}# reduce . 27)) ((cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) ($string
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (bitwise-xor-expression shift . 
    14) (bitwise-or-expression shift . 15) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) (logical-and-expression shift
    . 19) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23)
    ($:* shift . 24) ($:& shift . 25) (#{$:\x28;}# shift . 26) (
    primary-expression shift . 27) ($:. shift . 235) (#{$:\x5b;}# shift . 236)
    (logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (designator shift . 237) (unary-expression shift . 34) (
    conditional-expression shift . 35) (designator-list shift . 238) (
    #{$:\x7b;}# shift . 239) (assignment-expression shift . 240) (initializer 
    shift . 241) (designation shift . 242) (initializer-list shift . 243)) ((
    $:? reduce . 33) ($:* reduce . 33) ($:/ reduce . 33) ($:% reduce . 33) (
    $:- reduce . 33) ($:+ reduce . 33) ($:<< reduce . 33) ($:>> reduce . 33) (
    $:>= reduce . 33) ($:<= reduce . 33) ($:> reduce . 33) ($:< reduce . 33) (
    $:== reduce . 33) ($:!= reduce . 33) ($:& reduce . 33) ($:^ reduce . 33) (
    $:| reduce . 33) ($:&& reduce . 33) ($:|| reduce . 33) ($:, reduce . 33) (
    #{$:\x29;}# reduce . 33) (#{$:\x5d;}# reduce . 33) ($:: reduce . 33) (
    #{$:\x7d;}# reduce . 33) ($end reduce . 33) ($:|= reduce . 33) ($:^= 
    reduce . 33) ($:&= reduce . 33) ($:>>= reduce . 33) ($:<<= reduce . 33) (
    $:%= reduce . 33) ($:/= reduce . 33) ($:*= reduce . 33) ($:-= reduce . 33)
    ($:+= reduce . 33) ($:= reduce . 33) (#{$:;}# reduce . 33)) ((cpp-ident 
    shift . 12) ($ident shift . 13) (identifier shift . 325)) (($string shift 
    . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (cpp-ident
    shift . 12) ($ident shift . 13) (string-literal shift . 16) (constant 
    shift . 17) (identifier shift . 18) ($:! shift . 20) ($:~ shift . 21) ($:-
    shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift . 25) (
    primary-expression shift . 27) ($:sizeof shift . 29) (unary-operator shift
    . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift . 33)
    (#{$:\x28;}# shift . 26) (unary-expression shift . 60) (cast-expression 
    shift . 1) (multiplicative-expression shift . 2) (additive-expression 
    shift . 3) (shift-expression shift . 4) (relational-expression shift . 5) 
    (equality-expression shift . 6) (bitwise-and-expression shift . 7) (
    bitwise-xor-expression shift . 14) (bitwise-or-expression shift . 15) (
    logical-and-expression shift . 19) (logical-or-expression shift . 28) (
    conditional-expression shift . 291) (constant-expression shift . 324)) ((
    $:= reduce . 239) ($:. reduce . 239) (#{$:\x5b;}# reduce . 239)) (($:= 
    shift . 322) ($:. shift . 235) (#{$:\x5b;}# shift . 236) (designator shift
    . 323)) ((cast-expression shift . 1) (multiplicative-expression shift . 2
    ) (additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) ($:. shift 
    . 235) (#{$:\x5b;}# shift . 236) (logical-or-expression shift . 28) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (designator shift . 237) (
    unary-expression shift . 34) (conditional-expression shift . 35) (
    designator-list shift . 238) (#{$:\x7b;}# shift . 239) (
    assignment-expression shift . 240) (initializer shift . 241) (designation 
    shift . 242) (initializer-list shift . 321)) ((#{$:\x7d;}# reduce . 231) (
    $:, reduce . 231)) ((#{$:\x7d;}# reduce . 235) ($:, reduce . 235)) ((
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    #{$:\x7b;}# shift . 239) (assignment-expression shift . 240) (initializer 
    shift . 320)) ((#{$:\x7d;}# shift . 318) ($:, shift . 319)) ((#{$:\x28;}# 
    reduce . 225) (#{$:\x5b;}# reduce . 225) (#{$:\x29;}# reduce . 225) ($:, 
    reduce . 225)) ((cast-expression shift . 1) (multiplicative-expression 
    shift . 2) (additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 317)) (($:* reduce . 194) ($:restrict reduce
    . 194) ($:volatile reduce . 194) ($:const reduce . 194) (#{$:\x5b;}# 
    reduce . 194) (#{$:\x28;}# reduce . 194) (#{$:\x29;}# reduce . 194) (
    cpp-ident reduce . 194) ($ident reduce . 194) ($:static reduce . 194) (
    #{$:\x5d;}# reduce . 194) ($chlit reduce . 194) ($float reduce . 194) (
    $fixed reduce . 194) ($string reduce . 194) ($:! reduce . 194) ($:~ reduce
    . 194) ($:- reduce . 194) ($:+ reduce . 194) ($:& reduce . 194) ($:sizeof
    reduce . 194) ($:-- reduce . 194) ($:++ reduce . 194) ($:, reduce . 194))
    ((#{$:\x28;}# reduce . 218) (#{$:\x5b;}# reduce . 218) (#{$:\x29;}# 
    reduce . 218) ($:, reduce . 218)) ((#{$:\x5d;}# shift . 316)) ((
    cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 314) ($:restrict shift . 81) ($:volatile 
    shift . 82) ($:const shift . 83) (type-qualifier shift . 246) (#{$:\x5d;}#
    shift . 315)) ((#{$:\x28;}# reduce . 219) (#{$:\x5b;}# reduce . 219) (
    #{$:\x29;}# reduce . 219) ($:, reduce . 219)) ((#{$:\x28;}# reduce . 228) 
    (#{$:\x5b;}# reduce . 228) (#{$:\x29;}# reduce . 228) ($:, reduce . 228)) 
    ((typename shift . 67) ($:enum shift . 68) ($:union shift . 69) ($:struct 
    shift . 70) ($:_Complex shift . 71) ($:double shift . 72) ($:float shift 
    . 73) ($:char shift . 74) ($:unsigned shift . 75) ($:long shift . 76) (
    $:int shift . 77) ($:signed shift . 78) ($:short shift . 79) ($:inline 
    shift . 80) ($:restrict shift . 81) ($:volatile shift . 82) ($:const shift
    . 83) (typedef-name shift . 84) (enum-specifier shift . 85) (
    struct-or-union-specifier shift . 86) (complex-type-specifier shift . 87) 
    ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) ($:typedef shift . 92
    ) ($:static shift . 93) ($:register shift . 94) ($:extern shift . 95) (
    $:auto shift . 96) (function-specifier shift . 97) (type-qualifier shift 
    . 146) (type-specifier shift . 147) (storage-class-specifier shift . 100) 
    (declaration-specifiers shift . 223) (parameter-declaration shift . 312) (
    $:... shift . 313)) ((#{$:;}# reduce . 176) ($:: reduce . 176) (
    #{$:\x28;}# reduce . 176) (#{$:\x5b;}# reduce . 176) ($:, reduce . 176) (
    #{$:\x29;}# reduce . 176)) ((#{$:\x5b;}# shift . 310) (#{$:\x28;}# shift 
    . 311) ($:: reduce . 175) (#{$:;}# reduce . 175) ($:, reduce . 175) (
    #{$:\x29;}# reduce . 175)) ((#{$:\x29;}# reduce . 199) ($:, reduce . 199))
    ((#{$:\x5b;}# shift . 140) (direct-abstract-declarator shift . 143) (
    abstract-declarator shift . 222) (typename shift . 67) ($:enum shift . 68)
    ($:union shift . 69) ($:struct shift . 70) ($:_Complex shift . 71) (
    $:double shift . 72) ($:float shift . 73) ($:char shift . 74) ($:unsigned 
    shift . 75) ($:long shift . 76) ($:int shift . 77) ($:signed shift . 78) (
    $:short shift . 79) ($:inline shift . 80) ($:restrict shift . 81) (
    $:volatile shift . 82) ($:const shift . 83) (typedef-name shift . 84) (
    enum-specifier shift . 85) (struct-or-union-specifier shift . 86) (
    complex-type-specifier shift . 87) ($:_Bool shift . 88) (
    float-type-specifier shift . 89) (fixed-type-specifier shift . 90) ($:void
    shift . 91) ($:typedef shift . 92) ($:static shift . 93) ($:register 
    shift . 94) ($:extern shift . 95) ($:auto shift . 96) (function-specifier 
    shift . 97) (type-qualifier shift . 146) (type-specifier shift . 147) (
    storage-class-specifier shift . 100) (declaration-specifiers shift . 223) 
    (parameter-declaration shift . 224) (parameter-list shift . 225) (
    parameter-type-list shift . 226) (#{$:\x29;}# shift . 227) (cpp-ident 
    shift . 12) ($ident shift . 13) ($:* shift . 142) (#{$:\x28;}# shift . 256
    ) (identifier shift . 253) (direct-declarator shift . 254) (pointer shift 
    . 257) (declarator shift . 301)) ((#{$:\x5b;}# shift . 140) (
    direct-abstract-declarator shift . 216) (cpp-ident shift . 12) ($ident 
    shift . 13) (#{$:\x28;}# shift . 256) (identifier shift . 253) (
    direct-declarator shift . 300) (#{$:\x29;}# reduce . 206) ($:, reduce . 
    206)) ((#{$:\x29;}# reduce . 200) ($:, reduce . 200)) ((#{$:\x28;}# reduce
    . 209) (#{$:\x5b;}# reduce . 209) (#{$:\x29;}# reduce . 209) ($:, reduce 
    . 209)) ((#{$:\x5b;}# reduce . 191) (#{$:\x28;}# reduce . 191) (
    #{$:\x29;}# reduce . 191) (cpp-ident reduce . 191) ($ident reduce . 191) (
    $:, reduce . 191)) ((#{$:\x28;}# reduce . 227) (#{$:\x5b;}# reduce . 227) 
    (#{$:\x29;}# reduce . 227) ($:, reduce . 227)) ((#{$:\x29;}# shift . 309))
    (($:restrict shift . 81) ($:volatile shift . 82) ($:const shift . 83) (
    type-qualifier shift . 219) (type-qualifier-list shift . 308)) ((
    #{$:\x28;}# reduce . 213) (#{$:\x5b;}# reduce . 213) (#{$:\x29;}# reduce 
    . 213) ($:, reduce . 213)) ((#{$:\x5d;}# shift . 307) (cpp-ident reduce . 
    27) ($ident reduce . 27) ($chlit reduce . 27) ($float reduce . 27) ($fixed
    reduce . 27) ($string reduce . 27) ($:! reduce . 27) ($:~ reduce . 27) (
    $:- reduce . 27) ($:+ reduce . 27) ($:* reduce . 27) ($:& reduce . 27) (
    $:sizeof reduce . 27) ($:-- reduce . 27) ($:++ reduce . 27) (#{$:\x28;}# 
    reduce . 27)) ((#{$:\x5d;}# shift . 306)) ((cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) ($string
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (bitwise-xor-expression shift . 
    14) (bitwise-or-expression shift . 15) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) (logical-and-expression shift
    . 19) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23)
    ($:* shift . 24) ($:& shift . 25) (#{$:\x28;}# shift . 26) (
    primary-expression shift . 27) (logical-or-expression shift . 28) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (unary-expression shift . 34) 
    (conditional-expression shift . 35) (assignment-expression shift . 303) (
    $:restrict shift . 81) ($:volatile shift . 82) ($:const shift . 83) (
    type-qualifier shift . 246) (#{$:\x5d;}# shift . 304) ($:static shift . 
    305)) ((#{$:\x29;}# reduce . 122) ($:inline reduce . 122) ($:auto reduce 
    . 122) ($:extern reduce . 122) ($:register reduce . 122) ($:static reduce 
    . 122) ($:typedef reduce . 122) ($:const reduce . 122) ($:volatile reduce 
    . 122) ($:restrict reduce . 122) ($:void reduce . 122) ($:_Bool reduce . 
    122) (typename reduce . 122) ($:enum reduce . 122) ($:struct reduce . 122)
    ($:union reduce . 122) ($:_Complex reduce . 122) ($:float reduce . 122) (
    $:double reduce . 122) ($:long reduce . 122) ($:short reduce . 122) (
    $:signed reduce . 122) ($:int reduce . 122) ($:unsigned reduce . 122) (
    $:char reduce . 122) ($:* reduce . 122) (#{$:\x5b;}# reduce . 122) (
    #{$:\x28;}# reduce . 122) ($:, reduce . 122) (cpp-ident reduce . 122) (
    $ident reduce . 122) ($:: reduce . 122)) ((#{$:\x29;}# reduce . 129) (
    $:inline reduce . 129) ($:auto reduce . 129) ($:extern reduce . 129) (
    $:register reduce . 129) ($:static reduce . 129) ($:typedef reduce . 129) 
    ($:const reduce . 129) ($:volatile reduce . 129) ($:restrict reduce . 129)
    ($:void reduce . 129) ($:_Bool reduce . 129) (typename reduce . 129) (
    $:enum reduce . 129) ($:struct reduce . 129) ($:union reduce . 129) (
    $:_Complex reduce . 129) ($:float reduce . 129) ($:double reduce . 129) (
    $:long reduce . 129) ($:short reduce . 129) ($:signed reduce . 129) ($:int
    reduce . 129) ($:unsigned reduce . 129) ($:char reduce . 129) ($:* reduce
    . 129) (#{$:\x5b;}# reduce . 129) (#{$:\x28;}# reduce . 129) ($:, reduce 
    . 129) (cpp-ident reduce . 129) ($ident reduce . 129) ($:: reduce . 129)) 
    ((#{$:\x29;}# reduce . 142) ($:inline reduce . 142) ($:auto reduce . 142) 
    ($:extern reduce . 142) ($:register reduce . 142) ($:static reduce . 142) 
    ($:typedef reduce . 142) ($:const reduce . 142) ($:volatile reduce . 142) 
    ($:restrict reduce . 142) ($:void reduce . 142) ($:_Bool reduce . 142) (
    typename reduce . 142) ($:enum reduce . 142) ($:struct reduce . 142) (
    $:union reduce . 142) ($:_Complex reduce . 142) ($:float reduce . 142) (
    $:double reduce . 142) ($:long reduce . 142) ($:short reduce . 142) (
    $:signed reduce . 142) ($:int reduce . 142) ($:unsigned reduce . 142) (
    $:char reduce . 142) ($:* reduce . 142) (#{$:\x5b;}# reduce . 142) (
    #{$:\x28;}# reduce . 142) ($:, reduce . 142) (cpp-ident reduce . 142) (
    $ident reduce . 142) ($:: reduce . 142)) ((#{$:\x7d;}# reduce . 149) (
    $lone-comm reduce . 149) ($:char reduce . 149) ($:unsigned reduce . 149) (
    $:int reduce . 149) ($:signed reduce . 149) ($:short reduce . 149) ($:long
    reduce . 149) ($:double reduce . 149) ($:float reduce . 149) ($:_Complex 
    reduce . 149) ($:union reduce . 149) ($:struct reduce . 149) ($:enum 
    reduce . 149) (typename reduce . 149) ($:_Bool reduce . 149) ($:void 
    reduce . 149) ($:restrict reduce . 149) ($:volatile reduce . 149) ($:const
    reduce . 149)) ((#{$:\x7d;}# reduce . 150) ($lone-comm reduce . 150) (
    $:char reduce . 150) ($:unsigned reduce . 150) ($:int reduce . 150) (
    $:signed reduce . 150) ($:short reduce . 150) ($:long reduce . 150) (
    $:double reduce . 150) ($:float reduce . 150) ($:_Complex reduce . 150) (
    $:union reduce . 150) ($:struct reduce . 150) ($:enum reduce . 150) (
    typename reduce . 150) ($:_Bool reduce . 150) ($:void reduce . 150) (
    $:restrict reduce . 150) ($:volatile reduce . 150) ($:const reduce . 150))
    ((#{$:\x7d;}# shift . 302) (typename shift . 67) ($:enum shift . 68) (
    $:union shift . 69) ($:struct shift . 70) ($:_Complex shift . 71) (
    $:double shift . 72) ($:float shift . 73) ($:char shift . 74) ($:unsigned 
    shift . 75) ($:long shift . 76) ($:int shift . 77) ($:signed shift . 78) (
    $:short shift . 79) ($:restrict shift . 81) ($:volatile shift . 82) (
    $:const shift . 83) (typedef-name shift . 84) (enum-specifier shift . 85) 
    (struct-or-union-specifier shift . 86) (complex-type-specifier shift . 87)
    ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) (type-qualifier shift
    . 199) (type-specifier shift . 200) (specifier-qualifier-list shift . 202
    ) (struct-declaration shift . 271) ($lone-comm shift . 201) (lone-comment 
    shift . 272)) ((#{$:\x29;}# reduce . 145) ($:inline reduce . 145) ($:auto 
    reduce . 145) ($:extern reduce . 145) ($:register reduce . 145) ($:static 
    reduce . 145) ($:typedef reduce . 145) ($:const reduce . 145) ($:volatile 
    reduce . 145) ($:restrict reduce . 145) ($:void reduce . 145) ($:_Bool 
    reduce . 145) (typename reduce . 145) ($:enum reduce . 145) ($:struct 
    reduce . 145) ($:union reduce . 145) ($:_Complex reduce . 145) ($:float 
    reduce . 145) ($:double reduce . 145) ($:long reduce . 145) ($:short 
    reduce . 145) ($:signed reduce . 145) ($:int reduce . 145) ($:unsigned 
    reduce . 145) ($:char reduce . 145) ($:* reduce . 145) (#{$:\x5b;}# reduce
    . 145) (#{$:\x28;}# reduce . 145) ($:, reduce . 145) (cpp-ident reduce . 
    145) ($ident reduce . 145) ($:: reduce . 145)) ((cpp-ident shift . 12) (
    $ident shift . 13) ($:* shift . 142) (#{$:\x28;}# shift . 275) (identifier
    shift . 253) (direct-declarator shift . 254) (pointer shift . 276) (
    declarator shift . 301)) ((cpp-ident shift . 12) ($ident shift . 13) (
    #{$:\x28;}# shift . 275) (identifier shift . 253) (direct-declarator shift
    . 300)) (($string shift . 8) ($chlit shift . 9) ($float shift . 10) (
    $fixed shift . 11) (cpp-ident shift . 12) ($ident shift . 13) (
    string-literal shift . 16) (constant shift . 17) (identifier shift . 18) (
    $:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* 
    shift . 24) ($:& shift . 25) (primary-expression shift . 27) ($:sizeof 
    shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ shift . 32
    ) (postfix-expression shift . 33) (#{$:\x28;}# shift . 26) (
    unary-expression shift . 60) (cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) (
    bitwise-xor-expression shift . 14) (bitwise-or-expression shift . 15) (
    logical-and-expression shift . 19) (logical-or-expression shift . 28) (
    conditional-expression shift . 291) (constant-expression shift . 299)) ((
    $:: shift . 298) (#{$:;}# reduce . 158) ($:, reduce . 158)) ((#{$:;}# 
    reduce . 156) ($:, reduce . 156)) ((#{$:;}# shift . 296) ($:, shift . 297)
    ) ((#{$:\x7d;}# shift . 295) (typename shift . 67) ($:enum shift . 68) (
    $:union shift . 69) ($:struct shift . 70) ($:_Complex shift . 71) (
    $:double shift . 72) ($:float shift . 73) ($:char shift . 74) ($:unsigned 
    shift . 75) ($:long shift . 76) ($:int shift . 77) ($:signed shift . 78) (
    $:short shift . 79) ($:restrict shift . 81) ($:volatile shift . 82) (
    $:const shift . 83) (typedef-name shift . 84) (enum-specifier shift . 85) 
    (struct-or-union-specifier shift . 86) (complex-type-specifier shift . 87)
    ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) (type-qualifier shift
    . 199) (type-specifier shift . 200) (specifier-qualifier-list shift . 202
    ) (struct-declaration shift . 271) ($lone-comm shift . 201) (lone-comment 
    shift . 272)) ((#{$:\x29;}# reduce . 163) ($:inline reduce . 163) ($:auto 
    reduce . 163) ($:extern reduce . 163) ($:register reduce . 163) ($:static 
    reduce . 163) ($:typedef reduce . 163) ($:const reduce . 163) ($:volatile 
    reduce . 163) ($:restrict reduce . 163) ($:void reduce . 163) ($:_Bool 
    reduce . 163) (typename reduce . 163) ($:enum reduce . 163) ($:struct 
    reduce . 163) ($:union reduce . 163) ($:_Complex reduce . 163) ($:float 
    reduce . 163) ($:double reduce . 163) ($:long reduce . 163) ($:short 
    reduce . 163) ($:signed reduce . 163) ($:int reduce . 163) ($:unsigned 
    reduce . 163) ($:char reduce . 163) ($:* reduce . 163) (#{$:\x5b;}# reduce
    . 163) (#{$:\x28;}# reduce . 163) ($:, reduce . 163) (cpp-ident reduce . 
    163) ($ident reduce . 163) ($:: reduce . 163)) ((#{$:\x7d;}# shift . 293) 
    (cpp-ident shift . 12) ($ident shift . 13) (identifier shift . 195) (
    enumerator shift . 294)) (($string shift . 8) ($chlit shift . 9) ($float 
    shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident shift . 13)
    (string-literal shift . 16) (constant shift . 17) (identifier shift . 18)
    ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* 
    shift . 24) ($:& shift . 25) (primary-expression shift . 27) ($:sizeof 
    shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ shift . 32
    ) (postfix-expression shift . 33) (#{$:\x28;}# shift . 26) (
    unary-expression shift . 60) (cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) (
    bitwise-xor-expression shift . 14) (bitwise-or-expression shift . 15) (
    logical-and-expression shift . 19) (logical-or-expression shift . 28) (
    conditional-expression shift . 291) (constant-expression shift . 292)) ((
    $:, shift . 289) (#{$:\x7d;}# shift . 290)) (($:, reduce . 63) (
    #{$:\x29;}# reduce . 63) (#{$:\x5d;}# reduce . 63) ($:: reduce . 63) (
    #{$:\x7d;}# reduce . 63) ($end reduce . 63) (#{$:;}# reduce . 63)) ((
    #{$:\x29;}# reduce . 19) ($:, reduce . 19)) ((#{$:\x29;}# reduce . 17) (
    $:, reduce . 17)) ((cpp-ident shift . 12) ($ident shift . 13) (identifier 
    shift . 195) (enumerator shift . 294) (#{$:\x7d;}# shift . 353)) ((
    #{$:\x29;}# reduce . 161) ($:inline reduce . 161) ($:auto reduce . 161) (
    $:extern reduce . 161) ($:register reduce . 161) ($:static reduce . 161) (
    $:typedef reduce . 161) ($:const reduce . 161) ($:volatile reduce . 161) (
    $:restrict reduce . 161) ($:void reduce . 161) ($:_Bool reduce . 161) (
    typename reduce . 161) ($:enum reduce . 161) ($:struct reduce . 161) (
    $:union reduce . 161) ($:_Complex reduce . 161) ($:float reduce . 161) (
    $:double reduce . 161) ($:long reduce . 161) ($:short reduce . 161) (
    $:signed reduce . 161) ($:int reduce . 161) ($:unsigned reduce . 161) (
    $:char reduce . 161) ($:* reduce . 161) (#{$:\x5b;}# reduce . 161) (
    #{$:\x28;}# reduce . 161) ($:, reduce . 161) (cpp-ident reduce . 161) (
    $ident reduce . 161) ($:: reduce . 161)) ((#{$:\x5d;}# reduce . 79) (
    #{$:;}# reduce . 79) ($:, reduce . 79) (#{$:\x7d;}# reduce . 79)) ((
    #{$:\x7d;}# reduce . 169) ($:, reduce . 169)) ((#{$:\x29;}# reduce . 164) 
    ($:inline reduce . 164) ($:auto reduce . 164) ($:extern reduce . 164) (
    $:register reduce . 164) ($:static reduce . 164) ($:typedef reduce . 164) 
    ($:const reduce . 164) ($:volatile reduce . 164) ($:restrict reduce . 164)
    ($:void reduce . 164) ($:_Bool reduce . 164) (typename reduce . 164) (
    $:enum reduce . 164) ($:struct reduce . 164) ($:union reduce . 164) (
    $:_Complex reduce . 164) ($:float reduce . 164) ($:double reduce . 164) (
    $:long reduce . 164) ($:short reduce . 164) ($:signed reduce . 164) ($:int
    reduce . 164) ($:unsigned reduce . 164) ($:char reduce . 164) ($:* reduce
    . 164) (#{$:\x5b;}# reduce . 164) (#{$:\x28;}# reduce . 164) ($:, reduce 
    . 164) (cpp-ident reduce . 164) ($ident reduce . 164) ($:: reduce . 164)) 
    ((#{$:\x7d;}# reduce . 167) ($:, reduce . 167)) ((#{$:\x29;}# reduce . 144
    ) ($:inline reduce . 144) ($:auto reduce . 144) ($:extern reduce . 144) (
    $:register reduce . 144) ($:static reduce . 144) ($:typedef reduce . 144) 
    ($:const reduce . 144) ($:volatile reduce . 144) ($:restrict reduce . 144)
    ($:void reduce . 144) ($:_Bool reduce . 144) (typename reduce . 144) (
    $:enum reduce . 144) ($:struct reduce . 144) ($:union reduce . 144) (
    $:_Complex reduce . 144) ($:float reduce . 144) ($:double reduce . 144) (
    $:long reduce . 144) ($:short reduce . 144) ($:signed reduce . 144) ($:int
    reduce . 144) ($:unsigned reduce . 144) ($:char reduce . 144) ($:* reduce
    . 144) (#{$:\x5b;}# reduce . 144) (#{$:\x28;}# reduce . 144) ($:, reduce 
    . 144) (cpp-ident reduce . 144) ($ident reduce . 144) ($:: reduce . 144)) 
    (($code-comm shift . 350) (code-comment shift . 351) (opt-code-comment 
    shift . 352) ($lone-comm reduce . 289) ($:char reduce . 289) ($:unsigned 
    reduce . 289) ($:int reduce . 289) ($:signed reduce . 289) ($:short reduce
    . 289) ($:long reduce . 289) ($:double reduce . 289) ($:float reduce . 
    289) ($:_Complex reduce . 289) ($:union reduce . 289) ($:struct reduce . 
    289) ($:enum reduce . 289) (typename reduce . 289) ($:_Bool reduce . 289) 
    ($:void reduce . 289) ($:restrict reduce . 289) ($:volatile reduce . 289) 
    ($:const reduce . 289) (#{$:\x7d;}# reduce . 289)) ((cpp-ident shift . 12)
    ($ident shift . 13) ($:* shift . 142) (#{$:\x28;}# shift . 275) (
    identifier shift . 253) (direct-declarator shift . 254) (pointer shift . 
    276) ($:: shift . 277) (declarator shift . 278) (struct-declarator shift 
    . 349)) (($string shift . 8) ($chlit shift . 9) ($float shift . 10) (
    $fixed shift . 11) (cpp-ident shift . 12) ($ident shift . 13) (
    string-literal shift . 16) (constant shift . 17) (identifier shift . 18) (
    $:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* 
    shift . 24) ($:& shift . 25) (primary-expression shift . 27) ($:sizeof 
    shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ shift . 32
    ) (postfix-expression shift . 33) (#{$:\x28;}# shift . 26) (
    unary-expression shift . 60) (cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) (
    bitwise-xor-expression shift . 14) (bitwise-or-expression shift . 15) (
    logical-and-expression shift . 19) (logical-or-expression shift . 28) (
    conditional-expression shift . 291) (constant-expression shift . 348)) ((
    #{$:;}# reduce . 160) ($:, reduce . 160)) ((#{$:\x5b;}# shift . 310) (
    #{$:\x28;}# shift . 311) (#{$:\x29;}# reduce . 174) ($:, reduce . 174) (
    $:: reduce . 174) (#{$:;}# reduce . 174)) ((#{$:\x29;}# shift . 347)) ((
    #{$:\x29;}# reduce . 141) ($:inline reduce . 141) ($:auto reduce . 141) (
    $:extern reduce . 141) ($:register reduce . 141) ($:static reduce . 141) (
    $:typedef reduce . 141) ($:const reduce . 141) ($:volatile reduce . 141) (
    $:restrict reduce . 141) ($:void reduce . 141) ($:_Bool reduce . 141) (
    typename reduce . 141) ($:enum reduce . 141) ($:struct reduce . 141) (
    $:union reduce . 141) ($:_Complex reduce . 141) ($:float reduce . 141) (
    $:double reduce . 141) ($:long reduce . 141) ($:short reduce . 141) (
    $:signed reduce . 141) ($:int reduce . 141) ($:unsigned reduce . 141) (
    $:char reduce . 141) ($:* reduce . 141) (#{$:\x5b;}# reduce . 141) (
    #{$:\x28;}# reduce . 141) ($:, reduce . 141) (cpp-ident reduce . 141) (
    $ident reduce . 141) ($:: reduce . 141)) ((#{$:\x5d;}# shift . 346)) ((
    #{$:\x28;}# reduce . 211) (#{$:\x5b;}# reduce . 211) (#{$:\x29;}# reduce 
    . 211) ($:, reduce . 211)) ((cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) ($string
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (bitwise-xor-expression shift . 
    14) (bitwise-or-expression shift . 15) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) (logical-and-expression shift
    . 19) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23)
    ($:* shift . 24) ($:& shift . 25) (#{$:\x28;}# shift . 26) (
    primary-expression shift . 27) (logical-or-expression shift . 28) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (unary-expression shift . 34) 
    (conditional-expression shift . 35) (assignment-expression shift . 345)) (
    (#{$:\x28;}# reduce . 212) (#{$:\x5b;}# reduce . 212) (#{$:\x29;}# reduce 
    . 212) ($:, reduce . 212)) ((#{$:\x28;}# reduce . 224) (#{$:\x5b;}# reduce
    . 224) (#{$:\x29;}# reduce . 224) ($:, reduce . 224)) ((#{$:\x5d;}# shift
    . 343) ($:restrict shift . 81) ($:volatile shift . 82) ($:const shift . 
    83) (type-qualifier shift . 246) (cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) ($string
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (bitwise-xor-expression shift . 
    14) (bitwise-or-expression shift . 15) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) (logical-and-expression shift
    . 19) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23)
    ($:* shift . 24) ($:& shift . 25) (#{$:\x28;}# shift . 26) (
    primary-expression shift . 27) (logical-or-expression shift . 28) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (unary-expression shift . 34) 
    (conditional-expression shift . 35) (assignment-expression shift . 344)) (
    (#{$:\x28;}# reduce . 226) (#{$:\x5b;}# reduce . 226) (#{$:\x29;}# reduce 
    . 226) ($:, reduce . 226)) (($:static shift . 338) (#{$:\x5d;}# shift . 
    339) (cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 340) ($:& shift
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 341) ($:restrict shift . 81) ($:volatile 
    shift . 82) ($:const shift . 83) (type-qualifier shift . 219) (
    type-qualifier-list shift . 342)) ((#{$:\x29;}# shift . 334) (cpp-ident 
    shift . 12) ($ident shift . 13) (identifier shift . 335) (identifier-list 
    shift . 336) (typename shift . 67) ($:enum shift . 68) ($:union shift . 69
    ) ($:struct shift . 70) ($:_Complex shift . 71) ($:double shift . 72) (
    $:float shift . 73) ($:char shift . 74) ($:unsigned shift . 75) ($:long 
    shift . 76) ($:int shift . 77) ($:signed shift . 78) ($:short shift . 79) 
    ($:inline shift . 80) ($:restrict shift . 81) ($:volatile shift . 82) (
    $:const shift . 83) (typedef-name shift . 84) (enum-specifier shift . 85) 
    (struct-or-union-specifier shift . 86) (complex-type-specifier shift . 87)
    ($:_Bool shift . 88) (float-type-specifier shift . 89) (
    fixed-type-specifier shift . 90) ($:void shift . 91) ($:typedef shift . 92
    ) ($:static shift . 93) ($:register shift . 94) ($:extern shift . 95) (
    $:auto shift . 96) (function-specifier shift . 97) (type-qualifier shift 
    . 146) (type-specifier shift . 147) (storage-class-specifier shift . 100) 
    (declaration-specifiers shift . 223) (parameter-declaration shift . 224) (
    parameter-list shift . 225) (parameter-type-list shift . 337)) (($:, 
    reduce . 198) (#{$:\x29;}# reduce . 198)) ((#{$:\x29;}# reduce . 196)) ((
    #{$:\x5d;}# shift . 333)) ((#{$:\x28;}# reduce . 222) (#{$:\x5b;}# reduce 
    . 222) (#{$:\x29;}# reduce . 222) ($:, reduce . 222)) ((#{$:\x28;}# reduce
    . 217) (#{$:\x5b;}# reduce . 217) (#{$:\x29;}# reduce . 217) ($:, reduce 
    . 217)) ((#{$:\x5d;}# shift . 332)) (($:= reduce . 14) ($:+= reduce . 14) 
    ($:-= reduce . 14) ($:*= reduce . 14) ($:/= reduce . 14) ($:%= reduce . 14
    ) ($:<<= reduce . 14) ($:>>= reduce . 14) ($:&= reduce . 14) ($:^= reduce 
    . 14) ($:|= reduce . 14) ($:-- reduce . 14) ($:++ reduce . 14) ($:-> 
    reduce . 14) ($:. reduce . 14) (#{$:\x28;}# reduce . 14) (#{$:\x5b;}# 
    reduce . 14) ($:? reduce . 14) ($:* reduce . 14) ($:/ reduce . 14) ($:% 
    reduce . 14) ($:- reduce . 14) ($:+ reduce . 14) ($:<< reduce . 14) ($:>> 
    reduce . 14) ($:>= reduce . 14) ($:<= reduce . 14) ($:> reduce . 14) ($:< 
    reduce . 14) ($:== reduce . 14) ($:!= reduce . 14) ($:& reduce . 14) ($:^ 
    reduce . 14) ($:| reduce . 14) ($:&& reduce . 14) ($:|| reduce . 14) ($:, 
    reduce . 14) (#{$:\x29;}# reduce . 14) (#{$:\x5d;}# reduce . 14) ($:: 
    reduce . 14) (#{$:\x7d;}# reduce . 14) ($end reduce . 14) (#{$:;}# reduce 
    . 14)) ((#{$:\x7d;}# shift . 329) (cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) ($string
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (bitwise-xor-expression shift . 
    14) (bitwise-or-expression shift . 15) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) (logical-and-expression shift
    . 19) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23)
    ($:* shift . 24) ($:& shift . 25) (#{$:\x28;}# shift . 26) (
    primary-expression shift . 27) (logical-or-expression shift . 28) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (unary-expression shift . 34) 
    (conditional-expression shift . 35) (#{$:\x7b;}# shift . 239) (
    assignment-expression shift . 240) (initializer shift . 330) ($:. shift . 
    235) (#{$:\x5b;}# shift . 236) (designator shift . 237) (designator-list 
    shift . 238) (designation shift . 331)) ((#{$:\x7d;}# reduce . 234) ($:, 
    reduce . 234)) (($:, shift . 327) (#{$:\x7d;}# shift . 328)) ((cpp-ident 
    reduce . 238) ($ident reduce . 238) ($chlit reduce . 238) ($float reduce 
    . 238) ($fixed reduce . 238) ($string reduce . 238) (#{$:\x28;}# reduce . 
    238) ($:! reduce . 238) ($:~ reduce . 238) ($:- reduce . 238) ($:+ reduce 
    . 238) ($:* reduce . 238) ($:& reduce . 238) ($:sizeof reduce . 238) ($:--
    reduce . 238) ($:++ reduce . 238) (#{$:\x7b;}# reduce . 238)) (($:= 
    reduce . 240) ($:. reduce . 240) (#{$:\x5b;}# reduce . 240)) ((#{$:\x5d;}#
    shift . 326)) (($:= reduce . 242) (#{$:\x5b;}# reduce . 242) ($:. reduce 
    . 242)) (($:= reduce . 241) (#{$:\x5b;}# reduce . 241) ($:. reduce . 241))
    ((cast-expression shift . 1) (multiplicative-expression shift . 2) (
    additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    #{$:\x7b;}# shift . 239) (assignment-expression shift . 240) (initializer 
    shift . 330) ($:. shift . 235) (#{$:\x5b;}# shift . 236) (designator shift
    . 237) (designator-list shift . 238) (designation shift . 331) (
    #{$:\x7d;}# shift . 367)) ((#{$:\x7d;}# reduce . 232) ($:, reduce . 232)) 
    (($:= reduce . 15) ($:+= reduce . 15) ($:-= reduce . 15) ($:*= reduce . 15
    ) ($:/= reduce . 15) ($:%= reduce . 15) ($:<<= reduce . 15) ($:>>= reduce 
    . 15) ($:&= reduce . 15) ($:^= reduce . 15) ($:|= reduce . 15) ($:-- 
    reduce . 15) ($:++ reduce . 15) ($:-> reduce . 15) ($:. reduce . 15) (
    #{$:\x28;}# reduce . 15) (#{$:\x5b;}# reduce . 15) ($:? reduce . 15) ($:* 
    reduce . 15) ($:/ reduce . 15) ($:% reduce . 15) ($:- reduce . 15) ($:+ 
    reduce . 15) ($:<< reduce . 15) ($:>> reduce . 15) ($:>= reduce . 15) (
    $:<= reduce . 15) ($:> reduce . 15) ($:< reduce . 15) ($:== reduce . 15) (
    $:!= reduce . 15) ($:& reduce . 15) ($:^ reduce . 15) ($:| reduce . 15) (
    $:&& reduce . 15) ($:|| reduce . 15) ($:, reduce . 15) (#{$:\x29;}# reduce
    . 15) (#{$:\x5d;}# reduce . 15) ($:: reduce . 15) (#{$:\x7d;}# reduce . 
    15) ($end reduce . 15) (#{$:;}# reduce . 15)) ((#{$:\x7d;}# reduce . 237) 
    ($:, reduce . 237)) ((cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) ($string
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (bitwise-xor-expression shift . 
    14) (bitwise-or-expression shift . 15) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) (logical-and-expression shift
    . 19) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23)
    ($:* shift . 24) ($:& shift . 25) (#{$:\x28;}# shift . 26) (
    primary-expression shift . 27) (logical-or-expression shift . 28) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (unary-expression shift . 34) 
    (conditional-expression shift . 35) (#{$:\x7b;}# shift . 239) (
    assignment-expression shift . 240) (initializer shift . 366)) ((
    #{$:\x28;}# reduce . 223) (#{$:\x5b;}# reduce . 223) (#{$:\x29;}# reduce 
    . 223) ($:, reduce . 223)) ((#{$:\x28;}# reduce . 221) (#{$:\x5b;}# reduce
    . 221) (#{$:\x29;}# reduce . 221) ($:, reduce . 221)) ((#{$:;}# reduce . 
    188) ($:: reduce . 188) (#{$:\x28;}# reduce . 188) (#{$:\x5b;}# reduce . 
    188) ($:, reduce . 188) (#{$:\x29;}# reduce . 188)) ((#{$:\x29;}# reduce 
    . 202) ($:, reduce . 202)) ((#{$:\x29;}# shift . 364) ($:, shift . 365)) (
    (#{$:\x29;}# shift . 363)) (($:restrict shift . 81) ($:volatile shift . 82
    ) ($:const shift . 83) (type-qualifier shift . 219) (type-qualifier-list 
    shift . 362)) ((#{$:;}# reduce . 181) ($:: reduce . 181) (#{$:\x28;}# 
    reduce . 181) (#{$:\x5b;}# reduce . 181) ($:, reduce . 181) (#{$:\x29;}# 
    reduce . 181)) ((#{$:\x5d;}# shift . 361) (cpp-ident reduce . 27) ($ident 
    reduce . 27) ($chlit reduce . 27) ($float reduce . 27) ($fixed reduce . 27
    ) ($string reduce . 27) ($:! reduce . 27) ($:~ reduce . 27) ($:- reduce . 
    27) ($:+ reduce . 27) ($:* reduce . 27) ($:& reduce . 27) ($:sizeof reduce
    . 27) ($:-- reduce . 27) ($:++ reduce . 27) (#{$:\x28;}# reduce . 27)) ((
    #{$:\x5d;}# shift . 360)) ((cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) ($string
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (bitwise-xor-expression shift . 
    14) (bitwise-or-expression shift . 15) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) (logical-and-expression shift
    . 19) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23)
    ($:& shift . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27)
    (logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 356) ($:restrict shift . 81) ($:volatile 
    shift . 82) ($:const shift . 83) (type-qualifier shift . 246) (#{$:\x5d;}#
    shift . 357) ($:static shift . 358) ($:* shift . 359)) ((#{$:\x28;}# 
    reduce . 215) (#{$:\x5b;}# reduce . 215) (#{$:\x29;}# reduce . 215) ($:, 
    reduce . 215)) ((#{$:\x5d;}# shift . 355)) ((#{$:\x5d;}# shift . 354)) ((
    #{$:\x28;}# reduce . 210) (#{$:\x5b;}# reduce . 210) (#{$:\x29;}# reduce 
    . 210) ($:, reduce . 210)) ((#{$:\x28;}# reduce . 177) (#{$:\x5b;}# reduce
    . 177) (#{$:\x29;}# reduce . 177) ($:, reduce . 177) (#{$:;}# reduce . 
    177) ($:: reduce . 177)) ((#{$:;}# reduce . 159) ($:, reduce . 159)) ((
    #{$:;}# reduce . 157) ($:, reduce . 157)) ((#{$:\x7d;}# reduce . 298) (
    $:const reduce . 298) ($:volatile reduce . 298) ($:restrict reduce . 298) 
    ($:void reduce . 298) ($:_Bool reduce . 298) (typename reduce . 298) (
    $:enum reduce . 298) ($:struct reduce . 298) ($:union reduce . 298) (
    $:_Complex reduce . 298) ($:float reduce . 298) ($:double reduce . 298) (
    $:long reduce . 298) ($:short reduce . 298) ($:signed reduce . 298) ($:int
    reduce . 298) ($:unsigned reduce . 298) ($:char reduce . 298) ($lone-comm
    reduce . 298)) ((#{$:\x7d;}# reduce . 290) ($:const reduce . 290) (
    $:volatile reduce . 290) ($:restrict reduce . 290) ($:void reduce . 290) (
    $:_Bool reduce . 290) (typename reduce . 290) ($:enum reduce . 290) (
    $:struct reduce . 290) ($:union reduce . 290) ($:_Complex reduce . 290) (
    $:float reduce . 290) ($:double reduce . 290) ($:long reduce . 290) (
    $:short reduce . 290) ($:signed reduce . 290) ($:int reduce . 290) (
    $:unsigned reduce . 290) ($:char reduce . 290) ($lone-comm reduce . 290)) 
    ((#{$:\x7d;}# reduce . 151) ($:const reduce . 151) ($:volatile reduce . 
    151) ($:restrict reduce . 151) ($:void reduce . 151) ($:_Bool reduce . 151
    ) (typename reduce . 151) ($:enum reduce . 151) ($:struct reduce . 151) (
    $:union reduce . 151) ($:_Complex reduce . 151) ($:float reduce . 151) (
    $:double reduce . 151) ($:long reduce . 151) ($:short reduce . 151) (
    $:signed reduce . 151) ($:int reduce . 151) ($:unsigned reduce . 151) (
    $:char reduce . 151) ($lone-comm reduce . 151)) ((#{$:\x29;}# reduce . 162
    ) ($:inline reduce . 162) ($:auto reduce . 162) ($:extern reduce . 162) (
    $:register reduce . 162) ($:static reduce . 162) ($:typedef reduce . 162) 
    ($:const reduce . 162) ($:volatile reduce . 162) ($:restrict reduce . 162)
    ($:void reduce . 162) ($:_Bool reduce . 162) (typename reduce . 162) (
    $:enum reduce . 162) ($:struct reduce . 162) ($:union reduce . 162) (
    $:_Complex reduce . 162) ($:float reduce . 162) ($:double reduce . 162) (
    $:long reduce . 162) ($:short reduce . 162) ($:signed reduce . 162) ($:int
    reduce . 162) ($:unsigned reduce . 162) ($:char reduce . 162) ($:* reduce
    . 162) (#{$:\x5b;}# reduce . 162) (#{$:\x28;}# reduce . 162) ($:, reduce 
    . 162) (cpp-ident reduce . 162) ($ident reduce . 162) ($:: reduce . 162)) 
    ((#{$:\x28;}# reduce . 216) (#{$:\x5b;}# reduce . 216) (#{$:\x29;}# reduce
    . 216) ($:, reduce . 216)) ((#{$:\x28;}# reduce . 214) (#{$:\x5b;}# 
    reduce . 214) (#{$:\x29;}# reduce . 214) ($:, reduce . 214)) ((#{$:\x5d;}#
    shift . 372)) ((#{$:;}# reduce . 179) ($:: reduce . 179) (#{$:\x28;}# 
    reduce . 179) (#{$:\x5b;}# reduce . 179) ($:, reduce . 179) (#{$:\x29;}# 
    reduce . 179)) ((cast-expression shift . 1) (multiplicative-expression 
    shift . 2) (additive-expression shift . 3) (shift-expression shift . 4) (
    relational-expression shift . 5) (equality-expression shift . 6) (
    bitwise-and-expression shift . 7) ($string shift . 8) ($chlit shift . 9) (
    $float shift . 10) ($fixed shift . 11) (cpp-ident shift . 12) ($ident 
    shift . 13) (bitwise-xor-expression shift . 14) (bitwise-or-expression 
    shift . 15) (string-literal shift . 16) (constant shift . 17) (identifier 
    shift . 18) (logical-and-expression shift . 19) ($:! shift . 20) ($:~ 
    shift . 21) ($:- shift . 22) ($:+ shift . 23) ($:* shift . 24) ($:& shift 
    . 25) (#{$:\x28;}# shift . 26) (primary-expression shift . 27) (
    logical-or-expression shift . 28) ($:sizeof shift . 29) (unary-operator 
    shift . 30) ($:-- shift . 31) ($:++ shift . 32) (postfix-expression shift 
    . 33) (unary-expression shift . 34) (conditional-expression shift . 35) (
    assignment-expression shift . 371)) ((#{$:\x5d;}# shift . 370) (cpp-ident 
    reduce . 27) ($ident reduce . 27) ($chlit reduce . 27) ($float reduce . 27
    ) ($fixed reduce . 27) ($string reduce . 27) ($:! reduce . 27) ($:~ reduce
    . 27) ($:- reduce . 27) ($:+ reduce . 27) ($:* reduce . 27) ($:& reduce 
    . 27) ($:sizeof reduce . 27) ($:-- reduce . 27) ($:++ reduce . 27) (
    #{$:\x28;}# reduce . 27)) ((#{$:;}# reduce . 180) ($:: reduce . 180) (
    #{$:\x28;}# reduce . 180) (#{$:\x5b;}# reduce . 180) ($:, reduce . 180) (
    #{$:\x29;}# reduce . 180)) ((#{$:;}# reduce . 185) ($:: reduce . 185) (
    #{$:\x28;}# reduce . 185) (#{$:\x5b;}# reduce . 185) ($:, reduce . 185) (
    #{$:\x29;}# reduce . 185)) ((cast-expression shift . 1) (
    multiplicative-expression shift . 2) (additive-expression shift . 3) (
    shift-expression shift . 4) (relational-expression shift . 5) (
    equality-expression shift . 6) (bitwise-and-expression shift . 7) ($string
    shift . 8) ($chlit shift . 9) ($float shift . 10) ($fixed shift . 11) (
    cpp-ident shift . 12) ($ident shift . 13) (bitwise-xor-expression shift . 
    14) (bitwise-or-expression shift . 15) (string-literal shift . 16) (
    constant shift . 17) (identifier shift . 18) (logical-and-expression shift
    . 19) ($:! shift . 20) ($:~ shift . 21) ($:- shift . 22) ($:+ shift . 23)
    ($:* shift . 24) ($:& shift . 25) (#{$:\x28;}# shift . 26) (
    primary-expression shift . 27) (logical-or-expression shift . 28) (
    $:sizeof shift . 29) (unary-operator shift . 30) ($:-- shift . 31) ($:++ 
    shift . 32) (postfix-expression shift . 33) (unary-expression shift . 34) 
    (conditional-expression shift . 35) (assignment-expression shift . 369) (
    $:restrict shift . 81) ($:volatile shift . 82) ($:const shift . 83) (
    type-qualifier shift . 246)) ((#{$:;}# reduce . 186) ($:: reduce . 186) (
    #{$:\x28;}# reduce . 186) (#{$:\x5b;}# reduce . 186) ($:, reduce . 186) (
    #{$:\x29;}# reduce . 186)) ((#{$:;}# reduce . 187) ($:: reduce . 187) (
    #{$:\x28;}# reduce . 187) (#{$:\x5b;}# reduce . 187) ($:, reduce . 187) (
    #{$:\x29;}# reduce . 187)) ((cpp-ident shift . 12) ($ident shift . 13) (
    identifier shift . 368)) ((#{$:\x7d;}# reduce . 236) ($:, reduce . 236)) (
    (#{$:\x7d;}# reduce . 233) ($:, reduce . 233)) ((#{$:\x29;}# reduce . 203)
    ($:, reduce . 203)) ((#{$:\x5d;}# shift . 374)) ((#{$:;}# reduce . 184) (
    $:: reduce . 184) (#{$:\x28;}# reduce . 184) (#{$:\x5b;}# reduce . 184) (
    $:, reduce . 184) (#{$:\x29;}# reduce . 184)) ((#{$:\x5d;}# shift . 373)) 
    ((#{$:;}# reduce . 178) ($:: reduce . 178) (#{$:\x28;}# reduce . 178) (
    #{$:\x5b;}# reduce . 178) ($:, reduce . 178) (#{$:\x29;}# reduce . 178)) (
    (#{$:;}# reduce . 183) ($:: reduce . 183) (#{$:\x28;}# reduce . 183) (
    #{$:\x5b;}# reduce . 183) ($:, reduce . 183) (#{$:\x29;}# reduce . 183)) (
    (#{$:;}# reduce . 182) ($:: reduce . 182) (#{$:\x28;}# reduce . 182) (
    #{$:\x5b;}# reduce . 182) ($:, reduce . 182) (#{$:\x29;}# reduce . 182))))

(define rto-v
  #($start translation-unit-proxy primary-expression primary-expression 
    primary-expression primary-expression postfix-expression 
    postfix-expression postfix-expression postfix-expression 
    postfix-expression postfix-expression postfix-expression 
    postfix-expression postfix-expression postfix-expression 
    argument-expression-list argument-expression-list argument-expression-list
    argument-expression-list unary-expression unary-expression 
    unary-expression unary-expression unary-expression unary-expression 
    unary-operator unary-operator unary-operator unary-operator unary-operator
    unary-operator cast-expression cast-expression multiplicative-expression 
    multiplicative-expression multiplicative-expression 
    multiplicative-expression additive-expression additive-expression 
    additive-expression shift-expression shift-expression shift-expression 
    relational-expression relational-expression relational-expression 
    relational-expression relational-expression equality-expression 
    equality-expression equality-expression bitwise-and-expression 
    bitwise-and-expression bitwise-xor-expression bitwise-xor-expression 
    bitwise-or-expression bitwise-or-expression logical-and-expression 
    logical-and-expression logical-or-expression logical-or-expression 
    conditional-expression conditional-expression assignment-expression 
    assignment-expression assignment-operator assignment-operator 
    assignment-operator assignment-operator assignment-operator 
    assignment-operator assignment-operator assignment-operator 
    assignment-operator assignment-operator assignment-operator expression 
    expression constant-expression declaration declaration $P1 
    declaration-specifiers declaration-specifiers declaration-specifiers 
    declaration-specifiers declaration-specifiers declaration-specifiers 
    declaration-specifiers declaration-specifiers init-declarator-list 
    init-declarator-list init-declarator init-declarator 
    storage-class-specifier storage-class-specifier storage-class-specifier 
    storage-class-specifier storage-class-specifier type-specifier 
    type-specifier type-specifier type-specifier type-specifier type-specifier
    type-specifier type-specifier fixed-type-specifier fixed-type-specifier 
    fixed-type-specifier fixed-type-specifier fixed-type-specifier 
    fixed-type-specifier fixed-type-specifier fixed-type-specifier 
    fixed-type-specifier fixed-type-specifier fixed-type-specifier 
    fixed-type-specifier fixed-type-specifier fixed-type-specifier 
    fixed-type-specifier fixed-type-specifier fixed-type-specifier 
    fixed-type-specifier fixed-type-specifier fixed-type-specifier 
    fixed-type-specifier fixed-type-specifier fixed-type-specifier 
    fixed-type-specifier fixed-type-specifier fixed-type-specifier 
    float-type-specifier float-type-specifier float-type-specifier 
    complex-type-specifier complex-type-specifier complex-type-specifier 
    complex-type-specifier struct-or-union-specifier struct-or-union-specifier
    struct-or-union-specifier struct-or-union-specifier 
    struct-or-union-specifier struct-or-union-specifier 
    struct-declaration-list struct-declaration-list struct-declaration-list 
    struct-declaration-list struct-declaration specifier-qualifier-list 
    specifier-qualifier-list specifier-qualifier-list specifier-qualifier-list
    struct-declarator-list struct-declarator-list struct-declarator 
    struct-declarator struct-declarator enum-specifier enum-specifier 
    enum-specifier enum-specifier enum-specifier enumerator-list 
    enumerator-list enumerator enumerator type-qualifier type-qualifier 
    type-qualifier function-specifier declarator declarator direct-declarator 
    direct-declarator direct-declarator direct-declarator direct-declarator 
    direct-declarator direct-declarator direct-declarator direct-declarator 
    direct-declarator direct-declarator direct-declarator direct-declarator 
    pointer pointer pointer pointer type-qualifier-list type-qualifier-list 
    parameter-type-list parameter-type-list parameter-list parameter-list 
    parameter-declaration parameter-declaration parameter-declaration 
    identifier-list identifier-list type-name type-name abstract-declarator 
    abstract-declarator abstract-declarator direct-abstract-declarator 
    direct-abstract-declarator direct-abstract-declarator 
    direct-abstract-declarator direct-abstract-declarator 
    direct-abstract-declarator direct-abstract-declarator 
    direct-abstract-declarator direct-abstract-declarator 
    direct-abstract-declarator direct-abstract-declarator 
    direct-abstract-declarator direct-abstract-declarator 
    direct-abstract-declarator direct-abstract-declarator 
    direct-abstract-declarator direct-abstract-declarator 
    direct-abstract-declarator direct-abstract-declarator 
    direct-abstract-declarator direct-abstract-declarator typedef-name 
    initializer initializer initializer initializer-list initializer-list 
    initializer-list initializer-list designation designator-list 
    designator-list designator designator statement statement statement 
    statement statement statement statement labeled-statement 
    labeled-statement labeled-statement compound-statement compound-statement 
    block-item-list block-item-list block-item block-item expression-statement
    expression-statement selection-statement selection-statement 
    selection-statement iteration-statement iteration-statement 
    iteration-statement iteration-statement iteration-statement 
    iteration-statement initial-clause initial-clause initial-clause 
    jump-statement jump-statement jump-statement jump-statement jump-statement
    translation-unit translation-unit external-declaration 
    external-declaration external-declaration external-declaration 
    external-declaration function-definition function-definition 
    declaration-list declaration-list opt-code-comment opt-code-comment 
    identifier identifier constant constant constant string-literal 
    string-literal code-comment lone-comment cpp-statement))

(define mtab
  '((cpp-stmt . cpp-stmt) ($lone-comm . $lone-comm) ($code-comm . $code-comm
    ) ($chlit . $chlit) ($float . $float) ($fixed . $fixed) (cpp-ident . 
    cpp-ident) ($ident . $ident) ($string . $string) ("return" . $:return) (
    "break" . $:break) ("continue" . $:continue) ("goto" . $:goto) ("for" . 
    $:for) ("do" . $:do) ("while" . $:while) ("switch" . $:switch) ("else" . 
    $:else) ("then" . $:then) ("if" . $:if) ("default" . $:default) ("case" . 
    $:case) (typename . typename) ("..." . $:...) ("inline" . $:inline) (
    "restrict" . $:restrict) ("volatile" . $:volatile) ("const" . $:const) (
    "enum" . $:enum) ("union" . $:union) ("struct" . $:struct) ("_Complex" . 
    $:_Complex) ("double" . $:double) ("float" . $:float) ("char" . $:char) (
    "unsigned" . $:unsigned) ("long" . $:long) ("signed" . $:signed) ("int" . 
    $:int) ("imp" . $:imp) ("short" . $:short) ("_Bool" . $:_Bool) ("void" . 
    $:void) ("typedef" . $:typedef) ("static" . $:static) ("register" . 
    $:register) ("extern" . $:extern) ("auto" . $:auto) (";" . #{$:;}#) ("|=" 
    . $:|=) ("^=" . $:^=) ("&=" . $:&=) (">>=" . $:>>=) ("<<=" . $:<<=) ("%=" 
    . $:%=) ("/=" . $:/=) ("*=" . $:*=) ("-=" . $:-=) ("+=" . $:+=) ("=" . $:=
    ) (":" . $::) ("?" . $:?) ("||" . $:||) ("&&" . $:&&) ("|" . $:|) ("^" . 
    $:^) ("!=" . $:!=) ("==" . $:==) (">=" . $:>=) ("<=" . $:<=) (">" . $:>) (
    "<" . $:<) (">>" . $:>>) ("<<" . $:<<) ("%" . $:%) ("/" . $:/) ("!" . $:!)
    ("~" . $:~) ("-" . $:-) ("+" . $:+) ("*" . $:*) ("&" . $:&) ("sizeof" . 
    $:sizeof) ("," . $:,) ("}" . #{$:\x7d;}#) ("{" . #{$:\x7b;}#) ("--" . $:--
    ) ("++" . $:++) ("->" . $:->) ("." . $:.) ("]" . #{$:\x5d;}#) ("[" . 
    #{$:\x5b;}#) (")" . #{$:\x29;}#) ("(" . #{$:\x28;}#) ($end . $end)))

;;; end tables
