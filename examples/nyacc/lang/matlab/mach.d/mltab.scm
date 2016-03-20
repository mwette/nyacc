;; mach.d/mltab.scm

;; Copyright (C) 2015,2016 Matthew R. Wette
;; 
;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;; or any later version published by the Free Software Foundation.  See the
;; file COPYING included with the this distribution.

(define len-v
  #(1 1 1 1 2 1 2 4 3 2 0 2 2 1 1 2 10 9 8 7 6 5 1 3 1 2 1 1 1 5 4 8 9 8 6 9
    8 6 9 6 2 3 1 3 1 1 1 2 4 5 0 5 1 1 3 3 1 3 1 3 1 3 1 3 3 1 3 3 3 3 1 3 3
    1 3 3 3 3 3 3 3 3 1 2 2 2 1 1 2 2 1 4 3 1 1 3 2 3 1 3 1 1 1 3 1 2 2 3 1 1
    1 1 1 1 1 1))

(define pat-v
  #((($ident shift . 1) ($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift
    . 4) (ident shift . 5) ($:clear shift . 6) ($:global shift . 7) (
    $:function shift . 8) ($lone-comm shift . 9) (command shift . 10) (
    $:return shift . 11) ($:switch shift . 12) ($:if shift . 13) ($:while 
    shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 16) (lval-expr shift 
    . 17) (term shift . 18) (function-decl-line shift . 19) (
    non-comment-statement shift . 20) (lone-comment shift . 21) (function-decl
    shift . 22) (function-defn shift . 23) (statement-list shift . 24) (
    statement shift . 25) (function-file shift . 26) (script-file shift . 27) 
    (mfile shift . 28)) ((#{$:\x28;}# reduce . 111) ($:= reduce . 111) ($:. 
    reduce . 111) (#\newline reduce . 111) (#{$:;}# reduce . 111) ($:, reduce 
    . 111) ($ident reduce . 111) ($:: reduce . 111) ($:| reduce . 111) ($:& 
    reduce . 111) ($:== reduce . 111) ($:~= reduce . 111) ($:>= reduce . 111) 
    ($:<= reduce . 111) ($:> reduce . 111) ($:< reduce . 111) ($:+ reduce . 
    111) ($:- reduce . 111) ($:.^ reduce . 111) ($:.\ reduce . 111) ($:./ 
    reduce . 111) ($:.* reduce . 111) ($:^ reduce . 111) ($:\ reduce . 111) (
    $:/ reduce . 111) ($:* reduce . 111) ($:.' reduce . 111) ($:' reduce . 111
    ) (#{$:\x5d;}# reduce . 111) (#{$:\x29;}# reduce . 111)) ((#{$:\x5b;}# 
    reduce . 110) ($:for reduce . 110) ($:while reduce . 110) ($:if reduce . 
    110) ($:switch reduce . 110) ($:return reduce . 110) ($:global reduce . 
    110) ($:clear reduce . 110) ($ident reduce . 110) (#\newline reduce . 110)
    (#{$:;}# reduce . 110) ($:, reduce . 110) ($lone-comm reduce . 110) (
    $:end reduce . 110) ($:otherwise reduce . 110) ($:case reduce . 110) (
    $:else reduce . 110) ($:elseif reduce . 110) ($end reduce . 110) (
    $:function reduce . 110)) ((#{$:\x5b;}# reduce . 109) ($:for reduce . 109)
    ($:while reduce . 109) ($:if reduce . 109) ($:switch reduce . 109) (
    $:return reduce . 109) ($:global reduce . 109) ($:clear reduce . 109) (
    $ident reduce . 109) (#\newline reduce . 109) (#{$:;}# reduce . 109) ($:, 
    reduce . 109) ($lone-comm reduce . 109) ($:end reduce . 109) ($:otherwise 
    reduce . 109) ($:case reduce . 109) ($:else reduce . 109) ($:elseif reduce
    . 109) ($end reduce . 109) ($:function reduce . 109)) ((#{$:\x5b;}# 
    reduce . 108) ($:for reduce . 108) ($:while reduce . 108) ($:if reduce . 
    108) ($:switch reduce . 108) ($:return reduce . 108) ($:global reduce . 
    108) ($:clear reduce . 108) ($ident reduce . 108) (#\newline reduce . 108)
    (#{$:;}# reduce . 108) ($:, reduce . 108) ($lone-comm reduce . 108) (
    $:end reduce . 108) ($:otherwise reduce . 108) ($:case reduce . 108) (
    $:else reduce . 108) ($:elseif reduce . 108) ($end reduce . 108) (
    $:function reduce . 108)) (($:= reduce . 90) (#{$:\x28;}# reduce . 90) (
    $:. reduce . 90) (#\newline reduce . 90) (#{$:;}# reduce . 90) ($:, reduce
    . 90) ($:' reduce . 90) ($:.' reduce . 90) ($:* reduce . 90) ($:/ reduce 
    . 90) ($:\ reduce . 90) ($:^ reduce . 90) ($:.* reduce . 90) ($:./ reduce 
    . 90) ($:.\ reduce . 90) ($:.^ reduce . 90) ($:- reduce . 90) ($:+ reduce 
    . 90) ($:< reduce . 90) ($:> reduce . 90) ($:<= reduce . 90) ($:>= reduce 
    . 90) ($:~= reduce . 90) ($:== reduce . 90) ($:& reduce . 90) ($:| reduce 
    . 90) ($:: reduce . 90) (#{$:\x5d;}# reduce . 90) (#{$:\x29;}# reduce . 90
    )) (($ident reduce . 45)) (($ident reduce . 44)) ((#{$:\x5b;}# shift . 68)
    ($ident shift . 1) (ident shift . 69)) (($lone-comm reduce . 115) ($:, 
    reduce . 115) (#{$:;}# reduce . 115) (#\newline reduce . 115) ($ident 
    reduce . 115) ($:clear reduce . 115) ($:global reduce . 115) ($:return 
    reduce . 115) ($:switch reduce . 115) ($:if reduce . 115) ($:while reduce 
    . 115) ($:for reduce . 115) (#{$:\x5b;}# reduce . 115) ($:end reduce . 115
    ) ($:else reduce . 115) ($:elseif reduce . 115) ($end reduce . 115) (
    $:function reduce . 115) ($:case reduce . 115) ($:otherwise reduce . 115))
    (($ident shift . 1) (ident shift . 66) (arg-list shift . 67)) (($:, shift
    . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (term shift . 65)) ((
    $string shift . 42) ($float shift . 43) ($fixed shift . 44) ($ident shift 
    . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47)
    (number shift . 48) (ident shift . 5) (primary-expr shift . 49) (
    lval-expr shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (
    postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr shift . 56) (
    add-expr shift . 57) (rel-expr shift . 58) (equality-expr shift . 59) (
    and-expr shift . 60) (or-expr shift . 61) (expr shift . 64)) (($string 
    shift . 42) ($float shift . 43) ($fixed shift . 44) ($ident shift . 1) (
    #{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47) (
    number shift . 48) (ident shift . 5) (primary-expr shift . 49) (lval-expr 
    shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (
    postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr shift . 56) (
    add-expr shift . 57) (rel-expr shift . 58) (equality-expr shift . 59) (
    and-expr shift . 60) (or-expr shift . 61) (expr shift . 63)) (($string 
    shift . 42) ($float shift . 43) ($fixed shift . 44) ($ident shift . 1) (
    #{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47) (
    number shift . 48) (ident shift . 5) (primary-expr shift . 49) (lval-expr 
    shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (
    postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr shift . 56) (
    add-expr shift . 57) (rel-expr shift . 58) (equality-expr shift . 59) (
    and-expr shift . 60) (or-expr shift . 61) (expr shift . 62)) (($ident 
    shift . 1) (ident shift . 41)) (($ident shift . 1) (ident shift . 5) (
    lval-expr shift . 39) (lval-expr-list shift . 40)) (($:= shift . 36) (
    #{$:\x28;}# shift . 37) ($:. shift . 38)) (($lone-comm reduce . 28) ($:, 
    reduce . 28) (#{$:;}# reduce . 28) (#\newline reduce . 28) ($ident reduce 
    . 28) ($:clear reduce . 28) ($:global reduce . 28) ($:return reduce . 28) 
    ($:switch reduce . 28) ($:if reduce . 28) ($:while reduce . 28) ($:for 
    reduce . 28) (#{$:\x5b;}# reduce . 28) ($:end reduce . 28) ($:else reduce 
    . 28) ($:elseif reduce . 28) ($end reduce . 28) ($:function reduce . 28) (
    $:case reduce . 28) ($:otherwise reduce . 28)) (($lone-comm shift . 9) (
    lone-comment shift . 34) (lone-comment-list shift . 35) ($:end reduce . 13
    ) ($:, reduce . 13) (#{$:;}# reduce . 13) (#\newline reduce . 13) ($ident 
    reduce . 13) ($:clear reduce . 13) ($:global reduce . 13) ($:return reduce
    . 13) ($:switch reduce . 13) ($:if reduce . 13) ($:while reduce . 13) (
    $:for reduce . 13) (#{$:\x5b;}# reduce . 13) ($:function reduce . 13) (
    $end reduce . 13)) ((#{$:\x5b;}# reduce . 27) ($:for reduce . 27) ($:while
    reduce . 27) ($:if reduce . 27) ($:switch reduce . 27) ($:return reduce 
    . 27) ($:global reduce . 27) ($:clear reduce . 27) ($ident reduce . 27) (
    #\newline reduce . 27) (#{$:;}# reduce . 27) ($:, reduce . 27) ($lone-comm
    reduce . 27) ($:end reduce . 27) ($:else reduce . 27) ($:elseif reduce . 
    27) ($end reduce . 27) ($:function reduce . 27) ($:case reduce . 27) (
    $:otherwise reduce . 27)) ((#{$:\x5b;}# reduce . 26) ($:for reduce . 26) (
    $:while reduce . 26) ($:if reduce . 26) ($:switch reduce . 26) ($:return 
    reduce . 26) ($:global reduce . 26) ($:clear reduce . 26) ($ident reduce 
    . 26) (#\newline reduce . 26) (#{$:;}# reduce . 26) ($:, reduce . 26) (
    $lone-comm reduce . 26) ($:end reduce . 26) ($:else reduce . 26) ($:elseif
    reduce . 26) ($end reduce . 26) ($:function reduce . 26) ($:case reduce 
    . 26) ($:otherwise reduce . 26)) (($ident shift . 1) ($:, shift . 2) (
    #{$:;}# shift . 3) (#\newline shift . 4) (ident shift . 5) ($:clear shift 
    . 6) ($:global shift . 7) (command shift . 10) ($:return shift . 11) (
    $:switch shift . 12) ($:if shift . 13) ($:while shift . 14) ($:for shift 
    . 15) (#{$:\x5b;}# shift . 16) (lval-expr shift . 17) (term shift . 18) (
    non-comment-statement shift . 31) ($:end shift . 32) (opt-end shift . 33) 
    ($end reduce . 10) ($:function reduce . 10)) (($:function reduce . 5) (
    $end reduce . 5)) (($ident shift . 1) ($:, shift . 2) (#{$:;}# shift . 3) 
    (#\newline shift . 4) (ident shift . 5) ($:clear shift . 6) ($:global 
    shift . 7) ($lone-comm shift . 9) (command shift . 10) ($:return shift . 
    11) ($:switch shift . 12) ($:if shift . 13) ($:while shift . 14) ($:for 
    shift . 15) (#{$:\x5b;}# shift . 16) (lval-expr shift . 17) (term shift . 
    18) (non-comment-statement shift . 20) (lone-comment shift . 21) (
    statement shift . 30)) (($lone-comm reduce . 24) ($:, reduce . 24) (
    #{$:;}# reduce . 24) (#\newline reduce . 24) ($ident reduce . 24) ($:clear
    reduce . 24) ($:global reduce . 24) ($:return reduce . 24) ($:switch 
    reduce . 24) ($:if reduce . 24) ($:while reduce . 24) ($:for reduce . 24) 
    (#{$:\x5b;}# reduce . 24) ($end reduce . 3)) (($:function shift . 8) (
    function-decl-line shift . 19) (function-decl shift . 22) (function-defn 
    shift . 29) ($end reduce . 2)) (($end reduce . 1)) (($end accept . 0)) ((
    $:function reduce . 6) ($end reduce . 6)) (($end reduce . 4) ($lone-comm 
    reduce . 25) ($:, reduce . 25) (#{$:;}# reduce . 25) (#\newline reduce . 
    25) ($ident reduce . 25) ($:clear reduce . 25) ($:global reduce . 25) (
    $:return reduce . 25) ($:switch reduce . 25) ($:if reduce . 25) ($:while 
    reduce . 25) ($:for reduce . 25) (#{$:\x5b;}# reduce . 25)) (($:end shift 
    . 32) (opt-end shift . 121) ($ident shift . 1) ($:, shift . 2) (#{$:;}# 
    shift . 3) (#\newline shift . 4) (ident shift . 5) ($:clear shift . 6) (
    $:global shift . 7) ($lone-comm shift . 9) (command shift . 10) ($:return 
    shift . 11) ($:switch shift . 12) ($:if shift . 13) ($:while shift . 14) (
    $:for shift . 15) (#{$:\x5b;}# shift . 16) (lval-expr shift . 17) (term 
    shift . 18) (non-comment-statement shift . 20) (lone-comment shift . 21) (
    statement shift . 122) (statement-list shift . 123) ($end reduce . 10) (
    $:function reduce . 10)) (($:, shift . 2) (#{$:;}# shift . 3) (#\newline 
    shift . 4) (term shift . 119) (term-list shift . 120)) (($:function reduce
    . 9) ($end reduce . 9)) ((#\newline shift . 118)) (($lone-comm shift . 9)
    (lone-comment shift . 117) ($:end reduce . 12) ($:, reduce . 12) (#{$:;}#
    reduce . 12) (#\newline reduce . 12) ($ident reduce . 12) ($:clear reduce
    . 12) ($:global reduce . 12) ($:return reduce . 12) ($:switch reduce . 12
    ) ($:if reduce . 12) ($:while reduce . 12) ($:for reduce . 12) (
    #{$:\x5b;}# reduce . 12) ($:function reduce . 12) ($end reduce . 12)) ((
    $string shift . 42) ($float shift . 43) ($fixed shift . 44) ($ident shift 
    . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47)
    (number shift . 48) (ident shift . 5) (primary-expr shift . 49) (
    lval-expr shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (
    postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr shift . 56) (
    add-expr shift . 57) (rel-expr shift . 58) (equality-expr shift . 59) (
    and-expr shift . 60) (or-expr shift . 61) (expr shift . 116)) (($string 
    shift . 42) ($float shift . 43) ($fixed shift . 44) ($ident shift . 1) (
    #{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47) (
    number shift . 48) (ident shift . 5) (primary-expr shift . 49) (lval-expr 
    shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (
    postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr shift . 56) (
    add-expr shift . 57) (rel-expr shift . 58) (equality-expr shift . 59) (
    and-expr shift . 60) (or-expr shift . 61) ($:: shift . 113) (expr shift . 
    114) (expr-list shift . 115)) (($ident shift . 1) (ident shift . 112)) ((
    #{$:\x28;}# shift . 103) ($:. shift . 38) (#{$:\x5d;}# reduce . 42) ($:, 
    reduce . 42)) (($:, shift . 110) (#{$:\x5d;}# shift . 111)) (($:= shift . 
    109)) (($:, reduce . 114) (#{$:;}# reduce . 114) (#\newline reduce . 114) 
    ($:: reduce . 114) ($:| reduce . 114) ($:& reduce . 114) ($:== reduce . 
    114) ($:~= reduce . 114) ($:>= reduce . 114) ($:<= reduce . 114) ($:> 
    reduce . 114) ($:< reduce . 114) ($:+ reduce . 114) ($:- reduce . 114) (
    $:.^ reduce . 114) ($:.\ reduce . 114) ($:./ reduce . 114) ($:.* reduce . 
    114) ($:^ reduce . 114) ($:\ reduce . 114) ($:/ reduce . 114) ($:* reduce 
    . 114) ($:.' reduce . 114) ($:' reduce . 114) (#{$:\x29;}# reduce . 114) (
    #{$:\x5d;}# reduce . 114)) (($:, reduce . 113) (#{$:;}# reduce . 113) (
    #\newline reduce . 113) ($:: reduce . 113) ($:| reduce . 113) ($:& reduce 
    . 113) ($:== reduce . 113) ($:~= reduce . 113) ($:>= reduce . 113) ($:<= 
    reduce . 113) ($:> reduce . 113) ($:< reduce . 113) ($:+ reduce . 113) (
    $:- reduce . 113) ($:.^ reduce . 113) ($:.\ reduce . 113) ($:./ reduce . 
    113) ($:.* reduce . 113) ($:^ reduce . 113) ($:\ reduce . 113) ($:/ reduce
    . 113) ($:* reduce . 113) ($:.' reduce . 113) ($:' reduce . 113) (
    #{$:\x29;}# reduce . 113) (#{$:\x5d;}# reduce . 113)) (($:, reduce . 112) 
    (#{$:;}# reduce . 112) (#\newline reduce . 112) ($:: reduce . 112) ($:| 
    reduce . 112) ($:& reduce . 112) ($:== reduce . 112) ($:~= reduce . 112) (
    $:>= reduce . 112) ($:<= reduce . 112) ($:> reduce . 112) ($:< reduce . 
    112) ($:+ reduce . 112) ($:- reduce . 112) ($:.^ reduce . 112) ($:.\ 
    reduce . 112) ($:./ reduce . 112) ($:.* reduce . 112) ($:^ reduce . 112) (
    $:\ reduce . 112) ($:/ reduce . 112) ($:* reduce . 112) ($:.' reduce . 112
    ) ($:' reduce . 112) (#{$:\x29;}# reduce . 112) (#{$:\x5d;}# reduce . 112)
    ) ((#{$:\x5d;}# shift . 105) ($string shift . 42) ($float shift . 43) (
    $fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 45) (
    #{$:\x28;}# shift . 46) (string shift . 47) (number shift . 48) (ident 
    shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) ($:~ shift . 
    51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54) (
    unary-expr shift . 55) (mul-expr shift . 56) (add-expr shift . 57) (
    rel-expr shift . 58) (equality-expr shift . 59) (and-expr shift . 60) (
    or-expr shift . 61) (expr shift . 106) (matrix-row shift . 107) (
    matrix-row-list shift . 108)) (($string shift . 42) ($float shift . 43) (
    $fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 45) (
    #{$:\x28;}# shift . 46) (string shift . 47) (number shift . 48) (ident 
    shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) ($:~ shift . 
    51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54) (
    unary-expr shift . 55) (mul-expr shift . 56) (add-expr shift . 57) (
    rel-expr shift . 58) (equality-expr shift . 59) (and-expr shift . 60) (
    or-expr shift . 61) (expr shift . 104)) ((#\newline reduce . 94) (#{$:;}# 
    reduce . 94) ($:, reduce . 94) ($:' reduce . 94) ($:.' reduce . 94) ($:* 
    reduce . 94) ($:/ reduce . 94) ($:\ reduce . 94) ($:^ reduce . 94) ($:.* 
    reduce . 94) ($:./ reduce . 94) ($:.\ reduce . 94) ($:.^ reduce . 94) ($:-
    reduce . 94) ($:+ reduce . 94) ($:< reduce . 94) ($:> reduce . 94) ($:<= 
    reduce . 94) ($:>= reduce . 94) ($:~= reduce . 94) ($:== reduce . 94) ($:&
    reduce . 94) ($:| reduce . 94) ($:: reduce . 94) (#{$:\x29;}# reduce . 94
    ) (#{$:\x5d;}# reduce . 94)) ((#\newline reduce . 93) (#{$:;}# reduce . 93
    ) ($:, reduce . 93) ($:' reduce . 93) ($:.' reduce . 93) ($:* reduce . 93)
    ($:/ reduce . 93) ($:\ reduce . 93) ($:^ reduce . 93) ($:.* reduce . 93) 
    ($:./ reduce . 93) ($:.\ reduce . 93) ($:.^ reduce . 93) ($:- reduce . 93)
    ($:+ reduce . 93) ($:< reduce . 93) ($:> reduce . 93) ($:<= reduce . 93) 
    ($:>= reduce . 93) ($:~= reduce . 93) ($:== reduce . 93) ($:& reduce . 93)
    ($:| reduce . 93) ($:: reduce . 93) (#{$:\x29;}# reduce . 93) (
    #{$:\x5d;}# reduce . 93)) (($:, reduce . 87) (#{$:;}# reduce . 87) (
    #\newline reduce . 87) ($:: reduce . 87) ($:| reduce . 87) ($:& reduce . 
    87) ($:== reduce . 87) ($:~= reduce . 87) ($:>= reduce . 87) ($:<= reduce 
    . 87) ($:> reduce . 87) ($:< reduce . 87) ($:+ reduce . 87) ($:- reduce . 
    87) ($:.^ reduce . 87) ($:.\ reduce . 87) ($:./ reduce . 87) ($:.* reduce 
    . 87) ($:^ reduce . 87) ($:\ reduce . 87) ($:/ reduce . 87) ($:* reduce . 
    87) ($:.' reduce . 87) ($:' reduce . 87) (#{$:\x29;}# reduce . 87) (
    #{$:\x5d;}# reduce . 87)) ((#{$:\x28;}# shift . 103) ($:. shift . 38) ($:,
    reduce . 86) (#{$:;}# reduce . 86) (#\newline reduce . 86) ($:: reduce . 
    86) ($:| reduce . 86) ($:& reduce . 86) ($:== reduce . 86) ($:~= reduce . 
    86) ($:>= reduce . 86) ($:<= reduce . 86) ($:> reduce . 86) ($:< reduce . 
    86) ($:+ reduce . 86) ($:- reduce . 86) ($:.^ reduce . 86) ($:.\ reduce . 
    86) ($:./ reduce . 86) ($:.* reduce . 86) ($:^ reduce . 86) ($:\ reduce . 
    86) ($:/ reduce . 86) ($:* reduce . 86) ($:.' reduce . 86) ($:' reduce . 
    86) (#{$:\x29;}# reduce . 86) (#{$:\x5d;}# reduce . 86)) (($string shift 
    . 42) ($float shift . 43) ($fixed shift . 44) ($ident shift . 1) (
    #{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47) (
    number shift . 48) (ident shift . 5) (primary-expr shift . 49) (lval-expr 
    shift . 50) (postfix-expr shift . 102)) (($string shift . 42) ($float 
    shift . 43) ($fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 45
    ) (#{$:\x28;}# shift . 46) (string shift . 47) (number shift . 48) (ident 
    shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) (postfix-expr 
    shift . 101)) (($string shift . 42) ($float shift . 43) ($fixed shift . 44
    ) ($ident shift . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (
    string shift . 47) (number shift . 48) (ident shift . 5) (primary-expr 
    shift . 49) (lval-expr shift . 50) (postfix-expr shift . 100)) (($:' shift
    . 98) ($:.' shift . 99) (#\newline reduce . 82) (#{$:;}# reduce . 82) (
    $:, reduce . 82) ($:* reduce . 82) ($:/ reduce . 82) ($:\ reduce . 82) (
    $:^ reduce . 82) ($:.* reduce . 82) ($:./ reduce . 82) ($:.\ reduce . 82) 
    ($:.^ reduce . 82) ($:- reduce . 82) ($:+ reduce . 82) ($:< reduce . 82) (
    $:> reduce . 82) ($:<= reduce . 82) ($:>= reduce . 82) ($:~= reduce . 82) 
    ($:== reduce . 82) ($:& reduce . 82) ($:| reduce . 82) ($:: reduce . 82) (
    #{$:\x29;}# reduce . 82) (#{$:\x5d;}# reduce . 82)) (($:, reduce . 73) (
    #{$:;}# reduce . 73) (#\newline reduce . 73) ($:: reduce . 73) ($:| reduce
    . 73) ($:& reduce . 73) ($:== reduce . 73) ($:~= reduce . 73) ($:>= 
    reduce . 73) ($:<= reduce . 73) ($:> reduce . 73) ($:< reduce . 73) ($:+ 
    reduce . 73) ($:- reduce . 73) ($:.^ reduce . 73) ($:.\ reduce . 73) ($:./
    reduce . 73) ($:.* reduce . 73) ($:^ reduce . 73) ($:\ reduce . 73) ($:/ 
    reduce . 73) ($:* reduce . 73) (#{$:\x29;}# reduce . 73) (#{$:\x5d;}# 
    reduce . 73)) (($:* shift . 90) ($:/ shift . 91) ($:\ shift . 92) ($:^ 
    shift . 93) ($:.* shift . 94) ($:./ shift . 95) ($:.\ shift . 96) ($:.^ 
    shift . 97) (#\newline reduce . 70) (#{$:;}# reduce . 70) ($:, reduce . 70
    ) ($:- reduce . 70) ($:+ reduce . 70) ($:< reduce . 70) ($:> reduce . 70) 
    ($:<= reduce . 70) ($:>= reduce . 70) ($:~= reduce . 70) ($:== reduce . 70
    ) ($:& reduce . 70) ($:| reduce . 70) ($:: reduce . 70) (#{$:\x29;}# 
    reduce . 70) (#{$:\x5d;}# reduce . 70)) (($:+ shift . 88) ($:- shift . 89)
    ($:, reduce . 65) (#{$:;}# reduce . 65) (#\newline reduce . 65) ($:: 
    reduce . 65) ($:| reduce . 65) ($:& reduce . 65) ($:== reduce . 65) ($:~= 
    reduce . 65) ($:>= reduce . 65) ($:<= reduce . 65) ($:> reduce . 65) ($:< 
    reduce . 65) (#{$:\x29;}# reduce . 65) (#{$:\x5d;}# reduce . 65)) (($:< 
    shift . 84) ($:> shift . 85) ($:<= shift . 86) ($:>= shift . 87) (
    #\newline reduce . 62) (#{$:;}# reduce . 62) ($:, reduce . 62) ($:~= 
    reduce . 62) ($:== reduce . 62) ($:& reduce . 62) ($:| reduce . 62) ($:: 
    reduce . 62) (#{$:\x29;}# reduce . 62) (#{$:\x5d;}# reduce . 62)) (($:== 
    shift . 82) ($:~= shift . 83) ($:, reduce . 60) (#{$:;}# reduce . 60) (
    #\newline reduce . 60) ($:: reduce . 60) ($:| reduce . 60) ($:& reduce . 
    60) (#{$:\x29;}# reduce . 60) (#{$:\x5d;}# reduce . 60)) (($:& shift . 81)
    (#\newline reduce . 58) (#{$:;}# reduce . 58) ($:, reduce . 58) ($:| 
    reduce . 58) ($:: reduce . 58) (#{$:\x29;}# reduce . 58) (#{$:\x5d;}# 
    reduce . 58)) (($:| shift . 80) ($:, reduce . 56) (#{$:;}# reduce . 56) (
    #\newline reduce . 56) ($:: reduce . 56) (#{$:\x29;}# reduce . 56) (
    #{$:\x5d;}# reduce . 56)) (($:, shift . 2) (#{$:;}# shift . 3) (#\newline 
    shift . 4) (term shift . 79) ($:: shift . 76)) (($:: shift . 76) ($:, 
    shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (term shift . 78)) ((
    $:: shift . 76) ($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) 
    (term shift . 77)) (($lone-comm reduce . 40) ($:, reduce . 40) (#{$:;}# 
    reduce . 40) (#\newline reduce . 40) ($ident reduce . 40) ($:clear reduce 
    . 40) ($:global reduce . 40) ($:return reduce . 40) ($:switch reduce . 40)
    ($:if reduce . 40) ($:while reduce . 40) ($:for reduce . 40) (#{$:\x5b;}#
    reduce . 40) ($:end reduce . 40) ($:else reduce . 40) ($:elseif reduce . 
    40) ($end reduce . 40) ($:function reduce . 40) ($:case reduce . 40) (
    $:otherwise reduce . 40)) (($:, reduce . 46) (#{$:;}# reduce . 46) (
    #\newline reduce . 46) ($ident reduce . 46)) (($:, shift . 2) (#{$:;}# 
    shift . 3) (#\newline shift . 4) (term shift . 74) ($ident shift . 1) (
    ident shift . 75)) (($ident shift . 1) (ident shift . 72) (ident-list 
    shift . 73)) ((#{$:\x28;}# shift . 70) ($:= shift . 71)) (($ident shift . 
    1) (ident shift . 72) (ident-list shift . 166) (#{$:\x29;}# shift . 167)) 
    (($ident shift . 1) (ident shift . 165)) ((#{$:\x5d;}# reduce . 22) ($:, 
    reduce . 22) (#{$:\x29;}# reduce . 22)) (($:, shift . 163) (#{$:\x5d;}# 
    shift . 164)) (($lone-comm reduce . 41) ($:, reduce . 41) (#{$:;}# reduce 
    . 41) (#\newline reduce . 41) ($ident reduce . 41) ($:clear reduce . 41) (
    $:global reduce . 41) ($:return reduce . 41) ($:switch reduce . 41) ($:if 
    reduce . 41) ($:while reduce . 41) ($:for reduce . 41) (#{$:\x5b;}# reduce
    . 41) ($:end reduce . 41) ($:else reduce . 41) ($:elseif reduce . 41) (
    $end reduce . 41) ($:function reduce . 41) ($:case reduce . 41) (
    $:otherwise reduce . 41)) (($:, reduce . 47) (#{$:;}# reduce . 47) (
    #\newline reduce . 47) ($ident reduce . 47)) (($string shift . 42) ($float
    shift . 43) ($fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 
    45) (#{$:\x28;}# shift . 46) (string shift . 47) (number shift . 48) (
    ident shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) ($:~ 
    shift . 51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54) (
    unary-expr shift . 55) (mul-expr shift . 56) (add-expr shift . 57) (
    rel-expr shift . 58) (equality-expr shift . 59) (and-expr shift . 60) (
    or-expr shift . 162)) ((case-list shift . 161) ($:otherwise reduce . 50) (
    $:end reduce . 50) ($:case reduce . 50)) (($ident shift . 1) ($:, shift . 
    2) (#{$:;}# shift . 3) (#\newline shift . 4) (ident shift . 5) ($:clear 
    shift . 6) ($:global shift . 7) ($lone-comm shift . 9) (command shift . 10
    ) ($:return shift . 11) ($:switch shift . 12) ($:if shift . 13) ($:while 
    shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 16) (lval-expr shift 
    . 17) (term shift . 18) (non-comment-statement shift . 20) (lone-comment 
    shift . 21) (statement shift . 122) (statement-list shift . 160)) (($ident
    shift . 1) ($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (
    ident shift . 5) ($:clear shift . 6) ($:global shift . 7) ($lone-comm 
    shift . 9) (command shift . 10) ($:return shift . 11) ($:switch shift . 12
    ) ($:if shift . 13) ($:while shift . 14) ($:for shift . 15) (#{$:\x5b;}# 
    shift . 16) (lval-expr shift . 17) (term shift . 18) (
    non-comment-statement shift . 20) (lone-comment shift . 21) (statement 
    shift . 122) (statement-list shift . 159)) (($string shift . 42) ($float 
    shift . 43) ($fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 45
    ) (#{$:\x28;}# shift . 46) (string shift . 47) (number shift . 48) (ident 
    shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) ($:~ shift . 
    51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54) (
    unary-expr shift . 55) (mul-expr shift . 56) (add-expr shift . 57) (
    rel-expr shift . 58) (equality-expr shift . 59) (and-expr shift . 158)) ((
    $string shift . 42) ($float shift . 43) ($fixed shift . 44) ($ident shift 
    . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47)
    (number shift . 48) (ident shift . 5) (primary-expr shift . 49) (
    lval-expr shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (
    postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr shift . 56) (
    add-expr shift . 57) (rel-expr shift . 58) (equality-expr shift . 157)) ((
    $string shift . 42) ($float shift . 43) ($fixed shift . 44) ($ident shift 
    . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47)
    (number shift . 48) (ident shift . 5) (primary-expr shift . 49) (
    lval-expr shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (
    postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr shift . 56) (
    add-expr shift . 57) (rel-expr shift . 156)) (($string shift . 42) ($float
    shift . 43) ($fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 
    45) (#{$:\x28;}# shift . 46) (string shift . 47) (number shift . 48) (
    ident shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) ($:~ 
    shift . 51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54) (
    unary-expr shift . 55) (mul-expr shift . 56) (add-expr shift . 57) (
    rel-expr shift . 155)) (($string shift . 42) ($float shift . 43) ($fixed 
    shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift
    . 46) (string shift . 47) (number shift . 48) (ident shift . 5) (
    primary-expr shift . 49) (lval-expr shift . 50) ($:~ shift . 51) ($:+ 
    shift . 52) ($:- shift . 53) (postfix-expr shift . 54) (unary-expr shift 
    . 55) (mul-expr shift . 56) (add-expr shift . 154)) (($string shift . 42) 
    ($float shift . 43) ($fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# 
    shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47) (number shift . 
    48) (ident shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) (
    $:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54
    ) (unary-expr shift . 55) (mul-expr shift . 56) (add-expr shift . 153)) ((
    $string shift . 42) ($float shift . 43) ($fixed shift . 44) ($ident shift 
    . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47)
    (number shift . 48) (ident shift . 5) (primary-expr shift . 49) (
    lval-expr shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (
    postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr shift . 56) (
    add-expr shift . 152)) (($string shift . 42) ($float shift . 43) ($fixed 
    shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift
    . 46) (string shift . 47) (number shift . 48) (ident shift . 5) (
    primary-expr shift . 49) (lval-expr shift . 50) ($:~ shift . 51) ($:+ 
    shift . 52) ($:- shift . 53) (postfix-expr shift . 54) (unary-expr shift 
    . 55) (mul-expr shift . 56) (add-expr shift . 151)) (($string shift . 42) 
    ($float shift . 43) ($fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# 
    shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47) (number shift . 
    48) (ident shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) (
    $:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54
    ) (unary-expr shift . 55) (mul-expr shift . 150)) (($string shift . 42) (
    $float shift . 43) ($fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# 
    shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47) (number shift . 
    48) (ident shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) (
    $:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54
    ) (unary-expr shift . 55) (mul-expr shift . 149)) (($string shift . 42) (
    $float shift . 43) ($fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# 
    shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47) (number shift . 
    48) (ident shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) (
    $:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54
    ) (unary-expr shift . 148)) (($string shift . 42) ($float shift . 43) (
    $fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 45) (
    #{$:\x28;}# shift . 46) (string shift . 47) (number shift . 48) (ident 
    shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) ($:~ shift . 
    51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54) (
    unary-expr shift . 147)) (($string shift . 42) ($float shift . 43) ($fixed
    shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# 
    shift . 46) (string shift . 47) (number shift . 48) (ident shift . 5) (
    primary-expr shift . 49) (lval-expr shift . 50) ($:~ shift . 51) ($:+ 
    shift . 52) ($:- shift . 53) (postfix-expr shift . 54) (unary-expr shift 
    . 146)) (($string shift . 42) ($float shift . 43) ($fixed shift . 44) (
    $ident shift . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (
    string shift . 47) (number shift . 48) (ident shift . 5) (primary-expr 
    shift . 49) (lval-expr shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- 
    shift . 53) (postfix-expr shift . 54) (unary-expr shift . 145)) (($string 
    shift . 42) ($float shift . 43) ($fixed shift . 44) ($ident shift . 1) (
    #{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47) (
    number shift . 48) (ident shift . 5) (primary-expr shift . 49) (lval-expr 
    shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (
    postfix-expr shift . 54) (unary-expr shift . 144)) (($string shift . 42) (
    $float shift . 43) ($fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# 
    shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47) (number shift . 
    48) (ident shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) (
    $:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54
    ) (unary-expr shift . 143)) (($string shift . 42) ($float shift . 43) (
    $fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 45) (
    #{$:\x28;}# shift . 46) (string shift . 47) (number shift . 48) (ident 
    shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) ($:~ shift . 
    51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54) (
    unary-expr shift . 142)) (($string shift . 42) ($float shift . 43) ($fixed
    shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# 
    shift . 46) (string shift . 47) (number shift . 48) (ident shift . 5) (
    primary-expr shift . 49) (lval-expr shift . 50) ($:~ shift . 51) ($:+ 
    shift . 52) ($:- shift . 53) (postfix-expr shift . 54) (unary-expr shift 
    . 141)) (($:, reduce . 88) (#{$:;}# reduce . 88) (#\newline reduce . 88) (
    $:: reduce . 88) ($:| reduce . 88) ($:& reduce . 88) ($:== reduce . 88) (
    $:~= reduce . 88) ($:>= reduce . 88) ($:<= reduce . 88) ($:> reduce . 88) 
    ($:< reduce . 88) ($:+ reduce . 88) ($:- reduce . 88) ($:.^ reduce . 88) (
    $:.\ reduce . 88) ($:./ reduce . 88) ($:.* reduce . 88) ($:^ reduce . 88) 
    ($:\ reduce . 88) ($:/ reduce . 88) ($:* reduce . 88) ($:.' reduce . 88) (
    $:' reduce . 88) (#{$:\x29;}# reduce . 88) (#{$:\x5d;}# reduce . 88)) ((
    $:, reduce . 89) (#{$:;}# reduce . 89) (#\newline reduce . 89) ($:: reduce
    . 89) ($:| reduce . 89) ($:& reduce . 89) ($:== reduce . 89) ($:~= reduce
    . 89) ($:>= reduce . 89) ($:<= reduce . 89) ($:> reduce . 89) ($:< reduce
    . 89) ($:+ reduce . 89) ($:- reduce . 89) ($:.^ reduce . 89) ($:.\ reduce
    . 89) ($:./ reduce . 89) ($:.* reduce . 89) ($:^ reduce . 89) ($:\ reduce
    . 89) ($:/ reduce . 89) ($:* reduce . 89) ($:.' reduce . 89) ($:' reduce 
    . 89) (#{$:\x29;}# reduce . 89) (#{$:\x5d;}# reduce . 89)) (($:' shift . 
    98) ($:.' shift . 99) (#\newline reduce . 83) (#{$:;}# reduce . 83) ($:, 
    reduce . 83) ($:* reduce . 83) ($:/ reduce . 83) ($:\ reduce . 83) ($:^ 
    reduce . 83) ($:.* reduce . 83) ($:./ reduce . 83) ($:.\ reduce . 83) (
    $:.^ reduce . 83) ($:- reduce . 83) ($:+ reduce . 83) ($:< reduce . 83) (
    $:> reduce . 83) ($:<= reduce . 83) ($:>= reduce . 83) ($:~= reduce . 83) 
    ($:== reduce . 83) ($:& reduce . 83) ($:| reduce . 83) ($:: reduce . 83) (
    #{$:\x29;}# reduce . 83) (#{$:\x5d;}# reduce . 83)) (($:' shift . 98) (
    $:.' shift . 99) (#\newline reduce . 84) (#{$:;}# reduce . 84) ($:, reduce
    . 84) ($:* reduce . 84) ($:/ reduce . 84) ($:\ reduce . 84) ($:^ reduce 
    . 84) ($:.* reduce . 84) ($:./ reduce . 84) ($:.\ reduce . 84) ($:.^ 
    reduce . 84) ($:- reduce . 84) ($:+ reduce . 84) ($:< reduce . 84) ($:> 
    reduce . 84) ($:<= reduce . 84) ($:>= reduce . 84) ($:~= reduce . 84) (
    $:== reduce . 84) ($:& reduce . 84) ($:| reduce . 84) ($:: reduce . 84) (
    #{$:\x29;}# reduce . 84) (#{$:\x5d;}# reduce . 84)) (($:' shift . 98) (
    $:.' shift . 99) (#\newline reduce . 85) (#{$:;}# reduce . 85) ($:, reduce
    . 85) ($:* reduce . 85) ($:/ reduce . 85) ($:\ reduce . 85) ($:^ reduce 
    . 85) ($:.* reduce . 85) ($:./ reduce . 85) ($:.\ reduce . 85) ($:.^ 
    reduce . 85) ($:- reduce . 85) ($:+ reduce . 85) ($:< reduce . 85) ($:> 
    reduce . 85) ($:<= reduce . 85) ($:>= reduce . 85) ($:~= reduce . 85) (
    $:== reduce . 85) ($:& reduce . 85) ($:| reduce . 85) ($:: reduce . 85) (
    #{$:\x29;}# reduce . 85) (#{$:\x5d;}# reduce . 85)) (($string shift . 42) 
    ($float shift . 43) ($fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# 
    shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47) (number shift . 
    48) (ident shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) (
    $:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54
    ) (unary-expr shift . 55) (mul-expr shift . 56) (add-expr shift . 57) (
    rel-expr shift . 58) (equality-expr shift . 59) (and-expr shift . 60) (
    or-expr shift . 61) ($:: shift . 113) (expr shift . 114) (expr-list shift 
    . 140)) ((#{$:\x29;}# shift . 139) ($:: shift . 76)) ((#\newline reduce . 
    96) (#{$:;}# reduce . 96) ($:, reduce . 96) ($:' reduce . 96) ($:.' reduce
    . 96) ($:* reduce . 96) ($:/ reduce . 96) ($:\ reduce . 96) ($:^ reduce 
    . 96) ($:.* reduce . 96) ($:./ reduce . 96) ($:.\ reduce . 96) ($:.^ 
    reduce . 96) ($:- reduce . 96) ($:+ reduce . 96) ($:< reduce . 96) ($:> 
    reduce . 96) ($:<= reduce . 96) ($:>= reduce . 96) ($:~= reduce . 96) (
    $:== reduce . 96) ($:& reduce . 96) ($:| reduce . 96) ($:: reduce . 96) (
    #{$:\x29;}# reduce . 96) (#{$:\x5d;}# reduce . 96)) (($:: shift . 76) (
    #{$:\x5d;}# reduce . 102) ($:, reduce . 102) (#{$:;}# reduce . 102) (
    #\newline reduce . 102)) (($:, shift . 138) (#{$:\x5d;}# reduce . 98) (
    #\newline reduce . 98) (#{$:;}# reduce . 98)) ((#{$:\x5d;}# shift . 134) (
    #\newline shift . 135) (#{$:;}# shift . 136) (row-term shift . 137)) ((
    $string shift . 42) ($float shift . 43) ($fixed shift . 44) ($ident shift 
    . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47)
    (number shift . 48) (ident shift . 5) (primary-expr shift . 49) (
    lval-expr shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (
    postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr shift . 56) (
    add-expr shift . 57) (rel-expr shift . 58) (equality-expr shift . 59) (
    and-expr shift . 60) (or-expr shift . 61) (expr shift . 133)) (($ident 
    shift . 1) (ident shift . 5) (lval-expr shift . 132)) (($:= shift . 131)) 
    (($:= reduce . 92) (#{$:\x28;}# reduce . 92) ($:. reduce . 92) (
    #{$:\x5d;}# reduce . 92) ($:, reduce . 92) (#\newline reduce . 92) (
    #{$:;}# reduce . 92) ($:' reduce . 92) ($:.' reduce . 92) ($:* reduce . 92
    ) ($:/ reduce . 92) ($:\ reduce . 92) ($:^ reduce . 92) ($:.* reduce . 92)
    ($:./ reduce . 92) ($:.\ reduce . 92) ($:.^ reduce . 92) ($:- reduce . 92
    ) ($:+ reduce . 92) ($:< reduce . 92) ($:> reduce . 92) ($:<= reduce . 92)
    ($:>= reduce . 92) ($:~= reduce . 92) ($:== reduce . 92) ($:& reduce . 92
    ) ($:| reduce . 92) ($:: reduce . 92) (#{$:\x29;}# reduce . 92)) ((
    #{$:\x29;}# reduce . 53) ($:, reduce . 53)) (($:: shift . 76) (#{$:\x29;}#
    reduce . 52) ($:, reduce . 52)) (($:, shift . 129) (#{$:\x29;}# shift . 
    130)) (($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (term 
    shift . 128) ($:: shift . 76)) ((#\newline shift . 127)) (($lone-comm 
    reduce . 106) ($:end reduce . 106) ($:, reduce . 106) (#{$:;}# reduce . 
    106) (#\newline reduce . 106) ($ident reduce . 106) ($:clear reduce . 106)
    ($:global reduce . 106) ($:return reduce . 106) ($:switch reduce . 106) (
    $:if reduce . 106) ($:while reduce . 106) ($:for reduce . 106) (
    #{$:\x5b;}# reduce . 106) ($:function reduce . 106) ($end reduce . 106)) (
    ($:, reduce . 104) (#{$:;}# reduce . 104) (#\newline reduce . 104) (
    $:function reduce . 104) ($end reduce . 104)) (($:, shift . 2) (#{$:;}# 
    shift . 3) (#\newline shift . 4) (term shift . 126) ($:function reduce . 
    11) ($end reduce . 11)) (($:function reduce . 8) ($end reduce . 8)) ((
    $:end reduce . 24) ($lone-comm reduce . 24) ($:, reduce . 24) (#{$:;}# 
    reduce . 24) (#\newline reduce . 24) ($ident reduce . 24) ($:clear reduce 
    . 24) ($:global reduce . 24) ($:return reduce . 24) ($:switch reduce . 24)
    ($:if reduce . 24) ($:while reduce . 24) ($:for reduce . 24) (#{$:\x5b;}#
    reduce . 24) ($:else reduce . 24) ($:elseif reduce . 24) ($:function 
    reduce . 24) ($end reduce . 24) ($:case reduce . 24) ($:otherwise reduce 
    . 24)) (($:end shift . 32) (opt-end shift . 124) ($ident shift . 1) ($:, 
    shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (ident shift . 5) (
    $:clear shift . 6) ($:global shift . 7) ($lone-comm shift . 9) (command 
    shift . 10) ($:return shift . 11) ($:switch shift . 12) ($:if shift . 13) 
    ($:while shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 16) (
    lval-expr shift . 17) (term shift . 18) (non-comment-statement shift . 20)
    (lone-comment shift . 21) (statement shift . 125) ($end reduce . 10) (
    $:function reduce . 10)) (($:function reduce . 7) ($end reduce . 7)) ((
    $:end reduce . 25) ($lone-comm reduce . 25) ($:, reduce . 25) (#{$:;}# 
    reduce . 25) (#\newline reduce . 25) ($ident reduce . 25) ($:clear reduce 
    . 25) ($:global reduce . 25) ($:return reduce . 25) ($:switch reduce . 25)
    ($:if reduce . 25) ($:while reduce . 25) ($:for reduce . 25) (#{$:\x5b;}#
    reduce . 25) ($:function reduce . 25) ($end reduce . 25) ($:else reduce 
    . 25) ($:elseif reduce . 25) ($:case reduce . 25) ($:otherwise reduce . 25
    )) (($:, reduce . 105) (#{$:;}# reduce . 105) (#\newline reduce . 105) (
    $:function reduce . 105) ($end reduce . 105)) (($lone-comm reduce . 107) (
    $:end reduce . 107) ($:, reduce . 107) (#{$:;}# reduce . 107) (#\newline 
    reduce . 107) ($ident reduce . 107) ($:clear reduce . 107) ($:global 
    reduce . 107) ($:return reduce . 107) ($:switch reduce . 107) ($:if reduce
    . 107) ($:while reduce . 107) ($:for reduce . 107) (#{$:\x5b;}# reduce . 
    107) ($:function reduce . 107) ($end reduce . 107)) (($lone-comm reduce . 
    30) ($:, reduce . 30) (#{$:;}# reduce . 30) (#\newline reduce . 30) (
    $ident reduce . 30) ($:clear reduce . 30) ($:global reduce . 30) ($:return
    reduce . 30) ($:switch reduce . 30) ($:if reduce . 30) ($:while reduce . 
    30) ($:for reduce . 30) (#{$:\x5b;}# reduce . 30) ($:end reduce . 30) (
    $:else reduce . 30) ($:elseif reduce . 30) ($end reduce . 30) ($:function 
    reduce . 30) ($:case reduce . 30) ($:otherwise reduce . 30)) (($:: shift 
    . 187) ($string shift . 42) ($float shift . 43) ($fixed shift . 44) (
    $ident shift . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (
    string shift . 47) (number shift . 48) (ident shift . 5) (primary-expr 
    shift . 49) (lval-expr shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- 
    shift . 53) (postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr 
    shift . 56) (add-expr shift . 57) (rel-expr shift . 58) (equality-expr 
    shift . 59) (and-expr shift . 60) (or-expr shift . 61) (expr shift . 188))
    (($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (term shift . 
    186) ($:= reduce . 91) (#{$:\x28;}# reduce . 91) ($:. reduce . 91)) ((
    $ident shift . 1) (ident shift . 185)) ((#{$:\x28;}# shift . 103) ($:. 
    shift . 38) (#{$:\x5d;}# reduce . 43) ($:, reduce . 43)) (($:, shift . 2) 
    (#{$:;}# shift . 3) (#\newline shift . 4) (term shift . 184) ($:: shift . 
    76)) ((#\newline reduce . 97) (#{$:;}# reduce . 97) ($:, reduce . 97) ($:'
    reduce . 97) ($:.' reduce . 97) ($:* reduce . 97) ($:/ reduce . 97) ($:\ 
    reduce . 97) ($:^ reduce . 97) ($:.* reduce . 97) ($:./ reduce . 97) ($:.\
    reduce . 97) ($:.^ reduce . 97) ($:- reduce . 97) ($:+ reduce . 97) ($:< 
    reduce . 97) ($:> reduce . 97) ($:<= reduce . 97) ($:>= reduce . 97) ($:~=
    reduce . 97) ($:== reduce . 97) ($:& reduce . 97) ($:| reduce . 97) ($:: 
    reduce . 97) (#{$:\x29;}# reduce . 97) (#{$:\x5d;}# reduce . 97)) (($ident
    reduce . 101) ($float reduce . 101) ($fixed reduce . 101) ($string reduce
    . 101) (#{$:\x5b;}# reduce . 101) (#{$:\x28;}# reduce . 101) ($:~ reduce 
    . 101) ($:+ reduce . 101) ($:- reduce . 101)) (($ident reduce . 100) (
    $float reduce . 100) ($fixed reduce . 100) ($string reduce . 100) (
    #{$:\x5b;}# reduce . 100) (#{$:\x28;}# reduce . 100) ($:~ reduce . 100) (
    $:+ reduce . 100) ($:- reduce . 100)) (($string shift . 42) ($float shift 
    . 43) ($fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 45) (
    #{$:\x28;}# shift . 46) (string shift . 47) (number shift . 48) (ident 
    shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) ($:~ shift . 
    51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54) (
    unary-expr shift . 55) (mul-expr shift . 56) (add-expr shift . 57) (
    rel-expr shift . 58) (equality-expr shift . 59) (and-expr shift . 60) (
    or-expr shift . 61) (expr shift . 106) (matrix-row shift . 183)) (($string
    shift . 42) ($float shift . 43) ($fixed shift . 44) ($ident shift . 1) (
    #{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47) (
    number shift . 48) (ident shift . 5) (primary-expr shift . 49) (lval-expr 
    shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (
    postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr shift . 56) (
    add-expr shift . 57) (rel-expr shift . 58) (equality-expr shift . 59) (
    and-expr shift . 60) (or-expr shift . 61) (expr shift . 182)) ((#\newline 
    reduce . 95) (#{$:;}# reduce . 95) ($:, reduce . 95) ($:' reduce . 95) (
    $:.' reduce . 95) ($:* reduce . 95) ($:/ reduce . 95) ($:\ reduce . 95) (
    $:^ reduce . 95) ($:.* reduce . 95) ($:./ reduce . 95) ($:.\ reduce . 95) 
    ($:.^ reduce . 95) ($:- reduce . 95) ($:+ reduce . 95) ($:< reduce . 95) (
    $:> reduce . 95) ($:<= reduce . 95) ($:>= reduce . 95) ($:~= reduce . 95) 
    ($:== reduce . 95) ($:& reduce . 95) ($:| reduce . 95) ($:: reduce . 95) (
    #{$:\x29;}# reduce . 95) (#{$:\x5d;}# reduce . 95)) ((#{$:\x29;}# shift . 
    181) ($:, shift . 129)) (($:, reduce . 81) (#{$:;}# reduce . 81) (
    #\newline reduce . 81) ($:: reduce . 81) ($:| reduce . 81) ($:& reduce . 
    81) ($:== reduce . 81) ($:~= reduce . 81) ($:>= reduce . 81) ($:<= reduce 
    . 81) ($:> reduce . 81) ($:< reduce . 81) ($:+ reduce . 81) ($:- reduce . 
    81) ($:.^ reduce . 81) ($:.\ reduce . 81) ($:./ reduce . 81) ($:.* reduce 
    . 81) ($:^ reduce . 81) ($:\ reduce . 81) ($:/ reduce . 81) ($:* reduce . 
    81) (#{$:\x29;}# reduce . 81) (#{$:\x5d;}# reduce . 81)) (($:, reduce . 80
    ) (#{$:;}# reduce . 80) (#\newline reduce . 80) ($:: reduce . 80) ($:| 
    reduce . 80) ($:& reduce . 80) ($:== reduce . 80) ($:~= reduce . 80) ($:>=
    reduce . 80) ($:<= reduce . 80) ($:> reduce . 80) ($:< reduce . 80) ($:+ 
    reduce . 80) ($:- reduce . 80) ($:.^ reduce . 80) ($:.\ reduce . 80) ($:./
    reduce . 80) ($:.* reduce . 80) ($:^ reduce . 80) ($:\ reduce . 80) ($:/ 
    reduce . 80) ($:* reduce . 80) (#{$:\x29;}# reduce . 80) (#{$:\x5d;}# 
    reduce . 80)) (($:, reduce . 79) (#{$:;}# reduce . 79) (#\newline reduce 
    . 79) ($:: reduce . 79) ($:| reduce . 79) ($:& reduce . 79) ($:== reduce 
    . 79) ($:~= reduce . 79) ($:>= reduce . 79) ($:<= reduce . 79) ($:> reduce
    . 79) ($:< reduce . 79) ($:+ reduce . 79) ($:- reduce . 79) ($:.^ reduce 
    . 79) ($:.\ reduce . 79) ($:./ reduce . 79) ($:.* reduce . 79) ($:^ reduce
    . 79) ($:\ reduce . 79) ($:/ reduce . 79) ($:* reduce . 79) (#{$:\x29;}# 
    reduce . 79) (#{$:\x5d;}# reduce . 79)) (($:, reduce . 78) (#{$:;}# reduce
    . 78) (#\newline reduce . 78) ($:: reduce . 78) ($:| reduce . 78) ($:& 
    reduce . 78) ($:== reduce . 78) ($:~= reduce . 78) ($:>= reduce . 78) (
    $:<= reduce . 78) ($:> reduce . 78) ($:< reduce . 78) ($:+ reduce . 78) (
    $:- reduce . 78) ($:.^ reduce . 78) ($:.\ reduce . 78) ($:./ reduce . 78) 
    ($:.* reduce . 78) ($:^ reduce . 78) ($:\ reduce . 78) ($:/ reduce . 78) (
    $:* reduce . 78) (#{$:\x29;}# reduce . 78) (#{$:\x5d;}# reduce . 78)) ((
    $:, reduce . 77) (#{$:;}# reduce . 77) (#\newline reduce . 77) ($:: reduce
    . 77) ($:| reduce . 77) ($:& reduce . 77) ($:== reduce . 77) ($:~= reduce
    . 77) ($:>= reduce . 77) ($:<= reduce . 77) ($:> reduce . 77) ($:< reduce
    . 77) ($:+ reduce . 77) ($:- reduce . 77) ($:.^ reduce . 77) ($:.\ reduce
    . 77) ($:./ reduce . 77) ($:.* reduce . 77) ($:^ reduce . 77) ($:\ reduce
    . 77) ($:/ reduce . 77) ($:* reduce . 77) (#{$:\x29;}# reduce . 77) (
    #{$:\x5d;}# reduce . 77)) (($:, reduce . 76) (#{$:;}# reduce . 76) (
    #\newline reduce . 76) ($:: reduce . 76) ($:| reduce . 76) ($:& reduce . 
    76) ($:== reduce . 76) ($:~= reduce . 76) ($:>= reduce . 76) ($:<= reduce 
    . 76) ($:> reduce . 76) ($:< reduce . 76) ($:+ reduce . 76) ($:- reduce . 
    76) ($:.^ reduce . 76) ($:.\ reduce . 76) ($:./ reduce . 76) ($:.* reduce 
    . 76) ($:^ reduce . 76) ($:\ reduce . 76) ($:/ reduce . 76) ($:* reduce . 
    76) (#{$:\x29;}# reduce . 76) (#{$:\x5d;}# reduce . 76)) (($:, reduce . 75
    ) (#{$:;}# reduce . 75) (#\newline reduce . 75) ($:: reduce . 75) ($:| 
    reduce . 75) ($:& reduce . 75) ($:== reduce . 75) ($:~= reduce . 75) ($:>=
    reduce . 75) ($:<= reduce . 75) ($:> reduce . 75) ($:< reduce . 75) ($:+ 
    reduce . 75) ($:- reduce . 75) ($:.^ reduce . 75) ($:.\ reduce . 75) ($:./
    reduce . 75) ($:.* reduce . 75) ($:^ reduce . 75) ($:\ reduce . 75) ($:/ 
    reduce . 75) ($:* reduce . 75) (#{$:\x29;}# reduce . 75) (#{$:\x5d;}# 
    reduce . 75)) (($:, reduce . 74) (#{$:;}# reduce . 74) (#\newline reduce 
    . 74) ($:: reduce . 74) ($:| reduce . 74) ($:& reduce . 74) ($:== reduce 
    . 74) ($:~= reduce . 74) ($:>= reduce . 74) ($:<= reduce . 74) ($:> reduce
    . 74) ($:< reduce . 74) ($:+ reduce . 74) ($:- reduce . 74) ($:.^ reduce 
    . 74) ($:.\ reduce . 74) ($:./ reduce . 74) ($:.* reduce . 74) ($:^ reduce
    . 74) ($:\ reduce . 74) ($:/ reduce . 74) ($:* reduce . 74) (#{$:\x29;}# 
    reduce . 74) (#{$:\x5d;}# reduce . 74)) (($:* shift . 90) ($:/ shift . 91)
    ($:\ shift . 92) ($:^ shift . 93) ($:.* shift . 94) ($:./ shift . 95) (
    $:.\ shift . 96) ($:.^ shift . 97) (#\newline reduce . 72) (#{$:;}# reduce
    . 72) ($:, reduce . 72) ($:- reduce . 72) ($:+ reduce . 72) ($:< reduce 
    . 72) ($:> reduce . 72) ($:<= reduce . 72) ($:>= reduce . 72) ($:~= reduce
    . 72) ($:== reduce . 72) ($:& reduce . 72) ($:| reduce . 72) ($:: reduce 
    . 72) (#{$:\x29;}# reduce . 72) (#{$:\x5d;}# reduce . 72)) (($:* shift . 
    90) ($:/ shift . 91) ($:\ shift . 92) ($:^ shift . 93) ($:.* shift . 94) (
    $:./ shift . 95) ($:.\ shift . 96) ($:.^ shift . 97) (#\newline reduce . 
    71) (#{$:;}# reduce . 71) ($:, reduce . 71) ($:- reduce . 71) ($:+ reduce 
    . 71) ($:< reduce . 71) ($:> reduce . 71) ($:<= reduce . 71) ($:>= reduce 
    . 71) ($:~= reduce . 71) ($:== reduce . 71) ($:& reduce . 71) ($:| reduce 
    . 71) ($:: reduce . 71) (#{$:\x29;}# reduce . 71) (#{$:\x5d;}# reduce . 71
    )) (($:+ shift . 88) ($:- shift . 89) ($:, reduce . 69) (#{$:;}# reduce . 
    69) (#\newline reduce . 69) ($:: reduce . 69) ($:| reduce . 69) ($:& 
    reduce . 69) ($:== reduce . 69) ($:~= reduce . 69) ($:>= reduce . 69) (
    $:<= reduce . 69) ($:> reduce . 69) ($:< reduce . 69) (#{$:\x29;}# reduce 
    . 69) (#{$:\x5d;}# reduce . 69)) (($:+ shift . 88) ($:- shift . 89) ($:, 
    reduce . 68) (#{$:;}# reduce . 68) (#\newline reduce . 68) ($:: reduce . 
    68) ($:| reduce . 68) ($:& reduce . 68) ($:== reduce . 68) ($:~= reduce . 
    68) ($:>= reduce . 68) ($:<= reduce . 68) ($:> reduce . 68) ($:< reduce . 
    68) (#{$:\x29;}# reduce . 68) (#{$:\x5d;}# reduce . 68)) (($:+ shift . 88)
    ($:- shift . 89) ($:, reduce . 67) (#{$:;}# reduce . 67) (#\newline 
    reduce . 67) ($:: reduce . 67) ($:| reduce . 67) ($:& reduce . 67) ($:== 
    reduce . 67) ($:~= reduce . 67) ($:>= reduce . 67) ($:<= reduce . 67) ($:>
    reduce . 67) ($:< reduce . 67) (#{$:\x29;}# reduce . 67) (#{$:\x5d;}# 
    reduce . 67)) (($:+ shift . 88) ($:- shift . 89) ($:, reduce . 66) (
    #{$:;}# reduce . 66) (#\newline reduce . 66) ($:: reduce . 66) ($:| reduce
    . 66) ($:& reduce . 66) ($:== reduce . 66) ($:~= reduce . 66) ($:>= 
    reduce . 66) ($:<= reduce . 66) ($:> reduce . 66) ($:< reduce . 66) (
    #{$:\x29;}# reduce . 66) (#{$:\x5d;}# reduce . 66)) (($:< shift . 84) ($:>
    shift . 85) ($:<= shift . 86) ($:>= shift . 87) (#\newline reduce . 64) (
    #{$:;}# reduce . 64) ($:, reduce . 64) ($:~= reduce . 64) ($:== reduce . 
    64) ($:& reduce . 64) ($:| reduce . 64) ($:: reduce . 64) (#{$:\x29;}# 
    reduce . 64) (#{$:\x5d;}# reduce . 64)) (($:< shift . 84) ($:> shift . 85)
    ($:<= shift . 86) ($:>= shift . 87) (#\newline reduce . 63) (#{$:;}# 
    reduce . 63) ($:, reduce . 63) ($:~= reduce . 63) ($:== reduce . 63) ($:& 
    reduce . 63) ($:| reduce . 63) ($:: reduce . 63) (#{$:\x29;}# reduce . 63)
    (#{$:\x5d;}# reduce . 63)) (($:== shift . 82) ($:~= shift . 83) ($:, 
    reduce . 61) (#{$:;}# reduce . 61) (#\newline reduce . 61) ($:: reduce . 
    61) ($:| reduce . 61) ($:& reduce . 61) (#{$:\x29;}# reduce . 61) (
    #{$:\x5d;}# reduce . 61)) (($:& shift . 81) (#\newline reduce . 59) (
    #{$:;}# reduce . 59) ($:, reduce . 59) ($:| reduce . 59) ($:: reduce . 59)
    (#{$:\x29;}# reduce . 59) (#{$:\x5d;}# reduce . 59)) (($:end shift . 180)
    ($ident shift . 1) ($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift 
    . 4) (ident shift . 5) ($:clear shift . 6) ($:global shift . 7) (
    $lone-comm shift . 9) (command shift . 10) ($:return shift . 11) ($:switch
    shift . 12) ($:if shift . 13) ($:while shift . 14) ($:for shift . 15) (
    #{$:\x5b;}# shift . 16) (lval-expr shift . 17) (term shift . 18) (
    non-comment-statement shift . 20) (lone-comment shift . 21) (statement 
    shift . 125)) (($:end shift . 176) ($ident shift . 1) ($:, shift . 2) (
    #{$:;}# shift . 3) (#\newline shift . 4) (ident shift . 5) ($:clear shift 
    . 6) ($:global shift . 7) ($lone-comm shift . 9) (command shift . 10) (
    $:return shift . 11) ($:switch shift . 12) ($:if shift . 13) ($:while 
    shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 16) (lval-expr shift 
    . 17) (term shift . 18) (non-comment-statement shift . 20) (lone-comment 
    shift . 21) (statement shift . 125) ($:else shift . 177) ($:elseif shift 
    . 178) (elseif-list shift . 179)) (($:end shift . 173) ($:case shift . 174
    ) ($:otherwise shift . 175)) (($:| shift . 80) ($:, reduce . 57) (#{$:;}# 
    reduce . 57) (#\newline reduce . 57) ($:: reduce . 57) (#{$:\x29;}# reduce
    . 57) (#{$:\x5d;}# reduce . 57)) (($ident shift . 1) (ident shift . 172))
    (($:= shift . 171)) ((#{$:\x28;}# shift . 170)) ((#{$:\x29;}# shift . 169
    ) ($:, shift . 163)) (($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift
    . 4) (term shift . 168)) ((#{$:\x5b;}# reduce . 21) ($:for reduce . 21) (
    $:while reduce . 21) ($:if reduce . 21) ($:switch reduce . 21) ($:return 
    reduce . 21) ($:global reduce . 21) ($:clear reduce . 21) ($ident reduce 
    . 21) (#\newline reduce . 21) (#{$:;}# reduce . 21) ($:, reduce . 21) (
    $:end reduce . 21) ($lone-comm reduce . 21) ($:function reduce . 21) ($end
    reduce . 21)) (($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) 
    (term shift . 203)) (($ident shift . 1) (ident shift . 72) (ident-list 
    shift . 201) (#{$:\x29;}# shift . 202)) (($ident shift . 1) (ident shift 
    . 200)) ((#{$:\x5d;}# reduce . 23) ($:, reduce . 23) (#{$:\x29;}# reduce 
    . 23)) (($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (term 
    shift . 199)) (($string shift . 42) ($float shift . 43) ($fixed shift . 44
    ) ($ident shift . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (
    string shift . 47) (number shift . 48) (ident shift . 5) (primary-expr 
    shift . 49) (lval-expr shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- 
    shift . 53) (postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr 
    shift . 56) (add-expr shift . 57) (rel-expr shift . 58) (equality-expr 
    shift . 59) (and-expr shift . 60) (or-expr shift . 61) (expr shift . 198))
    (($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (term shift . 
    197)) (($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (term 
    shift . 196)) (($ident shift . 1) ($:, shift . 2) (#{$:;}# shift . 3) (
    #\newline shift . 4) (ident shift . 5) ($:clear shift . 6) ($:global shift
    . 7) ($lone-comm shift . 9) (command shift . 10) ($:return shift . 11) (
    $:switch shift . 12) ($:if shift . 13) ($:while shift . 14) ($:for shift 
    . 15) (#{$:\x5b;}# shift . 16) (lval-expr shift . 17) (term shift . 18) (
    non-comment-statement shift . 20) (lone-comment shift . 21) (statement 
    shift . 122) (statement-list shift . 195)) (($string shift . 42) ($float 
    shift . 43) ($fixed shift . 44) ($ident shift . 1) (#{$:\x5b;}# shift . 45
    ) (#{$:\x28;}# shift . 46) (string shift . 47) (number shift . 48) (ident 
    shift . 5) (primary-expr shift . 49) (lval-expr shift . 50) ($:~ shift . 
    51) ($:+ shift . 52) ($:- shift . 53) (postfix-expr shift . 54) (
    unary-expr shift . 55) (mul-expr shift . 56) (add-expr shift . 57) (
    rel-expr shift . 58) (equality-expr shift . 59) (and-expr shift . 60) (
    or-expr shift . 61) (expr shift . 194)) (($:else shift . 192) ($:elseif 
    shift . 193)) (($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (
    term shift . 191)) ((#{$:\x5d;}# reduce . 91) ($:. reduce . 91) (
    #{$:\x28;}# reduce . 91) ($:, reduce . 91) (#\newline reduce . 91) (
    #{$:;}# reduce . 91) ($:' reduce . 91) ($:.' reduce . 91) ($:* reduce . 91
    ) ($:/ reduce . 91) ($:\ reduce . 91) ($:^ reduce . 91) ($:.* reduce . 91)
    ($:./ reduce . 91) ($:.\ reduce . 91) ($:.^ reduce . 91) ($:- reduce . 91
    ) ($:+ reduce . 91) ($:< reduce . 91) ($:> reduce . 91) ($:<= reduce . 91)
    ($:>= reduce . 91) ($:~= reduce . 91) ($:== reduce . 91) ($:& reduce . 91
    ) ($:| reduce . 91) ($:: reduce . 91) (#{$:\x29;}# reduce . 91)) (($:: 
    shift . 76) (#{$:\x5d;}# reduce . 103) ($:, reduce . 103) (#{$:;}# reduce 
    . 103) (#\newline reduce . 103)) (($:, shift . 138) (#{$:\x5d;}# reduce . 
    99) (#\newline reduce . 99) (#{$:;}# reduce . 99)) (($ident shift . 1) (
    $:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (ident shift . 5)
    ($:clear shift . 6) ($:global shift . 7) ($lone-comm shift . 9) (command 
    shift . 10) ($:return shift . 11) ($:switch shift . 12) ($:if shift . 13) 
    ($:while shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 16) (
    lval-expr shift . 17) (term shift . 18) (non-comment-statement shift . 20)
    (lone-comment shift . 21) (statement shift . 122) (statement-list shift 
    . 190)) ((#{$:\x28;}# shift . 189)) (($lone-comm reduce . 29) ($:, reduce 
    . 29) (#{$:;}# reduce . 29) (#\newline reduce . 29) ($ident reduce . 29) (
    $:clear reduce . 29) ($:global reduce . 29) ($:return reduce . 29) (
    $:switch reduce . 29) ($:if reduce . 29) ($:while reduce . 29) ($:for 
    reduce . 29) (#{$:\x5b;}# reduce . 29) ($:end reduce . 29) ($:else reduce 
    . 29) ($:elseif reduce . 29) ($end reduce . 29) ($:function reduce . 29) (
    $:case reduce . 29) ($:otherwise reduce . 29)) ((#{$:\x29;}# reduce . 55) 
    ($:, reduce . 55)) (($:: shift . 76) (#{$:\x29;}# reduce . 54) ($:, reduce
    . 54)) (($string shift . 42) ($float shift . 43) ($fixed shift . 44) (
    $ident shift . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (
    string shift . 47) (number shift . 48) (ident shift . 5) (primary-expr 
    shift . 49) (lval-expr shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- 
    shift . 53) (postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr 
    shift . 56) (add-expr shift . 57) (rel-expr shift . 58) (equality-expr 
    shift . 59) (and-expr shift . 60) (or-expr shift . 61) ($:: shift . 113) (
    expr shift . 114) (expr-list shift . 214) (#{$:\x29;}# shift . 215)) ((
    $:end shift . 213) ($ident shift . 1) ($:, shift . 2) (#{$:;}# shift . 3) 
    (#\newline shift . 4) (ident shift . 5) ($:clear shift . 6) ($:global 
    shift . 7) ($lone-comm shift . 9) (command shift . 10) ($:return shift . 
    11) ($:switch shift . 12) ($:if shift . 13) ($:while shift . 14) ($:for 
    shift . 15) (#{$:\x5b;}# shift . 16) (lval-expr shift . 17) (term shift . 
    18) (non-comment-statement shift . 20) (lone-comment shift . 21) (
    statement shift . 125)) (($lone-comm reduce . 34) ($:, reduce . 34) (
    #{$:;}# reduce . 34) (#\newline reduce . 34) ($ident reduce . 34) ($:clear
    reduce . 34) ($:global reduce . 34) ($:return reduce . 34) ($:switch 
    reduce . 34) ($:if reduce . 34) ($:while reduce . 34) ($:for reduce . 34) 
    (#{$:\x5b;}# reduce . 34) ($:end reduce . 34) ($:else reduce . 34) (
    $:elseif reduce . 34) ($end reduce . 34) ($:function reduce . 34) ($:case 
    reduce . 34) ($:otherwise reduce . 34)) (($ident shift . 1) ($:, shift . 2
    ) (#{$:;}# shift . 3) (#\newline shift . 4) (ident shift . 5) ($:clear 
    shift . 6) ($:global shift . 7) ($lone-comm shift . 9) (command shift . 10
    ) ($:return shift . 11) ($:switch shift . 12) ($:if shift . 13) ($:while 
    shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 16) (lval-expr shift 
    . 17) (term shift . 18) (non-comment-statement shift . 20) (lone-comment 
    shift . 21) (statement shift . 122) (statement-list shift . 212)) ((
    $string shift . 42) ($float shift . 43) ($fixed shift . 44) ($ident shift 
    . 1) (#{$:\x5b;}# shift . 45) (#{$:\x28;}# shift . 46) (string shift . 47)
    (number shift . 48) (ident shift . 5) (primary-expr shift . 49) (
    lval-expr shift . 50) ($:~ shift . 51) ($:+ shift . 52) ($:- shift . 53) (
    postfix-expr shift . 54) (unary-expr shift . 55) (mul-expr shift . 56) (
    add-expr shift . 57) (rel-expr shift . 58) (equality-expr shift . 59) (
    and-expr shift . 60) (or-expr shift . 61) (expr shift . 211)) (($:, shift 
    . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (term shift . 210) ($:: 
    shift . 76)) (($:end shift . 209) ($ident shift . 1) ($:, shift . 2) (
    #{$:;}# shift . 3) (#\newline shift . 4) (ident shift . 5) ($:clear shift 
    . 6) ($:global shift . 7) ($lone-comm shift . 9) (command shift . 10) (
    $:return shift . 11) ($:switch shift . 12) ($:if shift . 13) ($:while 
    shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 16) (lval-expr shift 
    . 17) (term shift . 18) (non-comment-statement shift . 20) (lone-comment 
    shift . 21) (statement shift . 125)) (($lone-comm reduce . 37) ($:, reduce
    . 37) (#{$:;}# reduce . 37) (#\newline reduce . 37) ($ident reduce . 37) 
    ($:clear reduce . 37) ($:global reduce . 37) ($:return reduce . 37) (
    $:switch reduce . 37) ($:if reduce . 37) ($:while reduce . 37) ($:for 
    reduce . 37) (#{$:\x5b;}# reduce . 37) ($:end reduce . 37) ($:else reduce 
    . 37) ($:elseif reduce . 37) ($end reduce . 37) ($:function reduce . 37) (
    $:case reduce . 37) ($:otherwise reduce . 37)) (($ident shift . 1) ($:, 
    shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (ident shift . 5) (
    $:clear shift . 6) ($:global shift . 7) ($lone-comm shift . 9) (command 
    shift . 10) ($:return shift . 11) ($:switch shift . 12) ($:if shift . 13) 
    ($:while shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 16) (
    lval-expr shift . 17) (term shift . 18) (non-comment-statement shift . 20)
    (lone-comment shift . 21) (statement shift . 122) (statement-list shift 
    . 208)) (($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (term 
    shift . 207) ($:: shift . 76)) (($lone-comm reduce . 39) ($:, reduce . 39)
    (#{$:;}# reduce . 39) (#\newline reduce . 39) ($ident reduce . 39) (
    $:clear reduce . 39) ($:global reduce . 39) ($:return reduce . 39) (
    $:switch reduce . 39) ($:if reduce . 39) ($:while reduce . 39) ($:for 
    reduce . 39) (#{$:\x5b;}# reduce . 39) ($:end reduce . 39) ($:else reduce 
    . 39) ($:elseif reduce . 39) ($end reduce . 39) ($:function reduce . 39) (
    $:case reduce . 39) ($:otherwise reduce . 39)) ((#{$:\x28;}# shift . 206))
    ((#{$:\x29;}# shift . 205) ($:, shift . 163)) (($:, shift . 2) (#{$:;}# 
    shift . 3) (#\newline shift . 4) (term shift . 204)) ((#{$:\x5b;}# reduce 
    . 20) ($:for reduce . 20) ($:while reduce . 20) ($:if reduce . 20) (
    $:switch reduce . 20) ($:return reduce . 20) ($:global reduce . 20) (
    $:clear reduce . 20) ($ident reduce . 20) (#\newline reduce . 20) (#{$:;}#
    reduce . 20) ($:, reduce . 20) ($:end reduce . 20) ($lone-comm reduce . 
    20) ($:function reduce . 20) ($end reduce . 20)) ((#{$:\x5b;}# reduce . 19
    ) ($:for reduce . 19) ($:while reduce . 19) ($:if reduce . 19) ($:switch 
    reduce . 19) ($:return reduce . 19) ($:global reduce . 19) ($:clear reduce
    . 19) ($ident reduce . 19) (#\newline reduce . 19) (#{$:;}# reduce . 19) 
    ($:, reduce . 19) ($:end reduce . 19) ($lone-comm reduce . 19) ($:function
    reduce . 19) ($end reduce . 19)) (($:, shift . 2) (#{$:;}# shift . 3) (
    #\newline shift . 4) (term shift . 227)) (($ident shift . 1) (ident shift 
    . 72) (ident-list shift . 225) (#{$:\x29;}# shift . 226)) (($ident shift 
    . 1) ($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (ident 
    shift . 5) ($:clear shift . 6) ($:global shift . 7) ($lone-comm shift . 9)
    (command shift . 10) ($:return shift . 11) ($:switch shift . 12) ($:if 
    shift . 13) ($:while shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 
    16) (lval-expr shift . 17) (term shift . 18) (non-comment-statement shift 
    . 20) (lone-comment shift . 21) (statement shift . 122) (statement-list 
    shift . 224)) (($:end shift . 223) ($ident shift . 1) ($:, shift . 2) (
    #{$:;}# shift . 3) (#\newline shift . 4) (ident shift . 5) ($:clear shift 
    . 6) ($:global shift . 7) ($lone-comm shift . 9) (command shift . 10) (
    $:return shift . 11) ($:switch shift . 12) ($:if shift . 13) ($:while 
    shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 16) (lval-expr shift 
    . 17) (term shift . 18) (non-comment-statement shift . 20) (lone-comment 
    shift . 21) (statement shift . 125)) (($:, shift . 2) (#{$:;}# shift . 3) 
    (#\newline shift . 4) (term shift . 222)) (($ident shift . 1) ($:, shift 
    . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (ident shift . 5) ($:clear 
    shift . 6) ($:global shift . 7) ($lone-comm shift . 9) (command shift . 10
    ) ($:return shift . 11) ($:switch shift . 12) ($:if shift . 13) ($:while 
    shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 16) (lval-expr shift 
    . 17) (term shift . 18) (non-comment-statement shift . 20) (lone-comment 
    shift . 21) (statement shift . 122) (statement-list shift . 221)) (($:, 
    shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (term shift . 220) (
    $:: shift . 76)) (($:end shift . 219) ($ident shift . 1) ($:, shift . 2) (
    #{$:;}# shift . 3) (#\newline shift . 4) (ident shift . 5) ($:clear shift 
    . 6) ($:global shift . 7) ($lone-comm shift . 9) (command shift . 10) (
    $:return shift . 11) ($:switch shift . 12) ($:if shift . 13) ($:while 
    shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 16) (lval-expr shift 
    . 17) (term shift . 18) (non-comment-statement shift . 20) (lone-comment 
    shift . 21) (statement shift . 125)) (($:, shift . 2) (#{$:;}# shift . 3) 
    (#\newline shift . 4) (term shift . 218)) ((#{$:\x29;}# shift . 217) ($:, 
    shift . 129)) (($:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (
    term shift . 216)) (($lone-comm reduce . 31) ($:, reduce . 31) (#{$:;}# 
    reduce . 31) (#\newline reduce . 31) ($ident reduce . 31) ($:clear reduce 
    . 31) ($:global reduce . 31) ($:return reduce . 31) ($:switch reduce . 31)
    ($:if reduce . 31) ($:while reduce . 31) ($:for reduce . 31) (#{$:\x5b;}#
    reduce . 31) ($:end reduce . 31) ($:else reduce . 31) ($:elseif reduce . 
    31) ($end reduce . 31) ($:function reduce . 31) ($:case reduce . 31) (
    $:otherwise reduce . 31)) (($:, shift . 2) (#{$:;}# shift . 3) (#\newline 
    shift . 4) (term shift . 233)) (($lone-comm reduce . 33) ($:, reduce . 33)
    (#{$:;}# reduce . 33) (#\newline reduce . 33) ($ident reduce . 33) (
    $:clear reduce . 33) ($:global reduce . 33) ($:return reduce . 33) (
    $:switch reduce . 33) ($:if reduce . 33) ($:while reduce . 33) ($:for 
    reduce . 33) (#{$:\x5b;}# reduce . 33) ($:end reduce . 33) ($:else reduce 
    . 33) ($:elseif reduce . 33) ($end reduce . 33) ($:function reduce . 33) (
    $:case reduce . 33) ($:otherwise reduce . 33)) (($:, shift . 2) (#{$:;}# 
    shift . 3) (#\newline shift . 4) (term shift . 232)) (($ident shift . 1) (
    $:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (ident shift . 5)
    ($:clear shift . 6) ($:global shift . 7) ($lone-comm shift . 9) (command 
    shift . 10) ($:return shift . 11) ($:switch shift . 12) ($:if shift . 13) 
    ($:while shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 16) (
    lval-expr shift . 17) (term shift . 18) (non-comment-statement shift . 20)
    (lone-comment shift . 21) (statement shift . 122) (statement-list shift 
    . 231)) (($ident shift . 1) ($:, shift . 2) (#{$:;}# shift . 3) (#\newline
    shift . 4) (ident shift . 5) ($:clear shift . 6) ($:global shift . 7) (
    $lone-comm shift . 9) (command shift . 10) ($:return shift . 11) ($:switch
    shift . 12) ($:if shift . 13) ($:while shift . 14) ($:for shift . 15) (
    #{$:\x5b;}# shift . 16) (lval-expr shift . 17) (term shift . 18) (
    non-comment-statement shift . 20) (lone-comment shift . 21) (statement 
    shift . 125) ($:else reduce . 48) ($:elseif reduce . 48)) (($lone-comm 
    reduce . 36) ($:, reduce . 36) (#{$:;}# reduce . 36) (#\newline reduce . 
    36) ($ident reduce . 36) ($:clear reduce . 36) ($:global reduce . 36) (
    $:return reduce . 36) ($:switch reduce . 36) ($:if reduce . 36) ($:while 
    reduce . 36) ($:for reduce . 36) (#{$:\x5b;}# reduce . 36) ($:end reduce 
    . 36) ($:else reduce . 36) ($:elseif reduce . 36) ($end reduce . 36) (
    $:function reduce . 36) ($:case reduce . 36) ($:otherwise reduce . 36)) ((
    $:, shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (term shift . 230
    )) (($ident shift . 1) ($:, shift . 2) (#{$:;}# shift . 3) (#\newline 
    shift . 4) (ident shift . 5) ($:clear shift . 6) ($:global shift . 7) (
    $lone-comm shift . 9) (command shift . 10) ($:return shift . 11) ($:switch
    shift . 12) ($:if shift . 13) ($:while shift . 14) ($:for shift . 15) (
    #{$:\x5b;}# shift . 16) (lval-expr shift . 17) (term shift . 18) (
    non-comment-statement shift . 20) (lone-comment shift . 21) (statement 
    shift . 125) ($:end reduce . 51) ($:case reduce . 51) ($:otherwise reduce 
    . 51)) ((#{$:\x29;}# shift . 229) ($:, shift . 163)) (($:, shift . 2) (
    #{$:;}# shift . 3) (#\newline shift . 4) (term shift . 228)) ((#{$:\x5b;}#
    reduce . 18) ($:for reduce . 18) ($:while reduce . 18) ($:if reduce . 18)
    ($:switch reduce . 18) ($:return reduce . 18) ($:global reduce . 18) (
    $:clear reduce . 18) ($ident reduce . 18) (#\newline reduce . 18) (#{$:;}#
    reduce . 18) ($:, reduce . 18) ($:end reduce . 18) ($lone-comm reduce . 
    18) ($:function reduce . 18) ($end reduce . 18)) ((#{$:\x5b;}# reduce . 17
    ) ($:for reduce . 17) ($:while reduce . 17) ($:if reduce . 17) ($:switch 
    reduce . 17) ($:return reduce . 17) ($:global reduce . 17) ($:clear reduce
    . 17) ($ident reduce . 17) (#\newline reduce . 17) (#{$:;}# reduce . 17) 
    ($:, reduce . 17) ($:end reduce . 17) ($lone-comm reduce . 17) ($:function
    reduce . 17) ($end reduce . 17)) (($:, shift . 2) (#{$:;}# shift . 3) (
    #\newline shift . 4) (term shift . 234)) (($lone-comm reduce . 38) ($:, 
    reduce . 38) (#{$:;}# reduce . 38) (#\newline reduce . 38) ($ident reduce 
    . 38) ($:clear reduce . 38) ($:global reduce . 38) ($:return reduce . 38) 
    ($:switch reduce . 38) ($:if reduce . 38) ($:while reduce . 38) ($:for 
    reduce . 38) (#{$:\x5b;}# reduce . 38) ($:end reduce . 38) ($:else reduce 
    . 38) ($:elseif reduce . 38) ($end reduce . 38) ($:function reduce . 38) (
    $:case reduce . 38) ($:otherwise reduce . 38)) (($ident shift . 1) ($:, 
    shift . 2) (#{$:;}# shift . 3) (#\newline shift . 4) (ident shift . 5) (
    $:clear shift . 6) ($:global shift . 7) ($lone-comm shift . 9) (command 
    shift . 10) ($:return shift . 11) ($:switch shift . 12) ($:if shift . 13) 
    ($:while shift . 14) ($:for shift . 15) (#{$:\x5b;}# shift . 16) (
    lval-expr shift . 17) (term shift . 18) (non-comment-statement shift . 20)
    (lone-comment shift . 21) (statement shift . 125) ($:else reduce . 49) (
    $:elseif reduce . 49)) (($lone-comm reduce . 35) ($:, reduce . 35) (
    #{$:;}# reduce . 35) (#\newline reduce . 35) ($ident reduce . 35) ($:clear
    reduce . 35) ($:global reduce . 35) ($:return reduce . 35) ($:switch 
    reduce . 35) ($:if reduce . 35) ($:while reduce . 35) ($:for reduce . 35) 
    (#{$:\x5b;}# reduce . 35) ($:end reduce . 35) ($:else reduce . 35) (
    $:elseif reduce . 35) ($end reduce . 35) ($:function reduce . 35) ($:case 
    reduce . 35) ($:otherwise reduce . 35)) (($lone-comm reduce . 32) ($:, 
    reduce . 32) (#{$:;}# reduce . 32) (#\newline reduce . 32) ($ident reduce 
    . 32) ($:clear reduce . 32) ($:global reduce . 32) ($:return reduce . 32) 
    ($:switch reduce . 32) ($:if reduce . 32) ($:while reduce . 32) ($:for 
    reduce . 32) (#{$:\x5b;}# reduce . 32) ($:end reduce . 32) ($:else reduce 
    . 32) ($:elseif reduce . 32) ($end reduce . 32) ($:function reduce . 32) (
    $:case reduce . 32) ($:otherwise reduce . 32)) ((#{$:\x5b;}# reduce . 16) 
    ($:for reduce . 16) ($:while reduce . 16) ($:if reduce . 16) ($:switch 
    reduce . 16) ($:return reduce . 16) ($:global reduce . 16) ($:clear reduce
    . 16) ($ident reduce . 16) (#\newline reduce . 16) (#{$:;}# reduce . 16) 
    ($:, reduce . 16) ($:end reduce . 16) ($lone-comm reduce . 16) ($:function
    reduce . 16) ($end reduce . 16))))

(define rto-v
  #($start mfile mfile script-file script-file function-file function-file 
    function-defn function-defn function-defn opt-end opt-end function-decl 
    function-decl nl-list nl-list function-decl-line function-decl-line 
    function-decl-line function-decl-line function-decl-line 
    function-decl-line ident-list ident-list statement-list statement-list 
    statement statement non-comment-statement non-comment-statement 
    non-comment-statement non-comment-statement non-comment-statement 
    non-comment-statement non-comment-statement non-comment-statement 
    non-comment-statement non-comment-statement non-comment-statement 
    non-comment-statement non-comment-statement non-comment-statement 
    lval-expr-list lval-expr-list command command arg-list arg-list 
    elseif-list elseif-list case-list case-list expr-list expr-list expr-list 
    expr-list expr expr or-expr or-expr and-expr and-expr equality-expr 
    equality-expr equality-expr rel-expr rel-expr rel-expr rel-expr rel-expr 
    add-expr add-expr add-expr mul-expr mul-expr mul-expr mul-expr mul-expr 
    mul-expr mul-expr mul-expr mul-expr unary-expr unary-expr unary-expr 
    unary-expr postfix-expr postfix-expr postfix-expr postfix-expr lval-expr 
    lval-expr lval-expr primary-expr primary-expr primary-expr primary-expr 
    primary-expr matrix-row-list matrix-row-list row-term row-term matrix-row 
    matrix-row term-list term-list lone-comment-list lone-comment-list term 
    term term ident number number string lone-comment))

(define mtab
  '(($lone-comm . $lone-comm) ($string . $string) ($float . $float) ($fixed 
    . $fixed) ($ident . $ident) (";" . #{$:;}#) ("." . $:.) (".'" . $:.') ("'"
    . $:') ("~" . $:~) (".^" . $:.^) (".\\" . $:.\) ("./" . $:./) (".*" . 
    $:.*) ("^" . $:^) ("\\" . $:\) ("/" . $:/) ("*" . $:*) ("-" . $:-) ("+" . 
    $:+) (">=" . $:>=) ("<=" . $:<=) (">" . $:>) ("<" . $:<) ("~=" . $:~=) (
    "==" . $:==) ("&" . $:&) ("|" . $:|) (":" . $::) ("case" . $:case) (
    "elseif" . $:elseif) ("clear" . $:clear) ("global" . $:global) ("return" 
    . $:return) ("otherwise" . $:otherwise) ("switch" . $:switch) ("else" . 
    $:else) ("if" . $:if) ("while" . $:while) ("for" . $:for) ("," . $:,) (")"
    . #{$:\x29;}#) ("(" . #{$:\x28;}#) ("=" . $:=) ("]" . #{$:\x5d;}#) ("[" 
    . #{$:\x5b;}#) ("function" . $:function) (#\newline . #\newline) ("end" . 
    $:end) ($end . $end)))

;;; end tables
