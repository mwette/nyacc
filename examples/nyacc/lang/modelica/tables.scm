;; lang/modelica/tables.scm

;; Copyright (C) 2015 Matthew R. Wette
;; 
;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;; or any later version published by the Free Software Foundation.  See the
;; file COPYING included with the this distribution.

(define len-v
  #(1 0 2 1 3 2 3 2 4 3 3 2 2 1 1 1 2 1 1 2 1 1 1 3 3 2 2 2 1 1 1 1 1 5 7 6 
    7 6 6 5 7 0 1 1 9 1 3 0 1 1 3 2 4 3 3 2 1 2 1 2 1 2 1 1 5 4 4 4 3 3 3 2 1 
    6 5 4 3 2 3 1 1 5 4 3 2 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 4 2 1 1 3 3 3 1 1 2 
    3 1 3 4 3 3 2 3 2 4 3 3 2 3 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 3 3 2 2 3 0 1 0 
    1 3 1 2 2 3 2 1 3 1 1 3 2 2 1 1 1 3 0 1 4 0 1 0 1 1 1 1 4 3 4 2 3 2 2 1 3 
    2 2 1 2 3 2 3 1 1 1 1 2 2 3 2 3 2 6 1 1 1 1 1 1 7 6 6 5 2 1 1 2 2 1 2 1 7 
    6 6 5 2 1 1 2 2 1 2 1 6 5 6 5 1 3 3 1 6 5 6 1 2 3 4 6 1 2 3 4 6 1 7 6 4 5 
    1 5 3 1 3 1 3 1 2 1 3 1 1 1 1 1 1 1 3 1 1 1 1 1 3 1 1 1 1 1 3 3 1 1 1 1 2 
    2 1 2 3 3 3 1 3 1 2 3 1 4 0 1 2 3 0 1 0 1 3 2 2 1 2 2 1 3 3 5 4 1 1 1 3 1 
    3 3 1 3 1 1 2 1 0 1 1 3 0 2 2 1 1 1 1))

(define pat-v
  #((($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) (
    $:package shift . 4) ($:type shift . 5) ($:connector shift . 6) (
    $:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (class-prefixes shift 
    . 15) ($:encapsulated shift . 16) (class-definition shift . 17) ($:final 
    shift . 18) ($:within shift . 19) (stored-definition-2 shift . 20) (
    stored-definition-1 shift . 21) (stored-definition shift . 22) ($end 
    reduce . 1)) (($ident reduce . 28) ($:extends reduce . 28)) (($:operator 
    shift . 46) ($:function shift . 47)) (($:operator shift . 44) ($:function 
    shift . 45)) (($ident reduce . 22) ($:extends reduce . 22)) (($ident 
    reduce . 21) ($:extends reduce . 21)) (($ident reduce . 20) ($:extends 
    reduce . 20)) (($:connector shift . 43)) (($ident reduce . 18) ($:extends 
    reduce . 18)) (($ident reduce . 17) ($:extends reduce . 17)) (($:record 
    shift . 41) ($:function shift . 42) ($ident reduce . 29) ($:extends reduce
    . 29)) (($ident reduce . 15) ($:extends reduce . 15)) (($ident reduce . 
    14) ($:extends reduce . 14)) (($:extends reduce . 13) ($ident reduce . 13)
    ) (($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) (
    $:package shift . 4) ($:type shift . 5) ($:connector shift . 6) (
    $:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 40)) (($ident shift . 26) ($:extends shift . 34) 
    (ident shift . 35) (der-class-specifier shift . 36) (short-class-specifier
    shift . 37) (long-class-specifier shift . 38) (class-specifier shift . 39
    )) (($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) (
    $:package shift . 4) ($:type shift . 5) ($:connector shift . 6) (
    $:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (class-prefixes shift 
    . 33)) ((#{$:;}# shift . 32)) (($:function shift . 1) ($:pure shift . 2) (
    $:impure shift . 3) ($:package shift . 4) ($:type shift . 5) ($:connector 
    shift . 6) ($:expandable shift . 7) ($:block shift . 8) ($:record shift . 
    9) ($:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (class-prefixes shift 
    . 15) ($:encapsulated shift . 16) (class-definition shift . 31)) (($ident 
    shift . 26) ($:. shift . 27) (ident shift . 28) (name shift . 29) (#{$:;}#
    shift . 30)) (($:final shift . 24) ($:function shift . 1) ($:pure shift 
    . 2) ($:impure shift . 3) ($:package shift . 4) ($:type shift . 5) (
    $:connector shift . 6) ($:expandable shift . 7) ($:block shift . 8) (
    $:record shift . 9) ($:operator shift . 10) ($:model shift . 11) ($:class 
    shift . 12) (class-prefixes-1 shift . 13) ($:partial shift . 14) (
    class-prefixes shift . 15) ($:encapsulated shift . 16) (class-definition 
    shift . 25) ($end reduce . 3)) (($:function shift . 1) ($:pure shift . 2) 
    ($:impure shift . 3) ($:package shift . 4) ($:type shift . 5) ($:connector
    shift . 6) ($:expandable shift . 7) ($:block shift . 8) ($:record shift 
    . 9) ($:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (class-prefixes shift 
    . 15) ($:encapsulated shift . 16) (class-definition shift . 17) ($:final 
    shift . 18) (stored-definition-2 shift . 23)) (($end accept . 0)) ((
    $:final shift . 24) ($:function shift . 1) ($:pure shift . 2) ($:impure 
    shift . 3) ($:package shift . 4) ($:type shift . 5) ($:connector shift . 6
    ) ($:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (class-prefixes shift 
    . 15) ($:encapsulated shift . 16) (class-definition shift . 25) ($end 
    reduce . 2)) (($:function shift . 1) ($:pure shift . 2) ($:impure shift . 
    3) ($:package shift . 4) ($:type shift . 5) ($:connector shift . 6) (
    $:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (class-prefixes shift 
    . 15) ($:encapsulated shift . 16) (class-definition shift . 62)) ((#{$:;}#
    shift . 61)) (($:import reduce . 349) ($:extends reduce . 349) (
    $:function reduce . 349) ($:pure reduce . 349) ($:impure reduce . 349) (
    $:package reduce . 349) ($:type reduce . 349) ($:connector reduce . 349) (
    $:expandable reduce . 349) ($:block reduce . 349) ($:record reduce . 349) 
    ($:operator reduce . 349) ($:model reduce . 349) ($:class reduce . 349) (
    $:partial reduce . 349) ($:encapsulated reduce . 349) ($:stream reduce . 
    349) ($:flow reduce . 349) ($:constant reduce . 349) ($:parameter reduce 
    . 349) ($:discrete reduce . 349) ($:output reduce . 349) ($:input reduce 
    . 349) ($ident reduce . 349) ($:. reduce . 349) ($:replaceable reduce . 
    349) ($:outer reduce . 349) ($:inner reduce . 349) ($:final reduce . 349) 
    ($:redeclare reduce . 349) ($string reduce . 349) ($:= reduce . 349) (
    #{$:;}# reduce . 349) (#{$:\x28;}# reduce . 349) (#{$:\x5b;}# reduce . 349
    ) ($::= reduce . 349) (#{$:\x29;}# reduce . 349) ($:, reduce . 349) (
    $:annotation reduce . 349) ($:if reduce . 349) ($:: reduce . 349) ($:^ 
    reduce . 349) ($:.^ reduce . 349) ($:./ reduce . 349) ($:.* reduce . 349) 
    ($:/ reduce . 349) ($:* reduce . 349) ($:+ reduce . 349) ($:- reduce . 349
    ) ($:.+ reduce . 349) ($:.- reduce . 349) ($:<> reduce . 349) ($:== reduce
    . 349) ($:>= reduce . 349) ($:> reduce . 349) ($:<= reduce . 349) ($:< 
    reduce . 349) ($:and reduce . 349) ($:or reduce . 349) (#{$:\x5d;}# reduce
    . 349) ($:for reduce . 349) ($:then reduce . 349) ($:loop reduce . 349) (
    $:in reduce . 349) (#{$:\x7d;}# reduce . 349) ($:else reduce . 349) (
    $:elseif reduce . 349) ($:constrainedby reduce . 349) ($:end reduce . 349)
    ($:elsewhen reduce . 349)) (($ident shift . 26) (ident shift . 60)) ((
    #{$:;}# reduce . 303) ($:. reduce . 303) (#{$:\x5b;}# reduce . 303) (
    $ident reduce . 303) (#{$:\x29;}# reduce . 303) ($string reduce . 303) (
    #{$:\x28;}# reduce . 303) ($::= reduce . 303) ($:= reduce . 303) ($:, 
    reduce . 303) ($:annotation reduce . 303) ($:: reduce . 303) ($:or reduce 
    . 303) ($:and reduce . 303) ($:< reduce . 303) ($:<= reduce . 303) ($:> 
    reduce . 303) ($:>= reduce . 303) ($:== reduce . 303) ($:<> reduce . 303) 
    ($:.- reduce . 303) ($:.+ reduce . 303) ($:- reduce . 303) ($:+ reduce . 
    303) ($:* reduce . 303) ($:/ reduce . 303) ($:.* reduce . 303) ($:./ 
    reduce . 303) ($:.^ reduce . 303) ($:^ reduce . 303) (#{$:\x5d;}# reduce 
    . 303) ($:then reduce . 303) ($:loop reduce . 303) ($:else reduce . 303) (
    $:elseif reduce . 303) ($:constrainedby reduce . 303) ($:for reduce . 303)
    (#{$:\x7d;}# reduce . 303) ($:end reduce . 303) ($:elsewhen reduce . 303)
    ($:stream reduce . 303) ($:flow reduce . 303) ($:constant reduce . 303) (
    $:parameter reduce . 303) ($:discrete reduce . 303) ($:output reduce . 303
    ) ($:input reduce . 303) ($:if reduce . 303)) ((#{$:;}# shift . 58) ($:. 
    shift . 59)) (($:function reduce . 5) ($:pure reduce . 5) ($:impure reduce
    . 5) ($:package reduce . 5) ($:type reduce . 5) ($:connector reduce . 5) 
    ($:expandable reduce . 5) ($:block reduce . 5) ($:record reduce . 5) (
    $:operator reduce . 5) ($:model reduce . 5) ($:class reduce . 5) (
    $:partial reduce . 5) ($:encapsulated reduce . 5) ($:final reduce . 5)) ((
    #{$:;}# shift . 57)) (($:function reduce . 7) ($:pure reduce . 7) (
    $:impure reduce . 7) ($:package reduce . 7) ($:type reduce . 7) (
    $:connector reduce . 7) ($:expandable reduce . 7) ($:block reduce . 7) (
    $:record reduce . 7) ($:operator reduce . 7) ($:model reduce . 7) ($:class
    reduce . 7) ($:partial reduce . 7) ($:encapsulated reduce . 7) ($:final 
    reduce . 7) ($end reduce . 7)) (($ident shift . 26) ($:extends shift . 34)
    (ident shift . 35) (der-class-specifier shift . 36) (
    short-class-specifier shift . 37) (long-class-specifier shift . 38) (
    class-specifier shift . 56)) (($ident shift . 26) (ident shift . 55)) ((
    $string shift . 50) (string shift . 51) (string-cat shift . 52) (
    string-comment shift . 53) ($:= shift . 54) ($:import reduce . 340) (
    $:extends reduce . 340) ($:function reduce . 340) ($:pure reduce . 340) (
    $:impure reduce . 340) ($:package reduce . 340) ($:type reduce . 340) (
    $:connector reduce . 340) ($:expandable reduce . 340) ($:block reduce . 
    340) ($:record reduce . 340) ($:operator reduce . 340) ($:model reduce . 
    340) ($:class reduce . 340) ($:partial reduce . 340) ($:encapsulated 
    reduce . 340) ($:stream reduce . 340) ($:flow reduce . 340) ($:constant 
    reduce . 340) ($:parameter reduce . 340) ($:discrete reduce . 340) (
    $:output reduce . 340) ($:input reduce . 340) ($ident reduce . 340) ($:. 
    reduce . 340) ($:replaceable reduce . 340) ($:outer reduce . 340) ($:inner
    reduce . 340) ($:final reduce . 340) ($:redeclare reduce . 340)) ((
    #{$:;}# reduce . 32) ($:constrainedby reduce . 32)) ((#{$:;}# reduce . 31)
    ($:constrainedby reduce . 31)) ((#{$:;}# reduce . 30) ($:constrainedby 
    reduce . 30)) ((#{$:;}# reduce . 11) ($:constrainedby reduce . 11)) ((
    $:extends reduce . 12) ($ident reduce . 12)) (($ident reduce . 16) (
    $:extends reduce . 16)) (($ident reduce . 27) ($:extends reduce . 27)) ((
    $ident reduce . 19) ($:extends reduce . 19)) (($:function shift . 49)) ((
    $ident reduce . 25) ($:extends reduce . 25)) (($:function shift . 48)) ((
    $ident reduce . 26) ($:extends reduce . 26)) (($ident reduce . 24) (
    $:extends reduce . 24)) (($ident reduce . 23) ($:extends reduce . 23)) ((
    $:import reduce . 350) ($:extends reduce . 350) ($:function reduce . 350) 
    ($:pure reduce . 350) ($:impure reduce . 350) ($:package reduce . 350) (
    $:type reduce . 350) ($:connector reduce . 350) ($:expandable reduce . 350
    ) ($:block reduce . 350) ($:record reduce . 350) ($:operator reduce . 350)
    ($:model reduce . 350) ($:class reduce . 350) ($:partial reduce . 350) (
    $:encapsulated reduce . 350) ($:stream reduce . 350) ($:flow reduce . 350)
    ($:constant reduce . 350) ($:parameter reduce . 350) ($:discrete reduce 
    . 350) ($:output reduce . 350) ($:input reduce . 350) ($ident reduce . 350
    ) ($:. reduce . 350) ($:replaceable reduce . 350) ($:outer reduce . 350) (
    $:inner reduce . 350) ($:final reduce . 350) ($:redeclare reduce . 350) (
    $:+ reduce . 350) ($:: reduce . 350) ($:= reduce . 350) ($:or reduce . 350
    ) ($:and reduce . 350) ($:< reduce . 350) ($:<= reduce . 350) ($:> reduce 
    . 350) ($:>= reduce . 350) ($:== reduce . 350) ($:<> reduce . 350) ($:.- 
    reduce . 350) ($:.+ reduce . 350) ($:- reduce . 350) ($:* reduce . 350) (
    $:/ reduce . 350) ($:.* reduce . 350) ($:./ reduce . 350) ($:.^ reduce . 
    350) ($:^ reduce . 350) (#{$:;}# reduce . 350) ($:annotation reduce . 350)
    (#{$:\x5d;}# reduce . 350) ($:, reduce . 350) ($:for reduce . 350) (
    #{$:\x29;}# reduce . 350) ($:then reduce . 350) ($:loop reduce . 350) (
    $:else reduce . 350) ($:elseif reduce . 350) ($:constrainedby reduce . 350
    ) ($string reduce . 350) (#{$:\x7d;}# reduce . 350) ($:end reduce . 350) (
    $:elsewhen reduce . 350) ($:if reduce . 350)) (($:redeclare reduce . 342) 
    ($:final reduce . 342) ($:inner reduce . 342) ($:outer reduce . 342) (
    $:replaceable reduce . 342) ($:. reduce . 342) ($ident reduce . 342) (
    $:input reduce . 342) ($:output reduce . 342) ($:discrete reduce . 342) (
    $:parameter reduce . 342) ($:constant reduce . 342) ($:flow reduce . 342) 
    ($:stream reduce . 342) ($:encapsulated reduce . 342) ($:partial reduce . 
    342) ($:class reduce . 342) ($:model reduce . 342) ($:operator reduce . 
    342) ($:record reduce . 342) ($:block reduce . 342) ($:expandable reduce 
    . 342) ($:connector reduce . 342) ($:type reduce . 342) ($:package reduce 
    . 342) ($:impure reduce . 342) ($:pure reduce . 342) ($:function reduce . 
    342) ($:extends reduce . 342) ($:import reduce . 342) ($:+ reduce . 342) (
    $:annotation reduce . 342) (#{$:;}# reduce . 342) ($:, reduce . 342) (
    $:constrainedby reduce . 342) (#{$:\x29;}# reduce . 342)) (($:+ shift . 
    100) ($:import reduce . 341) ($:extends reduce . 341) ($:function reduce 
    . 341) ($:pure reduce . 341) ($:impure reduce . 341) ($:package reduce . 
    341) ($:type reduce . 341) ($:connector reduce . 341) ($:expandable reduce
    . 341) ($:block reduce . 341) ($:record reduce . 341) ($:operator reduce 
    . 341) ($:model reduce . 341) ($:class reduce . 341) ($:partial reduce . 
    341) ($:encapsulated reduce . 341) ($:stream reduce . 341) ($:flow reduce 
    . 341) ($:constant reduce . 341) ($:parameter reduce . 341) ($:discrete 
    reduce . 341) ($:output reduce . 341) ($:input reduce . 341) ($ident 
    reduce . 341) ($:. reduce . 341) ($:replaceable reduce . 341) ($:outer 
    reduce . 341) ($:inner reduce . 341) ($:final reduce . 341) ($:redeclare 
    reduce . 341) ($:annotation reduce . 341) (#{$:;}# reduce . 341) ($:, 
    reduce . 341) ($:constrainedby reduce . 341) (#{$:\x29;}# reduce . 341)) (
    ($ident shift . 26) ($:. shift . 27) (ident shift . 28) ($:output shift . 
    70) ($:input shift . 71) ($:constant shift . 72) ($:parameter shift . 73) 
    ($:discrete shift . 74) ($:stream shift . 75) ($:flow shift . 76) (
    $:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package 
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) (
    $:model shift . 11) ($:class shift . 12) (name shift . 82) (type-prefix-3 
    shift . 77) (type-prefix-2 shift . 78) (type-prefix-1 shift . 79) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (type-specifier shift 
    . 83) (type-prefix shift . 84) (class-prefixes shift . 15) ($:encapsulated
    shift . 16) ($:extends shift . 85) ($:import shift . 86) ($:replaceable 
    shift . 87) (component-clause shift . 88) (class-definition shift . 89) (
    element-1 shift . 90) ($:outer shift . 91) ($:inner shift . 92) ($:final 
    shift . 93) ($:redeclare shift . 94) (extends-clause shift . 95) (
    import-clause shift . 96) (element shift . 97) (element-list shift . 98) (
    composition shift . 99)) (($:der shift . 68) ($:enumeration shift . 69) (
    $:output shift . 70) ($:input shift . 71) ($:constant shift . 72) (
    $:parameter shift . 73) ($:discrete shift . 74) ($:stream shift . 75) (
    $:flow shift . 76) (type-prefix-3 shift . 77) (type-prefix-2 shift . 78) (
    type-prefix-1 shift . 79) (type-prefix shift . 80) (base-prefix shift . 81
    ) ($ident reduce . 47) ($:. reduce . 47)) (($string shift . 50) (string 
    shift . 51) (string-cat shift . 52) (string-comment shift . 65) (
    #{$:\x28;}# shift . 66) (class-modification shift . 67) ($:import reduce 
    . 340) ($:extends reduce . 340) ($:function reduce . 340) ($:pure reduce 
    . 340) ($:impure reduce . 340) ($:package reduce . 340) ($:type reduce . 
    340) ($:connector reduce . 340) ($:expandable reduce . 340) ($:block 
    reduce . 340) ($:record reduce . 340) ($:operator reduce . 340) ($:model 
    reduce . 340) ($:class reduce . 340) ($:partial reduce . 340) (
    $:encapsulated reduce . 340) ($:stream reduce . 340) ($:flow reduce . 340)
    ($:constant reduce . 340) ($:parameter reduce . 340) ($:discrete reduce 
    . 340) ($:output reduce . 340) ($:input reduce . 340) ($ident reduce . 340
    ) ($:. reduce . 340) ($:replaceable reduce . 340) ($:outer reduce . 340) (
    $:inner reduce . 340) ($:final reduce . 340) ($:redeclare reduce . 340)) (
    (#{$:;}# reduce . 10) ($:constrainedby reduce . 10)) (($:function reduce 
    . 6) ($:pure reduce . 6) ($:impure reduce . 6) ($:package reduce . 6) (
    $:type reduce . 6) ($:connector reduce . 6) ($:expandable reduce . 6) (
    $:block reduce . 6) ($:record reduce . 6) ($:operator reduce . 6) ($:model
    reduce . 6) ($:class reduce . 6) ($:partial reduce . 6) ($:encapsulated 
    reduce . 6) ($:final reduce . 6) ($end reduce . 6)) (($:function reduce . 
    4) ($:pure reduce . 4) ($:impure reduce . 4) ($:package reduce . 4) (
    $:type reduce . 4) ($:connector reduce . 4) ($:expandable reduce . 4) (
    $:block reduce . 4) ($:record reduce . 4) ($:operator reduce . 4) ($:model
    reduce . 4) ($:class reduce . 4) ($:partial reduce . 4) ($:encapsulated 
    reduce . 4) ($:final reduce . 4)) (($ident shift . 26) (ident shift . 64))
    ((#{$:;}# reduce . 304) ($:. reduce . 304) (#{$:\x5b;}# reduce . 304) (
    $ident reduce . 304) (#{$:\x29;}# reduce . 304) ($string reduce . 304) (
    #{$:\x28;}# reduce . 304) ($::= reduce . 304) ($:= reduce . 304) ($:, 
    reduce . 304) ($:annotation reduce . 304) ($:: reduce . 304) ($:or reduce 
    . 304) ($:and reduce . 304) ($:< reduce . 304) ($:<= reduce . 304) ($:> 
    reduce . 304) ($:>= reduce . 304) ($:== reduce . 304) ($:<> reduce . 304) 
    ($:.- reduce . 304) ($:.+ reduce . 304) ($:- reduce . 304) ($:+ reduce . 
    304) ($:* reduce . 304) ($:/ reduce . 304) ($:.* reduce . 304) ($:./ 
    reduce . 304) ($:.^ reduce . 304) ($:^ reduce . 304) (#{$:\x5d;}# reduce 
    . 304) ($:for reduce . 304) ($:then reduce . 304) ($:loop reduce . 304) (
    $:else reduce . 304) ($:elseif reduce . 304) ($:constrainedby reduce . 304
    ) (#{$:\x7d;}# reduce . 304) ($:end reduce . 304) ($:elsewhen reduce . 304
    ) ($:stream reduce . 304) ($:flow reduce . 304) ($:constant reduce . 304) 
    ($:parameter reduce . 304) ($:discrete reduce . 304) ($:output reduce . 
    304) ($:input reduce . 304) ($:if reduce . 304)) (($:function reduce . 9) 
    ($:pure reduce . 9) ($:impure reduce . 9) ($:package reduce . 9) ($:type 
    reduce . 9) ($:connector reduce . 9) ($:expandable reduce . 9) ($:block 
    reduce . 9) ($:record reduce . 9) ($:operator reduce . 9) ($:model reduce 
    . 9) ($:class reduce . 9) ($:partial reduce . 9) ($:encapsulated reduce . 
    9) ($:final reduce . 9) ($end reduce . 9)) ((#{$:;}# shift . 63)) ((
    $:function reduce . 8) ($:pure reduce . 8) ($:impure reduce . 8) (
    $:package reduce . 8) ($:type reduce . 8) ($:connector reduce . 8) (
    $:expandable reduce . 8) ($:block reduce . 8) ($:record reduce . 8) (
    $:operator reduce . 8) ($:model reduce . 8) ($:class reduce . 8) (
    $:partial reduce . 8) ($:encapsulated reduce . 8) ($:final reduce . 8) (
    $end reduce . 8)) ((#{$:;}# reduce . 305) ($:. reduce . 305) ($:annotation
    reduce . 305) ($string reduce . 305) (#{$:\x5b;}# reduce . 305) ($ident 
    reduce . 305) (#{$:\x28;}# reduce . 305) ($:constrainedby reduce . 305) (
    #{$:\x29;}# reduce . 305) ($::= reduce . 305) ($:= reduce . 305) ($:, 
    reduce . 305) ($:: reduce . 305) (#{$:\x5d;}# reduce . 305) ($:or reduce 
    . 305) ($:and reduce . 305) ($:< reduce . 305) ($:<= reduce . 305) ($:> 
    reduce . 305) ($:>= reduce . 305) ($:== reduce . 305) ($:<> reduce . 305) 
    ($:.- reduce . 305) ($:.+ reduce . 305) ($:- reduce . 305) ($:+ reduce . 
    305) ($:* reduce . 305) ($:/ reduce . 305) ($:.* reduce . 305) ($:./ 
    reduce . 305) ($:.^ reduce . 305) ($:^ reduce . 305) ($:for reduce . 305) 
    ($:then reduce . 305) ($:loop reduce . 305) ($:else reduce . 305) (
    $:elseif reduce . 305) ($:stream reduce . 305) ($:flow reduce . 305) (
    $:constant reduce . 305) ($:parameter reduce . 305) ($:discrete reduce . 
    305) ($:output reduce . 305) ($:input reduce . 305) (#{$:\x7d;}# reduce . 
    305) ($:end reduce . 305) ($:elsewhen reduce . 305) ($:if reduce . 305)) (
    ($ident shift . 26) ($:. shift . 27) (ident shift . 28) ($:output shift . 
    70) ($:input shift . 71) ($:constant shift . 72) ($:parameter shift . 73) 
    ($:discrete shift . 74) ($:stream shift . 75) ($:flow shift . 76) (
    $:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package 
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) (
    $:model shift . 11) ($:class shift . 12) (name shift . 82) (type-prefix-3 
    shift . 77) (type-prefix-2 shift . 78) (type-prefix-1 shift . 79) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (type-specifier shift 
    . 83) (type-prefix shift . 84) (class-prefixes shift . 15) ($:encapsulated
    shift . 16) ($:extends shift . 85) ($:import shift . 86) ($:replaceable 
    shift . 87) (component-clause shift . 88) (class-definition shift . 89) (
    element-1 shift . 90) ($:outer shift . 91) ($:inner shift . 92) ($:final 
    shift . 93) ($:redeclare shift . 94) (extends-clause shift . 95) (
    import-clause shift . 96) (element shift . 97) (element-list shift . 98) (
    composition shift . 160)) (($ident shift . 26) ($:. shift . 27) (ident 
    shift . 28) ($:replaceable shift . 147) (name shift . 148) (
    element-replaceable shift . 149) (element-modification shift . 150) (
    $:redeclare shift . 151) (elt-mod-or-repl-1 shift . 152) ($:final shift . 
    153) ($:each shift . 154) (element-redeclaration shift . 155) (
    element-modification-or-replaceable shift . 156) (argument shift . 157) (
    argument-list shift . 158) (#{$:\x29;}# shift . 159)) (($string shift . 50
    ) (string shift . 51) (string-cat shift . 52) (string-comment shift . 146)
    ($:import reduce . 340) ($:extends reduce . 340) ($:function reduce . 340
    ) ($:pure reduce . 340) ($:impure reduce . 340) ($:package reduce . 340) (
    $:type reduce . 340) ($:connector reduce . 340) ($:expandable reduce . 340
    ) ($:block reduce . 340) ($:record reduce . 340) ($:operator reduce . 340)
    ($:model reduce . 340) ($:class reduce . 340) ($:partial reduce . 340) (
    $:encapsulated reduce . 340) ($:stream reduce . 340) ($:flow reduce . 340)
    ($:constant reduce . 340) ($:parameter reduce . 340) ($:discrete reduce 
    . 340) ($:output reduce . 340) ($:input reduce . 340) ($ident reduce . 340
    ) ($:. reduce . 340) ($:replaceable reduce . 340) ($:outer reduce . 340) (
    $:inner reduce . 340) ($:final reduce . 340) ($:redeclare reduce . 340)) (
    (#{$:\x28;}# shift . 145)) ((#{$:\x28;}# shift . 144)) (($:. reduce . 136)
    ($ident reduce . 136)) (($:. reduce . 135) ($ident reduce . 135)) (($:. 
    reduce . 134) ($ident reduce . 134) ($:output reduce . 134) ($:input 
    reduce . 134)) (($:. reduce . 133) ($ident reduce . 133) ($:output reduce 
    . 133) ($:input reduce . 133)) (($:. reduce . 132) ($ident reduce . 132) (
    $:output reduce . 132) ($:input reduce . 132)) (($:. reduce . 131) ($ident
    reduce . 131) ($:output reduce . 131) ($:input reduce . 131) ($:constant 
    reduce . 131) ($:parameter reduce . 131) ($:discrete reduce . 131)) (($:. 
    reduce . 130) ($ident reduce . 130) ($:output reduce . 130) ($:input 
    reduce . 130) ($:constant reduce . 130) ($:parameter reduce . 130) (
    $:discrete reduce . 130)) (($ident reduce . 129) ($:. reduce . 129)) ((
    $:output shift . 70) ($:input shift . 71) (type-prefix-3 shift . 143) (
    $ident reduce . 128) ($:. reduce . 128)) (($:constant shift . 72) (
    $:parameter shift . 73) ($:discrete shift . 74) (type-prefix-2 shift . 141
    ) ($:output shift . 70) ($:input shift . 71) (type-prefix-3 shift . 142) (
    $ident reduce . 127) ($:. reduce . 127)) (($ident reduce . 48) ($:. reduce
    . 48)) (($ident shift . 26) ($:. shift . 27) (ident shift . 28) (name 
    shift . 140)) (($:. shift . 59) ($ident reduce . 137) (#{$:\x5b;}# reduce 
    . 137)) ((#{$:\x5b;}# shift . 134) (array-subscripts shift . 135) ($ident 
    shift . 26) (ident shift . 136) (declaration shift . 137) (
    component-declaration shift . 138) (component-list shift . 139)) (($ident 
    shift . 26) ($:. shift . 27) (ident shift . 28) (name shift . 82) (
    type-specifier shift . 133)) (($ident shift . 26) ($:. shift . 27) (ident 
    shift . 28) (name shift . 132)) (($ident shift . 26) ($:. shift . 27) (
    name shift . 129) (ident shift . 130) (import-clause-1 shift . 131)) ((
    $ident shift . 26) ($:. shift . 27) (ident shift . 28) ($:output shift . 
    70) ($:input shift . 71) ($:constant shift . 72) ($:parameter shift . 73) 
    ($:discrete shift . 74) ($:stream shift . 75) ($:flow shift . 76) (
    $:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package 
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) (
    $:model shift . 11) ($:class shift . 12) (name shift . 82) (type-prefix-3 
    shift . 77) (type-prefix-2 shift . 78) (type-prefix-1 shift . 79) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (type-specifier shift 
    . 83) (type-prefix shift . 84) (class-prefixes shift . 15) ($:encapsulated
    shift . 16) (component-clause shift . 126) (class-definition shift . 127)
    (element-2 shift . 128)) ((#{$:;}# reduce . 99)) ((#{$:;}# reduce . 98)) 
    ((#{$:;}# reduce . 85)) (($ident shift . 26) ($:. shift . 27) (ident shift
    . 28) ($:output shift . 70) ($:input shift . 71) ($:constant shift . 72) 
    ($:parameter shift . 73) ($:discrete shift . 74) ($:stream shift . 75) (
    $:flow shift . 76) ($:function shift . 1) ($:pure shift . 2) ($:impure 
    shift . 3) ($:package shift . 4) ($:type shift . 5) ($:connector shift . 6
    ) ($:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (name 
    shift . 82) (type-prefix-3 shift . 77) (type-prefix-2 shift . 78) (
    type-prefix-1 shift . 79) (class-prefixes-1 shift . 13) ($:partial shift 
    . 14) (type-specifier shift . 83) (type-prefix shift . 84) (class-prefixes
    shift . 15) ($:encapsulated shift . 16) ($:replaceable shift . 87) (
    component-clause shift . 88) (class-definition shift . 89) (element-1 
    shift . 125)) (($:outer shift . 123) ($P6 shift . 124) ($:function reduce 
    . 96) ($:pure reduce . 96) ($:impure reduce . 96) ($:package reduce . 96) 
    ($:type reduce . 96) ($:connector reduce . 96) ($:expandable reduce . 96) 
    ($:block reduce . 96) ($:record reduce . 96) ($:operator reduce . 96) (
    $:model reduce . 96) ($:class reduce . 96) ($:partial reduce . 96) (
    $:encapsulated reduce . 96) ($:stream reduce . 96) ($:flow reduce . 96) (
    $:constant reduce . 96) ($:parameter reduce . 96) ($:discrete reduce . 96)
    ($:output reduce . 96) ($:input reduce . 96) ($ident reduce . 96) ($:. 
    reduce . 96) ($:replaceable reduce . 96)) (($:inner shift . 121) ($P4 
    shift . 122) ($:function reduce . 92) ($:pure reduce . 92) ($:impure 
    reduce . 92) ($:package reduce . 92) ($:type reduce . 92) ($:connector 
    reduce . 92) ($:expandable reduce . 92) ($:block reduce . 92) ($:record 
    reduce . 92) ($:operator reduce . 92) ($:model reduce . 92) ($:class 
    reduce . 92) ($:partial reduce . 92) ($:encapsulated reduce . 92) (
    $:stream reduce . 92) ($:flow reduce . 92) ($:constant reduce . 92) (
    $:parameter reduce . 92) ($:discrete reduce . 92) ($:output reduce . 92) (
    $:input reduce . 92) ($ident reduce . 92) ($:. reduce . 92) ($:replaceable
    reduce . 92) ($:outer reduce . 92)) (($:final shift . 119) ($P1 shift . 
    120) ($:function reduce . 86) ($:pure reduce . 86) ($:impure reduce . 86) 
    ($:package reduce . 86) ($:type reduce . 86) ($:connector reduce . 86) (
    $:expandable reduce . 86) ($:block reduce . 86) ($:record reduce . 86) (
    $:operator reduce . 86) ($:model reduce . 86) ($:class reduce . 86) (
    $:partial reduce . 86) ($:encapsulated reduce . 86) ($:stream reduce . 86)
    ($:flow reduce . 86) ($:constant reduce . 86) ($:parameter reduce . 86) (
    $:discrete reduce . 86) ($:output reduce . 86) ($:input reduce . 86) (
    $ident reduce . 86) ($:. reduce . 86) ($:replaceable reduce . 86) ($:outer
    reduce . 86) ($:inner reduce . 86)) ((#{$:;}# reduce . 80)) ((#{$:;}# 
    reduce . 79)) ((#{$:;}# shift . 118)) (($:algorithm shift . 103) (
    $:equation shift . 104) ($:initial shift . 105) (algorithm-section shift 
    . 106) (equation-section shift . 107) ($:protected shift . 108) ($:public 
    shift . 109) (composition-1 shift . 110) (composition-1-list shift . 111) 
    ($:external shift . 112) (external-part shift . 113) ($:annotation shift 
    . 114) (annotation shift . 115) (opt-annotation shift . 116) ($ident shift
    . 26) ($:. shift . 27) (ident shift . 28) ($:output shift . 70) ($:input 
    shift . 71) ($:constant shift . 72) ($:parameter shift . 73) ($:discrete 
    shift . 74) ($:stream shift . 75) ($:flow shift . 76) ($:function shift . 
    1) ($:pure shift . 2) ($:impure shift . 3) ($:package shift . 4) ($:type 
    shift . 5) ($:connector shift . 6) ($:expandable shift . 7) ($:block shift
    . 8) ($:record shift . 9) ($:operator shift . 10) ($:model shift . 11) (
    $:class shift . 12) (name shift . 82) (type-prefix-3 shift . 77) (
    type-prefix-2 shift . 78) (type-prefix-1 shift . 79) (class-prefixes-1 
    shift . 13) ($:partial shift . 14) (type-specifier shift . 83) (
    type-prefix shift . 84) (class-prefixes shift . 15) ($:encapsulated shift 
    . 16) ($:extends shift . 85) ($:import shift . 86) ($:replaceable shift . 
    87) (component-clause shift . 88) (class-definition shift . 89) (element-1
    shift . 90) ($:outer shift . 91) ($:inner shift . 92) ($:final shift . 93
    ) ($:redeclare shift . 94) (extends-clause shift . 95) (import-clause 
    shift . 96) (element shift . 117) ($:end reduce . 344)) (($:end shift . 
    102)) (($string shift . 50) (string shift . 101)) (($:redeclare reduce . 
    343) ($:final reduce . 343) ($:inner reduce . 343) ($:outer reduce . 343) 
    ($:replaceable reduce . 343) ($:. reduce . 343) ($ident reduce . 343) (
    $:input reduce . 343) ($:output reduce . 343) ($:discrete reduce . 343) (
    $:parameter reduce . 343) ($:constant reduce . 343) ($:flow reduce . 343) 
    ($:stream reduce . 343) ($:encapsulated reduce . 343) ($:partial reduce . 
    343) ($:class reduce . 343) ($:model reduce . 343) ($:operator reduce . 
    343) ($:record reduce . 343) ($:block reduce . 343) ($:expandable reduce 
    . 343) ($:connector reduce . 343) ($:type reduce . 343) ($:package reduce 
    . 343) ($:impure reduce . 343) ($:pure reduce . 343) ($:function reduce . 
    343) ($:extends reduce . 343) ($:import reduce . 343) ($:+ reduce . 343) (
    $:annotation reduce . 343) (#{$:;}# reduce . 343) ($:, reduce . 343) (
    $:constrainedby reduce . 343) (#{$:\x29;}# reduce . 343)) (($ident shift 
    . 26) (ident shift . 284)) (($ident shift . 26) ($:. shift . 241) (ident 
    shift . 268) (component-reference-1 shift . 242) ($:when shift . 269) (
    $:while shift . 270) ($:for shift . 271) ($:if shift . 272) (
    when-statement shift . 273) (while-statement shift . 274) (for-statement 
    shift . 275) (if-statement shift . 276) ($:return shift . 277) ($:break 
    shift . 278) (#{$:\x28;}# shift . 279) (component-reference shift . 280) (
    statement-1 shift . 281) (statement shift . 282) (statement-list shift . 
    283) ($:annotation reduce . 186) ($:equation reduce . 186) ($:algorithm 
    reduce . 186) ($:initial reduce . 186) ($:protected reduce . 186) (
    $:public reduce . 186) ($:external reduce . 186) ($:end reduce . 186)) ((
    $string shift . 50) ($float shift . 195) ($fixed shift . 196) (#{$:\x7b;}#
    shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der 
    shift . 200) ($:true shift . 202) ($:false shift . 203) (string shift . 
    204) (unsigned-number shift . 205) (primary shift . 206) (factor shift . 
    207) (term shift . 208) (arithmetic-expression shift . 209) ($:not shift 
    . 210) (relation shift . 211) (logical-factor shift . 212) ($ident shift 
    . 26) (logical-term shift . 213) ($:. shift . 27) (ident shift . 28) (
    logical-expression shift . 214) ($:connect shift . 255) ($:when shift . 
    256) ($:for shift . 257) ($:if shift . 258) (name shift . 259) (
    when-equation shift . 260) (connect-clause shift . 261) (for-equation 
    shift . 262) (if-equation shift . 263) (simple-expression shift . 264) (
    equation-1 shift . 265) (equation shift . 266) (equation-list shift . 267)
    ($:annotation reduce . 182) ($:equation reduce . 182) ($:algorithm reduce
    . 182) ($:initial reduce . 182) ($:protected reduce . 182) ($:public 
    reduce . 182) ($:external reduce . 182) ($:end reduce . 182)) (($:equation
    shift . 253) ($:algorithm shift . 254)) (($:annotation reduce . 63) (
    $:public reduce . 63) ($:protected reduce . 63) ($:initial reduce . 63) (
    $:algorithm reduce . 63) ($:equation reduce . 63) ($:external reduce . 63)
    ($:end reduce . 63)) (($:annotation reduce . 62) ($:public reduce . 62) (
    $:protected reduce . 62) ($:initial reduce . 62) ($:algorithm reduce . 62)
    ($:equation reduce . 62) ($:external reduce . 62) ($:end reduce . 62)) ((
    $ident shift . 26) ($:. shift . 27) (ident shift . 28) ($:output shift . 
    70) ($:input shift . 71) ($:constant shift . 72) ($:parameter shift . 73) 
    ($:discrete shift . 74) ($:stream shift . 75) ($:flow shift . 76) (
    $:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package 
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) (
    $:model shift . 11) ($:class shift . 12) (name shift . 82) (type-prefix-3 
    shift . 77) (type-prefix-2 shift . 78) (type-prefix-1 shift . 79) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (type-specifier shift 
    . 83) (type-prefix shift . 84) (class-prefixes shift . 15) ($:encapsulated
    shift . 16) ($:extends shift . 85) ($:import shift . 86) ($:replaceable 
    shift . 87) (component-clause shift . 88) (class-definition shift . 89) (
    element-1 shift . 90) ($:outer shift . 91) ($:inner shift . 92) ($:final 
    shift . 93) ($:redeclare shift . 94) (extends-clause shift . 95) (
    import-clause shift . 96) (element shift . 97) (element-list shift . 252) 
    ($:annotation reduce . 60) ($:public reduce . 60) ($:protected reduce . 60
    ) ($:initial reduce . 60) ($:algorithm reduce . 60) ($:equation reduce . 
    60) ($:external reduce . 60) ($:end reduce . 60)) (($ident shift . 26) (
    $:. shift . 27) (ident shift . 28) ($:output shift . 70) ($:input shift . 
    71) ($:constant shift . 72) ($:parameter shift . 73) ($:discrete shift . 
    74) ($:stream shift . 75) ($:flow shift . 76) ($:function shift . 1) (
    $:pure shift . 2) ($:impure shift . 3) ($:package shift . 4) ($:type shift
    . 5) ($:connector shift . 6) ($:expandable shift . 7) ($:block shift . 8)
    ($:record shift . 9) ($:operator shift . 10) ($:model shift . 11) (
    $:class shift . 12) (name shift . 82) (type-prefix-3 shift . 77) (
    type-prefix-2 shift . 78) (type-prefix-1 shift . 79) (class-prefixes-1 
    shift . 13) ($:partial shift . 14) (type-specifier shift . 83) (
    type-prefix shift . 84) (class-prefixes shift . 15) ($:encapsulated shift 
    . 16) ($:extends shift . 85) ($:import shift . 86) ($:replaceable shift . 
    87) (component-clause shift . 88) (class-definition shift . 89) (element-1
    shift . 90) ($:outer shift . 91) ($:inner shift . 92) ($:final shift . 93
    ) ($:redeclare shift . 94) (extends-clause shift . 95) (import-clause 
    shift . 96) (element shift . 97) (element-list shift . 251) ($:annotation 
    reduce . 58) ($:public reduce . 58) ($:protected reduce . 58) ($:initial 
    reduce . 58) ($:algorithm reduce . 58) ($:equation reduce . 58) (
    $:external reduce . 58) ($:end reduce . 58)) (($:annotation reduce . 56) (
    $:equation reduce . 56) ($:algorithm reduce . 56) ($:initial reduce . 56) 
    ($:protected reduce . 56) ($:public reduce . 56) ($:external reduce . 56) 
    ($:end reduce . 56)) (($:annotation shift . 114) (annotation shift . 115) 
    (opt-annotation shift . 248) ($:algorithm shift . 103) ($:equation shift 
    . 104) ($:initial shift . 105) (algorithm-section shift . 106) (
    equation-section shift . 107) ($:protected shift . 108) ($:public shift . 
    109) (composition-1 shift . 249) ($:external shift . 112) (external-part 
    shift . 250) ($:end reduce . 344)) (($string shift . 50) (string shift . 
    239) (language-specification shift . 240) ($:. shift . 241) ($ident shift 
    . 26) (component-reference-1 shift . 242) (ident shift . 243) (
    component-reference shift . 244) (external-function-call shift . 245) (
    $:annotation shift . 114) (annotation shift . 246) (#{$:;}# shift . 247)) 
    (($:annotation shift . 114) (annotation shift . 115) (opt-annotation shift
    . 238) ($:end reduce . 344)) ((#{$:\x28;}# shift . 66) (
    class-modification shift . 237)) ((#{$:;}# shift . 236)) (($:end reduce . 
    55)) ((#{$:;}# shift . 235)) (($:end reduce . 77) ($:annotation reduce . 
    77) ($:external reduce . 77) ($:equation reduce . 77) ($:algorithm reduce 
    . 77) ($:initial reduce . 77) ($:protected reduce . 77) ($:public reduce 
    . 77) ($:import reduce . 77) ($:extends reduce . 77) ($:function reduce . 
    77) ($:pure reduce . 77) ($:impure reduce . 77) ($:package reduce . 77) (
    $:type reduce . 77) ($:connector reduce . 77) ($:expandable reduce . 77) (
    $:block reduce . 77) ($:record reduce . 77) ($:operator reduce . 77) (
    $:model reduce . 77) ($:class reduce . 77) ($:partial reduce . 77) (
    $:encapsulated reduce . 77) ($:stream reduce . 77) ($:flow reduce . 77) (
    $:constant reduce . 77) ($:parameter reduce . 77) ($:discrete reduce . 77)
    ($:output reduce . 77) ($:input reduce . 77) ($ident reduce . 77) ($:. 
    reduce . 77) ($:replaceable reduce . 77) ($:outer reduce . 77) ($:inner 
    reduce . 77) ($:final reduce . 77) ($:redeclare reduce . 77)) (($:function
    reduce . 87) ($:pure reduce . 87) ($:impure reduce . 87) ($:package 
    reduce . 87) ($:type reduce . 87) ($:connector reduce . 87) ($:expandable 
    reduce . 87) ($:block reduce . 87) ($:record reduce . 87) ($:operator 
    reduce . 87) ($:model reduce . 87) ($:class reduce . 87) ($:partial reduce
    . 87) ($:encapsulated reduce . 87) ($:stream reduce . 87) ($:flow reduce 
    . 87) ($:constant reduce . 87) ($:parameter reduce . 87) ($:discrete 
    reduce . 87) ($:output reduce . 87) ($:input reduce . 87) ($ident reduce 
    . 87) ($:. reduce . 87) ($:replaceable reduce . 87) ($:outer reduce . 87) 
    ($:inner reduce . 87)) (($:inner shift . 233) ($P2 shift . 234) (
    $:function reduce . 88) ($:pure reduce . 88) ($:impure reduce . 88) (
    $:package reduce . 88) ($:type reduce . 88) ($:connector reduce . 88) (
    $:expandable reduce . 88) ($:block reduce . 88) ($:record reduce . 88) (
    $:operator reduce . 88) ($:model reduce . 88) ($:class reduce . 88) (
    $:partial reduce . 88) ($:encapsulated reduce . 88) ($:stream reduce . 88)
    ($:flow reduce . 88) ($:constant reduce . 88) ($:parameter reduce . 88) (
    $:discrete reduce . 88) ($:output reduce . 88) ($:input reduce . 88) (
    $ident reduce . 88) ($:. reduce . 88) ($:replaceable reduce . 88) ($:outer
    reduce . 88)) (($:function reduce . 93) ($:pure reduce . 93) ($:impure 
    reduce . 93) ($:package reduce . 93) ($:type reduce . 93) ($:connector 
    reduce . 93) ($:expandable reduce . 93) ($:block reduce . 93) ($:record 
    reduce . 93) ($:operator reduce . 93) ($:model reduce . 93) ($:class 
    reduce . 93) ($:partial reduce . 93) ($:encapsulated reduce . 93) (
    $:stream reduce . 93) ($:flow reduce . 93) ($:constant reduce . 93) (
    $:parameter reduce . 93) ($:discrete reduce . 93) ($:output reduce . 93) (
    $:input reduce . 93) ($ident reduce . 93) ($:. reduce . 93) ($:replaceable
    reduce . 93) ($:outer reduce . 93)) (($:outer shift . 231) ($P5 shift . 
    232) ($:function reduce . 94) ($:pure reduce . 94) ($:impure reduce . 94) 
    ($:package reduce . 94) ($:type reduce . 94) ($:connector reduce . 94) (
    $:expandable reduce . 94) ($:block reduce . 94) ($:record reduce . 94) (
    $:operator reduce . 94) ($:model reduce . 94) ($:class reduce . 94) (
    $:partial reduce . 94) ($:encapsulated reduce . 94) ($:stream reduce . 94)
    ($:flow reduce . 94) ($:constant reduce . 94) ($:parameter reduce . 94) (
    $:discrete reduce . 94) ($:output reduce . 94) ($:input reduce . 94) (
    $ident reduce . 94) ($:. reduce . 94) ($:replaceable reduce . 94)) ((
    $:function reduce . 97) ($:pure reduce . 97) ($:impure reduce . 97) (
    $:package reduce . 97) ($:type reduce . 97) ($:connector reduce . 97) (
    $:expandable reduce . 97) ($:block reduce . 97) ($:record reduce . 97) (
    $:operator reduce . 97) ($:model reduce . 97) ($:class reduce . 97) (
    $:partial reduce . 97) ($:encapsulated reduce . 97) ($:stream reduce . 97)
    ($:flow reduce . 97) ($:constant reduce . 97) ($:parameter reduce . 97) (
    $:discrete reduce . 97) ($:output reduce . 97) ($:input reduce . 97) (
    $ident reduce . 97) ($:. reduce . 97) ($:replaceable reduce . 97)) ((
    $ident shift . 26) ($:. shift . 27) (ident shift . 28) ($:output shift . 
    70) ($:input shift . 71) ($:constant shift . 72) ($:parameter shift . 73) 
    ($:discrete shift . 74) ($:stream shift . 75) ($:flow shift . 76) (
    $:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package 
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) (
    $:model shift . 11) ($:class shift . 12) (name shift . 82) (type-prefix-3 
    shift . 77) (type-prefix-2 shift . 78) (type-prefix-1 shift . 79) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (type-specifier shift 
    . 83) (type-prefix shift . 84) (class-prefixes shift . 15) ($:encapsulated
    shift . 16) ($:replaceable shift . 87) (component-clause shift . 88) (
    class-definition shift . 89) (element-1 shift . 230)) ((#{$:;}# reduce . 
    84)) (($:constrainedby reduce . 103) (#{$:;}# reduce . 103)) ((
    $:constrainedby reduce . 102) (#{$:;}# reduce . 102)) (($:constrainedby 
    shift . 228) (constraining-clause shift . 229) (#{$:;}# reduce . 101)) ((
    $:. shift . 227) ($string reduce . 107) ($:annotation reduce . 107) (
    #{$:;}# reduce . 107)) (($:= shift . 226) ($:annotation reduce . 303) (
    $string reduce . 303) ($:. reduce . 303) (#{$:;}# reduce . 303)) (($string
    shift . 50) (string shift . 51) (string-cat shift . 52) (string-comment 
    shift . 184) (comment shift . 225) (#{$:;}# reduce . 340) ($:annotation 
    reduce . 340)) (($:. shift . 59) ($:annotation shift . 114) (annotation 
    shift . 223) (#{$:\x28;}# shift . 66) (class-modification shift . 224) (
    #{$:;}# reduce . 116)) (($ident shift . 26) (ident shift . 136) (
    declaration shift . 137) (component-declaration shift . 138) (
    component-list shift . 221) (#{$:\x5b;}# shift . 134) (array-subscripts 
    shift . 222)) (($ident shift . 26) ($string shift . 50) ($float shift . 
    195) ($fixed shift . 196) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}#
    shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der 
    shift . 200) (name shift . 201) ($:true shift . 202) ($:false shift . 203)
    (string shift . 204) (unsigned-number shift . 205) (primary shift . 206) 
    (factor shift . 207) (term shift . 208) (arithmetic-expression shift . 209
    ) ($:not shift . 210) (relation shift . 211) (logical-factor shift . 212) 
    (logical-term shift . 213) (logical-expression shift . 214) ($:if shift . 
    215) (simple-expression shift . 216) (expression shift . 217) ($:: shift 
    . 218) (subscript shift . 219) (array-subscript-list shift . 220)) ((
    $ident shift . 26) (ident shift . 136) (declaration shift . 137) (
    component-declaration shift . 138) (component-list shift . 194)) ((
    #{$:\x5b;}# shift . 134) (array-subscripts shift . 192) ($P7 shift . 193) 
    ($string reduce . 144) ($:annotation reduce . 144) ($:if reduce . 144) (
    $:, reduce . 144) (#{$:;}# reduce . 144) ($:constrainedby reduce . 144) (
    #{$:\x29;}# reduce . 144) (#{$:\x28;}# reduce . 144) ($::= reduce . 144) (
    $:= reduce . 144)) (($:if shift . 189) (condition-attribute shift . 190) (
    $string shift . 50) (string shift . 51) (string-cat shift . 52) (
    string-comment shift . 184) (comment shift . 191) ($:, reduce . 340) (
    #{$:;}# reduce . 340) ($:constrainedby reduce . 340) ($:annotation reduce 
    . 340)) (($:, reduce . 138) (#{$:;}# reduce . 138) ($:constrainedby reduce
    . 138)) (($:, shift . 188) (#{$:;}# reduce . 122) ($:constrainedby reduce
    . 122)) (($string shift . 50) (string shift . 51) (string-cat shift . 52)
    (string-comment shift . 184) (comment shift . 185) ($:. shift . 59) (
    #{$:\x28;}# shift . 66) (class-modification shift . 186) (#{$:\x5b;}# 
    shift . 134) (array-subscripts shift . 187) (#{$:;}# reduce . 340) (
    $:constrainedby reduce . 340) ($:stream reduce . 340) ($:flow reduce . 340
    ) ($:constant reduce . 340) ($:parameter reduce . 340) ($:discrete reduce 
    . 340) ($:output reduce . 340) ($:input reduce . 340) (#{$:\x29;}# reduce 
    . 340) ($:, reduce . 340) ($:annotation reduce . 340)) (($:output shift . 
    70) ($:input shift . 71) (type-prefix-3 shift . 183) ($ident reduce . 124)
    ($:. reduce . 124)) (($ident reduce . 125) ($:. reduce . 125)) (($ident 
    reduce . 126) ($:. reduce . 126)) (($ident shift . 26) (ident shift . 178)
    (enumeration-literal shift . 179) ($:: shift . 180) (enum-list shift . 
    181) (filler-1 shift . 182) (#{$:\x29;}# reduce . 41)) (($ident shift . 26
    ) ($:. shift . 27) (ident shift . 28) (name shift . 177)) (($ident shift 
    . 26) ($:. shift . 27) (ident shift . 28) ($:output shift . 70) ($:input 
    shift . 71) ($:constant shift . 72) ($:parameter shift . 73) ($:discrete 
    shift . 74) ($:stream shift . 75) ($:flow shift . 76) ($:function shift . 
    1) ($:pure shift . 2) ($:impure shift . 3) ($:package shift . 4) ($:type 
    shift . 5) ($:connector shift . 6) ($:expandable shift . 7) ($:block shift
    . 8) ($:record shift . 9) ($:operator shift . 10) ($:model shift . 11) (
    $:class shift . 12) (name shift . 82) (type-prefix-3 shift . 77) (
    type-prefix-2 shift . 78) (type-prefix-1 shift . 79) (class-prefixes-1 
    shift . 13) ($:partial shift . 14) (type-specifier shift . 83) (
    type-prefix shift . 84) (class-prefixes shift . 15) ($:encapsulated shift 
    . 16) ($:extends shift . 85) ($:import shift . 86) ($:replaceable shift . 
    87) (component-clause shift . 88) (class-definition shift . 89) (element-1
    shift . 90) ($:outer shift . 91) ($:inner shift . 92) ($:final shift . 93
    ) ($:redeclare shift . 94) (extends-clause shift . 95) (import-clause 
    shift . 96) (element shift . 97) (element-list shift . 98) (composition 
    shift . 176)) (($:function shift . 1) ($:pure shift . 2) ($:impure shift 
    . 3) ($:package shift . 4) ($:type shift . 5) ($:connector shift . 6) (
    $:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (class-prefixes shift 
    . 174) (short-class-definition shift . 175)) ((#{$:\x28;}# shift . 66) (
    $::= shift . 169) ($:= shift . 170) (class-modification shift . 171) (
    modification shift . 172) ($P9 shift . 173) ($:. shift . 59) (#{$:\x29;}# 
    reduce . 165) ($:, reduce . 165) ($string reduce . 165)) ((#{$:\x29;}# 
    reduce . 163) ($:, reduce . 163)) ((#{$:\x29;}# reduce . 162) ($:, reduce 
    . 162)) (($:each shift . 167) ($P10 shift . 168) ($:function reduce . 168)
    ($:pure reduce . 168) ($:impure reduce . 168) ($:package reduce . 168) (
    $:type reduce . 168) ($:connector reduce . 168) ($:expandable reduce . 168
    ) ($:block reduce . 168) ($:record reduce . 168) ($:operator reduce . 168)
    ($:model reduce . 168) ($:class reduce . 168) ($:partial reduce . 168) (
    $:stream reduce . 168) ($:flow reduce . 168) ($:constant reduce . 168) (
    $:parameter reduce . 168) ($:discrete reduce . 168) ($:output reduce . 168
    ) ($:input reduce . 168) ($:replaceable reduce . 168) ($:final reduce . 
    168)) ((#{$:\x29;}# reduce . 161) ($:, reduce . 161)) (($ident shift . 26)
    ($:. shift . 27) (ident shift . 28) ($:replaceable shift . 147) (name 
    shift . 148) (element-replaceable shift . 149) (element-modification shift
    . 150) (elt-mod-or-repl-1 shift . 166)) (($:final shift . 164) ($ident 
    shift . 26) ($:. shift . 27) (ident shift . 28) ($:replaceable shift . 147
    ) (name shift . 148) (element-replaceable shift . 149) (
    element-modification shift . 150) (elt-mod-or-repl-1 shift . 165)) ((
    #{$:\x29;}# reduce . 157) ($:, reduce . 157)) ((#{$:\x29;}# reduce . 156) 
    ($:, reduce . 156)) ((#{$:\x29;}# reduce . 154) ($:, reduce . 154)) ((
    #{$:\x29;}# shift . 162) ($:, shift . 163)) (($:import reduce . 153) (
    $:extends reduce . 153) ($:function reduce . 153) ($:pure reduce . 153) (
    $:impure reduce . 153) ($:package reduce . 153) ($:type reduce . 153) (
    $:connector reduce . 153) ($:expandable reduce . 153) ($:block reduce . 
    153) ($:record reduce . 153) ($:operator reduce . 153) ($:model reduce . 
    153) ($:class reduce . 153) ($:partial reduce . 153) ($:encapsulated 
    reduce . 153) ($:stream reduce . 153) ($:flow reduce . 153) ($:constant 
    reduce . 153) ($:parameter reduce . 153) ($:discrete reduce . 153) (
    $:output reduce . 153) ($:input reduce . 153) ($ident reduce . 153) ($:. 
    reduce . 153) ($:replaceable reduce . 153) ($:outer reduce . 153) ($:inner
    reduce . 153) ($:final reduce . 153) ($:redeclare reduce . 153) ($string 
    reduce . 153) ($:annotation reduce . 153) ($:= reduce . 153) (#{$:;}# 
    reduce . 153) ($:constrainedby reduce . 153) (#{$:\x29;}# reduce . 153) (
    $:, reduce . 153) ($:if reduce . 153)) (($:end shift . 161)) (($ident 
    shift . 26) (ident shift . 401)) (($:import reduce . 152) ($:extends 
    reduce . 152) ($:function reduce . 152) ($:pure reduce . 152) ($:impure 
    reduce . 152) ($:package reduce . 152) ($:type reduce . 152) ($:connector 
    reduce . 152) ($:expandable reduce . 152) ($:block reduce . 152) ($:record
    reduce . 152) ($:operator reduce . 152) ($:model reduce . 152) ($:class 
    reduce . 152) ($:partial reduce . 152) ($:encapsulated reduce . 152) (
    $:stream reduce . 152) ($:flow reduce . 152) ($:constant reduce . 152) (
    $:parameter reduce . 152) ($:discrete reduce . 152) ($:output reduce . 152
    ) ($:input reduce . 152) ($ident reduce . 152) ($:. reduce . 152) (
    $:replaceable reduce . 152) ($:outer reduce . 152) ($:inner reduce . 152) 
    ($:final reduce . 152) ($:redeclare reduce . 152) ($string reduce . 152) (
    $:annotation reduce . 152) ($:= reduce . 152) (#{$:;}# reduce . 152) (
    $:constrainedby reduce . 152) (#{$:\x29;}# reduce . 152) ($:, reduce . 152
    ) ($:if reduce . 152)) (($ident shift . 26) ($:. shift . 27) (ident shift 
    . 28) ($:replaceable shift . 147) (name shift . 148) (element-replaceable 
    shift . 149) (element-modification shift . 150) ($:redeclare shift . 151) 
    (elt-mod-or-repl-1 shift . 152) ($:final shift . 153) ($:each shift . 154)
    (element-redeclaration shift . 155) (element-modification-or-replaceable 
    shift . 156) (argument shift . 400)) (($ident shift . 26) ($:. shift . 27)
    (ident shift . 28) ($:replaceable shift . 147) (name shift . 148) (
    element-replaceable shift . 149) (element-modification shift . 150) (
    elt-mod-or-repl-1 shift . 399)) ((#{$:\x29;}# reduce . 159) ($:, reduce . 
    159)) ((#{$:\x29;}# reduce . 160) ($:, reduce . 160)) (($:function reduce 
    . 169) ($:pure reduce . 169) ($:impure reduce . 169) ($:package reduce . 
    169) ($:type reduce . 169) ($:connector reduce . 169) ($:expandable reduce
    . 169) ($:block reduce . 169) ($:record reduce . 169) ($:operator reduce 
    . 169) ($:model reduce . 169) ($:class reduce . 169) ($:partial reduce . 
    169) ($:stream reduce . 169) ($:flow reduce . 169) ($:constant reduce . 
    169) ($:parameter reduce . 169) ($:discrete reduce . 169) ($:output reduce
    . 169) ($:input reduce . 169) ($:replaceable reduce . 169) ($:final 
    reduce . 169)) (($:final shift . 397) ($P11 shift . 398) ($:function 
    reduce . 170) ($:pure reduce . 170) ($:impure reduce . 170) ($:package 
    reduce . 170) ($:type reduce . 170) ($:connector reduce . 170) (
    $:expandable reduce . 170) ($:block reduce . 170) ($:record reduce . 170) 
    ($:operator reduce . 170) ($:model reduce . 170) ($:class reduce . 170) (
    $:partial reduce . 170) ($:stream reduce . 170) ($:flow reduce . 170) (
    $:constant reduce . 170) ($:parameter reduce . 170) ($:discrete reduce . 
    170) ($:output reduce . 170) ($:input reduce . 170) ($:replaceable reduce 
    . 170)) (($ident shift . 26) ($string shift . 50) ($float shift . 195) (
    $fixed shift . 196) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift
    . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der shift 
    . 200) (name shift . 201) ($:true shift . 202) ($:false shift . 203) (
    string shift . 204) (unsigned-number shift . 205) (primary shift . 206) (
    factor shift . 207) (term shift . 208) (arithmetic-expression shift . 209)
    ($:not shift . 210) (relation shift . 211) (logical-factor shift . 212) (
    logical-term shift . 213) (logical-expression shift . 214) ($:if shift . 
    215) (simple-expression shift . 216) (expression shift . 396)) (($ident 
    shift . 26) ($string shift . 50) ($float shift . 195) ($fixed shift . 196)
    ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (
    #{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (
    name shift . 201) ($:true shift . 202) ($:false shift . 203) (string shift
    . 204) (unsigned-number shift . 205) (primary shift . 206) (factor shift 
    . 207) (term shift . 208) (arithmetic-expression shift . 209) ($:not shift
    . 210) (relation shift . 211) (logical-factor shift . 212) (logical-term 
    shift . 213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 395)) (($:= shift . 394
    ) ($string reduce . 149) (#{$:\x29;}# reduce . 149) ($:, reduce . 149) (
    $:annotation reduce . 149) ($:if reduce . 149) (#{$:;}# reduce . 149) (
    $:constrainedby reduce . 149)) (($string reduce . 166) (#{$:\x29;}# reduce
    . 166) ($:, reduce . 166)) (($string shift . 50) (string shift . 51) (
    string-cat shift . 52) (string-comment shift . 393) ($:, reduce . 340) (
    #{$:\x29;}# reduce . 340)) (($ident shift . 26) (ident shift . 391) (
    short-class-specifier shift . 392)) (($:output shift . 70) ($:input shift 
    . 71) ($:constant shift . 72) ($:parameter shift . 73) ($:discrete shift 
    . 74) ($:stream shift . 75) ($:flow shift . 76) (type-prefix-3 shift . 77)
    (type-prefix-2 shift . 78) (type-prefix-1 shift . 79) (type-prefix shift 
    . 389) (component-clause1 shift . 390)) (($:end shift . 388)) (($:, shift 
    . 387) ($:. shift . 59)) (($string shift . 50) (string shift . 51) (
    string-cat shift . 52) (string-comment shift . 184) (comment shift . 386) 
    (#{$:\x29;}# reduce . 340) ($:, reduce . 340) ($:annotation reduce . 340))
    ((#{$:\x29;}# reduce . 49) ($:, reduce . 49)) ((#{$:\x29;}# reduce . 43))
    (($:, shift . 385) (#{$:\x29;}# reduce . 42)) ((#{$:\x29;}# shift . 384))
    (($ident reduce . 123) ($:. reduce . 123)) (($:annotation shift . 114) (
    annotation shift . 383) (#{$:;}# reduce . 339) ($:, reduce . 339) (
    $:constrainedby reduce . 339) (#{$:\x29;}# reduce . 339) ($:stream reduce 
    . 339) ($:flow reduce . 339) ($:constant reduce . 339) ($:parameter reduce
    . 339) ($:discrete reduce . 339) ($:output reduce . 339) ($:input reduce 
    . 339)) ((#{$:;}# reduce . 39) ($:constrainedby reduce . 39) ($:stream 
    reduce . 39) ($:flow reduce . 39) ($:constant reduce . 39) ($:parameter 
    reduce . 39) ($:discrete reduce . 39) ($:output reduce . 39) ($:input 
    reduce . 39) (#{$:\x29;}# reduce . 39) ($:, reduce . 39)) (($string shift 
    . 50) (string shift . 51) (string-cat shift . 52) (string-comment shift . 
    184) (comment shift . 382) (#{$:;}# reduce . 340) ($:constrainedby reduce 
    . 340) ($:stream reduce . 340) ($:flow reduce . 340) ($:constant reduce . 
    340) ($:parameter reduce . 340) ($:discrete reduce . 340) ($:output reduce
    . 340) ($:input reduce . 340) (#{$:\x29;}# reduce . 340) ($:, reduce . 
    340) ($:annotation reduce . 340)) ((#{$:\x28;}# shift . 66) (
    class-modification shift . 380) ($string shift . 50) (string shift . 51) (
    string-cat shift . 52) (string-comment shift . 184) (comment shift . 381) 
    (#{$:;}# reduce . 340) ($:constrainedby reduce . 340) ($:stream reduce . 
    340) ($:flow reduce . 340) ($:constant reduce . 340) ($:parameter reduce 
    . 340) ($:discrete reduce . 340) ($:output reduce . 340) ($:input reduce 
    . 340) (#{$:\x29;}# reduce . 340) ($:, reduce . 340) ($:annotation reduce 
    . 340)) (($ident shift . 26) (ident shift . 136) (declaration shift . 137)
    (component-declaration shift . 379)) (($ident shift . 26) ($string shift 
    . 50) ($float shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (
    #{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) ($:true 
    shift . 202) ($:false shift . 203) (string shift . 204) (unsigned-number 
    shift . 205) (primary shift . 206) (factor shift . 207) (term shift . 208)
    (arithmetic-expression shift . 209) ($:not shift . 210) (relation shift 
    . 211) (logical-factor shift . 212) (logical-term shift . 213) (
    logical-expression shift . 214) ($:if shift . 215) (simple-expression 
    shift . 216) (expression shift . 378)) (($string shift . 50) (string shift
    . 51) (string-cat shift . 52) (string-comment shift . 184) (comment shift
    . 377) ($:, reduce . 340) (#{$:;}# reduce . 340) ($:constrainedby reduce 
    . 340) ($:annotation reduce . 340)) (($:, reduce . 141) (#{$:;}# reduce . 
    141) ($:constrainedby reduce . 141)) ((#{$:\x28;}# reduce . 145) ($::= 
    reduce . 145) ($:= reduce . 145) ($string reduce . 145) ($:annotation 
    reduce . 145) ($:if reduce . 145) ($:, reduce . 145) (#{$:;}# reduce . 145
    ) ($:constrainedby reduce . 145) (#{$:\x29;}# reduce . 145)) ((#{$:\x28;}#
    shift . 66) ($::= shift . 169) ($:= shift . 170) (class-modification 
    shift . 171) (modification shift . 375) ($P8 shift . 376) (#{$:\x29;}# 
    reduce . 146) ($:constrainedby reduce . 146) (#{$:;}# reduce . 146) ($:, 
    reduce . 146) ($:if reduce . 146) ($:annotation reduce . 146) ($string 
    reduce . 146)) (($:, shift . 188) (#{$:;}# reduce . 121) ($:constrainedby 
    reduce . 121)) (($:: reduce . 348) ($:= reduce . 348) ($:or reduce . 348) 
    ($:and reduce . 348) ($:< reduce . 348) ($:<= reduce . 348) ($:> reduce . 
    348) ($:>= reduce . 348) ($:== reduce . 348) ($:<> reduce . 348) ($:.- 
    reduce . 348) ($:.+ reduce . 348) ($:- reduce . 348) ($:+ reduce . 348) (
    $:* reduce . 348) ($:/ reduce . 348) ($:.* reduce . 348) ($:./ reduce . 
    348) ($:.^ reduce . 348) ($:^ reduce . 348) (#{$:\x5d;}# reduce . 348) (
    $:, reduce . 348) ($:for reduce . 348) (#{$:;}# reduce . 348) (#{$:\x29;}#
    reduce . 348) ($:then reduce . 348) ($:loop reduce . 348) ($:else reduce 
    . 348) ($:elseif reduce . 348) ($string reduce . 348) ($:annotation reduce
    . 348) ($:constrainedby reduce . 348) (#{$:\x7d;}# reduce . 348) ($:end 
    reduce . 348) ($:elsewhen reduce . 348) ($:if reduce . 348)) (($:: reduce 
    . 347) ($:= reduce . 347) ($:or reduce . 347) ($:and reduce . 347) ($:< 
    reduce . 347) ($:<= reduce . 347) ($:> reduce . 347) ($:>= reduce . 347) (
    $:== reduce . 347) ($:<> reduce . 347) ($:.- reduce . 347) ($:.+ reduce . 
    347) ($:- reduce . 347) ($:+ reduce . 347) ($:* reduce . 347) ($:/ reduce 
    . 347) ($:.* reduce . 347) ($:./ reduce . 347) ($:.^ reduce . 347) ($:^ 
    reduce . 347) (#{$:\x5d;}# reduce . 347) ($:, reduce . 347) ($:for reduce 
    . 347) (#{$:;}# reduce . 347) (#{$:\x29;}# reduce . 347) ($:then reduce . 
    347) ($:loop reduce . 347) ($:else reduce . 347) ($:elseif reduce . 347) (
    $string reduce . 347) ($:annotation reduce . 347) ($:constrainedby reduce 
    . 347) (#{$:\x7d;}# reduce . 347) ($:end reduce . 347) ($:elsewhen reduce 
    . 347) ($:if reduce . 347)) (($string shift . 50) ($float shift . 195) (
    $fixed shift . 196) ($:. shift . 27) (#{$:\x7b;}# shift . 197) (
    #{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (
    name shift . 201) ($:true shift . 202) ($:false shift . 203) (string shift
    . 204) (unsigned-number shift . 205) (primary shift . 206) (factor shift 
    . 207) (term shift . 208) (arithmetic-expression shift . 209) ($:not shift
    . 210) (relation shift . 211) (logical-factor shift . 212) (logical-term 
    shift . 213) ($ident shift . 26) (logical-expression shift . 214) (ident 
    shift . 368) ($:if shift . 215) (simple-expression shift . 216) (
    expression shift . 369) ($:function shift . 370) (named-argument shift . 
    371) (named-arguments shift . 372) (function-argument shift . 373) (
    function-arguments shift . 374)) (($ident shift . 26) ($string shift . 50)
    ($float shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident shift 
    . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# 
    shift . 199) ($:der shift . 200) (name shift . 201) ($:true shift . 202) (
    $:false shift . 203) (string shift . 204) (unsigned-number shift . 205) (
    primary shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) (logical-term shift . 213) (
    logical-expression shift . 214) ($:if shift . 215) (simple-expression 
    shift . 216) (expression shift . 365) (expression-list shift . 366) (
    expression-list-list shift . 367)) (($ident shift . 26) ($string shift . 
    50) ($float shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (
    #{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) ($:true 
    shift . 202) ($:false shift . 203) (string shift . 204) (unsigned-number 
    shift . 205) (primary shift . 206) (factor shift . 207) (term shift . 208)
    (arithmetic-expression shift . 209) ($:not shift . 210) (relation shift 
    . 211) (logical-factor shift . 212) (logical-term shift . 213) (
    logical-expression shift . 214) ($:if shift . 215) (simple-expression 
    shift . 216) (expression shift . 291) ($:, shift . 292) (
    output-expression-list shift . 364)) ((#{$:\x28;}# shift . 289) (
    function-call-args shift . 363)) ((#{$:\x28;}# shift . 289) (
    function-call-args shift . 362) (#{$:\x5b;}# shift . 134) (
    array-subscripts shift . 307) ($:. shift . 59) (#{$:\x5d;}# reduce . 296) 
    ($:: reduce . 296) ($:^ reduce . 296) ($:.^ reduce . 296) ($:./ reduce . 
    296) ($:.* reduce . 296) ($:/ reduce . 296) ($:* reduce . 296) ($:+ reduce
    . 296) ($:- reduce . 296) ($:.+ reduce . 296) ($:.- reduce . 296) ($:<> 
    reduce . 296) ($:== reduce . 296) ($:>= reduce . 296) ($:> reduce . 296) (
    $:<= reduce . 296) ($:< reduce . 296) ($:and reduce . 296) ($:or reduce . 
    296) ($:, reduce . 296) ($:for reduce . 296) (#{$:;}# reduce . 296) (
    #{$:\x29;}# reduce . 296) ($:then reduce . 296) ($:loop reduce . 296) (
    $:else reduce . 296) ($:elseif reduce . 296) ($string reduce . 296) (
    $:annotation reduce . 296) ($:constrainedby reduce . 296) ($:= reduce . 
    296) (#{$:\x7d;}# reduce . 296) ($:end reduce . 296) ($:elsewhen reduce . 
    296) ($:if reduce . 296)) (($:= reduce . 293) ($:: reduce . 293) ($:^ 
    reduce . 293) ($:.^ reduce . 293) ($:./ reduce . 293) ($:.* reduce . 293) 
    ($:/ reduce . 293) ($:* reduce . 293) ($:+ reduce . 293) ($:- reduce . 293
    ) ($:.+ reduce . 293) ($:.- reduce . 293) ($:<> reduce . 293) ($:== reduce
    . 293) ($:>= reduce . 293) ($:> reduce . 293) ($:<= reduce . 293) ($:< 
    reduce . 293) ($:and reduce . 293) ($:or reduce . 293) (#{$:\x5d;}# reduce
    . 293) ($:, reduce . 293) ($:for reduce . 293) (#{$:;}# reduce . 293) (
    #{$:\x29;}# reduce . 293) ($:then reduce . 293) ($:loop reduce . 293) (
    $:else reduce . 293) ($:elseif reduce . 293) ($string reduce . 293) (
    $:annotation reduce . 293) ($:constrainedby reduce . 293) (#{$:\x7d;}# 
    reduce . 293) ($:end reduce . 293) ($:elsewhen reduce . 293) ($:if reduce 
    . 293)) (($:= reduce . 292) ($:: reduce . 292) ($:^ reduce . 292) ($:.^ 
    reduce . 292) ($:./ reduce . 292) ($:.* reduce . 292) ($:/ reduce . 292) (
    $:* reduce . 292) ($:+ reduce . 292) ($:- reduce . 292) ($:.+ reduce . 292
    ) ($:.- reduce . 292) ($:<> reduce . 292) ($:== reduce . 292) ($:>= reduce
    . 292) ($:> reduce . 292) ($:<= reduce . 292) ($:< reduce . 292) ($:and 
    reduce . 292) ($:or reduce . 292) (#{$:\x5d;}# reduce . 292) ($:, reduce 
    . 292) ($:for reduce . 292) (#{$:;}# reduce . 292) (#{$:\x29;}# reduce . 
    292) ($:then reduce . 292) ($:loop reduce . 292) ($:else reduce . 292) (
    $:elseif reduce . 292) ($string reduce . 292) ($:annotation reduce . 292) 
    ($:constrainedby reduce . 292) (#{$:\x7d;}# reduce . 292) ($:end reduce . 
    292) ($:elsewhen reduce . 292) ($:if reduce . 292)) (($:= reduce . 291) (
    $:: reduce . 291) ($:^ reduce . 291) ($:.^ reduce . 291) ($:./ reduce . 
    291) ($:.* reduce . 291) ($:/ reduce . 291) ($:* reduce . 291) ($:+ reduce
    . 291) ($:- reduce . 291) ($:.+ reduce . 291) ($:.- reduce . 291) ($:<> 
    reduce . 291) ($:== reduce . 291) ($:>= reduce . 291) ($:> reduce . 291) (
    $:<= reduce . 291) ($:< reduce . 291) ($:and reduce . 291) ($:or reduce . 
    291) (#{$:\x5d;}# reduce . 291) ($:, reduce . 291) ($:for reduce . 291) (
    #{$:;}# reduce . 291) (#{$:\x29;}# reduce . 291) ($:then reduce . 291) (
    $:loop reduce . 291) ($:else reduce . 291) ($:elseif reduce . 291) (
    $string reduce . 291) ($:annotation reduce . 291) ($:constrainedby reduce 
    . 291) (#{$:\x7d;}# reduce . 291) ($:end reduce . 291) ($:elsewhen reduce 
    . 291) ($:if reduce . 291)) (($:= reduce . 290) ($:: reduce . 290) ($:^ 
    reduce . 290) ($:.^ reduce . 290) ($:./ reduce . 290) ($:.* reduce . 290) 
    ($:/ reduce . 290) ($:* reduce . 290) ($:+ reduce . 290) ($:- reduce . 290
    ) ($:.+ reduce . 290) ($:.- reduce . 290) ($:<> reduce . 290) ($:== reduce
    . 290) ($:>= reduce . 290) ($:> reduce . 290) ($:<= reduce . 290) ($:< 
    reduce . 290) ($:and reduce . 290) ($:or reduce . 290) (#{$:\x5d;}# reduce
    . 290) ($:, reduce . 290) ($:for reduce . 290) (#{$:;}# reduce . 290) (
    #{$:\x29;}# reduce . 290) ($:then reduce . 290) ($:loop reduce . 290) (
    $:else reduce . 290) ($:elseif reduce . 290) ($string reduce . 290) (
    $:annotation reduce . 290) ($:constrainedby reduce . 290) (#{$:\x7d;}# 
    reduce . 290) ($:end reduce . 290) ($:elsewhen reduce . 290) ($:if reduce 
    . 290)) (($:: reduce . 287) ($:= reduce . 287) ($:or reduce . 287) ($:and 
    reduce . 287) ($:< reduce . 287) ($:<= reduce . 287) ($:> reduce . 287) (
    $:>= reduce . 287) ($:== reduce . 287) ($:<> reduce . 287) ($:.- reduce . 
    287) ($:.+ reduce . 287) ($:- reduce . 287) ($:+ reduce . 287) ($:* reduce
    . 287) ($:/ reduce . 287) ($:.* reduce . 287) ($:./ reduce . 287) ($:.^ 
    reduce . 287) ($:^ reduce . 287) (#{$:\x5d;}# reduce . 287) ($:, reduce . 
    287) ($:for reduce . 287) (#{$:;}# reduce . 287) (#{$:\x29;}# reduce . 287
    ) ($:then reduce . 287) ($:loop reduce . 287) ($:else reduce . 287) (
    $:elseif reduce . 287) ($string reduce . 287) ($:annotation reduce . 287) 
    ($:constrainedby reduce . 287) (#{$:\x7d;}# reduce . 287) ($:end reduce . 
    287) ($:elsewhen reduce . 287) ($:if reduce . 287)) (($:^ shift . 360) (
    $:.^ shift . 361) ($:= reduce . 281) ($:: reduce . 281) ($:./ reduce . 281
    ) ($:.* reduce . 281) ($:/ reduce . 281) ($:* reduce . 281) ($:+ reduce . 
    281) ($:- reduce . 281) ($:.+ reduce . 281) ($:.- reduce . 281) ($:<> 
    reduce . 281) ($:== reduce . 281) ($:>= reduce . 281) ($:> reduce . 281) (
    $:<= reduce . 281) ($:< reduce . 281) ($:and reduce . 281) ($:or reduce . 
    281) (#{$:\x5d;}# reduce . 281) ($:, reduce . 281) ($:for reduce . 281) (
    #{$:;}# reduce . 281) (#{$:\x29;}# reduce . 281) ($:then reduce . 281) (
    $:loop reduce . 281) ($:else reduce . 281) ($:elseif reduce . 281) (
    $string reduce . 281) ($:annotation reduce . 281) ($:constrainedby reduce 
    . 281) (#{$:\x7d;}# reduce . 281) ($:end reduce . 281) ($:elsewhen reduce 
    . 281) ($:if reduce . 281)) (($:./ shift . 355) ($:.* shift . 356) ($:/ 
    shift . 357) ($:* shift . 358) (mul-op shift . 359) ($:: reduce . 275) (
    $:= reduce . 275) ($:or reduce . 275) ($:and reduce . 275) ($:< reduce . 
    275) ($:<= reduce . 275) ($:> reduce . 275) ($:>= reduce . 275) ($:== 
    reduce . 275) ($:<> reduce . 275) ($:.- reduce . 275) ($:.+ reduce . 275) 
    ($:- reduce . 275) ($:+ reduce . 275) (#{$:\x5d;}# reduce . 275) ($:, 
    reduce . 275) ($:for reduce . 275) (#{$:;}# reduce . 275) (#{$:\x29;}# 
    reduce . 275) ($:then reduce . 275) ($:loop reduce . 275) ($:else reduce 
    . 275) ($:elseif reduce . 275) ($string reduce . 275) ($:annotation reduce
    . 275) ($:constrainedby reduce . 275) (#{$:\x7d;}# reduce . 275) ($:end 
    reduce . 275) ($:elsewhen reduce . 275) ($:if reduce . 275)) (($:.- shift 
    . 350) ($:.+ shift . 351) ($:- shift . 352) ($:+ shift . 353) (add-op 
    shift . 354) ($:= reduce . 267) ($:: reduce . 267) ($:<> reduce . 267) (
    $:== reduce . 267) ($:>= reduce . 267) ($:> reduce . 267) ($:<= reduce . 
    267) ($:< reduce . 267) ($:and reduce . 267) ($:or reduce . 267) (
    #{$:\x5d;}# reduce . 267) ($:, reduce . 267) ($:for reduce . 267) (#{$:;}#
    reduce . 267) (#{$:\x29;}# reduce . 267) ($:then reduce . 267) ($:loop 
    reduce . 267) ($:else reduce . 267) ($:elseif reduce . 267) ($string 
    reduce . 267) ($:annotation reduce . 267) ($:constrainedby reduce . 267) (
    #{$:\x7d;}# reduce . 267) ($:end reduce . 267) ($:elsewhen reduce . 267) (
    $:if reduce . 267)) (($ident shift . 26) ($string shift . 50) ($float 
    shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident shift . 28) (
    #{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 
    199) ($:der shift . 200) (name shift . 201) ($:true shift . 202) ($:false 
    shift . 203) (string shift . 204) (unsigned-number shift . 205) (primary 
    shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) (relation shift . 349)) (($:<> shift . 
    342) ($:== shift . 343) ($:>= shift . 344) ($:> shift . 345) ($:<= shift 
    . 346) ($:< shift . 347) (rel-op shift . 348) ($:: reduce . 265) ($:= 
    reduce . 265) ($:or reduce . 265) ($:and reduce . 265) (#{$:\x5d;}# reduce
    . 265) ($:, reduce . 265) ($:for reduce . 265) (#{$:;}# reduce . 265) (
    #{$:\x29;}# reduce . 265) ($:then reduce . 265) ($:loop reduce . 265) (
    $:else reduce . 265) ($:elseif reduce . 265) ($string reduce . 265) (
    $:annotation reduce . 265) ($:constrainedby reduce . 265) (#{$:\x7d;}# 
    reduce . 265) ($:end reduce . 265) ($:elsewhen reduce . 265) ($:if reduce 
    . 265)) (($:= reduce . 263) ($:: reduce . 263) ($:and reduce . 263) ($:or 
    reduce . 263) (#{$:\x5d;}# reduce . 263) ($:, reduce . 263) ($:for reduce 
    . 263) (#{$:;}# reduce . 263) (#{$:\x29;}# reduce . 263) ($:then reduce . 
    263) ($:loop reduce . 263) ($:else reduce . 263) ($:elseif reduce . 263) (
    $string reduce . 263) ($:annotation reduce . 263) ($:constrainedby reduce 
    . 263) (#{$:\x7d;}# reduce . 263) ($:end reduce . 263) ($:elsewhen reduce 
    . 263) ($:if reduce . 263)) (($:and shift . 341) ($:: reduce . 261) ($:= 
    reduce . 261) ($:or reduce . 261) (#{$:\x5d;}# reduce . 261) ($:, reduce 
    . 261) ($:for reduce . 261) (#{$:;}# reduce . 261) (#{$:\x29;}# reduce . 
    261) ($:then reduce . 261) ($:loop reduce . 261) ($:else reduce . 261) (
    $:elseif reduce . 261) ($string reduce . 261) ($:annotation reduce . 261) 
    ($:constrainedby reduce . 261) (#{$:\x7d;}# reduce . 261) ($:end reduce . 
    261) ($:elsewhen reduce . 261) ($:if reduce . 261)) (($:: shift . 339) (
    $:or shift . 340) ($:= reduce . 258) (#{$:\x5d;}# reduce . 258) ($:, 
    reduce . 258) ($:for reduce . 258) (#{$:;}# reduce . 258) (#{$:\x29;}# 
    reduce . 258) ($:then reduce . 258) ($:loop reduce . 258) ($:else reduce 
    . 258) ($:elseif reduce . 258) ($string reduce . 258) ($:annotation reduce
    . 258) ($:constrainedby reduce . 258) (#{$:\x7d;}# reduce . 258) ($:end 
    reduce . 258) ($:elsewhen reduce . 258) ($:if reduce . 258)) (($ident 
    shift . 26) ($string shift . 50) ($float shift . 195) ($fixed shift . 196)
    ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (
    #{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (
    name shift . 201) ($:true shift . 202) ($:false shift . 203) (string shift
    . 204) (unsigned-number shift . 205) (primary shift . 206) (factor shift 
    . 207) (term shift . 208) (arithmetic-expression shift . 209) ($:not shift
    . 210) (relation shift . 211) (logical-factor shift . 212) (logical-term 
    shift . 213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 338)) ((#{$:\x5d;}# 
    reduce . 253) ($:, reduce . 253) ($:for reduce . 253) (#{$:;}# reduce . 
    253) (#{$:\x29;}# reduce . 253) ($:then reduce . 253) ($:loop reduce . 253
    ) ($:else reduce . 253) ($:elseif reduce . 253) ($string reduce . 253) (
    $:annotation reduce . 253) ($:constrainedby reduce . 253) (#{$:\x7d;}# 
    reduce . 253) ($:end reduce . 253) ($:elsewhen reduce . 253) ($:if reduce 
    . 253)) ((#{$:\x5d;}# reduce . 337) ($:, reduce . 337)) ((#{$:\x5d;}# 
    reduce . 336) ($:, reduce . 336)) ((#{$:\x5d;}# reduce . 334) ($:, reduce 
    . 334)) ((#{$:\x5d;}# shift . 336) ($:, shift . 337)) (($:, shift . 188) (
    #{$:;}# reduce . 120) ($:constrainedby reduce . 120)) (($ident shift . 26)
    (ident shift . 136) (declaration shift . 137) (component-declaration 
    shift . 138) (component-list shift . 335)) ((#{$:;}# reduce . 115)) ((
    $:annotation shift . 114) (annotation shift . 334) (#{$:;}# reduce . 114))
    ((#{$:;}# reduce . 104)) (($ident shift . 26) ($:. shift . 27) (ident 
    shift . 28) (name shift . 333)) (($ident shift . 26) (ident shift . 64) (
    #{$:\x7b;}# shift . 330) ($:* shift . 331) (import-clause-2 shift . 332)) 
    (($ident shift . 26) ($:. shift . 27) (ident shift . 28) (name shift . 329
    )) (($string shift . 50) (string shift . 51) (string-cat shift . 52) (
    string-comment shift . 184) (comment shift . 328) (#{$:;}# reduce . 340) (
    $:annotation reduce . 340)) ((#{$:;}# reduce . 83)) (($:function reduce . 
    95) ($:pure reduce . 95) ($:impure reduce . 95) ($:package reduce . 95) (
    $:type reduce . 95) ($:connector reduce . 95) ($:expandable reduce . 95) (
    $:block reduce . 95) ($:record reduce . 95) ($:operator reduce . 95) (
    $:model reduce . 95) ($:class reduce . 95) ($:partial reduce . 95) (
    $:encapsulated reduce . 95) ($:stream reduce . 95) ($:flow reduce . 95) (
    $:constant reduce . 95) ($:parameter reduce . 95) ($:discrete reduce . 95)
    ($:output reduce . 95) ($:input reduce . 95) ($ident reduce . 95) ($:. 
    reduce . 95) ($:replaceable reduce . 95)) (($ident shift . 26) ($:. shift 
    . 27) (ident shift . 28) ($:output shift . 70) ($:input shift . 71) (
    $:constant shift . 72) ($:parameter shift . 73) ($:discrete shift . 74) (
    $:stream shift . 75) ($:flow shift . 76) ($:function shift . 1) ($:pure 
    shift . 2) ($:impure shift . 3) ($:package shift . 4) ($:type shift . 5) (
    $:connector shift . 6) ($:expandable shift . 7) ($:block shift . 8) (
    $:record shift . 9) ($:operator shift . 10) ($:model shift . 11) ($:class 
    shift . 12) (name shift . 82) (type-prefix-3 shift . 77) (type-prefix-2 
    shift . 78) (type-prefix-1 shift . 79) (class-prefixes-1 shift . 13) (
    $:partial shift . 14) (type-specifier shift . 83) (type-prefix shift . 84)
    (class-prefixes shift . 15) ($:encapsulated shift . 16) ($:replaceable 
    shift . 87) (component-clause shift . 88) (class-definition shift . 89) (
    element-1 shift . 327)) (($:function reduce . 89) ($:pure reduce . 89) (
    $:impure reduce . 89) ($:package reduce . 89) ($:type reduce . 89) (
    $:connector reduce . 89) ($:expandable reduce . 89) ($:block reduce . 89) 
    ($:record reduce . 89) ($:operator reduce . 89) ($:model reduce . 89) (
    $:class reduce . 89) ($:partial reduce . 89) ($:encapsulated reduce . 89) 
    ($:stream reduce . 89) ($:flow reduce . 89) ($:constant reduce . 89) (
    $:parameter reduce . 89) ($:discrete reduce . 89) ($:output reduce . 89) (
    $:input reduce . 89) ($ident reduce . 89) ($:. reduce . 89) ($:replaceable
    reduce . 89) ($:outer reduce . 89)) (($:outer shift . 325) ($P3 shift . 
    326) ($:function reduce . 90) ($:pure reduce . 90) ($:impure reduce . 90) 
    ($:package reduce . 90) ($:type reduce . 90) ($:connector reduce . 90) (
    $:expandable reduce . 90) ($:block reduce . 90) ($:record reduce . 90) (
    $:operator reduce . 90) ($:model reduce . 90) ($:class reduce . 90) (
    $:partial reduce . 90) ($:encapsulated reduce . 90) ($:stream reduce . 90)
    ($:flow reduce . 90) ($:constant reduce . 90) ($:parameter reduce . 90) (
    $:discrete reduce . 90) ($:output reduce . 90) ($:input reduce . 90) (
    $ident reduce . 90) ($:. reduce . 90) ($:replaceable reduce . 90)) (($:end
    reduce . 78) ($:annotation reduce . 78) ($:external reduce . 78) (
    $:equation reduce . 78) ($:algorithm reduce . 78) ($:initial reduce . 78) 
    ($:protected reduce . 78) ($:public reduce . 78) ($:import reduce . 78) (
    $:extends reduce . 78) ($:function reduce . 78) ($:pure reduce . 78) (
    $:impure reduce . 78) ($:package reduce . 78) ($:type reduce . 78) (
    $:connector reduce . 78) ($:expandable reduce . 78) ($:block reduce . 78) 
    ($:record reduce . 78) ($:operator reduce . 78) ($:model reduce . 78) (
    $:class reduce . 78) ($:partial reduce . 78) ($:encapsulated reduce . 78) 
    ($:stream reduce . 78) ($:flow reduce . 78) ($:constant reduce . 78) (
    $:parameter reduce . 78) ($:discrete reduce . 78) ($:output reduce . 78) (
    $:input reduce . 78) ($ident reduce . 78) ($:. reduce . 78) ($:replaceable
    reduce . 78) ($:outer reduce . 78) ($:inner reduce . 78) ($:final reduce 
    . 78) ($:redeclare reduce . 78)) (($:end reduce . 345)) ((#{$:;}# reduce 
    . 346) ($:, reduce . 346) ($:constrainedby reduce . 346) (#{$:\x29;}# 
    reduce . 346) ($:stream reduce . 346) ($:flow reduce . 346) ($:constant 
    reduce . 346) ($:parameter reduce . 346) ($:discrete reduce . 346) (
    $:output reduce . 346) ($:input reduce . 346)) (($:end reduce . 54)) ((
    #{$:;}# reduce . 72) ($:annotation reduce . 72) ($:. reduce . 72) ($ident 
    reduce . 72)) ((#{$:;}# shift . 322) ($:annotation shift . 114) (
    annotation shift . 323) ($:. shift . 241) ($ident shift . 26) (
    component-reference-1 shift . 242) (ident shift . 243) (
    component-reference shift . 244) (external-function-call shift . 324)) ((
    $ident shift . 26) (ident shift . 321)) (($:. shift . 320) (#{$:\x28;}# 
    reduce . 306) ($::= reduce . 306) ($:= reduce . 306) ($:, reduce . 306) (
    #{$:\x29;}# reduce . 306)) ((#{$:\x28;}# shift . 319) (#{$:\x5b;}# shift 
    . 134) (array-subscripts shift . 300) ($P13 shift . 301) ($:= reduce . 312
    ) ($:. reduce . 312)) (($:= shift . 318)) ((#{$:;}# shift . 316) (
    $:annotation shift . 114) (annotation shift . 317)) ((#{$:;}# shift . 315)
    ) (($:annotation reduce . 71) ($:end reduce . 71)) (($:end reduce . 53)) (
    ($:annotation reduce . 57) ($:equation reduce . 57) ($:algorithm reduce . 
    57) ($:initial reduce . 57) ($:protected reduce . 57) ($:public reduce . 
    57) ($:external reduce . 57) ($:end reduce . 57)) (($:annotation shift . 
    114) (annotation shift . 115) (opt-annotation shift . 314) ($:end reduce 
    . 344)) (($ident shift . 26) ($:. shift . 27) (ident shift . 28) ($:output
    shift . 70) ($:input shift . 71) ($:constant shift . 72) ($:parameter 
    shift . 73) ($:discrete shift . 74) ($:stream shift . 75) ($:flow shift . 
    76) ($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) (
    $:package shift . 4) ($:type shift . 5) ($:connector shift . 6) (
    $:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (name 
    shift . 82) (type-prefix-3 shift . 77) (type-prefix-2 shift . 78) (
    type-prefix-1 shift . 79) (class-prefixes-1 shift . 13) ($:partial shift 
    . 14) (type-specifier shift . 83) (type-prefix shift . 84) (class-prefixes
    shift . 15) ($:encapsulated shift . 16) ($:extends shift . 85) ($:import 
    shift . 86) ($:replaceable shift . 87) (component-clause shift . 88) (
    class-definition shift . 89) (element-1 shift . 90) ($:outer shift . 91) (
    $:inner shift . 92) ($:final shift . 93) ($:redeclare shift . 94) (
    extends-clause shift . 95) (import-clause shift . 96) (element shift . 117
    ) ($:annotation reduce . 59) ($:public reduce . 59) ($:protected reduce . 
    59) ($:initial reduce . 59) ($:algorithm reduce . 59) ($:equation reduce 
    . 59) ($:external reduce . 59) ($:end reduce . 59)) (($ident shift . 26) (
    $:. shift . 27) (ident shift . 28) ($:output shift . 70) ($:input shift . 
    71) ($:constant shift . 72) ($:parameter shift . 73) ($:discrete shift . 
    74) ($:stream shift . 75) ($:flow shift . 76) ($:function shift . 1) (
    $:pure shift . 2) ($:impure shift . 3) ($:package shift . 4) ($:type shift
    . 5) ($:connector shift . 6) ($:expandable shift . 7) ($:block shift . 8)
    ($:record shift . 9) ($:operator shift . 10) ($:model shift . 11) (
    $:class shift . 12) (name shift . 82) (type-prefix-3 shift . 77) (
    type-prefix-2 shift . 78) (type-prefix-1 shift . 79) (class-prefixes-1 
    shift . 13) ($:partial shift . 14) (type-specifier shift . 83) (
    type-prefix shift . 84) (class-prefixes shift . 15) ($:encapsulated shift 
    . 16) ($:extends shift . 85) ($:import shift . 86) ($:replaceable shift . 
    87) (component-clause shift . 88) (class-definition shift . 89) (element-1
    shift . 90) ($:outer shift . 91) ($:inner shift . 92) ($:final shift . 93
    ) ($:redeclare shift . 94) (extends-clause shift . 95) (import-clause 
    shift . 96) (element shift . 117) ($:annotation reduce . 61) ($:public 
    reduce . 61) ($:protected reduce . 61) ($:initial reduce . 61) (
    $:algorithm reduce . 61) ($:equation reduce . 61) ($:external reduce . 61)
    ($:end reduce . 61)) (($string shift . 50) ($float shift . 195) ($fixed 
    shift . 196) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (
    #{$:\x28;}# shift . 199) ($:der shift . 200) ($:true shift . 202) ($:false
    shift . 203) (string shift . 204) (unsigned-number shift . 205) (primary 
    shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) ($ident shift . 26) (logical-term shift 
    . 213) ($:. shift . 27) (ident shift . 28) (logical-expression shift . 214
    ) ($:connect shift . 255) ($:when shift . 256) ($:for shift . 257) ($:if 
    shift . 258) (name shift . 259) (when-equation shift . 260) (
    connect-clause shift . 261) (for-equation shift . 262) (if-equation shift 
    . 263) (simple-expression shift . 264) (equation-1 shift . 265) (equation 
    shift . 266) (equation-list shift . 313) ($:annotation reduce . 181) (
    $:equation reduce . 181) ($:algorithm reduce . 181) ($:initial reduce . 
    181) ($:protected reduce . 181) ($:public reduce . 181) ($:external reduce
    . 181) ($:end reduce . 181)) (($ident shift . 26) ($:. shift . 241) (
    ident shift . 268) (component-reference-1 shift . 242) ($:when shift . 269
    ) ($:while shift . 270) ($:for shift . 271) ($:if shift . 272) (
    when-statement shift . 273) (while-statement shift . 274) (for-statement 
    shift . 275) (if-statement shift . 276) ($:return shift . 277) ($:break 
    shift . 278) (#{$:\x28;}# shift . 279) (component-reference shift . 280) (
    statement-1 shift . 281) (statement shift . 282) (statement-list shift . 
    312) ($:annotation reduce . 185) ($:equation reduce . 185) ($:algorithm 
    reduce . 185) ($:initial reduce . 185) ($:protected reduce . 185) (
    $:public reduce . 185) ($:external reduce . 185) ($:end reduce . 185)) ((
    #{$:\x28;}# shift . 311)) (($ident shift . 26) ($string shift . 50) (
    $float shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident shift . 
    28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift
    . 199) ($:der shift . 200) (name shift . 201) ($:true shift . 202) (
    $:false shift . 203) (string shift . 204) (unsigned-number shift . 205) (
    primary shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) (logical-term shift . 213) (
    logical-expression shift . 214) ($:if shift . 215) (simple-expression 
    shift . 216) (expression shift . 310)) (($ident shift . 26) (ident shift 
    . 295) (for-index shift . 296) (for-indices shift . 309)) (($ident shift 
    . 26) ($string shift . 50) ($float shift . 195) ($fixed shift . 196) ($:. 
    shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# 
    shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 
    201) ($:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 209) ($:not shift . 210) 
    (relation shift . 211) (logical-factor shift . 212) (logical-term shift . 
    213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 308)) (($:. shift . 59)
    (#{$:\x28;}# shift . 289) (function-call-args shift . 306) (#{$:\x5b;}# 
    shift . 134) (array-subscripts shift . 307) ($:= reduce . 296) ($:: reduce
    . 296) ($:^ reduce . 296) ($:.^ reduce . 296) ($:./ reduce . 296) ($:.* 
    reduce . 296) ($:/ reduce . 296) ($:* reduce . 296) ($:+ reduce . 296) (
    $:- reduce . 296) ($:.+ reduce . 296) ($:.- reduce . 296) ($:<> reduce . 
    296) ($:== reduce . 296) ($:>= reduce . 296) ($:> reduce . 296) ($:<= 
    reduce . 296) ($:< reduce . 296) ($:and reduce . 296) ($:or reduce . 296))
    ((#{$:;}# reduce . 194) ($string reduce . 194) ($:annotation reduce . 194
    )) ((#{$:;}# reduce . 193) ($string reduce . 193) ($:annotation reduce . 
    193)) ((#{$:;}# reduce . 192) ($string reduce . 192) ($:annotation reduce 
    . 192)) ((#{$:;}# reduce . 191) ($string reduce . 191) ($:annotation 
    reduce . 191)) (($:= shift . 305)) (($string shift . 50) (string shift . 
    51) (string-cat shift . 52) (string-comment shift . 184) (comment shift . 
    304) (#{$:;}# reduce . 340) ($:annotation reduce . 340)) ((#{$:;}# shift 
    . 303)) (($string shift . 50) ($float shift . 195) ($fixed shift . 196) (
    #{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 
    199) ($:der shift . 200) ($:true shift . 202) ($:false shift . 203) (
    string shift . 204) (unsigned-number shift . 205) (primary shift . 206) (
    factor shift . 207) (term shift . 208) (arithmetic-expression shift . 209)
    ($:not shift . 210) (relation shift . 211) (logical-factor shift . 212) (
    $ident shift . 26) (logical-term shift . 213) ($:. shift . 27) (ident 
    shift . 28) (logical-expression shift . 214) ($:connect shift . 255) (
    $:when shift . 256) ($:for shift . 257) ($:if shift . 258) (name shift . 
    259) (when-equation shift . 260) (connect-clause shift . 261) (
    for-equation shift . 262) (if-equation shift . 263) (simple-expression 
    shift . 264) (equation-1 shift . 265) (equation shift . 302) ($:annotation
    reduce . 180) ($:equation reduce . 180) ($:algorithm reduce . 180) (
    $:initial reduce . 180) ($:protected reduce . 180) ($:public reduce . 180)
    ($:external reduce . 180) ($:end reduce . 180)) ((#{$:\x5b;}# shift . 134
    ) (array-subscripts shift . 300) ($P13 shift . 301) (#{$:\x29;}# reduce . 
    312) ($:, reduce . 312) (#{$:\x28;}# reduce . 312) ($::= reduce . 312) (
    $:. reduce . 312)) (($ident shift . 26) ($string shift . 50) ($float shift
    . 195) ($fixed shift . 196) ($:. shift . 27) (ident shift . 28) (
    #{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 
    199) ($:der shift . 200) (name shift . 201) ($:true shift . 202) ($:false 
    shift . 203) (string shift . 204) (unsigned-number shift . 205) (primary 
    shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) (logical-term shift . 213) (
    logical-expression shift . 214) ($:if shift . 215) (simple-expression 
    shift . 216) (expression shift . 299)) (($ident shift . 26) ($string shift
    . 50) ($float shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (
    #{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) ($:true 
    shift . 202) ($:false shift . 203) (string shift . 204) (unsigned-number 
    shift . 205) (primary shift . 206) (factor shift . 207) (term shift . 208)
    (arithmetic-expression shift . 209) ($:not shift . 210) (relation shift 
    . 211) (logical-factor shift . 212) (logical-term shift . 213) (
    logical-expression shift . 214) ($:if shift . 215) (simple-expression 
    shift . 216) (expression shift . 298)) (($ident shift . 26) (ident shift 
    . 295) (for-index shift . 296) (for-indices shift . 297)) (($ident shift 
    . 26) ($string shift . 50) ($float shift . 195) ($fixed shift . 196) ($:. 
    shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# 
    shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 
    201) ($:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 209) ($:not shift . 210) 
    (relation shift . 211) (logical-factor shift . 212) (logical-term shift . 
    213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 294)) ((#{$:;}# reduce 
    . 207) ($string reduce . 207) ($:annotation reduce . 207)) ((#{$:;}# 
    reduce . 206) ($string reduce . 206) ($:annotation reduce . 206)) ((
    #{$:;}# reduce . 205) ($string reduce . 205) ($:annotation reduce . 205)) 
    ((#{$:;}# reduce . 204) ($string reduce . 204) ($:annotation reduce . 204)
    ) ((#{$:;}# reduce . 203) ($string reduce . 203) ($:annotation reduce . 
    203)) ((#{$:;}# reduce . 202) ($string reduce . 202) ($:annotation reduce 
    . 202)) (($ident shift . 26) ($string shift . 50) ($float shift . 195) (
    $fixed shift . 196) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift
    . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der shift 
    . 200) (name shift . 201) ($:true shift . 202) ($:false shift . 203) (
    string shift . 204) (unsigned-number shift . 205) (primary shift . 206) (
    factor shift . 207) (term shift . 208) (arithmetic-expression shift . 209)
    ($:not shift . 210) (relation shift . 211) (logical-factor shift . 212) (
    logical-term shift . 213) (logical-expression shift . 214) ($:if shift . 
    215) (simple-expression shift . 216) (expression shift . 291) ($:, shift 
    . 292) (output-expression-list shift . 293)) (($::= shift . 288) (
    #{$:\x28;}# shift . 289) (function-call-args shift . 290)) (($string shift
    . 50) (string shift . 51) (string-cat shift . 52) (string-comment shift 
    . 184) (comment shift . 287) (#{$:;}# reduce . 340) ($:annotation reduce 
    . 340)) ((#{$:;}# shift . 286)) (($ident shift . 26) ($:. shift . 241) (
    ident shift . 268) (component-reference-1 shift . 242) ($:when shift . 269
    ) ($:while shift . 270) ($:for shift . 271) ($:if shift . 272) (
    when-statement shift . 273) (while-statement shift . 274) (for-statement 
    shift . 275) (if-statement shift . 276) ($:return shift . 277) ($:break 
    shift . 278) (#{$:\x28;}# shift . 279) (component-reference shift . 280) (
    statement-1 shift . 281) (statement shift . 285) ($:annotation reduce . 
    184) ($:equation reduce . 184) ($:algorithm reduce . 184) ($:initial 
    reduce . 184) ($:protected reduce . 184) ($:public reduce . 184) (
    $:external reduce . 184) ($:end reduce . 184)) ((#{$:;}# reduce . 33) (
    $:constrainedby reduce . 33)) ((#{$:;}# shift . 471)) (($ident reduce . 
    196) ($:. reduce . 196) ($:if reduce . 196) ($:for reduce . 196) ($:while 
    reduce . 196) ($:when reduce . 196) ($:return reduce . 196) ($:break 
    reduce . 196) (#{$:\x28;}# reduce . 196) ($:end reduce . 196) (
    $:annotation reduce . 196) ($:equation reduce . 196) ($:algorithm reduce 
    . 196) ($:initial reduce . 196) ($:protected reduce . 196) ($:public 
    reduce . 196) ($:external reduce . 196) ($:elseif reduce . 196) ($:else 
    reduce . 196) ($:elsewhen reduce . 196)) ((#{$:;}# reduce . 198)) (($ident
    shift . 26) ($string shift . 50) ($float shift . 195) ($fixed shift . 196
    ) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (
    #{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (
    name shift . 201) ($:true shift . 202) ($:false shift . 203) (string shift
    . 204) (unsigned-number shift . 205) (primary shift . 206) (factor shift 
    . 207) (term shift . 208) (arithmetic-expression shift . 209) ($:not shift
    . 210) (relation shift . 211) (logical-factor shift . 212) (logical-term 
    shift . 213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 470)) (($string shift 
    . 50) ($float shift . 195) ($fixed shift . 196) ($:. shift . 27) (
    #{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 
    199) ($:der shift . 200) (name shift . 201) ($:true shift . 202) ($:false 
    shift . 203) (string shift . 204) (unsigned-number shift . 205) (primary 
    shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) (logical-term shift . 213) ($ident shift
    . 26) (logical-expression shift . 214) (ident shift . 368) ($:if shift . 
    215) (simple-expression shift . 216) (expression shift . 369) ($:function 
    shift . 370) (named-argument shift . 371) (named-arguments shift . 372) (
    function-argument shift . 373) (function-arguments shift . 468) (
    #{$:\x29;}# shift . 469)) ((#{$:;}# reduce . 200) ($string reduce . 200) (
    $:annotation reduce . 200)) ((#{$:\x29;}# reduce . 329) ($:, reduce . 329)
    ) ((#{$:\x29;}# reduce . 328) ($:, reduce . 328)) ((#{$:\x29;}# shift . 
    467) ($:, shift . 427)) (($:then shift . 461) (then-st-part shift . 466)) 
    (($:in shift . 465) ($:loop reduce . 239) ($:, reduce . 239) (#{$:\x7d;}# 
    reduce . 239) (#{$:\x29;}# reduce . 239)) (($:loop reduce . 236) ($:, 
    reduce . 236) (#{$:\x7d;}# reduce . 236) (#{$:\x29;}# reduce . 236)) (($:,
    shift . 456) ($:loop shift . 464)) (($:loop shift . 463)) (($:then shift 
    . 461) (then-st-part shift . 462)) (($:. reduce . 313) ($:= reduce . 313) 
    ($::= reduce . 313) (#{$:\x28;}# reduce . 313) ($:, reduce . 313) (
    #{$:\x29;}# reduce . 313)) (($:. reduce . 310) ($:= reduce . 310) ($::= 
    reduce . 310) (#{$:\x28;}# reduce . 310) ($:, reduce . 310) (#{$:\x29;}# 
    reduce . 310)) ((#{$:;}# shift . 460)) (($float reduce . 187) ($fixed 
    reduce . 187) ($string reduce . 187) (#{$:\x7b;}# reduce . 187) (
    #{$:\x5b;}# reduce . 187) (#{$:\x28;}# reduce . 187) ($:der reduce . 187) 
    ($:true reduce . 187) ($:false reduce . 187) ($:not reduce . 187) ($:if 
    reduce . 187) ($:for reduce . 187) ($:connect reduce . 187) ($:when reduce
    . 187) ($ident reduce . 187) ($:. reduce . 187) ($:end reduce . 187) (
    $:annotation reduce . 187) ($:equation reduce . 187) ($:algorithm reduce 
    . 187) ($:initial reduce . 187) ($:protected reduce . 187) ($:public 
    reduce . 187) ($:external reduce . 187) ($:elseif reduce . 187) ($:else 
    reduce . 187) ($:elsewhen reduce . 187)) ((#{$:;}# reduce . 189)) (($ident
    shift . 26) ($string shift . 50) ($float shift . 195) ($fixed shift . 196
    ) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (
    #{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (
    name shift . 201) ($:true shift . 202) ($:false shift . 203) (string shift
    . 204) (unsigned-number shift . 205) (primary shift . 206) (factor shift 
    . 207) (term shift . 208) (arithmetic-expression shift . 209) ($:not shift
    . 210) (relation shift . 211) (logical-factor shift . 212) (logical-term 
    shift . 213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 459)) (($:= reduce . 
    294) ($:: reduce . 294) ($:^ reduce . 294) ($:.^ reduce . 294) ($:./ 
    reduce . 294) ($:.* reduce . 294) ($:/ reduce . 294) ($:* reduce . 294) (
    $:+ reduce . 294) ($:- reduce . 294) ($:.+ reduce . 294) ($:.- reduce . 
    294) ($:<> reduce . 294) ($:== reduce . 294) ($:>= reduce . 294) ($:> 
    reduce . 294) ($:<= reduce . 294) ($:< reduce . 294) ($:and reduce . 294) 
    ($:or reduce . 294) (#{$:;}# reduce . 195) ($string reduce . 195) (
    $:annotation reduce . 195)) ((#{$:\x5d;}# reduce . 297) ($:: reduce . 297)
    ($:^ reduce . 297) ($:.^ reduce . 297) ($:./ reduce . 297) ($:.* reduce 
    . 297) ($:/ reduce . 297) ($:* reduce . 297) ($:+ reduce . 297) ($:- 
    reduce . 297) ($:.+ reduce . 297) ($:.- reduce . 297) ($:<> reduce . 297) 
    ($:== reduce . 297) ($:>= reduce . 297) ($:> reduce . 297) ($:<= reduce . 
    297) ($:< reduce . 297) ($:and reduce . 297) ($:or reduce . 297) ($:, 
    reduce . 297) ($:for reduce . 297) (#{$:;}# reduce . 297) (#{$:\x29;}# 
    reduce . 297) ($:then reduce . 297) ($:loop reduce . 297) ($:else reduce 
    . 297) ($:elseif reduce . 297) ($string reduce . 297) ($:annotation reduce
    . 297) ($:constrainedby reduce . 297) ($:= reduce . 297) (#{$:\x7d;}# 
    reduce . 297) ($:end reduce . 297) ($:elsewhen reduce . 297) ($:if reduce 
    . 297)) (($:then shift . 454) (then-eq-part shift . 458)) (($:, shift . 
    456) ($:loop shift . 457)) (($:then shift . 454) (then-eq-part shift . 455
    )) (($ident shift . 26) ($:. shift . 241) (ident shift . 268) (
    component-reference-1 shift . 242) (component-reference shift . 453)) ((
    $ident shift . 26) ($:. shift . 241) (ident shift . 268) (
    component-reference-1 shift . 242) ($:when shift . 269) ($:while shift . 
    270) ($:for shift . 271) ($:if shift . 272) (when-statement shift . 273) (
    while-statement shift . 274) (for-statement shift . 275) (if-statement 
    shift . 276) ($:return shift . 277) ($:break shift . 278) (#{$:\x28;}# 
    shift . 279) (component-reference shift . 280) (statement-1 shift . 281) (
    statement shift . 285) ($:annotation reduce . 183) ($:equation reduce . 
    183) ($:algorithm reduce . 183) ($:initial reduce . 183) ($:protected 
    reduce . 183) ($:public reduce . 183) ($:external reduce . 183) ($:end 
    reduce . 183)) (($string shift . 50) ($float shift . 195) ($fixed shift . 
    196) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# 
    shift . 199) ($:der shift . 200) ($:true shift . 202) ($:false shift . 203
    ) (string shift . 204) (unsigned-number shift . 205) (primary shift . 206)
    (factor shift . 207) (term shift . 208) (arithmetic-expression shift . 
    209) ($:not shift . 210) (relation shift . 211) (logical-factor shift . 
    212) ($ident shift . 26) (logical-term shift . 213) ($:. shift . 27) (
    ident shift . 28) (logical-expression shift . 214) ($:connect shift . 255)
    ($:when shift . 256) ($:for shift . 257) ($:if shift . 258) (name shift 
    . 259) (when-equation shift . 260) (connect-clause shift . 261) (
    for-equation shift . 262) (if-equation shift . 263) (simple-expression 
    shift . 264) (equation-1 shift . 265) (equation shift . 302) ($:annotation
    reduce . 179) ($:equation reduce . 179) ($:algorithm reduce . 179) (
    $:initial reduce . 179) ($:protected reduce . 179) ($:public reduce . 179)
    ($:external reduce . 179) ($:end reduce . 179)) (($:end reduce . 52)) ((
    $:annotation reduce . 70) ($:end reduce . 70)) (($:annotation reduce . 69)
    ($:end reduce . 69)) ((#{$:;}# shift . 452)) (($ident shift . 26) (ident 
    shift . 451)) ((#{$:\x29;}# shift . 449) ($ident shift . 26) ($string 
    shift . 50) ($float shift . 195) ($fixed shift . 196) ($:. shift . 27) (
    ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (
    #{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) ($:true 
    shift . 202) ($:false shift . 203) (string shift . 204) (unsigned-number 
    shift . 205) (primary shift . 206) (factor shift . 207) (term shift . 208)
    (arithmetic-expression shift . 209) ($:not shift . 210) (relation shift 
    . 211) (logical-factor shift . 212) (logical-term shift . 213) (
    logical-expression shift . 214) ($:if shift . 215) (simple-expression 
    shift . 216) (expression shift . 365) (expression-list shift . 450)) ((
    $ident shift . 26) (ident shift . 448)) ((#{$:\x5b;}# shift . 134) (
    array-subscripts shift . 446) ($P14 shift . 447) (#{$:\x29;}# reduce . 314
    ) ($:, reduce . 314) ($:= reduce . 314) (#{$:\x28;}# reduce . 314) ($::= 
    reduce . 314) ($:. reduce . 314)) (($:annotation reduce . 68) ($:end 
    reduce . 68)) ((#{$:;}# shift . 445)) (($:annotation shift . 114) (
    annotation shift . 443) (#{$:;}# shift . 444)) (($:function reduce . 91) (
    $:pure reduce . 91) ($:impure reduce . 91) ($:package reduce . 91) ($:type
    reduce . 91) ($:connector reduce . 91) ($:expandable reduce . 91) (
    $:block reduce . 91) ($:record reduce . 91) ($:operator reduce . 91) (
    $:model reduce . 91) ($:class reduce . 91) ($:partial reduce . 91) (
    $:encapsulated reduce . 91) ($:stream reduce . 91) ($:flow reduce . 91) (
    $:constant reduce . 91) ($:parameter reduce . 91) ($:discrete reduce . 91)
    ($:output reduce . 91) ($:input reduce . 91) ($ident reduce . 91) ($:. 
    reduce . 91) ($:replaceable reduce . 91)) (($ident shift . 26) ($:. shift 
    . 27) (ident shift . 28) ($:output shift . 70) ($:input shift . 71) (
    $:constant shift . 72) ($:parameter shift . 73) ($:discrete shift . 74) (
    $:stream shift . 75) ($:flow shift . 76) ($:function shift . 1) ($:pure 
    shift . 2) ($:impure shift . 3) ($:package shift . 4) ($:type shift . 5) (
    $:connector shift . 6) ($:expandable shift . 7) ($:block shift . 8) (
    $:record shift . 9) ($:operator shift . 10) ($:model shift . 11) ($:class 
    shift . 12) (name shift . 82) (type-prefix-3 shift . 77) (type-prefix-2 
    shift . 78) (type-prefix-1 shift . 79) (class-prefixes-1 shift . 13) (
    $:partial shift . 14) (type-specifier shift . 83) (type-prefix shift . 84)
    (class-prefixes shift . 15) ($:encapsulated shift . 16) ($:replaceable 
    shift . 87) (component-clause shift . 88) (class-definition shift . 89) (
    element-1 shift . 442)) ((#{$:;}# reduce . 82)) ((#{$:;}# reduce . 100)) (
    ($:. shift . 59) (#{$:\x28;}# shift . 66) (class-modification shift . 441)
    ($string reduce . 118) ($:annotation reduce . 118) (#{$:;}# reduce . 118)
    (#{$:\x29;}# reduce . 118) ($:, reduce . 118)) ((#{$:\x7d;}# shift . 438)
    ($ident shift . 26) (ident shift . 439) (import-list shift . 440)) ((
    $string reduce . 108) ($:annotation reduce . 108) (#{$:;}# reduce . 108)) 
    (($string reduce . 106) ($:annotation reduce . 106) (#{$:;}# reduce . 106)
    ) (($:. shift . 59) ($string reduce . 105) ($:annotation reduce . 105) (
    #{$:;}# reduce . 105)) ((#{$:;}# reduce . 113)) (($:, shift . 188) (
    #{$:;}# reduce . 119) ($:constrainedby reduce . 119)) (($ident reduce . 
    333) ($:= reduce . 333) ($::= reduce . 333) (#{$:\x28;}# reduce . 333) (
    $string reduce . 333) ($:annotation reduce . 333) ($:if reduce . 333) ($:,
    reduce . 333) (#{$:;}# reduce . 333) ($:constrainedby reduce . 333) (
    #{$:\x5d;}# reduce . 333) ($:: reduce . 333) ($:^ reduce . 333) ($:.^ 
    reduce . 333) ($:./ reduce . 333) ($:.* reduce . 333) ($:/ reduce . 333) (
    $:* reduce . 333) ($:+ reduce . 333) ($:- reduce . 333) ($:.+ reduce . 333
    ) ($:.- reduce . 333) ($:<> reduce . 333) ($:== reduce . 333) ($:>= reduce
    . 333) ($:> reduce . 333) ($:<= reduce . 333) ($:< reduce . 333) ($:and 
    reduce . 333) ($:or reduce . 333) ($:for reduce . 333) (#{$:\x29;}# reduce
    . 333) ($:then reduce . 333) ($:loop reduce . 333) ($:else reduce . 333) 
    ($:elseif reduce . 333) ($:. reduce . 333) ($:stream reduce . 333) ($:flow
    reduce . 333) ($:constant reduce . 333) ($:parameter reduce . 333) (
    $:discrete reduce . 333) ($:output reduce . 333) ($:input reduce . 333) (
    #{$:\x7d;}# reduce . 333) ($:end reduce . 333) ($:elsewhen reduce . 333)) 
    (($ident shift . 26) ($string shift . 50) ($float shift . 195) ($fixed 
    shift . 196) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197)
    (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (
    name shift . 201) ($:true shift . 202) ($:false shift . 203) (string shift
    . 204) (unsigned-number shift . 205) (primary shift . 206) (factor shift 
    . 207) (term shift . 208) (arithmetic-expression shift . 209) ($:not shift
    . 210) (relation shift . 211) (logical-factor shift . 212) (logical-term 
    shift . 213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 217) ($:: shift . 218) 
    (subscript shift . 437)) (($:then shift . 436)) (($ident shift . 26) (
    $string shift . 50) ($float shift . 195) ($fixed shift . 196) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 
    198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) (
    $:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 209) ($:not shift . 210) 
    (relation shift . 211) (logical-factor shift . 212) (logical-term shift . 
    213) (logical-expression shift . 435)) (($ident shift . 26) ($string shift
    . 50) ($float shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (
    #{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) ($:true 
    shift . 202) ($:false shift . 203) (string shift . 204) (unsigned-number 
    shift . 205) (primary shift . 206) (factor shift . 207) (term shift . 208)
    (arithmetic-expression shift . 209) ($:not shift . 210) (relation shift 
    . 211) (logical-factor shift . 212) (logical-term shift . 434)) (($ident 
    shift . 26) ($string shift . 50) ($float shift . 195) ($fixed shift . 196)
    ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (
    #{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (
    name shift . 201) ($:true shift . 202) ($:false shift . 203) (string shift
    . 204) (unsigned-number shift . 205) (primary shift . 206) (factor shift 
    . 207) (term shift . 208) (arithmetic-expression shift . 209) ($:not shift
    . 210) (relation shift . 211) (logical-factor shift . 433)) (($float 
    reduce . 274) ($fixed reduce . 274) ($string reduce . 274) ($ident reduce 
    . 274) ($:. reduce . 274) (#{$:\x7b;}# reduce . 274) (#{$:\x5b;}# reduce 
    . 274) (#{$:\x28;}# reduce . 274) ($:der reduce . 274) ($:true reduce . 
    274) ($:false reduce . 274)) (($float reduce . 273) ($fixed reduce . 273) 
    ($string reduce . 273) ($ident reduce . 273) ($:. reduce . 273) (
    #{$:\x7b;}# reduce . 273) (#{$:\x5b;}# reduce . 273) (#{$:\x28;}# reduce 
    . 273) ($:der reduce . 273) ($:true reduce . 273) ($:false reduce . 273)) 
    (($float reduce . 272) ($fixed reduce . 272) ($string reduce . 272) (
    $ident reduce . 272) ($:. reduce . 272) (#{$:\x7b;}# reduce . 272) (
    #{$:\x5b;}# reduce . 272) (#{$:\x28;}# reduce . 272) ($:der reduce . 272) 
    ($:true reduce . 272) ($:false reduce . 272)) (($float reduce . 271) (
    $fixed reduce . 271) ($string reduce . 271) ($ident reduce . 271) ($:. 
    reduce . 271) (#{$:\x7b;}# reduce . 271) (#{$:\x5b;}# reduce . 271) (
    #{$:\x28;}# reduce . 271) ($:der reduce . 271) ($:true reduce . 271) (
    $:false reduce . 271)) (($float reduce . 270) ($fixed reduce . 270) (
    $string reduce . 270) ($ident reduce . 270) ($:. reduce . 270) (
    #{$:\x7b;}# reduce . 270) (#{$:\x5b;}# reduce . 270) (#{$:\x28;}# reduce 
    . 270) ($:der reduce . 270) ($:true reduce . 270) ($:false reduce . 270)) 
    (($float reduce . 269) ($fixed reduce . 269) ($string reduce . 269) (
    $ident reduce . 269) ($:. reduce . 269) (#{$:\x7b;}# reduce . 269) (
    #{$:\x5b;}# reduce . 269) (#{$:\x28;}# reduce . 269) ($:der reduce . 269) 
    ($:true reduce . 269) ($:false reduce . 269)) (($ident shift . 26) (
    $string shift . 50) ($float shift . 195) ($fixed shift . 196) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 
    198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) (
    $:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 432)) (($:<> shift . 342)
    ($:== shift . 343) ($:>= shift . 344) ($:> shift . 345) ($:<= shift . 346
    ) ($:< shift . 347) (rel-op shift . 348) ($:: reduce . 266) ($:= reduce . 
    266) ($:or reduce . 266) ($:and reduce . 266) (#{$:\x5d;}# reduce . 266) (
    $:, reduce . 266) ($:for reduce . 266) (#{$:;}# reduce . 266) (#{$:\x29;}#
    reduce . 266) ($:then reduce . 266) ($:loop reduce . 266) ($:else reduce 
    . 266) ($:elseif reduce . 266) ($string reduce . 266) ($:annotation reduce
    . 266) ($:constrainedby reduce . 266) (#{$:\x7d;}# reduce . 266) ($:end 
    reduce . 266) ($:elsewhen reduce . 266) ($:if reduce . 266)) (($float 
    reduce . 280) ($fixed reduce . 280) ($string reduce . 280) ($ident reduce 
    . 280) ($:. reduce . 280) (#{$:\x7b;}# reduce . 280) (#{$:\x5b;}# reduce 
    . 280) (#{$:\x28;}# reduce . 280) ($:der reduce . 280) ($:true reduce . 
    280) ($:false reduce . 280)) (($float reduce . 279) ($fixed reduce . 279) 
    ($string reduce . 279) ($ident reduce . 279) ($:. reduce . 279) (
    #{$:\x7b;}# reduce . 279) (#{$:\x5b;}# reduce . 279) (#{$:\x28;}# reduce 
    . 279) ($:der reduce . 279) ($:true reduce . 279) ($:false reduce . 279)) 
    (($float reduce . 278) ($fixed reduce . 278) ($string reduce . 278) (
    $ident reduce . 278) ($:. reduce . 278) (#{$:\x7b;}# reduce . 278) (
    #{$:\x5b;}# reduce . 278) (#{$:\x28;}# reduce . 278) ($:der reduce . 278) 
    ($:true reduce . 278) ($:false reduce . 278)) (($float reduce . 277) (
    $fixed reduce . 277) ($string reduce . 277) ($ident reduce . 277) ($:. 
    reduce . 277) (#{$:\x7b;}# reduce . 277) (#{$:\x5b;}# reduce . 277) (
    #{$:\x28;}# reduce . 277) ($:der reduce . 277) ($:true reduce . 277) (
    $:false reduce . 277)) (($ident shift . 26) ($string shift . 50) ($float 
    shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident shift . 28) (
    #{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 
    199) ($:der shift . 200) (name shift . 201) ($:true shift . 202) ($:false 
    shift . 203) (string shift . 204) (unsigned-number shift . 205) (primary 
    shift . 206) (factor shift . 207) (term shift . 431)) (($float reduce . 
    286) ($fixed reduce . 286) ($string reduce . 286) ($ident reduce . 286) (
    $:. reduce . 286) (#{$:\x7b;}# reduce . 286) (#{$:\x5b;}# reduce . 286) (
    #{$:\x28;}# reduce . 286) ($:der reduce . 286) ($:true reduce . 286) (
    $:false reduce . 286)) (($float reduce . 285) ($fixed reduce . 285) (
    $string reduce . 285) ($ident reduce . 285) ($:. reduce . 285) (
    #{$:\x7b;}# reduce . 285) (#{$:\x5b;}# reduce . 285) (#{$:\x28;}# reduce 
    . 285) ($:der reduce . 285) ($:true reduce . 285) ($:false reduce . 285)) 
    (($float reduce . 284) ($fixed reduce . 284) ($string reduce . 284) (
    $ident reduce . 284) ($:. reduce . 284) (#{$:\x7b;}# reduce . 284) (
    #{$:\x5b;}# reduce . 284) (#{$:\x28;}# reduce . 284) ($:der reduce . 284) 
    ($:true reduce . 284) ($:false reduce . 284)) (($float reduce . 283) (
    $fixed reduce . 283) ($string reduce . 283) ($ident reduce . 283) ($:. 
    reduce . 283) (#{$:\x7b;}# reduce . 283) (#{$:\x5b;}# reduce . 283) (
    #{$:\x28;}# reduce . 283) ($:der reduce . 283) ($:true reduce . 283) (
    $:false reduce . 283)) (($ident shift . 26) ($string shift . 50) ($float 
    shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident shift . 28) (
    #{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 
    199) ($:der shift . 200) (name shift . 201) ($:true shift . 202) ($:false 
    shift . 203) (string shift . 204) (unsigned-number shift . 205) (primary 
    shift . 206) (factor shift . 430)) (($ident shift . 26) ($string shift . 
    50) ($float shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (
    #{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) ($:true 
    shift . 202) ($:false shift . 203) (string shift . 204) (unsigned-number 
    shift . 205) (primary shift . 429)) (($ident shift . 26) ($string shift . 
    50) ($float shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (
    #{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) ($:true 
    shift . 202) ($:false shift . 203) (string shift . 204) (unsigned-number 
    shift . 205) (primary shift . 428)) ((#{$:\x5d;}# reduce . 294) ($:: 
    reduce . 294) ($:^ reduce . 294) ($:.^ reduce . 294) ($:./ reduce . 294) (
    $:.* reduce . 294) ($:/ reduce . 294) ($:* reduce . 294) ($:+ reduce . 294
    ) ($:- reduce . 294) ($:.+ reduce . 294) ($:.- reduce . 294) ($:<> reduce 
    . 294) ($:== reduce . 294) ($:>= reduce . 294) ($:> reduce . 294) ($:<= 
    reduce . 294) ($:< reduce . 294) ($:and reduce . 294) ($:or reduce . 294) 
    ($:, reduce . 294) ($:for reduce . 294) (#{$:;}# reduce . 294) (
    #{$:\x29;}# reduce . 294) ($:then reduce . 294) ($:loop reduce . 294) (
    $:else reduce . 294) ($:elseif reduce . 294) ($string reduce . 294) (
    $:annotation reduce . 294) ($:constrainedby reduce . 294) ($:= reduce . 
    294) (#{$:\x7d;}# reduce . 294) ($:end reduce . 294) ($:elsewhen reduce . 
    294) ($:if reduce . 294)) (($:= reduce . 295) ($:: reduce . 295) ($:^ 
    reduce . 295) ($:.^ reduce . 295) ($:./ reduce . 295) ($:.* reduce . 295) 
    ($:/ reduce . 295) ($:* reduce . 295) ($:+ reduce . 295) ($:- reduce . 295
    ) ($:.+ reduce . 295) ($:.- reduce . 295) ($:<> reduce . 295) ($:== reduce
    . 295) ($:>= reduce . 295) ($:> reduce . 295) ($:<= reduce . 295) ($:< 
    reduce . 295) ($:and reduce . 295) ($:or reduce . 295) (#{$:\x5d;}# reduce
    . 295) ($:, reduce . 295) ($:for reduce . 295) (#{$:;}# reduce . 295) (
    #{$:\x29;}# reduce . 295) ($:then reduce . 295) ($:loop reduce . 295) (
    $:else reduce . 295) ($:elseif reduce . 295) ($string reduce . 295) (
    $:annotation reduce . 295) ($:constrainedby reduce . 295) (#{$:\x7d;}# 
    reduce . 295) ($:end reduce . 295) ($:elsewhen reduce . 295) ($:if reduce 
    . 295)) ((#{$:\x29;}# shift . 426) ($:, shift . 427)) ((#{$:\x5d;}# reduce
    . 331) ($:, reduce . 331) (#{$:;}# reduce . 331) (#{$:\x29;}# reduce . 
    331) ($:end reduce . 331) ($:elsewhen reduce . 331)) (($:, shift . 425) (
    #{$:\x5d;}# reduce . 301) (#{$:;}# reduce . 301)) ((#{$:\x5d;}# shift . 
    423) (#{$:;}# shift . 424)) (($:= shift . 422) (#{$:\x5b;}# reduce . 303) 
    ($:: reduce . 303) ($:, reduce . 303) ($:for reduce . 303) (#{$:\x28;}# 
    reduce . 303) ($:or reduce . 303) ($:and reduce . 303) ($:< reduce . 303) 
    ($:<= reduce . 303) ($:> reduce . 303) ($:>= reduce . 303) ($:== reduce . 
    303) ($:<> reduce . 303) ($:.- reduce . 303) ($:.+ reduce . 303) ($:- 
    reduce . 303) ($:+ reduce . 303) ($:* reduce . 303) ($:/ reduce . 303) (
    $:.* reduce . 303) ($:./ reduce . 303) ($:.^ reduce . 303) ($:^ reduce . 
    303) ($:. reduce . 303)) (($:for reduce . 327) ($:, reduce . 327) (
    #{$:\x7d;}# reduce . 327) (#{$:\x29;}# reduce . 327)) (($ident shift . 26)
    ($:. shift . 27) (ident shift . 28) (name shift . 421)) ((#{$:\x7d;}# 
    reduce . 322) ($:, reduce . 322) (#{$:\x29;}# reduce . 322)) (($:, shift 
    . 420) (#{$:\x7d;}# reduce . 319) (#{$:\x29;}# reduce . 319)) (($:for 
    shift . 417) ($:, shift . 418) (function-argument-1 shift . 419)) ((
    #{$:\x7d;}# shift . 416)) (($string reduce . 147) ($:annotation reduce . 
    147) ($:if reduce . 147) ($:, reduce . 147) (#{$:;}# reduce . 147) (
    $:constrainedby reduce . 147) (#{$:\x29;}# reduce . 147)) (($string reduce
    . 143) ($:annotation reduce . 143) ($:if reduce . 143) ($:, reduce . 143)
    (#{$:;}# reduce . 143) ($:constrainedby reduce . 143) (#{$:\x29;}# reduce
    . 143)) (($:, reduce . 140) (#{$:;}# reduce . 140) ($:constrainedby 
    reduce . 140)) (($string reduce . 142) ($:annotation reduce . 142) ($:, 
    reduce . 142) (#{$:;}# reduce . 142) ($:constrainedby reduce . 142)) (($:,
    reduce . 139) (#{$:;}# reduce . 139) ($:constrainedby reduce . 139)) ((
    $string shift . 50) (string shift . 51) (string-cat shift . 52) (
    string-comment shift . 184) (comment shift . 415) (#{$:;}# reduce . 340) (
    $:constrainedby reduce . 340) ($:stream reduce . 340) ($:flow reduce . 340
    ) ($:constant reduce . 340) ($:parameter reduce . 340) ($:discrete reduce 
    . 340) ($:output reduce . 340) ($:input reduce . 340) (#{$:\x29;}# reduce 
    . 340) ($:, reduce . 340) ($:annotation reduce . 340)) ((#{$:;}# reduce . 
    37) ($:constrainedby reduce . 37) ($:stream reduce . 37) ($:flow reduce . 
    37) ($:constant reduce . 37) ($:parameter reduce . 37) ($:discrete reduce 
    . 37) ($:output reduce . 37) ($:input reduce . 37) (#{$:\x29;}# reduce . 
    37) ($:, reduce . 37)) ((#{$:;}# reduce . 38) ($:constrainedby reduce . 38
    ) ($:stream reduce . 38) ($:flow reduce . 38) ($:constant reduce . 38) (
    $:parameter reduce . 38) ($:discrete reduce . 38) ($:output reduce . 38) (
    $:input reduce . 38) (#{$:\x29;}# reduce . 38) ($:, reduce . 38)) ((
    #{$:;}# reduce . 338) ($:, reduce . 338) ($:constrainedby reduce . 338) (
    #{$:\x29;}# reduce . 338) ($:stream reduce . 338) ($:flow reduce . 338) (
    $:constant reduce . 338) ($:parameter reduce . 338) ($:discrete reduce . 
    338) ($:output reduce . 338) ($:input reduce . 338)) (($string shift . 50)
    (string shift . 51) (string-cat shift . 52) (string-comment shift . 184) 
    (comment shift . 414) (#{$:;}# reduce . 340) ($:constrainedby reduce . 340
    ) ($:stream reduce . 340) ($:flow reduce . 340) ($:constant reduce . 340) 
    ($:parameter reduce . 340) ($:discrete reduce . 340) ($:output reduce . 
    340) ($:input reduce . 340) (#{$:\x29;}# reduce . 340) ($:, reduce . 340) 
    ($:annotation reduce . 340)) (($ident shift . 26) (ident shift . 178) (
    enumeration-literal shift . 413)) ((#{$:\x29;}# reduce . 51) ($:, reduce 
    . 51)) (($ident shift . 26) (ident shift . 411) (der-class-specifier-1 
    shift . 412)) (($ident shift . 26) (ident shift . 410)) (($ident shift . 
    26) ($:. shift . 27) (ident shift . 28) (name shift . 82) (type-specifier 
    shift . 409)) (($:constrainedby shift . 228) (constraining-clause shift . 
    408) (#{$:\x29;}# reduce . 176) ($:, reduce . 176)) (($:= shift . 407)) ((
    $:stream reduce . 178) ($:flow reduce . 178) ($:constant reduce . 178) (
    $:parameter reduce . 178) ($:discrete reduce . 178) ($:output reduce . 178
    ) ($:input reduce . 178) (#{$:\x29;}# reduce . 178) ($:, reduce . 178)) ((
    #{$:\x29;}# reduce . 164) ($:, reduce . 164)) (($ident shift . 26) (
    $string shift . 50) ($float shift . 195) ($fixed shift . 196) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 
    198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) (
    $:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 209) ($:not shift . 210) 
    (relation shift . 211) (logical-factor shift . 212) (logical-term shift . 
    213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 406)) (($string reduce 
    . 150) (#{$:\x29;}# reduce . 150) ($:, reduce . 150) ($:annotation reduce 
    . 150) ($:if reduce . 150) (#{$:;}# reduce . 150) ($:constrainedby reduce 
    . 150)) (($string reduce . 151) (#{$:\x29;}# reduce . 151) ($:, reduce . 
    151) ($:annotation reduce . 151) ($:if reduce . 151) (#{$:;}# reduce . 151
    ) ($:constrainedby reduce . 151)) (($:function reduce . 171) ($:pure 
    reduce . 171) ($:impure reduce . 171) ($:package reduce . 171) ($:type 
    reduce . 171) ($:connector reduce . 171) ($:expandable reduce . 171) (
    $:block reduce . 171) ($:record reduce . 171) ($:operator reduce . 171) (
    $:model reduce . 171) ($:class reduce . 171) ($:partial reduce . 171) (
    $:stream reduce . 171) ($:flow reduce . 171) ($:constant reduce . 171) (
    $:parameter reduce . 171) ($:discrete reduce . 171) ($:output reduce . 171
    ) ($:input reduce . 171) ($:replaceable reduce . 171)) (($:output shift . 
    70) ($:input shift . 71) ($:constant shift . 72) ($:parameter shift . 73) 
    ($:discrete shift . 74) ($:stream shift . 75) ($:flow shift . 76) (
    $:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package 
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) (
    $:model shift . 11) ($:class shift . 12) (type-prefix-3 shift . 77) (
    type-prefix-2 shift . 78) (type-prefix-1 shift . 79) (class-prefixes-1 
    shift . 13) ($:partial shift . 14) (class-prefixes shift . 174) (
    type-prefix shift . 389) ($:replaceable shift . 147) (element-replaceable 
    shift . 402) (component-clause1 shift . 403) (short-class-definition shift
    . 404) (elt-redecl-1 shift . 405)) ((#{$:\x29;}# reduce . 158) ($:, 
    reduce . 158)) ((#{$:\x29;}# reduce . 155) ($:, reduce . 155)) ((#{$:;}# 
    reduce . 35) ($:constrainedby reduce . 35)) ((#{$:\x29;}# reduce . 174) (
    $:, reduce . 174)) ((#{$:\x29;}# reduce . 173) ($:, reduce . 173)) ((
    #{$:\x29;}# reduce . 172) ($:, reduce . 172)) ((#{$:\x29;}# reduce . 167) 
    ($:, reduce . 167)) (($string reduce . 148) (#{$:\x29;}# reduce . 148) (
    $:, reduce . 148) ($:annotation reduce . 148) ($:if reduce . 148) (#{$:;}#
    reduce . 148) ($:constrainedby reduce . 148)) (($:enumeration shift . 69)
    ($:output shift . 70) ($:input shift . 71) ($:constant shift . 72) (
    $:parameter shift . 73) ($:discrete shift . 74) ($:stream shift . 75) (
    $:flow shift . 76) (type-prefix-3 shift . 77) (type-prefix-2 shift . 78) (
    type-prefix-1 shift . 79) (type-prefix shift . 80) (base-prefix shift . 81
    ) ($ident reduce . 47) ($:. reduce . 47)) ((#{$:\x29;}# reduce . 175) ($:,
    reduce . 175)) (($ident shift . 26) (ident shift . 136) (declaration 
    shift . 523)) ((#{$:;}# reduce . 34) ($:constrainedby reduce . 34)) ((
    #{$:\x29;}# reduce . 45) (#{$:;}# reduce . 45)) ((#{$:\x29;}# shift . 521)
    (#{$:;}# shift . 522)) ((#{$:\x29;}# reduce . 50) ($:, reduce . 50)) ((
    #{$:;}# reduce . 40) ($:constrainedby reduce . 40) ($:stream reduce . 40) 
    ($:flow reduce . 40) ($:constant reduce . 40) ($:parameter reduce . 40) (
    $:discrete reduce . 40) ($:output reduce . 40) ($:input reduce . 40) (
    #{$:\x29;}# reduce . 40) ($:, reduce . 40)) ((#{$:;}# reduce . 36) (
    $:constrainedby reduce . 36) ($:stream reduce . 36) ($:flow reduce . 36) (
    $:constant reduce . 36) ($:parameter reduce . 36) ($:discrete reduce . 36)
    ($:output reduce . 36) ($:input reduce . 36) (#{$:\x29;}# reduce . 36) (
    $:, reduce . 36)) (($:= reduce . 300) ($:: reduce . 300) ($:^ reduce . 300
    ) ($:.^ reduce . 300) ($:./ reduce . 300) ($:.* reduce . 300) ($:/ reduce 
    . 300) ($:* reduce . 300) ($:+ reduce . 300) ($:- reduce . 300) ($:.+ 
    reduce . 300) ($:.- reduce . 300) ($:<> reduce . 300) ($:== reduce . 300) 
    ($:>= reduce . 300) ($:> reduce . 300) ($:<= reduce . 300) ($:< reduce . 
    300) ($:and reduce . 300) ($:or reduce . 300) (#{$:\x5d;}# reduce . 300) (
    $:, reduce . 300) ($:for reduce . 300) (#{$:;}# reduce . 300) (#{$:\x29;}#
    reduce . 300) ($:then reduce . 300) ($:loop reduce . 300) ($:else reduce 
    . 300) ($:elseif reduce . 300) ($string reduce . 300) ($:annotation reduce
    . 300) ($:constrainedby reduce . 300) (#{$:\x7d;}# reduce . 300) ($:end 
    reduce . 300) ($:elsewhen reduce . 300) ($:if reduce . 300)) (($ident 
    shift . 26) (ident shift . 295) (for-index shift . 296) (for-indices shift
    . 520)) (($string shift . 50) ($float shift . 195) ($fixed shift . 196) (
    $:. shift . 27) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (
    #{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) ($:true 
    shift . 202) ($:false shift . 203) (string shift . 204) (unsigned-number 
    shift . 205) (primary shift . 206) (factor shift . 207) (term shift . 208)
    (arithmetic-expression shift . 209) ($:not shift . 210) (relation shift 
    . 211) (logical-factor shift . 212) (logical-term shift . 213) ($ident 
    shift . 26) (logical-expression shift . 214) (ident shift . 368) ($:if 
    shift . 215) (simple-expression shift . 216) (expression shift . 369) (
    $:function shift . 370) (named-argument shift . 371) (named-arguments 
    shift . 372) (function-argument shift . 373) (function-arguments shift . 
    519)) ((#{$:\x7d;}# reduce . 318) (#{$:\x29;}# reduce . 318)) (($ident 
    shift . 26) (ident shift . 517) (named-argument shift . 518)) (($:. shift 
    . 59) (#{$:\x28;}# shift . 516)) (($ident shift . 26) ($string shift . 50)
    ($float shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident shift 
    . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# 
    shift . 199) ($:der shift . 200) (name shift . 201) ($:true shift . 202) (
    $:false shift . 203) (string shift . 204) (unsigned-number shift . 205) (
    primary shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) (logical-term shift . 213) (
    logical-expression shift . 214) ($:if shift . 215) (simple-expression 
    shift . 216) (expression shift . 369) ($:function shift . 370) (
    function-argument shift . 515)) (($:= reduce . 299) ($:: reduce . 299) (
    $:^ reduce . 299) ($:.^ reduce . 299) ($:./ reduce . 299) ($:.* reduce . 
    299) ($:/ reduce . 299) ($:* reduce . 299) ($:+ reduce . 299) ($:- reduce 
    . 299) ($:.+ reduce . 299) ($:.- reduce . 299) ($:<> reduce . 299) ($:== 
    reduce . 299) ($:>= reduce . 299) ($:> reduce . 299) ($:<= reduce . 299) (
    $:< reduce . 299) ($:and reduce . 299) ($:or reduce . 299) (#{$:\x5d;}# 
    reduce . 299) ($:, reduce . 299) ($:for reduce . 299) (#{$:;}# reduce . 
    299) (#{$:\x29;}# reduce . 299) ($:then reduce . 299) ($:loop reduce . 299
    ) ($:else reduce . 299) ($:elseif reduce . 299) ($string reduce . 299) (
    $:annotation reduce . 299) ($:constrainedby reduce . 299) (#{$:\x7d;}# 
    reduce . 299) ($:end reduce . 299) ($:elsewhen reduce . 299) ($:if reduce 
    . 299)) (($ident shift . 26) ($string shift . 50) ($float shift . 195) (
    $fixed shift . 196) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift
    . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der shift 
    . 200) (name shift . 201) ($:true shift . 202) ($:false shift . 203) (
    string shift . 204) (unsigned-number shift . 205) (primary shift . 206) (
    factor shift . 207) (term shift . 208) (arithmetic-expression shift . 209)
    ($:not shift . 210) (relation shift . 211) (logical-factor shift . 212) (
    logical-term shift . 213) (logical-expression shift . 214) ($:if shift . 
    215) (simple-expression shift . 216) (expression shift . 365) (
    expression-list shift . 514)) (($ident shift . 26) ($string shift . 50) (
    $float shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident shift . 
    28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift
    . 199) ($:der shift . 200) (name shift . 201) ($:true shift . 202) (
    $:false shift . 203) (string shift . 204) (unsigned-number shift . 205) (
    primary shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) (logical-term shift . 213) (
    logical-expression shift . 214) ($:if shift . 215) (simple-expression 
    shift . 216) (expression shift . 513)) (($:= reduce . 298) ($:: reduce . 
    298) ($:^ reduce . 298) ($:.^ reduce . 298) ($:./ reduce . 298) ($:.* 
    reduce . 298) ($:/ reduce . 298) ($:* reduce . 298) ($:+ reduce . 298) (
    $:- reduce . 298) ($:.+ reduce . 298) ($:.- reduce . 298) ($:<> reduce . 
    298) ($:== reduce . 298) ($:>= reduce . 298) ($:> reduce . 298) ($:<= 
    reduce . 298) ($:< reduce . 298) ($:and reduce . 298) ($:or reduce . 298) 
    (#{$:\x5d;}# reduce . 298) ($:, reduce . 298) ($:for reduce . 298) (
    #{$:;}# reduce . 298) (#{$:\x29;}# reduce . 298) ($:then reduce . 298) (
    $:loop reduce . 298) ($:else reduce . 298) ($:elseif reduce . 298) (
    $string reduce . 298) ($:annotation reduce . 298) ($:constrainedby reduce 
    . 298) (#{$:\x7d;}# reduce . 298) ($:end reduce . 298) ($:elsewhen reduce 
    . 298) ($:if reduce . 298)) (($ident shift . 26) ($string shift . 50) (
    $float shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident shift . 
    28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift
    . 199) ($:der shift . 200) (name shift . 201) ($:true shift . 202) (
    $:false shift . 203) (string shift . 204) (unsigned-number shift . 205) (
    primary shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) (logical-term shift . 213) (
    logical-expression shift . 214) ($:if shift . 215) (simple-expression 
    shift . 216) (expression shift . 512)) (($:: reduce . 289) ($:= reduce . 
    289) ($:or reduce . 289) ($:and reduce . 289) ($:< reduce . 289) ($:<= 
    reduce . 289) ($:> reduce . 289) ($:>= reduce . 289) ($:== reduce . 289) (
    $:<> reduce . 289) ($:.- reduce . 289) ($:.+ reduce . 289) ($:- reduce . 
    289) ($:+ reduce . 289) ($:* reduce . 289) ($:/ reduce . 289) ($:.* reduce
    . 289) ($:./ reduce . 289) ($:.^ reduce . 289) ($:^ reduce . 289) (
    #{$:\x5d;}# reduce . 289) ($:, reduce . 289) ($:for reduce . 289) (#{$:;}#
    reduce . 289) (#{$:\x29;}# reduce . 289) ($:then reduce . 289) ($:loop 
    reduce . 289) ($:else reduce . 289) ($:elseif reduce . 289) ($string 
    reduce . 289) ($:annotation reduce . 289) ($:constrainedby reduce . 289) (
    #{$:\x7d;}# reduce . 289) ($:end reduce . 289) ($:elsewhen reduce . 289) (
    $:if reduce . 289)) (($:: reduce . 288) ($:= reduce . 288) ($:or reduce . 
    288) ($:and reduce . 288) ($:< reduce . 288) ($:<= reduce . 288) ($:> 
    reduce . 288) ($:>= reduce . 288) ($:== reduce . 288) ($:<> reduce . 288) 
    ($:.- reduce . 288) ($:.+ reduce . 288) ($:- reduce . 288) ($:+ reduce . 
    288) ($:* reduce . 288) ($:/ reduce . 288) ($:.* reduce . 288) ($:./ 
    reduce . 288) ($:.^ reduce . 288) ($:^ reduce . 288) (#{$:\x5d;}# reduce 
    . 288) ($:, reduce . 288) ($:for reduce . 288) (#{$:;}# reduce . 288) (
    #{$:\x29;}# reduce . 288) ($:then reduce . 288) ($:loop reduce . 288) (
    $:else reduce . 288) ($:elseif reduce . 288) ($string reduce . 288) (
    $:annotation reduce . 288) ($:constrainedby reduce . 288) (#{$:\x7d;}# 
    reduce . 288) ($:end reduce . 288) ($:elsewhen reduce . 288) ($:if reduce 
    . 288)) (($:^ shift . 360) ($:.^ shift . 361) ($:= reduce . 282) ($:: 
    reduce . 282) ($:./ reduce . 282) ($:.* reduce . 282) ($:/ reduce . 282) (
    $:* reduce . 282) ($:+ reduce . 282) ($:- reduce . 282) ($:.+ reduce . 282
    ) ($:.- reduce . 282) ($:<> reduce . 282) ($:== reduce . 282) ($:>= reduce
    . 282) ($:> reduce . 282) ($:<= reduce . 282) ($:< reduce . 282) ($:and 
    reduce . 282) ($:or reduce . 282) (#{$:\x5d;}# reduce . 282) ($:, reduce 
    . 282) ($:for reduce . 282) (#{$:;}# reduce . 282) (#{$:\x29;}# reduce . 
    282) ($:then reduce . 282) ($:loop reduce . 282) ($:else reduce . 282) (
    $:elseif reduce . 282) ($string reduce . 282) ($:annotation reduce . 282) 
    ($:constrainedby reduce . 282) (#{$:\x7d;}# reduce . 282) ($:end reduce . 
    282) ($:elsewhen reduce . 282) ($:if reduce . 282)) (($:./ shift . 355) (
    $:.* shift . 356) ($:/ shift . 357) ($:* shift . 358) (mul-op shift . 359)
    ($:: reduce . 276) ($:= reduce . 276) ($:or reduce . 276) ($:and reduce 
    . 276) ($:< reduce . 276) ($:<= reduce . 276) ($:> reduce . 276) ($:>= 
    reduce . 276) ($:== reduce . 276) ($:<> reduce . 276) ($:.- reduce . 276) 
    ($:.+ reduce . 276) ($:- reduce . 276) ($:+ reduce . 276) (#{$:\x5d;}# 
    reduce . 276) ($:, reduce . 276) ($:for reduce . 276) (#{$:;}# reduce . 
    276) (#{$:\x29;}# reduce . 276) ($:then reduce . 276) ($:loop reduce . 276
    ) ($:else reduce . 276) ($:elseif reduce . 276) ($string reduce . 276) (
    $:annotation reduce . 276) ($:constrainedby reduce . 276) (#{$:\x7d;}# 
    reduce . 276) ($:end reduce . 276) ($:elsewhen reduce . 276) ($:if reduce 
    . 276)) (($:.- shift . 350) ($:.+ shift . 351) ($:- shift . 352) ($:+ 
    shift . 353) (add-op shift . 354) ($:= reduce . 268) ($:: reduce . 268) (
    $:<> reduce . 268) ($:== reduce . 268) ($:>= reduce . 268) ($:> reduce . 
    268) ($:<= reduce . 268) ($:< reduce . 268) ($:and reduce . 268) ($:or 
    reduce . 268) (#{$:\x5d;}# reduce . 268) ($:, reduce . 268) ($:for reduce 
    . 268) (#{$:;}# reduce . 268) (#{$:\x29;}# reduce . 268) ($:then reduce . 
    268) ($:loop reduce . 268) ($:else reduce . 268) ($:elseif reduce . 268) (
    $string reduce . 268) ($:annotation reduce . 268) ($:constrainedby reduce 
    . 268) (#{$:\x7d;}# reduce . 268) ($:end reduce . 268) ($:elsewhen reduce 
    . 268) ($:if reduce . 268)) (($:= reduce . 264) ($:: reduce . 264) ($:and 
    reduce . 264) ($:or reduce . 264) (#{$:\x5d;}# reduce . 264) ($:, reduce 
    . 264) ($:for reduce . 264) (#{$:;}# reduce . 264) (#{$:\x29;}# reduce . 
    264) ($:then reduce . 264) ($:loop reduce . 264) ($:else reduce . 264) (
    $:elseif reduce . 264) ($string reduce . 264) ($:annotation reduce . 264) 
    ($:constrainedby reduce . 264) (#{$:\x7d;}# reduce . 264) ($:end reduce . 
    264) ($:elsewhen reduce . 264) ($:if reduce . 264)) (($:and shift . 341) (
    $:: reduce . 262) ($:= reduce . 262) ($:or reduce . 262) (#{$:\x5d;}# 
    reduce . 262) ($:, reduce . 262) ($:for reduce . 262) (#{$:;}# reduce . 
    262) (#{$:\x29;}# reduce . 262) ($:then reduce . 262) ($:loop reduce . 262
    ) ($:else reduce . 262) ($:elseif reduce . 262) ($string reduce . 262) (
    $:annotation reduce . 262) ($:constrainedby reduce . 262) (#{$:\x7d;}# 
    reduce . 262) ($:end reduce . 262) ($:elsewhen reduce . 262) ($:if reduce 
    . 262)) (($:: shift . 511) ($:or shift . 340) ($:= reduce . 260) (
    #{$:\x5d;}# reduce . 260) ($:, reduce . 260) ($:for reduce . 260) (#{$:;}#
    reduce . 260) (#{$:\x29;}# reduce . 260) ($:then reduce . 260) ($:loop 
    reduce . 260) ($:else reduce . 260) ($:elseif reduce . 260) ($string 
    reduce . 260) ($:annotation reduce . 260) ($:constrainedby reduce . 260) (
    #{$:\x7d;}# reduce . 260) ($:end reduce . 260) ($:elsewhen reduce . 260) (
    $:if reduce . 260)) (($ident shift . 26) ($string shift . 50) ($float 
    shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident shift . 28) (
    #{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 
    199) ($:der shift . 200) (name shift . 201) ($:true shift . 202) ($:false 
    shift . 203) (string shift . 204) (unsigned-number shift . 205) (primary 
    shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) (logical-term shift . 213) (
    logical-expression shift . 214) ($:if shift . 215) (simple-expression 
    shift . 216) (expression shift . 510)) ((#{$:\x5d;}# reduce . 335) ($:, 
    reduce . 335)) (($string reduce . 109) ($:annotation reduce . 109) (
    #{$:;}# reduce . 109)) ((#{$:\x7d;}# reduce . 111) ($:, reduce . 111)) ((
    #{$:\x7d;}# shift . 508) ($:, shift . 509)) (($string reduce . 117) (
    $:annotation reduce . 117) (#{$:;}# reduce . 117) (#{$:\x29;}# reduce . 
    117) ($:, reduce . 117)) ((#{$:;}# reduce . 81)) ((#{$:;}# shift . 507)) (
    ($:annotation reduce . 65) ($:end reduce . 65)) (($:annotation reduce . 66
    ) ($:end reduce . 66)) (($:. reduce . 315) ($::= reduce . 315) (
    #{$:\x28;}# reduce . 315) ($:= reduce . 315) ($:, reduce . 315) (
    #{$:\x29;}# reduce . 315)) (($:. reduce . 311) ($::= reduce . 311) (
    #{$:\x28;}# reduce . 311) ($:= reduce . 311) ($:, reduce . 311) (
    #{$:\x29;}# reduce . 311)) ((#{$:\x5b;}# shift . 134) (array-subscripts 
    shift . 505) ($P12 shift . 506) (#{$:\x29;}# reduce . 308) ($:, reduce . 
    308) ($:= reduce . 308) ($::= reduce . 308) (#{$:\x28;}# reduce . 308)) ((
    #{$:;}# reduce . 76) ($:annotation reduce . 76)) ((#{$:\x29;}# shift . 504
    ) ($:, shift . 425)) ((#{$:\x28;}# shift . 503)) (($:annotation reduce . 
    67) ($:end reduce . 67)) (($:, shift . 502)) (($string shift . 50) ($float
    shift . 195) ($fixed shift . 196) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# 
    shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) ($:true shift 
    . 202) ($:false shift . 203) (string shift . 204) (unsigned-number shift 
    . 205) (primary shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) ($ident shift . 26) (logical-term shift 
    . 213) ($:. shift . 27) (ident shift . 28) (logical-expression shift . 214
    ) ($:connect shift . 255) ($:when shift . 256) ($:for shift . 257) ($:if 
    shift . 258) (name shift . 259) (when-equation shift . 260) (
    connect-clause shift . 261) (for-equation shift . 262) (if-equation shift 
    . 263) (simple-expression shift . 264) (equation-1 shift . 265) (equation 
    shift . 266) (equation-list shift . 501) ($:elseif reduce . 213) ($:else 
    reduce . 213) ($:end reduce . 213) ($:elsewhen reduce . 213)) (($:elsewhen
    shift . 498) (elsewhen-eq-part shift . 499) (elsewhen-eq-list shift . 500
    )) (($ident shift . 26) (ident shift . 295) (for-index shift . 497)) ((
    $string shift . 50) ($float shift . 195) ($fixed shift . 196) (#{$:\x7b;}#
    shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der 
    shift . 200) ($:true shift . 202) ($:false shift . 203) (string shift . 
    204) (unsigned-number shift . 205) (primary shift . 206) (factor shift . 
    207) (term shift . 208) (arithmetic-expression shift . 209) ($:not shift 
    . 210) (relation shift . 211) (logical-factor shift . 212) ($ident shift 
    . 26) (logical-term shift . 213) ($:. shift . 27) (ident shift . 28) (
    logical-expression shift . 214) ($:connect shift . 255) ($:when shift . 
    256) ($:for shift . 257) ($:if shift . 258) (name shift . 259) (
    when-equation shift . 260) (connect-clause shift . 261) (for-equation 
    shift . 262) (if-equation shift . 263) (simple-expression shift . 264) (
    equation-1 shift . 265) (equation shift . 266) (equation-list shift . 495)
    ($:end shift . 496)) (($:elseif shift . 489) (elseif-eq-part shift . 490)
    (elseif-eq-list shift . 491) ($:else shift . 492) (else-eq-part shift . 
    493) ($:end shift . 494)) ((#{$:;}# reduce . 190) ($string reduce . 190) (
    $:annotation reduce . 190)) (($float reduce . 188) ($fixed reduce . 188) (
    $string reduce . 188) (#{$:\x7b;}# reduce . 188) (#{$:\x5b;}# reduce . 188
    ) (#{$:\x28;}# reduce . 188) ($:der reduce . 188) ($:true reduce . 188) (
    $:false reduce . 188) ($:not reduce . 188) ($:if reduce . 188) ($:for 
    reduce . 188) ($:connect reduce . 188) ($:when reduce . 188) ($ident 
    reduce . 188) ($:. reduce . 188) ($:annotation reduce . 188) ($:equation 
    reduce . 188) ($:algorithm reduce . 188) ($:initial reduce . 188) (
    $:protected reduce . 188) ($:public reduce . 188) ($:external reduce . 188
    ) ($:end reduce . 188) ($:elseif reduce . 188) ($:else reduce . 188) (
    $:elsewhen reduce . 188)) (($ident shift . 26) ($:. shift . 241) (ident 
    shift . 268) (component-reference-1 shift . 242) ($:when shift . 269) (
    $:while shift . 270) ($:for shift . 271) ($:if shift . 272) (
    when-statement shift . 273) (while-statement shift . 274) (for-statement 
    shift . 275) (if-statement shift . 276) ($:return shift . 277) ($:break 
    shift . 278) (#{$:\x28;}# shift . 279) (component-reference shift . 280) (
    statement-1 shift . 281) (statement shift . 282) (statement-list shift . 
    488) ($:elseif reduce . 225) ($:else reduce . 225) ($:end reduce . 225) (
    $:elsewhen reduce . 225)) (($:elsewhen shift . 485) (elsewhen-st-part 
    shift . 486) (elsewhen-st-list shift . 487)) (($ident shift . 26) ($:. 
    shift . 241) (ident shift . 268) (component-reference-1 shift . 242) (
    $:when shift . 269) ($:while shift . 270) ($:for shift . 271) ($:if shift 
    . 272) (when-statement shift . 273) (while-statement shift . 274) (
    for-statement shift . 275) (if-statement shift . 276) ($:return shift . 
    277) ($:break shift . 278) (#{$:\x28;}# shift . 279) (component-reference 
    shift . 280) (statement-1 shift . 281) (statement shift . 282) (
    statement-list shift . 483) ($:end shift . 484)) (($ident shift . 26) ($:.
    shift . 241) (ident shift . 268) (component-reference-1 shift . 242) (
    $:when shift . 269) ($:while shift . 270) ($:for shift . 271) ($:if shift 
    . 272) (when-statement shift . 273) (while-statement shift . 274) (
    for-statement shift . 275) (if-statement shift . 276) ($:return shift . 
    277) ($:break shift . 278) (#{$:\x28;}# shift . 279) (component-reference 
    shift . 280) (statement-1 shift . 281) (statement shift . 282) (
    statement-list shift . 481) ($:end shift . 482)) (($ident shift . 26) (
    $string shift . 50) ($float shift . 195) ($fixed shift . 196) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 
    198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) (
    $:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 209) ($:not shift . 210) 
    (relation shift . 211) (logical-factor shift . 212) (logical-term shift . 
    213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 480)) (($:elseif shift 
    . 474) (elseif-st-part shift . 475) (elseif-st-list shift . 476) ($:else 
    shift . 477) (else-st-part shift . 478) ($:end shift . 479)) (($::= shift 
    . 473)) ((#{$:\x29;}# shift . 472)) (($:= reduce . 317) ($:: reduce . 317)
    ($:^ reduce . 317) ($:.^ reduce . 317) ($:./ reduce . 317) ($:.* reduce 
    . 317) ($:/ reduce . 317) ($:* reduce . 317) ($:+ reduce . 317) ($:- 
    reduce . 317) ($:.+ reduce . 317) ($:.- reduce . 317) ($:<> reduce . 317) 
    ($:== reduce . 317) ($:>= reduce . 317) ($:> reduce . 317) ($:<= reduce . 
    317) ($:< reduce . 317) ($:and reduce . 317) ($:or reduce . 317) (
    #{$:\x5d;}# reduce . 317) ($:, reduce . 317) ($:for reduce . 317) (#{$:;}#
    reduce . 317) (#{$:\x29;}# reduce . 317) ($:then reduce . 317) ($:loop 
    reduce . 317) ($:else reduce . 317) ($:elseif reduce . 317) ($string 
    reduce . 317) ($:annotation reduce . 317) ($:constrainedby reduce . 317) (
    #{$:\x7d;}# reduce . 317) ($:end reduce . 317) ($:elsewhen reduce . 317) (
    $:if reduce . 317)) ((#{$:;}# reduce . 199) ($string reduce . 199) (
    $:annotation reduce . 199)) (($ident reduce . 197) ($:. reduce . 197) (
    $:if reduce . 197) ($:for reduce . 197) ($:while reduce . 197) ($:when 
    reduce . 197) ($:return reduce . 197) ($:break reduce . 197) (#{$:\x28;}# 
    reduce . 197) ($:annotation reduce . 197) ($:equation reduce . 197) (
    $:algorithm reduce . 197) ($:initial reduce . 197) ($:protected reduce . 
    197) ($:public reduce . 197) ($:external reduce . 197) ($:end reduce . 197
    ) ($:elseif reduce . 197) ($:else reduce . 197) ($:elsewhen reduce . 197))
    (($:= reduce . 316) ($:: reduce . 316) ($:^ reduce . 316) ($:.^ reduce . 
    316) ($:./ reduce . 316) ($:.* reduce . 316) ($:/ reduce . 316) ($:* 
    reduce . 316) ($:+ reduce . 316) ($:- reduce . 316) ($:.+ reduce . 316) (
    $:.- reduce . 316) ($:<> reduce . 316) ($:== reduce . 316) ($:>= reduce . 
    316) ($:> reduce . 316) ($:<= reduce . 316) ($:< reduce . 316) ($:and 
    reduce . 316) ($:or reduce . 316) (#{$:\x5d;}# reduce . 316) ($:, reduce 
    . 316) ($:for reduce . 316) (#{$:;}# reduce . 316) (#{$:\x29;}# reduce . 
    316) ($:then reduce . 316) ($:loop reduce . 316) ($:else reduce . 316) (
    $:elseif reduce . 316) ($string reduce . 316) ($:annotation reduce . 316) 
    ($:constrainedby reduce . 316) (#{$:\x7d;}# reduce . 316) ($:end reduce . 
    316) ($:elsewhen reduce . 316) ($:if reduce . 316)) (($ident shift . 26) (
    $:. shift . 241) (ident shift . 268) (component-reference-1 shift . 242) (
    component-reference shift . 563)) (($ident shift . 26) ($:. shift . 241) (
    ident shift . 268) (component-reference-1 shift . 242) ($:when shift . 269
    ) ($:while shift . 270) ($:for shift . 271) ($:if shift . 272) (
    when-statement shift . 273) (while-statement shift . 274) (for-statement 
    shift . 275) (if-statement shift . 276) ($:return shift . 277) ($:break 
    shift . 278) (#{$:\x28;}# shift . 279) (component-reference shift . 280) (
    statement-1 shift . 281) (statement shift . 282) (statement-list shift . 
    562) ($:end reduce . 229) ($:elseif reduce . 229) ($:else reduce . 229)) (
    ($:end reduce . 226) ($:elseif reduce . 226) ($:else reduce . 226)) ((
    $:end shift . 559) ($:elseif shift . 474) (elseif-st-part shift . 560) (
    $:else shift . 477) (else-st-part shift . 561)) (($ident shift . 26) ($:. 
    shift . 241) (ident shift . 268) (component-reference-1 shift . 242) (
    $:when shift . 269) ($:while shift . 270) ($:for shift . 271) ($:if shift 
    . 272) (when-statement shift . 273) (while-statement shift . 274) (
    for-statement shift . 275) (if-statement shift . 276) ($:return shift . 
    277) ($:break shift . 278) (#{$:\x28;}# shift . 279) (component-reference 
    shift . 280) (statement-1 shift . 281) (statement shift . 282) (
    statement-list shift . 558) ($:end reduce . 231)) (($:end shift . 557)) ((
    $:if shift . 556)) (($:loop reduce . 238) ($:, reduce . 238) (#{$:\x7d;}# 
    reduce . 238) (#{$:\x29;}# reduce . 238)) (($:end shift . 555) ($ident 
    shift . 26) ($:. shift . 241) (ident shift . 268) (component-reference-1 
    shift . 242) ($:when shift . 269) ($:while shift . 270) ($:for shift . 271
    ) ($:if shift . 272) (when-statement shift . 273) (while-statement shift 
    . 274) (for-statement shift . 275) (if-statement shift . 276) ($:return 
    shift . 277) ($:break shift . 278) (#{$:\x28;}# shift . 279) (
    component-reference shift . 280) (statement-1 shift . 281) (statement 
    shift . 285)) (($:for shift . 554)) (($:end shift . 553) ($ident shift . 
    26) ($:. shift . 241) (ident shift . 268) (component-reference-1 shift . 
    242) ($:when shift . 269) ($:while shift . 270) ($:for shift . 271) ($:if 
    shift . 272) (when-statement shift . 273) (while-statement shift . 274) (
    for-statement shift . 275) (if-statement shift . 276) ($:return shift . 
    277) ($:break shift . 278) (#{$:\x28;}# shift . 279) (component-reference 
    shift . 280) (statement-1 shift . 281) (statement shift . 285)) (($:while 
    shift . 552)) (($ident shift . 26) ($string shift . 50) ($float shift . 
    195) ($fixed shift . 196) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}#
    shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der 
    shift . 200) (name shift . 201) ($:true shift . 202) ($:false shift . 203)
    (string shift . 204) (unsigned-number shift . 205) (primary shift . 206) 
    (factor shift . 207) (term shift . 208) (arithmetic-expression shift . 209
    ) ($:not shift . 210) (relation shift . 211) (logical-factor shift . 212) 
    (logical-term shift . 213) (logical-expression shift . 214) ($:if shift . 
    215) (simple-expression shift . 216) (expression shift . 551)) (($:end 
    reduce . 248) ($:elsewhen reduce . 248)) (($:end shift . 549) ($:elsewhen 
    shift . 485) (elsewhen-st-part shift . 550)) (($ident shift . 26) ($:. 
    shift . 241) (ident shift . 268) (component-reference-1 shift . 242) (
    $:when shift . 269) ($:while shift . 270) ($:for shift . 271) ($:if shift 
    . 272) (when-statement shift . 273) (while-statement shift . 274) (
    for-statement shift . 275) (if-statement shift . 276) ($:return shift . 
    277) ($:break shift . 278) (#{$:\x28;}# shift . 279) (component-reference 
    shift . 280) (statement-1 shift . 281) (statement shift . 285) ($:elseif 
    reduce . 224) ($:else reduce . 224) ($:end reduce . 224) ($:elsewhen 
    reduce . 224)) (($string shift . 50) ($float shift . 195) ($fixed shift . 
    196) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# 
    shift . 199) ($:der shift . 200) ($:true shift . 202) ($:false shift . 203
    ) (string shift . 204) (unsigned-number shift . 205) (primary shift . 206)
    (factor shift . 207) (term shift . 208) (arithmetic-expression shift . 
    209) ($:not shift . 210) (relation shift . 211) (logical-factor shift . 
    212) ($ident shift . 26) (logical-term shift . 213) ($:. shift . 27) (
    ident shift . 28) (logical-expression shift . 214) ($:connect shift . 255)
    ($:when shift . 256) ($:for shift . 257) ($:if shift . 258) (name shift 
    . 259) (when-equation shift . 260) (connect-clause shift . 261) (
    for-equation shift . 262) (if-equation shift . 263) (simple-expression 
    shift . 264) (equation-1 shift . 265) (equation shift . 266) (
    equation-list shift . 548) ($:end reduce . 217) ($:elseif reduce . 217) (
    $:else reduce . 217)) (($:end reduce . 214) ($:elseif reduce . 214) (
    $:else reduce . 214)) (($:end shift . 545) ($:elseif shift . 489) (
    elseif-eq-part shift . 546) ($:else shift . 492) (else-eq-part shift . 547
    )) (($string shift . 50) ($float shift . 195) ($fixed shift . 196) (
    #{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 
    199) ($:der shift . 200) ($:true shift . 202) ($:false shift . 203) (
    string shift . 204) (unsigned-number shift . 205) (primary shift . 206) (
    factor shift . 207) (term shift . 208) (arithmetic-expression shift . 209)
    ($:not shift . 210) (relation shift . 211) (logical-factor shift . 212) (
    $ident shift . 26) (logical-term shift . 213) ($:. shift . 27) (ident 
    shift . 28) (logical-expression shift . 214) ($:connect shift . 255) (
    $:when shift . 256) ($:for shift . 257) ($:if shift . 258) (name shift . 
    259) (when-equation shift . 260) (connect-clause shift . 261) (
    for-equation shift . 262) (if-equation shift . 263) (simple-expression 
    shift . 264) (equation-1 shift . 265) (equation shift . 266) (
    equation-list shift . 544) ($:end reduce . 219)) (($:end shift . 543)) ((
    $:if shift . 542)) (($:end shift . 541) ($string shift . 50) ($float shift
    . 195) ($fixed shift . 196) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift 
    . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) ($:true shift . 202) 
    ($:false shift . 203) (string shift . 204) (unsigned-number shift . 205) (
    primary shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) ($ident shift . 26) (logical-term shift 
    . 213) ($:. shift . 27) (ident shift . 28) (logical-expression shift . 214
    ) ($:connect shift . 255) ($:when shift . 256) ($:for shift . 257) ($:if 
    shift . 258) (name shift . 259) (when-equation shift . 260) (
    connect-clause shift . 261) (for-equation shift . 262) (if-equation shift 
    . 263) (simple-expression shift . 264) (equation-1 shift . 265) (equation 
    shift . 302)) (($:for shift . 540)) (($:loop reduce . 237) ($:, reduce . 
    237) (#{$:\x7d;}# reduce . 237) (#{$:\x29;}# reduce . 237)) (($ident shift
    . 26) ($string shift . 50) ($float shift . 195) ($fixed shift . 196) ($:.
    shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# 
    shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 
    201) ($:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 209) ($:not shift . 210) 
    (relation shift . 211) (logical-factor shift . 212) (logical-term shift . 
    213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 539)) (($:end reduce . 
    243) ($:elsewhen reduce . 243)) (($:end shift . 537) ($:elsewhen shift . 
    498) (elsewhen-eq-part shift . 538)) (($string shift . 50) ($float shift 
    . 195) ($fixed shift . 196) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift 
    . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) ($:true shift . 202) 
    ($:false shift . 203) (string shift . 204) (unsigned-number shift . 205) (
    primary shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) ($ident shift . 26) (logical-term shift 
    . 213) ($:. shift . 27) (ident shift . 28) (logical-expression shift . 214
    ) ($:connect shift . 255) ($:when shift . 256) ($:for shift . 257) ($:if 
    shift . 258) (name shift . 259) (when-equation shift . 260) (
    connect-clause shift . 261) (for-equation shift . 262) (if-equation shift 
    . 263) (simple-expression shift . 264) (equation-1 shift . 265) (equation 
    shift . 302) ($:elseif reduce . 212) ($:else reduce . 212) ($:end reduce 
    . 212) ($:elsewhen reduce . 212)) (($ident shift . 26) ($:. shift . 241) (
    ident shift . 268) (component-reference-1 shift . 242) (
    component-reference shift . 536)) ((#{$:\x29;}# shift . 534) ($ident shift
    . 26) ($string shift . 50) ($float shift . 195) ($fixed shift . 196) ($:.
    shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# 
    shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 
    201) ($:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 209) ($:not shift . 210) 
    (relation shift . 211) (logical-factor shift . 212) (logical-term shift . 
    213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 365) (expression-list 
    shift . 535)) ((#{$:;}# reduce . 75) ($:annotation reduce . 75)) ((
    #{$:\x28;}# reduce . 309) ($::= reduce . 309) ($:= reduce . 309) ($:, 
    reduce . 309) (#{$:\x29;}# reduce . 309)) ((#{$:\x28;}# reduce . 307) (
    $::= reduce . 307) ($:= reduce . 307) ($:, reduce . 307) (#{$:\x29;}# 
    reduce . 307)) (($:annotation reduce . 64) ($:end reduce . 64)) (($string 
    reduce . 110) ($:annotation reduce . 110) (#{$:;}# reduce . 110)) (($ident
    shift . 26) (ident shift . 533)) (($:else shift . 530) ($:elseif shift . 
    531) (elseif-ex-list shift . 532)) (($ident shift . 26) ($string shift . 
    50) ($float shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (
    #{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) ($:true 
    shift . 202) ($:false shift . 203) (string shift . 204) (unsigned-number 
    shift . 205) (primary shift . 206) (factor shift . 207) (term shift . 208)
    (arithmetic-expression shift . 209) ($:not shift . 210) (relation shift 
    . 211) (logical-factor shift . 212) (logical-term shift . 213) (
    logical-expression shift . 529)) ((#{$:\x29;}# reduce . 330) ($:, reduce 
    . 330)) ((#{$:\x5d;}# reduce . 332) ($:, reduce . 332) (#{$:;}# reduce . 
    332) (#{$:\x29;}# reduce . 332) ($:end reduce . 332) ($:elsewhen reduce . 
    332)) (($:, shift . 425) (#{$:\x5d;}# reduce . 302) (#{$:;}# reduce . 302)
    ) ((#{$:\x7d;}# reduce . 324) ($:, reduce . 324) (#{$:\x29;}# reduce . 324
    )) (($ident shift . 26) (ident shift . 517) (named-argument shift . 371) (
    named-arguments shift . 527) (#{$:\x29;}# shift . 528)) (($:= shift . 422)
    ) ((#{$:\x7d;}# reduce . 323) ($:, reduce . 323) (#{$:\x29;}# reduce . 323
    )) ((#{$:\x7d;}# reduce . 320) (#{$:\x29;}# reduce . 320)) (($:, shift . 
    456) (#{$:\x7d;}# reduce . 321) (#{$:\x29;}# reduce . 321)) (($string 
    shift . 50) (string shift . 51) (string-cat shift . 52) (string-comment 
    shift . 184) (comment shift . 526) (#{$:;}# reduce . 340) ($:constrainedby
    reduce . 340) ($:annotation reduce . 340)) (($ident shift . 26) (ident 
    shift . 525)) (($string shift . 50) (string shift . 51) (string-cat shift 
    . 52) (string-comment shift . 184) (comment shift . 524) ($:constrainedby 
    reduce . 340) (#{$:\x29;}# reduce . 340) ($:, reduce . 340) ($:annotation 
    reduce . 340)) (($:constrainedby reduce . 177) (#{$:\x29;}# reduce . 177) 
    ($:, reduce . 177)) ((#{$:\x29;}# reduce . 46) (#{$:;}# reduce . 46)) ((
    #{$:;}# reduce . 44) ($:constrainedby reduce . 44)) ((#{$:\x29;}# shift . 
    584) ($:, shift . 420)) (($:for reduce . 326) ($:, reduce . 326) (
    #{$:\x7d;}# reduce . 326) (#{$:\x29;}# reduce . 326)) (($:or shift . 340) 
    ($:= reduce . 259) (#{$:\x5d;}# reduce . 259) ($:, reduce . 259) ($:for 
    reduce . 259) (#{$:;}# reduce . 259) (#{$:\x29;}# reduce . 259) ($:then 
    reduce . 259) ($:loop reduce . 259) ($:else reduce . 259) ($:elseif reduce
    . 259) ($string reduce . 259) ($:annotation reduce . 259) (
    $:constrainedby reduce . 259) (#{$:\x7d;}# reduce . 259) ($:end reduce . 
    259) ($:elsewhen reduce . 259) ($:if reduce . 259)) (($ident shift . 26) (
    $string shift . 50) ($float shift . 195) ($fixed shift . 196) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 
    198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) (
    $:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 209) ($:not shift . 210) 
    (relation shift . 211) (logical-factor shift . 212) (logical-term shift . 
    213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 583)) (($ident shift . 
    26) ($string shift . 50) ($float shift . 195) ($fixed shift . 196) ($:. 
    shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# 
    shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 
    201) ($:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 209) ($:not shift . 210) 
    (relation shift . 211) (logical-factor shift . 212) (logical-term shift . 
    213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 582)) (($:else shift . 
    580) ($:elseif shift . 581)) ((#{$:\x7d;}# reduce . 112) ($:, reduce . 112
    )) ((#{$:;}# reduce . 74) ($:annotation reduce . 74)) ((#{$:\x29;}# shift 
    . 579) ($:, shift . 425)) ((#{$:\x29;}# shift . 578)) (($:when shift . 577
    )) (($:end reduce . 244) ($:elsewhen reduce . 244)) (($:then shift . 576))
    (($:annotation reduce . 233) ($string reduce . 233) (#{$:;}# reduce . 233
    )) (($:for shift . 575)) (($:annotation reduce . 211) ($string reduce . 
    211) (#{$:;}# reduce . 211)) (($:if shift . 574)) (($string shift . 50) (
    $float shift . 195) ($fixed shift . 196) (#{$:\x7b;}# shift . 197) (
    #{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (
    $:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 209) ($:not shift . 210) 
    (relation shift . 211) (logical-factor shift . 212) ($ident shift . 26) (
    logical-term shift . 213) ($:. shift . 27) (ident shift . 28) (
    logical-expression shift . 214) ($:connect shift . 255) ($:when shift . 
    256) ($:for shift . 257) ($:if shift . 258) (name shift . 259) (
    when-equation shift . 260) (connect-clause shift . 261) (for-equation 
    shift . 262) (if-equation shift . 263) (simple-expression shift . 264) (
    equation-1 shift . 265) (equation shift . 302) ($:end reduce . 218)) ((
    $:if shift . 573)) (($:end reduce . 215) ($:elseif reduce . 215) ($:else 
    reduce . 215)) (($:end shift . 572)) (($string shift . 50) ($float shift 
    . 195) ($fixed shift . 196) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift 
    . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) ($:true shift . 202) 
    ($:false shift . 203) (string shift . 204) (unsigned-number shift . 205) (
    primary shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) ($ident shift . 26) (logical-term shift 
    . 213) ($:. shift . 27) (ident shift . 28) (logical-expression shift . 214
    ) ($:connect shift . 255) ($:when shift . 256) ($:for shift . 257) ($:if 
    shift . 258) (name shift . 259) (when-equation shift . 260) (
    connect-clause shift . 261) (for-equation shift . 262) (if-equation shift 
    . 263) (simple-expression shift . 264) (equation-1 shift . 265) (equation 
    shift . 302) ($:end reduce . 216) ($:elseif reduce . 216) ($:else reduce 
    . 216)) (($:when shift . 571)) (($:end reduce . 249) ($:elsewhen reduce . 
    249)) (($:then shift . 570)) (($:annotation reduce . 241) ($string reduce 
    . 241) (#{$:;}# reduce . 241)) (($:while shift . 569)) (($:annotation 
    reduce . 235) ($string reduce . 235) (#{$:;}# reduce . 235)) (($:for shift
    . 568)) (($:annotation reduce . 223) ($string reduce . 223) (#{$:;}# 
    reduce . 223)) (($:if shift . 567)) (($ident shift . 26) ($:. shift . 241)
    (ident shift . 268) (component-reference-1 shift . 242) ($:when shift . 
    269) ($:while shift . 270) ($:for shift . 271) ($:if shift . 272) (
    when-statement shift . 273) (while-statement shift . 274) (for-statement 
    shift . 275) (if-statement shift . 276) ($:return shift . 277) ($:break 
    shift . 278) (#{$:\x28;}# shift . 279) (component-reference shift . 280) (
    statement-1 shift . 281) (statement shift . 285) ($:end reduce . 230)) ((
    $:if shift . 566)) (($:end reduce . 227) ($:elseif reduce . 227) ($:else 
    reduce . 227)) (($:end shift . 565)) (($ident shift . 26) ($:. shift . 241
    ) (ident shift . 268) (component-reference-1 shift . 242) ($:when shift . 
    269) ($:while shift . 270) ($:for shift . 271) ($:if shift . 272) (
    when-statement shift . 273) (while-statement shift . 274) (for-statement 
    shift . 275) (if-statement shift . 276) ($:return shift . 277) ($:break 
    shift . 278) (#{$:\x28;}# shift . 279) (component-reference shift . 280) (
    statement-1 shift . 281) (statement shift . 285) ($:end reduce . 228) (
    $:elseif reduce . 228) ($:else reduce . 228)) ((#{$:\x28;}# shift . 289) (
    function-call-args shift . 564)) ((#{$:;}# reduce . 201) ($string reduce 
    . 201) ($:annotation reduce . 201)) (($:if shift . 591)) (($:annotation 
    reduce . 221) ($string reduce . 221) (#{$:;}# reduce . 221)) ((
    $:annotation reduce . 222) ($string reduce . 222) (#{$:;}# reduce . 222)) 
    (($:annotation reduce . 234) ($string reduce . 234) (#{$:;}# reduce . 234)
    ) (($:annotation reduce . 240) ($string reduce . 240) (#{$:;}# reduce . 
    240)) (($ident shift . 26) ($:. shift . 241) (ident shift . 268) (
    component-reference-1 shift . 242) ($:when shift . 269) ($:while shift . 
    270) ($:for shift . 271) ($:if shift . 272) (when-statement shift . 273) (
    while-statement shift . 274) (for-statement shift . 275) (if-statement 
    shift . 276) ($:return shift . 277) ($:break shift . 278) (#{$:\x28;}# 
    shift . 279) (component-reference shift . 280) (statement-1 shift . 281) (
    statement shift . 282) (statement-list shift . 590) ($:end reduce . 250) (
    $:elsewhen reduce . 250)) (($:annotation reduce . 247) ($string reduce . 
    247) (#{$:;}# reduce . 247)) (($:if shift . 589)) (($:annotation reduce . 
    209) ($string reduce . 209) (#{$:;}# reduce . 209)) (($:annotation reduce 
    . 210) ($string reduce . 210) (#{$:;}# reduce . 210)) (($:annotation 
    reduce . 232) ($string reduce . 232) (#{$:;}# reduce . 232)) (($ident 
    shift . 26) ($string shift . 50) ($float shift . 195) ($fixed shift . 196)
    ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (
    #{$:\x5b;}# shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (
    name shift . 201) ($:true shift . 202) ($:false shift . 203) (string shift
    . 204) (unsigned-number shift . 205) (primary shift . 206) (factor shift 
    . 207) (term shift . 208) (arithmetic-expression shift . 209) ($:not shift
    . 210) (relation shift . 211) (logical-factor shift . 212) (logical-term 
    shift . 213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 365) (expression-list 
    shift . 588) ($:end reduce . 245) ($:elsewhen reduce . 245)) ((
    $:annotation reduce . 242) ($string reduce . 242) (#{$:;}# reduce . 242)) 
    (($:annotation reduce . 252) ($string reduce . 252) (#{$:;}# reduce . 252)
    ) ((#{$:;}# reduce . 73) ($:annotation reduce . 73)) (($ident shift . 26) 
    ($string shift . 50) ($float shift . 195) ($fixed shift . 196) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 
    198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) (
    $:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 209) ($:not shift . 210) 
    (relation shift . 211) (logical-factor shift . 212) (logical-term shift . 
    213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 587)) (($ident shift . 
    26) ($string shift . 50) ($float shift . 195) ($fixed shift . 196) ($:. 
    shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# 
    shift . 198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 
    201) ($:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 209) ($:not shift . 210) 
    (relation shift . 211) (logical-factor shift . 212) (logical-term shift . 
    213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 586)) (($:then shift . 
    585)) ((#{$:\x5d;}# reduce . 255) ($:, reduce . 255) ($:for reduce . 255) 
    (#{$:;}# reduce . 255) (#{$:\x29;}# reduce . 255) ($:then reduce . 255) (
    $:loop reduce . 255) ($:else reduce . 255) ($:elseif reduce . 255) (
    $string reduce . 255) ($:annotation reduce . 255) ($:constrainedby reduce 
    . 255) (#{$:\x7d;}# reduce . 255) ($:end reduce . 255) ($:elsewhen reduce 
    . 255) ($:if reduce . 255)) (($:for reduce . 325) ($:, reduce . 325) (
    #{$:\x7d;}# reduce . 325) (#{$:\x29;}# reduce . 325)) (($ident shift . 26)
    ($string shift . 50) ($float shift . 195) ($fixed shift . 196) ($:. shift
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 
    198) (#{$:\x28;}# shift . 199) ($:der shift . 200) (name shift . 201) (
    $:true shift . 202) ($:false shift . 203) (string shift . 204) (
    unsigned-number shift . 205) (primary shift . 206) (factor shift . 207) (
    term shift . 208) (arithmetic-expression shift . 209) ($:not shift . 210) 
    (relation shift . 211) (logical-factor shift . 212) (logical-term shift . 
    213) (logical-expression shift . 214) ($:if shift . 215) (
    simple-expression shift . 216) (expression shift . 593)) (($:then shift . 
    592)) ((#{$:\x5d;}# reduce . 254) ($:, reduce . 254) ($:for reduce . 254) 
    (#{$:;}# reduce . 254) (#{$:\x29;}# reduce . 254) ($:then reduce . 254) (
    $:loop reduce . 254) ($:else reduce . 254) ($:elseif reduce . 254) (
    $string reduce . 254) ($:annotation reduce . 254) ($:constrainedby reduce 
    . 254) (#{$:\x7d;}# reduce . 254) ($:end reduce . 254) ($:elsewhen reduce 
    . 254) ($:if reduce . 254)) (($:, shift . 425) ($:end reduce . 246) (
    $:elsewhen reduce . 246)) (($:annotation reduce . 208) ($string reduce . 
    208) (#{$:;}# reduce . 208)) (($ident shift . 26) ($:. shift . 241) (ident
    shift . 268) (component-reference-1 shift . 242) ($:when shift . 269) (
    $:while shift . 270) ($:for shift . 271) ($:if shift . 272) (
    when-statement shift . 273) (while-statement shift . 274) (for-statement 
    shift . 275) (if-statement shift . 276) ($:return shift . 277) ($:break 
    shift . 278) (#{$:\x28;}# shift . 279) (component-reference shift . 280) (
    statement-1 shift . 281) (statement shift . 285) ($:end reduce . 251) (
    $:elsewhen reduce . 251)) (($:annotation reduce . 220) ($string reduce . 
    220) (#{$:;}# reduce . 220)) (($ident shift . 26) ($string shift . 50) (
    $float shift . 195) ($fixed shift . 196) ($:. shift . 27) (ident shift . 
    28) (#{$:\x7b;}# shift . 197) (#{$:\x5b;}# shift . 198) (#{$:\x28;}# shift
    . 199) ($:der shift . 200) (name shift . 201) ($:true shift . 202) (
    $:false shift . 203) (string shift . 204) (unsigned-number shift . 205) (
    primary shift . 206) (factor shift . 207) (term shift . 208) (
    arithmetic-expression shift . 209) ($:not shift . 210) (relation shift . 
    211) (logical-factor shift . 212) (logical-term shift . 213) (
    logical-expression shift . 214) ($:if shift . 215) (simple-expression 
    shift . 216) (expression shift . 594)) (($:else reduce . 256) ($:elseif 
    reduce . 256)) (($:else reduce . 257) ($:elseif reduce . 257))))

(define rto-v
  #($start stored-definition stored-definition stored-definition 
    stored-definition-1 stored-definition-1 stored-definition-2 
    stored-definition-2 stored-definition-2 stored-definition-2 
    class-definition class-definition class-prefixes class-prefixes 
    class-prefixes-1 class-prefixes-1 class-prefixes-1 class-prefixes-1 
    class-prefixes-1 class-prefixes-1 class-prefixes-1 class-prefixes-1 
    class-prefixes-1 class-prefixes-1 class-prefixes-1 class-prefixes-1 
    class-prefixes-1 class-prefixes-1 class-prefixes-1 class-prefixes-1 
    class-specifier class-specifier class-specifier long-class-specifier 
    long-class-specifier long-class-specifier short-class-specifier 
    short-class-specifier short-class-specifier short-class-specifier 
    short-class-specifier filler-1 filler-1 filler-1 der-class-specifier 
    der-class-specifier-1 der-class-specifier-1 base-prefix base-prefix 
    enum-list enum-list enumeration-literal composition composition 
    composition composition composition-1-list composition-1-list 
    composition-1 composition-1 composition-1 composition-1 composition-1 
    composition-1 external-part external-part external-part external-part 
    external-part external-part external-part external-part 
    language-specification external-function-call external-function-call 
    external-function-call external-function-call element-list element-list 
    element element element element element element element $P1 $P1 $P2 $P2 
    $P3 $P3 $P4 $P4 $P5 $P5 $P6 $P6 element-1 element-1 element-1 element-1 
    element-2 element-2 import-clause import-clause-1 import-clause-1 
    import-clause-1 import-clause-2 import-clause-2 import-clause-2 
    import-list import-list extends-clause extends-clause extends-clause 
    extends-clause constraining-clause constraining-clause component-clause 
    component-clause component-clause component-clause type-prefix type-prefix
    type-prefix type-prefix type-prefix type-prefix type-prefix type-prefix-1
    type-prefix-1 type-prefix-2 type-prefix-2 type-prefix-2 type-prefix-3 
    type-prefix-3 type-specifier component-list component-list 
    component-declaration component-declaration condition-attribute 
    declaration $P7 $P7 $P8 $P8 modification modification modification 
    modification class-modification class-modification argument-list 
    argument-list argument argument element-modification-or-replaceable 
    element-modification-or-replaceable element-modification-or-replaceable 
    element-modification-or-replaceable elt-mod-or-repl-1 elt-mod-or-repl-1 
    element-modification $P9 $P9 element-redeclaration $P10 $P10 $P11 $P11 
    elt-redecl-1 elt-redecl-1 elt-redecl-1 element-replaceable 
    element-replaceable component-clause1 short-class-definition 
    equation-section equation-section equation-section equation-section 
    algorithm-section algorithm-section algorithm-section algorithm-section 
    equation-list equation-list equation equation-1 equation-1 equation-1 
    equation-1 equation-1 equation-1 statement-list statement-list statement 
    statement-1 statement-1 statement-1 statement-1 statement-1 statement-1 
    statement-1 statement-1 statement-1 if-equation if-equation if-equation 
    if-equation then-eq-part then-eq-part elseif-eq-list elseif-eq-list 
    elseif-eq-part elseif-eq-part else-eq-part else-eq-part if-statement 
    if-statement if-statement if-statement then-st-part then-st-part 
    elseif-st-list elseif-st-list elseif-st-part elseif-st-part else-st-part 
    else-st-part for-equation for-equation for-statement for-statement 
    for-indices for-indices for-index for-index while-statement 
    while-statement when-equation elsewhen-eq-list elsewhen-eq-list 
    elsewhen-eq-part elsewhen-eq-part when-statement elsewhen-st-list 
    elsewhen-st-list elsewhen-st-part elsewhen-st-part connect-clause 
    expression expression expression elseif-ex-list elseif-ex-list 
    simple-expression simple-expression simple-expression logical-expression 
    logical-expression logical-term logical-term logical-factor logical-factor
    relation relation rel-op rel-op rel-op rel-op rel-op rel-op 
    arithmetic-expression arithmetic-expression add-op add-op add-op add-op 
    term term mul-op mul-op mul-op mul-op factor factor factor primary primary
    primary primary primary primary primary primary primary primary primary 
    expression-list-list expression-list-list name name name 
    component-reference component-reference $P12 $P12 component-reference-1 
    component-reference-1 $P13 $P13 $P14 $P14 function-call-args 
    function-call-args function-arguments function-arguments 
    function-argument-1 function-argument-1 named-arguments named-arguments 
    named-argument function-argument function-argument function-argument 
    output-expression-list output-expression-list output-expression-list 
    expression-list expression-list array-subscripts array-subscript-list 
    array-subscript-list subscript subscript comment comment string-comment 
    string-comment string-cat string-cat opt-annotation opt-annotation 
    annotation unsigned-number unsigned-number ident string))

(define mtab
  '(($string . $string) ($ident . $ident) ($float . $float) ($fixed . $fixed
    ) ("annotation" . $:annotation) ("]" . #{$:\x5d;}#) ("[" . #{$:\x5b;}#) (
    "true" . $:true) ("false" . $:false) (".^" . $:.^) ("^" . $:^) ("./" . 
    $:./) (".*" . $:.*) ("/" . $:/) (".-" . $:.-) (".+" . $:.+) ("-" . $:-) (
    "+" . $:+) ("<>" . $:<>) ("==" . $:==) (">=" . $:>=) (">" . $:>) ("<=" . 
    $:<=) ("<" . $:<) ("not" . $:not) ("and" . $:and) ("or" . $:or) ("connect"
    . $:connect) ("elsewhen" . $:elsewhen) ("when" . $:when) ("while" . 
    $:while) ("in" . $:in) ("loop" . $:loop) ("for" . $:for) ("else" . $:else)
    ("elseif" . $:elseif) ("then" . $:then) ("return" . $:return) ("break" . 
    $:break) ("algorithm" . $:algorithm) ("equation" . $:equation) ("initial" 
    . $:initial) ("each" . $:each) (":=" . $::=) ("if" . $:if) ("output" . 
    $:output) ("input" . $:input) ("constant" . $:constant) ("parameter" . 
    $:parameter) ("discrete" . $:discrete) ("stream" . $:stream) ("flow" . 
    $:flow) ("constrainedby" . $:constrainedby) ("}" . #{$:\x7d;}#) ("{" . 
    #{$:\x7b;}#) ("*" . $:*) ("." . $:.) ("import" . $:import) ("replaceable" 
    . $:replaceable) ("outer" . $:outer) ("inner" . $:inner) ("redeclare" . 
    $:redeclare) ("external" . $:external) ("protected" . $:protected) (
    "public" . $:public) ("," . $:,) ("der" . $:der) (":" . $::) (")" . 
    #{$:\x29;}#) ("(" . #{$:\x28;}#) ("enumeration" . $:enumeration) ("=" . 
    $:=) ("extends" . $:extends) ("end" . $:end) ("pure" . $:pure) ("function"
    . $:function) ("impure" . $:impure) ("package" . $:package) ("type" . 
    $:type) ("connector" . $:connector) ("expandable" . $:expandable) ("block"
    . $:block) ("record" . $:record) ("operator" . $:operator) ("model" . 
    $:model) ("class" . $:class) ("partial" . $:partial) ("encapsulated" . 
    $:encapsulated) ("final" . $:final) (";" . #{$:;}#) ("within" . $:within) 
    ($end . $end)))

;;; end tables
