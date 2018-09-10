;; mach.d/motab.scm

;; Copyright 2016-2017 Matthew R. Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING.LESSER included with the this distribution.

(define len-v
  #(1 0 2 1 3 2 3 2 4 3 3 2 2 1 1 1 2 1 1 2 1 1 1 3 3 2 2 2 1 1 1 1 1 5 7 6 7
    6 6 5 7 7 9 1 3 1 1 1 3 2 4 3 3 2 1 1 2 1 2 1 2 1 1 5 4 4 4 3 3 3 2 1 6 5 
    4 3 1 2 3 1 1 5 4 3 2 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 4 2 1 1 3 3 3 1 1 2 3 
    1 3 4 3 3 2 3 2 4 3 3 2 3 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 3 3 2 2 3 0 1 0 1 
    3 1 2 2 3 2 1 3 1 1 3 2 2 1 1 1 3 0 1 4 0 1 0 1 1 1 1 4 3 4 2 3 2 2 1 3 2 
    2 1 1 2 3 2 3 1 1 1 1 2 2 3 2 3 2 6 1 1 1 1 1 1 7 6 6 5 2 1 1 2 2 1 2 1 7 
    6 6 5 2 1 1 2 2 1 2 1 6 5 6 5 1 3 3 1 6 5 6 1 2 3 4 6 1 2 3 4 6 1 7 6 4 5 
    1 5 3 1 3 1 3 1 2 1 3 1 1 1 1 1 1 1 3 1 1 1 1 1 3 1 1 1 1 1 3 3 1 1 1 1 2 
    2 1 2 3 3 3 1 3 1 2 3 1 4 0 1 2 3 0 1 0 1 3 2 1 1 1 3 1 1 3 3 3 5 4 1 1 1 
    3 1 3 3 1 3 1 1 2 1 0 1 1 3 0 2 2 1 1 1 1))

(define pat-v
  #((($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) (
    $:package shift . 4) ($:type shift . 5) ($:connector shift . 6) (
    $:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (class-prefixes shift 
    . 15) ($:encapsulated shift . 16) (class-definition shift . 17) ($:final 
    shift . 18) ($:within shift . 19) (stored-definition-2 shift . 20) 
    (stored-definition-1 shift . 21) (stored-definition shift . 22) ($end 
    reduce . 1)) (($ident reduce . 28) ($:extends reduce . 28)) (($:operator 
    shift . 46) ($:function shift . 47)) (($:operator shift . 44) ($:function 
    shift . 45)) (($ident reduce . 22) ($:extends reduce . 22)) (($ident 
    reduce . 21) ($:extends reduce . 21)) (($ident reduce . 20) ($:extends 
    reduce . 20)) (($:connector shift . 43)) (($ident reduce . 18) ($:extends 
    reduce . 18)) (($ident reduce . 17) ($:extends reduce . 17)) (($:record 
    shift . 41) ($:function shift . 42) ($ident reduce . 29) ($:extends reduce
    . 29)) (($ident reduce . 15) ($:extends reduce . 15)) (($ident reduce . 14
    ) ($:extends reduce . 14)) (($:extends reduce . 13) ($ident reduce . 13)) 
    (($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) 
    ($:model shift . 11) ($:class shift . 12) (class-prefixes-1 shift . 40)) 
    (($ident shift . 26) ($:extends shift . 34) (ident shift . 35) (
    der-class-specifier shift . 36) (short-class-specifier shift . 37) 
    (long-class-specifier shift . 38) (class-specifier shift . 39)) ((
    $:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package 
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) 
    ($:model shift . 11) ($:class shift . 12) (class-prefixes-1 shift . 13) 
    ($:partial shift . 14) (class-prefixes shift . 33)) ((#{$:;}# shift . 32))
    (($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) 
    ($:model shift . 11) ($:class shift . 12) (class-prefixes-1 shift . 13) 
    ($:partial shift . 14) (class-prefixes shift . 15) ($:encapsulated shift 
    . 16) (class-definition shift . 31)) (($ident shift . 26) ($:. shift . 27)
    (ident shift . 28) (name shift . 29) (#{$:;}# shift . 30)) (($:final shift
    . 24) ($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) 
    ($:package shift . 4) ($:type shift . 5) ($:connector shift . 6) (
    $:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (class-prefixes shift 
    . 15) ($:encapsulated shift . 16) (class-definition shift . 25) ($end 
    reduce . 3)) (($:function shift . 1) ($:pure shift . 2) ($:impure shift . 
    3) ($:package shift . 4) ($:type shift . 5) ($:connector shift . 6) 
    ($:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (class-prefixes shift 
    . 15) ($:encapsulated shift . 16) (class-definition shift . 17) ($:final 
    shift . 18) (stored-definition-2 shift . 23)) (($end accept . 0)) (
    ($:final shift . 24) ($:function shift . 1) ($:pure shift . 2) ($:impure 
    shift . 3) ($:package shift . 4) ($:type shift . 5) ($:connector shift . 6
    ) ($:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) 
    ($:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (class-prefixes shift 
    . 15) ($:encapsulated shift . 16) (class-definition shift . 25) ($end 
    reduce . 2)) (($:function shift . 1) ($:pure shift . 2) ($:impure shift . 
    3) ($:package shift . 4) ($:type shift . 5) ($:connector shift . 6) 
    ($:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (class-prefixes shift 
    . 15) ($:encapsulated shift . 16) (class-definition shift . 62)) ((#{$:;}#
    shift . 61)) (($:import reduce . 352) ($:extends reduce . 352) ($:function
    reduce . 352) ($:pure reduce . 352) ($:impure reduce . 352) ($:package 
    reduce . 352) ($:type reduce . 352) ($:connector reduce . 352) (
    $:expandable reduce . 352) ($:block reduce . 352) ($:record reduce . 352) 
    ($:operator reduce . 352) ($:model reduce . 352) ($:class reduce . 352) 
    ($:partial reduce . 352) ($:encapsulated reduce . 352) ($:stream reduce . 
    352) ($:flow reduce . 352) ($:constant reduce . 352) ($:parameter reduce 
    . 352) ($:discrete reduce . 352) ($:output reduce . 352) ($:input reduce 
    . 352) ($ident reduce . 352) ($:. reduce . 352) ($:replaceable reduce . 
    352) ($:outer reduce . 352) ($:inner reduce . 352) ($:final reduce . 352) 
    ($:redeclare reduce . 352) ($string reduce . 352) ($:= reduce . 352) 
    (#{$:;}# reduce . 352) (#{$:\x28;}# reduce . 352) (#{$:\x5b;}# reduce . 
    352) ($::= reduce . 352) (#{$:\x29;}# reduce . 352) ($:, reduce . 352) 
    ($:annotation reduce . 352) ($:if reduce . 352) ($:: reduce . 352) 
    ($:^ reduce . 352) ($:.^ reduce . 352) ($:./ reduce . 352) ($:.* reduce . 
    352) ($:/ reduce . 352) ($:* reduce . 352) ($:+ reduce . 352) ($:- reduce 
    . 352) ($:.+ reduce . 352) ($:.- reduce . 352) ($:<> reduce . 352) 
    ($:== reduce . 352) ($:>= reduce . 352) ($:> reduce . 352) ($:<= reduce . 
    352) ($:< reduce . 352) ($:and reduce . 352) ($:or reduce . 352) (
    #{$:\x5d;}# reduce . 352) (#{$:\x7d;}# reduce . 352) ($:then reduce . 352)
    ($:loop reduce . 352) ($:in reduce . 352) ($:else reduce . 352) ($:elseif 
    reduce . 352) ($:constrainedby reduce . 352) ($:end reduce . 352) (
    $:elsewhen reduce . 352)) (($ident shift . 26) (ident shift . 60)) 
    ((#{$:;}# reduce . 304) ($:. reduce . 304) (#{$:\x5b;}# reduce . 304) 
    ($ident reduce . 304) (#{$:\x29;}# reduce . 304) ($string reduce . 304) 
    (#{$:\x28;}# reduce . 304) ($::= reduce . 304) ($:= reduce . 304) ($:, 
    reduce . 304) ($:annotation reduce . 304) ($:: reduce . 304) ($:or reduce 
    . 304) ($:and reduce . 304) ($:< reduce . 304) ($:<= reduce . 304) 
    ($:> reduce . 304) ($:>= reduce . 304) ($:== reduce . 304) ($:<> reduce . 
    304) ($:.- reduce . 304) ($:.+ reduce . 304) ($:- reduce . 304) ($:+ 
    reduce . 304) ($:* reduce . 304) ($:/ reduce . 304) ($:.* reduce . 304) 
    ($:./ reduce . 304) ($:.^ reduce . 304) ($:^ reduce . 304) (#{$:\x5d;}# 
    reduce . 304) ($:then reduce . 304) ($:loop reduce . 304) ($:else reduce 
    . 304) ($:elseif reduce . 304) ($:constrainedby reduce . 304) (#{$:\x7d;}#
    reduce . 304) ($:end reduce . 304) ($:elsewhen reduce . 304) ($:stream 
    reduce . 304) ($:flow reduce . 304) ($:constant reduce . 304) ($:parameter
    reduce . 304) ($:discrete reduce . 304) ($:output reduce . 304) ($:input 
    reduce . 304) ($:if reduce . 304)) ((#{$:;}# shift . 58) ($:. shift . 59))
    (($:function reduce . 5) ($:pure reduce . 5) ($:impure reduce . 5) 
    ($:package reduce . 5) ($:type reduce . 5) ($:connector reduce . 5) 
    ($:expandable reduce . 5) ($:block reduce . 5) ($:record reduce . 5) 
    ($:operator reduce . 5) ($:model reduce . 5) ($:class reduce . 5) (
    $:partial reduce . 5) ($:encapsulated reduce . 5) ($:final reduce . 5)) 
    ((#{$:;}# shift . 57)) (($:function reduce . 7) ($:pure reduce . 7) 
    ($:impure reduce . 7) ($:package reduce . 7) ($:type reduce . 7) (
    $:connector reduce . 7) ($:expandable reduce . 7) ($:block reduce . 7) 
    ($:record reduce . 7) ($:operator reduce . 7) ($:model reduce . 7) 
    ($:class reduce . 7) ($:partial reduce . 7) ($:encapsulated reduce . 7) 
    ($:final reduce . 7) ($end reduce . 7)) (($ident shift . 26) ($:extends 
    shift . 34) (ident shift . 35) (der-class-specifier shift . 36) (
    short-class-specifier shift . 37) (long-class-specifier shift . 38) 
    (class-specifier shift . 56)) (($ident shift . 26) (ident shift . 55)) 
    (($string shift . 50) (string shift . 51) (string-cat shift . 52) (
    string-comment shift . 53) ($:= shift . 54) ($:import reduce . 343) 
    ($:extends reduce . 343) ($:function reduce . 343) ($:pure reduce . 343) 
    ($:impure reduce . 343) ($:package reduce . 343) ($:type reduce . 343) 
    ($:connector reduce . 343) ($:expandable reduce . 343) ($:block reduce . 
    343) ($:record reduce . 343) ($:operator reduce . 343) ($:model reduce . 
    343) ($:class reduce . 343) ($:partial reduce . 343) ($:encapsulated 
    reduce . 343) ($:stream reduce . 343) ($:flow reduce . 343) ($:constant 
    reduce . 343) ($:parameter reduce . 343) ($:discrete reduce . 343) 
    ($:output reduce . 343) ($:input reduce . 343) ($ident reduce . 343) 
    ($:. reduce . 343) ($:replaceable reduce . 343) ($:outer reduce . 343) 
    ($:inner reduce . 343) ($:final reduce . 343) ($:redeclare reduce . 343)) 
    ((#{$:;}# reduce . 32) ($:constrainedby reduce . 32)) ((#{$:;}# reduce . 
    31) ($:constrainedby reduce . 31)) ((#{$:;}# reduce . 30) ($:constrainedby
    reduce . 30)) ((#{$:;}# reduce . 11) ($:constrainedby reduce . 11)) 
    (($:extends reduce . 12) ($ident reduce . 12)) (($ident reduce . 16) 
    ($:extends reduce . 16)) (($ident reduce . 27) ($:extends reduce . 27)) 
    (($ident reduce . 19) ($:extends reduce . 19)) (($:function shift . 49)) 
    (($ident reduce . 25) ($:extends reduce . 25)) (($:function shift . 48)) 
    (($ident reduce . 26) ($:extends reduce . 26)) (($ident reduce . 24) 
    ($:extends reduce . 24)) (($ident reduce . 23) ($:extends reduce . 23)) 
    (($:import reduce . 353) ($:extends reduce . 353) ($:function reduce . 353
    ) ($:pure reduce . 353) ($:impure reduce . 353) ($:package reduce . 353) 
    ($:type reduce . 353) ($:connector reduce . 353) ($:expandable reduce . 
    353) ($:block reduce . 353) ($:record reduce . 353) ($:operator reduce . 
    353) ($:model reduce . 353) ($:class reduce . 353) ($:partial reduce . 353
    ) ($:encapsulated reduce . 353) ($:stream reduce . 353) ($:flow reduce . 
    353) ($:constant reduce . 353) ($:parameter reduce . 353) ($:discrete 
    reduce . 353) ($:output reduce . 353) ($:input reduce . 353) ($ident 
    reduce . 353) ($:. reduce . 353) ($:replaceable reduce . 353) ($:outer 
    reduce . 353) ($:inner reduce . 353) ($:final reduce . 353) ($:redeclare 
    reduce . 353) ($:+ reduce . 353) ($:: reduce . 353) ($:= reduce . 353) 
    ($:or reduce . 353) ($:and reduce . 353) ($:< reduce . 353) ($:<= reduce 
    . 353) ($:> reduce . 353) ($:>= reduce . 353) ($:== reduce . 353) ($:<> 
    reduce . 353) ($:.- reduce . 353) ($:.+ reduce . 353) ($:- reduce . 353) 
    ($:* reduce . 353) ($:/ reduce . 353) ($:.* reduce . 353) ($:./ reduce . 
    353) ($:.^ reduce . 353) ($:^ reduce . 353) (#{$:;}# reduce . 353) 
    ($:annotation reduce . 353) (#{$:\x5d;}# reduce . 353) ($:, reduce . 353) 
    (#{$:\x7d;}# reduce . 353) (#{$:\x29;}# reduce . 353) ($:then reduce . 353
    ) ($:loop reduce . 353) ($:else reduce . 353) ($:elseif reduce . 353) 
    ($:constrainedby reduce . 353) ($string reduce . 353) ($:end reduce . 353)
    ($:elsewhen reduce . 353) ($:if reduce . 353)) (($:redeclare reduce . 345)
    ($:final reduce . 345) ($:inner reduce . 345) ($:outer reduce . 345) 
    ($:replaceable reduce . 345) ($:. reduce . 345) ($ident reduce . 345) 
    ($:input reduce . 345) ($:output reduce . 345) ($:discrete reduce . 345) 
    ($:parameter reduce . 345) ($:constant reduce . 345) ($:flow reduce . 345)
    ($:stream reduce . 345) ($:encapsulated reduce . 345) ($:partial reduce . 
    345) ($:class reduce . 345) ($:model reduce . 345) ($:operator reduce . 
    345) ($:record reduce . 345) ($:block reduce . 345) ($:expandable reduce 
    . 345) ($:connector reduce . 345) ($:type reduce . 345) ($:package reduce 
    . 345) ($:impure reduce . 345) ($:pure reduce . 345) ($:function reduce . 
    345) ($:extends reduce . 345) ($:import reduce . 345) ($:+ reduce . 345) 
    ($:annotation reduce . 345) (#{$:;}# reduce . 345) ($:, reduce . 345) 
    ($:constrainedby reduce . 345) (#{$:\x29;}# reduce . 345)) (($:+ shift . 
    102) ($:import reduce . 344) ($:extends reduce . 344) ($:function reduce 
    . 344) ($:pure reduce . 344) ($:impure reduce . 344) ($:package reduce . 
    344) ($:type reduce . 344) ($:connector reduce . 344) ($:expandable reduce
    . 344) ($:block reduce . 344) ($:record reduce . 344) ($:operator reduce 
    . 344) ($:model reduce . 344) ($:class reduce . 344) ($:partial reduce . 
    344) ($:encapsulated reduce . 344) ($:stream reduce . 344) ($:flow reduce 
    . 344) ($:constant reduce . 344) ($:parameter reduce . 344) ($:discrete 
    reduce . 344) ($:output reduce . 344) ($:input reduce . 344) ($ident 
    reduce . 344) ($:. reduce . 344) ($:replaceable reduce . 344) ($:outer 
    reduce . 344) ($:inner reduce . 344) ($:final reduce . 344) ($:redeclare 
    reduce . 344) ($:annotation reduce . 344) (#{$:;}# reduce . 344) ($:, 
    reduce . 344) ($:constrainedby reduce . 344) (#{$:\x29;}# reduce . 344)) 
    (($ident shift . 26) ($:. shift . 27) (ident shift . 28) ($:output shift 
    . 73) ($:input shift . 74) ($:constant shift . 75) ($:parameter shift . 76
    ) ($:discrete shift . 77) ($:stream shift . 78) ($:flow shift . 79) 
    ($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package 
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) 
    ($:model shift . 11) ($:class shift . 12) (name shift . 80) (type-prefix-3
    shift . 81) (type-prefix-2 shift . 82) (type-prefix-1 shift . 83) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (type-specifier shift 
    . 84) (type-prefix shift . 85) (class-prefixes shift . 15) ($:encapsulated
    shift . 16) ($:extends shift . 86) ($:import shift . 87) ($:replaceable 
    shift . 88) (component-clause shift . 89) (class-definition shift . 90) 
    (element-1 shift . 91) ($:outer shift . 92) ($:inner shift . 93) ($:final 
    shift . 94) ($:redeclare shift . 95) (extends-clause shift . 96) (
    import-clause shift . 97) (element shift . 98) (element-list-1 shift . 99)
    (element-list shift . 100) (composition shift . 101)) (($:der shift . 68) 
    ($:enumeration shift . 69) ($:output shift . 70) ($:input shift . 71) 
    (base-prefix shift . 72)) (($string shift . 50) (string shift . 51) 
    (string-cat shift . 52) (string-comment shift . 65) (#{$:\x28;}# shift . 
    66) (class-modification shift . 67) ($:import reduce . 343) ($:extends 
    reduce . 343) ($:function reduce . 343) ($:pure reduce . 343) ($:impure 
    reduce . 343) ($:package reduce . 343) ($:type reduce . 343) ($:connector 
    reduce . 343) ($:expandable reduce . 343) ($:block reduce . 343) ($:record
    reduce . 343) ($:operator reduce . 343) ($:model reduce . 343) ($:class 
    reduce . 343) ($:partial reduce . 343) ($:encapsulated reduce . 343) 
    ($:stream reduce . 343) ($:flow reduce . 343) ($:constant reduce . 343) 
    ($:parameter reduce . 343) ($:discrete reduce . 343) ($:output reduce . 
    343) ($:input reduce . 343) ($ident reduce . 343) ($:. reduce . 343) 
    ($:replaceable reduce . 343) ($:outer reduce . 343) ($:inner reduce . 343)
    ($:final reduce . 343) ($:redeclare reduce . 343)) ((#{$:;}# reduce . 10) 
    ($:constrainedby reduce . 10)) (($:function reduce . 6) ($:pure reduce . 6
    ) ($:impure reduce . 6) ($:package reduce . 6) ($:type reduce . 6) 
    ($:connector reduce . 6) ($:expandable reduce . 6) ($:block reduce . 6) 
    ($:record reduce . 6) ($:operator reduce . 6) ($:model reduce . 6) 
    ($:class reduce . 6) ($:partial reduce . 6) ($:encapsulated reduce . 6) 
    ($:final reduce . 6) ($end reduce . 6)) (($:function reduce . 4) ($:pure 
    reduce . 4) ($:impure reduce . 4) ($:package reduce . 4) ($:type reduce . 
    4) ($:connector reduce . 4) ($:expandable reduce . 4) ($:block reduce . 4)
    ($:record reduce . 4) ($:operator reduce . 4) ($:model reduce . 4) 
    ($:class reduce . 4) ($:partial reduce . 4) ($:encapsulated reduce . 4) 
    ($:final reduce . 4)) (($ident shift . 26) (ident shift . 64)) ((#{$:;}# 
    reduce . 305) ($:. reduce . 305) (#{$:\x5b;}# reduce . 305) ($ident reduce
    . 305) (#{$:\x29;}# reduce . 305) ($string reduce . 305) (#{$:\x28;}# 
    reduce . 305) ($::= reduce . 305) ($:= reduce . 305) ($:, reduce . 305) 
    ($:annotation reduce . 305) ($:: reduce . 305) ($:or reduce . 305) 
    ($:and reduce . 305) ($:< reduce . 305) ($:<= reduce . 305) ($:> reduce . 
    305) ($:>= reduce . 305) ($:== reduce . 305) ($:<> reduce . 305) ($:.- 
    reduce . 305) ($:.+ reduce . 305) ($:- reduce . 305) ($:+ reduce . 305) 
    ($:* reduce . 305) ($:/ reduce . 305) ($:.* reduce . 305) ($:./ reduce . 
    305) ($:.^ reduce . 305) ($:^ reduce . 305) (#{$:\x5d;}# reduce . 305) 
    (#{$:\x7d;}# reduce . 305) ($:then reduce . 305) ($:loop reduce . 305) 
    ($:else reduce . 305) ($:elseif reduce . 305) ($:constrainedby reduce . 
    305) ($:end reduce . 305) ($:elsewhen reduce . 305) ($:stream reduce . 305
    ) ($:flow reduce . 305) ($:constant reduce . 305) ($:parameter reduce . 
    305) ($:discrete reduce . 305) ($:output reduce . 305) ($:input reduce . 
    305) ($:if reduce . 305)) (($:function reduce . 9) ($:pure reduce . 9) 
    ($:impure reduce . 9) ($:package reduce . 9) ($:type reduce . 9) (
    $:connector reduce . 9) ($:expandable reduce . 9) ($:block reduce . 9) 
    ($:record reduce . 9) ($:operator reduce . 9) ($:model reduce . 9) 
    ($:class reduce . 9) ($:partial reduce . 9) ($:encapsulated reduce . 9) 
    ($:final reduce . 9) ($end reduce . 9)) ((#{$:;}# shift . 63)) ((
    $:function reduce . 8) ($:pure reduce . 8) ($:impure reduce . 8) (
    $:package reduce . 8) ($:type reduce . 8) ($:connector reduce . 8) 
    ($:expandable reduce . 8) ($:block reduce . 8) ($:record reduce . 8) 
    ($:operator reduce . 8) ($:model reduce . 8) ($:class reduce . 8) (
    $:partial reduce . 8) ($:encapsulated reduce . 8) ($:final reduce . 8) 
    ($end reduce . 8)) ((#{$:;}# reduce . 306) ($:. reduce . 306) (
    $:annotation reduce . 306) ($string reduce . 306) (#{$:\x5b;}# reduce . 
    306) ($ident reduce . 306) (#{$:\x28;}# reduce . 306) ($:, reduce . 306) 
    ($:constrainedby reduce . 306) (#{$:\x29;}# reduce . 306) ($::= reduce . 
    306) ($:= reduce . 306) ($:: reduce . 306) (#{$:\x5d;}# reduce . 306) 
    ($:or reduce . 306) ($:and reduce . 306) ($:< reduce . 306) ($:<= reduce 
    . 306) ($:> reduce . 306) ($:>= reduce . 306) ($:== reduce . 306) ($:<> 
    reduce . 306) ($:.- reduce . 306) ($:.+ reduce . 306) ($:- reduce . 306) 
    ($:+ reduce . 306) ($:* reduce . 306) ($:/ reduce . 306) ($:.* reduce . 
    306) ($:./ reduce . 306) ($:.^ reduce . 306) ($:^ reduce . 306) (
    #{$:\x7d;}# reduce . 306) ($:then reduce . 306) ($:loop reduce . 306) 
    ($:else reduce . 306) ($:elseif reduce . 306) ($:stream reduce . 306) 
    ($:flow reduce . 306) ($:constant reduce . 306) ($:parameter reduce . 306)
    ($:discrete reduce . 306) ($:output reduce . 306) ($:input reduce . 306) 
    ($:end reduce . 306) ($:elsewhen reduce . 306) ($:if reduce . 306)) 
    (($ident shift . 26) ($:. shift . 27) (ident shift . 28) ($:output shift 
    . 73) ($:input shift . 74) ($:constant shift . 75) ($:parameter shift . 76
    ) ($:discrete shift . 77) ($:stream shift . 78) ($:flow shift . 79) 
    ($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package 
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) 
    ($:model shift . 11) ($:class shift . 12) (name shift . 80) (type-prefix-3
    shift . 81) (type-prefix-2 shift . 82) (type-prefix-1 shift . 83) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (type-specifier shift 
    . 84) (type-prefix shift . 85) (class-prefixes shift . 15) ($:encapsulated
    shift . 16) ($:extends shift . 86) ($:import shift . 87) ($:replaceable 
    shift . 88) (component-clause shift . 89) (class-definition shift . 90) 
    (element-1 shift . 91) ($:outer shift . 92) ($:inner shift . 93) ($:final 
    shift . 94) ($:redeclare shift . 95) (extends-clause shift . 96) (
    import-clause shift . 97) (element shift . 98) (element-list-1 shift . 99)
    (element-list shift . 100) (composition shift . 163)) (($ident shift . 26)
    ($:. shift . 27) (ident shift . 28) ($:replaceable shift . 150) (name 
    shift . 151) (element-replaceable shift . 152) (element-modification shift
    . 153) ($:redeclare shift . 154) (elt-mod-or-repl-1 shift . 155) ($:final 
    shift . 156) ($:each shift . 157) (element-redeclaration shift . 158) 
    (element-modification-or-replaceable shift . 159) (argument shift . 160) 
    (argument-list shift . 161) (#{$:\x29;}# shift . 162)) (($string shift . 
    50) (string shift . 51) (string-cat shift . 52) (string-comment shift . 
    149) ($:import reduce . 343) ($:extends reduce . 343) ($:function reduce 
    . 343) ($:pure reduce . 343) ($:impure reduce . 343) ($:package reduce . 
    343) ($:type reduce . 343) ($:connector reduce . 343) ($:expandable reduce
    . 343) ($:block reduce . 343) ($:record reduce . 343) ($:operator reduce 
    . 343) ($:model reduce . 343) ($:class reduce . 343) ($:partial reduce . 
    343) ($:encapsulated reduce . 343) ($:stream reduce . 343) ($:flow reduce 
    . 343) ($:constant reduce . 343) ($:parameter reduce . 343) ($:discrete 
    reduce . 343) ($:output reduce . 343) ($:input reduce . 343) ($ident 
    reduce . 343) ($:. reduce . 343) ($:replaceable reduce . 343) ($:outer 
    reduce . 343) ($:inner reduce . 343) ($:final reduce . 343) ($:redeclare 
    reduce . 343)) ((#{$:\x28;}# shift . 148)) ((#{$:\x28;}# shift . 147)) 
    (($ident reduce . 46) ($:. reduce . 46)) (($ident reduce . 45) ($:. reduce
    . 45)) (($ident shift . 26) ($:. shift . 27) (ident shift . 28) (name 
    shift . 80) (type-specifier shift . 146)) (($:. reduce . 136) ($ident 
    reduce . 136)) (($:. reduce . 135) ($ident reduce . 135)) (($:. reduce . 
    134) ($ident reduce . 134) ($:output reduce . 134) ($:input reduce . 134))
    (($:. reduce . 133) ($ident reduce . 133) ($:output reduce . 133) ($:input
    reduce . 133)) (($:. reduce . 132) ($ident reduce . 132) ($:output reduce 
    . 132) ($:input reduce . 132)) (($:. reduce . 131) ($ident reduce . 131) 
    ($:output reduce . 131) ($:input reduce . 131) ($:constant reduce . 131) 
    ($:parameter reduce . 131) ($:discrete reduce . 131)) (($:. reduce . 130) 
    ($ident reduce . 130) ($:output reduce . 130) ($:input reduce . 130) 
    ($:constant reduce . 130) ($:parameter reduce . 130) ($:discrete reduce . 
    130)) (($:. shift . 59) ($ident reduce . 137) (#{$:\x5b;}# reduce . 137) 
    ($string reduce . 137) ($:annotation reduce . 137) (#{$:\x28;}# reduce . 
    137) ($:, reduce . 137) (#{$:;}# reduce . 137) ($:constrainedby reduce . 
    137) ($:stream reduce . 137) ($:flow reduce . 137) ($:constant reduce . 
    137) ($:parameter reduce . 137) ($:discrete reduce . 137) ($:output reduce
    . 137) ($:input reduce . 137) (#{$:\x29;}# reduce . 137)) (($ident reduce 
    . 129) ($:. reduce . 129)) (($:output shift . 73) ($:input shift . 74) 
    (type-prefix-3 shift . 145) ($ident reduce . 128) ($:. reduce . 128)) 
    (($:constant shift . 75) ($:parameter shift . 76) ($:discrete shift . 77) 
    (type-prefix-2 shift . 143) ($:output shift . 73) ($:input shift . 74) 
    (type-prefix-3 shift . 144) ($ident reduce . 127) ($:. reduce . 127)) 
    ((#{$:\x5b;}# shift . 137) (array-subscripts shift . 138) ($ident shift . 
    26) (ident shift . 139) (declaration shift . 140) (component-declaration 
    shift . 141) (component-list shift . 142)) (($ident shift . 26) ($:. shift
    . 27) (ident shift . 28) (name shift . 80) (type-specifier shift . 136)) 
    (($ident shift . 26) ($:. shift . 27) (ident shift . 28) (name shift . 135
    )) (($ident shift . 26) ($:. shift . 27) (name shift . 132) (ident shift 
    . 133) (import-clause-1 shift . 134)) (($ident shift . 26) ($:. shift . 27
    ) (ident shift . 28) ($:output shift . 73) ($:input shift . 74) (
    $:constant shift . 75) ($:parameter shift . 76) ($:discrete shift . 77) 
    ($:stream shift . 78) ($:flow shift . 79) ($:function shift . 1) ($:pure 
    shift . 2) ($:impure shift . 3) ($:package shift . 4) ($:type shift . 5) 
    ($:connector shift . 6) ($:expandable shift . 7) ($:block shift . 8) 
    ($:record shift . 9) ($:operator shift . 10) ($:model shift . 11) ($:class
    shift . 12) (name shift . 80) (type-prefix-3 shift . 81) (type-prefix-2 
    shift . 82) (type-prefix-1 shift . 83) (class-prefixes-1 shift . 13) 
    ($:partial shift . 14) (type-specifier shift . 84) (type-prefix shift . 85
    ) (class-prefixes shift . 15) ($:encapsulated shift . 16) (
    component-clause shift . 129) (class-definition shift . 130) (element-2 
    shift . 131)) ((#{$:;}# reduce . 99)) ((#{$:;}# reduce . 98)) ((#{$:;}# 
    reduce . 85)) (($ident shift . 26) ($:. shift . 27) (ident shift . 28) 
    ($:output shift . 73) ($:input shift . 74) ($:constant shift . 75) 
    ($:parameter shift . 76) ($:discrete shift . 77) ($:stream shift . 78) 
    ($:flow shift . 79) ($:function shift . 1) ($:pure shift . 2) ($:impure 
    shift . 3) ($:package shift . 4) ($:type shift . 5) ($:connector shift . 6
    ) ($:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) 
    ($:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (name 
    shift . 80) (type-prefix-3 shift . 81) (type-prefix-2 shift . 82) (
    type-prefix-1 shift . 83) (class-prefixes-1 shift . 13) ($:partial shift 
    . 14) (type-specifier shift . 84) (type-prefix shift . 85) (class-prefixes
    shift . 15) ($:encapsulated shift . 16) ($:replaceable shift . 88) 
    (component-clause shift . 89) (class-definition shift . 90) (element-1 
    shift . 128)) (($:outer shift . 126) ($P6 shift . 127) ($:function reduce 
    . 96) ($:pure reduce . 96) ($:impure reduce . 96) ($:package reduce . 96) 
    ($:type reduce . 96) ($:connector reduce . 96) ($:expandable reduce . 96) 
    ($:block reduce . 96) ($:record reduce . 96) ($:operator reduce . 96) 
    ($:model reduce . 96) ($:class reduce . 96) ($:partial reduce . 96) 
    ($:encapsulated reduce . 96) ($:stream reduce . 96) ($:flow reduce . 96) 
    ($:constant reduce . 96) ($:parameter reduce . 96) ($:discrete reduce . 96
    ) ($:output reduce . 96) ($:input reduce . 96) ($ident reduce . 96) 
    ($:. reduce . 96) ($:replaceable reduce . 96)) (($:inner shift . 124) 
    ($P4 shift . 125) ($:function reduce . 92) ($:pure reduce . 92) ($:impure 
    reduce . 92) ($:package reduce . 92) ($:type reduce . 92) ($:connector 
    reduce . 92) ($:expandable reduce . 92) ($:block reduce . 92) ($:record 
    reduce . 92) ($:operator reduce . 92) ($:model reduce . 92) ($:class 
    reduce . 92) ($:partial reduce . 92) ($:encapsulated reduce . 92) (
    $:stream reduce . 92) ($:flow reduce . 92) ($:constant reduce . 92) 
    ($:parameter reduce . 92) ($:discrete reduce . 92) ($:output reduce . 92) 
    ($:input reduce . 92) ($ident reduce . 92) ($:. reduce . 92) (
    $:replaceable reduce . 92) ($:outer reduce . 92)) (($:final shift . 122) 
    ($P1 shift . 123) ($:function reduce . 86) ($:pure reduce . 86) ($:impure 
    reduce . 86) ($:package reduce . 86) ($:type reduce . 86) ($:connector 
    reduce . 86) ($:expandable reduce . 86) ($:block reduce . 86) ($:record 
    reduce . 86) ($:operator reduce . 86) ($:model reduce . 86) ($:class 
    reduce . 86) ($:partial reduce . 86) ($:encapsulated reduce . 86) (
    $:stream reduce . 86) ($:flow reduce . 86) ($:constant reduce . 86) 
    ($:parameter reduce . 86) ($:discrete reduce . 86) ($:output reduce . 86) 
    ($:input reduce . 86) ($ident reduce . 86) ($:. reduce . 86) (
    $:replaceable reduce . 86) ($:outer reduce . 86) ($:inner reduce . 86)) 
    ((#{$:;}# reduce . 80)) ((#{$:;}# reduce . 79)) ((#{$:;}# shift . 121)) 
    (($ident shift . 26) ($:. shift . 27) (ident shift . 28) ($:output shift 
    . 73) ($:input shift . 74) ($:constant shift . 75) ($:parameter shift . 76
    ) ($:discrete shift . 77) ($:stream shift . 78) ($:flow shift . 79) 
    ($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package 
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) 
    ($:model shift . 11) ($:class shift . 12) (name shift . 80) (type-prefix-3
    shift . 81) (type-prefix-2 shift . 82) (type-prefix-1 shift . 83) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (type-specifier shift 
    . 84) (type-prefix shift . 85) (class-prefixes shift . 15) ($:encapsulated
    shift . 16) ($:extends shift . 86) ($:import shift . 87) ($:replaceable 
    shift . 88) (component-clause shift . 89) (class-definition shift . 90) 
    (element-1 shift . 91) ($:outer shift . 92) ($:inner shift . 93) ($:final 
    shift . 94) ($:redeclare shift . 95) (extends-clause shift . 96) (
    import-clause shift . 97) (element shift . 120) ($:end reduce . 76) 
    ($:annotation reduce . 76) ($:external reduce . 76) ($:equation reduce . 
    76) ($:algorithm reduce . 76) ($:initial reduce . 76) ($:protected reduce 
    . 76) ($:public reduce . 76)) (($:algorithm shift . 105) ($:equation shift
    . 106) ($:initial shift . 107) (algorithm-section shift . 108) (
    equation-section shift . 109) ($:protected shift . 110) ($:public shift . 
    111) (composition-1 shift . 112) (composition-1-list-1 shift . 113) 
    (composition-1-list shift . 114) ($:external shift . 115) (external-part 
    shift . 116) ($:annotation shift . 117) (annotation shift . 118) (
    opt-annotation shift . 119) ($:end reduce . 347)) (($:end shift . 104)) 
    (($string shift . 50) (string shift . 103)) (($:redeclare reduce . 346) 
    ($:final reduce . 346) ($:inner reduce . 346) ($:outer reduce . 346) 
    ($:replaceable reduce . 346) ($:. reduce . 346) ($ident reduce . 346) 
    ($:input reduce . 346) ($:output reduce . 346) ($:discrete reduce . 346) 
    ($:parameter reduce . 346) ($:constant reduce . 346) ($:flow reduce . 346)
    ($:stream reduce . 346) ($:encapsulated reduce . 346) ($:partial reduce . 
    346) ($:class reduce . 346) ($:model reduce . 346) ($:operator reduce . 
    346) ($:record reduce . 346) ($:block reduce . 346) ($:expandable reduce 
    . 346) ($:connector reduce . 346) ($:type reduce . 346) ($:package reduce 
    . 346) ($:impure reduce . 346) ($:pure reduce . 346) ($:function reduce . 
    346) ($:extends reduce . 346) ($:import reduce . 346) ($:+ reduce . 346) 
    ($:annotation reduce . 346) (#{$:;}# reduce . 346) ($:, reduce . 346) 
    ($:constrainedby reduce . 346) (#{$:\x29;}# reduce . 346)) (($ident shift 
    . 26) (ident shift . 287)) (($ident shift . 26) ($:. shift . 243) (ident 
    shift . 271) (component-reference-1 shift . 244) ($:when shift . 272) 
    ($:while shift . 273) ($:for shift . 274) ($:if shift . 275) (
    when-statement shift . 276) (while-statement shift . 277) (for-statement 
    shift . 278) (if-statement shift . 279) ($:return shift . 280) ($:break 
    shift . 281) (#{$:\x28;}# shift . 282) (component-reference shift . 283) 
    (statement-1 shift . 284) (statement shift . 285) (statement-list shift . 
    286) ($:annotation reduce . 186) ($:public reduce . 186) ($:protected 
    reduce . 186) ($:initial reduce . 186) ($:algorithm reduce . 186) (
    $:equation reduce . 186) ($:external reduce . 186) ($:end reduce . 186)) 
    (($string shift . 50) ($float shift . 197) ($fixed shift . 198) (
    #{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 
    201) ($:der shift . 202) ($:true shift . 204) ($:false shift . 205) 
    (string shift . 206) (unsigned-number shift . 207) (primary shift . 208) 
    (factor shift . 209) (term shift . 210) (arithmetic-expression shift . 211
    ) ($:not shift . 212) (relation shift . 213) (logical-factor shift . 214) 
    ($ident shift . 26) (logical-term shift . 215) ($:. shift . 27) (ident 
    shift . 28) (logical-expression shift . 216) ($:connect shift . 257) 
    ($:when shift . 258) ($:for shift . 259) ($:if shift . 260) (name shift . 
    261) (when-equation shift . 262) (connect-clause shift . 263) (
    for-equation shift . 264) (if-equation shift . 265) (simple-expression 
    shift . 266) (equation-1 shift . 267) (equation shift . 268) (
    equation-list-1 shift . 269) (equation-list shift . 270) ($:annotation 
    reduce . 182) ($:public reduce . 182) ($:protected reduce . 182) (
    $:initial reduce . 182) ($:algorithm reduce . 182) ($:equation reduce . 
    182) ($:external reduce . 182) ($:end reduce . 182)) (($:equation shift . 
    255) ($:algorithm shift . 256)) (($:annotation reduce . 62) ($:equation 
    reduce . 62) ($:algorithm reduce . 62) ($:initial reduce . 62) (
    $:protected reduce . 62) ($:public reduce . 62) ($:external reduce . 62) 
    ($:end reduce . 62)) (($:annotation reduce . 61) ($:equation reduce . 61) 
    ($:algorithm reduce . 61) ($:initial reduce . 61) ($:protected reduce . 61
    ) ($:public reduce . 61) ($:external reduce . 61) ($:end reduce . 61)) 
    (($ident shift . 26) ($:. shift . 27) (ident shift . 28) ($:output shift 
    . 73) ($:input shift . 74) ($:constant shift . 75) ($:parameter shift . 76
    ) ($:discrete shift . 77) ($:stream shift . 78) ($:flow shift . 79) 
    ($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package 
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) 
    ($:model shift . 11) ($:class shift . 12) (name shift . 80) (type-prefix-3
    shift . 81) (type-prefix-2 shift . 82) (type-prefix-1 shift . 83) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (type-specifier shift 
    . 84) (type-prefix shift . 85) (class-prefixes shift . 15) ($:encapsulated
    shift . 16) ($:extends shift . 86) ($:import shift . 87) ($:replaceable 
    shift . 88) (component-clause shift . 89) (class-definition shift . 90) 
    (element-1 shift . 91) ($:outer shift . 92) ($:inner shift . 93) ($:final 
    shift . 94) ($:redeclare shift . 95) (extends-clause shift . 96) (
    import-clause shift . 97) (element shift . 98) (element-list-1 shift . 99)
    (element-list shift . 254) ($:annotation reduce . 59) ($:equation reduce 
    . 59) ($:algorithm reduce . 59) ($:initial reduce . 59) ($:protected 
    reduce . 59) ($:public reduce . 59) ($:external reduce . 59) ($:end reduce
    . 59)) (($ident shift . 26) ($:. shift . 27) (ident shift . 28) ($:output 
    shift . 73) ($:input shift . 74) ($:constant shift . 75) ($:parameter 
    shift . 76) ($:discrete shift . 77) ($:stream shift . 78) ($:flow shift . 
    79) ($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) 
    ($:package shift . 4) ($:type shift . 5) ($:connector shift . 6) (
    $:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (name 
    shift . 80) (type-prefix-3 shift . 81) (type-prefix-2 shift . 82) (
    type-prefix-1 shift . 83) (class-prefixes-1 shift . 13) ($:partial shift 
    . 14) (type-specifier shift . 84) (type-prefix shift . 85) (class-prefixes
    shift . 15) ($:encapsulated shift . 16) ($:extends shift . 86) ($:import 
    shift . 87) ($:replaceable shift . 88) (component-clause shift . 89) 
    (class-definition shift . 90) (element-1 shift . 91) ($:outer shift . 92) 
    ($:inner shift . 93) ($:final shift . 94) ($:redeclare shift . 95) 
    (extends-clause shift . 96) (import-clause shift . 97) (element shift . 98
    ) (element-list-1 shift . 99) (element-list shift . 253) ($:annotation 
    reduce . 57) ($:equation reduce . 57) ($:algorithm reduce . 57) ($:initial
    reduce . 57) ($:protected reduce . 57) ($:public reduce . 57) ($:external 
    reduce . 57) ($:end reduce . 57)) (($:annotation reduce . 55) ($:public 
    reduce . 55) ($:protected reduce . 55) ($:initial reduce . 55) (
    $:algorithm reduce . 55) ($:equation reduce . 55) ($:external reduce . 55)
    ($:end reduce . 55)) (($:annotation reduce . 54) ($:equation reduce . 54) 
    ($:algorithm reduce . 54) ($:initial reduce . 54) ($:protected reduce . 54
    ) ($:public reduce . 54) ($:external reduce . 54) ($:end reduce . 54)) 
    (($:annotation shift . 117) (annotation shift . 118) (opt-annotation shift
    . 250) ($:algorithm shift . 105) ($:equation shift . 106) ($:initial shift
    . 107) (algorithm-section shift . 108) (equation-section shift . 109) 
    ($:protected shift . 110) ($:public shift . 111) (composition-1 shift . 
    251) ($:external shift . 115) (external-part shift . 252) ($:end reduce . 
    347)) (($string shift . 50) (string shift . 241) (language-specification 
    shift . 242) ($:. shift . 243) ($ident shift . 26) (component-reference-1 
    shift . 244) (ident shift . 245) (component-reference shift . 246) 
    (external-function-call shift . 247) ($:annotation shift . 117) (
    annotation shift . 248) (#{$:;}# shift . 249)) (($:annotation shift . 117)
    (annotation shift . 118) (opt-annotation shift . 240) ($:end reduce . 347)
    ) ((#{$:\x28;}# shift . 66) (class-modification shift . 239)) ((#{$:;}# 
    shift . 238)) (($:end reduce . 53)) ((#{$:;}# shift . 237)) (($:public 
    reduce . 77) ($:protected reduce . 77) ($:initial reduce . 77) (
    $:algorithm reduce . 77) ($:equation reduce . 77) ($:external reduce . 77)
    ($:annotation reduce . 77) ($:end reduce . 77) ($:import reduce . 77) 
    ($:extends reduce . 77) ($:function reduce . 77) ($:pure reduce . 77) 
    ($:impure reduce . 77) ($:package reduce . 77) ($:type reduce . 77) 
    ($:connector reduce . 77) ($:expandable reduce . 77) ($:block reduce . 77)
    ($:record reduce . 77) ($:operator reduce . 77) ($:model reduce . 77) 
    ($:class reduce . 77) ($:partial reduce . 77) ($:encapsulated reduce . 77)
    ($:stream reduce . 77) ($:flow reduce . 77) ($:constant reduce . 77) 
    ($:parameter reduce . 77) ($:discrete reduce . 77) ($:output reduce . 77) 
    ($:input reduce . 77) ($ident reduce . 77) ($:. reduce . 77) (
    $:replaceable reduce . 77) ($:outer reduce . 77) ($:inner reduce . 77) 
    ($:final reduce . 77) ($:redeclare reduce . 77)) (($:function reduce . 87)
    ($:pure reduce . 87) ($:impure reduce . 87) ($:package reduce . 87) 
    ($:type reduce . 87) ($:connector reduce . 87) ($:expandable reduce . 87) 
    ($:block reduce . 87) ($:record reduce . 87) ($:operator reduce . 87) 
    ($:model reduce . 87) ($:class reduce . 87) ($:partial reduce . 87) 
    ($:encapsulated reduce . 87) ($:stream reduce . 87) ($:flow reduce . 87) 
    ($:constant reduce . 87) ($:parameter reduce . 87) ($:discrete reduce . 87
    ) ($:output reduce . 87) ($:input reduce . 87) ($ident reduce . 87) 
    ($:. reduce . 87) ($:replaceable reduce . 87) ($:outer reduce . 87) 
    ($:inner reduce . 87)) (($:inner shift . 235) ($P2 shift . 236) (
    $:function reduce . 88) ($:pure reduce . 88) ($:impure reduce . 88) 
    ($:package reduce . 88) ($:type reduce . 88) ($:connector reduce . 88) 
    ($:expandable reduce . 88) ($:block reduce . 88) ($:record reduce . 88) 
    ($:operator reduce . 88) ($:model reduce . 88) ($:class reduce . 88) 
    ($:partial reduce . 88) ($:encapsulated reduce . 88) ($:stream reduce . 88
    ) ($:flow reduce . 88) ($:constant reduce . 88) ($:parameter reduce . 88) 
    ($:discrete reduce . 88) ($:output reduce . 88) ($:input reduce . 88) 
    ($ident reduce . 88) ($:. reduce . 88) ($:replaceable reduce . 88) 
    ($:outer reduce . 88)) (($:function reduce . 93) ($:pure reduce . 93) 
    ($:impure reduce . 93) ($:package reduce . 93) ($:type reduce . 93) 
    ($:connector reduce . 93) ($:expandable reduce . 93) ($:block reduce . 93)
    ($:record reduce . 93) ($:operator reduce . 93) ($:model reduce . 93) 
    ($:class reduce . 93) ($:partial reduce . 93) ($:encapsulated reduce . 93)
    ($:stream reduce . 93) ($:flow reduce . 93) ($:constant reduce . 93) 
    ($:parameter reduce . 93) ($:discrete reduce . 93) ($:output reduce . 93) 
    ($:input reduce . 93) ($ident reduce . 93) ($:. reduce . 93) (
    $:replaceable reduce . 93) ($:outer reduce . 93)) (($:outer shift . 233) 
    ($P5 shift . 234) ($:function reduce . 94) ($:pure reduce . 94) ($:impure 
    reduce . 94) ($:package reduce . 94) ($:type reduce . 94) ($:connector 
    reduce . 94) ($:expandable reduce . 94) ($:block reduce . 94) ($:record 
    reduce . 94) ($:operator reduce . 94) ($:model reduce . 94) ($:class 
    reduce . 94) ($:partial reduce . 94) ($:encapsulated reduce . 94) (
    $:stream reduce . 94) ($:flow reduce . 94) ($:constant reduce . 94) 
    ($:parameter reduce . 94) ($:discrete reduce . 94) ($:output reduce . 94) 
    ($:input reduce . 94) ($ident reduce . 94) ($:. reduce . 94) (
    $:replaceable reduce . 94)) (($:function reduce . 97) ($:pure reduce . 97)
    ($:impure reduce . 97) ($:package reduce . 97) ($:type reduce . 97) 
    ($:connector reduce . 97) ($:expandable reduce . 97) ($:block reduce . 97)
    ($:record reduce . 97) ($:operator reduce . 97) ($:model reduce . 97) 
    ($:class reduce . 97) ($:partial reduce . 97) ($:encapsulated reduce . 97)
    ($:stream reduce . 97) ($:flow reduce . 97) ($:constant reduce . 97) 
    ($:parameter reduce . 97) ($:discrete reduce . 97) ($:output reduce . 97) 
    ($:input reduce . 97) ($ident reduce . 97) ($:. reduce . 97) (
    $:replaceable reduce . 97)) (($ident shift . 26) ($:. shift . 27) (ident 
    shift . 28) ($:output shift . 73) ($:input shift . 74) ($:constant shift 
    . 75) ($:parameter shift . 76) ($:discrete shift . 77) ($:stream shift . 
    78) ($:flow shift . 79) ($:function shift . 1) ($:pure shift . 2) (
    $:impure shift . 3) ($:package shift . 4) ($:type shift . 5) ($:connector 
    shift . 6) ($:expandable shift . 7) ($:block shift . 8) ($:record shift . 
    9) ($:operator shift . 10) ($:model shift . 11) ($:class shift . 12) 
    (name shift . 80) (type-prefix-3 shift . 81) (type-prefix-2 shift . 82) 
    (type-prefix-1 shift . 83) (class-prefixes-1 shift . 13) ($:partial shift 
    . 14) (type-specifier shift . 84) (type-prefix shift . 85) (class-prefixes
    shift . 15) ($:encapsulated shift . 16) ($:replaceable shift . 88) 
    (component-clause shift . 89) (class-definition shift . 90) (element-1 
    shift . 232)) ((#{$:;}# reduce . 84)) (($:constrainedby reduce . 103) 
    (#{$:;}# reduce . 103)) (($:constrainedby reduce . 102) (#{$:;}# reduce . 
    102)) (($:constrainedby shift . 230) (constraining-clause shift . 231) 
    (#{$:;}# reduce . 101)) (($:. shift . 229) ($string reduce . 107) (
    $:annotation reduce . 107) (#{$:;}# reduce . 107)) (($:= shift . 228) 
    ($:annotation reduce . 304) ($string reduce . 304) ($:. reduce . 304) 
    (#{$:;}# reduce . 304)) (($string shift . 50) (string shift . 51) (
    string-cat shift . 52) (string-comment shift . 185) (comment shift . 227) 
    (#{$:;}# reduce . 343) ($:annotation reduce . 343)) (($:. shift . 59) 
    ($:annotation shift . 117) (annotation shift . 225) (#{$:\x28;}# shift . 
    66) (class-modification shift . 226) (#{$:;}# reduce . 116)) (($ident 
    shift . 26) (ident shift . 139) (declaration shift . 140) (
    component-declaration shift . 141) (component-list shift . 223) (
    #{$:\x5b;}# shift . 137) (array-subscripts shift . 224)) (($ident shift . 
    26) ($string shift . 50) ($float shift . 197) ($fixed shift . 198) 
    ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}#
    shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 
    203) ($:true shift . 204) ($:false shift . 205) (string shift . 206) 
    (unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 219) ($:: shift . 220) 
    (subscript shift . 221) (array-subscript-list shift . 222)) (($ident shift
    . 26) (ident shift . 139) (declaration shift . 140) (component-declaration
    shift . 141) (component-list shift . 196)) ((#{$:\x5b;}# shift . 137) 
    (array-subscripts shift . 194) ($P7 shift . 195) ($string reduce . 144) 
    ($:annotation reduce . 144) ($:if reduce . 144) ($:, reduce . 144) 
    (#{$:;}# reduce . 144) ($:constrainedby reduce . 144) (#{$:\x29;}# reduce 
    . 144) (#{$:\x28;}# reduce . 144) ($::= reduce . 144) ($:= reduce . 144)) 
    (($:if shift . 191) (condition-attribute shift . 192) ($string shift . 50)
    (string shift . 51) (string-cat shift . 52) (string-comment shift . 185) 
    (comment shift . 193) ($:, reduce . 343) (#{$:;}# reduce . 343) (
    $:constrainedby reduce . 343) ($:annotation reduce . 343)) (($:, reduce . 
    138) (#{$:;}# reduce . 138) ($:constrainedby reduce . 138)) (($:, shift . 
    190) (#{$:;}# reduce . 122) ($:constrainedby reduce . 122)) (($:output 
    shift . 73) ($:input shift . 74) (type-prefix-3 shift . 189) ($ident 
    reduce . 124) ($:. reduce . 124)) (($ident reduce . 125) ($:. reduce . 125
    )) (($ident reduce . 126) ($:. reduce . 126)) (($string shift . 50) 
    (string shift . 51) (string-cat shift . 52) (string-comment shift . 185) 
    (comment shift . 186) (#{$:\x28;}# shift . 66) (class-modification shift 
    . 187) (#{$:\x5b;}# shift . 137) (array-subscripts shift . 188) (#{$:;}# 
    reduce . 343) ($:constrainedby reduce . 343) ($:stream reduce . 343) 
    ($:flow reduce . 343) ($:constant reduce . 343) ($:parameter reduce . 343)
    ($:discrete reduce . 343) ($:output reduce . 343) ($:input reduce . 343) 
    (#{$:\x29;}# reduce . 343) ($:, reduce . 343) ($:annotation reduce . 343))
    (($:: shift . 181) ($ident shift . 26) (ident shift . 182) (
    enumeration-literal shift . 183) (enum-list shift . 184)) (($ident shift 
    . 26) ($:. shift . 27) (ident shift . 28) (name shift . 80) (
    type-specifier shift . 180)) (($ident shift . 26) ($:. shift . 27) 
    (ident shift . 28) ($:output shift . 73) ($:input shift . 74) ($:constant 
    shift . 75) ($:parameter shift . 76) ($:discrete shift . 77) ($:stream 
    shift . 78) ($:flow shift . 79) ($:function shift . 1) ($:pure shift . 2) 
    ($:impure shift . 3) ($:package shift . 4) ($:type shift . 5) ($:connector
    shift . 6) ($:expandable shift . 7) ($:block shift . 8) ($:record shift . 
    9) ($:operator shift . 10) ($:model shift . 11) ($:class shift . 12) 
    (name shift . 80) (type-prefix-3 shift . 81) (type-prefix-2 shift . 82) 
    (type-prefix-1 shift . 83) (class-prefixes-1 shift . 13) ($:partial shift 
    . 14) (type-specifier shift . 84) (type-prefix shift . 85) (class-prefixes
    shift . 15) ($:encapsulated shift . 16) ($:extends shift . 86) ($:import 
    shift . 87) ($:replaceable shift . 88) (component-clause shift . 89) 
    (class-definition shift . 90) (element-1 shift . 91) ($:outer shift . 92) 
    ($:inner shift . 93) ($:final shift . 94) ($:redeclare shift . 95) 
    (extends-clause shift . 96) (import-clause shift . 97) (element shift . 98
    ) (element-list-1 shift . 99) (element-list shift . 100) (composition 
    shift . 179)) (($:function shift . 1) ($:pure shift . 2) ($:impure shift 
    . 3) ($:package shift . 4) ($:type shift . 5) ($:connector shift . 6) 
    ($:expandable shift . 7) ($:block shift . 8) ($:record shift . 9) (
    $:operator shift . 10) ($:model shift . 11) ($:class shift . 12) (
    class-prefixes-1 shift . 13) ($:partial shift . 14) (class-prefixes shift 
    . 177) (short-class-definition shift . 178)) ((#{$:\x28;}# shift . 66) 
    ($::= shift . 172) ($:= shift . 173) (class-modification shift . 174) 
    (modification shift . 175) ($P9 shift . 176) ($:. shift . 59) (#{$:\x29;}#
    reduce . 165) ($:, reduce . 165) ($string reduce . 165)) ((#{$:\x29;}# 
    reduce . 163) ($:, reduce . 163)) ((#{$:\x29;}# reduce . 162) ($:, reduce 
    . 162)) (($:each shift . 170) ($P10 shift . 171) ($:function reduce . 168)
    ($:pure reduce . 168) ($:impure reduce . 168) ($:package reduce . 168) 
    ($:type reduce . 168) ($:connector reduce . 168) ($:expandable reduce . 
    168) ($:block reduce . 168) ($:record reduce . 168) ($:operator reduce . 
    168) ($:model reduce . 168) ($:class reduce . 168) ($:partial reduce . 168
    ) ($:stream reduce . 168) ($:flow reduce . 168) ($:constant reduce . 168) 
    ($:parameter reduce . 168) ($:discrete reduce . 168) ($:output reduce . 
    168) ($:input reduce . 168) ($:replaceable reduce . 168) ($:final reduce 
    . 168)) ((#{$:\x29;}# reduce . 161) ($:, reduce . 161)) (($ident shift . 
    26) ($:. shift . 27) (ident shift . 28) ($:replaceable shift . 150) 
    (name shift . 151) (element-replaceable shift . 152) (element-modification
    shift . 153) (elt-mod-or-repl-1 shift . 169)) (($:final shift . 167) 
    ($ident shift . 26) ($:. shift . 27) (ident shift . 28) ($:replaceable 
    shift . 150) (name shift . 151) (element-replaceable shift . 152) (
    element-modification shift . 153) (elt-mod-or-repl-1 shift . 168)) 
    ((#{$:\x29;}# reduce . 157) ($:, reduce . 157)) ((#{$:\x29;}# reduce . 156
    ) ($:, reduce . 156)) ((#{$:\x29;}# reduce . 154) ($:, reduce . 154)) 
    ((#{$:\x29;}# shift . 165) ($:, shift . 166)) (($:import reduce . 153) 
    ($:extends reduce . 153) ($:function reduce . 153) ($:pure reduce . 153) 
    ($:impure reduce . 153) ($:package reduce . 153) ($:type reduce . 153) 
    ($:connector reduce . 153) ($:expandable reduce . 153) ($:block reduce . 
    153) ($:record reduce . 153) ($:operator reduce . 153) ($:model reduce . 
    153) ($:class reduce . 153) ($:partial reduce . 153) ($:encapsulated 
    reduce . 153) ($:stream reduce . 153) ($:flow reduce . 153) ($:constant 
    reduce . 153) ($:parameter reduce . 153) ($:discrete reduce . 153) 
    ($:output reduce . 153) ($:input reduce . 153) ($ident reduce . 153) 
    ($:. reduce . 153) ($:replaceable reduce . 153) ($:outer reduce . 153) 
    ($:inner reduce . 153) ($:final reduce . 153) ($:redeclare reduce . 153) 
    ($string reduce . 153) ($:annotation reduce . 153) ($:= reduce . 153) 
    (#{$:;}# reduce . 153) ($:constrainedby reduce . 153) (#{$:\x29;}# reduce 
    . 153) ($:, reduce . 153) ($:if reduce . 153)) (($:end shift . 164)) 
    (($ident shift . 26) (ident shift . 406)) (($:import reduce . 152) 
    ($:extends reduce . 152) ($:function reduce . 152) ($:pure reduce . 152) 
    ($:impure reduce . 152) ($:package reduce . 152) ($:type reduce . 152) 
    ($:connector reduce . 152) ($:expandable reduce . 152) ($:block reduce . 
    152) ($:record reduce . 152) ($:operator reduce . 152) ($:model reduce . 
    152) ($:class reduce . 152) ($:partial reduce . 152) ($:encapsulated 
    reduce . 152) ($:stream reduce . 152) ($:flow reduce . 152) ($:constant 
    reduce . 152) ($:parameter reduce . 152) ($:discrete reduce . 152) 
    ($:output reduce . 152) ($:input reduce . 152) ($ident reduce . 152) 
    ($:. reduce . 152) ($:replaceable reduce . 152) ($:outer reduce . 152) 
    ($:inner reduce . 152) ($:final reduce . 152) ($:redeclare reduce . 152) 
    ($string reduce . 152) ($:annotation reduce . 152) ($:= reduce . 152) 
    (#{$:;}# reduce . 152) ($:constrainedby reduce . 152) (#{$:\x29;}# reduce 
    . 152) ($:, reduce . 152) ($:if reduce . 152)) (($ident shift . 26) 
    ($:. shift . 27) (ident shift . 28) ($:replaceable shift . 150) (name 
    shift . 151) (element-replaceable shift . 152) (element-modification shift
    . 153) ($:redeclare shift . 154) (elt-mod-or-repl-1 shift . 155) ($:final 
    shift . 156) ($:each shift . 157) (element-redeclaration shift . 158) 
    (element-modification-or-replaceable shift . 159) (argument shift . 405)) 
    (($ident shift . 26) ($:. shift . 27) (ident shift . 28) ($:replaceable 
    shift . 150) (name shift . 151) (element-replaceable shift . 152) (
    element-modification shift . 153) (elt-mod-or-repl-1 shift . 404)) 
    ((#{$:\x29;}# reduce . 159) ($:, reduce . 159)) ((#{$:\x29;}# reduce . 160
    ) ($:, reduce . 160)) (($:function reduce . 169) ($:pure reduce . 169) 
    ($:impure reduce . 169) ($:package reduce . 169) ($:type reduce . 169) 
    ($:connector reduce . 169) ($:expandable reduce . 169) ($:block reduce . 
    169) ($:record reduce . 169) ($:operator reduce . 169) ($:model reduce . 
    169) ($:class reduce . 169) ($:partial reduce . 169) ($:stream reduce . 
    169) ($:flow reduce . 169) ($:constant reduce . 169) ($:parameter reduce 
    . 169) ($:discrete reduce . 169) ($:output reduce . 169) ($:input reduce 
    . 169) ($:replaceable reduce . 169) ($:final reduce . 169)) (($:final 
    shift . 402) ($P11 shift . 403) ($:function reduce . 170) ($:pure reduce 
    . 170) ($:impure reduce . 170) ($:package reduce . 170) ($:type reduce . 
    170) ($:connector reduce . 170) ($:expandable reduce . 170) ($:block 
    reduce . 170) ($:record reduce . 170) ($:operator reduce . 170) ($:model 
    reduce . 170) ($:class reduce . 170) ($:partial reduce . 170) ($:stream 
    reduce . 170) ($:flow reduce . 170) ($:constant reduce . 170) ($:parameter
    reduce . 170) ($:discrete reduce . 170) ($:output reduce . 170) ($:input 
    reduce . 170) ($:replaceable reduce . 170)) (($ident shift . 26) ($string 
    shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) 
    (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) 
    (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) ($:true 
    shift . 204) ($:false shift . 205) (string shift . 206) (unsigned-number 
    shift . 207) (primary shift . 208) (factor shift . 209) (term shift . 210)
    (arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 216) ($:if shift . 217) (simple-expression 
    shift . 218) (expression shift . 401)) (($ident shift . 26) ($string shift
    . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (
    #{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) ($:true 
    shift . 204) ($:false shift . 205) (string shift . 206) (unsigned-number 
    shift . 207) (primary shift . 208) (factor shift . 209) (term shift . 210)
    (arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 216) ($:if shift . 217) (simple-expression 
    shift . 218) (expression shift . 400)) (($:= shift . 399) ($string reduce 
    . 149) (#{$:\x29;}# reduce . 149) ($:, reduce . 149) ($:annotation reduce 
    . 149) ($:if reduce . 149) (#{$:;}# reduce . 149) ($:constrainedby reduce 
    . 149)) (($string reduce . 166) (#{$:\x29;}# reduce . 166) ($:, reduce . 
    166)) (($string shift . 50) (string shift . 51) (string-cat shift . 52) 
    (string-comment shift . 398) ($:, reduce . 343) (#{$:\x29;}# reduce . 343)
    ) (($ident shift . 26) (ident shift . 396) (short-class-specifier shift . 
    397)) (($:output shift . 73) ($:input shift . 74) ($:constant shift . 75) 
    ($:parameter shift . 76) ($:discrete shift . 77) ($:stream shift . 78) 
    ($:flow shift . 79) (type-prefix-3 shift . 81) (type-prefix-2 shift . 82) 
    (type-prefix-1 shift . 83) (type-prefix shift . 394) (component-clause1 
    shift . 395)) (($:end shift . 393)) (($:, shift . 392)) ((#{$:\x29;}# 
    shift . 391)) (($string shift . 50) (string shift . 51) (string-cat shift 
    . 52) (string-comment shift . 185) (comment shift . 390) (#{$:\x29;}# 
    reduce . 343) ($:, reduce . 343) ($:annotation reduce . 343)) ((
    #{$:\x29;}# reduce . 47) ($:, reduce . 47)) ((#{$:\x29;}# shift . 388) 
    ($:, shift . 389)) (($:annotation shift . 117) (annotation shift . 387) 
    (#{$:;}# reduce . 342) ($:, reduce . 342) ($:constrainedby reduce . 342) 
    (#{$:\x29;}# reduce . 342) ($:stream reduce . 342) ($:flow reduce . 342) 
    ($:constant reduce . 342) ($:parameter reduce . 342) ($:discrete reduce . 
    342) ($:output reduce . 342) ($:input reduce . 342)) ((#{$:;}# reduce . 39
    ) ($:constrainedby reduce . 39) ($:stream reduce . 39) ($:flow reduce . 39
    ) ($:constant reduce . 39) ($:parameter reduce . 39) ($:discrete reduce . 
    39) ($:output reduce . 39) ($:input reduce . 39) (#{$:\x29;}# reduce . 39)
    ($:, reduce . 39)) (($string shift . 50) (string shift . 51) (string-cat 
    shift . 52) (string-comment shift . 185) (comment shift . 386) (#{$:;}# 
    reduce . 343) ($:constrainedby reduce . 343) ($:stream reduce . 343) 
    ($:flow reduce . 343) ($:constant reduce . 343) ($:parameter reduce . 343)
    ($:discrete reduce . 343) ($:output reduce . 343) ($:input reduce . 343) 
    (#{$:\x29;}# reduce . 343) ($:, reduce . 343) ($:annotation reduce . 343))
    ((#{$:\x28;}# shift . 66) (class-modification shift . 384) ($string shift 
    . 50) (string shift . 51) (string-cat shift . 52) (string-comment shift . 
    185) (comment shift . 385) (#{$:;}# reduce . 343) ($:constrainedby reduce 
    . 343) ($:stream reduce . 343) ($:flow reduce . 343) ($:constant reduce . 
    343) ($:parameter reduce . 343) ($:discrete reduce . 343) ($:output reduce
    . 343) ($:input reduce . 343) (#{$:\x29;}# reduce . 343) ($:, reduce . 343
    ) ($:annotation reduce . 343)) (($ident reduce . 123) ($:. reduce . 123)) 
    (($ident shift . 26) (ident shift . 139) (declaration shift . 140) 
    (component-declaration shift . 383)) (($ident shift . 26) ($string shift 
    . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (
    #{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) ($:true 
    shift . 204) ($:false shift . 205) (string shift . 206) (unsigned-number 
    shift . 207) (primary shift . 208) (factor shift . 209) (term shift . 210)
    (arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 216) ($:if shift . 217) (simple-expression 
    shift . 218) (expression shift . 382)) (($string shift . 50) (string shift
    . 51) (string-cat shift . 52) (string-comment shift . 185) (comment shift 
    . 381) ($:, reduce . 343) (#{$:;}# reduce . 343) ($:constrainedby reduce 
    . 343) ($:annotation reduce . 343)) (($:, reduce . 141) (#{$:;}# reduce . 
    141) ($:constrainedby reduce . 141)) ((#{$:\x28;}# reduce . 145) ($::= 
    reduce . 145) ($:= reduce . 145) ($string reduce . 145) ($:annotation 
    reduce . 145) ($:if reduce . 145) ($:, reduce . 145) (#{$:;}# reduce . 145
    ) ($:constrainedby reduce . 145) (#{$:\x29;}# reduce . 145)) ((#{$:\x28;}#
    shift . 66) ($::= shift . 172) ($:= shift . 173) (class-modification shift
    . 174) (modification shift . 379) ($P8 shift . 380) (#{$:\x29;}# reduce . 
    146) ($:constrainedby reduce . 146) (#{$:;}# reduce . 146) ($:, reduce . 
    146) ($:if reduce . 146) ($:annotation reduce . 146) ($string reduce . 146
    )) (($:, shift . 190) (#{$:;}# reduce . 121) ($:constrainedby reduce . 121
    )) (($:: reduce . 351) ($:= reduce . 351) ($:or reduce . 351) ($:and 
    reduce . 351) ($:< reduce . 351) ($:<= reduce . 351) ($:> reduce . 351) 
    ($:>= reduce . 351) ($:== reduce . 351) ($:<> reduce . 351) ($:.- reduce 
    . 351) ($:.+ reduce . 351) ($:- reduce . 351) ($:+ reduce . 351) ($:* 
    reduce . 351) ($:/ reduce . 351) ($:.* reduce . 351) ($:./ reduce . 351) 
    ($:.^ reduce . 351) ($:^ reduce . 351) (#{$:\x5d;}# reduce . 351) ($:, 
    reduce . 351) (#{$:\x7d;}# reduce . 351) (#{$:;}# reduce . 351) (
    #{$:\x29;}# reduce . 351) ($:then reduce . 351) ($:loop reduce . 351) 
    ($:else reduce . 351) ($:elseif reduce . 351) ($string reduce . 351) 
    ($:annotation reduce . 351) ($:constrainedby reduce . 351) ($:end reduce 
    . 351) ($:elsewhen reduce . 351) ($:if reduce . 351)) (($:: reduce . 350) 
    ($:= reduce . 350) ($:or reduce . 350) ($:and reduce . 350) ($:< reduce . 
    350) ($:<= reduce . 350) ($:> reduce . 350) ($:>= reduce . 350) ($:== 
    reduce . 350) ($:<> reduce . 350) ($:.- reduce . 350) ($:.+ reduce . 350) 
    ($:- reduce . 350) ($:+ reduce . 350) ($:* reduce . 350) ($:/ reduce . 350
    ) ($:.* reduce . 350) ($:./ reduce . 350) ($:.^ reduce . 350) ($:^ reduce 
    . 350) (#{$:\x5d;}# reduce . 350) ($:, reduce . 350) (#{$:\x7d;}# reduce 
    . 350) (#{$:;}# reduce . 350) (#{$:\x29;}# reduce . 350) ($:then reduce . 
    350) ($:loop reduce . 350) ($:else reduce . 350) ($:elseif reduce . 350) 
    ($string reduce . 350) ($:annotation reduce . 350) ($:constrainedby reduce
    . 350) ($:end reduce . 350) ($:elsewhen reduce . 350) ($:if reduce . 350))
    (($string shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift
    . 27) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# 
    shift . 201) ($:der shift . 202) (name shift . 203) ($:true shift . 204) 
    ($:false shift . 205) (string shift . 206) (unsigned-number shift . 207) 
    (primary shift . 208) (factor shift . 209) (term shift . 210) (
    arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 216) ($ident shift . 26) ($:if shift . 217) 
    (simple-expression shift . 218) (expression shift . 371) ($:function shift
    . 372) (ident shift . 373) (named-argument shift . 374) (function-argument
    shift . 375) (named-only-arguments-1 shift . 376) (function-arguments-1 
    shift . 377) (function-arguments shift . 378)) (($ident shift . 26) 
    ($string shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 
    200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) 
    ($:true shift . 204) ($:false shift . 205) (string shift . 206) (
    unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 368) (expression-list 
    shift . 369) (expression-list-list shift . 370)) (($ident shift . 26) 
    ($string shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 
    200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) 
    ($:true shift . 204) ($:false shift . 205) (string shift . 206) (
    unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 294) ($:, shift . 295) 
    (output-expression-list shift . 367)) ((#{$:\x28;}# shift . 292) (
    function-call-args shift . 366)) ((#{$:\x28;}# shift . 292) (
    function-call-args shift . 365) (#{$:\x5b;}# shift . 137) (
    array-subscripts shift . 310) ($:. shift . 59) (#{$:\x5d;}# reduce . 297) 
    ($:: reduce . 297) ($:^ reduce . 297) ($:.^ reduce . 297) ($:./ reduce . 
    297) ($:.* reduce . 297) ($:/ reduce . 297) ($:* reduce . 297) ($:+ reduce
    . 297) ($:- reduce . 297) ($:.+ reduce . 297) ($:.- reduce . 297) ($:<> 
    reduce . 297) ($:== reduce . 297) ($:>= reduce . 297) ($:> reduce . 297) 
    ($:<= reduce . 297) ($:< reduce . 297) ($:and reduce . 297) ($:or reduce 
    . 297) ($:, reduce . 297) (#{$:\x7d;}# reduce . 297) (#{$:;}# reduce . 297
    ) (#{$:\x29;}# reduce . 297) ($:then reduce . 297) ($:loop reduce . 297) 
    ($:else reduce . 297) ($:elseif reduce . 297) ($string reduce . 297) 
    ($:annotation reduce . 297) ($:constrainedby reduce . 297) ($:= reduce . 
    297) ($:end reduce . 297) ($:elsewhen reduce . 297) ($:if reduce . 297)) 
    (($:= reduce . 294) ($:: reduce . 294) ($:^ reduce . 294) ($:.^ reduce . 
    294) ($:./ reduce . 294) ($:.* reduce . 294) ($:/ reduce . 294) ($:* 
    reduce . 294) ($:+ reduce . 294) ($:- reduce . 294) ($:.+ reduce . 294) 
    ($:.- reduce . 294) ($:<> reduce . 294) ($:== reduce . 294) ($:>= reduce 
    . 294) ($:> reduce . 294) ($:<= reduce . 294) ($:< reduce . 294) ($:and 
    reduce . 294) ($:or reduce . 294) (#{$:\x5d;}# reduce . 294) ($:, reduce 
    . 294) (#{$:\x7d;}# reduce . 294) (#{$:;}# reduce . 294) (#{$:\x29;}# 
    reduce . 294) ($:then reduce . 294) ($:loop reduce . 294) ($:else reduce 
    . 294) ($:elseif reduce . 294) ($string reduce . 294) ($:annotation reduce
    . 294) ($:constrainedby reduce . 294) ($:end reduce . 294) ($:elsewhen 
    reduce . 294) ($:if reduce . 294)) (($:= reduce . 293) ($:: reduce . 293) 
    ($:^ reduce . 293) ($:.^ reduce . 293) ($:./ reduce . 293) ($:.* reduce . 
    293) ($:/ reduce . 293) ($:* reduce . 293) ($:+ reduce . 293) ($:- reduce 
    . 293) ($:.+ reduce . 293) ($:.- reduce . 293) ($:<> reduce . 293) 
    ($:== reduce . 293) ($:>= reduce . 293) ($:> reduce . 293) ($:<= reduce . 
    293) ($:< reduce . 293) ($:and reduce . 293) ($:or reduce . 293) (
    #{$:\x5d;}# reduce . 293) ($:, reduce . 293) (#{$:\x7d;}# reduce . 293) 
    (#{$:;}# reduce . 293) (#{$:\x29;}# reduce . 293) ($:then reduce . 293) 
    ($:loop reduce . 293) ($:else reduce . 293) ($:elseif reduce . 293) 
    ($string reduce . 293) ($:annotation reduce . 293) ($:constrainedby reduce
    . 293) ($:end reduce . 293) ($:elsewhen reduce . 293) ($:if reduce . 293))
    (($:= reduce . 292) ($:: reduce . 292) ($:^ reduce . 292) ($:.^ reduce . 
    292) ($:./ reduce . 292) ($:.* reduce . 292) ($:/ reduce . 292) ($:* 
    reduce . 292) ($:+ reduce . 292) ($:- reduce . 292) ($:.+ reduce . 292) 
    ($:.- reduce . 292) ($:<> reduce . 292) ($:== reduce . 292) ($:>= reduce 
    . 292) ($:> reduce . 292) ($:<= reduce . 292) ($:< reduce . 292) ($:and 
    reduce . 292) ($:or reduce . 292) (#{$:\x5d;}# reduce . 292) ($:, reduce 
    . 292) (#{$:\x7d;}# reduce . 292) (#{$:;}# reduce . 292) (#{$:\x29;}# 
    reduce . 292) ($:then reduce . 292) ($:loop reduce . 292) ($:else reduce 
    . 292) ($:elseif reduce . 292) ($string reduce . 292) ($:annotation reduce
    . 292) ($:constrainedby reduce . 292) ($:end reduce . 292) ($:elsewhen 
    reduce . 292) ($:if reduce . 292)) (($:= reduce . 291) ($:: reduce . 291) 
    ($:^ reduce . 291) ($:.^ reduce . 291) ($:./ reduce . 291) ($:.* reduce . 
    291) ($:/ reduce . 291) ($:* reduce . 291) ($:+ reduce . 291) ($:- reduce 
    . 291) ($:.+ reduce . 291) ($:.- reduce . 291) ($:<> reduce . 291) 
    ($:== reduce . 291) ($:>= reduce . 291) ($:> reduce . 291) ($:<= reduce . 
    291) ($:< reduce . 291) ($:and reduce . 291) ($:or reduce . 291) (
    #{$:\x5d;}# reduce . 291) ($:, reduce . 291) (#{$:\x7d;}# reduce . 291) 
    (#{$:;}# reduce . 291) (#{$:\x29;}# reduce . 291) ($:then reduce . 291) 
    ($:loop reduce . 291) ($:else reduce . 291) ($:elseif reduce . 291) 
    ($string reduce . 291) ($:annotation reduce . 291) ($:constrainedby reduce
    . 291) ($:end reduce . 291) ($:elsewhen reduce . 291) ($:if reduce . 291))
    (($:: reduce . 288) ($:= reduce . 288) ($:or reduce . 288) ($:and reduce 
    . 288) ($:< reduce . 288) ($:<= reduce . 288) ($:> reduce . 288) ($:>= 
    reduce . 288) ($:== reduce . 288) ($:<> reduce . 288) ($:.- reduce . 288) 
    ($:.+ reduce . 288) ($:- reduce . 288) ($:+ reduce . 288) ($:* reduce . 
    288) ($:/ reduce . 288) ($:.* reduce . 288) ($:./ reduce . 288) ($:.^ 
    reduce . 288) ($:^ reduce . 288) (#{$:\x5d;}# reduce . 288) ($:, reduce . 
    288) (#{$:\x7d;}# reduce . 288) (#{$:;}# reduce . 288) (#{$:\x29;}# reduce
    . 288) ($:then reduce . 288) ($:loop reduce . 288) ($:else reduce . 288) 
    ($:elseif reduce . 288) ($string reduce . 288) ($:annotation reduce . 288)
    ($:constrainedby reduce . 288) ($:end reduce . 288) ($:elsewhen reduce . 
    288) ($:if reduce . 288)) (($:^ shift . 363) ($:.^ shift . 364) ($:= 
    reduce . 282) ($:: reduce . 282) ($:./ reduce . 282) ($:.* reduce . 282) 
    ($:/ reduce . 282) ($:* reduce . 282) ($:+ reduce . 282) ($:- reduce . 282
    ) ($:.+ reduce . 282) ($:.- reduce . 282) ($:<> reduce . 282) ($:== reduce
    . 282) ($:>= reduce . 282) ($:> reduce . 282) ($:<= reduce . 282) ($:< 
    reduce . 282) ($:and reduce . 282) ($:or reduce . 282) (#{$:\x5d;}# reduce
    . 282) ($:, reduce . 282) (#{$:\x7d;}# reduce . 282) (#{$:;}# reduce . 282
    ) (#{$:\x29;}# reduce . 282) ($:then reduce . 282) ($:loop reduce . 282) 
    ($:else reduce . 282) ($:elseif reduce . 282) ($string reduce . 282) 
    ($:annotation reduce . 282) ($:constrainedby reduce . 282) ($:end reduce 
    . 282) ($:elsewhen reduce . 282) ($:if reduce . 282)) (($:./ shift . 358) 
    ($:.* shift . 359) ($:/ shift . 360) ($:* shift . 361) (mul-op shift . 362
    ) ($:: reduce . 276) ($:= reduce . 276) ($:or reduce . 276) ($:and reduce 
    . 276) ($:< reduce . 276) ($:<= reduce . 276) ($:> reduce . 276) ($:>= 
    reduce . 276) ($:== reduce . 276) ($:<> reduce . 276) ($:.- reduce . 276) 
    ($:.+ reduce . 276) ($:- reduce . 276) ($:+ reduce . 276) (#{$:\x5d;}# 
    reduce . 276) ($:, reduce . 276) (#{$:\x7d;}# reduce . 276) (#{$:;}# 
    reduce . 276) (#{$:\x29;}# reduce . 276) ($:then reduce . 276) ($:loop 
    reduce . 276) ($:else reduce . 276) ($:elseif reduce . 276) ($string 
    reduce . 276) ($:annotation reduce . 276) ($:constrainedby reduce . 276) 
    ($:end reduce . 276) ($:elsewhen reduce . 276) ($:if reduce . 276)) 
    (($:.- shift . 353) ($:.+ shift . 354) ($:- shift . 355) ($:+ shift . 356)
    (add-op shift . 357) ($:= reduce . 268) ($:: reduce . 268) ($:<> reduce . 
    268) ($:== reduce . 268) ($:>= reduce . 268) ($:> reduce . 268) ($:<= 
    reduce . 268) ($:< reduce . 268) ($:and reduce . 268) ($:or reduce . 268) 
    (#{$:\x5d;}# reduce . 268) ($:, reduce . 268) (#{$:\x7d;}# reduce . 268) 
    (#{$:;}# reduce . 268) (#{$:\x29;}# reduce . 268) ($:then reduce . 268) 
    ($:loop reduce . 268) ($:else reduce . 268) ($:elseif reduce . 268) 
    ($string reduce . 268) ($:annotation reduce . 268) ($:constrainedby reduce
    . 268) ($:end reduce . 268) ($:elsewhen reduce . 268) ($:if reduce . 268))
    (($ident shift . 26) ($string shift . 50) ($float shift . 197) ($fixed 
    shift . 198) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199)
    (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) 
    (name shift . 203) ($:true shift . 204) ($:false shift . 205) (string 
    shift . 206) (unsigned-number shift . 207) (primary shift . 208) (factor 
    shift . 209) (term shift . 210) (arithmetic-expression shift . 211) 
    (relation shift . 352)) (($:<> shift . 345) ($:== shift . 346) ($:>= shift
    . 347) ($:> shift . 348) ($:<= shift . 349) ($:< shift . 350) (rel-op 
    shift . 351) ($:: reduce . 266) ($:= reduce . 266) ($:or reduce . 266) 
    ($:and reduce . 266) (#{$:\x5d;}# reduce . 266) ($:, reduce . 266) 
    (#{$:\x7d;}# reduce . 266) (#{$:;}# reduce . 266) (#{$:\x29;}# reduce . 
    266) ($:then reduce . 266) ($:loop reduce . 266) ($:else reduce . 266) 
    ($:elseif reduce . 266) ($string reduce . 266) ($:annotation reduce . 266)
    ($:constrainedby reduce . 266) ($:end reduce . 266) ($:elsewhen reduce . 
    266) ($:if reduce . 266)) (($:= reduce . 264) ($:: reduce . 264) ($:and 
    reduce . 264) ($:or reduce . 264) (#{$:\x5d;}# reduce . 264) ($:, reduce 
    . 264) (#{$:\x7d;}# reduce . 264) (#{$:;}# reduce . 264) (#{$:\x29;}# 
    reduce . 264) ($:then reduce . 264) ($:loop reduce . 264) ($:else reduce 
    . 264) ($:elseif reduce . 264) ($string reduce . 264) ($:annotation reduce
    . 264) ($:constrainedby reduce . 264) ($:end reduce . 264) ($:elsewhen 
    reduce . 264) ($:if reduce . 264)) (($:and shift . 344) ($:: reduce . 262)
    ($:= reduce . 262) ($:or reduce . 262) (#{$:\x5d;}# reduce . 262) ($:, 
    reduce . 262) (#{$:\x7d;}# reduce . 262) (#{$:;}# reduce . 262) (
    #{$:\x29;}# reduce . 262) ($:then reduce . 262) ($:loop reduce . 262) 
    ($:else reduce . 262) ($:elseif reduce . 262) ($string reduce . 262) 
    ($:annotation reduce . 262) ($:constrainedby reduce . 262) ($:end reduce 
    . 262) ($:elsewhen reduce . 262) ($:if reduce . 262)) (($:: shift . 342) 
    ($:or shift . 343) ($:= reduce . 259) (#{$:\x5d;}# reduce . 259) ($:, 
    reduce . 259) (#{$:\x7d;}# reduce . 259) (#{$:;}# reduce . 259) (
    #{$:\x29;}# reduce . 259) ($:then reduce . 259) ($:loop reduce . 259) 
    ($:else reduce . 259) ($:elseif reduce . 259) ($string reduce . 259) 
    ($:annotation reduce . 259) ($:constrainedby reduce . 259) ($:end reduce 
    . 259) ($:elsewhen reduce . 259) ($:if reduce . 259)) (($ident shift . 26)
    ($string shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 
    200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) 
    ($:true shift . 204) ($:false shift . 205) (string shift . 206) (
    unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 341)) ((#{$:\x5d;}# 
    reduce . 254) ($:, reduce . 254) (#{$:\x7d;}# reduce . 254) (#{$:;}# 
    reduce . 254) (#{$:\x29;}# reduce . 254) ($:then reduce . 254) ($:loop 
    reduce . 254) ($:else reduce . 254) ($:elseif reduce . 254) ($string 
    reduce . 254) ($:annotation reduce . 254) ($:constrainedby reduce . 254) 
    ($:end reduce . 254) ($:elsewhen reduce . 254) ($:if reduce . 254)) 
    ((#{$:\x5d;}# reduce . 340) ($:, reduce . 340)) ((#{$:\x5d;}# reduce . 339
    ) ($:, reduce . 339)) ((#{$:\x5d;}# reduce . 337) ($:, reduce . 337)) 
    ((#{$:\x5d;}# shift . 339) ($:, shift . 340)) (($:, shift . 190) (#{$:;}# 
    reduce . 120) ($:constrainedby reduce . 120)) (($ident shift . 26) 
    (ident shift . 139) (declaration shift . 140) (component-declaration shift
    . 141) (component-list shift . 338)) ((#{$:;}# reduce . 115)) ((
    $:annotation shift . 117) (annotation shift . 337) (#{$:;}# reduce . 114))
    ((#{$:;}# reduce . 104)) (($ident shift . 26) ($:. shift . 27) (ident 
    shift . 28) (name shift . 336)) (($ident shift . 26) (ident shift . 64) 
    (#{$:\x7b;}# shift . 333) ($:* shift . 334) (import-clause-2 shift . 335))
    (($ident shift . 26) ($:. shift . 27) (ident shift . 28) (name shift . 332
    )) (($string shift . 50) (string shift . 51) (string-cat shift . 52) 
    (string-comment shift . 185) (comment shift . 331) (#{$:;}# reduce . 343) 
    ($:annotation reduce . 343)) ((#{$:;}# reduce . 83)) (($:function reduce 
    . 95) ($:pure reduce . 95) ($:impure reduce . 95) ($:package reduce . 95) 
    ($:type reduce . 95) ($:connector reduce . 95) ($:expandable reduce . 95) 
    ($:block reduce . 95) ($:record reduce . 95) ($:operator reduce . 95) 
    ($:model reduce . 95) ($:class reduce . 95) ($:partial reduce . 95) 
    ($:encapsulated reduce . 95) ($:stream reduce . 95) ($:flow reduce . 95) 
    ($:constant reduce . 95) ($:parameter reduce . 95) ($:discrete reduce . 95
    ) ($:output reduce . 95) ($:input reduce . 95) ($ident reduce . 95) 
    ($:. reduce . 95) ($:replaceable reduce . 95)) (($ident shift . 26) 
    ($:. shift . 27) (ident shift . 28) ($:output shift . 73) ($:input shift 
    . 74) ($:constant shift . 75) ($:parameter shift . 76) ($:discrete shift 
    . 77) ($:stream shift . 78) ($:flow shift . 79) ($:function shift . 1) 
    ($:pure shift . 2) ($:impure shift . 3) ($:package shift . 4) ($:type 
    shift . 5) ($:connector shift . 6) ($:expandable shift . 7) ($:block shift
    . 8) ($:record shift . 9) ($:operator shift . 10) ($:model shift . 11) 
    ($:class shift . 12) (name shift . 80) (type-prefix-3 shift . 81) (
    type-prefix-2 shift . 82) (type-prefix-1 shift . 83) (class-prefixes-1 
    shift . 13) ($:partial shift . 14) (type-specifier shift . 84) (
    type-prefix shift . 85) (class-prefixes shift . 15) ($:encapsulated shift 
    . 16) ($:replaceable shift . 88) (component-clause shift . 89) (
    class-definition shift . 90) (element-1 shift . 330)) (($:function reduce 
    . 89) ($:pure reduce . 89) ($:impure reduce . 89) ($:package reduce . 89) 
    ($:type reduce . 89) ($:connector reduce . 89) ($:expandable reduce . 89) 
    ($:block reduce . 89) ($:record reduce . 89) ($:operator reduce . 89) 
    ($:model reduce . 89) ($:class reduce . 89) ($:partial reduce . 89) 
    ($:encapsulated reduce . 89) ($:stream reduce . 89) ($:flow reduce . 89) 
    ($:constant reduce . 89) ($:parameter reduce . 89) ($:discrete reduce . 89
    ) ($:output reduce . 89) ($:input reduce . 89) ($ident reduce . 89) 
    ($:. reduce . 89) ($:replaceable reduce . 89) ($:outer reduce . 89)) 
    (($:outer shift . 328) ($P3 shift . 329) ($:function reduce . 90) ($:pure 
    reduce . 90) ($:impure reduce . 90) ($:package reduce . 90) ($:type reduce
    . 90) ($:connector reduce . 90) ($:expandable reduce . 90) ($:block reduce
    . 90) ($:record reduce . 90) ($:operator reduce . 90) ($:model reduce . 90
    ) ($:class reduce . 90) ($:partial reduce . 90) ($:encapsulated reduce . 
    90) ($:stream reduce . 90) ($:flow reduce . 90) ($:constant reduce . 90) 
    ($:parameter reduce . 90) ($:discrete reduce . 90) ($:output reduce . 90) 
    ($:input reduce . 90) ($ident reduce . 90) ($:. reduce . 90) (
    $:replaceable reduce . 90)) (($:public reduce . 78) ($:protected reduce . 
    78) ($:initial reduce . 78) ($:algorithm reduce . 78) ($:equation reduce 
    . 78) ($:external reduce . 78) ($:annotation reduce . 78) ($:end reduce . 
    78) ($:import reduce . 78) ($:extends reduce . 78) ($:function reduce . 78
    ) ($:pure reduce . 78) ($:impure reduce . 78) ($:package reduce . 78) 
    ($:type reduce . 78) ($:connector reduce . 78) ($:expandable reduce . 78) 
    ($:block reduce . 78) ($:record reduce . 78) ($:operator reduce . 78) 
    ($:model reduce . 78) ($:class reduce . 78) ($:partial reduce . 78) 
    ($:encapsulated reduce . 78) ($:stream reduce . 78) ($:flow reduce . 78) 
    ($:constant reduce . 78) ($:parameter reduce . 78) ($:discrete reduce . 78
    ) ($:output reduce . 78) ($:input reduce . 78) ($ident reduce . 78) 
    ($:. reduce . 78) ($:replaceable reduce . 78) ($:outer reduce . 78) 
    ($:inner reduce . 78) ($:final reduce . 78) ($:redeclare reduce . 78)) 
    (($:end reduce . 348)) ((#{$:;}# reduce . 349) ($:, reduce . 349) (
    $:constrainedby reduce . 349) (#{$:\x29;}# reduce . 349) ($:stream reduce 
    . 349) ($:flow reduce . 349) ($:constant reduce . 349) ($:parameter reduce
    . 349) ($:discrete reduce . 349) ($:output reduce . 349) ($:input reduce 
    . 349)) (($:end reduce . 52)) ((#{$:;}# reduce . 71) ($:annotation reduce 
    . 71) ($:. reduce . 71) ($ident reduce . 71)) ((#{$:;}# shift . 325) 
    ($:annotation shift . 117) (annotation shift . 326) ($:. shift . 243) 
    ($ident shift . 26) (component-reference-1 shift . 244) (ident shift . 245
    ) (component-reference shift . 246) (external-function-call shift . 327)) 
    (($ident shift . 26) (ident shift . 324)) (($:. shift . 323) (#{$:\x28;}# 
    reduce . 307) ($::= reduce . 307) ($:= reduce . 307) ($:, reduce . 307) 
    (#{$:\x29;}# reduce . 307)) ((#{$:\x28;}# shift . 322) (#{$:\x5b;}# shift 
    . 137) (array-subscripts shift . 303) ($P13 shift . 304) ($:= reduce . 313
    ) ($:. reduce . 313)) (($:= shift . 321)) ((#{$:;}# shift . 319) (
    $:annotation shift . 117) (annotation shift . 320)) ((#{$:;}# shift . 318)
    ) (($:annotation reduce . 70) ($:end reduce . 70)) (($:end reduce . 51)) 
    (($:annotation reduce . 56) ($:public reduce . 56) ($:protected reduce . 
    56) ($:initial reduce . 56) ($:algorithm reduce . 56) ($:equation reduce 
    . 56) ($:external reduce . 56) ($:end reduce . 56)) (($:annotation shift 
    . 117) (annotation shift . 118) (opt-annotation shift . 317) ($:end reduce
    . 347)) (($:annotation reduce . 58) ($:equation reduce . 58) ($:algorithm 
    reduce . 58) ($:initial reduce . 58) ($:protected reduce . 58) ($:public 
    reduce . 58) ($:external reduce . 58) ($:end reduce . 58)) (($:annotation 
    reduce . 60) ($:equation reduce . 60) ($:algorithm reduce . 60) ($:initial
    reduce . 60) ($:protected reduce . 60) ($:public reduce . 60) ($:external 
    reduce . 60) ($:end reduce . 60)) (($string shift . 50) ($float shift . 
    197) ($fixed shift . 198) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 
    200) (#{$:\x28;}# shift . 201) ($:der shift . 202) ($:true shift . 204) 
    ($:false shift . 205) (string shift . 206) (unsigned-number shift . 207) 
    (primary shift . 208) (factor shift . 209) (term shift . 210) (
    arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) ($ident shift . 26) (logical-term shift 
    . 215) ($:. shift . 27) (ident shift . 28) (logical-expression shift . 216
    ) ($:connect shift . 257) ($:when shift . 258) ($:for shift . 259) 
    ($:if shift . 260) (name shift . 261) (when-equation shift . 262) (
    connect-clause shift . 263) (for-equation shift . 264) (if-equation shift 
    . 265) (simple-expression shift . 266) (equation-1 shift . 267) (equation 
    shift . 268) (equation-list-1 shift . 269) (equation-list shift . 316) 
    ($:annotation reduce . 181) ($:public reduce . 181) ($:protected reduce . 
    181) ($:initial reduce . 181) ($:algorithm reduce . 181) ($:equation 
    reduce . 181) ($:external reduce . 181) ($:end reduce . 181)) (($ident 
    shift . 26) ($:. shift . 243) (ident shift . 271) (component-reference-1 
    shift . 244) ($:when shift . 272) ($:while shift . 273) ($:for shift . 274
    ) ($:if shift . 275) (when-statement shift . 276) (while-statement shift 
    . 277) (for-statement shift . 278) (if-statement shift . 279) ($:return 
    shift . 280) ($:break shift . 281) (#{$:\x28;}# shift . 282) (
    component-reference shift . 283) (statement-1 shift . 284) (statement 
    shift . 285) (statement-list shift . 315) ($:annotation reduce . 185) 
    ($:public reduce . 185) ($:protected reduce . 185) ($:initial reduce . 185
    ) ($:algorithm reduce . 185) ($:equation reduce . 185) ($:external reduce 
    . 185) ($:end reduce . 185)) ((#{$:\x28;}# shift . 314)) (($ident shift . 
    26) ($string shift . 50) ($float shift . 197) ($fixed shift . 198) 
    ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}#
    shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 
    203) ($:true shift . 204) ($:false shift . 205) (string shift . 206) 
    (unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 313)) (($ident shift . 
    26) (ident shift . 298) (for-index shift . 299) (for-indices shift . 312))
    (($ident shift . 26) ($string shift . 50) ($float shift . 197) ($fixed 
    shift . 198) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199)
    (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) 
    (name shift . 203) ($:true shift . 204) ($:false shift . 205) (string 
    shift . 206) (unsigned-number shift . 207) (primary shift . 208) (factor 
    shift . 209) (term shift . 210) (arithmetic-expression shift . 211) 
    ($:not shift . 212) (relation shift . 213) (logical-factor shift . 214) 
    (logical-term shift . 215) (logical-expression shift . 216) ($:if shift . 
    217) (simple-expression shift . 218) (expression shift . 311)) (($:. shift
    . 59) (#{$:\x28;}# shift . 292) (function-call-args shift . 309) (
    #{$:\x5b;}# shift . 137) (array-subscripts shift . 310) ($:= reduce . 297)
    ($:: reduce . 297) ($:^ reduce . 297) ($:.^ reduce . 297) ($:./ reduce . 
    297) ($:.* reduce . 297) ($:/ reduce . 297) ($:* reduce . 297) ($:+ reduce
    . 297) ($:- reduce . 297) ($:.+ reduce . 297) ($:.- reduce . 297) ($:<> 
    reduce . 297) ($:== reduce . 297) ($:>= reduce . 297) ($:> reduce . 297) 
    ($:<= reduce . 297) ($:< reduce . 297) ($:and reduce . 297) ($:or reduce 
    . 297)) ((#{$:;}# reduce . 195) ($string reduce . 195) ($:annotation 
    reduce . 195)) ((#{$:;}# reduce . 194) ($string reduce . 194) (
    $:annotation reduce . 194)) ((#{$:;}# reduce . 193) ($string reduce . 193)
    ($:annotation reduce . 193)) ((#{$:;}# reduce . 192) ($string reduce . 192
    ) ($:annotation reduce . 192)) (($:= shift . 308)) (($string shift . 50) 
    (string shift . 51) (string-cat shift . 52) (string-comment shift . 185) 
    (comment shift . 307) (#{$:;}# reduce . 343) ($:annotation reduce . 343)) 
    ((#{$:;}# shift . 306)) (($string shift . 50) ($float shift . 197) 
    ($fixed shift . 198) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) 
    (#{$:\x28;}# shift . 201) ($:der shift . 202) ($:true shift . 204) 
    ($:false shift . 205) (string shift . 206) (unsigned-number shift . 207) 
    (primary shift . 208) (factor shift . 209) (term shift . 210) (
    arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) ($ident shift . 26) (logical-term shift 
    . 215) ($:. shift . 27) (ident shift . 28) (logical-expression shift . 216
    ) ($:connect shift . 257) ($:when shift . 258) ($:for shift . 259) 
    ($:if shift . 260) (name shift . 261) (when-equation shift . 262) (
    connect-clause shift . 263) (for-equation shift . 264) (if-equation shift 
    . 265) (simple-expression shift . 266) (equation-1 shift . 267) (equation 
    shift . 305) ($:end reduce . 187) ($:annotation reduce . 187) ($:public 
    reduce . 187) ($:protected reduce . 187) ($:initial reduce . 187) (
    $:algorithm reduce . 187) ($:equation reduce . 187) ($:external reduce . 
    187) ($:elseif reduce . 187) ($:else reduce . 187) ($:elsewhen reduce . 
    187)) (($:annotation reduce . 180) ($:public reduce . 180) ($:protected 
    reduce . 180) ($:initial reduce . 180) ($:algorithm reduce . 180) (
    $:equation reduce . 180) ($:external reduce . 180) ($:end reduce . 180)) 
    ((#{$:\x5b;}# shift . 137) (array-subscripts shift . 303) ($P13 shift . 
    304) (#{$:\x29;}# reduce . 313) ($:, reduce . 313) (#{$:\x28;}# reduce . 
    313) ($::= reduce . 313) ($:. reduce . 313)) (($ident shift . 26) ($string
    shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) 
    (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) 
    (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) ($:true 
    shift . 204) ($:false shift . 205) (string shift . 206) (unsigned-number 
    shift . 207) (primary shift . 208) (factor shift . 209) (term shift . 210)
    (arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 216) ($:if shift . 217) (simple-expression 
    shift . 218) (expression shift . 302)) (($ident shift . 26) ($string shift
    . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (
    #{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) ($:true 
    shift . 204) ($:false shift . 205) (string shift . 206) (unsigned-number 
    shift . 207) (primary shift . 208) (factor shift . 209) (term shift . 210)
    (arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 216) ($:if shift . 217) (simple-expression 
    shift . 218) (expression shift . 301)) (($ident shift . 26) (ident shift 
    . 298) (for-index shift . 299) (for-indices shift . 300)) (($ident shift 
    . 26) ($string shift . 50) ($float shift . 197) ($fixed shift . 198) 
    ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}#
    shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 
    203) ($:true shift . 204) ($:false shift . 205) (string shift . 206) 
    (unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 297)) ((#{$:;}# reduce 
    . 208) ($string reduce . 208) ($:annotation reduce . 208)) ((#{$:;}# 
    reduce . 207) ($string reduce . 207) ($:annotation reduce . 207)) (
    (#{$:;}# reduce . 206) ($string reduce . 206) ($:annotation reduce . 206))
    ((#{$:;}# reduce . 205) ($string reduce . 205) ($:annotation reduce . 205)
    ) ((#{$:;}# reduce . 204) ($string reduce . 204) ($:annotation reduce . 
    204)) ((#{$:;}# reduce . 203) ($string reduce . 203) ($:annotation reduce 
    . 203)) (($ident shift . 26) ($string shift . 50) ($float shift . 197) 
    ($fixed shift . 198) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# 
    shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 201) ($:der 
    shift . 202) (name shift . 203) ($:true shift . 204) ($:false shift . 205)
    (string shift . 206) (unsigned-number shift . 207) (primary shift . 208) 
    (factor shift . 209) (term shift . 210) (arithmetic-expression shift . 211
    ) ($:not shift . 212) (relation shift . 213) (logical-factor shift . 214) 
    (logical-term shift . 215) (logical-expression shift . 216) ($:if shift . 
    217) (simple-expression shift . 218) (expression shift . 294) ($:, shift 
    . 295) (output-expression-list shift . 296)) (($::= shift . 291) (
    #{$:\x28;}# shift . 292) (function-call-args shift . 293)) (($string shift
    . 50) (string shift . 51) (string-cat shift . 52) (string-comment shift . 
    185) (comment shift . 290) (#{$:;}# reduce . 343) ($:annotation reduce . 
    343)) ((#{$:;}# shift . 289)) (($ident shift . 26) ($:. shift . 243) 
    (ident shift . 271) (component-reference-1 shift . 244) ($:when shift . 
    272) ($:while shift . 273) ($:for shift . 274) ($:if shift . 275) (
    when-statement shift . 276) (while-statement shift . 277) (for-statement 
    shift . 278) (if-statement shift . 279) ($:return shift . 280) ($:break 
    shift . 281) (#{$:\x28;}# shift . 282) (component-reference shift . 283) 
    (statement-1 shift . 284) (statement shift . 288) ($:annotation reduce . 
    184) ($:public reduce . 184) ($:protected reduce . 184) ($:initial reduce 
    . 184) ($:algorithm reduce . 184) ($:equation reduce . 184) ($:external 
    reduce . 184) ($:end reduce . 184)) ((#{$:;}# reduce . 33) (
    $:constrainedby reduce . 33)) ((#{$:;}# shift . 475)) (($ident reduce . 
    197) ($:. reduce . 197) ($:if reduce . 197) ($:for reduce . 197) ($:while 
    reduce . 197) ($:when reduce . 197) ($:return reduce . 197) ($:break 
    reduce . 197) (#{$:\x28;}# reduce . 197) ($:end reduce . 197) (
    $:annotation reduce . 197) ($:public reduce . 197) ($:protected reduce . 
    197) ($:initial reduce . 197) ($:algorithm reduce . 197) ($:equation 
    reduce . 197) ($:external reduce . 197) ($:elseif reduce . 197) ($:else 
    reduce . 197) ($:elsewhen reduce . 197)) ((#{$:;}# reduce . 199)) (
    ($ident shift . 26) ($string shift . 50) ($float shift . 197) ($fixed 
    shift . 198) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199)
    (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) 
    (name shift . 203) ($:true shift . 204) ($:false shift . 205) (string 
    shift . 206) (unsigned-number shift . 207) (primary shift . 208) (factor 
    shift . 209) (term shift . 210) (arithmetic-expression shift . 211) 
    ($:not shift . 212) (relation shift . 213) (logical-factor shift . 214) 
    (logical-term shift . 215) (logical-expression shift . 216) ($:if shift . 
    217) (simple-expression shift . 218) (expression shift . 474)) (($string 
    shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) 
    (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 
    201) ($:der shift . 202) (name shift . 203) ($:true shift . 204) ($:false 
    shift . 205) (string shift . 206) (unsigned-number shift . 207) (primary 
    shift . 208) (factor shift . 209) (term shift . 210) (
    arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 216) ($ident shift . 26) ($:if shift . 217) 
    (simple-expression shift . 218) (expression shift . 371) ($:function shift
    . 372) (ident shift . 373) (named-argument shift . 374) (function-argument
    shift . 375) (named-only-arguments-1 shift . 376) (function-arguments-1 
    shift . 377) (function-arguments shift . 472) (#{$:\x29;}# shift . 473)) 
    ((#{$:;}# reduce . 201) ($string reduce . 201) ($:annotation reduce . 201)
    ) ((#{$:\x29;}# reduce . 332) ($:, reduce . 332)) ((#{$:\x29;}# reduce . 
    331) ($:, reduce . 331)) ((#{$:\x29;}# shift . 471) ($:, shift . 431)) 
    (($:then shift . 465) (then-st-part shift . 470)) (($:in shift . 469) 
    ($:loop reduce . 240) ($:, reduce . 240)) (($:loop reduce . 237) ($:, 
    reduce . 237)) (($:, shift . 460) ($:loop shift . 468)) (($:loop shift . 
    467)) (($:then shift . 465) (then-st-part shift . 466)) (($:. reduce . 314
    ) ($:= reduce . 314) ($::= reduce . 314) (#{$:\x28;}# reduce . 314) 
    ($:, reduce . 314) (#{$:\x29;}# reduce . 314)) (($:. reduce . 311) 
    ($:= reduce . 311) ($::= reduce . 311) (#{$:\x28;}# reduce . 311) ($:, 
    reduce . 311) (#{$:\x29;}# reduce . 311)) ((#{$:;}# shift . 464)) (
    ($float reduce . 188) ($fixed reduce . 188) ($string reduce . 188) 
    (#{$:\x7b;}# reduce . 188) (#{$:\x5b;}# reduce . 188) (#{$:\x28;}# reduce 
    . 188) ($:der reduce . 188) ($:true reduce . 188) ($:false reduce . 188) 
    ($:not reduce . 188) ($:if reduce . 188) ($:for reduce . 188) ($:connect 
    reduce . 188) ($:when reduce . 188) ($ident reduce . 188) ($:. reduce . 
    188) ($:end reduce . 188) ($:annotation reduce . 188) ($:public reduce . 
    188) ($:protected reduce . 188) ($:initial reduce . 188) ($:algorithm 
    reduce . 188) ($:equation reduce . 188) ($:external reduce . 188) (
    $:elseif reduce . 188) ($:else reduce . 188) ($:elsewhen reduce . 188)) 
    ((#{$:;}# reduce . 190)) (($ident shift . 26) ($string shift . 50) 
    ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident shift . 
    28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift
    . 201) ($:der shift . 202) (name shift . 203) ($:true shift . 204) 
    ($:false shift . 205) (string shift . 206) (unsigned-number shift . 207) 
    (primary shift . 208) (factor shift . 209) (term shift . 210) (
    arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 216) ($:if shift . 217) (simple-expression 
    shift . 218) (expression shift . 463)) (($:= reduce . 295) ($:: reduce . 
    295) ($:^ reduce . 295) ($:.^ reduce . 295) ($:./ reduce . 295) ($:.* 
    reduce . 295) ($:/ reduce . 295) ($:* reduce . 295) ($:+ reduce . 295) 
    ($:- reduce . 295) ($:.+ reduce . 295) ($:.- reduce . 295) ($:<> reduce . 
    295) ($:== reduce . 295) ($:>= reduce . 295) ($:> reduce . 295) ($:<= 
    reduce . 295) ($:< reduce . 295) ($:and reduce . 295) ($:or reduce . 295) 
    (#{$:;}# reduce . 196) ($string reduce . 196) ($:annotation reduce . 196))
    ((#{$:\x5d;}# reduce . 298) ($:: reduce . 298) ($:^ reduce . 298) ($:.^ 
    reduce . 298) ($:./ reduce . 298) ($:.* reduce . 298) ($:/ reduce . 298) 
    ($:* reduce . 298) ($:+ reduce . 298) ($:- reduce . 298) ($:.+ reduce . 
    298) ($:.- reduce . 298) ($:<> reduce . 298) ($:== reduce . 298) ($:>= 
    reduce . 298) ($:> reduce . 298) ($:<= reduce . 298) ($:< reduce . 298) 
    ($:and reduce . 298) ($:or reduce . 298) ($:, reduce . 298) (#{$:\x7d;}# 
    reduce . 298) (#{$:;}# reduce . 298) (#{$:\x29;}# reduce . 298) ($:then 
    reduce . 298) ($:loop reduce . 298) ($:else reduce . 298) ($:elseif reduce
    . 298) ($string reduce . 298) ($:annotation reduce . 298) ($:constrainedby
    reduce . 298) ($:= reduce . 298) ($:end reduce . 298) ($:elsewhen reduce 
    . 298) ($:if reduce . 298)) (($:then shift . 458) (then-eq-part shift . 
    462)) (($:, shift . 460) ($:loop shift . 461)) (($:then shift . 458) 
    (then-eq-part shift . 459)) (($ident shift . 26) ($:. shift . 243) 
    (ident shift . 271) (component-reference-1 shift . 244) (
    component-reference shift . 457)) (($ident shift . 26) ($:. shift . 243) 
    (ident shift . 271) (component-reference-1 shift . 244) ($:when shift . 
    272) ($:while shift . 273) ($:for shift . 274) ($:if shift . 275) (
    when-statement shift . 276) (while-statement shift . 277) (for-statement 
    shift . 278) (if-statement shift . 279) ($:return shift . 280) ($:break 
    shift . 281) (#{$:\x28;}# shift . 282) (component-reference shift . 283) 
    (statement-1 shift . 284) (statement shift . 288) ($:annotation reduce . 
    183) ($:public reduce . 183) ($:protected reduce . 183) ($:initial reduce 
    . 183) ($:algorithm reduce . 183) ($:equation reduce . 183) ($:external 
    reduce . 183) ($:end reduce . 183)) (($:annotation reduce . 179) ($:public
    reduce . 179) ($:protected reduce . 179) ($:initial reduce . 179) (
    $:algorithm reduce . 179) ($:equation reduce . 179) ($:external reduce . 
    179) ($:end reduce . 179)) (($:end reduce . 50)) (($:annotation reduce . 
    69) ($:end reduce . 69)) (($:annotation reduce . 68) ($:end reduce . 68)) 
    ((#{$:;}# shift . 456)) (($ident shift . 26) (ident shift . 455)) (
    (#{$:\x29;}# shift . 453) ($ident shift . 26) ($string shift . 50) 
    ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident shift . 
    28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift
    . 201) ($:der shift . 202) (name shift . 203) ($:true shift . 204) 
    ($:false shift . 205) (string shift . 206) (unsigned-number shift . 207) 
    (primary shift . 208) (factor shift . 209) (term shift . 210) (
    arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 216) ($:if shift . 217) (simple-expression 
    shift . 218) (expression shift . 368) (expression-list shift . 454)) 
    (($ident shift . 26) (ident shift . 452)) ((#{$:\x5b;}# shift . 137) 
    (array-subscripts shift . 450) ($P14 shift . 451) (#{$:\x29;}# reduce . 
    315) ($:, reduce . 315) ($:= reduce . 315) (#{$:\x28;}# reduce . 315) 
    ($::= reduce . 315) ($:. reduce . 315)) (($:annotation reduce . 67) 
    ($:end reduce . 67)) ((#{$:;}# shift . 449)) (($:annotation shift . 117) 
    (annotation shift . 447) (#{$:;}# shift . 448)) (($:function reduce . 91) 
    ($:pure reduce . 91) ($:impure reduce . 91) ($:package reduce . 91) 
    ($:type reduce . 91) ($:connector reduce . 91) ($:expandable reduce . 91) 
    ($:block reduce . 91) ($:record reduce . 91) ($:operator reduce . 91) 
    ($:model reduce . 91) ($:class reduce . 91) ($:partial reduce . 91) 
    ($:encapsulated reduce . 91) ($:stream reduce . 91) ($:flow reduce . 91) 
    ($:constant reduce . 91) ($:parameter reduce . 91) ($:discrete reduce . 91
    ) ($:output reduce . 91) ($:input reduce . 91) ($ident reduce . 91) 
    ($:. reduce . 91) ($:replaceable reduce . 91)) (($ident shift . 26) 
    ($:. shift . 27) (ident shift . 28) ($:output shift . 73) ($:input shift 
    . 74) ($:constant shift . 75) ($:parameter shift . 76) ($:discrete shift 
    . 77) ($:stream shift . 78) ($:flow shift . 79) ($:function shift . 1) 
    ($:pure shift . 2) ($:impure shift . 3) ($:package shift . 4) ($:type 
    shift . 5) ($:connector shift . 6) ($:expandable shift . 7) ($:block shift
    . 8) ($:record shift . 9) ($:operator shift . 10) ($:model shift . 11) 
    ($:class shift . 12) (name shift . 80) (type-prefix-3 shift . 81) (
    type-prefix-2 shift . 82) (type-prefix-1 shift . 83) (class-prefixes-1 
    shift . 13) ($:partial shift . 14) (type-specifier shift . 84) (
    type-prefix shift . 85) (class-prefixes shift . 15) ($:encapsulated shift 
    . 16) ($:replaceable shift . 88) (component-clause shift . 89) (
    class-definition shift . 90) (element-1 shift . 446)) ((#{$:;}# reduce . 
    82)) ((#{$:;}# reduce . 100)) (($:. shift . 59) (#{$:\x28;}# shift . 66) 
    (class-modification shift . 445) ($string reduce . 118) ($:annotation 
    reduce . 118) (#{$:;}# reduce . 118) (#{$:\x29;}# reduce . 118) ($:, 
    reduce . 118)) ((#{$:\x7d;}# shift . 442) ($ident shift . 26) (ident shift
    . 443) (import-list shift . 444)) (($string reduce . 108) ($:annotation 
    reduce . 108) (#{$:;}# reduce . 108)) (($string reduce . 106) (
    $:annotation reduce . 106) (#{$:;}# reduce . 106)) (($:. shift . 59) 
    ($string reduce . 105) ($:annotation reduce . 105) (#{$:;}# reduce . 105))
    ((#{$:;}# reduce . 113)) (($:, shift . 190) (#{$:;}# reduce . 119) 
    ($:constrainedby reduce . 119)) (($ident reduce . 336) ($:= reduce . 336) 
    ($::= reduce . 336) (#{$:\x28;}# reduce . 336) ($string reduce . 336) 
    ($:annotation reduce . 336) ($:if reduce . 336) ($:, reduce . 336) 
    (#{$:;}# reduce . 336) ($:constrainedby reduce . 336) (#{$:\x5d;}# reduce 
    . 336) ($:: reduce . 336) ($:^ reduce . 336) ($:.^ reduce . 336) ($:./ 
    reduce . 336) ($:.* reduce . 336) ($:/ reduce . 336) ($:* reduce . 336) 
    ($:+ reduce . 336) ($:- reduce . 336) ($:.+ reduce . 336) ($:.- reduce . 
    336) ($:<> reduce . 336) ($:== reduce . 336) ($:>= reduce . 336) ($:> 
    reduce . 336) ($:<= reduce . 336) ($:< reduce . 336) ($:and reduce . 336) 
    ($:or reduce . 336) (#{$:\x7d;}# reduce . 336) (#{$:\x29;}# reduce . 336) 
    ($:then reduce . 336) ($:loop reduce . 336) ($:else reduce . 336) (
    $:elseif reduce . 336) ($:. reduce . 336) ($:stream reduce . 336) ($:flow 
    reduce . 336) ($:constant reduce . 336) ($:parameter reduce . 336) 
    ($:discrete reduce . 336) ($:output reduce . 336) ($:input reduce . 336) 
    ($:end reduce . 336) ($:elsewhen reduce . 336)) (($ident shift . 26) 
    ($string shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 
    200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) 
    ($:true shift . 204) ($:false shift . 205) (string shift . 206) (
    unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 219) ($:: shift . 220) 
    (subscript shift . 441)) (($:then shift . 440)) (($ident shift . 26) 
    ($string shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 
    200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) 
    ($:true shift . 204) ($:false shift . 205) (string shift . 206) (
    unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 439)) (($ident shift . 26) ($string shift
    . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (
    #{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) ($:true 
    shift . 204) ($:false shift . 205) (string shift . 206) (unsigned-number 
    shift . 207) (primary shift . 208) (factor shift . 209) (term shift . 210)
    (arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 438)) (($ident 
    shift . 26) ($string shift . 50) ($float shift . 197) ($fixed shift . 198)
    ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}#
    shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 
    203) ($:true shift . 204) ($:false shift . 205) (string shift . 206) 
    (unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 437)) (($float reduce . 275
    ) ($fixed reduce . 275) ($string reduce . 275) ($ident reduce . 275) 
    ($:. reduce . 275) (#{$:\x7b;}# reduce . 275) (#{$:\x5b;}# reduce . 275) 
    (#{$:\x28;}# reduce . 275) ($:der reduce . 275) ($:true reduce . 275) 
    ($:false reduce . 275)) (($float reduce . 274) ($fixed reduce . 274) 
    ($string reduce . 274) ($ident reduce . 274) ($:. reduce . 274) (
    #{$:\x7b;}# reduce . 274) (#{$:\x5b;}# reduce . 274) (#{$:\x28;}# reduce 
    . 274) ($:der reduce . 274) ($:true reduce . 274) ($:false reduce . 274)) 
    (($float reduce . 273) ($fixed reduce . 273) ($string reduce . 273) 
    ($ident reduce . 273) ($:. reduce . 273) (#{$:\x7b;}# reduce . 273) 
    (#{$:\x5b;}# reduce . 273) (#{$:\x28;}# reduce . 273) ($:der reduce . 273)
    ($:true reduce . 273) ($:false reduce . 273)) (($float reduce . 272) 
    ($fixed reduce . 272) ($string reduce . 272) ($ident reduce . 272) 
    ($:. reduce . 272) (#{$:\x7b;}# reduce . 272) (#{$:\x5b;}# reduce . 272) 
    (#{$:\x28;}# reduce . 272) ($:der reduce . 272) ($:true reduce . 272) 
    ($:false reduce . 272)) (($float reduce . 271) ($fixed reduce . 271) 
    ($string reduce . 271) ($ident reduce . 271) ($:. reduce . 271) (
    #{$:\x7b;}# reduce . 271) (#{$:\x5b;}# reduce . 271) (#{$:\x28;}# reduce 
    . 271) ($:der reduce . 271) ($:true reduce . 271) ($:false reduce . 271)) 
    (($float reduce . 270) ($fixed reduce . 270) ($string reduce . 270) 
    ($ident reduce . 270) ($:. reduce . 270) (#{$:\x7b;}# reduce . 270) 
    (#{$:\x5b;}# reduce . 270) (#{$:\x28;}# reduce . 270) ($:der reduce . 270)
    ($:true reduce . 270) ($:false reduce . 270)) (($ident shift . 26) 
    ($string shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 
    200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) 
    ($:true shift . 204) ($:false shift . 205) (string shift . 206) (
    unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 436)) (($:<> shift . 345
    ) ($:== shift . 346) ($:>= shift . 347) ($:> shift . 348) ($:<= shift . 
    349) ($:< shift . 350) (rel-op shift . 351) ($:: reduce . 267) ($:= reduce
    . 267) ($:or reduce . 267) ($:and reduce . 267) (#{$:\x5d;}# reduce . 267)
    ($:, reduce . 267) (#{$:\x7d;}# reduce . 267) (#{$:;}# reduce . 267) 
    (#{$:\x29;}# reduce . 267) ($:then reduce . 267) ($:loop reduce . 267) 
    ($:else reduce . 267) ($:elseif reduce . 267) ($string reduce . 267) 
    ($:annotation reduce . 267) ($:constrainedby reduce . 267) ($:end reduce 
    . 267) ($:elsewhen reduce . 267) ($:if reduce . 267)) (($float reduce . 
    281) ($fixed reduce . 281) ($string reduce . 281) ($ident reduce . 281) 
    ($:. reduce . 281) (#{$:\x7b;}# reduce . 281) (#{$:\x5b;}# reduce . 281) 
    (#{$:\x28;}# reduce . 281) ($:der reduce . 281) ($:true reduce . 281) 
    ($:false reduce . 281)) (($float reduce . 280) ($fixed reduce . 280) 
    ($string reduce . 280) ($ident reduce . 280) ($:. reduce . 280) (
    #{$:\x7b;}# reduce . 280) (#{$:\x5b;}# reduce . 280) (#{$:\x28;}# reduce 
    . 280) ($:der reduce . 280) ($:true reduce . 280) ($:false reduce . 280)) 
    (($float reduce . 279) ($fixed reduce . 279) ($string reduce . 279) 
    ($ident reduce . 279) ($:. reduce . 279) (#{$:\x7b;}# reduce . 279) 
    (#{$:\x5b;}# reduce . 279) (#{$:\x28;}# reduce . 279) ($:der reduce . 279)
    ($:true reduce . 279) ($:false reduce . 279)) (($float reduce . 278) 
    ($fixed reduce . 278) ($string reduce . 278) ($ident reduce . 278) 
    ($:. reduce . 278) (#{$:\x7b;}# reduce . 278) (#{$:\x5b;}# reduce . 278) 
    (#{$:\x28;}# reduce . 278) ($:der reduce . 278) ($:true reduce . 278) 
    ($:false reduce . 278)) (($ident shift . 26) ($string shift . 50) ($float 
    shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident shift . 28) 
    (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 
    201) ($:der shift . 202) (name shift . 203) ($:true shift . 204) ($:false 
    shift . 205) (string shift . 206) (unsigned-number shift . 207) (primary 
    shift . 208) (factor shift . 209) (term shift . 435)) (($float reduce . 
    287) ($fixed reduce . 287) ($string reduce . 287) ($ident reduce . 287) 
    ($:. reduce . 287) (#{$:\x7b;}# reduce . 287) (#{$:\x5b;}# reduce . 287) 
    (#{$:\x28;}# reduce . 287) ($:der reduce . 287) ($:true reduce . 287) 
    ($:false reduce . 287)) (($float reduce . 286) ($fixed reduce . 286) 
    ($string reduce . 286) ($ident reduce . 286) ($:. reduce . 286) (
    #{$:\x7b;}# reduce . 286) (#{$:\x5b;}# reduce . 286) (#{$:\x28;}# reduce 
    . 286) ($:der reduce . 286) ($:true reduce . 286) ($:false reduce . 286)) 
    (($float reduce . 285) ($fixed reduce . 285) ($string reduce . 285) 
    ($ident reduce . 285) ($:. reduce . 285) (#{$:\x7b;}# reduce . 285) 
    (#{$:\x5b;}# reduce . 285) (#{$:\x28;}# reduce . 285) ($:der reduce . 285)
    ($:true reduce . 285) ($:false reduce . 285)) (($float reduce . 284) 
    ($fixed reduce . 284) ($string reduce . 284) ($ident reduce . 284) 
    ($:. reduce . 284) (#{$:\x7b;}# reduce . 284) (#{$:\x5b;}# reduce . 284) 
    (#{$:\x28;}# reduce . 284) ($:der reduce . 284) ($:true reduce . 284) 
    ($:false reduce . 284)) (($ident shift . 26) ($string shift . 50) ($float 
    shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident shift . 28) 
    (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 
    201) ($:der shift . 202) (name shift . 203) ($:true shift . 204) ($:false 
    shift . 205) (string shift . 206) (unsigned-number shift . 207) (primary 
    shift . 208) (factor shift . 434)) (($ident shift . 26) ($string shift . 
    50) ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (
    #{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) ($:true 
    shift . 204) ($:false shift . 205) (string shift . 206) (unsigned-number 
    shift . 207) (primary shift . 433)) (($ident shift . 26) ($string shift . 
    50) ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (
    #{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) ($:true 
    shift . 204) ($:false shift . 205) (string shift . 206) (unsigned-number 
    shift . 207) (primary shift . 432)) ((#{$:\x5d;}# reduce . 295) ($:: 
    reduce . 295) ($:^ reduce . 295) ($:.^ reduce . 295) ($:./ reduce . 295) 
    ($:.* reduce . 295) ($:/ reduce . 295) ($:* reduce . 295) ($:+ reduce . 
    295) ($:- reduce . 295) ($:.+ reduce . 295) ($:.- reduce . 295) ($:<> 
    reduce . 295) ($:== reduce . 295) ($:>= reduce . 295) ($:> reduce . 295) 
    ($:<= reduce . 295) ($:< reduce . 295) ($:and reduce . 295) ($:or reduce 
    . 295) ($:, reduce . 295) (#{$:\x7d;}# reduce . 295) (#{$:;}# reduce . 295
    ) (#{$:\x29;}# reduce . 295) ($:then reduce . 295) ($:loop reduce . 295) 
    ($:else reduce . 295) ($:elseif reduce . 295) ($string reduce . 295) 
    ($:annotation reduce . 295) ($:constrainedby reduce . 295) ($:= reduce . 
    295) ($:end reduce . 295) ($:elsewhen reduce . 295) ($:if reduce . 295)) 
    (($:= reduce . 296) ($:: reduce . 296) ($:^ reduce . 296) ($:.^ reduce . 
    296) ($:./ reduce . 296) ($:.* reduce . 296) ($:/ reduce . 296) ($:* 
    reduce . 296) ($:+ reduce . 296) ($:- reduce . 296) ($:.+ reduce . 296) 
    ($:.- reduce . 296) ($:<> reduce . 296) ($:== reduce . 296) ($:>= reduce 
    . 296) ($:> reduce . 296) ($:<= reduce . 296) ($:< reduce . 296) ($:and 
    reduce . 296) ($:or reduce . 296) (#{$:\x5d;}# reduce . 296) ($:, reduce 
    . 296) (#{$:\x7d;}# reduce . 296) (#{$:;}# reduce . 296) (#{$:\x29;}# 
    reduce . 296) ($:then reduce . 296) ($:loop reduce . 296) ($:else reduce 
    . 296) ($:elseif reduce . 296) ($string reduce . 296) ($:annotation reduce
    . 296) ($:constrainedby reduce . 296) ($:end reduce . 296) ($:elsewhen 
    reduce . 296) ($:if reduce . 296)) ((#{$:\x29;}# shift . 430) ($:, shift 
    . 431)) ((#{$:\x5d;}# reduce . 334) ($:, reduce . 334) (#{$:;}# reduce . 
    334) (#{$:\x29;}# reduce . 334) ($:end reduce . 334) ($:elsewhen reduce . 
    334)) (($:, shift . 429) (#{$:\x5d;}# reduce . 302) (#{$:;}# reduce . 302)
    ) ((#{$:\x5d;}# shift . 427) (#{$:;}# shift . 428)) ((#{$:\x7d;}# reduce 
    . 330) ($:, reduce . 330) (#{$:\x29;}# reduce . 330)) (($ident shift . 26)
    ($:. shift . 27) (ident shift . 28) (name shift . 426)) (($:= shift . 425)
    (#{$:\x5b;}# reduce . 304) ($:: reduce . 304) (#{$:\x7d;}# reduce . 304) 
    (#{$:\x28;}# reduce . 304) ($:, reduce . 304) ($:or reduce . 304) ($:and 
    reduce . 304) ($:< reduce . 304) ($:<= reduce . 304) ($:> reduce . 304) 
    ($:>= reduce . 304) ($:== reduce . 304) ($:<> reduce . 304) ($:.- reduce 
    . 304) ($:.+ reduce . 304) ($:- reduce . 304) ($:+ reduce . 304) ($:* 
    reduce . 304) ($:/ reduce . 304) ($:.* reduce . 304) ($:./ reduce . 304) 
    ($:.^ reduce . 304) ($:^ reduce . 304) ($:. reduce . 304) (#{$:\x29;}# 
    reduce . 304)) ((#{$:\x7d;}# reduce . 324) ($:, reduce . 324) (#{$:\x29;}#
    reduce . 324)) ((#{$:\x7d;}# reduce . 321) ($:, reduce . 321) (#{$:\x29;}#
    reduce . 321)) (($:, shift . 424) (#{$:\x7d;}# reduce . 320) (#{$:\x29;}# 
    reduce . 320)) (($:, shift . 423) (#{$:\x7d;}# reduce . 319) (#{$:\x29;}# 
    reduce . 319)) ((#{$:\x7d;}# shift . 422)) (($string reduce . 147) 
    ($:annotation reduce . 147) ($:if reduce . 147) ($:, reduce . 147) 
    (#{$:;}# reduce . 147) ($:constrainedby reduce . 147) (#{$:\x29;}# reduce 
    . 147)) (($string reduce . 143) ($:annotation reduce . 143) ($:if reduce 
    . 143) ($:, reduce . 143) (#{$:;}# reduce . 143) ($:constrainedby reduce 
    . 143) (#{$:\x29;}# reduce . 143)) (($:, reduce . 140) (#{$:;}# reduce . 
    140) ($:constrainedby reduce . 140)) (($string reduce . 142) ($:annotation
    reduce . 142) ($:, reduce . 142) (#{$:;}# reduce . 142) ($:constrainedby 
    reduce . 142)) (($:, reduce . 139) (#{$:;}# reduce . 139) ($:constrainedby
    reduce . 139)) (($string shift . 50) (string shift . 51) (string-cat shift
    . 52) (string-comment shift . 185) (comment shift . 421) (#{$:;}# reduce 
    . 343) ($:constrainedby reduce . 343) ($:stream reduce . 343) ($:flow 
    reduce . 343) ($:constant reduce . 343) ($:parameter reduce . 343) 
    ($:discrete reduce . 343) ($:output reduce . 343) ($:input reduce . 343) 
    (#{$:\x29;}# reduce . 343) ($:, reduce . 343) ($:annotation reduce . 343))
    ((#{$:;}# reduce . 37) ($:constrainedby reduce . 37) ($:stream reduce . 37
    ) ($:flow reduce . 37) ($:constant reduce . 37) ($:parameter reduce . 37) 
    ($:discrete reduce . 37) ($:output reduce . 37) ($:input reduce . 37) 
    (#{$:\x29;}# reduce . 37) ($:, reduce . 37)) ((#{$:;}# reduce . 38) 
    ($:constrainedby reduce . 38) ($:stream reduce . 38) ($:flow reduce . 38) 
    ($:constant reduce . 38) ($:parameter reduce . 38) ($:discrete reduce . 38
    ) ($:output reduce . 38) ($:input reduce . 38) (#{$:\x29;}# reduce . 38) 
    ($:, reduce . 38)) ((#{$:;}# reduce . 341) ($:, reduce . 341) (
    $:constrainedby reduce . 341) (#{$:\x29;}# reduce . 341) ($:stream reduce 
    . 341) ($:flow reduce . 341) ($:constant reduce . 341) ($:parameter reduce
    . 341) ($:discrete reduce . 341) ($:output reduce . 341) ($:input reduce 
    . 341)) (($string shift . 50) (string shift . 51) (string-cat shift . 52) 
    (string-comment shift . 185) (comment shift . 420) (#{$:;}# reduce . 343) 
    ($:constrainedby reduce . 343) ($:stream reduce . 343) ($:flow reduce . 
    343) ($:constant reduce . 343) ($:parameter reduce . 343) ($:discrete 
    reduce . 343) ($:output reduce . 343) ($:input reduce . 343) (#{$:\x29;}# 
    reduce . 343) ($:, reduce . 343) ($:annotation reduce . 343)) (($ident 
    shift . 26) (ident shift . 182) (enumeration-literal shift . 419)) 
    ((#{$:\x29;}# reduce . 49) ($:, reduce . 49)) (($string shift . 50) 
    (string shift . 51) (string-cat shift . 52) (string-comment shift . 185) 
    (comment shift . 418) (#{$:;}# reduce . 343) ($:constrainedby reduce . 343
    ) ($:stream reduce . 343) ($:flow reduce . 343) ($:constant reduce . 343) 
    ($:parameter reduce . 343) ($:discrete reduce . 343) ($:output reduce . 
    343) ($:input reduce . 343) (#{$:\x29;}# reduce . 343) ($:, reduce . 343) 
    ($:annotation reduce . 343)) (($ident shift . 26) (ident shift . 416) 
    (der-class-specifier-1 shift . 417)) (($ident shift . 26) (ident shift . 
    415)) (($ident shift . 26) ($:. shift . 27) (ident shift . 28) (name shift
    . 80) (type-specifier shift . 414)) (($:constrainedby shift . 230) 
    (constraining-clause shift . 413) (#{$:\x29;}# reduce . 176) ($:, reduce 
    . 176)) (($:= shift . 412)) (($:stream reduce . 178) ($:flow reduce . 178)
    ($:constant reduce . 178) ($:parameter reduce . 178) ($:discrete reduce . 
    178) ($:output reduce . 178) ($:input reduce . 178) (#{$:\x29;}# reduce . 
    178) ($:, reduce . 178)) ((#{$:\x29;}# reduce . 164) ($:, reduce . 164)) 
    (($ident shift . 26) ($string shift . 50) ($float shift . 197) ($fixed 
    shift . 198) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199)
    (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) 
    (name shift . 203) ($:true shift . 204) ($:false shift . 205) (string 
    shift . 206) (unsigned-number shift . 207) (primary shift . 208) (factor 
    shift . 209) (term shift . 210) (arithmetic-expression shift . 211) 
    ($:not shift . 212) (relation shift . 213) (logical-factor shift . 214) 
    (logical-term shift . 215) (logical-expression shift . 216) ($:if shift . 
    217) (simple-expression shift . 218) (expression shift . 411)) (($string 
    reduce . 150) (#{$:\x29;}# reduce . 150) ($:, reduce . 150) ($:annotation 
    reduce . 150) ($:if reduce . 150) (#{$:;}# reduce . 150) ($:constrainedby 
    reduce . 150)) (($string reduce . 151) (#{$:\x29;}# reduce . 151) ($:, 
    reduce . 151) ($:annotation reduce . 151) ($:if reduce . 151) (#{$:;}# 
    reduce . 151) ($:constrainedby reduce . 151)) (($:function reduce . 171) 
    ($:pure reduce . 171) ($:impure reduce . 171) ($:package reduce . 171) 
    ($:type reduce . 171) ($:connector reduce . 171) ($:expandable reduce . 
    171) ($:block reduce . 171) ($:record reduce . 171) ($:operator reduce . 
    171) ($:model reduce . 171) ($:class reduce . 171) ($:partial reduce . 171
    ) ($:stream reduce . 171) ($:flow reduce . 171) ($:constant reduce . 171) 
    ($:parameter reduce . 171) ($:discrete reduce . 171) ($:output reduce . 
    171) ($:input reduce . 171) ($:replaceable reduce . 171)) (($:output shift
    . 73) ($:input shift . 74) ($:constant shift . 75) ($:parameter shift . 76
    ) ($:discrete shift . 77) ($:stream shift . 78) ($:flow shift . 79) 
    ($:function shift . 1) ($:pure shift . 2) ($:impure shift . 3) ($:package 
    shift . 4) ($:type shift . 5) ($:connector shift . 6) ($:expandable shift 
    . 7) ($:block shift . 8) ($:record shift . 9) ($:operator shift . 10) 
    ($:model shift . 11) ($:class shift . 12) (type-prefix-3 shift . 81) 
    (type-prefix-2 shift . 82) (type-prefix-1 shift . 83) (class-prefixes-1 
    shift . 13) ($:partial shift . 14) (class-prefixes shift . 177) (
    type-prefix shift . 394) ($:replaceable shift . 150) (element-replaceable 
    shift . 407) (component-clause1 shift . 408) (short-class-definition shift
    . 409) (elt-redecl-1 shift . 410)) ((#{$:\x29;}# reduce . 158) ($:, reduce
    . 158)) ((#{$:\x29;}# reduce . 155) ($:, reduce . 155)) ((#{$:;}# reduce 
    . 35) ($:constrainedby reduce . 35)) ((#{$:\x29;}# reduce . 174) ($:, 
    reduce . 174)) ((#{$:\x29;}# reduce . 173) ($:, reduce . 173)) ((
    #{$:\x29;}# reduce . 172) ($:, reduce . 172)) ((#{$:\x29;}# reduce . 167) 
    ($:, reduce . 167)) (($string reduce . 148) (#{$:\x29;}# reduce . 148) 
    ($:, reduce . 148) ($:annotation reduce . 148) ($:if reduce . 148) 
    (#{$:;}# reduce . 148) ($:constrainedby reduce . 148)) (($:enumeration 
    shift . 69) ($:output shift . 70) ($:input shift . 71) (base-prefix shift 
    . 72)) ((#{$:\x29;}# reduce . 175) ($:, reduce . 175)) (($ident shift . 26
    ) (ident shift . 139) (declaration shift . 527)) ((#{$:;}# reduce . 34) 
    ($:constrainedby reduce . 34)) ((#{$:\x29;}# reduce . 43) (#{$:;}# reduce 
    . 43)) ((#{$:\x29;}# shift . 525) (#{$:;}# shift . 526)) ((#{$:;}# reduce 
    . 41) ($:constrainedby reduce . 41) ($:stream reduce . 41) ($:flow reduce 
    . 41) ($:constant reduce . 41) ($:parameter reduce . 41) ($:discrete 
    reduce . 41) ($:output reduce . 41) ($:input reduce . 41) (#{$:\x29;}# 
    reduce . 41) ($:, reduce . 41)) ((#{$:\x29;}# reduce . 48) ($:, reduce . 
    48)) ((#{$:;}# reduce . 40) ($:constrainedby reduce . 40) ($:stream reduce
    . 40) ($:flow reduce . 40) ($:constant reduce . 40) ($:parameter reduce . 
    40) ($:discrete reduce . 40) ($:output reduce . 40) ($:input reduce . 40) 
    (#{$:\x29;}# reduce . 40) ($:, reduce . 40)) ((#{$:;}# reduce . 36) 
    ($:constrainedby reduce . 36) ($:stream reduce . 36) ($:flow reduce . 36) 
    ($:constant reduce . 36) ($:parameter reduce . 36) ($:discrete reduce . 36
    ) ($:output reduce . 36) ($:input reduce . 36) (#{$:\x29;}# reduce . 36) 
    ($:, reduce . 36)) (($:= reduce . 301) ($:: reduce . 301) ($:^ reduce . 
    301) ($:.^ reduce . 301) ($:./ reduce . 301) ($:.* reduce . 301) ($:/ 
    reduce . 301) ($:* reduce . 301) ($:+ reduce . 301) ($:- reduce . 301) 
    ($:.+ reduce . 301) ($:.- reduce . 301) ($:<> reduce . 301) ($:== reduce 
    . 301) ($:>= reduce . 301) ($:> reduce . 301) ($:<= reduce . 301) ($:< 
    reduce . 301) ($:and reduce . 301) ($:or reduce . 301) (#{$:\x5d;}# reduce
    . 301) ($:, reduce . 301) (#{$:\x7d;}# reduce . 301) (#{$:;}# reduce . 301
    ) (#{$:\x29;}# reduce . 301) ($:then reduce . 301) ($:loop reduce . 301) 
    ($:else reduce . 301) ($:elseif reduce . 301) ($string reduce . 301) 
    ($:annotation reduce . 301) ($:constrainedby reduce . 301) ($:end reduce 
    . 301) ($:elsewhen reduce . 301) ($:if reduce . 301)) ((named-argument 
    shift . 523) ($ident shift . 26) ($string shift . 50) ($float shift . 197)
    ($fixed shift . 198) ($:. shift . 27) (ident shift . 373) (#{$:\x7b;}# 
    shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 201) ($:der 
    shift . 202) (name shift . 203) ($:true shift . 204) ($:false shift . 205)
    (string shift . 206) (unsigned-number shift . 207) (primary shift . 208) 
    (factor shift . 209) (term shift . 210) (arithmetic-expression shift . 211
    ) ($:not shift . 212) (relation shift . 213) (logical-factor shift . 214) 
    (logical-term shift . 215) (logical-expression shift . 216) ($:if shift . 
    217) (simple-expression shift . 218) (expression shift . 371) ($:function 
    shift . 372) (function-argument shift . 524)) (($ident shift . 26) 
    (ident shift . 521) (named-argument shift . 522)) (($ident shift . 26) 
    ($string shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 
    200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) 
    ($:true shift . 204) ($:false shift . 205) (string shift . 206) (
    unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 371) ($:function shift 
    . 372) (function-argument shift . 520)) (($:. shift . 59) (#{$:\x28;}# 
    shift . 519)) (($:= reduce . 300) ($:: reduce . 300) ($:^ reduce . 300) 
    ($:.^ reduce . 300) ($:./ reduce . 300) ($:.* reduce . 300) ($:/ reduce . 
    300) ($:* reduce . 300) ($:+ reduce . 300) ($:- reduce . 300) ($:.+ reduce
    . 300) ($:.- reduce . 300) ($:<> reduce . 300) ($:== reduce . 300) 
    ($:>= reduce . 300) ($:> reduce . 300) ($:<= reduce . 300) ($:< reduce . 
    300) ($:and reduce . 300) ($:or reduce . 300) (#{$:\x5d;}# reduce . 300) 
    ($:, reduce . 300) (#{$:\x7d;}# reduce . 300) (#{$:;}# reduce . 300) 
    (#{$:\x29;}# reduce . 300) ($:then reduce . 300) ($:loop reduce . 300) 
    ($:else reduce . 300) ($:elseif reduce . 300) ($string reduce . 300) 
    ($:annotation reduce . 300) ($:constrainedby reduce . 300) ($:end reduce 
    . 300) ($:elsewhen reduce . 300) ($:if reduce . 300)) (($ident shift . 26)
    ($string shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 
    200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) 
    ($:true shift . 204) ($:false shift . 205) (string shift . 206) (
    unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 368) (expression-list 
    shift . 518)) (($ident shift . 26) ($string shift . 50) ($float shift . 
    197) ($fixed shift . 198) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}#
    shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 201) ($:der 
    shift . 202) (name shift . 203) ($:true shift . 204) ($:false shift . 205)
    (string shift . 206) (unsigned-number shift . 207) (primary shift . 208) 
    (factor shift . 209) (term shift . 210) (arithmetic-expression shift . 211
    ) ($:not shift . 212) (relation shift . 213) (logical-factor shift . 214) 
    (logical-term shift . 215) (logical-expression shift . 216) ($:if shift . 
    217) (simple-expression shift . 218) (expression shift . 517)) (($:= 
    reduce . 299) ($:: reduce . 299) ($:^ reduce . 299) ($:.^ reduce . 299) 
    ($:./ reduce . 299) ($:.* reduce . 299) ($:/ reduce . 299) ($:* reduce . 
    299) ($:+ reduce . 299) ($:- reduce . 299) ($:.+ reduce . 299) ($:.- 
    reduce . 299) ($:<> reduce . 299) ($:== reduce . 299) ($:>= reduce . 299) 
    ($:> reduce . 299) ($:<= reduce . 299) ($:< reduce . 299) ($:and reduce . 
    299) ($:or reduce . 299) (#{$:\x5d;}# reduce . 299) ($:, reduce . 299) 
    (#{$:\x7d;}# reduce . 299) (#{$:;}# reduce . 299) (#{$:\x29;}# reduce . 
    299) ($:then reduce . 299) ($:loop reduce . 299) ($:else reduce . 299) 
    ($:elseif reduce . 299) ($string reduce . 299) ($:annotation reduce . 299)
    ($:constrainedby reduce . 299) ($:end reduce . 299) ($:elsewhen reduce . 
    299) ($:if reduce . 299)) (($ident shift . 26) ($string shift . 50) 
    ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident shift . 
    28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift
    . 201) ($:der shift . 202) (name shift . 203) ($:true shift . 204) 
    ($:false shift . 205) (string shift . 206) (unsigned-number shift . 207) 
    (primary shift . 208) (factor shift . 209) (term shift . 210) (
    arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 216) ($:if shift . 217) (simple-expression 
    shift . 218) (expression shift . 516)) (($:: reduce . 290) ($:= reduce . 
    290) ($:or reduce . 290) ($:and reduce . 290) ($:< reduce . 290) ($:<= 
    reduce . 290) ($:> reduce . 290) ($:>= reduce . 290) ($:== reduce . 290) 
    ($:<> reduce . 290) ($:.- reduce . 290) ($:.+ reduce . 290) ($:- reduce . 
    290) ($:+ reduce . 290) ($:* reduce . 290) ($:/ reduce . 290) ($:.* reduce
    . 290) ($:./ reduce . 290) ($:.^ reduce . 290) ($:^ reduce . 290) (
    #{$:\x5d;}# reduce . 290) ($:, reduce . 290) (#{$:\x7d;}# reduce . 290) 
    (#{$:;}# reduce . 290) (#{$:\x29;}# reduce . 290) ($:then reduce . 290) 
    ($:loop reduce . 290) ($:else reduce . 290) ($:elseif reduce . 290) 
    ($string reduce . 290) ($:annotation reduce . 290) ($:constrainedby reduce
    . 290) ($:end reduce . 290) ($:elsewhen reduce . 290) ($:if reduce . 290))
    (($:: reduce . 289) ($:= reduce . 289) ($:or reduce . 289) ($:and reduce 
    . 289) ($:< reduce . 289) ($:<= reduce . 289) ($:> reduce . 289) ($:>= 
    reduce . 289) ($:== reduce . 289) ($:<> reduce . 289) ($:.- reduce . 289) 
    ($:.+ reduce . 289) ($:- reduce . 289) ($:+ reduce . 289) ($:* reduce . 
    289) ($:/ reduce . 289) ($:.* reduce . 289) ($:./ reduce . 289) ($:.^ 
    reduce . 289) ($:^ reduce . 289) (#{$:\x5d;}# reduce . 289) ($:, reduce . 
    289) (#{$:\x7d;}# reduce . 289) (#{$:;}# reduce . 289) (#{$:\x29;}# reduce
    . 289) ($:then reduce . 289) ($:loop reduce . 289) ($:else reduce . 289) 
    ($:elseif reduce . 289) ($string reduce . 289) ($:annotation reduce . 289)
    ($:constrainedby reduce . 289) ($:end reduce . 289) ($:elsewhen reduce . 
    289) ($:if reduce . 289)) (($:^ shift . 363) ($:.^ shift . 364) ($:= 
    reduce . 283) ($:: reduce . 283) ($:./ reduce . 283) ($:.* reduce . 283) 
    ($:/ reduce . 283) ($:* reduce . 283) ($:+ reduce . 283) ($:- reduce . 283
    ) ($:.+ reduce . 283) ($:.- reduce . 283) ($:<> reduce . 283) ($:== reduce
    . 283) ($:>= reduce . 283) ($:> reduce . 283) ($:<= reduce . 283) ($:< 
    reduce . 283) ($:and reduce . 283) ($:or reduce . 283) (#{$:\x5d;}# reduce
    . 283) ($:, reduce . 283) (#{$:\x7d;}# reduce . 283) (#{$:;}# reduce . 283
    ) (#{$:\x29;}# reduce . 283) ($:then reduce . 283) ($:loop reduce . 283) 
    ($:else reduce . 283) ($:elseif reduce . 283) ($string reduce . 283) 
    ($:annotation reduce . 283) ($:constrainedby reduce . 283) ($:end reduce 
    . 283) ($:elsewhen reduce . 283) ($:if reduce . 283)) (($:./ shift . 358) 
    ($:.* shift . 359) ($:/ shift . 360) ($:* shift . 361) (mul-op shift . 362
    ) ($:: reduce . 277) ($:= reduce . 277) ($:or reduce . 277) ($:and reduce 
    . 277) ($:< reduce . 277) ($:<= reduce . 277) ($:> reduce . 277) ($:>= 
    reduce . 277) ($:== reduce . 277) ($:<> reduce . 277) ($:.- reduce . 277) 
    ($:.+ reduce . 277) ($:- reduce . 277) ($:+ reduce . 277) (#{$:\x5d;}# 
    reduce . 277) ($:, reduce . 277) (#{$:\x7d;}# reduce . 277) (#{$:;}# 
    reduce . 277) (#{$:\x29;}# reduce . 277) ($:then reduce . 277) ($:loop 
    reduce . 277) ($:else reduce . 277) ($:elseif reduce . 277) ($string 
    reduce . 277) ($:annotation reduce . 277) ($:constrainedby reduce . 277) 
    ($:end reduce . 277) ($:elsewhen reduce . 277) ($:if reduce . 277)) 
    (($:.- shift . 353) ($:.+ shift . 354) ($:- shift . 355) ($:+ shift . 356)
    (add-op shift . 357) ($:= reduce . 269) ($:: reduce . 269) ($:<> reduce . 
    269) ($:== reduce . 269) ($:>= reduce . 269) ($:> reduce . 269) ($:<= 
    reduce . 269) ($:< reduce . 269) ($:and reduce . 269) ($:or reduce . 269) 
    (#{$:\x5d;}# reduce . 269) ($:, reduce . 269) (#{$:\x7d;}# reduce . 269) 
    (#{$:;}# reduce . 269) (#{$:\x29;}# reduce . 269) ($:then reduce . 269) 
    ($:loop reduce . 269) ($:else reduce . 269) ($:elseif reduce . 269) 
    ($string reduce . 269) ($:annotation reduce . 269) ($:constrainedby reduce
    . 269) ($:end reduce . 269) ($:elsewhen reduce . 269) ($:if reduce . 269))
    (($:= reduce . 265) ($:: reduce . 265) ($:and reduce . 265) ($:or reduce 
    . 265) (#{$:\x5d;}# reduce . 265) ($:, reduce . 265) (#{$:\x7d;}# reduce 
    . 265) (#{$:;}# reduce . 265) (#{$:\x29;}# reduce . 265) ($:then reduce . 
    265) ($:loop reduce . 265) ($:else reduce . 265) ($:elseif reduce . 265) 
    ($string reduce . 265) ($:annotation reduce . 265) ($:constrainedby reduce
    . 265) ($:end reduce . 265) ($:elsewhen reduce . 265) ($:if reduce . 265))
    (($:and shift . 344) ($:: reduce . 263) ($:= reduce . 263) ($:or reduce . 
    263) (#{$:\x5d;}# reduce . 263) ($:, reduce . 263) (#{$:\x7d;}# reduce . 
    263) (#{$:;}# reduce . 263) (#{$:\x29;}# reduce . 263) ($:then reduce . 
    263) ($:loop reduce . 263) ($:else reduce . 263) ($:elseif reduce . 263) 
    ($string reduce . 263) ($:annotation reduce . 263) ($:constrainedby reduce
    . 263) ($:end reduce . 263) ($:elsewhen reduce . 263) ($:if reduce . 263))
    (($:: shift . 515) ($:or shift . 343) ($:= reduce . 261) (#{$:\x5d;}# 
    reduce . 261) ($:, reduce . 261) (#{$:\x7d;}# reduce . 261) (#{$:;}# 
    reduce . 261) (#{$:\x29;}# reduce . 261) ($:then reduce . 261) ($:loop 
    reduce . 261) ($:else reduce . 261) ($:elseif reduce . 261) ($string 
    reduce . 261) ($:annotation reduce . 261) ($:constrainedby reduce . 261) 
    ($:end reduce . 261) ($:elsewhen reduce . 261) ($:if reduce . 261)) 
    (($ident shift . 26) ($string shift . 50) ($float shift . 197) ($fixed 
    shift . 198) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199)
    (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) 
    (name shift . 203) ($:true shift . 204) ($:false shift . 205) (string 
    shift . 206) (unsigned-number shift . 207) (primary shift . 208) (factor 
    shift . 209) (term shift . 210) (arithmetic-expression shift . 211) 
    ($:not shift . 212) (relation shift . 213) (logical-factor shift . 214) 
    (logical-term shift . 215) (logical-expression shift . 216) ($:if shift . 
    217) (simple-expression shift . 218) (expression shift . 514)) ((
    #{$:\x5d;}# reduce . 338) ($:, reduce . 338)) (($string reduce . 109) 
    ($:annotation reduce . 109) (#{$:;}# reduce . 109)) ((#{$:\x7d;}# reduce 
    . 111) ($:, reduce . 111)) ((#{$:\x7d;}# shift . 512) ($:, shift . 513)) 
    (($string reduce . 117) ($:annotation reduce . 117) (#{$:;}# reduce . 117)
    (#{$:\x29;}# reduce . 117) ($:, reduce . 117)) ((#{$:;}# reduce . 81)) 
    ((#{$:;}# shift . 511)) (($:annotation reduce . 64) ($:end reduce . 64)) 
    (($:annotation reduce . 65) ($:end reduce . 65)) (($:. reduce . 316) 
    ($::= reduce . 316) (#{$:\x28;}# reduce . 316) ($:= reduce . 316) ($:, 
    reduce . 316) (#{$:\x29;}# reduce . 316)) (($:. reduce . 312) ($::= reduce
    . 312) (#{$:\x28;}# reduce . 312) ($:= reduce . 312) ($:, reduce . 312) 
    (#{$:\x29;}# reduce . 312)) ((#{$:\x5b;}# shift . 137) (array-subscripts 
    shift . 509) ($P12 shift . 510) (#{$:\x29;}# reduce . 309) ($:, reduce . 
    309) ($:= reduce . 309) ($::= reduce . 309) (#{$:\x28;}# reduce . 309)) 
    ((#{$:;}# reduce . 75) ($:annotation reduce . 75)) ((#{$:\x29;}# shift . 
    508) ($:, shift . 429)) ((#{$:\x28;}# shift . 507)) (($:annotation reduce 
    . 66) ($:end reduce . 66)) (($:, shift . 506)) (($string shift . 50) 
    ($float shift . 197) ($fixed shift . 198) (#{$:\x7b;}# shift . 199) 
    (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) 
    ($:true shift . 204) ($:false shift . 205) (string shift . 206) (
    unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) ($ident shift . 26) 
    (logical-term shift . 215) ($:. shift . 27) (ident shift . 28) (
    logical-expression shift . 216) ($:connect shift . 257) ($:when shift . 
    258) ($:for shift . 259) ($:if shift . 260) (name shift . 261) (
    when-equation shift . 262) (connect-clause shift . 263) (for-equation 
    shift . 264) (if-equation shift . 265) (simple-expression shift . 266) 
    (equation-1 shift . 267) (equation shift . 268) (equation-list-1 shift . 
    269) (equation-list shift . 505) ($:elseif reduce . 214) ($:else reduce . 
    214) ($:end reduce . 214) ($:elsewhen reduce . 214)) (($:elsewhen shift . 
    502) (elsewhen-eq-part shift . 503) (elsewhen-eq-list shift . 504)) 
    (($ident shift . 26) (ident shift . 298) (for-index shift . 501)) (
    ($string shift . 50) ($float shift . 197) ($fixed shift . 198) (
    #{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 
    201) ($:der shift . 202) ($:true shift . 204) ($:false shift . 205) 
    (string shift . 206) (unsigned-number shift . 207) (primary shift . 208) 
    (factor shift . 209) (term shift . 210) (arithmetic-expression shift . 211
    ) ($:not shift . 212) (relation shift . 213) (logical-factor shift . 214) 
    ($ident shift . 26) (logical-term shift . 215) ($:. shift . 27) (ident 
    shift . 28) (logical-expression shift . 216) ($:connect shift . 257) 
    ($:when shift . 258) ($:for shift . 259) ($:if shift . 260) (name shift . 
    261) (when-equation shift . 262) (connect-clause shift . 263) (
    for-equation shift . 264) (if-equation shift . 265) (simple-expression 
    shift . 266) (equation-1 shift . 267) (equation shift . 268) (
    equation-list-1 shift . 269) (equation-list shift . 499) ($:end shift . 
    500)) (($:elseif shift . 493) (elseif-eq-part shift . 494) (elseif-eq-list
    shift . 495) ($:else shift . 496) (else-eq-part shift . 497) ($:end shift 
    . 498)) ((#{$:;}# reduce . 191) ($string reduce . 191) ($:annotation 
    reduce . 191)) (($float reduce . 189) ($fixed reduce . 189) ($string 
    reduce . 189) (#{$:\x7b;}# reduce . 189) (#{$:\x5b;}# reduce . 189) 
    (#{$:\x28;}# reduce . 189) ($:der reduce . 189) ($:true reduce . 189) 
    ($:false reduce . 189) ($:not reduce . 189) ($:if reduce . 189) ($:for 
    reduce . 189) ($:connect reduce . 189) ($:when reduce . 189) ($ident 
    reduce . 189) ($:. reduce . 189) ($:end reduce . 189) ($:annotation reduce
    . 189) ($:public reduce . 189) ($:protected reduce . 189) ($:initial 
    reduce . 189) ($:algorithm reduce . 189) ($:equation reduce . 189) 
    ($:external reduce . 189) ($:elseif reduce . 189) ($:else reduce . 189) 
    ($:elsewhen reduce . 189)) (($ident shift . 26) ($:. shift . 243) (ident 
    shift . 271) (component-reference-1 shift . 244) ($:when shift . 272) 
    ($:while shift . 273) ($:for shift . 274) ($:if shift . 275) (
    when-statement shift . 276) (while-statement shift . 277) (for-statement 
    shift . 278) (if-statement shift . 279) ($:return shift . 280) ($:break 
    shift . 281) (#{$:\x28;}# shift . 282) (component-reference shift . 283) 
    (statement-1 shift . 284) (statement shift . 285) (statement-list shift . 
    492) ($:elseif reduce . 226) ($:else reduce . 226) ($:end reduce . 226) 
    ($:elsewhen reduce . 226)) (($:elsewhen shift . 489) (elsewhen-st-part 
    shift . 490) (elsewhen-st-list shift . 491)) (($ident shift . 26) ($:. 
    shift . 243) (ident shift . 271) (component-reference-1 shift . 244) 
    ($:when shift . 272) ($:while shift . 273) ($:for shift . 274) ($:if shift
    . 275) (when-statement shift . 276) (while-statement shift . 277) (
    for-statement shift . 278) (if-statement shift . 279) ($:return shift . 
    280) ($:break shift . 281) (#{$:\x28;}# shift . 282) (component-reference 
    shift . 283) (statement-1 shift . 284) (statement shift . 285) (
    statement-list shift . 487) ($:end shift . 488)) (($ident shift . 26) 
    ($:. shift . 243) (ident shift . 271) (component-reference-1 shift . 244) 
    ($:when shift . 272) ($:while shift . 273) ($:for shift . 274) ($:if shift
    . 275) (when-statement shift . 276) (while-statement shift . 277) (
    for-statement shift . 278) (if-statement shift . 279) ($:return shift . 
    280) ($:break shift . 281) (#{$:\x28;}# shift . 282) (component-reference 
    shift . 283) (statement-1 shift . 284) (statement shift . 285) (
    statement-list shift . 485) ($:end shift . 486)) (($ident shift . 26) 
    ($string shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 
    200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) 
    ($:true shift . 204) ($:false shift . 205) (string shift . 206) (
    unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 484)) (($:elseif shift 
    . 478) (elseif-st-part shift . 479) (elseif-st-list shift . 480) ($:else 
    shift . 481) (else-st-part shift . 482) ($:end shift . 483)) (($::= shift 
    . 477)) ((#{$:\x29;}# shift . 476)) (($:= reduce . 318) ($:: reduce . 318)
    ($:^ reduce . 318) ($:.^ reduce . 318) ($:./ reduce . 318) ($:.* reduce . 
    318) ($:/ reduce . 318) ($:* reduce . 318) ($:+ reduce . 318) ($:- reduce 
    . 318) ($:.+ reduce . 318) ($:.- reduce . 318) ($:<> reduce . 318) 
    ($:== reduce . 318) ($:>= reduce . 318) ($:> reduce . 318) ($:<= reduce . 
    318) ($:< reduce . 318) ($:and reduce . 318) ($:or reduce . 318) (
    #{$:\x5d;}# reduce . 318) ($:, reduce . 318) (#{$:\x7d;}# reduce . 318) 
    (#{$:;}# reduce . 318) (#{$:\x29;}# reduce . 318) ($:then reduce . 318) 
    ($:loop reduce . 318) ($:else reduce . 318) ($:elseif reduce . 318) 
    ($string reduce . 318) ($:annotation reduce . 318) ($:constrainedby reduce
    . 318) ($:end reduce . 318) ($:elsewhen reduce . 318) ($:if reduce . 318))
    ((#{$:;}# reduce . 200) ($string reduce . 200) ($:annotation reduce . 200)
    ) (($ident reduce . 198) ($:. reduce . 198) ($:if reduce . 198) ($:for 
    reduce . 198) ($:while reduce . 198) ($:when reduce . 198) ($:return 
    reduce . 198) ($:break reduce . 198) (#{$:\x28;}# reduce . 198) (
    $:annotation reduce . 198) ($:public reduce . 198) ($:protected reduce . 
    198) ($:initial reduce . 198) ($:algorithm reduce . 198) ($:equation 
    reduce . 198) ($:external reduce . 198) ($:end reduce . 198) ($:elseif 
    reduce . 198) ($:else reduce . 198) ($:elsewhen reduce . 198)) (($:= 
    reduce . 317) ($:: reduce . 317) ($:^ reduce . 317) ($:.^ reduce . 317) 
    ($:./ reduce . 317) ($:.* reduce . 317) ($:/ reduce . 317) ($:* reduce . 
    317) ($:+ reduce . 317) ($:- reduce . 317) ($:.+ reduce . 317) ($:.- 
    reduce . 317) ($:<> reduce . 317) ($:== reduce . 317) ($:>= reduce . 317) 
    ($:> reduce . 317) ($:<= reduce . 317) ($:< reduce . 317) ($:and reduce . 
    317) ($:or reduce . 317) (#{$:\x5d;}# reduce . 317) ($:, reduce . 317) 
    (#{$:\x7d;}# reduce . 317) (#{$:;}# reduce . 317) (#{$:\x29;}# reduce . 
    317) ($:then reduce . 317) ($:loop reduce . 317) ($:else reduce . 317) 
    ($:elseif reduce . 317) ($string reduce . 317) ($:annotation reduce . 317)
    ($:constrainedby reduce . 317) ($:end reduce . 317) ($:elsewhen reduce . 
    317) ($:if reduce . 317)) (($ident shift . 26) ($:. shift . 243) (ident 
    shift . 271) (component-reference-1 shift . 244) (component-reference 
    shift . 569)) (($ident shift . 26) ($:. shift . 243) (ident shift . 271) 
    (component-reference-1 shift . 244) ($:when shift . 272) ($:while shift . 
    273) ($:for shift . 274) ($:if shift . 275) (when-statement shift . 276) 
    (while-statement shift . 277) (for-statement shift . 278) (if-statement 
    shift . 279) ($:return shift . 280) ($:break shift . 281) (#{$:\x28;}# 
    shift . 282) (component-reference shift . 283) (statement-1 shift . 284) 
    (statement shift . 285) (statement-list shift . 568) ($:end reduce . 230) 
    ($:elseif reduce . 230) ($:else reduce . 230)) (($:end reduce . 227) 
    ($:elseif reduce . 227) ($:else reduce . 227)) (($:end shift . 565) 
    ($:elseif shift . 478) (elseif-st-part shift . 566) ($:else shift . 481) 
    (else-st-part shift . 567)) (($ident shift . 26) ($:. shift . 243) 
    (ident shift . 271) (component-reference-1 shift . 244) ($:when shift . 
    272) ($:while shift . 273) ($:for shift . 274) ($:if shift . 275) (
    when-statement shift . 276) (while-statement shift . 277) (for-statement 
    shift . 278) (if-statement shift . 279) ($:return shift . 280) ($:break 
    shift . 281) (#{$:\x28;}# shift . 282) (component-reference shift . 283) 
    (statement-1 shift . 284) (statement shift . 285) (statement-list shift . 
    564) ($:end reduce . 232)) (($:end shift . 563)) (($:if shift . 562)) 
    (($:loop reduce . 239) ($:, reduce . 239)) (($:end shift . 561) ($ident 
    shift . 26) ($:. shift . 243) (ident shift . 271) (component-reference-1 
    shift . 244) ($:when shift . 272) ($:while shift . 273) ($:for shift . 274
    ) ($:if shift . 275) (when-statement shift . 276) (while-statement shift 
    . 277) (for-statement shift . 278) (if-statement shift . 279) ($:return 
    shift . 280) ($:break shift . 281) (#{$:\x28;}# shift . 282) (
    component-reference shift . 283) (statement-1 shift . 284) (statement 
    shift . 288)) (($:for shift . 560)) (($:end shift . 559) ($ident shift . 
    26) ($:. shift . 243) (ident shift . 271) (component-reference-1 shift . 
    244) ($:when shift . 272) ($:while shift . 273) ($:for shift . 274) 
    ($:if shift . 275) (when-statement shift . 276) (while-statement shift . 
    277) (for-statement shift . 278) (if-statement shift . 279) ($:return 
    shift . 280) ($:break shift . 281) (#{$:\x28;}# shift . 282) (
    component-reference shift . 283) (statement-1 shift . 284) (statement 
    shift . 288)) (($:while shift . 558)) (($ident shift . 26) ($string shift 
    . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident 
    shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (
    #{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) ($:true 
    shift . 204) ($:false shift . 205) (string shift . 206) (unsigned-number 
    shift . 207) (primary shift . 208) (factor shift . 209) (term shift . 210)
    (arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 216) ($:if shift . 217) (simple-expression 
    shift . 218) (expression shift . 557)) (($:end reduce . 249) ($:elsewhen 
    reduce . 249)) (($:end shift . 555) ($:elsewhen shift . 489) (
    elsewhen-st-part shift . 556)) (($ident shift . 26) ($:. shift . 243) 
    (ident shift . 271) (component-reference-1 shift . 244) ($:when shift . 
    272) ($:while shift . 273) ($:for shift . 274) ($:if shift . 275) (
    when-statement shift . 276) (while-statement shift . 277) (for-statement 
    shift . 278) (if-statement shift . 279) ($:return shift . 280) ($:break 
    shift . 281) (#{$:\x28;}# shift . 282) (component-reference shift . 283) 
    (statement-1 shift . 284) (statement shift . 288) ($:elseif reduce . 225) 
    ($:else reduce . 225) ($:end reduce . 225) ($:elsewhen reduce . 225)) 
    (($string shift . 50) ($float shift . 197) ($fixed shift . 198) (
    #{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 
    201) ($:der shift . 202) ($:true shift . 204) ($:false shift . 205) 
    (string shift . 206) (unsigned-number shift . 207) (primary shift . 208) 
    (factor shift . 209) (term shift . 210) (arithmetic-expression shift . 211
    ) ($:not shift . 212) (relation shift . 213) (logical-factor shift . 214) 
    ($ident shift . 26) (logical-term shift . 215) ($:. shift . 27) (ident 
    shift . 28) (logical-expression shift . 216) ($:connect shift . 257) 
    ($:when shift . 258) ($:for shift . 259) ($:if shift . 260) (name shift . 
    261) (when-equation shift . 262) (connect-clause shift . 263) (
    for-equation shift . 264) (if-equation shift . 265) (simple-expression 
    shift . 266) (equation-1 shift . 267) (equation shift . 268) (
    equation-list-1 shift . 269) (equation-list shift . 554) ($:end reduce . 
    218) ($:elseif reduce . 218) ($:else reduce . 218)) (($:end reduce . 215) 
    ($:elseif reduce . 215) ($:else reduce . 215)) (($:end shift . 551) 
    ($:elseif shift . 493) (elseif-eq-part shift . 552) ($:else shift . 496) 
    (else-eq-part shift . 553)) (($string shift . 50) ($float shift . 197) 
    ($fixed shift . 198) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) 
    (#{$:\x28;}# shift . 201) ($:der shift . 202) ($:true shift . 204) 
    ($:false shift . 205) (string shift . 206) (unsigned-number shift . 207) 
    (primary shift . 208) (factor shift . 209) (term shift . 210) (
    arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) ($ident shift . 26) (logical-term shift 
    . 215) ($:. shift . 27) (ident shift . 28) (logical-expression shift . 216
    ) ($:connect shift . 257) ($:when shift . 258) ($:for shift . 259) 
    ($:if shift . 260) (name shift . 261) (when-equation shift . 262) (
    connect-clause shift . 263) (for-equation shift . 264) (if-equation shift 
    . 265) (simple-expression shift . 266) (equation-1 shift . 267) (equation 
    shift . 268) (equation-list-1 shift . 269) (equation-list shift . 550) 
    ($:end reduce . 220)) (($:end shift . 549)) (($:if shift . 548)) (($:end 
    shift . 547)) (($:for shift . 546)) (($:loop reduce . 238) ($:, reduce . 
    238)) (($ident shift . 26) ($string shift . 50) ($float shift . 197) 
    ($fixed shift . 198) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# 
    shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 201) ($:der 
    shift . 202) (name shift . 203) ($:true shift . 204) ($:false shift . 205)
    (string shift . 206) (unsigned-number shift . 207) (primary shift . 208) 
    (factor shift . 209) (term shift . 210) (arithmetic-expression shift . 211
    ) ($:not shift . 212) (relation shift . 213) (logical-factor shift . 214) 
    (logical-term shift . 215) (logical-expression shift . 216) ($:if shift . 
    217) (simple-expression shift . 218) (expression shift . 545)) (($:end 
    reduce . 244) ($:elsewhen reduce . 244)) (($:end shift . 543) ($:elsewhen 
    shift . 502) (elsewhen-eq-part shift . 544)) (($:elseif reduce . 213) 
    ($:else reduce . 213) ($:end reduce . 213) ($:elsewhen reduce . 213)) 
    (($ident shift . 26) ($:. shift . 243) (ident shift . 271) (
    component-reference-1 shift . 244) (component-reference shift . 542)) 
    ((#{$:\x29;}# shift . 540) ($ident shift . 26) ($string shift . 50) 
    ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident shift . 
    28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift
    . 201) ($:der shift . 202) (name shift . 203) ($:true shift . 204) 
    ($:false shift . 205) (string shift . 206) (unsigned-number shift . 207) 
    (primary shift . 208) (factor shift . 209) (term shift . 210) (
    arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 216) ($:if shift . 217) (simple-expression 
    shift . 218) (expression shift . 368) (expression-list shift . 541)) 
    ((#{$:;}# reduce . 74) ($:annotation reduce . 74)) ((#{$:\x28;}# reduce . 
    310) ($::= reduce . 310) ($:= reduce . 310) ($:, reduce . 310) (
    #{$:\x29;}# reduce . 310)) ((#{$:\x28;}# reduce . 308) ($::= reduce . 308)
    ($:= reduce . 308) ($:, reduce . 308) (#{$:\x29;}# reduce . 308)) (
    ($:annotation reduce . 63) ($:end reduce . 63)) (($string reduce . 110) 
    ($:annotation reduce . 110) (#{$:;}# reduce . 110)) (($ident shift . 26) 
    (ident shift . 539)) (($:else shift . 536) ($:elseif shift . 537) (
    elseif-ex-list shift . 538)) (($ident shift . 26) ($string shift . 50) 
    ($float shift . 197) ($fixed shift . 198) ($:. shift . 27) (ident shift . 
    28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift
    . 201) ($:der shift . 202) (name shift . 203) ($:true shift . 204) 
    ($:false shift . 205) (string shift . 206) (unsigned-number shift . 207) 
    (primary shift . 208) (factor shift . 209) (term shift . 210) (
    arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 535)) ((#{$:\x29;}# reduce . 333) ($:, reduce 
    . 333)) ((#{$:\x5d;}# reduce . 335) ($:, reduce . 335) (#{$:;}# reduce . 
    335) (#{$:\x29;}# reduce . 335) ($:end reduce . 335) ($:elsewhen reduce . 
    335)) (($:, shift . 429) (#{$:\x5d;}# reduce . 303) (#{$:;}# reduce . 303)
    ) (($string shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. 
    shift . 27) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 200) (
    #{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) ($:true 
    shift . 204) ($:false shift . 205) (string shift . 206) (unsigned-number 
    shift . 207) (primary shift . 208) (factor shift . 209) (term shift . 210)
    (arithmetic-expression shift . 211) ($:not shift . 212) (relation shift . 
    213) (logical-factor shift . 214) (logical-term shift . 215) (
    logical-expression shift . 216) ($:if shift . 217) (simple-expression 
    shift . 218) ($ident shift . 26) (expression shift . 371) ($:function 
    shift . 372) (ident shift . 373) (function-argument shift . 375) (
    function-arguments-1 shift . 531) (named-argument shift . 374) (
    named-only-arguments-1 shift . 532) (named-arguments shift . 533) (
    #{$:\x29;}# shift . 534)) ((#{$:\x7d;}# reduce . 327) ($:, reduce . 327) 
    (#{$:\x29;}# reduce . 327)) (($:= shift . 425)) ((#{$:\x7d;}# reduce . 325
    ) ($:, reduce . 325) (#{$:\x29;}# reduce . 325)) ((#{$:\x7d;}# reduce . 
    326) ($:, reduce . 326) (#{$:\x29;}# reduce . 326)) ((#{$:\x7d;}# reduce 
    . 322) ($:, reduce . 322) (#{$:\x29;}# reduce . 322)) (($string shift . 50
    ) (string shift . 51) (string-cat shift . 52) (string-comment shift . 185)
    (comment shift . 530) (#{$:;}# reduce . 343) ($:constrainedby reduce . 343
    ) ($:annotation reduce . 343)) (($ident shift . 26) (ident shift . 529)) 
    (($string shift . 50) (string shift . 51) (string-cat shift . 52) (
    string-comment shift . 185) (comment shift . 528) ($:constrainedby reduce 
    . 343) (#{$:\x29;}# reduce . 343) ($:, reduce . 343) ($:annotation reduce 
    . 343)) (($:constrainedby reduce . 177) (#{$:\x29;}# reduce . 177) 
    ($:, reduce . 177)) ((#{$:\x29;}# reduce . 44) (#{$:;}# reduce . 44)) 
    ((#{$:;}# reduce . 42) ($:constrainedby reduce . 42)) (($:, shift . 423)) 
    (($:, shift . 424) (#{$:\x29;}# reduce . 323)) ((#{$:\x29;}# shift . 590))
    ((#{$:\x7d;}# reduce . 329) ($:, reduce . 329) (#{$:\x29;}# reduce . 329))
    (($:or shift . 343) ($:= reduce . 260) (#{$:\x5d;}# reduce . 260) ($:, 
    reduce . 260) (#{$:\x7d;}# reduce . 260) (#{$:;}# reduce . 260) (
    #{$:\x29;}# reduce . 260) ($:then reduce . 260) ($:loop reduce . 260) 
    ($:else reduce . 260) ($:elseif reduce . 260) ($string reduce . 260) 
    ($:annotation reduce . 260) ($:constrainedby reduce . 260) ($:end reduce 
    . 260) ($:elsewhen reduce . 260) ($:if reduce . 260)) (($ident shift . 26)
    ($string shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 
    200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) 
    ($:true shift . 204) ($:false shift . 205) (string shift . 206) (
    unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 589)) (($ident shift . 
    26) ($string shift . 50) ($float shift . 197) ($fixed shift . 198) 
    ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}#
    shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 
    203) ($:true shift . 204) ($:false shift . 205) (string shift . 206) 
    (unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 588)) (($:else shift . 
    586) ($:elseif shift . 587)) ((#{$:\x7d;}# reduce . 112) ($:, reduce . 112
    )) ((#{$:;}# reduce . 73) ($:annotation reduce . 73)) ((#{$:\x29;}# shift 
    . 585) ($:, shift . 429)) ((#{$:\x29;}# shift . 584)) (($:when shift . 583
    )) (($:end reduce . 245) ($:elsewhen reduce . 245)) (($:then shift . 582))
    (($:annotation reduce . 234) ($string reduce . 234) (#{$:;}# reduce . 234)
    ) (($:for shift . 581)) (($:annotation reduce . 212) ($string reduce . 212
    ) (#{$:;}# reduce . 212)) (($:if shift . 580)) (($:end reduce . 219)) 
    (($:if shift . 579)) (($:end reduce . 216) ($:elseif reduce . 216) 
    ($:else reduce . 216)) (($:end shift . 578)) (($:end reduce . 217) 
    ($:elseif reduce . 217) ($:else reduce . 217)) (($:when shift . 577)) 
    (($:end reduce . 250) ($:elsewhen reduce . 250)) (($:then shift . 576)) 
    (($:annotation reduce . 242) ($string reduce . 242) (#{$:;}# reduce . 242)
    ) (($:while shift . 575)) (($:annotation reduce . 236) ($string reduce . 
    236) (#{$:;}# reduce . 236)) (($:for shift . 574)) (($:annotation reduce 
    . 224) ($string reduce . 224) (#{$:;}# reduce . 224)) (($:if shift . 573))
    (($ident shift . 26) ($:. shift . 243) (ident shift . 271) (
    component-reference-1 shift . 244) ($:when shift . 272) ($:while shift . 
    273) ($:for shift . 274) ($:if shift . 275) (when-statement shift . 276) 
    (while-statement shift . 277) (for-statement shift . 278) (if-statement 
    shift . 279) ($:return shift . 280) ($:break shift . 281) (#{$:\x28;}# 
    shift . 282) (component-reference shift . 283) (statement-1 shift . 284) 
    (statement shift . 288) ($:end reduce . 231)) (($:if shift . 572)) 
    (($:end reduce . 228) ($:elseif reduce . 228) ($:else reduce . 228)) 
    (($:end shift . 571)) (($ident shift . 26) ($:. shift . 243) (ident shift 
    . 271) (component-reference-1 shift . 244) ($:when shift . 272) ($:while 
    shift . 273) ($:for shift . 274) ($:if shift . 275) (when-statement shift 
    . 276) (while-statement shift . 277) (for-statement shift . 278) (
    if-statement shift . 279) ($:return shift . 280) ($:break shift . 281) 
    (#{$:\x28;}# shift . 282) (component-reference shift . 283) (statement-1 
    shift . 284) (statement shift . 288) ($:end reduce . 229) ($:elseif reduce
    . 229) ($:else reduce . 229)) ((#{$:\x28;}# shift . 292) (
    function-call-args shift . 570)) ((#{$:;}# reduce . 202) ($string reduce 
    . 202) ($:annotation reduce . 202)) (($:if shift . 597)) (($:annotation 
    reduce . 222) ($string reduce . 222) (#{$:;}# reduce . 222)) ((
    $:annotation reduce . 223) ($string reduce . 223) (#{$:;}# reduce . 223)) 
    (($:annotation reduce . 235) ($string reduce . 235) (#{$:;}# reduce . 235)
    ) (($:annotation reduce . 241) ($string reduce . 241) (#{$:;}# reduce . 
    241)) (($ident shift . 26) ($:. shift . 243) (ident shift . 271) (
    component-reference-1 shift . 244) ($:when shift . 272) ($:while shift . 
    273) ($:for shift . 274) ($:if shift . 275) (when-statement shift . 276) 
    (while-statement shift . 277) (for-statement shift . 278) (if-statement 
    shift . 279) ($:return shift . 280) ($:break shift . 281) (#{$:\x28;}# 
    shift . 282) (component-reference shift . 283) (statement-1 shift . 284) 
    (statement shift . 285) (statement-list shift . 596) ($:end reduce . 251) 
    ($:elsewhen reduce . 251)) (($:annotation reduce . 248) ($string reduce . 
    248) (#{$:;}# reduce . 248)) (($:if shift . 595)) (($:annotation reduce . 
    210) ($string reduce . 210) (#{$:;}# reduce . 210)) (($:annotation reduce 
    . 211) ($string reduce . 211) (#{$:;}# reduce . 211)) (($:annotation 
    reduce . 233) ($string reduce . 233) (#{$:;}# reduce . 233)) (($ident 
    shift . 26) ($string shift . 50) ($float shift . 197) ($fixed shift . 198)
    ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}#
    shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 
    203) ($:true shift . 204) ($:false shift . 205) (string shift . 206) 
    (unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 368) (expression-list 
    shift . 594) ($:end reduce . 246) ($:elsewhen reduce . 246)) ((
    $:annotation reduce . 243) ($string reduce . 243) (#{$:;}# reduce . 243)) 
    (($:annotation reduce . 253) ($string reduce . 253) (#{$:;}# reduce . 253)
    ) ((#{$:;}# reduce . 72) ($:annotation reduce . 72)) (($ident shift . 26) 
    ($string shift . 50) ($float shift . 197) ($fixed shift . 198) ($:. shift 
    . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}# shift . 
    200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 203) 
    ($:true shift . 204) ($:false shift . 205) (string shift . 206) (
    unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 593)) (($ident shift . 
    26) ($string shift . 50) ($float shift . 197) ($fixed shift . 198) 
    ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199) (#{$:\x5b;}#
    shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) (name shift . 
    203) ($:true shift . 204) ($:false shift . 205) (string shift . 206) 
    (unsigned-number shift . 207) (primary shift . 208) (factor shift . 209) 
    (term shift . 210) (arithmetic-expression shift . 211) ($:not shift . 212)
    (relation shift . 213) (logical-factor shift . 214) (logical-term shift . 
    215) (logical-expression shift . 216) ($:if shift . 217) (
    simple-expression shift . 218) (expression shift . 592)) (($:then shift . 
    591)) ((#{$:\x5d;}# reduce . 256) ($:, reduce . 256) (#{$:\x7d;}# reduce 
    . 256) (#{$:;}# reduce . 256) (#{$:\x29;}# reduce . 256) ($:then reduce . 
    256) ($:loop reduce . 256) ($:else reduce . 256) ($:elseif reduce . 256) 
    ($string reduce . 256) ($:annotation reduce . 256) ($:constrainedby reduce
    . 256) ($:end reduce . 256) ($:elsewhen reduce . 256) ($:if reduce . 256))
    ((#{$:\x7d;}# reduce . 328) ($:, reduce . 328) (#{$:\x29;}# reduce . 328))
    (($ident shift . 26) ($string shift . 50) ($float shift . 197) ($fixed 
    shift . 198) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199)
    (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) 
    (name shift . 203) ($:true shift . 204) ($:false shift . 205) (string 
    shift . 206) (unsigned-number shift . 207) (primary shift . 208) (factor 
    shift . 209) (term shift . 210) (arithmetic-expression shift . 211) 
    ($:not shift . 212) (relation shift . 213) (logical-factor shift . 214) 
    (logical-term shift . 215) (logical-expression shift . 216) ($:if shift . 
    217) (simple-expression shift . 218) (expression shift . 599)) (($:then 
    shift . 598)) ((#{$:\x5d;}# reduce . 255) ($:, reduce . 255) (#{$:\x7d;}# 
    reduce . 255) (#{$:;}# reduce . 255) (#{$:\x29;}# reduce . 255) ($:then 
    reduce . 255) ($:loop reduce . 255) ($:else reduce . 255) ($:elseif reduce
    . 255) ($string reduce . 255) ($:annotation reduce . 255) ($:constrainedby
    reduce . 255) ($:end reduce . 255) ($:elsewhen reduce . 255) ($:if reduce 
    . 255)) (($:, shift . 429) ($:end reduce . 247) ($:elsewhen reduce . 247))
    (($:annotation reduce . 209) ($string reduce . 209) (#{$:;}# reduce . 209)
    ) (($ident shift . 26) ($:. shift . 243) (ident shift . 271) (
    component-reference-1 shift . 244) ($:when shift . 272) ($:while shift . 
    273) ($:for shift . 274) ($:if shift . 275) (when-statement shift . 276) 
    (while-statement shift . 277) (for-statement shift . 278) (if-statement 
    shift . 279) ($:return shift . 280) ($:break shift . 281) (#{$:\x28;}# 
    shift . 282) (component-reference shift . 283) (statement-1 shift . 284) 
    (statement shift . 288) ($:end reduce . 252) ($:elsewhen reduce . 252)) 
    (($:annotation reduce . 221) ($string reduce . 221) (#{$:;}# reduce . 221)
    ) (($ident shift . 26) ($string shift . 50) ($float shift . 197) ($fixed 
    shift . 198) ($:. shift . 27) (ident shift . 28) (#{$:\x7b;}# shift . 199)
    (#{$:\x5b;}# shift . 200) (#{$:\x28;}# shift . 201) ($:der shift . 202) 
    (name shift . 203) ($:true shift . 204) ($:false shift . 205) (string 
    shift . 206) (unsigned-number shift . 207) (primary shift . 208) (factor 
    shift . 209) (term shift . 210) (arithmetic-expression shift . 211) 
    ($:not shift . 212) (relation shift . 213) (logical-factor shift . 214) 
    (logical-term shift . 215) (logical-expression shift . 216) ($:if shift . 
    217) (simple-expression shift . 218) (expression shift . 600)) (($:else 
    reduce . 257) ($:elseif reduce . 257)) (($:else reduce . 258) ($:elseif 
    reduce . 258))))

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
    short-class-specifier short-class-specifier der-class-specifier 
    der-class-specifier-1 der-class-specifier-1 base-prefix base-prefix 
    enum-list enum-list enumeration-literal composition composition 
    composition composition composition-1-list composition-1-list-1 
    composition-1-list-1 composition-1 composition-1 composition-1 
    composition-1 composition-1 composition-1 external-part external-part 
    external-part external-part external-part external-part external-part 
    external-part language-specification external-function-call 
    external-function-call external-function-call external-function-call 
    element-list element-list-1 element-list-1 element element element element
    element element element $P1 $P1 $P2 $P2 $P3 $P3 $P4 $P4 $P5 $P5 $P6 $P6 
    element-1 element-1 element-1 element-1 element-2 element-2 import-clause 
    import-clause-1 import-clause-1 import-clause-1 import-clause-2 
    import-clause-2 import-clause-2 import-list import-list extends-clause 
    extends-clause extends-clause extends-clause constraining-clause 
    constraining-clause component-clause component-clause component-clause 
    component-clause type-prefix type-prefix type-prefix type-prefix 
    type-prefix type-prefix type-prefix type-prefix-1 type-prefix-1 
    type-prefix-2 type-prefix-2 type-prefix-2 type-prefix-3 type-prefix-3 
    type-specifier component-list component-list component-declaration 
    component-declaration condition-attribute declaration $P7 $P7 $P8 $P8 
    modification modification modification modification class-modification 
    class-modification argument-list argument-list argument argument 
    element-modification-or-replaceable element-modification-or-replaceable 
    element-modification-or-replaceable element-modification-or-replaceable 
    elt-mod-or-repl-1 elt-mod-or-repl-1 element-modification $P9 $P9 
    element-redeclaration $P10 $P10 $P11 $P11 elt-redecl-1 elt-redecl-1 
    elt-redecl-1 element-replaceable element-replaceable component-clause1 
    short-class-definition equation-section equation-section equation-section 
    equation-section algorithm-section algorithm-section algorithm-section 
    algorithm-section equation-list equation-list-1 equation-list-1 equation 
    equation-1 equation-1 equation-1 equation-1 equation-1 equation-1 
    statement-list statement-list statement statement-1 statement-1 
    statement-1 statement-1 statement-1 statement-1 statement-1 statement-1 
    statement-1 if-equation if-equation if-equation if-equation then-eq-part 
    then-eq-part elseif-eq-list elseif-eq-list elseif-eq-part elseif-eq-part 
    else-eq-part else-eq-part if-statement if-statement if-statement 
    if-statement then-st-part then-st-part elseif-st-list elseif-st-list 
    elseif-st-part elseif-st-part else-st-part else-st-part for-equation 
    for-equation for-statement for-statement for-indices for-indices for-index
    for-index while-statement while-statement when-equation elsewhen-eq-list 
    elsewhen-eq-list elsewhen-eq-part elsewhen-eq-part when-statement 
    elsewhen-st-list elsewhen-st-list elsewhen-st-part elsewhen-st-part 
    connect-clause expression expression expression elseif-ex-list 
    elseif-ex-list simple-expression simple-expression simple-expression 
    logical-expression logical-expression logical-term logical-term 
    logical-factor logical-factor relation relation rel-op rel-op rel-op 
    rel-op rel-op rel-op arithmetic-expression arithmetic-expression add-op 
    add-op add-op add-op term term mul-op mul-op mul-op mul-op factor factor 
    factor primary primary primary primary primary primary primary primary 
    primary primary primary expression-list-list expression-list-list name 
    name name component-reference component-reference $P12 $P12 
    component-reference-1 component-reference-1 $P13 $P13 $P14 $P14 
    function-call-args function-call-args function-arguments 
    function-arguments function-arguments-1 function-arguments-1 
    named-arguments named-only-arguments-1 named-only-arguments-1 
    named-only-arguments-1 named-argument function-argument function-argument 
    function-argument output-expression-list output-expression-list 
    output-expression-list expression-list expression-list array-subscripts 
    array-subscript-list array-subscript-list subscript subscript comment 
    comment string-comment string-comment string-cat string-cat opt-annotation
    opt-annotation annotation unsigned-number unsigned-number ident string))

(define mtab
  '(($string . $string) ($ident . $ident) ($float . $float) ($fixed . $fixed) 
    ("annotation" . $:annotation) ("]" . #{$:\x5d;}#) ("[" . #{$:\x5b;}#) 
    ("true" . $:true) ("false" . $:false) (".^" . $:.^) ("^" . $:^) ("./" . 
    $:./) (".*" . $:.*) ("/" . $:/) (".-" . $:.-) (".+" . $:.+) ("-" . $:-) 
    ("+" . $:+) ("<>" . $:<>) ("==" . $:==) (">=" . $:>=) (">" . $:>) ("<=" . 
    $:<=) ("<" . $:<) ("not" . $:not) ("and" . $:and) ("or" . $:or) ("connect"
    . $:connect) ("elsewhen" . $:elsewhen) ("when" . $:when) ("while" . 
    $:while) ("in" . $:in) ("loop" . $:loop) ("for" . $:for) ("else" . $:else)
    ("elseif" . $:elseif) ("then" . $:then) ("return" . $:return) ("break" . 
    $:break) ("algorithm" . $:algorithm) ("equation" . $:equation) ("initial" 
    . $:initial) ("each" . $:each) (":=" . $::=) ("if" . $:if) ("constant" . 
    $:constant) ("parameter" . $:parameter) ("discrete" . $:discrete) (
    "stream" . $:stream) ("flow" . $:flow) ("constrainedby" . $:constrainedby)
    ("}" . #{$:\x7d;}#) ("{" . #{$:\x7b;}#) ("*" . $:*) ("." . $:.) ("import" 
    . $:import) ("replaceable" . $:replaceable) ("outer" . $:outer) ("inner" 
    . $:inner) ("redeclare" . $:redeclare) ("external" . $:external) (
    "protected" . $:protected) ("public" . $:public) ("output" . $:output) 
    ("input" . $:input) ("," . $:,) ("der" . $:der) (":" . $::) (")" . 
    #{$:\x29;}#) ("(" . #{$:\x28;}#) ("enumeration" . $:enumeration) ("=" . 
    $:=) ("extends" . $:extends) ("end" . $:end) ("pure" . $:pure) ("function"
    . $:function) ("impure" . $:impure) ("package" . $:package) ("type" . 
    $:type) ("connector" . $:connector) ("expandable" . $:expandable) ("block"
    . $:block) ("record" . $:record) ("operator" . $:operator) ("model" . 
    $:model) ("class" . $:class) ("partial" . $:partial) ("encapsulated" . 
    $:encapsulated) ("final" . $:final) (";" . #{$:;}#) ("within" . $:within) 
    ($code-comm . $code-comm) ($lone-comm . $lone-comm) ($error . $error) 
    ($end . $end)))

;;; end tables
