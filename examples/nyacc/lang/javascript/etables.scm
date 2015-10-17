;; etables.scm

;; Copyright (C) 2015 Matthew R. Wette
;; 
;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;; or any later version published by the Free Software Foundation.  See the
;; file COPYING included with the this distribution.

(define len-v
  #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 2 5 4 2 1 4 3 1 2 2 3 3 5 1 1 1 1 4 
    3 3 1 2 2 2 4 3 2 3 1 3 1 1 1 3 3 0 0 1 2 2 2 2 2 2 2 2 2 1 3 3 3 1 3 3 1 
    3 3 3 1 3 3 3 3 3 1 3 3 3 3 1 3 1 3 1 3 1 3 1 3 1 5 1 3 1 1 1 1 1 1 1 1 1 
    1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 2 1 2 3 1 3 1 2 1 1 2 1 2 7 5 7 
    5 6 8 7 8 1 2 1 2 1 2 4 2 0 4 2 0 4 2 0 5 5 3 2 5 4 4 3 1 2 4 3 3 2 3 4 0 
    3 3 4 5 2 8 7 8 7 7 6 1 3 1 1 1 2 1 1))

(define pat-v
  #((($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- 
    shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) (
    $:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift 
    . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13
    ) (ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) ($:function shift . 62) (TryStatement shift . 
    63) (ThrowStatement shift . 64) (SwitchStatement shift . 65) (
    LabelledStatement shift . 66) (WithStatement shift . 67) (ReturnStatement 
    shift . 68) (BreakStatement shift . 69) (ContinueStatement shift . 70) (
    IterationStatement shift . 71) (IfStatement shift . 72) (
    ExpressionStatement shift . 73) (EmptyStatement shift . 74) (
    VariableStatement shift . 75) (Block shift . 76) (FunctionDeclaration 
    shift . 77) (Statement shift . 78) (SourceElement shift . 79)) (($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 167)) (($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 166)) (($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 165)) (($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 164)) (($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 163)) (($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 162)) (($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 161)) (($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 160)) (($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 159)) (($default 
    reduce . 55)) (($default reduce . 65)) (($:* shift . 155) ($:/ shift . 156
    ) ($:% shift . 157) ($default reduce . 69)) (($:+ shift . 153) ($:- shift 
    . 154) ($default reduce . 72)) (($:<< shift . 150) ($:>> shift . 151) (
    $:>>> shift . 152) ($default reduce . 76)) (($:< shift . 145) ($:> shift 
    . 146) ($:<= shift . 147) ($:>= shift . 148) ($:instanceof shift . 149) (
    $default reduce . 82)) (($:== shift . 141) ($:!= shift . 142) ($:=== shift
    . 143) ($:!== shift . 144) ($default reduce . 87)) (($default reduce . 10
    )) (($default reduce . 9)) (($default reduce . 8)) (($default reduce . 7))
    (($default reduce . 6)) (($default reduce . 5)) (($:& shift . 140) (
    $default reduce . 89)) ((#{$:\x5d;}# shift . 135) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) ($:, shift . 136) (AssignmentExpression 
    shift . 137) (Elision shift . 138) (ElementList shift . 139)) (($default 
    reduce . 4)) (($default reduce . 3)) (($default reduce . 2)) (($default 
    reduce . 1)) (($:^ shift . 134) ($default reduce . 91)) (($:! shift . 1) (
    $:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ 
    shift . 6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 44) (
    Expression shift . 133)) (($default reduce . 15)) (($default reduce . 14))
    (($default reduce . 12)) (($:| shift . 132) ($default reduce . 93)) ((
    $default reduce . 34)) (($:&& shift . 131) ($default reduce . 95)) ((
    NewExpression shift . 128) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (#{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral shift . 
    25) (NumericLiteral shift . 26) (BooleanLiteral shift . 27) (NullLiteral 
    shift . 28) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) ($:new shift . 37
    ) (PrimaryExpression shift . 35) (MemberExpression shift . 130)) ((
    #{$:\x28;}# shift . 119) (Arguments shift . 125) (#{$:\x5b;}# shift . 126)
    ($:. shift . 127) ($default reduce . 38)) (($:? shift . 123) ($:|| shift 
    . 124) ($default reduce . 97)) ((#{$:\x28;}# shift . 119) (Arguments shift
    . 120) (#{$:\x5b;}# shift . 121) ($:. shift . 122) ($default reduce . 49)
    ) (($default reduce . 48)) (($:|= shift . 104) ($:^= shift . 105) ($:&= 
    shift . 106) ($:>>>= shift . 107) ($:>>= shift . 108) ($:<<= shift . 109) 
    ($:-= shift . 110) ($:+= shift . 111) ($:%= shift . 112) ($:/= shift . 113
    ) ($:*= shift . 114) ($:= shift . 115) (AssignmentOperator shift . 116) (
    $P1 shift . 117) ($P2 shift . 118) ($:++ reduce . 53) ($:-- reduce . 54) (
    $default reduce . 50)) (($default reduce . 99)) (($default reduce . 113)) 
    (($default reduce . 11)) ((#{$:\x7b;}# shift . 61) (Block shift . 103)) ((
    $P6 shift . 102) ($default reduce . 183)) (($:: shift . 101) ($default 
    reduce . 13)) ((#{$:\x28;}# shift . 100)) ((#{$:\x28;}# shift . 99)) (($P5
    shift . 97) (#{$:;}# shift . 98) ($default reduce . 166)) (($P4 shift . 
    95) (#{$:;}# shift . 96) ($ident reduce . 163)) (($P3 shift . 93) (#{$:;}#
    shift . 94) ($ident reduce . 160)) ((#{$:\x28;}# shift . 92)) ((
    #{$:\x28;}# shift . 91)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) 
    ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) (
    $:void shift . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (
    UnaryExpression shift . 11) (MultiplicativeExpression shift . 12) (
    AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) (StringLiteral shift . 25) (NumericLiteral shift 
    . 26) (BooleanLiteral shift . 27) (NullLiteral shift . 28) (
    BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral 
    shift . 31) (Literal shift . 32) ($:this shift . 33) (BitwiseORExpression 
    shift . 34) (PrimaryExpression shift . 35) (LogicalANDExpression shift . 
    36) ($:new shift . 37) (MemberExpression shift . 38) (LogicalORExpression 
    shift . 39) (CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 90)) ((#{$:\x28;}# shift . 89)) ((#{$:;}# shift . 87) ($:, shift 
    . 88)) (($default reduce . 142)) (($ident shift . 45) (Identifier shift . 
    84) (VariableDeclaration shift . 85) (VariableDeclarationList shift . 86))
    (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- 
    shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) (
    $:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift 
    . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13
    ) (ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 81) (StatementList shift . 82) (#{$:\x7d;}# shift . 83)) (($ident 
    shift . 45) (Identifier shift . 80)) (($default reduce . 129)) (($default 
    reduce . 128)) (($default reduce . 127)) (($default reduce . 126)) ((
    $default reduce . 125)) (($default reduce . 124)) (($default reduce . 123)
    ) (($default reduce . 122)) (($default reduce . 121)) (($default reduce . 
    120)) (($default reduce . 119)) (($default reduce . 118)) (($default 
    reduce . 117)) (($default reduce . 116)) (($default reduce . 202)) ((
    $default reduce . 201)) (($end accept . 0)) ((#{$:\x28;}# shift . 234)) ((
    $default reduce . 132)) ((#{$:\x7d;}# shift . 232) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 233)) (($default reduce . 131)) (($:= shift . 230) (Initializer 
    shift . 231) (#{$:;}# reduce . 139) ($:, reduce . 139) ($:in reduce . 139)
    ) ((#{$:;}# reduce . 135) ($:, reduce . 135)) ((#{$:;}# shift . 228) ($:, 
    shift . 229)) (($default reduce . 143)) (($:! shift . 1) ($:~ shift . 2) (
    $:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof
    shift . 7) ($:void shift . 8) ($:delete shift . 9) (PostfixExpression 
    shift . 10) (UnaryExpression shift . 11) (MultiplicativeExpression shift 
    . 12) (AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral shift . 25) (
    NumericLiteral shift . 26) (BooleanLiteral shift . 27) (NullLiteral shift 
    . 28) (BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (
    ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 227)) (($:! shift . 1) ($:~ shift . 2) ($:- 
    shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof 
    shift . 7) ($:void shift . 8) ($:delete shift . 9) (PostfixExpression 
    shift . 10) (UnaryExpression shift . 11) (MultiplicativeExpression shift 
    . 12) (AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral shift . 25) (
    NumericLiteral shift . 26) (BooleanLiteral shift . 27) (NullLiteral shift 
    . 28) (BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (
    ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) (Expression shift . 226)) (($:while shift
    . 225)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) 
    ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) 
    ($:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift
    . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression shift . 
    13) (ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 44) (
    Expression shift . 224)) (($:var shift . 218) ($:! shift . 1) ($:~ shift 
    . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) (
    $:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 219) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 44) (
    Expression shift . 220) (ExpressionNoIn shift . 221) ($:: shift . 222) (
    OptExprStmtNoIn shift . 223)) (($ident shift . 45) (Identifier shift . 217
    )) (($default reduce . 159)) (($ident shift . 45) (Identifier shift . 216)
    ) (($default reduce . 162)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 
    3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) 
    ($:void shift . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (
    UnaryExpression shift . 11) (MultiplicativeExpression shift . 12) (
    AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral shift . 25) (
    NumericLiteral shift . 26) (BooleanLiteral shift . 27) (NullLiteral shift 
    . 28) (BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (
    ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) (Expression shift . 215)) (($default 
    reduce . 165)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift
    . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift
    . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression
    shift . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression 
    shift . 13) (ShiftExpression shift . 14) (RelationalExpression shift . 15)
    (EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) 
    ($fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 44) (
    Expression shift . 214)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) 
    ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) (
    $:void shift . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (
    UnaryExpression shift . 11) (MultiplicativeExpression shift . 12) (
    AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral shift . 25) (
    NumericLiteral shift . 26) (BooleanLiteral shift . 27) (NullLiteral shift 
    . 28) (BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (
    ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) (Expression shift . 213)) (($:! shift . 1
    ) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ 
    shift . 6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 212)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift 
    . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift 
    . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression 
    shift . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression 
    shift . 13) (ShiftExpression shift . 14) (RelationalExpression shift . 15)
    (EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) 
    ($fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 44) (
    Expression shift . 211)) (($:finally shift . 207) (Finally shift . 208) (
    $:catch shift . 209) (Catch shift . 210)) (($default reduce . 112)) ((
    $default reduce . 111)) (($default reduce . 110)) (($default reduce . 109)
    ) (($default reduce . 108)) (($default reduce . 107)) (($default reduce . 
    106)) (($default reduce . 105)) (($default reduce . 104)) (($default 
    reduce . 103)) (($default reduce . 102)) (($default reduce . 101)) (($:! 
    shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5
    ) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift
    . 9) (PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 206)) ((
    $:++ shift . 205)) (($:-- shift . 204)) ((#{$:\x29;}# shift . 201) ($:! 
    shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5
    ) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift
    . 9) (PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 202) (
    ArgumentList shift . 203)) (($default reduce . 41)) (($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 44) (
    Expression shift . 200)) (($ident shift . 45) (Identifier shift . 199)) ((
    $:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift
    . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) ($:delete 
    shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 198)) ((
    $string shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift
    . 20) ($:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) (
    $ident shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) 
    (BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) (BitwiseANDExpression shift . 23) (
    BitwiseXORExpression shift . 29) (BitwiseORExpression shift . 34) (
    LogicalANDExpression shift . 197)) (($default reduce . 40)) (($:! shift . 
    1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ 
    shift . 6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 44) (
    Expression shift . 196)) (($ident shift . 45) (Identifier shift . 195)) ((
    $default reduce . 39)) (($default reduce . 13)) ((#{$:\x28;}# shift . 119)
    (Arguments shift . 194) (#{$:\x5b;}# shift . 126) ($:. shift . 127) (
    $default reduce . 38)) (($string shift . 17) ($float shift . 18) ($fixed 
    shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift . 22) (
    #{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral shift . 25) (
    NumericLiteral shift . 26) (BooleanLiteral shift . 27) (NullLiteral shift 
    . 28) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 
    32) (Identifier shift . 129) ($:this shift . 33) (PrimaryExpression shift 
    . 35) ($:new shift . 37) (MemberExpression shift . 38) (CallExpression 
    shift . 40) (NewExpression shift . 41) (LeftHandSideExpression shift . 158
    ) ($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- 
    shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) (
    $:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift 
    . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13
    ) (ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) (BitwiseANDExpression shift . 23) (
    BitwiseXORExpression shift . 29) (BitwiseORExpression shift . 193)) ((
    $string shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift
    . 20) ($:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) (
    $ident shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) 
    (BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) (BitwiseANDExpression shift . 23) (
    BitwiseXORExpression shift . 192)) ((#{$:\x29;}# shift . 191) ($:, shift 
    . 88)) (($string shift . 17) ($float shift . 18) ($fixed shift . 19) (
    $:false shift . 20) ($:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# 
    shift . 24) ($ident shift . 45) (StringLiteral shift . 25) (NumericLiteral
    shift . 26) (BooleanLiteral shift . 27) (NullLiteral shift . 28) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    Identifier shift . 129) ($:this shift . 33) (PrimaryExpression shift . 35)
    ($:new shift . 37) (MemberExpression shift . 38) (CallExpression shift . 
    40) (NewExpression shift . 41) (LeftHandSideExpression shift . 158) ($:! 
    shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5
    ) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift
    . 9) (PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) (BitwiseANDExpression shift . 190)) ((
    $default reduce . 18)) (($default reduce . 25)) (($:, reduce . 22)) (($:! 
    shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5
    ) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift
    . 9) (PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 187) ($:, 
    shift . 188) (#{$:\x5d;}# shift . 189)) (($:, shift . 186)) (($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 185)) (($string shift . 17) ($float shift . 18)
    ($fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null 
    shift . 22) (#{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral 
    shift . 25) (NumericLiteral shift . 26) (BooleanLiteral shift . 27) (
    NullLiteral shift . 28) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31)
    (Literal shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    PrimaryExpression shift . 35) ($:new shift . 37) (MemberExpression shift 
    . 38) (CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ shift . 2) ($:- 
    shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof 
    shift . 7) ($:void shift . 8) ($:delete shift . 9) (PostfixExpression 
    shift . 10) (UnaryExpression shift . 11) (MultiplicativeExpression shift 
    . 12) (AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 184)) (($string shift . 17) ($float shift . 
    18) ($fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null 
    shift . 22) (#{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral 
    shift . 25) (NumericLiteral shift . 26) (BooleanLiteral shift . 27) (
    NullLiteral shift . 28) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31)
    (Literal shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    PrimaryExpression shift . 35) ($:new shift . 37) (MemberExpression shift 
    . 38) (CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ shift . 2) ($:- 
    shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof 
    shift . 7) ($:void shift . 8) ($:delete shift . 9) (PostfixExpression 
    shift . 10) (UnaryExpression shift . 11) (MultiplicativeExpression shift 
    . 12) (AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 183)) (($string shift . 17) ($float shift . 
    18) ($fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null 
    shift . 22) (#{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral 
    shift . 25) (NumericLiteral shift . 26) (BooleanLiteral shift . 27) (
    NullLiteral shift . 28) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31)
    (Literal shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    PrimaryExpression shift . 35) ($:new shift . 37) (MemberExpression shift 
    . 38) (CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ shift . 2) ($:- 
    shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof 
    shift . 7) ($:void shift . 8) ($:delete shift . 9) (PostfixExpression 
    shift . 10) (UnaryExpression shift . 11) (MultiplicativeExpression shift 
    . 12) (AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 182)) (($string shift . 17) ($float shift . 
    18) ($fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null 
    shift . 22) (#{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral 
    shift . 25) (NumericLiteral shift . 26) (BooleanLiteral shift . 27) (
    NullLiteral shift . 28) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31)
    (Literal shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    PrimaryExpression shift . 35) ($:new shift . 37) (MemberExpression shift 
    . 38) (CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ shift . 2) ($:- 
    shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof 
    shift . 7) ($:void shift . 8) ($:delete shift . 9) (PostfixExpression 
    shift . 10) (UnaryExpression shift . 11) (MultiplicativeExpression shift 
    . 12) (AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 181)) (($string shift . 17) ($float shift . 
    18) ($fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null 
    shift . 22) (#{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral 
    shift . 25) (NumericLiteral shift . 26) (BooleanLiteral shift . 27) (
    NullLiteral shift . 28) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31)
    (Literal shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    PrimaryExpression shift . 35) ($:new shift . 37) (MemberExpression shift 
    . 38) (CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ shift . 2) ($:- 
    shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof 
    shift . 7) ($:void shift . 8) ($:delete shift . 9) (PostfixExpression 
    shift . 10) (UnaryExpression shift . 11) (MultiplicativeExpression shift 
    . 12) (AdditiveExpression shift . 13) (ShiftExpression shift . 180)) ((
    $string shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift
    . 20) ($:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) (
    $ident shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) 
    (BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 179)) (($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (#{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral shift . 
    25) (NumericLiteral shift . 26) (BooleanLiteral shift . 27) (NullLiteral 
    shift . 28) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    PrimaryExpression shift . 35) ($:new shift . 37) (MemberExpression shift 
    . 38) (CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ shift . 2) ($:- 
    shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof 
    shift . 7) ($:void shift . 8) ($:delete shift . 9) (PostfixExpression 
    shift . 10) (UnaryExpression shift . 11) (MultiplicativeExpression shift 
    . 12) (AdditiveExpression shift . 13) (ShiftExpression shift . 178)) ((
    $string shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift
    . 20) ($:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) (
    $ident shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) 
    (BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 177)) (($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (#{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral shift . 
    25) (NumericLiteral shift . 26) (BooleanLiteral shift . 27) (NullLiteral 
    shift . 28) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    PrimaryExpression shift . 35) ($:new shift . 37) (MemberExpression shift 
    . 38) (CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ shift . 2) ($:- 
    shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof 
    shift . 7) ($:void shift . 8) ($:delete shift . 9) (PostfixExpression 
    shift . 10) (UnaryExpression shift . 11) (MultiplicativeExpression shift 
    . 12) (AdditiveExpression shift . 13) (ShiftExpression shift . 176)) ((
    $string shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift
    . 20) ($:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) (
    $ident shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) 
    (BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 175)) ((
    $string shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift
    . 20) ($:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) (
    $ident shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) 
    (BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 174)) ((
    $string shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift
    . 20) ($:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) (
    $ident shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) 
    (BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 173)) ((
    $string shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift
    . 20) ($:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) (
    $ident shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) 
    (BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 172)) (($string shift . 17) ($float shift
    . 18) ($fixed shift . 19) ($:false shift . 20) ($:true shift . 21) (
    $:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident shift . 45) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 30) (
    ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129) (
    $:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 171)) (($string shift . 17) ($float shift
    . 18) ($fixed shift . 19) ($:false shift . 20) ($:true shift . 21) (
    $:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident shift . 45) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 30) (
    ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129) (
    $:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 170)) (($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 169)) (($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (#{$:\x28;}# shift . 
    30) (ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129
    ) ($:this shift . 33) (PrimaryExpression shift . 35) ($:new shift . 37) (
    MemberExpression shift . 38) (CallExpression shift . 40) (NewExpression 
    shift . 41) (LeftHandSideExpression shift . 158) ($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 168)) (($P1 shift 
    . 117) ($P2 shift . 118) ($:++ reduce . 53) ($:-- reduce . 54) ($default 
    reduce . 50)) (($default reduce . 56)) (($default reduce . 57)) (($default
    reduce . 58)) (($default reduce . 59)) (($default reduce . 60)) ((
    $default reduce . 61)) (($default reduce . 62)) (($default reduce . 63)) (
    ($default reduce . 64)) (($default reduce . 68)) (($default reduce . 67)) 
    (($default reduce . 66)) (($:* shift . 155) ($:/ shift . 156) ($:% shift 
    . 157) ($default reduce . 71)) (($:* shift . 155) ($:/ shift . 156) ($:% 
    shift . 157) ($default reduce . 70)) (($:+ shift . 153) ($:- shift . 154) 
    ($default reduce . 75)) (($:+ shift . 153) ($:- shift . 154) ($default 
    reduce . 74)) (($:+ shift . 153) ($:- shift . 154) ($default reduce . 73))
    (($:<< shift . 150) ($:>> shift . 151) ($:>>> shift . 152) ($default 
    reduce . 81)) (($:<< shift . 150) ($:>> shift . 151) ($:>>> shift . 152) (
    $default reduce . 80)) (($:<< shift . 150) ($:>> shift . 151) ($:>>> shift
    . 152) ($default reduce . 79)) (($:<< shift . 150) ($:>> shift . 151) (
    $:>>> shift . 152) ($default reduce . 78)) (($:<< shift . 150) ($:>> shift
    . 151) ($:>>> shift . 152) ($default reduce . 77)) (($:< shift . 145) (
    $:> shift . 146) ($:<= shift . 147) ($:>= shift . 148) ($:instanceof shift
    . 149) ($default reduce . 86)) (($:< shift . 145) ($:> shift . 146) ($:<=
    shift . 147) ($:>= shift . 148) ($:instanceof shift . 149) ($default 
    reduce . 85)) (($:< shift . 145) ($:> shift . 146) ($:<= shift . 147) (
    $:>= shift . 148) ($:instanceof shift . 149) ($default reduce . 84)) (($:<
    shift . 145) ($:> shift . 146) ($:<= shift . 147) ($:>= shift . 148) (
    $:instanceof shift . 149) ($default reduce . 83)) (($:== shift . 141) (
    $:!= shift . 142) ($:=== shift . 143) ($:!== shift . 144) ($default reduce
    . 88)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) (
    $:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) (
    $:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift 
    . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13
    ) (ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 266) ($:, 
    shift . 136) (Elision shift . 267) (#{$:\x5d;}# shift . 268)) (($:, reduce
    . 21)) (($default reduce . 26)) (($default reduce . 17)) (($:& shift . 
    140) ($default reduce . 90)) (($default reduce . 16)) (($:^ shift . 134) (
    $default reduce . 92)) (($:| shift . 132) ($default reduce . 94)) ((
    $default reduce . 37)) (($default reduce . 36)) ((#{$:\x5d;}# shift . 265)
    ($:, shift . 88)) (($:&& shift . 131) ($default reduce . 96)) (($:: shift
    . 264)) (($default reduce . 43)) ((#{$:\x5d;}# shift . 263) ($:, shift . 
    88)) (($default reduce . 44)) ((#{$:\x29;}# reduce . 46) ($:, reduce . 46)
    ) ((#{$:\x29;}# shift . 261) ($:, shift . 262)) (($default reduce . 52)) (
    ($default reduce . 51)) (($default reduce . 100)) ((#{$:\x7b;}# shift . 61
    ) (Block shift . 260)) (($default reduce . 185)) ((#{$:\x28;}# shift . 259
    )) (($:finally shift . 207) (Finally shift . 258) ($default reduce . 184))
    ((#{$:;}# shift . 257) ($:, shift . 88)) (($default reduce . 181)) ((
    #{$:\x29;}# shift . 256) ($:, shift . 88)) ((#{$:\x29;}# shift . 255) ($:,
    shift . 88)) ((#{$:;}# shift . 254) ($:, shift . 88)) ((#{$:;}# shift . 
    253)) ((#{$:;}# shift . 252)) ((VariableDeclarationList shift . 248) (
    VariableDeclarationListNoIn shift . 249) ($ident shift . 45) (Identifier 
    shift . 84) (VariableDeclaration shift . 250) (VariableDeclarationNoIn 
    shift . 251)) (($:|= shift . 104) ($:^= shift . 105) ($:&= shift . 106) (
    $:>>>= shift . 107) ($:>>= shift . 108) ($:<<= shift . 109) ($:-= shift . 
    110) ($:+= shift . 111) ($:%= shift . 112) ($:/= shift . 113) ($:*= shift 
    . 114) ($:= shift . 115) (AssignmentOperator shift . 116) ($P1 shift . 117
    ) ($P2 shift . 118) ($:in shift . 247) ($:++ reduce . 53) ($:-- reduce . 
    54) ($default reduce . 50)) (($:, shift . 88) (#{$:;}# reduce . 115)) ((
    #{$:;}# shift . 246)) (($default reduce . 152)) (($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 44) (
    Expression shift . 243) (#{$:;}# shift . 244) (OptExprStmt shift . 245)) (
    (#{$:\x29;}# shift . 242) ($:, shift . 88)) ((#{$:\x28;}# shift . 241)) ((
    $:, shift . 88) (#{$:\x29;}# shift . 240)) (($default reduce . 114)) ((
    $default reduce . 134)) (($ident shift . 45) (Identifier shift . 84) (
    VariableDeclaration shift . 239)) (($:! shift . 1) ($:~ shift . 2) ($:- 
    shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof 
    shift . 7) ($:void shift . 8) ($:delete shift . 9) (PostfixExpression 
    shift . 10) (UnaryExpression shift . 11) (MultiplicativeExpression shift 
    . 12) (AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral shift . 25) (
    NumericLiteral shift . 26) (BooleanLiteral shift . 27) (NullLiteral shift 
    . 28) (BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (
    ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 238)) ((#{$:;}# reduce . 138) ($:, reduce . 
    138) ($:in reduce . 138)) (($default reduce . 130)) (($default reduce . 
    133)) (($ident shift . 45) (Identifier shift . 235) (FormalParameterList 
    shift . 236) (#{$:\x29;}# shift . 237)) ((#{$:\x29;}# reduce . 195) ($:, 
    reduce . 195)) ((#{$:\x29;}# shift . 288) ($:, shift . 289)) ((#{$:\x7b;}#
    shift . 287)) ((#{$:;}# reduce . 141) ($:, reduce . 141) ($:in reduce . 
    141)) ((#{$:;}# reduce . 136) ($:, reduce . 136)) (($:! shift . 1) ($:~ 
    shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 
    6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 286)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift 
    . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift 
    . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression 
    shift . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression 
    shift . 13) (ShiftExpression shift . 14) (RelationalExpression shift . 15)
    (EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) 
    ($fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 44) (
    Expression shift . 285)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) 
    ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) (
    $:void shift . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (
    UnaryExpression shift . 11) (MultiplicativeExpression shift . 12) (
    AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) (StringLiteral shift . 25) (NumericLiteral shift 
    . 26) (BooleanLiteral shift . 27) (NullLiteral shift . 28) (
    BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral 
    shift . 31) (Literal shift . 32) ($:this shift . 33) (BitwiseORExpression 
    shift . 34) (PrimaryExpression shift . 35) (LogicalANDExpression shift . 
    36) ($:new shift . 37) (MemberExpression shift . 38) (LogicalORExpression 
    shift . 39) (CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 284)) ((#{$:;}# shift . 283) ($:, shift . 88)) (($default reduce 
    . 154)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) (
    $:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) (
    $:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift 
    . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13
    ) (ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 44) (
    Expression shift . 280) (#{$:;}# shift . 281) (OptExprClose shift . 282)) 
    (($default reduce . 153)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3)
    ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) (
    $:void shift . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (
    UnaryExpression shift . 11) (MultiplicativeExpression shift . 12) (
    AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral shift . 25) (
    NumericLiteral shift . 26) (BooleanLiteral shift . 27) (NullLiteral shift 
    . 28) (BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (
    ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) (Expression shift . 279)) (($:, shift . 
    229) (#{$:;}# reduce . 137)) ((#{$:;}# shift . 278)) (($:in reduce . 140) 
    (#{$:;}# reduce . 135) ($:, reduce . 135)) (($:in shift . 277)) (($default
    reduce . 158)) (($default reduce . 161)) (($default reduce . 164)) (($:! 
    shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5
    ) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift
    . 9) (PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 276)) ((#{$:\x7b;}# shift . 274) (CaseBlock shift . 275)) ((
    $default reduce . 182)) (($default reduce . 186)) (($ident shift . 45) (
    Identifier shift . 273)) (($default reduce . 188)) (($default reduce . 45)
    ) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- 
    shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) (
    $:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift 
    . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13
    ) (ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 272)) ((
    $default reduce . 42)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) (
    $:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) (
    $:void shift . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (
    UnaryExpression shift . 11) (MultiplicativeExpression shift . 12) (
    AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral shift . 25) (
    NumericLiteral shift . 26) (BooleanLiteral shift . 27) (NullLiteral shift 
    . 28) (BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (
    ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 271)) (($default reduce . 35)) (($:, reduce 
    . 24)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) (
    $:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) (
    $:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift 
    . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13
    ) (ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 269) ($:, 
    shift . 188) (#{$:\x5d;}# shift . 270)) (($default reduce . 20)) (($:, 
    reduce . 23)) (($default reduce . 19)) (($default reduce . 98)) ((
    #{$:\x29;}# reduce . 47) ($:, reduce . 47)) ((#{$:\x29;}# shift . 308)) ((
    #{$:\x7d;}# shift . 302) ($:case shift . 303) (CaseClause shift . 304) (
    CaseClauses shift . 305) ($:default shift . 306) (DefaultClause shift . 
    307)) (($default reduce . 168)) (($default reduce . 167)) (($:! shift . 1)
    ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ 
    shift . 6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 44) (
    Expression shift . 301)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) 
    ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) (
    $:void shift . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (
    UnaryExpression shift . 11) (MultiplicativeExpression shift . 12) (
    AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) ($ident shift . 45) (StringLiteral shift . 25) (
    NumericLiteral shift . 26) (BooleanLiteral shift . 27) (NullLiteral shift 
    . 28) (BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (
    ArrayLiteral shift . 31) (Literal shift . 32) (Identifier shift . 129) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) (Expression shift . 243) (#{$:;}# shift 
    . 244) (OptExprStmt shift . 300)) ((#{$:\x29;}# shift . 299) ($:, shift . 
    88)) ((#{$:\x29;}# shift . 298) ($:, shift . 88)) (($default reduce . 156)
    ) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- 
    shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) (
    $:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift 
    . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13
    ) (ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 297)) (($default reduce . 155)) (($default reduce . 147)) ((
    #{$:\x29;}# shift . 296) ($:, shift . 88)) (($:else shift . 295) ($default
    reduce . 145)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ 
    shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void 
    shift . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (
    UnaryExpression shift . 11) (MultiplicativeExpression shift . 12) (
    AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) (StringLiteral shift . 25) (NumericLiteral shift 
    . 26) (BooleanLiteral shift . 27) (NullLiteral shift . 28) (
    BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral 
    shift . 31) (Literal shift . 32) ($:this shift . 33) (BitwiseORExpression 
    shift . 34) (PrimaryExpression shift . 35) (LogicalANDExpression shift . 
    36) ($:new shift . 37) (MemberExpression shift . 38) (LogicalORExpression 
    shift . 39) (CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) ($:function shift . 62) (TryStatement shift . 
    63) (ThrowStatement shift . 64) (SwitchStatement shift . 65) (
    LabelledStatement shift . 66) (WithStatement shift . 67) (ReturnStatement 
    shift . 68) (BreakStatement shift . 69) (ContinueStatement shift . 70) (
    IterationStatement shift . 71) (IfStatement shift . 72) (
    ExpressionStatement shift . 73) (EmptyStatement shift . 74) (
    VariableStatement shift . 75) (Block shift . 76) (FunctionDeclaration 
    shift . 77) (Statement shift . 78) (SourceElement shift . 292) (
    SourceElements shift . 293) (FunctionBody shift . 294)) ((#{$:\x7b;}# 
    shift . 291)) (($ident shift . 45) (Identifier shift . 290)) ((#{$:\x29;}#
    reduce . 196) ($:, reduce . 196)) (($:! shift . 1) ($:~ shift . 2) ($:- 
    shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof 
    shift . 7) ($:void shift . 8) ($:delete shift . 9) (PostfixExpression 
    shift . 10) (UnaryExpression shift . 11) (MultiplicativeExpression shift 
    . 12) (AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) (StringLiteral shift . 25) (NumericLiteral shift 
    . 26) (BooleanLiteral shift . 27) (NullLiteral shift . 28) (
    BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral 
    shift . 31) (Literal shift . 32) ($:this shift . 33) (BitwiseORExpression 
    shift . 34) (PrimaryExpression shift . 35) (LogicalANDExpression shift . 
    36) ($:new shift . 37) (MemberExpression shift . 38) (LogicalORExpression 
    shift . 39) (CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) ($:function shift . 62) (TryStatement shift . 
    63) (ThrowStatement shift . 64) (SwitchStatement shift . 65) (
    LabelledStatement shift . 66) (WithStatement shift . 67) (ReturnStatement 
    shift . 68) (BreakStatement shift . 69) (ContinueStatement shift . 70) (
    IterationStatement shift . 71) (IfStatement shift . 72) (
    ExpressionStatement shift . 73) (EmptyStatement shift . 74) (
    VariableStatement shift . 75) (Block shift . 76) (FunctionDeclaration 
    shift . 77) (Statement shift . 78) (SourceElement shift . 292) (
    SourceElements shift . 293) (FunctionBody shift . 324)) (($default reduce 
    . 199)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) (
    $:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) (
    $:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift 
    . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13
    ) (ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) ($:function shift . 62) (TryStatement shift . 
    63) (ThrowStatement shift . 64) (SwitchStatement shift . 65) (
    LabelledStatement shift . 66) (WithStatement shift . 67) (ReturnStatement 
    shift . 68) (BreakStatement shift . 69) (ContinueStatement shift . 70) (
    IterationStatement shift . 71) (IfStatement shift . 72) (
    ExpressionStatement shift . 73) (EmptyStatement shift . 74) (
    VariableStatement shift . 75) (Block shift . 76) (FunctionDeclaration 
    shift . 77) (Statement shift . 78) (SourceElement shift . 323) (
    #{$:\x7d;}# reduce . 197)) ((#{$:\x7d;}# shift . 322)) (($:! shift . 1) (
    $:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ 
    shift . 6) ($:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 321)) ((#{$:;}# shift . 320)) (($default reduce . 148)) (($default
    reduce . 157)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ 
    shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void 
    shift . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (
    UnaryExpression shift . 11) (MultiplicativeExpression shift . 12) (
    AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) (StringLiteral shift . 25) (NumericLiteral shift 
    . 26) (BooleanLiteral shift . 27) (NullLiteral shift . 28) (
    BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral 
    shift . 31) (Literal shift . 32) ($:this shift . 33) (BitwiseORExpression 
    shift . 34) (PrimaryExpression shift . 35) (LogicalANDExpression shift . 
    36) ($:new shift . 37) (MemberExpression shift . 38) (LogicalORExpression 
    shift . 39) (CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 319)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift 
    . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift 
    . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression 
    shift . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression 
    shift . 13) (ShiftExpression shift . 14) (RelationalExpression shift . 15)
    (EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) 
    ($fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 44) (
    Expression shift . 280) (#{$:;}# shift . 281) (OptExprClose shift . 318)) 
    ((#{$:\x29;}# shift . 317) ($:, shift . 88)) (($default reduce . 170)) ((
    $:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift
    . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) ($:delete 
    shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) ($ident 
    shift . 45) (StringLiteral shift . 25) (NumericLiteral shift . 26) (
    BooleanLiteral shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression 
    shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal 
    shift . 32) (Identifier shift . 129) ($:this shift . 33) (
    BitwiseORExpression shift . 34) (PrimaryExpression shift . 35) (
    LogicalANDExpression shift . 36) ($:new shift . 37) (MemberExpression 
    shift . 38) (LogicalORExpression shift . 39) (CallExpression shift . 40) (
    NewExpression shift . 41) (LeftHandSideExpression shift . 42) (
    ConditionalExpression shift . 43) (AssignmentExpression shift . 44) (
    Expression shift . 316)) (($:default reduce . 175) ($:case reduce . 175) (
    #{$:\x7d;}# reduce . 175)) (($:case shift . 303) (CaseClause shift . 313) 
    ($:default shift . 306) (DefaultClause shift . 314) (#{$:\x7d;}# shift . 
    315)) (($:: shift . 312)) ((#{$:\x7d;}# shift . 310) ($:case shift . 303) 
    (CaseClause shift . 304) (CaseClauses shift . 311)) ((#{$:\x7b;}# shift . 
    61) (Block shift . 309)) (($default reduce . 187)) (($default reduce . 174
    )) ((#{$:\x7d;}# shift . 332) ($:case shift . 303) (CaseClause shift . 313
    )) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- 
    shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) (
    $:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift 
    . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13
    ) (ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 81) (StatementList shift . 331) (#{$:\x7d;}# reduce . 180) ($:case
    reduce . 180)) (($:default reduce . 176) ($:case reduce . 176) (
    #{$:\x7d;}# reduce . 176)) (($:case shift . 303) (CaseClause shift . 304) 
    (CaseClauses shift . 329) (#{$:\x7d;}# shift . 330)) (($default reduce . 
    169)) (($:, shift . 88) ($:: shift . 328)) (($:! shift . 1) ($:~ shift . 2
    ) ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) (
    $:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 327)) (($:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift 
    . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift 
    . 8) ($:delete shift . 9) (PostfixExpression shift . 10) (UnaryExpression 
    shift . 11) (MultiplicativeExpression shift . 12) (AdditiveExpression 
    shift . 13) (ShiftExpression shift . 14) (RelationalExpression shift . 15)
    (EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) 
    ($fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 326)) (($default reduce . 150)) (($default reduce . 146)) ((
    $default reduce . 144)) (($default reduce . 190)) (($default reduce . 200)
    ) ((#{$:\x7d;}# shift . 325)) (($default reduce . 189)) (($default reduce 
    . 149)) (($default reduce . 151)) (($:! shift . 1) ($:~ shift . 2) ($:- 
    shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) ($:typeof 
    shift . 7) ($:void shift . 8) ($:delete shift . 9) (PostfixExpression 
    shift . 10) (UnaryExpression shift . 11) (MultiplicativeExpression shift 
    . 12) (AdditiveExpression shift . 13) (ShiftExpression shift . 14) (
    RelationalExpression shift . 15) (EqualityExpression shift . 16) ($string 
    shift . 17) ($float shift . 18) ($fixed shift . 19) ($:false shift . 20) (
    $:true shift . 21) ($:null shift . 22) (BitwiseANDExpression shift . 23) (
    #{$:\x5b;}# shift . 24) (StringLiteral shift . 25) (NumericLiteral shift 
    . 26) (BooleanLiteral shift . 27) (NullLiteral shift . 28) (
    BitwiseXORExpression shift . 29) (#{$:\x28;}# shift . 30) (ArrayLiteral 
    shift . 31) (Literal shift . 32) ($:this shift . 33) (BitwiseORExpression 
    shift . 34) (PrimaryExpression shift . 35) (LogicalANDExpression shift . 
    36) ($:new shift . 37) (MemberExpression shift . 38) (LogicalORExpression 
    shift . 39) (CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 81) (StatementList shift . 334) ($:default reduce . 178) ($:case 
    reduce . 178) (#{$:\x7d;}# reduce . 178)) ((#{$:\x7d;}# shift . 333) (
    $:case shift . 303) (CaseClause shift . 313)) (($default reduce . 172)) ((
    $:! shift . 1) ($:~ shift . 2) ($:- shift . 3) ($:+ shift . 4) ($:-- shift
    . 5) ($:++ shift . 6) ($:typeof shift . 7) ($:void shift . 8) ($:delete 
    shift . 9) (PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 233) (#{$:\x7d;}# reduce . 179) ($:case reduce . 179)) (($default 
    reduce . 173)) (($default reduce . 171)) (($:! shift . 1) ($:~ shift . 2) 
    ($:- shift . 3) ($:+ shift . 4) ($:-- shift . 5) ($:++ shift . 6) (
    $:typeof shift . 7) ($:void shift . 8) ($:delete shift . 9) (
    PostfixExpression shift . 10) (UnaryExpression shift . 11) (
    MultiplicativeExpression shift . 12) (AdditiveExpression shift . 13) (
    ShiftExpression shift . 14) (RelationalExpression shift . 15) (
    EqualityExpression shift . 16) ($string shift . 17) ($float shift . 18) (
    $fixed shift . 19) ($:false shift . 20) ($:true shift . 21) ($:null shift 
    . 22) (BitwiseANDExpression shift . 23) (#{$:\x5b;}# shift . 24) (
    StringLiteral shift . 25) (NumericLiteral shift . 26) (BooleanLiteral 
    shift . 27) (NullLiteral shift . 28) (BitwiseXORExpression shift . 29) (
    #{$:\x28;}# shift . 30) (ArrayLiteral shift . 31) (Literal shift . 32) (
    $:this shift . 33) (BitwiseORExpression shift . 34) (PrimaryExpression 
    shift . 35) (LogicalANDExpression shift . 36) ($:new shift . 37) (
    MemberExpression shift . 38) (LogicalORExpression shift . 39) (
    CallExpression shift . 40) (NewExpression shift . 41) (
    LeftHandSideExpression shift . 42) (ConditionalExpression shift . 43) (
    AssignmentExpression shift . 44) ($ident shift . 45) ($:try shift . 46) (
    $:throw shift . 47) (Identifier shift . 48) ($:switch shift . 49) ($:with 
    shift . 50) ($:return shift . 51) ($:break shift . 52) ($:continue shift 
    . 53) ($:for shift . 54) ($:while shift . 55) ($:do shift . 56) ($:if 
    shift . 57) (Expression shift . 58) (#{$:;}# shift . 59) ($:var shift . 60
    ) (#{$:\x7b;}# shift . 61) (TryStatement shift . 63) (ThrowStatement shift
    . 64) (SwitchStatement shift . 65) (LabelledStatement shift . 66) (
    WithStatement shift . 67) (ReturnStatement shift . 68) (BreakStatement 
    shift . 69) (ContinueStatement shift . 70) (IterationStatement shift . 71)
    (IfStatement shift . 72) (ExpressionStatement shift . 73) (EmptyStatement
    shift . 74) (VariableStatement shift . 75) (Block shift . 76) (Statement 
    shift . 233) ($:default reduce . 177) ($:case reduce . 177) (#{$:\x7d;}# 
    reduce . 177))))

(define rto-v
  #($start Literal Literal Literal Literal NullLiteral BooleanLiteral 
    BooleanLiteral NumericLiteral NumericLiteral StringLiteral Identifier 
    PrimaryExpression PrimaryExpression PrimaryExpression PrimaryExpression 
    PrimaryExpression ArrayLiteral ArrayLiteral ArrayLiteral ArrayLiteral 
    ElementList ElementList ElementList ElementList Elision Elision 
    ObjectLiteral ObjectLiteral PropertyNameAndValueList 
    PropertyNameAndValueList PropertyName PropertyName PropertyName 
    MemberExpression MemberExpression MemberExpression MemberExpression 
    NewExpression NewExpression CallExpression CallExpression CallExpression 
    CallExpression Arguments Arguments ArgumentList ArgumentList 
    LeftHandSideExpression LeftHandSideExpression PostfixExpression 
    PostfixExpression PostfixExpression $P1 $P2 UnaryExpression 
    UnaryExpression UnaryExpression UnaryExpression UnaryExpression 
    UnaryExpression UnaryExpression UnaryExpression UnaryExpression 
    UnaryExpression MultiplicativeExpression MultiplicativeExpression 
    MultiplicativeExpression MultiplicativeExpression AdditiveExpression 
    AdditiveExpression AdditiveExpression ShiftExpression ShiftExpression 
    ShiftExpression ShiftExpression RelationalExpression RelationalExpression 
    RelationalExpression RelationalExpression RelationalExpression 
    RelationalExpression EqualityExpression EqualityExpression 
    EqualityExpression EqualityExpression EqualityExpression 
    BitwiseANDExpression BitwiseANDExpression BitwiseXORExpression 
    BitwiseXORExpression BitwiseORExpression BitwiseORExpression 
    LogicalANDExpression LogicalANDExpression LogicalORExpression 
    LogicalORExpression ConditionalExpression ConditionalExpression 
    AssignmentExpression AssignmentExpression AssignmentOperator 
    AssignmentOperator AssignmentOperator AssignmentOperator 
    AssignmentOperator AssignmentOperator AssignmentOperator 
    AssignmentOperator AssignmentOperator AssignmentOperator 
    AssignmentOperator AssignmentOperator Expression Expression ExpressionNoIn
    Statement Statement Statement Statement Statement Statement Statement 
    Statement Statement Statement Statement Statement Statement Statement 
    Block Block StatementList StatementList VariableStatement 
    VariableDeclarationList VariableDeclarationList 
    VariableDeclarationListNoIn VariableDeclaration VariableDeclaration 
    VariableDeclarationNoIn Initializer EmptyStatement ExpressionStatement 
    IfStatement IfStatement IterationStatement IterationStatement 
    IterationStatement IterationStatement IterationStatement 
    IterationStatement OptExprStmtNoIn OptExprStmtNoIn OptExprStmt OptExprStmt
    OptExprClose OptExprClose ContinueStatement ContinueStatement $P3 
    BreakStatement BreakStatement $P4 ReturnStatement ReturnStatement $P5 
    WithStatement SwitchStatement CaseBlock CaseBlock CaseBlock CaseBlock 
    CaseBlock CaseBlock CaseClauses CaseClauses CaseClause CaseClause 
    DefaultClause DefaultClause LabelledStatement ThrowStatement $P6 
    TryStatement TryStatement TryStatement Catch Finally FunctionDeclaration 
    FunctionDeclaration FunctionExpression FunctionExpression 
    FunctionExpression FunctionExpression FormalParameterList 
    FormalParameterList FunctionBody Program SourceElements SourceElements 
    SourceElement SourceElement))

(define mtab
  '(("function" . $:function) ("finally" . $:finally) ("catch" . $:catch) (
    "try" . $:try) ("throw" . $:throw) ("default" . $:default) ("case" . 
    $:case) ("switch" . $:switch) ("with" . $:with) ("return" . $:return) (
    "break" . $:break) ("continue" . $:continue) ("in" . $:in) ("for" . $:for)
    ("while" . $:while) ("do" . $:do) ("then" . $:then) ("else" . $:else) (
    "if" . $:if) (";" . #{$:;}#) ("var" . $:var) ("|=" . $:|=) ("^=" . $:^=) (
    "&=" . $:&=) (">>>=" . $:>>>=) (">>=" . $:>>=) ("<<=" . $:<<=) ("-=" . 
    $:-=) ("+=" . $:+=) ("%=" . $:%=) ("/=" . $:/=) ("*=" . $:*=) ("=" . $:=) 
    ("?" . $:?) ("||" . $:||) ("&&" . $:&&) ("|" . $:|) ("^" . $:^) ("&" . $:&
    ) ("!==" . $:!==) ("===" . $:===) ("!=" . $:!=) ("==" . $:==) (
    "instanceof" . $:instanceof) (">=" . $:>=) ("<=" . $:<=) (">" . $:>) ("<" 
    . $:<) (">>>" . $:>>>) (">>" . $:>>) ("<<" . $:<<) ("%" . $:%) ("/" . $:/)
    ("*" . $:*) ("!" . $:!) ("~" . $:~) ("-" . $:-) ("+" . $:+) ("typeof" . 
    $:typeof) ("void" . $:void) ("delete" . $:delete) ("--" . $:--) ("++" . 
    $:++) ("new" . $:new) ("." . $:.) (":" . $::) ("}" . #{$:\x7d;}#) ("{" . 
    #{$:\x7b;}#) ("," . $:,) ("]" . #{$:\x5d;}#) ("[" . #{$:\x5b;}#) (")" . 
    #{$:\x29;}#) ("(" . #{$:\x28;}#) ("this" . $:this) ($ident . $ident) (
    $string . $string) ($float . $float) ($fixed . $fixed) ("false" . $:false)
    ("true" . $:true) ("null" . $:null) ($end . $end)))

;;; end tables
