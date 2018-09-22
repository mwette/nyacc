# tcl-01.tcl

# Copyright (C) 2018 Matthew R. Wette
# 
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

# set/expr

set a 1
set b $a$a
set c [expr $a + $b]

# proc

proc _1pl {y} {
    set x 1
    set z [expr $x + $y]
    return $z
}

# if-else

set d 0
set e 0
set f 0
if {$a == 1} {
    set d 3
}

if {$a < 0} {
    set e -4
} else {
    set e 4
}

if {$a < 0} {
    set f 5
} elseif {$a == 1} {
    set f 6
} else {
    set f 7
}

# --- last line ---
