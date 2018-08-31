#!/bin/sh
# source this file: $ . env.sh
topdir=`(cd ..; pwd)`
if [ "X$GUILE_LOAD_PATH" = "X" ]; then
 GUILE_LOAD_PATH=$topdir/module
else
 GUILE_LOAD_PATH=$topdir/module:$GUILE_LOAD_PATH
fi;
GUILE_LOAD_PATH=$topdir/examples:$GUILE_LOAD_PATH
GUILE_LOAD_PATH=$topdir/test-suite:$GUILE_LOAD_PATH
export GUILE_LOAD_PATH
