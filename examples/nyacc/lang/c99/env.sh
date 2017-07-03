#!/bin/sh
# source this file: $ . env.sh
(cd ../../../..;
 if [ "X$GUILE_LOAD_PATH" = "X" ]; then
  GUILE_LOAD_PATH=`pwd`/module:`pwd`/examples
 else
  GUILE_LOAD_PATH=`pwd`/module:`pwd`/examples:$GUILE_LOAD_PATH
 fi;
 export GUILE_LOAD_PATH)
