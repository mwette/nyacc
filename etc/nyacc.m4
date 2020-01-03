dnl nyacc.m4
dnl
dnl Copyright (C) 2019 Matthew R. Wette
dnl
dnl Copying and distribution of this file, with or without modification,
dnl are permitted in any medium without royalty provided the copyright
dnl notice and this notice are preserved.  This file is offered as-is,
dnl without any warranty.

dnl GUILE_SITE_GO_DIR
AC_DEFUN([GUILE_SITE_GO_DIR],
 [AC_MSG_CHECKING(for Guile site directory)
  GUILE_SITE_GO=`$GUILE -c '(display (%site-ccache-dir))'`
  AC_MSG_RESULT($GUILE_SITE_GO)
  if test "$GUILE_SITE_GO" = ""; then
     AC_MSG_FAILURE(sitedir not found)
  fi
  AC_SUBST(GUILE_SITE_GO)
 ])

dnl GUILE_DATA_DIR
AC_DEFUN([GUILE_DATA_DIR],
 [AC_MSG_CHECKING(for Guile data directory)
  GUILE_DATA=`$GUILE -c "(display (assq-ref %guile-build-info 'datadir))"`
  AC_MSG_RESULT($GUILE_DATA)
  if test "$GUILE_DATA" = ""; then
     AC_MSG_FAILURE(datadir not found)
  fi
  AC_SUBST(GUILE_DATA)
 ])


# --- last line ---
