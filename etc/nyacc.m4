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
 [AC_REQUIRE([GUILE_PKG])
  AC_MSG_CHECKING(for Guile site directory)
  GUILE_SITE_GO=`$PKG_CONFIG --print-errors --variable=siteccachedir guile-$GUILE_EFFECTIVE_VERSION`
  AC_MSG_RESULT($GUILE_SITE)
  if test "$GUILE_SITE" = ""; then
     AC_MSG_FAILURE(sitedir not found)
  fi
  AC_SUBST(GUILE_SITE_GO)
 ])

# --- last line ---
