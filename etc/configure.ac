dnl configure.ac for guile-nyacc

dnl Copyright (C) 2019 Matthew R. Wette
dnl
dnl Copying and distribution of this file, with or without modification,
dnl are permitted in any medium without royalty provided the copyright
dnl notice and this notice are preserved.  This file is offered as-is,
dnl without any warranty.

AC_INIT([nyacc], [1.00.0], [], [], [https://savannah.nongnu.org/project/nyacc])
AC_CONFIG_SRCDIR(module/nyacc)
AC_CONFIG_AUX_DIR([etc])
AC_CONFIG_MACRO_DIR([etc])

m4_include([./pkg.m4])
m4_include([./guile.m4])
m4_include([./nyacc.m4])

GUILE_PKG([3.2 3.0 2.9 2.2 2.0])
GUILE_PROGS
GUILE_SITE_DIR
GUILE_SITE_GO_DIR

GUILE_DATADIR=`$GUILE -c "(display (assq-ref %guile-build-info 'datadir))"`
AC_SUBST([GUILE_DATADIR])

GUILE_MODULE_AVAILABLE([have_bytestructures],[(bytestructures guile)])
if test $have_bytestructures = "yes"; then
  NYACC_FH_BINS='$(NYACC_FH_BINS)'
else
  NYACC_FH_BINS=''
fi
AC_SUBST([NYACC_FH_BINS])

AC_CONFIG_FILES([Makefile \
	module/Makefile \
	doc/nyacc/Makefile \
	test-suite/nyacc/Makefile \
	test-suite/nyacc/lang/Makefile \
	test-suite/nyacc/lang/c99/Makefile \
	examples/Makefile \
])

AC_OUTPUT

dnl --- last line ---
