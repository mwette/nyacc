dnl configure.ac for guile-nyacc

dnl Copyright (C) 2019-2020 Matthew R. Wette
dnl
dnl Copying and distribution of this file, with or without modification,
dnl are permitted in any medium without royalty provided the copyright
dnl notice and this notice are preserved.  This file is offered as-is,
dnl without any warranty.

AC_INIT([nyacc], [1.00.1], [], [], [https://savannah.nongnu.org/project/nyacc])
AC_CONFIG_SRCDIR(module/nyacc/version.scm)
AC_CONFIG_AUX_DIR([etc])
AC_CONFIG_MACRO_DIR([etc])

m4_include([./pkg.m4])
m4_include([./guile.m4])
m4_include([./nyacc.m4])

AC_ARG_WITH(guile, [    --with-guile            path to binary],
            ,with_guile=unspecified)

if test "_$with_guile" != "_unspecified"; then
  GUILE=$with_guile
  GUILE_EFFECTIVE_VERSION=`$GUILE -c "(display (effective-version))"`
  guile_libdir=`$GUILE -c "(display (assq-ref %guile-build-info 'libdir))"`
  PKG_CONFIG_LIBDIR=$guile_libdir/pkgconfig
  PKG_CONFIG=pkg-config
  export PKG_CONFIG_LIBDIR
else
  GUILE_PKG([3.2 3.0 2.9 2.2 2.0])
  GUILE_PROGS
fi

GUILE_SITE_DIR
GUILE_SITE_GO_DIR
GUILE_DATA_DIR

GUILE_MODULE_AVAILABLE([have_bytestructures],[(bytestructures guile)])
if test $have_bytestructures = "yes"; then
  NYACC_FH_BINS='$(NYACC_FH_BINS)'
else
  NYACC_FH_BINS=''
fi
AC_SUBST([NYACC_FH_BINS])

dnl needed by etc/guild:
AC_SUBST([installed_guile],$GUILE)

AC_CONFIG_FILES([Makefile \
	module/Makefile \
	doc/nyacc/Makefile \
	test-suite/Makefile \
	examples/Makefile \
])
AC_CONFIG_FILES([etc/guild],[chmod +x etc/guild])

AC_OUTPUT

dnl --- last line ---
