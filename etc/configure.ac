dnl configure.ac for guile-nyacc

dnl Copyright (C) 2019-2020 Matthew R. Wette
dnl
dnl Copying and distribution of this file, with or without modification,
dnl are permitted in any medium without royalty provided the copyright
dnl notice and this notice are preserved.  This file is offered as-is,
dnl without any warranty.

AC_INIT([nyacc], [1.06.7], [], [], [https://savannah.nongnu.org/project/nyacc])
AC_CONFIG_SRCDIR(module/nyacc/version.scm)
AC_CONFIG_AUX_DIR([etc])
AC_CONFIG_MACRO_DIR([etc])

AC_ARG_WITH(guile,  [  --with-guile            path to binary],
            ,with_guile=unspecified)
AC_ARG_WITH(target, [  --with-target           guild target flag],
            ,target=unspecified)
AC_ARG_ENABLE(debug,[  --enable-debug          build for debugging in guile],
            ,debug=unspecified)

if test "_$with_guile" != "_unspecified"; then
  GUILE=$with_guile
else
  AC_PATH_PROGS(GUILE,[guile guile-3.0 guile-2.2 guile-2.0])
  if test -z "$GUILE"; then
    AC_MSG_ERROR([guile required but not found])
  fi
fi
GUILE_EFFECTIVE_VERSION=`$GUILE -c "(display (effective-version))"`
guile_libdir=`$GUILE -c "(display (assq-ref %guile-build-info 'libdir))"`
guile_prefix=`$GUILE -c "(display (assq-ref %guile-build-info 'prefix))"`

if test "$guile_prefix" == "$prefix"; then
  echo "using paths for installed guile"
  GUILE_GLOBAL_SITE=`$GUILE -c '(display (%global-site-dir))'`
  GUILE_SITE_CCACHE=`$GUILE -c '(display (%site-ccache-dir))'`
  GUILE_DATA=`$GUILE -c "(display (assq-ref %guile-build-info 'datadir))"`
else
  echo "using default paths for guile"
  GUILE_GLOBAL_SITE=$prefix/share/guile/site
  GUILE_SITE_CCACHE=$prefix/lib/guile/$GUILE_EFFECTIVE_VERSION/site-ccache
  GUILE_DATA=$prefix/share
fi

if test "_$target" != "_unspecified"; then
  GUILE_TARGET=$target
else
  GUILE_TARGET=
fi
if test "_$debug" != "_unspecified"; then
  GUILE_DEBUG=-O0
else
  GUILE_DEBUG=
fi
AC_SUBST([GUILE_TARGET])
AC_SUBST([GUILE_DEBUG])

AC_SUBST([GUILE_GLOBAL_SITE])
AC_SUBST([GUILE_SITE_CCACHE])
AC_SUBST([GUILE_DATA])

AC_SUBST([GUILE_LOAD_PATH])
AC_SUBST([GUILE_LOAD_COMPILED_PATH])

AC_MSG_CHECKING([if (bytestructures guile) is available])
if $GUILE -c "(use-modules (bytestructures guile)) (exit ((lambda () 0)))" \
	>/dev/null 2>&1; then
  res=yes;
  NYACC_FH_BINS='$(NYACC_FH_BINS)'
else
  res=no;
  NYACC_FH_BINS=''
fi
AC_MSG_RESULT($res)
AC_SUBST([NYACC_FH_BINS])

dnl needed by etc/guild:
AC_SUBST([installed_guile],$GUILE)

AC_CONFIG_FILES([Makefile \
	module/Makefile \
	doc/Makefile \
	test-suite/Makefile \
	examples/Makefile \
])
AC_CONFIG_FILES([etc/guild],[chmod +x etc/guild])

AC_OUTPUT

dnl --- last line ---
