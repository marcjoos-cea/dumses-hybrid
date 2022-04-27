# SYNOPSIS
#
#   AX_LIB_PNETCDF()
#
# DESCRIPTION
#
#   This macro provides tests of the availability of Parallel NetCDF library.
#
#   The macro adds a --with-pnetcdf option accepting one of three values:
#
#     no   - do not check for the Parallel NetCDF library.
#     yes  - do check for Parallel NetCDF library in standard locations.
#     path - complete path to the Parallel NetCDF root installation.
#
#   If Parallel NetCDF is successfully found, this macro calls
#
#	AC_SUBST(PNETCDF_VERSION)
#	AC_SUBST(PNETCDF_CC)
#	AC_SUBST(PNETCDF_CFLAGS)
#	AC_SUBST(PNETCDF_LDFLAGS)
#	AC_SUBST(PNETCDF_LIBS)
#	AC_SUBST(PNETCDF_FFLAGS)
#	AC_SUBST(PNETCDF_FLIBS)
#	AC_DEFINE(HAVE_PNETCDF)
#
#   and sets with_pnetcdf="yes".  Additionally, the macro sets
#   with_pnetcdf_fortran="yes" if a matching Fortran wrapper script is found.
#   Note that Autoconf's Fortran support is not used to perform this check.
#   pnetcdf-config script will be used to retrieve this information.
#
#   If Parallel NetCDF is disabled or not found, this macros sets with_pnetcdf="no" and
#   with_pnetcdf_fortran="no".
#
#   PNETCDF_{C,CPP,LD}FLAGS may be used when building with C or C++.
#   PNETCDF_F{FLAGS,LIBS} should be used when building Fortran applications.
#
#   To use the macro, one would code the following in "configure.ac"
#   before AC_OUTPUT:
#
#        dnl Check for Parallel NetCDF support
#        AX_LIB_PNETCDF()
#
#   One could test $with_pnetcdf for the outcome or display it as follows
#
#     echo "Parallel NetCDF support:  $with_pnetcdf"
#
#
# LICENSE
#
#   Copyright (c) 2022 Marc Joos <marc.joos@cea.fr>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

AC_DEFUN([AX_LIB_PNETCDF], [

AC_REQUIRE([AC_PROG_SED])
AC_REQUIRE([AC_PROG_AWK])
AC_REQUIRE([AC_PROG_GREP])

dnl Check first argument is one of the recognized values.
dnl Fail eagerly if is incorrect as this simplifies case statements below.
if   test "m4_normalize(m4_default([$1],[]))" = ""        ; then
    : # Recognized value
else
    AC_MSG_ERROR([
Unrecognized value for AX[]_LIB_PNETCDF within configure.ac.
For now, no argument is expected.
])
fi

dnl Add a default --with-pnetcdf configuration option.
AC_ARG_WITH([pnetcdf],
  AS_HELP_STRING(
    [--with-pnetcdf=[yes/no/PATH]],
    m4_case(m4_normalize([$1]),
            [root of Parallel NetCDF installation])
  ),
  [if test "$withval" = "no"; then
     with_pnetcdf="no"
   elif test "$withval" = "yes"; then
     with_pnetcdf="yes"
   else
     with_pnetcdf="yes"
     PNETCDFPATH="$withval"
   fi],
   [with_pnetcdf="yes"]
)

dnl Set defaults to blank
PNETCDF_VERSION=""
PNETCDF_CC=""
PNETCDF_CFLAGS=""
PNETCDF_CPPFLAGS=""
PNETCDF_LDFLAGS=""
PNETCDF_LIBS=""
PNETCDF_FFLAGS=""
PNETCDF_FLIBS=""

dnl Try and find Parallel NetCDF tools and options.
if test "$with_pnetcdf" = "yes"; then
    if test -z "$PNETCDFCONFIG"; then
        dnl Check to see if pnetcdf-config is in the path.
	PNETCDFCONFIG=$PNETCDFPATH/bin/pnetcdf-config
        AC_PATH_PROGS(
            [PNETCDFCONFIG],
            m4_case(m4_normalize([$1]),
                [bin/pnetcdf-config]),
            [])
    else
        AC_MSG_CHECKING([Using provided pnetcdf-config script])
        AC_MSG_RESULT([$PNETCDFCONFIG])
    fi
    AC_MSG_CHECKING([for Parallel NetCDF library])
    if test ! -f "$PNETCDFCONFIG" || test ! -x "$PNETCDFCONFIG"; then
        AC_MSG_RESULT([no])
        AC_MSG_WARN(m4_case(m4_normalize([$1]),
            [
Unable to locate Parallel NetCDF script 'pnetcdf-config'. Please
specify --with-pnetcdf=<LOCATION> as the installation path of
Parallel NetCDF installation. 
Parallel NetCDF support is being disabled (equivalent to --with-pnetcdf=no).
]))
        with_pnetcdf="no"
        with_pnetcdf_fortran="no"
    else
        dnl Get the pnetcdf-config output
	PNETCDFCONFIG_SHOW=$(eval $PNETCDFCONFIG --all)

        dnl Get the actual compiler used
	PNETCDF_CC=$(eval $PNETCDFCONFIG --all | $GREP '\-cc' | $AWK '{print $[]NF}')

	dnl Check if it has Fortran support
	with_pnetcdf_fortran=$(eval $PNETCDFCONFIG --all | $GREP '\-has\-fortran' | $AWK '{print $[]NF}')

        dnl Look for "--version -> PnetCDF X.Y.Z"
        PNETCDF_VERSION=$(eval $PNETCDFCONFIG --all | $GREP 'version' | $AWK '{print $[]NF}')

	dnl Look for includedir
	PNETCDF_INCDIR=$(eval $PNETCDFCONFIG --all | $GREP '\-includedir' | $AWK '{print $[]NF}')

	dnl Look for libdir
	PNETCDF_LIBDIR=$(eval $PNETCDFCONFIG --all | $GREP '\-libdir' | $AWK '{print $[]NF}')

        PNETCDF_LIBS="-lpnetcdf -L$PNETCDF_LIBDIR"
        AC_MSG_RESULT([yes (version $[PNETCDF_VERSION])])

        dnl See if we can compile
        AC_LANG_PUSH([C])
        ax_lib_pnetcdf_save_CC=$CC
        ax_lib_pnetcdf_save_CPPFLAGS=$CPPFLAGS
        ax_lib_pnetcdf_save_LIBS=$LIBS
        ax_lib_pnetcdf_save_LDFLAGS=$LDFLAGS
	CC=$PNETCDF_CC
        LIBS=$PNETCDF_LIBS
	CPPFLAGS=$"-I$PNETCDF_INCDIR"
        LDFLAGS=$PNETCDF_LIBS
        AC_CHECK_HEADER([pnetcdf.h], [ac_cv_pnetcdf_h=yes], [ac_cv_pnetcdf_h=no])
        AC_CHECK_LIB([pnetcdf], [ncmpi_create], [ac_cv_libpnetcdf=yes],
                     [ac_cv_libpnetcdf=no])
        if test "$ac_cv_pnetcdf_h" = "no" && test "$ac_cv_libpnetcdf" = "no" ; then
          AC_MSG_WARN([Unable to compile Parallel NetCDF test program])
	  with_pnetcdf="no"
	  with_pnetcdf_fortran="no"
        fi

        CC=$ax_lib_pnetcdf_save_CC
        CPPFLAGS=$ax_lib_pnetcdf_save_CPPFLAGS
        LIBS=$ax_lib_pnetcdf_save_LIBS
        LDFLAGS=$ax_lib_pnetcdf_save_LDFLAGS
        AC_LANG_POP([C])

	PNETCDF_CFLAGS="-I$PNETCDF_INCDIR"
	PNETCDF_CPPFLAGS="-I$PNETCDF_INCDIR"
	PNETCDF_FFLAGS="-I$PNETCDF_INCDIR"
	PNETCDF_LDFLAGS="$PNETCDF_LIBS"
	PNETCDF_FLIBS="$PNETCDF_LIBS"

	AC_SUBST([PNETCDF_VERSION])
	AC_SUBST([PNETCDF_CC])
	AC_SUBST([PNETCDF_CFLAGS])
	AC_SUBST([PNETCDF_LDFLAGS])
	AC_SUBST([PNETCDF_LIBS])
	AC_SUBST([PNETCDF_FFLAGS])
	AC_SUBST([PNETCDF_FLIBS])
	AC_DEFINE([HAVE_PNETCDF], [1], [Defined if you have Parallel NetCDF support])
    fi
fi
])
