# SYNOPSIS
#
#   AX_LIB_NCCL()
#
# DESCRIPTION
#
#   This macro provides tests of the availability of NCCL library.
#
#   The macro adds a --with-nccl option accepting one of three values:
#
#     no   - do not check for the NCCL library.
#     yes  - do check for NCCL library in standard locations.
#     path - complete path to the NCCL root installation.
#
#   If NCCL is successfully found, this macro calls
#
#	AC_SUBST(NCCL_VERSION)
#	AC_SUBST(NCCL_CFLAGS)
#	AC_SUBST(NCCL_LDFLAGS)
#	AC_SUBST(NCCL_LIBS)
#	AC_SUBST(NCCL_FFLAGS)
#	AC_SUBST(NCCL_FLIBS)
#	AC_DEFINE(HAVE_NCCL)
#
#   and sets with_nccl="yes".
#
#   If NCCL is disabled or not found, this macros sets with_nccl="no".
#
#   NCCL_{C,CPP,LD}FLAGS may be used when building with C or C++.
#   NCCL_F{FLAGS,LIBS} should be used when building Fortran applications.
#
#   To use the macro, one would code the following in "configure.ac"
#   before AC_OUTPUT:
#
#        dnl Check for NCCL support
#        AX_LIB_NCCL()
#
#   One could test $with_nccl for the outcome or display it as follows
#
#     echo "NCCL support:  $with_nccl"
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

AC_DEFUN([AX_LIB_NCCL], [

AC_REQUIRE([AC_PROG_SED])
AC_REQUIRE([AC_PROG_AWK])
AC_REQUIRE([AC_PROG_GREP])

dnl Check first argument is one of the recognized values.
dnl Fail eagerly if is incorrect as this simplifies case statements below.
if   test "m4_normalize(m4_default([$1],[]))" = ""        ; then
    : # Recognized value
else
    AC_MSG_ERROR([
Unrecognized value for AX[]_LIB_NCCL within configure.ac.
For now, no argument is expected.
])
fi

dnl Add a default --with-nccl configuration option.
AC_ARG_WITH([nccl],
  AS_HELP_STRING(
    [--with-nccl=[yes/no/PATH]],
    m4_case(m4_normalize([$1]),
            [root of NCCL installation])
  ),
  [if test "$withval" = "no"; then
     with_nccl="no"
   elif test "$withval" = "yes"; then
     with_nccl="yes"
   else
     with_nccl="yes"
     NCCLPATH="$withval"
   fi],
   [with_nccl="yes"]
)

dnl Set defaults to blank
NCCL_VERSION=""
NCCL_CFLAGS=""
NCCL_CPPFLAGS=""
NCCL_LDFLAGS=""
NCCL_LIBS=""
NCCL_FFLAGS=""
NCCL_FLIBS=""

dnl Try and find NCCL configuration and options.
if test "$with_nccl" = "yes"; then
    AC_MSG_RESULT([checking for NCCL library...])

    dnl Define include and lib directories and check for header & informations
    NCCL_INCDIR="$NCCLPATH/include"
    NCCL_LIBDIR="$NCCLPATH/lib"

    NCCL_LIBS="-lnccl -L$NCCL_LIBDIR"

    dnl See if we can compile
    AC_LANG_PUSH([C])
    ax_lib_nccl_save_CPPFLAGS=$CPPFLAGS
    ax_lib_nccl_save_LIBS=$LIBS
    ax_lib_nccl_save_LDFLAGS=$LDFLAGS
    LIBS=$NCCL_LIBS
    CPPFLAGS=$"-I$NCCL_INCDIR"
    LDFLAGS=$NCCL_LIBS
    AC_CHECK_HEADER([nccl.h], [ac_cv_nccl_h=yes], [ac_cv_nccl_h=no])
    AC_CHECK_LIB([nccl], [ncclCommInitRank], [ac_cv_libncclcir=yes],
                     [ac_cv_libncclcir=no])
    AC_CHECK_LIB([nccl], [ncclSend], [ac_cv_libnccls=yes],
                     [ac_cv_libnccls=no])
    AC_CHECK_LIB([nccl], [ncclRecv], [ac_cv_libncclr=yes],
                     [ac_cv_libncclr=no])

    if test "$ac_cv_libncclcir" = "yes" && test "$ac_cv_libnccls" = "yes" \
                                        && test "$ac_cv_libncclr" = "yes" ; then
        ac_cv_libnccl=yes
    else
        ac_cv_libnccl=no
    fi

    if test "$ac_cv_nccl_h" = "no" && test "$ac_cv_libnccl" = "no" ; then
        AC_MSG_CHECKING([for NCCL library])
        AC_MSG_RESULT([no])
        AC_MSG_WARN([Unable to compile NCCL test program])
        with_nccl="no"
    else
        AC_MSG_CHECKING([for NCCL library])
        AC_MSG_RESULT([yes])

        NCCL_MAJOR=$($GREP NCCL_MAJOR $NCCL_INCDIR/nccl.h | $AWK '{print $[]NF}')
        NCCL_MINOR=$($GREP NCCL_MINOR $NCCL_INCDIR/nccl.h | $AWK '{print $[]NF}')
        NCCL_PATCH=$($GREP NCCL_PATCH $NCCL_INCDIR/nccl.h | $AWK '{print $[]NF}')
	NCCL_VERSION="$NCCL_MAJOR.$NCCL_MINOR.$NCCL_PATCH"
	AC_MSG_RESULT([NCCL version: $[NCCL_VERSION]])

        CPPFLAGS=$ax_lib_nccl_save_CPPFLAGS
        LIBS=$ax_lib_nccl_save_LIBS
        LDFLAGS=$ax_lib_nccl_save_LDFLAGS
        AC_LANG_POP([C])
    
        NCCL_CFLAGS="-I$NCCL_INCDIR"
        NCCL_CPPFLAGS="-I$NCCL_INCDIR"
        NCCL_FFLAGS="-I$NCCL_INCDIR"
        NCCL_LDFLAGS="$NCCL_LIBS"
        NCCL_FLIBS="$NCCL_LIBS"
    
        AC_SUBST([NCCL_VERSION])
        AC_SUBST([NCCL_CFLAGS])
        AC_SUBST([NCCL_LDFLAGS])
        AC_SUBST([NCCL_LIBS])
        AC_SUBST([NCCL_FFLAGS])
        AC_SUBST([NCCL_FLIBS])
        AC_DEFINE([HAVE_NCCL], [1], [Defined if you have NCCL support])
    fi
fi
])
