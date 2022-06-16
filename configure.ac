AC_PREREQ([2.63])
AC_INIT([DUMSES-HYBRID], [2.1], [marc.joos@cea.fr])
AC_CONFIG_SRCDIR([src/])
AC_LANG([Fortran])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([-Wall] [subdir-objects])
LT_INIT

dnl Arguments passing
AC_ARG_ENABLE([mpi],
  [AS_HELP_STRING([--enable-mpi], [Enable MPI (default is yes).])],
  [],
  [enable_mpi=yes])

AC_ARG_ENABLE([openmp],
  [AS_HELP_STRING([--enable-openmp], [Enable OpenMP (default is yes).])],
  [],
  [enable_openmp=yes])

AC_ARG_ENABLE([openacc],
  [AS_HELP_STRING([--enable-openacc], [Enable OpenACC (default is no).])],
  [],
  [enable_openacc=no])

AC_ARG_WITH([hdf5],
  [AS_HELP_STRING([--with-hdf5@<:@gHDF5 library path@:>@], [Check for HDF5 support])],
  [],
  []
  )

AC_ARG_WITH([pnetcdf],
  [AS_HELP_STRING([--with-pnetcdf@<:@gParallel NetCDF library path@:>@], [Check for Parallel NetCDF support])],
  [],
  []
  )

AC_ARG_WITH([nccl],
  [AS_HELP_STRING([--with-nccl@<:@NCCL library path@:>@], [Check for NCCL support. If it is used, communications will be done with NCCL library instead of MPI library.])],
  [],
  []
  )

AC_ARG_WITH([cuda-aware-mpi],
  [AS_HELP_STRING([--with-cuda-aware-mpi@<:@[0,1]@:>@], [Activate/deactivate CUDA-aware MPI communications on GPU])],
  [],
  [with_cuda_aware_mpi=1]
  )

AC_ARG_WITH([problem],
  [AS_HELP_STRING([--with-problem@<:@user defined problem@:>@], [Provide problem to compile])],
  [],
  [with_problem=magnetic_loop]
  )

AC_ARG_WITH([ndim],
  [AS_HELP_STRING([--with-ndim@<:@number of dimensions@:>@], [Provide the number of dimensions to use])],
  [],
  [with_ndim=3]
  )

AC_ARG_WITH([iso],
  [AS_HELP_STRING([--with-iso@<:@[0,1]@:>@], [Activate/deactivate isothermal computation])],
  [],
  [with_iso=0]
  )

AC_ARG_WITH([geometry],
  [AS_HELP_STRING([--with-geometry@<:@geometry@:>@], [Provide geometry to use])],
  [],
  [with_geometry=cartesian]
  )

dnl We'll work in Fortran
AC_PROG_FC([gfortran ifort nvfortran])
AM_CONDITIONAL([PGIFC], [test x$FC = xnvfortran])

dnl Set default flags if nothing is done by the user
AC_SUBST([AM_FCFLAGS], [])
AC_SUBST([AM_LDFLAGS], [])

AS_IF([test x$FC = xgfortran -a x$ac_cv_env_FCFLAGS_set = x],
  AC_SUBST([FCFLAGS], ["-g -O3 -fbacktrace"])
)
AS_IF([test x$FC = xifort -a x$ac_cv_env_FCFLAGS_set = x],
  AC_SUBST([FCFLAGS], ["-g -O3 -traceback"])
)
AS_IF([test x$FC = xnvfortran -a x$ac_cv_env_FCFLAGS_set = x],
  AC_SUBST([FCFLAGS], ["-g -O3 -traceback"])
)

dnl Define which compiler family we are currently using
AM_CONDITIONAL([GCC], [test x$FC = xgfortran])
AM_CONDITIONAL([INTEL], [test x$FC = xifort])
AM_CONDITIONAL([NVIDIA], [test x$FC = xnvfortran])
AS_IF([test x$FC = xgfortran], 
  [CC=gcc
  AC_SUBST([MODFLAG], ["-J"])
  AC_SUBST([FPPFLAGS], [" -x f95-cpp-input"])
  AC_SUBST([AM_FCFLAGS], ["-ffree-line-length-0"])
  ])
AS_IF([test x$FC = xifort], 
  [CC=icc
  AC_SUBST([MODFLAG], ["-module "])
  AC_SUBST([FPPFLAGS], ["-fpp"])
  ])
AS_IF([test x$FC = xnvfortran], 
  [CC=nvc
  AC_SUBST([MODFLAG], ["-module "])
  AC_SUBST([FPPFLAGS], ["-Mpreprocess"])
  ])

dnl Fortran checks
AC_FC_FREEFORM
AC_FC_CHECK_BOUNDS

dnl In the following, we define variables to custom preprocessing flags
dnl instead of using AC_DEFINE, mostly because we don't need default
dnl preprocessing variables defined in @DEFS@

dnl Check for OpenACC; define:
dnl  - OACC for preprocessing
dnl  - ENABLE_OPENACC for Makefiles
AC_SUBST([OACC], [0])
AS_IF([test "x$enable_openacc" != xno], [
  AC_LANG_PUSH([C])
  AC_CHECK_HEADERS([openacc.h], [HAVE_OPENACC=1],
    [AC_MSG_ERROR([cannot include openacc.h])])
  AC_LANG_PUSH([Fortran])
  AC_SUBST([OACC], [1])
])
AM_CONDITIONAL([ENABLE_OPENACC], [test x$HAVE_OPENACC = x1])

dnl Check for OpenMP; define:
dnl  - ENABLE_OPENMP for Makefiles
AS_IF([test x$enable_openmp != xno], [
  AC_OPENMP
])
AS_IF([test x$OPENMP_FCFLAGS != x], [
  HAVE_OPENMP=1
  AS_IF([test "x$HAVE_OPENMP" = x1], [
    AS_IF([test "x$HAVE_OPENACC" != x1], [ENABLE_OPENMP=1])
  ])
])
AM_CONDITIONAL([ENABLE_OPENMP], [test "x$ENABLE_OPENMP" = x1])

dnl Check for MPI; define:
dnl  - MPI for preprocessing
dnl  - redefine FC compiler
AC_SUBST([MPI], [0])
AS_IF([test x$enable_mpi != xno], [
  LX_FIND_MPI()
  AS_IF([test "x$MPIFC" != x], [
    AS_IF([test "x$MPIFC" != xnot-found], [
      AC_SUBST([MPI], [1])
      AC_SUBST([FC], [$MPIFC])
    ])
  ])
])

dnl Check for HDF5
dnl  - first, check if we have parallel HDF5 library
dnl  - if it fails, check if we have serial HDF5 library
AC_SUBST([HDF5], [0])
AC_SUBST([PHDF5], [0])
AS_IF([test x$with_hdf5 != x], [
  AC_SUBST([hdf5_path], [$with_hdf5])
  AC_SUBST([with_hdf5], [$hdf5_path/bin/h5pcc])
  AX_LIB_HDF5()
  AS_IF([test x$with_hdf5 = xno],
        [AC_SUBST([with_hdf5], [$hdf5_path/bin/h5cc])
         AX_LIB_HDF5()],
        [test x$with_hdf5 = xyes],
        [AS_IF([test x$HDF5_TYPE = xserial],
	       [AC_SUBST([HDF5], [1])],
               [test x$HDF5_TYPE = xparallel],
	       [AC_SUBST([PHDF5], [1])])
  ])
])

dnl Check for Parallel NetCDF
AC_SUBST([PNCDF], [0])
AS_IF([test x$with_pnetcdf != x], [
  AX_LIB_PNETCDF()
  AS_IF([test x$with_pnetcdf_fortran = xyes],
        [AC_SUBST([PNCDF], [1])])
])

dnl Check for NCCL
AC_SUBST([NCCL], [0])
AS_IF([test x$with_nccl != x], [
  AX_LIB_NCCL()
  AS_IF([test x$with_nccl = xyes],
    	[AC_SUBST([NCCL], [1])])
])

dnl TODO: need to check if mpi is cuda-aware
dnl  - for openmpi: use ompi_info ?
dnl  - can mpich be cuda-aware?
dnl For now we suppose that if MPI & OpenACC are defined
dnl one will want to use CUDA-aware MPI

dnl Check if CUDA-aware MPI communications are activated on GPU
AC_SUBST([CUDA_AWARE_MPI], [0])
AS_IF([test x$with_cuda_aware_mpi != x], [
  AS_IF([test "x$MPIFC" != x], [
    AS_IF([test "x$HAVE_OPENACC" = x1], [
      AC_SUBST([CUDA_AWARE_MPI], [1])],
      [AC_MSG_WARN([You are not using MPI or OpenACC; deactivate CUDA-aware MPI])]
    )])
])

dnl Define number of dimensions of the problem
AS_CASE([$with_ndim],
	[1], [AC_SUBST([NDIM], [1])],
	[2], [AC_SUBST([NDIM], [2])],
	[3], [AC_SUBST([NDIM], [3])],
	[AC_MSG_WARN([Unrecognized value for '--with-ndim', setting default to '3'])
	AC_SUBST([NDIM], [3])])

dnl Define if computation is isothermal
AS_CASE([$with_iso],
	[0], [AC_SUBST([ISO], [0])],
	[1], [AC_SUBST([ISO], [1])],
	[AC_MSG_WARN([Unrecognized value for '--with-iso', setting default to '0'])
	AC_SUBST([ISO], [0])])

dnl Define geometry of the problem
AS_CASE([$with_geometry],
	[cartesian],   [AC_SUBST([GEOM], [1])],
	[cylindrical], [AC_SUBST([GEOM], [2])],
	[spherical],   [AC_SUBST([GEOM], [3])],
	[AC_MSG_WARN([Unrecognized value for '--with-geometry', setting default to 'cartesian'])
	AC_SUBST([GEOM], [1])]])

dnl All filenames are retrieve without their directories, so they
dnl will be compiled directly in src/. This is needed to ensure
dnl that files will be compiled in the right order and that
dnl unnecessary files won't be compiled
dnl FYI -printf "%f " could be replaced by printf "%p " if we
dnl would need the full path

dnl Check for sources
AC_MSG_CHECKING([for source files])
SRCFILES=$(find "src" -maxdepth 1 -name "*f90" -type f -printf "%f ")
AC_SUBST([SRCFILES])
AC_MSG_RESULT([$SRCFILES])

dnl Check for subroutines
AC_MSG_CHECKING([for subroutine files])
SUBFILES=$(find "src/subroutines" -maxdepth 1 -name "*f90" -type f -printf "%f ")
AC_SUBST([SUBFILES])
AC_MSG_RESULT([$SUBFILES])

dnl Check for user files
AC_MSG_CHECKING([for user files])
USRFILES=$(find "src/userfiles" -maxdepth 1 -name "*f90" -type f -printf "%f ")
AC_SUBST([USRFILES])
AC_MSG_RESULT([$USRFILES])

dnl Check for modules and submodules
AC_MSG_CHECKING([for module files])
MODFILES=$(find "src/modules" -maxdepth 1 -name "*f90" -type f -printf "%f ")
AC_SUBST([MODFILES])
AC_MSG_RESULT([$MODFILES])

AC_MSG_CHECKING([for module subdirectories])
_MODSUBDIRS=$(find "src/modules" -mindepth 1 -maxdepth 1 -type d -printf "%p ")
MODSUBDIRS=$(find "src/modules" -mindepth 1 -maxdepth 1 -type d -printf "%p " | sed "s/src\///g")
AC_SUBST([MODSUBDIRS])
AC_MSG_RESULT([$MODSUBDIRS])

AS_IF([test "x$_MODSUBDIRS" != x], [
  AC_MSG_CHECKING([for files in module subdirectories])
  MODSUBFILES=$(echo "$_MODSUBDIRS" | sed "s/\n/ /" | while read dirs; do find $dirs -maxdepth 1 -name "*f90" -type f -printf "%f "; done)
  AC_SUBST([MODSUBFILES])
  AC_MSG_RESULT([$MODSUBFILES])
])

dnl Define problem to compile
AC_MSG_CHECKING([for problem])
_PRBDIR=$(find src/problem -name "$with_problem")
PRBDIR=$(find src/problem -name "$with_problem" | sed "s/src\///g")
AC_SUBST([PRBDIR])
AC_MSG_RESULT([$PRBDIR])

AC_MSG_CHECKING([for problem files])
PRBFILES=$(find "$_PRBDIR" -maxdepth 1 -name "*f90" -type f -printf "%f ")
AC_SUBST([PRBFILES])
AC_MSG_RESULT([$PRBFILES])

AC_MSG_CHECKING([for problem subdirectories])
_PRBSUBDIRS=$(find "$_PRBDIR" -mindepth 1 -maxdepth 1 -type d -printf "%p ")
PRBSUBDIRS=$(find "$_PRBDIR" -mindepth 1 -maxdepth 1 -type d -printf "%p " | sed "s/src\///g")
AC_SUBST([PRBSUBDIRS])
AC_MSG_RESULT([$PRBSUBDIRS])

AS_IF([test "x$_PRBSUBDIRS" != x], [
  AC_MSG_CHECKING([for files in problem subdirectories])
  PRBSUBFILES=$(echo "$_PRBSUBDIRS" | sed "s/\n/ /" | while read dirs; do find $dirs -maxdepth 1 -name "*f90" -type f -printf "%f "; done)
  AC_SUBST([PRBSUBFILES])
  AC_MSG_RESULT([$PRBSUBFILES])
])

AC_CONFIG_FILES([Makefile
  src/Makefile])
AC_OUTPUT

