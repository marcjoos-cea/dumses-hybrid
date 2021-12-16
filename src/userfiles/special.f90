!===============================================================================
!> \file special.f90
!! \brief
!! \b DUMSES-Hybrid:
!! This is special subroutines.
!! \details
!! Contains special()
!! \copyright
!! Copyrights 2013-2021, CEA.
!! This file is distributed under the CeCILL-A & GNU/GPL licenses, see
!! <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html> and
!! <http://www.gnu.org/licenses/>
!<
!===============================================================================
!> \brief
!! This routine produces special output for the simulation if dtspec > 0.
!===============================================================================
subroutine special
#if MPI == 1
  use mpi
#endif
  use params
  use variables
  use mpi_var
  use file
#if PHDF5 == 1 || HDF5 == 1
  use hdf5
  use write_h5
#endif
#if PNCDF == 1
  use pnetcdf
#endif
  implicit none

  return
end subroutine special
