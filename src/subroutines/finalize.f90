!===============================================================================
!> \file finalize.f90
!! \brief
!! \b DUMSES-Hybrid:
!! This is finalize subroutines.
!! \details
!! Contains deallocate_workspace()
!! \author
!! Marc Joos <marc.joos@cea.fr>, Sébastien Fromang, Romain Teyssier, 
!! Patrick Hennebelle
!! \copyright
!! Copyrights 2013-2015, CEA.
!! This file is distributed under the CeCILL-A & GNU/GPL licenses, see
!! <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html> and
!! <http://www.gnu.org/licenses/>
!! \date
!! \b created:          04-15-2013 
!! \b last \b modified: 01-06-2015
!<
!===============================================================================
!> Deallocate arrays
!===============================================================================
subroutine deallocate_workspace
  use variables
  use params
  use mpi_var
  implicit none

  !$py begin_statement

  !$acc exit data delete(slbound, srbound, rlbound, rrbound)
  deallocate(slbound, srbound, rlbound, rrbound)
  
  !$acc exit data delete(qm, qp, qRT, qRB, qLT, qLB, gravin, dq, bfc, dbfc, &
  !$acc                  flux, emfx, emfy, emfz, fgodunov, fgodunov_pre)
  deallocate(uin, qin, gravin, flux)
  deallocate(emfx, emfy, emfz)
  deallocate(x, y, z)
  deallocate(dv, ds)
#if NDIM == 3
  !$acc exit data delete(Ex, Ey)
  deallocate(Ex, Ey)
#endif
#if NDIM > 1
  !$acc exit data delete(Ez)
  deallocate(Ez)
#endif
  deallocate(bfc, dq, dbfc)
  deallocate(qm, qp, qRT, qRB, qLT, qLB)
  deallocate(fgodunov, fgodunov_pre)
  if (rhs .or. fargo) deallocate(uin_old)

  return
end subroutine deallocate_workspace
