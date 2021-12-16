!===============================================================================
!> \file finalize.f90
!! \brief
!! \b DUMSES-Hybrid:
!! This is finalize subroutines.
!! \details
!! Contains deallocate_workspace()
!! \copyright
!! Copyrights 2013-2021, CEA.
!! This file is distributed under the CeCILL-A & GNU/GPL licenses, see
!! <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html> and
!! <http://www.gnu.org/licenses/>
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

  !$acc exit data delete(slbound_x, srbound_x, rlbound_x, rrbound_x, &
  !$acc                  slbound_y, srbound_y, rlbound_y, rrbound_y, &
  !$acc                  slbound_z, srbound_z, rlbound_z, rrbound_z)
  deallocate(slbound_x, srbound_x, rlbound_x, rrbound_x, &
          &  slbound_y, srbound_y, rlbound_y, rrbound_y, &
          &  slbound_z, srbound_z, rlbound_z, rrbound_z)

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
