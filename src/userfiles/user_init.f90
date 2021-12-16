!===============================================================================
!> \file user_init.f90
!! \brief
!! \b DUMSES-Hybrid:
!! This is user's initialization subroutines.
!! \details
!! Contains user_init(), get_eta()
!! \copyright
!! Copyrights 2013-2021, CEA.
!! This file is distributed under the CeCILL-A & GNU/GPL licenses, see
!! <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html> and
!! <http://www.gnu.org/licenses/>
!<
!===============================================================================
!> User initialization
!===============================================================================
subroutine user_init
  use params
  implicit none

  return
end subroutine user_init
!===============================================================================
!> Get eta parameter
!===============================================================================
subroutine get_eta(etaval, r)
  !!$acc routine vector
  use params
  implicit none
  
  real(dp) :: etaval, r

  etaval = eta

  return
end subroutine get_eta
