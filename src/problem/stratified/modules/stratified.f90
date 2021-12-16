!===============================================================================
!> \file stratified.f90
!! \brief
!! \b DUMSES-Hybrid:
!! This is stratified module.
!! \details
!! Contains stratified
!! \copyright
!! Copyrights 2013-2021, CEA.
!! This file is distributed under the CeCILL-A & GNU/GPL licenses, see
!! <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html> and
!! <http://www.gnu.org/licenses/>
!<
!===============================================================================
!> Stratified module; define variables for stratified problem
!===============================================================================
module stratified
  use precision
  implicit none

  logical  :: floor, smooth, addmass, massDiff
  real(dp) :: zfloor, dfloor, d0, mass0

end module stratified
