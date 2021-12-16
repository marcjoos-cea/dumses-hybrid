!===============================================================================
!> \file file.f90
!! \brief
!! \b DUMSES-Hybrid:
!! This is file modules; subroutines to manage data filename.
!! \details
!! Contains get_filename(), get_filename_r(), convtoasc()
!! \copyright
!! Copyrights 2013-2021, CEA.
!! This file is distributed under the CeCILL-A & GNU/GPL licenses, see
!! <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html> and
!! <http://www.gnu.org/licenses/>
!<
!===============================================================================
module file

contains
!===============================================================================
!> Get \c filename (to write) from process rank and number of the output
!===============================================================================
  subroutine get_filename(dirname, ndump, mype, prefix, filename)
#if MPI == 1
    use mpi
#endif
    implicit none
  
    integer, intent(in) :: ndump, mype
    character(LEN=6)    :: snumdir, snumfile
    character(LEN=*), intent(in)   :: dirname, prefix
    character(LEN=80), intent(out) :: filename
    character(LEN=80)   :: filedir, filecmd
    integer :: ierr
  
    call convtoasc(ndump, snumdir)
    call convtoasc(mype, snumfile)
    filedir  = trim(dirname)//'_'//trim(snumdir)//'/'
    filecmd  = 'mkdir -p '//trim(filedir)
    filename = trim(filedir)//trim(prefix)//'.'//trim(snumfile)
  
#if BLUEGENE == 1
    ! 511 corresponds to the rights of the directory; dec(511) = oct(777)
    if (mype == 0) call mkdir(trim(filedir)//'\0', %val(511))
#else
    if (mype == 0) call system(filecmd)
#endif
#if MPI == 1
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
#endif
  
    return
  end subroutine get_filename
!===============================================================================
!> Get \c filename (to read) from process rank and number of the output
!===============================================================================
  subroutine get_filename_r(ndump, mype, prefix, filename)
    implicit none
  
    integer, intent(in) :: ndump, mype
    character(LEN=6)    :: snumdir, snumfile
    character(LEN=*), intent(in)   :: prefix
    character(LEN=80), intent(out) :: filename
    character(LEN=80)   :: filedir
  
    call convtoasc(ndump, snumdir)
    call convtoasc(mype, snumfile)
    filedir  = 'output_'//trim(snumdir)//'/'
    filename = trim(filedir)//trim(prefix)//'.'//trim(snumfile)
  
    return
  end subroutine get_filename_r
!===============================================================================
!> Convert a integer in a 6 characters string
!===============================================================================
  subroutine convtoasc(number, sstring)
    implicit none
    integer, intent(in) :: number
    integer :: istring, num, nums10, i
    character(LEN=6), intent(out) :: sstring
    character(LEN=10), parameter  :: nstring="0123456789"
    
    num    = 1000000
    nums10 = num/10
    do i = 1, 6
       istring      = 1 + mod(number,num)/nums10
       sstring(i:i) = nstring(istring:istring)
       num    = num/10
       nums10 = nums10/10
    enddo
  
    return
  end subroutine convtoasc
end module file
