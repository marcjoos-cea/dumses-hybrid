!===============================================================================
!> \file dumses.f90
!! \brief
!! \b DUMSES-Hybrid:
!! This is the main program.
!! \copyright
!! Copyrights 2013-2021, CEA.
!! This file is distributed under the CeCILL-A & GNU/GPL licenses, see
!! <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html> and
!! <http://www.gnu.org/licenses/>
!<
!===============================================================================
!>
!! \mainpage
!! <center>
!! <b>DUMSES-Hybrid</b>\n
!! A versatile MHD grid code for astrophysics\n
!! Copyrights 2013-2021, CEA, <a href="mailto:marc.joos@cea.fr">Marc Joos</a>, Sébastien Fromang, Patrick Hennebelle, Romain Teyssier\n
!! This software is distributed under the CeCILL-A & GNU/GPL licences (see 
!! <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html> and
!! <http://www.gnu.org/licenses/>)
!! </center>
!!
!! Main Contributors to the code:
!!  - Code architecture      : Marc Joos, Sébastien Fromang, Patrick Hennebelle, Romain Teyssier
!!  - MPI parallelization    : Sébastien Fromang, Patrick Hennebelle, Romain Teyssier
!!  - OpenMP parallelization : Marc Joos
!!  - OpenACC parallelization: Marc Joos, Damien Merret, Laëtitia Anton, Clément Fontenaille, Rémy Dubois
!!  - MHD                    : Sébastien Fromang, Patrick Hennebelle, Romain Teyssier
!!  - Parallel I/O           : Marc Joos, Pierre Kestener
!!
!! \c DUMSES is a 3D MPI/OpenMP & MPI/OpenACC Eulerian second-order Godunov (magneto)hydrodynamic simulation code in cartesian, spherical and cylindrical coordinates.
!!
!! This documentation is under the Creative Commons attribution-NonCommercial-ShareAlike license 4.0 International <http://creativecommons.org/licenses/by-nc-sa/4.0/>.
!<
!===============================================================================
program dumses
  use variables
  use params
  use mpi_var
  implicit none

  real(dp) :: thist, tdump, tspec

  ! Timing variables
  real(dp) :: tcpu0, tcpu1, cputime, elptime
  integer  :: ti0, ti1, tg0, tg1, rate
  real(dp) :: tcpug0, tcpug1, globcputime, globelptime
  logical  :: inter

  ndump = 0
  nspec = 0
  inter = .true.

  call init_parallel

  call init_param
  !$acc data copy(uin(:,:,:,:), qin(:,:,:,:), x(:), y(:), z(:), dv(:,:,:), ds(:,:,:,:))
  call init

  !$acc update host(uin(:,:,:,:))
  if (restart == 0) then
     call output
     ndump = ndump + 1
     call history
  endif
  thist = time; tdump = time; tspec = time
  
  ! CPU time
  call cpu_time(tcpug0)
  ! Elapsed time
  call system_clock(count=tg0, count_rate=rate)

  do
     if (inter) then
        call cpu_time(tcpu0)
        call system_clock(count=ti0, count_rate=rate)
     endif
  
     !$py start_timing Timestep
     call compute_dt(dt)
     !$py end_timing Timestep
     if (rhs) then
        !$acc update host(uin(:,:,:,:))
        uin_old = uin
     endif
     call godunov
     if (rhs) then
        !$py start_timing Source term
        !$acc update host(uin(:,:,:,:))
        call source_term
        !$acc update device(uin(:,:,:,:))
        !$py end_timing Source term
     endif
     !$py start_timing FARGO
     if (fargo) call fargo_update
     !$py end_timing FARGO

     !$py start_timing Boundary
     call boundary
     !$py end_timing Boundary
     !$py start_timing Dissipation processes
     if ((nu > zero) .or. (eta > zero)) call dissipation
     !$py end_timing Dissipation processes

     if (mype == 0) print '("Time/dt: ", 1PE14.8, ", ", 1PE14.8)', time, dt
  
     if (inter) then
        if (mype == 0) then
           call cpu_time(tcpu1)
           cputime = tcpu1 - tcpu0
           call system_clock(count=ti1, count_rate=rate)
           elptime = real(ti1 - ti0, kind=8)/real(rate, kind=8)
           print '("CPU time, Elapsed time, cell update: ", 1PE12.6E2, " s, ", 1PE12.6E2, " s, ", 1PE12.6E2, " cells/s")', &
                cputime, elptime, (nxglob*nyglob*nzglob)/elptime
        endif
     endif
  
     time = time + dt
     if (debug) then
        !$acc update host(uin(:,:,:,:))
        call history
        thist = thist + dthist
        call special
        nspec = nspec + 1
        tspec = tspec + dtspec
        call output
        ndump = ndump + 1
        tdump = tdump + dtdump
     else
        if (((time - dt) <= (thist + dthist)) .and. (time > (thist + dthist))) then
           !$acc update host(uin(:,:,:,:))
           call history
           thist = thist + dthist
        endif
        if (((time - dt) <= (tdump + dtdump)) .and. (time > (tdump + dtdump))) then
           !$acc update host(uin(:,:,:,:))
           call output
           ndump = ndump + 1
           tdump = tdump + dtdump
        endif
        if (((time - dt) <= (tspec + dtspec)) .and. (time > (tspec + dtspec))) then
           !$acc update host(uin(:,:,:,:))
           call special
           nspec = nspec + 1
           tspec = tspec + dtspec
        endif
     endif

     if (debug) then
        if (ndump > 5) then
           exit
        endif
     else
        if((time-dt) > tlim) then
           exit
        endif
     endif
  end do
  !$acc end data

  call deallocate_workspace
  
  if(mype == 0) then
     ! Elapsed time - end
     call system_clock(count=tg1, count_rate=rate)
     globelptime = real(tg1 - tg0, kind=8)/real(rate, kind=8)
     ! CPU time - end
     call cpu_time(tcpug1)
     globcputime = tcpug1 - tcpug0
     print '(/ 3X,"Elapsed time : ",1PE10.3," s",/ &
          &,3X,"CPU time     : ",1PE10.3," s", /)', globelptime, globcputime
  endif

#if MPI == 1
  call finalize_mpi
#endif

end program dumses
