!===============================================================================
!> \file parallel.f90
!! \brief
!! \b DUMSES-Hybrid:
!! This is parallel subroutines.
!! \details
!! Contains init_parallel(), finalize_mpi(), grid_structure(), boundary_x()
!! boundary_y(), boundary_z()
!! \copyright
!! Copyrights 2013-2021, CEA.
!! This file is distributed under the CeCILL-A & GNU/GPL licenses, see
!! <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html> and
!! <http://www.gnu.org/licenses/>
!<
!===============================================================================
!> Initialize parallel libraries (MPI, OpenMP and OpenACC)
!===============================================================================
subroutine init_parallel
#if MPI == 1
  use mpi
#endif
  !$ use OMP_LIB
#if OACC == 1
  use openacc
  use cudafor
#endif
#if NCCL == 1
  use nccl
  use nccl_var
#endif
  use params
  use mpi_var
  implicit none

  integer :: nthreads = 1
  integer :: required, provided
  integer :: i
  integer :: ierr

#if NCCL == 1
  ! NCCL_UNIQUE_ID_BYTES = 128; see nccl.h
  character(LEN=128) :: nccl_uid_internal
  integer :: nccl_uid_internal_size
#endif
  
#if MPI == 1
  ! Initialize MPI environment
#if _OPENMP
  required = MPI_THREAD_FUNNELED
  ! required can take a value in: MPI_THREAD_SINGLE, MPI_THREAD_FUNNELED, 
  ! MPI_THREAD_SERIALIZED, MPI_THREAD_MULTIPLE
  call MPI_Init_Thread(required, provided, ierr)
#else
  call MPI_Init(ierr)
#endif

  call MPI_Comm_Size(MPI_COMM_WORLD, npes, ierr)
  call MPI_Comm_Rank(MPI_COMM_WORLD, mype, ierr)

  ! Uncomment the following to check the MPI thread support
  ! if (mype == 0) then
  !    print '("MPI thread support: ")'
  !    print '(A12, " MPI_THREAD_SINGLE     ", A12)' &
  !         & , merge("required -->", "            " &
  !         & , required == MPI_THREAD_SINGLE) &
  !         & , merge("<-- provided", "            " &
  !         & , provided == MPI_THREAD_SINGLE)
  !    print '(A12, " MPI_THREAD_SERIALIZED ", A12)' &
  !         & , merge("required -->", "            " & 
  !         & , required == MPI_THREAD_SERIALIZED) &
  !         & , merge("<-- provided", "            " &
  !         & , provided == MPI_THREAD_SERIALIZED)
  !    print '(A12, " MPI_THREAD_FUNNELED   ", A12)' &
  !         & , merge("required -->", "            " &
  !         & , required == MPI_THREAD_FUNNELED) &
  !         & , merge("<-- provided", "            " &
  !         & , provided == MPI_THREAD_FUNNELED)
  !    print '(A12, " MPI_THREAD_MULTIPLE   ", A12)' &
  !         & , merge("required -->", "            " &
  !         & , required == MPI_THREAD_MULTIPLE) &
  !         & , merge("<-- provided", "            " &
  !         & , provided == MPI_THREAD_MULTIPLE)
  ! endif

#if OACC == 1
  call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, mype, MPI_INFO_NULL &
       , local_comm, ierr)  
  call MPI_Comm_rank(local_comm, local_mype, ierr)
  ierr = cudaSetDevice(local_mype)

  call acc_init(acc_get_device_type())
  call acc_set_device_num(local_mype, acc_get_device_type()) 
  nthreads = acc_get_num_devices(acc_get_device_type())
#endif

# if NCCL == 1
  if (mype == 0) then
     nccl_ierr = ncclGetUniqueId(nccl_uid)
  end if

  nccl_uid_internal_size = sizeof(nccl_uid%internal)
  call MPI_Bcast(nccl_uid%internal, nccl_uid_internal_size, MPI_CHAR, 0, MPI_COMM_WORLD, ierr)
  
  nccl_ierr = ncclCommInitRank(nccl_comm, npes, nccl_uid, mype)
#endif
#endif
  
  !$OMP PARALLEL
  !$OMP MASTER
  !$ nthreads = omp_get_num_threads()
  if (mype == 0) then
#if OACC == 1
     print '("===============================================================")'
     print '("    ________                                                   ")'
     print '("    ___  __ \\___  ________ _______________________             ")'
     print '("    __  / / /  / / /_  __ `__ \\_  ___/  _ \\_  ___/             ")'
     print '("    _  /_/ // /_/ /_  / / / / /(__  )/  __/(__  )              ")'
     print '("    /_____/ \\__,_/ /_/ /_/ /_//____/ \\___//____/               ")'
     print '("        _____________         ______        ______________     ")'
     print '("            ______  /______  ____  /___________(_)_____  /     ")'
     print '("              ___  __ \\_  / / /_  __ \\_  ___/_  /_  __  /      ")'
     print '("               _  / / /  /_/ /_  /_/ /  /   _  / / /_/ /       ")'
     print '("               /_/ /_/_\\__, / /_.___//_/    /_/  \\__,_/        ")'
     print '("                      /____/                                   ")'
     
     print '("         This is DUMSES - OpenACC/MPI hybrid version           ")'
#else
     print '("===============================================================")'
     print '("    ________                                                   ")'
     print '("    ___  __ \___  ________ _______________________             ")'
     print '("    __  / / /  / / /_  __ `__ \_  ___/  _ \_  ___/             ")'
     print '("    _  /_/ // /_/ /_  / / / / /(__  )/  __/(__  )              ")'
     print '("    /_____/ \__,_/ /_/ /_/ /_//____/ \___//____/               ")'
     print '("        _____________         ______        ______________     ")'
     print '("            ______  /______  ____  /___________(_)_____  /     ")'
     print '("              ___  __ \_  / / /_  __ \_  ___/_  /_  __  /      ")'
     print '("               _  / / /  /_/ /_  /_/ /  /   _  / / /_/ /       ")'
     print '("               /_/ /_/_\__, / /_.___//_/    /_/  \__,_/        ")'
     print '("                      /____/                                   ")'
     
     print '("         This is DUMSES - OpenMP/MPI hybrid version            ")'
#endif
     print '("(c) 2013-2015, CEA. This software is distributed under a joint ")'
     print '(" CeCILL-A and GNU/GPL license.                                 ")'
     print '("===============================================================")'
     print '("     Execution with", I6, " MPI process(es)")', npes
#if OACC == 1
     print '("       and ", I3, " GPU(s) available.")', nthreads
#else
     print '("       and ", I3, " OpenMP thread(s) per process.")', nthreads
#endif
     print '("                       Start execution!                        ")'
     print '("===============================================================")'
  endif
  !$OMP END MASTER
  !$OMP END PARALLEL

  return
end subroutine init_parallel
!===============================================================================
!> Finalize MPI
!===============================================================================
subroutine finalize_mpi
#if MPI == 1
  use mpi
#endif
#if OACC == 1
  use openacc
#endif
#if NCCL == 1
  use nccl
#endif
  use mpi_var
  use nccl_var
  implicit none
  
  integer :: error=0, ierr

#if NCCL == 1
  nccl_ierr = ncclCommDestroy(nccl_comm)
#endif
  
#if MPI == 1
  call MPI_Finalize(ierr)
#endif

#if OACC == 1
  call acc_shutdown(acc_device_nvidia)
#endif

  return
end subroutine finalize_mpi
!===============================================================================
!> Define the MPI grid process structure
!===============================================================================
subroutine grid_structure
#if MPI == 1
  use mpi
#endif
  use params
  use mpi_var
  implicit none

  integer :: i
  integer :: ierr

  xposition = mod(mype, nxslice)
  if (xposition == 0) then
     xleft = mype + nxslice - 1
  else
     xleft = mype - 1
  endif
  if (xposition == nxslice-1) then
     xright = mype - nxslice + 1
  else
     xright = mype + 1
  endif

  yposition = mod(mype/(nxslice*nzslice), nyslice)
  if (yposition == 0) then 
     yleft = mype + npes - nxslice*nzslice
  else
     yleft = mype - nxslice*nzslice
  endif
  if (yposition == nyslice-1) then 
     yright = mype - npes + nxslice*nzslice
  else
     yright = mype + nxslice*nzslice
  endif

  zposition = mod(mype/nxslice, nzslice)
  if (zposition == 0) then
     zleft  = mype + nxslice*nzslice - nxslice
  else
     zleft = mype - nxslice
  endif
  if (zposition == nzslice-1) then
     zright = mype - nxslice*nzslice + nxslice
  else
     zright = mype + nxslice
  endif

  do i = 1, npes
#if MPI == 1
     call MPI_Barrier(MPI_COMM_WORLD, ierr)
#endif
     if (mype == (i - 1)) then
        write(*, "('mype: ', I5, ' xleft: ', I5, ' xright: ', I5, ' yleft: ' &
             , I5, ' yright: ', I5, ' zleft: ', I5, ' zright: ', I5)") &
             mype, xleft, xright, yleft, yright, zleft, zright
     endif
  enddo

  return
end subroutine grid_structure
!===============================================================================
#if MPI == 1
!> Boundary conditions in x-direction with MPI
!===============================================================================
subroutine boundary_x
  use mpi
#if NCCL == 1
  use nccl
#endif
  use variables
  use params
  use mpi_var
  use nccl_var
  implicit none

  integer :: size
  integer :: j, k
  integer, dimension(MPI_STATUS_SIZE) :: status
  integer :: ierr

  !$acc data present(slbound_x, srbound_x, rlbound_x, rrbound_x)
  
  size = (ju2 - ju1 + 1)*(ku2 - ku1 + 1)*(nvar + 3)*nghost
  if (boundary_type(1) == 'periodic') then
     if (nxslice > 1) then
        !$acc kernels loop
        !$OMP PARALLEL DO SCHEDULE(RUNTIME)
        do k = ku1, ku2
           do j = ju1, ju2
              slbound_x(j,k,:,1) = uin(iu1+3,j,k,:)
              slbound_x(j,k,:,2) = uin(iu1+4,j,k,:)
              slbound_x(j,k,:,3) = uin(iu1+5,j,k,:)
              srbound_x(j,k,:,1) = uin(iu2-5,j,k,:)
              srbound_x(j,k,:,2) = uin(iu2-4,j,k,:)
              srbound_x(j,k,:,3) = uin(iu2-3,j,k,:)
           enddo
        enddo
        !$OMP END PARALLEL DO
        
#if CUDA_AWARE_MPI == 1
        !$acc host_data use_device(slbound_x, srbound_x, rlbound_x, rrbound_x)
#else
        !$acc update host(slbound_x, srbound_x)
#endif
#if NCCL == 1
     nccl_ierr = ncclGroupStart()
     nccl_ierr = ncclSend(slbound_x, size, ncclDouble, xleft, nccl_comm, 0)
     nccl_ierr = ncclRecv(rlbound_x, size, ncclDouble, xright, nccl_comm, 0)
     nccl_ierr = ncclGroupEnd()

     nccl_ierr = ncclGroupStart()
     nccl_ierr = ncclSend(srbound_x, size, ncclDouble, xright, nccl_comm, 0)
     nccl_ierr = ncclRecv(rrbound_x, size, ncclDouble, xleft, nccl_comm, 0)
     nccl_ierr = ncclGroupEnd()
#else     
        call MPI_Sendrecv(slbound_x, size, MPI_DOUBLE_PRECISION, xleft, 10 &
                        , rlbound_x, size, MPI_DOUBLE_PRECISION, xright, 10 &
                        , MPI_COMM_WORLD, status, ierr)
     
        call MPI_Sendrecv(srbound_x, size, MPI_DOUBLE_PRECISION, xright, 11 &
                        , rrbound_x, size, MPI_DOUBLE_PRECISION, xleft, 11 &
                        , MPI_COMM_WORLD, status, ierr)
#endif
#if CUDA_AWARE_MPI == 1
        !$acc end host_data
#else
        !$acc update device(rlbound_x, rrbound_x)
#endif
     
        !$acc kernels loop
        !$OMP PARALLEL DO SCHEDULE(RUNTIME)
        do k = ku1, ku2
           do j = ju1, ju2
              uin(iu1  ,j,k,:) = rrbound_x(j,k,:,1)
              uin(iu1+1,j,k,:) = rrbound_x(j,k,:,2)
              uin(iu1+2,j,k,:) = rrbound_x(j,k,:,3)
              uin(iu2-2,j,k,:) = rlbound_x(j,k,:,1)
              uin(iu2-1,j,k,:) = rlbound_x(j,k,:,2)
              uin(iu2  ,j,k,:) = rlbound_x(j,k,:,3)
           enddo
        enddo
        !$OMP END PARALLEL DO
     endif
  else
     if (nxslice > 1) then
#if NCCL == 1
        nccl_ierr = ncclGroupStart()
#endif
        if (xleft > mype) then
           !$acc kernels loop
           !$OMP PARALLEL DO SCHEDULE(RUNTIME)
           do k = ku1, ku2
              do j = ju1, ju2
                 srbound_x(j,k,:,1) = uin(iu2-5,j,k,:)
                 srbound_x(j,k,:,2) = uin(iu2-4,j,k,:)
                 srbound_x(j,k,:,3) = uin(iu2-3,j,k,:)
              enddo
           enddo
           !$OMP END PARALLEL DO
           
#if CUDA_AWARE_MPI == 1
           !$acc host_data use_device(srbound_x,rlbound_x)
#else
           !$acc update host(srbound_x)
#endif
#if NCCL == 1
           nccl_ierr = ncclSend(srbound_x, size, ncclDouble, xright, nccl_comm, 1)
           nccl_ierr = ncclRecv(rlbound_x, size, ncclDouble, xright, nccl_comm, 0)
#else     
           call MPI_Sendrecv(srbound_x, size, MPI_DOUBLE_PRECISION, xright, 11 &
                           , rlbound_x, size, MPI_DOUBLE_PRECISION, xright, 10 &
                           , MPI_COMM_WORLD, status, ierr)
#endif
#if CUDA_AWARE_MPI == 1
           !$acc end host_data
#else
           !$acc update device(rlbound_x)
#endif

           !$acc kernels loop
           !$OMP PARALLEL DO SCHEDULE(RUNTIME)
           do k = ku1, ku2
              do j = ju1, ju2
                 uin(iu2-2,j,k,:) = rlbound_x(j,k,:,1)
                 uin(iu2-1,j,k,:) = rlbound_x(j,k,:,2)
                 uin(iu2  ,j,k,:) = rlbound_x(j,k,:,3)
              enddo
           enddo
           !$OMP END PARALLEL DO
        else if (xright < mype) then
           !$acc kernels loop
           !$OMP PARALLEL DO SCHEDULE(RUNTIME)
           do k = ku1, ku2
              do j = ju1, ju2
                 slbound_x(j,k,:,1) = uin(iu1+3,j,k,:)
                 slbound_x(j,k,:,2) = uin(iu1+4,j,k,:)
                 slbound_x(j,k,:,3) = uin(iu1+5,j,k,:)
              enddo
           enddo
           !$OMP END PARALLEL DO

#if CUDA_AWARE_MPI == 1
           !$acc host_data use_device(slbound_x, srbound_x, rlbound_x, rrbound_x)
#else
           !$acc update host(slbound_x, srbound_x)
#endif
#if NCCL == 1
           nccl_ierr = ncclSend(slbound_x, size, ncclDouble, xleft, nccl_comm, 0)
           nccl_ierr = ncclRecv(rrbound_x, size, ncclDouble, xleft, nccl_comm, 1)
#else     
           call MPI_sendrecv(slbound_x, size, MPI_DOUBLE_PRECISION, xleft, 10 &
                           , rrbound_x, size, MPI_DOUBLE_PRECISION, xleft, 11 &
                           , MPI_COMM_WORLD, status, ierr)
#endif
#if CUDA_AWARE_MPI == 1
           !$acc end host_data
#else
           !$acc update device(rlbound_x, rrbound_x)
#endif
           
           !$acc kernels loop
           !$OMP PARALLEL DO SCHEDULE(RUNTIME)
           do k = ku1, ku2
              do j = ju1, ju2
                 uin(iu1  ,j,k,:) = rrbound_x(j,k,:,1)
                 uin(iu1+1,j,k,:) = rrbound_x(j,k,:,2)
                 uin(iu1+2,j,k,:) = rrbound_x(j,k,:,3)
              enddo
           enddo
           !$OMP END PARALLEL DO
        else
           !$acc kernels loop
           !$OMP PARALLEL DO SCHEDULE(RUNTIME)
           do k = ku1, ku2
              do j = ju1, ju2
                 slbound_x(j,k,:,1) = uin(iu1+3,j,k,:)
                 slbound_x(j,k,:,2) = uin(iu1+4,j,k,:)
                 slbound_x(j,k,:,3) = uin(iu1+5,j,k,:)
                 srbound_x(j,k,:,1) = uin(iu2-5,j,k,:)
                 srbound_x(j,k,:,2) = uin(iu2-4,j,k,:)
                 srbound_x(j,k,:,3) = uin(iu2-3,j,k,:)
              enddo
           enddo
           !$OMP END PARALLEL DO
           
#if CUDA_AWARE_MPI == 1
           !$acc host_data use_device(slbound_x, srbound_x, rlbound_x, rrbound_x)
#else
           !$acc update host(slbound_x, srbound_x)
#endif
#if NCCL == 1
           nccl_ierr = ncclSend(slbound_x, size, ncclDouble, xleft, nccl_comm, 0)
           nccl_ierr = ncclRecv(rlbound_x, size, ncclDouble, xright, nccl_comm, 0)

           nccl_ierr = ncclSend(srbound_x, size, ncclDouble, xright, nccl_comm, 1)
           nccl_ierr = ncclRecv(rrbound_x, size, ncclDouble, xleft, nccl_comm, 1)
#else     
           call MPI_Sendrecv(slbound_x, size, MPI_DOUBLE_PRECISION, xleft, 10 &
                           , rlbound_x, size, MPI_DOUBLE_PRECISION, xright, 10 &
                           , MPI_COMM_WORLD, status, ierr)
           
           call MPI_Sendrecv(srbound_x, size, MPI_DOUBLE_PRECISION, xright, 11 &
                           , rrbound_x, size, MPI_DOUBLE_PRECISION, xleft, 11 &
                           , MPI_COMM_WORLD, status, ierr)
#endif
#if CUDA_AWARE_MPI == 1
           !$acc end host_data
#else
           !$acc update device(rlbound_x, rrbound_x)
#endif
           
           !$acc kernels loop
           !$OMP PARALLEL DO SCHEDULE(RUNTIME)
           do k = ku1, ku2
              do j = ju1, ju2
                 uin(iu1  ,j,k,:) = rrbound_x(j,k,:,1)
                 uin(iu1+1,j,k,:) = rrbound_x(j,k,:,2)
                 uin(iu1+2,j,k,:) = rrbound_x(j,k,:,3)
                 uin(iu2-2,j,k,:) = rlbound_x(j,k,:,1)
                 uin(iu2-1,j,k,:) = rlbound_x(j,k,:,2)
                 uin(iu2  ,j,k,:) = rlbound_x(j,k,:,3)
              enddo
           enddo
           !$OMP END PARALLEL DO
        endif
#if NCCL == 1
        nccl_ierr = ncclGroupEnd()
#endif
     endif
  endif

  !$acc end data
  
  return
end subroutine boundary_x
!===============================================================================
!> Boundary conditions in y-direction with MPI
!===============================================================================
subroutine boundary_y
  use mpi
#if NCCL == 1
  use nccl
#endif
  use variables
  use params
  use mpi_var
  use nccl_var
  implicit none

  integer :: size
  integer :: i, k
  integer, dimension(MPI_STATUS_SIZE) :: status
  integer :: ierr

  !$acc data present(slbound_y, srbound_y, rlbound_y, rrbound_y)

  size = (iu2 - iu1 + 1)*(ku2 - ku1 + 1)*(nvar + 3)*nghost
  if (nyslice > 1) then
     !$acc kernels loop
     !$OMP PARALLEL DO SCHEDULE(RUNTIME)
     do k = ku1, ku2
        do i = iu1, iu2
           slbound_y(i,k,:,1) = uin(i,ju1+3,k,:)
           slbound_y(i,k,:,2) = uin(i,ju1+4,k,:)
           slbound_y(i,k,:,3) = uin(i,ju1+5,k,:)
           srbound_y(i,k,:,1) = uin(i,ju2-5,k,:)
           srbound_y(i,k,:,2) = uin(i,ju2-4,k,:)
           srbound_y(i,k,:,3) = uin(i,ju2-3,k,:)
        enddo
     enddo
     !$OMP END PARALLEL DO
     
#if CUDA_AWARE_MPI == 1
     !$acc host_data use_device(slbound_y, srbound_y, rlbound_y, rrbound_y)
#else
     !$acc update host(slbound_y, srbound_y)
#endif
#if NCCL == 1
     nccl_ierr = ncclGroupStart()
     nccl_ierr = ncclSend(slbound_y, size, ncclDouble, yleft, nccl_comm, 0)
     nccl_ierr = ncclRecv(rlbound_y, size, ncclDouble, yright, nccl_comm, 0)
     nccl_ierr = ncclGroupEnd()

     nccl_ierr = ncclGroupStart()
     nccl_ierr = ncclSend(srbound_y, size, ncclDouble, yright, nccl_comm, 0)
     nccl_ierr = ncclRecv(rrbound_y, size, ncclDouble, yleft, nccl_comm, 0)
     nccl_ierr = ncclGroupEnd()
#else     
     call MPI_Sendrecv(slbound_y, size, MPI_DOUBLE_PRECISION, yleft, 10 &
                     , rlbound_y, size, MPI_DOUBLE_PRECISION, yright, 10 &
                     , MPI_COMM_WORLD, status, ierr)
     
     call MPI_Sendrecv(srbound_y, size, MPI_DOUBLE_PRECISION, yright, 11 &
                     , rrbound_y, size, MPI_DOUBLE_PRECISION, yleft, 11 &
                     , MPI_COMM_WORLD, status, ierr)
#endif
#if CUDA_AWARE_MPI == 1
     !$acc end host_data
#else
     !$acc update device(rlbound_y, rrbound_y)
#endif

     if ((yposition == (nyslice-1)) .and. &
          (boundary_type(2) /= "periodic")) then
        !$acc kernels loop
        !$OMP PARALLEL DO SCHEDULE(RUNTIME)
        do k = ku1, ku2
           do i = iu1, iu2
              uin(i,ju2-2,k,1:6) = rlbound_y(i,k,1:6,1)
              uin(i,ju2-2,k,nvar:nvar+3) = rlbound_y(i,k,nvar:nvar+3,1)
              uin(i,ju2-1,k,:) = rlbound_y(i,k,:,2)
              uin(i,ju2  ,k,:) = rlbound_y(i,k,:,3)
              uin(i,ju1  ,k,:) = rrbound_y(i,k,:,1)
              uin(i,ju1+1,k,:) = rrbound_y(i,k,:,2)
              uin(i,ju1+2,k,:) = rrbound_y(i,k,:,3)
           enddo
        enddo
        !$OMP END PARALLEL DO
     else
        !$acc kernels loop
        !$OMP PARALLEL DO SCHEDULE(RUNTIME)
        do k = ku1, ku2
           do i = iu1, iu2
              uin(i,ju2-2,k,:) = rlbound_y(i,k,:,1)
              uin(i,ju2-1,k,:) = rlbound_y(i,k,:,2)
              uin(i,ju2  ,k,:) = rlbound_y(i,k,:,3)
              uin(i,ju1  ,k,:) = rrbound_y(i,k,:,1)
              uin(i,ju1+1,k,:) = rrbound_y(i,k,:,2)
              uin(i,ju1+2,k,:) = rrbound_y(i,k,:,3)
           enddo
        enddo
        !$OMP END PARALLEL DO
     endif
  endif

  !$acc end data
  
  return
end subroutine boundary_y
!===============================================================================
!> Boundary conditions in z-direction with MPI
!===============================================================================
subroutine boundary_z
  use mpi
#if NCCL == 1
  use nccl
#endif
  use variables
  use params
  use mpi_var
  use nccl_var
  implicit none

  integer :: size
  integer :: i, j
  integer, dimension(MPI_STATUS_SIZE) :: status
  integer :: ierr

  !$acc data present(slbound_z, srbound_z, rlbound_z, rrbound_z)
  
  size = (iu2 - iu1 + 1)*(ju2 - ju1 + 1)*(nvar + 3)*nghost
  if (nzslice > 1) then
     !$acc kernels loop
     !$OMP PARALLEL DO SCHEDULE(RUNTIME)
     do j = ju1, ju2
        do i = iu1, iu2
           slbound_z(i,j,:,1) = uin(i,j,ku1+3,:)
           slbound_z(i,j,:,2) = uin(i,j,ku1+4,:)
           slbound_z(i,j,:,3) = uin(i,j,ku1+5,:)
           srbound_z(i,j,:,1) = uin(i,j,ku2-5,:)
           srbound_z(i,j,:,2) = uin(i,j,ku2-4,:)
           srbound_z(i,j,:,3) = uin(i,j,ku2-3,:)
        enddo
     enddo
     !$OMP END PARALLEL DO
     
#if CUDA_AWARE_MPI == 1
     !$acc host_data use_device(slbound_z, srbound_z, rlbound_z, rrbound_z)
#else
     !$acc update host(slbound_z, srbound_z)
#endif
#if NCCL == 1
     nccl_ierr = ncclGroupStart()
     nccl_ierr = ncclSend(slbound_z, size, ncclDouble, zleft, nccl_comm, 0)
     nccl_ierr = ncclRecv(rlbound_z, size, ncclDouble, zright, nccl_comm, 0)
     nccl_ierr = ncclGroupEnd()

     nccl_ierr = ncclGroupStart()
     nccl_ierr = ncclSend(srbound_z, size, ncclDouble, zright, nccl_comm, 0)
     nccl_ierr = ncclRecv(rrbound_z, size, ncclDouble, zleft, nccl_comm, 0)
     nccl_ierr = ncclGroupEnd()
#else     
     call MPI_Sendrecv(slbound_z, size, MPI_DOUBLE_PRECISION, zleft, 10 &
                     , rlbound_z, size, MPI_DOUBLE_PRECISION, zright, 10 &
                     , MPI_COMM_WORLD, status, ierr)
     
     call MPI_Sendrecv(srbound_z, size, MPI_DOUBLE_PRECISION, zright, 11 &
                     , rrbound_z, size, MPI_DOUBLE_PRECISION, zleft, 11 &
                     , MPI_COMM_WORLD, status, ierr)
#endif
#if CUDA_AWARE_MPI == 1
     !$acc end host_data
#else
     !$acc update device(rlbound_z, rrbound_z)
#endif
     
     if ((zposition > 0) .or. (boundary_type(3) == 'periodic')) then
        !$acc kernels loop
        !$OMP PARALLEL DO SCHEDULE(RUNTIME)
        do j = ju1, ju2
           do i = iu1, iu2
              uin(i,j,ku1  ,:) = rrbound_z(i,j,:,1)
              uin(i,j,ku1+1,:) = rrbound_z(i,j,:,2)
              uin(i,j,ku1+2,:) = rrbound_z(i,j,:,3)
           enddo
        enddo
        !$OMP END PARALLEL DO
     endif
     if ((zposition < nzslice-1) .or. &
          & (boundary_type(3) == 'periodic')) then
        !$acc kernels loop
        !$OMP PARALLEL DO SCHEDULE(RUNTIME)
        do j = ju1, ju2
           do i = iu1, iu2
              uin(i,j,ku2-2,:) = rlbound_z(i,j,:,1)
              uin(i,j,ku2-1,:) = rlbound_z(i,j,:,2)
              uin(i,j,ku2  ,:) = rlbound_z(i,j,:,3)
           enddo
        enddo
        !$OMP END PARALLEL DO
     endif
  endif

  !$acc end data
  
  return
end subroutine boundary_z
#endif
!===============================================================================
!> This routine creates communicators in process planes
!===============================================================================
subroutine create_comm_plane(comm, direction)
#if MPI == 1
  use params
  use mpi_var
  use mpi
  implicit none

  integer, intent(in)  :: direction
  integer, intent(out) :: comm
  integer :: tmp_comm
  integer :: base_grp, grp_comm, ierr
  integer :: dim, nslice, position
  integer, allocatable, dimension(:) :: list
  integer :: i, j, id

  ! Create a master group to create other groups
  call MPI_Comm_Group(MPI_COMM_WORLD, base_grp, ierr)

  if (direction == 1) then
     dim    = nyslice*nzslice
     nslice = nxslice
  else if (direction == 2) then
     dim    = nxslice*nzslice
     nslice = nyslice
  else
     dim    = nxslice*nyslice
     nslice = nzslice
  endif

  allocate(list(dim))

  do j = 0, nslice-1
     id = 1
     do i = 0, nxslice*nyslice*nzslice-1
        if (direction == 1) position = mod(i, nxslice)
        if (direction == 2) position = mod(i/(nxslice*nzslice), nyslice)
        if (direction == 3) position = mod(i/nxslice, nzslice)
        if (position == j) then
           list(id) = i
           id       = id + 1
        endif
     enddo
     ! Create a group per slice
     call MPI_Group_Incl(base_grp, dim, list, grp_comm, ierr)
     call MPI_Comm_Create(MPI_COMM_WORLD, grp_comm, tmp_comm, ierr)
     ! Determine the group of the current thread
     if (direction == 1) position = mod(mype, nxslice)
     if (direction == 2) position = mod(mype/(nxslice*nzslice), nyslice)
     if (direction == 3) position = mod(mype/nxslice, nzslice)
     if (position == j) comm = tmp_comm
     call MPI_Group_Free(grp_comm, ierr)
  enddo

  call MPI_Group_Free(base_grp, ierr)
  deallocate(list)

#endif
  return
end subroutine create_comm_plane
!===============================================================================
!> This routine creates communicators in a given direction
!===============================================================================
subroutine create_comm_line(comm, direction)
#if MPI == 1
  use params
  use mpi_var
  use mpi
  implicit none

  integer, intent(in)  :: direction
  integer, intent(out) :: comm
  integer :: tmp_comm
  integer :: base_grp, grp_comm, ierr
  integer :: dim, n1slice, n2slice, position1, position2
  integer, allocatable, dimension(:) :: list
  integer :: i, j, k, id

  ! Create a master group to create other groups
  call MPI_Comm_Group(MPI_COMM_WORLD, base_grp, ierr)

  if (direction == 1) then
     dim     = nxslice
     n1slice = nyslice
     n2slice = nzslice
  else if (direction == 2) then
     dim     = nyslice
     n1slice = nxslice
     n2slice = nzslice
  else
     dim     = nzslice
     n1slice = nxslice
     n2slice = nyslice
  endif

  allocate(list(dim))

  do k = 0, n2slice-1
     do j = 0, n1slice-1
        id = 1
        do i = 0, nxslice*nyslice*nzslice-1
           if (direction == 1) then
              position1 = mod(i/(nxslice*nzslice), nyslice)
              position2 = mod(i/nxslice, nzslice)
           else if (direction == 2) then
              position1 = mod(i, nxslice)
              position2 = mod(i/nxslice, nzslice)
           else
              position1 = mod(i, nxslice)
              position2 = mod(i/(nxslice*nzslice), nyslice)
           endif
           if (position1 == j .and. position2 == k) then
              list(id) = i
              id       = id + 1
           endif
        enddo
        ! Create a group per line
        call MPI_Group_Incl(base_grp, dim, list, grp_comm, ierr)
        call MPI_Comm_Create(MPI_COMM_WORLD, grp_comm, tmp_comm, ierr)
        ! Determine the group of the current thread
        if (direction == 1) then
           position1 = mod(mype/(nxslice*nzslice), nyslice)
           position2 = mod(mype/nxslice, nzslice)
        else if (direction == 2) then
           position1 = mod(mype, nxslice)
           position2 = mod(mype/nxslice, nzslice)
        else
           position1 = mod(mype, nxslice)
           position2 = mod(mype/(nxslice*nzslice), nyslice)
        endif
        if (position1 == j .and. position2 == k) comm = tmp_comm
        call MPI_Group_Free(grp_comm, ierr)
     enddo
  enddo

  call MPI_Group_Free(base_grp, ierr)
  deallocate(list)

#endif
end subroutine create_comm_line

