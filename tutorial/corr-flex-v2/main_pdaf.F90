!$Id$
!>  Main driver for PDAF tutorial
!!
!! This is a simple model program to demonstrate the
!! fully-parallel implementation of the online mode of PDAF. 
!!
!! The simple model has a 2-dimensional mesh. The initial state
!! is read from a file. The time stepping consists in shifting
!! the field vertically (in the direction of the first array index)
!! by one grid point per time step. A period boundary condition is
!! applied by inserting the field from the upper boundary into the
!! lower one. 
!!
!! In this code variant the coupling to PDAF is completed.
!!
!! __Revision history:__
!! * 2013-09 - Lars Nerger - Initial code
!! * Later revisions - see repository log
!!
PROGRAM MAIN

  USE mpi
  use mod_model, &
       only: total_steps
  USE mod_parallel_pdaf, &     ! Parallelization variables
       ONLY: MPIerr, npes_world, mype_world, n_modeltasks, &
       init_parallel, finalize_parallel
#if defined USE_PDAF
  USE pdaf_interfaces_module, &   ! Interface definitions to PDAF core routines
       ONLY: PDAF_print_info, PDAF_deallocate
#endif
#if defined (_OPENMP)
  USE omp_lib, &
       ONLY: omp_get_num_threads, omp_get_thread_num
#endif
  IMPLICIT NONE

! Local variables
  INTEGER :: i                         ! Counter
  INTEGER, SAVE :: mythread, nthreads  ! Thread variables for OpenMP
!$OMP THREADPRIVATE(mythread, nthreads)


! ********************************
! ***      INITIALIZATION      ***
! ********************************

! #ifdef USE_PDAF
!   ! Add parallelization for ensemble integration
!   CALL init_parallel_pdaf(0, 1)
! #endif

  ! CALL init_parallel()


#if defined (_OPENMP)
! Initialize threading information in case of OpenMP
!$OMP PARALLEL DEFAULT(none)
  nthreads = omp_get_num_threads()
  mythread = omp_get_thread_num()
!$OMP END PARALLEL
#else
  nthreads = 1
  mythread = 0
#endif

! *** Initial Screen output ***
  initscreen: IF (mype_world == 0) THEN

     WRITE (*, '(/16x, a/)') '+++++ PDAF test suite +++++'
#ifndef USE_PDAF
     WRITE (*, '(18x, a)') 'Run only model forecast'
#else
     WRITE (*, '(16x, a)') 'Data assimilation with PDAF'
#endif

     IF (npes_world > 1) THEN
        WRITE (*, '(/21x, a, i3, a/)') 'Running on ', npes_world, ' PEs'
     ELSE
        WRITE (*, '(/21x, a/)') 'Running on 1 PE'
     END IF
     IF (nthreads > 1) THEN
        WRITE (*, '(15x, a, i3, a/)') 'Use OpenMP with ', nthreads, ' threads'
     END IF
     WRITE (*, '(/)')
     
  END IF initscreen



  ! *** Initial Screen output ***
  IF (mype_world==0) THEN
     WRITE (*, '(/17x, a/)') '+++++ PDAF: Modified by Shoale Badr for Cardiac Data Assimilation +++++'
     WRITE (*, '(16x, a)') '2-D Modified Mitchell-Schaeffer Model: Assimilation Phase'
     WRITE (*, '(/)')
  END IF

  CALL init_parallel_pdaf(0, 1)


  ! *** Initialize model ***
  CALL initialize()  

#ifdef USE_PDAF
  ! Initialize PDAF
  CALL init_pdaf()
#endif


! *****************************
! ***      Integration      ***
! *****************************

  ! *** Perform ensmeble assimilation ***
  ! CALL integrate_pdaf(total_steps)

  CALL assimilate_pdaf()

  ! Syncronize at barrier for exit
  CALL MPI_Barrier(MPI_COMM_WORLD, MPIerr) 
  WRITE (*,*) 'model PE exited: mype ', mype_world

#ifdef USE_PDAF
  ! End parallelization
  CALL finalize_pdaf()
#endif

END PROGRAM MAIN
