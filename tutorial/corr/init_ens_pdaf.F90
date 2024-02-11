!$Id$
!>  Initialize ensemble
!!
!! User-supplied call-back routine for PDAF.
!!
!! Used in all ensemble filters.
!!
!! The routine is called when the filter is
!! initialized in PDAF_filter_init.  It has
!! to initialize an ensemble of dim_ens states.
!!
!! The routine is called by all filter processes and 
!! initializes the ensemble for the PE-local domain.
!!
!! Implementation for the 2D online example
!! without parallelization. Here, the ensmeble is
!! directly read from files.
!!
!! __Revision history:__
!! * 2013-02 - Lars Nerger - Initial code
!! * Later revisions - see repository log
!!
SUBROUTINE init_ens_pdaf(filtertype, dim_p, dim_ens, state_p, Uinv, &
     ens_p, flag)

  USE mod_model, &         ! Model variables
       ONLY: nx
  USE mod_assimilation, &  ! Assimilation variables
       ONLY: ensgroup
  use output_bin, &
       only: read_txt

  IMPLICIT NONE

! *** Arguments ***
  INTEGER, INTENT(in) :: filtertype                !< Type of filter to initialize
  INTEGER, INTENT(in) :: dim_p                     !< PE-local state dimension
  INTEGER, INTENT(in) :: dim_ens                   !< Size of ensemble
  REAL, INTENT(inout) :: state_p(dim_p)            !< PE-local model state
  !< (It is not necessary to initialize the array 'state_p' for ensemble filters.
  !< It is available here only for convenience and can be used freely.)
  REAL, INTENT(inout) :: Uinv(dim_ens-1,dim_ens-1) !< Array not referenced for ensemble filters
  REAL, INTENT(out)   :: ens_p(dim_p, dim_ens)     !< PE-local state ensemble
  INTEGER, INTENT(inout) :: flag                   !< PDAF status flag

! *** local variables ***
  INTEGER :: i                       ! Counter
  CHARACTER(len=2) :: i_str          ! String for ensemble member
  real(8), allocatable :: ens_arr(:,:)



! **********************
! *** INITIALIZATION ***
! **********************

  ! *** Generate full ensemble on filter-PE 0 ***
  WRITE (*, '(/9x, a)') 'Initialize state ensemble'
  WRITE (*, '(9x, a)') '--- read ensemble from files'
  WRITE (*, '(9x, a, i5)') '--- Ensemble size:  ', dim_ens
  
  ! allocate memory for temporary fields
  allocate(ens_arr(nx*nx, dim_ens))

! ********************************
! *** Read ensemble from files ***
! ********************************
  do i = 1, dim_ens
    write(i_str, "(I0)") i
    call read_txt('ens_'//trim(i_str), ens_p(:,i))
  end do

! ****************
! *** clean up ***
! ****************

  DEALLOCATE(ens_arr)

END SUBROUTINE init_ens_pdaf
