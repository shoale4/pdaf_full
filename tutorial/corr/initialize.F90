!$Id$
!>  Initialize model
!!
!! Initialization routine for the simple 2D model without
!! parallelization of the model.
!!
!! The routine defines the size of the model grid and
!! reads the initial state from a file. 
!!
!! __Revision history:__
!! * 2013-09 - Lars Nerger - Initial code
!! * Later revisions - see repository log
!!
SUBROUTINE initialize()

  USE mod_model, &          ! Model variables
      ONLY: nx, v, h, total_steps, dt, &
      obs, tau_in, tau_out, tau_open, tau_close, v_gate, v_stim, &
      endtime, stim_dur, spiraltime, dx, diff, nstimdur, d_to_dx2, nspiraltime, &
      log1, logint1, jin, jout, dv, dh, xlap, jstim, step_null, & 
      tau_in, tau_out, tau_open, tau_close, v_gate, dx, diff, rmse_, &
      spinup_time_steps, spinup_phase, total_time_in_ms
      
    use parser_no_mpi, &
       only: parse

  IMPLICIT NONE

! *** local variables ***
  CHARACTER(len=32) :: handle  ! new sb 1/9/24 -- for command line parsing
  real(8), allocatable :: v_bin(:)
  real(8), allocatable :: h_bin(:)


! **********************
! *** INITIALIZATION ***
! **********************

  ! Model specifications
  nx = 200          ! Extent of grid in x-direction
  dt = 0.25
  spinup_time_steps = 500/dt ! number of time steps to let model spin-up (new sb 3/6/23)
  total_time_in_ms = 4000
  total_steps = total_time_in_ms/dt + spinup_time_steps ! Number of time steps to perform
  
  ! parameter values
	tau_in=0.35
	tau_out=6
	tau_open=120
	tau_close=150
	v_gate=0.13

  	! numerical and stimulation parameters
	endtime=4000
	stim_dur=2
	spiraltime=250
  dx=0.05
	diff=0.001

	nstimdur = stim_dur/dt
	d_to_dx2 = dt/(dx*dx)
	nspiraltime =  real(spiraltime)/dt

	allocate(log1(nx,nx))
	allocate(logint1(nx,nx))
	allocate(jin(nx,nx))
	allocate(jout(nx,nx))
	allocate(dv(nx,nx))
	allocate(dh(nx,nx))
	allocate(xlap(nx,nx))

  step_null = 0


  ! Screen output
  WRITE (*, '(1x, a)') 'INITIALIZE 2D MODIFIED MITCHELL-SCHAEFFER MODEL'
  WRITE (*, '(10x,a,i4,1x,a1,1x,i4)') 'Grid size:', nx, 'x', nx
  WRITE (*, '(10x,a,i4)') 'Time steps', total_steps

  ! allocate array for model field
  ALLOCATE(v(nx, nx))
  allocate(h(nx, nx))

  ! initialize rmse output files
  allocate(rmse_(total_steps))
  rmse_(:) = 0.0

  ! new sb 1/9/24 -- calling parse function given by pdaf
  handle = 'spinup'
  CALL parse(handle, spinup_phase)

  ! arrays to read in binary files
  allocate(v_bin(nx*nx))
  allocate(h_bin(nx*nx))

  ! initialize model
#ifndef USE_PDAF
  if (spinup_phase .eq. 0) then
    v(:,:) = 0
    h(:,:) = 1 * 0.5
  elseif (spinup_phase .eq. 1) then
    OPEN(11, file='outputs/for_elizabeth/v_init', status='old', form='unformatted', access='stream')
    READ (11) v_bin
    close (11)
    OPEN(11, file='outputs/for_elizabeth/h_init', status='old', form='unformatted', access='stream')
    READ (11) h_bin
    close (11)
  endif
#endif 

#ifdef USE_PDAF
  OPEN(11, file='outputs/for_elizabeth/v_init', status='old', form='unformatted', access='stream')
  READ (11) v_bin
  close (11)

  OPEN(11, file='outputs/for_elizabeth/h_init', status='old', form='unformatted', access='stream')
  READ (11) h_bin
  close (11)
#endif 

  v = reshape(v_bin, (/nx,nx/))
  h = reshape(h_bin, (/nx,nx/))
  
END SUBROUTINE initialize
