!$Id$
!> Module for model-specific variables
!!
!! This module provides variables needed for the 
!! 2-dimensional tutorial model without parallelization.
!!
!! __Revision history:__
!! * 2013-09 - Lars Nerger - Initial code
!! * Later revisions - see repository log
!!
MODULE mod_model

  IMPLICIT NONE
  SAVE

! *** Variables specific for 2D mMS model ***

  INTEGER :: nx                           !< Size of 2D grid in x-direction (same as y direction)
  INTEGER :: total_steps, step_null       !< Total number of time steps
  integer :: endtime, stim_dur, spiraltime, nsteps, nperiod, nstimdur, nspiraltime
  integer :: model_start, spinup_phase, spinup_time_steps, total_time_in_ms, spinup_time
  integer :: ass_step, rmse_arr_dim
  integer, allocatable :: logint1(:,:)
  real :: tau_in, tau_out, tau_open, tau_close, v_gate, v_stim  
  real :: dx, dt, diff, jstim, xlap1, xlap2, d_to_dx2
  REAL(8), ALLOCATABLE :: v(:,:), h(:,:)
  real(8), allocatable :: jin(:,:), jout(:,:), dh(:,:), dv(:,:), xlap(:,:)
  real(8), allocatable :: rmse_(:), spread_(:)
  logical(4), allocatable :: log1(:,:)
  character(len=32) :: prepoststate, prepostens, init1, init2, intpdaf1, intpdaf2, initens, obs


END MODULE mod_model
