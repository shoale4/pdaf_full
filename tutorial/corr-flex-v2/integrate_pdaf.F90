!$Id$
!>  Time stepping loop with adaption for assimilation
!!
!! Time integration for simple 2D tutorial model
!! without parallelization of the model. In this
!! code variant the coupling to PDAF for ensemble
!! assimilation is completed.
!!
!! Each time step the field is shifted by one grid 
!! point in the vertical direction (first array index).
!!
!! __Revision history:__
!! * 2013-09 - Lars Nerger - Initial code
!! * Later revisions - see repository log
!!
SUBROUTINE integrate_pdaf(nsteps)

  USE mod_model, &          ! Model variables
      ONLY: nx, v, h, total_steps, dt, &
      obs, tau_in, tau_out, tau_open, tau_close, v_gate, v_stim, &
      endtime, stim_dur, spiraltime, dx, diff, nstimdur, d_to_dx2, nspiraltime, &
      log1, logint1, jin, jout, dv, dh, xlap, jstim, step_null, & 
      tau_in, tau_out, tau_open, tau_close, v_gate, dx, diff, rmse_, &
      spinup_time_steps, spinup_phase, total_time_in_ms, xlap1, xlap2

  IMPLICIT NONE

! *** local variables ***
  INTEGER :: step, k, count, i, j      ! Counters
  character(len=16) :: filename

! args
  INTEGER, INTENT(in) :: nsteps ! Number of time steps to be performed


! ****************
! *** STEPPING ***
! ****************

  DO step = 1 , nsteps

     jstim = 0

     ! calculate currents
     jin = h*v*(v-v_gate)*(1-v)/tau_in
     jout = -(1-h)*v/tau_out


     ! update derivitaves for state variables
     dv = jin + jout + jstim

     ! doing the (v<vgate) and (v>=vgate)
     log1 = (v<v_gate)
     where(log1)
          logint1 = 1
     else where
          logint1 = 0
     end where


     dh = (logint1)*((1-h)/tau_open)+(1-logint1)*(-h/tau_close)

     ! calculate diffusive coupling
     xlap(:,:) = 0
     do j = 1, nx
          do i = 1, nx
               if (i .eq. 1) then
                    xlap1 = 2*(v(2,j) - v(1,j))
               else if (i .eq. nx) then
                    xlap1 = 2*(v(nx-1,j) - v(nx,j)) 
               else
                    xlap1 = v(i-1,j) - 2*v(i,j) + v(i+1,j)
               end if 

               if (j .eq. 1) then 
                    xlap2 = 2*(v(i,2) - v(i,1))
               else if (j .eq. nx) then
                    xlap2 = 2*(v(i, nx-1) - v(i, nx))
               else
                    xlap2 = v(i,j-1) - 2*v(i,j) + v(i,j+1)
               end if 

               xlap(i,j) = xlap1 + xlap2
          end do 			
     end do
     xlap = xlap * diff * d_to_dx2


     ! integrate using forward euler method
     v = v + dt*dv + xlap
     h = h + dt*dh

     ! call assimilate_pdaf()

  end do

END SUBROUTINE integrate_pdaf
