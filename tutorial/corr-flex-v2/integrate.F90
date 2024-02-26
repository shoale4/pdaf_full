!$Id$
!>  Time stepping loop of tutorial model
!!
!! Time integration for simple 2D tutorial model
!! without parallelization of the model.
!!
!! Each time step the field is shifted by one grid 
!! point in the vertical direction (first array index).
!!
!! __Revision history:__
!! * 2013-09 - Lars Nerger - Initial code
!! * Later revisions - see repository log
!!
SUBROUTINE integrate()

  USE mod_model, &          ! Model variables
      ONLY: nx, v, h, total_steps, dt, &
      tau_in, tau_out, tau_open, tau_close, v_gate, diff, &
      total_steps, d_to_dx2, nspiraltime, log1, logint1, jin, jout, & 
      dv, dh, xlap, jstim, xlap1, xlap2, tau_in, tau_out, &
      tau_open, tau_close, v_gate, diff, &
      spinup_phase, spinup_time_steps
   
   use output_bin, &
       only: write_txt

  IMPLICIT NONE

! *** local variables ***
  INTEGER :: step, i, j        ! Counters
  character(len=16) :: filename


! ****************
! *** STEPPING ***
! ****************

! time loop

	if (spinup_phase .eq. 0) then
		total_steps = spinup_time_steps
      WRITE (*,*) "SPINNING UP MODEL"
	else
      write (*,*) "WRITING BINARY STATE FILES"
   endif

	do step = 1, total_steps

      if (spinup_phase .eq. 0) then
         if (step .eq. 1) then
            v(:, 1:10) = 0.5
         end if 
         if (step .eq. nspiraltime) then
            v(1:(nx/2), :) = 0
         end if 
      end if 

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

      if (spinup_phase .eq. 0) then
         if (step == spinup_time_steps) then
            ! initial file output for spin-up cases (new sb 9/18/23)
            OPEN(11, file = 'outputs/v_init', status='replace', form='unformatted', access='stream', action='write')
            WRITE (11) reshape(v, (/nx*nx/))
            close(11)
            write (*,*) "Success writing initial V file"

            OPEN(12, file = 'outputs/h_init',  status='replace', form='unformatted', access='stream', action='write')
            write(12) reshape(h, (/nx*nx/))
            close(12)
            write (*,*) "Success writing initial H file"
         end if
      elseif (spinup_phase .eq. 1) then
         if (mod(step, 5) == 0) then
            write (*,*) "writing state @ time step: ", step 
            ! write(filename, "(I0)") step

            if (step .lt. 10) then
			      write(filename, "(I1)") step
            elseif (step .ge. 10 .and. step .lt. 100) then 
               write(filename, "(I2)") step
            elseif (step .ge. 100 .and. step .lt. 1000) then 
               write(filename, "(I3)") step
            elseif (step .ge. 1000 .and. step .lt. 10000) then 
               write(filename, "(I4)") step
            else
               write(filename, "(I5)") step
            endif

            OPEN (11, file='txt_file_data/state_step_'//TRIM(adjustl(filename)), status='replace', form='unformatted', & 
                     access='stream', action='write')
            write (11) reshape(v, (/nx*nx/))
            CLOSE(11)

            ! call write_txt(nx*nx, reshape(v, (/nx*nx/)), trim(filename))
         end if
      end if 

   end do 

END SUBROUTINE integrate
