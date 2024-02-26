!$Id$
!>  Routine to call PDAF for analysis step
!!
!! This routine is called during the model integrations at each time 
!! step. It calls the filter-specific assimilation routine of PDAF 
!! (PDAF_assimilate_X), which checks whether the forecast phase is
!! completed. If so, the analysis step is computed inside PDAF
!!
!! __Revision history:__
!! * 2013-08 - Lars Nerger - Initial code
!! * Later revisions - see repository log
!!
SUBROUTINE assimilate_pdaf()

  USE pdaf_interfaces_module, &   ! Interface definitions to PDAF core routines
       ONLY: PDAFomi_assimilate_local, PDAFomi_assimilate_global, &
       PDAFomi_assimilate_lenkf, PDAF_get_localfilter
  USE mod_parallel_pdaf, &        ! Parallelization
       ONLY: mype_world, abort_parallel
  USE mod_assimilation, &         ! Variables for assimilation
       ONLY: filtertype

  IMPLICIT NONE


! *** External subroutines ***
! Subroutine names are passed over to PDAF in the calls to 
! PDAF_get_state and PDAF_put_state_X. This allows the user 
! to specify the actual name of a routine.  
! The PDAF-internal name of a subroutine might be different
! from the external name!

  ! Interface between model and PDAF, and prepoststep
  EXTERNAL :: collect_state_pdaf, &   ! Collect a state vector from model fields
       distribute_state_pdaf, &       ! Distribute a state vector to model fields
       next_observation_pdaf, &       ! Provide time step of next observation
       prepoststep_ens_pdaf           ! User supplied pre/poststep routine
  ! Localization of state vector
  EXTERNAL :: init_n_domains_pdaf, &  ! Provide number of local analysis domains
       init_dim_l_pdaf, &             ! Initialize state dimension for local analysis domain
       g2l_state_pdaf, &              ! Get state on local analysis domain from global state
       l2g_state_pdaf                 ! Update global state from state on local analysis domain
  ! Interface to PDAF-OMI for local and global filters
  EXTERNAL :: init_dim_obs_pdafomi, & ! Get dimension of full obs. vector for PE-local domain
       obs_op_pdafomi, &              ! Obs. operator for full obs. vector for PE-local domain
       init_dim_obs_l_pdafomi, &      ! Get dimension of obs. vector for local analysis domain
       localize_covar_pdafomi         ! Apply localization to covariance matrix in LEnKF

! *** Local variables ***
  INTEGER :: nsteps      ! Number of time steps to be performed in current forecast
  INTEGER :: doexit      ! Whether to exit forecasting (1=true)
  INTEGER :: status      ! Status flag for filter routines
  REAL :: timenow        ! Current model time
  INTEGER :: localfilter ! Flag for domain-localized filter (1=true)

! *********************************
! ***     Perform forecasts     ***
! *********************************

  ! PDAF: External loop around model time stepper loop
  pdaf_modelloop: DO  

     ! *** PDAF: Get state and forecast information (nsteps,time)  ***
     CALL PDAF_get_state(nsteps, timenow, doexit, next_observation_pdaf, &
          distribute_state_pdaf, prepoststep_ens_pdaf, status)

     ! ! Check whether forecast has to be performed
     checkforecast: IF (doexit /= 1 .AND. status == 0) THEN

     !    ! *** Forecast ensemble state ***
      
        IF (nsteps > 0) THEN

     !       ! *** call time stepper ***  
           CALL integrate_pdaf(nsteps)

        END IF


          ! *********************************
          ! *** Call assimilation routine ***
          ! *********************************

          ! Check  whether the filter is domain-localized
          CALL PDAF_get_localfilter(localfilter)

          ! ! Call assimilate routine for global or local filter
          ! IF (localfilter==1) THEN
          !      CALL PDAFomi_assimilate_local(collect_state_pdaf, distribute_state_pdaf, &
          !           init_dim_obs_pdafomi, obs_op_pdafomi, prepoststep_ens_pdaf, init_n_domains_pdaf, &
          !           init_dim_l_pdaf, init_dim_obs_l_pdafomi, g2l_state_pdaf, l2g_state_pdaf, &
          !           next_observation_pdaf, status)
          ! ELSE
          !      IF (filtertype/=8) THEN
          !      ! All global filters, except LEnKF
          !      CALL PDAFomi_assimilate_global(collect_state_pdaf, distribute_state_pdaf, &
          !           init_dim_obs_pdafomi, obs_op_pdafomi, prepoststep_ens_pdaf, &
          !           next_observation_pdaf, status)
          !      ELSE
          !      ! localized EnKF has its own OMI interface routine
          !      CALL PDAFomi_assimilate_lenkf(collect_state_pdaf, distribute_state_pdaf, &
          !           init_dim_obs_pdafomi, obs_op_pdafomi, prepoststep_ens_pdaf, &
          !           localize_covar_pdafomi, next_observation_pdaf, status)
          !      END IF
          ! END IF

        IF (filtertype == 8) THEN
           ! localized EnKF has its own OMI interface routine
           CALL PDAFomi_put_state_lenkf(collect_state_pdaf, init_dim_obs_pdafomi, &
                obs_op_pdafomi, prepoststep_ens_pdaf, localize_covar_pdafomi, status)
        ELSE IF (filtertype == 100) THEN
           ! Observation generation has its own OMI interface routine
           CALL PDAFomi_put_state_generate_obs(collect_state_pdaf, init_dim_obs_pdafomi, &
                obs_op_pdafomi, prepoststep_ens_pdaf, status)
        ELSE
           ! All other filters can use one of the two generic OMI interface routines
           IF (localfilter==1) THEN
              CALL PDAFomi_put_state_local(collect_state_pdaf, init_dim_obs_pdafomi, &
                   obs_op_pdafomi, prepoststep_ens_pdaf, init_n_domains_pdaf, init_dim_l_pdaf, &
                   init_dim_obs_l_pdafomi, g2l_state_pdaf, l2g_state_pdaf, status)
           ELSE
              CALL PDAFomi_put_state_global(collect_state_pdaf, init_dim_obs_pdafomi, &
                   obs_op_pdafomi, prepoststep_ens_pdaf, status)
           END IF
        END IF

     ELSE checkforecast

        ! *** No more work, exit modeling loop
        EXIT pdaf_modelloop

     END IF checkforecast

  END DO pdaf_modelloop

  ! Check for errors during execution of PDAF

  IF (status /= 0) THEN
     WRITE (*,'(/1x,a6,i3,a43,i4,a1/)') &
          'ERROR ', status, &
          ' in PDAFomi_assimilate - stopping! (PE ', mype_world,')'
     CALL  abort_parallel()
  END IF

END SUBROUTINE assimilate_pdaf
