SUBROUTINE reservoir_subroutine(nresx, nd, q_surf,time, nd_year)
! SUBROUTINE reservoir_subroutine(T_epil,T_hypo, volume_e_x,volume_h_x)
   use Block_Reservoir
   use Block_Flow
   use Block_Network

 implicit none

 real :: dayx, q_surf
 real(8):: time
 integer :: nd,  nresx, nyear, nd_year
!  real, dimension (:), allocatable :: T_epil,T_hypo,volume_e_x,volume_h_x,stream_T_in


 ! ---------------- turnover loop driven only by T_epil and T_hyp ----------
        dayx = nd  ! day of year
        if ( ( T_epil(nresx) - T_hypo(nresx)) .lt. (2) .and. dayx .gt. 245) then !245 is September 1st
                if( (T_epil(nresx) - T_hypo(nresx)) .lt. (0) ) then
                         K_z(nresx) = 1 ! set high K_z when moderately unstable
                else
                         K_z(nresx) = 0.1 ! set moderate K_z when system is unstable
                end if
        else ! if T_epil greater than T_hypo
                  K_z(nresx) = 0.001  ! set the diffusion coeff. in m^2/day
                  K_z(nresx) = K_z(nresx) / (depth_e(nresx)/2) ! divide by approx thickness of thermocl.
        end if

        ! ################ This is specially for energy test###########!                
        !    K_z(nresx) = 0

      ! -------------------- calculate temperature terms  -------------------------
      dif_epi_x  = K_z(nresx) * surface_area(nresx) *  (T_hypo(nresx) - T_epil(nresx)) / volume_e_x(nresx)
      dif_hyp_x  = K_z(nresx) * surface_area(nresx) *  (T_epil(nresx) - T_hypo(nresx)) / volume_h_x(nresx)

      ! --------------------- calculate advection terms --------------------------- 
      advec_in_epix  = flow_in_epi_x * (T_res_in(nresx) - T_epil(nresx)) /volume_e_x(nresx)
      advec_epi_hyp = flow_epi_hyp_x *  (T_epil(nresx) - T_hypo(nresx)) / volume_h_x(nresx)
      advec_in_hypx = flow_in_hyp_x * (T_res_in(nresx) - T_hypo(nresx)) /volume_h_x(nresx)

      ! ------------------- calculate change in temperature  ---------------------

        ! ---------------- epilimnion -----------

           ! ------------ calculate total energy ----------

   ! ################ This is specially for energy test###########!                
       !   q_surf = 0  ! ONLY for RBM test
       !   dif_hyp_x = 0  ! ONLY for RBM test
       !   dif_epi_x = 0  ! ONLY for RBM test

         energy_x  = (q_surf * dt_comp ) / (depth_e(nresx) * density * heat_c_kcal ) ! kcal/sec*m2 to C/day

   ! ################ This is specially for energy test###########!                
   !      energy_x = (15.0 - 14.89) * sin(2*3.14159/nd_year * nd)

         temp_change_ep(nresx) = advec_in_epix + dif_epi_x + energy_x

!  print *, 'temp_change_ep', temp_change_ep(nresx), 'advec_in', advec_in_epix &
!         , 'energyx', energy_x, 'diffusion', dif_epi_x

         !----- update epilimnion volume for next time step -------
          T_epil(nresx) = T_epil(nresx) + temp_change_ep(nresx)

      ! ------------------ hypolimnion ----------------

         ! ------------ calculate total energy ----------
          temp_change_hyp(nresx) = advec_in_hypx + advec_epi_hyp + dif_hyp_x !  - advec_out_hypx - dV_dt_hyp(nresx)

         !----- update epilimnion volume for next time step -------
          T_hypo(nresx) = T_hypo(nresx) +  temp_change_hyp(nresx)
 
    !---------- calculate combined (hypo. and epil.) temperature of outflow -----
      epix = T_epil(nresx)*(flow_out_epi_x/outflow_x)  ! portion of temperature from epilim. 
      hypox= T_hypo(nresx)*(flow_out_hyp_x/outflow_x)  ! portion of temperature from hypol.
      temp_out(nresx) = epix + hypox   ! average outflow temperature
      volume_tot = volume_e_x(nresx)  + volume_h_x(nresx)
      T_res(nresx) = (T_epil(nresx) * (volume_e_x(nresx)/volume_tot)) + &
         (T_hypo(nresx)*(volume_h_x(nresx)/volume_tot) ) ! weighted averge temp

   ! ################ This is specially for energy test ###########!                
   !   T_res(nresx) = T_epil(nresx)

 ! non-essential - only to print out specific calculated variables
 if(nresx.eq.4) then
 
  write(48,*),time,advec_in_epix, flow_in_epi_x, T_res_in(nresx),  T_epil(nresx), volume_e_x(nresx) &
        , advec_in_hypx, flow_in_hyp_x, T_hypo(nresx), volume_h_x(nresx), flow_epi_hyp_x, temp_out(nresx)&
        , T_res(nresx), advec_epi_hyp, dif_hyp_x, energy_x 
 end if
end subroutine reservoir_subroutine
