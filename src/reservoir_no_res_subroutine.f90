SUBROUTINE reservoir_subroutine(nresx, nd, q_surf,time, nd_year, nyear)
    use Block_Reservoir
    use Block_Flow
    use Block_Network

    implicit none

    real :: dayx, q_surf, log_K_z, n_stability, density_dif
    real(8):: time
    integer :: nd,  nresx, nyear, nd_year

    T_epil(nresx) = (volume_h_x(nresx)*T_hypo(nresx) + volume_e_x(nresx)*T_epil(nresx) + &
                   (flow_in_epi_x + flow_in_hyp_x) * T_res_in(nresx) + &
                   (q_surf * dt_comp * surface_area(nresx)) / (density * heat_c_kcal)) / &
                   (volume_h_x(nresx) + volume_e_x(nresx) + flow_in_epi_x + flow_in_hyp_x)
    T_hypo(nresx) = T_epil(nresx)

end subroutine reservoir_subroutine
