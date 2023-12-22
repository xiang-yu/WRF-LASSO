































  MODULE module_force_crm

CONTAINS

   SUBROUTINE force_crm(itimestep, dt, crm_num_force_layers            &
                             , crm_force_opt                           &
                             , crm_th_adv_opt, crm_th_rlx_opt          &
                             , crm_qv_adv_opt, crm_qv_rlx_opt          &
                             , crm_adj_uv_opt, crm_uv_rlx_opt          &
                             , crm_vert_adv_opt                        &
                             , crm_wcpa_opt, crm_tau_s, crm_tau_m      &
                             , crm_ust_opt, crm_ust                    &
                             , th_adv, th_adv_tend                     &
                             , th_rlx, th_rlx_tend                     &
                             , qv_adv, qv_adv_tend                     &
                             , qv_rlx, qv_rlx_tend                     &
                             , u_ls, u_ls_tend                         & 
                             , v_ls, v_ls_tend                         & 
                             , w_ls, w_ls_tend                         & 
                             , z_ls, z_ls_tend                         & 
                             , inv_tau_s,inv_tau_m                     & 
                             , u_base, v_base, z_base                  &
                             , z, z_at_w, p, p8w, rho, th, qv, u, v    &
                             , znt                                     &
                             , thten, qvten, uten, vten                &
                             , w_dthdz, w_dqvdz, w_dudz, w_dvdz        &
                             , th_adv_term, qv_adv_term                &
                             , th_rlx_term, qv_rlx_term                &
                             , u_rlx_term,  v_rlx_term                 &
                             , isfflx, crm_flx_opt                     &
                             , crm_albedo_opt, crm_tsk_opt             &
                             , crm_sh_flx, crm_lh_flx                  &
                             , crm_flx_ct, crm_flx_psfcpa              &
                             , crm_albedo, crm_tsk                     &
                             , pre_sh_flx, pre_sh_flx_tend             &
                             , pre_lh_flx, pre_lh_flx_tend             &
                             , pre_albedo, pre_albedo_tend             &
                             , pre_tsk,    pre_tsk_tend                &
                             , hfx, qfx, albedo, tsk                   &
                             , ust, lh                                 &
                             , ids, ide, jds, jde, kds, kde            &
                             , ims, ime, jms, jme, kms, kme            &
                             , ips, ipe, jps, jpe, kps, kpe            &
                             , i_start, i_end, j_start, j_end          &
                             , kts, kte ,num_tiles                     &
                            )









   USE module_init_utilities,  ONLY : interp_0
   USE module_model_constants, ONLY : cp, r_d, p0, rcp, g 
   
   USE module_dm

   IMPLICIT NONE




   INTEGER,    INTENT(IN   )             :: itimestep, crm_num_force_layers
   REAL,       INTENT(IN   )             :: dt

   

   INTEGER,    INTENT(IN   )             :: crm_force_opt,  & 
                                           crm_th_adv_opt,  crm_th_rlx_opt, &
                                           crm_qv_adv_opt,  crm_qv_rlx_opt, &
                                           crm_adj_uv_opt,  crm_uv_rlx_opt, &
                                           crm_vert_adv_opt,crm_wcpa_opt

   REAL,       INTENT(IN   )             :: crm_tau_s, crm_tau_m

   REAL, DIMENSION(crm_num_force_layers), INTENT(INOUT) :: u_ls,v_ls 
   REAL, DIMENSION(crm_num_force_layers), INTENT(INOUT) :: w_ls      
   REAL, DIMENSION(crm_num_force_layers), INTENT(INOUT) :: th_adv, th_rlx 
   REAL, DIMENSION(crm_num_force_layers), INTENT(INOUT) :: qv_adv, qv_rlx 
   REAL, DIMENSION(crm_num_force_layers), INTENT(INOUT) :: z_ls             
   REAL, DIMENSION(crm_num_force_layers), INTENT(INOUT) :: inv_tau_s, inv_tau_m

   REAL, DIMENSION(kms:kme), INTENT(INOUT)              :: u_base, v_base
   REAL, DIMENSION(kms:kme), INTENT(INOUT)              :: z_base 

   REAL, DIMENSION(crm_num_force_layers), INTENT(IN   ) :: u_ls_tend, v_ls_tend
   REAL, DIMENSION(crm_num_force_layers), INTENT(IN   ) :: w_ls_tend 
   REAL, DIMENSION(crm_num_force_layers), INTENT(IN   ) :: th_adv_tend, th_rlx_tend
   REAL, DIMENSION(crm_num_force_layers), INTENT(IN   ) :: qv_adv_tend, qv_rlx_tend
   REAL, DIMENSION(crm_num_force_layers), INTENT(IN   ) :: z_ls_tend       

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN   ) :: z, z_at_w
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN   ) :: p, p8w, rho
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN   ) :: th, qv, u, v

   
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(  OUT) :: thten, qvten 
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(  OUT) :: uten, vten 
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(  OUT) :: w_dthdz, w_dqvdz, w_dudz, w_dvdz

   REAL, DIMENSION(kms:kme), INTENT(  OUT) :: th_adv_term, qv_adv_term, &
                                              th_rlx_term, qv_rlx_term, &
                                              u_rlx_term,  v_rlx_term

   
   
   INTEGER, INTENT(IN   ) :: isfflx, crm_flx_opt, crm_albedo_opt, crm_tsk_opt, & 
                                       crm_ust_opt

   REAL,    INTENT(IN   ) :: crm_sh_flx, crm_lh_flx, crm_flx_ct, crm_flx_psfcpa, &
                              crm_albedo, crm_tsk, crm_ust,       &
                              pre_sh_flx_tend, pre_lh_flx_tend,   & 
                              pre_albedo_tend, pre_tsk_tend       
         
   REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN   ) :: znt

   REAL,    INTENT(INOUT) :: pre_sh_flx, pre_lh_flx, pre_albedo, pre_tsk

   REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: hfx, qfx, albedo, tsk, ust, lh

   

   INTEGER,    INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ips,ipe, jps,jpe, kps,kpe, &
                                                         kts,kte, &
                                       num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN) :: i_start,i_end,j_start,j_end


   

   INTEGER            :: i,j,k, its,ite,jts,jte,ij
   REAL               :: th_rlx_grd,  qv_rlx_grd, w_ls_grd, rho_at_w, wcpa, &
                          u_ls_grd, v_ls_grd, itau, &
                          sum1, sum2, ave1, ave2, no_points 

   REAL, DIMENSION(kms:kme):: z1d, zw1d





   REAL     ::  dthdz, dqvdz, dudz, dvdz

   LOGICAL                      :: debug = .false.
   CHARACTER*256                :: message

   REAL                         ::  tk, xlv 

   REAL :: qfxtmp, hfxtmp, bflx, wnd 

   LOGICAL , EXTERNAL :: wrf_dm_on_monitor

  if ( crm_force_opt .eq. 0 ) return



  

     pre_albedo = pre_albedo + dt * pre_albedo_tend 
     pre_tsk    = pre_tsk    + dt * pre_tsk_tend 
     pre_sh_flx = pre_sh_flx + dt * pre_sh_flx_tend  
     pre_lh_flx = pre_lh_flx + dt * pre_lh_flx_tend 

  !$OMP PARALLEL DO   &
  !$OMP PRIVATE ( ij )

  DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)


  
  albd: SELECT CASE(crm_albedo_opt)

  CASE (0) 

  CASE (1) 

     do j = jts, jte
     do i = its, ite
       albedo(i,j) = crm_albedo
     enddo
     enddo

  CASE (2) 

     do j = jts, jte
     do i = its, ite
       albedo(i,j) = pre_albedo
     enddo
     enddo

  CASE DEFAULT
     CALL wrf_error_fatal3("<stdin>",223,&
'crm_albedo_opt value invalid' )

  END SELECT albd




  tskin: SELECT CASE(crm_tsk_opt)
     
  CASE (0) 

  CASE (1) 

     do j = jts, jte
     do i = its, ite
       tsk(i,j) = crm_tsk
     enddo
     enddo

  CASE (2) 

     do j = jts, jte
     do i = its, ite
       tsk(i,j) = pre_tsk
     enddo
     enddo

  CASE DEFAULT
     CALL wrf_error_fatal3("<stdin>",252,&
'crm_tsk_opt value invalid' )
     
  END SELECT tskin




   flux: SELECT CASE(crm_flx_opt)

   CASE (0) 

   CASE (1) 

      if(isfflx .ne. 10 .and. isfflx .ne. 11) &
         CALL wrf_error_fatal3("<stdin>",267,&
'isfflx value invalid (force_crm)' )
      
      do j = jts, jte
      do i = its, ite
        tk = th(i,kts,j) * (p(i,kts,j)/p0)**rcp 
	xlv = 3.148E6 - 2370 * tk 

        hfx(i,j) =  crm_sh_flx         
        qfx(i,j) =  crm_lh_flx / xlv   

        lh(i,j) = crm_lh_flx 
      enddo
      enddo

   CASE (2) 

      if(isfflx .ne. 10 .and. isfflx .ne. 11) &
         CALL wrf_error_fatal3("<stdin>",285,&
'isfflx value invalid (force_crm)' )
      
      do j = jts, jte
      do i = its, ite
        tk = th(i,kts,j) * (p(i,kts,j)/p0)**rcp 
	xlv = 3.148E6 - 2370 * tk

        hfx(i,j) =  pre_sh_flx
        qfx(i,j) =  pre_lh_flx / xlv

        lh(i,j) = pre_lh_flx 
      enddo
      enddo
















   CASE DEFAULT

    CALL wrf_error_fatal3("<stdin>",317,&
'crm_flx_opt value invalid' )

   END SELECT flux




   ustar: SELECT CASE(crm_ust_opt)

   CASE (0) 

   CASE (1) 

     do j = jts,jte
     do i = its,ite
       ust(i,j) = crm_ust
     end do
     end do

   CASE (2) 

     do j = jts, jte
     do i = its, ite
        qfxtmp = qfx(i,j)/rho(i,kts,j) 
        hfxtmp = hfx(i,j)/(cp*rho(i,kts,j))
        bflx = g/(th(i,kts,j)*(1.+0.61*qv(i,k,j)))*hfxtmp &
             + g*0.61/(1.+0.61*qv(i,kts,j))*qfxtmp 
        wnd  = (u(i,kts,j)**2 + v(i,kts,j)**2)**0.5
        ust(i,j) = diag_ustar(z(i,kts,j),bflx,wnd,znt(i,j))
     enddo
     enddo

   CASE DEFAULT

    CALL wrf_error_fatal3("<stdin>",352,&
'crm_ust_opt value invalid' )

   END SELECT ustar

  ENDDO 

  !$OMP END PARALLEL DO





  
  z_ls = z_ls + dt * z_ls_tend 

  u_ls = u_ls + dt * u_ls_tend 
  v_ls = v_ls + dt * v_ls_tend 

  th_rlx = th_rlx + dt * th_rlx_tend 
  qv_rlx = qv_rlx + dt * qv_rlx_tend 

  th_adv = th_adv + dt * th_adv_tend     
  qv_adv = qv_adv + dt * qv_adv_tend 

  w_ls = w_ls + dt * w_ls_tend


  
  
  IF ( wrf_dm_on_monitor() ) THEN
     do k = kms,kme-1
        z1d(k)  = z(its,k,jts)
        zw1d(k) = z_at_w(its,k,jts)
     enddo
  END IF
  call wrf_dm_bcast_real(z1d, kme-kms+1)
  call wrf_dm_bcast_real(zw1d,kme-kms+1)


  

    th_adv_term = 0.0
    qv_adv_term = 0.0
    dthdz       = 0.0
    dqvdz       = 0.0
    dudz        = 0.0
    dvdz        = 0.0
    w_dthdz     = 0.0
    w_dqvdz     = 0.0
    w_dudz      = 0.0
    w_dvdz      = 0.0

    th_rlx_term = 0.0
    qv_rlx_term = 0.0
    u_rlx_term = 0.0
    v_rlx_term = 0.0

    no_points = float((ide-ids)*(jde-jds))


    

    
    if ( crm_uv_rlx_opt .eq. 1 .or. crm_uv_rlx_opt .eq. 2 ) then

      do k = kms,kme-1

        u_ls_grd = interp_0(u_ls, z_ls, z1d(k), crm_num_force_layers)
        v_ls_grd = interp_0(v_ls, z_ls, z1d(k), crm_num_force_layers)

        
        sum1 = 0.0
        sum2 = 0.0
        do j = jts, jte
        do i = its, ite
          sum1 = sum1 + u(i,k,j)
          sum2 = sum2 + v(i,k,j)
        enddo
        enddo
        sum1 = wrf_dm_sum_real (sum1)
        sum2 = wrf_dm_sum_real (sum2)
        ave1 = sum1 / no_points
        ave2 = sum2 / no_points

        
        if (crm_uv_rlx_opt .eq. 1) then 

          if (crm_tau_m .eq. 0.0) &
            &call wrf_error_fatal3("<stdin>",441,&
' crm_tau_m cannot be zero' )
          itau = 1.0/crm_tau_m

        else if (crm_uv_rlx_opt .eq. 2) then 

          itau = interp_0(inv_tau_m, z_ls, z1d(k), crm_num_force_layers)

        else
          CALL wrf_error_fatal3("<stdin>",450,&
'wrong crm_uv_rlx_opt' )
        end if  

        u_rlx_term(k) = (u_ls_grd - ave1) * itau 
        v_rlx_term(k) = (v_ls_grd - ave2) * itau 

      enddo

    
    else if (crm_adj_uv_opt .eq. 1) then 

      do k = kms,kme-1
          u_base(k) = interp_0(u_ls, z_ls, z1d(k), crm_num_force_layers)
          v_base(k) = interp_0(v_ls, z_ls, z1d(k), crm_num_force_layers)
      enddo

    else if (crm_uv_rlx_opt .gt. 1 .and. crm_adj_uv_opt .eq. 1) then 

      write (*,*) 'crm_adj_uv_opt, crm_uv_rlx_opt', crm_adj_uv_opt, crm_uv_rlx_opt
      call wrf_error_fatal3("<stdin>",470,&
' crm_adj_uv_opt may not be used with crm_uv_rlx ')

    end if


    

    
    if (crm_th_rlx_opt .eq. 1 .or. crm_th_rlx_opt .eq. 2) then

      do k = kms,kme-1

        th_rlx_grd = interp_0(th_rlx, z_ls, z1d(k), crm_num_force_layers)

        
        sum1 = 0.0
        do j = jts, jte
        do i = its, ite
          sum1 = sum1 + th(i,k,j)
        enddo
        enddo 
        sum1 = wrf_dm_sum_real (sum1)
        ave1 = sum1 / no_points

        
        if (crm_th_rlx_opt .eq. 1) then
          if (crm_tau_s .eq. 0.0) &
            call wrf_error_fatal3("<stdin>",498,&
' crm_tau_s cannot be zero')
          itau = 1.0/crm_tau_s

        else if (crm_th_rlx_opt .eq. 2) then
          itau = interp_0(inv_tau_s, z_ls, z1d(k), crm_num_force_layers)
        else
          CALL wrf_error_fatal3("<stdin>",505,&
' wrong crm_th_rlx_opt')
        end if  

        th_rlx_term(k) = (th_rlx_grd - ave1) * itau 

      enddo
    endif

    
    if (crm_qv_rlx_opt .eq. 1 .or. crm_qv_rlx_opt .eq. 2) then

      do k = kms,kme-1

        qv_rlx_grd = interp_0(qv_rlx, z_ls, z1d(k), crm_num_force_layers)

        
        sum1 = 0.0
        do j = jts, jte
        do i = its, ite
          sum1 = sum1 + qv(i,k,j)
        enddo
        enddo
        sum1 = wrf_dm_sum_real (sum1)
        ave1 = sum1 / no_points

        
        if (crm_qv_rlx_opt .eq. 1) then
          if (crm_tau_s .eq. 0.0) call wrf_error_fatal3("<stdin>",533,&
' crm_tau_s cannot be zero')
          itau = 1.0/crm_tau_s
        else if (crm_qv_rlx_opt .eq. 2) then
          itau = interp_0(inv_tau_s, z_ls, z1d(k), crm_num_force_layers)
        else
          CALL wrf_error_fatal3("<stdin>",539,&
'wrong crm_qv_rlx_opt' )
        end if  
        qv_rlx_term(k) = (qv_rlx_grd - ave1) * itau 

      enddo 
    endif


    
  
    if (crm_th_adv_opt .eq. 1) then   
      do k = kms,kme-1
        th_adv_term(k) = interp_0(th_adv, z_ls, z1d(k), crm_num_force_layers)
      enddo
    endif

    if (crm_qv_adv_opt .eq. 1) then
      do k = kms,kme-1
        qv_adv_term(k) = interp_0(qv_adv, z_ls, z1d(k), crm_num_force_layers)
      enddo
    endif


    

    if (crm_vert_adv_opt .eq. 1 .or. crm_vert_adv_opt .eq. 2) then

      do k = kms+1,kme-1

        w_ls_grd = interp_0(w_ls, z_ls, zw1d(k), crm_num_force_layers)

        do j = jts,jte
        do i = its,ite
          dthdz = (th(i,k,j)-th(i,k-1,j))/(z(i,k,j)-z(i,k-1,j)) 
          dqvdz = (qv(i,k,j)-qv(i,k-1,j))/(z(i,k,j)-z(i,k-1,j))

          if (crm_wcpa_opt .eq. 0) then
	    wcpa = 0.0 
          elseif (crm_wcpa_opt .eq. 1) then
            rho_at_w = (rho(i,k,j) + rho(i,k-1,j))/2
            wcpa = (p0/p8w(i,k,j))**rcp * w_ls_grd / cp / rho_at_w 
          else
            CALL wrf_error_fatal3("<stdin>",582,&
'crm_wcpa_opt value invalid' )
          endif

          w_dthdz(i,k,j) = - 1. * w_ls_grd * dthdz + wcpa
          w_dqvdz(i,k,j) = - 1. * w_ls_grd * dqvdz

          
          if (crm_vert_adv_opt .eq. 2) then
            dudz  = (u(i,k,j)-u(i,k-1,j)) / (z(i,k,j)-z(i,k-1,j))
            dvdz  = (v(i,k,j)-v(i,k-1,j)) / (z(i,k,j)-z(i,k-1,j))

            w_dudz(i,k,j)  = - 1 * w_ls_grd * dudz
            w_dvdz(i,k,j)  = - 1 * w_ls_grd * dvdz
          end if
        enddo
        enddo

      enddo 

    end if 
       

    

     do j = jts, jte
       do k = kms, kme-1
         do i = its, ite

           thten(i,k,j) = th_adv_term(k)     &
                     + 0.5*(w_dthdz(i,k,j) + w_dthdz(i,k+1,j)) &
                     + th_rlx_term(k)
           qvten(i,k,j) = qv_adv_term(k)     &
                     + 0.5*(w_dqvdz(i,k,j) + w_dqvdz(i,k+1,j)) &
                     + qv_rlx_term(k)

           if (qv(i,k,j) .le. 0.000001) then
                qvten(i,k,j) = 0.0
           endif

          
          uten(i,k,j)  = 0.5*(w_dudz(i,k,j) + w_dudz(i,k+1,j)) + u_rlx_term(k)
          vten(i,k,j)  = 0.5*(w_dvdz(i,k,j) + w_dvdz(i,k+1,j)) + v_rlx_term(k)
 
        enddo
      enddo
    enddo

  RETURN

  END SUBROUTINE force_crm





   SUBROUTINE  init_tke_crm (itimestep, crm_init_tke_opt,           &
                    crm_init_tke, tke_1, tke_2,                      & 
                    ids,ide, jds,jde, kds,kde,                 & 
                    ims,ime, jms,jme, kms,kme,                        &
                    its,ite, jts,jte, kts,kte                         )








  IMPLICIT NONE



   

   INTEGER,    INTENT(IN   )             :: itimestep, crm_init_tke_opt 
   REAL,       INTENT(IN   )             :: crm_init_tke

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: tke_1,tke_2

   INTEGER,  INTENT(IN   )      ::        ids,ide, jds,jde, kds,kde, &
                                            ims,ime, jms,jme, kms,kme, &
                                            its,ite, jts,jte, kts,kte
   
   INTEGER :: i,k,j



   if ( itimestep .eq. 1 .and. crm_init_tke_opt .eq. 1) then

     do k = kms,kme-1
       do j = jms,jme
       do i = ims,ime
	tke_1(i,k,j) = crm_init_tke
	tke_2(i,k,j) = crm_init_tke
       end do
       end do
     end do

   endif

  END SUBROUTINE  init_tke_crm





  SUBROUTINE  update_crm_lsten (config_flags,mu,muu,muv,pi3d, & 
                    RULSTEN,RVLSTEN,RTHLSTEN,RQVLSTEN,        & 
                    rph_tendf,rt_tendf,ru_tendf,rv_tendf,     &
                    moist_tendf, scalar_tendf,mu_tendf,       &
                    n_moist,n_scalar,rk_step,adv_moist_cond,  &
                    ids,ide, jds,jde, kds,kde,                        & 
                    ims,ime, jms,jme, kms,kme,                        &
                    its,ite, jts,jte, kts,kte                         )











      USE module_state_description, ONLY:PARAM_FIRST_SCALAR
      USE module_configure

      USE module_physics_addtendc

      IMPLICIT NONE



      TYPE(grid_config_rec_type), INTENT(IN)     ::      config_flags

      INTEGER,  INTENT(IN   )   ::        ids,ide, jds,jde, kds,kde, &
                                            ims,ime, jms,jme, kms,kme, &
                                            its,ite, jts,jte, kts,kte

      INTEGER :: i,k,j
      INTEGER :: itf,ktf,jtf,itsu,jtsv

      

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )              , &
                INTENT(INOUT)   ::                            RULSTEN, &
                                                               RVLSTEN, &
                                                              RTHLSTEN, &
                                                              RQVLSTEN 

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )               , &
                INTENT(IN   )   ::                               pi3d
                                                                 
      REAL,     DIMENSION( ims:ime, jms:jme )                      , &
                INTENT(IN   )   ::                                mu, &
                                                                  muu, &
                                                                  muv                                   
      

      INTEGER , INTENT(IN)        ::  n_moist,n_scalar,rk_step  

      LOGICAL , INTENT(IN)        :: adv_moist_cond

      REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(INOUT) ::   &
                                                         ru_tendf, &
                                                         rv_tendf, &
                                                         rt_tendf, &
                                                         rph_tendf

      REAL , DIMENSION(ims:ime , jms:jme),INTENT(INOUT) ::  mu_tendf

      REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),           &
          INTENT(INOUT)     ::                        moist_tendf

      REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar),           &
          INTENT(INOUT)     ::                        scalar_tendf




      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      ktf=MIN(kte,kde-1)
      itsu=MAX(its,ids+1)
      jtsv=MAX(jts,jds+1)




   IF (config_flags%crm_force_opt .gt. 0) THEN

      DO J=jts,jtf
      DO K=kts,ktf
      DO I=its,itf
         RULSTEN(I,K,J) =mu(I,J)*RULSTEN(I,K,J)
         RVLSTEN(I,K,J) =mu(I,J)*RVLSTEN(I,K,J)
         RTHLSTEN(I,K,J)=mu(I,J)*RTHLSTEN(I,K,J)
      ENDDO
      ENDDO
      ENDDO

      IF (P_QV .ge. PARAM_FIRST_SCALAR) THEN 
         DO J=jts,jtf
         DO K=kts,ktf
         DO I=its,itf
            RQVLSTEN(I,K,J)=mu(I,J)*RQVLSTEN(I,K,J)
         ENDDO
         ENDDO
         ENDDO
      ENDIF
 
    ENDIF




   IF (config_flags%crm_force_opt .gt. 0) THEN
                     
           

           CALL add_a2a(rt_tendf,RTHLSTEN,config_flags,          &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_a2c_u(ru_tendf,RULSTEN,config_flags,         &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_a2c_v(rv_tendf,RVLSTEN,config_flags,         &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QV .ge. PARAM_FIRST_SCALAR)     &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVLSTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

   ENDIF

END SUBROUTINE update_crm_lsten





      REAL FUNCTION RSLF(P,T)





      IMPLICIT NONE
      REAL, INTENT(IN):: P, T  
      REAL:: ESL,X
      REAL, PARAMETER:: C0= .611583699E03
      REAL, PARAMETER:: C1= .444606896E02
      REAL, PARAMETER:: C2= .143177157E01
      REAL, PARAMETER:: C3= .264224321E-1
      REAL, PARAMETER:: C4= .299291081E-3
      REAL, PARAMETER:: C5= .203154182E-5
      REAL, PARAMETER:: C6= .702620698E-8
      REAL, PARAMETER:: C7= .379534310E-11
      REAL, PARAMETER:: C8=-.321582393E-13

      X=MAX(-80.,T-273.16)


      ESL=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      RSLF=.622*ESL/(P-ESL)

      END FUNCTION RSLF







  real function diag_ustar(z,bflx,wnd,z0)



























implicit none

real, parameter :: vonk = 0.4 
real, parameter :: g = 9.81 

real, parameter :: am = 4.8 
real, parameter :: bm = 19.3 
real, parameter :: eps = 1.e-10 

real, intent (in) :: z 
real, intent (in) :: bflx 
real, intent (in) :: wnd 

real, intent (in) :: z0 

integer :: iterate
real :: lnz, klnz, c1, x, psi1, zeta, rlmo, ustar

 lnz = log(z/z0)
 klnz = vonk/lnz
 c1 = 3.14159/2. - 3.*log(2.)

 ustar = wnd*klnz 

if (bflx .lt. 0.0) then
   rlmo = -bflx * vonk/(ustar**3 + eps) 
   zeta = z*rlmo
   ustar = vonk*wnd /(lnz + am*zeta)
 else 
   do iterate=1,4
    rlmo = -bflx * vonk/(ustar**3 + eps) 
    zeta = z*rlmo
    if (zeta > 0.) then
      ustar = vonk*wnd /(lnz + am*zeta)
    else
      x = sqrt( sqrt( 1.0 - bm*zeta ) )
      psi1 = 2.*log(1.0+x) + log(1.0+x*x) - 2.*atan(x) + c1
      ustar = wnd*vonk/(lnz - psi1)
    end if
   end do
 end if

diag_ustar = ustar

return

end function diag_ustar







   SUBROUTINE crm_aer(itimestep, dt, crm_morr_act_opt  &
                             , crm_num_aer_layers      &
                             , crm_morr_hygro_opt      &
                             , crm_morr_hygro          &
                             , num_modes_aer             &
                             , rm_aer1, rm_aer1_tend     &
                             , rm_aer2, rm_aer2_tend     &
                             , rm_aer3, rm_aer3_tend     &
                             , rm_aer4, rm_aer4_tend     &
                             , sig_aer1, sig_aer1_tend   &
                             , sig_aer2, sig_aer2_tend   &
                             , sig_aer3, sig_aer3_tend   &
                             , sig_aer4, sig_aer4_tend   &
                             , z_aer, z_aer_tend       &
                             , hygro_aer1, hygro_aer1_tend &
                             , hygro_aer2, hygro_aer2_tend &
                             , hygro_aer3, hygro_aer3_tend &
                             , hygro_aer4, hygro_aer4_tend &
                             , na_aer1, na_aer1_tend     &
                             , na_aer2, na_aer2_tend     &
                             , na_aer3, na_aer3_tend     &
                             , na_aer4, na_aer4_tend     &
                             , na_aer1_grd              &
                             , na_aer2_grd              &
                             , na_aer3_grd              &
                             , na_aer4_grd              &
                             , z, z_at_w, p, p8w, rho  &
                             , ids, ide, jds, jde, kds, kde            &
                             , ims, ime, jms, jme, kms, kme            &
                             , ips, ipe, jps, jpe, kps, kpe            &
                             , i_start, i_end, j_start, j_end          &
                             , kts, kte ,num_tiles                     &
                            )







   USE module_init_utilities,  ONLY : interp_0
   USE module_model_constants, ONLY : cp, r_d, p0, rcp 
   
   USE module_dm

   IMPLICIT NONE



   INTEGER,    INTENT(IN   )             :: itimestep, crm_morr_act_opt, & 
                                                     crm_num_aer_layers, & 
                                                     crm_morr_hygro_opt
                                                         
   REAL,       INTENT(IN   )             :: dt

   REAL,       INTENT(IN   )             :: crm_morr_hygro

   

   INTEGER,    INTENT(IN   )             :: num_modes_aer 

   REAL,    INTENT(INOUT) :: rm_aer1, sig_aer1, hygro_aer1,&
                             rm_aer2, sig_aer2, hygro_aer2,&
                             rm_aer3, sig_aer3, hygro_aer3,&
                             rm_aer4, sig_aer4, hygro_aer4

   REAL,    INTENT(IN   ) :: rm_aer1_tend, sig_aer1_tend, hygro_aer1_tend,&
                             rm_aer2_tend, sig_aer2_tend, hygro_aer2_tend,&
                             rm_aer3_tend, sig_aer3_tend, hygro_aer3_tend,&
                             rm_aer4_tend, sig_aer4_tend, hygro_aer4_tend

   
   REAL, DIMENSION(crm_num_aer_layers), INTENT(INOUT) :: z_aer, na_aer1, &
                                                                na_aer2, &
                                                                na_aer3, &
                                                                na_aer4
 
   REAL, DIMENSION(crm_num_aer_layers), INTENT(IN   ) :: z_aer_tend, na_aer1_tend,&  
                                                                     na_aer2_tend,&  
                                                                     na_aer3_tend,&  
                                                                     na_aer4_tend 
   
   REAL, DIMENSION( kms:kme ), INTENT(  OUT) :: na_aer1_grd, &
                                                na_aer2_grd, &
                                                na_aer3_grd, &
                                                na_aer4_grd

   
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN   ) :: z, z_at_w
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN   ) :: p, p8w, rho

   
   INTEGER,    INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ips,ipe, jps,jpe, kps,kpe, &
                                                         kts,kte, &
                                                       num_tiles
   INTEGER, DIMENSION(num_tiles), INTENT(IN) :: i_start,i_end,j_start,j_end

   
   INTEGER            :: i,j,k, its,ite,jts,jte,ij
   REAL, DIMENSION(kms:kme):: z1d

   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor



  if ( crm_morr_act_opt .ne. 2 ) return

  
  IF ( wrf_dm_on_monitor() ) THEN
     do k = kms,kme-1
        z1d(k)  = z(ips,k,jps)
        
     enddo
  END IF

  call wrf_dm_bcast_real(z1d, kme-kms+1)
  

  if (itimestep .eq. 1) then
    write (*,*) 'NUM_MODES_AER,rm_aer1 = ', NUM_MODES_AER, rm_aer1
  end if 
  if (num_modes_aer .le. 0 .or. num_modes_aer .ge. 5) then
    CALL wrf_error_fatal3("<stdin>",1073,&
'NUM_MODES_AER value invalid' )
  endif

  
  z_aer  = z_aer  + dt * z_aer_tend

  rm_aer1  = rm_aer1  + dt * rm_aer1_tend
  sig_aer1 = sig_aer1 + dt * sig_aer1_tend
  na_aer1  = na_aer1  + dt * na_aer1_tend
  do k = kms,kme-1
    na_aer1_grd(k) = interp_0(na_aer1, z_aer, z1d(k), crm_num_aer_layers)
  enddo

  if (num_modes_aer .ge. 2) then
    rm_aer2  = rm_aer2  + dt * rm_aer2_tend
    sig_aer2 = sig_aer2 + dt * sig_aer2_tend
    na_aer2  = na_aer2  + dt * na_aer2_tend
    do k = kms,kme-1
      na_aer2_grd(k) = interp_0(na_aer2, z_aer, z1d(k), crm_num_aer_layers)
    enddo
  endif

  if (num_modes_aer .ge. 3) then
    rm_aer3  = rm_aer3  + dt * rm_aer3_tend
    sig_aer3 = sig_aer3 + dt * sig_aer3_tend
    na_aer3  = na_aer3  + dt * na_aer3_tend
    do k = kms,kme-1
      na_aer3_grd(k) = interp_0(na_aer3, z_aer, z1d(k), crm_num_aer_layers)
    enddo
  endif

  if (num_modes_aer .ge. 4) then
    rm_aer4  = rm_aer4  + dt * rm_aer4_tend
    sig_aer4 = sig_aer4 + dt * sig_aer4_tend
    na_aer4  = na_aer4  + dt * na_aer4_tend
    do k = kms,kme-1
      na_aer4_grd(k) = interp_0(na_aer4, z_aer, z1d(k), crm_num_aer_layers)
    enddo
  endif

  if (crm_morr_hygro_opt .eq. 1) then 
    hygro_aer1 = crm_morr_hygro
    hygro_aer2 = crm_morr_hygro
    hygro_aer3 = crm_morr_hygro
    hygro_aer4 = crm_morr_hygro
  else if (crm_morr_hygro_opt .eq. 2) then 
    hygro_aer1 = hygro_aer1 + dt * hygro_aer1_tend
    hygro_aer2 = hygro_aer2 + dt * hygro_aer2_tend
    hygro_aer3 = hygro_aer3 + dt * hygro_aer3_tend
    hygro_aer4 = hygro_aer4 + dt * hygro_aer4_tend
  end if 

  RETURN

  END SUBROUTINE crm_aer





END MODULE module_force_crm





