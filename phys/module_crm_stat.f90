























MODULE module_crm_stat
  IMPLICIT NONE
CONTAINS

SUBROUTINE crm_stat (                          &
     crm_stat_count, dt, itimestep, curr_secs, &
     stat_count_nocl, stat_count_nocc,         &
     fnm, fnp, mut, rdx, rdy,                  &
     z, z8w, dz8w,p8w,p,rho,th,tk,             &
     u, uphy, v, vphy, w,                      &
     qv, qc, qr, qi, qs, qg, qnc, tke_sgs,     &
     m_opt, sfs_opt,                           &
     m11, m12, m13, m22, m23, m33,             &
     xkmv, xkhv, h_diabatic,                   &
     qvdt_pr ,qvdt_cond ,qcdt_pr,              &
     qcdt_sed, qrdt_sed,                       &
     rthratensw, rthratenlw,                   &
     rthlsten,   rqvlsten,                     &
     w_dthdz, w_dqvdz, w_dudz, w_dvdz,         &
     thdt_lshor, qvdt_lshor,                   &
     thdt_lsrlx, qvdt_lsrlx,                   &
     udt_lsrlx,  vdt_lsrlx,                    &
     hfx, lh, ust, tsk,                        &
     raincv,  rainncv,                         &
     ra_sw_physics, ra_lw_physics,             &
     swupt, swuptc, swdnt, swdntc,             &
     swupb, swupbc, swdnb, swdnbc,             &
     lwupt, lwuptc, lwdnt, lwdntc,             &
     lwupb, lwupbc, lwdnb, lwdnbc,             &
     swupfd, swdnfd, lwupfd, lwdnfd,           &
     sedfqc, sedfqr,                           &
     smaxact, rminact,                         &

     cst_cldlow, cst_cldtot, cst_lwp, cst_iwp, cst_precw,            &
     cst_tke, cst_tsair, cst_ps,                                     &
     cst_prect, cst_sh, cst_lh, cst_fsntc, cst_fsnt, cst_flntc,      &
     cst_flnt, cst_fsnsc, cst_fsns, cst_flnsc, cst_flns,             &
     cst_swinc, cst_tsk, cst_ust,                                    &

     csp_z, csp_z8w, csp_dz8w, csp_u, csp_v, csp_w,                  &
     csp_p, csp_th, csp_thv, csp_thl,                                &
     csp_qv, csp_qc, csp_qi, csp_ql, csp_qf, csp_qt,                 &
     csp_lwc, csp_iwc, csp_speqv, csp_a_cl, csp_rho,                 &
     csp_u2, csp_v2, csp_u2v2, csp_w2, csp_w3,                       &
     csp_wskew, csp_th2, csp_thv2, csp_thl2, csp_qv2,                &
     csp_uw, csp_vw, csp_wth, csp_wthv, csp_wthl,                    &
     csp_wqv, csp_wqc, csp_wqi, csp_wql, csp_wqf, csp_wqt,           &
     csp_sedfqc, csp_sedfqr,                                         &
     csp_uw_sgs, csp_vw_sgs, csp_wth_sgs, csp_wthv_sgs, csp_wthl_sgs,&
     csp_wqv_sgs, csp_wqc_sgs, csp_wqi_sgs, csp_wql_sgs, csp_wqf_sgs,&
     csp_wqt_sgs, csp_thdt_cond, csp_thdt_lw, csp_thdt_sw,           &
     csp_thdt_ls, csp_qvdt_pr, csp_qvdt_cond, csp_qvdt_ls,           &
     csp_qcdt_pr, csp_qcdt_sed, csp_qrdt_sed,                        &
     csp_thdt_lshor, csp_qvdt_lshor, csp_thdt_lsver, csp_qvdt_lsver, &
     csp_thdt_lsrlx, csp_qvdt_lsrlx, csp_udt_ls,     csp_vdt_ls,     &
     csp_udt_lsver,  csp_vdt_lsver, csp_udt_lsrlx,  csp_vdt_lsrlx,   &
     csp_tke_rs, csp_tke_sh, csp_tke_bu,                             &
     csp_tke_tr, csp_tke_di, csp_tke_sgs,                            &
     csp_swupf, csp_swdnf, csp_lwupf, csp_lwdnf,                     &
     csp_w_c, csp_w2_c, csp_thl_c, csp_qt_c, csp_qv_c, csp_ql_c,     &
     csp_qf_c, csp_qc_c, csp_qi_c, csp_qnc_c,                        &
     csp_thv_c, csp_aw_c, csp_awthl_c, csp_awqt_c, csp_awqv_c,       &
     csp_awql_c, csp_awqf_c, csp_awqc_c, csp_awqi_c, csp_awthv_c,    &
     csp_a_cc, csp_w_cc, csp_w2_cc, csp_thl_cc, csp_qt_cc, csp_qv_cc,&
     csp_ql_cc, csp_qf_cc, csp_qc_cc, csp_qi_cc,                     &
     csp_thv_cc, csp_aw_cc, csp_awthl_cc,                            &
     csp_awqt_cc, csp_awqv_cc, csp_awql_cc, csp_awqf_cc,             &
     csp_awqc_cc, csp_awqi_cc,                                       &
     csp_awthv_cc, csp_sigc_thl, csp_sigc_qt, csp_sigc_ql,           &
     csp_sigc_qf, csp_sigc_qc, csp_sigc_qi,                          &
     csp_sigc_thv, csp_smaxactmax, csp_rminactmin,                   &
     csp_wmax, csp_wmin,                                             &

     csv_th, csv_u, csv_v, csv_w, csv_w2,                            &
     csv_qv, csv_qc, csv_qr, csv_qi, csv_qs, csv_qg,                 &
     csv_lwc, csv_iwc, csv_cldfrac,                                  &

     css_lwp, css_iwp, css_cldtot, css_cldlow,                       &

     ids,ide, jds,jde, kds,kde,                                      &
     ims,ime, jms,jme, kms,kme,                                      &
     ips,ipe, jps,jpe, kps,kpe,                                      &
     i_start,i_end,j_start,j_end,kts,kte,num_tiles                   )







  USE module_dm, ONLY: wrf_dm_sum_real8, wrf_dm_sum_real, wrf_dm_max_real, wrf_dm_min_real 
  USE module_model_constants, ONLY : g, cp, r_d, r_v, p0, rcp 
   
  USE module_state_description, ONLY : &
       CAMLWSCHEME, RRTMG_LWSCHEME, RRTMG_LWSCHEME_FAST, CGILSRAD_LWSCHEME, &
       CAMSWSCHEME, RRTMG_SWSCHEME, RRTMG_SWSCHEME_FAST, CGILSRAD_SWSCHEME

   IMPLICIT NONE



   
   INTEGER, INTENT(IN   )    ::                                  &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      ips,ipe, jps,jpe, kps,kpe, &
                                                        kts,kte, &
                                                      num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN   ) ::               &
                                                  i_start,i_end, &
                                                  j_start,j_end

   
   REAL,           INTENT(IN   ) :: dt, rdy, rdx
   REAL, OPTIONAL, INTENT(IN   )::  curr_secs

   INTEGER,        INTENT(IN   )::  crm_stat_count, & 
                                    itimestep         

   INTEGER,        INTENT(IN   ) :: ra_lw_physics, ra_sw_physics, m_opt, sfs_opt

   
   INTEGER, DIMENSION( kms:kme ), INTENT(INOUT):: stat_count_nocl, stat_count_nocc

   REAL , DIMENSION( kms:kme ), INTENT(IN   ) :: fnm, fnp


   
   REAL , DIMENSION( kms:kme ), INTENT(IN   ) ::                 &
                                          thdt_lshor, qvdt_lshor,&
                                          thdt_lsrlx, qvdt_lsrlx,&
                                          udt_lsrlx,  vdt_lsrlx  

   
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN   ) ::        &
                                                            mut, &
                                              hfx, lh, ust, tsk, &
                                                rainncv, raincv, &  
                                   swupt, swuptc, swdnt, swdntc, &
                                   swupb, swupbc, swdnb, swdnbc, &
                                   lwupt, lwuptc, lwdnt, lwdntc, &
                                   lwupb, lwupbc, lwdnb, lwdnbc

   
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN ) ::  &
                                                   z, z8w, dz8w, &
                                                    p8w, p, rho, &
                                                         th, tk, &
                                            u, uphy, v, vphy, w, &
                                         qv, qc, qr, qi, qs, qg, &
                                                            qnc, &
                                                        tke_sgs, &
                                    qvdt_pr ,qvdt_cond ,qcdt_pr, &
                                             qcdt_sed, qrdt_sed, &
                               w_dthdz, w_dqvdz, w_dudz, w_dvdz, &
                                   m11, m12, m13, m22, m23, m33, &
                                                     xkmv, xkhv, &
                                                     h_diabatic, &
                                         rthratenlw, rthratensw, &
                                         rthlsten,   rqvlsten,   &
                                 swupfd, swdnfd, lwupfd, lwdnfd, &
                                                 sedfqc, sedfqr, &
                                               smaxact, rminact

   
   REAL, INTENT(INOUT) ::  &
      cst_cldlow, cst_cldtot, cst_lwp, cst_iwp, cst_precw,        &
      cst_tke, cst_tsair, cst_ps, cst_prect, cst_sh, cst_lh,      &
      cst_fsntc, cst_fsnt, cst_flntc, cst_flnt, cst_fsnsc,        &
      cst_fsns, cst_flnsc, cst_flns, cst_swinc, cst_tsk, cst_ust

   
   REAL, DIMENSION( kms:kme ), INTENT(INOUT) ::                   &
    csp_z, csp_z8w, csp_dz8w, csp_u, csp_v, csp_w,                &
    csp_p, csp_th, csp_thv, csp_thl,                              &
    csp_qv, csp_qc, csp_qi, csp_ql, csp_qf, csp_qt, csp_lwc,      &
    csp_iwc, csp_speqv, csp_a_cl, csp_rho,                        &
    csp_u2, csp_v2, csp_u2v2, csp_w2, csp_w3, csp_wskew,          &
    csp_th2, csp_thv2, csp_thl2, csp_qv2,                         &
    csp_uw, csp_vw, csp_wth, csp_wthv, csp_wthl, csp_wqv,         &
    csp_wqc, csp_wqi, csp_wql, csp_wqf, csp_wqt,                  &
    csp_sedfqc, csp_sedfqr,                                       &
    csp_uw_sgs, csp_vw_sgs, csp_wth_sgs, csp_wthv_sgs,            &
    csp_wthl_sgs, csp_wqv_sgs, csp_wqc_sgs, csp_wqi_sgs,          &
    csp_wql_sgs, csp_wqf_sgs, csp_wqt_sgs, csp_thdt_cond,         &
    csp_thdt_lw, csp_thdt_sw, csp_thdt_ls, csp_qvdt_pr,           &
    csp_qvdt_cond, csp_qvdt_ls, csp_qcdt_pr, csp_qcdt_sed,        &
    csp_qrdt_sed, csp_thdt_lshor, csp_qvdt_lshor, csp_thdt_lsver, &
    csp_qvdt_lsver, csp_thdt_lsrlx, csp_qvdt_lsrlx,               &
    csp_udt_ls, csp_vdt_ls, csp_udt_lsver, csp_vdt_lsver,         &
    csp_udt_lsrlx, csp_vdt_lsrlx,                                 &
    csp_tke_rs, csp_tke_sh, csp_tke_bu, csp_tke_tr, csp_tke_di,   &
    csp_tke_sgs,                                                  &
    csp_swupf, csp_swdnf, csp_lwupf, csp_lwdnf,                   &
    csp_w_c, csp_w2_c, csp_thl_c, csp_qt_c, csp_qv_c, csp_ql_c,   &
    csp_qf_c, csp_qc_c, csp_qi_c, csp_qnc_c,                      &
    csp_thv_c, csp_aw_c, csp_awthl_c, csp_awqt_c, csp_awqv_c,     &
    csp_awql_c, csp_awqf_c, csp_awqc_c, csp_awqi_c, csp_awthv_c,  &
    csp_a_cc, csp_w_cc, csp_w2_cc, csp_thl_cc, csp_qt_cc,         &
    csp_qv_cc,                                                    &
    csp_ql_cc, csp_qf_cc, csp_qc_cc, csp_qi_cc,                   &
    csp_thv_cc, csp_aw_cc, csp_awthl_cc, csp_awqt_cc,             &
    csp_awqv_cc, csp_awql_cc, csp_awqf_cc, csp_awqc_cc,           &
    csp_awqi_cc, csp_awthv_cc, csp_sigc_thl, csp_sigc_qt,         &
    csp_sigc_ql, csp_sigc_qf, csp_sigc_qc, csp_sigc_qi,           &
    csp_sigc_thv, csp_smaxactmax, csp_rminactmin,                 &
    csp_wmax, csp_wmin

   
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: &
      csv_th, csv_u, csv_v, csv_w, csv_w2,                        &
      csv_qv, csv_qc, csv_qr, csv_qi, csv_qs, csv_qg,             &
      csv_lwc, csv_iwc, csv_cldfrac

   
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::        &
      css_lwp, css_iwp, css_cldtot, css_cldlow



   INTEGER :: i,j,k,its,ite,jts,jte,ij

   REAL    :: npts, npts_stagx, npts_stagy, stat_count_r
   REAL    :: cldmask, cldmask_low, rdovrv, xlv, thresh_cldfrac, &
              thresh_cldlow_z, exnerpi, V0

   REAL(KIND=8) :: tsair_sum, prect_sum, ps_sum, sh_sum, lh_sum, &
              fsnt_sum,  flnt_sum,  fsns_sum,  flns_sum,    &
              fsntc_sum, flntc_sum, fsnsc_sum, flnsc_sum,   &
              cldlow_sum, cldtot_sum, swinc_sum, tsk_sum, ust_sum

   REAL(KIND=8) :: uphy_sum, vphy_sum, wphy_sum,               &
              z_sum, z8w_sum, dz8w_sum, u_sum, v_sum, w_sum,   &
              p_sum, rho_sum, th_sum, thv_sum, thl_sum,        &
              qv_sum, qc_sum, qi_sum, ql_sum, qf_sum,          &
              tke_sgs_sum, lwc_sum, iwc_sum, speqv_sum,        & 
              u2_sum, v2_sum, w2_sum, w3_sum, uw_sum, vw_sum,  &
              th2_sum, thv2_sum, thl2_sum, qv2_sum,            &
              wth_sum, wthv_sum, wthl_sum,                     &
              wqv_sum, wqc_sum, wqi_sum, wql_sum, wqf_sum,     &
              sedfqc_sum, sedfqr_sum,                          &
              uw_sgs_sum,  vw_sgs_sum,  wth_sgs_sum,           &
              wqv_sgs_sum, wqc_sgs_sum, wqr_sgs_sum,           &
              wqi_sgs_sum, wqs_sgs_sum, wqg_sgs_sum,           &
              thdt_cond_sum, thdt_lw_sum, thdt_sw_sum,         &
              qvdt_pr_sum,  qvdt_cond_sum, qcdt_pr_sum,        &
              qcdt_sed_sum, qrdt_sed_sum,                      &
              thdt_lsver_sum, qvdt_lsver_sum,                  &
              udt_lsver_sum,  vdt_lsver_sum,                   &
              tke_rs_sum, tke_tr_sum, tke_di_sum,              &
              swupf_sum, swdnf_sum, lwupf_sum, lwdnf_sum,      &
              a_cl_sum, w_c_sum, w2_c_sum, thl_c_sum,          &
              qv_c_sum, qc_c_sum, qi_c_sum, qnc_c_sum,         &
              ql_c_sum, qf_c_sum, thv_c_sum,                   &
              awthl_c_sum, awqv_c_sum, awqc_c_sum, awqi_c_sum, &
              awql_c_sum, awqf_c_sum, awthv_c_sum,             &
              a_cc_sum, w_cc_sum, w2_cc_sum, thl_cc_sum,       &
              qv_cc_sum,                                       &
              qc_cc_sum, qi_cc_sum, ql_cc_sum, qf_cc_sum,      &
              thv_cc_sum, awthl_cc_sum, awqv_cc_sum,           &
              awqc_cc_sum, awqi_cc_sum, awql_cc_sum,           &
              awqf_cc_sum, awthv_cc_sum, sigc_thl_sum,         &
              sigc_thv_sum, sigc_qt_sum, sigc_qc_sum,          &
              sigc_qi_sum, sigc_ql_sum, sigc_qf_sum 

   REAL    :: iwp, lwp, precw, intg_tke, tke_tmp,                   &
              n_c1, n_c2, n_cc1, n_cc2

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::       i_c, i_cc, &
                                           iwc, lwc, ql, qf, speqv, &
                                                   thl, thv, wphy, dz 

   REAL, DIMENSION( ims:ime, jms:jme ) ::       p_p, u_p, v_p, w_p, &
                                                th_p, thv_p, thl_p, &
                                      qv_p, qc_p, qi_p, ql_p, qf_p, &
                                                      i_clow, i_ct

   REAL, DIMENSION( ims:ime , jms:jme ) :: &
              swupt_tmp, swuptc_tmp, swdnt_tmp, swdntc_tmp, &
              swupb_tmp, swupbc_tmp, swdnb_tmp, swdnbc_tmp, &
              lwupt_tmp, lwuptc_tmp, lwdnt_tmp, lwdntc_tmp, &
              lwupb_tmp, lwupbc_tmp, lwdnb_tmp, lwdnbc_tmp
              
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: &              
              swupfd_tmp, swdnfd_tmp, lwupfd_tmp, lwdnfd_tmp


   REAL, DIMENSION( kms:kme ) :: uphy_ave, vphy_ave, wphy_ave,          &
              z_ave, z8w_ave, dz8w_ave, p_ave,                          &
              rho_ave, th_ave, thv_ave, thl_ave, u_ave, v_ave, w_ave,   &
              qv_ave, qc_ave, qi_ave, ql_ave, qf_ave, tke_sgs_ave,      &
              lwc_ave, iwc_ave, speqv_ave,                              &
              u2_ave, v2_ave, w2_ave, w3_ave, wskew_ave, uw_ave, vw_ave,&
              th2_ave, thv2_ave, thl2_ave, qv2_ave,                     &
              wth_ave, wthv_ave, wthl_ave, wqv_ave, wqc_ave, wqi_ave,   &
              wql_ave, wqf_ave,   &
              sedfqc_ave, sedfqr_ave,                                   &
              uw_sgs_ave,  vw_sgs_ave,  wth_sgs_ave,                    &
              wqv_sgs_ave, wqc_sgs_ave, wqr_sgs_ave,                    &
              wqi_sgs_ave, wqs_sgs_ave, wqg_sgs_ave,                    &
              wthv_sgs_ave, wthl_sgs_ave, wql_sgs_ave, wqf_sgs_ave,     &
              wqt_sgs_ave,                                              &
              thdt_cond_ave, thdt_lw_ave, thdt_sw_ave,                  &
              qvdt_pr_ave,  qvdt_cond_ave, qcdt_pr_ave,                 &
              qcdt_sed_ave, qrdt_sed_ave,                               &
              thdt_lsver_ave, qvdt_lsver_ave, udt_lsver_ave, vdt_lsver_ave, &
              tke_rs_ave, tke_tr_ave, tke_di_ave, tke_sh_ave, tke_bu_ave, & 
              swupf_ave, swdnf_ave, lwupf_ave, lwdnf_ave,               &
              a_cl_ave, w_c_ave, w2_c_ave, thl_c_ave, qv_c_ave,         &
              qc_c_ave, qi_c_ave, qnc_c_ave, ql_c_ave, qf_c_ave,        &
              thv_c_ave, awthl_c_ave, awqv_c_ave, awqc_c_ave,           &
              awqi_c_ave, awql_c_ave, awqf_c_ave, awthv_c_ave,          &
              a_cc_ave, w_cc_ave, w2_cc_ave, thl_cc_ave, qv_cc_ave,     &
              qc_cc_ave, qi_cc_ave, ql_cc_ave, qf_cc_ave,               &
              thv_cc_ave, awthl_cc_ave, awqv_cc_ave, awqc_cc_ave,       &
              awqi_cc_ave, awql_cc_ave, awqf_cc_ave, awthv_cc_ave,      &
              sigc_thl_ave, sigc_thv_ave, sigc_qt_ave, sigc_qc_ave,     &
              sigc_qi_ave, sigc_ql_ave, sigc_qf_ave 

   REAL(KIND=8), DIMENSION( kms:kme ) :: i_cl_k, i_cc_k 

   CHARACTER(LEN=250) :: message

   LOGICAL :: reset_time
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor






   npts = real((ide-ids)*(jde-jds))    
   npts_stagx = real((ide-ids+1)*(jde-jds)) 
   npts_stagy = real((ide-ids)*(jde-jds+1)) 
   stat_count_r = real(crm_stat_count)

   reset_time = .false.
   if (itimestep==0 .or. crm_stat_count==0) reset_time = .true.
   
   rdovrv = r_d/r_v 

   thresh_cldfrac = 1.E-6 
   thresh_cldlow_z = 5000. 





   SELECT CASE (ra_sw_physics)
   CASE (CAMSWSCHEME, RRTMG_SWSCHEME, RRTMG_SWSCHEME_FAST)
      swupt_tmp(:,:) = swupt(:,:)
      swuptc_tmp(:,:) = swuptc(:,:)
      swdnt_tmp(:,:) = swdnt(:,:)
      swdntc_tmp(:,:) = swdntc(:,:)
      swupb_tmp(:,:) = swupb(:,:)
      swupbc_tmp(:,:) = swupbc(:,:)
      swdnb_tmp(:,:) = swdnb(:,:)
      swdnbc_tmp(:,:) = swdnbc(:,:)
      swupfd_tmp(:,:,:) = 0.
      swdnfd_tmp(:,:,:) = 0.
   CASE (CGILSRAD_SWSCHEME)
      swupt_tmp(:,:) = swupt(:,:)
      swuptc_tmp(:,:) = swuptc(:,:)
      swdnt_tmp(:,:) = swdnt(:,:)
      swdntc_tmp(:,:) = swdntc(:,:)
      swupb_tmp(:,:) = swupb(:,:)
      swupbc_tmp(:,:) = swupbc(:,:)
      swdnb_tmp(:,:) = swdnb(:,:)
      swdnbc_tmp(:,:) = swdnbc(:,:)
      swupfd_tmp(:,:,:) = swupfd(:,:,:)
      swdnfd_tmp(:,:,:) = swdnfd(:,:,:)
   CASE DEFAULT
      swupt_tmp(:,:) = 0.
      swuptc_tmp(:,:) = 0.
      swdnt_tmp(:,:) = 0.
      swdntc_tmp(:,:) = 0.
      swupb_tmp(:,:) = 0.
      swupbc_tmp(:,:) = 0.
      swdnb_tmp(:,:) = 0.
      swdnbc_tmp(:,:) = 0.
      swupfd_tmp(:,:,:) = 0.
      swdnfd_tmp(:,:,:) = 0.
   END SELECT
   SELECT CASE (ra_lw_physics)
   CASE (CAMLWSCHEME, RRTMG_LWSCHEME, RRTMG_LWSCHEME_FAST)
      lwupt_tmp(:,:) = lwupt(:,:)
      lwuptc_tmp(:,:) = lwuptc(:,:)
      lwdnt_tmp(:,:) = lwdnt(:,:)
      lwdntc_tmp(:,:) = lwdntc(:,:)
      lwupb_tmp(:,:) = lwupb(:,:)
      lwupbc_tmp(:,:) = lwupbc(:,:)
      lwdnb_tmp(:,:) = lwdnb(:,:)
      lwdnbc_tmp(:,:) = lwdnbc(:,:)
      lwupfd_tmp(:,:,:) = 0.
      lwdnfd_tmp(:,:,:) = 0.
   CASE (CGILSRAD_LWSCHEME)
      lwupt_tmp(:,:) = lwupt(:,:)
      lwuptc_tmp(:,:) = lwuptc(:,:)
      lwdnt_tmp(:,:) = lwdnt(:,:)
      lwdntc_tmp(:,:) = lwdntc(:,:)
      lwupb_tmp(:,:) = lwupb(:,:)
      lwupbc_tmp(:,:) = lwupbc(:,:)
      lwdnb_tmp(:,:) = lwdnb(:,:)
      lwdnbc_tmp(:,:) = lwdnbc(:,:)
      lwupfd_tmp(:,:,:) = lwupfd(:,:,:)
      lwdnfd_tmp(:,:,:) = lwdnfd(:,:,:)
   CASE DEFAULT
      lwupt_tmp(:,:) = 0.
      lwuptc_tmp(:,:) = 0.
      lwdnt_tmp(:,:) = 0.
      lwdntc_tmp(:,:) = 0.
      lwupb_tmp(:,:) = 0.
      lwupbc_tmp(:,:) = 0.
      lwdnb_tmp(:,:) = 0.
      lwdnbc_tmp(:,:) = 0.
      lwupfd_tmp(:,:,:) = 0.
      lwdnfd_tmp(:,:,:) = 0.
   END SELECT


   DO j = jps, min(jpe,jde-1)
   DO k = kts, kte
   DO i = ips, min(ipe,ide-1)
      wphy(i,k,j) = (w(i,k,j) + w(i,k+1,j))*0.5
      dz(i,k,j)   = z(i,k,j) - z(i,k-1,j)
   END DO
   END DO
   END DO

      

   IF (reset_time) &
        CALL crm_stat_reset(                                            &
        
        cst_cldlow, cst_cldtot, cst_lwp, cst_iwp, cst_precw, cst_tke,   &
        cst_tsair, cst_ps, cst_prect, cst_sh, cst_lh, cst_fsntc,        &
        cst_fsnt, cst_flntc, cst_flnt, cst_fsnsc, cst_fsns, cst_flnsc,  &
        cst_flns, cst_swinc, cst_tsk, cst_ust,                          &
        
        csp_z, csp_z8w, csp_dz8w, csp_u, csp_v, csp_w,                  &
        csp_p, csp_th, csp_thv, csp_thl,                                &
        csp_qv, csp_qc, csp_qi, csp_ql, csp_qf, csp_qt, csp_lwc,        &
        csp_iwc, csp_speqv, csp_a_cl, csp_rho, csp_u2, csp_v2,          &
        csp_u2v2, csp_w2, csp_w3, csp_wskew, csp_th2, csp_thv2,         &
        csp_thl2, csp_qv2, csp_uw, csp_vw, csp_wth, csp_wthv, csp_wthl, &
        csp_wqv, csp_wqc, csp_wqi, csp_wql, csp_wqf, csp_wqt,           &
        csp_sedfqc, csp_sedfqr,                                         &
        csp_uw_sgs, csp_vw_sgs, csp_wth_sgs, csp_wthv_sgs, csp_wthl_sgs,&
        csp_wqv_sgs, csp_wqc_sgs, csp_wqi_sgs, csp_wql_sgs, csp_wqf_sgs,&
        csp_wqt_sgs, csp_thdt_cond, csp_thdt_lw, csp_thdt_sw,           &
        csp_thdt_ls, csp_qvdt_pr, csp_qvdt_cond, csp_qvdt_ls,           &
        csp_qcdt_pr, csp_qcdt_sed, csp_qrdt_sed,                        &
        csp_thdt_lshor, csp_qvdt_lshor, csp_thdt_lsver, csp_qvdt_lsver, &
        csp_thdt_lsrlx, csp_qvdt_lsrlx, csp_udt_ls,     csp_vdt_ls,     &
        csp_udt_lsver,  csp_vdt_lsver, csp_udt_lsrlx,  csp_vdt_lsrlx,   &
        csp_tke_rs, csp_tke_sh, csp_tke_bu,                             &
        csp_tke_tr, csp_tke_di, csp_tke_sgs,                            &
        csp_swupf, csp_swdnf, csp_lwupf, csp_lwdnf,                     &
        csp_w_c, csp_w2_c, csp_thl_c, csp_qt_c, csp_qv_c, csp_ql_c,     &
        csp_qf_c,                                                       &
        csp_qc_c, csp_qi_c, csp_qnc_c, csp_thv_c, csp_aw_c, csp_awthl_c,&
        csp_awqt_c, csp_awqv_c, csp_awql_c, csp_awqf_c, csp_awqc_c,     &
        csp_awqi_c, csp_awthv_c, csp_a_cc, csp_w_cc, csp_w2_cc,         &
        csp_thl_cc,                                                     &
        csp_qt_cc, csp_qv_cc, csp_ql_cc, csp_qf_cc, csp_qc_cc,          &
        csp_qi_cc, csp_thv_cc, csp_aw_cc, csp_awthl_cc, csp_awqt_cc,    &
        csp_awqv_cc, csp_awql_cc, csp_awqf_cc, csp_awqc_cc, csp_awqi_cc,&
        csp_awthv_cc, csp_sigc_thl, csp_sigc_qt, csp_sigc_ql,           &
        csp_sigc_qf, csp_sigc_qc, csp_sigc_qi, csp_sigc_thv,            &
        csp_smaxactmax, csp_rminactmin, csp_wmax, csp_wmin,             &
        stat_count_nocl, stat_count_nocc,                               &
        
        csv_th, csv_u, csv_v, csv_w, csv_w2,                            &
        csv_qv, csv_qc, csv_qr, csv_qi, csv_qs, csv_qg,                 &
        csv_lwc, csv_iwc, csv_cldfrac,                                  &
        
        css_lwp, css_iwp, css_cldtot, css_cldlow,                       &
        
        ids,ide, jds,jde, kds,kde,                                      &
        ims,ime, jms,jme, kms,kme,                                      &
        its, ite, jts, jte, kts, kte                                    )




   
   tsair_sum = 0.
   prect_sum = 0.
   ps_sum    = 0.
   sh_sum    = 0.
   lh_sum    = 0.

   fsnt_sum  = 0. 
   flnt_sum  = 0. 
   fsns_sum  = 0. 
   flns_sum  = 0. 
   fsntc_sum = 0. 
   flntc_sum = 0. 
   fsnsc_sum = 0. 
   flnsc_sum = 0. 

   swinc_sum = 0.
   tsk_sum   = 0.
   ust_sum   = 0.

   DO j = jps, min(jpe,jde-1)
   DO i = ips, min(ipe,ide-1)

     tsair_sum = tsair_sum + tk(i,kts,j)
     prect_sum = prect_sum + (rainncv(i,j) + raincv(i,j))/dt 
     ps_sum = ps_sum + p(i,kts,j) 
     sh_sum = sh_sum + hfx(i,j)
     lh_sum = lh_sum + lh(i,j)

     fsnt_sum  = fsnt_sum  + swupt(i,j)  - swdnt(i,j)
     flnt_sum  = flnt_sum  + lwupt(i,j)  - lwdnt(i,j)
     fsns_sum  = fsns_sum  + swupb(i,j)  - swdnb(i,j)
     flns_sum  = flns_sum  + lwupb(i,j)  - lwdnb(i,j)
     fsntc_sum = fsntc_sum + swuptc(i,j) - swdntc(i,j)
     flntc_sum = flntc_sum + lwuptc(i,j) - lwdntc(i,j)
     fsnsc_sum = fsnsc_sum + swupbc(i,j) - swdnbc(i,j)
     flnsc_sum = flnsc_sum + lwupbc(i,j) - lwdnbc(i,j)

     swinc_sum = swinc_sum + swdnt(i,j)
     tsk_sum   = tsk_sum   + tsk(i,j)
     ust_sum   = ust_sum   + ust(i,j)

   END DO
   END DO

   tsair_sum = wrf_dm_sum_real8( tsair_sum )
   prect_sum = wrf_dm_sum_real8( prect_sum )
   ps_sum    = wrf_dm_sum_real8( ps_sum )
   sh_sum    = wrf_dm_sum_real8( sh_sum )
   lh_sum    = wrf_dm_sum_real8( lh_sum )

   fsnt_sum  = wrf_dm_sum_real8( fsnt_sum  )
   flnt_sum  = wrf_dm_sum_real8( flnt_sum  )
   fsns_sum  = wrf_dm_sum_real8( fsns_sum  )
   flns_sum  = wrf_dm_sum_real8( flns_sum  )
   fsntc_sum = wrf_dm_sum_real8( fsntc_sum )
   flntc_sum = wrf_dm_sum_real8( flntc_sum )
   fsnsc_sum = wrf_dm_sum_real8( fsnsc_sum )
   flnsc_sum = wrf_dm_sum_real8( flnsc_sum )

   swinc_sum = wrf_dm_sum_real8( swinc_sum )
   tsk_sum   = wrf_dm_sum_real8( tsk_sum   )
   ust_sum   = wrf_dm_sum_real8( ust_sum   )



   cst_tsair = (cst_tsair * stat_count_r + tsair_sum / npts) / (stat_count_r + 1.)
   cst_prect = (cst_prect * stat_count_r + prect_sum / npts) / (stat_count_r + 1.)
   cst_ps    = (cst_ps    * stat_count_r + ps_sum    / npts) / (stat_count_r + 1.) 
   cst_sh    = (cst_sh    * stat_count_r + sh_sum    / npts) / (stat_count_r + 1.)
   cst_lh    = (cst_lh    * stat_count_r + lh_sum    / npts) / (stat_count_r + 1.)

   cst_fsnt  = (cst_fsnt  * stat_count_r + fsnt_sum  / npts) / (stat_count_r + 1.)
   cst_flnt  = (cst_flnt  * stat_count_r + flnt_sum  / npts) / (stat_count_r + 1.)
   cst_fsns  = (cst_fsns  * stat_count_r + fsns_sum  / npts) / (stat_count_r + 1.)
   cst_flns  = (cst_flns  * stat_count_r + flns_sum  / npts) / (stat_count_r + 1.)
   cst_fsntc = (cst_fsntc * stat_count_r + fsntc_sum / npts) / (stat_count_r + 1.)
   cst_flntc = (cst_flntc * stat_count_r + flntc_sum / npts) / (stat_count_r + 1.)
   cst_fsnsc = (cst_fsnsc * stat_count_r + fsnsc_sum / npts) / (stat_count_r + 1.)
   cst_flnsc = (cst_flnsc * stat_count_r + flnsc_sum / npts) / (stat_count_r + 1.)

   cst_swinc = (cst_swinc * stat_count_r + swinc_sum / npts) / (stat_count_r + 1.)
   cst_tsk   = (cst_tsk   * stat_count_r + tsk_sum   / npts) / (stat_count_r + 1.)
   cst_ust   = (cst_ust   * stat_count_r + ust_sum   / npts) / (stat_count_r + 1.)






   
   DO k = kts, kte+1
      z8w_sum = 0.
      w_sum   = 0.

      DO j = jps, min(jpe,jde-1)
      DO i = ips, min(ipe,ide-1)
         z8w_sum = z8w_sum + z8w(i,k,j)
         w_sum   = w_sum   + w(i,k,j)
      END DO 
      END DO 

      z8w_sum = wrf_dm_sum_real8( z8w_sum )
      w_sum   = wrf_dm_sum_real8( w_sum   )

      z8w_ave(k) = z8w_sum / npts
      w_ave(k)   = w_sum   / npts
   END DO 

   csp_z8w = (csp_z8w * stat_count_r + z8w_ave)/(stat_count_r + 1.)
   csp_w   = (csp_w   * stat_count_r + w_ave  )/(stat_count_r + 1.)

   
   DO k = kts, kte
      u_sum = 0.

      DO j = jps, min(jpe,jde-1)
      DO i = ips, min(ipe,ide)
         u_sum = u_sum + u(i,k,j)
      END DO 
      END DO 

      u_sum    = wrf_dm_sum_real8( u_sum )
      u_ave(k) = u_sum / npts_stagx
   END DO 
   u_ave(kte+1) = 0. 

   csp_u = (csp_u * stat_count_r + u_ave)/(stat_count_r + 1.)

   
   DO k = kts, kte
      v_sum = 0.

      DO j = jps, min(jpe,jde)
      DO i = ips, min(ipe,ide-1)
         v_sum = v_sum + v(i,k,j)
      END DO 
      END DO 

      v_sum    = wrf_dm_sum_real8( v_sum )
      v_ave(k) = v_sum / npts_stagy

   END DO 
   v_ave(kte+1) = 0. 

   csp_v = (csp_v * stat_count_r + v_ave)/(stat_count_r + 1.)

   
   lwp   = 0.  
   iwp   = 0.
   precw = 0.

   DO k = kts, kte
     uphy_sum = 0.  
     vphy_sum = 0.  
     wphy_sum = 0.

     z_sum   = 0.
     dz8w_sum= 0.
     p_sum   = 0.
     rho_sum = 0. 
     th_sum  = 0.
     thl_sum = 0.
     thv_sum = 0.
     qv_sum  = 0.
     qc_sum  = 0.  
     qi_sum  = 0.
     ql_sum  = 0.
     qf_sum  = 0.

     swupf_sum = 0.
     swdnf_sum = 0.
     lwupf_sum = 0.
     lwdnf_sum = 0.

     sedfqc_sum = 0.
     sedfqr_sum = 0.

     thdt_cond_sum = 0.
     thdt_lw_sum   = 0.
     thdt_sw_sum   = 0.
     qvdt_pr_sum   = 0.
     qvdt_cond_sum = 0.
     qcdt_pr_sum   = 0.
     qcdt_sed_sum  = 0.
     qrdt_sed_sum  = 0.

     tke_sgs_sum = 0.
     lwc_sum   = 0.
     iwc_sum   = 0.
     speqv_sum = 0.

     DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)

       ql(i,k,j)    = qc(i,k,j) + qr(i,k,j)
       qf(i,k,j)    = qi(i,k,j) + qs(i,k,j) + qg(i,k,j)
       lwc(i,k,j)   = ql(i,k,j) * rho(i,k,j)
       iwc(i,k,j)   = qf(i,k,j) * rho(i,k,j)
       speqv(i,k,j) = qv(i,k,j) * rho(i,k,j)

       xlv = 3.148E6 - 2370 * tk(i,k,j)
       thl(i,k,j) = (P0 / p(i,k,j))**rcp * (tk(i,k,j) - xlv * ql(i,k,j) / cp)    
       thv(i,k,j) = th(i,k,j) *(1. + rdovrv * qv(i,k,j) - ql(i,k,j))

       uphy_sum = uphy_sum + uphy(i,k,j)
       vphy_sum = vphy_sum + vphy(i,k,j)
       wphy_sum = wphy_sum + wphy(i,k,j)

       z_sum   = z_sum   + z(i,k,j)
       dz8w_sum= dz8w_sum+ dz8w(i,k,j)
       p_sum   = p_sum   + p(i,k,j)
       rho_sum = rho_sum + rho(i,k,j)
       th_sum  = th_sum  + th(i,k,j)
       thl_sum = thl_sum + thl(i,k,j)
       thv_sum = thv_sum + thv(i,k,j)
       qv_sum  = qv_sum  + qv(i,k,j)
       qc_sum  = qc_sum  + qc(i,k,j)
       qi_sum  = qi_sum  + qi(i,k,j)
       ql_sum  = ql_sum  + ql(i,k,j)
       qf_sum  = qf_sum  + qf(i,k,j)

       swupf_sum = swupf_sum + swupfd_tmp(i,k,j) 
       swdnf_sum = swdnf_sum + swdnfd_tmp(i,k,j) 
       lwupf_sum = lwupf_sum + lwupfd_tmp(i,k,j) 
       lwdnf_sum = lwdnf_sum + lwdnfd_tmp(i,k,j) 

       sedfqc_sum = sedfqc_sum + sedfqc(i,k,j)
       sedfqr_sum = sedfqr_sum + sedfqr(i,k,j)

       thdt_cond_sum = thdt_cond_sum + h_diabatic(i,k,j)
       thdt_lw_sum   = thdt_lw_sum   + rthratenlw(i,k,j) 
       thdt_sw_sum   = thdt_sw_sum   + rthratensw(i,k,j) 
       qvdt_pr_sum   = qvdt_pr_sum   + qvdt_pr(i,k,j)
       qvdt_cond_sum = qvdt_cond_sum + qvdt_cond(i,k,j)
       qcdt_pr_sum   = qcdt_pr_sum   + qcdt_pr(i,k,j)
       qcdt_sed_sum  = qcdt_sed_sum  + qcdt_sed(i,k,j)
       qrdt_sed_sum  = qrdt_sed_sum  + qrdt_sed(i,k,j)

       tke_sgs_sum  = tke_sgs_sum  + tke_sgs(i,k,j) 

       lwc_sum   = lwc_sum   + lwc(i,k,j)
       iwc_sum   = iwc_sum   + iwc(i,k,j)
       speqv_sum = speqv_sum + speqv(i,k,j)

     END DO 
     END DO 

     uphy_sum= wrf_dm_sum_real8( uphy_sum)
     vphy_sum= wrf_dm_sum_real8( vphy_sum)
     wphy_sum= wrf_dm_sum_real8( wphy_sum)

     z_sum   = wrf_dm_sum_real8( z_sum   )
     dz8w_sum= wrf_dm_sum_real8( dz8w_sum )
     p_sum   = wrf_dm_sum_real8( p_sum   )
     rho_sum = wrf_dm_sum_real8( rho_sum )
     th_sum  = wrf_dm_sum_real8( th_sum  )
     thl_sum = wrf_dm_sum_real8( thl_sum )
     thv_sum = wrf_dm_sum_real8( thv_sum )
     qv_sum  = wrf_dm_sum_real8( qv_sum  )
     qc_sum  = wrf_dm_sum_real8( qc_sum  )
     qi_sum  = wrf_dm_sum_real8( qi_sum  )
     ql_sum  = wrf_dm_sum_real8( ql_sum  )
     qf_sum  = wrf_dm_sum_real8( qf_sum  )

     tke_sgs_sum = wrf_dm_sum_real8( tke_sgs_sum  )

     lwc_sum   = wrf_dm_sum_real8( lwc_sum ) 
     iwc_sum   = wrf_dm_sum_real8( iwc_sum ) 
     speqv_sum = wrf_dm_sum_real8( speqv_sum ) 

     swupf_sum = wrf_dm_sum_real8( swupf_sum )
     swdnf_sum = wrf_dm_sum_real8( swdnf_sum )
     lwupf_sum = wrf_dm_sum_real8( lwupf_sum )
     lwdnf_sum = wrf_dm_sum_real8( lwdnf_sum )

     sedfqc_sum = wrf_dm_sum_real8( sedfqc_sum )
     sedfqr_sum = wrf_dm_sum_real8( sedfqr_sum )     

     thdt_cond_sum = wrf_dm_sum_real8( thdt_cond_sum )
     thdt_lw_sum   = wrf_dm_sum_real8( thdt_lw_sum   )
     thdt_sw_sum   = wrf_dm_sum_real8( thdt_sw_sum   )
     qvdt_pr_sum   = wrf_dm_sum_real8( qvdt_pr_sum   )
     qvdt_cond_sum = wrf_dm_sum_real8( qvdt_cond_sum )
     qcdt_pr_sum   = wrf_dm_sum_real8( qcdt_pr_sum   )
     qcdt_sed_sum  = wrf_dm_sum_real8( qcdt_sed_sum  )
     qrdt_sed_sum  = wrf_dm_sum_real8( qrdt_sed_sum  )

     uphy_ave(k)= uphy_sum/ npts
     vphy_ave(k)= vphy_sum/ npts
     wphy_ave(k)= wphy_sum/ npts

     z_ave(k)   = z_sum   / npts
     dz8w_ave(k)= dz8w_sum/ npts
     p_ave(k)   = p_sum   / npts
     rho_ave(k) = rho_sum / npts
     th_ave(k)  = th_sum  / npts
     thl_ave(k) = thl_sum / npts
     thv_ave(k) = thv_sum / npts
     qv_ave(k)  = qv_sum  / npts
     qc_ave(k)  = qc_sum  / npts
     qi_ave(k)  = qi_sum  / npts
     ql_ave(k)  = ql_sum  / npts
     qf_ave(k)  = qf_sum  / npts

     swupf_ave(k) = swupf_sum / npts
     swdnf_ave(k) = swdnf_sum / npts
     lwupf_ave(k) = lwupf_sum / npts
     lwdnf_ave(k) = lwdnf_sum / npts

     sedfqc_ave(k) = sedfqc_sum / npts
     sedfqr_ave(k) = sedfqr_sum / npts

     thdt_cond_ave(k) = thdt_cond_sum / npts
     thdt_lw_ave(k)   = thdt_lw_sum   / npts
     thdt_sw_ave(k)   = thdt_sw_sum   / npts
     qvdt_pr_ave(k)   = qvdt_pr_sum   / npts
     qvdt_cond_ave(k) = qvdt_cond_sum / npts
     qcdt_pr_ave(k)   = qcdt_pr_sum   / npts
     qcdt_sed_ave(k)  = qcdt_sed_sum  / npts
     qrdt_sed_ave(k)  = qrdt_sed_sum  / npts

     

     tke_sgs_ave(k) = tke_sgs_sum / npts
     
     lwc_ave(k) = lwc_sum / npts
     lwp = lwp +  lwc_ave(k) * dz8w_ave(k)

     iwc_ave(k) = iwc_sum / npts
     iwp = iwp +  iwc_ave(k) * dz8w_ave(k)

     speqv_ave(k) = speqv_sum / npts
     precw = precw + speqv_ave(k) * dz8w_ave(k)
 
   END DO 

   

   csp_z    = (csp_z    * stat_count_r + z_ave   )/(stat_count_r + 1.)  
   csp_dz8w = (csp_dz8w * stat_count_r + dz8w_ave)/(stat_count_r + 1.)  
   csp_p    = (csp_p    * stat_count_r + p_ave   )/(stat_count_r + 1.)
   csp_rho  = (csp_rho  * stat_count_r + rho_ave )/(stat_count_r + 1.)
   csp_th   = (csp_th   * stat_count_r + th_ave  )/(stat_count_r + 1.)
   csp_thv  = (csp_thv  * stat_count_r + thv_ave )/(stat_count_r + 1.)
   csp_thl  = (csp_thl  * stat_count_r + thl_ave )/(stat_count_r + 1.)
   csp_qv   = (csp_qv   * stat_count_r + qv_ave  )/(stat_count_r + 1.)
   csp_qc   = (csp_qc   * stat_count_r + qc_ave  )/(stat_count_r + 1.)
   csp_qi   = (csp_qi   * stat_count_r + qi_ave  )/(stat_count_r + 1.)
   csp_ql   = (csp_ql   * stat_count_r + ql_ave  )/(stat_count_r + 1.)
   csp_qf   = (csp_qf   * stat_count_r + qf_ave  )/(stat_count_r + 1.)

   csp_swupf = (csp_swupf * stat_count_r + swupf_ave)/(stat_count_r + 1.)
   csp_swdnf = (csp_swdnf * stat_count_r + swdnf_ave)/(stat_count_r + 1.)
   csp_lwupf = (csp_lwupf * stat_count_r + lwupf_ave)/(stat_count_r + 1.)
   csp_lwdnf = (csp_lwdnf * stat_count_r + lwdnf_ave)/(stat_count_r + 1.)

   csp_sedfqc  = (csp_sedfqc   * stat_count_r + sedfqc_ave )/(stat_count_r + 1.)
   csp_sedfqr  = (csp_sedfqr   * stat_count_r + sedfqr_ave )/(stat_count_r + 1.)

   csp_tke_sgs = (csp_tke_sgs  * stat_count_r + tke_sgs_ave )/(stat_count_r + 1.)
   csp_lwc     = (csp_lwc      * stat_count_r + lwc_ave     )/(stat_count_r + 1.)
   csp_iwc     = (csp_iwc      * stat_count_r + iwc_ave     )/(stat_count_r + 1.)
   csp_speqv   = (csp_speqv    * stat_count_r + speqv_ave   )/(stat_count_r + 1.)

   csp_qt = csp_qv + csp_ql + csp_qf

   csp_thdt_cond = (csp_thdt_cond * stat_count_r + thdt_cond_ave)/(stat_count_r + 1.)
   csp_thdt_lw   = (csp_thdt_lw   * stat_count_r + thdt_lw_ave  )/(stat_count_r + 1.) 
   csp_thdt_sw   = (csp_thdt_sw   * stat_count_r + thdt_sw_ave  )/(stat_count_r + 1.) 
   csp_qvdt_pr   = (csp_qvdt_pr   * stat_count_r + qvdt_pr_ave  )/(stat_count_r + 1.) 
   csp_qvdt_cond = (csp_qvdt_cond * stat_count_r + qvdt_cond_ave)/(stat_count_r + 1.) 
   csp_qcdt_pr   = (csp_qcdt_pr   * stat_count_r + qcdt_pr_ave  )/(stat_count_r + 1.) 
   csp_qcdt_sed  = (csp_qcdt_sed  * stat_count_r + qcdt_sed_ave )/(stat_count_r + 1.) 
   csp_qrdt_sed  = (csp_qrdt_sed  * stat_count_r + qrdt_sed_ave )/(stat_count_r + 1.) 

   
   cst_lwp   = (cst_lwp   * stat_count_r + lwp   )/(stat_count_r + 1.)
   cst_iwp   = (cst_iwp   * stat_count_r + iwp   )/(stat_count_r + 1.)
   cst_precw = (cst_precw * stat_count_r + precw )/(stat_count_r + 1.)






   DO k = kts, kte
     thdt_lsver_sum  = 0.
     qvdt_lsver_sum  = 0.
     udt_lsver_sum   = 0.
     vdt_lsver_sum   = 0.

     IF (k .eq. kte) then 
       DO j = jps, min(jpe,jde-1)
       DO i = ips, min(ipe,ide-1)
       thdt_lsver_sum = thdt_lsver_sum + w_dthdz(i,k,j) 
       qvdt_lsver_sum = qvdt_lsver_sum + w_dqvdz(i,k,j) 
       udt_lsver_sum  = udt_lsver_sum  + w_dudz(i,k,j) 
       vdt_lsver_sum  = vdt_lsver_sum  + w_dvdz(i,k,j) 
       END DO
       END DO
     ELSE 
       DO j = jps, min(jpe,jde-1)
       DO i = ips, min(ipe,ide-1)
       thdt_lsver_sum = thdt_lsver_sum + 0.5*(w_dthdz(i,k,j) + w_dthdz(i,k+1,j))
       qvdt_lsver_sum = qvdt_lsver_sum + 0.5*(w_dqvdz(i,k,j) + w_dqvdz(i,k+1,j))
       udt_lsver_sum  = udt_lsver_sum  + 0.5*(w_dudz(i,k,j)  + w_dudz(i,k+1,j)) 
       vdt_lsver_sum  = vdt_lsver_sum  + 0.5*(w_dvdz(i,k,j)  + w_dvdz(i,k+1,j)) 
       END DO
       END DO
     END IF

     thdt_lsver_sum = wrf_dm_sum_real8( thdt_lsver_sum )
     qvdt_lsver_sum = wrf_dm_sum_real8( qvdt_lsver_sum )
     udt_lsver_sum  = wrf_dm_sum_real8( udt_lsver_sum  )
     vdt_lsver_sum  = wrf_dm_sum_real8( vdt_lsver_sum  )

     thdt_lsver_ave(k)   = thdt_lsver_sum / npts
     qvdt_lsver_ave(k)   = qvdt_lsver_sum / npts
     udt_lsver_ave(k)    = udt_lsver_sum  / npts 
     vdt_lsver_ave(k)    = vdt_lsver_sum  / npts 
   END DO 

   

   csp_thdt_lsver  = (csp_thdt_lsver  * stat_count_r + thdt_lsver_ave )/(stat_count_r + 1.) 
   csp_qvdt_lsver  = (csp_qvdt_lsver  * stat_count_r + qvdt_lsver_ave )/(stat_count_r + 1.) 
   csp_udt_lsver   = (csp_udt_lsver   * stat_count_r + udt_lsver_ave  )/(stat_count_r + 1.) 
   csp_vdt_lsver   = (csp_vdt_lsver   * stat_count_r + vdt_lsver_ave  )/(stat_count_r + 1.) 

   
   csp_thdt_lshor  = (csp_thdt_lshor * stat_count_r + thdt_lshor )/(stat_count_r + 1.) 
   csp_qvdt_lshor  = (csp_qvdt_lshor * stat_count_r + qvdt_lshor )/(stat_count_r + 1.)
   csp_thdt_lsrlx  = (csp_thdt_lsrlx * stat_count_r + thdt_lsrlx )/(stat_count_r + 1.) 
   csp_qvdt_lsrlx  = (csp_qvdt_lsrlx * stat_count_r + qvdt_lsrlx )/(stat_count_r + 1.)
   csp_udt_lsrlx   = (csp_udt_lsrlx  * stat_count_r + udt_lsrlx  )/(stat_count_r + 1.) 
   csp_vdt_lsrlx   = (csp_vdt_lsrlx  * stat_count_r + vdt_lsrlx  )/(stat_count_r + 1.) 
   
   csp_thdt_ls  = (csp_thdt_ls * stat_count_r + thdt_lsver_ave+thdt_lsrlx+thdt_lshor)/(stat_count_r + 1.) 
   csp_qvdt_ls  = (csp_qvdt_ls * stat_count_r + qvdt_lsver_ave+qvdt_lsrlx+qvdt_lshor)/(stat_count_r + 1.)
   csp_udt_ls   = (csp_udt_ls  * stat_count_r + udt_lsver_ave +udt_lsrlx )/(stat_count_r + 1.) 
   csp_vdt_ls   = (csp_vdt_ls  * stat_count_r + vdt_lsver_ave +vdt_lsrlx )/(stat_count_r + 1.) 




   DO k = kts+1, kte 

     uw_sgs_sum   = 0.
     vw_sgs_sum   = 0.
     wth_sgs_sum  = 0.
     wqv_sgs_sum  = 0.
     wqc_sgs_sum  = 0.
     wqr_sgs_sum  = 0.
     wqi_sgs_sum  = 0.
     wqs_sgs_sum  = 0.
     wqg_sgs_sum  = 0.

     
     
     IF (m_opt>0 .or. sfs_opt>0) THEN
        DO j = jps, min(jpe,jde-1)
        DO i = ips, min(ipe,ide-1)

           uw_sgs_sum   = uw_sgs_sum + m13(i,k,j)
           vw_sgs_sum   = vw_sgs_sum + m23(i,k,j)

           
           wth_sgs_sum  = wth_sgs_sum - (fnm(k)*xkhv(i,k,j)+fnp(k)*xkhv(i,k-1,j)) &
                                &  *(th(i,k,j)-th(i,k-1,j)) / dz(i,k,j)
           wqv_sgs_sum  = wqv_sgs_sum  - (fnm(k)*xkhv(i,k,j)+fnp(k)*xkhv(i,k-1,j)) &
                                &  *(qv(i,k,j)-qv(i,k-1,j)) / dz(i,k,j)
           wqc_sgs_sum  = wqc_sgs_sum  - (fnm(k)*xkhv(i,k,j)+fnp(k)*xkhv(i,k-1,j)) &
                                &  *(qc(i,k,j)-qc(i,k-1,j)) / dz(i,k,j)
           wqr_sgs_sum  = wqr_sgs_sum  - (fnm(k)*xkhv(i,k,j)+fnp(k)*xkhv(i,k-1,j)) &
                                &  *(qr(i,k,j)-qr(i,k-1,j)) / dz(i,k,j)
           wqi_sgs_sum  = wqi_sgs_sum  - (fnm(k)*xkhv(i,k,j)+fnp(k)*xkhv(i,k-1,j)) &
                                &  *(qi(i,k,j)-qi(i,k-1,j)) / dz(i,k,j)
           wqs_sgs_sum  = wqs_sgs_sum  - (fnm(k)*xkhv(i,k,j)+fnp(k)*xkhv(i,k-1,j)) &
                                &  *(qs(i,k,j)-qs(i,k-1,j)) / dz(i,k,j)
           wqg_sgs_sum  = wqg_sgs_sum  - (fnm(k)*xkhv(i,k,j)+fnp(k)*xkhv(i,k-1,j)) &
                                &  *(qg(i,k,j)-qg(i,k-1,j)) / dz(i,k,j)

        END DO
        END DO

        uw_sgs_sum  = wrf_dm_sum_real8( uw_sgs_sum  )
        vw_sgs_sum  = wrf_dm_sum_real8( vw_sgs_sum  )
        wth_sgs_sum = wrf_dm_sum_real8( wth_sgs_sum )
        wqv_sgs_sum = wrf_dm_sum_real8( wqv_sgs_sum )
        wqc_sgs_sum = wrf_dm_sum_real8( wqc_sgs_sum )
        wqr_sgs_sum = wrf_dm_sum_real8( wqr_sgs_sum )
        wqi_sgs_sum = wrf_dm_sum_real8( wqi_sgs_sum )
        wqs_sgs_sum = wrf_dm_sum_real8( wqs_sgs_sum )
        wqg_sgs_sum = wrf_dm_sum_real8( wqg_sgs_sum )
     END IF

     uw_sgs_ave(k)  = uw_sgs_sum  / npts
     vw_sgs_ave(k)  = vw_sgs_sum  / npts 
     wth_sgs_ave(k) = wth_sgs_sum / npts 
     wqv_sgs_ave(k) = wqv_sgs_sum / npts 
     wqc_sgs_ave(k) = wqc_sgs_sum / npts 
     wqr_sgs_ave(k) = wqr_sgs_sum / npts 
     wqi_sgs_ave(k) = wqi_sgs_sum / npts 
     wqs_sgs_ave(k) = wqs_sgs_sum / npts 
     wqg_sgs_ave(k) = wqg_sgs_sum / npts 

   END DO


   

   uw_sgs_sum = 0.
   vw_sgs_sum = 0.
   DO j = jps, min(jpe,jde-1)
   DO i = ips, min(ipe,ide-1)
     V0 = ( u(i,kts,j)**2 + v(i,kts,j)**2 )**0.5
     uw_sgs_sum = uw_sgs_sum + ust(i,j) * u(i,kts,j)/V0 
     vw_sgs_sum = vw_sgs_sum + ust(i,j) * v(i,kts,j)/V0
   END DO
   END DO

   uw_sgs_sum   = wrf_dm_sum_real8( uw_sgs_sum )
   vw_sgs_sum   = wrf_dm_sum_real8( vw_sgs_sum )

   uw_sgs_ave(kts) = uw_sgs_sum / npts
   vw_sgs_ave(kts) = vw_sgs_sum / npts

   xlv = 3.148E6 - 2370 * th_ave(kts) * (p_ave(kts)/p0)**rcp
   wth_sgs_ave(kts) = sh_sum / npts / cp  / rho_ave(kts)
   wqv_sgs_ave(kts) = lh_sum / npts / xlv / rho_ave(kts)

   wqc_sgs_ave(kts) = 0.
   wqr_sgs_ave(kts) = 0.
   wqi_sgs_ave(kts) = 0.
   wqs_sgs_ave(kts) = 0.
   wqg_sgs_ave(kts) = 0.
  
   
   do k = kts, kte-1
     uw_sgs_ave(k) = (uw_sgs_ave(k)+uw_sgs_ave(k+1))*0.5
     vw_sgs_ave(k) = (vw_sgs_ave(k)+vw_sgs_ave(k+1))*0.5
     wqv_sgs_ave(k) = (wqv_sgs_ave(k)+wqv_sgs_ave(k+1))*0.5
     wqc_sgs_ave(k) = (wqc_sgs_ave(k)+wqc_sgs_ave(k+1))*0.5
     wqr_sgs_ave(k) = (wqr_sgs_ave(k)+wqr_sgs_ave(k+1))*0.5
     wqi_sgs_ave(k) = (wqi_sgs_ave(k)+wqi_sgs_ave(k+1))*0.5
     wqs_sgs_ave(k) = (wqs_sgs_ave(k)+wqs_sgs_ave(k+1))*0.5
     wqg_sgs_ave(k) = (wqg_sgs_ave(k)+wqg_sgs_ave(k+1))*0.5
     wth_sgs_ave(k) = (wth_sgs_ave(k)+wth_sgs_ave(k+1))*0.5
   end do
   

   wql_sgs_ave  = wqc_sgs_ave + wqr_sgs_ave
   wqf_sgs_ave  = wqi_sgs_ave + wqs_sgs_ave + wqg_sgs_ave
   wqt_sgs_ave  = wqv_sgs_ave + wql_sgs_ave + wqf_sgs_ave

   wthv_sgs_ave = rdovrv * th_ave * wqv_sgs_ave - th_ave * wql_sgs_ave &
                & + wth_sgs_ave * (1 + rdovrv * qv_ave - ql_ave) 

   DO k = kts, kte
     exnerpi = (p_ave(k)/p0)**rcp 
     xlv   = 3.148E6 - 2370 * th_ave(k) * exnerpi
     wthl_sgs_ave(k) = wth_sgs_ave(k) - xlv / exnerpi / cp * wql_sgs_ave(k)
   END DO

   

   csp_uw_sgs   = (csp_uw_sgs   * stat_count_r + uw_sgs_ave   )/(stat_count_r + 1.)
   csp_vw_sgs   = (csp_vw_sgs   * stat_count_r + vw_sgs_ave   )/(stat_count_r + 1.)

   csp_wth_sgs  = (csp_wth_sgs  * stat_count_r + wth_sgs_ave  )/(stat_count_r + 1.)
   csp_wqv_sgs  = (csp_wqv_sgs  * stat_count_r + wqv_sgs_ave  )/(stat_count_r + 1.)
   csp_wqc_sgs  = (csp_wqc_sgs  * stat_count_r + wqc_sgs_ave  )/(stat_count_r + 1.)
   csp_wqi_sgs  = (csp_wqi_sgs  * stat_count_r + wqi_sgs_ave  )/(stat_count_r + 1.)

   csp_wql_sgs  = (csp_wql_sgs  * stat_count_r + wql_sgs_ave  )/(stat_count_r + 1.)
   csp_wqf_sgs  = (csp_wqf_sgs  * stat_count_r + wqf_sgs_ave  )/(stat_count_r + 1.)
   csp_wqt_sgs  = (csp_wqt_sgs  * stat_count_r + wqt_sgs_ave  )/(stat_count_r + 1.)
   csp_wthv_sgs = (csp_wthv_sgs * stat_count_r + wthv_sgs_ave )/(stat_count_r + 1.)
   csp_wthl_sgs = (csp_wthl_sgs * stat_count_r + wthl_sgs_ave )/(stat_count_r + 1.)




   DO j = jps, min(jpe,jde-1)
   DO i = ips, min(ipe,ide-1)
     i_clow(i,j) = 0.
     i_ct(i,j) = 0.
   END DO
   END DO

   
   
   DO k = kts, kte+1
     w2_sum = 0.
     w3_sum = 0.

     DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)
        w_p(i,j) = w(i,k,j) - w_ave(k)

        w2_sum = w2_sum + w_p(i,j)**2
        w3_sum = w3_sum + w_p(i,j)**3
     END DO 
     END DO 

     w2_sum = wrf_dm_sum_real8( w2_sum )
     w3_sum = wrf_dm_sum_real8( w3_sum )

     w2_ave(k) = w2_sum / npts
     w3_ave(k) = w3_sum / npts
   END DO 

   csp_w2 = (csp_w2 * stat_count_r + w2_ave)/(stat_count_r + 1.)
   csp_w3 = (csp_w3 * stat_count_r + w3_ave)/(stat_count_r + 1.)

   
   DO k = kts, kte
     u2_sum   = 0.

     DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide)
        u_p(i,j) = u(i,k,j) - u_ave(k)

        u2_sum = u2_sum + u_p(i,j)**2
     END DO 
     END DO 

     u2_sum = wrf_dm_sum_real8( u2_sum )
     u2_ave(k) = u2_sum / npts_stagx
   END DO 

   csp_u2 = (csp_u2 * stat_count_r + u2_ave)/(stat_count_r + 1.) 

   
   DO k = kts, kte
     v2_sum = 0.

     DO j = jps, min(jpe,jde)
     DO i = ips, min(ipe,ide-1)
        v_p(i,j) = v(i,k,j) - v_ave(k)

        v2_sum = v2_sum + v_p(i,j)**2
     END DO 
     END DO 

     v2_sum = wrf_dm_sum_real8( v2_sum )
     v2_ave(k) = v2_sum / npts_stagy
   END DO 

   csp_v2 = (csp_v2 * stat_count_r + v2_ave)/(stat_count_r + 1.)

   
   DO k = kts, kte

     uw_sum   = 0.
     vw_sum   = 0.

     th2_sum  = 0.
     thv2_sum = 0.
     thl2_sum = 0.
     qv2_sum  = 0.

     wth_sum  = 0.
     wthv_sum = 0.
     wthl_sum = 0.
     wqv_sum  = 0.
     wqc_sum  = 0.
     wqi_sum  = 0.
     wql_sum  = 0.
     wqf_sum  = 0.

     tke_rs_sum = 0.  
     tke_tr_sum = 0.  
     tke_di_sum = 0.  

     a_cl_sum     = 0.  
     w_c_sum      = 0.  
     w2_c_sum     = 0.  
     thl_c_sum    = 0.  
     qv_c_sum     = 0.  
     qc_c_sum     = 0.  
     qi_c_sum     = 0.  
     qnc_c_sum    = 0.  
     ql_c_sum     = 0.  
     qf_c_sum     = 0.  
     thv_c_sum    = 0.           
     awthl_c_sum  = 0.  
     awqv_c_sum   = 0.  
     awqc_c_sum   = 0.  
     awqi_c_sum   = 0.  
     awql_c_sum   = 0.
     awqf_c_sum   = 0.  
     awthv_c_sum  = 0.  

     a_cc_sum     = 0.  
     w_cc_sum     = 0.  
     w2_cc_sum    = 0.  
     thl_cc_sum   = 0.  
     qv_cc_sum    = 0.  
     qc_cc_sum    = 0.  
     qi_cc_sum    = 0.  
     ql_cc_sum    = 0.  
     qf_cc_sum    = 0.  
     thv_cc_sum   = 0.  
     awthl_cc_sum = 0.  
     awqv_cc_sum  = 0.  
     awqc_cc_sum  = 0.  
     awqi_cc_sum  = 0.  
     awql_cc_sum  = 0.  
     awqf_cc_sum  = 0.  
     awthv_cc_sum = 0.

     i_cl_k(k)  = 0. 
     i_cc_k(k)  = 0. 


     DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)
       u_p(i,j)   = uphy(i,k,j) - uphy_ave(k) 
       v_p(i,j)   = vphy(i,k,j) - vphy_ave(k) 
       w_p(i,j)   = wphy(i,k,j) - wphy_ave(k) 
       p_p(i,j)   = p(i,k,j)   - p_ave(k)
       th_p(i,j)  = th(i,k,j)  - th_ave(k)
       thv_p(i,j) = thv(i,k,j) - thv_ave(k)
       thl_p(i,j) = thl(i,k,j) - thl_ave(k)
       qv_p(i,j)  = qv(i,k,j)  - qv_ave(k)
       qc_p(i,j)  = qc(i,k,j)  - qc_ave(k)
       qi_p(i,j)  = qi(i,k,j)  - qi_ave(k)
       ql_p(i,j)  = ql(i,k,j)  - ql_ave(k)
       qf_p(i,j)  = qf(i,k,j)  - qf_ave(k)
       
       uw_sum   = uw_sum   + u_p(i,j) * w_p(i,j)
       vw_sum   = vw_sum   + v_p(i,j) * w_p(i,j)

       th2_sum  = th2_sum  + th_p(i,j)**2
       thv2_sum = thv2_sum + thv_p(i,j)**2
       thl2_sum = thl2_sum + thl_p(i,j)**2
       qv2_sum  = qv2_sum  + qv_p(i,j)**2

       wth_sum  = wth_sum  + w_p(i,j) * th_p(i,j)
       wthv_sum = wthv_sum + w_p(i,j) * thv_p(i,j)
       wthl_sum = wthl_sum + w_p(i,j) * thl_p(i,j)
       wqv_sum  = wqv_sum  + w_p(i,j) * qv_p(i,j)
       wqc_sum  = wqc_sum  + w_p(i,j) * qc_p(i,j)
       wqi_sum  = wqi_sum  + w_p(i,j) * qi_p(i,j)
       wql_sum  = wql_sum  + w_p(i,j) * ql_p(i,j)
       wqf_sum  = wqf_sum  + w_p(i,j) * qf_p(i,j)

       tke_tmp = 0.5 * (u_p(i,j)**2 + v_p(i,j)**2 + w_p(i,j)**2 )
       tke_rs_sum = tke_rs_sum + tke_tmp
     END DO
     END DO

     
     
     IF (m_opt>0 .or. sfs_opt>0) THEN

        DO j = jps, min(jpe,jde-1)
        DO i = ips, min(ipe,ide-1)
           tke_tr_sum = tke_tr_sum + w_p(i,j)*(tke_tmp + p_p(i,j)/rho_ave(k)) &
                & - u_p(i,j) * 0.25*(m13(i,k,j)+m13(i,k+1,j)+m13(i+1,k,j)+m13(i+1,k+1,j)) &
                & - v_p(i,j) * 0.25*(m23(i,k,j)+m23(i,k+1,j)+m23(i,k,j+1)+m23(i,k+1,j+1)) &
                & - w_p(i,j) * 0.5 *(m33(i,k,j)*m33(i,k+1,j))
           
           tke_di_sum = tke_di_sum &
                & +m11(i,k,j)*(uphy(i+1,k,j)-uphy(i,k,j))*rdx +m12(i,k,j)*(uphy(i,k,j+1)-uphy(i,k,j))*rdy &
                & +m13(i,k,j)*(uphy(i,k+1,j)-uphy(i,k,j))/dz8w(i,k,j) & 
                & +m12(i,k,j)*(vphy(i+1,k,j)-vphy(i,k,j))*rdx +m22(i,k,j)*(vphy(i,k,j+1)-vphy(i,k,j))*rdy &
                & +m23(i,k,j)*(vphy(i,k+1,j)-vphy(i,k,j))/dz8w(i,k,j) & 
                & +m13(i,k,j)*(w(i+1,k,j)-w(i,k,j))*rdx +m23(i,k,j)*(w(i,k,j+1)-w(i,k,j))*rdy &
                & +m33(i,k,j)*(w(i,k+1,j)-w(i,k,j))/dz8w(i,k,j) 
        END DO
        END DO
     END IF

     DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)
       
       i_c(i,k,j)  = 0. 
       i_cc(i,k,j) = 0.

       if (qc(i,k,j)+qi(i,k,j) .gt. thresh_cldfrac) then 
         i_c(i,k,j) = 1.0    
         if (z(i,k,j) <= thresh_cldlow_z) i_clow(i,j) = 1.0 
         i_ct(i,j)  = 1.0    
         i_cl_k(k)  = 1.0    
         if (thv_p(i,j) .gt. 0.0) then 
           i_cc(i,k,j) = 1.0 
           i_cc_k(k)   = 1.0 
         endif
       endif

       a_cl_sum    = a_cl_sum    + i_c(i,k,j)
       w_c_sum     = w_c_sum     + i_c(i,k,j) * w_p(i,j)    
       w2_c_sum    = w2_c_sum    + i_c(i,k,j) * w_p(i,j)**2
       thl_c_sum   = thl_c_sum   + i_c(i,k,j) * thl(i,k,j)
       qv_c_sum    = qv_c_sum    + i_c(i,k,j) * qv(i,k,j)
       qc_c_sum    = qc_c_sum    + i_c(i,k,j) * qc(i,k,j)
       qi_c_sum    = qi_c_sum    + i_c(i,k,j) * qi(i,k,j)
       qnc_c_sum   = qnc_c_sum   + i_c(i,k,j) * qnc(i,k,j) * rho(i,k,j)*1.e-6*(1.+0.61*qv(i,k,j))
       ql_c_sum    = ql_c_sum    + i_c(i,k,j) * ql(i,k,j)
       qf_c_sum    = qf_c_sum    + i_c(i,k,j) * qf(i,k,j)
       thv_c_sum   = thv_c_sum   + i_c(i,k,j) * thv(i,k,j)
       awthl_c_sum = awthl_c_sum + i_c(i,k,j) * w_p(i,j) * thl_p(i,j)
       awqv_c_sum  = awqv_c_sum  + i_c(i,k,j) * w_p(i,j) * qv_p(i,j)
       awqc_c_sum  = awqc_c_sum  + i_c(i,k,j) * w_p(i,j) * qc_p(i,j)
       awqi_c_sum  = awqi_c_sum  + i_c(i,k,j) * w_p(i,j) * qi_p(i,j)
       awql_c_sum  = awql_c_sum  + i_c(i,k,j) * w_p(i,j) * ql_p(i,j)
       awqf_c_sum  = awqf_c_sum  + i_c(i,k,j) * w_p(i,j) * qf_p(i,j)
       awthv_c_sum = awthv_c_sum + i_c(i,k,j) * w_p(i,j) * thv_p(i,j)

       a_cc_sum     = a_cc_sum     + i_cc(i,k,j)
       w_cc_sum     = w_cc_sum     + i_cc(i,k,j) * w_p(i,j)
       w2_cc_sum    = w2_cc_sum    + i_cc(i,k,j) * w_p(i,j)**2
       thl_cc_sum   = thl_cc_sum   + i_cc(i,k,j) * thl(i,k,j)
       qv_cc_sum    = qv_cc_sum    + i_cc(i,k,j) * qv(i,k,j)
       qc_cc_sum    = qc_cc_sum    + i_cc(i,k,j) * qc(i,k,j)
       qi_cc_sum    = qi_cc_sum    + i_cc(i,k,j) * qi(i,k,j)
       ql_cc_sum    = ql_cc_sum    + i_cc(i,k,j) * ql(i,k,j)
       qf_cc_sum    = qf_cc_sum    + i_cc(i,k,j) * qf(i,k,j)
       thv_cc_sum   = thv_cc_sum   + i_cc(i,k,j) * thv(i,k,j)
       awthl_cc_sum = awthl_cc_sum + i_cc(i,k,j) * w_p(i,j) * thl_p(i,j)
       awqv_cc_sum  = awqv_cc_sum  + i_cc(i,k,j) * w_p(i,j) * qv_p(i,j)
       awqc_cc_sum  = awqc_cc_sum  + i_cc(i,k,j) * w_p(i,j) * qc_p(i,j)
       awqi_cc_sum  = awqi_cc_sum  + i_cc(i,k,j) * w_p(i,j) * qi_p(i,j)
       awql_cc_sum  = awql_cc_sum  + i_cc(i,k,j) * w_p(i,j) * ql_p(i,j)
       awqf_cc_sum  = awqf_cc_sum  + i_cc(i,k,j) * w_p(i,j) * qf_p(i,j)
       awthv_cc_sum = awthv_cc_sum + i_cc(i,k,j) * w_p(i,j) * thv_p(i,j)

     END DO
     END DO

     i_cl_k(k)  = wrf_dm_sum_real8( i_cl_k(k) )
     i_cc_k(k)  = wrf_dm_sum_real8( i_cc_k(k) )
     if (i_cl_k(k) .gt. 1.5) i_cl_k(k) = 1.0
     if (i_cc_k(k) .gt. 1.5) i_cc_k(k) = 1.0

     uw_sum   = wrf_dm_sum_real8( uw_sum   )
     vw_sum   = wrf_dm_sum_real8( vw_sum   )

     th2_sum   = wrf_dm_sum_real8( th2_sum   )
     thv2_sum  = wrf_dm_sum_real8( thv2_sum  )
     thl2_sum  = wrf_dm_sum_real8( thl2_sum  )
     qv2_sum   = wrf_dm_sum_real8( qv2_sum   )

     wth_sum  = wrf_dm_sum_real8( wth_sum  )
     wthv_sum = wrf_dm_sum_real8( wthv_sum )
     wthl_sum = wrf_dm_sum_real8( wthl_sum )
     wqv_sum  = wrf_dm_sum_real8( wqv_sum  )
     wqc_sum  = wrf_dm_sum_real8( wqc_sum  )
     wqi_sum  = wrf_dm_sum_real8( wqi_sum  )
     wql_sum  = wrf_dm_sum_real8( wql_sum  )
     wqf_sum  = wrf_dm_sum_real8( wqf_sum  )

     tke_rs_sum = wrf_dm_sum_real8( tke_rs_sum )
     tke_tr_sum = wrf_dm_sum_real8( tke_tr_sum )
     tke_di_sum = wrf_dm_sum_real8( tke_di_sum )

     uw_ave(k)   = uw_sum   / npts
     vw_ave(k)   = vw_sum   / npts

     th2_ave(k)   = th2_sum   / npts 
     thv2_ave(k)  = thv2_sum  / npts 
     thl2_ave(k)  = thl2_sum  / npts 
     qv2_ave(k)   = qv2_sum   / npts 

     wth_ave(k)  = wth_sum  / npts
     wthv_ave(k) = wthv_sum / npts
     wthl_ave(k) = wthl_sum / npts
     wqv_ave(k)  = wqv_sum  / npts
     wqc_ave(k)  = wqc_sum  / npts
     wqi_ave(k)  = wqi_sum  / npts
     wql_ave(k)  = wql_sum  / npts
     wqf_ave(k)  = wqf_sum  / npts
 
     tke_rs_ave(k) = tke_rs_sum / npts
     tke_tr_ave(k) = tke_tr_sum / npts
     tke_di_ave(k) = tke_di_sum / npts

     tke_sh_ave(k) = - uw_ave(k) * (u_ave(k+1)-u_ave(k)) / dz8w_ave(k) & 
                   & - vw_ave(k) * (v_ave(k+1)-v_ave(k)) / dz8w_ave(k) 

     tke_bu_ave(k) =  wthv_ave(k) * g / thv_ave(k)

     a_cl_sum     = wrf_dm_sum_real8( a_cl_sum    )
     w_c_sum      = wrf_dm_sum_real8( w_c_sum     )
     w2_c_sum     = wrf_dm_sum_real8( w2_c_sum    )
     thl_c_sum    = wrf_dm_sum_real8( thl_c_sum   )
     qv_c_sum     = wrf_dm_sum_real8( qv_c_sum    )
     qc_c_sum     = wrf_dm_sum_real8( qc_c_sum    )
     qi_c_sum     = wrf_dm_sum_real8( qi_c_sum    )
     qnc_c_sum    = wrf_dm_sum_real8( qnc_c_sum   )
     ql_c_sum     = wrf_dm_sum_real8( ql_c_sum    )
     qf_c_sum     = wrf_dm_sum_real8( qf_c_sum    )
     thv_c_sum    = wrf_dm_sum_real8( thv_c_sum   )
     awthl_c_sum  = wrf_dm_sum_real8( awthl_c_sum )
     awqv_c_sum   = wrf_dm_sum_real8( awqv_c_sum  )
     awqc_c_sum   = wrf_dm_sum_real8( awqc_c_sum  )
     awqi_c_sum   = wrf_dm_sum_real8( awqi_c_sum  )
     awql_c_sum   = wrf_dm_sum_real8( awql_c_sum  )
     awqf_c_sum   = wrf_dm_sum_real8( awqf_c_sum  )
     awthv_c_sum  = wrf_dm_sum_real8( awthv_c_sum )

     a_cc_sum     = wrf_dm_sum_real8( a_cc_sum    )
     w_cc_sum     = wrf_dm_sum_real8( w_cc_sum    )
     w2_cc_sum    = wrf_dm_sum_real8( w2_cc_sum   )
     thl_cc_sum   = wrf_dm_sum_real8( thl_cc_sum  )
     qv_cc_sum    = wrf_dm_sum_real8( qv_cc_sum   )
     qc_cc_sum    = wrf_dm_sum_real8( qc_cc_sum   )
     qi_cc_sum    = wrf_dm_sum_real8( qi_cc_sum   )
     ql_cc_sum    = wrf_dm_sum_real8( ql_cc_sum   )
     qf_cc_sum    = wrf_dm_sum_real8( qf_cc_sum   )
     thv_cc_sum   = wrf_dm_sum_real8( thv_cc_sum  )
     awthl_cc_sum = wrf_dm_sum_real8( awthl_cc_sum)
     awqv_cc_sum  = wrf_dm_sum_real8( awqv_cc_sum )
     awqc_cc_sum  = wrf_dm_sum_real8( awqc_cc_sum )
     awqi_cc_sum  = wrf_dm_sum_real8( awqi_cc_sum )
     awql_cc_sum  = wrf_dm_sum_real8( awql_cc_sum )
     awqf_cc_sum  = wrf_dm_sum_real8( awqf_cc_sum )
     awthv_cc_sum = wrf_dm_sum_real8( awthv_cc_sum)

     a_cl_ave(k)     = a_cl_sum     / npts

     if ( a_cl_sum .gt. 0.5) then 
       w_c_ave(k)      = w_c_sum      / a_cl_sum
       w2_c_ave(k)     = w2_c_sum     / a_cl_sum
       thl_c_ave(k)    = thl_c_sum    / a_cl_sum
       qv_c_ave(k)     = qv_c_sum     / a_cl_sum
       qc_c_ave(k)     = qc_c_sum     / a_cl_sum
       qi_c_ave(k)     = qi_c_sum     / a_cl_sum
       qnc_c_ave(k)    = qnc_c_sum    / a_cl_sum
       ql_c_ave(k)     = ql_c_sum     / a_cl_sum
       qf_c_ave(k)     = qf_c_sum     / a_cl_sum
       thv_c_ave(k)    = thv_c_sum    / a_cl_sum
     else
       w_c_ave(k)      = 0.
       w2_c_ave(k)     = 0.
       thl_c_ave(k)    = 0.
       qv_c_ave(k)     = 0.
       qc_c_ave(k)     = 0.
       qi_c_ave(k)     = 0.
       qnc_c_ave(k)    = 0.
       ql_c_ave(k)     = 0.
       qf_c_ave(k)     = 0.
       thv_c_ave(k)    = 0.
     endif
     
     awthl_c_ave(k)  = awthl_c_sum  / npts
     awqv_c_ave(k)   = awqv_c_sum   / npts
     awqc_c_ave(k)   = awqc_c_sum   / npts
     awqi_c_ave(k)   = awqi_c_sum   / npts
     awql_c_ave(k)   = awql_c_sum   / npts
     awqf_c_ave(k)   = awqf_c_sum   / npts
     awthv_c_ave(k)  = awthv_c_sum  / npts

     a_cc_ave(k)     = a_cc_sum     / npts

     if ( a_cc_sum .gt. 0.5) then
       w_cc_ave(k)     = w_cc_sum     / a_cc_sum
       w2_cc_ave(k)    = w2_cc_sum    / a_cc_sum
       thl_cc_ave(k)   = thl_cc_sum   / a_cc_sum
       qv_cc_ave(k)    = qv_cc_sum    / a_cc_sum
       qc_cc_ave(k)    = qc_cc_sum    / a_cc_sum
       qi_cc_ave(k)    = qi_cc_sum    / a_cc_sum
       ql_cc_ave(k)    = ql_cc_sum    / a_cc_sum
       qf_cc_ave(k)    = qf_cc_sum    / a_cc_sum
       thv_cc_ave(k)   = thv_cc_sum   / a_cc_sum
     else 
       w_cc_ave(k)      = 0.
       w2_cc_ave(k)     = 0.
       thl_cc_ave(k)    = 0.
       qv_cc_ave(k)     = 0.
       qc_cc_ave(k)     = 0.
       qi_cc_ave(k)     = 0.
       ql_cc_ave(k)     = 0.
       qf_cc_ave(k)     = 0.
       thv_cc_ave(k)    = 0.
     endif

     awthl_cc_ave(k) = awthl_cc_sum / npts
     awqv_cc_ave(k)  = awqv_cc_sum  / npts
     awqc_cc_ave(k)  = awqc_cc_sum  / npts
     awqi_cc_ave(k)  = awqi_cc_sum  / npts
     awql_cc_ave(k)  = awql_cc_sum  / npts
     awqf_cc_ave(k)  = awqf_cc_sum  / npts
     awthv_cc_ave(k) = awthv_cc_sum / npts

   END DO 

   
   wskew_ave(:) = 0.  
   DO k = kts, kte+1
     IF (w2_ave(k).ne.0.) wskew_ave(k) =  w3_ave(k) / w2_ave(k)**(3.0/2.0) 
   END DO

   intg_tke = 0.
   tke_tr_ave(kte+1) = 0. 
   DO k = kts, kte
     intg_tke = intg_tke + (tke_sgs_ave(k)+tke_rs_ave(k))*rho_ave(k)*dz8w_ave(k)
                           
     tke_tr_ave(k) = (tke_tr_ave(k+1)-tke_tr_ave(k)) / dz8w_ave(k)
   END DO

   cldlow_sum = 0.
   cldtot_sum = 0.
   DO j = jps, min(jpe,jde-1)
   DO i = ips, min(ipe,ide-1)
     cldlow_sum = cldlow_sum + i_clow(i,j)
     cldtot_sum = cldtot_sum + i_ct(i,j)
   END DO
   END DO
   cldlow_sum = wrf_dm_sum_real8( cldlow_sum )
   cldtot_sum = wrf_dm_sum_real8( cldtot_sum )
   cst_cldlow = (cst_cldlow * stat_count_r + cldlow_sum/npts)/(stat_count_r + 1.)
   cst_cldtot = (cst_cldtot * stat_count_r + cldtot_sum/npts)/(stat_count_r + 1.)

   cst_tke    = (cst_tke    * stat_count_r + intg_tke            )/(stat_count_r + 1.)

   
     
   csp_uw   = (csp_uw   * stat_count_r + uw_ave   + uw_sgs_ave  )/(stat_count_r + 1.) 
   csp_vw   = (csp_vw   * stat_count_r + vw_ave   + vw_sgs_ave  )/(stat_count_r + 1.) 
            
   csp_th2   = (csp_th2   * stat_count_r + th2_ave   )/(stat_count_r + 1.) 
   csp_thv2  = (csp_thv2  * stat_count_r + thv2_ave  )/(stat_count_r + 1.) 
   csp_thl2  = (csp_thl2  * stat_count_r + thl2_ave  )/(stat_count_r + 1.) 
   csp_qv2   = (csp_qv2   * stat_count_r + qv2_ave   )/(stat_count_r + 1.) 

   csp_wth  = (csp_wth  * stat_count_r + wth_ave  + wth_sgs_ave )/(stat_count_r + 1.) 
   csp_wthv = (csp_wthv * stat_count_r + wthv_ave + wthv_sgs_ave)/(stat_count_r + 1.) 
   csp_wthl = (csp_wthl * stat_count_r + wthl_ave + wthl_sgs_ave)/(stat_count_r + 1.) 
   csp_wqv  = (csp_wqv  * stat_count_r + wqv_ave  + wqv_sgs_ave )/(stat_count_r + 1.) 
   csp_wqc  = (csp_wqc  * stat_count_r + wqc_ave  + wqc_sgs_ave )/(stat_count_r + 1.) 
   csp_wqi  = (csp_wqi  * stat_count_r + wqi_ave  + wqi_sgs_ave )/(stat_count_r + 1.) 
   csp_wql  = (csp_wql  * stat_count_r + wql_ave  + wql_sgs_ave )/(stat_count_r + 1.) 
   csp_wqf  = (csp_wqf  * stat_count_r + wqf_ave  + wqf_sgs_ave )/(stat_count_r + 1.) 

   csp_wskew = (csp_wskew   * stat_count_r + wskew_ave  )/(stat_count_r + 1.)

   csp_wqt   = csp_wqv + csp_wql + csp_wqf
   csp_u2v2  = csp_u2 + csp_v2 

   csp_tke_rs = (csp_tke_rs * stat_count_r + tke_rs_ave )/(stat_count_r + 1.)
   csp_tke_sh = (csp_tke_sh * stat_count_r + tke_sh_ave )/(stat_count_r + 1.)
   csp_tke_bu = (csp_tke_bu * stat_count_r + tke_bu_ave )/(stat_count_r + 1.)
   csp_tke_tr = (csp_tke_tr * stat_count_r + tke_tr_ave )/(stat_count_r + 1.)
   csp_tke_di = (csp_tke_di * stat_count_r + tke_di_ave )/(stat_count_r + 1.)

   csp_a_cl   = (csp_a_cl   * stat_count_r + a_cl_ave   )/(stat_count_r + 1.)
   csp_a_cc   = (csp_a_cc   * stat_count_r + a_cc_ave   )/(stat_count_r + 1.)

   
   do k = kts, kte 
     if ( i_cl_k(k) .gt. 0.5) then 
       n_c1 = stat_count_r - real(stat_count_nocl(k))
       n_c2 = n_c1 + 1.0 

       csp_w_c(k)      = (csp_w_c(k)     * n_c1 + w_c_ave(k)   * i_cl_k(k)) / n_c2
       csp_w2_c(k)     = (csp_w2_c(k)    * n_c1 + w2_c_ave(k)  * i_cl_k(k)) / n_c2
       csp_thl_c(k)    = (csp_thl_c(k)   * n_c1 + thl_c_ave(k) * i_cl_k(k)) / n_c2
       csp_qv_c(k)     = (csp_qv_c(k)    * n_c1 + qv_c_ave(k)  * i_cl_k(k)) / n_c2
       csp_qc_c(k)     = (csp_qc_c(k)    * n_c1 + qc_c_ave(k)  * i_cl_k(k)) / n_c2
       csp_qi_c(k)     = (csp_qi_c(k)    * n_c1 + qi_c_ave(k)  * i_cl_k(k)) / n_c2
       csp_qnc_c(k)    = (csp_qnc_c(k)   * n_c1 + qnc_c_ave(k) * i_cl_k(k)) / n_c2
       csp_ql_c(k)     = (csp_ql_c(k)    * n_c1 + ql_c_ave(k)  * i_cl_k(k)) / n_c2
       csp_qf_c(k)     = (csp_qf_c(k)    * n_c1 + qf_c_ave(k)  * i_cl_k(k)) / n_c2
       csp_thv_c(k)    = (csp_thv_c(k)   * n_c1 + thv_c_ave(k) * i_cl_k(k)) / n_c2

       csp_awthl_c(k)  = (csp_awthl_c(k) * n_c1 + awthl_c_ave(k) * i_cl_k(k)) / n_c2
       csp_awqv_c(k)   = (csp_awqv_c(k)  * n_c1 + awqv_c_ave(k)  * i_cl_k(k)) / n_c2
       csp_awqc_c(k)   = (csp_awqc_c(k)  * n_c1 + awqc_c_ave(k)  * i_cl_k(k)) / n_c2
       csp_awqi_c(k)   = (csp_awqi_c(k)  * n_c1 + awqi_c_ave(k)  * i_cl_k(k)) / n_c2
       csp_awql_c(k)   = (csp_awql_c(k)  * n_c1 + awql_c_ave(k)  * i_cl_k(k)) / n_c2
       csp_awqf_c(k)   = (csp_awqf_c(k)  * n_c1 + awqf_c_ave(k)  * i_cl_k(k)) / n_c2
       csp_awthv_c(k)  = (csp_awthv_c(k) * n_c1 + awthv_c_ave(k) * i_cl_k(k)) / n_c2
     endif

     if ( crm_stat_count .ne. stat_count_nocc(k) .or. i_cc_k(k) .gt. 0.5) then 
       n_cc1 = stat_count_r - real(stat_count_nocc(k))
       n_cc2 = n_cc1 + 1.0

       csp_w_cc(k)   = ( csp_w_cc(k)  * n_cc1 + w_cc_ave(k)  *i_cc_k(k))/ n_cc2
       csp_w2_cc(k)  = ( csp_w2_cc(k) * n_cc1 + w2_cc_ave(k) *i_cc_k(k))/ n_cc2
       csp_thl_cc(k) = ( csp_thl_cc(k)* n_cc1 + thl_cc_ave(k)*i_cc_k(k))/ n_cc2
       csp_qv_cc(k)  = ( csp_qv_cc(k) * n_cc1 + qv_cc_ave(k) *i_cc_k(k))/ n_cc2
       csp_qc_cc(k)  = ( csp_qc_cc(k) * n_cc1 + qc_cc_ave(k) *i_cc_k(k))/ n_cc2
       csp_qi_cc(k)  = ( csp_qi_cc(k) * n_cc1 + qi_cc_ave(k) *i_cc_k(k))/ n_cc2
       csp_ql_cc(k)  = ( csp_ql_cc(k) * n_cc1 + ql_cc_ave(k) *i_cc_k(k))/ n_cc2
       csp_qf_cc(k)  = ( csp_qf_cc(k) * n_cc1 + qf_cc_ave(k) *i_cc_k(k))/ n_cc2
       csp_thv_cc(k) = ( csp_thv_cc(k)* n_cc1 + thv_cc_ave(k)*i_cc_k(k))/ n_cc2

       csp_awthl_cc(k) = ( csp_awthl_cc(k)* n_cc1 + awthl_cc_ave(k)*i_cc_k(k))/ n_cc2
       csp_awqv_cc(k)  = ( csp_awqv_cc(k) * n_cc1 + awqv_cc_ave(k) *i_cc_k(k))/ n_cc2
       csp_awqc_cc(k)  = ( csp_awqc_cc(k) * n_cc1 + awqc_cc_ave(k) *i_cc_k(k))/ n_cc2
       csp_awqi_cc(k)  = ( csp_awqi_cc(k) * n_cc1 + awqi_cc_ave(k) *i_cc_k(k))/ n_cc2
       csp_awql_cc(k)  = ( csp_awql_cc(k) * n_cc1 + awql_cc_ave(k) *i_cc_k(k))/ n_cc2
       csp_awqf_cc(k)  = ( csp_awqf_cc(k) * n_cc1 + awqf_cc_ave(k) *i_cc_k(k))/ n_cc2
       csp_awthv_cc(k) = ( csp_awthv_cc(k)* n_cc1 + awthv_cc_ave(k)*i_cc_k(k))/ n_cc2
     endif
   end do

   csp_aw_c    = csp_a_cl * csp_w_c 
   csp_qt_c    = csp_qv_c + csp_ql_c + csp_qf_c
   csp_awqt_c  = csp_awqv_c + csp_awql_c + csp_awqf_c

   csp_aw_cc   = csp_a_cc * csp_w_cc
   csp_qt_cc   = csp_qv_cc + csp_ql_cc + csp_qf_cc
   csp_awqt_cc = csp_awqv_cc + csp_awql_cc + csp_awqf_cc





   DO k = kts, kte
     sigc_thl_sum = 0.
     sigc_qt_sum  = 0.
     sigc_ql_sum  = 0.
     sigc_qf_sum  = 0.
     sigc_qc_sum  = 0.
     sigc_qi_sum  = 0.
     sigc_thv_sum = 0.

     DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)

       sigc_thl_sum = sigc_thl_sum + i_c(i,k,j) * (thl(i,k,j)-thl_c_ave(k))**2
       sigc_thv_sum = sigc_thv_sum + i_c(i,k,j) * (thv(i,k,j)-thv_c_ave(k))**2
       sigc_qt_sum  = sigc_qt_sum  + i_c(i,k,j) * (qv(i,k,j)+ql(i,k,j)+qf(i,k,j)-qv_c_ave(k)-ql_c_ave(k)-qf_c_ave(k))**2
       sigc_qc_sum  = sigc_qc_sum  + i_c(i,k,j) * (qc(i,k,j) -qc_c_ave(k))**2
       sigc_qi_sum  = sigc_qi_sum  + i_c(i,k,j) * (qi(i,k,j) -qi_c_ave(k))**2
       sigc_ql_sum  = sigc_ql_sum  + i_c(i,k,j) * (ql(i,k,j) -ql_c_ave(k))**2
       sigc_qf_sum  = sigc_qf_sum  + i_c(i,k,j) * (qf(i,k,j) -qf_c_ave(k))**2

     END DO
     END DO

     sigc_thl_sum = wrf_dm_sum_real8( sigc_thl_sum )
     sigc_thv_sum = wrf_dm_sum_real8( sigc_thv_sum )
     sigc_qt_sum  = wrf_dm_sum_real8( sigc_qt_sum  )
     sigc_qc_sum  = wrf_dm_sum_real8( sigc_qc_sum  )
     sigc_qi_sum  = wrf_dm_sum_real8( sigc_qi_sum  )
     sigc_ql_sum  = wrf_dm_sum_real8( sigc_ql_sum  )
     sigc_qf_sum  = wrf_dm_sum_real8( sigc_qf_sum  )

     if (a_cl_ave(k) .gt. 0.5/npts) then 
       sigc_thl_ave(k) = sigc_thl_sum / npts / a_cl_ave(k) 
       sigc_thv_ave(k) = sigc_thv_sum / npts / a_cl_ave(k)
       sigc_qt_ave(k)  = sigc_qt_sum  / npts / a_cl_ave(k)
       sigc_qc_ave(k)  = sigc_qc_sum  / npts / a_cl_ave(k)
       sigc_qi_ave(k)  = sigc_qi_sum  / npts / a_cl_ave(k)
       sigc_ql_ave(k)  = sigc_ql_sum  / npts / a_cl_ave(k)
       sigc_qf_ave(k)  = sigc_qf_sum  / npts / a_cl_ave(k)
     else 
       sigc_thl_ave(k) = 0.
       sigc_thv_ave(k) = 0.
       sigc_qt_ave(k)  = 0.
       sigc_qc_ave(k)  = 0.
       sigc_qi_ave(k)  = 0.
       sigc_ql_ave(k)  = 0. 
       sigc_qf_ave(k)  = 0. 
     end if

   END DO

   do k = kts, kte 
     if ( crm_stat_count .ne. stat_count_nocl(k) .or. i_cl_k(k) .gt. 0.5) then 
       n_c1 = stat_count_r - real(stat_count_nocl(k))
       n_c2 = n_c1 + 1.

       csp_sigc_thl(k) = (csp_sigc_thl(k) * n_c1 + sigc_thl_ave(k) * i_cl_k(k)) / n_c2
       csp_sigc_thv(k) = (csp_sigc_thv(k) * n_c1 + sigc_thv_ave(k) * i_cl_k(k)) / n_c2
       csp_sigc_qt(k)  = (csp_sigc_qt(k)  * n_c1 + sigc_qt_ave(k)  * i_cl_k(k)) / n_c2
       csp_sigc_qc(k)  = (csp_sigc_qc(k)  * n_c1 + sigc_qc_ave(k)  * i_cl_k(k)) / n_c2
       csp_sigc_qi(k)  = (csp_sigc_qi(k)  * n_c1 + sigc_qi_ave(k)  * i_cl_k(k)) / n_c2
       csp_sigc_ql(k)  = (csp_sigc_ql(k)  * n_c1 + sigc_ql_ave(k)  * i_cl_k(k)) / n_c2
       csp_sigc_qf(k)  = (csp_sigc_qf(k)  * n_c1 + sigc_qf_ave(k)  * i_cl_k(k)) / n_c2

     endif
   end do




   DO k = kts, kte
     DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)
       if (smaxact(i,k,j) .gt. 1.e-10 .and. smaxact(i,k,j) .gt. csp_smaxactmax(k)) csp_smaxactmax(k) = smaxact(i,k,j)
       if (rminact(i,k,j) .gt. 1.e-10 .and. rminact(i,k,j) .lt. csp_rminactmin(k)) csp_rminactmin(k) = rminact(i,k,j)
     END DO
     END DO

     csp_smaxactmax(k) = wrf_dm_max_real( csp_smaxactmax(k) )
     csp_rminactmin(k) = wrf_dm_min_real( csp_rminactmin(k) )  
   END DO

   DO k = kts, kte+1
     DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)
       if (w(i,k,j) .gt. csp_wmax(k))  csp_wmax(k) = w(i,k,j)
       if (w(i,k,j) .lt. csp_wmin(k))  csp_wmin(k) = w(i,k,j)
     END DO
     END DO

     
     
     
     
     csp_wmax(k)       = wrf_dm_max_real( csp_wmax(k) )
     csp_wmin(k)       = wrf_dm_min_real( csp_wmin(k) )
   END DO




   
   stat_count_nocl(kts:kte) = stat_count_nocl(kts:kte) + (1 - int(i_cl_k(kts:kte)))
   stat_count_nocc(kts:kte) = stat_count_nocc(kts:kte) + (1 - int(i_cc_k(kts:kte)))






   
   DO j = jps, min(jpe,jde-1)
   DO k = kts, kte
   DO i = ips, min(ipe,ide)
      csv_u(i,k,j) = ( csv_u(i,k,j)*stat_count_r + u(i,k,j) )/(stat_count_r + 1.)
   END DO 
   END DO 
   END DO 

   
   DO j = jps, min(jpe,jde)
   DO k = kts, kte
   DO i = ips, min(ipe,ide-1)
      csv_v(i,k,j) = ( csv_v(i,k,j)*stat_count_r + v(i,k,j) )/(stat_count_r + 1.)
   END DO 
   END DO 
   END DO 

   
   DO j = jps, min(jpe,jde-1)
   DO k = kts, kte+1
   DO i = ips, min(ipe,ide-1)
      csv_w(i,k,j) = ( csv_w(i,k,j)*stat_count_r + w(i,k,j) )/(stat_count_r + 1.)
      csv_w2(i,k,j) = ( csv_w2(i,k,j)*stat_count_r + (w(i,k,j)-w_ave(k))**2 )/(stat_count_r + 1.) 
   END DO 
   END DO 
   END DO 

   DO j = jps, min(jpe,jde-1)
   DO k = kts, kte
   DO i = ips, min(ipe,ide-1)
      csv_th(i,k,j) = ( csv_th(i,k,j)*stat_count_r + th(i,k,j) )/(stat_count_r + 1.)
      csv_qv(i,k,j) = ( csv_qv(i,k,j)*stat_count_r + qv(i,k,j) )/(stat_count_r + 1.)
      csv_qc(i,k,j) = ( csv_qc(i,k,j)*stat_count_r + qc(i,k,j) )/(stat_count_r + 1.)
      csv_qr(i,k,j) = ( csv_qr(i,k,j)*stat_count_r + qr(i,k,j) )/(stat_count_r + 1.)
      csv_qi(i,k,j) = ( csv_qi(i,k,j)*stat_count_r + qi(i,k,j) )/(stat_count_r + 1.)
      csv_qs(i,k,j) = ( csv_qs(i,k,j)*stat_count_r + qs(i,k,j) )/(stat_count_r + 1.)
      csv_qg(i,k,j) = ( csv_qg(i,k,j)*stat_count_r + qg(i,k,j) )/(stat_count_r + 1.)
      csv_lwc(i,k,j) = ( csv_lwc(i,k,j)*stat_count_r + lwc(i,k,j) )/(stat_count_r + 1.) 
      csv_iwc(i,k,j) = ( csv_iwc(i,k,j)*stat_count_r + iwc(i,k,j) )/(stat_count_r + 1.) 

      
      
      cldmask = 0.
      if( qc(i,k,j)+qi(i,k,j) > thresh_cldfrac ) cldmask = 1.
      csv_cldfrac(i,k,j) = ( csv_cldfrac(i,k,j)*stat_count_r + cldmask )/(stat_count_r + 1.)
   END DO 
   END DO 
   END DO 





   DO j = jps, min(jpe,jde-1)
   DO i = ips, min(ipe,ide-1)
      lwp = sum( lwc(i,kts:kte,j)*dz8w(i,kts:kte,j) ) 
      css_lwp(i,j) = ( css_lwp(i,j)*stat_count_r + lwp )/(stat_count_r + 1.)

      iwp = sum( iwc(i,kts:kte,j)*dz8w(i,kts:kte,j) ) 
      css_iwp(i,j) = ( css_iwp(i,j)*stat_count_r + iwp )/(stat_count_r + 1.)
   END DO 
   END DO 

   DO j = jps, min(jpe,jde-1)
   DO i = ips, min(ipe,ide-1)
      
      
      cldmask = 0.
      cldmask_low = 0. 
      DO k = kts, kte
        if( qc(i,k,j)+qi(i,k,j) > thresh_cldfrac ) cldmask = 1.
        if( qc(i,k,j)+qi(i,k,j) > thresh_cldfrac .and. z(i,k,j) <= thresh_cldlow_z) cldmask_low = 1.
      END DO 
      css_cldtot(i,j) = ( css_cldtot(i,j)*stat_count_r + cldmask )/(stat_count_r + 1.)
      css_cldlow(i,j) = ( css_cldlow(i,j)*stat_count_r + cldmask_low )/(stat_count_r + 1.)
   END DO 
   END DO 
      
END SUBROUTINE crm_stat



SUBROUTINE crm_stat_reset(                                           &

     cst_cldlow, cst_cldtot, cst_lwp, cst_iwp, cst_precw, cst_tke,   &
     cst_tsair, cst_ps, cst_prect, cst_sh, cst_lh, cst_fsntc,        &
     cst_fsnt, cst_flntc, cst_flnt, cst_fsnsc, cst_fsns, cst_flnsc,  &
     cst_flns, cst_swinc, cst_tsk, cst_ust,                          &

     csp_z, csp_z8w, csp_dz8w, csp_u, csp_v, csp_w,                  &
     csp_p, csp_th, csp_thv, csp_thl,                                &
     csp_qv, csp_qc, csp_qi, csp_ql, csp_qf, csp_qt, csp_lwc,        &
     csp_iwc, csp_speqv, csp_a_cl, csp_rho, csp_u2, csp_v2,          &
     csp_u2v2, csp_w2, csp_w3, csp_wskew, csp_th2, csp_thv2,         &
     csp_thl2, csp_qv2, csp_uw, csp_vw, csp_wth, csp_wthv, csp_wthl, &
     csp_wqv, csp_wqc, csp_wqi, csp_wql, csp_wqf, csp_wqt,           &
     csp_sedfqc, csp_sedfqr,                                         &
     csp_uw_sgs, csp_vw_sgs, csp_wth_sgs, csp_wthv_sgs, csp_wthl_sgs,&
     csp_wqv_sgs, csp_wqc_sgs, csp_wqi_sgs, csp_wql_sgs, csp_wqf_sgs,&
     csp_wqt_sgs, csp_thdt_cond, csp_thdt_lw, csp_thdt_sw,           &
     csp_thdt_ls, csp_qvdt_pr, csp_qvdt_cond, csp_qvdt_ls,           &
     csp_qcdt_pr, csp_qcdt_sed, csp_qrdt_sed,                        &
     csp_thdt_lshor, csp_qvdt_lshor, csp_thdt_lsver, csp_qvdt_lsver, &
     csp_thdt_lsrlx, csp_qvdt_lsrlx, csp_udt_ls,     csp_vdt_ls,     &
     csp_udt_lsver,  csp_vdt_lsver, csp_udt_lsrlx,  csp_vdt_lsrlx,   &
     csp_tke_rs, csp_tke_sh, csp_tke_bu,                             &
     csp_tke_tr, csp_tke_di, csp_tke_sgs,                            &
     csp_swupf, csp_swdnf, csp_lwupf, csp_lwdnf,                     &
     csp_w_c, csp_w2_c, csp_thl_c, csp_qt_c, csp_qv_c, csp_ql_c,     &
     csp_qf_c, csp_qc_c, csp_qi_c, csp_qnc_c,                        &
     csp_thv_c, csp_aw_c, csp_awthl_c, csp_awqt_c, csp_awqv_c,       &
     csp_awql_c, csp_awqf_c, csp_awqc_c, csp_awqi_c, csp_awthv_c,    &
     csp_a_cc, csp_w_cc, csp_w2_cc, csp_thl_cc, csp_qt_cc, csp_qv_cc,&
     csp_ql_cc, csp_qf_cc, csp_qc_cc, csp_qi_cc, csp_thv_cc,         &
     csp_aw_cc, csp_awthl_cc, csp_awqt_cc, csp_awqv_cc, csp_awql_cc, &
     csp_awqf_cc, csp_awqc_cc, csp_awqi_cc, csp_awthv_cc,            &
     csp_sigc_thl, csp_sigc_qt, csp_sigc_ql,csp_sigc_qf, csp_sigc_qc,&
     csp_sigc_qi, csp_sigc_thv, csp_smaxactmax, csp_rminactmin,      &
     csp_wmax, csp_wmin,                                             &
     stat_count_nocl, stat_count_nocc,                               &

     csv_th, csv_u, csv_v, csv_w, csv_w2,                            &
     csv_qv, csv_qc, csv_qr, csv_qi, csv_qs, csv_qg,                 &
     csv_lwc, csv_iwc, csv_cldfrac,                                  &

     css_lwp, css_iwp, css_cldtot, css_cldlow,                       &

     ids,ide, jds,jde, kds,kde,                                      &
     ims,ime, jms,jme, kms,kme,                                      &
     its, ite, jts, jte, kts, kte                                    )






  
  
  INTEGER, INTENT(IN   )    :: ids,ide, jds,jde, kds,kde, &
                               ims,ime, jms,jme, kms,kme, &
                               its, ite, jts, jte, kts, kte

  REAL, INTENT(INOUT) ::  &
       cst_cldlow, cst_cldtot, cst_lwp, cst_iwp, cst_precw, cst_tke,&
       cst_tsair, cst_ps, cst_prect, cst_sh, cst_lh, cst_fsntc,     &
       cst_fsnt, cst_flntc, cst_flnt, cst_fsnsc, cst_fsns,          &
       cst_flnsc, cst_flns, cst_swinc, cst_tsk, cst_ust

  REAL, DIMENSION( kms:kme ), INTENT(INOUT) ::                          &
       csp_z, csp_z8w, csp_dz8w, csp_u, csp_v, csp_w,                   &
       csp_p, csp_th, csp_thv, csp_thl,                                 &
       csp_qv, csp_qc, csp_qi, csp_ql, csp_qf, csp_qt, csp_lwc, csp_iwc,&
       csp_speqv, csp_a_cl, csp_rho, csp_u2, csp_v2, csp_u2v2, csp_w2,  &
       csp_w3, csp_wskew, csp_th2, csp_thv2, csp_thl2, csp_qv2,         &
       csp_uw, csp_vw, csp_wth, csp_wthv, csp_wthl, csp_wqv,            &
       csp_wqc, csp_wqi, csp_wql, csp_wqf, csp_wqt,                     &
       csp_sedfqc, csp_sedfqr,                                          &
       csp_uw_sgs, csp_vw_sgs, csp_wth_sgs, csp_wthv_sgs, csp_wthl_sgs, &
       csp_wqv_sgs, csp_wqc_sgs, csp_wqi_sgs, csp_wql_sgs, csp_wqf_sgs, &
       csp_wqt_sgs, csp_thdt_cond, csp_thdt_lw,                         &
       csp_thdt_sw, csp_thdt_ls, csp_qvdt_pr, csp_qvdt_cond,            &
       csp_qvdt_ls, csp_qcdt_pr, csp_qcdt_sed, csp_qrdt_sed,            &
       csp_thdt_lshor, csp_qvdt_lshor, csp_thdt_lsver, csp_qvdt_lsver,  &
       csp_thdt_lsrlx, csp_qvdt_lsrlx, csp_udt_ls,     csp_vdt_ls,      &
       csp_udt_lsver,  csp_vdt_lsver, csp_udt_lsrlx,  csp_vdt_lsrlx,    &
       csp_tke_rs, csp_tke_sh, csp_tke_bu, csp_tke_tr, csp_tke_di,      &
       csp_tke_sgs,                                                     &
       csp_swupf, csp_swdnf, csp_lwupf, csp_lwdnf,                      &
       csp_w_c, csp_w2_c, csp_thl_c, csp_qt_c, csp_qv_c, csp_ql_c,      &
       csp_qf_c, csp_qc_c, csp_qi_c, csp_qnc_c,                         &
       csp_thv_c, csp_aw_c, csp_awthl_c, csp_awqt_c, csp_awqv_c,        &
       csp_awql_c, csp_awqf_c, csp_awqc_c, csp_awqi_c, csp_awthv_c,     &
       csp_a_cc, csp_w_cc, csp_w2_cc, csp_thl_cc, csp_qt_cc, csp_qv_cc, &
       csp_ql_cc,                                                       &
       csp_qf_cc, csp_qc_cc, csp_qi_cc, csp_thv_cc, csp_aw_cc,          &
       csp_awthl_cc, csp_awqt_cc, csp_awqv_cc, csp_awql_cc, csp_awqf_cc,&
       csp_awqc_cc, csp_awqi_cc, csp_awthv_cc, csp_sigc_thl,            &
       csp_sigc_qt, csp_sigc_ql, csp_sigc_qf, csp_sigc_qc, csp_sigc_qi, &
       csp_sigc_thv,                                                    &
       csp_smaxactmax, csp_rminactmin, csp_wmax, csp_wmin

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::        &
       csv_th, csv_u, csv_v, csv_w, csv_w2,                             &
       csv_qv, csv_qc, csv_qr, csv_qi, csv_qs, csv_qg,                  &
       csv_lwc, csv_iwc, csv_cldfrac

  REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::                 &
       css_lwp, css_iwp, css_cldtot, css_cldlow

  INTEGER, DIMENSION( kms:kme ), INTENT(INOUT) ::                       &
       stat_count_nocl, stat_count_nocc

  
  
  

  
  cst_cldlow = 0.
  cst_cldtot = 0.
  cst_lwp = 0.
  cst_iwp = 0.
  cst_precw = 0.
  cst_tke = 0.
  cst_tsair = 0.
  cst_ps = 0.
  cst_prect = 0.
  cst_sh = 0.
  cst_lh = 0.
  cst_fsntc = 0.
  cst_fsnt = 0.
  cst_flntc = 0.
  cst_flnt = 0.
  cst_fsnsc = 0.
  cst_fsns = 0.
  cst_flnsc = 0.
  cst_flns = 0.
  cst_swinc = 0.
  cst_tsk = 0.
  cst_ust = 0.

  
  csp_z = 0.
  csp_z8w = 0.
  csp_dz8w = 0.
  csp_u = 0.
  csp_v = 0.
  csp_w = 0.
  csp_p = 0.
  csp_th = 0.
  csp_thv = 0.
  csp_thl = 0.
  csp_qv = 0.
  csp_qc = 0.
  csp_qi = 0.
  csp_ql = 0.
  csp_qf = 0.
  csp_qt = 0.
  csp_lwc = 0.
  csp_iwc = 0.
  csp_speqv = 0.
  csp_a_cl = 0.
  csp_rho = 0.
  csp_u2 = 0.
  csp_v2 = 0.
  csp_u2v2 = 0.
  csp_w2 = 0.
  csp_w3 = 0.
  csp_wskew = 0.
  csp_th2 = 0.
  csp_thv2 = 0.
  csp_thl2 = 0.
  csp_qv2 = 0.
  csp_uw = 0.
  csp_vw = 0.
  csp_wth = 0.
  csp_wthv = 0.
  csp_wthl = 0.
  csp_wqv = 0.
  csp_wqc = 0.
  csp_wqi = 0.
  csp_wql = 0.
  csp_wqf = 0.
  csp_wqt = 0.
  csp_sedfqc = 0.
  csp_sedfqr = 0.
  csp_uw_sgs = 0.
  csp_vw_sgs = 0.
  csp_wth_sgs = 0.
  csp_wthv_sgs = 0.
  csp_wthl_sgs = 0.
  csp_wqv_sgs = 0.
  csp_wqc_sgs = 0.
  csp_wqi_sgs = 0.
  csp_wql_sgs = 0.
  csp_wqf_sgs = 0.
  csp_wqt_sgs = 0.
  csp_thdt_cond = 0.
  csp_thdt_lw = 0.
  csp_thdt_sw = 0.
  csp_thdt_ls = 0.
  csp_qvdt_pr = 0.
  csp_qvdt_cond = 0.
  csp_qvdt_ls = 0.
  csp_qcdt_pr = 0.
  csp_qcdt_sed = 0.
  csp_qrdt_sed = 0.
  csp_thdt_lshor = 0.
  csp_qvdt_lshor = 0.
  csp_thdt_lsver = 0.
  csp_qvdt_lsver = 0.
  csp_thdt_lsrlx = 0.
  csp_qvdt_lsrlx = 0.
  csp_udt_ls = 0.
  csp_vdt_ls = 0.
  csp_udt_lsver = 0.
  csp_vdt_lsver = 0.
  csp_udt_lsrlx = 0.
  csp_vdt_lsrlx = 0.
  csp_tke_rs = 0.
  csp_tke_sh = 0.
  csp_tke_bu = 0.
  csp_tke_tr = 0.
  csp_tke_di = 0.
  csp_tke_sgs = 0.
  csp_swupf = 0.
  csp_swdnf = 0.
  csp_lwupf = 0.
  csp_lwdnf = 0.
  csp_w_c = 0.
  csp_w2_c = 0.
  csp_thl_c = 0.
  csp_qt_c = 0.
  csp_qv_c = 0.
  csp_ql_c = 0.
  csp_qf_c = 0.
  csp_qc_c = 0.
  csp_qi_c = 0.
  csp_qnc_c = 0.
  csp_thv_c = 0.
  csp_aw_c = 0.
  csp_awthl_c = 0.
  csp_awqt_c = 0.
  csp_awqv_c = 0.
  csp_awql_c = 0.
  csp_awqf_c = 0.
  csp_awqc_c = 0.
  csp_awqi_c = 0.
  csp_awthv_c = 0.
  csp_a_cc = 0.
  csp_w_cc = 0.
  csp_w2_cc = 0.
  csp_thl_cc = 0.
  csp_qt_cc = 0.
  csp_qv_cc = 0.
  csp_ql_cc = 0.
  csp_qf_cc = 0.
  csp_qc_cc = 0.
  csp_qi_cc = 0.
  csp_thv_cc = 0.
  csp_aw_cc = 0.
  csp_awthl_cc = 0.
  csp_awqt_cc = 0.
  csp_awqv_cc = 0.
  csp_awql_cc = 0.
  csp_awqf_cc = 0.
  csp_awqc_cc = 0.
  csp_awqi_cc = 0.
  csp_awthv_cc = 0.
  csp_sigc_thl = 0.
  csp_sigc_qt = 0.
  csp_sigc_ql = 0.
  csp_sigc_qf = 0.
  csp_sigc_qc = 0.
  csp_sigc_qi = 0.
  csp_sigc_thv = 0.
  csp_smaxactmax = 0.
  csp_rminactmin = 0.999e-3  
  csp_wmax = 0.  
  csp_wmin = 0.  
                 

  stat_count_nocl = 0
  stat_count_nocc = 0

  
  csv_th = 0.
  csv_u = 0.
  csv_v = 0.
  csv_w = 0.
  csv_w2 = 0.
  csv_qv = 0.
  csv_qc = 0.
  csv_qr = 0.
  csv_qi = 0.
  csv_qs = 0.
  csv_qg = 0.
  csv_lwc = 0.
  csv_iwc = 0.
  csv_cldfrac = 0.

  
  css_lwp = 0.
  css_iwp = 0.
  css_cldtot = 0.
  css_cldlow = 0.   

END SUBROUTINE crm_stat_reset

END MODULE module_crm_stat 

