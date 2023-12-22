!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_c.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'cldfra_old_mp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfra_mp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfra_mp_all')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfra_conv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfrai')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfral')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmcr_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmgr_sfcdif')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chgr_sfcdif')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cm_ac_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'canliqxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'canicexy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chleafxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chucxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chv2xy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chb2xy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chstarxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'canfra')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cto1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cto2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cto3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cto4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'canwat_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cov')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cov')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cf3d_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cfm_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfra_bl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%icloud_bl.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%icloud_bl.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_qvten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_tten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_qvtens')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_ttens')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_qcten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_qvten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_tten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_qvtens')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_ttens')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_qcten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmfmc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmfmcdzm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cape')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfratend_cup')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfra_cup')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfra_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cam_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cam_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmfmc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmfmc2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cbmf_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cin_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cinlcl_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cbmflimit_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cufrc_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cquad_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmfsl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmflq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfrash')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cush')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cl_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cf_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'car_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cg_flashcount')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cg_flashrate')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cg_flashcount')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cg_flashrate')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cg_flashcount')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cg_flashrate')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'canhfx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'canqfx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cfu1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cfd1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csol3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'crm_stat_count')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_cldlow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_cldtot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_lwp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_iwp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_precw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_tke')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_tsair')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_ps')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_prect')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_sh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_lh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_fsntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_fsnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_flntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_flnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_fsnsc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_fsns')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_flnsc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_flns')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_swinc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cst_ust')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_z')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_z8w')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_dz8w')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_w')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_p')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_th')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_ql')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_lwc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_iwc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_speqv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_a_cl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_rho')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_u2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_v2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_u2v2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_w2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_w3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wskew')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_uw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_vw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wth')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wthv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wthl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wqv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wqc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wqi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wql')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wqf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wqt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_uw_sgs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_vw_sgs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wth_sgs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wthv_sgs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wthl_sgs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wqv_sgs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wqc_sgs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wqi_sgs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wql_sgs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wqf_sgs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wqt_sgs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_sedfqc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_sedfqr')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thdt_cond')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thdt_lw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thdt_sw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thdt_ls')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qvdt_pr')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qvdt_cond')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qvdt_ls')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qcdt_pr')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qcdt_sed')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qrdt_sed')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thdt_lshor')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qvdt_lshor')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thdt_lsver')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qvdt_lsver')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thdt_lsrlx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qvdt_lsrlx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_udt_ls')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_vdt_ls')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_udt_lsver')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_vdt_lsver')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_udt_lsrlx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_vdt_lsrlx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_swupf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_swdnf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_lwupf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_lwdnf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_tke_rs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_tke_sh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_tke_bu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_tke_tr')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_tke_di')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_tke_sgs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_w_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_w2_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thl_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qt_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qv_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_ql_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qf_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qc_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qi_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qnc_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thv_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_aw_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awthl_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awqt_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awqv_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awql_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awqf_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awqc_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awqi_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awthv_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_a_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_w_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_w2_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thl_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qt_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qv_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_ql_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qf_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qc_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qi_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thv_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_aw_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awthl_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awqt_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awqv_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awql_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awqf_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awqc_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awqi_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_awthv_cc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_sigc_thl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_sigc_qt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_sigc_ql')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_sigc_qf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_sigc_qc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_sigc_qi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_sigc_thv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_th2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thv2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_thl2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_qv2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_smaxactmax')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_rminactmin')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wmax')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csp_wmin')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_th')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_w')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_w2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_qv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_qc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_qr')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_qi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_qs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_qg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_lwc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_iwc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csv_cldfrac')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'css_lwp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'css_iwp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'css_cldtot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'css_cldlow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_stat_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_stat_opt.EQ.1
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
