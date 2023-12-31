!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_n.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'neexy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'nppxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'numc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'nump')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'nrasubgrid')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ntraprd_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ntsnprd_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'nfuel_cat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'noninduc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%elec_physics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%elec_physics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'noninduc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%elec_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%elec_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'noninduc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%elec_physics.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%elec_physics.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'na_aer1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_morr_act_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_morr_act_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'na_aer1_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_morr_act_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_morr_act_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'na_aer1_grd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_morr_act_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_morr_act_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'na_aer2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_morr_act_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_morr_act_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'na_aer2_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_morr_act_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_morr_act_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'na_aer2_grd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_morr_act_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_morr_act_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'na_aer3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_morr_act_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_morr_act_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'na_aer3_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_morr_act_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_morr_act_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'na_aer3_grd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_morr_act_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_morr_act_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'na_aer4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_morr_act_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_morr_act_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'na_aer4_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_morr_act_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_morr_act_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'na_aer4_grd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%crm_morr_act_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%crm_morr_act_opt.EQ.2
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
