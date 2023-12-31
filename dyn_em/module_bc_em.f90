























MODULE module_bc_em

   USE module_bc
   USE module_configure
   USE module_wrf_error
   USE module_model_constants

CONTAINS



   SUBROUTINE spec_bdyupdate_ph( ph_save, field,      &
                               field_tend, mu_tend, muts, dt,     &
                               variable_in, config_flags, & 
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )





      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone
      CHARACTER,    INTENT(IN   )    :: variable_in
      REAL,         INTENT(IN   )    :: dt


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: field_tend, ph_save
      REAL,  DIMENSION( ims:ime , jms:jme ), INTENT(IN   ) :: mu_tend, muts
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist, b_limit



      REAL,  DIMENSION( its:ite , jts:jte ) :: mu_old
      LOGICAL    :: periodic_x

      periodic_x = config_flags%periodic_x

      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'
      IF (variable == 'M') variable = 'm'
      IF (variable == 'H') variable = 'h'

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      IF (variable == 'u') ibe = ide
      IF (variable == 'u') itf = min(ite,ide)
      IF (variable == 'v') jbe = jde
      IF (variable == 'v') jtf = min(jte,jde)
      IF (variable == 'm') ktf = kte
      IF (variable == 'h') ktf = kte

      IF (jts - jbs .lt. spec_zone) THEN

        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)

              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              field(i,k,j) = field(i,k,j)*mu_old(i,j)/muts(i,j) + &
                   dt*field_tend(i,k,j)/muts(i,j) +               &
                   ph_save(i,k,j)*(mu_old(i,j)/muts(i,j) - 1.)

            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 

        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)

              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              field(i,k,j) = field(i,k,j)*mu_old(i,j)/muts(i,j) + &
                   dt*field_tend(i,k,j)/muts(i,j) +               &
                   ph_save(i,k,j)*(mu_old(i,j)/muts(i,j) - 1.)

            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN

        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)

              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              field(i,k,j) = field(i,k,j)*mu_old(i,j)/muts(i,j) + &
                   dt*field_tend(i,k,j)/muts(i,j) +               &
                   ph_save(i,k,j)*(mu_old(i,j)/muts(i,j) - 1.)

            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN

        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)

              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              field(i,k,j) = field(i,k,j)*mu_old(i,j)/muts(i,j) + &
                   dt*field_tend(i,k,j)/muts(i,j) +               &
                   ph_save(i,k,j)*(mu_old(i,j)/muts(i,j) - 1.)

            ENDDO
          ENDDO
        ENDDO
      ENDIF 
    ENDIF

   END SUBROUTINE spec_bdyupdate_ph



   SUBROUTINE relax_bdy_dry ( config_flags,                                    &
                              ru_tendf, rv_tendf, ph_tendf, t_tendf,           &
                              rw_tendf, mu_tend,                               &
                              ru, rv, ph, t,                                   &
                              w, mu, mut,                                      &
                              u_bxs,u_bxe,u_bys,u_bye,                         &
                              v_bxs,v_bxe,v_bys,v_bye,                         &
                              ph_bxs,ph_bxe,ph_bys,ph_bye,                     &
                              t_bxs,t_bxe,t_bys,t_bye,                         &
                              w_bxs,w_bxe,w_bys,w_bye,                         &
                              mu_bxs,mu_bxe,mu_bys,mu_bye,                     &
                              u_btxs,u_btxe,u_btys,u_btye,                     &
                              v_btxs,v_btxe,v_btys,v_btye,                     &
                              ph_btxs,ph_btxe,ph_btys,ph_btye,                 &
                              t_btxs,t_btxe,t_btys,t_btye,                     &
                              w_btxs,w_btxe,w_btys,w_btye,                     &
                              mu_btxs,mu_btxe,mu_btys,mu_btye,                 &
                              spec_bdy_width, spec_zone, relax_zone,           &
                              dtbc, fcx, gcx,             &
                              ids,ide, jds,jde, kds,kde,  & 
                              ims,ime, jms,jme, kms,kme,  & 
                              ips,ipe, jps,jpe, kps,kpe,  & 
                              its, ite, jts, jte, kts, kte)
   IMPLICIT NONE

   
   TYPE( grid_config_rec_type ) config_flags

   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, & 
                                            its, ite, jts, jte, kts, kte
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone, relax_zone

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(IN   ) :: ru,     &
                                                                      rv,     &
                                                                      ph,     &
                                                                      w,      &
                                                                      t
   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(IN   )          :: mu  , &
                                                                      mut
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(INOUT) :: ru_tendf, &
                                                                      rv_tendf, &
                                                                      ph_tendf, &
                                                                      rw_tendf, &
                                                                      t_tendf
   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(INOUT)          :: mu_tend
   REAL , DIMENSION( spec_bdy_width) , INTENT(IN   ) :: fcx, gcx

   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width    ), INTENT(IN   ) :: u_bxs,u_bxe, &
                                                                               v_bxs,v_bxe, &
                                                                               ph_bxs,ph_bxe, &
                                                                               w_bxs,w_bxe, &
                                                                               t_bxs,t_bxe, &
                                                                               u_btxs,u_btxe, &
                                                                               v_btxs,v_btxe, &
                                                                               ph_btxs,ph_btxe, &
                                                                               w_btxs,w_btxe, &
                                                                               t_btxs,t_btxe

   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width    ), INTENT(IN   ) :: u_bys,u_bye, &
                                                                               v_bys,v_bye, &
                                                                               ph_bys,ph_bye, &
                                                                               w_bys,w_bye, &
                                                                               t_bys,t_bye, &
                                                                               u_btys,u_btye, &
                                                                               v_btys,v_btye, &
                                                                               ph_btys,ph_btye, &
                                                                               w_btys,w_btye, &
                                                                               t_btys,t_btye


   REAL,  DIMENSION( jms:jme , 1:1     , spec_bdy_width    ), INTENT(IN   ) :: mu_bxs,mu_bxe, &
                                                                               mu_btxs,mu_btxe

   REAL,  DIMENSION( ims:ime , 1:1     , spec_bdy_width    ), INTENT(IN   ) :: mu_bys,mu_bye, &
                                                                               mu_btys,mu_btye
   REAL, INTENT(IN   ) :: dtbc


   REAL , DIMENSION( its-1:ite+1 , kts:kte, jts-1:jte+1  ) :: rfield
   INTEGER :: i_start, i_end, j_start, j_end, i, j, k

           CALL relax_bdytend ( ru, ru_tendf,             &
                               u_bxs,u_bxe,u_bys,u_bye,u_btxs,u_btxe,u_btys,u_btye, &
                               'u'        , config_flags, &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )
           CALL relax_bdytend ( rv, rv_tendf,             &
                               v_bxs,v_bxe,v_bys,v_bye,v_btxs,v_btxe,v_btys,v_btye, &
                               'v'        , config_flags, &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )




           i_start = max(its-1, ids)
           i_end = min(ite+1, ide-1)
           j_start = max(jts-1, jds)
           j_end = min(jte+1, jde-1)

           DO j=j_start,j_end
           DO k=kts,kte
           DO i=i_start,i_end
              rfield(i,k,j) = ph(i,k,j)*mut(i,j)
           ENDDO
           ENDDO
           ENDDO
           CALL relax_bdytend_tile ( rfield, ph_tendf,             &
                               ph_bxs,ph_bxe,ph_bys,ph_bye, ph_btxs,ph_btxe,ph_btys,ph_btye,       &
                               'h'        , config_flags, &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte,  &
                               its-1, ite+1, jts-1,jte+1,kts,kte )  
           DO j=j_start,j_end
           DO k=kts,kte-1
           DO i=i_start,i_end
              rfield(i,k,j) = t(i,k,j)*mut(i,j)
           ENDDO
           ENDDO
           ENDDO
           CALL relax_bdytend_tile ( rfield, t_tendf,              &
                               t_bxs,t_bxe,t_bys,t_bye, t_btxs,t_btxe,t_btys,t_btye,       &
                               't'        , config_flags, &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte,  &
                               its-1, ite+1, jts-1,jte+1,kts,kte )  
           CALL relax_bdytend ( mu, mu_tend,               &
                               mu_bxs,mu_bxe,mu_bys,mu_bye, mu_btxs,mu_btxe,mu_btys,mu_btye,                &
                               'm'        , config_flags,  &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ids,ide, jds,jde, 1  ,1  ,  & 
                               ims,ime, jms,jme, 1  ,1  ,  & 
                               ips,ipe, jps,jpe, 1  ,1  ,  & 
                               its,ite, jts,jte, 1  ,1   )

         IF( config_flags%nested) THEN

           i_start = max(its-1, ids)
           i_end = min(ite+1, ide-1)
           j_start = max(jts-1, jds)
           j_end = min(jte+1, jde-1)

           DO j=j_start,j_end
           DO k=kts,kte
           DO i=i_start,i_end
              rfield(i,k,j) = w(i,k,j)*mut(i,j)
           ENDDO
           ENDDO
           ENDDO
           
           CALL relax_bdytend_tile ( rfield, rw_tendf,             &
                               w_bxs,w_bxe,w_bys,w_bye, w_btxs,w_btxe,w_btys,w_btye,       &
                               'h'        , config_flags, &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte,  &
                               its-1, ite+1, jts-1,jte+1,kts,kte )  

        END IF

   END SUBROUTINE relax_bdy_dry 

   SUBROUTINE relax_bdy_scalar ( scalar_tend,                &
                                 scalar, mu,                 &
                                 scalar_bxs,scalar_bxe,scalar_bys,scalar_bye, &
                                 scalar_btxs,scalar_btxe,scalar_btys,scalar_btye, &
                                 spec_bdy_width, spec_zone, relax_zone,       &
                                 dtbc, fcx, gcx,             &
                                 config_flags,               &
                                 ids,ide, jds,jde, kds,kde,  & 
                                 ims,ime, jms,jme, kms,kme,  & 
                                 ips,ipe, jps,jpe, kps,kpe,  & 
                                 its, ite, jts, jte, kts, kte)
   IMPLICIT NONE

   
   TYPE( grid_config_rec_type ) config_flags

   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, & 
                                            its, ite, jts, jte, kts, kte
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone, relax_zone

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(IN   ) :: scalar
   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(IN   ) :: mu
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(INOUT) :: scalar_tend
   REAL , DIMENSION( spec_bdy_width) , INTENT(IN   ) :: fcx, gcx

   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width    ), INTENT(IN   ) :: scalar_bxs,scalar_bxe, &
                                                                               scalar_btxs,scalar_btxe
   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width    ), INTENT(IN   ) :: scalar_bys,scalar_bye, &
                                                                               scalar_btys,scalar_btye
   REAL, INTENT(IN   ) :: dtbc

   INTEGER :: i,j,k, i_start, i_end, j_start, j_end
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) :: rscalar




           i_start = max(its-1, ids)
           i_end = min(ite+1, ide-1)
           j_start = max(jts-1, jds)
           j_end = min(jte+1, jde-1)

           DO j=j_start,j_end
           DO k=kts,min(kte,kde-1)
           DO i=i_start,i_end
              rscalar(i,k,j) = scalar(i,k,j)*mu(i,j)
           ENDDO
           ENDDO
           ENDDO

           CALL relax_bdytend (rscalar, scalar_tend,             &
                               scalar_bxs,scalar_bxe,scalar_bys,scalar_bye, scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,       &
                               'q'        , config_flags, &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )


   END SUBROUTINE relax_bdy_scalar 


   SUBROUTINE spec_bdy_dry ( config_flags,                        &
                             ru_tend, rv_tend, ph_tend, t_tend,   &
                             rw_tend, mu_tend,                    &
                             u_bxs,u_bxe,u_bys,u_bye,             &
                             v_bxs,v_bxe,v_bys,v_bye,             &
                             ph_bxs,ph_bxe,ph_bys,ph_bye,         &
                             t_bxs,t_bxe,t_bys,t_bye,             &
                             w_bxs,w_bxe,w_bys,w_bye,             &
                             mu_bxs,mu_bxe,mu_bys,mu_bye,         &
                             u_btxs,u_btxe,u_btys,u_btye,         &
                             v_btxs,v_btxe,v_btys,v_btye,         &
                             ph_btxs,ph_btxe,ph_btys,ph_btye,     &
                             t_btxs,t_btxe,t_btys,t_btye,         &
                             w_btxs,w_btxe,w_btys,w_btye,         &
                             mu_btxs,mu_btxe,mu_btys,mu_btye,     &
                             spec_bdy_width, spec_zone,           &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             its, ite, jts, jte, kts, kte)
   IMPLICIT NONE

   
   TYPE( grid_config_rec_type ) config_flags


   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, & 
                                            its, ite, jts, jte, kts, kte
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(OUT  ) :: ru_tend, &
                                                                      rv_tend, &
                                                                      ph_tend, &
                                                                      rw_tend, &
                                                                      t_tend
   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(OUT  )          :: mu_tend

   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width    ), INTENT(IN   ) :: u_bxs,u_bxe,  &
                                                                               v_bxs,v_bxe,  &
                                                                               ph_bxs,ph_bxe, &
                                                                               w_bxs,w_bxe, &
                                                                               t_bxs,t_bxe,  &
                                                                               u_btxs,u_btxe, &
                                                                               v_btxs,v_btxe, &
                                                                               ph_btxs,ph_btxe, &
                                                                               w_btxs,w_btxe, &
                                                                               t_btxs,t_btxe

   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width    ), INTENT(IN   ) :: u_bys,u_bye,  &
                                                                               v_bys,v_bye,  &
                                                                               ph_bys,ph_bye, &
                                                                               w_bys,w_bye, &
                                                                               t_bys,t_bye,  &
                                                                               u_btys,u_btye, &
                                                                               v_btys,v_btye, &
                                                                               ph_btys,ph_btye, &
                                                                               w_btys,w_btye, &
                                                                               t_btys,t_btye

   REAL,  DIMENSION( jms:jme , 1:1 ,     spec_bdy_width    ), INTENT(IN   ) :: mu_bxs,mu_bxe, &
                                                                               mu_btxs,mu_btxe

   REAL,  DIMENSION( ims:ime , 1:1 ,     spec_bdy_width    ), INTENT(IN   ) :: mu_bys,mu_bye, &
                                                                               mu_btys,mu_btye
         CALL spec_bdytend (   ru_tend,                &
                               u_bxs,u_bxe,u_bys,u_bye, u_btxs,u_btxe,u_btys,u_btye,    &
                               'u'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )
         CALL spec_bdytend (   rv_tend,                &
                               v_bxs,v_bxe,v_bys,v_bye, v_btxs,v_btxe,v_btys,v_btye,    &
                               'v'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )
         CALL spec_bdytend (   ph_tend,                &
                               ph_bxs,ph_bxe,ph_bys,ph_bye, ph_btxs,ph_btxe,ph_btys,ph_btye,    &
                               'h'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )
         CALL spec_bdytend (   t_tend,                &
                               t_bxs,t_bxe,t_bys,t_bye, t_btxs,t_btxe,t_btys,t_btye,    &
                               't'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )
         CALL spec_bdytend (   mu_tend,                &
                               mu_bxs,mu_bxe,mu_bys,mu_bye, mu_btxs,mu_btxe,mu_btys,mu_btye,       &
                               'm'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, 1  ,1  ,  & 
                               ims,ime, jms,jme, 1  ,1  ,  & 
                               ips,ipe, jps,jpe, 1  ,1  ,  & 
                               its,ite, jts,jte, 1  ,1   )

         if(config_flags%nested)                           &
         CALL spec_bdytend (   rw_tend,                    &
                               w_bxs,w_bxe,w_bys,w_bye, w_btxs,w_btxe,w_btys,w_btye,                  &
                               'h'     , config_flags,     &
                               spec_bdy_width, spec_zone,  &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )

   END SUBROUTINE spec_bdy_dry 










   SUBROUTINE spec_bdy_dry_perturb ( config_flags,                                   &
                                     ru_tend, rv_tend, t_tend,mu_2, mub,             &
      			             msfu, msfv, msft,                               &
                                     field_u_tend_perturb,field_v_tend_perturb,field_t_tend_perturb, &
				     spec_bdy_width, spec_zone,  &
                                     kme_stoch,                  & 
                                     ids,ide, jds,jde, kds,kde,  & 
				     ims,ime, jms,jme, kms,kme,  & 
                                     ips,ipe, jps,jpe, kps,kpe,  & 
                                     its, ite, jts, jte, kts, kte)
   IMPLICIT NONE

   
   TYPE( grid_config_rec_type ) config_flags


   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, & 
                                            its, ite, jts, jte, kts, kte, & 
                                            kme_stoch
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone

   REAL , DIMENSION( ims:ime , kms:kme ,jms:jme  ) , INTENT(INOUT) :: ru_tend, &
                                                                      rv_tend, &
                                                                      t_tend

   REAL,  DIMENSION( ims:ime ,           jms:jme ), INTENT(INOUT  )   :: mu_2	
   REAL,  DIMENSION( ims:ime ,           jms:jme ), INTENT(INOUT  )   :: mub
   REAL,  DIMENSION( ims:ime ,           jms:jme ), INTENT(IN     )   :: msfu
   REAL,  DIMENSION( ims:ime ,           jms:jme ), INTENT(IN     )   :: msfv
   REAL,  DIMENSION( ims:ime ,           jms:jme ), INTENT(IN     )   :: msft

   REAL,  DIMENSION( ims:ime , kms:kme_stoch , jms:jme ), INTENT(IN     )   :: field_u_tend_perturb, &
                                                                         field_v_tend_perturb, &
                                                                         field_t_tend_perturb

         CALL spec_bdytend_perturb (   ru_tend,            &
                               field_u_tend_perturb, mu_2,mub,   &
                               'u', msfu, config_flags,    &
                               spec_bdy_width, spec_zone,  & 
                               kme_stoch,                  & 
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )
         CALL spec_bdytend_perturb (   rv_tend,            &
                               field_v_tend_perturb,mu_2,mub,    &
                               'v', msfv, config_flags,    &
                               spec_bdy_width, spec_zone,  &
                               kme_stoch,                  & 
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )

         CALL spec_bdytend_perturb (   t_tend,             &
                               field_t_tend_perturb,mu_2,mub,    &
                               't', msft, config_flags,    &
                               spec_bdy_width, spec_zone,  &
                               kme_stoch,                  & 
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )

   END SUBROUTINE spec_bdy_dry_perturb 




   SUBROUTINE spec_bdy_chem_perturb (periodic_x,                &
                                     field_bdy_tend_xs, field_bdy_tend_xe, &
                                     field_bdy_tend_ys, field_bdy_tend_ye, &
                                     field_scalar_perturb, &
                                     spec_bdy_width, spec_zone,  &
                                     kme_stoch,                  & 
                                     ids,ide, jds,jde, kds,kde,  & 
				     ims,ime, jms,jme, kms,kme,  & 
                                     ips,ipe, jps,jpe, kps,kpe,  & 
                                     its, ite, jts, jte, kts, kte)
   IMPLICIT NONE


   LOGICAL ,               INTENT(IN   ) :: periodic_x
   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, & 
                                            its, ite, jts, jte, kts, kte, & 
                                            kme_stoch
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),        INTENT(IN   ) :: field_scalar_perturb
   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(INOUT) :: field_bdy_tend_xs, field_bdy_tend_xe
   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(INOUT) :: field_bdy_tend_ys, field_bdy_tend_ye


         CALL spec_bdytend_perturb_chem ( field_bdy_tend_xs, field_bdy_tend_xe, &
                                          field_bdy_tend_ys, field_bdy_tend_ye, &
                                          field_scalar_perturb, 'c',       &
                                          periodic_x,                 &
                                          spec_bdy_width, spec_zone,  &
                                          kme_stoch,                  & 
                                          ids,ide, jds,jde, kds,kde,  & 
                                          ims,ime, jms,jme, kms,kme,  & 
                                          ips,ipe, jps,jpe, kps,kpe,  & 
                                          its,ite, jts,jte, kts,kte )

   END SUBROUTINE spec_bdy_chem_perturb   




   SUBROUTINE spec_bdy_scalar ( scalar_tend,    &
                          scalar_bxs,scalar_bxe,scalar_bys,scalar_bye,  &
                          scalar_btxs,scalar_btxe,scalar_btys,scalar_btye, &
                          spec_bdy_width, spec_zone,                   &
                          config_flags,               &
                          ids,ide, jds,jde, kds,kde,  & 
                          ims,ime, jms,jme, kms,kme,  & 
                          ips,ipe, jps,jpe, kps,kpe,  & 
                          its, ite, jts, jte, kts, kte)
   IMPLICIT NONE

   
   TYPE( grid_config_rec_type ) config_flags


   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, & 
                                            its, ite, jts, jte, kts, kte
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(OUT  ) :: scalar_tend

   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width    ), INTENT(IN   ) :: scalar_bxs,scalar_bxe, &
                                                                               scalar_btxs,scalar_btxe

   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width    ), INTENT(IN   ) :: scalar_bys,scalar_bye, &
                                                                               scalar_btys,scalar_btye


   INTEGER :: i,j,k


         CALL spec_bdytend (   scalar_tend,                &
                               scalar_bxs,scalar_bxe,scalar_bys,scalar_bye, scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,    &
                               'q'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )


   END SUBROUTINE spec_bdy_scalar 



   SUBROUTINE set_phys_bc_dry_1( config_flags, u_1, u_2, v_1, v_2,   &
                                 rw_1, rw_2, w_1, w_2,           &
                                 t_1, t_2, tp_1, tp_2, pp, pip,  &
                                 ids,ide, jds,jde, kds,kde,      &
                                 ims,ime, jms,jme, kms,kme,      &
                                 ips,ipe, jps,jpe, kps,kpe,      &
                                 its,ite, jts,jte, kts,kte      )






      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      TYPE( grid_config_rec_type ) config_flags

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: &
           u_1,u_2, v_1, v_2, rw_1, rw_2, w_1, w_2,                  &
           t_1, t_2, tp_1, tp_2, pp, pip



      CALL set_physical_bc3d( u_1  , 'u', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( u_2  , 'u', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( v_1  , 'v', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( v_2  , 'v', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( rw_1 , 'w', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( rw_2 , 'w', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( w_1  , 'w', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( w_2  , 'w', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( t_1, 'p', config_flags,                 &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( t_2, 'p', config_flags,                 &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( tp_1, 'p', config_flags,                &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( tp_2, 'p', config_flags,                &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( pp , 'p', config_flags,                 &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( pip , 'p', config_flags,                &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )

  END SUBROUTINE set_phys_bc_dry_1



   SUBROUTINE set_phys_bc_dry_2( config_flags,                     &
                                 u_1, u_2, v_1, v_2, w_1, w_2,     &
                                 t_1, t_2, ph_1, ph_2, mu_1, mu_2, &
                                 ids,ide, jds,jde, kds,kde,        &
                                 ims,ime, jms,jme, kms,kme,        &
                                 ips,ipe, jps,jpe, kps,kpe,        &
                                 its,ite, jts,jte, kts,kte        )






      IMPLICIT NONE

      TYPE( grid_config_rec_type ) config_flags

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: &
         u_1, u_2, v_1, v_2, w_1, w_2,                       &
         t_1, t_2, ph_1, ph_2

      REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: &
                             mu_1, mu_2


      CALL set_physical_bc3d( u_1, 'U', config_flags,           &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( u_2, 'U', config_flags,           &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( v_1 , 'V', config_flags,          &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( v_2 , 'V', config_flags,          &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( w_1, 'w', config_flags,           &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( w_2, 'w', config_flags,           &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( t_1, 'p', config_flags,           &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( t_2, 'p', config_flags,           &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( ph_1 , 'w', config_flags,         &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( ph_2 , 'w', config_flags,         &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc2d( mu_1, 't', config_flags, &
                              ids, ide, jds, jde,  &
                              ims, ime, jms, jme,  &
                              ips, ipe, jps, jpe,  &
                              its, ite, jts, jte  )

      CALL set_physical_bc2d( mu_2, 't', config_flags, &
                              ids, ide, jds, jde,  &
                              ims, ime, jms, jme,  &
                              ips, ipe, jps, jpe,  &
                              its, ite, jts, jte  )

   END SUBROUTINE set_phys_bc_dry_2



   SUBROUTINE set_phys_bc_smallstep_1( config_flags, ru_1, du, rv_1, dv,   &
                                       ids,ide, jds,jde, kds,kde,      &
                                       ims,ime, jms,jme, kms,kme,      &
                                       ips,ipe, jps,jpe, kps,kpe,      &
                                       its,ite, jts,jte, kts,kte      )






      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      TYPE( grid_config_rec_type ) config_flags

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: &
           ru_1,du, rv_1, dv

      CALL set_physical_bc3d( ru_1  , 'u', config_flags,              &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kde )
      CALL set_physical_bc3d( du , 'u', config_flags,                 &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kde )
      CALL set_physical_bc3d( rv_1  , 'v', config_flags,              &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kde )
      CALL set_physical_bc3d( dv  , 'v', config_flags,                &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kde )

  END SUBROUTINE set_phys_bc_smallstep_1



   SUBROUTINE rk_phys_bc_dry_1( config_flags, u, v, rw, w,  &
                                muu, muv, mut, php, alt, p, &
                                ids,ide, jds,jde, kds,kde,  &
                                ims,ime, jms,jme, kms,kme,  &
                                ips,ipe, jps,jpe, kps,kpe,  &
                                its,ite, jts,jte, kts,kte  )






      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      TYPE( grid_config_rec_type ) config_flags

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                    &
                                INTENT(INOUT) ::  u, v, rw, w, php, alt, p
      REAL, DIMENSION( ims:ime, jms:jme ),                             &
                                INTENT(INOUT) ::    muu, muv, mut

      CALL set_physical_bc3d( u  , 'u', config_flags,             &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( v  , 'v', config_flags,             &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d(rw , 'w', config_flags,              &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( w , 'w', config_flags,              &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( php , 'w', config_flags,            &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( alt, 't', config_flags,             &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( p, 'p', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc2d( muu, 'u', config_flags,  &
                              ids, ide, jds, jde,      &
                              ims, ime, jms, jme,      &
                              ips, ipe, jps, jpe,      &
                              its, ite, jts, jte  )

      CALL set_physical_bc2d( muv, 'v', config_flags,  &
                              ids, ide, jds, jde,      &
                              ims, ime, jms, jme,      &
                              ips, ipe, jps, jpe,      &
                              its, ite, jts, jte  )

      CALL set_physical_bc2d( mut, 't', config_flags,  &
                              ids, ide, jds, jde,      &
                              ims, ime, jms, jme,      &
                              ips, ipe, jps, jpe,      &
                              its, ite, jts, jte  )

  END SUBROUTINE rk_phys_bc_dry_1



  SUBROUTINE rk_phys_bc_dry_2( config_flags, u, v, w,      &
                               t, ph, mu,                  &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               ips,ipe, jps,jpe, kps,kpe,  &
                               its,ite, jts,jte, kts,kte  )






      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      TYPE( grid_config_rec_type ) config_flags

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: &
                             u, v, w, t, ph

      REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: &
                             mu

      CALL set_physical_bc3d( u   , 'U', config_flags,            &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( v   , 'V', config_flags,            &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( w  , 'w', config_flags,             &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( t, 'p', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( ph  , 'w', config_flags,            &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc2d( mu, 't', config_flags, &
                              ids, ide, jds, jde,    &
                              ims, ime, jms, jme,    &
                              ips, ipe, jps, jpe,    &
                              its, ite, jts, jte    )

  END SUBROUTINE rk_phys_bc_dry_2



   SUBROUTINE zero_bdytend  (                                                  &
                              u_btxs,u_btxe,u_btys,u_btye,                     &
                              v_btxs,v_btxe,v_btys,v_btye,                     &
                              ph_btxs,ph_btxe,ph_btys,ph_btye,                 &
                              t_btxs,t_btxe,t_btys,t_btye,                     &
                              w_btxs,w_btxe,w_btys,w_btye,                     &
                              mu_btxs,mu_btxe,mu_btys,mu_btye,                 &
                              moist_btxs,moist_btxe,   &
                              moist_btys,moist_btye,   &
                              scalar_btxs,scalar_btxe,   &
                              scalar_btys,scalar_btye,   &
                              spec_bdy_width,n_moist,n_scalar,                 &
                              ids,ide, jds,jde, kds,kde,  & 
                              ims,ime, jms,jme, kms,kme,  & 
                              ips,ipe, jps,jpe, kps,kpe,  & 
                              its,ite, jts,jte, kts,kte   )
   IMPLICIT NONE

   

   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, n_moist,n_scalar

   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, &
                                            its, ite, jts, jte, kts, kte

   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width    ), INTENT(INOUT) :: u_btxs,u_btxe, &
                                                                               v_btxs,v_btxe, &
                                                                               ph_btxs,ph_btxe, &
                                                                               w_btxs,w_btxe, &
                                                                               t_btxs,t_btxe

   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width    ), INTENT(INOUT) :: u_btys,u_btye, &
                                                                               v_btys,v_btye, &
                                                                               ph_btys,ph_btye, &
                                                                               w_btys,w_btye, &
                                                                               t_btys,t_btye

   REAL,  DIMENSION( jms:jme , 1:1     , spec_bdy_width    ), INTENT(INOUT) :: mu_btxs,mu_btxe
   REAL,  DIMENSION( ims:ime , 1:1     , spec_bdy_width    ), INTENT(INOUT) :: mu_btys,mu_btye

   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width , n_moist ), INTENT(INOUT) :: moist_btxs,moist_btxe
   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width , n_moist ), INTENT(INOUT) :: moist_btys,moist_btye

   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width , n_scalar ), INTENT(INOUT) :: scalar_btxs,scalar_btxe
   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width , n_scalar ), INTENT(INOUT) :: scalar_btys,scalar_btye




       CALL wrf_debug( 10, 'In zero_bdytend, setting bdy tendencies to 0 during DFI' )
       u_btxs = 0.
       u_btxe = 0.
       u_btys = 0.
       u_btye = 0.
       v_btxs = 0.
       v_btxe = 0.
       v_btys = 0.
       v_btye = 0.
       t_btxs = 0.
       t_btxe = 0.
       t_btys = 0.
       t_btye = 0.
       ph_btxs = 0.
       ph_btxe = 0.
       ph_btys = 0.
       ph_btye = 0.
       mu_btxs = 0.
       mu_btxe = 0.
       mu_btys = 0.
       mu_btye = 0.
       moist_btxs = 0.
       moist_btxe = 0.
       moist_btys = 0.
       moist_btye = 0.
       scalar_btxs = 0.
       scalar_btxe = 0.
       scalar_btys = 0.
       scalar_btye = 0.




   END SUBROUTINE zero_bdytend



  SUBROUTINE set_w_surface( config_flags, znw, fill_w_flag,              &
                            w, ht, u, v, cf1, cf2, cf3, rdx, rdy,        &
                            msftx, msfty,                                &
                            ids, ide, jds, jde, kds, kde,                &
                            ims, ime, jms, jme, kms, kme,                &
                            its, ite, jts, jte, kts, kte                 )
     implicit none

     TYPE( grid_config_rec_type ) config_flags

     INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte

     REAL :: rdx, rdy, cf1, cf2, cf3

     REAL , DIMENSION(  ims:ime , kms:kme, jms:jme ) ,                      &
                                                 INTENT(IN   ) ::  u,       &
                                                                   v

     REAL , DIMENSION(  ims:ime , kms:kme, jms:jme ) ,                      &
                                              INTENT(INOUT) ::  w

     REAL , DIMENSION(  ims:ime , jms:jme ) , INTENT(IN   ) ::  ht,         &
                                                                msftx,      &
                                                                msfty
     REAL , DIMENSION( kms:kme ) , INTENT(IN   ) ::  znw
  
     LOGICAL, INTENT(IN   ) :: fill_w_flag


     INTEGER :: i,j,k
     INTEGER :: ip1,im1,jp1,jm1
     INTEGER :: ip1_limit,im1_limit,jp1_limit,jm1_limit
















     jm1_limit = jds        
     jp1_limit = jde-1
     im1_limit = ids
     ip1_limit = ide-1

     IF ( config_flags%periodic_x ) THEN
       im1_limit = ids-1
       ip1_limit = ide
     ENDIF

     IF ( config_flags%periodic_y ) THEN
       jm1_limit = jds-1
       jp1_limit = jde
     ENDIF

     DO j = jts,min(jte,jde-1)
       jm1 = max(j-1, jm1_limit)
       jp1 = min(j+1, jp1_limit)
     DO i = its,min(ite,ide-1)
       im1 = max(i-1, im1_limit)
       ip1 = min(i+1, ip1_limit)

         w(i,1,j)=  msfty(i,j)*                              &
                  .5*rdy*(                                   &
                           (ht(i,jp1)-ht(i,j  ))             &
          *(cf1*v(i,1,j+1)+cf2*v(i,2,j+1)+cf3*v(i,3,j+1))    &
                          +(ht(i,j  )-ht(i,jm1))             &
          *(cf1*v(i,1,j  )+cf2*v(i,2,j  )+cf3*v(i,3,j  ))  ) &
                    +msftx(i,j)*                             &
                  .5*rdx*(                                   &
                           (ht(ip1,j)-ht(i,j  ))             &
          *(cf1*u(i+1,1,j)+cf2*u(i+1,2,j)+cf3*u(i+1,3,j))    &
                          +(ht(i  ,j)-ht(im1,j))             &
          *(cf1*u(i  ,1,j)+cf2*u(i  ,2,j)+cf3*u(i  ,3,j))  )

      ENDDO
      ENDDO


      IF (fill_w_flag) THEN
        DO j = jts,min(jte,jde-1)
        DO k = kts+1,kte
        DO i = its,min(ite,ide-1)
          w(i,k,j) = w(i,1,j)*znw(k)*znw(k)
        ENDDO
        ENDDO
        ENDDO
      ENDIF

  END SUBROUTINE set_w_surface

  SUBROUTINE lbc_fcx_gcx ( fcx , gcx , spec_bdy_width , &
                           spec_zone , relax_zone , dt , spec_exp , &
                           specified , nested )
  
     IMPLICIT NONE
  
     INTEGER , INTENT(IN) :: spec_bdy_width , spec_zone , relax_zone
     REAL , INTENT(IN) :: dt , spec_exp
     LOGICAL , INTENT(IN) :: specified , nested
     REAL , DIMENSION(spec_bdy_width) :: fcx , gcx
  
     
  
     INTEGER :: loop 
     REAL :: spongeweight
  
     IF (specified) THEN
       
       
       
       DO loop = spec_zone + 1, spec_zone + relax_zone
         fcx(loop) = 0.1 / dt * (spec_zone + relax_zone - loop) / (relax_zone - 1)
         gcx(loop) = 1.0 / dt / 50. * (spec_zone + relax_zone - loop) / (relax_zone - 1)
         spongeweight=exp(-(loop-(spec_zone + 1))*spec_exp)
         fcx(loop) = fcx(loop)*spongeweight
         gcx(loop) = gcx(loop)*spongeweight
       ENDDO   
       
     ELSE IF (nested) THEN
       
       
       
       DO loop = spec_zone + 1, spec_zone + relax_zone
         fcx(loop) = 0.1 / dt * (spec_zone + relax_zone - loop) / (relax_zone - 1)
         gcx(loop) = 1.0 / dt / 50. * (spec_zone + relax_zone - loop) / (relax_zone - 1)





       ENDDO
       
     ENDIF
  
  END SUBROUTINE lbc_fcx_gcx



   SUBROUTINE theta_and_thetam_lbc_only (                              &
                                theta_to_thetam,                       &
                                mub,                                   &
                                mu_bdy_xs, mu_bdy_xe,                  &
                                mu_bdy_ys, mu_bdy_ye,                  &
                                mu_bdy_tend_xs, mu_bdy_tend_xe,        &
                                mu_bdy_tend_ys, mu_bdy_tend_ye,        &
                                t_bdy_xs, t_bdy_xe,                    &
                                t_bdy_ys, t_bdy_ye,                    &
                                t_bdy_tend_xs, t_bdy_tend_xe,          &
                                t_bdy_tend_ys, t_bdy_tend_ye,          &
                                moist_bdy_xs, moist_bdy_xe,            &
                                moist_bdy_ys, moist_bdy_ye,            &
                                moist_bdy_tend_xs, moist_bdy_tend_xe,  &
                                moist_bdy_tend_ys, moist_bdy_tend_ye,  &
                                spec_bdy_width,                        &
                                dt_interval,                           &
                                ids,ide, jds,jde, kds,kde,             &
                                ims,ime, jms,jme, kms,kme,             &
                                ips,ipe, jps,jpe, kps,kpe,             &
                                its,ite, jts,jte, kts,kte              )

      IMPLICIT NONE

      
      
      
      
      
      

      LOGICAL,      INTENT(IN   )    :: theta_to_thetam

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_bdy_width
      REAL   ,      INTENT(IN   )    :: dt_interval

      REAL,  DIMENSION( ims:ime ,           jms:jme        ), INTENT(IN   ) :: mub

      REAL,  DIMENSION( jms:jme , 1       , spec_bdy_width ), INTENT(IN   ) :: mu_bdy_xs, mu_bdy_xe
      REAL,  DIMENSION( ims:ime , 1       , spec_bdy_width ), INTENT(IN   ) :: mu_bdy_ys, mu_bdy_ye
      REAL,  DIMENSION( jms:jme , 1       , spec_bdy_width ), INTENT(IN   ) :: mu_bdy_tend_xs, mu_bdy_tend_xe
      REAL,  DIMENSION( ims:ime , 1       , spec_bdy_width ), INTENT(IN   ) :: mu_bdy_tend_ys, mu_bdy_tend_ye

      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(INOUT) :: t_bdy_xs, t_bdy_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(INOUT) :: t_bdy_ys, t_bdy_ye
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(INOUT) :: t_bdy_tend_xs, t_bdy_tend_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(INOUT) :: t_bdy_tend_ys, t_bdy_tend_ye

      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(IN   ) :: moist_bdy_xs, moist_bdy_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(IN   ) :: moist_bdy_ys, moist_bdy_ye
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(IN   ) :: moist_bdy_tend_xs, moist_bdy_tend_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(IN   ) :: moist_bdy_tend_ys, moist_bdy_tend_ye

      

      INTEGER    :: i, j, k, ii, jj

      REAL :: dt_time_until_next_lbc

      REAL :: mu_old_bdy_xs         , mu_old_bdy_xe
      REAL :: mu_old_bdy_ys         , mu_old_bdy_ye
      REAL :: mu_new_bdy_xs         , mu_new_bdy_xe
      REAL :: mu_new_bdy_ys         , mu_new_bdy_ye

      REAL :: t_old_bdy_xs          , t_old_bdy_xe
      REAL :: t_old_bdy_ys          , t_old_bdy_ye
      REAL :: t_new_bdy_xs          , t_new_bdy_xe
      REAL :: t_new_bdy_ys          , t_new_bdy_ye
      REAL :: t_old_bdy_tend_xs     , t_old_bdy_tend_xe
      REAL :: t_old_bdy_tend_ys     , t_old_bdy_tend_ye

      REAL :: moist_old_bdy_xs      , moist_old_bdy_xe
      REAL :: moist_old_bdy_ys      , moist_old_bdy_ye
      REAL :: moist_new_bdy_xs      , moist_new_bdy_xe
      REAL :: moist_new_bdy_ys      , moist_new_bdy_ye
      REAL :: moist_old_bdy_tend_xs , moist_old_bdy_tend_xe
      REAL :: moist_old_bdy_tend_ys , moist_old_bdy_tend_ye

      INTEGER :: i_min, i_max, j_min, j_max

      
      
      
      
      
      
      

      
      
      
      
      
      

      
      
      
      
      
      
      
      

      
      
      
      

      
      
      

      IF ( its .EQ. ips ) THEN
         i_min = ips-4
      ELSE
         i_min = its
      END IF

      IF ( ite .EQ. ipe ) THEN
         i_max = ipe+4
      ELSE
         i_max = ite
      END IF

      
      

      
      

      DO jj = MAX(jts,1) , MIN(jte,jde-1,spec_bdy_width)
         j = jj
         DO k = kts , kte-1


            DO i = MAX(1,i_min) , MIN(i_max,ide-1)
               mu_old_bdy_ys         =   mu_bdy_ys(i,1,j)    + mub(i,jj)
               t_old_bdy_ys          = ( t_bdy_ys(i,k,j)                                            ) / mu_old_bdy_ys
               moist_old_bdy_ys      = ( moist_bdy_ys(i,k,j)                                        ) / mu_old_bdy_ys
               mu_new_bdy_ys         =   mu_old_bdy_ys       + mu_bdy_tend_ys(i,1,j)   *dt_interval
               t_new_bdy_ys          = ( t_bdy_ys(i,k,j)     + t_bdy_tend_ys(i,k,j)    *dt_interval ) / mu_new_bdy_ys
               moist_new_bdy_ys      = ( moist_bdy_ys(i,k,j) + moist_bdy_tend_ys(i,k,j)*dt_interval ) / mu_new_bdy_ys
               t_old_bdy_tend_ys     = ( t_new_bdy_ys        - t_old_bdy_ys                         ) / dt_interval
               moist_old_bdy_tend_ys = ( moist_new_bdy_ys    - moist_old_bdy_ys                     ) / dt_interval
               IF ( theta_to_thetam ) THEN
                  t_bdy_ys(i,k,j) = ( ( ( t_old_bdy_ys + T0 ) * ( 1. + (R_v/R_d) * moist_old_bdy_ys ) ) - T0 ) * mu_old_bdy_ys
                  t_bdy_tend_ys(i,k,j) = ( ( mu_new_bdy_ys * ( ( t_new_bdy_ys + T0 ) * ( 1. + (R_v/R_d) * moist_new_bdy_ys ) - T0 ) ) - &
                                           ( mu_old_bdy_ys * ( ( t_old_bdy_ys + T0 ) * ( 1. + (R_v/R_d) * moist_old_bdy_ys ) - T0 ) ) ) / dt_interval
               ELSE
                  t_bdy_ys(i,k,j) = ( ( ( t_old_bdy_ys + T0 ) / ( 1. + (R_v/R_d) * moist_old_bdy_ys ) ) - T0 ) * mu_old_bdy_ys
                  t_bdy_tend_ys(i,k,j) = ( ( mu_new_bdy_ys * ( ( t_new_bdy_ys + T0 ) / ( 1. + (R_v/R_d) * moist_new_bdy_ys ) - T0 ) ) - &
                                           ( mu_old_bdy_ys * ( ( t_old_bdy_ys + T0 ) / ( 1. + (R_v/R_d) * moist_old_bdy_ys ) - T0 ) ) ) / dt_interval
               END IF
            END DO
         END DO
      END DO

      
      

      DO jj = MIN(jde-1,jte) , MAX(jde-spec_bdy_width,jts) , -1
         j = jde-jj
         DO k = kts , kte-1


            DO i = MAX(1,i_min) , MIN(i_max,ide-1)
               mu_old_bdy_ye         =   mu_bdy_ye(i,1,j)    + mub(i,jj)
               t_old_bdy_ye          = ( t_bdy_ye(i,k,j)                                            ) / mu_old_bdy_ye
               moist_old_bdy_ye      = ( moist_bdy_ye(i,k,j)                                        ) / mu_old_bdy_ye
               mu_new_bdy_ye         =   mu_old_bdy_ye       + mu_bdy_tend_ye(i,1,j)   *dt_interval
               t_new_bdy_ye          = ( t_bdy_ye(i,k,j)     + t_bdy_tend_ye(i,k,j)    *dt_interval ) / mu_new_bdy_ye
               moist_new_bdy_ye      = ( moist_bdy_ye(i,k,j) + moist_bdy_tend_ye(i,k,j)*dt_interval ) / mu_new_bdy_ye
               t_old_bdy_tend_ye     = ( t_new_bdy_ye        - t_old_bdy_ye                         ) / dt_interval
               moist_old_bdy_tend_ye = ( moist_new_bdy_ye    - moist_old_bdy_ye                     ) / dt_interval
               IF ( theta_to_thetam ) THEN
                  t_bdy_ye(i,k,j) = ( ( ( t_old_bdy_ye + T0 ) * ( 1. + (R_v/R_d) * moist_old_bdy_ye ) ) - T0 ) * mu_old_bdy_ye
                  t_bdy_tend_ye(i,k,j) = ( ( mu_new_bdy_ye * ( ( t_new_bdy_ye + T0 ) * ( 1. + (R_v/R_d) * moist_new_bdy_ye ) - T0 ) ) - &
                                           ( mu_old_bdy_ye * ( ( t_old_bdy_ye + T0 ) * ( 1. + (R_v/R_d) * moist_old_bdy_ye ) - T0 ) ) ) / dt_interval
               ELSE
                  t_bdy_ye(i,k,j) = ( ( ( t_old_bdy_ye + T0 ) / ( 1. + (R_v/R_d) * moist_old_bdy_ye ) ) - T0 ) * mu_old_bdy_ye
                  t_bdy_tend_ye(i,k,j) = ( ( mu_new_bdy_ye * ( ( t_new_bdy_ye + T0 ) / ( 1. + (R_v/R_d) * moist_new_bdy_ye ) - T0 ) ) - &
                                           ( mu_old_bdy_ye * ( ( t_old_bdy_ye + T0 ) / ( 1. + (R_v/R_d) * moist_old_bdy_ye ) - T0 ) ) ) / dt_interval
               END IF
            END DO
         END DO
      END DO

      
      
      

      IF ( jts .EQ. jps ) THEN
         j_min = jps-4
      ELSE
         j_min = jts
      END IF

      IF ( jte .EQ. jpe ) THEN
         j_max = jpe+4
      ELSE
         j_max = jte
      END IF

      
      

      
      

      DO ii = MAX(its,1) , MIN(ite,ide-1,spec_bdy_width)
         i = ii
         DO k = kts , kte-1


            DO j = MAX(1,j_min) , MIN(j_max,jde-1)
               mu_old_bdy_xs         =   mu_bdy_xs(j,1,i)    + mub(ii,j)
               t_old_bdy_xs          = ( t_bdy_xs(j,k,i)                                            ) / mu_old_bdy_xs
               moist_old_bdy_xs      = ( moist_bdy_xs(j,k,i)                                        ) / mu_old_bdy_xs
               mu_new_bdy_xs         =   mu_old_bdy_xs       + mu_bdy_tend_xs(j,1,i)   *dt_interval
               t_new_bdy_xs          = ( t_bdy_xs(j,k,i)     + t_bdy_tend_xs(j,k,i)    *dt_interval ) / mu_new_bdy_xs
               moist_new_bdy_xs      = ( moist_bdy_xs(j,k,i) + moist_bdy_tend_xs(j,k,i)*dt_interval ) / mu_new_bdy_xs
               t_old_bdy_tend_xs     = ( t_new_bdy_xs        - t_old_bdy_xs                         ) / dt_interval
               moist_old_bdy_tend_xs = ( moist_new_bdy_xs    - moist_old_bdy_xs                     ) / dt_interval
               IF ( theta_to_thetam ) THEN
                  t_bdy_xs(j,k,i) = ( ( ( t_old_bdy_xs + T0 ) * ( 1. + (R_v/R_d) * moist_old_bdy_xs ) ) - T0 ) * mu_old_bdy_xs
                  t_bdy_tend_xs(j,k,i) = ( ( mu_new_bdy_xs * ( ( t_new_bdy_xs + T0 ) * ( 1. + (R_v/R_d) * moist_new_bdy_xs ) - T0 ) ) - &
                                           ( mu_old_bdy_xs * ( ( t_old_bdy_xs + T0 ) * ( 1. + (R_v/R_d) * moist_old_bdy_xs ) - T0 ) ) ) / dt_interval
               ELSE
                  t_bdy_xs(j,k,i) = ( ( ( t_old_bdy_xs + T0 ) / ( 1. + (R_v/R_d) * moist_old_bdy_xs ) ) - T0 ) * mu_old_bdy_xs
                  t_bdy_tend_xs(j,k,i) = ( ( mu_new_bdy_xs * ( ( t_new_bdy_xs + T0 ) / ( 1. + (R_v/R_d) * moist_new_bdy_xs ) - T0 ) ) - &
                                           ( mu_old_bdy_xs * ( ( t_old_bdy_xs + T0 ) / ( 1. + (R_v/R_d) * moist_old_bdy_xs ) - T0 ) ) ) / dt_interval
               END IF
            END DO
         END DO
      END DO

      
      

      DO ii = MIN(ide-1,ite) , MAX(ide-spec_bdy_width,its) , -1
         i = ide-ii
         DO k = kts , kte-1


            DO j = MAX(1,j_min) , MIN(j_max,jde-1)
               mu_old_bdy_xe         =   mu_bdy_xe(j,1,i)    + mub(ii,j)
               t_old_bdy_xe          = ( t_bdy_xe(j,k,i)                                            ) / mu_old_bdy_xe
               moist_old_bdy_xe      = ( moist_bdy_xe(j,k,i)                                        ) / mu_old_bdy_xe
               mu_new_bdy_xe         =   mu_old_bdy_xe       + mu_bdy_tend_xe(j,1,i)   *dt_interval
               t_new_bdy_xe          = ( t_bdy_xe(j,k,i)     + t_bdy_tend_xe(j,k,i)    *dt_interval ) / mu_new_bdy_xe
               moist_new_bdy_xe      = ( moist_bdy_xe(j,k,i) + moist_bdy_tend_xe(j,k,i)*dt_interval ) / mu_new_bdy_xe
               t_old_bdy_tend_xe     = ( t_new_bdy_xe        - t_old_bdy_xe                         ) / dt_interval
               moist_old_bdy_tend_xe = ( moist_new_bdy_xe    - moist_old_bdy_xe                     ) / dt_interval
               IF ( theta_to_thetam ) THEN
                  t_bdy_xe(j,k,i) = ( ( ( t_old_bdy_xe + T0 ) * ( 1. + (R_v/R_d) * moist_old_bdy_xe ) ) - T0 ) * mu_old_bdy_xe
                  t_bdy_tend_xe(j,k,i) = ( ( mu_new_bdy_xe * ( ( t_new_bdy_xe + T0 ) * ( 1. + (R_v/R_d) * moist_new_bdy_xe ) - T0 ) ) - &
                                           ( mu_old_bdy_xe * ( ( t_old_bdy_xe + T0 ) * ( 1. + (R_v/R_d) * moist_old_bdy_xe ) - T0 ) ) ) / dt_interval
               ELSE
                  t_bdy_xe(j,k,i) = ( ( ( t_old_bdy_xe + T0 ) / ( 1. + (R_v/R_d) * moist_old_bdy_xe ) ) - T0 ) * mu_old_bdy_xe
                  t_bdy_tend_xe(j,k,i) = ( ( mu_new_bdy_xe * ( ( t_new_bdy_xe + T0 ) / ( 1. + (R_v/R_d) * moist_new_bdy_xe ) - T0 ) ) - &
                                           ( mu_old_bdy_xe * ( ( t_old_bdy_xe + T0 ) / ( 1. + (R_v/R_d) * moist_old_bdy_xe ) - T0 ) ) ) / dt_interval
               END IF
            END DO
         END DO
      END DO

   END SUBROUTINE theta_and_thetam_lbc_only


  
END MODULE module_bc_em
