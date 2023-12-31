
























MODULE module_advect_em

  USE module_bc
  USE module_model_constants
  USE module_wrf_error

CONTAINS



SUBROUTINE advect_u   ( u, u_old, tendency,            &
                        ru, rv, rom,                   &
                        mut, time_step, config_flags,  &
                        msfux, msfuy, msfvx, msfvy,    &
                        msftx, msfty,                  &
                        fzm, fzp,                      &
                        rdx, rdy, rdzw,                &
                        ids, ide, jds, jde, kds, kde,  &
                        ims, ime, jms, jme, kms, kme,  &
                        its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: u,     &
                                                                      u_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy
   INTEGER ,                                     INTENT(IN   ) :: time_step

   
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax, im, ip
   INTEGER :: jp1, jp0, jtmp

   INTEGER :: horz_order, vert_order

   REAL    :: mrdx, mrdy, ub, vb, uw, vw, dvm, dvp
   REAL , DIMENSION(its:ite, kts:kte) :: vflux


   REAL,  DIMENSION( its-1:ite+1, kts:kte ) :: fqx
   REAL,  DIMENSION( its:ite, kts:kte, 2) :: fqy
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye



   REAL    :: flux3, flux4, flux5, flux6
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel

   flux4(q_im2, q_im1, q_i, q_ip1, ua) =                         &
          ( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0

   flux3(q_im2, q_im1, q_i, q_ip1, ua) =                         &
            flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
            sign(1,time_step)*sign(1.,ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0

   flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =           &
                      ( 37.*(q_i+q_im1) - 8.*(q_ip1+q_im2)       &
                     +(q_ip2+q_im3) )/60.0

   flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =           &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)     &
            -sign(1,time_step)*sign(1.,ua)*(                     &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )/60.0


   LOGICAL :: specified

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.



   horz_order = config_flags%h_mom_adv_order
   vert_order = config_flags%v_mom_adv_order

   ktf=MIN(kte,kde-1)



   horizontal_order_test : IF( horz_order == 6 ) THEN








   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-4)                ) degrade_ye = .false.



      i_start = its
      i_end   = ite
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-1,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

      j_start = jts
      j_end   = MIN(jte,jde-1)




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)



     jp1 = 2
     jp0 = 1

     j_loop_y_flux_6 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN  

        DO k=kts,ktf
        DO i = i_start, i_end
          vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
          fqy( i, k, jp1 ) = vel*flux6(               &
                  u(i,k,j-3), u(i,k,j-2), u(i,k,j-1),       &
                  u(i,k,j  ), u(i,k,j+1), u(i,k,j+2),  vel )
        ENDDO
        ENDDO



      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i-1,k,j))  &
                                     *(u(i,k,j)+u(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
              fqy( i, k, jp1 ) = vel*flux4(      &
                   u(i,k,j-2),u(i,k,j-1), u(i,k,j),u(i,k,j+1),vel )
            ENDDO
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i-1,k,j))    &
                     *(u(i,k,j)+u(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
              fqy( i, k, jp1 ) = vel*flux4(     &
                   u(i,k,j-2),u(i,k,j-1),    &
                   u(i,k,j),u(i,k,j+1),vel )
            ENDDO
            ENDDO

      END IF



        
        IF ( config_flags%polar .AND. (j == jds+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfux(i,j-1)*rdy   
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
          END DO
          END DO
        
        ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfux(i,j-1)*rdy   
            tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
          END DO
          END DO
        ELSE  

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfux(i,j-1)*rdy   
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF

        END IF


        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

   ENDDO j_loop_y_flux_6



      i_start = its
      i_end   = ite

      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)
        i_start_f = ids+3
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-1,ite)
        i_end_f = ide-2
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
          fqx( i,k ) = vel*flux6( u(i-3,k,j), u(i-2,k,j),  &
                                         u(i-1,k,j), u(i  ,k,j),  &
                                         u(i+1,k,j), u(i+2,k,j),  &
                                         vel                     )
        ENDDO
        ENDDO




        IF( degrade_xs ) THEN

          IF( i_start == ids+1 ) THEN 
            i = ids+1
            DO k=kts,ktf
              ub = u(i-1,k,j)
              IF (specified .AND. u(i,k,j) .LT. 0.)ub = u(i,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i,k,j)+ub)
            ENDDO
          END IF

          i = ids+2
          DO k=kts,ktf
            vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
            fqx( i, k  ) = vel*flux4( u(i-2,k,j), u(i-1,k,j),  &
                                           u(i  ,k,j), u(i+1,k,j),  &
                                           vel                     )
          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          IF( i_end == ide-1 ) THEN 
            i = ide
            DO k=kts,ktf
              ub = u(i,k,j)
              IF (specified .AND. u(i-1,k,j) .GT. 0.)ub = u(i-1,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i-1,k,j)+ub)
            ENDDO
          ENDIF

          DO k=kts,ktf
          i = ide-1
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
          fqx( i,k ) = vel*flux4( u(i-2,k,j), u(i-1,k,j),  &
                                         u(i  ,k,j), u(i+1,k,j),  &
                                         vel                     )
          ENDDO

        ENDIF



        DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msfux(i,j)*rdx 
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO

   ELSE IF( horz_order == 5 ) THEN













   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-4)                ) degrade_ye = .false.



      i_start = its
      i_end   = ite
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-1,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

      j_start = jts
      j_end   = MIN(jte,jde-1)




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)



     jp1 = 2
     jp0 = 1

     j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN  

        DO k=kts,ktf
        DO i = i_start, i_end
          vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
          fqy( i, k, jp1 ) = vel*flux5(               &
                  u(i,k,j-3), u(i,k,j-2), u(i,k,j-1),       &
                  u(i,k,j  ), u(i,k,j+1), u(i,k,j+2),  vel )
        ENDDO
        ENDDO



      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i-1,k,j))  &
                                     *(u(i,k,j)+u(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
              fqy( i, k, jp1 ) = vel*flux3(      &
                   u(i,k,j-2),u(i,k,j-1), u(i,k,j),u(i,k,j+1),vel )
            ENDDO
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i-1,k,j))    &
                     *(u(i,k,j)+u(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
              fqy( i, k, jp1 ) = vel*flux3(     &
                   u(i,k,j-2),u(i,k,j-1),    &
                   u(i,k,j),u(i,k,j+1),vel )
            ENDDO
            ENDDO

      END IF



        
        IF ( config_flags%polar .AND. (j == jds+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfux(i,j-1)*rdy   
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
          END DO
          END DO
        
        ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfux(i,j-1)*rdy   
            tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
          END DO
          END DO
        ELSE  

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfux(i,j-1)*rdy   
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF

        END IF


        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

   ENDDO j_loop_y_flux_5



      i_start = its
      i_end   = ite

      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)
        i_start_f = ids+3
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-1,ite)
        i_end_f = ide-2
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
          fqx( i,k ) = vel*flux5( u(i-3,k,j), u(i-2,k,j),  &
                                         u(i-1,k,j), u(i  ,k,j),  &
                                         u(i+1,k,j), u(i+2,k,j),  &
                                         vel                     )
        ENDDO
        ENDDO




        IF( degrade_xs ) THEN

          IF( i_start == ids+1 ) THEN 
            i = ids+1
            DO k=kts,ktf
              ub = u(i-1,k,j)
              IF (specified .AND. u(i,k,j) .LT. 0.)ub = u(i,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i,k,j)+ub)
            ENDDO
          END IF

          i = ids+2
          DO k=kts,ktf
            vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
            fqx( i, k  ) = vel*flux3( u(i-2,k,j), u(i-1,k,j),  &
                                           u(i  ,k,j), u(i+1,k,j),  &
                                           vel                     )
          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          IF( i_end == ide-1 ) THEN 
            i = ide
            DO k=kts,ktf
              ub = u(i,k,j)
              IF (specified .AND. u(i-1,k,j) .GT. 0.)ub = u(i-1,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i-1,k,j)+ub)
            ENDDO
          ENDIF

          DO k=kts,ktf
          i = ide-1
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
          fqx( i,k ) = vel*flux3( u(i-2,k,j), u(i-1,k,j),  &
                                         u(i  ,k,j), u(i+1,k,j),  &
                                         vel                     )
          ENDDO

        ENDIF



        DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msfux(i,j)*rdx 
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO

   ELSE IF( horz_order == 4 ) THEN








   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-1)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.



      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-1
        i_end_f = ide-1
      ENDIF



      DO j = j_start, j_end

        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
          fqx( i, k ) = vel*flux4( u(i-2,k,j), u(i-1,k,j),      &
                                   u(i  ,k,j), u(i+1,k,j), vel )
        ENDDO
        ENDDO




        IF( degrade_xs ) THEN
          i = i_start
          DO k=kts,ktf
              ub = u(i-1,k,j)
              IF (specified .AND. u(i,k,j) .LT. 0.)ub = u(i,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i,k,j)+ub)
          ENDDO
        ENDIF

        IF( degrade_xe ) THEN
          i = i_end+1
          DO k=kts,ktf
              ub = u(i,k,j)
              IF (specified .AND. u(i-1,k,j) .GT. 0.)ub = u(i-1,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i-1,k,j)+ub)
          ENDDO
        ENDIF



        DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msfux(i,j)*rdx 
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO



      i_start = its
      i_end   = ite
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-1,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

      j_start = jts
      j_end   = MIN(jte,jde-1)




      j_start_f = j_start
      j_end_f   = j_end+1


      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-2
        j_end_f = jde-2
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)



     jp1 = 2
     jp0 = 1

   DO j = j_start, j_end+1

     IF ( (j < j_start_f) .and. degrade_ys) THEN
       DO k = kts, ktf
       DO i = i_start, i_end
         fqy(i, k, jp1) = 0.25*(rv(i,k,j_start)+rv(i-1,k,j_start))  &
               *(u(i,k,j_start)+u(i,k,j_start-1))
       ENDDO
       ENDDO
     ELSE IF ((j > j_end_f) .and. degrade_ye) THEN
       DO k = kts, ktf
       DO i = i_start, i_end
         


         fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i-1,k,j))    &
                *(u(i,k,j)+u(i,k,j-1))
       ENDDO
       ENDDO
     ELSE

       DO k = kts, ktf
       DO i = i_start, i_end
         vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
         fqy( i, k, jp1 ) = vel*flux4( u(i,k,j-2), u(i,k,j-1),  &
                                       u(i,k,j  ), u(i,k,j+1),  &
                                            vel                )
       ENDDO
       ENDDO

     END IF



     
     IF ( config_flags%polar .AND. (j == jds+1) ) THEN
       DO k=kts,ktf
       DO i = i_start, i_end
         mrdy=msfux(i,j-1)*rdy   
         tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
       END DO
       END DO
       
     ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
       DO k=kts,ktf
       DO i = i_start, i_end
         mrdy=msfux(i,j-1)*rdy   
         tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
       END DO
       END DO
     ELSE  

     IF (j > j_start) THEN

       DO k=kts,ktf
       DO i = i_start, i_end
          mrdy=msfux(i,j-1)*rdy 
          tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
       ENDDO
       ENDDO

     END IF

     END IF

     jtmp = jp1
     jp1 = jp0
     jp0 = jtmp

  ENDDO

  ELSE IF ( horz_order == 3 ) THEN











   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-1)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.



      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-1
        i_end_f = ide-1
      ENDIF



      DO j = j_start, j_end

        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
          fqx( i, k ) = vel*flux3( u(i-2,k,j), u(i-1,k,j),      &
                                   u(i  ,k,j), u(i+1,k,j), vel )
        ENDDO
        ENDDO




        IF( degrade_xs ) THEN
          i = i_start
          DO k=kts,ktf
              ub = u(i-1,k,j)
              IF (specified .AND. u(i,k,j) .LT. 0.)ub = u(i,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i,k,j)+ub)
          ENDDO
        ENDIF

        IF( degrade_xe ) THEN
          i = i_end+1
          DO k=kts,ktf
              ub = u(i,k,j)
              IF (specified .AND. u(i-1,k,j) .GT. 0.)ub = u(i-1,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i-1,k,j)+ub)
          ENDDO
        ENDIF



        DO k=kts,ktf
          DO i = i_start, i_end
          mrdx=msfux(i,j)*rdx 
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO
      ENDDO



      i_start = its
      i_end   = ite
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-1,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

      j_start = jts
      j_end   = MIN(jte,jde-1)




      j_start_f = j_start
      j_end_f   = j_end+1


      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-2
        j_end_f = jde-2
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)



     jp1 = 2
     jp0 = 1

   DO j = j_start, j_end+1

     IF ( (j < j_start_f) .and. degrade_ys) THEN
       DO k = kts, ktf
       DO i = i_start, i_end
         fqy(i, k, jp1) = 0.25*(rv(i,k,j_start)+rv(i-1,k,j_start))  &
               *(u(i,k,j_start)+u(i,k,j_start-1))
       ENDDO
       ENDDO
     ELSE IF ((j > j_end_f) .and. degrade_ye) THEN
       DO k = kts, ktf
       DO i = i_start, i_end
         


         fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i-1,k,j))    &
                *(u(i,k,j)+u(i,k,j-1))
       ENDDO
       ENDDO
     ELSE

       DO k = kts, ktf
       DO i = i_start, i_end
         vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
         fqy( i, k, jp1 ) = vel*flux3( u(i,k,j-2), u(i,k,j-1),  &
                                       u(i,k,j  ), u(i,k,j+1),  &
                                            vel                )
       ENDDO
       ENDDO

     END IF



     
     IF ( config_flags%polar .AND. (j == jds+1) ) THEN
       DO k=kts,ktf
       DO i = i_start, i_end
         mrdy=msfux(i,j-1)*rdy   
         tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
       END DO
       END DO
       
     ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
       DO k=kts,ktf
       DO i = i_start, i_end
         mrdy=msfux(i,j-1)*rdy   
         tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
       END DO
       END DO
     ELSE  

     IF (j > j_start) THEN

       DO k=kts,ktf
       DO i = i_start, i_end
          mrdy=msfux(i,j-1)*rdy      
          tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
       ENDDO
       ENDDO

     END IF

     END IF

     jtmp = jp1
     jp1 = jp0
     jp0 = jtmp

  ENDDO

  ELSE IF ( horz_order == 2 ) THEN

      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = MIN(jte,jde-1)

      IF ( config_flags%open_xs ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe ) i_end   = MIN(ide-1,ite)
      IF ( specified ) i_start = MAX(ids+2,its)
      IF ( specified ) i_end   = MIN(ide-2,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end
         mrdx=msfux(i,j)*rdx         
         tendency(i,k,j)=tendency(i,k,j)-mrdx*0.25 &
                *((ru(i+1,k,j)+ru(i,k,j))*(u(i+1,k,j)+u(i,k,j)) &
                -(ru(i,k,j)+ru(i-1,k,j))*(u(i,k,j)+u(i-1,k,j)))
      ENDDO
      ENDDO
      ENDDO

      IF ( specified .AND. its .LE. ids+1 .AND. .NOT. config_flags%periodic_x ) THEN
        DO j = j_start, j_end
        DO k=kts,ktf
           i = ids+1
           mrdx=msfux(i,j)*rdx       
           ub = u(i-1,k,j)
           IF (u(i,k,j) .LT. 0.) ub = u(i,k,j)
           tendency(i,k,j)=tendency(i,k,j)-mrdx*0.25 &
                  *((ru(i+1,k,j)+ru(i,k,j))*(u(i+1,k,j)+u(i,k,j)) &
                  -(ru(i,k,j)+ru(i-1,k,j))*(u(i,k,j)+ub))
        ENDDO
        ENDDO
      ENDIF
      IF ( specified .AND. ite .GE. ide-1 .AND. .NOT. config_flags%periodic_x ) THEN
        DO j = j_start, j_end
        DO k=kts,ktf
           i = ide-1
           mrdx=msfux(i,j)*rdx       
           ub = u(i+1,k,j)
           IF (u(i,k,j) .GT. 0.) ub = u(i,k,j)
           tendency(i,k,j)=tendency(i,k,j)-mrdx*0.25 &
                  *((ru(i+1,k,j)+ru(i,k,j))*(ub+u(i,k,j)) &
                  -(ru(i,k,j)+ru(i-1,k,j))*(u(i,k,j)+u(i-1,k,j)))
        ENDDO
        ENDDO
      ENDIF

      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-2,jte)

      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end
         mrdy=msfux(i,j)*rdy         
         
         
         IF ( (config_flags%polar) .AND. (j == jds) ) THEN
            tendency(i,k,j)=tendency(i,k,j)-mrdy*0.25 &
                            *(rv(i,k,j+1)+rv(i-1,k,j+1))*(u(i,k,j+1)+u(i,k,j))
         ELSE IF ( (config_flags%polar) .AND. (j == jde-1) ) THEN
            tendency(i,k,j)=tendency(i,k,j)+mrdy*0.25 &
                           *(rv(i,k,j)+rv(i-1,k,j))*(u(i,k,j)+u(i,k,j-1))
         ELSE  
            tendency(i,k,j)=tendency(i,k,j)-mrdy*0.25 &
                *((rv(i,k,j+1)+rv(i-1,k,j+1))*(u(i,k,j+1)+u(i,k,j)) &
                 -(rv(i,k,j)+rv(i-1,k,j))*(u(i,k,j)+u(i,k,j-1)))
         ENDIF
      ENDDO
      ENDDO
      ENDDO

   ELSE IF ( horz_order == 0 ) THEN

      

   ELSE

      WRITE ( wrf_err_message , * ) 'module_advect: advect_u_6a:  h_order not known ',horz_order
      CALL wrf_error_fatal3("<stdin>",1134,&
TRIM( wrf_err_message ) )

   ENDIF horizontal_order_test



      IF ( (config_flags%open_xs) .and. its == ids ) THEN

        j_start = jts
        j_end   = MIN(jte,jde-1)

        DO j = j_start, j_end
        DO k = kts, ktf
          ub = MIN(ru(its,k,j)-cb*mut(its,j), 0.)
          tendency(its,k,j) = tendency(its,k,j)                    &
                      - rdx*ub*(u_old(its+1,k,j) - u_old(its,k,j))
        ENDDO
        ENDDO

      ENDIF

      IF ( (config_flags%open_xe) .and. ite == ide ) THEN

        j_start = jts
        j_end   = MIN(jte,jde-1)

        DO j = j_start, j_end
        DO k = kts, ktf
          ub = MAX(ru(ite,k,j)+cb*mut(ite-1,j), 0.)
          tendency(ite,k,j) = tendency(ite,k,j)                    &
                      - rdx*ub*(u_old(ite,k,j) - u_old(ite-1,k,j))
        ENDDO
        ENDDO

      ENDIF





      i_start = its
      i_end   = MIN(ite,ide)
      imin    = ids
      imax    = ide-1

      IF (config_flags%open_xs) THEN
        i_start = MAX(ids+1, its)
        imin = ids
      ENDIF
      IF (config_flags%open_xe) THEN
        i_end = MIN(ite,ide-1)
        imax = ide-1
      ENDIF

   IF( (config_flags%open_ys) .and. (jts == jds)) THEN

      DO i = i_start, i_end

         mrdy=msfux(i,jts)*rdy       
         ip = MIN( imax, i   )
         im = MAX( imin, i-1 )

         DO k=kts,ktf

          vw = 0.5*(rv(ip,k,jts)+rv(im,k,jts))
          vb = MIN( vw, 0. )
          dvm =  rv(ip,k,jts+1)-rv(ip,k,jts)
          dvp =  rv(im,k,jts+1)-rv(im,k,jts)
          tendency(i,k,jts)=tendency(i,k,jts)-mrdy*(                &
                            vb*(u_old(i,k,jts+1)-u_old(i,k,jts))    &
                           +0.5*u(i,k,jts)*(dvm+dvp))
         ENDDO
      ENDDO

   ENDIF

   IF( (config_flags%open_ye) .and. (jte == jde)) THEN

      DO i = i_start, i_end

         mrdy=msfux(i,jte-1)*rdy     
         ip = MIN( imax, i   )
         im = MAX( imin, i-1 )

         DO k=kts,ktf

          vw = 0.5*(rv(ip,k,jte)+rv(im,k,jte))
          vb = MAX( vw, 0. )
          dvm =  rv(ip,k,jte)-rv(ip,k,jte-1)
          dvp =  rv(im,k,jte)-rv(im,k,jte-1)
          tendency(i,k,jte-1)=tendency(i,k,jte-1)-mrdy*(              &
                              vb*(u_old(i,k,jte-1)-u_old(i,k,jte-2))  &
                             +0.5*u(i,k,jte-1)*(dvm+dvp))
         ENDDO
      ENDDO

   ENDIF







   i_start = its
   i_end   = ite
   j_start = jts
   j_end   = min(jte,jde-1)




   IF ( config_flags%open_ys .or. specified ) i_start = MAX(ids+1,its)
   IF ( config_flags%open_ye .or. specified ) i_end   = MIN(ide-1,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

   DO i = i_start, i_end
     vflux(i,kts)=0.
     vflux(i,kte)=0.
   ENDDO

   vert_order_test : IF (vert_order == 6) THEN    

      DO j = j_start, j_end

         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=0.5*(rom(i-1,k,j)+rom(i,k,j))
           vflux(i,k) = vel*flux6(                     &
                   u(i,k-3,j), u(i,k-2,j), u(i,k-1,j),       &
                   u(i,k  ,j), u(i,k+1,j), u(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j))  &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))
           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i-1,k,j)) 
           vflux(i,k) = vel*flux4(       &
                   u(i,k-2,j), u(i,k-1,j),   &
                   u(i,k  ,j), u(i,k+1,j), -vel )
           k = ktf-1
           vel=0.5*(rom(i,k,j)+rom(i-1,k,j)) 
           vflux(i,k) = vel*flux4(       &
                   u(i,k-2,j), u(i,k-1,j),   &
                   u(i,k  ,j), u(i,k+1,j), -vel )
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j)) &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))

         ENDDO
         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO
      ENDDO

    ELSE IF (vert_order == 5) THEN    

      DO j = j_start, j_end

         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=0.5*(rom(i-1,k,j)+rom(i,k,j))
           vflux(i,k) = vel*flux5(                     &
                   u(i,k-3,j), u(i,k-2,j), u(i,k-1,j),       &
                   u(i,k  ,j), u(i,k+1,j), u(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j))  &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))
           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i-1,k,j)) 
           vflux(i,k) = vel*flux3(       &
                   u(i,k-2,j), u(i,k-1,j),   &
                   u(i,k  ,j), u(i,k+1,j), -vel )
           k = ktf-1
           vel=0.5*(rom(i,k,j)+rom(i-1,k,j)) 
           vflux(i,k) = vel*flux3(       &
                   u(i,k-2,j), u(i,k-1,j),   &
                   u(i,k  ,j), u(i,k+1,j), -vel )
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j)) &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))

         ENDDO
         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO
      ENDDO

    ELSE IF (vert_order == 4) THEN    

      DO j = j_start, j_end

         DO k=kts+2,ktf-1
         DO i = i_start, i_end
           vel=0.5*(rom(i-1,k,j)+rom(i,k,j))
           vflux(i,k) = vel*flux4(               &
                   u(i,k-2,j), u(i,k-1,j),       &
                   u(i,k  ,j), u(i,k+1,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j))  &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j)) &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))

         ENDDO
         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO
      ENDDO

    ELSE IF (vert_order == 3) THEN    

      DO j = j_start, j_end

         DO k=kts+2,ktf-1
         DO i = i_start, i_end
           vel=0.5*(rom(i-1,k,j)+rom(i,k,j))
           vflux(i,k) = vel*flux3(               &
                   u(i,k-2,j), u(i,k-1,j),       &
                   u(i,k  ,j), u(i,k+1,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j))  &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j)) &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))

         ENDDO
         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO
      ENDDO

    ELSE IF (vert_order == 2) THEN    

      DO j = j_start, j_end
         DO k=kts+1,ktf
         DO i = i_start, i_end
               vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j)) &
                                *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))
         ENDDO
         ENDDO


         DO k=kts,ktf
         DO i = i_start, i_end
               tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO

   ELSE

      WRITE ( wrf_err_message , * ) 'module_advect: advect_u_6a: v_order not known ',vert_order
      CALL wrf_error_fatal3("<stdin>",1419,&
TRIM( wrf_err_message ) )

   ENDIF vert_order_test

END SUBROUTINE advect_u



SUBROUTINE advect_v   ( v, v_old, tendency,            &
                        ru, rv, rom,                   &
                        mut, time_step, config_flags,  &
                        msfux, msfuy, msfvx, msfvy,    &
                        msftx, msfty,                  &
                        fzm, fzp,                      &
                        rdx, rdy, rdzw,                &
                        ids, ide, jds, jde, kds, kde,  &
                        ims, ime, jms, jme, kms, kme,  &
                        its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: v,     &
                                                                      v_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy
   INTEGER ,                                     INTENT(IN   ) :: time_step


   
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax

   REAL    :: mrdx, mrdy, ub, vb, uw, vw, dup, dum
   REAL , DIMENSION(its:ite, kts:kte) :: vflux


   REAL,  DIMENSION( its:ite+1, kts:kte ) :: fqx
   REAL,  DIMENSION( its:ite, kts:kte, 2 ) :: fqy

   INTEGER :: horz_order
   INTEGER :: vert_order
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

   INTEGER :: jp1, jp0, jtmp




   REAL    :: flux3, flux4, flux5, flux6
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel

   flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
          ( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0

   flux3(q_im2, q_im1, q_i, q_ip1, ua) =                     &
           flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
           sign(1,time_step)*sign(1.,ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0

   flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
                      ( 37.*(q_i+q_im1) - 8.*(q_ip1+q_im2)   &
                     +(q_ip2+q_im3) )/60.0

   flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)    &
            -sign(1,time_step)*sign(1.,ua)*(                    &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )/60.0



   LOGICAL :: specified

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.



   ktf=MIN(kte,kde-1)
   horz_order = config_flags%h_mom_adv_order
   vert_order = config_flags%v_mom_adv_order





   horizontal_order_test : IF( horz_order == 6 ) THEN








   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.



      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-1)
        j_end_f = jde-2
      ENDIF



     jp1 = 2
     jp0 = 1

     j_loop_y_flux_6 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN

        DO k=kts,ktf
        DO i = i_start, i_end
          vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
          fqy( i, k, jp1 ) = vel*flux6(               &
                  v(i,k,j-3), v(i,k,j-2), v(i,k,j-1),       &
                  v(i,k,j  ), v(i,k,j+1), v(i,k,j+2),  vel )
        ENDDO
        ENDDO




      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end
                vb = v(i,k,j-1)
                IF (specified .AND. v(i,k,j) .LT. 0.)vb = v(i,k,j)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))  &
                                 *(v(i,k,j)+vb)
            ENDDO
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
              fqy( i, k, jp1 ) = vel*flux4(      &
                   v(i,k,j-2),v(i,k,j-1),v(i,k,j),v(i,k,j+1),vel )
            ENDDO
            ENDDO


     ELSE IF ( j == jde ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
                vb = v(i,k,j)
                IF (specified .AND. v(i,k,j-1) .GT. 0.)vb = v(i,k,j-1)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))    &
                                 *(vb+v(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
              fqy( i, k, jp1 ) = vel*flux4(     &
                   v(i,k,j-2),v(i,k,j-1),v(i,k,j),v(i,k,j+1),vel )
            ENDDO
            ENDDO

      END IF



        
        
        
        
        IF ( config_flags%polar .AND. (j == jds+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            tendency(i,k,j-1) = 0.
          END DO
          END DO
        
        
        
        
        ELSE IF( config_flags%polar .AND. (j == jde+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            tendency(i,k,j-1) = 0.
          END DO
          END DO
        ELSE  

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfvy(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF

        END IF

        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

   ENDDO j_loop_y_flux_6



      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = jte
      
      IF ( config_flags%open_ys .or. specified .or. config_flags%polar ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified .or. config_flags%polar ) j_end   = MIN(jde-1,jte)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)

        i_start_f = MIN(i_start+2,ids+3)
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
          fqx( i, k ) = vel*flux6( v(i-3,k,j), v(i-2,k,j),  &
                                         v(i-1,k,j), v(i  ,k,j),  &
                                         v(i+1,k,j), v(i+2,k,j),  &
                                         vel                     )
        ENDDO
        ENDDO



        IF( degrade_xs ) THEN

          DO i=i_start,i_start_f-1

            IF(i == ids+1) THEN 
              DO k=kts,ktf
                fqx(i,k) = 0.25*(ru(i,k,j)+ru(i,k,j-1)) &
                                *(v(i,k,j)+v(i-1,k,j))
              ENDDO
            ENDIF

            IF(i == ids+2) THEN  
              DO k=kts,ktf
                vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
                fqx( i,k ) = vel*flux4( v(i-2,k,j), v(i-1,k,j),  &
                                        v(i  ,k,j), v(i+1,k,j),  &
                                        vel                     )
              ENDDO
            ENDIF

          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          DO i = i_end_f+1, i_end+1

            IF( i == ide-1 ) THEN 
              DO k=kts,ktf
                fqx(i,k) = 0.25*(ru(i_end+1,k,j)+ru(i_end+1,k,j-1))      &
                                *(v(i_end+1,k,j)+v(i_end,k,j))
              ENDDO
            ENDIF

            IF( i == ide-2 ) THEN 
              DO k=kts,ktf
                vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
                fqx( i,k ) = vel*flux4( v(i-2,k,j), v(i-1,k,j),  &
                                        v(i  ,k,j), v(i+1,k,j),  &
                                        vel                     )
              ENDDO
            ENDIF

          ENDDO

        ENDIF



        DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msfvy(i,j)*rdx      
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO

   ELSE IF( horz_order == 5 ) THEN













   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.



      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-1)
        j_end_f = jde-2
      ENDIF



     jp1 = 2
     jp0 = 1

     j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN

        DO k=kts,ktf
        DO i = i_start, i_end
          vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
          fqy( i, k, jp1 ) = vel*flux5(               &
                  v(i,k,j-3), v(i,k,j-2), v(i,k,j-1),       &
                  v(i,k,j  ), v(i,k,j+1), v(i,k,j+2),  vel )
        ENDDO
        ENDDO




      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end
                vb = v(i,k,j-1)
                IF (specified .AND. v(i,k,j) .LT. 0.)vb = v(i,k,j)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))  &
                                 *(v(i,k,j)+vb)
            ENDDO
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
              fqy( i, k, jp1 ) = vel*flux3(      &
                   v(i,k,j-2),v(i,k,j-1),v(i,k,j),v(i,k,j+1),vel )
            ENDDO
            ENDDO


     ELSE IF ( j == jde ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
                vb = v(i,k,j)
                IF (specified .AND. v(i,k,j-1) .GT. 0.)vb = v(i,k,j-1)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))    &
                                 *(vb+v(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
              fqy( i, k, jp1 ) = vel*flux3(     &
                   v(i,k,j-2),v(i,k,j-1),v(i,k,j),v(i,k,j+1),vel )
            ENDDO
            ENDDO

      END IF



        
        
        
        
        IF ( config_flags%polar .AND. (j == jds+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            tendency(i,k,j-1) = 0.
          END DO
          END DO
        
        
        
        
        ELSE IF( config_flags%polar .AND. (j == jde+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            tendency(i,k,j-1) = 0.
          END DO
          END DO
        ELSE  

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfvy(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF

        END IF

        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

   ENDDO j_loop_y_flux_5



      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = jte
      
      IF ( config_flags%open_ys .or. specified .or. config_flags%polar ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified .or. config_flags%polar ) j_end   = MIN(jde-1,jte)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)

        i_start_f = MIN(i_start+2,ids+3)
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
          fqx( i, k ) = vel*flux5( v(i-3,k,j), v(i-2,k,j),  &
                                         v(i-1,k,j), v(i  ,k,j),  &
                                         v(i+1,k,j), v(i+2,k,j),  &
                                         vel                     )
        ENDDO
        ENDDO



        IF( degrade_xs ) THEN

          DO i=i_start,i_start_f-1

            IF(i == ids+1) THEN 
              DO k=kts,ktf
                fqx(i,k) = 0.25*(ru(i,k,j)+ru(i,k,j-1)) &
                                *(v(i,k,j)+v(i-1,k,j))
              ENDDO
            ENDIF

            IF(i == ids+2) THEN  
              DO k=kts,ktf
                vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
                fqx( i,k ) = vel*flux3( v(i-2,k,j), v(i-1,k,j),  &
                                        v(i  ,k,j), v(i+1,k,j),  &
                                        vel                     )
              ENDDO
            ENDIF

          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          DO i = i_end_f+1, i_end+1

            IF( i == ide-1 ) THEN 
              DO k=kts,ktf
                fqx(i,k) = 0.25*(ru(i_end+1,k,j)+ru(i_end+1,k,j-1))      &
                                *(v(i_end+1,k,j)+v(i_end,k,j))
              ENDDO
            ENDIF

            IF( i == ide-2 ) THEN 
              DO k=kts,ktf
                vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
                fqx( i,k ) = vel*flux3( v(i-2,k,j), v(i-1,k,j),  &
                                        v(i  ,k,j), v(i+1,k,j),  &
                                        vel                     )
              ENDDO
            ENDIF

          ENDDO

        ENDIF



        DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msfvy(i,j)*rdx      
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO

   ELSE IF( horz_order == 4 ) THEN








   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-2)                ) degrade_ye = .false.




   ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte




      j_start_f = j_start
      j_end_f   = j_end+1


      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-1
        j_end_f = jde-1
      ENDIF




    jp0 = 1
    jp1 = 2

    DO j = j_start, j_end+1

      IF ((j == j_start) .and. degrade_ys) THEN
        DO k = kts,ktf
        DO i = i_start, i_end
                vb = v(i,k,j-1)
                IF (specified .AND. v(i,k,j) .LT. 0.)vb = v(i,k,j)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))  &
                                 *(v(i,k,j)+vb)
        ENDDO
        ENDDO
      ELSE IF ((j == j_end+1) .and. degrade_ye) THEN
        DO k = kts, ktf
        DO i = i_start, i_end
                vb = v(i,k,j)
                IF (specified .AND. v(i,k,j-1) .GT. 0.)vb = v(i,k,j-1)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))    &
                                 *(vb+v(i,k,j-1))
        ENDDO
        ENDDO
      ELSE
        DO k = kts, ktf
        DO i = i_start, i_end
          vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
          fqy( i,k,jp1 ) = vel*flux4( v(i,k,j-2), v(i,k,j-1),  &
                                     v(i,k,j  ), v(i,k,j+1),  &
                                      vel                        )
        ENDDO
        ENDDO
      END IF

      
      
      
      
      IF ( config_flags%polar .AND. (j == jds+1) ) THEN
        DO k=kts,ktf
        DO i = i_start, i_end
          tendency(i,k,j-1) = 0.
        END DO
        END DO
      
      
      
      
      ELSE IF( config_flags%polar .AND. (j == jde+1) ) THEN
        DO k=kts,ktf
        DO i = i_start, i_end
          tendency(i,k,j-1) = 0.
        END DO
        END DO
      ELSE  

      IF( j > j_start) THEN
        DO k = kts, ktf
        DO i = i_start, i_end
            mrdy=msfvy(i,j-1)*rdy     
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
        ENDDO
        ENDDO

      END IF

      END IF

      jtmp = jp1
      jp1 = jp0
      jp0 = jtmp

   ENDDO




      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = jte
      
      IF ( config_flags%open_ys .or. specified .or. config_flags%polar ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified .or. config_flags%polar ) j_end   = MIN(jde-1,jte)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-2
        i_end_f = ide-2
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
          fqx( i, k ) = vel*flux4( v(i-2,k,j), v(i-1,k,j),  &
                                  v(i  ,k,j), v(i+1,k,j),  &
                                  vel                     )
        ENDDO
        ENDDO



        IF( degrade_xs ) THEN
          DO k=kts,ktf
            fqx(i_start,k) = 0.25*(ru(i_start,k,j)+ru(i_start,k,j-1)) &
                   *(v(i_start,k,j)+v(i_start-1,k,j))
          ENDDO
        ENDIF

        IF( degrade_xe ) THEN
          DO k=kts,ktf
            fqx(i_end+1,k) = 0.25*(ru(i_end+1,k,j)+ru(i_end+1,k,j-1))      &
                   *(v(i_end+1,k,j)+v(i_end,k,j))
          ENDDO
        ENDIF



        DO k=kts,ktf
        DO i = i_start, i_end
            mrdx=msfvy(i,j)*rdx       
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
        ENDDO
        ENDDO

      ENDDO

   ELSE IF( horz_order == 3 ) THEN








   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-2)                ) degrade_ye = .false.




   ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte




      j_start_f = j_start
      j_end_f   = j_end+1


      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-1
        j_end_f = jde-1
      ENDIF




    jp0 = 1
    jp1 = 2

    DO j = j_start, j_end+1

      IF ((j == j_start) .and. degrade_ys) THEN
        DO k = kts,ktf
        DO i = i_start, i_end
                vb = v(i,k,j-1)
                IF (specified .AND. v(i,k,j) .LT. 0.)vb = v(i,k,j)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))  &
                                 *(v(i,k,j)+vb)
        ENDDO
        ENDDO
      ELSE IF ((j == j_end+1) .and. degrade_ye) THEN
        DO k = kts, ktf
        DO i = i_start, i_end
                vb = v(i,k,j)
                IF (specified .AND. v(i,k,j-1) .GT. 0.)vb = v(i,k,j-1)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))    &
                                 *(vb+v(i,k,j-1))
        ENDDO
        ENDDO
      ELSE
        DO k = kts, ktf
        DO i = i_start, i_end
          vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
          fqy( i,k,jp1 ) = vel*flux3( v(i,k,j-2), v(i,k,j-1),  &
                                     v(i,k,j  ), v(i,k,j+1),  &
                                      vel                        )
        ENDDO
        ENDDO
      END IF

      
      
      
      
      IF ( config_flags%polar .AND. (j == jds+1) ) THEN
        DO k=kts,ktf
        DO i = i_start, i_end
          tendency(i,k,j-1) = 0.
        END DO
        END DO
      
      
      
      
      ELSE IF( config_flags%polar .AND. (j == jde+1) ) THEN
        DO k=kts,ktf
        DO i = i_start, i_end
          tendency(i,k,j-1) = 0.
        END DO
        END DO
      ELSE  

      IF( j > j_start) THEN
        DO k = kts, ktf
        DO i = i_start, i_end
            mrdy=msfvy(i,j-1)*rdy     
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
        ENDDO
        ENDDO

      END IF

      END IF

      jtmp = jp1
      jp1 = jp0
      jp0 = jtmp

   ENDDO




      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = jte
      
      IF ( config_flags%open_ys .or. specified .or. config_flags%polar ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified .or. config_flags%polar ) j_end   = MIN(jde-1,jte)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-2
        i_end_f = ide-2
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
          fqx( i, k ) = vel*flux3( v(i-2,k,j), v(i-1,k,j),  &
                                  v(i  ,k,j), v(i+1,k,j),  &
                                  vel                     )
        ENDDO
        ENDDO



        IF( degrade_xs ) THEN
          DO k=kts,ktf
            fqx(i_start,k) = 0.25*(ru(i_start,k,j)+ru(i_start,k,j-1)) &
                   *(v(i_start,k,j)+v(i_start-1,k,j))
          ENDDO
        ENDIF

        IF( degrade_xe ) THEN
          DO k=kts,ktf
            fqx(i_end+1,k) = 0.25*(ru(i_end+1,k,j)+ru(i_end+1,k,j-1))      &
                   *(v(i_end+1,k,j)+v(i_end,k,j))
          ENDDO
        ENDIF



        DO k=kts,ktf
        DO i = i_start, i_end
            mrdx=msfvy(i,j)*rdx      
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
        ENDDO
        ENDDO

      ENDDO

   ELSE IF( horz_order == 2 ) THEN


      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte

      IF ( config_flags%open_ys ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye ) j_end   = MIN(jde-1,jte)
      IF ( specified ) j_start = MAX(jds+2,jts)
      IF ( specified ) j_end   = MIN(jde-2,jte)
      IF ( config_flags%polar ) j_start = MAX(jds+1,jts)
      IF ( config_flags%polar ) j_end   = MIN(jde-1,jte)

      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end

         mrdy=msfvy(i,j)*rdy          

            tendency(i,k,j)=tendency(i,k,j) -mrdy*0.25 &
                            *((rv(i,k,j+1)+rv(i,k,j  ))*(v(i,k,j+1)+v(i,k,j  )) &
                             -(rv(i,k,j  )+rv(i,k,j-1))*(v(i,k,j  )+v(i,k,j-1)))

      ENDDO
      ENDDO
      ENDDO

      
      
      
      IF (config_flags%polar) THEN
         IF (jts == jds) THEN
            DO k=kts,ktf
            DO i = i_start, i_end
               tendency(i,k,jds) = 0.
            END DO
            END DO
         END IF
         IF (jte == jde) THEN
            DO k=kts,ktf
            DO i = i_start, i_end
               tendency(i,k,jde) = 0.
            END DO
            END DO
         END IF
      END IF



      IF ( specified .AND. jts .LE. jds+1 ) THEN
        j = jds+1
        DO k=kts,ktf
        DO i = i_start, i_end
           mrdy=msfvy(i,j)*rdy       
           vb = v(i,k,j-1)
           IF (v(i,k,j) .LT. 0.) vb = v(i,k,j)

              tendency(i,k,j)=tendency(i,k,j) -mrdy*0.25 &
                              *((rv(i,k,j+1)+rv(i,k,j  ))*(v(i,k,j+1)+v(i,k,j  )) &
                               -(rv(i,k,j  )+rv(i,k,j-1))*(v(i,k,j  )+vb))

        ENDDO
        ENDDO
      ENDIF

      IF ( specified .AND. jte .GE. jde-1 ) THEN
        j = jde-1
        DO k=kts,ktf
        DO i = i_start, i_end

           mrdy=msfvy(i,j)*rdy       
           vb = v(i,k,j+1)
           IF (v(i,k,j) .GT. 0.) vb = v(i,k,j)

              tendency(i,k,j)=tendency(i,k,j) -mrdy*0.25 &
                              *((rv(i,k,j+1)+rv(i,k,j  ))*(vb+v(i,k,j  )) &
                               -(rv(i,k,j  )+rv(i,k,j-1))*(v(i,k,j  )+v(i,k,j-1)))

        ENDDO
        ENDDO
      ENDIF

      IF ( .NOT. config_flags%periodic_x ) THEN
        IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
        IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-2,ite)
      ENDIF
      IF ( config_flags%polar ) j_start = MAX(jds+1,jts)
      IF ( config_flags%polar ) j_end   = MIN(jde-1,jte)

      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end

         mrdx=msfvy(i,j)*rdx         

            tendency(i,k,j)=tendency(i,k,j)-mrdx*0.25 &
                            *((ru(i+1,k,j)+ru(i+1,k,j-1))*(v(i+1,k,j)+v(i  ,k,j)) &
                             -(ru(i  ,k,j)+ru(i  ,k,j-1))*(v(i  ,k,j)+v(i-1,k,j)))

      ENDDO
      ENDDO
      ENDDO

   ELSE IF ( horz_order == 0 ) THEN

      

  ELSE


      WRITE ( wrf_err_message , * ) 'module_advect: advect_v_6a: h_order not known ',horz_order
      CALL wrf_error_fatal3("<stdin>",2583,&
TRIM( wrf_err_message ) )

   ENDIF horizontal_order_test

   
   
   
   IF ( config_flags%polar .AND. (jts == jds) ) THEN
      DO i=its,ite
      DO k=kts,ktf
         tendency(i,k,jts)=0.
      END DO
      END DO
   END IF
   IF ( config_flags%polar .AND. (jte == jde) ) THEN
      DO i=its,ite
      DO k=kts,ktf
         tendency(i,k,jte)=0.
      END DO
      END DO
   END IF



      IF ( (config_flags%open_ys) .and. jts == jds ) THEN

        i_start = its
        i_end   = MIN(ite,ide-1)

        DO i = i_start, i_end
        DO k = kts, ktf
          vb = MIN(rv(i,k,jts)-cb*mut(i,jts), 0.)
          tendency(i,k,jts) = tendency(i,k,jts)                    &
                      - rdy*vb*(v_old(i,k,jts+1) - v_old(i,k,jts))
        ENDDO
        ENDDO

      ENDIF

      IF ( (config_flags%open_ye) .and. jte == jde ) THEN

        i_start = its
        i_end   = MIN(ite,ide-1)

        DO i = i_start, i_end
        DO k = kts, ktf
          vb = MAX(rv(i,k,jte)+cb*mut(i,jte-1), 0.)
          tendency(i,k,jte) = tendency(i,k,jte)                    &
                      - rdy*vb*(v_old(i,k,jte) - v_old(i,k,jte-1))
        ENDDO
        ENDDO

      ENDIF





      j_start = jts
      j_end   = MIN(jte,jde)

      jmin    = jds
      jmax    = jde-1

      IF (config_flags%open_ys) THEN
          j_start = MAX(jds+1, jts)
          jmin = jds
      ENDIF
      IF (config_flags%open_ye) THEN
          j_end = MIN(jte,jde-1)
          jmax = jde-1
      ENDIF



   IF( (config_flags%open_xs) .and. (its == ids)) THEN

      DO j = j_start, j_end

         mrdx=msfvy(its,j)*rdx       
         jp = MIN( jmax, j   )
         jm = MAX( jmin, j-1 )

         DO k=kts,ktf

          uw = 0.5*(ru(its,k,jp)+ru(its,k,jm))
          ub = MIN( uw, 0. )
          dup =  ru(its+1,k,jp)-ru(its,k,jp)
          dum =  ru(its+1,k,jm)-ru(its,k,jm)
          tendency(its,k,j)=tendency(its,k,j)-mrdx*(               &
                            ub*(v_old(its+1,k,j)-v_old(its,k,j))   &
                           +0.5*v(its,k,j)*(dup+dum))
         ENDDO
      ENDDO

   ENDIF

   IF( (config_flags%open_xe) .and. (ite == ide) ) THEN
      DO j = j_start, j_end

         mrdx=msfvy(ite-1,j)*rdx     
         jp = MIN( jmax, j   )
         jm = MAX( jmin, j-1 )

         DO k=kts,ktf

          uw = 0.5*(ru(ite,k,jp)+ru(ite,k,jm))
          ub = MAX( uw, 0. )
          dup = ru(ite,k,jp)-ru(ite-1,k,jp)
          dum = ru(ite,k,jm)-ru(ite-1,k,jm)






          tendency(ite-1,k,j)=tendency(ite-1,k,j)-mrdx*(              &
                            ub*(v_old(ite-1,k,j)-v_old(ite-2,k,j))    &
                           +0.5*v(ite-1,k,j)*(dup+dum))

         ENDDO
      ENDDO

   ENDIF









      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte

      DO i = i_start, i_end
         vflux(i,kts)=0.
         vflux(i,kte)=0.
      ENDDO

      
      
      IF ( config_flags%open_ys .or. specified .or. config_flags%polar ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified .or. config_flags%polar ) j_end   = MIN(jde-1,jte)

    vert_order_test : IF (vert_order == 6) THEN    

      DO j = j_start, j_end


         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1))
           vflux(i,k) = vel*flux6(                       &
                   v(i,k-3,j), v(i,k-2,j), v(i,k-1,j),       &
                   v(i,k  ,j), v(i,k+1,j), v(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end
           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1))  &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))
           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1)) 
           vflux(i,k) = vel*flux4(       &
                   v(i,k-2,j), v(i,k-1,j),   &
                   v(i,k  ,j), v(i,k+1,j), -vel )
           k = ktf-1
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1)) 
           vflux(i,k) = vel*flux4(       &
                   v(i,k-2,j), v(i,k-1,j),   &
                   v(i,k  ,j), v(i,k+1,j), -vel )
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1)) &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))

         ENDDO


         DO k=kts,ktf
         DO i = i_start, i_end
            
            
            tendency(i,k,j)=tendency(i,k,j)-(msfvy(i,j)/msfvx(i,j))*rdzw(k)*(vflux(i,k+1)-vflux(i,k)) 
         ENDDO
         ENDDO

      ENDDO

   ELSE IF (vert_order == 5) THEN    

      DO j = j_start, j_end


         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1))
           vflux(i,k) = vel*flux5(                       &
                   v(i,k-3,j), v(i,k-2,j), v(i,k-1,j),       &
                   v(i,k  ,j), v(i,k+1,j), v(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end
           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1))  &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))
           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1)) 
           vflux(i,k) = vel*flux3(       &
                   v(i,k-2,j), v(i,k-1,j),   &
                   v(i,k  ,j), v(i,k+1,j), -vel )
           k = ktf-1
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1)) 
           vflux(i,k) = vel*flux3(       &
                   v(i,k-2,j), v(i,k-1,j),   &
                   v(i,k  ,j), v(i,k+1,j), -vel )
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1)) &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))

         ENDDO


         DO k=kts,ktf
         DO i = i_start, i_end
            
            
            tendency(i,k,j)=tendency(i,k,j)-(msfvy(i,j)/msfvx(i,j))*rdzw(k)*(vflux(i,k+1)-vflux(i,k)) 
         ENDDO
         ENDDO

      ENDDO

    ELSE IF (vert_order == 4) THEN    

      DO j = j_start, j_end


         DO k=kts+2,ktf-1
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1))
           vflux(i,k) = vel*flux4(               &
                   v(i,k-2,j), v(i,k-1,j),       &
                   v(i,k  ,j), v(i,k+1,j), -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end
           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1))  &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1)) &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))

         ENDDO


         DO k=kts,ktf
         DO i = i_start, i_end
            
            
            tendency(i,k,j)=tendency(i,k,j)-(msfvy(i,j)/msfvx(i,j))*rdzw(k)*(vflux(i,k+1)-vflux(i,k)) 
         ENDDO
         ENDDO

      ENDDO

    ELSE IF (vert_order == 3) THEN    

      DO j = j_start, j_end


         DO k=kts+2,ktf-1
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1))
           vflux(i,k) = vel*flux3(               &
                   v(i,k-2,j), v(i,k-1,j),       &
                   v(i,k  ,j), v(i,k+1,j), -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end
           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1))  &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1)) &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))

         ENDDO


         DO k=kts,ktf
         DO i = i_start, i_end
            
            
            tendency(i,k,j)=tendency(i,k,j)-(msfvy(i,j)/msfvx(i,j))*rdzw(k)*(vflux(i,k+1)-vflux(i,k)) 
         ENDDO
         ENDDO

      ENDDO


    ELSE IF (vert_order == 2) THEN    

   DO j = j_start, j_end
      DO k=kts+1,ktf
      DO i = i_start, i_end

            vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1)) &
                                    *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))
      ENDDO
      ENDDO

      DO k=kts,ktf
      DO i = i_start, i_end
            
            
            tendency(i,k,j)=tendency(i,k,j)-(msfvy(i,j)/msfvx(i,j))*rdzw(k)*(vflux(i,k+1)-vflux(i,k)) 
      ENDDO
      ENDDO
   ENDDO

   ELSE

      WRITE ( wrf_err_message , * ) 'module_advect: advect_v_6a: v_order not known ',vert_order
      CALL wrf_error_fatal3("<stdin>",2916,&
TRIM( wrf_err_message ) )

   ENDIF vert_order_test

END SUBROUTINE advect_v



SUBROUTINE advect_scalar   ( field, field_old, tendency,    &
                             ru, rv, rom,                   &
                             mut, time_step, config_flags,  &
                             msfux, msfuy, msfvx, msfvy,    &
                             msftx, msfty,                  &
                             fzm, fzp,                      &
                             rdx, rdy, rdzw,                &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: field,     &
                                                                      field_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy
   INTEGER ,                                     INTENT(IN   ) :: time_step


   
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax

   REAL    :: mrdx, mrdy, ub, vb, uw, vw
   REAL , DIMENSION(its:ite, kts:kte) :: vflux


   REAL,  DIMENSION( its:ite+1, kts:kte  ) :: fqx
   REAL,  DIMENSION( its:ite, kts:kte, 2 ) :: fqy

   INTEGER :: horz_order, vert_order
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

   INTEGER :: jp1, jp0, jtmp




   REAL    :: flux3, flux4, flux5, flux6
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel

      flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
          ( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0

      flux3(q_im2, q_im1, q_i, q_ip1, ua) =                     &
           flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
           sign(1,time_step)*sign(1.,ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0

      flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
          ( 37.*(q_i+q_im1) - 8.*(q_ip1+q_im2)                  &
            +(q_ip2+q_im3) )/60.0

      flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)    &
            -sign(1,time_step)*sign(1.,ua)*(                    &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )/60.0


   LOGICAL :: specified

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.



  ktf=MIN(kte,kde-1)
  horz_order = config_flags%h_sca_adv_order
  vert_order = config_flags%v_sca_adv_order





  horizontal_order_test : IF( horz_order == 6 ) THEN








   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-4)                ) degrade_ye = .false.



      ktf=MIN(kte,kde-1)
      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)



     jp1 = 2
     jp0 = 1

     j_loop_y_flux_6 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN 

        DO k=kts,ktf
        DO i = i_start, i_end
          vel = rv(i,k,j)
          fqy( i, k, jp1 ) = vel*flux6(                                &
                  field(i,k,j-3), field(i,k,j-2), field(i,k,j-1),       &
                  field(i,k,j  ), field(i,k,j+1), field(i,k,j+2),  vel )
        ENDDO
        ENDDO


      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i,k, jp1) = 0.5*rv(i,k,j)*          &
                     (field(i,k,j)+field(i,k,j-1))

            ENDDO
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = rv(i,k,j)
              fqy( i, k, jp1 ) = vel*flux4(              &
                   field(i,k,j-2),field(i,k,j-1),field(i,k,j),field(i,k,j+1),vel )
            ENDDO
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*rv(i,k,j)*      &
                     (field(i,k,j)+field(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = rv(i,k,j)
              fqy( i, k, jp1) = vel*flux4(             &
                   field(i,k,j-2),field(i,k,j-1),    &
                   field(i,k,j),field(i,k,j+1),vel )
            ENDDO
            ENDDO

     ENDIF



        
        
        
        
        IF ( config_flags%polar .AND. (j == jds+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy     
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
          END DO
          END DO
        ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy     
            tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
          END DO
          END DO
        ELSE  

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF

        END IF

        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

      ENDDO j_loop_y_flux_6



      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)

        i_start_f = MIN(i_start+2,ids+3)
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = ru(i,k,j)
          fqx( i,k ) = vel*flux6( field(i-3,k,j), field(i-2,k,j),  &
                                         field(i-1,k,j), field(i  ,k,j),  &
                                         field(i+1,k,j), field(i+2,k,j),  &
                                         vel                             )
        ENDDO
        ENDDO



        IF( degrade_xs ) THEN

          DO i=i_start,i_start_f-1

            IF(i == ids+1) THEN 
              DO k=kts,ktf
                fqx(i,k) = 0.5*(ru(i,k,j)) &
                       *(field(i,k,j)+field(i-1,k,j))
              ENDDO
            ENDIF

            IF(i == ids+2) THEN  
              DO k=kts,ktf
                vel = ru(i,k,j)
                fqx( i,k ) = vel*flux4( field(i-2,k,j), field(i-1,k,j),  &
                                              field(i  ,k,j), field(i+1,k,j),  &
                                              vel                     )
              ENDDO
            END IF

          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          DO i = i_end_f+1, i_end+1

            IF( i == ide-1 ) THEN 
              DO k=kts,ktf
                fqx(i,k) = 0.5*(ru(i,k,j))      &
                       *(field(i,k,j)+field(i-1,k,j))
              ENDDO
           ENDIF

           IF( i == ide-2 ) THEN 
             DO k=kts,ktf
               vel = ru(i,k,j)
               fqx( i,k ) = vel*flux4( field(i-2,k,j), field(i-1,k,j),  &
                                       field(i  ,k,j), field(i+1,k,j),  &
                                       vel                             )
             ENDDO
           ENDIF

         ENDDO

       ENDIF



          DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msftx(i,j)*rdx      
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
          ENDDO

      ENDDO

  ELSE IF( horz_order == 5 ) THEN








   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-4)                ) degrade_ye = .false.



      ktf=MIN(kte,kde-1)
      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)



     jp1 = 2
     jp0 = 1

     j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN 

        DO k=kts,ktf
        DO i = i_start, i_end
          vel = rv(i,k,j)
          fqy( i, k, jp1 ) = vel*flux5(                                &
                  field(i,k,j-3), field(i,k,j-2), field(i,k,j-1),       &
                  field(i,k,j  ), field(i,k,j+1), field(i,k,j+2),  vel )
        ENDDO
        ENDDO


      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i,k, jp1) = 0.5*rv(i,k,j)*          &
                     (field(i,k,j)+field(i,k,j-1))

            ENDDO
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = rv(i,k,j)
              fqy( i, k, jp1 ) = vel*flux3(              &
                   field(i,k,j-2),field(i,k,j-1),field(i,k,j),field(i,k,j+1),vel )
            ENDDO
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*rv(i,k,j)*      &
                     (field(i,k,j)+field(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = rv(i,k,j)
              fqy( i, k, jp1) = vel*flux3(             &
                   field(i,k,j-2),field(i,k,j-1),    &
                   field(i,k,j),field(i,k,j+1),vel )
            ENDDO
            ENDDO

     ENDIF



        
        
        
        
        IF ( config_flags%polar .AND. (j == jds+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy     
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
          END DO
          END DO
        ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy     
            tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
          END DO
          END DO
        ELSE  

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF

        END IF

        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

      ENDDO j_loop_y_flux_5



      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)

        i_start_f = MIN(i_start+2,ids+3)
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = ru(i,k,j)
          fqx( i,k ) = vel*flux5( field(i-3,k,j), field(i-2,k,j),  &
                                         field(i-1,k,j), field(i  ,k,j),  &
                                         field(i+1,k,j), field(i+2,k,j),  &
                                         vel                             )
        ENDDO
        ENDDO



        IF( degrade_xs ) THEN

          DO i=i_start,i_start_f-1

            IF(i == ids+1) THEN 
              DO k=kts,ktf
                fqx(i,k) = 0.5*(ru(i,k,j)) &
                       *(field(i,k,j)+field(i-1,k,j))
              ENDDO
            ENDIF

            IF(i == ids+2) THEN  
              DO k=kts,ktf
                vel = ru(i,k,j)
                fqx( i,k ) = vel*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                              field(i  ,k,j), field(i+1,k,j),  &
                                              vel                     )
              ENDDO
            END IF

          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          DO i = i_end_f+1, i_end+1

            IF( i == ide-1 ) THEN 
              DO k=kts,ktf
                fqx(i,k) = 0.5*(ru(i,k,j))      &
                       *(field(i,k,j)+field(i-1,k,j))
              ENDDO
           ENDIF

           IF( i == ide-2 ) THEN 
             DO k=kts,ktf
               vel = ru(i,k,j)
               fqx( i,k ) = vel*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                       field(i  ,k,j), field(i+1,k,j),  &
                                       vel                             )
             ENDDO
           ENDIF

         ENDDO

       ENDIF



          DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msftx(i,j)*rdx      
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
          ENDDO

      ENDDO


   ELSE IF( horz_order == 4 ) THEN

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.




   ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-2
        i_end_f = ide-2
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f

          fqx( i, k) = ru(i,k,j)*flux4( field(i-2,k,j), field(i-1,k,j),  &
                                        field(i  ,k,j), field(i+1,k,j),  &
                                        ru(i,k,j)                       )
        ENDDO
        ENDDO



        IF( degrade_xs ) THEN
          DO k=kts,ktf
            fqx(i_start, k) = 0.5*ru(i_start,k,j)             &
                   *(field(i_start,k,j)+field(i_start-1,k,j))
          ENDDO
        ENDIF

        IF( degrade_xe ) THEN
          DO k=kts,ktf
            fqx(i_end+1,k ) = 0.5*ru(i_end+1,k,j)          &
                   *(field(i_end+1,k,j)+field(i_end,k,j))
          ENDDO
        ENDIF



        DO k=kts,ktf
        DO i = i_start, i_end
          mrdx=msftx(i,j)*rdx        
          tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
        ENDDO
        ENDDO

      ENDDO




      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-2
        j_end_f = jde-2
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)

    jp1 = 2
    jp0 = 1

  DO j = j_start, j_end+1

    IF ((j < j_start_f) .and. degrade_ys) THEN
      DO k = kts, ktf
      DO i = i_start, i_end
         fqy(i,k,jp1) = 0.5*rv(i,k,j_start)             &
                *(field(i,k,j_start)+field(i,k,j_start-1))
      ENDDO
      ENDDO
    ELSE IF ((j > j_end_f) .and. degrade_ye) THEN
      DO k = kts, ktf
      DO i = i_start, i_end
         


         fqy(i,k,jp1) = 0.5*rv(i,k,j)          &
                *(field(i,k,j)+field(i,k,j-1))
      ENDDO
      ENDDO
    ELSE

      DO k = kts, ktf
      DO i = i_start, i_end
         fqy( i, k, jp1 ) = rv(i,k,j)*flux4( field(i,k,j-2), field(i,k,j-1),  &
                                            field(i,k,j  ), field(i,k,j+1),  &
                                            rv(i,k,j)                       )
      ENDDO
      ENDDO
    END IF



    
    
    
    
    IF ( config_flags%polar .AND. (j == jds+1) ) THEN
      DO k=kts,ktf
      DO i = i_start, i_end
        mrdy=msftx(i,j-1)*rdy     
        tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
      END DO
      END DO
    ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
      DO k=kts,ktf
      DO i = i_start, i_end
        mrdy=msftx(i,j-1)*rdy     
        tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
      END DO
      END DO
    ELSE  

    IF ( j > j_start ) THEN

      DO k=kts,ktf
      DO i = i_start, i_end
        mrdy=msftx(i,j-1)*rdy        
        tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
      ENDDO
      ENDDO

    END IF

    END IF

    jtmp = jp1
    jp1 = jp0
    jp0 = jtmp

  ENDDO


   ELSE IF( horz_order == 3 ) THEN

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.




   ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-2
        i_end_f = ide-2
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f

          fqx( i, k) = ru(i,k,j)*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                        field(i  ,k,j), field(i+1,k,j),  &
                                        ru(i,k,j)                       )
        ENDDO
        ENDDO



        IF( degrade_xs ) THEN
          DO k=kts,ktf
            fqx(i_start, k) = 0.5*ru(i_start,k,j)             &
                   *(field(i_start,k,j)+field(i_start-1,k,j))
          ENDDO
        ENDIF

        IF( degrade_xe ) THEN
          DO k=kts,ktf
            fqx(i_end+1,k ) = 0.5*ru(i_end+1,k,j)          &
                   *(field(i_end+1,k,j)+field(i_end,k,j))
          ENDDO
        ENDIF



        DO k=kts,ktf
        DO i = i_start, i_end
          mrdx=msftx(i,j)*rdx        
          tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
        ENDDO
        ENDDO

      ENDDO




      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-2
        j_end_f = jde-2
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)

    jp1 = 2
    jp0 = 1

  DO j = j_start, j_end+1

    IF ((j < j_start_f) .and. degrade_ys) THEN
      DO k = kts, ktf
      DO i = i_start, i_end
         fqy(i,k,jp1) = 0.5*rv(i,k,j_start)             &
                *(field(i,k,j_start)+field(i,k,j_start-1))
      ENDDO
      ENDDO
    ELSE IF ((j > j_end_f) .and. degrade_ye) THEN
      DO k = kts, ktf
      DO i = i_start, i_end
         


         fqy(i,k,jp1) = 0.5*rv(i,k,j)          &
                *(field(i,k,j)+field(i,k,j-1))
      ENDDO
      ENDDO
    ELSE

      DO k = kts, ktf
      DO i = i_start, i_end
         fqy( i, k, jp1 ) = rv(i,k,j)*flux3( field(i,k,j-2), field(i,k,j-1),  &
                                            field(i,k,j  ), field(i,k,j+1),  &
                                            rv(i,k,j)                       )
      ENDDO
      ENDDO
    END IF



    
    
    
    
    IF ( config_flags%polar .AND. (j == jds+1) ) THEN
      DO k=kts,ktf
      DO i = i_start, i_end
        mrdy=msftx(i,j-1)*rdy     
        tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
      END DO
      END DO
    ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
      DO k=kts,ktf
      DO i = i_start, i_end
        mrdy=msftx(i,j-1)*rdy     
        tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
      END DO
      END DO
    ELSE  

    IF ( j > j_start ) THEN

      DO k=kts,ktf
      DO i = i_start, i_end
        mrdy=msftx(i,j-1)*rdy        
        tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
      ENDDO
      ENDDO

    END IF

    END IF

    jtmp = jp1
    jp1 = jp0
    jp0 = jtmp

  ENDDO

   ELSE IF( horz_order == 2 ) THEN

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      IF ( .NOT. config_flags%periodic_x ) THEN
        IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
        IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-2,ite)
      ENDIF

      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end
         mrdx=msftx(i,j)*rdx         
         tendency(i,k,j)=tendency(i,k,j)-mrdx*0.5 &
                         *(ru(i+1,k,j)*(field(i+1,k,j)+field(i  ,k,j)) &
                          -ru(i  ,k,j)*(field(i  ,k,j)+field(i-1,k,j)))
      ENDDO
      ENDDO
      ENDDO

      i_start = its
      i_end   = MIN(ite,ide-1)

      
      IF ( config_flags%open_ys .or. specified .or. config_flags%polar ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified .or. config_flags%polar ) j_end   = MIN(jde-2,jte)

      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end
         mrdy=msftx(i,j)*rdy         
         tendency(i,k,j)=tendency(i,k,j) -mrdy*0.5 &
                         *(rv(i,k,j+1)*(field(i,k,j+1)+field(i,k,j  )) &
                          -rv(i,k,j  )*(field(i,k,j  )+field(i,k,j-1))) 
      ENDDO
      ENDDO
      ENDDO
   
      
      
      IF (config_flags%polar) THEN
         IF (jts == jds) THEN
            DO k=kts,ktf
            DO i = i_start, i_end
               mrdy=msftx(i,jds)*rdy 
               tendency(i,k,jds)=tendency(i,k,jds) -mrdy*0.5 &
                                *rv(i,k,jds+1)*(field(i,k,jds+1)+field(i,k,jds))
            END DO
            END DO
         END IF
         IF (jte == jde) THEN
            DO k=kts,ktf
            DO i = i_start, i_end
               mrdy=msftx(i,jde-1)*rdy 
               tendency(i,k,jde-1)=tendency(i,k,jde-1) +mrdy*0.5 &
                                  *rv(i,k,jde-1)*(field(i,k,jde-1)+field(i,k,jde-2))
            END DO
            END DO
         END IF
      END IF

   ELSE IF ( horz_order == 0 ) THEN

      

   ELSE

      WRITE ( wrf_err_message , * ) 'module_advect: advect_scalar_6a, h_order not known ',horz_order
      CALL wrf_error_fatal3("<stdin>",3993,&
TRIM( wrf_err_message ) )

   ENDIF horizontal_order_test





      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)



   IF( (config_flags%open_xs) .and. (its == ids) ) THEN

       DO j = j_start, j_end
       DO k = kts, ktf
         ub = MIN( 0.5*(ru(its,k,j)+ru(its+1,k,j)), 0. )
         tendency(its,k,j) = tendency(its,k,j)                     &
               - rdx*(                                             &
                       ub*(   field_old(its+1,k,j)                 &
                            - field_old(its  ,k,j)   ) +           &
                       field(its,k,j)*(ru(its+1,k,j)-ru(its,k,j))  &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_xe) .and. (ite == ide) ) THEN

       DO j = j_start, j_end
       DO k = kts, ktf
         ub = MAX( 0.5*(ru(ite-1,k,j)+ru(ite,k,j)), 0. )
         tendency(i_end,k,j) = tendency(i_end,k,j)                   &
               - rdx*(                                               &
                       ub*(  field_old(i_end  ,k,j)                  &
                           - field_old(i_end-1,k,j) ) +              &
                       field(i_end,k,j)*(ru(ite,k,j)-ru(ite-1,k,j))  &
                                                                    )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_ys) .and. (jts == jds) ) THEN

       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MIN( 0.5*(rv(i,k,jts)+rv(i,k,jts+1)), 0. )
         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(  field_old(i,k,jts+1)                  &
                           - field_old(i,k,jts  ) ) +              &
                       field(i,k,jts)*(rv(i,k,jts+1)-rv(i,k,jts))  &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_ye) .and. (jte == jde)) THEN

       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MAX( 0.5*(rv(i,k,jte-1)+rv(i,k,jte)), 0. )
         tendency(i,k,j_end) = tendency(i,k,j_end)                   &
               - rdy*(                                               &
                       vb*(   field_old(i,k,j_end  )                 &
                            - field_old(i,k,j_end-1) ) +             &
                       field(i,k,j_end)*(rv(i,k,jte)-rv(i,k,jte-1))  &
                                                                    )
       ENDDO
       ENDDO

   ENDIF







      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      DO i = i_start, i_end
         vflux(i,kts)=0.
         vflux(i,kte)=0.
      ENDDO

    vert_order_test : IF (vert_order == 6) THEN    

      DO j = j_start, j_end

         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=rom(i,k,j)
           vflux(i,k) = vel*flux6(                                 &
                   field(i,k-3,j), field(i,k-2,j), field(i,k-1,j),       &
                   field(i,k  ,j), field(i,k+1,j), field(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
                                   
           k = kts+2
           vel=rom(i,k,j) 
           vflux(i,k) = vel*flux4(               &
                   field(i,k-2,j), field(i,k-1,j),   &
                   field(i,k  ,j), field(i,k+1,j), -vel )
           k = ktf-1
           vel=rom(i,k,j)
           vflux(i,k) = vel*flux4(               &
                   field(i,k-2,j), field(i,k-1,j),   &
                   field(i,k  ,j), field(i,k+1,j), -vel )

           k=ktf
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
         ENDDO

         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO

   ELSE IF (vert_order == 5) THEN    

      DO j = j_start, j_end

         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=rom(i,k,j)
           vflux(i,k) = vel*flux5(                                 &
                   field(i,k-3,j), field(i,k-2,j), field(i,k-1,j),       &
                   field(i,k  ,j), field(i,k+1,j), field(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
                                   
           k = kts+2
           vel=rom(i,k,j) 
           vflux(i,k) = vel*flux3(               &
                   field(i,k-2,j), field(i,k-1,j),   &
                   field(i,k  ,j), field(i,k+1,j), -vel )
           k = ktf-1
           vel=rom(i,k,j)
           vflux(i,k) = vel*flux3(               &
                   field(i,k-2,j), field(i,k-1,j),   &
                   field(i,k  ,j), field(i,k+1,j), -vel )

           k=ktf
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
         ENDDO

         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO

   ELSE IF (vert_order == 4) THEN    

      DO j = j_start, j_end

         DO k=kts+2,ktf-1
         DO i = i_start, i_end
           vel=rom(i,k,j)
           vflux(i,k) = vel*flux4(                                 &
                   field(i,k-2,j), field(i,k-1,j),       &
                   field(i,k  ,j), field(i,k+1,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           k=ktf
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
         ENDDO

         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO

   ELSE IF (vert_order == 3) THEN    

      DO j = j_start, j_end

         DO k=kts+2,ktf-1
         DO i = i_start, i_end
           vel=rom(i,k,j)
           vflux(i,k) = vel*flux3(                      &
                   field(i,k-2,j), field(i,k-1,j),      &
                   field(i,k  ,j), field(i,k+1,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           k=ktf
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
         ENDDO

         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO

   ELSE IF (vert_order == 2) THEN    

  DO j = j_start, j_end
     DO k = kts+1, ktf
     DO i = i_start, i_end
            vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
     ENDDO
     ENDDO

     DO k = kts, ktf
     DO i = i_start, i_end
       tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
     ENDDO
     ENDDO

  ENDDO

   ELSE

      WRITE (wrf_err_message,*) ' advect_scalar_6a, v_order not known ',vert_order
      CALL wrf_error_fatal3("<stdin>",4249,&
wrf_err_message )

   ENDIF vert_order_test

END SUBROUTINE advect_scalar



SUBROUTINE advect_w    ( w, w_old, tendency,            &
                         ru, rv, rom,                   &
                         mut, time_step, config_flags,  &
                         msfux, msfuy, msfvx, msfvy,    &
                         msftx, msfty,                  &
                         fzm, fzp,                      &
                         rdx, rdy, rdzu,                &
                         ids, ide, jds, jde, kds, kde,  &
                         ims, ime, jms, jme, kms, kme,  &
                         its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: w,     &
                                                                      w_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzu

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy
   INTEGER ,                                     INTENT(IN   ) :: time_step


   
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax

   REAL    :: mrdx, mrdy, ub, vb, uw, vw
   REAL , DIMENSION(its:ite, kts:kte) :: vflux

   INTEGER :: horz_order, vert_order

   REAL,  DIMENSION( its:ite+1, kts:kte ) :: fqx
   REAL,  DIMENSION( its:ite, kts:kte, 2 ) :: fqy
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

   INTEGER :: jp1, jp0, jtmp



   REAL    :: flux3, flux4, flux5, flux6
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel

      flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
          ( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0

      flux3(q_im2, q_im1, q_i, q_ip1, ua) =                     &
           flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
           sign(1,time_step)*sign(1.,ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0

      flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
                      ( 37.*(q_i+q_im1) - 8.*(q_ip1+q_im2)      &
                     +(q_ip2+q_im3) )/60.0

      flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)    &
            -sign(1,time_step)*sign(1.,ua)*(                    &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )/60.0


   LOGICAL :: specified

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.



  ktf=MIN(kte,kde-1)
  horz_order = config_flags%h_sca_adv_order
  vert_order = config_flags%v_sca_adv_order





  horizontal_order_test : IF( horz_order == 6 ) THEN








   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-4)                ) degrade_ye = .false.



      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)



     jp1 = 2
     jp0 = 1

     j_loop_y_flux_6 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN

        DO k=kts+1,ktf
        DO i = i_start, i_end
          vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
          fqy( i, k, jp1 ) = vel*flux6(                     &
                  w(i,k,j-3), w(i,k,j-2), w(i,k,j-1),       &
                  w(i,k,j  ), w(i,k,j+1), w(i,k,j+2),  vel )
        ENDDO
        ENDDO

        k = ktf+1
        DO i = i_start, i_end
          vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
          fqy( i, k, jp1 ) = vel*flux6(                     &
                  w(i,k,j-3), w(i,k,j-2), w(i,k,j-1),       &
                  w(i,k,j  ), w(i,k,j+1), w(i,k,j+2),  vel )
        ENDDO

      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts+1,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))*          &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))*          &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts+1,ktf
            DO i = i_start, i_end
              vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
              fqy( i, k, jp1 ) = vel*flux4(              &
                   w(i,k,j-2),w(i,k,j-1),w(i,k,j),w(i,k,j+1),vel )
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
              fqy( i, k, jp1 ) = vel*flux4(              &
                   w(i,k,j-2),w(i,k,j-1),w(i,k,j),w(i,k,j+1),vel )
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  

            DO k=kts+1,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))*      &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))*      &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  

            DO k=kts+1,ktf
            DO i = i_start, i_end
              vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
              fqy( i, k, jp1 ) = vel*flux4(             &
                   w(i,k,j-2),w(i,k,j-1),    &
                   w(i,k,j),w(i,k,j+1),vel )
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
              fqy( i, k, jp1 ) = vel*flux4(             &
                   w(i,k,j-2),w(i,k,j-1),    &
                   w(i,k,j),w(i,k,j+1),vel )
            ENDDO

     ENDIF



        
        
        
        
        IF ( config_flags%polar .AND. (j == jds+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
          END DO
          END DO
        ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
          END DO
          END DO
        ELSE  

        IF(j > j_start) THEN

          DO k=kts+1,ktf+1
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

       ENDIF

        END IF

        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

      ENDDO j_loop_y_flux_6



      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)

        i_start_f = MIN(i_start+2,ids+3)
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF



      DO j = j_start, j_end



        DO k=kts+1,ktf
        DO i = i_start_f, i_end_f
          vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
          fqx( i,k ) = vel*flux6( w(i-3,k,j), w(i-2,k,j),  &
                                  w(i-1,k,j), w(i  ,k,j),  &
                                  w(i+1,k,j), w(i+2,k,j),  &
                                  vel                     )
        ENDDO
        ENDDO

        k = ktf+1
        DO i = i_start_f, i_end_f
          vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
          fqx( i,k ) = vel*flux6( w(i-3,k,j), w(i-2,k,j),  &
                                  w(i-1,k,j), w(i  ,k,j),  &
                                  w(i+1,k,j), w(i+2,k,j),  &
                                  vel                     )
        ENDDO



        IF( degrade_xs ) THEN

          DO i=i_start,i_start_f-1

            IF(i == ids+1) THEN 
              DO k=kts+1,ktf
                fqx(i,k) = 0.5*(fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)) &
                                *(w(i,k,j)+w(i-1,k,j))
              ENDDO
              k = ktf+1
              fqx(i,k) = 0.5*((2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)) &
                     *(w(i,k,j)+w(i-1,k,j))
            ENDIF

            IF(i == ids+2) THEN  
              DO k=kts+1,ktf
                vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
                fqx( i,k ) = vel*flux4( w(i-2,k,j), w(i-1,k,j),  &
                                        w(i  ,k,j), w(i+1,k,j),  &
                                        vel                     )
              ENDDO
              k = ktf+1
              vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
              fqx( i,k ) = vel*flux4( w(i-2,k,j), w(i-1,k,j),  &
                                      w(i  ,k,j), w(i+1,k,j),  &
                                      vel                     )
            END IF

          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          DO i = i_end_f+1, i_end+1

            IF( i == ide-1 ) THEN 
              DO k=kts+1,ktf
                fqx(i,k) = 0.5*(fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j))      &
                                  *(w(i,k,j)+w(i-1,k,j))
              ENDDO
              k = ktf+1
              fqx(i,k) = 0.5*((2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j))      &
                     *(w(i,k,j)+w(i-1,k,j))
            ENDIF

            IF( i == ide-2 ) THEN 
              DO k=kts+1,ktf
                vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
                fqx( i,k ) = vel*flux4( w(i-2,k,j), w(i-1,k,j),  &
                                        w(i  ,k,j), w(i+1,k,j),  &
                                        vel                     )
              ENDDO
              k = ktf+1
              vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
              fqx( i,k ) = vel*flux4( w(i-2,k,j), w(i-1,k,j),  &
                                      w(i  ,k,j), w(i+1,k,j),  &
                                      vel                     )
            ENDIF

          ENDDO

        ENDIF



        DO k=kts+1,ktf+1
          DO i = i_start, i_end
            mrdx=msftx(i,j)*rdx      
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO

ELSE IF (horz_order == 5 ) THEN








   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-4)                ) degrade_ye = .false.



      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)



     jp1 = 2
     jp0 = 1

     j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN

        DO k=kts+1,ktf
        DO i = i_start, i_end
          vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
          fqy( i, k, jp1 ) = vel*flux5(                     &
                  w(i,k,j-3), w(i,k,j-2), w(i,k,j-1),       &
                  w(i,k,j  ), w(i,k,j+1), w(i,k,j+2),  vel )
        ENDDO
        ENDDO

        k = ktf+1
        DO i = i_start, i_end
          vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
          fqy( i, k, jp1 ) = vel*flux5(                     &
                  w(i,k,j-3), w(i,k,j-2), w(i,k,j-1),       &
                  w(i,k,j  ), w(i,k,j+1), w(i,k,j+2),  vel )
        ENDDO

      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts+1,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))*          &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))*          &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts+1,ktf
            DO i = i_start, i_end
              vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
              fqy( i, k, jp1 ) = vel*flux3(              &
                   w(i,k,j-2),w(i,k,j-1),w(i,k,j),w(i,k,j+1),vel )
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
              fqy( i, k, jp1 ) = vel*flux3(              &
                   w(i,k,j-2),w(i,k,j-1),w(i,k,j),w(i,k,j+1),vel )
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  

            DO k=kts+1,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))*      &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))*      &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  

            DO k=kts+1,ktf
            DO i = i_start, i_end
              vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
              fqy( i, k, jp1 ) = vel*flux3(             &
                   w(i,k,j-2),w(i,k,j-1),    &
                   w(i,k,j),w(i,k,j+1),vel )
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
              fqy( i, k, jp1 ) = vel*flux3(             &
                   w(i,k,j-2),w(i,k,j-1),    &
                   w(i,k,j),w(i,k,j+1),vel )
            ENDDO

     ENDIF



        
        
        
        
        IF ( config_flags%polar .AND. (j == jds+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
          END DO
          END DO
        ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
          END DO
          END DO
        ELSE  

        IF(j > j_start) THEN

          DO k=kts+1,ktf+1
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

       ENDIF

        END IF

        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

      ENDDO j_loop_y_flux_5



      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)

        i_start_f = MIN(i_start+2,ids+3)
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF



      DO j = j_start, j_end



        DO k=kts+1,ktf
        DO i = i_start_f, i_end_f
          vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
          fqx( i,k ) = vel*flux5( w(i-3,k,j), w(i-2,k,j),  &
                                  w(i-1,k,j), w(i  ,k,j),  &
                                  w(i+1,k,j), w(i+2,k,j),  &
                                  vel                     )
        ENDDO
        ENDDO

        k = ktf+1
        DO i = i_start_f, i_end_f
          vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
          fqx( i,k ) = vel*flux5( w(i-3,k,j), w(i-2,k,j),  &
                                  w(i-1,k,j), w(i  ,k,j),  &
                                  w(i+1,k,j), w(i+2,k,j),  &
                                  vel                     )
        ENDDO



        IF( degrade_xs ) THEN

          DO i=i_start,i_start_f-1

            IF(i == ids+1) THEN 
              DO k=kts+1,ktf
                fqx(i,k) = 0.5*(fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)) &
                                *(w(i,k,j)+w(i-1,k,j))
              ENDDO
              k = ktf+1
              fqx(i,k) = 0.5*((2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)) &
                     *(w(i,k,j)+w(i-1,k,j))
            ENDIF

            IF(i == ids+2) THEN  
              DO k=kts+1,ktf
                vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
                fqx( i,k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                        w(i  ,k,j), w(i+1,k,j),  &
                                        vel                     )
              ENDDO
              k = ktf+1
              vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
              fqx( i,k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                      w(i  ,k,j), w(i+1,k,j),  &
                                      vel                     )
            END IF

          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          DO i = i_end_f+1, i_end+1

            IF( i == ide-1 ) THEN 
              DO k=kts+1,ktf
                fqx(i,k) = 0.5*(fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j))      &
                                  *(w(i,k,j)+w(i-1,k,j))
              ENDDO
              k = ktf+1
              fqx(i,k) = 0.5*((2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j))      &
                     *(w(i,k,j)+w(i-1,k,j))
            ENDIF

            IF( i == ide-2 ) THEN 
              DO k=kts+1,ktf
                vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
                fqx( i,k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                        w(i  ,k,j), w(i+1,k,j),  &
                                        vel                     )
              ENDDO
              k = ktf+1
              vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
              fqx( i,k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                      w(i  ,k,j), w(i+1,k,j),  &
                                      vel                     )
            ENDIF

          ENDDO

        ENDIF



        DO k=kts+1,ktf+1
          DO i = i_start, i_end
            mrdx=msftx(i,j)*rdx      
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO

ELSE IF ( horz_order == 4 ) THEN

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.






   ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-2
        i_end_f = ide-2
      ENDIF



      DO j = j_start, j_end

        DO k=kts+1,ktf
        DO i = i_start_f, i_end_f
          vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
          fqx( i, k ) = vel*flux4( w(i-2,k,j), w(i-1,k,j),  &
                                  w(i  ,k,j), w(i+1,k,j),  &
                                  vel                     )
        ENDDO
        ENDDO

        k = ktf+1
        DO i = i_start_f, i_end_f
          vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
          fqx( i, k ) = vel*flux4( w(i-2,k,j), w(i-1,k,j),  &
                                  w(i  ,k,j), w(i+1,k,j),  &
                                  vel                     )
        ENDDO


        IF( degrade_xs ) THEN
          DO k=kts+1,ktf
            fqx(i_start, k) =                            &
               0.5*(fzm(k)*ru(i_start,k,j)+fzp(k)*ru(i_start,k-1,j))  &
                   *(w(i_start,k,j)+w(i_start-1,k,j))
          ENDDO
            k = ktf+1
            fqx(i_start, k) =                            &
               0.5*((2.-fzm(k-1))*ru(i_start,k-1,j)-fzp(k-1)*ru(i_start,k-2,j))  &
                   *(w(i_start,k,j)+w(i_start-1,k,j))
        ENDIF

        IF( degrade_xe ) THEN
          DO k=kts+1,ktf
            fqx(i_end+1, k) =                            &
               0.5*(fzm(k)*ru(i_end+1,k,j)+fzp(k)*ru(i_end+1,k-1,j))  &
                   *(w(i_end+1,k,j)+w(i_end,k,j))
          ENDDO
            k = ktf+1
            fqx(i_end+1, k) =                            &
               0.5*((2.-fzm(k-1))*ru(i_end+1,k-1,j)-fzp(k-1)*ru(i_end+1,k-2,j))  &
                   *(w(i_end+1,k,j)+w(i_end,k,j))
        ENDIF



        DO k=kts+1,ktf+1
        DO i = i_start, i_end
          mrdx=msftx(i,j)*rdx        
          tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
        ENDDO
        ENDDO

      ENDDO



      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)





      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-2
        j_end_f = jde-2
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)

        jp1 = 2
        jp0 = 1

      DO j = j_start, j_end+1

       IF ((j < j_start_f) .and. degrade_ys)  THEN
          DO k = kts+1, ktf
          DO i = i_start, i_end
            fqy(i, k, jp1) =                             &
               0.5*(fzm(k)*rv(i,k,j_start)+fzp(k)*rv(i,k-1,j_start))   &
                   *(w(i,k,j_start)+w(i,k,j_start-1))
          ENDDO
          ENDDO
          k = ktf+1
          DO i = i_start, i_end
            fqy(i, k, jp1) =                             &
               0.5*((2.-fzm(k-1))*rv(i,k-1,j_start)-fzp(k-1)*rv(i,k-2,j_start))   &
                   *(w(i,k,j_start)+w(i,k,j_start-1))
          ENDDO
       ELSE IF ((j > j_end_f) .and. degrade_ye)  THEN
          DO k = kts+1, ktf
          DO i = i_start, i_end
            



            fqy(i, k, jp1) =                             &
               0.5*(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))     &
                   *(w(i,k,j)+w(i,k,j-1))
          ENDDO
          ENDDO
          k = ktf+1
          DO i = i_start, i_end
            



            fqy(i, k, jp1) =                                         &
               0.5*((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))     &
                   *(w(i,k,j)+w(i,k,j-1))
          ENDDO
       ELSE

          DO k = kts+1, ktf
          DO i = i_start, i_end
            vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
            fqy( i, k, jp1 ) = vel*flux4( w(i,k,j-2), w(i,k,j-1),  &
                                    w(i,k,j  ), w(i,k,j+1),  &
                                    vel                     )
          ENDDO
          ENDDO
          k = ktf+1
          DO i = i_start, i_end
            vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
            fqy( i, k, jp1 ) = vel*flux4( w(i,k,j-2), w(i,k,j-1),  &
                                    w(i,k,j  ), w(i,k,j+1),  &
                                    vel                     )
          ENDDO
       END IF



       
       
       
       
       IF ( config_flags%polar .AND. (j == jds+1) ) THEN
         DO k=kts,ktf
         DO i = i_start, i_end
           mrdy=msftx(i,j-1)*rdy    
           tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
         END DO
         END DO
       ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
         DO k=kts,ktf
         DO i = i_start, i_end
           mrdy=msftx(i,j-1)*rdy    
           tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
         END DO
         END DO
       ELSE  

       IF( j > j_start ) THEN

          DO k = kts+1, ktf+1
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

       END IF

       END IF

       jtmp = jp1
       jp1 = jp0
       jp0 = jtmp

    ENDDO

ELSE IF ( horz_order == 3 ) THEN

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.






   ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-2
        i_end_f = ide-2
      ENDIF



      DO j = j_start, j_end

        DO k=kts+1,ktf
        DO i = i_start_f, i_end_f
          vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
          fqx( i, k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                  w(i  ,k,j), w(i+1,k,j),  &
                                  vel                     )
        ENDDO
        ENDDO
        k = ktf+1
        DO i = i_start_f, i_end_f
          vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
          fqx( i, k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                  w(i  ,k,j), w(i+1,k,j),  &
                                  vel                     )
        ENDDO



        IF( degrade_xs ) THEN
          DO k=kts+1,ktf
            fqx(i_start, k) =                            &
               0.5*(fzm(k)*ru(i_start,k,j)+fzp(k)*ru(i_start,k-1,j))  &
                   *(w(i_start,k,j)+w(i_start-1,k,j))
          ENDDO
            k = ktf+1
            fqx(i_start, k) =                            &
               0.5*((2.-fzm(k-1))*ru(i_start,k-1,j)-fzp(k-1)*ru(i_start,k-2,j))  &
                   *(w(i_start,k,j)+w(i_start-1,k,j))
        ENDIF

        IF( degrade_xe ) THEN
          DO k=kts+1,ktf
            fqx(i_end+1, k) =                            &
               0.5*(fzm(k)*ru(i_end+1,k,j)+fzp(k)*ru(i_end+1,k-1,j))  &
                   *(w(i_end+1,k,j)+w(i_end,k,j))
          ENDDO
            k = ktf+1
            fqx(i_end+1, k) =                            &
               0.5*((2.-fzm(k-1))*ru(i_end+1,k-1,j)-fzp(k-1)*ru(i_end+1,k-2,j))  &
                   *(w(i_end+1,k,j)+w(i_end,k,j))
        ENDIF



        DO k=kts+1,ktf+1
        DO i = i_start, i_end
          mrdx=msftx(i,j)*rdx        
          tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
        ENDDO
        ENDDO

      ENDDO



      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)





      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-2
        j_end_f = jde-2
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)

        jp1 = 2
        jp0 = 1

      DO j = j_start, j_end+1

       IF ((j < j_start_f) .and. degrade_ys)  THEN
          DO k = kts+1, ktf
          DO i = i_start, i_end
            fqy(i, k, jp1) =                             &
               0.5*(fzm(k)*rv(i,k,j_start)+fzp(k)*rv(i,k-1,j_start))   &
                   *(w(i,k,j_start)+w(i,k,j_start-1))
          ENDDO
          ENDDO
          k = ktf+1
          DO i = i_start, i_end
            fqy(i, k, jp1) =                             &
               0.5*((2.-fzm(k-1))*rv(i,k-1,j_start)-fzp(k-1)*rv(i,k-2,j_start))   &
                   *(w(i,k,j_start)+w(i,k,j_start-1))
          ENDDO
       ELSE IF ((j > j_end_f) .and. degrade_ye)  THEN
          DO k = kts+1, ktf
          DO i = i_start, i_end
            



            fqy(i, k, jp1) =                             &
               0.5*(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))     &
                   *(w(i,k,j)+w(i,k,j-1))
          ENDDO
          ENDDO
          k = ktf+1
          DO i = i_start, i_end
            



            fqy(i, k, jp1) =                             &
               0.5*((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))     &
                   *(w(i,k,j)+w(i,k,j-1))
          ENDDO
       ELSE

          DO k = kts+1, ktf
          DO i = i_start, i_end
            vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
            fqy( i, k, jp1 ) = vel*flux3( w(i,k,j-2), w(i,k,j-1),  &
                                    w(i,k,j  ), w(i,k,j+1),  &
                                    vel                     )
          ENDDO
          ENDDO
          k = ktf+1
          DO i = i_start, i_end
            vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
            fqy( i, k, jp1 ) = vel*flux3( w(i,k,j-2), w(i,k,j-1),  &
                                    w(i,k,j  ), w(i,k,j+1),  &
                                    vel                     )
          ENDDO
       END IF



       
       
       
       
       IF ( config_flags%polar .AND. (j == jds+1) ) THEN
         DO k=kts,ktf
         DO i = i_start, i_end
           mrdy=msftx(i,j-1)*rdy    
           tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
         END DO
         END DO
       ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
         DO k=kts,ktf
         DO i = i_start, i_end
           mrdy=msftx(i,j-1)*rdy    
           tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
         END DO
         END DO
       ELSE  

       IF( j > j_start ) THEN

          DO k = kts+1, ktf+1
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

       END IF

       END IF

       jtmp = jp1
       jp1 = jp0
       jp0 = jtmp

    ENDDO

ELSE IF (horz_order == 2 ) THEN

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      IF ( .NOT. config_flags%periodic_x ) THEN
        IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
        IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-2,ite)
      ENDIF

      DO j = j_start, j_end
      DO k=kts+1,ktf
      DO i = i_start, i_end

         mrdx=msftx(i,j)*rdx         

            tendency(i,k,j)=tendency(i,k,j)-mrdx*0.5            &
                   *((fzm(k)*ru(i+1,k,j)+fzp(k)*ru(i+1,k-1,j))  &
                                *(w(i+1,k,j)+w(i,k,j))          &
                    -(fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j))      &
                               *(w(i,k,j)+w(i-1,k,j)))

      ENDDO
      ENDDO

      k = ktf+1
      DO i = i_start, i_end

         mrdx=msftx(i,j)*rdx         

            tendency(i,k,j)=tendency(i,k,j)-mrdx*0.5            &
                   *(((2.-fzm(k-1))*ru(i+1,k-1,j)-fzp(k-1)*ru(i+1,k-2,j))      &
                                *(w(i+1,k,j)+w(i,k,j))          &
                    -((2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j))         &
                               *(w(i,k,j)+w(i-1,k,j)))

      ENDDO

      ENDDO

      i_start = its
      i_end   = MIN(ite,ide-1)
      
      IF ( config_flags%open_ys .or. specified .or. config_flags%polar ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified .or. config_flags%polar ) j_end   = MIN(jde-2,jte)

      DO j = j_start, j_end
      DO k=kts+1,ktf
      DO i = i_start, i_end

         mrdy=msftx(i,j)*rdy         

            tendency(i,k,j)=tendency(i,k,j) -mrdy*0.5           &
                   *((fzm(k)*rv(i,k,j+1)+fzp(k)*rv(i,k-1,j+1))* &
                                 (w(i,k,j+1)+w(i,k,j))          &
                    -(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))      &
                                 *(w(i,k,j)+w(i,k,j-1))) 

      ENDDO
      ENDDO

      k = ktf+1
      DO i = i_start, i_end

         mrdy=msftx(i,j)*rdy         

            tendency(i,k,j)=tendency(i,k,j) -mrdy*0.5       &
                   *(((2.-fzm(k-1))*rv(i,k-1,j+1)-fzp(k-1)*rv(i,k-2,j+1))* &
                                 (w(i,k,j+1)+w(i,k,j))      &
                    -((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))      &
                                 *(w(i,k,j)+w(i,k,j-1))) 

      ENDDO

      ENDDO

      
      IF (config_flags%polar) THEN
         IF (jts == jds) THEN
            DO k=kts+1,ktf
            DO i = i_start, i_end
               mrdy=msftx(i,jds)*rdy   
               tendency(i,k,jds)=tendency(i,k,jds) -mrdy*0.5 &
                          *((fzm(k)*rv(i,k,jds+1)+fzp(k)*rv(i,k-1,jds+1))* &
                            (w(i,k,jds+1)+w(i,k,jds)))
            END DO
            END DO
            k = ktf+1
            DO i = i_start, i_end
               mrdy=msftx(i,jds)*rdy   
               tendency(i,k,jds)=tendency(i,k,jds) -mrdy*0.5       &
                   *((2.-fzm(k-1))*rv(i,k-1,jds+1)-fzp(k-1)*rv(i,k-2,jds+1))* &
                                 (w(i,k,jds+1)+w(i,k,jds))
            ENDDO
         END IF
         IF (jte == jde) THEN
            DO k=kts+1,ktf
            DO i = i_start, i_end
               mrdy=msftx(i,jde-1)*rdy 
               tendency(i,k,jde-1)=tendency(i,k,jde-1) +mrdy*0.5 &
                          *((fzm(k)*rv(i,k,jde-1)+fzp(k)*rv(i,k-1,jde-1))* &
                            (w(i,k,jde-1)+w(i,k,jde-2)))
            END DO
            END DO
            k = ktf+1
            DO i = i_start, i_end
               mrdy=msftx(i,jde-1)*rdy 
               tendency(i,k,jde-1)=tendency(i,k,jde-1) +mrdy*0.5       &
                    *((2.-fzm(k-1))*rv(i,k-1,jde-1)-fzp(k-1)*rv(i,k-2,jde-1)) &
                                 *(w(i,k,jde-1)+w(i,k,jde-2))
            ENDDO
         END IF
      END IF

   ELSE IF ( horz_order == 0 ) THEN

      

   ELSE

      WRITE ( wrf_err_message ,*) ' advect_w_6a, h_order not known ',horz_order
      CALL wrf_error_fatal3("<stdin>",5576,&
wrf_err_message )

   ENDIF horizontal_order_test







      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

   IF( (config_flags%open_xs) .and. (its == ids)) THEN

       DO j = j_start, j_end
       DO k = kts+1, ktf

         uw = 0.5*(fzm(k)*(ru(its,k  ,j)+ru(its+1,k  ,j)) +  &
                   fzp(k)*(ru(its,k-1,j)+ru(its+1,k-1,j))   )
         ub = MIN( uw, 0. )

         tendency(its,k,j) = tendency(its,k,j)                     &
               - rdx*(                                             &
                       ub*(w_old(its+1,k,j) - w_old(its,k,j)) +    &
                       w(its,k,j)*(                                &
                       fzm(k)*(ru(its+1,k  ,j)-ru(its,k  ,j))+     &
                       fzp(k)*(ru(its+1,k-1,j)-ru(its,k-1,j)))     &
                                                                  )
       ENDDO
       ENDDO

       k = ktf+1
       DO j = j_start, j_end

         uw = 0.5*( (2.-fzm(k-1))*(ru(its,k-1,j)+ru(its+1,k-1,j))   &
                   -fzp(k-1)*(ru(its,k-2,j)+ru(its+1,k-2,j))   )
         ub = MIN( uw, 0. )

         tendency(its,k,j) = tendency(its,k,j)                     &
               - rdx*(                                             &
                       ub*(w_old(its+1,k,j) - w_old(its,k,j)) +    &
                       w(its,k,j)*(                                &
                             (2.-fzm(k-1))*(ru(its+1,k-1,j)-ru(its,k-1,j))-  &
                             fzp(k-1)*(ru(its+1,k-2,j)-ru(its,k-2,j)))  &
                                                                  )
       ENDDO

   ENDIF

   IF( (config_flags%open_xe) .and. (ite == ide)) THEN

       DO j = j_start, j_end
       DO k = kts+1, ktf

         uw = 0.5*(fzm(k)*(ru(ite-1,k  ,j)+ru(ite,k  ,j)) +  &
                   fzp(k)*(ru(ite-1,k-1,j)+ru(ite,k-1,j))   )
         ub = MAX( uw, 0. )

         tendency(i_end,k,j) = tendency(i_end,k,j)                     &
               - rdx*(                                                 &
                       ub*(w_old(i_end,k,j) - w_old(i_end-1,k,j)) +    &
                       w(i_end,k,j)*(                                  &
                            fzm(k)*(ru(ite,k  ,j)-ru(ite-1,k  ,j)) +   &
                            fzp(k)*(ru(ite,k-1,j)-ru(ite-1,k-1,j)))    &
                                                                    )
       ENDDO
       ENDDO

       k = ktf+1
       DO j = j_start, j_end

         uw = 0.5*( (2.-fzm(k-1))*(ru(ite-1,k-1,j)+ru(ite,k-1,j))    &
                   -fzp(k-1)*(ru(ite-1,k-2,j)+ru(ite,k-2,j))   )
         ub = MAX( uw, 0. )

         tendency(i_end,k,j) = tendency(i_end,k,j)                     &
               - rdx*(                                                 &
                       ub*(w_old(i_end,k,j) - w_old(i_end-1,k,j)) +    &
                       w(i_end,k,j)*(                                  &
                               (2.-fzm(k-1))*(ru(ite,k-1,j)-ru(ite-1,k-1,j)) -   &
                               fzp(k-1)*(ru(ite,k-2,j)-ru(ite-1,k-2,j)))    &
                                                                    )
       ENDDO

   ENDIF


   IF( (config_flags%open_ys) .and. (jts == jds)) THEN

       DO i = i_start, i_end
       DO k = kts+1, ktf

         vw = 0.5*( fzm(k)*(rv(i,k  ,jts)+rv(i,k  ,jts+1)) +  &
                    fzp(k)*(rv(i,k-1,jts)+rv(i,k-1,jts+1))   )
         vb = MIN( vw, 0. )

         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(w_old(i,k,jts+1) - w_old(i,k,jts)) +    &
                       w(i,k,jts)*(                                &
                       fzm(k)*(rv(i,k  ,jts+1)-rv(i,k  ,jts))+     &
                       fzp(k)*(rv(i,k-1,jts+1)-rv(i,k-1,jts)))     &
                                                                )
       ENDDO
       ENDDO

       k = ktf+1
       DO i = i_start, i_end
         vw = 0.5*( (2.-fzm(k-1))*(rv(i,k-1,jts)+rv(i,k-1,jts+1))    &
                   -fzp(k-1)*(rv(i,k-2,jts)+rv(i,k-2,jts+1))   )
         vb = MIN( vw, 0. )

         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(w_old(i,k,jts+1) - w_old(i,k,jts)) +    &
                       w(i,k,jts)*(                                &
                          (2.-fzm(k-1))*(rv(i,k-1,jts+1)-rv(i,k-1,jts))-     &
                          fzp(k-1)*(rv(i,k-2,jts+1)-rv(i,k-2,jts)))     &
                                                                )
       ENDDO

   ENDIF

   IF( (config_flags%open_ye) .and. (jte == jde) ) THEN

       DO i = i_start, i_end
       DO k = kts+1, ktf

         vw = 0.5*( fzm(k)*(rv(i,k  ,jte-1)+rv(i,k  ,jte)) +  &
                    fzp(k)*(rv(i,k-1,jte-1)+rv(i,k-1,jte))   )
         vb = MAX( vw, 0. )

         tendency(i,k,j_end) = tendency(i,k,j_end)                     &
               - rdy*(                                                 &
                       vb*(w_old(i,k,j_end) - w_old(i,k,j_end-1)) +    &
                       w(i,k,j_end)*(                                  &
                            fzm(k)*(rv(i,k  ,jte)-rv(i,k  ,jte-1))+    &
                            fzp(k)*(rv(i,k-1,jte)-rv(i,k-1,jte-1)))    &
                                                                      )
       ENDDO
       ENDDO

       k = ktf+1
       DO i = i_start, i_end

         vw = 0.5*( (2.-fzm(k-1))*(rv(i,k-1,jte-1)+rv(i,k-1,jte))    &
                   -fzp(k-1)*(rv(i,k-2,jte-1)+rv(i,k-2,jte))   )
         vb = MAX( vw, 0. )

         tendency(i,k,j_end) = tendency(i,k,j_end)                     &
               - rdy*(                                                 &
                       vb*(w_old(i,k,j_end) - w_old(i,k,j_end-1)) +    &
                       w(i,k,j_end)*(                                  &
                               (2.-fzm(k-1))*(rv(i,k-1,jte)-rv(i,k-1,jte-1))-    &
                               fzp(k-1)*(rv(i,k-2,jte)-rv(i,k-2,jte-1)))    &
                                                                      )
       ENDDO

   ENDIF






      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      DO i = i_start, i_end
         vflux(i,kts)=0.
         vflux(i,kte)=0.
      ENDDO

    vert_order_test : IF (vert_order == 6) THEN    

      DO j = j_start, j_end

         DO k=kts+3,ktf-1
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux6(                                   &
                   w(i,k-3,j), w(i,k-2,j), w(i,k-1,j),       &
                   w(i,k  ,j), w(i,k+1,j), w(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))

           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux4(               &
                   w(i,k-2,j), w(i,k-1,j),   &
                   w(i,k  ,j), w(i,k+1,j), -vel )

           k = ktf
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux4(               &
                   w(i,k-2,j), w(i,k-1,j),   &
                   w(i,k  ,j), w(i,k+1,j), -vel )

           k=ktf+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))

         ENDDO

         DO k=kts+1,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzu(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO


         k = ktf+1
         DO i = i_start, i_end
           tendency(i,k,j)=tendency(i,k,j)+2.*rdzu(k-1)*(vflux(i,k))
         ENDDO

      ENDDO

 ELSE IF (vert_order == 5) THEN    

      DO j = j_start, j_end

         DO k=kts+3,ktf-1
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux5(                                   &
                   w(i,k-3,j), w(i,k-2,j), w(i,k-1,j),       &
                   w(i,k  ,j), w(i,k+1,j), w(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))
                                   
           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux3(               &
                   w(i,k-2,j), w(i,k-1,j),   &
                   w(i,k  ,j), w(i,k+1,j), -vel )
           k = ktf
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux3(               &
                   w(i,k-2,j), w(i,k-1,j),   &
                   w(i,k  ,j), w(i,k+1,j), -vel )

           k=ktf+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))

         ENDDO

         DO k=kts+1,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzu(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO


         k = ktf+1
         DO i = i_start, i_end
           tendency(i,k,j)=tendency(i,k,j)+2.*rdzu(k-1)*(vflux(i,k))
         ENDDO

      ENDDO

 ELSE IF (vert_order == 4) THEN    

      DO j = j_start, j_end

         DO k=kts+2,ktf
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux4(              &
                   w(i,k-2,j), w(i,k-1,j),      &
                   w(i,k  ,j), w(i,k+1,j), -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))
           k=ktf+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))

         ENDDO

         DO k=kts+1,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzu(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO


         k = ktf+1
         DO i = i_start, i_end
           tendency(i,k,j)=tendency(i,k,j)+2.*rdzu(k-1)*(vflux(i,k))
         ENDDO

      ENDDO

 ELSE IF (vert_order == 3) THEN    

      DO j = j_start, j_end

         DO k=kts+2,ktf
!DEC$ vector always
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux3(              &
                   w(i,k-2,j), w(i,k-1,j),      &
                   w(i,k  ,j), w(i,k+1,j), -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))
           k=ktf+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))

         ENDDO

         DO k=kts+1,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzu(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO


         k = ktf+1
         DO i = i_start, i_end
           tendency(i,k,j)=tendency(i,k,j)+2.*rdzu(k-1)*(vflux(i,k))
         ENDDO

      ENDDO

 ELSE IF (vert_order == 2) THEN    

  DO j = j_start, j_end
     DO k=kts+1,ktf+1
     DO i = i_start, i_end

            vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))
     ENDDO
     ENDDO
     DO k=kts+1,ktf
     DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzu(k)*(vflux(i,k+1)-vflux(i,k))

     ENDDO
     ENDDO


     k = ktf+1
     DO i = i_start, i_end
       tendency(i,k,j)=tendency(i,k,j)+2.*rdzu(k-1)*(vflux(i,k))
     ENDDO

  ENDDO

   ELSE

      WRITE (wrf_err_message ,*) ' advect_w, v_order not known ',vert_order
      CALL wrf_error_fatal3("<stdin>",5952,&
wrf_err_message )

   ENDIF vert_order_test

END SUBROUTINE advect_w



SUBROUTINE advect_scalar_pd   ( field, field_old, tendency,    &
                                h_tendency, z_tendency,        & 
                                ru, rv, rom,                   &
                                mut, mub, mu_old,              &
                                time_step, config_flags,       &
                                tenddec,                       & 
                                msfux, msfuy, msfvx, msfvy,    &
                                msftx, msfty,                  &
                                fzm, fzp,                      &
                                rdx, rdy, rdzw, dt,            &
                                ids, ide, jds, jde, kds, kde,  &
                                ims, ime, jms, jme, kms, kme,  &
                                its, ite, jts, jte, kts, kte  )












   IMPLICIT NONE
   
   
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   LOGICAL ,                 INTENT(IN   ) :: tenddec  

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: field,     &
                                                                      field_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut, mub, mu_old
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(  OUT) :: h_tendency, z_tendency 

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy,  &
                                                                  dt
   INTEGER ,                                     INTENT(IN   ) :: time_step

   
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax

   REAL    :: mrdx, mrdy, ub, vb, uw, vw, mu



   REAL,  DIMENSION( its-1:ite+2, kts:kte, jts-1:jte+2  ) :: fqx, fqy, fqz

   REAL,  DIMENSION( its-1:ite+2, kts:kte, jts-1:jte+2  ) :: fqxl, fqyl, fqzl

   INTEGER :: horz_order, vert_order
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

   INTEGER :: jp1, jp0, jtmp

   REAL :: flux_out, ph_low, scale
   REAL, PARAMETER :: eps=1.e-20




   REAL    :: flux3, flux4, flux5, flux6, flux_upwind
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel, cr

      flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
            (7./12.)*(q_i + q_im1) - (1./12.)*(q_ip1 + q_im2)

      flux3(q_im2, q_im1, q_i, q_ip1, ua) =                     &
           flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
           sign(1,time_step)*sign(1.,ua)*(1./12.)*((q_ip1 - q_im2)-3.*(q_i-q_im1))

      flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
            (37./60.)*(q_i+q_im1) - (2./15.)*(q_ip1+q_im2)      &
            +(1./60.)*(q_ip2+q_im3)

      flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)    &
            -sign(1,time_step)*sign(1.,ua)*(1./60.)*(           &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )

      flux_upwind(q_im1, q_i, cr ) = 0.5*min( 1.0,(cr+abs(cr)))*q_im1 &
                                    +0.5*max(-1.0,(cr-abs(cr)))*q_i





    REAL     :: dx,dy,dz

    LOGICAL, PARAMETER :: pd_limit = .true.





    
    IF (config_flags%polar) THEN
       fqx(:,:,:)  = 0.
       fqy(:,:,:)  = 0.
       fqz(:,:,:)  = 0.
       fqxl(:,:,:) = 0.
       fqyl(:,:,:) = 0.
       fqzl(:,:,:) = 0.
    END IF

  ktf=MIN(kte,kde-1)
  horz_order = config_flags%h_sca_adv_order
  vert_order = config_flags%v_sca_adv_order








   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.





  horizontal_order_test : IF( horz_order == 6 ) THEN

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-4)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-4)                ) degrade_ye = .false.





      ktf=MIN(kte,kde-1)
      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1
      j_start_f = j_start
      j_end_f   = j_end+1





      IF(degrade_xs) i_start = MAX(its-1,ids)
      IF(degrade_xe) i_end   = MIN(ite+1,ide-1)

      IF(degrade_ys) then
        j_start = MAX(jts-1,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte+1,jde-2)
        j_end_f = jde-3
      ENDIF



      j_loop_y_flux_6 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN 

        DO k=kts,ktf
        DO i = i_start, i_end

          dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
          mu = 0.5*(mut(i,j)+mut(i,j-1))
          vel = rv(i,k,j)
          cr = vel*dt/dy/mu
          fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

          fqy( i, k, j  ) = vel*flux6(                                  &
                  field(i,k,j-3), field(i,k,j-2), field(i,k,j-1),       &
                  field(i,k,j  ), field(i,k,j+1), field(i,k,j+2),  vel )

          fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

        ENDDO
        ENDDO

      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy(i,k, j) = 0.5*rv(i,k,j)*          &
                     (field(i,k,j)+field(i,k,j-1))

              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy( i, k, j ) = vel*flux4(              &
                   field(i,k,j-2),field(i,k,j-1),field(i,k,j),field(i,k,j+1),vel )
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy(i, k, j ) = 0.5*rv(i,k,j)*      &
                     (field(i,k,j)+field(i,k,j-1))
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ELSE IF ( j == jde-2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy( i, k, j) = vel*flux4(             &
                   field(i,k,j-2),field(i,k,j-1),    &
                   field(i,k,j),field(i,k,j+1),vel )
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ENDIF

   ENDDO j_loop_y_flux_6





      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      i_start_f = i_start
      i_end_f   = i_end+1

      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1





      IF(degrade_ys) j_start = MAX(jts-1,jds)
      IF(degrade_ye) j_end   = MIN(jte+1,jde-1)

      IF(degrade_xs) then
        i_start = MAX(ids+1,its-1)
        i_start_f = ids+3
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite+1)
        i_end_f = ide-3
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f

          dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
          mu = 0.5*(mut(i,j)+mut(i-1,j))
          vel = ru(i,k,j)
          cr = vel*dt/dx/mu
          fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)

          fqx( i,k,j ) = vel*flux6( field(i-3,k,j), field(i-2,k,j),  &
                                         field(i-1,k,j), field(i  ,k,j),  &
                                         field(i+1,k,j), field(i+2,k,j),  &
                                         vel                             )
          fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

        ENDDO
        ENDDO



        IF( degrade_xs ) THEN

          DO i=i_start,i_start_f-1

            IF(i == ids+1) THEN 
              DO k=kts,ktf
                dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
                mu = 0.5*(mut(i,j)+mut(i-1,j))
                vel = ru(i,k,j)/mu
                cr = vel*dt/dx
                fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx(i,k,j) = 0.5*(ru(i,k,j)) &
                       *(field(i,k,j)+field(i-1,k,j))
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)
              ENDDO
            ENDIF

            IF(i == ids+2) THEN  
              DO k=kts,ktf
                dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
                mu = 0.5*(mut(i,j)+mut(i-1,j))
                vel = ru(i,k,j)
                cr = vel*dt/dx/mu
                fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx( i,k,j ) = vel*flux4( field(i-2,k,j), field(i-1,k,j),  &
                                          field(i  ,k,j), field(i+1,k,j),  &
                                          vel                             )
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)
              ENDDO
            ENDIF

          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          DO i = i_end_f+1, i_end+1

            IF( i == ide-1 ) THEN 
              DO k=kts,ktf
                dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
                mu = 0.5*(mut(i,j)+mut(i-1,j))
                vel = ru(i,k,j)
                cr = vel*dt/dx/mu
                fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx(i,k,j) = 0.5*(ru(i,k,j))      &
                       *(field(i,k,j)+field(i-1,k,j))
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)
              ENDDO
            ENDIF


            IF( i == ide-2 ) THEN 
              DO k=kts,ktf
                dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
                mu = 0.5*(mut(i,j)+mut(i-1,j))
                vel = ru(i,k,j)
                cr = vel*dt/dx/mu
                fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx( i,k,j ) = vel*flux4( field(i-2,k,j), field(i-1,k,j),  &
                                          field(i  ,k,j), field(i+1,k,j),  &
                                          vel                             )
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)
              ENDDO
            ENDIF

          ENDDO

        ENDIF

      ENDDO  



    ELSE IF( horz_order == 5 ) THEN

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-4)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-4)                ) degrade_ye = .false.





      ktf=MIN(kte,kde-1)
      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1
      j_start_f = j_start
      j_end_f   = j_end+1





      IF(degrade_xs) i_start = MAX(its-1,ids)
      IF(degrade_xe) i_end   = MIN(ite+1,ide-1)

      IF(degrade_ys) then
        j_start = MAX(jts-1,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte+1,jde-2)
        j_end_f = jde-3
      ENDIF



      j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN 

        DO k=kts,ktf
        DO i = i_start, i_end

          dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
          mu = 0.5*(mut(i,j)+mut(i,j-1))
          vel = rv(i,k,j)
          cr = vel*dt/dy/mu
          fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

          fqy( i, k, j  ) = vel*flux5(                                  &
                  field(i,k,j-3), field(i,k,j-2), field(i,k,j-1),       &
                  field(i,k,j  ), field(i,k,j+1), field(i,k,j+2),  vel )

          fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

        ENDDO
        ENDDO

      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy(i,k, j) = 0.5*rv(i,k,j)*          &
                     (field(i,k,j)+field(i,k,j-1))

              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy( i, k, j ) = vel*flux3(              &
                   field(i,k,j-2),field(i,k,j-1),field(i,k,j),field(i,k,j+1),vel )
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy(i, k, j ) = 0.5*rv(i,k,j)*      &
                     (field(i,k,j)+field(i,k,j-1))
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ELSE IF ( j == jde-2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy( i, k, j) = vel*flux3(             &
                   field(i,k,j-2),field(i,k,j-1),    &
                   field(i,k,j),field(i,k,j+1),vel )
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ENDIF

   ENDDO j_loop_y_flux_5





      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      i_start_f = i_start
      i_end_f   = i_end+1

      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1





      IF(degrade_ys) j_start = MAX(jts-1,jds)
      IF(degrade_ye) j_end   = MIN(jte+1,jde-1)

      IF(degrade_xs) then
        i_start = MAX(ids+1,its-1)
        i_start_f = ids+3
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite+1)
        i_end_f = ide-3
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f

          dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
          mu = 0.5*(mut(i,j)+mut(i-1,j))
          vel = ru(i,k,j)
          cr = vel*dt/dx/mu
          fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)

          fqx( i,k,j ) = vel*flux5( field(i-3,k,j), field(i-2,k,j),  &
                                         field(i-1,k,j), field(i  ,k,j),  &
                                         field(i+1,k,j), field(i+2,k,j),  &
                                         vel                             )
          fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

        ENDDO
        ENDDO



        IF( degrade_xs ) THEN

          DO i=i_start,i_start_f-1

            IF(i == ids+1) THEN 
              DO k=kts,ktf
                dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
                mu = 0.5*(mut(i,j)+mut(i-1,j))
                vel = ru(i,k,j)/mu
                cr = vel*dt/dx
                fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx(i,k,j) = 0.5*(ru(i,k,j)) &
                       *(field(i,k,j)+field(i-1,k,j))
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)
              ENDDO
            ENDIF

            IF(i == ids+2) THEN  
              DO k=kts,ktf
                dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
                mu = 0.5*(mut(i,j)+mut(i-1,j))
                vel = ru(i,k,j)
                cr = vel*dt/dx/mu
                fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx( i,k,j ) = vel*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                          field(i  ,k,j), field(i+1,k,j),  &
                                          vel                             )
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)
              ENDDO
            ENDIF

          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          DO i = i_end_f+1, i_end+1

            IF( i == ide-1 ) THEN 
              DO k=kts,ktf
                dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
                mu = 0.5*(mut(i,j)+mut(i-1,j))
                vel = ru(i,k,j)
                cr = vel*dt/dx/mu
                fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx(i,k,j) = 0.5*(ru(i,k,j))      &
                       *(field(i,k,j)+field(i-1,k,j))
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)
              ENDDO
            ENDIF


            IF( i == ide-2 ) THEN 
              DO k=kts,ktf
                dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
                mu = 0.5*(mut(i,j)+mut(i-1,j))
                vel = ru(i,k,j)
                cr = vel*dt/dx/mu
                fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx( i,k,j ) = vel*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                          field(i  ,k,j), field(i+1,k,j),  &
                                          vel                             )
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)
              ENDDO
            ENDIF

          ENDDO

        ENDIF

      ENDDO  



    ELSE IF( horz_order == 4 ) THEN

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+1)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+1)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-2)                ) degrade_ye = .false.





      ktf=MIN(kte,kde-1)
      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1
      j_start_f = j_start
      j_end_f   = j_end+1



      IF(degrade_xs) i_start = its
      IF(degrade_xe) i_end   = MIN(ite,ide-1)

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+2
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-2
      ENDIF



      j_loop_y_flux_4 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN 

        DO k=kts,ktf
        DO i = i_start, i_end

          dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
          mu = 0.5*(mut(i,j)+mut(i,j-1))
          vel = rv(i,k,j)
          cr = vel*dt/dy/mu
          fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

          fqy( i, k, j  ) = vel*flux4(  field(i,k,j-2), field(i,k,j-1),       &
                                        field(i,k,j  ), field(i,k,j+1), vel )

          fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

        ENDDO
        ENDDO

      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy(i,k, j) = 0.5*rv(i,k,j)*          &
                     (field(i,k,j)+field(i,k,j-1))

              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy(i, k, j ) = 0.5*rv(i,k,j)*      &
                     (field(i,k,j)+field(i,k,j-1))
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ENDIF

   ENDDO j_loop_y_flux_4





      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      i_start_f = i_start
      i_end_f   = i_end+1

      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1



      IF(degrade_ys) j_start = jts
      IF(degrade_ye) j_end   = MIN(jte,jde-1)

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-2
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f

          dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
          mu = 0.5*(mut(i,j)+mut(i-1,j))
          vel = ru(i,k,j)
          cr = vel*dt/dx/mu
          fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)

          fqx( i,k,j ) = vel*flux4( field(i-2,k,j), field(i-1,k,j), &
                                    field(i  ,k,j), field(i+1,k,j), vel )
          fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

        ENDDO
        ENDDO



        IF( degrade_xs ) THEN
          IF( i_start == ids+1 ) THEN 
            i = ids+1
            DO k=kts,ktf

              dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
              mu = 0.5*(mut(i,j)+mut(i-1,j))
              vel = ru(i,k,j)/mu
              cr = vel*dt/dx
              fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)

              fqx(i,k,j) = 0.5*(ru(i,k,j)) &
                     *(field(i,k,j)+field(i-1,k,j))

              fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

            ENDDO
          ENDIF
        ENDIF

        IF( degrade_xe ) THEN
          IF( i_end == ide-2 ) THEN 
            i = ide-1
            DO k=kts,ktf
              dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
              mu = 0.5*(mut(i,j)+mut(i-1,j))
              vel = ru(i,k,j)
              cr = vel*dt/dx/mu
              fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
              fqx(i,k,j) = 0.5*(ru(i,k,j))      &
                     *(field(i,k,j)+field(i-1,k,j))
              fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

            ENDDO
          ENDIF
        ENDIF

      ENDDO  



   ELSE IF( horz_order == 3 ) THEN

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-1)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-1)                ) degrade_ye = .false.





      ktf=MIN(kte,kde-1)
      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1
      j_start_f = j_start
      j_end_f   = j_end+1



      IF(degrade_xs) i_start = its
      IF(degrade_xe) i_end   = MIN(ite,ide-1)

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+2
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-2
      ENDIF



      j_loop_y_flux_3 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN 

        DO k=kts,ktf
        DO i = i_start, i_end

          dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
          mu = 0.5*(mut(i,j)+mut(i,j-1))
          vel = rv(i,k,j)
          cr = vel*dt/dy/mu
          fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

          fqy( i, k, j  ) = vel*flux3(  field(i,k,j-2), field(i,k,j-1),       &
                                        field(i,k,j  ), field(i,k,j+1), vel )

          fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

        ENDDO
        ENDDO

      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy(i,k, j) = 0.5*rv(i,k,j)*          &
                     (field(i,k,j)+field(i,k,j-1))

              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy(i, k, j ) = 0.5*rv(i,k,j)*      &
                     (field(i,k,j)+field(i,k,j-1))
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ENDIF

   ENDDO j_loop_y_flux_3





      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      i_start_f = i_start
      i_end_f   = i_end+1

      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1



      IF(degrade_ys) j_start = jts
      IF(degrade_ye) j_end   = MIN(jte,jde-1)

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-2
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f

          dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
          mu = 0.5*(mut(i,j)+mut(i-1,j))
          vel = ru(i,k,j)
          cr = vel*dt/dx/mu
          fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)

          fqx( i,k,j ) = vel*flux3( field(i-2,k,j), field(i-1,k,j), &
                                    field(i  ,k,j), field(i+1,k,j), vel )
          fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

        ENDDO
        ENDDO



        IF( degrade_xs ) THEN

          IF( i_start == ids+1 ) THEN 
            i = ids+1
            DO k=kts,ktf

              dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
              mu = 0.5*(mut(i,j)+mut(i-1,j))
              vel = ru(i,k,j)/mu
              cr = vel*dt/dx
              fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)

              fqx(i,k,j) = 0.5*(ru(i,k,j)) &
                     *(field(i,k,j)+field(i-1,k,j))

              fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

            ENDDO
          ENDIF
        ENDIF

        IF( degrade_xe ) THEN
          IF( i_end == ide-2 ) THEN 
            i = ide-1
            DO k=kts,ktf
              dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
              mu = 0.5*(mut(i,j)+mut(i-1,j))
              vel = ru(i,k,j)
              cr = vel*dt/dx/mu
              fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
              fqx(i,k,j) = 0.5*(ru(i,k,j))      &
                     *(field(i,k,j)+field(i-1,k,j))
              fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

            ENDDO
          ENDIF
        ENDIF

      ENDDO  




   ELSE IF( horz_order == 2 ) THEN

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+1)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+1)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-2)                ) degrade_ye = .false.



      ktf=MIN(kte,kde-1)
      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1



      IF(degrade_xs) i_start = its
      IF(degrade_xe) i_end   = MIN(ite,ide-1)
      IF(degrade_ys) j_start = MAX(jts,jds+1)
      IF(degrade_ye) j_end = MIN(jte,jde-2)



      DO j = j_start, j_end+1
        DO k=kts,ktf
        DO i = i_start, i_end
           dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
           mu = 0.5*(mut(i,j)+mut(i,j-1))
           vel = rv(i,k,j)
           cr = vel*dt/dy/mu
           fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

           fqy(i,k, j) = 0.5*rv(i,k,j)*          &
                  (field(i,k,j)+field(i,k,j-1))

           fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)
        ENDDO
        ENDDO
      ENDDO



      DO j = j_start, j_end
        DO k=kts,ktf
        DO i = i_start, i_end+1
            dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx 
            mu = 0.5*(mut(i,j)+mut(i-1,j))
            vel = ru(i,k,j)
            cr = vel*dt/dx/mu
            fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
            fqx( i,k,j ) = 0.5*ru(i,k,j)*          &
                  (field(i,k,j)+field(i-1,k,j))

            fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)
        ENDDO
        ENDDO
      ENDDO



   ELSE

      WRITE ( wrf_err_message , * ) 'module_advect: advect_scalar_pd, h_order not known ',horz_order
      CALL wrf_error_fatal3("<stdin>",7138,&
TRIM( wrf_err_message ) )

   ENDIF horizontal_order_test





      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)



   IF( (config_flags%open_xs) .and. (its == ids) ) THEN

       DO j = j_start, j_end
       DO k = kts, ktf
         ub = MIN( 0.5*(ru(its,k,j)+ru(its+1,k,j)), 0. )
         tendency(its,k,j) = tendency(its,k,j)                     &
               - rdx*(                                             &
                       ub*(   field_old(its+1,k,j)                 &
                            - field_old(its  ,k,j)   ) +           &
                       field(its,k,j)*(ru(its+1,k,j)-ru(its,k,j))  &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_xe) .and. (ite == ide) ) THEN

       DO j = j_start, j_end
       DO k = kts, ktf
         ub = MAX( 0.5*(ru(ite-1,k,j)+ru(ite,k,j)), 0. )
         tendency(i_end,k,j) = tendency(i_end,k,j)                   &
               - rdx*(                                               &
                       ub*(  field_old(i_end  ,k,j)                  &
                           - field_old(i_end-1,k,j) ) +              &
                       field(i_end,k,j)*(ru(ite,k,j)-ru(ite-1,k,j))  &
                                                                    )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_ys) .and. (jts == jds) ) THEN

       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MIN( 0.5*(rv(i,k,jts)+rv(i,k,jts+1)), 0. )
         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(  field_old(i,k,jts+1)                  &
                           - field_old(i,k,jts  ) ) +              &
                       field(i,k,jts)*(rv(i,k,jts+1)-rv(i,k,jts))  &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_ye) .and. (jte == jde)) THEN

       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MAX( 0.5*(rv(i,k,jte-1)+rv(i,k,jte)), 0. )
         tendency(i,k,j_end) = tendency(i,k,j_end)                   &
               - rdy*(                                               &
                       vb*(   field_old(i,k,j_end  )                 &
                            - field_old(i,k,j_end-1) ) +             &
                       field(i,k,j_end)*(rv(i,k,jte)-rv(i,k,jte-1))  &
                                                                    )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%polar) .and. (jts == jds) ) THEN

       
       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MIN( 0.5*rv(i,k,jts+1), 0. )
         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(  field_old(i,k,jts+1)                  &
                           - field_old(i,k,jts  ) ) +              &
                       field(i,k,jts)*rv(i,k,jts+1)                &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%polar) .and. (jte == jde)) THEN

       
       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MAX( 0.5*rv(i,k,jte-1), 0. )
         tendency(i,k,j_end) = tendency(i,k,j_end)                   &
               - rdy*(                                               &
                       vb*(   field_old(i,k,j_end  )                 &
                            - field_old(i,k,j_end-1) ) +             &
                       field(i,k,j_end)*(-rv(i,k,jte-1))             &
                                                                    )
       ENDDO
       ENDDO

   ENDIF





      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1



    IF(degrade_xs) i_start = MAX(its-1,ids)
    IF(degrade_xe) i_end   = MIN(ite+1,ide-1)
    IF(degrade_ys) j_start = MAX(jts-1,jds)
    IF(degrade_ye) j_end   = MIN(jte+1,jde-1)

    vert_order_test : IF (vert_order == 6) THEN    

      DO j = j_start, j_end

         DO i = i_start, i_end
           fqz(i,1,j)  = 0.
           fqzl(i,1,j) = 0.
           fqz(i,kde,j)  = 0.
           fqzl(i,kde,j) = 0.
         ENDDO

         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)

           fqz(i,k,j) = vel*flux6( field(i,k-3,j), field(i,k-2,j), field(i,k-1,j),      &
                                   field(i,k  ,j), field(i,k+1,j), field(i,k+2,j),  -vel )
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)
           fqz(i,k,j)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

           k=kts+2
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)

           fqz(i,k,j) = vel*flux4(                      &
                   field(i,k-2,j), field(i,k-1,j),      &
                   field(i,k  ,j), field(i,k+1,j),  -vel )
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

           k=ktf-1
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)

           fqz(i,k,j) = vel*flux4(                      &
                   field(i,k-2,j), field(i,k-1,j),      &
                   field(i,k  ,j), field(i,k+1,j),  -vel )
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

           k=ktf
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)
           fqz(i,k,j)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

         ENDDO

      ENDDO

    ELSE IF (vert_order == 5) THEN    

      DO j = j_start, j_end

         DO i = i_start, i_end
           fqz(i,1,j)  = 0.
           fqzl(i,1,j) = 0.
           fqz(i,kde,j)  = 0.
           fqzl(i,kde,j) = 0.
         ENDDO

         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)

           fqz(i,k,j) = vel*flux5( field(i,k-3,j), field(i,k-2,j), field(i,k-1,j),      &
                                   field(i,k  ,j), field(i,k+1,j), field(i,k+2,j),  -vel )
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)
           fqz(i,k,j)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

           k=kts+2
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)

           fqz(i,k,j) = vel*flux3(                      &
                   field(i,k-2,j), field(i,k-1,j),      &
                   field(i,k  ,j), field(i,k+1,j),  -vel )
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

           k=ktf-1
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)

           fqz(i,k,j) = vel*flux3(                      &
                   field(i,k-2,j), field(i,k-1,j),      &
                   field(i,k  ,j), field(i,k+1,j),  -vel )
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

           k=ktf
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)
           fqz(i,k,j)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

         ENDDO

      ENDDO

    ELSE IF (vert_order == 4) THEN    

      DO j = j_start, j_end

         DO i = i_start, i_end
           fqz(i,1,j)  = 0.
           fqzl(i,1,j) = 0.
           fqz(i,kde,j)  = 0.
           fqzl(i,kde,j) = 0.
         ENDDO

         DO k=kts+2,ktf-1
         DO i = i_start, i_end

           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)

           fqz(i,k,j) = vel*flux4(                      &
                   field(i,k-2,j), field(i,k-1,j),      &
                   field(i,k  ,j), field(i,k+1,j),  -vel )
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)
           fqz(i,k,j)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

           k=ktf
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)
           fqz(i,k,j)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

         ENDDO

      ENDDO

    ELSE IF (vert_order == 3) THEN    

      DO j = j_start, j_end

         DO i = i_start, i_end
           fqz(i,1,j)  = 0.
           fqzl(i,1,j) = 0.
           fqz(i,kde,j)  = 0.
           fqzl(i,kde,j) = 0.
         ENDDO

         DO k=kts+2,ktf-1
!DEC$ vector always
         DO i = i_start, i_end

           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)

           fqz(i,k,j) = vel*flux3(                      &
                   field(i,k-2,j), field(i,k-1,j),      &
                   field(i,k  ,j), field(i,k+1,j),  -vel )
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)
           fqz(i,k,j)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

           k=ktf
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)
           fqz(i,k,j)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

         ENDDO

      ENDDO

   ELSE IF (vert_order == 2) THEN    

      DO j = j_start, j_end

         DO i = i_start, i_end
           fqz(i,1,j)  = 0.
           fqzl(i,1,j) = 0.
           fqz(i,kde,j)  = 0.
           fqzl(i,kde,j) = 0.
         ENDDO

         DO k=kts+1,ktf
         DO i = i_start, i_end

           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)
           fqz(i,k,j)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

        ENDDO
        ENDDO

      ENDDO

   ELSE

      WRITE (wrf_err_message,*) ' advect_scalar_pd, v_order not known ',vert_order
      CALL wrf_error_fatal3("<stdin>",7547,&
wrf_err_message )

   ENDIF vert_order_test

   IF (pd_limit) THEN



   i_start = its-1
   i_end   = MIN(ite,ide-1)+1
   j_start = jts-1
   j_end   = MIN(jte,jde-1)+1



   IF(degrade_xs) i_start = MAX(its-1,ids)
   IF(degrade_xe) i_end   = MIN(ite+1,ide-1)
   IF(degrade_ys) j_start = MAX(jts-1,jds)
   IF(degrade_ye) j_end   = MIN(jte+1,jde-1)

   IF(config_flags%specified .or. config_flags%nested) THEN
     IF (degrade_xs) i_start = MAX(its-1,ids+1)
     IF (degrade_xe) i_end   = MIN(ite+1,ide-2)
     IF (degrade_ys) j_start = MAX(jts-1,jds+1)
     IF (degrade_ye) j_end   = MIN(jte+1,jde-2)
   END IF

   IF(config_flags%open_xs) THEN
     IF (degrade_xs) i_start = MAX(its-1,ids+1)
   END IF
   IF(config_flags%open_xe) THEN
     IF (degrade_xe) i_end   = MIN(ite+1,ide-2)
   END IF
   IF(config_flags%open_ys) THEN
     IF (degrade_ys) j_start = MAX(jts-1,jds+1)
   END IF
   IF(config_flags%open_ye) THEN
     IF (degrade_ye) j_end   = MIN(jte+1,jde-2)
   END IF
   
   
   
   
   



   DO j=j_start, j_end
   DO k=kts, ktf
!DIR$ vector always
   DO i=i_start, i_end

     ph_low = (mub(i,j)+mu_old(i,j))*field_old(i,k,j)        &
                - dt*( msftx(i,j)*msfty(i,j)*(               &
                       rdx*(fqxl(i+1,k,j)-fqxl(i,k,j)) +     &
                       rdy*(fqyl(i,k,j+1)-fqyl(i,k,j))  )    &
                      +msfty(i,j)*rdzw(k)*(fqzl(i,k+1,j)-fqzl(i,k,j)) )

     flux_out = dt*( (msftx(i,j)*msfty(i,j))*(                    &
                                rdx*(  max(0.,fqx (i+1,k,j))      &
                                      -min(0.,fqx (i  ,k,j)) )    &
                               +rdy*(  max(0.,fqy (i,k,j+1))      &
                                      -min(0.,fqy (i,k,j  )) ) )  &
                +msfty(i,j)*rdzw(k)*(  min(0.,fqz (i,k+1,j))      &
                                      -max(0.,fqz (i,k  ,j)) )   )

     IF( flux_out .gt. ph_low ) THEN

       scale = max(0.,ph_low/(flux_out+eps))
       IF( fqx (i+1,k,j) .gt. 0.) fqx(i+1,k,j) = scale*fqx(i+1,k,j)
       IF( fqx (i  ,k,j) .lt. 0.) fqx(i  ,k,j) = scale*fqx(i  ,k,j)
       IF( fqy (i,k,j+1) .gt. 0.) fqy(i,k,j+1) = scale*fqy(i,k,j+1)
       IF( fqy (i,k,j  ) .lt. 0.) fqy(i,k,j  ) = scale*fqy(i,k,j  )


       IF( fqz (i,k+1,j) .lt. 0.) fqz(i,k+1,j) = scale*fqz(i,k+1,j)
       IF( fqz (i,k  ,j) .gt. 0.) fqz(i,k  ,j) = scale*fqz(i,k  ,j)

     END IF

   ENDDO
   ENDDO
   ENDDO

   END IF



  i_start = its
  i_end   = MIN(ite,ide-1)
  j_start = jts
  j_end   = MIN(jte,jde-1)

  DO j = j_start, j_end
  DO k = kts, ktf
!DEC$ vector always
  DO i = i_start, i_end

     tendency (i,k,j) = tendency(i,k,j)                           &
                            -rdzw(k)*( fqz (i,k+1,j)-fqz (i,k,j)  &
                                      +fqzl(i,k+1,j)-fqzl(i,k,j))

  ENDDO
  ENDDO
  ENDDO

  IF(tenddec) THEN
  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

     z_tendency (i,k,j) = 0. -rdzw(k)*( fqz (i,k+1,j)-fqz (i,k,j)  &
                                      +fqzl(i,k+1,j)-fqzl(i,k,j))

  ENDDO
  ENDDO
  ENDDO
  END IF



  IF(degrade_xs) i_start = MAX(its,ids+1)
  IF(degrade_xe) i_end   = MIN(ite,ide-2)

  DO j = j_start, j_end
  DO k = kts, ktf
!DEC$ vector always  
  DO i = i_start, i_end

     
     tendency (i,k,j) = tendency(i,k,j)                           &
               - msftx(i,j)*( rdx*( fqx (i+1,k,j)-fqx (i,k,j)     &
                                   +fqxl(i+1,k,j)-fqxl(i,k,j))   )

  ENDDO
  ENDDO
  ENDDO

  IF(tenddec) THEN
  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

     h_tendency (i,k,j) = 0.                                      &
               - msftx(i,j)*( rdx*( fqx (i+1,k,j)-fqx (i,k,j)     &
                                   +fqxl(i+1,k,j)-fqxl(i,k,j))   )

  ENDDO
  ENDDO
  ENDDO
  END IF



  i_start = its
  i_end   = MIN(ite,ide-1)
  IF(degrade_ys) j_start = MAX(jts,jds+1)
  IF(degrade_ye) j_end   = MIN(jte,jde-2)

  DO j = j_start, j_end
  DO k = kts, ktf
!DEC$ vector always
  DO i = i_start, i_end

     
     
     tendency (i,k,j) = tendency(i,k,j)                           &
               - msftx(i,j)*( rdy*( fqy (i,k,j+1)-fqy (i,k,j)     &
                                   +fqyl(i,k,j+1)-fqyl(i,k,j))   )

  ENDDO
  ENDDO
  ENDDO

  IF(tenddec) THEN
  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

     h_tendency (i,k,j) = h_tendency (i,k,j)                      &
               - msftx(i,j)*( rdy*( fqy (i,k,j+1)-fqy (i,k,j)     &
                                   +fqyl(i,k,j+1)-fqyl(i,k,j))   )

  ENDDO
  ENDDO
  ENDDO
  END IF

END SUBROUTINE advect_scalar_pd



SUBROUTINE advect_scalar_wenopd ( field, field_old, tendency,    &
                                ru, rv, rom,                   &
                                mut, mub, mu_old,              &
                                time_step, config_flags,       &
                                msfux, msfuy, msfvx, msfvy,    &
                                msftx, msfty,                  &
                                fzm, fzp,                      &
                                rdx, rdy, rdzw, dt,            &
                                ids, ide, jds, jde, kds, kde,  &
                                ims, ime, jms, jme, kms, kme,  &
                                its, ite, jts, jte, kts, kte  )



















   IMPLICIT NONE
   
   
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: field,     &
                                                                      field_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut, mub, mu_old
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy,  &
                                                                  dt
   INTEGER ,                                     INTENT(IN   ) :: time_step

   
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax

   REAL    :: mrdx, mrdy, ub, vb, uw, vw, mu



   REAL,  DIMENSION( its-1:ite+2, kts:kte, jts-1:jte+2  ) :: fqx, fqy, fqz
   REAL,  DIMENSION( its-1:ite+2, kts:kte, jts-1:jte+2  ) :: fqxl, fqyl, fqzl

   INTEGER :: horz_order, vert_order
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

   INTEGER :: jp1, jp0, jtmp

   REAL :: flux_out, ph_low, scale
   REAL, PARAMETER :: eps=1.e-20

    real            :: dir, vv
    real            :: ue,vs,vn,wb,wt
    real, parameter :: f30 =  7./12., f31 = 1./12.
    real, parameter :: f50 = 37./60., f51 = 2./15., f52 = 1./60.

    real               :: qim2, qim1, qi, qip1, qip2
    double precision               :: beta0, beta1, beta2, f0, f1, f2, wi0, wi1, wi2, sumwk
    double precision, parameter    :: gi0 = 1.d0/10.d0, gi1 = 6.d0/10.d0, gi2 = 3.d0/10.d0, eps1=1.0d-28
    integer, parameter :: pw = 2




   REAL    :: flux3, flux4, flux5, flux6, flux_upwind
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel, cr

      flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
            (7./12.)*(q_i + q_im1) - (1./12.)*(q_ip1 + q_im2)

      flux3(q_im2, q_im1, q_i, q_ip1, ua) =                     &
           flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
           sign(1,time_step)*sign(1.,ua)*(1./12.)*((q_ip1 - q_im2)-3.*(q_i-q_im1))

      flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
            (37./60.)*(q_i+q_im1) - (2./15.)*(q_ip1+q_im2)      &
            +(1./60.)*(q_ip2+q_im3)

      flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)    &
            -sign(1,time_step)*sign(1.,ua)*(1./60.)*(           &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )

      flux_upwind(q_im1, q_i, cr ) = 0.5*min( 1.0,(cr+abs(cr)))*q_im1 &
                                    +0.5*max(-1.0,(cr-abs(cr)))*q_i





    REAL     :: dx,dy,dz

    LOGICAL, PARAMETER :: pd_limit = .true.





    
    IF (config_flags%polar) THEN
       fqx(:,:,:)  = 0.
       fqy(:,:,:)  = 0.
       fqz(:,:,:)  = 0.
       fqxl(:,:,:) = 0.
       fqyl(:,:,:) = 0.
       fqzl(:,:,:) = 0.
    END IF

  ktf=MIN(kte,kde-1)
  horz_order = config_flags%h_sca_adv_order
  vert_order = config_flags%v_sca_adv_order








   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.









   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-4)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-4)                ) degrade_ye = .false.





      ktf=MIN(kte,kde-1)
      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1
      j_start_f = j_start
      j_end_f   = j_end+1





      IF(degrade_xs) i_start = MAX(its-1,ids)
      IF(degrade_xe) i_end   = MIN(ite+1,ide-1)

      IF(degrade_ys) then
        j_start = MAX(jts-1,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte+1,jde-2)
        j_end_f = jde-3
      ENDIF



      j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN 

        DO k=kts,ktf
        DO i = i_start, i_end

          dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
          mu = 0.5*(mut(i,j)+mut(i,j-1))
          vel = rv(i,k,j)
          cr = vel*dt/dy/mu
          fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

         IF ( vel .ge. 0.0 ) THEN
            qip2 = field(i,k,j+1)
            qip1 = field(i,k,j  )
            qi   = field(i,k,j-1)
            qim1 = field(i,k,j-2)
            qim2 = field(i,k,j-3)
          ELSE
            qip2 = field(i,k,j-2)
            qip1 = field(i,k,j-1)
            qi   = field(i,k,j  )
            qim1 = field(i,k,j+1)
            qim2 = field(i,k,j+2)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps1 + beta0)**pw
         wi1 = gi1 / (eps1 + beta1)**pw
         wi2 = gi2 / (eps1 + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
          fqy( i, k, j ) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk





          fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

        ENDDO
        ENDDO

      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy(i,k, j) = 0.5*rv(i,k,j)*          &
                     (field(i,k,j)+field(i,k,j-1))

              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy( i, k, j ) = vel*flux3(              &
                   field(i,k,j-2),field(i,k,j-1),field(i,k,j),field(i,k,j+1),vel )
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy(i, k, j ) = 0.5*rv(i,k,j)*      &
                     (field(i,k,j)+field(i,k,j-1))
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ELSE IF ( j == jde-2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              dy = 2./(msftx(i,j)+msftx(i,j-1))/rdy  
              mu = 0.5*(mut(i,j)+mut(i,j-1))
              vel = rv(i,k,j)
              cr = vel*dt/dy/mu
              fqyl(i,k,j) = mu*(dy/dt)*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy( i, k, j) = vel*flux3(             &
                   field(i,k,j-2),field(i,k,j-1),    &
                   field(i,k,j),field(i,k,j+1),vel )
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

            ENDDO
            ENDDO

      ENDIF

   ENDDO j_loop_y_flux_5





      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      i_start_f = i_start
      i_end_f   = i_end+1

      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1





      IF(degrade_ys) j_start = MAX(jts-1,jds)
      IF(degrade_ye) j_end   = MIN(jte+1,jde-1)

      IF(degrade_xs) then
        i_start = MAX(ids+1,its-1)
        i_start_f = ids+3
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite+1)
        i_end_f = ide-3
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f

          dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
          mu = 0.5*(mut(i,j)+mut(i-1,j))
          vel = ru(i,k,j)
          cr = vel*dt/dx/mu
          fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)


         IF ( vel .ge. 0.0 ) THEN
            qip2 = field(i+1,k,j)
            qip1 = field(i,  k,j)
            qi   = field(i-1,k,j)
            qim1 = field(i-2,k,j)
            qim2 = field(i-3,k,j)
          ELSE
            qip2 = field(i-2,k,j)
            qip1 = field(i-1,k,j)
            qi   = field(i,  k,j)
            qim1 = field(i+1,k,j)
            qim2 = field(i+2,k,j)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps1 + beta0)**pw
         wi1 = gi1 / (eps1 + beta1)**pw
         wi2 = gi2 / (eps1 + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
         fqx(i,k,j) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk





          fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

        ENDDO
        ENDDO



        IF( degrade_xs ) THEN

          DO i=i_start,i_start_f-1

            IF(i == ids+1) THEN 
              DO k=kts,ktf
                dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
                mu = 0.5*(mut(i,j)+mut(i-1,j))
                vel = ru(i,k,j)/mu
                cr = vel*dt/dx
                fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx(i,k,j) = 0.5*(ru(i,k,j)) &
                       *(field(i,k,j)+field(i-1,k,j))
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)
              ENDDO
            ENDIF

            IF(i == ids+2) THEN  
              DO k=kts,ktf
                dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
                mu = 0.5*(mut(i,j)+mut(i-1,j))
                vel = ru(i,k,j)
                cr = vel*dt/dx/mu
                fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx( i,k,j ) = vel*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                          field(i  ,k,j), field(i+1,k,j),  &
                                          vel                             )
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)
              ENDDO
            ENDIF

          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          DO i = i_end_f+1, i_end+1

            IF( i == ide-1 ) THEN 
              DO k=kts,ktf
                dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
                mu = 0.5*(mut(i,j)+mut(i-1,j))
                vel = ru(i,k,j)
                cr = vel*dt/dx/mu
                fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx(i,k,j) = 0.5*(ru(i,k,j))      &
                       *(field(i,k,j)+field(i-1,k,j))
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)
              ENDDO
            ENDIF


            IF( i == ide-2 ) THEN 
              DO k=kts,ktf
                dx = 2./(msfty(i,j)+msfty(i-1,j))/rdx  
                mu = 0.5*(mut(i,j)+mut(i-1,j))
                vel = ru(i,k,j)
                cr = vel*dt/dx/mu
                fqxl(i,k,j) = mu*(dx/dt)*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx( i,k,j ) = vel*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                          field(i  ,k,j), field(i+1,k,j),  &
                                          vel                             )
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)
              ENDDO
            ENDIF

          ENDDO

        ENDIF

      ENDDO  














      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)



   IF( (config_flags%open_xs) .and. (its == ids) ) THEN

       DO j = j_start, j_end
       DO k = kts, ktf
         ub = MIN( 0.5*(ru(its,k,j)+ru(its+1,k,j)), 0. )
         tendency(its,k,j) = tendency(its,k,j)                     &
               - rdx*(                                             &
                       ub*(   field_old(its+1,k,j)                 &
                            - field_old(its  ,k,j)   ) +           &
                       field(its,k,j)*(ru(its+1,k,j)-ru(its,k,j))  &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_xe) .and. (ite == ide) ) THEN

       DO j = j_start, j_end
       DO k = kts, ktf
         ub = MAX( 0.5*(ru(ite-1,k,j)+ru(ite,k,j)), 0. )
         tendency(i_end,k,j) = tendency(i_end,k,j)                   &
               - rdx*(                                               &
                       ub*(  field_old(i_end  ,k,j)                  &
                           - field_old(i_end-1,k,j) ) +              &
                       field(i_end,k,j)*(ru(ite,k,j)-ru(ite-1,k,j))  &
                                                                    )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_ys) .and. (jts == jds) ) THEN

       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MIN( 0.5*(rv(i,k,jts)+rv(i,k,jts+1)), 0. )
         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(  field_old(i,k,jts+1)                  &
                           - field_old(i,k,jts  ) ) +              &
                       field(i,k,jts)*(rv(i,k,jts+1)-rv(i,k,jts))  &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_ye) .and. (jte == jde)) THEN

       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MAX( 0.5*(rv(i,k,jte-1)+rv(i,k,jte)), 0. )
         tendency(i,k,j_end) = tendency(i,k,j_end)                   &
               - rdy*(                                               &
                       vb*(   field_old(i,k,j_end  )                 &
                            - field_old(i,k,j_end-1) ) +             &
                       field(i,k,j_end)*(rv(i,k,jte)-rv(i,k,jte-1))  &
                                                                    )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%polar) .and. (jts == jds) ) THEN

       
       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MIN( 0.5*rv(i,k,jts+1), 0. )
         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(  field_old(i,k,jts+1)                  &
                           - field_old(i,k,jts  ) ) +              &
                       field(i,k,jts)*rv(i,k,jts+1)                &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%polar) .and. (jte == jde)) THEN

       
       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MAX( 0.5*rv(i,k,jte-1), 0. )
         tendency(i,k,j_end) = tendency(i,k,j_end)                   &
               - rdy*(                                               &
                       vb*(   field_old(i,k,j_end  )                 &
                            - field_old(i,k,j_end-1) ) +             &
                       field(i,k,j_end)*(-rv(i,k,jte-1))             &
                                                                    )
       ENDDO
       ENDDO

   ENDIF





      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1



    IF(degrade_xs) i_start = MAX(its-1,ids)
    IF(degrade_xe) i_end   = MIN(ite+1,ide-1)
    IF(degrade_ys) j_start = MAX(jts-1,jds)
    IF(degrade_ye) j_end   = MIN(jte+1,jde-1)






      DO j = j_start, j_end

         DO i = i_start, i_end
           fqz(i,1,j)  = 0.
           fqzl(i,1,j) = 0.
           fqz(i,kde,j)  = 0.
           fqzl(i,kde,j) = 0.
         ENDDO

         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)


         IF( -vel .ge. 0.0 ) THEN
            qip2 = field(i,k+1,j)
            qip1 = field(i,k  ,j)
            qi   = field(i,k-1,j)
            qim1 = field(i,k-2,j)
            qim2 = field(i,k-3,j)
          ELSE
            qip2 = field(i,k-2,j)
            qip1 = field(i,k-1,j)
            qi   = field(i,k  ,j)
            qim1 = field(i,k+1,j)
            qim2 = field(i,k+2,j)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps1 + beta0)**pw
         wi1 = gi1 / (eps1 + beta1)**pw
         wi2 = gi2 / (eps1 + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
          fqz(i,k,j) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk



           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)
           fqz(i,k,j)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

           k=kts+2
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)

           fqz(i,k,j) = vel*flux3(                      &
                   field(i,k-2,j), field(i,k-1,j),      &
                   field(i,k  ,j), field(i,k+1,j),  -vel )
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

           k=ktf-1
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)

           fqz(i,k,j) = vel*flux3(                      &
                   field(i,k-2,j), field(i,k-1,j),      &
                   field(i,k  ,j), field(i,k+1,j),  -vel )
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

           k=ktf
           dz = 2./(rdzw(k)+rdzw(k-1))
           mu = 0.5*(mut(i,j)+mut(i,j))
           vel = rom(i,k,j)
           cr = vel*dt/dz/mu
           fqzl(i,k,j) = mu*(dz/dt)*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)
           fqz(i,k,j)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

         ENDDO

      ENDDO









   IF (pd_limit) THEN



   i_start = its-1
   i_end   = MIN(ite,ide-1)+1
   j_start = jts-1
   j_end   = MIN(jte,jde-1)+1



   IF(degrade_xs) i_start = MAX(its-1,ids)
   IF(degrade_xe) i_end   = MIN(ite+1,ide-1)
   IF(degrade_ys) j_start = MAX(jts-1,jds)
   IF(degrade_ye) j_end   = MIN(jte+1,jde-1)

   IF(config_flags%specified .or. config_flags%nested) THEN
     IF (degrade_xs) i_start = MAX(its-1,ids+1)
     IF (degrade_xe) i_end   = MIN(ite+1,ide-2)
     IF (degrade_ys) j_start = MAX(jts-1,jds+1)
     IF (degrade_ye) j_end   = MIN(jte+1,jde-2)
   END IF

   IF(config_flags%open_xs) THEN
     IF (degrade_xs) i_start = MAX(its-1,ids+1)
   END IF
   IF(config_flags%open_xe) THEN
     IF (degrade_xe) i_end   = MIN(ite+1,ide-2)
   END IF
   IF(config_flags%open_ys) THEN
     IF (degrade_ys) j_start = MAX(jts-1,jds+1)
   END IF
   IF(config_flags%open_ye) THEN
     IF (degrade_ye) j_end   = MIN(jte+1,jde-2)
   END IF
   
   
   
   
   



   DO j=j_start, j_end
   DO k=kts, ktf
   DO i=i_start, i_end

     ph_low = (mub(i,j)+mu_old(i,j))*field_old(i,k,j)        &
                - dt*( msftx(i,j)*msfty(i,j)*(               &
                       rdx*(fqxl(i+1,k,j)-fqxl(i,k,j)) +     &
                       rdy*(fqyl(i,k,j+1)-fqyl(i,k,j))  )    &
                      +msfty(i,j)*rdzw(k)*(fqzl(i,k+1,j)-fqzl(i,k,j)) )

     flux_out = dt*( (msftx(i,j)*msfty(i,j))*(                    &
                                rdx*(  max(0.,fqx (i+1,k,j))      &
                                      -min(0.,fqx (i  ,k,j)) )    &
                               +rdy*(  max(0.,fqy (i,k,j+1))      &
                                      -min(0.,fqy (i,k,j  )) ) )  &
                +msfty(i,j)*rdzw(k)*(  min(0.,fqz (i,k+1,j))      &
                                      -max(0.,fqz (i,k  ,j)) )   )

     IF( flux_out .gt. ph_low ) THEN

       scale = max(0.,ph_low/(flux_out+eps))
       IF( fqx (i+1,k,j) .gt. 0.) fqx(i+1,k,j) = scale*fqx(i+1,k,j)
       IF( fqx (i  ,k,j) .lt. 0.) fqx(i  ,k,j) = scale*fqx(i  ,k,j)
       IF( fqy (i,k,j+1) .gt. 0.) fqy(i,k,j+1) = scale*fqy(i,k,j+1)
       IF( fqy (i,k,j  ) .lt. 0.) fqy(i,k,j  ) = scale*fqy(i,k,j  )


       IF( fqz (i,k+1,j) .lt. 0.) fqz(i,k+1,j) = scale*fqz(i,k+1,j)
       IF( fqz (i,k  ,j) .gt. 0.) fqz(i,k  ,j) = scale*fqz(i,k  ,j)

     END IF

   ENDDO
   ENDDO
   ENDDO

   END IF



  i_start = its
  i_end   = MIN(ite,ide-1)
  j_start = jts
  j_end   = MIN(jte,jde-1)

  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

     tendency (i,k,j) = tendency(i,k,j)                           &
                            -rdzw(k)*( fqz (i,k+1,j)-fqz (i,k,j)  &
                                      +fqzl(i,k+1,j)-fqzl(i,k,j))

  ENDDO
  ENDDO
  ENDDO



  IF(degrade_xs) i_start = MAX(its,ids+1)
  IF(degrade_xe) i_end   = MIN(ite,ide-2)

  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

     
     tendency (i,k,j) = tendency(i,k,j)                           &
               - msftx(i,j)*( rdx*( fqx (i+1,k,j)-fqx (i,k,j)     &
                                   +fqxl(i+1,k,j)-fqxl(i,k,j))   )

  ENDDO
  ENDDO
  ENDDO



  i_start = its
  i_end   = MIN(ite,ide-1)
  IF(degrade_ys) j_start = MAX(jts,jds+1)
  IF(degrade_ye) j_end   = MIN(jte,jde-2)

  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

     
     
     tendency (i,k,j) = tendency(i,k,j)                           &
               - msftx(i,j)*( rdy*( fqy (i,k,j+1)-fqy (i,k,j)     &
                                   +fqyl(i,k,j+1)-fqyl(i,k,j))   )

  ENDDO
  ENDDO
  ENDDO

END SUBROUTINE advect_scalar_wenopd



SUBROUTINE advect_scalar_mono   ( field, field_old, tendency,    &
                                  h_tendency, z_tendency,        & 
                                  ru, rv, rom,                   &
                                  mut, mub, mu_old,              &
                                  config_flags,                  &
                                  tenddec,                       & 
                                  msfux, msfuy, msfvx, msfvy,    &
                                  msftx, msfty,                  &
                                  fzm, fzp,                      &
                                  rdx, rdy, rdzw, dt,            &
                                  ids, ide, jds, jde, kds, kde,  &
                                  ims, ime, jms, jme, kms, kme,  &
                                  its, ite, jts, jte, kts, kte  )










   IMPLICIT NONE
   
   
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   LOGICAL ,                 INTENT(IN   ) :: tenddec 

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: field,     &
                                                                      field_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut, mub, mu_old
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(  OUT) :: h_tendency, z_tendency 

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy,  &
                                                                  dt

   
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax

   REAL    :: mrdx, mrdy, ub, vb, uw, vw, mu
   REAL , DIMENSION(its:ite, kts:kte) :: vflux




   REAL,  DIMENSION( its-2:ite+2, kts:kte, jts-2:jte+2  ) :: fqx, fqy, fqz
   REAL,  DIMENSION( its-2:ite+2, kts:kte, jts-2:jte+2  ) :: fqxl, fqyl, fqzl
   REAL,  DIMENSION( its-2:ite+2, kts:kte, jts-2:jte+2  ) :: qmin, qmax
   REAL,  DIMENSION( its-2:ite+2, kts:kte, jts-2:jte+2  ) :: scale_in, scale_out
   REAL :: ph_upwind

   INTEGER :: horz_order, vert_order
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

   INTEGER :: jp1, jp0, jtmp

   REAL :: flux_out, ph_low, flux_in, ph_hi, scale
   REAL, PARAMETER :: eps=1.e-20




   REAL    :: flux3, flux4, flux5, flux6, flux_upwind
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel, cr

      flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
            (7./12.)*(q_i + q_im1) - (1./12.)*(q_ip1 + q_im2)

      flux3(q_im2, q_im1, q_i, q_ip1, ua) =                     &
           flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
           sign(1.,ua)*(1./12.)*((q_ip1 - q_im2)-3.*(q_i-q_im1))

      flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
            (37./60.)*(q_i+q_im1) - (2./15.)*(q_ip1+q_im2)      &
            +(1./60.)*(q_ip2+q_im3)

      flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)    &
            -sign(1.,ua)*(1./60.)*(                             &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )


      flux_upwind(q_im1, q_i, cr ) = 0.5*(1.+sign(1.,cr))*q_im1 &
                                    +0.5*(1.-sign(1.,cr))*q_i

    LOGICAL, PARAMETER :: mono_limit = .true.



  ktf=MIN(kte,kde-1)
  horz_order = config_flags%h_sca_adv_order
  vert_order = config_flags%v_sca_adv_order

  do j=jts-2,jte+2
  do k=kts,kte
  do i=its-2,ite+2
    qmin(i,k,j) = field_old(i,k,j)
    qmax(i,k,j) = field_old(i,k,j)
    scale_in(i,k,j) = 1.
    scale_out(i,k,j) = 1.
    fqx(i,k,j) = 0.
    fqy(i,k,j) = 0.
    fqz(i,k,j) = 0.
    fqxl(i,k,j) = 0.
    fqyl(i,k,j) = 0.
    fqzl(i,k,j) = 0.
  enddo
  enddo
  enddo





  horizontal_order_test : IF( horz_order == 5 ) THEN








   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-4)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-4)                ) degrade_ye = .false.





      ktf=MIN(kte,kde-1)
      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1
      j_start_f = j_start
      j_end_f   = j_end+1






      IF(degrade_xs) i_start = MAX(its-1,ids)
      IF(degrade_xe) i_end   = MIN(ite+1,ide-1)












      IF(degrade_ys) then
        j_start = MAX(jts-1,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte+1,jde-2)
        j_end_f = jde-3
      ENDIF



      j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN 

        DO k=kts,ktf
        DO i = i_start, i_end

          vel = rv(i,k,j)
          cr = vel
          fqyl(i,k,j) = vel*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), vel)

          fqy( i, k, j  ) = vel*flux5(                                  &
                  field(i,k,j-3), field(i,k,j-2), field(i,k,j-1),       &
                  field(i,k,j  ), field(i,k,j+1), field(i,k,j+2),  vel )

          fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

          if(cr.gt. 0) then
             qmax(i,k,j)  = amax1(qmax(i,k,j),field_old(i,k,j-1))
             qmin(i,k,j)  = amin1(qmin(i,k,j),field_old(i,k,j-1))
          else
             qmax(i,k,j-1)  = amax1(qmax(i,k,j-1),field_old(i,k,j))
             qmin(i,k,j-1)  = amin1(qmin(i,k,j-1),field_old(i,k,j))
          end if

        ENDDO
        ENDDO

      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end

              vel = rv(i,k,j)
              cr = vel
              fqyl(i,k,j) = vel*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy(i,k, j) = 0.5*rv(i,k,j)*          &
                     (field(i,k,j)+field(i,k,j-1))

              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

          if(cr.gt. 0) then
             qmax(i,k,j)  = amax1(qmax(i,k,j),field_old(i,k,j-1))
             qmin(i,k,j)  = amin1(qmin(i,k,j),field_old(i,k,j-1))
          else
             qmax(i,k,j-1)  = amax1(qmax(i,k,j-1),field_old(i,k,j))
             qmin(i,k,j-1)  = amin1(qmin(i,k,j-1),field_old(i,k,j))
          end if

            ENDDO
            ENDDO

      ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              vel = rv(i,k,j)
              cr = vel
              fqyl(i,k,j) = vel*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy( i, k, j ) = vel*flux3(              &
                   field(i,k,j-2),field(i,k,j-1),field(i,k,j),field(i,k,j+1),vel )
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

          if(cr.gt. 0) then
             qmax(i,k,j)  = amax1(qmax(i,k,j),field_old(i,k,j-1))
             qmin(i,k,j)  = amin1(qmin(i,k,j),field_old(i,k,j-1))
          else
             qmax(i,k,j-1)  = amax1(qmax(i,k,j-1),field_old(i,k,j))
             qmin(i,k,j-1)  = amin1(qmin(i,k,j-1),field_old(i,k,j))
          end if

            ENDDO
            ENDDO

      ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              vel = rv(i,k,j)
              cr = vel
              fqyl(i,k,j) = vel*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy(i, k, j ) = 0.5*rv(i,k,j)*      &
                     (field(i,k,j)+field(i,k,j-1))
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

          if(cr.gt. 0) then
             qmax(i,k,j)  = amax1(qmax(i,k,j),field_old(i,k,j-1))
             qmin(i,k,j)  = amin1(qmin(i,k,j),field_old(i,k,j-1))
          else
             qmax(i,k,j-1)  = amax1(qmax(i,k,j-1),field_old(i,k,j))
             qmin(i,k,j-1)  = amin1(qmin(i,k,j-1),field_old(i,k,j))
          end if

            ENDDO
            ENDDO

      ELSE IF ( j == jde-2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              vel = rv(i,k,j)
              cr = vel
              fqyl(i,k,j) = vel*flux_upwind(field_old(i,k,j-1), field_old(i,k,j  ), cr)

              fqy( i, k, j) = vel*flux3(             &
                   field(i,k,j-2),field(i,k,j-1),    &
                   field(i,k,j),field(i,k,j+1),vel )
              fqy(i,k,j) = fqy(i,k,j) - fqyl(i,k,j)

          if(cr.gt. 0) then
             qmax(i,k,j)  = amax1(qmax(i,k,j),field_old(i,k,j-1))
             qmin(i,k,j)  = amin1(qmin(i,k,j),field_old(i,k,j-1))
          else
             qmax(i,k,j-1)  = amax1(qmax(i,k,j-1),field_old(i,k,j))
             qmin(i,k,j-1)  = amin1(qmin(i,k,j-1),field_old(i,k,j))
          end if

            ENDDO
            ENDDO

      ENDIF

   ENDDO j_loop_y_flux_5





      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      i_start_f = i_start
      i_end_f   = i_end+1

      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1






      IF(degrade_ys) j_start = MAX(jts-1,jds)
      IF(degrade_ye) j_end   = MIN(jte+1,jde-1)












      IF(degrade_xs) then
        i_start = MAX(ids+1,its-1)
        i_start_f = ids+3
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite+1)
        i_end_f = ide-3
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f

          vel = ru(i,k,j)
          cr = vel
          fqxl(i,k,j) = vel*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)

          fqx( i,k,j ) = vel*flux5( field(i-3,k,j), field(i-2,k,j),  &
                                         field(i-1,k,j), field(i  ,k,j),  &
                                         field(i+1,k,j), field(i+2,k,j),  &
                                         vel                             )
          fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

          if(cr.gt. 0) then
             qmax(i,k,j)  = amax1(qmax(i,k,j),field_old(i-1,k,j))
             qmin(i,k,j)  = amin1(qmin(i,k,j),field_old(i-1,k,j))
          else
             qmax(i-1,k,j)  = amax1(qmax(i-1,k,j),field_old(i,k,j))
             qmin(i-1,k,j)  = amin1(qmin(i-1,k,j),field_old(i,k,j))
          end if

        ENDDO
        ENDDO





        IF( degrade_xs ) THEN

          DO i=i_start,i_start_f-1

            IF(i == ids+1) THEN 
              DO k=kts,ktf
                vel = ru(i,k,j)
                cr = vel
                fqxl(i,k,j) = vel*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)

                fqx(i,k,j) = 0.5*(ru(i,k,j)) &
                       *(field(i,k,j)+field(i-1,k,j))

                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

                if(cr.gt. 0) then
                  qmax(i,k,j)  = amax1(qmax(i,k,j),field_old(i-1,k,j))
                  qmin(i,k,j)  = amin1(qmin(i,k,j),field_old(i-1,k,j))
                else
                  qmax(i-1,k,j)  = amax1(qmax(i-1,k,j),field_old(i,k,j))
                  qmin(i-1,k,j)  = amin1(qmin(i-1,k,j),field_old(i,k,j))
                end if
              ENDDO
            ENDIF

            IF(i == ids+2) THEN  
              DO k=kts,ktf
                vel = ru(i,k,j)
                cr = vel
                fqxl(i,k,j) = vel*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx( i,k,j ) = vel*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                          field(i  ,k,j), field(i+1,k,j),  &
                                          vel                             )
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

                if(cr.gt. 0) then
                  qmax(i,k,j)  = amax1(qmax(i,k,j),field_old(i-1,k,j))
                  qmin(i,k,j)  = amin1(qmin(i,k,j),field_old(i-1,k,j))
                else
                  qmax(i-1,k,j)  = amax1(qmax(i-1,k,j),field_old(i,k,j))
                  qmin(i-1,k,j)  = amin1(qmin(i-1,k,j),field_old(i,k,j))
                end if
              ENDDO
            ENDIF

          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          DO i = i_end_f+1, i_end+1

            IF( i == ide-1 ) THEN 
              DO k=kts,ktf
                vel = ru(i,k,j)
                cr = vel
                fqxl(i,k,j) = vel*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx(i,k,j) = 0.5*(ru(i,k,j))      &
                       *(field(i,k,j)+field(i-1,k,j))
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

                if(cr.gt. 0) then
                  qmax(i,k,j)  = amax1(qmax(i,k,j),field_old(i-1,k,j))
                  qmin(i,k,j)  = amin1(qmin(i,k,j),field_old(i-1,k,j))
                else
                  qmax(i-1,k,j)  = amax1(qmax(i-1,k,j),field_old(i,k,j))
                  qmin(i-1,k,j)  = amin1(qmin(i-1,k,j),field_old(i,k,j))
                end if
              ENDDO
            ENDIF

            IF( i == ide-2 ) THEN 
              DO k=kts,ktf
                vel = ru(i,k,j)
                cr = vel
                fqxl(i,k,j) = vel*flux_upwind(field_old(i-1,k,j), field_old(i,k,j  ), cr)
                fqx( i,k,j ) = vel*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                          field(i  ,k,j), field(i+1,k,j),  &
                                          vel                             )
                fqx(i,k,j) = fqx(i,k,j) - fqxl(i,k,j)

                if(cr.gt. 0) then
                  qmax(i,k,j)  = amax1(qmax(i,k,j),field_old(i-1,k,j))
                  qmin(i,k,j)  = amin1(qmin(i,k,j),field_old(i-1,k,j))
                else
                  qmax(i-1,k,j)  = amax1(qmax(i-1,k,j),field_old(i,k,j))
                  qmin(i-1,k,j)  = amin1(qmin(i-1,k,j),field_old(i,k,j))
                end if
              ENDDO
            ENDIF
          ENDDO
        ENDIF

      ENDDO  

   ELSE

      WRITE ( wrf_err_message , * ) 'module_advect: advect_scalar_mono, h_order not known ',horz_order
      CALL wrf_error_fatal3("<stdin>",9153,&
TRIM( wrf_err_message ) )

   ENDIF horizontal_order_test





      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)



   IF( (config_flags%open_xs) .and. (its == ids) ) THEN

       DO j = j_start, j_end
       DO k = kts, ktf
         ub = MIN( 0.5*(ru(its,k,j)+ru(its+1,k,j)), 0. )
         tendency(its,k,j) = tendency(its,k,j)                     &
               - rdx*(                                             &
                       ub*(   field_old(its+1,k,j)                 &
                            - field_old(its  ,k,j)   ) +           &
                       field(its,k,j)*(ru(its+1,k,j)-ru(its,k,j))  &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_xe) .and. (ite == ide) ) THEN

       DO j = j_start, j_end
       DO k = kts, ktf
         ub = MAX( 0.5*(ru(ite-1,k,j)+ru(ite,k,j)), 0. )
         tendency(i_end,k,j) = tendency(i_end,k,j)                   &
               - rdx*(                                               &
                       ub*(  field_old(i_end  ,k,j)                  &
                           - field_old(i_end-1,k,j) ) +              &
                       field(i_end,k,j)*(ru(ite,k,j)-ru(ite-1,k,j))  &
                                                                    )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_ys) .and. (jts == jds) ) THEN

       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MIN( 0.5*(rv(i,k,jts)+rv(i,k,jts+1)), 0. )
         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(  field_old(i,k,jts+1)                  &
                           - field_old(i,k,jts  ) ) +              &
                       field(i,k,jts)*(rv(i,k,jts+1)-rv(i,k,jts))  &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_ye) .and. (jte == jde)) THEN

       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MAX( 0.5*(rv(i,k,jte-1)+rv(i,k,jte)), 0. )
         tendency(i,k,j_end) = tendency(i,k,j_end)                   &
               - rdy*(                                               &
                       vb*(   field_old(i,k,j_end  )                 &
                            - field_old(i,k,j_end-1) ) +             &
                       field(i,k,j_end)*(rv(i,k,jte)-rv(i,k,jte-1))  &
                                                                    )
       ENDDO
       ENDDO

   ENDIF





      i_start = its-1
      i_end   = MIN(ite,ide-1)+1
      j_start = jts-1
      j_end   = MIN(jte,jde-1)+1









    IF(degrade_xs) i_start = MAX(its-1,ids)
    IF(degrade_xe) i_end   = MIN(ite+1,ide-1)
    IF(degrade_ys) j_start = MAX(jts-1,jds)
    IF(degrade_ye) j_end   = MIN(jte+1,jde-1)


    vert_order_test : IF (vert_order == 3) THEN    

      DO j = j_start, j_end

         DO i = i_start, i_end
           fqz(i,1,j)  = 0.
           fqzl(i,1,j) = 0.
           fqz(i,kde,j)  = 0.
           fqzl(i,kde,j) = 0.
         ENDDO

         DO k=kts+2,ktf-1
         DO i = i_start, i_end

           vel = rom(i,k,j)
           cr = -vel
           fqzl(i,k,j) = vel*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)

           fqz(i,k,j) = vel*flux3(                      &
                   field(i,k-2,j), field(i,k-1,j),      &
                   field(i,k  ,j), field(i,k+1,j),  -vel )
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

          if(cr.gt. 0) then
             qmax(i,k,j)  = amax1(qmax(i,k,j),field_old(i,k-1,j))
             qmin(i,k,j)  = amin1(qmin(i,k,j),field_old(i,k-1,j))
          else
             qmax(i,k-1,j)  = amax1(qmax(i,k-1,j),field_old(i,k,j))
             qmin(i,k-1,j)  = amin1(qmin(i,k-1,j),field_old(i,k,j))
          end if

         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vel = rom(i,k,j)
           cr = -vel
           fqzl(i,k,j) = vel*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)
           fqz(i,k,j)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

          if(cr.gt. 0) then
             qmax(i,k,j)  = amax1(qmax(i,k,j),field_old(i,k-1,j))
             qmin(i,k,j)  = amin1(qmin(i,k,j),field_old(i,k-1,j))
          else
             qmax(i,k-1,j)  = amax1(qmax(i,k-1,j),field_old(i,k,j))
             qmin(i,k-1,j)  = amin1(qmin(i,k-1,j),field_old(i,k,j))
          end if

           k=ktf
           vel = rom(i,k,j)
           cr = -vel
           fqzl(i,k,j) = vel*flux_upwind(field_old(i,k-1,j), field_old(i,k,j  ), cr)
           fqz(i,k,j)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           fqz(i,k,j) = fqz(i,k,j) - fqzl(i,k,j)

          if(cr.gt. 0) then
             qmax(i,k,j)  = amax1(qmax(i,k,j),field_old(i,k-1,j))
             qmin(i,k,j)  = amin1(qmin(i,k,j),field_old(i,k-1,j))
          else
             qmax(i,k-1,j)  = amax1(qmax(i,k-1,j),field_old(i,k,j))
             qmin(i,k-1,j)  = amin1(qmin(i,k-1,j),field_old(i,k,j))
          end if
         ENDDO

      ENDDO

   ELSE

      WRITE (wrf_err_message,*) ' advect_scalar_mono, v_order not known ',vert_order
      CALL wrf_error_fatal3("<stdin>",9328,&
wrf_err_message )

   ENDIF vert_order_test

   IF (mono_limit) THEN



   i_start = its-1
   i_end   = MIN(ite,ide-1)+1
   j_start = jts-1
   j_end   = MIN(jte,jde-1)+1






























   IF(degrade_xs) i_start = MAX(its-1,ids)
   IF(degrade_xe) i_end   = MIN(ite+1,ide-1)
   IF(degrade_ys) j_start = MAX(jts-1,jds)
   IF(degrade_ye) j_end   = MIN(jte+1,jde-1)

   IF(config_flags%specified .or. config_flags%nested) THEN
     IF (degrade_xs) i_start = MAX(its-1,ids+1)
     IF (degrade_xe) i_end   = MIN(ite+1,ide-2)
     IF (degrade_ys) j_start = MAX(jts-1,jds+1)
     IF (degrade_ye) j_end   = MIN(jte+1,jde-2)
   END IF

   IF(config_flags%open_xs) THEN
     IF (degrade_xs) i_start = MAX(its-1,ids+1)
   END IF
   IF(config_flags%open_xe) THEN
     IF (degrade_xe) i_end   = MIN(ite+1,ide-2)
   END IF
   IF(config_flags%open_ys) THEN
     IF (degrade_ys) j_start = MAX(jts-1,jds+1)
   END IF
   IF(config_flags%open_ye) THEN
     IF (degrade_ye) j_end   = MIN(jte+1,jde-2)
   END IF



   DO j=j_start, j_end
   DO k=kts, ktf
   DO i=i_start, i_end

     ph_upwind = (mub(i,j)+mu_old(i,j))*field_old(i,k,j)        &
                   - dt*( msftx(i,j)*msfty(i,j)*(               &
                          rdx*(fqxl(i+1,k,j)-fqxl(i,k,j)) +     &
                          rdy*(fqyl(i,k,j+1)-fqyl(i,k,j))  )    &
                         +msfty(i,j)*rdzw(k)*(fqzl(i,k+1,j)-fqzl(i,k,j)) )

     flux_in = -dt*( (msftx(i,j)*msfty(i,j))*(                   &
                               rdx*(  min(0.,fqx (i+1,k,j))      &
                                     -max(0.,fqx (i  ,k,j)) )    &
                              +rdy*(  min(0.,fqy (i,k,j+1))      &
                                     -max(0.,fqy (i,k,j  )) ) )  &
               +msfty(i,j)*rdzw(k)*(  max(0.,fqz (i,k+1,j))      &
                                     -min(0.,fqz (i,k  ,j)) )   )

     ph_hi = mut(i,j)*qmax(i,k,j) - ph_upwind
     IF( flux_in .gt. ph_hi ) scale_in(i,k,j) = max(0.,ph_hi/(flux_in+eps))


     flux_out = dt*( (msftx(i,j)*msfty(i,j))*(                    &
                                rdx*(  max(0.,fqx (i+1,k,j))      &
                                      -min(0.,fqx (i  ,k,j)) )    &
                               +rdy*(  max(0.,fqy (i,k,j+1))      &
                                      -min(0.,fqy (i,k,j  )) ) )  &
                +msfty(i,j)*rdzw(k)*(  min(0.,fqz (i,k+1,j))      &
                                      -max(0.,fqz (i,k  ,j)) )   )

     ph_low = ph_upwind - mut(i,j)*qmin(i,k,j)
     IF( flux_out .gt. ph_low ) scale_out(i,k,j) = max(0.,ph_low/(flux_out+eps))

   ENDDO
   ENDDO
   ENDDO

   DO j=j_start, j_end
   DO k=kts, ktf
   DO i=i_start, i_end+1
       IF( fqx (i,k,j) .gt. 0.) then
         fqx(i,k,j) = min(scale_in(i,k,j),scale_out(i-1,k,j))*fqx(i,k,j)
       ELSE
         fqx(i,k,j) = min(scale_out(i,k,j),scale_in(i-1,k,j))*fqx(i,k,j)
       ENDIF
   ENDDO
   ENDDO
   ENDDO

   DO j=j_start, j_end+1
   DO k=kts, ktf
   DO i=i_start, i_end
       IF( fqy (i,k,j) .gt. 0.) then
         fqy(i,k,j) = min(scale_in(i,k,j),scale_out(i,k,j-1))*fqy(i,k,j)
       ELSE
         fqy(i,k,j) = min(scale_out(i,k,j),scale_in(i,k,j-1))*fqy(i,k,j)
       ENDIF
   ENDDO
   ENDDO
   ENDDO

   DO j=j_start, j_end
   DO k=kts+1, ktf
   DO i=i_start, i_end
       IF( fqz (i,k,j) .lt. 0.) then
         fqz(i,k,j) = min(scale_in(i,k,j),scale_out(i,k-1,j))*fqz(i,k,j)
       ELSE
         fqz(i,k,j) = min(scale_out(i,k,j),scale_in(i,k-1,j))*fqz(i,k,j)
       ENDIF
   ENDDO
   ENDDO
   ENDDO

   END IF




  i_start = its
  i_end   = MIN(ite,ide-1)
  j_start = jts
  j_end   = MIN(jte,jde-1)

  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

     tendency (i,k,j) = tendency(i,k,j)                           &
                            -rdzw(k)*( fqz (i,k+1,j)-fqz (i,k,j)  &
                                      +fqzl(i,k+1,j)-fqzl(i,k,j))

  ENDDO
  ENDDO
  ENDDO

  IF(tenddec) THEN
  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

     z_tendency (i,k,j) = 0. -rdzw(k)*( fqz (i,k+1,j)-fqz (i,k,j)  &
                                      +fqzl(i,k+1,j)-fqzl(i,k,j))

  ENDDO
  ENDDO
  ENDDO
  END IF








  IF(degrade_xs) i_start = MAX(its,ids+1)
  IF(degrade_xe) i_end   = MIN(ite,ide-2)

  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

     
     tendency (i,k,j) = tendency(i,k,j)                           &
               - msftx(i,j)*( rdx*( fqx (i+1,k,j)-fqx (i,k,j)     &
                                   +fqxl(i+1,k,j)-fqxl(i,k,j))   )

  ENDDO
  ENDDO
  ENDDO

  IF(tenddec) THEN
  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

     h_tendency (i,k,j) = 0.                                      &
               - msftx(i,j)*( rdx*( fqx (i+1,k,j)-fqx (i,k,j)     &
                                   +fqxl(i+1,k,j)-fqxl(i,k,j))   )

  ENDDO
  ENDDO
  ENDDO
  END IF



  i_start = its
  i_end   = MIN(ite,ide-1)





  IF(degrade_ys) j_start = MAX(jts,jds+1)
  IF(degrade_ye) j_end   = MIN(jte,jde-2)

  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

     
     tendency (i,k,j) = tendency(i,k,j)                           &
               - msftx(i,j)*( rdy*( fqy (i,k,j+1)-fqy (i,k,j)     &
                                   +fqyl(i,k,j+1)-fqyl(i,k,j))   )

  ENDDO
  ENDDO
  ENDDO

  IF(tenddec) THEN
  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

     h_tendency (i,k,j) = h_tendency (i,k,j)                      &
               - msftx(i,j)*( rdy*( fqy (i,k,j+1)-fqy (i,k,j)     &
                                   +fqyl(i,k,j+1)-fqyl(i,k,j))   )

  ENDDO
  ENDDO
  ENDDO
  END IF

END SUBROUTINE advect_scalar_mono




SUBROUTINE advect_scalar_weno ( field, field_old, tendency,     &
                             ru, rv, rom,                   &
                             mut, time_step, config_flags,  &
                             msfux, msfuy, msfvx, msfvy,    &
                             msftx, msfty,                  &
                             fzm, fzp,                      &
                             rdx, rdy, rdzw,                &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte  )





   IMPLICIT NONE
   
   
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte
   
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: field,     &
                                                                      field_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy
   INTEGER ,                                     INTENT(IN   ) :: time_step


   
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax

   INTEGER , PARAMETER :: is=0, js=0, ks=0

   REAL    :: mrdx, mrdy, ub, vb, vw
   REAL , DIMENSION(its:ite, kts:kte) :: vflux


   REAL,  DIMENSION( its-is:ite+1, kts:kte  ) :: fqx

   REAL,  DIMENSION( its:ite, kts:kte, 2 ) :: fqy

   INTEGER :: horz_order, vert_order
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

   INTEGER :: jp1, jp0, jtmp

    real            :: dir, vv
    real            :: ue,uw,vs,vn,wb,wt
    real, parameter :: f30 =  7./12., f31 = 1./12.
    real, parameter :: f50 = 37./60., f51 = 2./15., f52 = 1./60.


   integer kt,kb
   
    
    real               :: qim2, qim1, qi, qip1, qip2
    double precision               :: beta0, beta1, beta2, f0, f1, f2, wi0, wi1, wi2, sumwk
    double precision, parameter    :: gi0 = 1.d0/10.d0, gi1 = 6.d0/10.d0, gi2 = 3.d0/10.d0, eps=1.0d-28
    integer, parameter :: pw = 2




   REAL    :: flux3, flux4, flux5, flux6
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel

      flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
            (7./12.)*(q_i + q_im1) - (1./12.)*(q_ip1 + q_im2)

      flux3(q_im2, q_im1, q_i, q_ip1, ua) =                     &
           flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
           sign(1.,ua)*(1./12.)*((q_ip1 - q_im2)-3.*(q_i-q_im1))

      flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
            (37./60.)*(q_i+q_im1) - (2./15.)*(q_ip1+q_im2)      &
            +(1./60.)*(q_ip2+q_im3)

      flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)    &
            -sign(1,time_step)*sign(1.,ua)*(1./60.)*(           &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )

   LOGICAL :: specified

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.



  ktf=MIN(kte,kde-1)
  horz_order = 5 
  vert_order = 5 






  IF( horz_order == 5 ) THEN








   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-4)                ) degrade_ye = .false.



      ktf=MIN(kte,kde-1)
      i_start = its
      i_end   = MIN(ite,ide-1)



      IF ( is == 1 ) THEN
        i_start = its
        i_end   = ite
        IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
        IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-1,ite)
        IF ( config_flags%periodic_x ) i_start = its
        IF ( config_flags%periodic_x ) i_end = ite
      ENDIF

      j_start = jts
      j_end   = MIN(jte,jde-1)




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)



     jp1 = 2
     jp0 = 1

     j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN 

        DO k=kts,ktf
        DO i = i_start, i_end

          vel = 0.5*( rv(i,k,j) + rv(i-is,k-ks,j-js) )

         IF ( vel .ge. 0.0 ) THEN
            qip2 = field(i,k,j+1)
            qip1 = field(i,k,j  )
            qi   = field(i,k,j-1)
            qim1 = field(i,k,j-2)
            qim2 = field(i,k,j-3)
          ELSE
            qip2 = field(i,k,j-2)
            qip1 = field(i,k,j-1)
            qi   = field(i,k,j  )
            qim1 = field(i,k,j+1)
            qim2 = field(i,k,j+2)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
          fqy( i, k, jp1 ) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk




        ENDDO
        ENDDO


      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i,k, jp1) = 0.5*rv(i,k,j)*          &

                     (field(i,k,j)+field(i,k,j-1))

            ENDDO
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              vel = rv(i,k,j)
              fqy( i, k, jp1 ) = vel*flux3(              &
                   field(i,k,j-2),field(i,k,j-1),field(i,k,j),field(i,k,j+1),vel )
            ENDDO
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end

              fqy(i, k, jp1) = 0.5*rv(i,k,j)*      &
                     (field(i,k,j)+field(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = rv(i,k,j)

              fqy( i, k, jp1) = vel*flux3(             &
                   field(i,k,j-2),field(i,k,j-1),    &
                   field(i,k,j),field(i,k,j+1),vel )
            ENDDO
            ENDDO

     ENDIF



      IF ( is == 0 ) THEN
        
        
        
        
        IF ( config_flags%polar .AND. (j == jds+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy     
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
          END DO
          END DO
        ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy     
            tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
          END DO
          END DO
        ELSE  

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF
        ENDIF
       ELSEIF ( is == 1 ) THEN

        
        IF ( config_flags%polar .AND. (j == jds+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfux(i,j-1)*rdy   
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
          END DO
          END DO
        
        ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfux(i,j-1)*rdy   
            tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
          END DO
          END DO
        ELSE  

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfux(i,j-1)*rdy   
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF

        END IF
       
       ENDIF

        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

      ENDDO j_loop_y_flux_5



      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)

        i_start_f = MIN(i_start+2,ids+3)
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f

          vel = 0.5*( ru(i,k,j) + ru(i-is,k-ks,j-js) )


         IF ( vel .ge. 0.0 ) THEN
            qip2 = field(i+1,k,j)
            qip1 = field(i,  k,j)
            qi   = field(i-1,k,j)
            qim1 = field(i-2,k,j)
            qim2 = field(i-3,k,j)
          ELSE
            qip2 = field(i-2,k,j)
            qip1 = field(i-1,k,j)
            qi   = field(i,  k,j)
            qim1 = field(i+1,k,j)
            qim2 = field(i+2,k,j)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
         fqx(i,k) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk





        ENDDO
        ENDDO



        IF( degrade_xs ) THEN

          DO i=i_start,i_start_f-1

            IF(i == ids+1) THEN 
              DO k=kts,ktf
                fqx(i,k) = 0.5*(ru(i,k,j)) &
                       *(field(i,k,j)+field(i-1,k,j))
              ENDDO
            ENDIF

            IF(i == ids+2) THEN  
              DO k=kts,ktf
                vel = ru(i,k,j)
                fqx( i,k ) = vel*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                              field(i  ,k,j), field(i+1,k,j),  &
                                              vel                     )
              ENDDO
            END IF

          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          DO i = i_end_f+1, i_end+1

            IF( i == ide-1 ) THEN 
              DO k=kts,ktf
                fqx(i,k) = 0.5*(ru(i,k,j))      &
                       *(field(i,k,j)+field(i-1,k,j))
              ENDDO
           ENDIF

           IF( i == ide-2 ) THEN 
             DO k=kts,ktf
               vel = ru(i,k,j)
               fqx( i,k ) = vel*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                       field(i  ,k,j), field(i+1,k,j),  &
                                       vel                             )
             ENDDO
           ENDIF

         ENDDO

       ENDIF



       IF ( is == 0 ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msftx(i,j)*rdx      
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
          ENDDO
       ELSEIF ( is == 1 ) THEN
        DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msfux(i,j)*rdx 
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO
       ENDIF

      ENDDO


   ENDIF
   





      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)



   IF( (config_flags%open_xs) .and. (its == ids) ) THEN

       DO j = j_start, j_end
       DO k = kts, ktf
         ub = MIN( 0.5*(ru(its,k,j)+ru(its+1,k,j)), 0. )
         tendency(its,k,j) = tendency(its,k,j)                     &
               - rdx*(                                             &
                       ub*(   field_old(its+1,k,j)                 &
                            - field_old(its  ,k,j)   ) +           &
                       field(its,k,j)*(ru(its+1,k,j)-ru(its,k,j))  &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_xe) .and. (ite == ide) ) THEN

       DO j = j_start, j_end
       DO k = kts, ktf
         ub = MAX( 0.5*(ru(ite-1,k,j)+ru(ite,k,j)), 0. )
         tendency(i_end,k,j) = tendency(i_end,k,j)                   &
               - rdx*(                                               &
                       ub*(  field_old(i_end  ,k,j)                  &
                           - field_old(i_end-1,k,j) ) +              &
                       field(i_end,k,j)*(ru(ite,k,j)-ru(ite-1,k,j))  &
                                                                    )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_ys) .and. (jts == jds) ) THEN

       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MIN( 0.5*(rv(i,k,jts)+rv(i,k,jts+1)), 0. )
         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(  field_old(i,k,jts+1)                  &
                           - field_old(i,k,jts  ) ) +              &
                       field(i,k,jts)*(rv(i,k,jts+1)-rv(i,k,jts))  &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_ye) .and. (jte == jde)) THEN

       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MAX( 0.5*(rv(i,k,jte-1)+rv(i,k,jte)), 0. )
         tendency(i,k,j_end) = tendency(i,k,j_end)                   &
               - rdy*(                                               &
                       vb*(   field_old(i,k,j_end  )                 &
                            - field_old(i,k,j_end-1) ) +             &
                       field(i,k,j_end)*(rv(i,k,jte)-rv(i,k,jte-1))  &
                                                                    )
       ENDDO
       ENDDO

   ENDIF







      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      DO i = i_start, i_end
         vflux(i,kts)=0.
         vflux(i,kte)=0.
      ENDDO



      DO j = j_start, j_end

         DO k=kts+3,ktf-2
         DO i = i_start, i_end

           vel = 0.5*( rom(i,k,j) + rom(i-is,k-ks,j-js) )

         IF( -vel .ge. 0.0 ) THEN
            qip2 = field(i,k+1,j)
            qip1 = field(i,k  ,j)
            qi   = field(i,k-1,j)
            qim1 = field(i,k-2,j)
            qim2 = field(i,k-3,j)
          ELSE
            qip2 = field(i,k-2,j)
            qip1 = field(i,k-1,j)
            qi   = field(i,k  ,j)
            qim1 = field(i,k+1,j)
            qim2 = field(i,k+2,j)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
          vflux(i,k) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk




         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
                                   
           k = kts+2
           vel=rom(i,k,j) 
           vflux(i,k) = vel*flux3(               &
                   field(i,k-2,j), field(i,k-1,j),   &
                   field(i,k  ,j), field(i,k+1,j), -vel )
           k = ktf-1
           vel=rom(i,k,j)
           vflux(i,k) = vel*flux3(               &
                   field(i,k-2,j), field(i,k-1,j),   &
                   field(i,k  ,j), field(i,k+1,j), -vel )

           k=ktf
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
         ENDDO

         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO



END SUBROUTINE advect_scalar_weno



SUBROUTINE advect_weno_u ( u, u_old, tendency,            &
                        ru, rv, rom,                   &
                        mut, time_step, config_flags,  &
                        msfux, msfuy, msfvx, msfvy,    &
                        msftx, msfty,                  &
                        fzm, fzp,                      &
                        rdx, rdy, rdzw,                &
                        ids, ide, jds, jde, kds, kde,  &
                        ims, ime, jms, jme, kms, kme,  &
                        its, ite, jts, jte, kts, kte  )







   IMPLICIT NONE
   
   
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: u,     &
                                                                      u_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy
   INTEGER ,                                     INTENT(IN   ) :: time_step

   
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax, im, ip
   INTEGER :: jp1, jp0, jtmp

    real            :: dir, vv
    real            :: ue,vs,vn,wb,wt
    real, parameter :: f30 =  7./12., f31 = 1./12.
    real, parameter :: f50 = 37./60., f51 = 2./15., f52 = 1./60.


   integer kt,kb
   
    
    real               :: qim2, qim1, qi, qip1, qip2
    double precision               :: beta0, beta1, beta2, f0, f1, f2, wi0, wi1, wi2, sumwk
    double precision, parameter    :: gi0 = 1.d0/10.d0, gi1 = 6.d0/10.d0, gi2 = 3.d0/10.d0, eps=1.0d-18
    integer, parameter :: pw = 2


   INTEGER :: horz_order, vert_order

   REAL    :: mrdx, mrdy, ub, vb, uw, vw, dvm, dvp
   REAL , DIMENSION(its:ite, kts:kte) :: vflux


   REAL,  DIMENSION( its-1:ite+1, kts:kte ) :: fqx
   REAL,  DIMENSION( its:ite, kts:kte, 2) :: fqy
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye



   REAL    :: flux3, flux4, flux5, flux6
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel

   flux4(q_im2, q_im1, q_i, q_ip1, ua) =                         &
          ( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0

   flux3(q_im2, q_im1, q_i, q_ip1, ua) =                         &
            flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
            sign(1,time_step)*sign(1.,ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0

   flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =           &
                      ( 37.*(q_i+q_im1) - 8.*(q_ip1+q_im2)       &
                     +(q_ip2+q_im3) )/60.0

   flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =           &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)     &
            -sign(1,time_step)*sign(1.,ua)*(                     &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )/60.0


   LOGICAL :: specified

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.



   horz_order = config_flags%h_mom_adv_order
   vert_order = config_flags%v_mom_adv_order

   ktf=MIN(kte,kde-1)



















   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-4)                ) degrade_ye = .false.



      i_start = its
      i_end   = ite
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-1,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

      j_start = jts
      j_end   = MIN(jte,jde-1)




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)



     jp1 = 2
     jp0 = 1

     j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN  

        DO k=kts,ktf
        DO i = i_start, i_end
          vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))

         IF ( vel .ge. 0.0 ) THEN
            qip2 = u(i,k,j+1)
            qip1 = u(i,k,j  )
            qi   = u(i,k,j-1)
            qim1 = u(i,k,j-2)
            qim2 = u(i,k,j-3)
          ELSE
            qip2 = u(i,k,j-2)
            qip1 = u(i,k,j-1)
            qi   = u(i,k,j  )
            qim1 = u(i,k,j+1)
            qim2 = u(i,k,j+2)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
          fqy( i, k, jp1 ) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk




        ENDDO
        ENDDO



      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i-1,k,j))  &
                                     *(u(i,k,j)+u(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
              fqy( i, k, jp1 ) = vel*flux3(      &
                   u(i,k,j-2),u(i,k,j-1), u(i,k,j),u(i,k,j+1),vel )
            ENDDO
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i-1,k,j))    &
                     *(u(i,k,j)+u(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
              fqy( i, k, jp1 ) = vel*flux3(     &
                   u(i,k,j-2),u(i,k,j-1),    &
                   u(i,k,j),u(i,k,j+1),vel )
            ENDDO
            ENDDO

      END IF



        
        IF ( config_flags%polar .AND. (j == jds+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfux(i,j-1)*rdy   
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
          END DO
          END DO
        
        ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfux(i,j-1)*rdy   
            tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
          END DO
          END DO
        ELSE  

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfux(i,j-1)*rdy   
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF

        END IF


        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

   ENDDO j_loop_y_flux_5



      i_start = its
      i_end   = ite

      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)
        i_start_f = ids+3
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-1,ite)
        i_end_f = ide-2
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))

         IF ( vel .ge. 0.0 ) THEN
            qip2 = u(i+1,k,j)
            qip1 = u(i,  k,j)
            qi   = u(i-1,k,j)
            qim1 = u(i-2,k,j)
            qim2 = u(i-3,k,j)
          ELSE
            qip2 = u(i-2,k,j)
            qip1 = u(i-1,k,j)
            qi   = u(i,  k,j)
            qim1 = u(i+1,k,j)
            qim2 = u(i+2,k,j)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
         fqx(i,k) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk





        ENDDO
        ENDDO




        IF( degrade_xs ) THEN

          IF( i_start == ids+1 ) THEN 
            i = ids+1
            DO k=kts,ktf
              ub = u(i-1,k,j)
              IF (specified .AND. u(i,k,j) .LT. 0.)ub = u(i,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i,k,j)+ub)
            ENDDO
          END IF

          i = ids+2
          DO k=kts,ktf
            vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
            fqx( i, k  ) = vel*flux3( u(i-2,k,j), u(i-1,k,j),  &
                                           u(i  ,k,j), u(i+1,k,j),  &
                                           vel                     )
          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          IF( i_end == ide-1 ) THEN 
            i = ide
            DO k=kts,ktf
              ub = u(i,k,j)
              IF (specified .AND. u(i-1,k,j) .GT. 0.)ub = u(i-1,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i-1,k,j)+ub)
            ENDDO
          ENDIF

          DO k=kts,ktf
          i = ide-1
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
          fqx( i,k ) = vel*flux3( u(i-2,k,j), u(i-1,k,j),  &
                                         u(i  ,k,j), u(i+1,k,j),  &
                                         vel                     )
          ENDDO

        ENDIF



        DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msfux(i,j)*rdx 
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO




      IF ( (config_flags%open_xs) .and. its == ids ) THEN

        j_start = jts
        j_end   = MIN(jte,jde-1)

        DO j = j_start, j_end
        DO k = kts, ktf
          ub = MIN(ru(its,k,j)-cb*mut(its,j), 0.)
          tendency(its,k,j) = tendency(its,k,j)                    &
                      - rdx*ub*(u_old(its+1,k,j) - u_old(its,k,j))
        ENDDO
        ENDDO

      ENDIF

      IF ( (config_flags%open_xe) .and. ite == ide ) THEN

        j_start = jts
        j_end   = MIN(jte,jde-1)

        DO j = j_start, j_end
        DO k = kts, ktf
          ub = MAX(ru(ite,k,j)+cb*mut(ite-1,j), 0.)
          tendency(ite,k,j) = tendency(ite,k,j)                    &
                      - rdx*ub*(u_old(ite,k,j) - u_old(ite-1,k,j))
        ENDDO
        ENDDO

      ENDIF





      i_start = its
      i_end   = MIN(ite,ide)
      imin    = ids
      imax    = ide-1

      IF (config_flags%open_xs) THEN
        i_start = MAX(ids+1, its)
        imin = ids
      ENDIF
      IF (config_flags%open_xe) THEN
        i_end = MIN(ite,ide-1)
        imax = ide-1
      ENDIF

   IF( (config_flags%open_ys) .and. (jts == jds)) THEN

      DO i = i_start, i_end

         mrdy=msfux(i,jts)*rdy       
         ip = MIN( imax, i   )
         im = MAX( imin, i-1 )

         DO k=kts,ktf

          vw = 0.5*(rv(ip,k,jts)+rv(im,k,jts))
          vb = MIN( vw, 0. )
          dvm =  rv(ip,k,jts+1)-rv(ip,k,jts)
          dvp =  rv(im,k,jts+1)-rv(im,k,jts)
          tendency(i,k,jts)=tendency(i,k,jts)-mrdy*(                &
                            vb*(u_old(i,k,jts+1)-u_old(i,k,jts))    &
                           +0.5*u(i,k,jts)*(dvm+dvp))
         ENDDO
      ENDDO

   ENDIF

   IF( (config_flags%open_ye) .and. (jte == jde)) THEN

      DO i = i_start, i_end

         mrdy=msfux(i,jte-1)*rdy     
         ip = MIN( imax, i   )
         im = MAX( imin, i-1 )

         DO k=kts,ktf

          vw = 0.5*(rv(ip,k,jte)+rv(im,k,jte))
          vb = MAX( vw, 0. )
          dvm =  rv(ip,k,jte)-rv(ip,k,jte-1)
          dvp =  rv(im,k,jte)-rv(im,k,jte-1)
          tendency(i,k,jte-1)=tendency(i,k,jte-1)-mrdy*(              &
                              vb*(u_old(i,k,jte-1)-u_old(i,k,jte-2))  &
                             +0.5*u(i,k,jte-1)*(dvm+dvp))
         ENDDO
      ENDDO

   ENDIF







   i_start = its
   i_end   = ite
   j_start = jts
   j_end   = min(jte,jde-1)




   IF ( config_flags%open_ys .or. specified ) i_start = MAX(ids+1,its)
   IF ( config_flags%open_ye .or. specified ) i_end   = MIN(ide-1,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

   DO i = i_start, i_end
     vflux(i,kts)=0.
     vflux(i,kte)=0.
   ENDDO





      DO j = j_start, j_end

         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=0.5*(rom(i-1,k,j)+rom(i,k,j))

         IF( -vel .ge. 0.0 ) THEN
            qip2 = u(i,k+1,j)
            qip1 = u(i,k  ,j)
            qi   = u(i,k-1,j)
            qim1 = u(i,k-2,j)
            qim2 = u(i,k-3,j)
          ELSE
            qip2 = u(i,k-2,j)
            qip1 = u(i,k-1,j)
            qi   = u(i,k  ,j)
            qim1 = u(i,k+1,j)
            qim2 = u(i,k+2,j)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
          vflux(i,k) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk




         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j))  &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))
           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i-1,k,j)) 
           vflux(i,k) = vel*flux3(       &
                   u(i,k-2,j), u(i,k-1,j),   &
                   u(i,k  ,j), u(i,k+1,j), -vel )
           k = ktf-1
           vel=0.5*(rom(i,k,j)+rom(i-1,k,j)) 
           vflux(i,k) = vel*flux3(       &
                   u(i,k-2,j), u(i,k-1,j),   &
                   u(i,k  ,j), u(i,k+1,j), -vel )
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j)) &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))

         ENDDO
         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO
      ENDDO


END SUBROUTINE advect_weno_u



SUBROUTINE advect_weno_v   ( v, v_old, tendency,            &
                        ru, rv, rom,                   &
                        mut, time_step, config_flags,  &
                        msfux, msfuy, msfvx, msfvy,    &
                        msftx, msfty,                  &
                        fzm, fzp,                      &
                        rdx, rdy, rdzw,                &
                        ids, ide, jds, jde, kds, kde,  &
                        ims, ime, jms, jme, kms, kme,  &
                        its, ite, jts, jte, kts, kte  )







   IMPLICIT NONE
   
   
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: v,     &
                                                                      v_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy
   INTEGER ,                                     INTENT(IN   ) :: time_step


   
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax

    real            :: dir, vv
    real            :: ue,vs,vn,wb,wt
    real, parameter :: f30 =  7./12., f31 = 1./12.
    real, parameter :: f50 = 37./60., f51 = 2./15., f52 = 1./60.


   integer kt,kb
   
    
    real               :: qim2, qim1, qi, qip1, qip2
    double precision               :: beta0, beta1, beta2, f0, f1, f2, wi0, wi1, wi2, sumwk
    double precision, parameter    :: gi0 = 1.d0/10.d0, gi1 = 6.d0/10.d0, gi2 = 3.d0/10.d0, eps=1.0d-18
    integer, parameter :: pw = 2


   REAL    :: mrdx, mrdy, ub, vb, uw, vw, dup, dum
   REAL , DIMENSION(its:ite, kts:kte) :: vflux


   REAL,  DIMENSION( its:ite+1, kts:kte ) :: fqx
   REAL,  DIMENSION( its:ite, kts:kte, 2 ) :: fqy

   INTEGER :: horz_order
   INTEGER :: vert_order
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

   INTEGER :: jp1, jp0, jtmp




   REAL    :: flux3, flux4, flux5, flux6
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel

   flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
          ( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0

   flux3(q_im2, q_im1, q_i, q_ip1, ua) =                     &
           flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
           sign(1,time_step)*sign(1.,ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0

   flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
                      ( 37.*(q_i+q_im1) - 8.*(q_ip1+q_im2)   &
                     +(q_ip2+q_im3) )/60.0

   flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)    &
            -sign(1,time_step)*sign(1.,ua)*(                    &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )/60.0



   LOGICAL :: specified

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.



   ktf=MIN(kte,kde-1)
   horz_order = config_flags%h_mom_adv_order
   vert_order = config_flags%v_mom_adv_order




















   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.



      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-1)
        j_end_f = jde-2
      ENDIF



     jp1 = 2
     jp0 = 1

     j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN

        DO k=kts,ktf
        DO i = i_start, i_end
          vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))

         IF ( vel .ge. 0.0 ) THEN
            qip2 = v(i,k,j+1)
            qip1 = v(i,k,j  )
            qi   = v(i,k,j-1)
            qim1 = v(i,k,j-2)
            qim2 = v(i,k,j-3)
          ELSE
            qip2 = v(i,k,j-2)
            qip1 = v(i,k,j-1)
            qi   = v(i,k,j  )
            qim1 = v(i,k,j+1)
            qim2 = v(i,k,j+2)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
          fqy( i, k, jp1 ) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk






        ENDDO
        ENDDO




      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts,ktf
            DO i = i_start, i_end
                vb = v(i,k,j-1)
                IF (specified .AND. v(i,k,j) .LT. 0.)vb = v(i,k,j)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))  &
                                 *(v(i,k,j)+vb)
            ENDDO
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
              fqy( i, k, jp1 ) = vel*flux3(      &
                   v(i,k,j-2),v(i,k,j-1),v(i,k,j),v(i,k,j+1),vel )
            ENDDO
            ENDDO


     ELSE IF ( j == jde ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
                vb = v(i,k,j)
                IF (specified .AND. v(i,k,j-1) .GT. 0.)vb = v(i,k,j-1)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))    &
                                 *(vb+v(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
              fqy( i, k, jp1 ) = vel*flux3(     &
                   v(i,k,j-2),v(i,k,j-1),v(i,k,j),v(i,k,j+1),vel )
            ENDDO
            ENDDO

      END IF



        
        
        
        
        IF ( config_flags%polar .AND. (j == jds+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            tendency(i,k,j-1) = 0.
          END DO
          END DO
        
        
        
        
        ELSE IF( config_flags%polar .AND. (j == jde+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            tendency(i,k,j-1) = 0.
          END DO
          END DO
        ELSE  

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfvy(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF

        END IF

        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

   ENDDO j_loop_y_flux_5



      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = jte
      
      IF ( config_flags%open_ys .or. specified .or. config_flags%polar ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified .or. config_flags%polar ) j_end   = MIN(jde-1,jte)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)

        i_start_f = MIN(i_start+2,ids+3)
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF



      DO j = j_start, j_end



        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))

         IF ( vel .ge. 0.0 ) THEN
            qip2 = v(i+1,k,j)
            qip1 = v(i,  k,j)
            qi   = v(i-1,k,j)
            qim1 = v(i-2,k,j)
            qim2 = v(i-3,k,j)
          ELSE
            qip2 = v(i-2,k,j)
            qip1 = v(i-1,k,j)
            qi   = v(i,  k,j)
            qim1 = v(i+1,k,j)
            qim2 = v(i+2,k,j)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
         fqx(i,k) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk





        ENDDO
        ENDDO



        IF( degrade_xs ) THEN

          DO i=i_start,i_start_f-1

            IF(i == ids+1) THEN 
              DO k=kts,ktf
                fqx(i,k) = 0.25*(ru(i,k,j)+ru(i,k,j-1)) &
                                *(v(i,k,j)+v(i-1,k,j))
              ENDDO
            ENDIF

            IF(i == ids+2) THEN  
              DO k=kts,ktf
                vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
                fqx( i,k ) = vel*flux3( v(i-2,k,j), v(i-1,k,j),  &
                                        v(i  ,k,j), v(i+1,k,j),  &
                                        vel                     )
              ENDDO
            ENDIF

          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          DO i = i_end_f+1, i_end+1

            IF( i == ide-1 ) THEN 
              DO k=kts,ktf
                fqx(i,k) = 0.25*(ru(i_end+1,k,j)+ru(i_end+1,k,j-1))      &
                                *(v(i_end+1,k,j)+v(i_end,k,j))
              ENDDO
            ENDIF

            IF( i == ide-2 ) THEN 
              DO k=kts,ktf
                vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
                fqx( i,k ) = vel*flux3( v(i-2,k,j), v(i-1,k,j),  &
                                        v(i  ,k,j), v(i+1,k,j),  &
                                        vel                     )
              ENDDO
            ENDIF

          ENDDO

        ENDIF



        DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msfvy(i,j)*rdx      
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO


   
   
   
   IF ( config_flags%polar .AND. (jts == jds) ) THEN
      DO i=its,ite
      DO k=kts,ktf
         tendency(i,k,jts)=0.
      END DO
      END DO
   END IF
   IF ( config_flags%polar .AND. (jte == jde) ) THEN
      DO i=its,ite
      DO k=kts,ktf
         tendency(i,k,jte)=0.
      END DO
      END DO
   END IF



      IF ( (config_flags%open_ys) .and. jts == jds ) THEN

        i_start = its
        i_end   = MIN(ite,ide-1)

        DO i = i_start, i_end
        DO k = kts, ktf
          vb = MIN(rv(i,k,jts)-cb*mut(i,jts), 0.)
          tendency(i,k,jts) = tendency(i,k,jts)                    &
                      - rdy*vb*(v_old(i,k,jts+1) - v_old(i,k,jts))
        ENDDO
        ENDDO

      ENDIF

      IF ( (config_flags%open_ye) .and. jte == jde ) THEN

        i_start = its
        i_end   = MIN(ite,ide-1)

        DO i = i_start, i_end
        DO k = kts, ktf
          vb = MAX(rv(i,k,jte)+cb*mut(i,jte-1), 0.)
          tendency(i,k,jte) = tendency(i,k,jte)                    &
                      - rdy*vb*(v_old(i,k,jte) - v_old(i,k,jte-1))
        ENDDO
        ENDDO

      ENDIF





      j_start = jts
      j_end   = MIN(jte,jde)

      jmin    = jds
      jmax    = jde-1

      IF (config_flags%open_ys) THEN
          j_start = MAX(jds+1, jts)
          jmin = jds
      ENDIF
      IF (config_flags%open_ye) THEN
          j_end = MIN(jte,jde-1)
          jmax = jde-1
      ENDIF



   IF( (config_flags%open_xs) .and. (its == ids)) THEN

      DO j = j_start, j_end

         mrdx=msfvy(its,j)*rdx       
         jp = MIN( jmax, j   )
         jm = MAX( jmin, j-1 )

         DO k=kts,ktf

          uw = 0.5*(ru(its,k,jp)+ru(its,k,jm))
          ub = MIN( uw, 0. )
          dup =  ru(its+1,k,jp)-ru(its,k,jp)
          dum =  ru(its+1,k,jm)-ru(its,k,jm)
          tendency(its,k,j)=tendency(its,k,j)-mrdx*(               &
                            ub*(v_old(its+1,k,j)-v_old(its,k,j))   &
                           +0.5*v(its,k,j)*(dup+dum))
         ENDDO
      ENDDO

   ENDIF

   IF( (config_flags%open_xe) .and. (ite == ide) ) THEN
      DO j = j_start, j_end

         mrdx=msfvy(ite-1,j)*rdx     
         jp = MIN( jmax, j   )
         jm = MAX( jmin, j-1 )

         DO k=kts,ktf

          uw = 0.5*(ru(ite,k,jp)+ru(ite,k,jm))
          ub = MAX( uw, 0. )
          dup = ru(ite,k,jp)-ru(ite-1,k,jp)
          dum = ru(ite,k,jm)-ru(ite-1,k,jm)






          tendency(ite-1,k,j)=tendency(ite-1,k,j)-mrdx*(              &
                            ub*(v_old(ite-1,k,j)-v_old(ite-2,k,j))    &
                           +0.5*v(ite-1,k,j)*(dup+dum))

         ENDDO
      ENDDO

   ENDIF









      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte

      DO i = i_start, i_end
         vflux(i,kts)=0.
         vflux(i,kte)=0.
      ENDDO

      
      
      IF ( config_flags%open_ys .or. specified .or. config_flags%polar ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified .or. config_flags%polar ) j_end   = MIN(jde-1,jte)





      DO j = j_start, j_end


         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1))

         IF( -vel .ge. 0.0 ) THEN
            qip2 = v(i,k+1,j)
            qip1 = v(i,k  ,j)
            qi   = v(i,k-1,j)
            qim1 = v(i,k-2,j)
            qim2 = v(i,k-3,j)
          ELSE
            qip2 = v(i,k-2,j)
            qip1 = v(i,k-1,j)
            qi   = v(i,k  ,j)
            qim1 = v(i,k+1,j)
            qim2 = v(i,k+2,j)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
          vflux(i,k) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk





         ENDDO
         ENDDO

         DO i = i_start, i_end
           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1))  &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))
           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1)) 
           vflux(i,k) = vel*flux3(       &
                   v(i,k-2,j), v(i,k-1,j),   &
                   v(i,k  ,j), v(i,k+1,j), -vel )
           k = ktf-1
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1)) 
           vflux(i,k) = vel*flux3(       &
                   v(i,k-2,j), v(i,k-1,j),   &
                   v(i,k  ,j), v(i,k+1,j), -vel )
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1)) &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))

         ENDDO


         DO k=kts,ktf
         DO i = i_start, i_end
            
            
            tendency(i,k,j)=tendency(i,k,j)-(msfvy(i,j)/msfvx(i,j))*rdzw(k)*(vflux(i,k+1)-vflux(i,k)) 
         ENDDO
         ENDDO

      ENDDO


END SUBROUTINE advect_weno_v




SUBROUTINE advect_weno_w    ( w, w_old, tendency,            &
                         ru, rv, rom,                   &
                         mut, time_step, config_flags,  &
                         msfux, msfuy, msfvx, msfvy,    &
                         msftx, msfty,                  &
                         fzm, fzp,                      &
                         rdx, rdy, rdzu,                &
                         ids, ide, jds, jde, kds, kde,  &
                         ims, ime, jms, jme, kms, kme,  &
                         its, ite, jts, jte, kts, kte  )







   IMPLICIT NONE
   
   
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: w,     &
                                                                      w_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzu

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy
   INTEGER ,                                     INTENT(IN   ) :: time_step


   
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax

   REAL    :: mrdx, mrdy, ub, vb, uw, vw
   REAL , DIMENSION(its:ite, kts:kte) :: vflux

    real            :: dir, vv
    real            :: ue,vs,vn,wb,wt
    real, parameter :: f30 =  7./12., f31 = 1./12.
    real, parameter :: f50 = 37./60., f51 = 2./15., f52 = 1./60.


   integer kt,kb
   
    
    real               :: qim2, qim1, qi, qip1, qip2
    double precision               :: beta0, beta1, beta2, f0, f1, f2, wi0, wi1, wi2, sumwk
    double precision, parameter    :: gi0 = 1.d0/10.d0, gi1 = 6.d0/10.d0, gi2 = 3.d0/10.d0, eps=1.0d-18
    integer, parameter :: pw = 2



   INTEGER :: horz_order, vert_order

   REAL,  DIMENSION( its:ite+1, kts:kte ) :: fqx
   REAL,  DIMENSION( its:ite, kts:kte, 2 ) :: fqy
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

   INTEGER :: jp1, jp0, jtmp



   REAL    :: flux3, flux4, flux5, flux6
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel

      flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
          ( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0

      flux3(q_im2, q_im1, q_i, q_ip1, ua) =                     &
           flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
           sign(1,time_step)*sign(1.,ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0

      flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
                      ( 37.*(q_i+q_im1) - 8.*(q_ip1+q_im2)      &
                     +(q_ip2+q_im3) )/60.0

      flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)    &
            -sign(1,time_step)*sign(1.,ua)*(                    &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )/60.0


   LOGICAL :: specified

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.



  ktf=MIN(kte,kde-1)
  horz_order = config_flags%h_sca_adv_order
  vert_order = config_flags%v_sca_adv_order















   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+3)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+3)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-4)                ) degrade_ye = .false.



      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)




      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

      IF(config_flags%polar) j_end = MIN(jte,jde-1)



     jp1 = 2
     jp0 = 1

     j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN

        DO k=kts+1,ktf
        DO i = i_start, i_end
          vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)

         IF ( vel .ge. 0.0 ) THEN
            qip2 = w(i,k,j+1)
            qip1 = w(i,k,j  )
            qi   = w(i,k,j-1)
            qim1 = w(i,k,j-2)
            qim2 = w(i,k,j-3)
          ELSE
            qip2 = w(i,k,j-2)
            qip1 = w(i,k,j-1)
            qi   = w(i,k,j  )
            qim1 = w(i,k,j+1)
            qim2 = w(i,k,j+2)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
          fqy( i, k, jp1 ) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk




        ENDDO
        ENDDO

        k = ktf+1
        DO i = i_start, i_end
          vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)

         IF ( vel .ge. 0.0 ) THEN
            qip2 = w(i,k,j+1)
            qip1 = w(i,k,j  )
            qi   = w(i,k,j-1)
            qim1 = w(i,k,j-2)
            qim2 = w(i,k,j-3)
          ELSE
            qip2 = w(i,k,j-2)
            qip1 = w(i,k,j-1)
            qi   = w(i,k,j  )
            qim1 = w(i,k,j+1)
            qim2 = w(i,k,j+2)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
          fqy( i, k, jp1 ) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk




        ENDDO

      ELSE IF ( j == jds+1 ) THEN   

            DO k=kts+1,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))*          &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))*          &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  

            DO k=kts+1,ktf
            DO i = i_start, i_end
              vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
              fqy( i, k, jp1 ) = vel*flux3(              &
                   w(i,k,j-2),w(i,k,j-1),w(i,k,j),w(i,k,j+1),vel )
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
              fqy( i, k, jp1 ) = vel*flux3(              &
                   w(i,k,j-2),w(i,k,j-1),w(i,k,j),w(i,k,j+1),vel )
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  

            DO k=kts+1,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))*      &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))*      &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  

            DO k=kts+1,ktf
            DO i = i_start, i_end
              vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
              fqy( i, k, jp1 ) = vel*flux3(             &
                   w(i,k,j-2),w(i,k,j-1),    &
                   w(i,k,j),w(i,k,j+1),vel )
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
              fqy( i, k, jp1 ) = vel*flux3(             &
                   w(i,k,j-2),w(i,k,j-1),    &
                   w(i,k,j),w(i,k,j+1),vel )
            ENDDO

     ENDIF



        
        
        
        
        IF ( config_flags%polar .AND. (j == jds+1) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*fqy(i,k,jp1)
          END DO
          END DO
        ELSE IF( config_flags%polar .AND. (j == jde) ) THEN
          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) + mrdy*fqy(i,k,jp0)
          END DO
          END DO
        ELSE  

        IF(j > j_start) THEN

          DO k=kts+1,ktf+1
          DO i = i_start, i_end
            mrdy=msftx(i,j-1)*rdy    
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

       ENDIF

        END IF

        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

      ENDDO j_loop_y_flux_5



      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = MIN(jte,jde-1)




      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)

        i_start_f = MIN(i_start+2,ids+3)
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF



      DO j = j_start, j_end



        DO k=kts+1,ktf
        DO i = i_start_f, i_end_f
          vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)

         IF ( vel .ge. 0.0 ) THEN
            qip2 = w(i+1,k,j)
            qip1 = w(i,  k,j)
            qi   = w(i-1,k,j)
            qim1 = w(i-2,k,j)
            qim2 = w(i-3,k,j)
          ELSE
            qip2 = w(i-2,k,j)
            qip1 = w(i-1,k,j)
            qi   = w(i,  k,j)
            qim1 = w(i+1,k,j)
            qim2 = w(i+2,k,j)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
         fqx(i,k) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk





        ENDDO
        ENDDO

        k = ktf+1
        DO i = i_start_f, i_end_f
          vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)

         IF ( vel .ge. 0.0 ) THEN
            qip2 = w(i+1,k,j)
            qip1 = w(i,  k,j)
            qi   = w(i-1,k,j)
            qim1 = w(i-2,k,j)
            qim2 = w(i-3,k,j)
          ELSE
            qip2 = w(i-2,k,j)
            qip1 = w(i-1,k,j)
            qi   = w(i,  k,j)
            qim1 = w(i+1,k,j)
            qim2 = w(i+2,k,j)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
         fqx(i,k) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk





        ENDDO



        IF( degrade_xs ) THEN

          DO i=i_start,i_start_f-1

            IF(i == ids+1) THEN 
              DO k=kts+1,ktf
                fqx(i,k) = 0.5*(fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)) &
                                *(w(i,k,j)+w(i-1,k,j))
              ENDDO
              k = ktf+1
              fqx(i,k) = 0.5*((2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)) &
                     *(w(i,k,j)+w(i-1,k,j))
            ENDIF

            IF(i == ids+2) THEN  
              DO k=kts+1,ktf
                vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
                fqx( i,k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                        w(i  ,k,j), w(i+1,k,j),  &
                                        vel                     )
              ENDDO
              k = ktf+1
              vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
              fqx( i,k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                      w(i  ,k,j), w(i+1,k,j),  &
                                      vel                     )
            END IF

          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          DO i = i_end_f+1, i_end+1

            IF( i == ide-1 ) THEN 
              DO k=kts+1,ktf
                fqx(i,k) = 0.5*(fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j))      &
                                  *(w(i,k,j)+w(i-1,k,j))
              ENDDO
              k = ktf+1
              fqx(i,k) = 0.5*((2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j))      &
                     *(w(i,k,j)+w(i-1,k,j))
            ENDIF

            IF( i == ide-2 ) THEN 
              DO k=kts+1,ktf
                vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
                fqx( i,k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                        w(i  ,k,j), w(i+1,k,j),  &
                                        vel                     )
              ENDDO
              k = ktf+1
              vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
              fqx( i,k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                      w(i  ,k,j), w(i+1,k,j),  &
                                      vel                     )
            ENDIF

          ENDDO

        ENDIF



        DO k=kts+1,ktf+1
          DO i = i_start, i_end
            mrdx=msftx(i,j)*rdx      
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO







      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

   IF( (config_flags%open_xs) .and. (its == ids)) THEN

       DO j = j_start, j_end
       DO k = kts+1, ktf

         uw = 0.5*(fzm(k)*(ru(its,k  ,j)+ru(its+1,k  ,j)) +  &
                   fzp(k)*(ru(its,k-1,j)+ru(its+1,k-1,j))   )
         ub = MIN( uw, 0. )

         tendency(its,k,j) = tendency(its,k,j)                     &
               - rdx*(                                             &
                       ub*(w_old(its+1,k,j) - w_old(its,k,j)) +    &
                       w(its,k,j)*(                                &
                       fzm(k)*(ru(its+1,k  ,j)-ru(its,k  ,j))+     &
                       fzp(k)*(ru(its+1,k-1,j)-ru(its,k-1,j)))     &
                                                                  )
       ENDDO
       ENDDO

       k = ktf+1
       DO j = j_start, j_end

         uw = 0.5*( (2.-fzm(k-1))*(ru(its,k-1,j)+ru(its+1,k-1,j))   &
                   -fzp(k-1)*(ru(its,k-2,j)+ru(its+1,k-2,j))   )
         ub = MIN( uw, 0. )

         tendency(its,k,j) = tendency(its,k,j)                     &
               - rdx*(                                             &
                       ub*(w_old(its+1,k,j) - w_old(its,k,j)) +    &
                       w(its,k,j)*(                                &
                             (2.-fzm(k-1))*(ru(its+1,k-1,j)-ru(its,k-1,j))-  &
                             fzp(k-1)*(ru(its+1,k-2,j)-ru(its,k-2,j)))  &
                                                                  )
       ENDDO

   ENDIF

   IF( (config_flags%open_xe) .and. (ite == ide)) THEN

       DO j = j_start, j_end
       DO k = kts+1, ktf

         uw = 0.5*(fzm(k)*(ru(ite-1,k  ,j)+ru(ite,k  ,j)) +  &
                   fzp(k)*(ru(ite-1,k-1,j)+ru(ite,k-1,j))   )
         ub = MAX( uw, 0. )

         tendency(i_end,k,j) = tendency(i_end,k,j)                     &
               - rdx*(                                                 &
                       ub*(w_old(i_end,k,j) - w_old(i_end-1,k,j)) +    &
                       w(i_end,k,j)*(                                  &
                            fzm(k)*(ru(ite,k  ,j)-ru(ite-1,k  ,j)) +   &
                            fzp(k)*(ru(ite,k-1,j)-ru(ite-1,k-1,j)))    &
                                                                    )
       ENDDO
       ENDDO

       k = ktf+1
       DO j = j_start, j_end

         uw = 0.5*( (2.-fzm(k-1))*(ru(ite-1,k-1,j)+ru(ite,k-1,j))    &
                   -fzp(k-1)*(ru(ite-1,k-2,j)+ru(ite,k-2,j))   )
         ub = MAX( uw, 0. )

         tendency(i_end,k,j) = tendency(i_end,k,j)                     &
               - rdx*(                                                 &
                       ub*(w_old(i_end,k,j) - w_old(i_end-1,k,j)) +    &
                       w(i_end,k,j)*(                                  &
                               (2.-fzm(k-1))*(ru(ite,k-1,j)-ru(ite-1,k-1,j)) -   &
                               fzp(k-1)*(ru(ite,k-2,j)-ru(ite-1,k-2,j)))    &
                                                                    )
       ENDDO

   ENDIF


   IF( (config_flags%open_ys) .and. (jts == jds)) THEN

       DO i = i_start, i_end
       DO k = kts+1, ktf

         vw = 0.5*( fzm(k)*(rv(i,k  ,jts)+rv(i,k  ,jts+1)) +  &
                    fzp(k)*(rv(i,k-1,jts)+rv(i,k-1,jts+1))   )
         vb = MIN( vw, 0. )

         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(w_old(i,k,jts+1) - w_old(i,k,jts)) +    &
                       w(i,k,jts)*(                                &
                       fzm(k)*(rv(i,k  ,jts+1)-rv(i,k  ,jts))+     &
                       fzp(k)*(rv(i,k-1,jts+1)-rv(i,k-1,jts)))     &
                                                                )
       ENDDO
       ENDDO

       k = ktf+1
       DO i = i_start, i_end
         vw = 0.5*( (2.-fzm(k-1))*(rv(i,k-1,jts)+rv(i,k-1,jts+1))    &
                   -fzp(k-1)*(rv(i,k-2,jts)+rv(i,k-2,jts+1))   )
         vb = MIN( vw, 0. )

         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(w_old(i,k,jts+1) - w_old(i,k,jts)) +    &
                       w(i,k,jts)*(                                &
                          (2.-fzm(k-1))*(rv(i,k-1,jts+1)-rv(i,k-1,jts))-     &
                          fzp(k-1)*(rv(i,k-2,jts+1)-rv(i,k-2,jts)))     &
                                                                )
       ENDDO

   ENDIF

   IF( (config_flags%open_ye) .and. (jte == jde) ) THEN

       DO i = i_start, i_end
       DO k = kts+1, ktf

         vw = 0.5*( fzm(k)*(rv(i,k  ,jte-1)+rv(i,k  ,jte)) +  &
                    fzp(k)*(rv(i,k-1,jte-1)+rv(i,k-1,jte))   )
         vb = MAX( vw, 0. )

         tendency(i,k,j_end) = tendency(i,k,j_end)                     &
               - rdy*(                                                 &
                       vb*(w_old(i,k,j_end) - w_old(i,k,j_end-1)) +    &
                       w(i,k,j_end)*(                                  &
                            fzm(k)*(rv(i,k  ,jte)-rv(i,k  ,jte-1))+    &
                            fzp(k)*(rv(i,k-1,jte)-rv(i,k-1,jte-1)))    &
                                                                      )
       ENDDO
       ENDDO

       k = ktf+1
       DO i = i_start, i_end

         vw = 0.5*( (2.-fzm(k-1))*(rv(i,k-1,jte-1)+rv(i,k-1,jte))    &
                   -fzp(k-1)*(rv(i,k-2,jte-1)+rv(i,k-2,jte))   )
         vb = MAX( vw, 0. )

         tendency(i,k,j_end) = tendency(i,k,j_end)                     &
               - rdy*(                                                 &
                       vb*(w_old(i,k,j_end) - w_old(i,k,j_end-1)) +    &
                       w(i,k,j_end)*(                                  &
                               (2.-fzm(k-1))*(rv(i,k-1,jte)-rv(i,k-1,jte-1))-    &
                               fzp(k-1)*(rv(i,k-2,jte)-rv(i,k-2,jte-1)))    &
                                                                      )
       ENDDO

   ENDIF






      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      DO i = i_start, i_end
         vflux(i,kts)=0.
         vflux(i,kte)=0.
      ENDDO





      DO j = j_start, j_end

         DO k=kts+3,ktf-1
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))

         IF( -vel .ge. 0.0 ) THEN
            qip2 = w(i,k+1,j)
            qip1 = w(i,k  ,j)
            qi   = w(i,k-1,j)
            qim1 = w(i,k-2,j)
            qim2 = w(i,k-3,j)
          ELSE
            qip2 = w(i,k-2,j)
            qip1 = w(i,k-1,j)
            qi   = w(i,k  ,j)
            qim1 = w(i,k+1,j)
            qim2 = w(i,k+2,j)
         ENDIF
    
         f0 =  1./3.*qim2 - 7./6.*qim1 + 11./6.*qi
         f1 = -1./6.*qim1 + 5./6.*qi   + 1./3. *qip1
         f2 =  1./3.*qi   + 5./6.*qip1 - 1./6. *qip2
    
         beta0 = 13./12.*(qim2 - 2.*qim1 + qi  )**2 + 1./4.*(qim2 - 4.*qim1 + 3.*qi)**2
         beta1 = 13./12.*(qim1 - 2.*qi   + qip1)**2 + 1./4.*(qim1 - qip1)**2
         beta2 = 13./12.*(qi   - 2.*qip1 + qip2)**2 + 1./4.*(qip2 - 4.*qip1 + 3.*qi)**2
    
         wi0 = gi0 / (eps + beta0)**pw
         wi1 = gi1 / (eps + beta1)**pw
         wi2 = gi2 / (eps + beta2)**pw
    
         sumwk = wi0 + wi1 + wi2
    
          vflux(i,k) = vel * (wi0*f0 + wi1*f1 + wi2*f2) / sumwk




         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))
                                   
           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux3(               &
                   w(i,k-2,j), w(i,k-1,j),   &
                   w(i,k  ,j), w(i,k+1,j), -vel )
           k = ktf
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux3(               &
                   w(i,k-2,j), w(i,k-1,j),   &
                   w(i,k  ,j), w(i,k+1,j), -vel )

           k=ktf+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))

         ENDDO

         DO k=kts+1,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzu(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO


         k = ktf+1
         DO i = i_start, i_end
           tendency(i,k,j)=tendency(i,k,j)+2.*rdzu(k-1)*(vflux(i,k))
         ENDDO

      ENDDO


END SUBROUTINE advect_weno_w


END MODULE module_advect_em

