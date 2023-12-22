



























MODULE module_crm_phy

CONTAINS



   SUBROUTINE crm_lw_rad(rthraten,olr,z_i    &
                       ,lwf0, lwf1, lwf2, lwf3      &
                       ,p8w,p3d,pi3d,t8w            &
                       ,z,dz8w,rho3d,qv3d,qc3d,qr3d &
                       ,ids,ide, jds,jde, kds,kde   & 
                       ,ims,ime, jms,jme, kms,kme   &
                       ,its,ite, jts,jte, kts,kte   )










   IMPLICIT NONE





   INTEGER, INTENT(IN )      ::      ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte



  REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         INTENT(INOUT)  ::                              RTHRATEN

  REAL, DIMENSION( ims:ime, jms:jme )                          , &
         INTENT(INOUT)  ::                                   OLR

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme )       , &
         INTENT(IN   ) ::                          z,    &  
                                                    dz8w, &
                                                    rho3D,&
                                                    qv3D, &
                                                    qc3D, &
                                                    qr3D

  REAL, DIMENSION( ims:ime, jms:jme ), INTENT(  OUT)  :: z_i 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(  OUT)  :: lwf0, lwf1, lwf2, lwf3



  REAL, DIMENSION( ims:ime, kms:kme, jms:jme )       , &
         INTENT(IN   ) ::                          p8w, &  
                                                    p3d, &
                                                    pi3d,&
                                                    t8w




  REAL, DIMENSION( kms:kme ) ::  Q0, Q1, term1, term2, term3a, term3b

  REAL :: qt, tmp

  INTEGER:: i,j,k,kk



   REAL,PARAMETER ::  kappa = 85, & 
                        F0    = 70, & 
                        F1    = 22    

   REAL,PARAMETER :: a     = 1.0, & 
                       rho_i = 1.12, & 
                       cp    = 1004, & 
                       qt_i  = 0.008,    & 
                       D     = 3.75E-6 













  j_loop: DO J=jts,jte
  i_loop: DO I=its,ite

    

    do k = kts, kte

      Q0(k) = 0.0
      Q1(k) = 0.0

      do kk = k, kte
        tmp = kappa * rho3D(i,kk,j) * (qc3D(i,kk,j) + qr3D(i,kk,j)) * dz8w(i,kk,j)
        Q0(k) = Q0(k) + tmp
      end do

      do kk = kts, k
        tmp = kappa * rho3D(i,kk,j) * (qc3D(i,kk,j) + qr3D(i,kk,j)) * dz8w(i,kk,j)
        Q1(k) = Q1(k) + tmp
      end do

    end do

    

    do k = kts, kte
      term1(k) = F0 * exp(- Q0(k))
      term2(k) = F1 * exp(- Q1(k))
    end do

    

    do k = kts, kte 

      qt = (qv3D(i,k,j) + qc3D(i,k,j) + qr3D(i,k,j))

      if (qt .lt. qt_i) then
        z_i(i,j) = z(i,k,j) 
	
	
	
        exit 
      end if

    end do

    

    do k = kts, kte

      term3a(k) = a * rho_i * cp * D

      
      if (z(i,k,j) .lt. z_i(i,j)) then
        term3b(k) = 0.0 
      else
        term3b(k) = 0.25 * (z(i,k,j) - z_i(i,j))**(4./3.) + z_i(i,j)*(z(i,k,j) - z_i(i,j))**(1./3.)
      end if

      

      
 
      lwf0(i,k,j) = term1(k) + term2(k) + term3a(k) * term3b(k)

      lwf1(i,k,j) = term1(k)
      lwf2(i,k,j) = term2(k)
      lwf3(i,k,j) = term3a(k) * term3b(k)

    end do 

    OLR(i,j) = lwf0(i,kte,j)

    
    
    do k = kts, kte -1
      RTHRATEN(i,k,j) = - 1 * (lwf0(i,k+1,j) - lwf0(i,k,j)) / dz8w(i,k,j) / (rho3D(i,k,j) * cp)
    end do

  END DO i_loop
  END DO j_loop                                           


  END SUBROUTINE crm_lw_rad


END MODULE module_crm_phy


