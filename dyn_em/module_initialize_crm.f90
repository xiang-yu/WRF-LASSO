









































MODULE module_initialize_ideal

   USE module_domain
   USE module_io_domain
   USE module_state_description
   USE module_model_constants
   USE module_bc
   USE module_timing
   USE module_configure
   USE module_init_utilities
   USE module_soil_pre 
   USE module_dm


CONTAINS











   SUBROUTINE init_domain ( grid )

   IMPLICIT NONE

   
   TYPE (domain), POINTER :: grid 
   
   INTEGER :: idum1, idum2

   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

     CALL init_domain_rk( grid &







,grid%moist,grid%moist_bxs,grid%moist_bxe,grid%moist_bys,grid%moist_bye,grid%moist_btxs,grid%moist_btxe,grid%moist_btys, &
grid%moist_btye,grid%dfi_moist,grid%dfi_moist_bxs,grid%dfi_moist_bxe,grid%dfi_moist_bys,grid%dfi_moist_bye,grid%dfi_moist_btxs, &
grid%dfi_moist_btxe,grid%dfi_moist_btys,grid%dfi_moist_btye,grid%scalar,grid%scalar_bxs,grid%scalar_bxe,grid%scalar_bys, &
grid%scalar_bye,grid%scalar_btxs,grid%scalar_btxe,grid%scalar_btys,grid%scalar_btye,grid%dfi_scalar,grid%dfi_scalar_bxs, &
grid%dfi_scalar_bxe,grid%dfi_scalar_bys,grid%dfi_scalar_bye,grid%dfi_scalar_btxs,grid%dfi_scalar_btxe,grid%dfi_scalar_btys, &
grid%dfi_scalar_btye,grid%aerod,grid%ozmixm,grid%aerosolc_1,grid%aerosolc_2,grid%fdda3d,grid%fdda2d,grid%advh_t,grid%advz_t, &
grid%nba_mij,grid%nba_rij,grid%chem,grid%tracer,grid%tracer_bxs,grid%tracer_bxe,grid%tracer_bys,grid%tracer_bye, &
grid%tracer_btxs,grid%tracer_btxe,grid%tracer_btys,grid%tracer_btye &


                        )

   END SUBROUTINE init_domain



   SUBROUTINE init_domain_rk ( grid &







,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,nba_mij,nba_rij,chem,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys, &
tracer_btye &


)
   IMPLICIT NONE

   
   TYPE (domain), POINTER :: grid







real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerod)           :: aerod
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm33:grid%em33,num_ozmixm)           :: ozmixm
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_1
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_fdda3d)           :: fdda3d
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_fdda2d)           :: fdda2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_t)           :: advh_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_t)           :: advz_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_mij)           :: nba_mij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_rij)           :: nba_rij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)           :: tracer
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btye


   TYPE (grid_config_rec_type)              :: config_flags

   
   INTEGER                             ::                       &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  its, ite, jts, jte, kts, kte, &
                                  i, j, k

   

   INTEGER, PARAMETER :: nl_max = 30000
   REAL, DIMENSION(nl_max) :: zk, p_in, theta, rho, u, v, qv, pd_in
   INTEGER :: nl_in


   INTEGER :: icm,jcm, ii, im1, jj, jm1, loop, error, fid, nxc, nyc
   REAL    :: u_mean,v_mean, f0, p_surf, p_level, qvf, z_at_v, z_at_u
   REAL    :: z_scale, cof1, cof2

   REAL    :: hm
   REAL    :: pi

   character (len=256) :: mminlu2 


   REAL    :: vnu, xnu, xnus, dinit0, cbh, p0_temp, t0_temp, zd, zw
   REAL    :: qvf1, qvf2, pd_surf
   INTEGER :: it
   real :: thtmp, ptmp, temp(3)

   LOGICAL :: moisture_init
   
   LOGICAL :: dry_sounding
   INTEGER :: stretch_grid 

  
  

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor


   integer :: seedsize,n
   integer,allocatable :: seed(:)

   real,allocatable :: randxy(:,:)
   real,allocatable :: randz(:)

   integer :: nl_grid_in
   real :: z_level  
   real, dimension(nl_max) :: zw_grid_in 

   











   SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
   kds = grid%sd31 ; kde = grid%ed31 ;
   ids = grid%sd32 ; ide = grid%ed32 ;
   jds = grid%sd33 ; jde = grid%ed33 ;

   kms = grid%sm31 ; kme = grid%em31 ;
   ims = grid%sm32 ; ime = grid%em32 ;
   jms = grid%sm33 ; jme = grid%em33 ;

   kts = grid%sp31 ; kte = grid%ep31 ;   
   its = grid%sp32 ; ite = grid%ep32 ;   
   jts = grid%sp33 ; jte = grid%ep33 ;   
         CASE ( DATA_ORDER_XYZ )
   ids = grid%sd31 ; ide = grid%ed31 ;
   jds = grid%sd32 ; jde = grid%ed32 ;
   kds = grid%sd33 ; kde = grid%ed33 ;

   ims = grid%sm31 ; ime = grid%em31 ;
   jms = grid%sm32 ; jme = grid%em32 ;
   kms = grid%sm33 ; kme = grid%em33 ;

   its = grid%sp31 ; ite = grid%ep31 ;   
   jts = grid%sp32 ; jte = grid%ep32 ;   
   kts = grid%sp33 ; kte = grid%ep33 ;   
         CASE ( DATA_ORDER_XZY )
   ids = grid%sd31 ; ide = grid%ed31 ;
   kds = grid%sd32 ; kde = grid%ed32 ;
   jds = grid%sd33 ; jde = grid%ed33 ;

   ims = grid%sm31 ; ime = grid%em31 ;
   kms = grid%sm32 ; kme = grid%em32 ;
   jms = grid%sm33 ; jme = grid%em33 ;

   its = grid%sp31 ; ite = grid%ep31 ;   
   kts = grid%sp32 ; kte = grid%ep32 ;   
   jts = grid%sp33 ; jte = grid%ep33 ;   

   END SELECT



   pi = 2.*asin(1.0)
   write(6,*) ' pi is ',pi

   
   nxc = (ide-ids)/2
   nyc = (jde-jds)/2

  
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   
   CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )

   moisture_init = .true.

    grid%itimestep=0

   CALL wrf_dm_bcast_bytes( icm , 4 )
   CALL wrf_dm_bcast_bytes( jcm , 4 )






 

    
    
    
    

    CALL nl_set_mminlu(1, '    ')
    CALL nl_set_iswater(1,16)      
    CALL nl_set_isice(1,3)         

    

    
    CALL nl_set_cen_lat(1,30.)
    CALL nl_set_cen_lon(1,-140.)

    CALL nl_set_truelat1(1,0.)
    CALL nl_set_truelat2(1,0.)
    CALL nl_set_moad_cen_lat (1,0.) 
    CALL nl_set_stand_lon (1,0.)
    CALL nl_set_pole_lon (1,0.)
    CALL nl_set_pole_lat (1,90.)
    CALL nl_set_map_proj(1,0)
    

    CALL nl_get_iswater(1,grid%iswater)




    DO j = jts, jte
      DO i = its, ite

         grid%msftx(i,j)    = 1.
         grid%msfty(i,j)    = 1.
         grid%msfux(i,j)    = 1.
         grid%msfuy(i,j)    = 1.
         grid%msfvx(i,j)    = 1.
         grid%msfvx_inv(i,j)= 1.
         grid%msfvy(i,j)    = 1.
         grid%sina(i,j)     = 0.
         grid%cosa(i,j)     = 1.

         
         
         
         
 
         
         grid%xlat(i,j)     = config_flags%crm_lat
         grid%xlong(i,j)    = config_flags%crm_lon


         
         
                                                    
         grid%e(i,j)        = 0. 
         
         grid%f(i,j)        = 0.
                                                   
         
         grid%xland(i,j)     = 1.  
         grid%lu_index(i,j)  = 18  

         grid%albedo(i,j)   = config_flags%crm_albedo
         grid%tsk(i,j)      = config_flags%crm_tsk

	 grid%tmn(i,j) = grid%tsk(i,j) - 5  

      END DO
    END DO

    write(6,*) ' Coriolis e is ', grid%e(2,2), '  f is ', grid%f(2,2)
    write(6,*) ' xlat is ', grid%xlat(2,2), '  xlong is ', grid%xlong(2,2)
    write(6,*) ' Initial albedo is ', grid%albedo(2,2)

 
   

    other_masked_fields : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

    CASE (SLABSCHEME) 

       

    CASE (LSMSCHEME)  

        DO j = jts , MIN(jde-1,jte)
           DO i = its , MIN(ide-1,ite)
              IF (grid%xland(i,j) .lt. 1.5) THEN
                 grid%vegfra(i,j) = 0.5
                 grid%canwat(i,j) = 0.
                 grid%ivgtyp(i,j) = 18
                 grid%isltyp(i,j) = 8
                 grid%xice(i,j) = 0.
                 grid%snow(i,j) = 0.
              ELSE
                 grid%vegfra(i,j) = 0.
                 grid%canwat(i,j) = 0.
                 grid%ivgtyp(i,j) = 16
                 grid%isltyp(i,j) = 14
                 grid%xice(i,j) = 0.
                 grid%snow(i,j) = 0.
              ENDIF
           END DO
        END DO

     CASE (RUCLSMSCHEME) 

        

   END SELECT other_masked_fields


   DO j = jts, jte
   DO k = kts, kte
   DO i = its, ite
   
      grid%ww(i,k,j)     = 0. 

   END DO
   END DO
   END DO

   grid%step_number = 0 


   CALL process_soil_ideal(grid%xland,grid%xice,grid%vegfra,grid%snow,grid%canwat, &
                                     grid%ivgtyp,grid%isltyp,grid%tslb,grid%smois, &
              grid%tsk,grid%tmn,grid%zs,grid%dzs,model_config_rec%num_soil_layers, &
                model_config_rec%sf_surface_physics(grid%id),&
                                   ids,ide, jds,jde, kds,kde,&
                                   ims,ime, jms,jme, kms,kme,&
                                   its,ite, jts,jte, kts,kte )














   stretch_grid = config_flags%crm_stretch 



   grid%rdx = 1./config_flags%dx
   grid%rdy = 1./config_flags%dy 




  dry_sounding = .true.
  IF ( wrf_dm_on_monitor() ) THEN
  write(6,*) ' getting dry sounding for base state '
  ENDIF

  

  CALL get_sounding(config_flags%crm_zsfc, zk, p_in, pd_in, theta, rho, &
                    u, v, qv, dry_sounding, nl_max, nl_in )

  CALL wrf_dm_bcast_real( zk , nl_max )
  CALL wrf_dm_bcast_real( p_in , nl_max )
  CALL wrf_dm_bcast_real( pd_in , nl_max )
  CALL wrf_dm_bcast_real( theta , nl_max )
  CALL wrf_dm_bcast_real( rho , nl_max )
  CALL wrf_dm_bcast_real( u , nl_max )
  CALL wrf_dm_bcast_real( v , nl_max )
  CALL wrf_dm_bcast_real( qv , nl_max )
  CALL wrf_dm_bcast_integer ( nl_in , 1 ) 

  write(6,*) ' returned from reading sounding, nl_in is ',nl_in

  
  DO j=jts,jte
  DO i=its,ite
    grid%ht(i,j) = config_flags%crm_zsfc 
  ENDDO
  ENDDO
  
  write(6,*) 'surface height is ',grid%ht(1,1), ' m'

  
  
  
  

  DO j=jts,jte
  DO i=its,ite
    grid%phb(i,1,j) = g * grid%ht(i,j)   
    grid%ph0(i,1,j) = g * grid%ht(i,j)
  ENDDO
  ENDDO



  

  if (stretch_grid .eq. 0 .or. stretch_grid .eq. 1) then

    
    
    grid%p_top = interp_0( p_in, zk, config_flags%ztop, nl_in )

    

    IF (stretch_grid .eq. 0) THEN 

      DO k=1, kde
        grid%znw(k) = 1. - float(k-1)/float(kde-1)
      ENDDO

      write(6,*) ' uniform eta grids',stretch_grid

    ELSE  

      z_scale = .40          

      DO k=1, kde
        grid%znw(k) = (exp(-float(k-1)/float(kde-1)/z_scale) - exp(-1./z_scale))/ &
                                (1.-exp(-1./z_scale))
      ENDDO

      write(6,*) ' stretched eta grids',stretch_grid

    ENDIF

   
   
   
   
   
   
   
   

   DO k=1, kde-1
    grid%dnw(k) = grid%znw(k+1) - grid%znw(k) 
    grid%rdnw(k) = 1./grid%dnw(k)
    grid%znu(k) = 0.5*(grid%znw(k+1)+grid%znw(k))
   ENDDO

   DO k=2, kde-1
    grid%dn(k) = 0.5*(grid%dnw(k)+grid%dnw(k-1))
    grid%rdn(k) = 1./grid%dn(k)
    grid%fnp(k) = .5* grid%dnw(k  )/grid%dn(k)
    grid%fnm(k) = .5* grid%dnw(k-1)/grid%dn(k)
   ENDDO

   cof1 = (2.*grid%dn(2)+grid%dn(3))/(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(2) 
   cof2 =     grid%dn(2)        /(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(3) 
   grid%cf1  = grid%fnp(2) + cof1
   grid%cf2  = grid%fnm(2) - cof1 - cof2
   grid%cf3  = cof2       

   grid%cfn  = (.5*grid%dnw(kde-1)+grid%dn(kde-1))/grid%dn(kde-1)
   grid%cfn1 = -.5*grid%dnw(kde-1)/grid%dn(kde-1)

  

   DO J = jts, jte
   DO I = its, ite

     p_surf = interp_0( p_in, zk, grid%phb(i,1,j)/g, nl_in )
     grid%mub(i,j) = p_surf-grid%p_top




     DO K = 1, kte-1
       p_level = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
       grid%pb(i,k,j) = p_level
       grid%t_init(i,k,j) = interp_0( theta, p_in, p_level, nl_in ) - t0
       grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
     ENDDO





     DO k  = 2,kte
       grid%phb(i,k,j) = grid%phb(i,k-1,j) - grid%dnw(k-1)*grid%mub(i,j)*grid%alb(i,k-1,j)
     ENDDO

    ENDDO
    ENDDO



  elseif (stretch_grid .eq. 2 .or. stretch_grid .eq. 3) then

  

    IF (stretch_grid .eq. 2) THEN 

      
      
      grid%p_top = interp_0( p_in, zk, config_flags%ztop, nl_in )

      DO k=2, kde 

        z_level = grid%ht(its,jts) + (config_flags%ztop - grid%ht(its,jts)) * float(k-1)/float(kde-1)

        DO J = jts, jte
        DO I = its, ite
          grid%phb(i,k,j) = z_level * g
        ENDDO
        ENDDO

      ENDDO

      write(6,*) ' uniform phb grids',stretch_grid

    ELSE  

      call get_zw_grid(zw_grid_in, nl_grid_in, nl_max)

      call wrf_dm_bcast_real( zw_grid_in , nl_max )
      call wrf_dm_bcast_integer ( nl_grid_in , 1 ) 

      write(6,*) ' read zw(phb) grids',stretch_grid

      if (zw_grid_in(1) .lt. config_flags%crm_zsfc) then 
        write(6,*) ' ERROR: zw_grid_in(1), crm_zsfc ', zw_grid_in(1), config_flags%crm_zsfc
        call wrf_error_fatal3("<stdin>",642,&
' first layer < crm_zsfc in namelist' )
      endif
      if (nl_grid_in .ne. kte) then
        write(6,*) ' ERROR: nl_grid_in, kte ', nl_grid_in, kte
        call wrf_error_fatal3("<stdin>",647,&
' Num of lines in grid file != Num of vertical levels in namelist' )
      endif
	  
      if (config_flags%ztop .ne. zw_grid_in(kde)) then 
        write(6,*) 'model top in namelist (ztop):', config_flags%ztop
        write(6,*) 'zw_grid_in(kde  )', zw_grid_in(kde)
        write(6,*) 'zw_grid_in(kde-1)', zw_grid_in(kde-1)
        write(6,*) 'zw_grid_in(kde) will be used for model top'
      endif

      
      grid%p_top = interp_0( p_in, zk, zw_grid_in(kde), nl_in )


  
      DO k=2, kde 

        DO J = jts, jte
        DO I = its, ite
          grid%phb(i,k,j) = zw_grid_in(k) * g
        ENDDO
        ENDDO

      ENDDO

    ENDIF 

	do k=1,kde
	    write(6,*) 'k phb/g',k, grid%phb(its,k,jts)/g
	end do 

  

   DO J = jts, jte
   DO I = its, ite

     p_surf = interp_0( p_in, zk, grid%phb(i,1,j)/g, nl_in )
     grid%mub(i,j) = p_surf - grid%p_top

     grid%pb(i,1,j) = p_surf 
     grid%t_init(i,1,j) = interp_0( theta, p_in, p_surf, nl_in ) - t0
     grid%alb(i,1,j) = (r_d/p1000mb)*(grid%t_init(i,1,j)+t0)*(grid%pb(i,1,j)/p1000mb)**cvpm

     
     

     DO K = 2, kte-1

       grid%pb(i,k,j) = grid%pb(i,k-1,j) - (grid%phb(i,k,j) - grid%phb(i,k-1,j)) / grid%alb(i,k-1,j)
       grid%t_init(i,k,j) = interp_0( theta, p_in, grid%pb(i,k,j), nl_in ) - t0
       grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm

     ENDDO

    ENDDO
    ENDDO

     DO K = 1, kte-1 

       grid%znw(k) = (grid%pb(its,k,jts) - grid%p_top) / grid%mub(its,jts)

     ENDDO


   
   
   
   
   
   
   
   

    DO k=1, kde-1
      grid%dnw(k) = grid%znw(k+1) - grid%znw(k) 
      grid%rdnw(k) = 1./grid%dnw(k)
      grid%znu(k) = 0.5*(grid%znw(k+1)+grid%znw(k))
    ENDDO

    DO k=2, kde-1
      grid%dn(k) = 0.5*(grid%dnw(k)+grid%dnw(k-1))
      grid%rdn(k) = 1./grid%dn(k)
      grid%fnp(k) = .5* grid%dnw(k  )/grid%dn(k)
      grid%fnm(k) = .5* grid%dnw(k-1)/grid%dn(k)
    ENDDO

    cof1 = (2.*grid%dn(2)+grid%dn(3))/(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(2) 
    cof2 =     grid%dn(2)        /(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(3) 
    grid%cf1  = grid%fnp(2) + cof1
    grid%cf2  = grid%fnm(2) - cof1 - cof2
    grid%cf3  = cof2       

    grid%cfn  = (.5*grid%dnw(kde-1)+grid%dn(kde-1))/grid%dn(kde-1)
    grid%cfn1 = -.5*grid%dnw(kde-1)/grid%dn(kde-1)

  else

    write(6,*) ' ERROR: crm_stretch = ', stretch_grid
    call wrf_error_fatal3("<stdin>",746,&
' crm_stretch has to be in 0-3 ' )

  endif 


  IF ( wrf_dm_on_monitor() ) THEN
    write(6,*) ' ptop is ',grid%p_top
    write(6,*) ' base state grid%mub(1,1), p_surf is ',grid%mub(1,1),grid%mub(1,1)+grid%p_top
  ENDIF






  write(6,*) ' getting moist sounding for full state '
  dry_sounding = .false.
  CALL get_sounding(config_flags%crm_zsfc, zk, p_in, pd_in, theta, rho, &
                       u, v, qv, dry_sounding, nl_max, nl_in )

  CALL wrf_dm_bcast_real( zk , nl_max )
  CALL wrf_dm_bcast_real( p_in , nl_max )
  CALL wrf_dm_bcast_real( pd_in , nl_max )
  CALL wrf_dm_bcast_real( theta , nl_max )
  CALL wrf_dm_bcast_real( rho , nl_max )
  CALL wrf_dm_bcast_real( u , nl_max )
  CALL wrf_dm_bcast_real( v , nl_max )
  CALL wrf_dm_bcast_real( qv , nl_max )
  CALL wrf_dm_bcast_integer ( nl_in , 1 ) 

  DO J = jts, min(jde-1,jte)
  DO I = its, min(ide-1,ite)




   pd_surf = interp_0( pd_in, zk, grid%phb(i,1,j)/g, nl_in )



    grid%mu_1(i,j) = pd_surf-grid%p_top - grid%mub(i,j)
    grid%mu_2(i,j) = grid%mu_1(i,j)
    grid%mu0(i,j) = grid%mu_1(i,j) + grid%mub(i,j)




    do k=1,kde-1

      p_level = grid%znu(k)*(pd_surf - grid%p_top) + grid%p_top

      moist(i,k,j,P_QV) = interp_0( qv, pd_in, p_level, nl_in )
      grid%t_1(i,k,j)          = interp_0( theta, pd_in, p_level, nl_in ) - t0
      grid%t_2(i,k,j)          = grid%t_1(i,k,j)

    enddo





    k = kte-1  

    qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k,j,P_QV))
    qvf2 = 1./(1.+qvf1)
    qvf1 = qvf1*qvf2


    grid%p(i,k,j) = - 0.5*(grid%mu_1(i,j)+qvf1*grid%mub(i,j))/grid%rdnw(k)/qvf2
    qvf = 1. + rvovrd*moist(i,k,j,P_QV)
    grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf* &
                (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
    grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)



    do k=kte-2,1,-1
      qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k+1,j,P_QV))
      qvf2 = 1./(1.+qvf1)
      qvf1 = qvf1*qvf2
      grid%p(i,k,j) = grid%p(i,k+1,j) - (grid%mu_1(i,j) + qvf1*grid%mub(i,j))/qvf2/grid%rdn(k+1)
      qvf = 1. + rvovrd*moist(i,k,j,P_QV)
      grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf* &
                  (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
      grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
    enddo






    grid%ph_1(i,1,j) = 0.
    DO k  = 2,kte
      grid%ph_1(i,k,j) = grid%ph_1(i,k-1,j) - (1./grid%rdnw(k-1))*(       &
                   (grid%mub(i,j)+grid%mu_1(i,j))*grid%al(i,k-1,j)+ &
                    grid%mu_1(i,j)*grid%alb(i,k-1,j)  )
                                                   
      grid%ph_2(i,k,j) = grid%ph_1(i,k,j) 
      grid%ph0(i,k,j) = grid%ph_1(i,k,j) + grid%phb(i,k,j)
    ENDDO

    IF ( wrf_dm_on_monitor() ) THEN
    if((i==2) .and. (j==2)) then
     write(6,*) ' grid%ph_1 calc ',grid%ph_1(2,1,2),grid%ph_1(2,2,2),&
                              grid%mu_1(2,2)+grid%mub(2,2),grid%mu_1(2,2), &
                              grid%alb(2,1,2),grid%al(1,2,1),grid%rdnw(1)
    endif
    ENDIF

  ENDDO
  ENDDO









  
  
  

  call random_seed(size=seedsize)
  allocate(seed(seedsize))

  seed = config_flags%crm_pert_random_seed
  call random_seed(put=seed)

  allocate(randxy(ids:ide-1,jds:jde-1))
  allocate(randz(1:config_flags%crm_num_pert_layers))

  call random_number(randxy)

  DO j = jts, min(jde-1,jte)

    DO i = its, min(ide-1,ite)

      seed = int(randxy(i,j)*1E9)
      call random_seed(put=seed)
      call random_number(randz)
      randz = (randz - 0.5) * 2  
      
      
      DO k = 1, config_flags%crm_num_pert_layers

           grid%t_1(i,k,j) = grid%t_1(i,k,j) + config_flags%crm_pert_amp * randz(k)
           grid%t_2(i,k,j) = grid%t_1(i,k,j)
           qvf = 1. + rvovrd * moist(i,k,j,P_QV)
           grid%alt(i,k,j) = (r_d/p1000mb) * (grid%t_1(i,k,j)+t0) * qvf * &
                        (((grid%p(i,k,j) + grid%pb(i,k,j)) / p1000mb)**cvpm)
           grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
           
           
      ENDDO

      
      DO k  = 2,kte
        grid%ph_1(i,k,j) = grid%ph_1(i,k-1,j) - (1./grid%rdnw(k-1))*(       &
                     (grid%mub(i,j)+grid%mu_1(i,j))*grid%al(i,k-1,j)+ &
                      grid%mu_1(i,j)*grid%alb(i,k-1,j)  )
                                                   
        grid%ph_2(i,k,j) = grid%ph_1(i,k,j) 
        grid%ph0(i,k,j) = grid%ph_1(i,k,j) + grid%phb(i,k,j)
      ENDDO

    ENDDO
  ENDDO

  write(6,*) ' perturbation amplitude pert_amp is ', config_flags%crm_pert_amp
  write(6,*) ' perturbation range 1 to ', config_flags%crm_num_pert_layers




   IF ( wrf_dm_on_monitor() ) THEN
   write(6,*) ' grid%mu_1 from comp ', grid%mu_1(1,1)
   write(6,*) ' full state sounding from comp, ph, grid%p, grid%al, grid%t_1, qv '
   do k=1,kde-1
     write(6,'(i5,1x,5(1x,1pe10.3))') k, grid%ph_1(1,k,1)+grid%phb(1,k,1), &
                                      grid%p(1,k,1)+grid%pb(1,k,1), grid%alt(1,k,1), &
                                      grid%t_1(1,k,1)+t0, moist(1,k,1,P_QV)
   enddo

   write(6,*) ' pert state sounding from comp, grid%ph_1, pp, alp, grid%t_1, qv '
   do k=1,kde-1
     write(6,'(i5,1x,5(1x,1pe10.3))') k, grid%ph_1(1,k,1), &
                                      grid%p(1,k,1), grid%al(1,k,1), &
                                      grid%t_1(1,k,1), moist(1,k,1,P_QV)
   enddo
   ENDIF



  DO J = jts, jte
  DO I = its, min(ide-1,ite)

    IF (j == jds) THEN
      z_at_v = grid%phb(i,1,j)/g
    ELSE IF (j == jde) THEN
      z_at_v = grid%phb(i,1,j-1)/g
    ELSE
      z_at_v = 0.5*(grid%phb(i,1,j)+grid%phb(i,1,j-1))/g
    END IF
    p_surf = interp_0( p_in, zk, z_at_v, nl_in )

    DO K = 1, kte-1
      p_level = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
      grid%v_1(i,k,j) = interp_0( v, p_in, p_level, nl_in )
      grid%v_2(i,k,j) = grid%v_1(i,k,j)
    ENDDO

  ENDDO
  ENDDO



  DO J = jts, min(jde-1,jte)
  DO I = its, ite

    IF (i == ids) THEN
      z_at_u = grid%phb(i,1,j)/g
    ELSE IF (i == ide) THEN
      z_at_u = grid%phb(i-1,1,j)/g
    ELSE
      z_at_u = 0.5*(grid%phb(i,1,j)+grid%phb(i-1,1,j))/g
    END IF

    p_surf = interp_0( p_in, zk, z_at_u, nl_in )

    DO K = 1, kte-1
      p_level = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
      grid%u_1(i,k,j) = interp_0( u, p_in, p_level, nl_in )
      grid%u_2(i,k,j) = grid%u_1(i,k,j)
    ENDDO

  ENDDO
  ENDDO



  DO J = jts, min(jde-1,jte)
  DO K = kts, kte
  DO I = its, min(ide-1,ite)
    grid%w_1(i,k,j) = 0.
    grid%w_2(i,k,j) = 0.
  ENDDO
  ENDDO
  ENDDO




  micro_select: SELECT CASE(config_flags%mp_physics)

   CASE (WDM5SCHEME, WDM6SCHEME)

      DO J = jts, min(jde-1,jte)
      DO K = kts, kte
      DO I = its, min(ide-1,ite)

        scalar(i,k,j,P_QNN) = config_flags%crm_init_ccn * 1000000 / grid%alb(i,k,j)  

      ENDDO
      ENDDO
      ENDDO

   END SELECT micro_select





  DO J = jts, min(jde-1,jte)
  DO K = kts, kte-1
  DO I = its, min(ide-1,ite)

    grid%h_diabatic(i,k,j) = 0.
    

  ENDDO
  ENDDO
  ENDDO

  IF ( wrf_dm_on_monitor() ) THEN
    DO k=1,kte-1
      grid%t_base(k) = grid%t_1(1,k,1)
      grid%qv_base(k) = moist(1,k,1,P_QV)
      grid%u_base(k) = grid%u_1(1,k,1)
      grid%v_base(k) = grid%v_1(1,k,1)
      grid%z_base(k) = 0.5*(grid%phb(1,k,1)+grid%phb(1,k+1,1)+grid%ph_1(1,k,1)+grid%ph_1(1,k+1,1))/g
    ENDDO
  ENDIF

  CALL wrf_dm_bcast_real( grid%t_base , kte )
  CALL wrf_dm_bcast_real( grid%qv_base, kte )
  CALL wrf_dm_bcast_real( grid%u_base , kte )
  CALL wrf_dm_bcast_real( grid%v_base , kte )
  CALL wrf_dm_bcast_real( grid%z_base , kte )

  DO J = jts, min(jde-1,jte)
  DO I = its, min(ide-1,ite)

     thtmp   = grid%t_2(i,1,j)+t0
     ptmp    = grid%p(i,1,j)+grid%pb(i,1,j)
     temp(1) = thtmp * (ptmp/p1000mb)**rcp
     thtmp   = grid%t_2(i,2,j)+t0
     ptmp    = grid%p(i,2,j)+grid%pb(i,2,j)
     temp(2) = thtmp * (ptmp/p1000mb)**rcp
     thtmp   = grid%t_2(i,3,j)+t0
     ptmp    = grid%p(i,3,j)+grid%pb(i,3,j)
     temp(3) = thtmp * (ptmp/p1000mb)**rcp




  ENDDO
  ENDDO

 END SUBROUTINE init_domain_rk



   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize



























      subroutine get_sounding( zsfc, zk, p, p_dry, theta, rho, &
                               u, v, qv, dry, nl_max, nl_in )
      implicit none

      integer nl_max, nl_in

      real zsfc 
      real zk(nl_max), p(nl_max), theta(nl_max), rho(nl_max), &
           u(nl_max), v(nl_max), qv(nl_max), p_dry(nl_max)
      logical dry

      integer n
      parameter(n=30000)
      logical debug
      parameter( debug = .true.)



      real p_surf, th_surf, qv_surf
      real pi_surf, pi(n)
      real h_input(n), th_input(n), qv_input(n), u_input(n), v_input(n)



      real rho_surf, p_input(n), rho_input(n)
      real pm_input(n)  



      real r
      parameter (r = r_d)
      integer k, it, nl
      real qvf, qvf1, dz



      call read_sounding( p_surf, th_surf, qv_surf, &
                          h_input, th_input, qv_input, u_input, v_input,n, nl, debug )

      if(dry) then
       do k=1,nl
         qv_input(k) = 0.
       enddo
      endif

      if(debug) write(6,*) ' number of input levels = ',nl

        nl_in = nl
        if(nl_in .gt. nl_max ) then
          write(6,*) ' too many levels for input arrays ',nl_in,nl_max
          call wrf_error_fatal3("<stdin>",1151,&
' too many levels for input arrays ' )
        end if




      do k=1,nl
        qv_input(k) = 0.001*qv_input(k)
      enddo

      p_surf = 100.*p_surf  
      qvf = 1. + rvovrd*qv_input(1) 
      rho_surf = 1./((r/p1000mb)*th_surf*qvf*((p_surf/p1000mb)**cvpm))
      pi_surf = (p_surf/p1000mb)**(r/cp)

      if(debug) then
        write(6,*) ' surface density is ',rho_surf
        write(6,*) ' surface pi is      ',pi_surf
      end if






          qvf = 1. + rvovrd*qv_input(1) 
          qvf1 = 1. + qv_input(1)
          rho_input(1) = rho_surf
          dz = h_input(1) - zsfc

          if(dz < 0.0) then

             write(6,*) "Your first input sounding level is below the surface level"
             stop "module_initialize_crm: get_sounding"
             
          endif

          do it=1,10
            pm_input(1) = p_surf &
                    - 0.5*dz*(rho_surf+rho_input(1))*g*qvf1
            rho_input(1) = 1./((r/p1000mb)*th_input(1)*qvf*((pm_input(1)/p1000mb)**cvpm))
          enddo



          do k=2,nl
            rho_input(k) = rho_input(k-1)
            dz = h_input(k)-h_input(k-1)
            qvf1 = 0.5*(2.+(qv_input(k-1)+qv_input(k)))
            qvf = 1. + rvovrd*qv_input(k)   
 
            do it=1,10
              pm_input(k) = pm_input(k-1) &
                      - 0.5*dz*(rho_input(k)+rho_input(k-1))*g*qvf1
              rho_input(k) = 1./((r/p1000mb)*th_input(k)*qvf*((pm_input(k)/p1000mb)**cvpm))
            enddo
          enddo






        p_input(nl) = pm_input(nl)

          do k=nl-1,1,-1
            dz = h_input(k+1)-h_input(k)
            p_input(k) = p_input(k+1) + 0.5*dz*(rho_input(k)+rho_input(k+1))*g
          enddo


        do k=1,nl

          zk(k) = h_input(k)
          p(k) = pm_input(k)
          p_dry(k) = p_input(k)
          theta(k) = th_input(k)
          rho(k) = rho_input(k)
          u(k) = u_input(k)
          v(k) = v_input(k)
          qv(k) = qv_input(k)

        enddo

     if(debug) then
      write(6,*) ' sounding '
      write(6,*) '  k  height(m)  press (Pa) pd(Pa) theta (K) den(kg/m^3)  u(m/s)     v(m/s)    qv(g/g) '
      do k=1,nl
        write(6,'(1x,i5,8(1x,1pe10.3))') k, zk(k), p(k), p_dry(k), theta(k), rho(k), u(k), v(k), qv(k)
      enddo

     end if

      end subroutine get_sounding



      subroutine read_sounding( ps,ts,qvs,h,th,qv,u,v,n,nl,debug )
      implicit none
      integer n,nl
      real ps,ts,qvs,h(n),th(n),qv(n),u(n),v(n)
      logical end_of_file
      logical debug

      integer k

      open(unit=10,file='input_sounding',form='formatted',status='old')
      rewind(10)
      read(10,*) ps, ts, qvs
      if(debug) then
        write(6,*) ' input sounding surface parameters '
        write(6,*) ' surface pressure (mb) ',ps
        write(6,*) ' surface pot. temp (K) ',ts
        write(6,*) ' surface mixing ratio (g/kg) ',qvs
      end if

      end_of_file = .false.
      k = 0

      do while (.not. end_of_file)

        read(10,*,end=100) h(k+1), th(k+1), qv(k+1), u(k+1), v(k+1)
        k = k+1
        if(debug) write(6,'(1x,i5,5(1x,e10.3))') k, h(k), th(k), qv(k), u(k), v(k)
        go to 110
 100    end_of_file = .true.
 110    continue
      enddo

      nl = k

      close(unit=10,status = 'keep')

      end subroutine read_sounding






      subroutine get_zw_grid( zw, nl, nl_max )








      implicit none

      integer nl_max 
      
      integer nl

      real zw(nl_max)

      logical end_of_file
      logical debug

      integer k
      parameter( debug = .true.)

      open(unit=10,file='input_zw_grid',form='formatted',status='old')
      rewind(10)

      end_of_file = .false.
      k = 0

      do while (.not. end_of_file)

        read(10,*,end=100) zw(k+1)
        k = k+1

        go to 110
 100    end_of_file = .true.
 110    continue
      enddo

      nl = k

      close(unit=10,status = 'keep')

      end subroutine get_zw_grid


END MODULE module_initialize_ideal


