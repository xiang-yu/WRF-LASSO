






















  module molec_diff

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  use module_cam_support,   only: iulog, t_stopf, t_startf
  implicit none
  private       
  save

  public init_molec_diff 
  public compute_molec_diff 
  public vd_lu_qdecomp

  
  
  

  integer,  parameter   :: r8 = selected_real_kind(12) 
  
  real(r8), parameter   :: km_fac = 3.55E-7_r8         
  real(r8), parameter   :: pr_num = 1._r8              
  real(r8), parameter   :: pwr    = 2._r8/3._r8        
  real(r8), parameter   :: d0     = 1.52E20_r8         
                                                       
                                                       

  real(r8)              :: rair                        
  real(r8)              :: mw_dry                      
  real(r8)              :: n_avog                      
  real(r8)              :: gravit     
  real(r8)              :: cpair
  real(r8)              :: kbtz                        

  integer               :: ntop_molec                  
  integer               :: nbot_molec                  
  real(r8), allocatable :: mw_fac(:)                   
  
  contains

  
  
  

  subroutine init_molec_diff( kind, ncnst, rair_in, ntop_molec_in, nbot_molec_in, &
                              mw_dry_in, n_avog_in, gravit_in, cpair_in, kbtz_in )
    
    use constituents,     only : cnst_mw
    use upper_bc,         only : ubc_init
    
    integer,  intent(in)  :: kind           
    integer,  intent(in)  :: ncnst          
    integer,  intent(in)  :: ntop_molec_in  
    integer,  intent(in)  :: nbot_molec_in  
    real(r8), intent(in)  :: rair_in
    real(r8), intent(in)  :: mw_dry_in      
    real(r8), intent(in)  :: n_avog_in      
    real(r8), intent(in)  :: gravit_in
    real(r8), intent(in)  :: cpair_in
    real(r8), intent(in)  :: kbtz_in        
    integer               :: m              
    
    if( kind .ne. r8 ) then
        write(iulog,*) 'KIND of reals passed to init_molec_diff -- exiting.'
        call wrf_message(iulog)
        stop 'init_molec_diff'
    endif
    
    rair       = rair_in
    mw_dry     = mw_dry_in
    n_avog     = n_avog_in
    gravit     = gravit_in
    cpair      = cpair_in
    kbtz       = kbtz_in
    ntop_molec = ntop_molec_in
    nbot_molec = nbot_molec_in
    
  

    call ubc_init()

  
  
 
    allocate(mw_fac(ncnst))
    do m = 1, ncnst
       mw_fac(m) = d0 * mw_dry * sqrt(1._r8/mw_dry + 1._r8/cnst_mw(m)) / n_avog
    end do

  end subroutine init_molec_diff

  
  
  
  
  
  

  integer function compute_molec_diff( lchnk       ,                                                                      &
                                       pcols       , pver       , ncnst      , ncol     , t      , pmid   , pint        , &
                                       zi          , ztodt      , kvh        , kvm      , tint   , rhoi   , tmpi2       , &
                                       kq_scal     , ubc_t      , ubc_mmr    , dse_top  , cc_top , cd_top , cnst_mw_out , &
                                       cnst_fixed_ubc_out , mw_fac_out , ntop_molec_out , nbot_molec_out )
    
    use upper_bc,        only : ubc_get_vals
    use constituents,    only : cnst_mw, cnst_fixed_ubc

    
    
    
    
    integer,  intent(in)    :: pcols
    integer,  intent(in)    :: pver
    integer,  intent(in)    :: ncnst
    integer,  intent(in)    :: ncol                      
    integer,  intent(in)    :: lchnk                     
    real(r8), intent(in)    :: t(pcols,pver)             
    real(r8), intent(in)    :: pmid(pcols,pver)          
    real(r8), intent(in)    :: pint(pcols,pver+1)        
    real(r8), intent(in)    :: zi(pcols,pver+1)          
    real(r8), intent(in)    :: ztodt                     
    
    real(r8), intent(inout) :: kvh(pcols,pver+1)         
    real(r8), intent(inout) :: kvm(pcols,pver+1)         
    real(r8), intent(inout) :: tint(pcols,pver+1)        
    real(r8), intent(inout) :: rhoi(pcols,pver+1)        
    real(r8), intent(inout) :: tmpi2(pcols,pver+1)       

    real(r8), intent(out)   :: kq_scal(pcols,pver+1)     
    real(r8), intent(out)   :: ubc_mmr(pcols,ncnst)      
    real(r8), intent(out)   :: cnst_mw_out(ncnst)
    logical,  intent(out)   :: cnst_fixed_ubc_out(ncnst)
    real(r8), intent(out)   :: mw_fac_out(ncnst)
    real(r8), intent(out)   :: dse_top(pcols)            
    real(r8), intent(out)   :: cc_top(pcols)             
    real(r8), intent(out)   :: cd_top(pcols)             
    integer,  intent(out)   :: ntop_molec_out   
    integer,  intent(out)   :: nbot_molec_out   

    
    
    

    integer                 :: m                          
    integer                 :: i                          
    integer                 :: k                          
    real(r8)                :: mkvisc                     
    real(r8)                :: ubc_t(pcols)               

    
    
    

  

    call ubc_get_vals( lchnk, ncol, ntop_molec, pint, zi, ubc_t, ubc_mmr )

  

    cnst_mw_out(:ncnst)        = cnst_mw(:ncnst)
    cnst_fixed_ubc_out(:ncnst) = cnst_fixed_ubc(:ncnst)
    mw_fac_out(:ncnst)         = mw_fac(:ncnst)
    ntop_molec_out             = ntop_molec
    nbot_molec_out             = nbot_molec
    
  
  

    tint (:ncol,ntop_molec) = ubc_t(:ncol)
    rhoi (:ncol,ntop_molec) = pint(:ncol,ntop_molec) / ( rair * tint(:ncol,ntop_molec) )
    tmpi2(:ncol,ntop_molec) = ztodt * ( gravit * rhoi(:ncol,ntop_molec))**2 &
                                    / ( pmid(:ncol,ntop_molec) - pint(:ncol,ntop_molec) )
    
  
  

    kq_scal(:ncol,1:ntop_molec-1) = 0._r8
    do k = ntop_molec, nbot_molec
       do i = 1, ncol
          mkvisc       = km_fac * tint(i,k)**pwr / rhoi(i,k)
          kvm(i,k)     = kvm(i,k) + mkvisc
          kvh(i,k)     = kvh(i,k) + mkvisc * pr_num
          kq_scal(i,k) = sqrt(tint(i,k)) / rhoi(i,k)
       end do
    end do
    kq_scal(:ncol,nbot_molec+1:pver+1) = 0._r8
    
  

    dse_top(:ncol) = cpair * tint(:ncol,ntop_molec) + gravit * zi(:ncol,ntop_molec)

  

    do i = 1, ncol
       cc_top(i) = ztodt * gravit**2 * rhoi(i,ntop_molec) * km_fac * ubc_t(i)**pwr / &
                   ( ( pint(i,2) - pint(i,1) ) * ( pmid(i,1) - pint(i,1) ) )
    enddo
    cd_top(:ncol) = cc_top(:ncol) * dse_top(:ncol)
    
    compute_molec_diff = 1
    return
  end function compute_molec_diff

  
  
  

  integer function vd_lu_qdecomp( pcols , pver   , ncol       , fixed_ubc  , mw     , ubc_mmr , &
                                  kv    , kq_scal, mw_facm    , tmpi       , rpdel  ,           &
                                  ca    , cc     , dnom       , ze         , rhoi   ,           &
                                  tint  , ztodt  , ntop_molec , nbot_molec , cd_top )

    
    
    
    
    

    
    
    

    integer,  intent(in)    :: pcols
    integer,  intent(in)    :: pver
    integer,  intent(in)    :: ncol                  

    integer,  intent(in)    :: ntop_molec
    integer,  intent(in)    :: nbot_molec

    logical,  intent(in)    :: fixed_ubc             
    real(r8), intent(in)    :: kv(pcols,pver+1)      
    real(r8), intent(in)    :: kq_scal(pcols,pver+1) 
    real(r8), intent(in)    :: mw                    
    real(r8), intent(in)    :: ubc_mmr(pcols)        
    real(r8), intent(in)    :: mw_facm               
    real(r8), intent(in)    :: tmpi(pcols,pver+1)    
    real(r8), intent(in)    :: rpdel(pcols,pver)     
    real(r8), intent(in)    :: rhoi(pcols,pver+1)    
    real(r8), intent(in)    :: tint(pcols,pver+1)    
    real(r8), intent(in)    :: ztodt                 

    real(r8), intent(inout) :: ca(pcols,pver)        
    real(r8), intent(inout) :: cc(pcols,pver)        
    real(r8), intent(inout) :: dnom(pcols,pver)      
    real(r8), intent(inout) :: ze(pcols,pver)        

    real(r8), intent(out)   :: cd_top(pcols)         

    
    
    

    integer                 :: i                     
    integer                 :: k, kp1                

    real(r8)                :: rghd(pcols,pver+1)    
    real(r8)                :: kmq(ncol)             
    real(r8)                :: wrk0(ncol)            
    real(r8)                :: wrk1(ncol)            

    real(r8)                :: cb(pcols,pver)        
    real(r8)                :: kvq(pcols,pver+1)     

    
    
    

    
    
    
    
    

    call t_startf('vd_lu_qdecomp')

    kvq(:,:)  = 0._r8
    cd_top(:) = 0._r8

  

    do k = ntop_molec, nbot_molec
       do i = 1, ncol
          rghd(i,k) = gravit / ( kbtz * n_avog * tint(i,k) ) * ( mw - mw_dry )
          rghd(i,k) = ztodt * gravit * rhoi(i,k) * rghd(i,k) 
       enddo
    enddo

    
    
    

    do k = nbot_molec - 1, ntop_molec, -1
       kp1 = k + 1
       kmq(:ncol)  = kq_scal(:ncol,kp1) * mw_facm
       wrk0(:ncol) = ( kv(:ncol,kp1) + kmq(:ncol) ) * tmpi(:ncol,kp1)
       wrk1(:ncol) = kmq(:ncol) * 0.5_r8 * rghd(:ncol,kp1)
     
       ca(:ncol,k  )  = ( wrk0(:ncol) - wrk1(:ncol) ) * rpdel(:ncol,k)
       cc(:ncol,kp1)  = ( wrk0(:ncol) + wrk1(:ncol) ) * rpdel(:ncol,kp1)
       kvq(:ncol,kp1) = kmq(:ncol)
    end do

    if( fixed_ubc ) then
        cc(:ncol,ntop_molec) = kq_scal(:ncol,ntop_molec) * mw_facm                 &
                             * ( tmpi(:ncol,ntop_molec) + rghd(:ncol,ntop_molec) ) &
                             * rpdel(:ncol,ntop_molec)
    end if

  

    do k = nbot_molec - 1, ntop_molec + 1, -1
       kp1 = k + 1
       cb(:ncol,k) = 1._r8 + ca(:ncol,k) + cc(:ncol,k)                   &
                   + rpdel(:ncol,k) * ( kvq(:ncol,kp1) * rghd(:ncol,kp1) &
                   - kvq(:ncol,k) * rghd(:ncol,k) )
       kvq(:ncol,kp1) = kv(:ncol,kp1) + kvq(:ncol,kp1)
    end do

    k   = ntop_molec
    kp1 = k + 1
    if( fixed_ubc ) then
        cb(:ncol,k) = 1._r8 + ca(:ncol,k)                                 &
                    + rpdel(:ncol,k) * kvq(:ncol,kp1) * rghd(:ncol,kp1)   &
                    + kq_scal(:ncol,ntop_molec) * mw_facm                 &
                    * ( tmpi(:ncol,ntop_molec) - rghd(:ncol,ntop_molec) ) &
                    * rpdel(:ncol,ntop_molec)
    else
        cb(:ncol,k) = 1._r8 + ca(:ncol,k) &
                    + rpdel(:ncol,k) * kvq(:ncol,kp1) * rghd(:ncol,kp1)
    end if

    k   = nbot_molec
    cb(:ncol,k) = 1._r8 + cc(:ncol,k) + ca(:ncol,k) &
                - rpdel(:ncol,k) * kvq(:ncol,k)*rghd(:ncol,k)
    do k = 1, nbot_molec + 1, -1
       cb(:ncol,k) = 1._r8 + ca(:ncol,k) + cc(:ncol,k)
    end do

  

    if( fixed_ubc ) then
        cd_top(:ncol) = cc(:ncol,ntop_molec) * ubc_mmr(:ncol)
    end if

    
    
    
    
    

    do k = nbot_molec, ntop_molec + 1, -1
       dnom(:ncol,k) = 1._r8 / ( cb(:ncol,k) - ca(:ncol,k) * ze(:ncol,k+1) )
       ze(:ncol,k)   = cc(:ncol,k) * dnom(:ncol,k)
    end do
    k = ntop_molec
    dnom(:ncol,k) = 1._r8 / ( cb(:ncol,k) - ca(:ncol,k) * ze(:ncol,k+1) )

    vd_lu_qdecomp = 1
    call t_stopf('vd_lu_qdecomp')
    return

  end function vd_lu_qdecomp

  end module molec_diff
