  *  o   k820309    w          19.1        �
Ub                                                                                                          
       module_diag_afwa.f90 DIAG_FUNCTIONS %         @                                                     	       #P    #T    #QV              
                                       	                
                                       	                
                                       	      %         @                                                     	       #U    #V              
                                       	                
                                       	      %         @                                                    	       #T 	   #P 
             
                                  	     	                
                                  
     	      %         @                                                    	       #TK    #P    #RH    #MIXR               @                                    	                                                       	                  @                                    	                                                       	       %         @                                                    	       #TK    #RH              
                                       	                
                                       	      %         @                                                    	       #THETAEK    #PRES    #FLAG              
                                       	                
                                       	                
D                                             %         @                                                    	       #TK    #W              
                                       	                
                                       	      %         @                                                    	       #TK    #P              
                                       	                
                                       	      %         @                                                     	       #NZ    #QV    #QC     #DZ8W !   #RHO "             
                                                      
                                                      	    p          5 � p        r        5 � p        r                               
                                                       	    p          5 � p        r        5 � p        r                               
                                  !                    	    p          5 � p        r        5 � p        r                               
                                  "                    	    p          5 � p        r        5 � p        r                      %         @                                 #                           #NZ $   #TK %   #RH &   #P '   #HGT (   #SFC )   #CAPE *   #CIN +   #ZLFC ,   #PLFC -   #LIDX .   #PARCEL /             
                                  $                    
  @                               %                    	    p          5 � p        r $       5 � p        r $                              
  @                               &                    	    p          5 � p        r $       5 � p        r $                              
  @                               '                    	    p          5 � p        r $       5 � p        r $                              
                                  (                    	    p          5 � p        r $       5 � p        r $                               
                                  )                     D                                 *     	                 D                                 +     	                 D                                 ,     	                 D                                 -     	                 D                                 .     	                 
                                  /           %         @                                 0                    	       #ZSFC 1   #PSFC 2   #ZLEV1 3   #QLEV1 4   #TLEV1 5             
                                  1     	                
                                  2     	                
                                  3     	                
                                  4     	                
                                  5     	      %         @                                 6                    	       #P 7   #TK 8   #RH 9             
  @                               7     	                
  @                               8     	                
  @                               9     	      %         @                                :                    	       #P ;   #TK <   #RH =             
                                  ;     	                
  @                               <     	                
  @                               =     	      %         @                                 >                    	       #TK ?   #WSPD @             
                                  ?     	                
                                  @     	      %         @                                 A                    	       #TK B   #RH C             
                                  B     	                
                                  C     	      %         @                                D                    	       #TK E             
  @                               E     	      %         @                                F                    	       #TC G   #RH H             
                                  G     	                
                                  H     	      %         @                                 I                    	       #UGRDBOT J   #UGRDTOP K   #VGRDBOT L   #VGRDTOP M   #DEFOR11BOT N   #DEFOR11TOP O   #DEFOR12BOT P   #DEFOR12TOP Q   #DEFOR22BOT R   #DEFOR22TOP S   #ZBOT T   #ZTOP U             
                                  J     	                
                                  K     	                
                                  L     	                
                                  M     	                
                                  N     	                
                                  O     	                
                                  P     	                
                                  Q     	                
                                  R     	                
                                  S     	                
                                  T     	                
                                  U     	      %         @                                 V                    	       #X W   #F X   #Y Y             
 @                               W                   	              &                                                     
                                  X                   	              &                                                     
                                  Y     	      %         @                                 Z                    	       #NLAYER [   #U \   #V ]             
                                  [                    
                                  \                    	    p          5 � p        r [       5 � p        r [                              
                                  ]                    	    p          5 � p        r [       5 � p        r [                     %         @                                 ^                    	       #NLAYER _   #TK `   #HGT a             
                                  _                    
                                  `                    	    p          5 � p        r _       5 � p        r _                              
                                  a                    	    p          5 � p        r _       5 � p        r _                     %         @                                 b                    	       #NLAYER c   #TDX d   #TDY e   #U f   #V g   #TK h   #HGT i             
                                  c                     
                                  d     	                
                                  e     	               
                                  f                    	    p          5 � p        r c       5 � p        r c                              
                                  g                    	    p          5 � p        r c       5 � p        r c                              
                                  h                    	    p          5 � p        r c       5 � p        r c                              
                                  i                    	    p          5 � p        r c       5 � p        r c                     %         @                                 j                    	       #NLAYER k   #U l   #V m   #P n             
                                  k                    
                                  l                    	    p          5 � p        r k       5 � p        r k                              
                                  m                    	    p          5 � p        r k       5 � p        r k                              
                                  n                    	    p          5 � p        r k       5 � p        r k                        �   ,      fn#fn    �   f       CALC_RH    2  @   a   CALC_RH%P    r  @   a   CALC_RH%T    �  @   a   CALC_RH%QV    �  ^       UV_WIND    P  @   a   UV_WIND%U    �  @   a   UV_WIND%V    �  ^       THETA    .  @   a   THETA%T    n  @   a   THETA%P    �  q       THETAE      @   a   THETAE%TK    _  @   a   THETAE%P    �  @   a   THETAE%RH    �  @   a   THETAE%MIXR      `       TLCL      @   a   TLCL%TK    �  @   a   TLCL%RH    �  q       THE2T    p  @   a   THE2T%THETAEK    �  @   a   THE2T%PRES    �  @   a   THE2T%FLAG #   0  _       VIRTUALTEMPERATURE &   �  @   a   VIRTUALTEMPERATURE%TK %   �  @   a   VIRTUALTEMPERATURE%W &     _       SATURATIONMIXINGRATIO )   n  @   a   SATURATIONMIXINGRATIO%TK (   �  @   a   SATURATIONMIXINGRATIO%P    �  {       PWAT    i	  @   a   PWAT%NZ    �	  �   a   PWAT%QV    ]
  �   a   PWAT%QC      �   a   PWAT%DZ8W    �  �   a   PWAT%RHO    y  �       BUOYANCY    7  @   a   BUOYANCY%NZ    w  �   a   BUOYANCY%TK    +  �   a   BUOYANCY%RH    �  �   a   BUOYANCY%P    �  �   a   BUOYANCY%HGT    G  @   a   BUOYANCY%SFC    �  @   a   BUOYANCY%CAPE    �  @   a   BUOYANCY%CIN      @   a   BUOYANCY%ZLFC    G  @   a   BUOYANCY%PLFC    �  @   a   BUOYANCY%LIDX     �  @   a   BUOYANCY%PARCEL      �       MSLP    �  @   a   MSLP%ZSFC    �  @   a   MSLP%PSFC      @   a   MSLP%ZLEV1    L  @   a   MSLP%QLEV1    �  @   a   MSLP%TLEV1    �  g       CALC_FITS    3  @   a   CALC_FITS%P    s  @   a   CALC_FITS%TK    �  @   a   CALC_FITS%RH    �  g       WETBULBTEMP    Z  @   a   WETBULBTEMP%P    �  @   a   WETBULBTEMP%TK    �  @   a   WETBULBTEMP%RH      b       CALC_WC    |  @   a   CALC_WC%TK    �  @   a   CALC_WC%WSPD    �  `       CALC_HI    \  @   a   CALC_HI%TK    �  @   a   CALC_HI%RH    �  X       CALC_ES    4  @   a   CALC_ES%TK    t  `       CALC_DEWPOINT !   �  @   a   CALC_DEWPOINT%TC !     @   a   CALC_DEWPOINT%RH    T  �       CATTURBULENCE &   L  @   a   CATTURBULENCE%UGRDBOT &   �  @   a   CATTURBULENCE%UGRDTOP &   �  @   a   CATTURBULENCE%VGRDBOT &     @   a   CATTURBULENCE%VGRDTOP )   L  @   a   CATTURBULENCE%DEFOR11BOT )   �  @   a   CATTURBULENCE%DEFOR11TOP )   �  @   a   CATTURBULENCE%DEFOR12BOT )     @   a   CATTURBULENCE%DEFOR12TOP )   L  @   a   CATTURBULENCE%DEFOR22BOT )   �  @   a   CATTURBULENCE%DEFOR22TOP #   �  @   a   CATTURBULENCE%ZBOT #     @   a   CATTURBULENCE%ZTOP    L  e       LIN_INTERP    �  �   a   LIN_INTERP%X    =  �   a   LIN_INTERP%F    �  @   a   LIN_INTERP%Y    	  j       LLT_WINDSPEED %   s  @   a   LLT_WINDSPEED%NLAYER     �  �   a   LLT_WINDSPEED%U     g   �   a   LLT_WINDSPEED%V "   !  m       LLT_THERMODYNAMIC )   �!  @   a   LLT_THERMODYNAMIC%NLAYER %   �!  �   a   LLT_THERMODYNAMIC%TK &   |"  �   a   LLT_THERMODYNAMIC%HGT !   0#  �       LLT_MOUNTAINWAVE (   �#  @   a   LLT_MOUNTAINWAVE%NLAYER %   �#  @   a   LLT_MOUNTAINWAVE%TDX %   =$  @   a   LLT_MOUNTAINWAVE%TDY #   }$  �   a   LLT_MOUNTAINWAVE%U #   1%  �   a   LLT_MOUNTAINWAVE%V $   �%  �   a   LLT_MOUNTAINWAVE%TK %   �&  �   a   LLT_MOUNTAINWAVE%HGT     M'  q       LLT_TRAPPEDWAVE '   �'  @   a   LLT_TRAPPEDWAVE%NLAYER "   �'  �   a   LLT_TRAPPEDWAVE%U "   �(  �   a   LLT_TRAPPEDWAVE%V "   f)  �   a   LLT_TRAPPEDWAVE%P 