










! 
! NOTE:  This file will be removed once encapsulation of bare ESMF 
!        calls is complete within WRF non-external source code.  
!
! NOTE:  This file was automatically generated by the findsymbol script 
!        based on WRFV2_20050512_1410.  Recipe follows:  
! 
! 1) Run the findsymbol script on a machine that has Ruby installed to 
!    generate this file.  Type "findsymbol -h" for help using findsymbol.  
!    For example:  
! >> hender IN loquat:/loquat2/hender/Ruby/FortranTools/ >> findsymbol -d /users/hender/Tasks/WRF_ESMF/WRFV2_20050512_1410_WORK/WRFV2/external/esmf_time_f90 -S ESMF_ -p -g WRFU_ -o ESMF_Mod -n module_symbols_util > & ! module_symbols_util.F90
! 
! 2) Added this comment block by hand.
!
      MODULE module_symbols_util

        USE ESMF_Mod, WRFU_ALARM => ESMF_ALARM
        USE ESMF_Mod, WRFU_ALARMCREATE => ESMF_ALARMCREATE
        USE ESMF_Mod, WRFU_ALARMDESTROY => ESMF_ALARMDESTROY
        USE ESMF_Mod, WRFU_ALARMDISABLE => ESMF_ALARMDISABLE
        USE ESMF_Mod, WRFU_ALARMENABLE => ESMF_ALARMENABLE
        USE ESMF_Mod, WRFU_ALARMGET => ESMF_ALARMGET
        USE ESMF_Mod, WRFU_ALARMISRINGING => ESMF_ALARMISRINGING
        USE ESMF_Mod, WRFU_ALARMPRINT => ESMF_ALARMPRINT
        USE ESMF_Mod, WRFU_ALARMRINGEROFF => ESMF_ALARMRINGEROFF
        USE ESMF_Mod, WRFU_ALARMRINGERON => ESMF_ALARMRINGERON
        USE ESMF_Mod, WRFU_ALARMSET => ESMF_ALARMSET
        USE ESMF_Mod, WRFU_ALARMVALIDATE => ESMF_ALARMVALIDATE
        USE ESMF_Mod, WRFU_ATTRIBUTE => ESMF_ATTRIBUTE
        USE ESMF_Mod, WRFU_ATTRIBUTECOPY => ESMF_ATTRIBUTECOPY
        USE ESMF_Mod, WRFU_ATTRIBUTECOPYALL => ESMF_ATTRIBUTECOPYALL
        USE ESMF_Mod, WRFU_ATTRIBUTEGET => ESMF_ATTRIBUTEGET
        USE ESMF_Mod, WRFU_ATTRIBUTEGETBYNUMBER => ESMF_ATTRIBUTEGETBYNUMBER
        USE ESMF_Mod, WRFU_ATTRIBUTEGETCOUNT => ESMF_ATTRIBUTEGETCOUNT
        USE ESMF_Mod, WRFU_ATTRIBUTEGETLIST => ESMF_ATTRIBUTEGETLIST
        USE ESMF_Mod, WRFU_ATTRIBUTEGETNAMELIST => ESMF_ATTRIBUTEGETNAMELIST
        USE ESMF_Mod, WRFU_ATTRIBUTEGETOBJECTLIST => ESMF_ATTRIBUTEGETOBJECTLIST
        USE ESMF_Mod, WRFU_ATTRIBUTESET => ESMF_ATTRIBUTESET
        USE ESMF_Mod, WRFU_ATTRIBUTESETLIST => ESMF_ATTRIBUTESETLIST
        USE ESMF_Mod, WRFU_ATTRIBUTESETOBJECTLIST => ESMF_ATTRIBUTESETOBJECTLIST
        USE ESMF_Mod, WRFU_AXISINDEX => ESMF_AXISINDEX
        USE ESMF_Mod, WRFU_AXISINDEXGET => ESMF_AXISINDEXGET
        USE ESMF_Mod, WRFU_BAD_POINTER => ESMF_BAD_POINTER
        USE ESMF_Mod, WRFU_BASE => ESMF_BASE
        USE ESMF_Mod, WRFU_BASETIME => ESMF_BASETIME
        USE ESMF_Mod, WRFU_CALENDAR => ESMF_CALENDAR
        USE ESMF_Mod, WRFU_CALENDARTYPE => ESMF_CALENDARTYPE
        USE ESMF_Mod, WRFU_CAL_360DAY => ESMF_CAL_360DAY
        USE ESMF_Mod, WRFU_CAL_GREGORIAN => ESMF_CAL_GREGORIAN
        USE ESMF_Mod, WRFU_CAL_NOCALENDAR => ESMF_CAL_NOCALENDAR
        USE ESMF_Mod, WRFU_CAL_NOLEAP => ESMF_CAL_NOLEAP
        USE ESMF_Mod, WRFU_CLOCK => ESMF_CLOCK
        USE ESMF_Mod, WRFU_CLOCKADDALARM => ESMF_CLOCKADDALARM
        USE ESMF_Mod, WRFU_CLOCKADVANCE => ESMF_CLOCKADVANCE
        USE ESMF_Mod, WRFU_CLOCKCREATE => ESMF_CLOCKCREATE
        USE ESMF_Mod, WRFU_CLOCKDESTROY => ESMF_CLOCKDESTROY
        USE ESMF_Mod, WRFU_CLOCKGET => ESMF_CLOCKGET
        USE ESMF_Mod, WRFU_CLOCKGETALARMLIST => ESMF_CLOCKGETALARMLIST
        USE ESMF_Mod, WRFU_CLOCKISSTOPTIME => ESMF_CLOCKISSTOPTIME
        USE ESMF_Mod, WRFU_CLOCKPRINT => ESMF_CLOCKPRINT
        USE ESMF_Mod, WRFU_CLOCKSET => ESMF_CLOCKSET
        USE ESMF_Mod, WRFU_CLOCKSTOPTIMEDISABLE => ESMF_CLOCKSTOPTIMEDISABLE
        USE ESMF_Mod, WRFU_CLOCKVALIDATE => ESMF_CLOCKVALIDATE
        USE ESMF_Mod, WRFU_DATATYPE => ESMF_DATATYPE
        USE ESMF_Mod, WRFU_DATATYPESTRING => ESMF_DATATYPESTRING
        USE ESMF_Mod, WRFU_DATAVALUE => ESMF_DATAVALUE
        USE ESMF_Mod, WRFU_DATA_CHARACTER => ESMF_DATA_CHARACTER
        USE ESMF_Mod, WRFU_DATA_INTEGER => ESMF_DATA_INTEGER
        USE ESMF_Mod, WRFU_DATA_LOGICAL => ESMF_DATA_LOGICAL
        USE ESMF_Mod, WRFU_DATA_REAL => ESMF_DATA_REAL
        USE ESMF_Mod, WRFU_FAILURE => ESMF_FAILURE
        USE ESMF_Mod, WRFU_FINALIZE => ESMF_FINALIZE
        USE ESMF_Mod, WRFU_FRACTION => ESMF_FRACTION
        USE ESMF_Mod, WRFU_GETNAME => ESMF_GETNAME
        USE ESMF_Mod, WRFU_GETPOINTER => ESMF_GETPOINTER
        USE ESMF_Mod, WRFU_GRID => ESMF_GRID
        USE ESMF_Mod, WRFU_GRIDCOMP => ESMF_GRIDCOMP
        USE ESMF_Mod, WRFU_INITIALIZE => ESMF_INITIALIZE
        USE ESMF_Mod, WRFU_ISINITIALIZED => ESMF_ISINITIALIZED
        USE ESMF_Mod, WRFU_KIND_C16 => ESMF_KIND_C16
        USE ESMF_Mod, WRFU_KIND_C8 => ESMF_KIND_C8
        USE ESMF_Mod, WRFU_KIND_I1 => ESMF_KIND_I1
        USE ESMF_Mod, WRFU_KIND_I2 => ESMF_KIND_I2
        USE ESMF_Mod, WRFU_KIND_I4 => ESMF_KIND_I4
        USE ESMF_Mod, WRFU_KIND_I8 => ESMF_KIND_I8
        USE ESMF_Mod, WRFU_KIND_R4 => ESMF_KIND_R4
        USE ESMF_Mod, WRFU_KIND_R8 => ESMF_KIND_R8
        USE ESMF_Mod, WRFU_LOG => ESMF_LOG
        USE ESMF_Mod, WRFU_LOGICAL => ESMF_LOGICAL
        USE ESMF_Mod, WRFU_LOGWRITE => ESMF_LOGWRITE
        USE ESMF_Mod, WRFU_LOG_ERROR => ESMF_LOG_ERROR
        USE ESMF_Mod, WRFU_LOG_INFO => ESMF_LOG_INFO
        USE ESMF_Mod, WRFU_LOG_WARNING => ESMF_LOG_WARNING
        USE ESMF_Mod, WRFU_MAJOR_VERSION => ESMF_MAJOR_VERSION
        USE ESMF_Mod, WRFU_MAXDECOMPDIM => ESMF_MAXDECOMPDIM
        USE ESMF_Mod, WRFU_MAXDIM => ESMF_MAXDIM
        USE ESMF_Mod, WRFU_MAXGRIDDIM => ESMF_MAXGRIDDIM
        USE ESMF_Mod, WRFU_MAXSTR => ESMF_MAXSTR
        USE ESMF_Mod, WRFU_MINOR_VERSION => ESMF_MINOR_VERSION
        USE ESMF_Mod, WRFU_MSGTYPE => ESMF_MSGTYPE
        USE ESMF_Mod, WRFU_NULL_POINTER => ESMF_NULL_POINTER
        USE ESMF_Mod, WRFU_POINTER => ESMF_POINTER
        USE ESMF_Mod, WRFU_REVISION => ESMF_REVISION
        USE ESMF_Mod, WRFU_SETNAME => ESMF_SETNAME
        USE ESMF_Mod, WRFU_SETNULLPOINTER => ESMF_SETNULLPOINTER
        USE ESMF_Mod, WRFU_SETPOINTER => ESMF_SETPOINTER
        USE ESMF_Mod, WRFU_STATE => ESMF_STATE
        USE ESMF_Mod, WRFU_STATE_INVALID => ESMF_STATE_INVALID
        USE ESMF_Mod, WRFU_STATUS => ESMF_STATUS
        USE ESMF_Mod, WRFU_STATUSSTRING => ESMF_STATUSSTRING
        USE ESMF_Mod, WRFU_SUCCESS => ESMF_SUCCESS
        USE ESMF_Mod, WRFU_TIME => ESMF_TIME
        USE ESMF_Mod, WRFU_TIMEEQ => ESMF_TIMEEQ
        USE ESMF_Mod, WRFU_TIMEGET => ESMF_TIMEGET
        USE ESMF_Mod, WRFU_TIMEINTERVAL => ESMF_TIMEINTERVAL
        USE ESMF_Mod, WRFU_TIMEINTERVALABSVALUE => ESMF_TIMEINTERVALABSVALUE
        USE ESMF_Mod, WRFU_TIMEINTERVALDIVQUOT => ESMF_TIMEINTERVALDIVQUOT
        USE ESMF_Mod, WRFU_TIMEINTERVALGET => ESMF_TIMEINTERVALGET
        USE ESMF_Mod, WRFU_TIMEINTERVALNEGABSVALUE => ESMF_TIMEINTERVALNEGABSVALUE
        USE ESMF_Mod, WRFU_TIMEINTERVALSET => ESMF_TIMEINTERVALSET
        USE ESMF_Mod, WRFU_TIMESET => ESMF_TIMESET
        USE ESMF_Mod, WRFU_VERSION_STRING => ESMF_VERSION_STRING
        USE ESMF_Mod, WRFU_VM => ESMF_VM

      END MODULE module_symbols_util

