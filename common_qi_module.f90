!-------------------------------------------------------------------------------
!
! NAME:
!    Common_QI
!
! FUNCTION:
!    Provide a Eumetsat QI version for the IWWG.
!
! DESCRIPTION:
!    This is a self-contained module for the Eumetsat QI.
!
! REFERENCE:
!    Holmlund, K., 1998: The Utilization of Statistical Properties of
!    Satellite-Derived Atmospheric Motion Vectors to Derive Quality Indicators.
!    Wea. Forecasting, 13, 1093–1104, doi:10.1175/1520-0434(1998)013<1093:TUOSPO>2.0.CO;2.
!
! CALLING SEQUENCE:
!    CALL Common_QI ( ?, ?  )
!
! INPUTS:
!    Num_Of_Input_Targets           Number of targets. Can be bad AMVs.
!
! INPUT/OUTPUT
!    Wind_Data                      Array of AMVS.
!      (i,1)  -> Latitude
!      (i,2)  -> Longitude
!      (i,3)  -> AMV U, converted to AMV Spd inside Common_QI
!      (i,4)  -> AMV V, converted to AMV Dir inside Common_QI
!      (i,5)  -> AMV U, reverse time
!      (i,6)  -> AMV V, reverse time
!      (i,7)  -> AMV U, forward time
!      (i,8)  -> AMV V, forward time
!      (i,9)  -> AMV Pressure (hPa)
!      (i,10) -> Fcst U, converted to Fcst Spd inside Common QI
!      (i,11) -> Fcst V, converted to Fcst Dir inside Common QI
!      (i,12) -> AMV Flag where 0 is a good AMV
!      (i,13) -> Final common QI
!      (i,14) -> QI Spd Flag
!      (i,15) -> QI Dir Flag
!      (i,16) -> QI Vector Flag
!      (i,17) -> QI Consistency Flag
!      (i,18) -> QI Fcst Flag
!      (i,19) -> QI Fcst Vector Difference
!      (i,20) -> Fcst Speed
!
! OUTPUTS:
!    Prints out the Common QI value.
!
! DEPENDENCIES:
!    None
!
! RESTRICTIONS:
!    None
!
! HISTORY:
!
!-------------------------------------------------------------------------------

MODULE Common_QI_Module

  IMPLICIT NONE

  INTEGER, PARAMETER :: Byte    = SELECTED_INT_KIND(1)   ! Byte  integer
  INTEGER, PARAMETER :: Short   = SELECTED_INT_KIND(4)   ! Short integer
  INTEGER, PARAMETER :: Long    = SELECTED_INT_KIND(8)   ! Long  integer
  INTEGER, PARAMETER :: Single  = SELECTED_REAL_KIND(6)  ! Single precision

  REAL(SINGLE), PARAMETER :: MISSING_VALUE_REAL4 = -999.0
  REAL(SINGLE), PARAMETER :: DEGREE0 = 0.0
  REAL(SINGLE), PARAMETER :: DEGREE90 = 90.0
  REAL(SINGLE), PARAMETER :: DEGREE180 = 180.0
  REAL(SINGLE), PARAMETER :: DEGREE270 = 270.0
  REAL(SINGLE), PARAMETER :: DEGREE360 = 360.0
  REAL(SINGLE), PARAMETER :: LOC_BOX_DEF = 1.0
  REAL(SINGLE), PARAMETER :: LOC_PRES_DIF = 50.0
  REAL(SINGLE), PARAMETER :: L1 = 0.2
  REAL(SINGLE), PARAMETER :: L2 = 1.0
  REAL(SINGLE), PARAMETER :: L3 = 3.0
  REAL(SINGLE), PARAMETER :: W_SPD = 1.0               ! AQC wind speed weight
  REAL(SINGLE), PARAMETER :: W_DIR = 1.0               ! AQC wind direction weight
  REAL(SINGLE), PARAMETER :: W_VEC = 1.0               ! AQC wind vector weight
  REAL(SINGLE), PARAMETER :: W_UC = 0.0                ! AQC wind u-component weight
  REAL(SINGLE), PARAMETER :: W_VC = 0.0                ! AQC wind v-component weight
  REAL(SINGLE), PARAMETER :: W_LC = 2.0                ! AQC buddy check weight
  !REAL(SINGLE), PARAMETER :: W_FC = 1.0               ! AQC forecast weight
  REAL(SINGLE), PARAMETER :: W_FC = 0.0                ! AQC forecast weight
  REAL(SINGLE), PARAMETER :: S1 = 0.2                  ! speed test parameter 1 (fraction)
  REAL(SINGLE), PARAMETER :: S2 = 1.0                  ! speed test parameter 2 (offset)
  REAL(SINGLE), PARAMETER :: S3 = 3.0                  ! speed test parameter 3 (exp. for normalization)
  REAL(SINGLE), PARAMETER :: D1 = 20.0                 ! direction test parameter 1  (exp. dependency)
  REAL(SINGLE), PARAMETER :: D2 = 10.0                 ! direction test parameter 2A (fraction)
  REAL(SINGLE), PARAMETER :: D22= 10.0                 ! direction test parameter 2B (offset)
  REAL(SINGLE), PARAMETER :: D3 = 4.0                  ! direction test parameter 3  (exp. for normalization)
  REAL(SINGLE), PARAMETER :: VEC1 = 0.2                ! vector test parameter 1 (fraction)
  REAL(SINGLE), PARAMETER :: VEC2 = 1.0                ! vector test parameter 2 (offset)
  REAL(SINGLE), PARAMETER :: VEC3 = 3.0                ! vector test parameter 3 (exp. for normalization)
  REAL(SINGLE), PARAMETER :: F1 = 0.4                  ! forecast parameter 1 (fraction)
  REAL(SINGLE), PARAMETER :: F2 = 1.0                  ! forecast parameter 2 (offset)
  REAL(SINGLE), PARAMETER :: F3 = 2.0                  ! forecast parameter 3 (exp. for normalization)
  REAL(SINGLE), PARAMETER :: U1 = 1.0                  ! u-component paramter 1 (fraction)
  REAL(SINGLE), PARAMETER :: U2 = 1.0                  ! u-component paramter 2 (offset)
  REAL(SINGLE), PARAMETER :: U3 = 2.0                  ! u-component paramter 3 (exp. for normalization)
  REAL(SINGLE), PARAMETER :: V1 = 1.0                  ! v-component paramter 1 (fraction)
  REAL(SINGLE), PARAMETER :: V2 = 1.0                  ! v-component paramter 2 (offset)
  REAL(SINGLE), PARAMETER :: V3 = 2.0                  ! v-component paramter 3 (exp. for normalization)
  REAL(SINGLE), PARAMETER :: MIN_SPD = 0.0             ! Minimum speed accepted by automatic quality control (AQC)
  INTEGER(SHORT), PARAMETER :: COMMON_MAX_PARAMETERS = 12    ! Number of variables in the array.
  INTEGER(LONG), PARAMETER :: MAX_RECORDS = 1700000  ! Max number of AMVS

  PRIVATE :: MISSING_VALUE_REAL4, DEGREE360, DEGREE180, W_SPD, W_DIR, W_VEC
  PRIVATE :: W_UC, W_VC, W_LC, W_FC, S1, S2, S3, D1, D2, D22, D3, VEC1, VEC2
  PRIVATE :: VEC3, L1, L2, L3, F1, F2, F3, U1, U2, U3, V1, V2, V3, MIN_SPD
  PRIVATE :: DEGREE90, LOC_BOX_DEF, LOC_PRES_DIF, DEGREE0, DEGREE270
  PRIVATE :: Byte, Short, Long, Single

  TYPE :: Common_Quality_Indicator_Variables

    REAL(SINGLE) :: U_Reverse            ! u component of first wind
    REAL(SINGLE) :: V_Reverse            ! v- component of first wind
    REAL(SINGLE) :: U_Forward            ! u component of second wind
    REAL(SINGLE) :: V_Forward            ! v-component of second wind
    REAL(SINGLE) :: Vec_Len              ! Currently the mean of the norms of the AMV
    REAL(SINGLE) :: Vel                  ! AMV speed
    REAL(SINGLE) :: Dir                  ! AMV direction
    REAL(SINGLE) :: Delta_Vec            ! Vector difference to best buddy
    REAL(SINGLE) :: Mean_Vec             ! Mean vector length of AMV and its buddy
    REAL(SINGLE) :: Vec_Diff             ! Vector difference of the AMV components
    REAL(SINGLE) :: Vel_Diff             ! Speed difference between AMV components 1
                                         ! and 2
    REAL(SINGLE) :: Dir_Diff             ! Direction difference for the AMV components
    REAL(SINGLE) :: Fcst_Vec_Diff        ! Vector difference against the forecast/
                                         ! background vector
    REAL(SINGLE) :: Fcst_Speed           ! Forecast speed
    REAL(SINGLE) :: Auto_QC_Flag         ! Final combined quality value
    REAL(SINGLE) :: Spd_Flag             ! speed consistency score
    REAL(SINGLE) :: Dir_Flag             ! direction consistency score
    REAL(SINGLE) :: Vec_Flag             ! vector consistency score
    REAL(SINGLE) :: U_Flag               ! u-component consistency score
    REAL(SINGLE) :: V_Flag               ! v-component consistency score
    REAL(SINGLE) :: Loc_Consistency_Flag ! local consistency score
    REAL(SINGLE) :: Fcst_Flag            ! forecast consistency score

  END TYPE Common_Quality_Indicator_Variables

  TYPE(Common_Quality_Indicator_Variables) :: Common_QI_Variables

  CONTAINS

  SUBROUTINE Common_QI(Wind_Data, Num_Of_Input_Targets)

    INTEGER(LONG), INTENT(IN) :: Num_Of_Input_Targets       ! Number of AMVs in Wind_Data array
    REAL(SINGLE), DIMENSION(:,:), INTENT(INOUT) :: Wind_Data  ! Array of wind data
    REAL(SINGLE), PARAMETER :: MISSING_VALUE_QI = -0.98

    !  Variable declarations
   
    REAL(SINGLE)   Lat           ! AMV latitude
    REAL(SINGLE)   Lon           ! AMV longitude
    REAL(SINGLE)   U_Forward     ! u-component of forward vector 
    REAL(SINGLE)   U_Reverse     ! u-component of reverse vector
    REAL(SINGLE)   V_Forward     ! v-component of forward vector
    REAL(SINGLE)   V_Reverse     ! v-component of reverse vector
    REAL(SINGLE)   Fcst_U        ! u-component of the forecast/background
    REAL(SINGLE)   Fcst_V        ! v-component of the forecast/background
    REAL(SINGLE)   Dir_Reverse   ! Direction components of the AMV
    REAL(SINGLE)   Dir_Forward
    REAL(SINGLE)   Speed_Reverse ! Speed components of the AMV
    REAL(SINGLE)   Speed_Forward
    REAL(SINGLE)   Fcst_Dir      ! Forecast direction
    REAL(SINGLE)   Speed         ! AMV speed
    REAL(SINGLE)   Dir           ! AMV direction
    REAL(SINGLE)   Fcst_Dir2
    REAL(SINGLE)   Fcst_Speed2
    REAL(SINGLE)   Press         ! Height of the AMV in hPa
    REAL(SINGLE)   U             ! u and v of the AMV
    REAL(SINGLE)   V
    REAL(SINGLE)   AMV_Flag      ! 0 = Good Flag.  All others non-zero.
    
    REAL(SINGLE)   Loc_Wind(COMMON_MAX_PARAMETERS, MAX_RECORDS) ! Intermediate array

    INTEGER(LONG)  Target_Index           ! Column in array
    INTEGER(LONG)  Num_Of_Vectors         ! Counter for valid number of vectors
    INTEGER(LONG)  Vector_Index           ! Index of vector

    ! ----------------------------------------------------------------------------
    ! Initialize variables
    ! ----------------------------------------------------------------------------

    print*,"IN COMMON QI : ", Num_Of_Input_Targets

    Loc_Wind(:,:) = MISSING_VALUE_REAL4
    Num_Of_Vectors = 0
    Vector_Index = 0

    DO Target_Index=1, Num_Of_Input_Targets

      Lat = Wind_Data(Target_Index, 1)
      Lon = Wind_Data(Target_Index, 2)
      U = Wind_Data(Target_Index, 3)
      V = Wind_Data(Target_Index, 4)
      U_Reverse = Wind_Data(Target_Index, 5)
      V_Reverse = Wind_Data(Target_Index, 6)
      U_Forward = Wind_Data(Target_Index, 7)
      V_Forward = Wind_Data(Target_Index, 8)
      Press = Wind_Data(Target_Index, 9)
      Fcst_U = Wind_Data(Target_Index, 10)
      Fcst_V = Wind_Data(Target_Index, 11)
      AMV_Flag = Wind_Data(Target_Index, 12)

      ! --------------------------------------------------------------------------
      ! Initialize QI
      ! --------------------------------------------------------------------------

      Wind_Data(Target_Index, 13) = MISSING_VALUE_QI 
      Wind_Data(Target_Index, 14) = MISSING_VALUE_QI
      Wind_Data(Target_Index, 15) = MISSING_VALUE_QI
      Wind_Data(Target_Index, 16) = MISSING_VALUE_QI
      Wind_Data(Target_Index, 17) = MISSING_VALUE_QI
      Wind_Data(Target_Index, 18) = MISSING_VALUE_QI
      Wind_Data(Target_Index, 19) = MISSING_VALUE_REAL4
      Wind_Data(Target_Index, 20) = MISSING_VALUE_REAL4

      ! --------------------------------------------------------------------------
      ! Fill intermediate array with good winds.
      ! --------------------------------------------------------------------------

      IF (Wind_Data(Target_Index, 12) .EQ. 0) THEN

        Num_Of_Vectors = Num_Of_Vectors + 1

        Loc_Wind(1,Num_Of_Vectors)= Lat
        Loc_Wind(2,Num_Of_Vectors)= Lon
        Loc_Wind(3,Num_Of_Vectors)= Press

        Loc_Wind(4,Num_Of_Vectors)= Wind_Data(Target_Index, 3)
        Loc_Wind(5,Num_Of_Vectors)= Wind_Data(Target_Index, 4)

        ! convert forecast u and v to speed and direction and save values
        CALL Common_UV2Spd(Fcst_Speed2,Fcst_Dir2,Fcst_U,Fcst_V)
        Wind_Data(Target_Index, 10) = Fcst_Speed2
        Wind_Data(Target_Index, 11) = Fcst_Dir2

        Loc_Wind(6,Num_Of_Vectors)= U_Reverse
        Loc_Wind(7,Num_Of_Vectors)= V_Reverse
        Loc_Wind(8,Num_Of_Vectors)= U_Forward
        Loc_Wind(9,Num_Of_Vectors)= V_Forward
        Loc_Wind(10,Num_Of_Vectors)= Fcst_U
        Loc_Wind(11,Num_Of_Vectors)= Fcst_V

      ENDIF

    ENDDO

    ! ----------------------------------------------------------------------------
    ! Loop over all targets
    ! ----------------------------------------------------------------------------

    DO Target_Index=1,Num_Of_Input_Targets

      CALL Initialize_Common_QI_Variables(Common_QI_Variables)

      IF (Wind_Data(Target_Index, 12) .EQ. 0) THEN

        Vector_Index = Vector_Index + 1

        ! convert avg u and v to speed and direction

        U = Loc_Wind(4,Vector_Index)
        V = Loc_Wind(5,Vector_Index)

        CALL Common_UV2Spd(Speed, Dir, U, V)

        Wind_Data(Target_Index, 3) = Speed
        Wind_Data(Target_Index, 4) = Dir

        ! convert u and v of first (reverse) vector to speed and direction

        Common_QI_Variables%U_Reverse = Loc_Wind(6,Vector_Index)
        Common_QI_Variables%V_Reverse = Loc_Wind(7,Vector_Index)

        CALL Common_UV2Spd(Speed_Reverse, Dir_Reverse, Common_QI_Variables%U_Reverse, &
                                                Common_QI_Variables%V_Reverse)

        ! convert u and v of second (forward) vector to speed and direction

        Common_QI_Variables%U_Forward = Loc_Wind(8,Vector_Index)
        Common_QI_Variables%V_Forward = Loc_Wind(9,Vector_Index)

        CALL Common_UV2Spd(Speed_Forward, Dir_Forward, Common_QI_Variables%U_Forward, &
                                                Common_QI_Variables%V_Forward)

        ! forecast vector
        Fcst_U = Loc_Wind(10,Vector_Index)
        Fcst_V = Loc_Wind(11,Vector_Index)

        CALL Common_UV2Spd(Common_QI_Variables%Fcst_Speed, Fcst_Dir, Fcst_U, Fcst_V)

        Common_QI_Variables%Vel_Diff = ABS(Speed_Reverse - Speed_Forward)
        Common_QI_Variables%Dir_Diff = ABS(Dir_Reverse - Dir_Forward)

        IF (Common_QI_Variables%Dir_Diff .gt. DEGREE180) THEN

          Common_QI_Variables%Dir_Diff = DEGREE360 - Common_QI_Variables%Dir_Diff

        ENDIF

        Common_QI_Variables%Vec_Diff = SQRT((Common_QI_Variables%U_Reverse - Common_QI_Variables% &
                     U_Forward)**2 + (Common_QI_Variables%V_Reverse - Common_QI_Variables% &
                                                              V_Forward)**2)

        Common_QI_Variables%Vel = (Speed_Reverse + Speed_Forward) / 2.

        !
        ! veclen is currently equal to vel. Originaly it was not the mean
        ! of the norms but the norm of the sums of the components. The
        ! latter, however, is sometimes problematic for really, really crappy
        ! vectors, hence the change.
        !

        Common_QI_Variables%Vec_Len = Common_QI_Variables%Vel
        Common_QI_Variables%Fcst_Vec_Diff=SQRT((Fcst_U-U)**2+(Fcst_V-V)**2)

        ! Get best buddy for the local consistency check

        CALL Common_Get_Best_Buddy(Loc_Wind, Vector_Index, Common_QI_Variables%Delta_Vec, &
                                     Common_QI_Variables%Mean_Vec, Num_Of_Vectors)

        ! Derive the quality flags. The final score is a weighted average of
        ! several individual test scores.

        CALL Common_MSG_Auto_QC(Common_QI_Variables)

        Wind_Data(Target_Index, 13) = Common_QI_Variables%Auto_QC_Flag
        Wind_Data(Target_Index, 14) = Common_QI_Variables%Spd_Flag
        Wind_Data(Target_Index, 15) = Common_QI_Variables%Dir_Flag
        Wind_Data(Target_Index, 16) = Common_QI_Variables%Vec_Flag
        Wind_Data(Target_Index, 17) = Common_QI_Variables%Loc_Consistency_Flag 
        Wind_Data(Target_Index, 18) = Common_QI_Variables%Fcst_Flag 
        Wind_Data(Target_Index, 19) = Common_QI_Variables%Fcst_Vec_Diff
        Wind_Data(Target_Index, 20) = Common_QI_Variables%Fcst_Speed

      ENDIF

    ENDDO

    ! print some statistics 

    PRINT "(' Total number of targets processed : ', I8)", Num_Of_Input_Targets
    PRINT "(' Total number of winds             : ', I8)", Num_Of_Vectors
    PRINT "(' --- Done Common QI --- ')"

  END SUBROUTINE Common_QI

  SUBROUTINE Initialize_Common_QI_Variables(Common_QI_Variables)

    TYPE(Common_Quality_Indicator_Variables), INTENT(INOUT) :: Common_QI_Variables
      
    Common_QI_Variables%U_Reverse = MISSING_VALUE_REAL4
    Common_QI_Variables%V_Reverse = MISSING_VALUE_REAL4
    Common_QI_Variables%U_Forward = MISSING_VALUE_REAL4
    Common_QI_Variables%V_Forward = MISSING_VALUE_REAL4
    Common_QI_Variables%Vec_Len = MISSING_VALUE_REAL4
    Common_QI_Variables%Vel = MISSING_VALUE_REAL4
    Common_QI_Variables%Dir = MISSING_VALUE_REAL4
    Common_QI_Variables%Delta_Vec = MISSING_VALUE_REAL4
    Common_QI_Variables%Mean_Vec = MISSING_VALUE_REAL4
    Common_QI_Variables%Vec_Diff = MISSING_VALUE_REAL4
    Common_QI_Variables%Vel_Diff = MISSING_VALUE_REAL4
    Common_QI_Variables%Dir_Diff = MISSING_VALUE_REAL4
    Common_QI_Variables%Fcst_Vec_Diff = MISSING_VALUE_REAL4
    Common_QI_Variables%Fcst_Speed = MISSING_VALUE_REAL4
    Common_QI_Variables%Auto_QC_Flag = MISSING_VALUE_REAL4
    Common_QI_Variables%Spd_Flag = MISSING_VALUE_REAL4
    Common_QI_Variables%Dir_Flag = MISSING_VALUE_REAL4
    Common_QI_Variables%Vec_Flag = MISSING_VALUE_REAL4
    Common_QI_Variables%U_Flag = MISSING_VALUE_REAL4
    Common_QI_Variables%V_Flag = MISSING_VALUE_REAL4
    Common_QI_Variables%Loc_Consistency_Flag = MISSING_VALUE_REAL4
    Common_QI_Variables%Fcst_Flag = MISSING_VALUE_REAL4

  END SUBROUTINE Initialize_Common_QI_Variables

  SUBROUTINE Common_UV2Spd(Spd,Dir,U,V)

  !-------------------------------------------------------------------------------
  !
  ! Name:
  !   Common_UV2Spd
  !
  ! Function:
  !   Converts the u- and v-components of a wind vector to speed and direction.
  !
  ! Description:
  !   This routine takes the u- and v-components of a wind vector and converts
  !   them to speed and direction.
  !
  ! Reference:
  !   None.
  !
  ! Calling Sequence:
  !   CALL Common_UV2Spd
  !
  ! Inputs:
  !   U                        U-component wind (a west wind is positive)
  !   V                        V-component wind (a south wind is positive)
  !
  ! Outputs:
  !   Spd                      Wind speed (m/s)
  !   Dir                      Wind direction (degrees)
  !
  ! Dependencies:
  !   None.
  !
  ! Restrictions:
  !
  ! Restrictions:
  !   None.
  !
  ! History:
  ! 08/2009 - Wayne Bresky - Created
  ! ------------------------------------------------------------------------------

    IMPLICIT NONE

    REAL(SINGLE), PARAMETER :: PI = 3.14159265359
    REAL(SINGLE), PARAMETER :: DTOR = PI / 180.
    REAL(SINGLE), PARAMETER :: RADDEG = DTOR**(-1)  
    REAL(SINGLE), INTENT(IN) :: U
    REAL(SINGLE), INTENT(IN) :: V
    REAL(SINGLE), INTENT(OUT) :: Spd
    REAL(SINGLE), INTENT(OUT) :: Dir
    REAL(SINGLE) :: Ang

    IF (U .EQ. 0. .AND. V .EQ. 0.) THEN
      Spd = 0.
      Dir = 0.
      RETURN
    ENDIF

    Spd = SQRT(U*U + V*V)

    IF (U .EQ. 0. .AND. V .LT. 0.)THEN
      Dir = DEGREE0
    ELSE IF (V .EQ. 0. .AND. U .LT. 0.) THEN
      Dir = DEGREE90
    ELSE IF (U .EQ. 0. .AND. V .GT. 0.) THEN
      Dir = DEGREE180
    ELSE IF (V .EQ. 0. .AND. U .GT. 0.) THEN
      Dir = DEGREE270
    ELSE IF (U .LT. 0. .AND. V .LT. 0.) THEN
      Ang = ASIN(ABS(U)/Spd)
      Dir = Ang*RADDEG
    ELSE IF (U .LT. 0. .AND. V .GT. 0.) THEN
      Ang = ASIN(ABS(V)/Spd)
      Dir = Ang*RADDEG + DEGREE90
    ELSE IF (U .GT. 0. .AND. V .GT. 0.) THEN
      Ang = ASIN(ABS(U)/Spd)
      Dir = Ang*RADDEG + DEGREE180
    ELSE IF (U .GT. 0. .AND. V .LT. 0.) THEN
      Ang = ASIN(ABS(V)/Spd)
      Dir = Ang*RADDEG + DEGREE270
    ENDIF

    RETURN

  END SUBROUTINE Common_UV2Spd

! ------------------------------------------------------------------------------
!
! Name:
!   Common_Get_Best_Buddy 
!
! Function:
!   Find nearest neighbor and compare with current AMV estimate.
!
! Description:
!   Searches for closest neighboring wind vector and computes the vector 
!   difference. This difference is subsequently used by the EUMETSAT QC
!   software to compute a local consistency QI score.
!
!   The first time this routine is called it creates an array of wind data
!   sorted by latitude.
!
! Reference:
!   Holmlund (1998), "The Utilization of Statistical Properties of Satellite-
!   Derived Atmospheric Motion Vectors to Derive Quality Indicators."
!   Weather and Forecasting, December 1998.
!
! Calling sequence:
!   CALL Common_Get_Best_Buddy 
!
! Inputs:
!   Loc_Wind             wind data
!   Vector_Index         current AMV estimate
!   Num_Of_Vectors       max number of winds 
!
! Outputs:
!   Delta_Vec            vector difference between AMV and nearest neighbor
!   Mean_Vec             mean vector of AMV and best buddy 
!
! Dependencies:
!   None
!
! Restrictions:
!   None
!
! History:
!   10/1997 - Ken Holmlund/EUMETSAT
!   02/2008 - Rewritten according to AIT standards - Wayne Bresky
!
!-------------------------------------------------------------------------------

SUBROUTINE Common_Get_Best_Buddy(Loc_Wind, Vector_Index, Delta_Vec, Mean_Vec, &
                                                       Num_Of_Vectors)
 
  REAL(SINGLE) Delta_Vec      ! vector difference between AMV and best buddy
  REAL(SINGLE) Mean_Vec       ! Mean vector of AMV and best buddy
  REAL(SINGLE) Lat_Buddy      ! latitude of buddy
  REAL(SINGLE) Lon_Buddy      ! longitude of buddy

  !  when changing the next two variables one must also change the call to sort
  !  and the sort subroutine

  ! wind data
  REAL(SINGLE) Loc_Wind(COMMON_MAX_PARAMETERS, MAX_RECORDS)

  ! data sorted by latitude
  REAL(SINGLE), SAVE :: Loc_Wind_Sorted(COMMON_MAX_PARAMETERS, MAX_RECORDS)

  REAL(SINGLE) Lat          ! AMV latitude
  REAL(SINGLE) Lon          ! AMV longitude
  REAL(SINGLE) Lat_Diff     ! difference in latitude between AMV and buddy
  REAL(SINGLE) Lon_Diff     ! Difference in lonigtude between AMV and buddy
  REAL(SINGLE) U_Buddy      ! u and v components of buddy
  REAL(SINGLE) V_Buddy
  REAL(SINGLE) Dir          ! direction of AMV
  REAL(SINGLE) Spd          ! AMV speed
  REAL(SINGLE) Loc_Vec_Len  ! Mean vector of AMV and buddy
  REAL(SINGLE) Loc_Vec_Diff ! Vector difference between AMV and buddy
  REAL(SINGLE) Min_Loc      ! Current best match-value
  REAL(SINGLE) Press        ! AMV height
  REAL(SINGLE) Press_Buddy  ! Buddy height
  REAL(SINGLE) U            ! AMV u and v components
  REAL(SINGLE) V

  INTEGER(LONG) Lat_Minus
  INTEGER(LONG) Lat_Plus
  INTEGER(LONG) Lat_Loop1          ! lower bound of latitude search box
  INTEGER(LONG) Lat_Loop2          ! upper bound of latitude search box
  INTEGER(LONG) Num_Of_Buddies     ! number of buddies found
  INTEGER(LONG), SAVE :: Lat_Pointer(-90:90)! Pointer to fast access of sorted array

  INTEGER(LONG) Vector_Index        ! vector number
  INTEGER(LONG) Winds_Counter       ! loop variables
  INTEGER(LONG) Lat_Counter
  INTEGER(LONG) Num_Of_Vectors      ! max number of winds 

  INTEGER(BYTE), SAVE :: First_Time = 1
  
  ! Only call sorting routine one time

  IF (First_Time .EQ. 1) THEN

    PRINT "(' common_get_best_buddy August 1998 ')"

    ! sort buddy array for more efficient reading

    PRINT "(' Calling sorting routine ')"

    CALL Common_Sort(Loc_Wind,COMMON_MAX_PARAMETERS,Num_Of_Vectors,Loc_Wind_Sorted)

    PRINT "(' Sorting finished ')"

    DO Winds_Counter=Num_Of_Vectors,1,-1

      Lat_Pointer(NINT(Loc_Wind_Sorted(1,Winds_Counter))) = Winds_Counter

    ENDDO

    First_Time = 0

  ENDIF

  Lat = Loc_Wind(1,Vector_Index)
  Lon = Loc_Wind(2,Vector_Index)
  Press = Loc_Wind(3,Vector_Index)
  U = Loc_Wind(4,Vector_Index)
  V = Loc_Wind(5,Vector_Index)
  CALL Common_UV2Spd(Spd, Dir, U, V)

  Mean_Vec=0.
  Min_Loc=999.
  Num_Of_Buddies=0
  Delta_Vec=999.

  ! Loop over all colums to find the right collocations for
  ! the local consistency check
  Lat_Minus = NINT(Lat) - 2
  Lat_Plus = NINT(Lat) + 2
  
  IF (Lat_Minus .LT. -NINT(DEGREE90)) Lat_Minus = -NINT(DEGREE90)
  IF (Lat_Plus .GT. NINT(DEGREE90)) Lat_Plus = NINT(DEGREE90)

  IF (Lat_Pointer(Lat_Minus) .LE. 0) THEN
    Lat_Loop1 = Lat_Pointer(NINT(Lat))
  ELSE 
    Lat_loop1 = Lat_Pointer(NINT(Lat)-2)
  ENDIF 

  IF (Lat_pointer(Lat_Plus) .LE. 0) THEN
    Lat_Loop2 = Lat_Pointer(NINT(Lat))
  ELSE 
    Lat_loop2 = Lat_Pointer(Lat_Plus)
  ENDIF

  DO Lat_Counter = Lat_Loop1, Lat_Loop2

    IF (Loc_Wind_Sorted(3,Lat_Counter) .GT. 0. ) THEN

      Lat_Diff = ABS(Loc_Wind_Sorted(1,Lat_Counter)-Lat)
      Lon_Diff = ABS(Loc_Wind_Sorted(2,Lat_Counter)-Lon)
      Press_Buddy = Loc_Wind_Sorted(3,Lat_Counter)

      ! Check that the buddy is within defined lat-lon box and pressure range
 
      IF (Lat_Diff .GT. DEGREE180) Lat_Diff = DEGREE360 - Lat_Diff
      IF (Lon_Diff .GT. DEGREE180) Lon_Diff = DEGREE360 - Lon_Diff

      IF ((Lat_Diff .LE. LOC_BOX_DEF) .AND. (Lat_Diff .GT. 0.) .AND. &
           (Lon_Diff .LE. LOC_BOX_DEF) .AND. (Lon_Diff .GT. 0.).AND. &
                    (ABS(Press_Buddy-Press) .LE. LOC_PRES_DIF)) THEN
 
        ! Get and convert buddy data

        Lat_Buddy=Loc_Wind_Sorted(1,Lat_Counter)
        Lon_Buddy=Loc_Wind_Sorted(2,Lat_Counter)
        Press_Buddy=Loc_Wind_Sorted(3,Lat_Counter)
        U_Buddy = Loc_Wind_Sorted(4,Lat_Counter)
        V_Buddy = Loc_Wind_Sorted(5,Lat_Counter) 
        Num_Of_Buddies=Num_Of_Buddies+1
        Loc_Vec_Len=SQRT((U_Buddy+U)**2+(V_Buddy+V)**2)/2.
        Loc_Vec_Diff=SQRT((U_Buddy-U)**2+(V_Buddy-V)**2)
              
        ! Check if new buddy is better than previous, if so replace

        IF (Loc_Vec_Diff/(l1*Loc_Vec_Len+l2) .LT. Min_Loc) THEN

          Min_Loc=Loc_Vec_Diff/(l1*Loc_Vec_Len+l2)
          Delta_Vec=Loc_Vec_Diff
          Mean_Vec=Loc_Vec_Len

        ENDIF

      ENDIF

    ENDIF

  ENDDO

  RETURN

END SUBROUTINE Common_Get_Best_Buddy

! ------------------------------------------------------------------------------
!
! Name:
!   Common_MSG_Auto_QC 
!
! Function:
!   Compute the EUMETSAT Quality Indicator (QI) for an individual wind vector.
!
! Description:
!   This routine performs the real AQC by normalising the test results amd 
!   combining them into a final quality mark.
!
! Reference:
!   Holmlund (1998), "The Utilization of Statistical Properties of Satellite-
!   Derived Atmospheric Motion Vectors to Derive Quality Indicators."
!   Weather and Forecasting, December 1998.
!
! Calling sequence:
!   Call MSG_Auto_QC 
!
! Input/Output:
!   QI_Variables           data structure of necessary variables
!
! Dependencies:
!   None
!
! Restrictions:
!   None
!
! History:
!   01/1997 - Ken Holmlund/EUMETSAT
!   02/2008 - Rewritten according to AIT standards - Wayne Bresky
!
! ------------------------------------------------------------------------------

SUBROUTINE Common_MSG_Auto_QC(Common_QI_Variables)

  TYPE(Common_Quality_Indicator_Variables), INTENT(INOUT) :: Common_QI_Variables

  REAL(SINGLE) Sum_Wgt  ! sum of all weights

  ! Sum the weights

  Sum_Wgt = W_SPD + W_DIR + W_VEC + W_UC + W_VC + W_LC + W_FC

  ! Compute the quality flags (i.e., the consistency test scores)

  Common_QI_Variables%Spd_Flag = 1 - (TANH(ABS(Common_QI_Variables%Vel_Diff) &
                          / (S1 * Common_QI_Variables%Vel + S2)))**S3

  Common_QI_Variables%Dir_Flag = 1 - (TANH(ABS(Common_QI_Variables%Dir_Diff) / &
                  (D1 * EXP(-Common_QI_Variables%Vel / D2) + D22)))**D3

  Common_QI_Variables%Vec_Flag = 1 - (TANH(Common_QI_Variables%Vec_Diff / &
               (VEC1 * Common_QI_Variables%Vec_Len + VEC2)))**VEC3

  Common_QI_Variables%Loc_Consistency_Flag = 1 - (TANH(Common_QI_Variables%Delta_Vec / &
                                 (L1 * Common_QI_Variables%Mean_Vec + L2)))**L3

  Common_QI_Variables%Fcst_Flag = 1 - (TANH(Common_QI_Variables%Fcst_Vec_Diff / &
                        (F1 * Common_QI_Variables%Fcst_Speed + F2)))**F3

  Common_QI_Variables%U_Flag = 1 - (TANH(ABS(Common_QI_Variables%U_Reverse - &
       Common_QI_Variables%U_Forward) / (ABS(Common_QI_Variables%U_Reverse + &
                      Common_QI_Variables%U_Forward) * U1 + U2)))**U3

  Common_QI_Variables%V_Flag = 1 - (TANH(ABS(Common_QI_Variables%V_Reverse - &
       Common_QI_Variables%V_Forward) / (ABS(Common_QI_Variables%V_Reverse + &
                      Common_QI_Variables%V_Forward) * V1 + V2)))**V3

  ! Check minimum speed, set quality to zero for low wind speeds

  IF (Common_QI_Variables%Vel .lt. MIN_SPD) THEN

    Common_QI_Variables%Auto_QC_Flag=0.0

  ELSE

    Common_QI_Variables%Auto_QC_Flag = (W_SPD * Common_QI_Variables%Spd_Flag + W_DIR * &
          Common_QI_Variables%Dir_Flag + W_VEC * Common_QI_Variables%Vec_Flag + W_UC * &
               Common_QI_Variables%U_Flag + W_VC * Common_QI_Variables%V_Flag + W_LC * &
                              Common_QI_Variables%Loc_Consistency_Flag + W_FC * &
                                       Common_QI_Variables%Fcst_Flag) / Sum_Wgt

  ENDIF

END SUBROUTINE Common_MSG_Auto_QC

! ------------------------------------------------------------------------------
!
! Name:
!   Common_Sort 
!
! Function:
!   Simple sorting routine. Sorting is done on the first element in the r_array.
!
! Description:
!   Common_Sort input array according to the first element of the array.
!
! Reference:
!
! Calling sequence:
!   CALL Common_Sort 
!
! Inputs:
!   r_array              input data array
!   ele                  number of elements in array
!   Num_Of_Input_Winds   number of winds
!
! Outputs:
!   out_arr              sorted output array
!
! Dependencies:
!   None
!
! Restrictions:
!   None
!
! History:
!   08/1998 - Ken Holmlund/EUMETSAT
!   02/2008 - Rewritten according to AIT standards - Wayne Bresky
!
!-------------------------------------------------------------------------------

SUBROUTINE Common_Sort(r_array, ele, Num_Of_Input_Winds, out_arr)
 
  INTEGER i                      ! Loop varaible
  INTEGER jj                     ! Loop variable
  INTEGER j                      ! Loop variable
  INTEGER k                      ! Loop variable
  INTEGER l                      ! Loop variable
  INTEGER(LONG) ele              ! Number of variables 
  INTEGER Num_Of_Input_Winds     ! Number of elements in array

  INTEGER(LONG), PARAMETER :: ISTEP = 1000

  REAL r_array(COMMON_MAX_PARAMETERS, MAX_RECORDS)
  REAL out_arr(COMMON_MAX_PARAMETERS, MAX_RECORDS)

  ! First step the first istep winds


  DO l=1,ele

    out_arr(l,1)=r_array(l,1)

  ENDDO

  LOOP1: DO i=2,MIN(ISTEP+1,Num_Of_Input_Winds)

    DO j=1,i-1

      IF (r_array(1,i) .LT. out_arr(1,j)) THEN

        DO k=i,j+1,-1

          DO l=1,ele

            out_arr(l,k)=out_arr(l,k-1)

          ENDDO

        ENDDO

        DO l=1,ele

          out_arr(l,j)=r_array(l,i)

        ENDDO

        CYCLE LOOP1

      ENDIF

    ENDDO

    DO l=1,ele

      out_arr(l,i)=r_array(l,i)

    ENDDO

  ENDDO LOOP1

  IF (Num_Of_Input_Winds .LE. ISTEP) RETURN

  ! Now go with giant istep steps through already sorted data
  ! and do microstepping only at relevant poins

  LOOP2: DO i=ISTEP+2,Num_Of_Input_Winds

    DO jj=1,i-1,ISTEP

      IF (r_array(1,i) .LT. out_arr(1,jj) ) THEN

        DO j=MAX(1,jj-istep),i-1

          IF (r_array(1,i) .LT. out_arr(1,j)) THEN

            DO k=i,j+1,-1

              DO l=1,ele

                out_arr(l,k)=out_arr(l,k-1)

              ENDDO

            ENDDO

            DO l=1,ele

              out_arr(l,j)=r_array(l,i)

            ENDDO

            CYCLE LOOP2

          ENDIF

        ENDDO

      ENDIF

    ENDDO

    ! Finally step over the last little bit being stepped over by giant step

    DO j=jj-ISTEP,i-1

      IF (r_array(1,i) .LT. out_arr(1,j)) THEN

        DO k=i,j+1,-1

          DO l=1,ele

            out_arr(l,k)=out_arr(l,k-1)

          ENDDO

        ENDDO

        DO l=1,ele

          out_arr(l,j)=r_array(l,i)

        ENDDO

        CYCLE LOOP2

      ENDIF

    ENDDO

    DO l=1,ele

      out_arr(l,i)=r_array(l,i)

    ENDDO

  ENDDO LOOP2

  RETURN

END SUBROUTINE Common_Sort 

END MODULE Common_QI_Module
