      SUBROUTINE DATA2
	IMPLICIT INTEGER (A-Z)
C***********************************************************************
C  SUBROUTINE DATA2 READS ZONAL, ZONE-TYPE AND SYSTEM-WIDE PARAMETERS.
C  THE MAIN INPUTS ARE FROM THE M01 AND M023 CARDS.
C
C  IN THIS VERSION THE CBD PARKING PARAMETERS FOR THE GORDON SCHULTZ
C  SUBMODEL ARE READ IN A SEPARATE FILE (CBDPARK.TXT).
C
C  THE OPTIONAL DISTR PARAMETERS ARE ALSO READ IN A SEPARATE FILE
C  (DISTR.TXT). 
C
C  VARIABLES INPUT IN DATA2 ARE AS FOLLOWS - APPLICABLE DEFAULTS IN 
C  PARENTHESES
C            I = THE CURRENT ZONE NUMBER
C        ZTYPE = ZONE TYPE #1: 1=CHICAGO CBD, 2=CHICAGO NON-CBD
C                              3=SUBURBAN CBD, 4=SPARSE SUBURBAN
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
C  FOLLOWING AUTO OWNERSHIP VARIABLES ARE NEEDED ONLY IF HW=.TRUE. AND 
C  LOW_INC=.FALSE. AND HI_INC=.FALSE.     
C       ICOWN0 = PROPORTION OF HOUSEHOLDS WITH 0 AUTOS (100'S)
C       ICOWN1 = PROPORTION OF HOUSEHOLDS WITH 1 AUTO (100'S)
C       ICOWN2 = PROPORTION OF HOUSEHOLDS WITH 2 OR MORE AUTOS (100'S)
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
C       PRCOST = PARK & RIDE COST IN CENTS/12 HOURS
C          INC = MEDIAN ZONAL INCOME
C       CTABUS = COST OF CTA (CITY) BUS
C        CTART = COST OF CTA (CITY) RAPID TRANSIT
C         XFER = COST OF CTA (CITY) TRANSFER
C       CTACBD = COST OF CTA BUS IN THE CBD FOR SUBURBAN RR COMMUTERS
C                (NOW THE LINKUP FARE PER TRIP $0.90 IN 1990)
C******
C       TRANC2 = SUBURBAN FEEDER BUS FARE TO RR STATION PARAMETERS
C                SECOND PARAMETER IS UNUSED
C                THIRD PARAMETER IS NOW TRANSFER FARE
C                (THE ADDITIONAL COST OF LINKUP FARE OVER
C                 FEEDER BUS FARE $0.90 - $0.75 = $0.15) IN 1990
C******
C       TRANC3 = SUBURBAN BUS FARE CALCULATION PARAMETERS
C                FIRST PARAMETER IS BASE FARE
C                SECOND PARAMETER IS UNUSED
C                THIRD PARAMETER IS TRANSFER
C          AOC = AUTO OPERATING COSTS
C       CBDPRK = CBD PARKING COST STRUCTURE
C       CSTBYM = AVERAGE OPERATING COST PER MILE FOR AUTO
C***********************************************************************
	INCLUDE 'Common_params.fi'
	INCLUDE 'Common_data.fi'
	INCLUDE 'Common_approach_model.fi'
	INCLUDE 'Common_cbdparking.fi'

	real*4 carimg(5,7)
      real*4 PCT_AUTO

	REAL*4 LOAD(4,4,3)

	REAL*4 LOWW_AUTO(4000,3), HIGHW_AUTO(4000,3) 

	CHARACTER*8 DATE8
      CHARACTER*10  CTIME10
      CHARACTER*2 DATE2(4), CTIME2(5)
      EQUIVALENCE (DATE8,DATE2(1)), (CTIME10,CTIME2(1))
	CHARACTER*1 ASTERIX(100)/100*'*'/

C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
C
C     THIS SECTION READS THE HOUSEHOLD VEHICLE OWNERSHIP FILE FROM
C     TRIP GENERATION
C
      IF (HW .AND. (LOW_INC .OR. HI_INC)) THEN
	  OPEN (UNIT=71,FILE='MCHW_HH.TXT',STATUS='OLD',ERR=9937)
          
        CALL DATE_AND_TIME(DATE8,CTIME10)

        WRITE (31,'(/15A)') ' MCHW_HH.TXT OPENED:  ',
     A    DATE2(3),'/',DATE2(4),'/',DATE2(1), DATE2(2),'  ', 
     B    CTIME2(1),':', CTIME2(2),':', CTIME2(3)
C
        DO IZ = 1,ZONES
	        
          READ (71,'(I5,6F12.4)',ERR=9938) NZ, (LOWW_AUTO(IZ,J),J=1,3),
     A	  (HIGHW_AUTO(IZ,J),J=1,3)

	    IF (LOW_INC) THEN
	      ICOWN0(IZ) = LOWW_AUTO(IZ,1)*10000
	      ICOWN1(IZ) = LOWW_AUTO(IZ,2)*10000.
	      ICOWN2(IZ) = LOWW_AUTO(IZ,3)*10000.
	    ENDIF	

	    IF (HI_INC) THEN
	      ICOWN0(IZ) = HIGHW_AUTO(IZ,1)*10000.
	      ICOWN1(IZ) = HIGHW_AUTO(IZ,2)*10000.
	      ICOWN2(IZ) = HIGHW_AUTO(IZ,3)*10000.
	    ENDIF	
        ENDDO
	ENDIF
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
C*******************
C
C  THIS SECTION WILL READ THE REVISED M01 CARD (UNIT 34).  THE FORMAT IS 
C  AS FOLLOWS (SET BY EASH 8/10/2001):
C
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
C
C    FILE IS NOW COMMA DELIMINTED.  THREE VARIABLES FOR HH AUTO 
C    OWNERSHIP ARE ONLY REQUIRED WHEN HOVFWAIT  OPTION IS TURNED ON BUT 
C    INCOME OPTIONS ARE TURNED OFF.
C
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
C
C    I (1-4)
C    PROPORTION OF HOUSEHOLDS WITH 0 AUTOS (%*10) (8-13) 
C    PROPORTION OF HOUSEHOLDS WITH 1 AUTO (%*10) (14-19)
C    PROPORTION OF HOUSEHOLDS WITH 2 OR MORE AUTOS (%*10) (20-25)
C    ZONE TYPE (26)
C    12 HOUR PARK 'N RIDE PARKING COSTS (28-30)
C     
C    MEDIAN ZONE INCOME (33-36) (REVISED FOR PRAIRIE PARKWAY PROJECT BY 
C      RWE 8/15/2003)
C
C    PARK AND RIDE FACILITY AVAILABLE (38)
C    FIRST WORK TRIP WAIT FOR BUS IN APPROACH MODEL (56-57)
C    FIRST NONWORK TRIP WAIT FOR BUS IN APPROACH MODEL (58-59)
C    FIRST WORK TRIP WAIT FOR FEEDER BUS IN APPROACH MODEL (60-61)
C    FIRST NONWORK TRIP WAIT FOR BUS IN APPROACH MODEL (62-63)
C    DESTINATION ZONE AUTO OCCUPANCY AUTO (68-70)
C
C*******************
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
10104 CONTINUE
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
      IF (.NOT. HW) THEN
        READ(34,*,END=10103) I, ZTYPE(I), PRCOST(I), INC(I), TEMP1,
     A    F1,F2,F3,F4
      ENDIF 
C
      IF (HW) THEN
	  IF ((.NOT. HOV2) .AND. (.NOT. HOV3)) THEN
          READ(34,*,END=10103) I, ZTYPE(I), PRCOST(I), INC(I), TEMP1, 
     A      F1, F2, F3, F4, ZOCC(I)
        ENDIF
C
        IF (HOV2 .OR. HOV3) THEN
	    IF ((.NOT. LOW_INC) .AND. (.NOT. HI_INC)) THEN
            READ(34,*,END=10103) I, ICOWN0(I), ICOWN1(I), ICOWN2(I), 
     A        ZTYPE(I), PRCOST(I), INC(I), TEMP1, F1, F2, F3, F4, 
     B        ZOCC(I)
	
	    ELSE
            READ(34,*,END=10103) I, ZTYPE(I), PRCOST(I), INC(I), TEMP1, 
     A        F1, F2, F3, F4, ZOCC(I)

          ENDIF
        ENDIF
      ENDIF
C
C     PROGRAM ASSUMES THAT MO1 FILE WILL BE IN SORT BY ZONES
C
      IF (I .GT. ZONES) THEN
	  WRITE(31,'(A)') ' ERROR:  M01 FILE DOES NOT MATCH ZONES'
	  STOP
	ENDIF 
C
C     IF THE PURPOSE IS HOME BASED OTHER USE A REGIONAL OCCUPANCY
C     OF 1.45 (CHECK THIS WITH THE SURVEY DATA).  IF THE PURPOSE
C     IS NON-HOME BASED USE A REGIONAL OCCUPANCY OF 1.15.
C     (CHECK THIS WITH SURVEY DATA)
C
C     SURVEY VALUES ARE 1.26 (HO) AND 1.17 (NH) RWE 12/9/93
C
C     CHANGES MADE ON 12/8/93 BY GWS/RWE (NEXT TWO LINES)
C
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
C
C     THE FOLLOWING TWO AUTO OCCUPANCIES WERE UPDATED BASED ON THE
C     CMAP HOUSEHOLD TRAVEL SURVEY RWE 8/26/2009
C     
C     HOME TO OTHER IS EQUAL TO 1.66.  NOTE THAT THIS VALUE NOW INCLUDES 
C     HOME TO SCHOOL TRIPS BY CHILDREN.
C     NONHOME IS EQUAL TO 1.19.
C
C
C     THE VEHICLE OCCUPANCY FOR HOME-OTHER INCLUDES DRIVER TRIPS FOR 
C     TRANSPORTING CHILDREN TO SCHOOL, WHICH WERE NOT INCLUDED IN THE
C     EARLIER SURVEYS
C

      IF (TPTYPE .EQ. 2) ZOCC(I) = 1.66
      IF (TPTYPE .EQ. 3) ZOCC(I) = 1.19
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
	IF ((ZTYPE(I) .EQ. 1) .AND. (.NOT. ZCBD(I))) THEN
	  WRITE (31,'(/A,I5)') ' ERROR:  TYPE 1 ZONE ON M01 FILE NOT IDENT
     AIFIED AS CBD ZONE',I
	  STOP
	ENDIF

	IF ((ZTYPE(I) .NE. 1) .AND. (ZCBD(I))) THEN
	  WRITE (31,'(/A,I5)') ' ERROR:  CBD ZONE NOT IDENTIFIED AS TYPE 1
     A ZONE ON M01 FILE',I
	  STOP
	ENDIF
C
C	FOLLOWING FORMAT REVISED FOR FOUR DIGIT INCOME BY RWE 8/15/2003
C
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
C10102 FORMAT(I4,3X,3I6,I1,1X,I3,2X,I4,1X,I1,17X,4I2,4X,F3.2)
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
      IF(TEMP1.NE.0) PNRAVL(I)=.TRUE.

      IF(TPTYPE.EQ.1) then
         fwbus(I)=F1
         fwfdr(I)=F3
      endif
      IF(TPTYPE.NE.1) then
         fwbus(I)=F2
         fwfdr(I)=F4
      endif
C
C    ACCUMULATE AUTO OWNERSHIP LEVELS
C
      IF (HOV2 .OR. HOV3) THEN
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
        IF ((.NOT. LOW_INC) .AND. (.NOT. HI_INC)) THEN

          IF (((ICOWN0(I)+ICOWN1(I)+ICOWN2(I)).GT. 1001) .OR. 
     A	  ((ICOWN0(I)+ICOWN1(I)+ICOWN2(I)).LT. 999)) THEN
	        WRITE (31,'(/A,3I8)') ' ERROR:  HH AUTO OWNERSHIP LEVELS D 
     AO NOT SUM TO APPROX 100 PERCENT: ',ICOWN0(I),ICOWN1(I),ICOWN2(I)
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
	      STOP
	    ENDIF
        ENDIF

        COWN0(I) = ICOWN0(I)
        COWN1(I) = COWN0(I) + ICOWN1(I)
        COWN2(I) = COWN1(I) + ICOWN2(I)
        IF (COWN2(I) .GT. 0.0) THEN
          COWN0(I) = COWN0(I)/COWN2(I)
          COWN1(I) = COWN1(I)/COWN2(I)
        ENDIF
	  COWN2(I) = 1.00
      ENDIF

      GO TO 10104
C
C	END OF LOOP FOR READING M01 FILE
C
10103 REWIND 34
C
C     CHECK THAT M01 FILE IS COMPLETE
C
	DO I=1,ZONES
	  IF (ZOI(I) .AND. (ZTYPE(I) .LT. 1)) 
     A    WRITE (31,'(A,I5)') ' ERROR:  MISSING M01 RECORD FOR ZONE',I
     	ENDDO
C
C     WRITE REPORT FOR M01 FILE
C
      CALL REPORT1
C***********************************************************************
C
C     THE FOLLOWING SECTION WILL READ AND PROCESS THE M023 CARDS
C     (UNIT 35)
C
C***********************************************************************

C     THE CITY TRANSIT FARE STRUCTURE
C
C     CTA BUS FARE (1-5)
C     CTA RAIL TRANSIT FARE (6-10)
C     CTA TRANSFER COST (11-15)
C     CTA CBD BUS FARE (LINKUP FARE) (16-20)  

      READ(35,'(4I5)',END=9935) CTABUS,CTART,XFER,CTACBD

C     THE SUBURBAN PACE FEEDER BUS FARE CALCULATION PARAMETERS
C
C     TRANC2(1) = SUBURBAN FEEDER BUS FARE TO RR STATION 
C     TRANC2(3) = TRANSFER FARE
C                (THE ADDITIONAL COST OF LINKUP FARE OVER
C                 FEEDER BUS FARE $0.90 - $0.75 = $0.15) IN 1990

      READ(35,'(I5, 5X, I5)',END=9935) TRANC2(1), TRANC2(3)

C     THE SUBURBAN PACE BUS FARE CALCULATION PARAMETERS
C
C     TRANC3(1) = SUBURBAN BUS BASE FARE
C     TRANC3(3) = TRANSFER FARE

      READ(35,'(I5, 5X, I5)',END=9935) TRANC3(1), TRANC3(3)
C
C     NOTE THAT COMMUTER RAIL INFORMATION IS NOW SKIMMED FROM NETWORK
C

C     THE AUTO OPERATING COST PARAMETERS (TWO INPUT LINES)
C
C     AOC(1) = COST IN CENTS*100 0 TO 5 MPH
C     AOC(2) = COST IN CENTS*100 5 TO 10 MPH
C
C     AOC(16) = COST IN CENTS*100 75 TO 80 MPH 

      READ(35,'(8I5)',END=9935) AOC
C
C     IF INCOST HAS BEEN SET TO TRUE, THIS SECTION WILL READ THE
C     AVERAGE AUTO COST BY ZONE AREA TYPE FROM THE M023 CARD
C
      IF(INCOST) READ(35,'(4F5.2)',END=9935) (CSTBYM(J),J=1,4)
C
C     DONE READING MO23 COST PARAMETERS
C
      REWIND 35
C*******************
C  THE FOLLOWING SECTION WILL PRINT OUT THE MO23 VALUES READ
C*******************

	WRITE (31,'(/A)') ' REGION WIDE PARAMETERS FROM M023 CARDS'
	WRITE (31,'(100A1)') ASTERIX

	WRITE (31,'(/A)') ' CTA FARES IN CENTS' 
	WRITE (31,'(A,I4)') '   BUS=',CTABUS
	WRITE (31,'(A,I4)') '   RAPID TRANSIT=',CTART
	WRITE (31,'(A,I4)') '   TRANSFER=',XFER
	WRITE (31,'(A,I4)') '   CBD BUS FARE FOR SUBURBAN RAIL COMMUTERS='
     A ,CTACBD
C
	WRITE (31,'(/A)')' PACE BUS AND FEEDER BUS TO RAIL FARES IN CENTS'
	WRITE (31,'(A,I4)') '   FEEDER BUS TO SUBURBAN RAIL=',TRANC2(1)
	WRITE (31,'(A,I4)') '   SUBURBAN RAIL TO DOWNTOWN BUS (LINKUP)=',
     A TRANC2(3)
	WRITE (31,'(A,I4)') '   FULL PACE FARE=',TRANC3(1)
	WRITE (31,'(A,I4)') '   PACE TRANSFER=',TRANC3(3)

      WRITE (31,'(/A)') ' AUTO OPERATING COSTS FOR SPECIFIED SPEED RANGE
     AS IN 1/100 OF CENTS'

      DO I=1,16
	  LO = (I-1)*5 + 1
	  HI = LO + 4
        WRITE(31,'(I5,A,I3,A,I5)') LO,' MPH TO',HI,' MPH= ',AOC(I) 
	ENDDO
	
	IF (INCOST) THEN
	  WRITE (31,'(/A)') ' OPTIONAL AUTO OPERATING COSTS BY ZONE TYPE F
     AOR APPROACH SUBMODEL'
	  WRITE (31,'(A,F8.3)') '   AREA TYPE 1 (CBD)= ',CSTBYM(1)
	  WRITE (31,'(A,F8.3)') '   AREA TYPE 2 (CHICAGO)= ',CSTBYM(2)
	  WRITE (31,'(A,F8.3)') '   AREA TYPE 3 (SUBURBAN)= ',CSTBYM(3)
	  WRITE (31,'(A,F8.3)') '   AREA TYPE 4 (RURAL)= ',CSTBYM(4)

	ENDIF
C***********************************************************************
C
C  THE CBD PARKING COST STRUCTURE NOW READ FROM CBDPARK.TXT (UNIT 36)                                      04900008
C
C    ZONE NUMBER (1-5)
C    CUMULATIVE PARKING PROBABILITY (6-10)
C    PARKING COST IN CENTS PER HOUR (11-15)
C    ASSOCIATED SAVINGS (16-20)
C    TIME TO WALK BLOCK IN SECONTS (21-25)
C
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
      IF (HW .AND. (NUMCBD_PARK .GT. 0)) THEN
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
	  WRITE (31,'(/A)') ' CBD PARKING SUBMODEL PARAMETER RECORDS'
	  WRITE (31,'(100A1)') ASTERIX
	  WRITE (31,'(/A)') ' 1.  CBD ZONE PARKING ATTRIBUTES'
	  WRITE (31,'(A)') '                    PARK COST    SAVINGS  WALK
     A SPEED'
	  WRITE (31,'(A)') '   ZONE   CUM PROB (CENTS/HR)    (CENTS)   (SE
     AC/BLK)'
	 
        DO I=1,NUMCBD_PARK

C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
          READ(36,*,END=9936) PKZN(I),(CBDPRK(I,1,K),K=1,4)
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
          WRITE (31,'(/I6,4I11)') PKZN(I),(CBDPRK(I,1,K),K=1,4)
	    JJ=PKZN(I)
	    IF (.NOT. ZCBD_PARK(JJ)) THEN
	      WRITE (31,'(A)') ' ERROR:  PARKING COST INPUT FILE', PKZN(I)
		  STOP
	    ENDIF	 

          DO J=2,5
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
            READ(36,*,END=9936) PKZN(I), (CBDPRK(I,J,K),K=1,4)
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
            WRITE (31,'(I6,4I11)') PKZN(I),(CBDPRK(I,J,K),K=1,4)
	    ENDDO
	  ENDDO	
c
c  #################################################################
c  #################################################################
c  ###      This is the new addition to the M023 card            ###
c  ###  It consists of five records which must come immediately  ###
c  ###  after the zone parking cost cards and before the average ###
c  ###  cost cards.  The information on these cards and the      ###
c  ###  format of these cards is as follows:                     ###
c  ###  Field  Columns Format      Description                   ###
c  ###      1    1-10    f10.0  Upper value of income range      ###
c  ###      2   11-15     F5.1  Percent of free parking          ###
c  ###      3   16-20     F5.1  Percent transit                  ###
c  ###      4   21-25     F5.1  Percent one person per car       ###
c  ###      5   26-30     f5.1  Percent two persons per car      ###
c  ###      6   31-35     F5.1  Percent Three persons per car    ###
c  ###      7   36-40     F5.1  Percnt Four + persons per car    ###
c  ###                                                           ###
c  ###  The five cards provide a range of five income groups     ###
c  ###  beginning with zero dollars.  For example if the actual  ###
c  ###  range of income is:                                      ###
c  ###       Group 1: from 0 to $10,000 per year                 ###
c  ###       Group 2: from $10,001 to $20,000                    ###
c  ###       Group 3: from $20,001 to $35,000                    ###
c  ###       Group 4: from $35,001 to $50,000                    ###
c  ###       Group 5: greater than $50,000                       ###
c  ###  The five input cards would be coded as:                  ###
c  ###                                                           ###
c  ###  10000                                                    ###
c  ###  20000                                                    ###
c  ###  35000                                                    ###
c  ###  50000                                                    ###
c  ###  50001                                                    ###
c  ###  The percent free refers to the percent of people         ###
c  ###  who are parking and have an income in the specified      ###
c  ###  range.  The 1990 data shows that the percent free        ###
c  ###  increases as income increases.  The percent transit is   ###
c  ###  a reasonable estimate of the percent transit for the     ###
c  ###  group.  The initial information was a single value but   ###
c  ###  additional analysis of the home interview or other data  ###
c  ###  might allow the analyst to have a different percent      ###
c  ###  transit by income group.  The percent by car occupancy   ###
c  ###  refers to the percent of people in each integer car      ###
c  ###  occupancy who park. Again the initial data only allowed  ###
c  ###  a single value for this set of paratmeters, but it may   ###
c  ###  be possible to expand this information when more data    ###
c  ###  becomes available.                                       ###
c  ###                                                           ###
c  ###  The intial 1990 data for the five cards is as follows:   ###
c  ###                                                           ###
c  ###         20000 21.1 65.0 75.5 20.4  2.8  1.3               ###
c  ###         30000 23.2 65.0 75.5 20.4  2.8  1.3               ###
c  ###         40000 27.6 65.0 75.5 20.4  2.8  1.3               ###
c  ###         50000 31.0 65.0 75.5 20.4  2.8  1.3               ###
c  ###         50001 36.7 65.0 75.5 20.4  2.8  1.3               ###
c  ###                                                           ###
c  ###  What are we going to do with this data?                  ###
c  ###  First we shall find the percent of all trips which have  ###
c  ###  the potential for free parking.  This percent is the     ###
c  ###  percent free times percent highway (1.0 less % transit)  ###
c  ###  for example if 21.1 percent of the parkers park for free ###
c  ###  and the percent transit is 65% than 7.4 percent of all   ###
c  ###  trips have a potential to park for free (21.1 * 0.35) .  ###
c  ###  We will use this percent to pick between free and paid   ###
c  ###  parking - using the same random monte carlo technique    ###
c  ###  used in picking income and parking cost.  If we have a   ###
c  ###  paid parking, we will then use the integer car occupancy ###
c  ###  percents to "pick" an occupancy to divide the parking    ###
c  ###  cost by.  For example if our random pick is for four     ###
c  ###  persons per car we shall then divide the parking cost    ###
c  ###  by four.                                                 ###
c  ###                                                           ###
c  ###  Now lets read the data cards (all five)                  ###
c  ###  for the time being I am going to put all                 ###
c  ###  seven data items in a single array called prkadj         ###
c  ###  We may change this later.                                ###
c  #################################################################
c
        do 60604  I=1,5
          read(36,'(F10.0,6F5.1)') (carimg(i,k),k=1,7)

          do k = 1,7
            prkadj(i,k) = carimg(i,k)
	    ENDDO
c
c   calculate the percent free for all trips and store in
c   area prkadj(i,2) - this simply saves doing this each
c   time we calculate an interchange.  Also make the data
c   probabilities rather than percents
c
          PCT_AUTO = 1.0 - (0.01 * prkadj(i,3))
          prkadj(i,2) = prkadj(i,2) * PCT_AUTO
c
c   now lets put the percent by integer car occupancy in
c   a cummulative percent calculation to make it easier to
c   pick a category
c
          prkadj(i,5) = prkadj(i,4) + prkadj(i,5)
          prkadj(i,6) = prkadj(i,5) + prkadj(i,6)
          prkadj(i,7) = prkadj(i,6) + prkadj(i,7)

          ichk1 = (prkadj(i,7) * 100) + 0.5

          if (ichk1 .NE. 10000) THEN
            WRITE(31,'(A,F10.0/6F5.1)') ' ERROR:  PERCENT AUTO OCCUPANCY
     A DID NOT ADD TO 100', (carimg(i,k),k=1,7)
	      STOP
	    ENDIF
c
c     now so we don't goof up later set prkadj(i,3) to 0.0
c
          prkadj(i,3) = 0.0

60604   continue

	  WRITE (31,'(/A)') ' 2.  CBD PARKING SUBMODEL USER CHARACTERISTIC
     AS'
	  WRITE (31,'(A)')  '   TOP INCOME  % fREE PARK   % 1 PERSON   % 2
     A PERSON   % 3 PERSON  % 4+ PERSON'

        DO I=1,5
	    WRITE (31,'(2X, F10.0, 5F13.1)') prkadj(I,1), prkadj(I,2), 
     A     (prkadj(I,jj),jj=4,7)
        ENDDO 
c
c  #################################################################
c  ###                                                           ###
c  ###   End of reading the new parking cost data                ###
c  ###                                                           ###
c  #################################################################
C
	  REWIND 36
      ENDIF
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
C*******************
C  IF NOT DEFAULTED, THIS SECTION READS THE APPROACH SUBMODEL
C                    DISTRIBUTION PARAMETERS (DISTR)
C  IN ADDITION TO THE DEFAULT VALUES, THE USER HAS THE OPTION
C              OF INPUTTING VALUES BY ZONE TYPE
C              OR BY INDIVIDUAL ZONES
C
C  IF ASM_AREA AND ASM_ZONE ARE FALSE, DEFAULT VALUES WILL BE USED
C
C  IF ASM_AREA = TRUE, AREA TYPE ZONE VALUES ARE READ (UNIT 37)
C
C  IF ASM_ZONE = TRUE, ZONE VALUES ARE READ (UNIT 37)
C*******************
C*******************  RWE CHANGE FOR I290 OCTOBER 2012  ***************
C
C  ADDITIONAL SET OF DISTANCE DISTRIBUTION PARAMETERS ADDED FOR 
C  DISTANCE TO PARK AND RIDE STATION
C
C*******************
20203 IF((.NOT. ASM_AREA) .AND. (.NOT. ASM_ZONE)) GO TO 20201

C  WE'VE GOT TO READ PARAMETERS

      DO I=1,ZONES
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
        READ(37,*,END=20201) Z, (DISTR(Z,1,KK),KK=1,3),
     A    (DISTR(Z,2,KK),KK=1,3), (DISTR(Z,3,KK),KK=1,3), 
     B    (DISTR(Z,4,KK),KK=1,3), (DISTR(Z,5,KK),KK=1,3)
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************

        IF(ASM_AREA .AND.I.EQ.4) GO TO 20201
	ENDDO
C*******************
C  THIS SECTION IS ACCESSED IF THE DISTRIBUTION PARAMETERS WERE
C              INPUT BY ZONE TYPE
C  IF SO, THE ZONE TYPE IS OBTAINED AND THE CORRESPONDING VALUE
C              LOADED INTO THE 'DISTR' ARRAY
C*******************
20201 IF(ASM_ZONE) GO TO 99999

      DO I=1,4
        DO J=1,5
	    DO K=1,3
            LOAD(I,J,K)=DISTR(I,J,K)
          ENDDO
        ENDDO
      ENDDO

      DO I=1,ZONES
        KL=ZTYPE(I)
        DO J=1,5
	    DO K=1,3
            IF(ZOI(I)) DISTR(I,J,K)=LOAD(KL,J,K)		
	    ENDDO
        ENDDO
      ENDDO

99999 CONTINUE
C
C     WRITE REPORT FOR DISTR INPUTS
C
      CALL REPORT2
C
      REWIND 37
      RETURN

 9935 WRITE (31,'(/A)') ' ERROR DETECTED WHILE READING M023 FILE'
	STOP

 9936 WRITE (31,'(/A)') ' ERROR DETECTED WHILE READING CBDPARK FILE'
	STOP
 
 9937 WRITE (31,'(/A)') ' ERROR WHILE OPENING MCHW_HH.TXT FILE'
	STOP

 9938 WRITE (31,'(/A)') ' ERROR DETECTED WHILE READING MCHW_HH.TXT FILE'
	STOP
C
      END