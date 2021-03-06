      SUBROUTINE DATA1
      IMPLICIT INTEGER (A-Z)
C***********************************************************************
C
C  THE DATA1 SUBROUTINE WILL READ NAMELISTS
C                            SET DEFAULT VALUES
C                            OBTAIN THE RANDOM NUMBER SEED
C
C ##  Heither 09-06-2016: Add ITER to program      
C
C     VARIABLES AND ARRAYS NAMELISTED (REVISED BY EASH, SEPTEMBER 2001)      
C
C
C           TITLE = TITLE DESCRIBING MODEL RUN (80 CHARACTERS MAX)
C
C &PARAM   ZONES  = HIGHEST ZONE NUMBER ON THE M01 INPUT FILE (3000 MAX)
C                   MUST BE LESS THAN OR EQUAL TO THE EMMEBANK ZONES
C          CBDZON = CBD ZONE NUMBERS (200 ENTRIES MAX)
C                   EXAMPLE:  CBDZON = 1,3,5,-45,....
C          RNSEED = RANDOM NUMBER SEED (ANY INTEGER VALUE BETWEEN ZERO
C                   AND 9999, ZERO IMPLIES A RANDOM SEED)
C          COEFF1 = COEFFICIENTS IN MODE CHOICE LOGIT EQUATION FOR
C                   NON-CBD TRIP
C          COEFF2 = COEFFICIENTS IN MODE CHOICE LOGIT EQUATION FOR
C                   CBD TRIP
C          APC    = AUTO PARKING COST BY ZONE AREA TYPE ON M01 FILE 
C                   ZONE TYPE 1 (CHICAGO CBD) DEFAULT IS 150 CENTS
C                   ZONE TYPE 2 (CHICAGO REMAINDER) DEFAULT IS 50 CENTS
C                   ZONE TYPE 3 (DENSE SUBURB) DEFAULT IS 30 CENTS
C                   ZONE TYPE 4 (SPARSE SUBURB) DEFAULT IS 0 CENTS
C          WFA    = WALK FROM AUTO TIME BY ZONE AREA TYPE ON MO1 FILE
C                   ZONE TYPE 1 (CHICAGO CBD) DEFAULT IS 5 MIN
C                   ZONE TYPE 2 (CHICAGO REMAINDER) DEFAULT IS 3 MIN
C                   ZONE TYPE 3 (DENSE SUBURB) DEFAULT IS 3 MIN
C                   ZONE TYPE 4 (SPARSE SUBURB) DEFAULT IS 3 MIN
C          PRKZON = ZONES IN CBD PARKING COST STRUCTURE 
C                   (200 ENTRIES MAX)
C                   EXAMPLE:  PRKZON = 1,3,5,-45,....
C            ITER = NUMBER OF TRIPS SIMULATED PER INTERCHANGE      

C &OPTION  HW     = HOME TO WORK TRIP, DEFAULT IS TRUE
C          HNW    = HOME TO NON WORK TRIP, DEFAULT IS FALSE
C          OTH    = NON WORK TO NON WORK TRIP, DEFAULT IS FALSE
C          HOV2   = INDICATES HOV (2+) VERSION TO BE RUN (FALSE)
C          HOV3   = INDICATES HOV (3+) VERSION TO BE RUN (FALSE)
C          TOLL   = INDICATES TOLL/NON-TOLL TRIP TABLES ARE
C                   PRODUCED (FALSE)
C          AOCC   = AUTO OCCUPANCY TABLE IS INPUT (FALSE)
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
C         LOW_INC = INDICATES THAT LOW INCOME WORKER TRIPS ARE TO BE 
C                   SPLIT BY MODE
C          HI_INC = INDICATES THAT HIGH INCOME WORKER TRIPS ARE TO BE
C                   SPLIT BY MODE 
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
C        ASM_AREA = TRUE, IF APPROACH SUBMODEL DISTRIBUTION PARAMETERS
C                   ARE READ IN BY ZONE AREA TYPE ON M01 FILE,
C                   DEFAULT IS FALSE
C        ASM_ZONE = TRUE. IF APPROACH SUBMODEL DISTRIBUTION PARAMETER 
C                   ARE READ IN BY ZONE, DEFAULT IS FALSE
C                   (NOTE:  PROGRAM DEFAULTS ARE USED IF ASM_AREA AND
C                    ASM_ZONE ARE BOTH FALSE.)
C          INCOST = TRUE, IF AVERAGE COST FOR AUTO IS INPUT BY ZONE
C                   AREA TYPE ON THE M02 CARD, DEFAULT IS FALSE
C           TRACE = TRUE, IF DETAILED INTERCHANGE FILE IS WRITTEN,
C                   DEFAULT IS FALSE

C &PROCESS   PZOI = LIST OF ZONES TO BE PROCESSED (200 ENTRIES MAX)
C                   EXAMPLE:  PZOI = 1,3,5,-45,....
C            QZOI = LIST OF ZONES TO BE PROCESSED (200 ENTRIES MAX)
C                   EXAMPLE:  QZOI = 1,3,5,-45,....

C &SYSTEM  SPDWLK = SYSTEM-WIDE SPEED OF WALKING, 
C                   DEFAULT IS 30 TENTHS OF A MILE PER HOUR
C          SPEEDS = SPEEDS OF APPROACH AUTO AND BUS BY ZONE AREA TYPE 
C                   AUTO APPROACH SPEEDS:
C                     ZONE TYPE 1 (CHICAGO CBD) DEFAULT IS 7 MPH
C                     ZONE TYPE 2 (CHICAGO REMAINDER) DEFAULT IS 15 MPH
C                     ZONE TYPE 3 (DENSE SUBURB) DEFAULT IS 20 MPH
C                     ZONE TYPE 4 (SPARSE SUBURB) DEFAULT IS 30 MPH
C                   BUS APPROACH SPEEDS:
C                     ZONE TYPE 1 (CHICAGO CBD) DEFAULT IS 5 MPH
C                     ZONE TYPE 2 (CHICAGO REMAINDER) DEFAULT IS 10 MPH
C                     ZONE TYPE 3 (DENSE SUBURB) DEFAULT IS 12 MPH
C                     ZONE TYPE 4 (SPARSE SUBURB) DEFAULT IS 17 MPH
C          DRVOT  = DRIVER'S VALUE OF TIME, DEFAULT IS 14 CENTS/MIN
C          AFC1   = AUTO FIXED COSTS FOR DRIVER, 
C                   DEFAULT IS 35 CENTS/TRIP
C          AFC2   = AUTO FIXED COSTS FOR AUTO PASSENGER,
C                   DEFAULT IS 20 CENTS PER TRIP
C          W2PNR  = WALK TIME TO STATION FROM PARK AND RIDE LOT, 
C                   DEFAULT IS 2 MINUTES
C          DISCNT = DISCOUNT FACTOR FOR COSTS USUALLY BETWEEN YEAR OF
C                   MODE CHOICE AND 1970, DEFAULT IS 1.00

C &TABNUM  TABLE_FMD = EMME INPUT TABLE WITH FIRST TRANSIT MODE,
C                      DEFAULT IS 1
C          TABLE_LMD = EMME INPUT TABLE WITH LAST TRANSIT MODE,
C                      DEFAULT IS 2
C          TABLE_IVT = EMME INPUT TABLE WITH TRANSIT IN-VEHICLE 
C                      TIME, DEFAULT IS 3
C          TABLE_OVT = EMME INPUT TABLE WITH TRANSIT OUT-OF-VEHICLE 
C                      TIME, DEFAULT IS 4
C          TABLE_HWAY = EMME INPUT TABLE WITH TRANSIT FIRST 
C                       HEADWAY, DEFAULT IS 5
C          TABLE_PMD = EMME INPUT TABLE WITH TRANSIT PRIORITY MODE,
C                      DEFAULT IS 6
C          TABLE_FARE = EMME INPUT TABLE WITH TRANSIT FARE,
C                       DEFAULT IS 7
C          TABLE_HTIME = EMME INPUT TABLE WITH HIGHWAY TIMES, 
C                        DEFAULT IS 8
C          TABLE_HDIST = EMME INPUT TABLE WITH HIGHWAY DISTANCES, 
C                        DEFAULT IS 9
C          TABLE_PTRIP = EMME INPUT TABLE WITH PERSON TRIPS,
C                        DEFAULT IS 21
C          TABLE_AOCC = EMME INPUT TABLE WITH AUTO OCCUPANCIES, 
C                       DEFAULT IS 22 (REQUIRED ONLY IF AOCC IS TRUE)
C          TABLE_TRANSIT = EMME OUTPUT TABLE OF TRANSIT TRIPS, 
C                          DEFAULT IS 23 
C          TABLE_AUTO = EMME OUTPUT TABLE OF AUTO TRIPS, DEFAULT IS 24 
C***********************************************************************
C############ Heither, 07-11-2018: updated to support 4000 zones
	INCLUDE 'Common_params.fi'
      INCLUDE 'Common_data.fi'
C
	NAMELIST/PARAM/ ZONES, CBDZON, RNSEED, COEFF1, COEFF2,
     A APC, WFA, PRKZON, ITER
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
	NAMELIST/OPTION/ HW, HNW, OTH, HOV2, HOV3, TOLL, AOCC, LOW_INC,
     A HI_INC, ASM_AREA, ASM_ZONE, INCOST, TRACE 
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
	NAMELIST/PROCESS/ PZOI, QZOI
	NAMELIST/SYSTEM/ SPDWLK, SPEEDS, DRVOT, AFC1, AFC2, W2PNR, DISCNT
	NAMELIST/TABNUM/ TABLE_FMD, TABLE_LMD, TABLE_IVT, TABLE_OVT,
     A  TABLE_HWAY, TABLE_PMD, TABLE_FARE, TABLE_HTIME, TABLE_HDIST,
     B  TABLE_PTRIP, TABLE_AOCC, TABLE_TRANSIT, TABLE_AUTO 

      LOGICAL*4 NLIST(5)/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./
	
	CHARACTER*8 CARD(5)/'  &PARAM',' &OPTION','&PROCESS',' &SYSTEM',
     A ' &TABNUM'/
 
	CHARACTER*1 ASTERIX(100)/100*'*'/

      CHARACTER*1 CTAB1
      CHARACTER*2 CTAB2
      CHARACTER*3 CTAB3
C
C     PROCESS NAMELIST CARDS
C
04100 READ(33,'(10A8)',END=04190) TITLE

      IF(TITLE(1) .EQ.' &PARAM ') NLIST(1)=.TRUE.
      IF(TITLE(1) .EQ.' &OPTION') NLIST(2)=.TRUE.
      IF(TITLE(1) .EQ.' &PROCES') NLIST(3)=.TRUE.
      IF(TITLE(1) .EQ.' &SYSTEM') NLIST(4)=.TRUE.
      IF(TITLE(1) .EQ.' &TABNUM') NLIST(5)=.TRUE.

      GO TO 04100
C
04190 REWIND 33
C
	READ(33,'(10A8)',END=07220) TITLE
C
      WRITE (*,'(/A,10A8)') ' PROJECT:  ', TITLE
	WRITE (*,'(100A1)') ASTERIX
 
      WRITE (31,'(/A,10A8)') ' PROJECT:  ', TITLE
	WRITE (31,'(100A1)') ASTERIX
	
      IF(NLIST(1)) READ(33,PARAM,END=07220)
      IF(NLIST(2)) READ(33,OPTION,END=07220)
      IF(NLIST(3)) READ(33,PROCESS,END=07220)
      IF(NLIST(4)) READ(33,SYSTEM,END=07220)
      IF(NLIST(5)) READ(33,TABNUM,END=07220)
C
07040 DO 7100 J=1,5
      IF(.NOT.NLIST(J)) WRITE(31,07230) CARD(J)
07230 FORMAT(/' (WARNING) CONTROL CARD ',A8,' IS MISSING.',/
     +        ' DEFAULT VALUES WILL BE ASSUMED.')
07100 CONTINUE
      REWIND (33)
      GOTO 07900
C
07220 WRITE(31,07200) CARD
07200 FORMAT(/' ERROR:  EARLY END OF FILE ENCOUNTERED WHILE'/
     +        '         READING CONTROL CARDS FROM UNIT 33.'/
     +        '         SEQUENCE OF CONTROL CARDS SHOULD'/
     +        '         BE AS FOLLOWS,'/
     +        '         ', 4(A8,', '))
      STOP 217
C
07900 CONTINUE
C*******************
C
C  BEGIN PROCESSING OF PARAMETERS AND OPTIONS
C
C*******************
C  SET THE TRIP TYPE SELECTED - TPTYPE = 1,2 OR 3
      IF(HW) TPTYPE = 1
      IF(HNW) THEN
	  IF (TPTYPE .GT. 0) THEN 
	    WRITE (31,'(A)') ' ERROR:  MORE THAN ONE TRIP TYPE SELECTED'
	    STOP
        ELSE
	    TPTYPE = 2
        ENDIF
      ENDIF
      IF(OTH) THEN
        IF (TPTYPE .GT. 0) THEN 
	    WRITE (31,'(A)') ' ERROR:  MORE THAN ONE TRIP TYPE SELECTED'
	    STOP
        ELSE
		TPTYPE = 3
        ENDIF
      ENDIF
C*******************
C     HOV OPTIONS ONLY POSSIBLE FOR WORK TRIPS
C*******************
      IF ((HOV2 .OR. HOV3) .AND. (TPTYPE .NE. 1)) THEN
	    WRITE (31,'(A)') ' ERROR:  HOV OPTION COMBINED WITH NONWORK TR
     AIP'
	    STOP
	ENDIF
      
	IF ((LOW_INC) .OR. (HI_INC)) THEN
        IF ((.NOT. HOV2) .AND. (.NOT. HOV3)) THEN
	    WRITE (31,'(A)') ' ERROR:  INCOME OPTION ON WITHOUT HOV OPTION
     A ON'
	    STOP
        ENDIF
	ENDIF
C*******************
C  START REPORTING INPUT PARAMETERS
C*******************
	WRITE (31, '(/A)') ' INPUT PARAMETERS FROM NAMELISTS' 
	WRITE (31,'(100A1)') ASTERIX
C
 	WRITE (31,'(/A)') ' &PARAM'
	WRITE (31,'(A,I5,A)') '   ZONES=', ZONES, ' HIGHEST ZONE NUMBER'
	IF (ZONES .GT. 4000) THEN
	  WRITE (*,'(/A)') ' ERROR  ZONES IS GREATER THAN 4000'
	  STOP
	ENDIF

      WRITE (31,'(/A)') '   ZONES IN CENTRAL BUSINESS DISTRICT' 
	WRITE (31,'(A,10I6)') '   CBDZON=', (CBDZON(I), I=1,10)
	DO J=11,200,10
	  IF (CBDZON(J) .NE. 0) THEN
	    WRITE (31,'(10X,10I6)') (CBDZON(I), I=J,J+9)
	  ENDIF
	ENDDO
	
	WRITE (31,'(/A,I5,A)') '   RNSEED=', RNSEED, 
     A' SEED FOR RANDOM NUMBER GENERATION'
	WRITE (31,'(10X, A)')' (IF ZERO, DEFAULT DERIVED FROM CLOCK TIME)'
     
	WRITE (31,'(/A)') '   THE COEFFICIENTS USED IN THE LOGIT EQUATION'    	
	WRITE (31,'(21X,A)')'IVT     COST    EXC     BIAS    OVT     HWAY' 
     	WRITE (31,'(A,6F8.4)') '   NON-CBD TRIPS= ', COEFF1
     	WRITE (31,'(A,6F8.4)') '   CBD TRIPS    = ', COEFF2

      WRITE (31,'(/A,4I5)')'   AUTO PARKING COST BY ZONE TYPE (CENTS)=',
     A APC
      WRITE (31,'(A,4I5)') '   TIME TO WALK FROM AUTO TO DESTINATION BY 
     AZONE TYPE (MIN)=', WFA
	
	WRITE (31,'(/A)') '   CBD ZONES IN CBD PARKING SUBMODEL' 
	WRITE (31,'(A,10I6)') '   PRKZON=', (PRKZON(I), I=1,10)
	DO J=11,200,10
	  IF (PRKZON(J) .NE. 0) THEN
	    WRITE (31,'(10X,10I6)') (PRKZON(I), I=J,J+9)
	  ENDIF
      ENDDO
      WRITE (31,'(/A,I5)') '   TRIPS SIMULATED PER INTERCHANGE=', ITER
	
	WRITE (31,'(A)') ' &END'
C
 	WRITE (31,'(/A)') ' &OPTION'

	WRITE(31,'(A,L1,A)') '   HW= ',HW,' HOME TO WORK TRIP TYPE (T)'
	WRITE(31,'(A,L1,A)') '   HNW= ',HNW,' HOME TO NONWORK TRIP TYPE (F
     A)'
	WRITE(31,'(A,L1,A)') '   OTH= ',OTH,' NONHOME TRIP TYPE (F)'
	WRITE(31,'(A,L1,A)') '   HOV2= ',HOV2,' RUN TWO OR MORE PERSON HOV
     A MODEL (F)'
	WRITE(31,'(A,L1,A)') '   HOV3= ',HOV3,' RUN THREE OR MORE PERSON H
     AOV MODEL (F)'
	WRITE(31,'(A,L1,A)') '   TOLL= ',TOLL,' OUTPUT TOLL AND NONTOLL AU
     ATO TRIP TABLES (F)'
	WRITE(31,'(A,L1,A)') '   AOCC= ',AOCC,' INPUT AUTO OCCUPANCY TABLE
     A (F)'
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
	WRITE(31,'(A,L1,A)') '   LOW_INC= ',LOW_INC,' BELOW MEDIAN EARNING
     AS WORK TRIPS OPTION (F)'
	WRITE(31,'(A,L1,A)') '   HI_INC= ',HI_INC,' ABOVE MEDIAN EARNINGS
     AWORK TRIPS OPTION (F)'
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
	WRITE(31,'(A,L1,A)') '   ASM_AREA= ',ASM_AREA,' APPROACH SUBMODEL
     A DISTRIBUTION PARAMETERS READ IN BY ZONE AREA TYPE (F)'
	WRITE(31,'(A,L1,A)') '   ASM_ZONE= ',ASM_ZONE,' APPROACH SUBMODEL
     A DISTRIBUTION PARAMETERS READ IN BY ZONE (F)'
	WRITE(31,'(A,L1,A)') '   INCOST= ',INCOST,' AVERAGE COST FOR AUTO 
     AIS INPUT BY ZONE (F)'
	WRITE(31,'(A,L1,A)') '   TRACE= ',TRACE,' WRITE INTERCHANGE VALUES
     A (F)'

	WRITE (31,'(A)') ' &END'
C
 	WRITE (31,'(/A)') ' &PROCESS'
C
      WRITE (31,'(/A)') '   ORIGIN ZONES TO BE PROCESSED' 
	WRITE (31,'(A,10I6)') '   PZOI=', (PZOI(I), I=1,10)
	DO J=11,200,10
	  IF (PZOI(J) .NE. 0) THEN
	    WRITE (31,'(8X,10I6)') (PZOI(I), I=J,J+9)
	  ENDIF
	ENDDO
	
	WRITE (31,'(/A)') '   DESTINATION ZONES TO BE PROCESSED' 
	WRITE (31,'(A,10I6)') '   QZOI=', (QZOI(I), I=1,10)
	DO J=11,200,10
	  IF (QZOI(J) .NE. 0) THEN
	    WRITE (31,'(8X,10I6)') (QZOI(I), I=J,J+9)
	  ENDIF
	ENDDO

	WRITE (31,'(/A)') ' &END'
C
 	WRITE (31,'(/A)') ' &SYSTEM'
	WRITE (31,'(A,I5,A)') '   SPDWLK=',SPDWLK,' THE SYSTEM-WIDE SPEED 
     AOF WALKING (MPH*10)'

      WRITE (31,'(/A)')'   AUTO SPEED (MPH) IN APPROACH SUBMODEL BY ZONE
     A TYPE'
	WRITE (31,'(A,4I5)') '   SPEEDS(1-4)=',(SPEEDS(J),J=1,4)
      WRITE (31,'(A)') '   BUS SPEED (MPH( IN APPROACH SUBMODEL BY ZONE 
     ATYPE'
	WRITE (31,'(A,4I5)') '   SPEEDS(5-8)=',(SPEEDS(J),J=5,8)
	WRITE (31,'(/A,I5,A)')'   DRVOT=',DRVOT,' DRIVER''S VALUE OF TIME 
     A(CENTS/MIN)'
	WRITE (31,'(A,I5,A)')'   AFC1=',AFC1,' AUTO FIXED COST FOR DRIVER 
     A(CENTS)'
	WRITE (31,'(A,I5,A)') '   AFC2=',AFC2,' AUTO FIXED COST FOR PASSEN
     AGER (CENTS)'
	WRITE (31,'(A,I5,A)') '   W2PNR=',W2PNR,' WALK TIME TO STATION FRO
     AM PARK & RIDE FACILITY (MIN)'
	WRITE (31,'(A,F6.2,A)') '   DISCNT=',DISCNT,' DISCOUNT FACTOR FOR 
     ACOSTS'

	WRITE (31,'(A)') ' &END'
C
C     CREATE EXTERNAL MATRIX FILE NAMES
C
C     FIRST MODE
C
      IF (TABLE_FMD .LT. 10) THEN
        WRITE (CTAB1,'(I1)') TABLE_FMD
        TEMX_FMD = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((TABLE_FMD .GE. 10).AND.(TABLE_FMD .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') TABLE_FMD
        TEMX_FMD = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (TABLE_FMD .GE. 100) THEN
        WRITE (CTAB3,'(I3)') TABLE_FMD
        TEMX_FMD = 'mf'//CTAB3//'.emx'
      ENDIF
C
C     LAST MODE
C
      IF (TABLE_LMD .LT. 10) THEN
        WRITE (CTAB1,'(I1)') TABLE_LMD
        TEMX_LMD = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((TABLE_LMD .GE. 10).AND.(TABLE_LMD .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') TABLE_LMD
        TEMX_LMD = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (TABLE_LMD .GE. 100) THEN
        WRITE (CTAB3,'(I3)') TABLE_LMD
        TEMX_LMD = 'mf'//CTAB3//'.emx'
      ENDIF
C
C     IN-VEHICLE TIME
C
      IF (TABLE_IVT .LT. 10) THEN
        WRITE (CTAB1,'(I1)') TABLE_IVT
        TEMX_IVT = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((TABLE_IVT .GE. 10).AND.(TABLE_IVT .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') TABLE_IVT
        TEMX_IVT = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (TABLE_IVT .GE. 100) THEN
        WRITE (CTAB3,'(I3)') TABLE_IVT
        TEMX_IVT = 'mf'//CTAB3//'.emx'
      ENDIF
C
C     OUT-OF-VEHICLE TIME
C
      IF (TABLE_OVT .LT. 10) THEN
        WRITE (CTAB1,'(I1)') TABLE_OVT
        TEMX_OVT = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((TABLE_OVT .GE. 10).AND.(TABLE_OVT .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') TABLE_OVT
        TEMX_OVT = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (TABLE_OVT .GE. 100) THEN
        WRITE (CTAB3,'(I3)') TABLE_OVT
        TEMX_OVT = 'mf'//CTAB3//'.emx'
      ENDIF
C
C     HEADWAY
C
      IF (TABLE_HWAY .LT. 10) THEN
        WRITE (CTAB1,'(I1)') TABLE_HWAY
        TEMX_HWAY = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((TABLE_HWAY .GE. 10).AND.(TABLE_HWAY .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') TABLE_HWAY
        TEMX_HWAY = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (TABLE_HWAY .GE. 100) THEN
        WRITE (CTAB3,'(I3)') TABLE_HWAY
        TEMX_HWAY = 'mf'//CTAB3//'.emx'
      ENDIF
C
C     PRIORITY MODE
C
      IF (TABLE_PMD .LT. 10) THEN
        WRITE (CTAB1,'(I1)') TABLE_PMD
        TEMX_PMD = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((TABLE_PMD .GE. 10).AND.(TABLE_PMD .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') TABLE_PMD
        TEMX_PMD = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (TABLE_PMD .GE. 100) THEN
        WRITE (CTAB3,'(I3)') TABLE_PMD
        TEMX_PMD = 'mf'//CTAB3//'.emx'
      ENDIF
C
C     TRANSIT FARE
C
      IF (TABLE_FARE .LT. 10) THEN
        WRITE (CTAB1,'(I1)') TABLE_FARE
        TEMX_FARE = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((TABLE_FARE .GE. 10).AND.(TABLE_FARE .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') TABLE_FARE
        TEMX_FARE = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (TABLE_FARE .GE. 100) THEN
        WRITE (CTAB3,'(I3)') TABLE_FARE
        TEMX_FARE = 'mf'//CTAB3//'.emx'
      ENDIF
C
C     HIGHWAY TIME
C
      IF (TABLE_HTIME .LT. 10) THEN
        WRITE (CTAB1,'(I1)') TABLE_HTIME
        TEMX_HTIME = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((TABLE_HTIME .GE. 10).AND.(TABLE_HTIME .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') TABLE_HTIME
        TEMX_HTIME = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (TABLE_HTIME .GE. 100) THEN
        WRITE (CTAB3,'(I3)') TABLE_HTIME
        TEMX_HTIME = 'mf'//CTAB3//'.emx'
      ENDIF
C
C     HIGHWAY DISTANCE
C
      IF (TABLE_HDIST .LT. 10) THEN
        WRITE (CTAB1,'(I1)') TABLE_HDIST
        TEMX_HDIST = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((TABLE_HDIST .GE. 10).AND.(TABLE_HDIST .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') TABLE_HDIST
        TEMX_HDIST = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (TABLE_HDIST .GE. 100) THEN
        WRITE (CTAB3,'(I3)') TABLE_HDIST
        TEMX_HDIST = 'mf'//CTAB3//'.emx'
      ENDIF
C
C     INPUT PERSON TRIPS
C
      IF (TABLE_PTRIP .LT. 10) THEN
        WRITE (CTAB1,'(I1)') TABLE_PTRIP
        TEMX_PTRIP = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((TABLE_PTRIP .GE. 10).AND.(TABLE_PTRIP .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') TABLE_PTRIP
        TEMX_PTRIP = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (TABLE_PTRIP .GE. 100) THEN
        WRITE (CTAB3,'(I3)') TABLE_PTRIP
        TEMX_PTRIP = 'mf'//CTAB3//'.emx'
      ENDIF
C
C     OPTIONAL AUTO OCCUPANCY
C
      IF (AOCC) THEN
        IF (TABLE_AOCC .LT. 10) THEN
          WRITE (CTAB1,'(I1)') TABLE_AOCC
          TEMX_AOCC = 'mf'//CTAB1//'.emx'
        ENDIF  

        IF ((TABLE_AOCC .GE. 10).AND.(TABLE_AOCC .LT. 100)) THEN
          WRITE (CTAB2,'(I2)') TABLE_AOCC
          TEMX_AOCC = 'mf'//CTAB2//'.emx'
        ENDIF  

        IF (TABLE_AOCC .GE. 100) THEN
          WRITE (CTAB3,'(I3)') TABLE_AOCC
          TEMX_AOCC = 'mf'//CTAB3//'.emx'
        ENDIF                  
      ENDIF 
C
C     OUTPUT TRANSIT PERSON TRIPS
C
      IF (TABLE_TRANSIT .LT. 10) THEN
        WRITE (CTAB1,'(I1)') TABLE_TRANSIT
        TEMX_TRANSIT = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((TABLE_TRANSIT .GE. 10).AND.(TABLE_TRANSIT .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') TABLE_TRANSIT
        TEMX_TRANSIT = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (TABLE_TRANSIT .GE. 100) THEN
        WRITE (CTAB3,'(I3)') TABLE_TRANSIT
        TEMX_TRANSIT = 'mf'//CTAB3//'.emx'
      ENDIF
C
C     OUTPUT AUTO PERSON TRIPS
C
      IF (TABLE_AUTO .LT. 10) THEN
        WRITE (CTAB1,'(I1)') TABLE_AUTO
        TEMX_AUTO = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((TABLE_AUTO .GE. 10).AND.(TABLE_AUTO .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') TABLE_AUTO
        TEMX_AUTO = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (TABLE_AUTO .GE. 100) THEN
        WRITE (CTAB3,'(I3)') TABLE_AUTO
        TEMX_AUTO = 'mf'//CTAB3//'.emx'
      ENDIF      
C
 	WRITE (31,'(/A)') ' &TABNUM'
	WRITE (31,'(A,I2,A,A)') '   FIRST TRANSIT MODE TABLE= MF',
     A  TABLE_FMD,': ',TEMX_FMD
	WRITE (31,'(A,I2,A,A)') '   LAST TRANSIT MODE TABLE= MF',
     A  TABLE_LMD,': ',TEMX_LMD
	WRITE (31,'(A,I2,A,A)') '   TRANSIT IN-VEHICLE TIME TABLE= MF',
     A  TABLE_IVT,': ',TEMX_IVT
	WRITE (31,'(A,I2,A,A)') '   TRANSIT OUT-OF-VEHICLE TIME TABLE= MF',
     A  TABLE_OVT,': ',TEMX_OVT
	WRITE (31,'(A,I2,A,A)') '   TRANSIT FIRST HEADWAY TABLE= MF',
     A  TABLE_HWAY,': ',TEMX_HWAY
	WRITE (31,'(A,I2,A,A)') '   TRANSIT PRIORITY MODE TABLE= MF',
     A  TABLE_PMD,': ',TEMX_PMD
	WRITE (31,'(A,I2,A,A)') '   TRANSIT FARE TABLE= MF',
     A  TABLE_FARE,': ',TEMX_FARE
	WRITE (31,'(A,I2,A,A)') '   HIGHWAY TIME TABLE= MF',
     A  TABLE_HTIME,': ',TEMX_HTIME
	WRITE (31,'(A,I2,A,A)') '   HIGHWAY DISTANCE TABLE= MF',
     A  TABLE_HDIST,': ',TEMX_HDIST
	WRITE (31,'(A,I2,A,A)') '   INPUT PERSON TRIP TABLE= MF',
     A  TABLE_PTRIP,': ',TEMX_PTRIP
      
      IF (AOCC)
     A  WRITE (31,'(A,I2,A,A)') '   INPUT AUTO OCCUPANCY TABLE= MF',
     B    TABLE_AOCC,': ',TEMX_AOCC
      
	WRITE (31,'(A,I2,A,A)') '   OUTPUT TRANSIT PERSON TRIP TABLE= MF',
     A  TABLE_TRANSIT,': ',TEMX_TRANSIT
	WRITE (31,'(A,I2,A,A)') '   OUTPUT AUTO PERSON TRIP TABLE= MF',
     A  TABLE_AUTO,': ',TEMX_AUTO

	WRITE (31,'(A)') ' &END'

	WRITE (31,'(100A1)') ASTERIX
C
C     COMPLETE LOGICAL ARRAYS THAT SHOW ZONES TO BE PROCESSED, CBD ZONES
C     AND ZONES IN CBD PARKING MODEL
C
	DO 10 I=1,200
	
	IF ((PZOI(I) .GT. 4000) .OR. (PZOI(I) .LT. -4000)) THEN 
	  WRITE (31,'(A,I5)') ' ERROR:  ORIGIN ZONE NUMBER OUT OF RANGE',
     A	PZOI(I)
	  STOP
	ENDIF
	    
	IF (PZOI(I) .GT. 0) THEN
	  J=PZOI(I)
	  ZOI(J)=.TRUE.
	ENDIF

	IF (PZOI(I) .LT. 0) THEN
	  JJ=-PZOI(I)
	  DO J=JJ,1,-1
	    IF (ZOI(J)) GO TO 10 
	    ZOI(J) = .TRUE.
	  ENDDO
	ENDIF

   10 CONTINUE
C
   	DO 15 I=1,200
	
	IF ((QZOI(I) .GT. 4000) .OR. (QZOI(I) .LT. -4000)) THEN 
	  WRITE (31,'(A,I5)') ' ERROR:  DESTINATION ZONE NUMBER OUT OF RAN
     AGE', QZOI(I)
	  STOP
	ENDIF
	    
	IF (QZOI(I) .GT. 0) THEN
	  J=QZOI(I)
	  DZOI(J)=.TRUE.
	ENDIF

	IF (QZOI(I) .LT. 0) THEN
	  JJ=-QZOI(I)
	  DO J=JJ,1,-1
	    IF (DZOI(J)) GO TO 15 
	    DZOI(J) = .TRUE.
	  ENDDO
	ENDIF

   15 CONTINUE
C
	DO 20 I=1,200
	
	IF ((CBDZON(I) .GT. 4000) .OR. (CBDZON(I) .LT. -4000)) THEN 
	  WRITE (31,'(A,I5)') ' ERROR:  CBD ZONE NUMBER OUT OF RANGE',
     A	CBDZON(I)
	  STOP
	ENDIF

	IF (CBDZON(I) .GT. 0) THEN
	  J=CBDZON(I)
	  ZCBD(J)=.TRUE.
	ENDIF

	IF (CBDZON(I) .LT. 0) THEN
	  JJ=-CBDZON(I)
	  DO J=JJ,1,-1
	    IF (ZCBD(J)) GO TO 20 
	    ZCBD(J) = .TRUE.
	  ENDDO
	ENDIF

   20 CONTINUE
C
	DO 30 I=1,200

	IF ((PRKZON(I) .GT. 0) .AND. (.NOT. HW)) THEN
	  WRITE (31,'(A,I5)') ' ERROR:  CBD PARKING ZONE MODEL SPECIFIED F
     AOR NON HOM-WORK TRIPS'
	  STOP
	ENDIF

	IF ((PRKZON(I) .GT. 4000) .OR. (PRKZON(I) .LT. -4000)) THEN 
	  WRITE (31,'(A,I5)') ' ERROR:  CBD PARKING ZONE NUMBER OUT OF RAN
     AGE',PRKZON(I)
	  STOP
	ENDIF

	IF (PRKZON(I) .GT. 0) THEN
	  J=PRKZON(I)
	  ZCBD_PARK(J)=.TRUE.
	ENDIF

	IF (PRKZON(I) .LT. 0) THEN
	  JJ=-PRKZON(I)
	  DO J=JJ,1,-1
	    IF (ZCBD_PARK(J)) GO TO 30 
	    ZCBD_PARK(J) = .TRUE.
	  ENDDO
	ENDIF

   30 CONTINUE
C
      MAXZONES = 0
      NUMZONE = 0
	NUMZONE_Q = 0
	NUMCBD = 0
	NUMCBD_PARK = 0

	DO I=1,ZONES
	  IF (ZOI(I)) MAXZONES = I
	  IF (ZOI(I)) NUMZONE=NUMZONE+1
	  IF (DZOI(I)) NUMZONE_Q=NUMZONE_Q+1
	  IF (ZCBD(I)) NUMCBD=NUMCBD+1
	  IF (ZCBD_PARK(I)) NUMCBD_PARK=NUMCBD_PARK+1

	  IF (ZCBD_PARK(I) .AND. (.NOT. ZCBD(I))) THEN
	    WRITE (31,'(A,2I5)') ' ERROR:  CBD PARKING ZONE AND CBD ZONE C
     AONFLICT',I
		STOP
	  ENDIF
	ENDDO

	WRITE (31,'(/A,I3)') ' TRIP TYPE FOR PROCESSING=',TPTYPE

C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
      IF ((.NOT. HW) .AND. (LOW_INC .OR. HI_INC)) THEN
	  WRITE (31,'(/A)') ' ERROR:  INCOME OPTION SELECTED AND TRIP TYPE
     N IS NOT HOME-WORK'
	  STOP
      ENDIF

	IF (LOW_INC .AND. HI_INC) THEN
        WRITE (31,'(/A)') ' ERROR:  BOTH INCOME OPTIONS SELECTED'
	  STOP
      ENDIF
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
	WRITE (31,'(A,I5)') ' NUMBER OF ORIGIN ZONES=',NUMZONE
	IF (NUMZONE .LT. 1) THEN
	  WRITE (31,'(/A)') ' ERROR:  NO ORIGIN ZONES SELECTED'
	  STOP
	ENDIF
      IF (NUMZONE .GT. 4000) THEN
	  WRITE (31,'(/A)') ' ERROR:  NUMZONE GREATER THAN 4000'
	  STOP
	ENDIF

	WRITE (31,'(A,I5)') ' NUMBER OF DESTINATION ZONES=',NUMZONE_Q
	IF (NUMZONE_Q .LT. 1) THEN
	  WRITE (31,'(/A)') ' ERROR:  NO DESTINATION ZONES'
	  STOP
	ENDIF
      IF (NUMZONE_Q .GT. 4000) THEN
	  WRITE (31,'(/A)') ' ERROR:  NUMZONE_Q GREATER THAN 4000'
	  STOP
	ENDIF

	WRITE (31,'(A,I5)') ' HIGHEST ORIGIN ZONE NUMBER=',MAXZONES

	WRITE (31,'(A,I5)') ' NUMBER OF CBD ZONES SPECIFIED=',NUMCBD
	IF (NUMCBD .GT. 200) THEN
	  WRITE (31,'(/A)') ' ERROR:  NUMCBD GREATER THAN 200'
	  STOP
	ENDIF

	WRITE (31,'(A,I5)') ' NUMBER OF CBD PARKING ZONES=',NUMCBD_PARK
	IF (NUMCBD_PARK .GT. 200) THEN
	  WRITE (31,'(/A)') ' ERROR:  NUMCBD_PARK GREATER THAN 200'
	  STOP
	ENDIF

	WRITE (31,'(100A1)') ASTERIX
C
      RETURN
      END