      SUBROUTINE TRIPS(ORIG)
      IMPLICIT INTEGER (A-Z)
C*******************
C  THIS SUBROUTINE PERFORMS THE OPERATION OF SIMULATING THE TRIPS
C       FROM THIS ORIGIN
C  THIS SUBROUTINE IS CALLED ONCE PER ZONE SELECTED FOR ANALYSIS
C*******************
C
C     THIS IS THE INTERCHANGE VERSION OF THE MODEL AND THERE ARE
C     SEVERAL CODING CHANGES IN THIS SUBROUTINE FROM THE TRIP
C     END VERSION.
C     FOR THE CALCULATION OF THE MODEL UTILITIES, THERE IS AN
C     ADDITIONAL LOOP OUTSIDE THE DO LOOP FOR ITERATIONS FOR ALL
C     DESTINATIONS FROM THE ORIGIN ZONE AS THE SIMULATION
C     PROGRESSES, TWO FILES ARE WRITTEN, ONE FOR SIMULATED TRANSIT
C     TRIP UTILITIES AND ONE FOR AUTO TRIP UTILITIES
C
C## This code has been revised to vectorize calculations - Heither, 11-02-2017
C 
      INCLUDE 'Common_params.fi'
	INCLUDE 'Common_data.fI'
      INCLUDE 'Common_emme4bank.fi'
	INCLUDE 'Common_approach_model.fi'
	INCLUDE 'Common_cbdparking.fi'

      REAL*4 ZUAUTO, ZUTRAN, ACOST
	REAL*4 D, DD, HEADER, R, RMEAN
	LOGICAL*4 CBDIND
      REAL,DIMENSION (ITER) :: INCOME
      REAL,DIMENSION (ITER) :: CAPK,ACOST1, AUTOCC, WALK, ARNUMT, ARNUMA
      INTEGER,DIMENSION (ITER) :: EC, EIVT, EOVT, EC1, INTOCC	
      INTEGER, ALLOCATABLE :: SEED(:)
      INTEGER SIZE
C
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
      OPEN (UNIT=951, FILE='emmemat/'//TEMX_AUTIL,
     A  ACCESS='DIRECT',RECL=1,STATUS='OLD')
      OPEN (UNIT=952, FILE='emmemat/'//TEMX_TUTIL,
     A  ACCESS='DIRECT',RECL=1,STATUS='OLD')
C
C--     ITER TRIPS ARE SIMULATED FOR EACH DESTINATION (THROUGH VECTORIZATION)
C     THIS STARTS THE LOOP BY DESTINATION ZONE
C
C DEBUGGING:         WRITE (*,'(A,2I4)') ' MAX. DEST:  ',ZONES 
	DO DEST =1,ZONES
		ZUAUTO = 0.0    !! AUTO UTILITY
		ZUTRAN = 0.0    !! TRANSIT UTILITY
                                   
C## -- PROCESS ZONAL INTERCHANGE IF NOT INTRAZONAL AND IF ACTUAL DESTINATION
		IF (DEST.NE.ORIG .AND. DZOI(DEST)) THEN
C              WRITE (*,'(A,2I5,L2)') ' Orig,DEST,DZOI(DEST): ',orig,
C     Adest,dzoi(dest) 
C DEBUGGING:                WRITE (31, '(A,10F8.4)')  '  RAN6 IN TRIPS',
C DEBUGGING:       ARAN6(1),RAN6(2),RAN6(3),RAN6(4),RAN6(5),RAN6(6),RAN6(7),RAN6(8),
C DEBUGGING:       BRAN6(9),RAN6(10)               
              
C##### Heither, 01-12-2019: GET ZONAL INTERCHANGE SEED VALUE              
              SEED1 = ((ORIG-1)*mcent) + DEST
              READ(40, REC=SEED1) ZNINTSD
              CALL RANDOM_SEED(SIZE=SIZE)
              ALLOCATE(SEED(SIZE))
              SEED = ZNINTSD
              CALL RANDOM_SEED(PUT=SEED)   !!! SET SEED VALUE
              IF (ORIG.EQ.1 .AND. DEST.LE.10) THEN
                  WRITE (31, '(A,3I8)')  '  ORIG,DEST,RANDOM SEED',
     AORIG,DEST,ZNINTSD
              ENDIF
              DEALLOCATE(SEED)
              
			ZLHT(DEST)=MAX(ZLHT(DEST),1.)   !! SET THE MINIMUM LINE HAUL DRIVING TIME TO ONE MINUTE  
			CBDIND=.FALSE.                  !! RUN A CHECK FOR A CBD DESTINATION 
			IF(ZCBD(DEST)) CBDIND=.TRUE.                              
              
C*******************
C  THE NEXT SECTION COMPUTES THE AUTO OPERATING COSTS IN CENTS
C*******************		   
			IF ((ZLHD(DEST).GT.0) .AND. (ZLHT(DEST).GT.0)) THEN 
				CALL AUTCST(ORIG,DEST,ZLHD(DEST),ZLHT(DEST),ACOST)
			ELSE
				ACOST = 0.0
              ENDIF       
C               
C*******************
C  THE NEXT SECTION OBTAINS THE INCOME OF THE TRIPMAKER
C      IF THIS A HOME BASED TRIP START WITH THE ZONAL MEDIAN INCOME
C         AT THE DESTINATION RATHER THAN AT THE ORIGIN
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
			IF (TPTYPE .NE. 3) THEN
				MEDINC=INC(ORIG)*100
				CALL INCDIS(ORIG,DEST,MEDINC,INCOME)
C  IF WE HAVE A NON-HOME BASED TRIP USE THE AVERAGE REGIONAL
C     2007 ACS HH INCOME FOR CHICAGO METRO AREA
			ELSE
				INCOME = 59300.
              ENDIF 

			ARNUMT=100.     !! SINGLE SIMULATION TRANSIT UTILITY DEFAULT
			ARNUMA=100.     !! SINGLE SIMULATION AUTO UTILITY DEFAULT              
                         
C  THE CALL TO PRKCST OBTAINS THE COST OF PARKING FOR A HIGHWAY TRIP
C    CAPK CONTAINS COST OF PARKING
C    WALK IS THE AVERAGE WALK TIME FROM THE AUTO TO THE DESTINATION			
			INTOCC = 0  !! AUTO OCCUPANCY
			CALL PRKCST(ORIG,DEST,INCOME,CAPK,WALK,INTOCC)
			ACOST1=.5*CAPK+ACOST+AFC1  !! AUTO COST (NOW VECTORIZED)
C
C     COSTS ARE DISCOUNTED FROM 1990 TO 1970 (CALIBRATION YEAR)
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
C     COSTS ARE ESTIMATED IN CURRENT DOLLARS FOR I290 RUNS AND MODEL 
C     COST COEFFICIENT HAS BEEN ADJUSTED ACCORDINGLY, SO DISCNT IS 
C     INPUT AS 1.00
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
              ACOST1 = ACOST1 * DISCNT              
                            
C     USE AUTO OCCUPANCY DETERMINED IN PRKCBD IF DEST IS A CBD ZONE
C     THAT USES PRKCBD
			AUTOCC = ZOCC(DEST)
			WHERE(INTOCC .GT. 0) AUTOCC=INTOCC              
              
C    COMPUTE FINAL AUTO COST
			ACOST1=ACOST1/AUTOCC              
C
C -- COMPUTE AUTO UTILITY -- USE ACOST1 NOW
              IF(.NOT.CBDIND) THEN
	            ARNUMA= COEFF1(1)*ZLHT(DEST)+
     A            COEFF1(2)*ACOST1+
     B            COEFF1(3)*WALK
	        ELSE
	            ARNUMA= COEFF2(1)*ZLHT(DEST)+
     A            COEFF2(2)*ACOST1+
     B            COEFF2(3)*WALK
	        ENDIF

              WHERE(ARNUMA>100) ARNUMA = 100.              
C
C     WRITE UTILITIES IF TRACE IS TURNED ON
	        IF (TRACE) THEN
	            WRITE (31, '(/A)') ' HIGHWAY INPUTS IN SUBROUTINE TRIPS'
	            WRITE (31, '(A,I6)') '   ORIGIN ZONE=', ORIG
	            WRITE (31, '(A,I6)') '   DESTINATION ZONE=', DEST
	            WRITE (31, '(A,I8)') '   HOUSEHOLD MEDIAN INCOME=', MEDINC
	            WRITE (31, '(A,F8.0)') '   HOUSEHOLD INCOME=', INCOME(1)
                  WRITE (31, '(A,F8.3)') '   HIGHWAY TRAVEL TIME (ZLHT)=
     A',ZLHT(DEST)
  	            WRITE (31, '(A,F8.3)') '   HIGHWAY DISTANCE (ZLHD)=',
     AZLHD(DEST)                  
  	            WRITE (31, '(A,F8.3)') '   AUTO OCCUPANCY (AUTOCC)= ',
     AAUTOCC(1)                    
  	            WRITE (31, '(A,F8.3)') '   CBD PARKING COST (CAPK)= ',
     ACAPK(1)                    
  	            WRITE (31, '(A,I8)')   '   AUTO FIXED COST (AFC1)= ',AFC1
  	            WRITE (31, '(A,F8.3)') '   FINAL AUTO COST (ACOST1)= ',
     AACOST1(1)                   
	            WRITE (31, '(A,F8.3)') '   WALK TIME (WALK)= ',WALK(1)
	            WRITE (31, '(A,F8.3)') '   HIGHWAY UTILITY (ARNUMA)=',
     AARNUMA(1)                   
              ENDIF               
C ----------------------------------------------------------------------------              
C    -- NOW LOOK AT TRANSIT --  
C ----------------------------------------------------------------------------
C  FOLLOWING CHANGE SETS MAXIMUM TRANSIT COST TO EQUAL WALK COST
C  WHEN ZONES ARE UNCONNECTED BY TRANSIT AND LESS THAN 1.5 MILES
C  APART.  THREE MPH WALK SPEED ASSUMED.
			IF ((PMD(DEST) .LT. 3) .AND. (ZLHD(DEST) .LT. 1.5)) THEN
				ARNUMT = (ZLHD(DEST)*20.0)*COEFF1(5) - COEFF1(4)
				IF (CBDIND) ARNUMT = (ZLHD(DEST)*20.0)*COEFF2(5) - COEFF2(4)
              ENDIF              

C  NOW THAT THE DESTINATION HAS BEEN OBTAINED,
C      CHECK TO SEE IF TRANSIT WILL GO TO THAT ZONE
			IF (PMD(DEST).GE.3) THEN
C DEBUGGING:                  WRITE (*,'(A)') ' START TRAPP  '
C  THE CALL TO SUBROUTINE TRAPP WILL RETURN THE TRANSIT APPROACH TIMES
				CALL TRAPP(ORIG,DEST,EIVT,EOVT,EC)
				HEADER=0.5*HWAY(DEST)      
C DEBUGGING:                  WRITE (*,'(A)') ' TRAPP DONE  '
C     COSTS ARE DISCOUNTED FROM 1990 TO 1970 (CALIBRATION YEAR)
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
C     COSTS ARE ESTIMATED IN CURRENT DOLLARS AND MODEL COST COEFFICIENT 
C     HAS BEEN ADJUSTED ACCORDINGLY, SO DISCNT = 1.00
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
C
                  FARE(DEST) = FARE(DEST) * DISCNT
C                  EC = EC + DISCNT
	            EC1 = EC * DISCNT    !! NOW VECTORIZED          
C
C   COMPUTE TRANSIT UTILITY:  NOTE THAT SIGN ON TRANSIT BIAS COEFFICIENT
C   IS NEGATIVE BECAUSE IT IS ENTERED AS MINUS NUMBER.  TRANSIT BIAS IS
C   AN ADDITIONAL TRANSIT COST, I.E., TRANSIT IS LESS THAN 50 PERCENT
C   EVERYTHING ELSE BEING EQUAL.
C
                  IF(.NOT.CBDIND) THEN
	                ARNUMT= COEFF1(1)*IVT(DEST)+
     A	            COEFF1(2)*(FARE(DEST)+EC1)+
     B                COEFF1(3)*EIVT-
     C                COEFF1(4)+
     D                COEFF1(5)*(OVT(DEST)+EOVT)+
     E                COEFF1(6)*HEADER
	            ELSE
	                ARNUMT= COEFF2(1)*IVT(DEST)+
     A	            COEFF2(2)*(FARE(DEST)+EC1)+
     B                COEFF2(3)*EIVT-
     C                COEFF2(4)+
     D                COEFF2(5)*(OVT(DEST)+EOVT)+
     E                COEFF2(6)*HEADER
                  ENDIF
       
                  WHERE(ARNUMT>100) ARNUMT = 100.       
C                  WRITE (31, '(A,6F8.3)') '   ARNUMT 6=',ARNUMT(1),
C     AARNUMT(2),ARNUMT(3),ARNUMT(4),ARNUMT(5),ARNUMT(6)  
C                  WRITE (31, '(A,6F8.3)') '   ARNUMA 6=',ARNUMA(1),
C     AARNUMA(2),ARNUMA(3),ARNUMA(4),ARNUMA(5),ARNUMA(6)
C
C     WRITE UTILITIES IF TRACE IS TURNED ON
	            IF (TRACE) THEN
	                WRITE (31, '(/A)') 'TRANSIT INPUTS IN SUBROUTINE TRIPS'
	                WRITE (31, '(A,I6)') '   ORIGIN ZONE=', ORIG
	                WRITE (31, '(A,I6)') '   DESTINATION ZONE=', DEST
  	                WRITE (31, '(A,F8.3)') '   FIRST MODE (FMD)=',
     AFMD(DEST)                     
  	                WRITE (31, '(A,F8.3)') '   LAST MODE (LMD)=',
     ALMD(DEST)                       
  	                WRITE (31, '(A,F8.3)') '   PRIORITY MODE (PMD)=',
     APMD(DEST)                           
  	                WRITE (31, '(A,F8.3)') '   IN-VEHICLE TIME (IVT)=',
     AIVT(DEST)                        
  	                WRITE (31, '(A,F8.3)') '   TRANSIT FARE (FARE)=',
     AFARE(DEST)                     
  	                WRITE (31, '(A,F8.3)') '   OUT-OF-VEHICLE TIME (OVT)=
     A',OVT(DEST)
  	                WRITE (31, '(A,F8.3)') '   INITIAL WAIT TIME (1/2 
     AHEADWAY)=',HEADER
	                WRITE (31, '(A,I6)') '   APPROACH IVT (EIVT)=',EIVT(1)
  	                WRITE (31, '(A,I6)') '   APPROACH TRANSIT FARE (EC)='
     A,EC(1)                     
  	                WRITE (31, '(A,I6)') '   APPROACH OVT (EOVT)=',
     AEOVT(1)                        
	                WRITE (31, '(A,F8.3)') '   TRANSIT UTILITY (ARNUMT)=',
     AARNUMT(1)                      
	            ENDIF           
              ENDIF  !! END OF TRANSIT AVAILABLE PROCESSING                  
C              write (31, '(A,F8.3)') '   sum(ARNUMT)=',SUM(ARNUMT)
C
C    COMPUTE COMBINED UTILITY (COST)
C    VALUES ARE SCALED BY EXP(2) TO FORCE EVERYTHING POSITIVE
              ARNUMA = ARNUMA + 2
              ARNUMT = ARNUMT + 2     
C              write (31, '(A,F8.3)') '   sum(ARNUMT)=',SUM(ARNUMT)
              ZUAUTO = SUM(ARNUMA)/ITER  !! ZONAL AUTO UTILITY
              ZUTRAN = SUM(ARNUMT)/ITER  !! ZONAL TRANSIT UTILITY                           
          ENDIF  !! END OF INTERZONAL AND ACTUAL DESTINATION PROCESSSING
C
	    IF (TRACE .AND. DZOI(DEST)) THEN
	        WRITE (31, '(/A)') ' FINAL UTILITIES'
	        WRITE (31, '(A,I6)') '   ORIGIN ZONE=', ORIG
	        WRITE (31, '(A,I6)') '   DESTINATION ZONE=', DEST
	        WRITE (31, '(A,F12.2)') '   AUTO UTILITY (ZUAUTO)=', ZUAUTO
	        WRITE (31, '(A,F12.2)') '   TRANSIT UTILITY (ZUTRAN)=', ZUTRAN
          ENDIF
C
C     WRITE COMBINED COSTS INTO EMMEBANK
C       MF10 = AUTO COST
C       MF11 = TRANSIT COST
          P = ORIG
	    Q = DEST
          REC1 = ((P-1)*mcent) + Q
          WRITE(951, REC=REC1) ZUAUTO
          WRITE(952, REC=REC1) ZUTRAN
C     REPEAT FOR NEXT DESTINATION   
C DEBUGGING:          WRITE (*,'(A,I4)') ' END DEST LOOP: DEST  ',DEST
      ENDDO                  
        
      
C -- PROCESSING FOR THIS ORIGIN ZONE IS COMPLETED --
	WRITE (*,'(A5,I5,A10)'),' ZONE ',ORIG,' COMPLETED'
	WRITE (31,'(A5,I5,A10)'),' ZONE ',ORIG,' COMPLETED' 	
	CLOSE (951)
	CLOSE (952)
      RETURN
      END      
