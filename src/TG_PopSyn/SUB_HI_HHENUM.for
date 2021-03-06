      SUBROUTINE SUB_HI_HHENUM
      IMPLICIT INTEGER(A-Z)
C***********************************************************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C***********************************************************************
C
C     THIS SUBROUTINE READS THE TRAVEL SURVEY HOUSEHOLD FILE INPUT FILE.
C     A COMMA DELIMITED FILE WITH THE FOLLOWING VARIABLES:
C
C       1.  PUMA5 FOR HOUSEHOLD (173010 FOR EXAMPLE)
C       2.  HHTYPE CODE FOR HOUSEHOLD.  THIS CODE HAS THE SAME RANGE
C           AS THE HOUSEHOLD CODE IN THE PUMS FILE EXCEPT THAT IT IS 
C           DEFINED WITH VEHICLE AVAILABILITY REPLACING INCOME QUARTILE.
C           SEE BELOW.
C       3.  HOUSEHOLD SERIAL NUMBER BUILT FROM HH SAMPLE NUMBER AND 
C           WEEKDAY OF SURVEY (7 DIGIT SERIAL NUMBER FOLLOWED BY ONE 
C           DIGIT DAY CODE)
C       4.  FORTY-NINE HOUSEHOLD TRIP PRODUCTIONS/ORIGINS.  SEE BELOW.
C 
C     SUBROUTINE FILLS ARRAYS FROM HI_HHENUM_IN.TXT FILE.  THEN 
C     RANDOMLY SELECTS PUMS HOUSEHOLDS MATCHING TG SUBZONE HOUSEHOLD
C     CHARACTERISTICS.
C
C     EASH, APRIL 2012
C
C***********************************************************************
C
C     REVISED HOUSEHOLD TYPE DEFINITIONS
C
C       0 VEHICLES IN HH, HOUSEHOLDER UNDER 35
C
C           HHVTYPE
C        CHILDREN 0-15
C        0   1   2   3+  WORKERS  ADULTS   HH VEHICLES  HOUSEHOLDER
C       -------------------------------------------------------------
C        1  14  27  40      0        1          0            1
C        2  15  28  41      0        2          0            1   
C        3  16  29  42      0        3          0            1   
C        4  17  30  43      0        4+         0            1
C        5  18  31  44      1        1          0            1
C        6  19  32  45      1        2          0            1
C        7  20  33  46      1        3          0            1
C        8  21  34  47      1        4+         0            1
C        9  22  35  48      2        2          0            1
C       10  23  36  49      2        3          0            1
C       11  24  37  50      2        4+         0            1
C       12  25  38  51      3+       3          0            1
C       13  26  39  52      3+       4+         0            1
C
C       1 VEHICLE IN HH, HOUSEHOLDER UNDER 35 (ADD 52 TO ABOVE)
C
C       2 VEHICLES IN HH, HOUSEHOLDER UNDER 35 (ADD 104 TO ABOVE)
C 
C       3 OR MORE VEHICLES IN HH, HOUSEHOLDER UNDER 35 
C        (ADD 156 TO ABOVE)
C
C       0 VEHICLES IN HH, HOUSEHOLDER 35-64 (ADD 208 TO ABOVE)
C
C       1 VEHICLE IN HH, HOUSEHOLDER 35-64 (ADD 260 TO ABOVE)
C
C       2 VEHICLES IN HH, HOUSEHOLDER 35-64 (ADD 312 TO ABOVE)
C
C       3 OR MORE VEHICLES IN HH, HOUSEHOLDER 35-64 (ADD 364 TO ABOVE)
C
C       0 VEHICLES IN HH, HOUSEHOLDER 65+ (ADD 416 TO ABOVE)
C
C       1 VEHICLE IN HH, HOUSEHOLDER 65+ (ADD 468 TO ABOVE)
C
C       2 VEHICLES IN HH, HOUSEHOLDER 65+ (ADD 520 TO ABOVE)
C
C       3 OR MORE VEHICLES IN HH, HOUSEHOLDER 65+ (ADD 572 TO ABOVE)
C
C***********************************************************************
C
C     TRIP CATEGORIES
C
C     TRIPS BY WORKERS
C    
C     1.  HOME PRODUCTION TO WORKPLACE ATTRACTION (LOW INCOME WORKER)
C     2.  HOME PRODUCTION TO WORKPLACE ATTRACTION (HIGH INCOME WORKER)
C     3.  HOME PRODUCTION TO WORK RELATED ATTRACTION
C     4.  HOME PRODUCTION TO SCHOOL ATTRACTION
C     5.  HOME PRODUCTION TO OTHER (HOME) ATTRACTION
C     6.  HOME PRODUCTION TO OTHER (NONHOME) ATTRACTION
C     7.  HOME PRODUCTION TO SHOP ATTRACTION
C     8.  WORK PRODUCTION TO OTHER (HOME) ATTRACTION
C     9.  WORK PRODUCTION TO OTHER (NONHOME) ATTRACTION
C    10.  WORK PRODUCTION TO SHOP ATTRACTION
C    11.  WORK ORIGIN/DESTINATION TO WORK ORIGIN/DESTINATION 
C    12.  OTHER (HOME) ORIGIN TO OTHER (HOME) DESTINATION
C    13.  OTHER (HOME) ORIGIN TO OTHER (NONHOME) DESTINATION
C    14.  OTHER (HOME) ORIGIN TO SHOP (DESTINATION) 
C    15.  OTHER (NONHOME) ORIGIN TO OTHER (HOME) DESTINATION
C    16.  OTHER (NONHOME) ORIGIN TO OTHER (NONHOME) DESTINATION
C    17.  OTHER (NONHOME) ORIGIN TO SHOP DESTINATION  
C    18.  SHOP ORIGIN TO OTHER (HOME) DESTINATION
C    19.  SHOP ORIGIN TO OTHER (NONHOME) DESTINATION
C    20.  SHOP ORIGIN/DESTINATION TO SHOP ORIGIN/DESTINATION  
C
C     TRIPS BY NONWORKERS
C    
C    21.  HOME PRODUCTION TO SCHOOL ATTRACTION
C    22.  HOME PRODUCTION TO OTHER (HOME) ATTRACTION
C    23.  HOME PRODUCTION TO OTHER (NONHOME) ATTRACTION
C    24.  HOME PRODUCTION TO SHOP ATTRACTION
C    25.  OTHER (HOME) ORIGIN TO OTHER (HOME) DESTINATION
C    26.  OTHER (HOME) ORIGIN TO OTHER (NONHOME) DESTINATION
C    27.  OTHER (HOME) ORIGIN TO SHOP (DESTINATION) 
C    28.  OTHER (NONHOME) ORIGIN TO OTHER (HOME) DESTINATION
C    29.  OTHER (NONHOME) ORIGIN TO OTHER (NONHOME) DESTINATION
C    30.  OTHER (NONHOME) ORIGIN TO SHOP DESTINATION  
C    31.  SHOP ORIGIN TO OTHER (HOME) DESTINATION
C    32.  SHOP ORIGIN TO OTHER (NONHOME) DESTINATION
C    33.  SHOP ORIGIN/DESTINATION TO SHOP ORIGIN/DESTINATION  
C
C     TRIPS BY CHILDREN 12 TO 15
C    
C    34.  HOME PRODUCTION TO SCHOOL ATTRACTION
C    35.  HOME PRODUCTION TO OTHER (HOME) ATTRACTION
C    36.  HOME PRODUCTION TO OTHER (NONHOME) ATTRACTION
C    37.  HOME PRODUCTION TO SHOP ATTRACTION
C    38.  SCHOOL PRODUCTION TO OTHER (HOME) ATTRACTION
C    39.  SCHOOL PRODUCTION TO OTHER (NONHOME) ATTRACTION
C    40.  SCHOOL PRODUCTION TO SHOP ATTRACTION
C    41.  OTHER (HOME) ORIGIN TO OTHER (HOME) DESTINATION
C    42.  OTHER (HOME) ORIGIN TO OTHER (NONHOME) DESTINATION
C    43.  OTHER (HOME) ORIGIN TO SHOP (DESTINATION) 
C    44.  OTHER (NONHOME) ORIGIN TO OTHER (HOME) DESTINATION
C    45.  OTHER (NONHOME) ORIGIN TO OTHER (NONHOME) DESTINATION
C    46.  OTHER (NONHOME) ORIGIN TO SHOP DESTINATION  
C    47.  SHOP ORIGIN TO OTHER (HOME) DESTINATION
C    48.  SHOP ORIGIN TO OTHER (NONHOME) DESTINATION
C    49.  SHOP ORIGIN/DESTINATION TO SHOP ORIGIN/DESTINATION
C
C***********************************************************************
      INCLUDE 'COMMON_GEOG.FI'
      INCLUDE 'COMMON_PARAM.FI'
      INCLUDE 'COMMON_CONTROL.FI'
      INCLUDE 'COMMON_PUMSHHS.FI'
      INCLUDE 'COMMON_HIHHS.FI'
      INCLUDE 'COMMON_HHSUMS.FI'
      INCLUDE 'COMMON_HHMISC0.FI'
      INCLUDE 'COMMON_POPSYN.FI' 
      
      CHARACTER*8 SERIAL, HHPICK
      
      INTEGER*4  HHINPUMA(100,624), HHINREG(624)
      INTEGER :: BB, X, REC, TEMPID
      
      REAL*4  HH2, PROB_HIW, U, UU   
C--   HEITHER, 04-12-2018      
      INTEGER,DIMENSION (43056) :: HHCAT, HHCHOICE        !! NEW FOR HH CHOICE FILES
      INTEGER*4,DIMENSION (43056) :: HHCODE
      INTEGER*4,DIMENSION (43056,3274) :: HHIDS           !! NEW FOR HH CHOICE FILES
      INTEGER,DIMENSION (43056,3274) :: HHRARE            !! NEW FOR HH CHOICE FILES
      
      OPEN (UNIT=41,FILE='HI_HHENUM_IN.TXT',STATUS='OLD',ERR=941)
      OPEN (UNIT=42,FILE='HI_HHENUM_TRIP_OUT.TXT',STATUS='NEW',ERR=942)
      OPEN (UNIT=43,FILE="HHID_choices1.csv",STATUS='OLD',READONLY,
     A ERR=943)
      OPEN (UNIT=44,FILE="HHID_choices2.csv",STATUS='OLD',READONLY,
     A ERR=944)

      COUNT=0
C
C     LOAD UP THE TRAVEL SURVEY ARRAYS
C
    5 CONTINUE
      COUNT = COUNT + 1
     
      READ (41,*,END=10) HI_PUMA5(COUNT), HI_HHVTYPE(COUNT), 
     A  HI_SERNO(COUNT),  HI_ADULT(COUNT), HI_WORKER(COUNT), 
     B  HI_NONWORKER(COUNT), HI_CHILD(COUNT), HI_CHILD1215(COUNT),
     C  HI_VEH(COUNT), (HI_TRIPS(COUNT,J),J=1,49)
      
C      HI_NEWCODE(COUNT) = HI_PUMA5(COUNT) * 1000 + HI_HHVTYPE(COUNT)  !! CREATE NEWCODE FOR SURVEY TRIP FILE
C      
C      WRITE (16,'(3I7,1X,A,49I3)') COUNT, HI_PUMA5(COUNT),
C     A  HI_HHVTYPE(COUNT), HI_SERNO(COUNT), 
C     B  (HI_TRIPS(COUNT,J),J=1,49)  
C      
C      IF (COUNT .GT. 100) GO TO 10
      GO TO 5
      
   10 CONTINUE 
      COUNT = COUNT - 1
      WRITE (*,'(/A,I8)') 'TRAVEL SURVEY HH RECORDS READ= ', COUNT
      WRITE (16,'(/A,I8)') 'TRAVEL SURVEY HH RECORDS READ= ', COUNT
C
C     COMPUTE THREE INDICES FOR EACH TRAVEL SURVEY RECORD
C     1.  INSIDE PUMA5 IF HHS GREATER THAN 4 IN PUMA
C     2.  INSIDE REGION IF HHS GREATER THAN 4 IN REGION
C     3.  ALL ODDBALL HHS, REGIONAL HHS LESS THAN 5
C
      DO I = 1,COUNT
        HHTV = HI_HHVTYPE(I)
        HHINREG(HHTV) = HHINREG(HHTV) + 1
      
        DO P1=1,PUMA5
	    IF (HI_PUMA5(I) .EQ. P5_NUM(P1)) P5 = P1
        ENDDO     
      
        HHINPUMA(P5,HHTV) = HHINPUMA(P5,HHTV) + 1
        
C        WRITE (16,*) I, P5, HHTV, HHINREG(HHTV), HHINPUMA(P5,HHTV)
      ENDDO
C
C     PRINT HOUSEHOLD TYPES
C
     	WRITE (16,'(/A)') 'HOUSEHOLD TYPES IN TRAVEL SURVEY RECORDS'
      WRITE(16,'(/A)')'                    HOUSEHOLD SIZE'

      WRITE (16,'(A)')'  HHV         -------------------------- HHOLDER'
      WRITE (16,'(A)')' CODE  NUMBER ADULTS WORKERS CHILD  VEHS   CODE'
      WRITE (16,'(A)')'------------------------------------------------'
      DO HHTV=1,624
        VV = MOD(HHTV,208)
        IF (VV .EQ. 0) VV=208
        VEHS = 3
        IF (VV.GE.105 .AND. VV.LE.156) VEHS = 2
        IF (VV.GE.53 .AND. VV.LE.104) VEHS = 1
        IF (VV.LE.52) VEHS = 0
        
        WRITE (16,'(I4,6I7)') HHTV, HHINREG(HHTV), HHT_ADULT(HHTV),
     A    HHT_WORKER(HHTV), HHT_CHILD(HHTV), VEHS, HHT_HHOLDER(HHTV)
      ENDDO        
C
C     CHECK FOR ADEQUATE HHS IN SAMPLE
C
C      HHODD = 0
C      DO HHTV = 1,624  
C        IF (HHINREG(HHTV) .LT. 5) THEN
C          HHODD = HHODD + HHINREG(HHTV)
C          HHINREG(HHTV) = 0
C        ENDIF
C      ENDDO
C      DO P1 = 1,PUMA5
C        DO HHTV = 1,624
C          IF (HHINPUMA(P1,HHTV) .LT. 5) HHINPUMA(P1,HHTV) = 0
C        ENDDO
C      ENDDO 
      
C      DO I = 1,COUNT
C        HHTV = HI_HHVTYPE(I)
C        IF (HHINREG(HHTV) .EQ. 0) HIPROB_ODDHH(I) = 1
C      ENDDO 
      
C      HH1 = 0
C      DO HHTV = 1,624
C        HH1 = HH1 + HHINREG(HHTV)  
C      ENDDO
C      HH2 = 0
C      DO P1 = 1,PUMA5
C        DO HHTV = 1,624
C          HH2 = HH2 + HHINPUMA(P1,HHTV)
C        ENDDO
C      ENDDO
C      HH2 = HH2/FLOAT(PUMA5)
         
C      WRITE (16,'(/A,F8.2)') 'AVERAGE TRAVEL SURVEY HOUSEHOLDS PER PUMA 
C     AFOR SAMPLING=', HH2 
C      WRITE (16,'(A,I8)')  'TRAVEL SURVEY HOUSEHOLDS FOR REGIONAL SAMPLI
C     ANG=', HH1
C      WRITE (16,'(A,I6)')  'TRAVEL SURVEY HOUSEHOLDS FOR RANDOM SAMPLING
C     A=', HHODD 
      
C      DO HHTV = 1,624
C        WRITE (16,*) HHTV, HHINREG(HHTV)
C        IF (HHINREG(HHTV) .GT. 100) WRITE (16,*) 
C     A   (HHINPUMA(JJ,HHTV),JJ=1,100)
C      ENDDO  
C
C     CHANGE HOUSEHOLD TYPE IN ENUMERATED HOUSEHOLDS FROM INCOME BASE
C     TO VEHICLE BASE
C
      DO I = 1,6000000

        IF (PUMS_HHTYPE(I) .GT. 0) THEN
          HHT = PUMS_HHTYPE(I)  
          HH_HHOLDER = HHT_HHOLDER(HHT)
          
          PUMS_HHVTYPE(I) = MOD(HHT,52)
          IF (PUMS_HHVTYPE(I) .EQ. 0) PUMS_HHVTYPE(I) = 52
          
          PUMS_HHVTYPE(I) = PUMS_HHVTYPE(I) + (PUMS_VEH(I)) * 52
          PUMS_HHVTYPE(I) = PUMS_HHVTYPE(I) + (HH_HHOLDER-1) * 208
          PUMS_NEWTYPE(I) = PUMS_PUMA5(I) * 1000 + PUMS_HHVTYPE(I) !! NEED TO CALCULATE NEWCODE
          
        ENDIF
      ENDDO  
C
C     SELECT TRAVEL SURVEY HOUSEHOLDS MATCHING TG SUBZONE HOUSEHOLDS
C
      ALL_HH = 0
      WRITE (*,'(/A/)') 'MATCH TG SUBZONE HOUSEHOLDS TO TRAVEL SURVEY HO
     AUSEHOLDS'            
      WRITE (16,'(/A)') 'MATCH TG SUBZONE HOUSEHOLDS TO TRAVEL SURVEY HO
     AUSEHOLDS'  

C-- Heither, 04-13-20418: READ IN SURVEY HH CHOICE FILES  
C -- BEGIN PROCESSING HHID CHOICE FILE --       
      I = 0
      DO
	    I = I + 1
          READ(43,*, END=23) HHCODE(I), HHCAT(I), HHCHOICE(I)
          IF (HHCHOICE(I) > 0) THEN
C         -- POPULATE HHID CHOICES --                   
              DO J=1,HHCHOICE(I)                   
                  READ(44,*) HHIDS(I,J), HHRARE(I,J)
              ENDDO
          ENDIF   
C-- Heither, DEBUGGING                     
C          IF(I==1) THEN
C              WRITE (16,'(A,I10)') 'HHCODE(1)= ', HHCODE(I)
C 	        WRITE (16,'(A,I10)') 'HHCAT(1)= ', HHCAT(I)
C	        WRITE (16,'(A,I10)') 'HHCHOICE(1)= ', HHCHOICE(I)
C             WRITE (16,'(A,I10)') 'HHIDS(1,2)= ', HHIDS(I,2)
C              WRITE (16,'(A,I10)') 'HHIDS(1,3)= ', HHIDS(I,3)
C              WRITE (16,'(A,I10)') 'HHIDS(1,4)= ', HHIDS(I,4)
C              WRITE (16,'(A,I10)') 'HHIDS(1,5)= ', HHIDS(I,5) 
C              WRITE (16,'(A,I10)') 'HHIDS(1,6)= ', HHIDS(I,6)
C              WRITE (16,'(A,I10)') 'HHIDS(1,7)= ', HHIDS(I,7)
C              WRITE (16,'(A,I10)') 'HHIDS(1,Choices)= ', 
C     AHHIDS(I,HHCHOICE(I))
C              WRITE (16,'(A,I10)') 'HHRARE(1,2)= ', HHRARE(I,2)
C              WRITE (16,'(A,I10)') 'HHRARE(1,3)= ', HHRARE(I,3)
C              WRITE (16,'(A,I10)') 'HHRARE(1,4)= ', HHRARE(I,4)
C              WRITE (16,'(A,I10)') 'HHRARE(1,Choices)= ', 
C     AHHRARE(I,HHCHOICE(I))              
C          ENDIF 	 
C          IF(I==2) THEN
C              WRITE (16,'(A,I10)') 'HHCODE(2)= ', HHCODE(I)
C	        WRITE (16,'(A,I10)') 'HHCAT(2)= ', HHCAT(I)
C	        WRITE (16,'(A,I10)') 'HHCHOICE(2)= ', HHCHOICE(I)
C             WRITE (16,'(A,I10)') 'HHIDS(2,2)= ', HHIDS(I,2)
C              WRITE (16,'(A,I10)') 'HHIDS(2,3)= ', HHIDS(I,3)
C              WRITE (16,'(A,I10)') 'HHIDS(2,4)= ', HHIDS(I,4)
C              WRITE (16,'(A,I10)') 'HHIDS(2,5)= ', HHIDS(I,5) 
C              WRITE (16,'(A,I10)') 'HHIDS(2,6)= ', HHIDS(I,6)
C              WRITE (16,'(A,I10)') 'HHIDS(2,7)= ', HHIDS(I,7) 
C              WRITE (16,'(A,I10)') 'HHIDS(2,Choices)= ', 
C     AHHIDS(I,HHCHOICE(I))
C              WRITE (16,'(A,I10)') 'HHRARE(2,5)= ', HHRARE(I,5) 
C              WRITE (16,'(A,I10)') 'HHRARE(2,6)= ', HHRARE(I,6)
C              WRITE (16,'(A,I10)') 'HHRARE(2,7)= ', HHRARE(I,7) 
C              WRITE (16,'(A,I10)') 'HHRARE(2,Choices)= ', 
C     AHHRARE(I,HHCHOICE(I))              
C          ENDIF 
      ENDDO	
   23 CONTINUE 

      MAT_PUMA = 0
      MAT_ADJ = 0
      MAT_REG = 0
      MAT_ODD = 0
      PUMS_HH_IN = 0
      
      DO I = 1,6000000
C
C     GET ORIGINAL HHTYPE FOR HIGH INCOME WORKER ESTIMATION
C
        HHTV = PUMS_HHVTYPE(I)
C
        IF (HHTV .LT. 1) GO TO 29  
        
        PUMS_HH_IN = PUMS_HH_IN + 1
        
        HHT = PUMS_HHTYPE(I)
        RC = PUMS_ROWCOL(I)
        HH_WORKER = HHT_WORKER(HHT)
        HH_ADULT = HHT_ADULT(HHT)
        HH_INCOME = HHT_INC4(HHT)
        HHT_INDEX = MOD(HHT,13)
        IF (HHT_INDEX .EQ. 0) HHT_INDEX = 13
                
        TTL = 0
        TTH = 0
C
C     PROBABILITY OF HIGH INCOME WORKER
C
        IF (HH_WORKER .GT. 0) THEN
          PROB_HIW = HHTYPE_HWINC(HHT_INDEX,HH_INCOME)
        ELSE
          PROB_HIW = 0.0
        ENDIF
        
        IF (MOD(I,50000) .EQ. 0) 
     A    WRITE (*,'(A,I8)') ' TG SUBZONE HOUSEHOLD RECORDS READ= ', I
        
C        WRITE (16,*) HHT, PUMS_VEH(I), HHTV, RC
        
C-- Heither, 04-13-20418: STEP 1: Get NEWCODE for enumerated HH and find index location in survey data        
        BB = 0
        DO AA = 1,SIZE(HHCODE)         
          IF (PUMS_NEWTYPE(I) .EQ. HHCODE(AA)) THEN
              BB = AA
              EXIT
          ENDIF
        ENDDO
        
C-- Heither, 04-13-20418: STEP 2: Get survey category and choices based on index value
        IF (HHCAT(BB) .EQ. 1) THEN
          MAT_PUMA = MAT_PUMA + 1
        ELSEIF (HHCAT(BB) .EQ. 2) THEN
          MAT_ADJ = MAT_ADJ + 1
        ELSEIF (HHCAT(BB) .EQ. 3) THEN
          MAT_REG = MAT_REG + 1
        ELSE
          MAT_ODD = MAT_ODD + 1
        ENDIF
        CALL RANDOM(U)
        X = FLOOR(U * HHCHOICE(BB)) + 1 !! RANDOMLY SELECTED HHID INDEX
C        CALL RANDOM(UU)
      
C-- Heither, 04-13-20418: STEP 3: Get HHID value and find index location in survey data 
        DO XY = 1,2
          IF (HHRARE(BB,X).EQ.1 .AND. UU.LT.0.75) THEN
C --   RARE HH AND RANDOM UNDER THRESHOLD SO RESAMPLE --    
              CALL RANDOM(U)
              X = FLOOR(U * HHCHOICE(BB)) + 1 !! RANDOMLY SELECT ANOTHER HHID INDEX
          ENDIF
        ENDDO
        REC = 0
C        HHPICK = HHIDS(BB,X)
        DO AA = 1,SIZE(HI_SERNO)         
          IF (HHIDS(BB,X) .EQ. HI_SERNO(AA)) THEN
              REC = AA
              EXIT
          ENDIF
        ENDDO      
 
C-- Heither, 04-13-20418: STEP 4: Determine hi income/low income worker       
        IF (HI_TRIPS(REC,1) .GT. 0) THEN
          DO TRIP = 1, HI_TRIPS(REC,1)
              CALL RANDOM(RAN_NUM)
              IF (RAN_NUM .LE. PROB_HIW) THEN
                    TTH = TTH + 1
              ELSE
                    TTL = TTL + 1  
              ENDIF
C             WRITE (16,*) RAN_NUM, PROB_HIW, TTH,TTL
          ENDDO
        ENDIF
C
C ## -- Heither, 05-17-2016: Add PopSyn HH serial number to output file --       
C ## -- Heither, 04-15-2018: change HI_SERNO(REC) type - now integer
C ## -- Heither, 04-16-2018: add match category to end of file
        WRITE (42,'(I5,I7,2I4,2I2,I8,55I3,I8,I1)') PUMS_SZ(I), 
     A    PUMS_PUMA5(I), PUMS_HHTYPE(I), PUMS_HHVTYPE(I), 
     B    PUMS_VEH(I), PUMS_ROWCOL(I), HI_SERNO(REC),
     C    HI_ADULT(REC), HI_WORKER(REC), HI_NONWORKER(REC),
     D    HI_CHILD(REC), HI_CHILD1215(REC), HI_VEH(REC),
     E    TTL, TTH, (HI_TRIPS(REC,J),J=3,49), PSSERIAL(I),
     F    HHCAT(BB)   
           
        ALL_HH = ALL_HH + 1      
      
C-- Heither, DEBUGGING        
C        IF(I==1) THEN
C              WRITE (16,'(A,I10)') 'ENUMER. HH HHCODE(1)= ', 
C     A PUMS_NEWTYPE(I)
C              WRITE (16,'(A,I10)') 'SURVEY HH HHCODE(BB)= ', HHCODE(BB)
C              WRITE (16,'(A,I10)') 'SURVEY HH HHCODE(1)= ', HHCODE(I)
C              WRITE (16,'(A,I2)') 'SURVEY CATEGORY HHCAT= ', HHCAT(BB)
C              WRITE (16,'(A,2I4,I10)') 'SURVEY CHOICES,SELECTED,PICK= ',
C     A HHCHOICE(BB), X, HHIDS(BB,X)
C              WRITE (16,'(A,I5)') 'BB= ', BB
C              WRITE (16,'(A,I5)') 'REC= ', REC
C              WRITE (16,'(A,I10)') 'SURVEY FILE HHID= ', HI_SERNO(REC)
C              WRITE (16,'(A,2I10)') 'TTL AND TTH= ', TTL, TTH
C        ENDIF 
C        IF((I.GT.100).AND.(I.LT.200)) THEN
C              WRITE (16,'(A,3I3,I4)') 'I,MATCHCAT,HHVTYPE,SELECTED INDEX
C     A= ', I, HHCAT(BB), PUMS_HHVTYPE(I), X
C        ENDIF 	 
C
C     ENDDO FOR ENUMERATED HOUSEHOLDS
C
   28 CONTINUE   
      ENDDO
      
   29 CONTINUE         
      
      WRITE (*,'(/A,I9)')  'TOTAL TG HOUSEHOLD RECORDS OUTPUT= ',ALL_HH 
      WRITE (16,'(/A,I9)') 'TOTAL TG HOUSEHOLD RECORDS OUTPUT= ',ALL_HH 
C
      WRITE (*,'(/A,I9)')  'TG HOUSEHOLD RECORDS PUMA MATCHED= ',
     A  MAT_PUMA 
      WRITE (16,'(/A,I9)') 'TG HOUSEHOLD RECORDS PUMA MATCHED= ',
     A  MAT_PUMA 
C
      WRITE (*,'(/A,I9)') 'TG HOUSEHOLD RECORDS IN ADJACENT PUMA= ',
     A  MAT_ADJ 
      WRITE (16,'(/A,I9)') 'TG HOUSEHOLD RECORDS IN ADJACENT PUMA= ',
     A  MAT_ADJ 
C      
      WRITE (*,'(/A,I9)')  'TG HOUSEHOLD RECORDS REGIONALLY MATCHED= ',
     A  MAT_REG
      WRITE (16,'(/A,I9)') 'TG HOUSEHOLD RECORDS REGIONALLY MATCHED= ',
     A  MAT_REG
C
      WRITE (*,'(/A,I9)')  'TG HOUSEHOLD RECORDS ODD HH MATCHED= ',
     A  MAT_ODD
      WRITE (16,'(/A,I9)') 'TG HOUSEHOLD RECORDS ODD HH MATCHED= ',
     A  MAT_ODD
      
      NOT_MAT = PUMS_HH_IN-ALL_HH
C
      WRITE (*,'(/A,I9)')  'TG HOUSEHOLD RECORDS NOT MATCHED= ',
     A  NOT_MAT
      WRITE (16,'(/A,I9)') 'TG HOUSEHOLD RECORDS NOT MATCHED= ',
     A  NOT_MAT
      
      WRITE (16,'(/A)') 'END OF HI_HHENUM'
      WRITE (*,'(/A)') 'END OF HI_HHENUM'
            
      RETURN
      
  941 WRITE (*,'(/A)')  'ERROR:  PROBLEM OPENING FILE HI_HHENUM_IN.TXT'
      STOP 941
  942 WRITE (*,'(/A)')  'ERROR:  PROBLEM OPENING FILE HI_HHENUM_TRIP_OUT
     A.TXT'
      STOP 942
  943 WRITE (*,'(/A)')  'ERROR:  PROBLEM OPENING FILE HHID_choices1.csv'
      STOP 943     
  944 WRITE (*,'(/A)')  'ERROR:  PROBLEM OPENING FILE HHID_choices2.csv'
      STOP 944    
      
      END