      SUBROUTINE SUB_HHVEH
      IMPLICIT INTEGER(A-Z)
C***********************************************************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C***********************************************************************
C
C     THIS SUBROUTINE WAS REVISED TO ESTIMATE VEHICLE OWNERSHIP FOR THE
C     PUMS ENUMERATED HOUSEHOLDS.  THESE ARE THE INTEGER HOUSEHOLDS IN 
C     THE MATRIX ISZ_MATRIX(SZ,HHT).
C
C     AFTER THE VEHICLE OWNERSHIP MODEL IS APPLIED THE HOUSEHOLD TYPE 
C     INDEX IS REORGANIZED BY REPLACING THE INCOME INDEX WITH THE NUMBER
C     OF VEHICLES AVAILABLE IN THE HOUSEHOLD PLUS ONE.  THE VEHICLE 
C     OWNERSHIP MODEL IS UNCHANGED EXCEPT FOR ADDED BIAS CONSTANTS FOR
C     THE AGE OF HOUSEHOLDER.
C
C     THE LARGE ARRAYS NEEDED FRO TRIP GENERATION ARE FILLED IN THIS
C     SUBROUTINE. 
C
C     EASH, APRIL 2012
C
C***********************************************************************
C
C     REVISIONS IN THIS SUBROUTINE FOR THE FALL 2008 UNDATING OF THE 
C     CMAP TRIP GENERATION INCORPORATING THE 2007-2008 CMAP HOUSEHOLD
C     TRAVEL SURVEY.
C
C     1. CODE CHANGED TO WORK WITH EITHER BASE OR PUMA5 HOUSEHOLD FILE
C     2. MODEL RECALIBRATED WITH MINOR CHANGES IN STRUCTURE AND MODEL
C        COEFFICIENTS
C
C     EASH, NOVEMBER 2008
C
C     HISTORY
C***********************************************************************
C
C     THIS IS THE THIRD PROGRAM IN THE REVISED HOUSEHOLD TRIP
C     GENERATION.  IT APPLIES THE HOUSHEHOLD VEHICLE OWNTERSHIP 
C     MODEL TO THE DISSAGGREGATE HOUSEHOLD FILE CREATED BY THE 
C     PREVIOUS TWO PROGRAMS.  THE OUTPUT FILE CONTAINS THE 
C     HOUSEHOLD CATEGORIES NEEDED TO APPLY THE TRIP GENERATION
C     RATES
C
C     THIS VERSION IS FOR SUN FORTRAN. 
C
C     EASH, AUGUST 1995
C
C***********************************************************************
C
C     REVISIONS FOR PRAIRIE PARKWAY TRIP GENERATION IN THIS VERSION.
C     1.  CHANGES IN INPUT-OUTPUT FILE NAMES AND FORMATS
C     2.  DESIGNED FOR SOCIOECONOMIC ZONES PREPARED BY SUHAIL ALCHALABI
C         FOR PRAIRIE PARKWAY (COMBINATION OF TG SUBZONES AND 
C         SECTIONS.
C     3.  HOUSEHOLD VEHICLE OWNERSHIP MODEL CHANGED TO NEW MODEL 
C         CALIBRATION COMPLETED FOR DISAGGREGATE MODE AND DESTINATION
C         CHOICES MODEL.
C     
C     EASH, JUNE 2003
C
C***********************************************************************
C
C     REVISIONS IN STRUCTURE OF UTILITY CALCULATIONS TO GET AROUND 
C     PROBLEM WITH ABS FUNCTION WHEN ARGUMENT IS LOGICAL.
C
C     EASH, AUGUST 2004
C
C***********************************************************************
C
C     REVISED FOR I-290 HOV WORK IN NOVEMBER 2006.  CHANGED TO 
C     ACCOMODATE NEW TEMPORARY FILE FORMATS.
C
C     EASH, NOVEMBER 2006
C
C***********************************************************************
C
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
C
C     REVISED AGAIN FOR I-HOV WORK IN OCTOBER 2009 TO OUTPUT FILE OF 
C     HOUSEHOLD CHARACTERISTICS FOR MODE CHOICE.
C
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
      INCLUDE 'COMMON_GEOG.FI'
	INCLUDE 'COMMON_PARAM.FI'
      INCLUDE 'COMMON_CONTROL.FI'
      INCLUDE 'COMMON_HHSUMS.FI'
      INCLUDE 'COMMON_PUMSHHS.FI'
      INCLUDE 'COMMON_HHMISC.FI'
      INCLUDE 'COMMON_POPSYN.FI'      

	REAL*8 UTIL0V, UTIL1V, UTIL2V, UTIL3V
      REAL*8 P0VEH, P1VEH, P2VEH, P3VEH
      REAL*4 PED_FACTOR, AUTOMS
	REAL*8 DENOM
C
      REAL*4 COEF_PEF, COEF_WORKER, COEF_HINC2, COEF_HINC3, COEF_AUTOMS,
     A  BIAS1, COEF_HINC4, BIAS2
      
      REAL*4  HHOLDER_BIAS
 
      REAL*4 COEF_WORK1, COEF_WORK2, COEF_CHILD, BIAS3

	REAL*4 COEF_WORK3, COEF_ADULT
C
	REAL*4 ROW_FRAC(5)
	INTEGER*4 REP_HH(624,4), TOT_REP_HH(624), SUM_REP_HH(5)
           
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
C
C     FOLLOWING ARRAYS WERE ADDED FOR THE FILE TO BE INPUT TO THE
C     I290 HOV MODE CHOICE MODEL.  
C
C       LOWW_AUTO(3000,3) IS THE PROBABILITY THAT A LOW EARNING WORKER
C       IS FROM A HOUSEHOLD WITH ONE OF THREE AUTO OWNERSHIP LEVELS
C
C       HIGHW_AUTO(3000,3) IS THE PROBABILITY THAT A HIGH EARNING WORKER
C       IS FROM A HOUSEHOLD WITH ONE OF THREE AUTO OWNERSHIP LEVELS
C
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
      REAL*8 LOWW_AUTO(3000,3), HIGHW_AUTO(3000,3)
	REAL*8 SUMLW_AUTO, SUMHW_AUTO
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
C ########## Heither, 09-03-2014: change file name to MCHW_HH.TXT 
C ########## for consistency with Mode Choice model.
      IF (MODE_CHOICE) OPEN (UNIT=71,FILE='MCHW_HH.TXT',
     A  STATUS='NEW',ERR=934)
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
C
C     START LOOPING, READ A TG SUBZONE RECORD FROM EITHER BASE OR 
C     PUMA5 TEMPORARY HOUSEHOLD TRIP GENERATION FILE
C
      WRITE (*,'(/A)')'START OF VEH OWNERSHIP SUBZONE LOOP'
      WRITE (16,'(/A/)')'START OF VEH OWNERSHIP SUBZONE LOOP'

      TOTAL_HH_IN = 0.0
	TOTAL_HH_OUT = 0.0

      COUNT = 0
C
C     DETERMINE HOUSEHOLD CHARACTERISTICS
C      
      DO SZ = 1,SUBZONES
          
        WRITE (*,'(A,I6)') ' TG SUBZONE= ', SZ
        WRITE (16,'(A,I6)') ' TG SUBZONE= ', SZ  
          
        DO HHT = 1,624    
            
          HH_SIZE = HHT_ADULT(HHT) 
          WORKER_HH = HHT_WORKER(HHT)
          NONW_HH = HH_SIZE - WORKER_HH
          CHILD_HH = HHT_CHILD(HHT) 
          INCOME_HH = HHT_INC4(HHT)
          HHOLDER = HHT_HHOLDER(HHT)
          
          IF (CHILD_HH .GT. 0) CHILD_HH = 1 
          PED_FACTOR = MIN(SZ_PEF(SZ),40.0)
            
          IF (ISZ_MATRIX(SZ,HHT) .GT. 0) THEN
C
C     LOOP THROUGH ALL HOUSEHOLDS
C
            DO HH = 1,ISZ_MATRIX(SZ,HHT)  
                
              TOTAL_HH_IN = TOTAL_HH_IN + 1  
                
              COUNT = COUNT + 1 
              
              PUMS_SZ(COUNT) = SZ
              PUMS_PUMA5(COUNT) = SZ_PUMA5(SZ)
              PUMS_HHTYPE(COUNT) = HHT 
              PUMS_ROWCOL(COUNT) = SZ_ROWCOL(SZ)
                   
	        UTIL0V = 0.0
	        UTIL1V = 0.0
              UTIL2V = 0.0
	        UTIL3V = 0.0
			
C ## -- Heither, 04-05-2018: Use SYTHN_VEH flag to determine if 
C ## --         HH vehicles will use the availability model or 
C ## --         synthetic household value.			
				IF (SYNTH_VEH) THEN
C     ALLOCATE HOUSEHOLDS TO VEHICLE OWNERSHIP LEVELS
C ## -- Heither, 05-16-2016: Use PopSyn values for vehicle --
C ## --  ownership levels - limit actual vehicles to values --
C ## --  consistent with index values (0/1/2/3). --
					PUMS_VEH(COUNT) = MIN0(PSVEH(COUNT),3)
C ## --------------------------------------------------------------------------------					
				ELSE
C     START LOOPING, READ A TG SUBZONE RECORD FROM EITHER BASE OR 
C     PUMA5 TEMPORARY HOUSEHOLD TRIP GENERATION FILE
C***********************************************************************
C     VEHICLE OWNERSHIP UTILITIES FOR ONE ADULT HOUSEHOLD
C***********************************************************************					
					IF (HH_SIZE .EQ. 1) THEN
C     NO VEHICLE UTILITY
						COEF_PEF = 0.06165
						UTIL0V = COEF_PEF * PED_FACTOR
C****
C      WRITE (16,*) UTIL0V, COEF_PEF, PED_FACTOR, HHOLDER_BIAS
C****
C     ONE VEHICLE UTILITY
						COEF_WORKER = 0.4731
						COEF_HINC2 = 1.182
						COEF_HINC3 = 0.9910
						COEF_AUTOMS = 4.677
						COEF_PEF = 0.03188
						IF (SZ_ROWCOL(SZ) .EQ. 1) BIAS1 = -2.600
						IF (SZ_ROWCOL(SZ) .EQ. 2) BIAS1 = -2.676
						IF (SZ_ROWCOL(SZ) .EQ. 3) BIAS1 = -2.869
						IF (SZ_ROWCOL(SZ) .EQ. 4) BIAS1 = -3.082
						IF (HHOLDER .EQ. 1) HHOLDER_BIAS = 0.392
						IF (HHOLDER .EQ. 2) HHOLDER_BIAS = 0.401
						IF (HHOLDER .EQ. 3) HHOLDER_BIAS = 0.249

						TEMP1 = 0
						TEMP2 = 0
						TEMP3 = 0
						IF (WORKER_HH .EQ. 1) TEMP1 = 1
						IF (INCOME_HH .GE. 2) TEMP2 = 1
						IF (INCOME_HH .GE. 3) TEMP3 = 1
						UTIL1V = COEF_WORKER*TEMP1 +
     A                   COEF_HINC2*TEMP2 +
     B                   COEF_HINC3*TEMP3 +         
     C                   COEF_AUTOMS*SZ_AUTOMS(SZ) +
     D                   COEF_PEF * PED_FACTOR +   
     E                   BIAS1 +
     F                   HHOLDER_BIAS          
C****
C      WRITE (16,*) UTIL1V, COEF_WORKER, TEMP1, COEF_HINC2, TEMP2, 
C     A  COEF_HINC3, TEMP3, COEF_AUTOMS, SZ_AUTOMS(SZ), COEF_PEF,
C     B  PED_FACTOR, BIAS1, HHOLDER_BIAS
C****
C
C     TWO OR MORE VEHICLE UTILITY
						COEF_WORKER = 0.4731
						COEF_HINC2 = 1.766
						COEF_HINC3 = 1.690
						COEF_HINC4 = 0.4668
						COEF_AUTOMS = 4.677
						IF (SZ_ROWCOL(SZ) .EQ. 1) BIAS2 = -5.077
						IF (SZ_ROWCOL(SZ) .EQ. 2) BIAS2 = -4.823
						IF (SZ_ROWCOL(SZ) .EQ. 3) BIAS2 = -4.914
						IF (SZ_ROWCOL(SZ) .EQ. 4) BIAS2 = -4.984
						IF (HHOLDER .EQ. 1) HHOLDER_BIAS = 0.394
						IF (HHOLDER .EQ. 2) HHOLDER_BIAS = 0.465
						IF (HHOLDER .EQ. 3) HHOLDER_BIAS = 0.218

						TEMP1 = 0
						TEMP2 = 0
						TEMP3 = 0
						TEMP4 = 0
						IF (WORKER_HH .EQ. 1) TEMP1 = 1
						IF (INCOME_HH .GE. 2) TEMP2 = 1
						IF (INCOME_HH .GE. 3) TEMP3 = 1
						IF (INCOME_HH .EQ. 4) TEMP4 = 1
	
						UTIL2V = COEF_WORKER*TEMP1 +
     A                   COEF_HINC2*TEMP2 +
     B                   COEF_HINC3*TEMP3 +         
     C                   COEF_HINC4*TEMP4 +         
     D                   COEF_AUTOMS*SZ_AUTOMS(SZ) +
     E	               BIAS2 +
     F                   HHOLDER_BIAS     
C****
C      WRITE (16,*) UTIL2V, COEF_WORKER, TEMP1, COEF_HINC2, TEMP2,
C     A  COEF_HINC3, TEMP3, COEF_HINC4, TEMP4, COEF_AUTOMS, 
C     B  SZ_AUTOMS(SZ), BIAS2, HHOLDER_BIAS    
C****
C
C     VEHICLE OWNERSHIP PROBABILITIES
						DENOM = EXP(UTIL0V)+EXP(UTIL1V)+EXP(UTIL2V)
						P0VEH = EXP(UTIL0V)/DENOM
						P1VEH = EXP(UTIL1V)/DENOM + P0VEH
						P2VEH = 1.000
						P3VEH = 1.000
					ENDIF
C***********************************************************************
C     VEHICLE OWNERSHIP UTILITIES FOR TWO ADULT HOUSEHOLD
C***********************************************************************
					IF (HH_SIZE .EQ. 2) THEN 
C     NO VEHICLE UTILITY
						COEF_PEF = 0.1280
						UTIL0V = COEF_PEF * PED_FACTOR
C****
C      WRITE (16,*) UTIL0V, COEF_PEF, PED_FACTOR, HHOLDER_BIAS    
C****
C     ONE VEHICLE UTILITY
						COEF_HINC2 = 1.702
						COEF_PEF = 0.06309
						IF (SZ_ROWCOL(SZ) .EQ. 1) BIAS1 = 2.018
						IF (SZ_ROWCOL(SZ) .EQ. 2) BIAS1 = 2.259
						IF (SZ_ROWCOL(SZ) .EQ. 3) BIAS1 = 2.151
						IF (SZ_ROWCOL(SZ) .EQ. 4) BIAS1 = 1.925
						IF (HHOLDER .EQ. 1) HHOLDER_BIAS = 0.392
						IF (HHOLDER .EQ. 2) HHOLDER_BIAS = 0.401
						IF (HHOLDER .EQ. 3) HHOLDER_BIAS = 0.249
						TEMP1 = 0

						IF (INCOME_HH .GE. 2) TEMP1 = 1

						UTIL1V = COEF_HINC2*TEMP1 +
     A                   COEF_PEF * PED_FACTOR +   
     B                   BIAS1 +
     C                   HHOLDER_BIAS            
C****
C      WRITE (16,*) UTIL1V, COEF_HINC2, TEMP1, COEF_PEF, PED_FACTOR,
C     A  BIAS1, HHOLDER_BIAS    
C****
C     TWO VEHICLE UTILITY
						COEF_WORK1 = 0.6940
						COEF_WORK2 = 0.5198
						COEF_HINC2 = 2.466
						COEF_HINC3 = 0.8650
						COEF_HINC4 = 0.4517
						COEF_AUTOMS = 5.284
						COEF_CHILD = 0.2218
						COEF_PEF = 0.03359
						IF (SZ_ROWCOL(SZ) .EQ. 1) BIAS2 = -2.827
						IF (SZ_ROWCOL(SZ) .EQ. 2) BIAS2 = -2.637
						IF (SZ_ROWCOL(SZ) .EQ. 3) BIAS2 = -2.728
						IF (SZ_ROWCOL(SZ) .EQ. 4) BIAS2 = -3.144
						IF (HHOLDER .EQ. 1) HHOLDER_BIAS = 0.394
						IF (HHOLDER .EQ. 2) HHOLDER_BIAS = 0.465
						IF (HHOLDER .EQ. 3) HHOLDER_BIAS = 0.218
                
						TEMP1 = 0
						TEMP2 = 0
						TEMP3 = 0
						TEMP4 = 0
						TEMP5 = 0
						IF (WORKER_HH .GE. 1) TEMP1 = 1
						IF (WORKER_HH .GE. 2) TEMP2 = 1
						IF (INCOME_HH .GE. 2) TEMP3 = 1
						IF (INCOME_HH .GE. 3) TEMP4 = 1
						IF (INCOME_HH .EQ. 4) TEMP5 = 1

						UTIL2V = COEF_WORK1*TEMP1 +
     A                   COEF_WORK2*TEMP2 +
     B                   COEF_HINC2*TEMP3 +
     C                   COEF_HINC3*TEMP4 +         
     D                   COEF_HINC4*TEMP5 + 
     E                   COEF_AUTOMS*SZ_AUTOMS(SZ) +
     F                   COEF_CHILD*CHILD_HH +
     G                   COEF_PEF * PED_FACTOR +   
     H                   BIAS2 +
     I                   HHOLDER_BIAS           
C****
C      WRITE (16,*) UTIL2V, COEF_WORK1, TEMP1, COEF_WORK2, TEMP2, 
C     A  COEF_HINC2, TEMP3, COEF_HINC3, TEMP4, COEF_HINC4, TEMP5,
C     B  COEF_AUTOMS, SZ_AUTOMS(SZ), COEF_CHILD, CHILD_HH, COEF_PEF,
C     C  PED_FACTOR, BIAS2, HHOLDER_BIAS     
C****
C     THREE OR MORE VEHICLE UTILITY
						COEF_WORK1 = 0.6940
						COEF_WORK2 = 0.5198
						COEF_HINC2 = 2.466
						COEF_HINC3 = 0.8650
						COEF_HINC4 = 0.8827
						COEF_AUTOMS = 5.284
						IF (SZ_ROWCOL(SZ) .EQ. 1) BIAS3 = -4.393
						IF (SZ_ROWCOL(SZ) .EQ. 2) BIAS3 = -3.944
						IF (SZ_ROWCOL(SZ) .EQ. 3) BIAS3 = -4.126
						IF (SZ_ROWCOL(SZ) .EQ. 4) BIAS3 = -4.302
						IF (HHOLDER .EQ. 1) HHOLDER_BIAS = 0.403
						IF (HHOLDER .EQ. 2) HHOLDER_BIAS = 0.574
						IF (HHOLDER .EQ. 3) HHOLDER_BIAS = 0.007

						TEMP1 = 0
						TEMP2 = 0
						TEMP3 = 0
						TEMP4 = 0
						TEMP5 = 0
						IF (WORKER_HH .GE. 1) TEMP1 = 1
						IF (WORKER_HH .GE. 2) TEMP2 = 1
						IF (INCOME_HH .GE. 2) TEMP3 = 1
						IF (INCOME_HH .GE. 3) TEMP4 = 1
						IF (INCOME_HH .EQ. 4) TEMP5 = 1
	   					UTIL3V = COEF_WORK1*TEMP1 +
     A                   COEF_WORK2*TEMP2 +
     B                   COEF_HINC2*TEMP3 +
     C                   COEF_HINC3*TEMP4 +         
     D                   COEF_HINC4*TEMP5 + 
     E                   COEF_AUTOMS*SZ_AUTOMS(SZ) +
     F                   BIAS3 +
     G                   HHOLDER_BIAS           
C****
C      WRITE (16,*) UTIL3V, COEF_WORK1, TEMP1, COEF_WORK2, TEMP2, 
C     A  COEF_HINC2, TEMP3, COEF_HINC3, TEMP4, COEF_HINC4, TEMP5,
C     B  COEF_AUTOMS, SZ_AUTOMS(SZ), BIAS3, HHOLDER_BIAS    
C****
C     VEHICLE OWNERSHIP PROBABILITIES
						DENOM = EXP(UTIL0V)+EXP(UTIL1V)+EXP(UTIL2V)+EXP(UTIL3V)
						P0VEH = EXP(UTIL0V)/DENOM
						P1VEH = EXP(UTIL1V)/DENOM + P0VEH 
						P2VEH = EXP(UTIL2V)/DENOM + P1VEH
						P3VEH = 1.000
					ENDIF
C***********************************************************************
C     VEHICLE OWNERSHIP UTILITIES FOR THREE OR MORE ADULT HOUSEHOLD
C***********************************************************************
					IF (HH_SIZE .GT. 2) THEN
C     NO VEHICLE UTILITY
						COEF_PEF = 0.1703
						UTIL0V = COEF_PEF * PED_FACTOR
C****
C      WRITE (16,*) UTIL0V, COEF_PEF, PED_FACTOR, HHOLDER_BIAS    
C****
C     ONE VEHICLE UTILITY
						COEF_WORK1 = 1.114
						COEF_HINC2 = 0.9492
						COEF_PEF = 0.06586
						IF (SZ_ROWCOL(SZ) .EQ. 1) BIAS1 = 2.806
						IF (SZ_ROWCOL(SZ) .EQ. 2) BIAS1 = 2.552
						IF (SZ_ROWCOL(SZ) .EQ. 3) BIAS1 = 1.547
						IF (SZ_ROWCOL(SZ) .EQ. 4) BIAS1 = 2.272
						IF (HHOLDER .EQ. 1) HHOLDER_BIAS = 0.392
						IF (HHOLDER .EQ. 2) HHOLDER_BIAS = 0.401
						IF (HHOLDER .EQ. 3) HHOLDER_BIAS = 0.249
	
						TEMP1 = 0
						TEMP2 = 0
						IF (WORKER_HH .GE. 1) TEMP1 = 1
						IF (INCOME_HH .GE. 2) TEMP2 = 1

						UTIL1V = COEF_WORK1*TEMP1 + 
     A                   COEF_HINC2*TEMP2 +
     B                   COEF_PEF * PED_FACTOR +   
     C                   BIAS1 +
     D                   HHOLDER_BIAS           
C****
C      WRITE (16,*) UTIL1V, COEF_WORK1, TEMP1, COEF_HINC2, TEMP2,
C     A  COEF_PEF, PED_FACTOR, BIAS1, HHOLDER_BIAS    
C****
C     TWO VEHICLE UTILITY
						COEF_WORK1 = 1.114
						COEF_WORK2 = 0.7934
						COEF_HINC2 = 1.487
						COEF_HINC3 = 0.8723
						COEF_HINC4 = 1.390
						COEF_AUTOMS = 4.959
						COEF_PEF = 0.06586
						IF (SZ_ROWCOL(SZ) .EQ. 1) BIAS2 = -1.836
						IF (SZ_ROWCOL(SZ) .EQ. 2) BIAS2 = -2.139
						IF (SZ_ROWCOL(SZ) .EQ. 3) BIAS2 = -2.783
						IF (SZ_ROWCOL(SZ) .EQ. 4) BIAS2 = -2.430
						IF (HHOLDER .EQ. 1) HHOLDER_BIAS = 0.394
						IF (HHOLDER .EQ. 2) HHOLDER_BIAS = 0.465
						IF (HHOLDER .EQ. 3) HHOLDER_BIAS = 0.218
	          
						TEMP1 = 0
						TEMP2 = 0
						TEMP3 = 0
						TEMP4 = 0
						TEMP5 = 0
						IF (WORKER_HH .GE. 1) TEMP1 = 1
						IF (WORKER_HH .GE. 2) TEMP2 = 1
						IF (INCOME_HH .GE. 2) TEMP3 = 1
						IF (INCOME_HH .GE. 3) TEMP4 = 1
						IF (INCOME_HH .EQ. 4) TEMP5 = 1

						UTIL2V = COEF_WORK1*TEMP1 +
     A                   COEF_WORK2*TEMP2 +
     B                   COEF_HINC2*TEMP3 +
     C                   COEF_HINC3*TEMP4 +         
     D                   COEF_HINC4*TEMP5 + 
     E                   COEF_AUTOMS*SZ_AUTOMS(SZ) +
     F                   COEF_PEF * PED_FACTOR +   
     G                   BIAS2 + 
     H                   HHOLDER_BIAS          
C****
C      WRITE (16,*) UTIL2V, COEF_WORK1, TEMP1, COEF_WORK2, TEMP2, 
C     A  COEF_HINC2, TEMP3, COEF_HINC3, TEMP4, COEF_HINC4, TEMP5,
C     B  COEF_AUTOMS, SZ_AUTOMS(SZ), COEF_PEF, PED_FACTOR, BIAS2,
C     C  HHOLDER_BIAS     
C****
C     THREE OR MORE VEHICLE UTILITY
						COEF_WORK1 = 1.114
						COEF_WORK2 = 0.7934
						COEF_WORK3 = 1.389
						COEF_HINC2 = 1.487
						COEF_HINC3 = 1.571
						COEF_HINC4 = 1.834
						COEF_ADULT = 0.1491
						COEF_AUTOMS = 4.959
						IF (SZ_ROWCOL(SZ) .EQ. 1) BIAS3 = -1.631
						IF (SZ_ROWCOL(SZ) .EQ. 2) BIAS3 = -1.789
						IF (SZ_ROWCOL(SZ) .EQ. 3) BIAS3 = -2.668
						IF (SZ_ROWCOL(SZ) .EQ. 4) BIAS3 = -2.278
						IF (HHOLDER .EQ. 1) HHOLDER_BIAS = 0.403
						IF (HHOLDER .EQ. 2) HHOLDER_BIAS = 0.574
						IF (HHOLDER .EQ. 3) HHOLDER_BIAS = 0.007

						TEMP1 = 0
						TEMP2 = 0
						TEMP3 = 0
						TEMP4 = 0
						TEMP5 = 0
						TEMP6 = 0
						IF (WORKER_HH .GE. 1) TEMP1 = 1
						IF (WORKER_HH .GE. 2) TEMP2 = 1
						IF (WORKER_HH .GE. 3) TEMP3 = 1
						IF (INCOME_HH .GE. 2) TEMP4 = 1
						IF (INCOME_HH .GE. 3) TEMP5 = 1
						IF (INCOME_HH .EQ. 4) TEMP6 = 1

						UTIL3V = COEF_WORK1*TEMP1 +
     A                   COEF_WORK2*TEMP2 +
     B                   COEF_WORK3*TEMP3 +
     C                   COEF_HINC2*TEMP4 +
     D                   COEF_HINC3*TEMP5 +         
     E                   COEF_HINC4*TEMP6 + 
     F                   COEF_ADULT*NONW_HH +         
     G                   COEF_AUTOMS*SZ_AUTOMS(SZ) +
     H                   BIAS3 +
     I                   HHOLDER_BIAS           
C****
C      WRITE (16,*) UTIL3V, COEF_WORK1, TEMP1, COEF_WORK2, TEMP2, 
C     A  COEF_WORK3, TEMP3, COEF_HINC2, TEMP4, COEF_HINC3, TEMP5, 
C     B  COEF_HINC4, TEMP6, COEF_ADULT, NONW_HH, COEF_AUTOMS, 
C     C  SZ_AUTOMS(SZ), BIAS3, HHOLDER_BIAS    
C****
C     VEHICLE OWNERSHIP PROBABILITIES
						DENOM = EXP(UTIL0V)+EXP(UTIL1V)+EXP(UTIL2V)+EXP(UTIL3V)
						P0VEH = EXP(UTIL0V)/DENOM
						P1VEH = EXP(UTIL1V)/DENOM + P0VEH
						P2VEH = EXP(UTIL2V)/DENOM + P1VEH
						P3VEH = 1.000
					ENDIF
C****
C      HEADER='HOUSEHOLD TYPE='
C      WRITE (16,*) HEADER, I
C      HEADER='INCOME QUARTILE='
C      WRITE (16,*) HEADER, J
C
C      HEADER = 'ZERO VEHICLE OWNERSHIP UTILITY AND PROB.= '
C      WRITE (16,*) HEADER, UTIL0V, P0VEH
C      HEADER = 'SINGLE VEHICLE OWNERSHIP UTILITY AND PROB.= '
C      WRITE (16,*) HEADER, UTIL1V, P1VEH
C      HEADER = 'TWO VEHICLE OWNERSHIP UTILITY AND PROB.= '
C      WRITE (16,*) HEADER, UTIL2V, P2VEH
C      HEADER = 'THREE PLUS VEHICLE OWNERSHIP UTILITY AND PROB.= '
C      WRITE (16,*) HEADER, UTIL3V, P3VEH
C
C     ALLOCATE HOUSEHOLDS TO VEHICLE OWNERSHIP LEVELS
C
C     WRITE (16,'(2I3, 4F10.4)') HH, SZ, UTIL0V, UTIL1V, UTIL2V, UTIL3V
C	WRITE (16,'(4F7.4)') P0VEH, P1VEH, P2VEH, P3VEH
C	WRITE (16,*) HH_SIZE, WORKER_HH, INCOME_HH, NONW_HH, CHILD_HH
C	WRITE (16,*) PED_FACTOR, SZ_AUTOMS(SZ), SZ_ROWCOL(SZ)
C****
					CALL RANDOM(RAN_NUM)
					IF (RAN_NUM .LE. P3VEH) PUMS_VEH(COUNT) = 3
					IF (RAN_NUM .LE. P2VEH) PUMS_VEH(COUNT) = 2
					IF (RAN_NUM .LE. P1VEH) PUMS_VEH(COUNT) = 1
					IF (RAN_NUM .LE. P0VEH) PUMS_VEH(COUNT) = 0
              
C              WRITE (16,*) RAN_NUM, P0VEH, P1VEH, P2VEH, P3VEH
C              WRITE (16,*) COUNT, PUMS_VEH(COUNT)
				ENDIF
				
            ENDDO
          ENDIF
        ENDDO
      ENDDO  
C
C     ACCUMULATE WEIGHTS FOR VEHICLE OWNERSHIP LEVELS INPUT FOR MODE
C     CHOICE
C  
      IF (MODE_CHOICE) THEN
	       
	  DO I = 1,COUNT
          SZ = PUMS_SZ(I)
          HHT = PUMS_HHTYPE(I)
           
          Z = SZ_ZONE(SZ)
          RC = PUMS_ROWCOL(I)    
          WORKER_HH = HHT_WORKER(HHT)
          VEH = PUMS_VEH(I)
          VEH = VEH + 1
          VEH = MIN0(VEH,3)
          
          HH_INCOME = HHT_INC4(HHT)
          HHT_INDEX = MOD(HHT,13)
          IF (HHT_INDEX .EQ. 0) HHT_INDEX = 13
 
	    LOWW_AUTO(Z,VEH) = LOWW_AUTO(Z,VEH) + 
     A      FLOAT(WORKER_HH) * (1.00-HHTYPE_HWINC(HHT_INDEX,HH_INCOME))
	
	    HIGHW_AUTO(Z,VEH) = HIGHW_AUTO(Z,VEH) + 
     A      FLOAT(WORKER_HH) * HHTYPE_HWINC(HHT_INDEX,HH_INCOME)
        ENDDO
C****
C        DO Z = 1,20
C          WRITE (16,*) Z, (LOWW_AUTO(Z,JJ),JJ=1,3), 
C     A      (HIGHW_AUTO(Z,JJ),JJ=1,3) 
C        ENDDO    
C****
      ENDIF 
C
C     ACCUMULATE SUMMARIES
C
      DO I = 1,COUNT
        VEH = PUMS_VEH(I) + 1
        SZ = PUMS_SZ(I)
        HHT = PUMS_HHTYPE(I)
        
        DO CO=1,COUNTIES
          IF (SZ_CO(SZ) .EQ. CO_NUM(CO)) CO_HH_VEH(CO,VEH) = 
     A      CO_HH_VEH(CO,VEH) + 1
        ENDDO
        DO P1=1,PUMA1
	    IF (SZ_PUMA1(SZ) .EQ. P1_NUM(P1)) PUMA1_HH_VEH(P1,VEH) = 
     A      PUMA1_HH_VEH(P1,VEH) + 1
        ENDDO
	  DO P5=1,PUMA5
	    IF (SZ_PUMA5(SZ) .EQ. P5_NUM(P5)) PUMA5_HH_VEH(P5,VEH) = 
     A      PUMA5_HH_VEH(P5,VEH) + 1
        ENDDO

	  TOT_VEH(VEH) = TOT_VEH(VEH) + 1

	  IF (SZ_CHI(SZ) .EQ. 1) CHI_HH_VEH(VEH) = CHI_HH_VEH(VEH) + 1
	  IF (SZ_CBD(SZ) .EQ. 1) CBD_HH_VEH(VEH) = CBD_HH_VEH(VEH) + 1

      ENDDO
C     
C     WRITE OUT MODE CHOICE ZONE LEVEL FILE OF WORKER HH AUTO OWNERSHIP
C     PROBABILITIES
C
      IF (MODE_CHOICE) THEN
	  
	  DO Z = 1,ZONES
          SUMLW_AUTO = LOWW_AUTO(Z,1)+LOWW_AUTO(Z,2)+LOWW_AUTO(Z,3)
          SUMHW_AUTO = HIGHW_AUTO(Z,1)+HIGHW_AUTO(Z,2)+HIGHW_AUTO(Z,3)
C****
C      WRITE (16,*) Z, SUMLW_AUTO, SUMHW_AUTO
C  	 WRITE (16,*) LOWW_AUTO(Z,1), LOWW_AUTO(Z,2), LOWW_AUTO(Z,3)
C      WRITE (16,*) HIGHW_AUTO(Z,1), HIGHW_AUTO(Z,2), HIGHW_AUTO(Z,3)
C****
	    DO VV=1,3
	      IF (SUMLW_AUTO .GT. 0)
     A        LOWW_AUTO(Z,VV) = LOWW_AUTO(Z,VV)/SUMLW_AUTO
            IF (SUMHW_AUTO .GT. 0) 
     A        HIGHW_AUTO(Z,VV) = HIGHW_AUTO(Z,VV)/SUMHW_AUTO
          ENDDO

          WRITE (71,'(I5,6F12.4)') Z, (LOWW_AUTO(Z,J),J=1,3),
     A	  (HIGHW_AUTO(Z,J),J=1,3)
        ENDDO
      CLOSE (71,DISP='KEEP')  
      ENDIF
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
C
C     REGIONAL HOUSEHOLD SUMMARY
C
	WRITE (16,'(/A)')  'ROW AND COLUMN TOTALS FOR HOUSEHOLDS BY VEHICL
     AES AVAILABLE'

      DO I = 1,COUNT
        HHT = PUMS_HHTYPE(I)
        VEH = PUMS_VEH(I) + 1
          
        REP_HH(HHT,VEH) = REP_HH(HHT,VEH) + 1
	  TOT_REP_HH(HHT) = TOT_REP_HH(HHT) + 1
	  SUM_REP_HH(VEH) = SUM_REP_HH(VEH) + 1
	  SUM_REP_HH(5) = SUM_REP_HH(5) + 1
      ENDDO
      
	WRITE (16,'(/A)') 'HOUSEHOLDS:  VEHICLES AVAILABLE BY HOUSEHOLD TY
     APE'
	WRITE (16,'(/A)')  ' HHTYPE    0 VEH    1 VEH    2 VEH   3+ VEH   
     A TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A------'       
      DO HHT = 1,624
	  WRITE (16,'(I7,5I9)') HHT, (REP_HH(HHT,J),J=1,4),TOT_REP_HH(HHT)
      ENDDO

	WRITE (16,'(A,5I9)') '  TOTAL', (SUM_REP_HH(J),J=1,5)
C
      DO I = 1,4
        TOT_REP_HH(I) = 0  
        DO HHT = 1,624
          REP_HH(HHT,I) = 0
        ENDDO
      ENDDO  
C      
      DO I = 1,COUNT
        RC = PUMS_ROWCOL(I)
        VEH = PUMS_VEH(I) + 1
          
        REP_HH(RC,VEH) = REP_HH(RC,VEH) + 1
	  TOT_REP_HH(RC) = TOT_REP_HH(RC) + 1
      ENDDO

      WRITE (16,'(/A)')'HOUSEHOLDS:  VEHICLES AVAILABLE BY ROW COLUMN FA
     ACTORING AREA'
	WRITE (16,'(/A)')  'ROW COL    0 VEH    1 VEH    2 VEH   3+ VEH   
     A TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A------'       
      DO RC = 1,4
	  WRITE (16,'(I7,5I9)') RC, (REP_HH(RC,J),J=1,4),TOT_REP_HH(RC)
      ENDDO

	WRITE (16,'(A,5I9)') '  TOTAL', (SUM_REP_HH(J),J=1,5)            
C
      DO I = 1,4
        TOT_REP_HH(I) = 0  
        DO HHT = 1,624
          REP_HH(HHT,I) = 0
        ENDDO
      ENDDO  
C      
      DO I = 1,COUNT
        HHT = PUMS_HHTYPE(I)
        AA = HHT_ADULT(HHT)
        VEH = PUMS_VEH(I) + 1
          
        REP_HH(AA,VEH) = REP_HH(AA,VEH) + 1
	  TOT_REP_HH(AA) = TOT_REP_HH(AA) + 1
      ENDDO

      WRITE (16,'(/A)') 'HOUSEHOLDS:  VEHICLES AVAILABLE BY ADULTS IN HO
     AUSEHOLD'
	WRITE (16,'(/A)')  ' ADULTS    0 VEH    1 VEH    2 VEH   3+ VEH   
     A TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A------'       
      DO AA = 1,4
	  WRITE (16,'(I7,5I9)') AA, (REP_HH(AA,J),J=1,4),TOT_REP_HH(AA)
      ENDDO

	WRITE (16,'(A,5I9)') '  TOTAL', (SUM_REP_HH(J),J=1,5)      
C
      DO I = 1,4
        TOT_REP_HH(I) = 0  
        DO HHT = 1,624
          REP_HH(HHT,I) = 0
        ENDDO
      ENDDO  
C
      DO I = 1,COUNT
        HHT = PUMS_HHTYPE(I)
        WW = HHT_WORKER(HHT) + 1
        VEH = PUMS_VEH(I) + 1
          
        REP_HH(WW,VEH) = REP_HH(WW,VEH) + 1
	  TOT_REP_HH(WW) = TOT_REP_HH(WW) + 1
      ENDDO

	WRITE (16,'(/A)')'HOUSEHOLDS:  VEHICLES AVAILABLE BY WORKERS IN HO
     AUSEHOLD'
	WRITE (16,'(/A)')  'WORKERS    0 VEH    1 VEH    2 VEH   3+ VEH   
     A TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A------'       
      DO WW = 1,4
        W1 =  WW-1  
        WRITE (16,'(I7,5I9)') W1, (REP_HH(WW,J),J=1,4),TOT_REP_HH(WW)
      ENDDO

	WRITE (16,'(A,5I9)') '  TOTAL', (SUM_REP_HH(J),J=1,5)      
C
      DO I = 1,4
        TOT_REP_HH(I) = 0  
        DO HHT = 1,624
          REP_HH(HHT,I) = 0
        ENDDO
      ENDDO  
C
      DO I = 1,COUNT
        HHT = PUMS_HHTYPE(I)
        II = HHT_INC4(HHT)
        VEH = PUMS_VEH(I) + 1
          
        REP_HH(II,VEH) = REP_HH(II,VEH) + 1
	  TOT_REP_HH(II) = TOT_REP_HH(II) + 1
      ENDDO
      
	WRITE (16,'(/A)')'HOUSEHOLDS:  VEHICLES AVAILABLE BY INCOME QUARTI
     ALE'
	WRITE (16,'(/A)')  'INC 1/4    0 VEH    1 VEH    2 VEH   3+ VEH   
     A TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A------'       
      DO II = 1,4
	  WRITE (16,'(I7,5I9)') II, (REP_HH(II,J),J=1,4),TOT_REP_HH(II)
      ENDDO

	WRITE (16,'(A,5I9)') '  TOTAL', (SUM_REP_HH(J),J=1,5)      
C
      DO I = 1,4
        TOT_REP_HH(I) = 0  
        DO HHT = 1,624
          REP_HH(HHT,I) = 0
        ENDDO
      ENDDO  
C
      DO I = 1,COUNT
        HHT = PUMS_HHTYPE(I)
        HH = HHT_HHOLDER(HHT)
        VEH = PUMS_VEH(I) + 1
          
        REP_HH(HH,VEH) = REP_HH(HH,VEH) + 1
	  TOT_REP_HH(HH) = TOT_REP_HH(HH) + 1
      ENDDO
      
	WRITE (16,'(/A)') 'HOUSEHOLDS:  VEHICLES AVAILABLE BY AGE OF HOUSE
     AHOLDER'
	WRITE (16,'(/A)')  ' HH AGE    0 VEH    1 VEH    2 VEH   3+ VEH   
     A TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A------'       
      DO HH = 1,3
	  WRITE (16,'(I7,5I9)') HH, (REP_HH(HH,J),J=1,4),TOT_REP_HH(HH)
      ENDDO

	WRITE (16,'(A,5I9)') '  TOTAL', (SUM_REP_HH(J),J=1,5)
C
C     PRINT ROW AND COLUMN TOTALS FOR CHECKING, COUNTIES
C
	WRITE (16,'(/A)')  'HOUSEHOLDS:  VEHICLES AVAILABLE BY COUNTY'

	WRITE (16,'(/A)')  ' COUNTY    0 VEHICLES     1 VEHICLE    2 VEHIC
     ALES   3+ VEHICLES    TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A--------------------------'       

      DO I=1,COUNTIES
	  ROW_TOT = CO_HH_VEH(I,1) + CO_HH_VEH(I,2) + 
     A    CO_HH_VEH(I,3) + CO_HH_VEH(I,4)

        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) 
     A      ROW_FRAC(J) = FLOAT(CO_HH_VEH(I,J))/FLOAT(ROW_TOT)
	  ENDDO	 

        WRITE (16,'(I7,5(I9,F5.2))') CO_NUM(I), 
     A    (CO_HH_VEH(I,J),ROW_FRAC(J),J=1,4), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_VEH(1) + TOT_VEH(2) + TOT_VEH(3) + TOT_VEH(4)

      DO J=1,4
	  ROW_FRAC(J) = FLOAT(TOT_VEH(J))/FLOAT(ROW_TOT)
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL', 
     A  (TOT_VEH(I), ROW_FRAC(I), I=1,4), ROW_TOT

	ROW_TOT = CHI_HH_VEH(1) + CHI_HH_VEH(2) + CHI_HH_VEH(3) + 
     A  CHI_HH_VEH(4)

      DO J=1,4
	  ROW_FRAC(J) = FLOAT(CHI_HH_VEH(J))/FLOAT(ROW_TOT)
	ENDDO	 
	
	WRITE (16,'(/A,5(I9,F5.2))') 'CHICAGO', 
     A  (CHI_HH_VEH(I), ROW_FRAC(I), I=1,4), ROW_TOT

	ROW_TOT = CBD_HH_VEH(1) + CBD_HH_VEH(2) + CBD_HH_VEH(3) + 
     A  CBD_HH_VEH(4)

      DO J=1,4
	  ROW_FRAC(J) = FLOAT(CBD_HH_VEH(J))/FLOAT(ROW_TOT)
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') 'CHI CBD', 
     A  (CBD_HH_VEH(I), ROW_FRAC(I), I=1,4), ROW_TOT
C
C     REPEAT FOR ONE PERCENT PUMS
C
	WRITE (16,'(/A)')  'HOUSEHOLDS:  VEHICLES AVAILABLE BY ONE PERCENT 
     A PUMAS'

	WRITE (16,'(/A)')  '  PUMA1    0 VEHICLES     1 VEHICLE    2 VEHIC
     ALES   3+ VEHICLES    TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A--------------------------'       

      DO I=1,PUMA1
	  ROW_TOT = PUMA1_HH_VEH(I,1) + PUMA1_HH_VEH(I,2) + 
     A    PUMA1_HH_VEH(I,3) + PUMA1_HH_VEH(I,4)

        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) 
     A      ROW_FRAC(J) = FLOAT(PUMA1_HH_VEH(I,J))/FLOAT(ROW_TOT)
	  ENDDO	 

        WRITE (16,'(I7,5(I9,F5.2))') P1_NUM(I), 
     A    (PUMA1_HH_VEH(I,J),ROW_FRAC(J),J=1,4), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_VEH(1) + TOT_VEH(2) + TOT_VEH(3) + TOT_VEH(4)

      DO J=1,4
	  ROW_FRAC(J) = FLOAT(TOT_VEH(J))/FLOAT(ROW_TOT)
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL', 
     A  (TOT_VEH(I), ROW_FRAC(I), I=1,4), ROW_TOT
C
C     REPEAT FOR FIVE PERCENT PUMS
C
	WRITE (16,'(/A)')  'HOUSEHOLDS:  VEHICLES AVAILABLE BY FIVE PERCEN
     AT PUMAS'

	WRITE (16,'(/A)')  '  PUMA5  PUMA1    0 VEHICLES     1 VEHICLE    
     A2 VEHICLES   3+ VEHICLES    TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'       

      DO I=1,PUMA5
	  ROW_TOT = PUMA5_HH_VEH(I,1) + PUMA5_HH_VEH(I,2) + 
     A    PUMA5_HH_VEH(I,3) + PUMA5_HH_VEH(I,4)

        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) 
     A      ROW_FRAC(J) = FLOAT(PUMA5_HH_VEH(I,J))/FLOAT(ROW_TOT)
	  ENDDO	 

        WRITE (16,'(2I7,5(I9,F5.2))') P5_NUM(I), P5_PUMA1(I), 
     B    (PUMA5_HH_VEH(I,J),ROW_FRAC(J),J=1,4), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_VEH(1) + TOT_VEH(2) + TOT_VEH(3) + TOT_VEH(4)

      DO J=1,4
	  ROW_FRAC(J) = FLOAT(TOT_VEH(J))/FLOAT(ROW_TOT)
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL       ', 
     A  (TOT_VEH(I), ROW_FRAC(I), I=1,4), ROW_TOT
C
	WRITE (16,'(/A,I9)') ' TOTAL HOUSEHOLDS PROCESSED=  ', 
     A  TOTAL_HH_IN

      WRITE (16,'(/A)') 'END OF HHVEH'
      WRITE (*,'(/A)') 'END OF HHVEH'

      RETURN
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
  934 WRITE (*,'(/A)')  'ERROR:  PROBLEM OPENING FILE HWMC_VEH_OUT.TXT'
      STOP 934
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
      END