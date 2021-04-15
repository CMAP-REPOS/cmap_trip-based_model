      SUBROUTINE SUB_TRIPGEN8
      IMPLICIT INTEGER(A-Z)
C***********************************************************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C***********************************************************************
C
C     THIS IS THE SEVENTH PROGRAM IN THE REVISED HOUSEHOLD TRIP
C     GENERATION PREPARED FOR THE PRAIRIE PARKWAY PROJECT.  THIS MODULE
C     ADJUSTS THE INITIAL TRIP PRODUCTIONS AND ATTRACTIONS TO ACCOUNT 
C     FOR TRIPS FROM OR TO EXTERNAL AREAS AND NONMOTORIZED TRAVEL.  THE
C     EXTERNAL TRIP FACTORS WERE DEVELOPED FROM THE CENSUS 2000 JOURNEY
C     TO WORK COUNTY FLOWS.  NONMOTORIZED TRIP FACTORS WERE COMPUTED
C     FROM THE 1995 NPTS AND SMOOTHED THROUGH REGRESSION.
C
C     EASH, SEPTEMBER 2003
C
C***********************************************************************
C
C     REVISED FOR I-290 HOV WORK IN NOVEMBER 2006.
C
C     1.  ACCOMODATES NEW TEMPORARY FILE FORMATS AND PUMS SUMMARIES.
C     2.  INCLUDES INPUT NONMOTORIZED FRACTIONS FOR WORK PRODUCTIONS 
C         AND ATTRACTIONS BY PUMA5 AREA.  THERE WERE ESTIMATED FROM
C         2000 CENSUS CTPP FILES.
C     3.  INCLUDES NEW INTERNAL-EXTERNAL FRACTIONS FOR PUMA5 
C         AREAS, ALSO DEVELOPED FROM 2000 CENSUS CTPP.  .
C
C     EXTERNAL TRIP PRODUCTIONS ARE TAKEN OUT FIRST.  FOR HOME-WORK 
C     TRIPS THE PROPORTION OF HOME-WORK EXTERNAL TRIP PRODUCTIONS IS 
C     DETERMINED BY THE PUMA5 "EXTERNAL" INPUT FILE.  AFTER TAKING OUT 
C     HOME-WORK PRODUCTIONS AN EQUAL NUMBER OF HOME-WORK ATTRACTIONS ARE 
C     ELIMINATED.  THE ATTRACTIONS ARE SUBTRACTED USING THE PUMA5 
C     ORIGIN-DESTINATION PROPORTIONS IN THE PUMA5 INPUT FILE.  NO 
C     EXTERNAL ADJUSTMENTS ARE ATTEMPTED FOR OTHER (SHORTER) TRIP 
C     PURPOSES.  THESE ADJUSTMENTS ARE THEN DISTRIBUTED PROPORTIONALLY
C     TO THE TAZS IN THE PUMA5.
C
C     NONMOTORIZED TRIP PRODUCTIONS ARE THEN FACTORED OUT.  FOR 
C     HOME-WORK TRIPS THE FRACTION OF NONMOTORIZED TRIPS BY PUMA5 IS
C     INPUT IN THE "NONMOTOR" INPUT FILE.  NOTE THAT THIS CALCULATIONS 
C     ONLY INVOLVES TRIPS THAT ARE INTERNAL IN THE STUDY AREA SINCE
C     EXTERNAL HOME-WORK TRIPS ARE PREVIOUSLY TAKEN OUT.  THE TRIP
C     REDUCTIONS ARE DISTRIBUTED AROUND THE TG ZONES IN THE HOME PUMA
C     USING THE 1995 NHTS RELATIONSHIPS.  
C
C     FOR ALL OTHER TRIP PURPOSES THE NONMOTORIZED TRIP REDUCTIONS ARE
C     BASED SOLELY ON THE 1995 NHTS RELATIONSHIPS.
C     
C     EASH, NOVEMBER 2006
C
C***********************************************************************
C
C     THIS IS A NEW SUBROUTINE TO ESTIMATE NONMOTORIZED TRIPS.  IT 
C     FEATURES NEW LOGIT NONMOTORIZED TRIP ESTIMATION MODELS BASED ON 
C     THE 2007-2008 CMAP HOUSEHOLD TRAVEL SURVEY.  OTHER DOCUMENTATION
C     IN BODY OF SUBROUTINE  
C
C     EASH FEBRUARY 2009
C
C***********************************************************************
C
C     MOTORIZED BIAS CONSTANTS ADJUSTED TO ACCOUNT FOR CENSUS 2010 
C     ADDITIONAL BLOCK CODING.  ALSO, NONMOTORIZED CALCULATIONS ARE NOW
C     SEPARATED BY CBD VERSUS NONCBD FOR WORKERS AND NONWORKERS.
C
C     EASH MARCH 2011
C
C***********************************************************************
C
C     CALCULATIONS ADJUSTED TO ACCOUNT FOR THE EARLIER REMOVAL OF 
C     GROUP QUARTERS NONMOTORIZED TRIPS.
C
C     EASH JUNE 2012
C
C***********************************************************************
	INCLUDE 'COMMON_PARAM.FI'
	INCLUDE 'COMMON_NM.FI'
	INCLUDE 'COMMON_GEOG.FI'
	INCLUDE 'COMMON_ATTR.FI'
	INCLUDE 'COMMON_HHSUMS.FI'
	INCLUDE 'COMMON_GQ.FI'

	REAL*8  NM_UTIL(4), M_UTIL(4), NM_FACTOR(4)
	REAL*8  TEMPA
	REAL*4  FRACP, FRACA, FRACGQP, FRACGQA, PED_FACTOR
	REAL*4  ZERO/0.000/

	REAL*8  SZ_TPRODS(49), SZ_TATTRS(49), GQ_TPRODS(33), GQ_TATTRS(33) 
      
C ## -- Heither, 05-24-2016: WRITE OUT FILE TO VERIFY NONMOTORIZED CALCULATIONS 
      OPEN (UNIT=80,FILE='NONMOTOR_REVIEW_OUT.TXT',STATUS='NEW',ERR=959)
C
C     NONMOTORIZED PRODUCTIONS AND ATTRACTIONS FOR HOUSEHOLDS
C
	DO SZ=1,SUBZONES
C
C     PEF LIMITED TO MAXIMUM OF 40
C
	PED_FACTOR = MIN(SZ_PEF(SZ),40.0)
C
C     START WITH WORKERS TRIPS (PRODS(1-20))
C
C       NM_FACTOR(1) = FROM HOME TO WORKPLACE OR WORK RELATED
C       NM_FACTOR(2) = FROM HOME TO NONWORK
C       NM_FACTOR(3) = FROM WORK
C       NM_FACTOR(4) = FROM NONHOME/WORK
C
        IF (SZ_HH(SZ) .GT. 0) THEN
          NM_UTIL(1) = 0.7994E-04*(SZ_TOTEMP(SZ)/SZ_AREA(SZ)) +
     A      0.1289E-01*PED_FACTOR +
     B      2.598 *(SZ_HH_NOVEH(SZ)/SZ_HH(SZ))

          IF (SZ_CBD(SZ) .EQ. 1) THEN 
		  M_UTIL(1) = 4.133
          ELSE 
	      M_UTIL(1) = 4.363
          ENDIF
        ELSE
	    NM_UTIL(1) = 0.0
	    M_UTIL(1) = 0.0
	  ENDIF
C
        IF (SZ_HH(SZ) .GT. 0) THEN
          NM_UTIL(2) = 0.2268E-05*(SZ_TOTEMP(SZ)/SZ_AREA(SZ)) +
     A      0.7235E-01*PED_FACTOR +
     B      2.398 *(SZ_HH_NOVEH(SZ)/SZ_HH(SZ))

          IF (SZ_CBD(SZ) .EQ. 1) THEN 
            M_UTIL(2) = 4.283
          ELSE
            M_UTIL(2) = 4.901
          ENDIF
        ELSE
	    NM_UTIL(2) = 0.0
	    M_UTIL(2) = 0.0
	  ENDIF
C
        NM_UTIL(3) = 0.8931E-05*(SZ_TOTEMP(SZ)/SZ_AREA(SZ)) +
     A    0.8500E-01*PED_FACTOR

        NM_UTIL(4) = 0.1380E-03*(SZ_HH(SZ)/SZ_AREA(SZ)) +
     A    0.8166E-05*(SZ_TOTEMP(SZ)/SZ_AREA(SZ)) +
     B    0.3803E-01*PED_FACTOR
         
	  IF (SZ_CBD(SZ) .EQ. 1) THEN 
          M_UTIL(3) = 4.504
          M_UTIL(4) = 3.889
        ELSE
	    M_UTIL(3) = 4.855
          M_UTIL(4) = 3.965
        ENDIF
C
        DO I=1,4
	    NM_FACTOR(I) = 
     A      EXP(NM_UTIL(I))/(EXP(M_UTIL(I))+EXP(NM_UTIL(I)))
C          WRITE (16, '(2I6,4F12.6)') SZ, I, NM_FACTOR(I), NM_UTIL(I),
C     A      M_UTIL(I), NM_UTIL(I)
        ENDDO
C
C     LIMIT FACTORS TO MAX OBSERVED LEVELS 
C
	  IF (SZ_CBD(SZ) .EQ. 1) THEN 
          IF (NM_FACTOR(1) .GT. 0.60) NM_FACTOR(1) = 0.60
          IF (NM_FACTOR(2) .GT. 0.60) NM_FACTOR(2) = 0.60
          IF (NM_FACTOR(3) .GT. 0.75) NM_FACTOR(3) = 0.75
          IF (NM_FACTOR(4) .GT. 0.75) NM_FACTOR(4) = 0.75
        ELSE
          IF (NM_FACTOR(1) .GT. 0.40) NM_FACTOR(1) = 0.40
          IF (NM_FACTOR(2) .GT. 0.50) NM_FACTOR(2) = 0.50
          IF (NM_FACTOR(3) .GT. 0.50) NM_FACTOR(3) = 0.50
          IF (NM_FACTOR(4) .GT. 0.60) NM_FACTOR(4) = 0.60
        ENDIF
C
C     FROM HOME TO WORKPLACE OR WORK RELATED (PRODS 1-3)
C
        DO T=1,3
	    SZ_NM_PA(SZ,T) = SZ_PRODS(SZ,T)*NM_FACTOR(1)
C ## -- Heither, 05-24-2016: WRITE OUT FILE TO VERIFY NONMOTORIZED CALCULATIONS          
          IF (SZ .EQ. 30) THEN
            WRITE (80,'(I6,I2,3F9.1,3I6,3F9.1,F9.3)') SZ, T, 
     A        SZ_NM_PA(SZ,T), SZ_PRODS(SZ,T), PED_FACTOR, 
     B        SZ_TOTEMP(SZ), SZ_HH_NOVEH(SZ), SZ_HH(SZ),
     C        NM_UTIL(1), NM_FACTOR(1), M_UTIL(1), SZ_AREA(SZ)         
          ENDIF
C          WRITE (16,'(2I5,3F12.6)')  SZ, T, SZ_NM_PA(SZ,T), 
C     A      SZ_PRODS(SZ,T), NM_FACTOR(1)
        ENDDO
C
C     FROM HOME TO NONWORK (PRODS 4-7)
C
        DO T=4,7
	    SZ_NM_PA(SZ,T) = SZ_PRODS(SZ,T)*NM_FACTOR(2)
C ## -- Heither, 05-24-2016: WRITE OUT FILE TO VERIFY NONMOTORIZED CALCULATIONS          
          IF (SZ .EQ. 30) THEN
            WRITE (80,'(I6,I2,3F9.1,3I6,3F9.1,F9.3)') SZ, T, 
     A        SZ_NM_PA(SZ,T), SZ_PRODS(SZ,T), PED_FACTOR, 
     B        SZ_TOTEMP(SZ), SZ_HH_NOVEH(SZ), SZ_HH(SZ),
     C        NM_UTIL(2), NM_FACTOR(2), M_UTIL(2), SZ_AREA(SZ)         
          ENDIF          
        ENDDO
C
C     FROM WORK (PRODS 8-11)
C
        DO T=8,11
	    SZ_NM_PA(SZ,T) = SZ_PRODS(SZ,T)*NM_FACTOR(3)
C ## -- Heither, 05-24-2016: WRITE OUT FILE TO VERIFY NONMOTORIZED CALCULATIONS          
          IF (SZ .EQ. 30) THEN
            WRITE (80,'(I6,I2,3F9.1,3I6,3F9.1,F9.3)') SZ, T,
     A        SZ_NM_PA(SZ,T), SZ_PRODS(SZ,T), PED_FACTOR, 
     B        SZ_TOTEMP(SZ), SZ_HH_NOVEH(SZ), SZ_HH(SZ),
     C        NM_UTIL(3), NM_FACTOR(3), M_UTIL(3), SZ_AREA(SZ)         
          ENDIF          
        ENDDO
C
C     REMAINING NONHOME/WORK PRUPOSES (PRODS 12-20)
C
        DO T=12,20
	    SZ_NM_PA(SZ,T) = SZ_PRODS(SZ,T)*NM_FACTOR(4)
C ## -- Heither, 05-24-2016: WRITE OUT FILE TO VERIFY NONMOTORIZED CALCULATIONS          
          IF (SZ .EQ. 30) THEN
            WRITE (80,'(I6,I2,3F9.1,3I6,3F9.1,F9.3)') SZ, T,
     A        SZ_NM_PA(SZ,T), SZ_PRODS(SZ,T), PED_FACTOR, 
     B        SZ_TOTEMP(SZ), SZ_HH_NOVEH(SZ), SZ_HH(SZ),
     C        NM_UTIL(4), NM_FACTOR(4), M_UTIL(4), SZ_AREA(SZ)         
          ENDIF          
        ENDDO
C
C     NEXT NONWORKERS (PRODS(21-33))
C
C       NM_FACTOR(1) = FROM HOME
C       NM_FACTOR(2) = FROM NONHOME/WORK
C
        IF (SZ_HH(SZ) .GT. 0) THEN
          NM_UTIL(1) = 0.5507E-01*PED_FACTOR +
     A      1.530 *(SZ_HH_NOVEH(SZ)/SZ_HH(SZ))

          IF (SZ_CBD(SZ) .EQ. 1) THEN 
            M_UTIL(1) = 4.566
          ELSE
            M_UTIL(1) = 3.968
          ENDIF
        ELSE
	    NM_UTIL(1) = 0.0
	    M_UTIL(1) = 0.0
	  ENDIF

        NM_UTIL(2) = 0.8307E-04*(SZ_HH(SZ)/SZ_AREA(SZ)) +
     A    0.2368E-05*(SZ_TOTEMP(SZ)/SZ_AREA(SZ)) +
     B    0.8552E-01*PED_FACTOR

        IF (SZ_CBD(SZ) .EQ. 1) THEN 
          M_UTIL(2) = 4.173
        ELSE
          M_UTIL(2) = 4.742
        ENDIF
C
        DO I=1,2
	    NM_FACTOR(I) = 
     A      EXP(NM_UTIL(I))/(EXP(M_UTIL(I))+EXP(NM_UTIL(I)))
        ENDDO
C
C     LIMIT FACTORS TO MAX OBSERVED LEVELS 
C
        IF (NM_FACTOR(1) .GT. 0.25) NM_FACTOR(1) = 0.25
        IF (NM_FACTOR(2) .GT. 0.60) NM_FACTOR(2) = 0.60
C
C     FROM HOME (PRODS 21-24)
C
        DO T=21,24
	    SZ_NM_PA(SZ,T) = SZ_PRODS(SZ,T)*NM_FACTOR(1)
C ## -- Heither, 05-24-2016: WRITE OUT FILE TO VERIFY NONMOTORIZED CALCULATIONS          
          IF (SZ .EQ. 30) THEN
            WRITE (80,'(I6,I2,3F9.1,3I6,3F9.1,F9.3)') SZ, T, 
     A        SZ_NM_PA(SZ,T), SZ_PRODS(SZ,T), PED_FACTOR, 
     B        SZ_TOTEMP(SZ), SZ_HH_NOVEH(SZ), SZ_HH(SZ),
     C        NM_UTIL(1), NM_FACTOR(1), M_UTIL(1), SZ_AREA(SZ)         
          ENDIF          
        ENDDO
C
C     REMAINING NONHOME/WORK PRUPOSES (PRODS 25-33)
C
        DO T=25,33
	    SZ_NM_PA(SZ,T) = SZ_PRODS(SZ,T)*NM_FACTOR(2)
C ## -- Heither, 05-24-2016: WRITE OUT FILE TO VERIFY NONMOTORIZED CALCULATIONS          
          IF (SZ .EQ. 30) THEN
            WRITE (80,'(I6,I2,3F9.1,3I6,3F9.1,F9.3)') SZ, T, 
     A        SZ_NM_PA(SZ,T), SZ_PRODS(SZ,T), PED_FACTOR, 
     B        SZ_TOTEMP(SZ), SZ_HH_NOVEH(SZ), SZ_HH(SZ),
     C        NM_UTIL(2), NM_FACTOR(2), M_UTIL(2), SZ_AREA(SZ)         
          ENDIF          
        ENDDO
C
C     FINALLY, CHILD 12-15 (PRODS 34-49)
C
C       NM_FACTOR(1) = FROM HOME
C       NM_FACTOR(2) = FROM NONHOME/WORK
C
        NM_UTIL(1) = 0.5351E-01*PED_FACTOR

        M_UTIL(1) = 2.418

        NM_UTIL(2) = 0.6245E-01*PED_FACTOR

        M_UTIL(2) = 2.454
C
        DO I=1,2
	    NM_FACTOR(I) = 
     A      EXP(NM_UTIL(I))/(EXP(M_UTIL(I))+EXP(NM_UTIL(I)))
        ENDDO
C
C     LIMIT FACTORS TO MAX OBSERVED LEVELS 
C
        IF (NM_FACTOR(1) .GT. 0.65) NM_FACTOR(1) = 0.65
        IF (NM_FACTOR(2) .GT. 0.65) NM_FACTOR(2) = 0.65
C
C     FROM HOME (PRODS 34-37)
C
        DO T=34,37
	    SZ_NM_PA(SZ,T) = SZ_PRODS(SZ,T)*NM_FACTOR(1)
C ## -- Heither, 05-24-2016: WRITE OUT FILE TO VERIFY NONMOTORIZED CALCULATIONS          
          IF (SZ .EQ. 30) THEN
            WRITE (80,'(I6,I2,3F9.1,3I6,3F9.1,F9.3)') SZ, T, 
     A        SZ_NM_PA(SZ,T), SZ_PRODS(SZ,T), PED_FACTOR, 
     B        SZ_TOTEMP(SZ), SZ_HH_NOVEH(SZ), SZ_HH(SZ),
     C        NM_UTIL(1), NM_FACTOR(1), M_UTIL(1), SZ_AREA(SZ)         
          ENDIF          
        ENDDO
C
C     REMAINING NONHOME/WORK PRUPOSES (PRODS 38-49)
C
        DO T=38,49
	    SZ_NM_PA(SZ,T) = SZ_PRODS(SZ,T)*NM_FACTOR(2)
C ## -- Heither, 05-24-2016: WRITE OUT FILE TO VERIFY NONMOTORIZED CALCULATIONS          
          IF (SZ .EQ. 30) THEN
            WRITE (80,'(I6,I2,3F9.1,3I6,3F9.1,F9.3)') SZ, T, 
     A        SZ_NM_PA(SZ,T), SZ_PRODS(SZ,T), PED_FACTOR, 
     B        SZ_TOTEMP(SZ), SZ_HH_NOVEH(SZ), SZ_HH(SZ),
     C        NM_UTIL(2), NM_FACTOR(2), M_UTIL(2), SZ_AREA(SZ)         
          ENDIF          
        ENDDO

      ENDDO
C****
C      DO SZ=1,SUBZONES
C	  WRITE (16,'(I6,2F9.1)') SZ, SZ_NM_PA(SZ,1), SZ_GQ_NM_PA(SZ,1)
C      ENDDO
C	DO SZ=1,SUBZONES
C	  WRITE (16,'(I6,2F9.1)') SZ, SZ_NM_PA(SZ,2), SZ_GQ_NM_PA(SZ,2)
C      ENDDO
C****       
C
C     CONVERT SF_PRODS AND SF_ATTRS TO MOTORIZED 
C 
      DO SZ=1,SUBZONES
	  DO T=1,49
	    SZ_PRODS(SZ,T) = SZ_PRODS(SZ,T) - SZ_NM_PA(SZ,T)
		
	    TEMPA = SZ_ATTRS(SZ,T) - SZ_NM_PA(SZ,T)
	    IF (TEMPA .LT. 0.000) THEN
	      SZ_ATTRS(SZ,T) = 0
          ELSE
	      SZ_ATTRS(SZ,T) = SZ_ATTRS(SZ,T) - SZ_NM_PA(SZ,T)
          ENDIF
        ENDDO
      ENDDO
C
C     REBALANCE MOTORIZED ATTRACTIONS TO MOTORIZED PRODUCTIONS
C
      DO SZ=1,SUBZONES
	  DO T=1,49
	    SZ_TPRODS(T) = SZ_TPRODS(T) + SZ_PRODS(SZ,T)
	    SZ_TATTRS(T) = SZ_TATTRS(T) + SZ_ATTRS(SZ,T)
        ENDDO
      ENDDO
C
      DO SZ=1,SUBZONES
	  DO T=1,49
          SZ_ATTRS(SZ,T) = SZ_ATTRS(SZ,T)*(SZ_TPRODS(T)/SZ_TATTRS(T))     
        ENDDO
      ENDDO
C
      DO SZ=1,SUBZONES
        GQ_TPRODS(1) = GQ_TPRODS(1) + SZ_GQ_PRODS(SZ,1)
	  GQ_TATTRS(1) = GQ_TATTRS(1) + SZ_GQ_ATTRS(SZ,1)

	  DO T=3,33
          GQ_TPRODS(T) = GQ_TPRODS(T) + SZ_GQ_PRODS(SZ,T)
	    GQ_TATTRS(T) = GQ_TATTRS(T) + SZ_GQ_ATTRS(SZ,T)
        ENDDO
      ENDDO
C
      DO SZ=1,SUBZONES
        SZ_GQ_ATTRS(SZ,1) = 
     A    SZ_GQ_ATTRS(SZ,1)*(GQ_TPRODS(1)/GQ_TATTRS(1))     

	  DO T=3,33
          SZ_GQ_ATTRS(SZ,T) = 
     A      SZ_GQ_ATTRS(SZ,T)*(GQ_TPRODS(T)/GQ_TATTRS(T))     
        ENDDO
      ENDDO
C
C     NOW COMBINE HH AND GQ PRODS AND ATTRACTIONS INTO FINAL ARRAYS
C
      DO SZ=1,SUBZONES

        SZ_PRODS(SZ,1) = SZ_PRODS(SZ,1) + SZ_GQ_PRODS(SZ,1)
	  SZ_ATTRS(SZ,1) = SZ_ATTRS(SZ,1) + SZ_GQ_ATTRS(SZ,1)

	  DO T=3,33
          SZ_PRODS(SZ,T) = SZ_PRODS(SZ,T) + SZ_GQ_PRODS(SZ,T)
	    SZ_ATTRS(SZ,T) = SZ_ATTRS(SZ,T) + SZ_GQ_ATTRS(SZ,T)
        ENDDO
      ENDDO
C
C     ACCUMULATE NONMOTORIZED TRIPS FOR SUMMARIES
C
      DO SZ=1,SUBZONES
	  DO J=1,COUNTIES
	    IF (SZ_CO(SZ) .EQ. CO_NUM(J)) ICO = J
	  ENDDO

        DO J=1,PUMA1
	    IF (SZ_PUMA1(SZ) .EQ. P1_NUM(J)) IP1 = J
	  ENDDO

        DO J=1,PUMA5
	    IF (SZ_PUMA5(SZ) .EQ. P5_NUM(J)) IP5 = J
	  ENDDO

	  ICHI = SZ_CHI(SZ)
	  ICBD = SZ_CBD(SZ)
C
        DO T=1,49

	    CO_NM_PA(ICO,T) = CO_NM_PA(ICO,T) + SZ_NM_PA(SZ,T)
	    P1_NM_PA(IP1,T) = P1_NM_PA(IP1,T) + SZ_NM_PA(SZ,T)
	    P5_NM_PA(IP5,T) = P5_NM_PA(IP5,T) + SZ_NM_PA(SZ,T)

          REG_NM_PA(T) = REG_NM_PA(T) + SZ_NM_PA(SZ,T)
C*****
C      WRITE (16,*) REG_NM_PA(1), REG_NM_PA(2)
C*****
          IF (ICHI .GT. 0) THEN 
            CHI_NM_PA(T) = CHI_NM_PA(T) + SZ_NM_PA(SZ,T)
          ENDIF

          IF (ICBD .GT. 0) THEN 
            CBD_NM_PA(T) = CBD_NM_PA(T) + SZ_NM_PA(SZ,T)
          ENDIF
        ENDDO
C
C     ACCUMULATE FOR ORIGINAL TRIP TYPES
C
        DO T=1,4
	    CO_NM_PAOLD(ICO,1) = CO_NM_PAOLD(ICO,1) + SZ_NM_PA(SZ,T)
	    P1_NM_PAOLD(IP1,1) = P1_NM_PAOLD(IP1,1) + SZ_NM_PA(SZ,T)
	    P5_NM_PAOLD(IP5,1) = P5_NM_PAOLD(IP5,1) + SZ_NM_PA(SZ,T)

          REG_NM_PAOLD(1) = REG_NM_PAOLD(1) + SZ_NM_PA(SZ,T)

          IF (ICHI .GT. 0) THEN 
            CHI_NM_PAOLD(1) = CHI_NM_PAOLD(1) + SZ_NM_PA(SZ,T)
          ENDIF

          IF (ICBD .GT. 0) THEN 
            CBD_NM_PAOLD(1) = CBD_NM_PAOLD(1) + SZ_NM_PA(SZ,T)
          ENDIF
        ENDDO
C
	  CO_NM_PAOLD(ICO,2) = CO_NM_PAOLD(ICO,2) + SZ_NM_PA(SZ,7)
	  P1_NM_PAOLD(IP1,2) = P1_NM_PAOLD(IP1,2) + SZ_NM_PA(SZ,7)
	  P5_NM_PAOLD(IP5,2) = P5_NM_PAOLD(IP5,2) + SZ_NM_PA(SZ,7)

        REG_NM_PAOLD(2) = REG_NM_PAOLD(2) + SZ_NM_PA(SZ,7)

        IF (ICHI .GT. 0) THEN 
          CHI_NM_PAOLD(2) = CHI_NM_PAOLD(2) + SZ_NM_PA(SZ,7)
        ENDIF

        IF (ICBD .GT. 0) THEN 
          CBD_NM_PAOLD(2) = CBD_NM_PAOLD(2) + SZ_NM_PA(SZ,7)
        ENDIF
C
        DO T=5,6
	    CO_NM_PAOLD(ICO,3) = CO_NM_PAOLD(ICO,3) + SZ_NM_PA(SZ,T)
	    P1_NM_PAOLD(IP1,3) = P1_NM_PAOLD(IP1,3) + SZ_NM_PA(SZ,T)
	    P5_NM_PAOLD(IP5,3) = P5_NM_PAOLD(IP5,3) + SZ_NM_PA(SZ,T)

          REG_NM_PAOLD(3) = REG_NM_PAOLD(3) + SZ_NM_PA(SZ,T)

          IF (ICHI .GT. 0) THEN 
            CHI_NM_PAOLD(3) = CHI_NM_PAOLD(3) + SZ_NM_PA(SZ,T)
          ENDIF

          IF (ICBD .GT. 0) THEN 
            CBD_NM_PAOLD(3) = CBD_NM_PAOLD(3) + SZ_NM_PA(SZ,T)
          ENDIF
        ENDDO
C
	  CO_NM_PAOLD(ICO,4) = CO_NM_PAOLD(ICO,4) + SZ_NM_PA(SZ,10)
	  P1_NM_PAOLD(IP1,4) = P1_NM_PAOLD(IP1,4) + SZ_NM_PA(SZ,10)
	  P5_NM_PAOLD(IP5,4) = P5_NM_PAOLD(IP5,4) + SZ_NM_PA(SZ,10)

        REG_NM_PAOLD(4) = REG_NM_PAOLD(4) + SZ_NM_PA(SZ,10)

        IF (ICHI .GT. 0) THEN 
          CHI_NM_PAOLD(4) = CHI_NM_PAOLD(4) + SZ_NM_PA(SZ,10)
        ENDIF

        IF (ICBD .GT. 0) THEN 
          CBD_NM_PAOLD(4) = CBD_NM_PAOLD(4) + SZ_NM_PA(SZ,10)
        ENDIF
C
        DO T=8,9
	    CO_NM_PAOLD(ICO,5) = CO_NM_PAOLD(ICO,5) + SZ_NM_PA(SZ,T)
	    P1_NM_PAOLD(IP1,5) = P1_NM_PAOLD(IP1,5) + SZ_NM_PA(SZ,T)
	    P5_NM_PAOLD(IP5,5) = P5_NM_PAOLD(IP5,5) + SZ_NM_PA(SZ,T)

          REG_NM_PAOLD(5) = REG_NM_PAOLD(5) + SZ_NM_PA(SZ,T)

          IF (ICHI .GT. 0) THEN 
            CHI_NM_PAOLD(5) = CHI_NM_PAOLD(5) + SZ_NM_PA(SZ,T)
          ENDIF

          IF (ICBD .GT. 0) THEN 
            CBD_NM_PAOLD(5) = CBD_NM_PAOLD(5) + SZ_NM_PA(SZ,T)
          ENDIF
        ENDDO
C
	  CO_NM_PAOLD(ICO,6) = CO_NM_PAOLD(ICO,6) + SZ_NM_PA(SZ,11)
	  P1_NM_PAOLD(IP1,6) = P1_NM_PAOLD(IP1,6) + SZ_NM_PA(SZ,11)
	  P5_NM_PAOLD(IP5,6) = P5_NM_PAOLD(IP5,6) + SZ_NM_PA(SZ,11)

        REG_NM_PAOLD(6) = REG_NM_PAOLD(6) + SZ_NM_PA(SZ,11)

        IF (ICHI .GT. 0) THEN 
          CHI_NM_PAOLD(6) = CHI_NM_PAOLD(6) + SZ_NM_PA(SZ,11)
        ENDIF

        IF (ICBD .GT. 0) THEN 
          CBD_NM_PAOLD(6) = CBD_NM_PAOLD(6) + SZ_NM_PA(SZ,11)
        ENDIF
C        
	  DO T=12,20
	    CO_NM_PAOLD(ICO,7) = CO_NM_PAOLD(ICO,7) + SZ_NM_PA(SZ,T)
	    P1_NM_PAOLD(IP1,7) = P1_NM_PAOLD(IP1,7) + SZ_NM_PA(SZ,T)
	    P5_NM_PAOLD(IP5,7) = P5_NM_PAOLD(IP5,7) + SZ_NM_PA(SZ,T)

          REG_NM_PAOLD(7) = REG_NM_PAOLD(7) + SZ_NM_PA(SZ,T)

          IF (ICHI .GT. 0) THEN 
            CHI_NM_PAOLD(7) = CHI_NM_PAOLD(7) + SZ_NM_PA(SZ,T)
          ENDIF

          IF (ICBD .GT. 0) THEN 
            CBD_NM_PAOLD(7) = CBD_NM_PAOLD(7) + SZ_NM_PA(SZ,T)
          ENDIF
        ENDDO
C
	  CO_NM_PAOLD(ICO,8) = CO_NM_PAOLD(ICO,8) + SZ_NM_PA(SZ,24)
	  P1_NM_PAOLD(IP1,8) = P1_NM_PAOLD(IP1,8) + SZ_NM_PA(SZ,24)
	  P5_NM_PAOLD(IP5,8) = P5_NM_PAOLD(IP5,8) + SZ_NM_PA(SZ,24)

        REG_NM_PAOLD(8) = REG_NM_PAOLD(8) + SZ_NM_PA(SZ,24)

        IF (ICHI .GT. 0) THEN 
          CHI_NM_PAOLD(8) = CHI_NM_PAOLD(8) + SZ_NM_PA(SZ,24)
        ENDIF

        IF (ICBD .GT. 0) THEN 
          CBD_NM_PAOLD(8) = CBD_NM_PAOLD(8) + SZ_NM_PA(SZ,24)
        ENDIF
C        
	  DO T=21,23
	    CO_NM_PAOLD(ICO,9) = CO_NM_PAOLD(ICO,9) + SZ_NM_PA(SZ,T)
	    P1_NM_PAOLD(IP1,9) = P1_NM_PAOLD(IP1,9) + SZ_NM_PA(SZ,T)
	    P5_NM_PAOLD(IP5,9) = P5_NM_PAOLD(IP5,9) + SZ_NM_PA(SZ,T)

          REG_NM_PAOLD(9) = REG_NM_PAOLD(9) + SZ_NM_PA(SZ,T)

          IF (ICHI .GT. 0) THEN 
            CHI_NM_PAOLD(9) = CHI_NM_PAOLD(9) + SZ_NM_PA(SZ,T)
          ENDIF

          IF (ICBD .GT. 0) THEN 
            CBD_NM_PAOLD(9) = CBD_NM_PAOLD(9) + SZ_NM_PA(SZ,T)
          ENDIF
        ENDDO
C
	  DO T=25,33
	    CO_NM_PAOLD(ICO,10) = CO_NM_PAOLD(ICO,10) + SZ_NM_PA(SZ,T)
	    P1_NM_PAOLD(IP1,10) = P1_NM_PAOLD(IP1,10) + SZ_NM_PA(SZ,T)
	    P5_NM_PAOLD(IP5,10) = P5_NM_PAOLD(IP5,10) + SZ_NM_PA(SZ,T)

          REG_NM_PAOLD(10) = REG_NM_PAOLD(10) + SZ_NM_PA(SZ,T)

          IF (ICHI .GT. 0) THEN 
            CHI_NM_PAOLD(10) = CHI_NM_PAOLD(10) + SZ_NM_PA(SZ,T)
          ENDIF

          IF (ICBD .GT. 0) THEN 
            CBD_NM_PAOLD(10) = CBD_NM_PAOLD(10) + SZ_NM_PA(SZ,T)
          ENDIF
        ENDDO
C
	  DO T=34,37
	    CO_NM_PAOLD(ICO,11) = CO_NM_PAOLD(ICO,11) + SZ_NM_PA(SZ,T)
	    P1_NM_PAOLD(IP1,11) = P1_NM_PAOLD(IP1,11) + SZ_NM_PA(SZ,T)
	    P5_NM_PAOLD(IP5,11) = P5_NM_PAOLD(IP5,11) + SZ_NM_PA(SZ,T)

          REG_NM_PAOLD(11) = REG_NM_PAOLD(11) + SZ_NM_PA(SZ,T)

          IF (ICHI .GT. 0) THEN 
            CHI_NM_PAOLD(11) = CHI_NM_PAOLD(11) + SZ_NM_PA(SZ,T)
          ENDIF

          IF (ICBD .GT. 0) THEN 
            CBD_NM_PAOLD(11) = CBD_NM_PAOLD(11) + SZ_NM_PA(SZ,T)
          ENDIF
        ENDDO
      ENDDO
C
C     PRINT NONMOTORIZED ROW AND COLUMN ATTRACTIONS FOR CHECKING
C
      IF (EXP_TTYPE) GO TO 1001
C
C     SUMMARIES USING ORIGINAL TRIP CATEGORIES
C
	WRITE(16,'(/A)') 'HOUSEHOLD AND GROUP QUARTERS NONMOTOR TRIPS'
	WRITE (16,'(/A)')  'COUNTIES'

	WRITE (16,'(/A)')  'WORKERS NONMOTOR TRIPS'
	WRITE (16,'(/A)')  ' COUNTY   HWORK   HSHOP   HOTHR   WSHOP   WOTH
     AR   WWORK   OTHER'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------------'       
	
	DO I=1,COUNTIES
	  WRITE (16,'(I7,7F8.0)') CO_NUM(I), (CO_NM_PAOLD(I,T),T=1,7) 
	ENDDO

	WRITE (16,'(A,7F8.0)')  '  TOTAL', (REG_NM_PAOLD(T),T=1,7) 
      WRITE (16,'(/A,7F8.0)') 'CHICAGO', (CHI_NM_PAOLD(T),T=1,7) 
	WRITE (16,'(A,7F8.0)')  'CHI CBD', (CBD_NM_PAOLD(T),T=1,7) 
C
	WRITE(16,'(/A)') 'NONWORKERS NONMOTOR TRIPS'
	WRITE (16,'(A)')  ' COUNTY   HSHOP   HOTHR   OTHER'
	WRITE (16,'(A)')  '-------------------------------'
	      
	DO I=1,COUNTIES
	  WRITE (16,'(I7,3F8.0)') CO_NUM(I), (CO_NM_PAOLD(I,T),T=8,10) 
	ENDDO

	WRITE (16,'(A,3F8.0)')  '  TOTAL', (REG_NM_PAOLD(T),T=8,10)
      WRITE (16,'(/A,3F8.0)') 'CHICAGO', (CHI_NM_PAOLD(T),T=8,10)
	WRITE (16,'(A,3F8.0)')  'CHI CBD', (CBD_NM_PAOLD(T),T=8,10)
C
	WRITE(16,'(/A)') 'CHILDREN(12-15) NONMOTOR TRIPS'
	WRITE (16,'(A)')  ' COUNTY   HOTHR'
	WRITE (16,'(A)')  '---------------'
      
	DO I=1,COUNTIES
	  WRITE (16,'(I7,F8.0)') CO_NUM(I), CO_NM_PAOLD(I,11) 
	ENDDO

	WRITE (16,'(A,F8.0)')  '  TOTAL', REG_NM_PAOLD(11)
      WRITE (16,'(/A,F8.0)') 'CHICAGO', CHI_NM_PAOLD(11)
	WRITE (16,'(A,F8.0)')  'CHI CBD', CBD_NM_PAOLD(11)
C
C     REPEAT FOR ONE PERCENT PUMS
C
	WRITE (16,'(/A)')  'ONE PERCENT PUMAS'

	WRITE (16,'(/A)')  'WORKERS NONMOTOR TRIPS'
	WRITE (16,'(/A)')  '  PUMA1   HWORK   HSHOP   HOTHR   WSHOP   WOTH
     AR   WWORK   OTHER'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------------'       
	
	DO I=1,PUMA1
	  WRITE (16,'(I7,7F8.0)') P1_NUM(I), (P1_NM_PAOLD(I,T),T=1,7) 
	ENDDO

	WRITE (16,'(A,7F8.0)')  '  TOTAL', (REG_NM_PAOLD(T),T=1,7)
C
	WRITE(16,'(/A)') 'NONWORKERS NONMOTOR TRIPS'
	WRITE (16,'(A)')  '  PUMA1   HSHOP   HOTHR   OTHER'
	WRITE (16,'(A)')  '-------------------------------'
	      
	DO I=1,PUMA1
	  WRITE (16,'(I7,3F8.0)') P1_NUM(I), (P1_NM_PAOLD(I,T),T=8,10) 
	ENDDO

	WRITE (16,'(A,3F8.0)')  '  TOTAL', (REG_NM_PAOLD(T),T=8,10)
C
	WRITE(16,'(/A)') 'CHILDREN(12-15) NONMOTOR TRIPS'
	WRITE (16,'(A)')  '  PUMA1   HOTHR'
	WRITE (16,'(A)')  '---------------'
      
	DO I=1,PUMA1
	  WRITE (16,'(I7,F8.0)') P1_NUM(I), P1_NM_PAOLD(I,11) 
	ENDDO

	WRITE (16,'(A,F8.0)')  '  TOTAL', REG_NM_PAOLD(11)
C
C     REPEAT FOR FIVE PERCENT PUMS
C
	WRITE (16,'(/A)')  'FIVE PERCENT PUMAS'

	WRITE (16,'(/A)')  'WORKERS NONMOTOR TRIPS'
	WRITE (16,'(/A)')  '  PUMA5   HWORK   HSHOP   HOTHR   WSHOP   WOTH
     AR   WWORK   OTHER'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------------' 
	
	DO I=1,PUMA5
	  WRITE (16,'(I7,7F8.0)') P5_NUM(I), (P5_NM_PAOLD(I,T),T=1,7) 
	ENDDO

	WRITE (16,'(A,7F8.0)')  '  TOTAL', (REG_NM_PAOLD(T),T=1,7)
C
	WRITE(16,'(/A)') 'NONWORKERS NONMOTOR TRIPS'
	WRITE (16,'(A)')  '  PUMA5   HSHOP   HOTHR   OTHER'
	WRITE (16,'(A)')  '-------------------------------'
	      
	DO I=1,PUMA5
	  WRITE (16,'(I7,3F8.0)') P5_NUM(I), (P5_NM_PAOLD(I,T),T=8,10) 
	ENDDO

	WRITE (16,'(A,3F8.0)')  '  TOTAL', (REG_NM_PAOLD(T),T=8,10)
C
	WRITE(16,'(/A)') 'CHILDREN(12-15) NONMOTOR TRIPS'
	WRITE (16,'(A)')  '  PUMA5   HOTHR'
	WRITE (16,'(A)')  '---------------'
      
	DO I=1,PUMA5
	  WRITE (16,'(I7,F8.0)') P5_NUM(I), P5_NM_PAOLD(I,11) 
	ENDDO

	WRITE (16,'(A,F8.0)')  '  TOTAL', REG_NM_PAOLD(11)
C
      GO TO 1002
C***********************************************************************
C
C     SUMMARIES USING EXPANDED TRIP CATEGORIES
C
C***********************************************************************
 1001 CONTINUE
C
	WRITE(16,'(/A)') 'EXPANDED HOUSEHOLD AND GROUP QUARTERS NONMOTOR T
     ARIPS'
	WRITE (16,'(/A)')  'COUNTIES'

	WRITE (16,'(/A)')  'WORKERS HOME BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   ' COUNTY WKPL(<$) WKPL(>$) WORK REL   SCHOOL  N
     AH/W(H) NH/W(NH)     SHOP'   
	WRITE (16,'(A)')   '----------------------------------------------
     A------------------------'       
	DO I=1,COUNTIES
	  WRITE (16,'(I7,7F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=1,7)
     	ENDDO
	
	WRITE (16,'(A,7F9.0)')  '  TOTAL', (REG_NM_PA(T),T=1,7) 
	WRITE (16,'(/A,7F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=1,7) 
	WRITE (16,'(A,7F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=1,7) 
C
	WRITE (16,'(/A)')  'WORKER WORK BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   ' COUNTY  NH/W(H) NH/W(NH)     SHOP     WORK'   
	WRITE (16,'(A)')   '-------------------------------------------'       
	DO I=1,COUNTIES
	  WRITE (16,'(I7,4F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=8,11)
	ENDDO

	WRITE (16,'(A,4F9.0)')  '  TOTAL', (REG_NM_PA(T),T=8,11) 
	WRITE (16,'(/A,4F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=8,11) 
	WRITE (16,'(A,4F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=8,11) 
C
      WRITE (16,'(/A)')  'WORKER NONH/W AT HOUSEHOLDS BASED NONMOTOR TRI
     APS'
	WRITE (16,'(A)')   ' COUNTY  NH/W(H) NH/W(NH)     SHOP'
	WRITE (16,'(A)')   '----------------------------------'  
	DO I=1,COUNTIES
	  WRITE (16,'(I7,3F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=12,14)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=12,14) 
	WRITE (16,'(/A,3F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=12,14) 
	WRITE (16,'(A,3F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=12,14) 
C
      WRITE (16,'(/A)')  'WORKER NONH/W NOT AT HOUSEHOLDS BASED NONMOTOR
     A TRIPS'
	WRITE (16,'(A)')   ' COUNTY  NH/W(H) NH/W(NH)     SHOP'
	WRITE (16,'(A)')   '----------------------------------'  
	DO I=1,COUNTIES
	  WRITE (16,'(I7,3F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=15,17)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=15,17) 
	WRITE (16,'(/A,3F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=15,17) 
	WRITE (16,'(A,3F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=15,17) 
C
      WRITE (16,'(/A)')  'WORKER SHOP BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   ' COUNTY  NH/W(H) NH/W(NH)     SHOP'
	WRITE (16,'(A)')   '----------------------------------'  
	DO I=1,COUNTIES
	  WRITE (16,'(I7,3F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=18,20)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=18,20) 
	WRITE (16,'(/A,3F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=18,20) 
	WRITE (16,'(A,3F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=18,20) 
C
      WRITE (16,'(/A)')  'NONWORKER HOME BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   ' COUNTY   SCHOOL    NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '-------------------------------------------'       
	DO I=1,COUNTIES
	  WRITE (16,'(I7,4F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=21,24)  
	ENDDO
	
	WRITE (16,'(A,4F9.0)')  '  TOTAL', (REG_NM_PA(T),T=21,24) 
	WRITE (16,'(/A,4F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=21,24) 
	WRITE (16,'(A,4F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=21,24) 
C
      WRITE (16,'(/A)')  'NONWORKER NONHOME AT HOUSEHOLDS BASED NONMOTOR 
     A TRIPS'
	WRITE (16,'(A)')   ' COUNTY   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,COUNTIES
	  WRITE (16,'(I7,3F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=25,27)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=25,27) 
	WRITE (16,'(/A,3F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=25,27) 
	WRITE (16,'(A,3F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=25,27) 
C
      WRITE (16,'(/A)')  'NONWORKER NONHOME NOT AT HOUSEHOLDS BASED NONM
     AOTOR TRIPS'
	WRITE (16,'(A)')   ' COUNTY   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,COUNTIES
	  WRITE (16,'(I7,3F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=28,30)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=28,30) 
	WRITE (16,'(/A,3F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=28,30) 
	WRITE (16,'(A,3F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=28,30) 
C
      WRITE (16,'(/A)')  'NONWORKER SHOP BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   ' COUNTY   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,COUNTIES
	  WRITE (16,'(I7,3F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=31,33)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=31,33) 
	WRITE (16,'(/A,3F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=31,33) 
	WRITE (16,'(A,3F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=31,33) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) HOME BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   ' COUNTY   SCHOOL    NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '-------------------------------------------'       
	DO I=1,COUNTIES
	  WRITE (16,'(I7,4F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=34,37)  
	ENDDO
	
	WRITE (16,'(A,4F9.0)')  '  TOTAL', (REG_NM_PA(T),T=34,37) 
	WRITE (16,'(/A,4F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=34,37) 
	WRITE (16,'(A,4F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=34,37) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) SCHOOL BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   ' COUNTY   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,COUNTIES
	  WRITE (16,'(I7,3F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=38,40)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=38,40) 
	WRITE (16,'(/A,3F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=38,40) 
	WRITE (16,'(A,3F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=38,40) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) NONHOME AT HOUSEHOLDS BASED NONMO
     ATOR TRIPS'
	WRITE (16,'(A)')   ' COUNTY   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,COUNTIES
	  WRITE (16,'(I7,3F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=41,43)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=41,43) 
	WRITE (16,'(/A,3F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=41,43) 
	WRITE (16,'(A,3F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=41,43) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) NONHOME NOT AT HOUSEHOLDS BASED N
     AONMOTOR TRIPS'
	WRITE (16,'(A)')   ' COUNTY   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,COUNTIES
	  WRITE (16,'(I7,3F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=44,46)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=44,46) 
	WRITE (16,'(/A,3F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=44,46) 
	WRITE (16,'(A,3F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=44,46) 

      WRITE (16,'(/A)')  'CHILD(12-15) SHOP BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   ' COUNTY   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,COUNTIES
	  WRITE (16,'(I7,3F9.0)') CO_NUM(I), (CO_NM_PA(I,T),T=47,49)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=47,49) 
	WRITE (16,'(/A,3F9.0)') 'CHICAGO', (CHI_NM_PA(T),T=47,49) 
	WRITE (16,'(A,3F9.0)')  'CHI CBD', (CBD_NM_PA(T),T=47,49) 
C***********************************************************************
C
	WRITE (16,'(/A)')  'ONE PERCENT PUMAS'

	WRITE (16,'(/A)')  'WORKERS HOME BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA1 WKPL(<$) WKPL(>$) WORK REL   SCHOOL  N
     AH/W(H) NH/W(NH)     SHOP'   
	WRITE (16,'(A)')   '----------------------------------------------
     A------------------------'       
	DO I=1,PUMA1
	  WRITE (16,'(I7,7F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=1,7)
     	ENDDO
	
	WRITE (16,'(A,7F9.0)')  '  TOTAL', (REG_NM_PA(T),T=1,7) 
C
	WRITE (16,'(/A)')  'WORKER WORK BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA1  NH/W(H) NH/W(NH)     SHOP     WORK'   
	WRITE (16,'(A)')   '-------------------------------------------'       
	DO I=1,PUMA1
	  WRITE (16,'(I7,4F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=8,11)
	ENDDO

	WRITE (16,'(A,4F9.0)')  '  TOTAL', (REG_NM_PA(T),T=8,11) 
C
      WRITE (16,'(/A)')  'WORKER NONH/W AT HOUSEHOLDS BASED NONMOTOR TRI
     APS'
	WRITE (16,'(A)')   '  PUMA1  NH/W(H) NH/W(NH)     SHOP'
	WRITE (16,'(A)')   '----------------------------------'  
	DO I=1,PUMA1
	  WRITE (16,'(I7,3F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=12,14)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=12,14) 
C
      WRITE (16,'(/A)')  'WORKER NONH/W NOT AT HOUSEHOLDS BASED NONMOTOR
     A TRIPS'
	WRITE (16,'(A)')   '  PUMA1  NH/W(H) NH/W(NH)     SHOP'
	WRITE (16,'(A)')   '----------------------------------'  
	DO I=1,PUMA1
	  WRITE (16,'(I7,3F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=15,17)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=15,17) 
C
      WRITE (16,'(/A)')  'WORKER SHOP BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA1  NH/W(H) NH/W(NH)     SHOP'
	WRITE (16,'(A)')   '----------------------------------'  
	DO I=1,PUMA1
	  WRITE (16,'(I7,3F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=18,20)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=18,20) 
C
      WRITE (16,'(/A)')  'NONWORKER HOME BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA1   SCHOOL    NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '-------------------------------------------'       
	DO I=1,PUMA1
	  WRITE (16,'(I7,4F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=21,24)  
	ENDDO
	
	WRITE (16,'(A,4F9.0)')  '  TOTAL', (REG_NM_PA(T),T=21,24) 
C
      WRITE (16,'(/A)')  'NONWORKER NONHOME AT HOUSEHOLDS BASED NONMOTOR 
     A TRIPS'
	WRITE (16,'(A)')   '  PUMA1   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA1
	  WRITE (16,'(I7,3F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=25,27)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=25,27) 
C
      WRITE (16,'(/A)')  'NONWORKER NONHOME NOT AT HOUSEHOLDS BASED NONM
     AOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA1   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA1
	  WRITE (16,'(I7,3F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=28,30)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=28,30) 
C
      WRITE (16,'(/A)')  'NONWORKER SHOP BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA1   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA1
	  WRITE (16,'(I7,3F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=31,33)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=31,33) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) HOME BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA1   SCHOOL    NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '-------------------------------------------'       
	DO I=1,PUMA1
	  WRITE (16,'(I7,4F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=34,37)  
	ENDDO
	
	WRITE (16,'(A,4F9.0)')  '  TOTAL', (REG_NM_PA(T),T=34,37) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) SCHOOL BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA1   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA1
	  WRITE (16,'(I7,3F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=38,40)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=38,40) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) NONHOME AT HOUSEHOLDS BASED NONMO
     ATOR TRIPS'
	WRITE (16,'(A)')   '  PUMA1   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA1
	  WRITE (16,'(I7,3F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=41,43)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=41,43) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) NONHOME NOT AT HOUSEHOLDS BASED N
     AONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA1   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA1
	  WRITE (16,'(I7,3F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=44,46)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=44,46) 

      WRITE (16,'(/A)')  'CHILD(12-15) SHOP BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA1   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA1
	  WRITE (16,'(I7,3F9.0)') P1_NUM(I), (P1_NM_PA(I,T),T=47,49)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=47,49) 
C***********************************************************************
C
	WRITE (16,'(/A)')  'FIVE PERCENT PUMAS'

	WRITE (16,'(/A)')  'WORKERS HOME BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA5 WKPL(<$) WKPL(>$) WORK REL   SCHOOL  N
     AH/W(H) NH/W(NH)     SHOP'   
	WRITE (16,'(A)')   '----------------------------------------------
     A------------------------'       
	DO I=1,PUMA5
	  WRITE (16,'(I7,7F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=1,7)
     	ENDDO
	
	WRITE (16,'(A,7F9.0)')  '  TOTAL', (REG_NM_PA(T),T=1,7) 
C
	WRITE (16,'(/A)')  'WORKER WORK BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA5  NH/W(H) NH/W(NH)     SHOP     WORK'   
	WRITE (16,'(A)')   '-------------------------------------------'       
	DO I=1,PUMA5
	  WRITE (16,'(I7,4F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=8,11)
	ENDDO

	WRITE (16,'(A,4F9.0)')  '  TOTAL', (REG_NM_PA(T),T=8,11) 
C
      WRITE (16,'(/A)')  'WORKER NONH/W AT HOUSEHOLDS BASED NONMOTOR TRI
     APS'
	WRITE (16,'(A)')   '  PUMA5  NH/W(H) NH/W(NH)     SHOP'
	WRITE (16,'(A)')   '----------------------------------'  
	DO I=1,PUMA5
	  WRITE (16,'(I7,3F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=12,14)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=12,14) 
C
      WRITE (16,'(/A)')  'WORKER NONH/W NOT AT HOUSEHOLDS BASED NONMOTOR
     A TRIPS'
	WRITE (16,'(A)')   '  PUMA5  NH/W(H) NH/W(NH)     SHOP'
	WRITE (16,'(A)')   '----------------------------------'  
	DO I=1,PUMA5
	  WRITE (16,'(I7,3F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=15,17)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=15,17) 
C
      WRITE (16,'(/A)')  'WORKER SHOP BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA5  NH/W(H) NH/W(NH)     SHOP'
	WRITE (16,'(A)')   '----------------------------------'  
	DO I=1,PUMA5
	  WRITE (16,'(I7,3F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=18,20)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=18,20) 
C
      WRITE (16,'(/A)')  'NONWORKER HOME BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA5   SCHOOL    NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '-------------------------------------------'       
	DO I=1,PUMA5
	  WRITE (16,'(I7,4F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=21,24)  
	ENDDO
	
	WRITE (16,'(A,4F9.0)')  '  TOTAL', (REG_NM_PA(T),T=21,24) 
C
      WRITE (16,'(/A)')  'NONWORKER NONHOME AT HOUSEHOLDS BASED NONMOTOR 
     A TRIPS'
	WRITE (16,'(A)')   '  PUMA5   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA5
	  WRITE (16,'(I7,3F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=25,27)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=25,27) 
C
      WRITE (16,'(/A)')  'NONWORKER NONHOME NOT AT HOUSEHOLDS BASED NONM
     AOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA5   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA5
	  WRITE (16,'(I7,3F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=28,30)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=28,30) 
C
      WRITE (16,'(/A)')  'NONWORKER SHOP BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA5   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA5
	  WRITE (16,'(I7,3F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=31,33)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=31,33) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) HOME BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA5   SCHOOL    NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '-------------------------------------------'       
	DO I=1,PUMA5
	  WRITE (16,'(I7,4F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=34,37)  
	ENDDO
	
	WRITE (16,'(A,4F9.0)')  '  TOTAL', (REG_NM_PA(T),T=34,37) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) SCHOOL BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA5   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA5
	  WRITE (16,'(I7,3F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=38,40)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=38,40) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) NONHOME AT HOUSEHOLDS BASED NONMO
     ATOR TRIPS'
	WRITE (16,'(A)')   '  PUMA5   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA5
	  WRITE (16,'(I7,3F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=41,43)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=41,43) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) NONHOME NOT AT HOUSEHOLDS BASED N
     AONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA5   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA5
	  WRITE (16,'(I7,3F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=44,46)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=44,46) 

      WRITE (16,'(/A)')  'CHILD(12-15) SHOP BASED NONMOTOR TRIPS'
	WRITE (16,'(A)')   '  PUMA5   NH(H)   NH(NH)     SHOP'   
	WRITE (16,'(A)')   '---------------------------------'  
	DO I=1,PUMA5
	  WRITE (16,'(I7,3F9.0)') P5_NUM(I), (P5_NM_PA(I,T),T=47,49)  
	ENDDO
	
	WRITE (16,'(A,3F9.0)')  '  TOTAL', (REG_NM_PA(T),T=47,49) 
C
 1002 CONTINUE 
C
C     OPTION TO OUTPUT FORMATED FILE OF NONMOTOR TRIPS
C
      IF (SAVE_FILE) THEN
C
C     FILE OF NONMOTOR TRIPS IN 49 EXPANDED TRIP CATEGORIES
C     IS UNIT 57  
C
        OPEN (UNIT=58,FILE='NONMOTOR_PA_OUT.TXT',STATUS='NEW',ERR=958)
	  
	  DO T=1,33
	    DO SZ = 1,SUBZONES

	      IF (SZ_PRODS(SZ,T) .GT. 0) THEN
	        FRACP = SZ_NM_PA(SZ,T)/(SZ_PRODS(SZ,T)+SZ_NM_PA(SZ,T))
            ELSE
	        FRACP = 0.0
		  ENDIF
		  
	      IF (SZ_ATTRS(SZ,T) .GT. 0) THEN
	        FRACA = SZ_NM_PA(SZ,T)/(SZ_ATTRS(SZ,T)+SZ_NM_PA(SZ,T))
            ELSE
	        FRACA = 0.0
		  ENDIF
C
            WRITE (58,'(2I6,I2,2(F9.1,F6.3))')  SZ, SZ_ZONE(SZ), T, 
     A        SZ_NM_PA(SZ,T), FRACP, SZ_NM_PA(SZ,T), FRACA
	    ENDDO
        ENDDO
C
        DO T=34,49
	    DO SZ = 1,SUBZONES

	      IF (SZ_PRODS(SZ,T) .GT. 0) THEN
	        FRACP = SZ_NM_PA(SZ,T)/(SZ_PRODS(SZ,T)+SZ_NM_PA(SZ,T))
            ELSE
	        FRACP = 0.0
		  ENDIF
		  
	      IF (SZ_ATTRS(SZ,T) .GT. 0) THEN
	        FRACA = SZ_NM_PA(SZ,T)/(SZ_ATTRS(SZ,T)+SZ_NM_PA(SZ,T))
            ELSE
	        FRACA = 0.0
		  ENDIF
C
            WRITE (58,'(2I6,I2,2(F9.1,F6.3))')  SZ, SZ_ZONE(SZ), T, 
     A        SZ_NM_PA(SZ,T), FRACP, SZ_NM_PA(SZ,T), FRACA
	    ENDDO
        ENDDO
        CLOSE (58,DISP='KEEP')
      ENDIF
C
      GO TO 999
C
  958 WRITE (*,'(A)') ' TROUBLE OPENING NONMOTOR_PA_OUT.TXT FILE' 
      STOP 958
C
  959 WRITE (*,'(A)') ' TROUBLE OPENING NONMOTOR_REVIEW_OUT.TXT FILE' 
      STOP 959   
C      
  999 CONTINUE
C
      WRITE (16,'(/A)') ' END OF TRIPGEN8'
      WRITE (*,'(/A)') ' END OF TRIPGEN8'
      
	RETURN
      END