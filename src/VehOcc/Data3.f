      SUBROUTINE DATA3(TTYPE,ORIG)
      IMPLICIT INTEGER (A-Z)
C*******************
C  THIS SUBROUTINE PROCESSES THE EMME MATRICES
C     IT IS CALLED ONCE FOR EACH ZONE SELECTED FOR ANALYSIS
C
C*******************
	INCLUDE 'COMMON_PARAMS.FI'
	INCLUDE 'COMMON_EMME4BANK.FI'

      REAL*4 TEST, RAN_NUM
      
C#      REWIND 32
C***********************************************************************
C
C     READ HOME-WORK TABLES BY ROW
C# ### Heither, CMAP 11-07-2013: modified for Emme4
C***********************************************************************
	P = ORIG
      
      DO Z=1,ZONES
        APTRIP(Z) = 0.0  
        APTRIP_1(Z) = 0.0  
        APTRIP_2(Z) = 0.0  
        APTRIP_3(Z) = 0.0  
      ENDDO  
C
C     TTYPE = 1 FOR HOV=FALSE, LOW_INC=FALSE, AND HIGH_INC=FALSE IN
C     MODE CHOICE MODEL
C
      IF (TTYPE .EQ. 1) THEN
	  DO Q=1,ZONES
C
C     P TO Q DIRECTION
C       
          REC1 = ((P-1)*mcent) + Q
          READ(901, REC=REC1) APTRIP(Q)
        ENDDO
      ENDIF  
C
C     TTYPE = 10 FOR HOV=TRUE, LOW_INC=FALSE, AND HIGH_INC=FALSE IN
C     MODE CHOICE MODEL
C
      IF (TTYPE .EQ. 10) THEN
	  DO Q=1,ZONES
C
C     P TO Q DIRECTION
C       
          REC1 = ((P-1)*mcent) + Q
          READ(902, REC=REC1) APTRIP_1(Q)
          READ(903, REC=REC1) APTRIP_2(Q)
          READ(904, REC=REC1) APTRIP_3(Q)
         ENDDO	
      ENDIF
C
C     TTYPE = 100 FOR HOV=TRUE, LOW_INC=TRUE AND HIGH_INC=TRUE IN
C     MODE CHOICE MODEL
C
      IF (TTYPE .EQ. 100) THEN
	  DO Q=1,ZONES
C
C     P TO Q DIRECTION
C       
          REC1 = ((P-1)*mcent) + Q   
          READ(905,REC=REC1) APTRIP_HW_L(Q)
          READ(906, REC=REC1) APTRIP_HW_H(Q)         
          APTRIP_1(Q) = APTRIP_HW_L(Q) + APTRIP_HW_H(Q)
C       
          READ(907, REC=REC1) APTRIP_HW_L(Q)
          READ(908, REC=REC1) APTRIP_HW_H(Q)          
          APTRIP_2(Q) = APTRIP_HW_L(Q) + APTRIP_HW_H(Q)
C       
          READ(909, REC=REC1) APTRIP_HW_L(Q)
          READ(910, REC=REC1) APTRIP_HW_H(Q)
          APTRIP_3(Q) = APTRIP_HW_L(Q) + APTRIP_HW_H(Q)                
        ENDDO	
      ENDIF        
C***********************************************************************
C
C     READ HOME-OTHER TABLES BY ROW (TTYPE=2)
C# ### Heither, CMAP 11-07-2013: modified for Emme4
C***********************************************************************
      IF (TTYPE .EQ. 2) THEN 
	  DO Q=1,ZONES
C
C     P TO Q DIRECTION
C       
          REC1 = ((P-1)*mcent) + Q
          READ(911, REC=REC1) APTRIP(Q)
        ENDDO
      ENDIF  
C***********************************************************************
C
C     READ NON-HOME TABLES BY ROW (TTYPE=3)
C# ### Heither, CMAP 11-07-2013: modified for Emme4
C***********************************************************************
      IF (TTYPE .EQ. 3) THEN	  
	  DO Q=1,ZONES
C
C     P TO Q DIRECTION
C       
          REC1 = ((P-1)*mcent) + Q
          READ(912, REC=REC1) APTRIP(Q)
        ENDDO	
      ENDIF  
C
C     FOLLOWING CODE TO TRACE ERRORS
C
C      WRITE(16,'(/I6)') ORIG
C      WRITE(16,'(A)') '     1     2     3     4     5     6     7     8 
C     A    9   10'
C      WRITE(16,'(10F6.1)') (APTRIP(Q), Q=1,100)
C      WRITE(16,'(A)') '     1     2     3     4     5     6     7     8 
C     A    9   10'
C      WRITE(16,'(10F6.1)') (APTRIP(Q), Q=1901,2000)

      RETURN
      END