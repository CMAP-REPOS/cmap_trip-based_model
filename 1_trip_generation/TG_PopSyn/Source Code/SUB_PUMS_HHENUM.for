      SUBROUTINE SUB_PUMS_HHENUM
      IMPLICIT INTEGER(A-Z)
C***********************************************************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C***********************************************************************
C
C     THIS SUBROUTINE READS THE ACS PUMS HOUSEHOLD FILE INPUT FILE.  A
C     COMMA DELIMITED FILE WITH THE FOLLOWING VARIABLES:
C
C       1.  PUMA5 FOR HOUSEHOLD
C       2.  HHTYPE CODE FOR HOUSEHOLD
C       3.  HOUSEHOLD SERIAL NUMBER FROM PUMS FILE
C       4.  ACS PUMS WEIGHT FOR HOUSEHOLD
C 
C     SUBROUTINE FILLS ARRAYS FROM PUMS_HHENUM_IN.TXT FILE.  THEN 
C     RANDOMLY SELECTS PUMS HOUSEHOLDS MATCHING TG SUBZONE HOUSEHOLD
C     CHARACTERISTICS.
C
C     EASH, APRIL 2012
C
C***********************************************************************
      INCLUDE 'COMMON_GEOG.FI'
      INCLUDE 'COMMON_HHSUMS.FI'
      INCLUDE 'COMMON_PARAM.FI'
      INCLUDE 'COMMON_CONTROL.FI'
      INCLUDE 'COMMON_PUMSHHS.FI' 
           
      CHARACTER*13 SERIAL
      REAL*8  RP
      
      REAL*4  TWGT(624)
      
      OPEN (UNIT=31,FILE='PUMS_HHENUM_IN.TXT',STATUS='OLD',ERR=931)
      OPEN (UNIT=32,FILE='SZ_PUMS_HHS_OUT.TXT',FORM='FORMATTED',
     A  STATUS='NEW',ERR=932)
      
      COUNT=0
C
C     LOAD UP THE ARRAYS IN ORDER BY PUMA5 AND HHTYPE
C
    5 CONTINUE      
      READ (31,*,END=10)  P5,  H1, SERIAL, WEIGHT 
      COUNT = COUNT + 1
      REG_PROB_TO(COUNT) = FLOAT(WEIGHT)/FLOAT(MATRIX(H1))
      
C      WRITE (16,'(2I6,1X,A,I6)') P5, H1, SERIAL, WEIGHT
      
      IF (MOD(COUNT,1000) .EQ. 0) 
     A  WRITE (*,'(A,I8)') ' PUMS HH RECORDS READ= ', COUNT
      
C      DO I = 1,COUNT
C        WRITE (16,'(4I6,1X,A,I6)') I, COUNT, PUMS_PUMA5(I), 
C     A    PUMS_HHTYPE(I), PUMS_SERNO(I), PUMS_WGT(I)
C      ENDDO  
      
      DO REC = 1,COUNT
C
C     ADD RECORD AT BOTTOM OF LIST IF NOT LOCATED EARLIER, DONE
C
        IF (REC .EQ. COUNT) THEN
          PUMS_PUMA5(REC) = P5
          PUMS_HHTYPE(REC) = H1
          PUMS_SERNO(REC) = SERIAL
          PUMS_WGT(REC) = WEIGHT
          GO TO 5
        ENDIF
C
C     SLOT NOT FOUND YET (PUMA GREATER)
C
        IF (P5 .GT. PUMS_PUMA5(REC)) GO TO 9
        IF (P5 .EQ. PUMS_PUMA5(REC)) THEN
C
C     SLOT NOT FOUND YET (PUMA EQUAL BUT HHTYPE GREATER)
C          
          IF (H1. GT. PUMS_HHTYPE(REC)) GO TO 9
C
C     FOUND SLOT (PUMA EQUAL, LOWER OR EQUAL HHTYPE) SHUFFLE ALL
C     DOWN
C
          DO NEWREC = COUNT,REC+1,-1
            PUMS_PUMA5(NEWREC) = PUMS_PUMA5(NEWREC-1)
            PUMS_HHTYPE(NEWREC) = PUMS_HHTYPE(NEWREC-1)
            PUMS_SERNO(NEWREC) = PUMS_SERNO(NEWREC-1)
            PUMS_WGT(NEWREC) = PUMS_WGT(NEWREC-1)
          ENDDO
        
C          DO I = 1,COUNT
C            WRITE (16,'(4I6,1X,A,I6)') I, COUNT, PUMS_PUMA5(I), 
C     A        PUMS_HHTYPE(I), PUMS_SERNO(I), PUMS_WGT(I)
C          ENDDO          
C
C     ADD RECORD TO LIST, DONE 
C        
          PUMS_PUMA5(REC) = P5
          PUMS_HHTYPE(REC) = H1
          PUMS_SERNO(REC) = SERIAL
          PUMS_WGT(REC) = WEIGHT          
          GO TO 5
        ELSE
C
C     FOUND SLOT (PUMA LOWER) SHUFFLE ALL DOWN
C
          DO NEWREC = COUNT,REC+1,-1
            PUMS_PUMA5(NEWREC) = PUMS_PUMA5(NEWREC-1)
            PUMS_HHTYPE(NEWREC) = PUMS_HHTYPE(NEWREC-1)
            PUMS_SERNO(NEWREC) = PUMS_SERNO(NEWREC-1)
            PUMS_WGT(NEWREC) = PUMS_WGT(NEWREC-1)
          ENDDO
        
C          DO I = 1,COUNT
C            WRITE (16,'(4I6,1X,A,I6)') I, COUNT, PUMS_PUMA5(I), 
C     A        PUMS_HHTYPE(I), PUMS_SERNO(I), PUMS_WGT(I)
C          ENDDO          
C
C     ADD RECORD TO LIST, DONE 
C        
          PUMS_PUMA5(REC) = P5
          PUMS_HHTYPE(REC) = H1
          PUMS_SERNO(REC) = SERIAL
          PUMS_WGT(REC) = WEIGHT
          
          GO TO 5
        ENDIF
                
    9   CONTINUE     
      
      ENDDO
      
   10 CONTINUE  
      WRITE (*,'(/A,I8/)') 'TOTAL PUMS HH RECORDS READ= ', COUNT
      WRITE (16,'(/A,I8/)') 'TOTAL PUMS HH RECORDS READ= ', COUNT   
C
C     CALCULATE REGIONAL PROBABILITY FOR HOUSEHOLDS
C
      DO HHT= 1,624
        II = 0  
        DO REC = 1,COUNT
          IF (PUMS_HHTYPE(REC) .EQ. HHT) THEN
            II = II + 1  
            IF (II .EQ. 1) THEN
              REG_PROB_FROM(REC) = 0.0000
              RP = REG_PROB_TO(REC)
            ELSE
              REG_PROB_FROM(REC) = RP
              REG_PROB_TO(REC) = RP + REG_PROB_TO(REC)
              RP = REG_PROB_TO(REC)
            ENDIF
          ENDIF
        ENDDO
      ENDDO 
C      
C      DO REC = 1,COUNT
C        IF (PUMS_HHTYPE(REC) .EQ. 4) 
C     A    WRITE (16,'(I8,2F12.8)') REC, REG_PROB_FROM(REC),
C     B      REG_PROB_TO(REC)  
C      ENDDO
C      
C      DO REC = 1,200
C        WRITE (16,'(3I8,1X,A,I6)') REC, PUMS_PUMA5(REC), 
C     A    PUMS_HHTYPE(REC), PUMS_SERNO(REC), PUMS_WGT(REC)
C      ENDDO
C
C     DETERMINE STARTING POSITIONS OF PUMAS AND HHTYPES IN ARRAYS
C
      PUMA_FROM(1) = 1
      PUMA_TO(1) = 1
      I = 1
      
      HHT = PUMS_HHTYPE(1)
      HHTYPE_FROM(1,HHT) = 1
      HHTYPE_TO(1,HHT) = 1
      
      DO REC = 2,COUNT
        IF (PUMS_PUMA5(REC) .NE. PUMS_PUMA5(REC-1)) THEN
          I = I+1   
          PUMA_FROM(I) = REC
          PUMA_TO(I) = REC
          HHT = PUMS_HHTYPE(REC)
          HHTYPE_FROM(I,HHT) = REC
          HHTYPE_TO(I,HHT) = REC
        ELSE
          IF (PUMS_HHTYPE(REC) .NE. PUMS_HHTYPE(REC-1)) THEN
            PUMA_TO(I) = REC
            HHT = PUMS_HHTYPE(REC)
            HHTYPE_FROM(I,HHT) = REC
            HHTYPE_TO(I,HHT) = REC
          ELSE
            PUMA_TO(I) = REC
            HHTYPE_TO(I,HHT) = REC 
          ENDIF  
        ENDIF  
      ENDDO
C      
C      DO I = 1,PUMA5
C        WRITE (16,'(3I10)')  I, PUMA_FROM(I), PUMA_TO(I)
C      ENDDO
C      
C      DO I=1,10
C        DO HHT=1,624
C          IF (HHTYPE_FROM(I,HHT) .GT. 0)   
C     A      WRITE (16,'(3I10)') HHT,HHTYPE_FROM(I,HHT),HHTYPE_TO(I,HHT)
C        ENDDO
C      ENDDO  
C
C     CALCULATE HHTYPE PROBABILITIES WITHIN PUMAS
C
      DO I = 1,100
        IF (PUMA_FROM(I) .NE. 0) THEN
               
          DO HHT = 1,624
            
            TWGT(HHT) = 0  
            DO REC = HHTYPE_FROM(I,HHT), HHTYPE_TO(I,HHT)
              IF (HHTYPE_FROM(I,HHT) .NE. 0) 
     A          TWGT(HHT) = TWGT(HHT) + PUMS_WGT(REC)
            ENDDO 
            
            DO REC = HHTYPE_FROM(I,HHT), HHTYPE_TO(I,HHT)
              IF (HHTYPE_FROM(I,HHT) .NE. 0) 
     A          PUMS_PROB(REC) = FLOAT(PUMS_WGT(REC))/TWGT(HHT)
            ENDDO 
C
C     ACCUMULATE PROBABILITIES
C
            DO REC = HHTYPE_FROM(I,HHT)+1, HHTYPE_TO(I,HHT)
              IF (HHTYPE_FROM(I,HHT) .NE. 0) 
     A          PUMS_PROB(REC) = PUMS_PROB(REC) + PUMS_PROB(REC-1)
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C      
C      DO I = 1,5
C        DO HHT = 1,624
C          IF (HHTYPE_FROM(I,HHT) .NE. 0) THEN
C            WRITE (16,'(4I10)')  I, HHT, HHTYPE_FROM(I,HHT),
C     A        HHTYPE_TO(I,HHT)
C            DO REC = HHTYPE_FROM(I,HHT), HHTYPE_TO(I,HHT)
C              WRITE (16,'(2I10,1X,A,I5,F11.8)') PUMS_PUMA5(REC),
C     A          PUMS_HHTYPE(REC), PUMS_SERNO(REC), PUMS_WGT(REC), 
C     B          PUMS_PROB(REC) 
C            ENDDO
C          ENDIF
C        ENDDO
C      ENDDO  
C
C     SELECT HOUSEHOLDS MATCHING TG SUBZONE HOUSEHOLDS
C
      ALL_HH = 0

      DO SZ=1,SUBZONES
        WRITE (*,'(A,I8)') 'FINDING PUMS HHS FOR SUBZONE= ', SZ      
        WRITE (16,'(A,I8)') 'FINDING PUMS HHS FOR SUBZONE= ', SZ  
          
	  DO J=1,PUMA5
	    IF (SZ_PUMA5(SZ) .EQ. P5_NUM(J)) I = J
        ENDDO  
         
        DO HHT=1,624  
          NUMHH = ISZ_MATRIX(SZ,HHT)
C
C     HAVE HHS OF THIS TYPE IN THIS SZ?
C
          IF (NUMHH .GT. 0) THEN
C
C     HAVE HHS OF THIS TYPE IN PUMA HH SAMPLE?
C
            IF (HHTYPE_FROM(I,HHT) .GT. 0) THEN
               
              DO IHH = 1,NUMHH
            
C                WRITE (16,'(A)') 'PUMA'
C                WRITE (16,'(8I8)') SZ, SZ_PUMA5(SZ), P5_NUM(J), I, J, 
C     A            HHT, NUMHH, IHH
                
                CALL RANDOM(RAN_NUM)
                REC = HHTYPE_FROM(I,HHT)
                IF (RAN_NUM .LE. PUMS_PROB(REC)) THEN 
                  ALL_HH = ALL_HH + 1  
                  WRITE (32,'(3I8,A)') SZ, SZ_PUMA5(SZ), HHT, 
     A              PUMS_SERNO(REC) 
                ENDIF  
                
                DO REC = HHTYPE_FROM(I,HHT)+1, HHTYPE_TO(I,HHT)
                  IF ((RAN_NUM .GT. PUMS_PROB(REC-1)) .AND.
     A              (RAN_NUM .LE. PUMS_PROB(REC))) THEN 
                    ALL_HH = ALL_HH + 1  
                    WRITE (32,'(3I8,A)') SZ, SZ_PUMA5(SZ), HHT, 
     A                PUMS_SERNO(REC)
                  ENDIF  
                ENDDO
              ENDDO  
C
C     HHTYPE NOT IN PUMA HH SAMPLE, LOOK FOR HH IN REGION'S HHS
C
            ELSE
              
              DO IHH = 1,NUMHH
                  
C                WRITE (16,'(A)') 'REGION'            
C                WRITE (16,'(8I8)') SZ, SZ_PUMA5(SZ), P5_NUM(J), I, J, 
C     A            HHT, NUMHH, IHH
                
                CALL RANDOM(RAN_NUM)
                DO REC = 1,COUNT
                
                  IF (HHT .EQ. PUMS_HHTYPE(REC)) THEN  
                    IF ((RAN_NUM .GT. REG_PROB_FROM(REC)) .AND.
     A                (RAN_NUM .LE. REG_PROB_TO(REC))) THEN
                      ALL_HH = ALL_HH + 1  
                      WRITE (32,'(3I8,A)') SZ, SZ_PUMA5(SZ), HHT, 
     A                  PUMS_SERNO(REC)
                    ENDIF  
                  ENDIF  
                ENDDO
              ENDDO
C
C           PUMA OR REGIONAL HHS ENDIF
C
            ENDIF
C
C           HHS OF THIS TYPE ENDIF
C
          ENDIF
C
C       END HHTYPE LOOP
C
        ENDDO
C
C     END TG SUBZONE LOOP
C
      ENDDO  
      
      WRITE (*,'(/A,I9)')  'TOTAL HOUSEHOLDS ENUMERATED= ',ALL_HH 
      WRITE (16,'(/A,I9)') 'TOTAL HOUSEHOLDS ENUMERATED= ',ALL_HH 
C
      WRITE (16,'(/A)') 'END OF PUMS_HHENUM'
      WRITE (*,'(/A)') 'END OF PUMS_HHENUM'
            
      RETURN
      
  931 WRITE (*,'(/A)') 'ERROR:  PROBLEM OPENING FILE PUMS_HHENUM_IN.TXT'
      STOP 931
  932 WRITE (*,'(/A)') 'ERROR:  PROBLEM OPENING FILE SZ_PUMS_HHS_OUT.TXT
     A'
      STOP 932
      END