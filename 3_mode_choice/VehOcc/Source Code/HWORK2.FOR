      SUBROUTINE HWORK2
      IMPLICIT INTEGER (A-Z)
C***********************************************************************
C
C     THIS SUBROUTINE CONTROLS THE READING OF THE HOME-WORK TRIP TABLE
C     IN THE EMMEBANK WHEN THE MODE CHOICE MODEL RUN DOES INCLUDES 
C     THE AUTO OCCUPANCY SUBMODEL
C
C***********************************************************************
      INCLUDE 'COMMON_EMME4BANK.FI'
      INCLUDE 'COMMON_PARAMS.FI'
      INCLUDE 'COMMON_DEST.FI'
      INCLUDE 'COMMON_OCC.FI'

      REAL*4 HWPROB(624), RAN_NUM
      REAL*8 POCC1, POCC2, POCC3, SUM_PROB, TRIP_ALL
      REAL*8 T_1(4000), T_2(4000), T_3(4000)
      REAL*8 APTRIP_REG1, APTRIP_REG2, APTRIP_REG3
      REAL*8 TRIP_ZONE(4000)
      REAL*8 WO1, WO2, WO3
      REAL*8 HWORK2_CNTR
       
C# ##      REWIND 32
      TRIP_ALL=0
C# ## Heither, 11-06-2014: add this counter to track trips written
C# ##         (integer or real)       
      HWORK2_CNTR = 0
      DO Z=1,ZONES
        TRIP_ZONE(Z) = 0
      ENDDO  
C
C     TTYPE = 10 WHEN HOME-WORK WITH OCCUPANCY SUBMODES FROM MODE 
C             CHOICE BUT WITHOUT HOME-WORK WORKER INCOME SEGMENTATION
C
C     TTYPE = 100 WHEN HOME-WORK WITH OCCUPANCY SUBMODES FROM MODE 
C             CHOICE AND HOME-WORK WORKER INCOME SEGMENTATION
C
      TTYPE = 10
      IF (WORKER$) TTYPE = 100
      APTRIP_REG1 = 0.0
      APTRIP_REG2 = 0.0
      APTRIP_REG3 = 0.0
            
      DO ORIG=BEGORIG,ENDORIG
        CALL DATA3(TTYPE,ORIG)
           
        WRITE (*,'(A,I6)') ' READ HOME-WORK TRIPS FOR ZONE=', ORIG 
C   
        DO Z=1,ZONES
          T_1(Z) = 0.0
          T_2(Z) = 0.0
          T_3(Z) = 0.0
        ENDDO
        
        DO DEST=1,ZONES
            
          APTRIP(DEST) = APTRIP_1(DEST) + APTRIP_2(DEST) + 
     A      APTRIP_3(DEST)  
          TRIP_ALL = TRIP_ALL + APTRIP(DEST)
          TRIP_ZONE(ORIG) = TRIP_ZONE(ORIG) + APTRIP(DEST)
          
C        WRITE (16,'(A,2I6,F8.2)') '***', ORIG, DEST, APTRIP(DEST)  
C
C     CHECK FOR DRIVE ALONE TRIP IN INTERCHANGE
C
          NT = 0
          IF (APTRIP_1(DEST) .GT. 0) THEN 
C
C     TRIP BY TRIP
C
            NT = APTRIP_1(DEST)
            DO T=1,NT  
C
C     PICK HOUSEHOLD TYPE
C             
              DO HHT=1,624
                IF (HW_ZONE(ORIG) .GT. 0) THEN  
                  HWPROB(HHT) = HW_TRIPS(ORIG,HHT)/HW_ZONE(ORIG)
                ELSE
                  HWPROB(HHT) = 0.0
                ENDIF  
              ENDDO
            
              DO HHT=2,624
                HWPROB(HHT) = HWPROB(HHT) + HWPROB(HHT-1)
              ENDDO  
              
              CALL RANDOM(RAN_NUM)
            
              PICK_HHT = 0
              IF (RAN_NUM .LE. HWPROB(1)) PICK_HHT = 1
              DO HHT=2,624
                IF ((RAN_NUM.GT.HWPROB(HHT-1)) .AND.
     A            (RAN_NUM.LE.HWPROB(HHT))) PICK_HHT = HHT  
              ENDDO
C     
C     ACCUMULATE DESTINATION WORK-OTHER TRIP ENDS BY OCCUPANCY
C
              IF (PICK_HHT .GT. 0) THEN
                WO_DEST1(DEST) = WO_DEST1(DEST) + 
     A            WO_TOT(PICK_HHT)/FLOAT(HW_TOT(PICK_HHT))
              ELSE
                WO_DEST1(DEST) = WO_DEST1(DEST) + WO_REG/HW_REG
              ENDIF  
C
C     DONE WITH ALL DRIVE ALONE TRIPS IN INTERCHANGE
C
            ENDDO                
C           
C     WRITE OUT DRIVE ALONE HOME-WORK AUTO OCCUPANCY TABLES TRIPS>0
C# ### Heither, CMAP 11-08-2013: modified for Emme4
            REC1 = ((ORIG-1)*mcent) + DEST
			
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_1(DEST)
            WRITE(913,REC=REC1) APTRIP_1(DEST)
            
            APTRIP_REG1 = APTRIP_REG1 + APTRIP_1(DEST)
          ELSE
            APTRIP_1(DEST) = 0.0
            REC1 = ((ORIG-1)*mcent) + DEST
			
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_1(DEST)
            WRITE(913,REC=REC1) APTRIP_1(DEST)
          ENDIF
C
C     CHECK FOR TWO PERSON TRIP IN INTERCHANGE
C
          NT = 0
          IF (APTRIP_2(DEST) .GT. 0) THEN 
C
C     TRIP BY TRIP
C
            NT = APTRIP_2(DEST)
            DO T=1,NT  
C
C     PICK HOUSEHOLD TYPE
C             
              DO HHT=1,624
                IF (HW_ZONE(ORIG) .GT. 0) THEN  
                  HWPROB(HHT) = HW_TRIPS(ORIG,HHT)/HW_ZONE(ORIG)
                ELSE
                  HWPROB(HHT) = 0.0
                ENDIF  
              ENDDO
            
              DO HHT=2,624
                HWPROB(HHT) = HWPROB(HHT) + HWPROB(HHT-1)
              ENDDO  
              
              CALL RANDOM(RAN_NUM)
            
              PICK_HHT = 0
              IF (RAN_NUM .LE. HWPROB(1)) PICK_HHT = 1
              DO HHT=2,624
                IF ((RAN_NUM.GT.HWPROB(HHT-1)) .AND.
     A            (RAN_NUM.LE.HWPROB(HHT))) PICK_HHT = HHT  
              ENDDO
C     
C     ACCUMULATE DESTINATION WORK-OTHER TRIP ENDS BY OCCUPANCY
C
              IF (PICK_HHT .GT. 0) THEN
                WO_DEST2(DEST) = WO_DEST2(DEST) + 
     A            WO_TOT(PICK_HHT)/FLOAT(HW_TOT(PICK_HHT))
              ELSE
                WO_DEST2(DEST) = WO_DEST2(DEST) + WO_REG/HW_REG
              ENDIF  
C
C     DONE WITH ALL TWO PERSON TRIPS IN INTERCHANGE
C
            ENDDO                
C
C     WRITE OUT TWO PERSON HOME-WORK AUTO OCCUPANCY TABLES TRIPS>0
C
            REC1 = ((ORIG-1)*mcent) + DEST
			
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_1(DEST)
            WRITE(914,REC=REC1) APTRIP_2(DEST)
            
            APTRIP_REG2 = APTRIP_REG2 + APTRIP_2(DEST)
          ELSE
            APTRIP_2(DEST) = 0.0
            REC1 = ((ORIG-1)*mcent) + DEST
			
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_1(DEST)
            WRITE(914,REC=REC1) APTRIP_2(DEST)
          ENDIF
C
C     CHECK FOR THREE OR MORE PERSON TRIP IN INTERCHANGE
C
          NT = 0
          IF (APTRIP_3(DEST) .GT. 0) THEN 
C
C     TRIP BY TRIP
C
            NT = APTRIP_3(DEST)
            DO T=1,NT  
C
C     PICK HOUSEHOLD TYPE
C             
              DO HHT=1,624
                IF (HW_ZONE(ORIG) .GT. 0) THEN  
                  HWPROB(HHT) = HW_TRIPS(ORIG,HHT)/HW_ZONE(ORIG)
                ELSE
                  HWPROB(HHT) = 0.0
                ENDIF  
              ENDDO
            
              DO HHT=2,624
                HWPROB(HHT) = HWPROB(HHT) + HWPROB(HHT-1)
              ENDDO  
              
              CALL RANDOM(RAN_NUM)
            
              PICK_HHT = 0
              IF (RAN_NUM .LE. HWPROB(1)) PICK_HHT = 1
              DO HHT=2,624
                IF ((RAN_NUM.GT.HWPROB(HHT-1)) .AND.
     A            (RAN_NUM.LE.HWPROB(HHT))) PICK_HHT = HHT  
              ENDDO
C     
C     ACCUMULATE DESTINATION WORK-OTHER TRIP ENDS BY OCCUPANCY
C
              IF (PICK_HHT .GT. 0) THEN
                WO_DEST3(DEST) = WO_DEST3(DEST) + 
     A            WO_TOT(PICK_HHT)/FLOAT(HW_TOT(PICK_HHT))
              ELSE
                WO_DEST3(DEST) = WO_DEST3(DEST) + WO_REG/HW_REG
              ENDIF  
C
C     DONE WITH ALL THREE OR MORE PERSON TRIPS IN INTERCHANGE
C
            ENDDO                
C
C     WRITE OUT THREE OR MORE PERSON HOME-WORK AUTO OCCUPANCY 
C     TABLES TRIPS>0
C
            REC1 = ((ORIG-1)*mcent) + DEST
			
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_1(DEST)
            WRITE(915,REC=REC1) APTRIP_3(DEST)
            
            APTRIP_REG3 = APTRIP_REG3 + APTRIP_3(DEST)
          ELSE
            APTRIP_3(DEST) = 0.0
            REC1 = ((ORIG-1)*mcent) + DEST
			
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_1(DEST)
            WRITE(915,REC=REC1) APTRIP_3(DEST)
          ENDIF          
C
C     END OF ATTRACTION ZONES
C
        ENDDO
        WRITE (16,'(A,I5,A,F10.1)') ' TOTAL ZONE', ORIG,' HOME-WORK TRIP
     AS PROCESSED=', TRIP_ZONE(ORIG)
C
C     END OF PRODUCTION ZONES
C
      ENDDO
      
C# ## Heither, 11-06-2014: Counter for trips written      
      HWORK2_CNTR = APTRIP_REG1 + APTRIP_REG2 + APTRIP_REG3
C
C     CHECK ATTRACTIONS
C
      DO Z=1,ZONES
        WO1 = WO1 + WO_DEST1(Z)
        WO2 = WO2 + WO_DEST2(Z)
        WO3 = WO3 + WO_DEST3(Z) 
      ENDDO  

      WRITE (16,'(/A)')  '              WORK OTHER ORIGINS'
      WRITE (16,'(/A)')  '                AUTO OCCUPANCY' 
      WRITE (16,'( A)')  '          --------------------------' 
      WRITE (16,'( A)')  '  ZONE     1 PER     2 PER    3+ PER'
      WRITE (16,'( A)')  '------------------------------------' 
      DO Z=1,ZONES
        WRITE (16,'(I6,3F10.1)') Z, WO_DEST1(Z), WO_DEST2(Z), 
     A    WO_DEST3(Z) 
      ENDDO
      
      WRITE (16,'(A,3F10.1)') ' TOTAL', WO1, WO2, WO3         
      
      WRITE (16,'(/A,F10.1)') ' TOTAL REGIONAL AUTO HOME-WORK TRIPS READ
     A=',  TRIP_ALL
      WRITE (16,'(/A,F10.1)') ' TOTAL REGIONAL AUTO HOME-WORK TRIPS WRIT
     ATEN=', HWORK2_CNTR
      
      WRITE (16,'(A,F10.1)') ' DRIVE ALONE HOME-WORK TRIPS WRITTEN=',
     A  APTRIP_REG1
      WRITE (16,'(A,F10.1)')' TWO PERSON AUTO HOME-WORK TRIPS WRITTEN=',
     A  APTRIP_REG2
      WRITE (16,'(A,F10.1)')' THREE OR MORE PERSON AUTO HOME-WORK TRIPS 
     AWRITTEN=', APTRIP_REG3
      
      RETURN
      END
