      SUBROUTINE HWORK1
      IMPLICIT INTEGER (A-Z)
C***********************************************************************
C
C     THIS SUBROUTINE CONTROLS THE READING OF THE HOME-WORK TRIP TABLE
C     IN THE EMMEBANK WHEN THE MODE CHOICE MODEL RUN DOES NOT INCLUDE 
C     THE AUTO OCCUPANCY SUBMODEL
C
C***********************************************************************
      INCLUDE 'COMMON_EMME4BANK.FI'
      INCLUDE 'COMMON_PARAMS.FI'
      INCLUDE 'COMMON_DEST.FI'
      INCLUDE 'COMMON_OCC.FI'

      REAL*4 HWPROB(624), RAN_NUM
      REAL*8 POCC1, POCC2, POCC3, SUM_PROB, TRIP_ALL_OUT, TRIP_ALL
      REAL*8 T_1(4000), T_2(4000), T_3(4000)
      REAL*8 APTRIP_REG1, APTRIP_REG2, APTRIP_REG3
C# ## Heither, 11-12-2014: variable to track non-integer trip remainders     
      REAL*8 HW_LEFT, HWORK_CNTR      
      REAL*4 TRIP_ZONE(4000)
      REAL*8 WO1, WO2, WO3
       
C# ##      REWIND 32
      TRIP_ALL=0
C# ## Heither, 11-12-2014: add this counter to track trips written
C# ##         (integer or real)       
      HWORK_CNTR = 0            
      DO Z=1,ZONES
        TRIP_ZONE(Z) = 0
      ENDDO  
C
C     TTYPE = 1 WHEN HOME-WORK WITHOUT OCCUPANCY SUBMODES FROM MODE 
C             CHOICE
C
      TTYPE = 1
      APTRIP_REG1 = 0.0
      APTRIP_REG2 = 0.0
      APTRIP_REG3 = 0.0
            
      DO ORIG=BEGORIG,ENDORIG
        CALL DATA3(TTYPE,ORIG)
             
        WRITE (*,'(A,I6)') ' READ HOME-WORK TRIPS FOR ZONE=', ORIG 
C   
        DO Z=1,ZONES
          APTRIP_1(Z) = 0.0
          APTRIP_2(Z) = 0.0
          APTRIP_3(Z) = 0.0
          
          T_1(Z) = 0.0
          T_2(Z) = 0.0
          T_3(Z) = 0.0
        ENDDO
        
        DO DEST=1,ZONES
          TRIP_ALL = TRIP_ALL + APTRIP(DEST)
          TRIP_ZONE(ORIG) = TRIP_ZONE(ORIG) + APTRIP(DEST)
          
C        WRITE (16,'(A,2I6,F8.2)') '***', ORIG, DEST, APTRIP(DEST)  
C
C     CHECK FOR TRIP IN INTERCHANGE
C
          NT = 0
          IF (APTRIP(DEST) .GT. 0) THEN 
C
C     TRIP BY TRIP
C
            NT = APTRIP(DEST)
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
              
C                IF (ORIG .EQ. 629) THEN
C                  WRITE (16,'(I5,3F7.4,I5)')  HHT, RAN_NUM, 
C     A            HWPROB(HHT), HWPROB(HHT-1), PICK_HHT
C                ENDIF  
              ENDDO
C
C     CHECK TO SEE IF HH IS PICKED, POCC IS ALLOCATION TO VEHICLE
C     OCCUPANCY
C
              IF (PICK_HHT .GT. 0) THEN
                POCC1 = P_HW1(PICK_HHT)
                POCC2 = P_HW2(PICK_HHT)
                POCC3 = P_HW3(PICK_HHT)
C                WRITE (16,'(A,3F7.4)') '1', POCC1, POCC2, POCC3
C
C    IT IS POSSIBLE TO HAVE ALL HOUSEHOLD ENUMERATED FOR ZONE WITHOUT
C    ANY AUTO TRIPS, SO USE REGIONAL AVERAGES
C
              ELSE
                POCC1 = P_REG_HW1
                POCC2 = P_REG_HW2
                POCC3 = P_REG_HW3
C                WRITE (16,'(A,3F7.4)') '2', POCC1, POCC2, POCC3
              ENDIF
              
              SUM_PROB = POCC1 + POCC2 + POCC3
C# ##              TRIP_ALL_OUT = TRIP_ALL_OUT + SUM_PROB
C
C     FILL AUTO OCCUPANCY TRIP ARRAYS
C
              IF ((POCC1+POCC2+POCC3) .GT. 1.001) THEN
                WRITE (16,'(A,2I6,3F9.6)') ' TRIP PROBS .GT. 1 ', 
     A            ORIG, DEST, POCC1, POCC2, POCC3  
                STOP 312
              ENDIF  

              T_1(DEST) = T_1(DEST) + POCC1
              T_2(DEST) = T_2(DEST) + POCC2
              T_3(DEST) = T_3(DEST) + POCC3
              
C              WRITE (16,'(A,2I6,3F10.7)') ' BAD SUMMING? ', 
C     A          ORIG, DEST, POCC1, POCC2, POCC3
C              WRITE (16,'(4F12.5)') TRIP_ALL_OUT, APTRIP_REG1, 
C     A          APTRIP_REG2, APTRIP_REG3 
C              IF ((APTRIP_REG1+APTRIP_REG2+APTRIP_REG3) .GT. 
C     A          (TRIP_ALL_OUT+10)) STOP 313
C     
C     ACCUMULATE DESTINATION WORK-OTHER TRIP ENDS BY OCCUPANCY
C
              IF (PICK_HHT .GT. 0) THEN
                WO_DEST1(DEST) = WO_DEST1(DEST) +
     A            POCC1 * WO_TOT(PICK_HHT)/FLOAT(HW_TOT(PICK_HHT))
                WO_DEST2(DEST) = WO_DEST2(DEST) +
     A            POCC2 * WO_TOT(PICK_HHT)/FLOAT(HW_TOT(PICK_HHT))
                WO_DEST3(DEST) = WO_DEST3(DEST) +
     A            POCC3 * WO_TOT(PICK_HHT)/FLOAT(HW_TOT(PICK_HHT))
              ELSE
                WO_DEST1(DEST) = WO_DEST1(DEST) + POCC1 * WO_REG/HW_REG
                WO_DEST2(DEST) = WO_DEST2(DEST) + POCC2 * WO_REG/HW_REG
                WO_DEST3(DEST) = WO_DEST3(DEST) + POCC3 * WO_REG/HW_REG
              ENDIF  
C
C     DONE WITH ALL TRIPS IN INTERCHANGE
C
          ENDDO
C
C # ## ============================================================= ##
C # ## Heither, 11-12-2014: BLOCK ADDED TO PROCESS REMAINING TRIPS
C # ##			IN ZONE INTERCHANGE, IF STORED AS NON-INTEGERS
C # ## ------------------------------------------------------------- ##

          HW_LEFT = APTRIP(DEST) - NT
          IF (HW_LEFT .GT. 0) THEN
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
              
C                IF (ORIG .EQ. 629) THEN
C                  WRITE (16,'(I5,3F7.4,I5)')  HHT, RAN_NUM, 
C     A            HWPROB(HHT), HWPROB(HHT-1), PICK_HHT
C                ENDIF  
              ENDDO
C
C     CHECK TO SEE IF HH IS PICKED, POCC IS ALLOCATION TO VEHICLE
C     OCCUPANCY
C
              IF (PICK_HHT .GT. 0) THEN
                POCC1 = P_HW1(PICK_HHT) * HW_LEFT
                POCC2 = P_HW2(PICK_HHT) * HW_LEFT
                POCC3 = P_HW3(PICK_HHT) * HW_LEFT
C                WRITE (16,'(A,3F7.4)') '1', POCC1, POCC2, POCC3
C
C    IT IS POSSIBLE TO HAVE ALL HOUSEHOLD ENUMERATED FOR ZONE WITHOUT
C    ANY AUTO TRIPS, SO USE REGIONAL AVERAGES
C
              ELSE
                POCC1 = P_REG_HW1 * HW_LEFT
                POCC2 = P_REG_HW2 * HW_LEFT
                POCC3 = P_REG_HW3 * HW_LEFT
C                WRITE (16,'(A,3F7.4)') '2', POCC1, POCC2, POCC3
              ENDIF
              
              SUM_PROB = POCC1 + POCC2 + POCC3
C# ##              TRIP_ALL_OUT = TRIP_ALL_OUT + SUM_PROB
C
C     FILL AUTO OCCUPANCY TRIP ARRAYS
C
              T_1(DEST) = T_1(DEST) + POCC1
              T_2(DEST) = T_2(DEST) + POCC2
              T_3(DEST) = T_3(DEST) + POCC3             
C     
C     ACCUMULATE DESTINATION WORK-OTHER TRIP ENDS BY OCCUPANCY
C
              IF (PICK_HHT .GT. 0) THEN
                WO_DEST1(DEST) = WO_DEST1(DEST) +
     A            POCC1 * WO_TOT(PICK_HHT)/FLOAT(HW_TOT(PICK_HHT))
                WO_DEST2(DEST) = WO_DEST2(DEST) +
     A            POCC2 * WO_TOT(PICK_HHT)/FLOAT(HW_TOT(PICK_HHT))
                WO_DEST3(DEST) = WO_DEST3(DEST) +
     A            POCC3 * WO_TOT(PICK_HHT)/FLOAT(HW_TOT(PICK_HHT))
              ELSE
                WO_DEST1(DEST) = WO_DEST1(DEST) + POCC1 * WO_REG/HW_REG
                WO_DEST2(DEST) = WO_DEST2(DEST) + POCC2 * WO_REG/HW_REG
                WO_DEST3(DEST) = WO_DEST3(DEST) + POCC3 * WO_REG/HW_REG
              ENDIF  
C
C     DONE WITH ALL TRIPS IN INTERCHANGE
C
            ENDIF                
C # ## ============================================================= ##          
C
C     WRITE OUT HOME-WORK AUTO OCCUPANCY TABLES TRIPS>0
C
C            WRITE (16,'(3I8)') ORIG, DEST, NT
C            WRITE (16,'(3I6)') OUTTABLE_HW_1, OUTTABLE_HW_2, 
C     A        OUTTABLE_HW_3
C            WRITE (16,'(2I8, 4F8.5)') ORIG, DEST, 
C     A        APTRIP(DEST), APTRIP_1(DEST), APTRIP_2(DEST), 
C     B        APTRIP_3(DEST)
            APTRIP_1(DEST) = T_1(DEST)
            APTRIP_2(DEST) = T_2(DEST)
            APTRIP_3(DEST) = T_3(DEST)

C# ### Heither, CMAP 11-08-2013: modified for Emme4			
			REC1 = ((ORIG-1)*mcent) + DEST

C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_1(DEST)
            WRITE(913,REC=REC1) APTRIP_1(DEST)
            APTRIP_REG1 = APTRIP_REG1 + APTRIP_1(DEST)
C
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_2(DEST)
            WRITE(914,REC=REC1) APTRIP_2(DEST)
            APTRIP_REG2 = APTRIP_REG2 + APTRIP_2(DEST)
C          
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_3(DEST)
            WRITE(915,REC=REC1) APTRIP_3(DEST) 
            APTRIP_REG3 = APTRIP_REG3 + APTRIP_3(DEST)

          ELSE
            APTRIP_1(DEST) = 0.0
            APTRIP_2(DEST) = 0.0
            APTRIP_3(DEST) = 0.0
C
C     WRITE OUT HOME-WORK AUTO OCCUPANCY TABLES TRIPS=0
C
C            WRITE (16,'(3I8)') ORIG, DEST, NT
C            WRITE (16,'(3I6)') OUTTABLE_HW_1, OUTTABLE_HW_2, 
C     A        OUTTABLE_HW_3
C            WRITE (16,'(3F8.5)') APTRIP_1(DEST), APTRIP_2(DEST), 
C     A        APTRIP_3(DEST)
            REC1 = ((ORIG-1)*mcent) + DEST			

C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_1(DEST)
            WRITE(913,REC=REC1) APTRIP_1(DEST)
C
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_2(DEST)
            WRITE(914,REC=REC1) APTRIP_2(DEST)
C          
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_3(DEST)
            WRITE(915,REC=REC1) APTRIP_3(DEST)
            
          ENDIF
C
C     END OF ATTRACTION ZONES
C
        ENDDO
        WRITE (16,'(A,I5,A,F12.1)') ' TOTAL ZONE', ORIG,' HOME-WORK TRIP
     AS PROCESSED=', TRIP_ZONE(ORIG)
C
C     END OF PRODUCTION ZONES
C
      ENDDO
C
C     CHECK ATTRACTIONS
C
      DO Z=1,ZONES
        WO1 = WO1 + WO_DEST1(Z)
        WO2 = WO2 + WO_DEST2(Z)
        WO3 = WO3 + WO_DEST3(Z) 
      ENDDO
      
      HWORK_CNTR = APTRIP_REG1 + APTRIP_REG2 + APTRIP_REG3

      WRITE (16,'(/A)')  '             WORK OTHER ORIGINS'
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
     A=', TRIP_ALL
      WRITE (16,'(/A,F10.1)') ' TOTAL REGIONAL AUTO HOME-WORK TRIPS WRIT
     ATEN=', HWORK_CNTR
      
      WRITE (16,'(A,F10.1)') ' DRIVE ALONE HOME-WORK TRIPS WRITTEN=',
     A  APTRIP_REG1
      WRITE (16,'(A,F10.1)')' TWO PERSON AUTO HOME-WORK TRIPS WRITTEN=',
     A  APTRIP_REG2
      WRITE (16,'(A,F10.1)')' THREE OR MORE PERSON AUTO HOME-WORK TRIPS 
     AWRITTEN=', APTRIP_REG3
      
      RETURN
      END
