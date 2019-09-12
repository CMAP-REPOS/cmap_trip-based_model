      SUBROUTINE NONHOME
      IMPLICIT INTEGER (A-Z)
C***********************************************************************
C
C     THIS SUBROUTINE CONTROLS THE READING OF THE NONHOME TRIP TABLE
C     IN THE EMMEBANK
C
C***********************************************************************
      INCLUDE 'COMMON_EMME4BANK.FI'
      INCLUDE 'COMMON_PARAMS.FI'
      INCLUDE 'COMMON_DEST.FI'
      INCLUDE 'COMMON_OCC.FI'
      
      REAL*8 POCC1, POCC2, POCC3, TRIP_ALL_OUT, TRIP_ALL
      REAL*8 APTRIP_REG1, APTRIP_REG2, APTRIP_REG3
      REAL*8 NH_TOTAL(4000)
      REAL*8 TRIP_ZONE(4000)
       
C# ##      REWIND 32
      TRIP_ALL=0
      DO Z=1,ZONES
        TRIP_ZONE(Z) = 0
        NH_TOTAL(Z) = NH_TOTAL(Z) + WO_DEST1(Z) + OO_DEST1(Z) + 
     A    WO_DEST2(Z) + OO_DEST2(Z) + WO_DEST3(Z) + OO_DEST3(Z)   
      ENDDO
C
C     TTYPE = 3 WHEN NONHOME
C
      TTYPE = 3
      APTRIP_REG1 = 0.0
      APTRIP_REG2 = 0.0
      APTRIP_REG3 = 0.0
            
      DO ORIG=BEGORIG,ENDORIG
        CALL DATA3(TTYPE,ORIG)
             
        WRITE (*,'(A,I6)') ' READ NONHOME TRIPS FOR ZONE=', ORIG 
C   
        DO Z=1,ZONES
          APTRIP_1(Z) = 0.0
          APTRIP_2(Z) = 0.0
          APTRIP_3(Z) = 0.0
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
C# ## Heither, 11-10-2014: MODIFY LOGIC TO ELIMINATE POSSIBILITY OF NaN               
            IF ((NH_TOTAL(ORIG).GT.0) .AND. (NH_TOTAL(DEST).GT.0)) THEN
                
              POCC1 =
     A         (WO_DEST1(ORIG)+OO_DEST1(ORIG))/(2.0*NH_TOTAL(ORIG)) +
     B           (WO_DEST1(DEST)+OO_DEST1(DEST))/(2.0*NH_TOTAL(DEST))
              
              POCC2 =      
     A         (WO_DEST2(ORIG)+OO_DEST2(ORIG))/(2.0*NH_TOTAL(ORIG)) +
     B           (WO_DEST2(DEST)+OO_DEST2(DEST))/(2.0*NH_TOTAL(DEST))
              
              POCC3 =      
     A         (WO_DEST3(ORIG)+OO_DEST3(ORIG))/(2.0*NH_TOTAL(ORIG)) +
     B           (WO_DEST3(DEST)+OO_DEST3(DEST))/(2.0*NH_TOTAL(DEST))
            
C              IF (ORIG .LE. 10 ) WRITE (16,'(A,3F7.4)') '1', 
C     A          POCC1, POCC2, POCC3
C
            ELSE IF (NH_TOTAL(ORIG).GT.0) THEN
                
              POCC1 =
     A         (WO_DEST1(ORIG)+OO_DEST1(ORIG))/(2.0*NH_TOTAL(ORIG))
              
              POCC2 =      
     A         (WO_DEST2(ORIG)+OO_DEST2(ORIG))/(2.0*NH_TOTAL(ORIG))
              
              POCC3 =      
     A         (WO_DEST3(ORIG)+OO_DEST3(ORIG))/(2.0*NH_TOTAL(ORIG))
C
            ELSE IF (NH_TOTAL(DEST).GT.0) THEN

              POCC1 =
     A           (WO_DEST1(DEST)+OO_DEST1(DEST))/(2.0*NH_TOTAL(DEST))
              
              POCC2 =      
     A           (WO_DEST2(DEST)+OO_DEST2(DEST))/(2.0*NH_TOTAL(DEST))
              
              POCC3 =      
     A           (WO_DEST3(DEST)+OO_DEST3(DEST))/(2.0*NH_TOTAL(DEST))
C                                                   
C    IT IS POSSIBLE TO HAVE ALL NHUSENHLD ENUMERATED FOR ZONE WITHOUT
C    ANY AUTO TRIPS, SO USE REGIONAL AVERAGES
C
            ELSE
              POCC1 = P_REG_OO1
              POCC2 = P_REG_OO2
              POCC3 = P_REG_OO3
C              IF (ORIG .LE. 10 ) WRITE (16,'(A,3F7.4)') '2', 
C     A         POCC1, POCC2, POCC3
            ENDIF
 
            APTRIP_1(DEST) = POCC1*APTRIP(DEST)
            APTRIP_2(DEST) = POCC2*APTRIP(DEST)
            APTRIP_3(DEST) = POCC3*APTRIP(DEST)
            
            TRIP_ALL_OUT = TRIP_ALL_OUT+APTRIP_1(DEST)+APTRIP_2(DEST)+
     A        APTRIP_3(DEST)
	 
C# ### Heither, CMAP 11-08-2013: modified for Emme4
            REC1 = ((ORIG-1)*mcent) + DEST
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_1(DEST)
            WRITE(919,REC=REC1) APTRIP_1(DEST)            
            APTRIP_REG1 = APTRIP_REG1 + APTRIP_1(DEST)
C          
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_2(DEST)
            WRITE(920,REC=REC1) APTRIP_2(DEST)            
            APTRIP_REG2 = APTRIP_REG2 + APTRIP_2(DEST)
C           
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_3(DEST)
            WRITE(921,REC=REC1) APTRIP_3(DEST)             
            APTRIP_REG3 = APTRIP_REG3 + APTRIP_3(DEST)

          ELSE
            APTRIP_1(DEST) = 0.0
            APTRIP_2(DEST) = 0.0
            APTRIP_3(DEST) = 0.0

            REC1 = ((ORIG-1)*mcent) + DEST
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_1(DEST)
            WRITE(919,REC=REC1) APTRIP_1(DEST)
C
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_2(DEST)
            WRITE(920,REC=REC1) APTRIP_2(DEST)
C          
C            WRITE (16,'(I16,F8.5)')  REC1, APTRIP_3(DEST)
            WRITE(921,REC=REC1) APTRIP_3(DEST)
            
          ENDIF
C
C     END OF ATTRACTION ZONES
C
        ENDDO
        WRITE (16,'(A,I5,A,F12.1)')   ' TOTAL ZONE', ORIG,' NONHOME TRIPS P
     AROCESSED=', TRIP_ZONE(ORIG)
C
C     END OF PRODUCTION ZONES
C
      ENDDO
       
      WRITE (16,'(/A,F12.1)')' TOTAL REGIONAL AUTO NONHOME TRIPS READ=',
     A  TRIP_ALL
      WRITE (16,'(/A,F12.1)')' TOTAL REGIONAL AUTO NONHOME TRIPS WRITTEN
     A=', TRIP_ALL_OUT
      
      WRITE (16,'(A,F12.1)') ' DRIVE ALONE NONHOME TRIPS WRITTEN=',
     A  APTRIP_REG1
      WRITE (16,'(A,F12.1)')' TWO PERSON AUTO NONHOME TRIPS WRITTEN='
     A,  APTRIP_REG2
      WRITE (16,'(A,F12.1)')' THREE OR MORE PERSON AUTO NONHOME TRIPS WR
     AITTEN=', APTRIP_REG3
      
      RETURN
      END
