      SUBROUTINE SUB_TAZ_HHTYPE
      IMPLICIT INTEGER(A-Z)
C***********************************************************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C***********************************************************************
C
C     REVISED VERSTION OF TRIPGEN1 TO FILL TAZ LEVEL HOUSEHOLD TYPE
C     TABLES WITH INITIAL EXPECTED VALUES.
C
C     EASH, APRIL 2012
C
C***********************************************************************
      INCLUDE 'COMMON_GEOG.FI'
	INCLUDE 'COMMON_PARAM.FI'
      INCLUDE 'COMMON_CONTROL.FI'
	INCLUDE 'COMMON_HHSUMS.FI'
      INCLUDE 'ROW_COL.FI'
      INCLUDE 'COMMON_POPSYN.FI'
      
      REAL*8 SZ_MATRIX(25000,624)
      
      REAL*8 AROW(4), WROW(4), CROW(4), IROW(4), HHROW(3)
      REAL*8 AWROW(4,4), TOTAL
C
      REAL*8 AIND, WIND, CIND, IIND, HHIND, AUTOMS, PEF
    
      REAL*8 ROWSUM(25000), COLSUM(624), ERRCOL(624), ERRROW(25000),
     A MAX_ERRCOL, MAX_ERRROW, SZ_HH_TOT
      
      REAL*8 TEST
      
      REAL*8 REAL_MATRIX(624), REAL_ADULT(4), REAL_WORKER(4), 
     A  REAL_CHILD(4), REAL_INC4(4), REAL_HHOLDER(3)

C ## -- Heither, 05-13-2016: Following added for PopSyn procedures--          
      INTEGER :: PSHHT, PSVEHICL, PSSERL

C
C     TRIP GENERATION INPUT FILE FROM TG DATABASE IS UNIT 20 
      OPEN (UNIT=20,FILE='HH_IN.TXT',STATUS='OLD',ERR=920)
C
C ## -- Heither, 05-13-2016: POPSYN_HH.CSV USED IN PLACE OF IPF PROCESS   
      OPEN (UNIT=79,FILE="POPSYN_HH.CSV",STATUS='OLD',
     A READONLY)
C      
C     READ INPUT FILE AND COMPARE HOUSEHOLD TOTALS
C
      TOTAL_HH_IN = 0.0
    3 CONTINUE       
      READ (20,*,END=6) SZ, HH 
      TOTAL_HH_IN = TOTAL_HH_IN + HH
      GO TO 3
    6 CONTINUE
C
	WRITE (16,'(/A,I9)') 'TOTAL HOUSEHOLDS READ FROM HH_IN.TXT= ', 
     A  TOTAL_HH_IN      

C ## -- Heither, 05-13-2016: comment out this HH totals check for now --      
C      IF (TOTAL_HH_IN .NE. HH1_TOT) GO TO 996
C
C ################################################################################
C --------------------------------------------------------------------------------
C ## -- Heither, 05-13-2016: --  
C ## --   ORIGINAL CODE BLOCK FOR IPF REMOVED BUT KEEP SOME NECESSARY CODE --
C ## --   GET PEF & AUTOMS FROM HH_IN.TXT --
	REWIND 20
      DO
          READ (20,*,END=198) SZ, HH, AIND, WIND, CIND, IIND, HHIND, 
     A AUTOMS, PEF
      
          IF (SZ .GT. SUBZONES) GO TO 995
          IF ((AIND .LT. 1.00) .AND. (HH .GT. 0)) THEN
              WRITE (*,'(/A,I6)')  'WARNING: ADULTS PER HH .LT. 1.0', SZ
              WRITE (16,'(/A,I6)')  'WARNING: ADULTS PER HH.LT.1.0', SZ
          ENDIF
          SZ_AUTOMS(SZ) = AUTOMS
          SZ_PEF(SZ) = PEF
      ENDDO	   
  198 CONTINUE       

C ## -- Heither, 05-13-2016: USE DATA IN POPSYN_HH.CSV TO POPULATE ENUMERATED --         
C ## --   HH MATRIX.  THIS REPLACES EASH'S IPF PROCESS. --     

      POPSYNID = 0
C ## -- BEGIN PROCESSING POPSYN HOUSEHOLD FILE -- 
C ## -- CREATE MATRIX OF INTEGER HOUSEHOLDS: SZ,HHT -- 
C ## -- REQUIRES POPSYN_HH.CSV TO BE SORTED BY SUBZONE & HHTYPE --           
      DO
	    READ (79,*,END=210) SZ, PSHHT, PSVEHICL, PSSERL  
		
          ISZ_MATRIX(SZ,PSHHT) = ISZ_MATRIX(SZ,PSHHT) + 1
		POPSYNID = POPSYNID + 1
          
          PSVEH(POPSYNID) = PSVEHICL
          PSSERIAL(POPSYNID) = PSSERL
          
C ## -- LOGIC MODIFIED TO WORK WITH POPSYN_HH.CSV --            
C     STORE HOUSEHOLDS FOR LATER USE
C     ALSO USED LATER IN ATTRACTION CALCULATIONS
C
          SZ_HH(SZ) = SZ_HH(SZ) + 1                   
      ENDDO		
  210    PRINT *, 'END OF POPSYN FILE REACHED'
		WRITE (16,'(/A,I9)') 'TOTAL HOUSEHOLDS READ FROM POPSYN_HH.CSV= ', 
     A  POPSYNID

C ################################################################################
C --------------------------------------------------------------------------------
C ## -- Heither, 05-13-2016: PROCESS TO CONVERT NUMBERS IN REAL SUBZONE MATRIX --
C ## --   TO INTEGER MATRIX NO LONGER NEEDED, SO CODE BLOCK REMOVED -- 
      DO SZ = 1,SUBZONES
        DO HHT = 1,624       
          ISZ_HHT(HHT) = ISZ_HHT(HHT) + ISZ_MATRIX(SZ,HHT)    
        ENDDO 
      ENDDO 
C
C --------------------------------------------------------------------------------
C ################################################################################              
          
      WRITE (16,'(/A)') 'FINAL ENUMERATED HOUSEHOLDS'
      
      WRITE (16,'(/A)')  '                REGIONAL     TAZ'
      WRITE (16,'(A)')   ' HHT  PUMS HHS     HHS       HHS'
      WRITE (16,'(A)')   '----------------------------------'
      DO HHT=1,624
        TSUM = 0
        DO RC = 1,4
          TSUM = TSUM + PUMS_HH(HHT,RC)
        ENDDO  
        WRITE (16,'(I4,3I10)') HHT, TSUM, MATRIX(HHT),
     A    ISZ_HHT(HHT)
      ENDDO
      
      T1 = 0
      T2 = 0
      T3 = 0
            
      DO HHT=1,624
        DO RC = 1,4  
          T1 = T1 + PUMS_HH(HHT,RC)
        ENDDO  
        T2 = T2 + MATRIX(HHT)
        T3 = T3 + ISZ_HHT(HHT)
      ENDDO
      
      WRITE (16,'(A,3I10)') ' TOT', T1, T2, T3
C
      WRITE (16,'(/A)') 'END OF TAZ_HHTYPE'
      WRITE (*,'(/A)') 'END OF TAZ_HHTYPE'
       
      RETURN
      
  920 WRITE (*,'(/A)')  'ERROR:  PROBLEM OPENING FILE HH_IN.TXT'
      STOP 920     
  995 WRITE (*,'(/A)') 'ERROR;  TG SUBZONE OUT OF RANGE IN HH_IN.TXT'
	STOP 995
  996 WRITE (*,'(/A)') 'ERROR;  HH_IN.TXT HOUSEHOLDS NOT EQUAL TO CONTRO
     AL TOTAL HOUSEHOLDS'
	STOP 996

      END