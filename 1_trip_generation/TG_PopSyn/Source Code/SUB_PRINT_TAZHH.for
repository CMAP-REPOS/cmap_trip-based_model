      SUBROUTINE SUB_PRINT_TAZHH
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
      INCLUDE 'COMMON_PARAM.FI'      
      INCLUDE 'COMMON_GEOG.FI'
	INCLUDE 'COMMON_HHSUMS.FI'
      
      REAL*4 ROW_FRAC(4), AROW_TOT
C
C     SUM SUBZONE HOUSEHOLD CATEGORIES FROM INTEGER MATRIX
C
      DO SZ = 1,SUBZONES
        
        DO J=1,COUNTIES
	    IF (SZ_CO(SZ) .EQ. CO_NUM(J)) CO_INDEX = J
	  ENDDO

	  DO J=1,PUMA1
	    IF (SZ_PUMA1(SZ) .EQ. P1_NUM(J)) PUMA1_INDEX = J
        ENDDO

	  DO J=1,PUMA5
	    IF (SZ_PUMA5(SZ) .EQ. P5_NUM(J)) PUMA5_INDEX = J
        ENDDO  
C        
C        WRITE (16,'(3I6)') CO_INDEX, PUMA1_INDEX, PUMA5_INDEX
C        
        DO HHT=1,624
                 
          I = HHT_ADULT(HHT) 
          ISZ_MAT_ADULT(SZ,I) = ISZ_MAT_ADULT(SZ,I)+ISZ_MATRIX(SZ,HHT)
          TOT_ADULT(I) = TOT_ADULT(I) + ISZ_MATRIX(SZ,HHT)
          CO_HH_ADULT(CO_INDEX,I) = CO_HH_ADULT(CO_INDEX,I) + 
     A      ISZ_MATRIX(SZ,HHT)
          
          IF (SZ_CHI(SZ) .EQ. 1)
     A      CHI_HH_ADULT(I) = CHI_HH_ADULT(I) + ISZ_MATRIX(SZ,HHT)
          
          IF (SZ_CBD(SZ) .EQ. 1)
     A      CBD_HH_ADULT(I) = CBD_HH_ADULT(I) + ISZ_MATRIX(SZ,HHT)
          
           PUMA1_HH_ADULT(PUMA1_INDEX,I)=PUMA1_HH_ADULT(PUMA1_INDEX,I)+ 
     A      ISZ_MATRIX(SZ,HHT)
          PUMA5_HH_ADULT(PUMA5_INDEX,I)=PUMA5_HH_ADULT(PUMA5_INDEX,I)+ 
     A      ISZ_MATRIX(SZ,HHT)
      
          I = HHT_WORKER(HHT) + 1
          ISZ_MAT_WORKER(SZ,I) = ISZ_MAT_WORKER(SZ,I)+ISZ_MATRIX(SZ,HHT)
          TOT_WORKER(I) = TOT_WORKER(I) + ISZ_MATRIX(SZ,HHT)
          CO_HH_WORKER(CO_INDEX,I) = CO_HH_WORKER(CO_INDEX,I) + 
     A      ISZ_MATRIX(SZ,HHT)
          
          IF (SZ_CHI(SZ) .EQ. 1)
     A      CHI_HH_WORKER(I) = CHI_HH_WORKER(I) + ISZ_MATRIX(SZ,HHT) 
          
          IF (SZ_CBD(SZ) .EQ. 1)
     A      CBD_HH_WORKER(I) = CBD_HH_WORKER(I) + ISZ_MATRIX(SZ,HHT)
          
          PUMA1_HH_WORKER(PUMA1_INDEX,I)=PUMA1_HH_WORKER(PUMA1_INDEX,I)+
     A      ISZ_MATRIX(SZ,HHT)
          PUMA5_HH_WORKER(PUMA5_INDEX,I)=PUMA5_HH_WORKER(PUMA5_INDEX,I)+
     A      ISZ_MATRIX(SZ,HHT)
      
          I = HHT_CHILD(HHT) + 1 
          ISZ_MAT_CHILD(SZ,I) = ISZ_MAT_CHILD(SZ,I)+ISZ_MATRIX(SZ,HHT)
          TOT_CHILD(I) = TOT_CHILD(I) + ISZ_MATRIX(SZ,HHT)
          CO_HH_CHILD(CO_INDEX,I) = CO_HH_CHILD(CO_INDEX,I) +      
     A      ISZ_MATRIX(SZ,HHT)
          
          IF (SZ_CHI(SZ) .EQ. 1)
     A      CHI_HH_CHILD(I) = CHI_HH_CHILD(I) + ISZ_MATRIX(SZ,HHT)
          
          IF (SZ_CBD(SZ) .EQ. 1)
     A      CBD_HH_CHILD(I) = CBD_HH_CHILD(I) + ISZ_MATRIX(SZ,HHT)
                              
          PUMA1_HH_CHILD(PUMA1_INDEX,I)=PUMA1_HH_CHILD(PUMA1_INDEX,I)+
     A      ISZ_MATRIX(SZ,HHT)
          PUMA5_HH_CHILD(PUMA5_INDEX,I)=PUMA5_HH_CHILD(PUMA5_INDEX,I)+
     A      ISZ_MATRIX(SZ,HHT)

          I = HHT_INC4(HHT)
          ISZ_MAT_INC4(SZ,I) = ISZ_MAT_INC4(SZ,I)+ISZ_MATRIX(SZ,HHT)
          TOT_INCOME(I) = TOT_INCOME(I) + ISZ_MATRIX(SZ,HHT)
          CO_HH_INCOME(CO_INDEX,I) = CO_HH_INCOME(CO_INDEX,I) +      
     A      ISZ_MATRIX(SZ,HHT)
          
          IF (SZ_CHI(SZ) .EQ. 1)
     A      CHI_HH_INCOME(I) = CHI_HH_INCOME(I) + ISZ_MATRIX(SZ,HHT)
          
          IF (SZ_CBD(SZ) .EQ. 1)
     A      CBD_HH_INCOME(I) = CBD_HH_INCOME(I) + ISZ_MATRIX(SZ,HHT)
          
          PUMA1_HH_INCOME(PUMA1_INDEX,I)=PUMA1_HH_INCOME(PUMA1_INDEX,I)+
     A      ISZ_MATRIX(SZ,HHT)
          PUMA5_HH_INCOME(PUMA5_INDEX,I)=PUMA5_HH_INCOME(PUMA5_INDEX,I)+
     A      ISZ_MATRIX(SZ,HHT)

          I = HHT_HHOLDER(HHT)
          ISZ_MAT_HHOLDER(SZ,I) = ISZ_MAT_HHOLDER(SZ,I)+
     A      ISZ_MATRIX(SZ,HHT)
          TOT_HHOLDER(I) = TOT_HHOLDER(I) + ISZ_MATRIX(SZ,HHT)
          CO_HH_HHOLDER(CO_INDEX,I) = CO_HH_HHOLDER(CO_INDEX,I) +      
     A      ISZ_MATRIX(SZ,HHT)
          
          IF (SZ_CHI(SZ) .EQ. 1)
     A      CHI_HH_HHOLDER(I) = CHI_HH_HHOLDER(I) + ISZ_MATRIX(SZ,HHT) 
          
          IF (SZ_CBD(SZ) .EQ. 1)
     A      CBD_HH_HHOLDER(I) = CBD_HH_HHOLDER(I) + ISZ_MATRIX(SZ,HHT)
          
          PUMA1_HH_HHOLDER(PUMA1_INDEX,I) =
     A      PUMA1_HH_HHOLDER(PUMA1_INDEX,I) + ISZ_MATRIX(SZ,HHT)
          PUMA5_HH_HHOLDER(PUMA5_INDEX,I) =
     A      PUMA5_HH_HHOLDER(PUMA5_INDEX,I) + ISZ_MATRIX(SZ,HHT)
          
        ENDDO
      ENDDO  
C
C     SUMMARIES
C     PRINT ROW AND COLUMN TOTALS FOR CHECKING, COUNTIES
C
	WRITE (16,'(//A)') 'ESTIMATED BASE HOUSEHOLDS'
	WRITE (16,'(/A)')  'COUNTIES'

	WRITE (16,'(/A)')  ' COUNTY       1 ADULT      2 ADULTS      3 ADU
     ALTS     4+ ADULTS    TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A--------------------------'       

      DO I=1,COUNTIES
	  ROW_TOT = CO_HH_ADULT(I,1) + CO_HH_ADULT(I,2) + 
     A    CO_HH_ADULT(I,3) + CO_HH_ADULT(I,4)
        AROW_TOT = ROW_TOT
        
        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J) = CO_HH_ADULT(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(I7,5(I9,F5.2))') CO_NUM(I), 
     A    (CO_HH_ADULT(I,J),ROW_FRAC(J),J=1,4), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_ADULT(1)+TOT_ADULT(2)+TOT_ADULT(3)+TOT_ADULT(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = TOT_ADULT(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL', 
     A  (TOT_ADULT(I), ROW_FRAC(I), I=1,4), ROW_TOT
      
	ROW_TOT = CHI_HH_ADULT(1) + CHI_HH_ADULT(2) + CHI_HH_ADULT(3) + 
     A  CHI_HH_ADULT(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = CHI_HH_ADULT(J)/AROW_TOT
	ENDDO	 

	WRITE (16,'(/A,5(I9,F5.2))') 'CHICAGO', 
     A  (CHI_HH_ADULT(I), ROW_FRAC(I), I=1,4), ROW_TOT

	ROW_TOT = CBD_HH_ADULT(1) + CBD_HH_ADULT(2) + CBD_HH_ADULT(3) + 
     A  CBD_HH_ADULT(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = CBD_HH_ADULT(J)/AROW_TOT
	ENDDO	 

	WRITE (16,'(A,5(I9,F5.2))') 'CHI CBD', 
     A  (CBD_HH_ADULT(I), ROW_FRAC(I), I=1,4), ROW_TOT
C	
      WRITE (16,'(/A)')  ' COUNTY      0 WORKER      1 WORKER     2 WORK
     AERS    3+ WORKERS    TOTAL'  
	WRITE (16,'(A)')   '----------------------------------------------
     A--------------------------'

      DO I=1,COUNTIES
	  ROW_TOT = CO_HH_WORKER(I,1) + CO_HH_WORKER(I,2) + 
     A    CO_HH_WORKER(I,3) + CO_HH_WORKER(I,4)
        AROW_TOT = ROW_TOT

        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J) = CO_HH_WORKER(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(I7,5(I9,F5.2))') CO_NUM(I), 
     A    (CO_HH_WORKER(I,J),ROW_FRAC(J),J=1,4), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_WORKER(1) + TOT_WORKER(2) + TOT_WORKER(3) + 
     A  TOT_WORKER(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = TOT_WORKER(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL', 
     A  (TOT_WORKER(I), ROW_FRAC(I), I=1,4), ROW_TOT

	ROW_TOT = CHI_HH_WORKER(1) + CHI_HH_WORKER(2) + CHI_HH_WORKER(3) + 
     A  CHI_HH_WORKER(4)
      AROW_TOT = ROW_TOT
      
      DO J=1,4
	  ROW_FRAC(J) = CHI_HH_WORKER(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(/A,5(I9,F5.2))') 'CHICAGO', 
     A  (CHI_HH_WORKER(I), ROW_FRAC(I), I=1,4), ROW_TOT

	ROW_TOT = CBD_HH_WORKER(1) + CBD_HH_WORKER(2) + CBD_HH_WORKER(3) + 
     A  CBD_HH_WORKER(4)
      AROW_TOT = ROW_TOT
      
      DO J=1,4
	  ROW_FRAC(J) = CBD_HH_WORKER(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') 'CHI CBD', 
     A  (CBD_HH_WORKER(I), ROW_FRAC(I), I=1,4), ROW_TOT
C	
      WRITE (16,'(/A)')  ' COUNTY       0 CHILD       1 CHILD       2 CH
     AILD      3+ CHILD    TOTAL' 
     	WRITE (16,'(A)')   '----------------------------------------------
     A--------------------------'

      DO I=1,COUNTIES
	  ROW_TOT = CO_HH_CHILD(I,1) + CO_HH_CHILD(I,2) + 
     A    CO_HH_CHILD(I,3) + CO_HH_CHILD(I,4)
        AROW_TOT = ROW_TOT

        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J) = CO_HH_CHILD(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(I7,5(I9,F5.2))') CO_NUM(I), 
     A    (CO_HH_CHILD(I,J),ROW_FRAC(J),J=1,4), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_CHILD(1)+TOT_CHILD(2)+TOT_CHILD(3)+TOT_CHILD(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = TOT_CHILD(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL', 
     A  (TOT_CHILD(I), ROW_FRAC(I), I=1,4), ROW_TOT

	ROW_TOT = CHI_HH_CHILD(1) + CHI_HH_CHILD(2) + CHI_HH_CHILD(3) +
     A  CHI_HH_CHILD(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = CHI_HH_CHILD(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(/A,5(I9,F5.2))') 'CHICAGO', 
     A  (CHI_HH_CHILD(I), ROW_FRAC(I), I=1,4), ROW_TOT

	ROW_TOT = CBD_HH_CHILD(1) + CBD_HH_CHILD(2) + CBD_HH_CHILD(3) +
     A  CBD_HH_CHILD(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = CBD_HH_CHILD(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') 'CHI CBD', 
     A  (CBD_HH_CHILD(I), ROW_FRAC(I), I=1,4), ROW_TOT
C	
	WRITE (16,'(/A)')  ' COUNTY      INCOME 1      INCOME 2      INCOM
     AE 3      INCOME 4    TOTAL'  
     	WRITE (16,'(A)')   '----------------------------------------------
     A--------------------------'

      DO I=1,COUNTIES
	  ROW_TOT = CO_HH_INCOME(I,1) + CO_HH_INCOME(I,2) + 
     A    CO_HH_INCOME(I,3) + CO_HH_INCOME(I,4)
        AROW_TOT = ROW_TOT
               
        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J) = CO_HH_INCOME(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(I7,5(I9,F5.2))') CO_NUM(I), 
     A    (CO_HH_INCOME(I,J),ROW_FRAC(J),J=1,4), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_INCOME(1) + TOT_INCOME(2) + TOT_INCOME(3) + 
     A  TOT_INCOME(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = TOT_INCOME(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL', 
     A  (TOT_INCOME(I), ROW_FRAC(I), I=1,4), ROW_TOT

	ROW_TOT = CHI_HH_INCOME(1) + CHI_HH_INCOME(2) + CHI_HH_INCOME(3) + 
     A  CHI_HH_INCOME(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = CHI_HH_INCOME(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(/A,5(I9,F5.2))') 'CHICAGO', 
     A  (CHI_HH_INCOME(I), ROW_FRAC(I), I=1,4), ROW_TOT

	ROW_TOT = CBD_HH_INCOME(1) + CBD_HH_INCOME(2) + CBD_HH_INCOME(3) + 
     A  CBD_HH_INCOME(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = CBD_HH_INCOME(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') 'CHI CBD', 
     A  (CBD_HH_INCOME(I), ROW_FRAC(I), I=1,4), ROW_TOT
C	
	WRITE (16,'(/A)')  ' COUNTY   HHOLDER <35 HHOLDER 35-64   HHOLDER 
     A65+    TOTAL'  
     	WRITE (16,'(A)')   '----------------------------------------------
     A------------'

      DO I=1,COUNTIES
	  ROW_TOT = CO_HH_HHOLDER(I,1) + CO_HH_HHOLDER(I,2) + 
     A    CO_HH_HHOLDER(I,3)
        AROW_TOT = ROW_TOT

        DO J=1,3
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J) = CO_HH_HHOLDER(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(I7,4(I9,F5.2))') CO_NUM(I), 
     A    (CO_HH_HHOLDER(I,J),ROW_FRAC(J),J=1,3), ROW_TOT 
      ENDDO    

	ROW_TOT = TOT_HHOLDER(1) + TOT_HHOLDER(2) + TOT_HHOLDER(3)
      AROW_TOT = ROW_TOT

      DO J=1,3
	  ROW_FRAC(J) = TOT_HHOLDER(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,4(I9,F5.2))') '  TOTAL', 
     A  (TOT_HHOLDER(I), ROW_FRAC(I), I=1,3), ROW_TOT

	ROW_TOT = CHI_HH_HHOLDER(1)+CHI_HH_HHOLDER(2)+CHI_HH_HHOLDER(3)
      AROW_TOT = ROW_TOT

      DO J=1,3
	  ROW_FRAC(J) = CHI_HH_HHOLDER(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(/A,4(I9,F5.2))') 'CHICAGO', 
     A  (CHI_HH_HHOLDER(I), ROW_FRAC(I), I=1,3), ROW_TOT

	ROW_TOT = CBD_HH_HHOLDER(1)+CBD_HH_HHOLDER(2)+CBD_HH_HHOLDER(3)
      AROW_TOT = ROW_TOT

      DO J=1,3
	  ROW_FRAC(J) = CBD_HH_HHOLDER(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,4(I9,F5.2))') 'CHI CBD', 
     A  (CBD_HH_HHOLDER(I), ROW_FRAC(I), I=1,3), ROW_TOT
C
C     REPEAT FOR ONE PERCENT PUMS
C
	WRITE (16,'(/A)')  'ONE PERCENT PUMAS'

	WRITE (16,'(/A)')  '  PUMA1       1 ADULT      2 ADULTS      3 ADU
     ALTS     4+ ADULTS    TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A--------------------------'       

      DO I=1,PUMA1
	  ROW_TOT = PUMA1_HH_ADULT(I,1) + PUMA1_HH_ADULT(I,2) + 
     A    PUMA1_HH_ADULT(I,3) + PUMA1_HH_ADULT(I,4)
        AROW_TOT = ROW_TOT

        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J) = PUMA1_HH_ADULT(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(I7,5(I9,F5.2))') P1_NUM(I), 
     A    (PUMA1_HH_ADULT(I,J),ROW_FRAC(J),J=1,4), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_ADULT(1)+TOT_ADULT(2)+TOT_ADULT(3)+TOT_ADULT(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = TOT_ADULT(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL', 
     A  (TOT_ADULT(I), ROW_FRAC(I), I=1,4), ROW_TOT
C	
      WRITE (16,'(/A)')  '  PUMA1      0 WORKER      1 WORKER     2 WORK
     AERS    3+ WORKERS    TOTAL'  
	WRITE (16,'(A)')   '----------------------------------------------
     A--------------------------'

      DO I=1,PUMA1
	  ROW_TOT = PUMA1_HH_WORKER(I,1) + PUMA1_HH_WORKER(I,2) + 
     A    PUMA1_HH_WORKER(I,3) + PUMA1_HH_WORKER(I,4)
        AROW_TOT = ROW_TOT

        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J)=PUMA1_HH_WORKER(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(I7,5(I9,F5.2))') P1_NUM(I), 
     A    (PUMA1_HH_WORKER(I,J),ROW_FRAC(J),J=1,4), ROW_TOT 
      ENDDO    

	ROW_TOT = TOT_WORKER(1) + TOT_WORKER(2) + TOT_WORKER(3) + 
     A  TOT_WORKER(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = TOT_WORKER(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL', 
     A  (TOT_WORKER(I), ROW_FRAC(I), I=1,4), ROW_TOT
		
      WRITE (16,'(/A)')  '  PUMA1       0 CHILD       1 CHILD       2 CH
     AILD      3+ CHILD    TOTAL' 
     	WRITE (16,'(A)')   '----------------------------------------------
     A--------------------------'

      DO I=1,PUMA1
	  ROW_TOT = PUMA1_HH_CHILD(I,1) + PUMA1_HH_CHILD(I,2) + 
     A    PUMA1_HH_CHILD(I,3) + PUMA1_HH_CHILD(I,4)
        AROW_TOT = ROW_TOT

        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J)=PUMA1_HH_CHILD(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(I7,5(I9,F5.2))') P1_NUM(I), 
     A    (PUMA1_HH_CHILD(I,J),ROW_FRAC(J),J=1,4), ROW_TOT 
      ENDDO    

	ROW_TOT = TOT_CHILD(1)+TOT_CHILD(2)+TOT_CHILD(3)+TOT_CHILD(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = TOT_CHILD(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL', 
     A  (TOT_CHILD(I), ROW_FRAC(I), I=1,4), ROW_TOT
C	
	WRITE (16,'(/A)')  '  PUMA1      INCOME 1      INCOME 2      INCOM
     AE 3      INCOME 4    TOTAL'  
     	WRITE (16,'(A)')   '----------------------------------------------
     A--------------------------'

      DO I=1,PUMA1
	  ROW_TOT = PUMA1_HH_INCOME(I,1) + PUMA1_HH_INCOME(I,2) + 
     A    PUMA1_HH_INCOME(I,3) + PUMA1_HH_INCOME(I,4)
        AROW_TOT = ROW_TOT

        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J)=PUMA1_HH_INCOME(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(I7,5(I9,F5.2))') P1_NUM(I), 
     A    (PUMA1_HH_INCOME(I,J),ROW_FRAC(J),J=1,4), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_INCOME(1) + TOT_INCOME(2) + TOT_INCOME(3) + 
     A  TOT_INCOME(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = TOT_INCOME(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL', 
     A  (TOT_INCOME(I), ROW_FRAC(I), I=1,4), ROW_TOT
C	
	WRITE (16,'(/A)')  '  PUMA1   HHOLDER <35 HHOLDER 35-64   HHOLDER 
     A65+    TOTAL'  
     	WRITE (16,'(A)')   '----------------------------------------------
     A------------'
 
      DO I=1,PUMA1
	  ROW_TOT = PUMA1_HH_HHOLDER(I,1) + PUMA1_HH_HHOLDER(I,2) + 
     A    PUMA1_HH_HHOLDER(I,3)
        AROW_TOT = ROW_TOT

        DO J=1,3
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J)=PUMA1_HH_HHOLDER(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(I7,4(I9,F5.2))') P1_NUM(I), 
     A    (PUMA1_HH_HHOLDER(I,J),ROW_FRAC(J),J=1,3), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_HHOLDER(1) + TOT_HHOLDER(2) + TOT_HHOLDER(3)
      AROW_TOT = ROW_TOT

      DO J=1,3
	  ROW_FRAC(J) = TOT_HHOLDER(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,4(I9,F5.2))') '  TOTAL', 
     A  (TOT_HHOLDER(I), ROW_FRAC(I), I=1,3), ROW_TOT      
C
C     REPEAT FOR FIVE PERCENT PUMS
C
	WRITE (16,'(/A)')  'FIVE PERCENT PUMAS'

	WRITE (16,'(/A)')  '  PUMA5  PUMA1       1 ADULT      2 ADULTS    
     A  3 ADULTS     4+ ADULTS    TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'       

      DO I=1,PUMA5
	  ROW_TOT = PUMA5_HH_ADULT(I,1) + PUMA5_HH_ADULT(I,2) + 
     A    PUMA5_HH_ADULT(I,3) + PUMA5_HH_ADULT(I,4)
        AROW_TOT = ROW_TOT

        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J)=PUMA5_HH_ADULT(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(2I7,5(I9,F5.2))') P5_NUM(I), P5_PUMA1(I), 
     A    (PUMA5_HH_ADULT(I,J),ROW_FRAC(J),J=1,4), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_ADULT(1)+TOT_ADULT(2)+TOT_ADULT(3)+TOT_ADULT(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = TOT_ADULT(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL       ', 
     A  (TOT_ADULT(I), ROW_FRAC(I), I=1,4), ROW_TOT
C	
      WRITE (16,'(/A)')  '  PUMA5  PUMA1      0 WORKER      1 WORKER    
     A 2 WORKERS    3+ WORKERS    TOTAL'  
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'

      DO I=1,PUMA5
	  ROW_TOT = PUMA5_HH_WORKER(I,1) + PUMA5_HH_WORKER(I,2) + 
     A    PUMA5_HH_WORKER(I,3) + PUMA5_HH_WORKER(I,4)
        AROW_TOT = ROW_TOT

        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J)=PUMA5_HH_WORKER(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(2I7,5(I9,F5.2))') P5_NUM(I), P5_PUMA1(I),  
     A    (PUMA5_HH_WORKER(I,J),ROW_FRAC(J),J=1,4), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_WORKER(1)+TOT_WORKER(2)+TOT_WORKER(3)+TOT_WORKER(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = TOT_WORKER(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL       ', 
     A  (TOT_WORKER(I), ROW_FRAC(I), I=1,4), ROW_TOT
C
      WRITE (16,'(/A)')  '  PUMA5  PUMA1       0 CHILD       1 CHILD    
     A   2 CHILD      3+ CHILD    TOTAL' 
     	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'

      DO I=1,PUMA5
	  ROW_TOT = PUMA5_HH_CHILD(I,1) + PUMA5_HH_CHILD(I,2) + 
     A    PUMA5_HH_CHILD(I,3) + PUMA5_HH_CHILD(I,4)
        AROW_TOT = ROW_TOT

        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J)=PUMA5_HH_CHILD(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(2I7,5(I9,F5.2))') P5_NUM(I), P5_PUMA1(I), 
     A    (PUMA5_HH_CHILD(I,J),ROW_FRAC(J),J=1,4), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_CHILD(1)+TOT_CHILD(2)+TOT_CHILD(3)+TOT_CHILD(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = TOT_CHILD(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL       ', 
     A  (TOT_CHILD(I), ROW_FRAC(I), I=1,4), ROW_TOT
C
	WRITE (16,'(/A)')  '  PUMA5  PUMA1      INCOME 1      INCOME 2    
     A  INCOME 3      INCOME 4    TOTAL'  
     	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'

      DO I=1,PUMA5
	  ROW_TOT = PUMA5_HH_INCOME(I,1) + PUMA5_HH_INCOME(I,2) + 
     A    PUMA5_HH_INCOME(I,3) + PUMA5_HH_INCOME(I,4)
        AROW_TOT = ROW_TOT

        DO J=1,4
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) ROW_FRAC(J)=PUMA5_HH_INCOME(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(2I7,5(I9,F5.2))') P5_NUM(I), P5_PUMA1(I), 
     A    (PUMA5_HH_INCOME(I,J),ROW_FRAC(J),J=1,4), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_INCOME(1) + TOT_INCOME(2) + TOT_INCOME(3) + 
     A  TOT_INCOME(4)
      AROW_TOT = ROW_TOT

      DO J=1,4
	  ROW_FRAC(J) = TOT_INCOME(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,5(I9,F5.2))') '  TOTAL       ', 
     A  (TOT_INCOME(I), ROW_FRAC(I), I=1,4), ROW_TOT
C
	WRITE (16,'(/A)')  '  PUMA5  PUMA1   HHOLDER <35 HHOLDER 35-64   H
     AHOLDER 65+    TOTAL'  
     	WRITE (16,'(A)')   '----------------------------------------------
     A-------------------'
      
      DO I=1,PUMA5
	  ROW_TOT = PUMA5_HH_HHOLDER(I,1) + PUMA5_HH_HHOLDER(I,2) + 
     A    PUMA5_HH_HHOLDER(I,3)
        AROW_TOT = ROW_TOT

        DO J=1,3
	    ROW_FRAC(J) = 0.0
	    IF (ROW_TOT .GT. 0) 
     A      ROW_FRAC(J) = PUMA5_HH_HHOLDER(I,J)/AROW_TOT
	  ENDDO	 

        WRITE (16,'(2I7,4(I9,F5.2))') P5_NUM(I), P5_PUMA1(I), 
     A    (PUMA5_HH_HHOLDER(I,J),ROW_FRAC(J),J=1,3), ROW_TOT
      ENDDO    

	ROW_TOT = TOT_HHOLDER(1) + TOT_HHOLDER(2) + TOT_HHOLDER(3)
      AROW_TOT = ROW_TOT

      DO J=1,3
	  ROW_FRAC(J) = TOT_HHOLDER(J)/AROW_TOT
	ENDDO	 
	
	WRITE (16,'(A,4(I9,F5.2))') '  TOTAL       ', 
     A  (TOT_HHOLDER(I), ROW_FRAC(I), I=1,3), ROW_TOT
C
C	READ BACK HOUSEHOLD FILE TO CHECK NUMBER OF HOUSEHOLDS
C
      TOTAL_HH_OUT = 0 
      DO SZ = 1,SUBZONES
        DO HHT = 1,624
	    TOTAL_HH_OUT = TOTAL_HH_OUT +ISZ_MATRIX(SZ,HHT)
        ENDDO
      ENDDO  

      WRITE (16,'(/A,I9)') 'TOTAL HOUSEHOLDS WRITTEN= ', 
     A  TOTAL_HH_OUT
C
      WRITE (16,'(/A)') 'END OF PRINT_TAZHH'
      WRITE (*,'(/A)') 'END OF PRINT_TAZHH'
C
C     CLOSE BASE_HHINPUT.TXT  
C
      CLOSE (20, STATUS='KEEP')
      RETURN
      

	END