      SUBROUTINE SUB_TRIPGEN9
      IMPLICIT INTEGER(A-Z)
C***********************************************************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C***********************************************************************
C
C     THIS IS A SECOND NEW SUBROUTINE THAT PREPARES THE FINAL SUMMARIES
C     OF PRODUCTIONS AND ATTRACTIONS AND WRITES OUT THE FINAL FILE.
C     TRIP PRODUCTIONS ARE EITHER THE 11 CURRENT CMAP TRIP CATEGORIES
C     OR THE EXPANDED 46 TRIP CATEGORIES DEPENDING ON EXP_TTYPE.
C
C     EASH FEBRUARY 2009
C
C***********************************************************************
	INCLUDE 'COMMON_PARAM.FI'
	INCLUDE 'COMMON_GEOG.FI'
	INCLUDE 'COMMON_ATTR.FI'
	INCLUDE 'COMMON_HHSUMS.FI'
C
C     ZERO OUT SUMMARY ARRAYS FROM TRIPGEN6
C
      DO T=1,49
	  DO ICO=1,COUNTIES
	    CO_PRODS(ICO,T) = 0.0
		CO_ATTRS(ICO,T) = 0.0
        ENDDO

	  DO IP1=1,PUMA1
	    P1_PRODS(IP1,T) = 0.0
	    P1_ATTRS(IP1,T) = 0.0
        ENDDO

	  DO IP5=1,PUMA5
	    P5_PRODS(IP5,T) = 0.0
	    P5_ATTRS(IP5,T) = 0.0
        ENDDO

        REG_PRODS(T) = 0.0
        REG_ATTRS(T) = 0.0

        CHI_PRODS(T) = 0.0
	  CHI_ATTRS(T) = 0.0
        CBD_PRODS(T) = 0.0
        CBD_ATTRS(T) = 0.0
      ENDDO
C
C     ACCUMULATE EXPANDED TRIP PRODUCTIONS AND ATTRACTIONS INTO CMAP
C     ORIGINAL TRIP TYPES
C
      DO SZ=1,SUBZONES

        DO T=1,4
	    SZ_PRODS_OLD(SZ,1) = SZ_PRODS_OLD(SZ,1) + SZ_PRODS(SZ,T)
	    SZ_ATTRS_OLD(SZ,1) = SZ_ATTRS_OLD(SZ,1) + SZ_ATTRS(SZ,T)
        ENDDO
C
	  SZ_PRODS_OLD(SZ,2) = SZ_PRODS_OLD(SZ,2) + SZ_PRODS(SZ,7)
	  SZ_ATTRS_OLD(SZ,2) = SZ_ATTRS_OLD(SZ,2) + SZ_ATTRS(SZ,7)
C
        DO T=5,6
	    SZ_PRODS_OLD(SZ,3) = SZ_PRODS_OLD(SZ,3) + SZ_PRODS(SZ,T)
	    SZ_ATTRS_OLD(SZ,3) = SZ_ATTRS_OLD(SZ,3) + SZ_ATTRS(SZ,T)
        ENDDO
C
	  SZ_PRODS_OLD(SZ,4) = SZ_PRODS_OLD(SZ,4) + SZ_PRODS(SZ,10)
	  SZ_ATTRS_OLD(SZ,4) = SZ_ATTRS_OLD(SZ,4) + SZ_ATTRS(SZ,10)
C
        DO T=8,9
	    SZ_PRODS_OLD(SZ,5) = SZ_PRODS_OLD(SZ,5) + SZ_PRODS(SZ,T)
	    SZ_ATTRS_OLD(SZ,5) = SZ_ATTRS_OLD(SZ,5) + SZ_ATTRS(SZ,T)
        ENDDO
C
	  SZ_PRODS_OLD(SZ,6) = SZ_PRODS_OLD(SZ,6) + SZ_PRODS(SZ,11)
	  SZ_ATTRS_OLD(SZ,6) = SZ_ATTRS_OLD(SZ,6) + SZ_ATTRS(SZ,11)
C        
	  DO T=12,20
	    SZ_PRODS_OLD(SZ,7) = SZ_PRODS_OLD(SZ,7) + SZ_PRODS(SZ,T)
	    SZ_ATTRS_OLD(SZ,7) = SZ_ATTRS_OLD(SZ,7) + SZ_ATTRS(SZ,T)
        ENDDO
C
	  SZ_PRODS_OLD(SZ,8) = SZ_PRODS_OLD(SZ,8) + SZ_PRODS(SZ,24)
	  SZ_ATTRS_OLD(SZ,8) = SZ_ATTRS_OLD(SZ,8) + SZ_ATTRS(SZ,24)
C        
	  DO T=21,23
	    SZ_PRODS_OLD(SZ,9) = SZ_PRODS_OLD(SZ,9) + SZ_PRODS(SZ,T)
	    SZ_ATTRS_OLD(SZ,9) = SZ_ATTRS_OLD(SZ,9) + SZ_ATTRS(SZ,T)
        ENDDO
C
	  DO T=25,33
	    SZ_PRODS_OLD(SZ,10) = SZ_PRODS_OLD(SZ,10) + SZ_PRODS(SZ,T)
	    SZ_ATTRS_OLD(SZ,10) = SZ_ATTRS_OLD(SZ,10) + SZ_ATTRS(SZ,T)
        ENDDO
C
	  DO T=34,37
	    SZ_PRODS_OLD(SZ,11) = SZ_PRODS_OLD(SZ,11) + SZ_PRODS(SZ,T)
	    SZ_ATTRS_OLD(SZ,11) = SZ_ATTRS_OLD(SZ,11) + SZ_ATTRS(SZ,T)
        ENDDO
	ENDDO
C
C     ACCUMULATE SUMMARIES
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

	    CO_PRODS(ICO,T) = CO_PRODS(ICO,T) + SZ_PRODS(SZ,T)
	    CO_ATTRS(ICO,T) = CO_ATTRS(ICO,T) + SZ_ATTRS(SZ,T)

	    P1_PRODS(IP1,T) = P1_PRODS(IP1,T) + SZ_PRODS(SZ,T)
	    P1_ATTRS(IP1,T) = P1_ATTRS(IP1,T) + SZ_ATTRS(SZ,T)

	    P5_PRODS(IP5,T) = P5_PRODS(IP5,T) + SZ_PRODS(SZ,T)
	    P5_ATTRS(IP5,T) = P5_ATTRS(IP5,T) + SZ_ATTRS(SZ,T)

          REG_PRODS(T) = REG_PRODS(T) + SZ_PRODS(SZ,T)
          REG_ATTRS(T) = REG_ATTRS(T) + SZ_ATTRS(SZ,T)

          IF (ICHI .GT. 0) THEN 
            CHI_PRODS(T) = CHI_PRODS(T) + SZ_PRODS(SZ,T)
	      CHI_ATTRS(T) = CHI_ATTRS(T) + SZ_ATTRS(SZ,T)
          ENDIF

          IF (ICBD .GT. 0) THEN 
            CBD_PRODS(T) = CBD_PRODS(T) + SZ_PRODS(SZ,T)
            CBD_ATTRS(T) = CBD_ATTRS(T) + SZ_ATTRS(SZ,T)
          ENDIF
        ENDDO
C
C     ACCUMULATE FOR ORIGINAL TRIP TYPES
C
        DO T=1,11

	    CO_PRODS_OLD(ICO,T) = CO_PRODS_OLD(ICO,T) + SZ_PRODS_OLD(SZ,T)
	    CO_ATTRS_OLD(ICO,T) = CO_ATTRS_OLD(ICO,T) + SZ_ATTRS_OLD(SZ,T)

	    P1_PRODS_OLD(IP1,T) = P1_PRODS_OLD(IP1,T) + SZ_PRODS_OLD(SZ,T)
	    P1_ATTRS_OLD(IP1,T) = P1_ATTRS_OLD(IP1,T) + SZ_ATTRS_OLD(SZ,T)

	    P5_PRODS_OLD(IP5,T) = P5_PRODS_OLD(IP5,T) + SZ_PRODS_OLD(SZ,T)
	    P5_ATTRS_OLD(IP5,T) = P5_ATTRS_OLD(IP5,T) + SZ_ATTRS_OLD(SZ,T)

          REG_PRODS_OLD(T) = REG_PRODS_OLD(T) + SZ_PRODS_OLD(SZ,T)
          REG_ATTRS_OLD(T) = REG_ATTRS_OLD(T) + SZ_ATTRS_OLD(SZ,T)

          IF (ICHI .GT. 0) THEN 
            CHI_PRODS_OLD(T) = CHI_PRODS_OLD(T) + SZ_PRODS_OLD(SZ,T)
	      CHI_ATTRS_OLD(T) = CHI_ATTRS_OLD(T) + SZ_ATTRS_OLD(SZ,T)
          ENDIF

          IF (ICBD .GT. 0) THEN 
            CBD_PRODS_OLD(T) = CBD_PRODS_OLD(T) + SZ_PRODS_OLD(SZ,T)
            CBD_ATTRS_OLD(T) = CBD_ATTRS_OLD(T) + SZ_ATTRS_OLD(SZ,T)
          ENDIF
        ENDDO
      ENDDO
C
C     PRINT OUT FINAL PRODUCTIONS AND ATTRACTIONS
C
      IF (EXP_TTYPE) GO TO 1001
C
C     SUMMARIES USING ORIGINAL TRIP CATEGORIES
C
	WRITE(16,'(/A)') 'FINAL VEHICLE TRIP PRODUCTIONS/ORIGINS AND ATTRA
     ACTIONS/DESTINATIONS'
	WRITE (16,'(/A)')  'COUNTIES'

	WRITE (16,'(/A)')  'WORKERS HOME BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               HWORK             HSHOP        
     A     HOTHR'  
      WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   ' COUNTY    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS'
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       
	
	DO I=1,COUNTIES
	  WRITE (16,'(I7,6F9.0)') CO_NUM(I), 
     A    ((CO_PRODS_OLD(I,T), CO_ATTRS_OLD(I,T)),T=1,3) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL', 
     A  ((REG_PRODS_OLD(T), REG_ATTRS_OLD(T)),T=1,3) 
      WRITE (16,'(/A,6F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS_OLD(T), CHI_ATTRS_OLD(T)),T=1,3) 
	WRITE (16,'(A,6F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS_OLD(T), CBD_ATTRS_OLD(T)),T=1,3) 
C
	WRITE (16,'(/A)')  'WORKERS WORK BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               WSHOP             WOTHR        
     A     WWORK'  
      WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   ' COUNTY    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS'
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       
	
	DO I=1,COUNTIES
	  WRITE (16,'(I7,6F9.0)') CO_NUM(I), 
     A    ((CO_PRODS_OLD(I,T), CO_ATTRS_OLD(I,T)),T=4,6) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL', 
     A  ((REG_PRODS_OLD(T), REG_ATTRS_OLD(T)),T=4,6) 
      WRITE (16,'(/A,6F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS_OLD(T), CHI_ATTRS_OLD(T)),T=4,6) 
	WRITE (16,'(A,6F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS_OLD(T), CBD_ATTRS_OLD(T)),T=4,6) 
C
	WRITE (16,'(/A)')  'WORKERS NONHOME/WORK BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               OTHER'  
      WRITE (16,'(A)')   '         ----------------'
      WRITE (16,'(A)')   ' COUNTY    ORIGS    DESTS'
	WRITE (16,'(A)')   '-------------------------'       
	
	DO I=1,COUNTIES
	  WRITE (16,'(I7,2F9.0)') CO_NUM(I), CO_PRODS_OLD(I,7), 
     A    CO_ATTRS_OLD(I,7)
	ENDDO

	WRITE (16,'(A,2F9.0)')  '  TOTAL', REG_PRODS_OLD(7), 
     A  REG_ATTRS_OLD(7) 
      WRITE (16,'(/A,2F9.0)') 'CHICAGO', CHI_PRODS_OLD(7), 
     A  CHI_ATTRS_OLD(7) 
	WRITE (16,'(A,2F9.0)')  'CHI CBD', CBD_PRODS_OLD(7),
     A  CBD_ATTRS_OLD(7)
C
	WRITE(16,'(/A)') 'NONWORKERS HOME BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               HSHOP             HOTHR'  
      WRITE (16,'(A)')   '         ----------------  ----------------'
      WRITE (16,'(A)')   ' COUNTY    PRODS    ATTRS    PRODS    ATTRS'
	WRITE (16,'(A)')   '-------------------------------------------'       
	
	DO I=1,COUNTIES
	  WRITE (16,'(I7,4F9.0)') CO_NUM(I), 
     A    ((CO_PRODS_OLD(I,T), CO_ATTRS_OLD(I,T)),T=8,9) 
	ENDDO

	WRITE (16,'(A,4F9.0)')  '  TOTAL', 
     A  ((REG_PRODS_OLD(T), REG_ATTRS_OLD(T)),T=8,9) 
      WRITE (16,'(/A,4F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS_OLD(T), CHI_ATTRS_OLD(T)),T=8,9) 
	WRITE (16,'(A,4F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS_OLD(T), CBD_ATTRS_OLD(T)),T=8,9) 
C
	WRITE (16,'(/A)')  'NONWORKERS NONHOME BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               OTHER'  
      WRITE (16,'(A)')   '         ----------------'
      WRITE (16,'(A)')   ' COUNTY    ORIGS    DESTS'
	WRITE (16,'(A)')   '-------------------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,2F9.0)') CO_NUM(I), CO_PRODS_OLD(I,10), 
     A    CO_ATTRS_OLD(I,10)
	ENDDO

	WRITE (16,'(A,2F9.0)')  '  TOTAL', REG_PRODS_OLD(10), 
     A  REG_ATTRS_OLD(10) 
      WRITE (16,'(/A,2F9.0)') 'CHICAGO', CHI_PRODS_OLD(10), 
     A  CHI_ATTRS_OLD(10) 
	WRITE (16,'(A,2F9.0)')  'CHI CBD', CBD_PRODS_OLD(10),
     A  CBD_ATTRS_OLD(10)
C
	WRITE(16,'(/A)') 'CHILDREN(12-15) HOME BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               HOTHER'  
      WRITE (16,'(A)')   '         ----------------'
      WRITE (16,'(A)')   ' COUNTY    PRODS    ATTRS'
	WRITE (16,'(A)')   '-------------------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,2F9.0)') CO_NUM(I), CO_PRODS_OLD(I,11), 
     A    CO_ATTRS_OLD(I,11)
	ENDDO

	WRITE (16,'(A,2F9.0)')  '  TOTAL', REG_PRODS_OLD(11), 
     A  REG_ATTRS_OLD(11) 
      WRITE (16,'(/A,2F9.0)') 'CHICAGO', CHI_PRODS_OLD(11), 
     A  CHI_ATTRS_OLD(11) 
	WRITE (16,'(A,2F9.0)')  'CHI CBD', CBD_PRODS_OLD(11),
     A  CBD_ATTRS_OLD(11)
C
C     REPEAT FOR ONE PERCENT PUMS
C
	WRITE (16,'(/A)')  'ONE PERCENT PUMAS'

	WRITE (16,'(/A)')  'WORKERS HOME BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               HWORK             HSHOP        
     A     HOTHR'  
      WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA1    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS'
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       
	
	DO I=1,PUMA1
	  WRITE (16,'(I7,6F9.0)') P1_NUM(I), 
     A    ((P1_PRODS_OLD(I,T), P1_ATTRS_OLD(I,T)),T=1,3) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL', 
     A  ((REG_PRODS_OLD(T), REG_ATTRS_OLD(T)),T=1,3) 
C
	WRITE (16,'(/A)')  'WORKERS WORK BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               WSHOP             WOTHR        
     A     WWORK'  
      WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA1    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS'
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       
	
	DO I=1,PUMA1
	  WRITE (16,'(I7,6F9.0)') P1_NUM(I), 
     A    ((P1_PRODS_OLD(I,T), P1_ATTRS_OLD(I,T)),T=4,6) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL', 
     A  ((REG_PRODS_OLD(T), REG_ATTRS_OLD(T)),T=4,6) 
C
	WRITE (16,'(/A)')  'WORKERS NONHOME/WORK BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               OTHER'  
      WRITE (16,'(A)')   '         ----------------'
      WRITE (16,'(A)')   '  PUMA1    ORIGS    DESTS'
	WRITE (16,'(A)')   '-------------------------'       
	
	DO I=1,PUMA1
	  WRITE (16,'(I7,2F9.0)') P1_NUM(I), P1_PRODS_OLD(I,7), 
     A    P1_ATTRS_OLD(I,7)
	ENDDO

	WRITE (16,'(A,2F9.0)')  '  TOTAL', REG_PRODS_OLD(7), 
     A  REG_ATTRS_OLD(7) 
C
	WRITE(16,'(/A)') 'NONWORKERS HOME BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               HSHOP             HOTHR'  
      WRITE (16,'(A)')   '         ----------------  ----------------'
      WRITE (16,'(A)')   '  PUMA1    PRODS    ATTRS    PRODS    ATTRS'
	WRITE (16,'(A)')   '-------------------------------------------'       
	
	DO I=1,PUMA1
	  WRITE (16,'(I7,4F9.0)') P1_NUM(I), 
     A    ((P1_PRODS_OLD(I,T), P1_ATTRS_OLD(I,T)),T=8,9) 
	ENDDO

	WRITE (16,'(A,4F9.0)')  '  TOTAL', 
     A  ((REG_PRODS_OLD(T), REG_ATTRS_OLD(T)),T=8,9) 
C
	WRITE (16,'(/A)')  'NONWORKERS NONHOME BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               OTHER'  
      WRITE (16,'(A)')   '         ----------------'
      WRITE (16,'(A)')   '  PUMA1    ORIGS    DESTS'
	WRITE (16,'(A)')   '-------------------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,2F9.0)') P1_NUM(I), P1_PRODS_OLD(I,10), 
     A    P1_ATTRS_OLD(I,10)
	ENDDO

	WRITE (16,'(A,2F9.0)')  '  TOTAL', REG_PRODS_OLD(10), 
     A  REG_ATTRS_OLD(10) 
C
	WRITE(16,'(/A)') 'CHILDREN(12-15) HOME BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               HOTHER'  
      WRITE (16,'(A)')   '         ----------------'
      WRITE (16,'(A)')   '  PUMA1    PRODS    ATTRS'
	WRITE (16,'(A)')   '-------------------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,2F9.0)') P1_NUM(I), P1_PRODS_OLD(I,11), 
     A    P1_ATTRS_OLD(I,11)
	ENDDO

	WRITE (16,'(A,2F9.0)')  '  TOTAL', REG_PRODS_OLD(11), 
     A  REG_ATTRS_OLD(11) 
C
C     REPEAT FOR FIVE PERCENT PUMS
C
	WRITE (16,'(/A)')  'FIVE PERCENT PUMAS'

	WRITE (16,'(/A)')  'WORKERS HOME BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               HWORK             HSHOP        
     A     HOTHR'  
      WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA5    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS'
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       
	
	DO I=1,PUMA5
	  WRITE (16,'(I7,6F9.0)') P5_NUM(I), 
     A    ((P5_PRODS_OLD(I,T), P5_ATTRS_OLD(I,T)),T=1,3) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL', 
     A  ((REG_PRODS_OLD(T), REG_ATTRS_OLD(T)),T=1,3) 
C
	WRITE (16,'(/A)')  'WORKERS WORK BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               WSHOP             WOTHR        
     A     WWORK'  
      WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA5    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS'
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       
	
	DO I=1,PUMA5
	  WRITE (16,'(I7,6F9.0)') P5_NUM(I), 
     A    ((P5_PRODS_OLD(I,T), P5_ATTRS_OLD(I,T)),T=4,6) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL', 
     A  ((REG_PRODS_OLD(T), REG_ATTRS_OLD(T)),T=4,6) 
C
	WRITE (16,'(/A)')  'WORKERS NONHOME/WORK BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               OTHER'  
      WRITE (16,'(A)')   '         ----------------'
      WRITE (16,'(A)')   '  PUMA5    ORIGS    DESTS'
	WRITE (16,'(A)')   '-------------------------'       
	
	DO I=1,PUMA5
	  WRITE (16,'(I7,2F9.0)') P5_NUM(I), P5_PRODS_OLD(I,7), 
     A    P5_ATTRS_OLD(I,7)
	ENDDO

	WRITE (16,'(A,2F9.0)')  '  TOTAL', REG_PRODS_OLD(7), 
     A  REG_ATTRS_OLD(7) 
C
	WRITE(16,'(/A)') 'NONWORKERS HOME BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               HSHOP             HOTHR'  
      WRITE (16,'(A)')   '         ----------------  ----------------'
      WRITE (16,'(A)')   '  PUMA5    PRODS    ATTRS    PRODS    ATTRS'
	WRITE (16,'(A)')   '-------------------------------------------'       
	
	DO I=1,PUMA5
	  WRITE (16,'(I7,4F9.0)') P5_NUM(I), 
     A    ((P5_PRODS_OLD(I,T), P5_ATTRS_OLD(I,T)),T=8,9) 
	ENDDO

	WRITE (16,'(A,4F9.0)')  '  TOTAL', 
     A  ((REG_PRODS_OLD(T), REG_ATTRS_OLD(T)),T=8,9) 
C
	WRITE (16,'(/A)')  'NONWORKERS NONHOME BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               OTHER'  
      WRITE (16,'(A)')   '         ----------------'
      WRITE (16,'(A)')   '  PUMA5    ORIGS    DESTS'
	WRITE (16,'(A)')   '-------------------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,2F9.0)') P5_NUM(I), P5_PRODS_OLD(I,10), 
     A    P5_ATTRS_OLD(I,10)
	ENDDO

	WRITE (16,'(A,2F9.0)')  '  TOTAL', REG_PRODS_OLD(10), 
     A  REG_ATTRS_OLD(10) 
C
	WRITE(16,'(/A)') 'CHILDREN(12-15) HOME BASED VEHICLE TRIPS'
	WRITE (16,'( A)')  '               HOTHER'  
      WRITE (16,'(A)')   '         ----------------'
      WRITE (16,'(A)')   '  PUMA5    PRODS    ATTRS'
	WRITE (16,'(A)')   '-------------------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,2F9.0)') P5_NUM(I), P5_PRODS_OLD(I,11), 
     A    P5_ATTRS_OLD(I,11)
	ENDDO

	WRITE (16,'(A,2F9.0)')  '  TOTAL', REG_PRODS_OLD(11), 
     A  REG_ATTRS_OLD(11) 
C
      GO TO 1002
C***********************************************************************
C
C     SUMMARIES USING EXPANDED TRIP CATEGORIES
C
C***********************************************************************
 1001 CONTINUE
C
	WRITE(16,'(/A)') 'FINAL TRIP PRODUCTIONS/ORIGINS AND ATTRACTIONS/D
     AESTINATIONS'
	WRITE(16,'(A)')  'FOR EXPANDED TRIP CATEGORIES'
	WRITE (16,'(/A)')  'COUNTIES'

	WRITE (16,'(/A)')  'WORKERS HOME BASED VEHICLE TRIPS (1/2)'
	WRITE (16,'(A)')   '              WKPL(<$)          WKPL(>$)      
     A    WORK REL           SCHOOL'  
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------  ----------------'
      WRITE (16,'(A)')   ' COUNTY    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS    PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,8F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=1,4) 
	ENDDO

	WRITE (16,'(A,8F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=1,4) 
      WRITE (16,'(/A,8F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=1,4) 
	WRITE (16,'(A,8F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=1,4)
C
	WRITE (16,'(/A)')  'WORKERS HOME BASED VEHICLE TRIPS (2/2)'
	WRITE (16,'(A)')   '              NH/W(H)           NH/W(NH)      
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   ' COUNTY    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,6F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=5,7) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=5,7) 
      WRITE (16,'(/A,6F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=5,7) 
	WRITE (16,'(A,6F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=5,7)
C
	WRITE (16,'(/A)')  'WORKERS WORK BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '               NH/W(H)          NH/W(NH)      
     A      SHOP              WORK'  
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------  ----------------'
      WRITE (16,'(A)')   ' COUNTY    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS    PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'   
     
	DO I=1,COUNTIES
	  WRITE (16,'(I7,8F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=8,11) 
	ENDDO

	WRITE (16,'(A,8F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=8,11) 
      WRITE (16,'(/A,8F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=8,11) 
	WRITE (16,'(A,8F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=8,11)
C
      WRITE (16,'(/A)')  'WORKERS NONH/W AT HOUSEHOLDS BASED VEHICLE TRI
     APS'
	WRITE (16,'(A)')   '              NH/W(H)           NH/W(NH)      
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   ' COUNTY    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,6F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=12,14) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=12,14) 
      WRITE (16,'(/A,6F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=12,14) 
	WRITE (16,'(A,6F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=12,14)
C
      WRITE (16,'(/A)')  'WORKER NONH/W NOT AT HOUSEHOLDS BASED VEHICLE 
     ATRIPS'
	WRITE (16,'(A)')   '              NH/W(H)           NH/W(NH)      
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   ' COUNTY    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,6F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=15,17) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=15,17) 
      WRITE (16,'(/A,6F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=15,17) 
	WRITE (16,'(A,6F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=15,17)
C
      WRITE (16,'(/A)')  'WORKER SHOP BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '              NH/W(H)           NH/W(NH)      
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   ' COUNTY    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,6F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=18,20) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=18,20) 
      WRITE (16,'(/A,6F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=18,20) 
	WRITE (16,'(A,6F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=18,20)
C
      WRITE (16,'(/A)')  'NONWORKER HOME BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '               SCHOOL             NH(H)       
     A     NH(NH)             SHOP'  
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------  ----------------'
      WRITE (16,'(A)')   ' COUNTY    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS    PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'   
     
	DO I=1,COUNTIES
	  WRITE (16,'(I7,8F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=21,24) 
	ENDDO

	WRITE (16,'(A,8F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=21,24) 
      WRITE (16,'(/A,8F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=21,24) 
	WRITE (16,'(A,8F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=21,24)
C
      WRITE (16,'(/A)')  'NONWORKER NONHOME AT HOUSEHOLDS BASED VEHICLE 
     ATRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   ' COUNTY    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,6F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=25,27) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=25,27) 
      WRITE (16,'(/A,6F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=25,27) 
	WRITE (16,'(A,6F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=25,27)
C
      WRITE (16,'(/A)')  'NONWORKER NONHOME NOT AT HOUSEHOLDS BASED VEHI
     ACLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   ' COUNTY    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,6F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=28,30) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=28,30) 
      WRITE (16,'(/A,6F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=28,30) 
	WRITE (16,'(A,6F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=28,30)
C
      WRITE (16,'(/A)')  'NONWORKER SHOP BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   ' COUNTY    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,6F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=31,33) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=31,33) 
      WRITE (16,'(/A,6F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=31,33) 
	WRITE (16,'(A,6F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=31,33)
C
      WRITE (16,'(/A)')  'CHILD(12-15) HOME BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '               SCHOOL             NH(H)       
     A     NH(NH)             SHOP'  
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------  ----------------'
      WRITE (16,'(A)')   ' COUNTY    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS    PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'   
     
	DO I=1,COUNTIES
	  WRITE (16,'(I7,8F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=34,37) 
	ENDDO

	WRITE (16,'(A,8F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=34,37) 
      WRITE (16,'(/A,8F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=34,37) 
	WRITE (16,'(A,8F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=34,37)
C
      WRITE (16,'(/A)')  'CHILD(12-15) SCHOOL BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   ' COUNTY    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,6F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=38,40) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=38,40) 
      WRITE (16,'(/A,6F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=38,40) 
	WRITE (16,'(A,6F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=38,40)
C
      WRITE (16,'(/A)') 'CHILD(12-15) NONHOME AT HOUSEHOLDS BASED VEHICL
     AE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   ' COUNTY    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,6F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=41,43) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=41,43) 
      WRITE (16,'(/A,6F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=41,43) 
	WRITE (16,'(A,6F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=41,43)
C
      WRITE (16,'(/A)')  'CHILD(12-15) NONHOME NOT AT HOUSEHOLDS BASED V
     AEHICLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   ' COUNTY    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,6F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=44,46) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=44,46) 
      WRITE (16,'(/A,6F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=44,46) 
	WRITE (16,'(A,6F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=44,46)
C
      WRITE (16,'(/A)')  'CHILD(12-15) SHOP BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   ' COUNTY    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,COUNTIES
	  WRITE (16,'(I7,6F9.0)') CO_NUM(I), 
     A    ((CO_PRODS(I,T), CO_ATTRS(I,T)),T=47,49) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=47,49) 
      WRITE (16,'(/A,6F9.0)') 'CHICAGO', 
     A  ((CHI_PRODS(T), CHI_ATTRS(T)),T=47,49) 
	WRITE (16,'(A,6F9.0)')  'CHI CBD', 
     A  ((CBD_PRODS(T), CBD_ATTRS(T)),T=47,49)
C***********************************************************************
C
	WRITE (16,'(/A)')  'ONE PERCENT PUMAS'

	WRITE (16,'(/A)')  'WORKERS HOME BASED VEHICLE TRIPS (1/2)'
	WRITE (16,'(A)')   '              WKPL(<$)          WKPL(>$)      
     A    WORK REL           SCHOOL'  
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------  ----------------'
      WRITE (16,'(A)')   '  PUMA1    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS    PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,8F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=1,4) 
	ENDDO

	WRITE (16,'(A,8F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=1,4) 
C
	WRITE (16,'(/A)')  'WORKERS HOME BASED VEHICLE TRIPS (2/2)'
	WRITE (16,'(A)')   '              NH/W(H)           NH/W(NH)      
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA1    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,6F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=5,7) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=5,7) 
C
	WRITE (16,'(/A)')  'WORKERS WORK BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '               NH/W(H)          NH/W(NH)      
     A      SHOP              WORK'  
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------  ----------------'
      WRITE (16,'(A)')   '  PUMA1    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS    PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'   
     
	DO I=1,PUMA1
	  WRITE (16,'(I7,8F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=8,11) 
	ENDDO

	WRITE (16,'(A,8F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=8,11) 
C
      WRITE (16,'(/A)')  'WORKERS NONH/W AT HOUSEHOLDS BASED VEHICLE TRI
     APS'
	WRITE (16,'(A)')   '              NH/W(H)           NH/W(NH)      
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA1    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,6F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=12,14) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=12,14) 
C
      WRITE (16,'(/A)')  'WORKER NONH/W NOT AT HOUSEHOLDS BASED VEHICLE 
     ATRIPS'
	WRITE (16,'(A)')   '              NH/W(H)           NH/W(NH)      
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA1    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,6F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=15,17) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=15,17) 
C
      WRITE (16,'(/A)')  'WORKER SHOP BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '              NH/W(H)           NH/W(NH)      
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA1    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,6F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=18,20) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=18,20) 
C
      WRITE (16,'(/A)')  'NONWORKER HOME BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '               SCHOOL             NH(H)       
     A     NH(NH)             SHOP'  
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------  ----------------'
      WRITE (16,'(A)')   '  PUMA1    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS    PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'   
     
	DO I=1,PUMA1
	  WRITE (16,'(I7,8F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=21,24) 
	ENDDO

	WRITE (16,'(A,8F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=21,24) 
C
      WRITE (16,'(/A)')  'NONWORKER NONHOME AT HOUSEHOLDS BASED VEHICLE 
     ATRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA1    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,6F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=25,27) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=25,27) 
C
      WRITE (16,'(/A)')  'NONWORKER NONHOME NOT AT HOUSEHOLDS BASED VEHI
     ACLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA1    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,6F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=28,30) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=28,30) 
C
      WRITE (16,'(/A)')  'NONWORKER SHOP BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA1    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,6F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=31,33) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=31,33) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) HOME BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '               SCHOOL             NH(H)       
     A     NH(NH)             SHOP'  
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------  ----------------'
      WRITE (16,'(A)')   '  PUMA1    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS    PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'   
     
	DO I=1,PUMA1
	  WRITE (16,'(I7,8F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=34,37) 
	ENDDO

	WRITE (16,'(A,8F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=34,37) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) SCHOOL BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA1    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,6F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=38,40) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=38,40) 
C
      WRITE (16,'(/A)') 'CHILD(12-15) NONHOME AT HOUSEHOLDS BASED VEHICL
     AE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA1    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,6F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=41,43) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=41,43) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) NONHOME NOT AT HOUSEHOLDS BASED V
     AEHICLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA1    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,6F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=44,46) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=44,46) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) SHOP BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA1    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA1
	  WRITE (16,'(I7,6F9.0)') P1_NUM(I), 
     A    ((P1_PRODS(I,T), P1_ATTRS(I,T)),T=47,49) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=47,49) 
C***********************************************************************
C
	WRITE (16,'(/A)')  'FIVE PERCENT PUMAS'

	WRITE (16,'(/A)')  'WORKERS HOME BASED VEHICLE TRIPS (1/2)'
	WRITE (16,'(A)')   '              WKPL(<$)          WKPL(>$)      
     A    WORK REL           SCHOOL'  
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------  ----------------'
      WRITE (16,'(A)')   '  PUMA5    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS    PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,8F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=1,4) 
	ENDDO

	WRITE (16,'(A,8F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=1,4) 
C
	WRITE (16,'(/A)')  'WORKERS HOME BASED VEHICLE TRIPS (2/2)'
	WRITE (16,'(A)')   '              NH/W(H)           NH/W(NH)      
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA5    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,6F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=5,7) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=5,7) 
C
	WRITE (16,'(/A)')  'WORKERS WORK BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '               NH/W(H)          NH/W(NH)      
     A      SHOP              WORK'  
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------  ----------------'
      WRITE (16,'(A)')   '  PUMA5    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS    PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'   
     
	DO I=1,PUMA5
	  WRITE (16,'(I7,8F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=8,11) 
	ENDDO

	WRITE (16,'(A,8F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=8,11) 
C
      WRITE (16,'(/A)')  'WORKERS NONH/W AT HOUSEHOLDS BASED VEHICLE TRI
     APS'
	WRITE (16,'(A)')   '              NH/W(H)           NH/W(NH)      
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA5    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,6F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=12,14) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=12,14) 
C
      WRITE (16,'(/A)')  'WORKER NONH/W NOT AT HOUSEHOLDS BASED VEHICLE 
     ATRIPS'
	WRITE (16,'(A)')   '              NH/W(H)           NH/W(NH)      
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA5    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,6F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=15,17) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=15,17) 
C
      WRITE (16,'(/A)')  'WORKER SHOP BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '              NH/W(H)           NH/W(NH)      
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA5    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,6F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=18,20) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=18,20) 
C
      WRITE (16,'(/A)')  'NONWORKER HOME BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '               SCHOOL             NH(H)       
     A     NH(NH)             SHOP'  
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------  ----------------'
      WRITE (16,'(A)')   '  PUMA5    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS    PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'   
     
	DO I=1,PUMA5
	  WRITE (16,'(I7,8F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=21,24) 
	ENDDO

	WRITE (16,'(A,8F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=21,24) 
C
      WRITE (16,'(/A)')  'NONWORKER NONHOME AT HOUSEHOLDS BASED VEHICLE 
     ATRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA5    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,6F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=25,27) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=25,27) 
C
      WRITE (16,'(/A)')  'NONWORKER NONHOME NOT AT HOUSEHOLDS BASED VEHI
     ACLE TS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA5    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,6F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=28,30) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=28,30) 
C
      WRITE (16,'(/A)')  'NONWORKER SHOP BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA5    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,6F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=31,33) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=31,33) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) HOME BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '               SCHOOL             NH(H)       
     A     NH(NH)             SHOP'  
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------  ----------------'
      WRITE (16,'(A)')   '  PUMA5    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS    PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------------------------'   
     
	DO I=1,PUMA5
	  WRITE (16,'(I7,8F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=34,37) 
	ENDDO

	WRITE (16,'(A,8F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=34,37) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) SCHOOL BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA5    PRODS    ATTRS    PRODS    ATTRS   
     A PRODS    ATTRS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,6F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=38,40) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=38,40) 
C
      WRITE (16,'(/A)') 'CHILD(12-15) NONHOME AT HOUSEHOLDS BASED VEHICL
     AE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA5    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,6F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=41,43) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=41,43) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) NONHOME NOT AT HOUSEHOLDS BASED V
     AEHICLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA5    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,6F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=44,46) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=44,46) 
C
      WRITE (16,'(/A)')  'CHILD(12-15) SHOP BASED VEHICLE TRIPS'
	WRITE (16,'(A)')   '                NH(H)            NH(NH)       
     A      SHOP'      
	WRITE (16,'(A)')   '         ----------------  ----------------  -
     A---------------'
      WRITE (16,'(A)')   '  PUMA5    ORIGS    DESTS    ORIGS    DESTS   
     A ORIGS    DESTS'    
	WRITE (16,'(A)')   '----------------------------------------------
     A---------------'       

	DO I=1,PUMA5
	  WRITE (16,'(I7,6F9.0)') P5_NUM(I), 
     A    ((P5_PRODS(I,T), P5_ATTRS(I,T)),T=47,49) 
	ENDDO

	WRITE (16,'(A,6F9.0)')  '  TOTAL',
     A  ((REG_PRODS(T), REG_ATTRS(T)),T=47,49) 
C
 1002 CONTINUE 
      
	IF (EXP_TTYPE) THEN
C
C     FILE OF PRODUCTIONS AND ATTRACTIONS IN 49 EXPANDED TRIP CATEGORIES
C     IS UNIT 60  
C
        OPEN (UNIT=60,FILE='TRIP49_PA_OUT.TXT',STATUS='NEW',ERR=960)

	  DO T=1,49
	    DO SZ = 1,SUBZONES

            WRITE (60,'(2I6,I2,2F9.1)')  SZ, SZ_ZONE(SZ), T, 
     A        SZ_PRODS(SZ,T), SZ_ATTRS(SZ,T)
	    ENDDO
        ENDDO
        CLOSE (60,DISP='KEEP')
      ELSE
C
C     FILE OF PRODUCTIONS AND ATTRACTIONS IN 11 ORIGINAL CMAP TRIP 
C     CATEGORIES IS UNIT 61  
C
        OPEN (UNIT=61,FILE='TRIP11_PA_OUT.TXT',STATUS='NEW',ERR=961)

	  DO T=1,11
	    DO SZ = 1,SUBZONES

            WRITE (61,'(2I6,I2,2F9.1)')  SZ, SZ_ZONE(SZ), T, 
     A        SZ_PRODS_OLD(SZ,T), SZ_ATTRS_OLD(SZ,T)
	    ENDDO
        ENDDO
        CLOSE (61,DISP='KEEP')
	ENDIF
C
      GO TO 999
C
  960 WRITE (*,'(A)') ' TROUBLE OPENING TRIP49_PA_OUT.TXT FILE' 
      STOP 960
  961 WRITE (*,'(A)') ' TROUBLE OPENING TRIP11_PA_OUT.TXT FILE' 
      STOP 961
C
  999 CONTINUE
C
      WRITE (16,'(/A)') ' END OF TRIPGEN9'
      WRITE (*,'(/A)') ' END OF TRIPGEN9'
      
	RETURN
      END