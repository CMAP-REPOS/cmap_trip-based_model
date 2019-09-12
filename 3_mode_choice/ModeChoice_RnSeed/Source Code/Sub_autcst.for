      SUBROUTINE AUTCST(ORIG,DEST,D,DD,ACOST) 

      INCLUDE 'Common_params.fi'
	INCLUDE 'Common_data.fI'
      INTEGER*4  ORIG, DEST
C
C     SUBROUTINE CALCULATES THE AUTO OPERATING COSTS IN CENTS
C     NOTE THAT COSTS PER FIVE MILE SPEED RANGE ARE IN AOC(16)
C     LOCATED IN COMMON DATA
C     
C
      V=D/DD*60.
      IV=V/5.
      IF(AMOD(V,5.).GT.0.) IV=IV+1
      IV=MIN0(16,IV)
      IAOC=AOC(IV)
      ACOST=FLOAT(IAOC)/100.*D
C
C## Heither, 08-02-2017: verifies this returns same value for each iteration of zone pair 
C 	IF ((ORIG.EQ.1944) .AND. (DEST.EQ.1943)) THEN
C          PRINT *, '--> ACOST: ', ACOST          
C      ENDIF    
      
	IF (TRACE) THEN
	  WRITE (31, '(/A)') ' AUTO COST VALUES IN SUBROUTINE AUTCST'
	  WRITE (31, '(A,I6)') '   ORIGIN ZONE=', ORIG
	  WRITE (31, '(A,I6)') '   DESTINATION ZONE=', DEST
  	  WRITE (31, '(A,F8.3)') '   HIGHWAY DISTANCE (D)=',D
  	  WRITE (31, '(A,F8.3)') '   HIGHWAY TIME (DD)=',DD
  	  WRITE (31, '(A,I4)')   '   COST INDEX (IV)=',IV
  	  WRITE (31, '(A,I8)')   '   OPERATING COST PER MILE (IAOC)=',IAOC
  	  WRITE (31, '(A,F8.3)') '   FINAL OPERATING COST (ACOST)=',ACOST
	ENDIF
C
      RETURN
      END