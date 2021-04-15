C***********************************************************************
C        1         2         3         4         5         6         7 
C23456789012345678901234567890123456789012345678901234567890123456789012
C***********************************************************************

      SUBROUTINE REPORT2

C***********************************************************************
C
C     THIS SUBROUTINE WRITES THE DISTR FILE INPUTS TO THE LOGFILE.
C    
C***********************************************************************
C
      IMPLICIT INTEGER (A-Z)

	INCLUDE 'Common_params.fi'
      INCLUDE 'Common_approach_model.fi'
	INCLUDE 'Common_data.fi'

	CHARACTER*1 ASTERIX(100)/100*'*'/

C
C     WRITE OUT REPORTS TO LOGFILE
C
      WRITE (31,'(/A)') ' APPROACH SUBMODEL DISTR INPUTS '
	WRITE (31,'(100A1)') ASTERIX
      
      LINER2 = 0
      DO 70 I=1,ZONES

	IF (ZOI(I) .OR. DZOI(I)) THEN

        LINER2 = LINER2 + 1

	  IF (MOD(LINER2,100) .EQ. 1) THEN
          WRITE (31,'(/A/)') '   NO.  ZONE  TYPE  ******COMMUTER RAIL***
     A**  ****CTA RAIL TRANSIT****  ****CTA AND PACE BUS****  *******FEE
     BDER BUS*******  ******PARK AND RIDE*****'

	  ENDIF

	  WRITE (31, 301) LINER2, I, ZTYPE(I), 
     A    (DISTR(I,1,KK),KK=1,3), (DISTR(I,2,KK),KK=1,3), 
     B    (DISTR(I,3,KK),KK=1,3), (DISTR(I,4,KK),KK=1,3),
     C    (DISTR(I,5,KK),KK=1,3)

	ENDIF
	  
  301 FORMAT(3I6,5(2X,3F8.3))

   70 CONTINUE

      RETURN
C
      END