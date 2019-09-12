C***********************************************************************
C        1         2         3         4         5         6         7 
C23456789012345678901234567890123456789012345678901234567890123456789012
C***********************************************************************

      SUBROUTINE REPORT1

C***********************************************************************
C
C     THIS SUBROUTINE WRITES THE M01 FILE INPUTS TO THE LOGFILE.
C    
C***********************************************************************
C
      IMPLICIT INTEGER (A-Z)

	INCLUDE 'Common_params.fi'
      INCLUDE 'Common_data.fi'
      INCLUDE 'Common_approach_model.fi'

	CHARACTER*1 ASTERIX(100)/100*'*'/

C
C     WRITE OUT REPORTS TO LOGFILE
C
      WRITE (31,'(/A)') ' M01 FILE INPUTS'
	WRITE (31,'(100A1)') ASTERIX
      
      LINER1 = 0
      DO 70 I=1,ZONES

	IF (ZOI(I) .OR. DZOI(I)) THEN

        LINER1 = LINER1 + 1

	  IF (MOD(LINER1,100) .EQ. 1) THEN
          WRITE (31,'(/A/)') '   NO.  ZONE  TYPE  CBD?  CBD PARK?  P&R?  
     A P&R COST  INCOME  AUTO OCC  BUS WAIT  FBUS WAIT'
	  ENDIF

	  WRITE (31,301) LINER1, I, ZTYPE(I), ZCBD(I), ZCBD_PARK(I),
     A    PNRAVL(I), PRCOST(I), INC(I), ZOCC(I), fwbus(i), fwfdr(i)

	ENDIF

  301 FORMAT(3I6,4X,L1,7X,L1,8X,L1,I11,I8,F10.2,I10,I11)

   70 CONTINUE
      RETURN
C
      END