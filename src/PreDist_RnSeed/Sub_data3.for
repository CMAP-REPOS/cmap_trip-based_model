      SUBROUTINE DATA3(ORIG)
      IMPLICIT INTEGER (A-Z)
C*******************
C  THIS SUBROUTINE PROCESSES THE EMME DATABANK
C     IT IS CALLED ONCE FOR EACH ZONE SELECTED FOR ANALYSIS
C
C  THE EMMEBANK CONTAINS THE FOLLOWING FULL MATRICES
C       MF01 = TRANSIT FIRST MODES (FMD)
C       MF02 = TRANSIT LAST MODES (LMD)
C       MF03 = TRANSIT IN VEHICLE TIMES IN MINUTES (IVT)
C       MF04 = TRANSIT OUT OF VEHICLE TIMES IN MINUTES (OVT)
C       MF05 = TRANSIT HEADWAY OF FIRST MODE IN MINUTES (HWAY)
C       MF06 = TRANSIT PRIORITY MODE (PMD)
C       MF07 = TRANSIT FARES IN CENTS (FARE)
C       MF08 = HIGHWAY TRAVEL TIME SKIM TREES IN MINUTES (ZLHT)
C       MF09 = HIGHWAY TRAVEL DISTANCE SKIM TREES IN MILES (ZLHD)
C*******************

	INCLUDE 'Common_params.fi'
	INCLUDE 'Common_emme4bank.fi'

C***********************************************************************
C
C     REOPEN EMMEBANK
C
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
C      OPEN (UNIT=32, FILE='EMMEBANK',
C     A  ACCESS='DIRECT',RECL=1,STATUS='OLD')
C*******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
      OPEN (UNIT=901, FILE='emmemat/'//TEMX_FMD,
     A  ACCESS='DIRECT',RECL=1,STATUS='OLD')
      OPEN (UNIT=902, FILE='emmemat/'//TEMX_LMD,
     A  ACCESS='DIRECT',RECL=1,STATUS='OLD')
      OPEN (UNIT=903, FILE='emmemat/'//TEMX_IVT,
     A  ACCESS='DIRECT',RECL=1,STATUS='OLD')
      OPEN (UNIT=904, FILE='emmemat/'//TEMX_OVT,
     A  ACCESS='DIRECT',RECL=1,STATUS='OLD')
      OPEN (UNIT=905, FILE='emmemat/'//TEMX_HWAY,
     A  ACCESS='DIRECT',RECL=1,STATUS='OLD')
      OPEN (UNIT=906, FILE='emmemat/'//TEMX_PMD,
     A  ACCESS='DIRECT',RECL=1,STATUS='OLD')
      OPEN (UNIT=907, FILE='emmemat/'//TEMX_FARE,
     A  ACCESS='DIRECT',RECL=1,STATUS='OLD')
      OPEN (UNIT=908, FILE='emmemat/'//TEMX_HTIME,
     A  ACCESS='DIRECT',RECL=1,STATUS='OLD')      
      OPEN (UNIT=909, FILE='emmemat/'//TEMX_HDIST,
     A  ACCESS='DIRECT',RECL=1,STATUS='OLD')      
C***********************************************************************
C
C     READ TABLES BY ROW
C
C***********************************************************************
	P = ORIG
      emcent=mcent

	DO Q=1,ZONES
C
C     P TO Q DIRECTION
C       
        REC1 = ((P-1)*emcent) + Q
C##        WRITE (31, '(A,4I11)') '   P,Q,REC1,mcent=',p,q,rec1,emcent
        READ(901, REC=REC1) FMD(Q)
        READ(902, REC=REC1) LMD(Q)
        READ(903, REC=REC1) IVT(Q)
        READ(904, REC=REC1) OVT(Q)
        READ(905, REC=REC1) HWAY(Q)
        READ(906, REC=REC1) PMD(Q)
        READ(907, REC=REC1) FARE(Q)
        READ(908, REC=REC1) ZLHT(Q)
        READ(909, REC=REC1) ZLHD(Q)

      ENDDO
C
C     FOLLOWING CODE TO TRACE ERRORS
C
      IF (TRACE) THEN

        WRITE(31,9001) ORIG

        DO Z=1,ZONES
          WRITE(31,9007)  Z,FMD(Z),LMD(Z),IVT(Z),OVT(Z),HWAY(Z),
     *      PMD(Z),FARE(Z),ZLHT(Z),ZLHD(Z)
        ENDDO
      ENDIF

 9001 FORMAT(' FOR ORIGIN ZONE ',I4,' THE EMME MATRIX DATA IS',/,
     +    ' ZONE       FMD       LMD       IVT       OVT',
     +   '      HWAY       PMD      FARE      ZLHT      ZLHD')
 9007 FORMAT(' ',I4,9F10.2)

      CLOSE (901)
      CLOSE (902)
      CLOSE (903)
      CLOSE (904)
      CLOSE (905)
      CLOSE (906)
      CLOSE (907)
      CLOSE (908)
      CLOSE (909)

      RETURN
      END