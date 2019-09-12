C***********************************************************************
C        1         2         3         4         5         6         7 
C23456789012345678901234567890123456789012345678901234567890123456789012
C***********************************************************************

      SUBROUTINE OPEN_AUTO_EMME4

C***********************************************************************
C
C     THIS SUBROUTINE OPENS THE OPTIONAL AUTO INPUT EMME DATA BANK
C     AND LOADS COMMON
C
C***********************************************************************
C
C     REVISED FROM EXISTING CODE BY EASH (SEPTEMBER 2001)
C
C***********************************************************************
C
C     NOTE THAT EMME FILE NUMBERS ARE +1 FROM INRO MANUAL
C
C***********************************************************************

      IMPLICIT INTEGER*4 (A-Z) 
          
      INCLUDE 'Common_auto_emme4bank.fi' 

	CHARACTER*1 ASTERIX(100)/100*'*'/
	CHARACTER*8 DATE8
      CHARACTER*10  CTIME10
      CHARACTER*2 DATE2(4), CTIME2(5)
      EQUIVALENCE (DATE8,DATE2(1)), (CTIME10,CTIME2(1))

      INTEGER*4 cflag(999)
C
C     OPEN EMMEBANK FOR INPUT (UNIT=42)
C
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
      OPEN (UNIT=42, FILE='EMMEBANK',
     A  ACCESS='DIRECT',RECL=1,STATUS='OLD', ERR=992)
C*******************  RWE CHANGE FOR I290 OCTOBER 2009  ****************
C
      CALL DATE_AND_TIME(DATE8,CTIME10)
      WRITE (*,'(/15A)') ' AUTO_EMMEBANK OPENED:  ',
     A DATE2(4),'/',DATE2(3),'/',DATE2(1), DATE2(2),'  ', 
     B CTIME2(1),':', CTIME2(2),':', CTIME2(3)
      WRITE (31,'(/15A)') ' AUTO_EMMEBANK OPENED:  ',
     A DATE2(4),'/',DATE2(3),'/',DATE2(1), DATE2(2),'  ', 
     B CTIME2(1),':', CTIME2(2),':', CTIME2(3)
C
C     INITIALIZE EMMEBANK COMMON AREAS
C
      DO NUM=1,200

        AUTO_EMME_FILE_OFFSET(NUM) = -99
        AUTO_EMME_FILE_TYPE(NUM) = -99
        AUTO_EMME_FILE_NUM_RECORDS(NUM) = -99
        AUTO_EMME_FILE_WORD_RECORD(NUM) = - 99   

      ENDDO
C
C     READ FIRST 512 WORDS IN EMMEBANK
C
      DO 28 WORD=1,100

      REC1 = WORD
      REC2 = WORD+100
      REC3 = WORD+200
      REC4 = WORD+300

      IF (WORD .GT. 1) THEN

        READ (42, REC=REC1) AUTO_EMME_FILE_OFFSET(WORD)
        READ (42, REC=REC2) AUTO_EMME_FILE_NUM_RECORDS(WORD)
        READ (42, REC=REC3) AUTO_EMME_FILE_WORD_RECORD(WORD)
        READ (42, REC=REC4) AUTO_EMME_FILE_TYPE(WORD)

      ENDIF

   28 CONTINUE
C
C     READ DATABANK DIMENSIONS FROM EMMEBANK
C
      FILE = 2

      REC1 = AUTO_EMME_FILE_OFFSET(FILE) + 51

      READ (42, REC=REC1) AUTO_mscen
      REC1 = REC1 + 1
      READ (42, REC=REC1) AUTO_mcent
      REC1 = REC1 + 1
      READ (42, REC=REC1) AUTO_mnode
      REC1 = REC1 + 1
      READ (42, REC=REC1) AUTO_mlink
      REC1 = REC1 + 1
      READ (42, REC=REC1) AUTO_mturn
      REC1 = REC1 + 1
      READ (42, REC=REC1) AUTO_mline
      REC1 = REC1 + 1
      READ (42, REC=REC1) AUTO_mlseg
      REC1 = REC1 + 1
      READ (42, REC=REC1) AUTO_mmat
      REC1 = REC1 + 1
      READ (42, REC=REC1) AUTO_mfunc
      REC1 = REC1 + 1
      READ (42, REC=REC1) AUTO_moper
C
C     READ PROJECT TITLE FROM AUTO EMMEBANK
C
      FILE = 3
      DO 39 WORD = 1,40
      REC1 = AUTO_EMME_FILE_OFFSET(FILE) + WORD
      READ (42, REC=REC1) AUTO_iptit(WORD)
   39 CONTINUE  
C
C     WRITE OUT EMMEBANK DATA
C
      WRITE (31,'(/A)') ' AUTO EMMEBANK DATABANK'
	WRITE (31,'(100A1)') ASTERIX

      WRITE (31,1002) (AUTO_iptit(WORD), WORD=1,40)
 1002 FORMAT (' AUTO DATABANK TITLE:  '40A2)
      WRITE(31,'(A,I5)')'   MAX NUMBER OF SCENARIOS= ', AUTO_mscen  
      WRITE(31,'(A,I5)')'   MAX NUMBER OF CENTROIDS= ', AUTO_mcent
      WRITE(31,'(A,I5)')'   MAX NUMBER OF NODES= ', AUTO_mnode
      WRITE(31,'(A,I5)')'   MAX NUMBER OF LINKS= ', AUTO_mlink
      WRITE(31,'(A,I5)')'   MAX LENGTH OF TURN PENALTY TABLE= ', 
     A  AUTO_mturn
      WRITE(31,'(A,I5)')'   MAX NUMBER OF TRANSIT LINES= ', AUTO_mline
      WRITE(31,'(A,I5)')'   MAX TOTAL LINE SEGMENTS= ', AUTO_mlseg       
      WRITE(31,'(A,I5)')'   MAX NUMBER OF MATRICES= ', AUTO_mmat
      WRITE(31,'(A,I5)')'   MAX NUMBER OF FUNCTIONS/CLASS= ', AUTO_mfunc
      WRITE(31,'(A,I5)')'   MAX NUMBER OF OPERATORS/FUNCTION CLASS= ',
     A AUTO_moper
C
      WRITE (31,'(/A)')  '   FILE  TYPE    OFFSET  WORD/REC   RECORDS'

      DO I=1,100
        IF (I .GT. 1) THEN
          FILE = I-1
          WRITE (31, '(I7, I6,3I10)') FILE, AUTO_EMME_FILE_TYPE(I),
     A      AUTO_EMME_FILE_OFFSET(I),
     B      AUTO_EMME_FILE_WORD_RECORD(I),
     C      AUTO_EMME_FILE_NUM_RECORDS(I)
        ENDIF
C 
      ENDDO
C***********************************************************************
C
C     CHECK TO SEE WHETHER MATRICES ARE STORED CORRECTLY
C
      DO I=1,AUTO_mmat

        J = 3*AUTO_mmat+I   

        REC1 = AUTO_EMME_FILE_OFFSET(61) + J
        READ (42, REC=REC1 ) cflag(I)

        IF (cflag(I) .GT. 1) THEN
          WRITE (31,'(A)') ' ERROR:  UNSUITABLE (COLUMNWISE) MATRIX mf', 
     A      I
          PAUSE
        ENDIF 

      ENDDO
C
	CLOSE (42)
C
      RETURN

  992 CONTINUE
      WRITE (31,'(A)') ' ERROR:  CANNOT OPEN UNIT 42 AUTO EMMEBANK'

      STOP 
C
      END