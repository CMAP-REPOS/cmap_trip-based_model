      SUBROUTINE DATA1
      IMPLICIT INTEGER (A-Z)
C***********************************************************************
C
C  THE DATA1 SUBROUTINE WILL READ NAMELISTS
C                            SET DEFAULT VALUES
C                            OBTAIN THE RANDOM NUMBER SEED
C
C           TITLE = TITLE DESCRIBING MODEL RUN (80 CHARACTERS MAX)
C
C &PARAM  ZONES  = HIGHEST ZONE NUMBER ON THE M01 INPUT FILE (4000 MAX)
C                  MUST BE LESS THAN OR EQUAL TO THE EMME2BANK ZONES
C         BEGORIG = BEGINNING ORIGIN ZONE IN RANGE TO PROCESS
C         ENDORIG = ENDING ORIGIN ZONE IN RANGE TO PROCESS
C         RNSEED = RANDOM NUMBER SEED (ANY INTEGER VALUE BETWEEN ZERO
C         HOV    = INDICATES THAT THE PROGRAM IS EXPECTING HOME-WORK
C                  TRIPS BY AUTO OCCUPANCY LEVEL (FALSE)
C        WORKER$ = LOW AND HIGH INCOME HOME-WORK TRIPS WILL BE INPUT
C                  (FALSE)
C
C &TAB_IN
C          INTABLE_HW = EMMEBANK INPUT TABLE WITH AUTO HOME-WORK TRIPS,
C                       DEFAULT IS 201
C        INTABLE_HW_1 = EMMEBANK INPUT TABLE WITH DRIVE ALONE AUTO 
C                       HOME-WORK TRIPS, DEFAULT IS 202
C        INTABLE_HW_2 = EMMEBANK INPUT TABLE WITH TWO PERSON AUTO 
C                       HOME-WORK TRIPS, DEFAULT IS 203
C        INTABLE_HW_3 = EMMEBANK INPUT TABLE WITH THREE OR MORE PERSON 
C                       AUTO HOME-WORK TRIPS, DEFAULT IS 204
C    INTABLE_HW_LOW_1 = EMMEBANK INPUT TABLE WITH DRIVE ALONE AUTO 
C                       HOME-WORK LOW INCOME WORKER TRIPS, 
C                       DEFAULT IS 205
C    INTABLE_HW_LOW_2 = EMMEBANK INPUT TABLE WITH TWO PERSON AUTO 
C                       HOME-WORK LOW INCOME WORKER TRIPS, 
C                       DEFAULT IS 206
C    INTABLE_HW_LOW_3 = EMMEBANK INPUT TABLE WITH THREE OR MORE PEROSN
C                       AUTO HOME-WORK LOW INCOME WORKER TRIPS, 
C                       DEFAULT IS 207
C   INTABLE_HW_HIGH_1 = EMMEBANK INPUT TABLE WITH DRIVE ALONE AUTO 
C                       HOME-WORK HIGH INCOME WORKER TRIPS, 
C                       DEFAULT IS 208
C   INTABLE_HW_HIGH_2 = EMMEBANK INPUT TABLE WITH TWO PERSON AUTO 
C                       HOME-WORK HIGH INCOME WORKER TRIPS, 
C                       DEFAULT IS 209
C   INTABLE_HW_HIGH_3 = EMMEBANK INPUT TABLE WITH THREE OR MORE PEROSN
C                       AUTO HOME-WORK HIGH INCOME WORKER TRIPS, 
C                       DEFAULT IS 210
C          INTABLE_HO = EMMEBANK INPUT TABLE WITH AUTO HOME-OTHER TRIPS,
C                       DEFAULT IS 211
C          INTABLE_NH = EMMEBANK INPUT TABLE WITH AUTO NON-HOME TRIPS,
C                       DEFAULT IS 212
C
C &TABNUM_OUT
C       OUTTABLE_HW_1 = EMMEBANK OUTPUT TABLE WITH DRIVE ALONE AUTO 
C                       HOME-WORK TRIPS (ONLY WHEN HOV IS FALSE),
C                       DEFAULT IS 213
C       OUTTABLE_HW_2 = EMMEBANK OUTPUT TABLE WITH TWO PERSON AUTO 
C                       HOME-WORK TRIPS (ONLY WHEN HOV IS FALSE),
C                       DEFAULT IS 214
C       OUTTABLE_HW_3 = EMMEBANK OUTPUT TABLE WITH THREE OR MORE PERSON
C                       AUTO HOME-WORK TRIPS (ONLY WHEN HOV IS FALSE),
C                       DEFAULT IS 215
C       OUTTABLE_HO_1 = EMMEBANK OUTPUT TABLE WITH DRIVE ALONE AUTO 
C                       HOME-OTHER TRIPS, DEFAULT IS 216
C       OUTTABLE_HO_2 = EMMEBANK OUTPUT TABLE WITH TWO PERSON AUTO 
C                       HOME-OTHER TRIPS, DEFAULT IS 217
C       OUTTABLE_HO_3 = EMMEBANK OUTPUT TABLE WITH THREE OR MORE PERSON
C                       AUTO HOME-OTHER TRIPS, DEFAULT IS 218
C       OUTTABLE_NH_1 = EMMEBANK OUTPUT TABLE WITH DRIVE ALONE AUTO 
C                       NON-HOME TRIPS, DEFAULT IS 219
C       OUTTABLE_NH_2 = EMMEBANK OUTPUT TABLE WITH TWO PERSON AUTO 
C                       NON-HOME TRIPS, DEFAULT IS 220
C       OUTTABLE_NH_3 = EMMEBANK OUTPUT TABLE WITH THREE OR MORE PERSON
C                       AUTO NON-HOME TRIPS, DEFAULT IS 221
C 
C## -- HEITHER, 11-10-2018: ADDED BEGORIG & ENDORIG TO ALLOW FOR SIMULTANEOUS PROCESSING IN BATCHES --
C***********************************************************************
	INCLUDE 'COMMON_PARAMS.FI'
C
	NAMELIST/PARAM/ ZONES, BEGORIG, ENDORIG, RNSEED, HOV, WORKER$

	NAMELIST/TAB_IN/ INTABLE_HW, INTABLE_HW_1, INTABLE_HW_2, 
     A  INTABLE_HW_3, INTABLE_HW_LOW_1, INTABLE_HW_LOW_2, 
     B  INTABLE_HW_LOW_3, INTABLE_HW_HIGH_1, INTABLE_HW_HIGH_2,
     C  INTABLE_HW_HIGH_3, INTABLE_HO, INTABLE_NH 
      
      NAMELIST/TAB_OUT/ OUTTABLE_HW_1, OUTTABLE_HW_2, OUTTABLE_HW_3,
     A  OUTTABLE_HO_1, OUTTABLE_HO_2, OUTTABLE_HO_3, OUTTABLE_NH_1,
     B  OUTTABLE_NH_2, OUTTABLE_NH_3

      LOGICAL*4 NLIST(3)/.FALSE.,.FALSE.,.FALSE./
	
	CHARACTER*8 CARD(3)/'  &PARAM',' &TAB_IN','&TAB_OUT'/
 
	CHARACTER*1 ASTERIX(80)/80*'*'/

C ##### Heither, CMAP  11-07-2013
C #####  Code block implements Eash's changes to read Emme4 full matrix files.	
      CHARACTER*1 CTAB1
      CHARACTER*2 CTAB2
      CHARACTER*3 CTAB3
		
C
C     PROCESS NAMELIST CARDS
C
04100 READ(33,'(10A8)',END=04190) TITLE

      IF(TITLE(1) .EQ.' &PARAM ') NLIST(1)=.TRUE.
      IF(TITLE(1) .EQ.' &TAB_IN') NLIST(2)=.TRUE.
      IF(TITLE(1) .EQ.' &TAB_OU') NLIST(3)=.TRUE.

      GO TO 04100
C
04190 REWIND 33
C
	READ(33,'(10A8)',END=07220) TITLE
C
      WRITE (*,'(/A,10A8)') ' PROJECT:  ', TITLE
	WRITE (*,'(80A1)') ASTERIX
 
      WRITE (16,'(/A,10A8)') ' PROJECT:  ', TITLE
	WRITE (16,'(80A1)') ASTERIX
	
      IF(NLIST(1)) READ(33,PARAM,END=07220)
      IF(NLIST(2)) READ(33,TAB_IN,END=07220)
      IF(NLIST(3)) READ(33,TAB_OUT,END=07220)
C
07040 DO 7100 J=1,3
      IF(.NOT.NLIST(J)) WRITE(16,07230) CARD(J)
07230 FORMAT(/' (WARNING) CONTROL CARD ',A8,' IS MISSING.',/
     +        ' DEFAULT VALUES WILL BE ASSUMED.')
07100 CONTINUE
      REWIND (33)
      GOTO 07900
C
07220 WRITE(16,07200) CARD
07200 FORMAT(/' ERROR:  EARLY END OF FILE ENCOUNTERED WHILE'/
     +        '         READING CONTROL CARDS FROM UNIT 33.'/
     +        '         SEQUENCE OF CONTROL CARDS SHOULD'/
     +        '         BE AS FOLLOWS,'/
     +        '         ', 4(A8,', '))
      STOP 217
C
07900 CONTINUE
C*******************
C
C  IF WORKER$ IS TRUE THEN HOV MUST BE TRUE AS WELL
C
C*******************
    	IF ((LOW_INC) .OR. (HI_INC)) THEN
        IF ((.NOT. HOV2) .AND. (.NOT. HOV3)) THEN
	    WRITE (16,'(A)') ' ERROR:  INCOME OPTION ON WITHOUT HOV OPTION
     A ON'
	    STOP
        ENDIF
	ENDIF
C*******************
C  START REPORTING INPUT PARAMETERS
C*******************
	WRITE (16, '(/A)') ' INPUT PARAMETERS FROM NAMELISTS' 
	WRITE (16,'(80A1)') ASTERIX
C
 	WRITE (16,'(/A)') ' &PARAM'
	WRITE (16,'(A,I5,A)') '   ZONES=', ZONES, ' HIGHEST ZONE NUMBER'
	IF (ZONES .GT. 4000) THEN
	  WRITE (*,'(/A)') ' ERROR  ZONES IS GREATER THAN 4000'
	  STOP
	ENDIF
	WRITE (16,'(A,I5)') '   BEGINNING ORIGIN ZONE TO PROCESS=', BEGORIG
	WRITE (16,'(A,I5)') '   ENDING ORIGIN ZONE TO PROCESS=', ENDORIG
	WRITE (16,'(/A,I5,A)') '   RNSEED=', RNSEED, ' SEED FOR RANDOM NUM
     ABER GENERATION'
	WRITE (16,'(13X, A)')' (IF ZERO, DEFAULT DERIVED FROM CLOCK TIME)'
     
	WRITE(16,'(A,L1,A)') '   HOV= ',HOV,' INPUT AUTO OCCUPANCY HOME-WO
     ARK TRIP TABLES (F)'
	WRITE(16,'(A,L1,A)') '   WORKER$= ',WORKER$,' INPUT LOW/HIGH INCOM
     AE WORKER HOME-WORK TRIP TABLES (F)'
	WRITE (16,'(A)') ' &END'
C

C ##### Heither, CMAP  09-20-2013
C #####  Code block implements Eash's changes to read Emme4 full matrix files.
C     CREATE EXTERNAL MATRIX FILE NAMES
C
C    -- HW INPUT (HOV=F & WORKER$=F) --
      IF (INTABLE_HW .LT. 10) THEN
        WRITE (CTAB1,'(I1)') INTABLE_HW        
        TEMX_IN_HW = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((INTABLE_HW .GE. 10).AND.(INTABLE_HW .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') INTABLE_HW
        TEMX_IN_HW = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (INTABLE_HW .GE. 100) THEN
        WRITE (CTAB3,'(I3)') INTABLE_HW
        TEMX_IN_HW = 'mf'//CTAB3//'.emx'
      ENDIF      
C
C    -- HW INPUT (HOV=T & WORKER$=F) --
      IF (INTABLE_HW_1 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') INTABLE_HW_1
        TEMX_IN_HW_1 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((INTABLE_HW_1 .GE. 10).AND.(INTABLE_HW_1 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') INTABLE_HW_1
        TEMX_IN_HW_1 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (INTABLE_HW_1 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') INTABLE_HW_1
        TEMX_IN_HW_1 = 'mf'//CTAB3//'.emx'
      ENDIF
	  
      IF (INTABLE_HW_2 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') INTABLE_HW_2
        TEMX_IN_HW_2 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((INTABLE_HW_2 .GE. 10).AND.(INTABLE_HW_2 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') INTABLE_HW_2
        TEMX_IN_HW_2 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (INTABLE_HW_2 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') INTABLE_HW_2
        TEMX_IN_HW_2 = 'mf'//CTAB3//'.emx'
      ENDIF
	  
      IF (INTABLE_HW_3 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') INTABLE_HW_3
        TEMX_IN_HW_3 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((INTABLE_HW_3 .GE. 10).AND.(INTABLE_HW_3 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') INTABLE_HW_3
        TEMX_IN_HW_3 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (INTABLE_HW_3 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') INTABLE_HW_3
        TEMX_IN_HW_3 = 'mf'//CTAB3//'.emx'
      ENDIF	  
C
C    -- HW INPUT (HOV=T & WORKER$=T) --
C       -- LOW INCOME SOV INPUT --
      IF (INTABLE_HW_LOW_1 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') INTABLE_HW_LOW_1
        TEMX_IN_HW_LOW_1 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((INTABLE_HW_LOW_1.GE.10).AND.(INTABLE_HW_LOW_1 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') INTABLE_HW_LOW_1
        TEMX_IN_HW_LOW_1 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (INTABLE_HW_LOW_1 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') INTABLE_HW_LOW_1
        TEMX_IN_HW_LOW_1 = 'mf'//CTAB3//'.emx'
      ENDIF
C       -- LOW INCOME HOV2 INPUT --
      IF (INTABLE_HW_LOW_2 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') INTABLE_HW_LOW_2
        TEMX_IN_HW_LOW_2 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((INTABLE_HW_LOW_2.GE.10).AND.(INTABLE_HW_LOW_2 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') INTABLE_HW_LOW_2
        TEMX_IN_HW_LOW_2 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (INTABLE_HW_LOW_2 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') INTABLE_HW_LOW_2
        TEMX_IN_HW_LOW_2 = 'mf'//CTAB3//'.emx'
      ENDIF
C       -- LOW INCOME HOV3+ INPUT --
      IF (INTABLE_HW_LOW_3 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') INTABLE_HW_LOW_3
        TEMX_IN_HW_LOW_3 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((INTABLE_HW_LOW_3.GE.10).AND.(INTABLE_HW_LOW_3 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') INTABLE_HW_LOW_3
        TEMX_IN_HW_LOW_3 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (INTABLE_HW_LOW_3 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') INTABLE_HW_LOW_3
        TEMX_IN_HW_LOW_3 = 'mf'//CTAB3//'.emx'
      ENDIF
C       -- HIGH INCOME SOV INPUT --
      IF (INTABLE_HW_HIGH_1 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') INTABLE_HW_HIGH_1
        TEMX_IN_HW_HIGH_1 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((INTABLE_HW_HIGH_1.GE.10).AND.(INTABLE_HW_HIGH_1.LT.100)) THEN
        WRITE (CTAB2,'(I2)') INTABLE_HW_HIGH_1
        TEMX_IN_HW_HIGH_1 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (INTABLE_HW_HIGH_1 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') INTABLE_HW_HIGH_1
        TEMX_IN_HW_HIGH_1 = 'mf'//CTAB3//'.emx'
      ENDIF
C       -- HIGH INCOME HOV2 INPUT --
      IF (INTABLE_HW_HIGH_2 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') INTABLE_HW_HIGH_2
        TEMX_IN_HW_HIGH_2 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((INTABLE_HW_HIGH_2.GE.10).AND.(INTABLE_HW_HIGH_2.LT.100)) THEN
        WRITE (CTAB2,'(I2)') INTABLE_HW_HIGH_2
        TEMX_IN_HW_HIGH_2 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (INTABLE_HW_HIGH_2 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') INTABLE_HW_HIGH_2
        TEMX_IN_HW_HIGH_2 = 'mf'//CTAB3//'.emx'
      ENDIF
C       -- HIGH INCOME HOV3+ INPUT --
      IF (INTABLE_HW_HIGH_3 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') INTABLE_HW_HIGH_3
        TEMX_IN_HW_HIGH_3 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((INTABLE_HW_HIGH_3.GE.10).AND.(INTABLE_HW_HIGH_3.LT.100)) THEN
        WRITE (CTAB2,'(I2)') INTABLE_HW_HIGH_3
        TEMX_IN_HW_HIGH_3 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (INTABLE_HW_HIGH_3 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') INTABLE_HW_HIGH_3
        TEMX_IN_HW_HIGH_3 = 'mf'//CTAB3//'.emx'
      ENDIF
C
C    -- HO INPUT --
      IF (INTABLE_HO .LT. 10) THEN
        WRITE (CTAB1,'(I1)') INTABLE_HO
        TEMX_IN_HO = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((INTABLE_HO .GE. 10).AND.(INTABLE_HO .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') INTABLE_HO
        TEMX_IN_HO = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (INTABLE_HO .GE. 100) THEN
        WRITE (CTAB3,'(I3)') INTABLE_HO
        TEMX_IN_HO = 'mf'//CTAB3//'.emx'
      ENDIF
C
C    -- NH INPUT --
      IF (INTABLE_NH .LT. 10) THEN
        WRITE (CTAB1,'(I1)') INTABLE_NH
        TEMX_IN_NH = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((INTABLE_NH .GE. 10).AND.(INTABLE_NH .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') INTABLE_NH
        TEMX_IN_NH = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (INTABLE_NH .GE. 100) THEN
        WRITE (CTAB3,'(I3)') INTABLE_NH
        TEMX_IN_NH = 'mf'//CTAB3//'.emx'
      ENDIF	  
C
C    -- HW OUTPUT --
      IF (OUTTABLE_HW_1 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') OUTTABLE_HW_1
        TEMX_OUT_HW_1 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((OUTTABLE_HW_1 .GE. 10).AND.(OUTTABLE_HW_1 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') OUTTABLE_HW_1
        TEMX_OUT_HW_1 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (OUTTABLE_HW_1 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') OUTTABLE_HW_1
        TEMX_OUT_HW_1 = 'mf'//CTAB3//'.emx'
      ENDIF
	  
      IF (OUTTABLE_HW_2 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') OUTTABLE_HW_2
        TEMX_OUT_HW_2 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((OUTTABLE_HW_2 .GE. 10).AND.(OUTTABLE_HW_2 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') OUTTABLE_HW_2
        TEMX_OUT_HW_2 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (OUTTABLE_HW_2 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') OUTTABLE_HW_2
        TEMX_OUT_HW_2 = 'mf'//CTAB3//'.emx'
      ENDIF
	  
      IF (OUTTABLE_HW_3 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') OUTTABLE_HW_3
        TEMX_OUT_HW_3 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((OUTTABLE_HW_3 .GE. 10).AND.(OUTTABLE_HW_3 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') OUTTABLE_HW_3
        TEMX_OUT_HW_3 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (OUTTABLE_HW_3 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') OUTTABLE_HW_3
        TEMX_OUT_HW_3 = 'mf'//CTAB3//'.emx'
      ENDIF	  
C
C    -- HO OUTPUT --
      IF (OUTTABLE_HO_1 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') OUTTABLE_HO_1
        TEMX_OUT_HO_1 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((OUTTABLE_HO_1 .GE. 10).AND.(OUTTABLE_HO_1 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') OUTTABLE_HO_1
        TEMX_OUT_HO_1 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (OUTTABLE_HO_1 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') OUTTABLE_HO_1
        TEMX_OUT_HO_1 = 'mf'//CTAB3//'.emx'
      ENDIF
	  
      IF (OUTTABLE_HO_2 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') OUTTABLE_HO_2
        TEMX_OUT_HO_2 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((OUTTABLE_HO_2 .GE. 10).AND.(OUTTABLE_HO_2 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') OUTTABLE_HO_2
        TEMX_OUT_HO_2 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (OUTTABLE_HO_2 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') OUTTABLE_HO_2
        TEMX_OUT_HO_2 = 'mf'//CTAB3//'.emx'
      ENDIF
	  
      IF (OUTTABLE_HO_3 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') OUTTABLE_HO_3
        TEMX_OUT_HO_3 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((OUTTABLE_HO_3 .GE. 10).AND.(OUTTABLE_HO_3 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') OUTTABLE_HO_3
        TEMX_OUT_HO_3 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (OUTTABLE_HO_3 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') OUTTABLE_HO_3
        TEMX_OUT_HO_3 = 'mf'//CTAB3//'.emx'
      ENDIF	  
C
C    -- NH OUTPUT --
      IF (OUTTABLE_NH_1 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') OUTTABLE_NH_1
        TEMX_OUT_NH_1 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((OUTTABLE_NH_1 .GE. 10).AND.(OUTTABLE_NH_1 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') OUTTABLE_NH_1
        TEMX_OUT_NH_1 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (OUTTABLE_NH_1 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') OUTTABLE_NH_1
        TEMX_OUT_NH_1 = 'mf'//CTAB3//'.emx'
      ENDIF
	  
      IF (OUTTABLE_NH_2 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') OUTTABLE_NH_2
        TEMX_OUT_NH_2 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((OUTTABLE_NH_2 .GE. 10).AND.(OUTTABLE_NH_2 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') OUTTABLE_NH_2
        TEMX_OUT_NH_2 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (OUTTABLE_NH_2 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') OUTTABLE_NH_2
        TEMX_OUT_NH_2 = 'mf'//CTAB3//'.emx'
      ENDIF
	  
      IF (OUTTABLE_NH_3 .LT. 10) THEN
        WRITE (CTAB1,'(I1)') OUTTABLE_NH_3
        TEMX_OUT_NH_3 = 'mf'//CTAB1//'.emx'
      ENDIF  

      IF ((OUTTABLE_NH_3 .GE. 10).AND.(OUTTABLE_NH_3 .LT. 100)) THEN
        WRITE (CTAB2,'(I2)') OUTTABLE_NH_3
        TEMX_OUT_NH_3 = 'mf'//CTAB2//'.emx'
      ENDIF  

      IF (OUTTABLE_NH_3 .GE. 100) THEN
        WRITE (CTAB3,'(I3)') OUTTABLE_NH_3
        TEMX_OUT_NH_3 = 'mf'//CTAB3//'.emx'
      ENDIF	  
C# #####
C ##### Heither, CMAP  10-31-2014: fix write params      
C
 	WRITE (16,'(/A)') ' &TAB_IN'
	WRITE (16,'(A,I3,A,A)') '   AUTO HOME-WORK TRIPS= MF',
     A  INTABLE_HW,': ',TEMX_IN_HW	
	WRITE (16,'(A,I3,A,A)') '   DRIVE ALONE AUTO HOME-WORK TRIPS= MF',
     A  INTABLE_HW_1,': ',TEMX_IN_HW_1
      WRITE (16,'(A,I3,A,A)') '   TWO PERSON AUTO HOME-WORK TRIPS= MF',
     A  INTABLE_HW_2,': ',TEMX_IN_HW_2
	WRITE (16,'(A,I3,A,A)') '   THREE OR MORE PERSON AUTO HOME-WORK TRIPS
     A= MF',  INTABLE_HW_3,': ',TEMX_IN_HW_3
	WRITE (16,'(A,I3,A,A)') '   DRIVE ALONE AUTO HOME-WORK LOW INCOME WORK
     AER TRIPS= MF', INTABLE_HW_LOW_1,': ',TEMX_IN_HW_LOW_1
	WRITE (16,'(A,I3,A,A)') '   TWO PERSON AUTO HOME-WORK LOW INCOME WORKE
     AR TRIPS= MF', INTABLE_HW_LOW_2,': ',TEMX_IN_HW_LOW_2
	WRITE (16,'(A,I3,A,A)') '   THREE OR MORE PERSON AUTO HOME-WORK LOW IN
     ACOME WORKER TRIPS= MF', INTABLE_HW_LOW_3,': ',TEMX_IN_HW_LOW_3
	WRITE (16,'(A,I3,A,A)') '   DRIVE ALONE AUTO HOME-WORK HIGH INCOME WOR
     AKER TRIPS= MF', INTABLE_HW_HIGH_1,': ',TEMX_IN_HW_HIGH_1
	WRITE (16,'(A,I3,A,A)') '   TWO PERSON AUTO HOME-WORK HIGH INCOME WORK
     AER TRIPS= MF', INTABLE_HW_HIGH_2,': ',TEMX_IN_HW_HIGH_2
	WRITE (16,'(A,I3,A,A)') '   THREE OR MORE PERSON AUTO HOME-WORK HIGH I
     ANCOME WORKER TRIPS= MF', INTABLE_HW_HIGH_3,': ',TEMX_IN_HW_HIGH_3
	WRITE (16,'(A,I3,A,A)') '   AUTO HOME-OTHER TRIPS= MF',
     A  INTABLE_HO,': ',TEMX_IN_HO		
	WRITE (16,'(A,I3,A,A)') '   AUTO NON-HOME TRIPS= MF',
     A  INTABLE_NH,': ',TEMX_IN_NH		
	WRITE (16,'(A)') ' &END'
C
 	WRITE (16,'(/A)') ' &TAB_OUT'     
	WRITE (16,'(A,I3,A,A)') '   DRIVE ALONE AUTO HOME-WORK TRIPS WHEN HOV 
     AIS FALSE= MF', OUTTABLE_HW_1,': ',TEMX_OUT_HW_1
	WRITE (16,'(A,I3,A,A)') '   TWO PERSON AUTO HOME-WORK TRIPS WHEN HOV I
     AS FALSE= MF', OUTTABLE_HW_2,': ',TEMX_OUT_HW_2
	WRITE (16,'(A,I3,A,A)') '   THREE OR MORE PERSON AUTO HOME-WORK TRIPS 
     AWHEN HOV IS FALSE= MF', OUTTABLE_HW_3,': ',TEMX_OUT_HW_3
	WRITE (16,'(A,I3,A,A)') '   DRIVE ALONE AUTO HOME-OTHER TRIPS= MF',
     A OUTTABLE_HO_1,': ',TEMX_OUT_HO_1
 	WRITE (16,'(A,I3,A,A)') '   TWO PERSON AUTO HOME-OTHER TRIPS= MF',
     A OUTTABLE_HO_2,': ',TEMX_OUT_HO_2
	WRITE (16,'(A,I3,A,A)') '   THREE OR MORE PERSON AUTO HOME-OTHER TRIPS
     A= MF', OUTTABLE_HO_3,': ',TEMX_OUT_HO_3
	WRITE (16,'(A,I3,A,A)') '   DRIVE ALONE AUTO NON-HOME TRIPS= MF',
     A OUTTABLE_NH_1,': ',TEMX_OUT_NH_1
 	WRITE (16,'(A,I3,A,A)') '   TWO PERSON AUTO NON-HOME TRIPS= MF',
     A OUTTABLE_NH_2,': ',TEMX_OUT_NH_2
	WRITE (16,'(A,I3,A,A)') '   THREE OR MORE PERSON AUTO NON-HOME TRIPS= 
     AMF', OUTTABLE_NH_3,': ',TEMX_OUT_NH_3
	WRITE (16,'(A)') ' &END'

	WRITE (16,'(80A1)') ASTERIX
C
      RETURN
      END