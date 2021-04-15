      SUBROUTINE SUB_ROWCOL
      IMPLICIT INTEGER(A-Z)
C***********************************************************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C***********************************************************************
C
C     SHORT NEW SUBROUTINE TO LIST HOUSEHOLD TYPE DISTRIBUTION VECTORS.
C
C     EASH, APRIL 2012
C
C***********************************************************************
	INCLUDE 'COMMON_HHSUMS.FI'
      INCLUDE 'ROW_COL.FI'
      
      REAL*4 ATOTAL(21), WTOTAL(26), CTOTAL(16), ITOTAL(31), HHTOTAL(21)
      REAL*4 TOTAL
C
      WRITE (16,'(/A)') 'ADULT, WORKER, CHILDREN, INCOME AND AGE OF HOUS
     AEHOLDER DISTRIBUTION TABLES'
      WRITE (16,'(A)')  'CONVERSION FROM TG SUBZONE AVERAGES TO CATEGORY
     A TOTALS'
C
C     AVERAGE ADULTS IN HH AND HH SIZE DISTRIBUTIONS
C
      WRITE (16,'(/A)')  'HOUSEHOLDS BY ADULTS SIXTEEN AND OLDER'
	
	WRITE (16,'(/A)')  'MATRIX BALANCING AREA 1'
	WRITE (16,'(A)')   '(INNER CHICAGO)'
      WRITE (16,'(A)')   ' SZ AVG   1 ADULT   2 ADULT   3 ADULT  4+ ADUL
     AT     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'
C
      DO I=1,21
        TOTAL = 0.0
	  ATOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + ADULT_SA1(II,I)
        ENDDO
	  DO II=1,4
	    ADULT_SA1(II,I) = ADULT_SA1(II,I)/TOTAL
          ATOTAL(I) = ATOTAL(I) + ADULT_SA1(II,I)
        ENDDO
        WRITE (16,6002) MNADLT(I), (ADULT_SA1(J,I), J=1,4), ATOTAL(I)
      ENDDO

 6002 FORMAT (F7.1, 5F10.3)
C
      WRITE (16,'(/A)')  'MATRIX BALANCING AREA 2'
	WRITE (16,'(A)')   '(OUTER CHICAGO AND INNER SUBURBS)'
      WRITE (16,'(A)')   ' SZ AVG   1 ADULT   2 ADULT   3 ADULT  4+ ADUL
     AT     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'
C
      DO I=1,21
        TOTAL = 0.0
	  ATOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + ADULT_SA2(II,I)
        ENDDO
	  DO II=1,4
	    ADULT_SA2(II,I) = ADULT_SA2(II,I)/TOTAL
          ATOTAL(I) = ATOTAL(I) + ADULT_SA2(II,I)
        ENDDO
        WRITE (16,6002) MNADLT(I), (ADULT_SA2(J,I), J=1,4), ATOTAL(I)
      ENDDO
C
      WRITE (16,'(/A)')  'MATRIX BALANCING AREA 3'
	WRITE (16,'(A)')   '(MID SUBURBAN)'
      WRITE (16,'(A)')   ' SZ AVG   1 ADULT   2 ADULT   3 ADULT  4+ ADUL
     AT     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'
C
      DO I=1,21
        TOTAL = 0.0
	  ATOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + ADULT_SA3(II,I)
        ENDDO
	  DO II=1,4
	    ADULT_SA3(II,I) = ADULT_SA3(II,I)/TOTAL
          ATOTAL(I) = ATOTAL(I) + ADULT_SA3(II,I)
        ENDDO
        WRITE (16,6002) MNADLT(I), (ADULT_SA3(J,I), J=1,4), ATOTAL(I)
      ENDDO
C
      WRITE (16,'(/A)')  'MATRIX BALANCING AREA 4'
	WRITE (16,'(A)')   '(FRINGE AND EXTERNAL COUNTIES)'
      WRITE (16,'(A)')   ' SZ AVG   1 ADULT   2 ADULT   3 ADULT  4+ ADUL
     AT     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'
C
      DO I=1,21
        TOTAL = 0.0
	  ATOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + ADULT_SA4(II,I)
        ENDDO
	  DO II=1,4
	    ADULT_SA4(II,I) = ADULT_SA4(II,I)/TOTAL
          ATOTAL(I) = ATOTAL(I) + ADULT_SA4(II,I)
        ENDDO
        WRITE (16,6002) MNADLT(I), (ADULT_SA4(J,I), J=1,4), ATOTAL(I)
      ENDDO
C
C     AVERAGE WORKERS IN HH AND HH SIZE DISTRIBUTIONS
C
      WRITE (16,'(/A)')  'HOUSEHOLDS BY WORKERS'

	WRITE (16,'(/A)')  'MATRIX BALANCING AREA 1'
	WRITE (16,'(A)')   '(INNER CHICAGO)'
      WRITE (16,'(A)')   ' SZ AVG  0 WORKER  1 WORKER  2 WORKER 3+ WORKE
     AR     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'

      DO I=1,26
        TOTAL = 0.0
	  WTOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + WORKER_SA1(II,I)
        ENDDO
	  DO II=1,4
	    WORKER_SA1(II,I) = WORKER_SA1(II,I)/TOTAL
          WTOTAL(I) = WTOTAL(I) + WORKER_SA1(II,I)
        ENDDO
        WRITE (16,6002) MNWORK(I), (WORKER_SA1(J,I), J=1,4), WTOTAL(I)
      ENDDO
C
      WRITE (16,'(/A)')  'MATRIX BALANCING AREA 2'
	WRITE (16,'(A)')   '(OUTER CHICAGO AND INNER SUBURBS)'
      WRITE (16,'(A)')   ' SZ AVG  0 WORKER  1 WORKER  2 WORKER 3+ WORKE
     AR     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'

      DO I=1,26
        TOTAL = 0.0
	  WTOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + WORKER_SA2(II,I)
        ENDDO
	  DO II=1,4
	    WORKER_SA2(II,I) = WORKER_SA2(II,I)/TOTAL
          WTOTAL(I) = WTOTAL(I) + WORKER_SA2(II,I)
        ENDDO
        WRITE (16,6002) MNWORK(I), (WORKER_SA2(J,I), J=1,4), WTOTAL(I)
      ENDDO
C
      WRITE (16,'(/A)')  'MATRIX BALANCING AREA 3'
	WRITE (16,'(A)')   '(MID SUBURBAN)'
      WRITE (16,'(A)')   ' SZ AVG  0 WORKER  1 WORKER  2 WORKER 3+ WORKE
     AR     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'

      DO I=1,26
        TOTAL = 0.0
	  WTOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + WORKER_SA3(II,I)
        ENDDO
	  DO II=1,4
	    WORKER_SA3(II,I) = WORKER_SA3(II,I)/TOTAL
          WTOTAL(I) = WTOTAL(I) + WORKER_SA3(II,I)
        ENDDO
        WRITE (16,6002) MNWORK(I), (WORKER_SA3(J,I), J=1,4), WTOTAL(I)
      ENDDO
C
      WRITE (16,'(/A)')  'MATRIX BALANCING AREA 4'
	WRITE (16,'(A)')   '(FRINGE AND EXTERNAL COUNTIES)'
      WRITE (16,'(A)')   ' SZ AVG  0 WORKER  1 WORKER  2 WORKER 3+ WORKE
     AR     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'

      DO I=1,26
        TOTAL = 0.0
	  WTOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + WORKER_SA4(II,I)
        ENDDO
	  DO II=1,4
	    WORKER_SA4(II,I) = WORKER_SA4(II,I)/TOTAL
          WTOTAL(I) = WTOTAL(I) + WORKER_SA4(II,I)
        ENDDO
        WRITE (16,6002) MNWORK(I), (WORKER_SA4(J,I), J=1,4), WTOTAL(I)
      ENDDO
C
C     AVERAGE CHILDREN IN HH AND HH SIZE DISTRIBUTIONS
C
      WRITE (16,'(/A)')  'HOUSEHOLDS BY CHILDREN'

	WRITE (16,'(/A)')  'MATRIX BALANCING AREA 1'
	WRITE (16,'(A)')   '(INNER CHICAGO)'
      WRITE (16,'(A)')   ' SZ AVG   0 CHILD   1 CHILD   2 CHILD  3+ CHIL
     AD     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'

      DO I=1,16
        TOTAL = 0.0
	  CTOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + CHILD_SA1(II,I)
        ENDDO
	  DO II=1,4
	    CHILD_SA1(II,I) = CHILD_SA1(II,I)/TOTAL
          CTOTAL(I) = CTOTAL(I) + CHILD_SA1(II,I)
        ENDDO
        WRITE (16,6002) MNCHLD(I), (CHILD_SA1(J,I), J=1,4), CTOTAL(I)
      ENDDO
C
      WRITE (16,'(/A)')  'MATRIX BALANCING AREA 2'
	WRITE (16,'(A)')   '(OUTER CHICAGO AND INNER SUBURBS)'
      WRITE (16,'(A)')   ' SZ AVG   0 CHILD   1 CHILD   2 CHILD  3+ CHIL
     AD     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'

      DO I=1,16
        TOTAL = 0.0
	  CTOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + CHILD_SA2(II,I)
        ENDDO
	  DO II=1,4
	    CHILD_SA2(II,I) = CHILD_SA2(II,I)/TOTAL
          CTOTAL(I) = CTOTAL(I) + CHILD_SA2(II,I)
        ENDDO
        WRITE (16,6002) MNCHLD(I), (CHILD_SA2(J,I), J=1,4), CTOTAL(I)
      ENDDO
C
      WRITE (16,'(/A)')  'MATRIX BALANCING AREA 3'
	WRITE (16,'(A)')   '(MID SUBURBAN)'
      WRITE (16,'(A)')   ' SZ AVG   0 CHILD   1 CHILD   2 CHILD  3+ CHIL
     AD     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'

      DO I=1,16
        TOTAL = 0.0
	  CTOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + CHILD_SA3(II,I)
        ENDDO
	  DO II=1,4
	    CHILD_SA3(II,I) = CHILD_SA3(II,I)/TOTAL
          CTOTAL(I) = CTOTAL(I) + CHILD_SA3(II,I)
        ENDDO
        WRITE (16,6002) MNCHLD(I), (CHILD_SA3(J,I), J=1,4), CTOTAL(I)
      ENDDO
C
      WRITE (16,'(/A)')  'MATRIX BALANCING AREA 4'
	WRITE (16,'(A)')   '(FRINGE AND EXTERNAL COUNTIES)'
      WRITE (16,'(A)')   ' SZ AVG   0 CHILD   1 CHILD   2 CHILD  3+ CHIL
     AD     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'

      DO I=1,16
        TOTAL = 0.0
	  CTOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + CHILD_SA4(II,I)
        ENDDO
	  DO II=1,4
	    CHILD_SA4(II,I) = CHILD_SA4(II,I)/TOTAL
          CTOTAL(I) = CTOTAL(I) + CHILD_SA4(II,I)
        ENDDO
        WRITE (16,6002) MNCHLD(I), (CHILD_SA4(J,I), J=1,4), CTOTAL(I)
      ENDDO
C
C     AVERAGE HOUSEHOLD INCOME AND HH INCOME QUARTILE DISTRIBUTIONS
C
      WRITE (16,'(/A)')  'HOUSEHOLDS BY INCOME QUARTILE'

	WRITE (16,'(/A)')  'MATRIX BALANCING AREA 1'
	WRITE (16,'(A)')   '(INNER CHICAGO)'
      WRITE (16,'(A)')   ' SZ AVG     FIRST    SECOND     THIRD    FOURT
     AH     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'

      DO I=1,31
        TOTAL = 0.0
	  ITOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + INCOME_SA1(II,I)
        ENDDO
	  DO II=1,4
	    INCOME_SA1(II,I) = INCOME_SA1(II,I)/TOTAL
          ITOTAL(I) = ITOTAL(I) + INCOME_SA1(II,I)
        ENDDO
        WRITE (16,6002) MNINC(I), (INCOME_SA1(J,I), J=1,4), ITOTAL(I)
      ENDDO
C
      WRITE (16,'(/A)')  'MATRIX BALANCING AREA 2'
	WRITE (16,'(A)')   '(OUTER CHICAGO AND INNER SUBURBS)'
      WRITE (16,'(A)')   ' SZ AVG     FIRST    SECOND     THIRD    FOURT
     AH     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'

      DO I=1,31
        TOTAL = 0.0
	  ITOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + INCOME_SA2(II,I)
        ENDDO
	  DO II=1,4
	    INCOME_SA2(II,I) = INCOME_SA2(II,I)/TOTAL
          ITOTAL(I) = ITOTAL(I) + INCOME_SA2(II,I)
        ENDDO
        WRITE (16,6002) MNINC(I), (INCOME_SA2(J,I), J=1,4), ITOTAL(I)
      ENDDO
C
      WRITE (16,'(/A)')  'MATRIX BALANCING AREA 3'
	WRITE (16,'(A)')   '(MID SUBURBAN)'
      WRITE (16,'(A)')   ' SZ AVG     FIRST    SECOND     THIRD    FOURT
     AH     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'

      DO I=1,31
        TOTAL = 0.0
	  ITOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + INCOME_SA3(II,I)
        ENDDO
	  DO II=1,4
	    INCOME_SA3(II,I) = INCOME_SA3(II,I)/TOTAL
          ITOTAL(I) = ITOTAL(I) + INCOME_SA3(II,I)
        ENDDO
        WRITE (16,6002) MNINC(I), (INCOME_SA3(J,I), J=1,4), ITOTAL(I)
      ENDDO
C
      WRITE (16,'(/A)')  'MATRIX BALANCING AREA 4'
	WRITE (16,'(A)')   '(FRINGE AND EXTERNAL COUNTIES)'
      WRITE (16,'(A)')   ' SZ AVG     FIRST    SECOND     THIRD    FOURT
     AH     TOTAL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-----------'

      DO I=1,31
        TOTAL = 0.0
	  ITOTAL(I) = 0.0
        DO II=1,4
          TOTAL = TOTAL + INCOME_SA4(II,I)
        ENDDO
	  DO II=1,4
	    INCOME_SA4(II,I) = INCOME_SA4(II,I)/TOTAL
          ITOTAL(I) = ITOTAL(I) + INCOME_SA4(II,I)
        ENDDO
        WRITE (16,6002) MNINC(I), (INCOME_SA4(J,I), J=1,4), ITOTAL(I)
      ENDDO
C
C     AVERAGE AGE OF HOUSEHOLDER CODE AND HH HOUSEHOLDER AGE DISTRIBUTIONS
C
      WRITE (16,'(/A)')  'HOUSEHOLDS BY AGE OF HOUSEHOLDER CODE'

	WRITE (16,'(/A)')  'MATRIX BALANCING AREA 1'
	WRITE (16,'(A)')   '(INNER CHICAGO)'
      WRITE (16,'(A)')   ' SZ AVG     16-35     36-64       65+     TOTA
     AL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-'

 6003 FORMAT (F7.1, 4F10.3 )
      
      DO I=1,21
        TOTAL = 0.0
	  HHTOTAL(I) = 0.0
        DO II=1,3
          TOTAL = TOTAL + HHOLDER_SA1(II,I)
        ENDDO
	  DO II=1,3
	    HHOLDER_SA1(II,I) = HHOLDER_SA1(II,I)/TOTAL
          HHTOTAL(I) = HHTOTAL(I) + HHOLDER_SA1(II,I)
        ENDDO
        WRITE (16,6003) MNHH(I), (HHOLDER_SA1(J,I), J=1,3), HHTOTAL(I)
      ENDDO
C
	WRITE (16,'(/A)')  'MATRIX BALANCING AREA 2'
	WRITE (16,'(A)')   '(OUTER CHICAGO AND INNER SUBURBS)'
      WRITE (16,'(A)')   ' SZ AVG     16-35     36-64       65+     TOTA
     AL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-'
      
      DO I=1,21
        TOTAL = 0.0
	  HHTOTAL(I) = 0.0
        DO II=1,3
          TOTAL = TOTAL + HHOLDER_SA2(II,I)
        ENDDO
	  DO II=1,3
	    HHOLDER_SA2(II,I) = HHOLDER_SA2(II,I)/TOTAL
          HHTOTAL(I) = HHTOTAL(I) + HHOLDER_SA2(II,I)
        ENDDO
        WRITE (16,6003) MNHH(I), (HHOLDER_SA2(J,I), J=1,3), HHTOTAL(I)
      ENDDO
C 
	WRITE (16,'(/A)')  'MATRIX BALANCING AREA 3'
	WRITE (16,'(A)')   '(MID SUBURBAN)'
      WRITE (16,'(A)')   ' SZ AVG     16-35     36-64       65+     TOTA
     AL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-'

      DO I=1,21
        TOTAL = 0.0
	  HHTOTAL(I) = 0.0
        DO II=1,3
          TOTAL = TOTAL + HHOLDER_SA3(II,I)
        ENDDO
	  DO II=1,3
	    HHOLDER_SA3(II,I) = HHOLDER_SA3(II,I)/TOTAL
          HHTOTAL(I) = HHTOTAL(I) + HHOLDER_SA3(II,I)
        ENDDO
        WRITE (16,6003) MNHH(I), (HHOLDER_SA3(J,I), J=1,3), HHTOTAL(I)
      ENDDO
C 
	WRITE (16,'(/A)')  'MATRIX BALANCING AREA 4'
	WRITE (16,'(A)')   '(FRINGE AND EXTERNAL COUNTIES)'
      WRITE (16,'(A)')   ' SZ AVG     16-35     36-64       65+     TOTA
     AL'
	WRITE (16,'(A)')   '----------------------------------------------
     A-'

      DO I=1,21
        TOTAL = 0.0
	  HHTOTAL(I) = 0.0
        DO II=1,3
          TOTAL = TOTAL + HHOLDER_SA4(II,I)
        ENDDO
	  DO II=1,3
	    HHOLDER_SA4(II,I) = HHOLDER_SA4(II,I)/TOTAL
          HHTOTAL(I) = HHTOTAL(I) + HHOLDER_SA4(II,I)
        ENDDO
        WRITE (16,6003) MNHH(I), (HHOLDER_SA4(J,I), J=1,3), HHTOTAL(I)
      ENDDO
C
      WRITE (16,'(/A)') 'END OF ROWCOL'
      WRITE (*,'(/A)') 'END OF ROWCOL'
      
      RETURN
      END