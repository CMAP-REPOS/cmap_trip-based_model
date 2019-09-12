      BLOCK DATA  
C
C     DEFAULT VALUES FOR PROGRAM PARAMETERS
C	
      INCLUDE 'COMMON_PARAMS.FI'

C     DEFAULTS FOR &PARAM NAMELIST
C############ Heither, 07-11-2018: updated to support new zones
      DATA ZONES/3632/
	DATA BEGORIG/1/
      DATA ENDORIG/3632/
	DATA RNSEED/0/
	DATA HOV/.FALSE./
      DATA WORKER$/.FALSE./
C
C     DEFAULTS FOR &TAB_IN NAMELIST
C
      DATA INTABLE_HW/201/
      DATA INTABLE_HW_1/202/
      DATA INTABLE_HW_2/203/
      DATA INTABLE_HW_3/204/
      DATA INTABLE_HW_LOW_1/205/
      DATA INTABLE_HW_LOW_2/206/
      DATA INTABLE_HW_LOW_3/207/
      DATA INTABLE_HW_HIGH_1/208/
      DATA INTABLE_HW_HIGH_2/209/
      DATA INTABLE_HW_HIGH_3/210/
      DATA INTABLE_HO/211/
      DATA INTABLE_NH/212/
C
C     DEFAULTS FOR &TAB_OUT NAMELIST
C
      DATA OUTTABLE_HW_1/213/
      DATA OUTTABLE_HW_2/214/
      DATA OUTTABLE_HW_3/215/
      DATA OUTTABLE_HO_1/216/
      DATA OUTTABLE_HO_2/217/
      DATA OUTTABLE_HO_3/218/
      DATA OUTTABLE_NH_1/219/
      DATA OUTTABLE_NH_2/220/
      DATA OUTTABLE_NH_3/221/

      END