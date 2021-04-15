      SUBROUTINE RNORM(XBAR,SIGMA,RV)
C## Revised for vectorized calculations - Heither, 11-02-2017
      INCLUDE 'Common_params.fi'
      REAL,DIMENSION (ITER) :: R1,R2
      REAL,INTENT(OUT),DIMENSION (ITER) :: RV
      REAL,DIMENSION (ITER) :: V2,V,ZEE,SIGN
      
      CALL RANDOM_NUMBER(R1) 
	WHERE(R1<0.0001) R1=0.0001
	WHERE(R1>0.9999) R1=0.9999   
	CALL RANDOM_NUMBER(R2)
	WHERE(R2<0.0001) R2=0.0001
	WHERE(R2>0.9999) R2=0.9999
      
	  
      V2 = -2. * ALOG(.5 * (1. - ABS(1. - 2 * R1)))
      V = SQRT(V2)
      WHERE(R2.EQ..5) R2 = 1.5
      ZEE = V - (2.515517 + .802853 * V + .010328 * V2) /
     A      (1. + 1.432788 * V + .189269 * V2 + .001308 * V * V2)
      SIGN = (R2 - .5) / ABS(R2 - .5)
      RV = XBAR + SIGN * SIGMA * ZEE

C
	IF (TRACE) THEN
  	  WRITE (31, '(A,4F8.4,F8.0,2F8.4)')          
     A  '   RNORM PARAMETERS (R1,R2,RMEAN{xbar},SD{SIGMA},RV,SIGN,ZEE)='
     B    ,R1(1),R2(1),XBAR,SIGMA,RV(1),SIGN(1),ZEE(1)
  	  WRITE (31, '(A,4F8.4,F8.0)')
     A  '   2ND RNORM PARAMETERS (R1,R2,RMEAN,SD,RV)=',R1(2),R2(2),
     B      XBAR,SIGMA,RV(2)        
	ENDIF	  
      RETURN
      END