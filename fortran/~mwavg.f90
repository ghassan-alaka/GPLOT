!-------------------------------------------------------------------------
!   SUBROUTINE mwavg
!   -------------------
!   INPUT				- 3D field to be mass-weighted
!   OUTPUT				- 2D field that contains mass-weighted average
!   LEV					- 1D array of pressure levels
!	MAXLEV				- Maximum pressure level, used to calculate weights
!	NI					- Number of x-direction indices
!	NJ					- Number of y-direction indices
!	NK					- Number of z-direction indices

 SUBROUTINE mwavg (INPUT,OUTPUT,LEV,MAXLEV,NI,NJ,NK)

 IMPLICIT NONE

! Input/Output Variables
 INTEGER,	INTENT(IN)      :: NI, NJ, NK
 REAL,		INTENT(IN)      :: INPUT(NI,NJ,NK), LEV(NK), MAXLEV
 REAL,		INTENT(INOUT)	:: OUTPUT(NI,NJ)

! Local Variables
 REAL				:: WGT, TOTWGT
 INTEGER	:: I,J,K


! 1. Establish TOTWGT=0
	TOTWGT=0
	OUTPUT(1:NI,1:NJ)=0


! 2. Loop over all vertical levels
	DO K=1,NK

		! a. Calculate weight
		WGT=LEV(K)/MAXLEV
		TOTWGT=TOTWGT+WGT

		! b. Loop over horizontal direction to build mass-weighted total.
		DO J=1,NJ
		DO I=1,NI
			OUTPUT(I,J) = OUTPUT(I,J) + WGT*INPUT(I,J,K)
		ENDDO
		ENDDO
	ENDDO


! 3. Loop over horizontal direction to calculcate mass-weighted average.
	DO J=1,NJ
	DO I=1,NI
		OUTPUT(I,J) = OUTPUT(I,J)/TOTWGT
	ENDDO
	ENDDO


! 4. Return
	RETURN

end SUBROUTINE mwavg
