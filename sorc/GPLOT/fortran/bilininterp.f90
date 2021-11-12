!-------------------------------------------------------------------------
!   SUBROUTINE bilininterp
!   -------------------
!   V_IN		- Input, 3D & in any uniformly-spaced grid
!   V_OUT		- Output, 3D & in any uniformly-spaced grid
!   X_IN		- Input X array
!   XISZ		- Size of X_IN
!   Y_IN		- Input Y array
!   YISZ		- Size of Y_IN
!   KSZ			- Size of the z-dimension could be 1
!   X_OUT		- Output X array
!   XOSZ		- Size of X_OUT
!   Y_OUT		- Output Y array
!   YOSZ		- Size of Y_OUT
!   FVAL		- Missing Value
!
!   DESCRIPTION		Bilinear interpolation is achieved through a 
!			4 nearest neighbor technique. The closest 4 (X_IN,Y_IN)
!			points are weighted based on proximity to the
!			(X_OUT,Y_OUT) point of interest. (X_OUT,Y_OUT) have
!			the same units as (X_IN,Y_IN). The weights
!			are represented by aaa,bbb,ccc,ddd below.


 SUBROUTINE bilininterp (V_IN,V_OUT,X_IN,XISZ,Y_IN,YISZ,KSZ,X_OUT,XOSZ,Y_OUT,YOSZ,FVAL)

 IMPLICIT NONE

! Input/Ouput Variables
 INTEGER, INTENT(IN)	:: XISZ, YISZ, KSZ, XOSZ, YOSZ
 REAL,	  INTENT(IN)	:: V_IN(XISZ,YISZ,KSZ), FVAL
 REAL,	  INTENT(IN)	:: X_IN(XISZ), Y_IN(YISZ), X_OUT(XOSZ), Y_OUT(YOSZ)
 REAL,	  INTENT(INOUT)	:: V_OUT(XOSZ,YOSZ,KSZ)
 
! Local Variables
 INTEGER	:: i, j, k, a, b
 INTEGER	:: x1, x2, y1, y2
 REAL		:: X_DIFF(XISZ), Y_DIFF(YISZ), jjj, iii
 REAL		:: DXI, DYI, DXO, DYO
 REAL		:: aaa, bbb, ccc, ddd, eee, fff
 

! Get delta values
 DXI = ABS(X_IN(1) - X_IN(2))
 DYI = ABS(Y_IN(1) - Y_IN(2))
 DXO = ABS(X_OUT(1) - X_OUT(2))
 DYO = ABS(Y_OUT(1) - Y_OUT(2))


! Loop over the VERTICAL dimension
 DO k = 1,KSZ
	
	! Loop over the Y dimension
	DO j = 1,YOSZ
	
		! Check if current Y_OUT value is within the bounds of Y_IN
		jjj = Y_OUT(j)
		IF ( jjj>MAXVAL(Y_IN) .or. jjj<MINVAL(Y_IN) ) THEN
			CYCLE
		ENDIF
		
		
		! Get Y_IN indices/weights for neighbors
		DO a = 1,YISZ
			IF ( jjj > Y_IN(a) ) THEN
				Y_DIFF(a) = 1000000.
			ELSE
				Y_DIFF(a) = Y_IN(a) - jjj
			ENDIF
		ENDDO
		y2	= INT(MINLOC(Y_DIFF, 1))
		y1	= y2 - 1
		ccc	= Y_DIFF(y2)/DYI
		ddd	= 1. - ccc
		
		
		! Loop over the X dimension
		DO i = 1,XOSZ
			
			! Check if current X_OUT value is within the bounds of
			! X_IN
			iii = X_OUT(i)
			IF ( iii>MAXVAL(X_IN) .or. iii<MINVAL(X_IN) ) THEN
				CYCLE
			ENDIF
			
			
			! Get X_IN indices/weights for neighbors
			DO b = 1,XISZ
				IF ( iii > X_IN(b) ) THEN
					X_DIFF(b) = 1000000.
				ELSE
					X_DIFF(b) = X_IN(b) - iii
				ENDIF
			ENDDO
			x2	= INT(MINLOC(X_DIFF,1))
			x1	= x2 - 1
			aaa	= X_DIFF(x2)/DXI
			bbb	= 1. - aaa
			
			
			! Check that indices for interpolation are within expected bounds.
			IF ( x1<0 .or. x2<0 .or. y1<0 .or. y2<0 ) THEN
				eee = FVAL
                                fff = FVAL
			ELSEIF ( y1>JSZ .or. y2>JSZ ) THEN
				eee = FVAL
                                fff = FVAL
			ELSEIF ( x1>ISZ .or. x2>ISZ ) THEN
				eee = FVAL
                                fff = FVAL
			ELSE
				eee = MINVAL((/V_IN(x1,y1,k), V_IN(x1,y2,k), &
					       V_IN(x2,y1,k), V_IN(x2,y2,k)/))
                                fff = MAXVAL((/V_IN(x1,y1,k), V_IN(x1,y2,k), &
                                               V_IN(x2,y1,k), V_IN(x2,y2,k)/))
			ENDIF
			!WRITE (*,*) aaa, bbb, ccc, ddd, eee
			!WRITE (*,*) x1, x2, y1, y2
		
		
			! INTERPOLATE to the (X_OUT,Y_OUT) point of interest.
			IF ( eee /= FVAL .and. fff /= FVAL ) THEN
				V_OUT(i,j,k)	= bbb*(ddd*V_IN(x2,y2,k) + ccc*V_IN(x2,y1,k)) &
						+ aaa*(ddd*V_IN(x1,y2,k) + ccc*V_IN(x1,y1,k))
			ENDIF
			!WRITE (*,*) V_OUT(i,j,k)
			
		ENDDO
	ENDDO
 ENDDO
 
 end SUBROUTINE bilininterp
