!-------------------------------------------------------------------------
!   SUBROUTINE xy2cyn
!   -------------------
!   V_XYZ		- Input, 3D & in cartesian coordinates
!   V_CYN		- Output, 3D & in polar-cylindrical coordinates
!   ISZ			- Size of the x-dimension of V_XYZ
!   JSZ			- Size of the y-dimension of V_XYZ
!   KSZ			- Size of the z-dimension of V_XYZ, could be 0
!   RAD			- Radius dimension
!   RSZ			- Size of the r-dimension of V_CYN
!   THETA		- Azimuthal Angle dimension
!   THSZ		- Size of the theta-dimension of V_CYN
!   ICEN		- Index of TC center in X
!   JCEN		- Index of TC center in Y
!   DX			- Resoulution in XY units
!   FVAL		- Missing Value
!
!   DESCRIPTION	Coordinate transformation is achieved through a 
!			4 nearest neigbor technique. The closest 4 (X,Y)
!			points are weighted based on proximity to the
!			(R,TH) point of interest. These weight are
!			represented by aaa,bbb,ccc,ddd below. RADIUS 
!			starts from 0 (i.e., TC center) and has the same
!			resolution as the input data (i.e., DX). THETA
!			starts pointing East and rotates counterclockwise.
!			The 1st and last THETA values are identical.
!
!	Written By:  Ghassan Alaka (ghassan.alaka@noaa.gov)
!

 SUBROUTINE xy2cyn (V_XYZ,V_CYN,ISZ,JSZ,KSZ,RAD,RSZ,THETA,THSZ,ICEN,JCEN,DX,FVAL)

 IMPLICIT NONE

! Input/Ouput Variables
 INTEGER, INTENT(IN)	:: ISZ, JSZ, KSZ, RSZ, THSZ
 INTEGER, INTENT(IN)	:: ICEN(KSZ), JCEN(KSZ)
 REAL,	  INTENT(IN)	:: V_XYZ(ISZ,JSZ,KSZ), DX, FVAL
 REAL,	  INTENT(INOUT)	:: V_CYN(THSZ,RSZ,KSZ)
 REAL,	  INTENT(INOUT)	:: RAD(RSZ), THETA(THSZ)
 
! Local Variables
 INTEGER	:: ibd, i1, i2, jbd, j1, j2
 INTEGER	:: i, iii, j, jjj, k, iloc, jloc
 INTEGER	:: m, m1, n, n1, xsign, ysign
 LOGICAL	:: TOO_SMALL
 REAL		:: pi_cnst = 3.141592653589793238462643383279502884
 REAL		:: dTH, RCOS(THSZ), RSIN(THSZ)
 REAL		:: xxx, yyy, aaa, bbb, ccc, ddd, eee


	
! Find minimum limits
! This helps determine if data is large enough to convert
! to a cylinder.
 ibd	= 3
 i1	= 1 + ibd
 i2	= ISZ - ibd
 jbd	= 3
 j1	= 1 + jbd
 j2	= JSZ - jbd
 TOO_SMALL = (i1 >= i2).OR.(j1 >= j2)
 IF ( TOO_SMALL ) THEN
	WRITE (*,*) "ERROR: xy2cyn: Data not large enough to create cylinder."
	call EXIT
 ENDIF
	
! Set up initial values
 RAD(1)		= 0
 THETA(1)	= 1.5*pi_cnst
 RCOS(1)	= COS(1.5*pi_cnst)
 RSIN(1)	= SIN(1.5*pi_cnst)
 dTH = 2.*pi_cnst/REAL(THSZ-1)
 WRITE (*,*) FVAL			


! Loop over the THETA dimension
 DO i = 2,THSZ
 	iii = i-1
 
	! For efficiency, calculate THETA here.
	! By default, THETA(0) point east and increasing
	! THETA rotates counterclockwise.
	THETA(i)= 1.5*pi_cnst - REAL(iii)*dTH
 	RCOS(i)	= COS(THETA(i))
	RSIN(i)	= SIN(THETA(i))
	
	
	! Loop over the RADIUS dimension
	 DO j = 2,RSZ
		jjj = j-1
		
		 
		RAD(j)	= DX*REAL(jjj)
		!WRITE (*,*) RAD(j), THETA(i), iii, dTH, REAL(iii)*2.*pi_cnst/REAL(THSZ-1)


		! Find closest X position based on RAD,THETA	
		xxx	= REAL(jjj)*RCOS(i)
		iloc	= INT(xxx)
		aaa	= ABS(xxx - iloc)
		bbb	= 1. - aaa
		IF ( xxx < 0 ) THEN
			xsign	= -1
		ELSE
			xsign	= 1
		ENDIF


		! Find closest Y position based on RAD,THETA	
		yyy	= REAL(jjj)*RSIN(i)
		jloc	= INT(yyy)
		ccc	= ABS(yyy - jloc)
		ddd	= 1. - ccc
		IF ( yyy < 0 ) THEN
			ysign	= -1
		ELSE
			ysign	= 1
		ENDIF
		
 
 		! Loop over the vertical coordinate	
		DO k = 1,KSZ		
			
			! m,m1 represent the two closest X indices to our (R,TH) location 
			m  = ICEN(k) + 1 + iloc
			m1 = m + xsign
			
			
			
			! n,n1 represent the two closest Y indices to our (R,TH) location
			n  = JCEN(k) + 1 + jloc
			n1 = n + ysign


			! Check that the indices are within data bounds
			IF ( n < 0 .or. m < 0 .or. n1 < 0 .or. m1 < 0 ) THEN
				eee = FVAL
			ELSEIF ( n > JSZ .or. n1 > JSZ ) THEN
				eee = FVAL
			ELSEIF ( m > ISZ .or. m1 > ISZ ) THEN
				eee = FVAL
			ELSEIF ( V_XYZ(m,n,k) == FVAL .or. V_XYZ(m,n1,k) == FVAL .or. V_XYZ(m1,n,k) == FVAL .or. V_XYZ(m1,n1,k) == FVAL ) THEN
				eee = FVAL
			ELSE
				eee = MIN( V_XYZ(m,n,k), V_XYZ(m,n1,k), V_XYZ(m1,n,k), V_XYZ(m1,n1,k) )
			ENDIF

			
			! Calculate the interpolated value on the polar cylindrical grid
			! Only if 'eee' is not missing
			IF ( eee /= FVAL ) THEN
				V_CYN(i,j,k) = bbb*( ddd*V_XYZ(m,n,k)  + ccc*V_XYZ(m,n1,k)  ) &
					     + aaa*( ddd*V_XYZ(m1,n,k) + ccc*V_XYZ(m1,n1,k) )
			ENDIF
		ENDDO
 	ENDDO
 ENDDO
 
 DO k = 1,KSZ
 	V_CYN(1,:,k) = V_CYN(THSZ,:,k)
	V_CYN(:,1,k) = V_XYZ(ICEN(k),JCEN(k),k)
 ENDDO

end SUBROUTINE xy2cyn
