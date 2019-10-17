!-------------------------------------------------------------------------
!   SUBROUTINE sph2cart
!   -------------------
!   V_SPH		- Input, 3D & in cartesian coordinates
!   V_CART		- Output, 3D & in polar-cylindrical coordinates
!   LON			- Longitude array (in degrees)
!   ISZ			- Size of LON
!   LAT			- Latitude array
!   JSZ			- Size of LAT (in degrees)
!   KSZ			- Size of the z-dimension could be 1
!   X			- X array (in km)
!   XSZ			- Size of X
!   Y			- Y array (in km)
!   YSZ			- Size of Y
!   ICEN		- TC center in LAT (in degrees)
!   JCEN		- TC center in LON (in degrees)
!   FVAL		- Missing Value
!
!   DESCRIPTION		Coordinate transformation is achieved through a 
!			4 nearest neigbor technique. The closest 4 (LAT,LON)
!			points are weighted based on proximity to the
!			(X,Y) point of interest. (X,Y) are given as
!			kilometers relative to the TC center. These weights
!			are represented by aaa,bbb,ccc,ddd below. This
!			calculation takes into account the curvature of the
!			Earth.


 SUBROUTINE sph2cart (V_SPH,V_CART,LON,ISZ,LAT,JSZ,KSZ,X,XSZ,Y,YSZ,ICEN,JCEN,FVAL)

 IMPLICIT NONE

! Input/Ouput Variables
 INTEGER, INTENT(IN)	:: ISZ, JSZ, KSZ, XSZ, YSZ
 REAL,    INTENT(IN)	:: ICEN(KSZ), JCEN(KSZ)
 REAL,	  INTENT(IN)	:: V_SPH(ISZ,JSZ,KSZ), FVAL
 REAL,	  INTENT(IN)	:: X(XSZ), Y(YSZ), LON(ISZ), LAT(JSZ)
 REAL,	  INTENT(INOUT)	:: V_CART(XSZ,YSZ,KSZ)
 
! Local Variables
 INTEGER	:: ibd, i1, i2, jbd, j1, j2
 INTEGER	:: i, j, k, a, b
 INTEGER	:: x1, x2, y1, y2
 LOGICAL	:: defined
 REAL		:: PI, D2R, R2D, R
 REAL		:: iDiff(ISZ), jDiff(JSZ), jjj, iii
 REAL		:: DI, DJ, DX, DY
 REAL		:: LONr(ISZ), LATr(JSZ), ICENr(KSZ), JCENr(KSZ)
 REAL		:: aaa, bbb, ccc, ddd, eee
 
 
! Setup
 PI  = 3.141592653589793238462643383279502884
 D2R = PI/180.
 R2D = 180./PI
 R   = 6371.


! Get delta values
 DI = ABS(LON(1) - LON(2))
 DJ = ABS(LAT(1) - LAT(2))
 DX = ABS(X(1) - X(2))
 DY = ABS(Y(1) - Y(2)) 
	
	
! Find minimum limits for interpolation
 ibd	= 3
 i1	= 1 + ibd
 i2	= ISZ - ibd
 jbd	= 3
 j1	= 1 + jbd
 j2	= JSZ - jbd
 
 
! Loop over the VERTICAL dimension
 DO k = 1,KSZ
 	! Check if TC center is available
	IF ( ICEN(k) == FVAL ) THEN
		CYCLE
	ENDIF
	
	
	! Convert radians to degrees
	ICENr(k) = D2R*ICEN(k)
	JCENr(k) = D2R*JCEN(k)
	
	
	! Loop over the Y dimension
	DO j = 1,YSZ
		! Convert radians to degrees
		LATr = D2R*LAT(j)
	
	
		! Get corresponding LATITUDE location
		jjj = R2D*((Y(j)/R)+JCENr(k))
		IF ( jjj>MAXVAL(LAT) .or. jjj<MINVAL(LAT) ) THEN
			CYCLE
		ENDIF
		
		
		! Get LATITUDE indices/weights for neighbors
		DO a = 1,JSZ
			IF ( jjj > LAT(a) ) THEN
				jDiff(a) = 1000000.
			ELSE
				jDiff(a) = LAT(a) - jjj
			ENDIF
		ENDDO
		y2	= INT(MINLOC(jDiff, 1))
		y1	= y2 - 1
		ccc	= jDiff(y2)/DJ
		ddd	= 1. - ccc
		
		
		! Loop over the X dimension
		DO i = 1,XSZ
			! Convert radians to degrees
			LONr = D2R*LON(i)
			
			
			! Get corresponding LONGITUDE location
			iii	= R2D*(X(i)/(R*COS((Y(j)/R)+JCENr(k))) + ICENr(k))
			IF ( iii>MAXVAL(LON) .or. iii<MINVAL(LON) ) THEN
				CYCLE
			ENDIF
			
			
			! Get LONGITUDE indices/weights for neighbors
			DO b = 1,ISZ
				IF ( iii > LON(b) ) THEN
					iDiff(b) = 1000000.
				ELSE
					iDiff(b) = LON(b) - iii
				ENDIF
			ENDDO
			x2	= INT(MINLOC(iDiff,1))
			x1	= x2 - 1
			aaa	= iDiff(x2)/DI
			bbb	= 1. - aaa
			
			
			! Check that domain is large enough for interpolation
			! Use limits defined prior to loops.
			!defined = (i1<=x1).and.(x2<=i2) .and. (j1<=y1).and.(y2<=j2)
			!IF ( .not.defined ) THEN
			!	WRITE (*,*) "ERROR: sph2cart: Domain is too small."
			!	call EXIT
			!ENDIF
			
		
			! Check that indices for interpolation are within expected bounds.
			IF ( x1<0 .or. x2<0 .or. y1<0 .or. y2<0 ) THEN
				eee = FVAL
			ELSEIF ( y1>JSZ .or. y2>JSZ ) THEN
				eee = FVAL
			ELSEIF ( x1>ISZ .or. x2>ISZ ) THEN
				eee = FVAL
			ELSE
				eee = MINVAL((/V_SPH(x1,y1,k), V_SPH(x1,y2,k), &
					       V_SPH(x2,y1,k), V_SPH(x2,y2,k)/))
			ENDIF
			!WRITE (*,*) aaa, bbb, ccc, ddd, eee
			!WRITE (*,*) x1, x2, y1, y2
		
		
			! INTERPOLATE to the (X,Y) point of interest.
			IF ( eee /= FVAL ) THEN
				V_CART(i,j,k)	= bbb*(ddd*V_SPH(x2,y2,k) + ccc*V_SPH(x2,y1,k)) &
						+ aaa*(ddd*V_SPH(x1,y2,k) + ccc*V_SPH(x1,y1,k))
			ENDIF
			!WRITE (*,*) V_CART(i,j,k)
			
		ENDDO
	ENDDO
 ENDDO
 
 end SUBROUTINE sph2cart
