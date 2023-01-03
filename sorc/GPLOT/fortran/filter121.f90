!-------------------------------------------------------------------------
!   SUBROUTINE filter121
!   -------------------
!   V1			- Input, 3D & on standard grid
!   V2			- Output, 3D & on same grid as V1
!   ISZ			- Size of X dimension
!   JSZ			- Size of Y dimension
!   KSZ			- Size of Z dimension, could be 1
!   N			- Iterations for filter
!   CYC			- True if cylic for X dimension
!
!   DESCRIPTION		1-2-1 Filter to quickly smooth data, mostly for present-
!			ations. The filter runs through the X dimension then runs
!			through the Y dimension. Input data must be 3D, but the
!			vertical dimension can be 1 to overcome this.


 SUBROUTINE filter121 (V1,V2,ISZ,JSZ,KSZ,N,CYC)

 IMPLICIT NONE

! Input/Ouput Variables
 INTEGER, INTENT(IN)	:: ISZ, JSZ, KSZ, N
 LOGICAL, INTENT(IN)	:: CYC
 REAL,	  INTENT(INOUT)	:: V1(ISZ,JSZ,KSZ), V2(ISZ,JSZ,KSZ)
 
! Local Variables
 INTEGER	:: i, j, k, a
 REAL		:: WGT(3)
 
 
 ! Setup the weights
 ! 0.25/0.50/0.25 is standard for a 1-2-1 filter
 WGT = (/0.25,0.50,0.25/)
 
 
 ! Loop over the filter
 DO a=1,N
 ! Loop over the veritcal dimension
 DO k=1,KSZ
 
 	! Filter in the X-direction
 	DO j=1,JSZ
	DO i=1,ISZ
 		IF ( i == 1 ) THEN
			IF ( CYC ) THEN
				V2(i,j,k) = WGT(1)*V1(ISZ,j,k) + WGT(2)*V1(i,j,k) + WGT(3)*V1(i+1,j,k)
			ELSE
				V2(i,j,k) = (WGT(2)/(WGT(2)+WGT(3)))*V1(i,j,k) + (WGT(3)/(WGT(2)+WGT(3)))*V1(i+1,j,k)
			ENDIF
		ELSEIF ( i == ISZ ) THEN
			IF ( CYC ) THEN
				V2(i,j,k) = WGT(1)*V1(i-1,j,k) + WGT(2)*V1(i,j,k) + WGT(3)*V1(1,j,k)
			ELSE
				V2(i,j,k) = (WGT(1)/(WGT(1)+WGT(2)))*V1(i-1,j,k) + (WGT(2)/(WGT(1)+WGT(2)))*V1(i,j,k)
			ENDIF
		ELSE
			V2(i,j,k) = WGT(1)*V1(i-1,j,k) + WGT(2)*V1(i,j,k) + WGT(3)*V1(i+1,j,k)
		ENDIF
		
 	ENDDO
	ENDDO
	
	
	! Filer in the Y-direction
	DO i=1,ISZ
	DO j=1,JSZ
 		IF ( j == 1 ) THEN
			V2(i,j,k) = (WGT(2)/(WGT(2)+WGT(3)))*V1(i,j,k) + (WGT(3)/(WGT(2)+WGT(3)))*V1(i,j+1,k)
		ELSEIF ( j == JSZ ) THEN
			V2(i,j,k) = (WGT(1)/(WGT(1)+WGT(2)))*V1(i,j-1,k) + (WGT(2)/(WGT(1)+WGT(2)))*V1(i,j,k)
		ELSE
			V2(i,j,k) = WGT(1)*V1(i,j-1,k) + WGT(2)*V1(i,j,k) + WGT(3)*V1(i,j+1,k)
		ENDIF	
 	ENDDO
	ENDDO
	
	
	! Update V1
	V1 = V2
	
ENDDO
ENDDO
 
 
 end SUBROUTINE filter121
