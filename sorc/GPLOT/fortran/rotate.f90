!-------------------------------------------------------------------------
!   SUBROUTINE rotate
!   -------------------
!   V1			- Input, 3D & in polar-cylindrical coordinates
!   V2			- Output, 3D & in polar-cylindrical coordinates
!   RSZ			- Size of the RADIUS dimension
!   THSZ		- Size of the THETA dimension
!   KSZ			- Size of the VERTICAL dimension, could be 1
!   THETA		- Azimuthal Angle dimension
!   ANG			- Angle to be rotated to point UP
!
!   DESCRIPTION		Rotate polar-cylindrical data so that it
!			is oriented with 'ANG' point up. This could be to
!			align graphics with the motion or shear vectors.


 SUBROUTINE rotate (V1,V2,THSZ,RSZ,KSZ,THETA,ANG)

 IMPLICIT NONE

! Input/Ouput Variables
 INTEGER, INTENT(IN)	:: KSZ, RSZ, THSZ
 REAL,	  INTENT(IN)	:: THETA(THSZ), ANG
 REAL,	  INTENT(INOUT)	:: V1(THSZ,RSZ,KSZ), V2(THSZ,RSZ,KSZ)
 
! Local Variables
 INTEGER	:: i, j, k, iii, i2, i3
 REAL		:: aDiff(THSZ)

 DO i = 1,THSZ
 	aDiff(i) = ABS(ANG - THETA(i))
 ENDDO
 iii = INT(MINLOC(aDiff,1))

 DO i = 1,THSZ-1
 	i2 = i+iii
 	IF ( i2>THSZ ) THEN
		i3 = i2-THSZ+1
	ELSE
	 	i3 = i2
	ENDIF
	
	
 	DO k = 1,KSZ
 	DO j = 1,RSZ
		V2(i,j,k) = V1(i3,j,k)
 
 	ENDDO
 	ENDDO
 ENDDO
 
 V2(THSZ,:,:) = V2(1,:,:)

end SUBROUTINE rotate
