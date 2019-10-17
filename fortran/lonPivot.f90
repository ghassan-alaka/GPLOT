!-------------------------------------------------------------------------
!   SUBROUTINE lonPivot
!   -------------------
!   V1			- Input, 3D & in standard coordinates
!   V2			- Output, 3D & in standard coordinates
!   LON1		- Input Longitude array (in degrees)
!   LON2		- Output Longitude array (in degrees)
!   ISZ			- Size of LON
!   JSZ			- Size of LAT
!   KSZ			- Size of the VERTICAL dimension, could be 1
!   PVT			- LON value (in degrees) to pivot around
!   RO			- True/False for special filtering
!
!   DESCRIPTION		Based on the NCL function by Denis Shea, but
!			hopefully with some better speed. The output
!			includes the 3D and longitude arrays.
!			The special filtering is just to bump up/down
!			the values, as to not be too negative or too
!			positive, depending on the needs.


 SUBROUTINE lonPivot (V1,V2,LON1,LON2,ISZ,JSZ,KSZ,PVT,RO)

 IMPLICIT NONE

! Input/Ouput Variables
 INTEGER, INTENT(IN)	:: ISZ, JSZ, KSZ
 LOGICAL, INTENT(IN)	:: RO
 REAL,	  INTENT(IN)	:: PVT, LON1(ISZ), LON2(ISZ)
 REAL,	  INTENT(INOUT)	:: V1(ISZ,JSZ,KSZ), V2(ISZ,JSZ,KSZ)
 
! Local Variables
 INTEGER	:: IPVT, N, i, j, k
 
 
 ! CHeck that PVT is an acceptable value
 IF ( PVT<MINVAL(LON) .or. PVT>MAXVAL(LON) ) THEN
 	WRITE (*,*) "ERROR: lonPivot: 'PVT' must be within longitude bounds."
	call EXIT
 ENDIF
 
 
 ! Get the index around which to pivot
 DO i=1,ISZ
 	lDiff(i) = ABS(LON1(i) - PVT)
 ENDDO
 IPVT = INT(MINLOC(lDiff,1))
 N    = ISZ-IPVT
 
 ! Loop over all dimensions
 DO k=1,KSZ
 DO j=1,JSZ
 DO i=1,ISZ
 
 	! The actual reordering
	IF ( i=<N ) THEN
		V2(i,j,k) = V1(IPVT+i-1,j,k)
		LON2(i)   = LON1(IPVT+i-1)
	ELSE
		V2(i,j,k) = V1(i-N,j,k)
		LON2(i)   = LON1(i-N)
	ENDIF
	
	
	! Re-calculate LON so that it is motonic which respect to the pivot
	IF ( i<=N .and. LON2(i)>PVT ) THEN
		LON2(i) = LON2(i) - 360.
	ELSEIF ( i>N .and. LON2(i)<PVT ) THEN
		LON2(i) = LON2(i) + 360.
	ENDIF
 
 ENDDO
 ENDDO
 ENDDO
 
 
 ! Special reordering to increase/decrease the mean value
 ! Useful if you want  (don't want) a lot of negative values.
 IF ( RO ) THEN
 IF ( MINVAL(LON)>180 ) THEN
 	LON2(:) = LON2(:) - 360.
 ELSEIF ( MAXVAL(LON)<180 ) THEN
 	LON2(:) = LON2(:) + 360.
 ENDIF
 ENDIF
 
 end SUBROUTINE lonPivot
