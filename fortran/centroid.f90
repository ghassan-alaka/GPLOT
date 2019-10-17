!-------------------------------------------------------------------------
!   SUBROUTINE centroid
!   -------------------
!   INPUT              - field to be filtered
!   OUTPUT             - 
!   A                  - Threshold (must be within bounds of INPUT
!   B                  - Centroid type (-1: less-than, 0: equal-to, 1: greater-than)
!   NI                 - Number of points in I direction
!   NJ                 - Number of points in J direction


 SUBROUTINE centroid (INPUT,ICTR,A,B,NI,NJ,NK)

 IMPLICIT NONE

! Input/Putput Variables
 INTEGER,   INTENT(IN)      :: B, NI, NJ, NK
 REAL,      INTENT(IN)      :: INPUT(NI,NJ,NK),A(NK)
 INTEGER,	INTENT(INOUT)   :: ICTR(NK,2)

! Local Variables
 REAL						:: MINV,MAXV,ISUM,JSUM,NSUM
 INTEGER                    :: I,J,K
 REAL						:: V(NI,NJ), CTR(NK,2)


! 1. Loop over all vertical levels. Could be 1.
	DO K=1,NK


	! 2. Error Messages
		V(1:NI,1:NJ)=INPUT(1:NI,1:NJ,K)
		MINV=MINVAL(V)
		MAXV=MAXVAL(V)

			IF ( A(K) < MINV .OR. A(K) > MAXV ) THEN
			PRINT *,"ERROR: centroid threshold is outside of data bounds."
			call EXIT
		ENDIF
		IF ( B /= 1 .AND. B /= 0 .AND. B /= -1 ) THEN
			PRINT *,"ERROR: centroid type must be: -1, 0, 1"
			call EXIT
		ENDIF


	! 3. Calculate centroid
		ISUM=0
		JSUM=0
		NSUM=0
		DO I=1,NI
		DO J=1,NJ
			! Less-Than Centroid
			IF ( B == -1 ) THEN
				IF ( V(I,J) <= A(K) ) THEN
					IF ( A(K) > 0 ) THEN
						V(I,J) = V(I,J) - A(K)
					ENDIF
					ISUM = ISUM - 1*REAL(I)*V(I,J)
					JSUM = JSUM - 1*REAL(J)*V(I,J)
					NSUM = NSUM - V(I,J)
				ENDIF
			ENDIF

			! Equal-To Centroid
		    IF ( B == 0 ) THEN
		        IF ( V(I,J) == A(K) ) THEN
		            ISUM = ISUM + 1*REAL(I)*V(I,J)
		            JSUM = JSUM + 1*REAL(J)*V(I,J)
		            NSUM = NSUM + V(I,J)
		        ENDIF
	    	ENDIF

			! Greater-Than Centroid
		    IF ( B == 1 ) THEN
		        IF ( V(I,J) >= A(K) ) THEN
		            IF ( A(K) < 0 ) THEN
		                V(I,J) = V(I,J) + A(K)
		            ENDIF	
		            ISUM = ISUM + 1*REAL(I)*V(I,J)
		            JSUM = JSUM + 1*REAL(J)*V(I,J)
		            NSUM = NSUM + V(I,J)
		        ENDIF
		    ENDIF

		ENDDO
		ENDDO


	! 4. Get final centroid indices
		CTR(K,1) = ISUM/NSUM
		CTR(K,2) = JSUM/NSUM
		ICTR(K,1) = int(CTR(K,1))
		ICTR(K,2) = int(CTR(K,2))

	ENDDO

	RETURN

end SUBROUTINE centroid
