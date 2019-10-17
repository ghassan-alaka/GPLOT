!-------------------------------------------------------------------------
!   SUBROUTINE hbfilter
!   -------------------
!   Description: Remove the vortex from input data using the methodology
!                of Kurihara (1993).
!
!   INPUT		- field to be filtered
!   OUPUT		- filtered output, same dimensions at INPUT
!   NMAX		- recrussive application of filter (suggested 3 for 9 km and 27 for 3 km)
!   NI			- Number of points in I direction
!   NJ			- Number of points in J direction
!   NK			- Number of points in K direction


 SUBROUTINE hbfilter(INPUT,OUTPUT,NMAX,NI,NJ,NK)


 IMPLICIT NONE

 INTEGER,	INTENT(IN)		:: NMAX, NI, NJ, NK
 REAL,		INTENT(IN)		:: INPUT(NI,NJ,NK)
 REAL,		INTENT(INOUT)		:: OUTPUT(NI,NJ,NK)

! local

 INTEGER					:: I,J,K,N,JMIN,JMAX,IMIN,IMAX,KMIN,KMAX,REPEAT
 REAL						:: PI
 REAL						:: M(11)
 DATA						M /2,3,4,2,5,6,7,2,8,9,2/
 INTEGER,	PARAMETER			:: NF=11
 REAL						:: FK(NF)
 REAL						:: XTU(NI,NF)
 REAL						:: YTU(NJ,NF)

!------------------------------------------------------------------
!
!     Purpose: This routine filters and removes
!              hurricane signals (Kurihara et al., 1993, MWR)
!
!     Called from: vortex
!
!     Original Code: Qingfu Liu (EMC)
!     Modification history: This is gopal's doing
!-----------------------------------------------------------------


!   DOMAIN
    IMIN=1     !(1,ID0-40)
    IMAX=NI !min(IDE-1,ID0+40)
    JMIN=1     !max(1,JD0-67)
    JMAX=NJ !min(JDE-1,JD0+67)
	KMIN=1
	KMAX=NK


!   INPUT:  Local variable defined for testing
    PI=4.0* ATAN(1.0)
    DO K=KMIN,KMAX
    DO J=JMIN,JMAX
    DO I=IMIN,IMAX
      OUTPUT(I,J,K)= INPUT(I,J,K) !SIN(2.0*PI/(21-1)*(I-1)) ! INPUT(I,J,K)
    ENDDO
    ENDDO
    ENDDO


!   DEFINE FILTER FUNCTION
    DO N=1,NF
      FK(N)=0.5/(1-COS(2.*PI/M(N)))
    ENDDO

    DO K=KMIN,KMAX


!.. DO ZONAL FILTER
     DO REPEAT=1,NMAX
       DO J=JMIN,JMAX-1
        DO N=1,NF
         XTU(IMIN,N)   = OUTPUT(IMIN,J,K)
         XTU(IMAX,N)   = OUTPUT(IMAX,J,K)
        ENDDO

        DO I=IMIN+1,IMAX-1
         XTU(I,1) = OUTPUT(I,J,K)+FK(1)*(OUTPUT(I-1,J,K)+OUTPUT(I+1,J,K)-2.*OUTPUT(I,J,K))
        ENDDO

        DO N=2,NF
        DO I=IMIN+1,IMAX-1
           XTU(I,N)=XTU(I,N-1)+FK(N)*(XTU(I-1,N-1)+XTU(I+1,N-1)-2.*XTU(I,N-1))
        ENDDO
        ENDDO

        DO I=IMIN,IMAX-1
           OUTPUT(I,J,K) = XTU(I,NF)
        ENDDO
       ENDDO ! J loop
     ENDDO   ! End recrussive repeat


!.. DO MERIDIONAL FILTER
     DO REPEAT=1,NMAX
       DO I=IMIN,IMAX
        DO N=1,NF
         YTU(JMIN,N) = OUTPUT(I,JMIN,K)
         YTU(JMAX,N) = OUTPUT(I,JMAX,K)
        ENDDO

        DO J=JMIN+1,JMAX-1
         YTU(J,1) = OUTPUT(I,J,K) + FK(1)*(OUTPUT(I,J-1,K) + OUTPUT(I,J+1,K) -2.*OUTPUT(I,J,K))
        ENDDO

        DO N = 2,NF
        DO J = JMIN+1,JMAX-1
         YTU(J,N) = YTU(J,N-1) + FK(N)*(YTU(J-1,N-1)  + YTU(J+1,N-1) - 2.*YTU(J,N-1))
        ENDDO
        ENDDO

        DO J = JMIN,JMAX-1
         OUTPUT(I,J,K)   =  YTU(J,NF)
        ENDDO
       ENDDO ! I loop
     ENDDO   ! End recrussive repeat

    ENDDO  ! K loop
    RETURN


 END SUBROUTINE hbfilter

