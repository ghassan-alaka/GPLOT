      SUBROUTINE DTGNUM ( INDTG, IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS,
     *                    ISTAT )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGNUM
C
C  DESCRIPTION:  Given a DTG (YYYYMMDDHH), return integer values for
C		 year, month, day, hour, days into the year, and hours
C		 into the year.
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):   S. Glassner
C
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
C
C  LIBRARIES OF RESIDENCE(UTIL*):
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):
C
C    CALL DTGNUM ( INDTG, IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS, ISTAT )
C
C  INPUT PARAMETERS:
C
C    INDTG	C*10	    Day-time group, yyyymmddhh
C
C  OUTPUT PARAMETERS:
C
C    IYR	INT	    Year.
C    IMO	INT	    Month.
C    IDAY	INT         Day.
C    IHOUR	INT	    Hour.
C    IYRDAY	INT	    Days of the year.
C    IYRHRS	INT	    Hours of the year.
C    ISTAT	INT	    Error status return, = 0, OK; = -1,
C			     bad input DTG.
C
C  CALLS:
C
C    DTGCHK	A function that validates DTGs.  Output is a 10-character
C		  string that is blank if the DTG is valid.  It will
C		  contain asterisks corresponding to where the DTG is invalid
C		  otherwise.
C
C  ERRORS CONDITIONS:
C
C    Invalid DTG.  All integers will return with zero. ISTAT will be
C     set to -1.
C
C  EXAMPLE:
C
C    CHARACTER*10 INDTG
C
C    INTEGER IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS, ISTAT
C    DATA INDTG / '1990120312' /
C
C    CALL DTGNUM ( INDTG, IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS,
C   *              ISTAT )
C
C    		...
C
C    Values returned will be:
C	IYR     - 1990
C	IMO     -   12
C	IDAY    -   03
C	IHOUR	-   12
C	IYRDAY  -  337
C 	IYRHRS  - 8076
C       ISTAT   -    0
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    DTGERR	 C*10	Holds error return from DTGCHK.
C    IDX         INT    Set to 1 if the input year is not a leap
C			  year. If it is a leap year, it will be
C			  set to 2. Used as a subscript for the
C			  month array.
C    MONTH(12,2) INT	Number of days elapsed before getting to
C          	          this month.  There are two sets
C			  of these, one for non-leap years (second
C			  subscript = 1), one for leap years (second
C			  subscript = 2).  For example, MONTH(3,2),
C			  for March in a leap year, contains 60, the
C			  the number of days elapsed before March.
C			  The idea is that if IDAYS comes up with
C			  64 days in a leap year, we know the month
C			  should be March because it is greater than
C			  60 and less than the end of March, 91.
C
C  METHOD:
C
C    1.  Do an internal read of the input DTG to get year, month,
C	 day, hour.  Call DTGCHK to make sure it's valid first.
C	 If it is invalid, set ISTAT to 0 and return.
C    2.  Set leap year index, IDX.
C    3.  Calculate number of days in the year by adding IDAY to the
C	 number of days elapsed before the first of the month, IMO.
C	 Days elapsed is in the MONTH array.
C    4.  Calculate number of hours into the year by multiplying
C        whole days of the year (IYRDAY-1) by 24 hours and adding
C        IHOUR.
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Changes to DTGNUM required to port to Cray:
C                      from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C
C.........END PROLOGUE..................................................

C
      IMPLICIT NONE

      CHARACTER*10 INDTG, DTGERR, DTGCHK
C
      INTEGER IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS, ISTAT, IDX, MOD
      INTEGER MONTH(12,2)
C
      DATA MONTH /0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334,
     *            0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335
     *           /
C
      DATA DTGERR /'          '/
C
      IYR    = 0
      IMO    = 0
      IDAY   = 0
      IHOUR  = 0
      IYRDAY = 0
      IYRHRS = 0
      ISTAT  = 0

************************************************************************
*	 Call DTGCHK to validate the DTG.
*	 If it's ok, do an internal read of the integer date parts.
************************************************************************

      DTGERR = DTGCHK ( INDTG )
      IF ( DTGERR .NE. '          ' ) THEN
	 ISTAT = -1
         GO TO 5000
      ELSE
         READ ( INDTG, '(I4, 3I2)' ) IYR, IMO, IDAY, IHOUR
      END IF

************************************************************************
*        Set the leap year index, IDX, to 1 if non-leap year and 2
*	   if leap year.
*	 If year is a century, it's a leap year if it's evenly divisible
*	   by 400. If it's not a century, it's a leap year if it's
*	   evenly divisible by 4.
************************************************************************

      IF ( MOD ( IYR, 100 ) . EQ. 0 ) THEN
	 IF ( MOD ( IYR, 400 ) .EQ. 0 ) THEN
	    IDX = 2
	 ELSE
	    IDX = 1
	 END IF

      ELSEIF (MOD ( IYR, 4 ) .EQ. 0 ) THEN
	 IDX = 2

      ELSE
	 IDX = 1
      END IF
C
      IYRDAY = MONTH(IMO,IDX) + IDAY
      IYRHRS = ( IYRDAY-1 ) * 24 + IHOUR
C
 5000 RETURN
      END
      