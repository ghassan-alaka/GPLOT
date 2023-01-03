      SUBROUTINE DTGMOD ( INDTG, IDIF, NEWDTG, ISTAT )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGMOD
C
C  DESCRIPTION:  Given base DTG and increment (+/- hours), return new DTG
C                 ( = indtg + idif ) and the status value.
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
C    CHARACTER*10 INDTG, NEWDTG
C
C    CALL DTGMOD ( INDTG, IDIF, NEWDTG, ISTAT )
C
C  INPUT PARAMETERS:
C
C    INDTG	C*10	Base DTG in the format YYYYMMDDHH.
C    IDIF       INT     Difference in hours
C		         (-8775216 through +4294967295).
C
C  OUTPUT PARAMETERS:
C
C    NEWDTG	C*10	New DTG, the sum of INDTG and IDIF. If one of
C			  the errors listed below occurrs, NEWDTG will
C			  be returned asterisk-filled.
C    ISTAT      INT     Error return.
C			  ISTAT =  0, ok.
C			  ISTAT = -1, Invalid DTG.
C			  ISTAT = -2, Invalid increment.
C			  ISTAT = -3, DTGYRHR returned a non-zero
C				      status.
C
C  CALLS:
C
C    DTGNUM		Get integer year, month, day, hour, days of the
C			  year and hours of the year from DTG.
C    DTGYRHR		Get DTG from julian date.
C
C  EXAMPLE:
C
C    CHARACTER*10 CURDTG, NEWDTG
C    INTEGER IDIF
C
C    DATA CURDTG / '1991030612' /`
C    DATA IDIF  / 2 /
C
C    CALL DTGMOD ( CURDTG, IDIF, NEWDTG, ISTAT )
C		...
C
C    NEWDTG will contain 1991030614.
C
C  ERROR CONDITIONS:
C
C    Invalid DTG.   ISTAT=-1 at return. NEWDTG asterisk-filled.
C    Invalid IDIF.  ISTAT=-2 at return. NEWDTG asterisk-filled.
C    Non-zero status return from DTGYRHR. ISTAT=-3 at return.
C      NEWDTG asterisk-filled.
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    BADDTG     C*10    Value returned in NEWDTG if an error occured.
C    IDX        INT     Set to 1 if the input year is a leap
C			  year. Otherwise, it will be 2, 3 or 4
C			  (the remainder resulting from the mod
C			  function). Used as a subscript for the
C			  IHOURS array.
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
C     IHOURS(4)	INT	Number of hours in the year.  IHOURS(1) contains
C			  the number of hours in a leap year.
C     IYR	INT	Year, extracted from INDTG by DTGNUM.
C     IMO	INT	Month, extracted from INDTG by DTGNUM.
C     IDAY	INT	Day, extracted from INDTG by DTGNUM.
C     IHR	INT	Hour, extracted from INDTG by DTGNUM.
C     IDAOFYR	INT	Day of the year, extracted from INDTG by DTGNUM.
C     IHROFYR	INT	Hour of year, extracted from INDTG by DTGNUM.
C     JSTAT	INT	Error return from DTGNUM. If=0, ok.
C     KSTAT	INT	Error return from DTGYRHR. If=0, ok.
C
C  METHOD
C
C     1.  Call DTGNUM to get hours of the year.
C     2.  Add IDIF to the hours of the year.
C     3.  If the new sum is negative or has too many hours for one year,
C         adjust the sum and input year until the sum is positive and within
C         one year.
C     4.  Call DTGYRHR, passing it the year and hours of the year, to get
C         the new DTG.
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Change to DTGMOD required to port to Cray:
C                      from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C
C.........END PROLOGUE..................................................
C
      IMPLICIT NONE
C
      CHARACTER*10 INDTG, NEWDTG, BADDTG
C
      INTEGER IHOURS(4), IYR, IMO, IDAY, IHR, IDAOFYR, IHROFYR, NEWHRS,
     *        IDIF, IDX, I, ISTAT, JSTAT, KSTAT
C
      DATA IHOURS / 8784, 3*8760 /
      DATA BADDTG / '**********' /
C
      ISTAT = 0
C
      CALL DTGNUM ( INDTG, IYR, IMO, IDAY, IHR, IDAOFYR, IHROFYR,
     *              JSTAT )
      IF ( JSTAT .NE. 0 ) THEN
	 ISTAT = -1
	 NEWDTG = BADDTG
         GO TO 5000
      END IF

****************************************************************************
*        Increment (decrement) hour of the year (IHROFYR) by IDIF,
*        test for change of year, adjust if necessary, reformat to NEWDTG.
****************************************************************************

      NEWHRS = IHROFYR + IDIF

****************************************************************************
*         See if NEWHRS is negative.  If it is,  perform a loop that
*		Subtracts 1 from the year,
*		Adds a year's worth of hours to NEWHRS, the number of hours
*		  depending on whether the year is a leap year or not.
*		Sees if NEWHRS is still negative.  Leave the loop when it
*		  becomes zero or positive.
****************************************************************************

      IF ( NEWHRS .LT. 0 ) THEN
C
	 DO 10 I=1,1000
            IYR = IYR - 1
C
	    IF ( MOD(IYR, 100) .EQ. 0 ) THEN
	       IF ( MOD(IYR, 400) .EQ. 0 ) THEN
		  IDX = 1
	       ELSE
		  IDX = 2
	       END IF
            ELSE
 	       IDX = MOD(IYR, 4) + 1
	    END IF
C
    	    NEWHRS = NEWHRS + IHOURS(IDX)
            IF ( NEWHRS .GE. 0 ) GO TO 30
   10    CONTINUE
C
         GO TO 900

****************************************************************************
*         NEWHRS is positive or 0.
*         Perform a loop until NEWHRS' value is less than or equal to the
*           number of hours in a year.
*              See if there is more than one year's worth of hours in NEWHRS.
*              If there is, perform a loop that
*	         Subtracts a year's worth of hours from NEWHRS, the number
*		   of hours depending on whether the year is a leap year or
*                  not.
*	         Adds 1 to the year.
***************************************************************************

      ELSE
         DO 20 I=1,1000
	    IF ( MOD(IYR, 100) .EQ. 0 ) THEN
	       IF ( MOD(IYR, 400) .EQ. 0 ) THEN
		  IDX = 1
	       ELSE
		  IDX = 2
	       END IF
            ELSE
 	       IDX = MOD(IYR, 4) + 1
	    END IF
C
            IF ( NEWHRS .LT. IHOURS(IDX) ) GO TO 30
            NEWHRS = NEWHRS - IHOURS(IDX)
            IYR = IYR + 1
   20    CONTINUE
C
          GO TO 900
      END IF

   30 CALL DTGYRHR ( IYR, NEWHRS, NEWDTG, KSTAT )
      IF ( KSTAT .NE. 0 ) THEN
         NEWDTG = BADDTG
	 ISTAT = -3
      END IF
C
      GO TO 5000

**************************************************************************
*        Error.
**************************************************************************

  900 NEWDTG = BADDTG
      ISTAT = -2
C
 5000 RETURN
      END
      