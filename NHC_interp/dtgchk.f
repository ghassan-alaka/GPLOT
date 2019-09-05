      FUNCTION DTGCHK ( DTG )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGCHK
C
C  DESCRIPTION:  Check to see if a DTG is valid.
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):  S. Glassner
C
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
C
C  LIBRARIES OF RESIDENCE(UTIL*):
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):
C
C    CHARACTER*10 DTG, ERRDTG
C    ERRDTG = DTGCHK ( DTG )
C            or
C    CHARACTER*12 DTG, ERRDTG
C    ERRDTG = DTGCHK ( DTG )
C
C  INPUT PARAMETERS:
C
C    DTG           	  Date-time group in one of two formats:
C		C*10        YYYYMMDDHH
C		C*12	    YYYYMMDDHHmm
C
C  OUTPUT PARAMETERS:
C
C    ERRDTG	C*10/12   Error designator.  All blank if DTG was valid.
C			    Characters 1-4   = **** if year was bad.
C		            Characters 5-6   = **   if month was bad.
C			    Characters 7-8   = **   if day was bad.
C			    Characters 9-10  = **   if hour was bad.
C			    Characters 11-12 = **   if minutes were bad.
C
C  CALLS:
C
C    LEN	Returns the length of a string.
C
C  EXAMPLE:
C
C    1)  CHARACTER*10 DTGCHK, DTG, ERRDTG
C
C	 DATA DTG / '1995070312' /
C
C        ERRDTG = DTGCHK ( DTG )
C	 IF ( ERRDTG .EQ. ' ' ) THEN
C	    No error...
C	 ELSE
C	    Error...
C	 END IF
C
C        ERRDTG will be blank.
C
C    2)  CHARACTER*12 DTGCHK, DTG, ERRDTG
C
C	 DATA DTG / '198607031245' /
C
C        ERRDTG = DTGCHK ( DTG )
C	 IF ( ERRDTG .EQ. ' ' ) THEN
C	    No error...
C	 ELSE
C	    Error...
C	 END IF
C
C        ERRDTG will be blank.
C
C    3)  CHARACTER*12 DTGCHK, DTG, ERRDTG
C
C	 DATA DTG / '198677031290' /
C
C        ERRDTG = DTGCHK ( DTG )
C	 IF ( ERRDTG .EQ. ' ' ) THEN
C	    No error...
C	 ELSE
C	    Error...
C	 END IF
C
C        ERRDTG will look like this, where a period denotes a blank:
C	   ....**....**	 meaning that the month and minutes are invalid.
C
C  RESTRICTIONS:
C
C    The DTG may not contain any blanks.
C    The DTG must be either 10 or 12 digits long.
C    DTGCHK rejects DTG years that are outside the range 1800-2799.
C
C  ERROR CONDITIONS:
C
C    Non-numeric DTG. DTGCHK will be asterisk-filled.
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    IDAY	INT	Integer day from the input DTG.
C    IHOUR	INT	Integer hour from the input DTG.
C    IMO	INT	Integer month from the input DTG.
C    IYR	INT	Integer year from the input DTG.
C    LENGTH	INT	Gets the return from LEN, i.e. the length
C			  of the argument string.
C    MIN	INT	Integer minutes from the input DTG.
C    MONTH(12)	INT	An array containing the number of days in
C			  each month.
C
C  METHOD:
C
C    Check that each value is numeric and within the ranges:
C	Year:  1800-2799.
C	Month: 1-12.
C	Day:   1-31 for Jan, Mar, May, July, Aug, Oct and Dec.
C	       1-30 for Apr, June, Sep, Nov.
C	       28 or 29 for Feb, depending on whether year is a leap year.
C		  Note that 1900 is not a leap year and 2000 is.
C	Hour:  0-23
C	Min:   0-59
C    Whenever a value is outside its range, set the corresponding characters
C	of DTGCHK to asterisks.
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Change to DTGCHK required to port to Cray:
C                      from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C
C.........END PROLOGUE..................................................
C
      IMPLICIT NONE
C
      CHARACTER DTGCHK*(*), DTG*(*)
C
      INTEGER MONTH(12), IYR, IMO, IDAY, IHOUR, MIN, LENGTH, LEN, I
C
      DATA MONTH / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
C
      LENGTH = LEN ( DTG )
      IF ( LENGTH .NE. 10 .AND. LENGTH .NE. 12 ) THEN
	 DTGCHK = '************'
	 GO TO 5000
      END IF

****************************************************************************
*        Make sure every character in the DTG is a number.
****************************************************************************

      DO 10 I=1,LENGTH
	 IF ( DTG(I:I) .LT. '0' .OR. DTG(I:I) .GT. '9' ) THEN
C
	    IF (LENGTH .EQ. 10 ) THEN
               DTGCHK = '**********'
	    ELSE
  	       DTGCHK = '************'
	    END IF
C
	    GO TO 5000
         END IF
   10 CONTINUE

****************************************************************************
*        MONTH(2) will be changed to 29 later if the year is a leap year.
****************************************************************************

      MONTH(2) = 28

****************************************************************************
*        Get the integer versions of the year, month, day, and hour.
****************************************************************************

      READ ( DTG, '(I4,3I2)' ) IYR, IMO, IDAY, IHOUR

****************************************************************************
*       Get minutes if they're there.
****************************************************************************

      DTGCHK(1:LENGTH) = ' '
C
      IF ( LENGTH .EQ. 12 ) THEN
	 IF ( (DTG(11:11) .GE. '0') .AND. (DTG(11:11) .LE. '9') .AND.
     *        (DTG(12:12) .GE. '0') .AND. (DTG(12:12) .LE. '9') ) THEN
  	    READ (DTG, '(10X, I2)' ) MIN
	 ELSE
	    MIN = 99
	 END IF
      END IF

****************************************************************************
*        Check out the DTG.
*          Year...
*        Allow iyr == 0 for util_julday(), ajs  09/2002
****************************************************************************

      IF ( ((IYR .LT. 1800) .OR. (IYR .GT. 2799)) .AND. IYR .NE. 0 )
     *   DTGCHK(1:4) = '****'

****************************************************************************
*          Month...
****************************************************************************

      IF ( (IMO .LT. 1) .OR. (IMO .GT. 12) ) DTGCHK(5:6) = '**'

****************************************************************************
*          Day...
*	     Set February days to 29 if this is a leap year.
*
*	     If DTGCHK(1:6) (year/month) already has asterisks, it was
*              invalid, so don't do the day check.  If the year was invalid,
*	       we can't see if it was a leap year.  If the month was
*	       invalid, we can't see if it was February.
*	     See if this is a leap year, i.e. evenly divisible by four or,
*	       if it's a century (dtg(3:4) = '00'), evenly divisible
*	       by 400.
****************************************************************************

      IF ( DTGCHK(1:6) .EQ. '  ' ) THEN
         IF ( (DTG(3:4) .NE. '00') .AND. (MOD(IYR, 4) .EQ. 0) .OR.
     *      ( (DTG(3:4) .EQ. '00') .AND. (MOD(IYR, 400) .EQ. 0 ) )  )
     * 	       MONTH(2) = 29
C
         IF ( (IDAY .LT. 1) .OR. (IDAY .GT. MONTH(IMO)) )
     *      DTGCHK(7:8) = '**'
      END IF

****************************************************************************
*          Hour...
****************************************************************************

      IF ( (IHOUR .LT. 0) .OR. (IHOUR .GT. 23) ) DTGCHK(9:10) = '**'

****************************************************************************
*	   Minutes...
****************************************************************************

      IF ( LENGTH .EQ. 12 ) THEN
	 IF ( (MIN .LT. 0) .OR. (MIN .GT. 59) ) DTGCHK(11:12) = '**'
      END IF
C
 5000 RETURN
      END
      