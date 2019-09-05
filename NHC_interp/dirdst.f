      subroutine dirdst( flat , flng , tlat , tlng , dir , dis )

cxxx  This routine calculates the direction (dir in degree 1.-360.) and
cxxx  distance (dis in n.m.) of two points.
cxxx  flat,flng:  from this lat,long the dis and dir are calculated
cxxx  tlat,tlng:    to this lat,long the dis and dir are calculated

cxxx  Longitude ranges from 1. to 360. Western Hemisphere.

ckpd  No, it doesn't.  Longitude ranges from 0.1 to 360.0; we do our
ckpd  positions to the tenth of a degree.  This oversight has spawned
ckpd  an amazing replication of erroneous direction modulus
ckpd  calculations thoughout MS-DOS ATCF.  See one example, below.

ckpd                   Input "from" point latitude, in degrees; carries
ckpd                   at least one significant decimal, may carry more.

      real*4           flat

ckpd                   Input "from" point longitude, in degrees; carries
ckpd                   at least one significant decimal, may carry more.

      real*4           flng

ckpd                   Input "to" point latitude, in degrees; carries at
ckpd                   least one significant decimal, may carry more.

      real*4           tlat

ckpd                   Input "to" point longitude, in degrees; carries
ckpd                   at least one significant decimal, may carry more.

      real*4           tlng

ckpd                   Output rhumb line direction of "from" - "to"
ckpd                   "vector" in degrees; by convention, value lies
ckpd                   between 0.1 and 360.0, but in fact extra
ckpd                   precision is carried until the value is printed.

ckpd                   Also used as a flag on input; if the value is
ckpd                   less than 1.0, the direction calculation is
ckpd                   skipped, saving some trig calls.

      real*4           dir

ckpd                   Output rhumb line distance of "from" - "to"
ckpd                   "vector" in nautical miles.  Display and archive
ckpd                   values are in whole miles, but additional
ckpd                   precision is carried through calculations.

      real*4           dis

ckpd                   The half-circumference of a circle in radians.

      real*4           pi

ckpd                   Conversion factor from degrees to radians.

      real*4           degrad

ckpd                   Conversion factor from radians to degrees.

      real*4           raddeg

ckpd                   Conversion factor from great circle degrees to
ckpd                   nautical miles.

      real*4           degnmi

ckpd                   Work variable in which the cosine angle of the
ckpd                   arc subtended by the rhumb line distance is
ckpd                   developed.

      real*4           ca

ckpd                   Work variable into which the sine angle of the
ckpd                   arc subtended by the rhumb line distance is
ckpd                   developed.

      real*4           sa

ckpd                   Work variable into which the absolute difference
ckpd                   in longitudes is calculated for a check to detect
ckpd                   a substantially north-south vector.

      real*4           delng

ckpd                   Work variable into which the radian difference in
ckpd                   longitudes in calculated.  Used in cosine angle
ckpd                   computation.

      real*4           angle

ckpd                   Work variable into which the mathematician's
ckpd                   compass version of the "from" latitude is
ckpd                   calculated.  Used in the cosine angle computation
ckpd                   and the direction computation.

      real*4           from

ckpd                   Work variable into which the mathematician's
ckpd                   compass version of the "to" latitude is
ckpd                   calculated.  Used in the cosine angle computation
ckpd                   and the direction computation.

      real*4           to

ckpd                   Flag to identify cases where longitudes are
ckpd                   converted to achieve the desired order for the
ckpd                   calculation, so that they can be converted back
ckpd                   before being returned to the calling program.

      logical*1        lngchg

ckpd  An amazing argument in favor of literate programming and parallel
ckpd  layouts of parallel steps.  The author typed in a value of pi each
ckpd  time it was used, and used different values of pi in the same
ckpd  calculation!

c  KPD  Since we write on the tlat,tlng variables, and someone might
c       well have passed an expression rather than a variable for
c  either, save our own lives by copying them to temporaries and using
c  the temporaries instead.


      real*4           myflng
      real*4           mytlng

c     call prctrc('dirdst',.true.)

      myflng = flng

      mytlng = tlng

      pi = acos( -1.0 )

      degrad = pi / 180.0

      raddeg = 180.0 / pi

      degnmi = 60.0

      lngchg = .false.

      if ( abs( myflng - mytlng ) .ge. 180.0 ) then

        lngchg = .true.

        myflng = amod( myflng + 180.0 , 360.0 )

        mytlng = amod( mytlng + 180.0 , 360.0 )

      endif

      delng = abs( myflng - mytlng )

ckpd  If the "from" - "to" vector is substantially north - south, ignore
ckpd  the small longitude contribution and take advantage of the cheap
ckpd  direct conversion of nautical miles to latitude degrees by a
ckpd  simple scaling computation.

      if ( delng .lt. 0.05 ) then

        dis = 60.0 * ( tlat - flat )

        dir = 360.0

        if ( dis .lt. 0.0 ) dir = 180.0

        dis = abs( dis )

      else

ckpd  Commented out code converted to standard software engineering
ckpd  "parallel code layout of parallel semantics" format to make the
ckpd  problem jump off the screen at the reader.

ckpd  150 from  = ( 90.0 - flat )         * 3.141592654     / 180.0

ckpd      to    = ( 90.0 - tlat )         * 3.1415926535898 / 180.0

ckpd      angle = abs( myflng - mytlng )      * 3.1415926535898 / 180.0

ckpd    Convert from mariners' compass, with zero at north and angles in
ckpd    degrees increasing clockwise, to mathematician's compass, with
ckpd    zero at east, and angles in radians increasing counterclockwise.

ckpd    This is needed because we forecast for mariners but do so using
ckpd    trig functions designed for mathematicians.

        from  = ( 90.0 - flat ) * degrad

        to    = ( 90.0 - tlat ) * degrad

        angle = abs( myflng - mytlng ) * degrad

ckpd    This is a truly wondrous way to achieve a result.  What I want
ckpd    to do is to take

ckpd      the square root of
ckpd      (
ckpd        (
ckpd          the square of
ckpd          (
ckpd            the latitude difference converted to nautical miles by
ckpd            simple scaling
ckpd          )
ckpd        )
ckpd        plus
ckpd        (
ckpd          the square of
ckpd          (
ckpd            the longitude difference in degrees converted to
ckpd            nautical miles by simple scaling and appropriately
ckpd            scaled down by the cosine of the mid-latitude [to adjust
ckpd            for degrees of longitude shrinking in nautical mile size
ckpd            away from the equator]
ckpd          )
ckpd        )
ckpd      )

ckpd    which I would then call the great circle distance between the
ckpd    points.

ckpd    This calculation achieves substantially the same results, at
ckpd    least for small difference vectors, and has almost the format of
ckpd    the above calculation, though with quite different values, but
ckpd    gives few clues as to how that correct result happens!

        ca =   cos( from ) * cos( to )
     &       + sin( from ) * sin( to ) * cos( angle )

ckpd      dis = acos( ca ) * 60.0 * 180.0 / 3.1415926535898

        dis = acos( ca ) * raddeg * degnmi

        dis = abs( dis )

ckpd    Check the flag value of dir before wasting time on an unwanted
ckpd    calculation.

        if ( dir .ge. 1.0 ) then

          sa = sqrt( 1.0 - ca * ca )

          dir = ( cos( to ) - cos( from ) * ca ) / ( sin( from ) * sa )

ckpd        dir = acos( dir ) *      180.0 / 3.1415926535898

          dir = acos( dir ) * raddeg

          if ( mytlng .gt. myflng ) dir = 360.0 - dir

ckpd  This code is done just this way, wrong, an amazing number of times
ckpd  thoughout MS-DOS ATCF, lending support to Dr. Nancy Levison's
ckpd  software engineering studies on the correlation of errors in
ckpd  "independently" developed software [to the point that agreement of
ckpd  independently coded computations of the same results from the same
ckpd  data gives little guarantee of correctness of the programs or of
ckpd  the results].

ckpd  What is _supposed_ to be happening here is that a direction of 0.0
ckpd  degrees is reserved for a "no interesting value" token, usually
ckpd  used when the corresponding distance is zero, while meaningful
ckpd  directions are encoded between 0.1 and 360.0.

ckpd  The problem arose because the various routine authors were
ckpd  thinking "greater than zero and less than or equal to three-sixty"
ckpd  in degrees instead of tenths of degrees, and ended up with an
ckpd  encoding range from 1.0 to 360.9, surely not what was intended!

ckpd  Errors like this are _rife_ throughout FNOC code, because "old
ckpd  school" FORTRAN programmers, who have never learned to incorporate
ckpd  good software engineering practices in their code, and hate the
ckpd  very thought, insist on "saving" bytes by omitting inline comments
ckpd  fully describing the calculation, and by typing as little as
ckpd  humanly possible, coding, in this case, real values as "3."
ckpd  instead of "3.0".

ckpd  By so doing, they forget, because they fail to remind themselves
ckpd  constantly with the menmonic "3.0" format in the code, that the
ckpd  data is carrying a significant digit to the right of the decimal
ckpd  place, and so code errors like this occur, that a moment's thought
ckpd  and a frequent reminder in the code would avoid.

ckpd        dir = dir + 359.0

ckpd        dir = amod( dir , 360.0 ) + 1.0

ckpd      Despite all that ranting, even this is imperfect, because the
ckpd      display angle is rarely used, while the full precision angle
ckpd      is passed from computation to computation often.  In the
ckpd      latter case, the result here is an angle between the binary
ckpd      values analogous to the decimal range 000.100 to 360.099.

ckpd      Better perhaps would be the range 000.05000+ to 360.050-,
ckpd      which would round correctly where intended for display use.

ckpd      Luckily the directions are generally handed to trig functions
ckpd      with values still very close to those functions' validated
ckpd      domains of (usually) minus two pi to two pi.

          dir = amod( ( dir + 359.9 ) , 360.0 ) + 0.1

        endif

      endif

c  KPD  This is now nonsense since we made a temporary copy for our use,
c       so don't do it any more.

ckpd  Check for messing with caller's data above, if so, restore it
ckpd  before returning.

ckpd  if ( lngchg ) then

ckpd    myflng = myflng - 180.0

ckpd    mytlng = mytlng - 180.0

ckpd    if ( myflng .le. 0.0 ) myflng = 360.0 + myflng

ckpd    if ( mytlng .le. 0.0 ) mytlng = 360.0 + mytlng

ckpd  endif

c     call prctrc('dirdst',.false.)

      return

ckpd  Notice that we managed to get here needing neither the goto
ckpd  statements nor the statement labels of the spagetti coded
ckpd  original.

      end
