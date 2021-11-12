subroutine spline_hermite_set ( ndata, tdata, c )
!
!*************************************************************************
!
!! SPLINE_HERMITE_SET sets up a piecewise cubic Hermite interpolant.
!
!
!  Reference:
!
!    Conte and de Boor,
!    Algorithm CALCCF,
!    Elementary Numerical Analysis,
!    1973, page 235.
!
!  Modified:
!
!    06 April 1999
!
!  Parameters:
!
!    Input, integer NDATA, the number of data points.
!    NDATA must be at least 2.
!
!    Input, real TDATA(NDATA), the abscissas of the data points.
!    The entries of TDATA are assumed to be strictly increasing.
!
!    Input/output, real C(4,NDATA).
!
!    On input, C(1,I) and C(2,I) should contain the value of the
!    function and its derivative at TDATA(I), for I = 1 to NDATA.
!    These values will not be changed by this routine.
!
!    On output, C(3,I) and C(4,I) contain the quadratic
!    and cubic coefficients of the Hermite polynomial
!    in the interval (TDATA(I), TDATA(I+1)), for I=1 to NDATA-1.
!    C(3,NDATA) and C(4,NDATA) are set to 0.
!
!    In the interval (TDATA(I), TDATA(I+1)), the interpolating Hermite
!    polynomial is given by
!
!    SVAL(TVAL) =                 C(1,I)
!       + ( TVAL - TDATA(I) ) * ( C(2,I)
!       + ( TVAL - TDATA(I) ) * ( C(3,I)
!       + ( TVAL - TDATA(I) ) *   C(4,I) ) )
!
  implicit none
!
  integer ndata
!
  real c(4,ndata)
  real divdif1
  real divdif3
  real dt
  integer i
  real tdata(ndata)
!
  do i = 1, ndata-1
    dt = tdata(i+1) - tdata(i)
    divdif1 = ( c(1,i+1) - c(1,i) ) / dt
    divdif3 = c(2,i) + c(2,i+1) - 2.0E+00 * divdif1
    c(3,i) = ( divdif1 - c(2,i) - divdif3 ) / dt
    c(4,i) = divdif3 / dt**2
  end do

  c(3,ndata) = 0.0E+00
  c(4,ndata) = 0.0E+00

  return
end
subroutine spline_hermite_val ( ndata, tdata, c, tval, sval )
!
!*************************************************************************
!
!! SPLINE_HERMITE_VAL evaluates a piecewise cubic Hermite interpolant.
!
!
!  Discussion:
!
!    SPLINE_HERMITE_SET must be called first, to set up the
!    spline data from the raw function and derivative data.
!
!  Reference:
!
!    Conte and de Boor,
!    Algorithm PCUBIC,
!    Elementary Numerical Analysis,
!    1973, page 234.
!
!  Modified:
!
!    06 April 1999
!
!  Parameters:
!
!    Input, integer NDATA, the number of data points.
!    NDATA is assumed to be at least 2.
!
!    Input, real TDATA(NDATA), the abscissas of the data points.
!    The entries of TDATA are assumed to be strictly increasing.
!
!    Input, real C(4,NDATA), contains the data computed by
!    SPLINE_HERMITE_SET.
!
!    Input, real TVAL, the point where the interpolant is to
!    be evaluated.
!
!    Output, real SVAL, the value of the interpolant at TVAL.
!
  implicit none
!
  integer ndata
!
  real c(4,ndata)
  real dt
  integer left
  integer right
  real sval
  real tdata(ndata)
  real tval
!
!  Find the interval [ TDATA(LEFT), TDATA(RIGHT) ] that contains
!  or is nearest to TVAL.
!
  call rvec_bracket ( ndata, tdata, tval, left, right )
!
!  Evaluate the cubic polynomial.
!
  dt = tval - tdata(left)

  sval = c(1,left) + dt * ( c(2,left) + dt * ( c(3,left) + dt * c(4,left) ) )

  return
end

subroutine timestamp ( )
!
!*******************************************************************************
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!
!  Example:
!
!    May 31 2001   9:45:54.872 AM
!
!  Modified:
!
!    31 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none
!
  character ( len = 8 ) ampm
  integer d
  character ( len = 8 ) date
  integer h
  integer m
  integer mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer n
  integer s
  character ( len = 10 )  time
  integer values(8)
  integer y
  character ( len = 5 ) zone
!
  call date_and_time ( date, time, zone, values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(a,1x,i2,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    trim ( month(m) ), d, y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end

subroutine rvec_bracket ( n, x, xval, left, right )
!
!*******************************************************************************
!
!! RVEC_BRACKET searches a sorted array for successive brackets of a value.
!
!
!  Discussion:
!
!    If the values in the vector are thought of as defining intervals
!    on the real line, then this routine searches for the interval
!    nearest to or containing the given value.
!
!  Modified:
!
!    06 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, length of input array.
!
!    Input, real X(N), an array sorted into ascending order.
!
!    Input, real XVAL, a value to be bracketed.
!
!    Output, integer LEFT, RIGHT, the results of the search.
!    Either:
!      XVAL < X(1), when LEFT = 1, RIGHT = 2;
!      XVAL > X(N), when LEFT = N-1, RIGHT = N;
!    or
!      X(LEFT) <= XVAL <= X(RIGHT).
!
  implicit none
!
  integer n
!
  integer i
  integer left
  integer right
  real x(n)
  real xval
!
  do i = 2, n - 1

    if ( xval < x(i) ) then
      left = i - 1
      right = i
      return
    end if

   end do

  left = n - 1
  right = n

  return
end
