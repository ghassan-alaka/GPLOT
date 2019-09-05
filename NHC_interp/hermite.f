c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine hermite ( ntauf, n, t, lat, lon, m, th, lath, lonh )
c***  This subroutine uses a couple of borrowed hermite spline
c***  interpolation routines.  The idea is that you pass the
c***  routines consensus lat, lon, and time information and
c***  the routine passes back hourly interpolated lat/lon
c***  information.
c***
c***  An interpolation case used in my GPCE-AX matlab code is
c***  provided for comparison
c***
c***  Original code: Jim Hansen
c***  Current programmer: Sampson, NRL
c***  Last modified: March 2009

      implicit none

c***  Length of the raw and interpolated datasets
      integer ltau
      integer n, m
      integer ntauf, mtauf

      integer i
      integer iprint

c***  Variable that hold various Hermite bits
      real c(4,n)

c***  Variables that hold the baseline time (t), lat (lat) or
c***  lon (lon), and derivitive (slopes) information.
      real t(n), lat(n), lon(n), slopes(n), hdir(n)

c***  Interploted lat and lon (lath, lonh) and associated time
c***  values
      real th(m), lath(m), lonh(m)

c***  Debugging print
c***
      iprint = 0

c***  Compute hours
c***
      do i= 1, m
         th(i) = float(i)-1.0
      enddo

c**********************************************
c***  Latitude interpolation
c**********************************************

c***  Calculate extra-special piece-wise hermite slopes
      call hermite_slope(t,lat,slopes,n)

c***  Populate work array
      do i=1,n
        c(1,i)=lat(i)
        c(2,i)=slopes(i)
      enddo

c***  Set up the Hermite coefficients
      call spline_hermite_set(n,t,c)

c***  Evalulate the Hermite polynomial at desired times
      do i=1,m
         call spline_hermite_val(n,t,c,th(i),lath(i))
      enddo
c**********************************************
c***  Longitude interpolation
c**********************************************

c***  Calculate extra-special piece-wise hermite slopes
      call hermite_slope(t,lon,slopes,n)

c***  Populate work array
      do i=1,n
        c(1,i)=lon(i)
        c(2,i)=slopes(i)
      enddo

c***  Set up the Hermite coefficients
      call spline_hermite_set(n,t,c)

c***  Evalulate the Hermite polynomial at desired times
      do i=1,m
         call spline_hermite_val(n,t,c,th(i),lonh(i))
      enddo

c***  Clean up output
      mtauf = (ntauf - 1)*12
      if (mtauf .gt. 0) then
        do i=mtauf+2,m
            lath(i)=0.0
            lonh(i)=0.0
        enddo
      endif

      if (iprint) then
        print *, 'hourly interpolated track'
        print *, 'tau    lat     lon'
        do i=1,m
           write (6, '(f6.0, 2f8.2)' ) th(i), lath(i), lonh(i)
        enddo
      endif

      return
      end
      subroutine greatcirc(x1,x2,d)

c***  Calculates the great circle distance between two points,
c***  X1, and X2.  X1 and X2 are 2d vectors that contain the
c***  latitude and longitude (in degrees, and in that order) of the
c***  points of interest.  The result is returned in nautical miles,
c***  and the calculation assumes that the earth is spherical.
c***
c***  Written by: Jim Hansen
c***  Last modified: Febrary 2009

      implicit none

c***  Pi and the radius of the earth
      real pi, re
      parameter(pi=3.14159,re=6371.)

c***  Input locations and distance
      real x1(2), x2(2), d

c***  Work variables
      real dtr, p1, p2, p3

c***  Check to see if the distance is zero
      if(abs(x1(1)-x2(1))<1.e-10.and.abs(x1(2)-x2(2))<1.e-10)then

         d=0.

c***  If not zero, then calculate distance
      else

c***  Conversion factor for degrees to radians
         dtr=pi/180.

c***  Trig magic to calculate distance in km
         p1=cos(dtr*x1(1))*cos(dtr*x1(2))
     >     *cos(dtr*x2(1))*cos(dtr*x2(2))
         p2=cos(dtr*x1(1))*sin(dtr*x1(2))
     >     *cos(dtr*x2(1))*sin(dtr*x2(2))
         p3=sin(dtr*x1(1))*sin(dtr*x2(1))
         d=acos(p1+p2+p3)*re

c***  Convert to nautical miles
         d=d/1.852

      endif

      return
      end

      subroutine mat_vect_mult(A,n,m,b,c)

c***  This subroutine computes c=A*b where A is
c***  an nxm matrix, b is a vector of length m, and
c***  c is a vector of length n.
c***
c***  Written by: Jim Hansen
c***  Last modified: February 2009

      implicit none

      integer i, j, n, m

      real A(n,m), B(m), C(n)
      real sumvar

      do i=1,n
         sumvar=0.
         do j=1,m
            sumvar=sumvar+A(i,j)*b(j)
         enddo
         c(i)=sumvar
      enddo

      return
      end
      subroutine direction_and_spread(nt,nth,nens,t,ti,
     >                                lat,lonw,glat,glonw,
     >                                lat_her,lon_herw,
     >                                theta,asprd,xsprd)

c***  This routine calculates the across-track and along-track
c***  directions using finite differenced Hermite-polynomial
c***  track information, calculates the across-track angle
c***  (angle defined in the math sense), and then calculates
c***  the across-track and along-track ensemble spread.
c***
c***  Written by: Jim Hansen
c***  Modified: Sampson March 2009  handle longitudes in ATCF coords

      implicit none

c***  nt: number of forecast times in the consensus forecast
c***  nth: number of forecast times in the Hermite interpolated forecast
c***  nens: number of ensemble members.
      integer nt, nth, nens

      integer ijim, i, j, k
      integer map(nt)

      real pi
      parameter(pi=3.14159)

c***  lat/lon: consensus forecast
c***  glat/glon: members of consensus forecast
c***  lat_her/lon_her: hourly consensus forecast positions
c***  t/ti: forecast leads from raw and hourly interpolated forecasts
c***  bstorm: storm number
c***  base: dtg
      real lon(nt), lat(nt)
      real glon(nt,nens), glat(nt,nens)
      real lon_her(nth), lat_her(nth)
      real t(nt), ti(nth)
cx    These are input longitudes that wrap 0-360 toward West
cx    We will convert them to wrap 0-360 toward East
      real lonw(nt)
      real glonw(nt,nens)
      real lon_herw(nth)

c***  Bits needed for angle calculation
      real tang(2)
      real tan_lat, tan_lon
      real tan_norm, slope, degtheta, dtr

c***  Bits needed for across-along distances
      real R(2,2), R2(2,2)
      real error(2)
      real acr_dis(2), alo_dis(2)
      real acr(2), alo(2)
      real con(2), pert_a(2), pert_x(2)
      real gacross(nens), galong(nens)
      real rot_dis(2)
      real suma, sumx, ri
      real gd_a, gd_x

c***  theta: across-track angle (math convention)
c***  aspred/xspred: along and across track spread
      real theta(nt), asprd(nt), xsprd(nt)


c***  Conversion factor for degrees to radians
      dtr=pi/180.

cx
cx    GPCE-AX requires longitude to increase toward East
cx    so convert these three sets of longitudes
      do i=1,nt
        if (lonw(i) .gt. 0.0 .and. lonw(i) .le. 360.0)  then
          lon(i)= 360.0 - lonw(i)
        else
          lon(i)= -999.0
        endif
      enddo

      do i=1,nt
        do j=1,nens
           if (glonw(i,j) .gt. 0.0 .and. glonw(i,j) .le. 360.0) then
             glon(i,j)=360.0 - glonw(i,j)
           else
             glon(i,j)=-999.0
           endif
        enddo
      enddo

      do i=1,nth
         if (lon_herw(i) .gt. 0.0 .and. lon_herw(i) .le. 360.0) then
             lon_her(i)=360.0 - lon_herw(i)
         else
             lon_her(i)=-999.0
         endif
      enddo


c***  To start, map the hourly interpolated forecast
c***  leads to the raw 6, 12, 24, etc. forecast leads
      do i=1,nt
         do j=1,nth
            if(abs(ti(j)-t(i))<1.e-3)then
               map(i)=j
            endif
         enddo
      enddo

c***  Loop over each raw forecast lead
      do ijim=1,nt

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c***  Do some finite differencing to calculate the slope of
c***  the across-track direction and the associated angle.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c***  If the first or last forecast point, use forward
c***  differencing to calculate tangent components, otherwise
c***  use centered differencing.
         if(ijim.ne.1.and.ijim.ne.nt)then
            tan_lat=lat_her(map(ijim)+1)-lat_her(map(ijim)-1)
            tan_lon=lon_her(map(ijim)+1)-lon_her(map(ijim)-1)
         elseif(ijim.eq.1)then
            tan_lat=lat_her(map(ijim)+1)-lat_her(map(ijim))
            tan_lon=lon_her(map(ijim)+1)-lon_her(map(ijim))
         else
            tan_lat=lat_her(map(ijim))-lat_her(map(ijim)-1)
            tan_lon=lon_her(map(ijim))-lon_her(map(ijim)-1)
         endif

c***  Calculate the tangent.  Actually, it's the negative
c***  of the direction orthogonal to the tangent.  The
c***  actual tangent would reverse tang(1) and tang(2).
         tan_norm=(tan_lon**2 + tan_lat**2)**0.5
         tang(1)=tan_lon/tan_norm
         tang(2)=tan_lat/tan_norm

c***  Calculate the angle associated with the tangent, but
c***  make sure you don't divide by zero!
         if(abs(tang(2))<1.e-8)then
            theta(ijim)=90.
            tang(1)=1.
            tang(2)=0.

c***  Note that the '-' in the slope calculation makes
c***  it the slope of the across-track direction.
         else
            slope=-tang(1)/tang(2)
            degtheta=atan(slope)
            degtheta=degtheta/dtr
            theta(ijim)=degtheta
         endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c***  Now play some games to figure out the real angle
c***  based on a knowledge of how fortran does atan.
c***
c***  BUCK: PLEASE CHECK THIS!!
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c***  Atan handles 0-90 just fine, but 91 is -89, and
c***  the computed angle grows to zero as the actual
c***  angle goes to 180.  To fix this, if you diagnose
c***  that you are in the second quadrant you simply
c***  add 180 to the angle value.
c***
c***  I know the across-track direction is in the second
c***  quadrant when both tang(1) and tang(2) are negative.
         if(tang(1).lt.0.0.and.tang(2).lt.0.0)then
            theta(ijim)=180.+degtheta
         endif

c***  The computed angle starts over at 1 degree for
c***  a true angle of 181, and the computed angle increases
c***  to 90 as the true angle increases to 270.  Again,
c***  to fix this all you have to do is diagnose you are
c***  in the 3rd quadrant and add 180 to the computed angle.
c***
c***  I know the across-track direction is in the 3rd
c***  quadrant when tang(1) is positive and tang(2) is
c***  negative.
         if(tang(1).gt.0.0.and.tang(2).lt.0.0)then
cx    I think the logic here is wrong...bs
cx          theta(ijim)=180.+degtheta
            theta(ijim)=-(90.+degtheta)
         endif

c***  At a true angle of 271, the computed angle goes
c***  back to -89 and grows to zero as the true angle
c***  approaches 360.  To fix this problem, diagnose that
c***  you are in the 4th quadrant and add 360 to the
c***  computed angle.
c***
c***  I know the across-track direction is in the 4th
c***  quadrant when tang(1) and tang(2) are both positive.
         if(tang(1).gt.0.0.and.tang(2).gt.0.0)then
            theta(ijim)=360.+degtheta
         endif

c***  The final clean-up for the rotation angle is to
c***  make 181-359 -199 to -1.
         if(theta(ijim)>180)then
            theta(ijim)=theta(ijim)-360.
         endif

c***  Define a rotation matrix that goes from lat/lon
c***  space to across/along space
         R(1,1)=cos(dtr*theta(ijim))
         R(1,2)=sin(dtr*theta(ijim))
         R(2,1)=-sin(dtr*theta(ijim))
         R(2,2)=cos(dtr*theta(ijim))

c***  Define a back-rotation matrix that goes from
c***  across/along space to lat/lon space
         R2(1,1)=cos(-dtr*theta(ijim))
         R2(1,2)=sin(-dtr*theta(ijim))
         R2(2,1)=-sin(-dtr*theta(ijim))
         R2(2,2)=cos(-dtr*theta(ijim))
        if(theta(ijim).eq.90)then
            R(1,1)=0.
            R(2,2)=0.
            R2(1,1)=0.
            R2(2,2)=0.
         endif

c***  Calculate the across and along distance from the
c***  mean to each available ensemble member
         j=0
         do i=1,nens

c***  Make sure the ensemble member is there!
            if(glon(ijim,i).gt.-999)then
               j=j+1

c***  Calculate lat/lon distance from concensus to ensemble member
               error(1)=lon(ijim)-glon(ijim,i)
               error(2)=lat(ijim)-glat(ijim,i)

c***  Rotate into across/along space
               call mat_vect_mult(R,2,2,error,rot_dis)

c***  Grab across-track bit of distance and rotate back
               acr_dis(1)=rot_dis(1)
               acr_dis(2)=0.
               call mat_vect_mult(R2,2,2,acr_dis,acr)

c***  Grab along-track bit of distance and rotate back
               alo_dis(1)=0.
               alo_dis(2)=rot_dis(2)
               call mat_vect_mult(R2,2,2,alo_dis,alo)

c***  For great circle distance calculations, define lat/lon
c***  vectors for the consensus
               con(1)=lat(ijim)
               con(2)=lon(ijim)

c***  Define lat/lon vector for across-track bit of ensemble
c***  member
               pert_x(1)=con(1)+acr(2)
               pert_x(2)=con(2)+acr(1)

c***  Calculate across-track great circle distance
               call greatcirc(con,pert_x,gd_x)
               gacross(j)=gd_x

c***  Define lat/lon vector for along-track bit of ensemble
c***  member
               pert_a(1)=con(1)+alo(2)
               pert_a(2)=con(2)+alo(1)

c***  Calculate across-track great circle distance
               call greatcirc(con,pert_a,gd_a)
               galong(j)=gd_a

c***  End of -999 conditional
            endif

c***  End of ensemble member loop
         enddo

c***  Calculate the across/along spread as the average
c***  distance from the consensus
         suma=0.
         sumx=0.
         ri=0.

c***  Sum up the distances
         do i=1,j
            ri=ri+1.
            suma=suma+galong(i)
            sumx=sumx+gacross(i)
         enddo

c***  Compute average
         asprd(ijim)=suma/ri
         xsprd(ijim)=sumx/ri

      enddo

      end
      subroutine hermite_slope(x,y,slopes,n)

c***  To be honest, at this point I don't entirely understand
c***  why it is necessary to come up with these fancy estimate
c***  of derivatives at the input points provided for the
c***  hermite polynomial interpolation.
c***
c***  Because I generate my GPCE-AX coefficients using across
c***  and along directions based on the Matlab Hermite polynomial
c***  interpolation, I want to be sure that the ATCF reproduces
c***  the Matlab interpolation as best as possible.
c***
c***  I went into the Matlab scripts and found that hidden in
c***  the pchip routine, which "provides the piecewise polynomial
c***  form of a certain shape-preserving piecewise cubic Hermite
c***  interpolant".  To achieve this aim, there are special
c***  "derivative values for shape-preserving Piecewise Cubic
c***  Hermite Interpolation".  A fortran conversion of the
c***  associated Matlab routine is provided here.
c***
c***  x is the independent variable (time, in this case)
c***  y is the dependent variable (lat or lon, in this case)
c***  slopes is the derivatives of y at x
c***  n is the size of x and y (and slopes)
c***
c***  Translated by: Jim Hansen
c***  Last modified: February 2009

      implicit none

      integer i, n

      real x(n), y(n)
      real h(n-1), dy(n-1), del(n-1)
      real hs(n), w1(n), w2(n), dmax(n), dmin(n)
      real signd(n), signdel(n-1)
      real slopes(n)

c***  Initialize
      do i=1,n
         slopes(i)=0.
      enddo

c***  Produce some finite difference variables for the
c***  dependent and independent vectors, and then an
c***  associated simple derivative.
      do i=1,n-1
         h(i)=x(i+1)-x(i)
         dy(i)=y(i+1)-y(i)
         del(i)=dy(i)/h(i)
      enddo

c***  Calculate slopes at interior points.
c***     d(k) = weighted average of del(k-1) and del(k)
c***            when they have the same sign.
c***     d(k) = 0 when del(k-1) and del(k) have opposites signs
c***            or either is zero.
      do i=1,n-2
         hs(i)=h(i)+h(i+1)
         w1(i)=(h(i)+hs(i))/(3*hs(i))
         w2(i)=(hs(i)+h(i+1))/(3*hs(i))
         dmax(i)=max(abs(del(i)),abs(del(i+1)))
         dmin(i)=min(abs(del(i)),abs(del(i+1)))
         slopes(i+1)=dmin(i)/(w1(i)*(del(i)/dmax(i))
     >              +w2(i)*(del(i+1)/dmax(i)))
      enddo

c***  Calculate slopes at end points.
c***     Set d(1) and d(n) via non-centered, shape-preserving
c***     three-point formulae.
      slopes(1)=((2.*h(1)+h(2))*del(1)-h(1)*del(2))/(h(1)+h(2))
      do i=1,n
         if(slopes(i).eq.0.)then
            signd(i)=0
         endif
         if(slopes(i).gt.0.)then
            signd(i)=1
         endif
         if(slopes(i).lt.0.)then
            signd(i)=-1
         endif
      enddo
      do i=1,n-1
         if(del(i).eq.0.)then
            signdel(i)=0
         endif
         if(del(i).gt.0.)then
            signdel(i)=1
         endif
         if(del(i).lt.0.)then
            signdel(i)=-1
         endif
      enddo
      if(signd(1).ne.signdel(1))then
         slopes(1)=0.
      elseif(signdel(1).ne.signdel(2))then
         if(abs(slopes(1)).gt.abs(3.*del(1)))then
            slopes(1)=3.*del(1)
         endif
      endif

      slopes(n)=((2.*h(n-1)+h(n-2))*del(n-1)
     >         -h(n-1)*del(n-2))/(h(n-1)+h(n-2))
      do i=n,n
         if(slopes(i).eq.0.)then
            signd(i)=0
         endif
         if(slopes(i).gt.0.)then
            signd(i)=1
         endif
         if(slopes(i).lt.0.)then
            signd(i)=-1
         endif
      enddo
      if(signd(n).ne.signdel(n-1))then
         slopes(n)=0.
      elseif(signdel(n-1).ne.signdel(n-2))then
         if(abs(slopes(n)).gt.abs(3.*del(n-1)))then
            slopes(n)=3.*del(n-1)
         endif
      endif

      return
      end
