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
      