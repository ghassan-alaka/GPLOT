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
      