function hwrf_graphics(args)

storm=subwrd(args,1)
date=subwrd(args,2)
fcsthr=subwrd(args,3)
timevalid=subwrd(args,4)
datevalid=subwrd(args,5)
clat=subwrd(args,6)
clon=subwrd(args,7)
ctlfile=subwrd(args,8)
doshear=subwrd(args,9)
domotion=subwrd(args,10)
ctlfile2=subwrd(args,11)

'open 'ctlfile
'q dims'
xdims=sublin(result,2)
ydims=sublin(result,3)
tdim=sublin(result,5)
minlon=subwrd(xdims,6)
maxlon=subwrd(xdims,8)
minlat=subwrd(ydims,6)
maxlat=subwrd(ydims,8)
nptsx=subwrd(xdims,13)
nptsy=subwrd(ydims,13)
timenow=subwrd(tdim,6)
'q ctlinfo'
xinfo=sublin(result,4)
yinfo=sublin(result,5)
zinfo=sublin(result,6)
nz=subwrd(zinfo,2)
dx=subwrd(xinfo,5)
dy=subwrd(yinfo,5)
yc=(nptsy-1)/2
xc=(nptsx-1)/2
clat=minlat+maxlat
clat=clat/2

if (nz = 46)
  z1=25
  z2=34
endif
if (nz = 25 )
  z1=14
  z2=25
endif

'set display color white'
'clear'
'set vpage 0 11.0 0 8.5'
'set parea 1 10.0 1 7.5'
'set gxout shaded'
*Possible GRAdS bug - throws an error
*'set mpdset hires'	
'set grads off'
'set lev 1000'
'define wind=mag(UGRD10M,VGRD10M)'
'd wind'
'xcbar 0.15 0.65 1.0 7.5 -dir v -fskip 2'

*Option to plot 700mb wind to diagnose vertical tilt of vortex
'set gxout stream'
'set lev 700'
'set grads off'
'set ccolor 0'
'set cthick 6'
'set strmden -10 1 .1 2'
'd UGRDprs;VGRDprs'

*Convective bursts are defined as Vertical velocities between 8 and 14km
*Approximate pressure levels set to 
*exceeding a threshold value
*this can be set to 5 m/s as in (Rogers et al 2014
*or as a percentage of maximum value

'define wmax=max(max(max(VVELprs,x=1,x='nptsx'),y=1,y='nptsy'),z='z1',z='z2')'
'q defval wmax 1 1'
wmax=subwrd(result,3)
say 'maximum vertical wind speed = 'wmax

*Top 10%
cbmin=0.9*wmax
say cbmin
*5 m/s
*cbmin=5.0

'set gxout line'
'set grads off'
'set line 1 1 2 '
i=1
j=1
levnow=200
while (levnow < 450)
   'set lev 'levnow
   'define www = VVELprs'
   while(j < nptsy )
      while(i < nptsx )
         'q defval 'www' 'i' 'j
          cb=subwrd(result,3)
          if(cb > cbmin)
          'q gr2xy 'i' 'j
          xpos=subwrd(result,3)
          ypos=subwrd(result,6)
          'draw mark 3 'xpos' 'ypos' .1'
          endif
       i = i + 1
     endwhile

     j = j + 1
     i = 1
   endwhile
   levnow=levnow+25
   j = 1
endwhile


*Mark the location of the maximum wind speed
'set warn off'
y1=yc-100
y2=yc+100
x1=xc-100
x2=xc+100
say 'xc yc 'xc' 'yc' x1 x2 'x1' 'x2' y1 y2 'y1' 'y2
'd maxloc(max(wind,y='y1',y='y2'),x='x1',x='x2'))'
rmwy=subwrd(result,4)
'd maxloc(max(wind,x='x1',x='x2'),y='y1',y='y2'))'
rmwx=subwrd(result,4)
say 'rmw i j 'rmwx' 'rmwy
'q gr2xy 'rmwx' 'rmwy
xloc=subwrd(result,3)
yloc=subwrd(result,6)
say 'rmw x y 'xloc' 'yloc
'set line 15 1 8'
'draw mark 1 'xloc' 'yloc' 0.2'

*find the center location
'set lev 850'
'define height=HGTprs'
'd ave(minloc(height,y='y1',y='y2'),x='x1',x='x2'))'
cy=subwrd(result,4)
'd ave(minloc(height,x='x1',x='x2'),y='y1',y='y2'))'
cx=subwrd(result,4)
say 'center i j 'cx' 'cy
'q gr2xy 'cx' 'cy
xcloc=subwrd(result,3)
ycloc=subwrd(result,6)
say '850 center x y 'xcloc' 'ycloc

xdist=xloc-xcloc
xdist=xdist*xdist
say 'xdist 'xdist
ydist=yloc-ycloc
ydist=ydist*ydist
dist=xdist+ydist
ring1=math_sqrt(dist)
*Draw a circle with radius RMW around storm center
ring1=math_abs(ring1)
ring1=ring1 + ring1
ring1=math_abs(ring1)
say ring1
'set line 15 1 8'
'draw mark 2 'xcloc' 'ycloc' 'ring1


'set string 1 c 6 0'
" draw string 5.5 8.2 "storm
" draw string 5.5 7.95 " fcsthr' Forecast Valid 'tvalid ' UTC 'datevalid
" draw string 5.5 7.7 10M WIND SPEED [m/s]"
'set line 15 1 8';'draw mark 1 7.0 2.0 0.2'
'set string 15 l 1 0'; 'draw string 7.3 2.0 Max Wind'
'set line 1 1 8';'draw mark 3 7.0 1.75 0.2'
'set string 1 l 1 0'; 'draw string 7.2 1.75 Conv. Burst'
'set string 2 bl 1 0' ; 'draw string 0.5 0.5 Experimental Product'
figname=storm'.'date'.'fcsthr'.WIND_CB.gif'

'close 1'
*Plot Shear and Storm motion vectors if requested
if(doshear = 1)
'run plot_SHEAR_grib 'ctlfile2
endif

if(domotion = 1)
sspd=subwrd(args,8)
sdir=subwrd(args,9)
'run plot_STORM_MOTION_grib '
endif

'printim 'figname' gif'
'quit'


































