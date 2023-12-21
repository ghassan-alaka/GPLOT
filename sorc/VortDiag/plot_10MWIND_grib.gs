function hwrfx_graphics(args)

*Plots 10M wind speed using the same color scale as TDR plots on AOML wesbsite
*Wind speed contours are in m/s but significant levels are labled in kts

*ARGUMENT EXAMPLE
*storm name: matthew14l (for plot titles and  file names)
*date: 2016100512 (forecast initial time)
*fcsthr: 030 (forecast hour)
*tvalid: 18  (forecast valid time)
*datevalid 20161006 (forecast valid date)
*clat: center latitude  (forecast time)
*clon: center longitude (forecast time)
*ctlfile (name of grads control file)
storm = subwrd(args,1)
date = subwrd(args,2)
fcsthr = subwrd(args,3)
tvalid = subwrd(args,4)
datevalid = subwrd(args,5)
clat = subwrd(args,6)
clon = subwrd(args,7)
ctlfile = subwrd(args,8)

figname = storm'.'date'.'fcsthr'hr.10MWINDSPEED.gif'

" open " ctlfile
"set display color white"
"clear"
"set lev 1000"

"define wind10=mag(UGRD10m,VGRD10m)"


*Define plot size
latr=0.01745*clat
londist=math_cos(latr)
londist=1/londist
londiff=londist*2.5
minlat=clat-2.5
maxlat=clat+2.5
*instead of a 5 x 5 degree grid scale so xdist = ydist = 555.55km
minlon=clon-londiff
maxlon=clon+londiff
*western hemisphere only
*GRADS lon defined from 0-360
minlon=minlon+360
maxlon=maxlon+360
say 'plot range'
say minlat' 'maxlat' 'minlon' 'maxlon

" set lat " minlat' 'maxlat
" set lon " minlon' 'maxlon


"set vpage 0 11.0 0 8.5 "
"set parea 1 10.0 1 7.5"
"set gxout shaded"
"set grads off"
"set mpdset hires"
"set cint 5"
"set clevs 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80"
"set ccols 9 14 4 11 5 13 3 10 7 12 8 2 6 9 5 10"

"d wind10"
" set string 1 r 5 0 "
" xcbar 10.1 10.6 1.0 7.5 -dir v -fskip 2 "

"set gxout contour"
"set ccolor 15"
"set cthick 6"
"set clevs 19.04"
"set clab 34kt"
"set clab masked"
"d wind10"

"set ccolor 15"
"set cthick 6"
"set clevs 27.2"
"set clab 50kt"
"set clab masked"
"d wind10"

"set ccolor 15"
"set cthick 6"
"set clevs 34.816"
"set clab 64kt"
"set clab masked"
"d wind10"
"clear mask"


*GET GRID INFORMATION
"q dims"
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
xinfo=sublin(result,3)
yinfo=sublin(result,4)
dx=subwrd(xinfo,5)
dy=subwrd(yinfo,5)
yc=(nptsy-1)/2
xc=(nptsx-1)/2
*Find center (assumes within 100 grid points of domain center
y1=yc-100
y2=yc+100
x1=xc-100
x2=xc+100


'define vtmax=max(max(wind10,x='x1', x='x2'),y='y1', y='y2')'
'q defval vtmax 1 1'
VMAX=subwrd(result,3)
VMAXnow=math_format('%6.2f',VMAX)
say Max Vt
say VMAXnow

'set warn off'
'define height=HGTprs'
'd ave(minloc(height,y='y1',y='y2'),x='x1',x='x2'))'
cy=subwrd(result,4)
'd ave(minloc(height,x='x1',x='x2'),y='y1',y='y2'))'
cx=subwrd(result,4)
say 'center i j 'cx' 'cy
'q gr2xy 'cx' 'cy
xcloc=subwrd(result,3)
ycloc=subwrd(result,6)
say 'center x y 'xcloc' 'ycloc


*RMW routines fail if large outliers are found
*RMW 
'd maxloc(max(wind10,y='y1',y='y2'),x='x1',x='x2'))'
rmwy=subwrd(result,4)
'd maxloc(max(wind10,x='x1',x='x2'),y='y1',y='y2'))'
rmwx=subwrd(result,4)
'q gr2xy 'rmwx' 'rmwy
xloc=subwrd(result,3)
yloc=subwrd(result,6)
say RMWLOC
say xloc' 'yloc
clocx=5.5
clocy=4.25
distx=0.0117
disty=0.0117

xdist=xcloc - xloc
xdist=math_abs(xdist)
xdistl=xdist/distx
xdist2=math_pow(xdistl,2)
xdist=xdist*xdist
ydist=ycloc - yloc
ydist=math_abs(ydist)
ydistl=ydist/disty
ydist2=math_pow(ydistl,2)
ydist=ydist*ydist
dist=xdist+ydist
dist1=math_sqrt(xdist2+ydist2)
rmwnow=math_format('%6.2f',dist)
say RMW
say rmwnow
say Distance
say xdist' 'ydist
ring=math_sqrt(dist)
ring=ring + ring
ring=math_abs(ring)
'set line 0 1 8'
'draw mark 2 'xcloc' 'ycloc' 'ring


" set string 1 c 6 0 "
xdist=xdist/distx
xdist2=math_pow(xdist,2)
ydist=clocy - yloc
ydist=ydist/disty
ydist2=math_pow(ydist,2)
dist=math_sqrt(xdist2+ydist2)
rmwnow=math_format('%6.2f',dist)


" draw string "clocx" 7.7 "fcsthr"hr Forecast Initialized "date 
" draw string "clocx" 7.95  "storm" "tvalid " UTC "datevalid
" draw string "clocx" 8.2. 10-M WIND SPEED [m/s] "
" draw string 3.5 7.00  10m Max VT (RMW): "
" draw string 3.5 6.75 " VMAXnow ' m/s (' rmwnow ' km )'
" set string 2 bl 1 0" ; "draw string 0.05 0.25 Experimental Product"

'printim 'figname' gif x1100 y850'
'close 1'
'quit'

