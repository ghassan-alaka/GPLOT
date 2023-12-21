function sethwindColors(args)

*Display 10m wind speed in HWIND format
*Assumes that grid center is also storm center

*ARGUMENT EXAMPLE
*storm name: matthew
*storm num:  14l
*date: 2016100512 (forecast initial time)
*fcsthr: 030 (must be 3 digit)
*tvalid: 18  (forecast valid time)
*datevalid 20161006 (forecast valid date)

storm = subwrd(args,1)
stormnum = subwrd(args,2)
date = subwrd(args,3)
fcsthr = subwrd(args,4)
tvalid = subwrd(args,5)
datevalid = subwrd(args,6)

*UPP OUTPUT
ctlfile = storm stormnum'.'date'.hwrfprs.core.0p02.f'fcsthr'.grb2.ctl'
*DIAPOST OUTPUT
*ctlfile=height_cpr.ctl
figname = storm stormnum'.'date'.'fcsthr'.HWIND_10M.gif'
"open "ctlfile
"set display color white"
"clear"

"set rgb 16 0 0 0"
"set rgb 17 0 24 255"
"set rgb 18 0 65 255"
"set rgb 19 0 106 255"
"set rgb 20 0 148 255"
"set rgb 21 0 189 255"
"set rgb 22 0 222 255"
"set rgb 23 0 246 255"
"set rgb 24 0 255 199"
"set rgb 25 0 255 143"
"set rgb 26 0 255 7"
"set rgb 27 159 255 0"
"set rgb 28 199 255 0"
"set rgb 29 247 255 0"
"set rgb 30 255 143 0"
"set rgb 31 255 103 0"
"set rgb 32 255 47 0"
"set rgb 33 255 0 111"
"set rgb 34 255 0 151"
"set rgb 35 255 0 191"
"set rgb 36 255 0 239"
"set rgb 37 248 0 255"
"set rgb 38 237 0 255"
"set rgb 39 226 0 255"
"set rgb 40 215 0 255"
"set rgb 41 204 0 255"
"set rgb 42 193 0 255"
"set rgb 43 182 0 255"
"set rgb 44 170 0 255"
"set rgb 45 159 0 255"
"set rgb 46 153 0 255"
"set rgb 47 142 0 255"
"set rgb 48 255 255 255"


"set lev 35"
***************************
*DIAPOST CYLINDRICAL OUTPUT
**"set mproj sps"
**"set mpdraw off"
***************************
*"define wind10=mag(u10,v10)*1.94"
**************************
*UPP LATLON OUTPUT
**************************
"define wind10=mag(UGRD10m,VGRD10m)*1.94"
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
say minlat' 'minlon' 'maxlat' 'maxlon' 'nptsx' 'nptsy' 'timenow
clat=minlat+maxlat
clat=clat/2
dlon=maxlon-minlon
dlon=dlon/nptsx
dlat=maxlat-minlat
dlat=dlat/nptsy
dy=dlat*111.11
say minlat' 'maxlat' 'dlat' 'minlon' 'maxlon' 'dlon' 'timenow' 'clat
*latitude in radians
clatr=clat*0.008727
lonfactor=math_cos(clatr)
dlon=dlon*lonfactor
dx=dlon*111.11
say dlon

*SHADED WIND SPEED
"set gxout shaded"
"set grads off"
"set vpage 0 8.5 0 8.5"
"set parea 1 7.5 1 7.5"
"set ccols 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 16 17 18 19 20 21 22"
"set clevs 0 5 10 15 20 25 30 34 40 45 50 55 60 64 70 75 80 85 90 95 100 105 110 115 120 125 130 135 140 145 150 155 160 165 170 175 180 185 190"
"d wind10 "
" set string 1 c 3 0 "
"xcbar 7.6 8.0 1.0 7.5 -dir v -fskip 2"

*10kt CONTOURS
"set gxout contour"
"set clab masked"
"set clevs 0 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 190"
"set ccolor 1"
"set cthick 0.5"
"d wind10"
"set gxout contour"
"set clab off"
*34kt
"set clevs 34"
"set ccolor 1"
"set cthick 8"
"d wind10"
*50kt
"set clevs 50"
"set ccolor 1"
"set cthick 8"
"d wind10 "
*64kt
"set clevs 64 "
"set ccolor 1"
"set cthick 8"
"d wind10 "
**************************
*DIAPOST CYLINDRICAL OUTPUT
*"define ukts=u10*1.94"
*"define vkts=v10*1.94"
**************************
*UPP LATLON OUTPUT
"define ukts=UGRD10m*1.94"
"define vkts=VGRD10m*1.94"
***************************
"set gxout vector"
"set strmden 1"
"set arrowhead  .05"
"set  arrscl    .15"
"set ccolor 0"
**************************
*DIAPOST CYLINDRICAL OUTPUT
*"d skip(-vkts,20);skip(ukts,20)"
*'set warn off'
*'d ave(maxloc(v10,y=1,y=135),x=1,x=361))'
*rmw=subwrd(result,4)
*rmw=math_format('%5.2f',rmw)
*say 'rmw ' rmw
*'q gr2xy 180 'rmw
*yloc=subwrd(result,6)
********************************
*UPP LATLON OUPUT
"d skip(ukts,20);skip(vkts,20)"
*********************************
********************************
********************************
*UPP LAT/LON DATA
*Need to manually provide rmw
*The code below will draw a circle with radius RMW
prompt "enter model RMW (nm) "
pull rmwmod
say rmwmod
*convert nautical miles to kilometers
rmw=rmwmod*1.854
*degrees latitude
rmw=rmw/111.11

say rmw
rloc=rmw+clat
say rloc
'q ll2xy 'minlon' 'rloc
yloc=subwrd(result,6)
***********************
*UPP and DIAPOST
************************
ring1=4.25 -  yloc
ring1=ring1 + ring1
ring1=math_abs(ring1)
say ring1
'set line 2 1 6'
'draw mark 2 4.25 4.25 'ring1

" printim "figname' gif x1700 y1700'
*"close 1"
*" quit "



