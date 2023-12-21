function hwrfx_graphics(args)

storm = subwrd(args,1)
date = subwrd(args,2)
fcsthr = subwrd(args,3)
tvalid=subwrd(args,4)
datevalid=subwrd(args,5)
ctlfile = subwrd(args,6)
*shdir = subwrd(args,7)
"reinit"
"open "ctlfile

'set display color white'
'clear'
'set vpage 0 11.0 0 8.5'
'set parea 1 10.0 1 7.5'
'set mproj sps'

'q dims'
xinfo=sublin(result,2)
yinfo=sublin(result,3)
xmin=subwrd(xinfo,6)
numx=subwrd(xinfo,13)
ymin=subwrd(yinfo,6)
numy=subwrd(yinfo,13)

say 'numx 'numx' numy 'numy
'set warn off'
'd ave(maxloc(vt10,y=1,y='numy'),x=1,x='numx')'
rmw=subwrd(result,4)
rmw=rmw-1
say  rmw 

'define uave=ave(vr10,lon=-180,lon=180)'
'define vave=ave(vt10,lon=-180,lon=180)'
*QUADRANT AVERAGES
*EARTH RELATIVE
'define uq1=ave(vr10,lon=0,lon=90)'
'define uq2=ave(vr10,lon=-90,lon=0)'
'define uq3=ave(vr10,lon=0,lon=90)'
'define uq4=ave(vr10,lon=90,lon=180)'
'define vq1=ave(vt10,lon=0,lon=90)'
'define vq2=ave(vt10,lon=-90,lon=0)'
'define vq3=ave(vt10,lon=-180,lon=-90)'
'define vq4=ave(vt10,lon=90,lon=180)'
*SHEAR RELATIVE
*****this needs work
*qstart=shdir - 180
*qsend=qstart + 90
*'define udr=ave(vr10,lon='qstart',lon='qend')'
*'define vdr=ave(vt10,lon='qstart',lon='qend')'
*qstart=qstart + 90
*qend=qend + 90
*'define uur=ave(vr10,lon='qstart',lon='qend')'
*'define vur=ave(vt10,lon='qstart',lon='qend')'
*qstart=qstart + 90
*qend=qend + 90
*'define udl=ave(vr10,lon='qstart',lon='qend')'
*'define vdl=ave(vt10,lon='qstart',lon='qend')'
*qstart=qstart + 90
*qend=qend + 90
*'define udr=ave(vr10,lon='qstart',lon='qend')'
*'define vdr=ave(vt10,lon='qstart',lon='qend')'

'set lev 1000 700'
'set lon 90'

'set gxout shaded'
'set xlab off'
'set ylab off'
'set frame on'
*COMMENT OUT TO ONLY PLOT ANGLE
'set clevs -5 -4 -3 -2 -1 0 1 2 3 4 5'
'set ccols 9 14 4 11 5 13 3 10 7 12 8 2'
'd ave(uu,lon=-180,lon=180)'
'xcbar 10.1 10.6 1.0 7.5 -dir v'


*9 plot inches / 112 gridpoints = 0.08357 inches between x gridpoints
xnow=1
denom=numy-1
xdist=9/denom
rmwx=rmw*xdist
rloc1=1+rmwx
rloc2=rloc1+rmwx
rloc3=rloc2+rmwx
rloc4=rloc3+rmwx

r=2
firstdone=0
'set line 1 1 6'

while(r<=numy)

'q defval uave 1 'r
uaa = subwrd(result,3)
'q defval vave 1 'r
vaa = subwrd(result,3)

angleaa=math_atan2(uaa,vaa)
angleaa=angleaa*57.2956

X=xnow+xdist
xnow=X
hgt=0.1625*angleaa
Y=math_abs(hgt)
Y=Y+1
say 'r = 'r'x=  ' X 
say 'angle = 'angleaa'y=  ' Y

if (firstdone)
'draw line 'xold' 'yold' 'X' 'Y
'draw mark 3 'xold' 'yold' 0.1'
else
'draw mark 3 'X' 'Y' 0.1'
endif

firstdone=1
xold=X
yold=Y
r=r+1
endwhile

*NE quadrant
xnow=1

r=2
firstdone=0
'set line 15 1 6'

while(r<=numy)

'q defval uq1 1 'r
uaa = subwrd(result,3)
'q defval vq1 1 'r
vaa = subwrd(result,3)

angleaa=math_atan2(uaa,vaa)
angleaa=angleaa*57.2956

X=xnow+xdist
xnow=X
hgt=0.1625*angleaa
Y=math_abs(hgt)
Y=Y+1

if (firstdone)
'draw line 'xold' 'yold' 'X' 'Y
'draw mark 2 'xold' 'yold' 0.05'
else
'draw mark 2 'X' 'Y' 0.05'
endif

firstdone=1
xold=X
yold=Y
r=r+1
endwhile

*NW quadrant
xnow=1

r=2
firstdone=0
'set line 15 5 6'

while(r<=numy)

'q defval uq2 1 'r
uaa = subwrd(result,3)
'q defval vq2 1 'r
vaa = subwrd(result,3)

angleaa=math_atan2(uaa,vaa)
angleaa=angleaa*57.2956

X=xnow+xdist
xnow=X
hgt=0.1625*angleaa
Y=math_abs(hgt)
Y=Y+1
if (firstdone)
'draw line 'xold' 'yold' 'X' 'Y
'draw mark 3 'xold' 'yold' 0.05'
else
'draw mark 3 'X' 'Y' 0.05'
endif

firstdone=1
xold=X
yold=Y
r=r+1
endwhile
xnow=1

*SW quadrant
r=2
firstdone=0
'set line 0 1 6'

while(r<=numy)

'q defval uq3 1 'r
uaa = subwrd(result,3)
'q defval vq3 1 'r
vaa = subwrd(result,3)

angleaa=math_atan2(uaa,vaa)
angleaa=angleaa*57.2956

X=xnow+xdist
xnow=X
hgt=0.1625*angleaa
Y=math_abs(hgt)
Y=Y+1
if (firstdone)
'draw line 'xold' 'yold' 'X' 'Y
'draw mark 2 'xold' 'yold' 0.05'
else
'draw mark 2 'X' 'Y' 0.05'
endif

firstdone=1
xold=X
yold=Y
r=r+1
endwhile

*SE quadrant
xnow=1

r=2
firstdone=0
'set line 0 5 6'

while(r<=numy)

'q defval uq4 1 'r
uaa = subwrd(result,3)
'q defval vq4 1 'r
vaa = subwrd(result,3)

angleaa=math_atan2(uaa,vaa)
angleaa=angleaa*57.2956

X=xnow+xdist
xnow=X
hgt=0.1625*angleaa
*Only plot inflow (negative angle)
if ( hgt <= 0.0 )
   Y=math_abs(hgt)
   Y=Y+1
   if (firstdone)
      'draw line 'xold' 'yold' 'X' 'Y
      'draw mark 3 'xold' 'yold' 0.05'
   else
      'draw mark 3 'X' 'Y' 0.05'
   endif
endif
firstdone=1
xold=X
yold=Y
r=r+1
endwhile

*end of inflow angle calculation
'set line 1 1 8'
'set string 1 bl 3 0'

'draw line 'rloc1' .9 'rloc1' 1'
'draw string 'rloc1' .75 1'
if ( rloc2 <= 10.0 )
'draw line 'rloc2' .9 'rloc2' 1'
'draw string 'rloc2' .75 2'
endif
if ( rloc3 <= 10.0 )
'draw line 'rloc3' .9 'rloc3' 1'
'draw string 'rloc3' .75 3'
endif
if (rloc4 <= 10.0 )
'draw line 'rloc4' .9 'rloc4' 1'
'draw string 'rloc4' .75 4'
endif
'draw string 5.5 .5 R/RMW'

'draw string 0.7 1.00 0'
'draw string 0.7 1.8125 5'
'draw string 0.7 2.625 10'
'draw string 0.7 3.4375 15'
'draw string 0.7 4.25 20'
'draw string 0.7 5.0625 25'
'draw string 0.7 5.875 30'
'draw string 0.7 6.6875 35'
'draw string 0.7 7.5 40'

'set string 1 tc 2 0'
"draw string 7.5 0.25 Initial date: "date
"set string 2 tl 1 0" ; "draw string 0.05 8.45 Experimental Product"
"set strsiz 0.2 0.2"
"set string 1 tc 3";"draw string 5.5 8.2 Radial Wind Speed [m/s] and Inflow angle"
"draw string 5.5 7.9 "storm" "fcsthr"hr Forecast "

'set strsiz 0.1 0.1'
'set string 1 bl 7 0'
'draw string 7.5 3.0 AZ Average'
'set string 15 bl 7 0'
'set line 15 1 6'
'draw mark 2 7.4 2.75 0.05'
'draw string 7.5 2.75 NEQ Average'
'draw mark 3 7.4 2.5 0.05'
'draw string 7.5 2.5 NWQ Average'
'set string 0 bl 7 0'
'set line 0 1 6'
'draw mark 2 7.4 2.25 0.05'
'draw string 7.5 2.25 SWQ Average'
'draw mark 3 7.4 2.0 0.05'
'draw string 7.5 2.0 SEQ Average'
'set string 1 c 1 90'
'draw string 0.3 4.25 ANGLE (deg ) '

figname=storm'.'date'.'fcsthr'hr.IANGLE.gif'
'printim 'figname' gif x1100 y850'
'close 1'
*'quit'


