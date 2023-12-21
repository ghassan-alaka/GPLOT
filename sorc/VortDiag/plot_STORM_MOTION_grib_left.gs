function hwrf_graphics(args)

ctlfile=subwrd(args,1)
say ctlfile

'open 'ctlfile

linenow=read(stormlocs.dat)
loc=sublin(linenow,2)
lat1=subwrd(loc,1)
linenow=read(stormlocs.dat)
loc=sublin(linenow,2)
lat2=subwrd(loc,1)
linenow=read(stormlocs.dat)
loc=sublin(linenow,2)
lon1=subwrd(loc,1)
linenow=read(stormlocs.dat)
loc=sublin(linenow,2)
lon2=subwrd(loc,1)
linenow=read(stormlocs.dat)
loc=sublin(linenow,2)
tinc=subwrd(loc,1)

xdistdeg=lon2 - lon1
xdistkm=xdistdeg*111.11
ydistdeg=lat2 - lat1
ydistkm=ydistdeg*111.11
clat=lat2 + lat1
clat=clat*0.0094886
coslat=math_cos(clat)
xdistkm=xdistkm*coslat
ydist2=ydistkm*ydistkm
xdist2=xdistkm*xdistkm
dist=ydist2+xdist2
dist=math_sqrt(dist)
spdkh=dist/tinc
*storm speed in kts
*SSPD=spdkh*0.539957
*SSPD=math_format('%4.1f',SSPD)
spdms=spdkh*0.277778
SSPD=math_format('%4.1f',spdms)
say 'spd 'SSPD
dirr=math_atan2(xdistdeg,ydistdeg)
SDIR=dirr*57.2958
say 'dir 'SDIR
  
"set vpage 0 10.5 0 8.5"
"set gxout line"
"set line 1 1 2"
rangle=90-SDIR
if(rangle < 0 )
rangle=rangle+360
endif
rangle=rangle*0.017453
x1=0.7
y1=6
y2=y1+math_sin(rangle)
y2=math_format('%4.2f',y2)
x2=x1+math_cos(rangle)
x2=math_format('%4.2f',x2)
'draw line 'x1' 'y1' 'x2' 'y2
xh=x2 - ((x2- x1)/3)
yh=y2 - ((y2 - y1)/3)
'draw line 'x1' 'y1' 'x2' 'y2
xh=x2 - ((x2- x1)/3)
yh=y2 - ((y2 - y1)/3)
x1 = xh - (0.3 * math_sin(rangle))
y1 = yh + (0.3 * math_cos(rangle))
'draw line 'x1' 'y1' 'x2' 'y2
x1 = xh + (0.3 * math_sin(rangle))
y1 = yh - (0.3 * math_cos(rangle))
'draw line 'x1' 'y1' 'x2' 'y2
'set string 1 bl 1 0'
'draw string 0.15 5.45 MOTION' 
'draw string 0.15 5.25 'SSPD'm/s'
"close 1"


































