function hwrf_graphics(args)

ctlfile=subwrd(args,1)

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
nptsz=46
timenow=subwrd(tdim,6)
'q ctlinfo'
xinfo=sublin(result,4)
yinfo=sublin(result,5)
dx=subwrd(xinfo,5)
dy=subwrd(yinfo,5)
yc=(nptsy-1)/2
xc=(nptsx-1)/2
clat=minlat+maxlat
clat=clat/2

"set lev 250"
"define U250=UGRDprs"
"define V250=VGRDprs"
"set lev 800"
"define U800=UGRDprs"
"define V800=VGRDprs"
"set lev 500"
"define U500=UGRDprs"
"define V500=VGRDprs"
"define UdiffD=U250-U800"
"define VdiffD=V250-V800"
"define UdiffS=U500-U800"
"define VdiffS=V500-V800"

clatr=clat*0.008727
lonfactor=math_cos(clatr)
dxkm=dx*111.11
dxkm=dxkm*lonfactor
dykm=dy*111.11
*Determine x/y points to limit shear to between 200 and 500 km from storm center
nx=200/dxkm
nx=math_int(nx)
ny=200/dykm
ny=math_int(ny)
y2=yc-ny
x2=xc-nx
y3=yc+ny
x3=xc+nx
nx=700/dxkm
nx=math_int(nx)
ny=700/dykm
ny=math_int(ny)
y1=yc-ny
x1=xc-nx
y4=yc+ny
x4=xc+nx

say 'storm center cut out from x = 'x2'-'x3' and y = 'y2'-'y3

*Comment out to include storm in calculation
"Uave=ave(ave(UdiffD,x="x1",x="x4"),y="y1",y="y4")"
"Uavevort=ave(ave(UdiffD,x="x2",x="x3"),y="y2",y="y3")"
"define UaveD=Uave-Uavevort"
"Vave=ave(ave(VdiffD,x="x1",x="x4"),y="y1",y="y4")"
"Vavevort=ave(ave(VdiffD,x="x2",x="x3"),y="y2",y="y3")"
"define VaveD=Vave - Vavevort"
"Uave=ave(ave(UdiffS,x="x1",x="x4"),y="y1",y="y4")"
"Uavevort=ave(ave(UdiffS,x="x2",x="x3"),y="y2",y="y3")"
"define UaveS=Uave-Uavevort"
"Vave=ave(ave(VdiffS,x="x1",x="x4"),y="y1",y="y4")"
"Vavevort=ave(ave(VdiffS,x="x2",x="x3"),y="y2",y="y3")"
"define VaveS=Vave - Vavevort"

*Uncomment to include storm in calculation
*"define UaveD=ave(ave(UdiffD,x=1,x="nptsx"),y=1,y="nptsy")"
*"define VaveD=ave(ave(VdiffD,x=1,x="nptsx"),y=1,y="nptsy")"
*"define UaveS=ave(ave(UdiffS,x=1,x="nptsx"),y=1,y="nptsy")"
*"define VaveS=ave(ave(VdiffS,x=1,x="nptsx"),y=1,y="nptsy")"

r2deg=57.2958

"d mag(UaveD,VaveD)"
DLMSH=subwrd(result,4)
DLMSHkts=DLMSH*1.944
DLMSH=math_format('%4.1f',DLMSH)
DLMSHkts=math_format('%4.1f',DLMSHkts)

"d UaveD"
u=subwrd(result,4)
"d VaveD"
v=subwrd(result,4)

SHDIRD=math_atan2(u,v)
SHDIRD=SHDIRD*r2deg

say 'deep layer shear'DLMSH'm/s or'DLMSHkts'kts'
say 'deep layer shear dir 'SHDIRD'deg'

"d mag(UaveS,VaveS)"
SLMSH=subwrd(result,4)
SLMSHkts=SLMSH*1.944
SLMSH=math_format('%4.1f',SLMSH)
SLMSHkts=math_format('%4.1f',SLMSHkts)
"d UaveS"
u2=subwrd(result,4)
"d VaveS"
v2=subwrd(result,4)
say 'u2 v2 'u2' 'v2
SHDIRS=math_atan2(u2,v2)
SHDIRS=SHDIRS*r2deg

say 'midlevel shear 'SLMSH'm/s or'SLMSHkts'kts'
say 'mid level shear dir 'SHDIRS'deg'

"set vpage 0 11.0 0 8.5"
"set gxout line"
"set line 1 1 2"
rangle=90 - SHDIRD
if(rangle < 0.0 )
rangle = rangle + 360
endif
rangle=rangle*0.017453
x1=9.7
y1=4
y2=y1+math_sin(rangle)
y2=math_format('%4.2f',y2)
x2=x1+math_cos(rangle)
x2=math_format('%4.2f',x2)
xh=x2 - ((x2- x1)/3)
yh=y2 - ((y2 - y1)/3)
'draw line 'x1' 'y1' 'x2' 'y2
x1 = xh - (0.3 *  math_sin(rangle))
y1 = yh + (0.3 * math_cos(rangle))
'draw line 'x1' 'y1' 'x2' 'y2
x1= xh + (0.3 * math_sin(rangle))
y1= yh - (0.3 * math_cos(rangle))
'draw line 'x1' 'y1' 'x2' 'y2
'set string 1 bl 1 0'
'draw string 9.15 3.65 SHEAR'
'draw string 9.15 3.45 800-200'
'draw string 9.15 3.25 'DLMSH' m/s'

rangle=90 - SHDIRS
if(rangle < 0.0 )
rangle = rangle + 360
endif
rangle=rangle*0.017453
x1=9.7
y1=2
y2=y1+math_sin(rangle)
y2=math_format('%4.2f',y2)
x2=x1+math_cos(rangle)
x2=math_format('%4.2f',x2)
xh=x2 - ((x2- x1)/3)
yh=y2 - ((y2 - y1)/3)
'draw line 'x1' 'y1' 'x2' 'y2
x1 = xh - (0.3 *  math_sin(rangle))
y1 = yh + (0.3 * math_cos(rangle))
'draw line 'x1' 'y1' 'x2' 'y2
x1= xh + (0.3 * math_sin(rangle))
y1= yh - (0.3 * math_cos(rangle))
'draw line 'x1' 'y1' 'x2' 'y2
'set string 1 bl 1 0'
'draw string 9.15 1.65 SHEAR'
'draw string 9.15 1.45 800-500'
'draw string 9.15 1.25 'SLMSH'm/s'
"close 1"


































