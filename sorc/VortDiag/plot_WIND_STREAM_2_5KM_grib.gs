function jetcolors(args)


storm = subwrd(args,1)
date = subwrd(args,2)
fcsthr = subwrd(args,3)
tvalid = subwrd(args,4)
datevalid = subwrd(args,5)
clat = subwrd(args,6)
clon = subwrd(args,7)
ctlfile =  subwrd(args,8)

figname = storm'.'date'.'fcsthr'hr.WINDSTREAM_2_5KM.gif'

" open " ctlfile
" set display color white "
" clear "

latr=0.01745*clat
latdiff=math_cos(latr)
latdiff=1/latdiff
londiff=latdiff*2.5

minlat=clat-2.5
maxlat=clat+2.5
*instead of a 5 x 5 degree grid scale so xdist = ydist = 555.55km
*minlon=clon-2.5
*maxlon=clon+2.5
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

" q dims "
xinfo = sublin(result,2) 
yinfo = sublin(result,3)
say xinfo 
say yinfo
xmin = subwrd(xinfo,11) 
xmax = subwrd(xinfo,13) 
ymin = subwrd(yinfo,11) 
ymax = subwrd(yinfo,13) 

say xmin xmax ymin ymax
*13 VALUE COLORMAP

" set rgb 25 0   0   170 "
" set rgb 26 0   0   255 "
" set rgb 27 0   85  255 "
" set rgb 28 0   170 255 "
" set rgb 29 0   255 255 "
" set rgb 30 85  255 170 "
" set rgb 31 170 255 85 "
" set rgb 32 255 255 0 "
" set rgb 33 255 170 0 "
" set rgb 34 255 85  0 "
" set rgb 35 255 0   0 "
" set rgb 36 170 0   0 "

" set vpage 0 11.0 0 8.5 "
" set parea 1 10.0 1 7.5 "

" set lev 850 "
" set gxout shaded "
" set grads off "
" set xlab off "
" set ylab off "
" set ccols  0 25 26 27 28 29 30 31 32 33 34 35 36 9 14 15 0"
" set clevs  0 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75"

" d mag(UGRDprs,VGRDprs) "
" set string 1 r 6 0 "
*default setting
" xcbar 10.0 10.5 0.5 8.0 -dir v -fskip 2 "

" set gxout stream "
" set strmden -8 1 0.1 2 "
" set cthick  9 "
" set ccolor 1 "
" d UGRDprs;VGRDprs "

" set lev 500 "
" set strmden -8 1 0.1 2 "
" set cthick  9 "
" set ccolor 15 "
" d UGRDprs;VGRDprs "

" set string 1 c 6 0 "

" set lev 850"
" define vtmax=max(max(mag(UGRDprs,VGRDprs),x="xmin', x='xmax'),y='ymin', y='ymax')'
" q defval vtmax 1 1 "
VMAX=subwrd(result,3) 
VMAXnow=math_format('%6.2f',VMAX)
say VMAXnow

"define wind=mag(UGRDprs,VGRDprs)"
"set warn off"
"d maxloc(maxloc(wind,y="ymin",y="ymax"),x="xmin",x="xmax"))"
rmw1=subwrd(result,4)
say  rmw1

"d maxloc(maxloc(wind,x="xmin",x="xmax"),y="ymin",y="ymax"))"
rmw2=subwrd(result,4)
say  rmw2
"q gr2xy "rmw2" "rmw1
xloc=subwrd(result,3)
yloc=subwrd(result,6)
say "xloc "xloc" yloc "yloc

"q ll2xy "minlat" "minlon
xminloc=subwrd(result,3)
yminloc=subwrd(result,6)
say xminloc yminloc

*uncomment to mark location of Vmax
*"set string 1"
*"draw mark 6  "xloc" "yloc" .1"
clocx=5.5
*use for scaled lat/lon projection (not recommended) 
*distx=0.0162
*else use
distx=0.0117
clocy=4.25
disty=0.0117

xdist=clocx - xloc
xdist=xdist/distx
xdist2=math_pow(xdist,2)
ydist=clocy - yloc
ydist=ydist/disty
ydist2=math_pow(ydist,2)
dist=math_sqrt(xdist2+ydist2)
rmwnow=math_format('%6.2f',dist)
say "rmw "rmwnow

*based on 6.5 inches = 555.55 km = 299.976 nm or 85.4692 km/inch
*100km = 1.17 inches  
*based on 9.0 inches = 555.55 km 100km = 1.62 inchec
*centrx =  5.5 centery = 4.25
*use for scaled lat/lon projection
*distx=1.62
distx=1.17
disty=1.17
distx2=clocx - distx
distx1=distx2 - distx
distx3=clocx + distx
distx4=distx3 + distx
disty2=clocy - disty
disty1=disty2 - disty
disty3=clocy + disty
disty4=disty3 + disty

" set string 1 c 6 0 "
" draw string "clocx" 8.2 "storm
" draw string "clocx" 7.95 " fcsthr' Forecast Valid 'tvalid ' UTC 'datevalid
" draw string "clocx" 7.7 WS (m/s) at 2.0 km; Streamlines at 2.0, 5.0 km "
" set string 2 l 6 0 "
" draw string 2.65 7.00  2-km Max VT (RMW): "
" draw string 2.65 6.75 " VMAXnow ' m/s (' rmwnow ' km )'

" set string 1 l 6 0 "
" draw string 2.65 2.0 Mean Center Fix"
" draw string 2.65 1.75 " clat", "clon
" set string 1 c 6 0 "
" draw string "clocx" 0.5  East-West distance (km) "
" draw string "clocx" 0.85 0 "
" draw string "distx2" 0.85 -100 "
" draw string "distx1" 0.85 -200 "
" draw string "distx3" 0.85 100 "
" draw string "distx4" 0.85 200 "
" set string 1 c 6 90 "
" draw string 0.5 "clocy" North-South distance (km) "
" set string 1 r 6 0 "
" draw string 1.5 "disty1" -200 "
" draw string 1.5 "disty2" -100 "
" draw string 1.5 "clocy" 0 "
" draw string 1.5 "disty3" 100 "
" draw string 1.5 "disty4" 200 "
" set string 2 bl 1 0 "
" draw string 0.5 0.5 Experimental Product "
" printim "figname' gif x1100 y850'

"close 1"
" quit "
