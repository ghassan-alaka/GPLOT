function jetcolors(args)

*Script to plot radar reflectivity at lowest P-3 TDR level (2km)
*Wind barbs in knots
*Color scale is for direct comparison with plots produced for P-3 TDR
*Center location is used to define a 5degree grid

*ARGUMENT EXAMPLE
*storm name: matthew
*storm num:  14l
*date: 2016100512 (forecast initial time)
*fcsthr: 030 (must be 3 digit)
*tvalid: 18  (forecast valid time)
*datevalid 20161006 (forecast valid date)
*center lat  (forecast time)
*center lon  (forecast time)
*control file name

storm = subwrd(args,1)
date = subwrd(args,2)
fcsthr = subwrd(args,3)
tvalid = subwrd(args,4)
datevalid = subwrd(args,5)
clat = subwrd(args,6)
clon = subwrd(args,7)
ctlfile = subwrd(args,8)

*Currently 2km level is estimated to be 850mb
*Option to change here for stronger/weaker storms
*Output is at 25hPa increments

lev2km=850

figname = storm'.'date'.'fcsthr'hr.WINDREFL_2KM.gif'

" open " ctlfile
" set display color white "
" clear "

latr=0.01745*clat
latdiff=math_cos(latr)
latdiff=1/latdiff
say latdiff
londiff=latdiff*2.5

minlat=clat-2.5
maxlat=clat+2.5
*scale so xdist = ydist = 555.55km
minlon=clon-londiff
maxlon=clon+londiff
minlon=minlon+360
maxlon=maxlon+360
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

say xmin 
say xmax 
say ymin 
say ymax
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

" set vpage 0 11 0 8.5 "
" set parea 1 10.0 1 7.5 "

" set lev "lev2km
" set gxout shaded "
" set grads off "
" set xlab off "
" set ylab off "
" set ccols  0 25 26 27 28 29 31 32 33 34 35 36 0"
" set clevs  -5 0 5 10 15 20 25 30 35 40 45 50 "
" d REFDprs "
" set string 1 r 6 0 "
" xcbar 10.0 10.5 0.5 8.0 -dir v -fskip 2 -foffset 1"

" define ukts=UGRDprs*1.94384 "
" define vkts=VGRDprs*1.94384 "
" set gxout barb "
" set cthick 6 " 
" set ccolor 1 "
" set arrowhead .025 "
" set arrscl 0.1 "
" d ukts;skip(vkts,25) "

*based on 6.5 inches = 555.55 km = 299.976 nm or 85.4692 km/inch
*100km = 1.17 inches
*based on 9.0 inches = 555.55 km 100km = 1.62 inches
*centrx =  5.5 centery = 4.25
clocx=5.5
clocy=4.25
*if using mproj scaled
*distx=1.62
*else
distx=1.17
disty=1.17
disty2=clocy - disty
disty1=disty2 - disty
disty3=clocy + disty
disty4=disty3 + disty
distx2=clocx - distx
distx1=distx2 - distx
distx3=clocx + distx
distx4=distx3 + distx

" set string 1 c 6 0 "
" draw string "clocx" 8.2 "storm
" draw string "clocx" 7.95 " fcsthr' Forecast Valid 'tvalid ' UTC 'datevalid
" draw string "clocx" 7.7 Reflectivity (dBZ) at 2.0 km "
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
" draw string  0.5 "clocy" North-South distance (km) "
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
