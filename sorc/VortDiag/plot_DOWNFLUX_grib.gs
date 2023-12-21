function thetaeflux(args)

*Downward flux of Theta-e for diagnosing boundary layer recovery
*Cuts out a 555km x 555km domain centered on the storm

*ARGUMENT EXAMPLE
*storm name: matthew11l 
*date: 2016100512 (forecast initial time)
*fcsthr: 030 (must be 3 digit)
*tvalid: 18  (forecast valid time)
*datevalid 20161006 (forecast valid date)
*clat center latitude
*clon center longitude
*ctlfile grads control file name

storm = subwrd(args,1)
date = subwrd(args,2)
fcsthr = subwrd(args,3)
tvalid = subwrd(args,4)
datevalid = subwrd(args,5)
clat = subwrd(args,6)
clon = subwrd(args,7)
ctlfile = subwrd(args,8)

*1.5 kilometer level estimated as 850mb may need to change for strongest storm
*data available in increments of 25mb

figname = storm'.'date'.'fcsthr'hr.THETAE_FLUX.gif'
"open "ctlfile
"set display color white"
"clear"

latr=0.01745*clat
latdiff=math_cos(latr)
latdiff=1/latdiff
say latdiff
londiff=latdiff*2.5

minlat=clat-2.5
maxlat=clat+2.5
minlon=clon-londiff
maxlon=clon+londiff
*western hemisphere only
*GRADS lon defined from 0-360
minlon=minlon+360
maxlon=maxlon+360
say 'plot range'
say minlat' 'maxlat' 'minlon' 'maxlon
"q dims"
xinfo=sublin(result,2)
yinfo=sublin(result,3)
x1=subwrd(xinfo,11)
x2=subwrd(xinfo,13)
y1=subwrd(yinfo,11)
y2=subwrd(yinfo,13)
say x1' 'x2' 'y1' 'y2
levnow=850

"define tnow=TMPprs*1.1765"
"define theta=pow(tnow,0.2856)"
"define thetae=theta*3*SPFHprs"
"define thetaeave=ave(ave(thetae,x="x1",x="x2"),y="y1",y="y2")"
"define thetaeanom=thetae-thetaeave"
"undefine thetaeave"
"define w=VVELprs"
"define wave=ave(ave(w,x="x1",x="x2"),y="y1",y="y2")"
"define wanom=w-wave"
"define flux=wanom*thetaeanom"
" set warn off"
"d max(max(flux,x="x1",x="x2"),y="y1",y="y2")"
fluxmax=subwrd(result,4)
say fluxmax
toppct=fluxmax*0.75

" set lat " minlat' 'maxlat
" set lon " minlon' 'maxlon
"set lev "levnow
"set vpage 0 11.0 0 8.5"
"set parea 1 10.0 1 7.5"
clocx=5.5
clocy=4.25
"set gxout shaded"
"set grads off"
"set mpdset hires"
"d thetaeanom"
" xcbar 9.8 10.3 1.0 7.5 -dir v -fskip 2 "

"set gxout contour"
"set ccolor 1"
"set cthick 6"
"set clab masked"
*"set clevs -2 -1 -.5 -.25 -.1 .1 .25 .5 1 2"
*"d w"
"d wanom"
*"d flux"
*top % downward flux of low theta-e
*"set clevs "toppct
*"d flux"

" set string 1 c 6 0 "
" draw string "clocx" 8.2 "storm
" draw string "clocx" 7.95 " fcsthr' Forecast Valid 'tvalid ' UTC 'datevalid
" draw string "clocx" 7.7 Downward Theta-e Flux [ thetae'W'] "
" set string 2 bl 1 0" ; "draw string 0.05 0.5 Experimental Product"
" set string 1 br 1 0" ; "draw string 10.5 0.5 Max flux="fluxmax
" printim "figname" gif x1100 y850"
"close 1"
"quit"
