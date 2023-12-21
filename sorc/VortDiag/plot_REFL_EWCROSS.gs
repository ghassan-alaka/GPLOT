function jetcolors(args)

*Script to plot radar reflectivity NS and EW cross-section 

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

figname = storm'.'date'.'fcsthr'hr.EWcross.gif'

" open " ctlfile
" set display color white "
" clear "
" set lat "clat 

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

" set lev 1000 100"
" set gxout shaded "
" set grads off "
" set ccols  0 25 26 27 28 29 31 32 33 34 35 36 0"
" set clevs  -5 0 5 10 15 20 25 30 35 40 45 50 "
" d REFDprs "
" set string 1 r 6 0 "
" xcbar 10.1 10.6 0.45 8.15 -dir v -fskip 2 -foffset 1"
" set gxout contour"
" set clab masked"
" d VGRDprs"

clocx=5.5
" set string 1 c 6 0 "
" draw string "clocx" 8.2 "storm
" draw string "clocx" 7.95 " fcsthr' Forecast Valid 'tvalid ' UTC 'datevalid
" draw string "clocx" 7.7 Reflectivity (dBZ) and Meridional Wind Speed (m/s) at "clat"N"
" set string 1 l 6 0 "
" draw string 0.5 0.5 Experimental Product "
" set line 15 3 8"
" draw line "clocx" 1.0 "clocx" 7.5"
" printim "figname' gif x1100 y850'
" close 1 "

*" quit "
