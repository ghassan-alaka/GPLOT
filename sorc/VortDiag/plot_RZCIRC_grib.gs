function hwrfx_graphics(args)

storm = subwrd(args,1)
date = subwrd(args,2)
fcsthr = subwrd(args,3)
tvalid=subwrd(args,4)
datevalid=subwrd(args,5)
ctlfile = subwrd(args,6)
coord = subwrd(args,7)
plotstream=subwrd(args,8)

*ARGUMENTS
*storm - storm name + number for labeling and naming
*date - initial forecast time
*fcsthr - integration time
*tvalid - forcast verification time
*datevalid - forecast verification date
*ctlfile - grads control file
*coord - (1) R/R* (2) km

figname=storm'.'date'.'fcsthr'hr.RZCIRC.gif'
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

"set warn off"
'd ave(maxloc(vt10,y=1,y='numy'),x=1,x=361))'
rmw=subwrd(result,4)
rmw=rmw - 1
say 'RMW x gridpoint ' rmw

*9 plot inches / 112 gridpoints = 0.08357 inches between x gridpoints
rmwx=rmw*0.080357
rloc1=1+rmwx
rloc2=rloc1+rmwx
rloc3=rloc2+rmwx
rloc4=rloc3+rmwx

'd max(ave(vt10,x=1,x=361),y=1,y='numy')'
vrmw=subwrd(result,4)
vrmw=math_format('%6.2f',vrmw)
rmwk=rmw*2
rmwk=rmwk-2
rmwk=math_format('%6.2f',rmwk)
say 'vmax rmw 'vrmw' m/s 'rmwk' km'

'set lev 1000 50'
'set gxout shaded'
'set grads off'
'set xlab off'
'set ylab on'
'set lon 90'
'd ave(vv,x=1,x=361)'
'xcbar 10.1 10.6 1.0 7.5 -dir v'
***********************************************
*DO NOT PLOT STEAMLINES AT T=0 NO VERTICAL WIND
if(plotstream=0)
***********************************************
*SECONDARY CIRCULATION CONTOURS
***********************************************
'set gxout contour'
'set ccolor 0'
'set cthick 8'
*UNCOMMENT TO INCLUDE CONTOUR VALUES
'set clab masked'
'set clopts 0 8 .09'
**UNCOMMENT TO HIDE CONTOUR VALUES
*'set clab off'
*'set clskip 2'
**PLOT RADIAL WIND SPEED
'set clevs -10 -8 -6 -4 -2 -1 1 2 4 6 8 10'
'd ave(uu,x=1,x=361)'
**PLOT VERTICAL WIND SPEED
if (fcsthr >= 1) 
'set ccolor 1'
'set cthick 8'
'set clab masked'
'set clopts 1 8 .09'
'set clab off'
'set clskip 2'
'set clevs -5 -4 -3 -2 -1 1 2 3 4 5'
'd ave(ww,x=1,x=361)'
endif
else
if (fcsthr >= 1)
******************************************
*SECONDARY CIRCULATION STREAMLINES
******************************************
'set gxout stream'
'set strmden 6 0.7 0.07 2'
'set ccolor 1'
'd ave(uu,x=1,x=361);ave(ww,x=1,x=361)'
endif
******************************************
*END STREAMLINES
******************************************
endif

'set line 1 1 8'
'set string 1 bl 3 0'

if (coord = 1)
*******************************************
*X LABELS R/RMW
*******************************************
   if ( rloc1 <= 10.0 )
   'draw line 'rloc1' .9 'rloc1' 1'
   'draw string 'rloc1' .75 1'
   endif
   if ( rloc2 <= 10.0 )
   'draw line 'rloc2' .9 'rloc2' 1'
   'draw string 'rloc2' .75 2'
   endif
   if ( rloc3 <= 10.0 )
   'draw line 'rloc3' .9 'rloc3' 1'
   'draw string 'rloc3' .75 3'
   endif
   if ( rloc4 <= 10.0 )
   'draw line 'rloc4' .9 'rloc4' 1'
   'draw string 'rloc4' .75 4'
   endif

   'draw string 5.5 .5 R/RMW'

else
************************************************
*X LABELS KM
************************************************
    'draw string 3.00893 0.75 50'
    'draw string 5.01786 0.75 100'
    'draw string 7.02679 .75 150'
    'draw string 9.03571 .75 200'
endif

"draw string 7.5 0.25 Initial date: "date
"set string 2 tl 1 0" ; "draw string 0.05 8.45 Experimental Product"
"set strsiz 0.2 0.2"
"set string 1 tc 3 0";"draw string 5.5 8.2 Tangential Wind Speed [m/s]"
"draw string 5.5 7.9 "storm" "fcsthr"hr Forecast "
*UNCOMMENT IF PLOTTING SECONDARY CIRCULATION
if (plotstream=0)
'set strsiz 0.1 0.1'
'set string 0 bl 7 0'
'draw string 8.0 2.0 Radial Wind Speed'
'set string 1 bl 7 0'
'draw string 8.0 1.75 Vertical Wind Speed'
endif
'set strsiz 0.1 0.1'
'set string 15 bl 3 0'
'draw string 8.0 7.25 VMAX = 'vrmw' m/s'
'draw string 8.0 7.0 RMW = 'rmwk' km'
'printim 'figname' gif x1100 y850'
'close 1'
'quit'

