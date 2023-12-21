function hwrfx_graphics(args)

storm = subwrd(args,1)
date = subwrd(args,2)
fcsthr = subwrd(args,3)
tvalid=subwrd(args,4)
datevalid=subwrd(args,5)
ctlfile = subwrd(args,6)

'reinit'
"open "ctlfile

'set display color white'
'clear'
'set vpage 0 11.0 0 8.5'
'set parea 1 10 1 7.5'
'set mproj sps'
'q dims'
xinfo=sublin(result,2)
yinfo=sublin(result,3)
xmin=subwrd(xinfo,6)
numx=subwrd(xinfo,13)
ymin=subwrd(yinfo,6)
numy=subwrd(yinfo,13)

say numx' 'numy' 'xmin' 'ymin
"set warn off"
'd ave(maxloc(vt10,y=1,y='numy'),x=1,x='numx'))'
rmw=subwrd(result,4)
say  'RMW X location 'rmw
rmw=rmw-1

'set lev 1000 700'
*'set lon 90'
'define vave=ave(vv,x=1,x='numx')'
'define uave=ave(uu,x=1,x='numx')'
'define vrmin=min(min(uave,y=1,y='numy'),z=1,z=13) * 0.1'
'q defval vrmin 1 1'
ans=sublin(result,1)
blht=subwrd(ans,3)
blht=math_format('%3.1f',blht)


*9 plot inches / 112 gridpoints = 0.08357 inches between x gridpoints
rmwx=rmw*0.080357
rloc1=rmwx+1
rloc2=rloc1+rmwx
rloc3=rloc2+rmwx
rloc4=rloc3+rmwx

'set xlab off'
'set ylab on'
'set gxout shaded'
'set lon 90'
'set grads off'
'd uave'
'xcbar 10.1 10.6 1.0 7.5 -dir v'

'set gxout contour'
'set ccolor 27'
'set cthick 12'
'set cstyle 1'
'set clevs 'blht
'set clab on'
'd uave'

'set gxout line'
'set ccolor 0'
'set cthick 12'
'set cstyle 1'
'set cmark 0'

'set vrange 1 13'
'set ylab off'
'd ave(maxloc(vv,z=1,z=13),x=1,x=361)'

'set line 1 1 8'
'set string 1 bl 3 0'
'draw string 5.5 .5 R/RMW'
'draw line 'rloc1' .9 'rloc1' 1'
'draw string 'rloc1' .75 1'
if (rloc2 <= 10.0 )
   'draw line 'rloc2' .9 'rloc2' 1'
   'draw string 'rloc2' .75 2'
endif
if (rloc3 <= 10.0)
   'draw line 'rloc3' .9 'rloc3' 1'
   'draw string 'rloc3' .75 3'
endif
if (rloc4 <= 10.0)
   'draw line 'rloc4' .9 'rloc4' 1'
   'draw string 'rloc4' .75 4'
endif

'draw string 7.5 0.25 Initial date: 'date 
'set string 2 tl 1 0'  
'draw string 0.05 8.45 Experimental Product'
'set strsiz 0.2 0.2'
'set string 1 tc 3'
'draw string 5.5 8.2 Vr [m/s] and Boundary Layer Hgt [m] '
"draw string 5.5 7.9 "storm" "fcsthr"hr Forecast "
'set strsiz 0.1 0.1'
'set string 0 bl 7 0'
'set string 27 bl 7 0'
'draw string 8.5 3.0 10% min Vr'
'set string 0 bl 7 0'
'draw string 8.5 2.75 Max Vt'

figname=storm'.'date'.'fcsthr'hr.PBLH.gif'
'printim 'figname' gif'
*for double resolution figure comment out above and uncomment here
'printim 'figname' gif x1100 y850'
'close 1'
"quit"
