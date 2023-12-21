function hwrfx_graphics(args)

storm = subwrd(args,1)
date = subwrd(args,2)
fcsthr = subwrd(args,3)
tvalid = subwrd(args,4)
datevalid=subwrd(args,5)
ctlfile = subwrd(args,6)

'reinit'
"open "ctlfile

figname=storm'.'date'.'fcsthr'hr.PBLH_thermo.gif'
'set display color white'
'clear'
'set parea 1 10 1 7.5'
'set vpage 0 11.0 0 8.5'
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
'define vave=ave(vv,x=1,x='numx')'
'define pnow=1000/lev'
'define powpnow=pow(pnow,0.2856)'
'define theta=tt*powpnow'
'undefine pnow'
'undefine powpnow'
'define tv=theta*(1.0+(0.608*qq))'
'undefine theta'
'define tvave=ave(tv,x=1,x='numx')'
'set lev 1000'
'define tv1=ave(tv,x=1,x='numx')'
'set lev 975'
'define tv2=ave(tv,x=1,x='numx')'
'set lev 950'
'define tv3=ave(tv,x=1,x='numx')'
'set lev 925'
'define tv4=ave(tv,x=1,x='numx')'
'define tvlow=0.25*(tv1 + tv2 + tv3+tv4)'
'undefine tv1'
'undefine tv2'
'undefine tv3'
'undefine tv4'

'set lev 1000 700'
'set lon 90'
'set xlab off'
'set ylab on'
'set gxout shaded'
'set grads off'
'd tvave - tvlow'
'xcbar 10.1 10.6 1.0 7.5 -dir v'

'set gxout contour'
'set clevs .5'
'set ccolor 15'
'set cthick 10'
'd tvave - tvlow'
'set clevs 3.0'
'set ccolor 1'
'set cthick 10'
'set cstyle 2'
'd cdiff(tvave,z)'

rmwx=rmw*0.080357
rloc1=rmwx+1
rloc2=rloc1+rmwx
rloc3=rloc2+rmwx
rloc4=rloc3+rmwx
'set line 1 1 8'
'set string 1 bl 3 0'
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
'draw string 5.5 .5 R/R*'


'draw string 7.5 0.25 Initial date: 'date
'set string 2 tl 1 0'
'draw string 0.05 8.45 Experimental Product'
'set strsiz 0.2 0.2'
'set string 1 tc 3'
'draw string 5.5 8.2 Theta-v Anomaly (K) '
"draw string 5.5 7.9 "storm" "fcsthr"hr Forecast "
'set strsiz 0.1 0.1'
'set string 0 bl 7 0'
'set string 15 bl 7 0'
'draw string 8.0 3.0 Tv-Tvlow = 0.5K '
'set string 1 bl 7 0'
'draw string 8.0 2.75 dTv/dz = 3K'

'close 1'

'printim 'figname' gif x1100 y850'
'quit'

