function plot(args)

storm = subwrd(args,1)
date = subwrd(args,2)
fcsthr = subwrd(args,3)
tvalid = subwrd(args,4)
datevalid = subwrd(args,5)
ctlfile = subwrd(args,6)

"open "ctlfile

tstring1=storm
tstring2=fcsthr"Forecast Valid "tvalid"UTC "datevalid
tstring3='SSMIS 37GHZ Color Composite'
*37GH V 
'set lev 11705' 
*operational HWRF
*'vpol37=BRTMPhbl'
*basin scale
'vpol37=BRTMPhlev'

*37GH H
'set lev 11704'
*operational HWRF
*'hpol37=BRTMPhbl'
'hpol37=BRTMPhlev'

'set display color white'
'clear'

"set gxout grfill"
'pct37 = (2.18 * (vpol37)) - (1.18 * (hpol37))'
'set mpdset hires'
"set vpage 0 11 0 8.5"
"set parea 1 10 1 7.5"
"set grads off"
"set xlint 1"
"set ylint 1"
"set grid on 5 1"
'run 37pctred.gs'
'set gxout grfill'
'd pct37'
'set string 1 c 6 0'
'draw string 5.5 8.2 'tstring1
'draw string 5.5 7.9 'tstring2
'draw string 5.5 7.7 'tstring3
'set string 2 bl 1 0'
'draw string 0.5 0.5 Experimental Product'
"printim 37pctred.bmp"
'clear'

"set vpage 0 11 0 8.5"
"set parea 1 10 1 7.5"
"set grads off"
"set xlint 1"
"set ylint 1"
"set grid on 5 1"
'set gxout grfill'
'run 37hpolblue.gs'
'd hpol37'
'set string 1 c 6 0'
'draw string 5.5 8.2 'tstring1
'draw string 5.5 7.9 'tstring2
'draw string 5.5 7.7 'tstring3
'set string 2 bl 1 0'
'draw string 0.5 0.5 Experimental Product'
'printim 37hpolblue.bmp'
'clear'

"set vpage 0 11 0 8.5"
"set parea 1 10 1 7.5"
"set grads off"
"set xlint 1"
"set ylint 1"
"set grid on 5 1"
'set gxout grfill'
"run 37vpolgreen.gs"
"d vpol37"
'set string 1 c 6 0'
'draw string 5.5 8.2 'tstring1
'draw string 5.5 7.9 'tstring2
'draw string 5.5 7.7 'tstring3
'set string 2 bl 1 0'
'draw string 0.5 0.5 Experimental Product'
"printim 37vpolgreen.bmp"
'clear'

'close 1'
*'quit'

return

