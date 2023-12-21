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
tstring3='SSMIS 91GHZ Color Composite'
*37GH V 
'set lev 11707'
*operational HWRF
*'vpol37=BRTMPhbl'
*basin scale
'vpol91=BRTMPhlev'

*37GH H
'set lev 11706'
*operational HWRF
*'hpol37=BRTMPhbl'
'hpol91=BRTMPhlev'

'set display color white'
'clear'

"set gxout grfill"
'pct91 = (2.18 * (vpol91)) - (1.18 * (hpol91))'
*'set mproj scaled'
'set mpdset hires'
"set vpage 0 11 0 8.5"
"set parea 1 10 1 7.5"
"set grads off"
"set xlint 1"
"set ylint 1"
"set grid on 5 1"
'run 91pctred.gs'
'set gxout grfill'
'd pct91'
'set string 1 c 6 0'
'draw string 5.5 8.2 'tstring1
'draw string 5.5 7.9 'tstring2
'draw string 5.5 7.7 'tstring3
'set string 2 bl 1 0'
'draw string 0.5 0.5 Experimental Product'
"printim 91pctred.bmp"
'clear'

"set vpage 0 11 0 8.5"
"set parea 1 10 1 7.5"
"set grads off"
"set xlint 1"
"set ylint 1"
"set grid on 5 1"
'set gxout grfill'
'run 91hpolblue.gs'
'd hpol91'
'set string 1 c 6 0'
'draw string 5.5 8.2 'tstring1
'draw string 5.5 7.9 'tstring2
'draw string 5.5 7.7 'tstring3
'set string 2 bl 1 0'
'draw string 0.5 0.5 Experimental Product'
'printim 91hpolblue.bmp'
'clear'

"set vpage 0 11 0 8.5"
"set parea 1 10 1 7.5"
"set grads off"
"set xlint 1"
"set ylint 1"
"set grid on 5 1"
'set gxout grfill'
"run 91vpolgreen.gs"
"d vpol91"
'set string 1 c 6 0'
'draw string 5.5 8.2 'tstring1
'draw string 5.5 7.9 'tstring2
'draw string 5.5 7.7 'tstring3
'set string 2 bl 1 0'
'draw string 0.5 0.5 Experimental Product'
"printim 91vpolgreen.bmp"
'clear'

'close 1'
'quit'

return

