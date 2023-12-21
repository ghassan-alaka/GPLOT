function get_grib2fields(args)

*GRADS script to extract subset of variables from Grib2 UPP ouput

ctlfile=subwrd(args,1) 
ctlfileout=subwrd(args,2) 
datfileout=subwrd(args,3)
dorefl=subwrd(args,4)

"open "ctlfile
*get grid information
"q ctlinfo"
xdef=sublin(result,4)
ydef=sublin(result,5)
zdef=sublin(result,6)

xi=subwrd(xdef,4)
xpts=subwrd(xdef,2)
dx=subwrd(xdef,5)

yi=subwrd(ydef,4)
ypts=subwrd(ydef,2)
dy=subwrd(ydef,5)
zpts=subwrd(zdef,2)

x=1
count=6
while(x=1)
   check=sublin(result,count)
   if(subwrd(check,1)='tdef')
      x=0
      tdef=sublin(result,count)
   endif
   count=count+1
endwhile

tpts=subwrd(tdef,2)
dt=subwrd(tdef,5)

"q dims"
timeline=sublin(result,5)
ti=subwrd(timeline,6)

"set fwrite "datfileout
"set gxout fwrite"
"set x 1 "xpts
"set y 1 "ypts

if(dorefl=1)
nvars3d=8
else
nvars3d=7
endif

znow=1
plevs=''

while(znow<=zpts)
   "set z "znow
   "q dims"
   level=sublin(result,4)
   lev=subwrd(level,6)
   plevs=plevs''lev' '
   "d UGRDprs"
   znow=znow+1
endwhile

znow=1

while(znow<=zpts)
   "set z "znow
   "d VGRDprs"
   znow=znow+1
endwhile

znow=1
while(znow<=zpts)
   "set z "znow
   "d VVELprs"
   znow=znow+1
endwhile
znow=1

while(znow<=zpts)
   "set z "znow
   "d ABSVprs"
   znow=znow+1
endwhile
znow=1

while(znow<=zpts)
   "set z "znow
   "d HGTprs"
   znow=znow+1
endwhile
znow=1

if(dorefl=1)
while(znow<=zpts)
   "set z "znow
   "d REFDprs"
   znow=znow+1
endwhile
endif

znow=1

while(znow<=zpts)
   "set z "znow
   "d SPFHprs"
   znow=znow+1
endwhile
znow=1

while(znow<=zpts)
   "set z "znow
   "d TMPprs"
   znow=znow+1
endwhile

if(dorefl=1)
nvars2d=9
else
nvars2d=8
endif

"set z "1
"d UGRD10m"
"d VGRD10m"
"d SPFH2m"
"d TMP2m"
if(dorefl=1)
"d UFLXsfc"
"d VFLXsfc"
else
"d MFLXsfc"
endif
"d SHTFLsfc"
"d LHTFLsfc"
"d PRESsfc"

nvarst=nvars3d+nvars2d

"disable fwrite"

  write (ctlfileout,'dset   ^'datfileout )
  write (ctlfileout,'undef 9.999E+20 ')
  write (ctlfileout,'title Subset from Grib file')
  write (ctlfileout,'xdef 'xpts' linear   'xi'  'dx)
  write (ctlfileout,'ydef 'ypts' linear   'yi'  'dy)
  write (ctlfileout,'tdef 'tpts' linear   'ti'  'dt   )
  write (ctlfileout,'zdef 'zpts' levels  'plevs   )
  write (ctlfileout,'vars 'nvarst )
  write (ctlfileout,' U     'zpts'   99     Zonal wind speed')
  write (ctlfileout,' V     'zpts'   99     Meridional wind speed')
  write (ctlfileout,' W     'zpts'   99     Vertical wind speed')
  write (ctlfileout,' VORT  'zpts'   99     Abs-Vorticity')
  write (ctlfileout,' GHGT  'zpts'   99     Geopotential height')
if(dorefl=1)
  write (ctlfileout,' REFL  'zpts'   99     Radar Reflectivity')
endif
  write (ctlfileout,' Q     'zpts'   99     Specific Humidity')
  write (ctlfileout,' T     'zpts'   99     Temperature')
  write (ctlfileout,' U10    0   99     10 meter Zonal wind ')
  write (ctlfileout,' V10    0   99     10 meter Meridional wind')
  write (ctlfileout,' Q2     0   99     2-meter specific humidity')
  write (ctlfileout,' T2     0   99     2-meter temperature')
if(dorefl=1)
  write (ctlfileout,' UMFLX   0   99     u momentum flux')
  write (ctlfileout,' VMFLX   0   99     v momentum flux')
else
  write (ctlfileout,' MFLX   0   99    momentum flux')
endif
  write (ctlfileout,' SFLX   0   99     sensible heat flux')
  write (ctlfileout,' LFLX   0   99     latent heat flux')
  write (ctlfileout,' MSLP   0   99     sea level pressure')
  write (ctlfileout,'endvars')

  write('gridinfo.txt',xpts' 'xi' 'dx)
  write('gridinfo.txt',ypts' 'yi' 'dy)
  write('gridinfo.txt',zpts)

"close 1"
"quit"

