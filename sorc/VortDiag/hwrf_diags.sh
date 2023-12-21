#!/bin/sh --login
#
#PBS -d .
#PBS -l partition=vjet
#PBS -l procs=1
#PBS -l vmem=8G

module load intel
module load szip
module load netcdf
module load hdf5
module load netcdf4
module load wgrib
module load wgrib2

#Script to make grads control file for UPP output and create cylindrical coordinate output
#extract_gribfiles.gs is a GRADS script which creates a subset of the data for the Fortran program to read in
#get_cyldata.f90 outputs HWRF variables in cylindrical coordinates

cd ${WORK_DIR}

fcstinc=3             #forecast time increment in hours
dorefl=1              #1 if reflectivity is in grib file/ 0 if not
ishedas=0             #1 if output from HEDAS configuration of HWRF/ 0 if not
isnest=1              #1 if inner nest 0 if outer d01
doshear=1             #1 if plotting shear vectors is requested
domotion=1            #1 if plotting storm motion vectors is requested
dorzmean=1            #1 if RZ mean plots requested
dopct=1               #1 if Satellite PCT plots requested
savecyl=1             #1 if saving cylindrical data is requested
rcoord=2              #1 if R/R* radial coordinate 2 if Radius in km

echo "HELLO"

#COMPUTE FORECAST VERIFICATION TIME
yy=`echo $initdate | cut -c1-4`
mo=`echo $initdate | cut -c5-6`
dd=`echo $initdate | cut -c7-8`
hh=`echo $initdate | cut -c9-10`
mn=0

fhr=`printf %03d $fcsthr`
fstart=`echo ${stormname}${stormnum} | tr [a-z] [A-Z]`
casename=${stormname}${stormnum}

ntime=`expr $fcsthr / $fcstinc`
addtime=`expr $fcsthr \* 60`
vtime=`./advance.sh $yy $mo $dd $hh $mn $addtime`
datevalid=`echo $vtime | cut -c1-8`
tvalid=`echo $vtime | cut -c9-12`

#Naming convention may vary between versions of HWRF 
#below is based on sample files hermine09l.2016090218.hwrfprs.d3.0p03.f048.grb2
#Be sure that grib file name is correct
#gribfiled02=${stormname}${stormnum}.${initdate}.hwrfprs.d3.0p03.f${fhr}.grb2
#gribfilesat=${stormname}${stormnum}.${initdate}.hwrfsat.d3.0p03.f${fhr}.grb2
#gribfiled01=${stormname}${stormnum}.${initdate}.hwrfprs.d1.0p25.f${fhr}.grb2
#alternative naming convention used in basin scale HWRF
gribfiled02=${stormname}${stormnum}.${initdate}.hwrfprs.storm.0p02.f${fhr}.grb2
gribfilesat=${stormname}${stormnum}.${initdate}.hwrfsat.storm.0p02.f${fhr}.grb2
gribfiled01=${stormname}${stormnum}.${initdate}.hwrfprs.synoptic.0p125.f${fhr}.grb2

ctlfiled01=${gribfiled01}.ctl
idxfiled01=${gribfiled01}.idx
ctlfiled02=${gribfiled02}.ctl
idxfiled02=${gribfiled02}.idx
ctlfilesat=${gribfilesat}.ctl
idxfilesat=${gribfilesat}.idx
statfile=${stormname}${stormnum}.${initdate}.stats.short


#you need d01 data to calculate shear
if [ $doshear == 1 ]
then
   cp ${GRIB_DIR}/${initdate}/${stormnum^^}/$gribfiled01 .
fi

cp ${GRIB_DIR}/${initdate}/${stormnum^^}/$gribfiled02 .

cp ${GRIB_DIR}/${initdate}/${stormnum^^}/$gribfilesat .

cp ${GRIB_DIR}/${initdate}/${stormnum^^}/$statfile .

#OPTIONAL
#obtain storm center from tcvitals 
#do not use in realtime
#obdir=/pan2/projects/hfip-hda/OBSDATA/${yyyy}/public/data/nhc/tcvitals
#tcvfile=${fstart}.${datevalid}${tvalid}00
#tcv_in=${obdir}/${tcvfile}
#
#linenum=0

#while read line
#do
#   if [ -n $line ]
#   then
#       linenum=`expr $linenum + 1`
#   fi
#
#   if [ $linenum == 1 ]
#   then
#      tcvlat=`echo $line`
#   fi
#
#   if [ $linenum == 2 ]
#   then
#      tcvlon=`echo $line`
#   fi
#
#done < $tcv_in
#
#echo $tcvlat $tcvlon  

#storm center from .stats file
   if [ $fhr != 000 ]
   then
      ftime=`echo $fcsthr | sed 's/^0*//'`
   if [ $ftime -lt 12 ]
   then
      prevtime=`expr $ftime - 1`
   else
      prevtime=`expr $ftime - $fcstinc`
   fi
   domotion=1
else
   ftime=0
   prevtime=0
   domotion=0
fi

ftime=${ftime}.0
prevtime=${prevtime}.0
echo 'ftime prevtime' $ftime $prevtime

if [ -f $statfile ]
then
   while read line
   do 
      timenow=`echo $line | cut -c6-10`
      if [ $timenow == $ftime ]
      then
         clat=`echo $line | cut -d':' -f4-4 | cut -d' ' -f2-2 `
         clon=`echo $line | cut -d':' -f3-3 | cut -d' ' -f2-2`
         echo $clat $prevlat > stormlocs.dat
         echo $clon $prevlon >> stormlocs.dat
      fi
      prevlat=`echo $line | cut -d':' -f4-4 | cut -d' ' -f2-2 `
      prevlon=`echo $line | cut -d':' -f3-3 | cut -d' ' -f2-2`
   done < $statfile
else
   domotion=0
fi

#produce control file for use with Grads
echo 'making grads control files'

${GASCRP}/g2ctl $gribfiled02 > $ctlfiled02
${GASCRP}/gribmap -i $ctlfiled02

${GASCRP}/g2ctl $gribfilesat > $ctlfilesat
${GASCRP}/gribmap -i $ctlfilesat
   
${GASCRP}/g2ctl $gribfiled01 > $ctlfiled01
${GASCRP}/gribmap -i $ctlfiled01

#MAKE PLOTS FROM GRIB FILES
#NHC Plots (units of nautical miles and knots)
#${GASCRP}/grads -blc "run plot_radarwind_grib_500m.gs $stormname $stormnum $initdate $fhr $tvalid $datevalid $clat $clon $ctlfile"
#${GASCRP}/grads -blc "run plot_radarwind_grib_3km.gs $stormname $stormnum $initdate $fhr $tvalid $datevalid $clat $clon $ctlfile"
#Radar reflectivity and wind barbs at 800mb  ~ 2km
echo 'plot reflectivity and wind'
${GASCRP}/grads -blc "run plot_WIND_REFL_2KM_grib.gs  $casename $initdate $fhr $tvalid $datevalid $clat $clon $ctlfiled02"
#Radar wind at 2km with wind streamlines at 5km to diagnose tilt
echo 'plot 2 level wind'
${GASCRP}/grads -blc "run  plot_WIND_STREAM_2_5KM_grib.gs $casename $initdate $fhr $tvalid $datevalid $clat $clon $ctlfiled02" 
#10m wind speed with RMW and maximum wind speed
echo 'plot 10 meter wind'
${GASCRP}/grads -blc "run plot_10MWIND_grib.gs  $casename $initdate $fhr $tvalid $datevalid $clat $clon $ctlfiled02"
#2km wind speed in same color scheme as AOML TDR plots  
echo 'plot 2km wind'
${GASCRP}/grads -blc "run plot_2KMWIND_grib.gs $casename $initdate $fhr $tvalid $datevalid $clat $clon $ctlfiled02"
#Convective bursts relative to shear and storm motion
echo 'plot convective bursts'
${GASCRP}/grads -blc "run plot_CBS_grib.gs $casename $initdate $fhr $tvalid $datevalid $clat $clon $ctlfiled02 $doshear $domotion $ctlfiled01"
#Downward flux of theta-e for diagnosing boundary layer recovery
echo 'plot downward thetae flux'
${GASCRP}/grads -blc "run plot_DOWNFLUX_grib.gs $casename $initdate $fhr $tvalid $datevalid $clat $clon $ctlfiled02"
##Cross sections of radar reflectivity
echo 'plot cross section'
${GASCRP}/grads -blc "run plot_REFL_CROSS_grib.gs $casename $initdate $fhr $tvalid $datevalid $clat $clon $ctlfiled02"
#SkewT Log P diagrams
#Need to fix UPP output so that dewpoint is on same levels as temperature
#${GASCRP}/grads -blc "run plot_SKEWT_grib.gs $fhr $casename $initdate $ctlfiled02 $clat $clon

#Extract a subset of the grib data for input into Fortran code

if [ $dorzmean -eq 1 ]
then
echo 'computing RZ mean'
   if [ $dorefl=="1" ]
   then
      ${GASCRP}/grads -blc "run extract_gribfields.gs "$ctlfiled02" subset.ctl subset.dat 1"
   else
      ${GASCRP}/grads -blc "run extract_gribfields.gs "$ctlfiled02" subset.ctl subset.dat 0"
   fi

   #check for output from grads script before continuing
   waittime=0
   wait_each=5
   wait_total=30
   done=0

   while [ $done -eq 0 ]
   do
      test -e ./subset.dat
      if [ $? -ne 0 ]
      then
         sleep $wait_each
         echo 'waiting for grads data file'
         done=0
      else 
         test -e ./subset.ctl
         if [ $? -ne 0 ]
         then
            sleep $wait_each
            echo 'waiting for grads ctl file'
            done=0
         else
           echo 'GRADS produced data subset ... continuing with RZ mean plots'
           done=1
         fi
      fi
      waittime=`expr $waittime + $wait_each`
      if [ $waittime -gt $wait_total ]
      then
         echo 'GRADS did not produce data subset ... unable to produce RZ mean plots'
         dorzmean=0
         done=1
         exit
      fi
   done
fi

if [ $dorzmean -eq 1 ]
then
   if [ $ishedas -eq 1 ]
   then
      ./get_cyldata.x 1
   else
   ./get_cyldata.x 0
   fi
else
   exit
fi

#check for output from Fortran program before continuing
waittime=0
wait_each=5
wait_total=120
done=0

while [ $done -eq 0 ]
do
   test -e ./grib2cyl.dat
   if [ $? -ne 0 ]
   then
      sleep $wait_each
      done=0
   else
      test -e ./grib2cyl.ctl
      if [ $? -ne 0 ]
      then
         sleep $wait_each
         done=0
      else
        done=1
      fi
   fi
   waittime=`expr $waittime + $wait_each`
   if [ $waittime -gt $wait_total ]
   then
      echo 'No output from Fortran code ... unable to produce RZ mean plots'
      dorzmean=0
      done=1
      exit
   fi
done

#now produce graphics
if [ $dorzmean -eq 1 ]
then
echo 'PBLH'
   ${GASCRP}/grads -blc "run plot_PBLH_grib.gs $casename $initdate $fcsthr $tvalid $datevalid grib2cyl.ctl"
echo 'RZWIND'
   ${GASCRP}/grads -blc "run plot_RZWIND_grib.gs $casename $initdate $fcsthr $tvalid $datevalid grib2cyl.ctl $rcoord"
echo 'THERMO'
   ${GASCRP}/grads -blc "run PBLH_THERMO_grib.gs $casename $initdate $fcsthr $tvalid $datevalid grib2cyl.ctl"
echo 'IANGLE'
   ${GASCRP}/grads -blc "run plot_IANGLE_grib.gs $casename $initdate $fcsthr $tvalid $datevalid grib2cyl.ctl"
#ADD THESE WHEN DIABATIC HEATING CAN BE EXTRACTED FROM GRIB FILES
#    ${GASCRP}/grads -blc "run plot_RZHEAT.gs $casename $initdate $fcsthr grib2cyl.ctl $rcoord"
#    ${GASCRP}/grads -blc "run plot_QUADS.gs $casename $initdate $fcsthr grib2cyl.ctl $roord"
else
   echo 'RZ mean plots not requested finish with vortex plots'
   exit
fi


if [ $dopct -eq 1 ]
then

sensor=SSMI
freq=37
filename=${stormname}${stormnum}.${initdate}.${fcsthr}hr.${sensor}_${freq}GHZ_COMPOSITE.gif
${GASCRP}/grads -blc "run plot_SSMIS_37COMPOSITE_grib.gs $casename $initdate $fcsthr $tvalid $datevalid $ctlfilesat"
composite -compose plus ${freq}pctred.bmp ${freq}vpolgreen.bmp tmp.bmp
composite -compose plus tmp.bmp ${freq}hpolblue.bmp ${freq}composite.bmp
convert ${freq}composite.bmp $filename
rm *.bmp

freq=91
filename=${stormname}${stormnum}.${initdate}.${fcsthr}hr.${sensor}_${freq}GHZ_COMPOSITE.gif
${GASCRP}/grads -blc "run plot_SSMIS_91COMPOSITE_grib.gs $casename $initdate $fcsthr $tvalid $datevalid $ctlfilesat"
composite -compose plus ${freq}pctred.bmp ${freq}vpolgreen.bmp tmp.bmp
composite -compose plus tmp.bmp ${freq}hpolblue.bmp ${freq}composite.bmp
convert ${freq}composite.bmp $filename
rm *.bmp

fi

mv *.gif ${FIGURES}/

rm $gribfiled01 $gribfiled02 $gribflesat $ctlfiled01 $ctlfiled02 $idxfiled01 $idxfiled02 $gribfilesat $ctlfilesat $idxfilesat

#rename cylindrical output
mv grib2cyl.ctl ${FIGURES}/grib2cyl_${fhr}.ctl
mv grib2cyl.dat ${FIGURES}/grib2cyl_${fhr}.dat

exit
