#!/bin/ksh --login
#PBS -o /home/${USER}/HWRF_interp/run_interp.out
#PBS -j oe
#PBS -N interp
#PBS -d .
#PBS -A hur-aoml
#PBS -l walltime=01:00:00
#PBS -l procs=1
#PBS -l partition=tjet:ujet

export PS4=' + Line $LINENO: '

MODEL=$1
rundir=$2
atcfdir=$3
ext=$4
basinlist=$5
yearlist=$6
isid=$7
if [ -z "$isid" ]; then
	isid="??"
fi

# USER SETTINGS ----------------------------#
#basinlist='al ep'                #
#yearlist='2017'    #
LateModel=${MODEL}                  #
# ------------------------------------------#


case ${LateModel} in
  EMXX)   EarlyModel=EMXI ;
          hr12Interp=EMX2 ;;
  AVNO)   EarlyModel=AVNI ;
          hr12Interp=AVN2 ;;
  H216)   EarlyModel=H16I ;
          hr12Interp=H162 ;;
  NMMB)   EarlyModel=NMMI ;
          hr12Interp=NMM2 ;;
  GEFP)   EarlyModel=GEPI ;
          hr12Interp=GEP2 ;;
  GRAP)   EarlyModel=GRAI ;
          hr12Interp=GRA2 ;;
  HB16)   EarlyModel=HB6I ;
          hr12Interp=HB62 ;;
  HB15)   EarlyModel=HB5I ;
          hr12Interp=HB52 ;;
  HB17)   EarlyModel=HB7I ;
          hr12Interp=HB72 ;;
  HB18)   EarlyModel=HB8I ;
          hr12Interp=HB82 ;;
  HB19)   EarlyModel=HB9I ;
          hr12Interp=HB92 ;;
  HWRF)   EarlyModel=HWFI ;
          hr12Interp=HWF2 ;;
  HWRF)   EarlyModel=HWFI ;
          hr12Interp=HWF2 ;;
  HAFS)   EarlyModel=HAFI ;
          hr12Interp=HAF2 ;;
  HAFA)   EarlyModel=HFAI ;
          hr12Interp=HFA2 ;;
  HAFB)   EarlyModel=HFBI ;
          hr12Interp=HFB2 ;;
  HNVI)   EarlyModel=HVII ;
          hr12Interp=HVI2 ;;
     *)   echo "Input model (${LateModel}) not as expected. Exiting..." ;
          exit 1 ;;
esac

#rundir=/lfs3/projects/hwrfv3/Lin.L.Zhu/programs/HWRF_interp
#rundir=${GPLOT_DIR}/HWRF_interp
#atcfdir=/misc/whome/Ghassan.Alaka/adeck/test
intx=${rundir}/nhc_intrfcst.x
namelist=${rundir}/namelist.input
intoffset=${rundir}/interp_offset.dat

cd ${atcfdir}/

for year in ${yearlist}
do

  for basin in ${basinlist}
  do

    for file in `grep ${LateModel} a${basin}${isid}${year}${ext}.dat | awk -F, '{print $2}' | awk -F: '{print $1}' | sort -u`
    do

      echo "Working on file $file..."
#      sid=`echo $file | cut -c1-9`
      sid=`echo $basin`
      sid=${sid}`echo $file`
      sid=${sid}`echo $year`
      echo "sid=${sid}"
#      stnum=`echo $file | cut -c4-5`
      stnum=`echo $file | cut -c1-2`
      echo "stnum=${stnum}"     
 
      cd ${rundir}/

      sed "s/stormid/${sid}/" ${namelist}.template > ${namelist}
      sed -i "s/AAAA/${LateModel}/"  ${namelist}
      sed -i "s/BBBB/${EarlyModel}/" ${namelist}
      sed -i "s/CCCC/${hr12Interp}/" ${namelist}
      sed -i "s#PATH#${atcfdir}#" ${namelist}
      sed -i "s/EXT/${ext}/" ${namelist}

      sed "s/AAAA/${LateModel}/" ${intoffset}.template > ${intoffset}
      sed -i "s/BBBB/${EarlyModel}/" ${intoffset}
      sed -i "s/CCCC/${hr12Interp}/" ${intoffset}

      $intx

      cd ${atcfdir}/
      cat a${basin}${stnum}${year}.interp | sort -u > temp.dat.interp
      mv temp.dat.interp a${basin}${stnum}${year}.${LateModel}.interp 

    done

    cd ${atcfdir}/

  done

done


