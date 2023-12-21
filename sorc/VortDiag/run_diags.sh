#!/bin/sh 

#set -x

export SGE_ROOT=/opt/sge/default
export QSUBCMD=/apps/torque/default/bin/qsub


#MAKE THESE ARGUMENTS
export stormname=${1}
export stormnum=${2}
export initdate=${3}
export fcsthr=${4}

#Directory where gribfiles from UPP output are located                 
#export GRIB_DIR=/lfs2/projects/hur-aoml/rthr-aoml/pytmp/HB17_v1_forecast/com/
export GRIB_DIR=/lfs2/projects/hur-aoml/Ghassan.Alaka/pytmp/HB17_v1_forecast/com/
#Directory where graphics will be produced
export GRAPHICS_DIR=/lfs2/projects/hur-aoml/Ghassan.Alaka/GPLOT/
#Directory where you will be running this script
export WORK_DIR=${GRAPHICS_DIR}/VortDiag
#Location of plotting code
export CODE_DIR=/misc/whome/Ghassan.Alaka/GPLOT/vortex_ks/
#Location where you want final output (.gif files) to end up
export FIGURES=${GRAPHICS_DIR}/HB17_v1_forecast/${initdate}/vortex
#CURRENT GRADS DIRECTORY ... MUST BE VERSION 2.0 OR LATER
export GASCRP="/home/Thiago.Quirino/GrADS/2.0.1/bin"
#This has been moved to working script
#export fhr=`printf %03d $fcsthr`
#export fstart=`echo ${stormname}${stormnum} | tr [a-z] [A-Z]`
#export casename=${stormname}${stormnum}

cd $GRAPHICS_DIR
mkdir -p $WORK_DIR
mkdir -p $FIGURES
cd $WORK_DIR

cp ${CODE_DIR}/hwrf_diags.sh .
cp ${CODE_DIR}/*.sh .
cp ${CODE_DIR}/*.gs .
cp ${CODE_DIR}/get_cyldata.x .

qsubout=`${QSUBCMD} -A hur-aoml -l walltime=00:15:00 -N HWRF_VORT_DIAGS -e log_hwrf_vort_diags -o log_hwrf_vort_diags -V ./hwrf_diags.sh`

#rm -rf ${WORK_DIR}
