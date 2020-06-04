#!/bin/sh

################################################################
#
# Name:  GPLOT_tarballer.sh
#
# Description:  This bash script creates archives of GPLOT
#               graphics on NOAA's HPSS. If the archive already
#               exists on HPSS, then a new archive will NOT
#               be created unless FORCE=YES. Scrubbing is
#               optional.
# 
# Created On:   February 13, 2020
#
# Example Invocation:
# ./GPLOT_tarballer.sh {EXPT} {IDIR} {HPSSDIR} {MATCH} {FORCE} {SCRUB}
# ./GPLOT_tarballer.sh GFS_Forecast /lfs3/projects/hur-aoml/Ghassan.Alaka/GPLOT/ /5year/HFIP/hur-aoml/Ghassan.Alaka/GPLOT/ 201906 NO YES
#
# Modification Log:
#
################################################################


# Use help option
if [ "$1" == "-h" ] || [ "$1" == "--help" ]; then
  echo "HELP PAGE FOR GPLOT_tarballer.sh"
  echo ""
  echo "Description:  This bash script creates archives of GPLOT"
  echo "              graphics on NOAA's HPSS. If the archive already"
  echo "              exists on HPSS, then a new archive will NOT"
  echo "              be created unless FORCE=YES. Scrubbing is"
  echo "              optional."
  echo ""
  echo "Example Invocation:"
  echo "./GPLOT_tarballer.sh {EXPT} {IDIR} {HPSSDIR} {MATCH} {FORCE} {SCRUB}"
  echo "./GPLOT_tarballer.sh GFS_Forecast /lfs3/projects/hur-aoml/Ghassan.Alaka/GPLOT/ /5year/HFIP/hur-aoml/Ghassan.Alaka/GPLOT/ 201906 NO YES"
  exit 0
fi

# Load modules
module load hpss


# Input variables via command line
EXPT="$1"
IDIR="$2"
HPSSDIR="$3"
MATCH="$4"
FORCE="$5"
SCRUB="$6"

if [ -z "$FORCE" ]; then
  FORCE="NO"
fi
if [ -z "$SCRUB" ]; then
  SCRUB="NO"
fi


# Uncomment these to manually set parameters.
# This would overwrite any command line input.
#EXPT=GFS_Forecast
#IDIR=/lfs2/projects/hur-aoml/Ghassan.Alaka/GPLOT/${EXPT}/
#HPSSDIR=/5year/HFIP/hur-aoml/Ghassan.Alaka/GPLOT/${EXPT}/
#MATCH=gfs.2016042700


# Check that directory exists on HPSS
/apps/hpss/hsi ls ${HPSSDIR} >& ${IDIR}/hsi.out
if grep -q ${EXPT} $IDIR/hsi.out; then
  echo "${EXPT}:  Archive path found."
else
  echo "${EXPT}:  Archive path not found. Creating..."
  /apps/hpss/hsi mkdir ${HPSSDIR}/${EXPT}
fi


# Loop over all available forecasts within EXPT
find ${IDIR}/*${MATCH}* -maxdepth 0 -type d | xargs -n 1 basename | while read TMP; do

  echo $TMP

  # ARCHIVE
  /apps/hpss/hsi ls ${HPSSDIR}/${EXPT}/ >& ${IDIR}/hsi.out
  if grep -q $TMP.tar $IDIR/hsi.out; then
    if [ "$FORCE" == "YES" ]; then
      echo "${TMP}:  Archive found. Deleting it and recreating."
      /apps/hpss/hsi rm ${HPSSDIR}/${EXPT}/${TMP}.tar
      /apps/hpss/htar -cvpf ${HPSSDIR}/${EXPT}/${TMP}.tar ${IDIR}/${TMP}/*
    else
      echo "${TMP}:  Archive found. Skipping."
    fi
  else
    echo "${TMP}:  Archive started."
    /apps/hpss/htar -cvpf ${HPSSDIR}/${EXPT}/${TMP}.tar ${IDIR}/${TMP}/*
    wait
    echo "${TMP}:  Archive completed."
  fi


  # SCRUB
  if [ "$SCRUB" == "YES" ]; then
    /apps/hpss/hsi ls ${HPSSDIR}/${EXPT}/ >& ${IDIR}/hsi.out
    if grep -q $TMP.tar $IDIR/hsi.out; then
      echo "${TMP}:  Archive confirmed. Will scrub from disk."
      rm -rf ${IDIR}/${TMP}
    else
      echo "${TMP}:  Archive unconfirmed. Check for errors."
    fi
  fi

done
