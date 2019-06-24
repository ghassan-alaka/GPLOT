#!/bin/sh

# Input variables via command line
EXPT="$1"
IDIR="$2"
HPSSDIR="$3"
MATCH="$4"


# Uncomment these to manually set parameters.
# This would overwrite any command line input.
#EXPT=GFS_Forecast
#IDIR=/lfs2/projects/hur-aoml/Ghassan.Alaka/GPLOT/${EXPT}/
#HPSSDIR=/5year/HFIP/hur-aoml/Ghassan.Alaka/GPLOT/${EXPT}/
#MATCH=gfs.2016042700


# Check that directory exists on HPSS
/apps/hpss/hsi ls ${HPSSDIR} >& ${IDIR}/${EXPT}/hsi.out
if grep -q ${EXPT} $IDIR/${EXPT}/hsi.out; then
  echo "${EXPT}:  Archive path found."
else
  echo "${EXPT}:  Archive path not found. Creating..."
  /apps/hpss/hsi mkdir ${HPSSDIR}/${EXPT}
fi


# Loop over all available forecasts within EXPT
find ${IDIR}/${EXPT}/*${MATCH}* -maxdepth 0 -type d | xargs -n 1 basename | while read TMP; do

  echo $TMP

  # ARCHIVE
  /apps/hpss/hsi ls ${HPSSDIR}/${EXPT}/ >& ${IDIR}/${EXPT}/hsi.out
  if grep -q $TMP.tar $IDIR/${EXPT}/hsi.out; then
    echo "${TMP}:  Archive found. Skipping."
  else
    echo "${TMP}:  Archive started."
    /apps/hpss/htar -cvpf ${HPSSDIR}/${EXPT}/${TMP}.tar ${IDIR}/${EXPT}/${TMP}/*
    wait
    echo "${TMP}:  Archive completed."
  fi


  # SCRUB
  /apps/hpss/hsi ls ${HPSSDIR}/${EXPT}/ >& ${IDIR}/${EXPT}/hsi.out
  if grep -q $TMP.tar $IDIR/${EXPT}/hsi.out; then
    echo "${TMP}:  Archive confirmed. Will scrub from disk."
    rm -rf ${IDIR}/${EXPT}/${TMP}
  else
    echo "${TMP}:  Archive unconfirmed. Check for errors."
  fi

done
