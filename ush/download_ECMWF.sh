#!/bin/sh

# This script downloads ECMWF model forecast output
# from the ECMWF Data Store (ECPDS).

echo "download_ECMWF.sh started at `date`"

# Get input arguments
ODIR="${1}"
#ODIR="/lfs1/HFIP/hur-aoml/Ghassan.Alaka/pytmp/ECMWF_Forecast_2022"

# Create and enter the output directory
echo "MSG: I will download data to this directory --> ${ODIR}"
mkdir -p ${ODIR}
cd ${ODIR}

# Set ECPDS directory
EC_DIR="https://data.ecmwf.int/forecasts"
EC_MODEL="0p4-beta"
EC_APP="oper"

# Get the current date and cutoff
YMDH="`date +'%Y%m%d%H'`"
YMDH_CUT="`date --date="3 day ago" +'%Y%m%d%H'`"
YYYY=`echo "${YMDH}" | cut -c1-4`
MM=`echo "${YMDH}" | cut -c5-6`
DD=`echo "${YMDH}" | cut -c7-8`
HH=`echo "${YMDH}" | cut -c9-10`
HH_OFFSET=$(( ${HH} % 6 ))
echo "MSG: The current date --> ${YMDH}"
echo "MSG: The cutoff date  --> ${YMDH_CUT}"
echo "MSG: The hour offset  --> ${HH_OFFSET}"

DATA_CYCLES=()
N=0
while [ "${YMDH}" -gt "${YMDH_CUT}" ]; do

    # Work backwards from the original date to find most recent data
    HOFF=$(( $HH_OFFSET + ( N * 6 ) ))
    YMDH="`date --date="${YYYY}-${MM}-${DD} ${HH}:00:00 ${HOFF} hours ago" +'%Y%m%d%H'`"
    YMD="`echo "${YMDH}" | cut -c1-8`"
    HH2="`echo "${YMDH}" | cut -c9-10`"
    echo "Current cycle --> ${YMDH}"

    # Save the date to an array and create the full output directory
    DATA_CYCLES+=( "${YMDH}" )
    ODIR_FULL="${ODIR}/${YMDH}"
    mkdir -p ${ODIR_FULL}

    # Create the full ECMWF source directory
    EC_DIR_FULL="${EC_DIR}/${YMD}/${HH2}z/${EC_MODEL}/${EC_APP}/"

    # Download the data with wget commands
    wget -np -nH --cut-dirs 5 -r -A "*.grib2" -T 5 -N ${EC_DIR_FULL} -P ${ODIR_FULL}
    wget -np -nH --cut-dirs 5 -r -A "*.index" -T 5 -N ${EC_DIR_FULL} -P ${ODIR_FULL}
    wget -np -nH --cut-dirs 5 -r -A "*.bufr" -T 5 -N ${EC_DIR_FULL} -P ${ODIR_FULL}

    # Adance the counter
    ((N++))
done

# Remove old cycles
DISK_CYCLES=( $(ls -d ${ODIR}/*/ | basename 2>/dev/null) )
for YMDH in ${DISK_CYCLES[@]}; do
    if [[ ${DATA_CYCLES[*]} != *"${YMDH}"* ]]; then
        echo "MSG: Removing ${YMDH} because it is too old"
        #rm -rf ${ODIR}/${YMDH}
    fi
done

