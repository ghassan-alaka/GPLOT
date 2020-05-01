#!/bin/sh --login

# This script downloads ECMWF model forecast output the ECMWF FTP
# Options include:
#     ODIR        /PATH/TO/COPY/ECMWF/DATA/
#     DO_NC       Combine the individual ECMWF data files into a single NetCDF file
#                 YES --> Do it.
#                 NO  --> Don't do it.
#
# Example invocations:  ./get_ECMWF.sh /lfs1/projects/hur-aoml/Ghassan.Alaka/pytmp/ECMWF_Forecast YES

#set -x

# Load modules
module load intel
module load ncl/6.5.0

# 1. Get input arguments
ODIR="$1"
DO_NC="$2"

FTP_DIR="dissemination.ecmwf.int"
FTP_LOGIN="wmo:essential"

# Create and enter the output directory
mkdir -p ${ODIR}
cd ${ODIR}

YMDHms="`date +'%Y%m%d%H'`0000"
YMDHms_OLD="`date --date="5 day ago" +'%Y%m%d%H'`0000"
YYYY=`echo "$YMDHms" | cut -c1-4`
MM=`echo "$YMDHms" | cut -c5-6`
DD=`echo "$YMDHms" | cut -c7-8`
HH=`echo "$YMDHms" | cut -c9-10`
HH_OFFSET=$(( $HH % 12 ))
echo "$YMDHms"
echo $HH_OFFSET

#YMDHms="${YYYY}${MM}${DD}${HH2}0000"

FTP_CYCLES=()
N=0
while [ "${YMDHms}" -gt "${YMDHms_OLD}" ]; do

    HH3=$(( $HH_OFFSET + ( N * 12 ) ))
    YMDH="`date --date="$HH3 hour ago" +'%Y%m%d%H'`"
    YMDHms="`date --date="$HH3 hour ago" +'%Y%m%d%H'`0000"

    echo "Current cycle --> ${YMDHms}"
    FTP_CYCLES+=( "${YMDHms}" )

    wget -r -A "*grib2.bin" -T 5 -N ftp://${FTP_LOGIN}@${FTP_DIR}/${YMDHms}/

    mkdir -p ${YMDH}

    # Link individual files into the expected location.
    ALL_FILES=( `find ${PWD}/${FTP_DIR}/${YMDHms}/ -type f -name "*_grib2.bin" 2> /dev/null` )
    if [ ! -z "${ALL_FILES[*]}" ]; then
        for FILE in ${ALL_FILES[@]}; do
            # Get the current file name and create the output file name.
            # This is important because NCL requires the "grib2" extension to read
            # the file properly.
            FILE_BASE=`basename $FILE`
            OFILE_BASE="`echo "$FILE_BASE" | rev | cut -c11- | rev`.grib2"

            # Don't link "_es_" or "_em_" files
            if [[ $FILE_BASE == *"_es_"* ]]; then
                continue
            elif [[ $FILE_BASE == *"_em_"* ]]; then
                continue
            fi

            #if [ ! -e ${YMDH}/${OFILE_BASE} ]; then
            ln -sf ${FILE} ${YMDH}/${OFILE_BASE}
            #fi
        done

        if [ "${DO_NC}" == "YES" ]; then
            # Call NCL script to combine multiple GRIB2 inputs into a single NetCDF input.
            ncl 'CYCLE="'"${YMDH}"'"' $GPLOT_DIR/ncl/ECMWF_combine.ncl
        fi

        # Clean up any TMP directories that still exist.
        rm -rf TMP.*

    fi

    ((N++))
done

DISK_CYCLES=( $(ls -d ${FTP_DIR}/*/ | basename 2>/dev/null) )
for CYCLE in ${DISK_CYCLES[@]}; do
    if [[ ${FTP_CYCLES[*]} != *"${CYCLE}"* ]]; then
        echo "MSG: Removing $CYCLE because it is too old"
        YMDH="`echo "$CYCLE" | cut -c1-10`"
        #rm -rf ${FTP_DIR}/${CYCLE}/
        #rm -rf ${ODIR}/${YMDH}/
    fi
done
echo ${FTP_CYCLES[*]}
echo $YMDHms_OLD

exit
