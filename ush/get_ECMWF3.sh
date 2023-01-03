#!/bin/sh --login

# get_ECMWF3.sh
# This script is retrieves ECMWF model output
# from the ECMWF data store. It uses 'wget' to
# download the GRIB2 data and 'wgrib2' to check
# the status of the GRIB2 file once downloaded.
# Data will be kept on disk for one week

echo "MSG: get_ECMWF3.sh started at `date`"

# Load modules
module load intel/2022.1.2
module load wgrib2/2.0.8

# Get input arguments
ODIR="${1}"

# Define important variables
HTTP_DIR="${2:-"https://data.ecmwf.int/forecasts"}"
RESOL="${3:-"0p4-beta"}"
STREAM00="${4:-"oper"}"
STREAM06="${5:-"scda"}"
TYPE="${6:-"fc"}"
FHR_LIST00=( $(seq 0 3 144) )
FHR_LIST00+=( $(seq 150 6 240) )
FHR_LIST06=( $(seq 0 3 90) )

# Create and enter the output directory
ODIR_FULL="${ODIR}/${RESOL}/${STREAM00}"
mkdir -p ${ODIR_FULL}
cd ${ODIR_FULL}

YMDH_NOW="`date +'%Y%m%d%H'`"
YMDH_OLD="`date --date="4 day ago" +'%Y%m%d%H'`"
YMDH_SCRUB="`date --date="7 day ago" +'%Y%m%d%H'`"
echo "MSG: Current date: ${YMDH_NOW}"


echo ""
echo "MSG: ***************************"
echo "MSG: DOWNLOAD SECTION"

HTTP_CYCLES=()
YMDH="${YMDH_NOW}"
N=0
while [ "${YMDH}" -gt "${YMDH_OLD}" ]; do

    # Get the current time and the expected offset
    # to get the cycle of interest
    YMDH_NOW="`date +'%Y%m%d%H'`"
    HH_NOW="`echo "${YMDH_NOW}" | cut -c9-10`"
    HH_OFF="$(( ${HH_NOW} % 6 + (N * 6) ))"

    # Get the cycle of interest.
    YMDH="`date --date="${HH_OFF} hour ago" +'%Y%m%d%H'`"
    HH=`echo "${YMDH}" | cut -c9-10`
    YMD="`date --date="${HH_OFF} hour ago" +'%Y%m%d'`"

    # Create the cycle directory.
    echo ""
    echo "MSG: Current cycle --> ${YMDH}"
    mkdir -p ${YMDH}

    # Define variables based on the epoch
    if [[ "00 12" == *"${HH}"* ]]; then
        FHR_LIST=( "${FHR_LIST00[@]}" )
        STREAM="${STREAM00}"
    elif [[ "06 18" == *"${HH}"* ]]; then
        FHR_LIST=( "${FHR_LIST06[@]}" )
        STREAM="${STREAM06}"
    fi

    # Build the expected HTTP path
    HTTP_PATH="${HTTP_DIR}/${YMD}/${HH}z/${RESOL}/${STREAM}"

    for F in ${FHR_LIST[@]}; do

        # 3-digit forecast hour
        FFF="$(printf "%03d\n" $((10#${F})))"

        # Define HTTP and output file names
        HTTP_FNAME="${YMDH}0000-${F}h-${STREAM}-${TYPE}.grib2"
        O_FNAME="ecmwf.oper-fc.${YMDH}.f${FFF}.grib2"
        echo ""
        echo "MSG: Retrieving this file: ${HTTP_PATH}/${HTTP_FNAME}"
        echo "MSG: Delivering file here: ${PWD}/${YMDH}/${O_FNAME}"

        # Check if the file is corrupt. If so, remove it.
        if [ -f ${YMDH}/${O_FNAME} ]; then
            echo "MSG: The file already exists, so I will check it."
            wgrib2 ${YMDH}/${O_FNAME} &> /dev/null
            if [ $? -ne 0 ]; then
                echo "MSG: File is corrupt, so I will remove it."
                rm -f ${YMDH}/${O_FNAME}
            else
                echo "MSG: The file looks good. Moving on."
                continue
            fi
        fi

        # If the file does not exist, download it.
        if [ ! -f ${YMDH}/${O_FNAME} ]; then
            echo "MSG: Downloading the file with this command:"
            echo "MSG: [wget -T 5 -O ${YMDH}/${O_FNAME} ${HTTP_PATH}/${HTTP_FNAME}]"
            #wget -T 5 -np -nH --cut-dirs=5 -O ${YMDH}/${O_FNAME} ${HTTP_PATH}/${HTTP_FNAME}
            wget -q -T 5 -O ${YMDH}/${O_FNAME} ${HTTP_PATH}/${HTTP_FNAME}
        fi

        # If the file is empty, delete it.
        if [ ! -s ${YMDH}/${O_FNAME} ]; then
            echo "MSG: File is empty, so I will remove it."
            rm -f ${YMDH}/${O_FNAME}

        # Otherwise, check if the file is corrupt.
        else
            wgrib2 ${YMDH}/${O_FNAME} &> /dev/null
            if [ $? -ne 0 ]; then
                echo "MSG: File is corrupt, so I will remove it."
                rm -f ${YMDH}/${O_FNAME}
            else
                echo "MSG: File downloaded successfully."
            fi
        fi

    done

    # Download the BUFR file with TC tracker info (if available)
    HTTP_FNAME="${YMDH}0000-${FHR_LIST[-1]}h-${STREAM}-tf.bufr"
    if [ ! -f ${YMDH}/${HTTP_FNAME} ]; then
        echo "MSG: Retrieving this file: ${HTTP_PATH}/${HTTP_FNAME}"
        echo "MSG: Delivering file here: ${PWD}/${YMDH}/${HTTP_FNAME}"
        echo "MSG: Downloading the file with this command:"
        echo "MSG: [wget -q -T 5 -O ${YMDH}/${HTTP_FNAME} ${HTTP_PATH}/${HTTP_FNAME}]"
        wget -q -T 5 -O ${YMDH}/${HTTP_FNAME} ${HTTP_PATH}/${HTTP_FNAME}

        # If the file is empty, delete it.
        if [ ! -s ${YMDH}/${HTTP_FNAME} ]; then
            echo "MSG: File is empty, so I will remove it."
            rm -f ${YMDH}/${HTTP_FNAME}
        fi
    fi


    # Check if the cycle directory is empty
    if [ -z "$(ls -A ${YMDH})" ]; then
        rm -rf ${YMDH}
    else
        HTTP_CYCLES+=( "${YMDH}" )
    fi

    # Increase the counter
    ((N++))

done


# Scrub empty directories
echo ""
echo "MSG: ***************************"
echo "MSG: SCRUB SECTION"
echo "MSG: I won't scrub these cycles because they are active --> ${HTTP_CYCLES[*]}"
DISK_CYCLES=( $(ls -d * 2>/dev/null) )
re='^[0-9]+$'
for CYCLE in ${DISK_CYCLES[@]}; do
    echo "MSG: Found this cycle on disk:  ${CYCLE}"

    if ! [[ ${CYCLE} =~ ${re} ]]; then
        echo "MSG: ${CYCLE} is not a date, so I will skip it."
        continue
    elif [ "${CYCLE}" -lt "${YMDH_SCRUB}" ]; then
        echo "MSG: Removing ${CYCLE} because it is too old."
        rm -rf ${CYCLE}
    elif [ -z "$(ls -A ${CYCLE})" ]; then
        echo "MSG: Removing ${CYCLE} because it is empty."
        rm -rf ${CYCLE}
    else:
        echo "MSG: ${CYCLE} is still active, so I will keep it."
    fi
done

echo "MSG: get_ECMWF3.sh completed at `date`"
