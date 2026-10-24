#!/bin/bash
#SBATCH --job-name=herc_s3_copy
#SBATCH --output=herc_s3_copy_%j.out
#SBATCH --error=herc_s3_copy_%j.err
#SBATCH --time=02:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

MAX_JOBS=12

#NOTE TO FUTURE EDITORS - MAX_JOBS limits simulatenous AWS calls 
#I got throttled when I set it to 12.

#Will need a condition to STOP this script if all data has already been copied.
#Currently, the only redundancy check is to skip any files in the target directory
#that contain the string .fNNN where N is 0-9. This way, .grb2 files won't be 
#re-downloaded, but allows for ATCF to be updated.

set -u

export AWS_CLI=/home/donahue/.local/bin/aws

# Get current cycle in YYYYMMDDHH format
#CYCLE=$(date -u +%Y%m%d%H)
#testing with old cycles:
CYCLE="2026090100"


# Determine ensemble members based on cycle hour
HH=${CYCLE:8:2}

if [[ "$HH" == "00" ]]; then
    MEMBERS=(00 01 03 05 07 09 11 13 15 17 19)
elif [[ "$HH" == "12" ]]; then
    MEMBERS=(00 02 04 06 08 10 12 14 16 18 20)
else
    echo "ERROR: Cycle hour is $HH, expected 00 or 12."
    exit 1
fi

BASE_DIR=/work/noaa/aoml-hafs1/donahue/HERC_2026

mkdir -p "${BASE_DIR}/${CYCLE}"

cd "${BASE_DIR}/${CYCLE}" || exit 1

echo "Cycle:   ${CYCLE}"
echo "Members: ${MEMBERS[*]}"
echo "Output:  ${BASE_DIR}/${CYCLE}"
echo

for MEM in "${MEMBERS[@]}"; do

    echo "========================================"
    echo "Processing member ${MEM}"
    echo "========================================"

    mkdir -p "${MEM}"
    cd "${MEM}" || exit 1

    mkdir -p "GRIB2SUBSET"
    mkdir -p "00l.${CYCLE}.RESTART"

    #retros
    # S3_PREFIX="s3://noaa-nws-hafs-pds/herc/archive/${MEM}/${CYCLE}"

    #live
    S3_PREFIX="s3://noaa-nws-hafs-pds/herc/H226_ensemble_cloud/${MEM}/${CYCLE}"

    echo "S3 source: ${S3_PREFIX}"


    # Get list of files in S3
    "${AWS_CLI}" s3 ls "${S3_PREFIX}" --recursive --no-sign-request |
    while read -r DATE TIME SIZE S3FILE; do
        # Skip empty lines
        [[ -z "$S3FILE" ]] && continue

        FILENAME=$(basename "$S3FILE")

        # Don't overwrite existing .fNNN files
        if [[ "$FILENAME" =~ \.f[0-9]{3} ]] && [[ -e "$FILENAME" ]]; then
            echo "Skipping existing forecast file: ${FILENAME}"
            continue
        fi

        # Don't overwrite any other existing file either - don't know if this is needed, we want ATCF to overwrite?
        #if [[ -e "$FILENAME" ]]; then
        #    echo "Skipping existing file: ${FILENAME}"
        #    continue
        #fi

        echo "Copying: ${FILENAME}"

	    #new version
        if [[ "$FILENAME" == *"subset"* ]]; then
            #MD 20260901 - don't need subset files I think, just continue here.
            continue
            DEST_DIR="$(pwd)/GRIB2SUBSET"

        elif [[ "$FILENAME" == "atmos_static.nc" || "$FILENAME" == "grid_spec.nc" ]]; then
            DEST_DIR="$(pwd)/00l.${CYCLE}.RESTART"

        else
            DEST_DIR="$(pwd)"
        fi

        /work/noaa/aoml-hafs1/donahue/copy_file_s3.sh \
            "s3://noaa-nws-hafs-pds/${S3FILE}" \
            "$DEST_DIR" >> /work/noaa/aoml-hafs1/donahue/HERC_COPY_LOG.txt 2>&1 &

        while (( $(jobs -rp | wc -l) >= MAX_JOBS )); do
                sleep 1
        done

	#old version
        #"${AWS_CLI}" s3 cp \
        #    --no-sign-request \
        #    "s3://noaa-nws-hafs-pds/${S3FILE}" \
        #    .

    done

    cd .. || exit 1

done

echo
echo "Finished ${CYCLE}"
