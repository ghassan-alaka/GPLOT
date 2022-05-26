#!/bin/sh --login
#SBATCH --account=hur-aoml
#SBATCH --nodes=1
#SBATCH --time=00:10:00
#SBATCH --partition=service
#SBATCH --mail-type=FAIL
#SBATCH --qos=windfall
#SBATCH --chdir=.
#SBATCH --output=/lfs4/HFIP/hur-aoml/Ghassan.Alaka/GPOUT/log/create_adeck.log
#SBATCH --error=/lfs4/HFIP/hur-aoml/Ghassan.Alaka/GPOUT/log/create_adeck.log
#SBATCH --job-name="create_adeck"
#SBATCH --mem=16G

################################################################################
#### UNIX Script Documentation Block
##
## Script name:         create_adeck.sh
##
## Script description:  Create a combined a-deck file
##
## Author:              Ghassan J. Alaka, Jr.
##
## Date:                2020-07-15
##
## Script history log:
##
## 2020-07-15  Ghassan J. Alaka, Jr. -- Original version.
##
## Usage: create_adeck.sh <Output Directory> <List of files>
##
################################################################################

# Parse input arguments
while test $# -gt 0; do

    case "$1" in
        -h|--help)
            echo "Create A-Deck -- parse & create combined A-Deck files"
            echo " "
            echo "create_adeck.sh [options]"
            echo " "
            echo "options:"
            echo "-h, --help               show help"
            echo "-i, --input              input data directory"
            echo "-o, --output             output data directory"
            echo "-n, --name               file name search string"
            echo "-m1, --modelin           input model name (AAAA)"
            echo "-m2, --modelout          output model name (BBBB)"
            exit 0
            ;;
        -i|--input)
            shift
            if test $# -gt 0; then
                IDIR="${1}"
                if [ ! -d "${IDIR}" ]; then
                    echo "ERROR: Input directory does not exist."
                    exit 1
                fi
            else
                echo "ERROR: Input directory not specified."
                exit 1
            fi
            shift
            ;;
        -o|--output)
            shift
            if test $# -gt 0; then
                OUTDIR="${1}"
                if [ ! -d "${OUTDIR}" ]; then
                    echo "MSG: Creating output directory because it does not exist."
                    mkdir -p ${OUTDIR}
                fi
            else
                echo "ERROR: Output directory not specified."
                exit 1
            fi
            shift
            ;;
        -n|--name)
            shift
            if test $# -gt 0; then
                TAG="${1}"
            else
                echo "WARNING: File name tag not specified. Defaulting to '*atcfunix'."
                TAG=".*/[a-z]\+[0-9]\{2\}[a-z]\.[0-9]\{10\}\.trak\..*\.atcfunix"
            fi
            shift
            ;;
        -m1|--modelin)
            shift
            if test $# -gt 0; then
                M_IN="${1}"
            fi
            shift
            ;;
        -m2|--modelout)
            shift
            if test $# -gt 0; then
                M_OUT="${1}"
            fi
            shift
            ;;
        *)
            break
            ;;

    esac

done

echo "MSG: create_adeck.sh started at `date`."

# Check if at least 2 arguments were specified.
#if [ $# -lt 2 ]; then
#    echo "ERROR: At least 2 arguments are required. See script header for details."
#    exit 1
#fi

# Read the output directory
#OUTDIR="$1"
#mkdir -p ${OUTDIR}
echo "MSG: Using output directory: ${OUTDIR}"

# Read the input directory
#IDIR="$2"
#if [ ! -d "${IDIR}" ]; then
#  echo "ERROR: Input directory must exist."
#  exit 2
#fi
echo "MSG: Using input directory: ${IDIR}"

# Read the file tag.
#TAG="$3"
if [ -z "${TAG}" ]; then
  TAG=".*/[a-z]\+[0-9]\{2\}[a-z]\.[0-9]\{10\}\.trak\..*\.atcfunix"
fi

# Create a temporary directory.
TMPDIR="${OUTDIR}/.create_adeck_tmp_${USER}_${HOSTNAME}_PROC-$$_RAND-$RANDOM"
mkdir -p ${TMPDIR}
if [ $? -ne 0 ] || [ ! -d ${TMPDIR} ]; then
    echo "ERROR: Cannot create temporary directory '${TMPDIR}'. Please check permissions and availability of disk space."
    exit 2
fi
echo "MSG: Created temporary directory --> ${TMPDIR}"
rm -rf "${TMPDIR}"/*

# Find all files that match user criteria
#TAG="atcfunix"
echo "find ${IDIR}/. -type f -regextype sed -regex \"${TAG}\" > \"${TMPDIR}/.inputFilesList\""
find -L ${IDIR}/. -type f -regextype sed -regex "${TAG}" > "${TMPDIR}/.inputFilesList"
ALL_FILES=( `find -L ${IDIR}/. -type f -regextype sed -regex "${TAG}" -printf "%f\n"` )

# Display some info to user
echo "MSG: Total files to be processed: ${#ALL_FILES[@]}"

# Iterate through each input a-deck file and sort them by storm, date, and model. Example temp file: AL032010___2010052600___HWRF
#printf "%b" "MSG: Processing input files: 0.00% complete -- Load First File"
echo "MSG: Processing input files: 0.00% complete -- Load First File"
count=0
while read file; do

    # Increment file counter.
    count=`expr "$count" + 1`
    #echo $count

    # Check this is a valid file.
    if [ ! -f "$file" ]; then
        echo "ERROR: Invalid input a-deck file '$file'."
        echo "WARNING: Non-fatal file error. Skipping..."
        continue
    fi

    # Iterate through each line in the file, removing leading and trailing whitespaces.
    FILE_CHK=0 # Did this file lead to the creation of a temporary file.
    cat "$file" | awk '{ gsub(/^[ \t]+|[ \t]+$/, ""); print }'| awk '(length($0)>0){print}' | while read line; do
#echo "$BASHPID"
        # Get the storm name, forecast date, and model name.
        SID=`echo "$line" | awk -F "," '{print $1,$2,$3}' | tr -d ' ' | cut -c1-8 | tr '[a-z]' '[A-Z]' | grep "^[A-Z]\{2,2\}[0-9]\{6,6\}$"`
        CYCLE=`echo "$line" | awk -F "," '{print $3}' | tr -d ' ' | grep "^[0-9]\{10,10\}$"`
        MODEL=`echo "$line" | awk -F "," '{print $5}' | tr -d ' '`
        FHR=`echo "$line" | awk -F "," '{print $6}' | tr -d ' '`

        # Check that all arguments are valid.
        if [ -z "$SID" ] || [ -z "$CYCLE" ] || [ -z "$MODEL" ]; then
            #printf "\nERROR: Invalid line in input a-deck file '$file'. Please check the file format.\nLine: $line\n"
            echo "ERROR: Invalid line in input a-deck file '$file'. Please check the file format."
            echo "ERROR: Invalid Line: ${line}"
            rm -rf "${TMPDIR}"
            exit 3
        fi

        # Check if the temporary file exists.
        if [ ! -f "${TMPDIR}/${SID}___${CYCLE}___${MODEL}" ]; then
            FILE_CHK=1

            # If this input file contains the forecast of a single model for a single storm starting
            # at a single date, then simply copy the input file as the temporary file.
            TEST1=`awk -F "," '{print $1,$2,$3}' "$file" | tr -d ' ' | cut -c1-8 | sort -u | wc -l`
            TEST2=`awk -F "," '{print $3}' "$file" | tr -d ' ' | sort -u | wc -l`
            TEST3=`awk -F "," '{print $5}' "$file" | tr -d ' ' | sort -u | wc -l`
            if [ ${TEST1} -eq 1 ] && [ ${TEST2} -eq 1 ] && [ ${TEST3} -eq 1 ]; then
                ln -sf "$file" "${TMPDIR}/${SID}___${CYCLE}___${MODEL}"
                break
            fi
        fi

        # Search for an existing entry in the temporary file, and quit if found.
        if [ "${FILE_CHK}" -ne 1 ] && [ ! -z "$(awk -F "," -v h="$FHR" '(1.0*h==1.0*$6){print}' "${TMPDIR}/${SID}___${CYCLE}___${MODEL}")" ]; then
            #printf "%b" "\nERROR: Duplicate entry found for storm '$SID', forecast date '$CYCLE', model '$MODEL', and forecast hour '$FHR'.\n"
            echo "WARNING: Duplicate entry found for storm '${SID}', forecast date '${CYCLE}', model '${MODEL}', and forecast hour '${FHR}'."
            continue
        fi

        # Ensure we are not attempting to write into a symbolic link temporary file
        # That represents an input ATCF file containing a unique forecast.
        if [ -h "${TMPDIR}/${SID}___${CYCLE}___${MODEL}" ]; then
            #printf "%b" "\nERROR: Tried to write data from input file '$file' into temporary symbolic link file '${TMPDIR}/${SID}___${CYCLE}___${MODEL}' which points to a previously processed input ATCF file. This means that the ATCF forecast data for storm '$SID', forecast date '$CYCLE', and model '$MODEL' are located in more than 1 file. You are on your own here, buddy. I don't handle that and I don't get paid enough. Why would you do that anyway? That is so weird, man.\n"
            echo "$BASHPID"
            echo "WARNING: The ATCF forecast data for storm '${SID}', forecast date '${CYCLE}', and model '${MODEL}' are located in more than 1 file."
            continue
        fi

        #Write line data to the specified file.
        echo "$line" >> "${TMPDIR}/${SID}___${CYCLE}___${MODEL}"

    done

    # Update the progress bar.
    pct=`echo "$count ${#ALL_FILES[@]}" | awk '{printf "%3.2f\n",(100.0*$1)/$2}'`
    f=`basename ${file}`
    f2=`printf "%50s" "$f"`
    #printf "%b" "\rMSG: Processing input files: ${pct}% complete -- ${f2}"
    echo "MSG: Processing input files: ${pct}% complete -- ${f2}"

done < "${TMPDIR}/.inputFilesList"

# Count the number of intermediary files created.
NTMP=`ls "$TMPDIR" | wc -l | tr -d ' '`
if [ "$NTMP" -eq 0 ]; then
    #printf "\nERROR: All lines in the input a-deck files were empty.\n"
    echo "ERROR: All lines in the input a-deck files were empty."
    rm -rf "${TMPDIR}"
    exit 4
else
    #printf "\nMSG: Unique model forecasts found: $NTMP\n"
    echo "MSG: Unique model forecasts found: ${NTMP}"
fi

# Get a count of the final number of output merged a-deck files.
SID_UNIQ=( `ls "${TMPDIR}" | awk -F "___" '{print $1}' | sort -u` ) #Get the unique storm IDs.
#echo "${SID_UNIQ[*]}"
NSTORMS=`printf "$SID_UNIQ\n" | wc -l | tr -d ' '`        #The number of merged files.
#echo "$NSTORMS"
#echo "${#SID_UNIQ[@]}"

# Merge all files belonging to the same storm into a single a-deck file.
# Iterate through every storm.
#printf "%b" "MSG: Merging unique forecasts: 0.00% complete"
echo "MSG: Merging unique forecasts: 0.00% complete"
count=0
#printf "$SID_UNIQ\n" | while read SID; do
for SID in ${SID_UNIQ[@]}; do

    echo "MSG: Processing ${SID}"

    #Increment file counter.
    count=`expr "$count" + 1`
    #pct=`echo "$count ${#SID_UNIQ[@]}" | awk '{printf "%3.2f\n",(100.0*$1)/$2}'`

    #Lowercase the storm ID for use in the merged a-deck file name.
    SID_lower=`echo "$SID" | tr '[A-Z]' '[a-z]'`

    #Remove any older version of the destination merged file.
    MERGED_FILE="${OUTDIR}/a${SID_lower}.dat"
    TMP_FILE="${TMPDIR}/$(date +%N).a${SID_lower}.dat"

    if [ -f ${MERGED_FILE} ]; then
        FILE_CHK="YES"
    else
        FILE_CHK="NO"
    fi

    #Iterate through the unique forecasts of this storm.
    ls "${TMPDIR}" | grep "^${SID}___" | awk -F "___" '{printf "%s___%s\n",$2,$3}' | sort -u | while read dateAndModel;do
        cat "${TMPDIR}/${SID}___${dateAndModel}" >> "${TMP_FILE}"
    done

    # Update the model code in the current A-Deck, if necessary
    if [ "${M_IN}" != "${M_OUT}" ]; then
        echo "MSG: Updating model code:  ${M_IN} --> ${M_OUT}"
        sed -i 's/'"${M_IN}"'/'"${M_OUT}"'/g' "${TMP_FILE}"
    fi

    # Check if the file needs to be updated, if necessary
    if [ "$FILE_CHK" == "YES" ]; then
        TMP="${TMPDIR}/$(date +%N).a${SID_lower}.dat"

        LOCK_FILE="${MERGED_FILE}.lock"
        T=0
        while [ -f ${LOCK_FILE} ] && [ "$T" -lt 120 ]; do
            #printf "%b" "\rMSG: Merging unique ATCF files (${pct}% complete): Output A-Deck is locked. Sleeping 5 seconds..."
            echo "MSG: Output A-Deck is locked. Sleeping 5 seconds..."
            sleep 5
            T=`expr $T + 5`
        done
        if [ $T -ge 120 ]; then
            echo "ERROR: I've been waiting too long for this file to unlock. Something went wrong."
            exit 100
        fi
        #printf "%b" "\rMSG: Merging unique ATCF files (${pct}% complete): Output A-Deck is unlocked. Locking it."
        echo "MSG: Output A-Deck is unlocked. Locking it."
        lockfile -r-1 -l 180 ${LOCK_FILE}

        cat ${TMP_FILE} ${MERGED_FILE} | sort -t, -k3,3 -k5,5 -k6,6n -k12,12 | sort -r | sort -k3,3 -k5,5 -k6,6n -k12,12 -u -t, > ${TMP}
        #cp -p ${TMP} /lfs1/HFIP/hur-aoml/Ghassan.Alaka/SCRATCH/a${SID_lower}.dat

        DIFF=$(diff ${TMP} ${MERGED_FILE})
        if [ "${DIFF}" != "" ]; then
            #printf "%b" "\rMSG: Merging unique ATCF files (${pct}% complete): Moving new deck to target location"
            #printf "%b" "\rMSG: Merging unique ATCF files (${pct}% complete): ${TMP} --> ${MERGED_FILE}"
            echo "MSG: Moving new deck to target location"
            echo "MSG: ${TMP} --> ${MERGED_FILE}"
            mv ${TMP} ${MERGED_FILE}
            if [ ! -f ${MERGED_FILE} ]; then
                echo "ERROR: The output a-deck doesn't exist. Something went wrong."
                exit 6
            fi
        else
            echo "MSG: Deck has not changed. Not moving anything."
            rm -f ${TMP}
        fi

        # Release the lock.
        rm -f ${LOCK_FILE}
        #printf "%b" "\rMSG: Merging unique ATCF files (${pct}% complete): Output A-Deck processing complete. Unlocking it."
        echo "MSG: Output A-Deck processing complete. Unlocking it."

        rm -f ${TMP_FILE}
    else
        mv ${TMP_FILE} ${MERGED_FILE}
    fi

    #Update the progress bar.
    pct=`echo "$count ${#SID_UNIQ[@]}" | awk '{printf "%3.2f\n",(100.0*$1)/$2}'`
    #printf "%b" "\rMSG: Merging unique forecasts (${pct}% complete): Finished."
    echo "MSG: Finished processing ${SID}."

done

# Delete the temporary directory and exit.
#printf "\nMSG: Created $numStorms merged a-deck ATCF files.\n"
#echo "MSG: Created $numStorms merged a-deck ATCF files."
rm -rf "${TMPDIR}"

echo "MSG: create_adeck.sh completed at `date`."
