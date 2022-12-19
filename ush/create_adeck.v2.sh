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

# Print some information
echo "MSG: Using output directory: ${OUTDIR}"
echo "MSG: Using input directory:  ${IDIR}"

# Read the file tag.
if [ -z "${TAG}" ]; then
  TAG=".*/[a-z]\+[0-9]\{2\}[a-z]\.[0-9]\{10\}\.trak\..*\.atcfunix"
fi

# Find all files that match user criteria
TMP="$(date +%N)"
ILIST="${OUTDIR}/.create_adeck.${TMP}.inputFilesList"
echo "MSG: Creating a list of input files using this command:"
echo "MSG: \[find -L ${IDIR}/. -type f -regextype sed -regex \"${TAG}\" > \"${ILIST}\"\]"
find -L ${IDIR}/. -type f -regextype sed -regex "${TAG}" > "${ILIST}"
readarray -t ALL_FILES < ${ILIST}
echo "MSG: Total files to be processed: ${#ALL_FILES[@]}"

# Iterate through each input a-deck file and sort them by storm, date, and model. Example temp file: AL032010___2010052600___HWRF
#printf "%b" "MSG: Processing input files: 0.00% complete -- Load First File"
echo "MSG: Processing input files: 0.00% complete -- Load First File"
count=0
TMP="$(date +%N)"
IFILES=()
rm -f ${OUTDIR}/${TMP}*.dat
while read FILE; do

    # Increment file counter.
    count=`expr "$count" + 1`

    # Check this is a valid file.
    if [ ! -f "${FILE}" ]; then
        echo "ERROR: Invalid input A-Deck file '${FILE}'."
        echo "WARNING: Non-fatal file error. Skipping..."
        continue
    fi

    SIDS=( `awk -F "," '{print $1,$2,$3}' ${FILE} | tr -d ' ' | cut -c1-8 | tr '[a-z]' '[A-Z]' | grep "^[A-Z]\{2,2\}[0-9]\{6,6\}$" | sort -u` )
    SNUMS=( `awk -F "," '{print $2}' ${FILE} | tr -d ' ' | sort -u` )
    SBASINS=( `awk -F "," '{print $1}' ${FILE} | tr -d ' ' | sort -u` )

    i=0
    for SID in ${SIDS[@]}; do
        RAWFILE="${OUTDIR}/${TMP}.a${SID,,}.dat"
        awk -v SNUM="${SNUMS[i]}" -v BASIN="${SBASINS[i]}" -F ",[ \t]" '$1==BASIN && $2==SNUM' ${FILE} >> ${RAWFILE}
        if [ -f "${RAWFILE}" ]; then
            RAWFILES+=( "${RAWFILE}" )
        fi
        ((i++))
    done

    # Update the progress bar.
    pct=`echo "$count ${#ALL_FILES[@]}" | awk '{printf "%3.2f\n",(100.0*$1)/$2}'`
    f=`basename ${FILE}`
    f2=`printf "%50s" "$f"`
    #printf "%b" "\rMSG: Processing input files: ${pct}% complete -- ${f2}"
    echo "MSG: Processing input files: ${pct}% complete -- ${f2}"

done < "${ILIST}"

# Remove the list of input files
rm -f "${ILIST}"

# Remove duplicates from raw A-Deck files
RAWFILES=( $(printf "%s\n" "${RAWFILES[@]}" | sort -u)  )

# Loop over all raw combined A-Deck files
for FILE in "${RAWFILES[@]}"; do

    echo "MSG: Processing this temporary input file --> ${FILE}"
    OFILE="${OUTDIR}/$(echo "${FILE}" | rev | cut -d "." -f-2 | rev)"
    TMPFILE="${OUTDIR}/$(date +%N).$(echo "${FILE}" | rev | cut -d "." -f-2 | rev)"

    # Update the model code in the current A-Deck, if necessary
    if [ "${M_IN}" != "${M_OUT}" ]; then
        echo "MSG: Updating model code:  ${M_IN} --> ${M_OUT}"
        sed -i 's/'"${M_IN}"'/'"${M_OUT}"'/g' "${FILE}"
    fi

    # Check if the file needs to be updated, if necessary
    LOCK_FILE="${OFILE}.lock"
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

    # Print duplicates lines in FILE that will be removed
    tac ${FILE} | awk -F ',[ \t]' 'x[$1,$2,$3,$5,$6,$12]++' | tac

    # If the merged file already exists, we need to re-sort
    # and remove duplicates after adding contents of FILE.
    # If no differences, then do nothing.
    # If differences, then copy the new combined file TMPFILE
    # to OFILE.
    if [ -f "${OFILE}" ]; then

        # Merge the raw file (FILE) so that any duplicate entries are removed and the
        # line from FILE is retained.
        cat ${OFILE} ${FILE} | tac | sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > ${TMPFILE}
    
        DIFF=$(diff ${TMPFILE} ${OFILE})
        if [ "${DIFF}" != "" ]; then
            #printf "%b" "\rMSG: Merging unique ATCF files (${pct}% complete): Moving new deck to target location"
            #printf "%b" "\rMSG: Merging unique ATCF files (${pct}% complete): ${TMP} --> ${OFILE}"
            echo "MSG: Moving new A-Deck to target location"
            echo "MSG: ${TMPFILE} --> ${OFILE}"
            mv ${TMPFILE} ${OFILE}
            if [ ! -f ${OFILE} ]; then
                echo "ERROR: The output A-Deck doesn't exist. Something went wrong."
                exit 6
            fi
        else
            echo "MSG: A-Deck has not changed. Not moving anything."
            rm -f ${TMPFILE}
        fi
    
    else
        echo "MSG: Creating merged A-Deck file --> ${OFILE}"
        tac ${FILE} | sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > ${OFILE}
    fi

    # Release the lock.
    rm -f ${LOCK_FILE}
    #printf "%b" "\rMSG: Merging unique ATCF files (${pct}% complete): Output A-Deck processing complete. Unlocking it."
    echo "MSG: Output A-Deck processing complete. Unlocking it."

    # Remove the temporary input file.
    rm -f ${FILE}

done

#    #Update the progress bar.
#    pct=`echo "$count ${#SID_UNIQ[@]}" | awk '{printf "%3.2f\n",(100.0*$1)/$2}'`
#    #printf "%b" "\rMSG: Merging unique forecasts (${pct}% complete): Finished."
#    echo "MSG: Finished processing ${SID}."

# Delete the temporary directory and exit.
#printf "\nMSG: Created $numStorms merged a-deck ATCF files.\n"
#echo "MSG: Created $numStorms merged a-deck ATCF files."
#rm -rf "${TMPDIR}"

echo "MSG: create_adeck.sh completed at `date`."
