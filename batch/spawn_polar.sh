#!/bin/sh --login
#set -x


# Load modules (based on NOAA's Jet)
#module load slurm

# Define critical environmental variables (based on NOAA's Jet)
#LD_LIBRARY_PATH="/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:${LD_LIBRARY_PATH}"


echo "MSG: Submitting jobs for POLAR module."


# Define important GPLOT directories
NMLIST_DIR="${GPLOT_DIR}/nmlist/"
BATCH_DIR="${GPLOT_DIR}/batch/"
PYTHON_DIR="${GPLOT_DIR}/python/"
TBL_DIR="${GPLOT_DIR}/tbl/"

# Get the namelist, could be from command line
if [ ! -z "$1" ];then
    NMLIST="$1"
else
    NMLIST="namelist.input.default"
fi

# Check if the namelist exists. If not, exit.
if [ ! -f ${NMLIST_DIR}${NMLIST} ]; then
    echo "ERROR: Namelist does not exist."
    exit
else
    echo "MSG: Found this namelist --> ${NMLIST_DIR}${NMLIST}."
fi



# Pull important variables from the namelist
DO_POLAR=`sed -n -e 's/^.*DO_POLAR =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
DSOURCE=`sed -n -e 's/^.*DSOURCE =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
EXPT=`sed -n -e 's/^.*EXPT =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
MCODE=`sed -n -e 's/^.*MCODE =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
IS_REAL=`sed -n -e 's/^.*IS_REAL =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
IS_MSTORM=`sed -n -e 's/^.*IS_MSTORM =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
ENSMEM=`sed -n -e 's/^.*ENSMEM =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
IDIR=`sed -n -e 's/^.*IDIR =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
ITAG=`sed -n -e 's/^.*ITAG =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
EXT=`sed -n -e 's/^.*EXT =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
ODIR=`sed -n -e 's/^.*ODIR =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
INIT_HR=`sed -n -e 's/^.*INIT_HR =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
FNL_HR=`sed -n -e 's/^.*FNL_HR =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
DT=`sed -n -e 's/^.*DT =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
IDATE=`sed -n -e 's/^.*IDATE =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
SID=`sed -n -e 's/^.*SID =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
ATCF1_DIR=`sed -n -e 's/^.*ATCF1_DIR =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
ATCF1_TAG=`sed -n -e 's/^.*ATCF1_TAG =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
ATCF2_DIR=`sed -n -e 's/^.*ATCF2_DIR =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
ATCF2_TAG=`sed -n -e 's/^.*ATCF2_TAG =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
#ATCF3_DIR=`sed -n -e 's/^.*ATCF3_DIR =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
#ATCF3_TAG=`sed -n -e 's/^.*ATCF3_TAG =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
FORCE=`sed -n -e 's/^.*FORCE =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
SYS_ENV=`sed -n -e 's/^SYS_ENV =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
CPU_ACCT=`sed -n -e 's/^CPU_ACCT =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
RESOLUTION=`sed -n -e 's/^.*RESOLUTION =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
RMAX=`sed -n -e 's/^.*RMAX =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
LEVS=`sed -n -e 's/^.*LEVS =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
PYTHONFILE=`sed -n -e 's/^.*PYTHONFILE =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`

# Print information
echo "MSG: Found this data source in the namelist      --> $DSOURCE"
echo "MSG: Found this experiment in the namelist       --> $EXPT"
if [ -z "$IDATE" ]; then
    echo "MSG: No cycles defined in the namelist. Will consider all."
else
    echo "MSG: Found these cycles in the namelist          --> $IDATE"
fi
if [ -z "$SID" ]; then
    echo "MSG: No Storm IDs defined in the namelist. Will consider all."
else
    echo "MSG: Found these Storm IDs in the namelist       --> $SID"
fi
if [ "$IS_REAL" == "True" ]; then
    echo "MSG: This is a real-time case."
else
    echo "MSG: This is not a real-time case."
fi
if [ "$IS_MSTORM" == "True" ]; then
    echo "MSG: Data source has been identified as HWRF-B."
fi
echo "MSG: Found this top level input directory in the namelist --> $IDIR"
if [ ! -z "$ITAG" ]; then
    echo "MSG: Considering these input file tag --> $ITAG"
fi
if [ ! -z "$EXT" ]; then
    echo "MSG: Considering these input file extensions --> $EXT"
fi
echo "MSG: Found this top level output directory in the namelist --> $ODIR"
if [ -z "$CPU_ACCT" ]; then
    echo "MSG: Could not find a CPU account in the namelist. Assuming 'hur-aoml'."
    CPU_ACCT="hur-aoml"
fi

# If FORCE is undefined, set it to False.
if [ -z "$FORCE" ]; then
    FORCE="False"
fi

# Get the batch submission mode [SBATCH,BACKGROUND,FOREGROUND]
BATCH_MODE=( `sed -n -e 's/^BATCH_MODE =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//' | tr a-z A-Z` )
if [ -z "$BATCH_MODE" ]; then
    BATCH_MODE="SBATCH"
    echo "MSG: No batch-submission found in the namelist. DEFAULT:   --> ${BATCH_MODE[*]}"
else
    echo "MSG: Found a batch-submission mode in the namelist   --> ${BATCH_MODE[*]}"
fi

# Get a list of forecast lead times
FHRS=( $(seq ${INIT_HR} ${DT} ${FNL_HR} | tr "\n" " ") )
echo "MSG: Will produce graphics for these forecast lead times --> ${FHRS[*]}"

# Get all of the ATCF files so they can be searched later.
# If duplicates exist, keep the final ATCF (ATCF2).
ATCF1_ALL=(`find ${ATCF1_DIR} -type f -name "*${ATCF1_TAG}"`)
ATCF2_ALL+=(`find ${ATCF2_DIR} -type f -name "*${ATCF2_TAG}"`)
for ATCF in ${ATCF2_ALL[@]}; do
    ATCF_BASE=`basename ${ATCF} | cut -d'.' -f-2`
    ATCF1_ALL=( ${ATCF1_ALL[@]/*$ATCF_BASE*/} )
done
ATCF_ALL=("${ATCF2_ALL[@]}" "${ATCF1_ALL[@]}")


# Determine if this experiment has ensemble members
# Deterministic forecasts will have ENSMEM=0 in the namelist
if [ $ENSMEM -eq 0 ] || [ -z $ENSMEM ]; then
    IS_ENS="False"
    ENSIDS=0
else
    IS_ENS="True"
    ENSIDS=`seq 1 $ENSMEM`  # SKIP MEM 00, Start from 01

fi

# Define other important variables
BATCHFILE1="batch_polar.generic.sh"
BATCHFILE2="batch_polar.${EXPT}.sh"


# Some housekeeping
cp ${BATCH_DIR}${BATCHFILE1} ${BATCH_DIR}${BATCHFILE2}
chmod +x ${BATCH_DIR}${BATCHFILE2}


# Find output files from which graphics should be created
if [ -z "$IDATE" ]; then
    CYCLES=( `ls -rd ${IDIR}/*[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]*/ | xargs -n 1 basename | tr "\n" " "` )
else
    CYCLES=( "${IDATE[@]}" )
fi
echo "MSG: Found these cycles: ${CYCLES[*]}"
echo ""


# If PYTHONFILE is empty, set it to a default script.
if [ -z "$PYTHONFILE" ]; then
    PYTHONFILE="make_rz_plots.py"
fi


# Define the maximum number of batch submissions.
# This is a safeguard to avoid overloading the batch scheduler.
MAXCOUNT=25





#############################################################
# 2. CALL GPLOT SHIPS                                       #
#    This script is responsible for creating graphics based #
#    on SHIPS fields and other relevant predictors.         #
#############################################################
if [ "${DO_POLAR}" = "True" ]; then
    DOMAIN="polar"
    TIER="Tier1"
    SC="True"
    ATCF_REQD="True"


    # Define the batch submission counter.
    N=0


    ##################################
    # LOOP OVER ALL AVAILABLE CYCLES #
    ##################################
    for CYCLE in ${CYCLES[@]}; do

        # Only retain the numbers for the cycle
        # Parse the prefix (e.g., gfs.) if it exists.
        CYCLE=`echo "$CYCLE" | sed 's/\([A-Za-z.]*\)\([0-9]*\)/\2/'`
        CPREFIX=`echo "$CYCLE" | sed 's/\([A-Za-z.]*\)\([0-9]*\)/\1/'`

        echo "MSG: Current cycle --> $CYCLE"

        # Get the cycle prefix from a table and define CYCLE_STR
        # CYCLE_STR should be used in file paths.
        if [ -z "$CPREFIX" ]; then
            CPREFIX=`awk -v DSRC=$DSOURCE '($1 == DSRC) { print $2 }' ${TBL_DIR}CyclePrefix.dat`
        fi
        CYCLE_STR="${CPREFIX}${CYCLE}"


        # Find the ATCFs for the current CYCLE.
        # It will be blank if no ATCFs are found.
        CYCLE_ATCF=`printf '%s\n' ${ATCF_ALL[@]} | grep "${CYCLE}"`


        # First, try to get STORMS from the namelist (SID)
        STORMS=()
        if [ ! -z "$SID" ]; then
            STORMS+=("${SID[@]}")
        fi

        # Second, try to get STORMS from the ATCF files
        if [ -z "$STORMS" ]; then
            for ATCF in ${CYCLE_ATCF}; do
                STORMS+=(`basename $ATCF | cut -d'.' -f1 | rev | cut -c1-3 | rev | tr '[:lower:]' '[:upper:]'`)
            done
        fi

        # Third, try to get STORMS from the HWRF file path.
        # This is hard-coded and might not work.
        if [ -z "$STORMS" ]; then
            if [ ! -z $(ls -d ${IDIR}${CYCLE}/[0-9][0-9][A-Z]/ 2>/dev/null) ]; then
                STORMS+=(`ls -d ${IDIR}${CYCLE}/[0-9][0-9][A-Z]/ | xargs -n 1 basename`)
            fi
        fi

        # Fourth, if STORMS is still undefined, then set it to "NONE"
        # Large-scale graphics may still proceed.
        # Storm-centered graphics will be skipped.
        if [ -z "$STORMS" ]; then
            STORMS+=("NONE")
        fi


        # Fifth, append Fake Storm (00L) is IS_MSTORM=True and if other storms
        # were found, i.e., STORMS != NONE
        if [ "$IS_MSTORM" == "True" ] && [ "$STORMS" != "NONE" ]; then
            STORMS+=("00L")
        fi


        ####################
        # LOOP OVER STORMS #
        ####################
        for STORM in ${STORMS[@]}; do
            echo "MSG: Current storm --> $STORM"

            # Find the forecast hours from the ATCF for thie particular storm
            STORM_ATCF=( `printf '%s\n' ${CYCLE_ATCF[@]} | grep -i "${STORM,,}"` )
            echo "MSG: The ATCF for this storm --> $STORM_ATCF"
            if [ -z "${STORM_ATCF[*]}" ]; then
                echo "WARNING: No ATCF found for this storm. This might be OK."
            else
                ATCF_FHRS=( `awk -F',' '{print $6}' ${STORM_ATCF[0]} | sort -u | sort -k1,1n | sed 's/^0*//' | sed -e 's/^[[:space:]]*//'` )
            fi

            #Keep only the ATCF forecast hours that match namelist options: INIT_HR,FNL_HR,DT
            NEW_ATCF_FHRS=()
            for FHR in ${ATCF_FHRS[@]}; do
                if [ $((10#$FHR)) -ge $INIT_HR ] && [ $((10#$FHR)) -le $FNL_HR ] && [ $((10#$FHR % $DT)) -eq 0 ]; then
                    NEW_ATCF_FHRS+=( $FHR )
                fi
            done
            ATCF_FHRS=("${NEW_ATCF_FHRS[@]}")


            # Set the STORMTAG for file names
            STORMTAG=".${STORM^^}"


            #########################
            # LOOP OVER MAP DOMAINS #
            #########################
            for DMN in ${DOMAIN}; do
                echo "MSG: Current domain --> $DMN"

                # Create full output path
                # Make the directory in case it doesn't already exist
                ODIR_FULL="${ODIR}${EXPT}/${CYCLE}/${DMN}/"
                echo "MSG: Output directory --> $ODIR_FULL"
                mkdir -p ${ODIR_FULL}


                # Get nest information from GPLOT table
                NEST=`awk -v DMN=$DMN '($1 == DMN) { print $2 }' ${TBL_DIR}DomainInfo.dat`
                if [ -z "$NEST" ]; then
                    echo "MSG: Domain $DMN not found in ${TBL_DIR}DomainInfo.dat."
                    echo "MSG: Assuming NEST=1."
                    NEST=1
                fi

                # Get file prefix information from table or namelist
                if [ -z "$ITAG" ]; then
                    FPREFIX=`awk -v DSRC=$DSOURCE -v N=$NEST '($1 == DSRC) { print $(1+N) }' ${TBL_DIR}FilePrefix.dat`
                else
                    FPREFIX="$ITAG"
                fi
                if [ -z "$FPREFIX" ]; then
                    echo "ERROR: File prefix not found for $DSOURCE."
                    echo "ERROR: Please add your DSOURCE to ${TBL_DIR}FilePrefix.dat."
                    echo "ERROR: Or define ITAG in the namelist."
                    exit
                fi

                # Get file hour string information from table or namelist
                if [ -z "$FHRSTR" ]; then
                    FHRSTR=`awk -v DSRC=$DSOURCE '($1 == DSRC) { print $2 }' ${TBL_DIR}FileTimeFormat.dat`
                else
                    FHRSTR="$FHRSTR"
                fi
                if [ -z "$FHRSTR" ]; then
                    echo "ERROR: File hour string not found for $DSOURCE."
                    echo "ERROR: Please add your DSOURCE to ${TBL_DIR}FileTimeFormat.dat."
                    echo "ERROR: Or define FHRSTR in the namelist."
                    exit
                fi

                # Get file hour format information from table or namelist
                FHRFMT=`sed -n -e 's/^.*FMT_HR =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
                if [ -z "$FHRFMT" ]; then
                    FHRFMT="%0`awk -v DSRC=$DSOURCE '($1 == DSRC) { print $3 }' ${TBL_DIR}FileTimeFormat.dat`d"
                else
                    FHRFMT="%0${FHRFMT}d"
                fi
                if [ -z "$FHRFMT" ]; then
                    echo "ERROR: File hour format not found for $DSOURCE."
                    echo "ERROR: Please add your DSOURCE to ${TBL_DIR}FileTimeFormat.dat."
                    echo "ERROR: Or define FHRFMT in the namelist."
                    exit
                fi

                # Get file extension information from table or namelist
                if [ -z "$EXT" ]; then
                    FSUFFIX=`awk -v DSRC=$DSOURCE '($1 == DSRC) { print $2 }' ${TBL_DIR}FileSuffix.dat`
                else
                    FSUFFIX="$EXT"
                fi
                if [ -z "$FSUFFIX" ]; then
                    echo "ERROR: File suffix not found for $DSOURCE."
                    echo "ERROR: Please add your DSOURCE to ${TBL_DIR}FileSuffix.dat."
                    echo "ERROR: Or define EXT in the namelist."
                    exit
                fi
                if [ "$FSUFFIX" == "NONE" ]; then
                    FSUFFIX=""
                fi

                # Run some tests on the ATCF for thie storm.
                # If domain is storm-centerd and ATCF is required, then ATCF must
                # be present and contain forecast hours
                if [ -z ${STORM_ATCF} ]; then
                    echo "ERROR: Domain is storm-centered and ATCF files are required."
                    echo "ERROR: But, found no matching ATCF files. So, nothing to do."
                    echo ""
                    continue
                elif [ -z "${ATCF_FHRS[*]}" ]; then
                    echo "ERROR: Domain is storm-centered and ATCF files are required."
                    echo "ERROR: ATCF was found for this storm, but no forecast hours were found."
                    echo ""
                    continue
                fi

                # Create a text file with ATCFs for this cycle in the output directory
                if [ -z "${CYCLE_ATCF}" ]; then
                    echo "NONE" > ${ODIR_FULL}ATCF_FILES.dat
                else
                    if [ -f "${ODIR_FULL}ATCF_FILES.dat" ]; then
                        rm -f ${ODIR_FULL}ATCF_FILES.dat
                    fi
                    for ATCF in ${CYCLE_ATCF}; do
                        echo "$ATCF" >> ${ODIR_FULL}ATCF_FILES.dat
                    done
                fi


                ###########################
                # LOOP OVER GRAPHIC TIERS #
                ###########################
                for TR in ${TIER}; do
                    echo "MSG: Current tier --> $TR"


                    ##########################
                    # LOOP OVER ENSEMBLE IDS #
                    ##########################
                    # Could be 1 iteration if no ensemble
                    for ID in $ENSIDS; do

                        # Set 2-digit variable ENSID
                        if [ "${IS_ENS}" == "False" ]; then
                            ENSID=""
                            ENSIDTAG=""
                        else
                            ENSID=$(printf "%02d\n" $ID)
                            ENSIDTAG=".E${ENSID}"
                            echo "MSG: Current ensemble member --> $ENSID"
                        fi

                        # Create a list of IDIR subdirectory options
                        IDIR_OPTS=("" "${EXPT}/com/${CYCLE_STR}/${STORM}/" "${EXPT}/com/${CYCLE_STR}/" "${EXPT}/com/" \
                                   "${EXPT}/" "${CYCLE_STR}/${STORM}/" "${CYCLE_STR}/" "${STORM}/" "${EXPT}/${CYCLE_STR}/${STORM}/" \
                                   "${EXPT}/${CYCLE_STR}/" "${EXPT}/com/${CYCLE_STR}/${STORM}/${ENSID}/" "${EXPT}/com/${CYCLE_STR}/${ENSID}/" \
                                   "${EXPT}/com/${ENSID}/" "${EXPT}/${ENSID}/" "${CYCLE_STR}/${STORM}/${ENSID}/" \
                                   "${ENSID}/${CYCLE_STR}/${STORM}/" "${CYCLE_STR}/${ENSID}/${STORM}/" "${CYCLE_STR}/${ENSID}/" \
                                   "${ENSID}/${CYCLE_STR}/" "${STORM_STR}/${ENSID}/" "${ENSID}/${STORM}/" "${EXPT}/com/${ENSID}/${CYCLE_STR}/" \
                                   "${EXPT}/${ENSID}/com/${CYCLE_STR}/${STORM}/" "${EXPT}/${ENSID}/com/${CYCLE_STR}/" "${EXPT}/${ENSID}/com/" \
                                   "${ENSID}/com/${CYCLE_STR}/${STORM}/" "com/${CYCLE_STR}/${STORM}/" "${ENSID}/" "${CYCLE_STR}/00L/" \
                                   "com/${CYCLE_STR}/00L/" "${EXPT}/com/${CYCLE_STR}/00L")

                        # Find all input files that match: FPREFIX,FHRSTR,FHRFMT,FSUFFIX
                        # If a match is found, write lead time to a data file "AllForecastHours"
                        IFILES=()
                        F=0
                        IFHRS=()
                        while [ -z "$IFILES" ]; do
                            IDIR_FULL="${IDIR}${IDIR_OPTS[$F]}"

                            # If the input directory doesn't exist, continue to the next option
                            if [ ! -d ${IDIR_FULL} ]; then
                                ((F=F+1))
                                continue
                            fi

                            # Get the right list of lead times
                            if [ "$SC" == "True" ] && [ "$ATCF_REQD" == "True" ]; then
                                FILE_FHRS=${ATCF_FHRS[@]}
                            else
                                FILE_FHRS=${FHRS[@]}
                            fi

                            # Loop over all lead times to find available files.
                            for FHR in ${FILE_FHRS[@]}; do
                                if [ -z "$FSUFFIX" ]; then
                                    FILE_SEARCH="${IDIR_FULL}*${FPREFIX}*${FHRSTR}$(printf "${FHRFMT}\n" $((10#$FHR)))"
                                else
                                    FILE_SEARCH="${IDIR_FULL}*${FPREFIX}*${FHRSTR}$(printf "${FHRFMT}\n" $((10#$FHR)))*${FSUFFIX}"
                                fi
                                if [ ! -z $(ls ${FILE_SEARCH} 2>/dev/null) ]; then
                                    IFILES+=(`ls ${FILE_SEARCH} 2>/dev/null`)
                                    IFHRS+=( ${FHR} )
                                fi
                            done

                            # Increase the counter to search the next input directory option
                            ((F=F+1))

                            # Break the loop if all input directory options have been searched
                            if [ $F -gt ${#IDIR_OPTS[@]} ]; then
                                echo "ERROR: No files were found. Try fixing IDIR in the namelist."
                                break
                                #exit
                            fi
                        done
                        if [ -z "${IFILES[*]}" ]; then
                            echo "WARNING: Nothing to do here. Moving on to the next case."
                            echo ""
                            continue
                        fi
                        echo "MSG: Full input directory --> ${IDIR_FULL}"


                        # Define the file that contains a list of plotted files (PLOTTED_FILE)
                        # Define the file that contains the status (STATUS_FILE)
                        PLOTTED_FILE="${ODIR_FULL}PlottedFiles.${DMN}.${TR}${STORMTAG}.log"
                        STATUS_FILE="${ODIR_FULL}status.${DMN}.${TR}${STORMTAG}.log"


                        # Get the list of plotted files for this case
                        CASE_PLOTTED=(`cat ${PLOTTED_FILE} 2>/dev/null`)
                        

                        # Get the status for this case
                        CASE_STATUS=`cat ${STATUS_FILE} 2>/dev/null`


                        # Print some information
                        if [ "$FORCE" == "False" ]; then
                            echo "MSG: Using this file of processed files --> ${PLOTTED_FILE}"
                            echo "MSG: Found ${#CASE_PLOTTED[@]} processed files. These will be skipped."
                            echo "MSG: To manually force reprocessing of all files, delete this file."
                        else
                            echo "MSG: Graphic production will be forced."
                            echo "MSG: Deleting this file of processed files --> ${PLOTTED_FILE}"
                            rm -f ${PLOTTED_FILE}
                            CASE_PLOTTED=()
                            CASE_STATUS="force"
                        fi
                        echo "MSG: Using this status file --> ${STATUS_FILE}"
                        echo "MSG: Found this status --> ${CASE_STATUS}"


                        # Loop through IFILES and retain only valid entries
                        i=0
                        BROKEN_LINK="NO"
                        for FILE in ${IFILES[@]}; do

                            # Remove files that are broken links
                            if [ ! -e "$FILE" ]; then
                                unset 'IFILES[$i]'
                                unset 'IFHRS[$i]'
                                BROKEN_LINK="YES"
                            fi

                            # Remove files that have already been processed (CASE_PLOTTED)
                            # from the list in input files (IFILES).
                            # If production is being forced, then CASE_PLOTTED will be empty.
                            # Only files that have not been modified in over 30 min are
                            # removed from the list.
                            if [ ! -z "${CASE_PLOTTED[*]}" ]; then
                                CFILE=$(printf -- '%s\n' "${CASE_PLOTTED[@]}" | grep "$FILE")
                                if [[ -n "$CFILE" ]]; then
                                    test=$(find ${IDIR_FULL} -name "`basename $CFILE`" -mmin +30 2>/dev/null)
                                    if [[ -n ${test} ]]; then
                                        unset 'IFILES[$i]'
                                        unset 'IFHRS[$i]'
                                    fi
                                fi
                            fi
                            ((i++))
                        done
                        if [ -z "${IFILES[*]}" ]; then
                            if [ "$BROKEN_LINK" == "YES" ]; then
                                echo "WARNING: No files to process, but broken links were detected."
                                echo "WARNING: Marking status as broken. It should be double-checked."
                                echo "broken" > ${STATUS_FILE}
                            else
                                echo "MSG: All available input files have been processed."
                                echo "MSG: Marking status as complete."
                                echo "complete" > ${STATUS_FILE}
                            fi
                            echo ""
                            continue
                        fi


                        # Remove any blank entries from IFILES & IFHRS
                        for i in "${!IFILES[@]}"; do
                            NEW_IFILES+=( "${IFILES[i]}" )
                            NEW_IFHRS+=( "${IFHRS[i]}" )
                        done
                        IFILES=("${NEW_IFILES[@]}")
                        IFHRS=("${NEW_IFHRS[@]}")
                        unset NEW_IFILES
                        unset NEW_IFHRS


                        # Check the status and update it if necessary.
                        # This logic will allow work to start on this case
                        # or will move on to the next case.
                        if [ "$CASE_STATUS" == "force" ]; then
                            echo "MSG: Forcing production. Ignoring status file."
                            echo "start" > ${STATUS_FILE}
                        elif [ "$CASE_STATUS" == "complete" ]; then
                            if [ -z "${IFILES[*]}" ]; then
                                echo "MSG: Status suggests this case has been completed."
                                echo "MSG: Nothing to do here. Moving on to next case."
                                echo ""
                                continue
                            else
                                echo "MSG: Status says complete, but unprocessed files were found."
                                echo "MSG: Deleting the status for a restart."
                                echo "start" > ${STATUS_FILE}
                            fi
                        elif [ "$CASE_STATUS" == "working" ]; then
                            echo "MSG: Status suggests this case is being worked on."
                            echo "MSG: Changing the status to 'update request 1'."
                            echo ""
                            echo "update request 1" > ${STATUS_FILE}
                            continue
                        elif [ "$CASE_STATUS" == "update request 1" ]; then
                            echo "MSG: Status suggests this case needs to be updated."
                            echo "MSG: Changing the status to 'update request 2'."
                            echo ""
                            echo "update request 2" > ${STATUS_FILE}
                            continue
                        elif [ "$CASE_STATUS" == "update request 2" ] || [ "$CASE_STATUS" == "failed" ]; then
                            echo "MSG: Status suggests this case has stalled/failed."
                            echo "MSG: Deleting the status for a restart."
                            echo "start" > ${STATUS_FILE}
                        elif [ "$CASE_STATUS" == "incomplete" ]; then
                            echo "MSG: Status suggests that this case is incomplete."
                            echo "MSG: Will try to find new input files."
                            echo "start" > ${STATUS_FILE}
                        elif [ "$CASE_STATUS" == "broken" ]; then
                            echo "MSG: Status suggests that this case is broken."
                            echo "MSG: It will require manual resubmission."
                            echo ""
                            continue
                        elif [ -z "$CASE_STATUS" ]; then
                            echo "MSG: Status not found. Treating this as a new case."
                            echo "start" > ${STATUS_FILE}
                        else
                            echo "MSG: Unknown status (${CASE_STATUS}). Treating this as a new case."
                            echo "start" > ${STATUS_FILE}
                        fi


                        # If IFILES is empty, then all available files have been plotted
                        # Then, skip to the next and mark status=complete
                        if [ -z "${IFILES[*]}" ]; then
                        #if [ -z "$(echo -e "${IFILES[*]}" | tr -d '[:space:]')" ]; then
                        #if [ -z $(echo "${IFILES[*]}" | sed -e 's/^[[:space:]]*//') ]; then
                            if ! find "${STORM_ATCF[0]}" -mmin +60 >/dev/null ; then
                                echo "MSG: All available files have already been processed."
                                echo "MSG: However, ATCF is not old enough to complete."
                                echo "MSG: More files might become available."
                                echo "MSG: Moving on to next case."
                                echo "incomplete" > ${STATUS_FILE}
                                echo ""
                            else
                                echo "MSG: All available files have already been processed."
                                echo "MSG: Marking the status as complete."
                                echo "MSG: Moving on to next case."
                                echo ""
                                echo "complete" > ${STATUS_FILE}
                            fi
                            continue
                        fi


                        # Write existing IFILES out to UNPLOTTED_FNAME log for use in plotting scripts
                        UNPLOTTED_FILE="${ODIR_FULL}UnplottedFiles.${DMN}.${TR}${STORMTAG}.log"
                        FHR_FILE="${ODIR_FULL}AllForecastHours.${DMN}.${TR}${STORMTAG}.log"
                        if [ -f "${UNPLOTTED_FILE}" ]; then
                            rm -f "${UNPLOTTED_FILE}"
                        fi
                        if [ -f "${FHR_FILE}" ]; then
                            rm -f "${FHR_FILE}"
                        fi
                        i=0
                        for LINE in ${IFILES[@]}; do
                            echo "${LINE}" >> ${UNPLOTTED_FILE}
                            echo "${IFHRS[$i]}" >> ${FHR_FILE}
                            ((i++))
                        done
                        echo "MSG: Using this file of unplotted files --> ${UNPLOTTED_FILE}"
                        echo "MSG: Using this file of unplotted lead times --> ${FHR_FILE}"


                        # Choose a proper wallclock time for this job based on the number of files.
                        if [ "${#IFILES[@]}" -le "15" ]; then
                            RUNTIME="01:59:59"
                        elif [ "${#IFILES[@]}" -le "30" ]; then
                            RUNTIME="02:29:59"
                        elif [ "${#IFILES[@]}" -le "45" ]; then
                            RUNTIME="02:59:59"
                        elif [ "${#IFILES[@]}" -le "60" ]; then
                            RUNTIME="03:29:59"
                        elif [ "${#IFILES[@]}" -le "75" ]; then
                            RUNTIME="03:59:59"
                        elif [ "${#IFILES[@]}" -le "90" ]; then
                            RUNTIME="04:29:59"
                        elif [ "${#IFILES[@]}" -le "105" ]; then
                            RUNTIME="04:59:59"
                        elif [ "${#IFILES[@]}" -le "120" ]; then
                            RUNTIME="05:29:59"
                        elif [ "${#IFILES[@]}" -le "135" ]; then
                            RUNTIME="05:59:59"
                        else
                            RUNTIME="06:29:59"
                        fi

                        # Check if a similar job is already submitted
                        echo "MSG: The batch file --> ${BATCH_DIR}${BATCHFILE2}"
                        if [ "$BATCH_MODE" == "FOREGROUND" ]; then
                            JOB_TEST=""
                        elif [ "$BATCH_MODE" == "BACKGROUND" ]; then
                            JOB_TEST=""
                        else
                            JOB_NAME="GPLOT.${EXPT}.${CYCLE}${ENSIDTAG}.${DMN}${STORMTAG}.${TR}"
                            JOB_TEST=`/apps/slurm/default/bin/squeue -u $USER -o %.100j | /bin/grep "${JOB_NAME}"`
                        fi

                        # Change options in the batch submission script.
                        if [ -z "$JOB_TEST" ]; then
                            LOG_DIR="$ODIR_FULL"
                            LOGFILE="GPLOT_Polar.${EXPT}.${CYCLE}${ENSIDTAG}.${DMN}${STORMTAG}.${TR}.log"
                            perl -pi -e "s/#SBATCH --account=.*/#SBATCH --account=${CPU_ACCT}/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/#SBATCH --job-name=.*/#SBATCH --job-name=\"GPLOT.${EXPT}.${CYCLE}${ENSIDTAG}.${DMN}${STORMTAG}.${TR}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/#SBATCH --output=.*/#SBATCH --output=\"${LOG_DIR////\/}GPLOT_Polar.${EXPT}.${CYCLE}${ENSIDTAG}.${DMN}${STORMTAG}.${TR}.out\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/#SBATCH --error=.*/#SBATCH --error=\"${LOG_DIR////\/}GPLOT_Polar.${EXPT}.${CYCLE}${ENSIDTAG}.${DMN}${STORMTAG}.${TR}.err\"/g" ${BATCH_DIR}${BATCHFILE2}
                            #perl -pi -e "s/#SBATCH --nodes=.*/#SBATCH --nodes=1/g" ${BATCH_DIR}${BATCHFILE2}
                            #perl -pi -e "s/#SBATCH --ntasks-per-node=.*/#SBATCH --ntasks-per-node=12/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/#SBATCH --mem=.*/#SBATCH --mem=32G/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/#SBATCH --time=.*/#SBATCH --time=${RUNTIME}/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^PYTHONDIR=.*/PYTHONDIR=\"${PYTHON_DIR////\/}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^PYTHONFILE=.*/PYTHONFILE=\"${PYTHONFILE}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^LOGDIR=.*/LOGDIR=\"${LOG_DIR////\/}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^LOGFILE=.*/LOGFILE=\"${LOGFILE}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^NMLIST=.*/NMLIST=\"${NMLIST}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^DOMAIN=.*/DOMAIN=\"${DMN}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^TIER=.*/TIER=\"${TR}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^ENSID=.*/ENSID=\"${ENSID}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^IDATE=.*/IDATE=\"${CYCLE}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^SID=.*/SID=\"${STORM}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^FORCE=.*/FORCE=\"${FORCE}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^RESOLUTION=.*/RESOLUTION=\"${RESOLUTION}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^RMAX=.*/RMAX=\"${RMAX}\"/g" ${BATCH_DIR}${BATCHFILE2}
                            perl -pi -e "s/^LEVS=.*/LEVS=\"${LEVS}\"/g" ${BATCH_DIR}${BATCHFILE2}

                            if [ "$SYS_ENV" == "JET" ]; then
                                perl -pi -e "s/#SBATCH --partition=.*/#SBATCH --partition=tjet,ujet,sjet,vjet,xjet,kjet/g" ${BATCH_DIR}${BATCHFILE2}
                                perl -pi -e "s/#SBATCH --qos=.*/#SBATCH --qos=batch/g" ${BATCH_DIR}${BATCHFILE2}
                                perl -pi -e "s/#SBATCH --ntasks-per-node=.*/#SBATCH --ntasks-per-node=12/g" ${BATCH_DIR}${BATCHFILE2}
                                perl -pi -e "s/#SBATCH --mem=.*/#SBATCH --mem=32G/g" ${BATCH_DIR}${BATCHFILE2}
                            elif [ "$SYS_ENV" == "HERA" ]; then
                                perl -pi -e "s/#SBATCH --partition=.*/#SBATCH --partition=hera/g" ${BATCH_DIR}${BATCHFILE2}
                                #perl -pi -e "s/#SBATCH --qos=.*/#SBATCH --qos=windfall/g" ${BATCH_DIR}${BATCHFILE2}
                                perl -pi -e "s/#SBATCH --ntasks-per-node=.*/#SBATCH --ntasks-per-node=20/g" ${BATCH_DIR}${BATCHFILE2}
                                perl -pi -e "s/#SBATCH --mem=.*/#SBATCH --mem=64G/g" ${BATCH_DIR}${BATCHFILE2}
                            fi

                            # Call the batch job
                            echo "MSG: Executing GPLOT batch job submission. BATCH_MODE ${BATCH_MODE}"			
                            if [ "$BATCH_MODE" == "FOREGROUND" ]; then
                                ${BATCH_DIR}${BATCHFILE2}
                            elif [ "$BATCH_MODE" == "BACKGROUND" ]; then
                                ${BATCH_DIR}${BATCHFILE2} &
                            else
                                sbatch ${BATCH_DIR}${BATCHFILE2}
                            fi

                            # Increase the batch job counter and check if we're over the limit.
                            ((N++))
                            if [ "$N" -ge "$MAXCOUNT" ]; then
                                echo "MSG: Maximum number of batch submissions has been reached."
                                echo "MSG: Further jobs will be submitted later."
                                echo "MSG: spawn_ships.sh completed at `date`"
                                exit
                            fi
                        else
                            echo "MSG: Found matching GPLOT batch job. Skipping submission."
                        fi #if [ -z "$JOB_TEST" ]; then else

                    done #end of ID loop
                done #end of TR loop
            done #end of DMN loop
        done #end of STORM loop
    done #end of CYCLE loop
fi #end of DO_POLAR

wait

echo "$?"
echo "COMPLETE!"
