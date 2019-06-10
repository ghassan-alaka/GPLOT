#!/bin/sh --login
#set -x


# Load modules (based on NOAA's Jet)
#module load slurm

# Define critical environmental variables (based on NOAA's Jet)
LD_LIBRARY_PATH="/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:${LD_LIBRARY_PATH}"


# Define important GPLOT directories
NMLIST_DIR="${GPLOT_DIR}/nmlist/"
BATCH_DIR="${GPLOT_DIR}/batch/"
NCL_DIR="${GPLOT_DIR}/ncl/"
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
DO_MAPS=`sed -n -e 's/^.*DO_MAPS =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
DO_STATS=`sed -n -e 's/^.*DO_STATS =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
DO_SHIPS=`sed -n -e 's/^.*DO_SHIPS =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
DSOURCE=`sed -n -e 's/^.*DSOURCE =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
EXPT=`sed -n -e 's/^.*EXPT =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
MCODE=`sed -n -e 's/^.*MCODE =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
IS_REAL=`sed -n -e 's/^.*IS_REAL =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
IS_HWRFB=`sed -n -e 's/^.*IS_HWRFB =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`
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
if [ "$IS_HWRFB" == "True" ]; then
    echo "MSG: Data source has been identified as HWRF-B."
fi
echo "MSG: Found this top level input directory in the namelist --> $IDIR"
if [ ! -z "$ITAG" ]; then
    echo "MSG: Condering these input file tag --> $ITAG"
fi
if [ ! -z "$EXT" ]; then
    echo "MSG: Considering these input file extensions --> $EXT"
fi
echo "MSG: Found this top level output directory in the namelist --> $ODIR"


# If FORCE is undefined, set it to False.
if [ -z "$FORCE" ]; then
    FORCE="False"
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
BATCHFILE1="batch_stats.generic.sh"
BATCHFILE2="batch_stats.${EXPT}.sh"
CTIME=`date +"%Y%m%d%H_%M"`
LOG_DIR=`sed -n -e 's/^.*ODIR =\s//p' ${NMLIST_DIR}${NMLIST} | sed 's/^\t*//'`"${EXPT}/log/${CTIME}/"

# Some housekeeping
mkdir -p ${LOG_DIR}
cp ${BATCH_DIR}${BATCHFILE1} ${BATCH_DIR}${BATCHFILE2}


# Find output files from which graphics should be created
#if [ -z "$IDATE" ]; then
#    CYCLES=`ls -d ${IDIR}*/ | xargs -n 1 basename | tr "\n" " "`
#else
    CYCLES="${IDATE[@]}"
#fi
echo "MSG: Found these cycles: ${CYCLES[*]}"
echo ""


####################################################
# 3. CALL GPLOT STATS                              #
#    This script is responsible for creating track #
#    & intensity guidance/verification.            #
####################################################
if [ "${DO_STATS}" = "True" ]; then
    echo "MSG: Submitting jobs for STATS."
    NCLFILE="GPLOT_stats.ncl"


    # Set the counter to limit submission to 50 jobs
    N=0

    ######################################
    # LOOP OVER ALL AVAILABLE ATCF FILES #
    ######################################
    for ATCF in "${ATCF_ALL[@]}"; do
        echo "MSG: Working on this ATCF:"
        echo "MSG: $ATCF"
        

        # Split up the ATCF file name to find the cycle and storm.
        # Skip to the next ATCF if cycle or storm is not found.
        ATCF_BASE=`basename "$ATCF"`
        IFS='.' read -r -a ATCF_ELM <<< "$ATCF_BASE"
        CYCLE=""
        STORM=""
        for A in "${ATCF_ELM[@]}"; do
            if [[ "$A" =~ ^[0-9]{10}$ ]]; then
                CYCLE="$A"
                #echo "Found Date --> $CYCLE"
            fi
            if [[ `echo "$A" | rev | cut -c1-3 | rev` =~ ^[0-9]{2}[a-z]$ ]]; then
                STORM=`echo "${A^^}" | rev | cut -c1-3 | rev`
                SIDLONG=`echo "${A,,}"`
                #echo "Found Storm --> $STORM"
            fi
            if [ ! -z "$CYCLE" ] && [ ! -z "$STORM" ]; then
                break
            fi
        done
        if [ -z "$CYCLE" ]; then
            echo "WARNING: Could not find the cycle from the ATCF file name."
            echo "WARNING: To process it, please add the cycle to the ATCF file name."
            echo "WARNING: Skipping this ATCF because cycle not found."
            continue
        fi
        if [ -z "$STORM" ]; then
            echo "WARNING: Could not find the storm ID from the ATCF file name."
            echo "WARNING: To process it, please add the storm ID to the ATCF file name."
            echo "WARNING: Skipping this ATCF because storm ID not found."
            continue
        fi

        # Process this ATCF only if the cycle is found in IDATE
        # or if IDATE is empty.
        if [ ! -z "${CYCLES[@]}" ]; then
            CYCLE_FOUND="False"
            for D in "${CYCLES[@]}"; do
                if [ "$D" == "$CYCLE" ]; then
                    CYCLE_FOUND="True"
                fi
            done
        else
            CYCLE_FOUND="True"
        fi

        # If the cycle is not found in IDATE, then skip to next ATCF
        if [ "$CYCLE_FOUND" == "False" ]; then
            echo "WARNING: Skipping this ATCF because namelist cycle (IDATE) not found."
            continue
        fi

        # Process this ATCF only if the cycle is found in SID
        # or if SID is empty.
        if [ ! -z "$SID" ]; then
            STORM_FOUND="False"
            for S in "${SID[@]}"; do
                if [ "$S" == "$STORM" ]; then
                    STORM_FOUND="True"
                fi
            done
        else
            STORM_FOUND="True"
        fi

        # If the storm is not found in SID, then skip to next ATCF
        if [ "$STORM_FOUND" == "False" ]; then
            echo "WARNING: Skipping this ATCF because namelist storm ID (SID) not found."
            continue
        fi

        echo "$CYCLE   $STORM"


        # Get the status for this case
        STATUS_FILE="${ODIR_FULL}status.${SIDLONG}.log"
        CASE_STATUS=`cat ${STATUS_FILE} 2>/dev/null`


        # Print some information
        echo "MSG: Using this status file --> ${STATUS_FILE}"
        echo "MSG: Found this status --> ${CASE_STATUS}"


        # Check the status and update it if necessary.
        # This logic will allow work to start on this case
        # or will move on to the next case.
        if [ "$FORCE" == "True" ]; then
            echo "MSG: Forcing production. Will ignore status."
            echo "start" > ${STATUS_FILE}
        else
            if [ "$CASE_STATUS" == "complete" ]; then
                echo "MSG: Status suggests this case has been completed."
                echo "MSG: Nothing to do here. Moving on to next case."
                echo ""
                continue
            elif [ "$CASE_STATUS" == "working" ]; then
                echo "MSG: Status suggests this case is being worked on."
                echo "MSG: Changing the status to 'update request 1'."
                echo ""
                echo "update request 1" > ${STATUS_FILE}
                continue
            elif [ "$CASE_STATUS" == "update request 1" ]; then
                echo "MSG: Status suggests this case has stalled/failed."
                echo "MSG: Deleting the status for a restart."
                echo "start" > ${STATUS_FILE}
            elif [ "$CASE_STATUS" == "incomplete" ]; then
                echo "MSG: Status suggests that this case is incomplete."
                echo "MSG: Will try to find new input files."
                echo "start" > ${STATUS_FILE}
            elif [ -z "$CASE_STATUS" ]; then
                echo "MSG: Status not found. Treating this as a new case."
                echo "start" > ${STATUS_FILE}
            else
                echo "MSG: Unknown status (${CASE_STATUS}). Treating this as a new case."
                echo "start" > ${STATUS_FILE}
            fi
        fi


        # Create full output path.
        # Make the directory in case it doesn't already exist.
        ODIR_FULL="${ODIR}${EXPT}/${CYCLE}/multi_model/"
        echo "MSG: Output directory --> $ODIR_FULL"
        mkdir -p ${ODIR_FULL}

        # Write the ATCF file name to a text file to be accessed later.
        # Be sure to delete duplicate entries.
        echo "$ATCF" >> ${ODIR_FULL}ATCF_FILES.dat
        sort -u ${ODIR_FULL}ATCF_FILES.dat > ${ODIR_FULL}ATCF_FILES.dat.TMP
        mv ${ODIR_FULL}ATCF_FILES.dat.TMP ${ODIR_FULL}ATCF_FILES.dat


	# Call the batch job
        LOGFILE="GPLOT_Stats.${EXPT}.${MCODE}.${CYCLE}.${STORM}.log"
        perl -pi -e "s/#SBATCH --job-name=.*/#SBATCH --job-name=\"GPLOT.${EXPT}.${MCODE}.${CYCLE}.${STORM}\"/g" ${BATCH_DIR}${BATCHFILE2}
        perl -pi -e "s/#SBATCH --output=.*/#SBATCH --output=\"${LOG_DIR////\/}GPLOT_Stats.${EXPT}.${MCODE}.${CYCLE}.${STORM}.out\"/g" ${BATCH_DIR}${BATCHFILE2}
        perl -pi -e "s/#SBATCH --error=.*/#SBATCH --error=\"${LOG_DIR////\/}GPLOT_Stats.${EXPT}.${MCODE}.${CYCLE}.${STORM}.err\"/g" ${BATCH_DIR}${BATCHFILE2}
        perl -pi -e "s/#SBATCH --nodes=.*/#SBATCH --nodes=1/g" ${BATCH_DIR}${BATCHFILE2}
        perl -pi -e "s/#SBATCH --ntasks-per-node=.*/#SBATCH --ntasks-per-node=12/g" ${BATCH_DIR}${BATCHFILE2}
        perl -pi -e "s/^NCLDIR=.*/NCLDIR=\"${NCL_DIR////\/}\"/g" ${BATCH_DIR}${BATCHFILE2}
        perl -pi -e "s/^NCLFILE=.*/NCLFILE=\"${NCLFILE}\"/g" ${BATCH_DIR}${BATCHFILE2}
        perl -pi -e "s/^LOGDIR=.*/LOGDIR=\"${LOG_DIR////\/}\"/g" ${BATCH_DIR}${BATCHFILE2}
        perl -pi -e "s/^LOGFILE=.*/LOGFILE=\"${LOGFILE}\"/g" ${BATCH_DIR}${BATCHFILE2}
        perl -pi -e "s/^NMLIST=.*/NMLIST=\"${NMLIST}\"/g" ${BATCH_DIR}${BATCHFILE2}
        perl -pi -e "s/^IDATE=.*/IDATE=\"${CYCLE}\"/g" ${BATCH_DIR}${BATCHFILE2}
        perl -pi -e "s/^SID=.*/SID=\"${STORM}\"/g" ${BATCH_DIR}${BATCHFILE2}
        perl -pi -e "s/^FORCE=.*/FORCE=\"${FORCE}\"/g" ${BATCH_DIR}${BATCHFILE2}

        echo "MSG: Executing GPLOT batch job submission."
        sbatch ${BATCH_DIR}${BATCHFILE2}


        # If the job was submitted, then increase the counter.
        N=$((N+1))


        # Limit the number of jobs to 50 to now overwhelm the batch scheduler
        if [[ N -ge 50 ]]; then
            echo "WARNING: Maximum number of jobs reahed (50)."
            break
        fi


    done
fi



wait

echo "$?"
echo "COMPLETE!"
