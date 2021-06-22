#!/bin/sh
#SBATCH --account=hur-aoml
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=00:45:00
#SBATCH --partition=tjet,ujet,sjet,vjet,xjet,kjet
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
#SBATCH --output=/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/log/GPLOT.Default.out
#SBATCH --error=/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/log/GPLOT.Default.err
#SBATCH --job-name="GPLOT.Default"
#SBATCH --mem=16G


#set -x


echo "MSG: Submitting jobs for STATS."


# Define important GPLOT directories
NMLIST_DIR="${GPLOT_DIR}/nmlist"
BATCH_DIR="${GPLOT_DIR}/batch"
NCL_DIR="${GPLOT_DIR}/ncl"
TBL_DIR="${GPLOT_DIR}/tbl"

# Get the namelist, could be from command line
if [ ! -z "$1" ];then
    NMLIST="$1"
else
    NMLIST="${NMLIST_DIR}/namelist.input.default"
fi

# Check if the namelist exists. If not, exit.
if [ ! -f ${NMLIST} ]; then
    echo "ERROR: Namelist does not exist."
    exit
else
    echo "MSG: Found this namelist --> ${NMLIST}."
fi



# Pull important variables from the namelist
DO_MAPS=`sed -n -e 's/^.*DO_MAPS =\s//p' ${NMLIST} | sed 's/^\t*//'`
DO_STATS=`sed -n -e 's/^.*DO_STATS =\s//p' ${NMLIST} | sed 's/^\t*//'`
DO_SHIPS=`sed -n -e 's/^.*DO_SHIPS =\s//p' ${NMLIST} | sed 's/^\t*//'`
DSOURCE=`sed -n -e 's/^.*DSOURCE =\s//p' ${NMLIST} | sed 's/^\t*//'`
EXPT=`sed -n -e 's/^.*EXPT =\s//p' ${NMLIST} | sed 's/^\t*//'`
MCODE=`sed -n -e 's/^.*MCODE =\s//p' ${NMLIST} | sed 's/^\t*//'`
IS_MSTORM=`sed -n -e 's/^.*IS_MSTORM =\s//p' ${NMLIST} | sed 's/^\t*//'`
ENSMEM=`sed -n -e 's/^.*ENSMEM =\s//p' ${NMLIST} | sed 's/^\t*//'`
IDIR=`sed -n -e 's/^.*IDIR =\s//p' ${NMLIST} | sed 's/^\t*//'`
ITAG=`sed -n -e 's/^.*ITAG =\s//p' ${NMLIST} | sed 's/^\t*//'`
EXT=`sed -n -e 's/^.*EXT =\s//p' ${NMLIST} | sed 's/^\t*//'`
ODIR=`sed -n -e 's/^.*ODIR =\s//p' ${NMLIST} | sed 's/^\t*//'`
INIT_HR=`sed -n -e 's/^.*INIT_HR =\s//p' ${NMLIST} | sed 's/^\t*//'`
FNL_HR=`sed -n -e 's/^.*FNL_HR =\s//p' ${NMLIST} | sed 's/^\t*//'`
DT=`sed -n -e 's/^.*DT =\s//p' ${NMLIST} | sed 's/^\t*//'`
IDATE=`sed -n -e 's/^.*IDATE =\s//p' ${NMLIST} | sed 's/^\t*//'`
SID=`sed -n -e 's/^.*SID =\s//p' ${NMLIST} | sed 's/^\t*//'`
BDECK_DIR=`sed -n -e 's/^.*BDECK_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`
ATCF1_DIR=`sed -n -e 's/^.*ATCF1_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`
ATCF1_TAG=`sed -n -e 's/^.*ATCF1_TAG =\s//p' ${NMLIST} | sed 's/^\t*//'`
ATCF2_DIR=`sed -n -e 's/^.*ATCF2_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`
ATCF2_TAG=`sed -n -e 's/^.*ATCF2_TAG =\s//p' ${NMLIST} | sed 's/^\t*//'`
BDECK_DIR=`sed -n -e 's/^.*BDECK_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`
SYS_ENV=`sed -n -e 's/^SYS_ENV =\s//p' ${NMLIST} | sed 's/^\t*//'`
CPU_ACCT=`sed -n -e 's/^CPU_ACCT =\s//p' ${NMLIST} | sed 's/^\t*//'`
QOS=`sed -n -e 's/^QOS =\s//p' ${NMLIST} | sed 's/^\t*//'`

#ATCF3_DIR=`sed -n -e 's/^.*ATCF3_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`
#ATCF3_TAG=`sed -n -e 's/^.*ATCF3_TAG =\s//p' ${NMLIST} | sed 's/^\t*//'`
#FORCE=`sed -n -e 's/^.*FORCE =\s//p' ${NMLIST} | sed 's/^\t*//'`

# Fallback option for BDECK_DIR
if [ -z "$BDECK_DIR" ]; then
    BDECK_DIR=`sed -n -e 's/^.*BDECK2_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`
fi

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
if [ "$IS_MSTORM" == "True" ]; then
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
if [ -z "$CPU_ACCT" ]; then
    echo "MSG: Could not find a CPU account in the namelist. Assuming 'hur-aoml'."
    CPU_ACCT="hur-aoml"
fi

if [ -z "$QOS" ]; then
    echo "MSG: Could not find a Queue of Service (QOS) in the namelist. Assuming 'batch'."
    QOS="batch"
fi

# If FORCE is undefined, set it to False.
if [ -z "$FORCE" ]; then
    FORCE="False"
fi


# Set the maximum number of job submissions
NMAX=25

# Get the batch submission mode [SBATCH,BACKGROUND,FOREGROUND]
BATCH_MODE=( `sed -n -e 's/^BATCH_MODE =\s//p' ${NMLIST} | sed 's/^\t*//' | tr a-z A-Z` )
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
ATCF1_ALL=(`find ${ATCF1_DIR} -type f -name "*${ATCF1_TAG}" | awk -F'/' '{print $NF $0}' | sort -t. -k2,2n | cut -d'/' -f2- | awk '{a="/"$0; print a}'`)
ATCF2_ALL=(`find ${ATCF2_DIR} -type f -name "*${ATCF2_TAG}" | awk -F'/' '{print $NF $0}' | sort -t. -k2,2n | cut -d'/' -f2- | awk '{a="/"$0; print a}'`)
for ATCF in ${ATCF2_ALL[@]}; do
    ATCF_BASE=`basename ${ATCF} | cut -d'.' -f-2`
    ATCF1_ALL=( ${ATCF1_ALL[@]/*$ATCF_BASE*/} )
done
ATCF_ALL=("${ATCF2_ALL[@]}" "${ATCF1_ALL[@]}")
NATCF="${#ATCF_ALL[*]}"
if [[ NATCF -gt NMAX ]]; then
    C=$((NATCF - NMAX))
    ATCF_ALL=("${ATCF_ALL[@]:$C}" "${ATCF_ALL[@]:0:$C}")
fi


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
BATCHFILE1="${BATCH_DIR}/batch_stats.generic.sh"
BATCHFILE2="${WORKhafs}/hrdgraphics/batch_stats.${EXPT}.sh"

# Some housekeeping
cp ${BATCHFILE1} ${BATCHFILE2}
chmod +x ${BATCHFILE2}


# Find output files from which graphics should be created
#if [ -z "$IDATE" ]; then
#    CYCLES=`ls -d ${IDIR}*/ | xargs -n 1 basename | tr "\n" " "`
#else
    CYCLES="${IDATE[@]}"
#fi
echo "MSG: Found these cycles: ${CYCLES[*]}"
echo ""


# For EXPT=GFS_Forecast, FNL_HR=240
if [ "$EXPT" == "GFS_Forecast" ]; then
    FNL_HR=240
fi




####################################################
# 3. CALL GPLOT STATS                              #
#    This script is responsible for creating track #
#    & intensity guidance/verification.            #
####################################################
if [ "${DO_STATS}" = "True" ]; then
    NCLFILE="GPLOT_stats.ncl"


    # Set the counter to limit submission to 50 jobs
    N=0


    ######################################
    # LOOP OVER ALL AVAILABLE ATCF FILES #
    ######################################
    for ATCF in "${ATCF_ALL[@]}"; do

        # Read 'FORCE' from the namelist.
        # If FORCE is undefined, set it to False.
        # FORCE may be automatically changed to 'True' later on,
        # so it is critical to redefine it here.
        FORCE=`sed -n -e 's/^FORCE =\s//p' ${NMLIST} | sed 's/^\t*//'`
        if [ -z "$FORCE" ]; then
            FORCE="False"
        fi

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
            echo "WARNING: Could not find the cycle from the ATCF file name --> ${ATCF}"
            echo "WARNING: To process it, please add the cycle to the ATCF file name."
            echo "WARNING: Skipping this ATCF because cycle not found."
            continue
        fi
        if [ -z "$STORM" ]; then
            echo "WARNING: Could not find the storm ID from the ATCF file name --> ${ATCF}"
            echo "WARNING: To process it, please add the storm ID to the ATCF file name."
            echo "WARNING: Skipping this ATCF because storm ID not found."
            continue
        fi

        # Parse important information from $STORM and $CYCLE
        SNUM=`echo "$STORM" | cut -c1-2`
        BASIN1=`echo "$STORM" | cut -c3`
        if [ "${BASIN1,,}" == "l" ]; then
            BASIN2="al"
        elif [ "${BASIN1,,}" == "e" ]; then
            BASIN2="ep"
        elif [ "${BASIN1,,}" == "c" ]; then
            BASIN2="cp"
        elif [ "${BASIN1,,}" == "w" ]; then
            BASIN2="wp"
        fi
        YYYY=`echo "$CYCLE" | cut -c1-4`
        MM=`echo "$CYCLE" | cut -c5-6`
        DD=`echo "$CYCLE" | cut -c7-8`
        HH=`echo "$CYCLE" | cut -c9-10`
        CYCLE2="${YYYY}-${MM}-${DD} ${HH}:00:00"

        # Create the B-Deck file name
        BDECK="${BDECK_DIR}/b${BASIN2}${SNUM}${YYYY}.dat"

        # Computes dates in YYYYMMDDHH format. This will be used to determine if
        # production for the current cycle should be forced.
        DATE_NOW="`date +'%Y%m%d%H'`"
        DATE_CUT="`date -d "${CYCLE2} UTC + ${FNL_HR} hours" +'%Y%m%d%H'`"
        echo "CYCLE = $CYCLE, DATE_NOW = $DATE_NOW, DATE_CUT = $DATE_CUT"

        # Process this ATCF only if the cycle is found in IDATE
        # or if IDATE is empty.
        if [ ! -z "${CYCLES[@]}" ]; then
            CYCLE_FOUND="False"
            for D in "${CYCLES[@]}"; do
                if [ "$D" == "$CYCLE" ]; then
                    CYCLE_FOUND="True"
                    break
                fi
            done
        else
            CYCLE_FOUND="True"
        fi

        # If the cycle is not found in IDATE, then skip to next ATCF
        if [ "$CYCLE_FOUND" == "False" ]; then
            #echo "WARNING: Skipping this ATCF because namelist cycle (IDATE) not found."
            continue
        fi

        # Process this ATCF only if the cycle is found in SID
        # or if SID is empty.
        if [ ! -z "$SID" ]; then
            STORM_FOUND="False"
            for S in "${SID[@]}"; do
                if [ "$S" == "$STORM" ]; then
                    STORM_FOUND="True"
                    break
                fi
            done
        else
            STORM_FOUND="True"
        fi

        # If the storm is not found in SID, then skip to next ATCF
        if [ "$STORM_FOUND" == "False" ]; then
            #echo "WARNING: Skipping this ATCF because namelist storm ID (SID) not found."
            continue
        fi

        # OK, checks have been passed so let's process this file.
        echo "MSG: Working on this ATCF --> $ATCF"


        # Create full output path.
        # Make the directory in case it doesn't already exist.
        ODIR_FULL="${ODIR}/guidance"
        echo "MSG: Output directory --> $ODIR_FULL"
        mkdir -p ${ODIR_FULL}


        # Get the status for this case
        STATUS_FILE="${ODIR_FULL}/status.${SIDLONG}.log"
        LOCK_FILE="${STATUS_FILE}.lock"

        lockfile -r-1 -l 180 "${LOCK_FILE}"
        CASE_STATUS=`cat ${STATUS_FILE} 2>/dev/null`
        rm -f "${LOCK_FILE}"


        # Print some information
        echo "MSG: Using this status file --> ${STATUS_FILE}"
        echo "MSG: Found this status --> ${CASE_STATUS}"


        # If the ATCF is new enough, force production.
        test=$(find ${ATCF} -mmin -20 2>/dev/null)
        if [[ -n $test ]]; then
            echo "MSG: This ATCF is not old enough. Forcing production."
            FORCE="True"
        fi


        # If the BDECK is new enough, force production.
        if [ -f ${BDECK} ]; then
            echo "MSG: Found this B-Deck --> $BDECK"
            test=$(find ${BDECK} -mmin -20 2>/dev/null)
            if [[ -n $test ]]; then
                echo "MSG: This BDECK is not old enough. Forcing production."
                FORCE="True"
            fi
        fi


        # If the current date is more recent than the date for the final lead time (DATE_CUT)
        # do NOT force production.
        if [ "$DATE_CUT" -lt "$DATE_NOW" ]; then
            echo "MSG: The current date ($DATE_NOW) is more recent than the cutoff date ($DATE_CUT). Not forcing production."
            FORCE="False"
        fi


        # Check the status and update it if necessary.
        # This logic will allow work to start on this case
        # or will move on to the next case.
        if [ "$FORCE" == "True" ]; then
            echo "MSG: Forcing production. Will ignore status."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "start" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
        elif [ "$CASE_STATUS" == "complete" ]; then
            echo "MSG: Status suggests this case has been completed."
            echo "MSG: Nothing to do here. Moving on to next case."
            echo ""
            continue
        elif [ "$CASE_STATUS" == "working" ]; then
            echo "MSG: Status suggests this case is being worked on."
            echo "MSG: Changing the status to 'update request 1'."
            echo ""
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "update request 1" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
            continue
        elif [ "$CASE_STATUS" == "update request 1" ]; then
            echo "MSG: Status suggests this case needs to be updated."
            echo "MSG: Changing the status to 'update request 2'."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "update request 2" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
            continue
        elif [ "$CASE_STATUS" == "update request 2" ]; then
            echo "MSG: Status suggests this case has stalled/failed."
            echo "MSG: Deleting the status for a restart."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "start" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
        elif [ "$CASE_STATUS" == "incomplete" ]; then
            echo "MSG: Status suggests that this case is incomplete."
            echo "MSG: Will try to find new input files."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "start" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
        elif [ -z "$CASE_STATUS" ]; then
            echo "MSG: Status not found. Treating this as a new case."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "start" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
        else
            echo "MSG: Unknown status (${CASE_STATUS}). Treating this as a new case."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "start" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
        fi


        # Create full output path.
        # Make the directory in case it doesn't already exist.
        ODIR_FULL="${ODIR}/guidance"
        echo "MSG: Output directory --> $ODIR_FULL"
        mkdir -p ${ODIR_FULL}

        # Write the ATCF file name to a text file to be accessed later.
        # Be sure to delete duplicate entries.
        if [ ! -z "$(cat ${ODIR_FULL}/ATCF_FILES.dat)" ]; then
            grep -v "${ATCF_BASE}" ${ODIR_FULL}/ATCF_FILES.dat > ${ODIR_FULL}/TMP.dat
            mv ${ODIR_FULL}/TMP.dat ${ODIR_FULL}/ATCF_FILES.dat
        fi
        echo "$ATCF" >> ${ODIR_FULL}/ATCF_FILES.dat
        sort -u ${ODIR_FULL}/ATCF_FILES.dat > ${ODIR_FULL}/ATCF_FILES.dat.TMP
        mv ${ODIR_FULL}/ATCF_FILES.dat.TMP ${ODIR_FULL}/ATCF_FILES.dat


        # Check if a similar job is already submitted
        echo "MSG: The batch file --> ${BATCH_DIR}${BATCHFILE2}"
        if [ "$BATCH_MODE" == "FOREGROUND" ]; then
            JOB_TEST=""
        elif [ "$BATCH_MODE" == "BACKGROUND" ]; then
            JOB_TEST=""
        else
            JOB_NAME="GPLOT.${EXPT}.${MCODE}.${CYCLE}.${STORM}"
            JOB_TEST=`/apps/slurm/default/bin/squeue -u $USER -o %.100j | /bin/grep "${JOB_NAME}"`
        fi

        # Change options in the batch submission script.
        if [ -z "$JOB_TEST" ]; then
            LOG_DIR="$ODIR_FULL"
            LOGFILE="GPLOT_Stats.${EXPT}.${MCODE}.${CYCLE}.${STORM}.log"
            sed -i 's@^NCLDIR=.*@NCLDIR='"${NCL_DIR}"'@g' ${BATCHFILE2}
            sed -i 's@^NCLFILE=.*@NCLFILE='"${NCLFILE}"'@g' ${BATCHFILE2}
            sed -i 's@^LOGDIR=.*@LOGDIR='"${LOG_DIR}"'@g' ${BATCHFILE2}
            sed -i 's@^LOGFILE=.*@LOGFILE='"${LOGFILE}"'@g' ${BATCHFILE2}
            sed -i 's@^NMLIST=.*@NMLIST='"${NMLIST}"'@g' ${BATCHFILE2}
            sed -i 's/^IDATE=.*/IDATE='"${CYCLE}"'/g' ${BATCHFILE2}
            sed -i 's/^SID=.*/SID='"${STORM}"'/g' ${BATCHFILE2}
            sed -i 's/^FORCE=.*/FORCE='"${FORCE}"'/g' ${BATCHFILE2}

            # Call the batch job
            echo "MSG: Executing GPLOT batch job submission. BATCH_MODE ${BATCH_MODE}"			
            if [ "$BATCH_MODE" == "FOREGROUND" ]; then
                ${BATCHFILE2}
            elif [ "$BATCH_MODE" == "BACKGROUND" ]; then
                ${BATCHFILE2} &
            else
                sed -i 's/^#SBATCH --account=.*/#SBATCH --account='"${CPU_ACCT}"'/g' ${BATCHFILE2}
                sed -i 's/^#SBATCH --job-name=.*/#SBATCH --job-name="GPLOT.'"${EXPT}"'.'"${CYCLE}${ENSIDTAG}"'.'"${DMN}${STORMTAG}"'.'"${TR}"'"/g' ${BATCHFILE2}
                sed -i 's@^#SBATCH --output=.*@#SBATCH --output="'"${LOG_DIR}"'GPLOT_Ships.'"${EXPT}"'.'"${CYCLE}${ENSIDTAG}"'.'"${DMN}${STORMTAG}"'.'"${TR}"'.out"/g' ${BATCHFILE2}
                sed -i 's@^#SBATCH --error=.*@#SBATCH --error="'"${LOG_DIR}"'GPLOT_Ships.'"${EXPT}"'.'"${CYCLE}${ENSIDTAG}"'.'"${DMN}${STORMTAG}"'.'"${TR}"'.err"/g' ${BATCHFILE2}
                sed -i 's/^#SBATCH --nodes=.*/#SBATCH --nodes=1/g' ${BATCHFILE2}
                sed -i 's/^#SBATCH --ntasks-per-node=.*/#SBATCH --ntasks-per-node=12/g' ${BATCHFILE2}
                sed -i 's/^#SBATCH --mem=.*/#SBATCH --mem=48G/g' ${BATCHFILE2}
                sed -i 's/^#SBATCH --time=.*/#SBATCH --time='"${RUNTIME}"'/g' ${BATCHFILE2}
                sed -i 's/^#SBATCH --qos=.*/#SBATCH --qos='"${QOS}"'/g' ${BATCHFILE2}    
                if [ -z "${PARTITION}" ]; then
                    if [ "$SYS_ENV" == "JET" ]; then
                        sed -i 's/^#SBATCH --partition=.*/#SBATCH --partition=tjet,ujet,sjet,vjet,xjet,kjet/g' ${BATCHFILE2}
                    elif [ "$SYS_ENV" == "HERA" ]; then
                        sed -i 's/^#SBATCH --partition=.*/#SBATCH --partition=hera/g' ${BATCHFILE2}
                    fi
                else
                    sed -i 's/^#SBATCH --partition=.*/#SBATCH --partition='"${PARTITION}"'/g' ${BATCHFILE2}
                fi
                sbatch ${BATCHFILE2}
            fi

            # If the job was submitted, then increase the counter.
            N=$((N+1))

            # Limit the number of jobs to now overwhelm the batch scheduler
            if [[ N -ge NMAX ]]; then
                echo "WARNING: Maximum number of jobs reahed (${NMAX})."
                break
            fi
        else
            echo "MSG: Found matching GPLOT batch job. Skipping submission."
        fi


    done
fi



wait

echo "$?"
echo "COMPLETE!"
