#!/bin/sh
#SBATCH --account=hur-aoml
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=00:15:00
#SBATCH --partition=tjet,ujet,sjet,vjet,xjet,kjet
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
#SBATCH --output=/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/log/GPLOT.Default.out
#SBATCH --error=/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/log/GPLOT.Default.err
#SBATCH --job-name="GPLOT.Default"
#SBATCH --mem=16G


#set -x

echo "MSG: spawn_polar.sh started at `date`"
echo "MSG: Submitting jobs for GPLOT Module 'STATS'."

# Determine the GPLOT source code directory
if [ -z "${GPLOT_DIR}" ]; then
    export GPLOT_DIR="$( echo "$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" | rev | cut -d'/' -f4- | rev )"
fi

# Define important GPLOT directories
NMLIST_DIR="${GPLOT_DIR}/parm/"
BATCH_DIR="${GPLOT_DIR}/sorc/GPLOT/batch/"
NCL_DIR="${GPLOT_DIR}/sorc/GPLOT/ncl/"
TBL_DIR="${GPLOT_DIR}/tbl/"

# Get the namelist, could be from command line
NMLIST="${1:-namelist.master.default}"

# Check if the namelist exists. If not, exit.
if [ ! -f ${NMLIST} ]; then
    echo "WARNING: Couldn't find this namelist --> ${NMLIST}"
    NMLIST="${NMLIST_DIR}${NMLIST}"
    if [ ! -f ${NMLIST} ]; then
        echo "WARNING: Couldn't find this namelist --> ${NMLIST}"
        echo "ERROR: I can't proceed without a namelist."
        exit
    fi
fi
echo "MSG: Found this namelist --> ${NMLIST}"

# Pull important variables from the namelist
DO_STATS="`sed -n -e 's/^DO_STATS =\s//p' ${NMLIST} | sed 's/^\t*//'`"
DSOURCE="`sed -n -e 's/^DSOURCE =\s//p' ${NMLIST} | sed 's/^\t*//'`"
EXPT="`sed -n -e 's/^EXPT =\s//p' ${NMLIST} | sed 's/^\t*//'`"
MCODE="`sed -n -e 's/^MCODE =\s//p' ${NMLIST} | sed 's/^\t*//'`"
IS_MSTORM="`sed -n -e 's/^IS_MSTORM =\s//p' ${NMLIST} | sed 's/^\t*//'`"
IDIR="`sed -n -e 's/^IDIR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ITAG="`sed -n -e 's/^ITAG =\s//p' ${NMLIST} | sed 's/^\t*//'`"
EXT="`sed -n -e 's/^EXT =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ODIR="`sed -n -e 's/^ODIR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ODIR_TYPE="`sed -n -e 's/^ODIR_TYPE =\s//p' ${NMLIST} | sed 's/^\t*//'`"
INIT_HR="`sed -n -e 's/^INIT_HR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
FNL_HR="`sed -n -e 's/^FNL_HR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
FHRFMT="`sed -n -e 's/^FMT_HR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
DT="`sed -n -e 's/^DT =\s//p' ${NMLIST} | sed 's/^\t*//'`"
IDATE="`sed -n -e 's/^IDATE =\s//p' ${NMLIST} | sed 's/^\t*//'`"
SID="`sed -n -e 's/^SID =\s//p' ${NMLIST} | sed 's/^\t*//'`"
BDECK_DIR="`sed -n -e 's/^BDECK_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ATCF1_DIR="`sed -n -e 's/^ATCF1_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ATCF1_TAG="`sed -n -e 's/^ATCF1_TAG =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ATCF2_DIR="`sed -n -e 's/^ATCF2_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ATCF2_TAG="`sed -n -e 's/^ATCF2_TAG =\s//p' ${NMLIST} | sed 's/^\t*//'`"
MACHINE="`sed -n -e 's/^MACHINE =\s//p' ${NMLIST} | sed 's/^\t*//'`"
CPU_ACCT="`sed -n -e 's/^CPU_ACCT =\s//p' ${NMLIST} | sed 's/^\t*//'`"
QOS="`sed -n -e 's/^QOS =\s//p' ${NMLIST} | sed 's/^\t*//'`"
PARTITION="`sed -n -e 's/^PARTITION =\s//p' ${NMLIST} | sed 's/^\t*//'`"

# Define batch defaults
BATCH_DFLTS="${NMLIST_DIR}batch.defaults.${MACHINE,,}"

# Fallback option for BDECK_DIR
if [ -z "${BDECK_DIR}" ]; then
    BDECK_DIR="`sed -n -e 's/^BDECK2_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
fi

# Print information
echo "MSG: Found this data source in the namelist      --> ${DSOURCE}"
echo "MSG: Found this experiment in the namelist       --> ${EXPT}"
if [ -z "${IDATE}" ]; then
    echo "MSG: No cycles defined in the namelist. Will consider all."
else
    echo "MSG: Found these cycles in the namelist          --> ${IDATE}"
fi
if [ -z "${SID}" ]; then
    echo "MSG: No Storm IDs defined in the namelist. Will consider all."
else
    echo "MSG: Found these Storm IDs in the namelist       --> ${SID}"
fi
if [ "${IS_MSTORM}" == "True" ]; then
    echo "MSG: Data source has been identified as a multi-storm configuration."
fi
echo "MSG: Found this top level input directory in the namelist --> ${IDIR}"
if [ ! -z "${ITAG}" ]; then
    echo "MSG: Considering this input file string          --> ${ITAG}"
fi
if [ ! -z "${EXT}" ]; then
    echo "MSG: Considering this input file extension       --> ${EXT}"
fi
echo "MSG: Found this top level output directory in the namelist --> ${ODIR}"
if [ -z "${ODIR_TYPE}" ]; then
    ODIR_TYPE="0"
fi
if [ -z "${MACHINE}" ]; then
    MACHINE="`sed -n -e 's/^SYS_ENV =\s//p' ${NMLIST} | sed 's/^\t*//'`"
fi
if [ -z "${MACHINE}" ]; then
    MACHINE="JET"
fi
if [ -z "${CPU_ACCT}" ]; then
    if [ "${MACHINE}" == "JET" ]; then
        CPU_ACCT="hur-aoml"
    elif [ "${MACHINE}" == "HERA" ] || [ "${MACHINE}" == "ORION" ]; then
        CPU_ACCT="aoml-hafs1"
    else
        CPU_ACCT="hur-aoml"
    fi
    echo "MSG: Could not find a CPU account in the namelist. Assuming '${CPU_ACCT}' because we are on ${MACHINE}."
fi

if [ -z "${QOS}" ]; then
    echo "MSG: Could not find a Queue of Service (QOS) in the namelist. Assuming 'batch'."
    QOS="batch"
fi

if [ -z "${PARTITION}" ]; then
    if [ "${MACHINE^^}" == "JET" ]; then
        PARTITION="tjet,ujet,sjet,vjet,xjet,kjet"
    elif [ "${MACHINE^^}" == "HERA" ]; then
        PARTITION="hera"
    elif [ "${MACHINE^^}" == "ORION" ]; then
        PARTITION="orion"
    else
        PARTITION="tjet,ujet,sjet,vjet,xjet,kjet"
    fi
fi

# Get the Model ID(s) [ABCD]
MID=( `sed -n -e 's/^MID =\s//p' ${NMLIST} | sed 's/^\t*//'` )
if [ -z "${MID}" ]; then
    MID=( `sed -n -e 's/^MORIG =\s//p' ${NMLIST} | sed 's/^\t*//'` )
fi
if [ -z "${MID}" ]; then
    MID=( `sed -n -e 's/^DSOURCE =\s//p' ${NMLIST} | sed 's/^\t*//'` )
fi

# If FORCE is undefined, set it to False.
if [ -z "${FORCE}" ]; then
    FORCE="False"
fi


# Set the maximum number of job submissions
NMAX=25

# Get the batch submission mode [SBATCH,BACKGROUND,FOREGROUND]
BATCH_MODE="`sed -n -e 's/^BATCH_MODE =\s//p' ${NMLIST} | sed 's/^\t*//' | tr a-z A-Z`"
BATCH_MODE="${BATCH_MODE:-SBATCH}"
echo "MSG: Using this batch-submission mode   --> ${BATCH_MODE}"


# Get the 'sbatch' executable
if [ -z "${X_SBATCH}" ]; then
    X_SBATCH="`which sbatch 2>/dev/null`"
fi
if [ -z "${X_SBATCH}" ] && [ -f ${BATCH_DFLTS} ]; then
    X_SBATCH="`sed -n -e 's/^sbatch =\s//p' ${BATCH_DFLTS} | sed 's/^\t*//'`"
fi
if [ -z "${X_SBATCH}" ] && [ "${BATCH_MODE^^}" == "SBATCH" ]; then
    echo "ERROR: Can't find 'sbatch'. Exiting."
    exit 2
fi

# Get the 'squeue' executable
if [ -z "${X_SQUEUE}" ]; then
    X_SQUEUE="`which squeue 2>/dev/null`"
fi
if [ -z "${X_SQUEUE}" ] && [ -f ${BATCH_DFLTS} ]; then
    X_SQUEUE="`sed -n -e 's/^squeue =\s//p' ${BATCH_DFLTS} | sed 's/^\t*//'`"
fi
if [ -z "${X_SQUEUE}" ] && [ "${BATCH_MODE^^}" == "SBATCH" ]; then
    echo "ERROR: Can't find 'squeue'. Exiting."
    exit 2
fi

# Get a list of forecast lead times
FHRS=( $(seq ${INIT_HR} ${DT} ${FNL_HR} | tr "\n" " ") )
echo "MSG: Will produce graphics for these forecast lead times --> ${FHRS[*]}"


################################
# PREPARE THE LIST OF ATCF FILES

# Get the current date in YYYYMMDDHH format
DATE_NOW="`date +'%Y%m%d%H'`"

# Get all of the ATCF files so they can be searched later.
# If duplicates exist, keep the final ATCF version (ATCF2).
ATCF_TMP=()
if [ "${ATCF1_DIR}" != "${ATCF2_DIR}" ] || [ "${ATCF1_TAG}" != "${ATCF2_TAG}" ]; then
    ATCF_TMP+=( `find ${ATCF1_DIR} -type f -name "*${SID,,}*${IDATE}*${ATCF1_TAG}" | awk -F'/' '{print $NF $0}' | sort -t. -k2,2n | cut -d'/' -f2- | awk '{a="/"$0; print a}'` )
fi
ATCF_TMP+=( `find ${ATCF2_DIR} -type f -name "*${SID,,}*${IDATE}*${ATCF2_TAG}" | awk -F'/' '{print $NF $0}' | sort -t. -k2,2nr | cut -d'/' -f2- | awk '{a="/"$0; print a}' | head -200` )
ATCF_TMP+=( `find ${ATCF2_DIR} -type f -name "*${SID,,}*${IDATE}*${ATCF2_TAG}" | shuf | head -100` ) #| awk -F'/' '{print $NF $0}' | sort -t. -k2,2n | cut -d'/' -f2- | awk '{a="/"$0; print a}'` )
ATCF_ALL=()
for ATCF in "${ATCF_TMP[@]}"; do
    ATCF_BASE="`basename ${ATCF} | cut -d'.' -f-2`"
    if [[ "${ATCF_ALL[*]}" != *"${ATCF_BASE}"* ]]; then
        ATCF_ALL+=( "${ATCF}" )
    fi
done
#printf '%s\n' "${ATCF_ALL[@]}"


# Limit the ATCF list based on NMAX
NATCF="${#ATCF_ALL[*]}"
#if [[ NATCF -gt NMAX ]]; then
#    C=$((NATCF - NMAX))
#    ATCF_ALL=("${ATCF_ALL[@]:$C}" "${ATCF_ALL[@]:0:$C}")
#fi


CYCLES="${IDATE[@]}"
echo "MSG: Found these cycles: ${CYCLES[*]}"


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
    BATCHFILE="batch_stats.sh"

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
        FORCE="${FORCE:-False}"

        # Split up the ATCF file name to find the cycle and storm.
        # Skip to the next ATCF if cycle or storm is not found.
        ATCF_BASE=`basename "${ATCF}"`
        IFS='.' read -r -a ATCF_ELM <<< "${ATCF_BASE}"
        CYCLE=""
        STORM=""
        for A in "${ATCF_ELM[@]}"; do
            if [[ "${A}" =~ ^[0-9]{10}$ ]]; then
                CYCLE="${A}"
            fi
            if [[ `echo "${A}" | rev | cut -c1-3 | rev` =~ ^[0-9]{2}[a-z]$ ]]; then
                STORM=`echo "${A^^}" | rev | cut -c1-3 | rev`
                SIDLONG=`echo "${A,,}"`
            fi
            if [ ! -z "${CYCLE}" ] && [ ! -z "${STORM}" ]; then
                break
            fi
        done
        if [ -z "${CYCLE}" ]; then
            echo "WARNING: Could not find the cycle from the ATCF file name --> ${ATCF}"
            echo "WARNING: To process it, please add the cycle to the ATCF file name."
            echo "WARNING: Skipping this ATCF because cycle not found."
            continue
        fi
        if [ -z "${STORM}" ]; then
            echo "WARNING: Could not find the storm ID from the ATCF file name --> ${ATCF}"
            echo "WARNING: To process it, please add the storm ID to the ATCF file name."
            echo "WARNING: Skipping this ATCF because storm ID not found."
            continue
        fi

        # Parse important information from $STORM and $CYCLE
        SNUM=`echo "${STORM}" | cut -c1-2`
        BASIN1=`echo "${STORM}" | cut -c3`
        if [ "${BASIN1,,}" == "l" ]; then
            BASIN2="al"
        elif [ "${BASIN1,,}" == "e" ]; then
            BASIN2="ep"
        elif [ "${BASIN1,,}" == "c" ]; then
            BASIN2="cp"
        elif [ "${BASIN1,,}" == "w" ]; then
            BASIN2="wp"
        fi
        YYYY=`echo "${CYCLE}" | cut -c1-4`
        MM=`echo "${CYCLE}" | cut -c5-6`
        DD=`echo "${CYCLE}" | cut -c7-8`
        HH=`echo "${CYCLE}" | cut -c9-10`
        CYCLE2="${YYYY}-${MM}-${DD} ${HH}:00:00"

        # Create the B-Deck file name
        BDECK="${BDECK_DIR}b${BASIN2}${SNUM}${YYYY}.dat"

        # Computes dates in YYYYMMDDHH format. This will be used to determine if
        # production for the current cycle should be forced.
        #DATE_NOW="`date +'%Y%m%d%H'`"
        DATE_CUT="`date -d "${CYCLE2} UTC + ${FNL_HR} hours" +'%Y%m%d%H'`"

        # Process this ATCF only if the cycle is found in IDATE
        # or if IDATE is empty.
        if [ ! -z "${CYCLES[@]}" ]; then
            CYCLE_FOUND="False"
            for D in "${CYCLES[@]}"; do
                if [ "${D}" == "$CYCLE" ]; then
                    CYCLE_FOUND="True"
                    break
                fi
            done
        else
            CYCLE_FOUND="True"
        fi

        # If the cycle is not found in IDATE, then skip to next ATCF
        if [ "${CYCLE_FOUND}" == "False" ]; then
            #echo "WARNING: Skipping this ATCF because namelist cycle (IDATE) not found."
            continue
        fi

        # Process this ATCF only if the cycle is found in SID
        # or if SID is empty.
        if [ ! -z "${SID[*]}" ]; then
            STORM_FOUND="False"
            for S in "${SID[@]}"; do
                if [ "${S}" == "$STORM" ]; then
                    STORM_FOUND="True"
                    break
                fi
            done
        else
            STORM_FOUND="True"
        fi

        # If the storm is not found in SID, then skip to next ATCF
        if [ "${STORM_FOUND}" == "False" ]; then
            #echo "WARNING: Skipping this ATCF because namelist storm ID (SID) not found."
            continue
        fi

        # OK, checks have been passed so let's process this file.
        echo ""
        echo "************************"
        echo "MSG: Working on this ATCF --> ${ATCF}"
        echo "MSG: CYCLE = ${CYCLE}, DATE_NOW = ${DATE_NOW}, DATE_CUT = ${DATE_CUT}"


        # Create full output path.
        # Make the directory in case it doesn't already exist.
        if [ "${ODIR_TYPE}" == "1" ]; then
            ODIR_FULL="${ODIR}/guidance/"
        else
            ODIR_FULL="${ODIR}${EXPT}/${CYCLE}/guidance/"
        fi
        ODIR_FULL="$(echo "${ODIR_FULL}" | sed s#//*#/#g)"
        echo "MSG: Output directory --> ${ODIR_FULL}"
        mkdir -p ${ODIR_FULL}


        # Get the status for this case
        STATUS_FILE="${ODIR_FULL}status.${SIDLONG}.log"
        LOCK_FILE="${STATUS_FILE}.lock"
        lockfile -r-1 -l 180 "${LOCK_FILE}"
        CASE_STATUS=`cat ${STATUS_FILE} 2>/dev/null`
        rm -f "${LOCK_FILE}"


        # Print some information
        echo "MSG: Using this status file --> ${STATUS_FILE}"
        echo "MSG: Found this status --> ${CASE_STATUS}"


        # If the current date is more recent than the date for the final lead time (DATE_CUT)
        # do NOT force production.
        if [ "${DATE_CUT}" -ge "${DATE_NOW}" ] && [ "${CASE_STATUS}" == "complete" ]; then
            echo "MSG: The cutoff date (${DATE_CUT}) is more recent than the current date (${DATE_NOW}). Forcing delayed production."
            FORCE="Delay"
        else
            echo "MSG: The current date (${DATE_NOW}) is more recent than the cutoff date (${DATE_CUT}). Not forcing production yet."
            FORCE="False"
        fi


        # If the ATCF is new enough, force production.
        test=$(find ${ATCF} -mmin -60 2>/dev/null)
        if [[ -n ${test} ]]; then
            echo "MSG: This ATCF is not old enough. Forcing production."
            FORCE="True"
        fi


        # If the BDECK is new enough, force production.
        if [ -f "${BDECK}" ]; then
            echo "MSG: Found this B-Deck --> ${BDECK}"
            test=$(find ${BDECK} -mmin -60 2>/dev/null)
            if [[ -n ${test} ]]; then
                echo "MSG: This BDECK is not old enough. Forcing production."
                FORCE="True"
            fi
        fi



        # Check the status and update it if necessary.
        # This logic will allow work to start on this case
        # or will move on to the next case.
        if [ "${FORCE}" == "True" ]; then
            echo "MSG: Forcing production. Will ignore status."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "start" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
        elif [ "${CASE_STATUS}" == "delayed start 1" ]; then
            echo "MSG: Status suggests a delayed start."
            echo "MSG: Changing the status to 'delayed start 2'."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "delayed start 2" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
            continue
        elif [ "${CASE_STATUS}" == "delayed start 2" ]; then
            echo "MSG: Status suggests a delayed start."
            echo "MSG: Changing the status to 'delayed start 3'."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "delayed start 3" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
            continue
        elif [ "${CASE_STATUS}" == "delayed start 3" ]; then
            echo "MSG: Status suggests delayed start is ready."
            echo "MSG: Changing the status to 'update request 1'."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "update request 1" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
            continue
        elif [ "${CASE_STATUS}" == "update request 1" ]; then
            echo "MSG: Status suggests an update is requested."
            echo "MSG: Changing the status to 'update request 2'."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "update request 2" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
            continue
        elif [ "${CASE_STATUS}" == "update request 2" ]; then
            echo "MSG: Status suggests a 2nd update is requested."
            echo "MSG: Will re-initiate production for this case."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "start" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
        elif [ "${FORCE}" == "Delay" ]; then
            echo "MSG: Forcing delayed production."
            echo "MSG: Changing the status to 'update request 1'."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "delayed start 1" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
            continue
        elif [ "${CASE_STATUS}" == "complete" ]; then
            echo "MSG: Status suggests this case has been completed."
            echo "MSG: Nothing to do here. Moving on to next case."
            echo ""
            continue
        elif [ "${CASE_STATUS}" == "working" ]; then
            echo "MSG: Status suggests this case is being worked on."
            echo "MSG: Changing the status to 'update request 1'."
            echo ""
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "update request 1" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
            continue
        elif [ "${CASE_STATUS}" == "incomplete" ]; then
            echo "MSG: Status suggests that this case is incomplete."
            echo "MSG: Will try to find new input files."
            lockfile -r-1 -l 180 "${LOCK_FILE}"
            echo "start" > ${STATUS_FILE}
            rm -f "${LOCK_FILE}"
        elif [ -z "${CASE_STATUS}" ]; then
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


        # Write the ATCF file name to a text file to be accessed later.
        # Be sure to delete duplicate entries.
        if [ ! -z "$(cat ${ODIR_FULL}ATCF_FILES.dat 2>/dev/null)" ]; then
            grep -v "${ATCF_BASE}" ${ODIR_FULL}ATCF_FILES.dat > ${ODIR_FULL}TMP.dat
            mv ${ODIR_FULL}TMP.dat ${ODIR_FULL}ATCF_FILES.dat
        fi
        echo "${ATCF}" >> ${ODIR_FULL}ATCF_FILES.dat
        sort -u ${ODIR_FULL}ATCF_FILES.dat > ${ODIR_FULL}ATCF_FILES.dat.TMP
        mv ${ODIR_FULL}ATCF_FILES.dat.TMP ${ODIR_FULL}ATCF_FILES.dat


        # Check if a similar job is already submitted
        echo "MSG: The batch file --> ${BATCH_DIR}${BATCHFILE}"
        RUNTIME="00:29:59"
        JOBNAME="GPLOT.${EXPT}.${CYCLE}.stats.${STORM}.${MCODE}"
        if [ "${BATCH_MODE^^}" == "SBATCH" ]; then
            JOB_TEST=`${X_SQUEUE} -u $USER -o %.100j | /bin/grep "${JOBNAME}"`
        else
            JOB_TEST=""
        fi

        # Change options in the batch submission script.
        if [ -z "${JOB_TEST}" ]; then
            LOG_DIR="${ODIR_FULL}"
            LOGFILE1="${LOG_DIR}GPLOT_Stats.${EXPT}.${MCODE}.${CYCLE}.${STORM}.log"
            LOGFILE2="${LOG_DIR}GPLOT_Stats.${EXPT}.${MCODE}.${CYCLE}.${STORM}.out"

            # Call the batch job
            echo "MSG: Executing GPLOT batch job submission. BATCH_MODE ${BATCH_MODE}"			
            FULL_CMD="${BATCH_DIR}/${BATCHFILE} ${MACHINE} ${NCL_DIR}${NCLFILE} ${LOGFILE1} ${NMLIST}"
            FULL_CMD="${FULL_CMD} ${CYCLE} ${STORM} ${FORCE}"
            if [ "${BATCH_MODE^^}" == "FOREGROUND" ]; then
                echo "MSG: Executing this command [${FULL_CMD}]."
                ${FULL_CMD}
            elif [ "${BATCH_MODE^^}" == "BACKGROUND" ]; then
                echo "MSG: Executing this command [${FULL_CMD} &]."
                ${FULL_CMD} &
            else
                SLRM_OPTS="--account=${CPU_ACCT} --job-name=${JOBNAME} --output=${LOGFILE2} --error=${LOGFILE2}"
                #SLRM_OPTS="${SLRM_OPTS} --nodes=1 --ntasks-per-node=12 --mem=48G --time=${RUNTIME} --qos=${QOS} --partition=${PARTITION}"
                SLRM_OPTS="${SLRM_OPTS} --ntasks=1 --time=${RUNTIME} --qos=${QOS} --partition=${PARTITION}"
                echo "MSG: Executing this command [${X_SBATCH} ${SLRM_OPTS} ${FULL_CMD}]."
                ${X_SBATCH} ${SLRM_OPTS} ${FULL_CMD}
            fi

            # If the job was submitted, then increase the counter.
            N=$((N+1))

            # Limit the number of jobs to now overwhelm the batch scheduler
            if [[ N -ge NMAX ]]; then
                echo "WARNING: Maximum number of jobs reached (${NMAX})."
                break
            fi
        else
            echo "MSG: Found matching GPLOT batch job. Skipping submission."
        fi


    done
fi



wait

echo "$?"
echo "MSG: spawn_stats.sh completed at `date`"
