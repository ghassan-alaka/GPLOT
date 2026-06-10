#!/bin/sh
#SBATCH --account=aoml-hafs1
##SBATCH --nodes=1
##SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --time=00:15:00
#SBATCH --partition=u1-compute
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
#SBATCH --output=/scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOT/log/GPLOT.Default.out
#SBATCH --error=/scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOT/log/GPLOT.Default.err
#SBATCH --job-name="GPLOT.Default"
#SBATCH --mem=1G


#set -x

echo "MSG: spawn_stats.sh started at `date`"
echo "MSG: Submitting jobs for GPLOT Module 'STATS'."

# Determine the GPLOT source code directory
if [ -z "${GPLOT_DIR}" ]; then
    export GPLOT_DIR="$( echo "$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" | rev | cut -d'/' -f4- | rev )"
fi

# Define important GPLOT directories
NMLIST_DIR="${GPLOT_DIR}/parm/"
BATCH_DIR="${GPLOT_DIR}/sorc/GPLOT/batch/"
PY_DIR="${GPLOT_DIR}/sorc/GPLOT/python/"
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
        CPU_ACCT="aoml-hafs1"
    elif [ "${MACHINE}" == "HERA" ] || [ "${MACHINE}" == "URSA" ] || [ "${MACHINE}" == "ORION" ] || [ "${MACHINE}" == "HERCULES" ]; then
        CPU_ACCT="aoml-hafs1"
    else
        CPU_ACCT="aoml-hafs1"
    fi
    echo "MSG: Could not find a CPU account in the namelist. Assuming '${CPU_ACCT}' because we are on ${MACHINE}."
fi

if [ -z "${QOS}" ]; then
    echo "MSG: Could not find a Queue of Service (QOS) in the namelist. Assuming 'batch'."
    QOS="batch"
fi

if [ -z "${PARTITION}" ]; then
    if [ "${MACHINE^^}" == "JET" ]; then
        PARTITION="u1-compute"
    elif [ "${MACHINE^^}" == "HERA" ]; then
        PARTITION="hera"
    elif [ "${MACHINE^^}" == "URSA" ]; then
        PARTITION="u1-compute"
    elif [ "${MACHINE^^}" == "ORION" ]; then
        PARTITION="orion"
    elif [ "${MACHINE^^}" == "HERCULES" ]; then
        PARTITION="hercules"
    else
        PARTITION="u1-compute"
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


# Determine whether this experiment is an ensemble and build the list of
# member ids (ENSIDS). Mirrors the detection in the other spawn_*.sh scripts.
# Deterministic runs (ENSMEM=0 or empty) get ENSIDS=("XX"); the stats
# submission loop below then runs exactly once with an empty member tag, so
# deterministic output is byte-for-byte identical to before this change.
EID=( `sed -n -e 's/^EID =\s//p' ${NMLIST} | sed 's/^\t*//'` )
if [ -z "${EID[*]}" ]; then
    EID=( `sed -n -e 's/^ENSMEM =\s//p' ${NMLIST} | sed 's/^\t*//'` )
fi
echo "MSG: Found these ensemble members --> ${EID[*]}"
# NOTE (from support/HAFS): "00" is now a valid member id; only a bare "0" or
# an empty list marks a deterministic run.
if [ "${EID[*]}" == "0" ] || [ -z "${EID[*]}" ]; then
    IS_ENS="False"
    ENSIDS=( "XX" )
elif [ ! -z $(echo "${EID[0]}" | cut -d'-' -f2) ]; then
    IS_ENS="True"
    E1=$(echo "${EID[0]}" | cut -d'-' -f1)
    E2=$(echo "${EID[0]}" | cut -d'-' -f2)
    ENSIDS=( `seq -f "%02g" ${E1} ${E2}` )
else
    IS_ENS="True"
    ENSIDS=( `printf "%02d\n" ${EID[*]}` )
fi
echo "MSG: IS_ENS=${IS_ENS}; member ids --> ${ENSIDS[*]}"


# Set the maximum number of job submissions
# This is a safeguard to avoid overloading the batch scheduler.
if [ "${IS_ENS}" == "True" ]; then
    MAX_JOBS=525
else
    MAX_JOBS=25
fi

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
# Storm token used in the ATCF filename glob. Ensemble member ATCF files are
# 00L-named (one per member, under per-member subdirs), so the deterministic
# SID glob (e.g. *13l*) would miss them entirely. For ensembles search on
# "00l" instead -- find recurses, so this sweeps every member's ATCF; the
# per-member ENSID loop below then selects the right member by /CYCLE/ENSID
# path. Deterministic / multistorm runs keep the SID glob (unchanged).
if [ "${IS_ENS}" == "True" ]; then
    ATCFSID="00l"
else
    ATCFSID="${SID,,}"
fi
ATCF_TMP=()
ATCF_TMP+=( `find ${ATCF1_DIR} -type f -name "*${ATCFSID}*${IDATE}*${ATCF1_TAG}" | awk -F'/' '{print $NF $0}' | sort -t. -k2,2n | cut -d'/' -f2- | awk '{a="/"$0; print a}'` )
if [ "${ATCF1_DIR}" != "${ATCF2_DIR}" ] || [ "${ATCF1_TAG}" != "${ATCF2_TAG}" ]; then
    ATCF_TMP+=( `find ${ATCF2_DIR} -type f -name "*${ATCFSID}*${IDATE}*${ATCF2_TAG}" | awk -F'/' '{print $NF $0}' | sort -t. -k2,2nr | cut -d'/' -f2- | awk '{a="/"$0; print a}' | head -200` )
    ATCF_TMP+=( `find ${ATCF2_DIR} -type f -name "*${ATCFSID}*${IDATE}*${ATCF2_TAG}" | shuf | head -100` ) #| awk -F'/' '{print $NF $0}' | sort -t. -k2,2n | cut -d'/' -f2- | awk '{a="/"$0; print a}'` )
fi
ATCF_ALL=()
for ATCF in "${ATCF_TMP[@]}"; do
    ATCF_BASE="`basename ${ATCF} | cut -d'.' -f-2`"
    if [[ "${ATCF_ALL[*]}" != *"${ATCF_BASE}"* ]]; then
        ATCF_ALL+=( "${ATCF}" )
    fi
done
#printf '%s\n' "${ATCF_ALL[@]}"


# Limit the ATCF list based onMAX_JOBS 
NATCF="${#ATCF_ALL[*]}"
#if [[ NATCF -gt MAX_JOBS ]]; then
#    C=$((NATCF - MAX_JOBS))
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
    PYFILE="GPLOT_stats.py"
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

        # Ensemble member ATCFs are 00L-named, so the filename parse above yields
        # STORM=00L. Use the namelist SID as the real storm instead, so the
        # status file, B-deck path, and job name all key off the same storm the
        # per-member Python job uses (it keys off SID). Without this the spawn
        # would write status.00l.log while Python writes status.<sid>.log and the
        # workflow's completion check would never converge. Deterministic /
        # multistorm runs (IS_ENS=False) keep the filename-derived storm.
        if [ "${IS_ENS}" == "True" ] && [ ! -z "${SID}" ]; then
            STORM="${SID^^}"
            SIDLONG="${SID,,}"
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

        # Gate: skip this case if no GRIB2 input files exist anywhere
        # under IDIR for this cycle/storm. Historically stats was the
        # only module that would still queue jobs purely from ATCFs
        # even when no model output existed -- which fills real-time
        # queues with guidance-only jobs for cycles whose model data
        # hasn't been retained on disk. Mirror the IDIR_OPTS gating
        # that maps/polar/ships/airsea apply: no GRIB2 => no job.
        IDIR_OPTS=("" "${EXPT}/com/${CYCLE}/${STORM}/" "${EXPT}/com/${CYCLE}/" \
                   "${EXPT}/com/" "${EXPT}/" "${CYCLE}/${STORM}/" "${CYCLE}/" \
                   "${STORM}/" "${EXPT}/${CYCLE}/${STORM}/" "${EXPT}/${CYCLE}/" \
                   "com/${CYCLE}/${STORM}/" "com/${CYCLE}/" \
                   "${DSOURCE,,}.${YYYY}${MM}${DD}/${HH}/" \
                   "${YYYY}${MM}${DD}/${HH}/" \
                   "${DSOURCE,,}.${YYYY}${MM}${DD}/${HH}/atmos/" \
                   "${DSOURCE,,}.${YYYY}${MM}${DD}/${HH}/products/atmos/grib2/0p25/")
        EXT_CHK="${EXT:-.grb2}"
        INPUT_FOUND="False"
        for IO in "${IDIR_OPTS[@]}"; do
            IDIR_FULL_CHK="$(echo "${IDIR}/${IO}" | sed s#//*#/#g)"
            [ -d "${IDIR_FULL_CHK}" ] || continue
            if compgen -G "${IDIR_FULL_CHK}*${CYCLE}*${EXT_CHK}" > /dev/null \
               || compgen -G "${IDIR_FULL_CHK}*${STORM,,}*${CYCLE}*${EXT_CHK}" > /dev/null; then
                INPUT_FOUND="True"
                break
            fi
        done
        if [ "${INPUT_FOUND}" == "False" ]; then
            echo "WARNING: No GRIB2 input files found for ${STORM} ${CYCLE} under IDIR=${IDIR}."
            echo "WARNING: Skipping stats for this case; check that model output exists for this cycle."
            # Mark the case as 'incomplete' so the workflow's status check
            # (find -name 'status.*') sees a non-complete entry and keeps
            # retrying. Only fill the gap if no status file exists yet --
            # never overwrite an active or terminal state. ODIR_FULL hasn't
            # been computed yet at this gate, so mirror the assignment from
            # below so the status lands at the same path the rest of the
            # script would have used.
            if [ "${ODIR_TYPE}" == "1" ]; then
                ODIR_FULL_NOINPUT="${ODIR}/guidance/"
            else
                ODIR_FULL_NOINPUT="${ODIR}${EXPT}/${CYCLE}/guidance/"
            fi
            ODIR_FULL_NOINPUT="$(echo "${ODIR_FULL_NOINPUT}" | sed s#//*#/#g)"
            mkdir -p "${ODIR_FULL_NOINPUT}"
            STATUS_FILE_NOINPUT="${ODIR_FULL_NOINPUT}status.${SIDLONG}.log"
            LOCK_FILE_NOINPUT="${STATUS_FILE_NOINPUT}.lock"
            lockfile -r-1 -l 180 "${LOCK_FILE_NOINPUT}"
            EXISTING_STATUS=$(cat "${STATUS_FILE_NOINPUT}" 2>/dev/null)
            if [ -z "${EXISTING_STATUS}" ]; then
                echo "MSG: No prior status; writing 'incomplete' so the workflow knows this case is outstanding."
                echo "incomplete" > "${STATUS_FILE_NOINPUT}"
            else
                echo "MSG: Status exists (${EXISTING_STATUS}); leaving it alone."
            fi
            rm -f "${LOCK_FILE_NOINPUT}"
            continue
        fi

        # OK, checks have been passed so let's process this file.
        echo ""
        echo "************************"
        echo "MSG: Working on this ATCF --> ${ATCF}"
        echo "MSG: CYCLE = ${CYCLE}, DATE_NOW = ${DATE_NOW}, DATE_CUT = ${DATE_CUT}"


        ##########################
        # LOOP OVER ENSEMBLE IDS #
        ##########################
        # Deterministic runs iterate exactly once with ENSID="XX" /
        # ENSID_DIR="" so everything below is identical to the pre-ensemble
        # behavior. Members get a per-member output subdir + their own ATCF.
        # The body is intentionally not re-indented (bash ignores it) to keep
        # this a minimal, reviewable diff.
        for ID in ${ENSIDS[@]}; do

        # Set the 2-digit member id + path tag.
        if [ "${IS_ENS}" == "False" ]; then
            ENSID="XX"
            ENSIDTAG=""
        else
            # %02s (string) not %02d so member ids "08"/"09" don't parse as
            # invalid octal. No "E" prefix.
            ENSID=$(printf "%02s\n" "${ID}")
            ENSIDTAG=".${ENSID}"
        fi
        ENSID_DIR="$(echo ${ENSIDTAG} | cut -c2-)"

        # For ensembles, narrow the ATCF to this member's per-member path
        # .../${CYCLE}/${ENSID}. The real storm id stays whatever was parsed
        # from the namelist SID; member ATCF files are 00L-named.
        if [ "${IS_ENS}" == "True" ]; then
            for ATCF_M in "${ATCF_TMP[@]}"; do
                if [[ "${ATCF_M}" == *"/${CYCLE}/${ENSID}"* ]]; then
                    ATCF="${ATCF_M}"
                    break
                fi
            done
        fi

        # Create full output path.
        # Make the directory in case it doesn't already exist. ENSID_DIR is
        # empty for deterministic runs, so the sed below collapses the double
        # slash and the path is unchanged from before.
        if [ "${ODIR_TYPE}" == "1" ]; then
            ODIR_FULL="${ODIR}/${ENSID_DIR}/guidance/"
        else
            ODIR_FULL="${ODIR}${EXPT}/${CYCLE}/${ENSID_DIR}/guidance/"
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
        if [ "${ODIR_TYPE}" == "0" ]; then
            if [ "${DATE_CUT}" -ge "${DATE_NOW}" ] && [ "${CASE_STATUS}" == "complete" ]; then
                echo "MSG: The cutoff date (${DATE_CUT}) is more recent than the current date (${DATE_NOW}). Forcing delayed production."
                FORCE="Delay"
            else
                echo "MSG: The current date (${DATE_NOW}) is more recent than the cutoff date (${DATE_CUT}). Not forcing production yet."
                FORCE="False"
            fi
        else
            echo "MSG: Not forcing production within model workflow (ODIR_TYPE=1). FYI, cutoff date=${DATE_CUT}, current date=${DATE_NOW}"
            FORCE="False"
        fi


        # If the ATCF is new enough, force production.
        test=$(find ${ATCF} -mmin -30 2>/dev/null)
        if [[ -n ${test} ]]; then
            echo "MSG: This ATCF is not old enough. Forcing production."
            FORCE="True"
        fi


        # If the BDECK is new enough, force production.
        if [ -f "${BDECK}" ]; then
            echo "MSG: Found this B-Deck --> ${BDECK}"
            test=$(find ${BDECK} -mmin -30 2>/dev/null)
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
            FULL_CMD="${BATCH_DIR}/${BATCHFILE} ${MACHINE} ${PY_DIR}${PYFILE} ${LOGFILE1} ${NMLIST}"
            FULL_CMD="${FULL_CMD} ${CYCLE} ${STORM} ${FORCE} ${ENSID}"
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

            # Limit the number of jobs to now overwhelm the batch scheduler.
            # break 2: exit BOTH the member (ID) loop and the ATCF loop, matching
            # the pre-ensemble behavior where this break stopped the ATCF loop.
            if [[ N -ge MAX_JOBS ]]; then
                echo "WARNING: Maximum number of jobs reached (${MAX_JOBS})."
                break 2
            fi
        else
            echo "MSG: Found matching GPLOT batch job. Skipping submission."
        fi

        done #end of ID (ensemble member) loop


    done
fi



wait

echo "$?"
echo "MSG: spawn_stats.sh completed at `date`"
