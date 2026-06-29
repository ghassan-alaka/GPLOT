#!/bin/sh
#SBATCH --account=aoml-hafs1
#SBATCH --ntasks=1
#SBATCH --time=00:19:30
#SBATCH --partition=u1-compute
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
#SBATCH --output=/scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOT/log/GPLOT.Default.out
#SBATCH --error=/scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOT/log/GPLOT.Default.err
#SBATCH --job-name="GPLOT.Default"
#SBATCH --mem=1G

echo "MSG: spawn_ens_compare.sh started at `date`"
echo "MSG: Submitting jobs for GPLOT Module 'ENS_COMPARE'."

# Determine the GPLOT source code directory
if [ -z "${GPLOT_DIR}" ]; then
    export GPLOT_DIR="$( echo "$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" | rev | cut -d'/' -f4- | rev )"
fi

# Define important GPLOT directories
echo "MSG: Using this GPLOT Directory --> ${GPLOT_DIR}"
NMLIST_DIR="${GPLOT_DIR}/parm/"
BATCH_DIR="${GPLOT_DIR}/sorc/GPLOT/batch/"
PY_DIR="${GPLOT_DIR}/sorc/GPLOT/python/"

# Get the namelist (from command line or default)
NMLIST="${1:-namelist.master.default}"

# Verify namelist exists
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

#
# Helper function to extract and trim namelist variables safely
get_var() {
    local var_name="$1"
    local file="$2"
    sed -n -e "s/^[[:space:]]*${var_name}[[:space:]]*=[[:space:]]*//p" "$file" | sed 's/[[:space:]]*$//'
}


# Pull variables from namelist
EXPT=$(get_var "EXPT" ${NMLIST})
MACHINE=$(get_var "MACHINE" ${NMLIST})

if [ -z "${MACHINE}" ]; then
    MACHINE=$(get_var "SYS_ENV" ${NMLIST})
fi

IDIR=$(get_var "IDIR" ${NMLIST})
ODIR=$(get_var "ODIR" ${NMLIST})
ODIR_TYPE="`sed -n -e 's/^ODIR_TYPE =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ATCF1_DIR="`sed -n -e 's/^ATCF1_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ATCF1_TAG="`sed -n -e 's/^ATCF1_TAG =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ATCF2_DIR="`sed -n -e 's/^ATCF2_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ATCF2_TAG="`sed -n -e 's/^ATCF2_TAG =\s//p' ${NMLIST} | sed 's/^\t*//'`"
IDATE=$(get_var "IDATE" ${NMLIST})
SID=$(get_var "SID" ${NMLIST})
EID=( `sed -n -e 's/^EID =\s//p' ${NMLIST} | sed 's/^\t*//'` )
IDIR="`sed -n -e 's/^IDIR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ITAG="`sed -n -e 's/^ITAG =\s//p' ${NMLIST} | sed 's/^\t*//'`"
EXT="`sed -n -e 's/^EXT =\s//p' ${NMLIST} | sed 's/^\t*//'`"
INIT_HR=$(get_var "INIT_HR" ${NMLIST})
FNL_HR=$(get_var "FNL_HR" ${NMLIST})
DT=$(get_var "DT" ${NMLIST})
CPU_ACCT=$(get_var "CPU_ACCT" ${NMLIST})
QOS=$(get_var "QOS" ${NMLIST})
PARTITION=$(get_var "PARTITION" ${NMLIST})

# Note: BATCH_MODE still needs the tr command to make it uppercase
BATCH_MODE=$(get_var "BATCH_MODE" ${NMLIST} | tr a-z A-Z)

# ens_compare is always an ensemble module. All Section-8 comparison settings
# (member range, cluster type, background field, plot toggles) are read directly
# from the namelist by plot_ens_compare.py, so they are NOT pulled or forwarded
# from here -- --master-nml is the sole config carrier.
IS_ENS="True"

# Cycle-discovery globs below use these; default if the namelist omits them.
MAX_CYCLES=$(get_var "MAX_CYCLES" ${NMLIST})
DSOURCE=$(get_var "DSOURCE" ${NMLIST})
if [ -z "${MAX_CYCLES}" ]; then MAX_CYCLES=25; fi
if [ -z "${DSOURCE}" ]; then DSOURCE="HAFS"; fi

# Cap submissions per spawn invocation so a large ensemble (many members x
# cycles) doesn't flood the scheduler. Once the cap is hit the spawn exits and
# the remaining jobs are picked up on the next workflow invocation (mirrors
# spawn_maps.sh). ens_compare is always an ensemble module, so use the
# ensemble-scale cap.
MAX_JOBS=$(get_var "MAX_JOBS" ${NMLIST})
if [ -z "${MAX_JOBS}" ]; then MAX_JOBS=525; fi

# Apply defaults where namelist didn't provide values
if [ -z "${MACHINE}" ]; then
    MACHINE="ORION"
fi
if [ -z "${CPU_ACCT}" ]; then
    CPU_ACCT="aoml-hafs1"
fi
if [ -z "${QOS}" ]; then
    QOS="batch"
fi
if [ -z "${PARTITION}" ]; then
    case "${MACHINE^^}" in
        JET)      PARTITION="u1-compute" ;;
        HERA)     PARTITION="hera" ;;
        URSA)     PARTITION="u1-compute" ;;
        ORION)    PARTITION="orion" ;;
        HERCULES) PARTITION="hercules" ;;
        *)        PARTITION="u1-compute" ;;
    esac
fi
if [ -z "${BATCH_MODE}" ]; then
    BATCH_MODE="SBATCH"
fi

echo "MSG: Experiment         --> ${EXPT}"
echo "MSG: Machine            --> ${MACHINE}"
echo "MSG: Input directory    --> ${IDIR}"
echo "MSG: Output directory   --> ${ODIR}"
echo "MSG: Cycle(s)           --> ${IDATE:-<auto-discover>}"
echo "MSG: Storm ID(s)        --> ${SID:-<all>}"
echo "MSG: Forecast hours     --> ${INIT_HR} to ${FNL_HR} by ${DT}"
echo "MSG: Batch mode         --> ${BATCH_MODE}"

# Ensure output log directory exists
LOGDIR="${ODIR}/log/"
mkdir -p ${LOGDIR}

# Find the forecast cycles for which graphics should be created
if [ -z "${IDATE}" ]; then
    echo ${IDIR}
    CYCLES=( `find ${IDIR}/ -maxdepth 4 \( -type d -o -xtype d \) -regextype sed -regex ".*/[0-9]\{10\}$" -exec basename {} \; | sort -u -r | head -${MAX_CYCLES} 2>/dev/null` )
    if [ -z "${CYCLES}" ]; then
        CYCLES=( `find ${IDIR}/ -maxdepth 4 \( -type d -o -xtype d \) -regextype sed -regex ".*/${DSOURCE,,}.[0-9]\{10\}$" -exec basename {} \; | sort -u -r | head -${MAX_CYCLES} 2>/dev/null` )
    fi
    if [ -z "${CYCLES}" ]; then
        CYCLES=( `find ${IDIR}/ -maxdepth 4 \( -type d -o -xtype d \) -regextype sed -regex ".*/[A-Za-z0-9]*\.[0-9]\{10\}$" -exec basename {} \; | sort -u -r | head -${MAX_CYCLES} 2>/dev/null` )
    fi
    if [ -z "${CYCLES}" ]; then
        CYCLES=( `find ${IDIR}/ -maxdepth 4 \( -type d -o -xtype d \) -regex ".*/\(00\|06\|12\|18\)" | grep -E "[0-9]{8}" | sort -u -r | rev | cut -d'/' -f-2 | sed 's@/@@g' | cut -d'.' -f1 | rev | tr "\n" " " | head -${MAX_CYCLES} 2>/dev/null` )
    fi
    if [ -z "${CYCLES}" ]; then
        CYCLES=( `ls -rd ${IDIR}/[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]/{00,06,12,18} 2>/dev/null | rev | cut -d'/' -f-2 2>/dev/null | sed 's@/@@g' | cut -d'.' -f1 | rev | tr "\n" " " | head -${MAX_CYCLES} 2>/dev/null` )
    fi
else
    CYCLES=( "${IDATE[@]}" )
fi
echo "MSG: Found these cycles --> ${CYCLES[*]}"
echo ""

# Build forecast hour list
FHOUR_LIST=$(seq ${INIT_HR} ${DT} ${FNL_HR} | tr '\n' ' ')
echo "MSG: Forecast hours --> ${FHOUR_LIST}"

# Set batch script and python script paths
BATCHFILE="${BATCH_DIR}batch_ens_compare.sh"
PYFILE="${PY_DIR}plot_ens_compare.py"

# Get all of the ATCF files across all forecast cycles so they can be searched later.
# If duplicates exist, keep the final ATCF (ATCF2).
ATCF_TMP=()
ATCF_ALL=()
for C in ${CYCLES[@]}; do
    CYCLE10="`echo "${C}" | rev | cut -d'.' -f1 | rev`"
    ATCF_TMP+=( `find ${ATCF2_DIR} -type f -name "*${CYCLE10}*${ATCF2_TAG}"` )
    ATCF_TMP+=( `find ${ATCF1_DIR} -type f -name "*${CYCLE10}*${ATCF1_TAG}"` )
done
for ATCF in ${ATCF_TMP[@]}; do
    ATCF_BASE="`basename ${ATCF} | cut -d'.' -f-2`"
    if [[ "${ATCF_ALL[*]}" != *"${ATCF_BASE}"* ]]; then
        ATCF_ALL+=( "${ATCF}" )
    fi
done

# Loop over cycles and submit one batch job per cycle.
# Iterate CYCLES (resolved above from IDATE or auto-discovery); IDATE is empty
# when cycles are auto-discovered, so looping it directly would never run.
N=0   # running count of submitted jobs (throttled by MAX_JOBS)
for DATE in ${CYCLES[@]}; do

    # Find the ATCFs for the current CYCLE.
    # It will be blank if no ATCFs are found.
    CYCLE_ATCF=( `printf '%s\n' ${ATCF_ALL[@]} | grep "${DATE}"` )

    #MD 20260622 - adding logic to search for storms in the ATCF
    # 1) Try to get STORMS from the namelist (SID)
    STORMS=()
    if [ ! -z "${SID}" ]; then
        STORMS+=("${SID[@]}")
    fi

    # 2) Try to get STORMS from the ATCF files
    if [ -z "${STORMS[*]}" ]; then
        # Lew.Gramer@noaa.gov 2025-09-04 (merged by Matt Donahue 2026-04-02)
        if [ "${IS_ENS}" == "False" ]; then 
            for ATCF in ${CYCLE_ATCF[@]}; do
                STORMS+=(`basename ${ATCF} | cut -d'.' -f1 | rev | cut -c1-3 | rev | tr '[:lower:]' '[:upper:]'`)
            done
        else
            for ATCF in ${CYCLE_ATCF[@]}; do
                # Find storms based on contents of ATCF file(s)...
                STORMS+=(`grep '^\(AL\|EP\)' ${ATCF} |sed -s 's/^\([A-Z][A-Z]*\), \([0-9][0-9]*\),.*/\2\1/'  | sed -s 's/AL/L/' | sed -s 's/EP/E/' | tr "\n" " "`)
            done
        fi
    fi

    # 3) Remove duplicate storms, if applicable.
    STORMS=($(printf "%s\n" "${STORMS[@]}" | sort -u))

    # We need a real storm to continue
    if [ -z "${STORMS[*]}" ] || [ "${STORMS[*]}" == "NONE" ]; then
        echo "WARNING: No storms were found. Can't continue."
        continue
    fi

    echo "MSG: Found these storms: ${STORMS[*]}"





    for STORM in ${STORMS[@]:-""}; do
        JOBNAME="GPLOT.ens_compare.${EXPT}.${DATE}"
        if [ ! -z "${STORM}" ]; then
            JOBNAME="${JOBNAME}.${STORM}"
        fi
        LOGFILE="${LOGDIR}ens_compare.${EXPT}.${DATE}.log"

        # Status tracking (mirrors the other modules' state machine, keyed per
        # (cycle, storm)). The status file lives where the Python writes its
        # output (ODIR/ensembleComparison). The spawn marks 'working' before
        # submitting; plot_ens_compare.py writes 'complete' on success. This is
        # what lets the HAFS workflow's `find -name 'status.*'` see ens_compare.
        STORMTAG="`echo "${STORM:-XXXX}" | tr '[:upper:]' '[:lower:]'`"
        ODIR_FULL="${ODIR}/ensembleComparison"
        mkdir -p "${ODIR_FULL}"
        STATUS_FILE="${ODIR_FULL}/status.ens_compare.${DATE}.${STORMTAG}.log"
        CASE_STATUS="`cat ${STATUS_FILE} 2>/dev/null`"
        if [ "${CASE_STATUS}" == "complete" ]; then
            echo "MSG: Status complete for ${DATE} ${STORM}; skipping."
            continue
        fi

        # GRIB2 pre-flight: ens_compare reads per-member GRIB2 from
        # ${IDIR}/${CYCLE}/<member>/*.grb2 (see HepTools.getGribData). Don't
        # submit a job that is guaranteed to fail because no member input exists
        # yet -- mark the case 'incomplete' (a non-complete marker the workflow's
        # `find -name 'status.*'` will keep retrying) and skip.
        GRB_CHECK=( `find ${IDIR}/${DATE} -maxdepth 2 -type f \( -name "*.grb2" -o -name "*.grib2" \) 2>/dev/null | head -1` )
        if [ -z "${GRB_CHECK[*]}" ]; then
            echo "WARNING: No member GRIB2 input found under ${IDIR}/${DATE} for ${STORM}; marking incomplete and skipping."
            echo "incomplete" > "${STATUS_FILE}"
            continue
        fi

        ARGS="${MACHINE} ${PYFILE} ${LOGFILE} ${NMLIST} ${DATE} ${STORM:-XXXX}"

        echo "MSG: Submitting ens_compare job --> ${JOBNAME}"
        echo "working" > "${STATUS_FILE}"

        if [ "${BATCH_MODE}" == "SBATCH" ]; then
            SLRM_OPTS="--job-name=${JOBNAME} --output=${LOGFILE} --error=${LOGFILE}"
            SLRM_OPTS="${SLRM_OPTS} --account=${CPU_ACCT} --partition=${PARTITION} --qos=${QOS}"
            SLRM_OPTS="${SLRM_OPTS} --ntasks=1 --mem=16G"
            sbatch ${SLRM_OPTS} ${BATCHFILE} ${ARGS}
        elif [ "${BATCH_MODE}" == "FOREGROUND" ]; then
            ${BATCHFILE} ${ARGS} > ${LOGFILE} 2>&1
        else
            ${BATCHFILE} ${ARGS} > ${LOGFILE} 2>&1 &
        fi

        # Throttle: stop submitting once the per-invocation cap is reached.
        ((N++))
        if [ "${N}" -ge "${MAX_JOBS}" ]; then
            echo "MSG: Reached MAX_JOBS (${MAX_JOBS}) submissions. Remaining jobs will be submitted on the next invocation."
            echo "MSG: spawn_ens_compare.sh completed at `date`"
            exit
        fi
    done
done

echo "MSG: spawn_ens_compare.sh completed at `date`"