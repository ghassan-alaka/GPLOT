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
VERBOSE=$(get_var "VERBOSE" ${NMLIST})

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

# CPU cores per ens_compare job. The module was tuned for multicore execution:
# two cluster-fetch threads each drive an OpenMP-threaded wgrib2, plus
# numpy/matplotlib work. --ntasks=1 alone allocates a single core under
# Slurm's cgroup enforcement, which serializes all of that (~4x slowdown
# observed). Override with ENS_COMPARE_CPUS in the namelist if desired.
ENS_CPUS=$(get_var "ENS_COMPARE_CPUS" ${NMLIST})
if [ -z "${ENS_CPUS}" ]; then ENS_CPUS=10; fi

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
#MD 20260810 - using bash array instead of space-separated string for fhour list.
#FHOUR_LIST=$(seq ${INIT_HR} ${DT} ${FNL_HR} | tr '\n' ' ')
read -ra FHOUR_LIST <<< "$(seq "$INIT_HR" "$DT" "$FNL_HR" | tr '\n' ' ')"
echo "MSG: Forecast hours --> ${FHOUR_LIST[*]}"

# Set batch script and python script paths
BATCHFILE="${BATCH_DIR}batch_ens_compare.sh"
PYFILE="${PY_DIR}plot_ens_compare.py"

# Get all of the ATCF files across all forecast cycles so they can be searched later.
# If duplicates exist, keep the final ATCF (ATCF2).
#MD 20260804. current behavior - atcf_tmp gets the full path of every ensemble member's atcf file from idir
#then atcf_all removes duplicates based on basename... so it just gets one of the ensemble members' atcf
#pros - this could work if all we want to know is what storms are present
#cons - this will not work for creating a full list of atcf files to read

#Follow-up - would it be marginally more robust to try to search for the control member? like add a "grep /00/" to the file search?

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

#MD added 20260804 - add robustness to sbatch and squeue executable search
#squeue is not currently used - it is used in other modules to check if similar jobs already exist
#should add this functionality.
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



# Loop over cycles and submit one batch job per cycle.
# Iterate CYCLES (resolved above from IDATE or auto-discovery); IDATE is empty
# when cycles are auto-discovered, so looping it directly would never run.
N=0   # running count of submitted jobs (throttled by MAX_JOBS)
for DATE in ${CYCLES[@]}; do

    # Find the ATCFs for the current CYCLE.
    # It will be blank if no ATCFs are found.
    CYCLE_ATCF=( `printf '%s\n' ${ATCF_ALL[@]} | grep "${DATE}"` )

    # If the CYCLE is empty, skip it
    if [ -z "$CYCLE_ATCF" ]; then
        echo "WARNING: The cycle is undefined. Skipping to next."
        continue
    fi

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

    #make output directory if not already there
    ODIR_FULL="${ODIR}/ensembleComparison"
    mkdir -p "${ODIR_FULL}"

#put the log file in ODIR_FULL rather than LOGDIR - LOGDIR should just contain spawn logfiles
    for STORM in ${STORMS[@]:-""}; do
        STORMTAG="`echo "${STORM:-XXXX}" | tr '[:upper:]' '[:lower:]'`"

        JOBNAME="GPLOT.ens_compare.${EXPT}.${DATE}"
        if [ ! -z "${STORM}" ]; then
            JOBNAME="${JOBNAME}.${STORM}"
        fi
        LOGFILE1="${ODIR_FULL}/GPLOT_ensembleComparison.${EXPT}.${DATE}.${STORMTAG}.log"
        LOGFILE2="${ODIR_FULL}/GPLOT_ensembleComparison.${EXPT}.${DATE}.${STORMTAG}.out"

        # Status tracking (mirrors the other modules' state machine, keyed per
        # (cycle, storm)). The status file lives where the Python writes its
        # output (ODIR/ensembleComparison). The spawn marks 'working' before
        # submitting; plot_ens_compare.py writes 'complete' on success. This is
        # what lets the HAFS workflow's `find -name 'status.*'` see ens_compare.

        STATUS_FILE="${ODIR_FULL}/status.ens_compare.${DATE}.${STORMTAG}.log"
        CASE_STATUS="`cat ${STATUS_FILE} 2>/dev/null`"
        if [ "${CASE_STATUS}" == "complete" ]; then
            echo "MSG: Status complete for ${DATE} ${STORM}; skipping."
            continue
        fi

        #MD 20260806 - I want to track expected output files, and if a file exists, skip it during the main plotting script
        #I think the best way would be to CREATE that tracking file here (only if it does not already exist)
        #Upon creation, mark all of the files as "not produced". 
        #Then if the spawn script is called again, it won't overwrite the file
        #It will only be edited by the plotting script when a plot is done
        ##################################### BEGIN UNPLOTTEDFILES CHECK #####################################################
        UNPLOTTED_FILE="${ODIR_FULL}/UnplottedFiles.${EXPT}.${DATE}.${STORMTAG}.dat"
        if [ ! -f "${UNPLOTTED_FILE}" ]; then
            echo "MSG: UnplottedFiles list not found. Creating."
            #data needed for this list: FHOUR_LIST - already exists
            ensembleLinePlots=$(get_var "ENSEMBLE_LINE_PLOTS" ${NMLIST})
            ensembleTracksColored=$(get_var "ENSEMBLE_TRACKS_COLORED" ${NMLIST})
            ensembleWindRadii=$(get_var "ENSEMBLE_WIND_RADII" ${NMLIST})
            ensembleClustering=$(get_var "ENSEMBLE_CLUSTERING" ${NMLIST})
            vortexAvgSteer=$(get_var "VORTEX_AVG_STEER" ${NMLIST})
            tiltPlots=$(get_var "TILT_PLOTS" ${NMLIST})

            #MD 20260819 - adjusting for more background fields
            bgFields=$(get_var "BG_FIELDS" ${NMLIST})
            #turn into array and remove whitespaces/commas
            bgFields="${bgFields//,/ }"
            read -ra bgFields <<< "$bgFields"
            bgFields=("${bgFields[@]//:/}")


            #DO_CONVERTGIF = $(get_var "DO_CONVERTGIF" ${NMLIST}), default to False
            DO_CONVERTGIF="False" #can add this capability when we actually add gif conversion to figures
            if [[ "$DO_CONVERTGIF" == "True" ]]; then
                FIGEXT=".gif"
            else
                FIGEXT=".png"
            fi

            ALLOWED_CLUSTER_TYPES=("MSLP" "R34" "R50" "R64" "ltrack" "xtrack")
            clusterTypes=$(get_var "CLUSTER_TYPES" ${NMLIST})
            if [ -z "${clusterTypes}" ]; then clusterTypes="all"; fi

            if [[ "$clusterTypes" == "all" ]]; then
                clusterTypes=("${ALLOWED_CLUSTER_TYPES[@]}")
            else
                # Sanity check: keep only allowed cluster types
                valid_cluster_types=()

                for clusterType in "${clusterTypes[@]}"; do
                    if [[ " ${ALLOWED_CLUSTER_TYPES[*]} " == *" $clusterType "* ]]; then
                        valid_cluster_types+=("$clusterType")
                    fi
                done

                clusterTypes=("${valid_cluster_types[@]}")

                # If nothing valid was provided, use all cluster types
                if [[ ${#clusterTypes[@]} -eq 0 ]]; then
                    clusterTypes=("${ALLOWED_CLUSTER_TYPES[@]}")
                fi
            fi

            # Build list of cluster file types
            CLUSTER_FILE_TYPES=()

            if [[ "$ensembleLinePlots" == "True" ]]; then
                CLUSTER_FILE_TYPES+=("line_plot")
            fi

            if [[ "$ensembleTracksColored" == "True" ]]; then
                CLUSTER_FILE_TYPES+=("spatial_tracks")
            fi

            if [[ "$ensembleClustering" == "True" ]]; then
                for bgField in "${bgFields[@]}"; do
                    CLUSTER_FILE_TYPES+=("${bgField}.spatial_cluster")
                done
            fi

            if [[ "$vortexAvgSteer" == "True" ]]; then
                CLUSTER_FILE_TYPES+=("wind.vortex_cluster")
            fi


            # Build all base filenames
            ALL_BASE_FILENAMES=()

            for clusterType in "${clusterTypes[@]}"; do
                for fileType in "${CLUSTER_FILE_TYPES[@]}"; do
                    ALL_BASE_FILENAMES+=("${fileType}.${clusterType}")
                done
            done

            if [[ "$tiltPlots" == "True" ]]; then
                ALL_BASE_FILENAMES+=("vortex_tilt")
            fi

            if [[ "$ensembleWindRadii" == "True" ]]; then
                ALL_BASE_FILENAMES+=("wind_radii.R34")
                ALL_BASE_FILENAMES+=("wind_radii.R50")
                ALL_BASE_FILENAMES+=("wind_radii.R64")
            fi


            # Build complete filenames
            ALL_FILENAMES=()

            for fhour in "${FHOUR_LIST[@]}"; do
                # Zero-pad forecast hour to 3 digits
                printf -v fhour_padded "%03d" "$fhour"

                for file_base in "${ALL_BASE_FILENAMES[@]}"; do
                    ALL_FILENAMES+=("${STORMTAG}.${DATE}.${file_base}.f${fhour_padded}${FIGEXT}")
                done
            done

            # Write filenames to output file
            printf '%s\n' "${ALL_FILENAMES[@]}" | sort > "$UNPLOTTED_FILE"

        elif [[ ! -s "$UNPLOTTED_FILE" ]]; then
            echo "MSG: Unplotted Files list already exists, and is empty."
            echo "MSG: This means there is nothing left to plot, and the job is done."
            echo "MSG: Updating status to complete for ${DATE} ${STORM} and skipping."
            echo "complete" > "${STATUS_FILE}"
            continue
            
        else
            echo "MSG: Unplotted Files list already exists, skipping creation."
        fi
        ##################################### END UNPLOTTEDFILES CHECK #######################################################


        #MD 20260804 - need to account for alternate file structures maybe
        #   staged data in lgramer directory:
        #   2024: .../2024100600/{ENSID}/00l.2024100600.hfsa.trak.atcfunix.all
        #   2025: .../2025102312/{ENSID}/00l.2025102312.hfsa.trak.atcfunix.all
        #   2026: .../H226_ens_2km_{ENSID}/com/2025102212/00L/00l.2025102212.hfsa.trak.atcfunix.all
        #
        #   in adeck, it can look like
        #   hafs.{CYCLE}/hp{ENSID}.t00z.cyclone.trackatcfunix and hafs.{CYCLE}/hc00.t00z.cyclone.trackatcfunix

        #That being said, the 2026 data is going to live in AWS bitbucket - we can transfer it to Ursa in any 
        #structure we want. For now, I will plan for transferring it in the same format.


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

        ARGS="${MACHINE} ${PYFILE} ${LOGFILE1} ${NMLIST} ${DATE} ${STORM:-XXXX} ${VERBOSE:-0}"

        echo "MSG: Submitting ens_compare job --> ${JOBNAME}"
        echo "working" > "${STATUS_FILE}"

        # MD 20260630 - need to add a runtime argument - job times out at 2hr having completed only 15 fhrs
        N_FHOURS=$(wc -w <<< "$FHOUR_LIST")
        if [[ $N_FHOURS -le 5 ]]; then
            RUNTIME="01:29:59"
        elif [[ $N_FHOURS -le 10 ]]; then
            RUNTIME="02:29:59"
        elif [[ $N_FHOURS -le 15 ]]; then
            RUNTIME="03:29:59"
        elif [[ $N_FHOURS -le 20 ]]; then
            RUNTIME="04:29:59"
        elif [[ $N_FHOURS -le 25 ]]; then
            RUNTIME="05:29:59"
        elif [[ $N_FHOURS -le 30 ]]; then
            RUNTIME="06:29:59"
        elif [[ $N_FHOURS -le 35 ]]; then
            RUNTIME="07:29:59"
        else
            RUNTIME="07:59:59"
        fi
        #feed output into the .out file, (will add .log file to command line arguments as logger)
        if [ "${BATCH_MODE}" == "SBATCH" ]; then
            SLRM_OPTS="--job-name=${JOBNAME} --output=${LOGFILE2} --error=${LOGFILE2}"
            SLRM_OPTS="${SLRM_OPTS} --account=${CPU_ACCT} --partition=${PARTITION} --qos=${QOS}"
            SLRM_OPTS="${SLRM_OPTS} --ntasks=1 --cpus-per-task=${ENS_CPUS} --mem=16G --time=${RUNTIME}"
            echo "MSG: Executing this command [${X_SBATCH} ${SLRM_OPTS} ${BATCHFILE} ${ARGS}]."
            ${X_SBATCH} ${SLRM_OPTS} ${BATCHFILE} ${ARGS}
        elif [ "${BATCH_MODE}" == "FOREGROUND" ]; then
            echo "MSG: Executing this command [${BATCHFILE} ${ARGS} > ${LOGFILE2} 2>&1]."
            ${BATCHFILE} ${ARGS} >> ${LOGFILE2} 2>&1
        else
            echo "MSG: Executing this command [${BATCHFILE} ${ARGS} > ${LOGFILE2} 2>&1 &]."
            ${BATCHFILE} ${ARGS} >> ${LOGFILE2} 2>&1 &
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