#!/bin/sh
#
# This is a wrapper script that calls a single script
# for one or more experiments (EXPT). The purpose of this
# wrapper is to loop over experiments and provide the
# namelist that corresponds to each experiment to the
# downstream script. The downstream script (BATCHFILE)
# is responsible for submitting a batch job, if necessary.
# This wrapper can run in CRONTAB.

set -a

echo "MSG: GPLOT wrapper started $(/bin/date)"
echo "MSG: Welcome to the GPLOT submission wrapper."
echo "MSG: This wrapper automatically submits appropriate"
echo "MSG: shell scripts for each component of GPLOT."


# GPLOT_DIR must be set. It is preferable for the user to set it
# as an environmental variable. If not, the script will attempt to

# Determine the GPLOT source code directory
if [ -z "${HOMEgplot}" ]; then
    export HOMEgplot="$( echo "$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" | rev | cut -d'/' -f4- | rev | sed s#//*#/#g)"
    export GPLOT_DIR="${HOMEgplot}"
fi
if [ -z "${SORCgplot}" ]; then
    export SORCgplot="$( echo "$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" | rev | cut -d'/' -f2- | rev | sed s#//*#/#g)"
fi
echo "MSG: Setting HOMEgplot --> ${HOMEgplot}"


# Test that HOMEgplot actually exists. If not, we can't continue.
if [ ! -d "${HOMEgplot}" ]; then
    echo "ERROR: HOMEgplot not found. Can't continue."
    exit
fi


# Create variables for GPLOT subdirectories.
NMLgplot="$(echo "${HOMEgplot}/parm/" | sed s#//*#/#g)"
BATCHgplot="$(echo "${SORCgplot}/batch/" | sed s#//*#/#g)"


# Store all experiments in the EXPT variable. The user should submit all
# experiments as command line args. However, if no options are submitted
# EXPT will be set to a hard-coded list of experiments. feel free to
# modify it so that it includes all experiments on which GPLOT should run.
# Example:  EXPT="EXPT1 EXPT2 EXPT3 ... EXPTN"
# For each experiment, a corresponding master namelist must exist.
# If the master namelist can't be found, then the submission will fail.
# Some old/inactive experiments:  H18W HP2H HB17_v1_history GFS_Forecast
#                                 fvGFS_ATL HB18_v2_forecast"
if [ $# -eq 0 ]; then
    echo "MSG: No experiments found via the command line."
    NML_LIST=( "${NMLgplot}namelist.master.GFS_Forecast" \
               "${NMLgplot}namelist.master.HWRF_Forecast" \
               "${NMLgplot}namelist.master.HMON_Forecast" )
else
    echo "MSG: Experiment found via the command line."
    NML_LIST=( "$@" )
fi
if [ -z "${NML_LIST[*]}" ]; then
    echo "ERROR: No experiments found. Something went wrong."
    exit
fi
echo "MSG: Found these experiment namelists --> ${NML_LIST[*]}"


# Loop over all experiments
for NML in "${NML_LIST[@]}"; do
    echo "MSG: GPLOT is working on this experiment namelist --> ${NML}"


    # Define the master namelist file name.
    # If this namelist is not found in $GPLOT_DIR/nmlist/,
    # the submission will fail.
    if [ ! -f "${NML}" ]; then
        NML2="${NMLgplot}${NML}"
	if [ ! -f "${NML2}" ]; then
            NML2="${NMLgplot}namelist.master.${NML}"
            if [ ! -f "${NML2}" ]; then
                echo "WARNING: Master namelist could not be found."
                echo "WARNING: Can't submit anything for this experiment."
                echo "WARNING: Skipping to next experiment namelist."
                continue
            else
                NML="${NML2}"
            fi
        else
            NML="${NML2}"
        fi
    fi
    echo "MSG: Hooray! Master namelist found --> ${NML}"


    # Determine the components of GPLOT that should be submitted.
    # These options currently include:  Maps, Ships, Stats, Polar
    GPMODLIST=()
    DO_MAPS="`sed -n -e 's/^DO_MAPS =\s//p' ${NML} | sed 's/^\t*//'`"
    if [ "${DO_MAPS}" = "True" ]; then
        GPMODLIST+=("maps")
    fi
    DO_STATS="`sed -n -e 's/^DO_STATS =\s//p' ${NML} | sed 's/^\t*//'`"
    if [ "${DO_STATS}" = "True" ]; then
        GPMODLIST+=("stats")
    fi
    DO_SHIPS="`sed -n -e 's/^DO_SHIPS =\s//p' ${NML} | sed 's/^\t*//'`"
    if [ "${DO_SHIPS}" = "True" ]; then
        GPMODLIST+=("ships")
    fi
    DO_POLAR="`sed -n -e 's/^DO_POLAR =\s//p' ${NML} | sed 's/^\t*//'`"
    if [ "${DO_POLAR}" = "True" ]; then
        GPMODLIST+=("polar")
    fi
    echo "MSG: Working on these GPLOT modules --> ${GPMODLIST[*]}"

    # Get the experiment name
    EXPT="${SUBEXPT:-$(sed -n -e 's/^EXPT =\s//p' ${NML} | sed 's/^\t*//')}"

    # Get the output directory for proper logging
    ODIR="`sed -n -e 's/^ODIR =\s//p' ${NML} | sed 's/^\t*//'| sed s#//*#/#g`"
    LOGDIR="$(echo "${ODIR}/log/" | sed s#//*#/#g)"
    if [ ! -d ${LOGDIR} ]; then
        mkdir -p ${LOGDIR}
    fi

    # Get the submission mode from the namelist.
    BATCH_MODE="`sed -n -e 's/^BATCH_MODE =\s//p' ${NML} | sed 's/^\t*//' | tr a-z A-Z`"
    if [ -z "${BATCH_MODE}" ]; then
        BATCH_MODE="BACKGROUND"
    fi
    echo "MSG: BATCH_MODE --> ${BATCH_MODE}."

    # Get the machine 
    MACHINE="`sed -n -e 's/^MACHINE =\s//p' ${NML} | sed 's/^\t*//'`"
    if [ -z "${MACHINE}" ]; then
        MACHINE="`sed -n -e 's/^SYS_ENV =\s//p' ${NML} | sed 's/^\t*//'`"
    fi

    # Define extra variables for batch submissions ($BATCH_MODE="SBATCH")
    if [ "${BATCH_MODE^^}" == "SBATCH" ]; then
        BATCH_DFLTS="${NMLgplot}batch.defaults.${MACHINE,,}"

        # Get the CPU account
        CPU_ACCT="`sed -n -e 's/^CPU_ACCT =\s//p' ${NML} | sed 's/^\t*//'`"
        if [ -z "${CPU_ACCT}" ] && [ -f ${BATCH_DFLTS} ]; then
            CPU_ACCT="`sed -n -e 's/^CPU_ACCT =\s//p' ${BATCH_DFLTS} | sed 's/^\t*//'`"
        fi

        # Get the Slurm Queue-Of-Service (QOS)
        QOS="`sed -n -e 's/^QOS =\s//p' ${NML} | sed 's/^\t*//'`"
        if [ -z "${QOS}" ] && [ -f ${BATCH_DFLTS} ]; then
            QOS="`sed -n -e 's/^QOS =\s//p' ${BATCH_DFLTS} | sed 's/^\t*//'`"
        fi

        # Get the Slurm partition
        PARTITION="`sed -n -e 's/^PARTITION =\s//p' ${NML} | sed 's/^\t*//'`"
        if [ -z "${PARTITION}" ] && [ -f ${BATCH_DFLTS} ]; then
            PARTITION="`sed -n -e 's/^PARTITION =\s//p' ${BATCH_DFLTS} | sed 's/^\t*//'`"
        fi

        # Get the 'sbatch' executable
        if [ -z "${X_SBATCH}" ]; then
            X_SBATCH="`which sbatch 2>/dev/null`"
        fi
        if [ -z "${X_SBATCH}" ] && [ -f ${BATCH_DFLTS} ]; then
            X_SBATCH="`sed -n -e 's/^sbatch =\s//p' ${BATCH_DFLTS} | sed 's/^\t*//'`"
        fi
        if [ -z "${X_SBATCH}" ]; then
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
        if [ -z "${X_SQUEUE}" ]; then
            echo "ERROR: Can't find 'squeue'. Exiting."
            exit 2
        fi
          
    fi


    # Loop over GPLOT modules
    for M in "${GPMODLIST[@]}"; do
        echo "****************"
        echo "MSG: Submission for GPLOT module ${M^^} is turned on."
        SPAWNFILE="${BATCHgplot}spawn_${M,,}.sh"
        SPAWNLOG1="${LOGDIR}spawn_${M,,}.${EXPT}.log"
        SPAWNLOG2="${LOGDIR}spawn_${M,,}.${EXPT}.out"
        JOBNAME="GPLOT.spawn_${M,,}.${EXPT}"
        echo "MSG: Spawn file --> ${SPAWNFILE}"
        echo "MSG: Spawn log --> ${SPAWNLOG}"
        if [ "${BATCH_MODE^^}" == "SBATCH" ]; then
            if ${X_SQUEUE} -u ${USER} -o "%.10i %.10P %.50j" | grep -q "${JOBNAME}"; then
                id=( `${X_SQUEUE} -u ${USER} -o "%.10i %.10P %.50j" | grep "${JOBNAME}" | sed 's/^[ \t]*//g' | cut -d' ' -f1` ) # 2>/dev/null 2>&1` )
                echo "MSG: Batch job(s) already exist(s) --> ${id[*]}"
                echo "MSG: Not submitting anything for ${M^^}."
            else
                SLRM_OPTS="--job-name=${JOBNAME}  --output=${SPAWNLOG2} --error=${SPAWNLOG2}"
                SLRM_OPTS="${SLRM_OPTS} --account=${CPU_ACCT} --partition=${PARTITION} --qos=${QOS}"
                ${X_SBATCH} ${SLRM_OPTS} ${SPAWNFILE} ${NML}
            fi
        elif [ "${BATCH_MODE^^}" == "FOREGROUND" ]; then
            ${SPAWNFILE} ${NML} > ${SPAWNLOG1}
        else
            ${SPAWNFILE} ${NML} > ${SPAWNLOG1} &
        fi
    done

done

echo "MSG: GPLOT wrapper completed $(/bin/date)"
