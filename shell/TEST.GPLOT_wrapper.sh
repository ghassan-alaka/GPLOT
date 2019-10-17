#!/bin/sh --login
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
if [ -z "${GPLOT_DIR}" ]; then
    GPLOT_DIR="/home/${USER}/GPLOT"
    echo "WARNING: Variable GPLOT_DIR not found in environment."
    echo "MSG: Setting GPLOT_DIR --> ${GPLOT_DIR}"
else
    echo "MSG: GPLOT_DIR found in environment --> ${GPLOT_DIR}"
fi


# Test that GPLOT_DIR actually exists. If not, we can't continue.
if [ ! -d "$GPLOT_DIR" ]; then
    echo "ERROR: GPLOT_DIR not found. Can't continue."
    exit
fi


# Create variables for GPLOT subdirectories.
NMLDIR="${GPLOT_DIR}/nmlist/"
BATCHDIR="${GPLOT_DIR}/batch/"
LOGDIR="${GPLOT_DIR}/log/"


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
    EXPT=( "HB18_v3_history" "GFS_Forecast" )
else
    echo "MSG: Experiment found via the command line."
    EXPT=( "$@" )
fi
if [ -z "$EXPT" ]; then
    echo "ERROR: No experiments found. Something went wrong."
    exit
fi
echo "MSG: Found these experiments --> ${EXPT[*]}"


# Loop over all experiments
for d in "${EXPT[@]}"; do
    echo "MSG: GPLOT is working on this experiment --> $d"


    # Define the master namelist file name.
    # If this namelist is not found in $GPLOT_DIR/nmlist/,
    # the submission will fail.
    NML="namelist.master.${d}"
    echo "MSG: Master namelist --> ${NMLDIR}${NML}"
    if [ -z "${NMLDIR}${NML}" ]; then
        echo "ERROR: Master namelist could not be found."
        echo "ERROR: Can't submit anything for this experiment."
        continue
    fi
    echo "MSG: Master namelist found. Hooray!"


    # Determine the components of GPLOT that should be submitted.
    # These options currently include:  Maps, Ships, Stats, Polar
    DO_MAPS=`sed -n -e 's/^.*DO_MAPS =\s//p' ${NMLDIR}${NML} | sed 's/^\t*//'`
    DO_STATS=`sed -n -e 's/^.*DO_STATS =\s//p' ${NMLDIR}${NML} | sed 's/^\t*//'`
    DO_SHIPS=`sed -n -e 's/^.*DO_SHIPS =\s//p' ${NMLDIR}${NML} | sed 's/^\t*//'`
    DO_POLAR=`sed -n -e 's/^.*DO_POLAR =\s//p' ${NMLDIR}${NML} | sed 's/^\t*//'`

    BATCH_MODE=`sed -n -e 's/^.*BATCH_MODE =\s//p' ${NMLDIR}${NML} | sed 's/^\t*//' | tr a-z A-Z`
    if [ -z "${BATCH_MODE}" ]; then
        BATCH_MODE="BACKGROUND"
    fi
    echo "MSG: BATCH_MODE --> ${BATCH_MODE}."


    # This part submits the spawn file for MAPS
    if [ "${DO_MAPS}" = "True" ]; then
        echo "MSG: MAPS submission is turned on."
        SPAWNFILE1="spawn_maps.generic.sh"
        SPAWNFILE2="spawn_maps.${d}.sh"
        cp ${BATCHDIR}${SPAWNFILE1} ${BATCHDIR}${SPAWNFILE2}
        SPAWNLOG="spawn_maps.${d}.log"
        echo "MSG: Spawn file --> ${BATCHDIR}${SPAWNFILE2}"
        echo "MSG: Spawn log --> ${LOGDIR}${SPAWNLOG}"
        if [ "${BATCH_MODE}" == "SBATCH" ]; then
            perl -pi -e "s/^#SBATCH --job-name=.*/#SBATCH --job-name=\"GPLOT.spawn_maps.${d}\"/g" ${BATCHDIR}${SPAWNFILE2}
            perl -pi -e "s/^#SBATCH --output=.*/#SBATCH --output=\"${LOGDIR////\/}spawn_maps.${d}.out\"/g" ${BATCHDIR}${SPAWNFILE2}
            perl -pi -e "s/^#SBATCH --error=.*/#SBATCH --error=\"${LOGDIR////\/}spawn_maps.${d}.err\"/g" ${BATCHDIR}${SPAWNFILE2}
            sbatch ${BATCHDIR}${SPAWNFILE2} ${NML} > ${LOGDIR}${SPAWNLOG}
        elif [ "${BATCH_MODE}" == "FOREGROUND" ]; then
	    ${BATCHDIR}${SPAWNFILE2} ${NML} > ${LOGDIR}${SPAWNLOG}
        else
	    ${BATCHDIR}${SPAWNFILE2} ${NML} > ${LOGDIR}${SPAWNLOG} &
        fi
    fi


    # This part submits the spawn file for SHIPS
    if [ "${DO_SHIPS}" = "True" ]; then
        echo "MSG: SHIPS submission is turned on."
        SPAWNFILE1="spawn_ships.generic.sh"
        SPAWNFILE2="spawn_ships.${d}.sh"
        cp ${BATCHDIR}${SPAWNFILE1} ${BATCHDIR}${SPAWNFILE2}
        SPAWNLOG="spawn_ships.${d}.log"
        echo "MSG: Spawn file --> ${BATCHDIR}${SPAWNFILE2}"
        echo "MSG: Spawn log --> ${LOGDIR}${SPAWNLOG}"
        if [ "${BATCH_MODE}" == "SBATCH" ]; then
            perl -pi -e "s/^#SBATCH --job-name=.*/#SBATCH --job-name=\"GPLOT.spawn_ships.${d}\"/g" ${BATCHDIR}${SPAWNFILE2}
            perl -pi -e "s/^#SBATCH --output=.*/#SBATCH --output=\"${LOGDIR////\/}spawn_ships.${d}.out\"/g" ${BATCHDIR}${SPAWNFILE2}
            perl -pi -e "s/^#SBATCH --error=.*/#SBATCH --error=\"${LOGDIR////\/}spawn_ships.${d}.err\"/g" ${BATCHDIR}${SPAWNFILE2}
            sbatch ${BATCHDIR}${SPAWNFILE2} ${NML} > ${LOGDIR}${SPAWNLOG}
        elif [ "${BATCH_MODE}" == "FOREGROUND" ]; then
            ${BATCHDIR}${SPAWNFILE2} ${NML} > ${LOGDIR}${SPAWNLOG}
        else
            ${BATCHDIR}${SPAWNFILE2} ${NML} > ${LOGDIR}${SPAWNLOG} &
        fi
    fi


    # This part submits the spawn file for STATS
    if [ "${DO_STATS}" = "True" ]; then
        echo "MSG: STATS submission is turned on."
        SPAWNFILE1="spawn_stats.generic.sh"
        SPAWNFILE2="spawn_stats.${d}.sh"
        cp ${BATCHDIR}${SPAWNFILE1} ${BATCHDIR}${SPAWNFILE2}
        SPAWNLOG="spawn_stats.${d}.log"
        echo "MSG: Spawn file --> ${BATCHDIR}${SPAWNFILE2}"
        echo "MSG: Spawn log --> ${LOGDIR}${SPAWNLOG}"
        if [ "${BATCH_MODE}" == "SBATCH" ]; then
            perl -pi -e "s/^#SBATCH --job-name=.*/#SBATCH --job-name=\"GPLOT.spawn_stats.${d}\"/g" ${BATCHDIR}${SPAWNFILE2}
            perl -pi -e "s/^#SBATCH --output=.*/#SBATCH --output=\"${LOGDIR////\/}spawn_stats.${d}.out\"/g" ${BATCHDIR}${SPAWNFILE2}
            perl -pi -e "s/^#SBATCH --error=.*/#SBATCH --error=\"${LOGDIR////\/}spawn_stats.${d}.err\"/g" ${BATCHDIR}${SPAWNFILE2}
            sbatch ${BATCHDIR}${SPAWNFILE2} ${NML} > ${LOGDIR}${SPAWNLOG}
        elif [ "${BATCH_MODE}" == "FOREGROUND" ]; then
            ${BATCHDIR}${SPAWNFILE2} ${NML} > ${LOGDIR}${SPAWNLOG}
        else
            ${BATCHDIR}${SPAWNFILE2} ${NML} > ${LOGDIR}${SPAWNLOG} &
        fi
    fi


    # This part submits the spawn file for POLAR
    if [ "${DO_POLAR}" = "True" ]; then
        echo "MSG: POLAR submission is turned on."
        SPAWNFILE1="spawn_polar.generic.sh"
        SPAWNFILE2="spawn_polar.${d}.sh"
        cp ${BATCHDIR}${SPAWNFILE1} ${BATCHDIR}${SPAWNFILE2}
        SPAWNLOG="spawn_polar.${d}.log"
        echo "MSG: Spawn file --> ${BATCHDIR}${SPAWNFILE2}"
        echo "MSG: Spawn log --> ${LOGDIR}${SPAWNLOG}"
        if [ "${BATCH_MODE}" == "SBATCH" ]; then
            perl -pi -e "s/^#SBATCH --job-name=.*/#SBATCH --job-name=\"GPLOT.spawn_polar.${d}\"/g" ${BATCHDIR}${SPAWNFILE2}
            perl -pi -e "s/^#SBATCH --output=.*/#SBATCH --output=\"${LOGDIR////\/}spawn_polar.${d}.out\"/g" ${BATCHDIR}${SPAWNFILE2}
            perl -pi -e "s/^#SBATCH --error=.*/#SBATCH --error=\"${LOGDIR////\/}spawn_polar.${d}.err\"/g" ${BATCHDIR}${SPAWNFILE2}
            sbatch ${BATCHDIR}${SPAWNFILE2} ${NML} > ${LOGDIR}${SPAWNLOG}
        elif [ "${BATCH_MODE}" == "FOREGROUND" ]; then
            ${BATCHDIR}${SPAWNFILE2} ${NML} > ${LOGDIR}${SPAWNLOG}
        else
            ${BATCHDIR}${SPAWNFILE2} ${NML} > ${LOGDIR}${SPAWNLOG} &
        fi
    fi
done

echo "MSG: GPLOT wrapper completed $(/bin/date)"
