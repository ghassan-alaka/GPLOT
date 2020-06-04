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
    echo "ERROR: No experiment namelists found via the command line."
    exit
else
    echo "MSG: Experiment namelist(s) found via the command line."
    NML_LIST=( "$@" )
fi
echo "MSG: Found these experiment namelists:"
echo "${NML_LIST[@]}"


# Loop over all experiment namelists.
for NML in "${NML_LIST[@]}"; do
    echo "MSG: GPLOT is working on this experiment namelist --> ${NML}"


    # Define the master namelist file name.
    # If this namelist is not found in $GPLOT_DIR/nmlist/,
    # the submission will fail.
    #NML="namelist.master.${d}"
    echo "MSG: Master namelist --> ${NML}"
    if [ ! -f "${NML}" ]; then
        echo "WARNING: Master namelist file could not be found."
        echo "WARNING: Can't submit anything for this experiment."
        echo "WARNING: Skipping to next experiment namelist."
        continue
    fi
    echo "MSG: Master namelist found. Hooray!"


    # Determine the components of GPLOT that should be submitted.
    # These options currently include:  Maps, Ships, Stats, Polar
    DO_MAPS=`sed -n -e 's/^DO_MAPS =\s//p' ${NML} | sed 's/^\t*//'`
    DO_STATS=`sed -n -e 's/^DO_STATS =\s//p' ${NML} | sed 's/^\t*//'`
    DO_SHIPS=`sed -n -e 's/^DO_SHIPS =\s//p' ${NML} | sed 's/^\t*//'`
    DO_POLAR=`sed -n -e 's/^DO_POLAR =\s//p' ${NML} | sed 's/^\t*//'`


    # Get the submission mode from the namelist.
    BATCH_MODE=`sed -n -e 's/^BATCH_MODE =\s//p' ${NML} | sed 's/^\t*//' | tr a-z A-Z`
    if [ -z "${BATCH_MODE}" ]; then
        BATCH_MODE="BACKGROUND"
    fi
    echo "MSG: BATCH_MODE --> ${BATCH_MODE}."


    # Get the system environment and project account for batch submissions ($BATCH_MODE="SBATCH")
    SYS_ENV=`sed -n -e 's/^SYS_ENV =\s//p' ${NML} | sed 's/^\t*//'`
    if [ "${BATCH_MODE}" == "SBATCH" ]; then
        BATCH_DFLTS="batch.defaults.${SYS_ENV,,}"
        AUTO_BATCH=`sed -n -e 's/^AUTO_BATCH =\s//p' ${NML} | sed 's/^\t*//'`
        CPU_ACCT=`sed -n -e 's/^CPU_ACCT =\s//p' ${NML} | sed 's/^\t*//'`
        if [ -z ${CPU_ACCT} ] || [ "${AUTO_BATCH}" = "True" ]; then
            CPU_ACCT=`sed -n -e 's/^CPU_ACCT =\s//p' ${NMLDIR}${BATCH_DFLTS} | sed 's/^\t*//'`
        fi
        QOS=`sed -n -e 's/^QOS =\s//p' ${NML} | sed 's/^\t*//'`
        if [ -z ${QOS} ] || [ "${AUTO_BATCH}" = "True" ]; then
            QOS=`sed -n -e 's/^QOS =\s//p' ${NMLDIR}${BATCH_DFLTS} | sed 's/^\t*//'`
        fi
        PARTITION=`sed -n -e 's/^PARTITION =\s//p' ${NML} | sed 's/^\t*//'`
        if [ -z ${PARTITION} ] || [ "${AUTO_BATCH}" = "True" ]; then
            PARTITION=`sed -n -e 's/^PARTITION =\s//p' ${NMLDIR}${BATCH_DFLTS} | sed 's/^\t*//'`
        fi
    fi


    # This part submits the spawn file for MAPS
    if [ "${DO_MAPS}" = "True" ]; then
        echo "MSG: MAPS submission is turned on."
        SPAWNFILE1="${BATCHDIR}/spawn_maps.generic.sh"
        SPAWNFILE2="${WORKhafs}/graphics/GPLOT.spawn_maps.${SUBEXPT}.sh"
        cp ${SPAWNFILE1} ${SPAWNFILE2}
        SPAWNLOG="${WORKhafs}/graphics/GPLOT.spawn_maps.${SUBEXPT}.log"
        echo "MSG: Spawn file --> ${SPAWNFILE2}"
        echo "MSG: Spawn log --> ${SPAWNLOG}"
        if [ "${BATCH_MODE}" == "SBATCH" ]; then
            if squeue -u $USER -o "%.10i %.10P %.50j" | grep -q "GPLOT.spawn_maps.${SUBEXPT}"; then
                id=( `squeue -u $USER -o "%.10i %.10P %.50j" | grep "GPLOT.spawn_maps.${SUBEXPT}" | sed 's/^[ \t]*//g' | cut -d' ' -f1` ) # 2>/dev/null 2>&1` )
                echo "MSG: Batch job(s) already exist(s) --> ${id[*]}"
                echo "MSG: Not submitting anything for MAPS."
            else
                sed -i 's/^#SBATCH --job-name=.*/#SBATCH --job-name="GPLOT.spawn_maps.'"${SUBEXPT}"'/g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --output=.*/#SBATCH --output="'"${WORKhafs}"'/graphics/GPLOT.spawn_maps.out"/g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --error=.*/#SBATCH --error="'"${WORKhafs}"'/graphics/GPLOT.spawn_maps.err"/g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --account=.*/#SBATCH --account='"${CPU_ACCT}"'/g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --partition=.*/#SBATCH --partition='"${PARTITION}"'/g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --qos=.*/#SBATCH --qos='"${QOS}"'/g' ${SPAWNFILE2}
                exit
                sbatch ${SPAWNFILE2} ${NML} &> ${SPAWNLOG}
            fi
        elif [ "${BATCH_MODE}" == "FOREGROUND" ]; then
            pwd
            ls
	    `pwd`/${SPAWNFILE2} ${NML} &> `pwd`/${SPAWNLOG}
        else
	    ${SPAWNFILE2} ${NML} > ${SPAWNLOG} &
        fi
    fi


    # This part submits the spawn file for SHIPS
    if [ "${DO_SHIPS}" = "True" ]; then
        echo "MSG: SHIPS submission is turned on."
        SPAWNFILE1="${BATCHDIR}/spawn_ships.generic.sh"
        SPAWNFILE2="${WORKhafs}/graphics/spawn_ships.${SUBEXPT}.sh"
        cp ${SPAWNFILE1} ${SPAWNFILE2}
        SPAWNLOG="${WORKhafs}/graphics/GPLOT.spawn_ships.${SUBEXPT}.log"
        echo "MSG: Spawn file --> ${SPAWNFILE2}"
        echo "MSG: Spawn log -->  ${SPAWNLOG}"
        if [ "${BATCH_MODE}" == "SBATCH" ]; then
            if squeue -u $USER -o "%.10i %.10P %.50j" | grep -q "GPLOT.spawn_ships.${SUBEXPT}"; then
                id=( `squeue -u $USER -o "%.10i %.10P %.50j" | grep "GPLOT.spawn_ships.${SUBEXPT}" | sed 's/^[ \t]*//g' | cut -d' ' -f1` ) # 2>/dev/null 2>&1` )
                echo "MSG: Batch job(s) already exist(s) --> ${id[*]}"
                echo "MSG: Not submitting anything for SHIPS."
            else
                
                sed -i 's/^#SBATCH --job-name=.*/#SBATCH --job-name="GPLOT.spawn_ships.'"${SUBEXPT}"'"/g' ${SPAWNFILE2}
                sed -i 's@^#SBATCH --output=.*@#SBATCH --output="'"${WORKhafs}"'/graphics/spawn_ships.'"${SUBEXPT}"'.out"@g' ${SPAWNFILE2}
                sed -i 's@^#SBATCH --error=.*@#SBATCH --error="'"${WORKhafs}"'/graphics/spawn_ships.'"${SUBEXPT}"'.err"@g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --account=.*/#SBATCH --account='"${CPU_ACCT}"'/g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --partition=.*/#SBATCH --partition='"${PARTITION}"'/g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --qos=.*/#SBATCH --qos='"${QOS}"'/g' ${SPAWNFILE2}
                sbatch ${SPAWNFILE2} ${NML} > ${SPAWNLOG}
            fi
        elif [ "${BATCH_MODE}" == "FOREGROUND" ]; then
            ${SPAWNFILE2} ${NML} > ${SPAWNLOG}
        else
            ${SPAWNFILE2} ${NML} > ${SPAWNLOG} &
        fi
    fi


    # This part submits the spawn file for STATS
    if [ "${DO_STATS}" = "True" ]; then
        echo "MSG: STATS submission is turned on."
        SPAWNFILE1="${BATCHDIR}/spawn_stats.generic.sh"
        SPAWNFILE2="${WORKhafs}/graphics/spawn_stats.${SUBEXPT}.sh"
        cp ${SPAWNFILE1} ${SPAWNFILE2}
        SPAWNLOG="${WORKhafs}/graphics/spawn_stats.${SUBEXPT}.log"
        echo "MSG: Spawn file --> ${SPAWNFILE2}"
        echo "MSG: Spawn log -->  ${SPAWNLOG}"
        if [ "${BATCH_MODE}" == "SBATCH" ]; then
            if squeue -u $USER -o "%.10i %.10P %.50j" | grep -q "GPLOT.spawn_stats.${SUBEXPT}"; then
                id=( `squeue -u $USER -o "%.10i %.10P %.50j" | grep "GPLOT.spawn_stats.${SUBEXPT}" | sed 's/^[ \t]*//g' | cut -d' ' -f1` ) # 2>/dev/null 2>&1` )
                echo "MSG: Batch job(s) already exist(s) --> ${id[*]}"
                echo "MSG: Not submitting anything for STATS."
            else
                sed -i 's/^#SBATCH --job-name=.*/#SBATCH --job-name="GPLOT.spawn_stats.'"${SUBEXPT}"'"/g' ${SPAWNFILE2}
                sed -i 's@^#SBATCH --output=.*@#SBATCH --output="'"${WORKhafs}"'/graphics/spawn_stats.'"${SUBEXPT}"'.out"@g' ${SPAWNFILE2}
                sed -i 's@^#SBATCH --error=.*@#SBATCH --error="'"${WORKhafs}"'/graphics/spawn_stats.'"${SUBEXPT}"'.err"@g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --account=.*/#SBATCH --account='"${CPU_ACCT}"'/g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --partition=.*/#SBATCH --partition='"${PARTITION}"'/g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --qos=.*/#SBATCH --qos='"${QOS}"'/g' ${SPAWNFILE2}
                sbatch ${SPAWNFILE2} ${NML} > ${SPAWNLOG}
            fi
        elif [ "${BATCH_MODE}" == "FOREGROUND" ]; then
            ${SPAWNFILE2} ${NML} > ${SPAWNLOG}
        else
            ${SPAWNFILE2} ${NML} > ${SPAWNLOG} &
        fi
    fi


    # This part submits the spawn file for POLAR
    if [ "${DO_POLAR}" = "True" ]; then
        echo "MSG: POLAR submission is turned on."
        SPAWNFILE1="${BATCHDIR}/spawn_polar.generic.sh"
        SPAWNFILE2="${WORKhafs}/graphics/spawn_polar.${SUBEXPT}.sh"
        cp ${SPAWNFILE1} ${SPAWNFILE2}
        SPAWNLOG="${WORKhafs}/graphics/spawn_polar.${SUBEXPT}.log"
        echo "MSG: Spawn file --> ${SPAWNFILE2}"
        echo "MSG: Spawn log --> ${SPAWNLOG}"
        if [ "${BATCH_MODE}" == "SBATCH" ]; then
            if squeue -u $USER -o "%.10i %.10P %.50j" | grep -q "GPLOT.spawn_polar.${SUBEXPT}"; then
                id=( `squeue -u $USER -o "%.10i %.10P %.50j" | grep "GPLOT.spawn_polar.${SUBEXPT}" | sed 's/^[ \t]*//g' | cut -d' ' -f1` ) # 2>/dev/null 2>&1` )
                echo "MSG: Batch job(s) already exist(s) --> ${id[*]}"
                echo "MSG: Not submitting anything for POLAR."
            else
                sed -i 's/^#SBATCH --job-name=.*/#SBATCH --job-name="GPLOT.spawn_polar.'"${SUBEXPT}"'"/g' ${SPAWNFILE2}
                sed -i 's@^#SBATCH --output=.*@#SBATCH --output="'"${WORKhafs}"'/graphics/spawn_polar.'"${SUBEXPT}"'.out"@g' ${SPAWNFILE2}
                sed -i 's@^#SBATCH --error=.*@#SBATCH --error="'"${WORKhafs}"'/graphics/spawn_polar.'"${SUBEXPT}"'.err"@g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --account=.*/#SBATCH --account='"${CPU_ACCT}"'/g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --partition=.*/#SBATCH --partition='"${PARTITION}"'/g' ${SPAWNFILE2}
                sed -i 's/^#SBATCH --qos=.*/#SBATCH --qos='"${QOS}"'/g' ${SPAWNFILE2}
                sbatch ${SPAWNFILE2} ${NML} > ${SPAWNLOG}
            fi
        elif [ "${BATCH_MODE}" == "FOREGROUND" ]; then
            ${SPAWNFILE2} ${NML} > ${SPAWNLOG}
        else
            ${SPAWNFILE2} ${NML} > ${SPAWNLOG} &
        fi
    fi
done

echo "MSG: GPLOT wrapper completed $(/bin/date)"
