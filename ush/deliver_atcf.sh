#!/bin/sh
##SBATCH --account=aoml-hafs1
##SBATCH --nodes=1
#SBATCH --time=00:19:30
#SBATCH --partition=service
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
##SBATCH --output=/lfs4/HFIP/hur-aoml/Ghassan.Alaka/GPOUT/log/deliver_atcf.log
##SBATCH --error=/lfs4/HFIP/hur-aoml/Ghassan.Alaka/GPOUT/log/deliver_atcf.log
#SBATCH --job-name="GPLOT_deliver_atcf"
#SBATCH --mem=16G

################################################################################
#### UNIX Script Documentation Block
##
## Script name:         deliver_atcf.sh
##
## Script description:  Deliver model ATCF files to a common location for GPLOT.
##
## Author:              Ghassan J. Alaka, Jr.
##
## Date:                2020-07-14
##
## Script history log:
##
## 2020-07-14  Ghassan J. Alaka, Jr. -- Original version.
##
## Usage: deliver_atcf.sh <Input Directory> <Output Directory>
##                            <Graphics Subdirectory Name> <Forecast Cycle>
##                            <Experiment Name>
##
################################################################################

echo "MSG: deliver_atcf.sh started at `date`"

# Read command line arguments
AIN="$1"
ATMP="$2"
AOUT="$3"
ANHC="$4"
MODIN="$5"
MODOUT="$6"
YYYY="$7"
BNHC="$8"
MODI="$9"
MOD12="${10}"
FNL_HR="${11}"
NAME_REGEX="${12}"

# Define other variables
EXT2=".dat"

# Define executables
X_CREATE="/home/Ghassan.Alaka/GPLOT/ush/create_adeck.v2.sh"
X_MERGE="/home/Ghassan.Alaka/Shell/ksh/atcfMerge.ksh"

# Safety checks
if [ ! -d ${AIN} ]; then
    echo "ERROR: The ATCF input directory (1st arg) must exist."
    exit 1
fi
if [ ! -d ${ANHC} ]; then
    echo "ERROR: The ATCF NHC directory (4th arg) must exist."
    exit 2
fi
if [ -z ${MODIN} ]; then
    echo "ERROR: Input model code (5th arg) must be defined."
    exit 3
fi
if [ -z ${MODOUT} ]; then
    echo "ERROR: Output model code (6th arg) must be defined."
    exit 4
fi
if [ -z ${YYYY} ]; then
    echo "ERROR: Year (7th arg) must be defined."
    exit 5
fi
if [ ! -z "${BNHC}" ]; then
    echo "MSG: Early forecasts will be produced via NHC interpolation software."
    DO_INTERP="YES"
    if [ -z "${MODI}" ] || [ -z "${MOD12}" ]; then
        echo "ERROR: Interpolated model codes are required."
    fi
else
    echo "MSG: Early forecasts will not be produced."
    DO_INTERP="NO"
fi
if [ -z "${NAME_REGEX}" ]; then
    NAME_REGEX=".*/[a-z]\+[0-9]\{2\}[a-z]\.[0-9]\{10\}\.trak\..*\.atcfunix"
fi

# Create directory names
mkdir -p ${ATMP}
mkdir -p ${AOUT}

# Create the combined A-Deck for each storm
echo "MSG: Combining individual ATCF files --> ${X_CREATE} -o ${ATMP} -i ${AIN} -n \"${NAME_REGEX}\" -m1 \"${MODIN}\" -m2 \"${MODOUT}\""
${X_CREATE} -o ${ATMP} -i ${AIN} -n "${NAME_REGEX}" -m1 "${MODIN}" -m2 "${MODOUT}"

# Update the model code in each A-Deck, if necessary
if [ "${MODIN}" != "${MODOUT}" ]; then
    DECKS=( `find ${ATMP}/. -type f -regextype sed -regex ".*/a[a-z]\{2\}[0-9]\{6\}${EXT2}" -printf "%f\n" `  )
    for D in "${DECKS[@]}"; do
        TMP="$(date +%N).${D}"
        sed 's/'"${MODIN}"'/'"${MODOUT}"'/g' ${ATMP}/${D} > ${ATMP}/${TMP}
        DIFF=$(diff ${ATMP}/${D} ${ATMP}/${TMP})
        if [ "${DIFF}" != "" ]; then
            mv ${ATMP}/${TMP} ${ATMP}/${D}
        else
            rm -f ${ATMP}/${TMP}
        fi
    done
fi

# Copy the interplation software and define required vars.
if [ "${DO_INTERP}" == "YES" ]; then
    cp -rp /home/Ghassan.Alaka/GPLOT/sorc/NHC_interp ${ATMP}/.
    INTERP_NML="intrfcst.input"
    INTERP_EXE="run.sh"
    INTERP_DIR="${ATMP}/NHC_interp"
    INTERP_ADIR="${ATMP}/NHC_interp/atcf"
    mkdir -p ${INTERP_ADIR}
fi

# Merge the NHC A-Deck
echo "*********************************************"
echo "MSG: PART ONE - MERGE THE NHC A_DECK"
NHC_DECKS=( `find ${ANHC}/. -mmin -10080 -name "a[a-z][a-z][0-9][0-9]${YYYY}${EXT2}" -type f -print0 | xargs -0 -r ls -t | xargs -r -L1 basename` )
if [ -z "${NHC_DECKS}" ]; then
    echo "WARNING: Couldn't find any recent NHC a-decks. This might be OK."
fi

for D in "${NHC_DECKS[@]}"; do
    echo "MSG: Working on this NHC a-deck --> ${D}"
    if [ -f ${AOUT}/${D} ]; then
        TMP="$(date +%N).${D}"

        # Wait for other processes to release the lock, then create a new lock.
        LOCK_FILE="${AOUT}/${D}.lock"
        T=0
        while [ -f ${LOCK_FILE} ] && [ "$T" -lt 120 ]; do
            echo "MSG: Output A-Deck is locked. Sleeping 5 seconds..."
            sleep 5
            T=`expr $T + 5`
        done
        if [ $T -ge 120 ]; then
            echo "ERROR: I've been waiting too long for this file to unlock. Something went wrong."
            exit 100
        fi
        echo "MSG: Output A-Deck is unlocked. Locking it."
        lockfile -r-1 -l 180 ${LOCK_FILE}

        # Merge the existing A-Deck with the A-Deck for this experiment. Remove duplicate entries.
        cat ${ANHC}/${D} ${AOUT}/${D} | tac | sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > ${AOUT}/${TMP}

        DIFF=$(diff ${AOUT}/${TMP} ${AOUT}/${D})
        if [ "${DIFF}" != "" ]; then
            echo "MSG: Moving new deck to target location"
            echo "MSG: ${AOUT}/${TMP} --> ${AOUT}/${D}"
            mv ${AOUT}/${TMP} ${AOUT}/${D}
            if [ ! -f ${AOUT}/${D} ]; then
                echo "ERROR: The output a-deck doesn't exist. Something went wrong."
                exit 6
            fi
        else
            echo "MSG: No differences, so keeping old deck --> ${AOUT}/${D}"
            echo "MSG: Removing this temporary file --> ${AOUT}/${TMP}"
            rm -f ${AOUT}/${TMP}
        fi

        # Release the lock.
        rm -f ${LOCK_FILE}
        echo "MSG: Output A-Deck processing complete. Unlocking it."

    else
        echo "MSG: Target file doesn't exist, so copy NHC deck to target location."
        cp -p ${ANHC}/${D} ${AOUT}/${D}
    fi
done

# Merge the model A-Deck
echo ""
echo "*********************************************"
echo "MSG: PART TWO - MERGE THE MODEL A_DECK"
M_DECKS=( `find ${ATMP}/ -maxdepth 1 -mmin -10080 -type f -name "a[a-z][a-z][0-9][0-9]${YYYY}${EXT2}" -print0 | xargs -0 -r ls -t | xargs -r -L1 basename` )
if [ -z "${M_DECKS}" ]; then
    echo "WARNING: Couldn't find any recent model a-decks. This might be OK."
fi
for D in "${M_DECKS[@]}"; do
    echo "MSG: Working on this model a-deck --> ${D}"

    # Define Storm ID (e.g., al022020) and B-Deck file name
    SID="`echo "${D}" | cut -c2-9`"
    B="b`echo "${D}" | cut -c2-`"

    # Merge the model specific A-Deck into the output A-Deck
    if [ -f ${AOUT}/${D} ]; then
        TMP="$(date +%N).${D}"

        # Wait for other processes to release the lock, then create a new lock.
        LOCK_FILE="${AOUT}/${D}.lock"
        T=0
        while [ -f ${LOCK_FILE} ] && [ "$T" -lt 120 ]; do
            echo "MSG: Output A-Deck is locked. Sleeping 5 seconds..."
            sleep 5
            T=`expr $T + 5`
        done
        if [ $T -ge 120 ]; then
            echo "ERROR: I've been waiting too long for this file to unlock. Something went wrong."
            exit 100
        fi
        echo "MSG: Output A-Deck is unlocked. Locking it."
        lockfile -r-1 -l 180 ${LOCK_FILE}

        # Merge the existing A-Deck with the A-Deck for this experiment. Remove duplicate entries.
        cat ${AOUT}/${D} ${ATMP}/${D} | tac | sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > ${AOUT}/${TMP}

        DIFF=$(diff ${AOUT}/${TMP} ${AOUT}/${D})
        if [ "${DIFF}" != "" ]; then
            echo "MSG: Moving new deck to target location"
            echo "MSG: ${AOUT}/${TMP} --> ${AOUT}/${D}"
            mv ${AOUT}/${TMP} ${AOUT}/${D}
            if [ ! -f ${AOUT}/${D} ]; then
                echo "ERROR: This a-deck should exist, but doesn't. Something went wrong."
                exit 6
            fi
            echo "MSG: Output A-Deck processing complete. Unlocking it."
        else
            echo "MSG: No differences, so keeping old deck --> ${AOUT}/${D}"
            echo "MSG: Removing this temporary file --> ${AOUT}/${TMP}"
            rm -f ${AOUT}/${TMP}
        fi

        # Release the lock.
        rm -f ${LOCK_FILE}
        echo "MSG: Output A-Deck processing complete. Unlocking it."

    else
        echo "MSG: Target file doesn't exist, so copy model deck to target location."
        cp -p ${ATMP}/${D} ${AOUT}/${D}
    fi

    # Enter this block if early forecast interpolation is requested.
    # The Best track file must be available.
    if [ "${DO_INTERP}" == "YES" ] && [ -f ${BNHC}/${B} ]; then

        # Define the output file name
        INTERP_FILE="a${SID}.gun"

        # Link in A-Deck and B-Deck files
        ln -sf ${BNHC}/${B} ${INTERP_ADIR}/${B}

        # Find all cycles in the A-Deck
        ALL_CYCLES=( `awk -F ',' '{ print $3 }' ${AOUT}/${D} | tr -d ' ' | sort -u` )

        # Update the namelist.
        cp -p ${INTERP_DIR}/${INTERP_NML}.template ${INTERP_DIR}/${INTERP_NML}
        sed -i 's/AAAA/'"${MODOUT}"'/g' ${INTERP_DIR}/${INTERP_NML}
        sed -i 's/BBBB/'"${MOD12}"'/g' ${INTERP_DIR}/${INTERP_NML}
        sed -i 's/CCCC/'"${MODI}"'/g' ${INTERP_DIR}/${INTERP_NML}
        sed -i 's/DDD/006/g' ${INTERP_DIR}/${INTERP_NML}
        #sed -i 's/EEE/'"${FNL_HR}"'/g' ${INTERP_DIR}/${INTERP_NML}
        sed -i 's/EEE/096/g' ${INTERP_DIR}/${INTERP_NML}

        # Update the executable.
        cp -p ${INTERP_DIR}/${INTERP_EXE}.template ${INTERP_DIR}/${INTERP_EXE}
        sed -i 's@AAAA@'"${INTERP_DIR}"'@g' ${INTERP_DIR}/${INTERP_EXE}
        sed -i 's@BBBB@'"${INTERP_ADIR}"'@g' ${INTERP_DIR}/${INTERP_EXE}

        # Loop over all cycles
        for CYCLE in ${ALL_CYCLES[@]}; do

            # Find the cycles for 6-h and 12-h interpolation
            YYYY=`echo "${CYCLE}" | cut -c1-4`
            MM=`echo "${CYCLE}" | cut -c5-6`
            DD=`echo "${CYCLE}" | cut -c7-8`
            HH=`echo "${CYCLE}" | cut -c9-10`
            CYCLE2="${YYYY}-${MM}-${DD} ${HH}:00 UTC"
            CYCLE06="`date -d "${CYCLE2} + 6 hours" +'%Y%m%d%H'`"
            CYCLE12="`date -d "${CYCLE2} + 12 hours" +'%Y%m%d%H'`"

            # Lock the interpolation A-Deck
            TMP="$(date +%N).${D}"
            LOCK_FILE="${INTERP_ADIR}/${D}.lock"
            T=0
            while [ -f ${LOCK_FILE} ] && [ "$T" -lt 120 ]; do
                echo "MSG: Interp. A-Deck is locked. Sleeping 5 seconds..."
                sleep 5
                T=`expr $T + 5`
            done
            if [ $T -ge 120 ]; then
                echo "ERROR: I've been waiting too long for this file to unlock. Something went wrong."
                exit 100
            fi
            echo "MSG: Interp. A-Deck is unlocked. Locking it."
            lockfile -r-1 -l 180 ${LOCK_FILE}

            # Create an A-Deck with CARQ and the current model forecast.
            grep "${CYCLE}" ${AOUT}/${D} | grep "${MODOUT}" > ${INTERP_ADIR}/${TMP}
            grep "CARQ" ${AOUT}/${D} >> ${INTERP_ADIR}/${TMP}
            tac ${INTERP_ADIR}/${TMP} | sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > ${INTERP_ADIR}/${D}
            rm -f ${INTERP_ADIR}/${TMP}

            # Run the interpolation software.
            echo "MSG: Running this command for 6-h interpolation: ${INTERP_EXE} ${SID} ${CYCLE06} ${INTERP_NML}"
            rm -f ${INTERP_ADIR}/${INTERP_FILE}
            #${INTERP_DIR}/${INTERP_EXE} ${SID} doall ${INTERP_NML} &> ${INTERP_ADIR}/a${SID}.log
            ${INTERP_DIR}/${INTERP_EXE} ${SID} ${CYCLE06} ${INTERP_NML} &>> ${INTERP_ADIR}/a${SID}.log

            # Release the lock now that interpolation has completed.    
            rm -f ${LOCK_FILE}
            echo "MSG: Interp. A-Deck processing complete. Unlocking it."

            # Merge the output into the existing A-Deck
            if [ -f ${INTERP_ADIR}/${INTERP_FILE} ]; then
                TMP="$(date +%N).${D}"

                # Wait for other processes to release the lock, then create a new lock.
                LOCK_FILE="${AOUT}/${D}.lock"
                T=0
                while [ -f ${LOCK_FILE} ] && [ "$T" -lt 120 ]; do
                    echo "MSG: Output A-Deck is locked. Sleeping 5 seconds..."
                    sleep 5
                    T=`expr $T + 5`
                done
                if [ $T -ge 120 ]; then
                    echo "ERROR: I've been waiting too long for this file to unlock. Something went wrong."
                    exit 100
                fi
                echo "MSG: Output A-Deck is unlocked. Locking it."
                lockfile -r-1 -l 180 ${LOCK_FILE}

                # Merge the existing A-Deck with the A-Deck for this experiment. Remove duplicate entries.
                cat ${AOUT}/${D} ${INTERP_ADIR}/${INTERP_FILE} | tac | sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > ${AOUT}/${TMP}

                DIFF=$(diff ${AOUT}/${TMP} ${AOUT}/${D})
                if [ "${DIFF}" != "" ]; then
                    echo "MSG: Moving new deck to target location"
                    echo "MSG: ${AOUT}/${TMP} --> ${AOUT}/${D}"
                    mv ${AOUT}/${TMP} ${AOUT}/${D}
                    if [ ! -f ${AOUT}/${D} ]; then
                        echo "ERROR: This a-deck should exist, but doesn't. Something went wrong."
                        exit 6
                    fi
                else
                    echo "MSG: No differences, so keeping old deck --> ${AOUT}/${D}"
                    echo "MSG: Removing this temporary file --> ${AOUT}/${TMP}"
                    rm -f ${AOUT}/${TMP}
                fi

                # Release the lock.
                rm -f ${LOCK_FILE}
                echo "MSG: Output A-Deck processing complete. Unlocking it."

            fi
    
            # Run the interpolation software.
            echo "MSG: Running this command for 12-h interpolation: ${INTERP_EXE} ${SID} ${CYCLE12} ${INTERP_NML}"
            rm -f ${INTERP_ADIR}/${INTERP_FILE}
            #${INTERP_DIR}/${INTERP_EXE} ${SID} doall ${INTERP_NML} &> ${INTERP_ADIR}/a${SID}.log
            ${INTERP_DIR}/${INTERP_EXE} ${SID} ${CYCLE12} ${INTERP_NML} &>> ${INTERP_ADIR}/a${SID}.log
    
            # Merge the output into the existing A-Deck
            if [ -f ${INTERP_ADIR}/${INTERP_FILE} ]; then
                TMP="$(date +%N).${D}"

                # Wait for other processes to release the lock, then create a new lock.
                LOCK_FILE="${AOUT}/${D}.lock"
                T=0
                while [ -f ${LOCK_FILE} ] && [ "$T" -lt 120 ]; do
                    echo "MSG: Output A-Deck is locked. Sleeping 5 seconds..."
                    sleep 5
                    T=`expr $T + 5`
                done
                if [ $T -ge 120 ]; then
                    echo "ERROR: I've been waiting too long for this file to unlock. Something went wrong."
                    exit 100
                fi
                echo "MSG: Output A-Deck is unlocked. Locking it."
                lockfile -r-1 -l 180 ${LOCK_FILE}

                # Merge the existing A-Deck with the A-Deck for this experiment. Remove duplicate entries.
                cat ${INTERP_ADIR}/${INTERP_FILE} ${AOUT}/${D} | sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > ${AOUT}/${TMP}

                DIFF=$(diff ${AOUT}/${TMP} ${AOUT}/${D})
                if [ "${DIFF}" != "" ]; then
                    echo "MSG: Moving new deck to target location"
                    echo "MSG: ${AOUT}/${TMP} --> ${AOUT}/${D}"
                    mv ${AOUT}/${TMP} ${AOUT}/${D}
                    if [ ! -f ${AOUT}/${D} ]; then
                        echo "ERROR: This a-deck should exist, but doesn't. Something went wrong."
                        exit 6
                    fi
                else
                    echo "MSG: No differences, so keeping old deck --> ${AOUT}/${D}"
                    echo "MSG: Removing this temporary file --> ${AOUT}/${TMP}"
                    rm -f ${AOUT}/${TMP}
                fi

                # Release the lock.
                rm -f ${LOCK_FILE}
                echo "MSG: Output A-Deck processing complete. Unlocking it."

            fi

        done

    fi



done
#${X_MERGE} ${MODOUT} ${AOUT} ${EXT2} 1 ${ATMP}/*${YYYY}${EXT2}

# Remove NHC Interpolation subdirectory.
#if [ "${DO_INTERP}" == "YES" ]; then
#    rm -rf ${ATMP}/NHC_interp
#fi


echo "MSG: deliver_atcf.sh completed at `date`"
