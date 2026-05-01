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
echo "MSG: Using this GPLOT Directory --> ${GPLOT_DIR}"
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
EID=( `sed -n -e 's/^EID =\s//p' ${NMLIST} | sed 's/^\t*//'` )
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
FORCE="`sed -n -e 's/^FORCE =\s//p' ${NMLIST} | sed 's/^\t*//'`"
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
    elif [ "${MACHINE}" == "HERA" ] || [ "${MACHINE}" == "URSA" ] || [ "${MACHINE}" == "ORION" ]; then
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
FORCE_ORIG="${FORCE}"

# Get the batch submission mode [SBATCH,BACKGROUND,FOREGROUND]
BATCH_MODE="`sed -n -e 's/^BATCH_MODE =\s//p' ${NMLIST} | sed 's/^\t*//' | tr a-z A-Z`"
if [ -z "${BATCH_MODE}" ]; then
    BATCH_MODE="SBATCH"
    echo "MSG: No batch-submission found in the namelist. DEFAULT:   --> ${BATCH_MODE}"
else
    echo "MSG: Found a batch-submission mode in the namelist   --> ${BATCH_MODE}"
fi

# Get a list of forecast lead times
FHRS=( $(seq ${INIT_HR} ${DT} ${FNL_HR} | tr "\n" " ") )
echo "MSG: Will produce graphics for these forecast lead times --> ${FHRS[*]}"

# Define a maximum number of cycles to be processed
#MAX_CYCLES=`sed -n -e 's/^MAX_CYCLES =\s//p' ${NMLIST} | sed 's/^\t*//'`
MAX_CYCLES=100

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

# Determine if this experiment has ensemble members
# Deterministic forecasts will have ENSMEM=0 in the namelist
echo "MSG: Found these ensemble members --> ${EID[*]}"
if [ -z "${EID[*]}" ]; then
    EID=( `sed -n -e 's/^ENSMEM =\s//p' ${NMLIST} | sed 's/^\t*//'` )
fi
### Matt Donahue 04/02/2026 - removed EID == 00 -> deterministic behavior
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

# Define the maximum number of batch submissions.
# This is a safeguard to avoid overloading the batch scheduler.
if [ "${IS_ENS}" == "False" ]; then
    MAX_JOBS=25
else
    MAX_JOBS=525

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


################################
# PREPARE THE LIST OF ATCF FILES

# Get the current date in YYYYMMDDHH format
DATE_NOW="`date +'%Y%m%d%H'`"

# Limit the ATCF list based onMAX_JOBS 
NATCF="${#ATCF_ALL[*]}"

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
    SC="True"





    # Define the batch submission counter.
    N=0


    ######################################
    # LOOP OVER ALL AVAILABLE ATCF FILES #
    ######################################
    ## matt 4/30 - I am going to try to refactor entirely - get rid of
    ## atcf loop and use cycle loop like others.
    #for ATCF in "${ATCF_ALL[@]}"; do

    ##################################
    # LOOP OVER ALL AVAILABLE CYCLES #
    ##################################
    for CYCLE in ${CYCLES[@]}; do


        # Only retain the numbers for the cycle
        # Parse the prefix (e.g., gfs.) if it exists.
        CPREFIX=`echo "${CYCLE}" | grep -E '^[A-Za-z0-9]*\..*$' | sed 's/\([A-Za-z0-9]*\.\)\([0-9]*\)/\1/'`
        CYCLE=`echo "${CYCLE}" | sed 's/\([A-Za-z0-9]*\.\)\([0-9]*\)/\2/'`

        # If the CYCLE is empty, skip it
        if [ -z "$CYCLE" ]; then
            echo "WARNING: The cycle is undefined. Skipping to next."
            continue
        fi
    
        # Parse the cycle into year, month, day, hour
        YYYY=`echo "${CYCLE}" | cut -c1-4`
        MM=`echo "${CYCLE}" | cut -c5-6`
        DD=`echo "${CYCLE}" | cut -c7-8`
        HH=`echo "${CYCLE}" | cut -c9-10`
        #Need CYCLE2 for date_cut logic
        CYCLE2="${YYYY}-${MM}-${DD} ${HH}:00:00"

            
        # Get the cycle prefix from a table and define CYCLE_STR
        # CYCLE_STR should be used in file paths.
        if [ -z "${CPREFIX}" ]; then
            CPREFIX=`awk -v DSRC=${DSOURCE} '($1 == DSRC) { print $2 }' ${TBL_DIR}CyclePrefix.dat`
        fi
        CYCLE_STR="${CPREFIX}${CYCLE}"

        # Find the ATCFs for the current CYCLE.
        # It will be blank if no ATCFs are found.
        CYCLE_ATCF=( `printf '%s\n' ${ATCF_ALL[@]} | grep "${CYCLE}"` )
    
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
                # end Lew + Matt changes from 2025-09-04 and 2026-04-02
            fi
    
            # 3) Try to get STORMS from the HWRF file path.
            # This is hard-coded and might not work.
            if [ -z "${STORMS[*]}" ]; then
                if [ ! -z "$(ls -d ${IDIR}${CYCLE}/[0-9][0-9][A-Z]/ 2>/dev/null)" ]; then
                    STORMS+=(`ls -d ${IDIR}${CYCLE}/[0-9][0-9][A-Z]/ | xargs -n 1 basename`)
                fi
            fi

            # 4) If STORMS is still undefined, then set it to "NONE"
            # Large-scale graphics may still proceed.
            # Storm-centered graphics will be skipped.
            if [ -z "${STORMS[*]}" ]; then
                STORMS+=("NONE")
            fi

            # 5) Remove duplicate storms, if applicable.
            STORMS=($(printf "%s\n" "${STORMS[@]}" | sort -u))
    
            # 6) Append Fake Storm (00L) if IS_MSTORM=True and if other storms
            # were found, i.e., STORMS != NONE, 2026-04-02 M.D. - only if not ENS!
            if [ "${IS_ENS}" == "False" ]; then 
                if [ "${IS_MSTORM}" == "True" ] && [ "${STORMS[*]}" != "NONE" ]; then
                    STORMS+=("00L")
                fi
            fi


            # We need a real storm to continue
            if [ -z "${STORMS[*]}" ] || [ "${STORMS[*]}" == "NONE" ]; then
                echo "WARNING: No storms were found. Can't continue."
            continue
        fi

        echo "MSG: Found these storms: ${STORMS[*]}"


        ####################
        # LOOP OVER STORMS #
        ####################
        for STORM in ${STORMS[@]}; do

            ### MATT CHANGE 7/26/2025 - only skip fake storm in deterministic
            ### Merged 04/02/2026
            if [ "${IS_ENS}" == "False" ]; then 
                # Never process the fake storm (00L)
                if [ "${STORM^^}" == "00L" ]; then
                    echo "MSG: Fake storm detected ${STORM}. Skipping."
                    continue
                fi
            fi

            # Parse important information from $STORM and $CYCLE
            #Needed to construct BDECK path
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
            # Create the B-Deck file name
            BDECK="${BDECK_DIR}b${BASIN2}${SNUM}${YYYY}.dat"

            # Computes dates in YYYYMMDDHH format. This will be used to determine if
            # production for the current cycle should be forced.
            #DATE_NOW="`date +'%Y%m%d%H'`"
            DATE_CUT="`date -d "${CYCLE2} UTC + ${FNL_HR} hours" +'%Y%m%d%H'`"

            #Also, need lowercase SID in SIDLONG for later filenames
            SIDLONG=`echo "${STORM,,}"`

            if [ "${IS_ENS}" == "False" ]; then 
                # Find the forecast hours from the ATCF for this particular storm
                # only in deterministic - Matt Donahue 04/02/2026
                STORM_ATCF=( `printf '%s\n' ${CYCLE_ATCF[@]} | grep -i "${STORM,,}.${CYCLE}" | head -1` )
                if [ ! -z "${STORM_ATCF[*]}" ]; then
                    echo "MSG: ATCF found for ${STORM} --> ${STORM_ATCF[0]}"
                fi
            else
                STORM_ATCF=( `printf '%s\n' ${CYCLE_ATCF[@]} | grep -i "00l.${CYCLE}" | head -1` )
                if [ ! -z "${STORM_ATCF[*]}" ]; then
                    echo "MSG: ATCF found for ${STORM} --> ${STORM_ATCF[0]}"
                fi
            fi
            # Set the STORMTAG for file names
            STORMTAG=".${STORM^^}"            echo ""
            echo "************************"
            echo "MSG: Working on this ATCF --> ${ATCF}"
            echo "MSG: CYCLE = ${CYCLE}, DATE_NOW = ${DATE_NOW}, DATE_CUT = ${DATE_CUT}"

            # Get file prefix information from table or namelist
            #Copied from spawn_ships while refactoring - do we need this here
            if [ -z "${ITAG}" ]; then
                FPREFIX=`awk -v DSRC=${DSOURCE} -v N=$NEST '($1 == DSRC) { print $(1+N) }' ${TBL_DIR}FilePrefix.dat`
            else
                FPREFIX="${ITAG}"
            fi
            if [ -z "${FPREFIX}" ]; then
                echo ""
                echo "MSG: Current cycle       --> ${CYCLE}"
                echo "MSG: Current storm       --> ${STORM}"
                echo "ERROR: File prefix not found for ${DSOURCE}."
                echo "ERROR: Please add your DSOURCE to ${TBL_DIR}FilePrefix.dat."
                echo "ERROR: Or define ITAG in the namelist."
                exit
            fi

            # Get file hour string information from table or namelist
            #Copied from spawn_ships while refactoring - do we need this here
            if [ -z "${FHRSTR}" ]; then
                FHRSTR=`awk -v DSRC=${DSOURCE} '($1 == DSRC) { print $2 }' ${TBL_DIR}FileTimeFormat.dat`
            else
                FHRSTR="${FHRSTR}"
            fi
            if [ -z "${FHRSTR}" ]; then
                echo ""
                echo "MSG: Current cycle       --> ${CYCLE}"
                echo "MSG: Current storm       --> ${STORM}"
                echo "ERROR: File hour string not found for ${DSOURCE}."
                echo "ERROR: Please add your DSOURCE to ${TBL_DIR}FileTimeFormat.dat."
                echo "ERROR: Or define FHRSTR in the namelist."
                exit
            fi
                
            # Get file hour format information from table or namelist
            #Copied from spawn_ships while refactoring - do we need this here
            if [ -z "${FHRFMT}" ]; then
                FHRFMT="%0`awk -v DSRC=${DSOURCE} '($1 == DSRC) { print $3 }' ${TBL_DIR}FileTimeFormat.dat`d"
            elif [ "${FHRFMT:0:1}" != "%" ]; then
                FHRFMT="%0${FHRFMT}d"
            fi
            if [ -z "${FHRFMT}" ]; then
                echo ""
                echo "MSG: Current cycle       --> ${CYCLE}"
                echo "MSG: Current storm       --> ${STORM}"
                echo "ERROR: File hour format not found for ${DSOURCE}."
                echo "ERROR: Please add your DSOURCE to ${TBL_DIR}FileTimeFormat.dat."
                echo "ERROR: Or define FHRFMT in the namelist."
                exit
            fi
                
            # Get file extension information from table or namelist
            #Copied from spawn_ships while refactoring - do we need this here
            if [ -z "${EXT}" ]; then
                FSUFFIX=`awk -v DSRC=${DSOURCE} '($1 == DSRC) { print $2 }' ${TBL_DIR}FileSuffix.dat`
            else
                FSUFFIX="${EXT}"
            fi
            if [ -z "${FSUFFIX}" ]; then
                echo ""
                echo "MSG: Current cycle       --> ${CYCLE}"
                echo "MSG: Current storm       --> ${STORM}"
                echo "ERROR: File suffix not found for ${DSOURCE}."
                echo "ERROR: Please add your DSOURCE to ${TBL_DIR}FileSuffix.dat."
                echo "ERROR: Or define EXT in the namelist."
                exit
            fi
            if [ "${FSUFFIX}" == "NONE" ]; then
                FSUFFIX=""
            fi
    
            # Run some tests on the ATCF for thie storm.
            # If domain is storm-centerd and ATCF is required, then ATCF must
            # be present and contain forecast hours
            #Copied from spawn_ships while refactoring - do we need this here
            if [ "${SC}" == "True" ] && [ "${ATCF_REQD}" == "True" ]; then
                if [ -z "${STORM_ATCF[*]}" ]; then
                    echo ""
                    echo "MSG: Current cycle       --> ${CYCLE}"
                    echo "MSG: Current storm       --> ${STORM}"
                    echo "WARNING: DOMAIN=${DMN} is storm-centered and ATCF files are required."
                    echo "WARNING: But, found no matching ATCF files. Skipping to next."
                    continue
                fi
            fi


            ##########################
            # LOOP OVER ENSEMBLE IDS #
            ##########################
            # Could be 1 iteration if no ensemble
            NID=0
            for ID in ${ENSIDS[@]}; do
                echo ""

                # Set 2-digit variable ENSID
                if [ "${IS_ENS}" == "False" ]; then
                    ENSID="XX"
                    ENSIDTAG=""
                    MODEL="${MID}"
                else ### MATT CHANGE 7/26/2025 - don't index MID, one model
                    ### merged 04/02/2026
                    ENSID=$(printf "%02s\n" "$ID")
                    
                    # LJG no "E" 
                    ENSIDTAG=".${ENSID}"
                    MODEL="${MID}"
                fi

                # Read 'FORCE' from the namelist.
                # If FORCE is undefined, set it to False.
                # FORCE may be automatically changed to 'True' later on,
                # so it is critical to redefine it here.
                ##MD moved this from start of old ATCF loop
                FORCE=`sed -n -e 's/^FORCE =\s//p' ${NMLIST} | sed 's/^\t*//'`
                FORCE="${FORCE:-False}"

                 #### MATT CHANGE 7/27/2025 - search for ensemble member ATCF file:
                #### note - can put this above in ELSE part of ensemble check, was just working on this at other time
                #### Merged 04/02/2026
                if [ "${IS_ENS}" == "True" ]; then
                    for ATCF in "${ATCF_TMP[@]}"; do
                        if [[ "$ATCF" == *"/${CYCLE}/${ENSID}"* ]]; then
                            STORM_ATCF="${ATCF}"
                            CYCLE_ATCF="${ATCF}"
                            break
                        fi
                    done
                fi

                # Create full output path
                if [ "${ODIR_TYPE}" == "1" ]; then
                    ODIR_FULL="${ODIR}/$(echo ${ENSIDTAG} | cut -c2-)/guidance/"
                #note - probably don't need this elif because when blank ensmem is added,
                #the result is the same
                elif [ "${IS_ENS}" == "False" ]; then 
                    ODIR_FULL="${ODIR}/${EXPT}/${CYCLE}/guidance/"
                else
                    ODIR_FULL="${ODIR}/${EXPT}/${CYCLE}/$(echo ${ENSIDTAG} | cut -c2-)/guidance/"
                fi
                ODIR_FULL="$(echo "${ODIR_FULL}" | sed s#//*#/#g)"
                echo "MSG: Output directory --> ${ODIR_FULL}"
                echo "DEBUG: Based on ODIR_TYPE=${ODIR_TYPE}, IS_ENS=${IS_ENS}, ENSID=${ENSID}"
                mkdir -p ${ODIR_FULL}

                # Print some information to the terminal
                echo ""
                echo "MSG: **********DETAILS FOR THIS CASE**********"
                echo "     Current cycle       --> ${CYCLE}"
                echo "     Current storm       --> ${STORM}"
                echo "     Output directory    --> ${ODIR_FULL}"
                if [ ! -z "${ENSIDTAG}" ]; then
                    echo "     Current Ensemble ID --> ${ENSID}"
                fi

                if [ -z "${STORM_ATCF[*]}" ]; then
                    echo "WARNING: No ATCF found for ${STORM}. This might be OK."
                else
                    echo "MSG: ATCF found for ${STORM} --> ${STORM_ATCF[0]}"
                fi

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
                JOBNAME="GPLOT.${EXPT}.${CYCLE}.${ENSIDTAG}.stats.${STORM}.${MCODE}"
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

                    # Call the batch job - adding EID to command 4/29/2026 - adding condition 4/30/2026
                    echo "MSG: Executing GPLOT batch job submission. BATCH_MODE ${BATCH_MODE}"			
                    FULL_CMD="${BATCH_DIR}/${BATCHFILE} ${MACHINE} ${NCL_DIR}${NCLFILE} ${LOGFILE1} ${NMLIST}"
                    if [ "${IS_ENS}" == "True" ]; then
                        FULL_CMD="${FULL_CMD} ${CYCLE} ${STORM} ${FORCE} ${ID}"
                    else
                        FULL_CMD="${FULL_CMD} ${CYCLE} ${STORM} ${FORCE}"
                    fi
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
                    if [[ N -ge MAX_JOBS ]]; then
                        echo "WARNING: Maximum number of jobs reached (${MAX_JOBS})."
                        break
                    fi
                else
                    echo "MSG: Found matching GPLOT batch job. Skipping submission."
                fi

                #increment ensemble ID counter
                ((NID++))
            done #END Ensemble loop
        done #END Storm loop


    done #END Cycle loop
fi



wait

echo "$?"
echo "MSG: spawn_stats.sh completed at `date`"
