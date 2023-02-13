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

echo "MSG: spawn_airsea.sh started at `date`"
echo "MSG: Submitting jobs for GPLOT Module 'AIRSEA'."

# Determine the GPLOT source code directory
if [ -z "${GPLOT_DIR}" ]; then
    export GPLOT_DIR="$( echo "$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" | rev | cut -d'/' -f4- | rev )"
fi

# Define important GPLOT directories
NMLIST_DIR="${GPLOT_DIR}/parm/"
BATCH_DIR="${GPLOT_DIR}/sorc/GPLOT/batch/"
PYTHON_DIR="${GPLOT_DIR}/sorc/GPLOT/python/"
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
DO_AIRSEA="`sed -n -e 's/^DO_AIRSEA =\s//p' ${NMLIST} | sed 's/^\t*//'`"
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
ATCF1_DIR="`sed -n -e 's/^ATCF1_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ATCF1_TAG="`sed -n -e 's/^ATCF1_TAG =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ATCF2_DIR="`sed -n -e 's/^ATCF2_DIR =\s//p' ${NMLIST} | sed 's/^\t*//'`"
ATCF2_TAG="`sed -n -e 's/^ATCF2_TAG =\s//p' ${NMLIST} | sed 's/^\t*//'`"
FORCE="`sed -n -e 's/^FORCE =\s//p' ${NMLIST} | sed 's/^\t*//'`"
MACHINE="`sed -n -e 's/^MACHINE =\s//p' ${NMLIST} | sed 's/^\t*//'`"
CPU_ACCT="`sed -n -e 's/^CPU_ACCT =\s//p' ${NMLIST} | sed 's/^\t*//'`"
QOS="`sed -n -e 's/^QOS =\s//p' ${NMLIST} | sed 's/^\t*//'`"
PARTITION="`sed -n -e 's/^PARTITION =\s//p' ${NMLIST} | sed 's/^\t*//'`"
RESOLUTION="`sed -n -e 's/^RESOLUTION =\s//p' ${NMLIST} | sed 's/^\t*//'`"
RMAX="`sed -n -e 's/^RMAX =\s//p' ${NMLIST} | sed 's/^\t*//'`"
LEVS="`sed -n -e 's/^LEVS =\s//p' ${NMLIST} | sed 's/^\t*//'`"
AIRSEA_PYTHONFILE="`sed -n -e 's/^AIRSEA_PYTHONFILE =\s//p' ${NMLIST} | sed 's/^\t*//'`"

# Define batch defaults
BATCH_DFLTS="${NMLIST_DIR}batch.defaults.${MACHINE,,}"

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
    MACHINE=`sed -n -e 's/^SYS_ENV =\s//p' ${NMLIST} | sed 's/^\t*//'`
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

# Get the batch submission mode [SBATCH,BACKGROUND,FOREGROUND]
BATCH_MODE="`sed -n -e 's/^BATCH_MODE =\s//p' ${NMLIST} | sed 's/^\t*//' | tr a-z A-Z`"
BATCH_MODE="${BATCH_MODE:-SBATCH}"
echo "MSG: Using this batch-submission mode   --> ${BATCH_MODE}"

# Get a list of forecast lead times
FHRS=( $(seq ${INIT_HR} ${DT} ${FNL_HR} | tr "\n" " ") )
echo "MSG: Will produce graphics for these forecast lead times --> ${FHRS[*]}"

# Find the forecast cycles for which graphics should be created
# if [ -z "${IDATE}" ]; then
#     echo ${IDIR}
#     CYCLES=( `find ${IDIR}/ -maxdepth 4 -type d -regextype sed -regex ".*/[0-9]\{10\}$" -exec basename {} \; | sort -u -r 2>/dev/null` )
#     if [ -z "${CYCLES}" ]; then
#         CYCLES=( `find ${IDIR}/ -maxdepth 4 -type d -regextype sed -regex ".*/${DSOURCE,,}.[0-9]\{10\}$" -exec basename {} \; | sort -u -r 2>/dev/null` )
#     fi
#     if [ -z "${CYCLES}" ]; then
#         CYCLES=( `find ${IDIR}/ -maxdepth 4 -type d -regex ".*/\(00\|06\|12\|18\)" | grep -E "[0-9]{8}" | sort -u -r | rev | cut -d'/' -f-2 | sed 's@/@@g' | cut -d'.' -f1 | rev | tr "\n" " " 2>/dev/null` )
#     fi
#     if [ -z "${CYCLES}" ]; then
#         CYCLES=( `ls -rd ${IDIR}/[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]/{00,06,12,18} 2>/dev/null | rev | cut -d'/' -f-2 2>/dev/null | sed 's@/@@g' | cut -d'.' -f1 | rev | tr "\n" " " 2>/dev/null` )
#     fi
# else
#     CYCLES=( "${IDATE[@]}" )
# fi
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
    ATCF_TMP+=( `find ${ATCF2_DIR} -type f -name *${CYCLE10}*${ATCF2_TAG}` )
    ATCF_TMP+=( `find ${ATCF1_DIR} -type f -name *${CYCLE10}*${ATCF1_TAG}` )
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
if [ "${EID[*]}" == "0" ] || [ "${EID[*]}" == "00" ] || [ -z "${EID[*]}" ]; then
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
MAXCOUNT=25

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





#############################################################
# 2. CALL GPLOT AIRSEA                                      #
#    This script is responsible for creating graphics based #
#    on SHIPS fields and other relevant predictors.         #
#############################################################
if [ "${DO_AIRSEA}" = "True" ]; then
    AIRSEA_PYTHONFILE="${AIRSEA_PYTHONFILE:-plot_airsea_pbl.py}"
    BATCHFILE="batch_airsea.sh"
    DOMAIN="airsea"
    TIER="Tier1"
    SC="True"
    ATCF_REQD="True"


    # Define the batch submission counter.
    N=0

    ###########################
    # LOOP OVER GRAPHIC TIERS #
    ###########################
    for TR in ${TIER[@]}; do

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
                for ATCF in ${CYCLE_ATCF[@]}; do
                    STORMS+=(`basename ${ATCF} | cut -d'.' -f1 | rev | cut -c1-3 | rev | tr '[:lower:]' '[:upper:]'`)
                done
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
            # were found, i.e., STORMS != NONE
            if [ "${IS_MSTORM}" == "True" ] && [ "${STORMS[*]}" != "NONE" ]; then
                STORMS+=("00L")
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
    
                # Never process the fake storm (00L)
                if [ "${STORM^^}" == "00L" ]; then
                    echo "MSG: Fake storm detected ${STORM}. Skipping."
                    continue
                fi
    
                # Find the forecast hours from the ATCF for this particular storm
                STORM_ATCF=( `printf '%s\n' ${CYCLE_ATCF[@]} | grep -i "${STORM,,}.${CYCLE}" | head -1` )
                if [ ! -z "${STORM_ATCF[*]}" ]; then
                    echo "MSG: ATCF found for ${STORM} --> ${STORM_ATCF[0]}"
                fi
    
                #Keep only the ATCF forecast hours that match namelist options: INIT_HR,FNL_HR,DT
                #NEW_ATCF_FHRS=()
                #for FHR in ${ATCF_FHRS[@]}; do
                #    if [ $((10#$FHR)) -ge $INIT_HR ] && [ $((10#$FHR)) -le $FNL_HR ] && [ $((10#$FHR % $DT)) -eq 0 ]; then
                #        NEW_ATCF_FHRS+=( $FHR )
                #    fi
                #done
                #ATCF_FHRS=("${NEW_ATCF_FHRS[@]}")
    
    
                # Set the STORMTAG for file names
                STORMTAG=".${STORM^^}"
    
    
                #########################
                # LOOP OVER MAP DOMAINS #
                #########################
                for DMN in ${DOMAIN[@]}; do
    
                    # Get nest information from GPLOT table
                    NEST=`awk -v DMN=${DMN} '($1 == DMN) { print $2 }' ${TBL_DIR}DomainInfo.dat`
                    if [ -z "${NEST}" ]; then
                        echo "WARNING: This domain (${DMN}) not found in ${TBL_DIR}DomainInfo.dat."
                        echo "WARNING: Assuming NEST=1."
                        NEST=1
                    fi
    
                    # Get file prefix information from table or namelist
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
                    echo "FHRFMT=${FHRFMT}, FHRFMT[0]=${FHRFMT:0:1}"
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

                        # Set 2-digit variable ENSID
                        if [ "${IS_ENS}" == "False" ]; then
                            ENSID="XX"
                            ENSIDTAG=""
                            MODEL="${MID}"
                        else
                            ENSID=$(printf "%02d\n" ${ID})
                            ENSIDTAG=".E${ENSID}"
                            MODEL="${MID[NID]}"
                        fi
                        ((NID++))

                        # Create full output path
                        if [ "${ODIR_TYPE}" == "1" ]; then
                            ODIR_FULL="${ODIR}/${DMN}/"
                        else
                            ODIR_FULL="${ODIR}/${EXPT}/$(echo ${ENSIDTAG} | cut -c2-)/${CYCLE}/${DMN}/"
                        fi
                        ODIR_FULL="$(echo "${ODIR_FULL}" | sed s#//*#/#g)"
                        mkdir -p ${ODIR_FULL}

                        # Print some information to the terminal
                        echo ""
                        echo "MSG: **********DETAILS FOR THIS CASE**********"
                        echo "     Current cycle       --> ${CYCLE}"
                        echo "     Current storm       --> ${STORM}"
                        echo "     Current domain      --> ${DMN}"
                        echo "     Current tier        --> ${TR}"
                        echo "     Output directory    --> ${ODIR_FULL}"
                        if [ ! -z "${ENSIDTAG}" ]; then
                            echo "     Current Ensemble ID --> ${ENSID}"
                        fi
                        if [ -z "${STORM_ATCF[*]}" ]; then
                            echo "WARNING: No ATCF found for ${STORM}. This might be OK."
                        else
                            echo "MSG: ATCF found for ${STORM} --> ${STORM_ATCF[0]}"
                        fi

                        # Find the forecast hours from the ATCF for this particular model
                        # MODEL_ATCF1: list of ATCF files filtered by Storm & Cycle
                        # MODEL_ATCF2: list of ATCF files filtered by Cycle
                        MODEL_ATCF1=( "" )
                        if [ ! -z "${STORM_ATCF[*]}" ]; then
                            MODEL_ATCF1=( `grep -l "${MODEL^^}" ${STORM_ATCF[*]}` )
                        fi
                        MODEL_ATCF2=( "" )
                        if [ ! -z "${CYCLE_ATCF[*]}" ]; then
                            MODEL_ATCF2=( `grep -l "${MODEL^^}" ${CYCLE_ATCF[*]}` )
                        fi
                        ATCF_FHRS=()
                        if [ -z "${MODEL_ATCF1[*]}" ] && [ -z "${MODEL_ATCF2[*]}" ]; then
                            echo "WARNING: No matching ATCFs found for MODEL=${MODEL}. This might be OK."
                        elif [ "${SC}" == "True" ] && [ -f ${MODEL_ATCF1[0]} ]; then
                            echo "MSG: ATCF(s) found for ${MODEL} --> ${MODEL_ATCF1[*]}"
                            ATCF_FHRS=( `cat ${MODEL_ATCF1[*]} | tr -d "[:blank:]" | awk -v MODEL="${MODEL^^}" -F, '$5==MODEL { printf("%03d\n", $6) }' | sort -u | sort -k1,1n | sed -e 's/^[[:space:]]*//'` )
                        elif [ "${SC}" == "False" ] && [ -f ${MODEL_ATCF2[0]} ]; then
                            echo "MSG: ATCF(s) found for ${MODEL} --> ${MODEL_ATCF2[*]}"
                            ATCF_FHRS=( `cat ${MODEL_ATCF2[*]} | tr -d "[:blank:]" | awk -v MODEL="${MODEL^^}" -F, '$5==MODEL { printf("%03d\n", $6) }' | sort -u | sort -k1,1n | sed -e 's/^[[:space:]]*//'` )
                        else
                            if [ "${SC}" == "True" ]; then
                                echo "WARNING: ATCF information for MODEL=${MODEL} could not be extracted from ${MODEL_ATCF1[*]}"
                            else
                                echo "WARNING: ATCF information for MODEL=${MODEL} could not be extracted from ${MODEL_ATCF2[*]}"
                            fi
                        fi
                        # Keep only the ATCF forecast hours that match namelist options: INIT_HR,FNL_HR,DT
                        NEW_ATCF_FHRS=()
                        for FHR in ${ATCF_FHRS[@]}; do
                            if [[ $((10#$FHR)) -ge $INIT_HR ]] && [[ $((10#$FHR)) -le $FNL_HR ]] && [[ $((10#$FHR % $DT)) -eq 0 ]]; then
                                NEW_ATCF_FHRS+=( $FHR )
                            fi
                        done
                        ATCF_FHRS=("${NEW_ATCF_FHRS[@]}")

                        # Run some tests on the ATCF for thie storm.
                        # If domain is storm-centerd and ATCF is required, then ATCF must
                        # be present and contain forecast hours
                        if [ "${SC}" == "True" ] && [ "${ATCF_REQD}" == "True" ]; then
                            if [ -z "${MODEL_ATCF1[*]}" ]; then
                                echo "WARNING: DOMAIN=${DMN} is storm-centered and ATCF files are required."
                                echo "WARNING: But, found no matching ATCF files. So, nothing to do."
                                continue
                            elif [ -z "${ATCF_FHRS[*]}" ]; then
                                echo "WARNING: DOMAIN=${DMN} is storm-centered and ATCF files are required."
                                echo "WARNING: ATCF was found for this storm, but no forecast hours were found."
                                continue
                            fi
                        fi

                        # If ATCF_FILES.dat already exists, check that no additional ATCFs
                        # were found. Also, check if the any of the ATCFs were modified
                        # in the last hour. If so, then force re-production of all graphics.
                        # This is important for non-storm-centered domains. ATCFs will be
                        # used to place markers of active TC locations in larger domains.
                        if [ -f "${ODIR_FULL}ATCF_FILES.dat" ]; then
                            PREV_ATCF=()
                            PREV_ATCF=( `cat ${ODIR_FULL}ATCF_FILES.dat` )
                            if [ "$SC" == "False" ]; then
                                if [ "${PREV_ATCF[*]}" == "NONE" ] && [ "${#CYCLE_ATCF[@]}" -gt "0" ]; then
                                    echo "MSG: Found ${#CYCLE_ATCF[@]} ATCFs this time. No ATCF found last time."
                                    echo "MSG: Forcing production."
                                    FORCE="True"
                                elif [ "${#PREV_ATCF[@]}" -lt "${#CYCLE_ATCF[@]}" ]; then
                                    echo "MSG: Found more ATCFs this time (${#CYCLE_ATCF[@]}) than last time (${#PREV_ATCF[@]})."
                                    echo "MSG: Forcing production."
                                    FORCE="True"
                                elif [ "${PREV_ATCF[*]}" != "NONE" ]; then
                                    for ATCF in ${PREV_ATCF[@]}; do
                                        test=$(find ${ATCF} -mmin -20 2>/dev/null)
                                        if [[ -n $test ]]; then
                                            echo "MSG: This ATCF is not old enough --> ${ATCF}"
                                            echo "MSG: Forcing production."
                                            FORCE="True"
                                            break
                                        fi
                                    done
                                fi
                            elif [ ! -z "${STORM_ATCF[*]}" ]; then
                                test=$(find ${STORM_ATCF[0]} -mmin -20 2>/dev/null)
                                if [[ -n $test ]]; then
                                    echo "MSG: This ATCF is not old enough --> ${ATCF}"
                                    echo "MSG: Forcing production."
                                    FORCE="True"
                                fi
                            fi
                        fi

                        # Create a list of IDIR subdirectory options
                        IDIR_OPTS=("" "${EXPT}/com/${CYCLE_STR}/${STORM}/" "${EXPT}/com/${CYCLE_STR}/" "${EXPT}/com/" \
                                   "${EXPT}/" "${CYCLE_STR}/${STORM}/" "${CYCLE_STR}/" "${STORM}/" "${EXPT}/${CYCLE_STR}/${STORM}/" \
                                   "${EXPT}/${CYCLE_STR}/" "${EXPT}/com/${CYCLE_STR}/${STORM}/${ENSID}/" "${EXPT}/com/${CYCLE_STR}/${ENSID}/" \
                                   "${EXPT}/com/${ENSID}/" "${EXPT}/${ENSID}/" "${CYCLE_STR}/${STORM}/${ENSID}/" \
                                   "${ENSID}/${CYCLE_STR}/${STORM}/" "${CYCLE_STR}/${ENSID}/${STORM}/" "${CYCLE_STR}/${ENSID}/" \
                                   "${ENSID}/${CYCLE_STR}/" "${STORM_STR}/${ENSID}/" "${ENSID}/${STORM}/" "${EXPT}/com/${ENSID}/${CYCLE_STR}/" \
                                   "${EXPT}/${ENSID}/com/${CYCLE_STR}/${STORM}/" "${EXPT}/${ENSID}/com/${CYCLE_STR}/" "${EXPT}/${ENSID}/com/" \
                                   "${ENSID}/com/${CYCLE_STR}/${STORM}/" "com/${CYCLE_STR}/${STORM}/" "${ENSID}/" "${CYCLE_STR}/00L/" \
                                   "com/${CYCLE_STR}/00L/" "${EXPT}/com/${CYCLE_STR}/00L/" "${EXPT}${ENSID}/com/${CYCLE_STR}/00L/" \
                                   "${DSOURCE,,}.${YYYY}${MM}${DD}/${HH}/" "${YYYY}${MM}${DD}/${HH}/" "${EXPT}_${ENSID}/com/${CYCLE_STR}/${STORM}/" \
                                   "${EXPT}_${ENSID}/com/${CYCLE_STR}/00L/" "${DSOURCE,,}.${YYYY}${MM}${DD}/${HH}/atmos/")

                        # Get the right list of lead times
                        if [ "${SC}" == "True" ] && [ "${ATCF_REQD}" == "True" ]; then
                            FILE_FHRS=( ${ATCF_FHRS[@]} )
                        else
                            FILE_FHRS=( ${FHRS[@]} )
                        fi
                        echo "MSG: I will only look for these forecast lead times --> ${FILE_FHRS[*]}"

                        # Find all input files that match: FPREFIX,FHRSTR,FHRFMT,FSUFFIX
                        # If a match is found, write lead time to a data file "AllForecastHours"
                        IFILES=()
                        F=0
                        IFHRS=()
                        while [ -z "${IFILES[*]}" ]; do
                            IDIR_FULL="$(echo "${IDIR}/${IDIR_OPTS[$F]}" | sed s#//*#/#g)"
                            # If the input directory doesn't exist, continue to the next option
                            if [ ! -d ${IDIR_FULL} ]; then
                                ((F=F+1))
                                continue
                            fi

                            # Loop over all lead times to find available files.
                            for FHR in ${FILE_FHRS[@]}; do

                                # Build the file search string.
                                echo "DEBUG:: FHRFMT=${FHRFMT}"
                                FILE_SEARCH="${IDIR_FULL}*${FPREFIX}*${FHRSTR}$(printf "${FHRFMT}\n" $((10#$FHR)))"
                                FILE_SEARCH2="${IDIR_FULL}*${STORM,,}*${FPREFIX}*${FHRSTR}$(printf "${FHRFMT}\n" $((10#$FHR)))"
                                FILE_SEARCH3="${IDIR_FULL}*${STORM,,}*${CYCLE}*${FPREFIX}*${FHRSTR}$(printf "${FHRFMT}\n" $((10#$FHR)))"
                                if [ ! -z "${FSUFFIX}" ]; then
                                    FILE_SEARCH="${FILE_SEARCH}*${FSUFFIX}"
                                    FILE_SEARCH2="${FILE_SEARCH2}*${FSUFFIX}"
                                    FILE_SEARCH3="${FILE_SEARCH3}*${FSUFFIX}"
                                fi

                                # Search for a matching file. If found, append the file and forecast hour to their respective arrays
                                FILE_LS=( `ls ${FILE_SEARCH3} 2>/dev/null` )
                                if [ "${#FILE_LS[@]}" -eq "1" ]; then
                                    IFILES+=("${FILE_LS[*]}")
                                    IFHRS+=( ${FHR} )
                                else
                                    FILE_LS=( `ls ${FILE_SEARCH2} 2>/dev/null` )
                                    if [ "${#FILE_LS[@]}" -eq "1" ]; then
                                        IFILES+=("${FILE_LS[*]}")
                                        IFHRS+=( ${FHR} )
                                    else
                                        if [[ "HWRF HMON HAFS" != *"${DSOURCE}"* ]]; then
                                            FILE_LS=( `ls ${FILE_SEARCH} 2>/dev/null` )
                                            if [ "${#FILE_LS[@]}" -eq "1" ]; then
                                                IFILES+=("${FILE_LS[*]}")
                                                IFHRS+=( ${FHR} )
                                            fi
                                        fi
                                    fi
                                fi
                            done

                            # Increase the counter to search the next input directory option
                            ((F=F+1))

                            # Break the loop if all input directory options have been searched
                            if [ ${F} -gt ${#IDIR_OPTS[@]} ]; then
                                echo "ERROR: No files were found. Try fixing IDIR in the namelist."
                                break
                                #exit
                            fi
                        done
                        if [ -z "${IFILES[*]}" ]; then
                            echo "WARNING: Nothing to do here. Moving on to the next case."
                            echo ""
                            continue
                        fi
                        echo "MSG: Full input directory --> ${IDIR_FULL}"

                        # Mark that files have been found
                        FOUND_FILES="True"

                        # Define the file that contains a list of plotted files (PLOTTED_FILE)
                        # Define the file that contains the status (STATUS_FILE)
                        PLOTTED_FILE="${ODIR_FULL}PlottedFiles.${DMN}.${TR}${STORMTAG}.log"
                        STATUS_FILE="${ODIR_FULL}status.${DMN}.${TR}${STORMTAG}.log"
                        LOCK_FILE="${STATUS_FILE}.lock"

                        # Get the list of plotted files for this case
                        CASE_PLOTTED=(`cat ${PLOTTED_FILE} | sed 's#//*#/#g' 2>/dev/null`)                        

                        # Get the status for this case
                        lockfile -r-1 -l 180 "${LOCK_FILE}"
                        CASE_STATUS=`cat ${STATUS_FILE} 2>/dev/null`
                        rm -f "${LOCK_FILE}"

                        # Print some information
                        # This depends on whether or not forcing is turned on.
                        # Don't do this for the HAFS workflow (i.e., ODIR_TYPE=0)
                        if [ "${ODIR_TYPE}" == "0" ]; then
                            if [ "${FORCE}" == "False" ]; then
                                echo "MSG: Using this processed file log --> ${PLOTTED_FILE}"
                                echo "MSG: Found ${#CASE_PLOTTED[@]} processed files. These will be skipped."
                                echo "MSG: To manually force reprocessing of all files, delete this file."
                            else
                                #  Don't do this if the status is working.
                                if [ "${CASE_STATUS}" != "working" ]; then
                                    echo "MSG: Graphic production will be forced."
                                    echo "MSG: Deleting the processed file log --> ${PLOTTED_FILE}"
                                    rm -f ${PLOTTED_FILE}
                                    CASE_PLOTTED=()
                                    CASE_STATUS="force"
                                fi
                            fi
                        fi
                        echo "MSG: Using this status file --> ${STATUS_FILE}"
                        echo "MSG: Found this status --> ${CASE_STATUS}"


                        # Loop through IFILES and retain only valid entries
                        i=0
                        BROKEN_LINK="NO"
                        for FILE in ${IFILES[@]}; do

                            # Remove files that are broken links
                            if [ ! -e "${FILE}" ]; then
                                unset 'IFILES[$i]'
                                unset 'IFHRS[$i]'
                                BROKEN_LINK="YES"
                            fi

                            # Remove files that have already been processed (CASE_PLOTTED)
                            # from the list in input files (IFILES).
                            # If production is being forced, then CASE_PLOTTED will be empty.
                            # Only files that have not been modified in over 30 min are
                            # removed from the list.
                            if [ ! -z "${CASE_PLOTTED[*]}" ]; then
                                #TMP=$(printf -- '%s\n' "${CASE_PLOTTED[@]}" | grep "$FILE")
                                TMP=$(grep "${FILE}" ${PLOTTED_FILE})
                                CFILE=`echo "${TMP}" | cut -d' ' -f1`
                                NATCF=`echo "${TMP}" | cut -d' ' -f2`
                                if [ ${SC} == "True" ]; then
                                    ATCF_EXP=1
                                else
                                    ATCF_EXP=${#CYCLE_ATCF[@]}
                                fi
                                if [[ -n "${CFILE}" ]]; then
                                    test=$(find ${IDIR_FULL} -name "`basename ${CFILE}`" -mmin +15 2>/dev/null)
                                    if [[ -n ${test} ]]; then
                                        unset 'IFILES[$i]'
                                        unset 'IFHRS[$i]'
                                    fi
                                fi
                            fi
                            ((i++))
                        done
                        if [ -z "${IFILES[*]}" ]; then
                            if [ "${BROKEN_LINK}" == "YES" ] && [ "${CASE_STATUS}" != "complete" ]; then
                                echo "WARNING: No files to process, but broken links were detected."
                                echo "WARNING: Marking status as broken. It should be double-checked."
                                lockfile -r-1 -l 180 "${LOCK_FILE}"
                                echo "broken" > ${STATUS_FILE}
                                rm -f "${LOCK_FILE}"
                            else
                                echo "MSG: All available input files have been processed."
                                echo "MSG: Marking status as complete."
                                lockfile -r-1 -l 180 "${LOCK_FILE}"
                                echo "complete" > ${STATUS_FILE}
                                rm -f "${LOCK_FILE}"
                            fi
                            echo ""
                            continue
                        fi


                        # Remove any blank entries from IFILES & IFHRS
                        for i in "${!IFILES[@]}"; do
                            NEW_IFILES+=( "${IFILES[i]}" )
                            NEW_IFHRS+=( "${IFHRS[i]}" )
                        done
                        IFILES=("${NEW_IFILES[@]}")
                        IFHRS=("${NEW_IFHRS[@]}")
                        unset NEW_IFILES
                        unset NEW_IFHRS


                        # Check the status and update it if necessary.
                        # This logic will allow work to start on this case
                        # or will move on to the next case.
                        if [ "${CASE_STATUS}" == "force" ]; then
                            echo "MSG: Forcing production. Ignoring status file."
                            lockfile -r-1 -l 180 "${LOCK_FILE}"
                            echo "start" > ${STATUS_FILE}
                            rm -f "${LOCK_FILE}"
                        elif [ "${CASE_STATUS}" == "complete" ]; then
                            if [ -z "${IFILES[*]}" ]; then
                                echo "MSG: Status suggests this case has been completed."
                                echo "MSG: Nothing to do here. Moving on to next case."
                                echo ""
                                continue
                            else
                                echo "MSG: Status says complete, but unprocessed files were found."
                                echo "MSG: Deleting the status for a restart."
                                lockfile -r-1 -l 180 "${LOCK_FILE}"
                                echo "start" > ${STATUS_FILE}
                                rm -f "${LOCK_FILE}"
                            fi
                        elif [ "${CASE_STATUS}" == "working" ]; then
                            echo "MSG: Status suggests this case is being worked on."
                            echo "MSG: Changing the status to 'update request 1'."
                            echo ""
                            lockfile -r-1 -l 180 "${LOCK_FILE}"
                            echo "update request 1" > ${STATUS_FILE}
                            rm -f "${LOCK_FILE}"
                            continue
                        elif [ "${CASE_STATUS}" == "update request 1" ]; then
                            echo "MSG: Status suggests this case needs to be updated."
                            echo "MSG: Changing the status to 'update request 2'."
                            echo ""
                            lockfile -r-1 -l 180 "${LOCK_FILE}"
                            echo "update request 2" > ${STATUS_FILE}
                            rm -f "${LOCK_FILE}"
                            continue
                        elif [ "${CASE_STATUS}" == "update request 2" ] || [ "${CASE_STATUS}" == "failed" ]; then
                            echo "MSG: Status suggests this case has stalled/failed."
                            echo "MSG: Deleting the status for a restart."
                            lockfile -r-1 -l 180 "${LOCK_FILE}"
                            echo "start" > ${STATUS_FILE}
                            rm -f "${LOCK_FILE}"
                        elif [ "${CASE_STATUS}" == "incomplete" ]; then
                            echo "MSG: Status suggests that this case is incomplete."
                            echo "MSG: Will try to find new input files."
                            lockfile -r-1 -l 180 "${LOCK_FILE}"
                            echo "start" > ${STATUS_FILE}
                            rm -f "${LOCK_FILE}"
                        elif [ "${CASE_STATUS}" == "broken" ]; then
                            echo "MSG: Status suggests that this case is broken."
                            echo "MSG: It will require manual resubmission."
                            echo ""
                            continue
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


                        # If IFILES is empty, then all available files have been plotted
                        # Then, skip to the next and mark status=complete
                        if [ -z "${IFILES[*]}" ]; then
                            if [ "${ATCF_REQD}" == "True" ] && ! find "${STORM_ATCF[0]}" -mmin +60 >/dev/null ; then
                                echo "MSG: All available files have already been processed."
                                echo "MSG: However, ATCF is not old enough to complete."
                                echo "MSG: More files might become available."
                                echo "MSG: Moving on to next case."
                                lockfile -r-1 -l 180 "${LOCK_FILE}"
                                echo "incomplete" > ${STATUS_FILE}
                                rm -f "${LOCK_FILE}"
                                echo ""
                            else
                                echo "MSG: All available files have already been processed."
                                echo "MSG: Marking the status as complete."
                                echo "MSG: Moving on to next case."
                                echo ""
                                lockfile -r-1 -l 180 "${LOCK_FILE}"
                                echo "complete" > ${STATUS_FILE}
                                rm -f "${LOCK_FILE}"
                            fi
                            continue
                        fi


                        # Write existing IFILES out to UNPLOTTED_FNAME log for use in plotting scripts
                        UNPLOTTED_FILE="${ODIR_FULL}/UnplottedFiles.${DMN}.${TR}${STORMTAG}.log"
                        FHR_FILE="${ODIR_FULL}/AllForecastHours.${DMN}.${TR}${STORMTAG}.log"
                        if [ -f "${UNPLOTTED_FILE}" ]; then
                            rm -f "${UNPLOTTED_FILE}"
                        fi
                        if [ -f "${FHR_FILE}" ]; then
                            rm -f "${FHR_FILE}"
                        fi
                        i=0
                        for LINE in ${IFILES[@]}; do
                            echo "${LINE}" >> ${UNPLOTTED_FILE}
                            echo "${IFHRS[$i]}" >> ${FHR_FILE}
                            ((i++))
                        done
                        echo "MSG: Using this file of unplotted files --> ${UNPLOTTED_FILE}"
                        echo "MSG: Using this file of unplotted lead times --> ${FHR_FILE}"


                        # Choose a proper wallclock time for this job based on the number of files.
                        if [ "${#IFILES[@]}" -le "15" ]; then
                            RUNTIME="00:29:59"
                        elif [ "${#IFILES[@]}" -le "30" ]; then
                            RUNTIME="00:59:59"
                        elif [ "${#IFILES[@]}" -le "45" ]; then
                            RUNTIME="01:29:59"
                        elif [ "${#IFILES[@]}" -le "60" ]; then
                            RUNTIME="01:59:59"
                        elif [ "${#IFILES[@]}" -le "75" ]; then
                            RUNTIME="02:29:59"
                        elif [ "${#IFILES[@]}" -le "90" ]; then
                            RUNTIME="02:59:59"
                        elif [ "${#IFILES[@]}" -le "105" ]; then
                            RUNTIME="03:29:59"
                        elif [ "${#IFILES[@]}" -le "120" ]; then
                            RUNTIME="03:59:59"
                        elif [ "${#IFILES[@]}" -le "135" ]; then
                            RUNTIME="04:29:59"
                        else
                            RUNTIME="04:59:59"
                        fi

                        # Check if a similar job is already submitted
                        echo "MSG: The batch file --> ${BATCH_DIR}${BATCHFILE}"
                        if [ "${BATCH_MODE^^}" == "FOREGROUND" ]; then
                            JOB_TEST=""
                        elif [ "${BATCH_MODE^^}" == "BACKGROUND" ]; then
                            JOB_TEST=""
                        #elif [ "${BATCH_MODE^^}" == "SBATCH" ]; then
                        else
                            JOB_NAME="GPLOT.${EXPT}.${CYCLE}${ENSIDTAG}.${DMN}${STORMTAG}.${TR}"
                            JOB_TEST=`${X_SQUEUE} -u $USER -o %.100j | /bin/grep "${JOB_NAME}"`
                        fi

                        # Change options in the batch submission script.
                        if [ -z "${JOB_TEST}" ]; then

                            # Create a text file with ATCFs for this cycle in the output directory.
                            # This file should only be updated when a new job is being submitted.
                            # If this file changes and a job is not submitted, then it could cause
                            # issues with storm labels for non-storm-centered graphics.
                            if [ -z "${MODEL_ATCF2[*]}" ]; then
                                echo "NONE" > ${ODIR_FULL}ATCF_FILES.dat
                            else
                                if [ -f "${ODIR_FULL}ATCF_FILES.dat" ]; then
                                    rm -f ${ODIR_FULL}ATCF_FILES.dat
                                fi
                                for ATCF in ${MODEL_ATCF2[@]}; do
                                    echo "${ATCF}" >> ${ODIR_FULL}ATCF_FILES.dat
                                done
                            fi

                            # Change options in the batch submission script.
                            LOG_DIR="${ODIR_FULL}"
                            LOGFILE1="${LOG_DIR}GPLOT_Airsea.${EXPT}.${CYCLE}${ENSIDTAG}.${DMN}${STORMTAG}.${TR}.log"
                            LOGFILE2="${LOG_DIR}GPLOT_Airsea.${EXPT}.${CYCLE}${ENSIDTAG}.${DMN}${STORMTAG}.${TR}.out"

                            # Submit the child batch job.
                            echo "MSG: Submitting GPLOT child batch job. BATCH_MODE = ${BATCH_MODE}"			
                            FULL_CMD="${BATCH_DIR}/${BATCHFILE} ${MACHINE} ${PYTHON_DIR}${AIRSEA_PYTHONFILE} ${LOGFILE1} ${NMLIST} ${ENSID}"
                            FULL_CMD="${FULL_CMD} ${CYCLE} ${STORM} ${DMN} ${TR} ${RESOLUTION} ${RMAX} ${LEVS} ${FORCE}"
                            if [ "${BATCH_MODE^^}" == "FOREGROUND" ]; then
                                echo "MSG: Executing this command [${FULL_CMD}]."
                                ${FULL_CMD}
                            elif [ "${BATCH_MODE^^}" == "BACKGROUND" ]; then
                                echo "MSG: Executing this command [${FULL_CMD} &]."
                                ${FULL_CMD} &
                            else
                                SLRM_OPTS="--account=${CPU_ACCT} --job-name=${JOB_NAME} --output=${LOGFILE2} --error=${LOGFILE2}"
                                SLRM_OPTS="${SLRM_OPTS} --nodes=1 --ntasks-per-node=12 --mem=32G --time=${RUNTIME} --qos=${QOS} --partition=${PARTITION}"
                                echo "MSG: Executing this command [${X_SBATCH} ${SLRM_OPTS} ${FULL_CMD}]."
                                ${X_SBATCH} ${SLRM_OPTS} ${FULL_CMD}
                            fi

                            # Increase the batch job counter and check if we're over the limit.
                            ((N++))
                            if [ "$N" -ge "$MAXCOUNT" ]; then
                                echo "MSG: Maximum number of batch submissions has been reached."
                                echo "MSG: Further jobs will be submitted later."
                                echo "MSG: spawn_ships.sh completed at `date`"
                                exit
                            fi
                        else
                            echo "MSG: Found matching GPLOT batch job. Skipping submission."
                        fi #if [ -z "$JOB_TEST" ]; then else

                        # Sleep to allow the current job to get started
                        sleep 10

                    done #end of ID loop
                done #end of TR loop
            done #end of DMN loop
        done #end of STORM loop
    done #end of CYCLE loop
fi #end of DO_AIRSEA

wait

echo "$?"
echo "MSG: spawn_airsea.sh completed at `date`"
