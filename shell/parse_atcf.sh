#!/bin/sh --login
#

#set -aeu

MODEL=$1
ODIR=$2
#ODIR="/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/GFS_Forecast/"
ADECKDIR="/lfs1/projects/hur-aoml/Ghassan.Alaka/adeck/NHC/"
BDECKDIR="/lfs1/projects/hur-aoml/Ghassan.Alaka/bdeck/ftp.nhc.noaa.gov/atcf/btk/"
MMIN="-43200"

mkdir -p ${ODIR}
cd ${ODIR}

ALL_ADECKS=( `find "${ADECKDIR}" -name 'a*.dat' -type f -mmin ${MMIN} | xargs -n 1 basename` )

for ADECK in ${ALL_ADECKS[@]}; do

    # Get some information from the A-Deck file name
    echo "MSG: Found this ADECK --> ${ADECK}"
    SID=`echo "$ADECK" | cut -d "_" -f1 | cut -c 4-5`
    BASIN=`echo "$ADECK" | cut -d "_" -f1 | cut -c 2-3`
    YEAR=`echo "$ADECK" | cut -d "_" -f1 | cut -c 6-9`


    # Parse only these basins
    if [ ${BASIN} = "al" ]; then
        BASIN2="l"
        echo "MSG: ATCF basin --> North Atlantic"
    elif [ ${BASIN} = "ep" ]; then
        BASIN2="e"
        echo "MSG: ATCF basin --> eastern North Pacific"
    elif [ ${BASIN} = "cp" ]; then
        BASIN2="c"
        echo "MSG: ATCF basin --> central North Pacific"
    else
        echo "WARNING: ATCF basin not recognized. Skipping."
        continue
    fi

    # Get a list of available cycles
    ALL_CYCLES=( `grep "${MODEL}" ${ADECKDIR}${ADECK} | cut -d "," -f3 | uniq | sed -e 's/^[[:space:]]*//'` )


    # Find corresponding BDECK file
    BDECK=${BDECKDIR}b${BASIN}${SID}${YEAR}.dat
    if [ -f ${BDECK} ]; then
        echo "MSG: B-Deck file found --> ${BDECK}"
    else
        echo "WARNING: B-Deck not found --> ${BDECK}"
        continue
    fi


    for CYCLE in ${ALL_CYCLES[@]}; do

        # If an entry exists for MODEL and DATE, then TCNAME will not be empty
        TCNAME=`grep "$CYCLE" ${BDECK} | head -1 | cut -d "," -f28 | sed -e 's/^[[:space:]]*//'`
        #TCNAME=`grep "${CYCLE}" ${BDECK} | grep -v " 50, NEQ," | grep -v " 64, NEQ," | cut -d "," -f28 | sed -e 's/^[[:space:]]*//' | tr '[:upper:]' '[:lower:]'`
        if [ -z "$TCNAME" ]; then
            echo "WARNING: B-Deck entry not found for ${CYCLE}"
            continue
        else
            echo "MSG: B-Deck entry found for ${CYCLE}"
        fi


        # Copy this entry to the new parsed ATCF
        OFILE="${ODIR}${TCNAME,,}${SID}${BASIN2}.${CYCLE}.trak.${MODEL,,}.atcfunix"
        if [ -f ${OFILE} ]; then
            grep "${MODEL}" ${ADECKDIR}${ADECK} | grep "${CYCLE}" | sort -u | sort -k3,3 -k5,5 -k6,6 > ${ODIR}TMP.atcfunix
            if diff -q "${OFILE}" "${ODIR}TMP.atcfunix" ; then
                echo "MSG: Parsed A-Deck has not changed --> ${OFILE}"
                rm -f ${ODIR}TMP.atcfunix
            else
                echo "MSG: Parsed A-Deck has changed. Copying new version --> ${OFILE}"
                mv ${ODIR}TMP.atcfunix ${OFILE}
            fi
        else
            grep "${MODEL}" ${ADECKDIR}${ADECK} | grep "${CYCLE}" | sort -u | sort -k3,3 -k5,5 -k6,6 > ${OFILE}
            echo "MSG: Parsed A-Deck does not exist. Writing new file --> ${OFILE}"
        fi

    done
done

echo "MSG: Completed at `date`"
