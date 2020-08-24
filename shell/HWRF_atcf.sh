#!/bin/sh -login
#
# Invocation:  ./parse_atcf.sh AVNO /lfs1/projects/hur-aoml/Ghassan.Alaka/noscrub/GFS_Forecast/ /lfs1/projects/hur-aoml/Ghassan.Alaka/adeck/NHC/ /lfs1/projects/hur-aoml/Ghassan.Alaka/bdeck/ /lfs3/projects/hwrf-data/hwrf-input/SYNDAT-PLUS/ 1
#
# TYPE:  1 --> Parse from NHC A-Deck
# TYPE:  2 --> Parse from combined A-Deck

#set -aeu

echo "MSG: HWRF_atcf.sh started at `date`"

ODIR="$1"
#ADECKDIR="$3"
#BDECKDIR="$4"
#TCVDIR="$5"
#TYPE="$6"
#ODIR="/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/GFS_Forecast/"
#ADECKDIR="/lfs1/projects/hur-aoml/Ghassan.Alaka/adeck/NHC/"
#BDECKDIR="/lfs1/projects/hur-aoml/Ghassan.Alaka/bdeck/ftp.nhc.noaa.gov/atcf/btk/"
#MMIN="-43200"
#SID_FILE="${GPLOT_DIR}/tbl/SIDs_Old_New.dat"

mkdir -p ${ODIR}
cd ${ODIR}

IDIR="/public/data/grids/hwrf/"
IEXT="3hourly"
OEXT="atcfunix"

# Find all ATCF output
ALL_ATCF=( `find "${IDIR}" -name "*[1-2][0-9][0-9][0-9][0-1][0-9][0-3][0-9][0-2][0-9]*${IEXT}" -type f` )

echo ${ALL_ATCF[*]}

for ATCF in ${ALL_ATCF[@]}; do
    echo ""
    echo "MSG: Found this ATCF --> ${ATCF}"

    ATCF_BASE=`basename $ATCF`
    ATCF_PATH="`echo ${ATCF} | rev | cut -d'/' -f2- | rev`/"

    OFILE="${ODIR}`echo ${ATCF_BASE} | rev | cut -d '.' -f2- | rev`.${OEXT}"

    if [ -f ${OFILE} ]; then
        echo "MSG: ATCF already exists. Assuming that version is the same or newer."
        #if diff -q "${OFILE}" "${ATCF}" >/dev/null ; then
        #    echo "MSG: ATCF has not changed --> ${OFILE}"
        #else
        #    echo "MSG: ATCF has changed. Copying new version --> ${OFILE}"
        #    cp ${ATCF} ${OFILE}
        #fi
    else
        echo "MSG: ATCF does not exist. Writing new file --> ${OFILE}"
        cp ${ATCF} ${OFILE}
    fi

done

echo "MSG: HWRF_atcf.sh completed at `date`"

exit

