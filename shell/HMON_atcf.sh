#!/bin/sh -login
#
# Invocation:  ./parse_atcf.sh AVNO /lfs1/projects/hur-aoml/Ghassan.Alaka/noscrub/GFS_Forecast/ /lfs1/projects/hur-aoml/Ghassan.Alaka/adeck/NHC/ /lfs1/projects/hur-aoml/Ghassan.Alaka/bdeck/ /lfs3/projects/hwrf-data/hwrf-input/SYNDAT-PLUS/ 1
#
# TYPE:  1 --> Parse from NHC A-Deck
# TYPE:  2 --> Parse from combined A-Deck

#set -aeu

echo "MSG: HMON_atcf.sh started at `date`"

ODIR="$1"

mkdir -p ${ODIR}
cd ${ODIR}

IDIR="/scratch2/BMC/public/data/grids/hmon"
IEXT="trak.hmon.atcfunix"
OEXT="atcfunix"

# Find all ATCF output
ALL_ATCF=( `find "${IDIR}/" -regextype sed -regex ".*[a-z]\+[0-9]\{2\}[a-z].[0-9]\{10\}.*${IEXT}" -type f` )

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
        cp -p ${ATCF} ${OFILE}
    fi

done

echo "MSG: HMON_atcf.sh completed at `date`"

exit

