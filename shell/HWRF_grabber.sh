#!/bin/sh --login
#
# This script links GFS Forecast GRIB2 data
# from the last 1 month to be plotted with
# the GPLOT plotting package.

#set -aeu

echo "HWRF_grabber.sh started `date`"
DSOURCE="$1"
ODIR="$2"


# Define certain variables based on DSOURCE.
if [ "$DSOURCE" == "PUB" ]; then
    HWRFDIR="/public/data/grids/hwrf/"
    #ODIR="/lfs1/projects/hur-aoml/Ghassan.Alaka/pytmp/HWRF_Forecast/"
fi


# Get an array of all files
ALLFILES=( `find $HWRFDIR -type f -name "*grb2" | xargs -n 1 basename` )

# Loop over all files
for FILE in ${ALLFILES[@]}; do
    if [ "$DSOURCE" == "PUB" ]; then
        YMDH=`echo "$FILE" | cut -d "." -f2`
        FHR=`echo "$FILE" | cut -d "." -f6 | cut -c2-4`
        LONGNAME=`echo "$FILE" | cut -d "." -f1`
        SID=`echo "$LONGNAME" | rev | cut -c1-3 | rev`

        ODIR_FULL="${ODIR}/${YMDH}/${SID^^}/"

        #Check if directory exists
        if [ ! -d ${ODIR_FULL} ]; then
            mkdir -p ${ODIR_FULL}
        fi

        #Check if the file exists. If not, link it.
        if [ ! -f ${ODIR_FULL}${FILE} ]; then
            echo "MSG: Linking HWRF file --> ${ODIR_FULL}${FILE}"
            ln -sf ${HWRFDIR}/${FILE} ${ODIR_FULL}/${FILE}
        else
            echo "MSG: HWRF file already linked --> ${ODIR_FULL}${FILE}"
        fi
    fi
done


# Remove directories for which HWRF files are no longer available.
if [ "$DSOURCE" == "PUB" ]; then
    echo "MSG: Checking available HWRF files."

    # Find files and soft links.
    ALL_HWRF=( `find ${ODIR} -type f` `find ${ODIR} -type l`) # | xargs -n 1 basename` )

    for FILE in "${ALL_HWRF[@]}"; do
        echo "MSG: Working on --> $FILE"

        # Extract info from the file path
        HWRF_BASE=`echo "$FILE" | xargs -n 1 basename`
        HWRF_PATH=`echo "$FILE" | rev | cut -d "/" -f2- | rev`
        YMDH=`echo "$HWRF_BASE" | cut -d "." -f2`

        # Remove the link if the source file does not exist in $HWRFDIR
        if ls ${HWRFDIR}/${HWRF_BASE} 2> /dev/null; then
            echo "MSG: HWRF file is available for ${YMDH}. Keeping link."
        else
            echo "MSG: HWRF file is no longer available for ${YMDH}. Deleting link."
            rm -f ${FILE}
            if [ -z "$(ls -A ${HWRF_PATH})" ]; then
                echo "MSG: Directory is empty. Deleting it."
                rm -rf ${HWRF_PATH}
            fi
        fi
    done

fi


echo "HWRF_grabber.sh completed `date`"
