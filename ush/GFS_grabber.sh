#!/bin/sh --login

################################################################################
#### UNIX Script Documentation Block
##
## Script name:         GFS_grabber.sh
##
## Script description:  This script links GFS Forecast GRIB2 data from the last
##                      10 days to be processed by GPLOT grahical software.
##
## Author:              Ghassan J. Alaka, Jr.
##
## Date:                2020-08-20
##
## Script history log:
##
## 2020-08-20  Ghassan J. Alaka, Jr. -- Original version.
##
## Usage: GFS_grabber.sh <Output Directory> <List of files>
##
################################################################################

#set -aeu


# Parse input arguments
while test $# -gt 0; do

    case "$1" in
        -h|--help)
            echo "Create A-Deck -- parse & create combined A-Deck files"
            echo " "
            echo "create_adeck.sh [options]"
            echo " "
            echo "options:"
            echo "-h, --help               show help"
            echo "-d, --dsource            data source (PUB, HWRF)"
            echo "-i1, --input1            1st input data directory"
            echo "-i2, --input2            2nd input data directory"
            echo "-o, --output             output data directory"
            exit 0
            ;;
        -d|--dsource)
            shift
            if test $# -gt 0; then
                DSOURCE="${1}"
            else
                echo "ERROR: Data source not specified."
                exit 1
            fi
            shift
            ;;
        -i1|--input1)
            shift
            if test $# -gt 0; then
                IDIR1="${1}"
                if [ ! -d "${IDIR1}" ]; then
                    echo "ERROR: First input directory does not exist."
                    exit 1
                fi
            else
                echo "ERROR: First input directory not specified."
                exit 1
            fi
            shift
            ;;
        -i2|--input2)
            shift
            if test $# -gt 0; then
                IDIR2="${1}"
                if [ ! -d "${IDIR2}" ]; then
                    echo "ERROR: Second input directory does not exist."
                    exit 1
                fi
            else
                echo "WARNING: Second input directory not specified. Defaulting to empty."
                IDIR2=""
            fi
            shift
            ;;
        -o|--output)
            shift
            if test $# -gt 0; then
                ODIR="${1}"
                if [ ! -d "${ODIR}" ]; then
                    echo "MSG: Creating output directory because it does not exist."
                    mkdir -p ${ODIR}
                fi
            else
                echo "ERROR: Output directory not specified."
                exit 1
            fi
            shift
            ;;
        *)
            break
            ;;

    esac

done

echo "MSG: GFS_grabber.sh started at `date`."

# Determine the range of dates to be processed.
date_old=`/bin/date +"%Y%m%d%H" --date="10 days ago"`
date_now=`/bin/date +"%Y%m%d%H"`
echo "MSG: Maintain links to GFS data between ${date_old} and ${date_now}."
echo $DSOURCE
# For the public data directory
if [ "$DSOURCE" == "PUB" ]; then

    # DATA RETRIEVAL
    echo "MSG: Searching for data in the public data directory."

    ALL_FILES=( `find ${IDIR1}/ -regextype sed -regex ".*[0-9]\{13\}"` )

    for FILE in ${ALL_FILES[@]}; do

        echo "MSG: Processing ${FILE}"
        FILE_BASE=`basename ${FILE}`

        # Parse the raw file name (YYJJJHHmmFFFF)
        YY=`echo "${FILE_BASE}" | cut -c1-2`	# 2-digit year
        JJJ=`echo "${FILE_BASE}" | cut -c3-5`	# 3-digit Julian day
        HH=`echo "${FILE_BASE}" | cut -c6-7`	# 2-digit hour
        FHR=`echo "${FILE_BASE}" | cut -c11-13`	# 3-digit forecast lead time

        # Define the current cycle as YYYYMMDDHH
        GFS_DATE=`date -d "20${YY}-01-01 +${JJJ}days -1days" +"%Y%m%d"`${HH}
        ODIR_NOW="${ODIR}/gfs.${GFS_DATE}"

	echo "MSG: File information: YEAR=$YY JULIAN=$JJJ HOUR=$HH LEAD-TIME=$FHR"

	# Create output directory if it does not exist.
	if [ ! -d ${ODIR_NOW} ]; then
            mkdir -p ${ODIR_NOW}
	fi

	# Check if the file exists. If not, link it.
        OFILE="gfs.t${HH}z.pgrb2.0p25.f${FHR}.grb2"
	if [ ! -f ${ODIR_NOW}/${OFILE} ]; then
            echo "MSG: Linking GFS file --> ${ODIR_NOW}/${OFILE}"
	    ln -sf ${FILE} ${ODIR_NOW}/${OFILE}
	else
	    echo "MSG: GFS file already linked --> ${ODIR_NOW}/${OFILE}"
	fi

        # Check if the FHR=000 file exists. If not, link it.
        OFILE="gfs.t${HH}z.pgrb2.0p25.f000.grb2"
        FILE0="${YY}${JJJ}${HH}000000"
        if [ ! -f ${ODIR_NOW}/${OFILE} ] && [ -d ${IDIR2} ]; then
            echo "MSG: Linking GFS file --> ${ODIR_NOW}/${OFILE}"
            ln -sf ${IDIR2}/${FILE0} ${ODIR_NOW}/${OFILE}
        fi

    done


    # SCRUBBING
    echo "MSG: Checking available GFS files."

    ALL_DIRS=( `find ${ODIR}/ -type d -regextype sed -regex ".*gfs.[0-9]\{8\}"` )

    for DIR in ${ALL_DIRS[@]}; do

        echo "MSG: Checking on --> ${DIR}"

        DIR_BASE=`echo ${DIR} | basename`
        GFS_DATE=`echo ${DIR_BASE} | rev | cut -d'.' -f1 | rev`
        YYYY=`echo $iDate | cut -c1-4`
        YY=`echo $iDate | cut -c3-4`
        MM=`echo $iDate | cut -c5-6`
        DD=`echo $iDate | cut -c7-8`
        HH=`echo $iDate | cut -c9-10`
        JJJ=`/bin/date -d "$YYYY-$MM-$DD" +"%j"`

        if find ${IDIR1}/ -name "${YY}${JJJ}${HH}*"; then
            echo "MSG: GFS files are available for: ${GFS_DATE}"
            echo "MSG: Keeping links."
        else
            echo "MSG: GFS files are no longer available for: ${GFS_DATE}"
            echo "MSG: Deleting links."
            rm -rf ${DIR}
        fi
    done


elif [ "${DSOURCE}" == "HWRF" ]; then

    ALL_DIRS=( `ls ${IDIR1} | xargs -n 1 basename` )

    for DIR in ${ALL_DIRS[@]}; do

        echo "MSG: Processing ${DIR}"

	GFS_DATE=`echo "${FILE}" | awk -F "." '{print $NF}'`
	if [ ${GFS_DATE} -ge $date_old ]; then
	
	    #Check if directory exists
            if [ ! -d ${ODIR}/${DIR} ]; then
                mkdir -p ${ODIR}/${DIR}
            fi

            # Check if directory is empty
            if ls ${IDIR1}/${DIR}/*pgrb2.* 1> /dev/null 2>&1; then
                echo "MSG: GFS files are available."
                rm -rf ${ODIR}/${DIR}/*grb2*
                ln -sf ${IDIR1}/${DIR}/*pgrb2.* ${ODIR}/${DIR}/.
	        find ${ODIR}/${DIR}/gfs* -exec mv '{}' '{}'.grb2 \;
                echo "MSG: GFS data successfully linked."
                ls ${ODIR}/${DIR}/
            else
                echo "MSG: GFS files are not available."
            fi
	else
	    if [ -d ${ODIR}/${DIR} ]; then
		rm -rf ${ODIR}/${DIR}/
		echo "MSG: Deleting old directory."
	    else
		echo "MSG: Directory does not exist. Skipping deletion."
	    fi
	fi	
    done

fi


echo "MSG: GFS_grabber.sh completed at `date`."
echo "COMPLETE!"
