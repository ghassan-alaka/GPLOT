#!/bin/sh --login
#
# Invocation:  ./parse_atcf.sh AVNO /lfs1/projects/hur-aoml/Ghassan.Alaka/noscrub/GFS_Forecast/ /lfs1/projects/hur-aoml/Ghassan.Alaka/adeck/NHC/ /lfs1/projects/hur-aoml/Ghassan.Alaka/bdeck/ /lfs3/projects/hwrf-data/hwrf-input/SYNDAT-PLUS/ 1
#              ./parse_atcf.sh HAFS /lfs1/projects/hur-aoml/Ghassan.Alaka/noscrub/HAFSV0.B_Forecast/ /lfs3/projects/hur-aoml/rthr-aoml/hafstmp/HAFS_jet/com/ /lfs1/projects/hur-aoml/Ghassan.Alaka/bdeck/ /lfs3/projects/hwrf-data/hwrf-input/SYNDAT-PLUS/ 2
#              ./parse_atcf.sh HAFS /lfs1/projects/hur-aoml/Ghassan.Alaka/noscrub/HAFSV0.B_Forecast/ /lfs3/projects/hur-aoml/rthr-aoml/noscrub/hafstrak/HAFS_jet/ /lfs1/projects/hur-aoml/Ghassan.Alaka/bdeck/ /lfs3/projects/hwrf-data/hwrf-input/SYNDAT-PLUS/ 2
#
# TYPE:  1 --> Parse from NHC A-Deck (Time-Dependent; Good to exclude older ATCFs during real-time)
# TYPE:  2 --> Parse from combined A-Deck (e.g., HAFS) (Time-Dependent; Good to exclude older ATCFs during real-time)
# TYPE:  3 --> Parse from NHC A-Deck (Non-Time-Dependent; Good to include all ATCFs for research mode)
# TYPE:  4 --> Parse from combined A-Deck (e.g., HAFS) (Non-Time-Dependent; Good to include all ATCFs for research mode)

#set -aeu

echo "MSG: parse_atcf.sh started at `date`"

MODEL="$1"
ODIR="$2"
ADECKDIR="$3"
BDECKDIR="$4"
TCVDIR="$5"
TYPE="$6"
AAA="$7"
#ODIR="/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/GFS_Forecast/"
#ADECKDIR="/lfs1/projects/hur-aoml/Ghassan.Alaka/adeck/NHC/"
#BDECKDIR="/lfs1/projects/hur-aoml/Ghassan.Alaka/bdeck/ftp.nhc.noaa.gov/atcf/btk/"
MMIN="-720"
SID_FILE="${GPLOT_DIR}/tbl/SIDs_Old_New.dat"

mkdir -p ${ODIR}
cd ${ODIR}

# Checks
if [ ! -d "$ADECKDIR" ]; then
    echo "ERROR: ADECKDIR is not a directory. Please fix this."
    exit 1
fi
if [ ! -d "$BDECKDIR" ]; then
    echo "ERROR: BDECKDIR is not a directory. Please fix this."
    exit 2
fi
if [ ! -d "$TCVDIR" ]; then
    echo "ERROR: TCVDIR is not a directory. Please fix this."
    exit 3
fi

# Find all A-DECKs
if [ -z "${AAA}" ]; then
    if [ "$TYPE" == "1" ]; then
        ALL_ADECKS=( `find "${ADECKDIR}" -name '*a[a-z][a-z][0-9][0-9][1-2][0-9][0-9][0-9]*.dat' -type f -mmin ${MMIN}` )
    elif [ "$TYPE" == "2" ]; then
        ALL_ADECKS=( `find "${ADECKDIR}" -name '*[1-2][0-9][0-9][0-9][0-1][0-9][0-3][0-9][0-2][0-9]*.atcfunix.all*' -type f -mmin ${MMIN}` )
    elif [ "$TYPE" == "3" ]; then
        ALL_ADECKS=( `find "${ADECKDIR}" -name '*a[a-z][a-z][0-9][0-9][1-2][0-9][0-9][0-9]*.dat' -type f` )
    elif [ "$TYPE" == "4" ]; then
        ALL_ADECKS=( `find "${ADECKDIR}" -name '*[1-2][0-9][0-9][0-9][0-1][0-9][0-3][0-9][0-2][0-9]*.atcfunix.all*' -type f` )
    elif [ "$TYPE" == "5" ]; then
        ALL_ADECKS=( `find "${ADECKDIR}" -name '*a[a-z][a-z][0-9][0-9][1-2][0-9][0-9][0-9]*.dat' -type f -mmin -10080` )
    elif [ "$TYPE" == "6" ]; then
        ALL_ADECKS=( `find "${ADECKDIR}" -name '*[1-2][0-9][0-9][0-9][0-1][0-9][0-3][0-9][0-2][0-9]*.atcfunix.all*' -type f -mmin -10080` )
    fi
else
    if [ "$TYPE" == "0" ]; then
        ALL_ADECKS=( `find "${ADECKDIR}" -name ${AAA} -type f` )
    elif [ "$TYPE" == "1" ]; then
        ALL_ADECKS=( `find "${ADECKDIR}" -name ${AAA} -type f -mmin ${MMIN}` )
    fi
fi

# Now, loop over the available A-DECKs
for ADECK in ${ALL_ADECKS[@]}; do

    # Get some information from the A-Deck file name
    echo ""
    echo "MSG: Found this ADECK --> ${ADECK}"

    #ALL_SNUM=( `awk -v MODEL="${MODEL}" -F ', ' '$5==MODEL && $2!="00"' ${ADECK} | cut -d "," -f2 | sort -u | sed -e 's/^[[:space:]]*//'` )
    ALL_SNUM=( `cat ${ADECK} | tr -d "[:blank:]" | awk -v MODEL="${MODEL}" -F, '$5==MODEL && $2!="00"' | cut -d "," -f2 | sort -u` )
    for SNUM in ${ALL_SNUM[@]}; do
        echo "MSG: Found this Storm Number --> ${SNUM}"
        if [ "$SNUM" == "00" ]; then
            continue
        fi

        # Get a list of available cycles
        ALL_CYCLES=( `cat ${ADECK} | tr -d "[:blank:]" | awk -v MODEL="${MODEL}" -v SNUM="${SNUM}" -F, '$5==MODEL && $2==SNUM' | cut -d "," -f3 | sort -u` )

        for CYCLE in ${ALL_CYCLES[@]}; do
            echo "MSG: Found this cycle --> ${CYCLE}"
            # Get basin and year information. This will help build the output ATCF
            BASIN="`cat ${ADECK} | tr -d "[:blank:]" | awk -v MODEL="${MODEL}" -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -F, '$5==MODEL && $2==SNUM && $3==CYCLE' | cut -d "," -f1 | sort -u`"
            YEAR="`echo "${CYCLE}" | cut -c1-4`"
            YMD="`echo "${CYCLE}" | cut -c1-8`"
            HHHH="`echo "${CYCLE}" | cut -c9-10`00"

            # Check that the B-Deck is available
            BDECK=${BDECKDIR}/b${BASIN,,}${SNUM}${YEAR}.dat
            if [ -f ${BDECK} ]; then
               echo "MSG: B-Deck file found --> ${BDECK}"
            else
               echo "WARNING: B-Deck file not found --> ${BDECK}"
               BDECK=
            fi

            # Check that TCVitals is available.
            SYNDAT="${TCVDIR}/syndat_tcvitals.${YEAR}"
            if [ -f ${SYNDAT} ]; then
                echo "MSG: TCVitals file found --> ${SYNDAT}"
            else
               echo "WARNING: TCVitals file not found --> ${SYNDAT}"
               SYNDAT=
            fi

            # Get the TC name. This is for the Parsed ATCF file name
            TCNAME=""

            # First, try to get the TC name from the CARQ entry in the A-Deck
            if [ -z "${TCNAME}" ]; then
                TCNAME="`cat ${ADECK} | tr -d "[:blank:]" | awk -v CYCLE="${CYCLE}" -F, '$3==CYCLE && $5=="CARQ"' | head -1 | cut -d "," -f28`"
            fi

            # Second, try to get the TC name from the Best Track entry in the B-Deck
            if [ -z "${TCNAME}" ]; then
                if [ ! -z "${BDECK}" ]; then
                    TCNAME="`awk -v CYCLE="${CYCLE}" '$3~CYCLE' ${BDECK} | head -1 | cut -d "," -f28 | awk '{$1=$1};1'`"
                fi
            fi

            # Third, try to get the TC name from the TCVItals entry
            if [ -z "${TCNAME}" ]; then
                if [ ! -z "${SYNDAT}" ]; then
                    TCNAME="`awk -v YMD="${YMD}" -v HHHH="${HHHH}" -v SID="${SID}" -F ' ' '$4==YMD && $5==HHHH && $2==SID' ${SYNDAT} | head -1 | awk '{print $3}' | sed -e 's/^[[:space:]]*//'`"
                fi
            fi

            # If TCNAME still is not set, set it to something generic like "INVEST" (for SNUM=90-99) or "STORM"
            if [ -z "${TCNAME}" ]; then
                echo "WARNING: TC Name not found for ${CYCLE}"
                if [ "$(echo ${SNUM} | cut -c1)" == "9" ]; then
                    TCNAME="INVEST"
                else
                    TCNAME="STORM"
                fi
            fi

            echo "MSG: TCNAME=${TCNAME}"

            # Get the first latitude and longitude from the B-Deck file
            if [ -f "${BDECK}" ]; then
                MY_DECK="${BDECK}"
            else
                MY_DECK="${ADECK}"
            fi
            MLON="1"
            if [ "$(awk -F',' 'NR==1{print $8}' ${MY_DECK} | tr -d "[:blank:]" | rev | cut -c1)" == "W" ]; then
                MLON="-1"
            fi
            LON="$(expr ${MLON} \* $(awk -F',' 'NR==1{print $8}' ${MY_DECK} | tr -d "[:blank:]" | rev | cut -c2- | rev) )"
            MLAT="1"
            if [ "$(awk -F',' 'NR==1{print $7}' ${MY_DECK} | tr -d "[:blank:]" | rev | cut -c1)" == "S" ]; then
                MLAT="-1"
            fi
            LAT="$(expr ${MLAT} \* $(awk -F',' 'NR==1{print $7}' ${MY_DECK} | tr -d "[:blank:]" | rev | cut -c2- | rev) )"



            # Parse only these basins
            if [ "${BASIN,,}" = "al" ]; then
                BASIN2="l"
                echo "MSG: ATCF basin --> North Atlantic Ocean"
            elif [ "${BASIN,,}" = "ep" ]; then
                BASIN2="e"
                echo "MSG: ATCF basin --> eastern North Pacific Ocean"
            elif [ "${BASIN,,}" = "cp" ]; then
                BASIN2="c"
                echo "MSG: ATCF basin --> central North Pacific Ocean"
            elif [ "${BASIN,,}" = "wp" ]; then
                BASIN2="w"
                echo "MSG: ATCF basin --> western North Pacific Ocean"
            elif [ "${BASIN,,}" = "io" ]; then
                if [ "${LON}" -ge "800" ]; then
                    echo "MSG: ATCF basin --> Bay of Bengal (North Indian Ocean)"
                    BASIN2="b"
                else
                    echo "MSG: ATCF basin --> Arabian Sea (North Indian Ocean)"
                    BASIN2="a"
                fi
                echo "MSG: ATCF basin --> North Indian Ocean"
            elif [ "${BASIN,,}" = "sh" ]; then
                if [ "${LON}" -ge "1350" ] || [ "${LON}" -le "-1200" ] ; then
                    echo "MSG: ATCF basin --> South Pacific Ocean (Southern Hemisphere)"
                    BASIN2="p"
                else
                    echo "MSG: ATCF basin --> South Indian Ocean (Southern Hemisphere)"
                    BASIN2="s"
                fi
            else
                echo "WARNING: ATCF basin not recognized. Skipping."
                continue
            fi

            # Define the Storm ID (SID)
            SID="${SNUM}${BASIN2^^}"

            # If applicable, get the old SID
            SID_OLD=""
            if [ -f "${SID_FILE}" ]; then
                SID_OLD=`grep "${BASIN^^}${SNUM}${YEAR}" ${SID_FILE} | awk '{ print $4 }'`
            fi
            if [ ! -z "${SID_OLD}" ]; then
                echo "MSG: Found this old Storm ID --> ${SID_OLD}"
            fi


            # Copy this entry to the new parsed ATCF
            OFILE="${ODIR}/${TCNAME,,}${SID,,}.${CYCLE}.trak.${MODEL,,}.atcfunix"
            OFILE2="${ODIR}/${TCNAME,,}${SID_OLD,,}.${CYCLE}.trak.${MODEL,,}.atcfunix"
            OFILE3="${ODIR}/invest${SID_OLD,,}.${CYCLE}.trak.${MODEL,,}.atcfunix"
            TMPFILE="${ODIR}/$(date +%N).${TCNAME,,}${SID,,}.${CYCLE}.trak.${MODEL,,}.atcfunix"
            #echo "OFILE2 = ${OFILE2}"
            if [ -z "${SID_OLD}" ]; then
                echo "WARNING: Old Storm ID not found. This might be OK."
            elif [ -f ${OFILE2} ] || [ -f "${OFILE3}" ]; then
                echo "MSG: ATCF already exists for the old SID (${SID_OLD}) --> ${OFILE2}"
                rm -f ${OFILE}
                continue
            fi
            if [ -f ${OFILE} ]; then
                #awk -v MODEL="${MODEL}" -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -F ', ' '$5==MODEL && $2==SNUM && $3==CYCLE' ${ADECK} | sort -u | sort -k3,3 -k5,5 -k6,6n > ${TMPFILE}
                #grep "${MODEL}," ${ADECK} | grep "${SNUM}," | grep "${CYCLE}," | sort -u | sort -k3,3 -k5,5 -k6,6n > ${TMPFILE}
                grep "${MODEL}," ${ADECK} | awk -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -F ', ' '$2==SNUM && $3==CYCLE' | sort -u | sort -k3,3 -k5,5 -k6,6n > ${TMPFILE}
                if diff -q "${OFILE}" "${TMPFILE}" ; then
                    echo "MSG: Parsed A-Deck has not changed --> ${OFILE}"
                    rm -f ${TMPFILE}
                else
                    echo "MSG: Parsed A-Deck has changed. Copying new version --> ${OFILE}"
                    mv ${TMPFILE} ${OFILE}
                fi
            else
                #awk -v MODEL="${MODEL}" -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -F ', ' '$5==MODEL && $2==SNUM && $3==CYCLE' ${ADECK} | sort -u | sort -k3,3 -k5,5 -k6,6n > ${OFILE}
                #grep "${MODEL}," ${ADECK} | grep "${SNUM}," | grep "${CYCLE}," | sort -u | sort -k3,3 -k5,5 -k6,6n > ${OFILE}
                grep "${MODEL}," ${ADECK} | awk -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -F ', ' '$2==SNUM && $3==CYCLE' | sort -u | sort -k3,3 -k5,5 -k6,6n > ${OFILE}
                echo "MSG: Parsed A-Deck does not exist. Writing new file --> ${OFILE}"
            fi

        done
    done
done

echo "MSG: parse_atcf.sh completed at `date`"
