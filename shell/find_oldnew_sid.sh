#!/bin/sh --login
#
# Invocation:  ./find_allnew_sid.sh /lfs3/projects/hwrf-data/hwrf-input/SYNDAT-PLUS/syndat_tcvitals.2019 /home/Ghassan.ALaka/GPLOT/tbl/SID_Test.dat
#

#set -aeu

echo "MSG: find_oldnew_sid.sh started at `date`"

# Read variables from the command line
TCV="$1"
OPATH="$2"


# Handle the TCVitals file
if [ ! -f "${TCV}" ]; then
    echo "ERROR: TCVitals file is not valid."
    exit
fi

# Back out the TCVitals directory and file name
TCVFILE=`basename ${TCV}`
TCVDIR="`echo "${TCV}" | rev | cut -d "/" -f2- | rev`/"

# Process the output path
if [ -f "${OPATH}" ]; then
    echo "MSG: Existing output file detected. Will append to it."
    echo "MSG: You must delete it in order to start fresh."
    NEW_OPATH="False"
else
    NEW_OPATH="True"
fi

# Back out the output directory and the outfile file name
OFILE=`basename ${OPATH}`
ODIR="`echo "${OPATH}" | rev | cut -d "/" -f2- | rev`/"

# Copy the TCVitals file into a temporary directory
TMPDIR="${ODIR}TMP.$(date +%N)/"
if [ -d ${TMPDIR} ]; then
    rm -rf ${TMPDIR}
fi
mkdir ${TMPDIR}
NEW_TCV="${TMPDIR}${TCVFILE}"
cp ${TCV} ${NEW_TCV}
if [ "$NEW_OPATH" == "False" ]; then
    cp ${OPATH} ${TMPDIR}.
fi

# Remove all duplicate entries. This dramatically 
# improves the runtime.
sort -u ${TCV} | sort -k4,4 -k5,5 -k2,2 > ${NEW_TCV}

# Create an array of unique dates (YMDH)
TMP_YMD=( `awk '{ print $4 }' ${TCV}` )
TMP_Hm=( `awk '{ print $5 }' ${TCV}` )
ALL_YMDH=()
ALL_YMD=()
ALL_Hm=()
i=0
for YMD in ${TMP_YMD[@]}; do
    YMDH="${YMD}$(echo "${TMP_Hm[i]}" | cut -c1-2)"
    if [[ "${ALL_YMDH[*]}" != *"${YMDH}"* ]]; then
        ALL_YMDH+=( "${YMDH}" )
        ALL_YMD+=( "${YMD}" )
        ALL_Hm+=( "${TMP_Hm[i]}" )
    fi
    ((i++))
done
#echo "${ALL_YMDH[*]}"

# Loop over all individual dates
i=0
for YMDH in "${ALL_YMDH[@]}"; do

    echo ""
    echo "MSG: Working on this cycle --> ${YMDH}"

    # Identify all unique SIDs for the given date
    ALL_SIDS=( `awk -v YMD="${ALL_YMD[i]}" -v Hm="${ALL_Hm[i]}" '$4==YMD && $5==Hm { print $2 }' ${NEW_TCV} | sort -u`  )
    echo "MSG: Found these Storm IDs --> ${ALL_SIDS[*]}"

    # If no SIDs or only 1 SID, then no comparisons to make.
    if [[ ${#ALL_SIDS} -eq 1 ]] || [ -z "${ALL_SIDS[*]}" ]; then
        echo "MSG: Skipping because not enough storms to compare."
        continue
    fi

    # Get the year
    YYYY=`echo "$YMDH" | cut -c1-4`

    # Loop over SIDs to find potential matches.
    # Most often, it will be a 90-99 storm being linked with
    # a 01-50 storm.
    for SID1 in ${ALL_SIDS[@]}; do

        # Get some information
        BASIN1="$(echo "${SID1}" | cut -c3)"
        BASINLONG1=""
        if [ "${BASIN1^^}" == "L" ]; then
            BASINLONG1="AL"
        elif [ "${BASIN1^^}" == "E" ]; then
            BASINLONG1="EP"
        elif [ "${BASIN1^^}" == "C" ]; then
            BASINLONG1="CP"
        elif [ "${BASIN1^^}" == "W" ]; then
            BASINLONG1="WP"
        fi

        # Is BASINLONG1 is undefined, then the basin is not recognized. Skip.
        if [ -z "$BASINLONG1" ]; then
            echo "MSG: Skipping because BASINLONG1 is undefined."
            continue
        fi

        # Get the storm number
        SNUM1="`echo "$SID1" | cut -c1-2`"

        # Get the latitude
        LAT1=( `awk -v YMD="${ALL_YMD[i]}" -v Hm="${ALL_Hm[i]}" -v SID="${SID1}" '$4==YMD && $5==Hm && $2==SID { print $6 }' ${NEW_TCV}` )
        LAT1="${LAT1[0]}"
        LAT1="`echo "${LAT1}" | cut -c1-2`.`echo "${LAT1}" | cut -c3`"
        #f [ "$(echo "${LAT1}" | cut -c4)" == "N" ]; then
        #   LAT1="`echo "${LAT1}" | cut -c1-2`.`echo "${LAT1}" | cut -c3`"
        #lif [ "$(echo "${LAT1}" | cut -c4)" == "S" ]; then
        #   LAT1="-`echo "${LAT1}" | cut -c1-2`.`echo "${LAT1}" | cut -c3`"
        #i
        LAT1="${LAT1#0}"

        # Get the longitude
        LON1=( `awk -v YMD="${ALL_YMD[i]}" -v Hm="${ALL_Hm[i]}" -v SID="${SID1}" '$4==YMD && $5==Hm && $2==SID { print $7 }' ${NEW_TCV}` )
        LON1="${LON1[0]}"
        LON1="`echo "${LON1}" | cut -c1-3`.`echo "${LON1}" | cut -c4`"
        #if [ "$(echo "${LON1}" | cut -c5)" == "E" ]; then
        #    LON1="`echo "${LON1}" | cut -c1-3`.`echo "${LON1}" | cut -c4`"
        #elif [ "$(echo "${LON1}" | cut -c5)" == "W" ]; then
        #    LON1="-`echo "${LON1}" | cut -c1-3`.`echo "${LON1}" | cut -c4`"
        #fi
        LON1="${LON1#0}"

        # Loop over the Storm IDs again to find a potential match.
        for SID2 in ${ALL_SIDS[@]}; do

            # Skip if SID1=SID2
            if [ "$SID1" == "$SID2" ]; then
                echo "MSG: Skipping because the Storm IDs match: SID1=${SID1}, SID2=${SID2}."
                continue
            fi

            # Skip if the basins are different
            if [ "$(echo "${SID1}" | cut -c3)" != "$(echo "${SID2}" | cut -c3)" ]; then
                echo "MSG: Skipping because the storm ID basins are different: BASIN1=$(echo "${SID1}" | cut -c3), BASIN2=$(echo "${SID2}" | cut -c3)."
                continue
            fi

            # Process the basin. The basins must match!
            BASIN2="$(echo "${SID2}" | cut -c3)"
            if [ "$BASIN1" != "$BASIN2" ]; then
                echo "MSG: Skipping because the basins do not match: BASIN1=${BASIN1}, BASIN2=${BASIN}."
                continue
            fi
            BASINLONG2=""
            if [ "${BASIN2^^}" == "L" ]; then
                BASINLONG2="AL"
            elif [ "${BASIN2^^}" == "E" ]; then
                BASINLONG2="EP"
            elif [ "${BASIN2^^}" == "C" ]; then
                BASINLONG2="CP"
            elif [ "${BASIN2^^}" == "W" ]; then
                BASINLONG2="WP"
            fi

            # Is BASINLONG2 is undefined, then the basin is not recognized. Skip.
            if [ -z "$BASINLONG2" ]; then
                echo "MSG: Skipping because BASINLONG2 is undefined."
                continue
            fi


            # Define Storm Number
            SNUM2="`echo "$SID2" | cut -c1-2`"

            # Determine which storm number applies to the tropical cyclone.
            # Do the same for the Storm ID.
            if [[ $((10#$SNUM1)) -lt $((10#$SNUM2)) ]]; then
                REAL_SNUM="${SNUM1}"
                REAL_SID="${SID1}"
                PRE_SID="${SID2}"
            else
                REAL_SNUM="${SNUM2}"
                REAL_SID="${SID2}"
                PRE_SID="${SID1}"
            fi
                

            # Get the latitude
            LAT2=( `awk -v YMD="${ALL_YMD[i]}" -v Hm="${ALL_Hm[i]}" -v SID="${SID2}" '$4==YMD && $5==Hm && $2==SID { print $6 }' ${NEW_TCV}` )
            LAT2="${LAT2[0]}"
            LAT2="`echo "${LAT2}" | cut -c1-2`.`echo "${LAT2}" | cut -c3`"
            #f [ "$(echo "${LAT2}" | cut -c4)" == "N" ]; then
            #   LAT2="`echo "${LAT2}" | cut -c1-2`.`echo "${LAT2}" | cut -c3`"
            #lif [ "$(echo "${LAT2}" | cut -c4)" == "S" ]; then
            #   LAT2="-`echo "${LAT2}" | cut -c1-2`.`echo "${LAT2}" | cut -c3`"
            #i
            LAT2="${LAT2#0}"

            # Get the longitude
            LON2=( `awk -v YMD="${ALL_YMD[i]}" -v Hm="${ALL_Hm[i]}" -v SID="${SID2}" '$4==YMD && $5==Hm && $2==SID { print $7 }' ${NEW_TCV}` )
            LON2="${LON2[0]}"
            LON2="`echo "${LON2}" | cut -c1-3`.`echo "${LON2}" | cut -c4`"
            #f [ "$(echo "${LON2}" | cut -c5)" == "E" ]; then
            #   LON2="`echo "${LON2}" | cut -c1-3`.`echo "${LON2}" | cut -c4`"
            #lif [ "$(echo "${LON2}" | cut -c5)" == "W" ]; then
            #   LON2="-`echo "${LON2}" | cut -c1-3`.`echo "${LON2}" | cut -c4`"
            #i
            LON2="${LON2#0}"

            # Find out if there is have a match based on proximity of lat/lon coordinates
            LATDIFF=`echo "scale=1;${LAT1}-${LAT2}" | bc | sed 's/-//'`
            LONDIFF=`echo "scale=1;${LON1}-${LON2}" | bc | sed 's/-//'`
            echo "MSG: LATDIFF=${LATDIFF}, LONDIFF=${LONDIFF}"
            echo "SID1=${SID1},SID2=${SID2},LON1=${LON1},LON2=${LON2},LAT1=${LAT1},LAT2=${LAT2}"
            if (( $(echo "$LATDIFF < 2.0" | bc) ))  && (( $(echo "$LONDIFF < 2.0" | bc) )); then
                echo "WE HAVE A MATCH between $SID1 and $SID2"
echo "Writing to this output file --> ${TMPDIR}${OFILE}"
echo "The line will look like this:"
echo "${BASINLONG2}${REAL_SNUM}${YYYY}     ${YYYY}     ${REAL_SID^^}     ${PRE_SID^^}"
                echo "${BASINLONG2}${REAL_SNUM}${YYYY}     ${YYYY}     ${REAL_SID^^}     ${PRE_SID^^}" >> ${TMPDIR}${OFILE}
            fi

        done
    done

    ((i++))

done


# Sort the output file
sort -u ${TMPDIR}${OFILE} | sort -k2,2 -k1,1 > ${TMPDIR}${OFILE}.TMP
mv ${TMPDIR}${OFILE}.TMP ${TMPDIR}${OFILE}
cp ${TMPDIR}${OFILE} ${OPATH}


# Clean up the temporary TCVitals file and directory
rm -rf ${TMPDIR}

exit








MODEt="$1"
ODIR="$2"
ADECKDIR="$3"
BDECKDIR="$4"
TCVDIR="$5"
TYPE="$6"
#ODIR="/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/GFS_Forecast/"
#ADECKDIR="/lfs1/projects/hur-aoml/Ghassan.Alaka/adeck/NHC/"
#BDECKDIR="/lfs1/projects/hur-aoml/Ghassan.Alaka/bdeck/ftp.nhc.noaa.gov/atcf/btk/"
MMIN="-43200"

mkdir -p ${ODIR}
cd ${ODIR}

# Checks
if [ ! -d "$ADECKDIR" ]; then
    echo "ERROR: ADECKDIR is not a directory. Please fix this."
    exit
fi
if [ ! -d "$BDECKDIR" ]; then
    echo "ERROR: BDECKDIR is not a directory. Please fix this."
    exit
fi
if [ ! -d "$TCVDIR" ]; then
    echo "ERROR: TCVDIR is not a directory. Please fix this."
    exit
fi

# Find all A-DECKs
if [ "$TYPE" == "1" ]; then
    ALL_ADECKS=( `find "${ADECKDIR}" -name '*a[a-z][a-z][0-9][0-9][1-2][0-9][0-9][0-9]*.dat' -type f -mmin ${MMIN}` )
elif [ "$TYPE" == "2" ]; then
    ALL_ADECKS=( `find "${ADECKDIR}" -name '*[1-2][0-9][0-9][0-9][0-1][0-9][0-3][0-9][0-2][0-9]*.atcfunix.all' -type f -mmin ${MMIN}` )
fi

for ADECK in ${ALL_ADECKS[@]}; do

    # Get some information from the A-Deck file name
    echo ""
    echo "MSG: Found this ADECK --> ${ADECK}"

    ALL_SNUM=( `awk -v MODEL="${MODEL}" -F ', ' '$5==MODEL && $2!="00"' ${ADECK} | cut -d "," -f2 | sort -u | sed -e 's/^[[:space:]]*//'` )

    for SNUM in ${ALL_SNUM[@]}; do
        echo "MSG: Found this Storm Number --> ${SNUM}"
        if [ "$SNUM" == "00" ]; then
            continue
        fi

        # Get a list of available cycles
        ALL_CYCLES=( `awk -v MODEL="${MODEL}" -v SNUM="${SNUM}" -F ', ' '$5==MODEL && $2==SNUM' ${ADECK} | cut -d "," -f3 | sort -u | sed -e 's/^[[:space:]]*//'` )

        for CYCLE in ${ALL_CYCLES[@]}; do
            echo "MSG: Found this cycle --> ${CYCLE}"
            # Get basin and year information. This will help build the output ATCF
            BASIN=`awk -v MODEL="${MODEL}" -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -F ', ' '$5==MODEL && $2==SNUM && $3==CYCLE' ${ADECK} | cut -d "," -f1 | sort -u | sed -e 's/^[[:space:]]*//'`
            YEAR=`echo "$CYCLE" | cut -c1-4`
            YMD=`echo "$CYCLE" | cut -c1-8`
            HHHH=`echo "$CYCLE" | cut -c9-10`"00"

            # Parse only these basins
            if [ "${BASIN,,}" = "al" ]; then
                BASIN2="l"
                echo "MSG: ATCF basin --> North Atlantic"
            elif [ "${BASIN,,}" = "ep" ]; then
                BASIN2="e"
                echo "MSG: ATCF basin --> eastern North Pacific"
            elif [ "${BASIN,,}" = "cp" ]; then
                BASIN2="c"
                echo "MSG: ATCF basin --> central North Pacific"
            else
                echo "WARNING: ATCF basin not recognized. Skipping."
                continue
            fi

            # Define the Storm ID (SID)
            SID="${SNUM}${BASIN2^^}"

            # Get the TC name. This is for the Parsed ATCF file name
            TCNAME=""

            # First, try to get the TC name from the CARQ entry in the A-Deck
            if [ -z "$TCNAME" ]; then
                TCNAME=`awk -v CYCLE="${CYCLE}" -F ', ' '$3==CYCLE && $5=="CARQ"' ${ADECK} | head -1 | cut -d "," -f28 | sed -e 's/^[[:space:]]*//'`
            fi

            # Second, try to get the TC name from the Best Track entry in the B-Deck
            if [ -z "$TCNAME" ]; then
                # Check that the B-Deck is available
                BDECK=${BDECKDIR}b${BASIN}${SID}${YEAR}.dat
                if [ -f ${BDECK} ]; then
                    echo "MSG: B-Deck file found --> ${BDECK}"
                    TCNAME=`awk -v CYCLE="${CYCLE}" '$3==CYCLE' ${BDECK} | head -1 | cut -d "," -f28 | sed -e 's/^[[:space:]]*//'`
                    #TCNAME=`grep "${CYCLE}" ${BDECK} | grep -v " 50, NEQ," | grep -v " 64, NEQ," | cut -d "," -f28 | sed -e 's/^[[:space:]]*//' | tr '[:upper:]' '[:lower:]'`
                else
                    echo "WARNING: B-Deck not found --> ${BDECK}"
                fi
            fi

            # Third, try to get the TC name from the TCVItals entry
            if [ -z "$TCNAME" ]; then
                SYNDAT="${TCVDIR}syndat_tcvitals.${YEAR}"
                if [ -f ${SYNDAT} ]; then
                    echo "MSG: TCVitals found --> ${SYNDAT}"
                    TCNAME=`awk -v YMD="${YMD}" -v HHHH="${HHHH}" -v SID="${SID}" -F ' ' '$4==YMD && $5==HHHH && $2==SID' ${SYNDAT} | head -1 | awk '{print $3}' | sed -e 's/^[[:space:]]*//'`
                else
                    echo "WARNING: TCVitals not found --> ${SYNDAT}"
                fi

            fi

            # If TCNAME still is not set, set it to something generic like "NONAME"
            if [ -z "$TCNAME" ]; then
                echo "WARNING: TC Name not found for ${CYCLE}"
                TCNAME="NONAME"
            fi

            echo "MSG: TCNAME=$TCNAME"

            # Copy this entry to the new parsed ATCF
            OFILE="${ODIR}${TCNAME,,}${SID,,}.${CYCLE}.trak.${MODEL,,}.atcfunix"
            if [ -f ${OFILE} ]; then
                awk -v MODEL="${MODEL}" -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -F ', ' '$5==MODEL && $2==SNUM && $3==CYCLE' ${ADECK} | sort -u | sort -k3,3 -k5,5 -k6,6n > ${ODIR}TMP.atcfunix
                #grep "${MODEL}" ${ADECK} | awk -F ', ' '$2=="${SNUM}"' | awk -F ', ' '$3=="${CYCLE}"' | sort -u | sort -k3,3 -k5,5 -k6,6n > ${ODIR}TMP.atcfunix
                if diff -q "${OFILE}" "${ODIR}TMP.atcfunix" ; then
                    echo "MSG: Parsed A-Deck has not changed --> ${OFILE}"
                    rm -f ${ODIR}TMP.atcfunix
                else
                    echo "MSG: Parsed A-Deck has changed. Copying new version --> ${OFILE}"
                    mv ${ODIR}TMP.atcfunix ${OFILE}
                fi
            else
                awk -v MODEL="${MODEL}" -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -F ', ' '$5==MODEL && $2==SNUM && $3==CYCLE' ${ADECK} | sort -u | sort -k3,3 -k5,5 -k6,6n > ${OFILE}
                #grep "${MODEL}" ${ADECK} | awk -F ', ' '$2=="${SNUM}"' | awk -F ', ' '$3=="${CYCLE}"' | sort -u | sort -k3,3 -k5,5 -k6,6n > ${OFILE}
                echo "MSG: Parsed A-Deck does not exist. Writing new file --> ${OFILE}"
            fi

        done
    done
done

echo "MSG: find_oldnew_sid.sh completed at `date`"
