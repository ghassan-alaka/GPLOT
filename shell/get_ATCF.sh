#!/bin/sh
##SBATCH --account=aoml-hafs1
##SBATCH --nodes=1
#SBATCH --time=00:10:00
#SBATCH --partition=service
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
##SBATCH --output=/lfs4/HFIP/hur-aoml/Ghassan.Alaka/GPOUT/log/deliver_atcf.log
##SBATCH --error=/lfs4/HFIP/hur-aoml/Ghassan.Alaka/GPOUT/log/deliver_atcf.log
#SBATCH --job-name="retrieve_nhc_atcf"
##SBATCH --mem=16G

# This script grabs decks from the NHC ATCF
# Options include:
#     DECK_TYPE:  realtime --> Download all Decks from the real-time directories.
#                              Includes subdirectories "aid_public" for A-Decks
#                              and "btk" for B-Decks.
#                 archive  --> Download all Decks from the archive directories.
#                              Must set "DECK_YR" to point to the correct ATCF
#                              subdirectory.
#                 archplus --> Download all Decks from the archive directories.
#                              Also, download and combine invest Decks. Must set
#                              "DECK_YR" to point to the correct ATCF subdir.
#     ADECK_ON:   0        --> Do NOT download A-Decks.
#                 1        --> Download A-Decks. Subdirectory is "adeck/NHC".
#     BDECK_ON:   0        --> Do NOT download B-Decks.
#                 1        --> Download B-Decks. Subdirectory is "bdeck".
#     DIR         /PATH/ABOVE/DECK/DIRECTORIES
#     BASIN       all      --> Download Decks from all basins.
#                 al/ep    --> Download Decks from a particular basin. Use a two-
#                              character basin ID.
#     SNUM        all      --> Download Decks for all storm numbers.
#                 01/02    --> Download Decks for a particular storm number. Use a
#                              two-digit identification number.
#     YYYY        all      --> Download Decks for all years.
#                 2020     --> Download Decks for a particular year.
#
# Example invocations:  ./get_ATCF.sh realtime 1 1 /lfs1/projects/hur-aoml/Ghassan.Alaka/ al 01 2020
#                       ./get_ATCF.sh archive 1 1 /lfs1/projects/hur-aoml/Ghassan.Alaka/ all all 2020

echo "MSG: get_ATCF.sh started at `date`."

# 1. Get input arguments
DECK_TYPE="$1"
ADECK_ON="$2"
BDECK_ON="$3"
ODIR="$4"
BASIN="$5"
SNUM="$6"
YYYY="$7"

if [ "$DECK_TYPE" != "realtime" ] && [ "$DECK_TYPE" != "archive" ] && \
   [ "$DECK_TYPE" != "archplus" ] && [ "$DECK_TYPE" != "archinvest" ]; then
    echo "ERROR: DECK_TYPE = ${DECK_TYPE}"
    echo "ERROR: DECK_TYPE must be 'realtime', 'archive', 'archplus', or archinvest."
    exit
elif [ "$DECK_TYPE" = "realtime" ]; then
    echo "MSG: Downloading real-time Decks from NHC ATCF."
    ARCHIVE="NO"
elif [ "$DECK_TYPE" == "archive" ] || [ "$DECK_TYPE" == "archplus" ] || \
     [ "$DECK_TYPE" == "archinvest" ]; then
    echo "MSG: Downloading archived Decks from NHC ATCF."
    ARCHIVE="YES"
    if [ -z "$YYYY" ]; then
        echo "ERROR: DECK_YR must be specified to download archived Decks."
        exit 1
    fi
fi

if [ "$BASIN" == "all" ] || [ -z "$BASIN" ]; then
    BASIN="[a-z][a-z]"
fi
if [ "$SNUM" == "all" ] || [ -z "$SNUM" ]; then
    SNUM="[0-9][0-9]"
fi
if [ "$YYYY" == "all" ] || [ -z "$YYYY" ]; then
    YYYY="[0-9][0-9][0-9][0-9]"
fi


# 2. Get A-Decks
if [ "$ADECK_ON" == "1" ]; then
    echo "MSG: Downloading A-Decks."

    # Define, Create, & Change directories
    ADIR="${ODIR}/adeck"
    NHCDIR="${ODIR}/adeck/NHC"
    WORKDIR="${ODIR}/adeck/TMP.$(date +%N)"
    mkdir -p ${NHCDIR}
    mkdir -p ${WORKDIR}
    cd ${ADIR}
    if [ "${ARCHIVE}" == "NO" ]; then
        FTPDIR="ftp.nhc.noaa.gov/atcf/aid_public"
    elif [ "${ARCHIVE}" == "YES" ]; then
        FTPDIR="ftp.nhc.noaa.gov/atcf/archive/${YYYY}"
    fi

    # Download A-Decks from the standard FTP location
    wget -r -l1 -A "a${BASIN,,}${SNUM}${YYYY}.dat.gz" -T 5 -N ftp://${FTPDIR}/ #>/dev/null 2>&1
    cp -p ${FTPDIR}/a${BASIN,,}${SNUM}${YYYY}.dat.gz ${WORKDIR}/.
    gunzip -f ${WORKDIR}/a${BASIN,,}${SNUM}${YYYY}.dat.gz #>/dev/null 2>&1

    # Loop over all A-Decks. Check if the file exists and if it as been modified
    # to only copy new updates. Special consideration is given to invests during
    # real-time downloads
    ALL_DECK=( `find ${WORKDIR}/. -name "a${BASIN,,}${SNUM}${YYYY}.dat" -printf "%f\n"` )
    for D in "${ALL_DECK[@]}"; do
        # Special consideration for invests during real-time downloads.
        INVEST="NO"
        if [ "$(echo "${D}" | cut -c4)" == "9" ] && [ "${ARCHIVE}" == "NO" ]; then
            INVEST="YES"
        fi

        # Decide if/how to copy the file to NHCDIR
        if [ -f ${NHCDIR}/${D} ]; then
            DIFF=$(diff ${NHCDIR}/${D} ${WORKDIR}/${D})
            if [ "${DIFF}" != "" ] &&[ "${INVEST}" == "YES" ]; then
                cat ${NHCDIR}/${D} ${WORKDIR}/${D} | sort -u | sort -k3,3 -k5,5 -k6,6n > ${WORKDIR}/TMP.${D}
                DIFF=$(diff ${NHCDIR}/${D} ${WORKDIR}/TMP.${D})
                if [ "${DIFF}" != "" ]; then
                    mv ${WORKDIR}/TMP.${D} ${NHCDIR}/${D}
                fi
            elif [ "${DIFF}" != "" ] && [ "${INVEST}" == "NO" ]; then
                cp -p ${WORKDIR}/${D} ${NHCDIR}/.
            fi
        else
            cp -p ${WORKDIR}/${D} ${NHCDIR}/.
        fi

    done

    # Download A-Decks from the invest FTP location, if necessary
    if [ "$DECK_TYPE" == "archplus" ] || [ "$DECK_TYPE" == "archinvest" ]; then

        # Define the FTP Directory
        FTPDIR="ftp.nhc.noaa.gov/atcf/archive/${YYYY}/invests"

        # Download A-Decks from the invest FTP location
        wget -r -l1 -A "a${BASIN,,}${SNUM}${YYYY}.dat.gz" -T 5 -N ftp://${FTPDIR}/ #>/dev/null 2>&1
        cp -p ${FTPDIR}/a${BASIN,,}${SNUM}${YYYY}.dat.gz ${WORKDIR}/.
        gunzip -f ${WORKDIR}/a${BASIN,,}${SNUM}${YYYY}.dat.gz

        # Loop over all A-Decks. Check if the file exists and if it as been modified
        # to only copy new updates.
        ALL_DECK=( `find ${WORKDIR}/ -name "a${BASIN,,}A[0-9]${YYYY}.dat" -printf "%f\n"` )
        for D in "${ALL_DECK[@]}"; do

            # The archived invest A-Decks Are labeled A-Z, so if there were 5 instances
            # of invest 90L, they would be aalA0, aalB0, aalC0, aalD0, and aalE0. This code
            # want to combine those into aal90.
            SNUM2="9`echo "${D}" | cut -c5`"
            SDIG2="`echo "${D}" | cut -c5`"
            FIN="a`echo "${D}" | cut -c2-3`[A-Z]`echo "${D}" | cut -c5-`"
            FOUT="a`echo "${D}" | cut -c2-3`9`echo "${D}" | cut -c5-`"
            cat ${WORKDIR}/${FIN} | sort -u | sort -k3,3 -k5,5 -k6,6n > ${WORKDIR}/${FOUT}
            sed -i 's/, [A-Z]'"${SDIG2}"',/, '"${SNUM2}"',/g' ${WORKDIR}/${FOUT}

            # Decide if/how to copy the file to NHCDIR
            if [ -f ${NHCDIR}/${FOUT} ]; then
                DIFF=$(diff ${NHCDIR}/${FOUT} ${WORKDIR}/${FOUT})
                if [ "${DIFF}" != "" ]; then
                    cp -p ${WORKDIR}/${FOUT} ${NHCDIR}/.
                fi
            else
                cp -p ${WORKDIR}/${FOUT} ${NHCDIR}/.
            fi
        done
    fi

    # Remove the temporary directory
    rm -rf ${WORKDIR}
fi


# 3. Get B-Decks
if [ "$BDECK_ON" == "1" ]; then
    echo "MSG: Downloading B-Decks."

    # Define, Create, & Change directories
    BDIR="${ODIR}/bdeck"
    WORKDIR="${ODIR}/bdeck/TMP.$(date +%N)"
    mkdir -p ${WORKDIR}
    cd ${BDIR}

    # Go here for real-time --> B-Decks are not compressed
    if [ "${ARCHIVE}" == "NO" ]; then
        FTPDIR="ftp.nhc.noaa.gov/atcf/btk"
        FIN="b${BASIN,,}${SNUM}${YYYY}.dat"
    elif [ "${ARCHIVE}" == "YES" ]; then
        FTPDIR="ftp.nhc.noaa.gov/atcf/archive/${YYYY}"
        FIN="b${BASIN,,}${SNUM}${YYYY}.dat.gz"
    fi

    # Download B-Decks
    wget -r -l1 -A "${FIN}" -T 5 -N ftp://${FTPDIR}/ #>/dev/null 2>&1
    cp -p ${FTPDIR}/${FIN} ${WORKDIR}/. 
    if [ "${ARCHIVE}" == "YES" ]; then
        gunzip -f ${WORKDIR}/${FIN} #>/dev/null 2>&1
    fi

    # Loop over all B-Decks. Check if the file exists and if it as been modified
    # to only copy new updates. Special consideration is given to invests during
    # real-time downloads
    ALL_DECK=( `find ${WORKDIR}/. -name "b${BASIN,,}${SNUM}${YYYY}.dat" -printf "%f\n"` )
    for D in "${ALL_DECK[@]}"; do

        # Special consideration for invests during real-time downloads.
        INVEST="NO"
        if [ "$(echo "${D}" | cut -c4)" == "9" ] && [ "${ARCHIVE}" == "NO" ]; then
            INVEST="YES"
        fi

        # Decide if/how to copy the file to NHCDIR
        if [ -f ${BDIR}/${D} ]; then
            DIFF=$(diff ${BDIR}/${D} ${WORKDIR}/${D})
            if [ "${DIFF}" != "" ] && [ "${INVEST}" == "YES" ]; then
                #cat ${BDIR}/${D} ${WORKDIR}/${D} | sort -t, -k3,3  | sort -k3,3 -k5,5 -k6,6n > ${WORKDIR}/TMP.${D}
                cat ${BDIR}/${D} ${WORKDIR}/${D} | sort -t, -k3,3 -k12,12n -k13,13 | awk -F, '{ $12 = ($12 == 0 ? "  34" : $12) } 1' OFS=, | sort -r -t, -k3,3 -k12,12n -k13,13 | sort -k3,3 -k12,12n -u -t, > ${WORKDIR}/TMP.${D}
                DIFF=$(diff ${BDIR}/${D} ${WORKDIR}/TMP.${D})
                if [ "${DIFF}" != "" ]; then
                    mv ${WORKDIR}/TMP.${D} ${BDIR}/${D}
                fi
            elif [ "${DIFF}" != "" ] && [ "${INVEST}" == "NO" ]; then
                cp -p ${WORKDIR}/${D} ${BDIR}/.
            fi
        else
            cp -p ${WORKDIR}/${D} ${BDIR}/.
        fi

    done


    # Download A-Decks from the invest FTP location, if necessary
    if [ "$DECK_TYPE" == "archplus" ] || [ "$DECK_TYPE" == "archinvest" ]; then

        # Define the FTP Directory
        FTPDIR="ftp.nhc.noaa.gov/atcf/archive/${YYYY}/invests"

        # Download B-Decks from the invest FTP location
        wget -r -l1 -A "b${BASIN,,}${SNUM}${YYYY}.dat.gz" -T 5 -N ftp://${FTPDIR}/ #>/dev/null 2>&1
        cp -p ${FTPDIR}/b${BASIN,,}${SNUM}${YYYY}.dat.gz ${WORKDIR}/.
        gunzip -f ${WORKDIR}/b${BASIN,,}${SNUM}${YYYY}.dat.gz

        # Loop over all B-Decks. Check if the file exists and if it as been modified
        # to only copy new updates.
        ALL_DECK=( `find ${WORKDIR}/ -name "b${BASIN,,}A[0-9]${YYYY}.dat" -printf "%f\n"` )
        for D in "${ALL_DECK[@]}"; do

            # The archived invest B-Decks Are labeled A-Z, so if there were 5 instances
            # of invest 90L, they would be balA0, balB0, balC0, balD0, and balE0. This code
            # want to combine those into bal90.
            SNUM2="9`echo "${D}" | cut -c5`"
            SDIG2="`echo "${D}" | cut -c5`"
            FIN="b`echo "${D}" | cut -c2-3`[A-Z]`echo "${D}" | cut -c5-`"
            FOUT="b`echo "${D}" | cut -c2-3`9`echo "${D}" | cut -c5-`"
            cat ${WORKDIR}/${FIN} | sort -u | sort -k3,3 -k5,5 -k6,6n > ${WORKDIR}/${FOUT}
            sed -i 's/, [A-Z]'"${SDIG2}"',/, '"${SNUM2}"',/g' ${WORKDIR}/${FOUT}

            # Decide if/how to copy the file to NHCDIR
            if [ -f ${BDIR}/${FOUT} ]; then
                DIFF=$(diff ${BDIR}/${FOUT} ${WORKDIR}/${FOUT})
                if [ "${DIFF}" != "" ]; then
                    cp -p ${WORKDIR}/${FOUT} ${BDIR}/.
                fi
            else
                cp -p ${WORKDIR}/${FOUT} ${BDIR}/.
            fi
        done
    fi

    # Remove the working directory
    rm -rf ${WORKDIR}

fi

echo "MSG: get_ATCF.sh completed at `date`."

