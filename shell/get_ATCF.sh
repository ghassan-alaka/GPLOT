#!/bin/sh

# This script grabs decks from the NHC ATCF
# Options include:
#     DECK_TYPE:  realtime --> Download all Decks from the real-time directories.
#                              Includes subdirectories "aid_public" for A-Decks
#                              and "btk" for B-Decks.
#                 archive  --> Download all Decks from the archive directories.
#                              Must set "DECK_YR" to point to the correct ATCF
#                              subdirectory.
#     ADECK_ON:   0        --> Do NOT download A-Decks.
#                 1        --> Download A-Decks. Subdirectory is "adeck/NHC".
#     BDECK_ON:   0        --> Do NOT download B-Decks.
#                 1        --> Download B-Decks. Subdirectory is "bdeck".
#     DIR         /PATH/ABOVE/DECK/DIRECTORIES
#     DECK_ID     al012018 --> Optional input to download only specific Decks.
#                              Format is BBSSYYYY, where BB is the two-character
#                              basin, SS is the two-digit Storm ID, and YYYY is the
#                              four-digit year. Can set to "ALL"
#     DECK_YR     2018     --> Optional input to download only archived Decks from
#                              specified years. Wildcards may be used but it is
#                              advised to download only 1 year at a time.
#
# Example invocations:  ./get_ATCF.sh realtime 1 1 /lfs1/projects/hur-aoml/Ghassan.Alaka/ al012018
#                       ./get_ATCF.sh archive 1 1 /lfs1/projects/hur-aoml/Ghassan.Alaka/ ALL 2018

# 1. Get input arguments
DECK_TYPE="$1"
ADECK_ON="$2"
BDECK_ON="$3"
ODIR="$4"
DECK_ID="$5"
DECK_YR="$6"

if [ "$DECK_TYPE" != "realtime" ] && [ "$DECK_TYPE" != "archive" ]; then
    echo "ERROR: DECK_TYPE = ${DECK_TYPE}"
    echo "ERROR: DECK_TYPE must be 'realtime' or 'archive'."
    exit
elif [ "$DECK_TYPE" = "realtime" ]; then
    echo "MSG: Downloading real-time Decks from NHC ATCF."
elif [ "$DECK_TYPE" = "archive" ]; then
    echo "MSG: Downloading archived Decks from NHC ATCF."
    if [ -z "$DECK_YR" ]; then
        echo "ERROR: DECK_YR must be specified to download archived Decks."
        exit
    fi
fi

if [ "$DECK_ID" == "ALL" ]; then
    DECK_ID="*"
fi

# 2. Get A-Decks
if [ "$ADECK_ON" == "1" ]; then
    echo "MSG: Downloading A-Decks."
    TMP_DIR="TMP.$(date +%N)"
    mkdir -p $ODIR/adeck/NHC
    mkdir -p $ODIR/adeck/$TMP_DIR
    cd $ODIR/adeck
    if [ "$DECK_TYPE" == "realtime" ]; then
        FTP_DIR="ftp.nhc.noaa.gov/atcf/aid_public/"
    elif [ "$DECK_TYPE" == "archive" ]; then
        FTP_DIR="ftp.nhc.noaa.gov/atcf/archive/${DECK_YR}/"
    fi
    wget -r -l1 -A "a*${DECK_ID}*dat.gz" -T 5 -N ftp://${FTP_DIR} #>/dev/null 2>&1
    cp -p ${FTP_DIR}a*${DECK_ID}*dat.gz $TMP_DIR/.
    gunzip -f $TMP_DIR/a*${DECK_ID}*dat.gz #>/dev/null 2>&1
    cp -p -u $TMP_DIR/a*${DECK_ID}*dat NHC/. #>/dev/null 2>&1
    rm -rf $TMP_DIR
fi


# 3. Get B-Decks
if [ "$BDECK_ON" == "1" ]; then
    echo "MSG: Downloading B-Decks."
    TMP_DIR="TMP.$(date +%N)"
    mkdir -p $ODIR/bdeck/$TMP_DIR
    cd $ODIR/bdeck
    if [ "$DECK_TYPE" == "realtime" ]; then
        FTP_DIR="ftp.nhc.noaa.gov/atcf/btk/"
        wget -r -l1 -A "b*${DECK_ID}*dat" -T 5 -N ftp://${FTP_DIR} #>/dev/null 2>&1
        cp -p -u ${FTP_DIR}b*${DECK_ID}*dat . #>/dev/null 2>&1
    elif [ "$DECK_TYPE" == "archive" ]; then
        FTP_DIR="ftp.nhc.noaa.gov/atcf/archive/${DECK_YR}/"
        wget -r -l1 -A "b*${DECK_ID}*dat.gz" -T 5 -N ftp://${FTP_DIR} #>/dev/null 2>&1
        cp -p ${FTP_DIR}b*${DECK_ID}*dat.gz $TMP_DIR/.
        gunzip -f $TMP_DIR/b*${DECK_ID}*dat.gz #>/dev/null 2>&1
        cp -p -u $TMP_DIR/b*${DECK_ID}*dat . #>/dev/null 2>&1
    fi
    rm -rf $TMP_DIR
fi

