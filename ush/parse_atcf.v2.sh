#!/bin/sh --login
#
# Invocation: ./parse_atcf.sh AVNO /lfs1/projects/hur-aoml/Ghassan.Alaka/noscrub/GFS_Forecast/ /lfs1/projects/hur-aoml/Ghassan.Alaka/adeck/NHC/ /lfs1/projects/hur-aoml/Ghassan.Alaka/bdeck/ /lfs3/projects/hwrf-data/hwrf-input/SYNDAT-PLUS/ 1
#       ./parse_atcf.sh HAFS /lfs1/projects/hur-aoml/Ghassan.Alaka/noscrub/HAFSV0.B_Forecast/ /lfs3/projects/hur-aoml/rthr-aoml/hafstmp/HAFS_jet/com/ /lfs1/projects/hur-aoml/Ghassan.Alaka/bdeck/ /lfs3/projects/hwrf-data/hwrf-input/SYNDAT-PLUS/ 2
#       ./parse_atcf.sh HAFS /lfs1/projects/hur-aoml/Ghassan.Alaka/noscrub/HAFSV0.B_Forecast/ /lfs3/projects/hur-aoml/rthr-aoml/noscrub/hafstrak/HAFS_jet/ /lfs1/projects/hur-aoml/Ghassan.Alaka/bdeck/ /lfs3/projects/hwrf-data/hwrf-input/SYNDAT-PLUS/ 2
#
# TYPE: 1 --> Parse from NHC A-Deck (Time-Dependent; Good to exclude older ATCFs during real-time)
# TYPE: 2 --> Parse from combined A-Deck (e.g., HAFS) (Time-Dependent; Good to exclude older ATCFs during real-time)
# TYPE: 3 --> Parse from NHC A-Deck (Non-Time-Dependent; Good to include all ATCFs for research mode)
# TYPE: 4 --> Parse from combined A-Deck (e.g., HAFS) (Non-Time-Dependent; Good to include all ATCFs for research mode)

#set -aeu

echo "MSG: parse_atcf.sh started at `date`"

# Parse input arguments
while test $# -gt 0; do

  case "$1" in
    -h|--help)
      echo "Create A-Deck -- parse & create combined A-Deck files"
      echo " "
      echo "create_adeck.sh [options]"
      echo " "
      echo "options:"
      echo "-h, --help        show help"
      echo "-m, --model       model ID to be parsed (ABCD)"
      echo "-o, --output       output data directory"
      echo "-a, --adeck       a-deck directory"
      echo "-b, --bdeck       b-deck directory"
      echo "-t, --tcvit       TCVitals directory"
      echo "-n, --name        file name search string"
      echo "-s, --sidfile      SID old/new file"
      echo "-mo, --omodel      model ID for output"
      echo "-no, --oname       output file name string"
      echo "--mmax          maximum age in minutes"
      exit 0
      ;;
    -m|--model)
      shift
      if test $# -gt 0; then
        MODEL="${1}"
      fi
      shift
      ;;
    -o|--output)
      shift
      if test $# -gt 0; then
        OUTDIR="${1}"
        if [ ! -d "${OUTDIR}" ]; then
          echo "MSG: Creating output directory because it does not exist --> ${OUTDIR}"
          mkdir -p ${OUTDIR}
        fi
      fi
      shift
      ;;
    -a|--adeck)
      shift
      if test $# -gt 0; then
        ADECKDIR="${1}"
        if [ ! -d "${ADECKDIR}" ]; then
          echo "ERROR: A-Deck directory does not exist."
          exit 1
        fi
      fi
      shift
      ;;
    -b|--bdeck)
      shift
      if test $# -gt 0; then
        BDECKDIR="${1}"
        if [ ! -d "${BDECKDIR}" ]; then
          echo "ERROR: B-Deck directory does not exist."
          exit 1
        fi
      fi
      shift
      ;;
    -t|--tcvit)
      shift
      if test $# -gt 0; then
        TCVDIR="${1}"
        if [ ! -d "${TCVDIR}" ]; then
          echo "ERROR: TCVitals directory does not exist."
          exit 1
        fi
      fi
      shift
      ;;
    -n|--name)
      shift
      if test $# -gt 0; then
        TAG="${1}"
      fi
      shift
      ;;
    -no|--oname)
      shift
      if test $# -gt 0; then
        OTAG="${1}"
      fi
      shift
      ;;
    -s|--sidfile)
      shift
      if test $# -gt 0; then
        SID_FILE="${1}"
      else
        echo "WARNING: SID Old/New file not specified. Defaulting to '${GPLOT_DIR}/tbl/SIDs_Old_New.dat'."
        SID_FILE="${GPLOT_DIR}/tbl/SIDs_Old_New.dat"
      fi
      shift
      ;;
    --mmax)
      shift
      if test $# -gt 0; then
        MMAX="${1}"
      fi
      shift
      ;;
    -mo|--omodel)
      shift
      if test $# -gt 0; then
        OMODEL="${1}"
        echo "MSG: Output model specified. Will change model ID to ${ODMOEL}"
      fi
      shift
      ;;
    --sidstrict)
      shift
      SIDSTRICT="YES"
      echo "MSG: Strict enforcement of SIDs, i.e., it must be in the file name."
      ;;
    --snum)
      shift
      if test $# -gt 0; then
        ISNUM="${1}"
        echo "MSG: Storm number specified --> ${ISNUM}"
      fi
      shift
      ;;
    --basin)
      shift
      if test $# -gt 0; then
        IBASIN="${1}"
        echo "MSG: Storm basin specified --> ${IBASIN}"
      fi
      shift
      ;;
    *)
      break
      ;;

  esac

done


if [ -z "${MODEL}" ]; then
  echo "ERROR: Model identifier (ABCD) not specified."
  exit 1
fi
if [ -z "${OUTDIR}" ]; then
  echo "ERROR: Output directory not specified."
  exit 1
fi
if [ -z "${ADECKDIR}" ]; then
  echo "ERROR: A-deck directory not specified."
  exit 1
fi
if [ -z "${BDECKDIR}" ]; then
  echo "ERROR: B-deck directory not specified."
  exit 1
fi
if [ -z "${TCVDIR}" ]; then
  echo "ERROR: TCVitals directory not specified."
  exit 1
fi
if [ -z "${TAG}" ]; then
  echo "WARNING: File name tag not specified. Defaulting to '*atcfunix'."
  TAG="*.atcfunix"
fi
if [ -z "${OTAG}" ]; then
  echo "WARNING: Output file name tag not specified. Defaulting to '' for now."
  OTAG=""
fi
if [ -z "${SID_FILE}" ]; then
  echo "WARNING: SID Old/New file not specified. Defaulting to '${GPLOT_DIR}/tbl/SIDs_Old_New.dat'."
  SID_FILE="${GPLOT_DIR}/tbl/SIDs_Old_New.dat"
fi
if [ -z "${SIDSTRICT}" ]; then
  echo "WARNING: Strict enforcement of SID not specified."
  SIDSTRICT="NO"
fi
if [ -z "${ISNUM}" ]; then
  echo "WARNING: Storm number not specified."
  ISNUM="all"
fi
if [ -z "${IBASIN}" ]; then
  echo "WARNING: Storm basin not specified."
  ISBASIN="all"
fi


# Change into the output directory
cd ${ODIR}


# Update the output file tag, if applicable
if [ -z "${OTAG}" ]; then
  if [ -z "${OMODEL}" ]; then
    OTAG="trak.${MODEL,,}.atcfunix"
  else
    OTAG="trak.${OMODEL,,}.atcfunix"
  fi
  echo "MSG: Updating the output file tag --> ${OTAG}"
fi


# Find all A-DECKs
if [ -z "${MMAX}" ]; then
  echo "MSG: Searching for A-Decks --> [find \"${ADECKDIR}\" -name ${TAG} -type f]"
  ALL_ADECKS=( `find "${ADECKDIR}" -name ${TAG} -type f` )
else
  echo "MSG: Searching for A-Decks --> [find \"${ADECKDIR}\" -name ${TAG} -type f -mmin ${MMAX}]"
  ALL_ADECKS=( `find "${ADECKDIR}" -name ${TAG} -type f -mmin ${MMAX}` )
fi

# Now, loop over the available A-DECKs
for ADECK in ${ALL_ADECKS[@]}; do

  # Get some information from the A-Deck file name
  echo ""
  echo "***********************"
  echo "MSG: Working on this A-Deck --> ${ADECK}"
  echo "***********************"
  #ALL_SNUM=( `awk -v MODEL="${MODEL}" -F ', ' '$5==MODEL && $2!="00"' ${ADECK} | cut -d "," -f2 | sort -u | sed -e 's/^[[:space:]]*//'` )
  ALL_SNUM=( `cat ${ADECK} | tr -d "[:blank:]" | awk -v MODEL="${MODEL^^}" -F, '$5==MODEL && $2!="00"' | cut -d "," -f2 | sort -u` )

  if [ -z "${ALL_SNUM[*]}" ]; then
    echo "WARNING: No storm numbers were found."
    continue
  fi

  for SNUM in ${ALL_SNUM[@]}; do
    if [ "$SNUM" == "00" ]; then
      echo "WARNING: The storm number can not be zero (${SNUM})."
      continue
    elif (( $(echo "${SNUM} > 50" | bc) )) && (( $(echo "${SNUM} < 90" | bc) )); then
      echo "WARNING: The storm number is out of range (50<${SNUM}<90)."
      continue
    fi
    

    # Get a list of available cycles
    ALL_CYCLES=( `cat ${ADECK} | tr -d "[:blank:]" | awk -v MODEL="${MODEL^^}" -v SNUM="${SNUM}" -F, '$5==MODEL && $2==SNUM' | cut -d "," -f3 | sort -u` )

    if [ -z "${ALL_CYCLES[*]}" ]; then
      echo "WARNING: No cycles were found for SNUM=${SNUM}."
      continue
    fi

    for CYCLE in ${ALL_CYCLES[@]}; do

      # Get basin and year information. This will help build the output ATCF
      ALL_BASINS=( `cat ${ADECK} | tr -d "[:blank:]" | awk -v MODEL="${MODEL^^}" -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -F, '$5==MODEL && $2==SNUM && $3==CYCLE' | cut -d "," -f1 | sort -u` )
      YEAR="`echo "$CYCLE" | cut -c1-4`"
      YMD="`echo "$CYCLE" | cut -c1-8`"
      HH="`echo "$CYCLE" | cut -c9-10`"
      YMDH="${YMD}${HH}"
      HHHH="${HH}00"

      if [ -z "${ALL_BASINS[*]}" ]; then
        echo "WARNING: No basins were found for SNUM=${SNUM} and CYCLE=${CYCLE}."
        continue
      fi

      for BASIN in ${ALL_BASINS[@]}; do
        echo ""
        echo "MSG: Found this A-Deck --------> ${ADECK}"
        echo "MSG: Found this Storm Number --> ${SNUM}"
        echo "MSG: Found this Cycle ---------> ${CYCLE}"
        echo "MSG: Found this Basin ---------> ${BASIN}"


        # Check that the B-Deck is available
        BDECK=${BDECKDIR}/b${BASIN,,}${SNUM}${YEAR}.dat
        if [ -f ${BDECK} ]; then
          echo "MSG: B-Deck file found --> ${BDECK}"
        else
          echo "WARNING: B-Deck file not found --> ${BDECK}"
          BDECK=
        fi


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
          BASIN3="AL"
          echo "MSG: ATCF basin --> North Atlantic Ocean"
        elif [ "${BASIN,,}" = "ep" ]; then
          BASIN2="e"
          BASIN3="EP"
          echo "MSG: ATCF basin --> eastern North Pacific Ocean"
        elif [ "${BASIN,,}" = "cp" ]; then
          BASIN2="c"
          BASIN3="CP"
          echo "MSG: ATCF basin --> central North Pacific Ocean"
        elif [ "${BASIN,,}" = "wp" ]; then
          BASIN2="w"
          BASIN3="WP"
          echo "MSG: ATCF basin --> western North Pacific Ocean"
        elif [ "${BASIN,,}" = "io" ]; then
          if [ "${LON}" -ge "800" ]; then
            echo "MSG: ATCF basin --> North Indian Ocean (Bay of Bengal)"
            BASIN2="b"
          else
            echo "MSG: ATCF basin --> Arabian Sea (North Indian Ocean)"
            BASIN2="a"
          fi
          BASIN="IO"
          BASIN3="IO"
        elif [ "${BASIN,,}" = "aa" ]; then
          BASIN="IO"
          BASIN2="a"
          BASIN3="AA"
          echo "MSG: ATCF basin --> North Indian Ocean (Arabian Sea)"
        elif [ "${BASIN,,}" = "bb" ]; then
          BASIN="IO"
          BASIN2="b"
          BASIN3="BB"
          echo "MSG: ATCF basin --> North Indian Ocean (Bay of Bengal)"
        elif [ "${BASIN,,}" = "sh" ]; then
          BASIN="SH"
          BASIN3="SH"
          if [ "${LON}" -ge "1350" ] || [ "${LON}" -le "-1200" ] ; then
            echo "MSG: ATCF basin --> Southern Hemisphere (South Pacific Ocean)"
            BASIN2="p"
          else
            echo "MSG: ATCF basin --> Southern Hemisphere (South Indian Ocean)"
            BASIN2="s"
          fi
        elif [ "${BASIN,,}" = "pp" ]; then
          BASIN="SH"
          BASIN2="p"
          BASIN3="PP"
          echo "MSG: ATCF basin --> South Hemisphere (South Pacific Ocean)"
        elif [ "${BASIN,,}" = "ss" ]; then
          BASIN="SH"
          BASIN2="s"
          BASIN3="SS"
          echo "MSG: ATCF basin --> South Hemisphere (South Indian Ocean)"
        elif [ "${BASIN,,}" = "ls" ]; then
          BASIN2="q"
          BASIN3="LS"
          echo "MSG: ATCF basin --> South Atlantic Ocean"
        else
          echo "WARNING: ATCF basin not recognized. Skipping."
          continue
        fi


        # Check that the B-Deck is available
        if [ -z "${BDECK}" ]; then
          BDECK=${BDECKDIR}/b${BASIN,,}${SNUM}${YEAR}.dat
          if [ -f ${BDECK} ]; then
            echo "MSG: B-Deck file found --> ${BDECK}"
          else
            echo "WARNING: B-Deck file not found --> ${BDECK}"
            BDECK=
          fi
        fi


        # Define the Storm ID (SID)
        SID="${SNUM}${BASIN2^^}"
        SID_TC=
        SID_INVEST=
        if (( $(echo "${SNUM} < 51" | bc) )); then
          SID_TC="${SID}"
          SID_INVEST=""
        elif (( $(echo "${SNUM} > 89" | bc) )); then
          SID_TC=""
          SID_INVEST="${SID}"
        fi


        # Check if the SID matches strict standards
        if [ "${SIDSTRICT}" == "YES" ] && [[ $(echo ${ADECK##*/} | awk '{print tolower($0)}' | cut -d'.' -f1) != *"${SID,,}"* ]]; then
          echo "WARNING: I will not process this ATCF because the storm ID can't be found in the file name."
          continue
        fi
        

        # Check that TCVitals is available.
        # If so, find matching Invest SIDs and TC SIDs that refer to the same storm
        # for more accurate ATCF file names.
        SYNDAT="${TCVDIR}/syndat_tcvitals.${YEAR}"
        if [ -f ${SYNDAT} ]; then
          echo "MSG: TCVitals file found --> ${SYNDAT}"
          THIS_LAT="`awk -v YMD="${YMD}" -v HHHH="${HHHH}" -F ' ' '$4==YMD && $5==HHHH {print $0}' ${SYNDAT} | sort -k2,2 -u | awk -v SID="${SID}" -F ' ' '$2==SID {print $6}'`"
          if [ "$(echo "${THIS_LAT}" | cut -c4)" == "S" ]; then
            THIS_LAT="-$(echo $(echo "${THIS_LAT}" | cut -c1-2).$(echo "${THIS_LAT}" | cut -c3) | bc -l)"
          else
            THIS_LAT="`echo "${THIS_LAT}" | cut -c1-2`.`echo "${THIS_LAT}" | cut -c3`"
          fi
          THIS_LON="`awk -v YMD="${YMD}" -v HHHH="${HHHH}" -F ' ' '$4==YMD && $5==HHHH {print $0}' ${SYNDAT} | sort -k2,2 -u | awk -v SID="${SID}" -F ' ' '$2==SID {print $7}'`"
          if [ "$(echo "${THIS_LON}" | cut -c5)" == "W" ]; then
            THIS_LON="-$(echo "$(echo "${THIS_LON}" | cut -c1-3).$(echo "${THIS_LON}" | cut -c4)" | bc -l)"
          else
            THIS_LON="`echo "${THIS_LON}" | cut -c1-3`.`echo "${THIS_LON}" | cut -c4`"
          fi

          OTHER_LATS=( `awk -v YMD="${YMD}" -v HHHH="${HHHH}" -F ' ' '$4==YMD && $5==HHHH {print $0}' ${SYNDAT} | sort -k2,2 -u | awk -v SID="${SID}" -F ' ' '$2!=SID {print $6}'` )
          OTHER_LONS=( `awk -v YMD="${YMD}" -v HHHH="${HHHH}" -F ' ' '$4==YMD && $5==HHHH {print $0}' ${SYNDAT} | sort -k2,2 -u | awk -v SID="${SID}" -F ' ' '$2!=SID {print $7}'` )
          OTHER_SIDS=( `awk -v YMD="${YMD}" -v HHHH="${HHHH}" -F ' ' '$4==YMD && $5==HHHH {print $0}' ${SYNDAT} | sort -k2,2 -u | awk -v SID="${SID}" -F ' ' '$2!=SID {print $2}'` )

          if [ ! -z "${OTHER_LATS[*]}" ] && [ ! -z "${OTHER_LONS[*]}" ]; then
            i=0
            for OTHER_LAT in ${OTHER_LATS[@]}; do
              if [ "$(echo "${OTHER_LAT}" | cut -c4)" == "S" ]; then
                OTHER_LAT="-$(echo $(echo "${OTHER_LAT}" | cut -c1-2).$(echo "${OTHER_LAT}}" | cut -c3) | bc -l)"
              else
                OTHER_LAT="`echo "${OTHER_LAT}" | cut -c1-2`.`echo "${OTHER_LAT}}" | cut -c3`"
              fi
              if [ "$(echo "${OTHER_LONS[i]}" | cut -c5)" == "W" ]; then
                OTHER_LON="-$(echo $(echo "${OTHER_LONS[i]}" | cut -c1-3).$(echo "${OTHER_LONS[i]}}" | cut -c4) | bc -l)"
              else
                OTHER_LON="`echo "${OTHER_LONS[i]}" | cut -c1-3`.`echo "${OTHER_LONS[i]}}" | cut -c4`"
              fi

              OTHER_SID="${OTHER_SIDS[i]}"
              OTHER_SNUM="$(echo "${OTHER_SID}" | cut -c1-2)"
              
              LATDIFF=`echo "scale=1;${OTHER_LAT} - ${THIS_LAT}" | bc | sed 's/-//'`
              LONDIFF=`echo "scale=1;${OTHER_LON} - ${THIS_LON}" | bc | sed 's/-//'`
              if (( $(echo "$LATDIFF < 2.0" | bc) )) && (( $(echo "$LONDIFF < 2.0" | bc) )); then
                echo "MSG: ${SID} and ${OTHER_SID} are the same storm."
                if (( $(echo "${SNUM} < 51" | bc) )) && (( $(echo "${OTHER_SNUM} > 89" | bc) )); then
                  SID_INVEST="${OTHER_SID}"
                  break
                elif (( $(echo "${SNUM} > 89" | bc) )) && (( $(echo "${OTHER_SNUM} < 51" | bc) )); then
                  SID_TC="${OTHER_SID}"
                  break
                elif (( $(echo "${SNUM} < 51" | bc) )) && (( $(echo "${OTHER_SNUM} < 51" | bc) )); then
                  echo "WARNING: The Storm ID (${SNUM}) and the other Storm ID (${OTHER_SNUM}) are both <= 50."
                  echo "WARNING: Did not find an invest Storm ID."
                  SID_INVEST=
                elif (( $(echo "${SNUM} > 89" | bc) )) && (( $(echo "${OTHER_SNUM} > 89" | bc) )); then
                  echo "WARNING: The Storm ID (${SNUM}) and the other Storm ID (${OTHER_SNUM}) are both >= 90."
                  echo "WARNING: Did not find a TC Storm ID."
                  SID_TC=
                fi
              fi
              ((i++))
            done
          fi
        else
          echo "WARNING: TCVitals file not found --> ${SYNDAT}"
          SYNDAT=
        fi

        # Get the TC name. This is for the Parsed ATCF file name
        TCNAME=""

        # 1) Try to get the TC name from the Best Track entry in the B-Deck
        if [ -z "${TCNAME}" ]; then
          if [ ! -z "${BDECK}" ]; then
            TCNAME="`awk -v CYCLE="${CYCLE}" '$3~CYCLE' ${BDECK} | head -1 | cut -d "," -f28 | awk '{$1=$1};1'`"
          fi
        fi

        # 2) Try to get the TC name from the TCVItals entry
        if [ -z "${TCNAME}" ]; then
          if [ ! -z "${SYNDAT}" ]; then
            TCNAME="`awk -v YMD="${YMD}" -v HHHH="${HHHH}" -v SID="${SID}" -F ' ' '$4==YMD && $5==HHHH && $2==SID' ${SYNDAT} | head -1 | awk '{print $3}' | sed -e 's/^[[:space:]]*//'`"
          fi
        fi

        # 3) Try to get the TC name from the CARQ entry in the A-Deck
        if [ -z "${TCNAME}" ]; then
          TCNAME="`cat ${ADECK} | tr -d "[:blank:]" | awk -v CYCLE="${CYCLE}" -F, '$3==CYCLE && $5=="CARQ"' | head -1 | cut -d "," -f28`"
        fi

        # 4) If TCNAME still is not set, set it to something generic like "INVEST" (for SNUM=90-99) or "STORM"
        if [ -z "${TCNAME}" ]; then
          echo "WARNING: TC Name not found for ${CYCLE}"
          if [ "$(echo ${SNUM} | cut -c1)" == "9" ]; then
            TCNAME="INVEST"
          else
            if [ ! -z "${BDECK}" ]; then
              TCNAME="`awk -v BASIN="${BASIN^^}" -v SNUM="${SNUM}" -F, '$1==BASIN && $2==SNUM' ${BDECK} | tail -1 | cut -d "," -f28 | awk '{$1=$1};1'`"
            fi
            if [ -z "${TCNAME}" ]; then
              TCNAME="STORM"
            fi
          fi
        fi

        # 5) If TCNAME is "INVEST", but Storm Number < 50, try to rename it.
        if [ ! -z "${BDECK}" ] && [ "$(echo ${SNUM} | cut -c1)" != "9" ] && [ "${TCNAME^^}" == "INVEST" ]; then
          TMP="`awk -v BASIN="${BASIN^^}" -v SNUM="${SNUM}" -F, '$1==BASIN && $2==SNUM' ${BDECK} | tail -1 | cut -d "," -f28 | awk '{$1=$1};1'`"
          if [ ! -z "${TMP}" ] && [ "${TMP^^}" != "INVEST" ]; then
            TCNAME="${TMP}"
          fi
        fi

        echo "MSG: TCNAME=${TCNAME}"



        # Copy this entry to the new parsed ATCF
        OFILE="${OUTDIR}/${TCNAME,,}${SID,,}.${CYCLE}.${OTAG}"
        if [ ! -z "${SID_INVEST}" ] && [ "${SID}" != "${SID_INVEST}" ]; then
          OFILE2="${OUTDIR}/invest${SID_INVEST,,}.${CYCLE}.${OTAG}"
        else
          OFILE2=
        fi
        if [ ! -z "${SID_TC}" ] && [ "${SID}" != "${SID_TC}" ]; then
          OFILE3="$( ls ${OUTDIR}/*${SID_TC,,}.${CYCLE}.${OTAG} )"
        else
          OFILE3=
        fi
        if [ ! -z "${OFILE2}" ] && [ -f "${OFILE2}" ]; then
          echo "MSG: ATCF for the Invest Storm ID is not required because a TC Storm ID exists."
          echo "MSG: Removing this ATCF --> ${OFILE2}"
          rm -f ${OFILE2}
        fi
        if [ ! -z "${OFILE3}" ] && [ -f "${OFILE3}" ]; then
          echo "MSG: ATCF for the TC Storm ID exists. No need to produce an ATCF for the Invest Storm ID."
          echo "MSG: Removing this ATCF --> ${OFILE}"
          rm -f ${OFILE}
          #OFILE="${OFILE3}"
          continue
        fi

        if [ -f ${OFILE} ]; then
          TMPFILE="${OUTDIR}/$(date +%N).${TCNAME,,}${SID,,}.${CYCLE}.${OTAG}"
          #TMPFILE2="${OUTDIR}/$(date +%N).${TCNAME,,}${SID,,}.${CYCLE}.${OTAG}"
          #grep "${MODEL}," ${ADECK} | awk -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -F ', ' '$2==SNUM && $3==CYCLE' > ${TMPFILE}
          #grep "${MODEL}," ${ADECK} | awk -v BASIN="${BASIN^^}" -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -F ', ' '$1==BASIN && $2==SNUM && $3==CYCLE' > ${TMPFILE}
          tac ${ADECK} | awk -v BASIN="${BASIN3^^}" -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -v MODEL="${MODEL^^}" -F ',[ \t]*' \
              '$1==BASIN && $2==SNUM && $3==CYCLE && $5==MODEL' | \
              sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > ${TMPFILE}
          if [ ! -z "${OMODEL}" ]; then
            sed -i 's/'"${MODEL}"'/'"${OMODEL}"'/g' ${TMPFILE}
          fi
          #cat ${OFILE} ${TMPFILE} | tac | sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > ${TMPFILE2}
          #rm -f ${TMPFILE}
          DIFF=$(diff ${TMPFILE} ${OFILE})
          if [ "${DIFF}" != "" ]; then
            echo "MSG: Moving new deck to target location"
            echo "MSG: ${TMPFILE} --> ${OFILE}"
            LOCK_FILE="${OFILE}.lock"
            T=0
            while [ -f ${LOCK_FILE} ] && [ "$T" -lt 120 ]; do
              echo "MSG: Output A-Deck is locked. Sleeping 5 seconds..."
              sleep 5
              T=`expr $T + 5`
            done
            if [ $T -ge 120 ]; then
              echo "ERROR: I've been waiting too long for this file to unlock. Something went wrong."
              exit 100
            fi
            echo "MSG: Output A-Deck is unlocked. Locking it."
            lockfile -r-1 -l 180 ${LOCK_FILE}
            mv ${TMPFILE} ${OFILE}
            if [ ! -f ${OFILE} ]; then
              echo "ERROR: The output a-deck doesn't exist. Something went wrong."
              exit 6
            fi
            rm -f ${LOCK_FILE}
            echo "MSG: Output A-Deck processing complete. Unlocking it."
          else
            echo "MSG: No differences, so keeping old deck --> ${OFILE}"
            echo "MSG: Removing this temporary file --> ${TMPFILE}"
            rm -f ${TMPFILE}
          fi
  
        else
          #grep "${MODEL}," ${ADECK} | awk -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -F ', ' '$2==SNUM && $3==CYCLE' | sort -t, -k3,3 -k5,5 -k6,6n -k12,12 | sort -r | sort -k3,3 -k5,5 -k6,6n -k12,12 -u -t, > ${OFILE}
          #grep "${MODEL}," ${ADECK} | awk -v BASIN="${BASIN^^}" -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -F ', ' '$1==BASIN && $2==SNUM && $3==CYCLE' | sort -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > ${OFILE}
          tac ${ADECK} | awk -v BASIN="${BASIN3^^}" -v SNUM="${SNUM}" -v CYCLE="${CYCLE}" -v MODEL="${MODEL^^}" -F ',[ \t]*' \
                  '$1==BASIN && $2==SNUM && $3==CYCLE && $5==MODEL' | \
                  sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > ${OFILE}
          if [ ! -z "${OMODEL}" ]; then
            sed -i 's/'"${MODEL}"'/'"${OMODEL}"'/g' ${OFILE}
          fi
          echo "MSG: Parsed A-Deck does not exist. Writing new file --> ${OFILE}"
        fi
      done
    done
  done
done

echo "MSG: parse_atcf.sh completed at `date`"
