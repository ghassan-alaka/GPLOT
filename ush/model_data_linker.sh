#!/bin/sh
#
# This script links GRIDDED MODEL DATA data
# that is available for a given input directory
# 

#set -aeu

echo "MSG: model_data_linker.sh started `date`"

while test $# -gt 0; do
  case ${1} in
    -i)
      shift
      if test $# -gt 0; then
        echo "MSG: -i was triggered, Description: input directory, Parameter: ${OPTARG}" >&2
        IDIR="${1}"
      fi
      shift;;
    -o)
      shift
      if test $# -gt 0; then
        echo "MSG: -o was triggered, Description: output directory, Parameter: ${OPTARG}" >&2
        ODIR=${1}
      fi
      shift;;
    -e)
      shift
      if test $# -gt 0; then
        echo "MSG: -e was triggered, Description: input file extension, Parameter: ${OPTARG}" >&2
        EXT=${1}
      fi
      shift;;
    -m)
      shift
      if test $# -gt 0; then
        echo "MSG: -m was triggered, Description: machine, Parameter: ${OPTARG}" >&2
        MACHINE=${1}
      fi
      shift;;
    *)
      break;;
  esac
done

ODIR="${ODIR}"
EXT="${EXT:-"*grb2"}"
MACHINE="${MACHINE:-JET}"
if [ ${MACHINE} == "JET" ]; then
  IDIR="${IDIR:-"/public/data/grids/hwrf/"}"
elif [ ${MACHINE} == "HERA" ]; then
  IDIR="${IDIR:-"/scratch2/BMC/public/data/grids/hwrf/"}"
fi


# Create output directory
mkdir -p ${ODIR}


# Get an array of all files
ALLFILES=( $(find ${IDIR} -type f -name "${EXT}" | xargs -n 1 basename 2>/dev/null) )

# Loop over all files
for FILE in ${ALLFILES[@]}; do
  YMDH="$(echo "${FILE}" | cut -d "." -f2)"
  FHR="$(echo "${FILE}" | cut -d "." -f6 | cut -c2-4)"
  LONGNAME="$(echo "${FILE}" | cut -d "." -f1)"
  SID="$(echo "${LONGNAME}" | rev | cut -c1-3 | rev)"

  ODIR_FULL="${ODIR}/${YMDH}/${SID^^}/"

  #Check if directory exists
  if [ ! -d ${ODIR_FULL} ]; then
    mkdir -p ${ODIR_FULL}
  fi

  #Check if the file exists. If not, link it.
  if [ ! -f ${ODIR_FULL}${FILE} ]; then
    echo "MSG: Linking MODEL file --> ${ODIR_FULL}${FILE}"
    ln -sf ${IDIR}/${FILE} ${ODIR_FULL}/${FILE}
  else
    echo "MSG: MODEL file already linked --> ${ODIR_FULL}${FILE}"
  fi
done


# Remove directories for which HWRF files are no longer available.
echo "MSG: Checking available MODEL files."

# Find files and soft links.
ALL_HWRF=( $(find ${ODIR} -type f) $(find ${ODIR} -type l)) # | xargs -n 1 basename` )

for FILE in "${ALL_HWRF[@]}"; do
  echo "MSG: Working on --> ${FILE}"

  # Extract info from the file path
  HWRF_BASE="$(echo "${FILE}" | xargs -n 1 basename)"
  HWRF_PATH="$(echo "${FILE}" | rev | cut -d "/" -f2- | rev)"
  YMDH="$(echo "${HWRF_BASE}" | cut -d "." -f2)"

  # Remove the link if the source file does not exist in $IDIR
  if ls ${IDIR}/${HWRF_BASE} 2> /dev/null; then
    echo "MSG: MODEL file is available for ${YMDH}. Keeping link."
  else
    echo "MSG: MODEL file is no longer available for ${YMDH}. Deleting link."
    rm -f ${FILE}
    if [ -z "$(ls -A ${HWRF_PATH})" ]; then
      echo "MSG: Directory is empty. Deleting it."
      rm -rf ${HWRF_PATH}
    fi
  fi
done

ALL_DIR=( $(find ${ODIR} -mindepth 1 -type d) )
for D in "${ALL_DIR[@]}"; do
  echo "MSG: Checking this directory --> ${D}"
  if [ -z "$(ls -A ${D})" ]; then
    echo "MSG: Directory is empty. Deleting..."
    rm -rf ${D}
  fi
done


echo "MSG: model_data_linker.sh completed `date`"
