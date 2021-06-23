#!/bin/sh --login
#SBATCH --account=hur-aoml
#SBATCH --ntasks=1
#SBATCH --time=00:10:00
#SBATCH --partition=service
#SBATCH --mail-type=FAIL
#SBATCH --qos=windfall
#SBATCH --chdir=.
#SBATCH --output=/lfs4/HFIP/hur-aoml/Ghassan.Alaka/GPOUT/log/deliver_atcf_nhc.log
#SBATCH --error=/lfs4/HFIP/hur-aoml/Ghassan.Alaka/GPOUT/log/deliver_atcf_nhc.log
#SBATCH --job-name="deliver_atcf_nhc"
##SBATCH --mem=16G

################################################################################
#### UNIX Script Documentation Block
##
## Script name:         deliver_atcf_nhc.sh
##
## Script description:  Deliver specific models to a special A-Deck for NHC
##
## Author:              Ghassan J. Alaka, Jr.
##
## Date:                2020-08-06
##
## Script history log:
##
## 2020-08-06  Ghassan J. Alaka, Jr. -- Original version.
##
## Usage: deliver_atcf_nhc.sh <Input Directory> <Output Directory> <Input file name> <Output file name> <MODEL1> <MODEL2> ...
##
################################################################################

# Read command line args
IDIR="$1"
shift
ODIR="$1"
shift
ITAG="$1"
shift
OTAG="$1"
shift
MODELS=( "$@" )


# Checks
if [ ! -d "${IDIR}" ]; then
    echo "ERROR: Input directory must exist."
    exit 1
fi
if [ ! -d "${ODIR}" ]; then
    echo "MSG: Output directory doesn't exist, so I will create it."
    mkdir -p ${ODIR}
fi
if [ -z "${MODELS[*]}" ]; then
    echo "ERROR: At least one model ID must be provided."
    exit 2
fi


# Additional variables
X_MERGE="/home/Ghassan.Alaka/Shell/sh/atcf_merge.sh"


# Loop over all models
for M in "${MODELS[@]}"; do
    echo ""
    echo "MSG: Merging ${M} into A-Deck --> ${X_MERGE} ${M} ${ODIR}/ ${OTAG} 1 ${IDIR}/${ITAG}"
    ${X_MERGE} ${M} ${ODIR}/ "${OTAG}" 1 "${IDIR}/"${ITAG}
done


