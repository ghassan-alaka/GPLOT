#!/bin/sh
##SBATCH --account=aoml-hafs1
#SBATCH --ntasks=1
#SBATCH --time=00:10:00
#SBATCH --partition=service
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
##SBATCH --output=/scratch2/NAGAPE/aoml-hafs1/Ghassan.Alaka/GPOUT/log/deliver_graphics.log
##SBATCH --error=/scratch2/NAGAPE/aoml-hafs1/Ghassan.Alaka/GPOUT/log/deliver_graphics.log
#SBATCH --job-name="GPLOT_deliver_graphics"
#SBATCH --mem=16G

################################################################################
#### UNIX Script Documentation Block
##
## Script name:         deliver_graphics.sh
##
## Script description:  Deliver GPLOT graphics from HAFS real-time workflows.
##
## Author:              Ghassan J. Alaka, Jr.
##
## Date:                2020-07-02
##
## Script history log:
##
## 2020-07-02  Ghassan J. Alaka, Jr. -- Original version.
##
## Usage: deliver_graphics.sh <Input Directory> <Output Directory>
##                            <Graphics Subdirectory Name> <Forecast Cycle>
##                            <Experiment Name>
##
################################################################################

echo "MSG: deliver_graphics.sh started at `date`"

# Read & Check input arguments
IDIR="$1"    # Input top-level directory
if [ ! -d ${IDIR} ]; then
    echo "ERROR: Input directory must exist."
fi
ODIR="$2"    # Output top-level directory
if [ ! -d ${ODIR} ]; then
    echo "ERROR: Output directory must exist."
fi
LOC="$3"     # Graphics subdirectory name
if [ "${LOC}" == "0" ]; then
    echo "MSG: Defaulting graphics subdirectory name to 'hrdgraphics'."
    LOC="hrdgraphics"
fi
CYCLE="$4"   # Forecast cycle (YYYYMMDDHH)
GET_CYCLE="NO"
if [ "$CYCLE" == "0" ]; then
    GET_CYCLE="YES"
fi
EXPT="$5"    # Experiment name, required for creating full output path
GET_EXPT="NO"
if [ "${EXPT}" == "0" ]; then
    echo "WARNING: Experiment name is undefined. Will attempt to parse from the input path."
    GET_EXPT="YES"
fi

# Define the rsync executable
X_RSYNC="/home/Ghassan.Alaka/GPLOT/ush/rsync_no_vanished.sh"

# Find all matching input directories beneath IDIR
echo "find ${IDIR} -type d -name \"${LOC}\""
ALL_DIRS=`find "${IDIR}"/ -type d -name "${LOC}"`

# Loop over all directories
for D in ${ALL_DIRS[@]}; do

    echo "MSG: Processing this directory --> ${D}"

    # If CYCLE or EXPT are set to "0", then search the input
    # path for these quantities. Searching for the CYCLE
    # works very well. It is not advised to search for the
    # EXPT in this way.
    if [ ${GET_CYCLE} == "YES" ] || [ ${GET_EXPT} == "YES" ]; then

        # Parse the current path into its elements
        IFS='/' read -r -a D_ELEM <<< "${D}"

        # Loop over all path elements
        for i in "${!D_ELEM[@]}"; do

            # Search through the elements in reverse order.
            # This is because the CYCLE and EXPT are more likely
            # to be at the end of the path rather than the beginning.
            j=$(expr ${#D_ELEM[@]} - $i - 1)
            elem=${D_ELEM[j]}

            # Find the forecast cycle from the input path if not defined.
            if [ ${GET_CYCLE} == "YES" ] && echo "${elem}" | grep -qE "[0-9]{10}"; then
                CYCLE="${elem}"
                echo "MSG: Found CYCLE=${CYCLE} in the path."
            fi

            # Guess the experiment name from the input path if not defined.
            if [ ${GET_EXPT} == "YES" ] && [ "$j" == "6" ]; then
                EXPT="${elem}"
                echo "MSG: Found EXPT=${EXPT} in the path."
            fi
   
        done
    fi

    # Define the full output path and create it.
    ODIR_FULL="${ODIR}/${EXPT}/${CYCLE}"
    mkdir -p ${ODIR_FULL}
    echo "MSG: Delivering graphics to this path --> ${ODIR_FULL}"

    # Copy all modified files.
    #${X_RSYNC} -zav --include="*/" --include="*gif" --include="*ships.dat" --exclude="*" ${D}/* ${ODIR_FULL}
    ${X_RSYNC} -zauv --include="*/" --include="*gif" --include="*ships.dat" --include="*structure*txt" --exclude="*" ${D}/* ${ODIR_FULL}

done


echo "MSG: deliver_graphics.sh completed at `date`"
