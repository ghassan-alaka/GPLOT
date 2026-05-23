#!/bin/bash

# Set up and activate GPLOT conda environment
export GPLOT_DIR=/work2/noaa/aoml-hafs1/nikhil/HAFS_ensemble
cd $GPLOT_DIR
source ${GPLOT_DIR}/modulefiles/modulefile.gplot.orion 1
conda activate GPLOT_env
cd /work2/noaa/aoml-hafs1/nikhil/GPLOT_Ensemble

# Load modules
module load spack-managed-x86-64_v3/v1.0 > /dev/null 2>&1
module load contrib wgrib2 > /dev/null 2>&1

# Turn plots on and off --------------------------------------------------------
ENSEMBLE_LINE_PLOTS=True
ENSEMBLE_TRACKS_COLORED=True
ENSEMBLE_WIND_RADII=True
ENSEMBLE_CLUSTERING=True
VORTEX_AVG_STEER=True

# Parameter Settings -----------------------------------------------------------
STORM="al132025"            # Storm to plot data for
DATE=2025102412             # Initialization Date: YYYYMMDDHH

# Forecast Hour Settings -------------------------------------------------------
FHOUR_START=36              # Starting forecast hour
FHOUR_END=48                # Ending forecast hour
FHOUR_INTERVAL=6            # Forecast hour interval

CLUSTER_TYPE="ltrack"        # Options: MSLP, R34, R50, R64, ltrack, xtrack
VARIABLE="HGT"              # Variable to plot as background for ENSEMBLE_CLUSTERING
LEVEL=500                   # Atmospheric level to plot for ENSEMBLE_CLUSTERING

MEMBERS_START=0             # Starting member number
MEMBERS_END=21              # Ending member number (exclusive)
CLUSTER_MEMBERS=4           # Number of members to include in each cluster

# Path Settings ----------------------------------------------------------------
BASE_DATA_PATH="/work/noaa/aoml-hafs1/lgramer/staging/data/ensemble/"
SAVE_PATH="/work2/noaa/aoml-hafs1/nikhil/GPLOT_Ensemble/GplotEnsFigs/"

# Clean up any leftover RAM disk files from crashed runs -----------------------
echo "Cleaning up stale RAM disk files..."
rm -f /dev/shm/temp_mem* /dev/shm/temp_centered_mem*

# Build space-separated list of forecast hours for Python ----------------------
FHOUR_LIST=$(seq $FHOUR_START $FHOUR_INTERVAL $FHOUR_END | tr '\n' ' ')

# Run Python once for all forecast hours ---------------------------------------
echo "Processing forecast hours: $FHOUR_LIST"
START_TIME=$(date +%s)

python ./GplotEnsemble.py \
    --ensembleLinePlots "$ENSEMBLE_LINE_PLOTS" \
    --ensembleTracksColored "$ENSEMBLE_TRACKS_COLORED" \
    --ensembleWindRadii "$ENSEMBLE_WIND_RADII" \
    --ensembleClustering "$ENSEMBLE_CLUSTERING" \
    --vortexAvgSteer "$VORTEX_AVG_STEER" \
    --fHours $FHOUR_LIST \
    --clusterType "$CLUSTER_TYPE" \
    --variable "$VARIABLE" \
    --level "$LEVEL" \
    --membersStart "$MEMBERS_START" \
    --membersEnd "$MEMBERS_END" \
    --clusterMembers "$CLUSTER_MEMBERS" \
    --date "$DATE" \
    --storm "$STORM" \
    --baseDataPath "$BASE_DATA_PATH" \
    --savePath "$SAVE_PATH"

END_TIME=$(date +%s)
TOTAL_ELAPSED=$((END_TIME - START_TIME))
echo ""
echo "All forecast hours completed"
echo "Total time: $TOTAL_ELAPSED seconds"