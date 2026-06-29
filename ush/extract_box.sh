#!/bin/bash
# Usage: ./extract_box.sh [IN] [OUT] [LON_MIN] [LON_MAX] [LAT_MIN] [LAT_MAX] [MATCH_STR]

# Assign arguments
INPUT_FILE="$1"
TEMP_FILE="$2"
LON_MIN="$3"
LON_MAX="$4"
LAT_MIN="$5"
LAT_MAX="$6"
MATCH_STR="$7"

# Run wgrib2 to only extract variables, coordinates, and vertical levels that are needed
# Resulting small, cropped GRIB file is written to RAM
# This process takes a while but massively reduces memory bottleneck in Python
wgrib2 "$INPUT_FILE" \
    -match "$MATCH_STR" \
    -small_grib ${LON_MIN}:${LON_MAX} ${LAT_MIN}:${LAT_MAX} \
    "$TEMP_FILE"