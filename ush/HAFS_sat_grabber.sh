#!/bin/bash
################################################################################
# HAFS_sat_grabber.sh
#
# Download HAFS satellite (".sat.") GRIB2 files from NOMADS for a single
# model (hfsa or hfsb). Crawls the NOMADS prod directory listing, finds all
# available dates/cycles/storms, and mirrors the files into the AOML
# MODEL_OUTPUT directory layout:
#
#     <OUTPUT_BASE>/<YYYYMMDDHH>/<SID>/<file>
#     e.g. .../HAFS/HFSB/v2/oper/2026071506/05E/05e.2026071506.hfsb.parent.sat.f039.grb2.idx
#
# By default only the small ".grb2.idx" index files are downloaded (for
# local testing with limited disk). Pass -g to download the full ".grb2"
# files as well (for the supercomputer).
#
# Files that already exist locally (non-empty) are skipped, so the script
# is safe to run repeatedly (e.g. from cron).
#
# Usage:
#   HAFS_sat_grabber.sh -m <hfsa|hfsb> -o <output_base> [-g] [-n]
#
#   -m   Model: hfsa or hfsb (required)
#   -o   Output base directory, the model-specific "oper" dir (required)
#        e.g. /Users/andrew.hazelton/Code/test_data/DATA/MODEL_OUTPUT/HAFS/HFSB/v2/oper
#        or   /scratch4/AOML/aoml-hafs1/role.aoml-hafs1/DATA/MODEL_OUTPUT/HAFS/HFSB/v2/oper
#   -g   Also download the full .grb2 files (default: .grb2.idx only)
#   -n   Dry run: list what would be downloaded without downloading
#
# Example (grab both models):
#   HAFS_sat_grabber.sh -m hfsa -o .../HAFS/HFSA/v2/oper
#   HAFS_sat_grabber.sh -m hfsb -o .../HAFS/HFSB/v2/oper
#
# Notes:
#   * NOMADS sends a malformed Content-Length header that breaks curl's
#     HTTP/2 mode, so all requests force --http1.1.
#   * NOMADS asks users to stay under ~120 requests/minute; a short sleep
#     is inserted between downloads (SLEEP_SECS below).
#   * Concurrent runs for the same model/output dir are prevented with an
#     flock on <OUTPUT_BASE>/.HAFS_sat_grabber.<model>.lock, so overlapping
#     cron invocations can't corrupt an in-progress download. If a run is
#     already active the new one exits 0 immediately. (On systems without
#     the flock utility, e.g. macOS, locking is skipped.)
################################################################################

set -u

NOMADS_BASE="https://nomads.ncep.noaa.gov/pub/data/nccf/com/hafs/prod"
SLEEP_SECS=0.5
CURL="curl -fsS --http1.1 --max-time 300 --retry 2 --retry-delay 5"

MODEL=""
OUTPUT_BASE=""
GET_GRB2="NO"
DRY_RUN="NO"

usage() { grep '^#' "$0" | sed 's/^# \{0,1\}//'; exit 1; }

while getopts ":m:o:gnh" opt; do
    case "${opt}" in
        m) MODEL="${OPTARG}" ;;
        o) OUTPUT_BASE="${OPTARG}" ;;
        g) GET_GRB2="YES" ;;
        n) DRY_RUN="YES" ;;
        h|*) usage ;;
    esac
done

if [ -z "${MODEL}" ] || [ -z "${OUTPUT_BASE}" ]; then
    echo "ERROR: -m and -o are required." >&2
    usage
fi
MODEL="$(echo "${MODEL}" | tr '[:upper:]' '[:lower:]')"
if [ "${MODEL}" != "hfsa" ] && [ "${MODEL}" != "hfsb" ]; then
    echo "ERROR: -m must be 'hfsa' or 'hfsb', got '${MODEL}'." >&2
    exit 1
fi
if [ ! -d "${OUTPUT_BASE}" ]; then
    echo "ERROR: output base directory does not exist: ${OUTPUT_BASE}" >&2
    exit 1
fi

# Guard against overlapping cron invocations: hold an exclusive lock for
# this model/output dir for the lifetime of the script (released on exit).
LOCKFILE="${OUTPUT_BASE}/.HAFS_sat_grabber.${MODEL}.lock"
if command -v flock >/dev/null 2>&1; then
    exec 200>"${LOCKFILE}"
    if ! flock -n 200; then
        echo "Another ${MODEL} instance already holds ${LOCKFILE}; exiting."
        exit 0
    fi
else
    echo "WARNING: 'flock' not found; skipping concurrent-run protection." >&2
fi

# Extract href targets from a NOMADS autoindex page. Returns one name per
# line; the parent-directory link (absolute path) is filtered out.
list_hrefs() {
    local url="$1"
    ${CURL} "${url}" 2>/dev/null | grep -oE 'href="[^"?/][^"]*"' | sed 's/^href="//; s/"$//'
}

N_NEW=0
N_SKIP=0
N_FAIL=0

echo "=== HAFS_sat_grabber: model=${MODEL} output=${OUTPUT_BASE} grb2=${GET_GRB2} dry_run=${DRY_RUN}"

# 1) Find all <model>.YYYYMMDD date directories currently on NOMADS.
DATE_DIRS="$(list_hrefs "${NOMADS_BASE}/" | grep -E "^${MODEL}\.[0-9]{8}/$")"
if [ -z "${DATE_DIRS}" ]; then
    echo "ERROR: no ${MODEL}.YYYYMMDD directories found under ${NOMADS_BASE}/" >&2
    exit 1
fi

for DDIR in ${DATE_DIRS}; do
    DDIR="${DDIR%/}"                       # hfsb.20260715
    YMD="${DDIR#${MODEL}.}"                # 20260715

    # 2) Find the cycle subdirectories (00/ 06/ 12/ 18/) for this date.
    CYCLES="$(list_hrefs "${NOMADS_BASE}/${DDIR}/" | grep -E '^[0-9]{2}/$')"
    for CYC in ${CYCLES}; do
        CYC="${CYC%/}"                     # 06
        CYCLE_URL="${NOMADS_BASE}/${DDIR}/${CYC}"
        YMDH="${YMD}${CYC}"                # 2026071506

        # 3) List the .sat. files in this cycle. Always take the .idx
        #    files; with -g take the .grb2 files too.
        if [ "${GET_GRB2}" == "YES" ]; then
            FILE_RE='\.sat\.f[0-9]{3}\.grb2(\.idx)?$'
        else
            FILE_RE='\.sat\.f[0-9]{3}\.grb2\.idx$'
        fi
        FILES="$(list_hrefs "${CYCLE_URL}/" | grep -E "${FILE_RE}")"
        [ -z "${FILES}" ] && continue

        echo "--- ${DDIR}/${CYC}: $(echo "${FILES}" | wc -l | tr -d ' ') matching file(s)"

        for FNAME in ${FILES}; do
            # Filename: <sid>.<YYYYMMDDHH>.<model>.<domain>.sat.fHHH.grb2[.idx]
            SID="$(echo "${FNAME}" | cut -d. -f1 | tr '[:lower:]' '[:upper:]')"   # 05E
            FCYCLE="$(echo "${FNAME}" | cut -d. -f2)"
            if [ "${FCYCLE}" != "${YMDH}" ]; then
                echo "    WARNING: cycle in filename (${FCYCLE}) != directory cycle (${YMDH}) for ${FNAME}; using filename." >&2
            fi

            DEST_DIR="${OUTPUT_BASE}/${FCYCLE}/${SID}"
            DEST="${DEST_DIR}/${FNAME}"

            # Skip anything we already have.
            if [ -s "${DEST}" ]; then
                N_SKIP=$((N_SKIP + 1))
                continue
            fi

            if [ "${DRY_RUN}" == "YES" ]; then
                echo "    [dry-run] would download ${CYCLE_URL}/${FNAME} -> ${DEST}"
                N_NEW=$((N_NEW + 1))
                continue
            fi

            # Create the date/storm subdirectory if it doesn't exist yet.
            if [ ! -d "${DEST_DIR}" ]; then
                mkdir -p "${DEST_DIR}" || { echo "ERROR: cannot create ${DEST_DIR}" >&2; exit 1; }
            fi

            # Download to a temp name, then move into place so a partial
            # transfer never masquerades as a completed file.
            if ${CURL} -o "${DEST}.tmp" "${CYCLE_URL}/${FNAME}"; then
                mv "${DEST}.tmp" "${DEST}"
                echo "    downloaded ${FNAME} -> ${DEST_DIR}/"
                N_NEW=$((N_NEW + 1))
            else
                rm -f "${DEST}.tmp"
                echo "    FAILED: ${CYCLE_URL}/${FNAME}" >&2
                N_FAIL=$((N_FAIL + 1))
            fi
            sleep "${SLEEP_SECS}"
        done
    done
done

echo "=== Done. downloaded=${N_NEW} skipped(existing)=${N_SKIP} failed=${N_FAIL}"
[ "${N_FAIL}" -gt 0 ] && exit 2
exit 0
