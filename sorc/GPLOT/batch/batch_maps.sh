#!/bin/sh
#SBATCH --account=aoml-hafs1
##SBATCH --nodes=1
##SBATCH --ntasks-per-node=12
#SBATCH --ntasks=1
#SBATCH --time=00:59:00
#SBATCH --partition=u1-compute
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
#SBATCH --output=/scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOT/log/GPLOT.Default.out
#SBATCH --error=/scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOT/log/GPLOT.Default.err
#SBATCH --job-name="GPLOT.Default"
#SBATCH --mem=96G

set -x

echo "`date`"

# 1. Get command line arguments
MACHINE="${1:-${MACHINE}}"
PYFILE="${2}"
LOGFILE="${3}"
NMLIST="${4:-namelist.master.default}"
DOMAIN="${5:-atl}"
TIER="${6:-Tier1}"
ENSID="${7:-XX}"
MODELID="${8:-XXXX}"
IDATE="${9}"
SID="${10:-00L}"
FORCE="${11:-False}"

# 2. Determine the GPLOT source code directory
if [ -z "${GPLOT_DIR}" ]; then
    export GPLOT_DIR="$( echo "$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" | rev | cut -d'/' -f4- | rev )"
fi

# 3. Source GPLOT modulefile to optimize the environment
source ${GPLOT_DIR}/modulefiles/modulefile.gplot.${MACHINE,,} 1

# Export the per-machine offline cartopy cache so plot_utils.configure_cartopy()
# can fall back to it (as CARTOPY_DATA_DIR) when a namelist lacks a valid
# CARTOPY_DIR -- e.g. namelist.master.HAFS_Default's placeholder. Without this,
# cartopy tries to download Natural Earth data on an offline compute node and hangs.
BATCH_DFLTS="${GPLOT_DIR}/parm/batch.defaults.${MACHINE,,}"
if [ -f "${BATCH_DFLTS}" ]; then
    CARTOPY_DIR_DFLT="`sed -n -e 's/^cartopy_dir =\s//p' ${BATCH_DFLTS} | sed 's/^\t*//'`"
    if [ -n "${CARTOPY_DIR_DFLT}" ]; then
        export CARTOPY_DATA_DIR="${CARTOPY_DIR_DFLT}"
    fi
fi

# 4. Build array of input arguments for Python
PY_ARGS=()
PY_ARGS+=("--idate" "${IDATE}")
PY_ARGS+=("--sid" "${SID}")
PY_ARGS+=("--domain" "${DOMAIN}")
PY_ARGS+=("--tier" "${TIER}")
PY_ARGS+=("--master-nml" "${NMLIST}")
if [ "${ENSID}" != "XX" ] && [ ! -z "${ENSID}" ]; then
    PY_ARGS+=("--ensid" "${ENSID}")
fi
if [ ! -z "${MODELID}" ] && [ "${MODELID}" != "XXXX" ]; then
    PY_ARGS+=("--modelid" "${MODELID}")
fi
if [ "${FORCE}" == "True" ]; then
    PY_ARGS+=("--force")
fi
if [ ! -f ${PYFILE} ]; then
    echo "ERROR: The run script does not exist --> ${PYFILE}"
    exit 2
fi

# 5. Submit the Python job
echo "python3 ${PYFILE} ${PY_ARGS[@]}"
python3 ${PYFILE} "${PY_ARGS[@]}" > ${LOGFILE} 2>&1

wait

echo "$?"
echo "COMPLETE!"

