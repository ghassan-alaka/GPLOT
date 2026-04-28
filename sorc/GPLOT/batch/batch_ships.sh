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
#SBATCH --mem=48G

set -x

# 1. Get command line arguments
MACHINE="${1:-${MACHINE}}"
PYFILE="${2}"
LOGFILE="${3}"
NMLIST="${4:-namelist.master.default}"
ENSID="${5:-XX}"
IDATE="${6}"
SID="${7:-00L}"
FORCE="${8:-False}"
DOMAIN="${9:-ships}"
TIER="${10:-Tier1}"

# 2. Determine the GPLOT source code directory
if [ -z "${GPLOT_DIR}" ]; then
    export GPLOT_DIR="$( echo "$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" | rev | cut -d'/' -f4- | rev )"
fi

# Source GPLOT_mods to optimize the environment
source ${GPLOT_DIR}/modulefiles/modulefile.gplot.${MACHINE,,} 0

# 2. Build list of input arguments for Python
PY_ARGS=()
PY_ARGS+=("--idate" "${IDATE}")
PY_ARGS+=("--sid" "${SID}")
PY_ARGS+=("--domain" "${DOMAIN}")
PY_ARGS+=("--tier" "${TIER}")
PY_ARGS+=("--master-nml" "${NMLIST}")
if [ "${ENSID}" != "XX" ] && [ ! -z "${ENSID}" ]; then
    PY_ARGS+=("--ensid" "${ENSID}")
fi
if [ "${FORCE}" == "True" ]; then
    PY_ARGS+=("--force")
fi

# 2. Submit the Python job
echo "python3 ${PYFILE} ${PY_ARGS[@]}"
python3 ${PYFILE} "${PY_ARGS[@]}" > ${LOGFILE}

wait

echo "$?"
echo "COMPLETE!"

