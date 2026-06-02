#!/bin/sh -x
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

# 1. Get command line variables
MACHINE="${1:-${MACHINE}}"
PYTHONFILE="${2}"
LOGFILE="${3}"
NMLIST="${4:-namelist.master.default}"
ENSID="${5:-XX}"
IDATE="${6}"
SID="${7}"
#DOMAIN="${8}"
OCEAN_DOMAIN="${8}"
TIER="${9}"
RESOLUTION="${10}"
RMAX="${11}"
LEVS="${12}"
FORCE="${13}"
OCEAN_SOURCE="${14}"
OCEAN_CFG="${15}"
FIX_DIR="${16}"
OCEAN_WRAP_LON="${17}"


# 2. Determine the GPLOT source code directory
if [ -z "${GPLOT_DIR}" ]; then
    export GPLOT_DIR="$( echo "$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" | rev | cut -d'/' -f4- | rev )"
fi

# 3. Source the .profile to optimize the environment
source ${GPLOT_DIR}/modulefiles/modulefile.gplot.${MACHINE,,} 1

# 2. Build list of named input arguments for Python
PYTHON_ARGS=()
PYTHON_ARGS+=("--idate" "${IDATE}")
PYTHON_ARGS+=("--sid" "${SID}")
PYTHON_ARGS+=("--ocean-domain" "${OCEAN_DOMAIN}")
PYTHON_ARGS+=("--tier" "${TIER}")
PYTHON_ARGS+=("--ensid" "${ENSID}")
PYTHON_ARGS+=("--resolution" "${RESOLUTION}")
PYTHON_ARGS+=("--rmax" "${RMAX}")
PYTHON_ARGS+=("--levs" "${LEVS}")
PYTHON_ARGS+=("--master-nml" "${NMLIST}")
PYTHON_ARGS+=("--ocean-source" "${OCEAN_SOURCE}")
PYTHON_ARGS+=("--ocean-cfg" "${OCEAN_CFG}")
if [ ! -z "${FIX_DIR}" ] && [ "${FIX_DIR}" != "MISSING" ]; then
    PYTHON_ARGS+=("--fix-dir" "${FIX_DIR}")
fi
if [ "${FORCE}" == "True" ]; then
    PYTHON_ARGS+=("--force" "True")
fi
if [ "${OCEAN_WRAP_LON}" == "True" ] || [ "${OCEAN_WRAP_LON}" == "true" ] || \
   [ "${OCEAN_WRAP_LON}" == "1" ]; then
    PYTHON_ARGS+=("--wrap-lon")
fi

# 3. Submit the Python job
echo "python3 ${PYTHONFILE} ${PYTHON_ARGS[@]}"
python3 ${PYTHONFILE} "${PYTHON_ARGS[@]}" > ${LOGFILE} 2>&1

wait

echo "$?"
echo "COMPLETE!"

