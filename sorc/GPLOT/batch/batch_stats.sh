#!/bin/sh
#SBATCH --account=aoml-hafs1
##SBATCH --nodes=1
##SBATCH --ntasks-per-node=12
#SBATCH --ntasks=1
#SBATCH --time=00:29:59
#SBATCH --partition=u1-compute
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
#SBATCH --output=/scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOT/log/GPLOT.Default.out
#SBATCH --error=/scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOT/log/GPLOT.Default.err
#SBATCH --job-name="GPLOT.Default"
#SBATCH --mem=96G

set -x

# 1. Get command line arguments
MACHINE="${1:-${MACHINE}}"
PYFILE="${2}"
LOGFILE="${3}"
NMLIST="${4:-namelist.master.default}"
IDATE="${5}"
SID="${6:-00L}"
FORCE="${7:-False}"

# 2. Determine the GPLOT source code directory
if [ -z "${GPLOT_DIR}" ]; then
    export GPLOT_DIR="$( echo "$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" | rev | cut -d'/' -f4- | rev )"
fi

# Source GPLOT_mods to optimize the environment
source ${GPLOT_DIR}/modulefiles/modulefile.gplot.${MACHINE,,} 1

# 2. Build list of input arguments for Python
PY_ARGS=()
PY_ARGS+=("--idate" "${IDATE}")
PY_ARGS+=("--sid" "${SID}")
PY_ARGS+=("--master-nml" "${NMLIST}")
if [ "${FORCE}" == "True" ]; then
    PY_ARGS+=("--force")
fi

# 2. Submit the Python job
echo "python3 ${PYFILE} ${PY_ARGS[@]}"
python3 ${PYFILE} "${PY_ARGS[@]}" > ${LOGFILE}

wait

echo "$?"
echo "COMPLETE!"
exit 0
