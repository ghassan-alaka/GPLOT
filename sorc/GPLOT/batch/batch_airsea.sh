#!/bin/sh
#SBATCH --account=aoml-hafs1
##SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=00:59:00
#SBATCH --partition=u1-compute
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
#SBATCH --output=/scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOT/log/GPLOT.Default.out
#SBATCH --error=/scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOT/log/GPLOT.Default.err
#SBATCH --job-name="GPLOT.Default"
#SBATCH --mem=128G

#set -x

# 1. Get command line variables
MACHINE="${1:-${MACHINE}}"
PYTHONFILE="${2}"
LOGFILE="${3}"
NMLIST="${4:-namelist.master.default}"
ENSID="${5:-XX}"
IDATE="${6}"
SID="${7}"
DOMAIN="${8}"
TIER="${9}"
RESOLUTION="${10}"
RMAX="${11}"
LEVS="${12}"
FORCE="${13}"

# 2. Determine the GPLOT source code directory
if [ -z "${GPLOT_DIR}" ]; then
    export GPLOT_DIR="$( echo "$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" | rev | cut -d'/' -f4- | rev )"
fi

# 3. Source the .profile to optimize the environment
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

# 2. Build list of input arguments for Python (argparse-style --flags)
PYTHON_ARGS=()
PYTHON_ARGS+=("--idate"      "${IDATE:-MISSING}")
PYTHON_ARGS+=("--sid"        "${SID:-MISSING}")
PYTHON_ARGS+=("--domain"     "${DOMAIN:-MISSING}")
PYTHON_ARGS+=("--tier"       "${TIER:-MISSING}")
PYTHON_ARGS+=("--ensid"      "${ENSID:-MISSING}")
PYTHON_ARGS+=("--force"      "${FORCE:-MISSING}")
PYTHON_ARGS+=("--resolution" "${RESOLUTION:-MISSING}")
PYTHON_ARGS+=("--rmax"       "${RMAX:-MISSING}")
PYTHON_ARGS+=("--levs"       "${LEVS:-MISSING}")
PYTHON_ARGS+=("--master-nml" "${NMLIST:-MISSING}")

# 2. Submit the Python job
echo "${PYTHON_ARGS[*]}"
python ${PYTHONFILE} "${PYTHON_ARGS[@]}" > ${LOGFILE} 2>&1

wait

echo "$?"
echo "COMPLETE!"

