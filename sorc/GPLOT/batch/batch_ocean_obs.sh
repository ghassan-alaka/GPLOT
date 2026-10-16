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
OCEAN_OBS_DIR="${18}"


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

# 2. Build list of named input arguments for Python.
# plot_ocean_obs.py uses argparse (--idate, --sid, --ocean-domain, ...);
# the legacy positional invocation that lived here was a leftover from
# pre-argparse and tripped "the following arguments are required:
# --idate, --sid, ..." the first time anyone turned DO_OCEAN_OBS=True.
# Mirror the batch_ocean_maps.sh pattern exactly so the two ocean
# submodules stay in sync.
PYTHON_ARGS=()
PYTHON_ARGS+=("--idate"        "${IDATE}")
PYTHON_ARGS+=("--sid"          "${SID}")
PYTHON_ARGS+=("--ocean-domain" "${OCEAN_DOMAIN}")
PYTHON_ARGS+=("--tier"         "${TIER}")
PYTHON_ARGS+=("--ensid"        "${ENSID}")
PYTHON_ARGS+=("--resolution"   "${RESOLUTION}")
PYTHON_ARGS+=("--rmax"         "${RMAX}")
PYTHON_ARGS+=("--levs"         "${LEVS}")
PYTHON_ARGS+=("--master-nml"   "${NMLIST}")
PYTHON_ARGS+=("--ocean-source" "${OCEAN_SOURCE}")
PYTHON_ARGS+=("--ocean-cfg"    "${OCEAN_CFG}")
if [ ! -z "${FIX_DIR}" ] && [ "${FIX_DIR}" != "MISSING" ]; then
    PYTHON_ARGS+=("--fix-dir" "${FIX_DIR}")
fi
if [ "${FORCE}" == "True" ]; then
    PYTHON_ARGS+=("--force" "True")
fi
# --wrap-lon is an action='store_true' flag (no value); the spawn
# script passes OCEAN_WRAP_LON as the string "True"/"False"/"1"/"0",
# so translate to flag-present / flag-absent.
if [ "${OCEAN_WRAP_LON}" == "True" ] || [ "${OCEAN_WRAP_LON}" == "true" ] || \
   [ "${OCEAN_WRAP_LON}" == "1" ]; then
    PYTHON_ARGS+=("--wrap-lon")
fi
# OCEAN_OBS_DIR (positional arg 18 from spawn) is intentionally not
# forwarded -- plot_ocean_obs.py reads OCEAN_OBS_DIR from the master
# namelist directly, so the CLI surface stays in sync with the
# plot_ocean_maps.py contract.

# 3. Submit the Python job
echo "python3 ${PYTHONFILE} ${PYTHON_ARGS[@]}"
python3 ${PYTHONFILE} "${PYTHON_ARGS[@]}" > ${LOGFILE} 2>&1

wait

echo "$?"
echo "COMPLETE!"

