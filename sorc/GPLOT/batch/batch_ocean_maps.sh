#!/bin/sh -x
#SBATCH --account=hur-aoml
##SBATCH --nodes=1
##SBATCH --ntasks-per-node=12
#SBATCH --ntasks=1
#SBATCH --time=00:59:00
#SBATCH --partition=tjet,ujet,sjet,vjet,xjet,kjet
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
#SBATCH --output=/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/log/GPLOT.Default.out
#SBATCH --error=/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/log/GPLOT.Default.err
#SBATCH --job-name="GPLOT.Default"
#SBATCH --mem=32G

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

# 2. Build list in input arguments for Python
PYTHON_ARGS=()
if [ ! -z "$IDATE" ]; then
    PYTHON_ARGS+=("${IDATE}")
else
    PYTHON_ARGS+=("MISSING")
fi
if [ ! -z "$SID" ]; then
    PYTHON_ARGS+=("${SID}")
else
    PYTHON_ARGS+=("MISSING")
fi
if [ ! -z "$OCEAN_DOMAIN" ]; then
    PYTHON_ARGS+=("${OCEAN_DOMAIN}")
else
    PYTHON_ARGS+=("MISSING")
fi
if [ ! -z "$TIER" ]; then
    PYTHON_ARGS+=("${TIER}")
else
    PYTHON_ARGS+=("MISSING")
fi
if [ ! -z "$ENSID" ]; then
    PYTHON_ARGS+=("${ENSID}")
else
    PYTHON_ARGS+=("MISSING")
fi
if [ ! -z "$FORCE" ]; then
    PYTHON_ARGS+=("${FORCE}")
else
    PYTHON_ARGS+=("MISSING")
fi
if [ ! -z "$RESOLUTION" ]; then
    PYTHON_ARGS+=("${RESOLUTION}")
else
    PYTHON_ARGS+=("MISSING")
fi
if [ ! -z "$RMAX" ]; then
    PYTHON_ARGS+=("${RMAX}")
else
    PYTHON_ARGS+=("MISSING")
fi
if [ ! -z "$LEVS" ]; then
    PYTHON_ARGS+=("${LEVS}")
else
    PYTHON_ARGS+=("MISSING")
fi
if [ ! -z "$NMLIST" ]; then
    PYTHON_ARGS+=("${NMLIST}")
else
    PYTHON_ARGS+=("MISSING")
fi
if [ ! -z "$OCEAN_SOURCE" ]; then
    PYTHON_ARGS+=("${OCEAN_SOURCE}")
else
    PYTHON_ARGS+=("MISSING")
fi
if [ ! -z "$OCEAN_CFG" ]; then
    PYTHON_ARGS+=("${OCEAN_CFG}")
else
    PYTHON_ARGS+=("MISSING")
fi

if [ ! -z "$FIX_DIR" ]; then
    PYTHON_ARGS+=("${FIX_DIR}")
else
    PYTHON_ARGS+=("MISSING")
fi

if [ ! -z "$OCEAN_WRAP_LON" ]; then
    PYTHON_ARGS+=("${OCEAN_WRAP_LON}")
else
    PYTHON_ARGS+=("MISSING")
fi

# 2. Submit the Python job
echo "${PYTHON_ARGS[*]}"
python ${PYTHONFILE} ${PYTHON_ARGS[*]} > ${LOGFILE}

wait

echo "$?"
echo "COMPLETE!"

