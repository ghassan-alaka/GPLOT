#!/bin/sh --login
#SBATCH --account=hur-aoml
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=12
#SBATCH --time=00:59:00
#SBATCH --partition=tjet,ujet,sjet,vjet,xjet,kjet
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
#SBATCH --output=/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/log/GPLOT.Default.out
#SBATCH --error=/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/log/GPLOT.Default.err
#SBATCH --job-name="GPLOT.Default"
#SBATCH --mem=16G

set -x

# Set stack size
#limit stacksize unlimited

# Source the .profile to optimize the environment
source ${GPLOT_DIR}/modulefiles/GPLOT_mods

# Load MET-TC library path
#LD_LIBRARY_PATH="/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:${LD_LIBRARY_PATH}"

# Load wgrib module (needed for gribmap)
#module load wgrib
#module load wgrib2/0.2.0.1

# Load GrADs for interfacing in Python
#module load grads

# Load intel module (needed for wgrib)
#module load intel

# Use the Python 3 install
#alias python /lfs3/projects/hur-aoml/Andrew.Hazelton/anaconda3/bin/python
#PYTHONEXE="/lfs3/projects/hur-aoml/Andrew.Hazelton/anaconda3/bin/python"
#PATH="/lfs3/projects/hur-aoml/Andrew.Hazelton/anaconda3/bin:${PATH}"

# Turn off buffered output
export PYTHONUNBUFFERED=1


# 1. Get command line variables
PYTHONDIR=
PYTHONFILE=
LOGDIR=
LOGFILE=
NMLIST=
IDATE=
SID=
DOMAIN=
TIER=
ENSID=
RESOLUTION=
RMAX=
LEVS=
FORCE=

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
if [ ! -z "$DOMAIN" ]; then
    PYTHON_ARGS+=("${DOMAIN}")
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
if [ ! -z "${PYTHONDIR}" ]; then
    PYTHON_ARGS+=("${PYTHONDIR}")
else
    PYTHON_ARGS+=("MISSING")
fi

# 2. Submit the Python job
echo "${PYTHON_ARGS[*]}"
#${PYTHONEXE} ${PYTHONDIR}${PYTHONFILE} ${PYTHON_ARGS[*]} > ${LOGDIR}${LOGFILE}
python ${PYTHONDIR}${PYTHONFILE} ${PYTHON_ARGS[*]} > ${LOGDIR}${LOGFILE}

wait

echo "$?"
echo "COMPLETE!"

