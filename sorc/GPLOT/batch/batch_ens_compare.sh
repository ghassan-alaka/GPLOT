#!/bin/sh
#SBATCH --account=aoml-hafs1
#SBATCH --ntasks=1
#SBATCH --time=01:59:00
#SBATCH --partition=u1-compute
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
#SBATCH --output=/scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOT/log/GPLOT.Default.out
#SBATCH --error=/scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOT/log/GPLOT.Default.err
#SBATCH --job-name="GPLOT.Default"
#SBATCH --mem=16G

set -x

echo "`date`"

# 1. Get command line arguments
MACHINE="${1:-${MACHINE}}"
PYFILE="${2}"
LOGFILE="${3}"
NMLIST="${4:-namelist.master.default}"
IDATE="${5}"
SID="${6:-XXXX}"

# 2. Determine the GPLOT source code directory
if [ -z "${GPLOT_DIR}" ]; then
    export GPLOT_DIR="$( echo "$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" | rev | cut -d'/' -f4- | rev )"
fi

# 3. Source GPLOT modulefile to set up environment
source ${GPLOT_DIR}/modulefiles/modulefile.gplot.${MACHINE,,} 1

# 4. Activate conda environment
conda activate GPLOT_env

# 5. Ensure wgrib2 is available (HepTools slices GRIB via ush/extract_box.sh).
#    The modulefile may already provide it; only attempt a load if it doesn't.
if ! command -v wgrib2 > /dev/null 2>&1; then
    module load spack-managed-x86-64_v3/v1.0 > /dev/null 2>&1
    module load contrib wgrib2 > /dev/null 2>&1
fi

# 6. Validate that the python script exists
if [ ! -f ${PYFILE} ]; then
    echo "ERROR: Python script not found --> ${PYFILE}"
    exit 2
fi

# 7. Clean up any leftover RAM disk files from crashed runs
echo "Cleaning up stale RAM disk files..."
rm -f /dev/shm/temp_mem* /dev/shm/temp_centered_mem*

# 8. Run the ensemble comparison python script.
#    All configuration (member range, cluster/background settings, plot toggles,
#    paths, forecast-hour range) is read from the namelist by the Python module;
#    only the per-invocation identity (--idate/--sid) plus --master-nml is passed.
START_TIME=$(date +%s)

python3 ${PYFILE} \
    --master-nml "${NMLIST}" \
    --idate "${IDATE}" \
    --sid "${SID}" \
    >> ${LOGFILE} 2>&1

END_TIME=$(date +%s)
echo ""
echo "Cycle ${IDATE} completed in $((END_TIME - START_TIME)) seconds"
echo "COMPLETE!"