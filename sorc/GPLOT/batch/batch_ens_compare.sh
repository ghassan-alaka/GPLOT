#!/bin/sh
#SBATCH --account=aoml-hafs1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
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

# 6b. Thread budget for wgrib2 (OpenMP-built in v3.x).
#     plot_ens_compare.py runs the two cluster extractions concurrently
#     (ThreadPoolExecutor, 2 workers), and each worker shells out to wgrib2.
#     Give each wgrib2 half of the allocated cores so the two concurrent
#     instances don't oversubscribe the cgroup. Falls back to 1 if the
#     allocation is tiny or SLURM_CPUS_PER_TASK is unset.
OMP_N=$(( ${SLURM_CPUS_PER_TASK:-2} / 2 ))
if [ "${OMP_N}" -lt 1 ]; then OMP_N=1; fi
export OMP_NUM_THREADS=${OMP_N}
echo "Allocated CPUs: ${SLURM_CPUS_PER_TASK:-unset} (nproc reports $(nproc)); OMP_NUM_THREADS=${OMP_NUM_THREADS}"

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