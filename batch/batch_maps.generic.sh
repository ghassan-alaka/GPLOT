#!/bin/sh --login
#SBATCH --account=hur-aoml
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=12
#SBATCH --time=00:59:00
#SBATCH --partition=tjet,ujet,sjet,vjet,xjet,kjet
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
#SBATCH --output=/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/log/GPLOT.Default.out
#SBATCH --error=/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/log/GPLOT.Default.err
#SBATCH --job-name="GPLOT.Default"
#SBATCH --mem=16G

set -x

echo "`date`"

# Source the .profile to optimize the environment
source ~/.profile

# Load modules
module load intel

# Define critical environmental variables (based on NOAA's Jet)
LD_LIBRARY_PATH="/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:${LD_LIBRARY_PATH}"
#export OMP_NUM_THREADS=1

# 1. Get command line variables
NCLDIR=
NCLFILE=
LOGDIR=
LOGFILE=
NMLIST=
IDATE=
SID=
DOMAIN=
TIER=
ENSID=
FORCE=

# 2. Build list in input arguments for NCL
NCL_ARGS=()
if [ ! -z "$IDATE" ]; then
    NCL_ARGS+=('IDATE="'"${IDATE}"'"')
fi
if [ ! -z "$SID" ]; then
    NCL_ARGS+=('SID="'"${SID}"'"')
fi
if [ ! -z "$DOMAIN" ]; then
    NCL_ARGS+=('DOMAIN="'"${DOMAIN}"'"')
fi
if [ ! -z "$TIER" ]; then
    NCL_ARGS+=('TIER="'"${TIER}"'"')
fi
if [ ! -z "$ENSID" ]; then
    NCL_ARGS+=('ENSID="'"${ENSID}"'"')
fi
if [ ! -z "$FORCE" ]; then
    NCL_ARGS+=('FORCE="'"${FORCE}"'"')
fi
if [ ! -z "$NMLIST" ]; then
    NCL_ARGS+=('MASTER_NML_IN="'"${NMLIST}"'"')
fi

# 2. Submit the NCL job
echo "${NCL_ARGS[*]}"
ncl "${NCL_ARGS[@]}" ${NCLDIR}${NCLFILE} >> ${LOGDIR}${LOGFILE}

wait

echo "$?"
echo "COMPLETE!"

