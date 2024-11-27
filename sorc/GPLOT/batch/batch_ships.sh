#!/bin/sh
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
#SBATCH --mem=48G

set -x

# 1. Get command line arguments
MACHINE="${1:-${MACHINE}}"
NCLFILE="${2}"
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

# Source the .profile to optimize the environment
source ${GPLOT_DIR}/modulefiles/modulefile.gplot.${MACHINE,,} 0

# 2. Build list in input arguments for NCL
NCL_ARGS=()
if [ ! -z "${IDATE}" ]; then
    NCL_ARGS+=('IDATE="'"${IDATE}"'"')
fi
if [ ! -z "${SID}" ]; then
    NCL_ARGS+=('SID="'"${SID}"'"')
fi
if [ ! -z "${DOMAIN}" ]; then
    NCL_ARGS+=('DOMAIN="'"${DOMAIN}"'"')
fi
if [ ! -z "${TIER}" ]; then
    NCL_ARGS+=('TIER="'"${TIER}"'"')
fi
if [ "${ENSID}" == "XX" ]; then
    NCL_ARGS+=('ENSID=""')
elif [ ! -z "${ENSID}" ]; then
    NCL_ARGS+=('ENSID="'"${ENSID}"'"')
fi
if [ ! -z "${FORCE}" ]; then
    NCL_ARGS+=('FORCE="'"${FORCE}"'"')
fi
if [ ! -z "${NMLIST}" ]; then
    NCL_ARGS+=('MASTER_NML_IN="'"${NMLIST}"'"')
fi

# 2. Submit the NCL job
echo "${NCL_ARGS[*]}"
ncl "${NCL_ARGS[@]}" ${NCLFILE} > ${LOGFILE}

wait

echo "$?"
echo "COMPLETE!"

