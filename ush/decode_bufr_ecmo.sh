#!/bin/sh

BUFRDIR="${1}"
ODIR="${2}"
PYTHONFILE="${3:-/home/Ghassan.Alaka/GPLOT/ush/bufr_read_tc_gus.py}"

# Python 3 Anaconda
PYDIR="${4:-/lfs1/HFIP/hur-aoml/Ghassan.Alaka/miniconda3_20220224}"
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('${PYDIR}/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "${PYDIR}/etc/profile.d/conda.sh" ]; then
        . "${PYDIR}/etc/profile.d/conda.sh" &>/dev/null
    else
        export PATH="${PYDIR}/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
conda activate GPLOT
export PYTHONBUFFERED=1


# ECCODES
export ECCODES_DIR="${5:-/lfs1/HFIP/hur-aoml/Ghassan.Alaka/software/eccodes/eccodes-2.22.1}"
export PATH="${ECCODES_DIR}:${PATH}"


# Get all BUFR files
ALL_BUFR=( `find ${BUFRDIR} -name "*.bufr" -type f` )
echo "${ALL_BUFR[*]}"

for BUFR in ${ALL_BUFR[@]}; do

    python ${PYTHONFILE} ${BUFR} ${ODIR}

    find ${ODIR} -size 0 -print -delete

done
