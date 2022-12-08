#!/bin/sh -x

# 1. Determine the GPLOT source code directory
if [ -z "${GPLOT_DIR}" ]; then
    export GPLOT_DIR="$( echo "$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" | rev | cut -d'/' -f4- | rev )"
fi

# 2. Determine the machine (via the hostname)
HN1="$( hostname | cut -c1 )"
if [ "${HN1}" == "f" ]; then
    MACHINE="jet"
elif [ "${HN1}" == "h" ]; then
    MACHINE="hera"
elif [ "${HN1}" == "O" ]; then
    MACHINE="orion"
else
    echo "ERROR: Machine could not be determined."
    exit 1
fi

# 3. Load the module file for this specific machine
source ${GPLOT_DIR}/modulefiles/modulefile.gplot.${MACHINE} 1

# 4. Check for the modules
cd ${GPLOT_DIR}/sorc/GPLOT/python
python -c "import modules.centroid"
if [ "$?" == "1" ]; then
    cd ${GPLOT_DIR}/sorc/GPLOT/python/modules
    python -m numpy.f2py -c ${GPLOT_DIR}/sorc/GPLOT/fortran/centroid.f90 -m centroid
    if [ "$?" == "0" ]; then
        echo "done" > ${GPLOT_DIR}/sorc/GPLOT/python/modules/build.done
    fi
fi
