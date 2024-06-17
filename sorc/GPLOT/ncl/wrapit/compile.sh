#!/bin/sh
# Be sure to load the correct modulefile before running this file

export NCLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd ${NCLDIR}

rm -f *.so
WRAPIT xy2cyn.stub ../../fortran/xy2cyn.f90
WRAPIT sph2cart.stub ../../fortran/sph2cart.f90
WRAPIT mwavg.stub ../../fortran/mwavg.f90
WRAPIT hbfilter.stub ../../fortran/hbfilter.f90
WRAPIT filter121.stub ../../fortran/filter121.f90
WRAPIT centroid.stub ../../fortran/centroid.f90
WRAPIT splcal_wrapit.f

echo "Found these shared objects:"
ls -l *.so
