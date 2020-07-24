#!/bin/sh --login

# MODULEFILE for JET (tcsh)

setenv A "$1"
if ( "${A}" == "" ) then
    setenv A "0"
endif

# Load modules
module purge
module load wgrib/1.8.1.0b
module load wgrib2/0.1.9.6a
module load intel/18.0.5.274
module load netcdf/4.6.1
if ( "${A}" == "0" ) then
    module load hdf5/1.10.4
    module load intelpython/3.6.5
    module use -a /contrib/met/modulefiles
    module load met/9.0
endif
module load ncl/6.5.0
module load grads/2.0.2

# NCL
setenv NCARG_COLORMAPS $GPLOT_DIR/ncl/colormaps:$NCARG_ROOT/lib/ncarg/colormaps

# Python 3 Anaconda
if ( "${A}" == "1" ) then
    setenv PYDIR "/lfs4/HFIP/hur-aoml/Levi.Cowan/anaconda3"
    # Python 3 Anaconda
    # >>> conda initialize >>>
    # !! Contents within this block are managed by 'conda init' !!
    if ( -f "${PYDIR}/etc/profile.d/conda.csh" ) then
        source "${PYDIR}/etc/profile.d/conda.csh" >& /dev/null
    else
        setenv PATH "${PYDIR}/bin:$PATH"
    endif
    # <<< conda initialize <<<
    conda activate GPLOT
endif

