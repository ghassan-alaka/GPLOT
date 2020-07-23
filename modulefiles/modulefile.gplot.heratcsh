#!/bin/sh --login

# MODULEFILE for JET (tcsh)

setenv A "$1"
if ( "${A}" == "" ) then
    setenv A "0"
endif

# Load modules
module purge
module load intel/18.0.5.274
module load wgrib/1.8.1.0b
module load wgrib2/2.0.8
if ( "${A}" == "0" ) then
    module use -a /contrib/anaconda/modulefiles
    module load anaconda/latest
    module use -a /contrib/met/modulefiles
    module load met/9.0_anacondal
endif
module load ncl/6.5.0
module load grads/2.0.2

# NCL
setenv NCARG_COLORMAPS $GPLOT_DIR/ncl/colormaps:$NCARG_ROOT/lib/ncarg/colormaps

# Python 3 Anaconda
if ( "${A}" == "1" ) then
    PYDIR="/scratch2/NAGAPE/aoml-hafs1/Levi.Cowan/anaconda3"
    # Python 3 Anaconda
    # >>> conda initialize >>>
    # !! Contents within this block are managed by 'conda init' !!
    if ( -f "${PYDIR}/etc/profile.d/conda.csh" ) then
        source "${PYDIR}/etc/profile.d/conda.csh" &> /dev/null
    else
        setenv PATH "${PYDIR}/bin:$PATH"
    endif
    # <<< conda initialize <<<
    conda activate GPLOT
endif

