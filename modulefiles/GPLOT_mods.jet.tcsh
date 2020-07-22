#!/bin/sh --login

# MODULEFILE for JET (tcsh)

# Load modules
module purge
module load wgrib/1.8.1.0b
module load wgrib2/0.1.9.6a
module load intel/18.0.5.274
module load netcdf/4.6.1
module load hdf5/1.10.4
module load intelpython/3.6.5
module use -a /contrib/met/modulefiles
module load met/9.0
module load ncl/6.5.0
module load grads/2.0.2

# NCL
setenv NCARG_COLORMAPS $GPLOT_DIR/ncl/colormaps:$NCARG_ROOT/lib/ncarg/colormaps

# Python 3 Anaconda
setenv python /lfs4/HFIP/hur-aoml/Andrew.Hazelton/anaconda3/bin/python
setenv PATH "/lfs4/HFIP/hur-aoml/Andrew.Hazelton/anaconda3/bin:${PATH}"
setenv PYTHONBUFFERED 1

