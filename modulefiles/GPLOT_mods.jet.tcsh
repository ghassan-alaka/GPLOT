#!/bin/sh --login

# MODULEFILE for JET (tcsh)

# Load modules
module purge
module load wgrib
module load wgrib2
module load intel
module load grads

# NCL
module load ncl/6.5.0
setenv NCARG_COLORMAPS $GPLOT_DIR/ncl/colormaps:$NCARG_ROOT/lib/ncarg/colormaps

# Model Evaluation Tools (MET)
#export PATH="/lfs1/projects/dtc-hurr/MET/MET_releases/8.0/bin:${PATH}"
#export LD_LIBRARY_PATH="/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:${LD_LIBRARY_PATH}"
module load contrib
module load met/8.1_beta2

# Python 3 Anaconda
setenv python /lfs3/projects/hur-aoml/Andrew.Hazelton/anaconda3/bin/python
setenv PATH "/lfs3/projects/hur-aoml/Andrew.Hazelton/anaconda3/bin:${PATH}"
setenv PYTHONBUFFERED 1

