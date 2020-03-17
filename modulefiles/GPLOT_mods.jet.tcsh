#!/bin/sh --login

# MODULEFILE for JET (tcsh)

setenv GPLOT_DIR /lfs1/projects/hur-aoml/Ghassan.Alaka/pyGPLOT

# Load modules
module purge
module load wgrib/1.8.1.0b
module load wgrib2/0.1.9.6a
module load contrib
module load intel/18.0.5.274
module load intelpython/3.6.5
module load met/9.0_beta3
module load ncl/6.5.0
module load grads/2.0.2

# NCL
setenv NCARG_COLORMAPS $GPLOT_DIR/ncl/colormaps:$NCARG_ROOT/lib/ncarg/colormaps

# Python 3 Anaconda
#setenv python /lfs3/HFIP/hur-aoml/Andrew.Hazelton/anaconda3/bin/python
#setenv PATH "/lfs3/HFIP/hur-aoml/Andrew.Hazelton/anaconda3/bin:${PATH}"
setenv python /lfs1/HFIP/hur-aoml/Lew.Gramer/miniconda3/bin/python
setenv PATH "/lfs1/HFIP/hur-aoml/Lew.Gramer/miniconda3/bin:${PATH}"
setenv PYTHONBUFFERED 1

