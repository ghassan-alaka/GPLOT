
# Load modules
module load intel
module load wgrib
module load wgrib2
module load grads

# NCAR Command Language (NCL)
module load ncl/6.5.0
setenv NCARG_COLORMAPS $GPLOT_DIR/ncl/colormaps:$NCARG_ROOT/lib/ncarg/colormaps

# Model Evaluation Tools (MET)
module load contrib
module load anaconda/anaconda2-4.4.0
module use /contrib/modulefiles
module load met/8.1

# Python 3 Anaconda
setenv python /scratch2/GFDL/nggps_aoml/Andrew.Hazelton/anaconda3/bin/python
setenv PATH /scratch2/GFDL/nggps_aoml/Andrew.Hazelton/anaconda3/bin:$PATH
setenv PYTHONBUFFERED 1
