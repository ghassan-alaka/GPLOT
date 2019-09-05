#!/bin/sh --login

#module load pgi
module load intel

# read args
SID="$1"
CYCLE="$2"
IFILE="$3"

# load necessary env vars
export ATCFINC=/lfs3/projects/hur-aoml/Ghassan.Alaka/GPLOT/HAFSV0.B_Forecast/2019090312/multi_model/atcf/NHC_interp
export ATCFSTRMS=/lfs3/projects/hur-aoml/Ghassan.Alaka/GPLOT/HAFSV0.B_Forecast/2019090312/multi_model/atcf/NHC_interp/atcf
#export ATCFINC=AAAA
#export ATCFSTRMS=BBBB


cd ${ATCFINC}
./intrfcst.exe $SID $CYCLE $IFILE
