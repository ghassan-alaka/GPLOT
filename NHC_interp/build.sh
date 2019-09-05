#!/bin/sh --login

#module load pgi
module load intel

#PATH="${PATH}:/apps/intel/compilers_and_libraries_2018.5.274/linux/bin/"
#LD_LILBRARY_PATH="${LD_LILBRARY_PATH}:/apps/intel/compilers_and_libraries_2018.5.274/linux/compiler/lib/"
#INTELLIB="/apps/intel/compilers_and_libraries_2018.5.274/linux/compiler/lib/"
#echo $PATH
#echo ${LD_LILBRARY_PATH}
C="ifort"
#C="pgf90"

#pgf90 -L${INTELLIB} -c anystips.f
${C} -c anystips.f
${C} -c dataio.f
${C} -c dirdst.f
${C} -c direction_and_spread.f
${C} -c dtgchk.f 
${C} -c dtgmod.f 
${C} -c dtgnum.f 
${C} -c dtgutils.f 
${C} -c dtgyrhr.f 
${C} -c greatcirc.f 
${C} -c hermite.f 
${C} -c hermite_routines.f90 
${C} -c mat_vect_mult.f 
${C} -c rltlg.f 
${C} -c site_cor.f 

${C} intrfcst.f dataio.o dirdst.o dtgchk.o dtgmod.o dtgnum.o dtgyrhr.o hermite.o hermite_routines.o rltlg.o -o intrfcst.exe
