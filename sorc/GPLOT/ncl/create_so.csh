#!/bin/csh
module purge
source ../../../modulefiles/modulefile.gplot.ursatcsh 0
setenv GPLOT_DIR /scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOThafs/
echo $GPLOT_DIR

set objects = ( centroid filter121 hbfilter mwavg sph2cart xy2cyn splcal )
#set objects = ( centroid )
foreach object ( $objects )
	echo $object
	if ( -f /scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOThafs/sorc/GPLOT/fortran/${object}.f90 ) then
		WRAPIT /scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOThafs/sorc/GPLOT/ncl/wrapit/${object}.stub /scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOThafs/sorc/GPLOT/fortran/${object}.f90
	else if ( -f /scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOThafs/sorc/GPLOT/fortran/${object}.f ) then
		WRAPIT /scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOThafs/sorc/GPLOT/ncl/wrapit/${object}.stub /scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOThafs/sorc/GPLOT/fortran/${object}.f
	else
		echo "FILE DOES NOT EXIST"
	endif
end
