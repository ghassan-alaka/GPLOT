# GPLOT
# Graphical Post-processed Locus for Output of Tropical cyclones

Git repository URL:  https://github.com/ghassan-alaka/GPLOT.git


# Instructions for installation

1. Add 'GPLOT_DIR' to environment
        Define GPLOT_DIR as the path where you cloned the GPLOT repository.
        This variable is critical to the functionality of GPLOT.
        Add it to your ~/.cshrc or ~/.bashrc file.


2. Install NCL (NCAR Command Language).
	Pre-compiled libraries are preferred. To learn more, start here:
		https://www.ncl.ucar.edu/Download/

	Version 6.5.0 has been well-tested for GPLOT:
		https://www.earthsystemgrid.org/dataset/ncl.650.html

	On NOAA Jet:
		*Load the module:	module load ncl/6.5.0

	On NOAA Hera:
		*Load the module:	module load ncl/6.5.0


3. Install MET (Model Evaluation Tools)
	To download the source code, go to the DTC web site:
		https://dtcenter.org/community-code/model-evaluation-tools-met/download

	Version 8.1 has been well-tested for GPLOT:
		https://dtcenter.org/sites/default/files/community-code/met/met-8.1.1.20190708.tar.gz

	On NOAA Jet:
		*Load the modules:	module load contrib
					module load intel/18.0.5.274
					module load met/8.1_beta2

	On NOAA Hera:
		*Load the modules:	module load intel/18.0.5.274
					module load anaconda/anaconda2-4.4.0
					module load met/8.1

4. Install Python 3 Anaconda
	To download the source code, go to the Python web site:
		https://www.python.org/downloads/

	Version 3.7.3 has been well-tested for GPLOT:
		https://www.python.org/downloads/release/python-373/

	On NOAA Jet:
		*Add alias for python:	alias python="/lfs3/projects/hur-aoml/Andrew.Hazelton/anaconda3/bin/python"
		*Add python to path:	export PATH="/lfs3/projects/hur-aoml/Andrew.Hazelton/anaconda3/bin:${PATH}"
		Build the centroid module:
					cd python/modules
					python -m numpy.f2py -c ${GPLOT_DIR}/fortran/centroid.f90 -m centroid

	On NOAA Hera:
		*Add alias for python:	alias python="/scratch2/GFDL/nggps_aoml/Andrew.Hazelton/anaconda3/bin/python"
		*Add python to path:	export PATH=/scratch2/GFDL/nggps_aoml/Andrew.Hazelton/anaconda3/bin/:$PATH
		Build the centroid module:
					cd python/modules
					python -m numpy.f2py -c ${GPLOT_DIR}/fortran/centroid.f90 -m centroid


5. Link the correct module file (for NOAA systems only)
	Anything with an asterisk in Steps 2-4 should be taken care of automatically by loading
	the appropriate modulefile.

	On NOAA Jet:	cd modulefiles
			ln -sf GPLOT_mods.jet GPLOT_mods

	On NOAA Hera:	cd modulefiles
			ln -sf GPLOT_mods.hera GPLOT_mods
	



# Instructions to run

1. Set the namelist
	Create a master namelist --> namelist.master.${EXPT}
	It is most critical to decide which modules to run. Currently, 4 options:
		i.   Maps
		ii.  Stats
		iii. Ships
		iv.  Polar

2. If the experiment is new, add it to the table:
	${GPLOT_DIR}/tbl/ExptInfo.dat

3. If the model is new, add it to the table:
	${GPLOT_DIR}/tbl/ModelInfo.dat

4. Run the shell wrapper script
	./shell/GPLOT_wrapper.sh ${EXPT}
	By providing ${EXPT} as an argument, the GPLOT wrapper can find the correct master namelist
	and run with user-requested options.

