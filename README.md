# GPLOT
# Graphic Post-processed Locus for Output of Tropical cyclones

Git repository URL:  https://github.com/ghassan-alaka/GPLOT.git


# Instructions for installation

1. Install NCL (NCAR Command Language).
	Pre-compiled libraries are preferred. To learn more, start here:
		https://www.ncl.ucar.edu/Download/

	Version 6.3.0 has been well-tested for GPLOT:
		https://www.earthsystemgrid.org/dataset/ncl.630.html

	On NOAA Jet:
		a) Copy the NCL6.3.0 tarball from the following location:
			/mnt/lfs1/projects/hur-aoml/Ghassan.Alaka/NCL/tarballs/ncl_ncarg-6.3.0.Linux_RHEL6.4_x86_64_nodap_gcc447.tar.gz
		b) Install the version with GCC 4.4.7:
			https://www.earthsystemgrid.org/dataset/ncl.630.1/file/ncl_ncarg-6.3.0.Linux_RHEL6.4_x86_64_nodap_gcc447.tar.gz


2. Install MET-TC (Model Evaluation Tools for Tropical Cyclones)
	To download the source code, go to the DTC web site:
		https://dtcenter.org/community-code/model-evaluation-tools-met/download

	Version 8.0 has been well-tested for GPLOT:
		https://dtcenter.org/sites/default/files/met-8.0.20180927.tar.gz

	On NOAA Jet, point to the pre-compiled version of MET-TC:
		Add important variables to your environment. Preferable to add these to your ~/.cshrc or ~/.bashrc file:
			PATH --> /lfs1/projects/dtc-hurr/MET/MET_releases/8.0/bin
			LD_LIBRARY_PATH --> /lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib


3. Install Python 3 Anaconda
	To download the source code, go to the Python web site:
		https://www.python.org/downloads/

	Version 3.6.5 has been well-tested for GPLOT:
		https://www.python.org/downloads/release/python-365/

	On NOAA Jet, point to the installed version of Python 3:
		Add to PATH --> /lfs3/projects/hur-aoml/Andrew.Hazelton/anaconda3/bin
		Alias python to /lfs3/projects/hur-aoml/Andrew.Hazelton/anaconda3/bin/python


4. Add 'GPLOT_DIR' to environment
	Define GPLOT_DIR as the path where you cloned the GPLOT repository.
	This variable is critical to the functionality of GPLOT.
	Add it to your ~/.cshrc or ~/.bashrc file.



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

