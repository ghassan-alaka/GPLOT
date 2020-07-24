# GPLOT
# Graphical Post-processed Locus for Output of Tropical cyclones

Git repository URL:  https://github.com/ghassan-alaka/GPLOT.git


# Instructions for installation

1. Add 'GPLOT_DIR' to environment [MANDATORY]
        Define GPLOT_DIR as the path where you cloned the GPLOT repository.
        This variable is critical to the functionality of GPLOT.
            csh/tcsh --> Add it to ~/.cshrc & ~/.profile
            bash/sh  --> Add it to ~/.bashrc & ~/.bash_profile


2. Link the correct module file [MANDATORY]
        Anything with an asterisk in Steps 2-4 should be taken care of automatically by loading
        the appropriate modulefile.

        On NOAA Jet:    cd ${GPLOT_DIR}/modulefiles
                        ln -sf modulefile.gplot.jet GPLOT_mods

        On NOAA Hera:   cd ${GPLOT_DIR}/modulefiles
                        ln -sf modulefile.gplot.hera GPLOT_mods

        If running via wrapper scripts and batch nodes, it is critical that GPLOT_mods point to the
        correct version.

        If running interactively in a tcsh or csh environment, you must specifically source the "tcsh"
        version of these files:
                source ${GPLOT_DIR}/modulefiles/modulefile.gplot.jettcsh


3. Install NCL (NCAR Command Language) [MANDATORY FOR NON-NOAA MACHINES]
	Pre-compiled libraries are preferred. To learn more, start here:
		https://www.ncl.ucar.edu/Download/

	Version 6.5.0 has been well-tested for GPLOT:
		https://www.earthsystemgrid.org/dataset/ncl.650.html

        NOTE:  The NCL project is "feature frozen" as of September 2019, meaning
               CISL has no plans to add new features to NCL. They will continue
               to maintain the code and will infrequently release bug fixes as
               well as user-contributed code.

	NCL is automatically loaded as part of the GPLOT environment.
        To manually load NCL on NOAA Jet and NOAA Hera:
		*Load the module:	module load ncl/6.5.0

	.hluresfile:
		NCL configuration options are stored in .hluresfile. An example version
		of this file is stored in ncl/.hluresfile.example. Copy it to your
		home directory, for example $HOME/.hluresfile. You may add/change any
		value in .hluresfile, but only change 'wxMaximumSize' if you have
		good reason for doing so. 'wsMaximumSize' controls the memory
		available for NCL contour graphics.


4. Install METplus (Model Evaluation Tools) [MANDATORY FOR NON-NOAA MACHINES]
	To download the source code, go to the DTC web site
		https://dtcenter.org/community-code/metplus/download

	Version 9.0.2 has been well-tested for GPLOT:
		https://dtcenter.org/sites/default/files/community-code/met/met-9.0.2.20200522.tar.gz

	METplus is automatically loaded as part of the GPLOT environment.
        To manually load METplus on NOAA Jet:
		*Load the modules:	module load intel/18.0.5.274
                                        module load netcdf/4.6.1
                                        module load hdf5/1.10.4
                                        module load intelpython/3.6.5
                                        module use -a /contrib/met/modulefiles
                                        module load met/9.0

	To manually load METplus on NOAA Hera:
		*Load the modules:	module load intel/18.0.5.274
                                        module use -a /contrib/anaconda/modulefiles
                                        module load anaconda/latest
                                        module use -a /contrib/met/modulefiles
                                        module load met/9.0_anacondal

5. Install Python 3 Anaconda [MANDATORY FOR NON-NOAA MACHINES]
	To download the source code, go to the Python web site.
        Version 3.7.3 has been well-tested for GPLOT:
		https://www.python.org/downloads/release/python-373/

        An Anaconda build is maintained on NOAA Jet and NOAA Hera. We encourage
        that this build be used. Anacoda is automatically loaded as part of the
        GPLOT environment.

        To manually load the envirnoment used for GPLOT on NOAA Jet:
                csh/tcsh --> source ${GPLOT_DIR}/modulefiles/modulefile.gplot.jet 1
                bash/sh  --> source ${GPLOT_DIR}/modulefiles/modulefile.gplot.jet 1

        To manually load the envirnoment used for GPLOT on NOAA Hera:
                csh/tcsh --> source ${GPLOT_DIR}/modulefiles/modulefile.gplot.hera 1
                bash/sh  --> source ${GPLOT_DIR}/modulefiles/modulefile.gplot.hera 1

        If the centroid module is not working, it can be reproduced by following
        these steps:
		cd ${GPLOT_DIR}/python/modules
		python -m numpy.f2py -c ${GPLOT_DIR}/fortran/centroid.f90 -m centroid

        If you would like to install your our Anaconda build on NOAA RDHPCS systems (not recommended)
        (e.g., Jet, Hera), follow these instructions to build:
                https://rdhpcs-common-docs.rdhpcs.noaa.gov/wiki/index.php/Anaconda






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

