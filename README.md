# GPLOT
# Graphical Post-processed Locus for Output of Tropical cyclones
# Created at NOAA's Hurricane Research Division.

[Git repository URL](https://github.com/ghassan-alaka/GPLOT.git):  https://github.com/ghassan-alaka/GPLOT.git


## Instructions for installation.

### 1. Add 'GPLOT_DIR' to environment [MANDATORY]
        Define GPLOT_DIR as the path where you cloned the GPLOT repository.
        It is typically defined automatically when loading the modulefile.


### 2. Check for the correct module file [MANDATORY]
        * Modulefiles are included for NOAA RDHPCS machines: Jet, Hera, and Orion.

        * By default, the SH versions are used in the GPLOT workflow.

        * For Jet/Hera, TCSH versions exist for testing, e.g.
```
		source ${GPLOT_DIR}/modulefiles/modulefile.gplot.jettcsh
		source ${GPLOT_DIR}/modulefiles/modulefile.gplot.heratcsh
```


### 3. Install NCL (NCAR Command Language) [MANDATORY FOR NON-NOAA MACHINES]
	* Pre-compiled libraries are preferred. To learn more, start at the [NCL Download Page](https://www.ncl.ucar.edu/Download/).

	* [NCL Version 6.6.2](https://www.earthsystemgrid.org/dataset/ncl.662.html) has been well-tested for GPLOT.

```
        NOTE:  The NCL project is "feature frozen" as of September 2019, meaning
               CISL has no plans to add new features to NCL. They will continue
               to maintain the code and will infrequently release bug fixes as
               well as user-contributed code.
```

	* NCL is automatically loaded as part of the GPLOT environment. To manually load NCL on NOAA's Jet/Hera/Orion:
```
		module load ncl/6.6.2
```

	* Customize .hluresfile:
          - NCL configuration options are stored in .hluresfile.
          - An example version of this file is stored in ncl/.hluresfile.example.
          - Copy it to your home directory, for example $HOME/.hluresfile.
          - 'wsMaximumSize' controls the memory available for NCL contour graphics.
          - You may add/change any value in .hluresfile, but only change 'wxMaximumSize' if you have good reason for doing so.


### 4. Install METplus (Model Evaluation Tools) [MANDATORY FOR NON-NOAA MACHINES]
	* To download the source code, go to the [DTC web site](https://dtcenter.org/community-code/metplus/download).

	* [METplus Version 9.0.2](https://dtcenter.org/sites/default/files/community-code/met/met-9.0.2.20200522.tar.gz) has been well-tested for GPLOT.

	METplus is automatically loaded as part of the GPLOT environment. To manually load METplus on NOAA Jet:
```
		module load intel/18.0.5.274
		module load netcdf/4.6.1
		module load hdf5/1.10.4
		module load intelpython/3.6.5
		module use -a /contrib/met/modulefiles
		module load met/9.0
```

	To manually load METplus on NOAA Hera:
```
		module load intel/18.0.5.274
		module use -a /contrib/anaconda/modulefiles
		module load anaconda/latest
		module use -a /contrib/met/modulefiles
		module load met/9.0_anacondal
```

### 5. Install Python 3 Anaconda [MANDATORY FOR NON-NOAA MACHINES]
	* To download the source code, go to the Python web site.

        * [Python Version 3.7.3](https://www.python.org/downloads/release/python-373/) has been well-tested for GPLOT.

        * An Anaconda build is maintained on NOAA Jet and NOAA Hera. We encourage that this build be used. Anacoda is automatically loaded as part of the GPLOT environment.

        * To manually load the envirnoment used for GPLOT on NOAA Jet:
```
		source ${GPLOT_DIR}/modulefiles/modulefile.gplot.jet 1
		source ${GPLOT_DIR}/modulefiles/modulefile.gplot.jettcsh 1
```

        * To manually load the envirnoment used for GPLOT on NOAA Hera:
```
                source ${GPLOT_DIR}/modulefiles/modulefile.gplot.hera 1
                source ${GPLOT_DIR}/modulefiles/modulefile.gplot.heratcsh 1
```

        * If the centroid module is not working, it can be reproduced by following these steps:
```
		source ${GPLOT_DIR}/modulefiles/modulefile.gplot.jet 1
		cd ${GPLOT_DIR}/python/modules
		python -m numpy.f2py -c ${GPLOT_DIR}/fortran/centroid.f90 -m centroid
```

        * If you would like to install your our Anaconda build on NOAA RDHPCS systems (not recommended), follow [these instructions](https://rdhpcs-common-docs.rdhpcs.noaa.gov/wiki/index.php/Anaconda) to build:






## Instructions to run

### 1. Set the namelist
	* Create a master namelist --> namelist.master.${EXPT}
	* It is most critical to decide which modules to run. Currently, 4 options:
          - Maps
	  - Stats
	  - Ships
	  - Polar

### 2. If the experiment is new, add it to the table:
	* ${GPLOT_DIR}/tbl/ExptInfo.dat

### 3. If the model is new, add it to the table:
	* ${GPLOT_DIR}/tbl/ModelInfo.dat

### 4. Run the shell wrapper script
	* ./shell/GPLOT_wrapper.sh ${GPLOT_DIR}/nmlist/namelist.master.${EXPT}
	* By providing ${EXPT} as an argument, the GPLOT wrapper can find the correct master namelist and run with user-requested options.


