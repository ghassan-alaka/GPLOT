#!/usr/bin/env python

# Import necessary modules
#import f90nml
import numpy as np
import os
import re
import shutil
import sys

# Import GPLOT modules
import gp_database as db
import gp_execute as xc
import gp_util as gpu
import gp_namelist as nm
from gp_util import Directories
from gp_namelist import Namelist_Launch
from gp_namelist import Namelist_Retrieve

"""
GPLOT Driver launcher
Main driver to launch GPLOT child processes for one or more experiments.

GPLOT is the Graphical Post-processed Locus for Output of Tropical cyclones.
It consists of independent modules used to create graphics targeted toward
presentations, posters, publications, and basic research related to tropical
cyclones. To learn more, please download the code from GitHub:
  https://github.com/ghassan-alaka/GPLOT

To learn more about the AOML/Hurricane Research Division, please visit:
  https://www.aoml.noaa.gov/our-research/hurricane-research-division/

Created By:    Ghassan Alaka Jr.
Assisted By:   
Date Created:  Febrary 6, 2020
Date Modified: February 21, 2020

Example call: python GPLOT_launcher.py <EXPT1> <EXPT2> ...

Modification Log:
2020-03-11 -- GJA added configuration file parsing for namelist options.
              GJA created a new GPLOT table file 'DBInfo.dat' to store the database file for each experiment.

"""

__version__ = '0.2.0';



###########################################################
def submit_spawn(N):
    """This subroutine handles the GPLOT spawn script submission
       for each module.
    @param N: pyGPLOT Namelist_Launch object
    """

    if len(N.MOD_ID) < 1:
        print("ERROR: At least one GPLOT module should be loaded.")

    # Loop over GPLOT modules
    print("MSG: The number of GPLOT modules = "+str(len(N.MOD_ID)))
    for MOD in N.MOD_ID:
        print("MSG: Working on submission for GPLOT Module "+MOD.upper())

        # Define some key file names
        SPAWNFILE1 = N.BATCHDIR+"GPLOT_spawn.generic.py"
        SPAWNFILE2 = N.BATCHDIR+"GPLOT_spawn."+MOD+"."+N.EXPT+".py"
        SPAWNLOG = N.LOGDIR+"GPLOT_spawn."+MOD+"."+N.EXPT+".log"

        # Copy the template spawn file to modify for this case
        gpu.file_copy(SPAWNFILE1,SPAWNFILE2,execute=True)

        # Decide what to run
        N.BATCH_MODE = 'foreground'
        PRE = xc.src_mods_pre(N.GPLOT_DIR)
        CMD_BASE = SPAWNFILE2+" "+N.EXPT+" "+N.GPOUT+" "+MOD
        if N.BATCH_MODE.lower() == 'sbatch':
            xc.spawn_prep(MOD,N.EXPT,SPAWNFILE2,N.LOGDIR,N.CPU_ACCT,N.PARTITION,N.QOS,RM_LOGS=True)
            id = xc.get_slurm_jobid('GPLOT_spawn.'+MOD+'.'+N.EXPT)
            if id is not None:
                print("MSG: Batch job(s) already exist(s) --> "+' '.join(id))
                print("MSG: Not submitting anything for "+MOD.upper()+".")
            else:
                print("MSG: Submitting "+SPAWNFILE2+" to the slurm batch scheduler.")
                print(PRE+"sbatch "+CMD_BASE+" > "+SPAWNLOG)
                xc.exec_subprocess(PRE+"sbatch "+CMD_BASE, GDIR=N.GPLOT_DIR)

        elif N.BATCH_MODE.lower() == 'foreground':
            print("MSG: Submitting "+SPAWNFILE2+" to the foreground.")
            print("MSG: "+PRE+CMD_BASE+" > "+SPAWNLOG)
            xc.exec_subprocess(PRE+CMD_BASE+" > "+SPAWNLOG, GDIR=N.GPLOT_DIR)

        elif N.BATCH_MODE.lower() == 'background':
            print("MSG: Submitting "+SPAWNFILE2+" to the background.")
            print("MSG: "+PRE+CMD_BASE+" > "+SPAWNLOG+" &")
            xc.exec_subprocess(PRE+CMD_BASE+" > "+SPAWNLOG+" &", GDIR=N.GPLOT_DIR)
            #xc.exec_subprocess("echo $GPLOT_DIR ; python -V > "+SPAWNLOG)

        else:
            print("MSG: Defaulting to a background job. Submitting "+SPAWNFILE2+".")
            print("MSG: "+PRE+CMD_BASE+" > "+SPAWNLOG+" &")
            xc.exec_subprocess(PRE+CMD_BASE+" > "+SPAWNLOG+" &", GDIR=N.GPLOT_DIR)



###########################################################
def main():

    # Get directories
    DIRS = Directories()

    # Parse inpurt arguments
    LARGS = gpu.parse_launch_args()
    EXPTS = LARGS.expt.split(',')
    print("MSG: Found these experiments --> "+repr(EXPTS))

    # Looping over all experiments. Could be one.
    for EXPT in EXPTS:
        print("MSG: Working on this experiment --> "+EXPT)

        # Launch the master namelist and write it to an SQLite database table, if required.
        if LARGS.delete_table:
            # Launch the namelist. Read the master namelist from a text file.
            CONFS = ['gp_master_default.conf','gp_master_'+EXPT+'.conf']
            if LARGS.config is not None:
                CONFS = CONFS + LARGS.config.split(',')
            NML = Namelist_Launch(DIRS,EXPT,CONFS)

            # Create the output directory
            gpu.dir_create(NML.GPOUT)

            # Create (or re-create) the database table 'namelist'
            NML.nml_db_initialize(destroy=True)

        # Retrieve the namelist from the database
        else:

            # Read the database file name from the GPLOT table
            DB_FILE = db.db_retrieve(DIRS.TBLDIR+'/DBInfo.dat',EXPT)

            # Retrieve the master namelist from the database file
            NML = Namelist_Retrieve(DIRS,EXPT,DB_FILE)

        if LARGS.verbose:
            print(db.select_rows('namelist_master','*',DFILE=NML.DB_FILE))

        # Loop over all MOD_IDs and submit spawn jobs.
        if LARGS.spawn:
            submit_spawn(NML)

        print("MSG: Done working on this experiment --> "+EXPT)



###########################################################
if __name__ == '__main__':
    main()
