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

Created By:    Ghassan Alaka, Jr.
Modified By:   Ghassan Alaka, Jr.
Date Created:  Febrary 6, 2020
Last Modified: March 12, 2020

Example call: python GPLOT_launcher.py <EXPT1> <EXPT2> ...

Modification Log:
2020-Mar-11 -- GJA added configuration file parsing for namelist options.
              GJA created a new GPLOT table file 'DBInfo.dat' to store the database file for each experiment.
2020-Mar-12 -- GJA streamlined the submit_spawn() function. Now, the generic spawn file is used for 
                background and foreground submissions.
"""

__version__ = '0.2.1';



###########################################################
def submit_spawn(N):
    """This subroutine handles the GPLOT spawn script submission
       for each module.
    @param N: pyGPLOT Namelist_Launch object
    """

    if len(N.MOD_ID) < 1:
        print("ERROR: At least one GPLOT module should be loaded.")
        sys.exit(2)

    # Loop over GPLOT modules
    print("MSG: The number of GPLOT modules = "+str(len(N.MOD_ID)))
    for MOD in N.MOD_ID:
        print("MSG: Working on submission for GPLOT Module "+MOD.upper())

        # Define spawn-specific variables:
        #   SPAWNJOB   --> the job name, also used to create files
        #   SPAWNFILE1 --> the generic GPLOT spawn file.
        #   SPAWNLOG   --> the log file for this specific experiment & module
        SPAWNJOB = "GPLOT_spawn."+MOD+"."+N.EXPT
        SPAWNFILE1 = N.BATCHDIR+"GPLOT_spawn.generic.py"
        SPAWNLOG = N.LOGDIR+SPAWNJOB+".log"

        # Define the command prefix, typically the modulefile to source
        N.BATCH_MODE = 'foreground'		# Testing
        PRE = xc.src_mods_pre(N.GPLOT_DIR)

        # Build the base command with input arguments. The file name is appended later.
        CMD_BASE = " -e "+N.EXPT+" -o "+N.GPOUT+" -m "+MOD

        # DECIDE HOW TO SUBMIT, THEN SUBMIT!
        if N.BATCH_MODE.lower() == 'sbatch':

            # Define a spawn file specifically for this case, and
            # copy the template spawn file to modify for this case
            SPAWNFILE2 = N.BATCHDIR+SPAWNJOB+".py"
            gpu.file_copy(SPAWNFILE1,SPAWNFILE2,execute=True)
            CMD_BASE = SPAWNFILE2+CMD_BASE

            # Modify the SBATCH header
            xc.spawn_prep(MOD,N.EXPT,SPAWNFILE2,N.LOGDIR,N.CPU_ACCT,N.PARTITION,N.QOS,RM_LOGS=True)

            # Get the Slurm Job ID for any matching jobs
            id = xc.get_slurm_jobid(SPAWNJOB)

            # Submit the job unless a matching job was found.
            if id is not None:
                print("MSG: Batch job(s) already exist(s) --> "+' '.join(id))
                print("MSG: Not submitting anything for "+MOD.upper()+".")
            else:
                print("MSG: Submitting "+SPAWNFILE2+" to the slurm batch scheduler.")
                print(PRE+"sbatch "+CMD_BASE+" > "+SPAWNLOG)
                xc.exec_subprocess(PRE+"sbatch "+CMD_BASE, GDIR=N.GPLOT_DIR)

        elif N.BATCH_MODE.lower() == 'foreground':
            CMD_BASE = SPAWNFILE1+CMD_BASE
            print("MSG: Submitting "+SPAWNFILE1+" to the foreground.")
            print("MSG: "+PRE+CMD_BASE+" > "+SPAWNLOG)
            xc.exec_subprocess(PRE+CMD_BASE+" > "+SPAWNLOG, GDIR=N.GPLOT_DIR)

        elif N.BATCH_MODE.lower() == 'background':
            CMD_BASE = SPAWNFILE1+CMD_BASE
            print("MSG: Submitting "+SPAWNFILE1+" to the background.")
            print("MSG: "+PRE+CMD_BASE+" > "+SPAWNLOG+" &")
            xc.exec_subprocess(PRE+CMD_BASE+" > "+SPAWNLOG+" &", GDIR=N.GPLOT_DIR)
            #xc.exec_subprocess("echo $GPLOT_DIR ; python -V > "+SPAWNLOG)

        else:
            CMD_BASE = SPAWNFILE1+CMD_BASE
            print("MSG: Defaulting to a background job. Submitting "+SPAWNFILE1+".")
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

        # Get the database file name from the GPLOT table
        DB_FILE = db.db_retrieve(DIRS.TBLDIR+'/DBInfo.dat',EXPT)

        # Launch the master namelist and write it to an SQLite database table, if required.
        if DB_FILE is None or LARGS.delete_table:

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
