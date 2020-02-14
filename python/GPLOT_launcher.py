#!python

# Import necessary modules
import f90nml
import os
import shutil
import sys

# Import GPLOT modules
import database as db
import execute as xc
import gputil
import namelist as nm
from gputil import Directories
from namelist import Namelist_Basic

"""
GPLOT Wrapper: Main submission script for all GPLOT child processes for a
               set of experiments.

GPLOT is the Graphical Post-processed Locus for Output of Tropical cyclones.
It consists of independent modules used to create graphics targeted toward
presentations, posters, publications, and basic research related to tropical
cyclones. To learn more, please download the code from GitHub:
  https://github.com/ghassan-alaka/GPLOT

To learn more about the AOML/Hurricane Research Division, please visit:
  https://www.aoml.noaa.gov/our-research/hurricane-research-division/

Created By:   Ghassan Alaka Jr.
Assisted By:
Date Created: Febrary 6, 2020

Example call: python ./GPLOT_launcher.py

Modification Log:


"""

__version__ = '0.1.0';




def db_create_table(NML,EXPT):
    """Create a database table for the namelist.
    @param NML: the namelist dictionary
    @param EXPT: a string of the experiment name
    """
    DB_FILE = NML.GPLOT_DIR+"/db/pysqlite."+EXPT+".db"
    conn = db.create_connection(DB_FILE)



def main():

    # Get directories
    DIRS = Directories()
    

    # Get the input arguments as independent experiments.    
    EXPTS = gputil.parse_input_args(sys.argv)
    print("MSG: Found these experiments --> "+repr(EXPTS))


    # Looping over all experiments. Could be one.
    for EXPT in EXPTS:

        # Get basic information about the namelist.
        # This class is imported from namelist.py
        NML = Namelist_Basic(DIRS.NMLDIR,EXPT)


        # Save namelist to an SQLite database
        #db_table(NML,EXPT)
        
        
        # Loop over all MOD_IDs and submit spawn jobs.
        for MOD in NML.MOD_ID:
            print("MSG: Working on submission for GPLOT Module "+MOD.upper())
            SPAWNFILE1 = DIRS.BATCHDIR+"spawn_"+MOD+".generic.sh"
            SPAWNFILE2 = DIRS.BATCHDIR+"spawn_"+MOD+"."+EXPT+".sh"
            SPAWNLOG = DIRS.LOGDIR+"spawn_"+MOD+"."+EXPT+".log"
            #continue
            gputil.file_copy(SPAWNFILE1,SPAWNFILE2)
            if NML.BATCH_MODE.lower() == 'sbatch':
                xc.spawn_prep(MOD,EXPT,SPAWNFILE2,DIRS.LOGDIR,NML.CPU_ACCT,NML.PARTITION,NML.QOS)
                print("MSG: Submitting "+SPAWNFILE2+" to the slurm batch scheduler.")
                print("sbatch "+SPAWNFILE2+" "+NML.FILE+" > "+SPAWNLOG)
                xc.exec_subprocess(True,"sbatch "+SPAWNFILE2+" "+NML.FILE+" > "+SPAWNLOG)
            elif BATCH_MODE.lower() == 'foreground':
                print("MSG: Submitting "+SPAWNFILE2+" to the foreground.")
                xc.exec_subprocess(True,SPAWNFILE2+" "+NML.FILE+" > "+SPAWNLOG)
            else:
                print("MSG: Submitting "+SPAWNFILE2+" to the background.")
                xc.exec_subprocess(True,SPAWNFILE2+" "+NML.FILE+" > "+SPAWNLOG+" &")
        
        



if __name__ == '__main__':
    main()
