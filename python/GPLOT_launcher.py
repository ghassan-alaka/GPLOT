#!python

# Import necessary modules
import os
import sys
import subprocess
import f90nml
import shutil
import database as db
import namelist as nm
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


class Directories:

    def __init__(self):

        # GPLOT_DIR must be set in the environment
        self.GPLOT_DIR = self.gplot_check()

        # Create variables for GPLOT subdirectories.
        self.NMLDIR=self.GPLOT_DIR+"/nmlist/"
        self.BATCHDIR=self.GPLOT_DIR+"/batch/"
        self.LOGDIR=self.GPLOT_DIR+"/log/"


    def gplot_check(self,gvar='GPLOT_DIR'):
        """Check if the GPLOT_DIR variable is defined in the user environment.
        @param gvar: the environmental variable, default is 'GPLOT_DIR'
        """
        GPLOT_DIR = os.environ[gvar]
        if not GPLOT_DIR:
            GPLOT_DIR = "/home/"+os.environ['USER']+"/GPLOT"
            print("WARNING: Variable GPLOT_DIR not found in environment.")
            print("MSG: Setting GPLOT_DIR --> "+GPLOT_DIR)
        else:
            print("MSG: GPLOT_DIR found in environment --> "+GPLOT_DIR)
    
        if not os.path.exists(GPLOT_DIR):
            print("ERROR: GPLOT_DIR not found. Can't continue.")
            sys.exit(2)
    
        return(GPLOT_DIR);
    

def spawn_prep(MOD,EXPT,SFILE,LOGDIR,CPU_ACCT,PARTITION,QOS):
    """Prepare the slurm header for a given spawn file
    @param MOD:       the GPLOT module ID
    @param EXPT:      the experiment name, typically from the master namelist file name
    @param SFILE:     the spawn file full path specific to the current experiment
    @param LOGDIR:    the GPLOT log directory
    @param CPU_ACCT:  the batch cpu account for the runs
    @param PARTITION: the batch partition(s) for the runs
    @param QOS:       the quality of service for the runs
    """
    subprocess.call(["sed -i 's/^#SBATCH --job-name=.*/#SBATCH --job-name=\"GPLOT.spawn_"+MOD+"."+EXPT+"\"/g' "+SFILE], shell=True)
    subprocess.call(["sed -i 's%^#SBATCH --output=.*%#SBATCH --output=\""+LOGDIR+"spawn_"+MOD+"."+EXPT+".out\"%g' "+SFILE], shell=True)
    subprocess.call(["sed -i 's%^#SBATCH --error=.*%#SBATCH --error=\""+LOGDIR+"spawn_"+MOD+"."+EXPT+".err\"%g' "+SFILE], shell=True)
    subprocess.call(["sed -i 's/^#SBATCH --account=.*/#SBATCH --account="+CPU_ACCT+"/g' "+SFILE], shell=True)
    subprocess.call(["sed -i 's/^#SBATCH --partition=.*/#SBATCH --partition="+PARTITION+"/g' "+SFILE], shell=True)
    subprocess.call(["sed -i 's/^#SBATCH --qos=.*/#SBATCH --qos="+QOS+"/g' "+SFILE], shell=True)


def exec_subprocess(shchk=True,*args):
    """Call a shell subprocess.
    @param shchk: check if the subprocess.call shell option should be True/False
    @param args:  at least 1 string containing information on what to run
    """
    # Check that at least 1 argument has been supplied.
    if len(args[:]) < 1:
        print("ERROR: subprocess needs at least 1 argument")
        sys.exit(2)

    # Run the subprocess according to the shchk option.
    if shchk:
        subprocess.call([args[0]], shell=True)
    else:
        subprocess.call([','.join(args[:])], shell=False)



def db_create_table(NML,EXPT):
    """Create a database table for the namelist.
    @param NML: the namelist dictionary
    @param EXPT: a string of the experiment name
    """
    DB_FILE = NML.GPLOT_DIR+"/db/pysqlite."+EXPT+".db"
    conn = db.create_connection(DB_FILE)


def parse_input_args(ARGS):
    """A simple parser that is mainly used for python input
       arguments not counting the file name.
    @param ARGS: usually sys.argv
    @return ARG2: filtered list of arguments
    """
    # Check that at least one additional argument is present.
    if len(ARGS) < 2:
        print("ERROR: Need at least 2 arguments.")
        sys.exit(2)

    # Remove the first index, which is simply the file name
    ARGS2= ARGS[1:]
    return(ARGS2); 



def main():

    # Get directories
    DIRS = Directories()
    

    # Get the input arguments as independent experiments.    
    EXPTS = parse_input_args(sys.argv)
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
            shutil.copy2(SPAWNFILE1,SPAWNFILE2)
            if NML.BATCH_MODE.lower() == 'sbatch':
                spawn_prep(MOD,EXPT,SPAWNFILE2,DIRS.LOGDIR,NML.CPU_ACCT,NML.PARTITION,NML.QOS)
                print("MSG: Submitting "+SPAWNFILE2+" to the slurm batch scheduler.")
                print("sbatch "+SPAWNFILE2+" "+NML.FILE+" > "+SPAWNLOG)
                exec_subprocess(True,"sbatch "+SPAWNFILE2+" "+NML.FILE+" > "+SPAWNLOG)
            elif BATCH_MODE.lower() == 'foreground':
                print("MSG: Submitting "+SPAWNFILE2+" to the foreground.")
                exec_subprocess(True,SPAWNFILE2+" "+NML.FILE+" > "+SPAWNLOG)
            else:
                print("MSG: Submitting "+SPAWNFILE2+" to the background.")
                exec_subprocess(True,SPAWNFILE2+" "+NML.FILE+" > "+SPAWNLOG+" &")
        
        



if __name__ == '__main__':
    main()
