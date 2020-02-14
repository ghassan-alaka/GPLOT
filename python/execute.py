#!python

# Import necessary modules
import os
import subprocess
import sys
#import sqlite3
#from sqlite3 import Error

"""
Package execute: Python functionality for general GPLOT utilities.

GPLOT is the Graphical Post-processed Locus for Output of Tropical cyclones.
It consists of independent modules used to create graphics targeted toward
presentations, posters, publications, and basic research related to tropical
cyclones. To learn more, please download the code from GitHub:
  https://github.com/ghassan-alaka/GPLOT

To learn more about the AOML/Hurricane Research Division, please visit:
  https://www.aoml.noaa.gov/our-research/hurricane-research-division/

Created By:   Ghassan Alaka Jr.
Assisted By:
Date Created: Febrary 14, 2020

Example call: python ./execute.py

Modification Log:


"""

__version__ = '0.1.0';



###########################################################
def exec_subprocess(shchk=True,*args):
    """Call a shell subprocess.
    @param shchk: check if the subprocess.call shell option
                  should be True/False
    @param args:  at least 1 string containing information
                  on what to run
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



###########################################################
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



