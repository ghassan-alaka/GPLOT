#!/usr/bin/env python

# Import necessary modules
import os, subprocess, sys
import numpy as np
import gputil as gpu

"""
GPLOT Package execute
Python functionality for executables, including running commands in a shell.

GPLOT is the Graphical Post-processed Locus for Output of Tropical cyclones.
It consists of independent modules used to create graphics targeted toward
presentations, posters, publications, and basic research related to tropical
cyclones. To learn more, please download the code from GitHub:
  https://github.com/ghassan-alaka/GPLOT

To learn more about the AOML/Hurricane Research Division, please visit:
  https://www.aoml.noaa.gov/our-research/hurricane-research-division/

Created By:   Ghassan Alaka Jr.
Assisted By:
Date Created:  February 14, 2020
Date Modified: February 21, 2020

Example call: INTERNAL CALLS ONLY

Modification Log:


"""

__version__ = '0.1.0';



###########################################################
def exec_subprocess(*args,GDIR=None):
    """Call a shell subprocess.
    @param args: at least 1 string containing information on what to run
    @kwarg GDIR: the GPLOT source directory,
                 if it is to be changed in the environment
    """
    # Check that at least 1 argument has been supplied.
    if len(args[:]) < 1:
        print("ERROR: subprocess needs at least 1 argument")
        sys.exit(2)

    # If necessary, assign the new GPLOT_DIR to the environment
    # to be passed to the shell subprocess
    MY_ENV = os.environ.copy()
    if GDIR is not None:
        MY_ENV["GPLOT_DIR"] = GDIR

    # Run the subprocess according to the shchk option.
    if len(args) == 1:
        subprocess.call(args, shell=True, executable='/bin/bash', env=MY_ENV)
    else:
        subprocess.call(args, shell=False)



###########################################################
def exec_subprocess_check(*args):
    """Call a shell subprocess and capture the result
    @param args: at least 1 string containing information on what to run
    @return R:   the output of the shell command
    """
    # Check that at least 1 argument has been supplied.
    if len(args[:]) < 1:
        print("ERROR: subprocess needs at least 1 argument")
        sys.exit(2)

    # Run the shell process
    #if args.count('squeue') >= 1:
    #    update_exec_name('squeue')
    #print(' '.join(args[:]))
    R = subprocess.check_output(args[0], shell=True).decode('utf-8')

    return(R);



###########################################################
def get_slurm_jobid(JOB):
    """Retrieve the slurm job identification
    @param JOB: the job id
    @return ID: the list of job ids or None
    """
    #DATA = np.reshape(np.array(list(filter(lambda x: x != '', re.split(' *|\n',DATA_RAW)))), (-1,2))
    DATA = np.reshape(np.array(exec_subprocess_check("squeue -u "+os.environ['USER']+" -o \"%.10i %.50j\"").split()), (-1,2))
    ID = None
    if any(JOB in s for s in DATA[:,1]):
        ID = DATA[[i for i, s in enumerate(DATA[:,1]) if JOB in s],0]
    return(ID);



###########################################################
def slurm_prep(MOD,EXPT,BFILE,LOGDIR,CPU_ACCT,PARTITION,QOS,NAME='spawn',RM_LOGS=False):
    """Prepare the slurm header for a given batch file
    @param MOD:       the GPLOT module ID
    @param EXPT:      the experiment name, typically from the master namelist file name
    @param BFILE:     the batch file full path specific to the current experiment
    @param LOGDIR:    the GPLOT log directory
    @param CPU_ACCT:  the batch cpu account for the runs
    @param PARTITION: the batch partition(s) for the runs
    @param QOS:       the quality of service for the runs
    @kwarg NAME:      the name of the GPLOT driver (e.g., spawn, bundle)
    @kwarg RM_LOGS:   logical to remove the batch log files
    """
    # Define the log files
    JNAME = "GPLOT_"+NAME+"."+MOD+"."+EXPT
    LOUT = LOGDIR+JNAME+".log"
    LERR = LOGDIR+JNAME+".err"

    # Make changes to the spawn batch header in a shell process
    if exec_subprocess_check(["grep -q '#SBATCH' "+BFILE]):
        exec_subprocess(["sed -i 's/^#SBATCH --job-name=.*/#SBATCH --job-name=\""+JNAME+"\"/g' "+BFILE])
        exec_subprocess(["sed -i 's%^#SBATCH --output=.*%#SBATCH --output=\""+LOUT+"\"%g' "+BFILE])
        exec_subprocess(["sed -i 's%^#SBATCH --error=.*%#SBATCH --error=\""+LERR+"\"%g' "+BFILE])
        exec_subprocess(["sed -i 's/^#SBATCH --account=.*/#SBATCH --account="+CPU_ACCT+"/g' "+BFILE])
        exec_subprocess(["sed -i 's/^#SBATCH --partition=.*/#SBATCH --partition="+PARTITION+"/g' "+BFILE])
        exec_subprocess(["sed -i 's/^#SBATCH --qos=.*/#SBATCH --qos="+QOS+"/g' "+BFILE])
    else:
        print("ERROR: Could not find any #SBATCH entries in "+BFILE)
        sys.exit(2)

    # Remove logs, if required
    if RM_LOGS:
        gpu.file_remove(LOUT)
        gpu.file_remove(LERR)

    return;



###########################################################
def src_mods_pre(GDIR):
    """Return the command to load a particular file in the shell
    @param GDIR: the GPLOT source directory
    @return PRE: the command statement string ready to be prepended
    """
    PRE = ". "+GDIR+"/modulefiles/GPLOT_mods ; "
    return(PRE);
