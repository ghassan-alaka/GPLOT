#!/usr/bin/env python

# Import necessary modules
import os, subprocess, sys
import numpy as np
import gp_log as log
import gp_util as gpu
from gp_log import Main_Logger

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

Created By:    Ghassan Alaka Jr.
Modified By:   Ghassan Alaka Jr.
Date Created:  February 14, 2020
Last Modified: March 19, 2020

Example call: For Internal Calls Only

Modification Log:
2020-Feb-27 -- GJA changed the file name from execute.py to gp_execute.py
2020-Mar-19 -- GJA added logging capabilities
"""

__version__ = '0.2.1';



###########################################################
def exec_subprocess(*args,GDIR=None,**kwargs):
    """Call a shell subprocess.
    @param args: at least 1 string containing information on what to run
    @kwarg GDIR: the GPLOT source directory,
                 if it is to be changed in the environment
    @kwargs:     extra keywords are allowed
    """

    # Get the logger object
    L = kwargs.get('logger',Main_Logger('Execute'))

    # Check that at least 1 argument has been supplied.
    if len(args[:]) < 1:
        L.logger.error("Subprocess needs at least 1 argument")
        sys.exit(2)

    # If necessary, assign the new GPLOT_DIR to the environment
    # to be passed to the shell subprocess
    MY_ENV = os.environ.copy()
    if GDIR is not None:  MY_ENV["GPLOT_DIR"] = GDIR

    # Run the subprocess according to the shchk option.
    if len(args) == 1:
        print(args[0])
        subprocess.call(args, shell=True, executable='/bin/bash', env=MY_ENV)
    else:
        subprocess.call(args, shell=False)



###########################################################
def exec_subprocess_check(*args,**kwargs):
    """Call a shell subprocess and capture the result
    @args:     at least 1 string containing information on what to run
    @kwargs:   extra keyword arguments are allowed
    @return R: the output of the shell command
    """

    # Get the logger object
    L = kwargs.get('logger',Main_Logger('Execute'))

    # Check that at least 1 argument has been supplied.
    if len(args[:]) < 1:
        L.logger.error("Subprocess needs at least 1 argument")
        sys.exit(2)

    # Run the shell process
    R = subprocess.check_output(args[0], shell=True).decode('utf-8')

    return(R);



###########################################################
def find_pre_job(GDIR):
    """Return the pre job shell script.
    @param GDIR: the GPLOT source directory
    """
    return(GDIR+'/shell/GPLOT_pre_job.sh');


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
def slurm_prep(BFILE,LOGDIR,MOD=None,EXPT=None,CPU_ACCT=None,PARTITION=None,QOS=None,JNAME=None,RM_LOGS=False,**kwargs):
    """Prepare the slurm header for a given batch file
    @param BFILE:     the batch file full path specific to the current experiment
    @param LOGDIR:    the GPLOT log directory
    @kwarg MOD:       the GPLOT module ID
    @kwarg EXPT:      the experiment name, typically from the master namelist file name
    @kwarg CPU_ACCT:  the batch cpu account for the runs
    @kwarg PARTITION: the batch partition(s) for the runs
    @kwarg QOS:       the quality of service for the runs
    @kwarg JNAME:      the name of the GPLOT driver (e.g., spawn, bundle)
    @kwarg RM_LOGS:   logical to remove the batch log files
    @kwargs:          extra keywords are allowed
    """

    # Get the logger object
    L = kwargs.get('logger',Main_Logger('Execute'))

    # Define the log files
    if JNAME is None:
        try:
            JNAME = "GPLOT_unknown."+MOD+"."+EXPT
        except:
            L.logger.error("MOD and EXPT can't be Nonetype is JNAME is Nonetype.")

    # Check that LOGDIR exists
    if os.path.isdir(LOGDIR):
        LOUT = LOGDIR+JNAME+".log"
        LERR = LOGDIR+JNAME+".log"
    else:
        L.logger.error("LOGDIR must be a real directory.")
        sys.exit(2)

    # Make changes to the spawn batch header in a shell process
    with open(BFILE) as f:
        if '#SBATCH' in f.read():
            exec_subprocess(f"sed -i 's/^#SBATCH --job-name=.*/#SBATCH --job-name=\"{JNAME}\"/g' {BFILE}")
            exec_subprocess(f"sed -i 's%^#SBATCH --output=.*%#SBATCH --output=\"{LOUT}\"%g' {BFILE}")
            exec_subprocess(f"sed -i 's%^#SBATCH --error=.*%#SBATCH --error=\"{LERR}\"%g' {BFILE}")
            if CPU_ACCT is not None:   exec_subprocess(f"sed -i 's/^#SBATCH --account=.*/#SBATCH --account={CPU_ACCT}/g' {BFILE}")
            if PARTITION is not None:  exec_subprocess(f"sed -i 's/^#SBATCH --partition=.*/#SBATCH --partition={PARTITION}/g' {BFILE}")
            if QOS is not None:        exec_subprocess(f"sed -i 's/^#SBATCH --qos=.*/#SBATCH --qos={QOS}/g' {BFILE}")
        else:
            L.logger.error(f"Could not find any #SBATCH entries in {BFILE}")
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
    PRE = GDIR+"/modulefiles/GPLOT_mods"
    return(PRE);
