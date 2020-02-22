#!/usr/bin/env python

# Import necessary modules
import os, shutil, stat, sys
from random import randint
#import sqlite3
#from sqlite3 import Error

"""
Package util: Python functionality for general GPLOT utilities.

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

Example call: python ./GPLOT_util.py

Modification Log:


"""

__version__ = '0.1.0';



###########################################################
class Directories:
    """A GPLOT class that assigns important directories.
       Most importantly, this class defined GPLOT_DIR.
    """

    def __init__(self):

        # GPLOT_DIR must be set in the environment
        self.GPLOT_DIR = self.gplot_check()

        # Create variables for GPLOT subdirectories.
        self.NMLDIR=self.GPLOT_DIR+"/nmlist/"
        self.BATCHDIR=self.GPLOT_DIR+"/batch/"
        self.LOGDIR=self.GPLOT_DIR+"/log/"
        self.TBLDIR=self.GPLOT_DIR+"/tbl/"
        self.PYDIR=self.GPLOT_DIR+"/python/"
        self.NCLDIR=self.GPLOT_DIR+"/ncl/"


    def gplot_check(self,gvar='GPLOT_DIR'):
        """Check if the GPLOT_DIR variable is defined in the user environment.
        @param gvar:       the environmental variable, default is 'GPLOT_DIR'
        @return GPLOT_DIR: the GPLOT main directory
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



###########################################################
def convert_boolean(V,quiet=True):
    """Convert input to boolean values if it matches the list.
    @param V:     the input value
    @kwarg quiet: logical to determine print statements
    @return V2:   the boolean, or the original input value
    """

    
    if V.lower() in ['true', '1', 't', 'y', 'yes', 'yeah', 'yup', 'certainly', 'uh-huh']:
        V2 = True
    elif V.lower() in ['false', '0', 'f', 'n', 'no', 'nah', 'nope']:
        V2 = False
    elif not quiet:
        print("MSG: Did not convert to boolean because no match with acceptable values.")
        V2 = V
    return(V2);





###########################################################
def dir_create(DIR,quiet=True):
    """Create a directory
    @param DIR:   the directory to be created
    @kwarg quiet: logical to determine print statements
    """
    if not os.path.exists(DIR):
        os.mkdir(DIR)
        if not quiet:
            print("MSG: Successfully created directory --> "+DIR)
    elif not quiet:
         print("WARNING: Directory already exists --> "+DIR)
    return;



###########################################################
def dir_remove(DIR,quiet=True):
    """Create a directory
    @param DIR:   the directory to be created
    @kwarg quiet: logical to determine print statements
    """
    if os.path.exists(DIR):
        shutil.rmtree(DIR)
        if not quiet:
            print("MSG: Successfully removed directory --> "+DIR)
    elif not quiet:
        print("WARNING: Directory does not exist --> "+DIR)
    return;



###########################################################
def file_copy(SRC,DEST,overwrite=True,quiet=True,execute=False):
    """Ccopy a file in the system.
    @param SRC:       the source file path
    @param DEST:      the destination file path
    @kwarg overwrite: logical to determine if DEST is to be
                      overwritten if it exists.
    @kwarg quiet:     logical to determine print statements
    @kwarg execute:   logical for giving copied file execute permissions
    """

    # Check if the destination file exists
    if os.path.exists(DEST):
        if overwrite:
            os.remove(DEST)
            shutil.copy(SRC,DEST)
        else:
            print("WARNING: Destination file already exists --> "+DEST)
            print("WARNING: Please select overwrite=True to avoid this.")
            print("WARNING: Will continue, but success is questionable.")
            #sys.exit(2)
            return;
    else:
        shutil.copy2(SRC,DEST)

    if execute:
        os.chmod(DEST,stat.S_IRWXU)

    if not quiet:
        print("MSG: Successfully copied "+SRC+" --> "+DEST)
        if execute:
            print("MSG: "+DEST+" now has execute permissions.")

    return;



###########################################################
def file_remove(FILE,quiet=True):
    """
    @param FILE:  the file (full path) to be removed
    @kwarg quiet: logical to determine print statements
    """
    if os.path.exists(FILE):
        os.remove(FILE)
    if not quiet:
        print("MSG: Successfully removed "+FILE)
    return;



###########################################################
def parse_input_args(ARGS):
    """A simple parser that is mainly used for python input
       arguments not counting the file name.
    @param ARGS:  usually sys.argv
    @return ARG2: filtered list of arguments
    """

    # Check that at least one additional argument is present.
    if len(ARGS) < 2:
        print("ERROR: Need at least 2 arguments.")
        sys.exit(2)

    # Remove the first index, which is simply the file name.
    ARGS2= ARGS[1:]

    # Return the truncated arguments list.
    return(ARGS2);



###########################################################
def random_N_digits(n):
    """Generate a random number with at least n digits
    From here: https://stackoverflow.com/questions/2673385/how-to-generate-random-number-with-the-specific-length-in-python
    @param n: the number of digits
    @return:  the random number
    """
    start = 10**(n-1)
    finish = (10**n)-1
    return randint(start,finish)
