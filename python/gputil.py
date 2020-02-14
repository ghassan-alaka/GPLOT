#!python

# Import necessary modules
import os
import shutil
import sys
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
def file_copy(SRC,DEST,overwrite=True,quiet=True):
    """Ccopy a file in the system.
    @param SRC:       the source file path
    @param DEST:      the destination file path
    @param overwrite: logical to determine if DEST is to be
                      overwritten if it exists.
    @param quiet:     logical to determine print statements
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

    if not quiet:
        print("MSG: Successfully copied "+SRC+" --> "+DEST)

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
