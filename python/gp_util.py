#!/usr/bin/env python

# Import necessary modules
import os, re, shutil, stat, sys, time
import configargparse
import logging
import pandas as pd
from random import randint
#import sqlite3
#from sqlite3 import Error

import gp_log
from gp_log import Main_Logger

"""
GPLOT Package util
Python functionality for general GPLOT utilities.

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
Last Modified: March 18, 2020

Example call: For Internal Calls Only

Modification Log:
2020-Feb-27 -- GJA changed the file name from gputil.py to gp_util.py
2020-Mar-12 -- GJA modified parse_launch_args() to use ConfigArgParse
               GJA created parse_spawn_args() to parse arguments for GPLOT spawn jobs
               GJA expanded the functionality of convert_boolean() so that it can
                 convert strings and integers to boolean.
2020-Mar-18 -- GJA created time_since_modified() to print the file/directory age
                 in seconds. This was motivated by the need to track the age of
                 ATCF Files
"""

__version__ = '0.3.0';



###########################################################
class Directories:
    """A GPLOT class that assigns important directories.
       Most importantly, this class defined GPLOT_DIR.
    """

    def __init__(self,L=None):

        # Setup logging
        if L is not None:
            self.logger = L.logger
        else:
            self.logger = Main_Logger('Utilities').logger
            #self.logger = self.logger.logger

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
            self.logger.warning("Variable GPLOT_DIR not found in environment.")
            self.logger.info("Setting GPLOT_DIR --> "+GPLOT_DIR)
        else:
            self.logger.info("GPLOT_DIR found in environment --> "+GPLOT_DIR)

        if not os.path.exists(GPLOT_DIR):
            self.logger.error("GPLOT_DIR not found. Can't continue.")
            sys.exit(2)

        return(GPLOT_DIR);



###########################################################
def atcf_read(F,COLS=None,DT=None,FHR_S=None,FHR_E=None, \
              DATE_S=None,DATE_E=None,LAT_N=None,LAT_S=None, \
              LON_W=None,LON_E=None,MAX_V=None,MIN_V=None, \
              SID=None,SNAME=None,STYPE=None,AIDS=None, \
              BASIN=None,MAX_P=None,MIN_P=None,DROPDUP=True, \
              EXTRACT=False):
    """Read ATCF data files as a pandas DataFrame
    ATCF Documentation: http://www.nrlmry.navy.mil/atcf_web/docs/database/new/abdeck.txt
    GFDL Tracker Documentation: https://dtcenter.org/HurrWRF/users/downloads/Tracker_releases/V3.4a/stand_alone_tracker_UG_v3.4a.pdf
    @param F:       the ATCF file, could be combined
    @kwarg COLS:    the ATCF columns of interest
    @kwarg AIDS:    the forecast aids (e.g., models), typically 4 alphanumeric digits
    @kwarg FHR_S:   the starting lead time
    @kwarg FHR_E:   the ending lead time
    @kwarg DT:      the lead time interval
    @kwarg DATE_S:  the starting date
    @kwarg DATE_E:  the ending date
    @kwarg LAT_N:   the northernmost latitude
    @kwarg LAT_S:   the southernmost latitude
    @kwarg LON_W:   the westernmost longitude
    @kwarg LON_E    the easternmost longitude
    @kwarg MAX_V:   the maximum max intensity
    @kwarg MIN_V:   the minimum max intensity
    @kwarg MAX_P:   the maximum min pressure
    @kwarg MIN_P:   the minimum min pressure
    @kwarg SID:     the storm identification number
    @kwarg SNAME:   the storm name
    @kwarg STYPE    the storm classification type (e.g., HU,TS,TD,etc.)
    @kwarg BASIN:   the storm basin (e.g., AL,EP,etc.)
    @kwarg DROPDUP: logical that controls dropping duplicate entries
    """

    # Column names for ATCF files. All columns may not be used
    # and will be set to NaN.
    CNAME = ['BASIN','SID','YMDH','TECHNUM','AID','FHR','LAT','LON',\
             'VMAX','MSLP','CLAS','RAD','WINDCODE','RAD1','RAD2','RAD3',\
             'RAD4','POUTER','ROUTER','RMW','GUSTS','EYE','SUBREGION',\
             'MAXSEAS','INITIALS','DIR','SPEED','STORMNAME','DEPTH',\
             'SEAS','SEASCODE','SEAS1','SEAS2','SEAS3','SEAS4',\
             'THERMOPARAMS','CROSSTHICKASYM','THERMALWINDLO','THERMALWINDHI',\
             'WARMCORE','WARMCOREMAG','CONST1','CONST2']

    # Read in the data as a pandas dataframe
    DATA = pd.read_csv(F, header=None, names=CNAME)
    #print(DATA)

    # Drop duplicates
    if DROPDUP:
        DATA.drop_duplicates(subset='FHR', keep='first', inplace=True)
    #print(DATA)
    # Retain only requested AIDS (e.g., models)
    if AIDS is not None:
        DATA = DATA.loc[DATA['AID'].isin(AIDS)]

    # Retain only requested Storm IDs
    if SID is not None:
        DATA = DATA.loc[DATA['SID'].isin(SID)]

    # Retain only requested basins
    if BASIN is not None:
        DATA = DATA.loc[DATA['BASIN'].isin(BASIN)]

    # Retain only requested classifications
    if STYPE is not None:
        DATA = DATA.loc[DATA['CLAS'].isin(STYPE)]

    # Retain only requested Storm Names
    if SNAME is not None:
        DATA = DATA.loc[DATA['STORMNAME'].isin(SNAME)]

    # Set the lead time bounds
    if FHR_S is not None:
        DATA = DATA.loc[DATA['FHR'] >= int(FHR_S)]
    if FHR_E is not None:
        DATA = DATA.loc[DATA['FHR'] <= int(FHR_E)]

    # Set the lead time interval
    if DT is not None:
        DATA = DATA.loc[DATA['FHR']%DT == 0]

    # Set the date bounds
    if DATE_S is not None:
        DATA = DATA.loc[DATA['YMDH'] >= int(DATE_S)]
    if DATE_E is not None:
        DATA = DATA.loc[DATA['YMDH'] <= int(DATE_E)]

    # Set latitude bounds
    if LAT_S is not None:
        DATA = DATA.loc[DATA['LAT'] >= LAT_S]
    if LAT_N is not None:
        DATA = DATA.loc[DATA['LAT'] <= LAT_N]

    # Set latitude bounds
    if LON_W is not None:
        DATA = DATA.loc[DATA['LON'] >= LON_W]
    if LON_E is not None:
        DATA = DATA.loc[DATA['LON'] <= LON_E]

    # Set maximum intensity bounds
    if MAX_V is not None:
        DATA = DATA.loc[DATA['VMAX'] <= MAX_V]
    if MIN_V is not None:
        DATA = DATA.loc[DATA['VMAX'] >= MIN_V]

    # Set minimum pressure bounds
    if MAX_P is not None:
        DATA = DATA.loc[DATA['MSLP'] <= MAX_P]
    if MIN_P is not None:
        DATA = DATA.loc[DATA['MSLP'] >= MIN_P]

    # Retain requested columns.
    if COLS is not None:
        DATA = DATA[COLS]

    # Return data. Set EXTRACT=True to convert to numpy array.
    if EXTRACT:
        return(DATA.values);
    else:
        return(DATA);



###########################################################
def convert_boolean(V,quiet=True,do_int=False,**kwargs):
    """Convert input to boolean values if it matches the list.
    @param V:      the input value
    @kwarg do_int: logical to allow integer conversions
    @kwarg quiet:  logical to determine print statements
    @kwargs:       extra keywords are allowed
    @return V2:    the boolean, or the original input value
    Notes: BOOL_FAIL could become an attribute of V2. To do this,
           V2 must become a class. I'm not sure that level of
           complexity is warranted here.
    """

    # Create the logging object
    L = kwargs.get('logger',Main_Logger('Utilities'))

    BOOL_FAIL = False
    if V is not None:

        # Create lists of acceptable strings to convert
        str_true = ['true', 't', 'y', 'yes', 'yeah', 'yup', 'certainly', 'uh-huh']
        str_false = ['false', 'f', 'n', 'no', 'nah', 'nope']

        # Make sure V is a list
        if not isinstance(V,list):
            VV = [V]
            scalar = True
        else:
            VV = V
            scalar = False

        # Loop over all values in VV (could be 1)
        V2 = []
        for v in VV:

            # To convert string types
            if type(v) == str:
                if v.lower() in str_true:     V2.append(True)
                elif v.lower() in str_false:  V2.append(False)
                else:
                    BOOL_FAIL = True
                    V2.append(v)

            # To convert integer types (do_int = True)
            elif type(v) == int and do_int:
                if v == 1:    V2.append(True)
                elif v == 0:  V2.append(False)
                else:
                    BOOL_FAIL = True
                    V2.append(v)

            # To propagate a boolean input
            elif type(v) == bool:  V2.append(v)
 
            # For everything else, return the original value and set BOOL_FAIL=True
            else:
                BOOL_FAIL = True
                V2.append(v)

        # If scalar value, convert back from list
        if scalar:  V2 = V2[0]

    # If V is None, enter here.
    else:
        BOOL_FAIL = True
        V2 = None

    # Print some information, if required
    if BOOL_FAIL and not quiet:
        L.logger.warning("Did not convert at least 1 value to boolean because it was not an acceptable input.")

    # Return the boolean value(s)
    return(V2);



###########################################################
def df_create(D1,CN1):
    """Create a pandas DataFrame
    @param D1:  the input lists, each sublist corresponds to
                  a column in the DataFrame
    @param CN1: the column names in the DataFrame
    @return:    the pandas DataFrame
    """
    # Make sure the inputs are lists
    if type(D1) != list:  D1 = [D1]
    if type(CN1) != list: CN1 = [CN1]

    # Create a data dictionary to become the DataFrame
    DATA = {}
    for (D,C) in zip(D1,CN1):
        DATA.update( {C : D} )

    # Return the DataFrame
    return(pd.DataFrame(DATA));



###########################################################
def data_table_read(F,A=None,C=None,R=None,IS_BOOL=False):
    """ Read a data table file into a pandas dataframe.
    @param F:       the data file
    @kwarg A:       the column of interest
    @kwarg C:       the column to test
    @kwarg R:       the value within column C to match
    @kwarg IS_BOOL: logical for converting to boolean
    """

    # Read in the data as a pandas dataframe
    DATA = pd.read_csv(F, delim_whitespace=True)
    if A is not None and C is not None and R is not None:
        DATA = DATA.loc[DATA[C] == R][A].values[0]
    elif A is not None and C is None:
        DATA = list(DATA[A].values)
    if IS_BOOL:
        DATA = convert_boolean(DATA)
    return(DATA);




###########################################################
def dir_create(DIR,quiet=True,**kwargs):
    """Create a directory
    @param DIR:   the directory to be created
    @kwarg quiet: logical to determine print statements
    @kwargs:      extra keywords are allowed.
    """
    L = kwargs.get('logger',Main_Logger('Utilities'))
    if not os.path.exists(DIR):
        os.mkdir(DIR)
        if not quiet:
            L.logger.info("Successfully created directory --> "+DIR)
    elif not quiet:
         L.logger.warning("Directory already exists --> "+DIR)
    return;



###########################################################
def dir_find(DIR,EXP,FULL=False):
    """Find directories
    @param DIR: find matching directories under this one
    @param EXP: the regular expression to find directories
    @kwarg FULL: Full path returned if True
    @return L:  the list of matching directories
    """
    L = []
    rc = re.compile(EXP)
    for dpath, dnames, fnames in os.walk(DIR):
        if FULL:
            if dpath[-1] == '/':
                L = L + [dpath+dn for dn in dnames if rc.match(dn)]
            else:
                L = L + [dpath+'/'+dn for dn in dnames if rc.match(dn)]
        else:
            L = L + [dn for dn in dnames if rc.match(dn)]
    return(L);



###########################################################
def dir_remove(DIR,quiet=True,**kwargs):
    """Create a directory
    @param DIR:   the directory to be created
    @kwarg quiet: logical to determine print statements
    @kwargs:      extra keywords are allowed
    """
    L = kwargs.get('logger',Main_Logger('Utilities'))
    if os.path.exists(DIR):
        shutil.rmtree(DIR)
        if not quiet:
            L.logger.info("Successfully removed directory --> "+DIR)
    elif not quiet:
        L.logger.warning("Directory does not exist --> "+DIR)
    return;



###########################################################
def file_copy(SRC,DEST,overwrite=True,quiet=True,execute=False,**kwargs):
    """Ccopy a file in the system.
    @param SRC:       the source file path
    @param DEST:      the destination file path
    @kwarg overwrite: logical to determine if DEST is to be
                      overwritten if it exists.
    @kwarg quiet:     logical to determine print statements
    @kwarg execute:   logical for giving copied file execute permissions
    @kwargs:          extra keywords are allowed
    """

    # Check if the destination file exists
    if os.path.exists(DEST):
        if overwrite:
            os.remove(DEST)
            shutil.copy(SRC,DEST)
        else:
            L.logger.warning("Destination file already exists --> "+DEST)
            L.logger.warning("Please select overwrite=True to avoid this.")
            L.logger.warning("Will continue, but success is questionable.")
            #sys.exit(2)
            return;
    else:
        shutil.copy2(SRC,DEST)

    if execute:
        os.chmod(DEST,stat.S_IRWXU)

    if not quiet:
        L.logger.info("Successfully copied "+SRC+" --> "+DEST)
        if execute:
            L.logger.info(DEST+" now has execute permissions.")

    return;



###########################################################
def file_find(DIR,EXP,FULL=False,FIRST_DIR=False):
    """Find directories
    @param DIR: find matching directories under this one
    @param EXP: the regular expression to find directories
    @kwarg FULL: logical to return with the full path
    @kwarg FIRST_DIR: logical to return the first directory only
    @return L:  the list of matching files
    """
    L = []
    rc = re.compile(EXP)
    for dpath, dnames, fnames in os.walk(DIR):
        if FIRST_DIR:
            if not L:  dpath1 = dpath
            else:
                if dpath != dpath1:  break
        if FULL:
            if dpath[-1] == '/':
                L = L + [dpath+fn for fn in fnames if rc.match(fn)]
            else:
                L = L + [dpath+'/'+fn for fn in fnames if rc.match(fn)]
        else:
            L = L + [fn for fn in fnames if rc.match(fn)]
    return(L);



###########################################################
def file_remove(FILE,quiet=True,**kwargs):
    """
    @param FILE:  the file (full path) to be removed
    @kwarg quiet: logical to determine print statements
    """
    L = kwargs.get('logger',Main_Logger('Utilities'))
    if os.path.exists(FILE):
        os.remove(FILE)
    if not quiet:
        L.logger.info("Successfully removed "+FILE)
    return;



###########################################################
def list_convert(L,dtype):
    """Convert a list to a particular type.
    @param L:     the list
    @kwarg dtype: the type to convert to
    @return:      the converted list
    """
    if isinstance(L, list):
        return([list_convert(x,dtype) for x in L]);
    return(dtype(L));



###########################################################
def list_filter_str(S,SUB):
    """Filter a list based on a substring
    @param S:   the list of strings
    @param SUB: the substring to filter by
    """
    return([str for str in S if any(x in str for x in SUB)]);



###########################################################
def list_find(LI,EXP):
    """Find a value within a list
    @param LI:  the input list
    @param EXP: the regular expression for the search
    @return LO:  the list of matching files
    """
    LO = []
    rc = re.compile(EXP)
    LO = [i for i in LI if rc.match(i)]
    return(LO);



###########################################################
def parse_input_args(ARGS,OPT=None):
    """A simple parser that is mainly used for python input
       arguments not counting the file name.
    @param ARGS:  usually sys.argv
    @return ARG2: filtered list of arguments
    """

    # Check that at least one additional argument is present.
    if len(ARGS) < 2:
        print("ERROR: Need at least 2 arguments.")
        sys.exit(2)

    if OPT is None:
        # Remove the first index, which is simply the file name.
        ARGS2= ARGS[1:]
    else:
        for A in ARGS[1:]:
            if A == '-e' and OPT == 'expt':
                ARGS2 = []
                
    # Return the truncated arguments list.
    return(ARGS2);



###########################################################
def parse_launch_args(args=sys.argv[1:]):
    """Parse the input arguments for the GPLOT launch
    @return args: an object containing the input arguments.
    """
    ap = configargparse.ArgParser(description='GPLOT Launch Parser')
    ap.add_argument("-e", "--expt", required=True, help="Comma separated list of experiments (e.g., EXPT1 or EXPT1,EXPT2,EXPT3,...)")
    ap.add_argument("-c", "--config", required=False, help="Comma separated list of configuration files (e.g., CONF1 or CONF1,CONF2,CONF3,...)")
    ap.add_argument("-d", "--delete_table", required=False, dest='delete_table', action='store_true', help="Force delete the namelist database table")
    ap.add_argument("--no-spawn", required=False, dest='spawn', action='store_false', help="Do not spawn child jobs. Useful for testing.")
    ap.set_defaults(spawn=True)
    ap.add_argument("-v", "--verbose", required=False, dest='verbose', action='store_true', help="Verbose mode")
    args, unknown = ap.parse_known_args()
    return(args);



###########################################################
def parse_spawn_args():
    """Parse the input arguments for the GPLOT spawn
    @return args: an object containing the input arguments.
    """
    ap = configargparse.ArgParser(description='GPLOT Spawn Parser')
    ap.add_argument("-e", "--expt", required=True, help="The GPLOT experiment")
    ap.add_argument("-o", "--gpout", required=True, help="The GPLOT output directory")
    ap.add_argument("-m", "--module", required=True, help="The GPLOT module")
    ap.add_argument("-v", "--verbose", required=False, dest='verbose', action='store_true', help="Verbose mode")
    ap.add_argument("--no-submit", required=False, dest='submit', action='store_false', help="Do not spawn child jobs. Useful for testing.")
    ap.set_defaults(submit=True)
    args, unknown = ap.parse_known_args()
    return(args);




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



###########################################################
def string_format_int(N):
    """Returns string formatters for integers
    @param N: the number of total padded digits
    @return:  the format statement
    """
    if type(N) != int:  N = int(N)
    if N == 2:    return("{0:0=2d}");
    elif N == 3:  return("{0:0=3d}");
    elif N == 4:  return("{0:0=4d}");



###########################################################
def time_since_modified(FD,VB=False,**kwargs):
    """Get the time since last modified
    @param FD: the file or directory, could be an array
    @kwarg VB: logical that controls verbosity
    @kwargs:   extra keyword arguments can be passed
    @return T: the time since last modified (seconds)
    """

    L = kwargs.get('logger',Main_Logger('Utilities'))

    # The type should be a list of strings
    if type(FD) == str:
        FD = [FD]

    # Loop over all elements, could be one
    T = []
    for (F,N) in zip(FD,range(len(FD))):
        try:
            T.append(int(time.time() - os.path.getmtime(F)))
        except FileNotFoundError as e:
            L.logger.error(e)
            T.append(None)

        if VB:
            L.logger.info(F+" is "+T[N]+"s old.")

    # Return the list of file/directory ages
    return(T);
