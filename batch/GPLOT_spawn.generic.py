#!/usr/bin/env python
#SBATCH --account=hur-aoml
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=00:45:00
#SBATCH --partition=tjet,ujet,sjet,vjet,xjet,kjet
#SBATCH --mail-type=FAIL
#SBATCH --qos=batch
#SBATCH --chdir=.
#SBATCH --output=/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/log/GPLOT.Default.out
#SBATCH --error=/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/log/GPLOT.Default.err
#SBATCH --job-name="GPLOT.Default"
#SBATCH --mem=16G


# Import necessary modules
import datetime, os, re, sys, time
import logging
import numpy as np

# Import custom GPLOT modules
sys.path.insert(1, os.environ['GPLOT_DIR']+"/python")
import gp_database as db
import gp_execute as xc
import gp_log as log
import gp_namelist as nm
import gp_util as gpu
from gp_database import Database_Status
from gp_log import Main_Logger
from gp_namelist import Namelist_Retrieve
from gp_util import Directories

"""
GPLOT Spawn Driver
Main driver for organizing GPLOT graphical production jobs.

GPLOT is the Graphical Post-processed Locus for Output of Tropical cyclones.
It consists of independent modules used to create graphics targeted toward
presentations, posters, publications, and basic research related to tropical
cyclones. To learn more, please download the code from GitHub:
  https://github.com/ghassan-alaka/GPLOT

To learn more about the AOML/Hurricane Research Division, please visit:
  https://www.aoml.noaa.gov/our-research/hurricane-research-division/

Created By:    Ghassan Alaka Jr.
Modified By:   Ghassan Alaka Jr.
Date Created:  February 21, 2020
Last Modified: March 24, 2020

Example call: python ./GPLOT_spawn.generic.py {-e EXPT} -o {GPOUT} -m {MODULE}
              EXPT: the experiment name (e.g., GFS_Forecast)
              GPOUT: the GPLOT top-level output directory (e.g., lfs3/projects/hur-aoml/Ghassan.Alaka/GPLOT/)
              MODULE: the module name (e.g., maps)

Modification Log:
2020-Feb-27 -- GJA updated the names of GLOT-specific modules, e.g., database --> gp_database
2020-Mar-17 -- GJA developed functionality for the main status database table. Some of this functionality
                 was moved to the Database_Status class in gp_database.py. This is important because completed
                 tasks can be bypassed.
            -- GJA developed functionality for the atcf status database table. Some of this functionality was
                 moved to the Database_Status class in gp_database.py. This database keeps track of ATCF files
                 and information about them.
2020-Mar-19 -- GJA implemented functionality for logging.
2020-Mar-24 -- GJA developed functionality for the graphic status database table. Some of this functionality
                 was moved to the Databse_Status class in gp_database.py. This database table keeps track of
                 each graphic that should be produced by GPLOT.
"""

__version__ = '0.4.0';




###########################################################
def retrieve_storms(NML,CY='',ALIST=None,Stage1=True,Stage2=True,Stage3=True,FORCE=False):
    """Retrieve a list of Storm IDs 
    @param NML:   the namelist object
    @kwarg ALIST: the list of ATCF files for the current cycle
    """

    STORMS = []

    # Stage 1: try to get STORMS fom the ATCF files
    if Stage1 and NML.SID is not None:
        STORMS.append(NML.SID)

    # Stage 2: try to get STORMS from the ATCF files
    if not STORMS or FORCE:
        if Stage2 and ALIST:
            for A in ALIST:
                S = ''.join(reversed(''.join(reversed(A.split('/')[-1].split('.')[0]))[0:3])).upper()
                if S not in STORMS:
                    STORMS.append(S)

    # Stage 3: try to get STORMS from the data input path
    if not STORMS or FORCE:
        if Stage3:
            SIDDIR = [];
            SIDDIR = gpu.list_find(gpu.dir_find(NML.DATADIR,"\d{2}[A-Z]",FULL=True),".*"+CY+"$")
            SIDDIR = [i.split('/')[-1] for i in SIDDIR]
            if SIDDIR:
                for S in SIDDIR:
                    if S not in STORMS:
                        STORMS.append(S)

    # If STORMS is still undefined, then set it to "NONE"
    # Large-scale graphics may still proceed.
    # Storm-centered graphics will be skipped.
    if not STORMS:
        STORMS = [None]

    # If this is a multistorm run, append Fake Storm (00L)
    # if IS_MSTORM=True and if other storms were found, i.e., STORMS != None
    if NML.IS_MSTORM and STORMS[0] is not None:
        if '00L' in STORMS:
            STORMS.remove('00L')
        STORMS.insert(0,'00L')

    return(STORMS);



###########################################################
def get_atcf_list(ALIST,L):
    """ Build a list of unique ATCF files.
    @param ALIST: a list of ATCF information, including directory & extention
    @param L:     the logging object
    @return AALL: a concatenated list of all ATCF files found. Duplicates
    """
    AALL = [];
    ABASE = [];

    # Loop over all ATCF locations
    for x in range(0,len(ALIST)):

        # Get the file basename. This is import to remove duplicates that exist
        # in different directories
        if AALL:
            ABASE = [i.split('/')[-1] for i in AALL]

        # An ATCF directory must exist to search.
        if ALIST[x][0] is not None:

            # Retrieve matching files
            if ALIST[x][1] is None:
                AADD = gpu.file_find(ALIST[x][0],".*",FULL=True)
            else:
                AADD = gpu.file_find(ALIST[x][0],".*"+ALIST[x][1]+"$",FULL=True)

            # Remove ATCF files that are already in the list.
            for A in ABASE:
                AADD = [x for x in AADD if not x.endswith(A)]

            # Add to the list, if necessary.
            if AADD:
                AALL = list(dict.fromkeys(AALL + AADD))

    # Check that AALL has entries
    if not AALL:
        L.logger.warning("No ATCF files were found. This might be OK.")

    # return the ATCF list.
    return(AALL);



###########################################################
def spawn_atcf_list(N,L,quiet=True):
    """Interface between main() and get_atcf_list() that
    accounts for a comparison dataset, if available.
    @param N:     the master namelist object
    @param L:     the logging object
    @kwarg quiet: logical to determine print statements
    """

    # First, get the list of ATCF files for the primary dataset
    try:
        A = get_atcf_list(N.ATCF_INFO,L)
    except TypeError as e:
        L.logger.error(e)
        sys.exit(2)
    except AttributeError as e:
        L.logger.error(e)
        sys.exit(2)

    # Second, get the list of ATCF files for the comparison dataset, if applicable.
    # If unavailable, take a list of the original list
    try:
        if N.DO_COMPARE:
            A = [A,get_atcf_list(N.ATCF_INFO2,L)]
        else:
            A = [A]
    except Exception as e:
        L.logger.error(e)
        if not quiet:
            L.logger.warning("ATCF files for comparison datset not found.")

    # Return A as a list of 1 or 2 lists of ATCF files.
    return(A);



###########################################################
def spawn_complete(L,n=0):
    """Exit the spawner
    @param L: the Main_Logger class object
    @kwarg n: the exit code
    """
    L.logger.info("Spawn completed at "+str(datetime.datetime.now()))
    sys.exit(n)




###########################################################
def spawn_cycle_atcf_list(N,AALL,CY):
    """Retrieve the ATCF file list for thie particular forecast cycle
    @param N:    the master namelist object
    @param AALL: the list of all ATCF files
    @param CY:   the forecast cycle
    @return A:   the list of ATCF files for this cycle
    """
    A = [None]

    # Create a list of ATCF files that match the requested cycle
    if len(AALL[0]) > 0:
        A = gpu.list_find(AALL[0],".*"+CY+".*")
        if not A:  A = []

    # Add another list for the comparison dataset, if necessary
    if N.DO_COMPARE:
        if len(AALL[1]) > 0:
            A = [A,gpu.list_find(AALL[1],".*"+CY+".*")]
    else:
        A = [A]

    return(A);




###########################################################
def spawn_cycles(NML,L,quiet=True):
    """Retrieve the available forecast cycles
    @param NML:   the master namelist object
    @param L:     the logging object
    @kwarg quiet: logical to determine print statements
    """
    CYC = gpu.dir_find(NML.DATADIR,".*\d{10}.*")
    for (C,N) in zip(CYC,range(len(CYC))):
        CYC[N] = ''.join(reversed(''.join(reversed(C)).split('.')[0]))
    CYC.sort()
    L.logger.info("Found these primary cycles --> "+' '.join(CYC))
    if NML.DO_COMPARE:
        CYC = list(CYC,gpu.dir_find(NML.DATADIR2,".*\d{10}.*"))
        for (C,N) in zip(CYC[1],range(len(CYC[1]))):
            CYC[1][N] = ''.join(reversed(''.join(reversed(C)).split('.')[0]))
        CYC[1].sort()
        L.logger.info("Found these comparison cycles --> "+' '.join(CYC[1]))
    else:
        CYC = [CYC]

    return(CYC);



###########################################################
def spawn_input_files(NML,CY,TC,L,EN=None,AHR=None):
    """Retrieve the input data files.
    @param NML: the master namelist object
    @param CY:  the GPLOT cycle
    @param TC:  the GPLOT tropical cyclone
    @param L:   the logging object
    @kwarg EN:  the ensemble ID
    @kwarg AHR: list of ATCF lead times
    """
    # Initialize a blank list
    FILES = []

    # Make sure prefix and suffix lists match
    if type(NML.FILE_PRE) != list:  NML.FILE_PRE = [NML.FILE_PRE]
    if type(NML.FILE_SUF) != list:  NML.FILE_SUF = [NML.FILE_SUF]
    if len(NML.FILE_SUF) == 1 and len(NML.FILE_PRE) > 1:  NML.FILE_SUF*len(NML.FILE_PRE)
 
    for (P,S) in zip(NML.FILE_PRE,NML.FILE_SUF):
        # Create the regular expression for this situation
        RE = r'^.*('+TC.lower()+'|).*'+CY+'.*'+P+'.*\d{'+NML.FILE_HFMT+'}.*'+S+'$'
 
        L.logger.debug("Searching this data directory --> "+NML.DATADIR)
        L.logger.debug("Used this regular expression to search for files:  "+RE)
 
        # Search for files using the regex.
        # Return full paths and first directory only.
        FFILE1 = gpu.file_find(NML.DATADIR,RE,FULL=True,FIRST_DIR=True)
        if NML.IS_ENSEMBLE and EN is not None:
            FFILE1 = gpu.list_find(FFILE1,"^.*/"+EN+"/.*$")
        if not FFILE1:  continue
        if NML.IS_STORMCEN and AHR is not None:
            FFILE2 = []
            for A in AHR[0]:
                FMT = gpu.string_format_int(NML.FILE_HFMT)
                FMT = FMT.format(A)
                X = NML.FILE_FHR+FMT
                VALS = [i for i, s in enumerate(FFILE1) if X in s]
                if VALS is None:  continue
                FFILE2.append(str(np.array(FFILE1,dtype=str)[VALS][0]))
            if not FFILE2:  continue
            FFILE1 = FFILE2
        FFILE1.sort()
        FILES.append(FFILE1)

        if FILES:  NML.FILES_FOUND = True
        else:
            NML.FILES_FOUND = False
            L.logger.warning("Could not find any input files in "+NML.DATADIR)

    # Follow the same steps for the comparison dataset
    if NML.DO_COMPARE:
        FILES2 = []
        if type(NML.FILE_PRE2) != list:  NML.FILE_PRE2 = [NML.FILE_PRE2]
        if type(NML.FILE_SUF2) != list:  NML.FILE_SUF2 = [NML.FILE_SUF2]
        if len(NML.FILE_SUF2) == 1 and len(NML.FILE_PRE2) > 1:  NML.FILE_SUF2*len(NML.FILE_PRE2)
        for (P,S) in zip(NML.FILE_PRE2,NML.FILE_SUF2):
            RE = r'^.*('+TC.lower()+'|).*'+CY+'.*'+P+'.*\d{'+NML.FILE_HFMT2+'}.*'+S+'$'
            L.logger.debug("Searching this comparison data directory --> "+NML.DATADIR2)
            L.logger.debug("Used this regular expression to search for files:  "+RE)
            FFILE1 = gpu.file_find(NML.DATADIR2,RE,FULL=True,FIRST_DIR=True)
            if not FFILE1:  continue
            if NML.IS_STORMCEN and AHR is not None:
                FFILE2 = []
                for A in AHR[1]:
                    FMT = gpu.string_format_int(NML.FILE_HFMT)
                    FMT = FMT.format(A)
                    X = NML.FILE_FHR+FMT
                    VALS = [i for i, s in enumerate(FFILE1) if X in s]
                    if VALS is None:  continue
                    FFILE2.append(str(np.array(FFILE1,dtype=str)[VALS][0]))
                if not FFILE2:  continue
                FFILE1 = FFILE2
            FFILE1.sort()
            FILES2.append(FFILE1)

        if FILES:  NML.FILES_FOUND = True
        else:
            NML.FILES_FOUND = False
            L.logger.warning("Could not find any input files in "+NML.DATADIR2)

        if FILES2:  FILES = [FILES,FILES2]
        else:       FILES = [FILES,[]]
    else:
        FILES = [FILES]

    # Return the list of list(s)
    return(FILES);



###########################################################
def spawn_storms(N,CY,ATCF,L):
    """Interface between main() and retrieve_storms() that
    accounts for a comparison dataset, if available.
    @param N:       the master namelist object
    @param CY:      the forecast cycle
    @param ATCF:    the list of ATCF files for this cycle
    @param L:       the logging object
    @return STORMS: the list of storms for this cycle
    """
    STORMS = retrieve_storms(N,CY=CY,ALIST=ATCF[0],Stage1=True,Stage2=True,Stage3=True,FORCE=False)
    if STORMS[0] is None:
        L.logger.warning("No Storms found for this forecast cycle.")
    else:
        L.logger.info("Found these storms --> "+', '.join(STORMS))

    if N.DO_COMPARE:
        STORMS = list(STORMS,retrieve_storms(N,CY=CY,ALIST=CYCLE_ATCF[1],Stage1=True,Stage2=True,Stage3=True,FORCE=False))
    else:
        STORMS = [STORMS]

    return(STORMS);


###########################################################
def spawn_storm_atcf(N,A1,TC,L,DO_FHRS=False,DO_AGE=False,):
    """Retrieve the TCF file for this storm, and, optionally,
    the corresponding list of lead times.
    @param N:       the master namelist object
    @param A1:      the list of ATCF files for this cycle
    @param TC:      the storm ID
    @param L:       the logging object
    @kwarg DO_FHRS: logical argument for finding lead times in this ATCF
    @kwarg DO_AGE:  logical argument for finding file ages in this ATCF
    """
    # Create empty lists
    A2 = [];
    if DO_FHRS:  F = [];
    if DO_AGE:   Z = [];

    # Try to find an ATCF for this TC in the list of ATCF files found
    # for this cycle.
    A2 = gpu.list_find(A1[0],".*"+TC.lower()+".*")

    # Determine if an ATCF was found. If found, then also find list of 
    # forecast lead times (F), if applicable.
    if not A2:
        L.logger.warning("No ATCF found for "+TC+". This might be OK.")
        A2 = [None]
        if DO_FHRS:  F = [None]
        if DO_AGE:   Z = [None]
    else:
        A2 = [A2[0]]
        L.logger.info("ATCF found for "+TC+" --> "+A2[0])
        if DO_FHRS:
            F = list(gpu.atcf_read(A2[0],COLS='FHR',FHR_S=N.HR_INIT,FHR_E=N.HR_FNL,EXTRACT=True))
        if DO_AGE:
            Z = gpu.time_since_modified(A2[0])

    # Follow the same steps for the comparison dataset, if applicable.
    if N.DO_COMPARE:
        A2 = [A2,gpu.list_find(A2[1],".*"+TC.lower()+".*")]
        if not A2[1]:
            A2[1] = [None]
            if DO_FHRS:  F[1] = [None]
            if DO_AGE:   Z[1] = [None]
        else:
            A2[1] = [A2[1][0]]
            if DO_FHRS:
                F = [F,list(gpu.atcf_read(A2[1][0],COLS='FHR',FHR_S=N.HR_INIT,FHR_E=N.HR_FNL,EXTRACT=True))]
            if DO_AGE:
                Z = [Z,gpu.time_since_modified(A2[1][0])]
    else:
        A2 = [A2]
        if DO_FHRS:  F = [F]
        if DO_AGE:   Z = [Z]

    # Return A2 and possibly F,Z.
    if DO_FHRS and DO_AGE:            return(A2,F,Z);
    elif DO_FHRS and not DO_AGE:      return(A2,F);
    elif DO_AGE and not DO_FHRS:      return(A2,Z);
    elif not DO_FHRS and not DO_AGE:  return(A2);



###########################################################
def submit_bundle(NML,DB,L):
    """Submit a bundle of production jobs
    @param NML: the Namelist_Retrieve class object
    @param DB:  the Database_Status class object
    @param L:   the Main_Logger class object
    """

    # Define bundle-specific variables
    BUNDLEJOB = "GPLOT_bundler."+NML.MOD+"."+NML.EXPT
    BUNDLEFILE1 = NML.BATCHDIR+"GPLOT_bundler.generic.py"

    # Define the command prefix, typically sourcing the modulefile
    PRE_JOB = xc.find_pre_job(NML.GPLOT_DIR)
    SRC_MODS = xc.src_mods_pre(NML.GPLOT_DIR)
    #PRE = xc.src_mods_pre(NML.GPLOT_DIR)

    # Build the base command with input arguments. The file name is appended later.
    CMD_BASE = " -e "+NML.EXPT+" -o "+NML.GPOUT+" -m "+NML.MOD+" -g "+','.join(DB.G_PROD)+" -c "+DB.CY+" -s "+DB.TC+" -d "+DB.DM+" --ensid "+DB.EN

    # DECIDE HOW TO SUBMIT, THEN SUBMIT!
    if NML.BATCH_MODE.lower() == 'sbatch':

        # Define a bundle file specifically for this case, and
        # copy the remplate bundle file to modify for this case
        BUNDLEID = gpu.random_N_digits(8)
        BUNDLEJOB = BUNDLEJOB+"."+str(BUNDLEID)
        BUNDLELOG = NML.LOGDIR+BUNDLEJOB+".log"
        BUNDLEFILE2 = NML.BATCHDIR+"GPLOT_bundler."+NML.MOD+"."+NML.EXPT+"."+str(BUNDLEID)+".py"
        L.logger.info("Copying "+BUNDLEFILE1+" --> "+BUNDLEFILE2)
        gpu.file_copy(BUNDLEFILE1,BUNDLEFILE2)
        CMD_BASE = BUNDLEFILE2+CMD_BASE

        # Modify the SBATCH header
        xc.slurm_prep(BUNDLEFILE2,NML.LOGDIR,MOD=NML.MOD,EXPT=NML.EXPT,CPU_ACCT=NML.CPU_ACCT,PARTITION=NML.PARTITION,\
                      QOS=NML.QOS,JNAME=BUNDLEJOB,RM_LOGS=True,logger=L)

        # Get the Slurm Job ID for any matching jobs
        id = xc.get_slurm_jobid(BUNDLEJOB)

        # Submit the job
        L.logger.info("Submitting "+BUNDLEFILE2+" to the slurm batch scheduler.")
        L.logger.info("command:  "+PRE+"sbatch "+CMD_BASE)
        xc.exec_subprocess(PRE+"sbatch "+CMD_BASE, GDIR=NML.GPLOT_DIR)

    elif NML.BATCH_MODE.lower() == 'foreground':
        BUNDLELOG = NML.LOGDIR+BUNDLEJOB+".log"
        CMD_BASE = BUNDLEFILE1+CMD_BASE
        L.logger.info("Submitting "+BUNDLEFILE1+" to the foreground.")
        L.logger.info("command:  "+PRE+CMD_BASE+" > "+BUNDLELOG)
        xc.exec_subprocess(PRE+CMD_BASE+" > "+BUNDLELOG, GDIR=NML.GPLOT_DIR)

    elif NML.BATCH_MODE.lower() == 'background':
        BUNDLELOG = NML.LOGDIR+BUNDLEJOB+".log"
        CMD_BASE = BUNDLEFILE1+CMD_BASE
        L.logger.info("Submitting "+BUNDLEFILE1+" to the background.")
        L.logger.info("command:  "+PRE+CMD_BASE+" > "+BUNDLELOG+" &")
        xc.exec_subprocess(PRE+CMD_BASE+" > "+BUNDLELOG+" &", GDIR=NML.GPLOT_DIR)

    else:
        BUNDLELOG = NML.LOGDIR+BUNDLEJOB+".log"
        CMD_BASE = BUNDLEFILE1+CMD_BASE
        L.logger.info("Defaulting to a background job. Submitting "+BUNDLEFILE1+".")
        L.logger.info("command:  "+PRE+CMD_BASE+" > "+BUNDLELOG+" &")
        xc.exec_subprocess(PRE+CMD_BASE+" > "+BUNDLELOG+" &", GDIR=NML.GPLOT_DIR)

    return;



###########################################################
def main():
    """ This is the main function of this driver. All important
        responsibilities of this driver are carried out through
        here.
    """

    # Start the logging
    L = Main_Logger('GPLOT_spawn')
    #L.add_stream_handler(STDOUT=True,LEVEL='debug')

    # Log some important information
    L.logger.info("Spawn began at "+str(datetime.datetime.now()))
    L.logger.debug("Under construction.")

    # Read input arguments: GPLOT experiment, GPLOT output directory, GPLOT module
    SARGS = gpu.parse_spawn_args()
    EXPT, GPOUT, MOD = SARGS.expt, SARGS.gpout, SARGS.module
    L.logger.info("Running GPLOT module '"+MOD+"' for experiment '"+EXPT+"'")
    L.logger.info("GPLOT graphics will eventually be placed in "+GPOUT)

    # Get important directories
    D = Directories(L=L)

    # Get the database file name from a GPLOT table
    DB = Database_Status(D,EXPT)
    DB_FILE = DB.DB_FILE
    DB.initiailize_tbl(logger=L)
    L.logger.info("Found this database file --> "+DB.DB_FILE)

    # Read namelist from the database using the Namelist_Retrieve class
    NML = Namelist_Retrieve(D,EXPT,DB_FILE,MOD,logger=L)

    # Compile a list of ATCF files from the primary dataset
    ATCF_ALL = spawn_atcf_list(NML,L)

    # Start a counter to track how many tasks per bundle
    CC = 0

    # Loop over all ensemble IDs
    for EN in NML.ENSID:

        # Determine if this is an ensemble experiment
        NML.nml_enesmble_check(EN)

        # Get the available forecast cycles
        CYCLES = spawn_cycles(NML,L)

        # Loop over all forecast cycles
        for CY in CYCLES[0]:

            L.logger.info("Working on forecast cycle "+CY)

            # Find the ATCFs for the current CYCLE.
            # It will be blank if no ATCFs are found.
            CYCLE_ATCF = spawn_cycle_atcf_list(NML,ATCF_ALL,CY)

            # Retrieve the list of Storm IDs
            STORMS = spawn_storms(NML,CY,CYCLE_ATCF,L)

            # Set the storm counter. This is important because large-scale
            # output files may be duplicated for different storms. For example,
            # HWRF-B/GFS files for the outer domain are identical for all storms.
            # Please note that this should only track real storms.
            NTC=0

            # Set a flag to determine whether or not files were found.
            # By default, it is False. If it is set to True, then all
            # non-storm-centered domains will use the same data. Storm-
            # centered domains still must check for unique data.
            FOUND_FILES="False"

            # Loop over all tropical cyclones for the current cycle
            for TC in STORMS[0]:
 
                # Skip to the next if no TC
                if TC is None:  continue

                L.logger.info("Working on storm "+TC)
                
                # If not the fake storm, increase the storm counter
                if TC != '00L':  NTC += 1
                L.logger.info("Storm number = "+str(NTC))

                # Find the forecast hours from the ATCF for this particular storm
                STORM_ATCF, ATCF_FHRS, AGES = spawn_storm_atcf(NML,CYCLE_ATCF,TC,L,DO_FHRS=True,DO_AGE=True)
                #print(CYCLE_ATCF)

                # Check the database table "atcf_status"
                #db.create_table(DB.A_TBL,DB.A_COL,DB.A_TYP,DFILE=DB_FILE,close=True,RM_TBL=False,logger=L)
                DB.assign_atcf_data([CY],[TC],['active'],A1=CYCLE_ATCF,A2=STORM_ATCF,FHR_S=ATCF_FHRS[0][0],FHR_E=ATCF_FHRS[0][-1],AGE=AGES[0][0])
                DB.update_tbl_by_age(logger=L)
                DB.retrieve_atcf_status()
                #print(DB.A_STATUS)
                #continue
                #sys.exit(2)

                # Loop over all domains for the current TC
                for DM in NML.DOMAIN:

                    # Determine if this domain is storm-centered (SC)
                    # For storm-centered domains, the Storm ID is appended
                    # to most file names. An ATCF is required for storm-
                    # centered domains. For non-storm-centered domains,
                    # use the namelist value for ATCF requirement.
                    NML.IS_STORMCEN = gpu.data_table_read(NML.TBLDIR+"/DomainInfo.dat",C='STORMCEN',TEST=f'DOMAIN == "{DM}"',IS_BOOL=True,delim_whitespace=True)[0]
                    if NML.IS_STORMCEN:  STORMTAG,AREQ = '.'+TC.upper(), True
                    else:                STORMTAG,AREQ = '', NML.ATCF_REQD
                    L.logger.info("Is this storm-centric? "+str(NML.IS_STORMCEN))

                    # Skip storm-centered domains for the fake storm
                    if TC == '00L' and NML.IS_STORMCEN:  continue

                    L.logger.info("Working on domain "+DM)

                    # Check the database table 'main_status'
                    #db.create_table(DB.M_TBL,DB.M_COL,DB.M_TYP,DFILE=DB_FILE,RM_TBL=True,close=True)
                    DB.assign_main_data(MOD,CY,DM,'started',TC)
                    DB.retrieve_main_status()
                    L.logger.info("The main status = "+DB.M_STATUS)
                    #continue
                    #sys.exit(2)

                    # Skip this cycle if its status is complete
                    for MS in DB.M_STATUS:
                        if MS in ['complete','working']:  continue

                    # Something with FORCE. Is it necessary?

                    # Create the full output path. This goes somewhere later.
                    NML.GPOUT_FULL = GPOUT+'/'+EXPT+'/'+CY+'/'+DM+'/'

                    # Get te nest information from GPLOT table
                    NML.nml_setup_nest(DM,logger=L)
                    NML.nml_setup_filename()

                    # Run some tests on the ATCF for thie storm.
                    # If domain is storm-centerd and ATCF is required, then ATCF must
                    # be present and contain forecast hours
                    if NML.IS_STORMCEN:# and NML.ATCF_REQD:
                        if not STORM_ATCF:
                            L.logger.error("DOMAIN="+DM+"is storm-centered and ATCF files are required.")
                            L.logger.error("But, found no matching ATCF files. So, nothing to do.")
                            continue
                        elif not ATCF_FHRS:
                            L.logger.error("DOMAIN="+DM+"is storm-centered and ATCF files are required.")
                            L.logger.error("ATCF was found for this storm, but no forecast hours were found.")
                            continue

                    # Get the module namelist for this MOD,EXPT,DM
                    #NML.PRIO_CUTOFF = 15
                    NML.find_module_nml(EXPT,MOD,DM,logger=L)
                    #print(NML.NMLMOD)
                    #sys.exit(2)

                    # Find the full input directory. This could also be stored in the database.
                    FILES = spawn_input_files(NML,CY,TC,L,EN=EN,AHR=ATCF_FHRS)
                    if not NML.FILES_FOUND:
                        L.logger.info("No files found. Moving on to the next option.")
                        continue
                    #print(FILES)
                    #print(len(FILES[0][0]))
                    #continue
                    #sys.exit(2)

                    # Then find all input files. Cross reference with and update a database table

                    # Reset the graphic production list
                    DB.G_PROD = []

                    # Loop over the graphics
                    for i,GR in enumerate(NML.NMLMOD['FILE_NAME']):
                        L.logger.info("Working on this graphic --> "+GR)
                        DB.assign_graphic_data(MOD,EN,CY,TC,DM,GR,NML.GPOUT_FULL)
                        DB.retrieve_graphic_status(NML,FILES,logger=L)
                        L.logger.debug("The status for this graphic is '"+','.join(DB.G_STATUS)+"'.")
                        L.logger.debug("Is this graphic active? "+str(DB.G_ACTIVE))

                        if DB.G_ACTIVE:
                            L.logger.info("Adding "+GR+" to the current bundle.")
                            DB.G_PROD = DB.G_PROD+[GR]

                        L.logger.debug(str(len(DB.G_PROD))+"/"+str(NML.MAX_BUNDLE))
                        if len(DB.G_PROD) >= NML.MAX_BUNDLE or i == len(NML.NMLMOD['FILE_NAME']):
                            L.logger.info("Submitting a bundle with these graphics: "+','.join(DB.G_PROD))
                            if SARGS.submit:  submit_bundle(NML,DB,L)
                            if NML.BATCH_MODE.lower() in ['foreground','background']:
                                L.logger.info("Maximum number of bundles for background/foreground (1) has been reached. Exiting.")
                                spawn_complete(L)
                            DB.G_PROD = []
                            CC = CC+1
                            if CC > NML.MAX_JOBS:
                                L.logger.info("Maximum number of bundles for sbatch ("+str(NML.MAX_JOBS)+") has been reached. Exiting.")
                                spawn_complete(L)


    # This is the end of the script
    spawn_complete(L)



###########################################################
if __name__ == '__main__':
    """Start here when the script is called.
    """
    main()
