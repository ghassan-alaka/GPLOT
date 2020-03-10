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
import datetime, os, re, sys

# Import custom GPLOT modules
sys.path.insert(1, os.environ['GPLOT_DIR']+"/python")
import gp_database as db
import gp_execute as xc
import gp_util as gpu
import gp_namelist as nm
from gp_util import Directories
from gp_namelist import Namelist_Retrieve

"""
GPLOT Spawn Driver
Main driver for submitting individual GPLOT graphical production jobs.

GPLOT is the Graphical Post-processed Locus for Output of Tropical cyclones.
It consists of independent modules used to create graphics targeted toward
presentations, posters, publications, and basic research related to tropical
cyclones. To learn more, please download the code from GitHub:
  https://github.com/ghassan-alaka/GPLOT

To learn more about the AOML/Hurricane Research Division, please visit:
  https://www.aoml.noaa.gov/our-research/hurricane-research-division/

Created By:   Ghassan Alaka Jr.
Modified By:
Date Created:  February 21, 2020
Last Modified: February 27, 2020

Example call: python ./GPLOT_spawn.generic.py {EXPT} {GPOUT} {MODULE}
              EXPT: the experiment name (e.g., GFS_Forecast)
              GPOUT: the GPLOT top-level output directory (e.g., lfs3/projects/hur-aoml/Ghassan.Alaka/GPLOT/)
              MODULE: the module name (e.g., maps)

Modification Log:
2020-Feb-27:  GJA updated the names of GLOT-specific modules, e.g., database --> gp_database

"""

__version__ = '0.2.0';




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
def get_atcf_list(ALIST):
    """ Build a list of unique ATCF files.
    @param ALIST: a list of ATCF information, including directory & extention
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
        print("WARNING: No ATCF files were found. This might be OK.")

    # return the ATCF list.
    return(AALL);



###########################################################
def spawn_atcf_list(N,quiet=True):
    """Interface between main() and get_atcf_list() that
    accounts for a comparison dataset, if available.
    @param N:     the master namelist object
    @kwarg quiet: logical to determine print statements
    """

    # First, get the list of ATCF files for the primary dataset
    try:
        A = get_atcf_list(N.ATCF_INFO)
    except TypeError as e:
        print(e)
        sys.exit(2)
    except AttributeError as e:
        print(e)
        sys.exit(2)

    # Second, get the list of ATCF files for the comparison dataset, if applicable.
    # If unavailable, take a list of the original list
    try:
        if N.DO_COMPARE:
            A = [A,get_atcf_list(N.ATCF_INFO2)]
    except:
        if not quiet:
            print("WARNING: ATCF files for comparison datset not found.")
    else:
        A = [A]

    # Return A as a list of 1 or 2 lists of ATCF files.
    return(A);



###########################################################
def spawn_cycles(NML,quiet=True):
    """Retrieve the available forecast cycles
    @param N:     the master namelist object
    @kwarg quiet: logical to determine print statements
    """
    CYC = gpu.dir_find(NML.DATADIR,".*\d{10}.*")
    for (C,N) in zip(CYC,range(len(CYC))):
        CYC[N] = ''.join(reversed(''.join(reversed(C)).split('.')[0]))
    CYC.sort()
    print("MSG: Found these primary cycles --> "+' '.join(CYC))
    if NML.DO_COMPARE:
        CYC = list(CYC,gpu.dir_find(NML.DATADIR2,".*\d{10}.*"))
        for (C,N) in zip(CYC[1],range(len(CYC[1]))):
            CYC[1][N] = ''.join(reversed(''.join(reversed(C)).split('.')[0]))
        CYC[1].sort()
        print("MSG: Found these comparison cycles --> "+' '.join(CYC[1]))
    else:
        CYC = [CYC]

    return(CYC);



###########################################################
def main():
    """ This is the main function of this driver. All important
        responsibilities of this driver are carried out through
        here.
    """
    print("MSG: Spawn began at "+str(datetime.datetime.now()))
    print("MSG: Under construction.")

    # Get the experiment and GPLOT output directory as an input args
    if len(sys.argv) < 4:
        print("ERROR: At least 3 total arguments must be supplied.")
        sys.exit(2)
    EXPT = sys.argv[1]
    GPOUT = sys.argv[2]
    MOD = sys.argv[3]
    print("MSG: Running GPLOT module '"+MOD+"' for experiment '"+EXPT+"'")
    print("MSG: GPLOT graphics will eventually be placed in "+GPOUT)

    # Get the database name
    DB_FILE = db.db_name(GPOUT,EXPT)
    print("MSG: The database file --> "+DB_FILE)

    # Get important directories
    D = Directories()

    # Read namelist from the database
    # Create a new class for this
    NML = Namelist_Retrieve(D,EXPT,DB_FILE,MOD)

    # Define the generic bundle file
    BATCHFILE1 = NML.BATCHDIR+"GPLOT_bundle.generic.py"

    # Compile a list of ATCF files from the primary dataset
    ATCF_ALL = spawn_atcf_list(NML)

    # Start a counter to track how many tasks per bundle
    CC = 0


    # Loop over all ensemble IDs
    for EN in NML.ENSID:

        # Determine if this is an ensemble experiment
        NML.nml_enesmble_check(EN)

        # Get the available forecast cycles
        CYCLES = spawn_cycles(NML)

        # Loop over all forecast cycles
        for CY in CYCLES[0]:

            print("MSG: Working on forecast cycle "+CY)

            # Find the ATCFs for the current CYCLE.
            # It will be blank if no ATCFs are found.
            CYCLE_ATCF = [None]
            if len(ATCF_ALL[0]) > 0:
                CYCLE_ATCF = gpu.list_find(ATCF_ALL[0],".*"+CY+".*")#".*"+CY+".*")
                if not CYCLE_ATCF:
                    CYCLE_ATCF = []
            if NML.DO_COMPARE:
                if len(ATCF_ALL[1]) > 0:
                    CYCLE_ATCF = [CYCLE_ATCF,gpu.list_find(ATCF_ALL[1],".*"+CY+".*")]
            else:
                CYCLE_ATCF = [CYCLE_ATCF]
            #print(CYCLE_ATCF)

            # Retrieve the list of Storm IDs
            STORMS = retrieve_storms(NML,CY=CY,ALIST=CYCLE_ATCF[0],Stage1=True,Stage2=True,Stage3=True,FORCE=False)
            if STORMS[0] is None:
                print("WARNING: No Storms found for this forecast cycle.")
            else:
                print("MSG: Found these storms --> "+', '.join(STORMS))
            if NML.DO_COMPARE:
                STORMS = list(STORMS,retrieve_storms(NML,CY=CY,ALIST=CYCLE_ATCF[0],Stage1=True,Stage2=True,Stage3=True,FORCE=False))
            else:
                STORMS = [STORMS]

            # Set the storm counter. This is important because large-scale
            # output files may be duplicated for different storms. For example,
            # HWRF-B/GFS files for the outer domain are identical for all storms.
            # Please note that this should only track real storms.
            NTC=0

            # Set a flag to determine whether or not files were found.
            # By default, it is False. If it is set to
            FOUND_FILES="False"

            # Loop over all tropical cyclones for the current cycle
            for TC in STORMS[0]:
 
                # Skip to the next if no TC
                if TC is None:
                    continue

                print("MSG: Working on storm "+TC)
                
                # If not the fake storm, increase the storm counter
                if TC != '00L':
                    NTC += 1
                print("MSG: Storm number = "+str(NTC))

                # Find the forecast hours from the ATCF for this particular storm
                STORM_ATCF = []
                ATCF_FHRS = [];
                STORM_ATCF = gpu.list_find(CYCLE_ATCF[0],".*"+TC.lower()+".*")
                if not STORM_ATCF:
                    print("WARNING: No ATCF found for "+TC+". This might be OK.")
                    STORM_ATCF = [None]
                else:
                    STORM_ATCF = STORM_ATCF[0]
                    print("MSG: ATCF found for "+TC+" --> "+STORM_ATCF)
                    ATCF_FHRS = list(gpu.atcf_read(STORM_ATCF,COLS='FHR',FHR_S=NML.HR_INIT,FHR_E=NML.HR_FNL,EXTRACT=True))
                    #sys.exit(2)
                if NML.DO_COMPARE:
                    STORM_ATCF = [STORM_ATCF,gpu.list_find(CYCLE_ATCF[1],".*"+TC.lower()+".*")]
                    if not STORM_ATCF[1]:
                        STORM_ATCF[1] = [None]
                    else:
                        STORM_ATCF[1] = STORM_ATCF[1][0]
                        ATCF_FHRS = [ATCF_FHRS,list(gpu.atcf_read(STORM_ATCF[1],COLS='FHR',FHR_S=NML.HR_INIT,FHR_E=NML.HR_FNL,EXTRACT=True))]
                else:
                    STORM_ATCF = [STORM_ATCF]
                    ATCF_FHRS = [ATCF_FHRS]

                # Loop over all domains for the current TC
                for DM in NML.DOMAIN:

                    # Determine if this domain is storm-centered (SC)
                    # For storm-centered domains, the Storm ID is appended
                    # to most file names.
                    IS_STORMCEN = gpu.data_table_read(NML.TBLDIR+"/DomainInfo.dat",'STORMCEN','DOMAIN',DM,IS_BOOL=True)
                    if IS_STORMCEN:
                        STORMTAG = '.'+TC.upper()
                    else:
                        STORMTAG = ''

                    # Skip storm-centered domains for the fake storm
                    if TC == '00L' and IS_STORMCEN:
                        continue

                    print("MSG: Working on domain "+DM)

                    # Something with FORCE. Is it necessary?

                    # Create the full output path. This goes somewhere later.

                    # Get ATCF_REQD to check if it is required.
                    # This is optional for MAPS.
                    if DM == 'd03':
                        A_R = True
                    else:
                        A_R = NML.ATCF_REQD

                    # Get te nest information from GPLOT table
                    NEST = gpu.data_table_read(NML.TBLDIR+"/DomainInfo.dat",'NEST','DOMAIN',DM)
                    if NEST is None:
                        print("WARNING: Domain "+DM+" not found in "+NML.TBLDIR+"/DomainInfo.dat. Assuming NEST=21.")
                        NEST=1

                    # Skip subsequent storms if the outer domain has been plotted
                    if not IS_STORMCEN and NTC > 1 and FOUND_FILES:
                        print("WARNING: Skipping this domain because it is not storm-centered and this is a least the second storm.")
                        continue

                    # Get file prefix information from table or namelist
                    if NML.DTAG is None:
                        FPRE = gpu.data_table_read(NML.TBLDIR+"/FilePrefix.dat",'D0'+str(NEST),'DSOURCE',NML.DSOURCE)
                    else:
                        FPRE = NML.DTAG

                    # Get file hour string, format information from table or namelist
                    FHRSTR = gpu.data_table_read(NML.TBLDIR+"/FileTimeFormat.dat",'HRSTR','DSOURCE',NML.DSOURCE)
                    if NML.HR_FMT is None:
                        FHRFMT = gpu.data_table_read(NML.TBLDIR+"/FileTimeFormat.dat",'HRFMT','DSOURCE',NML.DSOURCE)
                    else:
                        FHRFMT = NML.HR_FMT

                    # Get file suffix information from table or namelist
                    if NML.DEXT is None:
                        FSUFFIX = gpu.data_table_read(NML.TBLDIR+"/FileSuffix.dat",'SUFFIX','DSOURCE',NML.DSOURCE)
                    else:
                        FSUFFIX = NML.DEXT

                    # Run some tests on the ATCF for thie storm.
                    # If domain is storm-centerd and ATCF is required, then ATCF must
                    # be present and contain forecast hours
                    if IS_STORMCEN and NML.ATCF_REQD:
                        if not STORM_ATCF:
                            print("ERROR: DOMAIN="+DM+"is storm-centered and ATCF files are required.")
                            print("ERROR: But, found no matching ATCF files. So, nothing to do.")
                            continue
                        elif not ATCF_FHRS:
                            print("ERROR: DOMAIN="+DM+"is storm-centered and ATCF files are required.")
                            print("ERROR: ATCF was foundfor this storm, but no forecast hours were found.")
                            continue

                    # Get the module namelist for this EXPT,TR,DM
                    NMODLIST = ['namelist.'+MOD+'.'+EXPT+'.'+DM,'namelist.'+MOD+'.'+DM,'namelist.'+MOD+'.'+DM,'namelist.'+MOD+'.default']
                    for N in NMODLIST:
                        if os.path.exists(NML.NMLDIR+N):
                            NMLMOD = [gpu.data_table_read(NML.NMLDIR+N,A='FILE_NAME'),gpu.data_table_read(NML.NMLDIR+N,A='PLOT_ON',IS_BOOL=True),gpu.data_table_read(NML.NMLDIR+N,A='PRIO')]
                            print("MSG: Found this module namelist --> "+NML.NMLDIR+N)
                            break
                    if hasattr(NMLMOD,'empty'):
                        EMPTY = NMLMOD.empty
                    else:
                        EMPTY = False
                    if EMPTY:
                        print("ERROR: Module namelist not found. Can't continue.")
                        sys.exit(2)
                    print(NMLMOD)

                    sys.exit(2)

                    # How will ATCFs be stored for each cycle? Database table?

                    # Find the full input directory. This could also be stored in the database.

                    # Then find all input files. Cross reference with and update a database table

                    # Then update the status of the current job (cycle, storm, tier, domain, ensid)

                    # Submit the job!

                    # Eventually, all conditions will be met and the job will have been bundled.
                    CC = CC+1
                    if CC%5 == 0 and CC is None:
                        # Define the unique bundle files
                        BATCHFILE2 = NML.BATCHDIR+"GPLOT_bundle."+str(gpu.random_N_digits(8))+".py"
                        print("MSG: Copying "+BATCHFILE1+" --> "+BATCHFILE2)
                        gpu.file_copy(BATCHFILE1,BATCHFILE2)

    print(os.environ['GPLOT_DIR'])
    print("MSG: Spawn completed at "+str(datetime.datetime.now()))



###########################################################
if __name__ == '__main__':
    """Start here when the script is called.
    """
    main()
