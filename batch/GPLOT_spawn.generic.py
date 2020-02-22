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
import datetime, os, sys

# Import custom GPLOT modules
sys.path.insert(1, os.environ['GPLOT_DIR']+"/python")
import database as db
import execute as xc
import gputil as gpu
import namelist as nm
from gputil import Directories
from namelist import Namelist_Retrieve

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
Assisted By:
Date Created:  February 21, 2020
Date Modified: February 21, 2020

Example call: python ./GPLOT_spawn.generic.py

Modification Log:


"""

__version__ = '0.1.0';




###########################################################




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
    print("MSG: The experiment --> "+EXPT)

    # Get the database name
    DB_FILE = db.db_name(GPOUT,EXPT)
    print(DB_FILE)

    # Get important directories
    D = Directories()
    print(D.TBLDIR)

    # Read namelist from the database
    # Create a new class for this
    NML = Namelist_Retrieve(D,EXPT,DB_FILE,MOD)

    # Define the generic bundle file
    BATCHFILE1 = NML.BATCHDIR+"GPLOT_bundle.generic.py"

    # Start a counter
    CC = 0

    # Loop over all ensemble IDs
    for EN in NML.ENSID:

        # Determine if this is an ensemble experiment
        if int(EN) == 0:
            NML.IS_ENSEMBLE = False
        else:
            NML.IS_ENSEMBLE = True
            print("MSG: Working on ensemble member %d02"%EN)

        # Loop over all tiers
        for TR in NML.TIER:

            print("MSG: Working on "+TR)

            # Loop over all forecast cycles
            for CY in CYCLES:

                print("MSG: Working on forecast cycle "+CY)

                # Loop over all tropical cyclones
                for TC in STORMS:

                    print("MSG: Working on storm "+TC)

                    # Loop over all domains
                    for DM in NML.DOMAIN:

                        print("MSG: Working on domain "+DM)


                        # Eventually, all conditions will be met and the job will have been bundled.
                        CC = CC+1
                        if CC%5 == 0:
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
