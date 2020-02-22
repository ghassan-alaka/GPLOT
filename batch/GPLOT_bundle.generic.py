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
GPLOT Driver bundle
Main driver for submitting bundled GPLOT jobs to the batch scheduler or
even the front-end node (but be careful!

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

Example call: python ./GPLOT_bundle.generic.py

Modification Log:


"""

__version__ = '0.1.0';



###########################################################
def main():
    """ This is the main function of this driver. All important
        responsibilities of this driver are carried out through
        here.
    """
    print("MSG: Bundle began at "+str(datetime.datetime.now()))
    print("MSG: Under construction.")

    print(os.environ['GPLOT_DIR'])
    print("MSG: Bundle completed at "+str(datetime.datetime.now()))



###########################################################
if __name__ == '__main__':
    """Start here when the script is called.
    """
    main()
