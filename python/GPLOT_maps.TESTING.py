#!python

# Import necessary modules
import os
import sys
import subprocess

# These are rando modules that Andy used for the Polar module 
#from py3grads import Grads #This is how we'll get the data
#import numpy as np #Used for a lot of the calculations
#import matplotlib #The plotting routines
#matplotlib.use('Agg')
#import matplotlib.pyplot as plt #Command for the plotting
#import matplotlib.colors as colors #Command to do some colorbar stuff
#import scipy #Used for interpolation to polar coordinates
#from scipy import interpolate #The interpolation function
#from matplotlib.ticker import ScalarFormatter #Used to change the log-y-axis ticks
#import glob
#import cmath
#from mpl_toolkits.axes_grid1 import make_axes_locatable



"""
Module GPLOT Maps: Visualization of Gridded Data as Plan View Graphics.

GPLOT is the Graphical Post-processed Locus for Output of Tropical cyclones.
It consists of independent modules used to create graphics targeted toward
presentations, posters, publications, and basic research related to tropical
cyclones. To learn more, please download the code from GitHub:
  https://github.com/ghassan-alaka/GPLOT

The Maps module creates 2D map graphics with multiple overlay options from
gridded datasets. This package was originally created to visualize model
output. Now, graphics may be produced for any gridded dataset.

To learn more about the AOML/Hurricane Research Division, please visit:
  https://www.aoml.noaa.gov/our-research/hurricane-research-division/

Created By:   Ghassan Alaka Jr.
Assisted By:
Date Created: Febrary 5, 2020

Example call: python ./GPLOT_maps.py

Modification Log:


"""

__version__ = '0.1.0';


def main():

	# Check that GPLOT_DIR is defined in the environment.
	print('MSG: Checking GPLOT location.')
	GPLOT_DIR = os.environ['GPLOT_DIR']
	if not GPLOT_DIR:
		print('ERROR: Did not find a GPLOT location. Please define GPLOT_DIR in your environment.')
		sys.exit(2)
	print('MSG: Found this GPLOT location --> '+GPLOT_DIR)
	sys.path.append(GPLOT_DIR+'/python/modules')


	# Import custom modules
	#import centroid


	#Define Pygrads interface
	#ga = Grads(verbose=False)


	#Get command lines arguments
	exp_args = 8
	act_args = len(sys.argv) - 1
	
	if act_args < exp_args:
		print("ERROR: Expected "+str(exp_args)+" command line arguments. Got "+str(len(sys.argv)))
		sys.exit(2

	NMLIST, IDATE, SID, DOMAIN, TIER, ENSID, FORCE = sys.argv[1],sys.argv[2],sys.argv[3],sys.argv[4],sys.argv[5],sys.argv[6],sys.argv[7]
)
	IDATE = sys.argv[1]
	if IDATE == 'MISSING':
		IDATE = ''
	SID = sys.argv[2]
	if SID == 'MISSING':
		SID = ''
	DOMAIN = sys.argv[3]
	if DOMAIN == 'MISSING':
		DOMAIN = ''
	TIER = sys.argv[4]
	if TIER == 'MISSING':
		TIER = ''
	ENSID = sys.argv[5]
	if ENSID == 'MISSING':
		ENSID = ''
	FORCE = sys.argv[6]
	if FORCE == 'MISSING':
		FORCE = ''
	#RESOLUTION = sys.argv[7]
	#if RESOLUTION == 'MISSING':
	#        RESOLUTION = ''
	#RMAX = sys.argv[8]
	#if RMAX == 'MISSING':
	#        RMAX = ''
	#LEVS = sys.argv[9]
	#if LEVS == 'MISSING':
	#        LEVS = ''
	NMLIST = sys.argv[7]
	if NMLIST == 'MISSING':
		print("ERROR: Master Namelist can't be MISSING.")
		sys.exit()
	MASTER_NML_IN = GPLOT_DIR+'/nmlist/'+NMLIST

	# Read the master namelist
	NML = namelist(MASTER_NML_IN)
	#NML_DATA = np.genfromtxt(MASTER_NML_IN,dtype='str')
	#NML_DATE = np.loadtxt(MASTER_NML_IN)
	DSOURCE = subprocess.run(['grep','^DSOURCE',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1];
	EXPT = subprocess.run(['grep','^EXPT',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1];
	ODIR = subprocess.run(['grep','^ODIR',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1].strip()+'/'+EXPT.strip()+'/'+IDATE.strip()+'/'+DOMAIN.strip()+'/';
	#print(ODIR)

	try:
		DO_CONVERTGIF = subprocess.run(['grep','^DO_CONVERTGIF',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1].strip();
		DO_CONVERTGIF = (DO_CONVERTGIF == 'True');
	except:
		DO_CONVERTGIF = False;

	figext = '.png';

	# Create the temporary directory for GrADs files
	#TMPDIR = ODIR.strip()+'grads/'
	#if not os.path.exists(TMPDIR):
	#        os.mkdir(TMPDIR)

	# Define some important file names
	UNPLOTTED_FILE = ODIR.strip()+'UnplottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
	PLOTTED_FILE = ODIR.strip()+'PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
	ALLFHR_FILE = ODIR.strip()+'AllForecastHours.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
	STATUS_FILE = ODIR.strip()+'status.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
	ATCF_FILE = ODIR.strip()+'ATCF_FILES.dat'


	#Get parameters from input file
	#resolution = np.float(RESOLUTION)
	#rmax = np.float(RMAX)
	#zsize = np.int(LEVS)

	# Get the ATCF file.
	ATCF_LIST = np.genfromtxt(ODIR+'ATCF_FILES.dat',dtype='str')
	#print(str(ATCF_LIST))
	if ATCF_LIST.size > 1:
		print('Found multiple ATCFs')
		ATCF = ATCF_LIST[[i for i, s in enumerate(ATCF_LIST) if str(SID).lower() in s][:]][0]
	else:
		ATCF = ATCF_LIST
	print('MSG: Found this ATCF --> '+str(ATCF))
	LONGSID = str(ATCF).split('/')[-1].split('.')[0]
	#print('MSG: Running with this long Storm ID --> '+LONGSID.strip())
	TCNAME = LONGSID[::-1]
	TCNAME = TCNAME[3:]
	TCNAME = TCNAME[::-1]
	SNUM = LONGSID[::-1]
	SNUM = SNUM[1:3]
	SNUM = SNUM[::-1]
	BASINID = LONGSID[::-1]
	BASINID = BASINID[0]
	ATCF_DATA = np.atleast_2d(np.genfromtxt(str(ATCF),delimiter=',',dtype='str',autostrip='true'))
	ATCF_DATA = ATCF_DATA[list([i for i, s in enumerate(ATCF_DATA[:,11]) if '34' in s][:]),:]


	# Get the list of unplotted files
	UNPLOTTED_LIST = np.array( np.genfromtxt(UNPLOTTED_FILE,dtype='str') )

	#FHR_LIST = [ int(x) for x in [np.genfromtxt(ALLFHR_FILE,dtype='str')] ]
	FHR_LIST = np.array( np.genfromtxt(ALLFHR_FILE,dtype='int') )
	if (FHR_LIST.size == 1):
		FHR_LIST = np.append(FHR_LIST,"999")
		UNPLOTTED_LIST = np.append(UNPLOTTED_LIST,"MISSING")



if  __name__ == '__main__':
	main()
