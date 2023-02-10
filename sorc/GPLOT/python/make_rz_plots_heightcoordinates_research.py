#!/usr/bin/env python

# Check that GPLOT_DIR is defined in the environment.
import os
GPLOT_DIR = os.environ['GPLOT_DIR']
print('MSG: Found this GPLOT location --> '+GPLOT_DIR)

#Import necessary modules
print('MSG: Importing Everything Needed')
import datetime, glob, subprocess, sys, time, warnings
import math, cmath
from py3grads import Grads				# This is how we'll get the data
import numpy as np					# Used for a lot of the calculations
import metpy
from metpy import interpolate
import matplotlib 					# The plotting routines
matplotlib.use('Agg')
import matplotlib.pyplot as plt				# Command for the plotting
import matplotlib.colors as colors			# Command to do some colorbar stuff
#from matplotlib.ticker import ScalarFormatter		# Used to change the log-y-axis ticks
from mpl_toolkits.axes_grid1 import make_axes_locatable
import scipy						# Used for interpolation to polar coordinates
from scipy import interpolate				# The interpolation function
#import concurrent.futures
from functools import partial
#sys.path.append(GPLOT_DIR+'/sorc/GPLOT/python/modules')
import modules.skewTmodelTCpolar as skewTmodelTCpolar
import modules.shearandrhplot as shearandrhplot
import modules.centroid as centroid
import modules.interp as interp
import modules.io as io
import modules.plotting as plotting
import modules.multiprocess as mproc
import netCDF4
from netCDF4 import Dataset


##############################
def main():

	# Log some important information
	print(f'MSG: make_rz_plots_heightcoordinates_research.py began at {datetime.datetime.now()}')
	print('')
	print('MSG: Welcome to GPLOT, Polar Module.')
	print('MSG: GPLOT is the Graphical Post-processed Locus for Output for Tropical cyclones.')
	print('MSG: The Polar Module produces graphical products in polar cylindrical coordinates')
	print('MSG: centered on the TC or storm of interest. Research advancements interpolate')
	print('MSG: pressure coordinates to true height coordinates for optimal comparisons with')
	print('MSG: observational data. These products are organized into tiers so that the most')
	print('MSG: important graphics are produced first.')

	#Define Pygrads interface
	ga = Grads(verbose=False)
	
	#Get command lines arguments
	if len(sys.argv) < 11:
		print(f'ERROR: Expected 11 command line arguments. Got {len(sys.argv)} args.')
		sys.exit()
	IDATE = sys.argv[1]
	if IDATE == 'MISSING':       IDATE = ''
	SID = sys.argv[2]
	if SID == 'MISSING':         SID = ''
	DOMAIN = sys.argv[3]
	if DOMAIN == 'MISSING':      DOMAIN = ''
	TIER = sys.argv[4]
	if TIER == 'MISSING':        TIER = ''
	ENSID = sys.argv[5]
	if ENSID == 'MISSING':       ENSID = ''
	FORCE = sys.argv[6]
	if FORCE == 'MISSING':       FORCE = ''
	RESOLUTION = sys.argv[7]
	if RESOLUTION == 'MISSING':  RESOLUTION = ''
	RMAX = sys.argv[8]
	if RMAX == 'MISSING':        RMAX = ''
	LEVS = sys.argv[9]
	if LEVS == 'MISSING':        LEVS = ''
	NMLIST = sys.argv[10]
	if NMLIST == 'MISSING':
		print(f'ERROR: Master Namelist can\'t be {NMLIST}.')
		sys.exit()
	NMLDIR = f'{GPLOT_DIR}/parm'
	if os.path.exists(NMLIST):
		MASTER_NML_IN = NMLIST
	elif os.path.exists(f'{GPLOT_DIR}/parm/{NMLIST}'):
		MASTER_NML_IN = NML_DIR+'/'+NMLIST
	else:
		print("ERROR: I couldn't find the Master Namelist.")
		sys.exit()
	PYTHONDIR = f'{GPLOT_DIR}/sorc/GPLOT/python'
	
	
	# Read the master namelist
	DSOURCE = subprocess.run(['grep','^DSOURCE',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1]
	EXPT = subprocess.run(['grep','^EXPT',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1]
	ODIR = subprocess.run(['grep','^ODIR =',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1].strip()
	BASEDIR = ODIR
	try:
		ODIR_TYPE = int(subprocess.run(['grep','^ODIR_TYPE',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1])
	except:
		ODIR_TYPE = 0
	if ODIR_TYPE == 1:
		ODIR = ODIR+'/polar/'
		BASEDIR = BASEDIR+'/'
	else:
		ODIR = ODIR+'/'+EXPT.strip()+'/'+IDATE.strip()+'/polar/'
		BASEDIR = BASEDIR+'/'+EXPT.strip()+'/'+IDATE.strip()+'/'
	
	try:
		DO_CONVERTGIF = subprocess.run(['grep','^DO_CONVERTGIF',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1].strip();
		DO_CONVERTGIF = (DO_CONVERTGIF == 'True');
	except:
		DO_CONVERTGIF = False;
	
	figext = '.png';
	
	# Create the temporary directory for GrADs files
	TMPDIR = BASEDIR.strip()+'grads/'
	if not os.path.exists(TMPDIR):  os.mkdir(TMPDIR)
	
	# Define some important file names
	UNPLOTTED_FILE = f'{ODIR.strip()}UnplottedFiles.{DOMAIN.strip()}.{TIER.strip()}.{SID.strip()}.log'
	PLOTTED_FILE = f'{ODIR.strip()}PlottedFiles.{DOMAIN.strip()}.{TIER.strip()}.{SID.strip()}.log'
	ALLFHR_FILE = f'{ODIR.strip()}AllForecastHours.{DOMAIN.strip()}.{TIER.strip()}.{SID.strip()}.log'
	STATUS_FILE = f'{ODIR.strip()}status.{DOMAIN.strip()}.{TIER.strip()}.{SID.strip()}.log'
	ST_LOCK_FILE = f'{STATUS_FILE}.lock'
	ATCF_FILE = f'{ODIR.strip()}ATCF_FILES.dat'
	
	
	#Get parameters from input file
	resolution, rmax, zsize_pressure = float(RESOLUTION), float(RMAX), int(LEVS)
	
	# Get the ATCF file.
	ATCF_LIST = np.genfromtxt(f'{ODIR}ATCF_FILES.dat',dtype='str')
	if ATCF_LIST.size > 1:
		print('Found multiple ATCFs')
		ATCF = ATCF_LIST[[i for i, s in enumerate(ATCF_LIST) if str(SID+'.').lower() in s][:]][0]
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
	
	# Get the list of forecast lead time in hours
	FHR_LIST = np.array( np.genfromtxt(ALLFHR_FILE,dtype='int') )
	if (FHR_LIST.size == 1):
		FHR_LIST = np.append(FHR_LIST,"999")
		UNPLOTTED_LIST = np.append(UNPLOTTED_LIST,"MISSING")
	
	# Define executables
	X_G2CTL = f'{GPLOT_DIR}/sorc/GPLOT/grads/g2ctl.pl'
	
	for (FILE,fff) in zip(UNPLOTTED_LIST,np.array(range(UNPLOTTED_LIST.size))):
	
		if FILE == 'MISSING':  continue
	
		print(f'MSG: Working on this file --> {FILE}  {str(fff)}  {datetime.datetime.now()}')
	
		os.system(f'lockfile -r-1 -l 180 {ST_LOCK_FILE}')
		os.system(f'echo "working" > {STATUS_FILE}')
		os.system(f'rm -f {ST_LOCK_FILE}')
	
		# Get some useful information about the file name
		FILE_BASE = os.path.basename(FILE)
		FILE_DIR = os.path.dirname(FILE)
	
		# Find the index of the forecast lead time in the ATCF file.
		FHR = int(FHR_LIST[fff])
		FHRIND = [i for i, s in enumerate(ATCF_DATA[:,5]) if int(s)==FHR]
	
		# Get coordinate information from ATCF
		lonstr = ATCF_DATA[list(FHRIND),7][0]
		lonstr1 = lonstr[::-1]
		lonstr1 = lonstr1[1:]
		lonstr1 = lonstr1[::-1]
		lonstr2 = lonstr[::-1]
		lonstr2 = lonstr2[0]
		if (lonstr2 == 'W'):	centerlon = 360-float(lonstr1)/10
		else:			centerlon = float(lonstr1)/10
		latstr = ATCF_DATA[list(FHRIND),6][0]
		latstr1 = latstr[::-1]
		latstr1 = latstr1[1:]
		latstr1 = latstr1[::-1]
		latstr2 = latstr[::-1]
		latstr2 = latstr2[0]
		if (latstr2 == 'N'):	centerlat = float(latstr1)/10
		else:			centerlat = -1*float(latstr1)/10
		forecastinit = ATCF_DATA[list(FHRIND),2][0]
		maxwind = ATCF_DATA[list(FHRIND),8][0]
		minpressure = ATCF_DATA[list(FHRIND),9][0]
		rmwnmi = ATCF_DATA[list(FHRIND),19][0]
		print(f'MSG: centerlat,centerlon = {centerlat},{centerlon}')
	
		if ( centerlat > 50.0):
			# Write the input file to a log to mark that it has ben processed
			PLOTTED_FILES=ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
			os.system('echo "'+str(FILE)+'" >> '+PLOTTED_FILES)
			os.system('sort -u '+PLOTTED_FILES+' > '+PLOTTED_FILES+'.TMP')
			os.system('mv '+PLOTTED_FILES+'.TMP '+PLOTTED_FILES)
			break
	
		figuretest = np.shape([g for g in glob.glob(f"{ODIR}/*{TCNAME.lower()}*{FHR:03}{figext}")])[0]
		if (figuretest < 1):
			print(f'MSG: I can\'t find the graphical products for this lead time (figuretest={figuretest}).')
			#print('h = ',list(FHRIND))
	
			# Check that the data file 'FILE' exists
			gribfiletest = os.system(f'ls {FILE} >/dev/null')
	
			if (gribfiletest < 1):
	
				# Create the GrADs control file, if it hasn't already been created.
				CTL_FILE = f'{TMPDIR}{FILE_BASE}.ctl'
				IDX_FILE = f'{TMPDIR}{FILE_BASE}.2.idx'
				LOCK_FILE = f'{TMPDIR}{FILE_BASE}.lock'
				while os.path.exists(LOCK_FILE):
					print(f'MSG: {TMPDIR}{FILE_BASE} is locked. Sleeping for 5 seconds.')
					time.sleep(5)
					LOCK_TEST = os.popen(f'find {LOCK_FILE} -mmin +3 2>/dev/null').read()
					if LOCK_TEST:  os.system(f'rm -f {LOCK_FILE}')
	
				if not os.path.exists(CTL_FILE) or os.stat(CTL_FILE).st_size == 0:
					print('MSG: GrADs control file not found. Creating it now.')
					os.system(f'lockfile -r-1 -l 180 {LOCK_FILE}')
					command = f'{X_G2CTL} {FILE} {IDX_FILE} > {CTL_FILE}'
					os.system(command)
					command2 = f'gribmap -i {CTL_FILE} -big'
					os.system(command2)
					os.system(f'rm -f {LOCK_FILE}')
	
				while not os.path.exists(IDX_FILE):
					print('MSG: GrADs index file not found. Sleeping for 5 seconds.')
					time.sleep(5)
				
				# Open GrADs data file
				print('MSG: GrADs control and index files should be available.')
				ga(f'open {CTL_FILE}')
				env = ga.env()
	
				#Define how big of a box you want, based on lat distance
				yoffset = 6
				xoffset = None
				NL = yoffset-1
				while not xoffset:
					if NL > 25:
						print(f'ERROR: YOU NEED A BIGGER BOX THAN {NL} DEGREES. rmax={rmax}, test={test}, centerlat={centerlat}')
						sys.exit(1)
					NL = NL+1
					test = np.cos((abs(centerlat)+yoffset)*3.14159/180)*111.1*NL
					if test > rmax:  xoffset,yoffset = NL,NL
				print(f'MSG: Will use a box with side of {NL} degrees.')
	
				# Setup lat, lon boundaries
				ga('set z 1')
				lonmax, lonmin = centerlon+xoffset, centerlon-xoffset
				ga(f'set lon {lonmin} {lonmax}')
				latmax, latmin = centerlat+yoffset, centerlat-yoffset
				ga(f'set lat {latmin} {latmax}')

				# Fix to integer boundaries to prevent mismatching array shapes
				env = ga.env()
				ga(f'set x {env.xi[0]} {env.xi[1]}')
				ga(f'set y {env.yi[0]} {env.yi[1]}')

				# Read lat & lon
				lon = ga.exp('lon')[0,:]
				lat = ga.exp('lat')[:,0]
				#print(lat.shape, lon.shape)

				# Get pressure levels
				ga(f'set z 1 {zsize_pressure}')
				levs = ga.exp('lev')
				z = np.zeros((zsize_pressure,0))*np.nan
				for i in range(zsize_pressure):  z[i] = levs[1,1,i]
	
				#Get data
				print('MSG: Getting Data Now. Using an xoffset of '+str(xoffset)+' degrees')
				start = time.perf_counter()
				#varNames1 = ['ugrdprs', 'vgrdprs', 'vvelprs', 'refdprs', 'hgtprs', 'tmpprs', 'spfhprs', 'rhprs']
				#varNames2 = ['uwind', 'vwind', 'omega', 'dbz', 'hgt', 'temp', 'q', 'rh']
				#Data = multiprocess_prs_vars(ga=ga, names=varNames1)
				#global var
				#for var,name in zip(Data,varNames2):  exec(f'{name} = var', globals())
				#del var
				uwind, vwind, omega = ga.exp('ugrdprs'), ga.exp('vgrdprs'), ga.exp('vvelprs')
				print('MSG: Done reading: u,v,w')
				dbz, hgt, temp = ga.exp('refdprs'), ga.exp('hgtprs'), ga.exp('tmpprs')
				print('MSG: Done reading: dbz, hgt, temp')
				q, rh = ga.exp('spfhprs'), ga.exp('rhprs')
				print('MSG: Done reading: q, rh')
				
				#Get 2-d Data
				ga('set z 1')
				u10, v10 = ga.exp('ugrd10m'), ga.exp('vgrd10m')
				if DSOURCE == 'HAFS':	mslp = ga.exp('msletmsl')
				else:			mslp = ga.exp('prmslmsl')
				tmp2m, q2m, rh2m = ga.exp('tmp2m'), ga.exp('spfh2m'), ga.exp('rh2m')
				print('MSG: Done reading u10,v10,mslp,tmp2m,q2m')
				mixr2m = q2m/(1-q2m)
				temp_v_2m = tmp2m*(1+0.61*mixr2m)
				rho2m = mslp/(287*temp_v_2m)
				print('MSG: Done reading surface vars (e.g., u10,v10)')
				
				#Get u850, v850, u200, v200 for Shear Calculation
				ga('set lev 850')
				u850, v850, z850 = ga.exp('ugrdprs'), ga.exp('vgrdprs'), ga.exp('hgtprs')
				ga('set lev 200')
				u200, v200, z200 = ga.exp('ugrdprs'), ga.exp('vgrdprs'), ga.exp('hgtprs')
				ga('set z 1')
				finish = time.perf_counter()
				print(f'MSG: Total time to read data: {finish-start:.2f} second(s)')
				
				#Get W from Omega
				#w = -omega/(rho*g)
				#rho = p/(Rd*Tv)
				mixr = q/(1-q)
				temp_v = temp*(1+0.61*mixr)
				rho = (levs*1e2)/(287*temp_v)
				wwind = -omega/(rho*9.81)
	
				#Get storm-centered data
				lon_sr, lat_sr = lon-centerlon, lat-centerlat
				x_sr = lon_sr*111.1e3*np.cos(centerlat*3.14159/180)
				y_sr = lat_sr*111.1e3
	
				#Define the polar coordinates needed
				r = np.linspace(0,rmax,(int(rmax//resolution)+1))
				pi = np.arccos(-1)
				theta = np.arange(0,2*pi+pi/36,pi/36)
				R, THETA = np.meshgrid(r, theta)
				XI = R*np.cos(THETA)
				YI = R*np.sin(THETA)
	
				x_sr = np.round(x_sr/1000,3)
				y_sr = np.round(y_sr/1000,3)
	
				x_sr_2 = np.linspace(x_sr.min(), x_sr.max(), x_sr.size)
				y_sr_2 = np.linspace(y_sr.min(), y_sr.max(), y_sr.size)
	
				rnorm = np.linspace(0,6,121)
				Rnorm, THETAnorm = np.meshgrid(rnorm,theta)
				XInorm = Rnorm * np.cos(THETAnorm)
				YInorm = Rnorm * np.sin(THETAnorm)
				
				# Interpolate to Height Coordinates
				print('MSG: Doing Height Coordinate Interpolation Now')
				start = time.perf_counter()
				uwindT = np.transpose(uwind,(2,0,1))
				vwindT = np.transpose(vwind,(2,0,1))
				wwindT = np.transpose(wwind,(2,0,1))
				dbzT = np.transpose(dbz,(2,0,1))
				hgtT = np.transpose(hgt,(2,0,1))
				tempT = np.transpose(temp,(2,0,1))
				qT = np.transpose(q,(2,0,1))
				rhT = np.transpose(rh,(2,0,1))
				rhoT = np.transpose(rho,(2,0,1))
				pressureT = np.transpose(levs*1e2,(2,0,1))
				heightlevs = np.linspace(0,18000,37)
				zsize = np.shape(heightlevs)[0] #Change zsize here

				varInList = [uwindT, vwindT, wwindT, dbzT, tempT, qT, rhT, pressureT]
				HeightData = mproc.multiprocess_height_vars(hgt=hgtT, varList=varInList, levels=heightlevs)
				uwind, vwind, wwind = HeightData[0,:,:,:], HeightData[1,:,:,:], HeightData[2,:,:,:]
				dbz, temp, q = HeightData[3,:,:,:], HeightData[4,:,:,:], HeightData[5,:,:,:]
				rh, pressure = HeightData[6,:,:,:], HeightData[7,:,:,:]
				
				uwind[:,:,0], vwind[:,:,0], wwind[:,:,0] = u10, v10, np.nan
				dbz[:,:,0], temp[:,:,0], pressure[:,:,0] = np.nan, tmp2m, mslp
				q[:,:,0], rh[:,:,0] = q2m, rh2m

				heightlevs_pbl = np.linspace(0,3000,31)
				zsize_pbl = np.shape(heightlevs_pbl)[0]
				varList = [uwindT, vwindT, rhoT, pressureT]
				varNames = ['uwind_pbl', 'vwind_pbl', 'rho_pbl', 'pressure_pbl']
				HeightData = mproc.multiprocess_height_vars(hgt=hgtT, varList=varList, levels=heightlevs_pbl)
				uwind_pbl, vwind_pbl = HeightData[0,:,:,:], HeightData[1,:,:,:]
				rho_pbl, pressure_pbl = HeightData[2,:,:,:], HeightData[3,:,:,:]
				
				uwind_pbl[:,:,0], vwind_pbl[:,:,0] = u10, v10
				pressure_pbl[:,:,0], rho_pbl[:,:,0] = mslp, rho2m
				finish = time.perf_counter()
				print(f'MSG: Total time for height interpolation: {finish-start:.2f} second(s)')



				# DO POLAR INTERPOLATION
				print('MSG: Doing the Polar Interpolation Now')
	
				#First initialize u_p and v_p
				#u_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				#v_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				#w_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				#dbz_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				#temp_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				#q_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				#rh_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				#pressure_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
	
				#for k in range(zsize):
				#	f_uwind = interpolate.RegularGridInterpolator((y_sr, x_sr), uwind[:,:,k])
				#	f_vwind = interpolate.RegularGridInterpolator((y_sr, x_sr), vwind[:,:,k])
				#	f_wwind = interpolate.RegularGridInterpolator((y_sr, x_sr), wwind[:,:,k])
				#	f_dbz = interpolate.RegularGridInterpolator((y_sr, x_sr), dbz[:,:,k])
				#	f_temp = interpolate.RegularGridInterpolator((y_sr, x_sr), temp[:,:,k])
				#	f_q = interpolate.RegularGridInterpolator((y_sr, x_sr), q[:,:,k])
				#	f_rh = interpolate.RegularGridInterpolator((y_sr, x_sr), rh[:,:,k])
				#	f_pressure = interpolate.RegularGridInterpolator((y_sr, x_sr), pressure[:,:,k])
				
				#	u_p[:,:,k] = f_uwind((YI,XI),method='linear')
				#	v_p[:,:,k] = f_vwind((YI,XI),method='linear')
				#	w_p[:,:,k] = f_wwind((YI,XI),method='linear')
				#	dbz_p[:,:,k] = f_dbz((YI,XI),method='linear')
				#	temp_p[:,:,k] = f_temp((YI,XI),method='linear')
				#	q_p[:,:,k] = f_q((YI,XI),method='linear')
				#	rh_p[:,:,k] = f_rh((YI,XI),method='linear')
				#	pressure_p[:,:,k] = f_pressure((YI,XI),method='linear')
				start = time.perf_counter()
				varList = [np.transpose(uwind,(2,0,1)), np.transpose(vwind, (2,0,1)), np.transpose(wwind, (2,0,1)), \
					   np.transpose(dbz, (2,0,1)), np.transpose(temp, (2,0,1)), np.transpose(q, (2,0,1)), \
					   np.transpose(rh, (2,0,1)), np.transpose(pressure, (2,0,1))]
				PolarData = mproc.multiprocess_polar_vars(x_sr, y_sr, XI, YI, varList=varList, levels=heightlevs)
				u_p, v_p, w_p = PolarData[0,:,:,:], PolarData[1,:,:,:], PolarData[2,:,:,:]
				dbz_p, temp_p, q_p = PolarData[3,:,:,:], PolarData[4,:,:,:], PolarData[5,:,:,:]
				rh_p, pressure_p = PolarData[6,:,:,:], PolarData[7,:,:,:]
				finish = time.perf_counter()
				print(f'MSG: Total time for polar interpolation: {finish-start:.2f} second(s)')
				
	
				#Calculate tangential and radial wind
				vt_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				ur_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				for j in range(np.shape(XI)[1]):
					for k in range(zsize):
						vt_p[:,j,k] = -u_p[:,j,k]*np.sin(theta)+v_p[:,j,k]*np.cos(theta)
						ur_p[:,j,k] = u_p[:,j,k]*np.cos(theta)+v_p[:,j,k]*np.sin(theta)
	
				#Do PBL Interpolation
				u_pbl_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize_pbl))*np.nan
				v_pbl_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize_pbl))*np.nan
	
				for k in range(zsize_pbl):
					f_uwind_pbl = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), uwind_pbl[:,:,k])
					f_vwind_pbl = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), vwind_pbl[:,:,k])
			
					u_pbl_p[:,:,k] = f_uwind_pbl((YI,XI),method='linear')
					v_pbl_p[:,:,k] = f_vwind_pbl((YI,XI),method='linear')
	
				vt_pbl_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize_pbl))*np.nan
				ur_pbl_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize_pbl))*np.nan
				for j in range(np.shape(XI)[1]):
					for k in range(zsize_pbl):
						vt_pbl_p[:,j,k] = -u_pbl_p[:,j,k]*np.sin(theta)+v_pbl_p[:,j,k]*np.cos(theta)
						ur_pbl_p[:,j,k] = u_pbl_p[:,j,k]*np.cos(theta)+v_pbl_p[:,j,k]*np.sin(theta)
	
	
				#Get Polar u10, v10, u850, v850, u200, v200
				u10_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan	
				v10_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				f_u10 = interpolate.RegularGridInterpolator((y_sr, x_sr), u10[:,:])
				f_v10 = interpolate.RegularGridInterpolator((y_sr, x_sr), v10[:,:])
	
				u10_p[:,:] = f_u10((YI,XI),method='linear')
				v10_p[:,:] = f_v10((YI,XI),method='linear')
	
				u200_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				v200_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				u850_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				v850_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan			
				vt10_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				ur10_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				vt850_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				ur850_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				vt850_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				ur850_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				vt200_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				ur200_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
	
				f_u200 = interpolate.RegularGridInterpolator((y_sr, x_sr), u200[:,:])
				f_v200 = interpolate.RegularGridInterpolator((y_sr, x_sr), v200[:,:])
				f_u850 = interpolate.RegularGridInterpolator((y_sr, x_sr), u850[:,:])
				f_v850 = interpolate.RegularGridInterpolator((y_sr, x_sr), v850[:,:])
				
				u200_p = f_u200((YI,XI),method='linear')
				v200_p = f_v200((YI,XI),method='linear')
				u850_p = f_u850((YI,XI),method='linear')
				v850_p = f_v850((YI,XI),method='linear')
		
				for j in range(np.shape(XI)[1]):
					vt10_p[:,j] = -u10_p[:,j]*np.sin(theta)+v10_p[:,j]*np.cos(theta)
					ur10_p[:,j] = u10_p[:,j]*np.cos(theta)+v10_p[:,j]*np.sin(theta)
					vt850_p[:,j] = -u850_p[:,j]*np.sin(theta)+v850_p[:,j]*np.cos(theta)
					ur850_p[:,j] = u850_p[:,j]*np.cos(theta)+v850_p[:,j]*np.sin(theta)
					vt200_p[:,j] = -u200_p[:,j]*np.sin(theta)+v200_p[:,j]*np.cos(theta)
					ur200_p[:,j] = u200_p[:,j]*np.cos(theta)+v200_p[:,j]*np.sin(theta)			
				
				#Calculate shear
				#Two Metrics: 200 km - RMAX average, 0-500 km/rmax with vortex removed
				
				#First Do the 200 km - rmax average
				u850_p_ring = u850_p[:,int(np.round(200/resolution)):int(np.round(rmax/resolution))]
				v850_p_ring = v850_p[:,int(np.round(200/resolution)):int(np.round(rmax/resolution))]
				u200_p_ring = u200_p[:,int(np.round(200/resolution)):int(np.round(rmax/resolution))]
				v200_p_ring = v200_p[:,int(np.round(200/resolution)):int(np.round(rmax/resolution))]

				with warnings.catch_warnings():
					warnings.filterwarnings(action='ignore', message='Mean of empty slice')
					u850_p_ring_mean = np.nanmean(np.nanmean(u850_p_ring))
					v850_p_ring_mean = np.nanmean(np.nanmean(v850_p_ring))
					u200_p_ring_mean = np.nanmean(np.nanmean(u200_p_ring))
					v200_p_ring_mean = np.nanmean(np.nanmean(v200_p_ring))
	
				ushear1 = u200_p_ring_mean-u850_p_ring_mean
				vshear1 = v200_p_ring_mean-v850_p_ring_mean
	
				shearmag = np.hypot(ushear1,vshear1)
				sheardir = np.arctan2(vshear1,ushear1)*180.0/pi
				if np.isnan(shearmag):
					ga('close 1')
					io.update_plottedfile(ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log', FILE)
					continue
				else:
					shearstring = str(int(np.round(shearmag*1.94,0)))
	
				#Convert shear to meteorological convention
				if sheardir <=90:	sheardir_met = 90-sheardir
				else:			sheardir_met = 360-(sheardir-90)
	
				#Also convert shear to positive value
				if sheardir <0:		sheardir_math = sheardir+360
				else:			sheardir_math = sheardir
	
				#Round shear vector to nearest 5
				sheardir_5deg = (np.round((sheardir_math/5))*5)
	
				#Get index for shear
				sheardir_index = int(sheardir_5deg/5)
	
				vt_p_filtered = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				ur_p_filtered = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				u_p_filtered = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				v_p_filtered = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				u850_p_filtered = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				v850_p_filtered = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				u200_p_filtered = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				v200_p_filtered = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				vt850_p_filtered = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				ur850_p_filtered = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				vt200_p_filtered = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				ur200_p_filtered = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan

				with warnings.catch_warnings():
					warnings.filterwarnings(action='ignore', message='Mean of empty slice')	
					vt850_p_filtered[:,:] = vt850_p[:,:]-np.nanmean(vt850_p,0)
					ur850_p_filtered[:,:] = ur850_p[:,:]-np.nanmean(ur850_p,0)
					vt200_p_filtered[:,:] = vt200_p[:,:]-np.nanmean(vt200_p,0)
					ur200_p_filtered[:,:] = ur200_p[:,:]-np.nanmean(ur200_p,0)
	
				for k in range(zsize):
					with warnings.catch_warnings():
						warnings.filterwarnings(action='ignore', message='Mean of empty slice')
						vt_p_filtered[:,:,k] = vt_p[:,:,k]-np.nanmean(vt_p[:,:,k],0)
						ur_p_filtered[:,:,k] = ur_p[:,:,k]-np.nanmean(ur_p[:,:,k],0)

					for j in range(np.shape(XI)[1]):
						u_p_filtered[:,j,k] = ur_p_filtered[:,j,k]*np.cos(theta) - vt_p_filtered[:,j,k]*np.sin(theta)
						v_p_filtered[:,j,k] = ur_p_filtered[:,j,k]*np.sin(theta) + vt_p_filtered[:,j,k]*np.cos(theta)
	
				for j in range(np.shape(XI)[1]):
					u850_p_filtered[:,j] = ur850_p_filtered[:,j]*np.cos(theta) - vt850_p_filtered[:,j]*np.sin(theta)
					v850_p_filtered[:,j] = ur850_p_filtered[:,j]*np.sin(theta) + vt850_p_filtered[:,j]*np.cos(theta)
					u200_p_filtered[:,j] = ur200_p_filtered[:,j]*np.cos(theta) - vt200_p_filtered[:,j]*np.sin(theta)
					v200_p_filtered[:,j] = ur200_p_filtered[:,j]*np.sin(theta) + vt200_p_filtered[:,j]*np.cos(theta)
	
				ushear_p = u200_p_filtered-u850_p_filtered
				vshear_p = v200_p_filtered-v850_p_filtered

				with warnings.catch_warnings():
					warnings.filterwarnings(action='ignore', message='Mean of empty slice')
					ushear2 = np.nanmean(np.nanmean(ushear_p))
					vshear2 = np.nanmean(np.nanmean(vshear_p))
	
				#Rotate variables based on the shear
				#3D
				u_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				v_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				w_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				dbz_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				temp_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				q_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				rh_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				vt_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				ur_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				pressure_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
	
				#2D
				u10_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				v10_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				vt10_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				ur10_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				u200_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				v200_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				u850_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				v850_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
	
				#3D			
				u_p_rot = np.roll(u_p,[-sheardir_index, 0, 0],axis=(0,1,2))
				v_p_rot = np.roll(v_p,[-sheardir_index, 0, 0],axis=(0,1,2))
				w_p_rot = np.roll(w_p,[-sheardir_index, 0, 0],axis=(0,1,2))
				dbz_p_rot = np.roll(dbz_p,[-sheardir_index, 0, 0],axis=(0,1,2))
				temp_p_rot = np.roll(temp_p,[-sheardir_index, 0, 0],axis=(0,1,2))
				q_p_rot = np.roll(q_p,[-sheardir_index, 0, 0],axis=(0,1,2))
				rh_p_rot = np.roll(rh_p,[-sheardir_index, 0, 0],axis=(0,1,2))
				vt_p_rot = np.roll(vt_p,[-sheardir_index, 0, 0],axis=(0,1,2))
				ur_p_rot = np.roll(ur_p,[-sheardir_index, 0, 0],axis=(0,1,2))
				pressure_p_rot = np.roll(ur_p,[-sheardir_index, 0, 0],axis=(0,1,2))
	
				#2D
				u10_p_rot = np.roll(u10_p,[-sheardir_index, 0],axis=(0,1))
				v10_p_rot = np.roll(v10_p,[-sheardir_index, 0],axis=(0,1))
				vt10_p_rot = np.roll(vt10_p,[-sheardir_index, 0],axis=(0,1))
				ur10_p_rot = np.roll(ur10_p,[-sheardir_index, 0],axis=(0,1))
				u200_p_rot = np.roll(u200_p,[-sheardir_index, 0],axis=(0,1))
				v200_p_rot = np.roll(v200_p,[-sheardir_index, 0],axis=(0,1))
				u850_p_rot = np.roll(u850_p,[-sheardir_index, 0],axis=(0,1))
				v850_p_rot = np.roll(v850_p,[-sheardir_index, 0],axis=(0,1))
	
				#Calculate azimuthal means
				with warnings.catch_warnings():
					warnings.filterwarnings(action='ignore', message='Mean of empty slice')
	
					vt_p_mean = np.nanmean(vt_p,0)
					ur_p_mean = np.nanmean(ur_p,0)
					w_p_mean = np.NaN if np.isnan(w_p).all() else np.nanmean(w_p,0)
					dbz_p_mean = np.nanmean(dbz_p,0)
					temp_p_mean = np.nanmean(temp_p,0)
					q_p_mean = np.nanmean(q_p,0)
					rh_p_mean = np.nanmean(rh_p,0)
					pressure_p_mean = np.nanmean(pressure_p,0)
		
					vt_pbl_p_mean = np.nanmean(vt_pbl_p,0)
					ur_pbl_p_mean = np.nanmean(ur_pbl_p,0)
	
				#Calculate upshear, downshear, right of shear, and left of shear
				ur_p_downshear = np.concatenate((ur_p_rot[1:9,:,:],ur_p_rot[63:72,:,:]),axis=0)
				w_p_downshear = np.concatenate((w_p_rot[1:9,:,:],w_p_rot[63:72,:,:]),axis=0)
				dbz_p_downshear = np.concatenate((dbz_p_rot[1:9,:,:],dbz_p_rot[63:72,:,:]),axis=0)
				rh_p_downshear = np.concatenate((rh_p_rot[1:9,:,:],rh_p_rot[63:72,:,:]),axis=0)
	
				ur_p_upshear = ur_p_rot[27:45,:,:]
				w_p_upshear = w_p_rot[27:45,:,:]
				dbz_p_upshear = dbz_p_rot[27:45,:,:]
				rh_p_upshear = rh_p_rot[27:45,:,:]
	
				ur_p_leftshear = ur_p_rot[9:27,:,:]
				w_p_leftshear = w_p_rot[9:27,:,:]
				dbz_p_leftshear = dbz_p_rot[9:27,:,:]
				rh_p_leftshear = rh_p_rot[9:27,:,:]
	
				ur_p_rightshear = ur_p_rot[45:63,:,:]
				w_p_rightshear = w_p_rot[45:63,:,:]
				dbz_p_rightshear = dbz_p_rot[45:63,:,:]
				rh_p_rightshear = rh_p_rot[45:63,:,:]
	
				#Calculate shear-relative means
				with warnings.catch_warnings():
					warnings.filterwarnings(action='ignore', message='Mean of empty slice')
	
					ur_p_downshear_mean = np.nanmean(ur_p_downshear,0)
					w_p_downshear_mean = np.nanmean(w_p_downshear,0)
					dbz_p_downshear_mean = np.nanmean(dbz_p_downshear,0)
					rh_p_downshear_mean = np.nanmean(rh_p_downshear,0)
	
					ur_p_upshear_mean = np.nanmean(ur_p_upshear,0)
					w_p_upshear_mean = np.nanmean(w_p_upshear,0)
					dbz_p_upshear_mean = np.nanmean(dbz_p_upshear,0)
					rh_p_upshear_mean = np.nanmean(rh_p_upshear,0)
	
					ur_p_leftshear_mean = np.nanmean(ur_p_leftshear,0)
					w_p_leftshear_mean = np.nanmean(w_p_leftshear,0)
					dbz_p_leftshear_mean = np.nanmean(dbz_p_leftshear,0)
					rh_p_leftshear_mean = np.nanmean(rh_p_leftshear,0)
	
					ur_p_rightshear_mean = np.nanmean(ur_p_rightshear,0)
					w_p_rightshear_mean = np.nanmean(w_p_rightshear,0)
					dbz_p_rightshear_mean = np.nanmean(dbz_p_rightshear,0)
					rh_p_rightshear_mean = np.nanmean(rh_p_rightshear,0)
	
				#Calculate wavenumber-0,1,2 fits for both rotated and non-rotated grids 
				dbz5_p = dbz_p[:,:,10]
				dbz5_p_w0 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				dbz5_p_w1 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				dbz5_p_w2 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan	
				dbz5_p_whigher = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
		
				rh5_p = rh_p[:,:,10]
				rh5_p_w0 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				rh5_p_w1 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				rh5_p_w2 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
	
				vt10_p_w0 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				vt10_p_w1 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				vt10_p_w2 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				vt10_p_whigher  = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				ur10_p_w0 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				ur10_p_w1 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				ur10_p_w2 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan			
	
				for j in range(np.shape(r)[0]):
					dbzdata = dbz5_p[:,j]
					fourier_dbz = np.fft.fft(dbzdata)/len(dbzdata)
					amp0_dbz = np.real(fourier_dbz[0])
					A1_dbz = 2*np.real(fourier_dbz[1])
					B1_dbz = -2*np.imag(fourier_dbz[1])
					A2_dbz = 2*np.real(fourier_dbz[2])
					B2_dbz = -2*np.imag(fourier_dbz[2])
					dbz5_p_w0[:,j] = amp0_dbz
					dbz5_p_w1[:,j] = A1_dbz*np.cos(theta) + B1_dbz*np.sin(theta)
					dbz5_p_w2[:,j] = A2_dbz*np.cos(2*theta) + B2_dbz*np.sin(2*theta)
					dbz5_p_whigher[:,j] = 0
					for h in range (2,int((np.shape(theta)[0]+1)/2)):
						A = 2*np.real(fourier_dbz[h])
						B = -2*np.imag(fourier_dbz[h])
						dbz5_p_whigher[:,j] = dbz5_p_whigher[:,j]+A*np.cos(h*theta) + B*np.sin(h*theta)
	
					rhdata = rh5_p[:,j]
					fourier_rh = np.fft.fft(rhdata)/len(rhdata)
					amp0_rh = np.real(fourier_rh[0])
					A1_rh = 2*np.real(fourier_rh[1])
					B1_rh = -2*np.imag(fourier_rh[1])
					A2_rh = 2*np.real(fourier_rh[2])
					B2_rh = -2*np.imag(fourier_rh[2])
					rh5_p_w0[:,j] = amp0_rh
					rh5_p_w1[:,j] = A1_rh*np.cos(theta) + B1_rh*np.sin(theta)
					rh5_p_w2[:,j] = A2_rh*np.cos(2*theta) + B2_rh*np.sin(2*theta)
	
					vt10data = vt10_p[:,j]
					fourier_vt10 = np.fft.fft(vt10data)/len(vt10data)
					amp0_vt10 = np.real(fourier_vt10[0])
					A1_vt10 = 2*np.real(fourier_vt10[1])
					B1_vt10 = -2*np.imag(fourier_vt10[1])
					A2_vt10 = 2*np.real(fourier_vt10[2])
					B2_vt10 = -2*np.imag(fourier_vt10[2])
					vt10_p_w0[:,j] = amp0_vt10
					vt10_p_w1[:,j] = A1_vt10*np.cos(theta) + B1_vt10*np.sin(theta)
					vt10_p_w2[:,j] = A2_vt10*np.cos(2*theta) + B2_vt10*np.sin(2*theta)
					vt10_p_whigher[:,j] = 0
					for h in range (2,int((np.shape(theta)[0]+1)/2)):
						A = 2*np.real(fourier_vt10[h])
						B = -2*np.imag(fourier_vt10[h])
						vt10_p_whigher[:,j] = vt10_p_whigher[:,j]+A*np.cos(h*theta) + B*np.sin(h*theta)
							
					ur10data = ur10_p[:,j]
					fourier_ur10 = np.fft.fft(ur10data)/len(ur10data)
					amp0_ur10 = np.real(fourier_ur10[0])
					A1_ur10 = 2*np.real(fourier_ur10[1])
					B1_ur10 = -2*np.imag(fourier_ur10[1])
					A2_ur10 = 2*np.real(fourier_ur10[2])
					B2_ur10 = -2*np.imag(fourier_ur10[2])
					ur10_p_w0[:,j] = amp0_ur10
					ur10_p_w1[:,j] = A1_ur10*np.cos(theta) + B1_ur10*np.sin(theta)
					ur10_p_w2[:,j] = A2_ur10*np.cos(2*theta) + B2_ur10*np.sin(2*theta)
			
				#Calculate 2-km RMW and Average Vmax
	
				rmw_mean = np.ones(zsize)*np.nan
				rmw_mean_index = np.ones(zsize)*np.nan
				vt_rmw_mean = np.ones(zsize)*np.nan

				for k in range(zsize):
					rmw_mean[k] = np.round(np.median(r[vt_p_mean[:,k] > 0.95*np.nanmax(vt_p_mean[:,k])]))
					rmw_mean_index[k] = np.argmin(abs(r-rmw_mean[k])) 
					vt_rmw_mean[k] = vt_p_mean[np.int(rmw_mean_index[k]),k]
		
				rmw_2km = rmw_mean[4]
				vt_p_mean_max = np.max(vt_p_mean,0)
				vt_p_mean_max_2km = vt_p_mean_max[4]
				if np.isnan(rmw_2km):
					ga('close 1')
					io.update_plottedfile(ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log', FILE)
					continue
				else:
					rmwstring = str(int(np.round(rmw_2km*0.54,0)))
				if np.isnan(vt_p_mean_max_2km):
					ga('close 1')
					io.update_plottedfile(ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log', FILE)
					continue
				else:
					vmaxstring = str(int(np.round(vt_p_mean_max_2km*1.94,0)))
	
				if rmax/rmw_2km < np.max(rnorm): #Quick Fix For Big RMWs
					rnorm_max = np.round(rmax/rmw_2km,2)-math.fmod(np.round(rmax/rmw_2km,2),0.05)
					rnorm = np.linspace(0,rnorm_max,(int(rnorm_max/0.05))+1)
					Rnorm, THETAnorm = np.meshgrid(rnorm,theta)
					XInorm = Rnorm * np.cos(THETAnorm)
					YInorm = Rnorm * np.sin(THETAnorm)
				
				rmw_pbl_mean = np.ones(zsize_pbl)*np.nan
				for k in range(zsize_pbl):
					rmw_pbl_mean[k] = np.round(np.median(r[vt_pbl_p_mean[:,k] > 0.95*np.max(vt_pbl_p_mean[:,k])]))
	
		
				#Calculate Variables Normalized by RMW
				vt_p_mean_norm = np.ones((np.shape(rnorm)[0],zsize))*np.nan
				ur_p_mean_norm = np.ones((np.shape(rnorm)[0],zsize))*np.nan
				if ( rmw_2km < 200):
					for k in range(zsize):
						f_vt_p_mean_norm = scipy.interpolate.interp1d((r/rmw_2km), vt_p_mean[:,k], kind='linear')
						f_ur_p_mean_norm = scipy.interpolate.interp1d((r/rmw_2km), ur_p_mean[:,k], kind='linear')
	
						vt_p_mean_norm[:,k] = f_vt_p_mean_norm(rnorm)
						ur_p_mean_norm[:,k] = f_ur_p_mean_norm(rnorm)
				
				u2km = uwind[:,:,4]
				v2km = vwind[:,:,4]
				u5km = uwind[:,:,10]
				v5km = vwind[:,:,10]
				wind_2km = np.hypot(uwind[:,:,4],vwind[:,:,4])
				dbz_2km = dbz[:,:,4]
	
				#########################################################################################################
				###Block of code to calculate tangential wind tendency terms############################################
				#Calculate vorticity
	
				import metpy.calc as mpcalc
				from metpy.units import units
	
				vort = np.ones((np.shape(lat)[0],np.shape(lon)[0],np.shape(heightlevs)[0]))*np.nan
				with warnings.catch_warnings():
					warnings.filterwarnings(action='ignore', message='Mean of empty slice')
					xgrad = np.nanmean(np.gradient(x_sr))
					ygrad = np.nanmean(np.gradient(y_sr))
	
				for k in range(zsize):
					vort[:,:,k] = np.array(mpcalc.vorticity(uwind[:,:,k]* units.meter / units.second,vwind[:,:,k]* units.meter / units.second,xgrad*1e3* units.meter,ygrad*1e3* units.meter,dim_order='yx'))
	
				vort_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
	
				for k in range(zsize):
					f_vort = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), vort[:,:,k])
					vort_p[:,:,k] = f_vort((YI,XI),method='linear')
	
	
				#Calculate terms for Tangential Wind Budget
	
				#Initialize arrays
				vt_p_perturbation = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				ur_p_perturbation = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				w_p_perturbation = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				vort_p_perturbation = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
	
				f = 2*7.292e-5*np.sin(centerlat*3.14159/180)
				absvort_p = vort_p+f
	
				#Calculate azimuthal means and perturbations
				with warnings.catch_warnings():
					warnings.filterwarnings(action='ignore', message='Mean of empty slice')
					vort_p_mean = np.nanmean(vort_p,0)
					absvort_p_mean = np.nanmean(absvort_p,0)
	
				for k in range(zsize):
					vt_p_perturbation[:,:,k] = vt_p[:,:,k]-vt_p_mean[:,k]
					ur_p_perturbation[:,:,k] = ur_p[:,:,k]-ur_p_mean[:,k]
					w_p_perturbation[:,:,k] = w_p[:,:,k]-w_p_mean[:,k]
					vort_p_perturbation[:,:,k] = vort_p[:,:,k]-vort_p_mean[:,k]
	
				#Now Calculate Terms One by One
	
				#Term1 (Mean Radial Influx of Absolute Vorticity)
				term1_vt_tendency_mean_radial_flux = -ur_p_mean*absvort_p_mean
				term1_vt_tendency_mean_radial_flux[:,0] = np.nan
				term1_vt_tendency_mean_radial_flux[0,:] = np.nan
	
				#Term2 (Mean Vertical Advection of Mean Tangential Momentum)
				d_vt_p_mean_dz = np.array(metpy.calc.first_derivative(vt_p_mean,axis=1,delta=500))
				d_vt_p_mean_dz[:,0] = np.nan
				d_vt_p_mean_dz[0,:] = np.nan
				term2_vt_tendency_mean_vertical_advection = -w_p_mean*d_vt_p_mean_dz
				
				#Term3 (Eddy Flux)
				eddy_vort_flux_p = ur_p_perturbation*vort_p_perturbation
				with warnings.catch_warnings():
					warnings.filterwarnings(action='ignore', message='Mean of empty slice')
					eddy_vort_flux_p_mean = np.nanmean(eddy_vort_flux_p,0)
				term3_vt_tendency_eddy_flux = -eddy_vort_flux_p_mean
				term3_vt_tendency_eddy_flux[:,0] = np.nan
				term3_vt_tendency_eddy_flux[0,:] = np.nan
	
				#Term4 (Vertical Advection of Eddy Tangential Momentum)
				d_vt_p_perturbation_dz = metpy.calc.first_derivative(vt_p_perturbation,axis=2,delta=500)
				vertical_eddy_advection_p = w_p_perturbation*d_vt_p_perturbation_dz
				with warnings.catch_warnings():
					warnings.filterwarnings(action='ignore', message='Mean of empty slice')
					vertical_eddy_advection_p_mean = np.nanmean(vertical_eddy_advection_p,0)
				term4_vt_tendency_vertical_eddy_advection = -vertical_eddy_advection_p_mean
				term4_vt_tendency_vertical_eddy_advection[:,0] = np.nan
				term4_vt_tendency_vertical_eddy_advection[0,:] = np.nan
	
				terms_vt_tendency_sum = term1_vt_tendency_mean_radial_flux+term2_vt_tendency_mean_vertical_advection+term3_vt_tendency_eddy_flux+term4_vt_tendency_vertical_eddy_advection
	
				##################################################################################################################
	
	
				##################################################################################################################
				###Block of code to calculate vorticity budget terms
				#First Calculate Storm Motion
				centerlon_t0 = centerlon
				centerlat_t0 = centerlat
				if ( FHR > 0):
	
					# Get coordinate information from ATCF
					FHRIND2 = list(FHRIND)[0]-1
					FHR_tm1 = int(ATCF_DATA[FHRIND2,5])
					#print('MSG: h_tm1 = ',FHRIND2)
					#print('MSG: FHR_tm1 =',FHR_tm1)
					lonstr_tm1 = ATCF_DATA[FHRIND2,7]
					#print('MSG: lonstr_tm1 = ',lonstr_tm1)
					lonstr1_tm1 = lonstr_tm1[::-1]
					lonstr1_tm1 = lonstr1_tm1[1:]
					lonstr1_tm1 = lonstr1_tm1[::-1]
					lonstr2_tm1 = lonstr_tm1[::-1]
					lonstr2_tm1 = lonstr2_tm1[0]
					if (lonstr2_tm1 == 'W'):
						centerlon_tm1 = 360-float(lonstr1_tm1)/10
					else:
						centerlon_tm1 = float(lonstr1_tm1)/10
					latstr_tm1 = ATCF_DATA[FHRIND2,6]
					latstr1_tm1 = latstr_tm1[::-1]
					latstr1_tm1 = latstr1_tm1[1:]
					latstr1_tm1 = latstr1_tm1[::-1]
					latstr2_tm1 = latstr_tm1[::-1]
					latstr2_tm1 = latstr2_tm1[0]
					if (latstr2_tm1 == 'N'):
						centerlat_tm1 = float(latstr1_tm1)/10
					else:
						centerlat_tm1 = -1*float(latstr1_tm1)/10
				
					dt = (float(FHR)-float(FHR_tm1))*3600
					dx = (centerlon-centerlon_tm1)*111.1e3*np.cos(centerlat*3.14159/180)
					dy = (centerlat-centerlat_tm1)*111.1e3
				else:
					dt, dx, dy = np.nan, np.nan, np.nan
	
				umotion = dx/dt
				vmotion = dy/dt
				#print('MSG: fhr = ',FHR)
				#print('MSG: dt = ',dt)
				print(f'MSG: umotion,vmotion = {umotion:.2f},{vmotion:.2f}')
	
				uwind_sr = uwind-umotion
				vwind_sr = vwind-vmotion
	
				#Calculate Terms
				absvort = vort+f
	
				#Term1 (Horizontal Advection)
				d_eta_dx = np.array(metpy.calc.first_derivative(absvort,axis=1,delta=xgrad*1e3))
				d_eta_dy = np.array(metpy.calc.first_derivative(absvort,axis=0,delta=ygrad*1e3))
				horizontal_advection = -(uwind_sr*d_eta_dx + vwind_sr*d_eta_dy)
	
				#Term2 (Vertical advection)
				d_vort_dz = np.array(metpy.calc.first_derivative(vort,axis=2,delta=500))
				vertical_advection = -wwind*d_vort_dz
	
				#Term3 (Stretching/Convergence Term)
				d_u_sr_dx = np.array(metpy.calc.first_derivative(uwind_sr,axis=1,delta=xgrad*1e3))
				d_v_sr_dy = np.array(metpy.calc.first_derivative(vwind_sr,axis=0,delta=ygrad*1e3))
				stretching_convergence = -(absvort*d_u_sr_dx + absvort*d_v_sr_dy)
	
				#Term4 (Tilting of Horizontal Vorticity into the Vertical)
				d_w_dx = np.array(metpy.calc.first_derivative(wwind,axis=1,delta=xgrad*1e3))
				d_w_dy = np.array(metpy.calc.first_derivative(wwind,axis=0,delta=ygrad*1e3))
				d_u_sr_dz = np.array(metpy.calc.first_derivative(uwind_sr,axis=2,delta=500))
				d_v_sr_dz = np.array(metpy.calc.first_derivative(vwind_sr,axis=2,delta=500))
				tilting = -(d_w_dx*d_v_sr_dz - d_w_dy*d_u_sr_dz)
	
				horizontal_advection_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				vertical_advection_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				stretching_convergence_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
				tilting_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
	
				for k in range(zsize):
					f_horizontal_advection = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), horizontal_advection[:,:,k])
					f_vertical_advection = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), vertical_advection[:,:,k])
					f_stretching_convergence = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), stretching_convergence[:,:,k])
					f_tilting = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), tilting[:,:,k])
	
					horizontal_advection_p[:,:,k] = f_horizontal_advection((YI,XI),method='linear')
					vertical_advection_p[:,:,k] = f_vertical_advection((YI,XI),method='linear')
					stretching_convergence_p[:,:,k] = f_stretching_convergence((YI,XI),method='linear')
					tilting_p[:,:,k] = f_tilting((YI,XI),method='linear')
	
				with warnings.catch_warnings():
					warnings.filterwarnings(action='ignore', message='Mean of empty slice')
					term1_vort_tendency_horizontal_advection = np.nanmean(horizontal_advection_p,0)
					term2_vort_tendency_vertical_advection = np.nanmean(vertical_advection_p,0)
					term3_vort_tendency_stretching_convergence = np.nanmean(stretching_convergence_p,0)
					term4_vort_tendency_tilting = np.nanmean(tilting_p,0)
	
				term1_vort_tendency_horizontal_advection[:,0] = np.nan
				term1_vort_tendency_horizontal_advection[0,:] = np.nan
				term2_vort_tendency_vertical_advection[:,0] = np.nan
				term2_vort_tendency_vertical_advection[0,:] = np.nan
				term3_vort_tendency_stretching_convergence[:,0] = np.nan
				term3_vort_tendency_stretching_convergence[0,:] = np.nan
				term4_vort_tendency_tilting[:,0] = np.nan
				term4_vort_tendency_tilting[0,:] = np.nan
	
				terms_vort_tendency_sum = term1_vort_tendency_horizontal_advection+term2_vort_tendency_vertical_advection+term3_vort_tendency_stretching_convergence+term4_vort_tendency_tilting
	
				##################################################################################################################
						
	
				###################################################################################################################
				#Code to calculate precipitation partitioning (from Michael Fischer)
				hlevs = heightlevs/1000
				sref = np.ones((1,np.shape(dbz)[0],np.shape(dbz)[1],np.shape(dbz)[2]))*np.nan
				sref[0,:,:,:] = dbz
	
				### Define useful functions ###
	
				# Function to find index of element closest to specified value:
				def find_nearest(array, value):
					array = np.asarray(array)
					#idx = (np.abs(array - value)).argmin()
					X = np.abs(array - value)
					#idx = np.where( X == X.min() )
					idx = np.where( X == np.nanmin(X) )
					#xi = np.where( X == X.min() )[1]
					#yi = np.where( X == X.min() )[0]
					return array[idx]
	
				# Establish constants:
				dxgrid = xgrad
				dygrid = ygrad
				vlim = 200. # TCs must be equal to or less than this intensity (kt)
	
				# Precip classification constants:
				lev_ref = 2. #This is the height (km) to classify the precipitation
				lev_ref_bb = 4.5 #This is the height (km) of the brightband level (subjectively identified)
				rnear_dist = 11. #This is the adjacent radius (km) to use for background mean reflectivity
				rnear = round(rnear_dist/np.min([dxgrid,dygrid])) # This is the number of indices to search for the background mean reflectivity
				zti = 50 #This is the reflectivity threshold (dBZ) to determine convective pixels
				zwe = 20 #This is the reflectivity threshold (dBZ) to determine weak echo pixels
				za = 9 #tuning parameter for precipitation classification
				zb = 55 #tuning parameter for precipitation classification
				echo_tt = 30. # Echo threshold (dBZ) for convective classification
				mod_height = 6. # Echo top height (km) threshold for moderate convection
				deep_height = 10. # Echo top height (km) threshold for moderate convection
	
				### Determine precipitation type ###
	
				# sref is a numpy array of the following dimensional order: storm index, latitude, longitude, level,
	
				ptype = np.zeros((sref.shape[0],sref.shape[1],sref.shape[2])) #Precipitation classification
	
				#"""
				#The ptypes are defined as follows:
				#	if ptype == 1, it is "weak echo"
				#	if ptype == 2, it is stratiform
				#	if ptype == 3, it is shallow convection
				#	if ptype == 4, it is moderate convection
				#	if ptype == 5, it is deep convection
				#"""
	
				levi_ref = np.where(hlevs == lev_ref)[0][0] # Reference height (here, 2 km)
				levi_ref_bb = np.where(hlevs == lev_ref_bb)[0][0] # Bright-band reference height (here, 4.5 km)
	
				# Loop over all swaths:
				for pi in range(1): # Loop over storm index
					print('MSG: The current pass index is:',pi)
					for yi in range(ptype.shape[1]): # Loop over latitudinal index
						#print 'The current y-index is:',yi
						for xi in range(ptype.shape[2]): # Loop over longitudinal index
							#print ptype[pi,yi,xi]
							# Ensure good reflectivity data exists for current grid point:
							if np.isfinite(sref[pi,yi,xi,levi_ref]):
								# Make sure points hasn't already been filled in due to convective radius:
								if ptype[pi,yi,xi] == 0.:
									# Check to see if reflectivity at grid point exceeds convective threshold:
									if sref[pi,yi,xi,levi_ref] >= zti:
										ptype[pi,yi,xi] = 3.
										# Check to see if reflecitivty at grid point is less than weak echo threshold:
									elif sref[pi,yi,xi,levi_ref] < zwe:
										ptype[pi,yi,xi] = 1.
									# If reflectivity is between extrema, use peakedness:
									elif np.logical_and(sref[pi,yi,xi,levi_ref] >= zwe, sref[pi,yi,xi,levi_ref] < zti):
										# Determine background reflectivity (zbg)
										ymin = int(np.nanmax([0,yi - rnear]))
										ymax = int(np.nanmin([int(ptype.shape[1]),yi + rnear]))
										xmin = int(np.nanmax([0,xi - rnear]))
										xmax = int(np.nanmin([int(ptype.shape[2]),xi + rnear]))
										zbg = np.nanmean(sref[pi,ymin:ymax+1,xmin:xmax+1,levi_ref]) #background reflectivity (dBZ)
										zbg2 = np.nanmean(sref[pi,ymin:ymax+1,xmin:xmax+1,levi_ref_bb]) #background reflectivity (dBZ) at bright-band level
	
										# Calculate the "convective center criterion" from Steiner et al. (1995):
										zcc = za*np.cos((1./zb)*((np.pi*zbg)/2.))
	
										# If the current reflectivity exceedes the zbg by an amount > zcc, it is convective:
										ref_dif = sref[pi,yi,xi,levi_ref] - zbg
										if np.logical_and(ref_dif > zcc,zbg > zbg2):
											ptype[pi,yi,xi] = 3.
	
											# Now make necessary adjacent points convective as well:
											if zbg < 20:
												conv_rad = 0.5
											elif np.logical_and(zbg >= 20.,zbg < 35.):
												conv_rad = 0.5 + 3.5*((zbg - 20.)/15.)
											elif zbg >= 35.:
												conv_rad = 4.
	
											# Identify points falling within convective radius:
											curr_x_dist = np.arange(dxgrid*(0. - xi),dxgrid*(int(ptype.shape[2]) - xi),dxgrid)
											curr_y_dist = np.arange(dygrid*(0. - yi),dygrid*(int(ptype.shape[1]) - yi),dygrid)
											CXD,CYD = np.meshgrid(curr_x_dist,curr_y_dist)
											curr_dist = np.sqrt(CXD**2 + CYD**2) #total distance (km) from current pixel
	
											conv_rx = np.where(curr_dist <= conv_rad)[1] #zonal indices of grid points inside convective radius
											conv_ry = np.where(curr_dist <= conv_rad)[0] #merid indices of grid points inside convective radius
	
											#print np.shape(curr_y_dist),np.shape(curr_x_dist),np.shape(sref[pi,:,:,levi_ref])
											ptype[pi,conv_ry,conv_rx] = 3. #fill in all grid points within convective radius as convective
	
									# Otherwise the pixel is classified to be stratiform:
									if np.logical_and(np.isfinite(sref[pi,yi,xi,levi_ref]),ptype[pi,yi,xi] == 0.):
										ptype[pi,yi,xi] = 2.
	
					### Reclassify type of convection ###
					#Change From Michael Fischer
					#Test Comment
					for yi in range(ptype.shape[1]): # Loop over latitudinal index
						#print 'The current y-index is:',yi
	
						for xi in range(ptype.shape[2]): # Loop over longitudinal index
							#print ptype[pi,yi,xi]
	
							# Reclassify convective grid points as either shallow, moderate, or deep
							if np.logical_and(ptype[pi,yi,xi] == 3., np.isfinite(sref[pi,yi,xi,levi_ref])):
	
								# Find maximum height of vertical velocity convective threshold:
								if np.nanmax(sref[pi,yi,xi,:]) >= echo_tt:
									with warnings.catch_warnings():
										warnings.filterwarnings(action='ignore', message='invalid value encountered in greater_equal')
										max_height = hlevs[np.where(sref[pi,yi,xi,:] >= echo_tt)[0][-1]]
	
									# Classify convection:
									if np.logical_and(max_height >= mod_height, max_height < deep_height):
										ptype[pi,yi,xi] = 4.
									elif max_height >= deep_height:
										ptype[pi,yi,xi] = 5.	
				
				#Do Interpolation of P type to polar coordinates
				ptype = np.squeeze(ptype)
				ptype_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
				f_ptype = interpolate.RegularGridInterpolator((y_sr, x_sr), ptype[:,:])
				ptype_p = f_ptype((YI,XI),method='linear')
				ptype_p = np.round(ptype_p)
	
				f_ptype_norm = interpolate.RegularGridInterpolator((y_sr/rmw_2km, x_sr/rmw_2km), ptype[:,:])
				ptype_p_norm = f_ptype_norm((YInorm,XInorm),method='linear')
				ptype_p_norm = np.round(ptype_p_norm)
	
				#End of Block to Calculate Precipitation Partitioning
				###########################################################################################################################################
				
				############################################################################################################################################
				#Calculate some important structure metrics and write them and others to a text file
				vmax = float(maxwind)
				vt_max = np.nanmax(vt_p_mean)*1.94
	
				rossby = temp_anomaly_max = height_temp_anomaly_max = slope_rmw_1 = slope_rmw_2 = alpha = vortex_depth_vt_dynamic = vortex_depth_vt_static = tiltmag_mid_pressure = tiltdir_mid_pressure = tiltmag_mid_vort = tiltdir_mid_vort = tiltmag_deep_pressure = tiltdir_deep_pressure = tiltmag_deep_vort = tiltdir_deep_vort = weakpercent_inner = stratiformpercent_inner = shallowpercent_inner = moderatepercent_inner = deeppercent_inner = weakpercent_outer = stratiformpercent_outer = shallowpercent_outer = moderatepercent_outer = deeppercent_outer = closure_stratiform = closure_shallow = closure_moderate = closure_deep = symmetry_w1_dbz5_p = symmetry_all_dbz5_p = symmetry_w1_vt10_p = symmetry_all_vt10_p = shearmag_2km_5km_local = sheardir_2km_5km_local = shearmag_2km_8km_local = sheardir_2km_8km_local = shearmag_2km_10km_local = sheardir_2km_10km_local = np.nan
				if ( rmw_2km < 200 and vmax > 25 and vt_max > 30):
					#Calculate Vortex Depth based on Vt
					vt_rmw_ratio = vt_rmw_mean/vt_rmw_mean[4]
					threshold_ratio_vt_dynamic = 0.4
					if ( np.nanmin(vt_rmw_ratio) < 0.4):
						vortex_depth_vt_dynamic = np.nanmax(heightlevs[vt_rmw_ratio > threshold_ratio_vt_dynamic])/1000
						index_vortex_depth_vt_dynamic = np.argmin(abs(heightlevs/1000-vortex_depth_vt_dynamic))
					else:
						vortex_depth_vt_dynamic = np.nan
					
					if ( np.nanmin(vt_rmw_mean) < 24 and np.nanmax(vt_rmw_mean) >=24):
						vortex_depth_vt_static = np.nanmax(heightlevs[vt_rmw_mean > 24.0])/1000
						index_vortex_depth_vt_static = np.argmin(abs(heightlevs/1000-vortex_depth_vt_static))		
					else:
						vortex_depth_vt_static = np.nan

					if ( np.nanmin(vt_rmw_mean) < 8 and nanmax(vt_rmw_mean) >=8):
						vortex_depth_vt_temp = np.nanmax(heightlevs[vt_rmw_mean > 8.0])/1000
						index_vortex_depth_vt_temp = np.argmin(abs(heightlevs/1000-vortex_depth_vt_static))
						ivd = index_vortex_depth_vt_temp+1
					else:
						ivd=11

					print('IVD = ', ivd)

					#Calculate vortex depth based on vort
					vort_ratio = np.nanmax(vort_p_mean,0)/np.nanmax(vort_p_mean[:,4])
					threshold_ratio_vort = 0.5
					if (np.min(vort_ratio) < 0.5):
						vortex_depth_vort = np.min(heightlevs[np.argwhere(vort_ratio[5::] < threshold_ratio_vort)+5])/1000
						index_vortex_depth_vort = np.argmin(abs(heightlevs/1000-vortex_depth_vort))
					else:
						vortex_depth_vort = np.nan
	
					#Calculate the new centers at each height using the centroid function
					pressure_centroid = pressure[:,:,0:ivd]
					vort_centroid = vort[:,:,0:ivd]
					center_indices_pressure = np.zeros((np.shape(pressure_centroid)[2],2),order='F').astype(np.int32)
					center_indices_vort = np.zeros((np.shape(vort_centroid)[2],2),order='F').astype(np.int32)
					threshold_pressure = np.zeros((np.shape(pressure_centroid)[2]))
					threshold_vort = np.zeros((np.shape(vort_centroid)[2]))
					
	
					for k in range(ivd):
						x1 = np.argmin(abs(-rmw_mean[k]-x_sr))
						x2 = np.argmin(abs(rmw_mean[k]-x_sr))
						y1 = np.argmin(abs(-rmw_mean[k]-y_sr))
						y2 = np.argmin(abs(rmw_mean[k]-y_sr))
						r2 = np.argmin(abs(r-rmw_mean[k]))
	
						pressure_centroid[0:y1,:,k] = 9999999999
						pressure_centroid[y2::,:,k] = 9999999999
						pressure_centroid[:,0:x1,k] = 9999999999
						pressure_centroid[:,x2::,k] = 9999999999
	
						vort_centroid[0:y1,:,k] = -9999999999
						vort_centroid[y2::,:,k] = -9999999999
						vort_centroid[:,0:x1,k] = -9999999999
						vort_centroid[:,x2::,k] = -99999999999
	
						threshold_pressure[k] = np.nanmin(pressure_p_mean[0:r2+1,k])+0.2*(np.nanmax(pressure_p_mean[0:r2+1,k])-np.nanmin(pressure_p_mean[0:r2+1,k]))
						threshold_vort[k] = 0.80*np.nanmax(vort_p_mean[0:r2+1,k])
				
					#print('MSG: HERE ARE THE THRESHOLDS')
					#for k in range(ivd):
					#	print(threshold_pressure[k],threshold_vort[k])
		
					if ( np.min(threshold_vort) > 0):	
						centroid.centroid(pressure_centroid,center_indices_pressure,threshold_pressure,-1,np.shape(pressure_centroid)[0],np.shape(pressure_centroid)[1],np.shape(pressure_centroid)[2])
						centroid.centroid(vort_centroid,center_indices_vort,threshold_vort,1,np.shape(vort_centroid)[0],np.shape(vort_centroid)[1],np.shape(vort_centroid)[2])
						
						#print('HERE ARE THE INDICES')
						#for k in range(ivd):
						#	print(center_indices_vort[k,:])
			
						center_x_vort = np.ones(zsize)*np.nan
						center_y_vort = np.ones(zsize)*np.nan
						center_x_pressure = np.ones(zsize)*np.nan
						center_y_pressure = np.ones(zsize)*np.nan
						center_x_vort[0:ivd] = x_sr[center_indices_vort[:,1]]
						center_y_vort[0:ivd] = y_sr[center_indices_vort[:,0]]
						center_x_pressure[0:ivd] = x_sr[center_indices_pressure[:,1]]
						center_y_pressure[0:ivd] = y_sr[center_indices_pressure[:,0]]
	
	
						#Calculate 2-10 and 2-5 km tilt
						if ( vortex_depth_vort >= 10. ):
							tiltmag_deep_pressure = np.hypot(center_x_pressure[20]-center_x_pressure[4],center_y_pressure[20]-center_y_pressure[4])
							tiltdir_deep_pressure = np.arctan2(center_y_pressure[20]-center_y_pressure[4],center_x_pressure[20]-center_x_pressure[4])*180/np.pi
							if tiltdir_deep_pressure <=90:
								tiltdir_deep_pressure = 90-tiltdir_deep_pressure
							else:
								tiltdir_deep_pressure = 360-(tiltdir_deep_pressure-90)
					
							tiltmag_deep_vort = np.hypot(center_x_vort[20]-center_x_vort[4],center_y_vort[20]-center_y_vort[4])
							tiltdir_deep_vort = np.arctan2(center_y_vort[20]-center_y_vort[4],center_x_vort[20]-center_x_vort[4])*180/np.pi
							if tiltdir_deep_vort <=90:
								tiltdir_deep_vort = 90-tiltdir_deep_vort
							else:
								tiltdir_deep_vort = 360-(tiltdir_deep_vort-90)
	
							tiltmag_mid_pressure = np.hypot(center_x_pressure[10]-center_x_pressure[4],center_y_pressure[10]-center_y_pressure[4])
							tiltdir_mid_pressure = np.arctan2(center_y_pressure[10]-center_y_pressure[4],center_x_pressure[10]-center_x_pressure[4])*180/np.pi
							if tiltdir_mid_pressure <=90:
								tiltdir_mid_pressure = 90-tiltdir_mid_pressure
							else:   
								tiltdir_mid_pressure = 360-(tiltdir_mid_pressure-90)
							
							tiltmag_mid_vort = np.hypot(center_x_vort[10]-center_x_vort[4],center_y_vort[10]-center_y_vort[4])
							tiltdir_mid_vort = np.arctan2(center_y_vort[10]-center_y_vort[4],center_x_vort[10]-center_x_vort[4])*180/np.pi
							if tiltdir_mid_vort <=90:
								tiltdir_mid_vort = 90-tiltdir_mid_vort
							else:
								tiltdir_mid_vort = 360-(tiltdir_mid_vort-90)		
						elif ( vortex_depth_vort >= 5. and vortex_depth_vort <= 10. ):
							tiltmag_deep_pressure = np.nan
							tiltdir_deep_pressure = np.nan
							tiltmag_deep_vort = np.nan
							tiltdir_deep_vort = np.nan
	
							tiltmag_mid_pressure = np.hypot(center_x_pressure[10]-center_x_pressure[4],center_y_pressure[10]-center_y_pressure[4])
							tiltdir_mid_pressure = np.arctan2(center_y_pressure[10]-center_y_pressure[4],center_x_pressure[10]-center_x_pressure[4])*180/np.pi
							if tiltdir_mid_pressure <=90:
								tiltdir_mid_pressure = 90-tiltdir_mid_pressure
							else:
								tiltdir_mid_pressure = 360-(tiltdir_mid_pressure-90)
	
							tiltmag_mid_vort = np.hypot(center_x_vort[10]-center_x_vort[4],center_y_vort[10]-center_y_vort[4])
							tiltdir_mid_vort = np.arctan2(center_y_vort[10]-center_y_vort[4],center_x_vort[10]-center_x_vort[4])*180/np.pi
							if tiltdir_mid_vort <=90:
								tiltdir_mid_vort = 90-tiltdir_mid_vort
							else:   
								tiltdir_mid_vort = 360-(tiltdir_mid_vort-90)
						else:
							tiltmag_deep_pressure = np.nan
							tiltdir_deep_pressure = np.nan
							tiltmag_deep_vort = np.nan
							tiltdir_deep_vort = np.nan
							
							tiltmag_mid_pressure = np.nan
							tiltdir_mid_pressure = np.nan
							tiltmag_mid_vort = np.nan
							tiltdir_mid_vort = np.nan		
								
					#First, percentage of area in the inner and outer core with each precip type
					ptype_p_norm_inner = ptype_p_norm[:,15:26]
					ptype_p_norm_outer = ptype_p_norm[:,26:41]
				
					innersize = np.shape(ptype_p_norm_inner)[0]*np.shape(ptype_p_norm_inner)[1]
					if innersize > 0:
						weakpercent_inner = np.shape(ptype_p_norm_inner[ptype_p_norm_inner == 1.])[0]/innersize
						stratiformpercent_inner = np.shape(ptype_p_norm_inner[ptype_p_norm_inner == 2.])[0]/innersize
						shallowpercent_inner = np.shape(ptype_p_norm_inner[ptype_p_norm_inner == 3.])[0]/innersize
						moderatepercent_inner = np.shape(ptype_p_norm_inner[ptype_p_norm_inner == 4.])[0]/innersize
						deeppercent_inner = np.shape(ptype_p_norm_inner[ptype_p_norm_inner == 5.])[0]/innersize
					else:
						weakpercent_inner = np.nan
						stratiformpercent_inner = np.nan
						shallowpercent_inner = np.nan
						moderatepercent_inner = np.nan
						deeppercent_inner = np.nan
			
					outersize = np.shape(ptype_p_norm_outer)[0]*np.shape(ptype_p_norm_outer)[1]
					if outersize > 0:	
						weakpercent_outer = np.shape(ptype_p_norm_outer[ptype_p_norm_outer == 1.])[0]/outersize
						stratiformpercent_outer = np.shape(ptype_p_norm_outer[ptype_p_norm_outer == 2.])[0]/outersize
						shallowpercent_outer = np.shape(ptype_p_norm_outer[ptype_p_norm_outer == 3.])[0]/outersize
						moderatepercent_outer = np.shape(ptype_p_norm_outer[ptype_p_norm_outer == 4.])[0]/outersize
						deeppercent_outer = np.shape(ptype_p_norm_outer[ptype_p_norm_outer == 5.])[0]/outersize
					else:
						weakpercent_outer = np.nan
						stratiformpercent_outer = np.nan
						shallowpercent_outer = np.nan
						moderatepercent_outer = np.nan
						deeppercent_outer = np.nan
	
					#Next, calculate closure of the eyewall for stratiform, shallow, moderate, and deep convection
					ptype_p_norm_inner_max = np.max(ptype_p_norm_inner,1)
					closure_stratiform = np.shape(ptype_p_norm_inner_max[ptype_p_norm_inner_max >= 2.])[0]/np.shape(ptype_p_norm_inner_max)[0]
					closure_shallow = np.shape(ptype_p_norm_inner_max[ptype_p_norm_inner_max >= 3.])[0]/np.shape(ptype_p_norm_inner_max)[0]
					closure_moderate = np.shape(ptype_p_norm_inner_max[ptype_p_norm_inner_max >= 4.])[0]/np.shape(ptype_p_norm_inner_max)[0]
					closure_deep = np.shape(ptype_p_norm_inner_max[ptype_p_norm_inner_max >= 5.])[0]/np.shape(ptype_p_norm_inner_max)[0]
	
					#RMW Slope
					slope_rmw_1 = np.linalg.lstsq((heightlevs[4:21]/1000-heightlevs[4]/1000).reshape(-1,1), (rmw_mean[4:21]-rmw_mean[4]), rcond=None)[0][0]
					slope_rmw_2 = (rmw_mean[20]-rmw_mean[4])/8
				 
					#Alpha Parameter
					if (rmw_2km < 100. and (3*rmw_2km) < rmax):
						alpha = np.log(vt_p_mean_norm[20,4]/vt_p_mean_norm[60,4])/np.log(3)
					else:
						alpha = np.nan
	
					#Rossby Number
					rmw_mean_10m = rmw_mean[0]
					rmw_mean_index_10m = int(rmw_mean_index[0])
					vt10_p_mean = np.nanmean(vt10_p,0)
					coriolis = 2*7.292e-5*np.sin(centerlat*3.14159/180)
					rossby = vt10_p_mean[rmw_mean_index_10m]/(rmw_mean_10m*1000*coriolis)
					
					#Calculate Magnitude and Height of Warm Core Anomaly
					r15km_index = np.argmin(np.abs(r-15))
					r200km_index = np.argmin(np.abs(r-200))
					r300km_index = np.argmin(np.abs(r-300))
					temp_p_mean_core = temp_p_mean[0:r15km_index+1,:]
					temp_p_mean_outer = temp_p_mean[r200km_index:r300km_index+1,:]
					temp_p_mean_core_mean = np.nanmean(temp_p_mean_core,0)
					temp_p_mean_outer_mean = np.nanmean(temp_p_mean_outer,0)
					temp_anomaly = temp_p_mean_core_mean-temp_p_mean_outer_mean
					temp_anomaly_max = np.max(temp_anomaly[1::])
					height_temp_anomaly_max = heightlevs[np.argmax(temp_anomaly[1::])+1]/1000	
	
					#Calculate symmetry of precipitation 
					dbz5_p_w0_ring = dbz5_p_w0[:,np.argmin(np.abs(r-0.75*rmw_mean[4])):np.argmin(np.abs(r-1.25*rmw_mean[4]))+1]
					dbz5_p_w1_ring = dbz5_p_w1[:,np.argmin(np.abs(r-0.75*rmw_mean[4])):np.argmin(np.abs(r-1.25*rmw_mean[4]))+1]
					dbz5_p_whigher_ring = dbz5_p_whigher[:,np.argmin(np.abs(r-0.75*rmw_mean[4])):np.argmin(np.abs(r-1.25*rmw_mean[4]))+1]
					amp_dbz5_p_w0_ring = np.nanmean(np.nanmax(dbz5_p_w0_ring,0)) 
					amp_dbz5_p_w1_ring = np.nanmean(np.nanmax(dbz5_p_w1_ring,0)-np.mean(dbz5_p_w1_ring,0))
					amp_dbz5_p_whigher_ring = np.nanmean(np.nanmax(dbz5_p_whigher_ring,0)-np.mean(dbz5_p_whigher_ring,0))
					symmetry_w1_dbz5_p = amp_dbz5_p_w0_ring/(amp_dbz5_p_w0_ring+amp_dbz5_p_w1_ring)
					symmetry_all_dbz5_p = amp_dbz5_p_w0_ring/(amp_dbz5_p_w0_ring+amp_dbz5_p_w1_ring+amp_dbz5_p_whigher_ring)
					if symmetry_w1_dbz5_p < 0: symmetry_w1_dbz5_p = 0
					if symmetry_all_dbz5_p < 0: symmetry_all_dbz5_p = 0
	
					vt10_p_w0_ring = vt10_p_w0[:,np.argmin(np.abs(r-0.75*rmw_mean[0])):np.argmin(np.abs(r-1.25*rmw_mean[0]))+1]
					vt10_p_w1_ring = vt10_p_w1[:,np.argmin(np.abs(r-0.75*rmw_mean[0])):np.argmin(np.abs(r-1.25*rmw_mean[0]))+1]
					vt10_p_whigher_ring = vt10_p_whigher[:,np.argmin(np.abs(r-0.75*rmw_mean[0])):np.argmin(np.abs(r-1.25*rmw_mean[0]))+1]
					amp_vt10_p_w0_ring = np.nanmean(np.nanmax(vt10_p_w0_ring,0))
					amp_vt10_p_w1_ring = np.nanmean(np.nanmax(vt10_p_w1_ring,0)-np.mean(vt10_p_w1_ring,0))
					amp_vt10_p_whigher_ring = np.nanmean(np.nanmax(vt10_p_whigher_ring,0)-np.mean(vt10_p_whigher_ring,0))
					symmetry_w1_vt10_p = amp_vt10_p_w0_ring/(amp_vt10_p_w0_ring+amp_vt10_p_w1_ring)
					symmetry_all_vt10_p = amp_vt10_p_w0_ring/(amp_vt10_p_w0_ring+amp_vt10_p_w1_ring+amp_vt10_p_whigher_ring)
					if symmetry_w1_vt10_p < 0: symmetry_w1_vt10_p = 0
					if symmetry_all_vt10_p < 0: symmetry_all_vt10_p = 0
					
					#Calculate Local Shear
					if ( np.min(threshold_vort) > 0):
						rmaxlocal = 102
						rlocal = np.linspace(0,rmaxlocal,(int(rmaxlocal//resolution)+1))
						Rlocal, THETAlocal = np.meshgrid(rlocal, theta)
						XIlocal = Rlocal * np.cos(THETAlocal)
						YIlocal = Rlocal * np.sin(THETAlocal)
						u2km = uwind[:,:,4]
						v2km = vwind[:,:,4]
						u5km = uwind[:,:,10]
						v5km = vwind[:,:,10]
						u8km = uwind[:,:,16]
						v8km = vwind[:,:,16]
						u10km = uwind[:,:,4]
						v10km = vwind[:,:,20]
	
						u2km_p_local = np.ones((np.shape(XIlocal)[0],np.shape(XIlocal)[1]))*np.nan
						v2km_p_local = np.ones((np.shape(XIlocal)[0],np.shape(XIlocal)[1]))*np.nan
						u5km_p_local = np.ones((np.shape(XIlocal)[0],np.shape(XIlocal)[1]))*np.nan
						v5km_p_local = np.ones((np.shape(XIlocal)[0],np.shape(XIlocal)[1]))*np.nan
						u8km_p_local = np.ones((np.shape(XIlocal)[0],np.shape(XIlocal)[1]))*np.nan
						v8km_p_local = np.ones((np.shape(XIlocal)[0],np.shape(XIlocal)[1]))*np.nan
						u10km_p_local = np.ones((np.shape(XIlocal)[0],np.shape(XIlocal)[1]))*np.nan
						v10km_p_local = np.ones((np.shape(XIlocal)[0],np.shape(XIlocal)[1]))*np.nan
	
						if (center_x_vort[4] < 200 and center_y_vort[4] < 200):
							f_u2km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[4], x_sr+center_x_vort[4]), u2km[:,:])
							u2km_p_local[:,:] = f_u2km((YIlocal,XIlocal),method='linear')
							f_v2km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[4], x_sr+center_x_vort[4]), v2km[:,:])
							v2km_p_local[:,:] = f_v2km((YIlocal,XIlocal),method='linear')
	
						if (vortex_depth_vort >= 5 and center_x_vort[10] < 200 and center_y_vort[10] < 200):
							f_u5km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[10], x_sr+center_x_vort[10]), u5km[:,:])
							u5km_p_local[:,:] = f_u5km((YIlocal,XIlocal),method='linear')
							f_v5km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[10], x_sr+center_x_vort[10]), v5km[:,:])
							v5km_p_local[:,:] = f_v5km((YIlocal,XIlocal),method='linear')
	
						if (vortex_depth_vort >= 8 and center_x_vort[16] < 200 and center_y_vort[16] < 200):
							f_u8km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[16], x_sr+center_x_vort[16]), u8km[:,:])
							u8km_p_local[:,:] = f_u8km((YIlocal,XIlocal),method='linear')
							f_v8km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[16], x_sr+center_x_vort[16]), v8km[:,:])
							v8km_p_local[:,:] = f_v8km((YIlocal,XIlocal),method='linear')
	
						if (vortex_depth_vort >= 10 and center_x_vort[20] < 200 and center_y_vort[20] < 200):
							f_u10km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[20], x_sr+center_x_vort[20]), u10km[:,:])
							u10km_p_local[:,:] = f_u10km((YIlocal,XIlocal),method='linear')
							f_v10km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[20], x_sr+center_x_vort[20]), v10km[:,:])
							v10km_p_local[:,:] = f_v10km((YIlocal,XIlocal),method='linear')
	
						rlocal50 = np.argmin(np.abs(rlocal-50))

						with warnings.catch_warnings():
							warnings.filterwarnings(action='ignore', message='Mean of empty slice')
							u2km_p_local_ring50km_mean = np.nanmean(u2km_p_local[:,rlocal50+1])
							v2km_p_local_ring50km_mean = np.nanmean(v2km_p_local[:,rlocal50+1])
							u5km_p_local_ring50km_mean = np.nanmean(u5km_p_local[:,rlocal50+1])
							v5km_p_local_ring50km_mean = np.nanmean(v5km_p_local[:,rlocal50+1])
							u8km_p_local_ring50km_mean = np.nanmean(u8km_p_local[:,rlocal50+1])
							v8km_p_local_ring50km_mean = np.nanmean(v8km_p_local[:,rlocal50+1])
							u10km_p_local_ring50km_mean = np.nanmean(u10km_p_local[:,rlocal50+1])
							v10km_p_local_ring50km_mean = np.nanmean(v10km_p_local[:,rlocal50+1])

						ushear_2km_5km_local_ring50km = u5km_p_local_ring50km_mean-u2km_p_local_ring50km_mean
						vshear_2km_5km_local_ring50km = v5km_p_local_ring50km_mean-v2km_p_local_ring50km_mean
						ushear_2km_8km_local_ring50km = u8km_p_local_ring50km_mean-u2km_p_local_ring50km_mean
						vshear_2km_8km_local_ring50km = v8km_p_local_ring50km_mean-v2km_p_local_ring50km_mean
						ushear_2km_10km_local_ring50km = u10km_p_local_ring50km_mean-u2km_p_local_ring50km_mean
						vshear_2km_10km_local_ring50km = v10km_p_local_ring50km_mean-v2km_p_local_ring50km_mean
	
						shearmag_2km_5km_local = np.hypot(ushear_2km_5km_local_ring50km,vshear_2km_5km_local_ring50km)
						sheardir_2km_5km_local = np.arctan2(vshear_2km_5km_local_ring50km,ushear_2km_5km_local_ring50km)*180.0/np.pi
						if sheardir_2km_5km_local <=90:
							sheardir_2km_5km_local = 90-sheardir_2km_5km_local
						else:
							sheardir_2km_5km_local = 360-(sheardir_2km_5km_local-90)
	
						shearmag_2km_8km_local = np.hypot(ushear_2km_8km_local_ring50km,vshear_2km_8km_local_ring50km)
						sheardir_2km_8km_local = np.arctan2(vshear_2km_8km_local_ring50km,ushear_2km_8km_local_ring50km)*180.0/np.pi
						if sheardir_2km_8km_local <=90:
							sheardir_2km_8km_local = 90-sheardir_2km_8km_local
						else:
							sheardir_2km_8km_local = 360-(sheardir_2km_8km_local-90)
	
						shearmag_2km_10km_local = np.hypot(ushear_2km_10km_local_ring50km,vshear_2km_10km_local_ring50km)
						sheardir_2km_10km_local = np.arctan2(vshear_2km_10km_local_ring50km,ushear_2km_10km_local_ring50km)*180.0/np.pi
						if sheardir_2km_10km_local <=90:
							sheardir_2km_10km_local = 90-sheardir_2km_10km_local
						else:
							sheardir_2km_10km_local = 360-(sheardir_2km_10km_local-90)
	
			
				structurefile = ODIR+'/'+LONGSID.lower()+'.structure_statistics.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.txt'
				f = open(structurefile,'w')
				f.write("%4s, %4.0f, %5.1f, %5.1f, %4.1f, %4.1f, %5.2f, %5.2f, %4.2f, %4.1f, %4.1f, %5.1f, %4.0f, %5.1f, %4.0f, %5.1f, %4.0f, %5.1f, %4.0f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %4.1f, %4.0f, %4.1f, %4.0f, %4.1f, %4.0f" % (FHR,vmax,rmw_2km,rossby,temp_anomaly_max,height_temp_anomaly_max,slope_rmw_1,slope_rmw_2,alpha,vortex_depth_vt_dynamic,vortex_depth_vt_static,tiltmag_mid_pressure,tiltdir_mid_pressure,tiltmag_mid_vort,tiltdir_mid_vort,tiltmag_deep_pressure,tiltdir_deep_pressure,tiltmag_deep_vort,tiltdir_deep_vort,weakpercent_inner,stratiformpercent_inner,shallowpercent_inner,moderatepercent_inner,deeppercent_inner,weakpercent_outer,stratiformpercent_outer,shallowpercent_outer,moderatepercent_outer,deeppercent_outer,closure_stratiform,closure_shallow,closure_moderate,closure_deep,symmetry_w1_dbz5_p,symmetry_all_dbz5_p,symmetry_w1_vt10_p,symmetry_all_vt10_p,shearmag_2km_5km_local,sheardir_2km_5km_local,shearmag_2km_8km_local,sheardir_2km_8km_local,shearmag_2km_10km_local,sheardir_2km_10km_local))
				f.close()
	

				if os.path.exists(f'{NMLDIR}/namelist.polar.structure.{EXPT}'):
					namelist_structure_vars = np.genfromtxt(f'{NMLDIR}/namelist.polar.structure.{EXPT}',delimiter=',',dtype='str')
				else:
					namelist_structure_vars = np.genfromtxt(f'{NMLDIR}/namelist.polar.structure',delimiter=',',dtype='str')

				#############################################################################################################################################
				# THIS BLOCK OF CODE WRITES AN OPTIONAL NETCDF FILE (BASED ON A NAMELIST PARAMETER) WITH AZIMUTHAL MEAN VARIABLES
				#############################################################################################################################################
				do_write_netcdf = namelist_structure_vars[22,1]
				if do_write_netcdf == 'Y':
				
					#Make File With Azimuthal-Means and Structure Variables
					fn = ODIR+'/'+LONGSID.lower()+'.polar_data.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.nc'
					ds = netCDF4.Dataset(fn, 'w', format='NETCDF4')

					rdim = ds.createDimension('rdim',np.shape(r)[0])
					adim = ds.createDimension('adim',np.shape(theta)[0])
					zdim = ds.createDimension('zdim',np.shape(heightlevs)[0])
					zpbldim = ds.createDimension('zpbldim',np.shape(heightlevs_pbl)[0])

					radius_write = ds.createVariable('radius','f4',('rdim',))
					azimuth_write = ds.createVariable('azimuth','f4',('adim',))
					height_write = ds.createVariable('height','f4',('zdim'))
					vt_write = ds.createVariable('tangential_wind','f4',('adim','rdim','zdim'))
					vt_write.units = 'Unknown'
					ur_write = ds.createVariable('radial_wind','f4',('adim','rdim','zdim'))
					ur_write.units = 'Unknown'
					w_write = ds.createVariable('vertical_wind','f4',('adim','rdim','zdim'))
					w_write.units = 'Unknown'
					dbz_write = ds.createVariable('reflectivity','f4',('adim','rdim','zdim'))
					dbz_write.units = 'Unknown'
					q_write = ds.createVariable('specific_humidity','f4',('adim','rdim','zdim'))
					q_write.units = 'Unknown'
					rh_write = ds.createVariable('relative_humidity','f4',('adim','rdim','zdim'))
					rh_write.units = 'Unknown'
					temp_write = ds.createVariable('temperature','f4',('adim','rdim','zdim'))
					temp_write.units = 'Unknown'
					pressure_write = ds.createVariable('pressure','f4',('adim','rdim','zdim'))
					pressure_write.units = 'Unknown'

					vt_pbl_write = ds.createVariable('pbl_tangential_wind','f4',('adim','rdim','zpbldim'))
					vt_pbl_write.units = 'Unknown'
					ur_pbl_write = ds.createVariable('pbl_radial_wind','f4',('adim','rdim','zpbldim'))
					ur_pbl_write.units = 'Unknown'

					rmw2km_write = ds.createVariable('rmw_2km','f4')
					rmw2km_write.units = 'Unknown'
					vmax_write = ds.createVariable('vmax','f4')
					vmax_write.units = 'Unknown'
					pmin_write = ds.createVariable('pmin','f4')
					pmin_write.units = 'Unknown'
					shearmagnitude_write = ds.createVariable('shearmag','f4')
					shearmagnitude_write.units = 'Unknown'
					sheardirection_write = ds.createVariable('sheardir','f4')
					sheardirection_write.units = 'Unknown'
					longitude_write = ds.createVariable('longitude','f4')
					longitude_write.units = 'Unknown'
					latitude_write = ds.createVariable('latitude','f4')
					latitude_write.units = 'Unknown'
					vortex_depth_dynamic_write = ds.createVariable('vortex_depth_dynamic','f4')
					vortex_depth_dynamic_write.units = 'Unknown'
					vortex_depth_static_write = ds.createVariable('vortex_depth_static','f4')
					vortex_depth_static_write.units = 'Unknown'
					slopermw1_write = ds.createVariable('slope_rmw_linear_best_fit','f4')
					slopermw1_write.units = 'Unknown'
					slopermw2_write = ds.createVariable('slope_rmw_ratio','f4')
					slopermw2_write.units = 'Unknown'
					alphaparameter_write = ds.createVariable('alpha_decay_parameter','f4')
					alphaparameter_write.units = 'Unknown'
					rossbynumber_write = ds.createVariable('rossby','f4')
					rossbynumber_write.units = 'Unknown'
					warmcoremagnitude_write = ds.createVariable('warm_core_magnitude','f4')
					warmcoremagnitude_write.units = 'Unknown'
					warmcoreheight_write = ds.createVariable('warm_core_height','f4')
					warmcoreheight_write.units = 'Unknown'

					radius_write[:] = r
					height_write[:] = heightlevs
					vt_write[:] = vt_p
					ur_write[:] = ur_p
					w_write[:] = w_p
					dbz_write[:] = dbz_p
					q_write[:] = q_p
					rh_write[:] = rh_p
					temp_write[:] = temp_p
					pressure_write[:] = pressure_p
					vt_pbl_write[:] = vt_pbl_p
					ur_pbl_write[:] = ur_pbl_p
					rmw2km_write[:] = rmw_2km
					vmax_write[:] = maxwind
					pmin_write[:] = minpressure
					longitude_write[:] = centerlon
					latitude_write[:] = centerlat
					shearmagnitude_write[:] = shearmag
					sheardirection_write[:] = sheardir
					vortex_depth_dynamic_write[:] = vortex_depth_vt_dynamic
					vortex_depth_static_write[:] = vortex_depth_vt_static
					slopermw1_write[:] = slope_rmw_1
					slopermw2_write[:] = slope_rmw_2
					alphaparameter_write[:] = alpha
					rossbynumber_write[:] = rossby
					warmcoremagnitude_write[:] = temp_anomaly_max
					warmcoreheight_write[:] = height_temp_anomaly_max

					ds.close()
					
				#############################################################################################################################################
				# END OF BLOCK OF CODE TO WRITE A NETCDF FILE
				#############################################################################################################################################
				
				#############################################################################################################################################
				# CREATE THE GRAPHICS HERE
				#############################################################################################################################################
				print('MSG: Doing Plots Now')
				start = time.perf_counter()
				do_ur_mean = namelist_structure_vars[0,1]
				do_vt_mean = namelist_structure_vars[1,1]
				do_w_mean = namelist_structure_vars[2,1]
				do_dbz_mean = namelist_structure_vars[3,1]
				do_rh_mean = namelist_structure_vars[4,1]
				do_dbz_alongshear = namelist_structure_vars[5,1]
				do_ur_alongshear = namelist_structure_vars[6,1]
				do_w_alongshear = namelist_structure_vars[7,1]
				do_rh_alongshear = namelist_structure_vars[8,1]
				do_dbz_acrossshear = namelist_structure_vars[9,1]
				do_ur_acrossshear = namelist_structure_vars[10,1]
				do_w_acrossshear = namelist_structure_vars[11,1]
				do_rh_acrossshear = namelist_structure_vars[12,1]
				do_dbz5km_wavenumber = namelist_structure_vars[13,1]
				do_rh5km_wavenumber = namelist_structure_vars[14,1]
				do_vt10_wavenumber = namelist_structure_vars[15,1]
				do_vt_tendency = namelist_structure_vars[16,1]
				do_vort_tendency = namelist_structure_vars[17,1]
				do_ur_pbl_p_mean = namelist_structure_vars[18,1]
				do_radar_plots = namelist_structure_vars[19,1]
				do_soundings = namelist_structure_vars[20,1]
				do_shear_and_rh_plots = namelist_structure_vars[21,1]
	
				#Do the Sounding Plots First Since Those Call an External Function      
				if ( do_soundings == 'Y'):
					skewTmodelTCpolar.skewTmodelTCpolar(r,theta,pressure_p,u_p,v_p,temp_p,rh_p,float(rmwnmi),GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)
	
				#Load the colormaps needed
				color_data_vt = np.genfromtxt(f'{PYTHONDIR}/colormaps/colormap_wind.txt')
				colormap_vt = matplotlib.colors.ListedColormap(color_data_vt)
				levs_vt = np.linspace(0,80,41,endpoint=True)
				norm_vt = colors.BoundaryNorm(levs_vt,256)
	
				color_data_ur = np.genfromtxt(f'{PYTHONDIR}/colormaps/bluewhitered.txt')
				colormap_ur = matplotlib.colors.ListedColormap(color_data_ur)
				levs_ur = np.linspace(-30,30,31,endpoint=True)
				norm_ur = colors.BoundaryNorm(levs_ur,256)
	
				color_data_w = np.genfromtxt(f'{PYTHONDIR}/colormaps/bluewhitered.txt')
				colormap_w = matplotlib.colors.ListedColormap(color_data_w)
				levs_w = np.linspace(-5,5,41,endpoint=True)
				norm_w = colors.BoundaryNorm(levs_w,256)
	
				color_data_dbz = np.genfromtxt(f'{PYTHONDIR}/colormaps/colormap_radar.txt')
				colormap_dbz = matplotlib.colors.ListedColormap(color_data_dbz)
				levs_dbz = np.linspace(0,80,41,endpoint=True)
				norm_dbz = colors.BoundaryNorm(levs_dbz,256)
	
				color_data_rh = np.genfromtxt(f'{PYTHONDIR}/colormaps/colormap_brown_to_green.txt')
				colormap_rh = matplotlib.colors.ListedColormap(color_data_rh)
				levs_rh = np.linspace(0,100,41,endpoint=True)
				norm_rh = colors.BoundaryNorm(levs_rh,256)
	
	
				color_data_wind = np.genfromtxt(f'{PYTHONDIR}/colormaps/colormap_wind.txt')
				colormap_wind = matplotlib.colors.ListedColormap(color_data_wind)
				levs_wind = [0,7,10,13,16,19,22,25,28,31,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,69.333,74.666,80,85.333,90.666,96,100.666,105.333,110,115,120,125,130,132,140,145,150,155,160]
				norm_wind = colors.BoundaryNorm(levs_wind,256)
	
				color_data_vt_budget = np.genfromtxt(f'{PYTHONDIR}/colormaps/bluewhitered.txt')
				colormap_vt_budget = matplotlib.colors.ListedColormap(color_data_vt_budget)
				levs_vt_budget = np.linspace(-10,10,81,endpoint=True)
				norm_vt_budget = colors.BoundaryNorm(levs_vt_budget,256)
	
				color_data_vort_budget = np.genfromtxt(f'{PYTHONDIR}/colormaps/bluewhitered.txt')
				colormap_vort_budget = matplotlib.colors.ListedColormap(color_data_vort_budget)
				levs_vort_budget = np.linspace(-40,40,41,endpoint=True)
				norm_vort_budget = colors.BoundaryNorm(levs_vort_budget,256)
	
				# FIGURE 1: Azimuthal Mean Radial Wind
				if do_ur_mean == 'Y':
					fig1 = plt.figure(figsize=(20.5, 10.5))
					ax1 = fig1.add_subplot(1, 1, 1)
					co1 = ax1.contourf(r, heightlevs/1000, np.flipud(np.rot90(ur_p_mean,1)), levs_ur, \
							  cmap=colormap_ur, norm=norm_ur, extend='both')
					ax1 = plotting.axes_radhgt(ax1, xmax=rmax)
					cbar1 = plt.colorbar(co1, ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
					cbar1.ax.tick_params(labelsize=24)
					ax1.set_title(f'{EXPT.strip()}\n' + \
						      r'Azimuthal Mean Radial Wind ($m\ s^{-1}$, Shading)' + \
						      f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						      fontsize=24, weight='bold', loc='left')
					ax1.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.ur_mean.{forecastinit}.polar.f{FHR:03}'
					fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig1)
	
	
				# FIGURE 2: Azimuthal Mean Tangential Wind
				if do_vt_mean == 'Y':
					fig2 = plt.figure(figsize=(20.5, 10.5))
					ax2 = fig2.add_subplot(1, 1, 1)
					co2 = ax2.contourf(r, heightlevs/1000, np.flipud(np.rot90(vt_p_mean, 1)), levs_vt, \
							   cmap=colormap_vt, norm=norm_vt, extend='max')
					ax2 = plotting.axes_radhgt(ax2, xmax=rmax)
					cbar2 = plt.colorbar(co2, ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80])
					cbar2.ax.tick_params(labelsize=24)
					ax2.set_title(f'{EXPT.strip()}\n' + \
						      r'Azimuthal Mean Tangential Wind ($m\ s^{-1}$, Shading)' + \
						      f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						      fontsize=24, weight='bold', loc='left')
					ax2.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.vt_mean.{forecastinit}.polar.f{FHR:03}'
					fig2.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig2)
	
	
				# FIGURE 3: Azimuthal Mean Vertical Velocity
				if do_w_mean == 'Y':
					fig3 = plt.figure(figsize=(20.5, 10.5))
					ax3 = fig3.add_subplot(1, 1, 1)
					co3 = ax3.contourf(r, heightlevs/1000, np.flipud(np.rot90(w_p_mean, 1)), levs_w, \
							   cmap=colormap_w, norm=norm_w, extend='both')
					ax3 = plotting.axes_radhgt(ax3, xmax=rmax)
					cbar3 = plt.colorbar(co3, ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
					cbar3.ax.tick_params(labelsize=24)
					ax3.set_title(f'{EXPT.strip()}\n' + \
						      r'Azimuthal Mean W ($m\ s^{-1}$, Shading)' + \
						      f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						      fontsize=24, weight='bold', loc='left')
					ax3.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.w_mean.{forecastinit}.polar.f{FHR:03}'
					fig3.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig3)
	
	
				# FIGURE 4: Azimuthal Mean Reflectivity
				if do_dbz_mean == 'Y':
					fig4 = plt.figure(figsize=(20.5, 10.5))
					ax4 = fig4.add_subplot(1, 1, 1)
					co4 = ax4.contourf(r, heightlevs/1000, np.flipud(np.rot90(dbz_p_mean, 1)), levs_dbz, \
							   cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax4 = plotting.axes_radhgt(ax4, xmax=rmax)
					cbar4 = plt.colorbar(co4, ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
					cbar4.ax.tick_params(labelsize=24)
					ax4.set_title(f'{EXPT.strip()}\n' + \
						      r'Azimuthal Mean Reflectivity ($dBZ$, Shading)' + \
						      f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						      fontsize=24, weight='bold', loc='left')
					ax4.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.dbz_mean.{forecastinit}.polar.f{FHR:03}'
					fig4.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig4)
	
	
				# FIGURE 5: Azimuthal Mean Relative Humidity
				if do_rh_mean == 'Y':
					fig5 = plt.figure(figsize=(20.5, 10.5))
					ax5 = fig5.add_subplot(1, 1, 1)
					co5 = ax5.contourf(r, heightlevs/1000, np.flipud(np.rot90(rh_p_mean, 1)), levs_rh, \
							   cmap=colormap_rh, norm=norm_rh, extend='max')
					ax5 = plotting.axes_radhgt(ax5, xmax=rmax)
					cbar5 = plt.colorbar(co5, ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
					cbar5.ax.tick_params(labelsize=24)
					ax5.set_title(f'{EXPT.strip()}\n' + \
						      r'Azimuthal Mean Relative Humidity ($\%$, Shading)' + \
						      f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						      fontsize=24, weight='bold', loc='left')
					ax5.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.rh_mean.{forecastinit}.polar.f{FHR:03}'
					fig5.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig5)
	
	
				# FIGURE 6: Along-Shear Reflectivity
				if do_dbz_alongshear == 'Y':
					fig6 = plt.figure(figsize=(20.5, 10.5))
					ax6 = fig6.add_subplot(1, 1, 1)
					co6 = ax6.contourf(r, heightlevs/1000, np.flipud(np.rot90(dbz_p_downshear_mean, 1)), levs_dbz, \
							   cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax6.contourf(-r, heightlevs/1000, np.flipud(np.rot90(dbz_p_upshear_mean, 1)), levs_dbz, \
						     cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax6 = plotting.axes_radhgt(ax6, xmax=rmax, xmin=-rmax)
					cbar6 = plt.colorbar(co6, ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
					cbar6.ax.tick_params(labelsize=24)
					ax6.text(-rmax+0.05*(2*rmax), 18-(0.05*18), 'Upshear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax6.text(rmax-0.05*(2*rmax), 18-(0.05*18), 'Downshear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax6.set_title(f'{EXPT.strip()}\n' + \
						      r'Along-Shear Reflectivity ($dBZ$, Shading)' + \
						      f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						      fontsize=24, weight='bold', loc='left')
					ax6.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.dbz_alongshear.{forecastinit}.polar.f{FHR:03}'
					fig6.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig6)
	
	
				# FIGURE 7: Along-Shear Radial Wind
				if do_ur_alongshear == 'Y':
					fig7 = plt.figure(figsize=(20.5, 10.5))
					ax7 = fig7.add_subplot(1, 1, 1)
					co7 = ax7.contourf(r, heightlevs/1000, np.flipud(np.rot90(ur_p_downshear_mean, 1)), levs_ur, \
							   cmap=colormap_ur, norm=norm_ur, extend='both')
					ax7.contourf(-r,heightlevs/1000,np.flipud(np.rot90(ur_p_upshear_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
					ax7 = plotting.axes_radhgt(ax7, xmax=rmax, xmin=-rmax)
					cbar7 = plt.colorbar(co7, ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
					cbar7.ax.tick_params(labelsize=24)
					ax7.text(-rmax+0.05*(2*rmax), 18-(0.05*18), 'Upshear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax7.text(rmax-0.05*(2*rmax), 18-(0.05*18), 'Downshear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax7.set_title(f'{EXPT.strip()}\n' + \
						      r'Along-Shear Radial Wind ($m\ s^{-1}$, Shading)' + \
						      f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						      fontsize=24, weight='bold', loc='left')
					ax7.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.ur_alongshear.{forecastinit}.polar.f{FHR:03}'
					fig7.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig7)
	
	
				# FIGURE 8: Along-Shear Vertical Velocity
				if do_w_alongshear == 'Y':
					fig8 = plt.figure(figsize=(20.5, 10.5))
					ax8 = fig8.add_subplot(1, 1, 1)
					co8 = ax8.contourf(r, heightlevs/1000, np.flipud(np.rot90(w_p_downshear_mean, 1)), levs_w, \
							   cmap=colormap_w, norm=norm_w, extend='both')
					ax8.contourf(-r, heightlevs/1000, np.flipud(np.rot90(w_p_upshear_mean, 1)), levs_w, \
						     cmap=colormap_w, norm=norm_w, extend='both')
					ax8 = plotting.axes_radhgt(ax8, xmax=rmax, xmin=-rmax)
					cbar8 = plt.colorbar(co8, ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
					cbar8.ax.tick_params(labelsize=24)
					ax8.text(-rmax+0.05*(2*rmax), 18-(0.05*18), 'Upshear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax8.text(rmax-0.05*(2*rmax), 18-(0.05*18), 'Downshear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax8.set_title(f'{EXPT.strip()}\n' + \
						      r'Along-Shear W ($m\ s^{-1}$, Shading)' + \
						      f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						      fontsize=24, weight='bold', loc='left')
					ax8.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.w_alongshear.{forecastinit}.polar.f{FHR:03}'
					fig8.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig8)
	
	
				# FIGURE 9: Along-Shear Relative Humidity
				if do_rh_alongshear == 'Y':
					fig9 = plt.figure(figsize=(20.5, 10.5))
					ax9 = fig9.add_subplot(1, 1, 1)
					co9 = ax9.contourf(r, heightlevs/1000, np.flipud(np.rot90(rh_p_downshear_mean, 1)), levs_rh, \
							   cmap=colormap_rh, norm=norm_rh, extend='both')
					ax9.contourf(-r, heightlevs/1000, np.flipud(np.rot90(rh_p_upshear_mean, 1)), levs_rh, \
						     cmap=colormap_rh, norm=norm_rh, extend='both')
					ax9 = plotting.axes_radhgt(ax9, xmax=rmax, xmin=-rmax)
					cbar9 = plt.colorbar(co9, ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
					cbar9.ax.tick_params(labelsize=24)
					ax9.text(-rmax+0.05*(2*rmax), 18-(0.05*18), 'Upshear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax9.text(rmax-0.05*(2*rmax), 18-(0.05*18), 'Downshear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax9.set_title(f'{EXPT.strip()}\n' + \
						      r'Along-Shear RH ($\%$, Shading)' + \
						      f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						      fontsize=24, weight='bold', loc='left')
					ax9.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.rh_alongshear.{forecastinit}.polar.f{FHR:03}'
					fig9.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig9)
	
	
				# FIGURE 10: Across-Shear Reflectivity
				if do_dbz_acrossshear == 'Y':
					fig10 = plt.figure(figsize=(20.5, 10.5))
					ax10 = fig10.add_subplot(1, 1, 1)
					co10 = ax10.contourf(r, heightlevs/1000, np.flipud(np.rot90(dbz_p_rightshear_mean, 1)), levs_dbz, \
							   cmap=colormap_dbz, norm=norm_dbz, extend='both')
					ax10.contourf(-r, heightlevs/1000, np.flipud(np.rot90(dbz_p_leftshear_mean, 1)), levs_dbz, \
						     cmap=colormap_dbz, norm=norm_dbz, extend='both')
					ax10 = plotting.axes_radhgt(ax10, xmax=rmax, xmin=-rmax)
					cbar10 = plt.colorbar(co10, ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
					cbar10.ax.tick_params(labelsize=24)
					ax10.text(-rmax+0.05*(2*rmax), 18-(0.05*18), 'Left of shear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax10.text(rmax-0.05*(2*rmax), 18-(0.05*18), 'Right of shear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax10.set_title(f'{EXPT.strip()}\n' + \
						       r'Across-Shear Reflectivity ($dBZ$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax10.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.rh_acrossshear.{forecastinit}.polar.f{FHR:03}'
					fig10.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig10)
	
	
				# FIGURE 11: Across-Shear Radiaul Wind
				if do_ur_acrossshear == 'Y':
					fig11 = plt.figure(figsize=(20.5, 10.5))
					ax11 = fig11.add_subplot(1, 1, 1)
					co11 = ax11.contourf(r, heightlevs/1000, np.flipud(np.rot90(ur_p_rightshear_mean, 1)), levs_ur, \
							   cmap=colormap_ur, norm=norm_ur, extend='both')
					ax11.contourf(-r, heightlevs/1000, np.flipud(np.rot90(ur_p_leftshear_mean, 1)), levs_ur, \
						     cmap=colormap_ur, norm=norm_ur, extend='both')
					ax11 = plotting.axes_radhgt(ax11, xmax=rmax, xmin=-rmax)
					cbar11 = plt.colorbar(co11, ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
					cbar11.ax.tick_params(labelsize=24)
					ax11.text(-rmax+0.05*(2*rmax), 18-(0.05*18), 'Left of shear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax11.text(rmax-0.05*(2*rmax), 18-(0.05*18), 'Right of shear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax11.set_title(f'{EXPT.strip()}\n' + \
						       r'Across-Shear Radial Wind ($m\ s^{-1}$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax11.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.ur_acrossshear.{forecastinit}.polar.f{FHR:03}'
					fig11.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig11)
	
	
				# FIGURE 12: Across-Shear Vertical Velocity
				if do_w_acrossshear == 'Y':
					fig12 = plt.figure(figsize=(20.5, 10.5))
					ax12 = fig12.add_subplot(1, 1, 1)
					co12 = ax12.contourf(r, heightlevs/1000, np.flipud(np.rot90(w_p_rightshear_mean, 1)), levs_w, \
							   cmap=colormap_w, norm=norm_w, extend='both')
					ax12.contourf(-r, heightlevs/1000, np.flipud(np.rot90(w_p_leftshear_mean, 1)), levs_w, \
						     cmap=colormap_w, norm=norm_w, extend='both')
					ax12 = plotting.axes_radhgt(ax12, xmax=rmax, xmin=-rmax)
					cbar12 = plt.colorbar(co12, ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
					cbar12.ax.tick_params(labelsize=24)
					ax12.text(-rmax+0.05*(2*rmax), 18-(0.05*18), 'Left of shear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax12.text(rmax-0.05*(2*rmax), 18-(0.05*18), 'Right of shear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax12.set_title(f'{EXPT.strip()}\n' + \
						       r'Across-Shear W ($m\ s^{-1}$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax12.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.w_acrossshear.{forecastinit}.polar.f{FHR:03}'
					fig12.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig12)
	
	
				# FIGURE 13: Across-Shear Relative Humidity
				if do_rh_acrossshear == 'Y':
					fig13 = plt.figure(figsize=(20.5, 10.5))
					ax13 = fig13.add_subplot(1, 1, 1)
					co13 = ax13.contourf(r, heightlevs/1000, np.flipud(np.rot90(rh_p_rightshear_mean, 1)), levs_rh, \
							   cmap=colormap_rh, norm=norm_rh, extend='both')
					ax13.contourf(-r, heightlevs/1000, np.flipud(np.rot90(rh_p_leftshear_mean, 1)), levs_rh, \
						     cmap=colormap_rh, norm=norm_rh, extend='both')
					ax13 = plotting.axes_radhgt(ax13, xmax=rmax, xmin=-rmax)
					cbar13 = plt.colorbar(co13, ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
					cbar13.ax.tick_params(labelsize=24)
					ax13.text(-rmax+0.05*(2*rmax), 18-(0.05*18), 'Left of shear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax13.text(rmax-0.05*(2*rmax), 18-(0.05*18), 'Right of shear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax13.set_title(f'{EXPT.strip()}\n' + \
						       r'Across-Shear RH ($\%$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax13.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.rh_acrossshear.{forecastinit}.polar.f{FHR:03}'
					fig13.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig13)
	
	
				# FIGURE 14: Wavenumber 0,1,2 components of 5-km Reflectivity
				if do_dbz5km_wavenumber == 'Y':
					fig14 = plt.figure(figsize=(15,15))
					ticks14 = [0, 10, 20, 30, 40, 50, 60, 70]

					# Panel A	
					ax14a = fig14.add_subplot(2, 2, 1)
					co14a = ax14a.contourf(XI, YI, dbz5_p[:,:], levs_dbz, \
								cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax14a = plotting.axes_wavenumber(ax14a, rmax/2, -rmax/2)
					cbar14a = plt.colorbar(co14a, ticks=ticks14)
					cbar14a.ax.tick_params(labelsize=18)
					ax14a.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax14a.set_title(f'{EXPT.strip()}\n' + \
							r'WV#0,1,2 5-km Reflectivity ($dBZ$, Shading)' + \
							f'\nShear Vector in Black\nInit: {forecastinit}\nForecast Hour:[{FHR:03}]', \
							fontsize=20, weight='bold', loc='left')
					ax14a.text(0,rmax/2-50,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

					# Panel B
					ax14b = fig14.add_subplot(2, 2, 2)
					co14b = ax14b.contourf(XI, YI, dbz5_p_w0[:,:], levs_dbz, \
								cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax14b = plotting.axes_wavenumber(ax14b, rmax/2, -rmax/2)
					cbar14b = plt.colorbar(co14b, ticks=ticks14)
					cbar14b.ax.tick_params(labelsize=18)
					ax14b.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax14b.set_title(f'{LONGSID.upper()}\nVMAX= {maxwind} kt\nPMIN= {minpressure} hPa' + \
							f'\nShear Magnitude= {str(int(np.round(shearmag*1.94,0)))}kts\nShear Direction= {str(int(np.round(sheardir_met,0)))}$^\circ$', \
							fontsize=20, color='brown', loc='right')
					ax14b.text(0,rmax/2-50,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

					# Panel C	
					ax14c = fig14.add_subplot(2, 2, 3)
					co14c = ax14c.contourf(XI, YI, dbz5_p_w1[:,:], levs_dbz, \
								cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax14c = plotting.axes_wavenumber(ax14c, rmax/2, -rmax/2)
					cbar14c = plt.colorbar(co14c, ticks=ticks14)
					cbar14c.ax.tick_params(labelsize=18)
					ax14c.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax14c.text(0,rmax/2-50,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

					# Panel D
					ax14d = fig14.add_subplot(2, 2, 4)
					co14d = ax14d.contourf(XI, YI, dbz5_p_w2[:,:], levs_dbz, \
								cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax14d = plotting.axes_wavenumber(ax14d, rmax/2, -rmax/2)
					cbar14d = plt.colorbar(co14d, ticks=ticks14)
					cbar14d.ax.tick_params(labelsize=18)
					ax14d.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax14d.text(0,rmax/2-50,'Wavenumber 2',fontsize=20,style='italic',horizontalalignment='center')

					# Finalize figure
					figfname = f'{ODIR}/{LONGSID.lower()}.dbz5km_wavenumber.{forecastinit}.polar.f{FHR:03}'
					#fig14.tight_layout()
					fig14.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					plt.close(fig14)
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')

	
				# FIGURE 15: Wavenumber 0,1,2 components of 5-km Relative Humidity
				if do_rh5km_wavenumber == 'Y':
					fig15 = plt.figure(figsize=(15,15))
					ticks15 = [0, 20, 40, 60, 80, 100]

					# Panel A
					ax15a = fig15.add_subplot(2, 2, 1)
					co15a = ax15a.contourf(XI, YI, rh5_p[:,:], levs_rh, \
								cmap=colormap_rh, norm=norm_rh, extend='max')
					ax15a = plotting.axes_wavenumber(ax15a, rmax/2, -rmax/2)
					cbar15a = plt.colorbar(co15a, ticks=ticks15)
					cbar15a.ax.tick_params(labelsize=18)
					ax15a.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax15a.set_title(f'{EXPT.strip()}\n' + \
							r'WV#0,1,2 5-km RH ($\%$, Shading)' + \
							f'\nShear Vector in Black\nInit: {forecastinit}\nForecast Hour:[{FHR:03}]', \
							fontsize=20, weight='bold', loc='left')
					ax15a.text(0,rmax/2-50,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

					# Panel B
					ax15b = fig15.add_subplot(2, 2, 2)
					co15b = ax15b.contourf(XI, YI, rh5_p_w0[:,:], levs_rh, \
								cmap=colormap_rh, norm=norm_rh, extend='max')
					ax15b = plotting.axes_wavenumber(ax15b, rmax/2, -rmax/2)
					cbar15b = plt.colorbar(co15b, ticks=ticks15)
					cbar15b.ax.tick_params(labelsize=18)
					ax15b.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax15b.set_title(f'{LONGSID.upper()}\nVMAX= {maxwind} kt\nPMIN= {minpressure} hPa' + \
							f'\nShear Magnitude= {str(int(np.round(shearmag*1.94,0)))}kts\nShear Direction= {str(int(np.round(sheardir_met,0)))}$^\circ$', \
							fontsize=20, color='brown', loc='right')
					ax15b.text(0,rmax/2-50,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

					# Panel C
					ax15c = fig15.add_subplot(2, 2, 3)
					co15c = ax15c.contourf(XI, YI, rh5_p_w1[:,:], levs_rh, \
								cmap=colormap_rh, norm=norm_rh, extend='max')
					ax15c = plotting.axes_wavenumber(ax15c, rmax/2, -rmax/2)
					cbar15c = plt.colorbar(co15c, ticks=ticks15)
					cbar15c.ax.tick_params(labelsize=18)
					ax15c.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax15c.text(0,rmax/2-50,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

					# Panel D
					ax15d = fig15.add_subplot(2, 2, 4)
					co15d = ax15d.contourf(XI, YI, rh5_p_w2[:,:], levs_rh, \
								cmap=colormap_rh, norm=norm_rh, extend='max')
					ax15d = plotting.axes_wavenumber(ax15d, rmax/2, -rmax/2)
					cbar15d = plt.colorbar(co15d, ticks=ticks15)
					cbar15d.ax.tick_params(labelsize=18)
					ax15d.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax15d.text(0,rmax/2-50,'Wavenumber 2',fontsize=20,style='italic',horizontalalignment='center')

					# Finalize figure
					figfname = f'{ODIR}/{LONGSID.lower()}.rh5km_wavenumber.{forecastinit}.polar.f{FHR:03}'
					fig15.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					plt.close(fig15)
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
	
	
				# FIGURE 16: Wavenumber 0,1,2 components of 10-m Tangential Wind
				if do_vt10_wavenumber == 'Y':
					fig16 = plt.figure(figsize=(15,15))
					ticks16 = [-30, -20, -10, 0, 10, 20, 30]

					# Panel A
					ax16a = fig16.add_subplot(2, 2, 1)
					co16a = ax16a.contourf(XI, YI, vt10_p[:,:], levs_vt, \
								cmap=colormap_vt, norm=norm_vt, extend='max')
					ax16a = plotting.axes_wavenumber(ax16a, rmax/2, -rmax/2)
					cbar16a = plt.colorbar(co16a, ticks=ticks16)
					cbar16a.ax.tick_params(labelsize=18)
					ax16a.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax16a.set_title(f'{EXPT.strip()}\n' +\
							r'WV#0,1,2 10-m Tangential Wind ($m\ s^{-1}$, Shading)' + \
							f'\nShear Vector in Black\nInit: {forecastinit}\nForecast Hour:[{FHR:03}]', \
							fontsize=20, weight='bold', loc='left')
					ax16a.text(0,rmax/2-50,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

					# Panel B
					ax16b = fig16.add_subplot(2, 2, 2)
					co16b = ax16b.contourf(XI, YI, vt10_p_w0[:,:], levs_vt, \
								cmap=colormap_vt, norm=norm_vt, extend='max')
					ax16b = plotting.axes_wavenumber(ax16b, rmax/2, -rmax/2)
					cbar16b = plt.colorbar(co16b, ticks=ticks16)
					cbar16b.ax.tick_params(labelsize=18)
					ax16b.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax16b.set_title(f'{LONGSID.upper()}\nVMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n' + \
							f'Shear Magnitude= {str(int(np.round(shearmag*1.94,0)))}kts\nShear Direction= {str(int(np.round(sheardir_met,0)))}$^\circ$', \
							fontsize=20, color='brown', loc='right')
					ax16b.text(0,rmax/2-50,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

					# Panel C
					ax16c = fig16.add_subplot(2, 2, 3)
					co16c = ax16c.contourf(XI, YI, vt10_p_w1[:,:], levs_vt, \
								cmap=colormap_vt, norm=norm_vt, extend='max')
					ax16c = plotting.axes_wavenumber(ax16c, rmax/2, -rmax/2)
					cbar16c = plt.colorbar(co16c, ticks=ticks16)
					cbar16c.ax.tick_params(labelsize=18)
					ax16c.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax16c.text(0,rmax/2-50,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

					# Panel D
					ax16d = fig16.add_subplot(2, 2, 4)
					co16d = ax16d.contourf(XI, YI, vt10_p_w2[:,:], levs_vt, \
								cmap=colormap_vt, norm=norm_vt, extend='max')
					ax16d = plotting.axes_wavenumber(ax16d, rmax/2, -rmax/2)
					cbar16d = plt.colorbar(co16d, ticks=ticks16)
					cbar16d.ax.tick_params(labelsize=18)
					ax16d.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax16d.text(0,rmax/2-50,'Wavenumber 2',fontsize=20,style='italic',horizontalalignment='center')

					# Finalize figure
					figfname = f'{ODIR}/{LONGSID.lower()}.vt10_wavenumber.{forecastinit}.polar.f{FHR:03}'
					fig16.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					plt.close(fig16)			
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
	
	
				# FIGURES 17-21: Tangential Wind Tendency Terms
				if do_vt_tendency == 'Y':

					# Mean Radial Flux
					fig17 = plt.figure(figsize=(20.5,10.5))
					ax17 = fig17.add_subplot(1, 1, 1)
					co17 = ax17.contourf(r, heightlevs/1000, np.flipud(np.rot90(term1_vt_tendency_mean_radial_flux*1e3,1)), levs_vt_budget, \
							     cmap=colormap_vt_budget, norm=norm_vt_budget, extend='both')
					ax17 = plotting.axes_radhgt(ax17, xmax=rmax, formatters=True)
					cbar17 = plt.colorbar(co17, ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
					cbar17.ax.tick_params(labelsize=24)
					sc17 = ax17.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
					ax17.set_title(f'{EXPT.strip()}\n' + \
						       r'$-\langle u_{r} \rangle \langle f+\zeta \rangle$ ($10^{-3} m s^{-2}$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax17.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.vt_tendency_term1_mean_radial_flux_mean.{forecastinit}.polar.f{FHR:03}'
					fig17.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig17)

					# Mean Vertical Advection
					fig18 = plt.figure(figsize=(20.5,10.5))
					ax18 = fig18.add_subplot(1, 1, 1)
					co18 = ax18.contourf(r, heightlevs/1000, np.flipud(np.rot90(term2_vt_tendency_mean_vertical_advection*1e3,1)), levs_vt_budget, \
							     cmap=colormap_vt_budget, norm=norm_vt_budget, extend='both')
					ax18 = plotting.axes_radhgt(ax18, xmax=rmax, formatters=True)
					cbar18 = plt.colorbar(co18, ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
					cbar18.ax.tick_params(labelsize=24)
					sc18 = ax18.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
					ax18.set_title(f'{EXPT.strip()}\n' + \
						       r'$-\langle w \rangle \frac{\partial{\langle v_{t} \rangle}}{\partial z}$ ($10^{-3} m s^{-2}$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax18.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.vt_tendency_term2_mean_vertical_advection_mean.{forecastinit}.polar.f{FHR:03}'
					fig18.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig18)

					# Mean Eddy Flux
					fig19 = plt.figure(figsize=(20.5,10.5))
					ax19 = fig19.add_subplot(1, 1, 1)
					co19 = ax19.contourf(r, heightlevs/1000, np.flipud(np.rot90(term3_vt_tendency_eddy_flux*1e3,1)), levs_vt_budget, \
							     cmap=colormap_vt_budget, norm=norm_vt_budget, extend='both')
					ax19 = plotting.axes_radhgt(ax19, xmax=rmax, formatters=True)
					cbar19 = plt.colorbar(co19, ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
					cbar19.ax.tick_params(labelsize=24)
					sc19 = ax19.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
					ax19.set_title(f'{EXPT.strip()}\n' + \
						       r'$-\langle u^{\prime}_{r}\zeta^{\prime} \rangle$ ($10^{-3} m s^{-2}$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax19.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.vt_tendency_term3_eddy_flux_mean.{forecastinit}.polar.f{FHR:03}'
					fig19.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig19)

					# Mean Vertical Eddy Advection
					fig20 = plt.figure(figsize=(20.5,10.5))
					ax20 = fig20.add_subplot(1, 1, 1)
					co20 = ax20.contourf(r, heightlevs/1000, np.flipud(np.rot90(term4_vt_tendency_vertical_eddy_advection*1e3,1)), levs_vt_budget, \
							     cmap=colormap_vt_budget, norm=norm_vt_budget, extend='both')
					ax20 = plotting.axes_radhgt(ax20, xmax=rmax, formatters=True)
					cbar20 = plt.colorbar(co20, ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
					cbar20.ax.tick_params(labelsize=24)
					sc20 = ax20.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
					ax20.set_title(f'{EXPT.strip()}\n' + \
						       r'$-\langle w^{\prime}\frac{\partial{v^{\prime}_{t}}}{\partial z} \rangle$ ($10^{-3} m s^{-2}$, Shading)' + 
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax20.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.vt_tendency_term4_vertical_eddy_advection_mean.{forecastinit}.polar.f{FHR:03}'
					fig20.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig20)

					# Sum of Mean Tendency Terms
					fig21 = plt.figure(figsize=(20.5,10.5))
					ax21 = fig21.add_subplot(1, 1, 1)
					co21 = ax21.contourf(r, heightlevs/1000, np.flipud(np.rot90(terms_vt_tendency_sum*1e3,1)), levs_vt_budget, \
							     cmap=colormap_vt_budget, norm=norm_vt_budget, extend='both')
					ax21 = plotting.axes_radhgt(ax21, xmax=rmax, formatters=True)
					cbar21 = plt.colorbar(co21, ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
					cbar21.ax.tick_params(labelsize=24)
					sc21 = ax21.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
					ax21.set_title(f'{EXPT.strip()}\n' + \
						       r'Sum of $\frac{\partial{\langle v_{t} \rangle}}{\partial t}$ Terms ($10^{-3} m s^{-2}$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax21.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.vt_tendency_terms_sum_mean.{forecastinit}.polar.f{FHR:03}'
					fig21.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig21)

				# FIGURES 22-26: Vorticity Tendency Terms
				if ( do_vort_tendency == 'Y'):

					# Mean Horizontal Advection
					fig22 = plt.figure(figsize=(20.5,10.5))
					ax22 = fig22.add_subplot(1, 1, 1)
					co22 = ax22.contourf(r, heightlevs/1000, np.flipud(np.rot90(term1_vort_tendency_horizontal_advection*1e5*60,1)), levs_vort_budget, \
							     cmap=colormap_vort_budget, norm=norm_vort_budget, extend='both')
					ax22 = plotting.axes_radhgt(ax22, xmax=rmax, formatters=True)
					cbar22 = plt.colorbar(co22, ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
					cbar22.ax.tick_params(labelsize=24)
					sc22 = ax22.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
					ax22.set_title(f'{EXPT.strip()}\n' + \
						       r'$\langle -u_{SR}\frac{\partial{\eta}} {\partial x} - v_{SR}\frac{\partial{\eta}} {\partial y} \rangle$ ($10^{-5} s^{-1} min^{-1}$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax22.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.vort_tendency_term1_horizontal_advection_mean.{forecastinit}.polar.f{FHR:03}'
					fig22.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')

					# Mean Vertical Advection
					fig23 = plt.figure(figsize=(20.5,10.5))
					ax23 = fig23.add_subplot(1, 1, 1)
					co23 = ax23.contourf(r, heightlevs/1000, np.flipud(np.rot90(term2_vort_tendency_vertical_advection*1e5*60,1)), levs_vort_budget, \
							     cmap=colormap_vort_budget, norm=norm_vort_budget, extend='both')
					ax23 = plotting.axes_radhgt(ax23, xmax=rmax, formatters=True)
					cbar23 = plt.colorbar(co23, ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
					cbar23.ax.tick_params(labelsize=24)
					sc23 = ax23.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
					ax23.set_title(f'{EXPT.strip()}\n' + \
						       r'$\langle -w\frac{\partial{\zeta}} {\partial z} \rangle$ ($10^{-5} s^{-1} min^{-1}$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax23.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.vort_tendency_term2_vertical_advection_mean.{forecastinit}.polar.f{FHR:03}'
					fig23.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig23)

					# Mean Stretching COnvergence
					fig24 = plt.figure(figsize=(20.5,10.5))
					ax24 = fig24.add_subplot(1, 1, 1)
					co24 = ax24.contourf(r, heightlevs/1000, np.flipud(np.rot90(term3_vort_tendency_stretching_convergence*1e5*60,1)), levs_vort_budget, \
							     cmap=colormap_vort_budget, norm=norm_vort_budget, extend='both')
					ax24 = plotting.axes_radhgt(ax24, xmax=rmax, formatters=True)
					cbar24 = plt.colorbar(co24, ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
					cbar24.ax.tick_params(labelsize=24)
					sc24 = ax24.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
					ax24.set_title(f'{EXPT.strip()}\n' + \
						       r'$\langle -\eta\frac{\partial{u_{SR}}} {\partial x} - \eta\frac{\partial{v_{SR}}} {\partial y} \rangle$ ($10^{-5} s^{-1} min^{-1}$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax24.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.vort_tendency_term3_stretching_convergence_mean.{forecastinit}.polar.f{FHR:03}'
					fig24.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig24)

					# Mean Tilting
					fig25 = plt.figure(figsize=(20.5,10.5))
					ax25 = fig25.add_subplot(1, 1, 1)
					co25 = ax25.contourf(r, heightlevs/1000, np.flipud(np.rot90(term4_vort_tendency_tilting*1e5*60,1)), levs_vort_budget, \
							     cmap=colormap_vort_budget, norm=norm_vort_budget, extend='both')
					ax25 = plotting.axes_radhgt(ax25, xmax=rmax, formatters=True)
					cbar25 = plt.colorbar(co25, ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
					cbar25.ax.tick_params(labelsize=24)
					sc25 = ax25.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
					ax25.set_title(f'{EXPT.strip()}\n' + \
						       r'$\langle -\frac{\partial{w}}{\partial x}\frac{\partial{v_{SR}}} {\partial z} + \frac{\partial{w}}{\partial y}\frac{\partial{u_{SR}}} {\partial z} \rangle$ ($10^{-5} s^{-1} min^{-1}$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax25.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.vort_tendency_term4_tilting_mean.{forecastinit}.polar.f{FHR:03}'
					fig25.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig25)

					# Sum of Mean Tendency Terms
					fig26 = plt.figure(figsize=(20.5,10.5))
					ax26 = fig26.add_subplot(1, 1, 1)
					co26 = ax26.contourf(r, heightlevs/1000, np.flipud(np.rot90(terms_vort_tendency_sum*1e5*60,1)), levs_vort_budget, \
							     cmap=colormap_vort_budget, norm=norm_vort_budget, extend='both')
					ax26 = plotting.axes_radhgt(ax26, xmax=rmax, formatters=True)
					cbar26 = plt.colorbar(co26, ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
					cbar26.ax.tick_params(labelsize=24)
					sc26 = ax26.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
					ax26.set_title(f'{EXPT.strip()}\n' + \
						       r'Sum of $\frac{\partial{\langle \zeta \rangle}}{\partial t}$ Terms ($10^{-5} s^{-1} min^{-1}$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax26.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.vort_tendency_terms_sum_mean.{forecastinit}.polar.f{FHR:03}'
					fig26.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig26)
	
				if ( do_ur_pbl_p_mean == 'Y'):
					#Plot PBL Inflow
					fig27 = plt.figure(figsize=(20.5,10.5))
					ax27 = fig27.add_subplot(1, 1, 1)
					co27 = ax27.contourf(r, heightlevs_pbl, np.flipud(np.rot90(ur_pbl_p_mean,1)), levs_ur, \
							     cmap=colormap_ur, norm=norm_ur, extend='both')
					ax27 = plotting.axes_radhgt(ax27, xmax=rmax, ymax=3000, ny=7, yunit='m', formatters=True)
					cbar27 = plt.colorbar(co27, ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
					cbar27.ax.tick_params(labelsize=24)
					co27b = ax27.contour(r, heightlevs_pbl, np.flipud(np.rot90(ur_pbl_p_mean,1)), \
							     levels=[0.1*np.nanmin(ur_pbl_p_mean)], colors='w', linewidths=4)
					sc27 = ax27.scatter(rmw_pbl_mean, heightlevs_pbl, 70, 'k')
					ax27.set_title(f'{EXPT.strip()}\n' + \
						       r'Radial Wind in PBL ($m\ s^{-1}$, Shading)' + \
						       f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
						       fontsize=24, weight='bold', loc='left')
					ax27.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
					figfname = f'{ODIR}/{LONGSID.lower()}.ur_pbl_p_mean.{forecastinit}.polar.f{FHR:03}'
					fig27.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if DO_CONVERTGIF:
						os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')
					plt.close(fig27)
			
				if ( do_radar_plots == 'Y'):	
					#Make Plots for Comparison With Radar
					#Make Horizontal Wind and Reflectivity Plot
					plt.figure(figsize=(19.5,12))
					plt.subplot(121)
					plt.contourf(x_sr*0.54,y_sr*0.54,dbz_2km,levs_dbz,cmap=colormap_dbz,norm=norm_dbz)
					plt.xlim(-132,132)
					plt.ylim(-132,132)
					plt.xticks(np.linspace(-100,100,5),fontsize=14)
					plt.yticks(np.linspace(-100,100,5),fontsize=14)
					plt.gca().set_aspect('equal', adjustable='box')
					plt.grid()
					plt.arrow(0,0,ushear1*3.5*1.94,vshear1*3.5*1.94, width=2, head_width=10, head_length=10, fc='blue', ec='black')
					plt.xticks(np.linspace(-100,100,5),fontsize=14)
					plt.yticks(np.linspace(-100,100,5),fontsize=14)
					plt.xlabel('East-West Distance (n mi)',fontsize=18)
					plt.ylabel('North-South Distance (n mi)',fontsize=18)
					plt.title(EXPT.strip()+'\n'+ '2-km Reflectivity (dbz, Shading)'+'\n'+'2-km Wind Barbs (kt)'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
					plt.barbs(x_sr[::9]*0.54,y_sr[::9]*0.54,u2km[::9,::9]*1.94,v2km[::9,::9]*1.94,length=6,sizes=dict(spacing=0.15,height=0.4))
					ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80]
					ax = plt.gca()
					divider = make_axes_locatable(ax)
					cax = divider.append_axes("right", size="5%", pad=0.05)
					cbar_l = plt.colorbar(cax=cax,ticks=ticks,norm=norm_dbz,drawedges=True)
					cbar_l.set_ticklabels([0, 10, 20, 30, 40, 50, 60, 70, 80])
					cbar_l.ax.tick_params(labelsize=14)
					#cbar_l.outline.set_color('black')
					cbar_l.outline.set_linewidth(1)
					cbar_l.dividers.set_color('black')
					cbar_l.dividers.set_linewidth(1)
	
					plt.subplot(122)
					plt.contourf(x_sr*0.54,y_sr*0.54,wind_2km*1.94,levs_wind,cmap=colormap_wind,norm=norm_wind)
					plt.xlim(-132,132)
					plt.ylim(-132,132)
					plt.xticks(np.linspace(-100,100,5),fontsize=14)
					plt.yticks(np.linspace(-100,100,5),fontsize=14)
					plt.gca().set_aspect('equal', adjustable='box')
					plt.grid()
					plt.xticks(np.linspace(-100,100,5),fontsize=14)
					plt.yticks(np.linspace(-100,100,5),fontsize=14)
					plt.xlabel('East-West Distance (n mi)',fontsize=18)
					plt.ylabel('North-South Distance (n mi)',fontsize=18)
					plt.title(EXPT.strip()+'\n'+'2-km Wind (kt, Shading)'+'\n'+'2-km (Black) and 5-km (Gray) Streamlines'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
					plt.text(-127,127,'2-km Max'+r'$\bf\overline{V}_{t}$'+' (RMW):\n'+vmaxstring+' kt ('+rmwstring+' n mi)',fontsize=14,verticalalignment='top', horizontalalignment='left',color='k',weight = 'bold',bbox=dict(facecolor='white', edgecolor='black'))
					plt.text(127,127,'Shear:\n'+shearstring+' kt',fontsize=14,verticalalignment='top', horizontalalignment='right',color='blue',weight = 'bold',bbox=dict(facecolor='white', edgecolor='black'))
					ticks=[7, 16, 25, 34, 40, 46, 52, 58, 64, 80, 96, 110, 125, 140, 155]
					plt.gca().streamplot(x_sr_2*0.54,y_sr_2*0.54,u2km*1.94,v2km*1.94,density=3,color='k',linewidth=2,arrowstyle='->',arrowsize=2)
					plt.gca().streamplot(x_sr_2*0.54,y_sr_2*0.54,u5km*1.94,v5km*1.94,density=3,color='0.5',linewidth=2,arrowstyle='->',arrowsize=2)
					plt.gca().arrow(0,0,ushear1*3.5*1.94,vshear1*3.5*1.94, width=2, head_width=10, head_length=10, fc='blue', ec='black',zorder=10)
					ax = plt.gca()
					divider = make_axes_locatable(ax)
					cax = divider.append_axes("right", size="5%", pad=0.05)
					cbar_r = plt.colorbar(cax=cax,ticks=ticks,norm=norm_wind,drawedges=True)
					cbar_r.set_ticklabels([7, 16, 25, 34, 40, 46, 52, 58, 64, 80, 96, 110, 125, 140, 155])
					cbar_r.ax.tick_params(labelsize=14)
					#cbar_r.outline.set_color('black')
					cbar_r.outline.set_linewidth(1)
					cbar_r.dividers.set_color('black')
					cbar_r.dividers.set_linewidth(1)
	
					plt.subplots_adjust(wspace=.25)
					figfname = ODIR+'/'+LONGSID.lower()+'.dbz_2km_wind_5km_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')
					plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					plt.close()
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
					
					#Make Plot of Precipitation Type
					plt.figure(figsize=(20.5,12))
					plt.subplot(121)
					plt.contourf(x_sr*0.54,y_sr*0.54,dbz[:,:,4],levs_dbz,cmap=colormap_dbz,norm=norm_dbz)
					plt.xlim(-132,132)
					plt.ylim(-132,132)
					plt.xticks(np.linspace(-100,100,5),fontsize=14)
					plt.yticks(np.linspace(-100,100,5),fontsize=14)
					plt.gca().set_aspect('equal', adjustable='box')
					plt.grid()
					plt.xlabel('East-West Distance (n mi)',fontsize=18)
					plt.ylabel('North-South Distance (n mi)',fontsize=18)
					plt.title(EXPT.strip()+'\n'+'2-km Precipitation Type'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
					ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80]
					ax = plt.gca()
					divider = make_axes_locatable(ax)
					cax = divider.append_axes("bottom", size="5%", pad=1.0)
					cbar_l = plt.colorbar(cax=cax,ticks=ticks,norm=norm_dbz,drawedges=True,orientation='horizontal')
					cbar_l.set_ticklabels([0, 10, 20, 30, 40, 50, 60, 70, 80])
					cbar_l.ax.tick_params(labelsize=18)
					cbar_l.outline.set_linewidth(1)
					cbar_l.dividers.set_color('black')
					cbar_l.dividers.set_linewidth(1)
	
					plt.subplot(122)
					plt.contourf(x_sr*0.54,y_sr*0.54,ptype[:,:],[0,1,2,3,4,5],colors=['xkcd:white','xkcd:green','xkcd:yellow','xkcd:orange','xkcd:red'])#,extendfrac='auto')
					plt.xlim(-132,132)
					plt.ylim(-132,132)
					plt.xticks(np.linspace(-100,100,5),fontsize=14)
					plt.yticks(np.linspace(-100,100,5),fontsize=14)
					plt.gca().set_aspect('equal', adjustable='box')
					plt.grid()
					plt.xlabel('East-West Distance (n mi)',fontsize=18)
					plt.ylabel('North-South Distance (n mi)',fontsize=18)
					plt.title(EXPT.strip()+'\n'+'2-km Reflectivity (dBZ)'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
					ax = plt.gca()
					divider = make_axes_locatable(ax)
					cax = divider.append_axes("bottom", size="5%", pad=1.0)
					cbar_l = plt.colorbar(cax=cax,ticks=[0.5,1.5,2.5,3.5,4.5],drawedges=True,orientation='horizontal')
					cbar_l.set_ticklabels(['None','Stratiform', 'Shallow', 'Moderate', 'Deep'])
					cbar_l.ax.tick_params(labelsize=18)
					
					plt.subplots_adjust(wspace=.25)
					figfname = ODIR+'/'+LONGSID.lower()+'.2km_reflectivity_and_precip_type_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')
					plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					plt.close()
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
						
					#Make Azimuthal Mean Tangential Wind Plot With Smaller x-axis
					plt.figure()
					plt.gcf().set_size_inches(20.5, 10.5)
					plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(vt_p_mean,1)),levs_vt,cmap=colormap_vt,norm=norm_vt,extend='max')
					plt.grid()
					plt.xlim(0,150)
					plt.ylim(0,18)
					cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80])
					cbar.ax.tick_params(labelsize=24)
					plt.xticks(np.linspace(0,150,11),fontsize=24)
					plt.yticks(np.linspace(0,18,10),fontsize=24)
					plt.xlabel('Radius (km)',fontsize=24)
					plt.ylabel('Height (km)',fontsize=24)
					plt.title(EXPT.strip()+'\n'+ r'Azimuthal Mean Tangential Wind ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
					plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
					#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.vt_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
					figfname = ODIR+'/'+LONGSID.lower()+'.vt_mean_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')
					plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					plt.close()
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");
					
					#Make Azimuthal Mean Radial Wind Plot With Smaller x-axis
					plt.figure()
					plt.gcf().set_size_inches(20.5, 10.5)
					plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(ur_p_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
					plt.grid()
					plt.xlim(0,150)
					plt.ylim(0,18)
					cbar = plt.colorbar(ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
					cbar.ax.tick_params(labelsize=24)
					plt.xticks(np.linspace(0,150,11),fontsize=24)
					plt.yticks(np.linspace(0,18,10),fontsize=24)
					plt.xlabel('Radius (km)',fontsize=24)
					plt.ylabel('Height (km)',fontsize=24)
					plt.title(EXPT.strip()+'\n'+ r'Azimuthal Mean Radial Wind ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=24, weight = 'bold',loc='left')
					plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
					#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.ur_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
					figfname = ODIR+'/'+LONGSID.lower()+'.ur_mean_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')
					plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					plt.close()
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
						
					#Make Azimuthal Mean Reflectivity Plot With Smaller x-axis
					plt.figure()
					plt.gcf().set_size_inches(20.5, 10.5)
					plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(dbz_p_mean,1)),levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
					plt.grid()
					plt.xlim(0,150)
					plt.ylim(0,18)
					cbar = plt.colorbar(ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
					cbar.ax.tick_params(labelsize=24)
					plt.xticks(np.linspace(0,150,11),fontsize=24)
					plt.yticks(np.linspace(0,18,10),fontsize=24)
					plt.xlabel('Radius (km)',fontsize=24)
					plt.ylabel('Height (km)',fontsize=24)
					plt.title(EXPT.strip()+'\n'+ r'Azimuthal Mean Reflectivity ($dBZ$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
					plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
					#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.dbz_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
					figfname = ODIR+'/'+LONGSID.lower()+'.dbz_mean_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')
					plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					plt.close()
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

				# Close the GrADs control file
				ga('close 1')
	

				#Make Shear/RH Combo Plot With Vortex-Removed Shear, If Flag is Set on
				if (do_shear_and_rh_plots == 'Y'):
					shearandrhplot.shearandrhplot(XI, YI, theta, r, ushear_p, vshear_p, np.nanmean(rh_p[:,:,6:10],2), '3km', '5km', rmw_mean[6], rmw_mean[10], GPLOT_DIR, \
								      EXPT, FHR, maxwind, minpressure, LONGSID, ODIR, forecastinit, DO_CONVERTGIF)

				finish = time.perf_counter()
				print(f'MSG: Total time for plotting: {finish-start:.2f} second(s)')

		# Write the input file to a log to mark that it has ben processed
		io.update_plottedfile(f'{ODIR}/PlottedFiles.{DOMAIN.strip()}.{TIER.strip()}.{SID.strip()}.log', FILE)

	
	print('MSG: DOING THE EXTRA STUFF')
	combinedfile = f'{ODIR}/{LONGSID.lower()}.structure_statistics.{forecastinit}.polar.all.txt'
	pastecmd = 'paste -sd"\\n" '+ODIR+'/'+LONGSID.lower()+'.structure_statistics.'+forecastinit+'.polar.f*.txt'+' > '+combinedfile
	print(f'MSG: pastecmd = {pastecmd}')
	os.system(pastecmd)
	pythonexec = sys.executable
	runcmd = f'{pythonexec} {PYTHONDIR}/plot_structure_metrics.py {combinedfile} {EXPT.strip()} {ODIR} {forecastinit} {LONGSID}'
	print(f'MSG: runcmd = {runcmd}')
	subprocess.call(runcmd,shell=True)
	
	print('MSG: COMPLETING')
	os.system(f'lockfile -r-1 -l 180 {ST_LOCK_FILE}')
	os.system(f'echo "complete" > {STATUS_FILE}')
	os.system(f'rm -f {ST_LOCK_FILE}')

	# Log some important information
	print(f'MSG: make_rz_plots_heightcoordinates_research.py completed at {datetime.datetime.now()}')



##############################
def wait_random(lev):
	rand = np.random.randint(2,10)
	print(f'MSG: Waiting {rand} seconds for lev={lev}.')
	time.sleep(rand)
	return rand, lev


##############################
if __name__ == '__main__':
	main()
