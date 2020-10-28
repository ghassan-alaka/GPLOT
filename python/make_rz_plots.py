#!/usr/bin/env python

# Check that GPLOT_DIR is defined in the environment.
import os, time
GPLOT_DIR = os.environ['GPLOT_DIR']
print('MSG: Found this GPLOT location --> '+GPLOT_DIR)

#Import necessary modules
print('Importing Everything Needed')
from py3grads import Grads #This is how we'll get the data
import numpy as np #Used for a lot of the calculations
import matplotlib #The plotting routines
matplotlib.use('Agg')
import matplotlib.pyplot as plt #Command for the plotting
import matplotlib.colors as colors #Command to do some colorbar stuff
import scipy #Used for interpolation to polar coordinates
from scipy import interpolate #The interpolation function
from matplotlib.ticker import ScalarFormatter #Used to change the log-y-axis ticks
import sys #To change the path 
sys.path.append(GPLOT_DIR+'/python/modules')
import centroid
import glob
import cmath
import subprocess
from mpl_toolkits.axes_grid1 import make_axes_locatable

#Define Pygrads interface
ga = Grads(verbose=False)

#Get command lines arguments
if len(sys.argv) < 11:
	print("ERROR: Expected 11 command line arguments. Got "+str(len(sys.argv)))
	sys.exit()
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
RESOLUTION = sys.argv[7]
if RESOLUTION == 'MISSING':
	RESOLUTION = ''
RMAX = sys.argv[8]
if RMAX == 'MISSING':
	RMAX = ''
LEVS = sys.argv[9]
if LEVS == 'MISSING':
	LEVS = ''
NMLIST = sys.argv[10]
if NMLIST == 'MISSING':
	print("ERROR: Master Namelist can't be MISSING.")
	sys.exit()
MASTER_NML_IN = NMLIST

# Read the master namelist
DSOURCE = subprocess.run(['grep','^DSOURCE',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1]
EXPT = subprocess.run(['grep','^EXPT',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1]
ODIR = subprocess.run(['grep','^ODIR',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1].strip()+'/polar/'

try:
	DO_CONVERTGIF = subprocess.run(['grep','^DO_CONVERTGIF',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1].strip();
	DO_CONVERTGIF = (DO_CONVERTGIF == 'True');
except:
	DO_CONVERTGIF = False;

figext = '.png';

# Create the temporary directory for GrADs files
TMPDIR = ODIR.strip()+'grads/'
if not os.path.exists(TMPDIR):
	os.mkdir(TMPDIR)

# Define some important file names
UNPLOTTED_FILE = ODIR.strip()+'UnplottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
PLOTTED_FILE = ODIR.strip()+'PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
ALLFHR_FILE = ODIR.strip()+'AllForecastHours.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
STATUS_FILE = ODIR.strip()+'status.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
ST_LOCK_FILE =  ODIR.strip()+'status.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log.lock'
ATCF_FILE = ODIR.strip()+'ATCF_FILES.dat'


#Get parameters from input file
resolution = np.float(RESOLUTION)
rmax = np.float(RMAX)
zsize = np.int(LEVS)

# Get the ATCF file.
ATCF_LIST = np.genfromtxt(ODIR+'ATCF_FILES.dat',dtype='str')
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
X_G2CTL = GPLOT_DIR+'/grads/g2ctl.pl'

for (FILE,fff) in zip(UNPLOTTED_LIST,np.array(range(UNPLOTTED_LIST.size))):

	if (FILE == 'MISSING'):  continue

	print('MSG: Working on this file --> '+str(FILE)+'  '+str(fff))

	os.system('lockfile -r-1 -l 180 '+ST_LOCK_FILE)
	os.system('echo "working" > '+STATUS_FILE)
	os.system('rm -f '+ST_LOCK_FILE)

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
	if (lonstr2 == 'W'):
		centerlon = 360-float(lonstr1)/10
	else:
		centerlon = float(lonstr1)/10
	latstr = ATCF_DATA[list(FHRIND),6][0]
	latstr1 = latstr[::-1]
	latstr1 = latstr1[1:]
	latstr1 = latstr1[::-1]
	latstr2 = latstr[::-1]
	latstr2 = latstr2[0]
	if (latstr2 == 'N'):
		centerlat = float(latstr1)/10
	else:
		centerlat = -1*float(latstr1)/10
	forecastinit = ATCF_DATA[list(FHRIND),2][0]
	maxwind = ATCF_DATA[list(FHRIND),8][0]
	minpressure = ATCF_DATA[list(FHRIND),9][0]

	if ( centerlat > 50.0):
		# Write the input file to a log to mark that it has ben processed
		PLOTTED_FILES=ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
		os.system('echo "'+np.str(FILE)+'" >> '+PLOTTED_FILES)
		os.system('sort -u '+PLOTTED_FILES+' > '+PLOTTED_FILES+'.TMP')
		os.system('mv '+PLOTTED_FILES+'.TMP '+PLOTTED_FILES)
		break

	figuretest = np.shape([g for g in glob.glob(f"{ODIR}/*{TCNAME.lower()}*{format(FHR,'03d')}{figext}")])[0]
	if (figuretest < 1):
		print('None of These Yet!')
		print(figuretest)
		print('h = ',list(FHRIND))

		# Check that the data file 'FILE' exists
		gribfiletest = os.system('ls '+FILE)

		if (gribfiletest < 1):

			# Create the GrADs control file, if is hasn't already been created.
			CTL_FILE = TMPDIR+FILE_BASE+'.ctl'
			IDX_FILE = TMPDIR+FILE_BASE+'.2.idx'
			LOCK_FILE = TMPDIR+FILE_BASE+'.lock'
			while os.path.exists(LOCK_FILE):
				print('MSG: '+TMPDIR+FILE_BASE+' is locked. Sleeping for 5 seconds.')
				time.sleep(5)
				LOCK_TEST = os.popen('find '+LOCK_FILE+' -mmin +3 2>/dev/null').read()
				if LOCK_TEST:
					os.system('rm -f '+LOCK_FILE)

			if not os.path.exists(CTL_FILE) or os.stat(CTL_FILE).st_size == 0:
				print('MSG: GrADs control file not found. Creating it now.')
				os.system('lockfile -r-1 -l 180 '+LOCK_FILE)
				command = X_G2CTL+' '+FILE+' '+IDX_FILE+' > '+CTL_FILE
				os.system(command)
				command2 = 'gribmap -i '+CTL_FILE+' -big'
				os.system(command2)
				os.system('rm -f '+LOCK_FILE)

			while not os.path.exists(IDX_FILE):
				print('MSG: GrADs index file not found. Sleeping for 5 seconds.')
				time.sleep(5)
			
			# Open GrADs data file
			print('MSG: GrADs control and index files should be available.')
			ga('open '+CTL_FILE)
			env = ga.env()

			#Define how big of a box you want, based on lat distance
			yoffset = 7
			test7 = np.cos((centerlat+7)*3.14159/180)*111.1*7
			test8 = np.cos((centerlat+8)*3.14159/180)*111.1*8
			test9 = np.cos((centerlat+9)*3.14159/180)*111.1*9
			test10 = np.cos((centerlat+10)*3.14159/180)*111.1*10
			test11 = np.cos((centerlat+11)*3.14159/180)*111.1*11
			test12 = np.cos((centerlat+12)*3.14159/180)*111.1*12

			if (test7 > rmax):
				xoffset = 7
			elif (test8 > rmax):
				xoffset = 8
			elif (test9 > rmax):
				xoffset = 9
			elif (test10 > rmax):
				xoffset = 10
			elif (test11 > rmax):
				xoffset = 11
			elif (test12 > rmax):
				xoffset = 12
			else:
				print('YOU NEED A BIGGER BOX THAN 12 DEGREES')


			#Get lat, lon, levs
			ga('set z 1')
			lonmax = centerlon + xoffset
			latmax = centerlat + yoffset
			lonmin = centerlon - xoffset
			latmin = centerlat - yoffset
			lonstring = 'set lon '+np.str(lonmin)+' '+np.str(lonmax)
			latstring = 'set lat '+np.str(latmin)+' '+np.str(latmax)
			ga(lonstring)
			ga(latstring)
			lats = ga.exp('lat')
			lons = ga.exp('lon')
			zstring = 'set z 1 '+np.str(zsize)
			ga(zstring)
			levs = ga.exp('lev')
			zlevs = levs[0,0,:]

		
			#Make them all a 1-d array
			lon = lons[0,:]
			lat = lats[:,0]
			z = np.zeros((zsize,0))*np.nan
			for i in range(zsize):
				#print(i)
				z[i] = levs[1,1,i]

			#Get data
			print('Getting Data Now. Using an xoffset of '+np.str(xoffset)+' degrees')
			uwind = ga.exp('ugrdprs')
			vwind = ga.exp('vgrdprs')
			omega = ga.exp('vvelprs')
			print('Done With u,v,w')
			dbz = ga.exp('refdprs')
			hgt = ga.exp('hgtprs')
			temp = ga.exp('tmpprs')
			print('Done with dbz, hgt, temp')
			q = ga.exp('spfhprs')
			rh = ga.exp('rhprs')
			print('Done with q, rh')
			u10 = ga.exp('ugrd10m')
			v10 = ga.exp('vgrd10m')
			print('Done with u10,v10')
			
			#If height is below the ground, make data nan
			mask = hgt < 0
			uwind[mask] = np.nan
			vwind[mask] = np.nan
			omega[mask] = np.nan
			dbz[mask] = np.nan
			hgt[mask] = np.nan
			temp[mask] = np.nan
			q[mask] = np.nan
			rh[mask] = np.nan

			#Get W from Omega
			#w = -omega/(rho*g)
			#rho = p/(Rd*Tv)
			mixr = q/(1-q)
			temp_v = temp*(1+0.61*mixr)
			rho = (levs*1e2)/(287*temp_v)
			wwind = -omega/(rho*9.81)

			#Get storm-centered data
			lon_sr = lon-centerlon
			lat_sr = lat-centerlat
			x_sr = lon_sr*111.1e3*np.cos(centerlat*3.14159/180)
			y_sr = lat_sr*111.1e3

			#Define the polar coordinates needed
			r = np.linspace(0,rmax,np.int(rmax/resolution)+1)
			pi = np.arccos(-1)
			theta = np.arange(0,2*pi+pi/36,pi/36)
			R, THETA = np.meshgrid(r, theta)
			XI = R * np.cos(THETA)
			YI = R * np.sin(THETA)

			x_sr = np.round(x_sr/1000,3)
			y_sr = np.round(y_sr/1000,3)

			x_sr_2 = np.linspace(x_sr.min(), x_sr.max(), x_sr.size)
			y_sr_2 = np.linspace(y_sr.min(), y_sr.max(), y_sr.size)

			#Do interpolation
			print('Doing the Interpolation Now')

			#First initialize u_p and v_p
			u_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			v_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			w_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			dbz_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			hgt_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			temp_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			q_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			rh_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan

			for k in range(zsize):
				f_uwind = interpolate.RegularGridInterpolator((y_sr, x_sr), uwind[:,:,k])
				f_vwind = interpolate.RegularGridInterpolator((y_sr, x_sr), vwind[:,:,k])
				f_wwind = interpolate.RegularGridInterpolator((y_sr, x_sr), wwind[:,:,k])
				f_dbz = interpolate.RegularGridInterpolator((y_sr, x_sr), dbz[:,:,k])
				f_hgt = interpolate.RegularGridInterpolator((y_sr, x_sr), hgt[:,:,k])
				f_temp = interpolate.RegularGridInterpolator((y_sr, x_sr), temp[:,:,k])
				f_q = interpolate.RegularGridInterpolator((y_sr, x_sr), q[:,:,k])
				f_rh = interpolate.RegularGridInterpolator((y_sr, x_sr), rh[:,:,k])
			
				u_p[:,:,k] = f_uwind((YI,XI),method='linear')
				v_p[:,:,k] = f_vwind((YI,XI),method='linear')
				w_p[:,:,k] = f_wwind((YI,XI),method='linear')
				dbz_p[:,:,k] = f_dbz((YI,XI),method='linear')
				hgt_p[:,:,k] = f_hgt((YI,XI),method='linear')
				temp_p[:,:,k] = f_temp((YI,XI),method='linear')
				q_p[:,:,k] = f_q((YI,XI),method='linear')
				rh_p[:,:,k] = f_rh((YI,XI),method='linear')

			#Calculate tangential and radial wind
			vt_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			ur_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			for j in range(np.shape(XI)[1]):
				for k in range(zsize):
						vt_p[:,j,k] = -u_p[:,j,k]*np.sin(theta)+v_p[:,j,k]*np.cos(theta)
						ur_p[:,j,k] = u_p[:,j,k]*np.cos(theta)+v_p[:,j,k]*np.sin(theta)

			#Get Polar u10 and v10
			u10_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan	
			v10_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			f_u10 = interpolate.RegularGridInterpolator((y_sr, x_sr), u10[:,:,k])
			f_v10 = interpolate.RegularGridInterpolator((y_sr, x_sr), v10[:,:,k])

			u10_p[:,:] = f_u10((YI,XI),method='linear')
			v10_p[:,:] = f_v10((YI,XI),method='linear')
			
			vt10_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			ur10_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan

			for j in range(np.shape(XI)[1]):
				vt10_p[:,j] = -u10_p[:,j]*np.sin(theta)+v10_p[:,j]*np.cos(theta)
				ur10_p[:,j] = u10_p[:,j]*np.cos(theta)+v10_p[:,j]*np.sin(theta)

			#Calculate shear
			u_p_850 = u_p[:,:,6]
			v_p_850 = v_p[:,:,6]
			u_p_200 = u_p[:,:,32]
			v_p_200 = v_p[:,:,32]

			u_p_850_ring = u_p_850[:,np.int(np.round(200/resolution)):np.int(np.round(600/resolution))]
			v_p_850_ring = v_p_850[:,np.int(np.round(200/resolution)):np.int(np.round(600/resolution))]
			u_p_200_ring = u_p_200[:,np.int(np.round(200/resolution)):np.int(np.round(600/resolution))]
			v_p_200_ring = v_p_200[:,np.int(np.round(200/resolution)):np.int(np.round(600/resolution))]

			u_p_850_ring_mean = np.nanmean(np.nanmean(u_p_850_ring))
			v_p_850_ring_mean = np.nanmean(np.nanmean(v_p_850_ring))
			u_p_200_ring_mean = np.nanmean(np.nanmean(u_p_200_ring))
			v_p_200_ring_mean = np.nanmean(np.nanmean(v_p_200_ring))

			ushear = u_p_200_ring_mean-u_p_850_ring_mean
			vshear = v_p_200_ring_mean-v_p_850_ring_mean

			shearmag = np.hypot(ushear,vshear)
			sheardir = np.arctan2(vshear,ushear)*180.0/pi
			shearstring = np.str(np.int(np.round(shearmag*1.94,0)))

			#Convert shear to meteorological convention
			if sheardir <=90:
				sheardir_met = 90-sheardir
			else:
				sheardir_met = 360-(sheardir-90)

			#Also convert shear to positive value
			if sheardir <0:
				sheardir_math = sheardir+360
			else:
				sheardir_math = sheardir

			#Round shear vector to nearest 5
			sheardir_5deg = (np.round((sheardir_math/5))*5)

			#Get index for shear
			sheardir_index = np.int(sheardir_5deg/5)

			#Rotate variables
			#3D
			u_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			v_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			w_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			dbz_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			hgt_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			temp_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			q_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			rh_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			vt_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			ur_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan

			#2D
			u10_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			v10_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			vt10_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			ur10_p_rot = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			
			#3D			
			u_p_rot = np.roll(u_p,[-sheardir_index, 0, 0],axis=(0,1,2))
			v_p_rot = np.roll(v_p,[-sheardir_index, 0, 0],axis=(0,1,2))
			w_p_rot = np.roll(w_p,[-sheardir_index, 0, 0],axis=(0,1,2))
			dbz_p_rot = np.roll(dbz_p,[-sheardir_index, 0, 0],axis=(0,1,2))
			hgt_p_rot = np.roll(hgt_p,[-sheardir_index, 0, 0],axis=(0,1,2))
			temp_p_rot = np.roll(temp_p,[-sheardir_index, 0, 0],axis=(0,1,2))
			q_p_rot = np.roll(q_p,[-sheardir_index, 0, 0],axis=(0,1,2))
			rh_p_rot = np.roll(rh_p,[-sheardir_index, 0, 0],axis=(0,1,2))
			vt_p_rot = np.roll(vt_p,[-sheardir_index, 0, 0],axis=(0,1,2))
			ur_p_rot = np.roll(ur_p,[-sheardir_index, 0, 0],axis=(0,1,2))

			#2D
			u10_p_rot = np.roll(u10_p,[-sheardir_index, 0],axis=(0,1))
			v10_p_rot = np.roll(v10_p,[-sheardir_index, 0],axis=(0,1))
			vt10_p_rot = np.roll(vt10_p,[-sheardir_index, 0],axis=(0,1))
			ur10_p_rot = np.roll(ur10_p,[-sheardir_index, 0],axis=(0,1))

			#Calculate azimuthal means
			vt_p_mean = np.nanmean(vt_p,0)
			ur_p_mean = np.nanmean(ur_p,0)
			w_p_mean = np.nanmean(w_p,0)
			dbz_p_mean = np.nanmean(dbz_p,0)
			temp_p_mean = np.nanmean(temp_p,0)
			q_p_mean = np.nanmean(q_p,0)
			rh_p_mean = np.nanmean(rh_p,0)

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
			dbz_p_w0 = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			dbz_p_w1 = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			dbz_p_w2 = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
	
			for j in range(np.shape(r)[0]):
				for k in range(zsize):
					dbzdata = dbz_p[:,j,k]
					fourier_dbz = np.fft.fft(dbzdata)/len(dbzdata)
					amp0_dbz = np.real(fourier_dbz[0])
					phase0_dbz = cmath.polar(fourier_dbz[0])[1]
					A1_dbz = 2*np.real(fourier_dbz[1])
					B1_dbz = -2*np.imag(fourier_dbz[1])
					A2_dbz = 2*np.real(fourier_dbz[2])
					B2_dbz = -2*np.imag(fourier_dbz[2])
					dbz_p_w0[:,j,k] = amp0_dbz
					dbz_p_w1[:,j,k] = A1_dbz*np.cos(theta) + B1_dbz*np.sin(theta)
					dbz_p_w2[:,j,k] = A2_dbz*np.cos(2*theta) + B2_dbz*np.sin(2*theta)

			vt10_p_w0 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			vt10_p_w1 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			vt10_p_w2 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			ur10_p_w0 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			ur10_p_w1 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			ur10_p_w2 = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan			

			rh_p_w0 = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			rh_p_w1 = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			rh_p_w2 = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			
			for j in range(np.shape(r)[0]):
				for k in range(zsize):
					rhdata = rh_p[:,j,k]
					fourier_rh = np.fft.fft(rhdata)/len(rhdata)
					amp0_rh = np.real(fourier_rh[0])
					A1_rh = 2*np.real(fourier_rh[1])
					B1_rh = -2*np.imag(fourier_rh[1])
					A2_rh = 2*np.real(fourier_rh[2])
					B2_rh = -2*np.imag(fourier_rh[2])
					rh_p_w0[:,j,k] = amp0_rh
					rh_p_w1[:,j,k] = A1_rh*np.cos(theta) + B1_rh*np.sin(theta)
					rh_p_w2[:,j,k] = A2_rh*np.cos(2*theta) + B2_rh*np.sin(2*theta)

			for j in range(np.shape(r)[0]):
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
		
			#Calculate the new centers at each height using the centroid function
			threshold = np.ones(zsize)*np.nan
			for k in range(zsize):
				threshold[k] = np.nanmin(hgt[:,:,k])+0.2*(np.nanmax(hgt[:,:,k])-np.nanmin(hgt[:,:,k]))

			wind = np.hypot(uwind,vwind)
			threshold2 = np.ones(zsize)*np.nan
			for k in range(zsize):
				threshold2[k] = np.nanmax(wind[:,:,k])-0.05*(np.nanmax(wind[:,:,k])-np.nanmin(wind[:,:,k]))
			center_z = np.ones((np.shape(hgt)[2],2),order='F').astype(np.int32)
			center_z_2 = np.ones((np.shape(wind)[2],2),order='F').astype(np.int32)
			centroid.centroid(hgt,center_z,threshold,-1,np.shape(hgt)[0],np.shape(hgt)[1],np.shape(hgt)[2])
			centroid.centroid(hgt,center_z_2,threshold,-1,np.shape(hgt)[0],np.shape(hgt)[1],np.shape(hgt)[2])


			#Calculate 750-hPa RMW and Average Vmax
			rmw_mean_index = np.argmax(vt_p_mean,0)
			rmw_mean = r[rmw_mean_index]
			vt_p_mean_max = np.max(vt_p_mean,0)
			rmw_750 = rmw_mean[10]
			vt_p_mean_max_750 = vt_p_mean_max[10]
			rmwstring = np.str(np.int(np.round(rmw_750*0.54,0)))
			vmaxstring = np.str(np.int(np.round(vt_p_mean_max_750*1.94,0)))

			u750 = uwind[:,:,10]
			v750 = vwind[:,:,10]
			u450 = uwind[:,:,22]
			v450 = vwind[:,:,22]
			wind_750 = np.hypot(uwind[:,:,10],vwind[:,:,10])
			dbz_750 = dbz[:,:,10]

			#Make Plots
			print('Doing Plots Now')

			#Load the colormaps needed
			color_data_vt = np.genfromtxt(GPLOT_DIR+'/python/colormaps/colormap_wind.txt')
			colormap_vt = matplotlib.colors.ListedColormap(color_data_vt)
			levs_vt = np.linspace(0,80,41,endpoint=True)
			norm_vt = colors.BoundaryNorm(levs_vt,256)

			color_data_ur = np.genfromtxt(GPLOT_DIR+'/python/colormaps/bluewhitered.txt')
			colormap_ur = matplotlib.colors.ListedColormap(color_data_ur)
			levs_ur = np.linspace(-30,30,31,endpoint=True)
			norm_ur = colors.BoundaryNorm(levs_ur,256)

			color_data_w = np.genfromtxt(GPLOT_DIR+'/python/colormaps/bluewhitered.txt')
			colormap_w = matplotlib.colors.ListedColormap(color_data_w)
			levs_w = np.linspace(-5,5,41,endpoint=True)
			norm_w = colors.BoundaryNorm(levs_w,256)

			color_data_dbz = np.genfromtxt(GPLOT_DIR+'/python/colormaps/colormap_radar.txt')
			colormap_dbz = matplotlib.colors.ListedColormap(color_data_dbz)
			levs_dbz = np.linspace(0,80,41,endpoint=True)
			norm_dbz = colors.BoundaryNorm(levs_dbz,256)

			color_data_rh = np.genfromtxt(GPLOT_DIR+'/python/colormaps/colormap_brown_to_green.txt')
			colormap_rh = matplotlib.colors.ListedColormap(color_data_rh)
			levs_rh = np.linspace(0,100,41,endpoint=True)
			norm_rh = colors.BoundaryNorm(levs_rh,256)


			color_data_wind = np.genfromtxt(GPLOT_DIR+'/python/colormaps/colormap_wind.txt')
			colormap_wind = matplotlib.colors.ListedColormap(color_data_wind)
			levs_wind = [0,7,10,13,16,19,22,25,28,31,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,69.333,74.666,80,85.333,90.666,96,100.666,105.333,110,115,120,125,130,132,140,145,150,155,160]
			norm_wind = colors.BoundaryNorm(levs_wind,256)

			#First do Azimuthal Mean Radial Wind
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,zlevs,np.flipud(np.rot90(ur_p_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
			plt.yscale('log')
			plt.grid()
			plt.xlim(0,rmax)
			plt.ylim(100,1000)
			plt.gca().invert_yaxis()
			cbar = plt.colorbar(ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
			cbar.ax.tick_params(labelsize=24)
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,rmax,11),fontsize=24)
			plt.yticks(np.linspace(1000,100,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Pressure Level (hPa)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+ r'Azimuthal Mean Radial Wind ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.ur_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.ur_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")

			#Next do azimuthal mean tangential wind
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,zlevs,np.flipud(np.rot90(vt_p_mean,1)),levs_vt,cmap=colormap_vt,norm=norm_vt,extend='max')
			plt.yscale('log')
			plt.grid()
			plt.xlim(0,rmax)
			plt.ylim(100,1000)
			plt.gca().invert_yaxis()
			cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80])
			cbar.ax.tick_params(labelsize=24)
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,rmax,11),fontsize=24)
			plt.yticks(np.linspace(1000,100,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Pressure Level (hPa)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+ r'Azimuthal Mean Tangential Wind ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.vt_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.vt_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

			#Next do azimuthal mean vertical velocity
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,zlevs,np.flipud(np.rot90(w_p_mean,1)),levs_w,cmap=colormap_w,norm=norm_w,extend='both')
			plt.yscale('log')
			plt.grid()
			plt.xlim(0,rmax)
			plt.ylim(100,1000)
			plt.gca().invert_yaxis()
			cbar = plt.colorbar(ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
			cbar.ax.tick_params(labelsize=24)
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,rmax,11),fontsize=24)
			plt.yticks(np.linspace(1000,100,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Pressure Level (hPa)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+ r'Azimuthal Mean W ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.w_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.w_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

			#Next do azimuthal mean reflectivity
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,zlevs,np.flipud(np.rot90(dbz_p_mean,1)),levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.yscale('log')
			plt.grid()
			plt.xlim(0,rmax)
			plt.ylim(100,1000)
			plt.gca().invert_yaxis()
			cbar = plt.colorbar(ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
			cbar.ax.tick_params(labelsize=24)
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,rmax,11),fontsize=24)
			plt.yticks(np.linspace(1000,100,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Pressure Level (hPa)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+ r'Azimuthal Mean Reflectivity ($dBZ$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.dbz_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.dbz_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

			#Next do azimuthal mean RH
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,zlevs,np.flipud(np.rot90(rh_p_mean,1)),levs_rh,cmap=colormap_rh,norm=norm_rh,extend='max')
			plt.yscale('log')
			plt.grid()
			plt.xlim(0,rmax)
			plt.ylim(100,1000)
			plt.gca().invert_yaxis()
			cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
			cbar.ax.tick_params(labelsize=24)
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,rmax,11),fontsize=24)
			plt.yticks(np.linspace(1000,100,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Pressure Level (hPa)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+ r'Azimuthal Mean Relative Humidity ($\%$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.rh_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.rh_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

			#Next do along-shear reflectivity
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,zlevs,np.flipud(np.rot90(dbz_p_downshear_mean,1)),levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.contourf(-r,zlevs,np.flipud(np.rot90(dbz_p_upshear_mean,1)),levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.yscale('log')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(100,1000)
			plt.gca().invert_yaxis()
			cbar = plt.colorbar(ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
			cbar.ax.tick_params(labelsize=24)
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(1000,100,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Pressure Level (hPa)',fontsize=24)
			plt.text(-rmax+50,150,'Upshear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,150,'Downshear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
			plt.title(EXPT.strip()+'\n'+ r'Along-Shear Reflectivity ($dBZ$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.dbz_alongshear.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.dbz_alongshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

			#Next do along-shear radial wind
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,zlevs,np.flipud(np.rot90(ur_p_downshear_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
			plt.contourf(-r,zlevs,np.flipud(np.rot90(ur_p_upshear_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
			plt.yscale('log')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(100,1000)
			plt.gca().invert_yaxis()
			cbar = plt.colorbar(ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
			cbar.ax.tick_params(labelsize=24)
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(1000,100,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Pressure Level (hPa)',fontsize=24)
			plt.text(-rmax+50,150,'Upshear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,150,'Downshear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
			plt.title(EXPT.strip()+'\n'+ r'Along-Shear Radial Wind ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.ur_alongshear.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.ur_alongshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

			#Next do along-shear vertical velocity
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,zlevs,np.flipud(np.rot90(w_p_downshear_mean,1)),levs_w,cmap=colormap_w,norm=norm_w,extend='both')
			plt.contourf(-r,zlevs,np.flipud(np.rot90(w_p_upshear_mean,1)),levs_w,cmap=colormap_w,norm=norm_w,extend='both')
			plt.yscale('log')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(100,1000)
			plt.gca().invert_yaxis()
			cbar = plt.colorbar(ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
			cbar.ax.tick_params(labelsize=24)
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(1000,100,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Pressure Level (hPa)',fontsize=24)
			plt.text(-rmax+50,150,'Upshear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,150,'Downshear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
			plt.title(EXPT.strip()+'\n'+ r'Along-Shear W ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.w_alongshear.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.w_alongshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

			#Next do along-shear RH
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,zlevs,np.flipud(np.rot90(rh_p_downshear_mean,1)),levs_rh,cmap=colormap_rh,norm=norm_rh,extend='both')
			plt.contourf(-r,zlevs,np.flipud(np.rot90(rh_p_upshear_mean,1)),levs_rh,cmap=colormap_rh,norm=norm_rh,extend='both')
			plt.yscale('log')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(100,1000)
			plt.gca().invert_yaxis()
			cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
			cbar.ax.tick_params(labelsize=24)
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(1000,100,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Pressure Level (hPa)',fontsize=24)
			plt.text(-rmax+50,150,'Upshear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,150,'Downshear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
			plt.title(EXPT.strip()+'\n'+ r'Along-Shear RH ($\%$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.rh_alongshear.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.rh_alongshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

			#Next do across-shear reflectivity
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,zlevs,np.flipud(np.rot90(dbz_p_rightshear_mean,1)),levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.contourf(-r,zlevs,np.flipud(np.rot90(dbz_p_leftshear_mean,1)),levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.yscale('log')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(100,1000)
			plt.gca().invert_yaxis()
			cbar = plt.colorbar(ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
			cbar.ax.tick_params(labelsize=24)
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(1000,100,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Pressure Level (hPa)',fontsize=24)
			plt.text(-rmax+50,150,'Left of shear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,150,'Right of shear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
			plt.title(EXPT.strip()+'\n'+ r'Across-Shear Reflectivity ($dBZ$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.dbz_acrosshear.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.dbz_acrosshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

			#Next do across-shear radial wind
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,zlevs,np.flipud(np.rot90(ur_p_rightshear_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
			plt.contourf(-r,zlevs,np.flipud(np.rot90(ur_p_leftshear_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
			plt.yscale('log')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(100,1000)
			plt.gca().invert_yaxis()
			cbar = plt.colorbar(ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
			cbar.ax.tick_params(labelsize=24)
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(1000,100,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Pressure Level (hPa)',fontsize=24)
			plt.text(-rmax+50,150,'Left of shear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,150,'Right of shear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
			plt.title(EXPT.strip()+'\n'+ r'Across-Shear Radial Wind ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.ur_acrosshear.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.ur_acrosshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")

			#Next do across-shear vertical velocity
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,zlevs,np.flipud(np.rot90(w_p_rightshear_mean,1)),levs_w,cmap=colormap_w,norm=norm_w,extend='both')
			plt.contourf(-r,zlevs,np.flipud(np.rot90(w_p_leftshear_mean,1)),levs_w,cmap=colormap_w,norm=norm_w,extend='both')
			plt.yscale('log')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(100,1000)
			plt.gca().invert_yaxis()
			cbar = plt.colorbar(ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
			cbar.ax.tick_params(labelsize=24)
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(1000,100,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Pressure Level (hPa)',fontsize=24)
			plt.text(-rmax+50,150,'Left of shear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,150,'Right of shear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
			plt.title(EXPT.strip()+'\n'+ r'Across-Shear W ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.w_acrosshear.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.w_acrosshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")

			#Next do across-shear RH
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,zlevs,np.flipud(np.rot90(rh_p_rightshear_mean,1)),levs_rh,cmap=colormap_rh,norm=norm_rh,extend='both')
			plt.contourf(-r,zlevs,np.flipud(np.rot90(rh_p_leftshear_mean,1)),levs_rh,cmap=colormap_rh,norm=norm_rh,extend='both')
			plt.yscale('log')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(100,1000)
			plt.gca().invert_yaxis()
			cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
			cbar.ax.tick_params(labelsize=24)
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(1000,100,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Pressure Level (hPa)',fontsize=24)
			plt.text(-rmax+50,150,'Left of shear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,150,'Right of shear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
			plt.title(EXPT.strip()+'\n'+ r'Across-Shear RH ($\%$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.rh_acrosshear.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.rh_acrosshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")

			#Make a plot of wavenumber-0,1,2 components of 500-mb reflectivity
			plt.figure()
			plt.gcf().set_size_inches(15,15)

			plt.subplot(221)
			plt.contourf(XI,YI,dbz_p[:,:,20],levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.xlim(-rmax/2,rmax/2)
			plt.ylim(-rmax/2,rmax/2)
			plt.gca().set_aspect('equal', adjustable='box')
			cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70])
			cbar.ax.tick_params(labelsize=18)
			plt.xticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.yticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.xlabel('X (km)',fontsize=20)
			plt.ylabel('Y (km)',fontsize=20)
			plt.arrow(0,0,(ushear/25)*np.max(XI/2),(vshear/25)*np.max(YI/2), linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
			plt.gca().set_aspect('equal', adjustable='box')
			plt.grid()
			plt.title(EXPT.strip()+'\n'+ r'WV#0,1,2 500-hPa Reflectivity ($dBZ$, Shading)'+'\n'+'Shear Vector in Black'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
			plt.text(0,rmax/2-50,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

			plt.subplot(222)
			plt.contourf(XI,YI,dbz_p_w0[:,:,20],levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.xlim(-rmax/2,rmax/2)
			plt.ylim(-rmax/2,rmax/2)
			plt.gca().set_aspect('equal', adjustable='box')
			cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70])
			cbar.ax.tick_params(labelsize=18)
			plt.xticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.yticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.xlabel('X (km)',fontsize=20)
			plt.ylabel('Y (km)',fontsize=20)
			plt.arrow(0,0,(ushear/25)*np.max(XI/2),(vshear/25)*np.max(YI/2), linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
			plt.gca().set_aspect('equal', adjustable='box')
			plt.grid()
			plt.title(LONGSID.upper()+'\n'+'VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+'Shear Magnitude= '+np.str(np.int(np.round(shearmag*1.94,0)))+'kts'+'\n'+'Shear Direction= '+np.str(np.int(np.round(sheardir_met,0)))+'$^\circ$',fontsize=20,color='brown',loc='right')
			plt.text(0,rmax/2-50,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

			plt.subplot(223)
			plt.contourf(XI,YI,dbz_p_w1[:,:,20],levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.xlim(-rmax/2,rmax/2)
			plt.ylim(-rmax/2,rmax/2)
			plt.gca().set_aspect('equal', adjustable='box')
			cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70])
			cbar.ax.tick_params(labelsize=18)
			plt.xticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.yticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.xlabel('X (km)',fontsize=20)
			plt.ylabel('Y (km)',fontsize=20)
			plt.arrow(0,0,(ushear/25)*np.max(XI/2),(vshear/25)*np.max(YI/2), linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
			plt.gca().set_aspect('equal', adjustable='box')
			plt.grid()
			plt.text(0,rmax/2-50,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

			plt.subplot(224)
			plt.contourf(XI,YI,dbz_p_w2[:,:,20],levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.xlim(-rmax/2,rmax/2)
			plt.ylim(-rmax/2,rmax/2)
			plt.gca().set_aspect('equal', adjustable='box')
			cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70])
			cbar.ax.tick_params(labelsize=18)
			plt.xticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.yticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.xlabel('X (km)',fontsize=20)
			plt.ylabel('Y (km)',fontsize=20)
			plt.arrow(0,0,(ushear/25)*np.max(XI/2),(vshear/25)*np.max(YI/2), linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
			plt.gca().set_aspect('equal', adjustable='box')
			plt.grid()
			plt.text(0,rmax/2-50,'Wavenumber 2',fontsize=20,style='italic',horizontalalignment='center')

			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.dbz500_wavenumber.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.dbz500_wavenumber.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")

			plt.figure()
			plt.gcf().set_size_inches(15,15)

			plt.subplot(221)
			plt.contourf(XI,YI,rh_p[:,:,20],levs_rh,cmap=colormap_rh,norm=norm_rh,extend='both')
			plt.xlim(-rmax/2,rmax/2)
			plt.ylim(-rmax/2,rmax/2)
			plt.gca().set_aspect('equal', adjustable='box')
			cbar = plt.colorbar(ticks=[0, 20, 40, 60, 80, 100])
			cbar.ax.tick_params(labelsize=18)
			plt.xticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.yticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.xlabel('X (km)',fontsize=20)
			plt.ylabel('Y (km)',fontsize=20)
			plt.arrow(0,0,(ushear/25)*np.max(XI/2),(vshear/25)*np.max(YI/2), linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
			plt.gca().set_aspect('equal', adjustable='box')
			plt.grid()
			plt.title(EXPT.strip()+'\n'+ r'WV#0,1,2 500-hPa RH ($\%$, Shading)'+'\n'+'Shear Vector in Black'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
			plt.text(0,rmax/2-50,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

			plt.subplot(222)
			plt.contourf(XI,YI,rh_p_w0[:,:,20],levs_rh,cmap=colormap_rh,norm=norm_rh,extend='both')
			plt.xlim(-rmax/2,rmax/2)
			plt.ylim(-rmax/2,rmax/2)
			plt.gca().set_aspect('equal', adjustable='box')
			cbar = plt.colorbar(ticks=[0, 20, 40, 60, 80, 100])
			cbar.ax.tick_params(labelsize=18)
			plt.xticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.yticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.xlabel('X (km)',fontsize=20)
			plt.ylabel('Y (km)',fontsize=20)
			plt.arrow(0,0,(ushear/25)*np.max(XI/2),(vshear/25)*np.max(YI/2), linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
			plt.gca().set_aspect('equal', adjustable='box')
			plt.grid()
			plt.title(LONGSID.upper()+'\n'+'VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+'Shear Magnitude= '+np.str(np.int(np.round(shearmag*1.94,0)))+'kts'+'\n'+'Shear Direction= '+np.str(np.int(np.round(sheardir_met,0)))+'$^\circ$',fontsize=20,color='brown',loc='right')
			plt.text(0,rmax/2-50,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

			plt.subplot(223)
			plt.contourf(XI,YI,rh_p_w1[:,:,20],np.linspace(-30,30,31),cmap=colormap_rh,norm=colors.BoundaryNorm(np.linspace(-30,30,31),256),extend='both')
			plt.xlim(-rmax/2,rmax/2)
			plt.ylim(-rmax/2,rmax/2)
			plt.gca().set_aspect('equal', adjustable='box')
			cbar = plt.colorbar(ticks=[-30, -20, -10, 0, 10, 20, 30])
			cbar.ax.tick_params(labelsize=18)
			plt.xticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.yticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.xlabel('X (km)',fontsize=20)
			plt.ylabel('Y (km)',fontsize=20)
			plt.arrow(0,0,(ushear/25)*np.max(XI/2),(vshear/25)*np.max(YI/2), linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
			plt.gca().set_aspect('equal', adjustable='box')
			plt.grid()
			plt.text(0,rmax/2-50,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

			plt.subplot(224)
			plt.contourf(XI,YI,rh_p_w2[:,:,20],np.linspace(-30,30,31),cmap=colormap_rh,norm=colors.BoundaryNorm(np.linspace(-30,30,31),256),extend='both')
			plt.xlim(-rmax/2,rmax/2)
			plt.ylim(-rmax/2,rmax/2)
			plt.gca().set_aspect('equal', adjustable='box')
			cbar = plt.colorbar(ticks=[-30, -20, -10, 0, 10, 20, 30])
			cbar.ax.tick_params(labelsize=18)
			plt.xticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.yticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.xlabel('X (km)',fontsize=20)
			plt.ylabel('Y (km)',fontsize=20)
			plt.arrow(0,0,(ushear/25)*np.max(XI/2),(vshear/25)*np.max(YI/2), linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
			plt.gca().set_aspect('equal', adjustable='box')
			plt.grid()
			plt.text(0,rmax/2-50,'Wavenumber 2',fontsize=20,style='italic',horizontalalignment='center')

			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.rh500_wavenumber.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.rh500_wavenumber.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")

			plt.figure()
			plt.gcf().set_size_inches(15,15)


			plt.subplot(221)
			plt.contourf(XI,YI,vt10_p[:,:],levs_vt,cmap=colormap_vt,norm=norm_vt,extend='both')
			plt.xlim(-rmax/2,rmax/2)
			plt.ylim(-rmax/2,rmax/2)
			plt.gca().set_aspect('equal', adjustable='box')
			cbar = plt.colorbar(ticks=[0, 20, 40, 60, 80])
			cbar.ax.tick_params(labelsize=18)
			plt.xticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.yticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.xlabel('X (km)',fontsize=20)
			plt.ylabel('Y (km)',fontsize=20)
			plt.arrow(0,0,(ushear/25)*np.max(XI/2),(vshear/25)*np.max(YI/2), linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
			plt.gca().set_aspect('equal', adjustable='box')
			plt.grid()
			plt.title(EXPT.strip()+'\n'+ r'WV#0,1,2 10-m Tangential Wind ($m\ s^{-1}$, Shading)'+'\n'+'Shear Vector in Black'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
			plt.text(0,rmax/2-50,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

			plt.subplot(222)
			plt.contourf(XI,YI,vt10_p_w0[:,:],levs_vt,cmap=colormap_vt,norm=norm_vt,extend='both')
			plt.xlim(-rmax/2,rmax/2)
			plt.ylim(-rmax/2,rmax/2)
			plt.gca().set_aspect('equal', adjustable='box')
			cbar = plt.colorbar(ticks=[0, 20, 40, 60, 80, 100])
			cbar.ax.tick_params(labelsize=18)
			plt.xticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.yticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.xlabel('X (km)',fontsize=20)
			plt.ylabel('Y (km)',fontsize=20)
			plt.arrow(0,0,(ushear/25)*np.max(XI/2),(vshear/25)*np.max(YI/2), linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
			plt.gca().set_aspect('equal', adjustable='box')
			plt.grid()
			plt.title(LONGSID.upper()+'\n'+'VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+'Shear Magnitude= '+np.str(np.int(np.round(shearmag*1.94,0)))+'kts'+'\n'+'Shear Direction= '+np.str(np.int(np.round(sheardir_met,0)))+'$^\circ$',fontsize=20,color='brown',loc='right')
			plt.text(0,rmax/2-50,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

			plt.subplot(223)
			plt.contourf(XI,YI,vt10_p_w1[:,:],np.linspace(-15,15,31),cmap=colormap_ur,norm=colors.BoundaryNorm(np.linspace(-15,15,31),256),extend='both')
			plt.xlim(-rmax/2,rmax/2)
			plt.ylim(-rmax/2,rmax/2)
			plt.gca().set_aspect('equal', adjustable='box')
			cbar = plt.colorbar(ticks=[-15, -10, -5, 0, 5, 10, 15])
			cbar.ax.tick_params(labelsize=18)
			plt.xticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.yticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.xlabel('X (km)',fontsize=20)
			plt.ylabel('Y (km)',fontsize=20)
			plt.arrow(0,0,(ushear/25)*np.max(XI/2),(vshear/25)*np.max(YI/2), linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
			plt.gca().set_aspect('equal', adjustable='box')
			plt.grid()
			plt.text(0,rmax/2-50,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

			plt.subplot(224)
			plt.contourf(XI,YI,vt10_p_w2[:,:],np.linspace(-15,15,31),cmap=colormap_ur,norm=colors.BoundaryNorm(np.linspace(-15,15,31),256),extend='both')
			plt.xlim(-rmax/2,rmax/2)
			plt.ylim(-rmax/2,rmax/2)
			plt.gca().set_aspect('equal', adjustable='box')
			cbar = plt.colorbar(ticks=[-15, -10, -5, 0, 5, 10, 15])
			cbar.ax.tick_params(labelsize=18)
			plt.xticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.yticks(np.linspace(-rmax/2,rmax/2,7),fontsize=18)
			plt.xlabel('X (km)',fontsize=20)
			plt.ylabel('Y (km)',fontsize=20)
			plt.arrow(0,0,(ushear/25)*np.max(XI/2),(vshear/25)*np.max(YI/2), linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
			plt.gca().set_aspect('equal', adjustable='box')
			plt.grid()
			plt.text(0,rmax/2-50,'Wavenumber 2',fontsize=20,style='italic',horizontalalignment='center')

			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.vt10_wavenumber.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.vt10_wavenumber.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()			
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")

			#Now Make Plot for Comparison With Radar
			plt.figure(figsize=(19.5,12))
			plt.subplot(121)
			plt.contourf(x_sr*0.54,y_sr*0.54,dbz_750,levs_dbz,cmap=colormap_dbz,norm=norm_dbz)
			plt.xlim(-132,132)
			plt.ylim(-132,132)
			plt.xticks(np.linspace(-100,100,5),fontsize=14)
			plt.yticks(np.linspace(-100,100,5),fontsize=14)
			plt.gca().set_aspect('equal', adjustable='box')
			plt.grid()
			plt.arrow(0,0,ushear*3.5*1.94,vshear*3.5*1.94, width=2, head_width=10, head_length=10, fc='blue', ec='black')
			plt.xticks(np.linspace(-100,100,5),fontsize=14)
			plt.yticks(np.linspace(-100,100,5),fontsize=14)
			plt.xlabel('East-West Distance (n mi)',fontsize=18)
			plt.ylabel('North-South Distance (n mi)',fontsize=18)
			plt.title(EXPT.strip()+'\n'+ '750-hPa Reflectivity (dbz, Shading)'+'\n'+'750-hPa Wind Barbs (kt)'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
			plt.barbs(x_sr[::9]*0.54,y_sr[::9]*0.54,u750[::9,::9]*1.94,v750[::9,::9]*1.94,length=6,sizes=dict(spacing=0.15,height=0.4))
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
			plt.contourf(x_sr*0.54,y_sr*0.54,wind_750*1.94,levs_wind,cmap=colormap_wind,norm=norm_wind)
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
			plt.title(EXPT.strip()+'\n'+'750-hPa Wind (kt, Shading)'+'\n'+'750-hPa (Black) and 450-hPa (Gray) Streamlines'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
			plt.text(-127,127,'2-km Max'+r'$\bf\overline{V}_{t}$'+' (RMW):\n'+vmaxstring+' kt ('+rmwstring+' n mi)',fontsize=14,verticalalignment='top', horizontalalignment='left',color='k',weight = 'bold',bbox=dict(facecolor='white', edgecolor='black'))
			plt.text(127,127,'Shear:\n'+shearstring+' kt',fontsize=14,verticalalignment='top', horizontalalignment='right',color='blue',weight = 'bold',bbox=dict(facecolor='white', edgecolor='black'))
			ticks=[7, 16, 25, 34, 40, 46, 52, 58, 64, 80, 96, 110, 125, 140, 155]
			plt.gca().streamplot(x_sr_2*0.54,y_sr_2*0.54,u750*1.94,v750*1.94,density=3,color='k',linewidth=2,arrowstyle='->',arrowsize=2)
			plt.gca().streamplot(x_sr_2*0.54,y_sr_2*0.54,u450*1.94,v450*1.94,density=3,color='0.5',linewidth=2,arrowstyle='->',arrowsize=2)
			plt.gca().arrow(0,0,ushear*3.5*1.94,vshear*3.5*1.94, width=2, head_width=10, head_length=10, fc='blue', ec='black',zorder=10)
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
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.dbz_750_wind_750_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.dbz_750_wind_750_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()

			# Close the GrADs control file
			ga('close 1')

			# Convert the figure to GIF format, if required.
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")

			
	# Write the input file to a log to mark that it has ben processed
	PLOTTED_FILES=ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
	os.system("sed -i '/"+np.str(os.path.basename(FILE))+"/d' "+PLOTTED_FILES)
	os.system('echo "'+np.str(FILE)+' 1" >> '+PLOTTED_FILES)
	os.system('sort -u '+PLOTTED_FILES+' > '+PLOTTED_FILES+'.TMP')
	os.system('mv '+PLOTTED_FILES+'.TMP '+PLOTTED_FILES)


os.system('lockfile -r-1 -l 180 '+ST_LOCK_FILE)
os.system('echo "complete" > '+STATUS_FILE)
os.system('rm -f '+ST_LOCK_FILE)
