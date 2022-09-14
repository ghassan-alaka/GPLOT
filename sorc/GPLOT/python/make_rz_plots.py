#!/usr/bin/env python

# Check that GPLOT_DIR is defined in the environment.
import os, time, warnings
GPLOT_DIR = os.environ['GPLOT_DIR']
print('MSG: Found this GPLOT location --> '+GPLOT_DIR)

#Import necessary modules
print('MSG: Importing Everything Needed')
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
sys.path.append(GPLOT_DIR+'/sorc/GPLOT/python/modules')
import centroid
import glob
import cmath
import subprocess
from mpl_toolkits.axes_grid1 import make_axes_locatable


##############################
def main():

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
	NMLDIR = GPLOT_DIR+'/parm'
	if os.path.exists(NMLIST):
		MASTER_NML_IN = NMLIST
	elif os.path.exists(GPLOT_DIR+'/parm/'+NMLIST):
		MASTER_NML_IN = NML_DIR+'/'+NMLIST
	else:
		print("ERROR: I couldn't find the Master Namelist.")
		sys.exit()
	PYTHONDIR = GPLOT_DIR+'/sorc/GPLOT/python'
	
	
	# Read the master namelist
	DSOURCE = subprocess.run(['grep','^DSOURCE',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1]
	EXPT = subprocess.run(['grep','^EXPT',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1]
	ODIR = subprocess.run(['grep','^ODIR =',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1].strip()
	try:
		ODIR_TYPE = int(subprocess.run(['grep','^ODIR_TYPE',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1])
	except:
		ODIR_TYPE = 0
	if ODIR_TYPE == 1:
		ODIR = ODIR+'/polar/'
	else:
		ODIR = ODIR+'/'+EXPT.strip()+'/'+IDATE.strip()+'/polar/'
	
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
	ST_LOCK_FILE = ODIR.strip()+'status.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log.lock'
	ATCF_FILE = ODIR.strip()+'ATCF_FILES.dat'
	
	
	#Get parameters from input file
	resolution = float(RESOLUTION)
	rmax = float(RMAX)
	zsize = int(LEVS)
	
	# Get the ATCF file.
	ATCF_LIST = np.genfromtxt(ODIR+'ATCF_FILES.dat',dtype='str')
	if ATCF_LIST.size > 1:
		print('MSG: Found multiple ATCFs')
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
	
	# Get the forecast lead time list.
	FHR_LIST = np.array( np.genfromtxt(ALLFHR_FILE,dtype='int') )
	if (FHR_LIST.size == 1):
		FHR_LIST = np.append(FHR_LIST,"999")
		UNPLOTTED_LIST = np.append(UNPLOTTED_LIST,"MISSING")
	
	# Define executables
	X_G2CTL = GPLOT_DIR+'/sorc/GPLOT/grads/g2ctl.pl'
	
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
	
		# IS THIS IMPORTANT FOR REAL-TIME??
		#if ( centerlat > 50.0):
		#	# Write the input file to a log to mark that it has ben processed
		#	PLOTTED_FILES=ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
		#	os.system('echo "'+str(FILE)+'" >> '+PLOTTED_FILES)
		#	os.system('sort -u '+PLOTTED_FILES+' > '+PLOTTED_FILES+'.TMP')
		#	os.system('mv '+PLOTTED_FILES+'.TMP '+PLOTTED_FILES)
		#	break
	
		figuretest = np.shape([g for g in glob.glob(f"{ODIR}/*{TCNAME.lower()}*{format(FHR,'03d')}{figext}")])[0]
		if (figuretest < 1):
			print('None of These Yet!')
			print(figuretest)
			print('h = ',list(FHRIND))
	
			# Make sure the data file 'FILE' exists.
			gribfiletest = os.system('ls '+FILE)
	
			if (gribfiletest < 1):
	
				# Create the GrADs control file, if it hasn't already been created.
				CTL_FILE = TMPDIR+FILE_BASE+'.ctl'
				IDX_FILE = TMPDIR+FILE_BASE+'.2.idx'
				LOCK_FILE = TMPDIR+FILE_BASE+'.lock'
				while os.path.exists(LOCK_FILE):
					print('MSG: '+TMPDIR+FILE_BASE+' is locked. Sleeping for 5 seconds.')
					time.sleep(5)
					LOCK_TEST = os.popen('find '+LOCK_FILE+' -mmin +3 2>/dev/null').read()
					if LOCK_TEST:  os.system('rm -f '+LOCK_FILE)
	
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
				lonmax = centerlon + xoffset
				lonmin = centerlon - xoffset
				ga(f'set lon {lonmin} {lonmax}')
				latmax = centerlat + yoffset
				latmin = centerlat - yoffset
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
				ga(f'set z 1 {zsize}')
				levs = ga.exp('lev')
				zlevs = levs[0,0,:]
				z = np.zeros((zsize,0))*np.nan
				for i in range(zsize):  z[i] = levs[1,1,i]

				#Get data
				print('MSG: Getting Data Now. Using an xoffset of '+str(xoffset)+' degrees')
				uwind = ga.exp('ugrdprs')
				vwind = ga.exp('vgrdprs')
				omega = ga.exp('vvelprs')
				print('MSG: Done With u,v,w')
				dbz = ga.exp('refdprs')
				hgt = ga.exp('hgtprs')
				temp = ga.exp('tmpprs')
				print('MSG: Done with dbz, hgt, temp')
				q = ga.exp('spfhprs')
				rh = ga.exp('rhprs')
				print('MSG: Done with q, rh')
				u10 = ga.exp('ugrd10m')
				v10 = ga.exp('vgrd10m')
				print('MSG: Done with u10,v10')
				
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
				r = np.linspace(0,rmax,(int(rmax//resolution)+1))
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
				print('MSG: Doing the Polar Interpolation Now')
	
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
	
				u_p_850_ring = u_p_850[:,int(np.round(200/resolution)):int(np.round(rmax/resolution))]
				v_p_850_ring = v_p_850[:,int(np.round(200/resolution)):int(np.round(rmax/resolution))]
				u_p_200_ring = u_p_200[:,int(np.round(200/resolution)):int(np.round(rmax/resolution))]
				v_p_200_ring = v_p_200[:,int(np.round(200/resolution)):int(np.round(rmax/resolution))]
	
				u_p_850_ring_mean = np.nanmean(np.nanmean(u_p_850_ring))
				v_p_850_ring_mean = np.nanmean(np.nanmean(v_p_850_ring))
				u_p_200_ring_mean = np.nanmean(np.nanmean(u_p_200_ring))
				v_p_200_ring_mean = np.nanmean(np.nanmean(v_p_200_ring))
	
				ushear = u_p_200_ring_mean-u_p_850_ring_mean
				vshear = v_p_200_ring_mean-v_p_850_ring_mean
	
				shearmag = np.hypot(ushear,vshear)
				sheardir = np.arctan2(vshear,ushear)*180.0/pi
				if np.isnan(shearmag):
					ga('close 1')
					update_plottedfile(ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log', FILE)
					continue
				else:
					shearstring = str(int(np.round(shearmag*1.94,0)))

	
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
				sheardir_index = int(sheardir_5deg/5)
	
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
				with warnings.catch_warnings():
					warnings.filterwarnings(action='ignore', message='Mean of empty slice')
					vt_p_mean = np.nanmean(vt_p,0)
					ur_p_mean = np.nanmean(ur_p,0)
					w_p_mean = np.NaN if np.isnan(w_p).all() else np.nanmean(w_p,0)
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
				with warnings.catch_warnings():
					warnings.filterwarnings(action='ignore', message='Mean of empty slice')
					warnings.filterwarnings(action='ignore', message='All-NaN slice encountered')
					for k in range(zsize):
						threshold[k] = np.nanmin(hgt[:,:,k])+0.2*(np.nanmax(hgt[:,:,k])-np.nanmin(hgt[:,:,k]))
	
				wind = np.hypot(uwind,vwind)
				threshold2 = np.ones(zsize)*np.nan
				with warnings.catch_warnings():
					warnings.filterwarnings(action='ignore', message='Mean of empty slice')
					warnings.filterwarnings(action='ignore', message='All-NaN slice encountered')
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
				if np.isnan(rmw_750):
					ga('close 1')
					update_plottedfile(ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log', FILE)
					continue
				else:
					rmwstring = str(int(np.round(rmw_750*0.54,0)))
				if np.isnan(vt_p_mean_max_750):
					ga('close 1')
					update_plottedfile(ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log', FILE)
					continue
				else:
					vmaxstring = str(int(np.round(vt_p_mean_max_750*1.94,0)))

	
				u750 = uwind[:,:,10]
				v750 = vwind[:,:,10]
				u450 = uwind[:,:,22]
				v450 = vwind[:,:,22]
				wind_750 = np.hypot(uwind[:,:,10],vwind[:,:,10])
				dbz_750 = dbz[:,:,10]
	
				#Make Plots
				print('MSG: Doing Plots Now')
				if os.path.exists(f'{NMLDIR}/namelist.polar.structure.{EXPT}'):
					namelist_structure_vars = np.genfromtxt(f'{NMLDIR}/namelist.polar.structure.{EXPT}',delimiter=',',dtype='str')
				else:
					namelist_structure_vars = np.genfromtxt(f'{NMLDIR}/namelist.polar.structure',delimiter=',',dtype='str')
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
				do_dbz_wavenumber = namelist_structure_vars[13,1]
				do_rh_wavenumber = namelist_structure_vars[14,1]
				do_vt10_wavenumber = namelist_structure_vars[15,1]
				do_vt_tendency = namelist_structure_vars[16,1]
				do_vort_tendency = namelist_structure_vars[17,1]
				do_ur_pbl_p_mean = namelist_structure_vars[18,1]
				do_radar_plots = namelist_structure_vars[19,1]
				do_soundings = namelist_structure_vars[20,1]
				do_shear_and_rh_plots = namelist_structure_vars[21,1]
	
				#Load the colormaps needed
				color_data_vt = np.genfromtxt(GPLOT_DIR+'/sorc/GPLOT/python/colormaps/colormap_wind.txt')
				colormap_vt = matplotlib.colors.ListedColormap(color_data_vt)
				levs_vt = np.linspace(0,80,41,endpoint=True)
				norm_vt = colors.BoundaryNorm(levs_vt,256)
	
				color_data_ur = np.genfromtxt(GPLOT_DIR+'/sorc/GPLOT/python/colormaps/bluewhitered.txt')
				colormap_ur = matplotlib.colors.ListedColormap(color_data_ur)
				levs_ur = np.linspace(-30,30,31,endpoint=True)
				norm_ur = colors.BoundaryNorm(levs_ur,256)
	
				color_data_w = np.genfromtxt(GPLOT_DIR+'/sorc/GPLOT/python/colormaps/bluewhitered.txt')
				colormap_w = matplotlib.colors.ListedColormap(color_data_w)
				levs_w = np.linspace(-5,5,41,endpoint=True)
				norm_w = colors.BoundaryNorm(levs_w,256)
	
				color_data_dbz = np.genfromtxt(GPLOT_DIR+'/sorc/GPLOT/python/colormaps/colormap_radar.txt')
				colormap_dbz = matplotlib.colors.ListedColormap(color_data_dbz)
				levs_dbz = np.linspace(0,80,41,endpoint=True)
				norm_dbz = colors.BoundaryNorm(levs_dbz,256)
	
				color_data_rh = np.genfromtxt(GPLOT_DIR+'/sorc/GPLOT/python/colormaps/colormap_brown_to_green.txt')
				colormap_rh = matplotlib.colors.ListedColormap(color_data_rh)
				levs_rh = np.linspace(0,100,41,endpoint=True)
				norm_rh = colors.BoundaryNorm(levs_rh,256)
	
	
				color_data_wind = np.genfromtxt(GPLOT_DIR+'/sorc/GPLOT/python/colormaps/colormap_wind.txt')
				colormap_wind = matplotlib.colors.ListedColormap(color_data_wind)
				levs_wind = [0,7,10,13,16,19,22,25,28,31,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,69.333,74.666,80,85.333,90.666,96,100.666,105.333,110,115,120,125,130,132,140,145,150,155,160]
				norm_wind = colors.BoundaryNorm(levs_wind,256)
	
	
				# FIGURE 1: Azimuthal Mean Radial Wind
				if do_ur_mean == 'Y':
					fig1 = plt.figure(figsize=(20.5, 10.5))
					ax1 = fig1.add_subplot(1, 1, 1)
					co1 = ax1.contourf(r, zlevs, np.flipud(np.rot90(ur_p_mean,1)), levs_ur, \
							  cmap=colormap_ur, norm=norm_ur, extend='both')
					ax1 = axes_radpres(ax1, rmax, 0)
					cbar1 = plt.colorbar(co1, ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
					cbar1.ax.tick_params(labelsize=24)
					ax1.set_title(EXPT.strip()+'\n'+ r'Azimuthal Mean Radial Wind ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=24, weight = 'bold',loc='left')
					ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
					figfname = ODIR+'/'+LONGSID.lower()+'.ur_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
					plt.close(fig1)
	
	
				# FIGURE 2: Azimuthal Mean Tangential Wind
				if do_vt_mean == 'Y':
					fig2 = plt.figure(figsize=(20.5, 10.5))
					ax2 = fig2.add_subplot(1, 1, 1)
					co2 = ax2.contourf(r, zlevs, np.flipud(np.rot90(vt_p_mean, 1)), levs_vt, \
							   cmap=colormap_vt, norm=norm_vt, extend='max')
					ax2 = axes_radpres(ax2, rmax, 0)
					cbar2 = plt.colorbar(co2, ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80])
					cbar2.ax.tick_params(labelsize=24)
					ax2.set_title(EXPT.strip()+'\n'+ r'Azimuthal Mean Tangential Wind ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=24, weight = 'bold',loc='left')
					ax2.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
					figfname = ODIR+'/'+LONGSID.lower()+'.vt_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig2.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");
					plt.close(fig2)
	
	
				# FIGURE 3: Azimuthal Mean Vertical Velocity
				if do_w_mean == 'Y':
					fig3 = plt.figure(figsize=(20.5, 10.5))
					ax3 = fig3.add_subplot(1, 1, 1)
					co3 = ax3.contourf(r, zlevs, np.flipud(np.rot90(w_p_mean, 1)), levs_w, \
							   cmap=colormap_w, norm=norm_w, extend='both')
					ax3 = axes_radpres(ax3, rmax, 0)
					cbar3 = plt.colorbar(co3, ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
					cbar3.ax.tick_params(labelsize=24)
					ax3.set_title(EXPT.strip()+'\n'+ r'Azimuthal Mean W ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
					ax3.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
					figfname = ODIR+'/'+LONGSID.lower()+'.w_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig3.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");
					plt.close(fig3)
	
	
				# FIGURE 4: Azimuthal Mean Reflectivity
				if do_dbz_mean == 'Y':
					fig4 = plt.figure(figsize=(20.5, 10.5))
					ax4 = fig4.add_subplot(1, 1, 1)
					co4 = ax4.contourf(r, zlevs, np.flipud(np.rot90(dbz_p_mean, 1)), levs_dbz, \
							   cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax4 = axes_radpres(ax4, rmax, 0)
					cbar4 = plt.colorbar(co4, ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
					cbar4.ax.tick_params(labelsize=24)
					ax4.set_title(EXPT.strip()+'\n'+ r'Azimuthal Mean Reflectivity ($dBZ$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
					ax4.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
					figfname = ODIR+'/'+LONGSID.lower()+'.dbz_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig4.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");
					plt.close(fig4)
	
	
				# FIGURE 5: Azimuthal Mean Relative Humidity
				if do_rh_mean == 'Y':
					fig5 = plt.figure(figsize=(20.5, 10.5))
					ax5 = fig5.add_subplot(1, 1, 1)
					co5 = ax5.contourf(r, zlevs, np.flipud(np.rot90(rh_p_mean, 1)), levs_rh, \
							   cmap=colormap_rh, norm=norm_rh, extend='max')
					ax5 = axes_radpres(ax5, rmax, 0)
					cbar5 = plt.colorbar(co5, ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
					cbar5.ax.tick_params(labelsize=24)
					ax5.set_title(EXPT.strip()+'\n'+ r'Azimuthal Mean Relative Humidity ($\%$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
					ax5.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
					figfname = ODIR+'/'+LONGSID.lower()+'.rh_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig5.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");
					plt.close(fig5)
	
	
				# FIGURE 6: Along-Shear Reflectivity
				if do_dbz_alongshear == 'Y':
					fig6 = plt.figure(figsize=(20.5, 10.5))
					ax6 = fig6.add_subplot(1, 1, 1)
					co6 = ax6.contourf(r, zlevs, np.flipud(np.rot90(dbz_p_downshear_mean, 1)), levs_dbz, \
							   cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax6.contourf(-r, zlevs, np.flipud(np.rot90(dbz_p_upshear_mean, 1)), levs_dbz, \
						     cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax6 = axes_radpres(ax6, rmax, -rmax)
					cbar6 = plt.colorbar(co6, ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
					cbar6.ax.tick_params(labelsize=24)
					ax6.text(-rmax+0.05*(2*rmax), 150, 'Upshear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax6.text(rmax-0.05*(2*rmax), 150, 'Downshear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax6.set_title(EXPT.strip()+'\n'+ r'Along-Shear Reflectivity ($dBZ$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
					ax6.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
					figfname = ODIR+'/'+LONGSID.lower()+'.dbz_alongshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig6.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");
					plt.close(fig6)
	
	
				# FIGURE 7: Along-Shear Radial Wind
				if do_ur_alongshear == 'Y':
					fig7 = plt.figure(figsize=(20.5, 10.5))
					ax7 = fig7.add_subplot(1, 1, 1)
					co7 = ax7.contourf(r, zlevs, np.flipud(np.rot90(ur_p_downshear_mean, 1)), levs_ur, \
							   cmap=colormap_ur, norm=norm_ur, extend='both')
					ax7.contourf(-r,zlevs,np.flipud(np.rot90(ur_p_upshear_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
					ax7 = axes_radpres(ax7, rmax, -rmax)
					cbar7 = plt.colorbar(co7, ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
					cbar7.ax.tick_params(labelsize=24)
					ax7.text(-rmax+0.05*(2*rmax), 150, 'Upshear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax7.text(rmax-0.05*(2*rmax), 150, 'Downshear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax7.set_title(EXPT.strip()+'\n'+ r'Along-Shear Radial Wind ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
					ax7.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
					figfname = ODIR+'/'+LONGSID.lower()+'.ur_alongshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig7.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");
					plt.close(fig7)
	
	
				# FIGURE 8: Along-Shear Vertical Velocity
				if do_w_alongshear == 'Y':
					fig8 = plt.figure(figsize=(20.5, 10.5))
					ax8 = fig8.add_subplot(1, 1, 1)
					co8 = ax8.contourf(r, zlevs, np.flipud(np.rot90(w_p_downshear_mean, 1)), levs_w, \
							   cmap=colormap_w, norm=norm_w, extend='both')
					ax8.contourf(-r, zlevs, np.flipud(np.rot90(w_p_upshear_mean, 1)), levs_w, \
						     cmap=colormap_w, norm=norm_w, extend='both')
					ax8 = axes_radpres(ax8, rmax, -rmax)
					cbar8 = plt.colorbar(co8, ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
					cbar8.ax.tick_params(labelsize=24)
					ax8.text(-rmax+0.05*(2*rmax), 150, 'Upshear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax8.text(rmax-0.05*(2*rmax), 150, 'Downshear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax8.set_title(EXPT.strip()+'\n'+ r'Along-Shear W ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
					ax8.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
					figfname = ODIR+'/'+LONGSID.lower()+'.w_alongshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig8.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");
					plt.close(fig8)
	
	
				# FIGURE 9: Along-Shear Relative Humidity
				if do_rh_alongshear == 'Y':
					fig9 = plt.figure(figsize=(20.5, 10.5))
					ax9 = fig9.add_subplot(1, 1, 1)
					co9 = ax9.contourf(r, zlevs, np.flipud(np.rot90(rh_p_downshear_mean, 1)), levs_rh, \
							   cmap=colormap_rh, norm=norm_rh, extend='both')
					ax9.contourf(-r, zlevs, np.flipud(np.rot90(rh_p_upshear_mean, 1)), levs_rh, \
						     cmap=colormap_rh, norm=norm_rh, extend='both')
					ax9 = axes_radpres(ax9, rmax, -rmax)
					cbar9 = plt.colorbar(co9, ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
					cbar9.ax.tick_params(labelsize=24)
					ax9.text(-rmax+0.05*(2*rmax), 150, 'Upshear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax9.text(rmax-0.05*(2*rmax), 150, 'Downshear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax9.set_title(EXPT.strip()+'\n'+ r'Along-Shear RH ($\%$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
					ax9.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
					figfname = ODIR+'/'+LONGSID.lower()+'.rh_alongshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig9.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");
					plt.close(fig9)
	
	
				# FIGURE 10: Across-Shear Reflectivity
				if do_dbz_acrossshear == 'Y':
					fig10 = plt.figure(figsize=(20.5, 10.5))
					ax10 = fig10.add_subplot(1, 1, 1)
					co10 = ax10.contourf(r, zlevs, np.flipud(np.rot90(dbz_p_rightshear_mean, 1)), levs_dbz, \
							   cmap=colormap_dbz, norm=norm_dbz, extend='both')
					ax10.contourf(-r, zlevs, np.flipud(np.rot90(dbz_p_leftshear_mean, 1)), levs_dbz, \
						     cmap=colormap_dbz, norm=norm_dbz, extend='both')
					ax10 = axes_radpres(ax10, rmax, -rmax)
					cbar10 = plt.colorbar(co10, ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
					cbar10.ax.tick_params(labelsize=24)
					ax10.text(-rmax+0.05*(2*rmax), 150, 'Left of shear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax10.text(rmax-0.05*(2*rmax), 150, 'Right of shear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax10.set_title(EXPT.strip()+'\n'+ r'Across-Shear Reflectivity ($dBZ$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']', fontsize=24, weight='bold', loc='left')
					ax10.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
					figfname = ODIR+'/'+LONGSID.lower()+'.rh_acrossshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig10.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");
					plt.close(fig10)
	
	
				# FIGURE 11: Across-Shear Radiaul Wind
				if do_ur_acrossshear == 'Y':
					fig11 = plt.figure(figsize=(20.5, 10.5))
					ax11 = fig11.add_subplot(1, 1, 1)
					co11 = ax11.contourf(r, zlevs, np.flipud(np.rot90(ur_p_rightshear_mean, 1)), levs_ur, \
							   cmap=colormap_ur, norm=norm_ur, extend='both')
					ax11.contourf(-r, zlevs, np.flipud(np.rot90(ur_p_leftshear_mean, 1)), levs_ur, \
						     cmap=colormap_ur, norm=norm_ur, extend='both')
					ax11 = axes_radpres(ax11, rmax, -rmax)
					cbar11 = plt.colorbar(co11, ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
					cbar11.ax.tick_params(labelsize=24)
					ax11.text(-rmax+0.05*(2*rmax), 150, 'Left of shear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax11.text(rmax-0.05*(2*rmax), 150, 'Right of shear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax11.set_title(EXPT.strip()+'\n'+ r'Across-Shear Radial Wind ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']', fontsize=24, weight='bold', loc='left')
					ax11.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(), fontsize=24, color='brown', loc='right')
					figfname = ODIR+'/'+LONGSID.lower()+'.ur_acrossshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig11.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
					plt.close(fig11)
	
	
				# FIGURE 12: Across-Shear Vertical Velocity
				if do_w_acrossshear == 'Y':
					fig12 = plt.figure(figsize=(20.5, 10.5))
					ax12 = fig12.add_subplot(1, 1, 1)
					co12 = ax12.contourf(r, zlevs, np.flipud(np.rot90(w_p_rightshear_mean, 1)), levs_w, \
							   cmap=colormap_w, norm=norm_w, extend='both')
					ax12.contourf(-r, zlevs, np.flipud(np.rot90(w_p_leftshear_mean, 1)), levs_w, \
						     cmap=colormap_w, norm=norm_w, extend='both')
					ax12 = axes_radpres(ax12, rmax, -rmax)
					cbar12 = plt.colorbar(co12, ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
					cbar12.ax.tick_params(labelsize=24)
					ax12.text(-rmax+0.05*(2*rmax), 150, 'Left of shear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax12.text(rmax-0.05*(2*rmax), 150, 'Right of shear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax12.set_title(EXPT.strip()+'\n'+ r'Across-Shear W ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']', fontsize=24, weight='bold', loc='left')
					ax12.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(), fontsize=24, color='brown', loc='right')
					figfname = ODIR+'/'+LONGSID.lower()+'.w_acrossshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig12.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
					plt.close(fig12)
	
	
				# FIGURE 13: Across-Shear Relative Humidity
				if do_rh_acrossshear == 'Y':
					fig13 = plt.figure(figsize=(20.5, 10.5))
					ax13 = fig13.add_subplot(1, 1, 1)
					co13 = ax13.contourf(r, zlevs, np.flipud(np.rot90(rh_p_rightshear_mean, 1)), levs_rh, \
							   cmap=colormap_rh, norm=norm_rh, extend='both')
					ax13.contourf(-r, zlevs, np.flipud(np.rot90(rh_p_leftshear_mean, 1)), levs_rh, \
						     cmap=colormap_rh, norm=norm_rh, extend='both')
					ax13 = axes_radpres(ax13, rmax, -rmax)
					cbar13 = plt.colorbar(co13, ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
					cbar13.ax.tick_params(labelsize=24)
					ax13.text(-rmax+0.05*(2*rmax), 150, 'Left of shear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
					ax13.text(rmax-0.05*(2*rmax), 150, 'Right of shear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
					ax13.set_title(EXPT.strip()+'\n'+ r'Across-Shear RH ($\%$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']', fontsize=24, weight='bold', loc='left')
					ax13.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(), fontsize=24, color='brown', loc='right')
					figfname = ODIR+'/'+LONGSID.lower()+'.rh_acrossshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig13.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
					plt.close(fig13)
	
	
				# FIGURE 14: Wavenumber 0,1,2 components of 500-hPa Reflectivity
				if do_dbz_wavenumber == 'Y':
					fig14 = plt.figure(figsize=(15,15))
					ticks14 = [0, 10, 20, 30, 40, 50, 60, 70]

					# Panel A	
					ax14a = fig14.add_subplot(2, 2, 1)
					co14a = ax14a.contourf(XI, YI, dbz_p[:,:,20], levs_dbz, \
								cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax14a = axes_wavenumber(ax14a, rmax/2, -rmax/2)
					cbar14a = plt.colorbar(co14a, ticks=ticks14)
					cbar14a.ax.tick_params(labelsize=18)
					ax14a.arrow(0, 0, (ushear/25)*np.max(XI/2), (vshear/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax14a.set_title(EXPT.strip()+'\n'+ r'WV#0,1,2 500-hPa Reflectivity ($dBZ$, Shading)'+'\n'+'Shear Vector in Black'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
					ax14a.text(0,rmax/2-50,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

					# Panel B
					ax14b = fig14.add_subplot(2, 2, 2)
					co14b = ax14b.contourf(XI, YI, dbz_p_w0[:,:,20], levs_dbz, \
								cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax14b = axes_wavenumber(ax14b, rmax/2, -rmax/2)
					cbar14b = plt.colorbar(co14b, ticks=ticks14)
					cbar14b.ax.tick_params(labelsize=18)
					ax14b.arrow(0, 0, (ushear/25)*np.max(XI/2), (vshear/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax14b.set_title(LONGSID.upper()+'\n'+'VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+'Shear Magnitude= '+str(int(np.round(shearmag*1.94,0)))+'kts'+'\n'+'Shear Direction= '+str(int(np.round(sheardir_met,0)))+'$^\circ$',fontsize=20,color='brown',loc='right')
					ax14b.text(0,rmax/2-50,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

					# Panel C	
					ax14c = fig14.add_subplot(2, 2, 3)
					co14c = ax14c.contourf(XI, YI, dbz_p_w1[:,:,20], levs_dbz, \
								cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax14c = axes_wavenumber(ax14c, rmax/2, -rmax/2)
					cbar14c = plt.colorbar(co14c, ticks=ticks14)
					cbar14c.ax.tick_params(labelsize=18)
					ax14c.arrow(0, 0, (ushear/25)*np.max(XI/2), (vshear/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax14c.text(0,rmax/2-50,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

					# Panel D
					ax14d = fig14.add_subplot(2, 2, 4)
					co14d = ax14d.contourf(XI, YI, dbz_p_w2[:,:,20], levs_dbz, \
								cmap=colormap_dbz, norm=norm_dbz, extend='max')
					ax14d = axes_wavenumber(ax14d, rmax/2, -rmax/2)
					cbar14d = plt.colorbar(co14d, ticks=ticks14)
					cbar14d.ax.tick_params(labelsize=18)
					ax14d.arrow(0, 0, (ushear/25)*np.max(XI/2), (vshear/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax14d.text(0,rmax/2-50,'Wavenumber 2',fontsize=20,style='italic',horizontalalignment='center')

					# Finalize figure
					figfname = ODIR+'/'+LONGSID.lower()+'.dbz500_wavenumber.'+forecastinit+'.polar.f'+format(FHR,'03d')
					#fig14.tight_layout()
					fig14.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					plt.close(fig14)
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")

	
				# FIGURE 15: Wavenumber 0,1,2 components of 500-hPa Relative Humidity
				if do_rh_wavenumber == 'Y':
					fig15 = plt.figure(figsize=(15,15))
					ticks15 = [0, 20, 40, 60, 80, 100]

					# Panel A
					ax15a = fig15.add_subplot(2, 2, 1)
					co15a = ax15a.contourf(XI, YI, rh_p[:,:,20], levs_rh, \
								cmap=colormap_rh, norm=norm_rh, extend='max')
					ax15a = axes_wavenumber(ax15a, rmax/2, -rmax/2)
					cbar15a = plt.colorbar(co15a, ticks=ticks15)
					cbar15a.ax.tick_params(labelsize=18)
					ax15a.arrow(0, 0, (ushear/25)*np.max(XI/2), (vshear/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax15a.set_title(EXPT.strip()+'\n'+ r'WV#0,1,2 500-hPa RH ($\%$, Shading)'+'\n'+'Shear Vector in Black'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
					ax15a.text(0,rmax/2-50,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

					# Panel B
					ax15b = fig15.add_subplot(2, 2, 2)
					co15b = ax15b.contourf(XI, YI, rh_p_w0[:,:,20], levs_rh, \
								cmap=colormap_rh, norm=norm_rh, extend='max')
					ax15b = axes_wavenumber(ax15b, rmax/2, -rmax/2)
					cbar15b = plt.colorbar(co15b, ticks=ticks15)
					cbar15b.ax.tick_params(labelsize=18)
					ax15b.arrow(0, 0, (ushear/25)*np.max(XI/2), (vshear/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax15b.set_title(LONGSID.upper()+'\n'+'VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+'Shear Magnitude= '+str(int(np.round(shearmag*1.94,0)))+'kts'+'\n'+'Shear Direction= '+str(int(np.round(sheardir_met,0)))+'$^\circ$',fontsize=20,color='brown',loc='right')
					ax15b.text(0,rmax/2-50,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

					# Panel C
					ax15c = fig15.add_subplot(2, 2, 3)
					co15c = ax15c.contourf(XI, YI, rh_p_w1[:,:,20], levs_rh, \
								cmap=colormap_rh, norm=norm_rh, extend='max')
					ax15c = axes_wavenumber(ax15c, rmax/2, -rmax/2)
					cbar15c = plt.colorbar(co15c, ticks=ticks15)
					cbar15c.ax.tick_params(labelsize=18)
					ax15c.arrow(0, 0, (ushear/25)*np.max(XI/2), (vshear/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax15c.text(0,rmax/2-50,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

					# Panel D
					ax15d = fig15.add_subplot(2, 2, 4)
					co15d = ax15d.contourf(XI, YI, rh_p_w2[:,:,20], levs_rh, \
								cmap=colormap_rh, norm=norm_rh, extend='max')
					ax15d = axes_wavenumber(ax15d, rmax/2, -rmax/2)
					cbar15d = plt.colorbar(co15d, ticks=ticks15)
					cbar15d.ax.tick_params(labelsize=18)
					ax15d.arrow(0, 0, (ushear/25)*np.max(XI/2), (vshear/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax15d.text(0,rmax/2-50,'Wavenumber 2',fontsize=20,style='italic',horizontalalignment='center')

					# Finalize figure
					figfname = ODIR+'/'+LONGSID.lower()+'.rh500_wavenumber.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig15.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					plt.close(fig15)
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
	
	
				# FIGURE 16: Wavenumber 0,1,2 components of 10-m Tangential Wind
				if do_vt10_wavenumber == 'Y':
					fig16 = plt.figure(figsize=(15,15))
					ticks16 = [-30, -20, -10, 0, 10, 20, 30]

					# Panel A
					ax16a = fig16.add_subplot(2, 2, 1)
					co16a = ax16a.contourf(XI, YI, vt10_p[:,:], levs_vt, \
								cmap=colormap_vt, norm=norm_vt, extend='max')
					ax16a = axes_wavenumber(ax16a, rmax/2, -rmax/2)
					cbar16a = plt.colorbar(co16a, ticks=ticks16)
					cbar16a.ax.tick_params(labelsize=18)
					ax16a.arrow(0, 0, (ushear/25)*np.max(XI/2), (vshear/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax16a.set_title(EXPT.strip()+'\n'+ r'WV#0,1,2 10-m Tangential Wind ($m\ s^{-1}$, Shading)'+'\n'+'Shear Vector in Black'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
					ax16a.text(0,rmax/2-50,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

					# Panel B
					ax16b = fig16.add_subplot(2, 2, 2)
					co16b = ax16b.contourf(XI, YI, vt10_p_w0[:,:], levs_vt, \
								cmap=colormap_vt, norm=norm_vt, extend='max')
					ax16b = axes_wavenumber(ax16b, rmax/2, -rmax/2)
					cbar16b = plt.colorbar(co16b, ticks=ticks16)
					cbar16b.ax.tick_params(labelsize=18)
					ax16b.arrow(0, 0, (ushear/25)*np.max(XI/2), (vshear/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax16b.set_title(LONGSID.upper()+'\n'+'VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+'Shear Magnitude= '+str(int(np.round(shearmag*1.94,0)))+'kts'+'\n'+'Shear Direction= '+str(int(np.round(sheardir_met,0)))+'$^\circ$',fontsize=20,color='brown',loc='right')
					ax16b.text(0,rmax/2-50,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

					# Panel C
					ax16c = fig16.add_subplot(2, 2, 3)
					co16c = ax16c.contourf(XI, YI, vt10_p_w1[:,:], levs_vt, \
								cmap=colormap_vt, norm=norm_vt, extend='max')
					ax16c = axes_wavenumber(ax16c, rmax/2, -rmax/2)
					cbar16c = plt.colorbar(co16c, ticks=ticks16)
					cbar16c.ax.tick_params(labelsize=18)
					ax16c.arrow(0, 0, (ushear/25)*np.max(XI/2), (vshear/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax16c.text(0,rmax/2-50,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

					# Panel D
					ax16d = fig16.add_subplot(2, 2, 4)
					co16d = ax16d.contourf(XI, YI, vt10_p_w2[:,:], levs_vt, \
								cmap=colormap_vt, norm=norm_vt, extend='max')
					ax16d = axes_wavenumber(ax16d, rmax/2, -rmax/2)
					cbar16d = plt.colorbar(co16d, ticks=ticks16)
					cbar16d.ax.tick_params(labelsize=18)
					ax16d.arrow(0, 0, (ushear/25)*np.max(XI/2), (vshear/25)*np.max(YI/2), \
							linewidth = 3, head_width=rmax/20, head_length=rmax/10, fc='k', ec='k')
					ax16d.text(0,rmax/2-50,'Wavenumber 2',fontsize=20,style='italic',horizontalalignment='center')

					# Finalize figure
					figfname = ODIR+'/'+LONGSID.lower()+'.vt10_wavenumber.'+forecastinit+'.polar.f'+format(FHR,'03d')
					fig16.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					plt.close(fig16)			
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
	
	
				# FIGURE XX: Radar Comparisons
				if do_radar_plots == 'Y':
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
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
	
	
				# Close the GrADs control file
				ga('close 1')
	
				
		# Write the input file to a log to mark that it has ben processed
		update_plottedfile(ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log', FILE)
	
	
	print('MSG: COMPLETING')
	os.system('lockfile -r-1 -l 180 '+ST_LOCK_FILE)
	os.system('echo "complete" > '+STATUS_FILE)
	os.system('rm -f '+ST_LOCK_FILE)


##############################
def axes_wavenumber(ax, xmax, xmin):
	"""Set up common axes attributes for wavenumber graphics.
	@param ax:   the axes object
	@param xmax: max value of both x/y axes
	@param xmin: min value of both x/y axes
	"""
	ticks = np.linspace(xmin,xmax,7)

	ax.set_xlim(xmin,xmax)
	ax.set_xticks(ticks)
	ax.set_xticklabels([str(int(x)) for x in ticks], fontsize=18)
	ax.set_xlabel('X (km)', fontsize=20)

	ax.set_ylim(xmin,xmax)
	ax.set_yticks(ticks)
	ax.set_yticklabels([str(int(x)) for x in ticks], fontsize=18)
	ax.set_ylabel('Y (km)', fontsize=20)

	ax.set_aspect('equal', adjustable='box')
	ax.grid()

	return ax


def axes_radpres(ax, xmax, xmin, ymax=1000, ymin=100):
	"""Set up common axes attributes for wavenumber graphics.
	@param ax:    the axes object
	@param axmax: max value of both x/y axes
	@param axmin: min value of both x/y axes
	"""
	xticks = np.linspace(xmin,xmax,11)
	yticks = np.linspace(1000,100,10)

	ax.set_xlim(xmin, xmax)
	ax.set_xticks(xticks)
	ax.set_xticklabels([str(int(x)) for x in xticks], fontsize=24)
	ax.set_xlabel('Radius (km)', fontsize=24)

	ax.set_yscale('log')
	ax.set_ylim(ymin,ymax)
	ax.invert_yaxis()
	ax.set_yticks(yticks)
	ax.set_yticklabels([str(int(x)) for x in yticks], fontsize=24)
	ax.set_ylabel('Pressure Level (hPa)', fontsize=24)

	ax.grid()

	return ax


def update_plottedfile(OFILE, IFILE):
        """Update the GPLOT PlottedFiles file to mark a file as processed.
        @param OFILE: the plotted file path as a string
        @param IFILE: the model output file that was processed
        """
        os.system("sed -i '/"+str(os.path.basename(IFILE))+"/d' "+OFILE)
        os.system('echo "'+str(IFILE)+' 1" >> '+OFILE)
        os.system('sort -u '+OFILE+' > '+OFILE+'.TMP')
        os.system('mv '+OFILE+'.TMP '+OFILE)


##############################
if __name__ == '__main__':
	main()
