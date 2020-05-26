###!/lfs3/projects/hur-aoml/Andrew.Hazelton/anaconda3/bin/python

# Check that GPLOT_DIR is defined in the environment.
import os
GPLOT_DIR = os.environ['GPLOT_DIR']
print('MSG: Found this GPLOT location --> '+GPLOT_DIR)

#Import necessary modules
print('Importing Everything Needed')
from py3grads import Grads #This is how we'll get the data
import numpy as np #Used for a lot of the calculations
import metpy
from metpy import interpolate
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
import math
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
PYTHONDIR = sys.argv[11]
if PYTHONDIR == '':
	print("ERROR: Python Directory can't be MISSING.")
	sys.exit()
MASTER_NML_IN = GPLOT_DIR+'/nmlist/'+NMLIST

# Read the master namelist
#NML_DATA = np.genfromtxt(MASTER_NML_IN,dtype='str')
#NML_DATE = np.loadtxt(MASTER_NML_IN)
DSOURCE = subprocess.run(['grep','^DSOURCE',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1]
EXPT = subprocess.run(['grep','^EXPT',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1]
ODIR = subprocess.run(['grep','^ODIR',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1].strip()+'/'+EXPT.strip()+'/'+IDATE.strip()+'/polar/'
#print(ODIR)

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
ATCF_FILE = ODIR.strip()+'ATCF_FILES.dat'

#Get parameters from input file
resolution = np.float(RESOLUTION)
rmax = np.float(RMAX)
zsize_pressure = np.int(LEVS)

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


for (FILE,fff) in zip(UNPLOTTED_LIST,np.array(range(UNPLOTTED_LIST.size))):
	if (FILE == 'MISSING'):
		continue

	print('MSG: Working on this file --> '+str(FILE)+'  '+str(fff))

	os.system('echo "working" > '+STATUS_FILE)

	# Get some useful information about the file name
	FILE_BASE = os.path.basename(FILE)
	FILE_DIR = os.path.dirname(FILE)

	# Find the index of the forecast lead time in the ATCF file.
	FHR = int(FHR_LIST[fff])
	FHRIND = [i for i, s in enumerate(ATCF_DATA[:,5]) if int(s)==FHR]

	# Get coordinate information from ATCF
	lonstr = ATCF_DATA[list(FHRIND),7][0]
	print('lonstr = ',lonstr)
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
	#centerlon = 360-(np.char.strip(lonstr,'W').astype(np.float)/10)
	#centerlat = np.char.strip(latstr,'N').astype(np.float)/10
	forecastinit = ATCF_DATA[list(FHRIND),2][0]
	maxwind = ATCF_DATA[list(FHRIND),8][0]
	minpressure = ATCF_DATA[list(FHRIND),9][0]
	print(str(centerlon)+'  '+str(centerlat))
	exit

	#figuretest = np.shape([g for g in glob.glob(f"{ODIR}/*{TCNAME.lower()}*{format(FHR,'03d')}.png")])[0]
	figuretest = np.shape([g for g in glob.glob(f"{ODIR}/*{TCNAME.lower()}*{format(FHR,'03d')}{figext}")])[0]
	if (figuretest < 1):
		print('None of These Yet!')
		print(figuretest)
		print('h = ',list(FHRIND))

		#Make GrADS control file and index file
		
		#gribfile = datadir+'/natl00l.'+forecastinit+'.'+modeltag+'.f'+format(FHR,'03d')+'.grb2'
		#gribfile2 = datadir+'/natl00l.'+forecastinit+'.'+modeltag+'.f'+format(FHR,'03d')+'.grb2'
		#print(gribfile)
		lscommand = 'ls '+FILE
		gribfiletest = os.system(lscommand)

		if (gribfiletest < 1):
			#command = '/home/rthr-aoml/GPLOT/grads/g2ctl.pl'+' '+gribfile+' '+gribfile+'.2.idx'+' > '+gribfile+'.ctl'
			command = GPLOT_DIR+'/grads/g2ctl.pl'+' '+FILE+' '+TMPDIR+FILE_BASE+'.2.idx'+' > '+TMPDIR+FILE_BASE+'.ctl'
			os.system(command)
			#command2 = 'gribmap -i '+gribfile+'.ctl'
			command2 = 'gribmap -i '+TMPDIR+FILE_BASE+'.ctl -big'
			os.system(command2)
			
			#Open data file
			#datafile = gribfile+'.ctl'
			datafile = TMPDIR+FILE_BASE+'.ctl'

			#Open data file
			ga('open '+datafile)
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
			zstring = 'set z 1 '+np.str(zsize_pressure)
			ga(zstring)
			levs = ga.exp('lev')
		
			#Make them all a 1-d array
			lon = lons[0,:]
			lat = lats[:,0]
			z = np.zeros((zsize_pressure,0))*np.nan
			for i in range(zsize_pressure):
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
			
			#Get 2-d Data
			ga('set z 1')
			u10 = ga.exp('ugrd10m')
			v10 = ga.exp('vgrd10m')
			mslp = ga.exp('msletmsl')
			tmp2m = ga.exp('tmp2m')
			q2m = ga.exp('spfh2m')
			rh2m = ga.exp('rh2m')
			print('Done with u10,v10,mslp,tmp2m,q2m')
			mixr2m = q2m/(1-q2m)
			temp_v_2m = tmp2m*(1+0.61*mixr2m)
			rho2m = mslp/(287*temp_v_2m)
			print('Done with u10,v10')
			
			#Get u850, v850, u200, v200 for Shear Calculation
			ga('set lev 850')
			u850 = ga.exp('ugrdprs')
			v850 = ga.exp('vgrdprs')
			ga('set lev 200')
			u200 = ga.exp('ugrdprs')
			v200 = ga.exp('vgrdprs')
			ga('set z 1')
						
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
			r = np.linspace(0,rmax,(rmax//resolution+1))
			#r = np.linspace(0,600,201)
			#print(r)
			#sys.exit()
			pi = np.arccos(-1)
			theta = np.arange(0,2*pi+pi/36,pi/36)
			R, THETA = np.meshgrid(r, theta)
			XI = R * np.cos(THETA)
			YI = R * np.sin(THETA)

			x_sr = np.round(x_sr/1000,3)
			y_sr = np.round(y_sr/1000,3)

			x_sr_2 = np.linspace(x_sr.min(), x_sr.max(), x_sr.size)
			y_sr_2 = np.linspace(y_sr.min(), y_sr.max(), y_sr.size)

			rnorm = np.linspace(0,6,121)
			Rnorm, THETAnorm = np.meshgrid(rnorm,theta)
			XInorm = Rnorm * np.cos(THETAnorm)
			YInorm = Rnorm * np.sin(THETAnorm)
			
			#Interpolate to Height Coordinates
			print('Doing Height Coordinate Interpolation Now')
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

			uwind = np.ones((np.shape(lat)[0],np.shape(lon)[0],np.shape(heightlevs)[0]))*np.nan
			vwind = np.ones((np.shape(lat)[0],np.shape(lon)[0],np.shape(heightlevs)[0]))*np.nan
			wwind = np.ones((np.shape(lat)[0],np.shape(lon)[0],np.shape(heightlevs)[0]))*np.nan
			dbz = np.ones((np.shape(lat)[0],np.shape(lon)[0],np.shape(heightlevs)[0]))*np.nan
			temp = np.ones((np.shape(lat)[0],np.shape(lon)[0],np.shape(heightlevs)[0]))*np.nan
			q = np.ones((np.shape(lat)[0],np.shape(lon)[0],np.shape(heightlevs)[0]))*np.nan
			rh = np.ones((np.shape(lat)[0],np.shape(lon)[0],np.shape(heightlevs)[0]))*np.nan
			pressure = np.ones((np.shape(lat)[0],np.shape(lon)[0],np.shape(heightlevs)[0]))*np.nan

			for k in range(np.shape(heightlevs)[0]):
				uwind[:,:,k] = metpy.interpolate.interpolate_to_isosurface(hgtT,uwindT,heightlevs[k])
				vwind[:,:,k] = metpy.interpolate.interpolate_to_isosurface(hgtT,vwindT,heightlevs[k])
				wwind[:,:,k] = metpy.interpolate.interpolate_to_isosurface(hgtT,wwindT,heightlevs[k])
				dbz[:,:,k] = metpy.interpolate.interpolate_to_isosurface(hgtT,dbzT,heightlevs[k])
				temp[:,:,k] = metpy.interpolate.interpolate_to_isosurface(hgtT,tempT,heightlevs[k])
				q[:,:,k] = metpy.interpolate.interpolate_to_isosurface(hgtT,qT,heightlevs[k])
				rh[:,:,k] = metpy.interpolate.interpolate_to_isosurface(hgtT,rhT,heightlevs[k])
				pressure[:,:,k] = metpy.interpolate.interpolate_to_isosurface(hgtT,pressureT,heightlevs[k])
			
			uwind[:,:,0] = u10
			vwind[:,:,0] = v10
			wwind[:,:,0] = np.nan
			dbz[:,:,0] = np.nan
			temp[:,:,0] = tmp2m
			q[:,:,0] = q2m
			rh[:,:,0] = rh2m
			pressure[:,:,0] = mslp

			heightlevs_pbl = np.linspace(0,3000,31)
			zsize_pbl = np.shape(heightlevs_pbl)[0]
			uwind_pbl = np.ones((np.shape(lat)[0],np.shape(lon)[0],np.shape(heightlevs_pbl)[0]))*np.nan
			vwind_pbl = np.ones((np.shape(lat)[0],np.shape(lon)[0],np.shape(heightlevs_pbl)[0]))*np.nan
			rho_pbl = np.ones((np.shape(lat)[0],np.shape(lon)[0],np.shape(heightlevs_pbl)[0]))*np.nan
			pressure_pbl = np.ones((np.shape(lat)[0],np.shape(lon)[0],np.shape(heightlevs_pbl)[0]))*np.nan

			for k in range(np.shape(heightlevs_pbl)[0]):
				uwind_pbl[:,:,k] = metpy.interpolate.interpolate_to_isosurface(hgtT,uwindT,heightlevs_pbl[k])
				vwind_pbl[:,:,k] = metpy.interpolate.interpolate_to_isosurface(hgtT,vwindT,heightlevs_pbl[k])
				rho_pbl[:,:,k] = metpy.interpolate.interpolate_to_isosurface(hgtT,rhoT,heightlevs_pbl[k])
				pressure_pbl[:,:,k] = metpy.interpolate.interpolate_to_isosurface(hgtT,pressureT,heightlevs_pbl[k])
			
			uwind_pbl[:,:,0] = u10
			vwind_pbl[:,:,0] = v10
			pressure_pbl[:,:,0] = mslp
			rho_pbl[:,:,0] = rho2m


			#Do interpolation
			print('Doing the Polar Interpolation Now')

			#First initialize u_p and v_p
			u_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			v_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			w_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			dbz_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			temp_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			q_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			rh_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan
			pressure_p = np.ones((np.shape(XI)[0],np.shape(XI)[1],zsize))*np.nan

			for k in range(zsize):
				f_uwind = interpolate.RegularGridInterpolator((y_sr, x_sr), uwind[:,:,k])
				f_vwind = interpolate.RegularGridInterpolator((y_sr, x_sr), vwind[:,:,k])
				f_wwind = interpolate.RegularGridInterpolator((y_sr, x_sr), wwind[:,:,k])
				f_dbz = interpolate.RegularGridInterpolator((y_sr, x_sr), dbz[:,:,k])
				f_temp = interpolate.RegularGridInterpolator((y_sr, x_sr), temp[:,:,k])
				f_q = interpolate.RegularGridInterpolator((y_sr, x_sr), q[:,:,k])
				f_rh = interpolate.RegularGridInterpolator((y_sr, x_sr), rh[:,:,k])
				f_pressure = interpolate.RegularGridInterpolator((y_sr, x_sr), pressure[:,:,k])
			
				u_p[:,:,k] = f_uwind((YI,XI),method='linear')
				v_p[:,:,k] = f_vwind((YI,XI),method='linear')
				w_p[:,:,k] = f_wwind((YI,XI),method='linear')
				dbz_p[:,:,k] = f_dbz((YI,XI),method='linear')
				temp_p[:,:,k] = f_temp((YI,XI),method='linear')
				q_p[:,:,k] = f_q((YI,XI),method='linear')
				rh_p[:,:,k] = f_rh((YI,XI),method='linear')
				pressure_p[:,:,k] = f_pressure((YI,XI),method='linear')

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
			
			vt10_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			ur10_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan

			u200_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			v200_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			u850_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
			v850_p = np.ones((np.shape(XI)[0],np.shape(XI)[1]))*np.nan
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

			
			#Calculate shear
			u850_p_ring = u850_p[:,np.int(np.round(200/resolution)):np.int(np.round(600/resolution))]
			v850_p_ring = v850_p[:,np.int(np.round(200/resolution)):np.int(np.round(600/resolution))]
			u200_p_ring = u200_p[:,np.int(np.round(200/resolution)):np.int(np.round(600/resolution))]
			v200_p_ring = v200_p[:,np.int(np.round(200/resolution)):np.int(np.round(600/resolution))]

			u850_p_ring_mean = np.nanmean(np.nanmean(u850_p_ring))
			v850_p_ring_mean = np.nanmean(np.nanmean(v850_p_ring))
			u200_p_ring_mean = np.nanmean(np.nanmean(u200_p_ring))
			v200_p_ring_mean = np.nanmean(np.nanmean(v200_p_ring))

			ushear = u200_p_ring_mean-u850_p_ring_mean
			vshear = v200_p_ring_mean-v850_p_ring_mean

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
			vt_p_mean = np.nanmean(vt_p,0)
			ur_p_mean = np.nanmean(ur_p,0)
			w_p_mean = np.nanmean(w_p,0)
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
				for h in range (2,np.int((np.shape(theta)[0]+1)/2)):
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
				for h in range (2,np.int((np.shape(theta)[0]+1)/2)):
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
			for k in range(zsize):
				rmw_mean[k] = np.round(np.median(r[vt_p_mean[:,k] > 0.95*np.max(vt_p_mean[:,k])]))
				rmw_mean_index[k] = np.argmin(abs(r-rmw_mean[k])) 
			
			rmw_2km = rmw_mean[4]
			vt_p_mean_max = np.max(vt_p_mean,0)
			vt_p_mean_max_2km = vt_p_mean_max[4]
			rmwstring = np.str(np.int(np.round(rmw_2km*0.54,0)))
			vmaxstring = np.str(np.int(np.round(vt_p_mean_max_2km*1.94,0)))

			if rmax/rmw_2km < np.max(rnorm): #Quick Fix For Big RMWs
				rnorm_max = np.round(rmax/rmw_2km,2)-math.fmod(np.round(rmax/rmw_2km,2),0.05)
				rnorm = np.linspace(0,rnorm_max,(rnorm_max/0.05)+1)
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
			eddy_vort_flux_p_mean = np.nanmean(eddy_vort_flux_p,0)
			term3_vt_tendency_eddy_flux = -eddy_vort_flux_p_mean
			term3_vt_tendency_eddy_flux[:,0] = np.nan
			term3_vt_tendency_eddy_flux[0,:] = np.nan

			#Term4 (Vertical Advection of Eddy Tangential Momentum)
			d_vt_p_perturbation_dz = metpy.calc.first_derivative(vt_p_perturbation,axis=2,delta=500)
			vertical_eddy_advection_p = w_p_perturbation*d_vt_p_perturbation_dz
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
				print('h_tm1 = ',FHRIND2)
				print('FHR_tm1 =',FHR_tm1)
				lonstr_tm1 = ATCF_DATA[FHRIND2,7]
				print('lonstr_tm1 = ',lonstr_tm1)
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
				dt = np.nan
				dx = np.nan
				dy = np.nan

			umotion = dx/dt
			vmotion = dy/dt
			print('fhr = ',FHR)
			print('dt = ',dt)
			print('umotion = ',umotion)
			print('vmotion = ',vmotion)

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

			term1_vort_tendency_horizontal_advection = np.nanmean(horizontal_advection_p,0)
			term1_vort_tendency_horizontal_advection[:,0] = np.nan
			term1_vort_tendency_horizontal_advection[0,:] = np.nan
			term2_vort_tendency_vertical_advection = np.nanmean(vertical_advection_p,0)
			term2_vort_tendency_vertical_advection[:,0] = np.nan
			term2_vort_tendency_vertical_advection[0,:] = np.nan
			term3_vort_tendency_stretching_convergence = np.nanmean(stretching_convergence_p,0)
			term3_vort_tendency_stretching_convergence[:,0] = np.nan
			term3_vort_tendency_stretching_convergence[0,:] = np.nan
			term4_vort_tendency_tilting = np.nanmean(tilting_p,0)
			term4_vort_tendency_tilting[:,0] = np.nan
			term4_vort_tendency_tilting[0,:] = np.nan

			terms_vort_tendency_sum = term1_vort_tendency_horizontal_advection+term2_vort_tendency_vertical_advection+term3_vort_tendency_stretching_convergence+term4_vort_tendency_tilting

			##################################################################################################################
					

			###################################################################################################################
			#Code to calculate precipitation partitioning (from Michael Fischer)
			hlevs = heightlevs/1000
			import datetime as DT
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

							# Reclassify convective grid points as either shallow, moderate, or deep
							if np.logical_and(ptype[pi,yi,xi] == 3., np.isfinite(sref[pi,yi,xi,levi_ref])):

								# Find maximum height of 20 dBZ echo top:
								if np.nanmax(sref[pi,yi,xi,:]) >= echo_tt:
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
			if ( rmw_2km > 200):	
				slope_rmw_1 = slope_rmw_2 = alpha = vortex_depth_vort = tiltmag_mid_pressure = tiltdir_mid_pressure = tiltmag_mid_vort = tiltdir_mid_vort = tiltmag_deep_pressure = tiltdir_deep_pressure = tiltmag_deep_vort = tiltdir_deep_vort = weakpercent_inner = stratiformpercent_inner = shallowpercent_inner = moderatepercent_inner = deeppercent_inner = weakpercent_outer = stratiformpercent_outer = shallowpercent_outer = moderatepercent_outer = deeppercent_outer = closure_stratiform = closure_shallow = closure_moderate = closure_deep = symmetry_w1_dbz5_p = symmetry_all_dbz5_p = symmetry_w1_vt10_p = symmetry_all_vt10_p = shearmag_2km_5km_local = sheardir_2km_5km_local = shearmag_2km_8km_local = sheardir_2km_8km_local = shearmag_2km_10km_local = sheardir_2km_10km_local = np.nan
			else:
				#Calculate Vortex Depth based on Vt
				vt_ratio = np.nanmax(vt_p_mean,0)/np.nanmax(vt_p_mean[:,4])
				threshold_ratio_vt = 0.5
				if ( np.min(vt_ratio) < 0.5):
					vortex_depth_vt = np.min(heightlevs[vt_ratio < threshold_ratio_vt])/1000
					index_vortex_depth_vt = np.argmin(abs(heightlevs/1000-vortex_depth_vt))
				else:
					vortex_depth_vt = np.nan
				
				#Calculate vortex depth based on vort
				vort_ratio = np.nanmax(vort_p_mean,0)/np.nanmax(vort_p_mean[:,4])
				threshold_ratio_vort = 0.5
				if (np.min(vort_ratio) < 0.5):
					vortex_depth_vort = np.min(heightlevs[np.argwhere(vort_ratio[5::] < threshold_ratio_vort)+5])/1000
					index_vortex_depth_vort = np.argmin(abs(heightlevs/1000-vortex_depth_vort))
					ivd = index_vortex_depth_vort+1	
				else:
					vortex_depth_vort = np.nan
					ivd = 11

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
			
				print('HERE ARE THE THRESHOLDS')
				for k in range(ivd):
					print(threshold_pressure[k],threshold_vort[k])
		
				centroid.centroid(pressure_centroid,center_indices_pressure,threshold_pressure,-1,np.shape(pressure_centroid)[0],np.shape(pressure_centroid)[1],np.shape(pressure_centroid)[2])
				centroid.centroid(vort_centroid,center_indices_vort,threshold_vort,1,np.shape(vort_centroid)[0],np.shape(vort_centroid)[1],np.shape(vort_centroid)[2])
				
				print('HERE ARE THE INDICES')
				for k in range(ivd):
					print(center_indices_vort[k,:])
	
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
				slope_rmw_1 = np.linalg.lstsq((heightlevs[4:21]/1000-heightlevs[4]/1000).reshape(-1,1),(rmw_mean[4:21]-rmw_mean[4]))[0][0]
				slope_rmw_2 = (rmw_mean[20]-rmw_mean[4])/8
			 
				#Alpha Parameter
				if (rmw_2km < 100. and (3*rmw_2km) < rmax):
					alpha = np.log(vt_p_mean_norm[20,4]/vt_p_mean_norm[60,4])/np.log(3)
				else:
					alpha = np.nan
			
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
				rmaxlocal = 102
				rlocal = np.linspace(0,rmaxlocal,(rmaxlocal//resolution+1))
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

		
			vmax = float(maxwind)		
			structurefile = ODIR+'/'+LONGSID.lower()+'.structure_statistics.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.txt'
			f = open(structurefile,'w')
			f.write("%4s, %4.0f, %4.1f, %5.2f, %5.2f, %4.2f, %4.1f, %5.1f, %4.0f, %5.1f, %4.0f, %5.1f, %4.0f, %5.1f, %4.0f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %4.1f, %4.0f, %4.1f, %4.0f, %4.1f, %4.0f" % (FHR,vmax,rmw_2km,slope_rmw_1,slope_rmw_2,alpha,vortex_depth_vort,tiltmag_mid_pressure,tiltdir_mid_pressure,tiltmag_mid_vort,tiltdir_mid_vort,tiltmag_deep_pressure,tiltdir_deep_pressure,tiltmag_deep_vort,tiltdir_deep_vort,weakpercent_inner,stratiformpercent_inner,shallowpercent_inner,moderatepercent_inner,deeppercent_inner,weakpercent_outer,stratiformpercent_outer,shallowpercent_outer,moderatepercent_outer,deeppercent_outer,closure_stratiform,closure_shallow,closure_moderate,closure_deep,symmetry_w1_dbz5_p,symmetry_all_dbz5_p,symmetry_w1_vt10_p,symmetry_all_vt10_p,shearmag_2km_5km_local,sheardir_2km_5km_local,shearmag_2km_8km_local,sheardir_2km_8km_local,shearmag_2km_10km_local,sheardir_2km_10km_local))	
			f.close()

			#############################################################################################################################################
			
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

			color_data_vt_budget = np.genfromtxt('/home/Andrew.Hazelton/python/colormaps/bluewhitered.txt')
			colormap_vt_budget = matplotlib.colors.ListedColormap(color_data_vt_budget)
			levs_vt_budget = np.linspace(-10,10,81,endpoint=True)
			norm_vt_budget = colors.BoundaryNorm(levs_vt_budget,256)

			color_data_vort_budget = np.genfromtxt('/home/Andrew.Hazelton/python/colormaps/bluewhitered.txt')
			colormap_vort_budget = matplotlib.colors.ListedColormap(color_data_vort_budget)
			levs_vort_budget = np.linspace(-40,40,41,endpoint=True)
			norm_vort_budget = colors.BoundaryNorm(levs_vort_budget,256)

			#First do Azimuthal Mean Radial Wind
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(ur_p_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
			plt.grid()
			plt.xlim(0,rmax)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
			cbar.ax.tick_params(labelsize=24)
			plt.xticks(np.linspace(0,rmax,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
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
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(vt_p_mean,1)),levs_vt,cmap=colormap_vt,norm=norm_vt,extend='max')
			plt.grid()
			plt.xlim(0,rmax)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80])
			cbar.ax.tick_params(labelsize=24)
			plt.xticks(np.linspace(0,rmax,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
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
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(w_p_mean,1)),levs_w,cmap=colormap_w,norm=norm_w,extend='both')
			plt.grid()
			plt.xlim(0,rmax)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
			cbar.ax.tick_params(labelsize=24)
			plt.xticks(np.linspace(0,rmax,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
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
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(dbz_p_mean,1)),levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.grid()
			plt.xlim(0,rmax)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
			cbar.ax.tick_params(labelsize=24)
			plt.xticks(np.linspace(0,rmax,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
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
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(rh_p_mean,1)),levs_rh,cmap=colormap_rh,norm=norm_rh,extend='max')
			plt.grid()
			plt.xlim(0,rmax)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
			cbar.ax.tick_params(labelsize=24)
			plt.xticks(np.linspace(0,rmax,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
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
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(dbz_p_downshear_mean,1)),levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.contourf(-r,heightlevs/1000,np.flipud(np.rot90(dbz_p_upshear_mean,1)),levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
			cbar.ax.tick_params(labelsize=24)
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.text(-rmax+50,17,'Upshear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,17,'Downshear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
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
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(ur_p_downshear_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
			plt.contourf(-r,heightlevs/1000,np.flipud(np.rot90(ur_p_upshear_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
			cbar.ax.tick_params(labelsize=24)
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.text(-rmax+50,17,'Upshear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,17,'Downshear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
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
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(w_p_downshear_mean,1)),levs_w,cmap=colormap_w,norm=norm_w,extend='both')
			plt.contourf(-r,heightlevs/1000,np.flipud(np.rot90(w_p_upshear_mean,1)),levs_w,cmap=colormap_w,norm=norm_w,extend='both')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
			cbar.ax.tick_params(labelsize=24)
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.text(-rmax+50,17,'Upshear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,17,'Downshear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
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
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(rh_p_downshear_mean,1)),levs_rh,cmap=colormap_rh,norm=norm_rh,extend='both')
			plt.contourf(-r,heightlevs/1000,np.flipud(np.rot90(rh_p_upshear_mean,1)),levs_rh,cmap=colormap_rh,norm=norm_rh,extend='both')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
			cbar.ax.tick_params(labelsize=24)
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.text(-rmax+50,17,'Upshear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,17,'Downshear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
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
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(dbz_p_rightshear_mean,1)),levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.contourf(-r,heightlevs/1000,np.flipud(np.rot90(dbz_p_leftshear_mean,1)),levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
			cbar.ax.tick_params(labelsize=24)
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.text(-rmax+50,17,'Left of shear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,17,'Right of shear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
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
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(ur_p_rightshear_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
			plt.contourf(-r,heightlevs/1000,np.flipud(np.rot90(ur_p_leftshear_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
			cbar.ax.tick_params(labelsize=24)
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.text(-rmax+50,17,'Left of shear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,17,'Right of shear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
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
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(w_p_rightshear_mean,1)),levs_w,cmap=colormap_w,norm=norm_w,extend='both')
			plt.contourf(-r,heightlevs/1000,np.flipud(np.rot90(w_p_leftshear_mean,1)),levs_w,cmap=colormap_w,norm=norm_w,extend='both')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
			cbar.ax.tick_params(labelsize=24)
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.text(-rmax+50,17,'Left of shear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,17,'Right of shear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
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
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(rh_p_rightshear_mean,1)),levs_rh,cmap=colormap_rh,norm=norm_rh,extend='both')
			plt.contourf(-r,heightlevs/1000,np.flipud(np.rot90(rh_p_leftshear_mean,1)),levs_rh,cmap=colormap_rh,norm=norm_rh,extend='both')
			plt.grid()
			plt.xlim(-rmax,rmax)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
			cbar.ax.tick_params(labelsize=24)
			plt.xticks(np.linspace(-rmax,rmax,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.text(-rmax+50,17,'Left of shear',fontsize=22,horizontalalignment = 'left',style = 'italic', weight = 'bold')
			plt.text(rmax-50,17,'Right of shear',fontsize=22,horizontalalignment = 'right',style = 'italic', weight = 'bold')
			plt.title(EXPT.strip()+'\n'+ r'Across-Shear RH ($\%$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.rh_acrosshear.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.rh_acrosshear.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")

			#Make a plot of wavenumber-0,1,2 components of 5-km reflectivity
			plt.figure()
			plt.gcf().set_size_inches(15,15)

			plt.subplot(221)
			plt.contourf(XI,YI,dbz5_p[:,:],levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
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
			plt.title(EXPT.strip()+'\n'+ r'WV#0,1,2 5-km Reflectivity ($dBZ$, Shading)'+'\n'+'Shear Vector in Black'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
			plt.text(0,rmax/2-50,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

			plt.subplot(222)
			plt.contourf(XI,YI,dbz5_p_w0[:,:],levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
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
			plt.contourf(XI,YI,dbz5_p_w1[:,:],levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
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
			plt.contourf(XI,YI,dbz5_p_w2[:,:],levs_dbz,cmap=colormap_dbz,norm=norm_dbz,extend='max')
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

			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.dbz5km_wavenumber.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.dbz5km_wavenumber.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")

			plt.figure()
			plt.gcf().set_size_inches(15,15)

			plt.subplot(221)
			plt.contourf(XI,YI,rh5_p[:,:],levs_rh,cmap=colormap_rh,norm=norm_rh,extend='both')
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
			plt.title(EXPT.strip()+'\n'+ r'WV#0,1,2 5-km RH ($\%$, Shading)'+'\n'+'Shear Vector in Black'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
			plt.text(0,rmax/2-50,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

			plt.subplot(222)
			plt.contourf(XI,YI,rh5_p_w0[:,:],levs_rh,cmap=colormap_rh,norm=norm_rh,extend='both')
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
			plt.contourf(XI,YI,rh5_p_w1[:,:],np.linspace(-30,30,31),cmap=colormap_rh,norm=colors.BoundaryNorm(np.linspace(-30,30,31),256),extend='both')
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
			plt.contourf(XI,YI,rh5_p_w2[:,:],np.linspace(-30,30,31),cmap=colormap_rh,norm=colors.BoundaryNorm(np.linspace(-30,30,31),256),extend='both')
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

			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.rh5km_wavenumber.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.rh5km_wavenumber.'+forecastinit+'.polar.f'+format(FHR,'03d')
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
			plt.contourf(x_sr*0.54,y_sr*0.54,dbz_2km,levs_dbz,cmap=colormap_dbz,norm=norm_dbz)
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
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.dbz_2km_wind_2km_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.dbz_2km_wind_5km_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
			
			#Begin Block of Research-Mode Plots
			#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
			plt.contourf(x_sr*0.54,y_sr*0.54,ptype[:,:],[0,1,2,3,4,5],colors=['xkcd:white','xkcd:green','xkcd:yellow','xkcd:orange','xkcd:red'],extendfrac='auto')
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
			#plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.dbz_2km_wind_2km_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
			figfname = ODIR+'/'+LONGSID.lower()+'.2km_reflectivity_and_precip_type.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")

			#Plot Tangential Wind Tendency Terms	
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(term1_vt_tendency_mean_radial_flux*1e3,1)),levs_vt_budget,cmap=colormap_vt_budget,norm=norm_vt_budget,extend='both')
			plt.grid()
			plt.xlim(0,200)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
			cbar.ax.tick_params(labelsize=24)
			plt.scatter(rmw_mean[4:20],heightlevs[4:20]/1000,70,'k')
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,200,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+r'$-\langle u_{r} \rangle \langle f+\zeta \rangle$ ($10^{-3} m s^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			figfname = ODIR+'/'+LONGSID.lower()+'.vt_tendency_term1_mean_radial_flux_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");			

			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(term2_vt_tendency_mean_vertical_advection*1e3,1)),levs_vt_budget,cmap=colormap_vt_budget,norm=norm_vt_budget,extend='both')
			plt.grid()
			plt.xlim(0,200)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
			cbar.ax.tick_params(labelsize=24)
			plt.scatter(rmw_mean[4:20],heightlevs[4:20]/1000,70,'k')
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,200,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+r'$-\langle w \rangle \frac{\partial{\langle v_{t} \rangle}}{\partial z}$ ($10^{-3} m s^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			figfname = ODIR+'/'+LONGSID.lower()+'.vt_tendency_term2_mean_vertical_advection_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");
			
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(term3_vt_tendency_eddy_flux*1e3,1)),levs_vt_budget,cmap=colormap_vt_budget,norm=norm_vt_budget,extend='both')
			plt.grid()
			plt.xlim(0,200)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
			cbar.ax.tick_params(labelsize=24)
			plt.scatter(rmw_mean[4:20],heightlevs[4:20]/1000,70,'k')
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,200,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+r'$-\langle u^{\prime}_{r}\zeta^{\prime} \rangle$ ($10^{-3} m s^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			figfname = ODIR+'/'+LONGSID.lower()+'.vt_tendency_term3_eddy_flux_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");			

			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(term4_vt_tendency_vertical_eddy_advection*1e3,1)),levs_vt_budget,cmap=colormap_vt_budget,norm=norm_vt_budget,extend='both')
			plt.grid()
			plt.xlim(0,200)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
			cbar.ax.tick_params(labelsize=24)
			plt.scatter(rmw_mean[4:20],heightlevs[4:20]/1000,70,'k')
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,200,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+r'$-\langle w^{\prime}\frac{\partial{v^{\prime}_{t}}}{\partial z} \rangle$ ($10^{-3} m s^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			figfname = ODIR+'/'+LONGSID.lower()+'.vt_tendency_term4_vertical_eddy_advection_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");			

			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(terms_vt_tendency_sum*1e3,1)),levs_vt_budget,cmap=colormap_vt_budget,norm=norm_vt_budget,extend='both')
			plt.grid()
			plt.xlim(0,200)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
			cbar.ax.tick_params(labelsize=24)
			plt.scatter(rmw_mean[4:20],heightlevs[4:20]/1000,70,'k')
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,200,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+r'Sum of $\frac{\partial{\langle v_{t} \rangle}}{\partial t}$ Terms ($10^{-3} m s^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			figfname = ODIR+'/'+LONGSID.lower()+'.vt_tendency_terms_sum_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");			

			#Plot Vorticity Budget Terms
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(term1_vort_tendency_horizontal_advection*1e5*60,1)),levs_vort_budget,cmap=colormap_vort_budget,norm=norm_vort_budget,extend='both')
			plt.grid()
			plt.xlim(0,200)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
			cbar.ax.tick_params(labelsize=24)
			plt.scatter(rmw_mean[4:20],heightlevs[4:20]/1000,70,'k')
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,200,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+r'$\langle -u_{SR}\frac{\partial{\eta}} {\partial x} - v_{SR}\frac{\partial{\eta}} {\partial y} \rangle$ ($10^{-5} s^{-1} min^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			figfname = ODIR+'/'+LONGSID.lower()+'.vort_tendency_term1_horizontal_advection_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");			

			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(term2_vort_tendency_vertical_advection*1e5*60,1)),levs_vort_budget,cmap=colormap_vort_budget,norm=norm_vort_budget,extend='both')
			plt.grid()
			plt.xlim(0,200)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
			cbar.ax.tick_params(labelsize=24)
			plt.scatter(rmw_mean[4:20],heightlevs[4:20]/1000,70,'k')
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,200,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+r'$\langle -w\frac{\partial{\zeta}} {\partial z} \rangle$ ($10^{-5} s^{-1} min^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			figfname = ODIR+'/'+LONGSID.lower()+'.vort_tendency_term2_vertical_advection_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");			
			
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(term3_vort_tendency_stretching_convergence*1e5*60,1)),levs_vort_budget,cmap=colormap_vort_budget,norm=norm_vort_budget,extend='both')
			plt.grid()
			plt.xlim(0,200)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
			cbar.ax.tick_params(labelsize=24)
			plt.scatter(rmw_mean[4:20],heightlevs[4:20]/1000,70,'k')
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,200,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+r'$\langle -\eta\frac{\partial{u_{SR}}} {\partial x} - \eta\frac{\partial{v_{SR}}} {\partial y} \rangle$ ($10^{-5} s^{-1} min^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			figfname = ODIR+'/'+LONGSID.lower()+'.vort_tendency_term3_stretching_convergence_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");			

			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(term4_vort_tendency_tilting*1e5*60,1)),levs_vort_budget,cmap=colormap_vort_budget,norm=norm_vort_budget,extend='both')
			plt.grid()
			plt.xlim(0,200)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
			cbar.ax.tick_params(labelsize=24)
			plt.scatter(rmw_mean[4:20],heightlevs[4:20]/1000,70,'k')
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,200,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+r'$\langle -\frac{\partial{w}}{\partial x}\frac{\partial{v_{SR}}} {\partial z} + \frac{\partial{w}}{\partial y}\frac{\partial{u_{SR}}} {\partial z} \rangle$ ($10^{-5} s^{-1} min^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			figfname = ODIR+'/'+LONGSID.lower()+'.vort_tendency_term4_tilting_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,heightlevs/1000,np.flipud(np.rot90(terms_vort_tendency_sum*1e5*60,1)),levs_vort_budget,cmap=colormap_vort_budget,norm=norm_vort_budget,extend='both')
			plt.grid()
			plt.xlim(0,200)
			plt.ylim(0,18)
			cbar = plt.colorbar(ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
			cbar.ax.tick_params(labelsize=24)
			plt.scatter(rmw_mean[4:20],heightlevs[4:20]/1000,70,'k')
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,200,11),fontsize=24)
			plt.yticks(np.linspace(0,18,10),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (km)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+r'Sum of $\frac{\partial{\langle \zeta \rangle}}{\partial t}$ Terms ($10^{-5} s^{-1} min^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			figfname = ODIR+'/'+LONGSID.lower()+'.vort_tendency_terms_sum_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

			#Plot PBL Inflow	
			plt.figure()
			plt.gcf().set_size_inches(20.5, 10.5)
			plt.contourf(r,heightlevs_pbl,np.flipud(np.rot90(ur_pbl_p_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
			plt.grid()
			plt.xlim(0,200)
			plt.ylim(0,3000)
			cbar = plt.colorbar(ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
			cbar.ax.tick_params(labelsize=24)
			plt.contour(r,heightlevs_pbl,np.flipud(np.rot90(ur_pbl_p_mean,1)),levels=[0.1*np.nanmin(ur_pbl_p_mean)],colors='w',linewidths=4)
			plt.scatter(rmw_pbl_mean,heightlevs_pbl,70,'k')
			ax1 = plt.axes()
			ax1.yaxis.set_major_formatter(ScalarFormatter())
			ax1.yaxis.set_minor_formatter(plt.NullFormatter())
			plt.xticks(np.linspace(0,200,11),fontsize=24)
			plt.yticks(np.linspace(0,3000,7),fontsize=24)
			plt.xlabel('Radius (km)',fontsize=24)
			plt.ylabel('Height (m)',fontsize=24)
			plt.title(EXPT.strip()+'\n'+r'Radial Wind in PBL ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
			plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
			figfname = ODIR+'/'+LONGSID.lower()+'.ur_pbl_p_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')
			plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
			plt.close()
			plt.close()
			if ( DO_CONVERTGIF ):
				os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");
	
			ga('close 1')

	# Write the input file to a log to mark that it has ben processed
	PLOTTED_FILES=ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
	os.system('echo "'+np.str(FILE)+'" >> '+PLOTTED_FILES)
	os.system('sort -u '+PLOTTED_FILES+' > '+PLOTTED_FILES+'.TMP')
	os.system('mv '+PLOTTED_FILES+'.TMP '+PLOTTED_FILES)

print('DOING THE EXTRA STUFF')
import subprocess
combinedfile = ODIR+'/'+LONGSID.lower()+'.structure_statistics.'+forecastinit+'.polar.all.txt'
pastecmd = 'paste -sd"\\n" '+ODIR+'/'+LONGSID.lower()+'.structure_statistics.'+forecastinit+'.polar.f*.txt'+' > '+combinedfile
print('pastecmd = ',pastecmd)
os.system(pastecmd)
pythonexec = sys.executable
runcmd = pythonexec+' '+PYTHONDIR+'/plot_structure_metrics.py'+' '+combinedfile+' '+EXPT.strip()+' '+ODIR+' '+forecastinit+' '+LONGSID
print('runcmd = ',runcmd)
subprocess.call(runcmd,shell=True)

print('COMPLETING')
os.system('echo "complete" > '+STATUS_FILE)
