#!/usr/bin/env python

# Check that GPLOT_DIR is defined in the environment.
import os, time, warnings
GPLOT_DIR = os.environ['GPLOT_DIR']
print('MSG: Found this GPLOT location --> '+GPLOT_DIR)

#Import necessary modules
print('MSG: Importing Everything Needed')
from datetime import datetime
from py3grads import Grads #This is how we'll get the data
import numpy as np #Used for a lot of the calculations
import metpy
from metpy import interpolate
import metpy.calc as mpcalc
from metpy.units import units

import matplotlib #The plotting routines
matplotlib.use('Agg')
import matplotlib.pyplot as plt #Command for the plotting
import matplotlib.colors as colors #Command to do some colorbar stuff
from matplotlib.axes import Axes

import cartopy.crs as ccrs;
import cartopy.feature as cfeature;
from cartopy.vector_transform import vector_scalar_to_grid
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
import matplotlib.ticker as mticker;

import scipy #Used for interpolation to polar coordinates
from scipy import interpolate #The interpolation function
from matplotlib.ticker import ScalarFormatter #Used to change the log-y-axis ticks
import sys #To change the path 
sys.path.append(GPLOT_DIR+'/sorc/GPLOT/python/modules')
import skewTmodelTCpolar
import shearandrhplot
import centroid
import glob
import math
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
		ODIR = ODIR+'/airsea/'
	else:
		ODIR = ODIR+'/'+EXPT.strip()+'/'+IDATE.strip()+'/airsea/'
	
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
	zsize_pressure = int(LEVS)
	
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
		forecastinit = ATCF_DATA[list(FHRIND),2][0]
		maxwind = ATCF_DATA[list(FHRIND),8][0]
		minpressure = ATCF_DATA[list(FHRIND),9][0]
		rmwnmi = ATCF_DATA[list(FHRIND),19][0]
	
		if ( centerlat > 50.0):
			# Write the input file to a log to mark that it has ben processed
			PLOTTED_FILES=ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
			os.system('echo "'+str(FILE)+'" >> '+PLOTTED_FILES)
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
				ga(f'set z 1 {zsize_pressure}')
				levs = ga.exp('lev')
				z = np.zeros((zsize_pressure,0))*np.nan
				for i in range(zsize_pressure):  z[i] = levs[1,1,i]
	
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
				lhtflx = ga.exp('lhtflsfc')[...,0]
				shtflx = ga.exp('shtflsfc')[...,0]
				dlwflx = ga.exp('dlwrfavesfc')[...,0]
				ulwflx = ga.exp('ulwrfavesfc')[...,0]
				dswflx = ga.exp('dswrfavesfc')[...,0]
				uswflx = ga.exp('uswrfavesfc')[...,0]
				print('MSG: Done with [ls]htflx, [du][sl]wrfavesfc')
				
				#Get 2-d Data
				ga('set z 1')
				u10 = ga.exp('ugrd10m')
				v10 = ga.exp('vgrd10m')
				if DSOURCE == 'HAFS':
					mslp = ga.exp('msletmsl')
				else:
					mslp = ga.exp('prmslmsl')
				tmp2m = ga.exp('tmp2m')
				q2m = ga.exp('spfh2m')
				rh2m = ga.exp('rh2m')
				print('MSG: Done with u10,v10,mslp,tmp2m,q2m')
				mixr2m = q2m/(1-q2m)
				temp_v_2m = tmp2m*(1+0.61*mixr2m)
				rho2m = mslp/(287*temp_v_2m)
				print(f'MSG: Done with surface vars (e.g., u10,v10) {datetime.now()}')
				
				#Get u850, v850, u200, v200 for Shear Calculation
				ga('set lev 850')
				u850 = ga.exp('ugrdprs')
				v850 = ga.exp('vgrdprs')
				z850 = ga.exp('hgtprs')
				ga('set lev 200')
				u200 = ga.exp('ugrdprs')
				v200 = ga.exp('vgrdprs')
				z200 = ga.exp('hgtprs')
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
                                
				rnorm = np.linspace(0,6,121)
				Rnorm, THETAnorm = np.meshgrid(rnorm,theta)
				XInorm = Rnorm * np.cos(THETAnorm)
				YInorm = Rnorm * np.sin(THETAnorm)
				
				#Make Plots
				print(f'MSG: Doing Plots Now {datetime.now()}')
				if os.path.exists(f'{NMLDIR}/namelist.airsea.structure.{EXPT}'):
					namelist_structure_vars = np.genfromtxt(f'{NMLDIR}/namelist.airsea.pbl.{EXPT}',delimiter=',',dtype='str')
				else:
					namelist_structure_vars = np.genfromtxt(f'{NMLDIR}/namelist.airsea.pbl',delimiter=',',dtype='str')
				do_turb_flux = namelist_structure_vars[0,1]
				do_total_flux = namelist_structure_vars[1,1]
				do_theta_e_550 = namelist_structure_vars[2,1]
				do_theta_e_700 = namelist_structure_vars[3,1]
				do_theta_e_850 = namelist_structure_vars[4,1]
				
				#Load the colormaps needed
				color_data_vt = np.genfromtxt(GPLOT_DIR+'/sorc/GPLOT/python/colormaps/colormap_wind.txt')
				colormap_vt = matplotlib.colors.ListedColormap(color_data_vt)
				levs_vt = np.linspace(0,80,41,endpoint=True)
				norm_vt = colors.BoundaryNorm(levs_vt,256)
				
				#color_data_th = np.genfromtxt(GPLOT_DIR+'/sorc/GPLOT/python/colormaps/bluewhitered.txt')
				color_data_th = np.genfromtxt(GPLOT_DIR+'/sorc/GPLOT/python/colormaps/colormap_wind.txt')
				colormap_th = matplotlib.colors.ListedColormap(color_data_th)
				levs_th = np.linspace(350,380,31,endpoint=True)
				norm_th = colors.BoundaryNorm(levs_th,256)
				
				#turb_flux_levs = np.linspace(-50,1350,15,endpoint=True)
				turb_flux_levs = np.arange(-400,1400+1e-6,50.0);	turb_flux_ticks = np.arange(-400,1400+1e-6,100.0)
				total_flux_levs = np.arange(-500,2000+1e-6,50.0);	total_flux_ticks = np.arange(-500,2000+1e-6,100.0);
				theta_e_550_levs = np.arange(330,380+1e-6,2.0);		theta_e_550_ticks = np.arange(330,380+1e-6,5.0)
				theta_e_700_levs = np.arange(330,380+1e-6,2.0);		theta_e_700_ticks = np.arange(330,380+1e-6,5.0)
				theta_e_850_levs = np.arange(330,380+1e-6,2.0);		theta_e_850_ticks = np.arange(330,380+1e-6,5.0)
				
				DPT = mpcalc.dewpoint_from_specific_humidity(q*metpy.units.units("kg/kg"),\
                                                                             temp*metpy.units.units.K,\
                                                                             levs*metpy.units.units.hPa)
                                                                             #levs[...,0:37]*metpy.units.units.hPa)
				THETA_E = mpcalc.equivalent_potential_temperature(levs*metpy.units.units.hPa,\
                                                                                  temp*metpy.units.units.K,DPT);
                                # Streamplots require equally spaced x and y
				xi = np.linspace(lon.min(),lon.max(),lon.shape[0]);
				yi = np.linspace(lat.min(),lat.max(),lat.shape[0]);
				
				figsize = (24,24);
				fontsize = 24
				small_fontsize = 24
				
				# FIGURE: Total turbulent heat flux (enthalpy flux) at the sea surface
				if do_turb_flux == 'Y':
					turb_flux = lhtflx + shtflx;
					
					fig1 = plt.figure(figsize=figsize)
					ax1 = fig1.add_subplot(1, 1, 1)
					co1 = ax1.contourf(lon,lat, turb_flux, levels=turb_flux_levs, extend='both')
					# ax1 = axes_radhgt(ax1, rmax, 0)
					cbar1 = plt.colorbar(co1, ticks=turb_flux_ticks)
					cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
					Axes.streamplot(ax1,xi,yi,u10,v10,color='gray',density=0.5);
					ax1.set_title(EXPT.strip()+'\n'+ r'Enthalpy Fluxes ($W\ m^{-2}$, Shading), U$_{10m}$ ($m\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
					ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
					figfname = ODIR+'/'+LONGSID.lower()+'.turb_flux.'+forecastinit+'.airsea.f'+format(FHR,'03d')
					fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
					plt.close(fig1)
				# FIGURE: Total net heat flux (turbulent+radiative) at the sea surface
				if do_total_flux == 'Y':
					total_flux = lhtflx + shtflx - dlwflx + ulwflx - dswflx + uswflx;
					
					fig1 = plt.figure(figsize=figsize)
					ax1 = fig1.add_subplot(1, 1, 1)
					co1 = ax1.contourf(lon,lat, total_flux, levels=total_flux_levs, extend='both')
					# ax1 = axes_radhgt(ax1, rmax, 0)
					cbar1 = plt.colorbar(co1, ticks=total_flux_ticks)
					cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
					Axes.streamplot(ax1,xi,yi,u10,v10,color='gray',density=0.5);
					ax1.set_title(EXPT.strip()+'\n'+ r'Sfc. Ht. Fluxes ($W\ m^{-2}$, Shading), U$_{10m}$ ($m\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
					ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
					figfname = ODIR+'/'+LONGSID.lower()+'.total_flux.'+forecastinit+'.airsea.f'+format(FHR,'03d')
					fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
					plt.close(fig1)
				# FIGURE: Equivalent potential temperature from 550 to 700 hPa
				if do_theta_e_550 == 'Y':
					THETA_E_550 = np.nanmean(np.where((550<=levs) & (levs<700), THETA_E, np.nan),axis=2);
					uwind_550 = np.nanmean(np.where((550<=levs) & (levs<700), uwind, np.nan),axis=2);
					vwind_550 = np.nanmean(np.where((550<=levs) & (levs<700), vwind, np.nan),axis=2);
					
					fig1 = plt.figure(figsize=figsize)
					ax1 = fig1.add_subplot(1, 1, 1)
					co1 = ax1.contourf(lon,lat, THETA_E_550, levels=theta_e_550_levs, extend='both')
					# ax1 = axes_radhgt(ax1, rmax, 0)
					cbar1 = plt.colorbar(co1, ticks=theta_e_550_ticks)
					cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
					Axes.streamplot(ax1,xi,yi,uwind_550,vwind_550,color='gray',density=0.5);
					ax1.set_title(EXPT.strip()+'\n'+ r'550 hPa Equiv. Pot. Temp. (K, Shading), Wind ($m\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
					ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
					figfname = ODIR+'/'+LONGSID.lower()+'.theta_e_550.'+forecastinit+'.airsea.f'+format(FHR,'03d')
					fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
					plt.close(fig1)
				# FIGURE: Equivalent potential temperature from 700 to 850 hPa
				if do_theta_e_700 == 'Y':
					THETA_E_700 = np.nanmean(np.where((700<=levs) & (levs<850), THETA_E, np.nan),axis=2);
					uwind_700 = np.nanmean(np.where((700<=levs) & (levs<850), uwind, np.nan),axis=2);
					vwind_700 = np.nanmean(np.where((700<=levs) & (levs<850), vwind, np.nan),axis=2);
					
					fig1 = plt.figure(figsize=figsize)
					ax1 = fig1.add_subplot(1, 1, 1)
					co1 = ax1.contourf(lon,lat, THETA_E_700, levels=theta_e_700_levs, extend='both')
					#cbar1 = plt.colorbar(co1, ticks=np.linspace(350,380,7,endpoint=True))
					cbar1 = plt.colorbar(co1, ticks=theta_e_700_ticks)
					cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
					Axes.streamplot(ax1,xi,yi,uwind_700,vwind_700,color='gray',density=0.5);
					ax1.set_title(EXPT.strip()+'\n'+ r'700 hPa Equiv. Pot. Temp. (K, Shading), Wind ($m\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
					ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
					figfname = ODIR+'/'+LONGSID.lower()+'.theta_e_700.'+forecastinit+'.airsea.f'+format(FHR,'03d')
					fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
					plt.close(fig1)
				# FIGURE: Equivalent potential temperature below 850 hPa
				if do_theta_e_850 == 'Y':
					THETA_E_850 = np.nanmean(np.where((850<=levs), THETA_E, np.nan),axis=2);
					print(np.nanmin(THETA_E), np.nanmax(THETA_E), np.nanmin(THETA_E_550), np.nanmax(THETA_E_550), np.nanmin(THETA_E_700), np.nanmax(THETA_E_700), np.nanmin(THETA_E_850), np.nanmax(THETA_E_850), int(maxwind));
					uwind_850 = np.nanmean(np.where((850<=levs), uwind, np.nan),axis=2);
					vwind_850 = np.nanmean(np.where((850<=levs), vwind, np.nan),axis=2);
					
					fig1 = plt.figure(figsize=figsize)
					ax1 = fig1.add_subplot(1, 1, 1)
					# #co1 = ax1.contourf(lon,lat, THETA_E, levs_th, \
					# # co1 = ax1.contourf(lon,lat, THETA_E_850, \
					# # 		  cmap=colormap_th, norm=norm_th, transform=ccrs.PlateCarree(), extend='both')
					#co1 = ax1.contourf(lon,lat, THETA_E_850, cmap=colormap_th, norm=norm_th, extend='both')
					co1 = ax1.contourf(lon,lat, THETA_E_850, levels=theta_e_850_levs, extend='both')
					# ax1 = axes_radhgt(ax1, rmax, 0)
					cbar1 = plt.colorbar(co1, ticks=theta_e_850_ticks)
					cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
					Axes.streamplot(ax1,xi,yi,uwind_850,vwind_850,color='gray',density=0.5);
					ax1.set_title(EXPT.strip()+'\n'+ r'850 hPa Equiv. Pot. Temp. (K, Shading), Wind ($m\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
					ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
					figfname = ODIR+'/'+LONGSID.lower()+'.theta_e_850.'+forecastinit+'.airsea.f'+format(FHR,'03d')
					fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
					if ( DO_CONVERTGIF ):
						os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
					plt.close(fig1)
	
	
				# Close the GrADs control file
				ga('close 1')
	

			
		# Write the input file to a log to mark that it has ben processed
		update_plottedfile(ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log', FILE)
	
	print('MSG: COMPLETING')
	os.system('lockfile -r-1 -l 180 '+ST_LOCK_FILE)
	os.system('echo "complete" > '+STATUS_FILE)
	os.system('rm -f '+ST_LOCK_FILE)


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
