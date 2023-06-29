#!/usr/bin/env python

# Check that GPLOT_DIR is defined in the environment.
import os, time, warnings
GPLOT_DIR = os.environ['GPLOT_DIR']
print('MSG: Found this GPLOT location --> '+GPLOT_DIR)

#Import necessary modules
print('MSG: Importing Everything Needed')
from datetime import datetime
# from py3grads import Grads #This is how we'll get the data
import numpy as np #Used for a lot of the calculations
import numpy.ma as ma
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
import struct;
import sys #To change the path 
import xarray as xr

#import modules.skewTmodelTCpolar as skewTmodelTCpolar
#import modules.shearandrhplot as shearandrhplot
#import modules.interp as interp
import modules.io as io
#import modules.plotting as plotting
#import modules.multiprocess as mproc
import glob
import math
import cmath
import subprocess
from mpl_toolkits.axes_grid1 import make_axes_locatable


def debug_dump_range(FHR,varnm,var):
  #print(f'DEBUG: FHR {int(FHR)}: {varnm} in {np.nanmin(var.values)},{np.nanpercentile(var.values,25)},{np.nanmedian(var.values)},{np.nanpercentile(var.values,75)},{np.nanmax(var.values)}');
  pass;

def add_center_label(ax1,centerlon,centerlat,minpressure):
  ax1.text(centerlon,centerlat,f'{minpressure}\n  L',color='black',fontsize=28,fontweight='extra bold');
  ax1.text(centerlon,centerlat,f'{minpressure}\n  L',color='red',fontsize=28);

#def read_fix_hycom_depth(domnm='htrop',idm=None,jdm=None,hafs_fix_idir=default_fix_idir,ddir=default_ddir):
def read_fix_hycom_depth(basefname):
  '''Read HYCOM DEPTH (model bathymetry) fix file.'''
  lines=[line.rstrip() for line in open(basefname+'.b')]
  rangelines = [line.split() for line in lines if 'i/jdm = ' in line];
  idm = int(rangelines[0][2])
  jdm = int(rangelines[0][3].split(';')[0])
  fname = basefname+'.a';
  ijdm=idm*jdm
  #DEBUG:    print(f'Loading {fname} at {datetime.datetime.now()}');
  res = ma.array([],fill_value=1e30);
  fid=open(fname,'rb')
  fld=fid.read(ijdm*4)
  fid.close()
  fld=struct.unpack('>'+str(ijdm)+'f',fld)
  fld=np.array(fld)
  fld=ma.reshape(fld,(jdm,idm))
  res=fld.copy()
  res=ma.masked_greater(res,1e5)
  return(res)
#read_fix_hycom_depth


##############################
def main():

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
  # DOMAIN = sys.argv[3]
  # if DOMAIN == 'MISSING':
  #   DOMAIN = ''
  OCEAN_DOMAIN = sys.argv[3]
  if OCEAN_DOMAIN == 'MISSING':
    OCEAN_DOMAIN = ''
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
  OCEAN_SOURCE = sys.argv[11]
  if OCEAN_SOURCE == 'MISSING' or OCEAN_SOURCE == '':
    OCEAN_SOURCE = 'HYCOM'
  OCEAN_CFG = sys.argv[12]
  if OCEAN_CFG == 'MISSING' or OCEAN_CFG == '':
    OCEAN_CFG = 'NHC'
  FIX_DIR = sys.argv[13]
  if FIX_DIR == 'MISSING' or FIX_DIR == '':
    FIX_DIR = GPLOT_DIR+'/fix'
  OCEAN_WRAP_LON = sys.argv[14]
  if OCEAN_WRAP_LON == 'MISSING' or OCEAN_WRAP_LON == '':
    OCEAN_WRAP_LON = 'False'
  
  
  # Read the master namelist
  DSOURCE = subprocess.run(['grep','^DSOURCE',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1]
  #OCEAN_DSOURCE = subprocess.run(['grep','^OCEAN_DSOURCE',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1]
  EXPT = subprocess.run(['grep','^EXPT',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1]
  ODIR = subprocess.run(['grep','^ODIR =',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1].strip()
  try:
    ODIR_TYPE = int(subprocess.run(['grep','^ODIR_TYPE',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1])
  except:
    ODIR_TYPE = 0
  if ODIR_TYPE == 1:
    ODIR = ODIR+'/ocean_'+OCEAN_DOMAIN+'/'
  else:
    ODIR = ODIR+'/'+EXPT.strip()+'/'+IDATE.strip()+'/ocean_'+OCEAN_DOMAIN+'/'

  figext = '.png'
  try:
    DO_CONVERTGIF = subprocess.run(['grep','^DO_CONVERTGIF',MASTER_NML_IN], stdout=subprocess.PIPE).stdout.decode('utf-8').split(" = ")[1].strip()
    DO_CONVERTGIF = (DO_CONVERTGIF == 'True')
    figext2 = '.gif'
  except:
    DO_CONVERTGIF = False
    figext2 = '.png'
  
  # Define some important file names
  UNPLOTTED_FILE = ODIR.strip()+'UnplottedOceanFiles.'+OCEAN_DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
  PLOTTED_FILE = ODIR.strip()+'PlottedOceanFiles.'+OCEAN_DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
  ALLFHR_FILE = ODIR.strip()+'AllForecastHours.'+OCEAN_DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
  STATUS_FILE = ODIR.strip()+'status.'+OCEAN_DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
  ST_LOCK_FILE = ODIR.strip()+'status.'+OCEAN_DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log.lock'
  ATCF_FILE = ODIR.strip()+'ATCF_FILES.dat'
  
  print(f'DEBUG: OCEAN_SOURCE {OCEAN_SOURCE}')
  # Get ocean depths (for some reason, generally left out of OCN_POST output files)
  if ( OCEAN_SOURCE == 'HYCOM' ):
    DEPTH_FILE = FIX_DIR.strip()+DSOURCE.strip().lower()+'_'+OCEAN_SOURCE.strip().lower()+'_'+OCEAN_CFG.strip().lower()+'.basin.regional.depth'
    depths = read_fix_hycom_depth(DEPTH_FILE);
  else:
    DEPTH_FILE = f'{FIX_DIR.strip()}/{OCEAN_CFG.strip().lower()}/ocean_topog.nc';
    depths_ds = xr.open_dataset(DEPTH_FILE);
    depths = depths_ds.depth.values
    depths_ds.close();
  #DEBUG:  print(f'DEBUG:: DEPTH_FILE={DEPTH_FILE}, shape={depths.shape}');
  
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
  
  SSH0 = None
  dSSH = None
  
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
    #trey additions start
    neq34 = float(ATCF_DATA[list(FHRIND),13][0]) 
    seq34 = float(ATCF_DATA[list(FHRIND),14][0]) 
    swq34 = float(ATCF_DATA[list(FHRIND),15][0]) 
    nwq34 = float(ATCF_DATA[list(FHRIND),16][0])
    #treyend
    # HACK: This should be revisited.
    if centerlat > 50.0:
      print('WARNING: The latitude is poleward of +/- 50. Skipping.')
      # Write the input file to a log to mark that it has ben processed
      io.update_plottedfile(PLOTTED_FILE, FILE)
      continue

    print(f'MSG: Searching for graphics products that match --> {ODIR}/*{LONGSID.lower()}*f{FHR:03}{figext2}')
    figuretest = np.shape([g for g in glob.glob(f"{ODIR}/*{LONGSID.lower()}*f{format(FHR,'03d')}{figext2}")])[0]
    if figuretest > 0:
      print(f'MSG: Found {figuretest} matching graphical products for this lead time.')
      print(f'MSG: Please delete all {figext2} files for this lead time to reproduce graphics. Skipping.')

      # Write the input file to a log to mark that it has ben processed
      io.update_plottedfile(PLOTTED_FILE, FILE)
      continue

    print(f'MSG: I can\'t find the graphical products for this lead time (figuretest={figuretest}). Proceeding.')
    #print('h = ',list(FHRIND))
  
    # Check that the data file 'FILE' exists
    gribfiletest = os.system('ls '+FILE)
    if gribfiletest > 0:
      print(f'MSG: The input file does not exist. Nothing to do. Skipping.')
      continue

    if ( OCEAN_DOMAIN == 'd03' ):
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
      lonmax = centerlon + xoffset
      lonmin = centerlon - xoffset
      latmax = centerlat + yoffset
      latmin = centerlat - yoffset

      # # Ocean data has longitudes -180 -> +180
      # lonmin = lonmin - 360
      # lonmax = lonmax - 360

    #Get data
    if ( OCEAN_DOMAIN == 'd03' ):
      print('MSG: Getting Data Now. Using an xoffset of '+str(xoffset)+' degrees')
    else:
      print('MSG: Getting Data Now.')
    all_ds = xr.open_dataset(FILE);
    if ( OCEAN_SOURCE == 'HYCOM' ):
      if ( OCEAN_WRAP_LON == 'True' ):
        print('MSG: Wrapping 3D Longitudes');
        all_ds['Longitude'] = all_ds.Longitude + 360
      ds = all_ds.where(~depths.mask);
      if ( OCEAN_DOMAIN == 'd03' ):
        #DEBUG:        print(f'DEBUG: ds.where( {lonmin}<={all_ds.Longitude.min().values} & {all_ds.Longitude.max().values}<={lonmax} & {latmin}<={all_ds.Latitude.min().values} & {all_ds.Latitude.max().values}<={latmax} )');
        ds = ds.where((lonmin<=all_ds.Longitude) & (all_ds.Longitude<=lonmax) & (latmin<=all_ds.Latitude) & (all_ds.Latitude<=latmax));
      lon = ds.Longitude.squeeze();
      lat = ds.Latitude.squeeze();
      ucurr = ds.u_velocity.squeeze()
      vcurr = ds.v_velocity.squeeze()
      wcurr = ds.w_velocity.squeeze()
      print('MSG: Done With u,v,w')
      MLD = ds.mixed_layer_thickness.squeeze()
      #DEBUG:        MLD.plot(figsize=(11,9)); plt.savefig('3z.png');
      T = ds.temperature.squeeze()
      S = ds.salinity.squeeze()
      SSS = S[0,...].squeeze();
      OHC = ds.ocean_heat_content.squeeze()
      #i26 = ds['depth of 26C isotherm'].squeeze().to_masked_array()
      i26 = ds['depth of 26C isotherm'].squeeze()
      i20 = ds['depth of 20C isotherm'].squeeze()
      ds.close()
      print('MSG: Done with T, S, MLD, OHC, Iso')
      
      dZ = ds.Z.diff(dim='Z');
      MLu = ucurr.where(ds.Z < MLD).mean(axis=0)
      MLv = vcurr.where(ds.Z < MLD).mean(axis=0)
      MLT = T.where(ds.Z < MLD).mean(axis=0)
      MLS = S.where(ds.Z < MLD).mean(axis=0)
      print('MSG: Done with MLu, MLv, MLT, MLS')
      
      #Get 2-d Data
      oFILE = FILE.replace('3z','2d')
      ofiletest = os.system('ls '+oFILE)
      SHF = None;
      SSH = None;
      if (ofiletest < 1):
        all_ods = xr.open_dataset(oFILE)
        if ( OCEAN_WRAP_LON == 'True' ):
          print('MSG: Wrapping 2D Longitudes');
          all_ods['Longitude'] = all_ods.Longitude + 360
        ods = all_ods.where(~depths.mask);
        if ( OCEAN_DOMAIN == 'd03' ):
          #DEBUG:          print(f'DEBUG: ods.where( {lonmin}<={all_ods.Longitude.min().values} & {all_ods.Longitude.max().values}<={lonmax} & {latmin}<={all_ods.Latitude.min().values} & {all_ods.Latitude.max().values}<={latmax} )');
          ods = ods.where((lonmin<=all_ods.Longitude) & (all_ods.Longitude<=lonmax) \
                & (latmin<=all_ods.Latitude) & (all_ods.Latitude<=latmax));
        SHF = ods.surface_heat_flux.squeeze()
        #DEBUG:        print('SHF',SHF.min().values,SHF.max().values)
        SSH = ods.sea_surface_height.squeeze()
        if ( SSH0 is None ):
          SSH0 = SSH
        dSSH = (SSH - SSH0) / (np.double(FHR)/24) #[cm/d]
        #DEBUG:          print('SSH',SSH.max().values)
        Mon = ods.montgomery_potential_surf.squeeze()
        # MLu = ods.mixed_layer_u_velocity.squeeze()
        # MLv = ods.mixed_layer_v_velocity.squeeze()
        # MLD = ods.mixed_layer_thickness.squeeze()
        # MLD.plot(figsize=(11,9)); plt.savefig('2d.png');
        # MLT = ods.mixed_layer_temperature.squeeze()
        # MLS = ods.mixed_layer_salinity.squeeze()
        ods.barotropic_u_velocity.squeeze()
        ods.barotropic_v_velocity.squeeze()
        ods.close()
        print(f'MSG: Done with surface vars (e.g., redo of MLu,MLv) {datetime.now()}')
    
    elif ( OCEAN_SOURCE == 'MOM6' ):
      all_ds = all_ds.interp( {'xq':all_ds.xh, 'yq':all_ds.yh} )
      if ( OCEAN_WRAP_LON == 'True' ):
        print('MSG: Wrapping 3D Longitudes');
        all_ds['xh'] = all_ds.xh + 360
        all_ds['geolon'] = all_ds.geolon + 360
      #ds = all_ds.where(~depths.mask);
      ds = all_ds;
      if ( OCEAN_DOMAIN == 'd03' ):
        #DEBUG:        print(f'DEBUG: ds.where( {lonmin}<={all_ds.geolon.min().values} & {all_ds.geolon.max().values}<={lonmax} & {latmin}<={all_ds.geolat.min().values} & {all_ds.geolat.max().values}<={latmax} )');
        #ds = ds.where((lonmin<=all_ds.geolon) & (all_ds.geolon<=lonmax) & (latmin<=all_ds.geolat) & (all_ds.geolat<=latmax));
        ds = ds.where((lonmin<=all_ds.xh) & (all_ds.xh<=lonmax) & (latmin<=all_ds.yh) & (all_ds.yh<=latmax));
      #lon = ds.geolon.squeeze();
      #lat = ds.geolat.squeeze();
      lon = ds.xh.squeeze();
      lat = ds.yh.squeeze();
      # ucurr = ds.uo.squeeze().interp( {'xq':ds.xh, 'yh':ds.yh} )
      # vcurr = ds.vo.squeeze().interp( {'xh':ds.xh, 'yq':ds.yh} )
      ucurr = ds.uo.squeeze()
      vcurr = ds.vo.squeeze()
      #wcurr = ds.w_velocity.squeeze()
      print('MSG: Done With u,v,w')
      #MLD = ds.MLD_003.squeeze()
      MLD = ds.MLD_0125.squeeze()
      #DEBUG:        MLD.plot(figsize=(11,9)); plt.savefig('3z.png');
      T = ds.temp.squeeze()
      S = ds.so.squeeze()
      SSS = S[0,...].squeeze();
      #i26 = ds['depth of 26C isotherm'].squeeze().to_masked_array()
      i26 = ds.z_l.where(T>=26).max(axis=0)
      i20 = ds.z_l.where(T>=20).max(axis=0)
      cp = 4178;                  # Specific heat capacity of seawater [J kg^-1 K^-1]
      rho = 1026;                 # Mean water density [kg m^-3]
      kJcm2_per_Jm2 = 1e-7;       # Unit conversion kJ/cm^2 == 10^7 J/m^2
      delT = T - 26;
      delT = xr.where(delT > 0, delT, 0);
      dZ = ds.z_l.broadcast_like(delT).diff(0)
      delTdz = delT * dZ;
      OHC = cp*rho*delTdz.sum(axis=0) * kJcm2_per_Jm2;
      print('MSG: Done with T, S, MLD, Iso, OHC')
      
      MLu = ucurr.where(ds.z_l < MLD).mean(axis=0)
      MLv = vcurr.where(ds.z_l < MLD).mean(axis=0)
      MLT = T.where(ds.z_l < MLD).mean(axis=0)
      MLS = S.where(ds.z_l < MLD).mean(axis=0)
      print('MSG: Done with MLu, MLv, MLT, MLS')
      
      #Get 2-d Data
      SHF = -(ds.LwLatSens.squeeze() + ds.SW.squeeze());
      #DEBUG:      print('SHF',SHF.min().values,SHF.max().values)
      SSH = ds.SSH.squeeze()*1e2 #[m]=>[cm]
      if ( SSH0 is None ):
        SSH0 = SSH
      dSSH = (SSH - SSH0) / (np.double(FHR)/24) #[cm/d]
      #DEBUG:      print('SSH',SSH.max().values)
      ds.close()
      print(f'MSG: Done with surface vars (e.g., redo of MLu,MLv) {datetime.now()}')
    
    else:
        print(f'ERROR: OCEAN_SOURCE {OCEAN_SOURCE} not yet handled by PLOT_OCEAN_MAPS.py!')
        sys.exit(1)
    
    lonstretch = np.cos(np.deg2rad(lon.mean()));
    secPerDay = 24*3600
    
    #Make Plots
    print(f'MSG: Doing Plots Now {datetime.now()}')
    if os.path.exists(f'{NMLDIR}/namelist.ocean.structure.{EXPT}'):
      namelist_structure_vars = np.genfromtxt(f'{NMLDIR}/namelist.ocean_maps.{EXPT}',delimiter=',',dtype='str')
    else:
      namelist_structure_vars = np.genfromtxt(f'{NMLDIR}/namelist.ocean_maps',delimiter=',',dtype='str')
    do_iso_26 = namelist_structure_vars[0,1]
    do_iso_20 = namelist_structure_vars[1,1]
    do_ohc = namelist_structure_vars[2,1]
    do_sfc_conv = namelist_structure_vars[3,1]
    do_sfc_vort = namelist_structure_vars[4,1]
    do_ml_conv = namelist_structure_vars[5,1]
    do_ml_vort = namelist_structure_vars[6,1]
    do_ssh = namelist_structure_vars[7,1]
    do_shf = namelist_structure_vars[8,1]
    do_dpi = namelist_structure_vars[9,1]
    do_mld = namelist_structure_vars[10,1]
    do_sss = namelist_structure_vars[11,1]
    do_mlt = namelist_structure_vars[12,1]
    do_mls = namelist_structure_vars[13,1]
    do_ssh_tendency = namelist_structure_vars[14,1]
    do_ships_output = namelist_structure_vars[15,1]
    
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
    
    if ( OCEAN_SOURCE == 'HYCOM' ):
      ml_u_x = MLu.differentiate('Longitude') / 1e2 / (lonstretch*111e3) # [cm/s/degree] => [1/s]
      ml_u_y = MLu.differentiate('Latitude') / 1e2 / 111e3
      ml_v_x = MLv.differentiate('Longitude') / 1e2 / (lonstretch*111e3)
      ml_v_y = MLv.differentiate('Latitude') / 1e2 / 111e3
      sfc_u_x = ucurr[1,:].differentiate('Longitude') / 1e2 / (lonstretch*111e3) # [cm/s/degree] => [1/s]
      sfc_u_y = ucurr[1,:].differentiate('Latitude') / 1e2 / 111e3
      sfc_v_x = vcurr[1,:].differentiate('Longitude') / 1e2 / (lonstretch*111e3)
      sfc_v_y = vcurr[1,:].differentiate('Latitude') / 1e2 / 111e3
      
      ml_conv = -(ml_u_x + ml_v_y)*secPerDay;    # CONvergence on model layer
      ml_vort = (ml_v_x - ml_u_y)*secPerDay;     # Relative vorticity on model layer
      sfc_conv = -(sfc_u_x + sfc_v_y)*secPerDay;    # CONvergence on model layer
      sfc_vort = (sfc_v_x - sfc_u_y)*secPerDay;     # Relative vorticity on model layer
      
      iso_26_levs = np.arange(0,160+1e-6,5.0);		iso_26_ticks = np.arange(0,160+1e-6,20.0)
      iso_20_levs = np.arange(0,300+1e-6,10.0);		iso_20_ticks = np.arange(0,300+1e-6,20.0)
      OHC_levs = np.arange(0,150+1e-6,5.0);		OHC_ticks = np.arange(0,150+1e-6,20.0);
      sfc_conv_levs = np.arange(-20,20+1e-8,0.5);	sfc_conv_ticks = np.arange(-20,20+1e-8,2)
      sfc_vort_levs = np.arange(-20,20+1e-8,0.5);	sfc_vort_ticks = np.arange(-20,20+1e-8,2)
      ml_conv_levs = np.arange(-20,20+1e-8,0.5);	ml_conv_ticks = np.arange(-20,20+1e-8,2)
      ml_vort_levs = np.arange(-20,20+1e-8,0.5);	ml_vort_ticks = np.arange(-20,20+1e-8,2)
      SSH_levs = np.arange(-100,100+1e-6,5.0);		SSH_ticks = np.arange(-100,100+1e-6,20.0)
      dSSH_levs = np.arange(-20,20+1e-6,1.0);		dSSH_ticks = np.arange(-20,20+1e-6,5.0)
      SHF_levs = np.arange(-1600,1600+1e-6,50.0);	SHF_ticks = np.arange(-1600,1600+1e-6,200.0)
      DPI_levs = np.arange(0,30+1e-6,1.0);		DPI_ticks = np.arange(0,30+1e-6,2.0)
      MLD_levs = np.arange(0,160+1e-6,5.0);		MLD_ticks = np.arange(0,160+1e-6,10.0)
      SSS_levs = np.arange(32,38+1e-6,0.2);		SSS_ticks = np.arange(32,38+1e-6,0.5)
      MLT_levs = np.arange(26,30+1e-6,0.2);		MLT_ticks = np.arange(26,30+1e-6,0.5)
      MLS_levs = np.arange(32,38+1e-6,0.2);		MLS_ticks = np.arange(32,38+1e-6,0.5)
    
    elif ( OCEAN_SOURCE == 'MOM6' ):
      ml_u_x = MLu.differentiate('xh') / 1e2 / (lonstretch*111e3) # [cm/s/degree] => [1/s]
      ml_u_y = MLu.differentiate('yh') / 1e2 / 111e3
      ml_v_x = MLv.differentiate('xh') / 1e2 / (lonstretch*111e3)
      ml_v_y = MLv.differentiate('yh') / 1e2 / 111e3
      sfc_u_x = ucurr[1,:].differentiate('xh') / 1e2 / (lonstretch*111e3) # [cm/s/degree] => [1/s]
      sfc_u_y = ucurr[1,:].differentiate('yh') / 1e2 / 111e3
      sfc_v_x = vcurr[1,:].differentiate('xh') / 1e2 / (lonstretch*111e3)
      sfc_v_y = vcurr[1,:].differentiate('yh') / 1e2 / 111e3
      
      ml_conv = -(ml_u_x + ml_v_y)*secPerDay;    # CONvergence on model layer
      ml_vort = (ml_v_x - ml_u_y)*secPerDay;     # Relative vorticity on model layer
      sfc_conv = -(sfc_u_x + sfc_v_y)*secPerDay;    # CONvergence on model layer
      sfc_vort = (sfc_v_x - sfc_u_y)*secPerDay;     # Relative vorticity on model layer
      
      iso_26_levs = np.arange(0,160+1e-6,5.0);		iso_26_ticks = np.arange(0,160+1e-6,20.0)
      iso_20_levs = np.arange(0,300+1e-6,10.0);		iso_20_ticks = np.arange(0,300+1e-6,20.0)
      #OHC_levs = np.arange(0,150+1e-6,5.0);		OHC_ticks = np.arange(0,150+1e-6,20.0);
      OHC_levs = np.arange(0,200+1e-6,5.0);		OHC_ticks = np.arange(0,200+1e-6,20.0);
      #sfc_conv_levs = np.arange(-2e-3,2e-3+1e-8,1e-4);	sfc_conv_ticks = np.arange(-2e-3,2e-3+1e-8,5e-4)
      sfc_conv_levs = np.arange(-2e-2,2e-2+1e-8,5e-4);	sfc_conv_ticks = np.arange(-2e-2,2e-2+1e-8,2e-3)
      #sfc_vort_levs = np.arange(-5e-3,5e-3+1e-8,2e-4);	sfc_vort_ticks = np.arange(-5e-3,5e-3+1e-8,10e-4)
      sfc_vort_levs = np.arange(-4e-2,4e-2+1e-8,2e-3);	sfc_vort_ticks = np.arange(-4e-2,4e-2+1e-8,5e-3)
      #ml_conv_levs = np.arange(-2e-3,2e-3+1e-8,1e-4);	ml_conv_ticks = np.arange(-2e-3,2e-3+1e-8,5e-4)
      ml_conv_levs = np.arange(-2e-2,2e-2+1e-8,1e-3);	ml_conv_ticks = np.arange(-2e-2,2e-2+1e-8,5e-3)
      #ml_vort_levs = np.arange(-5e-3,5e-3+1e-8,2e-4);	ml_vort_ticks = np.arange(-5e-3,5e-3+1e-8,10e-4)
      ml_vort_levs = np.arange(-4e-2,4e-2+1e-8,2e-3);	ml_vort_ticks = np.arange(-4e-2,4e-2+1e-8,5e-3)
      SSH_levs = np.arange(-100,100+1e-6,5.0);		SSH_ticks = np.arange(-100,100+1e-6,20.0)
      #dSSH_levs = np.arange(-10,10+1e-6,0.5);		dSSH_ticks = np.arange(-10,10+1e-6,2.0)
      #dSSH_levs = np.arange(-20,20+1e-6,1.0);		dSSH_ticks = np.arange(-20,20+1e-6,5.0)
      dSSH_levs = np.arange(-100,100+1e-6,5.0);		dSSH_ticks = np.arange(-100,100+1e-6,20.0)
      SHF_levs = np.arange(-800,800+1e-6,50.0);		SHF_ticks = np.arange(-800,800+1e-6,200.0)
      DPI_levs = np.arange(0,30+1e-6,1.0);		DPI_ticks = np.arange(0,30+1e-6,2.0)
      MLD_levs = np.arange(0,160+1e-6,5.0);		MLD_ticks = np.arange(0,160+1e-6,10.0)
      SSS_levs = np.arange(32,38+1e-6,0.2);		SSS_ticks = np.arange(32,38+1e-6,0.5)
      MLT_levs = np.arange(26,30+1e-6,0.2);		MLT_ticks = np.arange(26,30+1e-6,0.5)
      MLS_levs = np.arange(32,38+1e-6,0.2);		MLS_ticks = np.arange(32,38+1e-6,0.5)
    
    # Dynamic potential intensity (Balaguru et al. 2015)
    rho0 = 1025.0;  # Reference density
    ustar = 0.20;	# [m/s] Frictional velocity HACK HACK HACK
    U = 10;		# TC translation speed [m/s]
    R = 50e3;	# [m] Radius of mixing-inducing winds (e.g., RMW, R64, ...)
    tmix = R/U;	# [s] Mixing length scale
    kappa = 0.40;   # von Karman constant
    g = 9.806;      # Gravity
    T0 = (-75+273.14);       # Outflow temperature (Komaromi and Doyle 2017)
    alpha = 0.03    # ??? "the rate of increase of potential density with depth beneath the mixed layer" HACK HACK HACK
    oL = MLD + ( ( (2*rho0*(ustar**3)*tmix)/(kappa*g*alpha) )**(1/3) )
    if ( OCEAN_SOURCE == 'HYCOM' ):
      Tdy = (1/oL) * (dZ * (T+273.14)).where(ds.Z <= oL).sum(dim='Z');
    elif ( OCEAN_SOURCE == 'MOM6' ):
      Tdy = (1/oL) * (dZ * (T+273.14)).where(ds.z_l <= oL).sum(dim='z_l');
    Ck_Cd = 0.9     # Ck/Cd (Bister and Emanuel 2002)
    cpa = 1.006     # [kJ/kg/oC]
    hg = 2549       # []
    # HACK HACK HACK
    aT = 26+273.14  # [oC] Air temperature atmospheric boundary layer HACK HACK HACK
    aQ = 0.01       # [kg/kg] Specific humidity atmospheric boundary layer HACK HACK HACK
    # HACK HACK HACK
    k = (cpa*aT) + hg*(aQ);
    kdy = (cpa*Tdy) + hg*(1);
    DPI = np.sqrt( ((Tdy - T0) / T0) * (Ck_Cd) * (kdy - k) )
    print('DPI',DPI.max().values)
    
    # Streamplots require equally spaced x and y
    xi = np.linspace(lon.min(),lon.max(),lon.shape[0]);
    yi = np.linspace(lat.min(),lat.max(),lat.shape[0]);
    
    figsize = (24,24);
    fontsize = 24
    small_fontsize = 24
    # Default for axis labels, etc.
    plt.rcParams.update({'font.size': 20})
    
    if (do_ships_output == 'Y') & (OCEAN_DOMAIN == 'd03'):
      T0 = T[0,:,:].squeeze()
      #Mark radial distance on lat/long grid ----------------------
      dlats = (lat * (math.pi)/180.) - (centerlat* (math.pi)/180.)
      dlons = (lon * (math.pi)/180.) - (centerlon* (math.pi)/180.)
      aa = ((np.sin(dlats/2))**2 + np.cos((centerlat*(math.pi)/180)) * np.cos((lat * (math.pi)/180)) * (np.sin(dlons/2))**2)
      cc = 2 * np.arctan2(np.sqrt(aa),np.sqrt(1-aa))
      rad_distances = cc * 6371.      
      bearings1 = ((np.arctan2 ( (np.sin (dlons)) * (np.cos(lat * (math.pi)/180.)) , ((np.cos(centerlat*(math.pi)/180.)) * (np.sin(lat * (math.pi)/180.))) - (((np.sin(centerlat*(math.pi)/180.)) * (np.cos(lat * (math.pi)/180.))) * (np.cos(dlons))))) * (180./(math.pi))) % 360
      bearings1 = np.array(bearings1)
      bearings = np.transpose(bearings1) 
      where500 = np.where(rad_distances <= 500.)
      where200 = np.where(rad_distances <= 200.)    

      #print(bearings[0,0])
      #print(bearings[0,2400])
      #print(bearings[950,2400])
      #print(bearings[950,0])
      #print(lat[0])
      #print(lat[900])
      #print(lon[0])
      #print(lon[950])
      #sys.exit()
      #make sure there's at least 50% ocean coverage
      wherecov = np.where(T0 > 0.)
      if ((np.size(wherecov) / np.size(where500)) < .5):
        T500 = np.nan
        OHC500 = np.nan
      else: 
        T500 = np.nanmean(T0[where500])
        OHC500 = np.nanmean(OHC[where500])
 
      if ((np.size(wherecov) / np.size(where200)) < .5):
        T200 = np.nan
        OHC200 = np.nan
      else:
        T200 = np.nanmean(T0[where200])
        OHC200 = np.nanmean(OHC[where200])
      #sst where 34kt quadrant wind---------
      where34 = np.where(((bearings < 90.) & (rad_distances < neq34)) | ((bearings < 180.) & (bearings >= 90.) & (rad_distances < seq34)) | ((bearings < 270.) & (bearings >= 180.) & (rad_distances < swq34)) | ((bearings < 360.) & (bearings >= 270.) & (rad_distances < nwq34)))
      print(np.size(where34))
      print(neq34)
      T34 = np.nanmean(T0[where34])
      OHC34 = np.nanmean(OHC[where34])

      #write to files----------
      sstfname = ODIR+'/'+LONGSID.lower()+'.ships.sst.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.dat'
      ohcfname = ODIR+'/'+LONGSID.lower()+'.ships.ohc.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.dat'
      
      f = open(sstfname,'a+')
      print(f'{FHR}, {T200}, {T500}, {T34} ',file=f)
      f.close()      

      f = open(ohcfname,'a+')    
      print(f'{FHR}, {OHC200}, {OHC500}, {OHC34}',file=f)
      f.close()

      pass;

    # FIGURE: Depth of the 26 oC isotherm
    if do_iso_26 == 'Y':
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, i26, levels=iso_26_levs, extend='both')
      debug_dump_range(FHR,'i26',i26)
      cbar1 = plt.colorbar(co1, ticks=iso_26_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Depth of 26 $^oC$ Isotherm (m, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Depth of 26 $^oC$ Isotherm (m, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          #DEBUG:          print(f'DEBUG: set_xlim( {lonmin},{lonmax} ), set_ylim( {latmin},{latmax} )');
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.iso_26.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
        os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    # FIGURE: Depth of the 20 oC isotherm
    if do_iso_20 == 'Y':
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, i20, levels=iso_20_levs, extend='both')
      debug_dump_range(FHR,'i20',i20)
      cbar1 = plt.colorbar(co1, ticks=iso_20_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Depth of 20 $^oC$ Isotherm (m, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Depth of 20 $^oC$ Isotherm (m, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.iso_20.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
        os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    # FIGURE: Ocean heat content [J/cm^2]
    if do_ohc == 'Y':
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, OHC, levels=OHC_levs, extend='both')
      debug_dump_range(FHR,'OHC',OHC)
      cbar1 = plt.colorbar(co1, ticks=OHC_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.ohc.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
        os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    # FIGURE: Convergence in surface currents
    if do_sfc_conv == 'Y':
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, sfc_conv, levels=sfc_conv_levs, cmap='seismic',extend='both')
      debug_dump_range(FHR,'sfc_conv',sfc_conv)
      cbar1 = plt.colorbar(co1, ticks=sfc_conv_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Surface Convergence ($d^{-1}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Surface Convergence ($d^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.sfc_conv.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
        os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    # FIGURE: Curl in surface currents
    if do_sfc_vort == 'Y':
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, sfc_vort, levels=sfc_vort_levs, cmap='seismic',extend='both')
      debug_dump_range(FHR,'sfc_vort',sfc_vort)
      cbar1 = plt.colorbar(co1, ticks=sfc_vort_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Surface Vorticity ($d^{-1}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Surface Vorticity ($d^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.sfc_vort.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
        os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    # FIGURE: Convergence in mixed-layer currents
    if do_ml_conv == 'Y':
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, ml_conv, levels=ml_conv_levs, cmap='seismic',extend='both')
      debug_dump_range(FHR,'ml_conv',ml_conv)
      cbar1 = plt.colorbar(co1, ticks=ml_conv_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Mixed-layer Convergence ($d^{-1}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          ax1.set_title(EXPT.strip()+'\n'+ r'Mixed-layer Convergence ($d^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.ml_conv.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
        os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    # FIGURE: Curl in mixed-layer currents
    if do_ml_vort == 'Y':
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, ml_vort, levels=ml_vort_levs, cmap='seismic',extend='both')
      debug_dump_range(FHR,'ml_vort',ml_vort)
      cbar1 = plt.colorbar(co1, ticks=ml_vort_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Mixed-layer Vorticity ($d^{-1}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Mixed-layer Vorticity ($d^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.ml_vort.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
        os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    # FIGURE: Sea-surface height [cm]
    if do_ssh == 'Y' and SSH is not None:
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, SSH, levels=SSH_levs, cmap='seismic',extend='both')
      debug_dump_range(FHR,'SSH',SSH)
      cbar1 = plt.colorbar(co1, ticks=SSH_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Sea-Surface Height ($cm$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Sea-Surface Height ($cm$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.ssh.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
        os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    # FIGURE: Sea-surface height TENDENCY [cm/d]
    if do_ssh_tendency == 'Y' and dSSH is not None:
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, dSSH, levels=dSSH_levs, cmap='seismic',extend='both')
      debug_dump_range(FHR,'dSSH',dSSH)
      cbar1 = plt.colorbar(co1, ticks=dSSH_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Sea-Sfc. Ht. Tendency ($cm/d$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Sea-Sfc. Ht. Tendency ($cm/d$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.dssh.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
        os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    # FIGURE: Sea-surface Heat Flux [W/m^2]
    if do_shf == 'Y' and SHF is not None:
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, SHF, levels=SHF_levs, cmap='seismic',extend='both')
      debug_dump_range(FHR,'SHF',SHF)
      cbar1 = plt.colorbar(co1, ticks=SHF_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Surface Heat Flux ($W m^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Surface Heat Flux ($W m^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.shf.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
        os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    # FIGURE: Dynamic Potential Intensity (Balaguru et al. 2015)
    if do_dpi == 'Y':
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, DPI, levels=DPI_levs, extend='both')
      debug_dump_range(FHR,'DPI',DPI)
      cbar1 = plt.colorbar(co1, ticks=DPI_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Dynamic Potential Intensity ($m\ s^{-1}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Dynamic Potential Intensity ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.dpi.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
        os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    
    
    # FIGURE: Mixed-layer depth [m]
    if do_mld == 'Y' and MLD is not None:
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, MLD, levels=MLD_levs, extend='both')
      debug_dump_range(FHR,'MLD',MLD)
      cbar1 = plt.colorbar(co1, ticks=MLD_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Mixed-Layer Depth ($m$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Mixed-Layer Depth ($m$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.mld.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
          os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    
    # FIGURE: Sea-surface salinity [psu]
    if do_sss == 'Y' and SSS is not None:
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, SSS, levels=SSS_levs, cmap='Blues',extend='both')
      debug_dump_range(FHR,'SSS',SSS)
      cbar1 = plt.colorbar(co1, ticks=SSS_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Sea-Surface Salinity ($psu$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Sea-Surface Salinity ($psu$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.sss.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
          os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    
    # FIGURE: Mixed-layer temperature [oC]
    if do_mlt == 'Y' and MLT is not None:
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, MLT, levels=MLT_levs, cmap='Reds',extend='both')
      debug_dump_range(FHR,'MLT',MLT)
      cbar1 = plt.colorbar(co1, ticks=MLT_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Mixed-Layer Temperature ($^oC$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Mixed-Layer Temperature ($^oC$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.mlt.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
          os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    
    # FIGURE: Mixed-layer salinity [psu]
    if do_mls == 'Y' and MLS is not None:
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, MLS, levels=MLS_levs, cmap='Blues',extend='both')
      debug_dump_range(FHR,'MLS',MLS)
      cbar1 = plt.colorbar(co1, ticks=MLS_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      ax1.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(ax1,centerlon,centerlat,minpressure);
      if ( OCEAN_DOMAIN == 'd03' ):
          Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=0.5);
          ax1.set_title(EXPT.strip()+'\n'+ r'Mixed-Layer Salinity ($psu$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      else:
          #Axes.streamplot(ax1,xi,yi,MLu,MLv,color='gray',density=5.0);
          ax1.set_title(EXPT.strip()+'\n'+ r'Mixed-Layer Salinity ($psu$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
          ax1.set_xlim([lonmin,lonmax]); ax1.set_ylim([latmin,latmax]);
      figfname = ODIR+'/'+LONGSID.lower()+'.mls.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.f'+format(FHR,'03d')
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if ( DO_CONVERTGIF ):
          os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
      plt.close(fig1)
    
    # Write the input file to a log to mark that it has ben processed
    update_plottedfile(PLOTTED_FILE, FILE)
    print(f'MSG: Done with Plots {datetime.now()}')
  
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

##############################
def axes_radpres(ax, xmax, xmin, ymax=1000, ymin=100):
  """Set up common axes attributes for radius-pressure graphics.
  @param ax:   the axes object
  @param xmax: max value of both x/y axes
  @param xmin: min value of both x/y axes
  @kwarg ymax: max value of y-axis
  @kwarg ymin: min value of y-axis
  """
  xticks = np.linspace(xmin,xmax,11)
  yticks = np.linspace(ymax,ymin,10)

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


def axes_radhgt(ax, xmax, xmin, ymax=18, ymin=0):
  """Set up common axes attributes for wavenumber graphics.
  @param ax:   the axes object
  @param xmax: max value of x-axis
  @param xmin: min value of x-axis
  @kwarg ymax: max value of y-axis
  @kwarg ymin: min value of y-axis
  """
  xticks = np.linspace(xmin,xmax,11)
  yticks = np.linspace(ymin,ymax,10)

  ax.set_xlim(xmin, xmax)
  ax.set_xticks(xticks)
  ax.set_xticklabels([str(int(x)) for x in xticks], fontsize=24)
  ax.set_xlabel('Radius (km)', fontsize=24)

  ax.set_ylim(ymin,ymax)
  ax.set_yticks(yticks)
  ax.set_yticklabels([str(int(x)) for x in yticks], fontsize=24)
  ax.set_ylabel('Height (km)', fontsize=24)

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
