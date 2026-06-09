#!/usr/bin/env python

# Check that GPLOT_DIR is defined in the environment.
import os, time, warnings
GPLOT_DIR = os.environ['GPLOT_DIR']
print('MSG: Found this GPLOT location --> '+GPLOT_DIR)

#Import necessary modules
print('MSG: Importing Everything Needed')
from datetime import datetime, timedelta
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

import pandas as pd;
import pyproj;
import scipy #Used for interpolation to polar coordinates
from scipy import interpolate #The interpolation function
from matplotlib.ticker import ScalarFormatter #Used to change the log-y-axis ticks
import struct;
import sys #To change the path 
import xarray as xr

import argparse
import glob
import math
import cmath
import re
from mpl_toolkits.axes_grid1 import make_axes_locatable

# GPLOT utility package (Sessions 1-7 infrastructure)
from gplot_utils import namelist as nml_utils
from gplot_utils import atcf as atcf_utils
from gplot_utils import ensemble as ens_utils
from gplot_utils import plot_utils
from gplot_utils import ocean_reader
from gplot_utils import constants as gplot_const


default_ddir='/scratch4/AOML/aoml-hafs1/Lew.Gramer/ocean/data'
if ( not os.path.exists(default_ddir) ):
  default_ddir='/work2/noaa/aoml-hafs1/lgramer/ocean/data'
if ( not os.path.exists(default_ddir) ):
  default_ddir='/lfs5/HFIP/hur-aoml/Lew.Gramer/ocean'
if ( not os.path.exists(default_ddir) ):
  # Deferred error: some HPC-only obs paths may not exist on dev machines.
  # Emit a warning here so the module can still be imported for smoke tests;
  # the obs data loaders below will fail loudly if actually invoked.
  warnings.warn(f'NO OCEAN DATA PATH FOUND (using {default_ddir}); obs loaders '
                f'will fail if invoked.')

# Physical constants (imported from gplot_utils/constants.py)
cp = gplot_const.cp_sw                  # Specific heat capacity of seawater [J kg^-1 K^-1]
rho = gplot_const.rho_sw                # Mean water density [kg m^-3]
kJcm2_per_Jm2 = gplot_const.kJcm2_per_Jm2  # Unit conversion kJ/cm^2 == 10^7 J/m^2

# Maximum distance away from other platforms (buoy, Argo, etc.) to plot NGE-OHC profiles
#max_nge_dist = 78
max_nge_dist = 111

markersize=24

# Vertices for different plot markers (left half-squares, top half-squares, etc.)
lower_square_verts = [[-1, -1], [+1, -1], [+1,  0], [-1,  0]];
right_square_verts = [[-1, -1], [ 0, -1], [ 0, +1], [-1, +1]];
upper_square_verts = [[+1, +1], [+1, +1], [+1,  0], [+1,  0]];
left_square_verts  = [[ 0, -1], [+1, -1], [+1, +1], [ 0, +1]];

UL_triangle_verts  = [[-1, -1], [+1, +1], [-1,  +1]];
LR_triangle_verts  = [[-1, -1], [+1, -1], [+1,  +1]];
UR_triangle_verts  = [[-1, +1], [+1, +1], [+1,  -1]];
LL_triangle_verts  = [[-1, -1], [+1, -1], [-1,  +1]];

def debug_dump_range(FHR,varnm,var):
  #print(f'DEBUG: FHR {int(FHR)}: {varnm} in {np.nanmin(var.values)},{np.nanpercentile(var.values,25)},{np.nanmedian(var.values)},{np.nanpercentile(var.values,75)},{np.nanmax(var.values)}');
  pass;

def add_center_label(ax1,centerlon,centerlat,minpressure):
  ax1.text(centerlon,centerlat,f'{minpressure}\n  L',color='black',fontsize=28,fontweight='extra bold');
  ax1.text(centerlon,centerlat,f'{minpressure}\n  L',color='red',fontsize=28);
  ax1.axvline(centerlon,color='k',linestyle='--',linewidth=1.0);
  ax1.axhline(centerlat,color='k',linestyle='--',linewidth=1.0);

# HYCOM bathymetry reader now lives in gplot_utils.ocean_reader.read_hycom_depth
# (see Phase 0c of the NCL→Python migration plan). Local copy removed.

def str2latlon(s,div=1.0,unwrap=False):
  if ( isinstance(s,str) ):
    s = s.strip();
    if ( s[:1].isdigit() or (s[:1]=='-' and s[1:2].isdigit()) ):
      if ( s.endswith('S') or s.endswith('W') ):
        s = '-' + s;
      try:
        if ( s.endswith('W') and unwrap ):
          s = 360 + (np.double(s.strip('NSEW'))/div);
        else:
          s = np.double(s.strip('NSEW'))/div;
      except:
        pass;
  elif ( hasattr(s,'__iter__') ):
    ses = s;
    s = [];
    for anS in ses:
      s.append(str2latlon(anS,div,unwrap))
  return(s);

def load_noaa_recon(dt,ddir=default_ddir):
  '''Load latitude and longitude for time stamp DT from NOAA (Hurricane Hunter) Reconnaissance HDOB.'''
  lat = np.nan
  lon = np.nan
  yr = dt.strftime('%Y')
  FDT = dt.strftime('%Y%m%d%H')
  TSTMP = dt.strftime('%H%M'); #TSTMP = dt.strftime('%H%M%S')
  #grep 0125 ${LEW2}/ocean/data/noaa-recon/2023/AHONT1-KNHC.2023090901*
  #for fdfname in sorted(glob.glob(f'{fdfpatt}')):
  fdfpatt = f'{ddir}/noaa-recon/{yr}/AHONT1-K*.{FDT}*.txt'
  fdfnames = sorted(glob.glob(f'{fdfpatt}'))
  # Also check files for one hour in the future just in case
  dtP1H = dt + timedelta(hours=1);
  FDTP1H = dtP1H.strftime('%Y%m%d%H')
  fdfpattP1H = f'{ddir}/noaa-recon/{yr}/AHONT1-K*.{FDTP1H}*.txt'
  fdfnames.extend(sorted(glob.glob(f'{fdfpattP1H}')))
  for fdfname in fdfnames:
    fd = open(fdfname)
    for ln in fd.readlines():
      if re.match(TSTMP,ln):
        #DEBUG:        print(f'FOUND {fdfname}');
        tmstr,latstr,lonstr,ig = ln.split(' ',3)
        #DEBUG:        print(tmstr,latstr,lonstr);
        lat = str2latlon(latstr,100.0)
        lon = str2latlon(lonstr,100.0)
        if ( (np.isnan(lon)) | (np.isnan(lat)) ): 
          #DEBUG:          breakpoint();
          pass;
        #DEBUG:        breakpoint();
        break;
    fd.close()
    if ( (~np.isnan(lon)) & (~np.isnan(lat)) ):
      break;
  #DEBUG:  breakpoint();
  return(lat,lon);

def load_axbt_data(BASEFDT,ndays=(-3,-2,-1,0,1,2,3,4),ddir=default_ddir):
    '''Create XArray Dataset of AXBT float profiles for all flights during days in BASEFDT +/- NDAYS'''
    basefdt = datetime.strptime(BASEFDT,'%Y%m%d');
    for nday in ndays:
        fdt = basefdt + timedelta(days=nday);
        FDT = fdt.strftime('%Y%m%d');
        #/scratch2/AOML/aoml-hafs1/Lew.Gramer/ocean/data/axbts/20230910I1/20230910I1_BT_120527.dat
        fdfpatt = f'{ddir}/axbts/{FDT}*/{FDT}*.dat'
        for fdfname in sorted(glob.glob(f'{fdfpatt}')):
            try:
                axbt = pd.read_csv(fdfname,r'\s+',low_memory=False,header=2)
            except:
                print(f'Failed read_csv: {fdfname}');
                #DEBUG:                breakpoint()
                continue;
            
            flight = os.path.basename(fdfname).split('_')[0]
            t = datetime.strptime(axbt.columns[0]+axbt.columns[1], '%Y%m%d%H%M%S')
            try:
                lat = np.double(axbt.columns[2])
                lon = np.double(axbt.columns[3])
            except:
                lat = 0
                lon = 0
            if ( (lat == 0) & (lon == 0) ):
              lat,lon = load_noaa_recon(t);
            if ( lon < 0 ):
              lon = lon + 360
            platform = axbt.columns[4]
            storm = axbt.columns[5]
            axbt.dropna(axis=1,inplace=True)
            axbt.columns = ['depth','temperature','ignore']
            T = axbt.temperature.values.reshape((1,len(axbt.temperature)))
            T[T<0] = np.nan
            z = axbt.depth
            
            axbt = xr.Dataset( { "T": (("t","z"),T), "lon": ("t",[lon]), "lat": ("t",[lat]), \
                                 "flight": ("t",[flight]), "storm": ("t",[storm]), "platform": ("t",[platform]), }, \
                               coords={"t": [t], "z": (("z"), z) }, )
            #DEBUG:            breakpoint()
            if ( 'axbts' in locals() ):
                axbts = xr.merge([axbts,axbt]);
            else:
                axbts = axbt;
                
    
    if ( 'axbts' in locals() ):
      delT = axbts.T - 26.0; delT = delT.where(delT>0);
      axbts['delTdz'] = delT * 1.5;
      axbts['CalcOHC'] = cp*rho*axbts.delTdz.sum('z') * kJcm2_per_Jm2
      axbts['CalcIso26'] = axbts.z.where(axbts.T >= 26.0).max(axis=0)
      axbts['CalcIso20'] = axbts.z.where(axbts.T >= 20.0).max(axis=0)
      axbts['CalcMLD'] = axbts.z.where(((axbts.T.T-axbts.T.values[:,0]).T)<-0.35).min(axis=0)
      axbts['CalcSST'] = axbts.T[:,0]
      axbts['CalcMLT'] = axbts.T.where(axbts.T.z<axbts.CalcMLD).mean(axis=1);
      #DEBUG:      breakpoint();
      return(axbts)
    else:
      return(xr.Dataset(coords={'lon':[],'lat':[],'t':[]}))
#load_axbt_data


def get_axbt_subset(AXBTs,lonmin,lonmax,latmin,latmax,tmin,tmax):
  if ( AXBTs is not None and len(AXBTs) > 0 ):
    AXBTs = AXBTs.where((lonmin<AXBTs.lon) & (AXBTs.lon<lonmax) & (latmin<AXBTs.lat) & (AXBTs.lat<latmax) & (tmin<AXBTs.t) & (AXBTs.t<tmax), drop=True)
  #DEBUG:    print(np.count_nonzero(~np.isnan(AXBTs.T)))
  return(AXBTs);


def load_nge_data(BASEFDT,ndays=(-2,-1,0,1,2,3,4),ddir=default_ddir):
  #DEBUG:  print(f'load_nge_data({BASEFDT},ndays=${ndays}...)');
  #ohcfname = f'{ddir}/OHC/NGEOHC_NA_l2_v0r1_multi_s20230913000000_e20230913235959_c20230914064456.nc';
  #ohcfname = f'{ddir}/OHC/NGEOHC_NA_l2_v0r1_multi_s20230914000000_e20230914235959_c20230915064026.nc';
  ohcfnames = [];
  basefdt = datetime.strptime(BASEFDT,'%Y%m%d');
  ohcfnames = glob.glob(f'{ddir}/NGE-OHC/NGEOHC_NA_l2_v0r1_multi_s{BASEFDT}*.nc');
  if ( len(ohcfnames) == 0 ):
    for nday in ndays:
      fdt = basefdt + timedelta(days=nday);
      FDT = fdt.strftime('%Y%m%d');
      ohcfnames = glob.glob(f'{ddir}/NGE-OHC/NGEOHC_NA_l2_v0r1_multi_s{FDT}*.nc');
      if ( len(ohcfnames) > 0 ):
        break;
  if ( len(ohcfnames) == 0 ):
    print(f'WARNING:: No NGE-OHC data for {BASEFDT} +/- {ndays}');
    return(None);
    #raise ValueError(f'No NGE-OHC data for {BASEFDT} +/- {ndays}');
  ds = xr.open_dataset(ohcfnames[0]);
  ds.lon[ds.lon<0] = ds.lon[ds.lon<0] + 360;
  ds.CT[:] = ds.CT / 1e2;
  ds['CalcSST'] = ds.CT[:,0];
  delT = ds.CT - 26.0; delT = delT.where(delT>0);
  ds['delTdz'] = delT * np.median(np.diff(ds.pressure));
  ds['CalcOHC'] = cp*rho*ds.delTdz.sum('pressure') * kJcm2_per_Jm2
  ds['CalcMLT'] = ds.CT.where(ds.CT.pressure>ds.MLD).mean(axis=1);
  ds['CalcSSS'] = ds.SA[:,0];
  ds['CalcMLS'] = ds.SA.where(ds.CT.pressure>ds.MLD).mean(axis=1);
  #DEBUG:  breakpoint();
  return(ds);

def load_argo_data(BASEFDT,ndays=(-3,-2,-1,0,1,2,3,4),ddir=default_ddir):
  '''Create DICT of Argo float profiles for all of the days in BASEFDT + NDAYS. DICT has one element (an XArray Dataset) for each day.'''
  #DEBUG:  print(f'load_argo_data({BASEFDT},ndays=${ndays}...)');
  argos = []
  basefdt = datetime.strptime(BASEFDT,'%Y%m%d');
  for nday in ndays:
    fdt = basefdt + timedelta(days=nday);
    FDT = fdt.strftime('%Y%m%d');
    FDTy = f'{fdt.year}';
    FDTm = f'{fdt.month:02}'
    fdfname = f'{ddir}/argo/usgodae.org/pub/outgoing/argo/geo/atlantic_ocean/{FDTy}/{FDTm}/{FDT}_prof.nc'
    
    try:
      argo = xr.open_dataset(fdfname);
    except:
      print(f'Failed load_argo_data: {fdfname}');
      #DEBUG:      breakpoint();
      continue
    argo.LONGITUDE[argo.LONGITUDE<0] = argo.LONGITUDE[argo.LONGITUDE<0] + 360
    goodix = ( (argo.TEMP_QC==b'1') | (argo.TEMP_QC==b'2') ) & ( (argo.PRES_QC==b'1') | (argo.PRES_QC==b'2') ) & ( (argo.PSAL_QC==b'1') | (argo.PSAL_QC==b'2') )
    #goodix = ( (argo.TEMP_QC==b'1') | (argo.TEMP_QC==b'2') ) & ( (argo.PRES_QC==b'1') | (argo.PRES_QC==b'2') )
    #goodix = ( (argo.TEMP_QC==b'1') ) & ( (argo.PRES_QC==b'1') ) & ( (argo.PSAL_QC==b'1') )
    argo.PRES[:] = argo.PRES.where(goodix)[:];
    argo.TEMP[:] = argo.TEMP.where(goodix)[:];
    argo = argo.dropna('N_PROF',how='all',subset=['TEMP']);
    
    delT = argo.TEMP - 26.0; delT = delT.where(delT>0);
    #argo['delTdz'] = delT * np.nanmedian(np.diff(argo.PRES));
    argo['delTdz'] = delT;
    argo['delTdz'][:,0:-1] = delT[:,0:-1] * np.diff(argo.PRES);
    argo['CalcOHC'] = cp*rho*argo.delTdz.sum('N_LEVELS') * kJcm2_per_Jm2
    argo['CalcIso26'] = argo.PRES.where(argo.TEMP<=26.0).min(axis=1)
    argo['CalcIso20'] = argo.PRES.where(argo.TEMP<=20.0).min(axis=1)
    #DEBUG:    print(f'Argo XXXargo.PLATFORM_NUMBER.valuesXXX:');
    #DEBUG:    print(f'Argo {argo.PLATFORM_NUMBER.values}: CalcOHC={argo.CalcOHC}');
    #DEBUG:    print(f'Argo                                   CalcOHC={argo.CalcOHC}');
    argo['CalcMLD'] = argo.PRES.where(((argo.TEMP.T-argo.TEMP.values[:,0]).T)<-0.35).min(axis=1)
    #argo['CalcMLD'] = 200; #### *** HACK *** HACK *** HACK ***
    #DEBUG:    print(f'Argo {argo.PLATFORM_NUMBER.values}: CalcMLD={argo.CalcMLD}');
    #DEBUG:    print(f'Argo                                   CalcMLD={argo.CalcMLD}');
    argo['CalcMLT'] = argo.TEMP.where(argo.PRES<=argo.CalcMLD).mean(axis=1);
    argo['CalcSSS'] = argo.PSAL[0];
    argo['CalcMLS'] = argo.PSAL.where(argo.PRES<=argo.CalcMLD).mean(axis=1);
    #DEBUG:    breakpoint();
    argos.append(argo);
    # except:
    #   print(f'Failed load_argo_data: {fdfname}');
    #   breakpoint();
  return(argos)

def find_domain_argos(argos,corners,timerng=None):
  '''Return array of (subgroup-indexed) indices into ARGOS with all Argo profiles inside domain corners==(LON1,LAT1,LON2,LAT2)'''
  argo_idxs = []
  search_start_idx = 0;
  for argo in argos:
    if ( timerng is not None ):
      idxs = np.where( (corners[0]<=argo.LONGITUDE) & (argo.LONGITUDE<=corners[2]) & (corners[1]<=argo.LATITUDE) & (argo.LATITUDE<=corners[3]) & (np.datetime64(timerng[0])<=argo.JULD) & (argo.JULD<=np.datetime64(timerng[1])) )
    else:
      idxs = np.where( (corners[0]<=argo.LONGITUDE) & (argo.LONGITUDE<=corners[2]) & (corners[1]<=argo.LATITUDE) & (argo.LATITUDE<=corners[3]) )
    if ( len(idxs) > 0 ):
        idxs[0][:] = idxs[0][:] + search_start_idx
        argo_idxs = np.unique(np.concatenate((argo_idxs,idxs[0]))).astype(int);
    search_start_idx = search_start_idx + len(argo.N_PROF)
  return(argo_idxs);

def calc_argo_dists(argos,pt):
  '''Calculate distance from point PT (a (LON,LAT) tuple) to each Argo profile in DICT of XArray.Datasets ARGOS'''
  argo_dists = [];
  for argo in argos:
    argots = np.sqrt( ((argo.LONGITUDE-pt[0])**2)  + ((argo.LATITUDE-pt[1])**2) ).values * 111
    if ( not isinstance(argots,np.ndarray) ):
      x = np.ndarray((1,)); x[:] = argots; argots = x; del(x)
    if ( len(argo_dists) == 0 ):
      argo_dists = argots;
    else:
      argo_dists = np.concatenate((argo_dists,argots));
  return(argo_dists);

def get_argo_profile(argos,fix):
  '''Retrieve an Argo float profile at index FIX from among DICT of multiple XArray.Datasets. NOTE: FIX must be <= sum(len(ARGOS)).'''
  fix_checked = 0;
  fix_searched = 0;
  for argo in argos:
    fix_checked = fix_searched + len(argo.N_PROF)
    if ( fix < fix_checked ):
      prof = argo.isel({'N_PROF':fix-fix_searched})
      prof['N_PROF'] = [argo.N_PROF[fix-fix_searched]]
      break;
    fix_searched = fix_checked;
  return(prof);

def get_argo_subset(argos,fdixes):
  argosubset = [];
  for ix in fdixes:
    prof = get_argo_profile(argos,ix);
    argosubset.append(prof);
  return(argosubset);


def load_glider_data(BASEFDT,ndays=np.arange(-14,1,1),ddir=default_ddir):
  '''Create DICT of Hurricane Glider profiles for all of the days in BASEFDT + NDAYS. DICT has one element (an XArray Dataset) for each day.'''
  #DEBUG:  print(f'load_glider_data({BASEFDT},ndays=${ndays}...)');
  fdses = []
  locsfname = f'{ddir}/gliders/locations.dat'
  basefdt = datetime.strptime(BASEFDT,'%Y%m%d');
  for nday in ndays:
    fdt = basefdt + timedelta(days=int(nday));
    FDT = fdt.strftime('%Y%m%d');
    # /scratch2/AOML/aoml-hafs1/Lew.Gramer/ocean/data/gliders/gliders.ioos.us/erddap/tabledap/mote-dora-20220907T1200.nc
    # /scratch2/AOML/aoml-hafs1/Lew.Gramer/ocean/glider.txt
    for gldfname in sorted(glob.glob(f'{ddir}/gliders/gliders.ioos.us/erddap/tabledap/*-{FDT}T*.nc')):
      (platformID,FDTM) = os.path.splitext(os.path.basename(gldfname))[0].rsplit('-',1); #platformID may have '-' in it...
      print('gldfname=',gldfname);
      fdtm = datetime.strptime(FDTM,'%Y%m%dT%H%M');
      try:
        fds = xr.open_dataset(gldfname);
        profids = np.unique(fds.profile_id.values)
        print(f'FOUND GLIDER PROFILES: {profids}');
        # breakpoint();
      except:
        pass;
  return(fdses)

# figsize = (12,12);
# fontsize = 18
# small_fontsize = 14
figsize = (24,24);
fontsize = 24
small_fontsize = 24
# Default for axis labels, etc.
plt.rcParams.update({'font.size': 20})

nrows = 7;
ncols = 6;

global label1, label2, label3, label4, label5
label1=None;
label2=None;
label3=None;
label4=None;
label5=None;

Tcmap = plt.cm.get_cmap('Reds')
Tnorm = matplotlib.colors.Normalize(vmin=20.0,vmax=32.0)
OHCcmap = plt.cm.get_cmap('rainbow')
OHCnorm = matplotlib.colors.Normalize(vmin=0.0,vmax=150.0)
Scmap = plt.cm.get_cmap('Greens')
Snorm = matplotlib.colors.Normalize(vmin=34.0,vmax=37.0)
Dcmap = plt.cm.get_cmap('Blues')
Dnorm = matplotlib.colors.Normalize(vmin=0.0,vmax=200.0)
iso_26_levs = np.arange(0,160+1e-6,5.0);		iso_26_ticks = np.arange(0,160+1e-6,20.0)
OHC_levs = np.arange(0,150+1e-6,5.0);   		OHC_ticks = np.arange(0,150+1e-6,20.0);
SST_levs = np.arange(26,30+1e-6,0.2);   		SST_ticks = np.arange(26,30+1e-6,0.5);    	SST_cnts = np.arange(26,30+1e-6,1.0);


def getNGEVar1(nge,var1,ix):
  if ( var1 == 'T' ):
    ngeVar1 = nge.CT[ix]
  elif ( var1 == 'S' ):
    ngeVar1 = nge.SA[ix]
  elif ( var1 == 'D' ):
    ngeVar1 = nge.MLD[ix]
  elif ( var1 == 'i26' ):
    ngeVar1 = nge.Z26[ix]
  else:
    raise ValueError(f'Unknown "var1" variable type for NGE: {var1}');
  return(ngeVar1);

def getNGEVar2(nge,var2,ix):
  if ( var2 == 'OHC' ):
    ngeVar2 = nge.CalcOHC[ix]
  elif ( var2 == 'S' ):
    ngeVar2 = nge.CalcMLS[ix]
  elif ( var2 == 'T' ):
    ngeVar2 = nge.CalcMLT[ix]
  elif ( var2 == 'D' ):
    ngeVar2 = nge.MLD[ix]
  elif ( var2 == 'i26' ):
    ngeVar2 = nge.Z26[ix]
  else:
    raise ValueError(f'Unknown "var2" variable type for NGE: {var2}');
  return(ngeVar2);

def getARGOVar1(argo,var1):
  if ( var1 == 'T' ):
    argoVar1 = argo.TEMP
  elif ( var1 == 'S' ):
    argoVar1 = argo.PSAL
  elif ( var1 == 'D' ):
    argoVar1 = argo.CalcMLD
  elif ( var1 == 'i26' ):
    argoVar1 = argo.iso_26
  else:
    raise ValueError(f'Unknown "var1" variable type for ARGO: {var1}');
  return(argoVar1);

def getARGOVar2(argo,var2):
  if ( var2 == 'OHC' ):
    argoVar2 = argo.CalcOHC
  elif ( var2 == 'S' ):
    argoVar2 = argo.CalcMLS
  elif ( var2 == 'T' ):
    argoVar2 = argo.CalcMLT
  elif ( var2 == 'D' ):
    argoVar2 = argo.CalcMLD
  elif ( var2 == 'i26' ):
    argoVar2 = argo.CalcIso26
  else:
    raise ValueError(f'Unknown "var2" variable type for ARGO: {var2}');
  return(argoVar2);

def getAXBTVar1(axbt,var1,xix):
  if ( var1 == 'T' ):
    axbtVar1 = axbt.T[xix]
  elif ( var1 == 'D' ):
    axbtVar1 = axbt.CalcMLD[xix]
  elif ( var1 == 'i26' ):
    axbtVar1 = axbt.CalcIso26[xix]
  elif ( var1 == 'OHC' ):
    axbtVar1 = axbt.CalcOHC[xix]
  else:
    raise ValueError(f'Unknown "var1" variable type for AXBT: {var1}');
  return(axbtVar1);

def getAXBTVar2(axbt,var2,xix):
  if ( var2 == 'OHC' ):
    axbtVar2 = axbt.CalcOHC[xix]
  elif ( var2 == 'T' ):
    axbtVar2 = axbt.CalcMLT[xix]
  elif ( var2 == 'D' ):
    axbtVar2 = axbt.CalcMLD[xix]
  elif ( var2 == 'i26' ):
    axbtVar2 = axbt.CalcIso26[xix]
  else:
    raise ValueError(f'Unknown "var2" variable type for AXBT: {var2}');
  return(axbtVar2);


def plot_profile(axT,axO,axP,lon,lat,z,T,OHC=None,mdlds=None,color='k',marker='d',linewidth=1.5,var1='T',var2='T',mdlsrc=None):
  '''Plot an observation/model profile comparison in axP, and add map markers color-coded for, e.g., SST (axO) and OHC (axT), respectively.'''
  mrkh = None;
  mmrkh = None;
  lhs = None;
  mlhs = None;
  mcolor = 'dodgerblue';
  # Find an appropriate pair of half-markers corresponding to each marker choice
  if ( marker=='d' ):
    marker = 8; #CARETLEFTBASE
    mmarker = 9; #CARETRIGHTTBASE
  elif ( marker=='s'):
    marker = 11; #CARETDOWNBASE
    mmarker = 10; #CARETUPBASE
  elif ( marker=='o'):
    # marker = 'o';
    # mmarker = '.';
    marker = lower_square_verts;
    mmarker = upper_square_verts;
  elif ( marker=='v'):
    marker = left_square_verts;
    mmarker = right_square_verts;
  
  if ( var1=='T' ):
    Tcolor = Tcmap(Tnorm(T[1]));
  elif ( var1=='S' ):
    Tcolor = Scmap(Snorm(T[1]));
  elif ( var1=='D' or var1=='i26' ):
    Tcolor = Dcmap(Dnorm(T));

  if ( color != 'orange' and color != 'lightgreen' ):
    breakpoint();
  axT.plot(lon,lat,marker=marker,markersize=markersize+2,markerfacecolor=None,markeredgecolor=color,markeredgewidth=2);
  mrkh = axT.plot(lon,lat,marker=marker,markersize=markersize,markerfacecolor=Tcolor,color=color);
  if ( OHC is not None ):
    if ( var2=='OHC' ):
      OHCcolor = OHCcmap(OHCnorm(OHC));
    elif ( var2=='T' ):
      OHCcolor = Tcmap(Tnorm(OHC));
    elif ( var2=='S' ):
      OHCcolor = Scmap(Snorm(OHC));
    elif ( var2=='D' or var2=='i26' ):
      OHCcolor = Dcmap(Dnorm(OHC));
    axO.plot(lon,lat,marker=marker,markersize=markersize+2,markerfacecolor=None,markeredgecolor=color,markeredgewidth=2);
    axO.plot(lon,lat,marker=marker,markersize=markersize,markerfacecolor=OHCcolor,color=color);
  
  if ( mdlds is not None ):
    if ( var1=='T' ):
      if ( mdlsrc == 'HYCOM' ):
        modelT = mdlds.temperature.interp( {'Longitude':lon, 'Latitude':lat} )[0]
        mTcolor = Tcmap(Tnorm(modelT.T[1]));
      elif ( mdlsrc == 'MOM6' ):
        modelT = mdlds.SST.interp( {'xh':lon, 'yh':lat} )
        mTcolor = Tcmap(Tnorm(modelT.T[0]));
      else:
        raise ValueError(f'Unknown mdlsrc {mdlsrc}');
    elif ( var1=='S' ):
      if ( mdlsrc == 'HYCOM' ):
        modelT = mdlds.salinity.interp( {'Longitude':lon, 'Latitude':lat} )[0]
        mTcolor = Scmap(Snorm(modelT.T[1]));
      elif ( mdlsrc == 'MOM6' ):
        modelT = mdlds.SSS.interp( {'xh':lon, 'yh':lat} )
        mTcolor = Scmap(Snorm(modelT.T[0]));
    elif ( var1=='D' ):
      if ( mdlsrc == 'HYCOM' ):
        modelT = mdlds.MLD.interp( {'Longitude':lon, 'Latitude':lat} )
      elif ( mdlsrc == 'MOM6' ):
        modelT = mdlds.MLD_0125.interp( {'xh':lon, 'yh':lat} )
      mTcolor = Dcmap(Dnorm(modelT));
    elif ( var1=='i26' ):
      if ( mdlsrc == 'HYCOM' ):
        modelT = mdlds.i26.interp( {'Longitude':lon, 'Latitude':lat} )
      elif ( mdlsrc == 'MOM6' ):
        modelT = mdlds.i26.interp( {'xh':lon, 'yh':lat} )
      mTcolor = Dcmap(Dnorm(modelT));
    
    if ( var2=='OHC' ):
      if ( mdlsrc == 'HYCOM' ):
        modelOHC = mdlds.ocean_heat_content.interp( {'Longitude':lon, 'Latitude':lat} )[0]
      elif ( mdlsrc == 'MOM6' ):
        modelOHC = mdlds.OHC.interp( {'xh':lon, 'yh':lat} )
      mOHCcolor = OHCcmap(OHCnorm(modelOHC));
    elif ( var2=='T' ):
      if ( mdlsrc == 'HYCOM' ):
        modelOHC = mdlds.MLT.interp( {'Longitude':lon, 'Latitude':lat} )
      elif ( mdlsrc == 'MOM6' ):
        modelOHC = mdlds.MLT.interp( {'xh':lon, 'yh':lat} )
      mOHCcolor = Tcmap(Tnorm(modelOHC));
    elif ( var2=='S' ):
      if ( mdlsrc == 'HYCOM' ):
        modelOHC = mdlds.MLS.interp( {'Longitude':lon, 'Latitude':lat} )
      elif ( mdlsrc == 'MOM6' ):
        modelOHC = mdlds.MLS.interp( {'xh':lon, 'yh':lat} )
      mOHCcolor = Scmap(Snorm(modelOHC));
    elif ( var2=='D' ):
      if ( mdlsrc == 'HYCOM' ):
        modelOHC = mdlds.MLD.interp( {'Longitude':lon, 'Latitude':lat} )
      elif ( mdlsrc == 'MOM6' ):
        modelOHC = mdlds.MLD.interp( {'xh':lon, 'yh':lat} )
      mOHCcolor = Dcmap(Dnorm(modelOHC));
    elif ( var2=='i26' ):
      if ( mdlsrc == 'HYCOM' ):
        modelOHC = mdlds.i26.interp( {'Longitude':lon, 'Latitude':lat} )
      elif ( mdlsrc == 'MOM6' ):
        modelOHC = mdlds.i26.interp( {'xh':lon, 'yh':lat} )
      mOHCcolor = Dcmap(Dnorm(modelOHC));
    
    axT.plot(lon,lat,marker=mmarker,markersize=markersize+2,markerfacecolor=None,markeredgecolor=mcolor,markeredgewidth=2);
    mmrkh = axT.plot(lon,lat,marker=mmarker,markersize=markersize,markerfacecolor=mTcolor,color=mcolor);
    axO.plot(lon,lat,marker=mmarker,markersize=markersize+2,markerfacecolor=None,markeredgecolor=mcolor,markeredgewidth=2);
    axO.plot(lon,lat,marker=mmarker,markersize=markersize,markerfacecolor=mOHCcolor,color=mcolor);
  
  lhs = axP.plot(T,z,color,linewidth=linewidth);
  if ( mdlds is not None ):
    if ( mdlsrc == 'HYCOM' and 'Z' in modelT.coords ):
      mlhs = axP.plot(modelT,-modelT.Z,mcolor,linewidth=linewidth);
    elif ( mdlsrc == 'MOM6' and 'z_l' in modelT.coords ):
      mlhs = axP.plot(modelT,-modelT.z_l,mcolor,linewidth=linewidth);
    else:
      mlhs = None;
  else:
    mlhs = None;
  if ( var1 == 'T' ):
    axP.set_xlim([20.0,32.0]); 
    axP.axvline(x=26.0,color='k',linestyle='--',linewidth=1.0);
  elif ( var1 == 'S' ):
    axP.set_xlim([34.0,37.0]); 
    axP.axvline(x=36.5,color='k',linestyle='--',linewidth=1.0);
  elif ( var1 == 'D' or var1 == 'i26' ):
    axP.set_xlim([0.0,200.0]); 
    axP.axvline(x=50.0,color='k',linestyle='--',linewidth=1.0);
  axP.set_ylim([-200,0]);
  axP.grid(True);
  
  return(mrkh,mmrkh,lhs,mlhs);
#plot_profile


# Must be reset to 1 for each GRAPHIC...
global_profile_num = 1;


def plot_quadrant_profiles(fig1,axT,axO,AXBTq,Argoq,NGEq,Gliq,Buoyq,Sailq,minrow,maxrow,mincol,maxcol,mdlds=None,var1='T',var2='OHC',mdlsrc=None):
  '''Plot temperature (or S or other) profiles from quadrant subsets AXBTq,
  Argoq, NGEq, and Gliq, and near-surface temps. (or salinity or other)
  from quadrant subsets Buoyq, Sailq, at subplot2grid locations between
  minrow,mincol and maxrow,maxcol, inclusive. Also plot model ocean profile
  at corresponding gridpoint, if mdlds is not None. Plot markers color-coded
  for OHC (MLS, etc.) in axO and variable (e.g., SST) in axT, for each
  element in AXBTq and Argoq also.'''
  
  global global_profile_num;
  global label1, label2, label3, label4, label5
  
  #DEBUG:  print(f'START plot_quadrant_profiles: label1={label1}, label2={label2}, label3={label3}, label4={label4}, label5={label5}');
  cix = mincol;
  rix = minrow;
  
  ####
  # Handle any AXBTs in the quadrant first (if var1 == 'T')
  skipargoix = [];
  if ( var1 == 'T' or var1 == 'D' or var1 == 'i26' ):
   for xix,xlon in enumerate(AXBTq.lon):
    if ( rix > maxrow ):
      break;
    if ( (~np.isnan(xlon)) & ((AXBTq.T[xix].max()-AXBTq.T[xix].min())>0.2) ):
      if ( 'axP' not in locals() ):
        axP = plt.subplot2grid((nrows,ncols),(rix,cix),1,1,fig1);
      else:
        axP = plt.subplot2grid((nrows,ncols),(rix,cix),1,1,fig1,sharex=axP,sharey=axP);
      
      # Did this AXBT happen to correspond with an NGE OHC track?
      if ( NGEq is not None ):
        ngedists = np.sqrt( ((NGEq.lon-AXBTq.lon[xix])**2)  + ((NGEq.lat-AXBTq.lat[xix])**2) ).values * 111
        #doubleix = np.where(ngedists<18)[0];
        doubleix = np.where(ngedists<max_nge_dist)[0];
        if ( len(doubleix) > 0 ):
          ix = doubleix[0];
          # Plot NGE lowest in the z-ordering (vs. Argo and AXBT - below)
          NGEqVar1 = getNGEVar1(NGEq,var1,ix);
          NGEqVar2 = getNGEVar2(NGEq,var2,ix);
          m,mm,l,ml=plot_profile(axT,axO,axP,NGEq.lon[ix],NGEq.lat[ix],-NGEq.pressure,NGEqVar1,OHC=NGEqVar2,mdlds=None,color='orange',marker='o',var1=var1,var2=var2,mdlsrc=mdlsrc);
          if ( label5 is None ):
            label5 = 'NGE-OHC';
            m[0].set_label(label5);
      
      # Did this AXBT happen to correspond with an Argo profile?? Look for distances < 9 ocean gridcells
      dists = calc_argo_dists(Argoq,(AXBTq.lon[xix],AXBTq.lat[xix]));
      if ( len(dists) > 0 ):
        #doubleix = np.where(dists<20)[0];
        doubleix = np.where(dists<80)[0];
        if ( len(doubleix) > 0 ):
          argo = get_argo_profile(Argoq,doubleix[0]);
          argoVar1 = getARGOVar1(argo,var1);
          argoVar2 = getARGOVar2(argo,var2);
          m,mm,l,ml=plot_profile(axT,axO,axP,argo.LONGITUDE,argo.LATITUDE,-argo.PRES,argoVar1,OHC=argoVar2,mdlds=mdlds,color='lightgreen',marker='s',linewidth=3.0,var1=var1,var2=var2,mdlsrc=mdlsrc);
          if ( label3 is None ):
            label3 = 'Argo';
            m[0].set_label(label3);
            label4 = 'Model';
            mm[0].set_label(label4);
          skipargoix.append(doubleix[0]);
          print(f'FOUND matching AXBT and Argo: {argo.LONGITUDE},{argo.LATITUDE}');
          #DEBUG:          breakpoint()
          #Argoq = remove_argo_profile(Argoq,doubleix[0]);
      
      # Always plot AXBT markers and profiles most prominently
      axbtVar1 = getAXBTVar1(AXBTq,var1,xix);
      axbtVar2 = getAXBTVar2(AXBTq,var2,xix);
      m,mm,l,ml=plot_profile(axT,axO,axP,AXBTq.lon[xix],AXBTq.lat[xix],-AXBTq.z,axbtVar1,axbtVar2,mdlds=mdlds,color='k',var1=var1,var2=var2,mdlsrc=mdlsrc);
      if ( label1 is None ):
        label1 = 'AXBT';
        m[0].set_label(label1);
        label2 = 'Model';
        mm[0].set_label(label2);
      
      axT.text(AXBTq.lon[xix],AXBTq.lat[xix],fr'$.     \leftarrow    {global_profile_num}$',color='k');
      axO.text(AXBTq.lon[xix],AXBTq.lat[xix],fr'$.     \leftarrow    {global_profile_num}$',color='k');
      #axP.text(AXBTq.T[xix][1],0,f'{global_profile_num}',color='k');
      if ( var1 == 'T' ):
        axP.text(31,-50,f'{global_profile_num}',color='k');
      elif ( var1 == 'D' or var1 == 'i26' ):
        axP.text(50,-50,f'{global_profile_num}',color='k');
      
      global_profile_num = global_profile_num + 1;
      
      cix = cix + 1
      if ( cix > maxcol ):
          cix = mincol;
          rix = rix + 1;
  
  ####
  # Handle any (remaining) Argos in the quadrant
  for aix,argo in enumerate(Argoq):
    # Did we already plot this Argo profile with an AXBT in the code above?
    if ( aix in skipargoix ):
      #DEBUG:      breakpoint()
      continue;
    if ( rix > maxrow ):
      break;
    if ( 'axP' not in locals() ):
      axP = plt.subplot2grid((nrows,ncols),(rix,cix),1,1,fig1)
    else:
      axP = plt.subplot2grid((nrows,ncols),(rix,cix),1,1,fig1,sharex=axP,sharey=axP);

    # Did this Argo happen to correspond with an NGE OHC track?
    if ( NGEq is not None ):
      ngedists = np.sqrt( ((NGEq.lon-argo.LONGITUDE)**2)  + ((NGEq.lat-argo.LATITUDE)**2) ).values * 111
      #doubleix = np.where(ngedists<18)[0];
      doubleix = np.where(ngedists<max_nge_dist)[0];
      if ( len(doubleix) > 0 ):
        ix = doubleix[0];
        # Plot NGE lowest in the z-ordering (vs. Argo - below)
        NGEqVar1 = getNGEVar1(NGEq,var1,ix);
        NGEqVar2 = getNGEVar2(NGEq,var2,ix);
        m,mm,l,ml=plot_profile(axT,axO,axP,NGEq.lon[ix],NGEq.lat[ix],-NGEq.pressure,NGEqVar1,OHC=NGEqVar2,mdlds=None,color='orange',marker='o',var1=var1,var2=var2,mdlsrc=mdlsrc);
        if ( label5 is None ):
          label5 = 'NGE-OHC';
          m[0].set_label(label5);
    
    argoVar1 = getARGOVar1(argo,var1);
    argoVar2 = getARGOVar2(argo,var2);
    m,mm,l,ml=plot_profile(axT,axO,axP,argo.LONGITUDE,argo.LATITUDE,-argo.PRES,argoVar1,OHC=argoVar2,mdlds=mdlds,color='lightgreen',marker='s',linewidth=3.0,var1=var1,var2=var2,mdlsrc=mdlsrc);
    if ( label3 is None ):
      label3 = 'Argo';
      m[0].set_label(label3);
      label4 = 'Model';
      mm[0].set_label(label4);
    # #axP.text(argo.TEMP[1],0,f'{global_profile_num}',color='k');
    # axT.text(argo.LONGITUDE,argo.LATITUDE,f'{global_profile_num}',color='k');
    # axO.text(argo.LONGITUDE,argo.LATITUDE,f'{global_profile_num}',color='k');
    axT.text(argo.LONGITUDE,argo.LATITUDE,fr'$.     \leftarrow    {global_profile_num}$',color='k');
    axO.text(argo.LONGITUDE,argo.LATITUDE,fr'$.     \leftarrow    {global_profile_num}$',color='k');
    if ( var1 == 'T' ):
      axP.text(31,-50,f'{global_profile_num}',color='k');
    elif ( var1 == 'S' ):
      axP.text(35,-50,f'{global_profile_num}',color='k');
    elif ( var1 == 'D' or var1 == 'i26' ):
      axP.text(50,-50,f'{global_profile_num}',color='k');
    global_profile_num = global_profile_num + 1;
    
    cix = cix + 1
    if ( cix > maxcol ):
      cix = mincol;
      rix = rix + 1;
  
  #DEBUG:  print(f'FINISH plot_quadrant_profiles: label1={label1}, label2={label2}, label3={label3}, label4={label4}, label5={label5}');
#plot_quadrant_profiles


def fig_title(
    fig: matplotlib.figure.Figure, txt: str, loc="center", fontdict=None, **kwargs
):
  """Alternative to fig.suptitle that behaves like ax.set_title. DO NOT use with suptitle.
  Adapted from:
   https://stackoverflow.com/questions/60840293/is-it-possible-to-have-more-than-one-suptitle
  See also:
   https://stackoverflow.com/a/77063164/8954109
  """
  
  if fontdict is not None:
    kwargs = {**fontdict, **kwargs}
  if "fontsize" not in kwargs and "size" not in kwargs:
    kwargs["fontsize"] = plt.rcParams["axes.titlesize"]
  
  if "fontweight" not in kwargs and "weight" not in kwargs:
    kwargs["fontweight"] = plt.rcParams["figure.titleweight"]
  
  default_y = 0.98
  default_va = 'top';
  if "y" not in kwargs:
    y = default_y
  
  x = 0
  if loc == "left":
    x = 0.02
    default_ha = 'left'
  elif loc == "center":
    x = 0.50
    default_ha = 'left'
  elif loc == "right":
    x = 0.98
    default_ha = 'right'
  else:
    raise ValueError(f"invalid loc: {loc}")
  
  if "verticalalignment" not in kwargs and "va" not in kwargs:
    kwargs["verticalalignment"] = default_va
  if "horizontalalignment" not in kwargs and "ha" not in kwargs:
    kwargs["horizontalalignment"] = default_ha
  
  # Tell the layout engine that our text is using space at the top of the figure
  # so that tight_layout does not break.
  # Is there a more direct way to do this?
  fig.suptitle(" ")
  texth = fig.text(x, y, txt, transform=fig.transFigure, in_layout=True, **kwargs)
  
  return texth
#fig_title


geod = None;
def azimuth_distance_wgs84(lon1,lat1,lon2,lat2,**kwargs):
  '''Calculate distance in [m] and azimuth of line between two points (lat/lon coordinates). Uses WGS84 datum by default.'''
  global geod
  if ( geod is None ):
    geod = pyproj.Geod(ellps='WGS84')
  az12,az21,dst = geod.inv(lon1,lat1,lon2,lat2);
  return(az12,dst);

def translate_wgs84(lon1,lat1,dst,az,**kwargs):
  '''Calculate lon,lat of new location DST m along azimuth AZ from points lon1,lat1. Uses WGS84 datum by default.'''
  global geod
  if ( geod is None ):
    geod = pyproj.Geod(ellps='WGS84')
  
  lon2,lat2,backaz = geod.fwd(np.array(lon1),np.array(lat1),np.array(az),np.array(dst));
  lon2 = np.array(lon2); lat2 = np.array(lat2); 
  lon2[lon2<0] = lon2[lon2<0] + 360
  return(lon2,lat2,backaz);

def kts2mps(kts):
  '''Convert from Knots to [m/s]'''
  mps = kts * 0.5144444444;
  return(mps);



##############################
def _parse_args():
  parser = argparse.ArgumentParser(description='GPLOT Ocean Obs plotter')
  parser.add_argument('--idate', required=True, help='Forecast init date YYYYMMDDHH')
  parser.add_argument('--sid', required=True, help='Storm ID (e.g. 13L)')
  parser.add_argument('--ocean-domain', required=True, dest='ocean_domain')
  parser.add_argument('--tier', required=True)
  parser.add_argument('--ensid', default='')
  parser.add_argument('--force', default='')
  parser.add_argument('--resolution', type=float, required=True)
  parser.add_argument('--rmax', type=float, required=True)
  parser.add_argument('--levs', type=int, required=True)
  parser.add_argument('--master-nml', required=True, dest='master_nml')
  parser.add_argument('--ocean-source', default='HYCOM', dest='ocean_source',
                      choices=['HYCOM', 'MOM6'])
  parser.add_argument('--ocean-cfg', default='NHC', dest='ocean_cfg')
  parser.add_argument('--fix-dir', default='', dest='fix_dir')
  parser.add_argument('--wrap-lon', action='store_true', default=False, dest='wrap_lon')
  return parser.parse_args()


def main():

  global global_profile_num;
  global label1, label2, label3, label4, label5

  args = _parse_args()
  IDATE        = args.idate
  SID          = args.sid
  OCEAN_DOMAIN = args.ocean_domain
  TIER         = args.tier
  ENSID        = ens_utils.normalize_ensid(args.ensid)
  FORCE        = args.force
  OCEAN_SOURCE = args.ocean_source or 'HYCOM'
  OCEAN_CFG    = args.ocean_cfg or 'NHC'
  OCEAN_WRAP_LON = 'True' if args.wrap_lon else 'False'  # keep string form for existing compares
  resolution = args.resolution
  rmax = args.rmax
  zsize_pressure = args.levs

  # Locate master namelist
  NMLIST = args.master_nml
  if os.path.exists(NMLIST):
    MASTER_NML_IN = NMLIST
  elif os.path.exists(os.path.join(GPLOT_DIR, 'parm', NMLIST)):
    MASTER_NML_IN = os.path.join(GPLOT_DIR, 'parm', NMLIST)
  else:
    print("ERROR: I couldn't find the Master Namelist.")
    sys.exit(1)

  PYTHONDIR = GPLOT_DIR + '/sorc/GPLOT/python'
  FIX_DIR = args.fix_dir.strip() if args.fix_dir else os.path.join(GPLOT_DIR, 'fix')

  # Read the master namelist (replaces subprocess grep calls)
  nml = nml_utils.read_master_namelist(MASTER_NML_IN)
  plot_utils.configure_cartopy(nml.get('CARTOPY_DIR'))
  DSOURCE       = nml.get('DSOURCE', 'HAFS')
  OCEAN_DSOURCE = (nml.get('OCEAN_DSOURCE') or DSOURCE).strip()
  EXPT          = nml.get('EXPT', '').strip()
  ODIR_base     = nml.get('ODIR', '').strip()
  try:
    ODIR_TYPE = int(nml.get('ODIR_TYPE', 0) or 0)
  except (TypeError, ValueError):
    ODIR_TYPE = 0
  DO_CONVERTGIF = bool(nml.get('DO_CONVERTGIF', False))

  # Ensemble member sub-directory ('' for deterministic -> unchanged path).
  ENS_SUB = (ens_utils.member_segment(ENSID) + '/') \
      if ens_utils.member_segment(ENSID) else ''
  if ODIR_TYPE == 1:
    ODIR = ODIR_base + '/' + ENS_SUB + 'ocean_' + OCEAN_DOMAIN + '_obs' + '/'
  else:
    ODIR = ODIR_base + '/' + EXPT + '/' + IDATE.strip() + '/' + ENS_SUB + 'ocean_' + OCEAN_DOMAIN + '_obs' + '/'

  figext  = '.png'
  figext2 = '.gif' if DO_CONVERTGIF else '.png'

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
    depths_arr = ocean_reader.read_hycom_depth(FIX_DIR, OCEAN_DSOURCE.lower(), OCEAN_CFG.lower())
    # Wrap in masked array so downstream ``depths.mask`` checks keep working.
    depths = ma.masked_invalid(depths_arr)
    DEPTH_FILE = os.path.join(
        FIX_DIR,
        f'{OCEAN_DSOURCE.lower()}_hycom_{OCEAN_CFG.lower()}.basin.regional.depth')
  else:
    DEPTH_FILE = f'{FIX_DIR.strip()}/{OCEAN_CFG.strip().lower()}/ocean_topog.nc';
    depths_ds = xr.open_dataset(DEPTH_FILE);
    depths = depths_ds.depth.values
    depths_ds.close();
  #DEBUG:
  print(f'DEBUG:: DEPTH_FILE={DEPTH_FILE}, shape={depths.shape}');

  # Read the plot title
  TBLDIR = GPLOT_DIR+'/tbl'
  print(f'EXPT --> {EXPT}');
  EXPT_TITLE = EXPT
  tbl_path = os.path.join(TBLDIR, 'ExptInfo.dat')
  if os.path.isfile(tbl_path):
    pat = re.compile(r'^\s+' + re.escape(EXPT) + r'\s*,')
    with open(tbl_path) as fh:
      for line in fh:
        if pat.match(line):
          parts = line.split(',')
          if len(parts) > 1:
            EXPT_TITLE = parts[1].strip()
          break
  print(f'EXPT_TITLE --> {EXPT_TITLE}');

  # Get the ATCF file.
  ATCF_LIST = np.genfromtxt(ODIR+'ATCF_FILES.dat',dtype='str')
  if ATCF_LIST.size > 1:
    print('Found multiple ATCFs')
    ATCF = ATCF_LIST[[i for i, s in enumerate(ATCF_LIST) if str(SID+'.').lower() in s][:]][0]
  else:
    ATCF = ATCF_LIST
  print('MSG: Found this ATCF --> '+str(ATCF))
  # Ensemble member ATCF is 00L-named, so use the requested SID instead of the
  # filename to identify the storm.
  if ENSID and SID:
    LONGSID = SID
  else:
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
  # Parse ATCF into DataFrame (replaces manual genfromtxt + string reversal).
  # read_atcf() already filters to the 34-kt wind radii rows, matching the
  # legacy "ATCF_DATA[:,11] contains '34'" filter.
  atcf_df = atcf_utils.read_atcf(str(ATCF))

  # Ensemble member ATCFs are multi-storm; keep only this storm.
  if ENSID and SID:
    atcf_df = ens_utils.filter_atcf_df(atcf_df, SID[-1], SID[:-1])


  # Get the list of unplotted files
  UNPLOTTED_LIST = np.array( np.genfromtxt(UNPLOTTED_FILE,dtype='str') )
  
  # Get the list of forecast lead time in hours
  FHR_LIST = np.array( np.genfromtxt(ALLFHR_FILE,dtype='int') )
  if (FHR_LIST.size == 1):
    FHR_LIST = np.append(FHR_LIST,"999")
    UNPLOTTED_LIST = np.append(UNPLOTTED_LIST,"MISSING")
  
  SSH0 = None
  dSSH = None
  DFHR = int(FHR_LIST[-1]) - int(FHR_LIST[-2])
  
  global_profile_num = 1;
  
  for (FILE,fff) in zip(UNPLOTTED_LIST,np.array(range(UNPLOTTED_LIST.size))):
    
    if (FILE == 'MISSING'):  continue
    
    print('MSG: Working on this file --> '+str(FILE)+'  '+str(fff))
    
    os.system('lockfile -r-1 -l 180 '+ST_LOCK_FILE)
    os.system('echo "working" > '+STATUS_FILE)
    os.system('rm -f '+ST_LOCK_FILE)
    
    # Get some useful information about the file name
    FILE_BASE = os.path.basename(FILE)
    FILE_DIR = os.path.dirname(FILE)
    
    # Find this forecast hour in the ATCF DataFrame.
    FHR = int(FHR_LIST[fff])
    row_mask = atcf_df['fhr'] == FHR
    if not row_mask.any():
      print(f'WARNING: fhr={FHR} not present in ATCF. Skipping.')
      plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
      continue
    row = atcf_df[row_mask].iloc[0]
    hist = atcf_df[atcf_df['fhr'] <= FHR].sort_values('fhr')

    # Get coordinate information from ATCF DataFrame (lat/lon already
    # decimal degrees, signed).
    centerlon = float(row['lon'])
    if centerlon < 0:
      centerlon = centerlon + 360
    centerlat = float(row['lat'])
    print('centerlon, centerlat = ', centerlon, centerlat)
    tracklats = list(hist['lat'].astype(float).values)
    tracklons = list(hist['lon'].astype(float).values)
    # Unwrap longitudes to match the old unwrap=True behaviour used for the
    # 6-hour projection below.
    tracklons = [ln + 360 if ln < 0 else ln for ln in tracklons]

    # Where do we forecast the storm will be in 6 h?
    tspeedkts = float(row['storm_speed']) / 10 if row['storm_speed'] != -99 else 0.0
    tdir      = float(row['storm_dir'])        if row['storm_dir']   != -99 else 0.0
    [projlon, projlat, backaz] = translate_wgs84(tracklons[-1], tracklats[-1],
                                                 kts2mps(tspeedkts) * 3600 * 6,
                                                 tdir)

    forecastinit = str(row['cycle'])
    maxwind      = str(int(row['vmax']))
    minpressure  = str(int(row['mslp']))
    rmwnmi       = str(int(row['rmw']))
    #trey additions start
    neq34 = float(row['rad_ne'])
    seq34 = float(row['rad_se'])
    swq34 = float(row['rad_sw'])
    nwq34 = float(row['rad_nw'])
    #treyend
    # HACK: This should be revisited.
    if centerlat > 50.0:
      print('WARNING: The latitude is poleward of +/- 50. Skipping.')
      # Write the input file to a log to mark that it has ben processed
      plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
      continue

    print(f'MSG: Searching for graphics products that match --> {ODIR}/*{LONGSID.lower()}*f{FHR:03}{figext2}')
    figuretest = np.shape([g for g in glob.glob(f"{ODIR}/*{LONGSID.lower()}*f{format(FHR,'03d')}{figext2}")])[0]
    if figuretest > 0:
      print(f'MSG: Found {figuretest} matching graphical products for this lead time.')
      print(f'MSG: Please delete all {figext2} files for this lead time to reproduce graphics. Skipping.')

      # Write the input file to a log to mark that it has ben processed
      plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
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


    # What is our time tolerance for finding observations associated with each forecast hour?
    delh = 8

    ################################################################################
    ########## HANDLE HYbrid Coordinate Ocean Model POST OUTPUT
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
      modelt = ds.MT.values[0]
      ucurr = ds.u_velocity.squeeze()
      vcurr = ds.v_velocity.squeeze()
      wcurr = ds.w_velocity.squeeze()
      print('MSG: Done With u,v,w')
      MLD = ds.mixed_layer_thickness.squeeze()
      #DEBUG:        MLD.plot(figsize=(11,9)); plt.savefig('3z.png');
      T = ds.temperature.squeeze()
      S = ds.salinity.squeeze()
      SST = T[0,...].squeeze();
      SSS = S[0,...].squeeze();
      OHC = ds.ocean_heat_content.squeeze()
      #i26 = ds['depth of 26C isotherm'].squeeze().to_masked_array()
      i26 = ds['depth of 26C isotherm'].squeeze()
      i20 = ds['depth of 20C isotherm'].squeeze()
      ds.close()
      ds['i26'] = i26
      ds['i20'] = i20
      ds['OHC'] = OHC
      ds['MLD'] = MLD
      print('MSG: Done with T, S, MLD, OHC, Iso')
      
      dZ = ds.Z.diff(dim='Z');
      MLu = ucurr.where(ds.Z < MLD).mean(axis=0)
      MLv = vcurr.where(ds.Z < MLD).mean(axis=0)
      MLT = T.where(ds.Z < MLD).mean(axis=0)
      MLS = S.where(ds.Z < MLD).mean(axis=0)
      ds['MLT'] = MLT
      ds['MLS'] = MLS
      ds['MLD'] = MLD
      print('MSG: Done with MLu, MLv, MLT, MLS')
      
      tmin = modelt - np.timedelta64(delh,'h')
      tmax = modelt + np.timedelta64(delh,'h')
      if ( FHR == 0 ):
        tmin = tmin - np.timedelta64(2,'D')
      
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
        if ( SSH0 is None ):
          SSH0 = ods.sea_surface_height.squeeze()
        if ( OCEAN_DOMAIN == 'd03' ):
          #DEBUG:          print(f'DEBUG: ods.where( {lonmin}<={all_ods.Longitude.min().values} & {all_ods.Longitude.max().values}<={lonmax} & {latmin}<={all_ods.Latitude.min().values} & {all_ods.Latitude.max().values}<={latmax} )');
          ods = ods.where((lonmin<=all_ods.Longitude) & (all_ods.Longitude<=lonmax) \
                & (latmin<=all_ods.Latitude) & (all_ods.Latitude<=latmax));
        SHF = ods.surface_heat_flux.squeeze()
        #DEBUG:        print('SHF',SHF.min().values,SHF.max().values)
        SSH = ods.sea_surface_height.squeeze()
        # This approach does not work well for d03 - we "lose" the domain pretty quickly
        # if ( SSH0 is None ):
        #   SSH0 = SSH
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
    
    ################################################################################
    ########## HANDLE Modular Ocean Model v6 POST OUTPUT
    elif ( OCEAN_SOURCE == 'MOM6' ):
      all_ds = all_ds.interp( {'xq':all_ds.xh, 'yq':all_ds.yh} )
      if ( OCEAN_WRAP_LON == 'True' ):
        print('MSG: Wrapping 3D Longitudes');
        all_ds['xh'] = all_ds.xh + 360
        all_ds['geolon'] = all_ds.geolon + 360
      #ds = all_ds.where(~depths.mask);
      ds = all_ds;
      if ( SSH0 is None ):
        SSH0 = ds.SSH.squeeze()*1e2 #[m]=>[cm]
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
      SST = T[0,...].squeeze();
      SSS = S[0,...].squeeze();
      #i26 = ds['depth of 26C isotherm'].squeeze().to_masked_array()
      i26 = ds.z_l.where(T>=26).max(axis=0)
      i20 = ds.z_l.where(T>=20).max(axis=0)
      delT = T - 26;
      delT = xr.where(delT > 0, delT, 0);
      dZ = ds.z_l.broadcast_like(delT).diff(0)
      delTdz = delT * dZ;
      OHC = cp*rho*delTdz.sum(axis=0) * kJcm2_per_Jm2;
      ds['i26'] = i26
      ds['i20'] = i20
      ds['OHC'] = OHC
      ds['MLD'] = MLD
      print('MSG: Done with T, S, MLD, Iso, OHC')
      
      MLu = ucurr.where(ds.z_l < MLD).mean(axis=0)
      MLv = vcurr.where(ds.z_l < MLD).mean(axis=0)
      MLT = T.where(ds.z_l < MLD).mean(axis=0)
      MLS = S.where(ds.z_l < MLD).mean(axis=0)
      ds['MLT'] = MLT
      ds['MLS'] = MLS
      print('MSG: Done with MLu, MLv, MLT, MLS')
      
      tmin = ds.time.values[0] - np.timedelta64(delh,'h')
      tmax = ds.time.values[0] + np.timedelta64(delh,'h')
      if ( FHR == 0 ):
        tmin = tmin - np.timedelta64(2,'D')

      #Get 2-d Data
      SHF = -(ds.LwLatSens.squeeze() + ds.SW.squeeze());
      #DEBUG:      print('SHF',SHF.min().values,SHF.max().values)
      SSH = ds.SSH.squeeze()*1e2 #[m]=>[cm]
      # This approach does not work well for d03 - we "lose" the domain pretty quickly
      # if ( SSH0 is None ):
      #   SSH0 = SSH
      dSSH = (SSH - SSH0) / (np.double(FHR)/24) #[cm/d]
      #DEBUG:      print('SSH',SSH.max().values)
      ds.close()
      print(f'MSG: Done with surface vars (e.g., redo of MLu,MLv) {datetime.now()}')
    
    else:
        print(f'ERROR: OCEAN_SOURCE {OCEAN_SOURCE} not yet handled by PLOT_OCEAN_MAPS.py!')
        sys.exit(1)
    
    lonstretch = np.cos(np.deg2rad(lon.mean()));
    secPerDay = 24*3600
    
    
    print(f'MSG: Getting Ocean Obs Now {datetime.now()}')

    print(f'WARNING: SKIPPING Glider Obs for Now {datetime.now()}')
    #gliders = load_glider_data(IDATE[0:8]);
    
    AXBTs = load_axbt_data(IDATE[0:8]);
    
    print('TEST TEST TEST')
    print(IDATE[0:8])
    Argos = load_argo_data(IDATE[0:8]);
    #Argos = load_argo_data(IDATE[0:8],ndays=(-5,-4,-3,-2,-1,0,1,2,3,4,5)); ### HACK HACK HACK
    
    #NGEs = load_nge_data(IDATE[0:8]);
    #Lew.Gramer@noaa.gov 2025-08-13: Turn off NAtl product display per PI request... Will reenable with Gulf product soon
    NGEs = None;
    
    AXBTsNW = get_axbt_subset(AXBTs,lonmin,centerlon,centerlat,latmax,tmin,tmax);
    if (len(AXBTsNW)>0): AXBTsNW = AXBTsNW.sortby('lon')
    AXBTsNE = get_axbt_subset(AXBTs,centerlon,lonmax,centerlat,latmax,tmin,tmax);
    if (len(AXBTsNE)>0): AXBTsNE = AXBTsNE.sortby('lon')
    AXBTsSW = get_axbt_subset(AXBTs,lonmin,centerlon,latmin,centerlat,tmin,tmax);
    if (len(AXBTsSW)>0): AXBTsSW = AXBTsSW.sortby('lon')
    AXBTsSE = get_axbt_subset(AXBTs,centerlon,lonmax,latmin,centerlat,tmin,tmax);
    if (len(AXBTsSE)>0): AXBTsSE = AXBTsSE.sortby('lon')
    
    ####ArgoHits = get_argo_subset(Argos,find_domain_argos(Argos,[lonmin,latmin,lonmax,latmax]));
    #ArgosNW = get_argo_subset(Argos,find_domain_argos(Argos,[lonmin,centerlat,centerlon,latmax],[tmin-np.timedelta64(5,'D'),tmax+np.timedelta64(5,'D')])); ArgosNW.sort(key=(lambda x: x.LONGITUDE)); ### HACK HACK HACK
    ArgosNW = get_argo_subset(Argos,find_domain_argos(Argos,[lonmin,centerlat,centerlon,latmax],[tmin,tmax]));
    if (len(ArgosNW)>0): ArgosNW.sort(key=(lambda x: x.LONGITUDE))
    ArgosNE = get_argo_subset(Argos,find_domain_argos(Argos,[centerlon,centerlat,lonmax,latmax],[tmin,tmax]));
    if (len(ArgosNE)>0): ArgosNE.sort(key=(lambda x: x.LONGITUDE));
    ArgosSW = get_argo_subset(Argos,find_domain_argos(Argos,[lonmin,latmin,centerlon,centerlat],[tmin,tmax]));
    if (len(ArgosSW)>0): ArgosSW.sort(key=(lambda x: x.LONGITUDE));
    ArgosSE = get_argo_subset(Argos,find_domain_argos(Argos,[centerlon,latmin,lonmax,centerlat],[tmin,tmax]));
    if (len(ArgosSE)>0): ArgosSE.sort(key=(lambda x: x.LONGITUDE));
    print(f'MSG: Done with Ocean Obs (e.g., Gliders, NGE-OHC, AXBTS, Argo) {datetime.now()}')
    
    #Make Plots
    print(f'MSG: Doing Plots Now {datetime.now()}')
    try:
      if os.path.exists(f'{NMLDIR}/namelist.ocean_obs.{EXPT}'):
        namelist_structure_vars = np.genfromtxt(f'{NMLDIR}/namelist.ocean_obs.{EXPT}',delimiter=',',dtype='str')
      else:
        namelist_structure_vars = np.genfromtxt(f'{NMLDIR}/namelist.ocean_obs',delimiter=',',dtype='str')
      do_ships_output = namelist_vars[0,1]
      do_sst_ohc_profiles = namelist_vars[1,1]
      do_dsst_dohc_profiles = namelist_vars[2,1]
      do_ssh_ssh_tendency_fields = namelist_vars[3,1]
      do_iso_26_ohc_profiles = namelist_vars[4,1]
      do_iso_20_mld_profiles = namelist_vars[5,1]
      do_ssh_tendency_iso_26_tendency = namelist_vars[6,1]
      do_delta_t_delta_q_shf_fields = namelist_vars[7,1]
      do_dpi_profiles = namelist_vars[8,1]
      do_sss_mls_profiles = namelist_vars[9,1]
      do_sst_mlt_profiles = namelist_vars[10,1]
      do_iso_26_mlt_profiles = namelist_vars[11,1]
      do_mlt_mld_profiles = namelist_vars[12,1]
    except:
      do_ships_output = 'Y'
      do_sst_ohc_profiles = 'Y'
      do_dsst_dohc_profiles = 'N'
      do_ssh_ssh_tendency_fields = 'N'
      do_iso_26_ohc_profiles = 'N'
      do_iso_20_mld_profiles = 'N'
      do_ssh_tendency_iso_26_tendency = 'N'
      do_delta_t_delta_q_shf_fields = 'N'
      do_dpi_profiles = 'N'
      do_sss_mls_profiles = 'Y'
      do_sst_mlt_profiles = 'Y'
      do_iso_26_mlt_profiles = 'N'
      do_mlt_mld_profiles = 'Y'
    
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
      SST_levs = np.arange(26,30+1e-6,0.2);		SST_ticks = np.arange(26,30+1e-6,0.5)
      SSS_levs = np.arange(34,37+1e-6,0.2);		SSS_ticks = np.arange(34,37+1e-6,0.5)
      MLT_levs = np.arange(26,30+1e-6,0.2);		MLT_ticks = np.arange(26,30+1e-6,0.5)
      MLS_levs = np.arange(34,37+1e-6,0.2);		MLS_ticks = np.arange(34,37+1e-6,0.5)
    
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
      SST_levs = np.arange(26,30+1e-6,0.2);		SST_ticks = np.arange(26,30+1e-6,0.5)
      SSS_levs = np.arange(34,37+1e-6,0.2);		SSS_ticks = np.arange(34,37+1e-6,0.5)
      MLT_levs = np.arange(26,30+1e-6,0.2);		MLT_ticks = np.arange(26,30+1e-6,0.5)
      MLS_levs = np.arange(34,37+1e-6,0.2);		MLS_ticks = np.arange(34,37+1e-6,0.5)
    
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
    xi = np.linspace(float(lon.min()),float(lon.max()),lon.shape[0]);
    yi = np.linspace(float(lat.min()),float(lat.max()),lat.shape[0]);
    
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
      try:
        if ((np.size(wherecov) / np.size(where500)) < .5):
          T500 = np.nan
          OHC500 = np.nan
        else: 
          T500 = np.nanmean(T0[where500])
          OHC500 = np.nanmean(OHC[where500])
      except:
          T500 = np.nan
          OHC500 = np.nan
      
      try:
        if ((np.size(wherecov) / np.size(where200)) < .5):
          T200 = np.nan
          OHC200 = np.nan
        else:
          T200 = np.nanmean(T0[where200])
          OHC200 = np.nanmean(OHC[where200])
      except:
          T200 = np.nan
          OHC200 = np.nan
      
      #sst where 34kt quadrant wind---------
      where34 = np.where(((bearings < 90.) & (rad_distances < neq34)) | ((bearings < 180.) & (bearings >= 90.) & (rad_distances < seq34)) | ((bearings < 270.) & (bearings >= 180.) & (rad_distances < swq34)) | ((bearings < 360.) & (bearings >= 270.) & (rad_distances < nwq34)))
      print(np.size(where34))
      print(neq34)
      T34 = np.nanmean(T0[where34])
      OHC34 = np.nanmean(OHC[where34])

      # #write to files----------
      # sstfname = ODIR+'/'+LONGSID.lower()+'.ships.sst.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.dat'
      # ohcfname = ODIR+'/'+LONGSID.lower()+'.ships.ohc.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'.dat'
      # 
      # f = open(sstfname,'a+')
      # print(f'{FHR}, {T200}, {T500}, {T34} ',file=f)
      # f.close()      
      # 
      # f = open(ohcfname,'a+')    
      # print(f'{FHR}, {OHC200}, {OHC500}, {OHC34}',file=f)
      # f.close()
      # 
      # pass;

    # FIGURE: SST and OHC comparisons
    if do_sst_ohc_profiles == 'Y':
      global_profile_num=1;
      label1=None;
      label2=None;
      label3=None;
      label4=None;
      label5=None;
      
      fig1 = plt.figure(figsize=figsize)

      axT = plt.subplot2grid((nrows,ncols),(1,0),3,3,fig1)
      coT = axT.contourf(lon,lat, SST, levels=SST_levs, cmap=Tcmap,extend='both')
      debug_dump_range(FHR,'SST',SST)
      cbarT = plt.colorbar(coT, ticks=SST_ticks)
      cbarT.ax.tick_params(labelsize=fontsize) #labelsize=24
      axT.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      print('depths',np.nanmin(depths),np.nanmax(depths));
      add_center_label(axT,centerlon,centerlat,minpressure);
      axT.tick_params(labelsize=fontsize) #labelsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
        axT.set_xlim([lonmin,lonmax]); axT.set_ylim([latmin,latmax]);
      #DEBUG:      print(f'axT.plot({tracklons},{tracklats}...');
      axT.plot(tracklons,tracklats,'ko-');
      # axT.arrow(centerlon,centerlat, centerlon-projlon,centerlat-projlat,
      #           width=2, head_width=10, head_length=10, fc='blue', ec='black');
      axT.arrow(centerlon,centerlat, projlon-centerlon,projlat-centerlat, width=0.1,fc='blue', ec='black');
      
      axO = plt.subplot2grid((nrows,ncols),(1,3),3,3,fig1)
      coO = axO.contourf(lon,lat, OHC, levels=OHC_levs, cmap=OHCcmap,extend='both')
      debug_dump_range(FHR,'OHC',OHC)
      cbarO = plt.colorbar(coO, ticks=OHC_ticks)
      cbarO.ax.tick_params(labelsize=fontsize) #labelsize=24
      #axO.contour(lon,lat,SST,levels=SST_cnts,linewidths=2); # Plot SST contours
      axO.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(axO,centerlon,centerlat,minpressure);
      axO.tick_params(labelsize=fontsize) #labelsize=24
      # if ( OCEAN_DOMAIN == 'd03' ):
      #     Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=0.5);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      # else:
      #     #Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=5.0);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
        axO.set_xlim([lonmin,lonmax]); axO.set_ylim([latmin,latmax]);
      #DEBUG:      print(f'axO.plot({tracklons},{tracklats}...');
      axO.plot(tracklons,tracklats,'ko-');
      # axO.arrow(centerlon,centerlat, centerlon-projlon,centerlat-projlat,
      #           width=2, head_width=10, head_length=10, fc='blue', ec='black');
      axO.arrow(centerlon,centerlat, projlon-centerlon,projlat-centerlat, width=0.1,fc='blue', ec='black');
      
      # axT.plot(NGEs.lon,NGEs.lat,'y.'); #,color=Tcmap(Tnorm(NGEs.SST)));
      # axO.plot(NGEs.lon,NGEs.lat,'y.'); #,color=OHCcmap(OHCnorm(NGEs.OHC)));
      
      if ( NGEs is not None ):
        axT.scatter(NGEs.lon,NGEs.lat,c=Tcmap(Tnorm(NGEs.CalcSST)),marker='.',s=18);
        axO.scatter(NGEs.lon,NGEs.lat,c=OHCcmap(OHCnorm(NGEs.CalcOHC)),marker='.',s=18);
      
      # No room on the figure for this!
      #texth = fig1.text(0.50, 0.44, 'Ocean Temperature Profiles Arranged by Storm Quadrant',
      #                 transform=fig1.transFigure, in_layout=True, ha='center',fontsize=16)
      
      fig1.add_artist(matplotlib.lines.Line2D([0.00, 1.00], [0.14, 0.14], linestyle='--',color='k'))
      fig1.add_artist(matplotlib.lines.Line2D([0.50, 0.50], [0.00, 0.42], linestyle='--',color='k'))
      
      plot_quadrant_profiles(fig1,axT,axO,AXBTsNW,ArgosNW,NGEs,None,None,None,4,5,0,2,ds,var1='T',var2='OHC',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,AXBTsNE,ArgosNE,NGEs,None,None,None,4,5,3,5,ds,var1='T',var2='OHC',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,AXBTsSW,ArgosSW,NGEs,None,None,None,6,6,0,2,ds,var1='T',var2='OHC',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,AXBTsSE,ArgosSE,NGEs,None,None,None,6,6,3,5,ds,var1='T',var2='OHC',mdlsrc=OCEAN_SOURCE)
      
      fig_title(fig1,EXPT_TITLE.strip()+'\n'+ r'Sea Surace Temperature ($^oC$; left), Ocean Heat Content ($kJ\ cm^{-2}$; right)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      fig_title(fig1,'VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right')
      
      #fig1.legend(loc=(0.28,0.84), ncol=5)
      #DEBUG:      print(f'LEGEND: label1={label1}, label2={label2}, label3={label3}, label4={label4}, label5={label5}');
      nlegends = np.count_nonzero([label1,label2,label3,label4,label5])
      #legendxpos = 0.20 + (0.065*(5-nlegends))
      legendxpos = 0.20 + (0.125*(5-nlegends))
      try:
        if ( nlegends > 0 ):
          fig1.legend(loc=(legendxpos,0.90), ncol=nlegends)
      except:
        pass;
      
      plt.tight_layout()
      
      # if ( OCEAN_DOMAIN == 'd03' ):
      #     Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=0.5);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      # else:
      #     #Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=5.0);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      #axO.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right')
      #print(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR))
      
      figfname = ODIR+'/'+LONGSID.lower()+'.sst_ohc_profs.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'_obs'+'.f'+format(FHR,'03d')
      #DEBUB:
      print(figfname+figext);
      plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)


    if do_sst_mlt_profiles == 'Y':
      global_profile_num=1;
      label1=None;
      label2=None;
      label3=None;
      label4=None;
      label5=None;
      
      fig1 = plt.figure(figsize=figsize)

      axT = plt.subplot2grid((nrows,ncols),(1,0),3,3,fig1)
      coT = axT.contourf(lon,lat, SST, levels=SST_levs, cmap=Tcmap,extend='both')
      debug_dump_range(FHR,'SST',SST)
      cbarT = plt.colorbar(coT, ticks=SST_ticks)
      cbarT.ax.tick_params(labelsize=fontsize) #labelsize=24
      axT.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      print('depths',np.nanmin(depths),np.nanmax(depths));
      add_center_label(axT,centerlon,centerlat,minpressure);
      axT.tick_params(labelsize=fontsize) #labelsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
        axT.set_xlim([lonmin,lonmax]); axT.set_ylim([latmin,latmax]);
      #DEBUG:      print(f'axT.plot({tracklons},{tracklats}...');
      axT.plot(tracklons,tracklats,'ko-');
      # axT.arrow(centerlon,centerlat, centerlon-projlon,centerlat-projlat,
      #           width=2, head_width=10, head_length=10, fc='blue', ec='black');
      axT.arrow(centerlon,centerlat, projlon-centerlon,projlat-centerlat, width=0.1,fc='blue', ec='black');
      
      axO = plt.subplot2grid((nrows,ncols),(1,3),3,3,fig1)
      coO = axO.contourf(lon,lat, MLT, levels=MLT_levs, cmap=Tcmap,extend='both')
      debug_dump_range(FHR,'MLT',MLT)
      cbarO = plt.colorbar(coO, ticks=MLT_ticks)
      cbarO.ax.tick_params(labelsize=fontsize) #labelsize=24
      #axO.contour(lon,lat,SST,levels=SST_cnts,linewidths=2); # Plot SST contours
      axO.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(axO,centerlon,centerlat,minpressure);
      axO.tick_params(labelsize=fontsize) #labelsize=24
      # if ( OCEAN_DOMAIN == 'd03' ):
      #     Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=0.5);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      # else:
      #     #Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=5.0);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
        axO.set_xlim([lonmin,lonmax]); axO.set_ylim([latmin,latmax]);
      #DEBUG:      print(f'axO.plot({tracklons},{tracklats}...');
      axO.plot(tracklons,tracklats,'ko-');
      # axO.arrow(centerlon,centerlat, centerlon-projlon,centerlat-projlat,
      #           width=2, head_width=10, head_length=10, fc='blue', ec='black');
      axO.arrow(centerlon,centerlat, projlon-centerlon,projlat-centerlat, width=0.1,fc='blue', ec='black');
      
      if ( NGEs is not None ):
        axT.scatter(NGEs.lon,NGEs.lat,c=Tcmap(Tnorm(NGEs.CalcSST)),marker='.',s=18);
        axO.scatter(NGEs.lon,NGEs.lat,c=Tcmap(Tnorm(NGEs.CalcMLT)),marker='.',s=18);
      
      # No room on the figure for this!
      #texth = fig1.text(0.50, 0.44, 'Ocean Temperature Profiles Arranged by Storm Quadrant',
      #                 transform=fig1.transFigure, in_layout=True, ha='center',fontsize=16)
      
      fig1.add_artist(matplotlib.lines.Line2D([0.00, 1.00], [0.14, 0.14], linestyle='--',color='k'))
      fig1.add_artist(matplotlib.lines.Line2D([0.50, 0.50], [0.00, 0.42], linestyle='--',color='k'))
      
      plot_quadrant_profiles(fig1,axT,axO,AXBTsNW,ArgosNW,NGEs,None,None,None,4,5,0,2,ds,var1='T',var2='T',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,AXBTsNE,ArgosNE,NGEs,None,None,None,4,5,3,5,ds,var1='T',var2='T',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,AXBTsSW,ArgosSW,NGEs,None,None,None,6,6,0,2,ds,var1='T',var2='T',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,AXBTsSE,ArgosSE,NGEs,None,None,None,6,6,3,5,ds,var1='T',var2='T',mdlsrc=OCEAN_SOURCE)
      
      fig_title(fig1,EXPT_TITLE.strip()+'\n'+ r'Sea Surace Temperature ($^oC$; left), Mixed-Layer Temperature ($^oC$; right)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      fig_title(fig1,'VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right')
      
      #fig1.legend(loc=(0.28,0.84), ncol=5)
      #DEBUG:      print(f'LEGEND: label1={label1}, label2={label2}, label3={label3}, label4={label4}, label5={label5}');
      nlegends = np.count_nonzero([label1,label2,label3,label4,label5])
      legendxpos = 0.20 + (0.125*(5-nlegends))
      try:
        if ( nlegends > 0 ):
          fig1.legend(loc=(legendxpos,0.90), ncol=nlegends)
      except:
        pass;
      
      plt.tight_layout()
      
      # if ( OCEAN_DOMAIN == 'd03' ):
      #     Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=0.5);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      # else:
      #     #Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=5.0);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      #axO.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right')
      #print(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR))
      
      figfname = ODIR+'/'+LONGSID.lower()+'.sst_mlt_profs.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'_obs'+'.f'+format(FHR,'03d')
      #DEBUB:
      print(figfname+figext);
      plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)
    
    
    if do_iso_26_mlt_profiles == 'Y':
      global_profile_num=1;
      label1=None;
      label2=None;
      label3=None;
      label4=None;
      label5=None;
      
      fig1 = plt.figure(figsize=figsize)

      axT = plt.subplot2grid((nrows,ncols),(1,0),3,3,fig1)
      coT = axT.contourf(lon,lat, i26, levels=iso_26_levs, cmap=Dcmap,extend='both')
      debug_dump_range(FHR,'i26',i26)
      cbarT = plt.colorbar(coT, ticks=iso_26_ticks)
      cbarT.ax.tick_params(labelsize=fontsize) #labelsize=24
      axT.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      print('depths',np.nanmin(depths),np.nanmax(depths));
      add_center_label(axT,centerlon,centerlat,minpressure);
      axT.tick_params(labelsize=fontsize) #labelsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
        axT.set_xlim([lonmin,lonmax]); axT.set_ylim([latmin,latmax]);
      #DEBUG:      print(f'axT.plot({tracklons},{tracklats}...');
      axT.plot(tracklons,tracklats,'ko-');
      # axT.arrow(centerlon,centerlat, centerlon-projlon,centerlat-projlat,
      #           width=2, head_width=10, head_length=10, fc='blue', ec='black');
      axT.arrow(centerlon,centerlat, projlon-centerlon,projlat-centerlat, width=0.1,fc='blue', ec='black');
      
      axO = plt.subplot2grid((nrows,ncols),(1,3),3,3,fig1)
      coO = axO.contourf(lon,lat, MLT, levels=MLT_levs, cmap=Tcmap,extend='both')
      debug_dump_range(FHR,'MLT',MLT)
      cbarO = plt.colorbar(coO, ticks=MLT_ticks)
      cbarO.ax.tick_params(labelsize=fontsize) #labelsize=24
      #axO.contour(lon,lat,SST,levels=SST_cnts,linewidths=2); # Plot SST contours
      axO.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(axO,centerlon,centerlat,minpressure);
      axO.tick_params(labelsize=fontsize) #labelsize=24
      # if ( OCEAN_DOMAIN == 'd03' ):
      #     Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=0.5);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      # else:
      #     #Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=5.0);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
        axO.set_xlim([lonmin,lonmax]); axO.set_ylim([latmin,latmax]);
      #DEBUG:      print(f'axO.plot({tracklons},{tracklats}...');
      axO.plot(tracklons,tracklats,'ko-');
      # axO.arrow(centerlon,centerlat, centerlon-projlon,centerlat-projlat,
      #           width=2, head_width=10, head_length=10, fc='blue', ec='black');
      axO.arrow(centerlon,centerlat, projlon-centerlon,projlat-centerlat, width=0.1,fc='blue', ec='black');
      
      if ( NGEs is not None ):
        axT.scatter(NGEs.lon,NGEs.lat,c=Dcmap(Dnorm(NGEs.Z26)),marker='.',s=18);
        axO.scatter(NGEs.lon,NGEs.lat,c=Tcmap(Tnorm(NGEs.CalcMLT)),marker='.',s=18);
      
      # No room on the figure for this!
      #texth = fig1.text(0.50, 0.44, 'Ocean Temperature Profiles Arranged by Storm Quadrant',
      #                 transform=fig1.transFigure, in_layout=True, ha='center',fontsize=16)
      
      fig1.add_artist(matplotlib.lines.Line2D([0.00, 1.00], [0.14, 0.14], linestyle='--',color='k'))
      fig1.add_artist(matplotlib.lines.Line2D([0.50, 0.50], [0.00, 0.42], linestyle='--',color='k'))
      
      plot_quadrant_profiles(fig1,axT,axO,AXBTsNW,ArgosNW,NGEs,None,None,None,4,5,0,2,ds,var1='i26',var2='T',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,AXBTsNE,ArgosNE,NGEs,None,None,None,4,5,3,5,ds,var1='i26',var2='T',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,AXBTsSW,ArgosSW,NGEs,None,None,None,6,6,0,2,ds,var1='i26',var2='T',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,AXBTsSE,ArgosSE,NGEs,None,None,None,6,6,3,5,ds,var1='i26',var2='T',mdlsrc=OCEAN_SOURCE)
      
      fig_title(fig1,EXPT_TITLE.strip()+'\n'+ r'26 C Isotherm ($m$; left), Mixed-Layer Temperature ($^oC$; right)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      fig_title(fig1,'VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right')
      
      #fig1.legend(loc=(0.28,0.84), ncol=5)
      #DEBUG:      print(f'LEGEND: label1={label1}, label2={label2}, label3={label3}, label4={label4}, label5={label5}');
      nlegends = np.count_nonzero([label1,label2,label3,label4,label5])
      legendxpos = 0.20 + (0.125*(5-nlegends))
      try:
        if ( nlegends > 0 ):
          fig1.legend(loc=(legendxpos,0.90), ncol=nlegends)
      except:
        pass;
      
      plt.tight_layout()
      
      # if ( OCEAN_DOMAIN == 'd03' ):
      #     Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=0.5);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      # else:
      #     #Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=5.0);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      #axO.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right')
      #print(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR))
      
      figfname = ODIR+'/'+LONGSID.lower()+'.iso_26_mlt_profs.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'_obs'+'.f'+format(FHR,'03d')
      #DEBUB:
      print(figfname+figext);
      plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)


    if do_mlt_mld_profiles == 'Y':
      global_profile_num=1;
      label1=None;
      label2=None;
      label3=None;
      label4=None;
      label5=None;
      
      fig1 = plt.figure(figsize=figsize)

      axT = plt.subplot2grid((nrows,ncols),(1,0),3,3,fig1)
      coT = axT.contourf(lon,lat, MLT, levels=MLT_levs, cmap=Tcmap,extend='both')
      debug_dump_range(FHR,'MLT',MLT)
      cbarT = plt.colorbar(coT, ticks=MLT_ticks)
      cbarT.ax.tick_params(labelsize=fontsize) #labelsize=24
      axT.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      print('depths',np.nanmin(depths),np.nanmax(depths));
      add_center_label(axT,centerlon,centerlat,minpressure);
      axT.tick_params(labelsize=fontsize) #labelsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
        axT.set_xlim([lonmin,lonmax]); axT.set_ylim([latmin,latmax]);
      #DEBUG:      print(f'axT.plot({tracklons},{tracklats}...');
      axT.plot(tracklons,tracklats,'ko-');
      # axT.arrow(centerlon,centerlat, centerlon-projlon,centerlat-projlat,
      #           width=2, head_width=10, head_length=10, fc='blue', ec='black');
      axT.arrow(centerlon,centerlat, projlon-centerlon,projlat-centerlat, width=0.1,fc='blue', ec='black');
      
      axO = plt.subplot2grid((nrows,ncols),(1,3),3,3,fig1)
      coO = axO.contourf(lon,lat, MLD, levels=MLD_levs, cmap=Dcmap,extend='both')
      debug_dump_range(FHR,'MLD',MLD)
      cbarO = plt.colorbar(coO, ticks=MLD_ticks)
      cbarO.ax.tick_params(labelsize=fontsize) #labelsize=24
      #axO.contour(lon,lat,SST,levels=SST_cnts,linewidths=2); # Plot SST contours
      axO.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(axO,centerlon,centerlat,minpressure);
      axO.tick_params(labelsize=fontsize) #labelsize=24
      # if ( OCEAN_DOMAIN == 'd03' ):
      #     Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=0.5);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      # else:
      #     #Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=5.0);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
        axO.set_xlim([lonmin,lonmax]); axO.set_ylim([latmin,latmax]);
      #DEBUG:      print(f'axO.plot({tracklons},{tracklats}...');
      axO.plot(tracklons,tracklats,'ko-');
      # axO.arrow(centerlon,centerlat, centerlon-projlon,centerlat-projlat,
      #           width=2, head_width=10, head_length=10, fc='blue', ec='black');
      axO.arrow(centerlon,centerlat, projlon-centerlon,projlat-centerlat, width=0.1,fc='blue', ec='black');
      
      if ( NGEs is not None ):
        axT.scatter(NGEs.lon,NGEs.lat,c=Tcmap(Tnorm(NGEs.CalcMLT)),marker='.',s=18);
        #axO.scatter(NGEs.lon,NGEs.lat,c=Dcmap(Dnorm(NGEs.CalcMLD)),marker='.',s=18);
        axO.scatter(NGEs.lon,NGEs.lat,c=Dcmap(Dnorm(NGEs.MLD)),marker='.',s=18);
      
      # No room on the figure for this!
      #texth = fig1.text(0.50, 0.44, 'Ocean Temperature Profiles Arranged by Storm Quadrant',
      #                 transform=fig1.transFigure, in_layout=True, ha='center',fontsize=16)
      
      fig1.add_artist(matplotlib.lines.Line2D([0.00, 1.00], [0.14, 0.14], linestyle='--',color='k'))
      fig1.add_artist(matplotlib.lines.Line2D([0.50, 0.50], [0.00, 0.42], linestyle='--',color='k'))
      
      plot_quadrant_profiles(fig1,axT,axO,AXBTsNW,ArgosNW,NGEs,None,None,None,4,5,0,2,ds,var1='T',var2='D',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,AXBTsNE,ArgosNE,NGEs,None,None,None,4,5,3,5,ds,var1='T',var2='D',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,AXBTsSW,ArgosSW,NGEs,None,None,None,6,6,0,2,ds,var1='T',var2='D',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,AXBTsSE,ArgosSE,NGEs,None,None,None,6,6,3,5,ds,var1='T',var2='D',mdlsrc=OCEAN_SOURCE)
      
      fig_title(fig1,EXPT_TITLE.strip()+'\n'+ r'Mixed-Layer Temperature ($^oC$; left), Mixed-Layer Depth ($m$; right)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      fig_title(fig1,'VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right')
      
      #fig1.legend(loc=(0.28,0.84), ncol=5)
      #DEBUG:      print(f'LEGEND: label1={label1}, label2={label2}, label3={label3}, label4={label4}, label5={label5}');
      nlegends = np.count_nonzero([label1,label2,label3,label4,label5])
      legendxpos = 0.20 + (0.125*(5-nlegends))
      try:
        if ( nlegends > 0 ):
          fig1.legend(loc=(legendxpos,0.90), ncol=nlegends)
      except:
        pass;
      
      plt.tight_layout()
      
      # if ( OCEAN_DOMAIN == 'd03' ):
      #     Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=0.5);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      # else:
      #     #Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=5.0);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      #axO.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right')
      #print(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR))
      
      figfname = ODIR+'/'+LONGSID.lower()+'.mlt_mld_profs.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'_obs'+'.f'+format(FHR,'03d')
      #DEBUB:
      print(figfname+figext);
      plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)


    if do_sss_mls_profiles == 'Y':
      global_profile_num=1;
      label1=None;
      label2=None;
      label3=None;
      label4=None;
      label5=None;
      
      fig1 = plt.figure(figsize=figsize)
      
      axT = plt.subplot2grid((nrows,ncols),(1,0),3,3,fig1)
      coT = axT.contourf(lon,lat, SSS, levels=SSS_levs, cmap=Scmap,extend='both')
      debug_dump_range(FHR,'SSS',SSS)
      cbarT = plt.colorbar(coT, ticks=SSS_ticks)
      cbarT.ax.tick_params(labelsize=fontsize) #labelsize=24
      axT.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      print('depths',np.nanmin(depths),np.nanmax(depths));
      add_center_label(axT,centerlon,centerlat,minpressure);
      axT.tick_params(labelsize=fontsize) #labelsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
        axT.set_xlim([lonmin,lonmax]); axT.set_ylim([latmin,latmax]);
      #DEBUG:      print(f'axT.plot({tracklons},{tracklats}...');
      axT.plot(tracklons,tracklats,'ko-');
      # axT.arrow(centerlon,centerlat, centerlon-projlon,centerlat-projlat,
      #           width=2, head_width=10, head_length=10, fc='blue', ec='black');
      axT.arrow(centerlon,centerlat, projlon-centerlon,projlat-centerlat, width=0.1,fc='blue', ec='black');
      
      axO = plt.subplot2grid((nrows,ncols),(1,3),3,3,fig1)
      coO = axO.contourf(lon,lat, MLS, levels=MLS_levs, cmap=Scmap,extend='both')
      debug_dump_range(FHR,'MLS',MLS)
      cbarO = plt.colorbar(coO, ticks=MLS_ticks)
      cbarO.ax.tick_params(labelsize=fontsize) #labelsize=24
      axO.contour(lon,lat,depths,levels=[150],colors='lightblue',linestyles='--',linewidths=3); # Plot the 150 m isobath
      add_center_label(axO,centerlon,centerlat,minpressure);
      axO.tick_params(labelsize=fontsize) #labelsize=24
      # if ( OCEAN_DOMAIN == 'd03' ):
      #     Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=0.5);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      # else:
      #     #Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=5.0);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      if ( OCEAN_DOMAIN == 'd03' ):
        axO.set_xlim([lonmin,lonmax]); axO.set_ylim([latmin,latmax]);
      #DEBUG:      print(f'axO.plot({tracklons},{tracklats}...');
      axO.plot(tracklons,tracklats,'ko-');
      # axO.arrow(centerlon,centerlat, centerlon-projlon,centerlat-projlat,
      #           width=2, head_width=10, head_length=10, fc='blue', ec='black');
      axO.arrow(centerlon,centerlat, projlon-centerlon,projlat-centerlat, width=0.1,fc='blue', ec='black');
      
      if ( NGEs is not None ):
        axT.scatter(NGEs.lon,NGEs.lat,c=Scmap(Snorm(NGEs.CalcSSS)),marker='.',s=18);
        axO.scatter(NGEs.lon,NGEs.lat,c=Scmap(Snorm(NGEs.CalcMLS)),marker='.',s=18);
      
      # No room on the figure for this!
      #texth = fig1.text(0.50, 0.44, 'Ocean Temperature Profiles Arranged by Storm Quadrant',
      #                 transform=fig1.transFigure, in_layout=True, ha='center',fontsize=16)
      
      fig1.add_artist(matplotlib.lines.Line2D([0.00, 1.00], [0.14, 0.14], linestyle='--',color='k'))
      fig1.add_artist(matplotlib.lines.Line2D([0.50, 0.50], [0.00, 0.42], linestyle='--',color='k'))
      
      plot_quadrant_profiles(fig1,axT,axO,None,ArgosNW,NGEs,None,None,None,4,5,0,2,ds,var1='S',var2='S',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,None,ArgosNE,NGEs,None,None,None,4,5,3,5,ds,var1='S',var2='S',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,None,ArgosSW,NGEs,None,None,None,6,6,0,2,ds,var1='S',var2='S',mdlsrc=OCEAN_SOURCE)
      plot_quadrant_profiles(fig1,axT,axO,None,ArgosSE,NGEs,None,None,None,6,6,3,5,ds,var1='S',var2='S',mdlsrc=OCEAN_SOURCE)
      
      fig_title(fig1,EXPT_TITLE.strip()+'\n'+ r'Sea Surace Salinity ($o/ooo$; left), Mixed-Layer Salinity ($o/ooo$; right)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      fig_title(fig1,'VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right')
      
      #fig1.legend(loc=(0.28,0.84), ncol=5)
      #DEBUG:      print(f'LEGEND: label1={label1}, label2={label2}, label3={label3}, label4={label4}, label5={label5}');
      nlegends = np.count_nonzero([label1,label2,label3,label4,label5])
      legendxpos = 0.20 + (0.125*(5-nlegends))
      try:
        if ( nlegends > 0 ):
          fig1.legend(loc=(legendxpos,0.90), ncol=nlegends)
      except:
        pass;
      
      plt.tight_layout()
      
      # if ( OCEAN_DOMAIN == 'd03' ):
      #     Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=0.5);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      # else:
      #     #Axes.streamplot(axO,xi,yi,MLu,MLv,color='gray',density=5.0);
      #     axO.set_title(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      #axO.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right')
      #print(EXPT_TITLE.strip()+'\n'+ r'Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR))
      
      figfname = ODIR+'/'+LONGSID.lower()+'.sss_mls_profs.'+forecastinit+'.ocean_'+OCEAN_DOMAIN+'_obs'+'.f'+format(FHR,'03d')
      #DEBUB:
      print(figfname+figext);
      plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)


    # Write the name of the input file to a log to mark that it has ben processed
    plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
    print(f'MSG: Done with Plots {datetime.now()}')
  
  # Retry-convert any orphan .png left behind by transient ImageMagick
  # failures. If the retry also fails, write status='incomplete' so
  # the workflow re-invokes us next iteration.
  _sweep = plot_utils.sweep_orphan_pngs(ODIR)
  _status_value = 'incomplete' if _sweep.get('still_failed', 0) > 0 else 'complete'
  if _status_value == 'incomplete':
      print(f"WARNING: ocean_obs: {_sweep['still_failed']} PNG(s) still "
            f"unconverted after sweep; writing status='incomplete'.")

  print('MSG: COMPLETING')
  os.system('lockfile -r-1 -l 180 '+ST_LOCK_FILE)
  os.system('echo "'+_status_value+'" > '+STATUS_FILE)
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


# NOTE: The legacy update_plottedfile() has been replaced by
# gplot_utils.plot_utils.update_plotted_file(); see Session C of the
# legacy-Python refactor in memory/ncl_to_python_migration.md.


##############################
if __name__ == '__main__':
  main()
