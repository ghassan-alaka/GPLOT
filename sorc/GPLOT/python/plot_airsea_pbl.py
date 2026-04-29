#!/usr/bin/env python

# Check that GPLOT_DIR is defined in the environment.
import os, time, warnings
GPLOT_DIR = os.environ['GPLOT_DIR']
print('MSG: Found this GPLOT location --> '+GPLOT_DIR)

#Import necessary modules
print('MSG: Importing Everything Needed')
from datetime import datetime
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
import argparse
import re
import sys #To change the path
import glob
import math
import cmath
from mpl_toolkits.axes_grid1 import make_axes_locatable

# GPLOT utility package (Sessions 1-7 infrastructure)
from gplot_utils import namelist as nml_utils
from gplot_utils import atcf as atcf_utils
from gplot_utils import plot_utils
from gplot_utils import grib_reader
from gplot_utils import constants as gplot_const

# COUNTIES / STATES are now loaded inside main() via
# plot_utils.load_county_state_shapes(nml.get('CARTOPY_DIR')) so a single
# CARTOPY_DIR namelist entry drives the path on every host. The legacy
# module-scope try/except (with hardwired ``/home/role.aoml-hafs1/.local/share/cartopy``
# vs ``/home/ahazelto/.local/share/cartopy`` fallback) was removed.


def debug_dump_range(FHR,varnm,var):
  print(f'DEBUG: FHR {int(FHR)}: {varnm} in {np.nanmin(var)},{np.nanpercentile(var,25)},{np.nanmedian(var)},{np.nanpercentile(var,75)},{np.nanmax(var)}');
  pass;

def add_center_label(ax1,centerlon,centerlat,minpressure):
  ax1.text(centerlon,centerlat,f'{minpressure}\n  L',color='black',fontsize=28,fontweight='extra bold');
  ax1.text(centerlon,centerlat,f'{minpressure}\n  L',color='red',fontsize=28);

##############################
def _parse_args():
  parser = argparse.ArgumentParser(description='GPLOT AirSea/PBL plotter')
  parser.add_argument('--idate', required=True, help='Forecast init date YYYYMMDDHH')
  parser.add_argument('--sid', required=True, help='Storm ID (e.g. 13L)')
  parser.add_argument('--domain', required=True)
  parser.add_argument('--tier', required=True)
  parser.add_argument('--ensid', default='')
  parser.add_argument('--force', default='')
  parser.add_argument('--resolution', type=float, required=True)
  parser.add_argument('--rmax', type=float, required=True)
  parser.add_argument('--levs', type=int, required=True)
  parser.add_argument('--master-nml', required=True, dest='master_nml')
  return parser.parse_args()


def main():

  # Log some important information
  print(f'MSG: plot_airsea_pbl.py began at {datetime.now()}')
  print('')
  print('MSG: Welcome to GPLOT, AirSea Module.')
  print('MSG: GPLOT is the Graphical Post-processed Locus for Output for Tropical cyclones.')
  print('MSG: The AirSea Module produces graphical products that focus on the air-sea interface')
  print('MSG: and related fields.')

  # Parse command-line args (argparse replaces the old sys.argv[1:11] block)
  args = _parse_args()
  IDATE      = args.idate      if args.idate != 'MISSING' else ''
  SID        = args.sid        if args.sid   != 'MISSING' else ''
  DOMAIN     = args.domain     if args.domain!= 'MISSING' else ''
  TIER       = args.tier       if args.tier  != 'MISSING' else ''
  ENSID      = args.ensid      if args.ensid != 'MISSING' else ''
  FORCE      = args.force      if args.force != 'MISSING' else ''
  resolution = args.resolution
  rmax       = args.rmax
  zsize_pressure = args.levs

  NMLDIR = GPLOT_DIR + '/parm'
  NMLIST = args.master_nml
  if os.path.exists(NMLIST):
    MASTER_NML_IN = NMLIST
  elif os.path.exists(os.path.join(GPLOT_DIR, 'parm', NMLIST)):
    MASTER_NML_IN = os.path.join(GPLOT_DIR, 'parm', NMLIST)
  else:
    print("ERROR: I couldn't find the Master Namelist.")
    sys.exit(1)
  PYTHONDIR = GPLOT_DIR + '/sorc/GPLOT/python'


  # Read the master namelist via nml_utils (replaces subprocess.grep calls)
  nml = nml_utils.read_master_namelist(MASTER_NML_IN)
  plot_utils.configure_cartopy(nml.get('CARTOPY_DIR'))
  COUNTIES, STATES = plot_utils.load_county_state_shapes(nml.get('CARTOPY_DIR'))
  DSOURCE = (nml.get('DSOURCE') or 'HAFS').strip()
  EXPT    = (nml.get('EXPT') or '').strip()
  ODIR    = (nml.get('ODIR') or '').strip()
  BASEDIR = ODIR
  try:
    ODIR_TYPE = int(nml.get('ODIR_TYPE', 0) or 0)
  except (TypeError, ValueError):
    ODIR_TYPE = 0
  if ODIR_TYPE == 1:
    ODIR = ODIR + '/airsea/'
    BASEDIR = BASEDIR + '/'
  else:
    ODIR = ODIR + '/' + EXPT + '/' + IDATE.strip() + '/airsea/'
    BASEDIR = BASEDIR + '/' + EXPT + '/' + IDATE.strip() + '/'

  DO_CONVERTGIF = bool(nml.get('DO_CONVERTGIF', False))
  figext  = '.png'
  figext2 = '.gif' if DO_CONVERTGIF else '.png'

  # Define some important file names
  UNPLOTTED_FILE = ODIR.strip()+'UnplottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
  PLOTTED_FILE = ODIR.strip()+'PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
  ALLFHR_FILE = ODIR.strip()+'AllForecastHours.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
  STATUS_FILE = ODIR.strip()+'status.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log'
  ST_LOCK_FILE = ODIR.strip()+'status.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log.lock'
  ATCF_FILE = ODIR.strip()+'ATCF_FILES.dat'


  # Read the plot title from tbl/ExptInfo.dat using a Python regex loop
  TBLDIR = GPLOT_DIR + '/tbl'
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
  # read_atcf() already filters to the 34-kt wind-radii rows, matching the
  # legacy ATCF_DATA[:,11]=='34' filter.
  atcf_df = atcf_utils.read_atcf(str(ATCF))


  # Get the list of unplotted files
  UNPLOTTED_LIST = np.array( np.genfromtxt(UNPLOTTED_FILE,dtype='str') )
  
  # Get the list of forecast lead time in hours
  FHR_LIST = np.array( np.genfromtxt(ALLFHR_FILE,dtype='int') )
  if (FHR_LIST.size == 1):
    FHR_LIST = np.append(FHR_LIST,"999")
    UNPLOTTED_LIST = np.append(UNPLOTTED_LIST,"MISSING")
  
  for (FILE,fff) in zip(UNPLOTTED_LIST,np.array(range(UNPLOTTED_LIST.size))):

    if (FILE == 'MISSING'):  continue

    print('MSG: Working on this file --> '+str(FILE)+'  '+str(fff))

    os.system('lockfile -r-1 -l 180 '+ST_LOCK_FILE)
    os.system('echo "working" > '+STATUS_FILE)
    os.system('rm -f '+ST_LOCK_FILE)

    # Get some useful information about the file name
    FILE_BASE = os.path.basename(FILE)
    FILE_DIR = os.path.dirname(FILE)

    # Find this forecast hour in the ATCF DataFrame (replaces manual
    # genfromtxt + column-5 linear scan + string-reverse lat/lon parsing).
    FHR = int(FHR_LIST[fff])
    row_mask = atcf_df['fhr'] == FHR
    if not row_mask.any():
      print(f'WARNING: fhr={FHR} not present in ATCF. Skipping.')
      plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
      continue
    row = atcf_df[row_mask].iloc[0]

    centerlon = float(row['lon'])
    if centerlon < 0:
      centerlon = centerlon + 360
    centerlat = float(row['lat'])
    print(f'centerlon, centerlat = {centerlon}, {centerlat}')
    forecastinit = str(row['cycle'])
    maxwind      = str(int(row['vmax']))
    minpressure  = str(int(row['mslp']))
    rmwnmi       = str(int(row['rmw']))

    # HACK: This should be revisited.
    #if centerlat > 50.0:
    #  print('WARNING: The latitude is poleward of +/- 50. Skipping.')
    #  # Write the input file to a log to mark that it has ben processed
    #  plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
    #  continue

    # Search for matching graphics that have already been produced for this particular file/lead time.
    print(f'MSG: Searching for graphics products that match --> {ODIR}/*{LONGSID.lower()}*f{FHR:03}{figext2}')
    figuretest = np.shape([g for g in glob.glob(f"{ODIR}/*{LONGSID.lower()}*f{FHR:03}{figext2}")])[0]
    if figuretest > 0:
      print(f'MSG: Found {figuretest} matching graphical products for this lead time.')
      print(f'MSG: Please delete all {figext2} files for this lead time to reproduce graphics. Skipping.')

      # Write the input file to a log to mark that it has ben processed
      plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
      continue

    print(f'MSG: I can\'t find the graphical products for this lead time (figuretest={figuretest}). Proceeding.')

    # Check that the data file 'FILE' exists
    if not os.path.exists(FILE):
      print(f'MSG: The input file does not exist. Nothing to do. Skipping.')
      continue

    # Determine the lat/lon bounding box that contains the requested rmax.
    # This is identical to the legacy py3grads "set lat/lon" walk, but is
    # now applied as a grib_reader subset rather than a GrADS clip.
    yoffset = 6
    xoffset = None
    NL = yoffset-1
    while not xoffset:
      if NL > 25:
        print(f'ERROR: YOU NEED A BIGGER BOX THAN {NL} DEGREES. rmax={rmax}, centerlat={centerlat}')
        sys.exit(1)
      NL = NL+1
      test = np.cos((abs(centerlat)+yoffset)*3.14159/180)*111.1*NL
      if test > rmax:  xoffset,yoffset = NL,NL
    print(f'MSG: Will use a box with side of {NL} degrees.')

    lonmin = centerlon - xoffset
    lonmax = centerlon + xoffset
    latmin = centerlat - yoffset
    latmax = centerlat + yoffset
    # grib_reader bounds: (lat_n, lat_s, lon_w, lon_e)
    bounds = (latmax, latmin, lonmin, lonmax)

    # Open GRIB2 file via xarray+cfgrib (replaces g2ctl.pl + gribmap + ga('open')).
    print('MSG: Getting data now via xarray+cfgrib.')
    start = time.perf_counter()
    try:
      datasets = grib_reader.open_grib2(FILE)
    except Exception as exc:
      print(f'ERROR: Could not open GRIB2 file {FILE}: {exc}')
      continue

    # Helper: request a 3D pressure-level field, return (ny, nx, nz) array.
    # grib_reader returns (lev, lat, lon); transpose to match the legacy
    # axis order expected by downstream slicing (e.g. ws[:,:,6]).
    def _fetch3d(var):
      r = grib_reader.get_var_3d(datasets, DSOURCE, var, 1.0, 1100.0,
                                 bounds=bounds)
      if r is None:
        raise RuntimeError(f'3D variable {var} missing from {FILE}')
      return (np.transpose(np.asarray(r['data']), (1, 2, 0)),
              np.asarray(r['lat']),
              np.asarray(r['lon']),
              np.asarray(r['lev']))

    def _fetch2d(var, level=''):
      r = grib_reader.get_var_2d(datasets, DSOURCE, var, level=level,
                                 bounds=bounds)
      if r is None:
        raise RuntimeError(f'2D variable {var} (level={level}) missing from {FILE}')
      return np.asarray(r['data']).squeeze()

    # --- 3D pressure-level fields -------------------------------------------------
    # grib_reader auto-converts wind from m/s to knots; the airsea script
    # expects m/s for density/gust/metpy calculations, so convert back.
    uwind_kt, lat, lon, lev1d = _fetch3d('U')
    vwind_kt, _, _, _         = _fetch3d('V')
    uwind = uwind_kt * gplot_const.kts2ms
    vwind = vwind_kt * gplot_const.kts2ms

    # Normalize centerlon to match the convention of the lon array returned
    # by grib_reader. centerlon was wrapped to 0..360 above (for the
    # rectangular bounds calculation), but HAFS GRIB2 lon arrays come back
    # in -180..180 form. The mismatch silently corrupts ``lon_sr =
    # lon - centerlon`` and pushes ``add_center_label`` text out by ~360°,
    # which inflates the saved bbox to a 19:1 aspect ratio on every
    # non-cartopy figure.
    if float(lon.max()) <= 180.0 and centerlon > 180.0:
      centerlon = centerlon - 360.0
    elif float(lon.min()) >= 0.0 and centerlon < 0.0:
      centerlon = centerlon + 360.0
    print(f'MSG: centerlon normalized to lon convention: centerlon={centerlon:.4f}, '
          f'lon range=[{float(lon.min()):.4f}, {float(lon.max()):.4f}]')

    omega,    _, _, _ = _fetch3d('OMEGA')    # Pa/s (no unit conversion)
    dbz,      _, _, _ = _fetch3d('REFL')
    # HGT is auto-converted m -> dam by grib_reader; restore meters
    hgt_dam,  _, _, _ = _fetch3d('HGT')
    hgt = hgt_dam * 10.0
    temp,     _, _, _ = _fetch3d('T')        # K
    q,        _, _, _ = _fetch3d('Q')        # kg/kg
    rh,       _, _, _ = _fetch3d('RH')       # %

    # Truncate to zsize_pressure levels (legacy "set z 1 {zsize_pressure}").
    ny, nx, nz = uwind.shape
    if nz > zsize_pressure:
      uwind = uwind[:, :, :zsize_pressure]
      vwind = vwind[:, :, :zsize_pressure]
      omega = omega[:, :, :zsize_pressure]
      dbz   = dbz[:, :, :zsize_pressure]
      hgt   = hgt[:, :, :zsize_pressure]
      temp  = temp[:, :, :zsize_pressure]
      q     = q[:, :, :zsize_pressure]
      rh    = rh[:, :, :zsize_pressure]
      lev1d = lev1d[:zsize_pressure]
      nz = zsize_pressure
    z = np.asarray(lev1d, dtype=float)
    # Broadcast pressure levels to full 3D for use in np.where((550<=levs)&...).
    levs = np.broadcast_to(z[np.newaxis, np.newaxis, :], (ny, nx, nz)).copy()

    # --- 2D surface fields --------------------------------------------------------
    sst = _fetch2d('SST')
    # Guard against SST occasionally returning a 3D array (legacy 'wtmpsfc' hack)
    if sst.ndim > 2:
      print('WARNING: SST had three dimensions!')
      sst = sst[..., 0].squeeze()

    lhtflx = _fetch2d('LHFLX')
    shtflx = _fetch2d('SHFLX')
    dlwflx = _fetch2d('DLWRF')
    ulwflx = _fetch2d('ULWRF')
    dswflx = _fetch2d('DSWRF')
    uswflx = _fetch2d('USWRF')
    print('MSG: Done with [ls]htflx, [du][sl]wrf')

    # 10-m wind -- grib_reader returns kt; convert back to m/s for downstream use
    u10 = _fetch2d('U', level='10') * gplot_const.kts2ms
    v10 = _fetch2d('V', level='10') * gplot_const.kts2ms

    # MSLP: grib_reader converts Pa -> hPa automatically.
    if DSOURCE == 'HAFS':
      mslp = _fetch2d('MSLP')
    else:
      mslp = _fetch2d('PRMSL')

    tmp2m = _fetch2d('T',  level='2')
    q2m   = _fetch2d('Q',  level='2')
    rh2m  = _fetch2d('RH', level='2')
    print('MSG: Done with u10,v10,mslp,tmp2m,q2m')

    # Dead-code density diagnostics (kept for parity with legacy script).
    mixr2m = q2m/(1-q2m)
    temp_v_2m = tmp2m*(1+0.61*mixr2m)
    # rho2m uses MSLP in Pa; grib_reader returned hPa, so rescale.
    rho2m = (mslp*100.0)/(gplot_const.R_d*temp_v_2m)

    # Friction velocity
    ustar = _fetch2d('FRICV')
    print(f'MSG: Done with surface vars (e.g., u10,v10) {datetime.now()}')

    # 850/200 hPa single-level fields for shear diagnostics
    u850 = _fetch2d('U',   level='850') * gplot_const.kts2ms
    v850 = _fetch2d('V',   level='850') * gplot_const.kts2ms
    z850 = _fetch2d('HGT', level='850') * 10.0   # dam -> m
    u200 = _fetch2d('U',   level='200') * gplot_const.kts2ms
    v200 = _fetch2d('V',   level='200') * gplot_const.kts2ms
    z200 = _fetch2d('HGT', level='200') * 10.0

    # Close datasets now that all data is materialised into NumPy arrays.
    for _ds in datasets:
      try:
        _ds.close()
      except Exception:
        pass

    finish = time.perf_counter()
    print(f'MSG: Total time to read data: {finish-start:.2f} second(s)')

    # Get W from Omega (rho = p / (Rd * Tv); pressures must be in Pa).
    mixr = q/(1-q)
    temp_v = temp*(1+0.61*mixr)
    rho = (levs*1e2)/(gplot_const.R_d*temp_v)
    wwind = -omega/(rho*gplot_const.g)
    
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
    # Module namelist read via nml_utils (replaces raw np.genfromtxt CSV).
    # read_airsea_namelist returns {flag: bool}; translate back to 'Y'/'N'
    # strings so the existing plot guards keep working.
    _nml_candidates = [
      f'{NMLDIR}/namelist.airsea.pbl.{EXPT}',
      f'{NMLDIR}/namelist.airsea.pbl',
    ]
    _nml_path = next((p for p in _nml_candidates if os.path.isfile(p)),
                     _nml_candidates[-1])
    _airsea_flags = nml_utils.read_airsea_namelist(_nml_path)
    def _yn(key):
      return 'Y' if _airsea_flags.get(key, False) else 'N'
    do_turb_flux   = _yn('do_turb_flux')
    do_total_flux  = _yn('do_total_flux')
    do_theta_e_550 = _yn('do_theta_e_550')
    do_theta_e_700 = _yn('do_theta_e_700')
    do_theta_e_850 = _yn('do_theta_e_850')
    do_delta_t     = _yn('do_delta_t')
    do_delta_q     = _yn('do_delta_q')
    do_gusts       = _yn('do_gusts')
    
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
    
    color_data_wind = np.genfromtxt(GPLOT_DIR+'/sorc/GPLOT/python/colormaps/colormap_wind.txt')
    colormap_wind = matplotlib.colors.ListedColormap(color_data_wind)
    levs_wind = np.linspace(0,140,71)
    norm_wind = colors.BoundaryNorm(levs_wind,256)

    levs_gf = np.linspace(1,2,21)
    norm_gf = colors.BoundaryNorm(levs_gf,256)

    #turb_flux_levs = np.linspace(-50,1350,15,endpoint=True)
    turb_flux_levs = np.arange(-400,1400+1e-6,50.0);  turb_flux_ticks = np.arange(-400,1400+1e-6,100.0)
    total_flux_levs = np.arange(-500,2000+1e-6,50.0);  total_flux_ticks = np.arange(-500,2000+1e-6,100.0);
    theta_e_550_levs = np.arange(330,380+1e-6,2.0);    theta_e_550_ticks = np.arange(330,380+1e-6,5.0)
    theta_e_700_levs = np.arange(330,380+1e-6,2.0);    theta_e_700_ticks = np.arange(330,380+1e-6,5.0)
    theta_e_850_levs = np.arange(330,380+1e-6,2.0);    theta_e_850_ticks = np.arange(330,380+1e-6,5.0)
    delta_t_levs = np.arange(-6,12+1e-6,0.2);          delta_t_ticks = np.arange(-6,12+1e-6,0.5)
    #delta_q_levs = np.arange(0.5,2.5+1e-6,0.05);       delta_q_ticks = np.arange(0.5,2.5+1e-6,0.1)
    delta_q_levs = np.arange(1.05,1.20+1e-6,0.002);    delta_q_ticks = np.arange(1.05,1.20+1e-6,0.01)
    
    DELTA_T = sst - temp[...,0].squeeze();
    # DPT=SST at sfc
    sfcq = mpcalc.specific_humidity_from_dewpoint(mslp.squeeze()*metpy.units.units.hPa,\
                                                  (sst+273.15)*metpy.units.units.K)
    DELTA_Q = sfcq.squeeze() - q[...,0].squeeze();
    DPT = mpcalc.dewpoint_from_specific_humidity(levs*metpy.units.units.hPa,\
                                                 temp*metpy.units.units.K,\
                                                 q*metpy.units.units("kg/kg"))
    THETA_E = mpcalc.equivalent_potential_temperature(levs*metpy.units.units.hPa,\
                                                      temp*metpy.units.units.K,\
                                                      DPT);
    
    if ( np.all(np.isnan(THETA_E)) ):
      print(f'WARNING: THETA_E ALL NaNs in {FILE}: Skipping this forecast hour')
      plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
      continue

    
    #Calculate Wind Gusts
    wind10=np.squeeze(np.hypot(u10,v10))
    ws=np.hypot(uwind,vwind)
    ws850=np.squeeze(ws[:,:,6])
    ws950=np.squeeze(ws[:,:,2])
    wsd=ws850-ws950;
    wsd[wsd < 0] = 0

    #Calculate Default Gust Factor 
    gust1_old=7.71*ustar

    ws1_old=wind10
    wstt1_old=ws1_old+gust1_old
    gf1_old=wstt1_old/ws1_old

    gust2_old=0.6*wsd;

    wstt2_old=(wstt1_old+gust2_old)
    gf2_old=wstt2_old/ws1_old

    #Now Calculate a New Gust Factor 
    gust1_new=3*ustar

    ws1_new=wind10
    wstt1_new=ws1_new+gust1_new
    gf1_new=wstt1_new/ws1_new

    gust2_new=0.3*wsd;

    wstt2_new=(wstt1_new+gust2_new)
    gf2_new=wstt2_new/ws1_new


    # Streamplots require equally spaced x and y
    print(float(lon.min()),float(lon.max()),lon.shape[0])
    xi = np.linspace(float(lon.min()),float(lon.max()),lon.shape[0]);
    yi = np.linspace(float(lat.min()),float(lat.max()),lat.shape[0]);

    # Plot extent for the non-cartopy figures: clip to the data bounds so
    # any stray artist (text, runaway streamplot trajectory, future
    # annotation) cannot stretch the saved bbox.
    plot_xlim = (float(lon.min()), float(lon.max()))
    plot_ylim = (float(lat.min()), float(lat.max()))

    figsize = (24,24);
    fontsize = 24
    small_fontsize = 24
    # Default for axis labels, etc.
    plt.rcParams.update({'font.size': 20})
    
    # FIGURE: Total turbulent heat flux (enthalpy flux) at the sea surface
    if do_turb_flux == 'Y':
      turb_flux = lhtflx + shtflx;
      
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, turb_flux, levels=turb_flux_levs, extend='both')
      # ax1 = axes_radhgt(ax1, rmax, 0)
      cbar1 = plt.colorbar(co1, ticks=turb_flux_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      add_center_label(ax1,centerlon,centerlat,minpressure);
      Axes.streamplot(ax1,xi,yi,u10,v10,color='gray',density=0.5);
      ax1.set_title(EXPT_TITLE.strip()+'\n'+ r'Enthalpy Fluxes ($W\ m^{-2}$, Shading), U$_{10m}$ ($m\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      ax1.set_xlim(plot_xlim); ax1.set_ylim(plot_ylim)
      figfname = ODIR+'/'+LONGSID.lower()+'.turb_flux.'+forecastinit+'.airsea.f'+format(FHR,'03d')
      plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)
    # FIGURE: Total net heat flux (turbulent+radiative) at the sea surface
    if do_total_flux == 'Y':
      total_flux = lhtflx + shtflx - dlwflx + ulwflx - dswflx + uswflx;
      
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, total_flux, levels=total_flux_levs, extend='both')
      # ax1 = axes_radhgt(ax1, rmax, 0)
      cbar1 = plt.colorbar(co1, ticks=total_flux_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      add_center_label(ax1,centerlon,centerlat,minpressure);
      Axes.streamplot(ax1,xi,yi,u10,v10,color='gray',density=0.5);
      ax1.set_title(EXPT_TITLE.strip()+'\n'+ r'Sfc. Ht. Fluxes ($W\ m^{-2}$, Shading), U$_{10m}$ ($m\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      ax1.set_xlim(plot_xlim); ax1.set_ylim(plot_ylim)
      figfname = ODIR+'/'+LONGSID.lower()+'.total_flux.'+forecastinit+'.airsea.f'+format(FHR,'03d')
      plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)
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
      add_center_label(ax1,centerlon,centerlat,minpressure);
      Axes.streamplot(ax1,xi,yi,uwind_550,vwind_550,color='gray',density=0.5);
      ax1.set_title(EXPT_TITLE.strip()+'\n'+ r'550 hPa Equiv. Pot. Temp. (K, Shading), Wind ($m\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      ax1.set_xlim(plot_xlim); ax1.set_ylim(plot_ylim)
      figfname = ODIR+'/'+LONGSID.lower()+'.theta_e_550.'+forecastinit+'.airsea.f'+format(FHR,'03d')
      plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)
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
      add_center_label(ax1,centerlon,centerlat,minpressure);
      Axes.streamplot(ax1,xi,yi,uwind_700,vwind_700,color='gray',density=0.5);
      ax1.set_title(EXPT_TITLE.strip()+'\n'+ r'700 hPa Equiv. Pot. Temp. (K, Shading), Wind ($m\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      ax1.set_xlim(plot_xlim); ax1.set_ylim(plot_ylim)
      figfname = ODIR+'/'+LONGSID.lower()+'.theta_e_700.'+forecastinit+'.airsea.f'+format(FHR,'03d')
      plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)
    # FIGURE: Equivalent potential temperature below 850 hPa
    if do_theta_e_850 == 'Y':
      THETA_E_850 = np.nanmean(np.where((850<=levs), THETA_E, np.nan),axis=2);
      uwind_850 = np.nanmean(np.where((850<=levs), uwind, np.nan),axis=2);
      vwind_850 = np.nanmean(np.where((850<=levs), vwind, np.nan),axis=2);
      
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      # #co1 = ax1.contourf(lon,lat, THETA_E, levs_th, \
      # # co1 = ax1.contourf(lon,lat, THETA_E_850, \
      # #       cmap=colormap_th, norm=norm_th, transform=ccrs.PlateCarree(), extend='both')
      #co1 = ax1.contourf(lon,lat, THETA_E_850, cmap=colormap_th, norm=norm_th, extend='both')
      co1 = ax1.contourf(lon,lat, THETA_E_850, levels=theta_e_850_levs, extend='both')
      # ax1 = axes_radhgt(ax1, rmax, 0)
      cbar1 = plt.colorbar(co1, ticks=theta_e_850_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      add_center_label(ax1,centerlon,centerlat,minpressure);
      Axes.streamplot(ax1,xi,yi,uwind_850,vwind_850,color='gray',density=0.5);
      ax1.set_title(EXPT_TITLE.strip()+'\n'+ r'850 hPa Equiv. Pot. Temp. (K, Shading), Wind ($m\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      ax1.set_xlim(plot_xlim); ax1.set_ylim(plot_ylim)
      figfname = ODIR+'/'+LONGSID.lower()+'.theta_e_850.'+forecastinit+'.airsea.f'+format(FHR,'03d')
      plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)
    # FIGURE: Air-sea temperature contrast
    if do_delta_t == 'Y':
      print('DELTA_T', np.nanmin(DELTA_T), np.nanmean(DELTA_T), np.nanmax(DELTA_T), int(maxwind));
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, DELTA_T, levels=delta_t_levs, cmap='seismic',extend='both')
      # ax1 = axes_radhgt(ax1, rmax, 0)
      cbar1 = plt.colorbar(co1, ticks=delta_t_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      add_center_label(ax1,centerlon,centerlat,minpressure);
      Axes.streamplot(ax1,xi,yi,uwind_850,vwind_850,color='gray',density=0.5);
      ax1.set_title(EXPT_TITLE.strip()+'\n'+ r'Air-Sea Temp. Contrast (K, Shading), U$_{10m}$ ($m\ s^{-1}$, Strmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      ax1.set_xlim(plot_xlim); ax1.set_ylim(plot_ylim)
      figfname = ODIR+'/'+LONGSID.lower()+'.delta_t.'+forecastinit+'.airsea.f'+format(FHR,'03d')
      plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)
    # FIGURE: Air-sea specific humidity contrast
    if do_delta_q == 'Y':
      print('DELTA_Q', np.nanmin(DELTA_Q), np.nanmean(DELTA_Q), np.nanmax(DELTA_Q), int(maxwind));
      fig1 = plt.figure(figsize=figsize)
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(lon,lat, DELTA_Q, levels=delta_q_levs, cmap='seismic',extend='both')
      #DEBUG:      debug_dump_range(FHR,'DELTA_Q',DELTA_Q)
      # ax1 = axes_radhgt(ax1, rmax, 0)
      cbar1 = plt.colorbar(co1, ticks=delta_q_ticks)
      cbar1.ax.tick_params(labelsize=fontsize) #labelsize=24
      add_center_label(ax1,centerlon,centerlat,minpressure);
      Axes.streamplot(ax1,xi,yi,uwind_850,vwind_850,color='gray',density=0.5);
      ax1.set_title(EXPT_TITLE.strip()+'\n'+ r'Air-Sea Sp. Hum. Contrast (g/km, Shading), U$_{10m}$ ($m\ s^{-1}$, Stmlns.)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
      ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
      ax1.set_xlim(plot_xlim); ax1.set_ylim(plot_ylim)
      figfname = ODIR+'/'+LONGSID.lower()+'.delta_q.'+forecastinit+'.airsea.f'+format(FHR,'03d')
      plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)


    # FIGURE: Wind Gusts
    if do_gusts == 'Y':
        #Make 6x6 plot of Wind Gusts
        lonplotmin = centerlon-3
        lonplotmax = centerlon+3
        latplotmin = centerlat-3
        latplotmax = centerlat+3
        lonplot = np.arange(int(round(lonplotmin,0))-1,int(round(lonplotmax,0))+1,1)
        latplot = np.arange(int(round(latplotmin,0))-1,int(round(latplotmax,0))+1,1)

        fig1 = plt.figure(figsize=(15.5,15.5))
        ax1 = fig1.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())
        ax1.set_extent([lonplotmin,lonplotmax,latplotmin,latplotmax], crs=ccrs.PlateCarree())
        plt.contourf(lon, lat, wstt2_new*1.94, levs_wind, cmap=colormap_wind, norm=norm_wind, extend='both', transform=ccrs.PlateCarree())
        ax1.set_title(EXPT_TITLE.strip()+'\n'+ 'Gusts (kt)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
        ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
        ax1.add_feature(cfeature.COASTLINE.with_scale('50m'), zorder=10)
        ax1.add_feature(STATES, facecolor='none', edgecolor='black', zorder=10)
        ax1.add_feature(COUNTIES, facecolor='none', edgecolor='gray')
        #coast = cfeature.GSHHSFeature(scale='f')
        #ax1.add_feature(coast)
        gl = ax1.gridlines(crs=ccrs.PlateCarree(), linewidth=2, color='black', alpha=0.5, linestyle='--', draw_labels=True)
        gl.x_inline = False
        gl.y_inline = False
        gl.rotate_labels = True
        gl.xlabels_top = False
        gl.xlabels_bottom = True
        gl.ylabels_left = True
        gl.ylabels_right = False
        gl.xlines = True
        gl.ylines = True
        gl.xlocator = mticker.FixedLocator(lonplot)
        gl.ylocator = mticker.FixedLocator(latplot)
        gl.xformatter = LONGITUDE_FORMATTER
        gl.yformatter = LATITUDE_FORMATTER
        gl.xlabel_style = {'size': 12, 'color': 'black', 'weight': 'bold'}
        gl.ylabel_style = {'size': 12, 'color': 'black', 'weight': 'bold'}
        divider = make_axes_locatable(ax1)
        cax = divider.append_axes("right", size="5%", pad=1.0, axes_class=plt.Axes)
        cbar = plt.colorbar(ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140],cax=cax)
        cbar.ax.tick_params(labelsize=24)
        figfname = ODIR+'/'+LONGSID.lower()+'.gusts_6degreebox.'+forecastinit+'.airsea.f'+format(FHR,'03d')
        plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)

        #Make 6x6 plot of 10-m Wind With GF Overlaid
        lonplotmin = centerlon-3
        lonplotmax = centerlon+3
        latplotmin = centerlat-3
        latplotmax = centerlat+3
        lonplot = np.arange(int(round(lonplotmin,0))-1,int(round(lonplotmax,0))+1,1)
        latplot = np.arange(int(round(latplotmin,0))-1,int(round(latplotmax,0))+1,1)
        fig1 = plt.figure(figsize=(15.5,15.5))
        ax1 = fig1.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())
        ax1.set_extent([lonplotmin,lonplotmax,latplotmin,latplotmax], crs=ccrs.PlateCarree())
        plt.contourf(lon, lat, gf2_new, levs_gf, cmap='Reds', norm=norm_gf, extend='both', transform=ccrs.PlateCarree())
        cbar = plt.colorbar(ticks=[1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0],shrink=0.8)
        cbar.ax.tick_params(labelsize=24)
        # CS=plt.contour(lon, lat, ws1_new*1.94, [10,20,30,40,50,60,70,80,90,100,110,120,130,140,150], colors='xkcd:black',linewidths=4,linestyles='solid',zorder=11)
        # plt.clabel(CS, inline=True, fmt='%3i', fontsize=16)
        # Lew.Gramer@noaa.gov 2024-07-19 change suggested by role.aoml-hafs1@noaa.gov based on comment from Lev Looney
        #plt.barbs(lon2d[::10,::10],lat2d[::10,::10],u10[::10,::10]*1.94,v10[::10,::10]*1.94)
        plt.barbs(lon[::10],lat[::10],u10[::10,::10]*1.94,v10[::10,::10]*1.94)
        ax1.set_title(EXPT_TITLE.strip()+'\n'+ '10-m Wind (kt) and Gust Factor'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=small_fontsize, weight = 'bold',loc='left') #fontsize=24
        ax1.set_title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=fontsize,color='brown',loc='right') #fontsize=24
        ax1.add_feature(cfeature.COASTLINE.with_scale('50m'), zorder=10)
        ax1.add_feature(STATES, facecolor='none', edgecolor='gray', zorder=10)
        ax1.add_feature(COUNTIES, facecolor='none', edgecolor='gray')
        #coast = cfeature.GSHHSFeature(scale='f')
        #ax1.add_feature(coast)
        gl = ax1.gridlines(crs=ccrs.PlateCarree(), linewidth=2, color='black', alpha=0.5, linestyle='--', draw_labels=True)
        gl.x_inline = False
        gl.y_inline = False
        gl.rotate_labels = True
        gl.xlabels_top = False
        gl.xlabels_bottom = True
        gl.ylabels_left = True
        gl.ylabels_right = False
        gl.xlines = True
        gl.ylines = True
        gl.xlocator = mticker.FixedLocator(lonplot)
        gl.ylocator = mticker.FixedLocator(latplot)
        gl.xformatter = LONGITUDE_FORMATTER
        gl.yformatter = LATITUDE_FORMATTER
        gl.xlabel_style = {'size': 12, 'color': 'black', 'weight': 'bold'}
        gl.ylabel_style = {'size': 12, 'color': 'black', 'weight': 'bold'}
        figfname = ODIR+'/'+LONGSID.lower()+'.wind10m_and_gf_6degreebox.'+forecastinit+'.airsea.f'+format(FHR,'03d')
        plot_utils.save_figure(fig1, figfname, do_trim=False, do_gif=DO_CONVERTGIF)


    # Write the input file to a log to mark that it has been processed
    plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
  
  print('MSG: COMPLETING')
  os.system('lockfile -r-1 -l 180 '+ST_LOCK_FILE)
  os.system('echo "complete" > '+STATUS_FILE)
  os.system('rm -f '+ST_LOCK_FILE)




##############################
if __name__ == '__main__':
  main()
