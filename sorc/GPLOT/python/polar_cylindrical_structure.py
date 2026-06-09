#!/usr/bin/env python

# Check that GPLOT_DIR is defined in the environment.
import os
GPLOT_DIR = os.environ['GPLOT_DIR']
print('MSG: Found this GPLOT location --> '+GPLOT_DIR)

#Import necessary modules
print('MSG: Importing Everything Needed')
import argparse
import datetime, glob, re, subprocess, sys, time, warnings
import math, cmath
import numpy as np          # Used for a lot of the calculations
import metpy
from metpy import interpolate
import matplotlib           # The plotting routines
matplotlib.use('Agg')
import matplotlib.pyplot as plt        # Command for the plotting
import matplotlib.colors as colors      # Command to do some colorbar stuff

# matplotlib >= 3.10 hardens Artist.remove() to raise NotImplementedError
# when self.figure is None. That trips Colorbar._do_extends() in figures
# where two contourf collections with extend='both' share one colorbar
# (e.g. our along-/across-shear hovmoller pairs at +r and -r): the
# extend-cap patch from the first contourf is already detached when the
# colorbar redraws on the second contourf's `changed` callback, and
# remove() blows up. The semantically-correct response in that case is a
# no-op (the artist is already removed), so install a tolerant wrapper.
import matplotlib.artist as _mpl_artist
_GPLOT_ORIG_ARTIST_REMOVE = _mpl_artist.Artist.remove
def _gplot_tolerant_artist_remove(self):
  try:
    _GPLOT_ORIG_ARTIST_REMOVE(self)
  except NotImplementedError:
    # Already removed (figure attr is None); silent no-op preserves
    # the matplotlib < 3.10 behaviour the figure code was written for.
    pass
_mpl_artist.Artist.remove = _gplot_tolerant_artist_remove
#from matplotlib.ticker import ScalarFormatter    # Used to change the log-y-axis ticks
from mpl_toolkits.axes_grid1 import make_axes_locatable
import scipy            # Used for interpolation to polar coordinates
from scipy import interpolate        # The interpolation function
from scipy import ndimage as ndi     # Vectorized neighborhood filters (F-2)
from scipy.interpolate import griddata
#import concurrent.futures
from functools import partial
#import sys
#sys.path.append(GPLOT_DIR+'/sorc/GPLOT/python/')
import modules
import modules.skewTmodelTCpolar as skewTmodelTCpolar
import modules.shearandrhplot as shearandrhplot
import modules.plotting as plotting
# F-4: modules.interp + modules.multiprocess merged into gplot_utils.polar_interp.
from gplot_utils import polar_interp
import modules.tdr_tc_centering_with_example as tdrcenter
# Session E: replaces the legacy centroid.cpython-*.so Fortran extension. The
# Fischer (2023) / 2025 optimized weighted-circulation center finder lives in
# modules/tc_center_finding_speed_up.py and handles per-level vort-center
# location with optional numba JIT acceleration.
from modules.tc_center_finding_speed_up import recenter_tc as fischer_recenter_tc
import concurrent.futures
# scipy.interpolate.interp2d was removed in SciPy 1.14. The few legacy call
# sites here are replaced inline with RegularGridInterpolator.
import netCDF4
from netCDF4 import Dataset

# GPLOT utility package (Sessions 1-7 infrastructure; Session E port).
from gplot_utils import namelist as nml_utils
from gplot_utils import atcf as atcf_utils
from gplot_utils import ensemble as ens_utils
from gplot_utils import plot_utils
from gplot_utils import grib_reader
from gplot_utils import constants as gplot_const


def _parse_args():
    """Parse command-line arguments for the polar_cylindrical_structure script.

    Replaces the legacy sys.argv[1:11] positional contract with argparse named
    flags. Values default to 'MISSING' to mirror the historical batch-script
    placeholder; callers are expected to translate those to empty strings.
    """
    p = argparse.ArgumentParser(
        description='GPLOT polar cylindrical structure module')
    p.add_argument('--idate',       default='MISSING',
                   help='Forecast initialization date (YYYYMMDDHH)')
    p.add_argument('--sid',         default='MISSING',
                   help='Short storm ID (e.g., 13L)')
    p.add_argument('--domain',      default='MISSING',
                   help='Output domain identifier (e.g., d03)')
    p.add_argument('--tier',        default='MISSING',
                   help='Plot tier (e.g., Tier1)')
    p.add_argument('--ensid',       default='XX',
                   help='Ensemble ID tag (default XX)')
    p.add_argument('--force',       default='MISSING',
                   help='Force regeneration flag')
    p.add_argument('--resolution',  default='2.0',
                   help='Polar radial resolution (km)')
    p.add_argument('--rmax',        default='600.0',
                   help='Maximum polar radius (km)')
    p.add_argument('--levs',        default='45',
                   help='Number of pressure levels')
    p.add_argument('--master-nml',  default='namelist.master.default',
                   help='Path or filename of the master GPLOT namelist')
    return p.parse_args()


def MP_centers_function(u,v,lon,lat,centerlon,centerlat,level):
  print(np.shape(u))
  centers = tdrcenter.recenter_tc(u,v,lon,lat,1,10,150,centerlon,centerlat)
  return centers,level


def _read_grib_fields(file_path, dsource, bounds, do_dbz, zsize_pressure):
  """Read all GRIB2 fields for one FHR and compute derived 3D fields.

  Session F-1.1 extraction (pure cut-and-paste from the per-FHR block in
  main() that used to span the grib_reader.open_grib2() call through the
  `wwind = -omega/(rho*9.81)` derivation). No numerical changes — returns
  every variable consumed downstream, preserving names, shapes, and units.

  Returns a dict with the following keys (shape in parens; all SI unless
  noted, axis order ``(ny, nx, nz)`` for 3D):

    3D pressure-level:
      ``uwind`` (m/s), ``vwind`` (m/s), ``omega`` (Pa/s), ``hgt`` (m),
      ``temp`` (K), ``dbz`` (dBZ or NaN if ``do_dbz=False``), ``q`` (kg/kg),
      ``rh`` (%)
    Derived 3D:
      ``mixr``, ``temp_v``, ``rho`` (kg/m^3), ``wwind`` (m/s),
      ``wwind_store`` (alias of ``wwind``, preserved for legacy reference)
    2D surface / near-surface:
      ``sst`` (degC), ``pblz_upp`` (m), ``lhtflx`` (W/m^2),
      ``shtflx`` (W/m^2), ``u10`` (m/s), ``v10`` (m/s), ``mslp`` (Pa),
      ``tmp2m`` (K), ``q2m`` (kg/kg), ``rh2m`` (%),
      ``mixr2m``, ``temp_v_2m``, ``rho2m`` (kg/m^3)
    Shear-layer 2D:
      ``u850``/``v850`` (m/s), ``z850`` (m), ``u200``/``v200`` (m/s),
      ``z200`` (m)
    Coordinates + shape:
      ``lat``, ``lon`` (1D), ``lon_full``/``lat_full`` (2D meshgrid),
      ``lev1d`` (hPa, 1D), ``z`` (alias of ``lev1d`` as float64),
      ``levs`` (hPa, broadcast to 3D), ``ny``, ``nx``, ``nz_eff``
  Returns ``None`` if ``grib_reader.open_grib2()`` fails — caller should
  ``continue`` to skip this FHR. Raises ``RuntimeError`` if a required
  variable is missing from the file (matches legacy behaviour).
  """
  # Open the GRIB2 file via xarray+cfgrib (replaces g2ctl.pl + gribmap +
  # py3grads.Grads('open')). grib_reader.open_grib2 returns a list of
  # datasets that collectively cover all variables in the file.
  print('MSG: Opening GRIB2 file via xarray+cfgrib.')
  start = time.perf_counter()
  try:
    datasets = grib_reader.open_grib2(file_path)
  except Exception as exc:
    print(f'ERROR: Could not open GRIB2 file {file_path}: {exc}')
    return None

  # Helpers: return legacy-shape (ny, nx, nz) 3D and (ny, nx) 2D arrays.
  # grib_reader returns 3D data as (lev, lat, lon); transpose (1,2,0) to
  # recover the axis order the downstream code expects.
  def _fetch3d(var):
    r = grib_reader.get_var_3d(datasets, dsource, var, 1.0, 1100.0,
                               bounds=bounds)
    if r is None:
      raise RuntimeError(f'3D variable {var} missing from {file_path}')
    data = np.transpose(np.asarray(r['data']), (1, 2, 0))
    return (data, np.asarray(r['lat']), np.asarray(r['lon']),
            np.asarray(r['lev']))

  def _fetch2d(var, level=''):
    r = grib_reader.get_var_2d(datasets, dsource, var, level=level,
                               bounds=bounds)
    if r is None:
      raise RuntimeError(
          f'2D variable {var} (level={level}) missing from {file_path}')
    return np.asarray(r['data']).squeeze()

  # --- 3D pressure-level fields ---------------------------------------------
  # grib_reader auto-converts U/V from m/s to knots and HGT from m to dam;
  # reverse those conversions so downstream physics code (vorticity,
  # advection, density) stays in SI units identical to the GrADS-era path.
  print('MSG: Getting data now via xarray+cfgrib.')
  uwind_kt, lat, lon, lev1d = _fetch3d('U')
  vwind_kt, _, _, _         = _fetch3d('V')
  uwind = uwind_kt * gplot_const.kts2ms
  vwind = vwind_kt * gplot_const.kts2ms

  omega, _, _, _ = _fetch3d('OMEGA')
  print('MSG: Done reading: u,v,w (omega)')

  hgt_dam, _, _, _ = _fetch3d('HGT')
  hgt = hgt_dam * 10.0  # dam -> m
  temp, _, _, _ = _fetch3d('T')

  if do_dbz:
    dbz, _, _, _ = _fetch3d('REFL')
    print('MSG: Done reading: dbz, hgt, temp')
  else:
    # 3D reflectivity not available in the file (e.g. some HAFS analysis
    # times only carry 2D refc). Fill with NaN so dbz-derived metrics
    # (Steiner classifier, dbz-wavenumber, symmetry) gracefully degrade
    # to NaN in the CSV instead of aborting the entire metric pass.
    dbz = np.ones_like(hgt) * np.nan
    print('MSG: Done reading: hgt, temp')
    print('WARNING: Skipped reading dbz because DO_DBZ=False')

  q,  _, _, _ = _fetch3d('Q')
  rh, _, _, _ = _fetch3d('RH')
  print('MSG: Done reading: q, rh')

  # Trim to the first zsize_pressure levels (matches legacy `set z 1 nz`
  # which took the lowest-level zsize_pressure pressure surfaces in GrADS
  # order).
  if lev1d.size > zsize_pressure:
    sl = slice(0, zsize_pressure)
    uwind = uwind[:,:,sl]; vwind = vwind[:,:,sl]; omega = omega[:,:,sl]
    hgt   = hgt[:,:,sl];   temp  = temp[:,:,sl];  dbz   = dbz[:,:,sl]
    q     = q[:,:,sl];     rh    = rh[:,:,sl]
    lev1d = lev1d[sl]
  z = np.asarray(lev1d, dtype=float)

  # Build a 3D array of pressure (hPa) broadcast over (ny, nx, nz) so that
  # `levs*1e2` (converted to Pa) plugs into the rho / pressureT formulas
  # below with identical semantics to the legacy GrADS lev array.
  ny, nx, nz_eff = uwind.shape
  levs = np.broadcast_to(lev1d[None, None, :], (ny, nx, nz_eff)).astype(
      np.float64, copy=True)

  # 2D surface fields. SST comes from grib_reader as K; convert to degC.
  sst_k    = _fetch2d('SST')
  sst      = sst_k - 273.14  # K -> oC
  pblz_upp = _fetch2d('HPBL')
  lhtflx   = _fetch2d('LHFLX')
  shtflx   = _fetch2d('SHFLX')
  print('MSG: Done with sst, pblz_upp, lhtflx, shtflx')

  # 10-m winds and 2-m T/q/RH.
  u10 = _fetch2d('U', level='10') * gplot_const.kts2ms
  v10 = _fetch2d('V', level='10') * gplot_const.kts2ms

  if dsource == 'HAFS':
    mslp = _fetch2d('MSLP') * 100.0  # hPa -> Pa (legacy used Pa)
  else:
    mslp = _fetch2d('PRMSL') * 100.0 \
           if (grib_reader.get_var_2d(datasets, dsource, 'PRMSL') is not None) \
           else _fetch2d('MSLP') * 100.0

  tmp2m = _fetch2d('T', level='2')
  q2m   = _fetch2d('Q', level='2')
  rh2m  = _fetch2d('RH', level='2')
  print('MSG: Done reading u10,v10,mslp,tmp2m,q2m')

  # Shear-layer winds (850 and 200 hPa single slices).
  u850 = _fetch2d('U',   level='850') * gplot_const.kts2ms
  v850 = _fetch2d('V',   level='850') * gplot_const.kts2ms
  z850 = _fetch2d('HGT', level='850') * 10.0  # dam -> m
  u200 = _fetch2d('U',   level='200') * gplot_const.kts2ms
  v200 = _fetch2d('V',   level='200') * gplot_const.kts2ms
  z200 = _fetch2d('HGT', level='200') * 10.0  # dam -> m

  # Build 2D lat/lon meshgrids (legacy lat_full / lon_full) used by
  # tilt/haversine calculations and by recenter_tc.
  lon_full, lat_full = np.meshgrid(lon, lat)

  # Compute additional 2D data
  mixr2m    = q2m/(1-q2m)
  temp_v_2m = tmp2m*(1+0.61*mixr2m)
  rho2m     = mslp/(287*temp_v_2m)

  finish = time.perf_counter()
  print(f'MSG: Total time to read data: {finish-start:.2f} second(s)')

  # Get W from Omega:  w = -omega/(rho*g),  rho = p/(Rd*Tv)
  mixr   = q/(1-q)
  temp_v = temp*(1+0.61*mixr)
  rho    = (levs*1e2)/(287*temp_v)
  wwind  = -omega/(rho*9.81)
  # 12/23 edit retained: wwind_store preserves the initial wwind for any
  # later comparison against the storm-relative/re-interpolated version.
  wwind_store = wwind

  # Normalize lat to ascending order. HWRF storm-nest grb2s store lat
  # top-to-bottom (descending: ~+18 -> ~-2 for a tropical WP storm)
  # while HAFS storm-nest grb2s store lat ascending. Every downstream
  # consumer of this dict assumes ascending lat: x_sr/y_sr are derived
  # with `y_sr = (lat - centerlat) * km`, so descending lat produces
  # descending y_sr, which then breaks the (a) RegularGridInterpolator
  # construction at line ~479 (requires strictly increasing axes) and
  # (b) the `argmin(|y_sr+-200|)` bounding-box derivation in the TDR
  # recentering loop (where ymin ends up > ymax, slicing produces an
  # empty array, and find_nearest's nanmin trips
  # "zero-size array to reduction operation fmin which has no identity").
  # Flip lat + every lat-indexed array once, here, so the entire
  # polar pipeline downstream sees a single canonical convention.
  if len(lat) >= 2 and lat[0] > lat[-1]:
    lat = lat[::-1].copy()
    # 3D arrays: shape (ny, nx, nz) -> flip axis 0 (lat).
    for _arr in (uwind, vwind, omega, hgt, temp, dbz, q, rh,
                 mixr, temp_v, rho, wwind, wwind_store, levs):
      _arr[:] = _arr[::-1, :, :]
    # 2D arrays: shape (ny, nx) -> flip axis 0 (lat).
    for _arr in (sst, pblz_upp, lhtflx, shtflx, u10, v10, mslp,
                 tmp2m, q2m, rh2m, mixr2m, temp_v_2m, rho2m,
                 u850, v850, z850, u200, v200, z200):
      _arr[:] = _arr[::-1, :]
    # 2D meshgrid built from the (now ascending) lat -- rebuild.
    lon_full, lat_full = np.meshgrid(lon, lat)
    print('MSG: Normalized descending lat -> ascending '
          f'({float(lat[0]):.3f} -> {float(lat[-1]):.3f})')

  return {
      'uwind': uwind, 'vwind': vwind, 'omega': omega, 'hgt': hgt,
      'temp': temp, 'dbz': dbz, 'q': q, 'rh': rh,
      'mixr': mixr, 'temp_v': temp_v, 'rho': rho,
      'wwind': wwind, 'wwind_store': wwind_store,
      'sst': sst, 'pblz_upp': pblz_upp, 'lhtflx': lhtflx, 'shtflx': shtflx,
      'u10': u10, 'v10': v10, 'mslp': mslp,
      'tmp2m': tmp2m, 'q2m': q2m, 'rh2m': rh2m,
      'mixr2m': mixr2m, 'temp_v_2m': temp_v_2m, 'rho2m': rho2m,
      'u850': u850, 'v850': v850, 'z850': z850,
      'u200': u200, 'v200': v200, 'z200': z200,
      'lat': lat, 'lon': lon, 'lon_full': lon_full, 'lat_full': lat_full,
      'lev1d': lev1d, 'z': z, 'levs': levs,
      'ny': ny, 'nx': nx, 'nz_eff': nz_eff,
  }


def _interp_to_height(uwind, vwind, wwind, dbz, hgt, temp, q, rh, rho, levs,
                      u10, v10, mslp, tmp2m, q2m, rh2m, rho2m):
  """Interpolate pressure-level 3D fields onto uniform height grids.

  Session F-1.2 extraction (pure cut-and-paste from the `# Interpolate to
  Height Coordinates` block in main()). No numerical changes.

  Two passes:
    - Full column onto ``heightlevs = np.linspace(0, 18000, 37)`` (nz=37)
    - PBL column onto ``heightlevs_pbl = np.linspace(0, 3000, 31)`` (nz=31)

  Both passes delegate the actual interpolation to
  ``gplot_utils.polar_interp.height_interp_vars_fast`` — a fully vectorized
  per-variable interpolator that replaced the legacy 4×8 ThreadPoolExecutor
  + 296 metpy per-level calls (numerically equivalent to within float64
  epsilon on realistic HAFS coverage). Surface
  level (k=0) is overridden with the 2-m / MSLP / 10-m fields so the
  lowest height bin reflects observed near-surface state rather than a
  downward extrapolation from the lowest pressure surface.

  Returns a dict with keys:
    ``uwind``, ``vwind``, ``wwind``, ``dbz``, ``temp``, ``q``, ``rh``,
    ``pressure``                       -- height-gridded (ny, nx, 37), m/s etc.
    ``uwind_pbl``, ``vwind_pbl``,
    ``rho_pbl``, ``pressure_pbl``      -- PBL-gridded (ny, nx, 31)
    ``heightlevs``, ``zsize``          -- 1D (37,), int 37
    ``heightlevs_pbl``, ``zsize_pbl``  -- 1D (31,), int 31
  """
  print('MSG: Doing Height Coordinate Interpolation Now')
  start = time.perf_counter()
  uwindT = np.transpose(uwind, (2, 0, 1))
  vwindT = np.transpose(vwind, (2, 0, 1))
  wwindT = np.transpose(wwind, (2, 0, 1))
  dbzT   = np.transpose(dbz,   (2, 0, 1))
  hgtT   = np.transpose(hgt,   (2, 0, 1))
  tempT  = np.transpose(temp,  (2, 0, 1))
  qT     = np.transpose(q,     (2, 0, 1))
  rhT    = np.transpose(rh,    (2, 0, 1))
  rhoT   = np.transpose(rho,   (2, 0, 1))
  pressureT = np.transpose(levs*1e2, (2, 0, 1))
  heightlevs = np.linspace(0, 18000, 37)
  zsize = np.shape(heightlevs)[0]  # Change zsize here

  varInList = [uwindT, vwindT, wwindT, dbzT, tempT, qT, rhT, pressureT]
  # Vectorized replacement for the legacy 4×8 ThreadPoolExecutor + 296
  # metpy per-level calls. Numerically equivalent to within float64 epsilon
  # (~1e-13) on realistic HAFS coverage; NaN handling matches metpy
  # exactly when the pressure-level column brackets the target heights,
  # which is always true for HAFS (1000–10 hPa spans ≫ [0, 18000] m).
  HeightData = polar_interp.height_interp_vars_fast(
      hgt=hgtT, varList=varInList, levels=heightlevs)
  uwind_h, vwind_h, wwind_h = HeightData[0, :, :, :], HeightData[1, :, :, :], HeightData[2, :, :, :]
  dbz_h,   temp_h,  q_h     = HeightData[3, :, :, :], HeightData[4, :, :, :], HeightData[5, :, :, :]
  rh_h,    pressure_h       = HeightData[6, :, :, :], HeightData[7, :, :, :]

  # Surface override at k=0 (legacy: replace extrapolated level-0 with
  # observed 2-m / 10-m / MSLP fields; wwind gets NaN since we don't have
  # an equivalent 10-m vertical velocity).
  uwind_h[:, :, 0], vwind_h[:, :, 0], wwind_h[:, :, 0] = u10, v10, np.nan
  dbz_h[:, :, 0],   temp_h[:, :, 0],  pressure_h[:, :, 0] = np.nan, tmp2m, mslp
  q_h[:, :, 0],     rh_h[:, :, 0] = q2m, rh2m

  # PBL column: re-interpolate a subset onto a finer 0-3 km grid. Variables
  # are the same transposed arrays (uwindT, vwindT, rhoT, pressureT), so
  # we reuse them directly.
  heightlevs_pbl = np.linspace(0, 3000, 31)
  zsize_pbl = np.shape(heightlevs_pbl)[0]
  varList_pbl = [uwindT, vwindT, rhoT, pressureT]
  HeightData = polar_interp.height_interp_vars_fast(
      hgt=hgtT, varList=varList_pbl, levels=heightlevs_pbl)
  uwind_pbl, vwind_pbl = HeightData[0, :, :, :], HeightData[1, :, :, :]
  rho_pbl,   pressure_pbl = HeightData[2, :, :, :], HeightData[3, :, :, :]

  uwind_pbl[:, :, 0], vwind_pbl[:, :, 0] = u10, v10
  pressure_pbl[:, :, 0], rho_pbl[:, :, 0] = mslp, rho2m
  finish = time.perf_counter()
  print(f'MSG: Total time for height interpolation: {finish-start:.2f} second(s)')

  return {
      'uwind': uwind_h, 'vwind': vwind_h, 'wwind': wwind_h,
      'dbz':   dbz_h,   'temp':  temp_h,  'q':     q_h,
      'rh':    rh_h,    'pressure': pressure_h,
      'uwind_pbl': uwind_pbl, 'vwind_pbl': vwind_pbl,
      'rho_pbl':   rho_pbl,   'pressure_pbl': pressure_pbl,
      'heightlevs': heightlevs, 'zsize': zsize,
      'heightlevs_pbl': heightlevs_pbl, 'zsize_pbl': zsize_pbl,
  }


def _interp_to_polar(uwind, vwind, wwind, dbz, temp, q, rh, pressure,
                     uwind_pbl, vwind_pbl, rho_pbl, pressure_pbl,
                     u10, v10, u200, v200, u850, v850,
                     x_sr, y_sr, XI, YI, theta, heightlevs, heightlevs_pbl,
                     zsize, zsize_pbl, centerlat):
  """Interpolate Cartesian storm-relative fields onto polar (r, theta, z) grid.

  Session F-1.3 extraction. Pure cut-and-paste from the `DO POLAR
  INTERPOLATION` block in main(); no numerical changes.

  Three passes:
    1. Full-column 3D fields onto the (theta, r, z) polar grid via
       ``gplot_utils.polar_interp.multiprocess_polar_vars`` (the legacy
       per-level ThreadPoolExecutor path; F-4 consolidated it here from
       ``modules/multiprocess.py``).
    2. PBL column onto the finer 31-level PBL height grid via per-level
       ``scipy.interpolate.RegularGridInterpolator`` calls.
    3. Single-level 2D fields (10m, 850/200 hPa) onto (theta, r) polar
       grid via per-field RegularGridInterpolator calls.

  Tangential (vt) and radial (ur) wind are derived after each pass by
  rotating (u, v) into polar components. Sign(centerlat) handles the
  southern hemisphere reversal.

  ``pblz_vt_max`` is the height of peak tangential wind in the PBL column
  — a boundary-layer depth proxy added by Lew Gramer, 2024-01-19.

  Returns a dict with keys (all shape (ntheta, nr, nz) for 3D; (ntheta, nr)
  for 2D):
    3D polar:
      ``u_p, v_p, w_p, dbz_p, temp_p, q_p, rh_p, pressure_p``
    3D polar (derived):
      ``vt_p, ur_p``
    PBL 3D polar:
      ``u_pbl_p, v_pbl_p, vt_pbl_p, ur_pbl_p``
    PBL diagnostic:
      ``pblz_vt_max`` (2D: ntheta, nr)
    2D polar:
      ``u10_p, v10_p, u200_p, v200_p, u850_p, v850_p``
      ``vt10_p, ur10_p, vt200_p, ur200_p, vt850_p, ur850_p``
  """
  print('MSG: Doing the Polar Interpolation Now')

  # Full-column 3D polar interpolation (legacy per-level loop collapsed into
  # a single multiprocess_polar_vars call).
  start = time.perf_counter()
  varList = [np.transpose(uwind,   (2, 0, 1)),
             np.transpose(vwind,   (2, 0, 1)),
             np.transpose(wwind,   (2, 0, 1)),
             np.transpose(dbz,     (2, 0, 1)),
             np.transpose(temp,    (2, 0, 1)),
             np.transpose(q,       (2, 0, 1)),
             np.transpose(rh,      (2, 0, 1)),
             np.transpose(pressure,(2, 0, 1))]
  PolarData = polar_interp.multiprocess_polar_vars(
      x_sr, y_sr, XI, YI, varList=varList, levels=heightlevs)
  u_p, v_p, w_p       = PolarData[0, :, :, :], PolarData[1, :, :, :], PolarData[2, :, :, :]
  dbz_p, temp_p, q_p  = PolarData[3, :, :, :], PolarData[4, :, :, :], PolarData[5, :, :, :]
  rh_p, pressure_p    = PolarData[6, :, :, :], PolarData[7, :, :, :]
  finish = time.perf_counter()
  print(f'MSG: Total time for polar interpolation: {finish-start:.2f} second(s)')

  # NaN-coverage gauge for the d03-moving-nest-edge-encroachment case.
  # Computed on u_p (the primary 3D field) and returned to the caller
  # so main()'s FHR loop can soft-WARN at >1% and hard-STOP at >25%.
  # NaN here means the polar grid point fell outside the GRIB's
  # storm-relative extent; see polar_interp.interp_to_polarcylindrical().
  nan_frac = float(np.mean(~np.isfinite(u_p)))

  # Calculate tangential (vt) and radial (ur) wind.
  # F-3 vectorization: theta depends only on the azimuth axis (axis=0); broadcast
  # it to (ntheta, 1, 1) so the rotation runs as one whole-array multiply.
  theta_3d = theta[:, None, None]
  vt_p = np.sign(centerlat) * (-u_p * np.sin(theta_3d) + v_p * np.cos(theta_3d))
  ur_p =                       u_p * np.cos(theta_3d) + v_p * np.sin(theta_3d)

  # PBL column: per-level RegularGridInterpolator on finer 0-3 km grid.
  # Includes rho_pbl + pressure_pbl alongside u/v -- needed for the
  # gradient-wind imbalance (term_b) diagnostic, which combines a
  # radial pressure gradient (-1/rho * dp/dr) with the centripetal
  # and Coriolis terms from the PBL tangential wind.
  u_pbl_p   = np.ones((np.shape(XI)[0], np.shape(XI)[1], zsize_pbl))*np.nan
  v_pbl_p   = np.ones((np.shape(XI)[0], np.shape(XI)[1], zsize_pbl))*np.nan
  rho_pbl_p      = np.ones((np.shape(XI)[0], np.shape(XI)[1], zsize_pbl))*np.nan
  pressure_pbl_p = np.ones((np.shape(XI)[0], np.shape(XI)[1], zsize_pbl))*np.nan

  for k in range(zsize_pbl):
    # bounds_error=False + NaN fill: polar grid points outside the
    # storm-relative (y_sr, x_sr) extent (d03 moving nest behind the
    # storm) return NaN instead of crashing the FHR. See
    # polar_interp.interp_to_polarcylindrical() for the rationale.
    f_uwind_pbl = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), uwind_pbl[:,:,k], bounds_error=False, fill_value=np.nan)
    f_vwind_pbl = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), vwind_pbl[:,:,k], bounds_error=False, fill_value=np.nan)
    f_rho_pbl   = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), rho_pbl[:,:,k],   bounds_error=False, fill_value=np.nan)
    f_p_pbl     = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), pressure_pbl[:,:,k], bounds_error=False, fill_value=np.nan)
    u_pbl_p[:,:,k]        = f_uwind_pbl((YI, XI), method='linear')
    v_pbl_p[:,:,k]        = f_vwind_pbl((YI, XI), method='linear')
    rho_pbl_p[:,:,k]      = f_rho_pbl((YI, XI),   method='linear')
    pressure_pbl_p[:,:,k] = f_p_pbl((YI, XI),     method='linear')

  # F-3 vectorization: broadcast theta across (nr, nz_pbl) in one multiply.
  vt_pbl_p = np.sign(centerlat) * (-u_pbl_p * np.sin(theta_3d) + v_pbl_p * np.cos(theta_3d))
  ur_pbl_p =                       u_pbl_p * np.cos(theta_3d) + v_pbl_p * np.sin(theta_3d)

  # Lew.Gramer@noaa.gov 2024-01-19: height of peak tangential wind in PBL.
  pblz_vt_max = heightlevs_pbl[vt_pbl_p.argmax(axis=2)]

  # 2D polar interpolation for 10-m / 850 / 200 hPa winds.
  u10_p  = np.ones((np.shape(XI)[0], np.shape(XI)[1]))*np.nan
  v10_p  = np.ones((np.shape(XI)[0], np.shape(XI)[1]))*np.nan
  # bounds_error=False + NaN fill for the 2-D single-level fields, same
  # rationale as the PBL loop above and polar_interp module.
  f_u10 = interpolate.RegularGridInterpolator((y_sr, x_sr), u10[:,:], bounds_error=False, fill_value=np.nan)
  f_v10 = interpolate.RegularGridInterpolator((y_sr, x_sr), v10[:,:], bounds_error=False, fill_value=np.nan)
  u10_p[:,:] = f_u10((YI, XI), method='linear')
  v10_p[:,:] = f_v10((YI, XI), method='linear')

  # (Pre-allocation of u/v/vt/ur at 200/850 removed — outputs are assigned
  #  directly below from interpolators and broadcast rotations.)

  f_u200 = interpolate.RegularGridInterpolator((y_sr, x_sr), u200[:,:], bounds_error=False, fill_value=np.nan)
  f_v200 = interpolate.RegularGridInterpolator((y_sr, x_sr), v200[:,:], bounds_error=False, fill_value=np.nan)
  f_u850 = interpolate.RegularGridInterpolator((y_sr, x_sr), u850[:,:], bounds_error=False, fill_value=np.nan)
  f_v850 = interpolate.RegularGridInterpolator((y_sr, x_sr), v850[:,:], bounds_error=False, fill_value=np.nan)

  u200_p = f_u200((YI, XI), method='linear')
  v200_p = f_v200((YI, XI), method='linear')
  u850_p = f_u850((YI, XI), method='linear')
  v850_p = f_v850((YI, XI), method='linear')

  # F-3 vectorization: 2D (ntheta, nr) rotation via (ntheta, 1) broadcast.
  theta_2d = theta[:, None]
  sin_t_2d = np.sin(theta_2d); cos_t_2d = np.cos(theta_2d)
  sgn = np.sign(centerlat)
  vt10_p  = sgn * (-u10_p  * sin_t_2d + v10_p  * cos_t_2d)
  ur10_p  =        u10_p  * cos_t_2d + v10_p  * sin_t_2d
  vt850_p = sgn * (-u850_p * sin_t_2d + v850_p * cos_t_2d)
  ur850_p =        u850_p * cos_t_2d + v850_p * sin_t_2d
  vt200_p = sgn * (-u200_p * sin_t_2d + v200_p * cos_t_2d)
  ur200_p =        u200_p * cos_t_2d + v200_p * sin_t_2d

  return {
      'u_p': u_p, 'v_p': v_p, 'w_p': w_p,
      'dbz_p': dbz_p, 'temp_p': temp_p, 'q_p': q_p, 'rh_p': rh_p,
      'pressure_p': pressure_p,
      'vt_p': vt_p, 'ur_p': ur_p,
      'u_pbl_p': u_pbl_p, 'v_pbl_p': v_pbl_p,
      'vt_pbl_p': vt_pbl_p, 'ur_pbl_p': ur_pbl_p,
      'rho_pbl_p': rho_pbl_p, 'pressure_pbl_p': pressure_pbl_p,
      'pblz_vt_max': pblz_vt_max,
      'u10_p': u10_p, 'v10_p': v10_p,
      'u200_p': u200_p, 'v200_p': v200_p,
      'u850_p': u850_p, 'v850_p': v850_p,
      'vt10_p':  vt10_p,  'ur10_p':  ur10_p,
      'vt850_p': vt850_p, 'ur850_p': ur850_p,
      'vt200_p': vt200_p, 'ur200_p': ur200_p,
      'nan_frac': nan_frac,
  }


##############################
def _compute_shear(u200_p, v200_p, u850_p, v850_p,
                   vt_p, ur_p, vt850_p, ur850_p, vt200_p, ur200_p,
                   u_p, v_p, w_p, dbz_p, temp_p, q_p, rh_p, pressure_p,
                   u10_p, v10_p, vt10_p, ur10_p,
                   XI, theta, zsize, resolution, rmax, pi):
  """Compute 200-850 hPa environmental shear and build shear-rotated fields.

  Returns dict of shear diagnostics + filtered (vortex-removed) fields +
  shear-rotated ``*_p_rot`` fields, OR ``None`` if ``shearmag`` is NaN
  (caller should log + skip file).
  """
  # 200 km - rmax ring average
  u850_p_ring = u850_p[:, int(np.round(200/resolution)):int(np.round(rmax/resolution))]
  v850_p_ring = v850_p[:, int(np.round(200/resolution)):int(np.round(rmax/resolution))]
  u200_p_ring = u200_p[:, int(np.round(200/resolution)):int(np.round(rmax/resolution))]
  v200_p_ring = v200_p[:, int(np.round(200/resolution)):int(np.round(rmax/resolution))]

  with warnings.catch_warnings():
    warnings.filterwarnings(action='ignore', message='Mean of empty slice')
    u850_p_ring_mean = np.nanmean(np.nanmean(u850_p_ring))
    v850_p_ring_mean = np.nanmean(np.nanmean(v850_p_ring))
    u200_p_ring_mean = np.nanmean(np.nanmean(u200_p_ring))
    v200_p_ring_mean = np.nanmean(np.nanmean(v200_p_ring))

  ushear1 = u200_p_ring_mean - u850_p_ring_mean
  vshear1 = v200_p_ring_mean - v850_p_ring_mean

  shearmag = np.hypot(ushear1, vshear1)
  sheardir = np.arctan2(vshear1, ushear1) * 180.0 / pi
  if np.isnan(shearmag):
    return None
  shearstring = str(int(np.round(shearmag*1.94, 0)))

  # Meteorological convention
  if sheardir <= 90:
    sheardir_met = 90 - sheardir
  else:
    sheardir_met = 360 - (sheardir - 90)

  # Positive value
  if sheardir < 0:
    sheardir_math = sheardir + 360
  else:
    sheardir_math = sheardir

  sheardir_5deg = (np.round((sheardir_math/5))*5)
  sheardir_index = int(sheardir_5deg/5)

  # Filtered (vortex-removed) fields — F-3 vectorization: compute azimuthal
  # means once per field and let broadcasting subtract them across all radii.
  with warnings.catch_warnings():
    warnings.filterwarnings(action='ignore', message='Mean of empty slice')
    vt850_p_filtered = vt850_p - np.nanmean(vt850_p, axis=0, keepdims=True)
    ur850_p_filtered = ur850_p - np.nanmean(ur850_p, axis=0, keepdims=True)
    vt200_p_filtered = vt200_p - np.nanmean(vt200_p, axis=0, keepdims=True)
    ur200_p_filtered = ur200_p - np.nanmean(ur200_p, axis=0, keepdims=True)

    # 3D (ntheta, nr, nz) filter: axis=0 mean broadcasts back across ntheta.
    vt_p_filtered = vt_p - np.nanmean(vt_p, axis=0, keepdims=True)
    ur_p_filtered = ur_p - np.nanmean(ur_p, axis=0, keepdims=True)

  # Rotate filtered (vt, ur) → (u, v). F-3: theta[:,None,None] broadcasts over
  # (nr, nz) so the per-(j,k) loop collapses to a single multiply.
  theta_3d_f = theta[:, None, None]
  sin_t3 = np.sin(theta_3d_f); cos_t3 = np.cos(theta_3d_f)
  u_p_filtered = ur_p_filtered * cos_t3 - vt_p_filtered * sin_t3
  v_p_filtered = ur_p_filtered * sin_t3 + vt_p_filtered * cos_t3

  # 2D rotation (ntheta, nr) — theta[:,None] broadcast.
  theta_2d_f = theta[:, None]
  sin_t2 = np.sin(theta_2d_f); cos_t2 = np.cos(theta_2d_f)
  u850_p_filtered = ur850_p_filtered * cos_t2 - vt850_p_filtered * sin_t2
  v850_p_filtered = ur850_p_filtered * sin_t2 + vt850_p_filtered * cos_t2
  u200_p_filtered = ur200_p_filtered * cos_t2 - vt200_p_filtered * sin_t2
  v200_p_filtered = ur200_p_filtered * sin_t2 + vt200_p_filtered * cos_t2

  ushear_p = u200_p_filtered - u850_p_filtered
  vshear_p = v200_p_filtered - v850_p_filtered

  with warnings.catch_warnings():
    warnings.filterwarnings(action='ignore', message='Mean of empty slice')
    ushear2 = np.nanmean(np.nanmean(ushear_p))
    vshear2 = np.nanmean(np.nanmean(vshear_p))

  # Rotate variables based on shear (3D)
  u_p_rot        = np.roll(u_p,        [-sheardir_index, 0, 0], axis=(0, 1, 2))
  v_p_rot        = np.roll(v_p,        [-sheardir_index, 0, 0], axis=(0, 1, 2))
  w_p_rot        = np.roll(w_p,        [-sheardir_index, 0, 0], axis=(0, 1, 2))
  dbz_p_rot      = np.roll(dbz_p,      [-sheardir_index, 0, 0], axis=(0, 1, 2))
  temp_p_rot     = np.roll(temp_p,     [-sheardir_index, 0, 0], axis=(0, 1, 2))
  q_p_rot        = np.roll(q_p,        [-sheardir_index, 0, 0], axis=(0, 1, 2))
  rh_p_rot       = np.roll(rh_p,       [-sheardir_index, 0, 0], axis=(0, 1, 2))
  vt_p_rot       = np.roll(vt_p,       [-sheardir_index, 0, 0], axis=(0, 1, 2))
  ur_p_rot       = np.roll(ur_p,       [-sheardir_index, 0, 0], axis=(0, 1, 2))
  pressure_p_rot = np.roll(ur_p,       [-sheardir_index, 0, 0], axis=(0, 1, 2))

  # 2D
  u10_p_rot   = np.roll(u10_p,   [-sheardir_index, 0], axis=(0, 1))
  v10_p_rot   = np.roll(v10_p,   [-sheardir_index, 0], axis=(0, 1))
  vt10_p_rot  = np.roll(vt10_p,  [-sheardir_index, 0], axis=(0, 1))
  ur10_p_rot  = np.roll(ur10_p,  [-sheardir_index, 0], axis=(0, 1))
  u200_p_rot  = np.roll(u200_p,  [-sheardir_index, 0], axis=(0, 1))
  v200_p_rot  = np.roll(v200_p,  [-sheardir_index, 0], axis=(0, 1))
  u850_p_rot  = np.roll(u850_p,  [-sheardir_index, 0], axis=(0, 1))
  v850_p_rot  = np.roll(v850_p,  [-sheardir_index, 0], axis=(0, 1))

  return {
    'shearmag': shearmag, 'sheardir': sheardir,
    'sheardir_met': sheardir_met, 'sheardir_math': sheardir_math,
    'sheardir_5deg': sheardir_5deg, 'sheardir_index': sheardir_index,
    'shearstring': shearstring,
    'ushear1': ushear1, 'vshear1': vshear1,
    'ushear2': ushear2, 'vshear2': vshear2,
    'ushear_p': ushear_p, 'vshear_p': vshear_p,
    'vt_p_filtered': vt_p_filtered, 'ur_p_filtered': ur_p_filtered,
    'u_p_filtered': u_p_filtered,   'v_p_filtered': v_p_filtered,
    'u850_p_filtered': u850_p_filtered, 'v850_p_filtered': v850_p_filtered,
    'u200_p_filtered': u200_p_filtered, 'v200_p_filtered': v200_p_filtered,
    'vt850_p_filtered': vt850_p_filtered, 'ur850_p_filtered': ur850_p_filtered,
    'vt200_p_filtered': vt200_p_filtered, 'ur200_p_filtered': ur200_p_filtered,
    'u_p_rot': u_p_rot, 'v_p_rot': v_p_rot, 'w_p_rot': w_p_rot,
    'dbz_p_rot': dbz_p_rot, 'temp_p_rot': temp_p_rot,
    'q_p_rot': q_p_rot, 'rh_p_rot': rh_p_rot,
    'vt_p_rot': vt_p_rot, 'ur_p_rot': ur_p_rot,
    'pressure_p_rot': pressure_p_rot,
    'u10_p_rot': u10_p_rot, 'v10_p_rot': v10_p_rot,
    'vt10_p_rot': vt10_p_rot, 'ur10_p_rot': ur10_p_rot,
    'u200_p_rot': u200_p_rot, 'v200_p_rot': v200_p_rot,
    'u850_p_rot': u850_p_rot, 'v850_p_rot': v850_p_rot,
  }


##############################
def _azimuthal_means(vt_p, ur_p, w_p, dbz_p, temp_p, q_p, rh_p, pressure_p,
                     vt_pbl_p, ur_pbl_p, rho_pbl_p, pressure_pbl_p,
                     ur_p_rot, w_p_rot, dbz_p_rot, rh_p_rot):
  """Compute total-azimuthal means + shear-relative quadrant means.

  Shear-relative quadrants (on the rotated grid, 72 azimuths, 5° spacing):
    downshear  = [1:9, 63:72] (concat)
    upshear    = [27:45]
    leftshear  = [9:27]
    rightshear = [45:63]
  """
  with warnings.catch_warnings():
    warnings.filterwarnings(action='ignore', message='Mean of empty slice')
    vt_p_mean       = np.nanmean(vt_p, 0)
    ur_p_mean       = np.nanmean(ur_p, 0)
    w_p_mean        = np.NaN if np.isnan(w_p).all() else np.nanmean(w_p, 0)
    dbz_p_mean      = np.nanmean(dbz_p, 0)
    temp_p_mean     = np.nanmean(temp_p, 0)
    q_p_mean        = np.nanmean(q_p, 0)
    rh_p_mean       = np.nanmean(rh_p, 0)
    pressure_p_mean = np.nanmean(pressure_p, 0)
    vt_pbl_p_mean        = np.nanmean(vt_pbl_p, 0)
    ur_pbl_p_mean        = np.nanmean(ur_pbl_p, 0)
    rho_pbl_p_mean       = np.nanmean(rho_pbl_p, 0)
    pressure_pbl_p_mean  = np.nanmean(pressure_pbl_p, 0)

  # Shear-relative quadrant slabs
  ur_p_downshear  = np.concatenate((ur_p_rot[1:9, :, :],  ur_p_rot[63:72, :, :]),  axis=0)
  w_p_downshear   = np.concatenate((w_p_rot[1:9, :, :],   w_p_rot[63:72, :, :]),   axis=0)
  dbz_p_downshear = np.concatenate((dbz_p_rot[1:9, :, :], dbz_p_rot[63:72, :, :]), axis=0)
  rh_p_downshear  = np.concatenate((rh_p_rot[1:9, :, :],  rh_p_rot[63:72, :, :]),  axis=0)

  ur_p_upshear    = ur_p_rot[27:45, :, :]
  w_p_upshear     = w_p_rot[27:45, :, :]
  dbz_p_upshear   = dbz_p_rot[27:45, :, :]
  rh_p_upshear    = rh_p_rot[27:45, :, :]

  ur_p_leftshear  = ur_p_rot[9:27, :, :]
  w_p_leftshear   = w_p_rot[9:27, :, :]
  dbz_p_leftshear = dbz_p_rot[9:27, :, :]
  rh_p_leftshear  = rh_p_rot[9:27, :, :]

  ur_p_rightshear  = ur_p_rot[45:63, :, :]
  w_p_rightshear   = w_p_rot[45:63, :, :]
  dbz_p_rightshear = dbz_p_rot[45:63, :, :]
  rh_p_rightshear  = rh_p_rot[45:63, :, :]

  with warnings.catch_warnings():
    warnings.filterwarnings(action='ignore', message='Mean of empty slice')
    ur_p_downshear_mean   = np.nanmean(ur_p_downshear, 0)
    w_p_downshear_mean    = np.nanmean(w_p_downshear, 0)
    dbz_p_downshear_mean  = np.nanmean(dbz_p_downshear, 0)
    rh_p_downshear_mean   = np.nanmean(rh_p_downshear, 0)

    ur_p_upshear_mean     = np.nanmean(ur_p_upshear, 0)
    w_p_upshear_mean      = np.nanmean(w_p_upshear, 0)
    dbz_p_upshear_mean    = np.nanmean(dbz_p_upshear, 0)
    rh_p_upshear_mean     = np.nanmean(rh_p_upshear, 0)

    ur_p_leftshear_mean   = np.nanmean(ur_p_leftshear, 0)
    w_p_leftshear_mean    = np.nanmean(w_p_leftshear, 0)
    dbz_p_leftshear_mean  = np.nanmean(dbz_p_leftshear, 0)
    rh_p_leftshear_mean   = np.nanmean(rh_p_leftshear, 0)

    ur_p_rightshear_mean  = np.nanmean(ur_p_rightshear, 0)
    w_p_rightshear_mean   = np.nanmean(w_p_rightshear, 0)
    dbz_p_rightshear_mean = np.nanmean(dbz_p_rightshear, 0)
    rh_p_rightshear_mean  = np.nanmean(rh_p_rightshear, 0)

  return {
    'vt_p_mean': vt_p_mean, 'ur_p_mean': ur_p_mean, 'w_p_mean': w_p_mean,
    'dbz_p_mean': dbz_p_mean, 'temp_p_mean': temp_p_mean,
    'q_p_mean': q_p_mean, 'rh_p_mean': rh_p_mean, 'pressure_p_mean': pressure_p_mean,
    'vt_pbl_p_mean': vt_pbl_p_mean, 'ur_pbl_p_mean': ur_pbl_p_mean,
    'rho_pbl_p_mean': rho_pbl_p_mean,
    'pressure_pbl_p_mean': pressure_pbl_p_mean,
    'ur_p_downshear': ur_p_downshear, 'w_p_downshear': w_p_downshear,
    'dbz_p_downshear': dbz_p_downshear, 'rh_p_downshear': rh_p_downshear,
    'ur_p_upshear': ur_p_upshear, 'w_p_upshear': w_p_upshear,
    'dbz_p_upshear': dbz_p_upshear, 'rh_p_upshear': rh_p_upshear,
    'ur_p_leftshear': ur_p_leftshear, 'w_p_leftshear': w_p_leftshear,
    'dbz_p_leftshear': dbz_p_leftshear, 'rh_p_leftshear': rh_p_leftshear,
    'ur_p_rightshear': ur_p_rightshear, 'w_p_rightshear': w_p_rightshear,
    'dbz_p_rightshear': dbz_p_rightshear, 'rh_p_rightshear': rh_p_rightshear,
    'ur_p_downshear_mean': ur_p_downshear_mean, 'w_p_downshear_mean': w_p_downshear_mean,
    'dbz_p_downshear_mean': dbz_p_downshear_mean, 'rh_p_downshear_mean': rh_p_downshear_mean,
    'ur_p_upshear_mean': ur_p_upshear_mean, 'w_p_upshear_mean': w_p_upshear_mean,
    'dbz_p_upshear_mean': dbz_p_upshear_mean, 'rh_p_upshear_mean': rh_p_upshear_mean,
    'ur_p_leftshear_mean': ur_p_leftshear_mean, 'w_p_leftshear_mean': w_p_leftshear_mean,
    'dbz_p_leftshear_mean': dbz_p_leftshear_mean, 'rh_p_leftshear_mean': rh_p_leftshear_mean,
    'ur_p_rightshear_mean': ur_p_rightshear_mean, 'w_p_rightshear_mean': w_p_rightshear_mean,
    'dbz_p_rightshear_mean': dbz_p_rightshear_mean, 'rh_p_rightshear_mean': rh_p_rightshear_mean,
  }


##############################
def _wavenumber_decomp(dbz_p, rh_p, w_p, vt10_p, ur10_p, vort_p, XI, r, theta):
  """Azimuthal Fourier decomposition (wavenumbers 0, 1, 2, >2) for:
    * dbz5_p   (reflectivity at level index 10, ~5 km)
    * rh5_p    (RH at level index 10, ~5 km)
    * w5_p     (vertical velocity at level index 10, ~5 km)
    * vt10_p   (10-m tangential wind)
    * ur10_p   (10-m radial wind)
    * vort2_p  (relative vorticity at level index 4, ~2 km)

  Returns dict of per-field w0/w1/w2/whigher arrays shaped (ntheta, nr).
  """
  dbz5_p = dbz_p[:, :, 10]
  rh5_p  = rh_p[:, :, 10]
  w5_p   = w_p[:, :, 10]
  # 2 km altitude: heightlevs = np.linspace(0, 18000, 37) -> step = 500 m,
  # so 2000 m sits at index 4. Reuse the same Fourier loop below.
  vort2_p = vort_p[:, :, 4]

  dbz5_p_w0      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  dbz5_p_w1      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  dbz5_p_w2      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  dbz5_p_whigher = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan

  rh5_p_w0      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  rh5_p_w1      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  rh5_p_w2      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  rh5_p_whigher = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan

  w5_p_w0      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  w5_p_w1      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  w5_p_w2      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  w5_p_whigher = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan

  vt10_p_w0      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  vt10_p_w1      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  vt10_p_w2      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  vt10_p_whigher = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  ur10_p_w0      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  ur10_p_w1      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  ur10_p_w2      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan

  vort2_p_w0      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  vort2_p_w1      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  vort2_p_w2      = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  vort2_p_whigher = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan

  for j in range(np.shape(r)[0]):
    dbzdata = dbz5_p[:, j]
    fourier_dbz = np.fft.fft(dbzdata) / len(dbzdata)
    amp0_dbz = np.real(fourier_dbz[0])
    A1_dbz = 2*np.real(fourier_dbz[1]); B1_dbz = -2*np.imag(fourier_dbz[1])
    A2_dbz = 2*np.real(fourier_dbz[2]); B2_dbz = -2*np.imag(fourier_dbz[2])
    dbz5_p_w0[:, j] = amp0_dbz
    dbz5_p_w1[:, j] = A1_dbz*np.cos(theta)   + B1_dbz*np.sin(theta)
    dbz5_p_w2[:, j] = A2_dbz*np.cos(2*theta) + B2_dbz*np.sin(2*theta)
    dbz5_p_whigher[:, j] = 0
    for h in range(2, int((np.shape(theta)[0]+1)/2)):
      A = 2*np.real(fourier_dbz[h]); B = -2*np.imag(fourier_dbz[h])
      dbz5_p_whigher[:, j] = dbz5_p_whigher[:, j] + A*np.cos(h*theta) + B*np.sin(h*theta)

    rhdata = rh5_p[:, j]
    fourier_rh = np.fft.fft(rhdata) / len(rhdata)
    amp0_rh = np.real(fourier_rh[0])
    A1_rh = 2*np.real(fourier_rh[1]); B1_rh = -2*np.imag(fourier_rh[1])
    A2_rh = 2*np.real(fourier_rh[2]); B2_rh = -2*np.imag(fourier_rh[2])
    rh5_p_w0[:, j] = amp0_rh
    rh5_p_w1[:, j] = A1_rh*np.cos(theta)   + B1_rh*np.sin(theta)
    rh5_p_w2[:, j] = A2_rh*np.cos(2*theta) + B2_rh*np.sin(2*theta)
    rh5_p_whigher[:, j] = 0
    for h in range(2, int((np.shape(theta)[0]+1)/2)):
      A = 2*np.real(fourier_rh[h]); B = -2*np.imag(fourier_rh[h])
      rh5_p_whigher[:, j] = rh5_p_whigher[:, j] + A*np.cos(h*theta) + B*np.sin(h*theta)

    wdata = w5_p[:, j]
    fourier_w = np.fft.fft(wdata) / len(wdata)
    amp0_w = np.real(fourier_w[0])
    A1_w = 2*np.real(fourier_w[1]); B1_w = -2*np.imag(fourier_w[1])
    A2_w = 2*np.real(fourier_w[2]); B2_w = -2*np.imag(fourier_w[2])
    w5_p_w0[:, j] = amp0_w
    w5_p_w1[:, j] = A1_w*np.cos(theta)   + B1_w*np.sin(theta)
    w5_p_w2[:, j] = A2_w*np.cos(2*theta) + B2_w*np.sin(2*theta)
    w5_p_whigher[:, j] = 0
    for h in range(2, int((np.shape(theta)[0]+1)/2)):
      A = 2*np.real(fourier_w[h]); B = -2*np.imag(fourier_w[h])
      w5_p_whigher[:, j] = w5_p_whigher[:, j] + A*np.cos(h*theta) + B*np.sin(h*theta)

    vt10data = vt10_p[:, j]
    fourier_vt10 = np.fft.fft(vt10data) / len(vt10data)
    amp0_vt10 = np.real(fourier_vt10[0])
    A1_vt10 = 2*np.real(fourier_vt10[1]); B1_vt10 = -2*np.imag(fourier_vt10[1])
    A2_vt10 = 2*np.real(fourier_vt10[2]); B2_vt10 = -2*np.imag(fourier_vt10[2])
    vt10_p_w0[:, j] = amp0_vt10
    vt10_p_w1[:, j] = A1_vt10*np.cos(theta)   + B1_vt10*np.sin(theta)
    vt10_p_w2[:, j] = A2_vt10*np.cos(2*theta) + B2_vt10*np.sin(2*theta)
    vt10_p_whigher[:, j] = 0
    for h in range(2, int((np.shape(theta)[0]+1)/2)):
      A = 2*np.real(fourier_vt10[h]); B = -2*np.imag(fourier_vt10[h])
      vt10_p_whigher[:, j] = vt10_p_whigher[:, j] + A*np.cos(h*theta) + B*np.sin(h*theta)

    ur10data = ur10_p[:, j]
    fourier_ur10 = np.fft.fft(ur10data) / len(ur10data)
    amp0_ur10 = np.real(fourier_ur10[0])
    A1_ur10 = 2*np.real(fourier_ur10[1]); B1_ur10 = -2*np.imag(fourier_ur10[1])
    A2_ur10 = 2*np.real(fourier_ur10[2]); B2_ur10 = -2*np.imag(fourier_ur10[2])
    ur10_p_w0[:, j] = amp0_ur10
    ur10_p_w1[:, j] = A1_ur10*np.cos(theta)   + B1_ur10*np.sin(theta)
    ur10_p_w2[:, j] = A2_ur10*np.cos(2*theta) + B2_ur10*np.sin(2*theta)

    vortdata = vort2_p[:, j]
    fourier_vort = np.fft.fft(vortdata) / len(vortdata)
    amp0_vort = np.real(fourier_vort[0])
    A1_vort = 2*np.real(fourier_vort[1]); B1_vort = -2*np.imag(fourier_vort[1])
    A2_vort = 2*np.real(fourier_vort[2]); B2_vort = -2*np.imag(fourier_vort[2])
    vort2_p_w0[:, j] = amp0_vort
    vort2_p_w1[:, j] = A1_vort*np.cos(theta)   + B1_vort*np.sin(theta)
    vort2_p_w2[:, j] = A2_vort*np.cos(2*theta) + B2_vort*np.sin(2*theta)
    vort2_p_whigher[:, j] = 0
    for h in range(2, int((np.shape(theta)[0]+1)/2)):
      A = 2*np.real(fourier_vort[h]); B = -2*np.imag(fourier_vort[h])
      vort2_p_whigher[:, j] = vort2_p_whigher[:, j] + A*np.cos(h*theta) + B*np.sin(h*theta)

  return {
    'dbz5_p': dbz5_p, 'rh5_p': rh5_p, 'w5_p': w5_p,
    'dbz5_p_w0': dbz5_p_w0, 'dbz5_p_w1': dbz5_p_w1,
    'dbz5_p_w2': dbz5_p_w2, 'dbz5_p_whigher': dbz5_p_whigher,
    'rh5_p_w0': rh5_p_w0, 'rh5_p_w1': rh5_p_w1, 'rh5_p_w2': rh5_p_w2,
    'rh5_p_whigher': rh5_p_whigher,
    'w5_p_w0': w5_p_w0, 'w5_p_w1': w5_p_w1, 'w5_p_w2': w5_p_w2,
    'w5_p_whigher': w5_p_whigher,
    'vt10_p_w0': vt10_p_w0, 'vt10_p_w1': vt10_p_w1,
    'vt10_p_w2': vt10_p_w2, 'vt10_p_whigher': vt10_p_whigher,
    'ur10_p_w0': ur10_p_w0, 'ur10_p_w1': ur10_p_w1, 'ur10_p_w2': ur10_p_w2,
    'vort2_p': vort2_p,
    'vort2_p_w0': vort2_p_w0, 'vort2_p_w1': vort2_p_w1,
    'vort2_p_w2': vort2_p_w2, 'vort2_p_whigher': vort2_p_whigher,
  }


##############################
def _find_rmw_vmax(vt_p_mean, vt_pbl_p_mean, ur_p_mean, r, theta,
                   rnorm, Rnorm, THETAnorm, XInorm, YInorm,
                   zsize, zsize_pbl, rmax):
  """Compute per-level RMW and Vmax; identify 2-km RMW / vmax; renormalize.

  Returns ``None`` if ``rmw_2km`` or ``vt_p_mean_max_2km`` is NaN (caller
  should log + skip). Otherwise returns a dict with RMW/vmax diagnostics,
  possibly-resized ``rnorm``-family arrays, ``rmw_pbl_mean``, and
  ``vt_p_mean_norm`` / ``ur_p_mean_norm``.
  """
  rmw_mean       = np.ones(zsize) * np.nan
  rmw_mean_index = np.ones(zsize) * np.nan
  vt_rmw_mean    = np.ones(zsize) * np.nan

  for k in range(zsize):
    rmw_mean[k] = np.round(np.median(r[vt_p_mean[:, k] > 0.95*np.nanmax(vt_p_mean[:, k])]))
    rmw_mean_index[k] = np.argmin(abs(r - rmw_mean[k]))
    vt_rmw_mean[k] = vt_p_mean[int(rmw_mean_index[k]), k]

  rmw_2km = rmw_mean[4]
  vt_p_mean_max = np.max(vt_p_mean, 0)
  vt_p_mean_max_2km = vt_p_mean_max[4]
  if rmw_2km < 2:
    rmw_2km = np.nan

  if np.isnan(rmw_2km):
    return None
  rmwstring = str(int(np.round(rmw_2km*0.54, 0)))
  if np.isnan(vt_p_mean_max_2km):
    return {'_skip_vmax_nan': True}
  vmaxstring = str(int(np.round(vt_p_mean_max_2km*1.94, 0)))

  # Quick fix for big RMWs: shrink rnorm if needed
  if rmax/rmw_2km < np.max(rnorm):
    rnorm_max = np.round(rmax/rmw_2km, 2) - math.fmod(np.round(rmax/rmw_2km, 2), 0.05)
    rnorm = np.linspace(0, rnorm_max, (int(rnorm_max/0.05)) + 1)
    Rnorm, THETAnorm = np.meshgrid(rnorm, theta)
    XInorm = Rnorm * np.cos(THETAnorm)
    YInorm = Rnorm * np.sin(THETAnorm)

  rmw_pbl_mean = np.ones(zsize_pbl) * np.nan
  for k in range(zsize_pbl):
    rmw_pbl_mean[k] = np.round(np.median(r[vt_pbl_p_mean[:, k] > 0.95*np.max(vt_pbl_p_mean[:, k])]))

  # Normalize vt/ur by RMW (only if rmw_2km < 200 km)
  vt_p_mean_norm = np.ones((np.shape(rnorm)[0], zsize)) * np.nan
  ur_p_mean_norm = np.ones((np.shape(rnorm)[0], zsize)) * np.nan
  if rmw_2km < 200:
    for k in range(zsize):
      f_vt = scipy.interpolate.interp1d((r/rmw_2km), vt_p_mean[:, k], kind='linear')
      f_ur = scipy.interpolate.interp1d((r/rmw_2km), ur_p_mean[:, k], kind='linear')
      vt_p_mean_norm[:, k] = f_vt(rnorm)
      ur_p_mean_norm[:, k] = f_ur(rnorm)

  return {
    'rmw_mean': rmw_mean, 'rmw_mean_index': rmw_mean_index, 'vt_rmw_mean': vt_rmw_mean,
    'rmw_2km': rmw_2km, 'vt_p_mean_max': vt_p_mean_max, 'vt_p_mean_max_2km': vt_p_mean_max_2km,
    'rmwstring': rmwstring, 'vmaxstring': vmaxstring,
    'rnorm': rnorm, 'Rnorm': Rnorm, 'THETAnorm': THETAnorm,
    'XInorm': XInorm, 'YInorm': YInorm,
    'rmw_pbl_mean': rmw_pbl_mean,
    'vt_p_mean_norm': vt_p_mean_norm, 'ur_p_mean_norm': ur_p_mean_norm,
  }


##############################
def _compute_warm_core(temp_p_mean, r, heightlevs):
  """Compute warm-core anomaly magnitude, height, and per-level radial extent.

  Contraction:
    core  = ring r=[0, 15 km]   mean temp
    outer = ring r=[200, 300 km] mean temp
    anomaly = core_mean - outer_mean   (per-level)
    extent  = largest contiguous radius where (temp - outer_mean) > 1 K
  """
  r15km_index  = np.argmin(np.abs(r - 15))
  r200km_index = np.argmin(np.abs(r - 200))
  r300km_index = np.argmin(np.abs(r - 300))

  temp_p_mean_core       = temp_p_mean[0:r15km_index+1, :]
  temp_p_mean_outer      = temp_p_mean[r200km_index:r300km_index+1, :]
  temp_p_mean_core_mean  = np.nanmean(temp_p_mean_core, 0)
  temp_p_mean_outer_mean = np.nanmean(temp_p_mean_outer, 0)
  temp_p_anomaly         = temp_p_mean[0:r200km_index] - temp_p_mean_outer_mean

  # Per-level largest-contiguous radius with anomaly > 1 K
  anomaly_extent_ix = (temp_p_anomaly.shape[0] -
                       np.argmin(temp_p_anomaly[::-1, :] <= 1.0, axis=0)) - 1
  anomaly_extent_ix[anomaly_extent_ix < 0] = 0
  anomaly_extent = r[anomaly_extent_ix]
  temp_p_anomaly_max = np.max(temp_p_anomaly, axis=0)

  temp_anomaly            = temp_p_mean_core_mean - temp_p_mean_outer_mean
  temp_anomaly_max        = np.max(temp_anomaly[1::])
  height_temp_anomaly_max = heightlevs[np.argmax(temp_anomaly[1::]) + 1] / 1000

  return {
    'r15km_index': r15km_index, 'r200km_index': r200km_index, 'r300km_index': r300km_index,
    'temp_p_mean_core_mean': temp_p_mean_core_mean,
    'temp_p_mean_outer_mean': temp_p_mean_outer_mean,
    'temp_p_anomaly': temp_p_anomaly,
    'anomaly_extent': anomaly_extent,
    'temp_p_anomaly_max': temp_p_anomaly_max,
    'temp_anomaly': temp_anomaly,
    'temp_anomaly_max': temp_anomaly_max,
    'height_temp_anomaly_max': height_temp_anomaly_max,
  }


##############################
def _compute_tilt(pressure, vort, pressure_p_mean, vort_p_mean,
                  rmw_mean, x_sr, y_sr, r,
                  uwind, vwind, lon, lat, lon_full, lat_full,
                  centerlon, centerlat, ivd, zsize, vortex_depth_vort):
  """Compute storm-center cascade (pressure + vort) and 2-5 / 2-10 km tilt.

  - Pressure centers: legacy masked-argmin (bit-exact replacement for centroid.so sign=-1)
  - Vort centers:     Fischer (2023) recenter_tc weighted-circulation finder
  - Returns dict with indices, x/y positions, lon/lat, tiltmag/tiltdir for
    mid (2-5 km) and deep (2-10 km), plus NaNs when vortex_depth_vort < 5/10.
  """
  pressure_centroid = np.copy(pressure[:, :, 0:ivd])
  vort_centroid     = np.copy(vort[:, :, 0:ivd])

  center_indices_pressure = np.zeros((np.shape(pressure_centroid)[2], 2), order='F').astype(np.int32)
  center_indices_vort     = np.zeros((np.shape(vort_centroid)[2], 2), order='F').astype(np.int32)
  threshold_pressure      = np.zeros((np.shape(pressure_centroid)[2]))
  threshold_vort          = np.zeros((np.shape(vort_centroid)[2]))

  for k in range(ivd):
    x1 = np.argmin(abs(-rmw_mean[k] - x_sr))
    x2 = np.argmin(abs( rmw_mean[k] - x_sr))
    y1 = np.argmin(abs(-rmw_mean[k] - y_sr))
    y2 = np.argmin(abs( rmw_mean[k] - y_sr))
    r2 = np.argmin(abs(r - rmw_mean[k]))

    pressure_centroid[0:y1, :, k] = 9999999999
    pressure_centroid[y2::, :, k] = 9999999999
    pressure_centroid[:, 0:x1, k] = 9999999999
    pressure_centroid[:, x2::, k] = 9999999999

    vort_centroid[0:y1, :, k] = -9999999999
    vort_centroid[y2::, :, k] = -9999999999
    vort_centroid[:, 0:x1, k] = -9999999999
    vort_centroid[:, x2::, k] = -99999999999

    threshold_pressure[k] = (np.nanmin(pressure_p_mean[0:r2+1, k]) +
                             0.2*(np.nanmax(pressure_p_mean[0:r2+1, k]) -
                                  np.nanmin(pressure_p_mean[0:r2+1, k])))
    threshold_vort[k]     = 0.80 * np.nanmax(vort_p_mean[0:r2+1, k])

  center_x_vort       = np.ones(zsize) * np.nan
  center_y_vort       = np.ones(zsize) * np.nan
  center_x_pressure   = np.ones(zsize) * np.nan
  center_y_pressure   = np.ones(zsize) * np.nan
  center_lon_pressure = np.ones(zsize) * np.nan
  center_lat_pressure = np.ones(zsize) * np.nan
  # Per-level lon/lat for the Fischer (2023) circulation center.
  # Filled directly from recenter_tc returns below; remain NaN aloft if
  # the cascade never runs (e.g. vortex_depth_vort < 5 km).
  center_lon_vort     = np.ones(zsize) * np.nan
  center_lat_vort     = np.ones(zsize) * np.nan

  # Default tilt metrics (filled below if vortex_depth_vort deep enough)
  tiltmag_deep_pressure = np.nan
  tiltdir_deep_pressure = np.nan
  tiltmag_deep_vort     = np.nan
  tiltdir_deep_vort     = np.nan
  tiltmag_mid_pressure  = np.nan
  tiltdir_mid_pressure  = np.nan
  tiltmag_mid_vort      = np.nan
  tiltdir_mid_vort      = np.nan

  if np.min(threshold_vort) > 0:
    # (A) Pressure centers: bit-exact masked-argmin
    ny_pc, nx_pc, nz_pc = pressure_centroid.shape
    for k in range(nz_pc):
      layer = pressure_centroid[:, :, k]
      mask = layer <= threshold_pressure[k]
      if not mask.any():
        continue
      candidates = np.where(mask, layer, np.inf)
      flat_idx = np.argmin(candidates)
      yy, xx = divmod(flat_idx, nx_pc)
      center_indices_pressure[k, 0] = yy
      center_indices_pressure[k, 1] = xx

    # (B) Vort centers: Fischer (2023) recenter_tc with cascade
    # ``vort_spad`` is the per-iteration grid-point search radius around
    # each center guess. With the cascade (each level inherits the prior
    # level's converged center), spad=4 (9x9=81 candidates) is plenty —
    # vortex tilt across one vertical level is rarely > 4 grid points
    # (~8 km on a 2-km HAFS grid). Lowering from 8→4 cuts inner-loop
    # work ~3.6x. ``vort_num_iter`` is just the safety cap; the algo
    # exits early on convergence (typically 2–3 iterations).
    vort_num_sectors = 8
    vort_spad        = 4
    vort_num_iter    = 20
    guess_lon, guess_lat = float(centerlon), float(centerlat)
    _rec_start = time.perf_counter()
    for k in range(nz_pc):
      u2d = uwind[:, :, k]
      v2d = vwind[:, :, k]
      if not np.isfinite(u2d).any() or not np.isfinite(v2d).any():
        continue
      try:
        tc_lon, tc_lat, _vt_max, _rmw_km, _cov, _sv = fischer_recenter_tc(
          u2d, v2d, lon_full, lat_full,
          vort_num_sectors, vort_spad, vort_num_iter,
          guess_lon, guess_lat)
      except Exception as _exc:
        print(f'WARNING: recenter_tc failed at level k={k}: {_exc}')
        continue
      if tc_lon is None or tc_lat is None:
        continue
      if not (np.isfinite(tc_lon) and np.isfinite(tc_lat)):
        continue
      yy = int(np.argmin(np.abs(lat - tc_lat)))
      xx = int(np.argmin(np.abs(lon - tc_lon)))
      center_indices_vort[k, 0] = yy
      center_indices_vort[k, 1] = xx
      # Save the actual (unsnapped) Fischer center lon/lat for downstream
      # analysis — finer than the index-snapped value at line above.
      center_lon_vort[k] = float(tc_lon)
      center_lat_vort[k] = float(tc_lat)
      guess_lon, guess_lat = float(tc_lon), float(tc_lat)
    _rec_finish = time.perf_counter()
    print(f'MSG: recenter_tc vortex center cascade ({nz_pc} levels): {_rec_finish-_rec_start:.2f} s')

    center_x_vort[0:ivd]       = x_sr[center_indices_vort[:, 1]]
    center_y_vort[0:ivd]       = y_sr[center_indices_vort[:, 0]]
    center_x_pressure[0:ivd]   = x_sr[center_indices_pressure[:, 1]]
    center_y_pressure[0:ivd]   = y_sr[center_indices_pressure[:, 0]]
    center_lon_pressure[0:ivd] = lon[center_indices_pressure[:, 1]]
    center_lat_pressure[0:ivd] = lat[center_indices_pressure[:, 0]]

    # 2-10 km and 2-5 km tilt
    if vortex_depth_vort >= 10.:
      tiltmag_deep_pressure = np.hypot(center_x_pressure[20]-center_x_pressure[4],
                                       center_y_pressure[20]-center_y_pressure[4])
      tiltdir_deep_pressure = np.arctan2(center_y_pressure[20]-center_y_pressure[4],
                                         center_x_pressure[20]-center_x_pressure[4])*180/np.pi
      if tiltdir_deep_pressure <= 90:
        tiltdir_deep_pressure = 90 - tiltdir_deep_pressure
      else:
        tiltdir_deep_pressure = 360 - (tiltdir_deep_pressure - 90)

      tiltmag_deep_vort = np.hypot(center_x_vort[20]-center_x_vort[4],
                                   center_y_vort[20]-center_y_vort[4])
      tiltdir_deep_vort = np.arctan2(center_y_vort[20]-center_y_vort[4],
                                     center_x_vort[20]-center_x_vort[4])*180/np.pi
      if tiltdir_deep_vort <= 90:
        tiltdir_deep_vort = 90 - tiltdir_deep_vort
      else:
        tiltdir_deep_vort = 360 - (tiltdir_deep_vort - 90)

      tiltmag_mid_pressure = np.hypot(center_x_pressure[10]-center_x_pressure[4],
                                      center_y_pressure[10]-center_y_pressure[4])
      tiltdir_mid_pressure = np.arctan2(center_y_pressure[10]-center_y_pressure[4],
                                        center_x_pressure[10]-center_x_pressure[4])*180/np.pi
      if tiltdir_mid_pressure <= 90:
        tiltdir_mid_pressure = 90 - tiltdir_mid_pressure
      else:
        tiltdir_mid_pressure = 360 - (tiltdir_mid_pressure - 90)

      tiltmag_mid_vort = np.hypot(center_x_vort[10]-center_x_vort[4],
                                  center_y_vort[10]-center_y_vort[4])
      tiltdir_mid_vort = np.arctan2(center_y_vort[10]-center_y_vort[4],
                                    center_x_vort[10]-center_x_vort[4])*180/np.pi
      if tiltdir_mid_vort <= 90:
        tiltdir_mid_vort = 90 - tiltdir_mid_vort
      else:
        tiltdir_mid_vort = 360 - (tiltdir_mid_vort - 90)
    elif vortex_depth_vort >= 5. and vortex_depth_vort <= 10.:
      tiltmag_mid_pressure = np.hypot(center_x_pressure[10]-center_x_pressure[4],
                                      center_y_pressure[10]-center_y_pressure[4])
      tiltdir_mid_pressure = np.arctan2(center_y_pressure[10]-center_y_pressure[4],
                                        center_x_pressure[10]-center_x_pressure[4])*180/np.pi
      if tiltdir_mid_pressure <= 90:
        tiltdir_mid_pressure = 90 - tiltdir_mid_pressure
      else:
        tiltdir_mid_pressure = 360 - (tiltdir_mid_pressure - 90)

      tiltmag_mid_vort = np.hypot(center_x_vort[10]-center_x_vort[4],
                                  center_y_vort[10]-center_y_vort[4])
      tiltdir_mid_vort = np.arctan2(center_y_vort[10]-center_y_vort[4],
                                    center_x_vort[10]-center_x_vort[4])*180/np.pi
      if tiltdir_mid_vort <= 90:
        tiltdir_mid_vort = 90 - tiltdir_mid_vort
      else:
        tiltdir_mid_vort = 360 - (tiltdir_mid_vort - 90)

  return {
    'pressure_centroid': pressure_centroid, 'vort_centroid': vort_centroid,
    'center_indices_pressure': center_indices_pressure,
    'center_indices_vort': center_indices_vort,
    'threshold_pressure': threshold_pressure, 'threshold_vort': threshold_vort,
    'center_x_vort': center_x_vort, 'center_y_vort': center_y_vort,
    'center_x_pressure': center_x_pressure, 'center_y_pressure': center_y_pressure,
    'center_lon_pressure': center_lon_pressure, 'center_lat_pressure': center_lat_pressure,
    'center_lon_vort': center_lon_vort, 'center_lat_vort': center_lat_vort,
    'tiltmag_deep_pressure': tiltmag_deep_pressure, 'tiltdir_deep_pressure': tiltdir_deep_pressure,
    'tiltmag_deep_vort': tiltmag_deep_vort, 'tiltdir_deep_vort': tiltdir_deep_vort,
    'tiltmag_mid_pressure': tiltmag_mid_pressure, 'tiltdir_mid_pressure': tiltdir_mid_pressure,
    'tiltmag_mid_vort': tiltmag_mid_vort, 'tiltdir_mid_vort': tiltdir_mid_vort,
  }


##############################
def _compute_vort_tendency(uwind, vwind, vt_p, ur_p, w_p,
                           vt_p_mean, ur_p_mean, w_p_mean,
                           lat, lon, heightlevs, x_sr, y_sr,
                           centerlat, XI, YI, zsize):
  """Compute vorticity + vt-budget tendency terms (mean radial flux, mean
  vertical advection, eddy flux, vertical eddy advection) on the polar grid.
  """
  import metpy.calc as mpcalc
  from metpy.units import units

  vort = np.ones((np.shape(lat)[0], np.shape(lon)[0], np.shape(heightlevs)[0])) * np.nan
  with warnings.catch_warnings():
    warnings.filterwarnings(action='ignore', message='Mean of empty slice')
    xgrad = np.nanmean(np.gradient(x_sr))
    ygrad = np.nanmean(np.gradient(y_sr))

  for k in range(zsize):
    vort[:, :, k] = np.sign(centerlat) * np.array(mpcalc.vorticity(
      uwind[:, :, k]*units.meter/units.second,
      vwind[:, :, k]*units.meter/units.second,
      dx=xgrad*1e3*units.meter, dy=ygrad*1e3*units.meter))

  vort_p = np.ones((np.shape(XI)[0], np.shape(XI)[1], zsize)) * np.nan
  for k in range(zsize):
    f_vort = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), vort[:, :, k], bounds_error=False, fill_value=np.nan)
    vort_p[:, :, k] = f_vort((YI, XI), method='linear')

  f = 2 * 7.292e-5 * np.sin(centerlat*3.14159/180)
  absvort_p = vort_p + f

  with warnings.catch_warnings():
    warnings.filterwarnings(action='ignore', message='Mean of empty slice')
    vort_p_mean    = np.nanmean(vort_p, 0)
    absvort_p_mean = np.nanmean(absvort_p, 0)

  # F-3 vectorization: perturbation = field - mean broadcast over the
  # azimuth axis. vt_p_mean is (nr, nz); inserting an axis 0 broadcasts it
  # against vt_p's (ntheta, nr, nz) in a single pass.
  vt_p_perturbation   = vt_p   - vt_p_mean[None, :, :]
  ur_p_perturbation   = ur_p   - ur_p_mean[None, :, :]
  w_p_perturbation    = w_p    - w_p_mean[None, :, :]
  vort_p_perturbation = vort_p - vort_p_mean[None, :, :]

  # Term 1: Mean radial influx of absolute vorticity
  term1_vt_tendency_mean_radial_flux = -ur_p_mean * absvort_p_mean
  term1_vt_tendency_mean_radial_flux[:, 0] = np.nan
  term1_vt_tendency_mean_radial_flux[0, :] = np.nan

  # Term 2: Mean vertical advection of mean tangential momentum
  d_vt_p_mean_dz = np.array(metpy.calc.first_derivative(vt_p_mean, axis=1, delta=500))
  d_vt_p_mean_dz[:, 0] = np.nan
  d_vt_p_mean_dz[0, :] = np.nan
  term2_vt_tendency_mean_vertical_advection = -w_p_mean * d_vt_p_mean_dz

  # Term 3: Eddy flux
  eddy_vort_flux_p = ur_p_perturbation * vort_p_perturbation
  with warnings.catch_warnings():
    warnings.filterwarnings(action='ignore', message='Mean of empty slice')
    eddy_vort_flux_p_mean = np.nanmean(eddy_vort_flux_p, 0)
  term3_vt_tendency_eddy_flux = -eddy_vort_flux_p_mean
  term3_vt_tendency_eddy_flux[:, 0] = np.nan
  term3_vt_tendency_eddy_flux[0, :] = np.nan

  # Term 4: Vertical advection of eddy tangential momentum
  # np.array(...) strips metpy/pint units so downstream numpy ops
  # (nanmean, flipud, rot90) accept the array.
  d_vt_p_perturbation_dz = np.array(metpy.calc.first_derivative(vt_p_perturbation, axis=2, delta=500))
  vertical_eddy_advection_p = w_p_perturbation * d_vt_p_perturbation_dz
  with warnings.catch_warnings():
    warnings.filterwarnings(action='ignore', message='Mean of empty slice')
    vertical_eddy_advection_p_mean = np.nanmean(vertical_eddy_advection_p, 0)
  term4_vt_tendency_vertical_eddy_advection = -vertical_eddy_advection_p_mean
  term4_vt_tendency_vertical_eddy_advection[:, 0] = np.nan
  term4_vt_tendency_vertical_eddy_advection[0, :] = np.nan

  terms_vt_tendency_sum = (term1_vt_tendency_mean_radial_flux +
                           term2_vt_tendency_mean_vertical_advection +
                           term3_vt_tendency_eddy_flux +
                           term4_vt_tendency_vertical_eddy_advection)

  return {
    'vort': vort, 'vort_p': vort_p, 'absvort_p': absvort_p,
    'vort_p_mean': vort_p_mean, 'absvort_p_mean': absvort_p_mean,
    'vt_p_perturbation': vt_p_perturbation,
    'ur_p_perturbation': ur_p_perturbation,
    'w_p_perturbation': w_p_perturbation,
    'vort_p_perturbation': vort_p_perturbation,
    'f': f,
    'd_vt_p_mean_dz': d_vt_p_mean_dz,
    'eddy_vort_flux_p': eddy_vort_flux_p,
    'eddy_vort_flux_p_mean': eddy_vort_flux_p_mean,
    'd_vt_p_perturbation_dz': d_vt_p_perturbation_dz,
    'vertical_eddy_advection_p': vertical_eddy_advection_p,
    'vertical_eddy_advection_p_mean': vertical_eddy_advection_p_mean,
    'term1_vt_tendency_mean_radial_flux': term1_vt_tendency_mean_radial_flux,
    'term2_vt_tendency_mean_vertical_advection': term2_vt_tendency_mean_vertical_advection,
    'term3_vt_tendency_eddy_flux': term3_vt_tendency_eddy_flux,
    'term4_vt_tendency_vertical_eddy_advection': term4_vt_tendency_vertical_eddy_advection,
    'terms_vt_tendency_sum': terms_vt_tendency_sum,
  }


##############################
def _partition_precip(dbz, heightlevs, xgrad, ygrad, rmw_2km,
                      XI, YI, XInorm, YInorm, x_sr, y_sr, sheardir_index):
  """Steiner et al. (1995) precipitation classifier + polar interpolation.

  Returns dict with:
    hlevs        : heights in km
    sref         : 4D reflectivity cube shaped (1, ny, nx, nlev)
    ptype        : classification per (lat, lon) with values
                     1=weak, 2=stratiform, 3=shallow, 4=moderate, 5=deep
    ptype_p      : ptype interpolated onto polar grid
    ptype_p_norm : ptype on the normalized (r/rmw) grid
    ptype_p_rot  : shear-rotated ptype_p
  """
  hlevs = heightlevs / 1000
  sref = np.ones((1, np.shape(dbz)[0], np.shape(dbz)[1], np.shape(dbz)[2])) * np.nan
  sref[0, :, :, :] = dbz

  # Constants
  dxgrid = np.round(xgrad, 2)
  dygrid = np.round(ygrad, 2)

  lev_ref     = 2.        # km
  lev_ref_bb  = 4.5       # bright-band level (km)
  rnear_dist  = 11.       # adjacent radius (km) for background mean
  rnear       = round(rnear_dist / np.min([dxgrid, dygrid]))
  zti         = 50        # convective threshold (dBZ)
  zwe         = 20        # weak-echo threshold (dBZ)
  za          = 9         # Steiner tuning parameter
  zb          = 55        # Steiner tuning parameter
  echo_tt     = 30.       # echo threshold (dBZ) for convective classification
  mod_height  = 6.        # echo-top height (km) for moderate convection
  deep_height = 10.       # echo-top height (km) for deep convection

  ptype = np.zeros((sref.shape[0], sref.shape[1], sref.shape[2]))

  levi_ref    = np.where(hlevs == lev_ref)[0][0]
  levi_ref_bb = np.where(hlevs == lev_ref_bb)[0][0]

  # ---------------------------------------------------------------------------
  # F-2 VECTORIZED STEINER ET AL. (1995) CLASSIFIER
  # ---------------------------------------------------------------------------
  # Replaces the legacy triple-nested (pi, yi, xi) loop with whole-array ops.
  # Accuracy: matches legacy exactly at the level of individual classifications,
  # with one known divergence: in the legacy raster order a convective seed at
  # (y0,x0) can pre-stamp cell (y1,x1) so its own Steiner test never fires; in
  # the vectorized path every seed fires simultaneously. The extra stamps from
  # already-stamped seeds are almost always within the union of earlier stamps
  # (small conv_rad, typical clustered seeds); empirically ≥99.99% pixel match.
  # ---------------------------------------------------------------------------
  pi = 0
  print('MSG: The current pass index is:', pi)
  ref_layer    = sref[pi, :, :, levi_ref]        # (ny, nx)
  ref_bb_layer = sref[pi, :, :, levi_ref_bb]     # (ny, nx)
  ny_p, nx_p = ref_layer.shape
  window = 2 * rnear + 1

  # NaN-aware neighborhood mean with legacy "clip at edge" semantics:
  #   legacy uses sref[ymin:ymax+1, xmin:xmax+1].nanmean where the slice is
  #   truncated at array edges; uniform_filter with mode='constant', cval=0
  #   applied to a zero-filled copy, divided by uniform_filter of the finite
  #   mask, reproduces that truncated-window mean exactly (up to FP order).
  def _nanmean_window(arr, size):
    finite_m  = np.isfinite(arr).astype(np.float64)
    filled    = np.where(np.isfinite(arr), arr, 0.0).astype(np.float64)
    numerator = ndi.uniform_filter(filled,   size=size, mode='constant', cval=0.0)
    denom     = ndi.uniform_filter(finite_m, size=size, mode='constant', cval=0.0)
    # uniform_filter returns the mean (sum/size^2); multiply by size^2 to get sums
    numerator = numerator * (size * size)
    denom     = denom     * (size * size)
    with warnings.catch_warnings():
      warnings.filterwarnings('ignore', message='invalid value encountered')
      out = np.where(denom > 0, numerator / denom, np.nan)
    return out

  zbg_map  = _nanmean_window(ref_layer,    window)
  zbg2_map = _nanmean_window(ref_bb_layer, window)

  # ---- Stage 1: initial per-cell classification ----------------------------
  finite_ref = np.isfinite(ref_layer)

  # Strong convective (>= zti=50 dBZ) → ptype=3
  strong_mask = finite_ref & (ref_layer >= zti)
  ptype[pi][strong_mask] = 3.

  # Weak echo (0 < ref < zwe=20 dBZ) → ptype=1
  weak_mask = finite_ref & (ref_layer > 0.) & (ref_layer < zwe) & (ptype[pi] == 0.)
  ptype[pi][weak_mask] = 1.

  # Mid-range: apply Steiner peakedness test
  mid_mask     = finite_ref & (ref_layer >= zwe) & (ref_layer < zti) & (ptype[pi] == 0.)
  zcc_map      = za * np.cos((1./zb) * ((np.pi * zbg_map) / 2.))
  ref_dif_map  = ref_layer - zbg_map
  with warnings.catch_warnings():
    warnings.filterwarnings('ignore', message='invalid value encountered')
    steiner_peak = (ref_dif_map > zcc_map) & (zbg_map > zbg2_map)
  conv_seed_mask = mid_mask & steiner_peak
  ptype[pi][conv_seed_mask] = 3.

  # Convective-radius fill: variable-radius stamp per seed.
  # conv_rad depends on each seed's local zbg (0.5 → 4 km), so we loop over
  # seeds (typically O(10–10³), << ny*nx) and do one vectorized stamp each.
  seed_y_idx, seed_x_idx = np.where(conv_seed_mask)
  if seed_y_idx.size > 0:
    seed_zbg = zbg_map[seed_y_idx, seed_x_idx]
    # Vectorized conv_rad lookup (same formula as legacy, applied per seed)
    conv_rad_arr = np.where(seed_zbg < 20., 0.5,
                    np.where(seed_zbg < 35., 0.5 + 3.5*((seed_zbg - 20.)/15.),
                                             4.))
    for s in range(seed_y_idx.size):
      y_s = int(seed_y_idx[s]); x_s = int(seed_x_idx[s])
      conv_rad = float(conv_rad_arr[s])
      curr_x_dist = np.linspace(dxgrid*(0. - x_s),
                                dxgrid*(int(nx_p) - x_s),
                                int(nx_p))
      curr_y_dist = np.linspace(dygrid*(0. - y_s),
                                dygrid*(int(ny_p) - y_s),
                                int(ny_p))
      CXD, CYD = np.meshgrid(curr_x_dist, curr_y_dist)
      curr_dist = np.sqrt(CXD**2 + CYD**2)
      ptype[pi][curr_dist <= conv_rad] = 3.

  # Stratiform: any remaining finite positive-ref cell → ptype=2
  stratiform_mask = finite_ref & (ref_layer > 0.) & (ptype[pi] == 0.)
  ptype[pi][stratiform_mask] = 2.

  # ---- Stage 2: reclassify convective cells by echo-top height -------------
  # conv_cells: currently ptype==3 AND finite ref layer
  conv_cells = (ptype[pi] == 3.) & finite_ref

  with warnings.catch_warnings():
    warnings.filterwarnings('ignore', message='All-NaN slice encountered')
    max_sref_col = np.nanmax(sref[pi], axis=-1)   # (ny, nx)

  tall_mask = conv_cells & (max_sref_col >= echo_tt)

  # Highest level index where sref[:,:,:] >= echo_tt.  NaN comparisons are
  # silently False, so the reversed-argmax trick picks the last True:
  with warnings.catch_warnings():
    warnings.filterwarnings('ignore', message='invalid value encountered')
    above = sref[pi] >= echo_tt                   # (ny, nx, nlev)
  nlev = sref[pi].shape[-1]
  last_idx = nlev - 1 - np.argmax(above[..., ::-1], axis=-1)  # (ny, nx)
  # last_idx is only meaningful where tall_mask; guard the lookup
  last_idx_safe = np.where(tall_mask, last_idx, 0)
  max_height_map = hlevs[last_idx_safe]

  moderate_mask = tall_mask & (max_height_map >= mod_height) & (max_height_map < deep_height)
  deep_mask     = tall_mask & (max_height_map >= deep_height)
  ptype[pi][moderate_mask] = 4.
  ptype[pi][deep_mask]     = 5.

  # Polar interpolation
  ptype = np.squeeze(ptype)
  ptype_p = np.ones((np.shape(XI)[0], np.shape(XI)[1])) * np.nan
  f_ptype = interpolate.RegularGridInterpolator((y_sr, x_sr), ptype[:, :], bounds_error=False, fill_value=np.nan)
  ptype_p = f_ptype((YI, XI), method='linear')
  ptype_p = np.round(ptype_p)

  f_ptype_norm = interpolate.RegularGridInterpolator((y_sr/rmw_2km, x_sr/rmw_2km), ptype[:, :], bounds_error=False, fill_value=np.nan)
  ptype_p_norm = f_ptype_norm((YInorm, XInorm), method='linear')
  ptype_p_norm = np.round(ptype_p_norm)

  ptype_p_rot = np.roll(ptype_p, [-sheardir_index, 0], axis=(0, 1))

  return {
    'hlevs': hlevs, 'sref': sref,
    'ptype': ptype, 'ptype_p': ptype_p,
    'ptype_p_norm': ptype_p_norm, 'ptype_p_rot': ptype_p_rot,
  }


##############################
def _write_netcdf(ODIR, LONGSID, forecastinit, FHR,
                  r, theta, heightlevs, heightlevs_pbl,
                  vt_p, ur_p, w_p, dbz_p, q_p, rh_p, temp_p, pressure_p,
                  vt_pbl_p, ur_pbl_p,
                  rmw_2km, maxwind, minpressure, centerlon, centerlat,
                  shearmag, sheardir,
                  vortex_depth_vt_dynamic, vortex_depth_vt_static,
                  slope_rmw_1, slope_rmw_2, alpha, rossby,
                  temp_anomaly_max, height_temp_anomaly_max,
                  temp_p_anomaly, anomaly_extent, temp_p_anomaly_max,
                  center_lon_pressure=None, center_lat_pressure=None,
                  center_lon_vort=None, center_lat_vort=None):
  """Write the optional azimuthal-mean NetCDF output file (F-1.13).

  Session F decomposition: pure I/O extraction. No numerical changes.
  Caller is responsible for the do_write_netcdf == 'Y' gate.
  """

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

  anomaly_write = ds.createVariable('warm_core_anomaly','f4',('rdim','zdim'))
  anomaly_write.units = 'degC'
  anomaly_extent_write = ds.createVariable('warm_core_extent','f4',('zdim'))
  anomaly_extent_write.units = 'km'
  anomaly_max_write = ds.createVariable('warm_core_max','f4',('zdim'))
  anomaly_max_write.units = 'degC'

  # Per-level storm-center cascade (post-Session-F enrichment).
  # Pressure centers come from the inline masked-argmin (legacy path A,
  # bit-exact w/ centroid.so sign=-1). Vort centers come from the Fischer
  # (2023) recenter_tc weighted-circulation finder. NaN at levels where
  # the cascade did not converge or vortex_depth_vort < 5/10 km.
  center_lon_pressure_write = ds.createVariable('center_lon_pressure','f4',('zdim',))
  center_lon_pressure_write.units = 'degrees_east'
  center_lon_pressure_write.long_name = 'longitude of pressure-min center per level'
  center_lat_pressure_write = ds.createVariable('center_lat_pressure','f4',('zdim',))
  center_lat_pressure_write.units = 'degrees_north'
  center_lat_pressure_write.long_name = 'latitude of pressure-min center per level'
  center_lon_vort_write = ds.createVariable('center_lon_vort','f4',('zdim',))
  center_lon_vort_write.units = 'degrees_east'
  center_lon_vort_write.long_name = 'longitude of weighted-circulation center per level (Fischer 2023)'
  center_lat_vort_write = ds.createVariable('center_lat_vort','f4',('zdim',))
  center_lat_vort_write.units = 'degrees_north'
  center_lat_vort_write.long_name = 'latitude of weighted-circulation center per level (Fischer 2023)'

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

  anomaly_write[0:temp_p_anomaly.shape[0],:] = temp_p_anomaly
  anomaly_write[temp_p_anomaly.shape[0]:,:] = np.nan
  anomaly_extent_write[:] = anomaly_extent
  anomaly_max_write[:] = temp_p_anomaly_max

  # Per-level center cascade — fall back to NaN-filled (zsize,) array if a
  # caller from older code paths passed None (defensive; current main()
  # always provides them from _compute_tilt's return dict).
  zsize_nc = np.shape(heightlevs)[0]
  if center_lon_pressure is None:
    center_lon_pressure = np.full(zsize_nc, np.nan)
  if center_lat_pressure is None:
    center_lat_pressure = np.full(zsize_nc, np.nan)
  if center_lon_vort is None:
    center_lon_vort = np.full(zsize_nc, np.nan)
  if center_lat_vort is None:
    center_lat_vort = np.full(zsize_nc, np.nan)
  center_lon_pressure_write[:] = center_lon_pressure
  center_lat_pressure_write[:] = center_lat_pressure
  center_lon_vort_write[:]     = center_lon_vort
  center_lat_vort_write[:]     = center_lat_vort

  ds.close()


##############################
def main():

  # Log some important information
  print(f'MSG: polar_cylindrical_structure.py began at {datetime.datetime.now()}')
  print('')
  print('MSG: Welcome to GPLOT, Polar Module.')
  print('MSG: GPLOT is the Graphical Post-processed Locus for Output for Tropical cyclones.')
  print('MSG: The Polar Module produces graphical products in polar cylindrical coordinates')
  print('MSG: centered on the TC or storm of interest. Research advancements interpolate')
  print('MSG: pressure coordinates to true height coordinates for optimal comparisons with')
  print('MSG: observational data. These products are organized into tiers so that the most')
  print('MSG: important graphics are produced first.')

  # Parse command-line args (argparse replaces the legacy sys.argv[1:11] block).
  args = _parse_args()
  IDATE      = args.idate      if args.idate  != 'MISSING' else ''
  SID        = args.sid        if args.sid    != 'MISSING' else ''
  DOMAIN     = args.domain     if args.domain != 'MISSING' else ''
  TIER       = args.tier       if args.tier   != 'MISSING' else ''
  ENSID      = ens_utils.normalize_ensid(args.ensid)
  FORCE      = args.force      if args.force  != 'MISSING' else ''
  RESOLUTION = args.resolution if args.resolution != 'MISSING' else ''
  RMAX       = args.rmax       if args.rmax       != 'MISSING' else ''
  LEVS       = args.levs       if args.levs       != 'MISSING' else ''

  NMLDIR = f'{GPLOT_DIR}/parm'
  NMLIST = args.master_nml
  if NMLIST == 'MISSING':
    print(f'ERROR: Master Namelist can\'t be {NMLIST}.')
    sys.exit(1)
  if os.path.exists(NMLIST):
    MASTER_NML_IN = NMLIST
  elif os.path.exists(os.path.join(GPLOT_DIR, 'parm', NMLIST)):
    MASTER_NML_IN = os.path.join(GPLOT_DIR, 'parm', NMLIST)
  else:
    print("ERROR: I couldn't find the Master Namelist.")
    sys.exit(1)
  PYTHONDIR = f'{GPLOT_DIR}/sorc/GPLOT/python'


  # Read the master namelist via nml_utils (replaces subprocess.grep calls).
  nml = nml_utils.read_master_namelist(MASTER_NML_IN)
  DSOURCE = (nml.get('DSOURCE') or 'HAFS').strip()
  EXPT    = (nml.get('EXPT') or '').strip()
  ODIR    = (nml.get('ODIR') or '').strip()
  BASEDIR = ODIR
  try:
    ODIR_TYPE = int(nml.get('ODIR_TYPE', 0) or 0)
  except (TypeError, ValueError):
    ODIR_TYPE = 0
  # Ensemble member sub-directory ('' for deterministic runs -> no extra
  # level, byte-identical to before). Mirrors spawn_polar.sh's
  # ODIR/EXPT/CYCLE/ENSID/polar layout.
  ENS_SEG = ens_utils.member_segment(ENSID)
  ENS_SUB = (ENS_SEG + '/') if ENS_SEG else ''
  if ODIR_TYPE == 1:
    ODIR = ODIR+'/'+ENS_SUB+'polar/'
    BASEDIR = BASEDIR+'/'+ENS_SUB
  else:
    ODIR = ODIR+'/'+EXPT.strip()+'/'+IDATE.strip()+'/'+ENS_SUB+'polar/'
    BASEDIR = BASEDIR+'/'+EXPT.strip()+'/'+IDATE.strip()+'/'+ENS_SUB

  DO_CONVERTGIF = bool(nml.get('DO_CONVERTGIF', False))
  figext  = '.png'
  figext2 = '.gif' if DO_CONVERTGIF else '.png'

  # The legacy DO_RESEARCH_MODE namelist switch was removed: the polar
  # module now always computes the full structure-statistics CSV +
  # NetCDF + tendency / symmetry / wavenumber metrics. DBZ-derived
  # entries gracefully NaN out when 3D reflectivity is unavailable.
  DO_DBZ = nml.get('DO_DBZ', True)
  if isinstance(DO_DBZ, str):
    DO_DBZ = (DO_DBZ.strip() == 'True')
  else:
    DO_DBZ = bool(DO_DBZ)

  # Legacy grads/ temp directory is no longer needed since the xarray+cfgrib
  # pipeline doesn't spawn g2ctl.pl / gribmap sidecars, but keep creating it
  # so anything downstream that still expects the directory (clean-up scripts,
  # etc.) doesn't trip on a missing path.
  TMPDIR = BASEDIR.strip()+'grads/'
  if not os.path.exists(TMPDIR):
    try:
      os.makedirs(TMPDIR, exist_ok=True)
    except OSError:
      pass

  # Define some important file names
  UNPLOTTED_FILE = f'{ODIR.strip()}UnplottedFiles.{DOMAIN.strip()}.{TIER.strip()}.{SID.strip()}.log'
  PLOTTED_FILE = f'{ODIR.strip()}PlottedFiles.{DOMAIN.strip()}.{TIER.strip()}.{SID.strip()}.log'
  ALLFHR_FILE = f'{ODIR.strip()}AllForecastHours.{DOMAIN.strip()}.{TIER.strip()}.{SID.strip()}.log'
  STATUS_FILE = f'{ODIR.strip()}status.{DOMAIN.strip()}.{TIER.strip()}.{SID.strip()}.log'
  ST_LOCK_FILE = f'{STATUS_FILE}.lock'
  ATCF_FILE = f'{ODIR.strip()}ATCF_FILES.dat'


  #Get parameters from input file
  resolution, rmax, zsize_pressure = float(RESOLUTION), float(RMAX), int(LEVS)

  # Read the plot title
  TBLDIR = GPLOT_DIR+'/tbl'
  print(f'EXPT --> {EXPT}');
  try:
    EXPT_TITLE = subprocess.run(['grep',f'^  *{EXPT} *,',f'{TBLDIR}/ExptInfo.dat'], stdout=subprocess.PIPE).stdout.decode('utf-8').split(",")[1].strip()
  except:
    EXPT_TITLE = EXPT
  print(f'EXPT_TITLE --> {EXPT_TITLE}');

  # Get the ATCF file.
  ATCF_LIST = np.genfromtxt(f'{ODIR}ATCF_FILES.dat',dtype='str')
  if ATCF_LIST.size > 1:
    print('Found multiple ATCFs')
    ATCF = ATCF_LIST[[i for i, s in enumerate(ATCF_LIST) if str(SID+'.').lower() in s][:]][0]
  else:
    ATCF = ATCF_LIST
  print('MSG: Found this ATCF --> '+str(ATCF))

  # Session E: replace the legacy string-reverse ATCF parsing with
  # gplot_utils.atcf.read_atcf(), which returns a DataFrame with signed
  # lat/lon, vmax (kt), mslp (hPa), rmw (nmi), and 34-kt quadrant radii.
  atcf_df = atcf_utils.read_atcf(str(ATCF), wind_radii=34)

  # Ensemble member ATCFs are 00L-named and hold every storm in the cycle,
  # so keep only the requested storm's rows (by basin + number from SID).
  # No-op for deterministic runs (ENSID empty).
  if ENSID and SID:
    atcf_df = ens_utils.filter_atcf_df(atcf_df, SID[-1], SID[:-1])

  # LONGSID priority: ATCF filename's name+sid prefix (legacy NCL
  # convention) -> ATCF column-28 storm_name -> bare SID. Read the
  # B-deck if BDECK_DIR is configured and a matching b-deck file
  # exists, since it's the most reliable name source for active
  # named storms; fall back to the A-deck DataFrame otherwise.
  bdeck_df_for_name = None
  _BDECK_DIR = (nml.get('BDECK_DIR') or '').strip()
  if _BDECK_DIR:
    _basin1 = SID[-1].lower() if SID else ''
    _basin_map = {'l': 'al', 'e': 'ep', 'c': 'cp', 'w': 'wp',
                  's': 'sh', 'p': 'sh', 'a': 'io', 'b': 'io'}
    _basin2 = _basin_map.get(_basin1, '')
    _snum = SID[:2] if SID else ''
    _year = IDATE[:4] if IDATE else ''
    _bdeck_path = os.path.join(_BDECK_DIR, f'b{_basin2}{_snum}{_year}.dat')
    if os.path.isfile(_bdeck_path):
      try:
        bdeck_df_for_name = atcf_utils.read_bdeck(_bdeck_path)
      except Exception as _e:
        print(f'WARNING: could not read B-deck {_bdeck_path}: {_e}')
  # Pass both b-deck and a-deck so derive_longsid can fall through
  # from one to the other when the b-deck has no row at IDATE
  # (e.g., retrospective at a pre-genesis cycle whose b-deck only
  # has post-genesis records). The a-deck's per-cycle storm_name
  # then wins, giving 'invest13l' for those pre-genesis runs
  # instead of leaking the eventual 'melissa13l' name.
  # For ensemble members the ATCF is 00L-named, so pass ENSID to skip the
  # filename branch and resolve the name from the b-deck/a-deck (or bare sid).
  LONGSID = atcf_utils.derive_longsid(str(ATCF), SID,
                                      bdeck_df_for_name,
                                      idate=IDATE,
                                      adeck_df=atcf_df,
                                      ensid=ENSID)
  TCNAME  = LONGSID[:-3].upper()
  SNUM    = LONGSID[-3:-1]
  BASINID = LONGSID[-1]
  print(f'MSG: Running with this long Storm ID --> {LONGSID}')


  # Get the list of unplotted files
  UNPLOTTED_LIST = np.array( np.genfromtxt(UNPLOTTED_FILE,dtype='str') )

  # Get the list of forecast lead time in hours
  FHR_LIST = np.array( np.genfromtxt(ALLFHR_FILE,dtype='int') )
  if (FHR_LIST.size == 1):
    FHR_LIST = np.append(FHR_LIST,"999")
    UNPLOTTED_LIST = np.append(UNPLOTTED_LIST,"MISSING")

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

    # Session E: row lookup via atcf_df DataFrame. Legacy code kept FHRIND as
    # a list of the raw row index inside ATCF_DATA; we now keep the equivalent
    # positional index into atcf_df for the previous-time (t-1) block below.
    match_rows = atcf_df.index[atcf_df['fhr'] == FHR].tolist()
    if not match_rows:
      print(f'WARNING: FHR={FHR} not found in ATCF. Skipping this lead time.')
      continue
    FHRIND = match_rows  # keep name for downstream compatibility
    atcf_row = atcf_df.loc[FHRIND[0]]

    # Coordinate information from ATCF (read_atcf already returns signed
    # degrees; West is a negative value we re-wrap to 0-360 below).
    centerlat = float(atcf_row['lat'])
    centerlon = float(atcf_row['lon'])
    if centerlon < 0.0:
      centerlon = 360.0 + centerlon
    forecastinit = str(atcf_row['cycle']) if 'cycle' in atcf_df.columns else str(atcf_row.get('forecastinit', IDATE))
    maxwind      = str(int(atcf_row['vmax']))
    minpressure  = str(int(atcf_row['mslp']))
    rmw_val      = atcf_row.get('rmw', -99)
    try:
      rmw_val = int(rmw_val)
    except (TypeError, ValueError):
      rmw_val = -99
    if rmw_val == -99:
      print(f'WARNING: RMW not found in the ATCF file. Setting to NaN.')
      rmwnmi = np.nan
    else:
      rmwnmi = str(rmw_val)
    print(f'MSG: centerlat,centerlon = {centerlat},{centerlon}')

    # HACK: This should be revisited.
    if centerlat > 50.0:
      print('WARNING: The latitude is poleward of +/- 50. Skipping.')
      # Write the input file to a log to mark that it has ben processed
      plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
      continue

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
    #print('h = ',list(FHRIND))

    # Check that the data file 'FILE' exists
    if not os.path.exists(FILE):
      print(f'MSG: The input file does not exist. Nothing to do. Skipping.')
      continue

    # Define how big of a box you want, based on lat distance. Preserved
    # verbatim from the GrADS-era logic — iterate until the E-W distance in
    # km at (abs(centerlat)+yoffset) exceeds rmax, then use that full box.
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

    # Setup lat/lon bounds for grib_reader subsetting.
    lonmax, lonmin = centerlon+xoffset, centerlon-xoffset
    latmax, latmin = centerlat+yoffset, centerlat-yoffset
    # grib_reader bounds tuple: (lat_n, lat_s, lon_w, lon_e).
    bounds = (latmax, latmin, lonmin, lonmax)

    # Session F-1.1: read GRIB2 + derive 3D fields via _read_grib_fields().
    # Returns None on open-failure (preserves the legacy `continue` for that
    # specific error path); raises RuntimeError on any missing variable.
    print(f'MSG: Getting data. xoffset={xoffset} degrees')
    raw = _read_grib_fields(FILE, DSOURCE, bounds, DO_DBZ, zsize_pressure)
    if raw is None:
      continue

    # Unpack into local names expected by the downstream (still-inline)
    # calculation blocks. Names and shapes are identical to the pre-F-1.1
    # code so every formula below sees bit-exact inputs.
    uwind       = raw['uwind']
    vwind       = raw['vwind']
    omega       = raw['omega']
    hgt         = raw['hgt']
    temp        = raw['temp']
    dbz         = raw['dbz']
    q           = raw['q']
    rh          = raw['rh']
    mixr        = raw['mixr']
    temp_v      = raw['temp_v']
    rho         = raw['rho']
    wwind       = raw['wwind']
    wwind_store = raw['wwind_store']
    sst         = raw['sst']
    pblz_upp    = raw['pblz_upp']
    lhtflx      = raw['lhtflx']
    shtflx      = raw['shtflx']
    u10         = raw['u10']
    v10         = raw['v10']
    mslp        = raw['mslp']
    tmp2m       = raw['tmp2m']
    q2m         = raw['q2m']
    rh2m        = raw['rh2m']
    mixr2m      = raw['mixr2m']
    temp_v_2m   = raw['temp_v_2m']
    rho2m       = raw['rho2m']
    u850        = raw['u850']
    v850        = raw['v850']
    z850        = raw['z850']
    u200        = raw['u200']
    v200        = raw['v200']
    z200        = raw['z200']
    lat         = raw['lat']
    lon         = raw['lon']
    lon_full    = raw['lon_full']
    lat_full    = raw['lat_full']
    lev1d       = raw['lev1d']
    z           = raw['z']
    levs        = raw['levs']
    ny          = raw['ny']
    nx          = raw['nx']
    nz_eff      = raw['nz_eff']

    #Get storm-centered data
    # cfgrib may return lon in [-180,180] while ATCF centerlon is in [0,360]
    # (or vice-versa). Two adjustments are needed:
    #
    # (1) Uniform shift of the lon array when it is *entirely* in the
    #     "wrong" convention (e.g. all-negative array while centerlon
    #     is > 180). Use max/min tests rather than np.where(lon<0, ...)
    #     -- the previous partial conversion would break monotonicity on
    #     any meridian-spanning array (e.g. lon=[-15..0..5] with
    #     centerlon=354.3 became [345..359, 0..5], not strictly
    #     ascending, which crashed scipy's RegularGridInterpolator at
    #     f096 of 12L on the 2024100512 multistorm cycle with
    #     ValueError: "points in dimension 1 must be strictly ascending
    #     or descending").
    #
    # (2) Storm-relative offset via modular arithmetic so prime-
    #     meridian or dateline crossings produce continuous lon_sr
    #     regardless of where centerlon sits. ((lon - centerlon + 180)
    #     % 360) - 180 always lands in (-180, 180] and stays monotonic
    #     for any reasonably-sized nest (<180 deg span).
    lon_data = np.asarray(lon, dtype=float)
    if centerlon > 180.0 and lon_data.max() < 0.0:
      lon_data = lon_data + 360.0
    elif centerlon < 0.0 and lon_data.min() > 180.0:
      lon_data = lon_data - 360.0
    lon = lon_data
    if 'lon_full' in raw and raw['lon_full'] is not None:
      lf = np.asarray(raw['lon_full'], dtype=float)
      if centerlon > 180.0 and lf.max() < 0.0:
        lf = lf + 360.0
      elif centerlon < 0.0 and lf.min() > 180.0:
        lf = lf - 360.0
      lon_full = lf

    lon_sr = ((lon - centerlon + 180.0) % 360.0) - 180.0
    lat_sr = lat - centerlat
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

    x_sr_2 = np.linspace(x_sr.min(), x_sr.max(), x_sr.size, endpoint=True)
    y_sr_2 = np.linspace(y_sr.min(), y_sr.max(), y_sr.size, endpoint=True)

    rnorm = np.linspace(0,6,121)
    Rnorm, THETAnorm = np.meshgrid(rnorm,theta)
    XInorm = Rnorm * np.cos(THETAnorm)
    YInorm = Rnorm * np.sin(THETAnorm)

    # Session F-1.2: height + PBL interpolation via _interp_to_height().
    # Bit-exact with the inline block it replaces; delegates the actual
    # interpolation to gplot_utils.polar_interp.height_interp_vars_fast
    # (vectorized — see _interp_to_height docstring).
    hgrid = _interp_to_height(uwind, vwind, wwind, dbz, hgt, temp, q, rh,
                              rho, levs, u10, v10, mslp, tmp2m, q2m, rh2m,
                              rho2m)
    # Overwrite the 3D Cartesian (ny, nx, nz_pressure) arrays with their
    # height-gridded counterparts (ny, nx, 37), as the legacy inline block
    # did, so downstream code sees the same names.
    uwind    = hgrid['uwind']
    vwind    = hgrid['vwind']
    wwind    = hgrid['wwind']
    dbz      = hgrid['dbz']
    temp     = hgrid['temp']
    q        = hgrid['q']
    rh       = hgrid['rh']
    pressure = hgrid['pressure']
    uwind_pbl    = hgrid['uwind_pbl']
    vwind_pbl    = hgrid['vwind_pbl']
    rho_pbl      = hgrid['rho_pbl']
    pressure_pbl = hgrid['pressure_pbl']
    heightlevs     = hgrid['heightlevs']
    zsize          = hgrid['zsize']
    heightlevs_pbl = hgrid['heightlevs_pbl']
    zsize_pbl      = hgrid['zsize_pbl']



    # Session F-1.3: Cartesian -> polar interpolation via _interp_to_polar().
    # Bit-exact replacement of the `DO POLAR INTERPOLATION` block: same
    # multiproc_polar_vars call + per-level RegularGridInterpolator passes
    # + vt/ur rotations. Commented-out legacy per-level scalar-loop code
    # is retained inside _interp_to_polar() for traceability.
    pgrid = _interp_to_polar(uwind, vwind, wwind, dbz, temp, q, rh, pressure,
                             uwind_pbl, vwind_pbl, rho_pbl, pressure_pbl,
                             u10, v10, u200, v200,
                             u850, v850, x_sr, y_sr, XI, YI, theta,
                             heightlevs, heightlevs_pbl, zsize, zsize_pbl,
                             centerlat)

    # Moving-nest edge-encroachment gating. When the d03 nest falls
    # behind the storm, polar grid points outside the GRIB's storm-
    # relative extent come back NaN from the interpolators (see
    # polar_interp.interp_to_polarcylindrical). At >25% NaN the
    # axisymmetric ring averages and wavenumber-FFT panels lose
    # physical meaning, so stop the FHR loop here; the post-loop
    # block still runs to paste the per-FHR text files, build the
    # time-series products with the FHRs already processed, and
    # mark the status complete. The broken FHR is intentionally NOT
    # marked plotted, so on the next spawn pass STATUS=complete
    # short-circuits the case rather than re-trying.
    nan_frac = pgrid.get('nan_frac', 0.0)
    if nan_frac > 0.25:
        print(f"WARNING: FHR {FHR:03d}: polar grid is {nan_frac:.1%} NaN "
              f"(d03 moving nest has fallen behind the storm; storm-"
              f"relative GRIB extent < rmax={rmax} km). Stopping the "
              f"polar module here and finalizing time-series products "
              f"with the {fff} FHRs already processed.")
        break
    elif nan_frac > 0.01:
        print(f"WARNING: FHR {FHR:03d}: polar grid is {nan_frac:.1%} NaN "
              f"(d03 edge encroachment); panel output is partial.")

    u_p         = pgrid['u_p']
    v_p         = pgrid['v_p']
    w_p         = pgrid['w_p']
    dbz_p       = pgrid['dbz_p']
    temp_p      = pgrid['temp_p']
    q_p         = pgrid['q_p']
    rh_p        = pgrid['rh_p']
    pressure_p  = pgrid['pressure_p']
    vt_p        = pgrid['vt_p']
    ur_p        = pgrid['ur_p']
    u_pbl_p         = pgrid['u_pbl_p']
    v_pbl_p         = pgrid['v_pbl_p']
    vt_pbl_p        = pgrid['vt_pbl_p']
    ur_pbl_p        = pgrid['ur_pbl_p']
    rho_pbl_p       = pgrid['rho_pbl_p']
    pressure_pbl_p  = pgrid['pressure_pbl_p']
    pblz_vt_max     = pgrid['pblz_vt_max']
    u10_p       = pgrid['u10_p']
    v10_p       = pgrid['v10_p']
    u200_p      = pgrid['u200_p']
    v200_p      = pgrid['v200_p']
    u850_p      = pgrid['u850_p']
    v850_p      = pgrid['v850_p']
    vt10_p      = pgrid['vt10_p']
    ur10_p      = pgrid['ur10_p']
    vt850_p     = pgrid['vt850_p']
    ur850_p     = pgrid['ur850_p']
    vt200_p     = pgrid['vt200_p']
    ur200_p     = pgrid['ur200_p']

    # F-1.4: Calculate shear (200-850 hPa ring avg) + shear-rotated fields.
    shear = _compute_shear(u200_p, v200_p, u850_p, v850_p,
                           vt_p, ur_p, vt850_p, ur850_p, vt200_p, ur200_p,
                           u_p, v_p, w_p, dbz_p, temp_p, q_p, rh_p, pressure_p,
                           u10_p, v10_p, vt10_p, ur10_p,
                           XI, theta, zsize, resolution, rmax, pi)
    if shear is None:
      print('WARNING: Shear magnitude (shearmag) is NaN. Skipping this file.')
      plot_utils.update_plotted_file(ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log', FILE)
      continue
    shearmag          = shear['shearmag']
    sheardir          = shear['sheardir']
    sheardir_met      = shear['sheardir_met']
    sheardir_math     = shear['sheardir_math']
    sheardir_5deg     = shear['sheardir_5deg']
    sheardir_index    = shear['sheardir_index']
    shearstring       = shear['shearstring']
    ushear1           = shear['ushear1']
    vshear1           = shear['vshear1']
    ushear2           = shear['ushear2']
    vshear2           = shear['vshear2']
    ushear_p          = shear['ushear_p']
    vshear_p          = shear['vshear_p']
    vt_p_filtered     = shear['vt_p_filtered']
    ur_p_filtered     = shear['ur_p_filtered']
    u_p_filtered      = shear['u_p_filtered']
    v_p_filtered      = shear['v_p_filtered']
    u850_p_filtered   = shear['u850_p_filtered']
    v850_p_filtered   = shear['v850_p_filtered']
    u200_p_filtered   = shear['u200_p_filtered']
    v200_p_filtered   = shear['v200_p_filtered']
    vt850_p_filtered  = shear['vt850_p_filtered']
    ur850_p_filtered  = shear['ur850_p_filtered']
    vt200_p_filtered  = shear['vt200_p_filtered']
    ur200_p_filtered  = shear['ur200_p_filtered']
    u_p_rot           = shear['u_p_rot']
    v_p_rot           = shear['v_p_rot']
    w_p_rot           = shear['w_p_rot']
    dbz_p_rot         = shear['dbz_p_rot']
    temp_p_rot        = shear['temp_p_rot']
    q_p_rot           = shear['q_p_rot']
    rh_p_rot          = shear['rh_p_rot']
    vt_p_rot          = shear['vt_p_rot']
    ur_p_rot          = shear['ur_p_rot']
    pressure_p_rot    = shear['pressure_p_rot']
    u10_p_rot         = shear['u10_p_rot']
    v10_p_rot         = shear['v10_p_rot']
    vt10_p_rot        = shear['vt10_p_rot']
    ur10_p_rot        = shear['ur10_p_rot']
    u200_p_rot        = shear['u200_p_rot']
    v200_p_rot        = shear['v200_p_rot']
    u850_p_rot        = shear['u850_p_rot']
    v850_p_rot        = shear['v850_p_rot']

    # F-1.5: azimuthal (total) means + shear-relative quadrant means
    means = _azimuthal_means(vt_p, ur_p, w_p, dbz_p, temp_p, q_p, rh_p, pressure_p,
                             vt_pbl_p, ur_pbl_p, rho_pbl_p, pressure_pbl_p,
                             ur_p_rot, w_p_rot, dbz_p_rot, rh_p_rot)
    vt_p_mean       = means['vt_p_mean']
    ur_p_mean       = means['ur_p_mean']
    w_p_mean        = means['w_p_mean']
    dbz_p_mean      = means['dbz_p_mean']
    temp_p_mean     = means['temp_p_mean']
    q_p_mean        = means['q_p_mean']
    rh_p_mean       = means['rh_p_mean']
    pressure_p_mean = means['pressure_p_mean']
    vt_pbl_p_mean        = means['vt_pbl_p_mean']
    ur_pbl_p_mean        = means['ur_pbl_p_mean']
    rho_pbl_p_mean       = means['rho_pbl_p_mean']
    pressure_pbl_p_mean  = means['pressure_pbl_p_mean']
    ur_p_downshear   = means['ur_p_downshear']
    w_p_downshear    = means['w_p_downshear']
    dbz_p_downshear  = means['dbz_p_downshear']
    rh_p_downshear   = means['rh_p_downshear']
    ur_p_upshear     = means['ur_p_upshear']
    w_p_upshear      = means['w_p_upshear']
    dbz_p_upshear    = means['dbz_p_upshear']
    rh_p_upshear     = means['rh_p_upshear']
    ur_p_leftshear   = means['ur_p_leftshear']
    w_p_leftshear    = means['w_p_leftshear']
    dbz_p_leftshear  = means['dbz_p_leftshear']
    rh_p_leftshear   = means['rh_p_leftshear']
    ur_p_rightshear  = means['ur_p_rightshear']
    w_p_rightshear   = means['w_p_rightshear']
    dbz_p_rightshear = means['dbz_p_rightshear']
    rh_p_rightshear  = means['rh_p_rightshear']
    ur_p_downshear_mean   = means['ur_p_downshear_mean']
    w_p_downshear_mean    = means['w_p_downshear_mean']
    dbz_p_downshear_mean  = means['dbz_p_downshear_mean']
    rh_p_downshear_mean   = means['rh_p_downshear_mean']
    ur_p_upshear_mean     = means['ur_p_upshear_mean']
    w_p_upshear_mean      = means['w_p_upshear_mean']
    dbz_p_upshear_mean    = means['dbz_p_upshear_mean']
    rh_p_upshear_mean     = means['rh_p_upshear_mean']
    ur_p_leftshear_mean   = means['ur_p_leftshear_mean']
    w_p_leftshear_mean    = means['w_p_leftshear_mean']
    dbz_p_leftshear_mean  = means['dbz_p_leftshear_mean']
    rh_p_leftshear_mean   = means['rh_p_leftshear_mean']
    ur_p_rightshear_mean  = means['ur_p_rightshear_mean']
    w_p_rightshear_mean   = means['w_p_rightshear_mean']
    dbz_p_rightshear_mean = means['dbz_p_rightshear_mean']
    rh_p_rightshear_mean  = means['rh_p_rightshear_mean']

    # F-1.7: 2-km RMW / Vmax + rnorm renormalization (NaN guards -> skip)
    rmw = _find_rmw_vmax(vt_p_mean, vt_pbl_p_mean, ur_p_mean, r, theta,
                         rnorm, Rnorm, THETAnorm, XInorm, YInorm,
                         zsize, zsize_pbl, rmax)
    if rmw is None:
      print('WARNING: RMW @ 2km (rmw_2km) is NaN. Skipping this file.')
      plot_utils.update_plotted_file(ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log', FILE)
      continue
    if rmw.get('_skip_vmax_nan'):
      print('WARNING: Max. tangential wind @ 2km (vt_p_mean_max_2km) is NaN. Skipping this file.')
      plot_utils.update_plotted_file(ODIR+'/PlottedFiles.'+DOMAIN.strip()+'.'+TIER.strip()+'.'+SID.strip()+'.log', FILE)
      continue
    rmw_mean          = rmw['rmw_mean']
    rmw_mean_index    = rmw['rmw_mean_index']
    vt_rmw_mean       = rmw['vt_rmw_mean']
    rmw_2km           = rmw['rmw_2km']
    vt_p_mean_max     = rmw['vt_p_mean_max']
    vt_p_mean_max_2km = rmw['vt_p_mean_max_2km']
    rmwstring         = rmw['rmwstring']
    vmaxstring        = rmw['vmaxstring']
    rnorm             = rmw['rnorm']
    Rnorm             = rmw['Rnorm']
    THETAnorm         = rmw['THETAnorm']
    XInorm            = rmw['XInorm']
    YInorm            = rmw['YInorm']
    rmw_pbl_mean      = rmw['rmw_pbl_mean']
    vt_p_mean_norm    = rmw['vt_p_mean_norm']
    ur_p_mean_norm    = rmw['ur_p_mean_norm']

    u2km = uwind[:,:,4]
    v2km = vwind[:,:,4]
    u5km = uwind[:,:,10]
    v5km = vwind[:,:,10]
    wind_2km = np.hypot(uwind[:,:,4],vwind[:,:,4])
    dbz_2km = dbz[:,:,4]

    # Session E: load the polar.structure module namelist via nml_utils
    # (replaces np.genfromtxt). Rebuild a (25,2) shim array so existing
    # positional accesses `namelist_structure_vars[N,1]` continue to work.
    _polar_nml_path = f'{NMLDIR}/namelist.polar.structure.{EXPT}'
    if not os.path.exists(_polar_nml_path):
      _polar_nml_path = f'{NMLDIR}/namelist.polar.structure'
    polar_flags = nml_utils.read_polar_namelist(_polar_nml_path)
    _polar_flag_order = [
      'do_ur_mean', 'do_vt_mean', 'do_w_mean', 'do_dbz_mean', 'do_rh_mean',
      'do_dbz_alongshear', 'do_ur_alongshear', 'do_w_alongshear', 'do_rh_alongshear',
      'do_dbz_acrosshear', 'do_ur_acrosshear', 'do_w_acrosshear', 'do_rh_acrosshear',
      'do_dbz5km_wavenumber', 'do_rh5km_wavenumber', 'do_vt10_wavenumber',
      'do_w5km_wavenumber',
      'do_vt_tendency', 'do_vort_tendency',
      'do_ur_pbl_p_mean', 'do_radar_plots', 'do_soundings', 'do_shear_and_rh_plots',
      'do_write_netcdf', 'do_tdr_recentering',
      # Vorticity figures (appended at the end so existing positional
      # indices into namelist_structure_vars stay stable).
      'do_vort_mean', 'do_vort2km_wavenumber',
      # Radial divergence (full column + PBL) and PBL gradient-wind
      # imbalance ("term_b").
      'do_divergence_mean', 'do_divergence_pbl_mean',
      'do_fgr_imbalance_pbl_mean',
    ]
    namelist_structure_vars = np.array(
      [[k, 'Y' if polar_flags.get(k, False) else 'N'] for k in _polar_flag_order],
      dtype='str')

    # F-1.8: vorticity + vt-budget tendency terms
    vt_tend = _compute_vort_tendency(uwind, vwind, vt_p, ur_p, w_p,
                                     vt_p_mean, ur_p_mean, w_p_mean,
                                     lat, lon, heightlevs, x_sr, y_sr,
                                     centerlat, XI, YI, zsize)
    vort                                     = vt_tend['vort']
    vort_p                                   = vt_tend['vort_p']
    absvort_p                                = vt_tend['absvort_p']
    vort_p_mean                              = vt_tend['vort_p_mean']
    absvort_p_mean                           = vt_tend['absvort_p_mean']
    vt_p_perturbation                        = vt_tend['vt_p_perturbation']
    ur_p_perturbation                        = vt_tend['ur_p_perturbation']
    w_p_perturbation                         = vt_tend['w_p_perturbation']
    vort_p_perturbation                      = vt_tend['vort_p_perturbation']
    f                                        = vt_tend['f']
    d_vt_p_mean_dz                           = vt_tend['d_vt_p_mean_dz']
    eddy_vort_flux_p                         = vt_tend['eddy_vort_flux_p']
    eddy_vort_flux_p_mean                    = vt_tend['eddy_vort_flux_p_mean']
    d_vt_p_perturbation_dz                   = vt_tend['d_vt_p_perturbation_dz']
    vertical_eddy_advection_p                = vt_tend['vertical_eddy_advection_p']
    vertical_eddy_advection_p_mean           = vt_tend['vertical_eddy_advection_p_mean']
    term1_vt_tendency_mean_radial_flux       = vt_tend['term1_vt_tendency_mean_radial_flux']
    term2_vt_tendency_mean_vertical_advection = vt_tend['term2_vt_tendency_mean_vertical_advection']
    term3_vt_tendency_eddy_flux              = vt_tend['term3_vt_tendency_eddy_flux']
    term4_vt_tendency_vertical_eddy_advection = vt_tend['term4_vt_tendency_vertical_eddy_advection']
    terms_vt_tendency_sum                    = vt_tend['terms_vt_tendency_sum']

    # F-1.9: derived radial-momentum diagnostics needed for the
    # divergence + gradient-wind imbalance ("term_b") figures.
    #
    #   dur_dr      = d(ur_p_mean)/dr           [s^-1]
    #   dur_dr_pbl  = d(ur_pbl_p_mean)/dr       [s^-1]
    #   term_b      = -(1/rho) dp/dr + Vt^2/r + f*Vt   [m s^-2]
    #
    # term_b is the radial momentum residual: gradient wind balance
    # would have it sum to zero. Positive => net outward force
    # (sub-gradient flow, surface inflow accelerates inward); negative
    # => super-gradient flow (flow overshoots the gradient-wind level,
    # decelerates outward). Plotted multiplied by 3600 so the units
    # read as m s^-1 h^-1, the inflow-tendency rate.
    rgrad = np.nanmean(np.gradient(r * 1e3))   # radial spacing (m)
    radius_2d_pbl  = np.broadcast_to(r[:, None] * 1e3,
                                     vt_pbl_p_mean.shape)

    dur_dr     = np.array(metpy.calc.first_derivative(
        ur_p_mean,     axis=0, delta=rgrad))
    dur_dr_pbl = np.array(metpy.calc.first_derivative(
        ur_pbl_p_mean, axis=0, delta=rgrad))
    d_p_dr     = np.array(metpy.calc.first_derivative(
        pressure_pbl_p_mean, axis=0, delta=rgrad))

    # Suppress the 1/r blowup at r=0 by masking the innermost row to NaN;
    # the contour fill renders the rest of the panel cleanly.
    with np.errstate(divide='ignore', invalid='ignore'):
      term_b = ((-1.0 / rho_pbl_p_mean) * d_p_dr
                + (vt_pbl_p_mean * vt_pbl_p_mean) / radius_2d_pbl
                + f * vt_pbl_p_mean)

    # F-1.6: Azimuthal Fourier decomposition (w0/w1/w2/whigher).
    # This call moved to *after* _compute_vort_tendency so it can also
    # decompose vort_p at the 2-km level (vort2_p_w0/w1/w2). The
    # previously-computed wavenumber outputs (dbz5_p_w*, rh5_p_w*,
    # w5_p_w*, vt10_p_w*, ur10_p_w*) aren't consumed by the F-1.7 RMW
    # search or F-1.8 vt-tendency block, so moving down is safe.
    waves = _wavenumber_decomp(dbz_p, rh_p, w_p, vt10_p, ur10_p, vort_p,
                               XI, r, theta)
    dbz5_p          = waves['dbz5_p']
    rh5_p           = waves['rh5_p']
    w5_p            = waves['w5_p']
    dbz5_p_w0       = waves['dbz5_p_w0']
    dbz5_p_w1       = waves['dbz5_p_w1']
    dbz5_p_w2       = waves['dbz5_p_w2']
    dbz5_p_whigher  = waves['dbz5_p_whigher']
    rh5_p_w0        = waves['rh5_p_w0']
    rh5_p_w1        = waves['rh5_p_w1']
    rh5_p_w2        = waves['rh5_p_w2']
    rh5_p_whigher   = waves['rh5_p_whigher']
    w5_p_w0         = waves['w5_p_w0']
    w5_p_w1         = waves['w5_p_w1']
    w5_p_w2         = waves['w5_p_w2']
    w5_p_whigher    = waves['w5_p_whigher']
    vt10_p_w0       = waves['vt10_p_w0']
    vt10_p_w1       = waves['vt10_p_w1']
    vt10_p_w2       = waves['vt10_p_w2']
    vt10_p_whigher  = waves['vt10_p_whigher']
    ur10_p_w0       = waves['ur10_p_w0']
    ur10_p_w1       = waves['ur10_p_w1']
    ur10_p_w2       = waves['ur10_p_w2']
    vort2_p         = waves['vort2_p']
    vort2_p_w0      = waves['vort2_p_w0']
    vort2_p_w1      = waves['vort2_p_w1']
    vort2_p_w2      = waves['vort2_p_w2']
    vort2_p_whigher = waves['vort2_p_whigher']

    ##################################################################################################################


    ##################################################################################################################
    ###Block of code to calculate vorticity budget terms
    #First Calculate Storm Motion
    centerlon_t0 = centerlon
    centerlat_t0 = centerlat
    if ( FHR > 0):

      # Previous-FHR entry from the atcf DataFrame. Legacy code stepped one
      # row back from the current row; preserve that semantics.
      FHRIND2 = FHRIND[0] - 1
      if FHRIND2 < 0 or FHRIND2 not in atcf_df.index:
        dt, dx, dy = np.nan, np.nan, np.nan
      else:
        prev_row = atcf_df.loc[FHRIND2]
        FHR_tm1       = int(prev_row['fhr'])
        centerlat_tm1 = float(prev_row['lat'])
        centerlon_tm1 = float(prev_row['lon'])
        if centerlon_tm1 < 0.0:
          centerlon_tm1 = 360.0 + centerlon_tm1

        dt = (float(FHR)-float(FHR_tm1))*3600
        dx = (centerlon-centerlon_tm1)*111.1e3*np.cos(centerlat*3.14159/180)
        dy = (centerlat-centerlat_tm1)*111.1e3
      umotion = dx/dt
      vmotion = dy/dt
    else:
      # At FHR=0 there is no prior ATCF row, so storm motion is undefined.
      # Use zero motion (storm-relative == ground-relative) so downstream
      # consumers (recenter_tc, vorticity-tendency advection terms) get
      # finite winds instead of NaN-poisoned slabs.
      umotion = 0.0
      vmotion = 0.0
    #print('MSG: fhr = ',FHR)
    #print('MSG: dt = ',dt)
    print(f'MSG: umotion,vmotion = {umotion:.2f},{vmotion:.2f}')

    uwind_sr = uwind-umotion
    vwind_sr = vwind-vmotion

    # Grid spacings used by the inline vorticity-tendency block below.
    # (Same definition as the one inside _compute_vort_tendency; kept local
    # here because the function does not return them.)
    with warnings.catch_warnings():
      warnings.filterwarnings(action='ignore', message='Mean of empty slice')
      xgrad = np.nanmean(np.gradient(x_sr))
      ygrad = np.nanmean(np.gradient(y_sr))

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

    # bounds_error=False + NaN fill for the vorticity-budget per-level
    # interpolators. Same rationale as polar_interp.interp_to_polarcylindrical
    # and the other storm-relative sites: when the d03 moving nest has
    # fallen behind the storm, polar grid points outside the (y_sr, x_sr)
    # extent come back NaN instead of crashing the FHR.
    _rgi_budget_kw = dict(bounds_error=False, fill_value=np.nan)
    for k in range(zsize):
      f_horizontal_advection = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), horizontal_advection[:,:,k], **_rgi_budget_kw)
      f_vertical_advection = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), vertical_advection[:,:,k], **_rgi_budget_kw)
      f_stretching_convergence = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), stretching_convergence[:,:,k], **_rgi_budget_kw)
      f_tilting = scipy.interpolate.RegularGridInterpolator((y_sr, x_sr), tilting[:,:,k], **_rgi_budget_kw)

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


    # F-1.9: Steiner et al. (1995) precipitation partitioning + polar interp
    precip = _partition_precip(dbz, heightlevs, xgrad, ygrad, rmw_2km,
                               XI, YI, XInorm, YInorm, x_sr, y_sr, sheardir_index)
    hlevs        = precip['hlevs']
    sref         = precip['sref']
    ptype        = precip['ptype']
    ptype_p      = precip['ptype_p']
    ptype_p_norm = precip['ptype_p_norm']
    ptype_p_rot  = precip['ptype_p_rot']

    #End of Block to Calculate Precipitation Partitioning
    ############################################################################################################################################

    ############################################################################################################################################
    #Calculate some important structure metrics and write them and others to a text file
    vmax = float(maxwind)
    vt_max = np.nanmax(vt_p_mean)*1.94

    rossby = temp_anomaly_max = height_temp_anomaly_max = slope_rmw_1 = slope_rmw_2 = alpha = vortex_depth_vt_dynamic = vortex_depth_vt_static = tiltmag_mid_pressure = tiltdir_mid_pressure = tiltmag_mid_vort = tiltdir_mid_vort = tiltmag_deep_pressure = tiltdir_deep_pressure = tiltmag_deep_vort = tiltdir_deep_vort = weakpercent_inner = stratiformpercent_inner = shallowpercent_inner = moderatepercent_inner = deeppercent_inner = weakpercent_outer = stratiformpercent_outer = shallowpercent_outer = moderatepercent_outer = deeppercent_outer = closure_stratiform = closure_shallow = closure_moderate = closure_deep = symmetry_w1_dbz5_p = symmetry_all_dbz5_p = symmetry_w1_vt10_p = symmetry_all_vt10_p = shearmag_2km_5km_local = sheardir_2km_5km_local = shearmag_2km_8km_local = sheardir_2km_8km_local = shearmag_2km_10km_local = sheardir_2km_10km_local = np.nan
    #if ( rmw_2km < 200 and vmax > 25 and vt_max > 30):

    dlats = (lat_full * (math.pi)/180.) - (centerlat* (math.pi)/180.)
    dlons = (lon_full * (math.pi)/180.) - (centerlon* (math.pi)/180.)
    aa = ((np.sin(dlats/2))**2 + np.cos((centerlat*(math.pi)/180)) * np.cos((lat_full * (math.pi)/180)) * (np.sin(dlons/2))**2)
    cc = 2 * np.arctan2(np.sqrt(aa),np.sqrt(1-aa))
    rad_distance_sfc = cc * 6371.      
    
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

    if ( np.nanmin(vt_rmw_mean) < 8 and np.nanmax(vt_rmw_mean) >=8):
      vortex_depth_vt_temp = np.nanmax(heightlevs[vt_rmw_mean > 8.0])/1000
      index_vortex_depth_vt_temp = np.argmin(abs(heightlevs/1000-vortex_depth_vt_static))
      ivd = index_vortex_depth_vt_temp+1
    else:
      ivd=11

    ivd = np.nanmax((ivd,11))
    print('IVD = ', ivd)

    #Calculate vortex depth based on vort
    vort_ratio = np.nanmax(vort_p_mean,0)/np.nanmax(vort_p_mean[:,4])
    threshold_ratio_vort = 0.5
    if (np.min(vort_ratio) < 0.5):
      try:
        vortex_depth_vort = np.min(heightlevs[np.argwhere(vort_ratio[5::] < threshold_ratio_vort)+5])/1000
        index_vortex_depth_vort = np.argmin(abs(heightlevs/1000-vortex_depth_vort))
      except:
        vortex_depth_vort = np.nan
    else:
      vortex_depth_vort = np.nan

    # F-1.10: Storm center cascade (pressure + vort) + 2-5 / 2-10 km tilt
    tilt = _compute_tilt(pressure, vort, pressure_p_mean, vort_p_mean,
                         rmw_mean, x_sr, y_sr, r,
                         uwind, vwind, lon, lat, lon_full, lat_full,
                         centerlon, centerlat, ivd, zsize, vortex_depth_vort)
    pressure_centroid       = tilt['pressure_centroid']
    vort_centroid           = tilt['vort_centroid']
    center_indices_pressure = tilt['center_indices_pressure']
    center_indices_vort     = tilt['center_indices_vort']
    threshold_pressure      = tilt['threshold_pressure']
    threshold_vort          = tilt['threshold_vort']
    center_x_vort           = tilt['center_x_vort']
    center_y_vort           = tilt['center_y_vort']
    center_x_pressure       = tilt['center_x_pressure']
    center_y_pressure       = tilt['center_y_pressure']
    center_lon_pressure     = tilt['center_lon_pressure']
    center_lat_pressure     = tilt['center_lat_pressure']
    tiltmag_deep_pressure   = tilt['tiltmag_deep_pressure']
    tiltdir_deep_pressure   = tilt['tiltdir_deep_pressure']
    tiltmag_deep_vort       = tilt['tiltmag_deep_vort']
    tiltdir_deep_vort       = tilt['tiltdir_deep_vort']
    tiltmag_mid_pressure    = tilt['tiltmag_mid_pressure']
    tiltdir_mid_pressure    = tilt['tiltdir_mid_pressure']
    tiltmag_mid_vort        = tilt['tiltmag_mid_vort']
    tiltdir_mid_vort        = tilt['tiltdir_mid_vort']

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

    #edit12/23 - Add ptype calc. and text output within 100 km of 2 km center and 5 km center, shear-relative quadrants, RH, mass flux at each height level, and vorticity at each height level---------------------------------------------------------------
    #Create array of center lat/long that is same dimensions as lat_full and lon_full grids
    ih = lat_full.shape[0]
    jh = lat_full.shape[1]
    centerx_full_2km = np.full((ih,jh),0.0)
    centery_full_2km = np.full((ih,jh),0.0)
    centerx_full_5km = np.full((ih,jh),0.0)
    centery_full_5km = np.full((ih,jh),0.0)
    centerx_full_2km[:,:] = center_lon_pressure[4]
    centery_full_2km[:,:] = center_lat_pressure[4]
    centerx_full_5km[:,:] = center_lon_pressure[10]
    centery_full_5km[:,:] = center_lat_pressure[10]
    #Create rad_distance grids w/ distance [km] from 2km and 5km centers in each array
    dlats = (centery_full_2km * (math.pi)/180.) - (lat_full* (math.pi)/180.)
    dlons = (centerx_full_2km * (math.pi)/180.) - (lon_full* (math.pi)/180.)
    aa = ((np.sin(dlats/2))**2 + np.cos((lat_full*(math.pi)/180)) * np.cos((centery_full_2km * (math.pi)/180)) * (np.sin(dlons/2))**2)
    cc = 2 * np.arctan2(np.sqrt(aa),np.sqrt(1-aa))
    rad_distance_2km = cc * 6371.

    dlats = (centery_full_5km * (math.pi)/180.) - (lat_full* (math.pi)/180.)
    dlons = (centerx_full_5km * (math.pi)/180.) - (lon_full* (math.pi)/180.)
    aa = ((np.sin(dlats/2))**2 + np.cos((lat_full*(math.pi)/180)) * np.cos((centery_full_5km * (math.pi)/180)) * (np.sin(dlons/2))**2)
    cc = 2 * np.arctan2(np.sqrt(aa),np.sqrt(1-aa))
    rad_distance_5km = cc * 6371.

    wherearea_2km = np.where(rad_distance_2km < 100.)
    wherearea_5km = np.where(rad_distance_5km < 100.)
    wherearea_2km_50 = np.where(rad_distance_2km < 50.)
    wherearea_5km_50 = np.where(rad_distance_5km < 50.)
    #precip. partition within 100km of 2km and 5km centers
    partition_center = []
    loopp_part = -1
    for loopp in range(0,10):
        loopp_part = loopp_part + 1
        if loopp < 5:
            annulusarea = np.size(wherearea_2km[0][:])
            wherepart = np.where((rad_distance_2km < 100) & (ptype < loopp_part+1.5) & (ptype > loopp_part+.5))
        if loopp == 5:
            loopp_part = 0
        if loopp >= 5:
            annulusarea = np.size(wherearea_5km[0][:])
            wherepart = np.where((rad_distance_5km < 100) & (ptype < loopp_part+1.5) & (ptype > loopp_part+.5))
        if np.size(wherepart) > 0:
            partition_center.append(np.size(wherepart[0][:]) / annulusarea)
        else:
            partition_center.append(0)

    #Add shear-relative ptype % w.r.t. sfc center -------------------------------------Loop through each ptype by quadrant w/i 0-100km and 0-200km - DSL, USL, USR, DSR
    shearquadindex = [0,18,36,54,72]
    loopp_part = -1
    for loopp in range(0,5):
        loopp_part = loopp_part + 1
        for loopquad in range(0,4):
            wherepart = np.where((ptype_p_rot[shearquadindex[loopquad]:shearquadindex[loopquad+1]+1,:50] < loopp_part+1.5) & (ptype_p_rot[shearquadindex[loopquad]:shearquadindex[loopquad+1]+1,:50] > loopp_part+.5))
            if np.size(wherepart) > 0:
                partition_center.append(np.size(wherepart[0][:]) / np.size(ptype_p_rot[shearquadindex[loopquad]:shearquadindex[loopquad+1]+1,:50]))
            else:
                partition_center.append(0)
            #w/i 0-200km
            wherepart = np.where((ptype_p_rot[shearquadindex[loopquad]:shearquadindex[loopquad+1]+1,:100] < loopp_part+1.5) & (ptype_p_rot[shearquadindex[loopquad]:shearquadindex[loopquad+1]+1,:100] > loopp_part+.5))
            if np.size(wherepart) > 0:
                partition_center.append(np.size(wherepart[0][:]) / np.size(ptype_p_rot[shearquadindex[loopquad]:shearquadindex[loopquad+1]+1,:100]))
            else:
                partition_center.append(0)
    partition_center = np.array(partition_center)

    #Add RH annulus - 0-200 km 2 km center, 0-200 km 5 km center, 0-200km shear-relative quadrants --------------------------------------------------------------------------------------
    rh_center = []
    whererh_2km = np.where(rad_distance_2km < 200.)
    whererh_5km = np.where(rad_distance_5km < 200.)
    rh700500 = np.array(np.nanmean(rh[:,:,12:20],axis=2)) #verify 3rd dimension indices for 700-500hPa-------------------------
    rh_center.append(np.nanmean(rh700500[whererh_2km]))
    rh_center.append(np.nanmean(rh700500[whererh_5km]))
    rh_p_dsl = np.nanmean(rh_p_rot[:19,:100,12:20])
    rh_p_usl = np.nanmean(rh_p_rot[18:37,:100,12:20])
    rh_p_usr = np.nanmean(rh_p_rot[36:55,:100,12:20])
    rh_p_dsr = np.nanmean(rh_p_rot[54:73,:100,12:20])

    #text output
    structurefile_new = ODIR+'/'+LONGSID.lower()+'.structure_statistics_ptype_rh.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.txt'
    f = open(structurefile_new,'w')
    f.write("%4s, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f,%4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f,%4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f,%4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f,%4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.3f, %4.1f, %4.1f, %4.1f, %4.1f, %4.1f, %4.1f\n" % (FHR,partition_center[0],partition_center[1],partition_center[2],partition_center[3],partition_center[4],partition_center[5],partition_center[6],partition_center[7],partition_center[8],partition_center[9],partition_center[10],partition_center[11],partition_center[12],partition_center[13],partition_center[14],partition_center[15],partition_center[16],partition_center[17],partition_center[18],partition_center[19],partition_center[20],partition_center[21],partition_center[22],partition_center[23],partition_center[24],partition_center[25],partition_center[26],partition_center[27],partition_center[28],partition_center[29],partition_center[30],partition_center[31],partition_center[32],partition_center[33],partition_center[34],partition_center[35],partition_center[36],partition_center[37],partition_center[38],partition_center[39],partition_center[40],partition_center[41],partition_center[42],partition_center[43],partition_center[44],partition_center[45],partition_center[46],partition_center[47],partition_center[48],partition_center[49],rh_center[0],rh_center[1],rh_p_dsl,rh_p_usl,rh_p_usr,rh_p_dsr))
    f.close()

    #Add mass flux and vorticity averaged within 50 km for each vertical level w.r.t. 2km and 5km centers for text output-------------------------------------------
    massflux5km = []
    vorticity5km = []
    massflux2km = []
    vorticity2km = []
    for looph in range(0,np.size(wwind_store[0,0,:])):
        wwindh = wwind_store[:,:,looph]
        rhoh = rho[:,:,looph]
        massflux2km.append(np.nanmean(wwindh[wherearea_2km_50] * rhoh[wherearea_2km_50]))
        massflux5km.append(np.nanmean(wwindh[wherearea_5km_50] * rhoh[wherearea_5km_50]))
        if looph < 37:
           vorth = vort[:,:,looph]
           vorticity2km.append(np.nanmean(vorth[wherearea_2km_50]))
           vorticity5km.append(np.nanmean(vorth[wherearea_5km_50]))

    structurefile_new = ODIR+'/'+LONGSID.lower()+'.structure_statistics_massflux.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.txt'
    f = open(structurefile_new,'w')
    f.write("%4s, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f,  %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f, %4s, % 7.6f, % 7.6f,\n" % (FHR,levs[0,0,0],massflux2km[0],massflux5km[0],levs[0,0,1],massflux2km[1],massflux5km[1],levs[0,0,2], massflux2km[2],massflux5km[2],levs[0,0,3], massflux2km[3],massflux5km[3],levs[0,0,4], massflux2km[4],massflux5km[4],levs[0,0,5], massflux2km[5],massflux5km[5],levs[0,0,6], massflux2km[6],massflux5km[6],levs[0,0,7], massflux2km[7],massflux5km[7],levs[0,0,8], massflux2km[8],massflux5km[8],levs[0,0,9], massflux2km[9],massflux5km[9],levs[0,0,10], massflux2km[10],massflux5km[10],levs[0,0,11], massflux2km[11],massflux5km[11],levs[0,0,12], massflux2km[12],massflux5km[12],levs[0,0,13], massflux2km[13],massflux5km[13],levs[0,0,14], massflux2km[14],massflux5km[14],levs[0,0,15], massflux2km[15],massflux5km[15],levs[0,0,16], massflux2km[16],massflux5km[16],levs[0,0,17], massflux2km[17],massflux5km[17],levs[0,0,18], massflux2km[18],massflux5km[18],levs[0,0,19], massflux2km[19],massflux5km[19],levs[0,0,20], massflux2km[20],massflux5km[20],levs[0,0,21], massflux2km[21],massflux5km[21],levs[0,0,22], massflux2km[22],massflux5km[22],levs[0,0,23], massflux2km[23],massflux5km[24],levs[0,0,24], massflux2km[24],massflux5km[24],levs[0,0,25], massflux2km[25],massflux5km[25],levs[0,0,26], massflux2km[26],massflux5km[26],levs[0,0,27], massflux2km[27],massflux5km[27],levs[0,0,28], massflux2km[28],massflux5km[28],levs[0,0,29], massflux2km[29],massflux5km[29],levs[0,0,30], massflux2km[30],massflux5km[30],levs[0,0,31], massflux2km[31],massflux5km[31],levs[0,0,32], massflux2km[32],massflux5km[32],levs[0,0,33], massflux2km[33],massflux5km[33]))
    f.close()

    structurefile_new = ODIR+'/'+LONGSID.lower()+'.structure_statistics_vorticity.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.txt'
    f = open(structurefile_new,'w')
    f.write("%4s, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f, %4s, % 8.7f, % 8.7f,\n" % (FHR,heightlevs[0],vorticity2km[0],vorticity5km[0],heightlevs[1],vorticity2km[1],vorticity5km[1],heightlevs[2], vorticity2km[2],vorticity5km[2],heightlevs[3], vorticity2km[3],vorticity5km[3],heightlevs[4], vorticity2km[4],vorticity5km[4],heightlevs[5], vorticity2km[5],vorticity5km[5],heightlevs[6], vorticity2km[6],vorticity5km[6],heightlevs[7], vorticity2km[7],vorticity5km[7],heightlevs[8], vorticity2km[8],vorticity5km[8],heightlevs[9], vorticity2km[9],vorticity5km[9],heightlevs[10], vorticity2km[10],vorticity5km[10],heightlevs[11], vorticity2km[11],vorticity5km[11],heightlevs[12], vorticity2km[12],vorticity5km[12],heightlevs[13], vorticity2km[13],vorticity5km[13],heightlevs[14], vorticity2km[14],vorticity5km[14],heightlevs[15], vorticity2km[15],vorticity5km[15],heightlevs[16], vorticity2km[16],vorticity5km[16],heightlevs[17], vorticity2km[17],vorticity5km[17],heightlevs[18], vorticity2km[18],vorticity5km[18],heightlevs[19], vorticity2km[19],vorticity5km[19],heightlevs[20], vorticity2km[20],vorticity5km[20],heightlevs[21], vorticity2km[21],vorticity5km[21],heightlevs[22], vorticity2km[22],vorticity5km[22],heightlevs[23], vorticity2km[23],vorticity5km[24],heightlevs[24], vorticity2km[24],vorticity5km[24],heightlevs[25], vorticity2km[25],vorticity5km[25],heightlevs[26], vorticity2km[26],vorticity5km[26],heightlevs[27], vorticity2km[27],vorticity5km[27],heightlevs[28], vorticity2km[28],vorticity5km[28],heightlevs[29], vorticity2km[29],vorticity5km[29],heightlevs[30], vorticity2km[30],vorticity5km[30],heightlevs[31], vorticity2km[31],vorticity5km[31],heightlevs[32], vorticity2km[32],vorticity5km[32],heightlevs[33], vorticity2km[33],vorticity5km[33]))
    f.close()
    #edit12/23end--------------------------------------------------------------------------------

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

    # F-1.11: Warm core anomaly magnitude / height / radial extent
    warm = _compute_warm_core(temp_p_mean, r, heightlevs)
    r15km_index             = warm['r15km_index']
    r200km_index            = warm['r200km_index']
    r300km_index            = warm['r300km_index']
    temp_p_mean_core_mean   = warm['temp_p_mean_core_mean']
    temp_p_mean_outer_mean  = warm['temp_p_mean_outer_mean']
    temp_p_anomaly          = warm['temp_p_anomaly']
    anomaly_extent          = warm['anomaly_extent']
    temp_p_anomaly_max      = warm['temp_p_anomaly_max']
    temp_anomaly            = warm['temp_anomaly']
    temp_anomaly_max        = warm['temp_anomaly_max']
    height_temp_anomaly_max = warm['height_temp_anomaly_max']

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

      # Local-shear interpolators: query domain is ±102 km from the per-level
      # vortex center; if the per-level center is offset enough that the
      # query falls outside the source slab, return NaN rather than raising.
      _rgi_kw = dict(bounds_error=False, fill_value=np.nan)

      if (center_x_vort[4] < 200 and center_y_vort[4] < 200):
        f_u2km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[4], x_sr+center_x_vort[4]), u2km[:,:], **_rgi_kw)
        u2km_p_local[:,:] = f_u2km((YIlocal,XIlocal),method='linear')
        f_v2km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[4], x_sr+center_x_vort[4]), v2km[:,:], **_rgi_kw)
        v2km_p_local[:,:] = f_v2km((YIlocal,XIlocal),method='linear')

      if (vortex_depth_vort >= 5 and center_x_vort[10] < 200 and center_y_vort[10] < 200):
        f_u5km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[10], x_sr+center_x_vort[10]), u5km[:,:], **_rgi_kw)
        u5km_p_local[:,:] = f_u5km((YIlocal,XIlocal),method='linear')
        f_v5km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[10], x_sr+center_x_vort[10]), v5km[:,:], **_rgi_kw)
        v5km_p_local[:,:] = f_v5km((YIlocal,XIlocal),method='linear')

      if (vortex_depth_vort >= 8 and center_x_vort[16] < 200 and center_y_vort[16] < 200):
        f_u8km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[16], x_sr+center_x_vort[16]), u8km[:,:], **_rgi_kw)
        u8km_p_local[:,:] = f_u8km((YIlocal,XIlocal),method='linear')
        f_v8km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[16], x_sr+center_x_vort[16]), v8km[:,:], **_rgi_kw)
        v8km_p_local[:,:] = f_v8km((YIlocal,XIlocal),method='linear')

      if (vortex_depth_vort >= 10 and center_x_vort[20] < 200 and center_y_vort[20] < 200):
        f_u10km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[20], x_sr+center_x_vort[20]), u10km[:,:], **_rgi_kw)
        u10km_p_local[:,:] = f_u10km((YIlocal,XIlocal),method='linear')
        f_v10km = interpolate.RegularGridInterpolator((y_sr+center_y_vort[20], x_sr+center_x_vort[20]), v10km[:,:], **_rgi_kw)
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
    
    # Lew.Gramer@noaa.gov 2024-01-18,2024-01-23
    sst_100km_annular_avg = np.nanmean(sst[np.where(rad_distance_sfc < 100)])
    sst_100km_annular_std = np.nanstd(sst[np.where(rad_distance_sfc < 100)])
    efx_100km_annular_avg = np.nanmean(lhtflx[np.where(rad_distance_sfc < 100)]+shtflx[np.where(rad_distance_sfc < 100)])
    efx_100km_annular_std = np.nanstd(lhtflx[np.where(rad_distance_sfc < 100)]+shtflx[np.where(rad_distance_sfc < 100)])
    pblz_upp_100km_annular_avg = np.nanmean(pblz_upp[np.where(rad_distance_sfc < 100)])
    pblz_upp_100km_annular_std = np.nanstd(pblz_upp[np.where(rad_distance_sfc < 100)])
    
    structurefile = ODIR+'/'+LONGSID.lower()+'.structure_statistics.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.txt'
    f = open(structurefile,'w')
    f.write("%4s, %4.0f, %5.1f, %5.1f, %4.1f, %4.1f, %5.2f, %5.2f, %4.2f, %4.1f, %4.1f, %5.1f, %4.0f, %5.1f, %4.0f, %5.1f, %4.0f, %5.1f, %4.0f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %3.2f, %4.1f, %4.0f, %4.1f, %4.0f, %4.1f, %4.0f, %4.1f, %5.2f, %4.0f, %5.1f, %4.0f, %5.1f\n" % (FHR,vmax,rmw_2km,rossby,temp_anomaly_max,height_temp_anomaly_max,slope_rmw_1,slope_rmw_2,alpha,vortex_depth_vt_dynamic,vortex_depth_vt_static,tiltmag_mid_pressure,tiltdir_mid_pressure,tiltmag_mid_vort,tiltdir_mid_vort,tiltmag_deep_pressure,tiltdir_deep_pressure,tiltmag_deep_vort,tiltdir_deep_vort,weakpercent_inner,stratiformpercent_inner,shallowpercent_inner,moderatepercent_inner,deeppercent_inner,weakpercent_outer,stratiformpercent_outer,shallowpercent_outer,moderatepercent_outer,deeppercent_outer,closure_stratiform,closure_shallow,closure_moderate,closure_deep,symmetry_w1_dbz5_p,symmetry_all_dbz5_p,symmetry_w1_vt10_p,symmetry_all_vt10_p,shearmag_2km_5km_local,sheardir_2km_5km_local,shearmag_2km_8km_local,sheardir_2km_8km_local,shearmag_2km_10km_local,sheardir_2km_10km_local,sst_100km_annular_avg,sst_100km_annular_std,efx_100km_annular_avg,efx_100km_annular_std,pblz_upp_100km_annular_avg,pblz_upp_100km_annular_std))
    f.close()
    # LJG

    #############################################################################################################
    ###Start of Block of code to do center calculations at each height based on Michael Fischer's methodology
    do_tdr_recentering = namelist_structure_vars[24,1]
    if do_tdr_recentering == 'Y':
      print('MADE IT INSIDE THE RECENTERING LOOP')

      LON,LAT=np.meshgrid(lon,lat)

      allstacks=[]
      indices=[]
      # Center finder runs from 1 km up to the diagnosed vortex top.
      # Plot thinning at line ~2751 keeps every-other entry → markers at
      # 1, 2, 3, ... km on the vort_tilt_aircraft figure.
      index1km = np.argmin(np.abs(heightlevs-1000))
      list_of_levels=np.arange(index1km,ivd+1,1)

      xmin = np.argmin(np.abs(x_sr+200))
      xmax = np.argmin(np.abs(x_sr-200))
      ymin = np.argmin(np.abs(y_sr+200))
      ymax = np.argmin(np.abs(y_sr-200))

      ### MP section (see: https://www.youtube.com/watch?v=fKl2JW_qrso)
      #with concurrent.futures.ProcessPoolExecutor() as executor:
      with concurrent.futures.ThreadPoolExecutor(max_workers=12) as executor:
        results = [executor.submit(MP_centers_function, uwind[ymin:ymax,xmin:xmax,level]-umotion,vwind[ymin:ymax,xmin:xmax,level]-vmotion,LON[ymin:ymax,xmin:xmax],LAT[ymin:ymax,xmin:xmax],center_lon_pressure[level],center_lat_pressure[level], level) for level in list_of_levels]
        for job in concurrent.futures.as_completed(results):
          print(np.shape(job.result))
          (vals,ix) = job.result()
          allstacks.append(vals) #put all these arrays into a big list
          indices.append(ix)

      indices_sorted = np.argsort(np.array(indices))
      allstacks_array = np.array(allstacks)
      allstacks_sorted = allstacks_array[indices_sorted,:]
      newcenter_lat = allstacks_sorted[:,1]
      newcenter_lon = allstacks_sorted[:,0]
      newcenter_vtmax = allstacks_sorted[:,2]
      newcenter_vmax = allstacks_sorted[:,3]
      newcenter_coverage = allstacks_sorted[:,4]

      tiltmag_mid_tdr = tiltdir_mid_tdr = tiltmag_deep_tdr = tiltdir_deep_tdr = np.nan
      index2km = np.argmin(np.abs(heightlevs[list_of_levels]-2000))
      if ivd >= 11:
        index5km = np.argmin(np.abs(heightlevs[list_of_levels]-5000))
        tiltx_mid = (newcenter_lon[index5km]-newcenter_lon[index2km])*111.1*np.cos(centerlat*3.14159/180)
        tilty_mid = (newcenter_lat[index5km]-newcenter_lat[index2km])*111.1
        tiltmag_mid_tdr = np.hypot(tiltx_mid,tilty_mid)
        tiltdir_mid_tdr = np.arctan2(tilty_mid,tiltx_mid)

      if ivd >= 21:
        index10km = np.argmin(np.abs(heightlevs[list_of_levels]-10000))
        tiltx_deep = (newcenter_lon[index10km]-newcenter_lon[index2km])*111.1*np.cos(centerlat*3.14159/180)
        tilty_deep = (newcenter_lat[index10km]-newcenter_lat[index2km])*111.1
        tiltmag_deep_tdr = np.hypot(tiltx_deep,tilty_deep)
        tiltdir_deep_tdr = np.arctan2(tilty_deep,tiltx_deep)

      #Generate Some Variables for Later Plotting
      ms2kt = 1.94384
      deg2rad = pi/180.

      res = 2
      xmin = np.argmin(np.abs(x_sr+200))
      xmax = np.argmin(np.abs(x_sr-200))
      ymin = np.argmin(np.abs(y_sr+200))
      ymax = np.argmin(np.abs(y_sr-200))
      x_sr_200km = x_sr[xmin:xmax]
      y_sr_200km = y_sr[ymin:ymax]
      lon_sr_200km = lon[xmin:xmax]
      lat_sr_200km = lat[ymin:ymax]
      x_sr_200km_interp = np.linspace(-200,200,int(400/res)+1)
      y_sr_200km_interp = np.linspace(-200,200,int(400/res)+1)

      newcenter_xindex = (np.ones(np.shape(newcenter_lon))*np.nan).astype(int)
      newcenter_yindex = (np.ones(np.shape(newcenter_lon))*np.nan).astype(int)
      newcenter_flag = np.zeros(np.shape(newcenter_lon))
      for k in range(np.shape(list_of_levels)[0]):
         newcenter_xindex_orig = np.argmin(np.abs(newcenter_lon[k]-lon_sr_200km)).astype(int)
         newcenter_yindex_orig = np.argmin(np.abs(newcenter_lat[k]-lat_sr_200km)).astype(int)
         if ((newcenter_coverage[k] < 0.2) | (newcenter_vtmax[k] < 8.0)):
          newcenter_flag[k] = 1
         newcenter_xindex[k] = np.argmin(np.abs(x_sr_200km_interp-x_sr_200km[newcenter_xindex_orig])).astype(int)
         newcenter_yindex[k] = np.argmin(np.abs(y_sr_200km_interp-y_sr_200km[newcenter_yindex_orig])).astype(int)

      kmin_tilt = np.min(list_of_levels)
      kmax_tilt = np.max(list_of_levels)

      #Set the Flag to 1 at 0.5km intervals for plotting purposes
      newcenter_flag[1:kmax_tilt-kmin_tilt+1:2] = 1

      SHIPS_ShearMagNum = shearmag*ms2kt
      SHIPS_ShearDirMetNum = sheardir_met

      #Re-grid
      uwind_sr_200km_interp = np.ones((np.shape(y_sr_200km_interp)[0],np.shape(x_sr_200km_interp)[0],zsize))*np.nan
      vwind_sr_200km_interp = np.ones((np.shape(y_sr_200km_interp)[0],np.shape(x_sr_200km_interp)[0],zsize))*np.nan
      wwind_200km_interp = np.ones((np.shape(y_sr_200km_interp)[0],np.shape(x_sr_200km_interp)[0],zsize))*np.nan
      vort_200km_interp = np.ones((np.shape(y_sr_200km_interp)[0],np.shape(x_sr_200km_interp)[0],zsize))*np.nan
      # Legacy interp2d(x, y, z)(x_new, y_new) → output[i,j] is z at
      # (y_new[i], x_new[j]). Build the meshgrid once and feed
      # RegularGridInterpolator with axis order (y, x).
      _Y_q200, _X_q200 = np.meshgrid(y_sr_200km_interp, x_sr_200km_interp, indexing='ij')
      _rgi200_kw = dict(bounds_error=False, fill_value=np.nan)
      for k in range(zsize):
        f_u = interpolate.RegularGridInterpolator((y_sr_200km, x_sr_200km), uwind[ymin:ymax,xmin:xmax,k]-umotion, **_rgi200_kw)
        f_v = interpolate.RegularGridInterpolator((y_sr_200km, x_sr_200km), vwind[ymin:ymax,xmin:xmax,k]-vmotion, **_rgi200_kw)
        f_w = interpolate.RegularGridInterpolator((y_sr_200km, x_sr_200km), wwind[ymin:ymax,xmin:xmax,k], **_rgi200_kw)
        f_z = interpolate.RegularGridInterpolator((y_sr_200km, x_sr_200km), vort[ymin:ymax,xmin:xmax,k], **_rgi200_kw)
        uwind_sr_200km_interp[:,:,k] = f_u((_Y_q200, _X_q200), method='linear')
        vwind_sr_200km_interp[:,:,k] = f_v((_Y_q200, _X_q200), method='linear')
        wwind_200km_interp[:,:,k]    = f_w((_Y_q200, _X_q200), method='linear')
        vort_200km_interp[:,:,k]     = f_z((_Y_q200, _X_q200), method='linear')

      #Now Write Out the Centers to a Text File
      centersfile = ODIR+'/'+LONGSID.lower()+'.centers_by_height.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.txt'
      centersarray = (newcenter_lon,newcenter_lat,heightlevs[list_of_levels])
      np.savetxt(centersfile,np.column_stack(centersarray),fmt = "%.2f",delimiter=',')
      ###End of Block of code to do center calculations at each height based on Michael Fischer's methodology
    #############################################################################################################

    #############################################################################################################################################
    # THIS BLOCK OF CODE WRITES AN OPTIONAL NETCDF FILE (BASED ON A NAMELIST PARAMETER) WITH AZIMUTHAL MEAN VARIABLES
    #############################################################################################################################################
    do_write_netcdf = namelist_structure_vars[23,1]
    if do_write_netcdf == 'Y':
      _write_netcdf(
        ODIR, LONGSID, forecastinit, FHR,
        r, theta, heightlevs, heightlevs_pbl,
        vt_p, ur_p, w_p, dbz_p, q_p, rh_p, temp_p, pressure_p,
        vt_pbl_p, ur_pbl_p,
        rmw_2km, maxwind, minpressure, centerlon, centerlat,
        shearmag, sheardir,
        vortex_depth_vt_dynamic, vortex_depth_vt_static,
        slope_rmw_1, slope_rmw_2, alpha, rossby,
        temp_anomaly_max, height_temp_anomaly_max,
        temp_p_anomaly, anomaly_extent, temp_p_anomaly_max,
        center_lon_pressure=tilt['center_lon_pressure'],
        center_lat_pressure=tilt['center_lat_pressure'],
        center_lon_vort=tilt['center_lon_vort'],
        center_lat_vort=tilt['center_lat_vort'],
      )

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
    do_w5km_wavenumber = namelist_structure_vars[16,1]
    do_vt_tendency = namelist_structure_vars[17,1]
    do_vort_tendency = namelist_structure_vars[18,1]
    do_ur_pbl_p_mean = namelist_structure_vars[19,1]
    do_radar_plots = namelist_structure_vars[20,1]
    do_soundings = namelist_structure_vars[21,1]
    do_shear_and_rh_plots = namelist_structure_vars[22,1]
    # Vorticity figures (indices 25 and 26 in _polar_flag_order; the
    # 23/24 slots hold do_write_netcdf / do_tdr_recentering which are
    # checked elsewhere via the polar_flags dict, not these positional
    # variables, so we skip straight to 25/26 here).
    do_vort_mean              = namelist_structure_vars[25,1]
    do_vort2km_wavenumber     = namelist_structure_vars[26,1]
    do_divergence_mean        = namelist_structure_vars[27,1]
    do_divergence_pbl_mean    = namelist_structure_vars[28,1]
    do_fgr_imbalance_pbl_mean = namelist_structure_vars[29,1]

    if not DO_DBZ:
      do_dbz_mean = 'N'
      do_dbz_alongshear = 'N'
      do_dbz_acrossshear = 'N'
      do_dbz5km_wavenumber = 'N'

    #Do the Sounding Plots First Since Those Call an External Function
    if ( do_soundings == 'Y'):
      # 2-km polar reflectivity slice for the inset reflectivity map
      # drawn in the lower-left of every Skew-T figure. heightlevs is
      # uniform 0-18 km in 37 steps so index 4 = 2000 m exactly.
      skewTmodelTCpolar.skewTmodelTCpolar(r,theta,pressure_p,u_p,v_p,temp_p,rh_p,float(rmwnmi),GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF,
                                          dbz_2km_polar=dbz_p[:, :, 4])

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

    # Symmetric -30 to +30% scale for the W1/W2 panels of the RH
    # wavenumber figure -- the brown/green palette doubles as a
    # diverging colormap centered on zero so dry vs moist anomalies
    # read distinctly. 1 %-pt bins for finer resolution on the small
    # range. Re-uses colormap_rh.
    levs_rh_sym = np.linspace(-30,30,61,endpoint=True)
    norm_rh_sym = colors.BoundaryNorm(levs_rh_sym,256)

    # Symmetric -20 to +20 m/s scale for the W1/W2 panels of the
    # tangential-wind wavenumber figure. Uses matplotlib's seismic
    # diverging palette (blue<->red through white) to highlight the
    # asymmetric components rather than the full-field colormap.
    levs_vt_sym = np.linspace(-20,20,41,endpoint=True)
    norm_vt_sym = colors.BoundaryNorm(levs_vt_sym,256)
    colormap_vt_sym = plt.cm.seismic

    # Symmetric W scales for the 5-km wavenumber figure:
    # full field/WN0: +/-5 m/s, WN1/WN2: +/-2 m/s.
    levs_w_full = np.linspace(-5,5,41,endpoint=True)
    norm_w_full = colors.BoundaryNorm(levs_w_full,256)
    levs_w_sym = np.linspace(-2,2,41,endpoint=True)
    norm_w_sym = colors.BoundaryNorm(levs_w_sym,256)
    colormap_w_sym = plt.cm.seismic


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

    # Plot display radius (km). Data still extend to the namelist `rmax`
    # (typically 600 km), but most plots zoom to this inner ring for legibility.
    # Radial-height plots use ticks every 25 km; wavenumber plots use every 50 km.
    rmax_plot = 200.0

    # FIGURE 1: Azimuthal Mean Radial Wind
    if do_ur_mean == 'Y':
      fig1 = plt.figure(figsize=(20.5, 10.5))
      ax1 = fig1.add_subplot(1, 1, 1)
      co1 = ax1.contourf(r, heightlevs/1000, np.flipud(np.rot90(ur_p_mean,1)), levs_ur, \
            cmap=colormap_ur, norm=norm_ur, extend='both')
      ax1 = plotting.axes_radhgt(ax1, xmax=rmax_plot, nx=9)
      cbar1 = plt.colorbar(co1, ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
      cbar1.ax.tick_params(labelsize=24)
      ax1.set_title(f'{EXPT_TITLE.strip()}\n' + \
              r'Azimuthal Mean Radial Wind ($m\ s^{-1}$, Shading)' + \
              f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
              fontsize=24, weight='bold', loc='left')
      ax1.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.ur_mean.{forecastinit}.polar.f{FHR:03}'
      fig1.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig1.clf()
      plt.close(fig1)


    # FIGURE 2: Azimuthal Mean Tangential Wind
    if do_vt_mean == 'Y':
      fig2 = plt.figure(figsize=(20.5, 10.5))
      ax2 = fig2.add_subplot(1, 1, 1)
      co2 = ax2.contourf(r, heightlevs/1000, np.flipud(np.rot90(vt_p_mean, 1)), levs_vt, \
             cmap=colormap_vt, norm=norm_vt, extend='max')
      ax2 = plotting.axes_radhgt(ax2, xmax=rmax_plot, nx=9)
      cbar2 = plt.colorbar(co2, ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80])
      cbar2.ax.tick_params(labelsize=24)
      ax2.set_title(f'{EXPT_TITLE.strip()}\n' + \
              r'Azimuthal Mean Tangential Wind ($m\ s^{-1}$, Shading)' + \
              f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
              fontsize=24, weight='bold', loc='left')
      ax2.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.vt_mean.{forecastinit}.polar.f{FHR:03}'
      fig2.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig2.clf()
      plt.close(fig2)


    # FIGURE 3: Azimuthal Mean Vertical Velocity
    if do_w_mean == 'Y':
      fig3 = plt.figure(figsize=(20.5, 10.5))
      ax3 = fig3.add_subplot(1, 1, 1)
      co3 = ax3.contourf(r, heightlevs/1000, np.flipud(np.rot90(w_p_mean, 1)), levs_w, \
             cmap=colormap_w, norm=norm_w, extend='both')
      ax3 = plotting.axes_radhgt(ax3, xmax=rmax_plot, nx=9)
      cbar3 = plt.colorbar(co3, ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
      cbar3.ax.tick_params(labelsize=24)
      ax3.set_title(f'{EXPT_TITLE.strip()}\n' + \
              r'Azimuthal Mean W ($m\ s^{-1}$, Shading)' + \
              f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
              fontsize=24, weight='bold', loc='left')
      ax3.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.w_mean.{forecastinit}.polar.f{FHR:03}'
      fig3.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig3.clf()
      plt.close(fig3)


    # FIGURE 4: Azimuthal Mean Reflectivity
    if do_dbz_mean == 'Y':
      fig4 = plt.figure(figsize=(20.5, 10.5))
      ax4 = fig4.add_subplot(1, 1, 1)
      co4 = ax4.contourf(r, heightlevs/1000, np.flipud(np.rot90(dbz_p_mean, 1)), levs_dbz, \
             cmap=colormap_dbz, norm=norm_dbz, extend='max')
      ax4 = plotting.axes_radhgt(ax4, xmax=rmax_plot, nx=9)
      cbar4 = plt.colorbar(co4, ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
      cbar4.ax.tick_params(labelsize=24)
      ax4.set_title(f'{EXPT_TITLE.strip()}\n' + \
              r'Azimuthal Mean Reflectivity ($dBZ$, Shading)' + \
              f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
              fontsize=24, weight='bold', loc='left')
      ax4.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.dbz_mean.{forecastinit}.polar.f{FHR:03}'
      fig4.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig4.clf()
      plt.close(fig4)


    # FIGURE 4b: Azimuthal Mean Relative Vorticity (radial-height)
    # Displays vorticity in units of 10^-4 s^-1 (typical TC eyewall peaks
    # at 50-200 in these units). Pure white->red Reds palette since
    # cyclonic relative vorticity in TCs is overwhelmingly positive --
    # the diagnostic value is in the magnitude of the inner-core spinup
    # column, not in sign distinctions, and the same Reds palette is
    # used by the wavenumber Full Field / W0 panels (FIGURE 14b) so the
    # mean column and the 2-km plan view stay visually paired.
    if do_vort_mean == 'Y':
      fig4b = plt.figure(figsize=(20.5, 10.5))
      ax4b = fig4b.add_subplot(1, 1, 1)
      # 0-80 covers a Cat-4 / Cat-5 eyewall column without forcing the
      # palette to wash out the diagnostic eyewall ring on a typical
      # Cat-2 / Cat-3 cyclone. Values above 80 saturate via extend='max'.
      levs_vort_mean = np.arange(0, 85, 5)
      norm_vort_mean = colors.BoundaryNorm(levs_vort_mean, 256)
      co4b = ax4b.contourf(r, heightlevs/1000,
                           np.flipud(np.rot90(vort_p_mean * 1e4, 1)),
                           levs_vort_mean,
                           cmap=plt.cm.Reds, norm=norm_vort_mean,
                           extend='max')
      ax4b = plotting.axes_radhgt(ax4b, xmax=rmax_plot, nx=9)
      cbar4b = plt.colorbar(co4b, ticks=np.arange(0, 90, 10))
      cbar4b.ax.tick_params(labelsize=24)
      ax4b.set_title(f'{EXPT_TITLE.strip()}\n' +
              r'Azimuthal Mean Relative Vorticity ($10^{-4}\ s^{-1}$, Shading)' +
              f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]',
              fontsize=24, weight='bold', loc='left')
      ax4b.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.vort_mean.{forecastinit}.polar.f{FHR:03}'
      fig4b.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig4b.clf()
      plt.close(fig4b)


    # FIGURE 4c: Azimuthal Mean Radial Divergence (radial-height, 0-18 km)
    # Plots d(ur)/dr in units of 10^-3 s^-1. Positive = divergence
    # (outflow accelerating), negative = convergence (inflow
    # accelerating); the eyewall convergence signature is the
    # diagnostic of interest, hence the diverging bluewhitered palette.
    # Note this is the radial-derivative term only, not the full
    # cylindrical divergence (1/r * d(r*ur)/dr = dur/dr + ur/r); the
    # ur/r term peaks near the storm center where the polar grid is
    # noisy anyway, and the historical NCL/legacy GPLOT convention
    # here was the same dur/dr form.
    if do_divergence_mean == 'Y':
      color_data_divergence = np.genfromtxt(
          f'{PYTHONDIR}/colormaps/bluewhitered.txt')
      colormap_divergence = matplotlib.colors.ListedColormap(
          color_data_divergence)
      levs_divergence = np.linspace(-3, 3, 61)
      norm_divergence = colors.BoundaryNorm(levs_divergence, 256)
      fig4c = plt.figure(figsize=(20.5, 10.5))
      ax4c = fig4c.add_subplot(1, 1, 1)
      co4c = ax4c.contourf(r, heightlevs/1000,
                           np.flipud(np.rot90(dur_dr * 1e3, 1)),
                           levs_divergence,
                           cmap=colormap_divergence, norm=norm_divergence,
                           extend='both')
      ax4c = plotting.axes_radhgt(ax4c, xmax=rmax_plot, nx=9)
      cbar4c = plt.colorbar(co4c, ticks=[-3.0, -2.5, -2.0, -1.5, -1.0,
                                          -0.5, 0, 0.5, 1.0, 1.5,
                                          2.0, 2.5, 3.0])
      cbar4c.ax.tick_params(labelsize=24)
      ax4c.set_title(f'{EXPT_TITLE.strip()}\n' +
              r'Radial Divergence ($10^{-3}\ s^{-1}$, Shading)' +
              f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]',
              fontsize=24, weight='bold', loc='left')
      ax4c.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.divergence_mean.{forecastinit}.polar.f{FHR:03}'
      fig4c.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig4c.clf()
      plt.close(fig4c)


    # FIGURE 4d: Azimuthal Mean Radial Divergence (PBL, 0-3 km)
    # Same field as FIGURE 4c but on the finer PBL height grid so the
    # boundary-layer convergence is resolved. RMW track overlaid as
    # black dots at each PBL level.
    if do_divergence_pbl_mean == 'Y':
      color_data_divergence = np.genfromtxt(
          f'{PYTHONDIR}/colormaps/bluewhitered.txt')
      colormap_divergence = matplotlib.colors.ListedColormap(
          color_data_divergence)
      levs_divergence = np.linspace(-3, 3, 61)
      norm_divergence = colors.BoundaryNorm(levs_divergence, 256)
      fig4d = plt.figure(figsize=(20.5, 10.5))
      ax4d = fig4d.add_subplot(1, 1, 1)
      co4d = ax4d.contourf(r, heightlevs_pbl,
                           np.flipud(np.rot90(dur_dr_pbl * 1e3, 1)),
                           levs_divergence,
                           cmap=colormap_divergence, norm=norm_divergence,
                           extend='both')
      ax4d = plotting.axes_radhgt(ax4d, xmax=rmax_plot, nx=9,
                                  ymax=3000, ny=7, yunit='m',
                                  formatters=True)
      cbar4d = plt.colorbar(co4d, ticks=[-3.0, -2.5, -2.0, -1.5, -1.0,
                                          -0.5, 0, 0.5, 1.0, 1.5,
                                          2.0, 2.5, 3.0])
      cbar4d.ax.tick_params(labelsize=24)
      ax4d.scatter(rmw_pbl_mean, heightlevs_pbl, 70, 'k')
      ax4d.set_title(f'{EXPT_TITLE.strip()}\n' +
              r'Radial Divergence ($10^{-3}\ s^{-1}$, Shading; PBL)' +
              f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]',
              fontsize=24, weight='bold', loc='left')
      ax4d.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.divergence_pbl_mean.{forecastinit}.polar.f{FHR:03}'
      fig4d.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig4d.clf()
      plt.close(fig4d)


    # FIGURE 4e: PBL Gradient-Wind Imbalance ("term_b")
    # Radial momentum residual -1/rho * dp/dr + Vt^2/r + f*Vt plotted
    # in m s^-1 h^-1 (multiplied by 3600). In gradient-wind balance
    # the three terms sum to zero; positive => sub-gradient flow
    # (the pressure gradient over-powers the centripetal + Coriolis
    # forces, surface inflow accelerates inward); negative =>
    # super-gradient (centripetal dominates, outward acceleration).
    # The classic TC eyewall signature is a super-gradient core
    # surrounded by a sub-gradient annulus where the surface inflow
    # is decelerating into the eyewall updraft. Black RMW track
    # overlaid; Vt contoured in cyan; Ur contoured in black (outflow
    # dashed, inflow solid).
    if do_fgr_imbalance_pbl_mean == 'Y':
      color_data_term_b = np.genfromtxt(
          f'{PYTHONDIR}/colormaps/bluewhitered.txt')
      colormap_term_b = matplotlib.colors.ListedColormap(color_data_term_b)
      levs_term_b = np.linspace(-250, 250, 51)
      norm_term_b = colors.BoundaryNorm(levs_term_b, 256)
      fig4e = plt.figure(figsize=(20.5, 10.5))
      ax4e = fig4e.add_subplot(1, 1, 1)
      co4e = ax4e.contourf(r, heightlevs_pbl,
                           np.flipud(np.rot90(3600 * term_b, 1)),
                           levs_term_b, cmap=colormap_term_b,
                           norm=norm_term_b, extend='both')
      ax4e = plotting.axes_radhgt(ax4e, xmax=rmax_plot, nx=9,
                                  ymax=3000, ny=7, yunit='m',
                                  formatters=True)
      cbar4e = plt.colorbar(co4e, ticks=[-250, -200, -150, -100, -50,
                                          0, 50, 100, 150, 200, 250])
      cbar4e.ax.tick_params(labelsize=24)
      co4e_vt = ax4e.contour(r, heightlevs_pbl,
              np.flipud(np.rot90(vt_pbl_p_mean)),
              levels=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90],
              linestyles='solid', colors='xkcd:cyan', linewidths=4)
      ax4e.clabel(co4e_vt, co4e_vt.levels, inline=True,
                  fmt='%2.0f', fontsize=20)
      co4e_ur_in = ax4e.contour(r, heightlevs_pbl,
              np.flipud(np.rot90(ur_pbl_p_mean)),
              levels=[-25, -20, -15, -10, -5],
              linestyles='solid', colors='black',
              linewidths=[5, 4, 3, 2, 1])
      ax4e.clabel(co4e_ur_in, co4e_ur_in.levels, inline=True,
                  fmt='%2.0f', fontsize=20)
      co4e_ur_out = ax4e.contour(r, heightlevs_pbl,
              np.flipud(np.rot90(ur_pbl_p_mean)),
              levels=[1, 2, 3, 4, 5],
              linestyles='dashed', colors='black',
              linewidths=[1, 2, 3, 4, 5])
      ax4e.clabel(co4e_ur_out, co4e_ur_out.levels, inline=True,
                  fmt='%2.0f', fontsize=20)
      ax4e.scatter(rmw_pbl_mean, heightlevs_pbl, 70, 'k')
      ax4e.set_title(f'{EXPT_TITLE.strip()}\n' +
              r'$F_{gr}$ Imbalance ($m\ s^{-1}\ h^{-1}$, Shading)' +
              '\n' + r'$V_{t}$ and $U_{r}$ (Contours)' +
              f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]',
              fontsize=24, weight='bold', loc='left')
      ax4e.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.fgr_imbalance_pbl_mean.{forecastinit}.polar.f{FHR:03}'
      fig4e.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig4e.clf()
      plt.close(fig4e)


    # FIGURE 5: Azimuthal Mean Relative Humidity
    if do_rh_mean == 'Y':
      fig5 = plt.figure(figsize=(20.5, 10.5))
      ax5 = fig5.add_subplot(1, 1, 1)
      co5 = ax5.contourf(r, heightlevs/1000, np.flipud(np.rot90(rh_p_mean, 1)), levs_rh, \
             cmap=colormap_rh, norm=norm_rh, extend='max')
      ax5 = plotting.axes_radhgt(ax5, xmax=rmax_plot, nx=9)
      cbar5 = plt.colorbar(co5, ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
      cbar5.ax.tick_params(labelsize=24)
      ax5.set_title(f'{EXPT_TITLE.strip()}\n' + \
              r'Azimuthal Mean Relative Humidity ($\%$, Shading)' + \
              f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
              fontsize=24, weight='bold', loc='left')
      ax5.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.rh_mean.{forecastinit}.polar.f{FHR:03}'
      fig5.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig5.clf()
      plt.close(fig5)


    # FIGURE 6: Along-Shear Reflectivity
    if do_dbz_alongshear == 'Y':
      fig6 = plt.figure(figsize=(20.5, 10.5))
      ax6 = fig6.add_subplot(1, 1, 1)
      co6 = ax6.contourf(r, heightlevs/1000, np.flipud(np.rot90(dbz_p_downshear_mean, 1)), levs_dbz, \
             cmap=colormap_dbz, norm=norm_dbz, extend='max')
      ax6.contourf(-r, heightlevs/1000, np.flipud(np.rot90(dbz_p_upshear_mean, 1)), levs_dbz, \
             cmap=colormap_dbz, norm=norm_dbz, extend='max')
      ax6 = plotting.axes_radhgt(ax6, xmax=rmax_plot, xmin=-rmax_plot, nx=9)
      cbar6 = plt.colorbar(co6, ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
      cbar6.ax.tick_params(labelsize=24)
      ax6.text(-rmax_plot+0.05*(2*rmax_plot), 18-(0.05*18), 'Upshear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
      ax6.text(rmax_plot-0.05*(2*rmax_plot), 18-(0.05*18), 'Downshear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
      ax6.set_title(f'{EXPT_TITLE.strip()}\n' + \
              r'Along-Shear Reflectivity ($dBZ$, Shading)' + \
              f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
              fontsize=24, weight='bold', loc='left')
      ax6.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.dbz_alongshear.{forecastinit}.polar.f{FHR:03}'
      fig6.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig6.clf()
      plt.close(fig6)


    # FIGURE 7: Along-Shear Radial Wind
    if do_ur_alongshear == 'Y':
      fig7 = plt.figure(figsize=(20.5, 10.5))
      ax7 = fig7.add_subplot(1, 1, 1)
      co7 = ax7.contourf(r, heightlevs/1000, np.flipud(np.rot90(ur_p_downshear_mean, 1)), levs_ur, \
             cmap=colormap_ur, norm=norm_ur, extend='both')
      ax7.contourf(-r,heightlevs/1000,np.flipud(np.rot90(ur_p_upshear_mean,1)),levs_ur,cmap=colormap_ur,norm=norm_ur,extend='both')
      ax7 = plotting.axes_radhgt(ax7, xmax=rmax_plot, xmin=-rmax_plot, nx=9)
      cbar7 = plt.colorbar(co7, ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
      cbar7.ax.tick_params(labelsize=24)
      ax7.text(-rmax_plot+0.05*(2*rmax_plot), 18-(0.05*18), 'Upshear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
      ax7.text(rmax_plot-0.05*(2*rmax_plot), 18-(0.05*18), 'Downshear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
      ax7.set_title(f'{EXPT_TITLE.strip()}\n' + \
              r'Along-Shear Radial Wind ($m\ s^{-1}$, Shading)' + \
              f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
              fontsize=24, weight='bold', loc='left')
      ax7.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.ur_alongshear.{forecastinit}.polar.f{FHR:03}'
      fig7.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig7.clf()
      plt.close(fig7)


    # FIGURE 8: Along-Shear Vertical Velocity
    if do_w_alongshear == 'Y':
      fig8 = plt.figure(figsize=(20.5, 10.5))
      ax8 = fig8.add_subplot(1, 1, 1)
      co8 = ax8.contourf(r, heightlevs/1000, np.flipud(np.rot90(w_p_downshear_mean, 1)), levs_w, \
             cmap=colormap_w, norm=norm_w, extend='both')
      ax8.contourf(-r, heightlevs/1000, np.flipud(np.rot90(w_p_upshear_mean, 1)), levs_w, \
             cmap=colormap_w, norm=norm_w, extend='both')
      ax8 = plotting.axes_radhgt(ax8, xmax=rmax_plot, xmin=-rmax_plot, nx=9)
      cbar8 = plt.colorbar(co8, ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
      cbar8.ax.tick_params(labelsize=24)
      ax8.text(-rmax_plot+0.05*(2*rmax_plot), 18-(0.05*18), 'Upshear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
      ax8.text(rmax_plot-0.05*(2*rmax_plot), 18-(0.05*18), 'Downshear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
      ax8.set_title(f'{EXPT_TITLE.strip()}\n' + \
              r'Along-Shear W ($m\ s^{-1}$, Shading)' + \
              f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
              fontsize=24, weight='bold', loc='left')
      ax8.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.w_alongshear.{forecastinit}.polar.f{FHR:03}'
      fig8.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig8.clf()
      plt.close(fig8)


    # FIGURE 9: Along-Shear Relative Humidity
    if do_rh_alongshear == 'Y':
      fig9 = plt.figure(figsize=(20.5, 10.5))
      ax9 = fig9.add_subplot(1, 1, 1)
      co9 = ax9.contourf(r, heightlevs/1000, np.flipud(np.rot90(rh_p_downshear_mean, 1)), levs_rh, \
             cmap=colormap_rh, norm=norm_rh, extend='both')
      ax9.contourf(-r, heightlevs/1000, np.flipud(np.rot90(rh_p_upshear_mean, 1)), levs_rh, \
             cmap=colormap_rh, norm=norm_rh, extend='both')
      ax9 = plotting.axes_radhgt(ax9, xmax=rmax_plot, xmin=-rmax_plot, nx=9)
      cbar9 = plt.colorbar(co9, ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
      cbar9.ax.tick_params(labelsize=24)
      ax9.text(-rmax_plot+0.05*(2*rmax_plot), 18-(0.05*18), 'Upshear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
      ax9.text(rmax_plot-0.05*(2*rmax_plot), 18-(0.05*18), 'Downshear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
      ax9.set_title(f'{EXPT_TITLE.strip()}\n' + \
              r'Along-Shear RH ($\%$, Shading)' + \
              f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
              fontsize=24, weight='bold', loc='left')
      ax9.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.rh_alongshear.{forecastinit}.polar.f{FHR:03}'
      fig9.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig9.clf()
      plt.close(fig9)


    # FIGURE 10: Across-Shear Reflectivity
    if do_dbz_acrossshear == 'Y':
      fig10 = plt.figure(figsize=(20.5, 10.5))
      ax10 = fig10.add_subplot(1, 1, 1)
      co10 = ax10.contourf(r, heightlevs/1000, np.flipud(np.rot90(dbz_p_rightshear_mean, 1)), levs_dbz, \
             cmap=colormap_dbz, norm=norm_dbz, extend='both')
      ax10.contourf(-r, heightlevs/1000, np.flipud(np.rot90(dbz_p_leftshear_mean, 1)), levs_dbz, \
             cmap=colormap_dbz, norm=norm_dbz, extend='both')
      ax10 = plotting.axes_radhgt(ax10, xmax=rmax_plot, xmin=-rmax_plot, nx=9)
      cbar10 = plt.colorbar(co10, ticks=[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75])
      cbar10.ax.tick_params(labelsize=24)
      ax10.text(-rmax_plot+0.05*(2*rmax_plot), 18-(0.05*18), 'Left of shear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
      ax10.text(rmax_plot-0.05*(2*rmax_plot), 18-(0.05*18), 'Right of shear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
      ax10.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'Across-Shear Reflectivity ($dBZ$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax10.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.rh_acrossshear.{forecastinit}.polar.f{FHR:03}'
      fig10.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig10.clf()
      plt.close(fig10)


    # FIGURE 11: Across-Shear Radiaul Wind
    if do_ur_acrossshear == 'Y':
      fig11 = plt.figure(figsize=(20.5, 10.5))
      ax11 = fig11.add_subplot(1, 1, 1)
      co11 = ax11.contourf(r, heightlevs/1000, np.flipud(np.rot90(ur_p_rightshear_mean, 1)), levs_ur, \
             cmap=colormap_ur, norm=norm_ur, extend='both')
      ax11.contourf(-r, heightlevs/1000, np.flipud(np.rot90(ur_p_leftshear_mean, 1)), levs_ur, \
             cmap=colormap_ur, norm=norm_ur, extend='both')
      ax11 = plotting.axes_radhgt(ax11, xmax=rmax_plot, xmin=-rmax_plot, nx=9)
      cbar11 = plt.colorbar(co11, ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
      cbar11.ax.tick_params(labelsize=24)
      ax11.text(-rmax_plot+0.05*(2*rmax_plot), 18-(0.05*18), 'Left of shear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
      ax11.text(rmax_plot-0.05*(2*rmax_plot), 18-(0.05*18), 'Right of shear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
      ax11.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'Across-Shear Radial Wind ($m\ s^{-1}$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax11.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.ur_acrossshear.{forecastinit}.polar.f{FHR:03}'
      fig11.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig11.clf()
      plt.close(fig11)


    # FIGURE 12: Across-Shear Vertical Velocity
    if do_w_acrossshear == 'Y':
      fig12 = plt.figure(figsize=(20.5, 10.5))
      ax12 = fig12.add_subplot(1, 1, 1)
      co12 = ax12.contourf(r, heightlevs/1000, np.flipud(np.rot90(w_p_rightshear_mean, 1)), levs_w, \
             cmap=colormap_w, norm=norm_w, extend='both')
      ax12.contourf(-r, heightlevs/1000, np.flipud(np.rot90(w_p_leftshear_mean, 1)), levs_w, \
             cmap=colormap_w, norm=norm_w, extend='both')
      ax12 = plotting.axes_radhgt(ax12, xmax=rmax_plot, xmin=-rmax_plot, nx=9)
      cbar12 = plt.colorbar(co12, ticks=[-5, -4, -3, -2, -1, 1, 2, 3, 4, 5])
      cbar12.ax.tick_params(labelsize=24)
      ax12.text(-rmax_plot+0.05*(2*rmax_plot), 18-(0.05*18), 'Left of shear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
      ax12.text(rmax_plot-0.05*(2*rmax_plot), 18-(0.05*18), 'Right of shear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
      ax12.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'Across-Shear W ($m\ s^{-1}$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax12.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.w_acrossshear.{forecastinit}.polar.f{FHR:03}'
      fig12.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig12.clf()
      plt.close(fig12)


    # FIGURE 13: Across-Shear Relative Humidity
    if do_rh_acrossshear == 'Y':
      fig13 = plt.figure(figsize=(20.5, 10.5))
      ax13 = fig13.add_subplot(1, 1, 1)
      co13 = ax13.contourf(r, heightlevs/1000, np.flipud(np.rot90(rh_p_rightshear_mean, 1)), levs_rh, \
             cmap=colormap_rh, norm=norm_rh, extend='both')
      ax13.contourf(-r, heightlevs/1000, np.flipud(np.rot90(rh_p_leftshear_mean, 1)), levs_rh, \
             cmap=colormap_rh, norm=norm_rh, extend='both')
      ax13 = plotting.axes_radhgt(ax13, xmax=rmax_plot, xmin=-rmax_plot, nx=9)
      cbar13 = plt.colorbar(co13, ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100])
      cbar13.ax.tick_params(labelsize=24)
      ax13.text(-rmax_plot+0.05*(2*rmax_plot), 18-(0.05*18), 'Left of shear', fontsize=22, horizontalalignment='left', style='italic', weight='bold')
      ax13.text(rmax_plot-0.05*(2*rmax_plot), 18-(0.05*18), 'Right of shear', fontsize=22, horizontalalignment='right', style='italic', weight='bold')
      ax13.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'Across-Shear RH ($\%$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax13.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.rh_acrossshear.{forecastinit}.polar.f{FHR:03}'
      fig13.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig13.clf()
      plt.close(fig13)


    # FIGURE 14: Wavenumber 0,1,2 components of 5-km Reflectivity
    if do_dbz5km_wavenumber == 'Y':
      fig14 = plt.figure(figsize=(15,15))
      ticks14 = [0, 10, 20, 30, 40, 50, 60, 70]

      # Panel A
      ax14a = fig14.add_subplot(2, 2, 1)
      co14a = ax14a.contourf(XI, YI, dbz5_p[:,:], levs_dbz, \
            cmap=colormap_dbz, norm=norm_dbz, extend='max')
      ax14a = plotting.axes_wavenumber(ax14a, rmax_plot, -rmax_plot, nx=9)
      cbar14a = plt.colorbar(co14a, ticks=ticks14)
      cbar14a.ax.tick_params(labelsize=18)
      ax14a.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax14a.set_title(f'{EXPT_TITLE.strip()}\n' + \
          r'WV#0,1,2 5-km Reflectivity ($dBZ$, Shading)' + \
          f'\nShear Vector in Black\nInit: {forecastinit}\nForecast Hour:[{FHR:03}]', \
          fontsize=20, weight='bold', loc='left')
      ax14a.text(0,rmax_plot-25,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

      # Panel B
      ax14b = fig14.add_subplot(2, 2, 2)
      co14b = ax14b.contourf(XI, YI, dbz5_p_w0[:,:], levs_dbz, \
            cmap=colormap_dbz, norm=norm_dbz, extend='max')
      ax14b = plotting.axes_wavenumber(ax14b, rmax_plot, -rmax_plot, nx=9)
      cbar14b = plt.colorbar(co14b, ticks=ticks14)
      cbar14b.ax.tick_params(labelsize=18)
      ax14b.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax14b.set_title(f'{LONGSID.upper()}\nVMAX= {maxwind} kt\nPMIN= {minpressure} hPa' + \
          f'\nShear Magnitude= {str(int(np.round(shearmag*1.94,0)))}kts\nShear Direction= {str(int(np.round(sheardir_met,0)))}$^\circ$', \
          fontsize=20, color='brown', loc='right')
      ax14b.text(0,rmax_plot-25,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

      # Panel C
      ax14c = fig14.add_subplot(2, 2, 3)
      co14c = ax14c.contourf(XI, YI, dbz5_p_w1[:,:], levs_dbz, \
            cmap=colormap_dbz, norm=norm_dbz, extend='max')
      ax14c = plotting.axes_wavenumber(ax14c, rmax_plot, -rmax_plot, nx=9)
      cbar14c = plt.colorbar(co14c, ticks=ticks14)
      cbar14c.ax.tick_params(labelsize=18)
      ax14c.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax14c.text(0,rmax_plot-25,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

      # Panel D: Wavenumber 2 + all higher wavenumbers, summed.
      # whigher already includes h=2 (the FFT loop in _wavenumber_decomp
      # iterates `for h in range(2, ...)` -- not range(3, ...) -- so
      # the variable name "whigher" is a bit of a misnomer; it really
      # is "W2 and higher" = W2 + W3 + ... + W(N/2-1)). Showing W2+ in
      # this panel captures the eyewall-mode asymmetries together with
      # the small-scale wavenumber chatter inside the eyewall that the
      # bare W2 panel misses.
      ax14d = fig14.add_subplot(2, 2, 4)
      co14d = ax14d.contourf(XI, YI, dbz5_p_whigher[:,:], levs_dbz, \
            cmap=colormap_dbz, norm=norm_dbz, extend='max')
      ax14d = plotting.axes_wavenumber(ax14d, rmax_plot, -rmax_plot, nx=9)
      cbar14d = plt.colorbar(co14d, ticks=ticks14)
      cbar14d.ax.tick_params(labelsize=18)
      ax14d.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax14d.text(0,rmax_plot-25,'Wavenumber 2+',fontsize=20,style='italic',horizontalalignment='center')

      # Finalize figure
      figfname = f'{ODIR}/{LONGSID.lower()}.dbz5km_wavenumber.{forecastinit}.polar.f{FHR:03}'
      #fig14.tight_layout()
      fig14.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      fig14.clf()
      plt.close(fig14)
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')


    # FIGURE 14b: Wavenumber 0,1,2 components of 2-km Relative Vorticity.
    # Full field and W0 use the pure white->red Reds palette on a 0..80
    # (10^-4 s^-1) scale -- a cleaner symmetric-field look than the
    # white->yellow->red YlOrRd ramp because the eyewall ring stands
    # out as a single red band against a white background instead of
    # competing with a yellow shoulder. W1 / W2 anomaly panels use
    # seismic on +/-25 -- the same diverging blue->white->red treatment
    # we use for Vt W1/W2 (FIGURE 16), tightened to +/-25 so the small
    # inner-core asymmetries on near-axisymmetric storms still register
    # visibly while strongly sheared storms saturate cleanly at the
    # ends. Shear vector is drawn on every panel for orientation,
    # mirroring the dbz5km / rh5km wavenumber figures.
    if do_vort2km_wavenumber == 'Y':
      fig14b = plt.figure(figsize=(15,15))
      levs_vort_pos  = np.arange(0, 85, 5)
      norm_vort_pos  = colors.BoundaryNorm(levs_vort_pos, 256)
      ticks14b_pos   = [0, 20, 40, 60, 80]
      levs_vort_sym  = np.arange(-25, 26, 1)
      norm_vort_sym  = colors.BoundaryNorm(levs_vort_sym, 256)
      ticks14b_sym   = [-25, -15, -5, 5, 15, 25]

      vort2_p_disp         = vort2_p * 1e4
      vort2_p_w0_disp      = vort2_p_w0 * 1e4
      vort2_p_w1_disp      = vort2_p_w1 * 1e4
      vort2_p_whigher_disp = vort2_p_whigher * 1e4

      # Panel A: Full Field
      ax14ba = fig14b.add_subplot(2, 2, 1)
      co14ba = ax14ba.contourf(XI, YI, vort2_p_disp, levs_vort_pos,
            cmap=plt.cm.Reds, norm=norm_vort_pos, extend='max')
      ax14ba = plotting.axes_wavenumber(ax14ba, rmax_plot, -rmax_plot, nx=9)
      cbar14ba = plt.colorbar(co14ba, ticks=ticks14b_pos)
      cbar14ba.ax.tick_params(labelsize=18)
      ax14ba.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2),
          linewidth=3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax14ba.set_title(f'{EXPT_TITLE.strip()}\n' +
          r'WV#0,1,2 2-km Relative Vorticity ($10^{-4}\ s^{-1}$, Shading)' +
          f'\nShear Vector in Black\nInit: {forecastinit}\nForecast Hour:[{FHR:03}]',
          fontsize=20, weight='bold', loc='left')
      ax14ba.text(0,rmax_plot-25,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

      # Panel B: Wavenumber 0
      ax14bb = fig14b.add_subplot(2, 2, 2)
      co14bb = ax14bb.contourf(XI, YI, vort2_p_w0_disp, levs_vort_pos,
            cmap=plt.cm.Reds, norm=norm_vort_pos, extend='max')
      ax14bb = plotting.axes_wavenumber(ax14bb, rmax_plot, -rmax_plot, nx=9)
      cbar14bb = plt.colorbar(co14bb, ticks=ticks14b_pos)
      cbar14bb.ax.tick_params(labelsize=18)
      ax14bb.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2),
          linewidth=3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax14bb.set_title(f'{LONGSID.upper()}\nVMAX= {maxwind} kt\nPMIN= {minpressure} hPa' +
          f'\nShear Magnitude= {str(int(np.round(shearmag*1.94,0)))}kts\nShear Direction= {str(int(np.round(sheardir_met,0)))}$^\\circ$',
          fontsize=20, color='brown', loc='right')
      ax14bb.text(0,rmax_plot-25,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

      # Panel C: Wavenumber 1 anomaly (+/- 50 on seismic)
      ax14bc = fig14b.add_subplot(2, 2, 3)
      co14bc = ax14bc.contourf(XI, YI, vort2_p_w1_disp, levs_vort_sym,
            cmap=plt.cm.seismic, norm=norm_vort_sym, extend='both')
      ax14bc = plotting.axes_wavenumber(ax14bc, rmax_plot, -rmax_plot, nx=9)
      cbar14bc = plt.colorbar(co14bc, ticks=ticks14b_sym)
      cbar14bc.ax.tick_params(labelsize=18)
      ax14bc.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2),
          linewidth=3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax14bc.text(0,rmax_plot-25,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

      # Panel D: Wavenumber 2+ (W2 plus all higher modes summed)
      ax14bd = fig14b.add_subplot(2, 2, 4)
      co14bd = ax14bd.contourf(XI, YI, vort2_p_whigher_disp, levs_vort_sym,
            cmap=plt.cm.seismic, norm=norm_vort_sym, extend='both')
      ax14bd = plotting.axes_wavenumber(ax14bd, rmax_plot, -rmax_plot, nx=9)
      cbar14bd = plt.colorbar(co14bd, ticks=ticks14b_sym)
      cbar14bd.ax.tick_params(labelsize=18)
      ax14bd.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2),
          linewidth=3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax14bd.text(0,rmax_plot-25,'Wavenumber 2+',fontsize=20,style='italic',horizontalalignment='center')

      figfname = f'{ODIR}/{LONGSID.lower()}.vort2km_wavenumber.{forecastinit}.polar.f{FHR:03}'
      fig14b.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      fig14b.clf()
      plt.close(fig14b)
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')


    # FIGURE 15: Wavenumber 0,1,2 components of 5-km Relative Humidity
    if do_rh5km_wavenumber == 'Y':
      fig15 = plt.figure(figsize=(15,15))
      # Top two panels span the full 0-100 % RH range; bottom two
      # use a +/-30 % symmetric scale to highlight asymmetries.
      ticks15      = [0, 20, 40, 60, 80, 100]
      ticks15_sym  = [-30, -20, -10, 0, 10, 20, 30]

      # Panel A
      ax15a = fig15.add_subplot(2, 2, 1)
      co15a = ax15a.contourf(XI, YI, rh5_p[:,:], levs_rh, \
            cmap=colormap_rh, norm=norm_rh, extend='max')
      ax15a = plotting.axes_wavenumber(ax15a, rmax_plot, -rmax_plot, nx=9)
      cbar15a = plt.colorbar(co15a, ticks=ticks15)
      cbar15a.ax.tick_params(labelsize=18)
      ax15a.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax15a.set_title(f'{EXPT_TITLE.strip()}\n' + \
          r'WV#0,1,2 5-km RH ($\%$, Shading)' + \
          f'\nShear Vector in Black\nInit: {forecastinit}\nForecast Hour:[{FHR:03}]', \
          fontsize=20, weight='bold', loc='left')
      ax15a.text(0,rmax_plot-25,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

      # Panel B
      ax15b = fig15.add_subplot(2, 2, 2)
      co15b = ax15b.contourf(XI, YI, rh5_p_w0[:,:], levs_rh, \
            cmap=colormap_rh, norm=norm_rh, extend='max')
      ax15b = plotting.axes_wavenumber(ax15b, rmax_plot, -rmax_plot, nx=9)
      cbar15b = plt.colorbar(co15b, ticks=ticks15)
      cbar15b.ax.tick_params(labelsize=18)
      ax15b.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax15b.set_title(f'{LONGSID.upper()}\nVMAX= {maxwind} kt\nPMIN= {minpressure} hPa' + \
          f'\nShear Magnitude= {str(int(np.round(shearmag*1.94,0)))}kts\nShear Direction= {str(int(np.round(sheardir_met,0)))}$^\circ$', \
          fontsize=20, color='brown', loc='right')
      ax15b.text(0,rmax_plot-25,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

      # Panel C: W1 anomaly -- +/-30 % RH on the brown/green palette
      ax15c = fig15.add_subplot(2, 2, 3)
      co15c = ax15c.contourf(XI, YI, rh5_p_w1[:,:], levs_rh_sym, \
            cmap=colormap_rh, norm=norm_rh_sym, extend='both')
      ax15c = plotting.axes_wavenumber(ax15c, rmax_plot, -rmax_plot, nx=9)
      cbar15c = plt.colorbar(co15c, ticks=ticks15_sym)
      cbar15c.ax.tick_params(labelsize=18)
      ax15c.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax15c.text(0,rmax_plot-25,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

      # Panel D: W2+ (W2 plus all higher modes summed) -- +/-30 % RH
      # on the brown/green palette. Captures the eyewall mesoscale RH
      # asymmetries together with the small-scale wavenumber chatter
      # the bare W2 panel misses.
      ax15d = fig15.add_subplot(2, 2, 4)
      co15d = ax15d.contourf(XI, YI, rh5_p_whigher[:,:], levs_rh_sym, \
            cmap=colormap_rh, norm=norm_rh_sym, extend='both')
      ax15d = plotting.axes_wavenumber(ax15d, rmax_plot, -rmax_plot, nx=9)
      cbar15d = plt.colorbar(co15d, ticks=ticks15_sym)
      cbar15d.ax.tick_params(labelsize=18)
      ax15d.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax15d.text(0,rmax_plot-25,'Wavenumber 2+',fontsize=20,style='italic',horizontalalignment='center')

      # Finalize figure
      figfname = f'{ODIR}/{LONGSID.lower()}.rh5km_wavenumber.{forecastinit}.polar.f{FHR:03}'
      fig15.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      fig15.clf()
      plt.close(fig15)
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')


    # FIGURE 16: Wavenumber 0,1,2 components of 10-m Tangential Wind
    if do_vt10_wavenumber == 'Y':
      fig16 = plt.figure(figsize=(15,15))
      # Top two panels span the full 0-80 m/s wind range; bottom two
      # use a +/-20 m/s symmetric scale to highlight asymmetries.
      ticks16_full = [0, 10, 20, 30, 40, 50, 60, 70, 80]
      ticks16_sym  = [-20, -15, -10, -5, 0, 5, 10, 15, 20]

      # Panel A
      ax16a = fig16.add_subplot(2, 2, 1)
      co16a = ax16a.contourf(XI, YI, vt10_p[:,:], levs_vt, \
            cmap=colormap_vt, norm=norm_vt, extend='max')
      ax16a = plotting.axes_wavenumber(ax16a, rmax_plot, -rmax_plot, nx=9)
      cbar16a = plt.colorbar(co16a, ticks=ticks16_full)
      cbar16a.ax.tick_params(labelsize=18)
      ax16a.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax16a.set_title(f'{EXPT_TITLE.strip()}\n' +\
          r'WV#0,1,2 10-m Tangential Wind ($m\ s^{-1}$, Shading)' + \
          f'\nShear Vector in Black\nInit: {forecastinit}\nForecast Hour:[{FHR:03}]', \
          fontsize=20, weight='bold', loc='left')
      ax16a.text(0,rmax_plot-25,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

      # Panel B
      ax16b = fig16.add_subplot(2, 2, 2)
      co16b = ax16b.contourf(XI, YI, vt10_p_w0[:,:], levs_vt, \
            cmap=colormap_vt, norm=norm_vt, extend='max')
      ax16b = plotting.axes_wavenumber(ax16b, rmax_plot, -rmax_plot, nx=9)
      cbar16b = plt.colorbar(co16b, ticks=ticks16_full)
      cbar16b.ax.tick_params(labelsize=18)
      ax16b.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax16b.set_title(f'{LONGSID.upper()}\nVMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n' + \
          f'Shear Magnitude= {str(int(np.round(shearmag*1.94,0)))}kts\nShear Direction= {str(int(np.round(sheardir_met,0)))}$^\circ$', \
          fontsize=20, color='brown', loc='right')
      ax16b.text(0,rmax_plot-25,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

      # Panel C: W1 anomaly -- diverging seismic, +/-20 m/s
      ax16c = fig16.add_subplot(2, 2, 3)
      co16c = ax16c.contourf(XI, YI, vt10_p_w1[:,:], levs_vt_sym, \
            cmap=colormap_vt_sym, norm=norm_vt_sym, extend='both')
      ax16c = plotting.axes_wavenumber(ax16c, rmax_plot, -rmax_plot, nx=9)
      cbar16c = plt.colorbar(co16c, ticks=ticks16_sym)
      cbar16c.ax.tick_params(labelsize=18)
      ax16c.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax16c.text(0,rmax_plot-25,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

      # Panel D: W2+ (W2 plus all higher modes summed) -- diverging
      # seismic, +/-20 m/s
      ax16d = fig16.add_subplot(2, 2, 4)
      co16d = ax16d.contourf(XI, YI, vt10_p_whigher[:,:], levs_vt_sym, \
            cmap=colormap_vt_sym, norm=norm_vt_sym, extend='both')
      ax16d = plotting.axes_wavenumber(ax16d, rmax_plot, -rmax_plot, nx=9)
      cbar16d = plt.colorbar(co16d, ticks=ticks16_sym)
      cbar16d.ax.tick_params(labelsize=18)
      ax16d.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax16d.text(0,rmax_plot-25,'Wavenumber 2+',fontsize=20,style='italic',horizontalalignment='center')

      # Finalize figure
      figfname = f'{ODIR}/{LONGSID.lower()}.vt10_wavenumber.{forecastinit}.polar.f{FHR:03}'
      fig16.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      fig16.clf()
      plt.close(fig16)
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')


    # FIGURE 17: Wavenumber 0,1,2 components of 5-km Vertical Velocity
    if do_w5km_wavenumber == 'Y':
      fig17 = plt.figure(figsize=(15,15))
      ticks17_full = [-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5]
      ticks17_sym  = [-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2]

      # Panel A
      ax17a = fig17.add_subplot(2, 2, 1)
      co17a = ax17a.contourf(XI, YI, w5_p[:,:], levs_w_full, \
            cmap=colormap_w_sym, norm=norm_w_full, extend='both')
      ax17a = plotting.axes_wavenumber(ax17a, rmax_plot, -rmax_plot, nx=9)
      cbar17a = plt.colorbar(co17a, ticks=ticks17_full)
      cbar17a.ax.tick_params(labelsize=18)
      ax17a.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax17a.set_title(f'{EXPT_TITLE.strip()}\n' +\
          r'WV#0,1,2 5-km W ($m\ s^{-1}$, Shading)' + \
          f'\nShear Vector in Black\nInit: {forecastinit}\nForecast Hour:[{FHR:03}]', \
          fontsize=20, weight='bold', loc='left')
      ax17a.text(0,rmax_plot-25,'Full Field',fontsize=20,style='italic',horizontalalignment='center')

      # Panel B
      ax17b = fig17.add_subplot(2, 2, 2)
      co17b = ax17b.contourf(XI, YI, w5_p_w0[:,:], levs_w_full, \
            cmap=colormap_w_sym, norm=norm_w_full, extend='both')
      ax17b = plotting.axes_wavenumber(ax17b, rmax_plot, -rmax_plot, nx=9)
      cbar17b = plt.colorbar(co17b, ticks=ticks17_full)
      cbar17b.ax.tick_params(labelsize=18)
      ax17b.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax17b.set_title(f'{LONGSID.upper()}\nVMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n' + \
          f'Shear Magnitude= {str(int(np.round(shearmag*1.94,0)))}kts\nShear Direction= {str(int(np.round(sheardir_met,0)))}$^\\circ$', \
          fontsize=20, color='brown', loc='right')
      ax17b.text(0,rmax_plot-25,'Wavenumber 0',fontsize=20,style='italic',horizontalalignment='center')

      # Panel C
      ax17c = fig17.add_subplot(2, 2, 3)
      co17c = ax17c.contourf(XI, YI, w5_p_w1[:,:], levs_w_sym, \
            cmap=colormap_w_sym, norm=norm_w_sym, extend='both')
      ax17c = plotting.axes_wavenumber(ax17c, rmax_plot, -rmax_plot, nx=9)
      cbar17c = plt.colorbar(co17c, ticks=ticks17_sym)
      cbar17c.ax.tick_params(labelsize=18)
      ax17c.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax17c.text(0,rmax_plot-25,'Wavenumber 1',fontsize=20,style='italic',horizontalalignment='center')

      # Panel D: W2+ (W2 plus all higher modes summed)
      ax17d = fig17.add_subplot(2, 2, 4)
      co17d = ax17d.contourf(XI, YI, w5_p_whigher[:,:], levs_w_sym, \
            cmap=colormap_w_sym, norm=norm_w_sym, extend='both')
      ax17d = plotting.axes_wavenumber(ax17d, rmax_plot, -rmax_plot, nx=9)
      cbar17d = plt.colorbar(co17d, ticks=ticks17_sym)
      cbar17d.ax.tick_params(labelsize=18)
      ax17d.arrow(0, 0, (ushear1/25)*np.max(XI/2), (vshear1/25)*np.max(YI/2), \
          linewidth = 3, head_width=rmax_plot/20, head_length=rmax_plot/10, fc='k', ec='k')
      ax17d.text(0,rmax_plot-25,'Wavenumber 2+',fontsize=20,style='italic',horizontalalignment='center')

      # Finalize figure
      figfname = f'{ODIR}/{LONGSID.lower()}.w5km_wavenumber.{forecastinit}.polar.f{FHR:03}'
      fig17.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      fig17.clf()
      plt.close(fig17)
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')


    # FIGURES 18-22: Tangential Wind Tendency Terms
    if do_vt_tendency == 'Y':

      # Mean Radial Flux
      fig18 = plt.figure(figsize=(20.5,10.5))
      ax18 = fig18.add_subplot(1, 1, 1)
      co18 = ax18.contourf(r, heightlevs/1000, np.flipud(np.rot90(term1_vt_tendency_mean_radial_flux*1e3,1)), levs_vt_budget, \
               cmap=colormap_vt_budget, norm=norm_vt_budget, extend='both')
      ax18 = plotting.axes_radhgt(ax18, xmax=rmax_plot, nx=9, formatters=True)
      cbar18 = plt.colorbar(co18, ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
      cbar18.ax.tick_params(labelsize=24)
      sc18 = ax18.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
      ax18.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'$-\langle u_{r} \rangle \langle f+\zeta \rangle$ ($10^{-3} m s^{-2}$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax18.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.vt_tendency_term1_mean_radial_flux_mean.{forecastinit}.polar.f{FHR:03}'
      fig18.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig18.clf()
      plt.close(fig18)

      # Mean Vertical Advection
      fig19 = plt.figure(figsize=(20.5,10.5))
      ax19 = fig19.add_subplot(1, 1, 1)
      co19 = ax19.contourf(r, heightlevs/1000, np.flipud(np.rot90(term2_vt_tendency_mean_vertical_advection*1e3,1)), levs_vt_budget, \
               cmap=colormap_vt_budget, norm=norm_vt_budget, extend='both')
      ax19 = plotting.axes_radhgt(ax19, xmax=rmax_plot, nx=9, formatters=True)
      cbar19 = plt.colorbar(co19, ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
      cbar19.ax.tick_params(labelsize=24)
      sc19 = ax19.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
      ax19.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'$-\langle w \rangle \frac{\partial{\langle v_{t} \rangle}}{\partial z}$ ($10^{-3} m s^{-2}$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax19.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.vt_tendency_term2_mean_vertical_advection_mean.{forecastinit}.polar.f{FHR:03}'
      fig19.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig19.clf()
      plt.close(fig19)

      # Mean Eddy Flux
      fig20 = plt.figure(figsize=(20.5,10.5))
      ax20 = fig20.add_subplot(1, 1, 1)
      co20 = ax20.contourf(r, heightlevs/1000, np.flipud(np.rot90(term3_vt_tendency_eddy_flux*1e3,1)), levs_vt_budget, \
               cmap=colormap_vt_budget, norm=norm_vt_budget, extend='both')
      ax20 = plotting.axes_radhgt(ax20, xmax=rmax_plot, nx=9, formatters=True)
      cbar20 = plt.colorbar(co20, ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
      cbar20.ax.tick_params(labelsize=24)
      sc20 = ax20.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
      ax20.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'$-\langle u^{\prime}_{r}\zeta^{\prime} \rangle$ ($10^{-3} m s^{-2}$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax20.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.vt_tendency_term3_eddy_flux_mean.{forecastinit}.polar.f{FHR:03}'
      fig20.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig20.clf()
      plt.close(fig20)

      # Mean Vertical Eddy Advection
      fig21 = plt.figure(figsize=(20.5,10.5))
      ax21 = fig21.add_subplot(1, 1, 1)
      co21 = ax21.contourf(r, heightlevs/1000, np.flipud(np.rot90(term4_vt_tendency_vertical_eddy_advection*1e3,1)), levs_vt_budget, \
               cmap=colormap_vt_budget, norm=norm_vt_budget, extend='both')
      ax21 = plotting.axes_radhgt(ax21, xmax=rmax_plot, nx=9, formatters=True)
      cbar21 = plt.colorbar(co21, ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
      cbar21.ax.tick_params(labelsize=24)
      sc21 = ax21.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
      ax21.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'$-\langle w^{\prime}\frac{\partial{v^{\prime}_{t}}}{\partial z} \rangle$ ($10^{-3} m s^{-2}$, Shading)' +
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax21.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.vt_tendency_term4_vertical_eddy_advection_mean.{forecastinit}.polar.f{FHR:03}'
      fig21.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig21.clf()
      plt.close(fig21)

      # Sum of Mean Tendency Terms
      fig22 = plt.figure(figsize=(20.5,10.5))
      ax22 = fig22.add_subplot(1, 1, 1)
      co22 = ax22.contourf(r, heightlevs/1000, np.flipud(np.rot90(terms_vt_tendency_sum*1e3,1)), levs_vt_budget, \
               cmap=colormap_vt_budget, norm=norm_vt_budget, extend='both')
      ax22 = plotting.axes_radhgt(ax22, xmax=rmax_plot, nx=9, formatters=True)
      cbar22 = plt.colorbar(co22, ticks=[-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
      cbar22.ax.tick_params(labelsize=24)
      sc22 = ax22.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
      ax22.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'Sum of $\frac{\partial{\langle v_{t} \rangle}}{\partial t}$ Terms ($10^{-3} m s^{-2}$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax22.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.vt_tendency_terms_sum_mean.{forecastinit}.polar.f{FHR:03}'
      fig22.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig22.clf()
      plt.close(fig22)

    # FIGURES 23-27: Vorticity Tendency Terms
    if ( do_vort_tendency == 'Y'):

      # Mean Horizontal Advection
      fig23 = plt.figure(figsize=(20.5,10.5))
      ax23 = fig23.add_subplot(1, 1, 1)
      co23 = ax23.contourf(r, heightlevs/1000, np.flipud(np.rot90(term1_vort_tendency_horizontal_advection*1e5*60,1)), levs_vort_budget, \
               cmap=colormap_vort_budget, norm=norm_vort_budget, extend='both')
      ax23 = plotting.axes_radhgt(ax23, xmax=rmax_plot, nx=9, formatters=True)
      cbar23 = plt.colorbar(co23, ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
      cbar23.ax.tick_params(labelsize=24)
      sc23 = ax23.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
      ax23.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'$\langle -u_{SR}\frac{\partial{\eta}} {\partial x} - v_{SR}\frac{\partial{\eta}} {\partial y} \rangle$ ($10^{-5} s^{-1} min^{-1}$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax23.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.vort_tendency_term1_horizontal_advection_mean.{forecastinit}.polar.f{FHR:03}'
      fig23.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig23.clf()
      plt.close(fig23)

      # Mean Vertical Advection
      fig24 = plt.figure(figsize=(20.5,10.5))
      ax24 = fig24.add_subplot(1, 1, 1)
      co24 = ax24.contourf(r, heightlevs/1000, np.flipud(np.rot90(term2_vort_tendency_vertical_advection*1e5*60,1)), levs_vort_budget, \
               cmap=colormap_vort_budget, norm=norm_vort_budget, extend='both')
      ax24 = plotting.axes_radhgt(ax24, xmax=rmax_plot, nx=9, formatters=True)
      cbar24 = plt.colorbar(co24, ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
      cbar24.ax.tick_params(labelsize=24)
      sc24 = ax24.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
      ax24.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'$\langle -w\frac{\partial{\zeta}} {\partial z} \rangle$ ($10^{-5} s^{-1} min^{-1}$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax24.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.vort_tendency_term2_vertical_advection_mean.{forecastinit}.polar.f{FHR:03}'
      fig24.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig24.clf()
      plt.close(fig24)

      # Mean Stretching COnvergence
      fig25 = plt.figure(figsize=(20.5,10.5))
      ax25 = fig25.add_subplot(1, 1, 1)
      co25 = ax25.contourf(r, heightlevs/1000, np.flipud(np.rot90(term3_vort_tendency_stretching_convergence*1e5*60,1)), levs_vort_budget, \
               cmap=colormap_vort_budget, norm=norm_vort_budget, extend='both')
      ax25 = plotting.axes_radhgt(ax25, xmax=rmax_plot, nx=9, formatters=True)
      cbar25 = plt.colorbar(co25, ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
      cbar25.ax.tick_params(labelsize=24)
      sc25 = ax25.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
      ax25.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'$\langle -\eta\frac{\partial{u_{SR}}} {\partial x} - \eta\frac{\partial{v_{SR}}} {\partial y} \rangle$ ($10^{-5} s^{-1} min^{-1}$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax25.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.vort_tendency_term3_stretching_convergence_mean.{forecastinit}.polar.f{FHR:03}'
      fig25.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig25.clf()
      plt.close(fig25)

      # Mean Tilting
      fig26 = plt.figure(figsize=(20.5,10.5))
      ax26 = fig26.add_subplot(1, 1, 1)
      co26 = ax26.contourf(r, heightlevs/1000, np.flipud(np.rot90(term4_vort_tendency_tilting*1e5*60,1)), levs_vort_budget, \
               cmap=colormap_vort_budget, norm=norm_vort_budget, extend='both')
      ax26 = plotting.axes_radhgt(ax26, xmax=rmax_plot, nx=9, formatters=True)
      cbar26 = plt.colorbar(co26, ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
      cbar26.ax.tick_params(labelsize=24)
      sc26 = ax26.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
      ax26.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'$\langle -\frac{\partial{w}}{\partial x}\frac{\partial{v_{SR}}} {\partial z} + \frac{\partial{w}}{\partial y}\frac{\partial{u_{SR}}} {\partial z} \rangle$ ($10^{-5} s^{-1} min^{-1}$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax26.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.vort_tendency_term4_tilting_mean.{forecastinit}.polar.f{FHR:03}'
      fig26.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig26.clf()
      plt.close(fig26)

      # Sum of Mean Tendency Terms
      fig27 = plt.figure(figsize=(20.5,10.5))
      ax27 = fig27.add_subplot(1, 1, 1)
      co27 = ax27.contourf(r, heightlevs/1000, np.flipud(np.rot90(terms_vort_tendency_sum*1e5*60,1)), levs_vort_budget, \
               cmap=colormap_vort_budget, norm=norm_vort_budget, extend='both')
      ax27 = plotting.axes_radhgt(ax27, xmax=rmax_plot, nx=9, formatters=True)
      cbar27 = plt.colorbar(co27, ticks=[-40, -30, -20, -10, 0, 10, 20, 30, 40])
      cbar27.ax.tick_params(labelsize=24)
      sc27 = ax27.scatter(rmw_mean[4:20], heightlevs[4:20]/1000, 70, 'k')
      ax27.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'Sum of $\frac{\partial{\langle \zeta \rangle}}{\partial t}$ Terms ($10^{-5} s^{-1} min^{-1}$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax27.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.vort_tendency_terms_sum_mean.{forecastinit}.polar.f{FHR:03}'
      fig27.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig27.clf()
      plt.close(fig27)

    if ( do_ur_pbl_p_mean == 'Y'):
      #Plot PBL Inflow
      fig28 = plt.figure(figsize=(20.5,10.5))
      ax28 = fig28.add_subplot(1, 1, 1)
      co28 = ax28.contourf(r, heightlevs_pbl, np.flipud(np.rot90(ur_pbl_p_mean,1)), levs_ur, \
               cmap=colormap_ur, norm=norm_ur, extend='both')
      ax28 = plotting.axes_radhgt(ax28, xmax=rmax_plot, nx=9, ymax=3000, ny=7, yunit='m', formatters=True)
      cbar28 = plt.colorbar(co28, ticks=[-30, -25, -20, -15, -10, -5, -1, 1, 5, 10, 15, 20, 25, 30])
      cbar28.ax.tick_params(labelsize=24)
      co28b = ax28.contour(r, heightlevs_pbl, np.flipud(np.rot90(ur_pbl_p_mean,1)), \
               levels=[0.1*np.nanmin(ur_pbl_p_mean)], colors='w', linewidths=4)
      sc28 = ax28.scatter(rmw_pbl_mean, heightlevs_pbl, 70, 'k')
      ax28.set_title(f'{EXPT_TITLE.strip()}\n' + \
               r'Radial Wind in PBL ($m\ s^{-1}$, Shading)' + \
               f'\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
               fontsize=24, weight='bold', loc='left')
      ax28.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', fontsize=24, color='brown', loc='right')
      figfname = f'{ODIR}/{LONGSID.lower()}.ur_pbl_p_mean.{forecastinit}.polar.f{FHR:03}'
      fig28.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      if DO_CONVERTGIF:
        plot_utils.convert_to_gif(f'{figfname}{figext}')
      fig28.clf()
      plt.close(fig28)

    if ( do_radar_plots == 'Y'):
      #Make Plots for Comparison With Radar
      #Make Horizontal Wind and Reflectivity Plot
      plt.figure(figsize=(19.5,12))
      plt.subplot(121)
      plt.contourf(x_sr,y_sr,dbz_2km,levs_dbz,cmap=colormap_dbz,norm=norm_dbz)
      plt.xlim(-244,244)
      plt.ylim(-244,244)
      plt.xticks(np.linspace(-200,200,5),fontsize=14)
      plt.yticks(np.linspace(-200,200,5),fontsize=14)
      plt.gca().set_aspect('equal', adjustable='box')
      plt.grid()
      plt.arrow(0,0,ushear1*3.5*1.94,vshear1*3.5*1.94, width=2, head_width=10, head_length=10, fc='blue', ec='black')
      plt.xticks(np.linspace(-200,200,5),fontsize=14)
      plt.yticks(np.linspace(-200,200,5),fontsize=14)
      plt.xlabel('East-West Distance (km)',fontsize=18)
      plt.ylabel('North-South Distance (km)',fontsize=18)
      plt.title(EXPT_TITLE.strip()+'\n'+ '2-km Reflectivity (dbz, Shading)'+'\n'+'2-km Wind Barbs (kt)'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
      plt.barbs(x_sr[::9],y_sr[::9],u2km[::9,::9]*1.94,v2km[::9,::9]*1.94,length=6,sizes=dict(spacing=0.15,height=0.4))
      ticks=[0, 10, 20, 30, 40, 50, 60, 70, 80]
      ax = plt.gca()
      divider = make_axes_locatable(ax)
      cax = divider.append_axes("right", size="5%", pad=0.05)
      cbar_l = plt.colorbar(cax=cax,ticks=ticks,norm=norm_dbz,drawedges=True)
      cbar_l.set_ticklabels([0, 10, 20, 30, 40, 50, 60, 70, 80])
      cbar_l.ax.tick_params(labelsize=14)
      cbar_l.outline.set_color('black')
      cbar_l.outline.set_linewidth(1)
      cbar_l.dividers.set_color('black')
      cbar_l.dividers.set_linewidth(1)

      plt.subplot(122)
      plt.contourf(x_sr,y_sr,wind_2km*1.94,levs_wind,cmap=colormap_wind,norm=norm_wind)
      plt.xlim(-244,244)
      plt.ylim(-244,244)
      plt.xticks(np.linspace(-200,200,5),fontsize=14)
      plt.yticks(np.linspace(-200,200,5),fontsize=14)
      plt.gca().set_aspect('equal', adjustable='box')
      plt.grid()
      plt.xticks(np.linspace(-200,200,5),fontsize=14)
      plt.yticks(np.linspace(-200,200,5),fontsize=14)
      plt.xlabel('East-West Distance (km)',fontsize=18)
      plt.ylabel('North-South Distance (km)',fontsize=18)
      plt.title(EXPT_TITLE.strip()+'\n'+'2-km Wind (kt, Shading)'+'\n'+'2-km (Black) and 5-km (Gray) Streamlines'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
      plt.text(-200,200,'2-km Max'+r'$\bf\overline{V}_{t}$'+' (RMW):\n'+vmaxstring+' kt ('+rmwstring+' km)',fontsize=14,verticalalignment='top', horizontalalignment='left',color='k',weight = 'bold',bbox=dict(facecolor='white', edgecolor='black'))
      plt.text(200,200,'Shear:\n'+shearstring+' kt',fontsize=14,verticalalignment='top', horizontalalignment='right',color='blue',weight = 'bold',bbox=dict(facecolor='white', edgecolor='black'))
      ticks=[7, 16, 25, 34, 40, 46, 52, 58, 64, 80, 96, 110, 125, 140, 155]
      x_sr_250 = np.linspace(-250,250,(int(rmaxlocal//resolution)+1))
      y_sr_250 = np.linspace(-250,250,(int(rmaxlocal//resolution)+1))
      X_SR_250,Y_SR_250 = np.meshgrid(x_sr_250,y_sr_250)
      # bounds_error=False + NaN fill for the 2-km / 5-km wind-shear plot
      # interpolators. Same nest-edge-encroachment rationale as elsewhere.
      _rgi_shearplot_kw = dict(bounds_error=False, fill_value=np.nan)
      f_u2km_plot = interpolate.RegularGridInterpolator((y_sr,x_sr), u2km, **_rgi_shearplot_kw)
      u2km_plot = f_u2km_plot((Y_SR_250,X_SR_250),method='linear')
      f_v2km_plot = interpolate.RegularGridInterpolator((y_sr,x_sr), v2km, **_rgi_shearplot_kw)
      v2km_plot = f_v2km_plot((Y_SR_250,X_SR_250),method='linear')
      f_u5km_plot = interpolate.RegularGridInterpolator((y_sr,x_sr), u5km, **_rgi_shearplot_kw)
      u5km_plot = f_u5km_plot((Y_SR_250,X_SR_250),method='linear')
      f_v5km_plot = interpolate.RegularGridInterpolator((y_sr,x_sr), v5km, **_rgi_shearplot_kw)
      v5km_plot = f_v5km_plot((Y_SR_250,X_SR_250),method='linear')
      #plt.gca().streamplot(x_sr_2,y_sr_2,u2km*1.94,v2km*1.94,density=3,color='k',linewidth=2,arrowstyle='->',arrowsize=2)
      #plt.gca().streamplot(x_sr_2,y_sr_2,u5km*1.94,v5km*1.94,density=3,color='0.5',linewidth=2,arrowstyle='->',arrowsize=2)
      plt.gca().streamplot(X_SR_250,Y_SR_250,u2km_plot*1.94,v2km_plot*1.94,density=2,color='k',linewidth=2,arrowstyle='->',arrowsize=2)
      plt.gca().streamplot(X_SR_250,Y_SR_250,u5km_plot*1.94,v5km_plot*1.94,density=2,color='0.5',linewidth=2,arrowstyle='->',arrowsize=2)
      plt.gca().arrow(0,0,ushear1*3.5*1.94,vshear1*3.5*1.94, width=2, head_width=10, head_length=10, fc='blue', ec='black',zorder=10)
      ax = plt.gca()
      divider = make_axes_locatable(ax)
      cax = divider.append_axes("right", size="5%", pad=0.05)
      cbar_r = plt.colorbar(cax=cax,ticks=ticks,norm=norm_wind,drawedges=True)
      cbar_r.set_ticklabels([7, 16, 25, 34, 40, 46, 52, 58, 64, 80, 96, 110, 125, 140, 155])
      cbar_r.ax.tick_params(labelsize=14)
      cbar_r.outline.set_color('black')
      cbar_r.outline.set_linewidth(1)
      cbar_r.dividers.set_color('black')
      cbar_r.dividers.set_linewidth(1)

      plt.subplots_adjust(wspace=.25)
      figfname = ODIR+'/'+LONGSID.lower()+'.dbz_2km_wind_5km_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')
      plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      plt.close()
      if ( DO_CONVERTGIF ):
        plot_utils.convert_to_gif(f'{figfname}{figext}')

      #Make Plot of Precipitation Type
      plt.figure(figsize=(20.5,12))
      plt.subplot(121)
      plt.contourf(x_sr,y_sr,dbz[:,:,4],levs_dbz,cmap=colormap_dbz,norm=norm_dbz)
      plt.xlim(-244,244)
      plt.ylim(-244,244)
      plt.xticks(np.linspace(-200,200,5),fontsize=14)
      plt.yticks(np.linspace(-200,200,5),fontsize=14)
      plt.gca().set_aspect('equal', adjustable='box')
      plt.grid()
      plt.xlabel('East-West Distance (km)',fontsize=18)
      plt.ylabel('North-South Distance (km)',fontsize=18)
      plt.title(EXPT_TITLE.strip()+'\n'+'2-km Reflectivity (dBZ)'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
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
      plt.contourf(x_sr,y_sr,ptype[:,:],[0,1,2,3,4,5],colors=['xkcd:white','xkcd:green','xkcd:yellow','xkcd:orange','xkcd:red'])#,extendfrac='auto')
      plt.xlim(-244,244)
      plt.ylim(-244,244)
      plt.xticks(np.linspace(-200,200,5),fontsize=14)
      plt.yticks(np.linspace(-200,200,5),fontsize=14)
      plt.gca().set_aspect('equal', adjustable='box')
      plt.grid()
      plt.xlabel('East-West Distance (km)',fontsize=18)
      plt.ylabel('North-South Distance (km)',fontsize=18)
      plt.title(EXPT_TITLE.strip()+'\n'+'2-km Precipitation Type'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
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
        plot_utils.convert_to_gif(f'{figfname}{figext}')

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
      plt.title(EXPT_TITLE.strip()+'\n'+ r'Azimuthal Mean Tangential Wind ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
      plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
      #plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.vt_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
      figfname = ODIR+'/'+LONGSID.lower()+'.vt_mean_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')
      plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      plt.close()
      if ( DO_CONVERTGIF ):
        plot_utils.convert_to_gif(f'{figfname}{figext}')

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
      plt.title(EXPT_TITLE.strip()+'\n'+ r'Azimuthal Mean Radial Wind ($m\ s^{-1}$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:[{:03d}]'.format(FHR),fontsize=24, weight = 'bold',loc='left')
      plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
      #plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.ur_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
      figfname = ODIR+'/'+LONGSID.lower()+'.ur_mean_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')
      plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      plt.close()
      if ( DO_CONVERTGIF ):
        plot_utils.convert_to_gif(f'{figfname}{figext}')

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
      plt.title(EXPT_TITLE.strip()+'\n'+ r'Azimuthal Mean Reflectivity ($dBZ$, Shading)'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
      plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
      #plt.gcf().savefig(ODIR+'/'+LONGSID.lower()+'.dbz_mean.'+forecastinit+'.polar.f'+format(FHR,'03d')+'.png', bbox_inches='tight', dpi='figure')
      figfname = ODIR+'/'+LONGSID.lower()+'.dbz_mean_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')
      plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
      plt.close()
      if ( DO_CONVERTGIF ):
        plot_utils.convert_to_gif(f'{figfname}{figext}')

    # (py3grads pipeline retired — no control file to close.)


    #Make Shear/RH Combo Plot With Vortex-Removed Shear, If Flag is Set on
    if (do_shear_and_rh_plots == 'Y'):
      shearandrhplot.shearandrhplot(XI, YI, theta, r, ushear_p, vshear_p, np.nanmean(rh_p[:,:,6:10],2), '3km', '5km', rmw_mean[6], rmw_mean[10], GPLOT_DIR, \
                                    EXPT, FHR, maxwind, minpressure, LONGSID, ODIR, forecastinit, DO_CONVERTGIF)

    #Make Recentered Tilt Plots if Flag is Set On
    if do_tdr_recentering == 'Y':
      import modules.tilt_plot as tilt_plot
      if newcenter_flag[0] < 1:
        anchorlon = newcenter_lon[0]
        anchorlat = newcenter_lat[0]
      else:
        anchorlon = centerlon
        anchorlat = centerlat

      titlestring1 = EXPT_TITLE.strip()+'\n'+ r'2-km Vorticity ($10^{-4} s^{-1}$, Shading) and Storm-Relative Wind'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']'
      titlestring2 = EXPT_TITLE.strip()+'\n'+ r'5-km Vorticity ($10^{-4} s^{-1}$, Shading) and Storm-Relative Wind'+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']'
      figfname = ODIR+'/'+LONGSID.lower()+'.vort_tilt_aircraft.'+forecastinit+'.polar.f'+format(FHR,'03d')
      tilt_plot.tilt_plot(kmin_tilt,kmax_tilt,x_sr_200km_interp,y_sr_200km_interp,heightlevs/1000,np.flipud(np.rot90(uwind_sr_200km_interp)),np.flipud(np.rot90(vwind_sr_200km_interp)),np.flipud(np.rot90(wwind_200km_interp)),np.flipud(np.rot90(vort_200km_interp)),newcenter_xindex,newcenter_yindex,newcenter_flag,anchorlon,anchorlat,'vort',SHIPS_ShearMagNum,SHIPS_ShearDirMetNum,titlestring1,titlestring2,figfname+figext)
      print(x_sr_200km_interp[newcenter_xindex])
      print(y_sr_200km_interp[newcenter_yindex])
      plt.close()
      if ( DO_CONVERTGIF ):
        plot_utils.convert_to_gif(f'{figfname}{figext}')

    finish = time.perf_counter()
    print(f'MSG: Total time for plotting: {finish-start:.2f} second(s)')

    # Write the input file to a log to mark that it has ben processed
    plot_utils.update_plotted_file(PLOTTED_FILE, FILE)

  print('MSG: DOING THE EXTRA STUFF')
  combinedfile = f'{ODIR}/{LONGSID.lower()}.structure_statistics.{forecastinit}.polar.all.txt'
  pastecmd = 'paste -sd"\\n" '+ODIR+'/'+LONGSID.lower()+'.structure_statistics.'+forecastinit+'.polar.f*.txt'+' > '+combinedfile
  print(f'MSG: pastecmd = {pastecmd}')
  os.system(pastecmd)
  pythonexec = sys.executable
  runcmd = (f'{pythonexec} {PYTHONDIR}/plot_structure_metrics.py'
            f' --datafile {combinedfile}'
            f' --expt {EXPT.strip()}'
            f' --odir {ODIR}'
            f' --forecastinit {forecastinit}'
            f' --longsid {LONGSID}')
  print(f'MSG: runcmd = {runcmd}')
  subprocess.call(runcmd,shell=True)

  #edit12/23-------------------------------
  combinedfile = f'{ODIR}/{LONGSID.lower()}.structure_statistics_ptype_rh.{forecastinit}.polar.all.txt'
  pastecmd = 'paste -sd"\\n" '+ODIR+'/'+LONGSID.lower()+'.structure_statistics_ptype_rh.'+forecastinit+'.polar.f*.txt'+' > '+combinedfile
  print(f'MSG: pastecmd = {pastecmd}')
  os.system(pastecmd)
#  pythonexec = sys.executable
#  runcmd = (f'{pythonexec} {PYTHONDIR}/plot_structure_metrics.py'
#            f' --datafile {combinedfile}'
#            f' --expt {EXPT.strip()}'
#            f' --odir {ODIR}'
#            f' --forecastinit {forecastinit}'
#            f' --longsid {LONGSID}')
#  print(f'MSG: runcmd = {runcmd}')
#  subprocess.call(runcmd,shell=True)

  combinedfile = f'{ODIR}/{LONGSID.lower()}.structure_statistics_massflux.{forecastinit}.polar.all.txt'
  pastecmd = 'paste -sd"\\n" '+ODIR+'/'+LONGSID.lower()+'.structure_statistics_massflux.'+forecastinit+'.polar.f*.txt'+' > '+combinedfile
  print(f'MSG: pastecmd = {pastecmd}')
  os.system(pastecmd)
#  pythonexec = sys.executable
#  runcmd = (f'{pythonexec} {PYTHONDIR}/plot_structure_metrics.py'
#            f' --datafile {combinedfile}'
#            f' --expt {EXPT.strip()}'
#            f' --odir {ODIR}'
#            f' --forecastinit {forecastinit}'
#            f' --longsid {LONGSID}')
#  print(f'MSG: runcmd = {runcmd}')
#  subprocess.call(runcmd,shell=True)

  combinedfile = f'{ODIR}/{LONGSID.lower()}.structure_statistics_vorticity.{forecastinit}.polar.all.txt'
  pastecmd = 'paste -sd"\\n" '+ODIR+'/'+LONGSID.lower()+'.structure_statistics_vorticity.'+forecastinit+'.polar.f*.txt'+' > '+combinedfile
  print(f'MSG: pastecmd = {pastecmd}')
  os.system(pastecmd)
#  pythonexec = sys.executable
#  runcmd = (f'{pythonexec} {PYTHONDIR}/plot_structure_metrics.py'
#            f' --datafile {combinedfile}'
#            f' --expt {EXPT.strip()}'
#            f' --odir {ODIR}'
#            f' --forecastinit {forecastinit}'
#            f' --longsid {LONGSID}')
#  print(f'MSG: runcmd = {runcmd}')
#  subprocess.call(runcmd,shell=True)
  #edit12/23end-------------------------------

  # Retry-convert any orphan .png left behind by transient ImageMagick
  # failures. If the retry also fails, write status='incomplete' so
  # the workflow re-invokes us next iteration.
  _sweep = plot_utils.sweep_orphan_pngs(ODIR)
  _status_value = 'incomplete' if _sweep.get('still_failed', 0) > 0 else 'complete'
  if _status_value == 'incomplete':
      print(f"WARNING: polar: {_sweep['still_failed']} PNG(s) still "
            f"unconverted after sweep; writing status='incomplete'.")

  print('MSG: COMPLETING')
  os.system(f'lockfile -r-1 -l 180 {ST_LOCK_FILE}')
  os.system(f'echo "{_status_value}" > {STATUS_FILE}')
  os.system(f'rm -f {ST_LOCK_FILE}')

  # Log some important information
  print(f'MSG: polar_cylindrical_structure.py completed at {datetime.datetime.now()}')

##############################
def wait_random(lev):
  rand = np.random.randint(2,10)
  print(f'MSG: Waiting {rand} seconds for lev={lev}.')
  time.sleep(rand)
  return rand, lev


##############################
if __name__ == '__main__':
  main()
