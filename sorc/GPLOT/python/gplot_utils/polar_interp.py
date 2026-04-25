"""Polar-cylindrical / height interpolation helpers for the GPLOT polar module.

Session F-4 consolidation: merges the old ``modules/interp.py`` and
``modules/multiprocess.py`` into one cohesive utility. Both legacy files were
only used by ``polar_cylindrical_structure.py``; ``modules/multiprocess.py``'s
``multiprocess_prs_vars`` helper (which depended on the retired py3grads
``io_extra.read_grads`` wrapper) is intentionally dropped here as dead code
after the Session E grib_reader migration.

Public API:
    interp_to_isosurface
    interp_to_polarcylindrical
    multiprocess_polar_interp
    multiprocess_polar_vars
    multiprocess_height_interp
    multiprocess_height_vars
"""

import concurrent.futures
import datetime

import numpy as np
import metpy
from scipy import interpolate


# ----------------------------------------------------------------------------
# Per-level primitives
# ----------------------------------------------------------------------------
def interp_to_isosurface(hgt, varPrs, lev, idx, ivar, verbose=False):
  """Interpolate input data (varPrs) from pressure levels to a height level
  (i.e., isosurface). Return 2D data (lat, lon) along with the height level
  and the height level index.
  @param hgt:     3D height data on pressure levels (lev, lat, lon)
  @param varPrs:  3D input data on pressure levels (lev, lat, lon)
  @param lev:     Height level to interpolate to
  @param idx:     Level index
  @param ivar:    Variable index
  @kwarg verbose: Logical to determine level of verbosity
  """
  if verbose:
    print(f'MSG: Interpolating to the {int(lev)}-m isosurface for var{ivar} - {datetime.datetime.now()}')
  return metpy.interpolate.interpolate_to_isosurface(hgt, varPrs, lev), lev, idx


def interp_to_polarcylindrical(varIn, lev, x, y, xi, yi, idx, ivar, verbose=False):
  """Interpolate 2D input data (varIn) from Cartesian coordinates to polar
  cylindrical coordinates for a given height level.
  @param varIn:   2D input data on a standard lat/lon or x/y grid
  @param lev:     Vertical level
  @param x:       Actual distance in the x-direction
  @param y:       Actual distance in the y-direction
  @param xi:      X locations that correspond to polar coordinates
  @param yi:      Y locations that correspond to polar coordinates
  @param idx:     Index of the current level
  @param ivar:    Index of the current input variable
  @kwarg verbose: Logical to control verbosity
  """
  if verbose:
    print(f'MSG: Interpolating to polar cylindrical coordinates for level {int(lev)} for var{ivar} - {datetime.datetime.now()}')
  varTmp = interpolate.RegularGridInterpolator((y, x), varIn)
  varPolar = varTmp((yi, xi), method='linear')
  return varPolar, lev, idx


# ----------------------------------------------------------------------------
# Thread-pool parallel wrappers
# ----------------------------------------------------------------------------
def multiprocess_polar_interp(varIn, x, y, xi, yi, levels, idx):
  """Parallelize polar interpolation over all vertical levels for one variable."""
  allstacks, indices = [], []
  with concurrent.futures.ThreadPoolExecutor(max_workers=8) as executor:
    results = [executor.submit(interp_to_polarcylindrical,
                               var, lev, x, y, xi, yi, iii, idx)
               for (iii, (var, lev)) in enumerate(zip(varIn, levels))]
    for job in concurrent.futures.as_completed(results):
      (data, lll, ix) = job.result()
      allstacks.append(data); indices.append(ix)

  indices_sorted = np.argsort(np.array(indices))
  varPolar = np.transpose(np.stack(allstacks)[indices_sorted, :, :], (1, 2, 0))
  return varPolar, idx


def multiprocess_polar_vars(x, y, xi, yi, varList=None, levels=None):
  """Parallelize polar interpolation across multiple variables (outer pool)."""
  if varList is None:
    raise ValueError('List of variables to be processed must be provided.')
  if levels is None:
    raise ValueError('List of height levels (m) must be provided.')

  allstacks, indices = [], []
  with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
    results = [executor.submit(multiprocess_polar_interp,
                               var, x, y, xi, yi, levels, idx)
               for idx, var in enumerate(varList)]
    for job in concurrent.futures.as_completed(results):
      (data, ix) = job.result()
      allstacks.append(data); indices.append(ix)

  indices_sorted = np.argsort(np.array(indices))
  varPolarList = np.stack(allstacks)[indices_sorted, :, :, :]
  return varPolarList


def multiprocess_height_interp(hgt=None, varPrs=None, levels=None, idx=0):
  """Parallelize interpolation from pressure surfaces to height surfaces.
  @kwarg hgt:    3D height data on pressure levels (lev, lat, lon)
  @kwarg varPrs: 3D input data on pressure levels (lev, lat, lon)
  @kwarg levels: 1D array/list of height levels in meters
  @kwarg idx:    Variable index
  """
  if hgt is None:
    raise ValueError('Height data must be defined.')
  if varPrs is None:
    raise ValueError('Data on pressure levels must be provided.')
  if levels is None:
    raise ValueError('List of height levels (m) must be provided.')

  allstacks, indices = [], []
  with concurrent.futures.ThreadPoolExecutor(max_workers=8) as executor:
    results = [executor.submit(interp_to_isosurface, hgt, varPrs, lev, iii, idx)
               for (iii, lev) in enumerate(levels)]
    for job in concurrent.futures.as_completed(results):
      (data, lll, ix) = job.result()
      allstacks.append(data); indices.append(ix)

  indices_sorted = np.argsort(np.array(indices))
  varHgt = np.transpose(np.stack(allstacks)[indices_sorted, :, :], (1, 2, 0))
  return varHgt, idx


def multiprocess_height_vars(hgt=None, varList=None, varNames=None, levels=None):
  """Parallelize height interpolation across multiple variables.
  @kwarg hgt:      3D height data on pressure levels (lev, lat, lon)
  @kwarg varList:  List of 3D data on pressure levels for each variable
  @kwarg varNames: List of variable names (unused; retained for API compat)
  @kwarg levels:   1D array/list of height levels in meters
  """
  if hgt is None:
    raise ValueError('Height data must be defined.')
  if varList is None:
    raise ValueError('List of variables to be processed must be provided.')
  if levels is None:
    raise ValueError('List of height levels (m) must be provided.')

  allstacks, indices = [], []
  with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
    results = [executor.submit(multiprocess_height_interp, hgt, var, levels, idx)
               for idx, var in enumerate(varList)]
    for job in concurrent.futures.as_completed(results):
      (data, ix) = job.result()
      allstacks.append(data); indices.append(ix)

  indices_sorted = np.argsort(np.array(indices))
  varHgtList = np.stack(allstacks)[indices_sorted, :, :, :]
  return varHgtList
