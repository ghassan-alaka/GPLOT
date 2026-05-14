"""Polar-cylindrical / height interpolation helpers for the GPLOT polar module.

Session F-4 consolidation: merges the old ``modules/interp.py`` and
``modules/multiprocess.py`` into one cohesive utility. Both legacy files were
only used by ``polar_cylindrical_structure.py``; the legacy
``multiprocess_prs_vars`` helper, which depended on a retired py3grads
read wrapper, was intentionally dropped here as dead code after the
Session E grib_reader migration.

Public API:
    interp_to_isosurface          (legacy single-level, metpy-backed)
    interp_to_isosurface_fast     (vectorized all-levels-at-once replacement)
    interp_to_polarcylindrical
    multiprocess_polar_interp
    multiprocess_polar_vars
    multiprocess_height_interp    (legacy thread-per-level)
    multiprocess_height_vars      (legacy thread-per-variable, calls above)
    height_interp_vars_fast       (vectorized replacement for multiprocess_height_vars)
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


def interp_to_isosurface_fast(hgt, varPrs, levels):
  """Vectorized pressure→height interpolation for *all* target levels at once.

  Drop-in numerical replacement for repeated calls to
  ``metpy.interpolate.interpolate_to_isosurface(hgt, varPrs, lev)`` over a
  list of target heights. metpy invokes per-call wrapper code (input
  validation, masking setup, ``np.atleast_1d`` etc.) on every invocation,
  so 37 calls per variable × 12 variables = 444 calls/file becomes the
  dominant cost in ``_interp_to_height``. This version does each variable
  in a single pass: 37 fully-vectorized numpy ops instead of 37 metpy
  calls.

  Numerical contract (matches metpy with ``from_below=True``):
    - For each (j, i) column, find the lowest pressure-level index k such
      that ``hgt[k+1, j, i] > lev`` and ``hgt[k, j, i] <= lev``.
    - Linearly interpolate ``varPrs`` between (k, k+1) using
      ``w = (lev - hgt[k]) / (hgt[k+1] - hgt[k])``.
    - Columns where ``lev`` falls outside the range of ``hgt[:, j, i]``
      receive NaN (consistent with metpy's out-of-range behavior).

  @param hgt:     3D height data on pressure levels, shape (nz_p, ny, nx).
                  Expected monotonically increasing along axis 0.
  @param varPrs:  3D field on the same pressure grid, shape (nz_p, ny, nx).
  @param levels:  1D array of target height levels (m), shape (nz_h,).
  @returns:       3D array on the height grid, shape (nz_h, ny, nx).
  """
  nz_p, ny, nx = hgt.shape
  nz_h = len(levels)
  out = np.empty((nz_h, ny, nx), dtype=np.float64)
  # Per target level: one vectorized pass over the (nz_p, ny, nx) cube.
  # Memory peak per iteration is ~4 × nz_p × ny × nx × 8 B (a few work
  # arrays of the same shape as hgt) — for a 37 × 600 × 600 grid that's
  # ~427 MB peak, which is fine on the typical HAFS host. We deliberately
  # avoid the (nz_h, nz_p, ny*nx) 4-D broadcast that would balloon to
  # multiple GB on bigger domains.
  for li, lev in enumerate(levels):
    mask = hgt > lev                                   # (nz_p, ny, nx) bool
    upper = mask.argmax(axis=0)                        # (ny, nx)  first True k
    lower = np.clip(upper - 1, 0, nz_p - 1)            # (ny, nx)
    # Out-of-range: column has no True (target above column top) or all
    # True (target below column bottom — i.e., below ground).
    no_true  = ~mask.any(axis=0)
    all_true = mask.all(axis=0)
    bad      = no_true | all_true
    # Gather bracketing values per column. ``np.take_along_axis`` needs a
    # 3-D index of the same rank as the source.
    upper3 = upper[None, :, :]
    lower3 = lower[None, :, :]
    h_lo = np.take_along_axis(hgt,    lower3, axis=0)[0]
    h_hi = np.take_along_axis(hgt,    upper3, axis=0)[0]
    v_lo = np.take_along_axis(varPrs, lower3, axis=0)[0]
    v_hi = np.take_along_axis(varPrs, upper3, axis=0)[0]
    denom = h_hi - h_lo
    # Avoid divide-by-zero where bracket collapsed (will be masked anyway).
    safe = np.where(denom != 0.0, denom, 1.0)
    w = (lev - h_lo) / safe
    layer = v_lo + w * (v_hi - v_lo)
    layer[bad] = np.nan
    out[li] = layer
  return out


def height_interp_vars_fast(hgt, varList, levels):
  """Vectorized replacement for ``multiprocess_height_vars``.

  Same input/output shapes; no thread pool. For each variable we make one
  call to :func:`interp_to_isosurface_fast`, which interpolates onto the
  full target-height column in a single vectorized pass. This eliminates
  the 4×8 nested ``ThreadPoolExecutor`` layout (which was largely
  GIL-bound on metpy's Python-level wrapper code) and the 420 per-leaf
  metpy calls per file.

  @param hgt:     3D height-on-pressure array, shape (nz_p, ny, nx).
  @param varList: Iterable of 3D fields, each shape (nz_p, ny, nx).
  @param levels:  1D array of target heights (m), shape (nz_h,).
  @returns:       4D stack, shape (n_vars, ny, nx, nz_h), matching the
                  layout produced by ``multiprocess_height_vars``.
  """
  if hgt is None:
    raise ValueError('Height data must be defined.')
  if varList is None:
    raise ValueError('List of variables to be processed must be provided.')
  if levels is None:
    raise ValueError('List of height levels (m) must be provided.')
  out_list = []
  for var in varList:
    cube = interp_to_isosurface_fast(hgt, var, levels)   # (nz_h, ny, nx)
    out_list.append(np.transpose(cube, (1, 2, 0)))       # (ny, nx, nz_h)
  return np.stack(out_list, axis=0)                      # (n_vars, ny, nx, nz_h)


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
  # bounds_error=False + fill_value=NaN: when the d03 moving nest has
  # fallen behind the storm, some polar points (yi, xi) fall outside
  # the storm-relative (y, x) bounds. Default would raise
  # "ValueError: One of the requested xi is out of bounds in
  # dimension 0" and abort the whole FHR. Filling with NaN keeps
  # ring metrics + panels usable for the covered quadrant; the caller
  # in polar_cylindrical_structure.py owns the >25%-NaN hard stop
  # that finalizes the time-series products and exits cleanly.
  varTmp = interpolate.RegularGridInterpolator((y, x), varIn,
                                                bounds_error=False,
                                                fill_value=np.nan)
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
  """[DEPRECATED — use ``height_interp_vars_fast``]
  Parallelize interpolation from pressure surfaces to height surfaces.
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
  """[DEPRECATED — use ``height_interp_vars_fast``]
  Parallelize height interpolation across multiple variables.
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
