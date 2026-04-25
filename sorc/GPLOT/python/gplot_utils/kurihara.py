"""
TC vortex-removal filters for steering-flow diagnostics.

The module exposes two smoothers:

* ``kurihara_smooth`` -- algorithmic port of
  ``sorc/GPLOT/fortran/hbfilter.f90`` (Kurihara et al. 1993, MWR).
  Separable 11-stage chain filter applied NMAX times per axis.
  Interior-point math matches the Fortran exactly (same M table, same
  FK(n) = 0.5 / (1 - cos(2*pi/M(n))) coefficients, same edge-pinning
  stage-by-stage). Boundary treatment is cleaner than the Fortran,
  which skipped the last row/column in its zonal outer loop -- a
  quirk that never mattered in production because the ``external
  HBFILTER`` declaration in ``ncl/GPLOT_maps.ncl`` was commented out
  and the .so was never actually called. On sub-3-km grids the
  low-pass cutoff is too narrow to strip the full TC vortex even at
  NMAX>=100 (see notes below), which is why ``vortex_filter`` defaults
  to the Gaussian method.

* ``vortex_filter`` -- *default* steering-flow filter. Uses a 2D isotropic
  Gaussian smoother whose sigma is specified in kilometres so the spatial
  cutoff scales consistently across grid resolutions. Empirically, a
  sigma of ~150 km attenuates a mature TC core's wind from ~65 kt to
  ~23 kt on a 2.2-km storm nest while preserving the broader synoptic
  flow (>500 km features). Runs in a few seconds per 3D cube via
  ``scipy.ndimage.gaussian_filter``.

Why the split
-------------
The Kurihara reference at NMAX=27/3km was tuned for ~12 gridpoints per
TC core. Modern HAFS storm nests resolve the core with ~50 gridpoints,
so the 11-stage cutoff (wavelengths up to ~9*dx * NMAX) no longer covers
the full vortex scale. A scale-explicit Gaussian is both faster and more
controllable than cranking NMAX up to the hundreds, which would also
smear synoptic structure. The Fortran pipeline never ran HBFILTER in
production (the ``external HBFILTER`` declaration in
``ncl/GPLOT_maps.ncl`` was commented out) so the Python port is free to
pick a working scheme rather than reproduce a broken one.

Performance
-----------
Gaussian filter on an 801 x 1001 x 25-level cube runs in ~7-10 seconds at
sigma=150 km. Kurihara at NMAX=27 runs in ~24 seconds and attenuates far
less. Both methods vectorize across the leading (vertical) axis for 3D
inputs.
"""

from __future__ import annotations

import logging
import numpy as np
from scipy.ndimage import convolve1d, gaussian_filter

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Filter coefficients (frozen at import time)
# ---------------------------------------------------------------------------

# Wavelength denominators from hbfilter.f90 DATA statement.
_M = np.array([2, 3, 4, 2, 5, 6, 7, 2, 8, 9, 2], dtype=float)

# FK(n) = 0.5 / (1 - cos(2*pi / M(n))). For M=2 this is 0.25, i.e.
# the classic 1-2-1 smoother. Other stages use larger FK, giving a
# broader response. Eleven stages combined produce a low-pass filter
# whose -3 dB point lies near the synoptic scale.
_FK = 0.5 / (1.0 - np.cos(2.0 * np.pi / _M))

# Pre-build the 11 convolution kernels [FK, 1 - 2*FK, FK].
_KERNELS = [np.array([fk, 1.0 - 2.0 * fk, fk], dtype=float) for fk in _FK]


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def kurihara_smooth(field, nmax):
    """
    Apply the Kurihara (1993) 11-stage separable filter NMAX times per axis.

    Parameters
    ----------
    field : np.ndarray
        2D (lat, lon) or 3D (lev, lat, lon) array. The last two axes are
        treated as the horizontal plane; a 3D input is filtered in parallel
        across the leading (vertical) axis.
    nmax : int
        Number of recursive applications (>=1). Suggested values: 3 at
        ~9 km grid spacing, 27 at ~3 km. See `vortex_filter` for an
        auto-scaler.

    Returns
    -------
    np.ndarray
        Filtered array, same shape and dtype as input (float32/float64
        preserved; integer inputs are promoted to float64).
    """
    if nmax is None or nmax < 1:
        return np.asarray(field)

    arr = np.asarray(field)
    if arr.ndim not in (2, 3):
        raise ValueError(
            f"kurihara_smooth: expected 2D or 3D array, got ndim={arr.ndim}")

    if not np.issubdtype(arr.dtype, np.floating):
        arr = arr.astype(np.float64)
    else:
        # Work on a copy so we can do in-place edge restores without
        # clobbering the caller's input.
        arr = arr.copy()

    # Mask NaNs with the column mean to keep the filter well-defined; we
    # restore the NaN mask at the end. Without this, a single NaN smears
    # across the whole field after 11 convolutions.
    nan_mask = ~np.isfinite(arr)
    if nan_mask.any():
        fill_val = np.nanmean(arr)
        if not np.isfinite(fill_val):
            fill_val = 0.0
        arr[nan_mask] = fill_val

    # Horizontal axes are the last two regardless of 2D vs 3D.
    lat_axis = arr.ndim - 2
    lon_axis = arr.ndim - 1

    # NMAX zonal passes, then NMAX meridional passes (matches hbfilter.f90).
    for _ in range(int(nmax)):
        arr = _apply_chain(arr, axis=lon_axis)
    for _ in range(int(nmax)):
        arr = _apply_chain(arr, axis=lat_axis)

    if nan_mask.any():
        arr[nan_mask] = np.nan
    return arr


# Default Gaussian sigma (in km) for TC vortex removal. Calibrated on a
# 2.2-km HAFS storm nest (2023082618 / 08L, mature hurricane): sigma=250 km
# drives all three steering layers (SFDL/SFML/SFSL) to a post-filter
# residual max of 15-20 kt -- in line with observed synoptic steering
# magnitudes -- while keeping the environmental mean ~11-13 kt. Smaller
# sigmas leave a visible vortex ring on the shallow-layer plot; larger
# sigmas start attenuating the synoptic gradient we are trying to show.
_DEFAULT_SIGMA_KM = 250.0


def vortex_filter(field, sigma_km=None, dx_km=None, method='gaussian',
                  nmax=None):
    """
    Remove the TC vortex from a 2D or 3D field so the residual reflects the
    environmental (steering-layer) flow.

    Parameters
    ----------
    field : np.ndarray
        2D (lat, lon) or 3D (lev, lat, lon) array.
    sigma_km : float, optional
        Gaussian sigma in kilometres for ``method='gaussian'``. Defaults
        to ``_DEFAULT_SIGMA_KM`` (150 km).
    dx_km : float, optional
        Horizontal grid spacing in km. Required for ``method='gaussian'``
        because the scipy kernel takes sigma in *grid points*. If not
        passed we default to 3 km (the HAFS d03 nest) and warn.
    method : {'gaussian', 'kurihara'}
        Filter family. 'gaussian' (default) is faster and more effective
        at high-resolution grids; 'kurihara' reproduces the legacy
        Fortran algorithm and exists for reference / comparison.
    nmax : int, optional
        Iteration count for ``method='kurihara'``. Auto-scaled from
        ``dx_km`` (27 at 3 km, 3 at 9 km, linearly interpolated).

    Returns
    -------
    np.ndarray
        Filtered array, same shape as ``field``. NaN-aware: missing cells
        are temporarily filled with the field mean for the smoothing,
        then re-masked in the output.
    """
    arr = np.asarray(field)

    if method == 'kurihara':
        if nmax is None:
            dx = float(dx_km) if dx_km else 3.0
            nmax = max(3, int(round(81.0 / dx)))
        logger.debug(
            "vortex_filter[kurihara]: dx_km=%s nmax=%d shape=%s",
            dx_km, nmax, arr.shape,
        )
        return kurihara_smooth(arr, nmax)

    if method != 'gaussian':
        raise ValueError(
            f"vortex_filter: unknown method '{method}' "
            "(expected 'gaussian' or 'kurihara')")

    if dx_km is None or dx_km <= 0:
        logger.debug("vortex_filter: dx_km not supplied, defaulting to 3.0")
        dx_km = 3.0
    if sigma_km is None:
        sigma_km = _DEFAULT_SIGMA_KM

    sigma_px = float(sigma_km) / float(dx_km)
    logger.debug(
        "vortex_filter[gaussian]: dx_km=%.3f sigma_km=%.1f sigma_px=%.1f "
        "shape=%s",
        dx_km, sigma_km, sigma_px, arr.shape,
    )

    if arr.ndim not in (2, 3):
        raise ValueError(
            f"vortex_filter: expected 2D or 3D array, got ndim={arr.ndim}")

    # Promote integer input to float so the filter has room to blend.
    if not np.issubdtype(arr.dtype, np.floating):
        arr = arr.astype(np.float64)

    nan_mask = ~np.isfinite(arr)
    if nan_mask.any():
        fill_val = np.nanmean(arr)
        if not np.isfinite(fill_val):
            fill_val = 0.0
        work = arr.copy()
        work[nan_mask] = fill_val
    else:
        work = arr

    # Smooth only on the horizontal axes; the leading axis (if any) is
    # the vertical dimension and must not mix pressure levels.
    if arr.ndim == 3:
        sigmas = (0.0, sigma_px, sigma_px)
    else:
        sigmas = (sigma_px, sigma_px)

    out = gaussian_filter(work, sigma=sigmas, mode='nearest')

    if nan_mask.any():
        out[nan_mask] = np.nan
    return out


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _apply_chain(arr, axis):
    """
    Apply the 11-stage Kurihara chain along one axis, pinning the first
    and last slices along that axis to their pre-chain values (mirrors the
    Fortran `XTU(IMIN,N) = OUTPUT(IMIN,J,K)` edge-clamping for all N).

    Parameters
    ----------
    arr : np.ndarray
        Array to filter; must be float.
    axis : int
        Axis along which to apply the chain.

    Returns
    -------
    np.ndarray
        Result of one full 11-stage pass (does NOT apply the NMAX outer
        loop).
    """
    n = arr.shape[axis]
    if n < 3:
        # Not enough points to run a 3-point stencil.
        return arr

    # Snapshot the leading and trailing slices so we can pin them after
    # each stage (matches hbfilter.f90 behaviour).
    edge_lo = np.take(arr, 0, axis=axis)
    edge_hi = np.take(arr, n - 1, axis=axis)

    out = arr
    for kernel in _KERNELS:
        out = convolve1d(out, kernel, axis=axis, mode='nearest')
        # Restore boundary slices
        _assign_slice(out, edge_lo, axis, 0)
        _assign_slice(out, edge_hi, axis, n - 1)
    return out


def _assign_slice(arr, values, axis, index):
    """arr[..., index, ...] = values (along the given axis). In-place."""
    slicer = [slice(None)] * arr.ndim
    slicer[axis] = index
    arr[tuple(slicer)] = values
